MODULE DataPlant

  ! MODULE INFORMATION:
  !       AUTHOR         Plant code authors?
  !       DATE WRITTEN
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This data-only module contains the structures for various parts of the Plant and
  ! Condenser Loops.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules (only modules that should be used here and sparingly)
USE DataPrecisionGlobals
USE DataGlobals,  ONLY: MaxNameLength,outputfiledebug
USE DataLoopNode, ONLY: SensedNodeFlagValue, NodeID

IMPLICIT NONE         ! Enforce explicit typing of all variables

PUBLIC ! Everything public in data-only modules

  !MODULE PARAMETER DEFINITIONS:
! Parameters for use in Load Distribution Schemes
INTEGER, PARAMETER :: OptimalLoading           = 1  ! Optimal Load Distribution Scheme
INTEGER, PARAMETER :: SequentialLoading        = 2  ! sequential Distribution Scheme
INTEGER, PARAMETER :: UniformLoading           = 3  ! sequential Distribution Scheme

! Parameters for scheme types
! Used in TYPE(OperationData)%OpSchemeType
! As in PlantLoop(:)%OpScheme(:)%OpSchemeType
! Also in PlantLoop()LoopSide()Branch()Comp()%CurOpSchemeType
INTEGER, PARAMETER :: UnknownStatusOpSchemeType          = -2
INTEGER, PARAMETER :: NoControlOpSchemeType              = -1         ! Scheme Type placeholder for items such as pipes
INTEGER, PARAMETER :: LoadRBOpSchemeType                 =  0         ! Scheme Type for Load Range Based Operation (Deprecated)
INTEGER, PARAMETER :: HeatingRBOpSchemeType              =  1         ! Scheme Type for Heating Load Range Based Operation
INTEGER, PARAMETER :: CoolingRBOpSchemeType              =  2         ! Scheme Type for Cooling  Load Range Based Operation
INTEGER, PARAMETER :: WetBulbRBOpSchemeType              =  3         ! Scheme Type for Wet bulb range based Operation
INTEGER, PARAMETER :: DrybulbRBOpSchemeType              =  4         ! Scheme Type for Dry bulb range based Operation
INTEGER, PARAMETER :: DewpointRBOpSchemeType             =  5         ! Scheme Type for Dewpoint range based Operation
INTEGER, PARAMETER :: RelHumRBOpSchemeType               =  6         ! Scheme Type for relative humidity range based Operation
INTEGER, PARAMETER :: DrybulbTDBOpSchemeType             =  7         ! Scheme Type for relative humidity range based Operation
INTEGER, PARAMETER :: WetBulbTDBOpSchemeType             =  8         ! Scheme Type for Wet bulb range based Operation
INTEGER, PARAMETER :: DewpointTDBOpSchemeType            =  9         ! Scheme Type for Wet bulb range based Operation
INTEGER, PARAMETER :: CompSetPtBasedSchemeType           = 10         !*Sankar Temp Based Control
INTEGER, PARAMETER :: UncontrolledOpSchemeType           = 11         ! Scheme Type for Uncontrolled Operation
!INTEGER, PARAMETER :: EMSOpSchemeType                    = 12         ! Scheme Type for EMS based operation
INTEGER, PARAMETER :: PumpOpSchemeType                   = 13         ! Not really an opscheme, just a placeholder
INTEGER, PARAMETER :: DemandOpSchemeType                 = 14         ! Plcaeholder for demand side equipment such as coils
INTEGER, PARAMETER :: FreeRejectionOpSchemeType          = 15         ! Scheme Type for waterside economizers and the like
INTEGER, PARAMETER :: WSEconOpSchemeType                 = 16         ! Scheme Type for waterside economizers and the like
                                                                            ! this may be changed later...

! These are useful for SELECT CASE statements rather than listing all of the individual types listed above
INTEGER, PARAMETER :: LoadRangeBasedMin      = 0
INTEGER, PARAMETER :: LoadRangeBasedMax      = 2
INTEGER, PARAMETER :: TempRangeBasedMin      = 3
INTEGER, PARAMETER :: TempRangeBasedMax      = 6
INTEGER, PARAMETER :: DeltaTempRangeBasedMin = 7
INTEGER, PARAMETER :: DeltaTempRangeBasedMax = 9

! SimFlagCriteriaTypes for use in performing interconnect re-sim checks
INTEGER, PARAMETER   :: CriteriaType_MassFlowRate     = 1
INTEGER, PARAMETER   :: CriteriaType_Temperature      = 2
INTEGER, PARAMETER   :: CriteriaType_HeatTransferRate = 3

! Criteria percentage limits for determining re-simulation of connected loop sides
REAL(r64), PARAMETER :: CriteriaDelta_MassFlowRate     = 0.001d0
REAL(r64), PARAMETER :: CriteriaDelta_Temperature      = 0.010d0
REAL(r64), PARAMETER :: CriteriaDelta_HeatTransferRate = 0.100d0

! Parameters for flow Control Types for branch flow resolution inside splitter/mixers
INTEGER, PARAMETER :: ControlType_Unknown      = 0
INTEGER, PARAMETER :: ControlType_Active       = 1      ! 'Active'
INTEGER, PARAMETER :: ControlType_Passive      = 2      ! 'Passive'
INTEGER, PARAMETER :: ControlType_SeriesActive = 3      ! 'SeriesActive'
INTEGER, PARAMETER :: ControlType_Bypass       = 4      ! 'Bypass
CHARACTER(len=*), PARAMETER, DIMENSION(0:4) :: cControlType=  &
                                    (/'Unknown     ',  &
                                      'Active      ',  &
                                      'Passive     ',  &
                                      'SeriesActive',  &
                                      'Bypass      '/)

! Parameters for loop flow request priority,
!     used in logic to deal with Node%MassFlowRequest for determining overall loop flow rate
INTEGER, PARAMETER :: LoopFlowStatus_Unknown              = 21  ! component's status is not yet set
INTEGER, PARAMETER :: LoopFlowStatus_NeedyAndTurnsLoopOn  = 22  ! component is a "winner" for loop flow requests
                                                                ! active valve inside component that modulates flow
                                                                !  gets the loop going under most conditions
INTEGER, PARAMETER :: LoopFlowStatus_NeedyIfLoopOn        = 23  !  component is a "winner" for loop flow requests
                                                                ! but doesn't normally get the loop going to start with
                                                                !  once loop is going, may increase needs, non-zero minimums
INTEGER, PARAMETER :: LoopFlowStatus_TakesWhatGets        = 24  ! component is a "loser" for loop flow requests, but if the loop is on it
                                                                ! it does make flow requests (for s/m resolution)

!Parameters for component character wrt how load gets met (or not)
!  used in %HowLoadServed to facilitate load dispatch logic
INTEGER, PARAMETER :: HowMet_Unknown                      = 50  ! not yet set
INTEGER, PARAMETER :: HowMet_NoneDemand                   = 51  ! does not meet a load, demand component
INTEGER, PARAMETER :: HowMet_PassiveCap                   = 52  ! Passive machine, does what conditions allow but
INTEGER, PARAMETER :: HowMet_ByNominalCap                 = 53  ! MaxLoad, MinLoad, OptLoad should work
INTEGER, PARAMETER :: HowMet_ByNominalCapLowOutLimit      = 54  ! MaxLoad, MinLoad, OptLoad but with low limit temp on outlet
INTEGER, PARAMETER :: HowMet_ByNominalCapHiOutLimit       = 55  ! MaxLoad, MinLoad, OptLoad but with high limit temp on outlet


! Parameters for use in Loop Demand Calculation Schemes
INTEGER, PARAMETER :: SingleSetPoint           = 1      ! Uses a single temp setpoint to calculate loop demand
INTEGER, PARAMETER :: DualSetPointDeadBand     = 2      ! Uses a dual temp setpoint with a deadband between the high
                                                        !  and the low to calculate loop demand
! Parameters for loop setpoint reference
INTEGER, PARAMETER :: Air                      = 1
INTEGER, PARAMETER :: Ground                   = 2
INTEGER, PARAMETER :: LoopNode                 = 3

! Parameters for common pipe
INTEGER, PARAMETER :: CommonPipe_No            = 0
INTEGER, PARAMETER :: CommonPipe_Single        = 1
INTEGER, PARAMETER :: CommonPipe_TwoWay        = 2

! Parameters for loop side location
INTEGER, PARAMETER :: DemandSupply_No          = 0
INTEGER, PARAMETER :: DemandSide               = 1
INTEGER, PARAMETER :: SupplySide               = 2
INTEGER, PARAMETER :: DemandSupply_Yes         = 3  !DSU

CHARACTER(len=*), PARAMETER, DIMENSION(0:3) :: cLoopSideLocations=  &
  (/'DemandSupply_No ',  &
    'DemandSide      ',  &
    'SupplySide      ',  &
    'DemandSupply_Yes'/)
! Parameters for economizer
INTEGER, PARAMETER :: Integrated               = 1
INTEGER, PARAMETER :: NonIntegrated            = 2
INTEGER, PARAMETER :: None                     = 3

! Parameters for tolerance
REAL(r64), PARAMETER :: MassFlowTol            = .0000001d0  ! minimum significant mass flow rate (kg/s)
REAL(r64), PARAMETER :: LoopDemandtol          = .1d0    ! minimum significant loop cooling or heating demand
REAL(r64), PARAMETER :: DeltaTemptol           = .0001d0 ! minimum significant loop temperature difference

! Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
INTEGER, PARAMETER :: LoopType_NoLoop    = 0
INTEGER, PARAMETER :: LoopType_Plant     = 1
INTEGER, PARAMETER :: LoopType_Condenser = 2
INTEGER, PARAMETER :: LoopType_Both      = 3

! Parameters for calls to simflag routines
INTEGER, PARAMETER :: PlantSupply  = -1
INTEGER, PARAMETER :: PlantDemand  = -2
INTEGER, PARAMETER :: CondSupply   = -3
INTEGER, PARAMETER :: CondDemand   = -4

! Parameters for FlowLock standardization
INTEGER, PARAMETER :: FlowPumpQuery = -1  ! Used to ask the pumps for their min/max avail based on no constraints
INTEGER, PARAMETER :: FlowUnlocked  = 0   ! components request flow
INTEGER, PARAMETER :: FlowLocked    = 1   ! components take their inlet flow

CHARACTER(len=*), DIMENSION(0:3), PARAMETER :: cLoopTypes=  &
  (/'None                ',  &
    'Plant               ',  &
    'Condenser           ',  &
    'Both Plant/Condenser'/)

! Pressure Routine Call Enumeration
INTEGER,          PARAMETER    :: PressureCall_Init         = -1
INTEGER,          PARAMETER    :: PressureCall_Calc         = -2
INTEGER,          PARAMETER    :: PressureCall_Update       = -3

! Pressure Curve Type: None, pressure, or generic curve (if generic it will be a postive value which is the curve manager index)
INTEGER,          PARAMETER    :: PressureCurve_Error       = -1
INTEGER,          PARAMETER    :: PressureCurve_None        = 0
INTEGER,          PARAMETER    :: PressureCurve_Pressure    = 1
INTEGER,          PARAMETER    :: PressureCurve_Generic     = 2

! Pressure Simulation Types
INTEGER,          PARAMETER    :: Press_NoPressure             = 1   !Nothing for that particular loop
INTEGER,          PARAMETER    :: Press_PumpPowerCorrection    = 2   !Only updating the pump power
INTEGER,          PARAMETER    :: Press_FlowCorrection         = 3   !Update pump flow rate based on pump curve
INTEGER,          PARAMETER    :: Press_FlowSimulation         = 4   !Full pressure network simulation
CHARACTER(len=MaxNameLength), PARAMETER    :: PressureSimType(4)      = (/'NONE               ',    &
                                                                          'PUMPPOWERCORRECTION',    &
                                                                          'LOOPFLOWCORRECTION ',    &
                                                                          'PRESSURESIMULATION '/)
! Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
INTEGER, PARAMETER :: NumSimPlantEquipTypes=77
CHARACTER(len=*), PARAMETER, DIMENSION(NumSimPlantEquipTypes) :: SimPlantEquipTypes=  &
      (/'BOILER:HOTWATER                                       ', & !01
        'BOILER:STEAM                                          ', & !02
        'CHILLER:ABSORPTION                                    ', & !03
        'CHILLER:ABSORPTION:INDIRECT                           ', & !04
        'CHILLER:COMBUSTIONTURBINE                             ', & !05
        'CHILLER:CONSTANTCOP                                   ', & !06
        'CHILLERHEATER:ABSORPTION:DIRECTFIRED                  ', & !07
        'CHILLER:ELECTRIC                                      ', & !08
        'CHILLER:ELECTRIC:EIR                                  ', & !09
        'CHILLER:ELECTRIC:REFORMULATEDEIR                      ', & !10
        'CHILLER:ENGINEDRIVEN                                  ', & !11
        'COOLINGTOWER:SINGLESPEED                              ', & !12
        'COOLINGTOWER:TWOSPEED                                 ', & !13
        'COOLINGTOWER:VARIABLESPEED                            ', & !14
        'GENERATOR:FUELCELL:EXHAUSTGASTOWATERHEATEXCHANGER     ', & !15
        'HEATEXCHANGER:HYDRONIC                                ', & !16
        'HEATEXCHANGER:WATERSIDEECONOMIZER                     ', & !17
        'WATERHEATER:HEATPUMP                                  ', & !18
        'HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING             ', & !19
        'HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING             ', & !20
        'HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING     ', & !21
        'HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING     ', & !22
        'PIPE:ADIABATIC                                        ', & !23
        'PIPE:ADIABATIC:STEAM                                  ', & !24
        'PIPE:OUTDOOR                                          ', & !25
        'PIPE:INDOOR                                           ', & !26
        'PIPE:UNDERGROUND                                      ', & !27
        'DISTRICTCOOLING                                       ', & !28
        'DISTRICTHEATING                                       ', & !29
        'THERMALSTORAGE:ICE:DETAILED                           ', & !30
        'THERMALSTORAGE:ICE:SIMPLE                             ', & !31
        'TEMPERINGVALVE                                        ', & !32
        'WATERHEATER:MIXED                                     ', & !33
        'WATERHEATER:STRATIFIED                                ', & !34
        'PUMP:VARIABLESPEED                                    ', & !35
        'PUMP:CONSTANTSPEED                                    ', & !36
        'PUMP:VARIABLESPEED:CONDENSATE                         ', & !37
        'HEADEREDPUMPS:VARIABLESPEED                           ', & !38
        'HEADEREDPUMPS:CONSTANTSPEED                           ', & !39
        'WATERUSE:CONNECTIONS                                  ', & !40 ! demand side component
        'COIL:COOLING:WATER                                    ', & !41 ! demand side component
        'COIL:COOLING:WATER:DETAILEDGEOMETRY                   ', & !42 ! demand side component
        'COIL:HEATING:WATER                                    ', & !43 ! demand side component
        'COIL:HEATING:STEAM                                    ', & !44 ! demand side component
        'SOLARCOLLECTOR:FLATPLATE:WATER                        ', & !45 ! demand side component
        'LOADPROFILE:PLANT                                     ', & !46 ! demand side component'
        'GROUNDHEATEXCHANGER:VERTICAL                          ', & !47
        'GROUNDHEATEXCHANGER:SURFACE                           ', & !48
        'GROUNDHEATEXCHANGER:POND                              ', & !49
        'HEATEXCHANGER:PLATE                                   ', & !50
        'GENERATOR:MICROTURBINE                                ', & !51
        'GENERATOR:INTERNALCOMBUSTIONENGINE                    ', & !52
        'GENERATOR:COMBUSTIONTURBINE                           ', & !53
        'GENERATOR:MICROCHP                                    ', & !54
        'GENERATOR:FUELCELL:STACKCOOLER                        ', & !55
        'FLUIDCOOLER:SINGLESPEED                               ', & !56
        'FLUIDCOOLER:TWOSPEED                                  ', & !57
        'EVAPORATIVEFLUIDCOOLER:SINGLESPEED                    ', & !58
        'EVAPORATIVEFLUIDCOOLER:TWOSPEED                       ', & !59
        'THERMALSTORAGE:CHILLEDWATER:MIXED                     ', & !60
        'THERMALSTORAGE:CHILLEDWATER:STRATIFIED                ', & !61
        'SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL          ', & !62
        'ZONEHVAC:BASEBOARD:CONVECTIVE:WATER                   ', & !63
        'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM            ', & !64
        'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER            ', & !65
        'ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW           ', & !66
        'ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW           ', & !67
        'AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:COOLEDBEAM      ', & !68
        'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT           ', & !69
        'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT           ', & !70
        'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION   ', & !71
        'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION   ', & !72
        'REFRIGERATION:CONDENSER:WATERCOOLED                   ', & !73
        'REFRIGERATION:COMPRESSORRACK                          ', & !74
        'AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED       ', & !75
        'CHILLERHEATER:ABSORPTION:DOUBLEEFFECT                 ', & !76
        'PIPINGSYSTEM:UNDERGROUND:PIPECIRCUIT                  '/)  !77

CHARACTER(len=*), PARAMETER, DIMENSION(NumSimPlantEquipTypes) :: ccSimPlantEquipTypes=  &
      (/'Boiler:HotWater                                       ', & !01
        'Boiler:Steam                                          ', & !02
        'Chiller:Absorption                                    ', & !03
        'Chiller:Absorption:Indirect                           ', & !04
        'Chiller:CombustionTurbine                             ', & !05
        'Chiller:ConstantCOP                                   ', & !06
        'ChillerHeater:Absorption:DirectFired                  ', & !07
        'Chiller:Electric                                      ', & !08
        'Chiller:Electric:EIR                                  ', & !09
        'Chiller:Electric:ReformulatedEIR                      ', & !10
        'Chiller:EngineDriven                                  ', & !11
        'CoolingTower:SingleSpeed                              ', & !12
        'CoolingTower:TwoSpeed                                 ', & !13
        'CoolingTower:VariableSpeed                            ', & !14
        'Generator:Fuelcell:ExhaustGastoWaterHeatExchanger     ', & !15
        'HeatExchanger:Hydronic                                ', & !16
        'HeatExchanger:WatersideEconomizer                     ', & !17
        'WaterHeater:Heatpump                                  ', & !18
        'Heatpump:WatertoWater:Equationfit:Cooling             ', & !19
        'Heatpump:WatertoWater:Equationfit:Heating             ', & !20
        'Heatpump:WatertoWater:ParameterEstimation:Cooling     ', & !21
        'Heatpump:WatertoWater:ParameterEstimation:Heating     ', & !22
        'Pipe:Adiabatic                                        ', & !23
        'Pipe:Adiabatic:Steam                                  ', & !24
        'Pipe:Outdoor                                          ', & !25
        'Pipe:Indoor                                           ', & !26
        'Pipe:Underground                                      ', & !27
        'DistrictCooling                                       ', & !28
        'DistrictHeating                                       ', & !29
        'ThermalStorage:Ice:Detailed                           ', & !30
        'ThermalStorage:Ice:Simple                             ', & !31
        'TemperingValve                                        ', & !32
        'WaterHeater:Mixed                                     ', & !33
        'WaterHeater:Stratified                                ', & !34
        'Pump:VariableSpeed                                    ', & !35
        'Pump:ConstantSpeed                                    ', & !36
        'Pump:VariableSpeed:Condensate                         ', & !37
        'HeaderedPumps:VariableSpeed                           ', & !38
        'HeaderedPumps:ConstantSpeed                           ', & !39
        'WaterUse:Connections                                  ', & !40 ! demand side component
        'Coil:Cooling:Water                                    ', & !41 Demand Side Component
        'Coil:Cooling:Water:DetailedGeometry                   ', & !42 Demand Side Component
        'Coil:Heating:Water                                    ', & !43 Demand Side Component
        'Coil:Heating:Steam                                    ', & !44 Demand Side Component
        'Solarcollector:Flatplate:Water                        ', & !45 Demand Side Component
        'LoadProfile:Plant                                     ', & !46 Demand Side Component'
        'GroundHeatExchanger:Vertical                          ', & !47
        'GroundHeatExchanger:Surface                           ', & !48
        'GroundHeatExchanger:Pond                              ', & !49
        'HeatExchanger:Plate                                   ', & !50
        'Generator:Microturbine                                ', & !51
        'Generator:InternalCombustionEngine                    ', & !52
        'Generator:CombustionTurbine                           ', & !53
        'Generator:Microchp                                    ', & !54
        'Generator:Fuelcell:StackCooler                        ', & !55
        'FluidCooler:SingleSpeed                               ', & !56
        'FluidCooler:TwoSpeed                                  ', & !57
        'EvaporativeFluidCooler:SingleSpeed                    ', & !58
        'EvaporativeFluidCooler:TwoSpeed                       ', & !59
        'ThermalStorage:ChilledWater:Mixed                     ', & !60
        'ThermalStorage:ChilledWater:Stratified                ', & !61
        'SolarCollector:FlatPlate:PhotovoltaicThermal          ', & !62
        'ZoneHVAC:Baseboard:Convective:Water                   ', & !63
        'ZoneHVAC:Baseboard:RadiantConvective:Steam            ', & !64
        'ZoneHVAC:Baseboard:RadiantConvective:Water            ', & !65
        'ZoneHVAC:LowTemperatureRadiant:VariableFlow           ', & !66
        'ZoneHVAC:LowTemperatureRadiant:ConstantFlow           ', & !67
        'AirTerminal:SingleDuct:ConstantVolume:CooledBeam      ', & !68
        'Coil:Heating:WaterToAirHeatPump:EquationFit           ', & !69
        'Coil:Cooling:WaterToAirHeatPump:EquationFit           ', & !70
        'Coil:Heating:WaterToAirHeatPump:ParameterEstimation   ', & !71
        'Coil:Cooling:WaterToAirHeatPump:ParameterEstimation   ', & !72
        'Refrigeration:Condenser:WaterCooled                   ', & !73
        'Refrigeration:CompressorRack                          ', & !74
        'AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed       ', & !75
        'ChillerHeater:Absorption:DoubleEffect                 ', & !76
        'PipingSystem:Underground:PipeCircuit                  '/)  !77

INTEGER, PARAMETER, DIMENSION(NumSimPlantEquipTypes) :: ValidLoopEquipTypes=  &
 (/LoopType_Plant,     &     ! 01  BOILER:HOTWATER
   LoopType_Plant,     &     ! 02  BOILER:STEAM
   LoopType_Plant,     &     ! 03  CHILLER:ABSORPTION
   LoopType_Plant,     &     ! 04  CHILLER:ABSORPTION:INDIRECT
   LoopType_Plant,     &     ! 05  CHILLER:COMBUSTIONTURBINE
   LoopType_Plant,     &     ! 06  CHILLER:CONSTANTCOP
   LoopType_Plant,     &     ! 07  CHILLERHEATER:ABSORPTION:DIRECTFIRED
   LoopType_Plant,     &     ! 08  CHILLER:ELECTRIC
   LoopType_Plant,     &     ! 09  CHILLER:ELECTRIC:EIR
   LoopType_Plant,     &     ! 10  CHILLER:ELECTRIC:REFORMULATEDEIR
   LoopType_Plant,     &     ! 11  CHILLER:ENGINEDRIVEN
   LoopType_Both,      &     ! 12  COOLINGTOWER:SINGLESPEED
   LoopType_Both,      &     ! 13  COOLINGTOWER:TWOSPEED
   LoopType_Both,      &     ! 14  COOLINGTOWER:VARIABLESPEED
   LoopType_Plant,     &     ! 15  GENERATOR:FUELCELL:EXHAUSTGASTOWATERHEATEXCHANGER
   LoopType_Plant,     &     ! 16  HEATEXCHANGER:HYDRONIC
   LoopType_Plant,     &     ! 17  HEATEXCHANGER:WATERSIDEECONOMIZER
   LoopType_Plant,     &     ! 18  WATERHEATER:HEATPUMP
   LoopType_Plant,     &     ! 19  HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING
   LoopType_Plant,     &     ! 20  HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING
   LoopType_Plant,     &     ! 21  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING
   LoopType_Plant,     &     ! 22  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING
   LoopType_Both,      &     ! 23  PIPE:ADIABATIC
   LoopType_Both,      &     ! 24  PIPE:ADIABATIC:STEAM
   LoopType_Both,      &     ! 25  PIPE:OUTDOOR
   LoopType_Both,      &     ! 26  PIPE:INDOOR
   LoopType_Both,      &     ! 27  PIPE:UNDERGROUND
   LoopType_Both,      &     ! 28  DISTRICTCOOLING
   LoopType_Both,      &     ! 29  DISTRICTHEATING
   LoopType_Plant,     &     ! 30  THERMALSTORAGE:ICE:DETAILED
   LoopType_Plant,     &     ! 31  THERMALSTORAGE:ICE:SIMPLE
   LoopType_Both,      &     ! 32  TEMPERINGVALVE
   LoopType_Both,      &     ! 33  WATERHEATER:MIXED
   LoopType_Both,      &     ! 34  WATERHEATER:STRATIFIED
   LoopType_Both,      &     ! 35  PUMP:VARIABLESPEED
   LoopType_Both,      &     ! 36  PUMP:CONSTANTSPEED
   LoopType_Both,      &     ! 37  PUMP:VARIABLESPEED:CONDENSATE
   LoopType_Both,      &     ! 38  HEADEREDPUMPS:VARIABLESPEED
   LoopType_Both,      &     ! 39  HEADEREDPUMPS:CONSTANTSPEED
   LoopType_Plant,     &     ! 40  WATERUSE:CONNECTIONS
   LoopType_Plant,     &     ! 41  COIL:COOLING:WATER
   LoopType_Plant,     &     ! 42  COIL:COOLING:WATER:DETAILEDGEOMETRY
   LoopType_Plant,     &     ! 43  COIL:HEATING:WATER
   LoopType_Plant,     &     ! 44  COIL:HEATING:STEAM
   LoopType_Plant,     &     ! 45  SOLARCOLLECTOR:FLATPLATE:WATER
   LoopType_Both,      &     ! 46  LOADPROFILE:PLANT
   LoopType_Both,      &     ! 47  GROUNDHEATEXCHANGER:VERTICAL
   LoopType_Both,      &     ! 48  GROUNDHEATEXCHANGER:SURFACE
   LoopType_Both,      &     ! 49  GROUNDHEATEXCHANGER:POND
   LoopType_Both,      &     ! 50  HEATEXCHANGER:PLATE
   LoopType_Plant,     &     ! 51  GENERATOR:MICROTURBINE
   LoopType_Plant,     &     ! 52  GENERATOR:INTERNALCOMBUSTIONENGINE
   LoopType_Plant,     &     ! 53  GENERATOR:COMBUSTIONTURBINE
   LoopType_Plant,     &     ! 54  GENERATOR:MICROCHP
   LoopType_Plant,     &     ! 55  GENERATOR:FUELCELL:STACKCOOLER
   LoopType_Both,      &     ! 56  FLUIDCOOLER:SINGLESPEED
   LoopType_Both,      &     ! 57  FLUIDCOOLER:TWOSPEED
   LoopType_Both,      &     ! 58  EVAPORATIVEFLUIDCOOLER:SINGLESPEED
   LoopType_Both,      &     ! 59  EVAPORATIVEFLUIDCOOLER:TWOSPEED
   LoopType_Both,     &     ! 60  THERMALSTORAGE:CHILLEDWATER:MIXED
   LoopType_Both,     &     ! 61  THERMALSTORAGE:CHILLEDWATER:STRATIFIED
   LoopType_Both,     &     ! 62  SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL
   LoopType_Plant,     &     ! 63  ZONEHVAC:BASEBOARD:CONVECTIVE:WATER
   LoopType_Plant,     &     ! 64  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM
   LoopType_Plant,     &     ! 65  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER
   LoopType_Plant,     &     ! 66  ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW
   LoopType_Plant,     &     ! 67  ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW
   LoopType_Both,     &     ! 68  AirTerminal:SingleDuct:ConstantVolume:CooledBeam
   LoopType_Both,      &     ! 69  Coil:Heating:WaterToAirHeatPump:EquationFit
   LoopType_Both,      &     ! 70  Coil:Cooling:WaterTOAIRHeatPump:EquationFit
   LoopType_Both,      &     ! 71  Coil:Heating:WaterTOAIRHeatPump:ParameterEstimation
   LoopType_Both,      &     ! 72  Coil:Cooling:WaterTOAIRHeatPump:ParameterEstimation
   LoopType_Both,      &     ! 73  Refrigeration:Condenser:WaterCooled
   LoopType_Both,      &     ! 74  Refrigeration:CompressorRack
   LoopType_Plant,     &     ! 75  AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed
   LoopType_Plant,     &     ! 76  CHILLERHEATER:ABSORPTION:DOUBLEEFFECT
   LoopType_Both       /)    ! 77  PipingSystem:Underground:PipeCircuit

INTEGER, PARAMETER :: TypeOf_Other                       = -1
INTEGER, PARAMETER :: TypeOf_Boiler_Simple               =  1
INTEGER, PARAMETER :: TypeOf_Boiler_Steam                =  2
INTEGER, PARAMETER :: TypeOf_Chiller_Absorption          =  3  ! older BLAST absorption chiller
INTEGER, PARAMETER :: TypeOf_Chiller_Indirect_Absorption =  4  ! revised absorption chiller
INTEGER, PARAMETER :: TypeOf_Chiller_CombTurbine         =  5
INTEGER, PARAMETER :: TypeOf_Chiller_ConstCOP            =  6
INTEGER, PARAMETER :: TypeOf_Chiller_DFAbsorption        =  7
INTEGER, PARAMETER :: TypeOf_Chiller_Electric            =  8  ! basic BLAST Chiller
INTEGER, PARAMETER :: TypeOf_Chiller_ElectricEIR         =  9
INTEGER, PARAMETER :: TypeOf_Chiller_ElectricReformEIR   = 10
INTEGER, PARAMETER :: TypeOf_Chiller_EngineDriven        = 11
INTEGER, PARAMETER :: TypeOf_CoolingTower_SingleSpd      = 12
INTEGER, PARAMETER :: TypeOf_CoolingTower_TwoSpd         = 13
INTEGER, PARAMETER :: TypeOf_CoolingTower_VarSpd         = 14
INTEGER, PARAMETER :: TypeOf_Generator_FCExhaust         = 15
INTEGER, PARAMETER :: TypeOf_FreeCoolingHtExchg          = 16
INTEGER, PARAMETER :: TypeOf_WaterSideEconHtExchg        = 17
INTEGER, PARAMETER :: TypeOf_HeatPumpWtrHeater           = 18
INTEGER, PARAMETER :: TypeOf_HPWaterEFCooling            = 19
INTEGER, PARAMETER :: TypeOf_HPWaterEFHeating            = 20
INTEGER, PARAMETER :: TypeOf_HPWaterPECooling            = 21
INTEGER, PARAMETER :: TypeOf_HPWaterPEHeating            = 22
INTEGER, PARAMETER :: TypeOf_Pipe                        = 23
INTEGER, PARAMETER :: TypeOf_PipeSteam                   = 24
INTEGER, PARAMETER :: TypeOf_PipeExterior                = 25
INTEGER, PARAMETER :: TypeOf_PipeInterior                = 26
INTEGER, PARAMETER :: TypeOf_PipeUnderground             = 27
INTEGER, PARAMETER :: TypeOf_PurchChilledWater           = 28
INTEGER, PARAMETER :: TypeOf_PurchHotWater               = 29
INTEGER, PARAMETER :: TypeOf_TS_IceDetailed              = 30
INTEGER, PARAMETER :: TypeOf_TS_IceSimple                = 31
INTEGER, PARAMETER :: TypeOf_ValveTempering              = 32
INTEGER, PARAMETER :: TypeOf_WtrHeaterMixed              = 33
INTEGER, PARAMETER :: TypeOf_WtrHeaterStratified         = 34
INTEGER, PARAMETER :: TypeOf_PumpVariableSpeed           = 35
INTEGER, PARAMETER :: TypeOf_PumpConstantSpeed           = 36
INTEGER, PARAMETER :: TypeOf_PumpCondensate              = 37
INTEGER, PARAMETER :: TypeOf_PumpBankVariableSpeed       = 38
INTEGER, PARAMETER :: TypeOf_PumpBankConstantSpeed       = 39
INTEGER, PARAMETER :: TypeOf_WaterUseConnection          = 40
INTEGER, PARAMETER :: TypeOf_CoilWaterCooling            = 41  ! demand side component
INTEGER, PARAMETER :: TypeOf_CoilWaterDetailedFlatCooling= 42  ! demand side component
INTEGER, PARAMETER :: TypeOf_CoilWaterSimpleHeating      = 43  ! demand side component
INTEGER, PARAMETER :: TypeOf_CoilSteamAirHeating         = 44  ! demand side component
INTEGER, PARAMETER :: TypeOf_SolarCollectorFlatPlate     = 45  ! demand side component
INTEGER, PARAMETER :: TypeOf_PlantLoadProfile            = 46  ! demand side component
INTEGER, PARAMETER :: TypeOf_GrndHtExchgVertical         = 47
INTEGER, PARAMETER :: TypeOf_GrndHtExchgSurface          = 48
INTEGER, PARAMETER :: TypeOf_GrndHtExchgPond             = 49
INTEGER, PARAMETER :: TypeOf_HtExchgPlateFreeClng        = 50
INTEGER, PARAMETER :: TypeOf_Generator_MicroTurbine      = 51  !newer FSEC turbine
INTEGER, PARAMETER :: TypeOf_Generator_ICEngine          = 52
INTEGER, PARAMETER :: TypeOf_Generator_CTurbine          = 53  !older BLAST turbine
INTEGER, PARAMETER :: TypeOf_Generator_MicroCHP          = 54
INTEGER, PARAMETER :: TypeOf_Generator_FCStackCooler     = 55
INTEGER, PARAMETER :: TypeOf_FluidCooler_SingleSpd       = 56
INTEGER, PARAMETER :: TypeOf_FluidCooler_TwoSpd          = 57
INTEGER, PARAMETER :: TypeOf_EvapFluidCooler_SingleSpd   = 58
INTEGER, PARAMETER :: TypeOf_EvapFluidCooler_TwoSpd      = 59
INTEGER, PARAMETER :: TypeOf_ChilledWaterTankMixed       = 60
INTEGER, PARAMETER :: TypeOf_ChilledWaterTankStratified  = 61
INTEGER, PARAMETER :: TypeOf_PVTSolarCollectorFlatPlate  = 62
INTEGER, PARAMETER :: TypeOf_Baseboard_Conv_Water        = 63
INTEGER, PARAMETER :: TypeOf_Baseboard_Rad_Conv_Steam    = 64
INTEGER, PARAMETER :: TypeOf_Baseboard_Rad_Conv_Water    = 65
INTEGER, PARAMETER :: TypeOf_LowTempRadiant_VarFlow      = 66
INTEGER, PARAMETER :: TypeOf_LowTempRadiant_ConstFlow    = 67
INTEGER, PARAMETER :: TypeOf_CooledBeamAirTerminal       = 68
INTEGER, PARAMETER :: TypeOf_CoilWAHPHeatingEquationFit  = 69
INTEGER, PARAMETER :: TypeOf_CoilWAHPCoolingEquationFit  = 70
INTEGER, PARAMETER :: TypeOf_CoilWAHPHeatingParamEst     = 71
INTEGER, PARAMETER :: TypeOf_CoilWAHPCoolingParamEst     = 72
INTEGER, PARAMETER :: TypeOf_RefrigSystemWaterCondenser  = 73
INTEGER, PARAMETER :: TypeOf_RefrigerationWaterCoolRack  = 74
INTEGER, PARAMETER :: TypeOf_MultiSpeedHeatPumpRecovery  = 75
INTEGER, PARAMETER :: TypeOf_Chiller_ExhFiredAbsorption  = 76
INTEGER, PARAMETER :: TypeOf_PipingSystemPipeCircuit     = 77

! Parameters for General Equipment Types
INTEGER, PARAMETER :: NumGeneralEquipTypes               = 21
CHARACTER(len=*), PARAMETER, &
     DIMENSION(NumGeneralEquipTypes) :: GeneralEquipTypes   =   &
                   (/'BOILER                 ',  &
                     'CHILLER                ',  &
                     'COOLINGTOWER           ',  &
                     'GENERATOR              ',  &
                     'HEATEXCHANGER          ',  &
                     'HEATPUMP               ',  &
                     'PIPE                   ',  &
                     'PUMP                   ',  &
                     'DISTRICT               ',  &
                     'THERMALSTORAGE         ',  &
                     'TEMPERINGVALVE         ',  &
                     'WATERHEATER            ',  &
                     'WATERUSE               ',  &
                     'DEMANDCOIL             ',  &
                     'SOLARCOLLECTOR         ',  &
                     'LOADPROFILE            ',  &
                     'FLUIDCOOLER            ',  &
                     'EVAPORATIVEFLUIDCOOLER ', &
                     'GROUNDHEATEXCHANGER    ', &
                     'ZONEHVACDEMAND         ', &
                     'REFRIGERATION          '/)

INTEGER, PARAMETER :: GenEquipTypes_Boiler               =  1
INTEGER, PARAMETER :: GenEquipTypes_Chiller              =  2
INTEGER, PARAMETER :: GenEquipTypes_CoolingTower         =  3
INTEGER, PARAMETER :: GenEquipTypes_Generator            =  4
INTEGER, PARAMETER :: GenEquipTypes_HeatExchanger        =  5
INTEGER, PARAMETER :: GenEquipTypes_HeatPump             =  6
INTEGER, PARAMETER :: GenEquipTypes_Pipe                 =  7
INTEGER, PARAMETER :: GenEquipTypes_Pump                 =  8
INTEGER, PARAMETER :: GenEquipTypes_Purchased            =  9
INTEGER, PARAMETER :: GenEquipTypes_ThermalStorage       = 10
INTEGER, PARAMETER :: GenEquipTypes_Valve                = 11
INTEGER, PARAMETER :: GenEquipTypes_WaterThermalTank     = 12
INTEGER, PARAMETER :: GenEquipTypes_WaterUse             = 13
INTEGER, PARAMETER :: GenEquipTypes_DemandCoil           = 14
INTEGER, PARAMETER :: GenEquipTypes_SolarCollector       = 15
INTEGER, PARAMETER :: GenEquipTypes_LoadProfile          = 16
INTEGER, PARAMETER :: GenEquipTypes_FluidCooler          = 17
INTEGER, PARAMETER :: GenEquipTypes_EvapFluidCooler      = 18
INTEGER, PARAMETER :: GenEquipTypes_GroundHeatExchanger  = 19
INTEGER, PARAMETER :: GenEquipTypes_ZoneHVACDemand       = 20
INTEGER, PARAMETER :: GenEquipTypes_Refrigeration        = 21

CHARACTER(len=*), PARAMETER, DIMENSION (0:11) :: OpSchemeTypes = &
                   (/'Load Range Based Operation                        ',   &  ! long since Deprecated, remove?
                     'PLANTEQUIPMENTOPERATION:HEATINGLOAD               ',   &
                     'PLANTEQUIPMENTOPERATION:COOLINGLOAD               ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORWETBULB            ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORDRYBULB            ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINT           ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORRELATIVEHUMIDITY   ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORDRYBULBDIFFERENCE  ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORWETBULBDIFFERENCE  ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINTDIFFERENCE ',   &
                     'PLANTEQUIPMENTOPERATION:COMPONENTSETPOINT         ',   &
                     'PLANTEQUIPMENTOPERATION:UNCONTROLLED              '/)

  ! DERIVED TYPE DEFINITIONS:
TYPE SplitterData
  LOGICAL                          :: Exists                = .FALSE. ! True if there is a splitter (only 1 allowed per loop)
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Name of the splitter
  INTEGER                          :: NodeNumIn             = 0       ! Node number for the inlet to the splitter
  INTEGER                          :: BranchNumIn           = 0       ! Reference number for branch connected to splitter inlet
  INTEGER                          :: LevelIn               = 0       ! Branch Level of splitter inlet in the loop topology
  INTEGER                          :: LevelOut              = 0       ! Branch Level of splitter outlet in the loop topology
  INTEGER                          :: CorrMixIndex          = 0       ! Index of Mixer corresponding to this Splitter
  CHARACTER(len=MaxNameLength)     :: NodeNameIn            = ' '     ! Node name for the inlet to the splitter
  INTEGER                          :: TotalOutletNodes      = 0       ! Number of outlet nodes for the splitter
  INTEGER, &
         ALLOCATABLE, DIMENSION(:) :: NodeNumOut                      ! Node number for the outlet to the splitter
  INTEGER, &
         ALLOCATABLE, DIMENSION(:) :: BranchNumOut                    ! Reference number for branch connected to splitter outlet
  CHARACTER(len=MaxNameLength), &
         ALLOCATABLE, DIMENSION(:) :: NodeNameOut                     ! Node name for the outlet to the splitter
END TYPE SplitterData

TYPE MixerData
  LOGICAL                          :: Exists                = .FALSE. ! True if there is a mixer (only 1 allowed per loop)
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Name of the mixer
  INTEGER                          :: NodeNumOut            = 0       ! Node number for the outlet to the mixer
  INTEGER                          :: BranchNumOut          = 0       ! Reference number for branch connected to splitter inlet
  INTEGER                          :: LevelIn               = 0       ! Branch Level of mixer inlet in the loop topology
  INTEGER                          :: LevelOut              = 0       ! Branch Level of mixer outlet in the loop topology
  INTEGER                          :: CorrSplitIndex        = 0       ! Index of Splitter corresponding to this mixer
  CHARACTER(len=MaxNameLength)     :: NodeNameOut           = ' '     ! Node name for the outlet to the mixer
  INTEGER                          :: TotalInletNodes       = 0       ! Number of inlet nodes for the mixer
  INTEGER, &
         ALLOCATABLE, DIMENSION(:) :: NodeNumIn                       ! Node number for the inlet to the mixer
  INTEGER, &
         ALLOCATABLE, DIMENSION(:) :: BranchNumIn                     ! Reference number for branch connected to splitter outlet
  CHARACTER(len=MaxNameLength), &
         ALLOCATABLE, DIMENSION(:) :: NodeNameIn                      ! Node name for the inlet to the mixer
END TYPE MixerData

TYPE LoopSidePumpInformation
  CHARACTER(len=MaxNameLength)     :: PumpName              = ' '
  INTEGER                          :: PumpTypeOf            = 0
  INTEGER                          :: BranchNum             = 0
  INTEGER                          :: CompNum               = 0
  INTEGER                          :: PumpOutletNode        = 0
  REAL(r64)                        :: PumpFlowRate          = 0.0d0
  REAL(r64)                        :: CurrentMinAvail       = 0.0d0
  REAL(r64)                        :: CurrentMaxAvail       = 0.0d0
  REAL(r64)                        :: PumpHeatToFluid       = 0.0d0
END TYPE LoopSidePumpInformation

TYPE EquipListPtrData
  INTEGER                          :: ListPtr               = 0     ! points to List on Opscheme on plant loop:
                                                           ! PlantLoop(loopNum)%Opscheme(Optschemeptr)%List(ListPtr)...
  INTEGER                          :: CompPtr               = 0     ! points to this component on List on Opscheme on plant loop:
                                                           ! PlantLoop(loopNum)%Opscheme(Optschemeptr)%List(ListPtr)%Comp(compPtr)
END TYPE EquipListPtrData

TYPE OpSchemePtrData
  INTEGER                          :: OpSchemePtr           = 0     ! DSU points to Opscheme on plant loop:
                                                           ! PlantLoop(loopNum)%Opscheme(Optschemeptr)...
  INTEGER                          :: NumEquipLists         = 0     ! DSU ALLOCATABLE to the schedule (for valid schedules)
  TYPE(EquipListPtrData), &
       ALLOCATABLE, DIMENSION(:)   :: EquipList                     ! DSU Component  list
END TYPE OpSchemePtrData                      ! DSU

TYPE CompData
  CHARACTER(len=MaxNameLength)     :: TypeOf                = ' '     ! The 'keyWord' identifying  component type
  INTEGER                          :: TypeOf_Num            = 0       ! Reference the "TypeOf" parameters in DataPlant
  INTEGER                          :: GeneralEquipType      = 0       ! General Equipment Type (e.g. Chillers, Pumps, etc)
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Component name
  INTEGER                          :: CompNum               = 0       ! Component ID number
  INTEGER                          :: FlowCtrl              = 0       ! flow control for splitter/mixer (ACTIVE/PASSIVE/BYPASS)
  INTEGER                          :: FlowPriority          = LoopFlowStatus_Unknown  ! status for overall loop flow determination
  LOGICAL                          :: ON                    = .FALSE. ! TRUE = designated component or operation scheme available
  LOGICAL                          :: Available             = .FALSE. ! TRUE = designated component or operation scheme available
  CHARACTER(len=MaxNameLength)     :: NodeNameIn            = ' '     ! Component inlet node name
  CHARACTER(len=MaxNameLength)     :: NodeNameOut           = ' '     ! Component outlet node name
  INTEGER                          :: NodeNumIn             = 0       ! Component inlet node number
  INTEGER                          :: NodeNumOut            = 0       ! Component outlet node number
  REAL(r64)                        :: MyLoad                = 0.0d0   ! Distributed Load
  REAL(r64)                        :: MaxLoad               = 0.0d0   ! Maximum load
  REAL(r64)                        :: MinLoad               = 0.0d0   ! Minimum Load
  REAL(r64)                        :: OptLoad               = 0.0d0   ! Optimal Load
  REAL(r64)                        :: SizFac                = 0.0d0   ! Sizing Fraction
  INTEGER                          :: CurOpSchemeType       = UnknownStatusOpSchemeType  !updated pointer to
                                                                      ! Plant()%OpScheme(CurOpSchemeType)...
  INTEGER                          :: NumOpSchemes          = 0       ! number of schemes held in the pointer array
  INTEGER                          :: CurCompLevelOpNum     = 0       ! pointer to the OpScheme array defined next
                                                                ! Plantloop()%loopside()%branch()%comp()%Opscheme(curopschemeptr)
  TYPE(OpSchemePtrData), &
       ALLOCATABLE, DIMENSION(:)   :: OpScheme                     ! Pointers to component on lists
  REAL(r64)                        :: EquipDemand           = 0.0d0   ! Component load request based on inlet temp and outlet SP
  LOGICAL                          :: EMSCtrl               = .FALSE.
  REAL(r64)                        :: EMSValue              = 0.0d0
  INTEGER                          :: HowLoadServed         = HowMet_Unknown ! nature of component in terms of how it can meet load
  REAL(r64)                        :: MinOutletTemp         = 0.0     ! Component lower limit temperature
  REAL(r64)                        :: MaxOutletTemp         = 0.0     ! Component upper limit temperature
  INTEGER                          :: IndexInLoopSidePumps  = 0       ! If I'm a pump, this tells my index in PL(:)%LS(:)%Pumps
END TYPE CompData

TYPE BranchData
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Name of the branch
  INTEGER                          :: ControlType           = 0
  REAL(r64)                        :: MinVolFlowRate        = 0.0d0
  REAL(r64)                        :: MaxVolFlowRate        = 0.0d0
  REAL(r64)                        :: Requestedmassflow     = 0.0d0
  INTEGER                          :: BranchLevel           = 0
  INTEGER                          :: FlowErrCount          = 0       ! For recurring error counting
  INTEGER                          :: FlowErrIndex          = 0       ! For recurring error index
  INTEGER                          :: TotalComponents       = 0       ! Total number of components on the branch
  INTEGER                          :: NodeNumIn             = 0       ! Component inlet node number
  INTEGER                          :: NodeNumOut            = 0       ! Component outlet node number
  LOGICAL                          :: IsByPass              = .FALSE.
  INTEGER                          :: PumpIndex             = 0
  real(r64)                        :: PumpSizFac            = 1.0d0
  LOGICAL                          :: EMSCtrl               = .FALSE.
  REAL(r64)                        :: EMSValue              = 0.0d0
  TYPE(CompData), &
       ALLOCATABLE, DIMENSION(:)   :: Comp                            ! Component type list
  LOGICAL                          :: HasPressureComponents = .FALSE.
  REAL(r64)                        :: PressureDrop          = 0.0d0
  INTEGER                          :: PressureCurveType     = 0       ! Either none, pressure curve, or generic curve
  INTEGER                          :: PressureCurveIndex    = 0       ! Curve: index for pressure drop calculations
  REAL(r64)                        :: PressureEffectiveK    = 0.0d0
END TYPE BranchData

TYPE EquipListCompData
  CHARACTER(len=MaxNameLength)     :: Name           = ' '                 ! The name of each item in the list
  CHARACTER(len=MaxNameLength)     :: TypeOf           = ' '                 ! The name of each item in the list
  INTEGER                          :: TypeOf_Num         = 0
  CHARACTER(len=MaxNameLength)     :: CtrlType           = ' ' ! CoolingOp, HeatingOp, DualOp
  INTEGER                          :: CtrlTypeNum          ! CoolingOp, HeatingOp, DualOp
  INTEGER                          :: LoopNumPtr          ! pointer to the comp location in the data structure
  INTEGER                          :: LoopSideNumPtr      ! pointer to the comp location in the data structure
  INTEGER                          :: BranchNumPtr        ! pointer to the comp location in the data structure
  INTEGER                          :: CompNumPtr         ! pointer to the comp location in the data structure
  REAL(r64)                        :: SetPointFlowRate       = 0.0d0   ! COMP SETPOINT CTRL ONLY--load calculation comp flow rate
  CHARACTER(len=MaxNameLength)     :: DemandNodeName     = ' '     ! COMP SETPOINT CTRL ONLY--The name of each item in the list
  INTEGER                          :: DemandNodeNum        ! COMP SETPOINT CTRL ONLY--The 'keyWord' identifying each item in list
  CHARACTER(len=MaxNameLength)     :: SetPointNodeName   = ' '     ! COMP SETPOINT CTRL ONLY--The name of each item in the list
  INTEGER                          :: SetpointNodeNum      ! COMP SETPOINT CTRL ONLY--The 'keyWord' identifying each item in list
END TYPE EquipListCompData

TYPE EquipOpList                          ! DSU
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '    ! The name of each item in the list
  REAL(r64)                        :: RangeUpperLimit       = 0.0d0    ! for range based controls
  REAL(r64)                        :: RangeLowerLimit       = 0.0d0    ! for range based controls
  INTEGER                          :: NumComps              = 0      ! ALLOCATABLE to the schedule (for valid schedules)
  TYPE(EquipListCompData), &
       ALLOCATABLE, DIMENSION(:)   :: Comp                ! Component type list
END TYPE EquipOpList                        ! DSU

TYPE OperationData                          ! DSU
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! The name of each item in the list
  CHARACTER(len=MaxNameLength)     :: TypeOf                = ' '     ! The 'keyWord' identifying each item in the list
  INTEGER                          :: OpSchemeType          = 0       ! Op scheme type (from keyword)
  CHARACTER(len=MaxNameLength)     :: Sched                 = ' '     ! The name of the schedule associated with the list
  INTEGER                          :: SchedPtr              = 0       ! ALLOCATABLE to the schedule (for valid schedules)
  LOGICAL                          :: Available             = .FALSE. ! TRUE = designated component or operation scheme available
  INTEGER                          :: NumEquipLists         = 0       ! number of equipment lists
  INTEGER                          :: CurListPtr            = 0       ! points to the current equipment list
  TYPE(EquipOpList), &
       ALLOCATABLE, DIMENSION(:)   :: EquipList                       ! Component type list

  CHARACTER(len=MaxNameLength)     :: ReferenceNodeName     = ' '     ! DELTA CTRL ONLY--for calculation of delta Temp
  INTEGER                          :: ReferenceNodeNumber              ! DELTA CTRL ONLY--for calculation of delta Temp
END TYPE OperationData                        ! DSU

TYPE ConnectedLoopData                        ! DSU
  INTEGER                          :: LoopNum               = 0  ! plant loop index pointer, for the other loop
  INTEGER                          :: LoopSideNum           = 0  ! plant loop side number, for the other loop
  INTEGER                          :: ConnectorTypeOf_Num   = 0  ! plant equipment type doing the connecting
  LOGICAL                          :: LoopDemandsOnRemote    = .FALSE. ! true if this loop puts demand on connected loop
END TYPE ConnectedLoopData                      ! DSU


! two-way common pipe variables
!TYPE TwoWayCommonPipeData
!  LOGICAL   :: SetpointOnSecondarySide  ! true if control point is demand inlet, otherwise supply inlet
!  REAL(r64) :: CurSecCPLegFlow    !Mass flow rate in primary common pipe leg
!  REAL(r64) :: CurPriCPLegFlow    !Mass flow rate in secondary common pipe leg
!  REAL(r64) :: CurSectoPriFlow    !Secondary side to Primary side Mass flow rate
!  REAL(r64) :: CurPritoSecFlow    !Primary side to Secondary side Mass flow rate
!  REAL(r64) :: CurSecOutTemp      !Secondary outlet temperature
!  REAL(r64) :: CurPriOutTemp      !Primary outlet temperature
!  REAL(r64) :: CurPriInTemp       !Primary inlet temperature
!  REAL(r64) :: CurSecInTemp       !Secondary inlet temperature
!  !new/missing?
!  REAL(r64) :: mdotPriRequest     ! primary total flow request
!
!
!END TYPE TwoWayCommonPipeData

INTEGER, PARAMETER :: NumConvergenceHistoryTerms = 5
TYPE PlantConvergencePoint
  REAL(r64), DIMENSION(NumConvergenceHistoryTerms) :: MassFlowRateHistory
  REAL(r64), DIMENSION(NumConvergenceHistoryTerms) :: TemperatureHistory
END TYPE

TYPE HalfLoopData
  LOGICAL                          :: SimLoopSideNeeded     = .TRUE.  ! Determine whether or not to re-simulate this plant loopside
  LOGICAL                          :: SimZoneEquipNeeded    = .TRUE.  ! Plant requests resimulate zone HVAC equipment
  LOGICAL                          :: SimAirLoopsNeeded     = .TRUE.  ! Plant requests resimulate air loop HVAC equipment
  LOGICAL                          :: SimNonZoneEquipNeeded = .TRUE.  ! Plant requests resimulate non zone Equip
  LOGICAL                          :: SimElectLoadCentrNeeded = .TRUE. ! Plant requests resimulate generators
  LOGICAL                          :: OncePerTimeStepOperations = .TRUE.
  REAL(r64)                        :: TimeElapsed           = 0.d0    ! store time for dynamic updates for last time
  REAL(r64)                        :: FlowRequest           = 0.0d0   ! Flow request in the half loop
  REAL(r64)                        :: FlowRequestTemperature = 0.0d0  ! Average Flow request outlet Temp in the half loop
  ! It's necessary to hold the values here since AIR and GROUND SPs aren't associated with either a node or a SP manager
  REAL(r64)                        :: TempSetPoint          = SensedNodeFlagValue    ! Loop temperature setpoint
  REAL(r64)                        :: TempSetPointHi        = SensedNodeFlagValue    ! High Loop temperature setpoint
  REAL(r64)                        :: TempSetPointLO        = SensedNodeFlagValue    ! Low Loop temperature setpoint
  REAL(r64)                        :: TempInterfaceTankOutlet  = 0.0d0   ! Used by interface manager in common pipe simulation
                                                                      ! This is the temperature at the loop outlet linterface
                                                                      ! with half-loop capacitance and pump heat accounted for.
  REAL(r64)                        :: LastTempInterfaceTankOutlet  = 0.0d0
  CHARACTER(len=MaxNameLength)     :: BranchList            = ' '     ! Branch list name for the half loop
  CHARACTER(len=MaxNameLength)     :: ConnectList           = ' '     ! Connector list name for the half loop
  INTEGER                          :: TotalBranches         = 0       ! Total number of branches on the half loop
  INTEGER                          :: NodeNumIn             = 0       ! Node number for the inlet to this loop
  CHARACTER(len=MaxNameLength)     :: NodeNameIn            = ' '     ! Node name for the inlet to this loop
  INTEGER                          :: NodeNumOut            = 0       ! Node number for the outlet to this loop
  CHARACTER(len=MaxNameLength)     :: NodeNameOut           = ' '     ! Node name for the outlet to this loop
  INTEGER                          :: NumSplitters          = 0       ! Number of splitters in the half loop
  INTEGER                          :: NumMixers             = 0       ! Number of mixers in the half loop
  LOGICAL                          :: SplitterExists        = .FALSE. ! Logical Flag indication splitter exists in the half loop
  LOGICAL                          :: MixerExists           = .FALSE. ! Logical Flag indication mixer exists in the half loop
  INTEGER                          :: TotalPumps            = 0       ! total number of pumps on the half loop
  LOGICAL                          :: BranchPumpsExist      = .FALSE. ! logical flag indication branch pumps exist on half loop
  TYPE(LoopSidePumpInformation), &
       ALLOCATABLE, DIMENSION(:)   :: Pumps
  REAL(r64)                        :: TotalPumpHeat         = 0.0d0   ! [W] total heat addition by the pumps to place in "tank"
  LOGICAL                          :: ByPassExists          = .FALSE.
  LOGICAL                          :: InletNodeSetPt        = .FALSE.
  LOGICAL                          :: OutletNodeSetPt       = .FALSE.
  LOGICAL                          :: EMSCtrl               = .FALSE.
  REAL(r64)                        :: EMSValue
  LOGICAL                          :: FlowRestrictionFlag   = .FALSE. ! Max available flow at the outlet of the half loop
                                                                      ! is less than max available flow at inlet
  INTEGER                          :: FlowLock              = 0       !DSU
  INTEGER                          :: TotalConnected        = 0       ! total number of other loops connected to this loop side
  TYPE (ConnectedLoopData), &
       ALLOCATABLE, DIMENSION(:)   :: Connected                       !DSU Other loops connected to this Loop side
  TYPE (BranchData), &
       ALLOCATABLE, DIMENSION(:)   :: Branch                          ! Branch data
  TYPE (SplitterData), &
       ALLOCATABLE, DIMENSION(:)   :: Splitter                        ! Data for splitter on branch (if any)
  TYPE (MixerData), &
       ALLOCATABLE, DIMENSION(:)   :: Mixer                           ! Data for splitter on branch (if any)
  LOGICAL                          :: HasPressureComponents = .FALSE.
  LOGICAL                          :: HasParallelPressComps = .FALSE.
  REAL(r64)                        :: PressureDrop          = 0.0d0
  REAL(r64)                        :: PressureEffectiveK    = 0.0d0
  INTEGER                          :: errCount_LoadWasntDist= 0
  INTEGER                          :: errIndex_LoadWasntDist= 0
  INTEGER                          :: errCount_LoadRemains  = 0
  INTEGER                          :: errIndex_LoadRemains  = 0
  REAL(r64)                        :: LoopSideInlet_TankTemp = 0.0d0
  TYPE(PlantConvergencePoint)      :: InletNode = PlantConvergencePoint(0.0d0,0.0d0)
  TYPE(PlantConvergencePoint)      :: OutletNode = PlantConvergencePoint(0.0d0,0.0d0)
END TYPE HalfLoopData

TYPE PlantLoopData
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Name of the component list
  CHARACTER(len=MaxNameLength)     :: FluidName             = ' '     ! Name of the fluid specified for this loop
  INTEGER                          :: FluidType             = 0       ! Type of fluid in the loop
  INTEGER                          :: FluidIndex            = 0       ! Index for Fluid in FluidProperties
  INTEGER                          :: MFErrIndex            = 0       ! for recurring mass flow errors
  ! Loop Operating Setpoints and Limits

  INTEGER                          :: TempSetPointNodeNum   = 0       ! Node Number for Loop Temp SP associated with SP manager
  INTEGER                          :: MaxBranch             = 0       ! Max branches in the loop
  REAL(r64)                        :: MinTemp               = 0.0d0   ! Minimum temperature allowed in the loop
  REAL(r64)                        :: MaxTemp               = 0.0d0   ! Maximum temperature allowed in the loop
  INTEGER                          :: MinTempErrIndex       = 0       ! for recurring too cold errors
  INTEGER                          :: MaxTempErrIndex       = 0       ! for recurring too hot errors
  REAL(r64)                        :: MinVolFlowRate        = 0.0d0   ! Minimum flow rate allowed in the loop
  REAL(r64)                        :: MaxVolFlowRate        = 0.0d0   ! Maximum flow rate allowed in the loop
  REAL(r64)                        :: MinMassFlowRate        = 0.0d0   ! Minimum flow rate allowed in the loop
  REAL(r64)                        :: MaxMassFlowRate        = 0.0d0   ! Maximum flow rate allowed in the loop
  REAL(r64)                        :: Volume                = 0.0d0   ! Volume of the fluid in the loop
  REAL(r64)                        :: Mass                  = 0.0d0   ! Mass of the fluid in the loop
  LOGICAL                          :: EMSCtrl               = .FALSE.
  REAL(r64)                        :: EMSValue              = 0.0d0
  ! Loop Inlet and Outlet Nodes
  TYPE(HalfLoopData), &
       ALLOCATABLE, DIMENSION(:)   :: LoopSide                        ! Half loop data (Demand side or Supply Side)
  CHARACTER(len=MaxNameLength)     :: OperationScheme       = ' '     ! Operation scheme name for the loop
  INTEGER                          :: NumOpSchemes          = 0       ! Number of items in list identified by "OpScheme"
  TYPE(OperationData), &
       ALLOCATABLE, DIMENSION(:)   :: OpScheme                        ! Operation scheme data
  INTEGER                          :: LoadDistribution      = 0       ! Load distribution scheme 1 for optimal, 2 for overloading
  INTEGER                          :: PlantSizNum           = 0       ! index to corresponding plant sizing data array
  INTEGER                          :: LoopDemandCalcScheme  = 0       ! Load distribution scheme 1 SingleSetPoint,
                                                                      ! 2 DualSetPointwithDeadBand
  INTEGER                          :: CommonPipeType        = 0
!  TYPE(TwoWayCommonPipeData), &
!       ALLOCATABLE                 :: TwoWayCommonPipe                ! two-way common pipe data, primary secondary loop model

  CHARACTER(len=MaxNameLength)     :: EconomizerHtExchanger       = ' ' !DSU review, should move these out of here
  CHARACTER(len=MaxNameLength)     :: EconPlantSideSensedNodeName = ' ' !DSU review, should move these out of here
  CHARACTER(len=MaxNameLength)     :: EconCondSideSensedNodeName  = ' ' !DSU review, should move these out of here
  INTEGER                          :: EconPlantSideSensedNodeNum  = 0 !DSU review, should move these out of here
  INTEGER                          :: EconCondSideSensedNodeNum   = 0 !DSU review, should move these out of here
  INTEGER                          :: EconPlacement         = 0 !DSU review, should move these out of here
  INTEGER                          :: EconBranch            = 0 !DSU review, should move these out of here
  INTEGER                          :: EconComp              = 0 !DSU review, should move these out of here
  REAL(r64)                        :: EconControlTempDiff   = 0.0d0 !DSU review, should move these out of here
  LOGICAL                          :: LoopHasConnectionComp = .FALSE.
  INTEGER                          :: TypeOfLoop = 0
  INTEGER                          :: PressureSimType       = 1
  LOGICAL                          :: HasPressureComponents = .FALSE.
  REAL(r64)                        :: PressureDrop          = 0.0d0
  LOGICAL                          :: UsePressureForPumpCalcs = .FALSE.
  REAL(r64)                        :: PressureEffectiveK    = 0.0d0
END TYPE PlantLoopData

TYPE ComponentData
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Name of the component
  CHARACTER(len=MaxNameLength)     :: NodeNameIn            = ' '     ! Name of Node In
  CHARACTER(len=MaxNameLength)     :: NodeNameOut           = ' '     ! Name of Node Out
  INTEGER                          :: NodeNumIn             = 0       ! Inlet node number
  INTEGER                          :: NodeNumOut            = 0       ! Outlet node number
END TYPE ComponentData

TYPE PipeData
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Pipe name
  INTEGER                          :: TypeOf                = 0
  INTEGER                          :: EquipNum              = 0
  INTEGER                          :: FlowCtrl              = 0       ! Pipe control (should always be BYPASS)
  CHARACTER(len=MaxNameLength)     :: NodeNameIn            = ' '     ! Pipe inlet node name
  INTEGER                          :: NodeNumIn             = 0       ! Pipe inlet node number
  CHARACTER(len=MaxNameLength)     :: NodeNameOut           = ' '     ! Pipe outlet node name
  INTEGER                          :: NodeNumOut            = 0       ! Pipe outlet node number
  INTEGER                          :: ParentHalfLoop        = 0       ! Half loop where the pipe is present
END TYPE PipeData

! The same as TYPE DefinePriAirSysAvailMgrs in DataAirLoop.f90.  A common definition would be nicer.
TYPE PlantAvailMgrData
  INTEGER                          :: NumAvailManagers      = 0       ! number of availability managers for this plant loop
  INTEGER                          :: AvailStatus           = 0       ! system availability status
  INTEGER                          :: StartTime             = 0       ! cycle on time (in SimTimeSteps)
  INTEGER                          :: StopTime              = 0       ! cycle off time (in SimTimeSteps)
  CHARACTER(len=MaxNameLength), &
          ALLOCATABLE, DIMENSION(:):: AvailManagerName                ! name of each availability manager
  INTEGER,ALLOCATABLE, DIMENSION(:):: AvailManagerType                ! type of availability manager
  INTEGER,ALLOCATABLE, DIMENSION(:):: AvailManagerNum                 ! index of availability manager
END TYPE PlantAvailMgrData

TYPE LoopSideReportVars
  REAL(r64)                        :: LoopSetPtDemandAtInlet = 0.0d0
  REAL(r64)                        :: ThisSideLoadAlterations = 0.0d0
END TYPE

TYPE ReportVars
  ! Whole loop descriptions
  REAL(r64)                        :: CoolingDemand         = 0.d0   ! Plant Loop Cooling Demand, W
  REAL(r64)                        :: HeatingDemand         = 0.d0   ! Plant Loop Heating Demand[W]
  REAL(r64)                        :: DemandNotDispatched   = 0.d0   ! Plant Loop Demand that was not distributed [W]
  REAL(r64)                        :: UnmetDemand           = 0.d0   ! Plant Loop Unmet Demand [W]

  ! Loop side data
  TYPE(LoopSideReportVars), DIMENSION(2) :: LoopSide

  REAL(r64)                        :: BypassFrac            = 0.d0   ! Debug Variable
  REAL(r64)                        :: InletNodeFlowrate     = 0.d0   ! Debug Variable
  REAL(r64)                        :: InletNodeTemperature  = 0.d0   ! Debug Variable
  REAL(r64)                        :: OutletNodeFlowrate    = 0.d0   ! Debug Variable
  REAL(r64)                        :: OutletNodeTemperature = 0.d0   ! Debug Variable
  INTEGER                          :: LastLoopSideSimulated = 0
END TYPE ReportVars

TYPE PlantConnection
  INTEGER                          :: LoopType              = 0       !1 = Plant, 2 = Condenser
  INTEGER                          :: LoopNum               = 0
  INTEGER                          :: BranchNum             = 0
  INTEGER                          :: CompNum               = 0
END TYPE PlantConnection

TYPE MeterData
  CHARACTER(len=MaxNameLength)     :: ReportVarName         = ' '
  CHARACTER(len=15)                :: ReportVarUnits        = ' '
  INTEGER                          :: ResourceType          = 0
  CHARACTER(len=MaxNameLength)     :: EndUse                = ' '
  INTEGER                          :: EndUse_CompMode       = 0
  CHARACTER(len=MaxNameLength)     :: Group                 = ' '
  INTEGER                          :: ReportVarIndex        = 0
  INTEGER                          :: ReportVarIndexType    = 0
  INTEGER                          :: ReportVarType         = 0
  REAL(r64)                        :: CurMeterReading       = 0.0d0
END TYPE MeterData

TYPE SubSubcomponentData  !data for an individual component
  CHARACTER(len=MaxNameLength)     :: TypeOf                = ' '     ! The 'keyWord' identifying  component type
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Component name
  INTEGER                          :: CompIndex             = 0       ! Component Index in whatever is using this component
  CHARACTER(len=MaxNameLength)     :: NodeNameIn            = ' '     ! Component inlet node name
  CHARACTER(len=MaxNameLength)     :: NodeNameOut           = ' '     ! Component outlet node name
  LOGICAL                          :: ON                    = .TRUE.  ! TRUE = designated component or operation scheme available
  INTEGER                          :: NodeNumIn             = 0       ! Component inlet node number
  INTEGER                          :: NodeNumOut            = 0       ! Component outlet node number
  LOGICAL                          :: MeteredVarsFound      = .FALSE.
  INTEGER                          :: NumMeteredVars        = 0
  INTEGER                          :: EnergyTransComp       = 0       !1=EnergyTransfer, 0=No EnergyTransfer - Reporting flag
  REAL(r64)                        :: TotPlantSupplyElec    = 0.0d0
  REAL(r64)                        :: PlantSupplyElecEff    = 0.0d0
  REAL(r64)                        :: PeakPlantSupplyElecEff= 0.0d0
  REAL(r64)                        :: TotPlantSupplyGas     = 0.0d0
  REAL(r64)                        :: PlantSupplyGasEff     = 0.0d0
  REAL(r64)                        :: PeakPlantSupplyGasEff = 0.0d0
  REAL(r64)                        :: TotPlantSupplyPurch   = 0.0d0
  REAL(r64)                        :: PlantSupplyPurchEff   = 0.0d0
  REAL(r64)                        :: PeakPlantSupplyPurchEff=0.0d0
  REAL(r64)                        :: TotPlantSupplyOther   = 0.0d0
  REAL(r64)                        :: PlantSupplyOtherEff   = 0.0d0
  REAL(r64)                        :: PeakPlantSupplyOtherEff=0.0d0
  REAL(r64)                        :: Capacity              = 0.0d0
  REAL(r64)                        :: Efficiency            = 0.0d0
  INTEGER                          :: OpMode                = 0
  TYPE(MeterData),ALLOCATABLE, &
                   DIMENSION(:)    :: MeteredVar                      ! Index of energy output report data
  INTEGER                          :: AirSysToPlantPtr=0              ! 0=No plant connection, >0 = index to AirSysToPlant array
END TYPE SubSubcomponentData

TYPE SubcomponentData  !data for an individual component
  CHARACTER(len=MaxNameLength)     :: TypeOf                = ' '     ! The 'keyWord' identifying  component type
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Component name
  INTEGER                          :: CompIndex             = 0       ! Component Index in whatever is using this component
  LOGICAL                          :: Parent                = .FALSE. ! TRUE = designated component is made up of sub-components
  INTEGER                          :: NumSubSubcomps        = 0
  CHARACTER(len=MaxNameLength)     :: NodeNameIn            = ' '     ! Component inlet node name
  CHARACTER(len=MaxNameLength)     :: NodeNameOut           = ' '     ! Component outlet node name
  INTEGER                          :: NodeNumIn             = 0       ! Component inlet node number
  INTEGER                          :: NodeNumOut            = 0       ! Component outlet node number
  LOGICAL                          :: MeteredVarsFound      = .FALSE.
  LOGICAL                          :: ON                    = .TRUE.  ! TRUE = designated component or operation scheme available
  INTEGER                          :: NumMeteredVars        = 0
  INTEGER                          :: EnergyTransComp       = 0       ! 1=EnergyTransfer, 0=No EnergyTransfer - Reporting flag
  REAL(r64)                        :: Capacity              = 0.0d0
  REAL(r64)                        :: Efficiency            = 0.0d0
  INTEGER                          :: OpMode                = 0
  REAL(r64)                        :: TotPlantSupplyElec    = 0.0d0
  REAL(r64)                        :: PlantSupplyElecEff    = 0.0d0
  REAL(r64)                        :: PeakPlantSupplyElecEff= 0.0d0
  REAL(r64)                        :: TotPlantSupplyGas     = 0.0d0
  REAL(r64)                        :: PlantSupplyGasEff     = 0.0d0
  REAL(r64)                        :: PeakPlantSupplyGasEff = 0.0d0
  REAL(r64)                        :: TotPlantSupplyPurch   = 0.0d0
  REAL(r64)                        :: PlantSupplyPurchEff   = 0.0d0
  REAL(r64)                        :: PeakPlantSupplyPurchEff=0.0d0
  REAL(r64)                        :: TotPlantSupplyOther   = 0.0d0
  REAL(r64)                        :: PlantSupplyOtherEff   = 0.0d0
  REAL(r64)                        :: PeakPlantSupplyOtherEff=0.0d0
  INTEGER                          :: AirSysToPlantPtr      = 0       ! 0=No plant connection, >0 = index to AirSysToPlant array
  REAL(r64)                        :: LoopLoadFrac          = 0.0d0
  TYPE(MeterData), &
         ALLOCATABLE, DIMENSION(:) :: MeteredVar                      !Index of energy output report data
  Type (SubSubcomponentData), &
         ALLOCATABLE, DIMENSION(:) :: SubSubComp                      ! Component list
END TYPE SubcomponentData

          ! The next three types (all starting with RepReport) are the "shadow"
          ! derived types for the ventilation reports.  It keeps the node and
          ! other connection information and adds variables for the ventilation
          ! reports.  This is the cleanest way to do this and not impact other
          ! data structures.  The actual derived types are defined (as allocatable)
          ! below with the rest of the declarations.

TYPE ReportCompData
  LOGICAL                          :: Parent                =.FALSE.  ! TRUE = designated component is made up of sub-components
  CHARACTER(len=MaxNameLength)     :: TypeOf                = ' '     ! The 'keyWord' identifying  component type
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Component name
  INTEGER                          :: CompIndex             = 0       ! Component Index in whatever is using this component
  CHARACTER(len=MaxNameLength)     :: NodeNameIn            = ' '     ! Component inlet node name
  CHARACTER(len=MaxNameLength)     :: NodeNameOut           = ' '     ! Component outlet node name
  INTEGER                          :: NodeNumIn             = 0       ! Component inlet node number
  INTEGER                          :: NodeNumOut            = 0       ! Component outlet node number
  INTEGER                          :: NumMeteredVars        = 0
  INTEGER                          :: NumSubComps           = 0
  REAL(r64)                        :: LoopLoadFrac          = 0.0d0   ! Fraction of loop load met by component
  REAL(r64)                        :: TotPlantSupplyElec    = 0.0d0
  REAL(r64)                        :: TotPlantSupplyGas     = 0.0d0
  REAL(r64)                        :: TotPlantSupplyPurch   = 0.0d0
  REAL(r64)                        :: TotPlantSupplyOther   = 0.0d0
  TYPE(PlantConnection)            :: ConnectPlant                    ! Index of energy output report data
  TYPE(MeterData), &
       ALLOCATABLE, DIMENSION(:)   :: MeteredVar                      ! Index of energy output report data
  TYPE (SubComponentData), &
       ALLOCATABLE, DIMENSION(:)   :: SubComp
END TYPE ReportCompData

TYPE ReportBranchData
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Name of the branch
  INTEGER                          :: TotalComponents       = 0       ! Total number of components on the branch
  INTEGER                          :: NodeNumIn             = 0       ! Branch inlet node number
  INTEGER                          :: NodeNumOut            = 0       ! Branch outlet node number
TYPE(ReportCompData), &
         ALLOCATABLE, DIMENSION(:) :: Comp                            ! Component type list
END TYPE ReportBranchData

TYPE ReportLoopData
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Name of the component list
  INTEGER                          :: NodeNumIn             = 0       ! Node number for the inlet to this loop
  CHARACTER(len=MaxNameLength)     :: NodeNameIn            = ' '     ! Node name for the inlet to this loop
  INTEGER                          :: NodeNumOut            = 0       ! Node number for the outlet to this loop
  CHARACTER(len=MaxNameLength)     :: NodeNameOut           = ' '     ! Node name for the outlet to this loop
  REAL(r64)                        :: Electric              = 0.0d0
  REAL(r64)                        :: Gas                   = 0.0d0
  REAL(r64)                        :: Purchased             = 0.0d0
  REAL(r64)                        :: OtherEnergy           = 0.0d0
  INTEGER                          :: TotalBranches         = 0       ! Total number of branches on the loop
  REAL(r64)                        :: LoopVentLoad          = 0.0d0
  REAL(r64)                        :: VentLoadFrac          = 0.0d0
  TYPE (ReportBranchData), &
         ALLOCATABLE, DIMENSION(:) :: Branch                          ! Branch data
END TYPE ReportLoopData

TYPE PlantCallingOrderInfoStruct
  INTEGER :: LoopIndex = 0 ! plant or condenser loop indexes in calling order
  INTEGER :: LoopSide = 0 ! plant or condenser loop sides in calling order
!  INTEGER :: InterAct1LoopIndex     = 0 ! primary interaction dependency reference loop index
!  INTEGER :: InterAct1LoopSide      = 0 ! primary interaction dependency reference loop side
!  INTEGER :: InterAct2LoopIndex     = 0 ! secondary interaction dependency reference loop index
!  INTEGER :: InterAct2LoopSide      = 0 ! secondary interaction dependency reference loop side
  INTEGER :: LoopPumpSimulationType = 0 ! type of pump topology on half loop
END TYPE

  !MODULE VARIABLE DECLARATIONS:

INTEGER                            :: NumPipes              = 0       ! Total number of pipes
INTEGER                            :: NumPlantPipes         = 0       ! Total number of plant pipes
INTEGER                            :: NumCondPipes          = 0       ! Total number of condenser pipes
REAL(r64)                          :: EconLoadMet           = 0.0d0   ! Load met by Economizer
INTEGER                            :: TotNumLoops           = 0       ! number of plant and condenser loops
INTEGER                            :: TotNumHalfLoops       = 0       ! number of half loops (2 * TotNumLoops)
LOGICAL                            :: PlantSizeNotComplete  = .TRUE.
LOGICAL                            :: PlantSizesOkayToFinalize = .FALSE. ! true when plant sizing is finishing and it is okay to save results


INTEGER, ALLOCATABLE, DIMENSION(:) :: EconBranchNum                   ! Branch num on which economizer is placed
INTEGER, ALLOCATABLE, DIMENSION(:) :: EconCompNum                     ! Component num of economizer in the economizer branch

LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckLoopEcon                   ! Flag for initializations
LOGICAL, ALLOCATABLE, DIMENSION(:) :: EconOn                          ! Flag specifying if economizer is ON

LOGICAL, ALLOCATABLE, DIMENSION(:) :: SimSupplySide                   !DSU, sim ctrl flag for plant supply sides
LOGICAL, ALLOCATABLE, DIMENSION(:) :: SimDemandSide                   !DSU, sim ctrl flag for plant supply sides

LOGICAL, ALLOCATABLE, DIMENSION(:)  :: LoadChangeDownStream  ! sim control flag.

INTEGER :: PlantManageSubIterations = 0 ! tracks plant iterations to characterize solver
INTEGER :: PlantManageHalfLoopCalls = 0 ! tracks number of half loop calls

! two-way common pipe variables
!REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurSecCPLegFlow    !Mass flow rate in primary common pipe leg
!REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurPriCPLegFlow    !Mass flow rate in secondary common pipe leg
!REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurSectoPriFlow    !Secondary side to Primary side Mass flow rate
!REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurPritoSecFlow    !Primary side to Secondary side Mass flow rate
!REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurSecOutTemp      !Secondary outlet temperature
!REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurPriOutTemp      !Primary outlet temperature
!REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurPriInTemp       !Primary inlet temperature
!REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurSecInTemp       !Secondary inlet temperature

TYPE (PipeData),          ALLOCATABLE, DIMENSION(:) :: Pipe
TYPE (PlantLoopData),     ALLOCATABLE, DIMENSION(:) :: PlantLoop
TYPE (PlantAvailMgrData), ALLOCATABLE, DIMENSION(:) :: PlantAvailMgr
TYPE (ReportVars),        ALLOCATABLE, DIMENSION(:) :: PlantReport
TYPE (ReportLoopData),    ALLOCATABLE, DIMENSION(:) :: VentRepPlantSupplySide
TYPE (ReportLoopData),    ALLOCATABLE, DIMENSION(:) :: VentRepPlantDemandSide
TYPE (ReportLoopData),    ALLOCATABLE, DIMENSION(:) :: VentRepCondSupplySide
TYPE (ReportLoopData),    ALLOCATABLE, DIMENSION(:) :: VentRepCondDemandSide
TYPE (PlantCallingOrderInfoStruct), ALLOCATABLE, DIMENSION(:) :: PlantCallingOrderInfo

! Routines within this module
PUBLIC  ScanPlantLoopsForObject
PUBLIC  ScanPlantLoopsForNodeNum
PUBLIC  AnyPlantLoopSidesNeedSim
PUBLIC  SetAllPlantSimFlagsToValue
PUBLIC  GetLoopSidePumpIndex
PUBLIC  ShowBranchesOnLoop

CONTAINS

SUBROUTINE ScanPlantLoopsForObject(CompName, CompType, LoopNum, LoopSideNum, BranchNum, CompNum, &
                                   LowLimitTemp, HighLimitTemp, CountMatchPlantLoops, InletNodeNumber, &
                                   errFlag, SingleLoopSearch)

   ! SUBROUTINE INFORMATION:
   !       AUTHOR         Edwin Lee
   !       DATE WRITTEN   November 2009
   !       MODIFIED       B. Griffith, changes to help with single component one multiple plant loops
   !       RE-ENGINEERED  na
   !
   ! PURPOSE OF THIS SUBROUTINE:
   ! This subroutine scans the plant loop structure trying to find the component by type then name.
   ! If there are more than one match, it counts them up and returns count using an optional output arg
   ! If the option input declaring the component inlet's node name, then the matching is more specific.
   ! An optional input, lowlimittemp, can be passed in to be used in the PlantCondLoopOperation routines
   !  when distributing loads to components
   !
   ! METHODOLOGY EMPLOYED:
   ! Standard EnergyPlus methodology.
   !
   ! REFERENCES:
   ! na
   !
   ! USE STATEMENTS:
USE DataGlobals
USE DataInterfaces, ONLY: ShowSevereError, ShowFatalError, ShowContinueError
USE InputProcessor, ONLY : SameString
USE General,        ONLY : RoundSigDigits

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

   ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*),    INTENT(IN)   :: CompName
INTEGER,             INTENT(IN)   :: CompType
INTEGER,             INTENT(OUT)  :: LoopNum
INTEGER,             INTENT(OUT)  :: LoopSideNum
INTEGER,             INTENT(OUT)  :: BranchNum
INTEGER,             INTENT(OUT)  :: CompNum
REAL(r64), OPTIONAL, INTENT(IN)   :: LowLimitTemp
REAL(r64), OPTIONAL, INTENT(IN)   :: HighLimitTemp
INTEGER,  OPTIONAL,  INTENT(OUT)  :: CountMatchPlantLoops
INTEGER,  OPTIONAL,  INTENT(IN)   :: InletNodeNumber
LOGICAL, OPTIONAL,   INTENT(INOUT):: errFlag
INTEGER,  OPTIONAL,  INTENT(IN)   :: SingleLoopSearch

   ! SUBROUTINE PARAMETER DEFINITIONS:
   ! na

   ! INTERFACE BLOCK SPECIFICATIONS
   ! na

   ! DERIVED TYPE DEFINITIONS
   ! na

   ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: LoopCtr
  INTEGER     :: LoopSideCtr
  INTEGER     :: BranchCtr
  INTEGER     :: CompCtr
  LOGICAL     :: FoundComponent
  INTEGER     :: FoundCount
  LOGICAL     :: FoundCompName
  INTEGER     :: StartingLoopNum
  INTEGER     :: EndingLoopNum
  logical :: printsteps

  FoundCount = 0

  FoundComponent = .FALSE.
  FoundCompName  = .false.
  if (compname == 'GASBWKESSEL') THEN
    printsteps=.true.
  ELSE
    printsteps=.false.
  ENDIF
  StartingLoopNum = 1
  EndingLoopNum   = TotNumLoops
  IF ( PRESENT ( SingleLoopSearch ) ) THEN
    StartingLoopNum = SingleLoopSearch
    EndingLoopNum   = SingleLoopSearch
  END IF

  PlantLoops: DO LoopCtr = StartingLoopNum, EndingLoopNum
    DO LoopSideCtr = 1, 2
      DO BranchCtr = 1, PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%TotalBranches
        DO CompCtr = 1, PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%TotalComponents
          IF(PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%TypeOf_Num == CompType) THEN
            IF(SameString(CompName, PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%Name))THEN
              FoundCompName=.true.
              IF ( PRESENT(InletNodeNumber)) THEN
                IF (InletNodeNumber > 0) THEN
                  ! check if inlet nodes agree
                  IF (InletNodeNumber == PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%NodeNumIn) THEN
                    FoundComponent = .TRUE.
                    FoundCount  = FoundCount + 1
                    LoopNum     = LoopCtr
                    LoopSideNum = LoopSideCtr
                    BranchNum   = BranchCtr
                    CompNum     = CompCtr
                  ENDIF
                ENDIF
              ELSE
                FoundComponent = .TRUE.
                FoundCount  = FoundCount + 1
                LoopNum     = LoopCtr
                LoopSideNum = LoopSideCtr
                BranchNum   = BranchCtr
                CompNum     = CompCtr
              ENDIF
              IF (PRESENT(LowLimitTemp)) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%MinOutletTemp = LowLimitTemp
              END IF
              IF (PRESENT(HighLimitTemp)) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%MaxOutletTemp = HighLimitTemp
              ENDIF
            END IF
          END IF
        END DO
      END DO
    END DO
  END DO PlantLoops

  IF (.NOT. FoundComponent) THEN
    IF (CompType >= 1 .and. CompType <= NumSimPlantEquipTypes) THEN
      IF (.not. PRESENT(SingleLoopSearch)) THEN
      CALL ShowSevereError('Plant Component '//trim(ccSimPlantEquipTypes(CompType))//' called "'//trim(CompName)//  &
         '" was not found on any plant loops.')
      ELSE
        CALL ShowSevereError('Plant Component '//trim(ccSimPlantEquipTypes(CompType))//' called "'//trim(CompName)//  &
           '" was not found on plant loop="'//trim(PlantLoop(SingleLoopSearch)%Name)//'".')
      ENDIF
      IF (PRESENT(InletNodeNumber)) THEN
      IF (FoundCompName) THEN
        CALL ShowContinueError('Looking for matching inlet Node="'//trim(NodeID(InletNodeNumber))//'".')
        ENDIF
      ENDIF
      IF (PRESENT(SingleLoopSearch)) THEN
        CALL ShowContinueError('Look at Operation Scheme="'//trim(PlantLoop(SingleLoopSearch)%OperationScheme)//'".')
        CALL ShowContinueError('Look at Branches and Components on the Loop.')
        CALL ShowBranchesOnLoop(SingleLoopSearch)
      ENDIF
      If (PRESENT(errFlag)) errFlag=.true.
    ELSE
      CALL ShowSevereError('ScanPlantLoopsForObject: Invalid CompType passed ['//trim(RoundSigDigits(CompType))//  &
         '], Name='//trim(CompName))
      CALL ShowContinueError('Valid CompTypes are in the range [1 - '//trim(RoundSigDigits(NumSimPlantEquipTypes))//  &
         '].')
      CALL ShowFatalError('Previous error causes program termination')
    ENDIF
  END IF

  IF (PRESENT(CountMatchPlantLoops)) THEN
    CountMatchPlantLoops = FoundCount
  ENDIF

END SUBROUTINE ScanPlantLoopsForObject

SUBROUTINE ScanPlantLoopsForNodeNum(CallerName, NodeNum, LoopNum, LoopSideNum, BranchNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb. 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get routine to return plant loop index and plant loop side
          ! based on node number.  for one time init routines only.

          ! METHODOLOGY EMPLOYED:
          ! Loop thru plant data structure and find matching node.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataGlobals
USE DataInterfaces, ONLY: ShowSevereError, ShowContinueError
USE General,        ONLY : RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),    INTENT(IN)   :: CallerName ! really used for error messages
  INTEGER, INTENT(IN)  :: NodeNum ! index in Node structure of node to be scanned
  INTEGER, INTENT(OUT) :: LoopNum ! return value for plant loop
  INTEGER, INTENT(OUT) :: LoopSideNum ! return value for plant loop side
  INTEGER, INTENT(OUT) :: BranchNum !

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER     :: LoopCtr
  INTEGER     :: LoopSideCtr
  INTEGER     :: BranchCtr
  INTEGER     :: CompCtr
  LOGICAL     :: FoundNode
  INTEGER     :: FoundCount

  FoundCount = 0

  FoundNode = .FALSE.

  PlantLoops: DO LoopCtr = 1, TotNumLoops
    DO LoopSideCtr = 1, 2
      DO BranchCtr = 1, PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%TotalBranches
        DO CompCtr = 1, PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%TotalComponents

         IF (NodeNum == PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%NodeNumIn) THEN
           FoundNode = .TRUE.
           FoundCount  = FoundCount + 1
           LoopNum     = LoopCtr
           LoopSideNum = LoopSideCtr
           BranchNum   = BranchCtr
         ENDIF

         IF (NodeNum == PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%NodeNumOut) THEN
           FoundCount  = FoundCount + 1
           LoopNum     = LoopCtr
           LoopSideNum = LoopSideCtr
           BranchNum   = BranchCtr
         ENDIF

        END DO
      END DO
    END DO
  END DO PlantLoops

  IF (.NOT. FoundNode) THEN
      CALL ShowSevereError('Plant Node was not found on any plant loops')
      Call ShowContinueError('Node number: ' //Trim(RoundSigDigits(NodeNum)) )
      Call ShowContinueError('ScanPlantLoopsForNodeNum: called by '//Trim(CallerName))
      ! fatal?
  END IF

  RETURN

END SUBROUTINE ScanPlantLoopsForNodeNum

LOGICAL FUNCTION AnyPlantLoopSidesNeedSim()

   ! FUNCTION INFORMATION:
   !       AUTHOR         Edwin Lee
   !       DATE WRITTEN   November 2009
   !       MODIFIED       na
   !       RE-ENGINEERED  na
   !
   ! PURPOSE OF THIS FUNCTION:
   ! This subroutine scans the plant loopside simflags and returns if any of them are still true
   !
   ! METHODOLOGY EMPLOYED:
   ! Standard EnergyPlus methodology.
   !
   ! REFERENCES:
   ! na
   !
   ! USE STATEMENTS:
   ! na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

   ! FUNCTION ARGUMENT DEFINITIONS:
   ! na

   ! FUNCTION PARAMETER DEFINITIONS:
   ! na

   ! INTERFACE BLOCK SPECIFICATIONS
   ! na

   ! DERIVED TYPE DEFINITIONS
   ! na

   ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: LoopCtr
  INTEGER     :: LoopSideCtr

  !Assume that there aren't any
  AnyPlantLoopSidesNeedSim = .FALSE.

  !Then check if there are any
  DO LoopCtr = 1, TotNumLoops
    DO LoopSideCtr = 1, 2
      IF (PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%SimLoopSideNeeded) THEN
        AnyPlantLoopSidesNeedSim = .TRUE.
        RETURN
      END IF
    END DO
  END DO

END FUNCTION AnyPlantLoopSidesNeedSim

SUBROUTINE SetAllPlantSimFlagsToValue(Value)

   ! SUBROUTINE INFORMATION:
   !       AUTHOR         Edwin Lee
   !       DATE WRITTEN   November 2009
   !       MODIFIED       na
   !       RE-ENGINEERED  B. Griffith Feb 2009 DSU3
   !
   ! PURPOSE OF THIS SUBROUTINE:
   ! Quickly sets all sim flags of a certain type (loop type/side) to a value
   !
   ! USE STATEMENTS:
 USE DataHVACGlobals, ONLY : NumPlantLoops, NumCondLoops

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

   ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: Value

   ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: LoopCtr

  !Loop over all loops
  DO LoopCtr = 1, TotNumLoops
    PlantLoop(LoopCtr)%LoopSide(DemandSide)%SimLoopSideNeeded = Value
    PlantLoop(LoopCtr)%LoopSide(SupplySide)%SimLoopSideNeeded = Value
  END DO

END SUBROUTINE SetAllPlantSimFlagsToValue

INTEGER FUNCTION GetLoopSidePumpIndex(LoopNum, LoopSideNum, BranchNum, CompNum)

   ! FUNCTION INFORMATION:
   !       AUTHOR         Edwin Lee
   !       DATE WRITTEN   April 2010
   !       MODIFIED       na
   !       RE-ENGINEERED  na
   !
   ! PURPOSE OF THIS FUNCTION:
   ! This subroutine scans the plant loopside pumps data structure, and returns the index or zero
   !
   ! METHODOLOGY EMPLOYED:
   ! Standard EnergyPlus methodology.
   !
   ! REFERENCES:
   ! na
   !
   ! USE STATEMENTS:
   ! na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

   ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: LoopNum
  INTEGER, INTENT(IN) :: LoopSideNum
  INTEGER, INTENT(IN) :: BranchNum
  INTEGER, INTENT(IN) :: CompNum

   ! FUNCTION PARAMETER DEFINITIONS:
   ! na

   ! INTERFACE BLOCK SPECIFICATIONS
   ! na

   ! DERIVED TYPE DEFINITIONS
   ! na

   ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PumpCtr

  ! Assume it isn't found
  GetLoopSidePumpIndex = 0

  ! If there aren't any pumps on this loop side then just exit
  IF (.NOT. ALLOCATED(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps) ) THEN
    RETURN
  END IF

  ! We can also make use of the TypeOfs to exit early
  IF (.NOT. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType == GenEquipTypes_Pump) &
    RETURN

  ! Loop across all the loops on this loop/loopside, and check the branch/comp location
  DO PumpCtr = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalPumps
    IF (      (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps(PumpCtr)%BranchNum == BranchNum) &
        .AND. (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps(PumpCtr)%CompNum == CompNum) ) THEN
      GetLoopSidePumpIndex = PumpCtr
      RETURN
    END IF
  END DO

 RETURN

END FUNCTION GetLoopSidePumpIndex

SUBROUTINE ShowBranchesOnLoop(LoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine will display (with continue error messages) the branch/component
          ! structure of the given loop.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowContinueError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: LoopNum   ! Loop number of loop

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=10) :: DemandSupply
    INTEGER :: LSN  ! Loopside counter
    INTEGER :: BrN  ! Branch counter
    INTEGER :: CpN  ! Component (on branch) counter

    DO LSN=DemandSide,SupplySide
      IF (LSN == DemandSide) THEN
        DemandSupply='Demand'
      ELSEIF (LSN == SupplySide) THEN
        DemandSupply='Supply'
      ELSE
        DemandSupply='Unknown'
      ENDIF
      CALL ShowContinueError(trim(DemandSupply)//' Branches:')
      DO BrN = 1, PlantLoop(LoopNum)%LoopSide(LSN)%TotalBranches
        CALL ShowContinueError('  '//trim(PlantLoop(LoopNum)%LoopSide(LSN)%Branch(BrN)%Name))
        CALL ShowContinueError('    Components on Branch:')
        DO CpN=1,PlantLoop(LoopNum)%LoopSide(LSN)%Branch(BrN)%TotalComponents
          CALL ShowContinueError('      '//trim(PlantLoop(LoopNum)%LoopSide(LSN)%Branch(BrN)%Comp(CpN)%TypeOf)//':'//  &
             trim(PlantLoop(LoopNum)%LoopSide(LSN)%Branch(BrN)%Comp(CpN)%Name))
        ENDDO
      ENDDO
    ENDDO

  RETURN

END SUBROUTINE ShowBranchesOnLoop

!     NOTICE
!
!     Copyright © 1996-2011 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory.  All rights reserved.
!
!     Portions of the EnergyPlus software package have been developed and copyrighted
!     by other individuals, companies and institutions.  These portions have been
!     incorporated into the EnergyPlus software package under license.   For a complete
!     list of contributors, see "Notice" located in EnergyPlus.f90.

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


END MODULE DataPlant

