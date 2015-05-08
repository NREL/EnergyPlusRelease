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
USE DataGlobals,  ONLY: MaxNameLength,DoingSizing,outputfiledebug
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
INTEGER, PARAMETER :: EMSOpSchemeType                    = 12         ! Scheme Type for EMS based operation user Define scheme
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

! Parameters for loop flow request priority,
!     used in logic to deal with Node%MassFlowRequest for determining overall loop flow rate
INTEGER, PARAMETER :: LoopFlowStatus_Unknown              = 21  ! component's status is not yet set
INTEGER, PARAMETER :: LoopFlowStatus_NeedyAndTurnsLoopOn  = 22  ! component is a "winner" for loop flow requests
                                                                ! active valve inside component that modulates flow
                                                                !  gets the loop going under most conditions
INTEGER, PARAMETER :: LoopFlowStatus_NeedyIfLoopOn        = 23  !  component is a "winner" for loop flow requests
                                                                ! but doesn't normally get the loop going to start with
                                                                !  once loop is going, may increase needs, non-zero minimums
INTEGER, PARAMETER :: LoopFlowStatus_TakesWhatGets        = 24  ! component is a "loser" for loop flow requests,
                                                                ! but if the loop is on it
                                                                ! it does make flow requests (for s/m resolution)

!Parameters for component character wrt how load gets met (or not)
!  used in %HowLoadServed to facilitate load dispatch logic
INTEGER, PARAMETER :: HowMet_Unknown                      = 50  ! not yet set
INTEGER, PARAMETER :: HowMet_NoneDemand                   = 51  ! does not meet a load, demand component
INTEGER, PARAMETER :: HowMet_PassiveCap                   = 52  ! Passive machine, does what conditions allow but
INTEGER, PARAMETER :: HowMet_ByNominalCap                 = 53  ! MaxLoad, MinLoad, OptLoad should work
INTEGER, PARAMETER :: HowMet_ByNominalCapLowOutLimit      = 54  ! MaxLoad, MinLoad, OptLoad but with low limit temp on outlet
INTEGER, PARAMETER :: HowMet_ByNominalCapHiOutLimit       = 55  ! MaxLoad, MinLoad, OptLoad but with high limit temp on outlet
INTEGER, PARAMETER :: HowMet_ByNominalCapFreeCoolCntrl    = 56  ! HowMet_ByNominalCap with free cool shutdown
INTEGER, PARAMETER :: HowMet_ByNominalCapLowOutLimitFreeCoolCntrl = 57 ! HowMet_ByNominalCapLowOutLimit with free cool shutdown

INTEGER, PARAMETER :: FreeCoolControlMode_WetBulb = 1 ! HeatExchanger:Hydronic model control type mode, outdoor wetbulb sensor
INTEGER, PARAMETER :: FreeCoolControlMode_DryBulb = 2 ! HeatExchanger:Hydronic model control type mode, outdoor drybulb sensor
INTEGER, PARAMETER :: FreeCoolControlMode_Loop    = 3 ! HeatExchanger:Hydronic model control type mode, loop setpoint sensor

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
INTEGER, PARAMETER :: NumSimPlantEquipTypes=89
CHARACTER(len=*), PARAMETER, DIMENSION(NumSimPlantEquipTypes) :: SimPlantEquipTypes=  &
      (/'BOILER:HOTWATER                                         ', & !01
        'BOILER:STEAM                                            ', & !02
        'CHILLER:ABSORPTION                                      ', & !03
        'CHILLER:ABSORPTION:INDIRECT                             ', & !04
        'CHILLER:COMBUSTIONTURBINE                               ', & !05
        'CHILLER:CONSTANTCOP                                     ', & !06
        'CHILLERHEATER:ABSORPTION:DIRECTFIRED                    ', & !07
        'CHILLER:ELECTRIC                                        ', & !08
        'CHILLER:ELECTRIC:EIR                                    ', & !09
        'CHILLER:ELECTRIC:REFORMULATEDEIR                        ', & !10
        'CHILLER:ENGINEDRIVEN                                    ', & !11
        'COOLINGTOWER:SINGLESPEED                                ', & !12
        'COOLINGTOWER:TWOSPEED                                   ', & !13
        'COOLINGTOWER:VARIABLESPEED                              ', & !14
        'GENERATOR:FUELCELL:EXHAUSTGASTOWATERHEATEXCHANGER       ', & !15
        'WATERHEATER:HEATPUMP                                    ', & !16
        'HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING               ', & !17
        'HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING               ', & !18
        'HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING       ', & !19
        'HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING       ', & !20
        'PIPE:ADIABATIC                                          ', & !21
        'PIPE:ADIABATIC:STEAM                                    ', & !22
        'PIPE:OUTDOOR                                            ', & !23
        'PIPE:INDOOR                                             ', & !24
        'PIPE:UNDERGROUND                                        ', & !25
        'DISTRICTCOOLING                                         ', & !26
        'DISTRICTHEATING                                         ', & !27
        'THERMALSTORAGE:ICE:DETAILED                             ', & !28
        'THERMALSTORAGE:ICE:SIMPLE                               ', & !29
        'TEMPERINGVALVE                                          ', & !30
        'WATERHEATER:MIXED                                       ', & !31
        'WATERHEATER:STRATIFIED                                  ', & !32
        'PUMP:VARIABLESPEED                                      ', & !33
        'PUMP:CONSTANTSPEED                                      ', & !34
        'PUMP:VARIABLESPEED:CONDENSATE                           ', & !35
        'HEADEREDPUMPS:VARIABLESPEED                             ', & !36
        'HEADEREDPUMPS:CONSTANTSPEED                             ', & !37
        'WATERUSE:CONNECTIONS                                    ', & !38 ! demand side component
        'COIL:COOLING:WATER                                      ', & !39 ! demand side component
        'COIL:COOLING:WATER:DETAILEDGEOMETRY                     ', & !40 ! demand side component
        'COIL:HEATING:WATER                                      ', & !41 ! demand side component
        'COIL:HEATING:STEAM                                      ', & !42 ! demand side component
        'SOLARCOLLECTOR:FLATPLATE:WATER                          ', & !43 ! demand side component
        'LOADPROFILE:PLANT                                       ', & !44 ! demand side component'
        'GROUNDHEATEXCHANGER:VERTICAL                            ', & !45
        'GROUNDHEATEXCHANGER:SURFACE                             ', & !46
        'GROUNDHEATEXCHANGER:POND                                ', & !47
        'GENERATOR:MICROTURBINE                                  ', & !48
        'GENERATOR:INTERNALCOMBUSTIONENGINE                      ', & !49
        'GENERATOR:COMBUSTIONTURBINE                             ', & !50
        'GENERATOR:MICROCHP                                      ', & !51
        'GENERATOR:FUELCELL:STACKCOOLER                          ', & !52
        'FLUIDCOOLER:SINGLESPEED                                 ', & !53
        'FLUIDCOOLER:TWOSPEED                                    ', & !54
        'EVAPORATIVEFLUIDCOOLER:SINGLESPEED                      ', & !55
        'EVAPORATIVEFLUIDCOOLER:TWOSPEED                         ', & !56
        'THERMALSTORAGE:CHILLEDWATER:MIXED                       ', & !57
        'THERMALSTORAGE:CHILLEDWATER:STRATIFIED                  ', & !58
        'SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL            ', & !59
        'ZONEHVAC:BASEBOARD:CONVECTIVE:WATER                     ', & !60
        'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM              ', & !61
        'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER              ', & !62
        'ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW             ', & !63
        'ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW             ', & !64
        'AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:COOLEDBEAM        ', & !65
        'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT             ', & !66
        'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT             ', & !67
        'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION     ', & !68
        'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION     ', & !69
        'REFRIGERATION:CONDENSER:WATERCOOLED                     ', & !70
        'REFRIGERATION:COMPRESSORRACK                            ', & !71
        'AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED         ', & !72
        'CHILLERHEATER:ABSORPTION:DOUBLEEFFECT                   ', & !73
        'PIPINGSYSTEM:UNDERGROUND:PIPECIRCUIT                    ', & !74
        'SOLARCOLLECTOR:INTEGRALCOLLECTORSTORAGE                 ', & !75
        'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', & !76
        'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', & !77
        'PLANTCOMPONENT:USERDEFINED                              ', & !78
        'COIL:USERDEFINED                                        ', & !79
        'ZONEHVAC:FORCEDAIR:USERDEFINED                          ', & !80
        'AIRTERMINAL:SINGLEDUCT:USERDEFINED                      ', & !81
        'AIRCONDITIONER:VARIABLEREFRIGERANTFLOW                  ', & !82
        'GROUNDHEATEXCHANGER:HORIZONTALTRENCH                    ', & !83
        'HEATEXCHANGER:FLUIDTOFLUID                              ', & !84
        'PLANTCOMPONENT:TEMPERATURESOURCE                        ', & !85
        'CENTRALHEATPUMPSYSTEM                                   ', & !86
        'AIRLOOPHVAC:UNITARYSYSTEM                               ', & !87
        'COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE              ', & !88
        'COOLINGTOWER:VARIABLESPEED:MERKEL                       '/)  !89


CHARACTER(len=*), PARAMETER, DIMENSION(NumSimPlantEquipTypes) :: ccSimPlantEquipTypes=  &
      (/'Boiler:HotWater                                         ', & !01
        'Boiler:Steam                                            ', & !02
        'Chiller:Absorption                                      ', & !03
        'Chiller:Absorption:Indirect                             ', & !04
        'Chiller:CombustionTurbine                               ', & !05
        'Chiller:ConstantCOP                                     ', & !06
        'ChillerHeater:Absorption:DirectFired                    ', & !07
        'Chiller:Electric                                        ', & !08
        'Chiller:Electric:EIR                                    ', & !09
        'Chiller:Electric:ReformulatedEIR                        ', & !10
        'Chiller:EngineDriven                                    ', & !11
        'CoolingTower:SingleSpeed                                ', & !12
        'CoolingTower:TwoSpeed                                   ', & !13
        'CoolingTower:VariableSpeed                              ', & !14
        'Generator:Fuelcell:ExhaustGastoWaterHeatExchanger       ', & !15
        'WaterHeater:Heatpump                                    ', & !16
        'Heatpump:WatertoWater:Equationfit:Cooling               ', & !17
        'Heatpump:WatertoWater:Equationfit:Heating               ', & !18
        'Heatpump:WatertoWater:ParameterEstimation:Cooling       ', & !19
        'Heatpump:WatertoWater:ParameterEstimation:Heating       ', & !20
        'Pipe:Adiabatic                                          ', & !21
        'Pipe:Adiabatic:Steam                                    ', & !22
        'Pipe:Outdoor                                            ', & !23
        'Pipe:Indoor                                             ', & !24
        'Pipe:Underground                                        ', & !25
        'DistrictCooling                                         ', & !26
        'DistrictHeating                                         ', & !27
        'ThermalStorage:Ice:Detailed                             ', & !28
        'ThermalStorage:Ice:Simple                               ', & !29
        'TemperingValve                                          ', & !30
        'WaterHeater:Mixed                                       ', & !31
        'WaterHeater:Stratified                                  ', & !32
        'Pump:VariableSpeed                                      ', & !33
        'Pump:ConstantSpeed                                      ', & !34
        'Pump:VariableSpeed:Condensate                           ', & !35
        'HeaderedPumps:VariableSpeed                             ', & !36
        'HeaderedPumps:ConstantSpeed                             ', & !37
        'WaterUse:Connections                                    ', & !38 ! demand side component
        'Coil:Cooling:Water                                      ', & !39 Demand Side Component
        'Coil:Cooling:Water:DetailedGeometry                     ', & !40 Demand Side Component
        'Coil:Heating:Water                                      ', & !41 Demand Side Component
        'Coil:Heating:Steam                                      ', & !42 Demand Side Component
        'Solarcollector:Flatplate:Water                          ', & !43 Demand Side Component
        'LoadProfile:Plant                                       ', & !44 Demand Side Component'
        'GroundHeatExchanger:Vertical                            ', & !45
        'GroundHeatExchanger:Surface                             ', & !46
        'GroundHeatExchanger:Pond                                ', & !47
        'Generator:Microturbine                                  ', & !48
        'Generator:InternalCombustionEngine                      ', & !49
        'Generator:CombustionTurbine                             ', & !50
        'Generator:Microchp                                      ', & !51
        'Generator:Fuelcell:StackCooler                          ', & !52
        'FluidCooler:SingleSpeed                                 ', & !53
        'FluidCooler:TwoSpeed                                    ', & !54
        'EvaporativeFluidCooler:SingleSpeed                      ', & !55
        'EvaporativeFluidCooler:TwoSpeed                         ', & !56
        'ThermalStorage:ChilledWater:Mixed                       ', & !57
        'ThermalStorage:ChilledWater:Stratified                  ', & !58
        'SolarCollector:FlatPlate:PhotovoltaicThermal            ', & !59
        'ZoneHVAC:Baseboard:Convective:Water                     ', & !60
        'ZoneHVAC:Baseboard:RadiantConvective:Steam              ', & !61
        'ZoneHVAC:Baseboard:RadiantConvective:Water              ', & !62
        'ZoneHVAC:LowTemperatureRadiant:VariableFlow             ', & !63
        'ZoneHVAC:LowTemperatureRadiant:ConstantFlow             ', & !64
        'AirTerminal:SingleDuct:ConstantVolume:CooledBeam        ', & !65
        'Coil:Heating:WaterToAirHeatPump:EquationFit             ', & !66
        'Coil:Cooling:WaterToAirHeatPump:EquationFit             ', & !67
        'Coil:Heating:WaterToAirHeatPump:ParameterEstimation     ', & !68
        'Coil:Cooling:WaterToAirHeatPump:ParameterEstimation     ', & !69
        'Refrigeration:Condenser:WaterCooled                     ', & !70
        'Refrigeration:CompressorRack                            ', & !71
        'AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed         ', & !72
        'ChillerHeater:Absorption:DoubleEffect                   ', & !73
        'PipingSystem:Underground:PipeCircuit                    ', & !74
        'SolarCollector:IntegralCollectorStorage                 ', & !75
        'Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit', & !76
        'Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit', & !77
        'PlantComponent:UserDefined                              ', & !78
        'Coil:UserDefined                                        ', & !79
        'ZoneHVAC:ForcedAir:UserDefined                          ', & !80
        'AirTerminal:SingleDuct:UserDefined                      ', & !81
        'AirConditioner:VariableRefrigerantFlow                  ', & !82
        'GroundHeatExchanger:HorizontalTrench                    ', & !83
        'HeatExchanger:FluidToFluid                              ', & !84
        'PlantComponent:TemperatureSource                        ', & !85
        'CentralHeatPumpSystem                                   ', & !86
        'AirloopHVAC:UnitarySystem                               ', & !87
        'Coil:Cooling:DX:SingleSpeed:ThermalStorage              ', & !88
        'CoolingTower:VariableSpeed:Merkel                       '/)  !89

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

   LoopType_Plant,     &     ! 16  WATERHEATER:HEATPUMP
   LoopType_Plant,     &     ! 17  HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING
   LoopType_Plant,     &     ! 18  HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING
   LoopType_Plant,     &     ! 19  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING
   LoopType_Plant,     &     ! 20  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING
   LoopType_Both,      &     ! 21  PIPE:ADIABATIC
   LoopType_Both,      &     ! 22  PIPE:ADIABATIC:STEAM
   LoopType_Both,      &     ! 23  PIPE:OUTDOOR
   LoopType_Both,      &     ! 24  PIPE:INDOOR
   LoopType_Both,      &     ! 25  PIPE:UNDERGROUND
   LoopType_Both,      &     ! 26  DISTRICTCOOLING
   LoopType_Both,      &     ! 27  DISTRICTHEATING
   LoopType_Plant,     &     ! 28  THERMALSTORAGE:ICE:DETAILED
   LoopType_Plant,     &     ! 29  THERMALSTORAGE:ICE:SIMPLE
   LoopType_Both,      &     ! 30  TEMPERINGVALVE
   LoopType_Both,      &     ! 31  WATERHEATER:MIXED
   LoopType_Both,      &     ! 32  WATERHEATER:STRATIFIED
   LoopType_Both,      &     ! 33  PUMP:VARIABLESPEED
   LoopType_Both,      &     ! 34  PUMP:CONSTANTSPEED
   LoopType_Both,      &     ! 35  PUMP:VARIABLESPEED:CONDENSATE
   LoopType_Both,      &     ! 36  HEADEREDPUMPS:VARIABLESPEED
   LoopType_Both,      &     ! 37  HEADEREDPUMPS:CONSTANTSPEED
   LoopType_Plant,     &     ! 38  WATERUSE:CONNECTIONS
   LoopType_Plant,     &     ! 39  COIL:COOLING:WATER
   LoopType_Plant,     &     ! 40  COIL:COOLING:WATER:DETAILEDGEOMETRY
   LoopType_Plant,     &     ! 41  COIL:HEATING:WATER
   LoopType_Plant,     &     ! 42  COIL:HEATING:STEAM
   LoopType_Plant,     &     ! 43  SOLARCOLLECTOR:FLATPLATE:WATER
   LoopType_Both,      &     ! 44  LOADPROFILE:PLANT
   LoopType_Both,      &     ! 45  GROUNDHEATEXCHANGER:VERTICAL
   LoopType_Both,      &     ! 46  GROUNDHEATEXCHANGER:SURFACE
   LoopType_Both,      &     ! 47  GROUNDHEATEXCHANGER:POND
   LoopType_Plant,     &     ! 48  GENERATOR:MICROTURBINE
   LoopType_Plant,     &     ! 49  GENERATOR:INTERNALCOMBUSTIONENGINE
   LoopType_Plant,     &     ! 50  GENERATOR:COMBUSTIONTURBINE
   LoopType_Plant,     &     ! 51  GENERATOR:MICROCHP
   LoopType_Plant,     &     ! 52  GENERATOR:FUELCELL:STACKCOOLER
   LoopType_Both,      &     ! 53  FLUIDCOOLER:SINGLESPEED
   LoopType_Both,      &     ! 54  FLUIDCOOLER:TWOSPEED
   LoopType_Both,      &     ! 55  EVAPORATIVEFLUIDCOOLER:SINGLESPEED
   LoopType_Both,      &     ! 56  EVAPORATIVEFLUIDCOOLER:TWOSPEED
   LoopType_Both,      &     ! 57  THERMALSTORAGE:CHILLEDWATER:MIXED
   LoopType_Both,      &     ! 58  THERMALSTORAGE:CHILLEDWATER:STRATIFIED
   LoopType_Both,      &     ! 59  SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL
   LoopType_Plant,     &     ! 60  ZONEHVAC:BASEBOARD:CONVECTIVE:WATER
   LoopType_Plant,     &     ! 61  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM
   LoopType_Plant,     &     ! 62  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER
   LoopType_Plant,     &     ! 63  ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW
   LoopType_Plant,     &     ! 64  ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW
   LoopType_Both,      &     ! 65 AirTerminal:SingleDuct:ConstantVolume:CooledBeam
   LoopType_Both,      &     ! 66  Coil:Heating:WaterToAirHeatPump:EquationFit
   LoopType_Both,      &     ! 67  Coil:Cooling:WaterTOAIRHeatPump:EquationFit
   LoopType_Both,      &     ! 68  Coil:Heating:WaterTOAIRHeatPump:ParameterEstimation
   LoopType_Both,      &     ! 69  Coil:Cooling:WaterTOAIRHeatPump:ParameterEstimation
   LoopType_Both,      &     ! 70  Refrigeration:Condenser:WaterCooled
   LoopType_Both,      &     ! 71  Refrigeration:CompressorRack
   LoopType_Plant,     &     ! 72  AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed
   LoopType_Plant,     &     ! 73  CHILLERHEATER:ABSORPTION:DOUBLEEFFECT
   LoopType_Both,      &     ! 74  PipingSystem:Underground:PipeCircuit
   LoopType_Both,      &     ! 75  SolarCollector:IntegralCollectorStorage
   LoopType_Both,      &     ! 76  Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit
   LoopType_Both,      &     ! 77  Coil:Cooling:WaterTOAIRHeatPump:VariableSpeedEquationFit
   LoopType_Both,      &     ! 78  PlantComponent:UserDefined
   LoopType_Both,      &     ! 79  Coil:UserDefined
   LoopType_Both,      &     ! 80  ZoneHVAC:ForcedAir:UserDefined
   LoopType_Both,      &     ! 81  AirTerminal:SingleDuct:UserDefined
   LoopType_Both,      &     ! 82  AirConditioner:VariableRefrigerantFlow
   LoopType_Both,      &     ! 83  GroundHeatExchanger:HorizontalTrench
   LoopType_Both,      &     ! 84  HeatExchanger:FluidToFluid
   LoopType_Both,      &     ! 85  PlantComponent:TemperatureSource
   LoopType_Plant,     &     ! 86  PlantCentralGroundSourceWrapper
   LoopType_Plant,     &     ! 87  AirloopHVAC:UnitarySystem
   LoopType_Both,      &     ! 88  Coil:Cooling:DX:SingleSpeed:ThermalStorage
   LoopType_Both/)           ! 89  CoolingTower:VariableSpeed:Merkel 

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
INTEGER, PARAMETER :: TypeOf_HeatPumpWtrHeater           = 16
INTEGER, PARAMETER :: TypeOf_HPWaterEFCooling            = 17
INTEGER, PARAMETER :: TypeOf_HPWaterEFHeating            = 18
INTEGER, PARAMETER :: TypeOf_HPWaterPECooling            = 19
INTEGER, PARAMETER :: TypeOf_HPWaterPEHeating            = 20
INTEGER, PARAMETER :: TypeOf_Pipe                        = 21
INTEGER, PARAMETER :: TypeOf_PipeSteam                   = 22
INTEGER, PARAMETER :: TypeOf_PipeExterior                = 23
INTEGER, PARAMETER :: TypeOf_PipeInterior                = 24
INTEGER, PARAMETER :: TypeOf_PipeUnderground             = 25
INTEGER, PARAMETER :: TypeOf_PurchChilledWater           = 26
INTEGER, PARAMETER :: TypeOf_PurchHotWater               = 27
INTEGER, PARAMETER :: TypeOf_TS_IceDetailed              = 28
INTEGER, PARAMETER :: TypeOf_TS_IceSimple                = 29
INTEGER, PARAMETER :: TypeOf_ValveTempering              = 30
INTEGER, PARAMETER :: TypeOf_WtrHeaterMixed              = 31
INTEGER, PARAMETER :: TypeOf_WtrHeaterStratified         = 32
INTEGER, PARAMETER :: TypeOf_PumpVariableSpeed           = 33
INTEGER, PARAMETER :: TypeOf_PumpConstantSpeed           = 34
INTEGER, PARAMETER :: TypeOf_PumpCondensate              = 35
INTEGER, PARAMETER :: TypeOf_PumpBankVariableSpeed       = 36
INTEGER, PARAMETER :: TypeOf_PumpBankConstantSpeed       = 37
INTEGER, PARAMETER :: TypeOf_WaterUseConnection          = 38
INTEGER, PARAMETER :: TypeOf_CoilWaterCooling            = 39  ! demand side component
INTEGER, PARAMETER :: TypeOf_CoilWaterDetailedFlatCooling= 40  ! demand side component
INTEGER, PARAMETER :: TypeOf_CoilWaterSimpleHeating      = 41  ! demand side component
INTEGER, PARAMETER :: TypeOf_CoilSteamAirHeating         = 42  ! demand side component
INTEGER, PARAMETER :: TypeOf_SolarCollectorFlatPlate     = 43  ! demand side component
INTEGER, PARAMETER :: TypeOf_PlantLoadProfile            = 44  ! demand side component
INTEGER, PARAMETER :: TypeOf_GrndHtExchgVertical         = 45
INTEGER, PARAMETER :: TypeOf_GrndHtExchgSurface          = 46
INTEGER, PARAMETER :: TypeOf_GrndHtExchgPond             = 47
INTEGER, PARAMETER :: TypeOf_Generator_MicroTurbine      = 48  !newer FSEC turbine
INTEGER, PARAMETER :: TypeOf_Generator_ICEngine          = 49
INTEGER, PARAMETER :: TypeOf_Generator_CTurbine          = 50  !older BLAST turbine
INTEGER, PARAMETER :: TypeOf_Generator_MicroCHP          = 51
INTEGER, PARAMETER :: TypeOf_Generator_FCStackCooler     = 52
INTEGER, PARAMETER :: TypeOf_FluidCooler_SingleSpd       = 53
INTEGER, PARAMETER :: TypeOf_FluidCooler_TwoSpd          = 54
INTEGER, PARAMETER :: TypeOf_EvapFluidCooler_SingleSpd   = 55
INTEGER, PARAMETER :: TypeOf_EvapFluidCooler_TwoSpd      = 56
INTEGER, PARAMETER :: TypeOf_ChilledWaterTankMixed       = 57
INTEGER, PARAMETER :: TypeOf_ChilledWaterTankStratified  = 58
INTEGER, PARAMETER :: TypeOf_PVTSolarCollectorFlatPlate  = 59
INTEGER, PARAMETER :: TypeOf_Baseboard_Conv_Water        = 60
INTEGER, PARAMETER :: TypeOf_Baseboard_Rad_Conv_Steam    = 61
INTEGER, PARAMETER :: TypeOf_Baseboard_Rad_Conv_Water    = 62
INTEGER, PARAMETER :: TypeOf_LowTempRadiant_VarFlow      = 63
INTEGER, PARAMETER :: TypeOf_LowTempRadiant_ConstFlow    = 64
INTEGER, PARAMETER :: TypeOf_CooledBeamAirTerminal       = 65
INTEGER, PARAMETER :: TypeOf_CoilWAHPHeatingEquationFit  = 66
INTEGER, PARAMETER :: TypeOf_CoilWAHPCoolingEquationFit  = 67
INTEGER, PARAMETER :: TypeOf_CoilWAHPHeatingParamEst     = 68
INTEGER, PARAMETER :: TypeOf_CoilWAHPCoolingParamEst     = 69
INTEGER, PARAMETER :: TypeOf_RefrigSystemWaterCondenser  = 70
INTEGER, PARAMETER :: TypeOf_RefrigerationWaterCoolRack  = 71
INTEGER, PARAMETER :: TypeOf_MultiSpeedHeatPumpRecovery  = 72
INTEGER, PARAMETER :: TypeOf_Chiller_ExhFiredAbsorption  = 73
INTEGER, PARAMETER :: TypeOf_PipingSystemPipeCircuit     = 74
INTEGER, PARAMETER :: TypeOf_SolarCollectorICS           = 75
INTEGER, PARAMETER :: TypeOf_CoilVSWAHPHeatingEquationFit= 76
INTEGER, PARAMETER :: TypeOf_CoilVSWAHPCoolingEquationFit= 77
INTEGER, PARAMETER :: TypeOf_PlantComponentUserDefined   = 78
INTEGER, PARAMETER :: TypeOf_CoilUserDefined             = 79
INTEGER, PARAMETER :: TypeOf_ZoneHVACAirUserDefined      = 80
INTEGER, PARAMETER :: TypeOf_AirTerminalUserDefined      = 81
INTEGER, PARAMETER :: TypeOf_HeatPumpVRF                 = 82
INTEGER, PARAMETER :: TypeOf_GrndHtExchgHorizTrench      = 83
INTEGER, PARAMETER :: TypeOf_FluidToFluidPlantHtExchg    = 84
INTEGER, PARAMETER :: TypeOf_WaterSource                 = 85
INTEGER, PARAMETER :: TypeOf_CentralGroundSourceHeatPump = 86
INTEGER, PARAMETER :: TypeOf_UnitarySystemRecovery       = 87
INTEGER, PARAMETER :: TypeOf_PackagedTESCoolingCoil      = 88
INTEGER, PARAMETER :: TypeOf_CoolingTower_VarSpdMerkel   = 89

! Parameters for General Equipment Types
INTEGER, PARAMETER :: NumGeneralEquipTypes               = 23
CHARACTER(len=*), PARAMETER, &
     DIMENSION(NumGeneralEquipTypes) :: GeneralEquipTypes   =   &
                   (/'BOILER                ',  &
                     'CHILLER               ',  &
                     'COOLINGTOWER          ',  &
                     'GENERATOR             ',  &
                     'HEATEXCHANGER         ',  &
                     'HEATPUMP              ',  &
                     'PIPE                  ',  &
                     'PUMP                  ',  &
                     'DISTRICT              ',  &
                     'THERMALSTORAGE        ',  &
                     'TEMPERINGVALVE        ',  &
                     'WATERHEATER           ',  &
                     'WATERUSE              ',  &
                     'DEMANDCOIL            ',  &
                     'SOLARCOLLECTOR        ',  &
                     'LOADPROFILE           ',  &
                     'FLUIDCOOLER           ',  &
                     'EVAPORATIVEFLUIDCOOLER', &
                     'GROUNDHEATEXCHANGER   ', &
                     'ZONEHVACDEMAND        ', &
                     'REFRIGERATION         ', &
                     'PLANTCOMPONENT        ', &
                     'CENTRALHEATPUMPSYSTEM '/)

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
INTEGER, PARAMETER :: GenEquipTypes_PlantComponent       = 22
INTEGER, PARAMETER :: GenEquipTypes_CentralHeatPumpSystem= 23

CHARACTER(len=*), PARAMETER, DIMENSION (0:12) :: OpSchemeTypes = &
                   (/'Load Range Based Operation                       ',   &  ! long since Deprecated, remove?
                     'PLANTEQUIPMENTOPERATION:HEATINGLOAD              ',   &
                     'PLANTEQUIPMENTOPERATION:COOLINGLOAD              ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORWETBULB           ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORDRYBULB           ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINT          ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORRELATIVEHUMIDITY  ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORDRYBULBDIFFERENCE ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORWETBULBDIFFERENCE ',   &
                     'PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINTDIFFERENCE',   &
                     'PLANTEQUIPMENTOPERATION:COMPONENTSETPOINT        ',   &
                     'PLANTEQUIPMENTOPERATION:USERDEFINED              ',   &
                     'PLANTEQUIPMENTOPERATION:UNCONTROLLED             '/)

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
  LOGICAL                          :: EMSLoadOverrideOn     = .FALSE. ! EMS is calling to override load dispatched to component
  REAL(r64)                        :: EMSLoadOverrideValue  = 0.0d0   ! EMS value to use for load when overridden [W] always positive.
  INTEGER                          :: HowLoadServed         = HowMet_Unknown ! nature of component in terms of how it can meet load
  REAL(r64)                        :: MinOutletTemp         = 0.0d0     ! Component exit lower limit temperature
  REAL(r64)                        :: MaxOutletTemp         = 0.0d0     ! Component exit upper limit temperature
  LOGICAL                          :: FreeCoolCntrlShutDown = .FALSE. ! true if component was shut down because of free cooling
  REAL(r64)                        :: FreeCoolCntrlMinCntrlTemp   = 0.D0  ! current control temp value for free cooling controls
  INTEGER                          :: FreeCoolCntrlMode     = 0 ! type of sensor used for free cooling controls
  INTEGER                          :: FreeCoolCntrlNodeNum  = 0 ! chiller condenser inlet node number for free cooling controls
  INTEGER                          :: IndexInLoopSidePumps  = 0       ! If I'm a pump, this tells my index in PL(:)%LS(:)%Pumps
  REAL(r64)                        :: TempDesCondIn         = 0.0d0
  REAL(r64)                        :: TempDesEvapOut        = 0.0d0
END TYPE CompData

TYPE BranchData
  CHARACTER(len=MaxNameLength)     :: Name                  = ' '     ! Name of the branch
  INTEGER                          :: ControlType           = 0
  REAL(r64)                        :: MinVolFlowRate        = 0.0d0
  REAL(r64)                        :: MaxVolFlowRate        = 0.0d0
  REAL(r64)                        :: RequestedMassFlow     = 0.0d0
  LOGICAL                          :: HasConstantSpeedBranchPump = .FALSE. ! true if branch has a constant speed branch pump
  REAL(r64)                        :: ConstantSpeedBranchMassFlow = 0.d0 ! nominal flow rate if constant speed branch pump on
  INTEGER                          :: BranchLevel           = 0
  INTEGER                          :: FlowErrCount          = 0       ! For recurring error counting
  INTEGER                          :: FlowErrIndex          = 0       ! For recurring error index
  INTEGER                          :: TotalComponents       = 0       ! Total number of components on the branch
  INTEGER                          :: NodeNumIn             = 0       ! Component inlet node number
  INTEGER                          :: NodeNumOut            = 0       ! Component outlet node number
  LOGICAL                          :: IsByPass              = .FALSE.
  INTEGER                          :: PumpIndex             = 0
  real(r64)                        :: PumpSizFac            = 1.0d0
  LOGICAL                          :: EMSCtrlOverrideOn     = .FALSE. ! if true, EMS is calling to override branch operation avail
  REAL(r64)                        :: EMSCtrlOverrideValue  = 0.0d0   ! value set by EMS system for branch override controls
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
  REAL(r64)                        :: EMSIntVarRemainingLoadValue = 0.d0 ! EMS internal variable remaining load, neg cooling [W]
  REAL(r64)                        :: EMSActuatorDispatchedLoadValue = 0.d0 ! EMS actuator for dispatched load, neg= cooling [W]
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
  INTEGER                          :: EquipListNumForLastStage = 0    ! points to the equipment list with the highest upper limit
  CHARACTER(len=MaxNameLength)     :: ReferenceNodeName     = ' '     ! DELTA CTRL ONLY--for calculation of delta Temp
  INTEGER                          :: ReferenceNodeNumber              ! DELTA CTRL ONLY--for calculation of delta Temp
  INTEGER    :: ErlSimProgramMngr = 0   ! EMS:ProgramManager to always run when this model is called
  INTEGER    :: ErlInitProgramMngr= 0   ! EMS:ProgramManager to run when this model is initialized and setup
  REAL(r64)  :: EMSIntVarLoopDemandRate = 0.d0 ! EMS internal variable for loop-level demand rate, neg cooling [W]
  LOGICAL    :: MyEnvrnFlag = .TRUE.
END TYPE OperationData                        ! DSU

TYPE ConnectedLoopData                        ! DSU
  INTEGER                          :: LoopNum               = 0  ! plant loop index pointer, for the other loop
  INTEGER                          :: LoopSideNum           = 0  ! plant loop side number, for the other loop
  INTEGER                          :: ConnectorTypeOf_Num   = 0  ! plant equipment type doing the connecting
  LOGICAL                          :: LoopDemandsOnRemote    = .FALSE. ! true if this loop puts demand on connected loop
END TYPE ConnectedLoopData                      ! DSU

TYPE PlantLocation
  INTEGER                          :: LoopNum
  INTEGER                          :: LoopSideNum
  INTEGER                          :: BranchNum
  INTEGER                          :: CompNum
END TYPE

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
REAL(r64), PARAMETER, DIMENSION(NumConvergenceHistoryTerms) :: ConvergenceHistoryARR = &
               (/0.d0, -1.d0, -2.d0, -3.d0, - 4.d0 /)
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
  INTEGER                          :: MFErrIndex1           = 0       ! for recurring mass flow errors
  INTEGER                          :: MFErrIndex2           = 0       ! for recurring mass flow errors
                                                                      ! (see CheckPlantMixerSplitterConsistency)
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
LOGICAL                            :: PlantSizesOkayToFinalize = .FALSE. ! true if plant sizing is finishing and can save results
LOGICAL                            :: AnyEMSPlantOpSchemesInModel = .FALSE.

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

! these variables are arrays, allocated for the number of those particular loopsides, containing data for the vent reports
! they are operated on like normal in almost all cases currently, except in the routine which actually mines data and sets them up
! in that routine in SystemReports.f90, a POINTER is used to iterate over the different array variables below
! this is why the TARGET attribute is applied to them here
! further info can be found in SystemReports
TYPE (ReportLoopData),    ALLOCATABLE, DIMENSION(:), TARGET :: VentRepPlantSupplySide
TYPE (ReportLoopData),    ALLOCATABLE, DIMENSION(:), TARGET :: VentRepPlantDemandSide
TYPE (ReportLoopData),    ALLOCATABLE, DIMENSION(:), TARGET :: VentRepCondSupplySide
TYPE (ReportLoopData),    ALLOCATABLE, DIMENSION(:), TARGET :: VentRepCondDemandSide
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
USE BranchInputManager, ONLY: AuditBranches

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
!  logical :: printsteps

  FoundCount = 0

  FoundComponent = .FALSE.
  FoundCompName  = .false.
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
        CALL AuditBranches(.true.,ccSimPlantEquipTypes(CompType),CompName)
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

SUBROUTINE ScanPlantLoopsForNodeNum(CallerName, NodeNum, LoopNum, LoopSideNum, BranchNum, CompNum)

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
  INTEGER, INTENT(OUT), OPTIONAL :: CompNum

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
  INTEGER     :: inFoundCount
  INTEGER     :: outFoundCount

  inFoundCount = 0
  outFoundCount = 0
  IF (PRESENT(CompNum)) THEN
    CompNum   = 0
  ENDIF
  FoundNode = .FALSE.

  PlantLoops: DO LoopCtr = 1, TotNumLoops
    DO LoopSideCtr = 1, 2
      DO BranchCtr = 1, PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%TotalBranches
        DO CompCtr = 1, PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%TotalComponents

         IF (NodeNum == PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%NodeNumIn) THEN
           FoundNode = .TRUE.
           inFoundCount  = inFoundCount + 1
           LoopNum     = LoopCtr
           LoopSideNum = LoopSideCtr
           BranchNum   = BranchCtr
           IF (PRESENT(CompNum)) THEN
             CompNum   = CompCtr
           ENDIF
         ENDIF

         IF (NodeNum == PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%NodeNumOut) THEN
           outFoundCount  = outFoundCount + 1
           LoopNum     = LoopCtr
           LoopSideNum = LoopSideCtr
           BranchNum   = BranchCtr
         ENDIF

        END DO
      END DO
    END DO
  END DO PlantLoops

  IF (.NOT. FoundNode) THEN
      CALL ShowSevereError('ScanPlantLoopsForNodeNum: Plant Node was not found as inlet node (for component) on any plant loops')
      Call ShowContinueError('Node Name="'//trim(NodeID(NodeNum))//'"' )
      IF (.not. DoingSizing) THEN
        Call ShowContinueError('called by '//Trim(CallerName))
      ELSE
        Call ShowContinueError('during sizing: called by '//Trim(CallerName))
      ENDIF
      IF (outFoundCount > 0)   &
        CALL ShowContinueError('Node was found as outlet node (for component) '//trim(RoundSigDigits(outFoundCount))//' time(s).')
      CALL ShowContinueError('Possible error in Branch inputs.  '//  &
         'For more information, look for other error messages related to this node name.')
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
   ! na

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
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType /=   &
       GenEquipTypes_Pump) &
    RETURN

  ! Loop across all the loops on this loop/loopside, and check the branch/comp location
  DO PumpCtr = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalPumps
    IF (      (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps(PumpCtr)%BranchNum == BranchNum) &
        .AND. (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps(PumpCtr)%CompNum == CompNum) ) THEN
      GetLoopSidePumpIndex = PumpCtr
      EXIT
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

FUNCTION MyPlantSizingIndex(CompType,CompName,NodeNumIn,NodeNumOut,ErrorsFound,SupressErrors) RESULT(MyPltSizNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Identify the correct Plant Sizing object for demand-side components such as heating and
          ! cooling coils.


          ! METHODOLOGY EMPLOYED:
          ! This function searches all plant loops for a component whose input and
          ! output nodes match the desired input & output nodes. This plant loop index is then used
          ! to search the Plant Sizing array for the matching Plant Sizing object.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList
  USE DataSizing, ONLY: NumPltSizInput, PlantSizData
  USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowContinueError
!  USE DataPlant, ONLY: PlantLoop, ScanPlantLoopsForNodeNum

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType      ! component description
  CHARACTER(len=*), INTENT(IN) :: CompName      ! user name of component
  INTEGER, INTENT(IN) :: NodeNumIn      ! component water inlet node
  INTEGER, INTENT(IN) :: NodeNumOut     ! component water outlet node
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if there's an error
  LOGICAL, OPTIONAL, INTENT(IN):: SupressErrors ! used for WSHP's where condenser loop may not be on a plant loop
  INTEGER                      :: MyPltSizNum  ! returned plant sizing index

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  INTEGER                      :: MyPltLoopNum
  INTEGER                      :: PlantLoopNum
  INTEGER                      :: DummyLoopSideNum
  INTEGER                      :: DummyBranchNum
  LOGICAL                      :: PrintErrorFlag

  MyPltLoopNum = 0
  MyPltSizNum = 0
  ErrorsFound = .false.
  IF(PRESENT(SupressErrors))THEN
    PrintErrorFlag = SupressErrors
  ELSE
    PrintErrorFlag = .TRUE.
  END IF

  CALL ScanPlantLoopsForNodeNum('MyPlantSizingIndex', NodeNumIn, PlantLoopNum, DummyLoopSideNum, DummyBranchNum )

  IF (PlantLoopNum > 0) THEN
    MyPltLoopNum = PlantLoopNum
  ELSE
    MyPltLoopNum = 0
  END IF

  IF (MyPltLoopNum > 0) THEN
    IF (NumPltSizInput > 0) THEN
      MyPltSizNum = FindItemInList(PlantLoop(MyPltLoopNum)%Name,PlantSizData%PlantLoopName,NumPltSizInput)
    ENDIF
    IF (MyPltSizNum == 0) THEN
      IF (PrintErrorFlag) THEN
        Call ShowSevereError('MyPlantSizingIndex: Could not find ' //TRIM(PlantLoop(MyPltLoopNum)%Name) &
                                 //' in Sizing:Plant objects.')
        Call ShowContinueError('...reference Component Type="'//trim(CompType)//'", Name="'//trim(CompName)//'".')
      ENDIF
      ErrorsFound=.true.
    ENDIF
  ELSE
    IF(PrintErrorFlag)THEN
      Call ShowWarningError('MyPlantSizingIndex: Could not find ' //TRIM(CompType)//  &
         ' with name '//TRIM(CompName)//' on any plant loop')
    END IF
    ErrorsFound=.true.
  END IF

  RETURN

END FUNCTION MyPlantSizingIndex

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

