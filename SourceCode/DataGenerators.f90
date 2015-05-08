MODULE DataGenerators

          ! MODULE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   March 2005
          !       MODIFIED
          !       RE-ENGINEERED  July 2006 BG, generalized and added data for ICE/SE model micro CHP

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for the variables that relate specifically
          ! to the Fuel cell and Micro CHP modeling in EnergyPlus
          !  the data for the older BLAST generators are in those component's modules

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: MaxNameLength

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: NormalizedCurveMode = 1 ! mode where efficiency curves are modifier curves
    INTEGER, PARAMETER :: DirectCurveMode     = 2 ! mode where efficiency curves are direct

    INTEGER, PARAMETER :: ConstantRateSkinLoss = 1 ! fixed rate mode for skin losses
    INTEGER, PARAMETER :: UADTSkinLoss         = 2 ! UAdelta T mode for skin losses
    INTEGER, PARAMETER :: QuadraticFuelNdotSkin= 3 ! Quadratic function of fuel flow for skin losses

    INTEGER, PARAMETER :: QuadraticFuncofNdot  = 1 ! function of fuel rate mode for air flow
    INTEGER, PARAMETER :: ConstantStoicsAirRat = 2 ! Constant air ratio in stoics with fuel constituents
    INTEGER, PARAMETER :: QuadraticFuncofPel   = 3 ! function of electric power mode

    INTEGER, PARAMETER :: NoRecoveryOnAirIntake = 101 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverBurnInvertBatt = 102 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverAuxiliaryBurner= 103 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverInverterBatt   = 104 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverInverter       = 105 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverBattery        = 106 ! mode for controlling intake air heat recovery

    INTEGER, PARAMETER :: RegularAir = 1 !
    INTEGER, PARAMETER :: UserDefinedConstituents = 2 !

    INTEGER, PARAMETER :: FuelInTempFromNode = 1
    INTEGER, PARAMETER :: FuelInTempSchedule = 2

    INTEGER, PARAMETER :: WaterInReformMains    = 21
    INTEGER, PARAMETER :: WaterInReformAirNode  = 22
    INTEGER, PARAMETER :: WaterInReformWaterNode = 23
    INTEGER, PARAMETER :: WaterInReformSchedule  = 24

    INTEGER, PARAMETER :: InverterEffConstant  = 1
    INTEGER, PARAMETER :: InverterEffQuadratic = 2

    INTEGER, PARAMETER :: FixedEffectiveness = 11 !exhaust gas HX modeling mode
    INTEGER, PARAMETER :: LMTDempiricalUAeff = 12 !exhaust gas HX modeling mode
    INTEGER, PARAMETER :: LMTDfundementalUAeff = 13 !exhaust gas HX modeling mode
    INTEGER, PARAMETER :: Condensing = 14           !exhaust gas HX modeling mode

    INTEGER, PARAMETER :: SimpleEffConstraints = 21 !electrical storage modeling mode
    INTEGER, PARAMETER :: LeadAcidBatterySaupe = 22 !electrical storage modeling mode
    INTEGER, PARAMETER :: LeadAcidBatterManwellMcGowan = 23 ! electrical storage modeling mode

    INTEGER, PARAMETER :: SurroundingZone = 31
    INTEGER, PARAMETER :: AirInletForFC   = 32

    INTEGER, PARAMETER :: OpModeOFF      = 1 ! CHP operating mode OFF
    INTEGER, PARAMETER :: OpModeStandby  = 2 ! CHP operating mode Stand By
    INTEGER, PARAMETER :: OpModeWarmUp   = 3 ! CHP operating mode Warm Up or start up
    INTEGER, PARAMETER :: OpModeNormal   = 4 ! CHP operating mode Normal
    INTEGER, PARAMETER :: opModeCoolDown = 5 ! CHP operating mode Cool down or shut down

    INTEGER, PARAMETER :: fuelModeGaseousConstituents = 301
    INTEGER, PARAMETER :: fuelModeGenericLiquid       = 302

    REAL(r64),    PARAMETER :: MinProductGasTemp = 100.0d0 ! Minimum bound on search for product gas temps
    REAL(r64),    PARAMETER :: MaxProductGasTemp = 2000.0d0 ! Maximum bound on search for product gas temps

    INTEGER, PARAMETER :: NISTShomate    = 41
    INTEGER, PARAMETER :: NASAPolynomial = 42

    REAL(r64),    PARAMETER :: RinKJperMolpK  = 0.0083145d0 !R is ideal gas constant (kJ/mol-K)
    REAL(r64),    PARAMETER :: InitHRTemp     = 50.0d0 !Initialization temperature for heat recovery water

    REAL(r64),    PARAMETER :: ImBalanceTol  = 0.00001d0 ! used as fraction of electrical power at power module

          ! DERIVED TYPE DEFINITIONS

TYPE FCPowerModuleStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name     = ' '  !name of this PowerModule data
    INTEGER                      :: EffMode  = 0 ! mode for efficiency curves
    INTEGER                      :: EffCurveID = 0 ! pointer to curve for efficiency
    REAL(r64)                    :: NomEff   = 0.0d0 ! nominal efficiency
    REAL(r64)                    :: NomPel  = 0.0d0 ! nominal power rate at rating point
    INTEGER                      :: NumCycles = 0  ! number of start stop cycles
    REAL(r64)                    :: CyclingDegradRat = 0.0d0 ! rate of degradation from cycles
    REAL(r64)                    :: NumRunHours = 0.0d0 ! number of hours of operation
    REAL(r64)                    :: OperateDegradRat = 0.0d0 ! rate of degradation from run time (per hour)
    REAL(r64)                    :: ThreshRunHours  = 0.0d0 ! number of hours before degradation starts
    REAL(r64)                    :: UpTranLimit = 0.0d0 !power up transient limit
    REAL(r64)                    :: DownTranLimit = 0.0d0 !power down tran limit
    REAL(r64)                    :: StartUpTime   = 0.0d0 !time for start up [hours]
    REAL(r64)                    :: StartUpFuel   = 0.0d0 !fuel use during start up
    REAL(r64)                    :: StartUpElectConsum  = 0.0d0 !electricity used during start up
    REAL(r64)                    :: StartUpElectProd  = 0.0d0 !electricity produced during start up
    REAL(r64)                    :: ShutDownTime  = 0.0d0 ! time to shut down [hours]
    REAL(r64)                    :: ShutDownFuel  = 0.0d0 ! fuel consumed during shut down
    REAL(r64)                    :: ShutDownElectConsum = 0.0d0 !Elect consumed during shut down
    REAL(r64)                    :: ANC0 = 0.0d0 ! Ancilliary Loads constant term
    REAL(r64)                    :: ANC1 = 0.0d0 ! Ancilliary Loads linear term
    INTEGER                      :: SkinLossMode = 0 ! how are skin losses determined
    CHARACTER(len=MaxNameLength) :: ZoneName = ' ' !
    Integer                      :: ZoneID = 0 ! "pointer" to zone with component in it
    REAL(r64)                    :: RadiativeFract = 0.0d0 !
    REAL(r64)                    :: QdotSkin = 0.0d0 !
    REAL(r64)                    :: UAskin = 0.0d0 !
    Integer                      :: SkinLossCurveID = 0 !
    INTEGER                      :: WaterSupplyCurveID = 0 ! pointer to curve for water use in reforming
    REAL(r64)                    :: NdotDilutionAir = 0.0d0 ! user defined constant flow of dilution air (kmol/sec)
    REAL(r64)                    :: StackHeatLossToDilution = 0.0d0 ! (watts)
    CHARACTER(len=MaxNameLength) :: DilutionInletNodeName = ' ' !dilution -> AirHR ?? added air heat recovery path
    INTEGER                      :: DilutionInletNode = 0 ! pointer to node for inlet
    CHARACTER(len=MaxNameLength) :: DilutionExhaustNodeName = ' '
    INTEGER                      :: DilutionExhaustNode = 0 ! pointer to node getting exhaust
    REAL(r64)                    :: PelMin = 0.0d0 ! minimum operating point for FCPM electrical power Pel
    REAL(r64)                    :: PelMax = 0.0d0 ! maximum operating point for FCPM electrical power Pel
   !Calculated values and input from elsewhere

    REAL(r64)                    :: Pel             = 0.0d0 !current DC electrical power produced
    REAL(r64)                    :: PelLastTimeStep = 0.0d0
    REAL(r64)                    :: Eel             = 0.0d0 ! power module efficiency
    REAL(r64)                    :: QdotStackCool   = 0.0d0 ! Heat removed by stack cooler
    REAL(r64)                    :: FractionalDayofLastStartUp = 0.0d0 !fractional days into simulation
    REAL(r64)                    :: FractionalDayofLastShutDown = 0.0d0 !fractional Days into simulations
    LOGICAL                      :: HasBeenOn =.true.
    LOGICAL                      :: DuringShutDown = .false.
    LOGICAL                      :: DuringStartUp = .false.
    REAL(r64)                    :: NdotFuel = 0.0d0 ! molar fuel use rate.  (kmol/sec)
    REAL(r64)                    :: TotFuelInEnthalphy = 0.0d0 ! Enthalpy of fuel coming into FCPM (watts)
    REAL(r64)                    :: NdotProdGas       = 0.0d0  !(kmol/sec)
    REAL(r64), DIMENSION(14)          :: ConstitMolalFract = 0.0d0
    INTEGER, DIMENSION(14)       :: GasLibID  = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array
    REAL(r64)                    :: TprodGasLeavingFCPM = 0.0d0
    REAL(r64)                    :: NdotAir  = 0.0d0 ! molar air use rate    (kmol/sec)
    REAL(r64)                    :: TotAirInEnthalphy  = 0.0d0 ! Enthalpy of air coming nto FCPM energy balance (watts)
    REAL(r64)                    :: NdotLiqwater= 0.0d0    ! molar water use rate (kmol/sec)
    REAL(r64)                    :: TwaterInlet = 0.0d0    !
    REAL(r64)                    :: WaterInEnthalpy    = 0.0d0 ! Enthalpy of liquid water used for reforming (watts)
    REAL(r64)                    :: DilutionAirInEnthalpy = 0.0d0  ! Enthalpy of Dilution air coming into FCPM (watts)
    REAL(r64)                    :: DilutionAirOutEnthalpy = 0.0d0 !
    REAL(r64)                    :: PelancillariesAC    = 0.0d0  !ancillary power (watts)
    REAL(r64)                    :: TotProdGasEnthalphy = 0.0d0 !Enthalphy of product gases leaving FCPM   (watts)
    REAL(r64)                    :: WaterOutEnthalpy    = 0.0d0 ! enthalpy of vapor from water used for reforming
    INTEGER                      :: SeqSubstitIter  = 0 !
    INTEGER                      :: RegulaFalsiIter = 0 !
END TYPE FCPowerModuleStruct

TYPE FCAirSupplyDataStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this
    CHARACTER(len=MaxNameLength) :: NodeName = ' ' ! Air supply node name
    INTEGER                      :: SupNodeNum = 0 ! Air supply node ID
    INTEGER                      :: BlowerPowerCurveID = 0 ! "pointer" to blower power quadratic
    REAL(r64)                    :: BlowerHeatLossFactor = 0.0d0 ! alpha for blower heat loss fraction
    INTEGER                      :: AirSupRateMode = 0 ! control for modeling method used to deterime supply air flow rate
    REAL(r64)                    :: Stoics = 0.0d0 !excess air ratio
    INTEGER                      :: AirFuncPelCurveID = 0 !"pointer" to curve for air as function of power
    REAL(r64)                    :: AirTempCoeff   = 0.0d0 ! coeff a3 in equ 16.
    INTEGER                      :: AirFuncNdotCurveID = 0 !"pointer" to curve for air as function of fuel flow rate
    INTEGER                      :: IntakeRecoveryMode = 0 !
    INTEGER                      :: ConstituentMode = 0 ! how are air data input
    INTEGER                      :: NumConstituents = 0 !
    CHARACTER(len=MaxNameLength), DIMENSION(14) :: ConstitName = ' '
    REAL(r64), DIMENSION(14) :: ConstitMolalFract = 0.0d0
   !Calculated values and input from elsewhere
    INTEGER, DIMENSION(14)        :: GasLibID  = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array

    REAL(r64)                    :: O2fraction = 0.0d0
    REAL(r64)                    :: TairIntoBlower = 0.0d0 ! temperature entering blower
    REAL(r64)                    :: TairIntoFCPM   = 0.0d0 ! temperature leaving blower and entering FCPM
    REAL(r64)                    :: PairCompEl     = 0.0d0 ! power drawn by compressor
    REAL(r64)                    :: QskinLoss      = 0.0d0 ! pumping losses for zone
    REAL(r64)                    :: QintakeRecovery = 0.0d0 !heat recovered on intake air by accessories

END TYPE FCAirSupplyDataStruct

TYPE FCStackCoolerDataStruct
    ! user input data
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this stack cooler module
    CHARACTER(len=MaxNameLength) :: WaterInNodeName  = ' ' !HR Water Inlet Node
    INTEGER                      :: WaterInNode      = 0  ! HR Water Outlet Node ID
    CHARACTER(len=MaxNameLength) :: WaterOutNodeName = ' ' !HR water outlet Node name
    INTEGER                      :: WaterOutNode     = 0 ! HR Water outlet Node ID
    REAL(r64)                    :: TstackNom        = 0.0d0 ! nominal fuel cell stack temperature
    REAL(r64)                    :: TstackActual     = 0.0d0 ! actual fuel cell stack temperature
    REAL(r64)                    :: r0               = 0.0d0 ! stack cooling power coefficient r0
    REAL(r64)                    :: r1               = 0.0d0 ! stack cooling power coefficient r1
    REAL(r64)                    :: r2               = 0.0d0 ! stack cooling power coefficient r2
    REAL(r64)                    :: r3               = 0.0d0 ! stack cooling power coefficient r3
    REAL(r64)                    :: MdotStackCoolant = 0.0d0 ! stack coolant flow rate kg/s
    REAL(r64)                    :: UAs_cool         = 0.0d0 ! stack heat transfer coef
    REAL(r64)                    :: Fs_cogen         = 0.0d0 !
    REAL(r64)                    :: As_cogen         = 0.0d0 !
    REAL(r64)                    :: MdotCogenNom     = 0.0d0 !
    REAL(r64)                    :: hCogenNom        = 0.0d0
    REAL(r64)                    :: ns               = 0.0d0 !
    REAL(r64)                    :: PstackPumpEl     = 0.0d0
    REAL(r64)                    :: PmpPowerLossFactor = 0.0d0
    REAL(r64)                    :: f0 = 0.0d0
    REAL(r64)                    :: f1 = 0.0d0
    REAL(r64)                    :: f2 = 0.0d0

    ! calculated and from elsewhere
    LOGICAL                      :: StackCoolerPresent = .false. ! control modeling
    REAL(r64)                    :: qs_cool  = 0.0d0 !
    REAL(r64)                    :: qs_air   = 0.0d0 !

END Type FCStackCoolerDataStruct

TYPE FCWaterSupplyDataStruct
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this water supply module
    INTEGER                      :: WaterTempMode =0 ! temperature of water inlet determination
    CHARACTER(len=MaxNameLength) :: NodeName = ' ' !node name for temperature at input
    INTEGER                      :: NodeNum = 0 ! node number for temperature at input
    INTEGER                      :: SchedNum = 0 ! water temperature at input
    INTEGER                      :: WaterSupRateCurveID = 0 ! "pointer" to water flow rate curve as a function of fuel rate
    INTEGER                      :: PmpPowerCurveID = 0 !"pointer to Pump power curve as a function of water flow Rate
    REAL(r64)                    :: PmpPowerLossFactor = 0.0d0 !Pump heat loss factor
    !calculated data
    LOGICAL                      :: IsModeled  = .TRUE.
    REAL(r64)                    :: TwaterIntoCompress = 0.0d0 ! inlet Water Temperature
    REAL(r64)                    :: TwaterIntoFCPM     = 0.0d0 ! pumped water temp
    REAL(r64)                    :: PwaterCompEl       = 0.0d0 ! water pump power
    REAL(r64)                    :: QskinLoss          = 0.0d0 ! pumping losses for zone


END TYPE FCWaterSupplyDataStruct

TYPE FCAuxilHeatDataStruct
    CHARACTER(len=MaxNameLength) :: Name      = ' ' !name of this auxiliary heating module
    CHARACTER(len=MaxNameLength) :: ZoneName  = ' '
    INTEGER                      :: ZoneID    = 0
    REAL(r64)                    :: UASkin    = 0.0d0 !for skin losses to zone
    REAL(r64)                    :: ExcessAirRAT = 0.0d0 !
    REAL(r64)                    :: ANC0      = 0.0d0
    REAL(r64)                    :: ANC1      = 0.0d0
    INTEGER                      :: SkinLossDestination = 0 !control mode for where lost heat goes
    REAL(r64)                    :: MaxPowerW = 0.0d0
    REAL(r64)                    :: MinPowerW = 0.0d0
    REAL(r64)                    :: MaxPowerkmolperSec = 0.0d0
    REAL(r64)                    :: MinPowerkmolperSec = 0.0d0
    ! calculated and from elsewhere
    INTEGER                      :: NumConstituents = 0
    REAL(r64)                    :: TauxMix           = 0.0d0
    REAL(r64)                    :: NdotAuxMix        = 0.0d0
    REAL(r64), DIMENSION(14)           :: ConstitMolalFract = 0.0d0
    INTEGER, DIMENSION(14)        :: GasLibID          = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array
    REAL(r64)                    :: QskinLoss         = 0.0d0 !Heat lost to room
    REAL(r64)                    :: QairIntake        = 0.0d0 ! heat into intake air


END TYPE FCAuxilHeatDataStruct

TYPE FCExhaustHXDataStruct
    ! user defined variables
    CHARACTER(len=MaxNameLength) :: Name             = ' ' !name of this exhaust gas heat recovery
    CHARACTER(len=MaxNameLength) :: WaterInNodeName  = ' ' !HR Water Inlet Node
    INTEGER                      :: WaterInNode      = 0  ! HR Water Outlet Node ID
    CHARACTER(len=MaxNameLength) :: WaterOutNodeName = ' ' !HR water outlet Node name
    INTEGER                      :: WaterOutNode     = 0 ! HR Water outlet Node ID
    REAL(r64)                    :: WaterVolumeFlowMax = 0.0d0 ! HR water flow rate max avail
    CHARACTER(len=MaxNameLength) :: ExhaustOutNodeName = ' ' ! air node for exhaust flow
    INTEGER                      :: ExhaustOutNode   = 0 ! Exhaust Air node ID
    INTEGER                      :: HXmodelMode      = 0  !Heat Exchanger Calculation Method
    REAL(r64)                    :: HXEffect         = 0.0d0 ! Heat Exchanger Effectiveness (method 1)
    REAL(r64)                    :: hxs0             = 0.0d0 !(method 2)
    REAL(r64)                    :: hxs1             = 0.0d0 ! (method 2)
    REAL(r64)                    :: hxs2             = 0.0d0 ! (method 2)
    REAL(r64)                    :: hxs3             = 0.0d0 ! (method 2)
    REAL(r64)                    :: hxs4             = 0.0d0 ! (method 2)
    REAL(r64)                    :: h0gas            = 0.0d0 ! (method 3)
    REAL(r64)                    :: NdotGasRef       = 0.0d0 ! (method 3)
    REAL(r64)                    :: nCoeff           = 0.0d0 ! (method 3)
    REAL(r64)                    :: AreaGas          = 0.0d0 ! (method 3)
    REAL(r64)                    :: h0Water          = 0.0d0  ! (method 3)
    REAL(r64)                    :: NdotWaterRef     = 0.0d0 !(method 3)
    REAL(r64)                    :: mCoeff           = 0.0d0 ! (method 3)
    REAL(r64)                    :: AreaWater        = 0.0d0 !(method 3)
    REAL(r64)                    :: Fadjust          = 0.0d0     ! (method 3)
    REAL(r64)                    :: l1Coeff          = 0.0d0 ! (method 4)
    REAL(r64)                    :: l2Coeff          = 0.0d0 ! (method 4)
    REAL(r64)                    :: CondensationThresholdTemp = 0.0d0 ! (method 4) [degrees C]
   !calculated
    REAL(r64)                    :: qHX = 0.0d0 ! heat flow from gas stream to water
    REAL(r64)                    :: THXexh = 0.0d0 ! temperature of exhaust gases leaving heat exchanger.
    REAL(r64)                    :: WaterMassFlowRateDesign = 0.0d0 !Design level of water flow rate
    REAL(r64)                    :: WaterMassFlowRate  = 0.0d0 ! water flow rate in plant loop
    REAL(r64)                    :: WaterInletTemp  = 0.0d0 !
    REAL(r64)                    :: WaterVaporFractExh = 0.0d0 ! water vapor fraction in exhaust gas stream.
    REAL(r64)                    :: CondensateRate     = 0.0d0 ! water condensation rate.
    REAL(r64), DIMENSION(14)     :: ConstitMolalFract = 0.0d0
    INTEGER, DIMENSION(14)       :: GasLibID          = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array
    REAL(r64)                    :: NdotHXleaving       = 0.0d0
    REAL(r64)                    :: WaterOutletTemp     = 0.0D0
    REAL(r64)                    :: WaterOutletEnthalpy = 0.0D0

END TYPE FCExhaustHXDataStruct

TYPE BatteryDichargeDataStruct
    ! user defined variables
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this battery data set
    REAL(r64)                    :: NumInSeries = 0.0d0
    REAL(r64)                    :: NumInParallel = 0.0d0
    REAL(r64)                    :: NominalVoltage = 0.0d0
    REAL(r64)                    :: LowVoltsDischarged = 0.0d0 !not used
    INTEGER                      :: NumTablePairs = 0
    REAL(r64), Allocatable, Dimension(:) :: DischargeCurrent  ! amps
    REAL(r64), Allocatable, Dimension(:) :: DischargeTime     ! hours
    ! calculated variables
    REAL(r64)                    :: k =0.0d0 !parameter in Manwell McGowan model
    REAL(r64)                    :: c =0.0d0 !parameter in Manwell McGowan model
    REAL(r64)                    :: qmax =0.0d0 !parameter in Manwell McGowan model

END TYPE BatteryDichargeDataStruct


TYPE FCElecStorageDataStruct
    !user defined variables
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this electrical storage module
    INTEGER                      :: StorageModelMode = 0
    REAL(r64)                    :: StartingEnergyStored = 0.0d0 !joules inside
    REAL(r64)                    :: EnergeticEfficCharge = 0.0d0 ! for
    REAL(r64)                    :: EnergeticEfficDischarge = 0.0d0
    REAL(r64)                    :: MaxPowerDraw  = 0.0d0 ! for simple bucket method 0
    REAL(r64)                    :: MaxPowerStore  = 0.0d0 ! for simple bucket method 0
    REAL(r64)                    :: NominalVoltage = 0.0d0 !
    REAL(r64)                    :: NominalEnergyCapacity = 0.0d0 ! [J]
    !calculated and from elsewhere vars
    REAL(r64)                    :: ThisTimeStepStateOfCharge =0.0d0 ! [J]
    REAL(r64)                    :: LastTimeStepStateOfCharge =0.0d0 ! [J]
    REAL(r64)                    :: PelNeedFromStorage =0.0d0
    REAL(r64)                    :: IdesiredDischargeCurrent = 0.0d0 !
    REAL(r64)                    :: PelFromStorage  = 0.0d0 ! power
    REAL(r64)                    :: IfromStorage = 0.0d0 ! current this timestepm
    REAL(r64)                    :: PelIntoStorage = 0.0d0 !
    REAL(r64)                    :: QairIntake =0.0d0 ! heat into intake air
    !nested structures
    TYPE(BatteryDichargeDataStruct) :: Battery

END TYPE FCElecStorageDataStruct


TYPE FCInverterDataStruct
    ! user-defined data
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this inverter
    INTEGER                      :: EffMode = 0 !efficiency calculation mode
    REAL(r64)                    :: ConstEff = 0.0d0 !
    INTEGER                      :: EffQuadraticCurveID = 0
    ! calculated and from elsewhere
    REAL(r64)                    :: PCUlosses  = 0.0d0
    REAL(r64)                    :: QairIntake =0.0d0

END TYPE FCInverterDataStruct


TYPE FCReportDataStruct  !these are all for reporting only!
       REAL(r64)    :: ACPowerGen                 = 0.0d0 ! reporting: power (W)
       REAL(r64)    :: ACEnergyGen                = 0.0d0 ! reporting: energy (J)

       REAL(r64)    :: QdotExhaust                = 0.0d0 ! reporting: exhaust gas heat recovered (W)
       REAL(r64)    :: TotalHeatEnergyRec         = 0.0d0 ! reporting: total heat recovered (J)
       REAL(r64)    :: ExhaustEnergyRec           = 0.0d0 ! reporting: exhaust gas heat recovered (J)
       REAL(r64)    :: FuelEnergyLHV              = 0.0d0 ! reporting: Fuel Energy used in Lower Heating Value(J)
       REAL(r64)    :: FuelEnergyUseRateLHV       = 0.0d0 ! reporting: Fuel Energy used in Lower Heating Value(W)
       REAL(r64)    :: FuelEnergyHHV              = 0.0d0 ! reporting: Fuel Energy used in Lower Heating Value(J)
       REAL(r64)    :: FuelEnergyUseRateHHV       = 0.0d0 ! reporting: Fuel Energy used in Lower Heating Value(W)
       REAL(r64)    :: FuelRateMdot               = 0.0d0 ! (Kg/s)
       REAL(r64)    :: HeatRecInletTemp           = 0.0d0 ! reporting: Heat Recovery Loop Inlet Temperature (C)
       REAL(r64)    :: HeatRecOutletTemp          = 0.0d0 ! reporting: Heat Recovery Loop Outlet Temperature (C)
       REAL(r64)    :: HeatRecMdot                = 0.0d0 ! reporting: Heat Recovery Loop Mass flow rate (kg/s)
        ! air supply and blower
       REAL(r64)    :: TairInlet         = 0.0d0 ! State point 1
       REAL(r64)    :: TairIntoFCPM      = 0.0d0 ! Temperature at State point 4
       REAL(r64)    :: NdotAir           = 0.0d0 ! air flow in kmol/sec
       REAL(r64)    :: TotAirInEnthalphy = 0.0d0 ! Enthalpy at State point 4
       REAL(r64)    :: BlowerPower       = 0.0d0 ! electrical power used by air supply blower
       REAL(r64)    :: BlowerEnergy      = 0.0d0 ! electrical energy used by air supply blower
       REAL(r64)    :: BlowerSkinLoss    = 0.0d0 ! heat rate of losses by blower
        !fuel supply and compressor
       REAL(r64)    :: TfuelInlet     = 0.0d0 ! State point 2 [C]
       REAL(r64)    :: TfuelIntoFCPM  = 0.0d0 ! state point 5 [C]
       REAL(r64)    :: NdotFuel       = 0.0d0 ! fuel flow in [kmol/sec]
       REAL(r64)    :: TotFuelInEnthalpy = 0.0d0 ! state point 5 [W]
       REAL(r64)    :: FuelCompressPower = 0.0d0 ! electrical power used by fuel supply compressor [W]
       REAL(r64)    :: FuelCompressEnergy= 0.0d0 ! electrical energy used by fuel supply compressor [J]
       REAL(r64)    :: FuelCompressSkinLoss = 0.0d0 !heat rate of losses.by fuel supply compressor [W]
        !reformer water supply
       REAL(r64)    :: TwaterInlet    = 0.0d0 ! State point 3
       REAL(r64)    :: TwaterIntoFCPM = 0.0d0 ! State point 6
       REAL(r64)    :: NdotWater      = 0.0d0 ! water flow in kmol/sec (reformer water)
       REAL(r64)    :: WaterPumpPower = 0.0d0 ! electrical power used by water pump [W]
       REAL(r64)    :: WaterPumpEnergy = 0.0d0 ! electrical energy used by water pump [J]
       REAL(r64)    :: WaterIntoFCPMEnthalpy = 0.0d0  ! state point 6
        !product (exhaust) gas leaving power module
       REAL(r64)    :: TprodGas       = 0.0d0 ! State point 7 Product Gas temperature
       REAL(r64)    :: EnthalProdGas  = 0.0d0 ! state point 7 product gas enthalpy
       REAL(r64)    :: NdotProdGas    = 0.0d0 ! point 7 flow rate [kmol/sec]
       REAL(r64)    :: NdotProdAr     = 0.0d0 ! argon flow rate at point 7
       REAL(r64)    :: NdotProdCO2    = 0.0d0 ! carbon dioxide flow rate at point 7
       REAL(r64)    :: NdotProdH2O    = 0.0d0 ! water vapor flow rate at point 7
       REAL(r64)    :: NdotProdN2     = 0.0d0 ! nitrogen flow rate at point 7
       REAL(r64)    :: NdotProdO2     = 0.0d0 ! oxygen flow rate at point 7

       !heat exchanger for water to exhaust heat recovery
       REAL(r64)    :: qHX = 0.0d0 ! heat flow from gas stream to water [W]
       REAL(r64)    :: HXenergy = 0.0d0 !energy from gas stream to water [J]
       REAL(r64)    :: THXexh = 0.0d0 ! temperature of exhaust gases leaving heat exchanger.
       REAL(r64)    :: WaterVaporFractExh = 0.0d0 ! water vapor fraction in exhaust gas stream
                                           ! relative to water vapor entering HX  (NdotH20/Ndoaux-mix)
       REAL(r64)    :: CondensateRate     = 0.0d0 ! water condensation rate [kmol/s]

       INTEGER :: SeqSubstIterations = 0 ! number of iterations in SOFC loop
       INTEGER :: RegulaFalsiIterations = 0 ! number of iterations in Tproduct gas solving

       REAL(r64)    :: ACancillariesPower  = 0.0d0 !
       REAL(r64)    :: ACancillariesEnergy = 0.0d0 !
       REAL(r64)    :: PCUlosses = 0.0d0  ! power conditioning Unit losses
       REAL(r64)    :: DCPowerGen = 0.0d0 ! Pel, Power module power level [W]
       REAL(r64)    :: DCPowerEff = 0.0d0 ! Eel, power module efficiency []
       REAL(r64)    :: ElectEnergyinStorage = 0.0d0 ! State of charge in Electrical Storage [J]
       REAL(r64)    :: StoredPower = 0.0d0 ! Power added to Electrical Storage [W]
       REAL(r64)    :: StoredEnergy = 0.0d0 ! energy added to Electrical STorage [J]
       REAL(r64)    :: DrawnPower = 0.0d0 ! Power drawn from Electrical STorage [W]
       REAL(r64)    :: DrawnEnergy = 0.0d0 ! Energy drawn from Electrical STorage [J]

       REAL(r64)    :: SkinLossPower   = 0.0d0 ! heat loss to surrounding zone [W]
       REAL(r64)    :: SkinLossEnergy  = 0.0d0 ! heat loss to surround zone [J]
       REAL(r64)    :: SkinLossConvect = 0.0d0 ! convective heat loss to zone [W]
       REAL(r64)    :: SkinLossRadiat  = 0.0d0 ! radiative heat loss to zone [W}

       REAL(r64)    :: ElectEfficiency  = 0.0d0
       REAL(r64)    :: ThermalEfficiency = 0.0d0
       REAL(r64)    :: OverallEfficiency = 0.0d0
       REAL(r64)    :: ExergyEfficiency  = 0.0d0

END TYPE FCReportDataStruct
TYPE FCDataStruct
  ! from input data and nested types for subsystems
    CHARACTER(len=MaxNameLength) :: Name           = ' ' ! user identifier
    CHARACTER(len=MaxNameLength) :: NameFCPM       = ' ' ! name of FC Power Module
    TYPE(FCPowerModuleStruct)    :: FCPM                 ! data for Power Module
    CHARACTER(len=MaxNameLength) :: NameFCAirSup   = ' ' ! name of air supply module for fuel cell
    TYPE(FCAirSupplyDataStruct)  :: AirSup               ! data for air supply module
    CHARACTER(len=MaxNameLength) :: NameFCFuelSup  = ' ' ! name of fuel supply module
    INTEGER                      :: FuelSupNum     = 0   ! indes for fuel supply module structure
    CHARACTER(len=MaxNameLength) :: NameFCWaterSup = ' ' ! name of water supply module
    TYPE(FCWaterSupplyDataStruct):: WaterSup             ! data for water supply module
    CHARACTER(len=MaxNameLength) :: NameFCAuxilHeat= ' ' ! name of auxiliary heating module
    TYPE(FCAuxilHeatDataStruct)  :: AuxilHeat            ! data for auxiliary heating module
    CHARACTER(len=MaxNameLength) :: NameExhaustHX  = ' ' ! name of Exhaust HX module
    TYPE(FCExhaustHXDataStruct)  :: ExhaustHX            ! data for Exhaust heat exchanger module
    CHARACTER(len=MaxNameLength) :: NameElecStorage= ' ' ! name of Battery module
    TYPE(FCElecStorageDataStruct):: ElecStorage          ! data for Battery module
    CHARACTER(len=MaxNameLength) :: NameInverter   = ' ' ! name of Inverter Module
    TYPE(FCInverterDataStruct)   :: Inverter             ! data for INverter module
    CHARACTER(len=MaxNameLength) :: NameStackCooler   = ' ' ! name of Inverter Module
    TYPE(FCStackCoolerDataStruct) :: StackCooler             ! data for INverter module
    INTEGER                      :: CWLoopNum     = 0  ! cooling water plant loop index number
    INTEGER                      :: CWLoopSideNum = 0  ! cooling water plant loop side index
    INTEGER                      :: CWBranchNum   = 0  ! cooling water plant loop branch index
    INTEGER                      :: CWCompNum     = 0  ! cooling water plant loop component index
    TYPE(FCReportDataStruct)     :: Report               ! data for reporting as E+ output variables

   ! calculated whole-system level variables
    REAL(r64)                    :: ACPowerGen   = 0.0d0 ! Net output from SOFC unit
    REAL(r64)                    :: QconvZone    = 0.0d0  ! convective heat lost to surrounding zone
    REAL(r64)                    :: QradZone     = 0.0d0  ! radiative heat lost to surrounding zone
    INTEGER                      :: DynamicsControlID = 0 !
    REAL(r64)                    :: TimeElapsed  = 0.0d0 ! used to track when timestep has changed
END TYPE

TYPE GeneratorFuelSupplyDataStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this fuel supply module
    INTEGER                      :: FuelTempMode = 0 ! temperature of fuel node
    INTEGER                      :: FuelTypeMode = 0 ! type of fuel, gasous or liquid
    CHARACTER(len=MaxNameLength) :: NodeName = ' ' !node name for temperature at input
    INTEGER                      :: NodeNum = 0 ! node number for temperature at input
    INTEGER                      :: SchedNum = 0 ! fuel temperature at input
    INTEGER                      :: CompPowerCurveID = 0 ! "pointer" to compressor power cubic curve
    REAL(r64)                    :: CompPowerLossFactor = 0.0d0
    INTEGER                      :: NumConstituents  !number of constituents in fue supply
    CHARACTER(len=MaxNameLength), DIMENSION(14) :: ConstitName = ' '
    REAL(r64),   DIMENSION(14)        :: ConstitMolalFract = 0.0d0

    !calculated data (except some for generic liquid)
    INTEGER, DIMENSION(14)       :: GasLibID  = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array
    REAL(r64)                    :: LHV  = 0.0d0     ! lower heating value of gaseous fuel (kJ/mol)
    REAL(r64)                    :: LHVJperkg = 0.0d0  ! lower heating value of gaseous fuel (J/kg)
    REAL(r64)                    :: LHVliquid = 0.0d0 ! userdefined lhv for generic liquid (J/kg)
    REAL(r64)                    :: HHV  = 0.0d0     ! higher heating value of fuel (J/kg)
    REAL(r64)                    :: MW   = 0.0d0     ! molecular weight g/mol
    REAL(r64)                    :: eCO2  = 0.0d0     ! mass flow based CO2 emmissions factor for complete combustion (-)
    REAL(r64)                    :: KmolPerSecToKgPerSec = 0.0d0 ! conversion from moles to kilograms for this fuel. (
    REAL(r64)                    :: StoicOxygenRate = 0.0d0
    REAL(r64)                    :: TfuelIntoCompress = 0.0d0  ! inlet fuel temperature
    REAL(r64)                    :: TfuelIntoFCPM     = 0.0d0  ! compressed fuel temp
    REAL(r64)                    :: PfuelCompEl       = 0.0d0  ! fuel compressor power
    REAL(r64)                    :: QskinLoss         = 0.0d0  ! pumping losses for zone
    REAL(r64)                    :: CO2ProductGasCoef = 0.0d0  ! molar multiplier for stoic products of this fuel
    REAL(r64)                    :: H20ProductGasCoef = 0.0d0  ! molar multiplier for stoic products of this fuel

END TYPE GeneratorFuelSupplyDataStruct

TYPE GasPropertyDataStruct
    CHARACTER(len=MaxNameLength) :: ConstituentName           = ' ' !
    CHARACTER(len=MaxNameLength) :: ConstituentFormula        = ' '
    REAL(r64)                    :: StdRefMolarEnthOfForm     = 0.0d0 !
    INTEGER                      :: ThermoMode = 0 ! method of calculation for thermodynamics
    REAL(r64)                    :: ShomateA = 0.0d0
    REAL(r64)                    :: ShomateB = 0.0d0
    REAL(r64)                    :: ShomateC = 0.0d0
    REAL(r64)                    :: ShomateD = 0.0d0
    REAL(r64)                    :: ShomateE = 0.0d0
    REAL(r64)                    :: ShomateF = 0.0d0
    REAL(r64)                    :: ShomateG = 0.0d0
    REAL(r64)                    :: ShomateH = 0.0d0
    REAL(r64)                    :: NumCarbons = 0.0d0
    REAL(r64)                    :: NumHydrogens = 0.0d0
    REAL(r64)                    :: NumOxygens   = 0.0d0
    REAL(r64)                    :: MolecularWeight = 0.0d0
    REAL(r64)                    :: NASA_A1 = 0.0d0
    REAL(r64)                    :: NASA_A2 = 0.0d0
    REAL(r64)                    :: NASA_A3 = 0.0d0
    REAL(r64)                    :: NASA_A4 = 0.0d0
    REAL(r64)                    :: NASA_A5 = 0.0d0
    REAL(r64)                    :: NASA_A6 = 0.0d0
    REAL(r64)                    :: NASA_A7 = 0.0d0
END TYPE

TYPE GeneratorDynamicsManagerStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name = ' '
    REAL(r64)                    :: PelMin = 0.0d0 ! minimum operating point for electrical power Pel
    REAL(r64)                    :: PelMax = 0.0d0 ! maximum operating point for electrical power Pel
    REAL(r64)                    :: UpTranLimit = 0.0d0 !power up transient limit W/s
    REAL(r64)                    :: DownTranLimit = 0.0d0 !power down tran limit  W/s
    REAL(r64)                    :: UpTranLimitFuel = 0.0d0 ! fuel up transient limit kg/s
    REAL(r64)                    :: DownTranLimitFuel = 0.0d0 ! fuel down transient limit kg/s

    LOGICAL                      :: WarmUpByTimeDelay = .false. ! Warm up mode control
    LOGICAL                      :: WarmUpByEngineTemp = .true. ! Warm up mode control
    REAL(r64)                    :: StartUpTimeDelay   = 0.0d0 !time for start up [hours]
    REAL(r64)                    :: WarmUpDelay   = 0.0d0 ! time for warm up delay [s]

    REAL(r64)                    :: StartUpFuel   = 0.0d0 !fuel use during start up
    REAL(r64)                    :: StartUpElectConsum  = 0.0d0 !electricity used during start up
    REAL(r64)                    :: StartUpElectProd  = 0.0d0 !electricity produced during start up

    REAL(r64)                    :: ShutDownFuel  = 0.0d0 ! fuel consumed during shut down
    REAL(r64)                    :: ShutDownElectConsum = 0.0d0 !Elect consumed during shut down
    REAL(r64)                    :: PcoolDown = 0.0d0 !power during cool down
    REAL(r64)                    :: CoolDownDelay = 0.0d0  ! time for cool down delay [hours]

    INTEGER                      :: NumCyclesInit = 0  ! number of start stop cycles at beginning
    REAL(r64)                    :: NumRunHoursInit = 0.0d0 ! number of hours of operation beginning

    REAL(r64)                    :: Pstandby = 0.0d0  ! standby power [w]
    REAL(r64)                    :: MCeng = 0.0d0 ! aggregated thermal mass of engine [  ]
    REAL(r64)                    :: MCcw  = 0.0d0 ! aggregated thermal mass of heat recovery [   ]
    REAL(r64)                    :: kf = 0.0d0 ! coefficient k_f for warmup fuel flow rate
    REAL(r64)                    :: TnomEngOp = 0.0d0 ! nominal engine operating temperature [C]
    REAL(r64)                    :: kp = 0.0d0 ! coefficient k_p for warmup power

    LOGICAL                      :: MandatoryFullCoolDown = .false. !
    LOGICAL                      :: WarmRestartOkay       = .true.
    INTEGER                      :: AvailabilitySchedID   = 0

   !Calculated values and input from elsewhere
    INTEGER                       :: CurrentOpMode  = OpModeOFF ! current operating mode, uses params like OpModeNormal
    INTEGER                       :: LastOpMode     = OpModeOFF !
    REAL(r64)                     :: FractionalDayofLastShutDown = 0.0d0
    REAL(r64)                     :: FractionalDayofLastStartUp = 0.0d0
    LOGICAL                       :: HasBeenOn = .false.
    LOGICAL                       :: DuringStartUp = .false.
    LOGICAL                       :: DuringShutDown = .false.
    REAL(r64)                     :: FuelMdotLastTimestep  = 0.0d0
    REAL(r64)                     :: PelLastTimeStep = 0.0d0
    INTEGER                       :: NumCycles =0
    REAL(r64)                     :: PLRforSubtimestepStartUp = 0.0d0
    REAL(r64)                     :: PLRforSubtimestepShutDown = 0.0d0 ! part load for not in shut down, shut down part is (1 - PLR)
    REAL(r64)                     :: ElectEffNom = 0.0d0 ! efficiency to use for control decisions
    REAL(r64)                     :: ThermEffNom = 0.0d0 ! thermal efficiency to use fo control decisions
    REAL(r64)                     :: QdotHXMax = 0.0d0 ! Thermal power max
    REAL(r64)                     :: QdotHXMin = 0.0d0 ! thermal power min
    REAL(r64)                     :: QdotHXOpt = 0.0d0 ! thermal power nominal/optimal
END TYPE !GeneratorDynamicsManagerStruct

Type MicroCHPParamsNonNormalized
    !user parameters
    CHARACTER(len=MaxNameLength) :: Name     = ' '  !name of this PowerModule data
    REAL(r64)                    :: MaxElecPower = 0.0d0 ! net electric power [W]
    REAL(r64)                    :: MinElecPower = 0.0d0 ! net electric power [W]
    REAL(r64)                    :: MinWaterMdot = 0.0d0 ! minimum cooling water flow [kg/s]
    REAL(r64)                    :: MaxWaterTemp = 0.0d0 ! limit temp for inlet cooling water [C]
    INTEGER                      :: ElecEffCurveID = 0 ! index for TriQuadratic for electrical efficiency
    INTEGER                      :: ThermalEffCurveID = 0 !index for TriQuadric for thermal efficiency
    LOGICAL                      :: InternalFlowControl = .false. ! Plant or Internal Flow rate control?
    LOGICAL                      :: PlantFlowControl = .true.  ! default is plant control
    INTEGER                      :: WaterFlowCurveID =0 ! index for BiQuadratic for water flow rate internal control
    INTEGER                      :: AirFlowCurveID =0 ! index for Quadratic for generator air flow
    REAL(r64)                    :: DeltaPelMax =0.0d0 ! max rate of change in net electric power [W/s}
    REAL(r64)                    :: DeltaFuelMdotMax = 0.0d0 !Maximum Rate of change in fuel flow rate [kmol/s2]
    REAL(r64)                    :: UAhx = 0.0d0 ! heat exchanger UA [W/K]
    REAL(r64)                    :: UAskin = 0.0d0 ! skin loss UA [W/K]
    REAL(r64)                    :: RadiativeFraction =0.0d0 ! skin loss fraction to radiant energy []
    REAL(r64)                    :: MCeng = 0.0d0 ! aggregated thermal mass of engine [J/K]
    REAL(r64)                    :: MCcw  = 0.0d0 ! aggregated thermal mass of heat recovery [J/k]
    REAL(r64)                    :: Pstandby = 0.0d0  ! standby power [w]
    LOGICAL                      :: WarmUpByTimeDelay = .false. ! Warm up mode control
    LOGICAL                      :: WarmUpByEngineTemp = .true. ! Warm up mode control
    REAL(r64)                    :: kf = 0.0d0 ! coefficient k_f for warmup fuel flow rate
    REAL(r64)                    :: TnomEngOp = 0.0d0 ! nominal engine operating temperature [C]
    REAL(r64)                    :: kp = 0.0d0 ! coefficient k_p for warmup power
    REAL(r64)                    :: Rfuelwarmup = 0.0d0 ! Warm Up Fuel Flow Rate Limit Ratio
    REAL(r64)                    :: WarmUpDelay = 0.0d0 ! time for warm up delay [s]
    REAL(r64)                    :: PcoolDown = 0.0d0 !power during cool down
    REAL(r64)                    :: CoolDownDelay = 0.0d0  ! time for cool down delay [s]
    LOGICAL                      :: MandatoryFullCoolDown = .false. !
    LOGICAL                      :: WarmRestartOkay       = .true.
    ! calculated and from elsewhere
    REAL(r64)                    :: TimeElapsed = 0.0d0     ! Fraction of the current hour that has elapsed (h)
                                                          ! Saved in order to identify the beginning of a new system time
    INTEGER                      :: opMode = 0
    REAL(r64)    :: OffModeTime      = 0.0d0 ! amount of time generator spent in Off mode
    REAL(r64)    :: StandyByModeTime = 0.0d0 ! amount of time generator spent in standby mode
    REAL(r64)    :: WarmUpModeTime   = 0.0d0 ! amount of time generator spent in warm up mode
    REAL(r64)    :: NormalModeTime   = 0.0d0 ! amount of time generator spent in normal mode
    REAL(r64)    :: CoolDownModeTime = 0.0d0 ! amount of time generator spent in Cool down mode
    REAL(r64)                    :: TengLast = 20.0d0  ! last timestep's value for engine temperature
    REAL(r64)                    :: TempCWOutLast = 20.0d0  !  last timestep's value for cooling water outlet temperature
    REAL(r64)                    :: Pnet = 0.0d0
    REAL(r64)                    :: ElecEff = 0.0d0
    REAL(r64)                    :: Qgross = 0.0d0
    REAL(r64)                    :: ThermEff = 0.0d0
    REAL(r64)                    :: Qgenss = 0.0d0
    REAL(r64)                    :: NdotFuel = 0.0d0
    REAL(r64)                    :: MdotFuel = 0.0d0
    REAL(r64)                    :: Teng = 20.0d0
    REAL(r64)                    :: Tcwin = 20.0d0
    REAL(r64)                    :: Tcwout = 20.0d0
    REAL(r64)                    :: MdotAir = 0.0d0
    REAL(r64)                    :: QdotSkin = 0.0d0 ! rate of heat loss to zone
    REAL(r64)                    :: QdotConvZone = 0.0d0
    REAL(r64)                    :: QdotRadZone= 0.0d0
END TYPE

TYPE MicroCHPReportDataStruct  !these are all for reporting only!
       INTEGER :: Mode                       = 0 ! report operating mode (dev only, remove at end)
       REAL(r64)    :: OffModeTime                = 0.0d0 ! amount of time generator spent in Off mode
       REAL(r64)    :: StandyByModeTime           = 0.0d0 ! amount of time generator spent in standby mode
       REAL(r64)    :: WarmUpModeTime             = 0.0d0 ! amount of time generator spent in warm up mode
       REAL(r64)    :: NormalModeTime             = 0.0d0 ! amount of time generator spent in normal mode
       REAL(r64)    :: CoolDownModeTime           = 0.0d0 ! amount of time generator spent in Cool down mode

       REAL(r64)    :: ACPowerGen                 = 0.0d0 ! reporting: power (W)
       REAL(r64)    :: ACEnergyGen                = 0.0d0 ! reporting: energy (J)
       REAL(r64)    :: Qdotgross                  = 0.0d0 ! reporting: interim gross power (W)
       REAL(r64)    :: Qgenss                     = 0.0d0 ! reporting: net recovered heat rate steadystate(0)
       REAL(r64)    :: QdotHX                     = 0.0d0 ! reporting: rate of heat exchange from engine to coolant (W)
       REAL(r64)    :: QdotHR                     = 0.0d0 ! reporting: rate of heat recovered (W)
       REAL(r64)    :: Tengine                    = 0.0d0 ! reporting: engine mass temperature (C)

       REAL(r64)    :: TotalHeatEnergyRec         = 0.0d0 ! reporting: total heat recovered (J)
       REAL(r64)    :: ExhaustEnergyRec           = 0.0d0 ! reporting: exhaust gas heat recovered (J)
       REAL(r64)    :: FuelEnergyLHV              = 0.0d0 ! reporting: Fuel Energy used in Lower Heating Value(J)
       REAL(r64)    :: FuelEnergyUseRateLHV       = 0.0d0 ! reporting: Fuel Energy used in Lower Heating Value(W)
       REAL(r64)    :: FuelEnergyHHV              = 0.0d0 ! reporting: Fuel Energy used in Higher Heating Value(J)
       REAL(r64)    :: FuelEnergyUseRateHHV       = 0.0d0 ! reporting: Fuel Energy used in Higher Heating Value(W)
       REAL(r64)    :: HeatRecInletTemp           = 0.0d0 ! reporting: Heat Recovery Loop Inlet Temperature (C)
       REAL(r64)    :: HeatRecOutletTemp          = 0.0d0 ! reporting: Heat Recovery Loop Outlet Temperature (C)
       REAL(r64)    :: HeatRecMdot                = 0.0d0 ! reporting: Heat Recovery Loop Mass flow rate (kg/s)


       ! air supply and blower
       REAL(r64)    :: TairInlet         = 0.0d0 ! State point 1
       REAL(r64)    :: MdotAir           = 0.0d0 ! air flow in kmol/sec
        !fuel supply and compressor
       REAL(r64)    :: TfuelInlet     = 0.0d0 ! State point 2 [C]
       REAL(r64)    :: NdotFuel       = 0.0d0 ! fuel flow in [kmol/sec]
       REAL(r64)    :: MdotFuel       = 0.0d0 ! fuel flow in [kg/s]
       REAL(r64)    :: FuelCompressPower = 0.0d0 ! electrical power used by fuel supply compressor [W]
       REAL(r64)    :: FuelCompressEnergy= 0.0d0 ! electrical energy used by fuel supply compressor [J]
       REAL(r64)    :: FuelCompressSkinLoss = 0.0d0 !heat rate of losses.by fuel supply compressor [W]

       !heat exchanger for water to exhaust heat recovery
    !   REAL(r64)    :: qHX = 0.0d0 ! heat flow from gas stream to water [W]
    !   REAL(r64)    :: HXenergy = 0.0d0 !energy from gas stream to water [J]
    !   REAL(r64)    :: THXexh = 0.0d0 ! temperature of exhaust gases leaving heat exchanger.
    !   REAL(r64)    :: WaterVaporFractExh = 0.0d0 ! water vapor fraction in exhaust gas stream
                                           ! relative to water vapor entering HX  (NdotH20/Ndoaux-mix)


   !    INTEGER :: SeqSubstIterations = 0 ! number of iterations in SOFC loop
   !    INTEGER :: RegulaFalsiIterations = 0 ! number of iterations in Tproduct gas solving


       REAL(r64)    :: SkinLossPower   = 0.0d0 ! heat loss to surrounding zone [W]
       REAL(r64)    :: SkinLossEnergy  = 0.0d0 ! heat loss to surround zone [J]
       REAL(r64)    :: SkinLossConvect = 0.0d0 ! convective heat loss to zone [W]
       REAL(r64)    :: SkinLossRadiat  = 0.0d0 ! radiative heat loss to zone [W}

       REAL(r64)    :: ElectEfficiency  = 0.0d0
       REAL(r64)    :: ThermalEfficiency = 0.0d0
       REAL(r64)    :: OverallEfficiency = 0.0d0

END TYPE MicroCHPReportDataStruct

TYPE MicroCHPDataStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name                = ' '  !name of this Micro CHP Generator
    CHARACTER(len=MaxNameLength) :: ParamObjName        = '' !name of parameter object
    Type(MicroCHPParamsNonNormalized):: A42Model ! Nested parameter data structure
    LOGICAL                      :: ModelTypeAnnex42  = .true. ! normalized =  non-normalized?
    REAL(r64)                    :: NomEff   = 0.0D0 ! nominal efficiency
    CHARACTER(len=MaxNameLength) :: ZoneName = '' !
    INTEGER                      :: ZoneID = 0 !
    CHARACTER(len=MaxNameLength) :: PlantInletNodeName  = ''
    INTEGER                      :: PlantInletNodeID    = 0
    CHARACTER(len=MaxNameLength) :: PlantOutletNodeName = ''
    INTEGER                      :: PlantOutletNodeID   = 0
    REAL(r64)        :: PlantMassFlowRate   = 0.0D0  ! only if internal control
    REAL(r64)        :: PlantMassFlowRateMax = 0.0D0 ! hardware limit for node%massflowrateMax
    CHARACTER(len=MaxNameLength) :: AirInletNodeName    = ''
    INTEGER                      :: AirInletNodeID      = 0
    CHARACTER(len=MaxNameLength) :: AirOutletNodeName   = ''
    INTEGER                      :: AirOutletNodeID     = 0
    TYPE(MicroCHPReportDataStruct)::Report ! structure of report variables
    INTEGER                      :: FuelSupplyID        = 0 ! index for fuel supply data structure
    INTEGER                      :: DynamicsControlID   = 0 ! index in GeneratorDynamics data where control issues are handled
    INTEGER                      :: AvailabilitySchedID = 0 !index for availability schedule
    INTEGER                      :: CWLoopNum     = 0  ! cooling water plant loop index number
    INTEGER                      :: CWLoopSideNum = 0  ! cooling water plant loop side index
    INTEGER                      :: CWBranchNum   = 0  ! cooling water plant loop branch index
    INTEGER                      :: CWCompNum     = 0  ! cooling water plant loop component index
END TYPE !MicroCHP


          ! MODULE VARIABLE DECLARATIONS:
TYPE (FCDataStruct),    ALLOCATABLE, DIMENSION(:)  :: FuelCell  !dimension to number of machines
TYPE (GasPropertyDataStruct), ALLOCATABLE, DIMENSION(:) :: GasPhaseThermoChemistryData
TYPE (GeneratorFuelSupplyDataStruct), ALLOCATABLE, DIMENSION(:) :: FuelSupply !fuel supply (reused across various)
TYPE (MicroCHPDataStruct), ALLOCATABLE, DIMENSION(:)            :: MicroCHP
TYPE (MicroCHPParamsNonNormalized), Allocatable, Dimension(:)   :: MicroCHPParamInput !  Used during get input then put into nested
TYPE (GeneratorDynamicsManagerStruct) , Allocatable, Dimension(:)  :: GeneratorDynamics

INTEGER  :: NumFuelConstit=0
INTEGER  :: NumGeneratorFuelSups=0
INTEGER  :: NumFuelCellGenerators=0 ! number of SOFC Generators specified in input
INTEGER  :: NumMicroCHPs=0 !
INTEGER  :: NumMicroCHPParams  =0  ! number of parameter sets for micro chp
INTEGER  :: NumGensWDynamics =0 ! number of dynamics controls for generators

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

END MODULE DataGenerators
