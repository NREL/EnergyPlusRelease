MODULE DataHeatBalance      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   August 1997 (rewritten)
          !       MODIFIED       Aug-Oct 1997 RKS (added derived types)
          !       MODIFIED       Feb 1999 (FW) Added visible radiation parameters,
          !                      WindowShadingControl type and SurfaceWindowCalc type
          !                      Sep 1999 (FW) Added/modified Window4 gas variables
          !                      Jul 2003 (CC) Added reference temperature variable for air models
          !                      Aug 2003 (FW) Added FractionReturnAirPlenTempCoeff1 and
          !                      FractionReturnAirPlenTempCoeff2 to Type LightsData
          !                      Nov 2003 (FW) Add FullExteriorWithRefl and FullInteriorExteriorWithRefl
          !                       as SolarDistribution values
          !                      Dec 2003 (PGE) Added Zone List and Zone Group; added SNLoad variables
          !                      August 2006 (COP) Added variable k coefficient and PCM enthalpy.
          !                      Dec 2006 (DJS-PSU) Added ecoroof material
          !                      Dec 2008 TH added new properties to MaterialProperties and
          !                              ConstructionData for thermochromic windows
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module should contain the information that is needed to pass
          ! from the Heat Balance Module and all of the Zone Initializations
          ! such as ConductionTransferFunction, GlassCalculation,
          ! SolarShading, etc. Modules.

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals,  ONLY: MaxNameLength,AutoCalculate,DegToRadians
USE DataSurfaces, ONLY: MaxSlatAngs
USE DataVectorTypes
USE DataBSDFWindow, ONLY: BSDFWindowInputStruct,BSDFLayerAbsorpStruct
USE DataComplexFenestration
USE DataWindowEquivalentLayer, ONLY: CFSMAXNL

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC    ! By definition, all variables which are placed in this data-only
          ! module should be available to other modules and routines.  Thus,
          ! all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:

          ! Parameters for the definition and limitation of arrays:
INTEGER, PARAMETER :: MaxLayersInConstruct = 11 ! Maximum number of layers allowed in a single construction
INTEGER, PARAMETER :: MaxCTFTerms = 19 ! Maximum number of CTF terms allowed to still allow stability
INTEGER, PARAMETER :: MaxSolidWinLayers = 5 ! Maximum number of solid layers in a window construction
INTEGER, PARAMETER :: MaxSpectralDataElements=800 ! Maximum number in Spectral Data arrays.

          ! Parameters to indicate material group type for use with the Material
          ! derived type (see below):

INTEGER, PARAMETER :: RegularMaterial         = 0
INTEGER, PARAMETER :: Air                     = 1
INTEGER, PARAMETER :: Shade                   = 2
INTEGER, PARAMETER :: WindowGlass             = 3
INTEGER, PARAMETER :: WindowGas               = 4
INTEGER, PARAMETER :: WindowBlind             = 5
INTEGER, PARAMETER :: WindowGasMixture        = 6
INTEGER, PARAMETER :: Screen                  = 7
INTEGER, PARAMETER :: EcoRoof                 = 8
INTEGER, PARAMETER :: IRTMaterial             = 9
INTEGER, PARAMETER :: WindowSimpleGlazing     = 10
INTEGER, PARAMETER :: ComplexWindowShade      = 11
INTEGER, PARAMETER :: ComplexWindowGap        = 12

INTEGER, PARAMETER :: GlassEquivalentLayer    = 13
INTEGER, PARAMETER :: ShadeEquivalentLayer    = 14
INTEGER, PARAMETER :: DrapeEquivalentLayer    = 15
INTEGER, PARAMETER :: BlindEquivalentLayer    = 16
INTEGER, PARAMETER :: ScreenEquivalentLayer   = 17
INTEGER, PARAMETER :: GapEquivalentLayer      = 18

CHARACTER(len=*), PARAMETER, DIMENSION(-1:18) :: cMaterialGroupType=  &
  (/'invalid                               ',  &
    'Material/Material:NoMass              ',  &
    'Material:AirGap                       ',  &
    'WindowMaterial:Shade                  ',  &
    'WindowMaterial:Glazing*               ',  &
    'WindowMaterial:Gas                    ',  &
    'WindowMaterial:Blind                  ',  &
    'WindowMaterial:GasMixture             ',  &
    'WindowMaterial:Screen                 ',  &
    'Material:RoofVegetation               ',  &
    'Material:InfraredTransparent          ',  &
    'WindowMaterial:SimpleGlazingSystem    ',  &
    'WindowMaterial:ComplexShade           ',  &
    'WindowMaterial:Gap                    ',  &
    'WindowMaterial:Glazing:EquivalentLayer',  &
    'WindowMaterial:Shade:EquivalentLayer  ',  &
    'WindowMaterial:Drape:EquivalentLayer  ',  &
    'WindowMaterial:Blind:EquivalentLayer  ',  &
    'WindowMaterial:Screen:EquivalentLayer ',  &
    'WindowMaterial:Gap:EquivalentLayer    '/)

          ! Parameters to indicate surface roughness for use with the Material
          ! derived type (see below):

INTEGER, PARAMETER :: VeryRough    = 1
INTEGER, PARAMETER :: Rough        = 2
INTEGER, PARAMETER :: MediumRough  = 3
INTEGER, PARAMETER :: MediumSmooth = 4
INTEGER, PARAMETER :: Smooth       = 5
INTEGER, PARAMETER :: VerySmooth   = 6

          ! Parameters to indicate blind orientation for use with the Material
          ! derived type (see below):

INTEGER, PARAMETER :: Horizontal = 1
INTEGER, PARAMETER :: Vertical   = 2
INTEGER, PARAMETER :: FixedSlats = 1
INTEGER, PARAMETER :: VariableSlats = 2
          ! Parameters for Interior and Exterior Solar Distribution

INTEGER, PARAMETER :: MinimalShadowing     = -1  ! all incoming solar hits floor, no exterior shadowing except reveals
INTEGER, PARAMETER :: FullExterior         =  0  ! all incoming solar hits floor, full exterior shadowing
INTEGER, PARAMETER :: FullInteriorExterior =  1  ! full interior solar distribution, full exterior solar shadowing
INTEGER, PARAMETER :: FullExteriorWithRefl =  2  ! all incoming solar hits floor, full exterior shadowing and reflections
INTEGER, PARAMETER :: FullInteriorExteriorWithRefl =  3  ! full interior solar distribution,
                                                         ! full exterior shadowing and reflections
          ! Parameters to indicate the zone type for use with the Zone derived
          ! type (see below--Zone%OfType):

INTEGER, PARAMETER :: StandardZone = 1
!INTEGER, PARAMETER :: PlenumZone = 2
!INTEGER, PARAMETER :: SolarWallZone = 11  ! from old ZTYP, OSENV
!INTEGER, PARAMETER :: RoofPondZone = 12   ! from old ZTYP, OSENV

          ! Parameters to indicate the convection correlation being used for use with
          ! InsideConvectionAlgo and OutsideConvectionAlgo

INTEGER, PARAMETER :: ASHRAESimple    = 1
INTEGER, PARAMETER :: ASHRAETARP      = 2
INTEGER, PARAMETER :: CeilingDiffuser = 3  ! Only valid for inside use
INTEGER, PARAMETER :: TrombeWall      = 4  ! Only valid for inside use
INTEGER, PARAMETER :: TarpHcOutside   = 5  ! Only valid for outside use
INTEGER, PARAMETER :: MoWittHcOutside = 6  ! Only valid for outside use
INTEGER, PARAMETER :: DOE2HcOutside   = 7  ! Only valid for outside use
INTEGER, PARAMETER :: BLASTHcOutside  = 8  ! Only valid for outside use
INTEGER, PARAMETER :: AdaptiveConvectionAlgorithm = 9 !

          ! Parameters for WarmupDays
INTEGER, PARAMETER :: DefaultMaxNumberOfWarmupDays=25   ! Default maximum number of warmup days allowed
INTEGER, PARAMETER :: DefaultMinNumberOfWarmupDays=6    ! Default minimum number of warmup days allowed


          ! Parameters for Sky Radiance Distribution
INTEGER, PARAMETER :: Isotropic      = 0
INTEGER, PARAMETER :: Anisotropic    = 1

           ! Parameters for HeatTransferAlgosUsed
INTEGER, PARAMETER :: UseCTF  = 1
INTEGER, PARAMETER :: UseEMPD = 2
INTEGER, PARAMETER :: UseCondFD  = 5
INTEGER, PARAMETER :: UseHAMT = 6

           ! Parameters for ZoneAirSolutionAlgo
INTEGER, PARAMETER :: Use3rdOrder  = 0
INTEGER, PARAMETER :: UseAnalyticalSolution = 1
INTEGER, PARAMETER :: UseEulerMethod  = 2

           ! Parameter for MRT calculation type
INTEGER, PARAMETER :: ZoneAveraged    = 1
INTEGER, PARAMETER :: SurfaceWeighted = 2
INTEGER, PARAMETER :: AngleFactor     = 3

          ! Parameters for Ventilation
INTEGER, PARAMETER :: NaturalVentilation = 0
INTEGER, PARAMETER :: IntakeVentilation  = 1
INTEGER, PARAMETER :: ExhaustVentilation = 2
INTEGER, PARAMETER :: BalancedVentilation = 3

          ! Parameters for hybrid ventilation using Ventilation and Mixing objects
INTEGER, PARAMETER :: HybridControlTypeIndiv  = 0
INTEGER, PARAMETER :: HybridControlTypeClose  = 1
INTEGER, PARAMETER :: HybridControlTypeGlobal = 2

! System type, detailed refrigeration or refrigerated case rack
INTEGER, PARAMETER :: RefrigSystemTypeDetailed    = 1
INTEGER, PARAMETER :: RefrigSystemTypeRack        = 2

! Refrigeration condenser type
INTEGER, PARAMETER :: RefrigCondenserTypeAir     = 1
INTEGER, PARAMETER :: RefrigCondenserTypeEvap    = 2
INTEGER, PARAMETER :: RefrigCondenserTypeWater   = 3
INTEGER, PARAMETER :: RefrigCondenserTypeCascade = 4

           ! Parameters for type of infiltration model
INTEGER, PARAMETER :: InfiltrationDesignFlowRate  = 1
INTEGER, PARAMETER :: InfiltrationShermanGrimsrud = 2
INTEGER, PARAMETER :: InfiltrationAIM2            = 3

           ! Parameters for type of ventilation model
INTEGER, PARAMETER :: VentilationDesignFlowRate  = 1
INTEGER, PARAMETER :: VentilationWindAndStack    = 2

           ! Parameters for type of zone air balance model
INTEGER, PARAMETER :: AirBalanceNone  = 0
INTEGER, PARAMETER :: AirBalanceQuadrature = 1


INTEGER, PARAMETER :: NumZoneIntGainDeviceTypes = 45
CHARACTER(len=*), PARAMETER, DIMENSION(NumZoneIntGainDeviceTypes) :: ZoneIntGainDeviceTypes= &
        (/'PEOPLE                                                              ', & !01
          'LIGHTS                                                              ', & !02
          'ELECTRICEQUIPMENT                                                   ', & !03
          'GASEQUIPMENT                                                        ', & !04
          'HOTWATEREQUIPMENT                                                   ', & !05
          'STEAMEQUIPMENT                                                      ', & !06
          'OTHEREQUIPMENT                                                      ', & !07
          'ZONEBASEBOARD:OUTDOORTEMPERATURECONTROLLED                          ', & !08
          'ZONECONTAMINANTSOURCEANDSINK:CARBONDIOXIDE                          ', & !09
          'WATERUSE:EQUIPMENT                                                  ', & !10
          'DAYLIGHTINGDEVICE:TUBULAR                                           ', & !11
          'WATERHEATER:MIXED                                                   ', & !12
          'WATERHEATER:STRATIFIED                                              ', & !13
          'THERMALSTORAGE:CHILLEDWATER:MIXED                                   ', & !14
          'THERMALSTORAGE:CHILLEDWATER:STRATIFIED                              ', & !15
          'GENERATOR:FUELCELL                                                  ', & !16
          'GENERATOR:MICROCHP                                                  ', & !17
          'ELECTRICLOADCENTER:TRANSFORMER                                      ', & !18
          'ELECTRICLOADCENTER:INVERTER:SIMPLE                                  ', & !19
          'ELECTRICLOADCENTER:INVERTER:FUNCTIONOFPOWER                         ', & !20
          'ELECTRICLOADCENTER:INVERTER:LOOKUPTABLE                             ', & !21
          'ELECTRICLOADCENTER:STORAGE:BATTERY                                  ', & !22
          'ELECTRICLOADCENTER:STORAGE:SIMPLE                                   ', & !23
          'PIPE:INDOOR                                                         ', & !24
          'REFRIGERATION:CASE                                                  ', & !25
          'REFRIGERATION:COMPRESSORRACK                                        ', & !26
          'REFRIGERATION:SYSTEM:CONDENSER:AIRCOOLED                            ', & !27
          'REFRIGERATION:TRANSCRITICALSYSTEM:GASCOOLER:AIRCOOLED               ', & !28
          'REFRIGERATION:SYSTEM:SUCTIONPIPE                                    ', & !29
          'REFRIGERATION:TRANSCRITICALSYSTEM:SUCTIONPIPEMT                     ', & !30
          'REFRIGERATION:TRANSCRITICALSYSTEM:SUCTIONPIPELT                     ', & !31
          'REFRIGERATION:SECONDARYSYSTEM:RECEIVER                              ', & !32
          'REFRIGERATION:SECONDARYSYSTEM:PIPE                                  ', & !33
          'REFRIGERATION:WALKIN                                                ', & !34
          'PUMP:VARIABLESPEED                                                  ', & !35
          'PUMP:CONSTANTSPEED                                                  ', & !36
          'PUMP:VARIABLESPEED:CONDENSATE                                       ', & !37
          'HEADEREDPUMPS:VARIABLESPEED                                         ', & !38
          'HEADEREDPUMPS:CONSTANTSPEED                                         ', & !39
          'ZONECONTAMINANTSOURCEANDSINK:GENERICCONTAMINANT                     ', & !40
          'PLANTCOMPONENT:USERDEFINED                                          ', & !41
          'COIL:USERDEFINED                                                    ', & !42
          'ZONEHVAC:FORCEDAIR:USERDEFINED                                      ', & !43
          'AIRTERMINAL:SINGLEDUCT:USERDEFINED                                  ', & !44
          'COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE                          '/)  !45

CHARACTER(len=*), PARAMETER, DIMENSION(NumZoneIntGainDeviceTypes) :: ccZoneIntGainDeviceTypes= &
        (/'People                                                              ', & !01
          'Lights                                                              ', & !02
          'ElectricEquipment                                                   ', & !03
          'GasEquipment                                                        ', & !04
          'HotWaterEquipment                                                   ', & !05
          'SteamEquipment                                                      ', & !06
          'OtherEquipment                                                      ', & !07
          'ZoneBaseboard:OutdoorTemperatureControlled                          ', & !08
          'ZoneContaminantSourceAndSink:CarbonDioxide                          ', & !09
          'WaterUse:Equipment                                                  ', & !10
          'DaylightingDevice:Tubular                                           ', & !11
          'WaterHeater:Mixed                                                   ', & !12
          'WaterHeater:Stratified                                              ', & !13
          'ThermalStorage:ChilledWater:Mixed                                   ', & !14
          'ThermalStorage:ChilledWater:Stratified                              ', & !15
          'Generator:FuelCell                                                  ', & !16
          'Generator:MicroCHP                                                  ', & !17
          'ElectricLoadCenter:Transformer                                      ', & !18
          'ElectricLoadCenter:Inverter:Simple                                  ', & !19
          'ElectricLoadCenter:Inverter:FunctionOfPower                         ', & !20
          'ElectricLoadCenter:Inverter:LookUpTable                             ', & !21
          'ElectricLoadCenter:Storage:Battery                                  ', & !22
          'ElectricLoadCenter:Storage:Simple                                   ', & !23
          'Pipe:Indoor                                                         ', & !24
          'Refrigeration:Case                                                  ', & !25
          'Refrigeration:CompressorRack                                        ', & !26
          'Refrigeration:System:Condenser:AirCooled                            ', & !27
          'Refrigeration:TranscriticalSystem:GasCooler:AirCooled               ', & !28
          'Refrigeration:System:SuctionPipe                                    ', & !29
          'Refrigeration:TranscriticalSystem:SuctionPipeMT                     ', & !30
          'Refrigeration:TranscriticalSystem:SuctionPipeLT                     ', & !31
          'Refrigeration:SecondarySystem:Receiver                              ', & !32
          'Refrigeration:SecondarySystem:Pipe                                  ', & !33
          'Refrigeration:WalkIn                                                ', & !34
          'Pump:VariableSpeed                                                  ', & !35
          'Pump:ConstantSpeed                                                  ', & !36
          'Pump:VariableSpeed:Condensate                                       ', & !37
          'HeaderedPumps:VariableSpeed                                         ', & !38
          'HeaderedPumps:ConstantSpeed                                         ', & !39
          'ZoneContaminantSourceAndSink:GenericContaminant                     ', & !40
          'PlantComponent:UserDefined                                          ', & !41
          'Coil:UserDefined                                                    ', & !42
          'ZoneHVAC:ForcedAir:UserDefined                                      ', & !43
          'AirTerminal:SingleDuct:UserDefined                                  ', & !44
          'Coil:Cooling:DX:SingleSpeed:ThermalStorage                          '/)  !45

INTEGER, PARAMETER ::  IntGainTypeOf_People                                    = 1
INTEGER, PARAMETER ::  IntGainTypeOf_Lights                                    = 2
INTEGER, PARAMETER ::  IntGainTypeOf_ElectricEquipment                         = 3
INTEGER, PARAMETER ::  IntGainTypeOf_GasEquipment                              = 4
INTEGER, PARAMETER ::  IntGainTypeOf_HotWaterEquipment                         = 5
INTEGER, PARAMETER ::  IntGainTypeOf_SteamEquipment                            = 6
INTEGER, PARAMETER ::  IntGainTypeOf_OtherEquipment                            = 7
INTEGER, PARAMETER ::  IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled = 8
INTEGER, PARAMETER ::  IntGainTypeOf_ZoneContaminantSourceAndSinkCarbonDioxide = 9
INTEGER, PARAMETER ::  IntGainTypeOf_WaterUseEquipment                         = 10
INTEGER, PARAMETER ::  IntGainTypeOf_DaylightingDeviceTubular                  = 11
INTEGER, PARAMETER ::  IntGainTypeOf_WaterHeaterMixed                          = 12
INTEGER, PARAMETER ::  IntGainTypeOf_WaterHeaterStratified                     = 13
INTEGER, PARAMETER ::  IntGainTypeOf_ThermalStorageChilledWaterMixed           = 14
INTEGER, PARAMETER ::  IntGainTypeOf_ThermalStorageChilledWaterStratified      = 15
INTEGER, PARAMETER ::  IntGainTypeOf_GeneratorFuelCell                         = 16
INTEGER, PARAMETER ::  IntGainTypeOf_GeneratorMicroCHP                         = 17
INTEGER, PARAMETER ::  IntGainTypeOf_ElectricLoadCenterTransformer             = 18
INTEGER, PARAMETER ::  IntGainTypeOf_ElectricLoadCenterInverterSimple          = 19
INTEGER, PARAMETER ::  IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower = 20
INTEGER, PARAMETER ::  IntGainTypeOf_ElectricLoadCenterInverterLookUpTable     = 21
INTEGER, PARAMETER ::  IntGainTypeOf_ElectricLoadCenterStorageBattery          = 22
INTEGER, PARAMETER ::  IntGainTypeOf_ElectricLoadCenterStorageSimple           = 23
INTEGER, PARAMETER ::  IntGainTypeOf_PipeIndoor                                = 24
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationCase                         = 25
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationCompressorRack               = 26
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationSystemAirCooledCondenser     = 27
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationTransSysAirCooledGasCooler   = 28
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationSystemSuctionPipe            = 29
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationTransSysSuctionPipeMT        = 30
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationTransSysSuctionPipeLT        = 31
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationSecondaryReceiver            = 32
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationSecondaryPipe                = 33
INTEGER, PARAMETER ::  IntGainTypeOf_RefrigerationWalkIn                       = 34
INTEGER, PARAMETER ::  IntGainTypeOf_Pump_VarSpeed                             = 35
INTEGER, PARAMETER ::  IntGainTypeOf_Pump_ConSpeed                             = 36
INTEGER, PARAMETER ::  IntGainTypeOf_Pump_Cond                                 = 37
INTEGER, PARAMETER ::  IntGainTypeOf_PumpBank_VarSpeed                         = 38
INTEGER, PARAMETER ::  IntGainTypeOf_PumpBank_ConSpeed                         = 39
INTEGER, PARAMETER ::  IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam = 40
INTEGER, PARAMETER ::  IntGainTypeOf_PlantComponentUserDefined                 = 41
INTEGER, PARAMETER ::  IntGainTypeOf_CoilUserDefined                           = 42
INTEGER, PARAMETER ::  IntGainTypeOf_ZoneHVACForcedAirUserDefined              = 43
INTEGER, PARAMETER ::  IntGainTypeOf_AirTerminalUserDefined                    = 44
INTEGER, PARAMETER ::  IntGainTypeOf_PackagedTESCoilTank                       = 45

!Parameters for checking surface heat transfer models
REAL(r64), PARAMETER :: HighDiffusivityThreshold = 1.d-5 ! used to check if Material properties are out of line.
REAL(r64), PARAMETER :: ThinMaterialLayerThreshold = 0.003d0 ! 3 mm lower limit to expected material layers

          ! DERIVED TYPE DEFINITIONS:

TYPE MaterialProperties

  CHARACTER(len=MaxNameLength) :: Name = ' ' ! Name of material layer
  INTEGER :: Group       = -1  ! Material group type (see Material Parameters above.  Currently
                               ! active: RegularMaterial, Shade, Air, WindowGlass,
                               ! WindowGas, WindowBlind, WindowGasMixture, Screen, EcoRoof,
                               ! IRTMaterial, WindowSimpleGlazing, ComplexWindowShade, ComplexWindowGap)
  INTEGER :: Roughness   = 0   ! Surface roughness index (See Surface Roughness parameters
                               ! above.  Current: VerySmooth, Smooth, MediumSmooth,
                               ! MediumRough, Rough, VeryRough)

          ! Thermo-physical material properties
  REAL(r64) :: Conductivity   = 0.0d0 ! Thermal conductivity of layer (W/m2K)
  REAL(r64) :: Density        = 0.0d0 ! Layer density (kg/m3)
  REAL(r64) :: IsoMoistCap    = 0.0d0 ! Isothermal moisture capacity on water vapor density (m3/kg)
  REAL(r64) :: Porosity       = 0.0d0 ! Layer porosity
  REAL(r64) :: Resistance     = 0.0d0 ! Layer thermal resistance (alternative to Density,
                             ! Conductivity, Thickness, and Specific Heat; K/W)
  LOGICAL :: ROnly    =.false. ! Material defined with "R" only
  REAL(r64) :: SpecHeat       = 0.0d0 ! Layer specific heat (J/kgK)
  REAL(r64) :: ThermGradCoef  = 0.0d0 ! Thermal-gradient coefficient for moisture capacity
                               ! based on the water vapor density (kg/kgK)
  REAL(r64) :: Thickness      = 0.0d0 ! Layer thickness (m)
  REAL(r64) :: VaporDiffus    = 0.0d0 ! Layer vapor diffusivity
  INTEGER :: GasType(5)  = 0   ! Gas type (air=1, argon=2, krypton=3, xenon=4, custom=0) for
                             !  up to 5 gases in a mixture [Window gas only].  It is defined as parameter (GasCoefs)
  INTEGER :: GlassSpectralDataPtr = 0   ! Number of a spectral data set associated with a window glass material
  INTEGER :: NumberOfGasesInMixture = 0 ! Number of gases in a window gas mixture
  REAL(r64) :: GasCon(5,3)          = 0.0d0 ! Gas conductance coefficients for up to 5 gases in a mixture
  REAL(r64) :: GasVis(5,3)          = 0.0d0 ! Gas viscosity coefficients for up to 5 gases in a mixture
  REAL(r64) :: GasCp(5,3)            = 0.0d0 ! Gas specific-heat coefficients for up to 5 gases in a mixture
  REAL(r64) :: GasWght(5)            = 0.0d0 ! Gas molecular weight for up to 5 gases in a mixture
  REAL(r64) :: GasSpecHeatRatio(5)  = 0.0d0 ! Gas specific heat ratio (used for low pressure calculations)
  REAL(r64) :: GasFract(5)          = 0.0d0 ! Gas fractions for up to 5 gases in a mixture

          ! Radiation parameters
  REAL(r64) :: AbsorpSolar                = 0.d0 ! Layer solar absorptance
  REAL(r64) :: AbsorpSolarInput           = 0.d0 ! Layer solar absorptance input by user
  LOGICAL   :: AbsorpSolarEMSOverrideOn   = .FALSE. ! if true, then EMS calling to override value for solar absorptance
  REAL(r64) :: AbsorpSolarEMSOverride     = 0.d0 ! value to use when EMS calling to override value for solar absorptance
  REAL(r64) :: AbsorpThermal              = 0.d0 ! Layer thermal absorptance
  REAL(r64) :: AbsorpThermalInput         = 0.d0 ! Layer thermal absorptance input by user
  LOGICAL   :: AbsorpThermalEMSOverrideOn = .FALSE. ! if true, then EMS calling to override value for thermal absorptance
  REAL(r64) :: AbsorpThermalEMSOverride   = 0.d0 ! value to use when EMS calling to override value for thermal absorptance
  REAL(r64) :: AbsorpVisible              = 0.d0 ! Layer Visible Absorptance
  REAL(r64) :: AbsorpVisibleInput         = 0.d0 ! Layer Visible Absorptance input by user
  LOGICAL   :: AbsorpVisibleEMSOverrideOn = .FALSE. ! if true, then EMS calling to override value for visible absorptance
  REAL(r64) :: AbsorpVisibleEMSOverride   = 0.d0 ! value to use when EMS calling to override value for visible absorptance

          ! Window-related radiation parameters
  REAL(r64) :: Trans                = 0.0d0 ! Transmittance of layer (glass, shade)
  REAL(r64) :: TransVis             = 0.0d0 ! Visible transmittance (at normal incidence)
  REAL(r64) :: GlassTransDirtFactor = 1.0d0 ! Multiplier on glass transmittance due to dirt
  LOGICAL :: SolarDiffusing    =.false. ! True if glass diffuses beam solar radiation
  REAL(r64) :: ReflectShade         = 0.0d0 ! Shade or screen reflectance (interior shade only)
  REAL(r64) :: ReflectShadeVis      = 0.0d0 ! Shade reflectance for visible radiation
  REAL(r64) :: AbsorpThermalBack    = 0.0d0 ! Infrared radiation back absorption
  REAL(r64) :: AbsorpThermalFront   = 0.0d0 ! Infrared radiation front absorption
  REAL(r64) :: ReflectSolBeamBack   = 0.0d0 ! Solar back reflectance (beam to everything)
  REAL(r64) :: ReflectSolBeamFront  = 0.0d0 ! Solar front reflectance (beam to everything)
  REAL(r64) :: ReflectSolDiffBack   = 0.0d0 ! Solar back diffuse reflectance
  REAL(r64) :: ReflectSolDiffFront  = 0.0d0 ! Solar front diffuse reflectance
  REAL(r64) :: ReflectVisBeamBack   = 0.0d0 ! Visible back reflectance (beam to everything)
  REAL(r64) :: ReflectVisBeamFront  = 0.0d0 ! Visible front reflectance (beam to everything)
  REAL(r64) :: ReflectVisDiffBack   = 0.0d0 ! Visible back diffuse reflectance
  REAL(r64) :: ReflectVisDiffFront  = 0.0d0 ! Visible front diffuse reflectance
  CHARACTER(len=MaxNameLength) :: ReflectanceModeling = ' ' ! method used to account for screen scattering
  REAL(r64) :: TransSolBeam         = 0.0d0 ! Solar transmittance (beam to everything)
  REAL(r64) :: TransThermal         = 0.0d0 ! Infrared radiation transmittance
  REAL(r64) :: TransVisBeam         = 0.0d0 ! Visible transmittance (beam to everything)
  INTEGER :: BlindDataPtr      =0   ! Pointer to window blind data
  INTEGER :: ScreenDataPtr     =0   ! Pointer to window screen data
  INTEGER :: ScreenMapResolution=0  ! Resolution of azimuth and altitude angles to print in transmittance map

  ! Complex fenestration parameters
  REAL(r64) :: YoungModulus = 0.0d0      ! Young's modulus (Pa) - used in window deflection calculations
  REAL(r64) :: PoissonsRatio = 0.0d0    ! Poisson's ratio - used in window deflection calculations
  REAL(r64) :: DeflectedThickness = 0.d0 ! Minimum gap thickness in deflected state (m).  Used with measured deflection
  REAL(r64) :: Pressure = 0.d0            ! Window Gap pressure (Pa)
  INTEGER    :: SupportPillarPtr    = 0    ! Pointer to support pillar data
  INTEGER    :: DeflectionStatePtr = 0    ! Pointer to deflection state
  INTEGER    :: ComplexShadePtr    = 0    ! Pointer to complex shade data
  INTEGER    :: GasPointer = 0            ! Pointer to gas or gas mixture used in the gap

          ! Window-shade thermal model parameters
  REAL(r64) :: WinShadeToGlassDist      = 0.0d0   ! Distance between window shade and adjacent glass (m)
  REAL(r64) :: WinShadeTopOpeningMult   = 0.0d0   ! Area of air-flow opening at top of shade, expressed as a fraction
                                          !  of the shade-to-glass opening area at the top of the shade
  REAL(r64) :: WinShadeBottomOpeningMult= 0.0d0   ! Area of air-flow opening at bottom of shade, expressed as a fraction
                                          !  of the shade-to-glass opening area at the bottom of the shade
  REAL(r64) :: WinShadeLeftOpeningMult  = 0.0d0   ! Area of air-flow opening at left side of shade, expressed as a fraction
                                          !  of the shade-to-glass opening area at the left side of the shade
  REAL(r64) :: WinShadeRightOpeningMult = 0.0d0   ! Area of air-flow opening at right side of shade, expressed as a fraction
                                          !  of the shade-to-glass opening area at the right side of the shade
  REAL(r64) :: WinShadeAirFlowPermeability = 0.0d0 ! The effective area of openings in the shade itself, expressed as a
                                           !  fraction of the shade area

  LOGICAL :: EMPDMaterialProps = .false.    ! True if EMPD properties have been assigned
  REAL(r64) :: EMPDVALUE    = 0.0d0
  REAL(r64) :: MoistACoeff  = 0.0d0
  REAL(r64) :: MoistBCoeff  = 0.0d0
  REAL(r64) :: MoistCCoeff  = 0.0d0
  REAL(r64) :: MoistDCoeff  = 0.0d0

  REAL(r64) :: EMPDaCoeff   = 0.0d0
  REAL(r64) :: EMPDbCoeff   = 0.0d0
  REAL(r64) :: EMPDcCoeff   = 0.0d0
  REAL(r64) :: EMPDdCoeff   = 0.0d0

          ! EcoRoof-Related properties, essentially for the plant layer,
          !    the soil layer uses the same resource as a regular material
          !
  INTEGER :: EcoRoofCalculationMethod = 0  ! 1-Simple, 2-SchaapGenuchten
  REAL(r64) :: HeightOfPlants = 0.0d0       !plants' height
  REAL(r64) :: LAI = 0.0d0                 !LeafAreaIndex (Dimensionless???)
  REAL(r64) :: Lreflectivity = 0.0d0       !LeafReflectivity
  REAL(r64) :: LEmissitivity = 0.0d0       !LeafEmissivity
  REAL(r64) :: InitMoisture = 0.0d0            ! Initial soil moisture DJS
  REAL(r64) :: MinMoisture = 0.0d0    !Minimum moisture allowed DJS
  REAL(r64) :: RStomata = 0.0d0 ! Minimum stomatal resistance DJS

    ! HAMT
     INTEGER :: niso=-1                 ! Number of data points
     REAL(r64), DIMENSION(27) :: isodata=0.0d0     !isotherm values
     REAL(r64), DIMENSION(27) :: isorh  =0.0d0     !isotherm RH values
     INTEGER :: nsuc=-1                 ! Number of data points
     REAL(r64), DIMENSION(27) :: sucdata=0.0d0     !suction values
     REAL(r64), DIMENSION(27) :: sucwater=0.0d0    !suction water values
     INTEGER :: nred=-1                 ! Number of data points
     REAL(r64), DIMENSION(27) :: reddata =0.0d0    !redistribution values
     REAL(r64), DIMENSION(27) :: redwater=0.0d0    !redistribution water values
     INTEGER :: nmu=-1                  ! Number of data points
     REAL(r64), DIMENSION(27) :: mudata  =0.0d0    !mu values
     REAL(r64), DIMENSION(27) :: murh    =0.0d0    !mu rh values
     INTEGER :: ntc=-1                  ! Number of data points
     REAL(r64), DIMENSION(27) :: tcdata  =0.0d0    !thermal conductivity values
     REAL(r64), DIMENSION(27) :: tcwater =0.0d0    !thermal conductivity water values
     REAL(r64) :: itemp=10.0d0                 ! initial Temperature
     REAL(r64) :: irh=0.5d0                    ! Initial RH
     REAL(r64) :: iwater=0.2d0                 ! Initial water content kg/kg
     INTEGER :: divs=3                         ! Number of divisions
     REAL(r64) :: divsize=0.005d0              ! Average Cell Size
     INTEGER :: divmin=3                       ! Minimum number of cells
     INTEGER :: divmax=10                      ! Maximum number of cells

     ! Added 12/22/2008 for thermochromic window glazing material
     REAL(r64) :: SpecTemp = 0.0D0             ! Temperature corresponding to the specified material properties
     INTEGER   :: TCParent = 0                 ! Reference to the parent object WindowMaterial:Glazing:Thermochromic

     ! Simple Glazing System
     REAL(r64) :: SimpleWindowUfactor = 0.0D0  ! user input for simple window U-factor with film coefs (W/m2-k)
     REAL(r64) :: SimpleWindowSHGC    = 0.0D0  ! user input for simple window Solar Heat Gain Coefficient (non-dimensional)
     REAL(r64) :: SimpleWindowVisTran = 0.0D0  ! (optional) user input for simple window Visual Transmittance (non-dimensional)
     LOGICAL   :: SimpleWindowVTinputByUser = .FALSE. ! false means not input, true means user provide VT input

     LOGICAL   :: WarnedForHighDiffusivity  = .FALSE. ! used to limit error messaging to just the first instance

     ! Equivalent Layer (ASHWAT) Model
     REAL(r64) :: ReflFrontBeamBeam         = 0.0d0 ! Beam-Beam solar reflectance front at zero incident
     REAL(r64) :: ReflBackBeamBeam          = 0.0d0 ! Beam-Beam solar reflectance back at zero incident
     REAL(r64) :: TausFrontBeamBeam         = 0.0d0 ! Beam-Beam solar transmittance front at zero incident
     REAL(r64) :: TausBackBeamBeam          = 0.0d0 ! Beam-Beam solar transmittance back at zero incident
     REAL(r64) :: ReflFrontBeamBeamVis      = 0.0d0 ! Beam-Beam visible reflectance front at zero incident
     REAL(r64) :: ReflBackBeamBeamVis       = 0.0d0 ! Beam-Beam visible reflectance back at zero incident
     REAL(r64) :: TausFrontBeamBeamVis      = 0.0d0 ! Beam-Beam visible transmittance front at zero incident
     REAL(r64) :: TausBackBeamBeamVis       = 0.0d0 ! Beam-Beam visible transmittance back at zero incident
     REAL(r64) :: ReflFrontBeamDiff         = 0.0d0 ! Beam-Diffuse solar reflectance front at zero incident
     REAL(r64) :: ReflBackBeamDiff          = 0.0d0 ! Beam-Diffuse solar reflectance back at zero incident
     REAL(r64) :: TausFrontBeamDiff         = 0.0d0 ! Beam-Diffuse solar transmittance front at zero incident
     REAL(r64) :: TausBackBeamDiff          = 0.0d0 ! Beam-Diffuse solar transmittance back at zero incident
     REAL(r64) :: ReflFrontBeamDiffVis      = 0.0d0 ! Beam-Diffuse visible reflectance front at zero incident
     REAL(r64) :: ReflBackBeamDiffVis       = 0.0d0 ! Beam-Diffuse visible reflectance back at zero incident
     REAL(r64) :: TausFrontBeamDiffVis      = 0.0d0 ! Beam-Diffuse visible transmittance front at zero incident
     REAL(r64) :: TausBackBeamDiffVis       = 0.0d0 ! Beam-Diffuse visible transmittance back at zero incident
     REAL(r64) :: ReflFrontDiffDiff         = 0.0d0 ! Diffuse-Diffuse solar reflectance front
     REAL(r64) :: ReflBackDiffDiff          = 0.0d0 ! Diffuse-Diffuse solar reflectance back
     REAL(r64) :: TausDiffDiff              = 0.0d0 ! Diffuse-Diffuse solar transmittance (front and back)
     REAL(r64) :: ReflFrontDiffDiffVis      = 0.0d0 ! Diffuse-Diffuse visible reflectance front
     REAL(r64) :: ReflBackDiffDiffVis       = 0.0d0 ! Diffuse-Diffuse visible reflectance back
     REAL(r64) :: TausDiffDiffVis           = 0.0d0 ! Diffuse-Diffuse visible transmittance (front and back)
     REAL(r64) :: EmissThermalFront         = 0.0d0 ! Front side thermal or infrared Emissivity
     REAL(r64) :: EmissThermalBack          = 0.0d0 ! Back side thermal or infrared Emissivity
     REAL(r64) :: TausThermal               = 0.0d0 ! Thermal transmittance (front and back)
     INTEGER   :: GapVentType               =0   ! Gap Ven type for equivalent Layer window model
     LOGICAL   :: ISPleatedDrape            =.false. ! if pleated drape= true, if nonpleated drape = false
     REAL(r64) :: PleatedDrapeWidth         = 0.0d0 ! width of the pleated drape fabric section
     REAL(r64) :: PleatedDrapeLength        = 0.0d0 ! length of the pleated drape fabric section
     REAL(r64) :: ScreenWireSpacing         = 0.0d0 ! insect screen wire spacing
     REAL(r64) :: ScreenWireDiameter        = 0.0d0 ! insect screen wire diameter

     REAL(r64) :: SlatWidth                 = 0.0d0 ! slat width
     REAL(r64) :: SlatSeparation            = 0.0d0 ! slat seperation
     REAL(r64) :: SlatCrown                 = 0.0d0 ! slat crown
     REAL(r64) :: SlatAngle                 = 0.0d0 ! slat angle
     INTEGER   :: SlatAngleType             =0   ! slat angle control type, 0=fixed, 1=maximize solar, 2=block beam
     INTEGER   :: SlatOrientation           =0   ! horizontal or veritical
     CHARACTER(len=MaxNameLength):: GasName =' ' ! Name of gas type ("Air", "Argon", "Krypton", "Xenon")

END TYPE MaterialProperties

! thermochromic windows
TYPE TCGlazingsType
    CHARACTER(len=MaxNameLength) :: Name = ' '                  ! Name
    INTEGER                      :: NumGlzMat = 0               ! Number of TC glazing materials
    INTEGER, ALLOCATABLE, DIMENSION(:)   :: LayerPoint          ! Layer pointer
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: SpecTemp            ! Temperature corresponding to the specified TC glaing optical data
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: LayerName  ! Name of the referenced WindowMaterial:Glazing object
END TYPE

TYPE ConstructionData

  CHARACTER(len=MaxNameLength) :: Name = ' ' ! Name of construction
  INTEGER :: TotLayers          = 0          ! Total number of layers for the construction; for windows
                                             !  this is the total of the glass, gas and shade layers
  INTEGER :: TotSolidLayers     = 0          ! Total number of solid (glass or shade) layers (windows only)
  INTEGER :: TotGlassLayers     = 0          ! Total number of glass layers (windows only)
  INTEGER :: LayerPoint(MaxLayersInConstruct) = 0 ! Pointer array which refers back to
                                                  ! the Material structure; LayerPoint(i)=j->Material(j)%Name,etc
  LOGICAL :: IsUsed                  =.false.  ! Marked true when the construction is used
  REAL(r64) :: InsideAbsorpVis       = 0.0d0         ! Inside Layer visible absorptance of an opaque surface; not used for windows.
  REAL(r64) :: OutsideAbsorpVis      = 0.0d0         ! Outside Layer visible absorptance of an opaque surface; not used for windows.
  REAL(r64) :: InsideAbsorpSolar     = 0.0d0         ! Inside Layer solar absorptance of an opaque surface; not used for windows.
  REAL(r64) :: OutsideAbsorpSolar    = 0.0d0         ! Outside Layer solar absorptance of an opaque surface; not used for windows.
  REAL(r64) :: InsideAbsorpThermal   = 0.0d0         ! Inside Layer Thermal absorptance for opaque surfaces or windows;
                                             ! for windows, applies to innermost glass layer
  REAL(r64) :: OutsideAbsorpThermal  = 0.0d0         ! Outside Layer Thermal absorptance
  INTEGER :: OutsideRoughness   =0           ! Outside Surface roughness index (6=very smooth, 5=smooth,
                                             ! 4=medium smooth, 3=medium rough, 2=rough, 1=very rough)
  INTEGER :: DayltPropPtr       =0           ! Pointer to Daylight Construction Properties
  INTEGER :: W5FrameDivider     = 0          ! FrameDivider number for window construction from Window5 data file;
                                             !  zero is construction not from Window5 file or Window5 construction has no frame.

          ! Conductive properties for the construction
  REAL(r64) :: CTFCross(0:MaxCTFTerms-1)    =0.0D0     ! Cross or Y terms of the CTF equation
  REAL(r64) :: CTFFlux(MaxCTFTerms-1)       =0.0D0     ! Flux history terms of the CTF equation
  REAL(r64) :: CTFInside(0:MaxCTFTerms-1)   =0.0D0     ! Inside or Z terms of the CTF equation
  REAL(r64) :: CTFOutside(0:MaxCTFTerms-1)  =0.0D0     ! Outside or X terms of the CTF equation
  REAL(r64) :: CTFSourceIn(0:MaxCTFTerms-1) =0.0D0     ! Heat source/sink inside terms of CTF equation
  REAL(r64) :: CTFSourceOut(0:MaxCTFTerms-1)=0.0D0     ! Heat source/sink outside terms of CTF equation
  REAL(r64) :: CTFTimeStep  ! Time increment for stable simulation of construct (could be greater than TimeStep)
                   ! The next three series of terms are used to calculate the temperature at the location of a source/sink
                   ! in the QTF formulation.  This calculation is necessary to allow the proper simulation of a
                   ! radiant system.
  REAL(r64) :: CTFTSourceOut(0:MaxCTFTerms-1)=0.0D0  ! Outside terms of the CTF equation for interior temp
                                                            ! calc@source location
  REAL(r64) :: CTFTSourceIn(0:MaxCTFTerms-1) =0.0D0  ! Inside terms of the CTF equation for interior temp
                                                            ! calc@source location
  REAL(r64) :: CTFTSourceQ(0:MaxCTFTerms-1)  =0.0D0  ! Source/sink terms of the CTF equation for interior temp
                                                            ! calc@source location
                   ! The next three series of terms are used to calculate the temperature at a location specified by the user.
                   ! This location must be between two layers and is intended to allow the user to evaluate whether or not
                   ! condensation is a possibility between material layers.
  REAL(r64) :: CTFTUserOut(0:MaxCTFTerms-1)  =0.0D0  ! Outside terms of the CTF equation for interior temp
                                                            ! calc@user location
  REAL(r64) :: CTFTUserIn(0:MaxCTFTerms-1)   =0.0D0  ! Inside terms of the CTF equation for interior temp
                                                            ! calc@user location
  REAL(r64) :: CTFTUserSource(0:MaxCTFTerms-1)=0.0D0 ! Source/sink terms of the CTF equation for interior temp
                                                            ! calc@user location
  INTEGER          :: NumHistories =0            ! CTFTimeStep/TimeStepZone or the number of temp/flux history series
                                                 ! for the construction
  INTEGER          :: NumCTFTerms  =0            ! Number of CTF terms for this construction (not including terms at current time)
  REAL(r64)        :: UValue       = 0.0d0          ! Overall heat transfer coefficient for the construction
  INTEGER          :: SolutionDimensions =0      ! Number of dimensions in the solution (1 for normal constructions,
                                                 ! 1 or 2 for constructions with sources or sinks)-->may allow 3-D later?
  INTEGER          :: SourceAfterLayer   =0      ! Source/sink is present after this layer in the construction
  INTEGER          :: TempAfterLayer     =0      ! User is requesting a temperature calculation after this layer in the construction
  REAL(r64)        :: ThicknessPerpend   = 0.0d0    ! Thickness between planes of symmetry in the direction
                                                 ! perpendicular to the main direction of heat transfer
                                                 ! (same as half the distance between tubes)
  ! Moisture Transfer Functions term belong here as well

          ! BLAST detailed solar model parameters
  REAL(r64) :: AbsDiffIn  = 0.0d0           ! Inner absorptance coefficient for diffuse radiation
  REAL(r64) :: AbsDiffOut = 0.0d0           ! Outer absorptance coefficient for diffuse radiation

          ! Variables for window constructions
  REAL(r64) :: AbsDiff(MaxSolidWinLayers)  = 0.0d0                 ! Diffuse solar absorptance for each glass layer,
                                                                ! bare glass or shade on
  REAL(r64) :: BlAbsDiff(MaxSolidWinLayers,MaxSlatAngs) = 0.0d0    ! Diffuse solar absorptance for each glass layer vs.
                                                                ! slat angle, blind on
  REAL(r64) :: BlAbsDiffGnd(MaxSolidWinLayers,MaxSlatAngs) = 0.0d0 ! Diffuse ground solar absorptance for each glass layer
                                                                ! vs. slat angle, blind on
  REAL(r64) :: BlAbsDiffSky(MaxSolidWinLayers,MaxSlatAngs) = 0.0d0 ! Diffuse sky solar absorptance for each glass layer
                                                                ! vs. slat angle, blind on
  REAL(r64) :: AbsDiffBack(MaxSolidWinLayers)  = 0.0d0             ! Diffuse back solar absorptance for each glass layer
  REAL(r64) :: BlAbsDiffBack(MaxSolidWinLayers,MaxSlatAngs)= 0.0d0 ! Diffuse back solar absorptance for each glass layer,
                                                           !  vs. slat angle, blind on
  REAL(r64) :: AbsDiffShade                 = 0.0d0 ! Diffuse solar absorptance for shade
  REAL(r64) :: AbsDiffBlind(MaxSlatAngs)    = 0.0d0 ! Diffuse solar absorptance for blind, vs. slat angle
  REAL(r64) :: AbsDiffBlindGnd(MaxSlatAngs) = 0.0d0 ! Diffuse ground solar absorptance for blind, vs. slat angle
  REAL(r64) :: AbsDiffBlindSky(MaxSlatAngs) = 0.0d0 ! Diffuse sky solar absorptance for blind, vs. slat angle
  REAL(r64) :: AbsDiffBackShade             = 0.0d0 ! Diffuse back solar absorptance for shade
  REAL(r64) :: AbsDiffBackBlind(MaxSlatAngs)= 0.0d0 ! Diffuse back solar absorptance for blind, vs. slat angle
  REAL(r64) :: ShadeAbsorpThermal           = 0.0d0 ! Diffuse back thermal absorptance of shade
  REAL(r64) :: AbsBeamCoef(MaxSolidWinLayers,6) = 0.0d0 ! Coefficients of incidence-angle polynomial for solar
                                                ! absorptance for each solid glazing layer

  REAL(r64) :: AbsBeamBackCoef(MaxSolidWinLayers,6) = 0.0d0 ! As for AbsBeamCoef but for back-incident solar
  REAL(r64) :: AbsBeamShadeCoef(6)           = 0.0d0    ! Coefficients of incidence-angle polynomial for solar
                                        ! absorptance of shade
  REAL(r64) :: TransDiff                     = 0.0d0   ! Diffuse solar transmittance, bare glass or shade on
  REAL(r64) :: BlTransDiff(MaxSlatAngs)      = 0.0d0   ! Diffuse solar transmittance, blind present, vs. slat angle
  REAL(r64) :: BlTransDiffGnd(MaxSlatAngs)   = 0.0d0   ! Ground diffuse solar transmittance, blind present, vs. slat angle
  REAL(r64) :: BlTransDiffSky(MaxSlatAngs)   = 0.0d0   ! Sky diffuse solar transmittance, blind present, vs. slat angle
  REAL(r64) :: TransDiffVis                  = 0.0d0   ! Diffuse visible transmittance, bare glass or shade on
  REAL(r64) :: BlTransDiffVis(MaxSlatAngs)   = 0.0d0   ! Diffuse visible transmittance, blind present, vs. slat angle
  REAL(r64) :: ReflectSolDiffBack            = 0.0d0   ! Diffuse back solar reflectance, bare glass or shade on
  REAL(r64) :: BlReflectSolDiffBack(MaxSlatAngs) = 0.0d0 ! Diffuse back solar reflectance, blind present, vs. slat angle
  REAL(r64) :: ReflectSolDiffFront               = 0.0d0 ! Diffuse front solar reflectance, bare glass or shade on
  REAL(r64) :: BlReflectSolDiffFront(MaxSlatAngs)= 0.0d0 ! Diffuse front solar reflectance, blind present, vs. slat angle
  REAL(r64) :: ReflectVisDiffBack                = 0.0d0 ! Diffuse back visible reflectance, bare glass or shade on
  REAL(r64) :: BlReflectVisDiffBack(MaxSlatAngs) = 0.0d0 ! Diffuse back visible reflectance, blind present, vs. slat angle
  REAL(r64) :: ReflectVisDiffFront               = 0.0d0 ! Diffuse front visible reflectance, bare glass or shade on
  REAL(r64) :: BlReflectVisDiffFront(MaxSlatAngs)= 0.0d0 ! Diffuse front visible reflectance, blind present, vs. slat angle
  REAL(r64) :: TransSolBeamCoef(6)               = 0.0d0 ! Coeffs of incidence-angle polynomial for beam sol trans,
                                                      ! bare glass or shade on
  REAL(r64) :: TransVisBeamCoef(6)               = 0.0d0 ! Coeffs of incidence-angle polynomial for beam vis trans,
                                                      ! bare glass or shade on
  REAL(r64) :: ReflSolBeamFrontCoef(6)           = 0.0d0 ! Coeffs of incidence-angle polynomial for beam sol front refl,
                                                      ! bare glass or shade on
  REAL(r64) :: ReflSolBeamBackCoef(6) = 0.0d0 ! Like ReflSolBeamFrontCoef, but for back-incident beam solar
  REAL(r64) :: tBareSolCoef(5,6)      = 0.0d0 ! Isolated glass solar transmittance coeffs of inc. angle polynomial
  REAL(r64) :: tBareVisCoef(5,6)      = 0.0d0 ! Isolated glass visible transmittance coeffs of inc. angle polynomial
  REAL(r64) :: rfBareSolCoef(5,6)     = 0.0d0 ! Isolated glass front solar reflectance coeffs of inc. angle polynomial
  REAL(r64) :: rfBareVisCoef(5,6)     = 0.0d0 ! Isolated glass front visible reflectance coeffs of inc. angle polynomial
  REAL(r64) :: rbBareSolCoef(5,6)     = 0.0d0 ! Isolated glass back solar reflectance coeffs of inc. angle polynomial
  REAL(r64) :: rbBareVisCoef(5,6)     = 0.0d0 ! Isolated glass back visible reflectance coeffs of inc. angle polynomial
  REAL(r64) :: afBareSolCoef(5,6)     = 0.0d0 ! Isolated glass front solar absorptance coeffs of inc. angle polynomial
  REAL(r64) :: abBareSolCoef(5,6)     = 0.0d0 ! Isolated glass back solar absorptance coeffs of inc. angle polynomial
  REAL(r64) :: tBareSolDiff(5)        = 0.0d0 ! Isolated glass diffuse solar transmittance
  REAL(r64) :: tBareVisDiff(5)        = 0.0d0 ! Isolated glass diffuse visible transmittance
  REAL(r64) :: rfBareSolDiff(5)       = 0.0d0 ! Isolated glass diffuse solar front reflectance
  REAL(r64) :: rfBareVisDiff(5)       = 0.0d0 ! Isolated glass diffuse visible front reflectance
  REAL(r64) :: rbBareSolDiff(5)       = 0.0d0 ! Isolated glass diffuse solar back reflectance
  REAL(r64) :: rbBareVisDiff(5)       = 0.0d0 ! Isolated glass diffuse visible back reflectance
  REAL(r64) :: afBareSolDiff(5)       = 0.0d0 ! Isolated glass diffuse solar front absorptance
  REAL(r64) :: abBareSolDiff(5)       = 0.0d0 ! Isolated glass diffuse solar back absorptance
  LOGICAL :: FromWindow5DataFile =.false. ! True if this is a window construction extracted from the Window5 data file
  REAL(r64) :: W5FileMullionWidth     = 0.0d0    ! Width of mullion for construction from Window5 data file (m)
  INTEGER   :: W5FileMullionOrientation = 0 ! Orientation of mullion, if present, for Window5 data file construction,
  REAL(r64) :: W5FileGlazingSysWidth     = 0.0d0 ! Glass width for construction from Window5 data file (m)
  REAL(r64) :: W5FileGlazingSysHeight    = 0.0d0 ! Glass height for construction form Window5 data file (m)
  REAL(r64) :: SummerSHGC                = 0.0d0 ! Calculated ASHRAE SHGC for summer conditions
  REAL(r64) :: VisTransNorm              = 0.0d0 ! The normal visible transmittance
  REAL(r64) :: SolTransNorm              =0.0D0 ! the normal solar transmittance

  LOGICAL :: SourceSinkPresent  =.false.  ! .TRUE. if there is a source/sink within this construction
  LOGICAL :: TypeIsWindow       =.false.  ! True if a window construction, false otherwise
  LOGICAL :: WindowTypeBSDF      =.false.  ! True for complex window, false otherwise
  LOGICAL :: TypeIsEcoRoof      =.false.  !-- true for construction with ecoRoof outside, the flag
                                          !-- is turned on when the outside layer is of type EcoRoof
  LOGICAL :: TypeIsIRT          =.false.  !-- true for construction with IRT material
  LOGICAL :: TypeIsCfactorWall  =.false.  !-- true for construction with Construction:CfactorUndergroundWall
  LOGICAL :: TypeIsFfactorFloor =.false.  !-- true for construction with Construction:FfactorGroundFloor

  ! Added TH 12/22/2008 for thermochromic windows
  INTEGER   :: TCFlag = 0                   ! 0: this construction is not a thermochromic window construction
                                            ! 1: it is a TC window construction
  INTEGER   :: TCLayer = 0                  ! Reference to the TC glazing material layer in the Material array
  INTEGER   :: TCMasterConst = 0            ! The master TC construction referenced by its slave constructions
  INTEGER   :: TCLayerID = 0                ! Which material layer is the TC glazing, counting all material layers.
  INTEGER   :: TCGlassID = 0                ! Which glass layer is the TC glazing, counting from glass layers only.

  !For CFactor underground walls
  REAL(r64) :: CFactor = 0.0d0
  REAL(r64) :: Height = 0.0d0

  !For FFactor slabs-on-grade or undeerground floors
  REAL(r64) :: FFactor = 0.0d0
  REAL(r64) :: Area = 0.0d0
  REAL(r64) :: PerimeterExposed = 0.0d0

  LOGICAL   :: ReverseConstructionNumLayersWarning = .false.
  LOGICAL   :: ReverseConstructionLayersOrderWarning = .false.

  !Complex Fenestration
  TYPE(BSDFWindowInputStruct) :: BSDFInput ! nest structure with user input for complex fenestration

  ! EquivalentLayer Window
  LOGICAL   :: WindowTypeEQL              =.false. ! True for equivalent layer window, false otherwise
  INTEGER   :: EQLConsPtr                 =0   ! Pointer to equivalent Layer window construction
  REAL(r64) :: AbsDiffFrontEQL(CFSMAXNL)  = 0.0d0 ! Diffuse layer system front absorptance for EQL window
  REAL(r64) :: AbsDiffBackEQL(CFSMAXNL)   = 0.0d0 ! Diffuse layer system back absorptance for EQL window
  REAL(r64) :: TransDiffFrontEQL          = 0.0d0 ! Diffuse system front transmittance for EQL window
  REAL(r64) :: TransDiffBackEQL           = 0.0d0 ! Diffuse system back transmittance for EQL window

END TYPE ConstructionData

TYPE SpectralDataProperties
  CHARACTER(len=MaxNameLength) :: Name  =' ' ! Name of spectral data set
  INTEGER :: NumOfWavelengths           = 0  ! Number of wavelengths in the data set
  REAL(r64),ALLOCATABLE,DIMENSION(:) :: WaveLength     ! Wavelength (microns)
  REAL(r64),ALLOCATABLE,DIMENSION(:) :: Trans           ! Transmittance at normal incidence
  REAL(r64),ALLOCATABLE,DIMENSION(:) :: ReflFront       ! Front reflectance at normal incidence
  REAL(r64),ALLOCATABLE,DIMENSION(:) :: ReflBack        ! Back reflectance at normal incidence
END TYPE SpectralDataProperties

TYPE ZoneData
    CHARACTER(len=MaxNameLength) :: Name=' '
    INTEGER :: Multiplier               = 1  ! Used in reporting and for systems calculations
    INTEGER :: ListMultiplier           = 1  ! For Zone Group object:  used in reporting and systems calculations
    INTEGER :: ListGroup                = 0  ! used only in Zone Group verification.  and for error message.
    REAL(r64) :: RelNorth               = 0.0d0 ! Relative North (to building north) [Degrees]
    REAL(r64) :: OriginX                = 0.0d0 ! X origin  [m]
    REAL(r64) :: OriginY                = 0.0d0 ! Y origin  [m]
    REAL(r64) :: OriginZ                = 0.0d0 ! Z origin  [m]
    REAL(r64) :: CeilingHeight          =AutoCalculate ! Ceiling Height entered by user [m] or calculated
    REAL(r64) :: Volume                 =AutoCalculate ! Volume entered by user [m3] or calculated
    INTEGER :: OfType                   = 1  ! 1=Standard Zone, Not yet used:
                                             ! 2=Plenum Zone, 11=Solar Wall, 12=Roof Pond
    REAL(r64) :: UserEnteredFloorArea   =AutoCalculate ! User input floor area for this zone
        ! Calculated after input
    REAL(r64) :: FloorArea              = 0.0d0 ! Floor area used for this zone
    REAL(r64) :: CalcFloorArea          = 0.0d0 ! Calculated floor area used for this zone
    LOGICAL :: HasFloor             =.false. ! Has "Floor" surface
    LOGICAL :: HasRoof              =.false. ! Has "Roof" or "Ceiling" Surface
    LOGICAL :: HasInterZoneWindow   =.false. ! Interzone Window(s) present in this zone
    LOGICAL :: HasWindow            =.false. ! Window(s) present in this zone
    REAL(r64) :: AirCapacity            =0.0d0
    REAL(r64) :: ExtWindowArea          = 0.0d0 ! Exterior Window Area for Zone
    REAL(r64) :: ExtGrossWallArea       = 0.0d0 ! Exterior Wall Area for Zone (Gross)
    REAL(r64) :: ExtWindowArea_Multiplied= 0.0d0 ! Exterior Window Area for Zone with multipliers
    REAL(r64) :: ExtGrossWallArea_Multiplied= 0.0d0 ! Exterior Wall Area for Zone (Gross) with multipliers
    REAL(r64) :: ExtNetWallArea         = 0.0d0 ! Exterior Wall Area for Zone (Net)
    REAL(r64) :: TotalSurfArea          = 0.0d0 ! Total surface area for Zone
    REAL(r64) :: ExteriorTotalSurfArea  = 0.0d0 ! Total surface area of all exterior surfaces for Zone
                                             ! (ignoring windows as they will be included in their base surfaces)
    REAL(r64) :: ExteriorTotalGroundSurfArea  = 0.0d0 ! Total surface area of all surfaces for Zone with ground contact
    REAL(r64) :: ExtGrossGroundWallArea       = 0.0d0 ! Ground contact Wall Area for Zone (Gross)
    REAL(r64) :: ExtGrossGroundWallArea_Multiplied= 0.0d0 ! Ground contact Wall Area for Zone (Gross) with multipliers
    INTEGER :: SystemZoneNodeNumber     = 0  ! This is the zone node number for the system for a controlled zone
    LOGICAL :: IsControlled             = .false.  ! True when this is a controlled zone.
    INTEGER :: TempControlledZoneIndex  = 0  ! this is the index number for TempControlledZone structure for lookup
    !            Pointers to Surface Data Structure
    INTEGER :: SurfaceFirst             = 0  ! First Surface in Zone
    INTEGER :: SurfaceLast              = 0  ! Last Surface in Zone
    INTEGER :: InsideConvectionAlgo     = ASHRAESimple  ! Ref: appropriate values for Inside Convection solution
    INTEGER :: NumSurfaces              = 0  ! Number of surfaces for this zone
    INTEGER :: NumSubSurfaces           = 0  ! Number of subsurfaces for this zone (windows, doors, tdd dome and diffusers)
    INTEGER :: NumShadingSurfaces       = 0  ! Number of shading surfaces for this zone
    INTEGER :: OutsideConvectionAlgo    = ASHRAESimple  ! Ref: appropriate values for Outside Convection solution
    TYPE (Vector) :: Centroid = Vector(0.0d0, 0.0d0, 0.0d0)  ! Center of the zone found by averaging wall, floor, and roof centroids
    REAL(r64) :: MinimumX               = 0.0d0 ! Minimum X value for entire zone
    REAL(r64) :: MaximumX               = 0.0d0 ! Maximum X value for entire zone
    REAL(r64) :: MinimumY               = 0.0d0 ! Minimum Y value for entire zone
    REAL(r64) :: MaximumY               = 0.0d0 ! Maximum Y value for entire zone
    REAL(r64) :: MinimumZ               = 0.0d0 ! Minimum Z value for entire zone
    REAL(r64) :: MaximumZ               = 0.0d0 ! Maximum Z value for entire zone
    REAL(r64) :: OutDryBulbTemp         = 0.0d0 ! Zone outside dry bulb air temperature (C)
    REAL(r64) :: OutWetBulbTemp         = 0.0d0 ! Zone outside wet bulb air temperature (C)
    REAL(r64) :: WindSpeed              = 0.0d0 ! Zone outside wind speed (m/s)
    LOGICAL :: isPartOfTotalArea        = .TRUE. ! Count the zone area when determining the building total floor area
    LOGICAL :: isNominalOccupied        = .false. !has occupancy nominally specified
    LOGICAL :: isNominalControlled      = .false. !has Controlled Zone Equip Configuration reference
    REAL(r64) :: TotOccupants           = 0.0d0 ! total design occupancy
                                                  ! (sum of NumberOfPeople for the zone from People object)
    INTEGER :: AirHBimBalanceErrIndex   = 0 ! error management counter
    LOGICAL :: NoHeatToReturnAir        =.FALSE.  ! TRUE means that heat to return air should be added to the zone load
    LOGICAL :: RefrigCaseRA             =.FALSE.  ! TRUE means there is potentially heat removal from return air
                                                  ! from refrigeration cases for this zone
    REAL(r64) :: InternalHeatGains      = 0.0d0      ! internal loads (W)
    REAL(r64) :: NominalInfilVent       = 0.0d0      ! internal infiltration/ventilaton
    REAL(r64) :: NominalMixing          = 0.0d0      ! internal mixing/cross mixing
    LOGICAL   :: TempOutOfBoundsReported=.false.  ! if any temp out of bounds errors, first will show zone details.
    LOGICAL   :: EnforcedReciprocity=.false.      ! if zone required forced reciprocity --
                                                  !   less out of bounds temperature errors allowed
    INTEGER   :: ZoneMinCO2SchedIndex   =0        ! Index for the schedule the schedule which determines minimum CO2 concentration
    INTEGER   :: ZoneContamControllerSchedIndex   =0   ! Index for this schedule
END TYPE ZoneData

TYPE ZoneListData
  CHARACTER(len=MaxNameLength)   :: Name = ''      ! Zone List name
  INTEGER                        :: NumOfZones = 0 ! Number of zones in the list
  INTEGER                        :: MaxZoneNameLength = 0  ! Max Name length of zones in the list
  INTEGER, ALLOCATABLE, DIMENSION(:) :: Zone           ! Pointers to zones in the list
END TYPE ZoneListData

TYPE ZoneGroupData
  CHARACTER(len=MaxNameLength) :: Name = ''      ! Zone Group name
  INTEGER                      :: ZoneList = 0   ! Pointer to the zone list
  INTEGER                      :: Multiplier = 1 ! Zone List multiplier
END TYPE ZoneGroupData

TYPE GlobalInternalGainMiscObject
  CHARACTER(len=MaxNameLength) :: Name  =' '
  INTEGER :: ZoneOrZoneListPtr          =0
  INTEGER :: NumOfZones                 =0
  INTEGER :: StartPtr                   =0
  LOGICAL :: ZoneListActive             =.false.
END TYPE GlobalInternalGainMiscObject

TYPE PeopleData
    CHARACTER(len=MaxNameLength) :: Name               =' ' ! PEOPLE object name
    INTEGER   :: ZonePtr                               = 0  ! Pointer to the zone number for this people statement
    REAL(r64) :: NumberOfPeople                        = 0.0d0 ! Maximum number of people for this statement
    INTEGER   :: NumberOfPeoplePtr                     = -1  ! Pointer to schedule for number of people
    LOGICAL   :: EMSPeopleOn                           = .FALSE. !EMS actuating number of people if .TRUE.
    REAL(r64) :: EMSNumberOfPeople                     = 0.0D0 ! Value EMS is directing to use for override

    ! Note that the schedule and maximum number was kept for people since it seemed likely that
    ! users would want to assign the same schedule to multiple people statements.
    INTEGER :: ActivityLevelPtr                        =-1  ! Pointer to schedule for activity level
    REAL(r64) :: FractionRadiant                            = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain from people
                                                            ! that is radiant
    REAL(r64) :: FractionConvected                          = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain from people
                                                            ! that is convective
    REAL(r64) :: NomMinNumberPeople           =0.d0 ! Nominal Minimum Number of People (min sch X number of people)
    REAL(r64) :: NomMaxNumberPeople           =0.d0 ! Nominal Maximum Number of People (min sch X number of people)

    INTEGER :: WorkEffPtr                              =-1  ! Pointer to schedule for work efficiency
    INTEGER :: ClothingPtr                             =-1  ! Pointer to schedule for clothing insulation
    INTEGER :: ClothingMethodPtr                       =-1
    INTEGER :: ClothingType                            =-1  ! Name of clothing type
    INTEGER :: AirVelocityPtr                          =-1  ! Pointer to schedule for air velocity in zone
    LOGICAL :: Fanger                            =.false.   ! True when Fanger calculation to be performed
    LOGICAL :: Pierce                            =.false.   ! True when Pierce 2-node calculation to be performed
    LOGICAL :: KSU                               =.false.   ! True when KSU 2-node calculation to be performed
    LOGICAL :: AdaptiveASH55                     =.false.   ! True when ASHRAE Standard 55 adaptive comfort calculation
                                                            !   to be performed
    LOGICAL :: AdaptiveCEN15251                  =.false.   ! True when CEN Standard 15251 adaptive comfort calculation
                                                            !   to be performed
    INTEGER :: MRTCalcType                             = 0  ! MRT calculation type (See MRT Calculation type parameters)
    INTEGER :: SurfacePtr                              =-1  ! Pointer to the name of surface
    CHARACTER(len=MaxNameLength) :: AngleFactorListName=' ' ! Name of angle factor list
    INTEGER :: AngleFactorListPtr                      =-1  ! Pointer to the name of angle factor list
    REAL(r64) :: UserSpecSensFrac                        = 0.0d0 ! User specified sensible fraction
    LOGICAL :: Show55Warning                    = .false.   ! show the warning messages about ASHRAE 55-2004
    REAL(r64) :: CO2RateFactor                              = 0.0d0 ! Carbon Dioxide Generation Rate [m3/s-W]

    ! Report variables
    REAL(r64) :: NumOcc                                     = 0.0d0  ! Number of occupants []
    REAL(r64) :: TemperatureInZone                          = 0.0d0  ! Temperature in zone (C)
    REAL(r64) :: RelativeHumidityInZone                     = 0.0d0  ! Relative humidity in zone

    REAL(r64) :: RadGainRate                                = 0.0d0 ! Radiant heat gain [W]
    REAL(r64) :: ConGainRate                                = 0.0d0 ! Convective heat gain [W]
    REAL(r64) :: SenGainRate                                = 0.0d0 ! Sensible heat gain [W]
    REAL(r64) :: LatGainRate                                = 0.0d0 ! Latent heat gain [W]
    REAL(r64) :: TotGainRate                                = 0.0d0 ! Total heat gain [W]
    REAL(r64) :: CO2GainRate                                = 0.0d0 ! Carbon Dioxide Gain Rate [m3/s]

    REAL(r64) :: RadGainEnergy                              = 0.0d0 ! Radiant heat gain [J]
    REAL(r64) :: ConGainEnergy                              = 0.0d0 ! Convective heat gain [J]
    REAL(r64) :: SenGainEnergy                              = 0.0d0 ! Sensible heat gain [J]
    REAL(r64) :: LatGainEnergy                              = 0.0d0 ! Latent heat gain [J]
    REAL(r64) :: TotGainEnergy                              = 0.0d0 ! Total heat gain [J]
    ! Air velocity check during run time for thermal comfort control
    INTEGER :: AirVelErrIndex                               = 0  ! Air velocity error index

! For AdaptiveComfort tabular report
    REAL(r64) :: TimeNotMetASH5580 = 0.0d0
    REAL(r64) :: TimeNotMetASH5590 = 0.0d0
    REAL(r64) :: TimeNotMetCEN15251CatI = 0.0d0
    REAL(r64) :: TimeNotMetCEN15251CatII = 0.0d0
    REAL(r64) :: TimeNotMetCEN15251CatIII = 0.0d0


END TYPE PeopleData

TYPE LightsData
    CHARACTER(len=MaxNameLength) :: Name      =' ' ! LIGHTS object name
    INTEGER :: ZonePtr                        = 0  ! Which zone lights are in
    INTEGER :: SchedPtr                       =-1  ! Schedule for lights
    REAL(r64) :: DesignLevel                       = 0.0d0 ! design level for lights [W]
    LOGICAL   :: EMSLightsOn                       = .FALSE. !EMS actuating Lighting power if .TRUE.
    REAL(r64) :: EMSLightingPower                  = 0.0D0 ! Value EMS is directing to use for override
    REAL(r64) :: FractionReturnAir                 = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain that is return air
    REAL(r64) :: FractionRadiant                   = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
    REAL(r64) :: FractionShortWave                 = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain that is short wave
    REAL(r64) :: FractionReplaceable               = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain that is replaceable
    REAL(r64) :: FractionConvected                 = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
    LOGICAL   :: FractionReturnAirIsCalculated= .false.
    REAL(r64) :: FractionReturnAirPlenTempCoeff1   =0.0d0
    REAL(r64) :: FractionReturnAirPlenTempCoeff2   =0.0d0

    REAL(r64) :: NomMinDesignLevel            =0.d0 ! Nominal Minimum Design Level (min sch X design level)
    REAL(r64) :: NomMaxDesignLevel            =0.d0 ! Nominal Maximum Design Level (max sch X design level)

    LOGICAL :: ManageDemand               =.FALSE. ! Flag to indicate whether to use demand limiting
    REAL(r64) :: DemandLimit                       = 0.0d0 ! Demand limit set by demand manager [W]

    ! Report variables
    REAL(r64) :: Power                             = 0.0d0 ! Electric power [W]
    REAL(r64) :: RadGainRate                       = 0.0d0 ! Radiant heat gain [W]
    REAL(r64) :: VisGainRate                       = 0.0d0 ! Visible heat gain [W]
    REAL(r64) :: ConGainRate                       = 0.0d0 ! Convective heat gain [W]
    REAL(r64) :: RetAirGainRate                    = 0.0d0 ! Return air heat gain [W]
    REAL(r64) :: TotGainRate                       = 0.0d0 ! Total heat gain [W]

    REAL(r64) :: Consumption                       = 0.0d0 ! Electric consumption [J]
    REAL(r64) :: RadGainEnergy                     = 0.0d0 ! Radiant heat gain [J]
    REAL(r64) :: VisGainEnergy                     = 0.0d0 ! Visible heat gain [J]
    REAL(r64) :: ConGainEnergy                     = 0.0d0 ! Convective heat gain [J]
    REAL(r64) :: RetAirGainEnergy                  = 0.0d0 ! Return air heat gain [J]
    REAL(r64) :: TotGainEnergy                     = 0.0d0 ! Total heat gain [J]
    CHARACTER(len=MaxNameLength) :: EndUseSubcategory = ' ' !user defined name for the end use category
    REAL(r64) :: SumConsumption                    = 0.0d0 ! sum of electric consumption [J] for reporting
    REAL(r64) :: SumTimeNotZeroCons                = 0.0d0 ! sum of time of positive electric consumption [hr]
END TYPE LightsData

TYPE ZoneEquipData  ! Electric, Gas, Other Equipment, CO2
    CHARACTER(len=MaxNameLength) :: Name      =' ' ! EQUIPMENT object name
    INTEGER :: ZonePtr                        = 0  ! Which zone internal gain is in
    INTEGER :: SchedPtr                       = 0  ! Schedule for internal gain
    REAL(r64) :: DesignLevel                  = 0.0d0 ! design level for internal gain [W]
    LOGICAL   :: EMSZoneEquipOverrideOn       = .FALSE. !EMS actuating equipment power if .TRUE.
    REAL(r64) :: EMSEquipPower                = 0.0D0 ! Value EMS is directing to use for override
    REAL(r64) :: FractionLatent               = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain that is latent
    REAL(r64) :: FractionRadiant              = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
    REAL(r64) :: FractionLost                 = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain that is lost
    REAL(r64) :: FractionConvected            = 0.0d0 ! Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
    REAL(r64) :: CO2DesignRate                =0.d0 ! CO2 design Rate [m3/s]
    REAL(r64) :: CO2RateFactor                =0.d0 ! CO2 rate factor [m3/s/W]

    REAL(r64) :: NomMinDesignLevel            =0.d0 ! Nominal Minimum Design Level (min sch X design level)
    REAL(r64) :: NomMaxDesignLevel            =0.d0 ! Nominal Maximum Design Level (max sch X design level)

    LOGICAL :: ManageDemand               =.FALSE. ! Flag to indicate whether to use demand limiting
    REAL(r64) :: DemandLimit                  = 0.0d0 ! Demand limit set by demand manager [W]

    ! Report variables
    REAL(r64) :: Power                        = 0.0d0 ! Electric/Gas/Fuel power [W]
    REAL(r64) :: RadGainRate                  = 0.0d0 ! Radiant heat gain [W]
    REAL(r64) :: ConGainRate                  = 0.0d0 ! Convective heat gain [W]
    REAL(r64) :: LatGainRate                  = 0.0d0 ! Latent heat gain [W]
    REAL(r64) :: LostRate                     = 0.0d0 ! Lost energy (converted to work) [W]
    REAL(r64) :: TotGainRate                  = 0.0d0 ! Total heat gain [W]
    REAL(r64) :: CO2GainRate                  = 0.d0 !CO2 gain rate [m3/s]

    REAL(r64) :: Consumption                  = 0.0d0 ! Electric/Gas/Fuel consumption [J]
    REAL(r64) :: RadGainEnergy                = 0.0d0 ! Radiant heat gain [J]
    REAL(r64) :: ConGainEnergy                = 0.0d0 ! Convective heat gain [J]
    REAL(r64) :: LatGainEnergy                = 0.0d0 ! Latent heat gain [J]
    REAL(r64) :: LostEnergy                   = 0.0d0 ! Lost energy (converted to work) [J]
    REAL(r64) :: TotGainEnergy                = 0.0d0 ! Total heat gain [J]
    CHARACTER(len=MaxNameLength) :: EndUseSubcategory = ' ' !user defined name for the end use category
END TYPE ZoneEquipData

TYPE BBHeatData
    CHARACTER(len=MaxNameLength) :: Name      =' ' ! BASEBOARD HEAT object name
    INTEGER :: ZonePtr                        =0
    INTEGER :: SchedPtr                       =0
    REAL(r64) :: CapatLowTemperature               = 0.0d0
    REAL(r64) :: LowTemperature                    = 0.0d0
    REAL(r64) :: CapatHighTemperature              = 0.0d0
    REAL(r64) :: HighTemperature                   = 0.0d0
    LOGICAL   :: EMSZoneBaseboardOverrideOn        = .FALSE. !EMS actuating equipment power if .TRUE.
    REAL(r64) :: EMSZoneBaseboardPower             = 0.0d0 ! Value EMS is directing to use for override
    REAL(r64) :: FractionRadiant                   = 0.0d0
    REAL(r64) :: FractionConvected                 = 0.0d0

    LOGICAL :: ManageDemand               =.FALSE. ! Flag to indicate whether to use demand limiting
    REAL(r64) :: DemandLimit                       = 0.0d0 ! Demand limit set by demand manager [W]

    ! Report variables
    REAL(r64) :: Power                             = 0.0d0 ! Electric power [W]
    REAL(r64) :: RadGainRate                       = 0.0d0 ! Radiant heat gain [W]
    REAL(r64) :: ConGainRate                       = 0.0d0 ! Convective heat gain [W]
    REAL(r64) :: TotGainRate                       = 0.0d0 ! Total heat gain [W]

    REAL(r64) :: Consumption                       = 0.0d0 ! Electric consumption [J]
    REAL(r64) :: RadGainEnergy                     = 0.0d0 ! Radiant heat gain [J]
    REAL(r64) :: ConGainEnergy                     = 0.0d0 ! Convective heat gain [J]
    REAL(r64) :: TotGainEnergy                     = 0.0d0 ! Total heat gain [J]
    CHARACTER(len=MaxNameLength) :: EndUseSubcategory = ' ' !user defined name for the end use category
END TYPE BBHeatData

TYPE InfiltrationData
    CHARACTER(len=MaxNameLength) :: Name      =' '
    INTEGER :: ZonePtr                        = 0  ! Which zone infiltration is in
    INTEGER :: SchedPtr                       = 0  ! Schedule for infiltration
    INTEGER :: ModelType                      = 0  ! which model is used for infiltration
    ! Design Flow Rate model terms
    REAL(r64) :: DesignLevel                  =0.0D0
    REAL(r64) :: ConstantTermCoef             =0.0D0
    REAL(r64) :: TemperatureTermCoef          =0.0D0
    REAL(r64) :: VelocityTermCoef             =0.0D0
    REAL(r64) :: VelocitySQTermCoef           =0.0D0

    ! Effective Leakage Area, Sherman Grimsrud terms
    REAL(r64) :: LeakageArea                  =0.0D0 ! "AL" effective air leakage area
    REAL(r64) :: BasicStackCoefficient        =0.0D0 ! "Cs" Stack coefficient
    REAL(r64) :: BasicWindCoefficient         =0.0D0 ! "Cw" wind coefficient

    ! Flow Coefficient, AIM-2, Walker and Wilson terms
    REAL(r64) :: FlowCoefficient              =0.0D0 ! "c" Flow coefficient
    REAL(r64) :: AIM2StackCoefficient         =0.0D0 ! "Cs" stack coefficient
    REAL(r64) :: AIM2WindCoefficient          =0.0D0 ! "Cw" wind coefficient
    REAL(r64) :: PressureExponent             =0.0D0 ! "n" pressure power law exponent
    REAL(r64) :: ShelterFactor                =0.0D0 ! "s" shelter factor

    LOGICAL   :: EMSOverrideOn                = .FALSE. ! if true then EMS is requesting to override
    REAL(r64) :: EMSAirFlowRateValue          = 0.0D0 ! value EMS is setting for air flow rate
    LOGICAL   :: QuadratureSum                = .FALSE. ! If quadrature sum of zone air balance method is used
    INTEGER :: OABalancePtr                   =0   ! A pointer to ZoneAirBalance If quadrature is true

END TYPE InfiltrationData

TYPE VentilationData
    CHARACTER(len=MaxNameLength) :: Name      =' '
    INTEGER :: ZonePtr                        =0
    INTEGER :: SchedPtr                       =0
    INTEGER :: ModelType                      =0  ! which model is used for ventilation: DesignFlowRate and WindandStackOpenArea
    REAL(r64) :: DesignLevel                       = 0.0d0
    LOGICAL   :: EMSSimpleVentOn                   = .FALSE. !EMS actuating ventilation flow rate if .TRUE.
    REAL(r64) :: EMSimpleVentFlowRate              = 0.0D0 ! Value EMS is directing to use for override
    REAL(r64) :: MinIndoorTemperature              =-100.0d0
    REAL(r64) :: DelTemperature                    = 0.0d0
    INTEGER :: FanType                        =0
    REAL(r64) :: FanPressure                       = 0.0d0
    REAL(r64) :: FanEfficiency                     = 0.0d0
    REAL(r64) :: FanPower                          = 0.0d0
    REAL(r64) :: AirTemp                           = 0.0d0
    REAL(r64) :: ConstantTermCoef                  = 0.0d0
    REAL(r64) :: TemperatureTermCoef               = 0.0d0
    REAL(r64) :: VelocityTermCoef                  = 0.0d0
    REAL(r64) :: VelocitySQTermCoef                = 0.0d0
    REAL(r64) :: MaxIndoorTemperature              =100.0d0
    REAL(r64) :: MinOutdoorTemperature             =-100.0d0
    REAL(r64) :: MaxOutdoorTemperature             =100.0d0
    REAL(r64) :: MaxWindSpeed                      =40.0d0
    INTEGER :: MinIndoorTempSchedPtr          =0   ! Minimum indoor temperature schedule index
    INTEGER :: MaxIndoorTempSchedPtr          =0   ! Maximum indoor temperature schedule index
    INTEGER :: DeltaTempSchedPtr              =0   ! Delta temperature schedule index
    INTEGER :: MinOutdoorTempSchedPtr         =0   ! Minimum outdoor temperature schedule index
    INTEGER :: MaxOutdoorTempSchedPtr         =0   ! Maximum outdoor temperature schedule index
    INTEGER :: IndoorTempErrCount             =0   ! Indoor temperature error count
    INTEGER :: OutdoorTempErrCount            =0   ! Outdoor temperature error count
    INTEGER :: IndoorTempErrIndex             =0   ! Indoor temperature error Index
    INTEGER :: OutdoorTempErrIndex            =0   ! Outdoor temperature error Index
    INTEGER :: HybridControlType              =0   ! Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
    INTEGER :: HybridControlMasterNum         =0   ! Hybrid ventilation control master object number
    LOGICAL :: HybridControlMasterStatus      =.FALSE.  ! Hybrid ventilation control master object opening status
    LOGICAL   :: QuadratureSum                = .FALSE. ! If quadrature sum of zone air balance method is used
    INTEGER :: OABalancePtr                   =0   ! A pointer to ZoneAirBalance
    ! WindandStackOpenArea
    REAL(r64) :: OpenArea                     = 0.0d0  ! Opening area [m2]
    INTEGER :: OpenAreaSchedPtr               =0   ! Opening area fraction schedule pointer
    REAL(r64) :: OpenEff                           = 0.0d0  ! Opening effectiveness [dimensionless]
    REAL(r64) :: EffAngle                          = 0.0d0  ! Effective angle [degree]
    REAL(r64) :: DH                                = 0.0d0  ! Height difference [m]
    REAL(r64) :: DiscCoef                          = 0.0d0  ! Discharge coefficient
END TYPE VentilationData

TYPE ZoneAirBalanceData
    CHARACTER(len=MaxNameLength) :: Name      =' '  ! Object name
    CHARACTER(len=MaxNameLength) :: ZoneName  =' '  ! Zone name
    INTEGER :: ZonePtr                        =0    ! Zone number
    INTEGER :: BalanceMethod                  =0    ! Air Balance Method: None=0, Quadrature = 1
    REAL(r64) :: InducedAirRate               = 0.0d0  ! Induced Outdoor Air Due to Duct Leakage Unbalance [m3/s]
    INTEGER :: InducedAirSchedPtr             =0    ! Induced Outdoor Air Fraction Schedule
    REAL(r64) :: BalMassFlowRate              = 0.0d0  ! balanced mass flow rate
    REAL(r64) :: InfMassFlowRate              = 0.0d0  ! unbalanced mass flow rate from infiltration
    REAL(r64) :: NatMassFlowRate              = 0.0d0  ! unbalanced mass flow rate from natural ventilaton
    REAL(r64) :: ExhMassFlowRate              = 0.0d0  ! unbalanced mass flow rate from exhaust ventilaton
    REAL(r64) :: IntMassFlowRate              = 0.0d0  ! unbalanced mass flow rate from intake ventilaton
    REAL(r64) :: ERVMassFlowRate              = 0.0d0  ! unbalanced mass flow rate from stand-alond ERV
    LOGICAL :: OneTimeFlag                    =.FALSE. ! One time flag to get nodes of stand alond ERV
    INTEGER :: NumOfERVs                      =0    ! Number of zone stand alone ERVs
    INTEGER, DIMENSION(:), ALLOCATABLE     :: ERVInletNode    ! Stand alone ERV supply air inlet nodes
    INTEGER, DIMENSION(:), ALLOCATABLE     :: ERVExhaustNode  ! Stand alone ERV air exhaust nodes
END TYPE ZoneAirBalanceData

TYPE MixingData
    CHARACTER(len=MaxNameLength) :: Name      =' '
    INTEGER :: ZonePtr                        =0
    INTEGER :: SchedPtr                       =0
    REAL(r64) :: DesignLevel                  = 0.0d0
    INTEGER :: FromZone                       =0
    REAL(r64) :: DeltaTemperature             = 0.0d0
    REAL(r64) :: DesiredAirFlowRate           = 0.0d0
    INTEGER :: DeltaTempSchedPtr              =0   ! Delta temperature schedule index
    INTEGER :: MinIndoorTempSchedPtr          =0   ! Minimum indoor temperature schedule index
    INTEGER :: MaxIndoorTempSchedPtr          =0   ! Maximum indoor temperature schedule index
    INTEGER :: MinSourceTempSchedPtr          =0   ! Minimum source zone temperature schedule index
    INTEGER :: MaxSourceTempSchedPtr          =0   ! Maximum source zone temperature schedule index
    INTEGER :: MinOutdoorTempSchedPtr         =0   ! Minimum outdoor temperature schedule index
    INTEGER :: MaxOutdoorTempSchedPtr         =0   ! Maximum outdoor temperature schedule index
    INTEGER :: IndoorTempErrCount             =0   ! Indoor temperature error count
    INTEGER :: SourceTempErrCount             =0   ! Source zone temperature error count
    INTEGER :: OutdoorTempErrCount            =0   ! Outdoor temperature error count
    INTEGER :: IndoorTempErrIndex             =0   ! Indoor temperature error Index
    INTEGER :: SourceTempErrIndex             =0   ! Source zone temperature error Index
    INTEGER :: OutdoorTempErrIndex            =0   ! Outdoor temperature error Index
    INTEGER :: HybridControlType              =0   ! Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
    INTEGER :: HybridControlMasterNum         =0   ! Hybrid ventilation control master ventilation object number
    INTEGER :: NumRefDoorConnections          = 0
    LOGICAL   :: EMSSimpleMixingOn            = .FALSE. !EMS actuating ventilation flow rate if .TRUE.
    LOGICAL   :: RefDoorMixFlag               = .FALSE. !Refrigeration door mixing within zone
    REAL(r64) :: EMSimpleMixingFlowRate       = 0.0D0 ! Value EMS is directing to use for override
    LOGICAL,   DIMENSION(:), ALLOCATABLE     :: EMSRefDoorMixingOn    !
    REAL(r64), DIMENSION(:), ALLOCATABLE     :: EMSRefDoorFlowRate    !
    REAL(r64), DIMENSION(:), ALLOCATABLE     :: VolRefDoorFlowRate    !
    INTEGER,   DIMENSION(:), ALLOCATABLE     :: OpenSchedPtr  ! Schedule for Refrigeration door open fraction
    REAL(r64), DIMENSION(:), ALLOCATABLE     :: DoorHeight    ! Door height for refrigeration door, m
    REAL(r64), DIMENSION(:), ALLOCATABLE     :: DoorArea      ! Door area for refrigeration door, m2
    REAL(r64), DIMENSION(:), ALLOCATABLE     :: Protection    ! Refrigeration door protection factor, dimensionless
    INTEGER,   DIMENSION(:), ALLOCATABLE     :: MateZonePtr   ! Zone connected by refrigeration door (MateZone > ZonePtr)
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE     :: DoorMixingObjectName ! Used in one error statement and eio
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE     :: DoorProtTypeName ! Used in eio
    !Note, for mixing and crossmixing, this type dimensioned by number of mixing objects.
    !For ref door mixing, dimensioned by number of zones.

END TYPE MixingData

TYPE GenericComponentZoneIntGainStruct
  CHARACTER(len=MaxNameLength) :: CompObjectType = ' '  ! device object class name
  CHARACTER(len=MaxNameLength) :: CompObjectName = ' '  ! device user unique name
  INTEGER                      :: CompTypeOfNum  = 0    ! type of internal gain device identifier
  REAL(r64) , POINTER :: PtrConvectGainRate  ! fortan POINTER to value of convection heat gain rate for device, watts
  REAL(r64)           :: ConvectGainRate  ! current timestep value of convection heat gain rate for device, watts
  REAL(r64) , POINTER :: PtrReturnAirConvGainRate ! fortan POINTER to value of return air convection heat gain rate for device, W
  REAL(r64)           :: ReturnAirConvGainRate ! urrent timestep value of return air convection heat gain rate for device, W
  REAL(r64) , POINTER :: PtrRadiantGainRate  ! fortan POINTER to value of thermal radiation heat gain rate for device, watts
  REAL(r64)           :: RadiantGainRate  !  current timestep value of thermal radiation heat gain rate for device, watts
  REAL(r64) , POINTER :: PtrLatentGainRate ! fortan POINTER to value of moisture gain rate for device, Watts
  REAL(r64)           :: LatentGainRate !  current timestep value of moisture gain rate for device, Watts
  REAL(r64) , POINTER :: PtrReturnAirLatentGainRate ! fortan POINTER to value of return air moisture gain rate for device, Watts
  REAL(r64)           :: ReturnAirLatentGainRate !  current timestep value of return air moisture gain rate for device, Watts
  REAL(R64) , POINTER :: PtrCarbonDioxideGainRate ! fortan POINTER to value of carbon dioxide gain rate for device
  REAL(R64)           :: CarbonDioxideGainRate !  current timestep value of carbon dioxide gain rate for device
  REAL(R64) , POINTER :: PtrGenericContamGainRate ! fortan POINTER to value of generic contaminant gain rate for device
  REAL(R64)           :: GenericContamGainRate !  current timestep value of generic contaminant gain rate for device
END TYPE GenericComponentZoneIntGainStruct

TYPE ZoneSimData        ! Calculated data by Zone during each time step/hour
  REAL(r64) :: NOFOCC  = 0.0d0        !Number of Occupants, zone total
  REAL(r64) :: QOCTOT  = 0.0d0        !Total Energy from Occupants
  REAL(r64) :: QOCSEN  = 0.0d0        !Sensible Energy from Occupants
  REAL(r64) :: QOCCON  = 0.0d0        !ENERGY CONVECTED FROM OCCUPANTS (WH)
  REAL(r64) :: QOCRAD  = 0.0d0        !ENERGY RADIATED FROM OCCUPANTS
  REAL(r64) :: QOCLAT  = 0.0d0        !LATENT ENERGY FROM OCCUPANTS

  REAL(r64) :: QLTTOT  = 0.0d0        !TOTAL ENERGY INTO LIGHTS (WH)
  REAL(r64) :: QLTCON  = 0.0d0        !ENERGY CONVECTED TO SPACE AIR FROM LIGHTS
  REAL(r64) :: QLTRAD  = 0.0d0        !ENERGY RADIATED TO SPACE FROM LIGHTS
  REAL(r64) :: QLTCRA  = 0.0d0        !ENERGY CONVECTED TO RETURN AIR FROM LIGHTS
  REAL(r64) :: QLTSW   = 0.0d0        !VISIBLE ENERGY FROM LIGHTS


  REAL(r64) :: QEECON  = 0.0d0        !ENERGY CONVECTED FROM ELECTRIC EQUIPMENT
  REAL(r64) :: QEERAD  = 0.0d0        !ENERCY RADIATED FROM ELECTRIC EQUIPMENT
  REAL(r64) :: QEELost = 0.0d0        ! Energy from Electric Equipment (lost)
  REAL(r64) :: QEELAT  = 0.0d0        !LATENT ENERGY FROM Electric Equipment

  REAL(r64) :: QGECON  = 0.0d0        !ENERGY CONVECTED FROM GAS EQUIPMENT
  REAL(r64) :: QGERAD  = 0.0d0        !ENERGY RADIATED FROM GAS EQUIPMENT
  REAL(r64) :: QGELost = 0.0d0        ! Energy from Gas Equipment (lost)
  REAL(r64) :: QGELAT  = 0.0d0        !LATENT ENERGY FROM Gas Equipment

  REAL(r64) :: QOECON  = 0.0d0        !ENERGY CONVECTED FROM OTHER EQUIPMENT
  REAL(r64) :: QOERAD  = 0.0d0        !ENERGY RADIATED FROM OTHER EQUIPMENT
  REAL(r64) :: QOELost = 0.0d0        ! Energy from Other Equipment (lost)
  REAL(r64) :: QOELAT  = 0.0d0        !LATENT ENERGY FROM Other Equipment

  REAL(r64) :: QHWCON  = 0.0d0        !ENERGY CONVECTED FROM Hot Water EQUIPMENT
  REAL(r64) :: QHWRAD  = 0.0d0        !ENERGY RADIATED FROM Hot Water EQUIPMENT
  REAL(r64) :: QHWLost = 0.0d0        ! Energy from Hot Water Equipment (lost)
  REAL(r64) :: QHWLAT  = 0.0d0        !LATENT ENERGY FROM Hot Water Equipment

  REAL(r64) :: QSECON  = 0.0d0        !ENERGY CONVECTED FROM Steam EQUIPMENT
  REAL(r64) :: QSERAD  = 0.0d0        !ENERGY RADIATED FROM Steam EQUIPMENT
  REAL(r64) :: QSELost = 0.0d0        ! Energy from Steam Equipment (lost)
  REAL(r64) :: QSELAT  = 0.0d0        !LATENT ENERGY FROM Steam Equipment

  REAL(r64) :: QBBCON  = 0.0d0        !ENERGY CONVECTED FROM BASEBOARD HEATING
  REAL(r64) :: QBBRAD  = 0.0d0        !ENERGY RADIATED FROM BASEBOARD HEATING

  INTEGER    :: NumberOfDevices = 0
  INTEGER    :: MaxNumberOfDevices = 0
  TYPE(GenericComponentZoneIntGainStruct), DIMENSION(:), ALLOCATABLE :: Device !

END TYPE ZoneSimData

TYPE WindowBlindProperties

  CHARACTER(MaxNameLength+1)    :: Name=' '
  INTEGER :: MaterialNumber        =0 ! Material pointer for the blind
                  ! Input properties
  INTEGER :: SlatOrientation=0           ! HORIZONTAL or VERTICAL
  INTEGER :: SlatAngleType  =FixedSlats  ! FIXED or VARIABLE
  REAL(r64) :: SlatWidth                = 0.0d0 ! Slat width (m)
  REAL(r64) :: SlatSeparation           = 0.0d0 ! Slat separation (m)
  REAL(r64) :: SlatThickness            = 0.0d0 ! Slat thickness (m)
  REAL(r64) :: SlatCrown                = 0.0d0 ! the height of the slate (length from the chord to the curve)
  REAL(r64) :: SlatAngle                = 0.0d0 ! Slat angle (deg)
  REAL(r64) :: MinSlatAngle             = 0.0d0 ! Minimum slat angle for variable-angle slats (deg) (user input)
  REAL(r64) :: MaxSlatAngle             = 0.0d0 ! Maximum slat angle for variable-angle slats (deg) (user input)
  REAL(r64) :: SlatConductivity         = 0.0d0 ! Slat conductivity (W/m-K)
    ! Solar slat properties
  REAL(r64) :: SlatTransSolBeamDiff     = 0.0d0 ! Slat solar beam-diffuse transmittance
  REAL(r64) :: SlatFrontReflSolBeamDiff = 0.0d0 ! Slat front solar beam-diffuse reflectance
  REAL(r64) :: SlatBackReflSolBeamDiff  = 0.0d0 ! Slat back solar beam-diffuse reflectance
  REAL(r64) :: SlatTransSolDiffDiff     = 0.0d0 ! Slat solar diffuse-diffuse transmittance
  REAL(r64) :: SlatFrontReflSolDiffDiff = 0.0d0 ! Slat front solar diffuse-diffuse reflectance
  REAL(r64) :: SlatBackReflSolDiffDiff  = 0.0d0 ! Slat back solar diffuse-diffuse reflectance
    ! Visible slat properties
  REAL(r64) :: SlatTransVisBeamDiff     = 0.0d0 ! Slat visible beam-diffuse transmittance
  REAL(r64) :: SlatFrontReflVisBeamDiff = 0.0d0 ! Slat front visible beam-diffuse reflectance
  REAL(r64) :: SlatBackReflVisBeamDiff  = 0.0d0 ! Slat back visible beam-diffuse reflectance
  REAL(r64) :: SlatTransVisDiffDiff     = 0.0d0 ! Slat visible diffuse-diffuse transmittance
  REAL(r64) :: SlatFrontReflVisDiffDiff = 0.0d0 ! Slat front visible diffuse-diffuse reflectance
  REAL(r64) :: SlatBackReflVisDiffDiff  = 0.0d0 ! Slat back visible diffuse-diffuse reflectance
    ! Long-wave (IR) slat properties
  REAL(r64) :: SlatTransIR              = 0.0d0 ! Slat IR transmittance
  REAL(r64) :: SlatFrontEmissIR         = 0.0d0 ! Slat front emissivity
  REAL(r64) :: SlatBackEmissIR          = 0.0d0 ! Slat back emissivity
    ! Some characteristics for blind thermal calculation
  REAL(r64) :: BlindToGlassDist         = 0.0d0 ! Distance between window shade and adjacent glass (m)
  REAL(r64) :: BlindTopOpeningMult      = 0.0d0 ! Area of air-flow opening at top of blind, expressed as a fraction
                                        !  of the blind-to-glass opening area at the top of the blind
  REAL(r64) :: BlindBottomOpeningMult   = 0.0d0 ! Area of air-flow opening at bottom of blind, expressed as a fraction
                                        !  of the blind-to-glass opening area at the bottom of the blind
  REAL(r64) :: BlindLeftOpeningMult     = 0.0d0 ! Area of air-flow opening at left side of blind, expressed as a fraction
                                        !  of the blind-to-glass opening area at the left side of the blind
  REAL(r64) :: BlindRightOpeningMult    = 0.0d0 ! Area of air-flow opening at right side of blind, expressed as a fraction
                                        !  of the blind-to-glass opening area at the right side of the blind
                      ! Calculated blind properties
    ! Blind solar properties
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolFrontBeamBeamTrans= 0.0d0 ! Blind solar front beam-beam transmittance vs.
                                                                    ! profile angle, slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolFrontBeamBeamRefl = 0.0d0 ! Blind solar front beam-beam reflectance vs. profile angle,
                                                                    ! slat angle (zero)
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolBackBeamBeamTrans = 0.0d0 ! Blind solar back beam-beam transmittance vs. profile angle,
                                                                    ! slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolBackBeamBeamRefl  = 0.0d0 ! Blind solar back beam-beam reflectance vs. profile angle,
                                                                    ! slat angle (zero)
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolFrontBeamDiffTrans= 0.0d0 ! Blind solar front beam-diffuse transmittance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolFrontBeamDiffRefl = 0.0d0 ! Blind solar front beam-diffuse reflectance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolBackBeamDiffTrans = 0.0d0 ! Blind solar back beam-diffuse transmittance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolBackBeamDiffRefl  = 0.0d0 ! Blind solar back beam-diffuse reflectance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolFrontDiffDiffTrans   = 0.0d0 ! Blind solar front diffuse-diffuse transmittance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolFrontDiffDiffTransGnd= 0.0d0 ! Blind ground solar front diffuse-diffuse transmittance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolFrontDiffDiffTransSky= 0.0d0 ! Blind sky solar front diffuse-diffuse transmittance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolFrontDiffDiffRefl    = 0.0d0 ! Blind solar front diffuse-diffuse reflectance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolFrontDiffDiffReflGnd = 0.0d0 ! Blind ground solar front diffuse-diffuse reflectance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolFrontDiffDiffReflSky = 0.0d0 ! Blind sky solar front diffuse-diffuse reflectance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolBackDiffDiffTrans    = 0.0d0 ! Blind solar back diffuse-diffuse transmittance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolBackDiffDiffRefl     = 0.0d0 ! Blind solar back diffuse-diffuse reflectance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolFrontBeamAbs         = 0.0d0 ! Blind solar front beam absorptance vs. slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: SolBackBeamAbs          = 0.0d0 ! Blind solar back beam absorptance vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolFrontDiffAbs         = 0.0d0 ! Blind solar front diffuse absorptance vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolFrontDiffAbsGnd      = 0.0d0 ! Blind ground solar front diffuse absorptance vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolFrontDiffAbsSky      = 0.0d0 ! Blind sky solar front diffuse absorptance vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: SolBackDiffAbs          = 0.0d0 ! Blind solar back diffuse absorptance vs. slat angle

    ! Blind visible properties
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: VisFrontBeamBeamTrans= 0.0d0 ! Blind visible front beam-beam transmittance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: VisFrontBeamBeamRefl = 0.0d0 ! Blind visible front beam-beam reflectance
                                                                    ! vs. profile angle, slat angle (zero)
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: VisBackBeamBeamTrans = 0.0d0 ! Blind visible back beam-beam transmittance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: VisBackBeamBeamRefl  = 0.0d0 ! Blind visible back beam-beam reflectance
                                                                    ! vs. profile angle, slat angle (zero)
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: VisFrontBeamDiffTrans= 0.0d0 ! Blind visible front beam-diffuse transmittance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: VisFrontBeamDiffRefl = 0.0d0 ! Blind visible front beam-diffuse reflectance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: VisBackBeamDiffTrans = 0.0d0 ! Blind visible back beam-diffuse transmittance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(37,MaxSlatAngs) :: VisBackBeamDiffRefl  = 0.0d0 ! Blind visible back beam-diffuse reflectance
                                                                    ! vs. profile angle, slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: VisFrontDiffDiffTrans   = 0.0d0 ! Blind visible front diffuse-diffuse transmittance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: VisFrontDiffDiffRefl    = 0.0d0 ! Blind visible front diffuse-diffuse reflectance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: VisBackDiffDiffTrans    = 0.0d0 ! Blind visible back diffuse-diffuse transmittance
                                                                    ! vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: VisBackDiffDiffRefl     = 0.0d0 ! Blind visible back diffuse-diffuse reflectance
                                                                    ! vs. slat angle

    ! Long-wave (IR) blind properties
  REAL(r64), DIMENSION(MaxSlatAngs) :: IRFrontTrans            = 0.0d0 ! Blind IR front transmittance vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: IRFrontEmiss            = 0.0d0 ! Blind IR front emissivity vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: IRBackTrans             = 0.0d0 ! Blind IR back transmittance vs. slat angle
  REAL(r64), DIMENSION(MaxSlatAngs) :: IRBackEmiss             = 0.0d0 ! Blind IR back emissivity vs. slat angle
END TYPE WindowBlindProperties

TYPE SurfaceScreenProperties
  INTEGER :: MaterialNumber        =0   ! Material pointer for the screen
  REAL(r64) :: BmBmTrans             = 0.0d0 ! Beam solar transmittance (dependent on sun angle)
                                        ! (this value can include scattering if the user so chooses)
  REAL(r64) :: BmBmTransBack         = 0.0d0 ! Beam solar transmittance (dependent on sun angle) from back side of screen
  REAL(r64) :: BmBmTransVis          = 0.0d0 ! Visible solar transmittance (dependent on sun angle)
                                        ! (this value can include visible scattering if the user so chooses)
  REAL(r64) :: BmDifTrans            = 0.0d0 ! Beam solar transmitted as diffuse radiation (dependent on sun angle)
  REAL(r64) :: BmDifTransBack        = 0.0d0 ! Beam solar transmitted as diffuse radiation (dependent on sun angle) from back side
  REAL(r64) :: BmDifTransVis         = 0.0d0 ! Visible solar transmitted as diffuse radiation (dependent on sun angle)
                                        ! The following reflectance properties are dependent on sun angle:
  REAL(r64) :: ReflectSolBeamFront   = 0.0d0 ! Beam solar reflected as diffuse radiation when sun is in front of screen
  REAL(r64) :: ReflectVisBeamFront   = 0.0d0 ! Visible solar reflected as diffuse radiation when sun is in front of screen
  REAL(r64) :: ReflectSolBeamBack    = 0.0d0 ! Beam solar reflected as diffuse radiation when sun is in back of screen
  REAL(r64) :: ReflectVisBeamBack    = 0.0d0 ! Visible solar reflected as diffuse radiation when sun is in back of screen
  REAL(r64) :: AbsorpSolarBeamFront  = 0.0d0 ! Front surface solar beam absorptance
  REAL(r64) :: AbsorpSolarBeamBack   = 0.0d0 ! Back surface solar beam absorptance
  REAL(r64) :: DifDifTrans           = 0.0d0 ! Back surface diffuse solar transmitted
  REAL(r64) :: DifDifTransVis        = 0.0d0 ! Back surface diffuse visible solar transmitted
  REAL(r64) :: DifScreenAbsorp       = 0.0d0 ! Absorption of diffuse radiation
  REAL(r64) :: DifReflect            = 0.0d0 ! Back reflection of solar diffuse radiation
  REAL(r64) :: DifReflectVis         = 0.0d0 ! Back reflection of visible diffuse radiation
  REAL(r64) :: ReflectScreen         = 0.0d0 ! Screen assembly solar reflectance (user input adjusted for holes in screen)
  REAL(r64) :: ReflectScreenVis      = 0.0d0 ! Screen assembly visible reflectance (user input adjusted for holes in screen)
  REAL(r64) :: ReflectCylinder       = 0.0d0 ! Screen material solar reflectance (user input, does not account for holes in screen)
  REAL(r64) :: ReflectCylinderVis    = 0.0d0 ! Screen material visible reflectance (user input, does not account for holes in screen)
  REAL(r64) :: ScreenDiameterToSpacingRatio = 0.0d0  ! ratio of screen material diameter to screen material spacing
  INTEGER :: ScreenBeamReflectanceAccounting = 0 ! user specified method of accounting for scattered solar beam
END TYPE SurfaceScreenProperties

TYPE ScreenTransData
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: Trans
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: Scatt
END TYPE ScreenTransData

TYPE ZoneCatEUseData
  REAL(r64), DIMENSION(0:25)  :: EEConvected     = 0.0d0 ! Category (0 to 25) Energy Convected from Electric Equipment
  REAL(r64), DIMENSION(0:25)  :: EERadiated      = 0.0d0 ! Category (0 to 25) Energy Radiated from Electric Equipment
  REAL(r64), DIMENSION(0:25)  :: EELost          = 0.0d0 ! Category (0 to 25) Energy from Electric Equipment (lost)
  REAL(r64), DIMENSION(0:25)  :: EELatent        = 0.0d0 ! Category (0 to 25) Latent Energy from Electric Equipment
END TYPE ZoneCatEUseData

TYPE RefrigCaseCreditData
  REAL(r64) :: SenCaseCreditToZone = 0.0d0 !Refrigerated display case sensible energy delivered to zone
                                         ! includes refrigeration pipe and receiver heat exchange with zone
  REAL(r64) :: LatCaseCreditToZone = 0.0d0 !Refrigerated display case latent energy delivered to zone
  REAL(r64) :: SenCaseCreditToHVAC = 0.0d0 !Refrigerated display case sensible energy delivered to HVAC RA duct
  REAL(r64) :: LatCaseCreditToHVAC = 0.0d0 !Refrigerated display case latent energy delivered to HVAC RA duct
END TYPE RefrigCaseCreditData

TYPE HeatReclaimRefrigeratedRackData
  CHARACTER(len=MaxNameLength) :: Name = ' '          !Name of refrigerated rack
  CHARACTER(len=MaxNameLength) :: SourceType = ' '    !object type for refrigerated rack
  REAL(r64)                    :: AvailCapacity = 0.0d0 !Total available heat reclaim capacity
  REAL(r64)                    :: UsedWaterHeater = 0.0d0 !amount of avail used at plant water heater
  REAL(r64)                    :: UsedHVACCoil = 0.0d0  !amount of avail used at hvac coil

END TYPE HeatReclaimRefrigeratedRackData

TYPE HeatReclaimRefrigCondenserData
  CHARACTER(len=MaxNameLength) :: Name = ' '          !Name of refrigeration system
  INTEGER                      :: SourceType    =0    !object type for refrigeration system
  REAL(r64)                    :: AvailCapacity = 0.0d0 !Total available heat reclaim capacity
  REAL(r64)                    :: AvailTemperature = 0.0d0 !Temperature of heat reclaim source
  REAL(r64)                    :: UsedWaterHeater = 0.0d0 !amount of avail used at plant water heater
  REAL(r64)                    :: UsedHVACCoil = 0.0d0  !amount of avail used at hvac coil
END TYPE HeatReclaimRefrigCondenserData

TYPE HeatReclaimDXCoilData
  CHARACTER(len=MaxNameLength) :: Name = ' '          !Name of DX Coil
  CHARACTER(len=MaxNameLength) :: SourceType = ' '    !SourceType for DX Coil
  REAL(r64)                    :: AvailCapacity = 0.0d0 !Total available heat reclaim capacity
END TYPE HeatReclaimDXCoilData

Type :: AirReportVars
  REAL(r64) :: MeanAirTemp         = 0.0d0 ! Mean Air Temperature {C}
  REAL(r64) :: OperativeTemp       = 0.0d0 ! Average of Mean Air Temperature {C} and Mean Radiant Temperature {C}
  REAL(r64) :: MeanAirHumRat       =0.0D0 ! Mean Air Humidity Ratio {kg/kg} (averaged over zone time step)
  REAL(r64) :: MeanAirDewpointTemp = 0.0d0 ! Mean Air Dewpoint Temperature {C}
  REAL(r64) :: ThermOperativeTemp  = 0.0d0 ! Mix or MRT and MAT for Zone Control:Thermostatic:Operative Temperature {C}
  REAL(r64) :: InfilHeatGain       = 0.0d0 ! Heat Gain {J} due to infiltration
  REAL(r64) :: InfilHeatLoss       = 0.0d0 ! Heat Loss {J} due to infiltration
  REAL(r64) :: InfilLatentGain     = 0.0d0 ! Latent Gain {J} due to infiltration
  REAL(r64) :: InfilLatentLoss     = 0.0d0 ! Latent Loss {J} due to infiltration
  REAL(r64) :: InfilTotalGain      = 0.0d0 ! Total Gain {J} due to infiltration (sensible+latent)
  REAL(r64) :: InfilTotalLoss      = 0.0d0 ! Total Loss {J} due to infiltration (sensible+latent)
  REAL(r64) :: InfilVolumeCurDensity = 0.0d0 ! Volume of Air {m3} due to infiltration at current zone air density
  REAL(r64) :: InfilVolumeStdDensity = 0.0d0 ! Volume of Air {m3} due to infiltration at standard density (adjusted for elevation)
  REAL(r64) :: InfilVdotCurDensity = 0.0d0 ! Volume flow rate of Air {m3/s} due to infiltration at current zone air density
  REAL(r64) :: InfilVdotStdDensity = 0.0d0 ! Volume flow rate of Air {m3/s} due to infiltration standard density (adjusted elevation)
  REAL(r64) :: InfilMass           = 0.0d0 ! Mass of Air {kg} due to infiltration
  REAL(r64) :: InfilAirChangeRate  = 0.0d0 ! Infiltration air change rate {ach}
  REAL(r64) :: VentilHeatLoss      = 0.0d0 ! Heat Gain {J} due to ventilation
  REAL(r64) :: VentilHeatGain      = 0.0d0 ! Heat Loss {J} due to ventilation
  REAL(r64) :: VentilLatentLoss    = 0.0d0 ! Latent Gain {J} due to ventilation
  REAL(r64) :: VentilLatentGain    = 0.0d0 ! Latent Loss {J} due to ventilation
  REAL(r64) :: VentilTotalLoss     = 0.0d0 ! Total Gain {J} due to ventilation
  REAL(r64) :: VentilTotalGain     = 0.0d0 ! Total Loss {J} due to ventilation
  REAL(r64) :: VentilVolumeCurDensity = 0.0d0 ! Volume of Air {m3} due to ventilation at current zone air density
  REAL(r64) :: VentilVolumeStdDensity = 0.0d0 ! Volume of Air {m3} due to ventilation at standard density (adjusted for elevation)
  REAL(r64) :: VentilVdotCurDensity = 0.0d0 ! Volume flow rate of Air {m3/s} due to ventilation at current zone air density
  REAL(r64) :: VentilVdotStdDensity = 0.0d0 ! Volume flowr of Air {m3/s} due to ventilation at standard density (adjusted elevation)
  REAL(r64) :: VentilMass          = 0.0d0 ! Mass of Air {kg} due to ventilation
  REAL(r64) :: VentilAirChangeRate = 0.0d0 ! Ventilation air change rate (ach)
  REAL(r64) :: VentilFanElec       = 0.0d0 ! Fan Electricity {W} due to ventilation
  REAL(r64) :: VentilAirTemp       = 0.0d0 ! Air Temp {C} of ventilation
  REAL(r64) :: MixVolume           = 0.0d0 ! Mixing volume of Air {m3}
  REAL(r64) :: MixMass             = 0.0d0 ! Mixing mass of air {kg}
  REAL(r64) :: MixHeatLoss         = 0.0d0 ! Heat Gain {J} due to mixing and cross mixing and refrigeration door mixing
  REAL(r64) :: MixHeatGain         = 0.0d0 ! Heat Loss {J} due to mixing and cross mixing and refrigeration door mixing
  REAL(r64) :: MixLatentLoss       = 0.0d0 ! Latent Gain {J} due to mixing and cross mixing and refrigeration door mixing
  REAL(r64) :: MixLatentGain       = 0.0d0 ! Latent Loss {J} due to mixing and cross mixing and refrigeration door mixing
  REAL(r64) :: MixTotalLoss        = 0.0d0 ! Total Gain {J} due to mixing and cross mixing and refrigeration door mixing
  REAL(r64) :: MixTotalGain        = 0.0d0 ! Total Loss {J} due to mixing and cross mixing and refrigeration door mixing
  ! air heat balance component load summary results
  REAL(r64) :: SumIntGains     = 0.0D0   ! Zone sum of convective internal gains
  REAL(r64) :: SumHADTsurfs    = 0.0D0   ! Zone sum of Hc*Area*(Tsurf - Tz)
  REAL(r64) :: SumMCpDTzones   = 0.0D0   ! zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
  REAL(r64) :: SumMCpDtInfil   = 0.0D0   ! Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
  REAL(r64) :: SumMCpDTsystem  = 0.0D0   ! Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
  REAL(r64) :: SumNonAirSystem = 0.0D0   ! Zone sum of system convective gains, collected via NonAirSystemResponse
  REAL(r64) :: CzdTdt          = 0.0D0   ! Zone air energy storage term.
  REAL(r64) :: imBalance       = 0.0D0   ! put all terms in eq. 5 on RHS , should be zero
  ! for ZoneAirBalance:OutdoorAir object Outputs only
  REAL(r64) :: OABalanceHeatLoss      = 0.0d0 ! Heat Gain {J} due to OA air balance
  REAL(r64) :: OABalanceHeatGain      = 0.0d0 ! Heat Loss {J} due to OA air balance
  REAL(r64) :: OABalanceLatentLoss    = 0.0d0 ! Latent Gain {J} due to OA air balance
  REAL(r64) :: OABalanceLatentGain    = 0.0d0 ! Latent Loss {J} due to OA air balance
  REAL(r64) :: OABalanceTotalLoss     = 0.0d0 ! Total Gain {J} due to OA air balance
  REAL(r64) :: OABalanceTotalGain     = 0.0d0 ! Total Loss {J} due to OA air balance
  REAL(r64) :: OABalanceVolumeCurDensity = 0.0d0 ! Volume of Air {m3} due to OA air balance
                                              ! at current zone air density
  REAL(r64) :: OABalanceVolumeStdDensity = 0.0d0 ! Volume of Air {m3} due to OA air balance
                                              ! at standard density (adjusted for elevation)
  REAL(r64) :: OABalanceVdotCurDensity = 0.0d0 ! Volume flow rate of Air {m3/s} due to OA air balance
                                            ! at current zone air density
  REAL(r64) :: OABalanceVdotStdDensity = 0.0d0 ! Volume flow rate of Air {m3/s} due to OA air balance
                                            ! at standard density (adjusted elevation)
  REAL(r64) :: OABalanceMass          = 0.0d0 ! Mass of Air {kg} due to OA air balance
  REAL(r64) :: OABalanceAirChangeRate = 0.0d0 ! OA air balance air change rate (ach)
  REAL(r64) :: OABalanceFanElec       = 0.0d0 ! Fan Electricity {W} due to OA air balance
END TYPE

  ! For predefined tabular reporting
TYPE ZonePreDefRepType
  LOGICAL :: isOccupied               = .false. !occupied during the current time step
  REAL(r64) :: NumOccAccum                 = 0.0d0     !number of occupants accumulating for entire simulation
  REAL(r64) :: NumOccAccumTime             = 0.0d0     !time that the number of occupants is accumulating to compute average
                                                !  - zone time step
  REAL(r64) :: TotTimeOcc                  = 0.0d0     !time occuped (and the mechnical ventilation volume is accumulating)
                                                !  - system time step
  REAL(r64) :: MechVentVolTotal            = 0.0d0     !volume for mechnical ventilation of outside air for entire simulation
  REAL(r64) :: MechVentVolMin              = 9.9d9   ! a large number since finding minimum volume
  REAL(r64) :: InfilVolTotal               = 0.0d0     !volume for infiltration of outside air for entire simulation
  REAL(r64) :: InfilVolMin                 = 9.9d9   ! a large number since finding minimum volume
  REAL(r64) :: AFNInfilVolTotal            = 0.0d0     !volume for infiltration of outside air for entire simulation
  REAL(r64) :: AFNInfilVolMin              = 9.9d9   ! a large number since finding minimum volume
  REAL(r64) :: SimpVentVolTotal            = 0.0d0     !volume for simple 'ZoneVentilation' of outside air for entire simulation
  REAL(r64) :: SimpVentVolMin              = 9.9d9   ! a large number since finding minimum volume
  ! for Sensible Heat Gas Component Report
  !annual
  REAL(r64) :: SHGSAnHvacHt                = 0.0d0     ! hvac air heating
  REAL(r64) :: SHGSAnHvacCl                = 0.0d0     ! hvac air cooling
  REAL(r64) :: SHGSAnSurfHt                = 0.0d0     ! heated surface heating
  REAL(r64) :: SHGSAnSurfCl                = 0.0d0     ! cooled surface cooling
  REAL(r64) :: SHGSAnPeoplAdd              = 0.0d0     ! people additions
  REAL(r64) :: SHGSAnLiteAdd               = 0.0d0     ! lighing addition
  REAL(r64) :: SHGSAnEquipAdd              = 0.0d0     ! equipment addition
  REAL(r64) :: SHGSAnWindAdd               = 0.0d0     ! window addition
  REAL(r64) :: SHGSAnIzaAdd                = 0.0d0     ! inter zone air addition
  REAL(r64) :: SHGSAnInfilAdd              = 0.0d0     ! infiltration addition
  REAL(r64) :: SHGSAnOtherAdd              = 0.0d0     ! opaque surface and other addition
  REAL(r64) :: SHGSAnEquipRem              = 0.0d0     ! equipment removal
  REAL(r64) :: SHGSAnWindRem               = 0.0d0     ! window removal
  REAL(r64) :: SHGSAnIzaRem                = 0.0d0     ! inter-zone air removal
  REAL(r64) :: SHGSAnInfilRem              = 0.0d0     ! infiltration removal
  REAL(r64) :: SHGSAnOtherRem              = 0.0d0     ! opaque surface and other removal
  !peak cooling
  INTEGER   :: clPtTimeStamp               = 0       ! timestamp for the cooling peak
  REAL(r64) :: clPeak                      = 0.0d0     ! cooling peak value (hvac air cooling + cooled surface)
  REAL(r64) :: SHGSClHvacHt                = 0.0d0     ! hvac air heating
  REAL(r64) :: SHGSClHvacCl                = 0.0d0     ! hvac air cooling
  REAL(r64) :: SHGSClSurfHt                = 0.0d0     ! heated surface heating
  REAL(r64) :: SHGSClSurfCl                = 0.0d0     ! cooled surface cooling
  REAL(r64) :: SHGSClPeoplAdd              = 0.0d0     ! people additions
  REAL(r64) :: SHGSClLiteAdd               = 0.0d0     ! lighing addition
  REAL(r64) :: SHGSClEquipAdd              = 0.0d0     ! equipment addition
  REAL(r64) :: SHGSClWindAdd               = 0.0d0     ! window addition
  REAL(r64) :: SHGSClIzaAdd                = 0.0d0     ! inter zone air addition
  REAL(r64) :: SHGSClInfilAdd              = 0.0d0     ! infiltration addition
  REAL(r64) :: SHGSClOtherAdd              = 0.0d0     ! opaque surface and other addition
  REAL(r64) :: SHGSClEquipRem              = 0.0d0     ! equipment removal
  REAL(r64) :: SHGSClWindRem               = 0.0d0     ! window removal
  REAL(r64) :: SHGSClIzaRem                = 0.0d0     ! inter-zone air removal
  REAL(r64) :: SHGSClInfilRem              = 0.0d0     ! infiltration removal
  REAL(r64) :: SHGSClOtherRem              = 0.0d0     ! opaque surface and other removal
  !peak heating
  INTEGER   :: htPtTimeStamp               = 0       ! timestamp for the heating peak
  REAL(r64) :: htPeak                      = 0.0d0     ! heating peak value (hvac air heating + heated surface)
  REAL(r64) :: SHGSHtHvacHt                = 0.0d0     ! hvac air heating
  REAL(r64) :: SHGSHtHvacCl                = 0.0d0     ! hvac air cooling
  REAL(r64) :: SHGSHtSurfHt                = 0.0d0     ! heated surface heating
  REAL(r64) :: SHGSHtSurfCl                = 0.0d0     ! cooled surface cooling
  REAL(r64) :: SHGSHtPeoplAdd              = 0.0d0     ! people additions
  REAL(r64) :: SHGSHtLiteAdd               = 0.0d0     ! lighing addition
  REAL(r64) :: SHGSHtEquipAdd              = 0.0d0     ! equipment addition
  REAL(r64) :: SHGSHtWindAdd               = 0.0d0     ! window addition
  REAL(r64) :: SHGSHtIzaAdd                = 0.0d0     ! inter zone air addition
  REAL(r64) :: SHGSHtInfilAdd              = 0.0d0     ! infiltration addition
  REAL(r64) :: SHGSHtOtherAdd              = 0.0d0     ! opaque surface and other addition
  REAL(r64) :: SHGSHtEquipRem              = 0.0d0     ! equipment removal
  REAL(r64) :: SHGSHtWindRem               = 0.0d0     ! window removal
  REAL(r64) :: SHGSHtIzaRem                = 0.0d0     ! inter-zone air removal
  REAL(r64) :: SHGSHtInfilRem              = 0.0d0     ! infiltration removal
  REAL(r64) :: SHGSHtOtherRem              = 0.0d0     ! opaque surface and other removal
END TYPE

  ! DERIVED TYPE DEFINITIONS:
TYPE ZoneReportVars  ! Zone level.
  ! People
  REAL(r64) :: PeopleRadGain                   = 0.0d0
  REAL(r64) :: PeopleConGain                   = 0.0d0
  REAL(r64) :: PeopleSenGain                   = 0.0d0
  REAL(r64) :: PeopleNumOcc                    = 0.0d0
  REAL(r64) :: PeopleLatGain                   = 0.0d0
  REAL(r64) :: PeopleTotGain                   = 0.0d0
  REAL(r64) :: PeopleRadGainRate               = 0.0d0
  REAL(r64) :: PeopleConGainRate               = 0.0d0
  REAL(r64) :: PeopleSenGainRate               = 0.0d0
  REAL(r64) :: PeopleLatGainRate               = 0.0d0
  REAL(r64) :: PeopleTotGainRate               = 0.0d0
  ! Lights
  REAL(r64) :: LtsPower                        = 0.0d0
  REAL(r64) :: LtsElecConsump                  = 0.0d0
  REAL(r64) :: LtsRadGain                      = 0.0d0
  REAL(r64) :: LtsVisGain                      = 0.0d0
  REAL(r64) :: LtsConGain                      = 0.0d0
  REAL(r64) :: LtsRetAirGain                   = 0.0d0
  REAL(r64) :: LtsTotGain                      = 0.0d0
  REAL(r64) :: LtsRadGainRate                  = 0.0d0
  REAL(r64) :: LtsVisGainRate                  = 0.0d0
  REAL(r64) :: LtsConGainRate                  = 0.0d0
  REAL(r64) :: LtsRetAirGainRate               = 0.0d0
  REAL(r64) :: LtsTotGainRate                  = 0.0d0
  ! Baseboard Heat
  REAL(r64) :: BaseHeatPower                   = 0.0d0
  REAL(r64) :: BaseHeatElecCons                = 0.0d0
  REAL(r64) :: BaseHeatRadGain                 = 0.0d0
  REAL(r64) :: BaseHeatConGain                 = 0.0d0
  REAL(r64) :: BaseHeatTotGain                 = 0.0d0
  REAL(r64) :: BaseHeatRadGainRate             = 0.0d0
  REAL(r64) :: BaseHeatConGainRate             = 0.0d0
  REAL(r64) :: BaseHeatTotGainRate             = 0.0d0
  ! Electric Equipment
  REAL(r64) :: ElecPower                       = 0.0d0
  REAL(r64) :: ElecConsump                     = 0.0d0
  REAL(r64) :: ElecRadGain                     = 0.0d0
  REAL(r64) :: ElecConGain                     = 0.0d0
  REAL(r64) :: ElecLatGain                     = 0.0d0
  REAL(r64) :: ElecLost                        = 0.0d0
  REAL(r64) :: ElecTotGain                     = 0.0d0
  REAL(r64) :: ElecRadGainRate                 = 0.0d0
  REAL(r64) :: ElecConGainRate                 = 0.0d0
  REAL(r64) :: ElecLatGainRate                 = 0.0d0
  REAL(r64) :: ElecLostRate                    = 0.0d0
  REAL(r64) :: ElecTotGainRate                 = 0.0d0
  ! Gas Equipment
  REAL(r64) :: GasPower                        = 0.0d0
  REAL(r64) :: GasConsump                      = 0.0d0
  REAL(r64) :: GasRadGain                      = 0.0d0
  REAL(r64) :: GasConGain                      = 0.0d0
  REAL(r64) :: GasLatGain                      = 0.0d0
  REAL(r64) :: GasLost                         = 0.0d0
  REAL(r64) :: GasTotGain                      = 0.0d0
  REAL(r64) :: GasRadGainRate                  = 0.0d0
  REAL(r64) :: GasConGainRate                  = 0.0d0
  REAL(r64) :: GasLatGainRate                  = 0.0d0
  REAL(r64) :: GasLostRate                     = 0.0d0
  REAL(r64) :: GasTotGainRate                  = 0.0d0
  ! Hot Water Equipment
  REAL(r64) :: HWPower                         = 0.0d0
  REAL(r64) :: HWConsump                       = 0.0d0
  REAL(r64) :: HWRadGain                       = 0.0d0
  REAL(r64) :: HWConGain                       = 0.0d0
  REAL(r64) :: HWLatGain                       = 0.0d0
  REAL(r64) :: HWLost                          = 0.0d0
  REAL(r64) :: HWTotGain                       = 0.0d0
  REAL(r64) :: HWRadGainRate                   = 0.0d0
  REAL(r64) :: HWConGainRate                   = 0.0d0
  REAL(r64) :: HWLatGainRate                   = 0.0d0
  REAL(r64) :: HWLostRate                      = 0.0d0
  REAL(r64) :: HWTotGainRate                   = 0.0d0
  ! Steam Equipment
  REAL(r64) :: SteamPower                      = 0.0d0
  REAL(r64) :: SteamConsump                    = 0.0d0
  REAL(r64) :: SteamRadGain                    = 0.0d0
  REAL(r64) :: SteamConGain                    = 0.0d0
  REAL(r64) :: SteamLatGain                    = 0.0d0
  REAL(r64) :: SteamLost                       = 0.0d0
  REAL(r64) :: SteamTotGain                    = 0.0d0
  REAL(r64) :: SteamRadGainRate                = 0.0d0
  REAL(r64) :: SteamConGainRate                = 0.0d0
  REAL(r64) :: SteamLatGainRate                = 0.0d0
  REAL(r64) :: SteamLostRate                   = 0.0d0
  REAL(r64) :: SteamTotGainRate                = 0.0d0
  ! Other Equipment
  REAL(r64) :: OtherRadGain                    = 0.0d0
  REAL(r64) :: OtherConGain                    = 0.0d0
  REAL(r64) :: OtherLatGain                    = 0.0d0
  REAL(r64) :: OtherLost                       = 0.0d0
  REAL(r64) :: OtherTotGain                    = 0.0d0
  REAL(r64) :: OtherRadGainRate                = 0.0d0
  REAL(r64) :: OtherConGainRate                = 0.0d0
  REAL(r64) :: OtherLatGainRate                = 0.0d0
  REAL(r64) :: OtherLostRate                   = 0.0d0
  REAL(r64) :: OtherTotGainRate                = 0.0d0
  ! Overall Zone Variables
  REAL(r64) :: TotRadiantGain                  = 0.0d0
  REAL(r64) :: TotVisHeatGain                  = 0.0d0
  REAL(r64) :: TotConvectiveGain               = 0.0d0
  REAL(r64) :: TotLatentGain                   = 0.0d0
  REAL(r64) :: TotTotalHeatGain                = 0.0d0
  REAL(r64) :: TotRadiantGainRate              = 0.0d0
  REAL(r64) :: TotVisHeatGainRate              = 0.0d0
  REAL(r64) :: TotConvectiveGainRate           = 0.0d0
  REAL(r64) :: TotLatentGainRate               = 0.0d0
  REAL(r64) :: TotTotalHeatGainRate            = 0.0d0
  ! Contaminant
  REAL(r64) :: CO2Rate                         = 0.0d0
  REAL(r64) :: GCRate                          = 0.0d0
END TYPE

  ! MODULE VARIABLE DECLARATIONS:
TYPE (ZonePreDefRepType),      ALLOCATABLE, DIMENSION(:) :: ZonePreDefRep
TYPE (ZonePreDefRepType),SAVE :: BuildingPreDefRep=  &
   ZonePreDefRepType(  &
      .false.,  &   !occupied during the current time step
      0.0,   &      !number of occupants accumulating for entire simulation
      0.0,   &      !time that the number of occupants is accumulating to compute average - zone time step
      0.0,   &      !time occuped (and the mechnical ventilation volume is accumulating) - system time step
      0.0,   &      !volume for mechnical ventilation of outside air for entire simulation
      9.9d9, &      ! a large number since finding minimum volume
      0.0,   &      !volume for infiltration of outside air for entire simulation
      9.9d9, &      ! a large number since finding minimum volume
      0.0,   &      !volume for infiltration of outside air for entire simulation
      9.9d9, &      ! a large number since finding minimum volume
      0.0,   &      !volume for simple 'ZoneVentilation' of outside air for entire simulation
      9.9d9, &      ! a large number since finding minimum volume
  ! for Sensible Heat Gas Component Report
  !    annual:
      0.0d0,  &       ! hvac air heating
      0.0d0,  &       ! hvac air cooling
      0.0d0,  &       ! heated surface heating
      0.0d0,  &       ! cooled surface cooling
      0.0d0,  &       ! people additions
      0.0d0,  &       ! lighing addition
      0.0d0,  &       ! equipment addition
      0.0d0,  &       ! window addition
      0.0d0,  &       ! inter zone air addition
      0.0d0,  &       ! infiltration addition
      0.0d0,  &       ! opaque surface and other addition
      0.0d0,  &       ! equipment removal
      0.0d0,  &       ! window removal
      0.0d0,  &       ! inter-zone air removal
      0.0d0,  &       ! infiltration removal
      0.0d0,  &       ! opaque surface and other removal
      !  peak cooling:
      0,  &         ! timestamp for the cooling peak
      0.0d0,  &       ! cooling peak value (hvac air cooling + cooled surface)
      0.0d0,  &       ! hvac air heating
      0.0d0,  &       ! hvac air cooling
      0.0d0,  &       ! heated surface heating
      0.0d0,  &       ! cooled surface cooling
      0.0d0,  &       ! people additions
      0.0d0,  &       ! lighing addition
      0.0d0,  &       ! equipment addition
      0.0d0,  &       ! window addition
      0.0d0,  &       ! inter zone air addition
      0.0d0,  &       ! infiltration addition
      0.0d0,  &       ! opaque surface and other addition
      0.0d0,  &       ! equipment removal
      0.0d0,  &       ! window removal
      0.0d0,  &       ! inter-zone air removal
      0.0d0,  &       ! infiltration removal
      0.0d0,  &       ! opaque surface and other removal
      !  peak heating:
      0,  &         ! timestamp for the heating peak
      0.0d0,  &       ! heating peak value (hvac air heating + heated surface)
      0.0d0,  &       ! hvac air heating
      0.0d0,  &       ! hvac air cooling
      0.0d0,  &       ! heated surface heating
      0.0d0,  &       ! cooled surface cooling
      0.0d0,  &       ! people additions
      0.0d0,  &       ! lighing addition
      0.0d0,  &       ! equipment addition
      0.0d0,  &       ! window addition
      0.0d0,  &       ! inter zone air addition
      0.0d0,  &       ! infiltration addition
      0.0d0,  &       ! opaque surface and other addition
      0.0d0,  &       ! equipment removal
      0.0d0,  &       ! window removal
      0.0d0,  &       ! inter-zone air removal
      0.0d0,  &       ! infiltration removal
      0.0d0)          ! opaque surface and other removal

! MODULE VARIABLE Type DECLARATIONS:
TYPE (ZoneSimData),            ALLOCATABLE, DIMENSION(:) :: ZoneIntGain
TYPE (MaterialProperties),     ALLOCATABLE, DIMENSION(:) :: Material
TYPE (GapSupportPillar),       ALLOCATABLE, DIMENSION(:) :: SupportPillar
TYPE (GapDeflectionState),     ALLOCATABLE, DIMENSION(:) :: DeflectionState
TYPE (ConstructionData),       ALLOCATABLE, DIMENSION(:) :: Construct
TYPE (SpectralDataProperties), ALLOCATABLE, DIMENSION(:) :: SpectralData
TYPE (ZoneData),               ALLOCATABLE, DIMENSION(:) :: Zone
TYPE (ZoneListData),           ALLOCATABLE, DIMENSION(:) :: ZoneList
TYPE (ZoneGroupData),          ALLOCATABLE, DIMENSION(:) :: ZoneGroup
TYPE (PeopleData),             ALLOCATABLE, DIMENSION(:) :: People
TYPE (LightsData),             ALLOCATABLE, DIMENSION(:) :: Lights
TYPE (ZoneEquipData),          ALLOCATABLE, DIMENSION(:) :: ZoneElectric
TYPE (ZoneEquipData),          ALLOCATABLE, DIMENSION(:) :: ZoneGas
TYPE (ZoneEquipData),          ALLOCATABLE, DIMENSION(:) :: ZoneOtherEq
TYPE (ZoneEquipData),          ALLOCATABLE, DIMENSION(:) :: ZoneHWEq
TYPE (ZoneEquipData),          ALLOCATABLE, DIMENSION(:) :: ZoneSteamEq
TYPE (BBHeatData),             ALLOCATABLE, DIMENSION(:) :: ZoneBBHeat
TYPE (InfiltrationData),       ALLOCATABLE, DIMENSION(:) :: Infiltration
TYPE (VentilationData),        ALLOCATABLE, DIMENSION(:) :: Ventilation
TYPE (ZoneAirBalanceData),     ALLOCATABLE, DIMENSION(:) :: ZoneAirBalance
TYPE (MixingData),             ALLOCATABLE, DIMENSION(:) :: Mixing
TYPE (MixingData),             ALLOCATABLE, DIMENSION(:) :: CrossMixing
TYPE (MixingData),             ALLOCATABLE, DIMENSION(:) :: RefDoorMixing
TYPE (WindowBlindProperties),  ALLOCATABLE, DIMENSION(:) :: Blind
TYPE (WindowComplexShade),     ALLOCATABLE, DIMENSION(:) :: ComplexShade
TYPE (WindowThermalModelParams), ALLOCATABLE, DIMENSION(:) :: WindowThermalModel
TYPE (SurfaceScreenProperties), ALLOCATABLE, DIMENSION(:) :: SurfaceScreens
TYPE (ScreenTransData),        ALLOCATABLE, DIMENSION(:) :: ScreenTrans
TYPE (MaterialProperties),     ALLOCATABLE, DIMENSION(:) :: MaterialSave
TYPE (ConstructionData),       ALLOCATABLE, DIMENSION(:) :: ConstructSave
TYPE (ZoneCatEUseData),        ALLOCATABLE, DIMENSION(:) :: ZoneIntEEuse
TYPE (RefrigCaseCreditData),   ALLOCATABLE, DIMENSION(:) :: RefrigCaseCredit
TYPE (HeatReclaimRefrigeratedRackData) , ALLOCATABLE, DIMENSION(:) :: HeatReclaimRefrigeratedRack
TYPE (HeatReclaimRefrigCondenserData) , ALLOCATABLE, DIMENSION(:) :: HeatReclaimRefrigCondenser
TYPE (HeatReclaimDXCoilData) , ALLOCATABLE, DIMENSION(:) :: HeatReclaimDXCoil
TYPE (AirReportVars), ALLOCATABLE, DIMENSION(:) :: ZnAirRpt
TYPE (TCGlazingsType), ALLOCATABLE, DIMENSION(:) :: TCGlazings
TYPE (ZoneEquipData),          ALLOCATABLE, DIMENSION(:) :: ZoneCO2Gen
TYPE (GlobalInternalGainMiscObject), ALLOCATABLE, DIMENSION(:) :: PeopleObjects
TYPE (GlobalInternalGainMiscObject), ALLOCATABLE, DIMENSION(:) :: LightsObjects
TYPE (GlobalInternalGainMiscObject), ALLOCATABLE, DIMENSION(:) :: ZoneElectricObjects
TYPE (GlobalInternalGainMiscObject), ALLOCATABLE, DIMENSION(:) :: ZoneGasObjects
TYPE (GlobalInternalGainMiscObject), ALLOCATABLE, DIMENSION(:) :: HotWaterEqObjects
TYPE (GlobalInternalGainMiscObject), ALLOCATABLE, DIMENSION(:) :: SteamEqObjects
TYPE (GlobalInternalGainMiscObject), ALLOCATABLE, DIMENSION(:) :: OtherEqObjects
TYPE (GlobalInternalGainMiscObject), ALLOCATABLE, DIMENSION(:) :: InfiltrationObjects
TYPE (GlobalInternalGainMiscObject), ALLOCATABLE, DIMENSION(:) :: VentilationObjects
TYPE (ZoneReportVars), ALLOCATABLE, PUBLIC, DIMENSION(:) :: ZnRpt


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:

! SiteData aka building data
REAL(r64) :: LowHConvLimit = 0.1d0 ! Lowest allowed convection coefficient for detailed model
                                       ! before reverting to the simple model.  This avoids a
                                       ! divide by zero elsewhere.  Not based on any physical
                                       ! reasoning, just the number that was picked.  It corresponds
                                       ! to a delta T for a vertical surface of 0.000444C.
!!REAL(r64), PARAMETER :: LowHConvLimit = 1.0 !W/m2-K  Lowest allowed natural convection coefficient
!!                           ! A lower limit is needed to avoid numerical problems
!!                           ! Natural convection correlations are a function of temperature difference,
!!                           !   there are many times when those temp differences pass through zero leading to non-physical results
!!                           ! Value of 1.0 chosen here is somewhat arbitrary, but based on the following reasons:
!!                           !  1) Low values of HconvIn indicate a layer of high thermal resistance, however
!!                           !       the R-value of a convection film layer should be relatively low (compared to building surfaces)
!!                           !  2) The value of 1.0 corresponds to the thermal resistance of 0.05 m of batt insulation
!!                           !  3) Limit on the order of 1.0 is suggested by the abrupt changes in an inverse relationship
!!                           !  4) A conduction-only analysis can model a limit by considering the thermal performance of
!!                           !       boundary layer to be pure conduction (with no movement to enhance heat transfer);
!!                           !       Taking the still gas thermal conductivity for air at 0.0267 W/m-K (at 300K), then
!!                           !       this limit of 1.0 corresponds to a completely still layer of air that is around 0.025 m thick
!!                           !  5) The previous limit of 0.1 (before ver. 3.1) caused loads initialization problems in test files
REAL(r64)  :: HighHConvLimit = 1000.d0 ! upper limit for Hconv, mostly used for user input limits in practics. !W/m2-K
REAL(r64)  :: MaxAllowedDelTempCondFD = 0.002d0 ! Convergence criteria for inside surface temperatures for CondFD



CHARACTER(len=MaxNameLength) :: BuildingName=' '  ! Name of building
REAL(r64) :: BuildingAzimuth=0.0d0                       ! North Axis of Building
REAL(r64) :: LoadsConvergTol=0.0d0                       ! Tolerance value for Loads Convergence
REAL(r64) :: TempConvergTol=0.0d0                        ! Tolerance value for Temperature Convergence
INTEGER :: DefaultInsideConvectionAlgo=1          ! 1 = simple (ASHRAE); 2 = detailed (ASHRAE); 3 = ceiling diffuser;
                                                  ! 4 = trombe wall
INTEGER :: DefaultOutsideConvectionAlgo=1         ! 1 = simple (ASHRAE); 2 = detailed; etc (BLAST, TARP, MOWITT, DOE-2)
INTEGER :: SolarDistribution=0                    ! Solar Distribution Algorithm
INTEGER :: InsideSurfIterations=0                 ! Counts inside surface iterations
INTEGER :: OverallHeatTransferSolutionAlgo = UseCTF  ! UseCTF Solution, UseEMPD moisture solution, UseCondFD solution
INTEGER :: NumberOfHeatTransferAlgosUsed = 1
INTEGER, DIMENSION(:), ALLOCATABLE :: HeatTransferAlgosUsed
INTEGER :: MaxNumberOfWarmupDays=25               ! Maximum number of warmup days allowed
INTEGER :: MinNumberOfWarmupDays=6                ! Minimum number of warmup days allowed
REAL(r64) :: CondFDRelaxFactor = 1.0d0  ! Relaxation factor, for looping across all the surfaces.
REAL(r64) :: CondFDRelaxFactorInput = 1.0d0  ! Relaxation factor, for looping across all the surfaces, user input value
!LOGICAL ::  CondFDVariableProperties = .FALSE. ! if true, then variable conductivity or enthalpy in Cond FD.

INTEGER :: ZoneAirSolutionAlgo = Use3rdOrder      ! ThirdOrderBackwardDifference, AnalyticalSolution, and EulerMethod
REAL(r64) :: BuildingRotationAppendixG=0.0d0        ! Building Rotation for Appendix G

!END SiteData

INTEGER :: NumOfZoneLists   =0 ! Total number of zone lists
INTEGER :: NumOfZoneGroups  =0 ! Total number of zone groups
INTEGER :: NumPeopleStatements = 0 ! Number of People objects in input - possibly global assignments
INTEGER :: NumLightsStatements = 0 ! Number of Lights objects in input - possibly global assignments
INTEGER :: NumZoneElectricStatements = 0 ! Number of ZoneElectric objects in input - possibly global assignments
INTEGER :: NumZoneGasStatements = 0 ! Number of ZoneGas objects in input - possibly global assignments
INTEGER :: NumInfiltrationStatements = 0 ! Number of Design Flow Infiltration objects in input - possibly global assignments
INTEGER :: NumVentilationStatements = 0 ! Number of Design Flow Ventilation objects in input - possibly global assignments
INTEGER :: NumHotWaterEqStatements = 0 ! number of Hot Water Equipment objects in input. - possibly global assignments
INTEGER :: NumSteamEqStatements = 0 ! number of Steam Equipment objects in input. - possibly global assignments
INTEGER :: NumOtherEqStatements = 0 ! number of Other Equipment objects in input. - possibly global assignments
INTEGER :: TotPeople        =0 ! Total People Statements in input and extrapolated from global assignments
INTEGER :: TotLights        =0 ! Total Lights Statements in input and extrapolated from global assignments
INTEGER :: TotElecEquip     =0 ! Total Electric Equipment Statements in input and extrapolated from global assignments
INTEGER :: TotGasEquip      =0 ! Total Gas Equipment Statements in input
INTEGER :: TotOthEquip      =0 ! Total Other Equipment Statements in input
INTEGER :: TotHWEquip       =0 ! Total Hot Water Equipment Statements in input
INTEGER :: TotStmEquip      =0 ! Total Steam Equipment Statements in input
INTEGER :: TotInfiltration  =0 ! Total Infiltration Statements in input and extrapolated from global assignments
INTEGER :: TotDesignFlowInfiltration = 0 ! number of Design Flow rate ZoneInfiltration in input
INTEGER :: TotShermGrimsInfiltration = 0 ! number of Sherman Grimsrud (ZoneInfiltration:ResidentialBasic) in input
INTEGER :: TotAIM2Infiltration = 0 ! number of AIM2 (ZoneInfiltration:ResidentialEnhanced) in input
INTEGER :: TotVentilation   =0 ! Total Ventilation Statements in input
INTEGER :: TotDesignFlowVentilation = 0 ! number of Design Flow rate ZoneVentilation in input
INTEGER :: TotWindAndStackVentilation = 0 ! number of wind and stack open area ZoneVentilation in input
INTEGER :: TotMixing        =0 ! Total Mixing Statements in input
INTEGER :: TotCrossMixing   =0 ! Total Cross Mixing Statements in input
INTEGER :: TotRefDoorMixing =0 ! Total RefrigerationDoor Mixing Statements in input
INTEGER :: TotBBHeat        =0 ! Total BBHeat Statements in input
INTEGER :: TotMaterials     =0 ! Total number of unique materials (layers) in this simulation
INTEGER :: TotConstructs    =0 ! Total number of unique constructions in this simulation
INTEGER :: TotSpectralData  =0 ! Total window glass spectral data sets
INTEGER :: W5GlsMat         =0 ! Window5 Glass Materials, specified by transmittance and front and back reflectance
INTEGER :: W5GlsMatAlt      =0 ! Window5 Glass Materials, specified by index of refraction and extinction coeff
INTEGER :: W5GasMat         =0 ! Window5 Single-Gas Materials
INTEGER :: W5GasMatMixture  =0 ! Window5 Gas Mixtures
INTEGER :: W7SupportPillars =0 ! Complex fenestration support pillars
INTEGER :: W7DeflectionStates =0 ! Complex fenestration deflection states
INTEGER :: W7MaterialGaps =0   ! Complex fenestration material gaps
INTEGER :: TotBlinds        =0 ! Total number of blind materials
INTEGER :: TotScreens       =0 ! Total number of exterior window screen materials
INTEGER :: TotTCGlazings = 0     ! Number of TC glazing object - WindowMaterial:Glazing:Thermochromic found in the idf file
INTEGER :: NumSurfaceScreens=0 ! Total number of screens on exterior windows
INTEGER :: TotShades        =0 ! Total number of shade materials
INTEGER :: TotComplexShades =0 ! Total number of shading materials for complex fenestrations
INTEGER :: TotComplexGaps    =0 ! Total number of window gaps for complex fenestrations
INTEGER :: TotSimpleWindow ! number of simple window systems.

INTEGER :: W5GlsMatEQL      =0  ! Window5 Single-Gas Materials for Equivalent Layer window model
INTEGER :: TotShadesEQL     =0  ! Total number of shade materials for Equivalent Layer window model
INTEGER :: TotDrapesEQL     =0  ! Total number of drape materials for Equivalent Layer window model
INTEGER :: TotBlindsEQL     =0  ! Total number of blind materials for Equivalent Layer window model
INTEGER :: TotScreensEQL    =0  ! Total number of exterior window screen materials for Equivalent Layer window model
INTEGER :: W5GapMatEQL      =0 ! Window5 Equivalent Layer Single-Gas Materials

INTEGER :: TotZoneAirBalance   =0 ! Total Zone Air Balance Statements in input
INTEGER :: TotFrameDivider  =0 ! Total number of window frame/divider objects
INTEGER :: AirFlowFlag=0
INTEGER :: TotCO2Gen        =0 ! Total CO2 source and sink statements in input
LOGICAL :: CalcWindowRevealReflection=.false. ! True if window reveal reflection is to be calculated
                                              ! for at least one exterior window
LOGICAL :: StormWinChangeThisDay=.false. ! True if a storm window has been added or removed from any
                                        ! window during the current day; can only be true for first
                                        ! time step of the day.
LOGICAL :: AdaptiveComfortRequested_CEN15251 = .false.  ! true if people objects have adaptive comfort requests. CEN15251
LOGICAL :: AdaptiveComfortRequested_ASH55    = .false.  ! true if people objects have adaptive comfort requests. ASH55
INTEGER :: NumRefrigeratedRacks =0 ! Total number of refrigerated case compressor racks in input
INTEGER :: NumRefrigSystems =0     ! Total number of detailed refrigeration systems in input
INTEGER :: NumRefrigCondensers =0  ! Total number of detailed refrigeration condensers in input
INTEGER :: NumRefrigChillerSets =0    ! Total number of refrigerated warehouse coils in input
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SNLoadHeatEnergy
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SNLoadCoolEnergy
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SNLoadHeatRate
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SNLoadCoolRate
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SNLoadPredictedRate
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SNLoadPredictedHSPRate ! Predicted load to heating setpoint (unmultiplied)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SNLoadPredictedCSPRate ! Predicted load to cooling setpoint (unmultiplied)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MoisturePredictedRate

REAL(r64), ALLOCATABLE, DIMENSION(:) :: ListSNLoadHeatEnergy
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ListSNLoadCoolEnergy
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ListSNLoadHeatRate
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ListSNLoadCoolRate

REAL(r64), ALLOCATABLE, DIMENSION(:) :: GroupSNLoadHeatEnergy
REAL(r64), ALLOCATABLE, DIMENSION(:) :: GroupSNLoadCoolEnergy
REAL(r64), ALLOCATABLE, DIMENSION(:) :: GroupSNLoadHeatRate
REAL(r64), ALLOCATABLE, DIMENSION(:) :: GroupSNLoadCoolRate

REAL(r64), ALLOCATABLE, DIMENSION(:) :: MRT   !MEAN RADIANT TEMPERATURE (C)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SUMAI               !1 over the Sum of zone areas or 1/SumA
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneTransSolar      ! Exterior beam plus diffuse solar entering zone;
                                                       !   sum of WinTransSolar for exterior windows in zone (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneWinHeatGain     ! Heat gain to zone from all exterior windows (includes
                                                       !   ZoneTransSolar); sum of WinHeatGain for exterior
                                                       !   windows in zone (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneWinHeatGainRep  ! = ZoneWinHeatGain when ZoneWinHeatGain >= 0
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneWinHeatLossRep  ! = -ZoneWinHeatGain when ZoneWinHeatGain < 0
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneBmSolFrExtWinsRep  ! Beam solar into zone from exterior windows [W]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneBmSolFrIntWinsRep  ! Beam solar into zone from interior windows [W]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: InitialZoneDifSolReflW   ! Initial diffuse solar in zone from ext and int windows
                                                            ! reflected from interior surfaces [W]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneDifSolFrExtWinsRep ! Diffuse solar into zone from exterior windows [W]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneDifSolFrIntWinsRep ! Diffuse solar into zone from interior windows [W]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOpaqSurfInsFaceCond ! Zone inside face opaque surface conduction (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOpaqSurfInsFaceCondGainRep ! = Zone inside face opaque surface conduction when >= 0
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOpaqSurfInsFaceCondLossRep ! = -Zone inside face opaque surface conduction when < 0
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOpaqSurfExtFaceCond ! Zone outside face opaque surface conduction (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOpaqSurfExtFaceCondGainRep ! = Zone outside face opaque surface conduction when >= 0
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOpaqSurfExtFaceCondLossRep ! = -Zone outside face opaque surface conduction when < 0
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadThermInAbs      !Thermal radiation absorbed on inside surfaces
REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: QRadSWwinAbs      !Short wave radiation absorbed in window glass layers
REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: InitialDifSolwinAbs !Initial diffuse solar absorbed in window glass layers
                                                              ! from inside(W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWOutIncident   !Exterior beam plus diffuse solar incident on surface (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWOutIncidentBeam !Exterior beam solar incident on surface (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: BmIncInsSurfIntensRep !Beam sol irrad from ext wins on inside of surface (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: BmIncInsSurfAmountRep !Beam sol amount from ext wins incident on inside of surface (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: IntBmIncInsSurfIntensRep !Beam sol irrad from int wins on inside of surface (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: IntBmIncInsSurfAmountRep !Beam sol amount from int wins incident on inside of surface (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWOutIncidentSkyDiffuse !Exterior sky diffuse solar incident on surface (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWOutIncidentGndDiffuse !Exterior ground diffuse solar incident on surface (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWOutIncBmToDiffReflGnd !Exterior diffuse solar incident from beam to diffuse
                                                               ! reflection from ground (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWOutIncSkyDiffReflGnd  !Exterior diffuse solar incident from sky diffuse
                                                              ! reflection from ground (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWOutIncBmToBmReflObs   !Exterior beam solar incident from beam-to-beam
                                                               ! reflection from obstructions (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWOutIncBmToDiffReflObs !Exterior diffuse solar incident from beam-to-diffuse
                                                               ! reflection from obstructions (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWOutIncSkyDiffReflObs  !Exterior diffuse solar incident from sky diffuse
                                                               ! reflection from obstructions (W/m2)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: CosIncidenceAngle   !Cosine of beam solar incidence angle (for reporting)
INTEGER, ALLOCATABLE, DIMENSION(:) :: BSDFBeamDirectionRep  ! BSDF beam direction number for given complex fenestration state (for reporting) []
REAL(r64), ALLOCATABLE, DIMENSION(:) :: BSDFBeamThetaRep  ! BSDF beam Theta angle (for reporting) [rad]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: BSDFBeamPhiRep  ! BSDF beam Phi angle (for reporting) [rad]

REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWwinAbsTot     !Exterior beam plus diffuse solar absorbed in glass layers of window (W)
                                                              !energy
REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: QRadSWwinAbsLayer  ! Exterior beam plus diffuse solar absorbed in glass layers of window
                                                              ! energy (W)
REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: FenLaySurfTempFront    ! Front surface temperatures of fenestration layers
REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: FenLaySurfTempBack    ! Back surface temperatures of fenestration layers
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneTransSolarEnergy ! Energy of ZoneTransSolar [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneWinHeatGainRepEnergy  ! Energy of ZoneWinHeatGainRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneWinHeatLossRepEnergy  ! Energy of ZoneWinHeatLossRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneBmSolFrExtWinsRepEnergy  ! Energy of ZoneBmSolFrExtWinsRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneBmSolFrIntWinsRepEnergy  ! Energy of ZoneBmSolFrIntWinsRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneDifSolFrExtWinsRepEnergy ! Energy of ZoneDifSolFrExtWinsRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneDifSolFrIntWinsRepEnergy ! Energy of ZoneDifSolFrIntWinsRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZnOpqSurfInsFaceCondGnRepEnrg ! Energy of ZoneOpaqSurfInsFaceCondGainRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZnOpqSurfInsFaceCondLsRepEnrg ! Energy of ZoneOpaqSurfInsFaceCondLossRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZnOpqSurfExtFaceCondGnRepEnrg ! Energy of ZoneOpaqSurfInsFaceCondGainRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZnOpqSurfExtFaceCondLsRepEnrg ! Energy of ZoneOpaqSurfInsFaceCondLossRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: BmIncInsSurfAmountRepEnergy !energy of BmIncInsSurfAmountRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: IntBmIncInsSurfAmountRepEnergy !energy of IntBmIncInsSurfAmountRep [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSWwinAbsTotEnergy     ! Energy of QRadSWwinAbsTot [J]
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SWwinAbsTotalReport  !Report - Total interior/exterior shortwave
                                                        !absorbed in all glass layers of window (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: InitialDifSolInAbsReport  !Report - Initial transmitted diffuse solar
                                                             !absorbed on inside of surface (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: InitialDifSolInTransReport  !Report - Initial transmitted diffuse solar
                                                               !transmitted out through inside of window surface (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SWInAbsTotalReport  !Report - Total interior/exterior shortwave
                                                       !absorbed on inside of surface (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SWOutAbsTotalReport  !Report - Total exterior shortwave/solar
                                                       !absorbed on outside of surface (W)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SWOutAbsEnergyReport  !Report - Total exterior shortwave/solar
                                                       !absorbed on outside of surface (j)

REAL(r64), ALLOCATABLE, DIMENSION(:) :: NominalR ! Nominal R value of each material -- used in matching interzone surfaces
REAL(r64), ALLOCATABLE, DIMENSION(:) :: NominalRSave
REAL(r64), ALLOCATABLE, DIMENSION(:) :: NominalRforNominalUCalculation ! Nominal R values are summed to calculate NominalU values
                                                                       ! for constructions.
REAL(r64), ALLOCATABLE, DIMENSION(:) :: NominalU ! Nominal U value for each construction -- used in matching interzone surfaces
REAL(r64), ALLOCATABLE, DIMENSION(:) :: NominalUSave

! removed variables (these were all arrays):
!REAL(r64), ALLOCATABLE, :: DifIncInsSurfIntensRep    !Diffuse sol irradiance from ext wins on inside of surface (W/m2)
!REAL(r64), ALLOCATABLE, :: DifIncInsSurfAmountRep    !Diffuse sol amount from ext wins on inside of surface (W)
!REAL(r64), ALLOCATABLE, :: IntDifIncInsSurfIntensRep    !Diffuse sol irradiance from int wins on inside of surface (W/m2)
!REAL(r64), ALLOCATABLE, :: IntDifIncInsSurfAmountRep    !Diffuse sol amount from int wins on inside of surface (W)
!REAL(r64), ALLOCATABLE, :: DifIncInsSurfAmountRepEnergy    !energy of DifIncInsSurfAmountRep [J]
!REAL(r64), ALLOCATABLE, :: IntDifIncInsSurfAmountRepEnergy    !energy of IntDifIncInsSurfAmountRep [J]

          ! Variables moved from HeatBalanceSurfaceManager and SolarShading
          ! to avoid conflict with their use in WindowManager

REAL(r64), ALLOCATABLE, DIMENSION(:) :: TempEffBulkAir      ! air temperature adjacent to the surface used for
                                                       ! inside surface heat balances
REAL(r64), ALLOCATABLE, DIMENSION(:) :: HConvIn        !INSIDE CONVECTION COEFFICIENT
REAL(r64), ALLOCATABLE, DIMENSION(:) :: AnisoSkyMult   ! Multiplier on exterior-surface sky view factor to
                                                       ! account for anisotropy of sky radiance; = 1.0 for
                                                       ! for isotropic sky

! Moved from SolarShading to avoid conflicts in DaylightingDevices
REAL(r64), ALLOCATABLE, DIMENSION(:) :: DifShdgRatioIsoSky ! Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: DifShdgRatioIsoSkyHRTS ! Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: curDifShdgRatioIsoSky ! Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: DifShdgRatioHoriz  ! Horizon shading ratio (WithShdgHoriz/WoShdgHoriz)
REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: DifShdgRatioHorizHRTS  ! Horizon shading ratio (WithShdgHoriz/WoShdgHoriz)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: WithShdgIsoSky     ! Diffuse solar irradiance from sky on surface, with shading
REAL(r64), ALLOCATABLE, DIMENSION(:) :: WoShdgIsoSky       ! Diffuse solar from sky on surface, without shading
REAL(r64), ALLOCATABLE, DIMENSION(:) :: WithShdgHoriz      ! Diffuse solar irradiance from horizon portion of sky on surface,
                                                           ! with shading
REAL(r64), ALLOCATABLE, DIMENSION(:) :: WoShdgHoriz        ! Diffuse solar irradiance from horizon portion of sky on surface,
                                                           ! without shading
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MultIsoSky         ! Contribution to eff sky view factor from isotropic sky
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MultCircumSolar    ! Contribution to eff sky view factor from circumsolar brightening
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MultHorizonZenith  ! Contribution to eff sky view factor from horizon or zenith brightening

REAL(r64), ALLOCATABLE, DIMENSION(:) :: QS                  ! Zone short-wave flux density; used to calculate short-wave
                                                            !     radiation absorbed on inside surfaces of zone
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QSLights            ! Like QS, but Lights short-wave only.

REAL(r64), ALLOCATABLE, DIMENSION(:) :: QSDifSol            ! Like QS, but diffuse solar short-wave only.
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ITABSF              !FRACTION OF THERMAL FLUX ABSORBED (PER UNIT AREA)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: TMULT               !TMULT  - MULTIPLIER TO COMPUTE 'ITABSF'
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QL                  !TOTAL THERMAL RADIATION ADDED TO ZONE
REAL(r64), ALLOCATABLE, DIMENSION(:,:)   :: SunlitFracHR ! Hourly fraction of heat transfer surface that is sunlit
REAL(r64), ALLOCATABLE, DIMENSION(:,:)   :: CosIncAngHR  ! Hourly cosine of beam radiation incidence angle on surface
REAL(r64), ALLOCATABLE, DIMENSION(:,:,:)   :: SunlitFrac ! TimeStep fraction of heat transfer surface that is sunlit
REAL(r64), ALLOCATABLE, DIMENSION(:,:,:)   :: SunlitFracWithoutReveal ! For a window with reveal, the sunlit fraction
                                                    ! without shadowing by the reveal
REAL(r64), ALLOCATABLE, DIMENSION(:,:,:)   :: CosIncAng  ! TimeStep cosine of beam radiation incidence angle on surface
INTEGER,ALLOCATABLE,DIMENSION(:,:,:,:) :: BackSurfaces ! For a given hour and timestep, a list of up to 20 surfaces receiving
                                                       ! beam solar radiation from a given exterior window
REAL(r64),ALLOCATABLE,DIMENSION(:,:,:,:) :: OverlapAreas ! For a given hour and timestep, the areas of the exterior window sending
                                                    ! beam solar radiation to the surfaces listed in BackSurfaces
REAL(r64)         :: GasCoeffsCon(10,3)   ! Gas conductivity coefficients for gases in a mixture
!                       Air       Argon     Krypton   Xenon
DATA GasCoeffsCon    /2.873d-3, 2.285d-3, 9.443d-4, 4.538d-4,  0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0, &
                      7.760d-5, 5.149d-5, 2.826d-5, 1.723d-5,  0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0, &
                      0.0d0   , 0.0d0   , 0.0d0   , 0.0d0   ,  0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0 /

REAL(r64)         :: GasCoeffsVis(10,3)   ! Gas viscosity coefficients for gases in a mixture
!                       Air       Argon     Krypton   Xenon
DATA GasCoeffsVis   /3.723d-6, 3.379d-6, 2.213d-6, 1.069d-6,  0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0, &
                     4.940d-8, 6.451d-8, 7.777d-8, 7.414d-8,  0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0, &
                       0.0d0   , 0.0d0   , 0.0d0   , 0.0d0   ,  0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0 /

REAL(r64)         :: GasCoeffsCp(10,3)   ! Gas specific heat coefficients for gases in a mixture
!                     Air       Argon     Krypton   Xenon
DATA GasCoeffsCp    /1002.737d0 , 521.929d0 , 248.091d0 ,  158.340d0, 0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0, &
                     1.2324d-2  , 0.0d0     , 0.0d0     ,      0.0d0, 0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0, &
                       0.0d0    , 0.0d0     , 0.0d0     ,      0.0d0, 0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0 /

REAL(r64)         :: GasWght(10)=         & ! Gas molecular weights for gases in a mixture
!                       Air       Argon     Krypton   Xenon
                   (/28.97d0,     39.948d0,    83.8d0,   131.3d0,     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0/)

REAL(r64)          :: GasSpecificHeatRatio(10) =    & !Gas specific heat ratios.  Used for gasses in low pressure
                    (/1.4d0,     1.67d0,    1.68d0,   1.66d0,     0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0/)

!Variables Dimensioned to Number of Zones
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MVFC      ! Design Mixing Flow Rate [m3/s] (Cross Zone Mixing)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MTC       ! Control Temperature For Mixing [C] (Cross Zone Mixing)

REAL(r64), TARGET :: ZeroPointerVal = 0.d0

          ! SUBROUTINE SPECIFICATIONS FOR MODULE DataHeatBalance:
PUBLIC  CheckAndSetConstructionProperties
PUBLIC  AssignReverseConstructionNumber
PUBLIC  AddVariableSlatBlind
PUBLIC  CalcScreenTransmittance

CONTAINS

SUBROUTINE CheckAndSetConstructionProperties(ConstrNum,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine checks some properties of entered constructions; sets some properties; and sets
          ! an error flag for certain error conditions.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowWarningError,ShowSevereError,ShowContinueError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ConstrNum    ! Construction number to be set/checked
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! error flag that is set when certain errors have occurred

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InsideLayer      ! Inside Layer of Construct; for window construct, layer no. of inside glass
  INTEGER :: MaterNum         ! Counters to keep track of the material number for a layer
  INTEGER :: OutsideMaterNum  ! Material "number" of the Outside layer
  INTEGER :: InsideMaterNum   ! Material "number" of the Inside layer
  INTEGER :: Layer            ! loop index for each of the construction layers
  INTEGER :: TotLayers          ! Number of layers in a construction
  INTEGER :: TotGlassLayers     ! Number of glass layers in a construction
  INTEGER :: TotShadeLayers     ! Number of shade layers in a construction
  INTEGER :: TotGasLayers       ! Number of gas layers in a construction
  LOGICAL :: WrongMaterialsMix  ! True if window construction has a layer that is not glass, gas or shade
  LOGICAL :: WrongWindowLayering ! True if error in layering of a window construction
  INTEGER :: MaterNumNext       ! Next material number in the layer sequence
  INTEGER :: IGas              ! Index for gases in a mixture of gases in a window gap
  INTEGER :: LayNumSh          ! Number of shade/blind layer in a construction
  INTEGER :: MatSh             ! Material number of a shade/blind layer
  INTEGER :: MatGapL           ! Material number of the gas layer to the left (outer side) of a shade/blind layer
  INTEGER :: MatGapR           ! Material number of the gas layer to the right (innner side) of a shade/blind layer
  INTEGER :: BlNum             ! Blind number
  LOGICAL :: ValidBGShadeBlindConst ! True if a valid window construction with between-glass shade/blind
  INTEGER :: GlassLayNum       ! Glass layer number

    TotLayers = Construct(ConstrNum)%TotLayers
    InsideLayer = TotLayers
    IF (Construct(ConstrNum)%LayerPoint(InsideLayer) <= 0) RETURN    ! Error condition
    IF (TotLayers == 0) RETURN ! error condition, hopefully caught elsewhere

!   window screen is not allowed on inside layer

    Construct(ConstrNum)%DayltPropPtr = 0
    InsideMaterNum=Construct(ConstrNum)%LayerPoint(InsideLayer)
    IF (InsideMaterNum /= 0) THEN
      Construct(ConstrNum)%InsideAbsorpVis = Material(InsideMaterNum)%AbsorpVisible
      Construct(ConstrNum)%InsideAbsorpSolar = Material(InsideMaterNum)%AbsorpSolar

      ! Following line applies only to opaque surfaces; it is recalculated later for windows.
      Construct(ConstrNum)%ReflectVisDiffBack = 1.0d0 -  Material(InsideMaterNum)%AbsorpVisible
    END IF

    OutsideMaterNum=Construct(ConstrNum)%LayerPoint(1)
    IF (OutsideMaterNum /= 0) THEN
      Construct(ConstrNum)%OutsideAbsorpVis = Material(OutsideMaterNum)%AbsorpVisible
      Construct(ConstrNum)%OutsideAbsorpSolar = Material(OutsideMaterNum)%AbsorpSolar
    END IF

    Construct(ConstrNum)%TotSolidLayers = 0
    Construct(ConstrNum)%TotGlassLayers = 0
    Construct(ConstrNum)%AbsDiffShade = 0.0d0

    ! Check if any layer is glass, gas, shade, screen or blind; if so it is considered a window construction for
    ! purposes of error checking.

    Construct(ConstrNum)%TypeIsWindow = .false.
    DO Layer = 1,TotLayers
      MaterNum  = Construct(ConstrNum)%LayerPoint(Layer)
      IF (MaterNum == 0) CYCLE ! error -- has been caught will stop program later
      IF(Material(MaterNum)%Group == WindowGlass .OR. Material(MaterNum)%Group == WindowGas &
         .OR. Material(MaterNum)%Group == WindowGasMixture &
         .OR. Material(MaterNum)%Group == Shade .OR. Material(MaterNum)%Group == WindowBlind &
         .OR. Material(MaterNum)%Group == Screen .OR. Material(MaterNum)%Group == WindowSimpleGlazing &
         .OR. Material(MaterNum)%Group == ComplexWindowShade .OR. Material(MaterNum)%Group == ComplexWindowGap &
         .OR. Material(MaterNum)%Group == GlassEquivalentLayer .OR. Material(MaterNum)%Group == ShadeEquivalentLayer &
         .OR. Material(MaterNum)%Group == DrapeEquivalentLayer .OR. Material(MaterNum)%Group == ScreenEquivalentLayer &
         .OR. Material(MaterNum)%Group == BlindEquivalentLayer .OR. Material(MaterNum)%Group == GapEquivalentLayer) &
         Construct(ConstrNum)%TypeIsWindow = .true.
    END DO

    IF (InsideMaterNum == 0) RETURN
    IF (OutsideMaterNum == 0) RETURN

    IF(Construct(ConstrNum)%TypeIsWindow) THEN

      Construct(ConstrNum)%NumCTFTerms = 0
      Construct(ConstrNum)%NumHistories = 0
      WrongMaterialsMix = .false.
      WrongWindowLayering = .false.
      DO Layer = 1,TotLayers
        MaterNum  = Construct(ConstrNum)%LayerPoint(Layer)
        IF (MaterNum == 0) CYCLE ! error -- has been caught will stop program later
        IF(Material(MaterNum)%Group /= WindowGlass .AND. Material(MaterNum)%Group /= WindowGas &
         .AND. Material(MaterNum)%Group /= WindowGasMixture &
         .AND. Material(MaterNum)%Group /= Shade .AND. Material(MaterNum)%Group /= WindowBlind &
         .AND. Material(MaterNum)%Group /= Screen .AND. Material(MaterNum)%Group /= WindowSimpleGlazing &
         .AND. Material(MaterNum)%Group /= ComplexWindowShade .AND. Material(MaterNum)%Group /= ComplexWindowGap &
         .AND. Material(MaterNum)%Group /= GlassEquivalentLayer .AND. Material(MaterNum)%Group /= GapEquivalentLayer &
         .AND. Material(MaterNum)%Group /= ShadeEquivalentLayer .AND. Material(MaterNum)%Group /= DrapeEquivalentLayer &
         .AND. Material(MaterNum)%Group /= ScreenEquivalentLayer .AND. Material(MaterNum)%Group /= BlindEquivalentLayer) &
         WrongMaterialsMix = .true.
      END DO

      IF(WrongMaterialsMix) THEN  !Illegal material for a window construction
        CALL ShowSevereError('Error: Window construction='//TRIM(Construct(ConstrNum)%Name)//  &
             ' has materials other than glass, gas, shade, screen, blind, complex shading, complex gap, or simple system.')
        ErrorsFound = .true.
      ! Do not check number of layers for BSDF type of window since that can be handled
      ELSE IF((TotLayers > 8).and.(.not.Construct(ConstrNum)%WindowTypeBSDF) &
                             .and.(.not.Construct(ConstrNum)%WindowTypeEQL) ) THEN  !Too many layers for a window construction
        CALL ShowSevereError('CheckAndSetConstructionProperties: Window construction='//TRIM(Construct(ConstrNum)%Name)//  &
             ' has too many layers (max of 8 allowed -- 4 glass + 3 gap + 1 shading device).')
        ErrorsFound = .true.

      ELSE IF (TotLayers == 1) THEN

        IF(Material(Construct(ConstrNum)%LayerPoint(1))%Group == Shade &
           .OR. Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowGas &
           .OR. Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowGasMixture &
           .OR. Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowBlind &
           .OR. Material(Construct(ConstrNum)%LayerPoint(1))%Group == Screen &
           .OR. Material(Construct(ConstrNum)%LayerPoint(1))%Group == ComplexWindowShade &
           .OR. Material(Construct(ConstrNum)%LayerPoint(1))%Group == ComplexWindowGap) THEN
          CALL ShowSevereError('CheckAndSetConstructionProperties: The single-layer window construction='//  &
                   TRIM(Construct(ConstrNum)%Name)// &
                   ' has a gas, complex gap, shade, complex shade, screen or blind material; '//  &
                   'it should be glass of simple glazing system.')
          ErrorsFound = .true.
        END IF
      END IF

      ! Find total glass layers, total shade/blind layers and total gas layers in a window construction

      TotGlassLayers = 0
      TotShadeLayers = 0 ! Includes shades, blinds, and screens
      TotGasLayers   = 0
      DO Layer = 1,TotLayers
        MaterNum  = Construct(ConstrNum)%LayerPoint(Layer)
        IF (MaterNum == 0) CYCLE ! error -- has been caught will stop program later
        IF(Material(MaterNum)%Group == WindowGlass) TotGlassLayers = TotGlassLayers + 1
        IF(Material(MaterNum)%Group == WindowSimpleGlazing) TotGlassLayers = TotGlassLayers + 1
        IF(Material(MaterNum)%Group == Shade .OR. Material(MaterNum)%Group == WindowBlind .OR. &
           Material(MaterNum)%Group == Screen .OR. Material(MaterNum)%Group == ComplexWindowShade)   &
              TotShadeLayers = TotShadeLayers + 1
        IF(Material(MaterNum)%Group == WindowGas .OR. Material(MaterNum)%Group == WindowGasMixture .OR.   &
           Material(MaterNum)%Group == ComplexWindowGap) &
              TotGasLayers = TotGasLayers + 1
        IF(Layer < TotLayers) THEN
          MaterNumNext = Construct(ConstrNum)%LayerPoint(Layer+1)
          ! Adjacent layers of same type not allowed
          IF (MaterNumNext == 0) CYCLE
          IF(Material(MaterNum)%Group == Material(MaterNumNext)%Group) WrongWindowLayering = .true.
        END IF
      END DO

      ! It is not necessary to check rest of BSDF window structure since that is performed inside TARCOG90 routine.
      ! That routine also allow structures which are not allowed in rest of this routine
      if (Construct(ConstrNum)%WindowTypeBSDF) then
        Construct(ConstrNum)%TotGlassLayers = TotGlassLayers
        Construct(ConstrNum)%TotSolidLayers = TotGlassLayers + TotShadeLayers
        Construct(ConstrNum)%InsideAbsorpThermal = Material(Construct(ConstrNum)%LayerPoint(InsideLayer))%AbsorpThermalBack
        Construct(ConstrNum)%OutsideAbsorpThermal = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermalFront
        return
      end if

      IF (Construct(ConstrNum)%WindowTypeEQL) Then
        Construct(ConstrNum)%InsideAbsorpThermal = Material(Construct(ConstrNum)%LayerPoint(InsideLayer))%AbsorpThermalBack
        Construct(ConstrNum)%OutsideAbsorpThermal = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermalFront
        Return
      ENDIF

      IF(Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowGas .or. &
         Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowGasMixture .or. &
         Material(Construct(ConstrNum)%LayerPoint(TotLayers))%Group == WindowGas .or. &
         Material(Construct(ConstrNum)%LayerPoint(TotLayers))%Group == WindowGasMixture) &
           WrongWindowLayering = .true. ! Gas cannot be first or last layer
      IF(TotShadeLayers > 1) WrongWindowLayering = .true. !At most one shade, screen or blind allowed

      ! If there is a diffusing glass layer no shade, screen or blind is allowed
      DO Layer = 1,TotLayers
        MaterNum  = Construct(ConstrNum)%LayerPoint(Layer)
        IF (MaterNum == 0) CYCLE ! error -- has been caught will stop program later
        IF(Material(MaterNum)%SolarDiffusing .AND. TotShadeLayers > 0) THEN
          ErrorsFound = .true.
          CALL ShowSevereError('CheckAndSetConstructionProperties: Window construction='//TRIM(Construct(ConstrNum)%Name))
          CALL ShowContinueError( &
            'has diffusing glass='//TRIM(Material(MaterNum)%Name)//' and a shade, screen or blind layer.')
          EXIT
        END IF
      END DO

      ! If there is a diffusing glass layer it must be the innermost layer
      IF(TotGlassLayers > 1) THEN
        GlassLayNum = 0
        DO Layer = 1,TotLayers
          MaterNum  = Construct(ConstrNum)%LayerPoint(Layer)
          IF (MaterNum == 0) CYCLE ! error -- has been caught will stop program later
          IF(Material(MaterNum)%Group == WindowGlass) THEN
            GlassLayNum = GlassLayNum + 1
            IF(GlassLayNum < TotGlassLayers .AND. Material(MaterNum)%SolarDiffusing) THEN
              ErrorsFound = .true.
              CALL ShowSevereError('CheckAndSetConstructionProperties: Window construction='//TRIM(Construct(ConstrNum)%Name))
              CALL ShowContinueError( &
                'has diffusing glass='//TRIM(Material(MaterNum)%Name)//' that is not the innermost glass layer.')
            END IF
          END IF
        END DO
      END IF

      ! interior window screen is not allowed. Check for invalid between-glass screen is checked below.
      IF(TotShadeLayers == 1 .AND. Material(Construct(ConstrNum)%LayerPoint(TotLayers))%Group == Screen .and.   &
         TotLayers /= 1) THEN
        WrongWindowLayering = .true.
      END IF

      ! Consistency checks for a construction with a between-glass shade or blind

      IF(TotShadeLayers == 1 .AND. Material(Construct(ConstrNum)%LayerPoint(1))%Group /= Shade .AND. &
          Material(Construct(ConstrNum)%LayerPoint(1))%Group /= WindowBlind .AND. &
          Material(Construct(ConstrNum)%LayerPoint(1))%Group /= Screen .AND. &
          Material(Construct(ConstrNum)%LayerPoint(TotLayers))%Group /= Shade .AND. &
          Material(Construct(ConstrNum)%LayerPoint(TotLayers))%Group /= WindowBlind .AND.  &
          Material(Construct(ConstrNum)%LayerPoint(TotLayers))%Group /= ComplexWindowShade .AND.  &
         .NOT.WrongWindowLayering) THEN


        ! This is a construction with a between-glass shade or blind

        IF(TotGlassLayers == 4) THEN
          ! Quadruple pane not allowed.
          WrongWindowLayering = .true.
        ELSE IF(TotGlassLayers == 2 .OR. TotGlassLayers == 3) THEN
          ValidBGShadeBlindConst = .FALSE.
          IF(TotGlassLayers == 2) THEN
            IF(TotLayers /= 5) THEN
              WrongWindowLayering = .TRUE.
            ELSE
              IF(Material(Construct(ConstrNum)%LayerPoint(1))%Group==WindowGlass .AND.  &
                (Material(Construct(ConstrNum)%LayerPoint(2))%Group==WindowGas .OR. &
                 Material(Construct(ConstrNum)%LayerPoint(2))%Group==WindowGasMixture) .AND. &
                ((Material(Construct(ConstrNum)%LayerPoint(3))%Group==Shade .OR. &
                 Material(Construct(ConstrNum)%LayerPoint(3))%Group==WindowBlind) .AND. &
                 .NOT. Material(Construct(ConstrNum)%LayerPoint(3))%Group==Screen) .AND. &
                (Material(Construct(ConstrNum)%LayerPoint(4))%Group==WindowGas .OR. &
                 Material(Construct(ConstrNum)%LayerPoint(4))%Group==WindowGasMixture) .AND. &
                 Material(Construct(ConstrNum)%LayerPoint(5))%Group==WindowGlass) &
                   ValidBGShadeBlindConst = .TRUE.
            END IF
          ELSE  ! TotGlassLayers = 3
            IF(TotLayers /= 7) THEN
              WrongWindowLayering = .TRUE.
            ELSE
             IF (Material(Construct(ConstrNum)%LayerPoint(1))%Group==WindowGlass .AND.  &
                (Material(Construct(ConstrNum)%LayerPoint(2))%Group==WindowGas .OR. &
                 Material(Construct(ConstrNum)%LayerPoint(2))%Group==WindowGasMixture) .AND. &
                 Material(Construct(ConstrNum)%LayerPoint(3))%Group==WindowGlass .AND.  &
                (Material(Construct(ConstrNum)%LayerPoint(4))%Group==WindowGas .OR. &
                 Material(Construct(ConstrNum)%LayerPoint(4))%Group==WindowGasMixture) .AND. &
                ((Material(Construct(ConstrNum)%LayerPoint(5))%Group==Shade .OR. &
                 Material(Construct(ConstrNum)%LayerPoint(5))%Group==WindowBlind) .AND. &
                 .NOT. Material(Construct(ConstrNum)%LayerPoint(5))%Group==Screen) .AND. &
                (Material(Construct(ConstrNum)%LayerPoint(6))%Group==WindowGas .OR. &
                 Material(Construct(ConstrNum)%LayerPoint(6))%Group==WindowGasMixture) .AND. &
                 Material(Construct(ConstrNum)%LayerPoint(7))%Group==WindowGlass) &
                   ValidBGShadeBlindConst = .TRUE.
            END IF
          END IF  ! End of check if TotGlassLayers = 2 or 3
          IF(.NOT.ValidBGShadeBlindConst) WrongWindowLayering = .TRUE.
          IF(.NOT.WrongWindowLayering) THEN
            LayNumSh = 2*TotGlassLayers - 1
            MatSh = Construct(ConstrNum)%LayerPoint(LayNumSh)
              ! For double pane, shade/blind must be layer #3.
              ! For triple pane, it must be layer #5 (i.e., between two inner panes).
            IF(Material(MatSh)%Group /= Shade .AND. Material(MatSh)%Group /= WindowBlind) WrongWindowLayering = .true.
            IF(TotLayers /= 2*TotGlassLayers + 1) WrongWindowLayering = .true.

            ! TH 8/26/2010 commented out, CR 8206
              ! All glass layers must be SpectralAverage
!            IF(.not.WrongWindowLayering) THEN
!              IF(TotGlassLayers == 2) THEN   ! Double pane
!                IF(Material(Construct(ConstrNum)%LayerPoint(1))%GlassSpectralDataPtr > 0 .OR.  &
!                   Material(Construct(ConstrNum)%LayerPoint(5))%GlassSpectralDataPtr > 0) THEN
!                     CALL ShowSevereError('CheckAndSetConstructionProperties: For window construction '//  &
!                               TRIM(Construct(ConstrNum)%Name))
!                     CALL ShowContinueError('Glass layers cannot use SpectralData -- must be SpectralAverage.')
!                     WrongWindowLayering = .true.
!                ENDIF
!              ELSE                           ! Triple pane
!                IF(Material(Construct(ConstrNum)%LayerPoint(1))%GlassSpectralDataPtr > 0 .OR.  &
!                   Material(Construct(ConstrNum)%LayerPoint(3))%GlassSpectralDataPtr > 0 .OR. &
!                   Material(Construct(ConstrNum)%LayerPoint(7))%GlassSpectralDataPtr > 0) THEN
!                     CALL ShowSevereError('CheckAndSetConstructionProperties: For window construction '//  &
!                               TRIM(Construct(ConstrNum)%Name))
!                     CALL ShowContinueError('Glass layers cannot use SpectralData -- must be SpectralAverage.')
!                     WrongWindowLayering = .true.
!                ENDIF
!              END IF
!            END IF

            IF(.NOT.WrongWindowLayering) THEN
              ! Gas on either side of a between-glass shade/blind must be the same
              MatGapL = Construct(ConstrNum)%LayerPoint(LayNumSh-1)
              MatGapR = Construct(ConstrNum)%LayerPoint(LayNumSh+1)
              DO IGas = 1,5
                IF((Material(MatGapL)%GasType(IGas) /= Material(MatGapR)%GasType(IGas)) .OR. &
                   (Material(MatGapL)%GasFract(IGas) /= Material(MatGapR)%GasFract(IGas))) WrongWindowLayering = .true.
              END DO
                ! Gap width on either side of a between-glass shade/blind must be the same
              IF(ABS(Material(MatGapL)%Thickness - Material(MatGapR)%Thickness) > 0.0005d0) WrongWindowLayering = .true.
              IF(Material(MatSh)%Group == WindowBlind) THEN
                BlNum = Material(MatSh)%BlindDataPtr
                IF(BlNum > 0) THEN
                  IF((Material(MatGapL)%Thickness + Material(MatGapR)%Thickness) < Blind(BlNum)%SlatWidth) THEN
                    ErrorsFound = .true.
                    CALL ShowSevereError('CheckAndSetConstructionProperties: For window construction '//  &
                               TRIM(Construct(ConstrNum)%Name))
                    CALL ShowContinueError('the slat width of the between-glass blind is greater than')
                    CALL ShowContinueError('the sum of the widths of the gas layers adjacent to the blind.')
                  END IF
                END IF  ! End of check if BlNum > 0
              END IF  ! End of check if material is window blind
            END IF  ! End of check if WrongWindowLayering
          END IF  ! End of check if WrongWindowLayering
        END IF  ! End of check on total glass layers
      END IF  ! End of check if construction has between-glass shade/blind

      ! Check Simple Windows,
      IF (Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowSimpleGlazing) THEN
        IF (TotLayers > 1) THEN
          ! check that none of the other layers are glazing or gas
          DO Layer = 1,TotLayers
            MaterNum  = Construct(ConstrNum)%LayerPoint(Layer)
            IF (MaterNum == 0) CYCLE ! error -- has been caught will stop program later
            IF(Material(MaterNum)%Group == WindowGlass) THEN
              ErrorsFound = .TRUE.
              CALL ShowSevereError('CheckAndSetConstructionProperties: Error in window construction '//  &
                            TRIM(Construct(ConstrNum)%Name)// '--')
              CALL ShowContinueError('For simple window constructions, no other glazing layers are allowed.')
            END IF
            IF(Material(MaterNum)%Group == WindowGas) THEN
              ErrorsFound = .TRUE.
              CALL ShowSevereError('CheckAndSetConstructionProperties: Error in window construction '//  &
                            TRIM(Construct(ConstrNum)%Name)// '--')
              CALL ShowContinueError('For simple window constructions, no other gas layers are allowed.')
            END IF
          END DO
        ENDIF
      ENDIF

      IF(WrongWindowLayering) THEN
        CALL ShowSevereError('CheckAndSetConstructionProperties: Error in window construction '//  &
                            TRIM(Construct(ConstrNum)%Name)// '--')
        CALL ShowContinueError('  For multi-layer window constructions the following rules apply:')
        CALL ShowContinueError('    --The first and last layer must be a solid layer (glass or shade/screen/blind),')
        CALL ShowContinueError('    --Adjacent glass layers must be separated by one and only one gas layer,')
        CALL ShowContinueError('    --Adjacent layers must not be of the same type,')
        CALL ShowContinueError('    --Only one shade/screen/blind layer is allowed,')
        CALL ShowContinueError('    --An exterior shade/screen/blind must be the first layer,')
        CALL ShowContinueError('    --An interior shade/blind must be the last layer,')
        CALL ShowContinueError('    --An interior screen is not allowed,')
        CALL ShowContinueError('    --For an exterior shade/screen/blind or interior shade/blind, there should not be a gas layer')
        CALL ShowContinueError('    ----between the shade/screen/blind and adjacent glass,')
        CALL ShowContinueError('    --A between-glass screen is not allowed,')
        CALL ShowContinueError('    --A between-glass shade/blind is allowed only for double and triple glazing,')
        CALL ShowContinueError('    --A between-glass shade/blind must have adjacent gas layers of the same type and width,')
!        CALL ShowContinueError('    --For between-glass shade/blind all glazing layers must be input using SpectralAverage data,')
        CALL ShowContinueError('    --For triple glazing the between-glass shade/blind must be between the two inner glass layers,')
        CALL ShowContinueError('    --The slat width of a between-glass blind must be less than the sum of the widths')
        CALL ShowContinueError('    ----of the gas layers adjacent to the blind.')
        ErrorsFound = .true.
      END IF

      Construct(ConstrNum)%TotGlassLayers = TotGlassLayers
      Construct(ConstrNum)%TotSolidLayers = TotGlassLayers + TotShadeLayers

      ! In following, InsideLayer is layer number of inside glass and InsideAbsorpThermal applies
      ! only to inside glass; it is corrected later in InitGlassOpticalCalculations
      ! if construction has inside shade or blind.
      IF (Material(Construct(ConstrNum)%LayerPoint(InsideLayer))%Group == Shade .OR. &
          Material(Construct(ConstrNum)%LayerPoint(InsideLayer))%Group == WindowBlind) THEN
        InsideLayer=InsideLayer-1
      ENDIF
      IF (InsideLayer > 0) THEN
        InsideMaterNum=Construct(ConstrNum)%LayerPoint(InsideLayer)
        Construct(ConstrNum)%InsideAbsorpThermal = &
          Material(Construct(ConstrNum)%LayerPoint(InsideLayer))%AbsorpThermalBack
      ENDIF
      IF (InsideMaterNum /= 0) THEN
        Construct(ConstrNum)%InsideAbsorpVis = Material(InsideMaterNum)%AbsorpVisible
        Construct(ConstrNum)%InsideAbsorpSolar = Material(InsideMaterNum)%AbsorpSolar
      ENDIF

      IF((Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowGlass) .OR.    & !Glass
         (Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowSimpleGlazing) )  THEN
        Construct(ConstrNum)%OutsideAbsorpThermal = &
          Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermalFront
      ELSE  !Exterior shade, blind or screen
        Construct(ConstrNum)%OutsideAbsorpThermal = &
          Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermal
      END IF

    ELSE  !Opaque surface
      Construct(ConstrNum)%InsideAbsorpThermal = &
        Material(Construct(ConstrNum)%LayerPoint(InsideLayer))%AbsorpThermal
      Construct(ConstrNum)%OutsideAbsorpThermal = &
        Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermal
    END IF

    Construct(ConstrNum)%OutsideRoughness=Material(Construct(ConstrNum)%LayerPoint(1))%Roughness

    IF (Material(Construct(ConstrNum)%LayerPoint(1))%Group == Air) THEN
      CALL ShowSevereError('CheckAndSetConstructionProperties: Outside Layer is Air for construction '//  &
                       TRIM(Construct(ConstrNum)%Name))
      CALL ShowContinueError('  Error in material '//TRIM(Material(Construct(ConstrNum)%LayerPoint(1))%Name))
      ErrorsFound = .true.
    ENDIF
    IF (InsideLayer > 0) THEN
      IF (Material(Construct(ConstrNum)%LayerPoint(InsideLayer))%Group == Air) THEN
        CALL ShowSevereError('CheckAndSetConstructionProperties: Inside Layer is Air for construction '//  &
                           TRIM(Construct(ConstrNum)%Name))
        CALL ShowContinueError('  Error in material '//TRIM(Material(Construct(ConstrNum)%LayerPoint(InsideLayer))%Name))
        ErrorsFound = .true.
      ENDIF
    ENDIF

    IF (Material(Construct(ConstrNum)%LayerPoint(1))%Group == EcoRoof) THEN
      Construct(ConstrNum)%TypeIsEcoRoof = .true.
      !need to check EcoRoof is not non-outside layer
      DO Layer=2,TotLayers
        IF (Material(Construct(ConstrNum)%LayerPoint(Layer))%Group == EcoRoof) THEN
          CALL ShowSevereError('CheckAndSetConstructionProperties: Interior Layer is EcoRoof for construction '//  &
                      TRIM(Construct(ConstrNum)%Name))
          CALL ShowContinueError('  Error in material '//TRIM(Material(Construct(ConstrNum)%LayerPoint(Layer))%Name))
          ErrorsFound = .true.
        ENDIF
      ENDDO
    ENDIF

    IF (Material(Construct(ConstrNum)%LayerPoint(1))%Group == IRTMaterial) THEN
      Construct(ConstrNum)%TypeIsIRT = .true.
      IF (Construct(ConstrNum)%TotLayers /= 1) THEN
        CALL ShowSevereError('CheckAndSetConstructionProperties: '//  &
                   'Infrared Transparent (IRT) Construction is limited to 1 layer '//  &
                    TRIM(Construct(ConstrNum)%Name))
        CALL ShowContinueError('  Too many layers in referenced construction.')
        ErrorsFound = .true.
      ENDIF
    ENDIF
  RETURN

END SUBROUTINE CheckAndSetConstructionProperties

FUNCTION AssignReverseConstructionNumber(ConstrNum,ErrorsFound) RESULT(NewConstrNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! For interzone, unentered surfaces, we need to have "reverse" constructions
          ! assigned to the created surfaces.  These need to be the reverse (outside to inside layer)
          ! of existing surfaces.  Plus, there may be one already in the data structure so this is looked for as well.

          ! METHODOLOGY EMPLOYED:
          ! Create reverse layers.  Look in current constructions to see if match.  If no match, create a new one.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ConstrNum  ! Existing Construction number of first surface
  LOGICAL, INTENT(INOUT) :: ErrorsFound
  INTEGER                :: NewConstrNum ! Reverse Construction Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: LayerPoint(MaxLayersInConstruct) = 0 ! Pointer array which refers back to
    INTEGER :: nLayer
    INTEGER :: Loop
    LOGICAL :: Found

    IF (ConstrNum == 0) THEN
      ! error caught elsewhere
      NewConstrNum=0
      RETURN
    ENDIF

    Construct(ConstrNum)%IsUsed=.true.
    nLayer=0
    LayerPoint=0
    DO Loop=Construct(ConstrNum)%TotLayers,1,-1
      nLayer=nLayer+1
      LayerPoint(nLayer)=Construct(ConstrNum)%LayerPoint(Loop)
    ENDDO

    ! now, got thru and see if there is a match already....
    NewConstrNum=0
    DO Loop=1,TotConstructs
      Found=.true.
      DO nLayer=1,MaxLayersInConstruct
        IF (Construct(Loop)%LayerPoint(nLayer) /= LayerPoint(nLayer)) THEN
          Found=.false.
          EXIT
        ENDIF
      ENDDO
      IF (Found) THEN
        NewConstrNum=Loop
        EXIT
      ENDIF
    ENDDO

    ! if need new one, bunch o stuff
    IF (NewConstrNum == 0) THEN
      ALLOCATE(ConstructSave(TotConstructs+1))
      ConstructSave(1:TotConstructs)=Construct(1:TotConstructs)
      ALLOCATE(NominalRSave(TotConstructs+1))
      ALLOCATE(NominalUSave(TotConstructs+1))
      NominalRSave=0.0d0
      NominalRSave(1:TotConstructs)=NominalRforNominalUCalculation(1:TotConstructs)
      NominalUSave=0.0d0
      NominalUSave(1:TotConstructs)=NominalU(1:TotConstructs)
      TotConstructs=TotConstructs+1
      DEALLOCATE(Construct)
      DEALLOCATE(NominalRforNominalUCalculation)
      DEALLOCATE(NominalU)
      ALLOCATE(Construct(TotConstructs))
      Construct=ConstructSave
      DEALLOCATE(ConstructSave)
      ALLOCATE(NominalRforNominalUCalculation(TotConstructs))
      ALLOCATE(NominalU(TotConstructs))
      NominalRforNominalUCalculation=NominalRSave
      NominalU=NominalUSave
      DEALLOCATE(NominalRSave)
      DEALLOCATE(NominalUSave)
      !  Put in new attributes
      NewConstrNum=TotConstructs
      Construct(NewConstrNum)%IsUsed=.true.
      Construct(TotConstructs)=Construct(ConstrNum)  ! preserve some of the attributes.
      ! replace others...
      Construct(TotConstructs)%Name='iz-'//TRIM(Construct(ConstrNum)%Name)
      Construct(TotConstructs)%TotLayers=Construct(ConstrNum)%TotLayers
      DO nLayer=1,MaxLayersInConstruct
        Construct(TotConstructs)%LayerPoint(nLayer)=LayerPoint(nLayer)
        IF (LayerPoint(nLayer) /= 0) THEN
          NominalRforNominalUCalculation(TotConstructs)=NominalRforNominalUCalculation(TotConstructs)+NominalR(LayerPoint(nLayer))
        ENDIF
      ENDDO

      ! no error if zero -- that will have been caught with earlier construction
      ! the following line was changed to fix CR7601
      IF (NominalRforNominalUCalculation(TotConstructs) /= 0.0d0) THEN
        NominalU(TotConstructs)=1.0/NominalRforNominalUCalculation(TotConstructs)
      ENDIF

      CALL CheckAndSetConstructionProperties(TotConstructs,ErrorsFound)

    ENDIF

  RETURN

END FUNCTION AssignReverseConstructionNumber

SUBROUTINE AddVariableSlatBlind(inBlindNumber,outBlindNumber,errFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Window Blinds are presented as "fixed" slat blinds.  However for certain Window Shading Controls,
          ! the program needs to set the property to "variable"/movable slats.  Since a blind could be in use
          ! elsewhere with "fixed", a material needs to be added with variable properties -- having most of the
          ! "fixed" properties in tact.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DataInterfaces, ONLY: ShowWarningError,ShowSevereError,ShowContinueError
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: inBlindNumber  ! current Blind Number/pointer to name
  INTEGER, INTENT(INOUT) :: outBlindNumber ! resultant Blind Number to pass back
  LOGICAL, INTENT(INOUT) :: errFlag        ! error flag should one be needed

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (WindowBlindProperties),  ALLOCATABLE, DIMENSION(:) :: tmpBlind
  INTEGER :: Found
  REAL(r64) :: MinSlatAngGeom
  REAL(r64) :: MaxSlatAngGeom

  ! maybe it's already there
  errFlag=.false.
  Found=FindItemInList('~'//trim(Blind(inBlindNumber)%Name),Blind%Name,TotBlinds)
  IF (Found == 0) THEN
    ! Add a new blind
    ALLOCATE(tmpBlind(TotBlinds))
    tmpBlind=Blind
    DEALLOCATE(Blind)
    TotBlinds=TotBlinds+1
    ALLOCATE(Blind(TotBlinds))
    Blind(1:TotBlinds-1)=tmpBlind(1:TotBlinds-1)
    DEALLOCATE(tmpBlind)
    Blind(TotBlinds)=Blind(inBlindNumber)
    Blind(TotBlinds)%Name='~'//Blind(InBlindNumber)%Name
    outBlindNumber=TotBlinds
    Blind(TotBlinds)%SlatAngleType = VariableSlats

      ! Minimum and maximum slat angles allowed by slat geometry
      IF(Blind(TotBlinds)%SlatWidth > Blind(TotBlinds)%SlatSeparation) THEN
        MinSlatAngGeom =   &
          ASIN(Blind(TotBlinds)%SlatThickness/(Blind(TotBlinds)%SlatThickness + Blind(TotBlinds)%SlatSeparation))/DegToRadians
      ELSE
        MinSlatAngGeom = 0.0d0
      END IF
      MaxSlatAngGeom = 180.d0- MinSlatAngGeom

      ! Error if maximum slat angle less than minimum

      IF(Blind(TotBlinds)%MaxSlatAngle < Blind(TotBlinds)%MinSlatAngle) THEN
        errFlag = .true.
        CALL ShowSevereError('WindowMaterial:Blind="'//trim(Blind(inBlindNumber)%Name)//'", Illegal value combination.')
        CALL ShowContinueError('Minimum Slat Angle=['//TRIM(RoundSigDigits(Blind(TotBlinds)%MinSlatAngle,1))//  &
           '], is greater than '//'Maximum Slat Angle=['//  &
           TRIM(RoundSigDigits(Blind(TotBlinds)%MaxSlatAngle,1))//'] deg.')
      END IF

      ! Error if input slat angle not in input min/max range

      IF (Blind(TotBlinds)%MaxSlatAngle > Blind(TotBlinds)%MinSlatAngle .AND.   &
         (Blind(TotBlinds)%SlatAngle < Blind(TotBlinds)%MinSlatAngle &
          .OR. Blind(TotBlinds)%SlatAngle > Blind(TotBlinds)%MaxSlatAngle)) THEN
        errFlag = .true.
        CALL ShowSevereError('WindowMaterial:Blind="'//trim(Blind(inBlindNumber)%Name)//'", Illegal value combination.')
        CALL ShowContinueError('Slat Angle=['//TRIM(RoundSigDigits(Blind(TotBlinds)%SlatAngle,1))//  &
           '] is outside of the input min/max range, min=['//TRIM(RoundSigDigits(Blind(TotBlinds)%MinSlatAngle,1))//  &
           '], max=['//TRIM(RoundSigDigits(Blind(TotBlinds)%MaxSlatAngle,1))//'] deg.')
      END IF

      ! Warning if input minimum slat angle is less than that allowed by slat geometry

      IF(Blind(TotBlinds)%MinSlatAngle < MinSlatAngGeom) THEN
        CALL ShowWarningError('WindowMaterial:Blind="'//trim(Blind(inBlindNumber)%Name)//'", Illegal value combination.')
        CALL ShowContinueError('Minimum Slat Angle=['//TRIM(RoundSigDigits(Blind(TotBlinds)%MinSlatAngle,1))//  &
           '] is less than the smallest allowed by slat dimensions and spacing, min=['//  &
           TRIM(RoundSigDigits(MinSlatAngGeom,1))//'] deg.')
        CALL ShowContinueError('Minimum Slat Angle will be set to '//TRIM(RoundSigDigits(MinSlatAngGeom,1))//' deg.')
        Blind(TotBlinds)%MinSlatAngle = MinSlatAngGeom
      END IF

      ! Warning if input maximum slat angle is greater than that allowed by slat geometry

      IF(Blind(TotBlinds)%MaxSlatAngle > MaxSlatAngGeom) THEN
        CALL ShowWarningError('WindowMaterial:Blind="'//trim(Blind(inBlindNumber)%Name)//'", Illegal value combination.')
        CALL ShowContinueError('Maximum Slat Angle=['//TRIM(RoundSigDigits(Blind(TotBlinds)%MaxSlatAngle,1))//  &
           '] is greater than the largest allowed by slat dimensions and spacing, ['//  &
           TRIM(RoundSigDigits(MaxSlatAngGeom,1))//'] deg.')
        CALL ShowContinueError('Maximum Slat Angle will be set to '//TRIM(RoundSigDigits(MaxSlatAngGeom,1))//' deg.')
        Blind(TotBlinds)%MaxSlatAngle = MaxSlatAngGeom
      END IF
  ELSE
    outBlindNumber=Found
  ENDIF

  RETURN

END SUBROUTINE AddVariableSlatBlind

SUBROUTINE CalcScreenTransmittance(SurfaceNum, Phi, Theta, ScreenNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   May 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Calculate transmittance of window screen given azimuth and altitude angle
          !  of sun and surface orientation.

          ! METHODOLOGY EMPLOYED:
          !  Window screen solar beam transmittance varies as the sun moves across the sky
          !  due to the geometry of the screen material and the angle of incidence
          !  of the solar beam. Azimuth and altitude angle are calculated with respect
          !  to the surface outward normal. Solar beam reflectance and absorptance are also
          !  accounted for.

          !  CALLs to CalcScreenTransmittance are primarily based on surface index. A typical call is:
          !  CALL CalcScreenTransmittance(SurfaceNum)
          !  Since a single Material:WindowScreen object may be used for multiple windows, the
          !  screen's direct beam properties are calculated for the screen material attached to this surface.
          !  If a single Material:WindowScreen object is used for 3 windows then SurfaceScreens(3) is allocated.

          !  CALLs to CalcScreenTransmittance may be done by using the optional arguments as follows:
          !  CALLs to CalcScreenTransmittance at normal incidence are:
          !
          !  CALL with a screen number and relative azimuth and altitude angles
          !  CALL CalcScreenTransmittance(0, Phi=0.0, Theta=0.0, ScreenNumber=ScNum)
          !   -OR-
          !  CALL same as above using the material structure
          !  CALL CalcScreenTransmittance(0, Phi=0.0, Theta=0.0, ScreenNumber=Material(MatShade)%ScreenDataPtr)
          !   -OR-
          !  CALL with the surface number and relative azimuth and altitude angles
          !  CALL CalcScreenTransmittance(SurfaceNum, Phi=0.0, Theta=0.0)

          !  CALL's passing the screen number without the relative azimuth and altitude angles is not allowed
          !  CALL CalcScreenTransmittance(0, ScreenNumber=ScNum) ! DO NOT use this syntax


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: Pi, PiOvr2, DegToRadians
  USE DataInterfaces,  ONLY: ShowFatalError
  USE DataSurfaces,    ONLY: Surface, SurfaceWindow, DoNotModel, ModelAsDiffuse, ModelAsDirectBeam
  USE DataEnvironment, ONLY: SOLCOS

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: SurfaceNum
! The optional arguments Phi and Theta are used to integrate over a hemisphere and are passed as pairs
  REAL(r64), INTENT (IN), OPTIONAL :: Phi    ! Optional sun altitude relative to surface outward normal (radians)
  REAL(r64), INTENT (IN), OPTIONAL :: Theta  ! Optional sun azimuth relative to surface outward normal (radians)
! The optional argument ScreenNumber is used during CalcWindowScreenProperties to integrate over a quarter hemispere
! "before" the surface # is known. Theta and Phi can be passed without ScreenNumber, but DO NOT pass ScreenNumber
! without Theta and Phi.
  INTEGER, INTENT (IN), OPTIONAL :: ScreenNumber ! Optional screen number

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: small  = 1.D-9 !Small Number used to approximate zero

          ! FUNCTION PARAMETER DEFINITIONS:
  INTEGER:: ScNum                        ! Index to screen data
  REAL(r64) :: Tdirect                      ! Beam solar transmitted through screen (dependent on sun angle)
  REAL(r64) :: Tscattered                   ! Beam solar reflected through screen (dependent on sun angle)
  REAL(r64) :: TscatteredVis                ! Visible beam solar reflected through screen (dependent on sun angle)
  REAL(r64) :: SunAzimuth                   ! Solar azimuth angle from north (rad)
  REAL(r64) :: SunAltitude                  ! Solar altitude angle from horizon (rad)
  REAL(r64) :: SurfaceAzimuth               ! Surface azimuth angle from north (rad)
  REAL(r64) :: SurfaceTilt                  ! Surface tilt angle from vertical (rad)
  REAL(r64) :: SunAzimuthToScreenNormal     ! Relative solar azimuth (sun angle from screen normal, 0 to PiOvr2, rad)
  REAL(r64) :: SunAltitudeToScreenNormal    ! Relative solar altitude (sun angle from screen normal, -PiOvr2 to PiOvr2, rad)
  REAL(r64) :: Beta                         ! Compliment of relative solar azimuth (rad)
  REAL(r64) :: TransXDir                    ! Horizontal component of direct beam transmittance
  REAL(r64) :: TransYDir                    ! Vertical component of direct beam transmittance
  REAL(r64) :: Delta                        ! Intermediate variable used for Tscatter calculation (deg)
  REAL(r64) :: DeltaMax                     ! Intermediate variable used for Tscatter calculation (deg)
  REAL(r64) :: Tscattermax                  ! Maximum solar beam  scattered transmittance
  REAL(r64) :: TscattermaxVis               ! Maximum visible beam scattered transmittance
  REAL(r64) :: ExponentInterior             ! Exponent used in scattered transmittance calculation
                                            ! when Delta < DeltaMax (0,0 to peak)
  REAL(r64) :: ExponentExterior             ! Exponent used in scattered transmittance calculation
                                            ! when Delta > DeltaMax (peak to max)
  REAL(r64) :: AlphaDblPrime, COSMu, Epsilon, Eta, MuPrime, Gamma  ! Intermediate variables (used in Eng. Doc.)
  REAL(r64) :: NormalAltitude               ! Actual altitude angle of sun wrt surface outward normal (rad)
  REAL(r64) :: NormalAzimuth                ! Actual azimuth angle of sun wrt surface outward normal (rad)
  REAL(r64) :: IncidentAngle                ! Solar angle wrt surface outward normal to determine
                                            ! if sun is in front of screen (rad)
  REAL(r64) :: PeakToPlateauRatio           ! Ratio of peak scattering to plateau at 0,0 incident angle
  REAL(r64) :: PeakToPlateauRatioVis        ! Ratio of peak visible scattering to plateau at 0,0 incident angle
  REAL(r64) :: ReflectCyl                   ! Screen material reflectance
  REAL(r64) :: ReflectCylVis                ! Screen material visible reflectance

! SurfaceScreens structure may be accessed using either the surface or screen index
! The screen index is based on the number of Surface:HeatTransfer:Sub objects using any Material:WindowScreen object
  IF (PRESENT(ScreenNumber))THEN
    ScNum = ScreenNumber
    IF (.NOT. PRESENT(Theta) .OR. .NOT. PRESENT(Phi))THEN
     CALL ShowFatalError('Syntax error, optional arguments Theta and Phi must be present when optional ScreenNumber is used.')
    END IF
  ELSE
    ScNum = SurfaceWindow(SurfaceNum)%ScreenNumber
  END IF

  IF (PRESENT(Theta)) THEN
    SunAzimuthToScreenNormal = ABS(Theta)
    IF(SunAzimuthToScreenNormal .GT. Pi) THEN
      SunAzimuthToScreenNormal = 0.0d0
    ELSE
      IF(SunAzimuthToScreenNormal .GT. PiOvr2) THEN
        SunAzimuthToScreenNormal = Pi - SunAzimuthToScreenNormal
      END IF
    END IF
    NormalAzimuth = SunAzimuthToScreenNormal
  ELSE
    SunAzimuth     = ATAN2(SOLCOS(1),SOLCOS(2))
    IF(SunAzimuth .LT. 0.0d0)SunAzimuth = 2.d0*Pi + SunAzimuth
    SurfaceAzimuth = Surface(SurfaceNum)%Azimuth * DegToRadians
    NormalAzimuth = SunAzimuth-SurfaceAzimuth
!   Calculate the transmittance whether sun is in front of or behind screen, place result in BmBmTrans or BmBmTransBack
    IF(ABS(SunAzimuth-SurfaceAzimuth) .GT. PiOvr2) THEN
      SunAzimuthToScreenNormal  = ABS(SunAzimuth-SurfaceAzimuth) - PiOvr2
    ELSE
      SunAzimuthToScreenNormal  = ABS(SunAzimuth-SurfaceAzimuth)
    END IF
  END IF

  IF (PRESENT(Phi)) THEN
    SunAltitudeToScreenNormal = ABS(Phi)
    IF(SunAltitudeToScreenNormal .GT. PiOvr2)THEN
      SunAltitudeToScreenNormal = Pi - SunAltitudeToScreenNormal
    END IF
    SunAltitude = SunAltitudeToScreenNormal
  ELSE
    SunAltitude = (PiOvr2 - ACOS(SOLCOS(3)))
    SurfaceTilt = Surface(SurfaceNum)%Tilt * DegToRadians
    SunAltitudeToScreenNormal = ABS(SunAltitude+(SurfaceTilt-PiOvr2))
    IF(SunAltitudeToScreenNormal .GT. PiOvr2) THEN
      SunAltitudeToScreenNormal = SunAltitudeToScreenNormal - PiOvr2
    END IF
  END IF

  IF(SurfaceNum .EQ. 0 .OR. .NOT. PRESENT(ScreenNumber)) THEN
    NormalAltitude = SunAltitude
  ELSE
    NormalAltitude = SunAltitude+(SurfaceTilt-PiOvr2)
  END IF

  IF(NormalAltitude .NE. 0.0d0 .AND. NormalAzimuth .NE. 0.0d0)THEN
    IncidentAngle = ACOS(SIN(NormalAltitude)/ (TAN(NormalAzimuth)*TAN(NormalAltitude)/SIN(NormalAzimuth)))
  ELSEIF(NormalAltitude .NE. 0.0d0 .AND. NormalAzimuth .EQ. 0.0d0)THEN
    IncidentAngle = NormalAltitude
  ELSEIF(NormalAltitude .EQ. 0.0d0 .AND. NormalAzimuth .NE. 0.0d0)THEN
    IncidentAngle = NormalAzimuth
  ELSE
    IncidentAngle = 0.0d0
  END IF

! ratio of screen material diameter to screen material spacing
  Gamma = SurfaceScreens(ScNum)%ScreenDiameterToSpacingRatio

! ************************************************************************************************
! * calculate transmittance of totally absorbing screen material (beam passing through open area)*
! ************************************************************************************************

! calculate compliment of relative solar azimuth
  Beta = PiOvr2 - SunAzimuthToScreenNormal

! Catch all divide by zero instances
  IF(Beta .GT. Small)THEN
    IF(ABS(SunAltitudeToScreenNormal-PiOvr2) .GT. Small) THEN
      AlphaDblPrime = ATAN(TAN(SunAltitudeToScreenNormal)/ &
                           COS(SunAzimuthToScreenNormal))
      TransYDir = 1.0d0 - Gamma*(COS(AlphaDblPrime)+ &
                                 SIN(AlphaDblPrime)* &
                                 TAN(SunAltitudeToScreenNormal)* &
                                 SQRT(1.d0+(1.d0/TAN(Beta))**2))
      TransYDir = MAX(0.0d0,TransYDir)
    ELSE
      TransYDir = 0.0d0
    END IF
  ELSE
    TransYDir = 0.0d0
  END IF

  COSMu = SQRT(COS(SunAltitudeToScreenNormal)**2 * &
               COS(SunAzimuthToScreenNormal)**2 +  &
               SIN(SunAltitudeToScreenNormal)**2)
  IF(CosMu .GT. Small) THEN
    Epsilon = ACOS(COS(SunAltitudeToScreenNormal)*COS(SunAzimuthToScreenNormal)/COSMu)
    Eta = PiOvr2 - Epsilon
    IF(COS(Epsilon) .NE. 0.0d0) THEN
      MuPrime = ATAN(TAN(ACOS(COSMu))/COS(Epsilon))
      IF(Eta .NE. 0.0d0) THEN
        TransXDir = 1.d0 - Gamma*(COS(MuPrime)+SIN(MuPrime)* &
                                                       TAN(ACOS(COSMu))*   &
                                                       SQRT(1.d0+(1.d0/TAN(Eta))**2))
        TransXDir = MAX(0.0d0,TransXDir)
      ELSE
        TransXDir = 0.0d0
      END IF
    ELSE
      TransXDir = 0.0d0
    END IF
  ELSE
    TransXDir = 1.0d0 - Gamma
  END IF
  Tdirect = MAX(0.0d0, TransXDir * TransYDir)

! *******************************************************************************
! * calculate transmittance of scattered beam due to reflecting screen material *
! *******************************************************************************

  ReflectCyl    = SurfaceScreens(ScNum)%ReflectCylinder
  ReflectCylVis = SurfaceScreens(ScNum)%ReflectCylinderVis

  IF(ABS(SunAzimuthToScreenNormal-PiOvr2) .LT. Small .OR. ABS(SunAltitudeToScreenNormal-PiOvr2) .LT. Small)THEN
    Tscattered = 0.0d0
    TscatteredVis = 0.0d0
  ELSE
!   DeltaMax and Delta are in degrees
    DeltaMax = 89.7d0 - (10.0d0 * Gamma/0.16d0)
    Delta    = SQRT((SunAzimuthToScreenNormal/DegToRadians)**2 + (SunAltitudeToScreenNormal/DegToRadians)**2)

!   Use empirical model to determine maximum (peak) scattering
    Tscattermax    = 0.0229d0 * Gamma + 0.2971d0 * ReflectCyl &
                    -0.03624d0 * Gamma**2 + 0.04763d0 * ReflectCyl**2 &
                    -0.44416d0 * Gamma * ReflectCyl
    TscattermaxVis = 0.0229d0 * Gamma + 0.2971d0 * ReflectCylVis &
                    -0.03624d0 * Gamma**2 + 0.04763d0 * ReflectCylVis**2 &
                    -0.44416d0 * Gamma * ReflectCylVis

!   Vary slope of interior and exterior surface of scattering model
    ExponentInterior = (-(ABS(Delta-DeltaMax))**2.0d0)/600.0d0
    ExponentExterior = (-(ABS(Delta-DeltaMax))**2.5d0)/600.0d0

!   Determine ratio of scattering at 0,0 incident angle to maximum (peak) scattering
    PeakToPlateauRatio    = 1.0d0/(0.2d0 * (1-Gamma) * ReflectCyl)
    PeakToPlateauRatioVis = 1.0d0/(0.2d0 * (1-Gamma) * ReflectCylVis)

    IF(Delta .GT. DeltaMax)THEN
!     Apply offset for plateau and use exterior exponential function to simulate actual scattering as a function of solar angles
      Tscattered = 0.2d0 * (1.d0-Gamma) * ReflectCyl * Tscattermax *   &
         (1.0d0 + (PeakToPlateauRatio-1.0d0)*EXP(ExponentExterior))
      TscatteredVis = 0.2d0 * (1.d0-Gamma) * ReflectCylVis * TscattermaxVis *   &
         (1.0d0 + (PeakToPlateauRatioVis-1.0d0)*EXP(ExponentExterior))
!     Trim off offset if solar angle (delta) is greater than maximum (peak) scattering angle
      Tscattered = Tscattered -(0.2d0 * (1.d0-Gamma) * ReflectCyl * Tscattermax) *   &
         MAX(0.0d0,(Delta-DeltaMax)/(90.0d0-DeltaMax))
      TscatteredVis = TscatteredVis -(0.2d0 * (1.d0-Gamma) * ReflectCylVis * TscattermaxVis) *   &
         MAX(0.0d0,(Delta-DeltaMax)/(90.0d0-DeltaMax))
    ELSE
!     Apply offset for plateau and use interior exponential function to simulate actual scattering as a function of solar angles
      Tscattered = 0.2d0 * (1.d0-Gamma) * ReflectCyl * Tscattermax *   &
         (1.0d0 + (PeakToPlateauRatio-1.0d0)*EXP(ExponentInterior))
      TscatteredVis = 0.2d0 * (1.d0-Gamma) * ReflectCylVis * TscattermaxVis *   &
         (1.0d0 + (PeakToPlateauRatioVis-1.0d0)*EXP(ExponentInterior))
    END IF
  END IF
  Tscattered    = MAX(0.0d0,Tscattered)
  TscatteredVis = MAX(0.0d0,TscatteredVis)

  IF(SurfaceScreens(ScNum)%ScreenBeamReflectanceAccounting == DoNotModel)THEN
    IF(ABS(IncidentAngle) .LE. PiOvr2)THEN
      SurfaceScreens(ScNum)%BmBmTrans     = Tdirect
      SurfaceScreens(ScNum)%BmBmTransVis  = Tdirect
      SurfaceScreens(ScNum)%BmBmTransBack = 0.0d0
    ELSE
      SurfaceScreens(ScNum)%BmBmTrans     = 0.0d0
      SurfaceScreens(ScNum)%BmBmTransVis  = 0.0d0
      SurfaceScreens(ScNum)%BmBmTransBack = Tdirect
    END IF
    Tscattered                            = 0.0d0
    TscatteredVis                         = 0.0d0
  ELSE IF(SurfaceScreens(ScNum)%ScreenBeamReflectanceAccounting == ModelAsDirectBeam)THEN
    IF(ABS(IncidentAngle) .LE. PiOvr2)THEN
      SurfaceScreens(ScNum)%BmBmTrans     = Tdirect + Tscattered
      SurfaceScreens(ScNum)%BmBmTransVis  = Tdirect + TscatteredVis
      SurfaceScreens(ScNum)%BmBmTransBack = 0.0d0
    ELSE
      SurfaceScreens(ScNum)%BmBmTrans     = 0.0d0
      SurfaceScreens(ScNum)%BmBmTransVis  = 0.0d0
      SurfaceScreens(ScNum)%BmBmTransBack = Tdirect + Tscattered
    END IF
    Tscattered                            = 0.0d0
    TscatteredVis                         = 0.0d0
  ELSE IF(SurfaceScreens(ScNum)%ScreenBeamReflectanceAccounting == ModelAsDiffuse)THEN
    IF(ABS(IncidentAngle) .LE. PiOvr2)THEN
      SurfaceScreens(ScNum)%BmBmTrans     = Tdirect
      SurfaceScreens(ScNum)%BmBmTransVis  = Tdirect
      SurfaceScreens(ScNum)%BmBmTransBack = 0.0d0
    ELSE
      SurfaceScreens(ScNum)%BmBmTrans     = 0.0d0
      SurfaceScreens(ScNum)%BmBmTransVis  = 0.0d0
      SurfaceScreens(ScNum)%BmBmTransBack = Tdirect
    END IF
  END IF

  IF(ABS(IncidentAngle) .LE. PiOvr2)THEN
    SurfaceScreens(ScNum)%BmDifTrans        = Tscattered
    SurfaceScreens(ScNum)%BmDifTransVis     = TscatteredVis
    SurfaceScreens(ScNum)%BmDifTransBack    = 0.0d0
    SurfaceScreens(ScNum)%ReflectSolBeamFront = MAX(0.0d0,ReflectCyl*(1.d0- Tdirect)-Tscattered)
    SurfaceScreens(ScNum)%ReflectVisBeamFront = MAX(0.0d0,ReflectCylVis*(1.d0- Tdirect)-TscatteredVis)
    SurfaceScreens(ScNum)%AbsorpSolarBeamFront = MAX(0.0d0,(1.0d0-Tdirect)*(1.0d0-ReflectCyl))
    SurfaceScreens(ScNum)%ReflectSolBeamBack = 0.0d0
    SurfaceScreens(ScNum)%ReflectVisBeamBack = 0.0d0
    SurfaceScreens(ScNum)%AbsorpSolarBeamBack = 0.0d0
  ELSE
    SurfaceScreens(ScNum)%BmDifTrans        = 0.0d0
    SurfaceScreens(ScNum)%BmDifTransVis     = 0.0d0
    SurfaceScreens(ScNum)%BmDifTransBack    = Tscattered
    SurfaceScreens(ScNum)%ReflectSolBeamBack = MAX(0.0d0,ReflectCyl*(1.d0- Tdirect)-Tscattered)
    SurfaceScreens(ScNum)%ReflectVisBeamBack = MAX(0.0d0,ReflectCylVis*(1.d0- Tdirect)-TscatteredVis)
    SurfaceScreens(ScNum)%AbsorpSolarBeamBack = MAX(0.0d0,(1.0d0-Tdirect)*(1.0d0-ReflectCyl))
    SurfaceScreens(ScNum)%ReflectSolBeamFront = 0.0d0
    SurfaceScreens(ScNum)%ReflectVisBeamFront = 0.0d0
    SurfaceScreens(ScNum)%AbsorpSolarBeamFront = 0.0d0
  END IF

 RETURN

END SUBROUTINE CalcScreenTransmittance

FUNCTION DisplayMaterialRoughness(Roughness) RESULT(cRoughness)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is given a roughness value and returns the character representation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: Roughness   ! Roughness String
  CHARACTER(len=20)     :: cRoughness  ! Character representation of Roughness

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


    !Select the correct Number for the associated ascii name for the roughness type
    SELECT CASE(Roughness)
      CASE(VeryRough)
        cRoughness='VeryRough'
      CASE(Rough)
        cRoughness='Rough'
      CASE(MediumRough)
        cRoughness='MediumRough'
      CASE(MediumSmooth)
        cRoughness='MediumSmooth'
      CASE(Smooth)
        cRoughness='Smooth'
      CASE(VerySmooth)
        cRoughness='VerySmooth'
      CASE DEFAULT
        cRoughness=' '
    END SELECT

  RETURN

END FUNCTION DisplayMaterialRoughness


FUNCTION ComputeNominalUwithConvCoeffs(numSurf,isValid) RESULT(NominalUwithConvCoeffs)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   September 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate Nominal U-value with convection/film coefficients for reporting by
          ! adding on prescribed R-values for interior and exterior convection coefficients
          ! as found in ASHRAE 90.1-2004, Appendix A. Used in EIO and tabular reports.
          !
          ! ASHRAE 90.1-2004 Section A9.4.1 shows the following:
          !      R-value Condition
          !      All exterior conditions                        IP: 0.17  SI: 0.0299
          !      All semi-exterior surfaces                     IP: 0.46  SI: 0.0810
          !      Interior horizontal surfaces, heat flow up     IP: 0.61  SI: 0.1074
          !      Interior horizontal surfaces, heat flow down   IP: 0.92  SI: 0.1620
          !      Interior vertical surfaces                     IP: 0.68  SI: 0.1198
          ! This section shows the same value in 90.1-2007 and 90.2-2010

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces, ONLY: Surface, SurfaceClass_Wall, SurfaceClass_Floor, &
                          SurfaceClass_Roof, ExternalEnvironment,&
                          Ground,GroundFCfactorMethod, SurfaceClass_Door

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: numSurf    ! index for Surface array.
  LOGICAL, INTENT(OUT)  :: isValid    ! returns true if result is valid
  REAL(r64)             :: NominalUwithConvCoeffs !return value
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: insideFilm
  REAL(r64) ::  outsideFilm


  isValid = .TRUE.
  ! exterior conditions
  SELECT CASE (Surface(numSurf)%ExtBoundCond)
    CASE (ExternalEnvironment)
      outsideFilm = 0.0299387d0 ! All exterior conditions
    CASE (Ground,GroundFCfactorMethod)
      outsideFilm = 0.0d0       ! No outside film when underground
    CASE DEFAULT
      IF (Surface(numSurf)%ExtBoundCond > 0) THEN !interzone partition
        !use companion surface in adjacent zone
        SELECT CASE (Surface(Surface(numSurf)%ExtBoundCond )%class)
        CASE (SurfaceClass_Wall,SurfaceClass_Door)  ! Interior:  vertical, still air, Rcin = 0.68 ft2-F-hr/BTU
          outsideFilm = 0.1197548d0
        CASE (SurfaceClass_Floor) ! Interior:  horizontal, still air, heat flow downward, Rcin = 0.92 ft2-F-hr/BTU
          outsideFilm = 0.1620212d0
        CASE (SurfaceClass_Roof)  ! Interior:  horizontal, still air, heat flow upward, Rcin = 0.61 ft2-F-hr/BTU
          outsideFilm = 0.1074271d0
        CASE DEFAULT
          outsideFilm = 0.0810106d0 ! All semi-exterior surfaces
        END SELECT
      ELSE
        outsideFilm = 0.0810106d0 ! All semi-exterior surfaces
      ENDIF
  END SELECT
  ! interior conditions
  IF (NominalU(Surface(numSurf)%Construction) > 0.0d0) THEN
    SELECT CASE (Surface(numSurf)%class)
      CASE (SurfaceClass_Wall,SurfaceClass_Door)  ! Interior:  vertical, still air, Rcin = 0.68 ft2-F-hr/BTU
        insideFilm = 0.1197548d0
      CASE (SurfaceClass_Floor) ! Interior:  horizontal, still air, heat flow downward, Rcin = 0.92 ft2-F-hr/BTU
        insideFilm = 0.1620212d0
      CASE (SurfaceClass_Roof)  ! Interior:  horizontal, still air, heat flow upward, Rcin = 0.61 ft2-F-hr/BTU
        insideFilm = 0.1074271d0
      CASE DEFAULT
        insideFilm = 0.0d0
        outsideFilm = 0.0d0
    END SELECT
    NominalUwithConvCoeffs = 1.0d0 / (insideFilm + (1.0d0 / NominalU(Surface(numSurf)%Construction)) + outsideFilm)
  ELSE
    isValid = .FALSE.
    NominalUwithConvCoeffs = NominalU(Surface(numSurf)%Construction)
  END IF
END FUNCTION ComputeNominalUwithConvCoeffs

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

END MODULE DataHeatBalance
