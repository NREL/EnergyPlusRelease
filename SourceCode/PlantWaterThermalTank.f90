MODULE WaterThermalTanks

        ! MODULE INFORMATION:
        !       AUTHOR         Brandon Anderson
        !       DATE WRITTEN   May 2000
        !       MODIFIED       Feb 2005, PGE; July 2005, FSEC - added HPWH's and desuperheater water heating coils
        !                      Jan 2007, PGE - added stratified water heater
        !                      Oct 2007, BTG - extended for indirect water heater
        !                      May 2008, Stovall - added desup from condenser and removed double counting
        !                           (includes "d0"s from revision 145)
        !                       Nov 2011, BAN; corrected use and source outlet temp. calculation of stratified tank
        !       RE-ENGINEERED  Feb 2004, PGE
        !                      Sep 2008, BTG - refactored, was PlantWaterHeater.f90 is now PlantWaterThermalTank.f90
        !                                 reuse water heater code for chilled water storage

        ! PURPOSE OF THIS MODULE:
        ! This module simulates water thermal storage tanks heaters in the plant loop.  Tanks can
        ! be positioned as supply side equipment or demand side equipment.  Water heater versions can be stand-alone as
        ! non-zone equipment.

        ! METHODOLOGY EMPLOYED:
        ! Two water thermal tank models are implemented, MIXED and STRATIFIED with hot and cold versions of each:
        !
        ! WaterHeater:Mixed simulates a well-mixed, single-node tank for hot water applications.  Source (e.g. heat recovery) and
        ! use plant connections are allowed.  A scheduled domestic hot water demand can also be specified
        ! to directly utilize the hot water without use side connections.
        !
        ! WaterHeater:Stratified simulates a stratified, multi-node tank for hot water applicatons.
        ! The model shares most of the same capabilities as WaterHeater:Mixed
        ! but also has up to two heating elements which can be operated in
        ! a master-slave mode or simultaneous mode.

        ! ThermalStorage:ChilledWater:Mixed simulates a well-mixed, single-node tank for chilled water applications

        ! ThermalStorage:ChilledWater:Stratified simulates a stratified, multi-node tank for chilled water applications.

        ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals,     ONLY: MaxNameLength, NumOfTimeStepInHour, InitConvTemp, SecInHour, OutputFileInits
USE DataHeatBalance, ONLY: NumRefrigeratedRacks, HeatReclaimRefrigeratedRack, HeatReclaimRefrigCondenser, &
                           HeatReclaimDXCoil, NumRefrigCondensers
USE DataPlant
USE General, ONLY: TrimSigDigits
USE DataInterfaces
USE ReportSizingManager, ONLY: ReportSizingOutput

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: cMixedWHModuleObj          = 'WaterHeater:Mixed'
CHARACTER(len=*), PARAMETER :: cStratifiedWHModuleObj     = 'WaterHeater:Stratified'
CHARACTER(len=*), PARAMETER :: cMixedCWTankModuleObj      = 'ThermalStorage:ChilledWater:Mixed'
CHARACTER(len=*), PARAMETER :: cStratifiedCWTankModuleObj = 'ThermalStorage:ChilledWater:Stratified'

INTEGER, PARAMETER :: HeatMode               =  1 ! heating source is on, source will not turn off until setpoint temp is reached
INTEGER, PARAMETER :: FloatMode              =  0 ! heating source is off, source will not turn on until cutin temp is reached
INTEGER, PARAMETER :: VentMode               = -1 ! tank temp is above maximum temperature and water is venting
INTEGER, PARAMETER :: CoolMode               =  2 ! cooling source is on, source will not turn off until setpoint temp is reached

INTEGER, PARAMETER :: AmbientTempSchedule    =  1 ! ambient temperature around tank (or HPWH inlet air) is scheduled
INTEGER, PARAMETER :: AmbientTempZone        =  2 ! tank is located in a zone or HPWH inlet air is zone air only
INTEGER, PARAMETER :: AmbientTempOutsideAir  =  3 ! tank is located outdoors or HPWH inlet air is outdoor air only
INTEGER, PARAMETER :: AmbientTempZoneAndOA   =  4 ! applicable to HPWH only, inlet air is mixture of OA and zone air

INTEGER, PARAMETER :: CrankcaseTempSchedule  =  1 ! temperature controlling compressor crankcase heater is scheduled
INTEGER, PARAMETER :: CrankcaseTempZone      =  2 ! temperature controlling compressor crankcase heater is zone air
INTEGER, PARAMETER :: CrankcaseTempExterior  =  3 ! temperature controlling compressor crankcase heater is outdoor air

INTEGER, PARAMETER :: ControlTypeCycle       =  1 ! water heater only, cycling heating source control
INTEGER, PARAMETER :: ControlTypeModulate    =  2 ! water heater only, modulating heating source control

INTEGER, PARAMETER :: TankShapeVertCylinder  =  1 ! tank shape is a vertical cylinder
INTEGER, PARAMETER :: TankShapeHorizCylinder =  2 ! tank shape is a horizontal cylinder
INTEGER, PARAMETER :: TankShapeOther         =  3 ! tank shape has an arbitrary perimeter shape

INTEGER, PARAMETER :: PriorityMasterSlave    =  1 ! water heater only, master-slave priority control of heater elements
INTEGER, PARAMETER :: PrioritySimultaneous   =  2 ! water heater only, simultaneous control of heater elements

INTEGER, PARAMETER :: InletModeFixed         =  1 ! water heater only, inlet water always enters at the user-specified height
INTEGER, PARAMETER :: InletModeSeeking       =  2 ! water heater only, inlet water seeks out the node with the closest temperature

! integer parameter for water heater
INTEGER, PARAMETER :: MixedWaterHeater       =  TypeOf_WtrHeaterMixed ! WaterHeater:Mixed
INTEGER, PARAMETER :: StratifiedWaterHeater  =  TypeOf_WtrHeaterStratified ! WaterHeater:Stratified
INTEGER, PARAMETER :: HeatPumpWaterHeater    =  TypeOf_HeatPumpWtrHeater ! WaterHeater:HeatPump
!stovall, next line never used because all desuperheater coils used in mixed water heater types
INTEGER, PARAMETER :: CoilWaterDesuperHeater =  4 ! Coil:WaterHeating:Desuperheater
INTEGER, PARAMETER :: MixedChilledWaterStorage = TypeOf_ChilledWaterTankMixed ! 'ThermalStorage:ChilledWater:Mixed'
INTEGER, PARAMETER :: StratifiedChilledWaterStorage = TypeOf_ChilledWaterTankStratified ! 'ThermalStorage:ChilledWater:Stratified'

! reclaim heat object types for Coil:WaterHeating:Desuperheater object
INTEGER, PARAMETER :: COMPRESSORRACK_REFRIGERATEDCASE = 1 ! reclaim heating source is refrigerated case compressor rack
INTEGER, PARAMETER :: COIL_DX_COOLING                 = 2 ! reclaim heating source is DX cooling coil
INTEGER, PARAMETER :: COIL_DX_MULTISPEED              = 3 ! reclaim heating source is DX multispeed coil
INTEGER, PARAMETER :: COIL_DX_MULTIMODE               = 4 ! reclaim heating source is DX multimode coil
INTEGER, PARAMETER :: CONDENSER_REFRIGERATION         = 5 ! reclaim heating source is detailed refrigeration system condenser

INTEGER, PARAMETER :: UseSide                = 101 ! Indicates Use side of water heater
INTEGER, PARAMETER :: SourceSide             = 102 ! Indicates Source side of water heater

INTEGER, PARAMETER :: SizeNotSet             = 200
INTEGER, PARAMETER :: SizePeakDraw           = 201
INTEGER, PARAMETER :: SizeResidentialMin     = 202
INTEGER, PARAMETER :: SizePerPerson          = 203
INTEGER, PARAMETER :: SizePerFloorArea       = 204
INTEGER, PARAMETER :: SizePerUnit            = 205
INTEGER, PARAMETER :: SizePerSolarColArea    = 206

INTEGER, PARAMETER :: HPWHControlNotSet      = 500
INTEGER, PARAMETER :: Heater1HPWHControl     = 501
INTEGER, PARAMETER :: Heater2HPWHControl     = 502
INTEGER, PARAMETER :: SourceInletHPWHControl = 503
INTEGER, PARAMETER :: SourceOutletHPWHControl= 504
INTEGER, PARAMETER :: UseInletHPWHControl    = 505
INTEGER, PARAMETER :: UseOutletHPWHControl   = 506

INTEGER, PARAMETER :: SourceSideStorageTank                 = 600
INTEGER, PARAMETER :: SourceSideIndirectHeatPrimarySetpoint = 601
INTEGER, PARAMETER :: SourceSideIndirectHeatAltSetpoint     = 602

          ! DERIVED TYPE DEFINITIONS:
TYPE StratifiedNodeData
  REAL(r64) :: Mass = 0.0d0                        ! All nodes have the same mass (kg)
  REAL(r64) :: OnCycLossCoeff = 0.0d0
  REAL(r64) :: OffCycLossCoeff = 0.0d0

  REAL(r64) :: Temp = 0.0d0
  REAL(r64) :: SavedTemp = 0.0d0
  REAL(r64) :: NewTemp = 0.0d0

  REAL(r64) :: TempSum = 0.0d0
  REAL(r64) :: TempAvg = 0.0d0                    ! Average node temperature over the time step (C)

  REAL(r64) :: CondCoeffUp = 0.0d0
  REAL(r64) :: CondCoeffDn = 0.0d0

  REAL(r64) :: OffCycParaLoad = 0.0d0   ! Heat delivered to the tank from off-cycle parasitic sources
  REAL(r64) :: OnCycParaLoad = 0.0d0

  REAL(r64) :: UseMassFlowRate = 0.0d0
  REAL(r64) :: SourceMassFlowRate = 0.0d0

  REAL(r64) :: MassFlowFromUpper = 0.0d0  ! Mass flow rate into this node from node above
  REAL(r64) :: MassFlowFromLower = 0.0d0  ! Mass flow rate into this node from node below
  REAL(r64) :: MassFlowToUpper = 0.0d0  ! Mass flow rate from this node to node above
  REAL(r64) :: MassFlowToLower = 0.0d0  ! Mass flow rate from this node to node below

  ! Report Variables
  REAL(r64) :: Volume = 0.0d0
  REAL(r64) :: Height = 0.0d0  ! Node height from top to bottom (like a thickness)

  REAL(r64) :: MaxCapacity = 0.0d0  ! For reporting

  INTEGER :: Inlets = 0
  INTEGER :: Outlets = 0

END TYPE StratifiedNodeData
TYPE WaterHeaterSizingData
  ! input data
  INTEGER :: DesignMode                        = SizeNotSet  ! what sizing method to use
  REAL(r64)    :: TankDrawTime                 = 0.0D0 ! in hours, time storage can meet peak demand
  REAL(r64)    :: RecoveryTime                 = 0.0D0 ! time for tank to recover
  REAL(r64)    :: NominalVolForSizingDemandSideFlow = 0.0D0 ! nominal tank size to use in sizing demand side connections
  INTEGER      :: NumberOfBedrooms             = 0 !
  REAL(r64)    :: NumberOfBathrooms            = 0.0D0 !
  REAL(r64)    :: TankCapacityPerPerson        = 0.0D0 !
  REAL(r64)    :: RecoveryCapacityPerPerson    = 0.0D0
  REAL(r64)    :: TankCapacityPerArea          = 0.0D0
  REAL(r64)    :: RecoveryCapacityPerArea      = 0.0D0
  REAL(r64)    :: NumberOfUnits                = 0.0D0
  REAL(r64)    :: TankCapacityPerUnit          = 0.0D0
  REAL(r64)    :: RecoveryCapacityPerUnit      = 0.0D0
  REAL(r64)    :: TankCapacityPerCollectorArea = 0.0D0
  REAL(r64)    :: HeightAspectRatio            = 0.0D0

  ! data from elsewhere in E+
  REAL(r64)    :: PeakDemand                   = 0.0D0
  REAL(r64)    :: PeakNumberOfPeople           = 0.0D0
  REAL(r64)    :: TotalFloorArea               = 0.0D0
  REAL(r64)    :: TotalSolarCollectorArea      = 0.0D0

END TYPE WaterHeaterSizingData


TYPE WaterThermalTankData
  CHARACTER(len=MaxNameLength) :: Name = ''                     ! Name of water heater
  CHARACTER(len=MaxNameLength) :: Type = ''                     ! Type of water heater (MIXED or STRATIFIED)
  INTEGER                      :: TypeNum = 0                   ! integer parameter for water heater(if part of an HPWH,then=HPWH)
  LOGICAL                      :: IsChilledWaterTank = .FALSE.  ! logical flag, true if for chilled water, false if for hot water
  CHARACTER(len=MaxNameLength) :: EndUseSubcategoryName = ''    ! User-defined end-use subcategory name
  LOGICAL                      :: Init = .TRUE.                 ! Flag for initialization:  TRUE means do the init
  LOGICAL                      :: StandAlone = .FALSE.          ! Flag for operation with no plant connections (no source or use)
  REAL(r64)                    :: Volume = 0.0d0                  ! Tank volume (m3)
  REAL(r64)                    :: Mass = 0.0d0                    ! Total mass of fluid in the tank (kg)

  REAL(r64)                    :: TimeElapsed = 0.0d0             ! Fraction of the current hour that has elapsed (h)
                                                                ! Saved in order to identify the beginning of a new system time

  INTEGER                      :: AmbientTempIndicator = 0      ! Indicator for ambient tank losses (SCHEDULE, ZONE, EXTERIOR)
  INTEGER                      :: AmbientTempSchedule = 0       ! Schedule index pointer
  INTEGER                      :: AmbientTempZone = 0           ! Number of ambient zone around tank
  INTEGER                      :: AmbientTempOutsideAirNode = 0 ! Number of outside air node
  REAL(r64)                    :: AmbientTemp = 0.0d0             ! Ambient temperature around tank (C)
  REAL(r64)                    :: AmbientZoneGain = 0.0d0         ! Internal gain to zone from tank losses (W)
  REAL(r64)                    :: LossCoeff = 0.0d0               ! Overall tank heat loss coefficient, UA (W/K)
  REAL(r64)                    :: OffCycLossCoeff = 0.0d0         ! Off-cycle overall tank heat loss coefficient, UA (W/K)
  REAL(r64)                    :: OffCycLossFracToZone = 0.0d0    ! Fraction of off-cycle losses added to zone
  REAL(r64)                    :: OnCycLossCoeff = 0.0d0          ! On-cycle overall tank heat loss coefficient, UA (W/K)
  REAL(r64)                    :: OnCycLossFracToZone = 0.0d0     ! Fraction of on-cycle losses added to zone

  INTEGER                      :: Mode = 0                      ! Indicator for current operating mode
  INTEGER                      :: SavedMode = 0                 ! Mode indicator saved from previous time step
  INTEGER                      :: ControlType = 1               ! Indicator for control type
  CHARACTER(len=MaxNameLength) :: FuelType = ''                 ! Fuel type
  REAL(r64)                    :: MaxCapacity = 0.0d0             ! Maximum capacity of auxiliary heater 1 (W)
  REAL(r64)                    :: MinCapacity = 0.0d0             ! Minimum capacity of auxiliary heater 1 (W)
  REAL(r64)                    :: Efficiency = 0.0d0              ! Thermal efficiency of auxiliary heater 1 ()
  INTEGER                      :: PLFCurve = 0                  ! Part load factor curve as a function of part load ratio
  INTEGER                      :: SetpointTempSchedule = 0      ! Schedule index pointer
  REAL(r64)                    :: SetpointTemp = 0.0d0            ! Setpoint temperature of auxiliary heater 1 (C)
  REAL(r64)                    :: DeadbandDeltaTemp = 0.0d0       ! Deadband temperature difference of auxiliary heater 1 (deltaC)
  REAL(r64)                    :: TankTempLimit = 0.0d0           ! Maximum tank temperature limit before venting (C)
  REAL(r64)                    :: IgnitionDelay = 0.0d0           ! Time delay before heater is allowed to turn on (s)

  REAL(r64)                    :: OffCycParaLoad = 0.0d0          ! Rate for off-cycle parasitic load (W)
  CHARACTER(len=MaxNameLength) :: OffCycParaFuelType = ''       ! Fuel type for off-cycle parasitic load
  REAL(r64)                    :: OffCycParaFracToTank = 0.0d0    ! Fraction of off-cycle parasitic energy ending up in tank (W)

  REAL(r64)                    :: OnCycParaLoad = 0.0d0           ! Rate for on-cycle parasitic load (W)
  CHARACTER(len=MaxNameLength) :: OnCycParaFuelType = ''        ! Fuel type for on-cycle parasitic load
  REAL(r64)                    :: OnCycParaFracToTank = 0.0d0     ! Fraction of on-cycle parasitic energy ending up in tank (W)

  INTEGER                      :: UseCurrentFlowLock = 0        ! current flow lock setting on use side

  INTEGER                      :: UseInletNode = 0              ! Inlet node on the use side; colder water returning to a hottank
  REAL(r64)                    :: UseInletTemp = 0.0d0            ! Use side inlet temperature (C)
  INTEGER                      :: UseOutletNode = 0             ! Outlet node on the use side; hot tank water
  REAL(r64)                    :: UseOutletTemp = 0.0d0           ! Use side outlet temperature (C)
  REAL(r64)                    :: UseMassFlowRate = 0.0d0         ! Mass flow rate on the use side (kg/s)
  REAL(r64)                    :: UseEffectiveness = 0.0d0        ! Heat transfer effectiveness on use side ()
  REAL(r64)                    :: PlantUseMassFlowRateMax = 0.0d0 ! Plant demand-side max flow request on use side (kg/s)
  REAL(r64)                    :: SavedUseOutletTemp = 0.0d0      ! Use side outlet temp saved for demand-side flow control (C)
  REAL(r64)                    :: UseDesignVolFlowRate = 0.0d0    ! Use side plant volume flow rate (input data, autosizable) m3/s
  INTEGER                      :: UseBranchControlType = 2      ! Use side plant branch control type e.g active, passive, bypass
  INTEGER                      :: UseSidePlantSizNum = 0        ! index in plant sizing that the use side is on
  LOGICAL                      :: UseSideSeries = .TRUE.
  INTEGER                      :: UseSideAvailSchedNum = 0      ! use side availability schedule
  REAL(r64)                    :: UseSideLoadRequested = 0.0D0  ! hold MyLoad request from plant management.
  INTEGER                      :: UseSidePlantLoopNum = 0       ! if not zero, then this use side is on plant loop #
  INTEGER                      :: UseSidePlantLoopSide = 0      ! use side is on loop side index
  INTEGER                      :: UseSidePlantBranchNum = 0     ! use side branch num in plant topology
  INTEGER                      :: UseSidePlantCompNum  = 0      ! use side component num in plant topology

  INTEGER                      :: SourceCurrentFlowLock = 0     ! current flow lock setting on source side
  INTEGER                      :: SourceInletNode = 0           ! Inlet node for the source side; hot water from supply
  REAL(r64)                    :: SourceInletTemp = 0.0d0         ! Source side inlet temperature (C)
  INTEGER                      :: SourceOutletNode = 0          ! Outlet node for the source side; colder tank water
  REAL(r64)                    :: SourceOutletTemp = 0.0d0        ! Source side outlet temperature (C)
  REAL(r64)                    :: SourceMassFlowRate = 0.0d0      ! Mass flow rate on the source side (kg/s)
  REAL(r64)                    :: SourceEffectiveness = 0.0d0     ! Heat transfer effectiveness on source side ()
  REAL(r64)                    :: PlantSourceMassFlowRateMax = 0.0d0 ! Plant demand-side max flow request on source side (kg/s)
  REAL(r64)                    :: SavedSourceOutletTemp = 0.0d0   ! Source side outlet temp saved for demand-side flow control (C)
  REAL(r64)                    :: SourceDesignVolFlowRate = 0.0d0 ! Source side plant volume flow rate (input , autosizable) m3/s
  INTEGER                      :: SourceBranchControlType = 2   ! source side plant branch control type e.g active, passive, bypass
  INTEGER                      :: SourceSidePlantSizNum = 0     ! index in plant sizing that the source side is on
  LOGICAL                      :: SourceSideSeries = .TRUE.
  INTEGER                      :: SourceSideAvailSchedNum = 0   ! source side availability schedule.
  INTEGER                      :: SourceSidePlantLoopNum = 0    ! if not zero, then this use side is on plant loop #
  INTEGER                      :: SourceSidePlantLoopSide = 0    ! loop side that Source side is one eg. supply or demand
  INTEGER                      :: SourceSidePlantBranchNum = 0   ! Source side branch num in plant topology
  INTEGER                      :: SourceSidePlantCompNum  = 0    ! Source side component num in plant topology
  INTEGER                      :: SourceSideControlMode = 0      ! flag for how source side flow is controlled 
  INTEGER                      :: SourceSideAltSetpointSchedNum = 0 ! schedule of alternate temperature setpoint values 

  REAL(r64)                    :: SizingRecoveryTime = 0.0d0      ! sizing parameter for autosizing indirect water heaters (hr)

  REAL(r64)                    :: MassFlowRateMax = 0.0d0         ! Maximum flow rate for scheduled DHW (kg/s)
  REAL(r64)                    :: VolFlowRateMin  = 0.d0        ! Minimum flow rate for heater ignition (kg/s)
  REAL(r64)                    :: MassFlowRateMin = 0.0d0         ! Minimum mass flow rate for heater ignition (kg/s)
  INTEGER                      :: FlowRateSchedule = 0          ! Schedule index pointer
  INTEGER                      :: UseInletTempSchedule = 0      ! Cold water supply temperature schedule index pointer

  REAL(r64)                    :: TankTemp = 0.0d0                ! Temperature of tank fluid (average, if stratified) (C)
  REAL(r64)                    :: SavedTankTemp = 0.0d0           ! Tank temp that is carried from time step to time step (C)
  REAL(r64)                    :: TankTempAvg = 0.0d0             ! Average tank temperature over the time step (C)

  ! Stratified variables (in addition to the above)
  REAL(r64)                    :: Height = 0.0d0                  ! Height of tank (m)
  REAL(r64)                    :: Perimeter = 0.0d0               ! Perimeter of tank (m), only used for OTHER shape
  INTEGER                      :: Shape = 0                     ! Tank shape:  VERTICAL CYLINDER, HORIZONTAL CYLINDER, or OTHER

  REAL(r64)                    :: HeaterHeight1 = 0.0d0
  INTEGER                      :: HeaterNode1 = 0
  LOGICAL                      :: HeaterOn1 = .FALSE.
  LOGICAL                      :: SavedHeaterOn1 = .FALSE.

  REAL(r64)                    :: HeaterHeight2 = 0.0d0
  INTEGER                      :: HeaterNode2 = 0
  LOGICAL                      :: HeaterOn2 = .FALSE.
  LOGICAL                      :: SavedHeaterOn2 = .FALSE.

  REAL(r64)                    :: AdditionalCond = 0.0d0           ! Additional destratification conductivity (W/m K)

  REAL(r64)                    :: SetpointTemp2 = 0.0d0            ! Setpoint temperature of auxiliary heater 2 (C)
  INTEGER                      :: SetpointTempSchedule2 = 0
  REAL(r64)                    :: DeadbandDeltaTemp2 = 0.0d0
  REAL(r64)                    :: MaxCapacity2 = 0.0d0

  REAL(r64)                    :: OffCycParaHeight = 0.0d0
  REAL(r64)                    :: OnCycParaHeight = 0.0d0

  REAL(r64)                    :: SkinLossCoeff = 0.0d0
  REAL(r64)                    :: SkinLossFracToZone = 0.0d0
  REAL(r64)                    :: OffCycFlueLossCoeff = 0.0d0
  REAL(r64)                    :: OffCycFlueLossFracToZone = 0.0d0

  REAL(r64)                    :: UseInletHeight = 0.0d0          ! Height of use side inlet (m)
  REAL(r64)                    :: UseOutletHeight = 0.0d0         ! Height of use side outlet (m)
  REAL(r64)                    :: SourceInletHeight = 0.0d0       ! Height of source side inlet (m)
  REAL(r64)                    :: SourceOutletHeight = 0.0d0      ! Height of source side outlet (m)

  INTEGER                      :: UseInletStratNode = 0         ! Use-side inlet node number
  INTEGER                      :: UseOutletStratNode = 0        ! Use-side outlet node number
  INTEGER                      :: SourceInletStratNode = 0      ! Source-side inlet node number
  INTEGER                      :: SourceOutletStratNode = 0     ! Source-side outlet node number

  INTEGER                      :: InletMode = 1                 ! Inlet position mode:  1 = FIXED; 2 = SEEKING

  REAL(r64)                    :: InversionMixingRate = 0.0d0

  REAL(r64), DIMENSION(:), ALLOCATABLE :: AdditionalLossCoeff        ! Loss coefficient added to the skin loss coefficient (W/m2-K)

  INTEGER                      :: Nodes = 0                     ! Number of nodes
  TYPE(StratifiedNodeData), DIMENSION(:), ALLOCATABLE :: Node   ! Array of node data

  ! Report variables
  REAL(r64)                    :: VolFlowRate = 0.0d0             ! Scheduled DHW demand (m3/s)
  REAL(r64)                    :: VolumeConsumed = 0.0d0          ! Volume of DHW consumed (m3)

  REAL(r64)                    :: UnmetRate = 0.0d0               ! Energy demand to heat tank water to setpoint (W)
  REAL(r64)                    :: LossRate = 0.0d0                ! Energy demand to support heat losses due to ambient temp (W)
  REAL(r64)                    :: FlueLossRate = 0.0d0            ! Heat loss rate to flue (W)
  REAL(r64)                    :: UseRate = 0.0d0                 ! Energy demand to heat the Use Side water to tank temp (W)
  REAL(r64)                    :: TotalDemandRate = 0.0d0         ! Total demand rate (sum of all above rates) (W)
  REAL(r64)                    :: SourceRate = 0.0d0              ! Energy supplied by the source side to help heat the tank (W)
  REAL(r64)                    :: HeaterRate = 0.0d0              ! The energy the water heater burner puts into the water (W)
  REAL(r64)                    :: HeaterRate1 = 0.0d0             ! The energy heater 1 puts into the water (W)
  REAL(r64)                    :: HeaterRate2 = 0.0d0             ! The energy heater 2 puts into the water (W)
  REAL(r64)                    :: FuelRate = 0.0d0                ! The fuel consumption rate for the water heater burner (W)
  REAL(r64)                    :: FuelRate1 = 0.0d0               ! The fuel consumption rate for heater 1 (W)
  REAL(r64)                    :: FuelRate2 = 0.0d0               ! The fuel consumption rate for heater 2 (W)
  REAL(r64)                    :: VentRate = 0.0d0                ! Heat recovery energy lost due to setpoint temp (W)
  REAL(r64)                    :: OffCycParaFuelRate = 0.0d0      ! Fuel consumption rate for off-cycle parasitic load (W)
  REAL(r64)                    :: OffCycParaRateToTank = 0.0d0    ! Heat rate to tank for off-cycle parasitic load (W)
  REAL(r64)                    :: OnCycParaFuelRate = 0.0d0       ! Fuel consumption rate for on-cycle parasitic load (W)
  REAL(r64)                    :: OnCycParaRateToTank = 0.0d0     ! Heat rate to tank for on-cycle parasitic load (W)
  REAL(r64)                    :: NetHeatTransferRate = 0.0d0     ! Net heat transfer rate to/from tank (W)

  INTEGER                      :: CycleOnCount = 0              ! Number of times heater cycles on in the current time step
  INTEGER                      :: CycleOnCount1 = 0             ! Number of times heater 1 cycles on in the current time step
  INTEGER                      :: CycleOnCount2 = 0             ! Number of times heater 2 cycles on in the current time step
  REAL(r64)                    :: RuntimeFraction = 0.0d0      ! Runtime fraction, fraction of timestep that any  heater is running
  REAL(r64)                    :: RuntimeFraction1 = 0.0d0        ! Runtime fraction, fraction of timestep that heater 1 is running
  REAL(r64)                    :: RuntimeFraction2 = 0.0d0        ! Runtime fraction, fraction of timestep that heater 2 is running
  REAL(r64)                    :: PartLoadRatio = 0.0d0           ! Part load ratio, fraction of maximum heater capacity

  REAL(r64)                    :: UnmetEnergy = 0.0d0             ! Energy to heat tank water to setpoint (J)
  REAL(r64)                    :: LossEnergy = 0.0d0              ! Energy to support heat losses due to ambient temp (J)
  REAL(r64)                    :: FlueLossEnergy = 0.0d0          ! Energy to support heat losses to the flue (J)
  REAL(r64)                    :: UseEnergy = 0.0d0               ! Energy to heat the use side water to tank temp (J)
  REAL(r64)                    :: TotalDemandEnergy = 0.0d0       ! Total energy demand (sum of all above energies) (J)
  REAL(r64)                    :: SourceEnergy = 0.0d0            ! Energy supplied by the source side to help heat the tank (J)
  REAL(r64)                    :: HeaterEnergy = 0.0d0            ! The energy the water heater burner puts into the water (J)
  REAL(r64)                    :: HeaterEnergy1 = 0.0d0           ! The energy heater 1 puts into the water (J)
  REAL(r64)                    :: HeaterEnergy2 = 0.0d0           ! The energy heater 2 puts into the water (J)
  REAL(r64)                    :: FuelEnergy = 0.0d0              ! The fuel consumption energy for the water heater burner (J)
  REAL(r64)                    :: FuelEnergy1 = 0.0d0             ! The fuel consumption energy for heater 1 (J)
  REAL(r64)                    :: FuelEnergy2 = 0.0d0             ! The fuel consumption energy for heater 2 (J)
  REAL(r64)                    :: VentEnergy = 0.0d0              ! Heat recovery energy lost due to setpoint temp (J)
  REAL(r64)                    :: OffCycParaFuelEnergy = 0.0d0    ! Fuel consumption energy for off-cycle parasitic load (J)
  REAL(r64)                    :: OffCycParaEnergyToTank = 0.0d0  ! Energy to tank for off-cycle parasitic load (J)
  REAL(r64)                    :: OnCycParaFuelEnergy = 0.0d0     ! Fuel consumption energy for on-cycle parasitic load (J)
  REAL(r64)                    :: OnCycParaEnergyToTank = 0.0d0   ! Energy to tank for on-cycle parasitic load (J)
  REAL(r64)                    :: NetHeatTransferEnergy = 0.0d0   ! Net heat transfer energy to/from tank (J)

  LOGICAL                      :: FirstRecoveryDone = .FALSE.   ! Flag to indicate when first recovery to the setpoint is done
  REAL(r64)                    :: FirstRecoveryFuel = 0.0d0       ! Fuel energy needed for first recovery to the setpoint (J)
  INTEGER                      :: HeatPumpNum = 0               ! Index to heat pump water heater
  INTEGER                      :: DesuperheaterNum = 0          ! Index to desuperheating coil
  LOGICAL                      :: ShowSetpointWarning = .TRUE.  ! Warn when set point is greater than max tank temp limit
  INTEGER                      :: MaxCycleErrorIndex = 0        ! recurring error index
  Type(WaterHeaterSizingData)  :: Sizing                        ! ancillary data for autosizing
END TYPE WaterThermalTankData


TYPE HeatPumpWaterHeaterData
  CHARACTER(len=MaxNameLength) :: Name                     = ''  ! Name of heat pump water heater
  CHARACTER(len=MaxNameLength) :: Type                     = ''  ! Type of water heater (HEAT PUMP:WATER HEATER)
  INTEGER                      :: TypeNum                  = 0   ! integer parameter for heat pump water heater
  CHARACTER(len=MaxNameLength) :: TankName                 = ''  ! Name of tank associated with heat pump water heater
  CHARACTER(len=MaxNameLength) :: TankType                 = ''  ! Type of water heater (MIXED or STRATIFIED) used with heat pump
  INTEGER                      :: TankTypeNum              = 0   ! Parameter for tank type (MIXED or STRATIFIED)
  LOGICAL                      :: StandAlone          = .FALSE.  ! Flag for operation with no plant connections (no use nodes)
  INTEGER                      :: AvailSchedPtr            = 0   ! Index to Availability Schedule curve index
  INTEGER                      :: SetpointTempSchedule     = 0   ! Index to Setpoint Temperature Schedule curve
  REAL(r64)                    :: DeadbandTempDiff         = 0.0d0 ! Dead band temperature difference (cut-in temperature)
  REAL(r64)                    :: Capacity                 = 0.0d0 ! Heat Pump rated capacity (W)
  REAL(r64)                    :: BackupElementCapacity    = 0.0d0 ! Tank backup element capacity (W)
  REAL(r64)                    :: BackupElementEfficiency  = 0.0d0 ! Tank backup element efficiency
  REAL(r64)                    :: WHOnCycParaLoad          = 0.0d0 ! tank's on-cycle parasitic load (W), disable for rating
  REAL(r64)                    :: WHOffCycParaLoad         = 0.0d0 ! tank's off-cycle parasitic load (W), disable for rating
  REAL(r64)                    :: WHOnCycParaFracToTank    = 0.0d0 ! tank's on-cycle parasitic frac to tank, disable for rating
  REAL(r64)                    :: WHOffCycParaFracToTank   = 0.0d0 ! tank's off-cycle parasitic frac to tank, disable for rating
  INTEGER                      :: WHPLFCurve               = 0   ! tank part-load fraction curve index, used for rating procedure
  REAL(r64)                    :: OperatingAirFlowRate     = 0.0d0 ! Operating volumetric air flow rate (m3/s)
  REAL(r64)                    :: OperatingWaterFlowRate   = 0.0d0 ! Operating volumetric water flow rate (m3/s)
  REAL(r64)                    :: COP                      = 0.0d0 ! Heat Pump coefficient of performance (W/W)
  REAL(r64)                    :: SHR                      = 0.0d0 ! Heat Pump air-side coil sensible heat ratio
  REAL(r64)                    :: RatedInletDBTemp         = 0.0d0 ! Rated evaporator inlet air dry-bulb temperature (C)
  REAL(r64)                    :: RatedInletWBTemp         = 0.0d0 ! Rated evaporator inlet air wet-bulb temperature (C)
  REAL(r64)                    :: RatedInletWaterTemp      = 0.0d0 ! Rated condenser inlet water temperature (C)
  LOGICAL                      :: FoundTank            = .FALSE. ! Found storage tank flag associated with HP water heater
  INTEGER                      :: HeatPumpAirInletNode     = 0   ! HP air inlet node (for zone, zone/outdoor or scheduled)
  INTEGER                      :: HeatPumpAirOutletNode    = 0   ! HP air outlet node (for zone, zone/outdoor or scheduled)
  INTEGER                      :: OutsideAirNode           = 0   ! outdoor air node (for outdoor or zone/outdoor air unit only)
  INTEGER                      :: ExhaustAirNode           = 0   ! Exhaust air node (for outdoor or zone/outdoor air unit only)
  INTEGER                      :: CondWaterInletNode       = 0   ! Condenser water inlet node
  INTEGER                      :: CondWaterOutletNode      = 0   ! Condenser water outlet node
  INTEGER                      :: WHUseInletNode           = 0   ! Water heater tank use side inlet node
  INTEGER                      :: WHUseOutletNode          = 0   ! Water heater tank use side outlet node
  INTEGER                      :: WHUseSidePlantLoopNum    = 0   ! if not zero, then this water heater is on plant loop #
  CHARACTER(len=MaxNameLength) :: DXCoilType               = ''  ! Type of DX coil (Coil:DX:HeatPumpWaterHeater)
  CHARACTER(len=MaxNameLength) :: DXCoilName               = ''  ! Name of DX coil
  INTEGER                      :: DXCoilNum                = 0   ! Index of DX coil
  INTEGER                      :: DXCoilAirInletNode       = 0   ! Inlet air node number of DX coil
  INTEGER                      :: DXCoilPLFFPLR            = 0   ! Index to HPWH's DX Coil PLF as a function of PLR curve
  CHARACTER(len=MaxNameLength) :: FanType                  = ''  ! Type of Fan (Fan:OnOff)
  INTEGER                      :: FanType_Num              = 0   ! Integer type of fan (3 = Fan:OnOff)
  CHARACTER(len=MaxNameLength) :: FanName                  = ''  ! Name of Fan
  INTEGER                      :: FanNum                   = 0   ! Index of Fan
  INTEGER                      :: FanPlacement             = 0   ! Location of Fan
  INTEGER                      :: FanOutletNode            = 0   ! Outlet node of heat pump water heater fan
  INTEGER                      :: WaterHeaterTankNum       = 0   ! Index of Water Heater Tank
  INTEGER                      :: OutletAirSplitterSchPtr  = 0   ! Index to air-side outlet air splitter schedule
  INTEGER                      :: InletAirMixerSchPtr      = 0   ! Index to air-side inlet air mixer schedule
  INTEGER                      :: Mode                     = 0   ! HP mode (0 = float, 1 = heating [-1 = venting na for HP])
  INTEGER                      :: SaveMode                 = 0   ! HP mode on first iteration
  INTEGER                      :: SaveWHMode               = 0   ! mode of water heater tank element (backup element)
  REAL(r64)                    :: Power                    = 0.0d0 ! HP power used for reporting
  REAL(r64)                    :: Energy                   = 0.0d0 ! HP energy used for reporting
  REAL(r64)                    :: HeatingPLR               = 0.0d0 ! HP PLR used for reporting
  REAL(r64)                    :: SetpointTemp             = 0.0d0 ! set point or cut-out temperature [C]
  REAL(r64)                    :: MinAirTempForHPOperation = 5.0d0 ! HP does not operate below this ambient temperature
  INTEGER                      :: InletAirMixerNode        = 0   ! Inlet air mixer node number of HP water heater
  INTEGER                      :: OutletAirSplitterNode    = 0   ! Outlet air splitter node number of HP water heater
  REAL(r64)                    :: SourceMassFlowRate       = 0.0d0 ! Maximum mass flow rate on the source side (kg/s)
  INTEGER                      :: InletAirConfiguration    = 0   ! Identifies source of HPWH inlet air
  INTEGER                      :: AmbientTempSchedule      = 0   ! Schedule index pointer for ambient air temp at HPWH inlet
  INTEGER                      :: AmbientRHSchedule        = 0   ! Schedule index pointer for ambient air RH at HPWH inlet
  INTEGER                      :: AmbientTempZone          = 0   ! Index of ambient zone for ambient air at HPWH inlet
  INTEGER                      :: CrankcaseTempIndicator   = 0   ! Indicator for HPWH compressor/crankcase heater location
  INTEGER                      :: CrankcaseTempSchedule    = 0   ! Schedule index pointer where crankcase heater is located
  INTEGER                      :: CrankcaseTempZone        = 0   ! Index of zone where compressor/crankcase heater is located
  REAL(r64)                    :: OffCycParaLoad           = 0.0d0 ! Rate for off-cycle parasitic load (W)
  REAL(r64)                    :: OnCycParaLoad            = 0.0d0 ! Rate for on-cycle parasitic load (W)
  INTEGER                      :: ParasiticTempIndicator   = 0   ! Indicator for HPWH parasitic heat rejection location
  REAL(r64)                    :: OffCycParaFuelRate       = 0.0d0 ! Electric consumption rate for off-cycle parasitic load (W)
  REAL(r64)                    :: OnCycParaFuelRate        = 0.0d0 ! Electric consumption rate for on-cycle parasitic load (W)
  REAL(r64)                    :: OffCycParaFuelEnergy     = 0.0d0 ! Electric energy consumption for off-cycle parasitic load (J)
  REAL(r64)                    :: OnCycParaFuelEnergy      = 0.0d0 ! Electric energy consumption for on-cycle parasitic load (J)
  LOGICAL                      :: AirFlowRateAutosized = .FALSE. ! Used to report air flow autosize info in Init
  LOGICAL                      :: WaterFlowRateAutosized = .FALSE. ! Used to report water flow autosize info in Init
  INTEGER                      :: HPSetPointError          = 0   ! Used when temperature SP's in tank and HP are reversed
  INTEGER                      :: HPSetPointErrIndex1      = 0   ! Index to recurring error for tank/HP set point temp
  INTEGER                      :: IterLimitErrIndex1       = 0   ! Index for recurring iteration limit warning messages
  INTEGER                      :: IterLimitExceededNum1    = 0   ! Counter for recurring iteration limit warning messages
  INTEGER                      :: RegulaFalsiFailedIndex1  = 0   ! Index for recurring RegulaFalsi failed warning messages
  INTEGER                      :: RegulaFalsiFailedNum1    = 0   ! Counter for recurring RegulaFalsi failed warning messages
  INTEGER                      :: IterLimitErrIndex2       = 0   ! Index for recurring iteration limit warning messages
  INTEGER                      :: IterLimitExceededNum2    = 0   ! Counter for recurring iteration limit warning messages
  INTEGER                      :: RegulaFalsiFailedIndex2  = 0   ! Index for recurring RegulaFalsi failed warning messages
  INTEGER                      :: RegulaFalsiFailedNum2    = 0   ! Counter for recurring RegulaFalsi failed warning messages
  LOGICAL                      :: FirstTimeThroughFlag = .TRUE.  ! Flag for saving water heater status
  LOGICAL                      :: ShowSetpointWarning = .TRUE.  ! Warn when set point is greater than max tank temp limit
  REAL(r64)               :: HPWaterHeaterSensibleCapacity =0.0d0 ! sensible capacity delivered when HPWH is attached to a zone (W)
  REAL(r64)               :: HPWaterHeaterLatentCapacity   =0.0d0 ! latent capacity delivered when HPWH is attached to a zone (kg/s)
  INTEGER                 :: ControlSensorLocation         = HPWHControlNotSet ! if using stratified tank, indicates control point
END TYPE HeatPumpWaterHeaterData

TYPE WaterHeaterDesuperheaterData
  CHARACTER(len=MaxNameLength) :: Name                     = ''  ! Name of heat pump water heater desuperheater
  CHARACTER(len=MaxNameLength) :: Type                     = ''  ! Type of water heater desuperheating coil
  INTEGER                      :: InsuffTemperatureWarn    = 0   ! Used for recurring error count on low source temperature
  INTEGER                      :: AvailSchedPtr            = 0   ! Index to Availability Schedule curve index
  INTEGER                      :: SetpointTempSchedule     = 0   ! Index to Setpoint Temperature Schedule curve
  REAL(r64)                    :: DeadbandTempDiff         = 0.0d0 ! Dead band temperature difference (cut-in temperature)
  REAL(r64)                    :: HeatReclaimRecoveryEff   = 0.0d0 ! recovery efficiency of desuperheater (0.3 max)
  INTEGER                      :: WaterInletNode           = 0   ! Desuperheater water inlet node
  INTEGER                      :: WaterOutletNode          = 0   ! Desuperheater water outlet node
  REAL(r64)                    :: RatedInletWaterTemp      = 0.0d0 ! Inlet water temp at rated heat reclaim recovery eff (C)
  REAL(r64)                    :: RatedOutdoorAirTemp      = 0.0d0 ! Outdoor air temp at rated heat reclaim recovery eff (C)
  REAL(r64)                    :: MaxInletWaterTemp        = 0.0d0 ! Max water temp for heat reclaim recovery (C)
  CHARACTER(len=MaxNameLength) :: TankType                 = ''  ! Type of water heater (MIXED or STRATIFIED)
  INTEGER                      :: TankTypeNum              = 0   ! Parameter for tank type (MIXED or STRATIFIED)
  CHARACTER(len=MaxNameLength) :: TankName                 = ''  ! Name of tank associated with desuperheater
  LOGICAL                      :: StandAlone          = .FALSE.  ! Flag for operation with no plant connections (no use nodes)
  !note char variable heatingsourcetype doesn't seem to be used anywhere
  CHARACTER(len=MaxNameLength) :: HeatingSourceType        = ''  ! Type of heating source (DX coil or refrigerated rack)
  CHARACTER(len=MaxNameLength) :: HeatingSourceName        = ''  ! Name of heating source
  REAL(r64)                    :: HeaterRate               = 0.0d0 ! Report variable for desuperheater heating rate [W]
  REAL(r64)                    :: HeaterEnergy             = 0.0d0 ! Report variable for desuperheater heating energy [J]
  REAL(r64)                    :: PumpPower                = 0.0d0 ! Report variable for water circulation pump power [W]
  REAL(r64)                    :: PumpEnergy               = 0.0d0 ! Report variable for water circulation pump energy [J]
  REAL(r64)                    :: PumpElecPower            = 0.0d0 ! Nominal power input to the water circulation pump [W]
  REAL(r64)                    :: PumpFracToWater          = 0.0d0 ! Nominal power fraction to water for the water circulation pump
  REAL(r64)                    :: OperatingWaterFlowRate   = 0.0d0 ! Operating volumetric water flow rate (m3/s)
  INTEGER                      :: HEffFTemp                = 0   ! Heating capacity as a function of temperature curve index
  REAL(r64)                    :: HEffFTempOutput          = 0.0d0 ! report variable for HEffFTemp curve
  REAL(r64)                    :: SetpointTemp             = 0.0d0 ! set point or cut-out temperature [C]
  INTEGER                      :: WaterHeaterTankNum       = 0   ! Index of Water Heater Tank
  REAL(r64)                    :: DesuperheaterPLR         = 0.0d0 ! part load ratio of desuperheater
  REAL(r64)                    :: OnCycParaLoad            = 0.0d0 ! Rate for on-cycle parasitic load (W)
  REAL(r64)                    :: OffCycParaLoad           = 0.0d0 ! Rate for off-cycle parasitic load (W)
  REAL(r64)                    :: OnCycParaFuelEnergy      = 0.0d0 ! Electric energy consumption for on-cycle parasitic load (J)
  REAL(r64)                    :: OnCycParaFuelRate        = 0.0d0 ! Electric consumption rate for on-cycle parasitic load (W)
  REAL(r64)                    :: OffCycParaFuelEnergy     = 0.0d0 ! Electric energy consumption for off-cycle parasitic load (J)
  REAL(r64)                    :: OffCycParaFuelRate       = 0.0d0 ! Electric consumption rate for off-cycle parasitic load (W)
  INTEGER                      :: Mode                     = 0   ! mode (0 = float, 1 = heating [-1=venting na for desuperheater])
  INTEGER                      :: SaveMode                 = 0   ! desuperheater mode on first iteration
  INTEGER                      :: SaveWHMode               = 0   ! mode of water heater tank element (backup element)
  REAL(r64)                    :: BackupElementCapacity    = 0.0d0 ! Tank backup element capacity (W)
  REAL(r64)                    :: DXSysPLR                 = 0.0d0 ! runtime fraction of desuperheater heating coil
  CHARACTER(len=MaxNameLength) :: ReclaimHeatingSourceName = ' ' ! The source name for the Desuperheater Heating Coil
  INTEGER                  :: ReclaimHeatingSourceIndexNum = 0   ! Index to reclaim heating source (condenser) of a specific type
  INTEGER                  :: ReclaimHeatingSource         = 0   ! The source for the Desuperheater Heating Coil
                                                                 ! COMPRESSOR RACK:REFRIGERATED CASE    = 1
                                                                 ! COIL:DX:COOLINGBYPASSFACTOREMPIRICAL = 2
                                                                 ! COIL:DX:MULTISPEED:COOLINGEMPIRICAL  = 3
                                                                 ! COIL:DX:MultiMode:CoolingEmpirical   = 4
                                                                 ! CONDENSER:REFRIGERATION              = 5
  INTEGER                  :: SetPointError                = 0   ! Used when temp SP in tank and desuperheater are reversed
  INTEGER                  :: SetPointErrIndex1            = 0   ! Index to recurring error for tank/desuperheater set point temp
  INTEGER                  :: IterLimitErrIndex1           = 0   ! Index for recurring iteration limit warning messages
  INTEGER                  :: IterLimitExceededNum1        = 0   ! Counter for recurring iteration limit warning messages
  INTEGER                  :: RegulaFalsiFailedIndex1      = 0   ! Index for recurring RegulaFalsi failed warning messages
  INTEGER                  :: RegulaFalsiFailedNum1        = 0   ! Counter for recurring RegulaFalsi failed warning messages
  INTEGER                  :: IterLimitErrIndex2           = 0   ! Index for recurring iteration limit warning messages
  INTEGER                  :: IterLimitExceededNum2        = 0   ! Counter for recurring iteration limit warning messages
  INTEGER                  :: RegulaFalsiFailedIndex2      = 0   ! Index for recurring RegulaFalsi failed warning messages
  INTEGER                  :: RegulaFalsiFailedNum2        = 0   ! Counter for recurring RegulaFalsi failed warning messages
END TYPE WaterHeaterDesuperheaterData

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE(WaterThermalTankData),                 ALLOCATABLE, DIMENSION(:) :: WaterThermalTank
TYPE(HeatPumpWaterHeaterData),         ALLOCATABLE, DIMENSION(:) :: HPWaterHeater
TYPE(WaterHeaterDesuperheaterData),    ALLOCATABLE, DIMENSION(:) :: WaterHeaterDesuperheater
LOGICAL, ALLOCATABLE, DIMENSION(:) :: ValidSourceType ! Used to determine if a source for a desuperheater heating coil is valid
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MyHPSizeFlag    ! Used to report autosize info in Init
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckWTTEquipName
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckHPWHEquipName

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: NumChilledWaterMixed          =0 ! number of mixed chilled water tanks
INTEGER :: NumChilledWaterStratified     =0 ! number of stratified chilled water tanks
INTEGER :: NumWaterHeaterMixed           =0 ! number of mixed water heaters
INTEGER :: NumWaterHeaterStratified      =0 ! number of stratified water heaters
INTEGER :: NumWaterThermalTank           =0 ! total number of water thermal tanks, hot and cold (MIXED + STRATIFIED)
INTEGER :: NumWaterHeaterDesuperheater   =0 ! number of desuperheater heating coils
INTEGER :: NumHeatPumpWaterHeater        =0 ! number of heat pump water heaters
!INTEGER :: MaxCyclesErrorCount           =0 ! error counter for water heater that cycles more than max during time step

REAL(r64)    :: HPPartLoadRatio               =0.0d0 ! part load ratio of HPWH
LOGICAL :: GetWaterThermalTankInputFlag = .TRUE.             ! Calls to Water Heater from multiple places in code
REAL(r64)    :: MixerInletAirSchedule         =0.0d0 ! output of inlet air mixer node schedule
REAL(r64)    :: MdotAir                       =0.0d0 ! mass flow rate of evaporator air, kg/s
INTEGER :: NumWaterHeaterSizing          =0 ! Number of sizing/design objects for water heaters.
LOGICAL, DIMENSION(:), ALLOCATABLE :: AlreadyRated ! control so we don't repeat again

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC  SimWaterThermalTank
PUBLIC  SimulateWaterHeaterStandAlone
PUBLIC  CalcWaterThermalTankZoneGains
PUBLIC  SimHeatPumpWaterHeater
PRIVATE GetWaterThermalTankInput
PRIVATE InitWaterThermalTank
PRIVATE SetupStratifiedNodes
PRIVATE CalcWaterThermalTankMixed
PRIVATE CalcWaterThermalTankStratified
PRIVATE CalcHeatPumpWaterHeater
PRIVATE PLRResidualMixedTank
PRIVATE PLRResidualStratifiedTank
PRIVATE CalcDesuperheaterWaterHeater
PRIVATE PartLoadFactor
PRIVATE PlantMassFlowRatesFunc
PRIVATE MinePlantStructForInfo
PRIVATE SizeDemandSidePlantConnections
PRIVATE SizeSupplySidePlantConnections
PRIVATE SizeTankForDemandSide
PRIVATE SizeTankForSupplySide
PRIVATE SizeStandAloneWaterHeater
PRIVATE UpdateWaterThermalTank
PRIVATE ReportWaterThermalTank
PRIVATE CalcStandardRatings
PRIVATE ReportCWTankInits
PRIVATE FindStratifiedTankSensedTemp

PUBLIC CalcTankTemp
PUBLIC CalcTempIntegral

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE SimWaterThermalTank(CompType,CompName,CompIndex,RunFlag,InitLoopEquip,  &  !DSU
                          MyLoad,MaxCap,MinCap,OptCap,FirstHVACIteration, LoopNum, LoopSideNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brandon Anderson
          !       DATE WRITTEN   May 2000
          !       MODIFIED       FSEC, July 2005
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The main subroutine for simulating a water heater, heat pump water heater, or desuperheater
          ! heating coil.  This routine will:
          !
          ! 1. Gets Input if necessary
          ! 2. Determines the load the water heater (or heat pump water heater) must support
          ! 3. Determine the type of water heater, heat pump water heater, or desuperheater
          !    heating coil to be simulated
          ! 4. Calls simulation routines

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology. Subroutine is called from PlantLoopEquipments

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: BeginEnvrnFlag, KickOffSimulation
  USE InputProcessor, ONLY: FindItem,MakeUPPERCase

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompType
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(INOUT)       :: CompIndex
  LOGICAL, INTENT(IN)          :: RunFlag !unused1208
  LOGICAL, INTENT(IN)          :: InitLoopEquip
  REAL(r64), INTENT(INOUT)          :: MyLoad
  REAL(r64), INTENT(INOUT)            :: MinCap
  REAL(r64), INTENT(INOUT)            :: MaxCap
  REAL(r64), INTENT(INOUT)            :: OptCap
  LOGICAL, INTENT(IN)          :: FirstHVACIteration ! TRUE if First iteration of simulation
  INTEGER, INTENT(IN) , Optional         :: LoopNum
  INTEGER, INTENT(IN) , Optional         :: LoopSideNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Logical, save :: OneTimeSetupFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyOneTimeFlagWH  ! first pass log
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyTwoTimeFlagWH  ! second pass do input check
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyOneTimeFlagHP  ! first pass log
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyTwoTimeFlagHP  ! second pass do input check
  INTEGER :: tmpLoopNum
  INTEGER :: tmpLoopSideNum
  INTEGER :: CompNum
  INTEGER :: TankNum

          ! FLOW:
  IF (GetWaterThermalTankInputFlag) THEN
    CALL GetWaterThermalTankInput
    GetWaterThermalTankInputFlag = .FALSE.
  END IF

  If (OneTimeSetupFlag) THEN
    ALLOCATE (MyOneTimeFlagWH ( NumWaterThermalTank ))
    ALLOCATE (MyTwoTimeFlagWH ( NumWaterThermalTank ))
    ALLOCATE (MyOneTimeFlagHP (NumHeatPumpWaterHeater ))
    ALLOCATE (MyTwoTimeFlagHP (NumHeatPumpWaterHeater ))
    MyOneTimeFlagWH = .TRUE.
    MyTwoTimeFlagWH = .TRUE.
    MyOneTimeFlagHP = .TRUE.
    MyTwoTimeFlagHP = .TRUE.
    OneTimeSetupFlag = .FALSE.
  END IF

   ! Find the correct Equipment
  IF (CompType /= TypeOf_HeatPumpWtrHeater) THEN
    IF (CompIndex == 0) THEN
      CompNum = FindItem(CompName,WaterThermalTank%Name,NumWaterThermalTank)
      IF (CompNum == 0) THEN
        CALL ShowFatalError('SimWaterThermalTank:  Unit not found='//TRIM(CompName))
      ENDIF
      CompIndex=CompNum
    ELSE
      CompNum=CompIndex
      IF (CompNum > NumWaterThermalTank .or. CompNum < 1) THEN
        CALL ShowFatalError('SimWaterThermalTank:  Invalid CompIndex passed='//  &
                             TRIM(TrimSigDigits(CompNum))// &
                             ', Number of Units='//TRIM(TrimSigDigits(NumWaterThermalTank))//  &
                             ', Entered Unit name='//TRIM(CompName))
      ENDIF
      IF (CheckWTTEquipName(CompNum)) THEN
        IF (CompName /= WaterThermalTank(CompNum)%Name) THEN
          CALL ShowFatalError('SimWaterThermalTank: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(CompNum))// &
                              ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                              TRIM(WaterThermalTank(CompNum)%Name))
        ENDIF
        CheckWTTEquipName(CompNum)=.false.
      ENDIF
    ENDIF
  ELSE
    IF (CompIndex == 0) THEN
      CompNum = FindItem(CompName,HPWaterHeater%Name,NumHeatPumpWaterHeater)
      IF (CompNum == 0) THEN
        CALL ShowFatalError('SimWaterThermalTank:  Unit not found='//TRIM(CompName))
      ENDIF
      CompIndex=CompNum
    ELSE
      CompNum=CompIndex
      IF (CompNum > NumWaterThermalTank .or. CompNum < 1) THEN
        CALL ShowFatalError('SimWaterThermalTank:  Invalid CompIndex passed='//  &
                             TRIM(TrimSigDigits(CompNum))// &
                             ', Number of Units='//TRIM(TrimSigDigits(NumHeatPumpWaterHeater))//  &
                             ', Entered Unit name='//TRIM(CompName))
      ENDIF
      IF (CheckHPWHEquipName(CompNum)) THEN
        IF (CompName /= HPWaterHeater(CompNum)%Name) THEN
          CALL ShowFatalError('SimWaterThermalTank: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(CompNum))// &
                              ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                              TRIM(HPWaterHeater(CompNum)%Name))
        ENDIF
        CheckHPWHEquipName(CompNum)=.false.
      ENDIF
    ENDIF
  ENDIF


  ! this case statement needs integerization.
  SELECT CASE (CompType)

! string comparisons to remove here.
! =========================  Water Heater and Chilled Water Storage
    CASE (TypeOf_WtrHeaterMixed, TypeOf_WtrHeaterStratified,  &
          TypeOf_ChilledWaterTankMixed, TypeOf_ChilledWaterTankStratified)

      IF (InitLoopEquip) THEN
        IF (PRESENT(LoopNum)) THEN
          CALL InitWaterThermalTank(CompNum, FirstHVACIteration, LoopNum, LoopSideNum)
        ELSE
          CALL InitWaterThermalTank(CompNum, FirstHVACIteration)
        ENDIF
        Call MinePlantStructForInfo(CompNum)
        IF (PRESENT(LoopNum)) THEN
          IF (((WaterThermalTank(CompNum)%SourceSidePlantLoopNum == LoopNum) &
                 .AND. (WaterThermalTank(CompNum)%SourceSidePlantLoopSide == LoopSideNum)) &
              .OR. ((WaterThermalTank(CompNum)%UseSidePlantLoopNum  == LoopNum) &
                     .AND. (WaterThermalTank(CompNum)%UseSidePlantLoopSide == LoopSideNum))) THEN
            CALL SizeTankForDemandSide(CompNum)
            CALL SizeDemandSidePlantConnections(CompNum)
            CALL SizeSupplySidePlantConnections(CompNum, LoopNum, LoopSideNum)
            CALL SizeTankForSupplySide(CompNum)
          ELSE
            RETURN
          ENDIF
        ELSE
          CALL SizeTankForDemandSide(CompNum)
          CALL SizeDemandSidePlantConnections(CompNum)
          CALL SizeSupplySidePlantConnections(CompNum)
          CALL SizeTankForSupplySide(CompNum)
        ENDIF

       ! Calculate and report water heater standard ratings to EIO file (now that sizing is done)
        IF (PlantSizesOkayToFinalize) THEN
          IF (.NOT. WaterThermalTank(CompNum)%IsChilledWaterTank) Then
            CALL CalcStandardRatings(CompNum)
          ELSE
            CALL ReportCWTankInits(CompNum)
          ENDIF
        ENDIF
        MinCap = 0.0d0
        MaxCap = WaterThermalTank(CompNum)%MaxCapacity
        OptCap = WaterThermalTank(CompNum)%MaxCapacity
        IF (PRESENT(LoopNum)) THEN
          CALL InitWaterThermalTank(CompNum, FirstHVACIteration, LoopNum, LoopSideNum)
        ELSE
          CALL InitWaterThermalTank(CompNum, FirstHVACIteration)
        ENDIF
        RETURN
      END IF

      If (MyOneTimeFlagWH(CompNum)) THen
        MyOneTimeFlagWH(CompNum) =.false.
      ELSE
        If (MyTwoTimeFlagWH(CompNum))  THEN
          Call MinePlantStructForInfo(CompNum)  ! call it again to get control types filled out
          MyTwoTimeFlagWH(CompNum) = .FALSE.
        ENDIF
      ENDIF
      WaterThermalTank(CompNum)%UseSideLoadRequested = ABS(MyLoad)
      tmpLoopNum = WaterThermalTank(CompNum)%UseSidePlantLoopNum
      tmpLoopSideNum = WaterThermalTank(CompNum)%UseSidePlantLoopSide
      IF (tmpLoopNum > 0 .AND. tmpLoopSideNum > 0 .and. .not. KickOffSimulation) THEN
         WaterThermalTank(CompNum)%UseCurrentFlowLock      = &
               PlantLoop(tmpLoopNum)%Loopside(LoopSideNum)%FlowLock
      ELSE
         WaterThermalTank(CompNum)%UseCurrentFlowLock      = 1
      ENDIF
      tmpLoopNum = WaterThermalTank(CompNum)%SourceSidePlantLoopNum
      tmpLoopSideNum = WaterThermalTank(CompNum)%SourceSidePlantLoopSide
      IF (tmpLoopNum > 0 .AND. tmpLoopSideNum > 0 .and. .not. KickOffSimulation) THEN
         WaterThermalTank(CompNum)%SourceCurrentFlowLock      = &
                 PlantLoop(tmpLoopNum)%Loopside(LoopSideNum)%FlowLock
      ELSE
         WaterThermalTank(CompNum)%SourceCurrentFlowLock      = 1
      ENDIF
      CALL InitWaterThermalTank(CompNum, FirstHVACIteration)
!       Plant connected water heaters may have a desuperheater heating coil attached
      IF(WaterThermalTank(CompNum)%DesuperheaterNum .EQ. 0)THEN
        IF ((WaterThermalTank(CompNum)%TypeNum == MixedWaterHeater) &
               .OR. (WaterThermalTank(CompNum)%TypeNum == MixedChilledWaterStorage)) THEN
          CALL CalcWaterThermalTankMixed(CompNum)
        ELSE IF ( ( WaterThermalTank(CompNum)%TypeNum == StratifiedWaterHeater) &
               .OR.  ( WaterThermalTank(CompNum)%TypeNum == StratifiedChilledWaterStorage )) THEN
          CALL CalcWaterThermalTankStratified(CompNum)
        END IF
      ELSEIF(WaterThermalTank(CompNum)%DesuperheaterNum .GT. 0)THEN
        CALL CalcDesuperheaterWaterHeater(CompNum, FirstHVACIteration)
      END IF
      CALL UpdateWaterThermalTank(CompNum)
      CALL ReportWaterThermalTank(CompNum)

! =========================  Heat Pump Water Heater
    CASE (TypeOf_HeatPumpWtrHeater)
      IF (InitLoopEquip) THEN
        ! CompNum is index to heatpump model, not tank so get the tank index
        TankNum = HPWaterHeater(CompNum)%WaterHeaterTankNum
        IF (PRESENT(LoopNum)) THEN
          CALL InitWaterThermalTank(TankNum, FirstHVACIteration, LoopNum, LoopSideNum)
        ELSE
          CALL InitWaterThermalTank(TankNum, FirstHVACIteration)
        ENDIF
        Call MinePlantStructForInfo(TankNum)
        IF (PRESENT(LoopNum)) THEN
          IF (((WaterThermalTank(TankNum)%SourceSidePlantLoopNum == LoopNum) &
                 .AND. (WaterThermalTank(TankNum)%SourceSidePlantLoopSide == LoopSideNum)) &
              .OR. ((WaterThermalTank(TankNum)%UseSidePlantLoopNum  == LoopNum) &
                     .AND. (WaterThermalTank(TankNum)%UseSidePlantLoopSide == LoopSideNum))) THEN
            CALL SizeTankForDemandSide(CompNum)
            CALL SizeDemandSidePlantConnections(CompNum)
            CALL SizeSupplySidePlantConnections(TankNum, LoopNum, LoopSideNum)
            CALL SizeTankForSupplySide(TankNum)
          ELSE
            RETURN
          ENDIF
        ELSE
          CALL SizeTankForDemandSide(CompNum)
          CALL SizeDemandSidePlantConnections(CompNum)
          CALL SizeSupplySidePlantConnections(TankNum)
          CALL SizeTankForSupplySide(TankNum)
        ENDIF

        IF (PlantSizesOkayToFinalize) THEN
          CALL CalcStandardRatings(TankNum)
        ENDIF
        MinCap = 0.0d0
        MaxCap = HPWaterHeater(CompNum)%Capacity
        OptCap = HPWaterHeater(CompNum)%Capacity

        RETURN
      END IF

      If (MyOneTimeFlagHP(CompNum)) THen
        MyOneTimeFlagHP(CompNum) =.false.
      ELSE
        If (MyTwoTimeFlagHP(CompNum))  THEN
          Call MinePlantStructForInfo(HPWaterHeater(CompNum)%WaterHeaterTankNum)  ! call it again to get control types filled out
          MyTwoTimeFlagHP(CompNum) = .FALSE.
        ENDIF
      ENDIF
      WaterThermalTank(HPWaterHeater(CompNum)%WaterHeaterTankNum)%UseSideLoadRequested = ABS(MyLoad)
      tmpLoopNum = WaterThermalTank(HPWaterHeater(CompNum)%WaterHeaterTankNum)%UseSidePlantLoopNum
      tmpLoopSideNum = WaterThermalTank(HPWaterHeater(CompNum)%WaterHeaterTankNum)%UseSidePlantLoopSide
      IF (tmpLoopNum > 0 .AND. tmpLoopSideNum > 0 .and. .not. KickOffSimulation) THEN
         WaterThermalTank(HPWaterHeater(CompNum)%WaterHeaterTankNum)%UseCurrentFlowLock      = &
               PlantLoop(tmpLoopNum)%Loopside(LoopSideNum)%FlowLock
      ELSE
         WaterThermalTank(HPWaterHeater(CompNum)%WaterHeaterTankNum)%UseCurrentFlowLock      = 1
      ENDIF
      IF (PRESENT(LoopNum)) THEN
        CALL InitWaterThermalTank(HPWaterHeater(CompNum)%WaterHeaterTankNum, FirstHVACIteration, LoopNum, LoopSideNum)
      ELSE
        CALL InitWaterThermalTank(HPWaterHeater(CompNum)%WaterHeaterTankNum, FirstHVACIteration)
      ENDIF
      CALL CalcHeatPumpWaterHeater(HPWaterHeater(CompNum)%WaterHeaterTankNum, FirstHVACIteration)
      CALL UpdateWaterThermalTank(HPWaterHeater(CompNum)%WaterHeaterTankNum)
      CALL ReportWaterThermalTank(HPWaterHeater(CompNum)%WaterHeaterTankNum)

    CASE DEFAULT
      CALL ShowSevereError('SimWaterThermalTank: Invalid Water Thermal Tank Equipment Type='//TRIM(TrimSigDigits(CompType)))
      CALL ShowContinueError('Occurs in Water Thermal Tank Equipment named = '//TRIM(CompName))
      CALL ShowFatalError('Preceding condition causes termination.')

  END SELECT

  RETURN

END SUBROUTINE SimWaterThermalTank

SUBROUTINE SimulateWaterHeaterStandAlone(WaterHeaterNum, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       July 2005, FSEC - added HPWHs and desuperheater water heating coils
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine acts an interface to SimWaterHeater for stand-alone water heaters with no plant connections,
          ! HPWHs not defined as zone equipment with no plant connections, and stand-alone water heaters with
          ! desuperheater heating coils with no plant connections.

          ! METHODOLOGY EMPLOYED:
          ! The necessary control flags and dummy variables are set and passed into SimWaterHeater. This subroutine is
          ! called from NonZoneEquipmentManager.

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: NumPlantLoops
  USE DataLoopNode   , ONLY: node
  USE DataPlant

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterHeaterNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: LocalRunFlag,LocalInitLoopEquip     ! local variables of similar name as others used in Sim modules
  REAL(r64)    :: MyLoad, MinCap, MaxCap, OptCap
  INTEGER :: TestNum

          ! FLOW:
  IF (GetWaterThermalTankInputFlag) THEN
    CALL GetWaterThermalTankInput
    GetWaterThermalTankInputFlag = .FALSE.
  END IF

  ! Only simulate stand-alone water heaters here.  Plant connected water heaters are called by the PlantLoopEquipments.
  IF (WaterThermalTank(WaterHeaterNum)%StandAlone) THEN
    LocalRunFlag = .TRUE.
    LocalInitLoopEquip = .FALSE.
    TestNum=WaterHeaterNum
    CALL SimWaterThermalTank(WaterThermalTank(WaterHeaterNum)%TypeNum,WaterThermalTank(WaterHeaterNum)%Name,TestNum, &
         LocalRunFlag,LocalInitLoopEquip,MyLoad,MinCap,MaxCap,OptCap,FirstHVACIteration)
    IF (TestNum /= WaterHeaterNum) THEN
      CALL ShowFatalError('SimulateWaterHeaterStandAlone: Input WaterHeater Num ['//trim(TrimSigDigits(WaterHeaterNum))//  &
         '] does not match returned WaterHeater Num['//trim(TrimSigDigits(TestNum))//'] Name="'//  &
         trim(WaterThermalTank(WaterHeaterNum)%Name)//'".')
    ENDIF

! HPWHs with inlet air from a zone and not connected to a plant loop are simulated through a CALL from ZoneEquipmentManager.
! HPWHs that are plant connected are always simulated through a CALL from PlantLoopEquipments directly to SimWaterThermalTank.

! NOTE: HPWHs with inlet air from a zone AND plant connected are not stand alone and are simulated in PlantLoopEquipments
  ELSE IF(WaterThermalTank(WaterHeaterNum)%HeatPumpNum .GT. 0) THEN
!   Only HPWHs with inlet air from outdoors or scheduled HPWHs (not connected to a plant loop) are simulated here.
    IF(HPWaterHeater(WaterThermalTank(WaterHeaterNum)%HeatPumpNum)%StandAlone .AND. &
       (HPWaterHeater(WaterThermalTank(WaterHeaterNum)%HeatPumpNum)%InletAirConfiguration .EQ. AmbientTempOutsideAir .OR. &
        HPWaterHeater(WaterThermalTank(WaterHeaterNum)%HeatPumpNum)%InletAirConfiguration .EQ. AmbientTempSchedule))THEN
      LocalRunFlag = .TRUE.
      LocalInitLoopEquip = .FALSE.
      CALL SimWaterThermalTank(HPWaterHeater(WaterThermalTank(WaterHeaterNum)%HeatPumpNum)%TypeNum, &
                          HPWaterHeater(WaterThermalTank(WaterHeaterNum)%HeatPumpNum)%Name, &
                          WaterThermalTank(WaterHeaterNum)%HeatPumpNum,LocalRunFlag, &
                          LocalInitLoopEquip,MyLoad,MinCap,MaxCap,OptCap,FirstHVACIteration)
    END IF

! Only simulate stand-alone water heaters with desuperheater water heating coils here.  Plant connected water heaters
! with desuperheater water heating coils are called by PlantLoopEquipments.
  ELSEIF(WaterThermalTank(WaterHeaterNum)%DesuperheaterNum .GT. 0)THEN
    IF(WaterHeaterDesuperheater(WaterThermalTank(WaterHeaterNum)%DesuperheaterNum)%StandAlone)THEN
      LocalRunFlag = .TRUE.
      LocalInitLoopEquip = .FALSE.
      TestNum=WaterHeaterNum
      CALL SimWaterThermalTank(WaterThermalTank(WaterHeaterNum)%TypeNum,WaterThermalTank(WaterHeaterNum)%Name,TestNum, &
           LocalRunFlag,LocalInitLoopEquip,MyLoad,MinCap,MaxCap,OptCap,FirstHVACIteration)
      IF (TestNum /= WaterHeaterNum) THEN
        CALL ShowFatalError('SimulateWaterHeaterStandAlone: Input WaterHeater Num ['//trim(TrimSigDigits(WaterHeaterNum))//  &
           '] does not match returned WaterHeater Num['//trim(TrimSigDigits(TestNum))//'] Name="'//  &
           trim(WaterThermalTank(WaterHeaterNum)%Name)//'".')
      ENDIF
    END IF
  END IF

  RETURN

END SUBROUTINE SimulateWaterHeaterStandAlone

SUBROUTINE SimHeatPumpWaterHeater(CompName,FirstHVACIteration,SensLoadMet,LatLoadMet,CompIndex)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   April 2005
          !       MODIFIED       Don Shirey, Aug 2009 (LatLoadMet)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine acts as an interface to SimWaterHeater.
          ! HPWHs defined as zone equipment and not connected to a plant loop are called here by ZoneEquipmentManager

          ! METHODOLOGY EMPLOYED:
          ! The necessary control flags and dummy variables are set and passed into SimWaterHeater.

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE DataGlobals,    ONLY: DoingSizing
  USE DataInterfaces, ONLY: ShowFatalError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN) :: CompName
  LOGICAL, INTENT(IN)                    :: FirstHVACIteration
  REAL(r64), INTENT(OUT)                 :: SensLoadMet ! sensible load met by this equipment and sent to zone, W
  REAL(r64), INTENT (OUT)                :: LatLoadMet  ! net latent load met and sent to zone (kg/s), dehumid = negative
  INTEGER, INTENT(INOUT)                 :: CompIndex

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: LocalRunFlag,LocalInitLoopEquip     ! local variables of similar name as others used in Sim modules
  INTEGER :: LocalFlowLock                       ! local variables of similar name as others used in sim modules
  INTEGER :: HeatPumpNum
  REAL(r64) :: MyLoad, MinCap, MaxCap, OptCap

          ! FLOW:
  IF (GetWaterThermalTankInputFlag) THEN
    CALL GetWaterThermalTankInput
    GetWaterThermalTankInputFlag = .FALSE.
  END IF

  ! Find the correct Heat Pump Water Heater
  IF (CompIndex == 0) THEN
    HeatPumpNum    = FindItemInList(CompName, HPWaterHeater%Name, NumHeatPumpWaterHeater)
    IF (HeatPumpNum == 0) THEN
      CALL ShowFatalError('SimHeatPumpWaterHeater: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=HeatPumpNum
  ELSE
    HeatPumpNum=CompIndex
    IF (HeatPumpNum > NumHeatPumpWaterHeater .or. HeatPumpNum < 1) THEN
      CALL ShowFatalError('SimHeatPumpWaterHeater:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HeatPumpNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumHeatPumpWaterHeater))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
  END IF

  ! Only simulate HPWHs specified as zone equipment and not connected to a plant loop.
  ! HPWHs not defined as zone equipment with no plant connections are simulated in NonZoneEquipmentManager.
  ! Plant connected HPWHs are called by PlantLoopEquipments (but only those on supply side ).
  SensLoadMet = 0.0d0
  LatLoadMet  = 0.0d0

  LocalRunFlag = .TRUE.
  LocalFlowLock = 1 ! .TRUE.
  LocalInitLoopEquip = .FALSE.

! HPWH will not be included in sizing calculations, fan is initialized only during BeginEnvrnFlag (FALSE during sizing)
! (fan will be turned off during Standard Ratings procedure yielding incorrect results)
  IF(DoingSizing)RETURN

! For HPWHs, StandAlone means not connected to a plant loop (use nodes are not used, source nodes are connected to a HPWH)
  IF(HPWaterHeater(HeatPumpNum)%StandAlone)THEN
    CALL SimWaterThermalTank(HPWaterHeater(HeatPumpNum)%TypeNum,HPWaterHeater(HeatPumpNum)%Name, HeatPumpNum, &
                        LocalRunFlag,LocalInitLoopEquip,MyLoad,MinCap,MaxCap,OptCap,FirstHVACIteration)
    SensLoadMet = HPWaterHeater(HeatPumpNum)%HPWaterHeaterSensibleCapacity
    LatLoadMet = HPWaterHeater(HeatPumpNum)%HPWaterHeaterLatentCapacity
  ELSE
    ! HPWH is plant connected and will get simulated when called from plant SimWaterThermalTank, but need to update loads met here
    SensLoadMet = HPWaterHeater(HeatPumpNum)%HPWaterHeaterSensibleCapacity
    LatLoadMet = HPWaterHeater(HeatPumpNum)%HPWaterHeaterLatentCapacity
  END IF

  RETURN

END SUBROUTINE SimHeatPumpWaterHeater

SUBROUTINE CalcWaterThermalTankZoneGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   March 2005
          !       MODIFIED       B. Griffith November 2011, new internal gains structure
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the zone internal gains due to water heater skin losses during sizing.
          ! initilizes gains to zone at begin environment.

          ! METHODOLOGY EMPLOYED:
          ! Sums the tank losses from all of the water heaters in the zone to add as a gain to the zone.
          ! Now used to determine tank losses during sizing.  Internal gains are summed in a centralized way now
          !

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: BeginEnvrnFlag, DoingSizing
  USE DataHeatBalFanSys, ONLY: MAT
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WaterThermalTankNum
  INTEGER :: ZoneNum
  LOGICAL, SAVE :: MyEnvrnFlag=.true.
  REAL(r64) :: TankTemp
  REAL(r64) :: QLossToZone
  INTEGER :: SchIndex

          ! FLOW:
  IF (NumWaterThermalTank == 0) THEN

   IF(.NOT. DoingSizing) THEN
     RETURN
   ELSE
     IF (GetWaterThermalTankInputFlag) THEN
       CALL GetWaterThermalTankInput
       GetWaterThermalTankInputFlag = .FALSE.
     END IF
     IF (NumWaterThermalTank == 0) Return
   ENDIF

  ENDIF

  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    WaterThermalTank%AmbientZoneGain      = 0.d0
    WaterThermalTank%FuelEnergy           = 0.d0
    WaterThermalTank%OffCycParaFuelEnergy = 0.d0
    WaterThermalTank%OnCycParaFuelEnergy  = 0.d0
    MyEnvrnFlag=.false.
  ENDIF

  IF (.not. BeginEnvrnFlag) MyEnvrnFlag=.true.

  DO WaterThermalTankNum = 1, NumWaterThermalTank
    IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempZone == 0) CYCLE
    ZoneNum = WaterThermalTank(WaterThermalTankNum)%AmbientTempZone
    IF(DoingSizing)THEN
      ! Initialize tank temperature to setpoint
      ! (use HPWH or Desuperheater heating coil set point if applicable)
      IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0)THEN
        SchIndex = HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SetpointTempSchedule
      ELSE IF(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum .GT. 0)THEN
        SchIndex = WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%SetpointTempSchedule
      ELSE
        SchIndex = WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule
      END IF

      IF (SchIndex > 0) THEN
        TankTemp = GetCurrentScheduleValue(SchIndex)
      ELSE
        TankTemp = 20.0d0
      END IF
      SELECT CASE(WaterThermalTank(WaterThermalTankNum)%TypeNum)
        CASE (MixedWaterHeater)
          QLossToZone = MAX(WaterThermalTank(WaterThermalTankNum)%OnCycLossCoeff * &
                            WaterThermalTank(WaterThermalTankNum)%OnCycLossFracToZone, &
                            WaterThermalTank(WaterThermalTankNum)%OffCycLossCoeff * &
                            WaterThermalTank(WaterThermalTankNum)%OffCycLossFracToZone) * &
                        (TankTemp-MAT(WaterThermalTank(WaterThermalTankNum)%AmbientTempZone))
        CASE(StratifiedWaterHeater)
          QLossToZone = MAX(WaterThermalTank(WaterThermalTankNum)%Node(1)%OnCycLossCoeff * &
                            WaterThermalTank(WaterThermalTankNum)%SkinLossFracToZone, &
                            WaterThermalTank(WaterThermalTankNum)%Node(1)%OffCycLossCoeff * &
                            WaterThermalTank(WaterThermalTankNum)%SkinLossFracToZone) * &
                        (TankTemp-MAT(WaterThermalTank(WaterThermalTankNum)%AmbientTempZone))
        CASE(MixedChilledWaterStorage)
          QLossToZone = WaterThermalTank(WaterThermalTankNum)%OffCycLossCoeff *          &
                          WaterThermalTank(WaterThermalTankNum)%OffCycLossFracToZone   * &
                         (TankTemp-MAT(WaterThermalTank(WaterThermalTankNum)%AmbientTempZone) )
        CASE(StratifiedChilledWaterStorage)
          QLossToZone = WaterThermalTank(WaterThermalTankNum)%Node(1)%OffCycLossCoeff  * &
                          WaterThermalTank(WaterThermalTankNum)%SkinLossFracToZone * &
                         (TankTemp-MAT(WaterThermalTank(WaterThermalTankNum)%AmbientTempZone) )
      END SELECT
      WaterThermalTank(WaterThermalTankNum)%AmbientZoneGain = QLossToZone
    END IF
  END DO

  RETURN

END SUBROUTINE CalcWaterThermalTankZoneGains


SUBROUTINE GetWaterThermalTankInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher and Brandon Anderson
          !       DATE WRITTEN   May 2000
          !       MODIFIED       R. Raustad, June 2005, added HPWH and desuperheater water heating coils
          !                      B. Griffith, Oct. 2007 extensions for indirect water heaters
          !                      B. Griffith, Feb. 2008 extensions for autosizing water heaters
          !                      BG Mar 2009.  Trap for bad heater height input for stratefied water heater CR7718
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the water heater, HPWH, and/or desuperheater heating coil input from the input file.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals,        ONLY: MaxNameLength, NumOfZones, AutoCalculate, ScheduleAlwaysOn, outputfiledebug
  USE DataInterfaces,     ONLY: ShowSevereError, ShowWarningError, ShowFatalError,  &
                                SetupOutputVariable, ShowContinueError
  USE InputProcessor,     ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, SameString, GetObjectDefMaxArgs
  USE DataIPShortCuts
  USE NodeInputManager,   ONLY: GetOnlySingleNode
  USE ScheduleManager,    ONLY: GetScheduleIndex, CheckScheduleValueMinMax
  USE BranchNodeConnections, ONLY: TestCompSet, SetUpCompSets
  USE Psychrometrics,     ONLY: PsyRhoAirFnPbTdbW
  USE FluidProperties,    ONLY: GetDensityGlycol
  USE DataLoopNode,       ONLY: Node, NodeType_Air, NodeType_Water, NodeConnectionType_Inlet, NodeConnectionType_Outlet, &
                                NodeConnectionType_ZoneExhaust,  NodeConnectionType_ReliefAir, & ! ,NodeConnectionType_Internal
                                NodeConnectionType_OutsideAir, NodeConnectionType_OutsideAirReference, &
                                ObjectIsParent, ObjectIsNotParent
  USE CurveManager,       ONLY: GetCurveIndex, GetCurveType, CurveValue
  USE DataHeatBalance,    ONLY: Zone, IntGainTypeOf_WaterHeaterMixed, IntGainTypeOf_WaterHeaterStratified, &
                                IntGainTypeOf_ThermalStorageChilledWaterMixed, IntGainTypeOf_ThermalStorageChilledWaterStratified
  USE DXCoils,            ONLY: DXCoil, GetDXCoilIndex, NumDXCoils
  USE General,            ONLY: TrimSigDigits,  RoundSigDigits
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE PlantUtilities,     ONLY: RegisterPlantCompDesignFlow
  USE Fans,               ONLY: GetFanType, GetFanIndex, GetFanVolFlow
  USE DataSizing,         ONLY: AutoSize
  USE DataZoneEquipment,  ONLY: ZoneEquipConfig, ZoneEquipList, ZoneEquipInputsFilled, GetZoneEquipmentData
  USE DataEnvironment,    ONLY: OutBaroPress
  USE DataHVACGlobals,    ONLY: FanType_SimpleOnOff, BlowThru, DrawThru
  USE OutAirNodeManager,  ONLY: CheckOutAirNodeNumber,CheckAndAddAirNodeNumber
  USE DataSizing,         ONLY: PlantSizData, NumPltSizInput
  USE RefrigeratedCase,   ONLY: CheckRefrigerationInput
  USE GlobalNames, ONLY: VerifyUniqueCoilName

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '
  CHARACTER(len=*), PARAMETER :: RoutineName = 'GetWaterThermalTankInput: '


          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:
  TYPE WaterHeaterSaveNodes
    CHARACTER(len=MaxNameLength) :: InletNodeName1      =' '
    CHARACTER(len=MaxNameLength) :: OutletNodeName1     =' '
    CHARACTER(len=MaxNameLength) :: InletNodeName2      =' '
    CHARACTER(len=MaxNameLength) :: OutletNodeName2     =' '
  END TYPE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                     :: WaterThermalTankNum          ! Index to WATER HEATER:*
  INTEGER                     :: WHsizingNum             ! Index to Water Heater:Sizing, for the IDF objects--not data storage
  INTEGER                     :: NodeNum                 ! Index to a stratified thermal node
  INTEGER                     :: CheckWaterHeaterNum     ! Used to search WATER HEATER:* to find association with HP Water Heater
  INTEGER                     :: DesuperheaterNum        ! Index to Coil:WaterHeating:Desuperheater
  INTEGER                     :: HPWaterHeaterNum        ! Index to HEAT PUMP:WATER HEATER
  INTEGER                     :: HeatingSourceNum        ! Index to DX cooling coil (heat source for desuperheater)
  INTEGER                     :: NumAlphas               ! Number of elements in the alpha array
  INTEGER                     :: NumNums                 ! Number of elements in the numeric array
!unused1208  INTEGER                     :: NumArgs                 ! Number of elements in the object (alpha + numeric)
  INTEGER                     :: RackNum                 ! Index to refrigrated display case rack
  INTEGER                     :: CondNum                 ! Index to refrigration condenser
  INTEGER                     :: DXCoilNum               ! Index to DX coils
  INTEGER                     :: IOStat                  ! IO Status when calling get input subroutine
  LOGICAL                     :: IsNotOK                 ! Flag to verify name
  LOGICAL                     :: IsBlank                 ! Flag for blank name
  LOGICAL                     :: IsValid                 ! Flag for validating PLF curve, OutsideAirNode
  LOGICAL                     :: ErrorsFound = .FALSE.   ! Flag for any error found during GetWaterThermalTankInput
  CHARACTER(len=MaxNameLength):: FanInletNode = ' '      ! Used to set up comp set
  CHARACTER(len=MaxNameLength):: FanOutletNode = ' '     ! Used to set up comp set
  CHARACTER(len=MaxNameLength):: CoilInletNode = ' '     ! Used to set up comp set
  CHARACTER(len=MaxNameLength):: CoilOutletNode = ' '    ! Used to set up comp set
  INTEGER                     :: SupAirIn  = 0           ! Used for error checking HPWHs
  INTEGER                     :: ExhAirOut = 0           ! Used for error checking HPWHs
  LOGICAL                     :: FoundInletNode  = .FALSE. ! Used for error checking HPWHs
  LOGICAL                     :: FoundOutletNode = .FALSE. ! Used for error checking HPWHs
  INTEGER                     :: ZoneNum = 0               ! Used for error checking HPWHs
  LOGICAL                     :: ValidScheduleValue = .FALSE. ! Used for error checking HPWH's inlet air mixer schedule
  INTEGER                     :: ZoneEquipConfigNum = 0  ! Used to determine if HPWH tank is in a Zone Equipment List (ZEL)
  INTEGER                     :: ZoneEquipListNum = 0    ! Used to determine if HPWH tank is in a Zone Equipment List
  INTEGER                     :: EquipmentTypeNum = 0    ! Used to determine if HPWH tank is in a Zone Equipment List
  LOGICAL                     :: FoundTankInList = .FALSE.  ! Used to determine if HPWH tank is listed in a Zone Equipment List
  LOGICAL                     :: TankNotLowestPriority = .FALSE. ! Used to determine if HPWH tank is prioritized correctly in ZEL
  INTEGER                     :: TankCoolingPriority = 0 ! Used to determine if a HPWH tank is prioritized correctly in ZEL
  INTEGER                     :: TankHeatingPriority = 0 ! Used to determine if a HPWH tank is prioritized correctly in ZEL
  LOGICAL                     :: DXCoilErrFlag = .FALSE. ! Used for error checking DX coils used with HPWHs
  REAL(r64)                   :: FanVolFlow = 0.0d0        ! Used for error checking fans used with HPWHs
  LOGICAL                     :: ErrFlag = .FALSE.       ! Used for error checking used with HPWHs
  REAL(r64)                   :: HEffFTemp = 0.0d0         ! Used for error checking desuperheater heating coils
  LOGICAL                     :: Okay

  ! Following allow for temporary storage of character strings but not saved in main structure
  TYPE (WaterHeaterSaveNodes), ALLOCATABLE, DIMENSION(:) :: HPWHSaveNodeNames  ! temporary for HPWH node names used in later checks
  TYPE (WaterHeaterSaveNodes), ALLOCATABLE, DIMENSION(:) :: WHSaveNodeNames    ! temporary for WH node names used in later checks
  TYPE (WaterHeaterSaveNodes), ALLOCATABLE, DIMENSION(:) :: CoilSaveNodeNames  ! temporary for coil node names used in later checks
  REAL(r64)  :: rho ! local fluid density
  INTEGER    :: DummyWaterIndex = 1

          ! FLOW:

  ! Make sure refrigeration input is gotten before this input
  CALL CheckRefrigerationInput

  IF (GetWaterThermalTankInputFlag) THEN
    NumWaterHeaterMixed       = GetNumObjectsFound(cMixedWHModuleObj)
    NumWaterHeaterStratified  = GetNumObjectsFound(cStratifiedWHModuleObj)
    NumChilledWaterMixed      = GetNumObjectsFound(cMixedCWTankModuleObj)
    NumChilledWaterStratified = GetNumObjectsFound(cStratifiedCWTankModuleObj)
    NumWaterThermalTank       = NumWaterHeaterMixed + NumWaterHeaterStratified &
                                 + NumChilledWaterMixed + NumChilledWaterStratified
    NumHeatPumpWaterHeater = GetNumObjectsFound('WaterHeater:HeatPump')
    NumWaterHeaterDesuperheater = GetNumObjectsFound('Coil:WaterHeating:Desuperheater')


    IF (NumWaterThermalTank > 0) THEN
      ! Write water heater header for EIO
      If ((NumWaterHeaterMixed >0) .OR. (NumWaterHeaterStratified >0)) WRITE(OutputFileInits,720)
      IF (NumHeatPumpWaterHeater > 0)  WRITE(OutputFileInits,721)
      IF (NumWaterHeaterStratified > 0) WRITE(OutputFileInits,722)
      IF (NumChilledWaterMixed > 0) WRITE(OutputFileInits, 725 )
      IF (NumChilledWaterStratified > 0) WRITE(OutputFileInits, 726)


    END IF

720  FORMAT( '! <Water Heater Information>,Type,Name,Volume {m3},Maximum Capacity {W},Standard Rated Recovery Efficiency, ', &
              'Standard Rated Energy Factor')
721  FORMAT( '! <Heat Pump Water Heater Information>,Type,Name,Volume {m3},Maximum Capacity {W},', &
             'Standard Rated Recovery Efficiency,Standard Rated Energy Factor,"DX Coil Total Cooling Rate {W, HPWH Only}"')
722  FORMAT( '! <Water Heater Stratified Node Information>,Node Number,Height {m},Volume {m3},Maximum Capacity {W},', &
             'Off-Cycle UA {W/K},On-Cycle UA {W/K},Number Of Inlets,Number Of Outlets')
725  FORMAT('! <Chilled Water Tank Information>,Type,Name,Volume {m3},Use Side Design Flow Rate {m3/s}, ', &
             'Source Side Design Flow Rate {m3/s}')
726  FORMAT( '! <Chilled Water Tank Stratified Node Information>,Node Number,Height {m},Volume {m3},', &
             'UA {W/K},Number Of Inlets,Number Of Outlets')

    IF (NumWaterThermalTank > 0) THEN
      ALLOCATE(WaterThermalTank(NumWaterThermalTank))
      ALLOCATE(WHSaveNodeNames(NumWaterThermalTank))
      ALLOCATE(CheckWTTEquipName(NumWaterThermalTank))
      CheckWTTEquipName=.true.
    ENDIF
    IF (NumHeatPumpWaterHeater > 0)THEN
      ALLOCATE(HPWaterHeater(NumHeatPumpWaterHeater))
      ALLOCATE(MyHPSizeFlag(NumHeatPumpWaterHeater))
      MyHPSizeFlag=.true.
      ALLOCATE(CheckHPWHEquipName(NumHeatPumpWaterHeater))
      CheckHPWHEquipName=.true.
      ALLOCATE(HPWHSaveNodeNames(NumHeatPumpWaterHeater))
    END IF
    IF (NumWaterHeaterDesuperheater > 0) THEN
      ALLOCATE(WaterHeaterDesuperheater(NumWaterHeaterDesuperheater))
      ALLOCATE(ValidSourceType(NumWaterHeaterDesuperheater))
      ValidSourceType=.false.
      ALLOCATE(CoilSaveNodeNames(NumWaterHeaterDesuperheater))
    ENDIF

!!!=======   Get Coil:WaterHeating:Desuperheater ======================================================================
    IF (NumWaterHeaterDesuperheater > 0) THEN
      cCurrentModuleObject = 'Coil:WaterHeating:Desuperheater'
      DO DesuperheaterNum = 1, NumWaterHeaterDesuperheater

        CALL GetObjectItem(cCurrentModuleObject,DesuperheaterNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1),WaterHeaterDesuperheater%Name,DesuperheaterNum-1, &
                        IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF
        CALL VerifyUniqueCoilName(cCurrentModuleObject,cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        WaterHeaterDesuperheater(DesuperheaterNum)%Name = cAlphaArgs(1)
        WaterHeaterDesuperheater(DesuperheaterNum)%Type = cCurrentModuleObject

!       convert availability schedule name to pointer
        IF(.NOT. lAlphaFieldBlanks(2) )THEN
          WaterHeaterDesuperheater(DesuperheaterNum)%AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2))
          IF (WaterHeaterDesuperheater(DesuperheaterNum)%AvailSchedPtr .EQ. 0) THEN
            CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)))
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.TRUE.
          END IF
        ELSE
          WaterHeaterDesuperheater(DesuperheaterNum)%AvailSchedPtr = ScheduleAlwaysOn
        END IF

!       convert schedule name to pointer
        WaterHeaterDesuperheater(DesuperheaterNum)%SetpointTempSchedule = GetScheduleIndex(cAlphaArgs(3))
        IF (WaterHeaterDesuperheater(DesuperheaterNum)%SetpointTempSchedule .EQ. 0) THEN
           CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)))
           CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.TRUE.
        END IF

        WaterHeaterDesuperheater(DesuperheaterNum)%DeadbandTempDiff       = rNumericArgs(1)
        IF(WaterHeaterDesuperheater(DesuperheaterNum)%DeadbandTempDiff .LE.  0.0d0 .OR. &
           WaterHeaterDesuperheater(DesuperheaterNum)%DeadbandTempDiff .GT. 20.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                              ': '//TRIM(cNumericFieldNames(1))//' must be > 0 and <= 20. '//TRIM(cNumericFieldNames(1))//' = ' &
                              //TRIM(TrimSigDigits(rNumericArgs(1),1)))
          ErrorsFound=.TRUE.
        END IF

        !WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff       = rNumericArgs(2)
        ! Error limits on heat reclaim efficiency applied after source type identified

        WaterHeaterDesuperheater(DesuperheaterNum)%RatedInletWaterTemp = rNumericArgs(3)
        WaterHeaterDesuperheater(DesuperheaterNum)%RatedOutdoorAirTemp = rNumericArgs(4)
        WaterHeaterDesuperheater(DesuperheaterNum)%MaxInletWaterTemp   = rNumericArgs(5)

        IF (.NOT. lAlphaFieldBlanks(4) ) THEN
          WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTemp = GetCurveIndex(cAlphaArgs(4))
          IF (WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTemp .EQ. 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)// &
                                 ':  '//TRIM(cAlphaFieldNames(4))//' not found = '//TRIM(cAlphaArgs(4)))
            ErrorsFound = .TRUE.
          ELSE
            ! Verify Curve Object, only legal type is Quadratic
            SELECT CASE(GetCurveType(WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTemp))

            CASE('BIQUADRATIC')

              IF(WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTemp .GT. 0)THEN
                HEffFTemp = MIN(1.0d0,MAX(0.0d0,CurveValue(WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTemp, &
                                           WaterHeaterDesuperheater(DesuperheaterNum)%RatedInletWaterTemp, &
                                           WaterHeaterDesuperheater(DesuperheaterNum)%RatedOutdoorAirTemp)))
                IF(ABS(HEffFTemp - 1.0d0) .GT. 0.05d0)THEN
                  CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//  &
                     TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//'":')
                  CALL ShowContinueError('The '//TRIM(cAlphaFieldNames(4))//' should be normalized ')
                  CALL ShowContinueError(' to 1.0 at the rating point. Curve output at the rating point = ' &
                                         //TrimSigDigits(HEffFTemp,3))
                  CALL ShowContinueError(' The simulation continues using the user-specified curve.')
                END IF
              END IF

            CASE DEFAULT
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)// &
                                   '" illegal '//TRIM(cAlphaFieldNames(4))//' type for this object = '// &
                                   TRIM(GetCurveType(WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTemp)))
              ErrorsFound=.true.
            END SELECT
          END IF
        END IF

        WaterHeaterDesuperheater(DesuperheaterNum)%WaterInletNode = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsParent)

        WaterHeaterDesuperheater(DesuperheaterNum)%WaterOutletNode = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsParent)

        CoilSaveNodeNames(DesuperheaterNum)%InletNodeName1=cAlphaArgs(5)
        CoilSaveNodeNames(DesuperheaterNum)%OutletNodeName1=cAlphaArgs(6)

        WaterHeaterDesuperheater(DesuperheaterNum)%TankType = cAlphaArgs(7)

        IF (.NOT. SameString(WaterHeaterDesuperheater(DesuperheaterNum)%TankType,cMixedWHModuleObj)  &
          .AND. .NOT. SameString(WaterHeaterDesuperheater(DesuperheaterNum)%TankType,cStratifiedWHModuleObj)) THEN

          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(DesuperheaterNum)%Name)//':')
          CALL ShowContinueError('Desuperheater can only be used with '//cMixedWHModuleObj//' or '//cStratifiedWHModuleObj//'.')
          ErrorsFound = .TRUE.
        END IF

        WaterHeaterDesuperheater(DesuperheaterNum)%TankName = cAlphaArgs(8)

!       get heat reclaim object
        IF(SameString(cAlphaArgs(9),'Coil:Cooling:DX:SingleSpeed') .OR. &
           SameString(cAlphaArgs(9),'Coil:Cooling:DX:TwoSpeed') .OR. &
           SameString(cAlphaArgs(9),'Coil:Cooling:DX:TwoStageWithHumidityControlMode'))THEN
          WaterHeaterDesuperheater(DesuperheaterNum)%HeatingSourceType  = cAlphaArgs(9)
          WaterHeaterDesuperheater(DesuperheaterNum)%HeatingSourceName  = cAlphaArgs(10)
!         load DX coil structure for connection to desuperheater heating coil (refrigerated rack have been loaded)
          ErrFlag=.false.
          CALL GetDXCoilIndex(WaterHeaterDesuperheater(DesuperheaterNum)%HeatingSourceName, &
                              HeatingSourceNum, ErrFlag, cCurrentModuleObject)
          IF (ErrFlag) THEN
            CALL ShowContinueError('...occurs in '//TRIM(cCurrentModuleObject)//'='// &
                                   TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name))
            ErrorsFound=.true.
          ENDIF
        ELSE IF((SameString(cAlphaArgs(9),'Refrigeration:CompressorRack')) .OR. &
                (SameString(cAlphaArgs(9),'Refrigeration:Condenser:AirCooled')).OR.&
                (SameString(cAlphaArgs(9),'Refrigeration:Condenser:EvaporativeCooled')).OR.&
                (SameString(cAlphaArgs(9),'Refrigeration:Condenser:WaterCooled')))&
        THEN
          WaterHeaterDesuperheater(DesuperheaterNum)%HeatingSourceType  = cAlphaArgs(9)
          WaterHeaterDesuperheater(DesuperheaterNum)%HeatingSourceName  = cAlphaArgs(10)
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//':')
          CALL ShowContinueError(' desuperheater can only be used with Coil:Cooling:DX:SingleSpeed, ')
          CALL ShowContinueError(' Coil:Cooling:DX:TwoSpeed, Coil:Cooling:DX:TwoStageWithHumidityControlMode, '//  &
             'Refrigeration:CompressorRack,')
          CALL ShowContinueError(' Refrigeration:Condenser:AirCooled ,Refrigeration:Condenser:EvaporativeCooled, ')
          CALL ShowContinueError(' or Refrigeration:Condenser:WaterCooled.')
          ErrorsFound = .TRUE.
        END IF

!       Set up comp set for water side nodes (reverse inlet/outlet for water heater)
        CALL SetUpCompSets(WaterHeaterDesuperheater(DesuperheaterNum)%Type, WaterHeaterDesuperheater(DesuperheaterNum)%Name, &
                     WaterHeaterDesuperheater(DesuperheaterNum)%TankType, &
                     WaterHeaterDesuperheater(DesuperheaterNum)%TankName,cAlphaArgs(6),cAlphaArgs(5))

!       Find the DX equipment index associated with the desuperheater heating coil.
        IF(SameString(cAlphaArgs(9),'Refrigeration:CompressorRack'))THEN
          WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource = COMPRESSORRACK_REFRIGERATEDCASE
          DO RackNum = 1,NumRefrigeratedRacks
            IF(.NOT. SameString(HeatReclaimRefrigeratedRack(RackNum)%Name,cAlphaArgs(10)))CYCLE
            WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum = RackNum
            IF(ALLOCATED(HeatReclaimRefrigeratedRack))ValidSourceType(DesuperheaterNum) = .TRUE.
            EXIT
          END DO
          IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)// &
                        '" desuperheater heat source object not found: '//TRIM(cAlphaArgs(9))//' "'//TRIM(cAlphaArgs(10))//'"')
            ErrorsFound = .TRUE.
          END IF
        ELSEIF((SameString(cAlphaArgs(9),'Refrigeration:Condenser:AirCooled')).OR.&
                (SameString(cAlphaArgs(9),'Refrigeration:Condenser:EvaporativeCooled')).OR.&
                (SameString(cAlphaArgs(9),'Refrigeration:Condenser:WaterCooled')))&
        THEN
          WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource = CONDENSER_REFRIGERATION
          DO CondNum = 1,NumRefrigCondensers
            IF(.NOT. SameString(HeatReclaimRefrigCondenser(CondNum)%Name,cAlphaArgs(10)))CYCLE
            WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum = CondNum
            IF(ALLOCATED(HeatReclaimRefrigCondenser))ValidSourceType(DesuperheaterNum) = .TRUE.
            EXIT
          END DO
          IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)// &
                        '" desuperheater heat source object not found: '//TRIM(cAlphaArgs(9))//' "'//TRIM(cAlphaArgs(10))//'"')
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(cAlphaArgs(9),'Coil:Cooling:DX:SingleSpeed'))THEN
          WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource = COIL_DX_COOLING
          DO DXCoilNum = 1, NumDXCoils
            IF(.NOT. SameString(HeatReclaimDXCoil(DXCoilNum)%Name,cAlphaArgs(10)))CYCLE
            WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum = DXCoilNum
            IF(ALLOCATED(HeatReclaimDXCoil))ValidSourceType(DesuperheaterNum) = .TRUE.
            EXIT
          END DO
          IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)// &
                        '" desuperheater heat source object not found: '//TRIM(cAlphaArgs(9))//' "'//TRIM(cAlphaArgs(10))//'"')
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(cAlphaArgs(9),'Coil:Cooling:DX:TwoSpeed'))THEN
          WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource = COIL_DX_MULTISPEED
          DO DXCoilNum = 1, NumDXCoils
            IF(.NOT. SameString(HeatReclaimDXCoil(DXCoilNum)%Name,cAlphaArgs(10)))CYCLE
            WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum = DXCoilNum
            IF(ALLOCATED(HeatReclaimDXCoil))ValidSourceType(DesuperheaterNum) = .TRUE.
            EXIT
          END DO
          IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)// &
                        '" desuperheater heat source object not found: '//TRIM(cAlphaArgs(9))//' "'//TRIM(cAlphaArgs(10))//'"')
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(cAlphaArgs(9),'Coil:Cooling:DX:TwoStageWithHumidityControlMode'))THEN
          WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource = COIL_DX_MULTIMODE
          DO DXCoilNum = 1, NumDXCoils
            IF(.NOT. SameString(HeatReclaimDXCoil(DXCoilNum)%Name,cAlphaArgs(10)))CYCLE
            WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum = DXCoilNum
            IF(ALLOCATED(HeatReclaimDXCoil))ValidSourceType(DesuperheaterNum) = .TRUE.
            EXIT
          END DO
          IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)// &
                        '" desuperheater heat source object not found: '//TRIM(cAlphaArgs(9))//' "'//TRIM(cAlphaArgs(10))//'"')
            ErrorsFound = .TRUE.
          END IF
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                           '" invalid desuperheater heat source object: '//TRIM(cAlphaArgs(9))//' "'//TRIM(cAlphaArgs(10))//'"')
          ErrorsFound = .TRUE.
        END IF

        !Now have source type, so set limits on heat recovery efficiency
        IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource == CONDENSER_REFRIGERATION) THEN
          IF (lNumericFieldBlanks(2)) THEN
            WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff       = 0.8d0
          ELSE
            WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff       = rNumericArgs(2)
            IF(WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff .LE. 0.0d0 .OR. &
                 WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff .GT. 0.9d0) THEN
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                                ': '//TRIM(cNumericFieldNames(2))//' must be > 0.0 and <= 0.9, Efficiency = ' &
                                //TRIM(TrimSigDigits(WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff,3)))
               ErrorsFound=.TRUE.
            END IF
          END IF !Blank Num(2)
        ELSE ! max is 0.3 for all other sources
          IF (lNumericFieldBlanks(2)) THEN
            WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff       = 0.25d0
          ELSE
            WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff       = rNumericArgs(2)
            IF(WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff .LE. 0.0d0 .OR. &
               WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff .GT. 0.3d0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                                ': '//TRIM(cNumericFieldNames(2))//' must be > 0.0 and <= 0.3, '//TRIM(cNumericFieldNames(2))// &
                                ' = '//TRIM(TrimSigDigits(WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff,3)))
              ErrorsFound=.TRUE.
            END IF
          END IF !Blank Num(2)
        END IF  !setting limits on heat recovery efficiency

        WaterHeaterDesuperheater(DesuperheaterNum)%OperatingWaterFlowRate = rNumericArgs(6)
        IF(WaterHeaterDesuperheater(DesuperheaterNum)%OperatingWaterFlowRate .LE. 0.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                           ': '//TRIM(cNumericFieldNames(6))//' must be greater than 0. '//TRIM(cNumericFieldNames(6))//' = ' &
                           //TRIM(TrimSigDigits(rNumericArgs(6),6)))
          ErrorsFound=.TRUE.
        END IF

        WaterHeaterDesuperheater(DesuperheaterNum)%PumpElecPower   = rNumericArgs(7)
        IF(WaterHeaterDesuperheater(DesuperheaterNum)%PumpElecPower .LT. 0.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                               ': '//TRIM(cNumericFieldNames(7))//' must be >= 0. '//TRIM(cNumericFieldNames(7))//' = ' &
                               //TRIM(TrimSigDigits(rNumericArgs(7),2)))
          ErrorsFound=.TRUE.
        END IF

        IF((WaterHeaterDesuperheater(DesuperheaterNum)%PumpElecPower/ &
           WaterHeaterDesuperheater(DesuperheaterNum)%OperatingWaterFlowRate) .GT. 7.9264d6)THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                                ': '//TRIM(cNumericFieldNames(7))//' to '//TRIM(cNumericFieldNames(6))//' ratio > 7.9264E6.' &
                                //' '//TRIM(cNumericFieldNames(7))//' to '//TRIM(cNumericFieldNames(6))//' = ' &
                                //TRIM(TrimSigDigits((WaterHeaterDesuperheater(DesuperheaterNum)%PumpElecPower/ &
                                                     WaterHeaterDesuperheater(DesuperheaterNum)%OperatingWaterFlowRate),3)))
          CALL ShowContinueError(' Suggest reducing '//TRIM(cNumericFieldNames(7))//' or increasing '// &
                                   TRIM(cNumericFieldNames(6))//'.')
          CALL ShowContinueError(' The simulation will continue using the user defined values.')
        END IF

        WaterHeaterDesuperheater(DesuperheaterNum)%PumpFracToWater = rNumericArgs(8)
        IF(WaterHeaterDesuperheater(DesuperheaterNum)%PumpFracToWater .LT. 0.0d0 .OR. &
           WaterHeaterDesuperheater(DesuperheaterNum)%PumpFracToWater .GT. 1.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                               ': '//TRIM(cNumericFieldNames(8))//' must be >= 0 or <= 1. '//TRIM(cNumericFieldNames(8))//' = ' &
                               //TRIM(TrimSigDigits(rNumericArgs(8),3)))
          ErrorsFound=.TRUE.
        END IF

        WaterHeaterDesuperheater(DesuperheaterNum)%OnCycParaLoad        = rNumericArgs(9)
        IF(WaterHeaterDesuperheater(DesuperheaterNum)%OnCycParaLoad .LT. 0.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                               ': '//TRIM(cNumericFieldNames(9))//' must be >= 0. '//TRIM(cNumericFieldNames(9))//' = ' &
                               //TRIM(TrimSigDigits(rNumericArgs(9),2)))
          ErrorsFound=.TRUE.
        END IF

        WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaLoad        = rNumericArgs(10)
        IF(WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaLoad .LT. 0.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                               ': '//TRIM(cNumericFieldNames(10))//' must be >= 0. '//TRIM(cNumericFieldNames(10))//' = ' &
                               //TRIM(TrimSigDigits(rNumericArgs(10),2)))
          ErrorsFound=.TRUE.
        END IF

      END DO

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in getting '//TRIM(cCurrentModuleObject)//  &
           ' input. Preceding condition causes termination.')
      END IF

    END IF

!!!=======   Get HEAT PUMP:WATER HEATER ===============================================================================

!   get input for heat pump water heater object
    IF (NumHeatPumpWaterHeater > 0) THEN
      cCurrentModuleObject = 'WaterHeater:HeatPump'
      DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater

        CALL GetObjectItem(cCurrentModuleObject,HPWaterHeaterNum, &
          cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
          NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
          AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1),HPWaterHeater%Name, &
                        HPWaterHeaterNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF

        HPWaterHeater(HPWaterHeaterNum)%Name    = cAlphaArgs(1)
        HPWaterHeater(HPWaterHeaterNum)%Type    = cCurrentModuleObject
        HPWaterHeater(HPWaterHeaterNum)%TypeNum = HeatPumpWaterHeater

!       convert schedule name to pointer
        IF (.not. lAlphaFieldBlanks(2)) THEN
          HPWaterHeater(HPWaterHeaterNum)%AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2))
          IF (HPWaterHeater(HPWaterHeaterNum)%AvailSchedPtr .EQ. 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", not found')
            CALL ShowContinueError(trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
            ErrorsFound=.TRUE.
          END IF
        ELSE
          HPWaterHeater(HPWaterHeaterNum)%AvailSchedPtr = ScheduleAlwaysOn
        ENDIF

!       convert schedule name to pointer
        IF (.not. lAlphaFieldBlanks(3)) THEN
          HPWaterHeater(HPWaterHeaterNum)%SetpointTempSchedule = GetScheduleIndex(cAlphaArgs(3))
          IF (HPWaterHeater(HPWaterHeaterNum)%SetpointTempSchedule .EQ. 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", not found')
            CALL ShowContinueError(trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
            ErrorsFound=.TRUE.
          ENDIF
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", ')
          CALL ShowContinueError('required '//trim(cAlphaFieldNames(3))//' is blank.')
          ErrorsFound=.TRUE.
        END IF

        HPWaterHeater(HPWaterHeaterNum)%DeadbandTempDiff       = rNumericArgs(1)
        IF(HPWaterHeater(HPWaterHeaterNum)%DeadbandTempDiff .LE.  0.0d0 .OR. &
           HPWaterHeater(HPWaterHeaterNum)%DeadbandTempDiff .GT. 20.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", ')
          CALL ShowContinueError(trim(cNumericFieldNames(1))//' difference must be > 0 and <= 20. Dead band = ' &
                         //TRIM(TrimSigDigits(rNumericArgs(1),1)))
          ErrorsFound=.TRUE.
        END IF

        HPWaterHeater(HPWaterHeaterNum)%CondWaterInletNode = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsParent)
        HPWHSaveNodeNames(HPWaterHeaterNum)%InletNodeName1=cAlphaArgs(4)
        HPWaterHeater(HPWaterHeaterNum)%CondWaterOutletNode = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsParent)
        HPWHSaveNodeNames(HPWaterHeaterNum)%OutletNodeName1=cAlphaArgs(5)

        HPWaterHeater(HPWaterHeaterNum)%OperatingWaterFlowRate = rNumericArgs(2)
        IF(HPWaterHeater(HPWaterHeaterNum)%OperatingWaterFlowRate .LE. 0.0d0 .AND. rNumericArgs(2) /= AutoCalculate) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", ')
          CALL ShowContinueError(trim(cNumericFieldNames(2))//' must be greater than 0. Condenser water flow rate = ' &
                         //TRIM(TrimSigDigits(rNumericArgs(2),6)))
          ErrorsFound=.TRUE.
        END IF

        HPWaterHeater(HPWaterHeaterNum)%OperatingAirFlowRate   = rNumericArgs(3)
        IF(HPWaterHeater(HPWaterHeaterNum)%OperatingAirFlowRate .LE. 0.0d0 .AND. rNumericArgs(3) /= AutoCalculate) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", ')
          CALL ShowContinueError(trim(cNumericFieldNames(3))//' must be greater than 0. Evaporator air flow rate = ' &
                         //TRIM(TrimSigDigits(rNumericArgs(3),6)))
          ErrorsFound=.TRUE.
        END IF

        SELECT CASE (cAlphaArgs(6))

          CASE ('SCHEDULE')
            HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration = AmbientTempSchedule
            IF (.not. lAlphaFieldBlanks(11)) THEN
              HPWaterHeater(HPWaterHeaterNum)%AmbientTempSchedule   = GetScheduleIndex(cAlphaArgs(11))
              IF (HPWaterHeater(HPWaterHeaterNum)%AmbientTempSchedule .EQ. 0) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", not found')
                CALL ShowContinueError(trim(cAlphaFieldNames(11))//'="'//TRIM(cAlphaArgs(11))//'".')
                ErrorsFound = .TRUE.
              ENDIF
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", ')
              CALL ShowContinueError('required '//trim(cAlphaFieldNames(11))//' is blank.')
              ErrorsFound=.TRUE.
            END IF
            IF (.not. lAlphaFieldBlanks(12)) THEN
              HPWaterHeater(HPWaterHeaterNum)%AmbientRHSchedule = GetScheduleIndex(cAlphaArgs(12))
              IF (HPWaterHeater(HPWaterHeaterNum)%AmbientRHSchedule .EQ. 0) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", not found')
                CALL ShowContinueError(trim(cAlphaFieldNames(12))//'="'//TRIM(cAlphaArgs(12))//'".')
                ErrorsFound = .TRUE.
              ELSE
                IF (.NOT. CheckScheduleValueMinMax(HPWaterHeater(HPWaterHeaterNum)%AmbientRHSchedule,'>=',0.0d0,'<=',1.0d0)) THEN
                  CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//  &
                     '", invalid values')
                  CALL ShowContinueError(trim(cAlphaFieldNames(12))//'="'//TRIM(cAlphaArgs(12))//'",'//  &
                                  ' schedule values must be (>=0., <=1.)')
                  ErrorsFound=.true.
                END IF
              END IF
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", ')
              CALL ShowContinueError('required '//trim(cAlphaFieldNames(12))//' is blank.')
              ErrorsFound=.TRUE.
            END IF

          CASE ('ZONEAIRONLY')
            HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration = AmbientTempZone
            IF (.not. lAlphaFieldBlanks(13)) THEN
              HPWaterHeater(HPWaterHeaterNum)%AmbientTempZone = FindItemInList(cAlphaArgs(13),Zone%Name,NumOfZones)
              IF (HPWaterHeater(HPWaterHeaterNum)%AmbientTempZone .EQ. 0) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", not found')
                CALL ShowContinueError(trim(cAlphaFieldNames(13))//'="'//TRIM(cAlphaArgs(13))//'".')
                ErrorsFound = .TRUE.
              END IF
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", ')
              CALL ShowContinueError('required '//trim(cAlphaFieldNames(13))//' is blank.')
              ErrorsFound=.TRUE.
            ENDIF

          CASE ('OUTDOORAIRONLY')
            HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration = AmbientTempOutsideAir

          CASE ('ZONEANDOUTDOORAIR')
            HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration = AmbientTempZoneAndOA
            IF (.not. lAlphaFieldBlanks(13)) THEN
              HPWaterHeater(HPWaterHeaterNum)%AmbientTempZone = FindItemInList(cAlphaArgs(13),Zone%Name,NumOfZones)
              IF (HPWaterHeater(HPWaterHeaterNum)%AmbientTempZone .EQ. 0) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", not found')
                CALL ShowContinueError(trim(cAlphaFieldNames(13))//'="'//TRIM(cAlphaArgs(13))//'".')
                ErrorsFound = .TRUE.
              END IF
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", ')
              CALL ShowContinueError('required '//trim(cAlphaFieldNames(13))//' is blank.')
              ErrorsFound=.TRUE.
            ENDIF

        END SELECT

!       Read air inlet nodes after mixer/splitter nodes have been read in (cAlphaArgs 7-10),
!       Node_ConnectionType differs for inlet node if mixer/splitter node exists

        HPWaterHeater(HPWaterHeaterNum)%TankType = cAlphaArgs(14)

        IF (.NOT. SameString(HPWaterHeater(HPWaterHeaterNum)%TankType,cMixedWHModuleObj)  &
          .AND. .NOT. SameString(HPWaterHeater(HPWaterHeaterNum)%TankType,cStratifiedWHModuleObj)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
          CALL ShowContinueError('Heat pump water heater can only be used with '//  &
                                  cMixedWHModuleObj//' or '//cStratifiedWHModuleObj//'.')
          ErrorsFound = .TRUE.
        END IF

!       Verify tank name after Water Heater:Mixed objects have been read in
        HPWaterHeater(HPWaterHeaterNum)%TankName = cAlphaArgs(15)

!       Get the water heater tank use side inlet node names for HPWHs connected to a plant loop
!       Save the name of the node for use with set up comp sets
        HPWHSaveNodeNames(HPWaterHeaterNum)%InletNodeName2=cAlphaArgs(16)
        HPWHSaveNodeNames(HPWaterHeaterNum)%OutletNodeName2=cAlphaArgs(17)

        IF(.not. lAlphaFieldBlanks(16) .AND. .not. lAlphaFieldBlanks(17))THEN
          HPWaterHeater(HPWaterHeaterNum)%WHUseInletNode = &
               GetOnlySingleNode(cAlphaArgs(16),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsParent)
          HPWaterHeater(HPWaterHeaterNum)%WHUseOutletNode = &
               GetOnlySingleNode(cAlphaArgs(17),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsParent)
        END IF

!       get Coil:DX:HeatPumpWaterHeater object
        HPWaterHeater(HPWaterHeaterNum)%DXCoilType = cAlphaArgs(18)
        HPWaterHeater(HPWaterHeaterNum)%DXCoilName = cAlphaArgs(19)

!       check that the DX Coil exists
        IF(.NOT. SameString(HPWaterHeater(HPWaterHeaterNum)%DXCoilType,'Coil:WaterHeating:AirToWaterHeatPump'))THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
          CALL ShowContinueError('Heat pump water heater can only be used with Coil:WaterHeating:AirToWaterHeatPump.')
          ErrorsFound = .TRUE.
        END IF

        DXCoilErrFlag=.false.
        CALL GetDXCoilIndex(HPWaterHeater(HPWaterHeaterNum)%DXCoilName, &
                            HPWaterHeater(HPWaterHeaterNum)%DXCoilNum, DXCoilErrFlag, cCurrentModuleObject)
        IF (DXCoilErrFlag) THEN
          CALL ShowContinueError('...occurs in WaterHeater:HeatPump ='//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name))
          CALL ShowContinueError('...entered DX CoilType='//TRIM(HPWaterHeater(HPWaterHeaterNum)%DXCoilType))
          ErrorsFound   = .TRUE.
        END IF

!       Set up comp set for condenser water side nodes (reverse inlet/outlet for water heater)
        CALL SetUpCompSets(HPWaterHeater(HPWaterHeaterNum)%Type, HPWaterHeater(HPWaterHeaterNum)%Name, &
                     HPWaterHeater(HPWaterHeaterNum)%DXCoilType, &
                     HPWaterHeater(HPWaterHeaterNum)%DXCoilName,cAlphaArgs(4),cAlphaArgs(5))

        CALL SetUpCompSets(HPWaterHeater(HPWaterHeaterNum)%Type, HPWaterHeater(HPWaterHeaterNum)%Name, &
                     HPWaterHeater(HPWaterHeaterNum)%TankType, &
                     HPWaterHeater(HPWaterHeaterNum)%TankName,cAlphaArgs(5),cAlphaArgs(4))

        HPWaterHeater(HPWaterHeaterNum)%MinAirTempForHPOperation = rNumericArgs(4)
        IF(HPWaterHeater(HPWaterHeaterNum)%MinAirTempForHPOperation .LT. 5) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//&
               '": minimum inlet air temperature for heat pump compressor operation must be greater than or equal to 5 C.')
          CALL ShowContinueError('...Minimum inlet air temperature = '//TRIM(TrimSigDigits(rNumericArgs(4),1)))
        END IF

!       Get compressor location
        SELECT CASE (cAlphaArgs(20))
          CASE ('SCHEDULE')
            HPWaterHeater(HPWaterHeaterNum)%CrankcaseTempIndicator = CrankcaseTempSchedule
            IF (.not. lAlphaFieldBlanks(21)) THEN
              HPWaterHeater(HPWaterHeaterNum)%CrankcaseTempSchedule = GetScheduleIndex(cAlphaArgs(21))
              IF (HPWaterHeater(HPWaterHeaterNum)%CrankcaseTempSchedule .EQ. 0) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", not found')
                CALL ShowContinueError(trim(cAlphaFieldNames(21))//'="'//TRIM(cAlphaArgs(21))//'".')
                ErrorsFound = .TRUE.
              END IF
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", ')
              CALL ShowContinueError('required '//trim(cAlphaFieldNames(21))//' is blank.')
              ErrorsFound=.TRUE.
            ENDIF

          CASE ('ZONE')
            HPWaterHeater(HPWaterHeaterNum)%CrankcaseTempIndicator = CrankcaseTempZone
            IF(HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempOutsideAir .OR. &
               HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempSchedule)THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
                                   '":  Inlet Air Configuration must be Zone Air Only or Zone And')
              CALL ShowContinueError(' Outdoor Air when compressor location equals ZONE.')
              ErrorsFound = .TRUE.
            END IF

            IF(.not. lAlphaFieldBlanks(21))THEN
              CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
                        '"  '//trim(cAlphaFieldNames(21))//' was provided but will not be used based'// &
                        ' on compressor location input="'//TRIM(cAlphaArgs(20))//'".')
            END IF
          CASE ('OUTDOORS')
            HPWaterHeater(HPWaterHeaterNum)%CrankcaseTempIndicator = CrankcaseTempExterior
            IF(.not. lAlphaFieldBlanks(21))THEN
              CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
                        '"  '//trim(cAlphaFieldNames(21))//' was provided but will not be used based'// &
                        ' on '//trim(cAlphaFieldNames(21))//'="'//TRIM(cAlphaArgs(20))//'".')
            END IF

        END SELECT

        HPWaterHeater(HPWaterHeaterNum)%FanType = cAlphaArgs(22)
        HPWaterHeater(HPWaterHeaterNum)%FanName = cAlphaArgs(23)

!       check that the fan exists
        ErrFlag=.false.
        CALL GetFanIndex(HPWaterHeater(HPWaterHeaterNum)%FanName, &
                         HPWaterHeater(HPWaterHeaterNum)%FanNum, ErrFlag,cCurrentModuleObject)
        IF (ErrFlag) THEN
          CALL ShowContinueError('...occurs in unit="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'".')
          ErrorsFound=.TRUE.
        ENDIF

        ErrFlag=.false.
        CALL GetFanType(HPWaterHeater(HPWaterHeaterNum)%FanName,HPWaterHeater(HPWaterHeaterNum)%FanType_Num,ErrFlag, &
                       cCurrentModuleObject,HPWaterHeater(HPWaterHeaterNum)%Name)

        IF(ErrFlag)THEN
          ErrorsFound=.TRUE.
        ELSE
          IF(HPWaterHeater(HPWaterHeaterNum)%FanType_Num .NE. FanType_SimpleOnOff)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' illegal fan type specified.')
            CALL ShowContinueError('Occurs in unit="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'".')
            CALL ShowContinueError(' The fan object ('//TRIM(HPWaterHeater(HPWaterHeaterNum)%FanName)// &
                                   ') type must be Fan:OnOff when used with a heat pump water heater')
            ErrorsFound = .TRUE.
          ELSE
            IF(.NOT. SameString(HPWaterHeater(HPWaterHeaterNum)%FanType,'Fan:OnOff'))THEN
              CALL ShowWarningError(TRIM(cCurrentModuleObject)//' illegal fan type = '//  &
                 TRIM(HPWaterHeater(HPWaterHeaterNum)%FanType))
              CALL ShowContinueError('Occurs in unit = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name))
              CALL ShowContinueError(' The fan object ('//TRIM(HPWaterHeater(HPWaterHeaterNum)%FanName)// &
                                     ') is actually the correct fan type and the simulation continues.')
              CALL ShowContinueError(' Node connection errors will result due to the inconsistent fan type.')
            END IF
          END IF
        END IF

        CALL GetFanVolFlow(HPWaterHeater(HPWaterHeaterNum)%FanNum,FanVolFlow)

        IF(FanVolFlow .NE. AutoSize .AND. .NOT. ErrFlag)THEN
          IF(FanVolFlow .LT. HPWaterHeater(HPWaterHeaterNum)%OperatingAirFlowRate)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' - air flow rate = '//TRIM(TrimSigDigits(FanVolFlow,7))// &
              ' in fan object '//TRIM(HPWaterHeater(HPWaterHeaterNum)%FanName)// &
              ' is less than the  HPWHs evaporator air flow rate.')
            CALL ShowContinueError(' The fan flow rate must be >= to the HPWHs evaporator volumetric air flow rate.')
            CALL ShowContinueError(' Occurs in unit = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF

        IF(SameString(cAlphaArgs(24),'BlowThrough')) THEN
          HPWaterHeater(HPWaterHeaterNum)%FanPlacement = BlowThru

        ELSEIF(SameString(cAlphaArgs(24),'DrawThrough')) THEN
          HPWaterHeater(HPWaterHeaterNum)%FanPlacement = DrawThru

        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", invalid ')
          CALL ShowContinueError(trim(cAlphaFieldNames(24))//'="'//TRIM(cAlphaArgs(24))//'".')
          ErrorsFound = .TRUE.
        END IF

        IF(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum .GT. 0)THEN
!         get HPWH capacity, air inlet node, and PLF curve info from DX coil object
          HPWaterHeater(HPWaterHeaterNum)%Capacity = DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%RatedTotCap2
          HPWaterHeater(HPWaterHeaterNum)%DXCoilAirInletNode = DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%AirInNode
          HPWaterHeater(HPWaterHeaterNum)%DXCoilPLFFPLR = DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%PLFFPLR(1)
!         check the range of condenser pump power to be <= 5 gpm/ton
          IF(DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%HPWHCondPumpElecNomPower/ &
             DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%RatedTotCap2 .GT. 0.1422d0)THEN
            CALL ShowWarningError(TRIM(DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%DXCoilType)// &
                 '= '//TRIM(DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%Name)//&
                ': Rated condenser pump power per watt of rated heating capacity has exceeded the recommended'// &
                ' maximum of 0.1422 W/W (41.67 watt/MBH). Condenser pump power per watt = ' &
                //TRIM(TrimSigDigits((DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%HPWHCondPumpElecNomPower/ &
                                      DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%RatedTotCap2),4)))
          END IF
        END IF

        IF (HPWaterHeater(HPWaterHeaterNum)%OperatingWaterFlowRate == AutoCalculate) THEN
          HPWaterHeater(HPWaterHeaterNum)%OperatingWaterFlowRate = 0.00000004487d0 * HPWaterHeater(HPWaterHeaterNum)%Capacity
          HPWaterHeater(HPWaterHeaterNum)%WaterFlowRateAutosized = .TRUE.
        END IF

        IF (HPWaterHeater(HPWaterHeaterNum)%OperatingAirFlowRate == AutoCalculate) THEN
          HPWaterHeater(HPWaterHeaterNum)%OperatingAirFlowRate = 0.00005035d0 * HPWaterHeater(HPWaterHeaterNum)%Capacity
          HPWaterHeater(HPWaterHeaterNum)%AirFlowRateAutosized = .TRUE.
        END IF

        HPWaterHeater(HPWaterHeaterNum)%OnCycParaLoad  = rNumericArgs(5)
        IF(HPWaterHeater(HPWaterHeaterNum)%OnCycParaLoad .LT. 0.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'",')
          CALL ShowContinueError(trim(cNumericFieldNames(5))//' must be >= 0. '//trim(cNumericFieldNames(5))//' = ' &
                         //TRIM(TrimSigDigits(rNumericArgs(5),2)))
          ErrorsFound=.TRUE.
        END IF

        HPWaterHeater(HPWaterHeaterNum)%OffCycParaLoad = rNumericArgs(6)
        IF(HPWaterHeater(HPWaterHeaterNum)%OffCycParaLoad .LT. 0.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'",')
          CALL ShowContinueError(trim(cNumericFieldNames(6))//' must be >= 0. '//trim(cNumericFieldNames(6))//' = ' &
                         //TRIM(TrimSigDigits(rNumericArgs(6),2)))
          ErrorsFound=.TRUE.
        END IF

        IF(SameString(cAlphaArgs(25),'Zone'))THEN
          HPWaterHeater(HPWaterHeaterNum)%ParasiticTempIndicator = AmbientTempZone
          IF(HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempOutsideAir .OR. &
             HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempSchedule)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'",')
            CALL ShowContinueError(trim(cAlphaFieldNames(25))//' must be ZoneAirOnly or ZoneAndOutdoorAir')
            CALL ShowContinueError(' when parasitic heat rejection location equals Zone.')
            ErrorsFound = .TRUE.
          END IF
        ELSEIF(SameString(cAlphaArgs(25),'Outdoors'))THEN
          HPWaterHeater(HPWaterHeaterNum)%ParasiticTempIndicator = AmbientTempOutsideAir
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
          CALL ShowContinueError(' parasitic heat rejection location must be either Zone or Outdoors.')
          ErrorsFound = .TRUE.
        END IF

!       get mixer/splitter nodes only when Inlet Air Configuration is ZoneAndOutdoorAir
        IF (.not. lAlphaFieldBlanks(26)) THEN
!         For the inlet air mixer node, NodeConnectionType is outlet from the HPWH inlet air node
          IF(HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZoneAndOA)THEN
            HPWaterHeater(HPWaterHeaterNum)%InletAirMixerNode = &
               GetOnlySingleNode(cAlphaArgs(26),ErrorsFound,'WaterHeater:HeatPump inlet air mixer',cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
          ELSE
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
            CALL ShowContinueError('Inlet air mixer node name specified but only required '//  &
               'when Inlet Air Configuration is selected'// &
               ' as Zone and OutdoorAir. Node name disregarded and simulation continues.')
          END IF
        ELSEIF(lAlphaFieldBlanks(26).AND. HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZoneAndOA) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
            CALL ShowContinueError('Inlet air mixer node name required '//  &
               'when Inlet Air Configuration is selected as ZoneAndOutdoorAir.')
            ErrorsFound = .TRUE.
        END IF

        IF (.not. lAlphaFieldBlanks(27)) THEN
!         For the outlet air splitter node, NodeConnectionType is inlet to the HPWH outlet air node
          IF(HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZoneAndOA)THEN
            HPWaterHeater(HPWaterHeaterNum)%OutletAirSplitterNode = &
              GetOnlySingleNode(cAlphaArgs(27),ErrorsFound,TRIM(cCurrentModuleObject)//'-OUTLET AIR SPLITTER',cAlphaArgs(1), &
                          NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
          ELSE
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
            CALL ShowContinueError('Outlet air splitter node name specified but only required when '//  &
               'Inlet Air Configuration is selected'// &
               ' as ZoneAndOutdoorAir. Node name disregarded and simulation continues.')
          END IF
        ELSEIF(lAlphaFieldBlanks(27) .AND. HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZoneAndOA) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
            CALL ShowContinueError('Outlet air splitter node name required when '//  &
               'Inlet Air Configuration is selected as ZoneAndOutdoorAir.')
            ErrorsFound = .TRUE.
        END IF

!       get node data for HPWH
        IF(HPWaterHeater(HPWaterHeaterNum)%InletAirMixerNode /= 0) THEN
!         when mixer/splitter nodes are used the HPWH's inlet/outlet node are set up as ObjectIsNotParent

          HPWaterHeater(HPWaterHeaterNum)%HeatPumpAirInletNode = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject)//'-INLET AIR MIXER',cAlphaArgs(1), &
                           NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

          HPWaterHeater(HPWaterHeaterNum)%HeatPumpAirOutletNode = &
               GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject)//'-OUTLET AIR SPLITTER',cAlphaArgs(1), &
                           NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

          HPWaterHeater(HPWaterHeaterNum)%OutsideAirNode = &
             GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsParent)
          IF (cAlphaArgs(9) /= ' ') THEN
            Call CheckAndAddAirNodeNumber(HPWaterHeater(HPWaterHeaterNum)%OutsideAirNode,Okay)
            IF (.not. Okay) THEN
              CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
                 '": Adding outdoor air node='//TRIM(cAlphaArgs(9)))
            ENDIF
          ENDIF

          HPWaterHeater(HPWaterHeaterNum)%ExhaustAirNode = &
             GetOnlySingleNode(cAlphaArgs(10),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                        NodeType_Air,NodeConnectionType_ReliefAir,1,ObjectIsParent)

        ELSE
!         when mixer/splitter nodes are NOT used the HPWH's inlet/outlet nodes are set up as ObjectIsParent
          IF(HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempSchedule)THEN
!           for scheduled HPWH's the inlet node is not on any branch or parent object, make it an outlet node
!           to avoid node connection errors
            HPWaterHeater(HPWaterHeaterNum)%HeatPumpAirInletNode = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

            HPWaterHeater(HPWaterHeaterNum)%HeatPumpAirOutletNode = &
               GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

          ELSE ! HPWH is connected to a zone with no mixer/splitter nodes
            IF(HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZone)THEN
              HPWaterHeater(HPWaterHeaterNum)%HeatPumpAirInletNode = &
                 GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

              HPWaterHeater(HPWaterHeaterNum)%HeatPumpAirOutletNode = &
                 GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
            ELSE ! HPWH is located outdoors
              HPWaterHeater(HPWaterHeaterNum)%OutsideAirNode = &
                 GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsParent)
              IF (.not. lAlphaFieldBlanks(9)) THEN
                CALL CheckAndAddAirNodeNumber(HPWaterHeater(HPWaterHeaterNum)%OutsideAirNode,Okay)
                IF (.not. Okay) THEN
                  CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
                     '": Adding outdoor air node ='//TRIM(cAlphaArgs(9)))
                ENDIF
              ENDIF

             HPWaterHeater(HPWaterHeaterNum)%ExhaustAirNode = &
                 GetOnlySingleNode(cAlphaArgs(10),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_ReliefAir,1,ObjectIsParent)
            END IF
          END IF
        END IF

!       check that the HPWH inlet and outlet nodes are in the same zone (ZoneHVAC:EquipmentConnections) when
!       Inlet Air Configuration is Zone Air Only or Zone and Outdoor Air
        IF((HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZone .OR. &
           HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZoneAndOA) .AND. &
           HPWaterHeater(HPWaterHeaterNum)%AmbientTempZone .GT. 0)THEN
          IF (.not. ZoneEquipInputsFilled) THEN
            CALL GetZoneEquipmentData
            ZoneEquipInputsFilled=.true.
          ENDIF
          IF(ALLOCATED(ZoneEquipConfig))THEN
            FoundInletNode  = .FALSE.
            FoundOutletNode = .FALSE.
            DO ZoneNum = 1, NumOfZones
              IF(HPWaterHeater(HPWaterHeaterNum)%AmbientTempZone .EQ. ZoneEquipConfig(ZoneNum)%ActualZoneNum)EXIT
            END DO
            IF(ZoneNum .LE. NumOfZones)THEN
              DO SupAirIn  = 1,ZoneEquipConfig(ZoneNum)%NumInletNodes
                IF(HPWaterHeater(HPWaterHeaterNum)%HeatPumpAirOutletNode .NE. ZoneEquipConfig(ZoneNum)%InletNode(SupAirIn))CYCLE
                FoundOutletNode = .TRUE.
              END DO
              DO ExhAirOut = 1,ZoneEquipConfig(ZoneNum)%NumExhaustNodes
                IF(HPWaterHeater(HPWaterHeaterNum)%HeatPumpAirInletNode .NE. ZoneEquipConfig(ZoneNum)%ExhaustNode(ExhAirOut))CYCLE
                FoundInletNode = .TRUE.
              END DO
              IF(.NOT. FoundInletNode)THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
                CALL ShowContinueError('The HPWH''s air inlet node name = '//TRIM(cAlphaArgs(7))//' was not properly specified ')
                CALL ShowContinueError('as an exhaust air node for zone = '//TRIM(cAlphaArgs(13))//' in a '// &
                                       'ZoneHVAC:EquipmentConnections object.')
                ErrorsFound = .TRUE.
              END IF
              IF(.NOT. FoundOutletNode)THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
                CALL ShowContinueError('The HPWH''s air outlet node name = '//TRIM(cAlphaArgs(8))//' was not properly specified ')
                CALL ShowContinueError('as an inlet air node for zone = '//TRIM(cAlphaArgs(13))//' in a '// &
                                       'ZoneHVAC:EquipmentConnections object.')
                ErrorsFound = .TRUE.
              END IF
            END IF
          ELSE
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'":')
            CALL ShowContinueError('Heat pump water heater air inlet node name and air outlet node name must be'// &
                                   ' listed in a ZoneHVAC:EquipmentConnections object when Inlet Air Configuration'// &
                                   ' is equal to ZoneAirOnly or ZoneAndOutdoorAir.')
            ErrorsFound = .TRUE.
          END IF
        END IF

!       only get the inlet air mixer schedule if the inlet air configuration is zone and outdoor air
        IF (.not. lAlphaFieldBlanks(28) .AND. HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZoneAndOA) THEN
          HPWaterHeater(HPWaterHeaterNum)%InletAirMixerSchPtr = GetScheduleIndex(cAlphaArgs(28))
          IF (HPWaterHeater(HPWaterHeaterNum)%InletAirMixerSchPtr .EQ. 0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", not found')
              CALL ShowContinueError(trim(cAlphaFieldNames(28))//'="'//TRIM(cAlphaArgs(28))//'",')
            ErrorsFound = .TRUE.
          ELSE
!           check schedule values to be between 0 and 1
            ValidScheduleValue=CheckScheduleValueMinMax(HPWaterHeater(HPWaterHeaterNum)%InletAirMixerSchPtr,'>=',0.0d0, '<=',1.0d0)
            IF (.not. ValidScheduleValue) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", not found')
              CALL ShowContinueError(trim(cAlphaFieldNames(28))//' values out of range of 0 to 1, Schedule="'//  &
                 TRIM(cAlphaArgs(28))//'".')
              ErrorsFound=.TRUE.
            ENDIF
!           set outlet air splitter schedule index equal to inlet air mixer schedule index
!           (place holder for when zone pressurization/depressurization is allowed and different schedules can be used)
            HPWaterHeater(HPWaterHeaterNum)%OutletAirSplitterSchPtr = GetScheduleIndex(cAlphaArgs(28))
          END IF
        END IF

!       set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
        IF(HPWaterHeater(HPWaterHeaterNum)%FanPlacement .EQ. DrawThru)THEN
           IF(HPWaterHeater(HPWaterHeaterNum)%OutletAirSplitterNode .NE. 0) THEN
             HPWaterHeater(HPWaterHeaterNum)%FanOutletNode = HPWaterHeater(HPWaterHeaterNum)%OutletAirSplitterNode
           ELSE
             IF(HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempOutsideAir)THEN
               HPWaterHeater(HPWaterHeaterNum)%FanOutletNode = HPWaterHeater(HPWaterHeaterNum)%ExhaustAirNode
             ELSE
               HPWaterHeater(HPWaterHeaterNum)%FanOutletNode = HPWaterHeater(HPWaterHeaterNum)%HeatPumpAirOutletNode
             END IF
           END IF
        ELSEIF(HPWaterHeater(HPWaterHeaterNum)%FanPlacement .EQ. BlowThru) THEN
!           set fan outlet node variable for use in setting Node(FanOutletNode)%MassFlowRateMax for fan object
            HPWaterHeater(HPWaterHeaterNum)%FanOutletNode = DXCoil(HPWaterHeater(HPWaterHeaterNum)%DXCoilNum)%AirInNode
        END IF

!       set the max mass flow rate for outdoor fans
        Node(HPWaterHeater(HPWaterHeaterNum)%FanOutletNode)%MassFlowRateMax =    &
                HPWaterHeater(HPWaterHeaterNum)%OperatingAirFlowRate *   &
                    PsyRhoAirFnPbTdbW(OutBaroPress,20.0d0, 0.0d0)

        IF(HPWaterHeater(HPWaterHeaterNum)%FanPlacement .EQ. BlowThru)THEN
          IF(HPWaterHeater(HPWaterHeaterNum)%InletAirMixerNode .GT. 0)THEN
!           cAlphaArgs(26) = Inlet Air Mixer Node
            FanInletNode = cAlphaArgs(26)
            FanOutletNode = 'UNDEFINED'
          ELSE
            IF(HPWaterHeater(HPWaterHeaterNum)%OutsideAirNode .EQ. 0)THEN
!             cAlphaArgs(7) = Heat Pump Air Inlet Node
              FanInletNode = cAlphaArgs(7)
              FanOutletNode = 'UNDEFINED'
            ELSE
!             cAlphaArgs(9) = Outside Air Node
              FanInletNode = cAlphaArgs(9)
              FanOutletNode = 'UNDEFINED'
            END IF
          END IF
          IF(HPWaterHeater(HPWaterHeaterNum)%OutletAirSplitterNode .GT. 0)THEN
!           cAlphaArgs(27) = Outlet Air Splitter Node
            CoilInletNode = 'UNDEFINED'
            CoilOutletNode = cAlphaArgs(27)
          ELSE
            IF(HPWaterHeater(HPWaterHeaterNum)%OutsideAirNode .EQ. 0)THEN
!             cAlphaArgs(8) = Heat Pump Air Outlet Node
              CoilInletNode = 'UNDEFINED'
              CoilOutletNode = cAlphaArgs(8)
            ELSE
              CoilInletNode = 'UNDEFINED'
!             cAlphaArgs(10) = Exhaust Air Node
              CoilOutletNode = cAlphaArgs(10)
            END IF
          END IF
        ELSE
          IF(HPWaterHeater(HPWaterHeaterNum)%InletAirMixerNode .GT. 0)THEN
            CoilInletNode = cAlphaArgs(26)
            CoilOutletNode = 'UNDEFINED'
          ELSE
            IF(HPWaterHeater(HPWaterHeaterNum)%OutsideAirNode .EQ. 0)THEN
              CoilInletNode = cAlphaArgs(7)
              CoilOutletNode = 'UNDEFINED'
            ELSE
              CoilInletNode = cAlphaArgs(9)
              CoilOutletNode = 'UNDEFINED'
            END IF
          END IF
          IF(HPWaterHeater(HPWaterHeaterNum)%OutletAirSplitterNode .GT. 0)THEN
            FanInletNode = 'UNDEFINED'
            FanOutletNode = cAlphaArgs(27)
          ELSE
            IF(HPWaterHeater(HPWaterHeaterNum)%OutsideAirNode .EQ. 0)THEN
              FanInletNode = 'UNDEFINED'
              FanOutletNode = cAlphaArgs(8)
            ELSE
              FanInletNode = 'UNDEFINED'
              FanOutletNode = cAlphaArgs(10)
            END IF
          END IF
        END IF

!       set up comp set for air side nodes (can be blow thru or draw thru, may or may not have damper nodes)
        CALL SetUpCompSets(HPWaterHeater(HPWaterHeaterNum)%Type, &
                           HPWaterHeater(HPWaterHeaterNum)%Name, &
                           HPWaterHeater(HPWaterHeaterNum)%DXCoilType, &
                           HPWaterHeater(HPWaterHeaterNum)%DXCoilName, &
                           CoilInletNode,CoilOutletNode)

        CALL SetUpCompSets(HPWaterHeater(HPWaterHeaterNum)%Type, &
                           HPWaterHeater(HPWaterHeaterNum)%Name, &
                           HPWaterHeater(HPWaterHeaterNum)%FanType, &
                           HPWaterHeater(HPWaterHeaterNum)%FanName, &
                           FanInletNode,FanOutletNode)

        IF (.not. lAlphaFieldBlanks(29)) THEN
          SELECT CASE (cAlphaArgs(29))
          CASE ('HEATER1')
            HPWaterHeater(HPWaterHeaterNum)%ControlSensorLocation = Heater1HPWHControl
          CASE ('HEATER2')
            HPWaterHeater(HPWaterHeaterNum)%ControlSensorLocation = Heater2HPWHControl
          CASE ('SOURCEINLET')
            HPWaterHeater(HPWaterHeaterNum)%ControlSensorLocation = SourceInletHPWHControl
          CASE ('SOURCEOUTLET')
            HPWaterHeater(HPWaterHeaterNum)%ControlSensorLocation = SourceOutletHPWHControl
          CASE ('USEINLET')
            HPWaterHeater(HPWaterHeaterNum)%ControlSensorLocation = UseInletHPWHControl
          CASE ('USEOUTLET')
            HPWaterHeater(HPWaterHeaterNum)%ControlSensorLocation = UseOutletHPWHControl
          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//'", invalid ')
            CALL ShowContinueError(trim(cAlphaFieldNames(29))//'="'//TRIM(cAlphaArgs(29))//'".')
            ErrorsFound=.TRUE.
          END SELECT

        ENDIF

      END DO ! DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in getting '//TRIM(cCurrentModuleObject)//  &
           ' input. Preceding condition causes termination.')
      END IF

    END IF !IF (NumHeatPumpWaterHeater > 0) THEN


!!!=======   Get WATER HEATER:MIXED ===================================================================================
    IF (NumWaterHeaterMixed > 0) THEN
      cCurrentModuleObject = cMixedWHModuleObj
      DO WaterThermalTankNum = 1, NumWaterHeaterMixed

        CALL GetObjectItem(cCurrentModuleObject,WaterThermalTankNum, &
          cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)


        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1),WaterThermalTank%Name,WaterThermalTankNum-1,IsNotOK,IsBlank,  &
           TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF
        WaterThermalTank(WaterThermalTankNum)%Name = cAlphaArgs(1)
        WaterThermalTank(WaterThermalTankNum)%Type = cCurrentModuleObject
        WaterThermalTank(WaterThermalTankNum)%TypeNum = MixedWaterHeater

        ! default to always on
        WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum = ScheduleAlwaysOn
        WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum = ScheduleAlwaysOn

          ! A user field will be added in a later release
        WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName = 'Water Heater'

        WaterThermalTank(WaterThermalTankNum)%Volume = rNumericArgs(1)
        IF (rNumericArgs(1) == 0.0d0) THEN
          ! Set volume to a really small number to simulate a tankless/instantaneous water heater
          WaterThermalTank(WaterThermalTankNum)%Volume = 0.000001d0 ! = 1 cm3
        END IF

        WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule = GetScheduleIndex(cAlphaArgs(2))
        IF (lAlphaFieldBlanks(2)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", missing data.')
          CALL ShowContinueError('blank field, missing '//TRIM(cAlphaFieldNames(2))//' is required')
          ErrorsFound = .TRUE.
        ELSEIF (WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule .EQ. 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  '//trim(cAlphaFieldNames(2))//' not found = '//TRIM(cAlphaArgs(2)))
          ErrorsFound = .TRUE.
        END IF

        IF (rNumericArgs(2) > 0.0001d0) THEN
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp = rNumericArgs(2)
        ELSE
          ! Default to very small number (however it can't be TINY or it will break the algorithm)
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp = 0.5d0
        END IF

        IF (rNumericArgs(3) > 0.0d0) THEN
          WaterThermalTank(WaterThermalTankNum)%TankTempLimit = rNumericArgs(3)
        ELSE
          ! Default to very large number
          ! BG comment why a large number here why not boilng point of water?
          WaterThermalTank(WaterThermalTankNum)%TankTempLimit = 100.0d0 !1.0E9
        END IF

        WaterThermalTank(WaterThermalTankNum)%MaxCapacity = rNumericArgs(4)

        IF ((rNumericArgs(5) > WaterThermalTank(WaterThermalTankNum)%MaxCapacity)   &
           .AND. (WaterThermalTank(WaterThermalTankNum)%MaxCapacity /= autosize)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Heater Minimum Capacity cannot be greater than Heater Maximum Capacity')
          ErrorsFound = .TRUE.
        ELSE
          WaterThermalTank(WaterThermalTankNum)%MinCapacity = rNumericArgs(5)
        END IF

        ! Validate Heater Control Type
        SELECT CASE (cAlphaArgs(3))
          CASE ('CYCLE')
            WaterThermalTank(WaterThermalTankNum)%ControlType = ControlTypeCycle
            WaterThermalTank(WaterThermalTankNum)%MinCapacity = WaterThermalTank(WaterThermalTankNum)%MaxCapacity

          CASE ('MODULATE')
            WaterThermalTank(WaterThermalTankNum)%ControlType = ControlTypeModulate

          !CASE ('MODULATE WITH OVERHEAT')  ! Not yet implemented

          !CASE ('MODULATE WITH UNDERHEAT')  ! Not yet implemented

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Control Type entered='//TRIM(cAlphaArgs(3)))
            ErrorsFound = .TRUE.
        END SELECT
        WaterThermalTank(WaterThermalTankNum)%VolFlowRateMin = rNumericArgs(6)
        WaterThermalTank(WaterThermalTankNum)%VolFlowRateMin = MAX(0.d0, WaterThermalTank(WaterThermalTankNum)%VolFlowRateMin)
!        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'GetWaterThermalTankInput')
!        WaterThermalTank(WaterThermalTankNum)%MassFlowRateMin = rNumericArgs(6) * rho   ! Not yet implemented
        WaterThermalTank(WaterThermalTankNum)%IgnitionDelay = rNumericArgs(7)  ! Not yet implemented

        ! Validate Heater Fuel Type
        SELECT CASE (cAlphaArgs(4))
          CASE ('ELECTRICITY','ELECTRIC','ELEC')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Electric'

          CASE ('GAS','NATURALGAS','NATURAL GAS')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Gas'

          CASE ('DIESEL')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Diesel'

          CASE ('GASOLINE')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Gasoline'

          CASE ('COAL')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Coal'

          CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'FuelOil#1'

          CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'FuelOil#2'

          CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Propane'

          CASE ('OTHERFUEL1')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'OtherFuel1'

          CASE ('OTHERFUEL2')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'OtherFuel2'

          CASE ('STEAM')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Steam'

          CASE ('DISTRICTHEATING')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'DistrictHeating'

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Heater Fuel Type entered='//TRIM(cAlphaArgs(4)))
            ! Set to Electric to avoid errors when setting up output variables
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Electric'
            ErrorsFound = .TRUE.
        END SELECT


        IF (rNumericArgs(8) > 0.0d0) THEN
          WaterThermalTank(WaterThermalTankNum)%Efficiency = rNumericArgs(8)
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))//  &
             ':  Heater Thermal Efficiency must be greater than zero')
          ErrorsFound = .TRUE.
        END IF

        IF (cAlphaArgs(5) /= Blank) THEN
          WaterThermalTank(WaterThermalTankNum)%PLFCurve = GetCurveIndex(cAlphaArgs(5))
          IF (WaterThermalTank(WaterThermalTankNum)%PLFCurve .EQ. 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Part Load Factor curve not found = '//TRIM(cAlphaArgs(5)))
            ErrorsFound = .TRUE.
          ELSE
            CALL ValidatePLFCurve(WaterThermalTank(WaterThermalTankNum)%PLFCurve, IsValid)

            IF (.NOT. IsValid) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Part Load Factor curve failed to evaluate to greater than zero for all numbers in the domain of 0 to 1')
              ErrorsFound = .TRUE.
            END IF
          END IF
        END IF

        WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad = rNumericArgs(9)

        ! Validate Off-Cycle Parasitic Fuel Type
        SELECT CASE (cAlphaArgs(6))
          CASE ('')  ! If blank, default to Fuel Type for heater
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = WaterThermalTank(WaterThermalTankNum)%FuelType

          CASE ('ELECTRICITY','ELECTRIC','ELEC')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Electric'

          CASE ('GAS','NATURALGAS','NATURAL GAS')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Gas'

          CASE ('DIESEL')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Diesel'

          CASE ('GASOLINE')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Gasoline'

          CASE ('COAL')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Coal'

          CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'FuelOil#1'

          CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'FuelOil#2'

          CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Propane'

          CASE ('OTHERFUEL1')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'OtherFuel1'

          CASE ('OTHERFUEL2')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'OtherFuel2'

          CASE ('STEAM')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Steam'

          CASE ('DISTRICTHEATING')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'DistrictHeating'

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Off-Cycle Parasitic Fuel Type entered='//TRIM(cAlphaArgs(6)))
            ! Set to Electric to avoid errors when setting up output variables
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Electric'
            ErrorsFound = .TRUE.
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank = rNumericArgs(10)


        WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad = rNumericArgs(11)

        ! Validate On-Cycle Parasitic Fuel Type
        SELECT CASE (cAlphaArgs(7))
          CASE ('')  ! If blank, default to Fuel Type for heater
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = WaterThermalTank(WaterThermalTankNum)%FuelType

          CASE ('ELECTRICITY','ELECTRIC','ELEC')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Electric'

          CASE ('GAS','NATURALGAS','NATURAL GAS')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Gas'

          CASE ('DIESEL')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Diesel'

          CASE ('GASOLINE')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Gasoline'

          CASE ('COAL')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Coal'

          CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'FuelOil#1'

          CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'FuelOil#2'

          CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Propane'

          CASE ('OTHERFUEL1')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'OtherFuel1'

          CASE ('OTHERFUEL2')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'OtherFuel2'

          CASE ('STEAM')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Steam'

          CASE ('DISTRICTHEATING')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'DistrictHeating'

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid On-Cycle Parasitic Fuel Type entered='//TRIM(cAlphaArgs(7)))
            ! Set to Electric to avoid errors when setting up output variables
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Electric'
            ErrorsFound = .TRUE.
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank = rNumericArgs(12)

        SELECT CASE (cAlphaArgs(8))
          CASE ('SCHEDULE')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempSchedule
            WaterThermalTank(WaterThermalTankNum)%AmbientTempSchedule = GetScheduleIndex(cAlphaArgs(9))
            IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempSchedule .EQ. 0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Ambient Temperature Schedule not found = '//TRIM(cAlphaArgs(9)))
              ErrorsFound = .TRUE.
            END IF

          CASE ('ZONE')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempZone
            WaterThermalTank(WaterThermalTankNum)%AmbientTempZone = FindItemInList(cAlphaArgs(10),Zone%Name,NumOfZones)
            IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempZone .EQ. 0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Ambient Temperature Zone not found = '//TRIM(cAlphaArgs(10)))
              ErrorsFound = .TRUE.
            END IF

          CASE ('OUTDOORS')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempOutsideAir
            WaterThermalTank(WaterThermalTankNum)%AmbientTempOutsideAirNode = GetOnlySingleNode(cAlphaArgs(11), ErrorsFound, &
              TRIM(cCurrentModuleObject), cAlphaArgs(1), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent)
            IF (cAlphaArgs(11) /= ' ') THEN
              IF (.not. CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum)%AmbientTempOutsideAirNode)) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                   ': Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node')
                CALL ShowContinueError('...Referenced Node Name='//TRIM(cAlphaArgs(11)))
                ErrorsFound=.true.
              ENDIF
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('An Ambient Outdoor Air Node name must be used when' // &
                               ' the Ambient Temperature Indicator is Outdoors.')
              ErrorsFound = .TRUE.
            ENDIF

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Ambient Temperature Indicator entered='//TRIM(cAlphaArgs(8)))
            CALL ShowContinueError(' Valid entries are SCHEDULE, ZONE, and OUTDOORS.')
            ErrorsFound = .TRUE.
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%OffCycLossCoeff = rNumericArgs(13)
        WaterThermalTank(WaterThermalTankNum)%OffCycLossFracToZone = rNumericArgs(14)

        WaterThermalTank(WaterThermalTankNum)%OnCycLossCoeff = rNumericArgs(15)
        WaterThermalTank(WaterThermalTankNum)%OnCycLossFracToZone = rNumericArgs(16)
        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'GetWaterThermalTankInput')
        WaterThermalTank(WaterThermalTankNum)%MassFlowRateMax = rNumericArgs(17) * rho

        IF ((cAlphaArgs(14) == Blank) .AND. (cAlphaArgs(15) == Blank)) THEN
          IF (cAlphaArgs(12) /= Blank) THEN
            WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule = GetScheduleIndex(cAlphaArgs(12))
            IF (WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule .EQ. 0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Flow Rate Schedule not found = '//TRIM(cAlphaArgs(12)))
              ErrorsFound = .TRUE.
            END IF
          END IF
        END IF

        IF (cAlphaArgs(13) /= Blank) THEN
          WaterThermalTank(WaterThermalTankNum)%UseInletTempSchedule = GetScheduleIndex(cAlphaArgs(13))
          IF (WaterThermalTank(WaterThermalTankNum)%UseInletTempSchedule .EQ. 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Cold Water Supply Temperature Schedule not found = '//TRIM(cAlphaArgs(13)))
            ErrorsFound = .TRUE.
          END IF
        END IF

        IF (NumNums > 17) THEN
          IF ((rNumericArgs(18) > 1) .OR. (rNumericArgs(18) < 0)) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Use Side Effectiveness is out of bounds (0 to 1)')
            ErrorsFound = .TRUE.
          END IF
          WaterThermalTank(WaterThermalTankNum)%UseEffectiveness = rNumericArgs(18)
        ELSE
          WaterThermalTank(WaterThermalTankNum)%UseEffectiveness = 1.0d0 ! Default for stand-alone mode
        END IF

        IF ((rNumericArgs(19) > 1) .OR. (rNumericArgs(19) < 0)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Source Side Effectiveness is out of bounds (0 to 1)')
          ErrorsFound = .TRUE.
        END IF
        WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness = rNumericArgs(19)

        ! If no plant nodes are connected, simulate in stand-alone mode.
        IF (cAlphaArgs(14) == Blank .AND. cAlphaArgs(15) == Blank .AND. cAlphaArgs(16) == Blank .AND. cAlphaArgs(17) == Blank) THEN
          WaterThermalTank(WaterThermalTankNum)%StandAlone = .TRUE.
        ENDIF

        IF (.NOT. lNumericFieldBlanks(20)) THEN
          WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    = rNumericArgs(20)
        ELSE
          WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    = 0.d0
        ENDIF
        WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide = DemandSupply_No

        IF (.NOT. lNumericFieldBlanks(21)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = rNumericArgs(21)
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = 0.d0
        END IF
        WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide = DemandSupply_No

        If (.NOT. lNumericFieldBlanks(22)) then
          WaterThermalTank(WaterThermalTankNum)%SizingRecoveryTime      = rNumericArgs(22)
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SizingRecoveryTime      = 1.5d0
        END IF

        IF ((cAlphaArgs(14) /= Blank) .OR. (cAlphaArgs(15) /= Blank)) THEN
          WaterThermalTank(WaterThermalTankNum)%UseInletNode = &
            GetOnlySingleNode(cAlphaArgs(14),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%InletNodeName1 = cAlphaArgs(14)
          WaterThermalTank(WaterThermalTankNum)%UseOutletNode = &
            GetOnlySingleNode(cAlphaArgs(15),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName1 = cAlphaArgs(15)

          IF (rNumericArgs(17) > 0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used')
          END IF

          IF (WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule > 0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used')
          END IF

          IF (WaterThermalTank(WaterThermalTankNum)%UseInletTempSchedule > 0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used')
          END IF
        END IF

        IF ((cAlphaArgs(16) /= Blank) .OR. (cAlphaArgs(17) /= Blank)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceInletNode = &
            GetOnlySingleNode(cAlphaArgs(16),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%InletNodeName2 = cAlphaArgs(16)
          WaterThermalTank(WaterThermalTankNum)%SourceOutletNode = &
            GetOnlySingleNode(cAlphaArgs(17),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName2 = cAlphaArgs(17)

        END IF

        IF (.NOT. lAlphaFieldBlanks(18)) THEN
          SELECT CASE (cAlphaArgs(18))
          CASE ('STORAGETANK' )
            WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode = SourceSideStorageTank
          CASE ('INDIRECTHEATPRIMARYSETPOINT')
            WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint
          CASE ('INDIRECTHEATALTERNATESETPOINT' )
            WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode = SourceSideIndirectHeatAltSetpoint
          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Control Mode entered='//TRIM(cAlphaArgs(18)))
            ErrorsFound = .TRUE.
          END SELECT
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint
        ENDIF

        IF (.NOT. lAlphaFieldBlanks(19)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceSideAltSetpointSchedNum = GetScheduleIndex(cAlphaArgs(19))
          IF (WaterThermalTank(WaterThermalTankNum)%SourceSideAltSetpointSchedNum == 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  '//trim(cAlphaFieldNames(19))//' not found = '//TRIM(cAlphaArgs(19)))
            ErrorsFound = .TRUE.
          ENDIF
        ENDIF


      END DO ! WaterThermalTankNum

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in getting '//TRIM(cCurrentModuleObject)//  &
           ' input. Preceding condition causes termination.')
      END IF

    END IF


!!!=======   Get WATER HEATER:STRATIFIED ==============================================================================
    IF (NumWaterHeaterStratified > 0) THEN
      cCurrentModuleObject = cStratifiedWHModuleObj !'WaterHeater:Stratified'

      DO WaterThermalTankNum = NumWaterHeaterMixed + 1, NumWaterHeaterMixed + NumWaterHeaterStratified

        CALL GetObjectItem(cCurrentModuleObject,WaterThermalTankNum-NumWaterHeaterMixed,  &
          cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1),WaterThermalTank%Name,WaterThermalTankNum-NumWaterHeaterMixed-1,IsNotOK,IsBlank,  &
                      TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF
        WaterThermalTank(WaterThermalTankNum)%Name = cAlphaArgs(1)
        WaterThermalTank(WaterThermalTankNum)%Type = cCurrentModuleObject
        WaterThermalTank(WaterThermalTankNum)%TypeNum = StratifiedWaterHeater

        ! default to always on
        WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum = ScheduleAlwaysOn
        WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum = ScheduleAlwaysOn

        WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName = cAlphaArgs(2)

        WaterThermalTank(WaterThermalTankNum)%Volume = rNumericArgs(1)
        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'GetWaterThermalTankInput')
        WaterThermalTank(WaterThermalTankNum)%Mass = WaterThermalTank(WaterThermalTankNum)%Volume * rho
        WaterThermalTank(WaterThermalTankNum)%Height = rNumericArgs(2)

        SELECT CASE (cAlphaArgs(3))
          CASE ('VERTICALCYLINDER')
            WaterThermalTank(WaterThermalTankNum)%Shape = TankShapeVertCylinder

          CASE ('HORIZONTALCYLINDER')
            WaterThermalTank(WaterThermalTankNum)%Shape = TankShapeHorizCylinder

          CASE ('OTHER')
            WaterThermalTank(WaterThermalTankNum)%Shape = TankShapeOther
            IF (rNumericArgs(3) > 0.0d0) THEN
              WaterThermalTank(WaterThermalTankNum)%Perimeter = rNumericArgs(3)
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Tank Perimeter must be greater than zero for Tank Shape=OTHER')
              ErrorsFound = .TRUE.
            END IF

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Tank Shape entered='//TRIM(cAlphaArgs(3)))
            WaterThermalTank(WaterThermalTankNum)%Shape = TankShapeVertCylinder
            ErrorsFound = .TRUE.
        END SELECT

        IF (rNumericArgs(4) > 0.0d0) THEN
          WaterThermalTank(WaterThermalTankNum)%TankTempLimit = rNumericArgs(4)
        ELSE
          ! Default to very large number
          WaterThermalTank(WaterThermalTankNum)%TankTempLimit = 1.0d9
        END IF

        ! Validate Heater Priority Control
        SELECT CASE (cAlphaArgs(4))
          CASE ('MASTERSLAVE')
            WaterThermalTank(WaterThermalTankNum)%ControlType = PriorityMasterSlave

          CASE ('SIMULTANEOUS')
            WaterThermalTank(WaterThermalTankNum)%ControlType = PrioritySimultaneous

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Heater Priority Control entered='//TRIM(cAlphaArgs(4)))
            ErrorsFound = .TRUE.
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule = GetScheduleIndex(cAlphaArgs(5))
        IF (lAlphaFieldBlanks(5)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", missing data.')
          CALL ShowContinueError('blank field, missing '//TRIM(cAlphaFieldNames(5))//' is required')
          ErrorsFound = .TRUE.
        ELSEIF (WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule .EQ. 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': '//trim(cAlphaFieldNames(5))//' not found = '//TRIM(cAlphaArgs(5)))
          ErrorsFound = .TRUE.
        END IF

        IF (rNumericArgs(5) > 0.0d0) THEN
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp = rNumericArgs(5)
        ELSE
          ! Default to very small number (however it can't be TINY or it will break the algorithm)
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp = 0.0001d0
        END IF

        WaterThermalTank(WaterThermalTankNum)%MaxCapacity = rNumericArgs(6)
        WaterThermalTank(WaterThermalTankNum)%HeaterHeight1 = rNumericArgs(7)

        !Test if Heater height is within range
        IF ((WaterThermalTank(WaterThermalTankNum)%Height /= Autosize) .AND. &
            (WaterThermalTank(WaterThermalTankNum)%HeaterHeight1 &
                >  WaterThermalTank(WaterThermalTankNum)%Height)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Heater 1 is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(7))//' = '//TRIM(RoundSigDigits(rNumericArgs(7), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule2 = GetScheduleIndex(cAlphaArgs(6))
        IF (lAlphaFieldBlanks(6)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", missing data.')
          CALL ShowContinueError('blank field, missing '//TRIM(cAlphaFieldNames(6))//' is required')
          ErrorsFound = .TRUE.
        ELSEIF (WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule2 .EQ. 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  '//trim(cAlphaFieldNames(6))//' not found = '//TRIM(cAlphaArgs(6)))
          ErrorsFound = .TRUE.
        END IF

        IF (rNumericArgs(5) > 0.0d0) THEN
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp2 = rNumericArgs(8)
        ELSE
          ! Default to very small number (however it can't be TINY or it will break the algorithm)
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp2 = 0.0001d0
        END IF

        WaterThermalTank(WaterThermalTankNum)%MaxCapacity2 = rNumericArgs(9)
        WaterThermalTank(WaterThermalTankNum)%HeaterHeight2 = rNumericArgs(10)

        !Test if Heater height is within range
        IF ((WaterThermalTank(WaterThermalTankNum)%Height /= Autosize) .AND. &
            (WaterThermalTank(WaterThermalTankNum)%HeaterHeight2 &
                 >  WaterThermalTank(WaterThermalTankNum)%Height)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Heater 2 is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(10))//' = '//TRIM(RoundSigDigits(rNumericArgs(10), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        ! Validate Heater Fuel Type
        SELECT CASE (cAlphaArgs(7))
          CASE ('ELECTRICITY','ELECTRIC','ELEC')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Electric'

          CASE ('GAS','NATURALGAS','NATURAL GAS')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Gas'

          CASE ('DIESEL')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Diesel'

          CASE ('GASOLINE')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Gasoline'

          CASE ('COAL')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Coal'

          CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'FuelOil#1'

          CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'FuelOil#2'

          CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Propane'

          CASE ('OTHERFUEL1')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'OtherFuel1'

          CASE ('OTHERFUEL2')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'OtherFuel2'

          CASE ('STEAM')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Steam'

          CASE ('DISTRICTHEATING')
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'DistrictHeating'

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Heater Fuel Type entered='//TRIM(cAlphaArgs(7)))
            ! Set to Electric to avoid errors when setting up output variables
            WaterThermalTank(WaterThermalTankNum)%FuelType = 'Electric'
            ErrorsFound = .TRUE.
        END SELECT

        IF (rNumericArgs(11) > 0.0d0) THEN
          WaterThermalTank(WaterThermalTankNum)%Efficiency = rNumericArgs(11)
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))//  &
             ':  Heater Thermal Efficiency must be greater than zero')
          ErrorsFound = .TRUE.
        END IF

        WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad = rNumericArgs(12)

        ! Validate Off-Cycle Parasitic Fuel Type
        SELECT CASE (cAlphaArgs(8))
          CASE ('')  ! If blank, default to Fuel Type for heater
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = WaterThermalTank(WaterThermalTankNum)%FuelType

          CASE ('ELECTRICITY','ELECTRIC','ELEC')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Electric'

          CASE ('GAS','NATURALGAS','NATURAL GAS')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Gas'

          CASE ('DIESEL')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Diesel'

          CASE ('GASOLINE')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Gasoline'

          CASE ('COAL')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Coal'

          CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'FuelOil#1'

          CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'FuelOil#2'

          CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Propane'

          CASE ('OTHERFUEL1')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'OtherFuel1'

          CASE ('OTHERFUEL2')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'OtherFuel2'

          CASE ('STEAM')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Steam'

          CASE ('DISTRICTHEATING')
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'DistrictHeating'

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Off-Cycle Parasitic Fuel Type entered='//TRIM(cAlphaArgs(8)))
            ! Set to Electric to avoid errors when setting up output variables
            WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Electric'
            ErrorsFound = .TRUE.
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank = rNumericArgs(13)
        WaterThermalTank(WaterThermalTankNum)%OffCycParaHeight = rNumericArgs(14)

        WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad = rNumericArgs(15)

        ! Validate On-Cycle Parasitic Fuel Type
        SELECT CASE (cAlphaArgs(9))
          CASE ('')  ! If blank, default to Fuel Type for heater
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = WaterThermalTank(WaterThermalTankNum)%FuelType

          CASE ('ELECTRICITY','ELECTRIC','ELEC')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Electric'

          CASE ('GAS','NATURALGAS','NATURAL GAS')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Gas'

          CASE ('DIESEL')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Diesel'

          CASE ('GASOLINE')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Gasoline'

          CASE ('COAL')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Coal'

          CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'FuelOil#1'

          CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'FuelOil#2'

          CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Propane'

          CASE ('OTHERFUEL1')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'OtherFuel1'

          CASE ('OTHERFUEL2')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'OtherFuel2'

          CASE ('STEAM')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Steam'

          CASE ('DISTRICTHEATING')
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'DistrictHeating'

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid On-Cycle Parasitic Fuel Type entered='//TRIM(cAlphaArgs(9)))
            ! Set to Electric to avoid errors when setting up output variables
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Electric'
            ErrorsFound = .TRUE.
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank = rNumericArgs(16)
        WaterThermalTank(WaterThermalTankNum)%OnCycParaHeight = rNumericArgs(17)


        SELECT CASE (cAlphaArgs(10))
          CASE ('SCHEDULE')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempSchedule
            WaterThermalTank(WaterThermalTankNum)%AmbientTempSchedule = GetScheduleIndex(cAlphaArgs(11))
            IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempSchedule .EQ. 0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Ambient Temperature Schedule not found = '//TRIM(cAlphaArgs(11)))
              ErrorsFound = .TRUE.
            END IF

          CASE ('ZONE')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempZone
            WaterThermalTank(WaterThermalTankNum)%AmbientTempZone = FindItemInList(cAlphaArgs(12),Zone%Name,NumOfZones)
            IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempZone .EQ. 0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Ambient Temperature Zone not found = '//TRIM(cAlphaArgs(12)))
              ErrorsFound = .TRUE.
            END IF

          CASE ('OUTDOORS')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempOutsideAir
            WaterThermalTank(WaterThermalTankNum)%AmbientTempOutsideAirNode = GetOnlySingleNode(cAlphaArgs(13), ErrorsFound, &
              TRIM(cCurrentModuleObject), cAlphaArgs(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent)
            IF (cAlphaArgs(13) /= ' ') THEN
              IF (.not. CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum)%AmbientTempOutsideAirNode)) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                   ': Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node')
                CALL ShowContinueError('...Referenced Node Name='//TRIM(cAlphaArgs(13)))
                ErrorsFound=.true.
              ENDIF
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('An Ambient Outdoor Air Node name must be used when' // &
                               ' the Ambient Temperature Indicator is Outdoors.')
              ErrorsFound = .TRUE.
            ENDIF

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Ambient Temperature Indicator entered='//TRIM(cAlphaArgs(10)))
            CALL ShowContinueError(' Valid entries are Schedule, Zone, and Outdoors.')
            ErrorsFound = .TRUE.
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%SkinLossCoeff = rNumericArgs(18)
        WaterThermalTank(WaterThermalTankNum)%SkinLossFracToZone = rNumericArgs(19)
        WaterThermalTank(WaterThermalTankNum)%OffCycFlueLossCoeff = rNumericArgs(20)
        WaterThermalTank(WaterThermalTankNum)%OffCycFlueLossFracToZone = rNumericArgs(21)

        !this is temporary until we know fluid type
        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'GetWaterThermalTankInput')
        WaterThermalTank(WaterThermalTankNum)%MassFlowRateMax = rNumericArgs(22) * rho

        IF ((cAlphaArgs(16) == Blank) .AND. (cAlphaArgs(17) == Blank)) THEN
          IF (cAlphaArgs(14) /= Blank) THEN
            WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule = GetScheduleIndex(cAlphaArgs(14))
            IF (WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule .EQ. 0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Flow Rate Schedule not found = '//TRIM(cAlphaArgs(14)))
              ErrorsFound = .TRUE.
            END IF
          END IF
        END IF

        IF (cAlphaArgs(15) /= Blank) THEN
          WaterThermalTank(WaterThermalTankNum)%UseInletTempSchedule = GetScheduleIndex(cAlphaArgs(15))
          IF (WaterThermalTank(WaterThermalTankNum)%UseInletTempSchedule .EQ. 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Cold Water Supply Temperature Schedule not found = '//TRIM(cAlphaArgs(15)))
            ErrorsFound = .TRUE.
          END IF
        END IF

        IF (NumNums > 22) THEN
          WaterThermalTank(WaterThermalTankNum)%UseEffectiveness = rNumericArgs(23)
        ELSE
          WaterThermalTank(WaterThermalTankNum)%UseEffectiveness = 1.0d0  ! Default for stand-alone mode
        END IF

        IF (NumNums > 23) THEN
          WaterThermalTank(WaterThermalTankNum)%UseInletHeight = rNumericArgs(24)
        ELSE
         ! Defaults to bottom of tank
          WaterThermalTank(WaterThermalTankNum)%UseInletHeight = 0.0d0
        END IF
        IF ((WaterThermalTank(WaterThermalTankNum)%Height /= Autosize) .AND. &
            (WaterThermalTank(WaterThermalTankNum)%UseInletHeight &
                   > WaterThermalTank(WaterThermalTankNum)%Height)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Use inlet is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(24))//' = '//TRIM(RoundSigDigits(rNumericArgs(24), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        IF ((NumNums > 24) .AND. (rNumericArgs(25) /= Autocalculate)) THEN
          WaterThermalTank(WaterThermalTankNum)%UseOutletHeight = rNumericArgs(25)
        ELSE
          ! Defaults to top of tank
          WaterThermalTank(WaterThermalTankNum)%UseOutletHeight = WaterThermalTank(WaterThermalTankNum)%Height
        END IF
        IF ((WaterThermalTank(WaterThermalTankNum)%Height /= Autosize) .AND. &
            (WaterThermalTank(WaterThermalTankNum)%UseOutletHeight &
              > WaterThermalTank(WaterThermalTankNum)%Height)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Use outlet is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(25))//' = '//TRIM(RoundSigDigits(rNumericArgs(25), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        IF (NumNums > 25) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness = rNumericArgs(26)
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness = 1.0D0
        END IF

        IF ((NumNums > 26) .AND. (rNumericArgs(27) /= Autocalculate)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceInletHeight = rNumericArgs(27)
        ELSE
          ! Defaults to top of tank
          WaterThermalTank(WaterThermalTankNum)%SourceInletHeight = WaterThermalTank(WaterThermalTankNum)%Height
        END IF
        IF ((WaterThermalTank(WaterThermalTankNum)%Height /= Autosize) .AND. &
            (WaterThermalTank(WaterThermalTankNum)%SourceInletHeight &
              > WaterThermalTank(WaterThermalTankNum)%Height)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Source inlet is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(27))//' = '//TRIM(RoundSigDigits(rNumericArgs(27), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        IF ((NumNums > 27) .AND. (rNumericArgs(28) /= Autocalculate)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceOutletHeight = rNumericArgs(28)
        ELSE
          ! Defaults to bottom of tank
          WaterThermalTank(WaterThermalTankNum)%SourceOutletHeight = 0.0D0
        END IF
        IF ((WaterThermalTank(WaterThermalTankNum)%Height /= Autosize) .AND. &
            (WaterThermalTank(WaterThermalTankNum)%SourceOutletHeight &
              > WaterThermalTank(WaterThermalTankNum)%Height)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Source outlet is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(28))//' = '//TRIM(RoundSigDigits(rNumericArgs(28), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        ! If no plant nodes are connected, simulate in stand-alone mode.
        IF (cAlphaArgs(16) == Blank .AND. cAlphaArgs(17) == Blank .AND. cAlphaArgs(18) == Blank .AND. cAlphaArgs(19) == Blank) &
          WaterThermalTank(WaterThermalTankNum)%StandAlone = .TRUE.

        IF (.NOT. lNumericFieldBlanks(29)) THEN
          WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    = rNumericArgs(29)
        ELSE
          WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    = 0.d0
        END IF

        WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide = DemandSupply_No

        IF (.NOT.  lNumericFieldBlanks(30) ) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = rNumericArgs(30)
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = 0.d0
        END IF

        If (NumNums > 30) then
          WaterThermalTank(WaterThermalTankNum)%SizingRecoveryTime      = rNumericArgs(31)
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SizingRecoveryTime      = 1.5d0
        END IF

          WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide = DemandSupply_No

        IF ((cAlphaArgs(16) /= Blank) .OR. (cAlphaArgs(17) /= Blank)) THEN
          WaterThermalTank(WaterThermalTankNum)%UseInletNode = &
            GetOnlySingleNode(cAlphaArgs(16),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%InletNodeName1 = cAlphaArgs(16)
          WaterThermalTank(WaterThermalTankNum)%UseOutletNode = &
            GetOnlySingleNode(cAlphaArgs(17),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName1 = cAlphaArgs(17)

          IF (rNumericArgs(22) > 0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Use side nodes are specified; Peak Volumetric Use Flow Rate will not be used')
          END IF

          IF (WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule > 0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Use side nodes are specified; Use Flow Rate Fraction Schedule will not be used')
          END IF

          IF (WaterThermalTank(WaterThermalTankNum)%UseInletTempSchedule > 0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Use side nodes are specified; Cold Water Supply Temperature Schedule will not be used')
          END IF
        END IF

        IF ((cAlphaArgs(18) /= Blank) .OR. (cAlphaArgs(19) /= Blank)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceInletNode = &
            GetOnlySingleNode(cAlphaArgs(18),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%InletNodeName2 = cAlphaArgs(18)
          WaterThermalTank(WaterThermalTankNum)%SourceOutletNode = &
            GetOnlySingleNode(cAlphaArgs(19),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName2 = cAlphaArgs(19)

        END IF

        ! Validate inlet mode
        SELECT CASE (cAlphaArgs(20))
          CASE ('FIXED')
            WaterThermalTank(WaterThermalTankNum)%InletMode = InletModeFixed

          CASE ('SEEKING')
            WaterThermalTank(WaterThermalTankNum)%InletMode = InletModeSeeking
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%Nodes = rNumericArgs(32)
        WaterThermalTank(WaterThermalTankNum)%AdditionalCond = rNumericArgs(33)

        ALLOCATE(WaterThermalTank(WaterThermalTankNum)%AdditionalLossCoeff(WaterThermalTank(WaterThermalTankNum)%Nodes))
        WaterThermalTank(WaterThermalTankNum)%AdditionalLossCoeff = 0.0d0
        DO NodeNum = 1, WaterThermalTank(WaterThermalTankNum)%Nodes
          IF (NumNums > 32 + NodeNum) THEN
            WaterThermalTank(WaterThermalTankNum)%AdditionalLossCoeff(NodeNum) = rNumericArgs(33 + NodeNum)
          ELSE
            EXIT
          END IF
        END DO

        IF (NumNums > 33 + WaterThermalTank(WaterThermalTankNum)%Nodes) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used')
        END IF

        CALL SetupStratifiedNodes(WaterThermalTankNum)

        IF (.NOT. lAlphaFieldBlanks(21)) THEN
          SELECT CASE (cAlphaArgs(21))
          CASE ('STORAGETANK' )
            WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode = SourceSideStorageTank
          CASE ('INDIRECTHEATPRIMARYSETPOINT')
            WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint
          CASE ('INDIRECTHEATALTERNATESETPOINT' )
            WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode = SourceSideIndirectHeatAltSetpoint
          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Control Mode entered='//TRIM(cAlphaArgs(21)))
            ErrorsFound = .TRUE.
          END SELECT
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode = SourceSideIndirectHeatPrimarySetpoint
        ENDIF

        IF (.NOT. lAlphaFieldBlanks(22)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceSideAltSetpointSchedNum = GetScheduleIndex(cAlphaArgs(22))
          IF (WaterThermalTank(WaterThermalTankNum)%SourceSideAltSetpointSchedNum == 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  '//trim(cAlphaFieldNames(22))//' not found = '//TRIM(cAlphaArgs(22)))
            ErrorsFound = .TRUE.
          ENDIF
        ENDIF


      END DO ! WaterThermalTankNum

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in getting '//TRIM(cCurrentModuleObject)//  &
           ' input. Preceding condition causes termination.')
      END IF

    END IF

!!!=======   Get Chilled Water :MIXED ===================================================================================
    IF (NumChilledWaterMixed > 0) THEN
      cCurrentModuleObject = cMixedCWTankModuleObj  ! 'ThermalStorage:ChilledWater:Mixed'
      DO WaterThermalTankNum = NumWaterHeaterMixed + NumWaterHeaterStratified + 1, &
                                   NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed

        CALL GetObjectItem(cCurrentModuleObject,WaterThermalTankNum - (NumWaterHeaterMixed + NumWaterHeaterStratified), &
                           cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1),WaterThermalTank%Name,WaterThermalTankNum-1,&
                               IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF
        WaterThermalTank(WaterThermalTankNum)%Name = cAlphaArgs(1)
        WaterThermalTank(WaterThermalTankNum)%Type = cCurrentModuleObject
        WaterThermalTank(WaterThermalTankNum)%TypeNum = MixedChilledWaterStorage
        WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank = .TRUE.
        WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName = 'Chilled Water Storage'

        WaterThermalTank(WaterThermalTankNum)%Volume = rNumericArgs(1)
        IF (rNumericArgs(1) == 0.0d0) THEN
          ! Set volume to a really small number to continue simulation
          WaterThermalTank(WaterThermalTankNum)%Volume = 0.000001d0 ! = 1 cm3
        END IF

        WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule = GetScheduleIndex(cAlphaArgs(2))
        IF (WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule .EQ. 0) THEN
          CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))

          ErrorsFound = .TRUE.
        END IF

        IF (rNumericArgs(2) > 0.0001d0) THEN
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp = rNumericArgs(2)
        ELSE
          ! Default to very small number (however it can't be TINY or it will break the algorithm)
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp = 0.5d0
        END IF

        IF (rNumericArgs(3) > 0.0d0) THEN
          WaterThermalTank(WaterThermalTankNum)%TankTempLimit = rNumericArgs(3)
        ELSE
          ! default to just above freezing
          WaterThermalTank(WaterThermalTankNum)%TankTempLimit = 1.0D0
        END IF

        WaterThermalTank(WaterThermalTankNum)%MaxCapacity = rNumericArgs(4)
        WaterThermalTank(WaterThermalTankNum)%MinCapacity = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%ControlType = ControlTypeCycle

        WaterThermalTank(WaterThermalTankNum)%MassFlowRateMin = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%IgnitionDelay   = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%FuelType        = 'Electric'
        WaterThermalTank(WaterThermalTankNum)%Efficiency      = 1.0D0
        WaterThermalTank(WaterThermalTankNum)%PLFCurve        = 0
        WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad  = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Electric'
        WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad   = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Electric'
        WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank = 0.0D0

        SELECT CASE (cAlphaArgs(3))
          CASE ('SCHEDULE')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempSchedule
            WaterThermalTank(WaterThermalTankNum)%AmbientTempSchedule = GetScheduleIndex(cAlphaArgs(4))
            IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempSchedule .EQ. 0) THEN
              CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(cAlphaArgs(4)))
              CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Schedule was not found.')
              ErrorsFound = .TRUE.
            END IF

          CASE ('ZONE')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempZone
            WaterThermalTank(WaterThermalTankNum)%AmbientTempZone = FindItemInList(cAlphaArgs(5),Zone%Name,NumOfZones)
            IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempZone .EQ. 0) THEN
              CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5)))
              CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Zone was not found.')
              ErrorsFound = .TRUE.
            END IF

          CASE ('OUTDOORS')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempOutsideAir
            WaterThermalTank(WaterThermalTankNum)%AmbientTempOutsideAirNode = GetOnlySingleNode(cAlphaArgs(6), ErrorsFound, &
              TRIM(cCurrentModuleObject), cAlphaArgs(1), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent)
            IF (.NOT. lAlphaFieldBlanks(6) ) THEN
              IF (.not. CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum)%AmbientTempOutsideAirNode)) THEN
                CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6)))
                CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
                CALL ShowContinueError('Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node')
                ErrorsFound=.true.
              ENDIF
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('An Ambient Outdoor Air Node name must be used when' // &
                               ' the Ambient Temperature Indicator is Outdoors.')
              ErrorsFound = .TRUE.
            ENDIF

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Ambient Temperature Indicator entered='//TRIM(cAlphaArgs(3)))
            CALL ShowContinueError(' Valid entries are Schedule, Zone, and Outdoors.')
            ErrorsFound = .TRUE.
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%OffCycLossCoeff = rNumericArgs(5)
        WaterThermalTank(WaterThermalTankNum)%OffCycLossFracToZone = 1.0D0

        WaterThermalTank(WaterThermalTankNum)%OnCycLossCoeff = rNumericArgs(5)
        WaterThermalTank(WaterThermalTankNum)%OnCycLossFracToZone =1.0D0

        WaterThermalTank(WaterThermalTankNum)%MassFlowRateMax = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule = 0
        WaterThermalTank(WaterThermalTankNum)%UseInletTempSchedule = 0

        ! default to always on
        WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum = ScheduleAlwaysOn
        WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum = ScheduleAlwaysOn

        IF ((rNumericArgs(6) > 1) .OR. (rNumericArgs(6) < 0)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Use Side Effectiveness is out of bounds (0 to 1)')
          ErrorsFound = .TRUE.
        END IF
        WaterThermalTank(WaterThermalTankNum)%UseEffectiveness = rNumericArgs(6)


        IF ((rNumericArgs(8) > 1) .OR. (rNumericArgs(8) < 0)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Source Side Effectiveness is out of bounds (0 to 1)')
          ErrorsFound = .TRUE.
        END IF
        WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness = rNumericArgs(8)

        IF (lNumericFieldBlanks(7)) THEN
          WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    = 0.d0
        ELSE
          WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    = rNumericArgs(7)
        ENDIF

        WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide = DemandSupply_No

        IF (lAlphaFieldBlanks(9)) THEN
          WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum = ScheduleAlwaysOn
        ELSE
          WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum = GetScheduleIndex(cAlphaArgs(9))
          IF (WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum == 0) Then
            CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(9))//' = '//TRIM(cAlphaArgs(9)))
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Schedule was not found.')
            ErrorsFound = .TRUE.
          ENDIF
        ENDIF

        WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide = DemandSupply_No

        IF (lNumericFieldBlanks(9)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = 0.d0
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = rNumericArgs(9)
        ENDIF

        IF (lAlphaFieldBlanks(12)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum = ScheduleAlwaysOn
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum = GetScheduleIndex(cAlphaArgs(12))
          IF (WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum == 0) Then
            CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(12))//' = '//TRIM(cAlphaArgs(12)))
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Schedule was not found.')
            ErrorsFound = .TRUE.
          ENDIF
        ENDIF
        IF (lNumericFieldBlanks(10)) THEN
          WaterThermalTank(WaterThermalTankNum)%SizingRecoveryTime   = 4.0D0
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SizingRecoveryTime   =  rNumericArgs(10)
        ENDIF

        IF ((.NOT. lAlphaFieldBlanks(7) ) .OR. (.NOT. lAlphaFieldBlanks(8) )) THEN
          WaterThermalTank(WaterThermalTankNum)%UseInletNode = &
            GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%InletNodeName1 = cAlphaArgs(7)
          WaterThermalTank(WaterThermalTankNum)%UseOutletNode = &
            GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName1 = cAlphaArgs(8)

        END IF

        IF ((.NOT. lAlphaFieldBlanks(10) ) .OR. (.NOT. lAlphaFieldBlanks(11))) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceInletNode = &
            GetOnlySingleNode(cAlphaArgs(10),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%InletNodeName2 = cAlphaArgs(10)
          WaterThermalTank(WaterThermalTankNum)%SourceOutletNode = &
            GetOnlySingleNode(cAlphaArgs(11),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName2 = cAlphaArgs(11)

        END IF

        IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide == DemandSide   &
            .and. WaterThermalTank(WaterThermalTankNum)%SourceInletNode /= 0) THEN
          CALL RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum)%SourceInletNode,  &
                                    WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate)
        ENDIF

      END DO ! WaterThermalTankNum

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in getting '//TRIM(cCurrentModuleObject)//  &
           ' input. Preceding condition causes termination.')
      END IF

    END IF

!! end chilled water mixed storage

!!!=======   Get 'ThermalStorage:ChilledWater:Stratified' =======================================================
    IF (NumChilledWaterStratified > 0) THEN
      cCurrentModuleObject = cStratifiedCWTankModuleObj ! 'ThermalStorage:ChilledWater:Stratified'

      DO WaterThermalTankNum = NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + 1 &
                              , NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed + NumChilledWaterStratified

        CALL GetObjectItem(cCurrentModuleObject,WaterThermalTankNum  &
                            -  (NumWaterHeaterMixed + NumWaterHeaterStratified + NumChilledWaterMixed),  &
                              cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK = .FALSE.
        IsBlank = .FALSE.
        CALL VerifyName(cAlphaArgs(1),WaterThermalTank%Name,WaterThermalTankNum-1,IsNotOK,IsBlank,  &
                      TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound = .TRUE.
          IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
        END IF
        WaterThermalTank(WaterThermalTankNum)%Name = cAlphaArgs(1)
        WaterThermalTank(WaterThermalTankNum)%Type = cCurrentModuleObject
        WaterThermalTank(WaterThermalTankNum)%TypeNum = StratifiedChilledWaterStorage
        WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank = .TRUE.
        WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName = 'Chilled Water Storage'

        WaterThermalTank(WaterThermalTankNum)%Volume = rNumericArgs(1)
        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'GetWaterThermalTankInput')
        WaterThermalTank(WaterThermalTankNum)%Mass = WaterThermalTank(WaterThermalTankNum)%Volume * rho
        WaterThermalTank(WaterThermalTankNum)%Height = rNumericArgs(2)

        SELECT CASE (cAlphaArgs(2))
          CASE ('VERTICALCYLINDER')
            WaterThermalTank(WaterThermalTankNum)%Shape = TankShapeVertCylinder

          CASE ('HORIZONTALCYLINDER')
            WaterThermalTank(WaterThermalTankNum)%Shape = TankShapeHorizCylinder

          CASE ('OTHER')
            WaterThermalTank(WaterThermalTankNum)%Shape = TankShapeOther
            IF (rNumericArgs(3) > 0.0d0) THEN
              WaterThermalTank(WaterThermalTankNum)%Perimeter = rNumericArgs(3)
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Tank Perimeter must be greater than zero for Tank Shape=OTHER')
              ErrorsFound = .TRUE.
            END IF

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Tank Shape entered='//TRIM(cAlphaArgs(2)))
            WaterThermalTank(WaterThermalTankNum)%Shape = TankShapeVertCylinder
            ErrorsFound = .TRUE.
        END SELECT

        IF (rNumericArgs(6) > 0.0d0) THEN
          WaterThermalTank(WaterThermalTankNum)%TankTempLimit = rNumericArgs(6)
        ELSE
          ! default to just above freezing
          WaterThermalTank(WaterThermalTankNum)%TankTempLimit = 1.0D0
        END IF

        WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule = GetScheduleIndex(cAlphaArgs(3))
        IF (WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule .EQ. 0) THEN
              CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)))
              CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Schedule was not found.')
          ErrorsFound = .TRUE.
        END IF

        IF (rNumericArgs(4) > 0.0d0) THEN
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp = rNumericArgs(4)
        ELSE
          ! Default to very small number (however it can't be TINY or it will break the algorithm)
          WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp = 0.0001d0
        END IF

        WaterThermalTank(WaterThermalTankNum)%HeaterHeight1         = rNumericArgs(5)
        WaterThermalTank(WaterThermalTankNum)%MaxCapacity          =  rNumericArgs(7)
        WaterThermalTank(WaterThermalTankNum)%Efficiency            = 1.0D0
        WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule2 = 0
        WaterThermalTank(WaterThermalTankNum)%MaxCapacity2          = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%HeaterHeight2         = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%FuelType = 'Electric'

        WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType = 'Electric'
        WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%OffCycParaHeight =0.0D0
        WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType = 'Electric'
        WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%OnCycParaHeight = 0.0D0


        SELECT CASE (cAlphaArgs(4))
          CASE ('SCHEDULE')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempSchedule
            WaterThermalTank(WaterThermalTankNum)%AmbientTempSchedule = GetScheduleIndex(cAlphaArgs(5))
            IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempSchedule .EQ. 0) THEN
              CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5)))
              CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Schedule was not found.')
              ErrorsFound = .TRUE.
            END IF

          CASE ('ZONE')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempZone
            WaterThermalTank(WaterThermalTankNum)%AmbientTempZone = FindItemInList(cAlphaArgs(6),Zone%Name,NumOfZones)
            IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempZone .EQ. 0) THEN
              CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6)))
              CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Zone was not found.')
              ErrorsFound = .TRUE.
            END IF
            WaterThermalTank(WaterThermalTankNum)%OffCycLossFracToZone = 1.0D0

          CASE ('OUTDOORS')
            WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator = AmbientTempOutsideAir
            WaterThermalTank(WaterThermalTankNum)%AmbientTempOutsideAirNode = GetOnlySingleNode(cAlphaArgs(7), ErrorsFound, &
              TRIM(cCurrentModuleObject), cAlphaArgs(1), NodeType_Air, NodeConnectionType_Inlet, 1, ObjectIsNotParent)
            IF (.NOT. lAlphaFieldBlanks(7) ) THEN
              IF (.not. CheckOutAirNodeNumber(WaterThermalTank(WaterThermalTankNum)%AmbientTempOutsideAirNode)) THEN
                CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(cAlphaArgs(7)))
                CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
                CALL ShowContinueError('Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node')
                ErrorsFound=.true.
              ENDIF
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('An Ambient Outdoor Air Node name must be used when' // &
                               ' the Ambient Temperature Indicator is Outdoors.')
              ErrorsFound = .TRUE.
            ENDIF

          CASE DEFAULT
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Invalid Ambient Temperature Indicator entered='//TRIM(cAlphaArgs(4)))
            CALL ShowContinueError('  Valid entries are Schedule, Zone, and Outdoors.')
            ErrorsFound = .TRUE.
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%SkinLossCoeff = rNumericArgs(8)
        WaterThermalTank(WaterThermalTankNum)%SkinLossFracToZone = 1.0D0
        WaterThermalTank(WaterThermalTankNum)%OffCycFlueLossCoeff = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%OffCycFlueLossFracToZone = 0.0D0

        WaterThermalTank(WaterThermalTankNum)%MassFlowRateMax = 0.0D0
        WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule = 0
        WaterThermalTank(WaterThermalTankNum)%UseInletTempSchedule = 0
        WaterThermalTank(WaterThermalTankNum)%UseEffectiveness = rNumericArgs(9)
        WaterThermalTank(WaterThermalTankNum)%UseInletHeight = rNumericArgs(10)

        ! default to always on
        WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum = ScheduleAlwaysOn
        WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum = ScheduleAlwaysOn

        IF (rNumericArgs(10) == Autocalculate) THEN
          WaterThermalTank(WaterThermalTankNum)%UseInletHeight = WaterThermalTank(WaterThermalTankNum)%Height  ! top of tank
        ENDIF
        IF (WaterThermalTank(WaterThermalTankNum)%UseInletHeight &
             > WaterThermalTank(WaterThermalTankNum)%Height) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Use inlet is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(10))//' = '//TRIM(RoundSigDigits(rNumericArgs(10), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        WaterThermalTank(WaterThermalTankNum)%UseOutletHeight = rNumericArgs(11)
        IF (WaterThermalTank(WaterThermalTankNum)%UseOutletHeight &
             > WaterThermalTank(WaterThermalTankNum)%Height) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Use outlet is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(11))//' = '//TRIM(RoundSigDigits(rNumericArgs(11), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness = rNumericArgs(13)

        WaterThermalTank(WaterThermalTankNum)%SourceInletHeight = rNumericArgs(14)
        IF (WaterThermalTank(WaterThermalTankNum)%SourceInletHeight &
             > WaterThermalTank(WaterThermalTankNum)%Height) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Source inlet is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(14))//' = '//TRIM(RoundSigDigits(rNumericArgs(14), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        WaterThermalTank(WaterThermalTankNum)%SourceOutletHeight = rNumericArgs(15)
        IF (rNumericArgs(15) == Autocalculate) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceOutletHeight = WaterThermalTank(WaterThermalTankNum)%Height  ! top of tank
        ENDIF
        IF (WaterThermalTank(WaterThermalTankNum)%SourceOutletHeight &
             > WaterThermalTank(WaterThermalTankNum)%Height) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ': Source outlet is located higher than overall tank height.' )
          CALL ShowContinueError( TRIM(cNumericFieldNames(2))//' = '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
          CALL ShowContinueError( TRIM(cNumericFieldNames(15))//' = '//TRIM(RoundSigDigits(rNumericArgs(15), 4)) )
          ErrorsFound = .TRUE.
        ENDIF

        WaterThermalTank(WaterThermalTankNum)%StandAlone = .FALSE.

        IF (lNumericFieldBlanks(12)) THEN
          WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    = 0.d0
        ELSE
          WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    = rNumericArgs(12)
        ENDIF

        WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide = DemandSupply_No

        IF (lNumericFieldBlanks(16)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = 0.d0
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = rNumericArgs(16)
        ENDIF

        WaterThermalTank(WaterThermalTankNum)%SizingRecoveryTime      = rNumericArgs(17)

        WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide = DemandSupply_No

        IF ((.NOT. lAlphaFieldBlanks(8) ) .OR. (.NOT. lAlphaFieldBlanks(9) )) THEN
          WaterThermalTank(WaterThermalTankNum)%UseInletNode = &
            GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%InletNodeName1 = cAlphaArgs(8)
          WaterThermalTank(WaterThermalTankNum)%UseOutletNode = &
            GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName1 = cAlphaArgs(9)

        ENDIF

        IF ((.NOT. lAlphaFieldBlanks(11) ) .OR. (.NOT. lAlphaFieldBlanks(12))) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceInletNode = &
            GetOnlySingleNode(cAlphaArgs(11),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%InletNodeName2 = cAlphaArgs(11)
          WaterThermalTank(WaterThermalTankNum)%SourceOutletNode = &
            GetOnlySingleNode(cAlphaArgs(12),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
            NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
          WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName2 = cAlphaArgs(12)

        END IF

        IF (lAlphaFieldBlanks(10)) THEN
          WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum = ScheduleAlwaysOn
        ELSE
          WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum = GetScheduleIndex(cAlphaArgs(10))
          IF (WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum == 0) Then
            CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(10))//' = '//TRIM(cAlphaArgs(10)))
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Schedule was not found.')
            ErrorsFound = .TRUE.
          ENDIF
        ENDIF

        IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide == DemandSide   &
            .and. WaterThermalTank(WaterThermalTankNum)%SourceInletNode /= 0) THEN
          CALL RegisterPlantCompDesignFlow(WaterThermalTank(WaterThermalTankNum)%SourceInletNode,  &
                                    WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate)
        ENDIF

        IF (lAlphaFieldBlanks(13)) THEN
          WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum = ScheduleAlwaysOn
        ELSE
          WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum = GetScheduleIndex(cAlphaArgs(13))
          IF (WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum == 0) Then
            CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(13))//' = '//TRIM(cAlphaArgs(13)))
            CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Schedule was not found.')
            ErrorsFound = .TRUE.
          ENDIF
        ENDIF

        ! Validate inlet mode
        SELECT CASE (cAlphaArgs(14))
          CASE ('FIXED')
            WaterThermalTank(WaterThermalTankNum)%InletMode = InletModeFixed

          CASE ('SEEKING')
            WaterThermalTank(WaterThermalTankNum)%InletMode = InletModeSeeking
        END SELECT

        WaterThermalTank(WaterThermalTankNum)%Nodes = rNumericArgs(18)
        WaterThermalTank(WaterThermalTankNum)%AdditionalCond = rNumericArgs(19)

        ALLOCATE(WaterThermalTank(WaterThermalTankNum)%AdditionalLossCoeff(WaterThermalTank(WaterThermalTankNum)%Nodes))
        WaterThermalTank(WaterThermalTankNum)%AdditionalLossCoeff = 0.0d0
        DO NodeNum = 1, WaterThermalTank(WaterThermalTankNum)%Nodes
          IF (NumNums > 19 + NodeNum) THEN
            WaterThermalTank(WaterThermalTankNum)%AdditionalLossCoeff(NodeNum) = rNumericArgs(19 + NodeNum)
          ELSE
            EXIT
          END IF
        END DO

        IF (NumNums > 19 + WaterThermalTank(WaterThermalTankNum)%Nodes) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  More Additional Loss Coefficients were entered than the number of nodes; extra coefficients will not be used')
        END IF

        CALL SetupStratifiedNodes(WaterThermalTankNum)

      END DO ! WaterThermalTankNum

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in getting '//TRIM(cCurrentModuleObject)//  &
           ' input. Preceding condition causes termination.')
      END IF

    END IF
!!  end stratified chilled water storage

!!!=======   Check Water Heaters ======================================================================================

!   Loop through all desuperheating coils and then search all water heaters for the tank connected to the desuperheating coil
    IF (NumWaterHeaterDesuperheater > 0) THEN
      cCurrentModuleObject = 'Coil:WaterHeating:Desuperheater'
      DO DesuperheaterNum = 1, NumWaterHeaterDesuperheater

        DO CheckWaterHeaterNum = 1, NumWaterThermalTank
          IF(.NOT. SameString(WaterHeaterDesuperheater(DesuperheaterNum)%TankName, WaterThermalTank(CheckWaterHeaterNum)%Name)  &
             .OR. &
             .NOT. SameString(WaterHeaterDesuperheater(DesuperheaterNum)%TankType, WaterThermalTank(CheckWaterHeaterNum)%Type))   &
                CYCLE
            WaterThermalTank(CheckWaterHeaterNum)%DesuperheaterNum = DesuperheaterNum
            WaterHeaterDesuperheater(DesuperheaterNum)%WaterHeaterTankNum = CheckWaterHeaterNum
            WaterHeaterDesuperheater(DesuperheaterNum)%TankTypeNum = WaterThermalTank(CheckWaterHeaterNum)%TypeNum
            WaterHeaterDesuperheater(DesuperheaterNum)%BackupElementCapacity = WaterThermalTank(CheckWaterHeaterNum)%MaxCapacity
            IF (WaterThermalTank(CheckWaterHeaterNum)%UseInletNode .EQ. 0 &
              .AND. WaterThermalTank(CheckWaterHeaterNum)%UseOutletNode .EQ. 0) &
                WaterHeaterDesuperheater(DesuperheaterNum)%StandAlone = .TRUE.

!         verify Desuperheater/tank source node connections
          IF(WaterHeaterDesuperheater(DesuperheaterNum)%WaterInletNode .NE.   &
             WaterThermalTank(CheckWaterHeaterNum)%SourceOutletNode) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//':')
            CALL ShowContinueError('Desuperheater inlet node name does not match '// &
                                   ' thermal tank source outlet node name.')
            CALL ShowContinueError('Desuperheater water inlet and outlet node names = '// &
                                   TRIM(CoilSaveNodeNames(DesuperheaterNum)%InletNodeName1)//' and '// &
                                   TRIM(CoilSaveNodeNames(DesuperheaterNum)%OutletNodeName1))
            CALL ShowContinueError('Thermal tank source side inlet and outlet node names      = '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName2)//' and '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName2))
            ErrorsFound = .TRUE.
          END IF

          IF(WaterHeaterDesuperheater(DesuperheaterNum)%WaterOutletNode .NE.   &
             WaterThermalTank(CheckWaterHeaterNum)%SourceInletNode) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//':')
            CALL ShowContinueError('Desuperheater water outlet node name does not match thermal'// &
                                   ' tank source inlet node name.')
            CALL ShowContinueError('Desuperheater water inlet and outlet node names = '// &
                                   TRIM(CoilSaveNodeNames(DesuperheaterNum)%InletNodeName1)//' and '// &
                                   TRIM(CoilSaveNodeNames(DesuperheaterNum)%OutletNodeName1))
            CALL ShowContinueError('Thermal tank source side inlet and outlet node names      = '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName2)//' and '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName2))
            ErrorsFound = .TRUE.
          END IF

        END DO ! DO CheckWaterHeaterNum = 1, NumWaterHeater

        IF(WaterHeaterDesuperheater(DesuperheaterNum)%WaterHeaterTankNum .EQ. 0)THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//':')
         CALL ShowContinueError(' Water heater tank = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%TankName)//' not found.')
         ErrorsFound = .TRUE.
        END IF

      END DO ! DO DesuperheaterNum = 1, NumWaterHeaterDesuperheater
    END IF

!   Loop through HPWH's and then search all water heaters for the tank connected to the HPWH
    IF (NumHeatPumpWaterHeater > 0) THEN

      cCurrentModuleObject = 'WaterHeater:HeatPump'

      DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater

!       find the tank associated with the heat pump water heater and change its %TYPE to HEAT PUMP:WATER HEATER
        DO CheckWaterHeaterNum = 1, NumWaterThermalTank
          IF(.NOT. SameString(HPWaterHeater(HPWaterHeaterNum)%TankName, WaterThermalTank(CheckWaterHeaterNum)%Name) .OR. &
             .NOT. SameString(HPWaterHeater(HPWaterHeaterNum)%TankType, WaterThermalTank(CheckWaterHeaterNum)%Type)) CYCLE

!         save backup element and on/off-cycle parasitic properties for use during standard rating procedure
          HPWaterHeater(HPWaterHeaterNum)%BackupElementCapacity   = WaterThermalTank(CheckWaterHeaterNum)%MaxCapacity
          HPWaterHeater(HPWaterHeaterNum)%BackupElementEfficiency = WaterThermalTank(CheckWaterHeaterNum)%Efficiency
          HPWaterHeater(HPWaterHeaterNum)%WHOnCycParaLoad         = WaterThermalTank(CheckWaterHeaterNum)%OnCycParaLoad
          HPWaterHeater(HPWaterHeaterNum)%WHOffCycParaLoad        = WaterThermalTank(CheckWaterHeaterNum)%OffCycParaLoad
          HPWaterHeater(HPWaterHeaterNum)%WHOnCycParaFracToTank   = WaterThermalTank(CheckWaterHeaterNum)%OnCycParaFracToTank
          HPWaterHeater(HPWaterHeaterNum)%WHOffCycParaFracToTank  = WaterThermalTank(CheckWaterHeaterNum)%OffCycParaFracToTank
          HPWaterHeater(HPWaterHeaterNum)%WHPLFCurve              = WaterThermalTank(CheckWaterHeaterNum)%PLFCurve

          IF(WaterThermalTank(CheckWaterHeaterNum)%Type .EQ. 'WATER HEATER:SIMPLE')THEN  ! name change issue here.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
            CALL ShowContinueError('WaterHeater:HeatPump cannot be used with WATER HEATER:SIMPLE.')
            ErrorsFound = .TRUE.
          ELSEIF ((WaterThermalTank(CheckWaterHeaterNum)%Type .EQ. cMixedWHModuleObj)  &
            .OR. (WaterThermalTank(CheckWaterHeaterNum)%Type .EQ. cStratifiedWHModuleObj)) THEN
            HPWaterHeater(HPWaterHeaterNum)%TankTypeNum = WaterThermalTank(CheckWaterHeaterNum)%TypeNum
!           use typenum parameter to simulate heatpumpwaterheater in standard ratings procedure
!           WaterThermalTank%TypeNum = HeatPumpWaterHeater for a HPWH
!            WaterThermalTank(CheckWaterHeaterNum)%TypeNum = HPWaterHeater(HPWaterHeaterNum)%TypeNum
          ELSE
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
            CALL ShowContinueError('Invalid water heater tank type ='//TRIM(WaterThermalTank(CheckWaterHeaterNum)%Type))
            ErrorsFound = .TRUE.
          END IF

!         do not allow modulating control for HPWH's (i.e. modulating control usually used for tankless WH's)
          IF(WaterThermalTank(CheckWaterHeaterNum)%ControlType .EQ. ControlTypeModulate)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
            CALL ShowContinueError('Heater Control Type for '//TRIM(WaterThermalTank(CheckWaterHeaterNum)%Type)//' = '// &
                                    TRIM(WaterThermalTank(CheckWaterHeaterNum)%Name)//' must be CYCLE.')
            ErrorsFound = .TRUE.
          END IF

          WaterThermalTank(CheckWaterHeaterNum)%HeatPumpNum = HPWaterHeaterNum
          HPWaterHeater(HPWaterHeaterNum)%WaterHeaterTankNum = CheckWaterHeaterNum

          IF(WaterThermalTank(CheckWaterHeaterNum)%DesuperheaterNum .GT. 0)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)// &
               'and Coil:WaterHeating:Desuperheater = '//TRIM(WaterHeaterDesuperheater(CheckWaterHeaterNum)%Name)// &
               ':  cannot be connected to the same water heater tank = '//TRIM(WaterThermalTank(CheckWaterHeaterNum)%Name))
          END IF

!         check that water heater source side effectiveness is greater than 0
          IF(WaterThermalTank(CheckWaterHeaterNum)%SourceEffectiveness .LE. 0.0d0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)// &
               ':  Invalid source side effectiveness for heat pump water heater = ' &
                 //TrimSigDigits(WaterThermalTank(CheckWaterHeaterNum)%SourceEffectiveness,3))
            CALL ShowContinueError(' water heater source effectiveness will default to 1.0 and simulation continues.')
             WaterThermalTank(CheckWaterHeaterNum)%SourceEffectiveness = 1.0d0
          END IF

!         Set HPWH structure variable StandAlone to TRUE if use nodes are not connected
          IF (WaterThermalTank(CheckWaterHeaterNum)%UseInletNode .EQ. 0 .AND.   &
             WaterThermalTank(CheckWaterHeaterNum)%UseOutletNode .EQ. 0) &
            HPWaterHeater(HPWaterHeaterNum)%StandAlone = .TRUE.

          IF(HPWaterHeater(HPWaterHeaterNum)%WHUseInletNode /= WaterThermalTank(CheckWaterHeaterNum)%UseInletNode .OR. &
             HPWaterHeater(HPWaterHeaterNum)%WHUseOutletNode /= WaterThermalTank(CheckWaterHeaterNum)%UseOutletNode)THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
            CALL ShowContinueError('Heat pump water heater tank use side inlet and outlet node names must match'// &
                                   ' the use side inlet and outlet node names for water heater tank = '// &
                                   TRIM(HPWaterHeater(HPWaterHeaterNum)%TankType)//': ' &
                                 //TRIM(HPWaterHeater(HPWaterHeaterNum)%TankName))
            CALL ShowContinueError('Heat pump water heater use side inlet and outlet node names = '// &
                                   TRIM(HPWHSaveNodeNames(HPWaterHeaterNum)%InletNodeName2)//' and '// &
                                   TRIM(HPWHSaveNodeNames(HPWaterHeaterNum)%OutletNodeName2))
            CALL ShowContinueError('Water heater tank use side inlet and outlet node names      = '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName1)//' and '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName1))
            ErrorsFound = .TRUE.
          ELSE
            IF(.NOT. HPWaterHeater(HPWaterHeaterNum)%StandAlone)THEN
!              removed next to avoid duplicate comp set issue, (should change so that Branch has tank object)
!              CALL SetUpCompSets(HPWaterHeater(HPWaterHeaterNum)%Type, HPWaterHeater(HPWaterHeaterNum)%Name, &
!                     HPWaterHeater(HPWaterHeaterNum)%TankType, &
!                     HPWaterHeater(HPWaterHeaterNum)%TankName, &
!                     WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName1,WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName1)
              CALL TestCompSet(HPWaterHeater(HPWaterHeaterNum)%Type,HPWaterHeater(HPWaterHeaterNum)%Name, &
                     WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName1,WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName1, &
                    'Water Nodes')
            END IF
          END IF

!         verify HP/tank source node connections
          IF(HPWaterHeater(HPWaterHeaterNum)%CondWaterInletNode .NE. WaterThermalTank(CheckWaterHeaterNum)%SourceOutletNode) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
            CALL ShowContinueError('Heat Pump condenser water inlet node name does not match water'// &
                                   ' heater tank source outlet node name.')
            CALL ShowContinueError('Heat pump condenser water inlet and outlet node names = '// &
                                   TRIM(HPWHSaveNodeNames(HPWaterHeaterNum)%InletNodeName1)//' and '// &
                                   TRIM(HPWHSaveNodeNames(HPWaterHeaterNum)%OutletNodeName1))
            CALL ShowContinueError('Water heater tank source side inlet and outlet node names      = '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName2)//' and '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName2))
            ErrorsFound = .TRUE.
          END IF

          IF(HPWaterHeater(HPWaterHeaterNum)%CondWaterOutletNode .NE. WaterThermalTank(CheckWaterHeaterNum)%SourceInletNode) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
            CALL ShowContinueError('Heat Pump condenser water outlet node name does not match water heater'// &
                                   ' tank source inlet node name.')
            CALL ShowContinueError('Heat pump condenser water inlet and outlet node names = '// &
                                   TRIM(HPWHSaveNodeNames(HPWaterHeaterNum)%InletNodeName1)//' and '// &
                                   TRIM(HPWHSaveNodeNames(HPWaterHeaterNum)%OutletNodeName1))
            CALL ShowContinueError('Water heater tank source side inlet and outlet node names      = '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%InletNodeName2)//' and '// &
                                   TRIM(WHSaveNodeNames(CheckWaterHeaterNum)%OutletNodeName2))
            ErrorsFound = .TRUE.
          END IF

          HPWaterHeater(HPWaterHeaterNum)%FoundTank = .TRUE.

!         Verify tank name is in a zone equipment list if HPWH Inlet Air Configuration is Zone Air Only or Zone and Outdoor Air
          IF(HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZone .OR. &
             HPWaterHeater(HPWaterHeaterNum)%InletAirConfiguration .EQ. AmbientTempZoneAndOA)THEN
            IF(ALLOCATED(ZoneEquipConfig) .AND. ALLOCATED(ZoneEquipList))THEN
              FoundTankInList = .FALSE.
              TankNotLowestPriority = .FALSE.
              DO ZoneEquipConfigNum = 1, NumOfZones
                IF(ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum .NE. HPWaterHeater(HPWaterHeaterNum)%AmbientTempZone)CYCLE
                IF(ZoneEquipConfigNum .LE. NumOfZones)THEN
                  DO ZoneEquipListNum = 1, NumOfZones
                    IF(ZoneEquipConfig(ZoneEquipConfigNum)%EquipListName .NE. ZoneEquipList(ZoneEquipListNum)%Name)CYCLE
                    IF(ZoneEquipConfigNum .LE. NumOfZones)THEN
                      DO EquipmentTypeNum = 1, ZoneEquipList(ZoneEquipListNum)%NumOfEquipTypes
                        IF(ZoneEquipList(ZoneEquipListNum)%EquipName(EquipmentTypeNum) .NE. &
                           HPWaterHeater(HPWaterHeaterNum)%Name)CYCLE
                        FoundTankInList = .TRUE.
                        TankCoolingPriority = ZoneEquipList(ZoneEquipListNum)%CoolingPriority(EquipmentTypeNum)
                        TankHeatingPriority = ZoneEquipList(ZoneEquipListNum)%HeatingPriority(EquipmentTypeNum)
                        EXIT
                      END DO ! EquipmentTypeNum
                      IF(.NOT. FoundTankInList)THEN
                        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
                        CALL ShowContinueError('Heat pump water heater type and name must be listed in the correct'// &
                                               ' ZoneHVAC:EquipmentList object when Inlet Air Configuration is equal'// &
                                               ' to ZoneAirOnly or ZoneAndOutdoorAir.')
                        ErrorsFound = .TRUE.
                      END IF
!                     check that tank has lower priority than all other non-HPWH objects in Zone Equipment List
                      DO EquipmentTypeNum = 1, ZoneEquipList(ZoneEquipListNum)%NumOfEquipTypes
                        IF(SameString(ZoneEquipList(ZoneEquipListNum)%EquipType(EquipmentTypeNum),cCurrentModuleObject))CYCLE
                          IF(TankCoolingPriority .GT. ZoneEquipList(ZoneEquipListNum)%CoolingPriority(EquipmentTypeNum) .OR. &
                             TankHeatingPriority .GT. ZoneEquipList(ZoneEquipListNum)%HeatingPriority(EquipmentTypeNum))THEN
                            TankNotLowestPriority = .TRUE.
                        END IF
                      END DO ! EquipmentTypeNum
                      IF(TankNotLowestPriority .AND. FoundTankInList)THEN
                          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
                          CALL ShowContinueError('Heat pump water heaters must have lower priorities than'// &
                                                 ' all other equipment types in a ZoneHVAC:EquipmentList.')
                        ErrorsFound = .TRUE.
                      END IF
                      EXIT
                   END IF ! ZoneEquipConfigNum .LE. NumOfZoneEquipLists
                  END DO ! ZoneEquipListNum
                  EXIT
                END IF ! ZoneEquipConfigNum .LE. NumOfZones
              END DO ! ZoneEquipConfigNum
            ELSE
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
              CALL ShowContinueError('ZoneHVAC:EquipmentList and ZoneHVAC:EquipmentConnections objects are '// &
                                     ' required when Inlet Air Configuration is either ZoneAirOnly or ZoneAndOutdoorAir.')
              ErrorsFound = .TRUE.
            END IF ! ALLOCATED
          END IF !InletAirConfiguration

        END DO ! DO CheckWaterHeaterNum = 1, NumWaterHeater

        IF(.NOT. HPWaterHeater(HPWaterHeaterNum)%FoundTank)THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%Name)//':')
          CALL ShowContinueError('Water heater tank object not found = '//TRIM(HPWaterHeater(HPWaterHeaterNum)%TankType)&
                                //', '//TRIM(HPWaterHeater(HPWaterHeaterNum)%TankName))
          ErrorsFound = .TRUE.
        END IF

      END DO ! DO HPWaterHeaterNum = 1, NumHeatPumpWaterHeater

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in getting '//TRIM(cCurrentModuleObject)//  &
           ' input. Preceding condition causes termination.')
      END IF

    END IF

   !Get water heater sizing input.
    cCurrentModuleObject='WaterHeater:Sizing'
    NumWaterHeaterSizing  = GetNumObjectsFound(cCurrentModuleObject)

    If (NumWaterHeaterSizing > 0) then

      DO WHsizingNum = 1, NumWaterHeaterSizing
        CALL GetObjectItem(cCurrentModuleObject, WHsizingNum, cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT)

        ! find which water heater this object is for
        WaterThermalTankNum = FindItemInList(cAlphaArgs(1),WaterThermalTank%Name, NumWaterThermalTank)
        IF (WaterThermalTankNum == 0) then
          ! did not match name throw warning.
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' object name: '//TRIM(cAlphaArgs(1))// ' does not match ' &
               //'any of the water heaters defined in the file')
          ErrorsFound = .TRUE.
          cycle
        ELSE ! we have a match
          ! store the sizing data in "sizing" nested derived type for the correct water heater

          IF (SameString(cAlphaArgs(2), 'PeakDraw')) THEN
            WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode = SizePeakDraw
          ELSEIF (SameString(cAlphaArgs(2), 'ResidentialHUD-FHAMinimum')) THEN
            WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode = SizeResidentialMin
          ELSEIF (SameString(cAlphaArgs(2), 'PerPerson')) THEN
            WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode = SizePerPerson
          ELSEIF (SameString(cAlphaArgs(2), 'PerFloorArea')) THEN
            WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode = SizePerFloorArea
          ELSEIF (SameString(cAlphaArgs(2), 'PerUnit')) THEN
            WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode = SizePerUnit
          ELSEIF (SameString(cAlphaArgs(2), 'PerSolarCollectorArea')) THEN
            WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode = SizePerSolarColArea
          ELSE
            ! wrong design mode entered, throw error
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//' object named: '//TRIM(cAlphaArgs(1))  &
              // ' contains an incorrect Design Mode of: ' //Trim(cAlphaArgs(2) ) )
            ErrorsFound = .TRUE.
          ENDIF

          WaterThermalTank(WaterThermalTankNum)%Sizing%TankDrawTime              = rNumericArgs(1)
          WaterThermalTank(WaterThermalTankNum)%Sizing%RecoveryTime              = rNumericArgs(2)
          WaterThermalTank(WaterThermalTankNum)%Sizing%NominalVolForSizingDemandSideFlow = rNumericArgs(3)
          WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms          = INT(rNumericArgs(4))
          WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms         = INT(rNumericArgs(5))
          WaterThermalTank(WaterThermalTankNum)%Sizing%TankCapacityPerPerson     = rNumericArgs(6)
          WaterThermalTank(WaterThermalTankNum)%Sizing%RecoveryCapacityPerPerson = rNumericArgs(7)
          WaterThermalTank(WaterThermalTankNum)%Sizing%TankCapacityPerArea       = rNumericArgs(8)
          WaterThermalTank(WaterThermalTankNum)%Sizing%RecoveryCapacityPerArea   = rNumericArgs(9)
          WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfUnits             = rNumericArgs(10)
          WaterThermalTank(WaterThermalTankNum)%Sizing%TankCapacityPerUnit       = rNumericArgs(11)
          WaterThermalTank(WaterThermalTankNum)%Sizing%RecoveryCapacityPerUnit   = rNumericArgs(12)
          WaterThermalTank(WaterThermalTankNum)%Sizing%TankCapacityPerCollectorArea = rNumericArgs(13)
          WaterThermalTank(WaterThermalTankNum)%Sizing%HeightAspectRatio         = rNumericArgs(14)

          Select Case (WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode)

          CASE (SizeNotSet)
            ! do nothing, error thrown if design mode not found
          CASE (SizePeakDraw) ! need to have entered a reasonable value for TankDrawTime
            IF (WaterThermalTank(WaterThermalTankNum)%Sizing%TankDrawTime <= 0.0D0 ) then
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                 ', design mode set to Peak Draw but needs a positive value for tank draw time' )
              ErrorsFound = .true.
            ENDIF
            !constrain crazy sizes by limiting to 10 years or 8760*10
            IF (WaterThermalTank(WaterThermalTankNum)%Sizing%TankDrawTime > 87600.0D0 )THEN
              CALL ShowWarningError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))//  &
                ',  has input with an unreasonably large Tank Draw Time, more than 10 years')
              ErrorsFound = .true.
            ENDIF
            ! if both volume and demand side flow connections are autosized, must be a good NominalVolForSizingDemandSideFlow
            If ((WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide == DemandSide) .AND. &
                (WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate == AutoSize)) Then
                IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NominalVolForSizingDemandSideFlow <= 0.0D0) then
                  Call ShowWarningError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))//  &
                    ' needs a value for Nominal Tank Volume for Autosizing Plant Connections')
                  ErrorsFound = .true.
                ENDIF
            ENDIF
            If ((WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide == DemandSide) .AND. &
                (WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate == AutoSize)) Then
                IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NominalVolForSizingDemandSideFlow <= 0.0D0) then
                  Call ShowWarningError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))//  &
                    ' needs a value for Nominal Tank Volume for Autosizing Plant Connections')
                  ErrorsFound = .true.
                ENDIF
            ENDIF

          CASE (SizeResidentialMin)
            ! it would have to have at least on bedroom and any more than 10 is crazy for this mode
            IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms < 1 ) then
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', mode needs at least one bedroom')
              ErrorsFound = .true.
            ENDIF
            IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms > 10 ) then
              CALL ShowWarningError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', probably has too many bedrooms for the selected design mode')
            ENDIF

          CASE (SizePerPerson)

            IF ((WaterThermalTank(WaterThermalTankNum)%Volume == Autosize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Sizing%TankCapacityPerPerson <= 0.d0)) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', PerPerson mode needs positive value input for storage capacity per person')
              ErrorsFound = .true.
            ENDIF

            IF ((WaterThermalTank(WaterThermalTankNum)%MaxCapacity == Autosize)  .AND. &
                (WaterThermalTank(WaterThermalTankNum)%sizing%RecoveryCapacityPerPerson <= 0.d0) ) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', PerPerson mode needs positive value input for recovery capacity per person')
              ErrorsFound = .true.
            ENDIF

          CASE (SizePerFloorArea)
            IF ((WaterThermalTank(WaterThermalTankNum)%Volume == Autosize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Sizing%TankCapacityPerArea <= 0.d0)) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', PerArea mode needs positive value input for storage capacity per floor area')
              ErrorsFound = .true.
            ENDIF
            IF ((WaterThermalTank(WaterThermalTankNum)%MaxCapacity == Autosize)  .AND. &
                (WaterThermalTank(WaterThermalTankNum)%sizing%RecoveryCapacityPerArea <= 0.d0) ) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', PerArea mode needs positive value input for recovery capacity per floor area')
              ErrorsFound = .true.
            ENDIF

          CASE (SizePerUnit)
            IF ((WaterThermalTank(WaterThermalTankNum)%Volume == Autosize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Sizing%TankCapacityPerUnit <= 0.d0)) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', PerUnit mode needs positive value input for storage capacity per unit')
              ErrorsFound = .true.
            ENDIF
            IF ((WaterThermalTank(WaterThermalTankNum)%Volume == Autosize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfUnits <= 0.d0)) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', PerUnit mode needs positive value input for number of units')
              ErrorsFound = .true.
            ENDIF
            IF ((WaterThermalTank(WaterThermalTankNum)%MaxCapacity == Autosize)  .AND. &
                (WaterThermalTank(WaterThermalTankNum)%sizing%RecoveryCapacityPerUnit <= 0.d0) ) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', PerUnit mode needs positive value input for recovery capacity per unit')
              ErrorsFound = .true.
            ENDIF
            IF ((WaterThermalTank(WaterThermalTankNum)%MaxCapacity == Autosize)  .AND. &
                (WaterThermalTank(WaterThermalTankNum)%sizing%NumberOfUnits <= 0.d0) ) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', PerUnit mode needs positive value input for number of units')
              ErrorsFound = .true.
            ENDIF
          CASE (SizePerSolarColArea)
            IF ((WaterThermalTank(WaterThermalTankNum)%Volume == Autosize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Sizing%TankCapacityPerCollectorArea <= 0.d0)) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', named '//TRIM(cAlphaArgs(1))// &
                ', PerSolarCollectorArea mode needs positive value input for storage capacity per collector area')
              ErrorsFound = .true.
            ENDIF
          END SELECT


        ENDIF !found water heater num okay
      ENDDO ! loop over sizing objects

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in getting '//TRIM(cCurrentModuleObject)//  &
           ' input. Preceding condition causes termination.')
      END IF

    ENDIF ! any water heater sizing objects

    !now check that if water heater fields were autosized, that there was also a sizing object for that water heater
    IF (NumWaterThermalTank > 0) THEN
      DO WaterThermalTankNum = 1, NumWaterThermalTank

        If ((WaterThermalTank(WaterThermalTankNum)%Volume == Autosize) &
           .AND. (WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode == SizeNotSet)) then
          CALL ShowWarningError('Water heater named '//TRIM(WaterThermalTank(WaterThermalTankNum)%Name) // &
                'has tank volume set to AUTOSIZE but it is missing associated WaterHeater:Sizing object')
          ErrorsFound = .TRUE.
        ENDIF
        If ((WaterThermalTank(WaterThermalTankNum)%MaxCapacity == Autosize) &
           .AND. (WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode == SizeNotSet)) then
          CALL ShowWarningError('Water heater named '//TRIM(WaterThermalTank(WaterThermalTankNum)%Name) // &
                'has heater capacity set to AUTOSIZE but it is missing associated WaterHeater:Sizing object')
          ErrorsFound = .TRUE.
        ENDIF
        If ((WaterThermalTank(WaterThermalTankNum)%Height == Autosize) &
           .AND. (WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode == SizeNotSet)) then
          CALL ShowWarningError('Water heater named '//TRIM(WaterThermalTank(WaterThermalTankNum)%Name) // &
                'has tank height set to AUTOSIZE but it is missing associated WaterHeater:Sizing object')
          ErrorsFound = .TRUE.
        ENDIF
      ENDDO

      If (ErrorsFound) then
        CALL ShowFatalError('Errors found in water heater input. Preceding condition causes termination.')
      ENDIF
    ENDIF

!!   now do calls to TestCompSet for tanks, depending on nodes and heat pump water heater
    IF (NumWaterThermalTank > 0) THEN
      DO WaterThermalTankNum = 1, NumWaterThermalTank
        IF ( WaterThermalTank(WaterThermalTankNum)%UseInletNode > 0  &
             .AND. WaterThermalTank(WaterThermalTankNum)%UseOutletNode > 0) THEN
          IF (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum > 0) THEN
            ! do nothing, Use nodes are tested for HeatPump:WaterHeater not tank
          ELSE
            CALL TestCompSet(WaterThermalTank(WaterThermalTankNum)%Type,  &
                             WaterThermalTank(WaterThermalTankNum)%Name,  &
                             WHSaveNodeNames(WaterThermalTankNum)%InletNodeName1 , &
                             WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName1, &
                            'Use Side Water Nodes')
          ENDIF
        ENDIF
        IF ( WaterThermalTank(WaterThermalTankNum)%SourceInletNode > 0  &
             .AND. WaterThermalTank(WaterThermalTankNum)%SourceOutletNode > 0) THEN

          CALL TestCompSet(WaterThermalTank(WaterThermalTankNum)%Type,  &
                           WaterThermalTank(WaterThermalTankNum)%Name,  &
                           WHSaveNodeNames(WaterThermalTankNum)%InletNodeName2 , &
                           WHSaveNodeNames(WaterThermalTankNum)%OutletNodeName2, &
                          'Source Side Water Nodes')
        ENDIF
      ENDDO
    ENDIF

    IF (NumWaterThermalTank > 0) THEN
      DO WaterThermalTankNum = 1, NumWaterThermalTank
        IF ((WaterThermalTank(WaterThermalTankNum)%TypeNum /= MixedChilledWaterStorage)   &
             .AND. (WaterThermalTank(WaterThermalTankNum)%TypeNum /= StratifiedChilledWaterStorage) ) THEN
          ! Setup report variables for WaterHeater:Mixed
          ! CurrentModuleObject='WaterHeater:Mixed'
          CALL SetupOutputVariable('Water Heater Tank Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%TankTempAvg,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Final Tank Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%TankTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Heat Loss Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%LossRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Heat Loss Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%LossEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Use Side Mass Flow Rate [kg/s]', &
            WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Use Side Inlet Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%UseInletTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Use Side Outlet Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%UseOutletTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Use Side Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%UseRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Use Side Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%UseEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Source Side Mass Flow Rate [kg/s]', &
            WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Source Side Inlet Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%SourceInletTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Source Side Outlet Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Source Side Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%SourceRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Source Side Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%SourceEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name, &
            ResourceTypeKey='PLANTLOOPHEATINGDEMAND',GroupKey='Plant', &
            EndUseKey='DHW',EndUseSubKey=WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName)

          CALL SetupOutputVariable('Water Heater Off Cycle Parasitic Tank Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%OffCycParaRateToTank,'System','Average',  &
               WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Off Cycle Parasitic Tank Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%OffCycParaEnergyToTank,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater On Cycle Parasitic Tank Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%OnCycParaRateToTank,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater On Cycle Parasitic Tank Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%OnCycParaEnergyToTank,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Total Demand Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%TotalDemandRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Total Demand Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%TotalDemandEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Heating Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%HeaterRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Heating Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%HeaterEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Unmet Demand Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%UnmetRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Unmet Demand Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%UnmetEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Venting Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%VentRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Venting Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%VentEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Net Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%NetHeatTransferRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Net Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%NetHeatTransferEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Water Heater Cycle On Count []', &
            WaterThermalTank(WaterThermalTankNum)%CycleOnCount,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Runtime Fraction []', &
            WaterThermalTank(WaterThermalTankNum)%RuntimeFraction,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Part Load Ratio []', &
            WaterThermalTank(WaterThermalTankNum)%PartLoadRatio,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType, 'Electric') ) THEN
            CALL SetupOutputVariable('Water Heater Electric Power [W]', &
              WaterThermalTank(WaterThermalTankNum)%FuelRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          ELSE
            CALL SetupOutputVariable('Water Heater '//TRIM(WaterThermalTank(WaterThermalTankNum)%FuelType)//' Rate [W]', &
              WaterThermalTank(WaterThermalTankNum)%FuelRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          ENDIF
          CALL SetupOutputVariable('Water Heater '//TRIM(WaterThermalTank(WaterThermalTankNum)%FuelType)//' Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%FuelEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name,  &
            ResourceTypeKey=WaterThermalTank(WaterThermalTankNum)%FuelType,GroupKey='Plant', &
            EndUseKey='DHW',EndUseSubKey=WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName)
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType, 'Electric') ) THEN
            CALL SetupOutputVariable( &
              'Water Heater Off Cycle Parasitic Electric Power [W]', &
              WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelRate, &
             'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          ELSE
            CALL SetupOutputVariable( &
              'Water Heater Off Cycle Parasitic '//  &
                 TRIM(WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType)//' Rate [W]', &
              WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelRate,&
              'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          ENDIF
          CALL SetupOutputVariable( &
              'Water Heater Off Cycle Parasitic '//  &
                 TRIM(WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType)//' Energy [J]', &
              WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelEnergy,'System','Sum', &
              WaterThermalTank(WaterThermalTankNum)%Name, &
              ResourceTypeKey=WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelType,GroupKey='Plant', &
              EndUseKey='DHW',EndUseSubKey=WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName)
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType, 'Electric') ) THEN
            CALL SetupOutputVariable( &
              'Water Heater On Cycle Parasitic Electric Power [W]', &
              WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          ELSE
            CALL SetupOutputVariable( &
              'Water Heater On Cycle Parasitic '//TRIM(WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType)//  &
                 ' Rate [W]', &
              WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          ENDIF

          CALL SetupOutputVariable( &
            'Water Heater On Cycle Parasitic '//TRIM(WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType)//  &
               ' Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name, &
            ResourceTypeKey=WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelType,GroupKey='Plant', &
            EndUseKey='DHW',EndUseSubKey=WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName)

          CALL SetupOutputVariable('Water Heater Water Volume Flow Rate [m3/s]', &
            WaterThermalTank(WaterThermalTankNum)%VolFlowRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Water Heater Water Volume [m3]',WaterThermalTank(WaterThermalTankNum)%VolumeConsumed, &
            'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name,ResourceTypeKey='Water',GroupKey='Plant', &
            EndUseKey='DHW',EndUseSubKey=WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName)
          CALL SetupOutputVariable('Water Heater Mains Water Volume [m3]',  &
             WaterThermalTank(WaterThermalTankNum)%VolumeConsumed, &
            'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name,ResourceTypeKey='MainsWater',GroupKey='Plant', &
            EndUseKey='DHW',EndUseSubKey=WaterThermalTank(WaterThermalTankNum)%EndUseSubcategoryName)

          IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0) THEN
            !CurrentModuleObject='WaterHeater:HeatPump'
            CALL SetupOutputVariable('Water Heater Compressor Part Load Ratio []', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%HeatingPLR,'System','Average', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Name)
            CALL SetupOutputVariable('Water Heater Off Cycle Ancillary Electric Power [W]', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%OffCycParaFuelRate,'System','Average', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Name)
            CALL SetupOutputVariable('Water Heater Off Cycle Ancillary Electric Energy [J]', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%OffCycParaFuelEnergy,'System','Sum', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Name, &
              ResourceTypeKey='Electric',EndUseKey='DHW',EndUseSubKey='Water Heater Parasitic', GroupKey='Plant')
            CALL SetupOutputVariable('Water Heater On Cycle Ancillary Electric Power [W]', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%OnCycParaFuelRate,'System','Average', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Name)
            CALL SetupOutputVariable('Water Heater On Cycle Ancillary Electric Energy [J]', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%OnCycParaFuelEnergy,'System','Sum', &
              HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Name, &
              ResourceTypeKey='Electric',EndUseKey='DHW',EndUseSubKey='Water Heater Parasitic',GroupKey='Plant')
          END IF

         IF(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum .GT. 0) THEN
            !CurrentModuleObject='Coil:WaterHeating:Desuperheater'
            CALL SetupOutputVariable('Water Heater Part Load Ratio []', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%DesuperheaterPLR,  &
                 'System','Average', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name)
            CALL SetupOutputVariable('Water Heater On Cycle Parasitic Electric Power [W]', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%OnCycParaFuelRate,  &
                 'System','Average', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name)
            CALL SetupOutputVariable('Water Heater On Cycle Parasitic Electric Energy [J]', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%OnCycParaFuelEnergy,  &
                 'System','Sum', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name, &
              ResourceTypeKey='Electric',EndUseKey='DHW',EndUseSubKey='Water Heater Parasitic',GroupKey='Plant')
            CALL SetupOutputVariable('Water Heater Off Cycle Parasitic Electric Power [W]', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%OffCycParaFuelRate,  &
                 'System','Average', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name)
            CALL SetupOutputVariable('Water Heater Off Cycle Parasitic Electric Energy [J]', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%OffCycParaFuelEnergy,  &
                 'System','Sum', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name, &
              ResourceTypeKey='Electric',EndUseKey='DHW',EndUseSubKey='Water Heater Parasitic', GroupKey='Plant')
            CALL SetupOutputVariable('Water Heater Heat Reclaim Efficiency Modifier Multiplier []', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%HEffFTempOutput,  &
                 'System','Average', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name)
            CALL SetupOutputVariable('Water Heater Pump Electric Power [W]', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%PumpPower,'System','Average', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name)
            CALL SetupOutputVariable('Water Heater Pump Electric Energy [J]', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%PumpEnergy,'System','Sum', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name, &
              ResourceTypeKey='Electric',EndUseKey='DHW',EndUseSubKey='Desuperheater Pump', GroupKey='Plant')
            CALL SetupOutputVariable('Water Heater Heating Rate [W]', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%HeaterRate,'System','Average', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name)
            CALL SetupOutputVariable('Water Heater Heating Energy [J]', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%HeaterEnergy,'System','Sum', &
              WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Name, &
              ResourceTypeKey='EnergyTransfer',EndUseKey='DHW',EndUseSubKey='Water Heater', GroupKey='Plant')
          END IF

          ! Setup report variables for WaterHeater:Stratified
          ! CurrentModuleObject='WaterHeater:Stratified'
          IF (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedWaterHeater) THEN

            CALL SetupOutputVariable('Water Heater Heater 1 Heating Rate [W]', &
              WaterThermalTank(WaterThermalTankNum)%HeaterRate1,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
            CALL SetupOutputVariable('Water Heater Heater 2 Heating Rate [W]', &
              WaterThermalTank(WaterThermalTankNum)%HeaterRate2,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

            CALL SetupOutputVariable('Water Heater Heater 1 Heating Energy [J]', &
              WaterThermalTank(WaterThermalTankNum)%HeaterEnergy1,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)
            CALL SetupOutputVariable('Water Heater Heater 2 Heating Energy [J]', &
              WaterThermalTank(WaterThermalTankNum)%HeaterEnergy2,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

            CALL SetupOutputVariable('Water Heater Heater 1 Cycle On Count []', &
              WaterThermalTank(WaterThermalTankNum)%CycleOnCount1,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)
            CALL SetupOutputVariable('Water Heater Heater 2 Cycle On Count  []', &
              WaterThermalTank(WaterThermalTankNum)%CycleOnCount2,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

            CALL SetupOutputVariable('Water Heater Heater 1 Runtime Fraction []', &
              WaterThermalTank(WaterThermalTankNum)%RuntimeFraction1,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
            CALL SetupOutputVariable('Water Heater Heater 2 Runtime Fraction []', &
              WaterThermalTank(WaterThermalTankNum)%RuntimeFraction2,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

            DO NodeNum = 1, WaterThermalTank(WaterThermalTankNum)%Nodes
              CALL SetupOutputVariable('Water Heater Temperature Node '//TRIM(TrimSigDigits(NodeNum))//' [C]', &
                WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%TempAvg,'System','Average',  &
                   WaterThermalTank(WaterThermalTankNum)%Name)
            END DO

            DO NodeNum = 1, WaterThermalTank(WaterThermalTankNum)%Nodes
              CALL SetupOutputVariable('Water Heater Final Temperature Node '//TRIM(TrimSigDigits(NodeNum))//'  [C]', &
                WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Temp,'System','Average',  &
                   WaterThermalTank(WaterThermalTankNum)%Name)
            END DO
          END IF



          IF (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedWaterHeater) THEN

    723 FORMAT('Water Heater Stratified Node Information',8(',',A))

            DO NodeNum = 1, WaterThermalTank(WaterThermalTankNum)%Nodes
              WRITE(OutputFileInits,723) TRIM(TrimSigDigits(NodeNum)),                            &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Height,4)),          &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Volume,4)),          &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MaxCapacity,3)),     &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OffCycLossCoeff,4)), &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OnCycLossCoeff,4)),  &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Inlets)),            &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Outlets))
            END DO
          END IF

          IF (ErrorsFound) THEN
            CALL ShowFatalError('Errors found in getting water heater input. Preceding condition causes termination.')
          END IF

        ELSEIF ((WaterThermalTank(WaterThermalTankNum)%TypeNum == MixedChilledWaterStorage)        &
                .OR. (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedChilledWaterStorage) ) THEN
          ! CurrentModuleObject='ThermalStorage:ChilledWater:Mixed/ThermalStorage:ChilledWater:Stratified'
          CALL SetupOutputVariable('Chilled Water Thermal Storage Tank Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%TankTempAvg,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Final Tank Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%TankTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Tank Heat Gain Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%LossRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Chilled Water Thermal Storage Tank Heat Gain Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%LossEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Use Side Mass Flow Rate [kg/s]', &
            WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Use Side Inlet Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%UseInletTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Use Side Outlet Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%UseOutletTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Use Side Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%UseRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Chilled Water Thermal Storage Use Side Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%UseEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Source Side Mass Flow Rate [kg/s]', &
            WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Source Side Inlet Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%SourceInletTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Source Side Outlet Temperature [C]', &
            WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)

          CALL SetupOutputVariable('Chilled Water Thermal Storage Source Side Heat Transfer Rate [W]', &
            WaterThermalTank(WaterThermalTankNum)%SourceRate,'System','Average',WaterThermalTank(WaterThermalTankNum)%Name)
          CALL SetupOutputVariable('Chilled Water Thermal Storage Source Side Heat Transfer Energy [J]', &
            WaterThermalTank(WaterThermalTankNum)%SourceEnergy,'System','Sum',WaterThermalTank(WaterThermalTankNum)%Name)

          IF (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedChilledWaterStorage) THEN

            DO NodeNum = 1, WaterThermalTank(WaterThermalTankNum)%Nodes
              CALL SetupOutputVariable('Chilled Water Thermal Storage Temperature Node '//TRIM(TrimSigDigits(NodeNum))//' [C]', &
                WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%TempAvg,'System','Average',  &
                   WaterThermalTank(WaterThermalTankNum)%Name)
            END DO

            DO NodeNum = 1, WaterThermalTank(WaterThermalTankNum)%Nodes
              CALL SetupOutputVariable('Chilled Water Thermal Storage Final Temperature Node ' &
                                      //TRIM(TrimSigDigits(NodeNum))//' [C]', &
                WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Temp,'System','Average',  &
                   WaterThermalTank(WaterThermalTankNum)%Name)
            END DO
          END IF



          IF (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedChilledWaterStorage) THEN

    724 FORMAT('Chilled Water Tank Stratified Node Information',6(',',A))

            DO NodeNum = 1, WaterThermalTank(WaterThermalTankNum)%Nodes
              WRITE(OutputFileInits,724) TRIM(TrimSigDigits(NodeNum)),                            &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Height,4)),          &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Volume,4)),          &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OffCycLossCoeff,4)), &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Inlets)),            &
                TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Outlets))
            END DO
          END IF

          IF (ErrorsFound) THEN
            CALL ShowFatalError('Errors found in getting chilled water tank input. Preceding condition causes termination.')
          END IF
        ENDIF

        ! set up internal gains if tank is in a thermal zone
        IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempZone > 0) THEN
          SELECT CASE (WaterThermalTank(WaterThermalTankNum)%TypeNum)

          CASE (MixedWaterHeater)
            CALL SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum)%AmbientTempZone, &
                                   'WaterHeater:Mixed', &
                                   WaterThermalTank(WaterThermalTankNum)%Name, &
                                   IntGainTypeOf_WaterHeaterMixed, &
                                   ConvectionGainRate = WaterThermalTank(WaterThermalTankNum)%AmbientZoneGain )
          CASE (StratifiedWaterHeater)
            CALL SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum)%AmbientTempZone, &
                                   'WaterHeater:Stratified', &
                                   WaterThermalTank(WaterThermalTankNum)%Name, &
                                   IntGainTypeOf_WaterHeaterStratified, &
                                   ConvectionGainRate = WaterThermalTank(WaterThermalTankNum)%AmbientZoneGain )
          CASE (MixedChilledWaterStorage)
            CALL SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum)%AmbientTempZone, &
                                   'ThermalStorage:ChilledWater:Mixed', &
                                   WaterThermalTank(WaterThermalTankNum)%Name, &
                                   IntGainTypeOf_ThermalStorageChilledWaterMixed, &
                                   ConvectionGainRate = WaterThermalTank(WaterThermalTankNum)%AmbientZoneGain )
          CASE (StratifiedChilledWaterStorage)
            CALL SetupZoneInternalGain(WaterThermalTank(WaterThermalTankNum)%AmbientTempZone, &
                                   'ThermalStorage:ChilledWater:Stratified', &
                                   WaterThermalTank(WaterThermalTankNum)%Name, &
                                   IntGainTypeOf_ThermalStorageChilledWaterStratified, &
                                   ConvectionGainRate = WaterThermalTank(WaterThermalTankNum)%AmbientZoneGain )
          END SELECT

        ENDIF

      END DO ! WaterThermalTankNum
    END IF

  END IF ! get input flag

  IF (ALLOCATED(HPWHSaveNodeNames)) DEALLOCATE(HPWHSaveNodeNames)
  IF (ALLOCATED(WHSaveNodeNames)) DEALLOCATE(WHSaveNodeNames)
  IF (ALLOCATED(CoilSaveNodeNames)) DEALLOCATE(CoilSaveNodeNames)

  RETURN

END SUBROUTINE GetWaterThermalTankInput

SUBROUTINE ValidatePLFCurve(CurveIndex, IsValid)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Validates the Part Load Factor curve by making sure it can never be less than or equal to zero
          ! over the domain of Part Load Ratio inputs from 0 to 1.

          ! METHODOLOGY EMPLOYED:
          ! Currently can only check 0 and 1.  Need changes in CurveManager to be able to check minimums and
          ! maximums.

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue, GetCurveType

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: CurveIndex
  LOGICAL, INTENT(OUT) :: IsValid

          ! FLOW:
  IsValid = .TRUE.

  ! Check 0 and 1
  IF (CurveValue(CurveIndex,0.0d0) <= 0) IsValid = .FALSE.
  IF (CurveValue(CurveIndex,1.0d0) <= 0) IsValid = .FALSE.

  IF (IsValid) THEN  ! Check min/maxs

    SELECT CASE (GetCurveType(CurveIndex))

      CASE ('QUADRATIC')
        ! Curve coeffs are not currently exposed so there's no good way to do this yet

      CASE ('CUBIC')

    END SELECT

  END IF

  RETURN

END SUBROUTINE ValidatePLFCurve


SUBROUTINE SetupStratifiedNodes(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Sets up node properties based on the tank shape, i.e., vertical cylinder, horizontal cylinder, or other.
          ! Node height, skin area, vertical conduction area, and loss coefficients are calculated and assigned.
          ! Heating elements, parasitics, and fluid inlet and outlet flows are assigned according to node height.

          ! METHODOLOGY EMPLOYED:
          ! Tank is divided into nodes of equal mass.  For horizontal cylinders, node heights are calculated using
          ! the Newton-Raphson iterative method.  For vertical cylinders and other shapes, the node heights are calculated
          ! using basic geometry.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: Pi
  USE FluidProperties, ONLY: GetDensityGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum      ! Water Heater being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64)           :: Tolerance = 1.0d-8  ! Tolerance for Newton-Raphson solution
  REAL(r64)           :: FluidCond = 0.6d0     ! Conductivity of water (W/m-K)

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: NumNodes            ! Number of stratified nodes
  INTEGER             :: NodeNum             ! Node number index
  REAL(r64)           :: NodeMass            ! Mass of one node (kg)
  REAL(r64)           :: EndArea             ! Circular area of one end of the cylinder (m2)
  REAL(r64)           :: CrossArea           ! Cross sectional area (for horizontal cylinders) (m2)
  REAL(r64)           :: NodeEndArea         ! Area of the node at the end of the horizontal cylinder (m2)
  REAL(r64)           :: NodeHeight          ! Height of one node (m)
  REAL(r64)           :: ApproxEndArea       ! End area approximated by Newton-Raphson iteration (m2)
  REAL(r64)           :: CondCoeff           ! Coefficient for vertical conduction between nodes (W/K)
  REAL(r64)           :: Radius              ! Radius of the tank (m)
  REAL(r64)           :: Perimeter           ! Perimeter of the tank (m)
  REAL(r64)           :: SkinArea            ! Area of skin exposed to ambient environment (m2)
  REAL(r64)           :: ChordLength         ! Chord length for horizontal tanks (m)
  REAL(r64)           :: TankHeight          ! Dimension in the vertical direction; for horizontal tanks it is radius * 2 (m)
  REAL(r64)           :: TankLength          ! For horizontal tanks, it is the length in the axial direction (m)
  REAL(r64)           :: R                   ! Radius (m)
  REAL(r64)           :: H0                  ! Starting height (m)
  REAL(r64)           :: H                   ! Ending height (m)
  REAL(r64)           :: a, b, c             ! Intermediate variables
  REAL(r64)           :: a0, b0, c0          ! Intermediate variables
  REAL(r64)           :: G                   ! Function that should converge to zero for the Newton-Raphson solution
  REAL(r64)           :: rho                 ! local fluid density (kg/m3)
  INTEGER             :: DummyWaterIndex = 1

          ! FLOW:
  NumNodes = WaterThermalTank(WaterThermalTankNum)%Nodes
  ALLOCATE(WaterThermalTank(WaterThermalTankNum)%Node(NumNodes))

  IF (( WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) .AND. Allocated(PlantLoop)) THEN
    rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                 'GetWaterThermalTankInput')
  ELSE
    rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'GetWaterThermalTankInput')
  ENDIF


  NodeMass = WaterThermalTank(WaterThermalTankNum)%Volume * rho / NumNodes

  ! Mixing rate set to 50% of the max value for dt = 1.0
  WaterThermalTank(WaterThermalTankNum)%InversionMixingRate = NodeMass * 0.5d0 * 1.0d0

  IF ((WaterThermalTank(WaterThermalTankNum)%Shape == TankShapeVertCylinder) &
    .OR. (WaterThermalTank(WaterThermalTankNum)%Shape == TankShapeOther)) THEN

    TankHeight = WaterThermalTank(WaterThermalTankNum)%Height
    EndArea = WaterThermalTank(WaterThermalTankNum)%Volume / TankHeight
    NodeHeight = TankHeight / NumNodes
    CondCoeff = (FluidCond + WaterThermalTank(WaterThermalTankNum)%AdditionalCond)* EndArea / NodeHeight

    IF (WaterThermalTank(WaterThermalTankNum)%Shape == TankShapeVertCylinder) THEN
      Radius = SQRT(EndArea / Pi)
      Perimeter = 2.0d0 * Pi * Radius
    ELSE  ! TankShapeOther
      Perimeter = WaterThermalTank(WaterThermalTankNum)%Perimeter
    END IF

    DO NodeNum = 1, NumNodes
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Mass = NodeMass
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Volume = WaterThermalTank(WaterThermalTankNum)%Volume / NumNodes
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Height = NodeHeight
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%CondCoeffUp = CondCoeff
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%CondCoeffDn = CondCoeff

      IF ((NodeNum == 1) .OR. (NodeNum == NumNodes)) THEN
        SkinArea = Perimeter * NodeHeight + EndArea
      ELSE
        SkinArea = Perimeter * NodeHeight
      END IF

      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OnCycLossCoeff = &
        WaterThermalTank(WaterThermalTankNum)%SkinLossCoeff * SkinArea +   &
           WaterThermalTank(WaterThermalTankNum)%AdditionalLossCoeff(NodeNum)

      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OffCycLossCoeff = &
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OnCycLossCoeff +   &
           WaterThermalTank(WaterThermalTankNum)%OffCycFlueLossCoeff

    END DO  ! NodeNum

    WaterThermalTank(WaterThermalTankNum)%Node(1)%CondCoeffUp = 0.0d0
    WaterThermalTank(WaterThermalTankNum)%Node(NumNodes)%CondCoeffDn = 0.0d0

  ELSE  ! WaterThermalTank(WaterThermalTankNum)%Shape == TankShapeHorizCylinder
    TankLength = WaterThermalTank(WaterThermalTankNum)%Height  ! Height is the length in the axial direction
    EndArea = WaterThermalTank(WaterThermalTankNum)%Volume / TankLength
    Radius = SQRT(EndArea / Pi)
    TankHeight = 2.0d0 * Radius  ! Actual vertical height
    NodeEndArea = EndArea / NumNodes

    R = Radius
    H0 = 0.0d0
    ChordLength = 0.0d0
    DO NodeNum = 1, NumNodes
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Mass = NodeMass
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Volume = WaterThermalTank(WaterThermalTankNum)%Volume / NumNodes

      IF (NodeNum == NumNodes) THEN
        H = TankHeight

      ELSE
        ! Use the Newton-Raphson method to solve the nonlinear algebraic equation for node height
        H = H0 + TankHeight / NumNodes  ! Initial guess

        DO WHILE (.TRUE.)
          a = SQRT(H)
          b = SQRT(2.0d0 * R - H)
          c = 2.0d0 * R * R * ATAN(a / b) - (2.0d0 * R * R - 3.0d0 * H * R + H * H) * (a / b)

          IF (H0 > 0.0d0) THEN
            a0 = SQRT(H0)
            b0 = SQRT(2.0d0 * R - H0)
            c0 = 2.0d0 * R * R * ATAN(a0 / b0) - (2.0d0 * R * R - 3.0d0 * H0 * R + H0 * H0) * (a0 / b0)
          ELSE
            c0 = 0.0d0
          END IF

          ApproxEndArea = c - c0  ! Area approximated by iteration
          G = ApproxEndArea - NodeEndArea  ! G is the function that should converge to zero

          IF (ABS(G) < Tolerance) THEN
            EXIT  ! Converged !!!
          ELSE
            H = H - G / (2.0d0 * a * b)  ! Calculate next guess:  H = Hprev - G/G'
          END IF
        END DO  ! Newton-Raphson

      END IF

      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Height = H - H0

      IF (NodeNum > 1) THEN
        CrossArea = 2.0d0 * ChordLength * TankLength  ! Use old ChordLength from previous node
        CondCoeff = (FluidCond + WaterThermalTank(WaterThermalTankNum)%AdditionalCond)* CrossArea &
          / (0.5d0 * (H - H0) + 0.5d0 * WaterThermalTank(WaterThermalTankNum)%Node(NodeNum - 1)%Height)
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum - 1)%CondCoeffUp = CondCoeff  ! Set for previous node
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%CondCoeffDn = CondCoeff  ! Set for this node
      END IF

      ChordLength = SQRT(2.0d0 * R * H - H * H)  ! Calc new ChordLength to be used with next node

      Perimeter = 2.0d0 * R * (ACOS((R - H)/ R) - ACOS((R - H0)/ R))  ! Segments of circular perimeter
      SkinArea = Perimeter * TankLength + 2.0d0 * NodeEndArea

      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OnCycLossCoeff = &
        WaterThermalTank(WaterThermalTankNum)%SkinLossCoeff * SkinArea +   &
           WaterThermalTank(WaterThermalTankNum)%AdditionalLossCoeff(NodeNum)

      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OffCycLossCoeff = &
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OnCycLossCoeff +   &
           WaterThermalTank(WaterThermalTankNum)%OffCycFlueLossCoeff
        ! Although it doesn't make much sense to have a flue in a horizontal tank, keep it in anyway

      H0 = H
    END DO  ! NodeNum

    WaterThermalTank(WaterThermalTankNum)%Node(1)%CondCoeffUp = 0.0d0
    WaterThermalTank(WaterThermalTankNum)%Node(NumNodes)%CondCoeffDn = 0.0d0
  END IF

  ! Loop through nodes again (from top to bottom this time) and assign heating elements, parasitics, flow inlets/outlets
  ! according to their vertical heights in the tank.
  H0 = TankHeight
  DO NodeNum = 1, NumNodes
    IF (NodeNum == NumNodes) THEN
      H = -1.0d0  ! Avoids rounding errors and ensures that anything at height 0.0 goes into the bottom node
    ELSE
      H = H0 - WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Height
    END IF

    ! Assign heater elements to the nodes at the specified heights
    IF ((WaterThermalTank(WaterThermalTankNum)%HeaterHeight1 <= H0) .AND.   &
       (WaterThermalTank(WaterThermalTankNum)%HeaterHeight1 > H)) THEN
!       sensor node will not get set if user enters 0 for this heater capacity
!       (WaterThermalTank(WaterThermalTankNum)%MaxCapacity > 0.0d0)) THEN
      WaterThermalTank(WaterThermalTankNum)%HeaterNode1 = NodeNum
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MaxCapacity = WaterThermalTank(WaterThermalTankNum)%MaxCapacity
    END IF

    IF ((WaterThermalTank(WaterThermalTankNum)%HeaterHeight2 <= H0) .AND.   &
       (WaterThermalTank(WaterThermalTankNum)%HeaterHeight2 > H)) THEN
!       sensor node will not get set if user enters 0 for this heater capacity
!      .AND. (WaterThermalTank(WaterThermalTankNum)%MaxCapacity2 > 0.0d0)) THEN
      WaterThermalTank(WaterThermalTankNum)%HeaterNode2 = NodeNum

      IF ((NodeNum == WaterThermalTank(WaterThermalTankNum)%HeaterNode1)  &
        .AND. (WaterThermalTank(WaterThermalTankNum)%ControlType == PrioritySimultaneous)) THEN
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MaxCapacity =   &
           WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MaxCapacity  &
          + WaterThermalTank(WaterThermalTankNum)%MaxCapacity2
      ELSE
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MaxCapacity = WaterThermalTank(WaterThermalTankNum)%MaxCapacity2
      END IF
    END IF

    ! Assign parasitic heat gains to the nodes at the specified heights
    IF ((WaterThermalTank(WaterThermalTankNum)%OffCycParaHeight <= H0) .AND.   &
       (WaterThermalTank(WaterThermalTankNum)%OffCycParaHeight > H) ) THEN
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OffCycParaLoad =  &
        WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank * WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad
    END IF

    IF ((WaterThermalTank(WaterThermalTankNum)%OnCycParaHeight <= H0) .AND.   &
       (WaterThermalTank(WaterThermalTankNum)%OnCycParaHeight > H) ) THEN
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OnCycParaLoad =  &
        WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank * WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad
    END IF

    ! Assign inlets and outlets to the nodes at the specified heights
    IF ((WaterThermalTank(WaterThermalTankNum)%UseInletHeight <= H0) .AND.  &
       (WaterThermalTank(WaterThermalTankNum)%UseInletHeight > H)) THEN
      WaterThermalTank(WaterThermalTankNum)%UseInletStratNode = NodeNum

      IF ((WaterThermalTank(WaterThermalTankNum)%UseInletNode > 0) .OR.  &
         (WaterThermalTank(WaterThermalTankNum)%MassFlowRateMax > 0.0d0))  &
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Inlets =   &
           WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Inlets + 1
    END IF

    IF ((WaterThermalTank(WaterThermalTankNum)%UseOutletHeight <= H0) .AND.  &
       (WaterThermalTank(WaterThermalTankNum)%UseOutletHeight > H)) THEN
      WaterThermalTank(WaterThermalTankNum)%UseOutletStratNode = NodeNum

      IF ((WaterThermalTank(WaterThermalTankNum)%UseOutletNode > 0) .OR.   &
         (WaterThermalTank(WaterThermalTankNum)%MassFlowRateMax > 0.0d0))  &
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Outlets =   &
           WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Outlets + 1
    END IF

    IF ((WaterThermalTank(WaterThermalTankNum)%SourceInletHeight <= H0) .AND.  &
       (WaterThermalTank(WaterThermalTankNum)%SourceInletHeight > H)  &
      .AND. (WaterThermalTank(WaterThermalTankNum)%SourceInletNode > 0)) THEN

      WaterThermalTank(WaterThermalTankNum)%SourceInletStratNode = NodeNum
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Inlets = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Inlets + 1
    END IF

    IF ((WaterThermalTank(WaterThermalTankNum)%SourceOutletHeight <= H0) .AND.   &
       (WaterThermalTank(WaterThermalTankNum)%SourceOutletHeight > H)  &
      .AND. (WaterThermalTank(WaterThermalTankNum)%SourceOutletNode > 0)) THEN

      WaterThermalTank(WaterThermalTankNum)%SourceOutletStratNode = NodeNum
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Outlets = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Outlets + 1
    END IF

    H0 = H
  END DO  ! NodeNum

  RETURN

END SUBROUTINE SetupStratifiedNodes


SUBROUTINE InitWaterThermalTank(WaterThermalTankNum, FirstHVACIteration, LoopNum, LoopSideNum )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2004
          !       MODIFIED       FSEC, July 2005
          !                      Brent Griffith, October 2007 indirect fired water heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initialize the water heater, heat pump water heater, or desuperheater heating coil objects during the simulation.
          ! determine flow rates thru use side and source side plant connections (if any)

          ! METHODOLOGY EMPLOYED:
          ! Inlet and outlet nodes are initialized.  Scheduled values are retrieved for the current timestep.

          ! USE STATEMENTS:
  USE DataGlobals,       ONLY: BeginEnvrnFlag, WarmupFlag, AnyPlantInModel
  USE DataInterfaces,    ONLY: ShowSevereError, ShowWarningError, ShowContinueError, ShowContinueErrorTimeStamp
  USE DataLoopNode,      ONLY: Node
  USE DataEnvironment,   ONLY: WaterMainsTemp, OutDryBulbTemp, OutBaroPress
  USE DataHeatBalFanSys, ONLY: MAT
  USE ScheduleManager,   ONLY: GetCurrentScheduleValue
  USE Psychrometrics,    ONLY: RhoH2O, PsyRhoAirFnPbTdbW, PsyWFnTdbRhPb, PsyHFnTdbW, PsyTwbFnTdbWPb, PsyWFnTdbTwbPb
  USE DataHVACGlobals,   ONLY: HPWHInletDBTemp, HPWHInletWBTemp, HPWHCrankcaseDBTemp, NumPlantLoops
  USE DataSizing,        ONLY: AutoSize
  USE InputProcessor,    ONLY: SameString
  USE General,           ONLY: TrimSigDigits, RoundSigDigits
  USE DataZoneEquipment, ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
  USE DataPlant
  USE PlantUtilities,    ONLY: InitComponentNodes, SetComponentFlowRate, InterConnectTwoPlantLoopSides
  USE FluidProperties,   ONLY: GetDensityGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration
  INTEGER, INTENT(IN), OPTIONAL :: LoopNum
  INTEGER, INTENT(IN), OPTIONAL :: LoopSideNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: UseInletNode            ! Water heater use inlet node number
  INTEGER             :: UseOutletNode           ! Water heater use outlet node number
  INTEGER             :: SourceInletNode         ! Water heater source inlet node number
  INTEGER             :: SourceOutletNode        ! Water heater source outlet node number
  INTEGER             :: SchIndex                ! Index to schedule
  INTEGER             :: HPNum                   ! Index to heat pump
  INTEGER             :: HPAirInletNode          ! HP air inlet node number
  INTEGER             :: HPAirOutletNode         ! HP air outlet node number
  INTEGER             :: OutdoorAirNode          ! Outdoor air inlet node number
  INTEGER             :: ExhaustAirNode          ! Exhaust air outlet node number
  INTEGER             :: HPWaterInletNode        ! HP condenser water inlet node number
  INTEGER             :: HPWaterOutletNode       ! HP condenser water outlet node number
  INTEGER             :: InletAirMixerNode       ! HP inlet node number after inlet mixing damper
  INTEGER             :: OutletAirSplitterNode   ! HP outlet node number before outlet mixing damper
  REAL(r64)           :: HPInletDryBulbTemp      ! HPWH's air inlet dry-bulb temperature, C
  REAL(r64)           :: HPInletHumRat           ! HPWH's air inlet humidity ratio, kg/kg
  REAL(r64)           :: HPInletRelHum           ! HPWH's air inlet relative humidity
  REAL(r64)           :: DeadBandTemp                 ! Minimum tank temperature (SetpointTemp - DeadbandDeltaTemp) (C)
!  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
!  Integer             :: Loop
  LOGICAL,SAVE        :: InitWaterThermalTanksOnce = .TRUE. ! flag for 1 time initialization
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! flag for init once at start of environment
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyWarmupFlag ! flag for init after warmup complete
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SetLoopIndexFlag    ! get loop number flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizingDoneFlag ! true if sizing is finished

  REAL(r64)           :: sensedTemp
  INTEGER             :: tmpNodeNum
  REAL(r64)           :: mdotUse ! local temporary for use side mass flow
  REAL(r64)           :: mdotSource ! local temporary for source side mass flow
  LOGICAL             :: errFlag
  REAL(r64)           :: rho ! local fluid density
  INTEGER             :: DummyWaterIndex = 1
  INTEGER             :: found = 0
  REAL(r64)           :: TankChangeRateScale = 0.d0 ! local temporary for nominal tank change rate
  REAL(r64)           :: MaxSideVolFlow = 0.d0 ! local temporary for largest connection design flow

          ! FLOW:

  If (InitWaterThermalTanksOnce) then
    ALLOCATE (MyEnvrnFlag(  NumWaterThermalTank  ))
    ALLOCATE (MyWarmupFlag( NumWaterThermalTank  ))
    ALLOCATE (SetLoopIndexFlag( NumWaterThermalTank ))
    ALLOCATE (MySizingDoneFlag( NumWaterThermalTank ))
    ALLOCATE(AlreadyRated(NumWaterThermalTank))
    AlreadyRated = .FALSE.
    MyEnvrnFlag = .TRUE.
    MyWarmupFlag = .FALSE.
    InitWaterThermalTanksOnce = .FALSE.
    SetLoopIndexFlag = .TRUE.
    MySizingDoneFlag = .FALSE.
  END IF

  UseInletNode     = WaterThermalTank(WaterThermalTankNum)%UseInletNode
  UseOutletNode    = WaterThermalTank(WaterThermalTankNum)%UseOutletNode
  SourceInletNode  = WaterThermalTank(WaterThermalTankNum)%SourceInletNode
  SourceOutletNode = WaterThermalTank(WaterThermalTankNum)%SourceOutletNode

  IF(SetLoopIndexFlag(WaterThermalTankNum) .AND. ALLOCATED(PlantLoop) )THEN

    IF ((UseInletNode > 0 ) .AND. (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum == 0))  THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject(WaterThermalTank(WaterThermalTankNum)%Name, &
                                   WaterThermalTank(WaterThermalTankNum)%TypeNum, &
                                   WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum,   &
                                   WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide,  &
                                   WaterThermalTank(WaterThermalTankNum)%UseSidePlantBranchNum, &
                                   WaterThermalTank(WaterThermalTankNum)%UseSidePlantCompNum,   &
                                   InletNodeNumber = UseInletNode,                              &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitWaterThermalTank: Program terminated due to previous condition(s).')
      ENDIF
      rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                 'GetWaterThermalTankInput')
      WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax = &
                             WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate * rho
      WaterThermalTank(WaterThermalTankNum)%Mass = WaterThermalTank(WaterThermalTankNum)%Volume * rho
            WaterThermalTank(WaterThermalTankNum)%UseSidePlantSizNum  = &
                      PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%PlantSizNum
      IF ((WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate == AutoSize) .AND. &
          (WaterThermalTank(WaterThermalTankNum)%UseSidePlantSizNum   == 0) ) THEN
        CALL ShowSevereError('InitWaterThermalTank: Did not find Sizing:Plant object for use side of plant thermal tank = ' &
                             //TRIM(WaterThermalTank(WaterThermalTankNum)%Name) )
        CALL ShowFatalError('InitWaterThermalTank: Program terminated due to previous condition(s).')
      ENDIF
    ENDIF
    IF ((UseInletNode > 0 ) .AND. (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum > 0))  THEN
      ! this is a heat pump water heater, need a separate block because TypeOf_HeatPumpWtrHeater shows up on Branch
      !  (input should probably have been the associated tank )
      errFlag=.false.
      CALL ScanPlantLoopsForObject(HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Name, &
                                   TypeOf_HeatPumpWtrHeater, &
                                   WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum,   &
                                   WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide,  &
                                   WaterThermalTank(WaterThermalTankNum)%UseSidePlantBranchNum, &
                                   WaterThermalTank(WaterThermalTankNum)%UseSidePlantCompNum,   &
                                   InletNodeNumber = UseInletNode,                              &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitWaterThermalTank: Program terminated due to previous condition(s).')
      ENDIF
      rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                 'GetWaterThermalTankInput')
      WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax = &
                             WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate * rho
      WaterThermalTank(WaterThermalTankNum)%Mass = WaterThermalTank(WaterThermalTankNum)%Volume * rho
      WaterThermalTank(WaterThermalTankNum)%UseSidePlantSizNum  = &
                      PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%PlantSizNum
      IF ((WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate == AutoSize) .AND. &
          (WaterThermalTank(WaterThermalTankNum)%UseSidePlantSizNum   == 0) ) THEN
        CALL ShowSevereError('InitWaterThermalTank: Did not find Sizing:Plant object for use side of plant thermal tank = ' &
                             //TRIM(WaterThermalTank(WaterThermalTankNum)%Name) )
        CALL ShowFatalError('InitWaterThermalTank: Program terminated due to previous condition(s).')
      ENDIF
    ENDIF
    IF ((SourceInletNode > 0) .AND. (WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum == 0) &
         .AND. (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum == 0)) THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject(WaterThermalTank(WaterThermalTankNum)%Name, &
                                   WaterThermalTank(WaterThermalTankNum)%TypeNum, &
                                   WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum,   &
                                   WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide,  &
                                   WaterThermalTank(WaterThermalTankNum)%SourceSidePlantBranchNum, &
                                   WaterThermalTank(WaterThermalTankNum)%SourceSidePlantCompNum,   &
                                   InletNodeNumber = SourceInletNode,                              &
                                   errFlag=errFlag)
      IF (UseInletNode > 0 ) THEN
        CALL InterConnectTwoPlantLoopSides( WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum, &
                                            WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide, &
                                            WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum, &
                                            WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide, &
                                            WaterThermalTank(WaterThermalTankNum)%TypeNum , .TRUE. )
      ENDIF

      IF (errFlag) THEN
        CALL ShowFatalError('InitWaterThermalTank: Program terminated due to previous condition(s).')
      ENDIF
      rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidName, &
                               InitConvTemp, &
                               PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidIndex, &
                               'GetWaterThermalTankInput')
      WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax = &
                           WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate * rho
      WaterThermalTank(WaterThermalTankNum)%SourceSidePlantSizNum  = &
                      PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%PlantSizNum
      IF ((WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate == AutoSize) .AND. &
          (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantSizNum   == 0) ) THEN
        CALL ShowSevereError('InitWaterThermalTank: Did not find Sizing:Plant object for source side of plant thermal tank = ' &
                             //TRIM(WaterThermalTank(WaterThermalTankNum)%Name) )
        CALL ShowFatalError('InitWaterThermalTank: Program terminated due to previous condition(s).')
      ENDIF
    ENDIF
    IF ((SourceInletNode > 0) .AND. (WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum > 0) &
         .OR. (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum > 0)) THEN
      SetLoopIndexFlag(WaterThermalTankNum) = .FALSE.
    ENDIF

    IF (PlantSizesOkayToFinalize) SetLoopIndexFlag(WaterThermalTankNum) = .FALSE.
    IF (WaterThermalTank(WaterThermalTankNum)%StandAlone) THEN
      CALL SizeStandAloneWaterHeater(WaterThermalTankNum)
      SetLoopIndexFlag(WaterThermalTankNum) = .FALSE.
    ENDIF

  ELSEIF( SetLoopIndexFlag(WaterThermalTankNum) .AND. .NOT. AnyPlantInModel) THEN
    IF (WaterThermalTank(WaterThermalTankNum)%StandAlone) THEN
      CALL SizeStandAloneWaterHeater(WaterThermalTankNum)
    ENDIF
 
    CALL CalcStandardRatings(WaterThermalTankNum)
    SetLoopIndexFlag(WaterThermalTankNum) = .FALSE.
  ENDIF

  IF (WaterThermalTank(WaterThermalTankNum)%StandAlone   .AND. (.NOT. AlreadyRated(WaterThermalTankNum))) THEN
    CALL CalcStandardRatings(WaterThermalTankNum)
  ENDIF

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(WaterThermalTankNum) .AND. .NOT. SetLoopIndexFlag(WaterThermalTankNum)) THEN

    IF (.NOT. MySizingDoneFlag(WaterThermalTankNum)) THEN
!      IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0 ) THEN
!        WaterThermalTank(WaterThermalTankNum)%UseSidePlantSizNum  = &
!                      PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%PlantSizNum
!      ENDIF
!      IF (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum > 0) THEN
!      WaterThermalTank(WaterThermalTankNum)%SourceSidePlantSizNum  = &
!                      PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%PlantSizNum
!      ENDIF
!   !   CALL MinePlantStructForInfo(WaterThermalTankNum)
      CALL SizeTankForDemandSide(WaterThermalTankNum)
      CALL SizeDemandSidePlantConnections(WaterThermalTankNum)
      IF (PRESENT(LoopNum)) THEN
        CALL SizeSupplySidePlantConnections(WaterThermalTankNum, LoopNum, LoopSideNum)
      ELSE
        CALL SizeSupplySidePlantConnections(WaterThermalTankNum)
      ENDIF

      CALL SizeTankForSupplySide(WaterThermalTankNum)
!
      IF (PlantSizesOkayToFinalize) THEN
        ! check to see if any autosize values left, depending on the nature of this tank
        IF (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum > 0 ) THEN ! this is heat pump water heater, source side not sized
          IF (WaterThermalTank(WaterThermalTankNum)%UseInletNode > 0) THEN
            IF ( (WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    /= AutoSize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Volume                  /= AutoSize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%MaxCapacity             /= Autosize) ) THEN
              MySizingDoneFlag(WaterThermalTankNum) = .TRUE.
            ENDIF
          ELSE
            IF ( (WaterThermalTank(WaterThermalTankNum)%Volume                  /= AutoSize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%MaxCapacity             /= Autosize) ) THEN
              MySizingDoneFlag(WaterThermalTankNum) = .TRUE.
            ENDIF
          ENDIF
        ELSE ! not a heat pump water heater
          IF ((WaterThermalTank(WaterThermalTankNum)%UseInletNode > 0) .AND. &
              (WaterThermalTank(WaterThermalTankNum)%SourceInletNode > 0 ) ) THEN
            IF ( (WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate /= AutoSize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    /= AutoSize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Volume                  /= AutoSize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Height                  /= Autosize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%MaxCapacity             /= Autosize) ) THEN
              MySizingDoneFlag(WaterThermalTankNum) = .TRUE.
            ENDIF
          ELSEIF ((WaterThermalTank(WaterThermalTankNum)%UseInletNode > 0) .AND. &
                   (WaterThermalTank(WaterThermalTankNum)%SourceInletNode == 0 ) ) THEN
            IF ( (WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    /= AutoSize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Volume                  /= AutoSize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Height                  /= Autosize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%MaxCapacity             /= Autosize) ) THEN
              MySizingDoneFlag(WaterThermalTankNum) = .TRUE.
            ENDIF
          ELSEIF ((WaterThermalTank(WaterThermalTankNum)%UseInletNode == 0) .AND. &
                  (WaterThermalTank(WaterThermalTankNum)%SourceInletNode == 0 ) ) THEN
            IF ( (WaterThermalTank(WaterThermalTankNum)%Volume                  /= AutoSize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%Height                  /= Autosize) .AND. &
                 (WaterThermalTank(WaterThermalTankNum)%MaxCapacity             /= Autosize) ) THEN
              MySizingDoneFlag(WaterThermalTankNum) = .TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      IF (.NOT. MySizingDoneFlag(WaterThermalTankNum)) THEN
        IF ((WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum > 0) &
           .OR. (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum > 0)) THEN
          IF ( (WaterThermalTank(WaterThermalTankNum)%UseInletNode            == 0)        .AND. &
               (WaterThermalTank(WaterThermalTankNum)%Volume                  /= AutoSize) .AND. &
               (WaterThermalTank(WaterThermalTankNum)%MaxCapacity             /= Autosize) ) THEN
              MySizingDoneFlag(WaterThermalTankNum) = .TRUE.
          ELSE
            RETURN
          ENDIF
        ELSE
          RETURN
        ENDIF
      ENDIF

      IF ((WaterThermalTank(WaterThermalTankNum)%ControlType == ControlTypeCycle) .AND. MySizingDoneFlag(WaterThermalTankNum)) THEN
        WaterThermalTank(WaterThermalTankNum)%MinCapacity = WaterThermalTank(WaterThermalTankNum)%MaxCapacity
      ENDIF

      ! check for sizing issues that model can not suppport

      ! if stratified tank model, ensure that nominal change over rate is greater than one minute, avoid numerical problems.

      IF (    (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedWaterHeater)  &
         .OR. (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedChilledWaterStorage)) THEN
        MaxSideVolFlow  = MAX( WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate, &
                               WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate)

        IF (MaxSideVolFlow > 0.d0) THEN ! protect div by zero
          TankChangeRateScale = WaterThermalTank(WaterThermalTankNum)%Volume / MaxSideVolFlow
          IF (TankChangeRateScale < 60.d0) THEN  ! nominal change over in less than one minute
            CALL ShowSevereError('InitWaterThermalTank: Detected problem for stratified tank model.  Model cannot be applied.')
            CALL ShowContinueError('Occurs for stratified tank name = ' //TRIM(WaterThermalTank(WaterThermalTankNum)%Name) )
            CALL ShowContinueError('Tank volume = '//TRIM(RoundSigDigits(WaterThermalTank(WaterThermalTankNum)%Volume, 4))//' [m3]')
            CALL ShowContinueError('Tank use side volume flow rate = ' &
                              //TRIM(RoundSigDigits(WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate, 4))//' [m3/s]' )
            CALL ShowContinueError('Tank source side volume flow rate = ' &
                              //TRIM(RoundSigDigits(WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate, 4))//' [m3/s]' )
            CALL ShowContinueError('Nominal tank change over rate = '//TRIM(RoundSigDigits(TankChangeRateScale, 2))//' [s]')
            CALL ShowContinueError('Change over rate is too fast, increase tank volume, decrease connection flow rates' &
                              //' or use mixed tank model')

            CALL ShowFatalError('InitWaterThermalTank: Simulation halted because of sizing problem in stratified tank model.')
          ENDIF
        ENDIF
      ENDIF

    ENDIF


    ! Clear node initial conditions
    IF (UseInletNode > 0 .AND. UseOutletNode > 0 ) THEN
      Node(UseInletNode)%Temp = 0.d0
      rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                 'GetWaterThermalTankInput')
      WaterThermalTank(WaterThermalTankNum)%MassFlowRateMin = WaterThermalTank(WaterThermalTankNum)%VolFlowRateMin * rho
      WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax = &
                             WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate * rho
      CALL InitComponentNodes(WaterThermalTank(WaterThermalTankNum)%MassFlowRateMin, &
                              WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax, &
                              UseInletNode, UseOutletNode, &
                              WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum, &
                              WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide, &
                              WaterThermalTank(WaterThermalTankNum)%UseSidePlantBranchNum, &
                              WaterThermalTank(WaterThermalTankNum)%UseSidePlantCompNum )
      WaterThermalTank(WaterThermalTankNum)%UseOutletTemp      = 0.d0
      WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate    = 0.d0
      WaterThermalTank(WaterThermalTankNum)%SavedUseOutletTemp = 0.d0

      WaterThermalTank(WaterThermalTankNum)%Mass = WaterThermalTank(WaterThermalTankNum)%Volume * rho
      WaterThermalTank(WaterThermalTankNum)%UseBranchControlType = &
                       PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)% &
                        LoopSide(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide)% &
                          Branch(WaterThermalTank(WaterThermalTankNum)%UseSidePlantBranchNum)% &
                            Comp(WaterThermalTank(WaterThermalTankNum)%UseSidePlantCompNum )%FlowCtrl

    END IF

    IF ((SourceInletNode > 0) .AND. (WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum == 0) &
         .AND. (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum == 0)) THEN
      rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidIndex, &
                                 'GetWaterThermalTankInput')
      WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax = &
                             WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate * rho
      CALL InitComponentNodes(0.d0, WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax, &
                              SourceInletNode, SourceOutletNode, &
                              WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum, &
                              WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide, &
                              WaterThermalTank(WaterThermalTankNum)%SourceSidePlantBranchNum, &
                              WaterThermalTank(WaterThermalTankNum)%SourceSidePlantCompNum )

      WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp      = 0.d0
      WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate    = 0.d0
      WaterThermalTank(WaterThermalTankNum)%SavedSourceOutletTemp = 0.d0

      WaterThermalTank(WaterThermalTankNum)%SourceBranchControlType = &
                       PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)% &
                        LoopSide(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide)% &
                          Branch(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantBranchNum)% &
                            Comp(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantCompNum )%FlowCtrl
    END IF

    IF ((SourceInletNode > 0) .AND. ((WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum > 0) &
         .OR. (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum > 0))) THEN
      Node(SourceInletNode)%Temp = 0.d0
      WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp      = 0.d0
      WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate    = 0.d0
      WaterThermalTank(WaterThermalTankNum)%SavedSourceOutletTemp = 0.d0
      rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'SizeTankForDemandSide')
      WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax = &
                             WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate * rho
    ENDIF

    ! Initialize tank temperature to setpoint of first hour of warm up period
    ! (use HPWH or Desuperheater heating coil set point if applicable)
    IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0)THEN
      HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Mode = FloatMode
      HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SaveMode  = FloatMode
      HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SaveWHMode = FloatMode
      SchIndex = HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SetpointTempSchedule
    ELSE IF(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum .GT. 0)THEN
      WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Mode = FloatMode
      SchIndex= WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%SetpointTempSchedule
    ELSE
      SchIndex = WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule
    END IF

    IF (SchIndex > 0) THEN
      WaterThermalTank(WaterThermalTankNum)%TankTemp = GetCurrentScheduleValue(SchIndex)
      WaterThermalTank(WaterThermalTankNum)%SavedTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp

      IF (WaterThermalTank(WaterThermalTankNum)%Nodes > 0) THEN
        WaterThermalTank(WaterThermalTankNum)%Node%Temp = WaterThermalTank(WaterThermalTankNum)%TankTemp
        WaterThermalTank(WaterThermalTankNum)%Node%SavedTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
      END IF
    ELSE
      WaterThermalTank(WaterThermalTankNum)%TankTemp = 20.0d0
      WaterThermalTank(WaterThermalTankNum)%SavedTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp

      IF (WaterThermalTank(WaterThermalTankNum)%Nodes > 0) THEN
        WaterThermalTank(WaterThermalTankNum)%Node%Temp = WaterThermalTank(WaterThermalTankNum)%TankTemp
        WaterThermalTank(WaterThermalTankNum)%Node%SavedTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
      END IF
    END IF
    WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp      = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
    WaterThermalTank(WaterThermalTankNum)%SavedSourceOutletTemp = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
    WaterThermalTank(WaterThermalTankNum)%UseOutletTemp         = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
    WaterThermalTank(WaterThermalTankNum)%SavedUseOutletTemp    = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
    WaterThermalTank(WaterThermalTankNum)%TankTempAvg           = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp

    WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn1 = .FALSE.
    WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn2 = .FALSE.
    WaterThermalTank(WaterThermalTankNum)%Mode           = 0
    WaterThermalTank(WaterThermalTankNum)%SavedMode      = 0
    WaterThermalTank(WaterThermalTankNum)%FirstRecoveryDone = .FALSE.
    WaterThermalTank(WaterThermalTankNum)%FirstRecoveryFuel = 0.d0
    WaterThermalTank(WaterThermalTankNum)%UnmetEnergy    = 0.d0
    WaterThermalTank(WaterThermalTankNum)%LossEnergy     = 0.d0
    WaterThermalTank(WaterThermalTankNum)%FlueLossEnergy = 0.d0
    WaterThermalTank(WaterThermalTankNum)%UseEnergy      = 0.d0
    WaterThermalTank(WaterThermalTankNum)%TotalDemandEnergy = 0.d0
    WaterThermalTank(WaterThermalTankNum)%SourceEnergy   = 0.d0
    WaterThermalTank(WaterThermalTankNum)%HeaterEnergy   = 0.d0
    WaterThermalTank(WaterThermalTankNum)%HeaterEnergy1  = 0.d0
    WaterThermalTank(WaterThermalTankNum)%HeaterEnergy2  = 0.d0
    WaterThermalTank(WaterThermalTankNum)%FuelEnergy     = 0.d0
    WaterThermalTank(WaterThermalTankNum)%FuelEnergy1    = 0.d0
    WaterThermalTank(WaterThermalTankNum)%FuelEnergy2    = 0.d0
    WaterThermalTank(WaterThermalTankNum)%VentEnergy     = 0.d0
    WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelEnergy = 0.d0
    WaterThermalTank(WaterThermalTankNum)%OffCycParaEnergyToTank = 0.d0
    WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelEnergy = 0.d0
    WaterThermalTank(WaterThermalTankNum)%OnCycParaEnergyToTank = 0.d0
    WaterThermalTank(WaterThermalTankNum)%NetHeatTransferEnergy = 0.d0

    IF ((WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate == Autosize) .or. &
        (WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate ==  Autosize) ) THEN
      MyEnvrnFlag(WaterThermalTankNum)  = .TRUE.
      MyWarmupFlag(WaterThermalTankNum) = .FALSE.
    ELSE
      MyEnvrnFlag(WaterThermalTankNum)  = .FALSE.
      MyWarmupFlag(WaterThermalTankNum) = .TRUE.
    ENDIF
  END IF

  IF (.NOT. BeginEnvrnFlag)  MyEnvrnFlag(WaterThermalTankNum) = .TRUE.

  IF ( MyWarmupFlag(WaterThermalTankNum) .and. (.not. WarmUpFlag)) then
      ! reInitialize tank temperature to setpoint of first hour (use HPWH or Desuperheater heating coil set point if applicable)
      ! BG's interpetation here is that its better to reset initial condition to setpoint once warm up is over.
      ! (otherwise with a dynamic storage model it is difficult for the user to see the initial performance if it isn't periodic.)
    IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0)THEN
      HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Mode = FloatMode
      SchIndex = HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SetpointTempSchedule
    ELSE IF(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum .GT. 0)THEN
      WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%Mode = FloatMode
      SchIndex = WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%SetpointTempSchedule
    ELSE
      SchIndex = WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule
    END IF

    IF (SchIndex > 0) THEN
      WaterThermalTank(WaterThermalTankNum)%TankTemp = GetCurrentScheduleValue(SchIndex)
      WaterThermalTank(WaterThermalTankNum)%SavedTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp

      IF (WaterThermalTank(WaterThermalTankNum)%Nodes > 0) THEN
        WaterThermalTank(WaterThermalTankNum)%Node%Temp = WaterThermalTank(WaterThermalTankNum)%TankTemp
        WaterThermalTank(WaterThermalTankNum)%Node%SavedTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
      END IF
    ELSE
      WaterThermalTank(WaterThermalTankNum)%TankTemp = 20.0d0
      WaterThermalTank(WaterThermalTankNum)%SavedTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp

      IF (WaterThermalTank(WaterThermalTankNum)%Nodes > 0) THEN
        WaterThermalTank(WaterThermalTankNum)%Node%Temp = WaterThermalTank(WaterThermalTankNum)%TankTemp
        WaterThermalTank(WaterThermalTankNum)%Node%SavedTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
      END IF
    END IF
    WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp      = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
    WaterThermalTank(WaterThermalTankNum)%SavedSourceOutletTemp = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
    WaterThermalTank(WaterThermalTankNum)%UseOutletTemp         = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
    WaterThermalTank(WaterThermalTankNum)%SavedUseOutletTemp    = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
    WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn1 = .FALSE.
    WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn2 = .FALSE.
    WaterThermalTank(WaterThermalTankNum)%Mode           = 0
    WaterThermalTank(WaterThermalTankNum)%SavedMode      = 0
    MyWarmupFlag(WaterThermalTankNum) = .false.


  END IF
  IF (WarmUpFlag)  MyWarmupFlag(WaterThermalTankNum) = .TRUE.

  IF (FirstHVACIteration) THEN
    ! Get all scheduled values
    SchIndex = WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule
    WaterThermalTank(WaterThermalTankNum)%SetpointTemp = GetCurrentScheduleValue(SchIndex)

    IF (.NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank) Then
      IF (WaterThermalTank(WaterThermalTankNum)%SetpointTemp > WaterThermalTank(WaterThermalTankNum)%TankTempLimit) THEN
        ! Setpoint temperature scheduled higher than maximum tank temperature limit
        WaterThermalTank(WaterThermalTankNum)%SetpointTemp = WaterThermalTank(WaterThermalTankNum)%TankTempLimit - 1.0d0

        IF(WaterThermalTank(WaterThermalTankNum)%ShowSetpointWarning)THEN
          CALL ShowSevereError('Water heater = '//TRIM(WaterThermalTank(WaterThermalTankNum)%Name)// &
            ':  Water heater tank set point temperature is greater than the maximum tank temperature limit.')
          CALL ShowContinueErrorTimeStamp(' Water heater tank set point temperature is reset to Tank Temperature'// &
                                 ' Limit minus 1 C ('//TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%SetpointTemp,2))// &
                                 ') and simulation continues. ')
          WaterThermalTank(WaterThermalTankNum)%ShowSetpointWarning = .FALSE.
        END IF

      END IF
    ELSE
      IF (WaterThermalTank(WaterThermalTankNum)%SetpointTemp < WaterThermalTank(WaterThermalTankNum)%TankTempLimit) THEN
        ! Setpoint temperature scheduled lower than minimum tank temperature limit
        WaterThermalTank(WaterThermalTankNum)%SetpointTemp = WaterThermalTank(WaterThermalTankNum)%TankTempLimit + 1.0d0

        IF(WaterThermalTank(WaterThermalTankNum)%ShowSetpointWarning)THEN
          CALL ShowSevereError('Chilled Water Tank = '//TRIM(WaterThermalTank(WaterThermalTankNum)%Name)// &
            ':  Water heater tank set point temperature is lower than the minimum tank temperature limit.')
          CALL ShowContinueErrorTimeStamp(' Chilled water tank set point temperature is reset to Tank Temperature'// &
                                 ' Limit plus 1 C ('//TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%SetpointTemp,2))// &
                                 ') and simulation continues. ')
          WaterThermalTank(WaterThermalTankNum)%ShowSetpointWarning = .FALSE.
        END IF

      END IF
    ENDIF

    SchIndex = WaterThermalTank(WaterThermalTankNum)%SetpointTempSchedule2
    IF (SchIndex > 0) THEN
      WaterThermalTank(WaterThermalTankNum)%SetpointTemp2 = GetCurrentScheduleValue(SchIndex)
    END IF

    SELECT CASE (WaterThermalTank(WaterThermalTankNum)%AmbientTempIndicator)
      CASE (AmbientTempSchedule)
        SchIndex = WaterThermalTank(WaterThermalTankNum)%AmbientTempSchedule
        WaterThermalTank(WaterThermalTankNum)%AmbientTemp = GetCurrentScheduleValue(SchIndex)

      CASE (AmbientTempZone)
        WaterThermalTank(WaterThermalTankNum)%AmbientTemp = MAT(WaterThermalTank(WaterThermalTankNum)%AmbientTempZone)

      CASE (AmbientTempOutsideAir)
        WaterThermalTank(WaterThermalTankNum)%AmbientTemp =   &
           Node(WaterThermalTank(WaterThermalTankNum)%AmbientTempOutsideAirNode)%Temp

    END SELECT

    IF (UseInletNode == 0) THEN ! Stand-alone operation

      SchIndex = WaterThermalTank(WaterThermalTankNum)%UseInletTempSchedule
      IF (SchIndex > 0) THEN
        WaterThermalTank(WaterThermalTankNum)%UseInletTemp = GetCurrentScheduleValue(SchIndex)
      ELSE
        WaterThermalTank(WaterThermalTankNum)%UseInletTemp = WaterMainsTemp
      END IF

      SchIndex = WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule
      IF (SchIndex > 0) THEN
        WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate = GetCurrentScheduleValue(SchIndex)*  &
                               WaterThermalTank(WaterThermalTankNum)%MassFlowRateMax


        WaterThermalTank(WaterThermalTankNum)%VolFlowRate =   &
           WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate / RhoH2O(InitConvTemp)
      ELSE
        WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate =   &
           WaterThermalTank(WaterThermalTankNum)%MassFlowRateMax
        WaterThermalTank(WaterThermalTankNum)%VolFlowRate =   &
           WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate / RhoH2O(InitConvTemp)
      END IF

    END IF

    IF (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0) THEN
      HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SetpointTemp = &
          GetCurrentScheduleValue(HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SetpointTempSchedule)
      IF (HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SetpointTemp .GE. &
          WaterThermalTank(WaterThermalTankNum)%TankTempLimit) THEN
        ! HP setpoint temperature scheduled equal to or higher than tank temperature limit
        HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SetpointTemp = &
           WaterThermalTank(WaterThermalTankNum)%TankTempLimit - 1.0d0

        IF(HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%ShowSetpointWarning)THEN
          CALL ShowSevereError('Heat Pump Water Heater = '// &
                       TRIM(HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Name)// &
            ':  Heat Pump water heater set point temperature is equal to or greater than the maximum tank temperature limit.')
          CALL ShowContinueErrorTimeStamp(' Heat Pump water heater tank set point temperature is reset to Tank Temperature'// &
              ' Limit minus 1 C ('// &
              TRIM(TrimSigDigits(HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%SetpointTemp,2))// &
              ') and simulation continues. ')
          HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%ShowSetpointWarning = .FALSE.
        END IF

      END IF
    END IF

    IF (WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum .GT. 0) THEN
      WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%SetpointTemp = &
          GetCurrentScheduleValue(  &
             WaterHeaterDesuperheater(WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum)%SetpointTempSchedule)
    END IF


  ENDIF ! first HVAC Iteration

  IF (UseInletNode > 0 .AND. .NOT. SetLoopIndexFlag(WaterThermalTankNum)) THEN ! setup mass flows for plant connections

    IF (WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank) Then
      DeadBandTemp = WaterThermalTank(WaterThermalTankNum)%SetpointTemp + WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp
    ELSE
      DeadBandTemp = WaterThermalTank(WaterThermalTankNum)%SetpointTemp - WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp
    ENDIF

    mdotUse = PlantMassFlowRatesFunc(WaterThermalTankNum, UseInletNode, &
                                        FirstHVACIteration, UseSide,    &
                                        WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide, &
                                        WaterThermalTank(WaterThermalTankNum)%UseSideSeries,        &
                                        WaterThermalTank(WaterThermalTankNum)%UseBranchControlType, &
                                        WaterThermalTank(WaterThermalTankNum)%SavedUseOutletTemp,   &
                                        DeadBandTemp, WaterThermalTank(WaterThermalTankNum)%SetpointTemp)
    CALL SetComponentFlowRate(mdotUse, UseInletNode, UseOutletNode,         &
                                        WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum,   &
                                        WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide,  &
                                        WaterThermalTank(WaterThermalTankNum)%UseSidePlantBranchNum, &
                                        WaterThermalTank(WaterThermalTankNum)%UseSidePlantCompNum)


    WaterThermalTank(WaterThermalTankNum)%UseInletTemp = Node(UseInletNode)%Temp
    WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate = mdotUse


  ENDIF

  IF (SourceInletNode > 0.AND. .NOT. SetLoopIndexFlag(WaterThermalTankNum)) THEN ! setup mass flows for plant connections

    IF (WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank) Then
      DeadBandTemp = WaterThermalTank(WaterThermalTankNum)%SetpointTemp + WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp
    ELSE
      DeadBandTemp = WaterThermalTank(WaterThermalTankNum)%SetpointTemp - WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp
    ENDIF

    IF (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedChilledWaterStorage) THEN
      tmpNodeNum = WaterThermalTank(WaterThermalTankNum)%HeaterNode1
      sensedTemp = WaterThermalTank(WaterThermalTankNum)%Node(tmpNodeNum)%SavedTemp
    ELSE
      sensedTemp = WaterThermalTank(WaterThermalTankNum)%SavedSourceOutletTemp
    ENDIF

    mdotSource = PlantMassFlowRatesFunc(WaterThermalTankNum, SourceInletNode, &
                                           FirstHVACIteration, SourceSide, &
                                           WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide, &
                                           WaterThermalTank(WaterThermalTankNum)%SourceSideSeries, &
                                           WaterThermalTank(WaterThermalTankNum)%SourceBranchControlType, &
                                           sensedTemp, &
                                           DeadBandTemp, WaterThermalTank(WaterThermalTankNum)%SetpointTemp)
    IF (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum > 0) THEN
      CALL SetComponentFlowRate(mdotSource, SourceInletNode, SourceOutletNode,         &
                                        WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum,   &
                                        WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide,  &
                                        WaterThermalTank(WaterThermalTankNum)%SourceSidePlantBranchNum, &
                                        WaterThermalTank(WaterThermalTankNum)%SourceSidePlantCompNum)
    ELSE !not really plant connected (desuperheater or heat pump)
      Node(SourceInletNode)%MassFLowRate  = mdotSource
      Node(SourceOutletNode)%MassFLowRate = mdotSource

    ENDIF

    WaterThermalTank(WaterThermalTankNum)%SourceInletTemp = Node(SourceInletNode)%Temp
    WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = mdotSource

  ENDIF
!

! initialize HPWHs each iteration
  IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0)THEN

    HPNum = WaterThermalTank(WaterThermalTankNum)%HeatPumpNum

    IF(MyHPSizeFlag(HPNum))THEN
!     autosize info must be calculated in GetWaterThermalTankInputFlag for use in StandardRating procedure
!       (called at end of GetWaterThermalTankInputFlag)
!     report autosizing information here (must be done after GetWaterThermalTankInputFlag is complete)
      IF(HPWaterHeater(HPNum)%WaterFlowRateAutosized)THEN
        CALL ReportSizingOutput(HPWaterHeater(HPNum)%Type, HPWaterHeater(HPNum)%Name, &
            'Condenser water flow rate [m3/s]', HPWaterHeater(HPNum)%OperatingWaterFlowRate)
      END IF
      IF(HPWaterHeater(HPNum)%AirFlowRateAutosized)THEN
        CALL ReportSizingOutput(HPWaterHeater(HPNum)%Type, HPWaterHeater(HPNum)%Name, &
            'Evaporator air flow rate [m3/s]', HPWaterHeater(HPNum)%OperatingAirFlowRate)
      END IF
      MyHPSizeFlag(HPNum) = .FALSE.
    END IF

    HPAirInletNode          = HPWaterHeater(HPNum)%HeatPumpAirInletNode
    HPAirOutletNode         = HPWaterHeater(HPNum)%HeatPumpAirOutletNode
    OutdoorAirNode          = HPWaterHeater(HPNum)%OutsideAirNode
    ExhaustAirNode          = HPWaterHeater(HPNum)%ExhaustAirNode
    HPWaterInletNode        = HPWaterHeater(HPNum)%CondWaterInletNode
    HPWaterOutletNode       = HPWaterHeater(HPNum)%CondWaterOutletNode
    InletAirMixerNode       = HPWaterHeater(HPNum)%InletAirMixerNode
    OutletAirSplitterNode   = HPWaterHeater(HPNum)%OutletAirSplitterNode

    SELECT CASE (HPWaterHeater(HPNum)%CrankcaseTempIndicator)
      CASE (CrankcaseTempZone)
        HPWHCrankcaseDBTemp = MAT(HPWaterHeater(HPNum)%AmbientTempZone)
      CASE (CrankcaseTempExterior)
        HPWHCrankcaseDBTemp = OutDryBulbTemp
      CASE (CrankcaseTempSchedule)
        HPWHCrankcaseDBTemp = GetCurrentScheduleValue(HPWaterHeater(HPNum)%CrankcaseTempSchedule)
    END SELECT

!   initialize HPWH report variables to 0 and set tank inlet node equal to outlet node
    HPWaterHeater(HPNum)%HPWaterHeaterSensibleCapacity                  = 0.0d0
    HPWaterHeater(HPNum)%HPWaterHeaterLatentCapacity                    = 0.0d0
    WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = 0.0d0
    HPWaterHeater(HPNum)%HeatingPLR                = 0.0d0
    WaterThermalTank(WaterThermalTankNum)%SourceInletTemp    = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp

!   determine HPWH inlet air conditions based on inlet air configuration (Zone, ZoneAndOA, OutdoorAir, or Schedule)
    SELECT CASE (HPWaterHeater(HPNum)%InletAirConfiguration)
      CASE (AmbientTempZone)
          MixerInletAirSchedule       = 0.0d0
          HPInletDryBulbTemp          = Node(HPAirInletNode)%Temp
          HPInletHumRat               = Node(HPAirInletNode)%HumRat
      CASE (AmbientTempZoneandOA)
        IF(HPWaterHeater(HPNum)%InletAirMixerSchPtr .GT. 0)THEN
!         schedule values are checked for boundary of 0 and 1 in GetWaterThermalTankInputFlag
          MixerInletAirSchedule       = GetCurrentScheduleValue(HPWaterHeater(HPNum)%InletAirMixerSchPtr)
        ELSE
          MixerInletAirSchedule       = 0.0d0
        END IF
        HPInletDryBulbTemp            = MixerInletAirSchedule * Node(OutdoorAirNode)%Temp + &
                                        (1.0d0 - MixerInletAirSchedule) * Node(HPAirInletNode)%Temp
        HPInletHumRat                 = MixerInletAirSchedule * Node(OutdoorAirNode)%Humrat + &
                                        (1.0d0 - MixerInletAirSchedule) * Node(HPAirInletNode)%HumRat
      CASE (AmbientTempOutsideAir)
        MixerInletAirSchedule         = 1.0d0
        HPInletDryBulbTemp            = Node(OutdoorAirNode)%Temp
        HPInletHumRat                 = Node(OutdoorAirNode)%Humrat

      CASE (AmbientTempSchedule)
        HPInletDryBulbTemp            = GetCurrentScheduleValue(HPWaterHeater(HPNum)%AmbientTempSchedule)
        HPInletRelHum                 = GetCurrentScheduleValue(HPWaterHeater(HPNum)%AmbientRHSchedule)
        HPInletHumRat                 = PsyWFnTdbRhPb(HPInletDryBulbTemp,HPInletRelHum,OutBaroPress, 'InitWaterThermalTank')
        Node(HPAirInletNode)%Temp     = HPInletDryBulbTemp
        Node(HPAirInletNode)%HumRat   = HPInletHumRat
        Node(HPAirInletNode)%Enthalpy = PsyHFnTdbW(HPInletDryBulbTemp,HPInletHumRat)
        Node(HPAirInletNode)%Press    = OutBaroPress

    END SELECT

    MdotAir = HPWaterHeater(HPNum)%OperatingAirFlowRate * &
              PsyRhoAirFnPbTdbW(OutBaroPress,HPInletDryBulbTemp, HPInletHumRat)

!   set up initial conditions on nodes
    IF(InletAirMixerNode .GT. 0) THEN
      Node(InletAirMixerNode)%MassFlowRate         = 0.0d0
      Node(InletAirMixerNode)%MassFlowRateMax      = MdotAir
      Node(InletAirMixerNode)%MassFlowRateMaxAvail = MdotAir
      Node(InletAirMixerNode)%Temp                 = HPInletDryBulbTemp
      Node(InletAirMixerNode)%HumRat               = HPInletHumRat
      Node(InletAirMixerNode)%Enthalpy             = PsyHFnTdbW(HPInletDryBulbTemp,HPInletHumRat)
      Node(HPAirInletNode)%MassFlowRate            = 0.0d0
      Node(HPAirOutletNode)%MassFlowRate           = 0.0d0
      Node(OutdoorAirNode)%MassFlowRate            = 0.0d0
      Node(ExhaustAirNode)%MassFlowRate            = 0.0d0
    ELSE
      IF(OutdoorAirNode .EQ. 0)THEN
        Node(HPAirInletNode)%MassFlowRate          = 0.0d0
        Node(HPAirInletNode)%MassFlowRateMax       = MdotAir
        Node(HPAirInletNode)%MassFlowRateMaxAvail  = MdotAir
        Node(HPAirOutletNode)%MassFlowRate         = 0.0d0
      ELSE
        Node(OutdoorAirNode)%MassFlowRate          = 0.0d0
        Node(OutdoorAirNode)%MassFlowRateMax       = MdotAir
        Node(OutdoorAirNode)%MassFlowRateMaxAvail  = MdotAir
        Node(ExhaustAirNode)%MassFlowRate          = 0.0d0
      END IF
    END IF

    IF(OutletAirSplitterNode .GT. 0)Node(OutletAirSplitterNode)%MassFlowRate = 0.0d0
    !these are water nodes are not managed by plant. the HP connects
    ! directly to the WH without using plant. will not change this code for DSU because of this
    Node(HPWaterInletNode)%MassFlowRate            = 0.0d0
    Node(HPWaterOutletNode)%MassFlowRate           = 0.0d0

!   set the max mass flow rate for outdoor fans
    Node(HPWaterHeater(HPNum)%FanOutletNode)%MassFlowRateMax = MdotAir

!   Curve objects in CalcHPWHDXCoil will use inlet conditions to HPWH not inlet air conditions to DX Coil
!   HPWHInletDBTemp and HPWHInletWBTemp are DataHVACGlobals to pass info to HPWHDXCoil
    HPWHInletDBTemp   = HPInletDryBulbTemp
    HPWHInletWBTemp   = PsyTwbFnTdbWPb(HPWHInletDBTemp,HPInletHumRat,OutBaroPress)

  END IF !  IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0)THEN

  RETURN

END SUBROUTINE InitWaterThermalTank


SUBROUTINE CalcWaterThermalTankMixed(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a well-mixed, single node water heater tank.

          ! METHODOLOGY EMPLOYED:
          ! This model uses analytical calculations based on the differential equation describing the tank energy
          ! balance.  The model operates in three different modes:  heating, floating, and venting.  Temperatures and
          ! energies change dynamically over the timestep.  The final reported tank temperature is the average over
          ! the timestep.  The final reported heat rates are averages based on the total energy transfer over the
          ! timestep.

          ! USE STATEMENTS:
  USE General,         ONLY: RoundSigDigits
  USE DataGlobals,     ONLY: TimeStep, TimeStepZone, WarmupFlag, HourOfDay
  USE DataInterfaces,  ONLY: ShowWarningError, ShowContinueError, ShowContinueErrorTimeStamp, ShowWarningMessage,  &
                             ShowRecurringWarningErrorAtEnd
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum      ! Water Heater being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: TimeElapsed         ! Fraction of the current hour that has elapsed (h)
  REAL(r64)           :: SetpointTemp        ! Current setpoint temperature (C)
  REAL(r64)           :: DeadBandTemp        ! Heating: Minimum tank temperature (SetpointTemp - DeadbandDeltaTemp) (C)
                                             ! Cooling: Maximum Tank temperature (SetpointTemp + DeadbandDeltaTemp) (C)
  REAL(r64)           :: MaxTemp             ! Maximum tank temperature before venting (C)
  REAL(r64)           :: AmbientTemp         ! Current ambient air temperature around tank (C)
  REAL(r64)           :: TankMass            ! Mass of water in tank (kg)
  REAL(r64)           :: LossCoeff           ! Loss coefficient to ambient environment (W/K)
  REAL(r64)           :: LossFracToZone      ! Fraction of losses added to the zone as a gain
  REAL(r64)           :: TankTemp            ! Instantaneous tank temperature (C)
  REAL(r64)           :: NewTankTemp         ! Predicted new tank temperature (C)
  REAL(r64)           :: TankTempAvg         ! Average tank temperature over the timestep (C)
  REAL(r64)           :: Cp                  ! Specific heat of water (J/kg K)
  REAL(r64)           :: Quse                ! Heating rate due to use side mass flow (W)
  REAL(r64)           :: Qsource             ! Heating rate due to source side mass flow (W)
  REAL(r64)           :: Qloss               ! Heating rate due to ambient environment (W)
  REAL(r64)           :: Qlosszone           ! Heating rate of fraction of losses added to the zone as a gain (W)
  REAL(r64)           :: Qheat               ! Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
  REAL(r64)           :: Qheater             ! Heating rate of the burner or electric heating element (W)
  REAL(r64)           :: Qmaxcap             ! Maximum capacity heating rate of the burner or electric heating element (W)
  REAL(r64)           :: Qmincap             ! Minimum capacity heating rate of the burner or electric heating element (W)
  REAL(r64)           :: Qoffcycfuel         ! Fuel consumption rate of off-cycle parasitics (W)
  REAL(r64)           :: Qoffcycheat         ! Heating rate of fraction of off-cycle parasitics added to the tank (W)
  REAL(r64)           :: Qoncycfuel          ! Fuel consumption rate on-cycle parasitics added to the tank (W)
  REAL(r64)           :: Qoncycheat          ! Heating rate of fraction of on-cycle parasitics added to the tank (W)
  REAL(r64)           :: Qneeded             ! Heating rate needed to recover or maintain the setpoint temperature (W)
  REAL(r64)           :: Qunmet              ! The difference between Qneeded and Qheater (W)
  REAL(r64)           :: Qvent               ! Heating rate due to venting because tank exceeded max temperature limit (W)
  REAL(r64)           :: Qnet                ! Net heat transfer rate including everything (W)
  REAL(r64)           :: Qfuel               ! Heating rate for fuel consumed (W)
  REAL(r64)           :: UseInletTemp        ! Use side inlet temperature (C)
  REAL(r64)           :: UseMassFlowRate     ! Use side flow rate, including effectiveness factor (kg/s)
  REAL(r64)           :: MinMassFlowRate     ! Minimum use side flow rate required before heater is enabled (kg/s)
  REAL(r64)           :: SourceInletTemp     ! Source side inlet temperature (C)
  REAL(r64)           :: SourceMassFlowRate  ! Source side flow rate, including effectiveness factor (kg/s)
  INTEGER             :: Mode                ! Indicator for current operating mode (HeatMode=1 | FloatMode=0 | VentMode=-1)
  REAL(r64)           :: SecInTimeStep       ! Seconds in one timestep (s)
  REAL(r64)           :: TimeRemaining       ! Time remaining in the current timestep (s)
  REAL(r64)           :: TimeNeeded          ! Time needed to reach the next substep (s)
  INTEGER             :: CycleOnCount        ! Number of times heater cycles on in the current time step
  INTEGER             :: MaxCycles           ! Maximum number of cycles allowed before exiting loop
  REAL(r64)           :: Runtime             ! Time that heater is running (s)
  REAL(r64)           :: RTF                 ! Runtime fraction, fraction of timestep that heater is running
  REAL(r64)           :: PLR                 ! Part load ratio, fraction of maximum heater capacity
  REAL(r64)           :: PLRsum              ! Integrated part load ratio over the timestep (J)
  REAL(r64)           :: PLF                 ! Part load factor, modifies thermal efficiency to get total energy efficiency
  REAL(r64)           :: Tsum                ! Integrated tank temp over the timestep, dividing by time gives the average (C s)
  REAL(r64)           :: deltaTsum           ! Change in integrated tank temperature, dividing by time gives the average (C s)
  REAL(r64)           :: Eloss               ! Energy change due to ambient losses over the timestep (J)
  REAL(r64)           :: Elosszone           ! Energy change to the zone due to ambient losses over the timestep (J)
  REAL(r64)           :: Euse                ! Energy change due to use side mass flow over the timestep (J)
  REAL(r64)           :: Esource             ! Energy change due to source side mass flow over the timestep (J)
  REAL(r64)           :: Eheater             ! Energy change due to the heater over the timestep (J)
  REAL(r64)           :: Eoncycfuel          ! Fuel energy consumed by on-cycle parasitics over the timestep (J)
  REAL(r64)           :: Eoffcycfuel         ! Fuel energy consumed by off-cycle parasitics over the timestep (J)
  REAL(r64)           :: Event               ! Energy change due to venting over the timestep (J)
  REAL(r64)           :: Eneeded             ! Energy change needed over the timestep (J)
  REAL(r64)           :: Eunmet              ! Energy change unmet over the timestep (J)
  REAL(r64)           :: Efuel               ! Energy change for fuel consumed over the timestep (J)
  LOGICAL             :: SetpointRecovered   ! Flag to indicate when setpoint is recovered for the first time
  REAL(r64)           :: rho
  INTEGER             :: DummyWaterIndex = 1

          ! FLOW:
  TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed

  IF (WaterThermalTank(WaterThermalTankNum)%TimeElapsed /= TimeElapsed) THEN
    ! The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
    ! timestep for use as the initial conditions of each iteration that does not advance the system timestep.
    WaterThermalTank(WaterThermalTankNum)%SavedTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
    WaterThermalTank(WaterThermalTankNum)%SavedMode = WaterThermalTank(WaterThermalTankNum)%Mode

    ! Save outlet temperatures for demand-side flow control
    WaterThermalTank(WaterThermalTankNum)%SavedUseOutletTemp = WaterThermalTank(WaterThermalTankNum)%UseOutletTemp
    WaterThermalTank(WaterThermalTankNum)%SavedSourceOutletTemp = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp

    WaterThermalTank(WaterThermalTankNum)%TimeElapsed = TimeElapsed
  END IF

  TankTemp = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
  Mode = WaterThermalTank(WaterThermalTankNum)%SavedMode

  Qmaxcap = WaterThermalTank(WaterThermalTankNum)%MaxCapacity
  Qmincap = WaterThermalTank(WaterThermalTankNum)%MinCapacity
  Qoffcycfuel = WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad
  Qoffcycheat = Qoffcycfuel * WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank
  Qoncycfuel = WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad
  Qoncycheat = Qoncycfuel * WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank

  SetpointTemp = WaterThermalTank(WaterThermalTankNum)%SetpointTemp
  DeadBandTemp = SetpointTemp - WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp
  MaxTemp = WaterThermalTank(WaterThermalTankNum)%TankTempLimit
  AmbientTemp = WaterThermalTank(WaterThermalTankNum)%AmbientTemp

  UseInletTemp = WaterThermalTank(WaterThermalTankNum)%UseInletTemp
  UseMassFlowRate = WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate *   &
     WaterThermalTank(WaterThermalTankNum)%UseEffectiveness
  MinMassFlowRate = WaterThermalTank(WaterThermalTankNum)%MassFlowRateMin
  SourceInletTemp = WaterThermalTank(WaterThermalTankNum)%SourceInletTemp
  SourceMassFlowRate = WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate *   &
     WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness

  IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) THEN
    rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                         TankTemp, &
                         PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                         'CalcWaterThermalTankMixed')
  ELSE
    rho = GetDensityGlycol('WATER', TankTemp, DummyWaterIndex, 'CalcWaterThermalTankMixed')
  ENDIF

  TankMass = rho * WaterThermalTank(WaterThermalTankNum)%Volume

  IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) THEN
    Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                         TankTemp, &
                         PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                         'CalcWaterThermalTankMixed')
  ELSE
    Cp = GetSpecificHeatGlycol('WATER', TankTemp, DummyWaterIndex, 'CalcWaterThermalTankMixed')
  ENDIF

  SecInTimeStep = TimeStepSys * SecInHour
  TimeRemaining = SecInTimeStep
  TimeNeeded = 0.0d0
  CycleOnCount = 0
  MaxCycles = SecInTimeStep
  Runtime = 0.0d0
  SetpointRecovered = .FALSE.

  Tsum = 0.0d0
  Eloss = 0.0d0
  Elosszone = 0.0d0
  Euse = 0.0d0
  Esource = 0.0d0
  Eheater = 0.0d0
  Event = 0.0d0
  Eneeded = 0.0d0
  Eunmet = 0.0d0
  Efuel = 0.0d0
  Eoncycfuel = 0.0d0
  Eoffcycfuel = 0.0d0
  PLR = 0.0d0
  PLRSum = 0.0d0

  Qheat = 0.0d0
  Qheater = 0.0d0
  Qvent = 0.0d0
  Qneeded = 0.0d0
  Qunmet = 0.0d0
  Qnet = 0.0d0
  Qfuel = 0.0d0

  ! Calculate steady-state heat rates
  Quse = UseMassFlowRate * Cp * (UseInletTemp - SetpointTemp)
  Qsource = SourceMassFlowRate * Cp * (SourceInletTemp - SetpointTemp)

  DO WHILE (TimeRemaining > 0.0d0)

    TimeNeeded = 0.0d0

    NewTankTemp = TankTemp

    SELECT CASE (Mode)

      CASE (HeatMode)  ! Heater is on

        ! Calculate heat rate needed to maintain the setpoint at steady-state conditions
        LossCoeff = WaterThermalTank(WaterThermalTankNum)%OnCycLossCoeff
        LossFracToZone = WaterThermalTank(WaterThermalTankNum)%OnCycLossFracToZone
        Qloss = LossCoeff * (AmbientTemp - SetpointTemp)
        Qneeded = -Quse - Qsource - Qloss - Qoncycheat

        IF (TankTemp > SetpointTemp) THEN
          ! Heater is not needed after all, possibly due to step change in scheduled SetpointTemp

          Qheater = 0.0d0
          Qunmet = 0.0d0
          Mode = FloatMode
          CYCLE

        ELSE IF (TankTemp < SetpointTemp) THEN
          ! Attempt to recover to the setpoint as quickly as possible by using maximum heater capacity

          ! Qneeded is calculated above
          ! Qneeded does not account for the extra energy needed to recover to the setpoint
          Qheater = Qmaxcap
          Qunmet = MAX(Qneeded - Qheater,0.0d0)
          Qheat = Qoncycheat + Qheater

          ! Calculate time needed to recover to the setpoint at maximum heater capacity
          TimeNeeded = CalcTimeNeeded(TankTemp, SetpointTemp, AmbientTemp, UseInletTemp, SourceInletTemp, &
                                      TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat)

          IF (TimeNeeded > TimeRemaining) THEN
            ! Heater is at maximum capacity and heats for all of the remaining time
            ! Setpoint temperature WILL NOT be recovered

            TimeNeeded = TimeRemaining

            NewTankTemp = CalcTankTemp(TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, &
                                       Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeNeeded)

          ELSE  ! TimeNeeded <= TimeRemaining
            ! Heater is at maximum capacity but will not heat for all of the remaining time (at maximum anyway)
            ! Setpoint temperature WILL be recovered

            NewTankTemp = SetpointTemp

            SetpointRecovered = .TRUE.

          END IF  ! TimeNeeded > TimeRemaining


        ELSE  ! TankTemp == SetpointTemp
          ! Attempt to maintain the setpoint by using the needed heater capacity (modulating, if allowed)

          IF (Qneeded <= 0.0d0) THEN
            ! Heater is not needed

            Qneeded = 0.0d0
            Qheater = 0.0d0
            Qunmet = 0.0d0
            Mode = FloatMode
            CYCLE

          ELSE IF (Qneeded < Qmincap) THEN
            ! Heater is required at less than the minimum capacity
            ! If cycling, Qmincap = Qmaxcap.  Once the setpoint is reached, heater will almost always be shut off here

            SELECT CASE (WaterThermalTank(WaterThermalTankNum)%ControlType)

              CASE (ControlTypeCycle)
                ! Control will cycle on and off based on DeadBandTemp
                Qheater = 0.0d0
                Qunmet = 0.0d0
                Mode = FloatMode
                CYCLE

              CASE (ControlTypeModulate)
                ! Control will cycle on and off based on DeadBandTemp until Qneeded > Qmincap again
                Qheater = 0.0d0
                Qunmet = Qneeded
                Mode = FloatMode
                CYCLE

              !CASE (ControlTypeModulateWithOverheat)  ! Not yet implemented
                ! Calculate time to reach steady-state temp; check for venting at MaxTemp limit
                !Qheater = Qmincap

              !CASE (ControlTypeModulateWithUnderheat)  ! Not yet implemented
                ! Heater must not come back on until Qneeded >= Qmincap
                !Mode = FloatMode

            END SELECT

          ELSE IF (Qneeded <= Qmaxcap) THEN
            ! Heater can exactly meet the needed heat rate (usually by modulating) and heats for all of the remaining time
            ! Setpoint temperature WILL be maintained

            TimeNeeded = TimeRemaining

            Qheater = Qneeded
            Qunmet = 0.0d0

            NewTankTemp = SetpointTemp

          ELSE  ! Qneeded > Qmaxcap
            ! Heater is at maximum capacity and heats for all of the remaining time
            ! Setpoint temperature WILL NOT be maintained

            TimeNeeded = TimeRemaining

            Qheater = Qmaxcap
            Qunmet = Qneeded - Qheater
            Qheat = Qoncycheat + Qheater

            NewTankTemp = CalcTankTemp(TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, &
                                       Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeNeeded)

          END IF  ! Qneeded > Qmaxcap



        END IF  ! TankTemp > SetpointTemp

        ! Update summed values
        Eneeded = Eneeded + Qneeded * TimeNeeded
        Eheater = Eheater + Qheater * TimeNeeded
        Eunmet = Eunmet + Qunmet * TimeNeeded
        Eoncycfuel = Eoncycfuel + Qoncycfuel * TimeNeeded

        IF (Qmaxcap > 0.0d0) PLR = Qheater / Qmaxcap
        PLF = PartLoadFactor(WaterThermalTankNum, PLR)
        Efuel = Efuel + Qheater * TimeNeeded / (PLF * WaterThermalTank(WaterThermalTankNum)%Efficiency)

        Runtime = Runtime + TimeNeeded
        PLRsum = PLRsum + PLR * TimeNeeded

        IF (.NOT. WaterThermalTank(WaterThermalTankNum)%FirstRecoveryDone) THEN
          WaterThermalTank(WaterThermalTankNum)%FirstRecoveryFuel = WaterThermalTank(WaterThermalTankNum)%FirstRecoveryFuel &
              + Efuel + Eoffcycfuel + Eoncycfuel
          IF (SetpointRecovered) WaterThermalTank(WaterThermalTankNum)%FirstRecoveryDone = .TRUE.
        END IF


      CASE (FloatMode, CoolMode)  ! Heater is off

        ! Calculate heat rate needed to maintain the setpoint at steady-state conditions
        LossCoeff = WaterThermalTank(WaterThermalTankNum)%OffCycLossCoeff
        LossFracToZone = WaterThermalTank(WaterThermalTankNum)%OffCycLossFracToZone
        Qloss = LossCoeff * (AmbientTemp - SetpointTemp)
        Qneeded = -Quse - Qsource - Qloss - Qoffcycheat

        ! This section really needs to work differently depending on ControlType
        ! CYCLE will look at TankTemp, MODULATE will look at Qneeded

        IF ((TankTemp < DeadBandTemp) .AND. (.NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank)) THEN
          ! Tank temperature is already below the minimum, possibly due to step change in scheduled SetpointTemp

          Mode = HeatMode
          CycleOnCount = CycleOnCount + 1
          CYCLE

        ELSEIF(( TankTemp >= DeadBandTemp) .AND. (.NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank)) THEN

          Qheat = Qoffcycheat

          ! Calculate time needed for tank temperature to fall to minimum (setpoint - deadband)
          TimeNeeded = CalcTimeNeeded(TankTemp, DeadBandTemp, AmbientTemp, UseInletTemp, SourceInletTemp, &
                                      TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat)

          IF (TimeNeeded <= TimeRemaining) THEN
            ! Heating will be needed in this timestep

            NewTankTemp = DeadBandTemp
            Mode = HeatMode
            CycleOnCount = CycleOnCount + 1

          ELSE  ! TimeNeeded > TimeRemaining
            ! Heating will not be needed for all of the remaining time

            NewTankTemp = CalcTankTemp(TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, &
                                       Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeRemaining)

            IF ( (NewTankTemp < MaxTemp)  .OR. (WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank) )THEN
              ! Neither heating nor venting is needed for all of the remaining time

              TimeNeeded = TimeRemaining

            ELSE  ! NewTankTemp >= MaxTemp
              ! Venting will be needed in this timestep

              ! Calculate time needed for tank temperature to rise to the maximum
              TimeNeeded = CalcTimeNeeded(TankTemp, MaxTemp, AmbientTemp, UseInletTemp, SourceInletTemp, &
                                          TankMass, Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat)

              NewTankTemp = MaxTemp
              Mode = VentMode

            END IF  ! NewTankTemp >= MaxTemp

          END IF  ! TimeNeeded <= TimeRemaining

        ELSEIF(( TankTemp > DeadBandTemp) .AND. (WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank)) THEN
          Mode = CoolMode
          Qheat = 0.0D0

          NewTankTemp = CalcTankTemp(TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, &
                                     Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeRemaining)
          TimeNeeded = TimeRemaining
        ELSEIF(( TankTemp <= DeadBandTemp) .AND. (WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank)) THEN

          IF (TankTemp < SetpointTemp) Mode = FloatMode

          Qheat = 0.0D0

          NewTankTemp = CalcTankTemp(TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, &
                                     Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeRemaining)
          TimeNeeded = TimeRemaining
        END IF  ! TankTemp vs DeadBandTemp for heaters and chilled water tanks

        ! Update summed values
        Eneeded = Eneeded + Qneeded * TimeNeeded
        Eunmet = Eunmet + Qunmet * TimeNeeded  ! Qunmet may be propagated thru from the previous iteration
        Eoffcycfuel = Eoffcycfuel + Qoffcycfuel * TimeNeeded

      CASE (VentMode)  ! Excess heat is vented

        LossCoeff = WaterThermalTank(WaterThermalTankNum)%OffCycLossCoeff
        LossFracToZone = WaterThermalTank(WaterThermalTankNum)%OffCycLossFracToZone
        Qheat = Qoffcycheat

        NewTankTemp = CalcTankTemp(TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, &
                                   Cp, UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeRemaining)

        IF (NewTankTemp < MaxTemp) THEN
          ! Venting is no longer needed because conditions have changed

          Mode = FloatMode
          CYCLE

        ELSE  ! NewTankTemp >= MaxTemp

          TimeNeeded = TimeRemaining

          ! Calculate the steady-state venting rate needed to maintain the tank at maximum temperature
          Qloss = LossCoeff * (AmbientTemp - MaxTemp)
          Quse = UseMassFlowRate * Cp * (UseInletTemp - MaxTemp)
          Qsource = SourceMassFlowRate * Cp * (SourceInletTemp - MaxTemp)
          Qvent = -Quse - Qsource - Qloss - Qoffcycheat

          NewTankTemp = MaxTemp

        END IF  ! NewTankTemp < MaxTemp

        ! Update summed values
        Event = Event + Qvent * TimeNeeded
        Eoffcycfuel = Eoffcycfuel + Qoffcycfuel * TimeNeeded



      CASE DEFAULT
        ! No default
    END SELECT


    deltaTsum = CalcTempIntegral(TankTemp, NewTankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, Cp, &
                                 UseMassFlowRate, SourceMassFlowRate, LossCoeff, Qheat, TimeNeeded)

    ! Update summed values
    Tsum = Tsum + deltaTsum
    Eloss = Eloss + LossCoeff * (AmbientTemp * TimeNeeded - deltaTsum)
    Elosszone = Elosszone + LossFracToZone * LossCoeff * (AmbientTemp * TimeNeeded - deltaTsum)
    Euse = Euse + UseMassFlowRate * Cp * (UseInletTemp * TimeNeeded - deltaTsum)
    Esource = Esource + SourceMassFlowRate * Cp * (SourceInletTemp * TimeNeeded - deltaTsum)

    TankTemp = NewTankTemp  ! Update tank temperature

    TimeRemaining = TimeRemaining - TimeNeeded


    IF (CycleOnCount > MaxCycles) THEN

      IF (.NOT. WarmupFlag) THEN
        IF (WaterThermalTank(WaterThermalTankNum)%MaxCycleErrorIndex == 0) THEN
          CALL ShowWarningError('WaterHeater:Mixed = '//TRIM(WaterThermalTank(WaterThermalTankNum)%Name)// &
            ':  Heater is cycling on and off more than once per second.')
          CALL ShowContinueError('Try increasing Deadband Temperature Difference or Tank Volume')
          CALL ShowContinueErrorTimeStamp(' ')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('WaterHeater:Mixed = '//TRIM(WaterThermalTank(WaterThermalTankNum)%Name)// &
            ' Heater is cycling on and off more than once per second:', &
            WaterThermalTank(WaterThermalTankNum)%MaxCycleErrorIndex)
      END IF

      EXIT

    END IF  ! CycleOnCount > MaxCycles

  END DO  ! TimeRemaining > 0.0

  ! Calculate average values over the timestep based on summed values, Q > 0 is a gain to the tank,  Q < 0 is a loss to the tank
  TankTempAvg = Tsum / SecInTimeStep
  Qloss = Eloss / SecInTimeStep
  Qlosszone = Elosszone / SecInTimeStep
  Quse = Euse / SecInTimeStep
  Qsource = Esource / SecInTimeStep
  Qheater = Eheater / SecInTimeStep
  Qoffcycfuel = Eoffcycfuel / SecInTimeStep
  Qoffcycheat = Qoffcycfuel * WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank
  Qoncycfuel = Eoncycfuel / SecInTimeStep
  Qoncycheat = Qoncycfuel * WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank
  Qvent = Event / SecInTimeStep
  Qneeded = Eneeded / SecInTimeStep
  Qunmet = Eunmet / SecInTimeStep
  RTF = Runtime / SecInTimeStep
  PLR = PLRSum / SecInTimeStep

  IF (WaterThermalTank(WaterThermalTankNum)%ControlType == ControlTypeCycle) THEN
    ! Recalculate Part Load Factor and fuel energy based on Runtime Fraction, instead of Part Load Ratio
    PLF = PartLoadFactor(WaterThermalTankNum, RTF)
    Efuel = Eheater / (PLF * WaterThermalTank(WaterThermalTankNum)%Efficiency)
  END IF

  Qfuel = Efuel / SecInTimeStep

  WaterThermalTank(WaterThermalTankNum)%Mode = Mode  ! Operating mode for carry-over to next timestep

  WaterThermalTank(WaterThermalTankNum)%TankTemp = TankTemp  ! Final tank temperature for carry-over to next timestep
  WaterThermalTank(WaterThermalTankNum)%TankTempAvg = TankTempAvg  ! Average tank temperature over the timestep for reporting
  WaterThermalTank(WaterThermalTankNum)%UseOutletTemp = TankTempAvg  ! Because entire tank is at same temperature
  WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp = TankTempAvg  ! Because entire tank is at same temperature

  WaterThermalTank(WaterThermalTankNum)%LossRate = Qloss
  WaterThermalTank(WaterThermalTankNum)%UseRate = Quse
  WaterThermalTank(WaterThermalTankNum)%SourceRate = Qsource
  WaterThermalTank(WaterThermalTankNum)%OffCycParaRateToTank = Qoffcycheat
  WaterThermalTank(WaterThermalTankNum)%OnCycParaRateToTank = Qoncycheat
  WaterThermalTank(WaterThermalTankNum)%TotalDemandRate = -Quse - Qsource - Qloss - Qoffcycheat - Qoncycheat
  WaterThermalTank(WaterThermalTankNum)%HeaterRate = Qheater
  WaterThermalTank(WaterThermalTankNum)%UnmetRate = Qunmet
  WaterThermalTank(WaterThermalTankNum)%VentRate = Qvent
  WaterThermalTank(WaterThermalTankNum)%NetHeatTransferRate = Quse + Qsource + Qloss + Qoffcycheat + Qoncycheat + Qheater + Qvent

  WaterThermalTank(WaterThermalTankNum)%CycleOnCount = CycleOnCount
  WaterThermalTank(WaterThermalTankNum)%RuntimeFraction = RTF
  WaterThermalTank(WaterThermalTankNum)%PartLoadRatio = PLR

  WaterThermalTank(WaterThermalTankNum)%FuelRate = Qfuel
  WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelRate = Qoffcycfuel
  WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelRate = Qoncycfuel

  ! Add water heater skin losses and venting losses to ambient zone, if specified
  IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempZone > 0)  &
     WaterThermalTank(WaterThermalTankNum)%AmbientZoneGain = -Qlosszone - Qvent

  RETURN

END SUBROUTINE CalcWaterThermalTankMixed


REAL(r64) FUNCTION CalcTimeNeeded(Ti, Tf, Ta, T1, T2, m, Cp, m1, m2, UA, Q)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the time needed for the tank temperature to change from Ti to Tf given heat loss,
          ! mass flow rates and temperatures, and net heat transfer due to heater and parasitics.

          ! METHODOLOGY EMPLOYED:
          ! Equations are derived by solving the differential equation governing the tank energy balance.
          ! Special cases which cause the natural logarithm to blow up are trapped and interpreted as
          ! requiring an infinite amount of time because Tf can never be reached under the given conditions.

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: Ti        ! Initial tank temperature (C)
  REAL(r64), INTENT(IN)  :: Tf        ! Final tank temperature (C)
  REAL(r64), INTENT(IN)  :: Ta        ! Ambient environment temperature (C)
  REAL(r64), INTENT(IN)  :: T1        ! Temperature of flow 1 (C)
  REAL(r64), INTENT(IN)  :: T2        ! Temperature of flow 2 (C)
  REAL(r64), INTENT(IN)  :: m         ! Mass of tank fluid (kg)
  REAL(r64), INTENT(IN)  :: Cp        ! Specific heat of fluid (J/kg deltaC)
  REAL(r64), INTENT(IN)  :: m1        ! Mass flow rate 1 (kg/s)
  REAL(r64), INTENT(IN)  :: m2        ! Mass flow rate 2 (kg/s)
  REAL(r64), INTENT(IN)  :: UA        ! Heat loss coefficient to ambient environment (W/deltaC)
  REAL(r64), INTENT(IN)  :: Q         ! Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER   :: Infinity = 99999999.9d0  ! A time interval much larger than any single timestep (s)

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)         :: a         ! Intermediate variable
  REAL(r64)         :: b         ! Intermediate variable
  REAL(r64)         :: Tm        ! Mixed temperature after an infinite amount of time has passed (C)
  REAL(r64)  :: quotient  ! Intermediate variable
  REAL(r64)         :: t         ! Time elapsed from Ti to Tf (s)

          ! FLOW:
  IF (Tf == Ti) THEN
    ! Already at Tf; no time is needed
    t = 0.0d0

  ELSE

    IF (UA / Cp + m1 + m2 == 0.0d0) THEN

      IF (Q == 0.0d0) THEN
        ! With no mass flow and no heat flow and Tf<>Ti, then Tf can never be reached
        t = Infinity

      ELSE
        a = Q / (m * Cp)

        t = (Tf - Ti) / a

      END IF

    ELSE
      a = (Q / Cp + UA * Ta / Cp + m1 * T1 + m2 * T2) / m
      b = -(UA / Cp + m1 + m2) / m

      ! Calculate the mixed temperature Tm of the tank after an infinite amount of time has passed
      Tm = -a / b

      IF (Tm == Ti) THEN
        ! Mixed temperature is the same as Ti; if Tf<>Ti, then Tf can never be reached
        t = Infinity

      ELSE IF (Tm == Tf) THEN
        ! Tf only approaches Tm; it can never actually get there in finite time (also avoids divide by zero error)
        t = Infinity

      ELSE
        quotient = (Tf - Tm) / (Ti - Tm)

        IF (quotient <= 0.0d0) THEN !Objexx:Num Changed < to <= to elim poss floating point error in LOG call
          ! Tm is in between Ti and Tf; Tf can never be reached
          t = Infinity

        ELSE
          t = LOG(quotient) / b

        END IF
      END IF
    END IF

    IF (t < 0.0d0) t = Infinity  ! If negative time, Tf can never be reached in the future

  END IF

  CalcTimeNeeded = t

  RETURN

END FUNCTION CalcTimeNeeded


REAL(r64) FUNCTION CalcTankTemp(Ti, Ta, T1, T2, m, Cp, m1, m2, UA, Q, t)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the final tank temperature Tf after time t has elapsed given heat loss,
          ! mass flow rates and temperatures, and net heat transfer due to heater and parasitics.

          ! METHODOLOGY EMPLOYED:
          ! Equations are derived by solving the differential equation governing the tank energy balance.

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: Ti       ! Initial tank temperature (C)
  REAL(r64), INTENT(IN)  :: Ta       ! Ambient environment temperature (C)
  REAL(r64), INTENT(IN)  :: T1       ! Temperature of flow 1 (C)
  REAL(r64), INTENT(IN)  :: T2       ! Temperature of flow 2 (C)
  REAL(r64), INTENT(IN)  :: m        ! Mass of tank fluid (kg)
  REAL(r64), INTENT(IN)  :: Cp       ! Specific heat of fluid (J/kg deltaC)
  REAL(r64), INTENT(IN)  :: m1       ! Mass flow rate 1 (kg/s)
  REAL(r64), INTENT(IN)  :: m2       ! Mass flow rate 2 (kg/s)
  REAL(r64), INTENT(IN)  :: UA       ! Heat loss coefficient to ambient environment (W/deltaC)
  REAL(r64), INTENT(IN)  :: Q        ! Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
  REAL(r64), INTENT(IN)  :: t        ! Time elapsed from Ti to Tf (s)

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)         :: a        ! Intermediate variable
  REAL(r64)  :: b        ! Intermediate variable
  REAL(r64)         :: Tf       ! Final tank temperature (C)

          ! FLOW:
  IF (UA / Cp + m1 + m2 == 0.0d0) THEN
    a = Q / (m * Cp)

    Tf = a * t + Ti

  ELSE
    a = (Q / Cp + UA * Ta / Cp + m1 * T1 + m2 * T2) / m
    b = -(UA / Cp + m1 + m2) / m

    Tf = (a / b + Ti) * EXP(b * t) - a / b

  END IF

  CalcTankTemp = Tf

  RETURN

END FUNCTION CalcTankTemp


REAL(r64) FUNCTION CalcTempIntegral(Ti, Tf, Ta, T1, T2, m, Cp, m1, m2, UA, Q, t)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the integral of the tank temperature from Ti to Tf.  The integral is added to a sum which is
          ! later divided by the elapsed time to yield the average tank temperature over the timestep.

          ! METHODOLOGY EMPLOYED:
          ! Equations are the mathematical integrals of the governing differential equations.

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: Ti        ! Initial tank temperature (C)
  REAL(r64), INTENT(IN)  :: Tf        ! Final tank temperature (C)
  REAL(r64), INTENT(IN)  :: Ta        ! Ambient environment temperature (C)
  REAL(r64), INTENT(IN)  :: T1        ! Temperature of flow 1 (C)
  REAL(r64), INTENT(IN)  :: T2        ! Temperature of flow 2 (C)
  REAL(r64), INTENT(IN)  :: m         ! Mass of tank fluid (kg)
  REAL(r64), INTENT(IN)  :: Cp        ! Specific heat of fluid (J/kg deltaC)
  REAL(r64), INTENT(IN)  :: m1        ! Mass flow rate 1 (kg/s)
  REAL(r64), INTENT(IN)  :: m2        ! Mass flow rate 2 (kg/s)
  REAL(r64), INTENT(IN)  :: UA        ! Heat loss coefficient to ambient environment (W/deltaC)
  REAL(r64), INTENT(IN)  :: Q         ! Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
  REAL(r64), INTENT(IN)  :: t         ! Time elapsed from Ti to Tf (s)

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)         :: a         ! Intermediate variable
  REAL(r64)  :: b         ! Intermediate variable
  REAL(r64)         :: dTsum     ! Integral of tank temperature (C s)

          ! FLOW:
  IF (t == 0.0d0) THEN
    dTsum = 0.0d0

  ELSE IF (Tf == Ti) THEN  ! Steady-state conditions
    dTsum = Tf * t

  ELSE IF (UA / Cp + m1 + m2 == 0.0d0) THEN
    a = Q / (m * Cp)

    ! Integral of T(t) = a * t + Ti, evaluated from 0 to t
    dTsum = 0.5d0 * a * t * t + Ti * t

  ELSE
    a = (Q / Cp + UA * Ta / Cp + m1 * T1 + m2 * T2) / m
    b = -(UA / Cp + m1 + m2) / m

    ! Integral of T(t) = (a / b + Ti) * EXP(b * t) - a / b, evaluated from 0 to t
    dTsum = (a / b + Ti) * (EXP(b * t) - 1.0d0) / b - a * t / b
  END IF

  CalcTempIntegral = dTsum

  RETURN

END FUNCTION CalcTempIntegral


REAL(r64) FUNCTION PartLoadFactor(WaterThermalTankNum, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the Part Load Factor (PLF) based on a curve correlated to Part Load Ratio, if Heater Control Type
          ! is MODULATE, or correlated to Runtime Fraction, if Heater Control Type is CYCLE.

          ! METHODOLOGY EMPLOYED:
          ! Uses CurveManager.  Part Load Factor is not allowed below 0.1.

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum
  REAL(r64), INTENT(IN)    :: PartLoadRatio

          ! FLOW:
  IF (WaterThermalTank(WaterThermalTankNum)%PLFCurve > 0) THEN
    PartLoadFactor = CurveValue(WaterThermalTank(WaterThermalTankNum)%PLFCurve, PartLoadRatio)

    PartLoadFactor = MAX(PartLoadFactor,0.1d0)
  ELSE
    ! No curve was defined
    PartLoadFactor = 1.0d0
  END IF

  RETURN

END FUNCTION PartLoadFactor


SUBROUTINE CalcWaterThermalTankStratified(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2007
          !       MODIFIED       na
          !                      Nov 2011, BAN; modified the use and source outlet temperature calculation
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a stratified, multi-node water heater tank with up to two heating elements.

          ! METHODOLOGY EMPLOYED:
          ! This model uses a numerical calculation based on the forward Euler method.  A heat balance is calculated for each
          ! node at a sub time step interval of one second.  Temperatures and energies change dynamically over the system
          ! time step.  Final node temperatures are reported as final instantaneous values as well as averages over the
          ! time step.  Heat transfer rates are averages over the time step.

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: TimeStep, TimeStepZone, HourOfDay
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum      ! Water Heater being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER     :: dt = 1.0d0          ! Sub time step interval (s)

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: TimeElapsed         ! Fraction of the current hour that has elapsed (h)
  REAL(r64)           :: SecInTimeStep       ! Seconds in one timestep (s)
  REAL(r64)           :: TimeRemaining       ! Time remaining in the current timestep (s)
  INTEGER             :: NumNodes            ! Number of stratified nodes
  INTEGER             :: NodeNum             ! Node number index
  REAL(r64)           :: NodeMass            ! Mass of water in a node (kg)
  REAL(r64)           :: NodeTemp            ! Instantaneous node temperature (C)
  REAL(r64)           :: TempUp              ! Temperature of the upper node (C)
  REAL(r64)           :: TempDn              ! Temperature of the lower node (C)
  REAL(r64)           :: InvMixUp            ! Inversion mixing rate with the upper node (kg/s)
  REAL(r64)           :: InvMixDn            ! Inversion mixing rate with the lower node (kg/s)
  REAL(r64)           :: Cp                  ! Specific heat of water (J/kg K)
  REAL(r64)           :: LossCoeff           ! Loss coefficient to ambient environment (W/K)
  REAL(r64)           :: AmbientTemp         ! Current ambient air temperature around tank (C)
  REAL(r64)           :: SetPointTemp1       ! Current set point temperature for heater 1 (C)
  REAL(r64)           :: SetPointTemp2       ! Current set point temperature for heater 2 (C)
  REAL(r64)           :: MinTemp1            ! Minimum tank temperature (SetPointTemp1 - DeadbandDeltaTemp1) (C)
  REAL(r64)           :: MinTemp2            ! Minimum tank temperature (SetPointTemp2 - DeadbandDeltaTemp2) (C)
  REAL(r64)           :: MaxTemp             ! Maximum tank temperature before venting (C)
  REAL(r64)           :: Quse                ! Heating rate due to use side mass flow (W)
  REAL(r64)           :: Qsource             ! Heating rate due to source side mass flow (W)
  REAL(r64)           :: Qcond               ! Heating rate due to vertical conduction between nodes
  REAL(r64)           :: Qflow               ! Heating rate due to fluid flow between inlet and outlet nodes
  REAL(r64)           :: Qmix                ! Heating rate due to temperature inversion mixing between nodes
  REAL(r64)           :: Qloss               ! Heating rate due to ambient environment (W)
  REAL(r64)           :: Qlosszone           ! Heating rate of fraction of losses added to the zone as a gain (W)
  REAL(r64)           :: Qheat               ! Net heating rate for non-temp dependent sources, i.e. heater and parasitics (W)
  REAL(r64)           :: Qheater1            ! Heating rate of burner or electric heating element 1 (W)
  REAL(r64)           :: Qheater2            ! Heating rate of burner or electric heating element 2 (W)
  REAL(r64)           :: Qheater             ! Combined heating rate of heater 1 and 2 (W)
  REAL(r64)           :: Qoffcycfuel         ! Fuel consumption rate of off-cycle parasitics (W)
  REAL(r64)           :: Qoffcycheat         ! Heating rate of fraction of off-cycle parasitics added to the tank (W)
  REAL(r64)           :: Qoncycfuel          ! Fuel consumption rate on-cycle parasitics added to the tank (W)
  REAL(r64)           :: Qoncycheat          ! Heating rate of fraction of on-cycle parasitics added to the tank (W)
  REAL(r64)           :: Qneeded             ! Heating rate needed to recover or maintain the setpoint temperature (W)
  REAL(r64)           :: Qunmet              ! The difference between Qneeded and Qheater (W)
  REAL(r64)           :: Qvent               ! Heating rate due to venting because tank exceeded max temperature limit (W)
  REAL(r64)           :: Qfuel               ! Heating rate for fuel consumed (W)
  REAL(r64)           :: UseInletTemp        ! Use side inlet temperature (C)
  REAL(r64)           :: UseMassFlowRate     ! Use side flow rate, including effectiveness factor (kg/s)
  REAL(r64)           :: SourceInletTemp     ! Source side inlet temperature (C)
  REAL(r64)           :: SourceMassFlowRate  ! Source side flow rate, including effectiveness factor (kg/s)
  INTEGER             :: CycleOnCount1       ! Number of times heater 1 cycles on in the current time step
  INTEGER             :: CycleOnCount2       ! Number of times heater 2 cycles on in the current time step
  REAL(r64)           :: Runtime1            ! Time that heater 1 is running (s)
  REAL(r64)           :: Runtime2            ! Time that heater 2 is running (s)
  REAL(r64)           :: Runtime             ! Time that either heater is running (s)
  REAL(r64)           :: RTF1                ! Runtime fraction, fraction of timestep that heater 1 is running
  REAL(r64)           :: RTF2                ! Runtime fraction, fraction of timestep that heater 2 is running
  REAL(r64)           :: RTF                 ! Runtime fraction, fraction of timestep that either heater is running
  REAL(r64)           :: Eloss               ! Energy change due to ambient losses over the timestep (J)
  REAL(r64)           :: Elosszone           ! Energy change to the zone due to ambient losses over the timestep (J)
  REAL(r64)           :: Euse                ! Energy change due to use side mass flow over the timestep (J)
  REAL(r64)           :: Esource             ! Energy change due to source side mass flow over the timestep (J)
  REAL(r64)           :: Eheater1            ! Energy change due to heater 1 over the timestep (J)
  REAL(r64)           :: Eheater2            ! Energy change due to heater 2 over the timestep (J)
  REAL(r64)           :: Eoncycfuel          ! Fuel energy consumed by on-cycle parasitics over the timestep (J)
  REAL(r64)           :: Eoffcycfuel         ! Fuel energy consumed by off-cycle parasitics over the timestep (J)
  REAL(r64)           :: Event               ! Energy change due to venting over the timestep (J)
  REAL(r64)           :: Eneeded             ! Energy change needed over the timestep (J)
  REAL(r64)           :: Eunmet              ! Energy change unmet over the timestep (J)
  REAL(r64)           :: Efuel               ! Energy change for fuel consumed over the timestep (J)
  LOGICAL             :: SetPointRecovered   ! Flag to indicate when set point is recovered for the first time
  INTEGER             :: DummyWaterIndex = 1

  ! FLOW:
  TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed

  IF (WaterThermalTank(WaterThermalTankNum)%TimeElapsed /= TimeElapsed) THEN
    ! The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
    ! timestep for use as the initial conditions of each iteration that does not advance the system timestep.
    WaterThermalTank(WaterThermalTankNum)%Node%SavedTemp = WaterThermalTank(WaterThermalTankNum)%Node%Temp
    WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn1 = WaterThermalTank(WaterThermalTankNum)%HeaterOn1
    WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn2 = WaterThermalTank(WaterThermalTankNum)%HeaterOn2

    ! Save outlet temperatures for demand-side flow control
    WaterThermalTank(WaterThermalTankNum)%SavedUseOutletTemp = WaterThermalTank(WaterThermalTankNum)%UseOutletTemp
    WaterThermalTank(WaterThermalTankNum)%SavedSourceOutletTemp = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp

    WaterThermalTank(WaterThermalTankNum)%TimeElapsed = TimeElapsed
  END IF

  WaterThermalTank(WaterThermalTankNum)%Node%Temp = WaterThermalTank(WaterThermalTankNum)%Node%SavedTemp
  WaterThermalTank(WaterThermalTankNum)%HeaterOn1 = WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn1
  WaterThermalTank(WaterThermalTankNum)%HeaterOn2 = WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn2

  SecInTimeStep = TimeStepSys * SecInHour
  NumNodes = WaterThermalTank(WaterThermalTankNum)%Nodes

  AmbientTemp = WaterThermalTank(WaterThermalTankNum)%AmbientTemp
  UseInletTemp = WaterThermalTank(WaterThermalTankNum)%UseInletTemp
  SourceInletTemp = WaterThermalTank(WaterThermalTankNum)%SourceInletTemp

  SetPointTemp1 = WaterThermalTank(WaterThermalTankNum)%SetPointTemp
  MinTemp1 = SetPointTemp1 - WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp
  SetPointTemp2 = WaterThermalTank(WaterThermalTankNum)%SetPointTemp2
  MinTemp2 = SetPointTemp2 - WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp2
  MaxTemp = WaterThermalTank(WaterThermalTankNum)%TankTempLimit

  IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) THEN
    Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                         WaterThermalTank(WaterThermalTankNum)%TankTemp, &
                         PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                         'CalcWaterThermalTankStratified')
  ELSE
    Cp  = GetSpecificHeatGlycol('WATER', WaterThermalTank(WaterThermalTankNum)%TankTemp, &
                                    DummyWaterIndex, 'CalcWaterThermalTankStratified')
  ENDIF

  TempUp = 0.0d0
  TempDn = 0.0d0
  Eloss = 0.0d0
  Elosszone = 0.0d0
  Euse = 0.0d0
  Esource = 0.0d0
  Eheater1 = 0.0d0
  Eheater2 = 0.0d0
  Event = 0.0d0
  Eneeded = 0.0d0
  Eunmet = 0.0d0
  Efuel = 0.0d0
  Eoncycfuel = 0.0d0
  Eoffcycfuel = 0.0d0
  CycleOnCount1 = 0
  CycleOnCount2 = 0
  Runtime = 0.0d0
  Runtime1 = 0.0d0
  Runtime2 = 0.0d0
  SetPointRecovered = .FALSE.

  IF (WaterThermalTank(WaterThermalTankNum)%InletMode == InletModeFixed)   &
     CALL CalcNodeMassFlows(WaterThermalTankNum, InletModeFixed)

  TimeRemaining = SecInTimeStep
  DO WHILE (TimeRemaining > 0.0d0)

    IF (WaterThermalTank(WaterThermalTankNum)%InletMode == InletModeSeeking)   &
       CALL CalcNodeMassFlows(WaterThermalTankNum, InletModeSeeking)

    IF ( .not. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank) THEN

      ! Control the first heater element (master)
      IF (WaterThermalTank(WaterThermalTankNum)%MaxCapacity > 0.0d0) THEN
        NodeNum = WaterThermalTank(WaterThermalTankNum)%HeaterNode1
        NodeTemp = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Temp

        IF (WaterThermalTank(WaterThermalTankNum)%HeaterOn1) THEN
          IF (NodeTemp >= SetPointTemp1) THEN
            WaterThermalTank(WaterThermalTankNum)%HeaterOn1 = .FALSE.
            SetPointRecovered = .TRUE.
          END IF
        ELSE  ! Heater is off
          IF (NodeTemp < MinTemp1) THEN
            WaterThermalTank(WaterThermalTankNum)%HeaterOn1 = .TRUE.
            CycleOnCount1 = CycleOnCount1 + 1
          END IF
        END IF
      END IF

      IF (WaterThermalTank(WaterThermalTankNum)%HeaterOn1) THEN
        Qheater1 = WaterThermalTank(WaterThermalTankNum)%MaxCapacity
        Runtime1 = Runtime1 + dt
      ELSE
        Qheater1 = 0.0D0
      END IF

      ! Control the second heater element (slave)
      IF (WaterThermalTank(WaterThermalTankNum)%MaxCapacity2 > 0.0d0) THEN
        IF ((WaterThermalTank(WaterThermalTankNum)%ControlType == PriorityMasterSlave) .AND.   &
           WaterThermalTank(WaterThermalTankNum)%HeaterOn1) THEN
          WaterThermalTank(WaterThermalTankNum)%HeaterOn2 = .FALSE.

        ELSE
          NodeNum = WaterThermalTank(WaterThermalTankNum)%HeaterNode2
          NodeTemp = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Temp

          IF (WaterThermalTank(WaterThermalTankNum)%HeaterOn2) THEN
            IF (NodeTemp >= SetPointTemp2) THEN
              WaterThermalTank(WaterThermalTankNum)%HeaterOn2 = .FALSE.
              SetPointRecovered = .TRUE.
            END IF
          ELSE  ! Heater is off
            IF (NodeTemp < MinTemp2) THEN
              WaterThermalTank(WaterThermalTankNum)%HeaterOn2 = .TRUE.
              CycleOnCount2 = CycleOnCount2 + 1
            END IF
          END IF
        END IF
      END IF

      IF (WaterThermalTank(WaterThermalTankNum)%HeaterOn2) THEN
        Qheater2 = WaterThermalTank(WaterThermalTankNum)%MaxCapacity2
        Runtime2 = Runtime2 + dt
      ELSE
        Qheater2 = 0.0d0
      END IF
    ELSE ! chilled water thank, no heating

      Qheater1 = 0.0D0
      Qheater2 = 0.0D0

    ENDIF

    IF (WaterThermalTank(WaterThermalTankNum)%HeaterOn1 .OR. WaterThermalTank(WaterThermalTankNum)%HeaterOn2) THEN
      Runtime = Runtime + dt

      Qfuel = (Qheater1 + Qheater2) / WaterThermalTank(WaterThermalTankNum)%Efficiency
      Qoncycfuel = WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad
      Qoffcycfuel = 0.0d0
    ELSE
      Qfuel = 0.0d0
      Qoncycfuel = 0.0d0
      Qoffcycfuel = WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad
    END IF

    ! Loop through all nodes and simulate heat balance
    DO NodeNum = 1, NumNodes
      NodeMass = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Mass
      NodeTemp = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Temp

      UseMassFlowRate = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%UseMassFlowRate *   &
         WaterThermalTank(WaterThermalTankNum)%UseEffectiveness
      SourceMassFlowRate = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%SourceMassFlowRate  &
        * WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness

      ! Heat transfer due to fluid flow entering an inlet node
      Quse = UseMassFlowRate * Cp * (UseInletTemp - NodeTemp)
      Qsource = SourceMassFlowRate * Cp * (SourceInletTemp - NodeTemp)

      InvMixUp = 0.0d0
      IF (NodeNum > 1) THEN
        TempUp = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum - 1)%Temp
        IF (TempUp < NodeTemp) InvMixUp = WaterThermalTank(WaterThermalTankNum)%InversionMixingRate
      END IF

      InvMixDn = 0.0d0
      IF (NodeNum < NumNodes) THEN
        TempDn = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum + 1)%Temp
        IF (TempDn > NodeTemp) InvMixDn = WaterThermalTank(WaterThermalTankNum)%InversionMixingRate
      END IF

      ! Heat transfer due to vertical conduction between nodes
      Qcond = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%CondCoeffUp * (TempUp - NodeTemp)  &
        + WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%CondCoeffDn * (TempDn - NodeTemp)

      ! Heat transfer due to fluid flow between inlet and outlet nodes
      Qflow = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromUpper * Cp * (TempUp - NodeTemp)  &
        + WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromLower * Cp * (TempDn - NodeTemp)

      ! Heat transfer due to temperature inversion mixing between nodes
      Qmix = InvMixUp * Cp * (TempUp - NodeTemp) + InvMixDn * Cp * (TempDn - NodeTemp)

      IF (WaterThermalTank(WaterThermalTankNum)%HeaterOn1 .OR. WaterThermalTank(WaterThermalTankNum)%HeaterOn2) THEN
        LossCoeff = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OnCycLossCoeff
        Qloss = LossCoeff * (AmbientTemp - NodeTemp)
        Qlosszone = Qloss * WaterThermalTank(WaterThermalTankNum)%SkinLossFracToZone
        Qoncycheat = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OnCycParaLoad *   &
           WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank

        Qneeded = MAX(- Quse - Qsource - Qloss - Qoncycheat, 0.0d0)

        Qheat = Qoncycheat
        IF (NodeNum == WaterThermalTank(WaterThermalTankNum)%HeaterNode1) Qheat = Qheat + Qheater1
        IF (NodeNum == WaterThermalTank(WaterThermalTankNum)%HeaterNode2) Qheat = Qheat + Qheater2
      ELSE
        LossCoeff = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OffCycLossCoeff
        Qloss = LossCoeff * (AmbientTemp - NodeTemp)
        Qlosszone = Qloss * WaterThermalTank(WaterThermalTankNum)%SkinLossFracToZone
        Qoffcycheat = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%OffCycParaLoad *   &
           WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank

        Qneeded = MAX(- Quse - Qsource - Qloss - Qoffcycheat, 0.0d0)
        Qheat = Qoffcycheat
      END IF

      Qunmet = MAX(Qneeded - Qheater1 - Qheater2, 0.0d0)

      ! Calculate node heat balance
      WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%NewTemp = NodeTemp  &
        + (Quse + Qsource + Qcond + Qflow + Qmix + Qloss + Qheat) * dt / (NodeMass * Cp)

      IF ( .not. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank) THEN
        IF ((NodeNum == 1) .AND. (WaterThermalTank(WaterThermalTankNum)%Node(1)%NewTemp > MaxTemp)) THEN
          Event = Event + NodeMass * Cp * (MaxTemp - WaterThermalTank(WaterThermalTankNum)%Node(1)%NewTemp)
          WaterThermalTank(WaterThermalTankNum)%Node(1)%NewTemp = MaxTemp
        END IF
      ENDIF

      Euse = Euse + Quse * dt
      Esource = Esource + Qsource * dt
      Eloss = Eloss + Qloss * dt
      Elosszone = Elosszone + Qlosszone * dt
      Eneeded = Eneeded + Qneeded * dt
      Eunmet = Eunmet + Qunmet * dt

    END DO  ! NodeNum

    ! Calculation for standard ratings
    IF (.NOT. WaterThermalTank(WaterThermalTankNum)%FirstRecoveryDone) THEN
      WaterThermalTank(WaterThermalTankNum)%FirstRecoveryFuel = WaterThermalTank(WaterThermalTankNum)%FirstRecoveryFuel &
          + (Qfuel + Qoffcycfuel + Qoncycfuel) * dt
      IF (SetpointRecovered) WaterThermalTank(WaterThermalTankNum)%FirstRecoveryDone = .TRUE.
    END IF

    ! Update node temperatures
    WaterThermalTank(WaterThermalTankNum)%Node%Temp = WaterThermalTank(WaterThermalTankNum)%Node%NewTemp
    WaterThermalTank(WaterThermalTankNum)%Node%TempSum = WaterThermalTank(WaterThermalTankNum)%Node%TempSum  &
      + WaterThermalTank(WaterThermalTankNum)%Node%Temp * dt

    TimeRemaining = TimeRemaining - dt

  END DO  ! TimeRemaining > 0.0

  Eheater1 = WaterThermalTank(WaterThermalTankNum)%MaxCapacity * Runtime1
  Eheater2 = WaterThermalTank(WaterThermalTankNum)%MaxCapacity2 * Runtime2
  Efuel = (Eheater1 + Eheater2) / WaterThermalTank(WaterThermalTankNum)%Efficiency
  Eoffcycfuel = WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad * (SecInTimeStep - Runtime)
  Eoncycfuel = WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad * Runtime

  ! Calculate average values over the time step based on summed values, Q > 0 is a gain to the tank,  Q < 0 is a loss to the tank
  Qloss = Eloss / SecInTimeStep
  Qlosszone = Elosszone / SecInTimeStep
  Quse = Euse / SecInTimeStep
  Qsource = Esource / SecInTimeStep
  Qheater1 = Eheater1 / SecInTimeStep
  Qheater2 = Eheater2 / SecInTimeStep
  Qheater = Qheater1 + Qheater2
  Qoffcycfuel = Eoffcycfuel / SecInTimeStep
  Qoffcycheat = Qoffcycfuel * WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank
  Qoncycfuel = Eoncycfuel / SecInTimeStep
  Qoncycheat = Qoncycfuel * WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank
  Qvent = Event / SecInTimeStep
  Qneeded = Eneeded / SecInTimeStep
  Qunmet = Eunmet / SecInTimeStep
  RTF = Runtime / SecInTimeStep
  RTF1 = Runtime1 / SecInTimeStep
  RTF2 = Runtime2 / SecInTimeStep
  Qfuel = Efuel / SecInTimeStep

  ! Calculate average node temperatures over the time step
  WaterThermalTank(WaterThermalTankNum)%Node%TempAvg = WaterThermalTank(WaterThermalTankNum)%Node%TempSum / SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%Node%TempSum = 0.0d0  ! Reset for next time step

  ! Calculate instantaneous and average tank temperature (all nodes have equal mass)
  WaterThermalTank(WaterThermalTankNum)%TankTemp = SUM(WaterThermalTank(WaterThermalTankNum)%Node%Temp) / NumNodes
  WaterThermalTank(WaterThermalTankNum)%TankTempAvg = SUM(WaterThermalTank(WaterThermalTankNum)%Node%TempAvg) / NumNodes

  NodeNum = WaterThermalTank(WaterThermalTankNum)%UseOutletStratNode
  IF (NodeNum > 0) WaterThermalTank(WaterThermalTankNum)%UseOutletTemp = WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%TempAvg
  ! Revised use outlet temperature to ensure energy balance. Assumes a constant CP. CR8341/CR8570
  IF (NodeNum > 0) Then
    IF (WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate .GT. 0.0d0) Then
      WaterThermalTank(WaterThermalTankNum)%UseOutletTemp =  WaterThermalTank(WaterThermalTankNum)%UseInletTemp     &
                                                          * (1.0d0 - WaterThermalTank(WaterThermalTankNum)%UseEffectiveness)  &
                                                          +  WaterThermalTank(WaterThermalTankNum)%UseOutletTemp   &
                                                          *  WaterThermalTank(WaterThermalTankNum)%UseEffectiveness
    End If
  End If
  NodeNum = WaterThermalTank(WaterThermalTankNum)%SourceOutletStratNode
  IF (NodeNum > 0) WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp =   &
     WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%TempAvg
  ! Revised use outlet temperature to ensure energy balance. Assumes a constant CP. CR8341/CR8570
  IF (NodeNum > 0) Then
    IF (WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate .GT. 0.0d0) Then
      WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp =  WaterThermalTank(WaterThermalTankNum)%SourceInletTemp     &
                                                       * (1.0d0 - WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness)  &
                                                       +  WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp   &
                                                       *  WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness
    End If
  End If

  WaterThermalTank(WaterThermalTankNum)%LossRate = Qloss
  WaterThermalTank(WaterThermalTankNum)%UseRate = Quse
  WaterThermalTank(WaterThermalTankNum)%SourceRate = Qsource
  WaterThermalTank(WaterThermalTankNum)%OffCycParaRateToTank = Qoffcycheat
  WaterThermalTank(WaterThermalTankNum)%OnCycParaRateToTank = Qoncycheat
  WaterThermalTank(WaterThermalTankNum)%TotalDemandRate = -Quse - Qsource - Qloss - Qoffcycheat - Qoncycheat
  WaterThermalTank(WaterThermalTankNum)%HeaterRate = Qheater
  WaterThermalTank(WaterThermalTankNum)%HeaterRate1 = Qheater1
  WaterThermalTank(WaterThermalTankNum)%HeaterRate2 = Qheater2

  WaterThermalTank(WaterThermalTankNum)%UnmetRate = Qunmet
  WaterThermalTank(WaterThermalTankNum)%VentRate = Qvent
  WaterThermalTank(WaterThermalTankNum)%NetHeatTransferRate = Quse + Qsource + Qloss + Qoffcycheat + Qoncycheat + Qheater + Qvent

  WaterThermalTank(WaterThermalTankNum)%CycleOnCount = CycleOnCount1 + CycleOnCount2
  WaterThermalTank(WaterThermalTankNum)%CycleOnCount1 = CycleOnCount1
  WaterThermalTank(WaterThermalTankNum)%CycleOnCount2 = CycleOnCount2

  WaterThermalTank(WaterThermalTankNum)%RuntimeFraction = RTF
  WaterThermalTank(WaterThermalTankNum)%RuntimeFraction1 = RTF1
  WaterThermalTank(WaterThermalTankNum)%RuntimeFraction2 = RTF2

  WaterThermalTank(WaterThermalTankNum)%FuelRate = Qfuel
  WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelRate = Qoffcycfuel
  WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelRate = Qoncycfuel

  ! Add water heater skin losses and venting losses to ambient zone, if specified
  IF (WaterThermalTank(WaterThermalTankNum)%AmbientTempZone > 0)   &
     WaterThermalTank(WaterThermalTankNum)%AmbientZoneGain = -Qlosszone - Qvent

  RETURN

END SUBROUTINE CalcWaterThermalTankStratified


SUBROUTINE CalcNodeMassFlows(WaterThermalTankNum, InletMode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2007
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines mass flow rates between nodes according to the locations of the use- and source-side inlet and outlet
          ! nodes.

          ! METHODOLOGY EMPLOYED:
          ! In 'Seeking' mode, nodes are searched between the user-specified inlet and outlet nodes to find the node closest
          ! in temperature to the inlet fluid temperature.  In 'Fixed' mode, the user-specified nodes are always used.
          ! Upward and downward flows are added to each node between an inlet and outlet.  Flows in both directions cancel out
          ! to leave only the net flow in one direction.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum      ! Water Heater being simulated
  INTEGER, INTENT(IN) :: InletMode           ! InletModeFixed or InletModeSeeking

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumNodes               ! Number of stratified nodes
  INTEGER :: UseInletStratNode      ! Use-side inlet node number
  INTEGER :: UseOutletStratNode     ! Use-side outlet node number
  INTEGER :: SourceInletStratNode   ! Source-side inlet node number
  INTEGER :: SourceOutletStratNode  ! Source-side outlet node number
  INTEGER :: NodeNum                ! Node number index
  REAL(r64) :: UseMassFlowRate           ! Use side flow rate, including effectiveness factor (kg/s)
  REAL(r64) :: SourceMassFlowRate        ! Source side flow rate, including effectiveness factor (kg/s)
  INTEGER :: Step                   ! DO loop step direction, 1 or -1
  REAL(r64) :: DeltaTemp                 ! Temperature difference between node and inlet (delta C)
  REAL(r64) :: MinDeltaTemp              ! Smallest temperature difference found so far (delta C)

          ! FLOW:
  NumNodes = WaterThermalTank(WaterThermalTankNum)%Nodes

  UseInletStratNode = WaterThermalTank(WaterThermalTankNum)%UseInletStratNode
  UseOutletStratNode = WaterThermalTank(WaterThermalTankNum)%UseOutletStratNode
  SourceInletStratNode = WaterThermalTank(WaterThermalTankNum)%SourceInletStratNode
  SourceOutletStratNode = WaterThermalTank(WaterThermalTankNum)%SourceOutletStratNode

  UseMassFlowRate = WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate *   &
     WaterThermalTank(WaterThermalTankNum)%UseEffectiveness
  SourceMassFlowRate = WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate *   &
     WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness

  WaterThermalTank(WaterThermalTankNum)%Node%UseMassFlowRate = 0.0d0
  WaterThermalTank(WaterThermalTankNum)%Node%SourceMassFlowRate = 0.0d0
  WaterThermalTank(WaterThermalTankNum)%Node%MassFlowFromUpper = 0.0d0
  WaterThermalTank(WaterThermalTankNum)%Node%MassFlowFromLower = 0.0d0
  WaterThermalTank(WaterThermalTankNum)%Node%MassFlowToUpper = 0.0d0
  WaterThermalTank(WaterThermalTankNum)%Node%MassFlowToLower = 0.0d0

  IF (InletMode == InletModeSeeking) THEN
    ! 'Seek' the node with the temperature closest to the inlet temperature
    ! Start at the user-specified inlet node and search to the user-specified outlet node

    IF (UseMassFlowRate > 0.0d0) THEN
      IF (UseInletStratNode > UseOutletStratNode) THEN
        Step = -1
      ELSE
        Step = 1
      END IF
      MinDeltaTemp = 1.0d6  ! Some big number
      DO NodeNum = UseInletStratNode, UseOutletStratNode, Step
        DeltaTemp = ABS(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Temp -   &
           WaterThermalTank(WaterThermalTankNum)%UseInletTemp)
        IF (DeltaTemp < MinDeltaTemp) THEN
          MinDeltaTemp = DeltaTemp
          UseInletStratNode = NodeNum
        ELSE IF (DeltaTemp > MinDeltaTemp) THEN
          EXIT
        END IF
      END DO
    END IF

    IF (SourceMassFlowRate > 0.0d0) THEN
      IF (SourceInletStratNode > SourceOutletStratNode) THEN
        Step = -1
      ELSE
        Step = 1
      END IF
      MinDeltaTemp = 1.0d6  ! Some big number
      DO NodeNum = SourceInletStratNode, SourceOutletStratNode, Step
        DeltaTemp = ABS(WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%Temp -   &
           WaterThermalTank(WaterThermalTankNum)%SourceInletTemp)
        IF (DeltaTemp < MinDeltaTemp) THEN
          MinDeltaTemp = DeltaTemp
          SourceInletStratNode = NodeNum
        ELSE IF (DeltaTemp > MinDeltaTemp) THEN
          EXIT
        END IF
      END DO
    END IF

  END IF

  IF (UseInletStratNode > 0)   &
     WaterThermalTank(WaterThermalTankNum)%Node(UseInletStratNode)%UseMassFlowRate = UseMassFlowRate
  IF (SourceInletStratNode > 0)   &
     WaterThermalTank(WaterThermalTankNum)%Node(SourceInletStratNode)%SourceMassFlowRate = SourceMassFlowRate


  IF (UseMassFlowRate > 0.0d0) THEN
    IF (UseOutletStratNode > UseInletStratNode) THEN
      ! Use-side flow is down
      DO NodeNum = UseInletStratNode, UseOutletStratNode - 1
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToLower =  &
          WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToLower + UseMassFlowRate
      END DO
      DO NodeNum = UseInletStratNode + 1, UseOutletStratNode
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromUpper =  &
          WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromUpper + UseMassFlowRate
      END DO

    ELSE IF (UseOutletStratNode < UseInletStratNode) THEN
      ! Use-side flow is up
      DO NodeNum = UseOutletStratNode, UseInletStratNode - 1
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromLower =  &
          WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromLower + UseMassFlowRate
      END DO
      DO NodeNum = UseOutletStratNode + 1, UseInletStratNode
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToUpper =  &
          WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToUpper + UseMassFlowRate
      END DO

    ELSE
      ! Use-side flow is across the node; no flow to other nodes
    END IF
  END IF

  IF (SourceMassFlowRate > 0.0d0) THEN
    IF (SourceOutletStratNode > SourceInletStratNode) THEN
      ! Source-side flow is down
      DO NodeNum = SourceInletStratNode, SourceOutletStratNode - 1
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToLower =  &
          WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToLower + SourceMassFlowRate
      END DO
      DO NodeNum = SourceInletStratNode + 1, SourceOutletStratNode
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromUpper =  &
          WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromUpper + SourceMassFlowRate
      END DO

    ELSE IF (SourceOutletStratNode < SourceInletStratNode) THEN
      ! Source-side flow is up
      DO NodeNum = SourceOutletStratNode, SourceInletStratNode - 1
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromLower =  &
          WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromLower + SourceMassFlowRate
      END DO
      DO NodeNum = SourceOutletStratNode + 1, SourceInletStratNode
        WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToUpper =  &
          WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToUpper + SourceMassFlowRate
      END DO

    ELSE
      ! Source-side flow is across the node; no flow to other nodes
    END IF
  END IF

  ! Cancel out any up and down flows
  DO NodeNum = 1, NumNodes
    WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromUpper =  &
      MAX((WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromUpper  &
      - WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToUpper), 0.0d0)
    WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromLower =  &
      MAX((WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowFromLower  &
      - WaterThermalTank(WaterThermalTankNum)%Node(NodeNum)%MassFlowToLower), 0.0d0)
  END DO

  RETURN

END SUBROUTINE CalcNodeMassFlows


SUBROUTINE CalcDesuperheaterWaterHeater(WaterThermalTankNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a refrigerant desuperheater to heat water

          ! METHODOLOGY EMPLOYED:
          ! This model uses the rated heat reclaim recovery efficiency, recovery efficiency modifier curve,
          ! set point temperature, and dead band temperature difference to simulate the desuperheater coil
          ! and sets up inputs to the tank model associated with the desuperheater coil

          ! USE STATEMENTS:
  USE CurveManager,      ONLY: CurveValue
  USE DataLoopNode,      ONLY: Node
  USE DXCoils,           ONLY: DXCoil
  USE Psychrometrics,    ONLY: CPHW, RhoH2O
  USE ScheduleManager,   ONLY: GetCurrentScheduleValue
  USE DataHeatBalance,   ONLY: HeatReclaimDXCoil
  USE DataGlobals,       ONLY: SecInHour, WarmupFlag, DoingSizing, KickOffSimulation
  USE DataInterfaces,    ONLY: ShowFatalError, ShowSevereError, ShowWarningError, ShowContinueError, &
                               ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
  USE DataHVACGlobals,   ONLY: TimeStepSys, ShortenTimeStepSys
  USE General,           ONLY: SolveRegulaFalsi, RoundSigDigits
  USE DataEnvironment,   ONLY: OutDryBulbTemp

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum      ! Water Heater being simulated
  LOGICAL, INTENT(IN) :: FirstHVACIteration  ! TRUE if First iteration of simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte = 500            ! Maximum number of iterations for RegulaFalsi
  REAL(r64), PARAMETER :: Acc =  0.00001D0        ! Accuracy of result from RegulaFalsi

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: AvailSchedule           ! desuperheater availability schedule
  REAL(r64)           :: SetPointTemp            ! desuperheater set point temperature (cut-out temperature, C)
  REAL(r64)           :: DeadbandTempDiff        ! desuperheater dead band temperature difference (C)
  REAL(r64)           :: CutInTemp               ! desuperheater cut-in temperature (SetpointTemp - DeadbandTempDiff, C)
  REAL(r64)           :: TankTemp                ! tank temperature before simulation, C
  REAL(r64)           :: NewTankTemp             ! tank temperature after simulation, C
  REAL(r64)           :: MdotWater               ! mass flow rate through desuperheater, kg/s
  REAL(r64)           :: PartLoadRatio           ! desuperheater part load ratio
  REAL(r64)           :: QHeatRate               ! desuperheater heating rate (W)
  REAL(r64)           :: AverageWasteHeat        ! average hating rate from DX system condenser (W)
  REAL(r64)           :: HEffFTemp               ! output of heating efficiency as a function of temperature curve
  REAL(r64)           :: Effic                   ! efficiency of desuperheater heating coil
  REAL(r64)           :: CpWater                 ! specific heat of water (J/Kg/k)
  INTEGER             :: WaterInletNode          ! desuperheater water inlet node number
  INTEGER             :: WaterOutletNode         ! desuperheater water outlet node number
  INTEGER             :: DesuperheaterNum        ! Index to desuperheater
  INTEGER             :: SolFla                  ! Flag of RegulaFalsi solver
  INTEGER             :: SourceID                ! Waste Heat Source ID number
  REAL(r64), DIMENSION(5)  :: Par                     ! Parameters passed to RegulaFalsi
  REAL(r64)           :: MinTemp = 0.0d0           ! used for error messages, C
  CHARACTER(len=20)   :: IterNum                    ! Max number of iterations for warning message

          ! FLOW:
  DesuperheaterNum  = WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum
  AvailSchedule     = GetCurrentScheduleValue(WaterHeaterDesuperheater(DesuperheaterNum)%AvailSchedPtr)
  WaterInletNode    = WaterHeaterDesuperheater(DesuperheaterNum)%WaterInletNode
  WaterOutletNode   = WaterHeaterDesuperheater(DesuperheaterNum)%WaterOutletNode


! initialize variables before invoking any RETURN statement
  PartLoadRatio                                    = 0.0d0
  WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate   = 0.0d0
! reset tank inlet temp from previous time step
  WaterThermalTank(WaterThermalTankNum)%SourceInletTemp      = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp
  WaterHeaterDesuperheater(DesuperheaterNum)%DesuperheaterPLR = 0.0d0

  Node(WaterInletNode)%MassFlowRate                = 0.0d0
  Node(WaterOutletNode)%MassFlowRate               = 0.0d0
  Node(WaterOutletNode)%Temp                       = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp

  WaterHeaterDesuperheater(DesuperheaterNum)%DesuperheaterPLR     = 0.0d0
  WaterHeaterDesuperheater(DesuperheaterNum)%OnCycParaFuelRate    = 0.0d0
  WaterHeaterDesuperheater(DesuperheaterNum)%OnCycParaFuelEnergy  = 0.0d0
  WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaFuelRate   = 0.0d0
  WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaFuelEnergy = 0.0d0
  WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTempOutput      = 0.0d0
  WaterHeaterDesuperheater(DesuperheaterNum)%HeaterRate           = 0.0d0
  WaterHeaterDesuperheater(DesuperheaterNum)%HeaterEnergy         = 0.0d0
  WaterHeaterDesuperheater(DesuperheaterNum)%PumpPower            = 0.0d0
  WaterHeaterDesuperheater(DesuperheaterNum)%PumpEnergy           = 0.0d0

! set up initial conditions
  QHeatRate         = 0.0d0
  SetPointTemp      = WaterHeaterDesuperheater(DesuperheaterNum)%SetpointTemp
  DeadbandTempDiff  = WaterHeaterDesuperheater(DesuperheaterNum)%DeadbandTempDiff

! simulate only the water heater tank if the desuperheater coil is scheduled off
  IF(AvailSchedule .EQ. 0.0d0)THEN
    WaterHeaterDesuperheater(DesuperheaterNum)%Mode     = FloatMode
    CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
    RETURN
  END IF

! simulate only the water heater tank if the lowest temperature available from the desuperheater coil
! is less than water inlet temperature if the reclaim source is a refrigeration condenser
  IF(ValidSourceType(DesuperheaterNum))THEN
    SourceID = WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum
    IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource == CONDENSER_REFRIGERATION) THEN
      IF(HeatReclaimRefrigCondenser(SourceID)%AvailTemperature .LE. WaterThermalTank(WaterThermalTankNum)%SourceInletTemp)THEN
        WaterHeaterDesuperheater(DesuperheaterNum)%Mode     = FloatMode
        CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
        CALL ShowRecurringWarningErrorAtEnd('WaterHeating:Desuperheater '// &
          TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name) // &
          ' - Waste heat source temperature was too low to be useful.',&
          WaterHeaterDesuperheater(DesuperheaterNum)%InsuffTemperatureWarn)
        RETURN
      END IF ! Temp too low
    END IF   ! desuperheater source is condenser_refrigeration
  END IF     ! validsourcetype

  WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaFuelRate   = WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaLoad * &
                                                                    (1.d0-PartLoadRatio)
  WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaFuelEnergy = WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaFuelRate *&
                                                                    TimeStepSys * SecInHour

! check that water heater tank cut-in temp is greater than desuperheater cut-in temp
  IF((SetPointTemp - DeadbandTempDiff) .LE. WaterThermalTank(WaterThermalTankNum)%SetpointTemp)THEN
    IF(.NOT. WarmupFlag .AND. .NOT. DoingSizing .AND. .NOT. KickOffSimulation ) THEN
      MinTemp = SetPointTemp - DeadbandTempDiff
      WaterHeaterDesuperheater(DesuperheaterNum)%SetPointError=WaterHeaterDesuperheater(DesuperheaterNum)%SetPointError+1
      IF (WaterHeaterDesuperheater(DesuperheaterNum)%SetPointError < 5) THEN
        CALL ShowWarningError(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                            //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
            '":  Water heater tank set point temperature is greater than or equal to the cut-in temperature'// &
            ' of the desuperheater. Desuperheater will be disabled.')
        CALL ShowContinueErrorTimeStamp(' '//'...Desuperheater cut-in temperature = '//TRIM(RoundSigDigits(MinTemp,2)))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                                        //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
          '":  Water heater tank set point temperature is greater than or equal to the cut-in temperature'// &
          ' of the desuperheater. Desuperheater will be disabled warning continues...' &
          , WaterHeaterDesuperheater(DesuperheaterNum)%SetPointErrIndex1, MinTemp, MinTemp)
      END IF
    END IF

!   Simulate tank if desuperheater unavailable for water heating
    CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
    RETURN
  END IF

  Effic = WaterHeaterDesuperheater(DesuperheaterNum)%HeatReclaimRecoveryEff

! store first iteration tank temperature and desuperheater mode of operation
  IF (FirstHVACIteration .AND. .NOT. ShortenTimeStepSys) THEN
    ! Save conditions from end of previous system timestep
    ! Every iteration that does not advance time should reset to these values
    WaterThermalTank(WaterThermalTankNum)%SavedTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
    WaterHeaterDesuperheater(DesuperheaterNum)%SaveMode = WaterHeaterDesuperheater(DesuperheaterNum)%Mode
  END IF

  TankTemp = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
  WaterHeaterDesuperheater(DesuperheaterNum)%Mode = WaterHeaterDesuperheater(DesuperheaterNum)%SaveMode

  IF(WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTemp .GT. 0)THEN
    HEffFTemp = MAX(0.0d0,CurveValue(WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTemp, TankTemp, OutDryBulbTemp))
  ELSE
    HEffFTemp = 1.0d0
  END IF

!set limits on heat recovery efficiency
  IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource == CONDENSER_REFRIGERATION) THEN
    IF((HEffFTemp * Effic) .GT. 0.9d0)HEffFTemp = 0.9d0 / Effic
  ELSE ! max is 0.3 for all other sources
    IF((HEffFTemp * Effic) .GT. 0.3d0)HEffFTemp = 0.3d0 / Effic
  END IF  !setting limits on heat recovery efficiency

  ! Access the appropriate structure to find the average heating capacity of the desuperheater heating coil
  IF(ValidSourceType(DesuperheaterNum))THEN
     SourceID = WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum
     IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. COMPRESSORRACK_REFRIGERATEDCASE)THEN
       !Refrigeration systems are solved outside the time step iteration, so the
       !  appropriate decrement for other waste heat applications is handled differently
       AverageWasteHeat = HeatReclaimRefrigeratedRack(SourceID)%AvailCapacity - &
          HeatReclaimRefrigeratedRack(SourceID)%UsedHVACCoil
       WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR = 1.0d0
     ELSEIF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. CONDENSER_REFRIGERATION)THEN
       AverageWasteHeat = HeatReclaimRefrigCondenser(SourceID)%AvailCapacity - &
          HeatReclaimRefrigCondenser(SourceID)%UsedHVACCoil
       WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR = 1.0d0
     ELSEIF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. COIL_DX_COOLING    .OR. &
            WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTISPEED .OR. &
            WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTIMODE) THEN
       AverageWasteHeat = HeatReclaimDXCoil(SourceID)%AvailCapacity
       WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR = DXCoil(SourceID)%PartLoadRatio
     END IF
  ELSE
     AverageWasteHeat = 0.0d0
  END IF

! simulate only water heater tank if reclaim heating source is off
  IF(WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR .EQ. 0.0d0)THEN
    CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
    RETURN
  END IF

! If the set point is higher than the maximum water temp, reset both the set point and the dead band temperature difference
  IF(SetPointTemp .GT. WaterHeaterDesuperheater(DesuperheaterNum)%MaxInletWaterTemp)THEN
    CutInTemp    = SetPointTemp - DeadbandTempDiff
    SetPointTemp = WaterHeaterDesuperheater(DesuperheaterNum)%MaxInletWaterTemp
    DeadbandTempDiff = MAX(0.0d0, (SetPointTemp - CutInTemp))
  END IF

! set the water-side mass flow rate
  CpWater   = CPHW(Node(WaterInletNode)%Temp)
  MdotWater = WaterHeaterDesuperheater(DesuperheaterNum)%OperatingWaterFlowRate * RhoH2O(Node(WaterInletNode)%Temp)
  IF(Node(WaterInletNode)%Temp .LE. WaterHeaterDesuperheater(DesuperheaterNum)%MaxInletWaterTemp+Acc)THEN
    QHeatRate   = ((AverageWasteHeat * Effic * HEffFTemp)/ WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR) + &
                   (WaterHeaterDesuperheater(DesuperheaterNum)%PumpElecPower * &
                    WaterHeaterDesuperheater(DesuperheaterNum)%PumpFracToWater)
  END IF

  IF(MdotWater .GT. 0.0d0)THEN
    Node(WaterOutletNode)%Temp = Node(WaterInletNode)%Temp + QHeatRate/(MdotWater * CpWater)
  ELSE
    Node(WaterOutletNode)%Temp = Node(WaterInletNode)%Temp
  END IF

! change to tanktypenum using parameters?
  SELECT CASE(WaterHeaterDesuperheater(DesuperheaterNum)%TankTypeNum)

    CASE(MixedWaterHeater)

      WaterHeaterDesuperheater(DesuperheaterNum)%SaveWHMode = WaterThermalTank(WaterThermalTankNum)%Mode

      SELECT CASE(WaterHeaterDesuperheater(DesuperheaterNum)%Mode)
        CASE(HeatMode)

          PartLoadRatio = WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR

!         set the full load outlet temperature on the water heater source inlet node (init has already been called)
          WaterThermalTank(WaterThermalTankNum)%SourceInletTemp    = Node(WaterOutletNode)%Temp

!         set the source mass flow rate for the tank
          WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = MdotWater * PartLoadRatio

          WaterThermalTank(WaterThermalTankNum)%MaxCapacity        =   &
             WaterHeaterDesuperheater(DesuperheaterNum)%BackupElementCapacity
          WaterThermalTank(WaterThermalTankNum)%MinCapacity        =   &
             WaterHeaterDesuperheater(DesuperheaterNum)%BackupElementCapacity

          CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
          NewTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp

          IF(NewTankTemp .GT. SetPointTemp) THEN
!           Only revert to floating mode if the tank temperature is higher than the cut out temperature
            IF(NewTankTemp .GT. WaterHeaterDesuperheater(DesuperheaterNum)%SetpointTemp)THEN
              WaterHeaterDesuperheater(DesuperheaterNum)%Mode = FloatMode
            END IF
            Par(1) = SetPointTemp
            Par(2) = WaterHeaterDesuperheater(DesuperheaterNum)%SaveWHMode
            Par(3) = WaterThermalTankNum
            IF(FirstHVACIteration) THEN
              Par(4) = 1.0d0
            ELSE
              Par(4) = 0.0d0
            END IF
            Par(5) = MdotWater
            CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadRatio, PLRResidualMixedTank, 0.0d0, &
                                  WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR, Par)
            IF (SolFla == -1) THEN
              WRITE(IterNum,*) MaxIte
              IterNum=ADJUSTL(IterNum)
              IF(.NOT. WarmupFlag)THEN
                WaterHeaterDesuperheater(DesuperheaterNum)%IterLimitExceededNum1 = &
                  WaterHeaterDesuperheater(DesuperheaterNum)%IterLimitExceededNum1 + 1
                IF (WaterHeaterDesuperheater(DesuperheaterNum)%IterLimitExceededNum1 .EQ. 1) THEN
                  CALL ShowWarningError(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                            //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//'"')
                  CALL ShowContinueError('Iteration limit exceeded calculating desuperheater unit part-load ratio, '// &
                                        'maximum iterations = '//TRIM(IterNum)// &
                                        '. Part-load ratio returned = '//TRIM(RoundSigDigits(PartLoadRatio,3)))
                  CALL ShowContinueErrorTimeStamp('This error occurred in heating mode.')
                ELSE
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                                        //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                  '":  Iteration limit exceeded in heating mode warning continues. Part-load ratio statistics follow.' &
                  , WaterHeaterDesuperheater(DesuperheaterNum)%IterLimitErrIndex1, PartLoadRatio, PartLoadRatio)
                END IF
              END IF
            ELSE IF (SolFla == -2) THEN
              PartLoadRatio = MAX(0.0d0,MIN(WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR, &
                                         (SetPointTemp - TankTemp)/(NewTankTemp - TankTemp)))
              IF(.NOT. WarmupFlag)THEN
                WaterHeaterDesuperheater(DesuperheaterNum)%RegulaFalsiFailedNum1 = &
                  WaterHeaterDesuperheater(DesuperheaterNum)%RegulaFalsiFailedNum1 + 1
                IF (WaterHeaterDesuperheater(DesuperheaterNum)%RegulaFalsiFailedNum1 .EQ. 1) THEN
                  CALL ShowWarningError(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                            //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//'"')
                  CALL ShowContinueError('Desuperheater unit part-load ratio calculation failed: PLR limits ' &
                                   //'of 0 to 1 exceeded. Part-load ratio used = '//TRIM(RoundSigDigits(PartLoadRatio,3)))
                  CALL ShowContinueError('Please send this information to the EnergyPlus support group.')
                  CALL ShowContinueErrorTimeStamp('This error occured in heating mode.')
                ELSE
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                                        //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                  '":  Part-load ratio calculation failed in heating mode warning continues. Part-load ratio statistics follow.'&
                  , WaterHeaterDesuperheater(DesuperheaterNum)%RegulaFalsiFailedIndex1, PartLoadRatio, PartLoadRatio)
                END IF
              END IF
            END IF
            NewTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
          ELSE
            PartLoadRatio = WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR
          END IF

        CASE(FloatMode)

!         check tank temperature by setting source inlet mass flow rate to zero
          PartLoadRatio = 0.0d0

!         set the full load outlet temperature on the water heater source inlet node (init has already been called)
          WaterThermalTank(WaterThermalTankNum)%SourceInletTemp = Node(WaterOutletNode)%Temp

!         check tank temperature by setting source inlet mass flow rate to zero
          WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = 0.0d0

!         disable the tank heater to find PLR of the HPWH
          WaterThermalTank(WaterThermalTankNum)%MaxCapacity = 0.0d0
          WaterThermalTank(WaterThermalTankNum)%MinCapacity = 0.0d0

          CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
          NewTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp

          IF(NewTankTemp .LE. (SetPointTemp - DeadbandTempDiff)) THEN
            WaterHeaterDesuperheater(DesuperheaterNum)%Mode = HeatMode
            WaterThermalTank(WaterThermalTankNum)%Mode = WaterHeaterDesuperheater(DesuperheaterNum)%SaveWHMode
            IF((Tanktemp - NewTankTemp) .NE. 0.0d0) THEN
              PartLoadRatio = MIN(WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR, &
                              MAX(0.0d0,((SetPointTemp - DeadbandTempDiff) - NewTankTemp) / (Tanktemp - NewTankTemp)))
            ELSE
              PartLoadRatio = WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR
            END IF

!           set the full load outlet temperature on the water heater source inlet node
            WaterThermalTank(WaterThermalTankNum)%SourceInletTemp    = Node(WaterOutletNode)%Temp

!           set the source mass flow rate for the tank and enable backup heating element
            WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = MdotWater * PartLoadRatio
            WaterThermalTank(WaterThermalTankNum)%MaxCapacity = WaterHeaterDesuperheater(DesuperheaterNum)%BackupElementCapacity
            WaterThermalTank(WaterThermalTankNum)%MinCapacity = WaterHeaterDesuperheater(DesuperheaterNum)%BackupElementCapacity

            CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
            NewTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
            IF(NewTankTemp .GT. SetPointTemp) THEN
              Par(1) = SetPointTemp
              Par(2) = WaterHeaterDesuperheater(DesuperheaterNum)%SaveWHMode
              Par(3) = WaterThermalTankNum
              IF(FirstHVACIteration) THEN
                Par(4) = 1.0d0
              ELSE
                Par(4) = 0.0d0
              END IF
              Par(5) = MdotWater
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadRatio, PLRResidualMixedTank, 0.0d0, &
                                    WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR, Par)
              IF (SolFla == -1) THEN
                WRITE(IterNum,*) MaxIte
                IterNum=ADJUSTL(IterNum)
                IF(.NOT. WarmupFlag)THEN
                  WaterHeaterDesuperheater(DesuperheaterNum)%IterLimitExceededNum2 = &
                    WaterHeaterDesuperheater(DesuperheaterNum)%IterLimitExceededNum2 + 1
                  IF (WaterHeaterDesuperheater(DesuperheaterNum)%IterLimitExceededNum2 .EQ. 1) THEN
                    CALL ShowWarningError(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                            //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//'"')
                    CALL ShowContinueError('Iteration limit exceeded calculating desuperheater unit part-load ratio, '// &
                                        'maximum iterations = '//TRIM(IterNum)// &
                                        '. Part-load ratio returned = '//TRIM(RoundSigDigits(PartLoadRatio,3)))
                    CALL ShowContinueErrorTimeStamp('This error occurred in float mode.')
                  ELSE
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                                        //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                    '":  Iteration limit exceeded in float mode warning continues. Part-load ratio statistics follow.' &
                    , WaterHeaterDesuperheater(DesuperheaterNum)%IterLimitErrIndex2, PartLoadRatio, PartLoadRatio)
                  END IF
                END IF
              ELSE IF (SolFla == -2) THEN
                PartLoadRatio = MAX(0.0d0,MIN(WaterHeaterDesuperheater(DesuperheaterNum)%DXSysPLR, &
                                           (SetPointTemp - TankTemp)/(NewTankTemp - TankTemp)))
                IF(.NOT. WarmupFlag)THEN
                  WaterHeaterDesuperheater(DesuperheaterNum)%RegulaFalsiFailedNum2 = &
                    WaterHeaterDesuperheater(DesuperheaterNum)%RegulaFalsiFailedNum2 + 1
                  IF (WaterHeaterDesuperheater(DesuperheaterNum)%RegulaFalsiFailedNum2 .EQ. 1) THEN
                    CALL ShowWarningError(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                            //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//'"')
                    CALL ShowContinueError('Desuperheater unit part-load ratio calculation failed: PLR limits ' &
                                   //'of 0 to 1 exceeded. Part-load ratio used = '//TRIM(RoundSigDigits(PartLoadRatio,3)))
                    CALL ShowContinueError('Please send this information to the EnergyPlus support group.')
                    CALL ShowContinueErrorTimeStamp('This error occured in float mode.')
                  ELSE
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Type)//' "' &
                                        //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name)//&
                    '": Part-load ratio calculation failed in float mode warning continues. Part-load ratio statistics follow.' &
                    , WaterHeaterDesuperheater(DesuperheaterNum)%RegulaFalsiFailedIndex2, PartLoadRatio, PartLoadRatio)
                  END IF
                END IF
              END IF
            END IF
          ELSE
            WaterThermalTank(WaterThermalTankNum)%MaxCapacity = WaterHeaterDesuperheater(DesuperheaterNum)%BackupElementCapacity
            WaterThermalTank(WaterThermalTankNum)%MinCapacity = WaterHeaterDesuperheater(DesuperheaterNum)%BackupElementCapacity
          END IF

        CASE DEFAULT
      END SELECT

!   should never get here, case is checked in GetWaterThermalTankInput
    CASE DEFAULT
      CALL ShowFatalError('Coil:WaterHeating:Desuperheater = '//TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%Name) &
          //':  invalid water heater tank type and name entered = ' &
          //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%TankType)//', ' &
          //TRIM(WaterHeaterDesuperheater(DesuperheaterNum)%TankName))

  END SELECT

  IF(QHeatRate .EQ. 0)PartLoadRatio = 0.0d0

  Node(WaterOutletNode)%MassFlowRate = MdotWater * PartLoadRatio
  WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTempOutput = HEffFtemp
  WaterHeaterDesuperheater(DesuperheaterNum)%HeaterRate = QHeatRate * PartLoadRatio
  WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = MdotWater * PartLoadRatio

  IF(PartLoadRatio .EQ. 0)THEN
    WaterThermalTank(WaterThermalTankNum)%SourceInletTemp = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp
    Node(WaterOutletNode)%Temp = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp
    WaterHeaterDesuperheater(DesuperheaterNum)%HEffFTempOutput = 0.0d0
    WaterHeaterDesuperheater(DesuperheaterNum)%HeaterRate = 0.0d0
  END IF

  WaterHeaterDesuperheater(DesuperheaterNum)%HeaterEnergy        = WaterHeaterDesuperheater(DesuperheaterNum)%HeaterRate * &
                                                                    TimeStepSys * SecInHour
  WaterHeaterDesuperheater(DesuperheaterNum)%DesuperheaterPLR    = PartLoadRatio
  WaterHeaterDesuperheater(DesuperheaterNum)%OnCycParaFuelRate   = WaterHeaterDesuperheater(DesuperheaterNum)%OnCycParaLoad * &
                                                                    PartLoadRatio
  WaterHeaterDesuperheater(DesuperheaterNum)%OnCycParaFuelEnergy = WaterHeaterDesuperheater(DesuperheaterNum)%OnCycParaFuelRate*&
                                                                    TimeStepSys * SecInHour
  WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaFuelRate   = WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaLoad * &
                                                                    (1-PartLoadRatio)
  WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaFuelEnergy = WaterHeaterDesuperheater(DesuperheaterNum)%OffCycParaFuelRate *&
                                                                    TimeStepSys * SecInHour
  WaterHeaterDesuperheater(DesuperheaterNum)%PumpPower           = WaterHeaterDesuperheater(DesuperheaterNum)%PumpElecPower * &
                                                                    (PartLoadRatio)
  WaterHeaterDesuperheater(DesuperheaterNum)%PumpEnergy          = WaterHeaterDesuperheater(DesuperheaterNum)%PumpPower * &
                                                                    TimeStepSys * SecInHour

! Update remaining waste heat (just in case multiple users of waste heat use same source)
  IF(ValidSourceType(DesuperheaterNum))THEN
     SourceID = WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSourceIndexNum
     IF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. COMPRESSORRACK_REFRIGERATEDCASE)THEN
!    Refrigeration systems are simulated at the zone time step, do not decrement available capacity
          HeatReclaimRefrigeratedRack(SourceID)%UsedWaterHeater = &
             WaterHeaterDesuperheater(DesuperheaterNum)%HeaterRate
     ELSEIF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. CONDENSER_REFRIGERATION)THEN
          HeatReclaimRefrigCondenser(SourceID)%UsedWaterHeater = &
             WaterHeaterDesuperheater(DesuperheaterNum)%HeaterRate
     ELSEIF(WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. COIL_DX_COOLING    .OR. &
            WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTISPEED .OR. &
            WaterHeaterDesuperheater(DesuperheaterNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTIMODE) THEN
       HeatReclaimDXCoil(SourceID)%AvailCapacity = HeatReclaimDXCoil(SourceID)%AvailCapacity - &
           WaterHeaterDesuperheater(DesuperheaterNum)%HeaterRate
     END IF
  END IF

  RETURN

END SUBROUTINE CalcDesuperheaterWaterHeater


SUBROUTINE CalcHeatPumpWaterHeater(WaterThermalTankNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   March 2005
          !       MODIFIED       B. Griffith, Jan 2012 for stratified tank
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a heat pump water heater

          ! METHODOLOGY EMPLOYED:
          ! Simulate the water heater tank, DX coil, and fan to meet the water heating requirements.

          ! USE STATEMENTS:
  USE DataLoopNode,      ONLY: Node
  USE DataHVACGlobals,   ONLY: ShortenTimeStepSys, TimeStepSys, HPWHInletDBTemp, HPWHInletWBTemp, HPWHCrankcaseDBTemp, &
                               BlowThru, CycFanCycCoil, SmallTempDiff
  USE DataGlobals,       ONLY: WarmupFlag, DoingSizing, KickOffSimulation
  USE DataInterfaces,    ONLY: ShowFatalError, ShowSevereError, ShowWarningError, ShowContinueError, &
                               ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
  USE DXCoils,           ONLY: SimDXCoil, CalcHPWHDXCoil
  USE Fans,              ONLY: SimulateFanComponents
  USE ScheduleManager,   ONLY: GetCurrentScheduleValue
  USE General,           ONLY: SolveRegulaFalsi, RoundSigDigits
  USE Psychrometrics,    ONLY: CPHW, PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb, RhoH2O !, PsyWFnTdbTwbPb

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum          ! Water Heater tank being simulated
  LOGICAL, INTENT(IN) :: FirstHVACIteration      ! TRUE if First iteration of simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte = 500            ! maximum number of iterations
  REAL(r64), PARAMETER :: Acc =  0.001D0          ! Accuracy of result from RegulaFalsi

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: AvailSchedule           ! HP compressor availability schedule
  REAL(r64)           :: SetPointTemp            ! HP set point temperature (cut-out temperature, C)
  REAL(r64)           :: DeadbandTempDiff        ! HP dead band temperature difference (C)
  REAL(r64)           :: TankTemp                ! tank temperature before simulation, C
  REAL(r64)           :: NewTankTemp             ! tank temperature after simulation, C
  REAL(r64)           :: CpAir                   ! specific heat of air, kJ/kg/K
  REAL(r64)           :: MdotWater               ! mass flow rate of condenser water, kg/s
  REAL(r64)           :: OutletAirSplitterSch    ! output of outlet air splitter schedule
  INTEGER             :: HPAirInletNode          ! HP air inlet node number
  INTEGER             :: HPAirOutletNode         ! HP air outlet node number
  INTEGER             :: OutdoorAirNode          ! Outdoor air inlet node number
  INTEGER             :: ExhaustAirNode          ! Exhaust air outlet node number
  INTEGER             :: HPWaterInletNode        ! HP condenser water inlet node number
  INTEGER             :: HPWaterOutletNode       ! HP condenser water outlet node number
  INTEGER             :: InletAirMixerNode       ! HP inlet air mixer node number
  INTEGER             :: OutletAirSplitterNode   ! HP outlet air splitter node number
  INTEGER             :: DXCoilAirInletNode      ! Inlet air node number of DX coil
  INTEGER             :: HPNum                   ! Index to heat pump water heater
  INTEGER             :: SolFla                  ! Flag of RegulaFalsi solver
  REAL(r64), DIMENSION(5)  :: Par                     ! Parameters passed to RegulaFalsi
  REAL(r64)           :: HPMinTemp               ! used for error messages, C
  CHARACTER(len=MaxNameLength) :: HPMinTempChar  ! used for error messages
  CHARACTER(len=20)   :: IterNum                 ! Max number of iterations for warning message
  INTEGER             :: CompOp                  ! DX compressor operation; 1=on, 0=off
  REAL(r64)           :: CondenserDeltaT         ! HPWH condenser water temperature difference
  REAL(r64)           :: HPWHCondInletNodeLast   ! Water temp sent from WH on last iteration
  INTEGER             :: loopIter                ! iteration loop counter

          ! FLOW:
! initialize local variables
  HPNum                   = WaterThermalTank(WaterThermalTankNum)%HeatPumpNum
  AvailSchedule           = GetCurrentScheduleValue(HPWaterHeater(HPNum)%AvailSchedPtr)
  HPAirInletNode          = HPWaterHeater(HPNum)%HeatPumpAirInletNode
  HPAirOutletNode         = HPWaterHeater(HPNum)%HeatPumpAirOutletNode
  OutdoorAirNode          = HPWaterHeater(HPNum)%OutsideAirNode
  ExhaustAirNode          = HPWaterHeater(HPNum)%ExhaustAirNode
  HPWaterInletNode        = HPWaterHeater(HPNum)%CondWaterInletNode
  HPWaterOutletNode       = HPWaterHeater(HPNum)%CondWaterOutletNode
  InletAirMixerNode       = HPWaterHeater(HPNum)%InletAirMixerNode
  OutletAirSplitterNode   = HPWaterHeater(HPNum)%OutletAirSplitterNode
  DXCoilAirInletNode      = HPWaterHeater(HPNum)%DXCoilAirInletNode
  HPPartLoadRatio         = 0.0d0
  CompOp                  = 0
  HPWaterHeater(HPNum)%OnCycParaFuelRate    = 0.0d0
  HPWaterHeater(HPNum)%OnCycParaFuelEnergy  = 0.0d0
  HPWaterHeater(HPNum)%OffCycParaFuelRate   = 0.0d0
  HPWaterHeater(HPNum)%OffCycParaFuelEnergy = 0.0d0
  Node(HPWaterOutletNode) = Node(HPWaterInletNode)

! assign set point temperature (cut-out) and dead band temp diff (cut-in = cut-out minus dead band temp diff)
  SetPointTemp      = HPWaterHeater(HPNum)%SetpointTemp
  DeadbandTempDiff  = HPWaterHeater(HPNum)%DeadbandTempDiff

! store first iteration tank temperature and HP mode of operation
! this code can be called more than once with FirstHVACIteration = .TRUE., use FirstTimeThroughFlag to control save
  IF (FirstHVACIteration .AND. .NOT. ShortenTimeStepSys .AND. HPWaterHeater(HPNum)%FirstTimeThroughFlag) THEN
    WaterThermalTank(WaterThermalTankNum)%SavedTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
    HPWaterHeater(HPNum)%SaveMode             = HPWaterHeater(HPNum)%Mode
    HPWaterHeater(HPNum)%SaveWHMode           = WaterThermalTank(WaterThermalTankNum)%Mode
    HPWaterHeater(HPNum)%FirstTimeThroughFlag = .FALSE.
  END IF

  IF(.NOT. FirstHVACIteration)HPWaterHeater(HPNum)%FirstTimeThroughFlag = .TRUE.

! check if HPWH is off for some reason and simulate HPWH air- and water-side mass flow rates of 0
! simulate only water heater tank if HP compressor is scheduled off
  IF(AvailSchedule .EQ. 0.0d0 .OR. &
!   simulate only water heater tank if HP compressor cut-out temperature is lower than the tank's cut-in temperature
    (SetPointTemp - DeadbandTempDiff) .LE. WaterThermalTank(WaterThermalTankNum)%SetpointTemp .OR. &
!    simulate only water heater tank if HP inlet air temperature is below minimum temperature for HP compressor operation
     HPWHInletDBTemp .LT. HPWaterHeater(HPNum)%MinAirTempForHPOperation .OR. &
!    if the tank maximum temperature limit is less than the HPWH set point temp, disable HPWH
     SetPointTemp .GE. WaterThermalTank(WaterThermalTankNum)%TankTempLimit)THEN
!   revert to float mode any time HPWH compressor is OFF
    HPWaterHeater(HPNum)%Mode     = FloatMode
    IF(InletAirMixerNode .GT. 0) THEN
      Node(InletAirMixerNode) = Node(HPAirInletNode)
    END IF
!   pass node info and simulate crankcase heater
    IF(HPWaterHeater(HPNum)%FanPlacement .EQ. BlowThru) THEN
      CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, FirstHVACIteration,HPWaterHeater(HPNum)%FanNum)
      CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName, CompOp, FirstHVACIteration,HPPartLoadRatio, &
                     HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
    ELSE
      CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName, CompOp, FirstHVACIteration,HPPartLoadRatio, &
                     HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
      CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, FirstHVACIteration,HPWaterHeater(HPNum)%FanNum)
    END IF
    IF(OutletAirSplitterNode .GT. 0) THEN
      Node(HPAirOutletNode) = Node(OutletAirSplitterNode)
    END IF

!   Simulate tank if HP compressor unavailable for water heating
    SELECT CASE(HPWaterHeater(HPNum)%TankTypeNum)

    CASE(MixedWaterHeater)
      CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
    CASE((StratifiedWaterHeater))
      CALL CalcWaterThermalTankStratified(WaterThermalTankNum)
    END SELECT

!   If HPWH compressor is available and unit is off for another reason, off-cycle parasitics are calculated
    IF(AvailSchedule .NE. 0)THEN
      HPWaterHeater(HPNum)%OffCycParaFuelRate   = HPWaterHeater(HPNum)%OffCycParaLoad * (1.0d0-HPPartLoadRatio)
      HPWaterHeater(HPNum)%OffCycParaFuelEnergy = HPWaterHeater(HPNum)%OffCycParaFuelRate * TimeStepSys * SecInHour
    END IF

!   Warn if HPWH compressor cut-in temperature is less than the water heater tank's set point temp
    IF(.NOT. WarmupFlag .AND. .NOT. DoingSizing .AND. .NOT. KickOffSimulation ) THEN
      IF((SetPointTemp - DeadbandTempDiff) .LE. WaterThermalTank(WaterThermalTankNum)%SetpointTemp)THEN
        HPMinTemp = SetPointTemp - DeadbandTempDiff
        WRITE(HPMinTempChar,*) HPMinTemp
        HPWaterHeater(HPNum)%HPSetPointError=HPWaterHeater(HPNum)%HPSetPointError+1
       !!add logic for warmup, kickoffsimulation and doing sizing here
        IF (HPWaterHeater(HPNum)%HPSetPointError .EQ. 1) THEN
          CALL ShowWarningError(TRIM(HPWaterHeater(HPNum)%Type)//' "'//TRIM(HPWaterHeater(HPNum)%Name)//&
              ':  Water heater tank set point temperature is greater than or equal to the cut-in temperature'// &
              ' of the heat pump water heater. Heat Pump will be disabled and simulation continues.')
          CALL ShowContinueErrorTimeStamp(' '//'...Heat Pump cut-in temperature='//TRIM(HPMinTempChar))
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(HPWaterHeater(HPNum)%Type)//' "'//TRIM(HPWaterHeater(HPNum)%Name)//&
            ':  Water heater tank set point temperature is greater than or equal to the cut-in temperature'// &
            ' of the heat pump water heater. Heat Pump will be disabled error continues...' &
            , HPWaterHeater(HPNum)%HPSetPointErrIndex1, HPMinTemp, HPMinTemp)
        END IF
      END IF
    ENDIF
    RETURN
  END IF
  SELECT CASE(HPWaterHeater(HPNum)%TankTypeNum)
  CASE(MixedWaterHeater)
    TankTemp                    = WaterThermalTank(WaterThermalTankNum)%SavedTankTemp
  CASE(StratifiedWaterHeater)
    TankTemp                    = FindStratifiedTankSensedTemp(WaterThermalTankNum, &
                                        HPWaterHeater(HPNum)%ControlSensorLocation)
  END SELECT
  HPWaterHeater(HPNum)%Mode     = HPWaterHeater(HPNum)%SaveMode

! set the heat pump air- and water-side mass flow rate
  MdotWater         = HPWaterHeater(HPNum)%OperatingWaterFlowRate * RhoH2O(TankTemp)

!     select mode of operation (float mode or heat mode)
  SELECT CASE(HPWaterHeater(HPNum)%Mode)
!       HPWH was heating last iteration and will continue to heat until the set point is reached
    CASE(HeatMode)

      HPPartLoadRatio                        = 1.0d0

!         set up full air flow on DX coil inlet node
      Node(DXCoilAirInletNode)%MassFlowRate = MdotAir

!         set the condenser inlet node mass flow rate prior to calling the HPWH DX coil
      Node(HPWaterInletNode)%MassFlowRate = MdotWater
      WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = MdotWater

      HPWHCondInletNodeLast = Node(HPWaterInletNode)%Temp
      DO loopIter = 1, 4
        CALL CalcHPWHDXCoil(HPWaterHeater(HPNum)%DXCoilNum, HPPartLoadRatio)
!       Currently, HPWH heating rate is only a function of inlet evap conditions and air flow rate
!       If HPWH is ever allowed to vary fan speed, this next sub should be called.
!       (possibly with an iteration loop to converge on a solution)
!       CALL CalcDOE2DXCoil(DXCoilNum, HPPartLoadRatio, FirstHVACIteration,PartLoadRatio, FanOpMode)
        CondenserDeltaT = Node(HPWaterOutletNode)%Temp - Node(HPWaterInletNode)%Temp

!           move the full load outlet temperature rate to the water heater structure variables
!           (water heaters source inlet node temperature/mdot are set in Init, set it here after CalcHPWHDXCoil has been called)
        WaterThermalTank(WaterThermalTankNum)%SourceInletTemp    = Node(HPWaterInletNode)%Temp + CondenserDeltaT
        WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = MdotWater

!           this CALL does not update node temps, must use WaterThermalTank variables
      ! select tank type
        SELECT CASE(HPWaterHeater(HPNum)%TankTypeNum)
        CASE(MixedWaterHeater)
          CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
          NewTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
        CASE(StratifiedWaterHeater)
          CALL CalcWaterThermalTankStratified(WaterThermalTankNum)
          NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTankNum, &
                               HPWaterHeater(HPNum)%ControlSensorLocation)
        END SELECT
        Node(HPWaterInletNode)%Temp = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp
        IF(ABS(Node(HPWaterInletNode)%Temp - HPWHCondInletNodeLast) < SmallTempDiff)EXIT
        HPWHCondInletNodeLast = Node(HPWaterInletNode)%Temp
      END DO

!         if tank temperature is greater than set point, calculate a PLR needed to exactly reach the set point
      IF(NewTankTemp .GT. SetPointTemp) THEN
        HPWaterHeater(HPNum)%Mode = FloatMode
        Par(1) = SetPointTemp
        Par(2) = HPWaterHeater(HPNum)%SaveWHMode
        Par(3) = WaterThermalTankNum
        IF(FirstHVACIteration) THEN
          Par(4) = 1.0d0
        ELSE
          Par(4) = 0.0d0
        END IF
        Par(5) = MdotWater
        SELECT CASE(HPWaterHeater(HPNum)%TankTypeNum)
        CASE(MixedWaterHeater)
          CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, HPPartLoadRatio, PLRResidualMixedTank, 0.0d0,    &
                             1.0d0, Par)
        CASE(StratifiedWaterHeater)
          CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, HPPartLoadRatio, PLRResidualStratifiedTank, 0.0d0,    &
                             1.0d0, Par)
        END SELECT
        IF (SolFla == -1) THEN
          WRITE(IterNum,*) MaxIte
          IterNum=ADJUSTL(IterNum)
          IF(.NOT. WarmupFlag)THEN
            HPWaterHeater(HPNum)%IterLimitExceededNum1 = HPWaterHeater(HPNum)%IterLimitExceededNum1 + 1
            IF (HPWaterHeater(HPNum)%IterLimitExceededNum1 .EQ. 1) THEN
              CALL ShowWarningError(TRIM(HPWaterHeater(HPNum)%Type)//' "'//TRIM(HPWaterHeater(HPNum)%Name)//'"')
              CALL ShowContinueError('Iteration limit exceeded calculating heat pump water heater compressor'// &
                                     ' part-load ratio, maximum iterations = '//TRIM(IterNum)// &
                                     '. Part-load ratio returned = '//TRIM(RoundSigDigits(HPPartLoadRatio,3)))
              CALL ShowContinueErrorTimeStamp('This error occurred in heating mode.')
            ELSE
              CALL ShowRecurringWarningErrorAtEnd(TRIM(HPWaterHeater(HPNum)%Type)//' "' &
                                                //TRIM(HPWaterHeater(HPNum)%Name)//&
              '":  Iteration limit exceeded in heating mode warning continues. Part-load ratio statistics follow.' &
              , HPWaterHeater(HPNum)%IterLimitErrIndex1, HPPartLoadRatio, HPPartLoadRatio)
            END IF
          END IF
        ELSE IF (SolFla == -2) THEN
          HPPartLoadRatio = MAX(0.0d0,MIN(1.0d0,(SetPointTemp - TankTemp)/(NewTankTemp - TankTemp)))
          IF(.NOT. WarmupFlag)THEN
            HPWaterHeater(HPNum)%RegulaFalsiFailedNum1 = HPWaterHeater(HPNum)%RegulaFalsiFailedNum1 + 1
            IF (HPWaterHeater(HPNum)%RegulaFalsiFailedNum1 .EQ. 1) THEN
              CALL ShowWarningError(TRIM(HPWaterHeater(HPNum)%Type)//' "'//TRIM(HPWaterHeater(HPNum)%Name)//'"')
              CALL ShowContinueError('Heat pump water heater compressor part-load ratio calculation failed: PLR limits ' &
                               //'of 0 to 1 exceeded. Part-load ratio used = '//TRIM(RoundSigDigits(HPPartLoadRatio,3)))
              CALL ShowContinueError('Please send this information to the EnergyPlus support group.')
              CALL ShowContinueErrorTimeStamp('This error occured in heating mode.')
            ELSE
              CALL ShowRecurringWarningErrorAtEnd(TRIM(HPWaterHeater(HPNum)%Type)//' "' &
                                                //TRIM(HPWaterHeater(HPNum)%Name)//&
              '":  Part-load ratio calculation failed in heating mode warning continues. Part-load ratio statistics follow.'&
              , HPWaterHeater(HPNum)%RegulaFalsiFailedIndex1, HPPartLoadRatio, HPPartLoadRatio)
            END IF
          END IF
        END IF
        SELECT CASE(HPWaterHeater(HPNum)%TankTypeNum)
        CASE(MixedWaterHeater)
          NewTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
        CASE(StratifiedWaterHeater)
          NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTankNum, &
                                        HPWaterHeater(HPNum)%ControlSensorLocation)
        END SELECT
      ELSE
        HPPartLoadRatio = 1.0d0
      END IF

!       HPWH was floating last iteration and will continue to float until the cut-in temperature is reached
    CASE(FloatMode)

!         set the condenser inlet node temperature and full mass flow rate prior to calling the HPWH DX coil
      SELECT CASE(HPWaterHeater(HPNum)%TankTypeNum)
      CASE(MixedWaterHeater)
        Node(HPWaterInletNode)%Temp          = TankTemp
        Node(HPWaterOutletNode)%Temp         = TankTemp
      CASE(StratifiedWaterHeater)
        Node(HPWaterInletNode)%Temp          = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp
        Node(HPWaterOutletNode)%Temp         = WaterThermalTank(WaterThermalTankNum)%SourceInletTemp
      END SELECT
      Node(HPWaterInletNode)%MassFlowRate  = 0.0d0
      Node(HPWaterOutletNode)%MassFlowRate = 0.0d0

!         check tank temperature by setting source inlet mass flow rate to zero
      HPPartLoadRatio = 0.0d0

!         set the full load outlet temperature on the water heater source inlet node (init has already been called)
      WaterThermalTank(WaterThermalTankNum)%SourceInletTemp = Node(HPWaterOutletNode)%Temp

!         check tank temperature by setting source inlet mass flow rate to zero
      WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = 0.0d0

!         disable the tank's internal heating element to find PLR of the HPWH using floating temperatures
      WaterThermalTank(WaterThermalTankNum)%MaxCapacity = 0.0d0
      WaterThermalTank(WaterThermalTankNum)%MinCapacity = 0.0d0
      SELECT CASE(HPWaterHeater(HPNum)%TankTypeNum)
      CASE(MixedWaterHeater)
        CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
        NewTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
      CASE(StratifiedWaterHeater)
        CALL CalcWaterThermalTankStratified(WaterThermalTankNum)
        NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTankNum, &
                                        HPWaterHeater(HPNum)%ControlSensorLocation)
      END SELECT


!         reset the tank's internal heating element capacity
      WaterThermalTank(WaterThermalTankNum)%MaxCapacity = HPWaterHeater(HPNum)%BackupElementCapacity
      WaterThermalTank(WaterThermalTankNum)%MinCapacity = HPWaterHeater(HPNum)%BackupElementCapacity

      IF(NewTankTemp .LE. (SetPointTemp - DeadbandTempDiff)) THEN

!           HPWH is now in heating mode
        HPWaterHeater(HPNum)%Mode = HeatMode
!           reset the water heater's mode (call above may have changed modes)
        WaterThermalTank(WaterThermalTankNum)%Mode = HPWaterHeater(HPNum)%SaveWHMode

!           estimate portion of time step that the HP operates based on a linear interpolation of the tank temperature decay
!           this assumes that all heating sources are off
        IF(Tanktemp .NE. NewTankTemp) THEN
          HPPartLoadRatio = MAX(0.0d0,MIN(1.0d0,((SetPointTemp - DeadbandTempDiff) - NewTankTemp) / (Tanktemp - NewTankTemp)))
        ELSE
          HPPartLoadRatio = 1.0d0
        END IF

!           set the condenser inlet node mass flow rate prior to calling the CalcHPWHDXCoil
        Node(HPWaterInletNode)%MassFlowRate    = MdotWater * HPPartLoadRatio
        WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = MdotWater * HPPartLoadRatio

        Node(DXCoilAirInletNode)%MassFlowRate = MdotAir * HPPartLoadRatio

        HPWHCondInletNodeLast = Node(HPWaterInletNode)%Temp
        DO loopIter = 1, 4
          CALL CalcHPWHDXCoil(HPWaterHeater(HPNum)%DXCoilNum, HPPartLoadRatio)
!         Currently, HPWH heating rate is only a function of inlet evap conditions and air flow rate
!         If HPWH is ever allowed to vary fan speed, this next sub should be called.
!         CALL CalcDOE2DXCoil(DXCoilNum, HPPartLoadRatio, FirstHVACIteration,PartLoadRatio, FanOpMode)
!         (possibly with an iteration loop to converge on a solution)
          CondenserDeltaT = Node(HPWaterOutletNode)%Temp - Node(HPWaterInletNode)%Temp
          WaterThermalTank(WaterThermalTankNum)%SourceInletTemp    = Node(HPWaterInletNode)%Temp + CondenserDeltaT

!             this CALL does not update node temps, must use WaterThermalTank variables
        ! select tank type
          SELECT CASE(HPWaterHeater(HPNum)%TankTypeNum)
          CASE(MixedWaterHeater)
            CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
            NewTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
          CASE(StratifiedWaterHeater)
            CALL CalcWaterThermalTankStratified(WaterThermalTankNum)
            NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTankNum, &
                                        HPWaterHeater(HPNum)%ControlSensorLocation)
          END SELECT
          Node(HPWaterInletNode)%Temp = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp
          IF(ABS(Node(HPWaterInletNode)%Temp - HPWHCondInletNodeLast) < SmallTempDiff)EXIT
          HPWHCondInletNodeLast = Node(HPWaterInletNode)%Temp
        END DO

!           if tank temperature is greater than set point, calculate a PLR needed to exactly reach the set point
        IF(NewTankTemp .GT. SetPointTemp) THEN
          HPWaterHeater(HPNum)%Mode = FloatMode
          Par(1) = SetPointTemp
          Par(2) = HPWaterHeater(HPNum)%SaveWHMode
          Par(3) = WaterThermalTankNum
          IF(FirstHVACIteration) THEN
            Par(4) = 1.0d0
          ELSE
            Par(4) = 0.0d0
          END IF
          Par(5) = MdotWater
          SELECT CASE(HPWaterHeater(HPNum)%TankTypeNum)
          CASE(MixedWaterHeater)
            CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, HPPartLoadRatio, PLRResidualMixedTank, 0.0d0,    &
                               1.0d0, Par)
          CASE(StratifiedWaterHeater)
            CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, HPPartLoadRatio, PLRResidualStratifiedTank, 0.0d0,    &
                               1.0d0, Par)
          END SELECT
          IF (SolFla == -1) THEN
            WRITE(IterNum,*) MaxIte
            IterNum=ADJUSTL(IterNum)
            IF(.NOT. WarmupFlag)THEN
              HPWaterHeater(HPNum)%IterLimitExceededNum2 = HPWaterHeater(HPNum)%IterLimitExceededNum2 + 1
              IF (HPWaterHeater(HPNum)%IterLimitExceededNum2 .EQ. 1) THEN
                CALL ShowWarningError(TRIM(HPWaterHeater(HPNum)%Type)//' "'//TRIM(HPWaterHeater(HPNum)%Name)//'"')
                CALL ShowContinueError('Iteration limit exceeded calculating heat pump water heater compressor'// &
                                       ' part-load ratio, maximum iterations = '//TRIM(IterNum)// &
                                       '. Part-load ratio returned = '//TRIM(RoundSigDigits(HPPartLoadRatio,3)))
                CALL ShowContinueErrorTimeStamp('This error occurred in float mode.')
              ELSE
                CALL ShowRecurringWarningErrorAtEnd(TRIM(HPWaterHeater(HPNum)%Type)//' "' &
                                                  //TRIM(HPWaterHeater(HPNum)%Name)//&
                '":  Iteration limit exceeded in float mode warning continues. Part-load ratio statistics follow.' &
                , HPWaterHeater(HPNum)%IterLimitErrIndex2, HPPartLoadRatio, HPPartLoadRatio)
              END IF
            END IF
          ELSE IF (SolFla == -2) THEN
            HPPartLoadRatio = MAX(0.0d0,MIN(1.0d0,(SetPointTemp - TankTemp)/(NewTankTemp - TankTemp)))
            IF(.NOT. WarmupFlag)THEN
              HPWaterHeater(HPNum)%RegulaFalsiFailedNum2 = HPWaterHeater(HPNum)%RegulaFalsiFailedNum2 + 1
              IF (HPWaterHeater(HPNum)%RegulaFalsiFailedNum2 .EQ. 1) THEN
                CALL ShowWarningError(TRIM(HPWaterHeater(HPNum)%Type)//' "'//TRIM(HPWaterHeater(HPNum)%Name)//'"')
                CALL ShowContinueError('Heat pump water heater compressor part-load ratio calculation failed: PLR limits ' &
                               //'of 0 to 1 exceeded. Part-load ratio used = '//TRIM(RoundSigDigits(HPPartLoadRatio,3)))
                CALL ShowContinueError('Please send this information to the EnergyPlus support group.')
                CALL ShowContinueErrorTimeStamp('This error occured in float mode.')
              ELSE
                CALL ShowRecurringWarningErrorAtEnd(TRIM(HPWaterHeater(HPNum)%Type)//' "' &
                                                  //TRIM(HPWaterHeater(HPNum)%Name)//&
                '": Part-load ratio calculation failed in float mode warning continues. Part-load ratio statistics follow.' &
                , HPWaterHeater(HPNum)%RegulaFalsiFailedIndex2, HPPartLoadRatio, HPPartLoadRatio)
              END IF
            END IF
          END IF
        END IF
      END IF

    CASE DEFAULT
!         Never gets here, only allowed modes for HPWH are float and heat
  END SELECT

! set air-side mass flow rate for final calculation
  IF(InletAirMixerNode .GT. 0) THEN
    Node(InletAirMixerNode)%MassFlowRate = MdotAir * HPPartLoadRatio
    Node(HPAirInletNode)%MassFlowRate    = MdotAir * HPPartLoadRatio * (1.0d0 - MixerInletAirSchedule)
    Node(OutdoorAirNode)%MassFlowRate    = MdotAir * HPPartLoadRatio * MixerInletAirSchedule
!   IF HPWH is off, pass zone node conditions through HPWH air-side
    IF(HPPartLoadRatio .EQ. 0)Node(InletAirMixerNode) = Node(HPAirInletNode)
  ELSE
    IF(OutdoorAirNode .EQ. 0)THEN
      Node(HPAirInletNode)%MassFlowRate    = MdotAir * HPPartLoadRatio
    ELSE
      Node(OutdoorAirNode)%MassFlowRate    = MdotAir * HPPartLoadRatio
    END IF
  END IF
  IF(HPPartLoadRatio .EQ. 0)WaterThermalTank(WaterThermalTankNum)%SourceInletTemp =   &
     WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp

! set water-side mass flow rate for final calculation
  Node(HPWaterInletNode)%MassFlowRate    = MdotWater * HPPartLoadRatio

! pass node information using resulting PLR
  IF(HPWaterHeater(HPNum)%FanPlacement .EQ. BlowThru) THEN
!   simulate fan and DX coil twice to pass PLF (OnOffFanPartLoadFraction) to fan
    CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, FirstHVACIteration,HPWaterHeater(HPNum)%FanNum)
    CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName, CompOp, FirstHVACIteration,HPPartLoadRatio, &
                   HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
    CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, FirstHVACIteration,HPWaterHeater(HPNum)%FanNum)
    CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName, CompOp,FirstHVACIteration, HPPartLoadRatio, &
                   HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
  ELSE
!   simulate DX coil and fan twice to pass fan power (FanElecPower) to DX coil
    CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName, CompOp, FirstHVACIteration,HPPartLoadRatio, &
                   HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
    CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, FirstHVACIteration,HPWaterHeater(HPNum)%FanNum)
    CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName, CompOp, FirstHVACIteration,HPPartLoadRatio, &
                   HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
    CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, FirstHVACIteration,HPWaterHeater(HPNum)%FanNum)
  END IF

! set HPWH outlet node equal to the outlet air splitter node conditions if outlet air splitter node exists
  IF(OutletAirSplitterNode .GT. 0)THEN
    Node(HPAirOutletNode) = Node(OutletAirSplitterNode)
    Node(ExhaustAirNode)  = Node(OutletAirSplitterNode)
  END IF

! Check schedule to divert air-side cooling to outdoors.
  IF(HPWaterHeater(HPNum)%OutletAirSplitterSchPtr .GT. 0)THEN
    OutletAirSplitterSch = GetCurrentScheduleValue(HPWaterHeater(HPNum)%OutletAirSplitterSchPtr)
    Node(HPAirOutletNode)%MassFlowRate = MdotAir * HPPartLoadRatio * (1.0d0 - OutletAirSplitterSch)
    Node(ExhaustAirNode)%MassFlowRate  = MdotAir * HPPartLoadRatio * OutletAirSplitterSch
  END IF

  HPWaterHeater(HPNum)%HeatingPLR           = HPPartLoadRatio
  HPWaterHeater(HPNum)%OnCycParaFuelRate    = HPWaterHeater(HPNum)%OnCycParaLoad * HPPartLoadRatio
  HPWaterHeater(HPNum)%OnCycParaFuelEnergy  = HPWaterHeater(HPNum)%OnCycParaFuelRate * TimeStepSys * SecInHour
  HPWaterHeater(HPNum)%OffCycParaFuelRate   = HPWaterHeater(HPNum)%OffCycParaLoad * (1.0d0-HPPartLoadRatio)
  HPWaterHeater(HPNum)%OffCycParaFuelEnergy = HPWaterHeater(HPNum)%OffCycParaFuelRate * TimeStepSys * SecInHour

  SELECT CASE (HPWaterHeater(HPNum)%InletAirConfiguration)

!   no sensible capacity to zone for outdoor and scheduled HPWH
    CASE (AmbientTempOutsideAir)
      HPWaterHeater(HPNum)%HPWaterHeaterSensibleCapacity = 0.0d0
      HPWaterHeater(HPNum)%HPWaterHeaterLatentCapacity   = 0.0d0

    CASE (AmbientTempSchedule)
      HPWaterHeater(HPNum)%HPWaterHeaterSensibleCapacity = 0.0d0
      HPWaterHeater(HPNum)%HPWaterHeaterLatentCapacity   = 0.0d0

!   calculate sensible capacity to zone for inlet air configuration equals Zone Only or Zone And Outdoor Air configurations
    CASE DEFAULT
      CpAir = PsyCpAirFnWTdb(Node(HPAirInletNode)%HumRat,Node(HPAirInletNode)%Temp)

!     add parasitics to zone heat balance if parasitic heat load is to zone otherwise neglect parasitics
      IF(HPWaterHeater(HPNum)%ParasiticTempIndicator .EQ. AmbientTempZone)THEN
        HPWaterHeater(HPNum)%HPWaterHeaterSensibleCapacity = (Node(HPAirOutletNode)%MassFlowRate * CpAir * &
                                        (Node(HPAirOutletNode)%Temp - Node(HPAirInletNode)%Temp)) + &
                                         HPWaterHeater(HPNum)%OnCycParaFuelRate + HPWaterHeater(HPNum)%OffCycParaFuelRate
      ELSE
        HPWaterHeater(HPNum)%HPWaterHeaterSensibleCapacity = Node(HPAirOutletNode)%MassFlowRate * CpAir * &
                                       (Node(HPAirOutletNode)%Temp - Node(HPAirInletNode)%Temp)
      END IF

      HPWaterHeater(HPNum)%HPWaterHeaterLatentCapacity = Node(HPAirOutletNode)%MassFlowRate * &
                                    (Node(HPAirOutletNode)%HumRat - Node(HPAirInletNode)%HumRat)

  END SELECT

  RETURN

END SUBROUTINE CalcHeatPumpWaterHeater


REAL(r64) FUNCTION PLRResidualMixedTank(HPPartLoadRatio, Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   May 2005
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function (desired tank temp - actual tank temp)
          !  HP water heater output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcWaterThermalTankMixed to get tank temperature at the given part load ratio (source water mass flow rate)
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: NumPlantLoops

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: HPPartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = HP set point temperature [C]
                                                  ! par(2) = tank mode
                                                  ! par(3) = water heater num
                                                  ! par(4) = FirstHVACIteration
                                                  ! par(5) = MdotWater

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WaterThermalTankNum     ! index of water heater
  REAL(r64)    :: NewTankTemp        ! resulting tank temperature [C]
  LOGICAL :: FirstHVACIteration ! FirstHVACIteration flag

  WaterThermalTankNum = INT(Par(3))
  WaterThermalTank(WaterThermalTankNum)%Mode = INT(Par(2))
  WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = Par(5) * HPPartLoadRatio
  ! FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
  IF(Par(4) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  CALL CalcWaterThermalTankMixed(WaterThermalTankNum)
  NewTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
  PLRResidualMixedTank = Par(1) - NewTankTemp
  RETURN

END FUNCTION PLRResidualMixedTank

REAL(r64) FUNCTION PLRResidualStratifiedTank(HPPartLoadRatio, Par)
          ! FUNCTION INFORMATION:
          !       AUTHOR         B.Griffith,  Richard Raustad
          !       DATE WRITTEN   Jan 2012
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function (desired tank temp - actual tank temp)
          !  HP water heater output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcWaterThermalTankStratified to get tank temperature at the given part load ratio (source water mass flow rate)
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: NumPlantLoops

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: HPPartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = HP set point temperature [C]
                                                  ! par(2) = tank mode
                                                  ! par(3) = water heater num
                                                  ! par(4) = FirstHVACIteration
                                                  ! par(5) = MdotWater

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WaterThermalTankNum     ! index of water heater
  REAL(r64)    :: NewTankTemp        ! resulting tank temperature [C]
  LOGICAL :: FirstHVACIteration ! FirstHVACIteration flag

  WaterThermalTankNum = INT(Par(3))
  WaterThermalTank(WaterThermalTankNum)%Mode = INT(Par(2))
  WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate = Par(5) * HPPartLoadRatio
  ! FirstHVACIteration is a logical, Par is real, so make 1.0=TRUE and 0.0=FALSE
  IF(Par(4) .EQ. 1.0d0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  CALL CalcWaterThermalTankStratified(WaterThermalTankNum)
  NewTankTemp = FindStratifiedTankSensedTemp(WaterThermalTankNum, &
                        HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%ControlSensorLocation)
  PLRResidualStratifiedTank = Par(1) - NewTankTemp
  RETURN

END FUNCTION PLRResidualStratifiedTank

REAL(r64) FUNCTION PlantMassFlowRatesFunc(WaterThermalTankNum, InNodeNum, FirstHVACIteration, &
                                WaterThermalTankSide, &
                                PlantLoopSide, PlumbedInSeries, BranchControlType, &
                                OutletTemp, DeadBandTemp, SetpointTemp)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! collect routines for setting flow rates for Water heaters
          ! with plant connections.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode,      ONLY: Node
  USE DataBranchAirLoopPlant
  USE ScheduleManager,   ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum     !
  INTEGER, INTENT(IN) :: InNodeNum          !
  LOGICAL, INTENT(IN) :: FirstHVACIteration !
  Integer, INTENT(IN) :: WaterThermalTankSide    !
  INTEGER, INTENT(IN) :: PlantLoopSide      !
  LOGICAL, INTENT(IN) :: PlumbedInSeries    ! !unused1208
  INTEGER, INTENT(IN) :: BranchControlType  !
  REAL(r64)   , INTENT(IN) :: OutletTemp         !
  REAL(r64)   , INTENT(IN) :: DeadBandTemp       !
  REAL(r64)   , INTENT(IN) :: SetpointTemp       !



          ! FUNCTION PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: PassingFlowThru      = 1
  INTEGER, PARAMETER :: MaybeRequestingFlow  = 2
  INTEGER, PARAMETER :: ThrottlingFlow       = 3

          ! FUNCTION BLOCK SPECIFICATIONS:
          ! na

          ! FUNCTION TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CurrentMode
  REAL(r64) :: MassFlowRequest
  LOGICAL   :: NeedsHeat
  LOGICAL   :: NeedsCool
  REAL(r64) :: FlowResult
  LOGICAL   :: ScheduledAvail
  REAL(r64) :: AltSetpointTemp
  REAL(r64) :: AltDeadBandTemp

  NeedsHeat = .FALSE.  ! init
  NeedsCool = .FALSE.  ! init

  ! determine current mode.  there are three possible
  !  1.  passing thru what was given to inlet node
  !  2.  potentially making a flow request
  !  3.  throttling flow in response to Plant's restrictions (MassFlowRateMaxAvail)
  ! init default mode to passing thru
  CurrentMode=PassingFlowThru   ! default

  If (PlantLoopSide == DemandSupply_No) then
    CurrentMode = PassingFlowThru
  Else If (PlantLoopSide == SupplySide) then
  ! If FlowLock is False (0), the tank sets the plant loop mdot
  ! If FlowLock is True (1),  the new resolved plant loop mdot is used
    If (WaterThermalTank(WaterThermalTankNum)%UseCurrentFlowLock == 0) Then
      CurrentMode = PassingFlowThru
      IF ((WaterThermalTank(WaterThermalTankNum)%UseSideLoadRequested > 0.0D0) &
                 .AND. (WaterThermalTankSide == UseSide) ) THEN
        CurrentMode = MaybeRequestingFlow
      ENDIF
    ELSE
      CurrentMode = PassingFlowThru

    ENDIF
    IF (WaterThermalTankSide == SourceSide) THEN
      CurrentMode = MaybeRequestingFlow
    ENDIF
  ELSE IF (PlantLoopSide == DemandSide) then
    !  1.  pass thru is default
    CurrentMode = PassingFlowThru

    !  2.  Might be Requesting Flow.
    IF (FirstHVACIteration) THEN
        IF (BranchControlType == ControlType_Bypass) THEN
         CurrentMode = PassingFlowThru
        ELSE
         ! IF (.not. PlumbedInSeries) THEN
            CurrentMode = MaybeRequestingFlow
         ! ELSE
         !   CurrentMode = PassingFlowThru
         ! ENDIF
        ENDIF
      ! ENDIF
    ELSE !(.not. FirstHVACIteration)
        IF (BranchControlType == ControlType_Bypass) THEN
          CurrentMode = PassingFlowThru
        ELSE
            ! 3.  ThrottlingFlow
        !  IF (.not. PlumbedInSeries) THEN
            CurrentMode = ThrottlingFlow
        !  ELSE
        !    CurrentMode = PassingFlowThru
        !  ENDIF

        ENDIF
    ENDIF
  ENDIF

  ! evaluate Availability schedule,
  ScheduledAvail = .TRUE.
  IF (WaterThermalTankSide == UseSide) THEN
!    IF (WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum > 0) Then
      IF (GetCurrentScheduleValue(WaterThermalTank(WaterThermalTankNum)%UseSideAvailSchedNum) == 0.0D0) Then
        ScheduledAvail = .FALSE.
      ENDIF
!    ENDIF
  ELSE IF (WaterThermalTankSide == SourceSide) THEN
!    IF (WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum > 0) Then
      IF (GetCurrentScheduleValue(WaterThermalTank(WaterThermalTankNum)%SourceSideAvailSchedNum) == 0.0D0) Then
        ScheduledAvail = .FALSE.
      ENDIF
!    ENDIF
  END IF

  ! now act based on current mode
  SELECT CASE (currentMode)

  CASE (PassingFlowThru)
    IF (.NOT. ScheduledAvail ) Then
      FlowResult = 0.0D0
    ELSE
      FlowResult = Node(InNodeNum)%MassFlowRate
    ENDIF

  CASE (ThrottlingFlow)
        ! first determine what mass flow would be if it is to requested
    IF (.NOT. ScheduledAvail ) Then
      MassFlowRequest = 0.0D0
    ELSE
      IF (WaterThermalTankSide == UseSide) THEN
        MassFlowRequest = WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax
      ELSE IF (WaterThermalTankSide == SourceSide) THEN
        MassFlowRequest = WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax
      END IF
    ENDIF

    ! next determine if tank temperature is such that source side flow might be requested
    IF (.NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank) THEN
      IF (OutletTemp < DeadBandTemp) THEN
        NeedsHeat = .TRUE.
      ELSE IF ((OutletTemp >= DeadBandTemp) .and. (OutletTemp < SetpointTemp)) THEN
        ! inside the deadband, use saved mode from water heater calcs
        IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == HeatMode) THEN
          NeedsHeat = .TRUE.
        ELSE IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == FloatMode) THEN
          NeedsHeat = .FALSE.
        ENDIF
      ELSE IF (OutletTemp >= SetpointTemp) THEN
       NeedsHeat = .FALSE.
      ENDIF
    ELSE ! is a chilled water tank so flip logic
      IF (OutletTemp > DeadBandTemp) THEN
        NeedsCool = .TRUE.
      ELSE IF ((OutletTemp <= DeadBandTemp) .and. (OutletTemp > SetpointTemp)) THEN
        ! inside the deadband, use saved mode from water thermal tank calcs (modes only for mixed)
        IF (WaterThermalTank(WaterThermalTankNum)%TypeNum == MixedChilledWaterStorage) THEN
          IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == CoolMode) THEN
            NeedsCool = .TRUE.
          ELSE IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == FloatMode) THEN
            NeedsCool = .FALSE.
          ENDIF
        ELSEIF (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedChilledWaterStorage) THEN
          NeedsCool = .TRUE.

        ENDIF

      ELSE IF (OutletTemp <= SetpointTemp) THEN
       NeedsCool = .FALSE.
      ENDIF
    ENDIF

    IF (MassFlowRequest > 0.0D0) THEN
      IF (WaterThermalTankSide == UseSide) THEN
        FlowResult = MassFlowRequest
      ELSE IF (WaterThermalTankSide == SourceSide) THEN
        IF (NeedsHeat .or. NeedsCool) THEN
          FlowResult = MassFlowRequest
        ELSE
          FlowResult = 0.0D0
        ENDIF
      END IF
    ELSE
     FlowResult = 0.0D0
    ENDIF

    ! now throttle against MassFlowRateMaxAvail, MassFlowRateMinAvail, MassFlowRateMax, and MassFlowRateMin
    ! see notes about reverse dd compliance (specifically 5ZoneWaterSystems file)
    FlowResult = MAX(Node(InNodeNum)%MassFlowRateMinAvail, FlowResult)  ! okay for compliance (reverse dd)
    FlowResult = MAX(Node(InNodeNum)%MassFlowRateMin,      FlowResult)  ! okay for compliance (reverse dd)
    FlowResult = MIN(Node(InNodeNum)%MassFlowRateMaxAvail, FlowResult)
!=> following might take out of reverse dd compliance
    FlowResult = MIN(Node(InNodeNum)%MassFlowRateMax,      FlowResult)

    !DSU> use SetComponentFlowRate for above?

  CASE (MaybeRequestingFlow)

    ! first determine what mass flow would be if it is to requested
    IF (.NOT. ScheduledAvail ) Then
      MassFlowRequest = 0.0D0
    ELSE
      IF (WaterThermalTankSide == UseSide) THEN
        IF ((WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank)  &
             .AND. (WaterThermalTank(WaterThermalTankNum)%UseSideLoadRequested > 0.0D0)) THEN
          MassFlowRequest = WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax
        ELSEIF ((WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank) &
             .AND. (WaterThermalTank(WaterThermalTankNum)%UseSideLoadRequested == 0.0D0)) THEN
          MassFlowRequest = 0.0D0
        ELSE
          MassFlowRequest = WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax
        ENDIF

      ELSE IF (WaterThermalTankSide == SourceSide) THEN
        MassFlowRequest = WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax
      END IF
    ENDIF

    IF (WaterThermalTankSide == SourceSide) THEN ! temperature dependent controls for indirect heating/cooling
      IF (.NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank) THEN
        ! next determine if tank temperature is such that flow is requested depending on mode
        IF (WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode == SourceSideIndirectHeatPrimarySetpoint) THEN
          IF (OutletTemp < DeadBandTemp) THEN
            NeedsHeat = .TRUE.
          ELSE IF ((OutletTemp >= DeadBandTemp) .and. (OutletTemp < SetpointTemp)) THEN
            ! inside the deadband, use saved mode from water heater calcs
            IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == HeatMode) THEN
              NeedsHeat = .TRUE.
            ELSE IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == FloatMode) THEN
              NeedsHeat = .FALSE.
            ENDIF

          ELSE IF (OutletTemp >= SetpointTemp) THEN
           NeedsHeat = .FALSE.
          ENDIF
        ELSEIF (WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode == SourceSideIndirectHeatAltSetpoint) THEN
          ! get alternate setpoint 
          AltSetpointTemp = GetCurrentScheduleValue(WaterThermalTank(WaterThermalTankNum)%SourceSideAltSetpointSchedNum)
          AltDeadBandTemp =  AltSetpointTemp  - WaterThermalTank(WaterThermalTankNum)%DeadbandDeltaTemp
          IF (OutletTemp < AltDeadBandTemp) THEN
            NeedsHeat = .TRUE.
          ELSE IF ((OutletTemp >= AltDeadBandTemp) .and. (OutletTemp < AltSetpointTemp)) THEN
            ! inside the deadband, use saved mode from water heater calcs
            IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == HeatMode) THEN
              NeedsHeat = .TRUE.
            ELSE IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == FloatMode) THEN
              NeedsHeat = .FALSE.
            ENDIF

          ELSE IF (OutletTemp >= AltSetpointTemp) THEN
           NeedsHeat = .FALSE.
          ENDIF
        ELSEIF (WaterThermalTank(WaterThermalTankNum)%SourceSideControlMode == SourceSideStorageTank) THEN
          IF (OutletTemp < WaterThermalTank(WaterThermalTankNum)%TankTempLimit) THEN
            NeedsHeat = .TRUE.
          ELSE
            NeedsHeat = .FALSE.
          ENDIF

        ENDIF
      ELSE ! is a chilled water tank so flip logic
        IF (OutletTemp > DeadBandTemp) THEN
          NeedsCool = .TRUE.
        ELSE IF ((OutletTemp <= DeadBandTemp) .and. (OutletTemp > SetpointTemp)) THEN
        ! inside the deadband, use saved mode from water thermal tank calcs (modes only for mixed)
          IF (WaterThermalTank(WaterThermalTankNum)%TypeNum == MixedChilledWaterStorage) THEN
            IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == CoolMode) THEN
              NeedsCool = .TRUE.
            ELSE IF (WaterThermalTank(WaterThermalTankNum)%SavedMode == FloatMode) THEN
              NeedsCool = .FALSE.
            ENDIF
          ELSEIF (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedChilledWaterStorage) THEN
            NeedsCool = .TRUE.
          ENDIF
        ELSE IF (OutletTemp >= SetpointTemp) THEN
         NeedsCool = .FALSE.
        ENDIF

      ENDIF ! chilled water

      IF (MassFlowRequest > 0.0D0) THEN
        IF (NeedsHeat .OR. NeedsCool) THEN
          FlowResult = MassFlowRequest
        ELSE
          FlowResult = 0.0D0
        ENDIF
      ELSE
        FlowResult = 0.0D0
      ENDIF
    Else ! end source side, begin use side
      IF (MassFlowRequest > 0.0D0) THEN
        FlowResult = MassFlowRequest
      ELSE
        FlowResult = 0.0D0
      ENDIF
    ENDIF
!    IF (FirstHVACIteration) Then
!      Node(InNodeNum)%MassFlowRateMaxAvail = FlowResult
!      Node(InNodeNum)%MassFlowRateMinAvail = 0.0D0
!    ENDIF

  END SELECT

  PlantMassFlowRatesFunc = FlowResult

  RETURN

END FUNCTION PlantMassFlowRatesFunc

SUBROUTINE MinePlantStructForInfo(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! get information from plant loop data structure
          ! check what we can learn from plant structure against user inputs

          ! METHODOLOGY EMPLOYED:
          ! looping

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals , ONLY: NumPlantLoops
  USE DataInterfaces, ONLY: ShowFatalError, ShowSevereError, ShowContinueError
  USE DataSizing, ONLY: AutoSize

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, Intent(In) :: WaterThermalTankNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PlantLoopNum            ! Used for looking up plant info
  INTEGER             :: LoopSideNum             ! Used for looking up plant info
!unused  INTEGER             :: BranchNum               ! Used for looking up plant info
!  INTEGER             :: CompNum                 ! Used for looking up plant info
  INTEGER             :: SplitNum                ! used for checking series parallel in plant
  INTEGER             :: UseInletNode            ! Water heater use inlet node number
  INTEGER             :: SourceInletNode         ! Water heater source inlet node number
  Logical             :: errorsFound

  errorsFound = .FALSE.

  !IF (WaterThermalTank(WaterThermalTankNum)%PlantStructureCheck .AND. ALLOCATED(PlantLoop)) THEN
  IF ( ALLOCATED(PlantLoop) .and. WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) THEN

    ! check plant structure for useful data.

    UseInletNode  = WaterThermalTank(WaterThermalTankNum)%UseInletNode
    PlantLoopNum=WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum
    LoopSideNum=WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide

    IF ((WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate == AutoSize) .AND. &
        (WaterThermalTank(WaterThermalTankNum)%UseSidePlantSizNum  ==  0)) THEN
      CALL ShowSevereError('Water heater = '//trim(WaterThermalTank(WaterThermalTankNum)%Name)//&
                            ' for autosizing Use side flow rate, did not find Sizing:Plant object '//  &
                           TRIM(PlantLoop(PlantLoopNum)%Name) )
      ErrorsFound = .true.
    ENDIF
    !Is this wh Use side plumbed in series (default) or are there other branches in parallel?
    IF (ALLOCATED(PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Splitter)) THEN
      DO SplitNum = 1, PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%NumSplitters
        If (ANY(PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%NodeNumOut &
               == UseInletNode) ) THEN ! this wh is on the splitter
          If (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%TotalOutletNodes > 1) THEN
            WaterThermalTank(WaterThermalTankNum)%UseSideSeries = .FALSE.
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  IF ( ALLOCATED(PlantLoop) .and. WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum > 0) THEN
    SourceInletNode  = WaterThermalTank(WaterThermalTankNum)%SourceInletNode
    PlantLoopNum=WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum
    LoopSideNum=WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide
    ! was user's input correct for plant loop name?
    IF ((WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate == AutoSize) .AND. &
        (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantSizNum  == 0)    .AND. &
        (WaterThermalTank(WaterThermalTankNum)%DesuperheaterNum == 0)          .AND. &
        (WaterThermalTank(WaterThermalTankNum)%HeatPumpNum == 0) ) THEN
      CALL ShowSevereError('Water heater = '//trim(WaterThermalTank(WaterThermalTankNum)%Name)//&
                            'for autosizing Source side flow rate, did not find Sizing:Plant object '//  &
                           TRIM(PlantLoop(PlantLoopNum)%Name))
        ErrorsFound = .true.
    ENDIF
    !Is this wh Source side plumbed in series (default) or are there other branches in parallel?
    IF (ALLOCATED(PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Splitter)) THEN
      DO SplitNum = 1, PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%NumSplitters
        IF (ANY(PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%NodeNumOut &
            == SourceInletNode) ) THEN ! this wh is on the splitter
          IF (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%TotalOutletNodes > 1) THEN
            WaterThermalTank(WaterThermalTankNum)%SourceSideSeries = .FALSE.
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDIF

  IF (errorsFound) THEN
    CALL ShowFatalError('Preceding water heater input errors cause program termination')
  ENDIF

  RETURN

END SUBROUTINE MinePlantStructForInfo

SUBROUTINE SizeSupplySidePlantConnections(WaterThermalTankNum, LoopNum, LoopSideNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing water heater plant connection flow rates
          ! on the supply that have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! This routine is called later in the simulation than the sizing routine for the demand side
          !  because the simulation needs to be further along before the needed data are available.
          ! For water heaters sides on Supply loopside, obtains hot water flow rate from the plant sizing array
          !  (adapted approach from boiler sizing routines)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop
  USE DataInterfaces, ONLY: ShowFatalError, ShowSevereError, ShowContinueError, ShowWarningError
  USE DataHVACGlobals, ONLY: SmallWaterVolFlow, NumPlantLoops
  USE FluidProperties,  ONLY: GetDensityGlycol
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum
  INTEGER, INTENT(IN), OPTIONAL :: LoopNum
  INTEGER, INTENT(IN), OPTIONAL :: LoopSideNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  INTEGER   :: DummyWaterIndex = 1
  REAL(r64) :: rho !temporary fluid density
  INTEGER   :: tmpLoopNum
  INTEGER   :: tmpLoopSideNum
  REAL(r64) :: tmpUseDesignVolFlowRate ! local use side design flow rate
  REAL(r64) :: tmpSourceDesignVolFlowRate ! local source side design flow rate

  PltSizNum = 0
  ErrorsFound =  .FALSE.
  tmpUseDesignVolFlowRate = WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate
  tmpSourceDesignVolFlowRate = WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate

  IF (.NOT. PRESENT(LoopSideNum)) THEN
    tmpLoopSideNum = SupplySide
  ELSE
    tmpLoopSideNum = LoopSideNum
  ENDIF
  IF (.NOT. PRESENT(LoopNum)) THEN
    tmpLoopNum = WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum
  ELSE
    tmpLoopNum = LoopNum
  ENDIF

  IF ((WaterThermalTank(WaterThermalTankNum)%UseInletNode > 0) &
      .AND. (tmpLoopNum == WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum )) THEN
    IF (WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate == AutoSize) THEN
      PltSizNum = WaterThermalTank(WaterThermalTankNum)%UseSidePlantSizNum
      IF (PltSizNum > 0) THEN ! we have a Plant Sizing Object
        IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide == SupplySide) THEN
          IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
            IF (PlantSizesOkayToFinalize) THEN
              WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate
            ELSE
              tmpUseDesignVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate
            ENDIF
          ELSE
            IF (PlantSizesOkayToFinalize) THEN
              WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate = 0.d0
            ELSE
              tmpUseDesignVolFlowRate = 0.d0
            ENDIF
          END IF
          IF (PlantSizesOkayToFinalize)   &
             CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type, WaterThermalTank(WaterThermalTankNum)%Name, &
                                'Use Side Design Flow Rate [m3/s]', &
                                WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate)
          IF (PlantSizesOkayToFinalize) THEN
            CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%UseInletNode,  &
                                              WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate)
          ELSE
            CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%UseInletNode,  &
                                                tmpUseDesignVolFlowRate)
          ENDIF

          rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                 'SizeSupplySidePlantConnections')
          IF (PlantSizesOkayToFinalize) THEN
            WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax    = &
                 WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    * rho
          ELSE
            WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax  = tmpUseDesignVolFlowRate * rho
          ENDIF
        END IF
      ELSE
         ! do nothing
      ENDIF !plant sizing object
    ELSE
      CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%UseInletNode,    &
          WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate)
      IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) THEN
        rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                   'SizeSupplySidePlantConnections')
      ELSE
        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'SizeSupplySidePlantConnections')
      ENDIF

      WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax    = &
                 WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    * rho

    END IF !autosizing needed.
  ENDIF ! connected to plant



  IF ((WaterThermalTank(WaterThermalTankNum)%SourceInletNode > 0) &
      .AND. (tmpLoopNum == WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum )) THEN
    IF (WaterThermalTank(WaterThermalTankNum)%sourceDesignVolFlowRate == AutoSize) THEN
      PltSizNum = WaterThermalTank(WaterThermalTankNum)%SourceSidePlantSizNum
      IF (PltSizNum > 0) THEN
        IF (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide == SupplySide) THEN
          IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
            IF (PlantSizesOkayToFinalize) THEN
              WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate
            ELSE
              tmpSourceDesignVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate
            ENDIF
          ELSE
            IF (PlantSizesOkayToFinalize) THEN
              WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate = 0.0d0
            ELSE
              tmpSourceDesignVolFlowRate = 0.d0
            ENDIF
          END IF
          IF (PlantSizesOkayToFinalize)   &
             CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type, WaterThermalTank(WaterThermalTankNum)%Name, &
                                                     'Source Side Design Flow Rate [m3/s]', &
                                                     WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate)
          IF (PlantSizesOkayToFinalize) THEN
            CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%SourceInletNode,  &
                                WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate)
          ELSE
            CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%SourceInletNode, tmpSourceDesignVolFlowRate)
          ENDIF
          rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidIndex, &
                                 'SizeSupplySidePlantConnections')
          IF (PlantSizesOkayToFinalize) THEN
            WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax = &
                           WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate * rho
          ELSE
            WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax = tmpSourceDesignVolFlowRate * rho
          ENDIF
        END IF ! plant loop allocation
      ELSE
        ! do nothing
      ENDIF !plant sizing object
    ELSE
      IF (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide == SupplySide) THEN
        CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%SourceInletNode,  &
                 WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate)
        IF (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum > 0) THEN
          rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidIndex, &
                                   'SizeSupplySidePlantConnections')
        ELSE
          rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'SizeSupplySidePlantConnections')
        ENDIF
        WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax = &
                WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate * rho
      ENDIF
    END IF !autosizing needed.
  ENDIF ! connected to plant

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeSupplySidePlantConnections

SUBROUTINE SizeTankForDemandSide(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   February 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing water heater tank volume and heater
          !  as best we can at this point in simulation. (prior to demand side
          !  sizing that needs volume).

          ! METHODOLOGY EMPLOYED:
          !  depending on the sizing design mode...
          !

          ! REFERENCES:
          ! BA benchmark report for residential design mode

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE DataHeatBalance, ONLY: zone
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop
  USE DataGlobals, ONLY: PI
  USE DataInterfaces, ONLY: ShowFatalError, ShowSevereError, ShowContinueError, ShowWarningError
  USE DataHVACGlobals, ONLY: SmallWaterVolFlow, NumPlantLoops
  USE FluidProperties, ONLY: GetDensityGLycol, GetSpecificHeatGlycol
  USE OutputReportPredefined
  USE SolarCollectors, ONLY: Collector, NumOfCollectors
  USE DataSurfaces, ONLY: Surface

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
  Real(r64), PARAMETER :: GalTocubicMeters = 0.0037854D0
  Real(r64), PARAMETER :: kBtuPerHrToWatts = 293.1D0
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Tstart        ! initial tank temp for sizing.
  REAL(r64) :: Tfinish       ! final target temp for sizing
  Real(r64) :: SumPeopleAllZones
  Real(r64) :: SumFloorAreaAllZones
!unused  INTEGER   :: CollectorNum  ! do loop index
  Logical   :: SizeVolume = .FALSE.
  LOGICAL   :: SizeMaxCapacity = .FALSE.
  REAL(r64) :: rho
  REAL(r64) :: Cp
  INTEGER   :: DummyWaterIndex = 1
  REAL(r64) :: tmpTankVolume ! local temporary for tank volume m3
  REAL(r64) :: tmpMaxCapacity ! local temporary for heating capacity W
  LOGICAL   :: FuelTypeIsLikeGas = .FALSE.

  ! local inits
  Tstart  = 14.44d0
  TFinish = 57.22d0
  SizeVolume = .FALSE.
  SizeMaxCapacity = .FALSE.

  tmpTankVolume = WaterThermalTank(WaterThermalTankNum)%Volume
  tmpMaxCapacity = WaterThermalTank(WaterThermalTankNum)%MaxCapacity
  IF (tmpTankVolume == Autosize) SizeVolume = .TRUE.
  IF (tmpMaxCapacity == Autosize) SizeMaxCapacity = .TRUE.

  SELECT CASE (WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode)

  CASE (SizeNotSet)

  CASE (SizePeakDraw)

  CASE (SizeResidentialMin)

    ! assume can propagate rules for gas to other fuels.
    FuelTypeIsLikeGas = .FALSE.
    IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Gas')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Diesel')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Gasoline')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Coal')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'FuelOil#1')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'FuelOil#2')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Propane')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Steam')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'OtherFuel1')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'OtherFuel2')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'DistrictHeating')) THEN
      FuelTypeIsLikeGas = .TRUE.
    ENDIF

    If (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 1 ) then
      If (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) then
        If (SizeVolume)      tmpTankVolume   = 20.0D0 * GalTocubicMeters
        If (SizeMaxCapacity) tmpMaxCapacity  = 2.5D0 * 1000.0D0  !2.5 kW
      else if (FuelTypeIsLikeGas) then
        If (SizeVolume)      tmpTankVolume  = 20.0D0 * GalTocubicMeters
        If (SizeMaxCapacity) tmpMaxCapacity = 27.0D0 * kBtuPerHrToWatts !27kBtu/hr
      endif

    Else If (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 2 ) THEN
      If (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms <= 1.5D0) THEN
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 30.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 3.5D0 * 1000.0D0  !3.5 kW
        ELSE IF (FuelTypeIsLikeGas) THEN
          If (SizeVolume)      tmpTankVolume  = 30.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
        ENDIF
      ELSE IF ((WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms > 1.5D0) &
               .and. (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms < 3.0D0)) then
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 4.5D0 * 1000.0D0  !4.5 kW
        ELSE IF (FuelTypeIsLikeGas ) THEN
          If (SizeVolume)      tmpTankVolume  = 30.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
        ENDIF
      ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms >= 3.0D0) then
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
        ELSE IF (FuelTypeIsLikeGas ) THEN
          If (SizeVolume)      tmpTankVolume = 40.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
        ENDIF
      ENDIF
    else if (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 3 ) then
      If (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms <= 1.5D0) THEN
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 4.5D0 * 1000.0D0  !4.5 kW
        ELSE IF (FuelTypeIsLikeGas ) THEN
          If (SizeVolume)      tmpTankVolume  = 30.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
        ENDIF
      ELSE IF ((WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms > 1.5D0) &
               .and. (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms < 3.0D0)) then
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
        ELSE IF (FuelTypeIsLikeGas ) THEN
          If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
        ENDIF
      ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms >= 3.0D0) then
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
        ELSE IF (FuelTypeIsLikeGas ) THEN
          If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 38.0D0 * kBtuPerHrToWatts !38 kBtu/hr
        ENDIF
      ENDIF
    else if (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 4 ) then
      If (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms <= 1.5D0) THEN
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
        ELSE IF (FuelTypeIsLikeGas) THEN
          If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
        ENDIF
      ELSE IF ((WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms > 1.5D0) &
               .and. (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms < 3.0D0)) then
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
        ELSE IF (FuelTypeIsLikeGas ) THEN
          If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 38.0D0 * kBtuPerHrToWatts !38 kBtu/hr
        ENDIF
      ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms >= 3.0D0) then
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 66.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
        ELSE IF (FuelTypeIsLikeGas ) THEN
          If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 38.0D0 * kBtuPerHrToWatts !38 kBtu/hr
        ENDIF
      ENDIF
    else if (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 5 ) then
      If (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) then
        If (SizeVolume)      tmpTankVolume  = 66.0D0 * GalTocubicMeters
        If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
      else if (FuelTypeIsLikeGas ) then
        If (SizeVolume)      tmpTankVolume = 50.0D0 * GalTocubicMeters
        If (SizeMaxCapacity) tmpMaxCapacity = 47.0D0 * kBtuPerHrToWatts !47 kBtu/hr
      endif
    else if (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms >= 6 ) then
      If (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) then
        If (SizeVolume)      tmpTankVolume  = 66.0D0 * GalTocubicMeters
        If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
      else if (FuelTypeIsLikeGas ) then
        If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
        If (SizeMaxCapacity) tmpMaxCapacity = 50.0D0 * kBtuPerHrToWatts !50 kBtu/hr
      endif
    ENDIF

    IF (SizeVolume .AND. PlantSizesOkayToFinalize) THEN
       WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
       CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                               WaterThermalTank(WaterThermalTankNum)%Name, &
                              'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
    ENDIF
    IF (SizeMaxCapacity .AND. PlantSizesOkayToFinalize) THEN
       WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
       CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
       WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
    ENDIF
  CASE (SizePerPerson)
    ! how to get number of people?

    SumPeopleAllZones = SUM(Zone%TotOccupants)
    If (SizeVolume) tmpTankVolume =   &
       WaterThermalTank(WaterThermalTankNum)%sizing%TankCapacityPerPerson &
                                                         * SumPeopleAllZones
    IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                   ((Tfinish + Tstart)/2.0D0), &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                   'SizeTankForDemandSide')
      Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                   ((Tfinish + Tstart)/2.0D0), &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                   'SizeTankForDemandSide')
    ELSE
      rho = GetDensityGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeTankForDemandSide')
      Cp  = GetSpecificHeatGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeTankForDemandSide')
    ENDIF

    IF (SizeMaxCapacity)    tmpMaxCapacity  = SumPeopleAllZones   &
                                            * WaterThermalTank(WaterThermalTankNum)%sizing%RecoveryCapacityPerPerson & !m3/hr/person
                                            * (Tfinish - Tstart) & ! delta T  in K
                                            * (1.0D0 / SecInHour)  & !  1 hr/ 3600 s
                                            * rho &  ! kg/m3
                                            * Cp  ! J/Kg/k
    IF (SizeVolume .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                              WaterThermalTank(WaterThermalTankNum)%Name, &
                              'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
    ENDIF
    IF (SizeMaxCapacity .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                              WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
    ENDIF
  CASE (SizePerFloorArea)

    SumFloorAreaAllZones = SUM(Zone%FloorArea)
    If (SizeVolume) tmpTankVolume =   &
           WaterThermalTank(WaterThermalTankNum)%sizing%TankCapacityPerArea &
                                                         * SumFloorAreaAllZones
    IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                   ((Tfinish + Tstart)/2.0D0), &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                   'SizeTankForDemandSide')
      Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                   ((Tfinish + Tstart)/2.0D0), &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                   'SizeTankForDemandSide')
    ELSE
      rho = GetDensityGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeTankForDemandSide')
      Cp  = GetSpecificHeatGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeTankForDemandSide')
    ENDIF

    If (SizeMaxCapacity) tmpMaxCapacity     = SumFloorAreaAllZones   & ! m2
                                            * WaterThermalTank(WaterThermalTankNum)%sizing%RecoveryCapacityPerArea & !m3/hr/m2
                                            * (Tfinish - Tstart) & ! delta T  in K
                                            * (1.0D0 / SecInHour)  & !  1 hr/ 3600 s
                                            * rho &  ! kg/m3
                                            * Cp  ! J/Kg/k
    IF (SizeVolume .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                              WaterThermalTank(WaterThermalTankNum)%Name, &
                             'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
    ENDIF
    IF (SizeMaxCapacity .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,  &
                                               WaterThermalTank(WaterThermalTankNum)%Name, &
                             'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
    ENDIF
  CASE (SizePerUnit)

    If (SizeVolume) tmpTankVolume  =   &
         WaterThermalTank(WaterThermalTankNum)%sizing%TankCapacityPerUnit &
                                       * WaterThermalTank(WaterThermalTankNum)%sizing%NumberOfUnits
    IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                   ((Tfinish + Tstart)/2.0D0), &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                   'SizeTankForDemandSide')
      Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                   ((Tfinish + Tstart)/2.0D0), &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                   'SizeTankForDemandSide')
    ELSE
      rho = GetDensityGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeTankForDemandSide')
      Cp  = GetSpecificHeatGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeTankForDemandSide')
    ENDIF
    If (SizeMaxCapacity)  tmpMaxCapacity =   &
       WaterThermalTank(WaterThermalTankNum)%sizing%NumberOfUnits   &
                                            * WaterThermalTank(WaterThermalTankNum)%sizing%RecoveryCapacityPerUnit & !m3/hr/ea
                                            * (Tfinish - Tstart) & ! delta T  in K
                                            * (1.0D0 / SecInHour)  & !  1 hr/ 3600 s
                                            * rho &  ! kg/m3
                                            * Cp ! J/Kg/k
    IF (SizeVolume .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,  &
                              WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
    ENDIF
    IF (SizeMaxCapacity .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,  &
                              WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
    ENDIF
  CASE (SizePerSolarColArea)

  END SELECT

  ! if stratified, might set height.
  IF ((SizeVolume) .AND. (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedWaterHeater) &
       .AND. PlantSizesOkayToFinalize ) THEN ! might set height
    IF ((WaterThermalTank(WaterThermalTankNum)%Height == Autosize) .AND.   &
       (WaterThermalTank(WaterThermalTankNum)%Volume /= autosize )) THEN
      WaterThermalTank(WaterThermalTankNum)%Height = ( ( 4.0D0 * WaterThermalTank(WaterThermalTankNum)%Volume  &
                                            * (WaterThermalTank(WaterThermalTankNum)%sizing%HeightAspectRatio**2) ) &
                                            / Pi)** 0.33333333333333D0
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type, WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Tank Height [m]', WaterThermalTank(WaterThermalTankNum)%Height )
      ! check if autocalculate Use outlet and source inlet are still set to autosize by earlier
      IF (WaterThermalTank(WaterThermalTankNum)%UseOutletHeight == Autosize) THEN
        WaterThermalTank(WaterThermalTankNum)%UseOutletHeight = WaterThermalTank(WaterThermalTankNum)%Height
      ENDIF
      IF (WaterThermalTank(WaterThermalTankNum)%SourceInletHeight == Autosize) THEN
        WaterThermalTank(WaterThermalTankNum)%SourceInletHeight = WaterThermalTank(WaterThermalTankNum)%Height
      ENDIF
    ENDIF
  ENDIF

  RETURN
END SUBROUTINE SizeTankForDemandSide

SUBROUTINE SizeTankForSupplySide(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   February 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing water heater tank volume and heater
          !   at a later point in the simulation when more of the plant is ready.

          ! METHODOLOGY EMPLOYED:
          !  depending on the sizing design mode...
          !

          ! REFERENCES:
          ! BA benchmark report for residential design mode

          ! USE STATEMENTS:
  USE DataSizing,            ONLY: AutoSize
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE OutputReportPredefined
  USE SolarCollectors, ONLY: Collector, NumOfCollectors
  USE DataSurfaces, ONLY: Surface
  USE DataGlobals, ONLY: Pi
  USE DataInterfaces, ONLY: ShowFatalError
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
  Real(r64), PARAMETER :: GalTocubicMeters = 0.0037854D0
  Real(r64), PARAMETER :: kBtuPerHrToWatts = 293.1D0
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Tstart        ! initial tank temp for sizing.
  REAL(r64) :: Tfinish       ! final target temp for sizing
  INTEGER   :: CollectorNum
  Logical   :: SizeVolume = .FALSE.
  LOGICAL   :: SizeMaxCapacity = .FALSE.
  REAL(r64) :: rho
  REAL(r64) :: Cp
  INTEGER   :: DummyWaterIndex = 1
  REAL(r64) :: tmpTankVolume ! local temporary for tank volume m3
  REAL(r64) :: tmpMaxCapacity ! local temporary for heating capacity W

  ! local inits
  Tstart  = 14.44d0
  TFinish = 57.22d0
  SizeVolume = .FALSE.
  SizeMaxCapacity = .FALSE.

  tmpTankVolume = WaterThermalTank(WaterThermalTankNum)%Volume
  tmpMaxCapacity = WaterThermalTank(WaterThermalTankNum)%MaxCapacity
  If (tmpTankVolume == Autosize) SizeVolume = .TRUE.
  If (tmpMaxCapacity == Autosize) SizeMaxCapacity = .TRUE.

  SELECT CASE (WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode)

  CASE (SizePeakDraw)
    If (SizeVolume) tmpTankVolume      =   &
       WaterThermalTank(WaterThermalTankNum)%Sizing%TankDrawTime  &  ! hours
                                              * WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate & ! m3/s
                                              * SecInHour  !  (3600 s/1 hour)
    IF (SizeVolume .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                           WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
    ENDIF
    If (SizeMaxCapacity) THEN
      IF (WaterThermalTank(WaterThermalTankNum)%Sizing%RecoveryTime > 0.0d0) THEN
        IF (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum > 0) THEN
          rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidName, &
                                   ((Tfinish + Tstart)/2.0D0), &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidIndex, &
                                   'SizeTankForSupplySide')
          Cp = GetSpecificHeatGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidName, &
                                   ((Tfinish + Tstart)/2.0D0), &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidIndex, &
                                   'SizeTankForSupplySide')
        ELSE
          rho = GetDensityGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeTankForSupplySide')
          Cp  = GetSpecificHeatGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeTankForSupplySide')
        ENDIF
          tmpMaxCapacity =  ( WaterThermalTank(WaterThermalTankNum)%Volume & ! m3
                                               * rho & ! kg/m3
                                               * Cp  &  ! J/Kg/K
                                               * (Tfinish - Tstart)) & !  K
                                              / (WaterThermalTank(WaterThermalTankNum)%Sizing%RecoveryTime * SecInHour) ! seconds
      ELSE
        CALL ShowFatalError('SizeTankForSupplySide: Tank="'//TRIM(WaterThermalTank(WaterThermalTankNum)%Name)//  &
          '", requested sizing for max capacity but entered Recovery Time is zero.')
      ENDIF
    ENDIF

    IF (SizeMaxCapacity .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                              WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
    ENDIF
  CASE (SizePerSolarColArea)

    WaterThermalTank(WaterThermalTankNum)%sizing%TotalSolarCollectorArea = 0.0D0
    DO CollectorNum = 1, NumOfCollectors
      WaterThermalTank(WaterThermalTankNum)%sizing%TotalSolarCollectorArea =   &
         WaterThermalTank(WaterThermalTankNum)%sizing%TotalSolarCollectorArea &
                                         + Surface( Collector(CollectorNum)%Surface )%Area !
    ENDDO

    IF (SizeVolume) tmpTankVolume =   &
       WaterThermalTank(WaterThermalTankNum)%sizing%TotalSolarCollectorArea &
                                         * WaterThermalTank(WaterThermalTankNum)%sizing%TankCapacityPerCollectorArea
    IF (SizeMaxCapacity) tmpMaxCapacity = 0.0D0
    IF (SizeVolume .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                              WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
    ENDIF
    IF (SizeMaxCapacity .AND. PlantSizesOkayToFinalize) THEN
      WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                              WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
    ENDIF
  END SELECT

  IF ((SizeVolume) .AND. (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedWaterHeater)&
       .AND. PlantSizesOkayToFinalize ) THEN ! might set height
    IF ((WaterThermalTank(WaterThermalTankNum)%Height == Autosize) .AND.   &
       (WaterThermalTank(WaterThermalTankNum)%Volume /= autosize )) THEN
      WaterThermalTank(WaterThermalTankNum)%Height = ( ( 4.0D0 * WaterThermalTank(WaterThermalTankNum)%Volume  &
                                            * (WaterThermalTank(WaterThermalTankNum)%sizing%HeightAspectRatio**2) ) &
                                            / Pi)** 0.33333333333333D0
      CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type, WaterThermalTank(WaterThermalTankNum)%Name, &
                           'Tank Height [m]', WaterThermalTank(WaterThermalTankNum)%Height )
    ENDIF
  ENDIF

  RETURN
END SUBROUTINE SizeTankForSupplySide

SUBROUTINE SizeDemandSidePlantConnections(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing water heater plant connection flow rates
          ! on the demand side that have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! For water heater sides on the Demand side, hot water flow rates are modeled entirely from user input data
          ! because the plant loop is not yet set up nor is plant sizing info populated.
          ! sizing is done by calculating an initial
          !  recovery rate that if continued would reheat tank in user specified amount of time.
          !  intial and final tank temperatures are 14.44 and reheat to 57.22 (values from CalcStandardRatings routine)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop
  USE DataInterfaces, ONLY: ShowFatalError, ShowSevereError, ShowContinueError, ShowWarningError
  USE DataHVACGlobals, ONLY: SmallWaterVolFlow, NumPlantLoops
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(R64)           :: TankVolume    ! local variable for tank volume.
  REAL(r64)           :: Tpdesign      ! plant sizing exit temperature
  REAL(r64)           :: Tstart        ! initial tank temp for sizing.
  REAL(r64)           :: Tfinish       ! final target temp for sizing
  REAL(r64)           :: tankRecoverhours ! parameter in sizing, hours to recover
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  LOGICAL             :: ErrorsFound   ! If errors detected in input

  REAL(r64)           :: eff           ! temporary effectiveness value for heat exchanger inside tank

  INTEGER      :: DummyWaterIndex = 1
  REAL(r64)    :: rho
  REAL(r64)    :: Cp
  REAL(r64)    :: tmpUseDesignVolFlowRate ! local use side design flow rate
  REAL(r64)    :: tmpSourceDesignVolFlowRate ! local use side design flow rate

  tankRecoverhours = WaterThermalTank(WaterThermalTankNum)%SizingRecoveryTime
  PltSizNum = 0
  ErrorsFound = .FALSE.
  tmpUseDesignVolFlowRate = WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate
  tmpSourceDesignVolFlowRate = WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate

  IF ( .NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank  ) THEN
    Tstart = 14.44d0
    TFinish = 57.22d0
  ELSE
    Tstart = 14.44d0
    TFinish = 9.0d0
  endif

  ! determine tank volume to use for sizing.
  TankVolume = WaterThermalTank(WaterThermalTankNum)%Volume
  If (TankVolume == Autosize) then
    TankVolume = WaterThermalTank(WaterThermalTankNum)%Sizing%NominalVolForSizingDemandSideFlow
  ENDIF


  IF (WaterThermalTank(WaterThermalTankNum)%UseInletNode > 0) THEN
    IF (WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate == AutoSize) THEN
      PltSizNum = WaterThermalTank(WaterThermalTankNum)%UseSidePlantSizNum
      IF (PltSizNum > 0) THEN ! we have a Plant Sizing Object
        IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopSide == DemandSide) THEN
          ! probably shouldn't come here as Use side is unlikley to be on demand side (?)
          ! but going to treat component with symetry so if connections are reversed it'll still work
          ! choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
          ! in user specified hours.
          !  using the plant inlet design temp for sizing.
          Tpdesign = PlantSizData(PltSizNum)%ExitTemp
          eff      = WaterThermalTank(WaterThermalTankNum)%UseEffectiveness
          IF ((Tpdesign >= 58.0d0) .AND. ( .NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank  )) THEN
            IF (PlantSizesOkayToFinalize) THEN
              WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate =       &
                 -1.0d0 *  (TankVolume                &
                   / (tankRecoverhours * SecInHour * eff) )                   &
                   * Log((Tpdesign - Tfinish)/(Tpdesign - Tstart) )
            ELSE
              tmpUseDesignVolFlowRate =       &
                 -1.0d0 *  (TankVolume                &
                   / (tankRecoverhours * SecInHour * eff) )                   &
                   * Log((Tpdesign - Tfinish)/(Tpdesign - Tstart) )
            ENDIF
          ELSEIF ((Tpdesign <= 8.0D0) .AND. ( WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank  )) THEN
            IF (PlantSizesOkayToFinalize) THEN
              WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate =       &
                  -1.0d0 *  (TankVolume                &
                   / (tankRecoverhours * SecInHour * eff) )                   &
                   * Log((Tpdesign - Tfinish)/(Tpdesign - Tstart) )
            ELSE
              tmpUseDesignVolFlowRate =        &
                  -1.0d0 *  (TankVolume                &
                   / (tankRecoverhours * SecInHour * eff) )                   &
                   * Log((Tpdesign - Tfinish)/(Tpdesign - Tstart) )
            ENDIF
          ELSE
            IF ( .NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank  ) THEN
            ! plant sizing object design temperature is set too low throw warning.
              CALL ShowSevereError('Autosizing of Use side water heater design flow rate requires ' &
                    // 'Sizing:Plant object to have an exit temperature >= 58C')
              CALL ShowContinueError('Occurs for water heater object='//TRIM(WaterThermalTank(WaterThermalTankNum)%Name))
            ELSE
            ! plant sizing object design temperature is set too hi throw warning.
              CALL ShowSevereError('Autosizing of Use side chilled water tank design flow rate requires ' &
                    // 'Sizing:Plant object to have an exit temperature <= 8C')
              CALL ShowContinueError('Occurs for chilled water storage tank object='//  &
                 TRIM(WaterThermalTank(WaterThermalTankNum)%Name))

            ENDIF
            ErrorsFound = .TRUE.
          END IF
          IF (PlantSizesOkayToFinalize)   &
             CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type, WaterThermalTank(WaterThermalTankNum)%Name, &
                                              'Use Side Design Flow Rate [m3/s]', &
                                              WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate)
          IF (PlantSizesOkayToFinalize) THEN
            CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%UseInletNode,  &
                                             WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate)
          ELSE
            CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%UseInletNode,  tmpUseDesignVolFlowRate)
          ENDIF
          rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                 'SizeDemandSidePlantConnections')
          IF (PlantSizesOkayToFinalize) THEN
            WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax    = &
                       WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    * rho
          ELSE
            WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax    = tmpUseDesignVolFlowRate  * rho
          ENDIF
        END IF ! Demand side
      ELSE
        ! do nothing
      ENDIF !plant sizing object

    ELSE
    ! not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
      CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%UseInletNode,  &
         WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate)
      IF (WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum > 0) THEN
        rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%UseSidePlantLoopNum)%FluidIndex, &
                                   'SizeDemandSidePlantConnections')
      ELSE
        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'SizeDemandSidePlantConnections')
      ENDIF
      WaterThermalTank(WaterThermalTankNum)%PlantUseMassFlowRateMax    = &
                WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate    * rho
    END IF !autosizing needed.
  ENDIF ! connected to plant


  IF (WaterThermalTank(WaterThermalTankNum)%SourceInletNode > 0) THEN
    IF (WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate == AutoSize) THEN
      PltSizNum = WaterThermalTank(WaterThermalTankNum)%SourceSidePlantSizNum
      IF (PltSizNum > 0) THEN
        IF (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopSide == DemandSide) then
          !  choose a flow rate that will allow the entire volume of the tank to go from 14.44 to 57.22 C
          ! in user specified hours.
          !  using the plant inlet design temp for sizing.
          Tpdesign = PlantSizData(PltSizNum)%ExitTemp
          eff      = WaterThermalTank(WaterThermalTankNum)%SourceEffectiveness
          IF ((Tpdesign >= 58.0d0) .AND. ( .NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank  )) THEN

            IF (PlantSizesOkayToFinalize) THEN
              WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate =                                    &
                  -1.0d0 * (TankVolume/ (tankRecoverhours * SecInHour * eff) ) *  &
                  Log((Tpdesign - Tfinish)/(Tpdesign - Tstart) )
            ELSE
              tmpSourceDesignVolFlowRate =                               &
                  -1.0d0 * (TankVolume/ (tankRecoverhours * SecInHour * eff) ) *  &
                  Log((Tpdesign - Tfinish)/(Tpdesign - Tstart) )
            ENDIF
          ELSEIF ((Tpdesign <= 8.0D0) .AND. ( WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank  )) THEN
            IF (PlantSizesOkayToFinalize) THEN
              WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate =       &
                  -1.0d0 * (TankVolume  / (tankRecoverhours * SecInHour * eff) )                   &
                   * Log((Tpdesign - Tfinish)/(Tpdesign - Tstart) )
            ELSE
              tmpSourceDesignVolFlowRate =       &
                  -1.0d0 * (TankVolume  / (tankRecoverhours * SecInHour * eff) )                   &
                   * Log((Tpdesign - Tfinish)/(Tpdesign - Tstart) )
            ENDIF
          ELSE
            IF ( .NOT. WaterThermalTank(WaterThermalTankNum)%IsChilledWaterTank  ) THEN
            ! plant sizing object design temperature is set too low throw warning.
              CALL ShowSevereError('Autosizing of Source side water heater design flow rate requires ' &
                  // 'Sizing:Plant object to have an exit temperature >= 58C')
              CALL ShowContinueError('Occurs for WaterHeater:Mixed object='//TRIM(WaterThermalTank(WaterThermalTankNum)%Name))
            ELSE
            ! plant sizing object design temperature is set too hi throw warning.
              CALL ShowSevereError('Autosizing of Source side chilled water tank design flow rate requires ' &
                    // 'Sizing:Plant object to have an exit temperature <= 8C')
              CALL ShowContinueError('Occurs for chilled water storage tank object='//  &
                 TRIM(WaterThermalTank(WaterThermalTankNum)%Name))
            ENDIF
            ErrorsFound = .TRUE.
          END IF
          IF (PlantSizesOkayToFinalize)   &
             CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type, WaterThermalTank(WaterThermalTankNum)%Name, &
                     'Source Side Design Flow Rate [m3/s]', &
                     WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate)
          IF (PlantSizesOkayToFinalize) THEN
            CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%SourceInletNode,  &
                                               WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate)
          ELSE
            CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%SourceInletNode,  &
                                               tmpSourceDesignVolFlowRate)
          ENDIF
          rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidIndex, &
                                 'SizeDemandSidePlantConnections')
          IF (PlantSizesOkayToFinalize) THEN
            WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax = &
                WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate * rho
          ELSE
            WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax = &
                tmpSourceDesignVolFlowRate * rho
          ENDIF
        END IF ! demand side
      ELSE
        ! do nothing
      ENDIF !plant sizing object

    ELSE
    ! not autosized - report flow to RegisterPlantCompDesignFlow for supply side component sizing
      CALL RegisterPlantCompDesignFlow( WaterThermalTank(WaterThermalTankNum)%SourceInletNode,  &
         WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate)
      IF (WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum > 0) THEN
        rho = GetDensityGlycol(PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(WaterThermalTank(WaterThermalTankNum)%SourceSidePlantLoopNum)%FluidIndex, &
                                   'SizeDemandSidePlantConnections')
      ELSE
        rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'SizeDemandSidePlantConnections')
      ENDIF
      WaterThermalTank(WaterThermalTankNum)%PlantSourceMassFlowRateMax = &
                  WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate * rho
    END IF !autosizing needed.
  ENDIF ! connected to plant

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeDemandSidePlantConnections

SUBROUTINE SizeStandAloneWaterHeater(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   October 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! allow autosizing of tank volume and heat capacity for stand alone tanks

          ! METHODOLOGY EMPLOYED:
          ! same as for plant connected water heaters, only draws are scheduled. 

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing,      ONLY: AutoSize
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE ScheduleManager, ONLY: GetScheduleMaxValue
  USE InputProcessor,  ONLY: SameString
  USE DataHeatBalance, ONLY: Zone
  USE SolarCollectors, ONLY: Collector, NumOfCollectors
  USE DataSurfaces,    ONLY: Surface

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  Real(r64), PARAMETER :: GalTocubicMeters = 0.0037854D0
  Real(r64), PARAMETER :: kBtuPerHrToWatts = 293.1D0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpTankVolume ! local temporary for tank volume m3
  REAL(r64) :: tmpMaxCapacity ! local temporary for heating capacity W
  REAL(r64) :: Tstart        ! initial tank temp for sizing.
  REAL(r64) :: Tfinish       ! final target temp for sizing
  Logical   :: SizeVolume = .FALSE.
  LOGICAL   :: SizeMaxCapacity = .FALSE.
  LOGICAL   :: FuelTypeIsLikeGas = .FALSE.
  INTEGER   :: DummyWaterIndex = 1
  REAL(r64) :: rho
  REAL(r64) :: Cp
  REAL(r64) :: DrawDesignVolFlowRate
  REAL(r64) :: SumPeopleAllZones
  REAL(r64) :: SumFloorAreaAllZones
  INTEGER   :: CollectorNum

  ! local inits
  Tstart  = 14.44d0
  TFinish = 57.22d0
  SizeVolume = .FALSE.
  SizeMaxCapacity = .FALSE.

  tmpTankVolume = WaterThermalTank(WaterThermalTankNum)%Volume
  tmpMaxCapacity = WaterThermalTank(WaterThermalTankNum)%MaxCapacity
  If (tmpTankVolume == Autosize) SizeVolume = .TRUE.
  If (tmpMaxCapacity == Autosize) SizeMaxCapacity = .TRUE.

  IF (SizeVolume .OR. SizeMaxCapacity) THEN

    SELECT CASE (WaterThermalTank(WaterThermalTankNum)%Sizing%DesignMode)


    CASE (SizePeakDraw)
      ! get draw rate from maximum in schedule
      rho = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, 'SizeStandAloneWaterHeater')
      DrawDesignVolFlowRate = GetScheduleMaxValue(WaterThermalTank(WaterThermalTankNum)%FlowRateSchedule) &
                              * WaterThermalTank(WaterThermalTankNum)%MassFlowRateMax &
                              / rho 

      If (SizeVolume) THEN
        tmpTankVolume      =   &
          WaterThermalTank(WaterThermalTankNum)%Sizing%TankDrawTime  &  ! hours
                                                * DrawDesignVolFlowRate & ! m3/s
                                                * SecInHour  !  (3600 s/1 hour)
        WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                             WaterThermalTank(WaterThermalTankNum)%Name, &
                             'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
      ENDIF
      If (SizeMaxCapacity) THEN
        IF (WaterThermalTank(WaterThermalTankNum)%Sizing%RecoveryTime > 0.0d0) THEN
          rho = GetDensityGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeStandAloneWaterHeater')
          Cp  = GetSpecificHeatGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeStandAloneWaterHeater')

          tmpMaxCapacity =  ( WaterThermalTank(WaterThermalTankNum)%Volume & ! m3
                                                * rho & ! kg/m3
                                                * Cp  &  ! J/Kg/K
                                                * (Tfinish - Tstart)) & !  K
                                              / (WaterThermalTank(WaterThermalTankNum)%Sizing%RecoveryTime * SecInHour) ! seconds
        ELSE
          CALL ShowFatalError('SizeStandAloneWaterHeater: Tank="'//TRIM(WaterThermalTank(WaterThermalTankNum)%Name)//  &
            '", requested sizing for max capacity but entered Recovery Time is zero.')
        ENDIF
        WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                                WaterThermalTank(WaterThermalTankNum)%Name, &
                             'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
      ENDIF

    CASE (SizeResidentialMin)
      ! assume can propagate rules for gas to other fuels.
      FuelTypeIsLikeGas = .FALSE.
      IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Gas')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Diesel')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Gasoline')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Coal')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'FuelOil#1')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'FuelOil#2')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Propane')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Steam')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'OtherFuel1')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'OtherFuel2')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ELSEIF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'DistrictHeating')) THEN
        FuelTypeIsLikeGas = .TRUE.
      ENDIF

      IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 1 ) THEN
        IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          IF (SizeVolume)      tmpTankVolume   = 20.0D0 * GalTocubicMeters
          IF (SizeMaxCapacity) tmpMaxCapacity  = 2.5D0 * 1000.0D0  !2.5 kW
        ELSE IF (FuelTypeIsLikeGas) THEN
          IF (SizeVolume)      tmpTankVolume  = 20.0D0 * GalTocubicMeters
          IF (SizeMaxCapacity) tmpMaxCapacity = 27.0D0 * kBtuPerHrToWatts !27kBtu/hr
        ENDIF

      ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 2 ) THEN
        IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms <= 1.5D0) THEN
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
            IF (SizeVolume)      tmpTankVolume  = 30.0D0 * GalTocubicMeters
            IF (SizeMaxCapacity) tmpMaxCapacity = 3.5D0 * 1000.0D0  !3.5 kW
          ELSE IF (FuelTypeIsLikeGas) THEN
            IF (SizeVolume)      tmpTankVolume  = 30.0D0 * GalTocubicMeters
            IF (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
          ENDIF
        ELSE IF ((WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms > 1.5D0) &
                 .and. (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms < 3.0D0)) THEN
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
            If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 4.5D0 * 1000.0D0  !4.5 kW
          ELSE IF (FuelTypeIsLikeGas ) THEN
            If (SizeVolume)      tmpTankVolume  = 30.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
          ENDIF
        ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms >= 3.0D0) then
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
            If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
          ELSE IF (FuelTypeIsLikeGas ) THEN
            If (SizeVolume)      tmpTankVolume = 40.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
          ENDIF
        ENDIF
      ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 3 ) THEN
        IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms <= 1.5D0) THEN
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
            If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 4.5D0 * 1000.0D0  !4.5 kW
          ELSE IF (FuelTypeIsLikeGas ) THEN
            If (SizeVolume)      tmpTankVolume  = 30.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
          ENDIF
        ELSE IF ((WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms > 1.5D0) &
                 .and. (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms < 3.0D0)) THEN
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
            IF (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
            IF (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
          ELSE IF (FuelTypeIsLikeGas ) THEN
            If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
          ENDIF
        ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms >= 3.0D0) THEN
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
            If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
          ELSE IF (FuelTypeIsLikeGas ) THEN
            If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 38.0D0 * kBtuPerHrToWatts !38 kBtu/hr
          ENDIF
        ENDIF
      ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 4 ) THEN
        IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms <= 1.5D0) THEN
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
            If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
          ELSE IF (FuelTypeIsLikeGas) THEN
            If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 36.0D0 * kBtuPerHrToWatts !36 kBtu/hr
          ENDIF
        ELSE IF ((WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms > 1.5D0) &
                 .and. (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms < 3.0D0)) THEN
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
            If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
          ELSE IF (FuelTypeIsLikeGas ) THEN
            If (SizeVolume)      tmpTankVolume  = 40.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 38.0D0 * kBtuPerHrToWatts !38 kBtu/hr
          ENDIF
        ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBathrooms >= 3.0D0) THEN
          IF (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
            If (SizeVolume)      tmpTankVolume  = 66.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
          ELSE IF (FuelTypeIsLikeGas ) THEN
            If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
            If (SizeMaxCapacity) tmpMaxCapacity = 38.0D0 * kBtuPerHrToWatts !38 kBtu/hr
          ENDIF
        ENDIF
      ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms == 5 ) THEN
        If (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 66.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
        ELSE IF (FuelTypeIsLikeGas ) THEN
          If (SizeVolume)      tmpTankVolume = 50.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 47.0D0 * kBtuPerHrToWatts !47 kBtu/hr
        endif
      ELSE IF (WaterThermalTank(WaterThermalTankNum)%Sizing%NumberOfBedrooms >= 6 ) THEN
        If (SameString(WaterThermalTank(WaterThermalTankNum)%FuelType , 'Electric') ) THEN
          If (SizeVolume)      tmpTankVolume  = 66.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 5.5D0 * 1000.0D0  !5.5 kW
        ELSE IF (FuelTypeIsLikeGas ) THEN
          If (SizeVolume)      tmpTankVolume  = 50.0D0 * GalTocubicMeters
          If (SizeMaxCapacity) tmpMaxCapacity = 50.0D0 * kBtuPerHrToWatts !50 kBtu/hr
        ENDIF
      ENDIF
      IF (SizeVolume ) THEN
         WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
         CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                                 WaterThermalTank(WaterThermalTankNum)%Name, &
                                'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
      ENDIF
      IF (SizeMaxCapacity ) THEN
         WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
         CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
         WaterThermalTank(WaterThermalTankNum)%Name, &
                             'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
      ENDIF

    CASE (SizePerPerson)
      ! how to get number of people?

      SumPeopleAllZones = SUM(Zone%TotOccupants)
      IF (SizeVolume) THEN
        tmpTankVolume =   &
           WaterThermalTank(WaterThermalTankNum)%sizing%TankCapacityPerPerson &
                                                           * SumPeopleAllZones
      ENDIF
      IF (SizeMaxCapacity) THEN
        rho = GetDensityGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeStandAloneWaterHeater')
        Cp  = GetSpecificHeatGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeStandAloneWaterHeater')
        tmpMaxCapacity  = SumPeopleAllZones   &
                            * WaterThermalTank(WaterThermalTankNum)%sizing%RecoveryCapacityPerPerson & !m3/hr/person
                                 * (Tfinish - Tstart) & ! delta T  in K
                                 * (1.0D0 / SecInHour)  & !  1 hr/ 3600 s
                                 * rho &  ! kg/m3
                                 * Cp  ! J/Kg/k
      ENDIF

      IF (SizeVolume ) THEN
        WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                                WaterThermalTank(WaterThermalTankNum)%Name, &
                                'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
      ENDIF
      IF (SizeMaxCapacity ) THEN
        WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                                WaterThermalTank(WaterThermalTankNum)%Name, &
                              'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
      ENDIF

    CASE (SizePerFloorArea)

      SumFloorAreaAllZones = SUM(Zone%FloorArea)
      IF (SizeVolume) THEN
        tmpTankVolume =   &
             WaterThermalTank(WaterThermalTankNum)%sizing%TankCapacityPerArea &
                                                           * SumFloorAreaAllZones
      ENDIF

      IF (SizeMaxCapacity) THEN
        rho = GetDensityGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeStandAloneWaterHeater')
        Cp  = GetSpecificHeatGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeStandAloneWaterHeater')
        tmpMaxCapacity     = SumFloorAreaAllZones   & ! m2
                                              * WaterThermalTank(WaterThermalTankNum)%sizing%RecoveryCapacityPerArea & !m3/hr/m2
                                              * (Tfinish - Tstart) & ! delta T  in K
                                              * (1.0D0 / SecInHour)  & !  1 hr/ 3600 s
                                              * rho &  ! kg/m3
                                              * Cp  ! J/Kg/k
      ENDIF
      IF (SizeVolume) THEN
        WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                                WaterThermalTank(WaterThermalTankNum)%Name, &
                               'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
      ENDIF
      IF (SizeMaxCapacity) THEN
        WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,  &
                                                 WaterThermalTank(WaterThermalTankNum)%Name, &
                               'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
      ENDIF
    CASE (SizePerUnit)

      If (SizeVolume) tmpTankVolume  =   &
           WaterThermalTank(WaterThermalTankNum)%sizing%TankCapacityPerUnit &
                                         * WaterThermalTank(WaterThermalTankNum)%sizing%NumberOfUnits

      If (SizeMaxCapacity) THEN
        rho = GetDensityGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeStandAloneWaterHeater')
        Cp  = GetSpecificHeatGlycol('WATER', ((Tfinish + Tstart)/2.0D0), DummyWaterIndex, 'SizeStandAloneWaterHeater')
        tmpMaxCapacity =   &
           WaterThermalTank(WaterThermalTankNum)%sizing%NumberOfUnits   &
                                            * WaterThermalTank(WaterThermalTankNum)%sizing%RecoveryCapacityPerUnit & !m3/hr/ea
                                            * (Tfinish - Tstart) & ! delta T  in K
                                            * (1.0D0 / SecInHour)  & !  1 hr/ 3600 s
                                            * rho &  ! kg/m3
                                            * Cp ! J/Kg/k
      ENDIF

      IF ( SizeVolume ) THEN
        WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,  &
                                WaterThermalTank(WaterThermalTankNum)%Name, &
                             'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
      ENDIF
      IF ( SizeMaxCapacity ) THEN
        WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,  &
                                WaterThermalTank(WaterThermalTankNum)%Name, &
                             'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
      ENDIF
    CASE (SizePerSolarColArea)
      WaterThermalTank(WaterThermalTankNum)%sizing%TotalSolarCollectorArea = 0.0D0
      DO CollectorNum = 1, NumOfCollectors
        WaterThermalTank(WaterThermalTankNum)%sizing%TotalSolarCollectorArea =   &
           WaterThermalTank(WaterThermalTankNum)%sizing%TotalSolarCollectorArea &
                                           + Surface( Collector(CollectorNum)%Surface )%Area !
      ENDDO

      IF (SizeVolume) tmpTankVolume =   &
         WaterThermalTank(WaterThermalTankNum)%sizing%TotalSolarCollectorArea &
                                           * WaterThermalTank(WaterThermalTankNum)%sizing%TankCapacityPerCollectorArea
      IF (SizeMaxCapacity) tmpMaxCapacity = 0.0D0
      IF (SizeVolume ) THEN
        WaterThermalTank(WaterThermalTankNum)%Volume = tmpTankVolume
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                                WaterThermalTank(WaterThermalTankNum)%Name, &
                             'Tank Volume [m3]', WaterThermalTank(WaterThermalTankNum)%Volume )
      ENDIF
      IF (SizeMaxCapacity ) THEN
        WaterThermalTank(WaterThermalTankNum)%MaxCapacity = tmpMaxCapacity
        CALL ReportSizingOutput(WaterThermalTank(WaterThermalTankNum)%Type,   &
                                WaterThermalTank(WaterThermalTankNum)%Name, &
                             'Maximum Heater Capacity [W]', WaterThermalTank(WaterThermalTankNum)%MaxCapacity )
      ENDIF

    END SELECT


  ENDIF

  RETURN

END SUBROUTINE SizeStandAloneWaterHeater

SUBROUTINE UpdateWaterThermalTank(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brandon Anderson
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !                      Nov 2011, BAN; removed the use and source heat rate re-calculation for stratified tank
          !                                     for energy conservation verification.
          !       RE-ENGINEERED  Feb 2004, PGE

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates the node variables with local variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node
  USE Psychrometrics, ONLY: CPHW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: UseInletNode
  INTEGER :: UseOutletNode
  INTEGER :: SourceInletNode
  INTEGER :: SourceOutletNode

          ! FLOW:
  UseInletNode  = WaterThermalTank(WaterThermalTankNum)%UseInletNode
  UseOutletNode = WaterThermalTank(WaterThermalTankNum)%UseOutletNode
  SourceInletNode  = WaterThermalTank(WaterThermalTankNum)%SourceInletNode
  SourceOutletNode = WaterThermalTank(WaterThermalTankNum)%SourceOutletNode

  IF (UseInletNode > 0 .AND. UseOutletNode > 0) THEN
    Node(UseOutletNode) = Node(UseInletNode) ! this could wipe out setpoints on outlet node

    Node(UseOutletNode)%Temp = WaterThermalTank(WaterThermalTankNum)%UseOutletTemp

  END IF

  IF (SourceInletNode > 0 .AND. SourceOutletNode > 0) THEN
    Node(SourceOutletNode) = Node(SourceInletNode)

    Node(SourceOutletNode)%Temp = WaterThermalTank(WaterThermalTankNum)%SourceOutletTemp

  END IF

  RETURN

END SUBROUTINE UpdateWaterThermalTank


SUBROUTINE ReportWaterThermalTank(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brandon Anderson
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  Feb 2004, PGE

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates report variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour
  USE DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: SecInTimeStep

          ! FLOW:
  SecInTimeStep = TimeStepSys * SecInHour

  WaterThermalTank(WaterThermalTankNum)%UnmetEnergy = WaterThermalTank(WaterThermalTankNum)%UnmetRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%LossEnergy = WaterThermalTank(WaterThermalTankNum)%LossRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%FlueLossEnergy = WaterThermalTank(WaterThermalTankNum)%FlueLossRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%UseEnergy = WaterThermalTank(WaterThermalTankNum)%UseRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%TotalDemandEnergy = WaterThermalTank(WaterThermalTankNum)%TotalDemandRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%SourceEnergy = WaterThermalTank(WaterThermalTankNum)%SourceRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%HeaterEnergy = WaterThermalTank(WaterThermalTankNum)%HeaterRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%HeaterEnergy1 = WaterThermalTank(WaterThermalTankNum)%HeaterRate1 * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%HeaterEnergy2 = WaterThermalTank(WaterThermalTankNum)%HeaterRate2 * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%FuelEnergy = WaterThermalTank(WaterThermalTankNum)%FuelRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%VentEnergy = WaterThermalTank(WaterThermalTankNum)%VentRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelEnergy =   &
     WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%OffCycParaEnergyToTank =   &
     WaterThermalTank(WaterThermalTankNum)%OffCycParaRateToTank * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelEnergy =   &
     WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%OnCycParaEnergyToTank =   &
     WaterThermalTank(WaterThermalTankNum)%OnCycParaRateToTank * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%NetHeatTransferEnergy =   &
     WaterThermalTank(WaterThermalTankNum)%NetHeatTransferRate * SecInTimeStep
  WaterThermalTank(WaterThermalTankNum)%VolumeConsumed = WaterThermalTank(WaterThermalTankNum)%VolFlowRate * SecInTimeStep

  RETURN

END SUBROUTINE ReportWaterThermalTank


SUBROUTINE CalcStandardRatings(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2005
          !       MODIFIED       R. Raustad, July 2005 - added HPWH to ratings procedure
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the water heater standard ratings, such as Energy Factor and Recovery Efficiency.  Results are written
          ! to the EIO file.  Standard ratings are not calculated for storage-only tanks, i.e., MaxCapacity = 0.

          ! METHODOLOGY EMPLOYED:
          ! Water heater inputs are set to the specified test conditions. For HPWHs, the heating capacity and COP are assumed
          ! to be the primary element in the water heater and are used during the rating procedure.  CalcWaterThermalTankMixed
          ! is iteratively called in a self-contained, 24 hour simulation of the standard test procedure.

          ! REFERENCES:
          ! Title 10, Code of Federal Regulations, Part 430- Energy Conservation Program for Consumer Products, Appendix E to
          ! Subpart B- Uniform Test Procedure for Measuring the Energy Consumption of Water Heaters, January 1, 2004.

          ! USE STATEMENTS:
  USE Psychrometrics,  ONLY: RhoH2O, CPHW, PsyRhoAirFnPbTdbW, PsyTwbFnTdbWPb , PsyHFnTdbW
  USE DataGlobals,     ONLY: InitConvTemp
  USE DataInterfaces,  ONLY: ShowWarningError, ShowContinueError
  USE CurveManager,    ONLY: CurveValue
  USE DXCoils,         ONLY: HPWHHeatingCapacity, HPWHHeatingCOP, SimDXCoil
  USE Fans,            ONLY: SimulateFanComponents
  USE DataLoopNode,    ONLY: Node
  USE DataEnvironment, ONLY: OutBaroPress
  USE DataHVACGlobals, ONLY: TimeStepSys, HPWHInletDBTemp, HPWHInletWBTemp, HPWHCrankcaseDBTemp, DXCoilTotalCapacity, &
                             BlowThru, CycFanCycCoil
  USE OutputReportPredefined
  USE General,         ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: TotalDrawMass            ! Total mass of hot water drawn during the test (kg), equivalent to 64.3 gallons
  REAL(r64)    :: DrawMass                 ! Mass of a single draw of hot water (kg)
  REAL(r64)    :: SecInTimeStep            ! Seconds per timestep, depends on user-specified system timestep (s)
  REAL(r64)    :: DrawMassFlowRate         ! Mass flow rate of all test draw (m3/s)
  INTEGER :: TimeStepPerHour          ! Number of timesteps per hour
  INTEGER :: Step                     ! Current timestep in the self-contained water heater simulation
  REAL(r64)    :: FuelEnergy               ! Cumulative fuel energy used to heat the tank (J)
  REAL(r64)    :: MaxCapacity              ! Maximum heating capacity (W)
  REAL(r64)    :: RecoveryEfficiency       ! Standard water heater rating
  REAL(r64)    :: EnergyFactor             ! Standard water heater rating
  INTEGER :: HPNum                    ! index to heat pump water heater
  REAL(r64)    :: MdotAir                  ! air mass flow rate through HP water heater evaporator (kg/s)
  REAL(r64)    :: MdotWater                ! water mass flow rate through HP water heater condenser (kg/s)
  REAL(r64)    :: AmbientHumRat            ! used during HPWH rating procedure
  REAL(r64)    :: RatedDXCoilTotalCapacity ! used during HPWH rating procedure
  LOGICAL :: FirstTimeFlag            ! used during HPWH rating procedure
  CHARACTER(len=MaxNameLength) :: equipName
  Logical, SAVE :: MyOneTimeSetupFlag = .true. ! one time setup flag

  If (AlreadyRated(WaterThermalTankNum)) Then ! bail we already did this one
    RETURN
  ENDIF

          ! FLOW:
  IF (WaterThermalTank(WaterThermalTankNum)%MaxCapacity > 0.0d0 .OR. WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .GT. 0) THEN
    ! Set test conditions
    WaterThermalTank(WaterThermalTankNum)%AmbientTemp   = 19.7222d0  ! 67.5 F
    WaterThermalTank(WaterThermalTankNum)%UseInletTemp  = 14.4444d0  ! 58 F
    WaterThermalTank(WaterThermalTankNum)%SetpointTemp  = 57.2222d0  ! 135 F
    WaterThermalTank(WaterThermalTankNum)%SetPointTemp2 = 57.2222d0  ! 135 F
    WaterThermalTank(WaterThermalTankNum)%TankTemp      = 57.2222d0  ! Initialize tank temperature
    IF (WaterThermalTank(WaterThermalTankNum)%Nodes > 0) WaterThermalTank(WaterThermalTankNum)%Node%Temp = 57.2222d0

    TotalDrawMass = 0.243402d0 * RhoH2O(InitConvTemp)  ! 64.3 gal * rho
    DrawMass = TotalDrawMass / 6.0d0  ! 6 equal draws
    SecInTimeStep = TimeStepSys * SecInHour
    DrawMassFlowRate = DrawMass / SecInTimeStep
    FuelEnergy = 0.0d0
    FirstTimeFlag = .TRUE.

    TimeStepPerHour = INT(1.0d0 / TimeStepSys)

    ! Simulate 24 hour test
    DO Step = 1, TimeStepPerHour * 24

      IF (Step == 1 .OR. &                          ! Hour 1
          Step == (1 + TimeStepPerHour) .OR. &      ! Hour 2
          Step == (1 + TimeStepPerHour * 2) .OR. &  ! Hour 3
          Step == (1 + TimeStepPerHour * 3) .OR. &  ! Hour 4
          Step == (1 + TimeStepPerHour * 4) .OR. &  ! Hour 5
          Step == (1 + TimeStepPerHour * 5)) THEN   ! Hour 6

        WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate = DrawMassFlowRate
      ELSE
        WaterThermalTank(WaterThermalTankNum)%UseMassFlowRate = 0.0d0
      END IF

      WaterThermalTank(WaterThermalTankNum)%SavedTankTemp = WaterThermalTank(WaterThermalTankNum)%TankTemp
      WaterThermalTank(WaterThermalTankNum)%SavedMode = WaterThermalTank(WaterThermalTankNum)%Mode
      IF (WaterThermalTank(WaterThermalTankNum)%Nodes > 0) THEN
        WaterThermalTank(WaterThermalTankNum)%Node%SavedTemp = WaterThermalTank(WaterThermalTankNum)%Node%Temp
        WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn1 = WaterThermalTank(WaterThermalTankNum)%HeaterOn1
        WaterThermalTank(WaterThermalTankNum)%SavedHeaterOn2 = WaterThermalTank(WaterThermalTankNum)%HeaterOn2
      END IF

      IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .EQ. 0) THEN

        SELECT CASE (WaterThermalTank(WaterThermalTankNum)%TypeNum)

          CASE (MixedWaterHeater)
            CALL CalcWaterThermalTankMixed(WaterThermalTankNum)

          CASE (StratifiedWaterHeater)
            CALL CalcWaterThermalTankStratified(WaterThermalTankNum)

          CASE DEFAULT
!         Unhandled water heater type

        END SELECT

      ELSE

        HPNum = WaterThermalTank(WaterThermalTankNum)%HeatPumpNum
        AmbientHumRat = 0.00717d0     ! Humidity ratio at 67.5 F / 50% RH

!       set the heat pump air- and water-side mass flow rate
        MdotWater = HPWaterHeater(HPNum)%OperatingWaterFlowRate * RhoH2O(WaterThermalTank(WaterThermalTankNum)%TankTemp)
        MdotAir   = HPWaterHeater(HPNum)%OperatingAirFlowRate * &
                    PsyRhoAirFnPbTdbW(OutBaroPress,WaterThermalTank(WaterThermalTankNum)%AmbientTemp, AmbientHumRat)

!       set the condenser inlet node mass flow rate and temperature
        Node(HPWaterHeater(HPNum)%CondWaterInletNode)%MassFlowRate = MdotWater
        Node(HPWaterHeater(HPNum)%CondWaterInletNode)%Temp         = WaterThermalTank(WaterThermalTankNum)%TankTemp

!       initialize temperatures for HPWH DX Coil heating capacity and COP curves
        HPWHInletDBTemp = WaterThermalTank(WaterThermalTankNum)%AmbientTemp
        HPWHInletWBTemp = PsyTwbFnTdbWPb(HPWHInletDBTemp,AmbientHumRat,OutBaroPress)

!       set up full air flow on DX coil inlet node
        IF(HPWaterHeater(HPNum)%InletAirMixerNode .GT. 0) THEN
          Node(HPWaterHeater(HPNum)%InletAirMixerNode)%MassFlowRate         = MdotAir
          Node(HPWaterHeater(HPNum)%InletAirMixerNode)%MassFlowRateMaxAvail = MdotAir
          Node(HPWaterHeater(HPNum)%InletAirMixerNode)%Temp                 = WaterThermalTank(WaterThermalTankNum)%AmbientTemp
          Node(HPWaterHeater(HPNum)%InletAirMixerNode)%HumRat               = AmbientHumRat
          Node(HPWaterHeater(HPNum)%InletAirMixerNode)%Enthalpy             = &
               PsyHFnTdbW(WaterThermalTank(WaterThermalTankNum)%AmbientTemp,AmbientHumRat)
        ELSE
          IF(HPWaterHeater(HPNum)%OutsideAirNode .EQ. 0)THEN
            Node(HPWaterHeater(HPNum)%HeatPumpAirInletNode)%MassFlowRate            = MdotAir
            Node(HPWaterHeater(HPNum)%HeatPumpAirInletNode)%MassFlowRateMaxAvail    = MdotAir
            Node(HPWaterHeater(HPNum)%HeatPumpAirInletNode)%Temp                    =   &
               WaterThermalTank(WaterThermalTankNum)%AmbientTemp
            Node(HPWaterHeater(HPNum)%HeatPumpAirInletNode)%HumRat                  = AmbientHumRat
            Node(HPWaterHeater(HPNum)%HeatPumpAirInletNode)%Enthalpy                = &
                 PsyHFnTdbW(WaterThermalTank(WaterThermalTankNum)%AmbientTemp,AmbientHumRat)
          ELSE
            Node(HPWaterHeater(HPNum)%OutsideAirNode)%MassFlowRate            = MdotAir
            Node(HPWaterHeater(HPNum)%OutsideAirNode)%MassFlowRateMaxAvail    = MdotAir
            Node(HPWaterHeater(HPNum)%OutsideAirNode)%Temp                    = WaterThermalTank(WaterThermalTankNum)%AmbientTemp
            Node(HPWaterHeater(HPNum)%OutsideAirNode)%HumRat                  = AmbientHumRat
            Node(HPWaterHeater(HPNum)%OutsideAirNode)%Enthalpy                = &
                 PsyHFnTdbW(WaterThermalTank(WaterThermalTankNum)%AmbientTemp,AmbientHumRat)
          END IF
        END IF

        HPWHCrankcaseDBTemp = WaterThermalTank(WaterThermalTankNum)%AmbientTemp

!       simulate the HPWH coil/fan to find heating capacity
        IF(HPWaterHeater(HPNum)%FanPlacement .EQ. BlowThru) THEN
          CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, .TRUE. ,HPWaterHeater(HPNum)%FanNum)
!         CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName, CompOp,  .TRUE.,PartLoadRatio, HPWaterHeater(HPNum)%DXCoilNum,FanOpMode)
          CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName,  1,   .TRUE.,     1.0d0, HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
          CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, .TRUE. ,HPWaterHeater(HPNum)%FanNum)
          CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName,  1,   .TRUE.,     1.0d0, HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
        ELSE
          CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName,  1,   .TRUE.,     1.0d0, HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
          CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, .TRUE. ,HPWaterHeater(HPNum)%FanNum)
          CALL SimDXCoil(HPWaterHeater(HPNum)%DXCoilName,  1,   .TRUE.,     1.0d0, HPWaterHeater(HPNum)%DXCoilNum,CycFanCycCoil)
          CALL SimulateFanComponents(HPWaterHeater(HPNum)%FanName, .TRUE. ,HPWaterHeater(HPNum)%FanNum)
        END IF

        IF(FirstTimeFlag)THEN
          RatedDXCoilTotalCapacity = DXCoilTotalCapacity
          FirstTimeFlag = .FALSE.
        END IF

!       Switch the HPWH info with the tank info and call CalcWaterThermalTankMixed to get Standard Rating
!       (backup element is assumed to be disabled during the rating procedure)
        WaterThermalTank(WaterThermalTankNum)%SourceMassFlowRate   = 0.0d0
        WaterThermalTank(WaterThermalTankNum)%MaxCapacity          = HPWHHeatingCapacity
        WaterThermalTank(WaterThermalTankNum)%MinCapacity          = HPWHHeatingCapacity
        WaterThermalTank(WaterThermalTankNum)%Efficiency           = HPWHHeatingCOP !* WaterHeater(WaterHeaterNum)%Efficiency
        WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad        = HPWaterHeater(HPNum)%OnCycParaLoad
        WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad       = HPWaterHeater(HPNum)%OffCycParaLoad
        WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank = 0.0d0
        WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank  = 0.0d0
        WaterThermalTank(WaterThermalTankNum)%PLFCurve             = HPWaterHeater(HPNum)%DXCoilPLFFPLR

        SELECT CASE (WaterThermalTank(WaterThermalTankNum)%TypeNum)

          CASE (MixedWaterHeater)
            IF (WaterThermalTank(WaterThermalTankNum)%Efficiency .GT. 0.0d0)   &
               CALL CalcWaterThermalTankMixed(WaterThermalTankNum)

          CASE (StratifiedWaterHeater)
            IF (WaterThermalTank(WaterThermalTankNum)%Efficiency .GT. 0.0d0)   &
               CALL CalcWaterThermalTankStratified(WaterThermalTankNum)

          CASE DEFAULT
!         Unhandled water heater type

        END SELECT

!       reset the water heater data to original values
        WaterThermalTank(WaterThermalTankNum)%MaxCapacity          = HPWaterHeater(HPNum)%BackupElementCapacity
        WaterThermalTank(WaterThermalTankNum)%MinCapacity          = HPWaterHeater(HPNum)%BackupElementCapacity
        WaterThermalTank(WaterThermalTankNum)%Efficiency           = HPWaterHeater(HPNum)%BackupElementEfficiency
        WaterThermalTank(WaterThermalTankNum)%OnCycParaLoad        = HPWaterHeater(HPNum)%WHOnCycParaLoad
        WaterThermalTank(WaterThermalTankNum)%OffCycParaLoad       = HPWaterHeater(HPNum)%WHOffCycParaLoad
        WaterThermalTank(WaterThermalTankNum)%OnCycParaFracToTank  = HPWaterHeater(HPNum)%WHOnCycParaFracToTank
        WaterThermalTank(WaterThermalTankNum)%OffCycParaFracToTank = HPWaterHeater(HPNum)%WHOffCycParaFracToTank
        WaterThermalTank(WaterThermalTankNum)%PLFCurve             = HPWaterHeater(HPNum)%WHPLFCurve

      END IF

      FuelEnergy = FuelEnergy + (WaterThermalTank(WaterThermalTankNum)%FuelRate +   &
         WaterThermalTank(WaterThermalTankNum)%OffCycParaFuelRate + &
                   WaterThermalTank(WaterThermalTankNum)%OnCycParaFuelRate) * SecInTimeStep

    END DO  ! Step

    IF (WaterThermalTank(WaterThermalTankNum)%FirstRecoveryDone .AND.   &
       WaterThermalTank(WaterThermalTankNum)%FirstRecoveryFuel > 0.0d0) THEN
        ! Calculate Recovery Efficiency based on energy used to recover from the first draw
        ! FirstRecoveryFuel is recorded inside the CalcWaterThermalTank subroutine
        RecoveryEfficiency = DrawMass * CPHW(57.2222d0) * (57.2222d0 - 14.4444d0) /   &
           WaterThermalTank(WaterThermalTankNum)%FirstRecoveryFuel

        ! Calculate Energy Factor based on total energy (including parasitics) used over entire test
        EnergyFactor = TotalDrawMass * CPHW(57.2222d0) * (57.2222d0 - 14.4444d0) / FuelEnergy

    ELSE
        RecoveryEfficiency = 0.0d0
        EnergyFactor = 0.0d0

        CALL ShowWarningError('Water heater = '//TRIM(WaterThermalTank(WaterThermalTankNum)%Name)// &
          ':  Recovery Efficiency and Energy Factor could not be calculated during the test for standard ratings')
        CALL ShowContinueError('Setpoint was never recovered and/or heater never turned on')
    END IF

  ELSE

    ! Storage-only tank
    RecoveryEfficiency = 0.0d0
    EnergyFactor = 0.0d0

  END IF  ! WaterThermalTank(WaterThermalTankNum)%MaxCapacity > 0.0

  !create predefined report
  ! Store values for the input verification and summary report
  IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .EQ. 0)THEN
    equipName = WaterThermalTank(WaterThermalTankNum)%Name
    CALL PreDefTableEntry(pdchSWHType,equipName,WaterThermalTank(WaterThermalTankNum)%Type)
    CALL PreDefTableEntry(pdchSWHVol,equipName,WaterThermalTank(WaterThermalTankNum)%Volume)
    CALL PreDefTableEntry(pdchSWHHeatIn,equipName,WaterThermalTank(WaterThermalTankNum)%MaxCapacity)
    CALL PreDefTableEntry(pdchSWHThEff,equipName,WaterThermalTank(WaterThermalTankNum)%Efficiency)
    CALL PreDefTableEntry(pdchSWHRecEff,equipName,RecoveryEfficiency)
    CALL PreDefTableEntry(pdchSWHEnFac,equipName,EnergyFactor)
  ELSE
    equipName = HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Name
    CALL PreDefTableEntry(pdchSWHType,equipName,HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Type)
    CALL PreDefTableEntry(pdchSWHVol,equipName,WaterThermalTank(WaterThermalTankNum)%Volume)
    CALL PreDefTableEntry(pdchSWHHeatIn,equipName,HPWHHeatingCapacity)
    CALL PreDefTableEntry(pdchSWHThEff,equipName,WaterThermalTank(WaterThermalTankNum)%Efficiency)
    CALL PreDefTableEntry(pdchSWHRecEff,equipName,RecoveryEfficiency)
    CALL PreDefTableEntry(pdchSWHEnFac,equipName,EnergyFactor)
  END IF

  ! Write test results
  IF(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum .EQ. 0)THEN

    IF (WaterThermalTank(WaterThermalTankNum)%TypeNum == StratifiedWaterHeater) THEN
      IF (WaterThermalTank(WaterThermalTankNum)%ControlType == PriorityMasterSlave) THEN
        MaxCapacity = MAX(WaterThermalTank(WaterThermalTankNum)%MaxCapacity, WaterThermalTank(WaterThermalTankNum)%MaxCapacity2)
      ELSE  ! PrioritySimultaneous
        MaxCapacity = WaterThermalTank(WaterThermalTankNum)%MaxCapacity + WaterThermalTank(WaterThermalTankNum)%MaxCapacity2
      END IF
    ELSE  ! WaterHeaterMixed
      MaxCapacity = WaterThermalTank(WaterThermalTankNum)%MaxCapacity
    END IF

    WRITE(OutputFileInits,720) TRIM(WaterThermalTank(WaterThermalTankNum)%Type), TRIM(WaterThermalTank(WaterThermalTankNum)%Name), &
      TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Volume,4)),  &
      TRIM(TrimSigDigits(MaxCapacity,1)),                         &
      TRIM(TrimSigDigits(RecoveryEfficiency,3)),                  &
      TRIM(TrimSigDigits(EnergyFactor,4))
  ELSE
    WRITE(OutputFileInits,721) TRIM(HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Type), &
      TRIM(HPWaterHeater(WaterThermalTank(WaterThermalTankNum)%HeatPumpNum)%Name), &
      TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Volume,4)),         &
      TRIM(TrimSigDigits(HPWHHeatingCapacity,1)),                        &
      TRIM(TrimSigDigits(RecoveryEfficiency,3)),                         &
      TRIM(TrimSigDigits(EnergyFactor,4)),                               &
      TRIM(TrimSigDigits(RatedDXCoilTotalCapacity,0))
  END IF

  720 FORMAT('Water Heater Information',6(',',A))
  721 FORMAT('Heat Pump Water Heater Information',7(',',A))

  AlreadyRated(WaterThermalTankNum) = .TRUE.
  RETURN

END SUBROUTINE CalcStandardRatings

SUBROUTINE ReportCWTankInits(WaterThermalTankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! send chilled water tank info to EIO

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WaterThermalTankNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Logical, SAVE :: MyOneTimeSetupFlag = .true. ! one time setup flag
  LOGICAL, SAVE, DIMENSION(:), ALLOCATABLE :: AlreadyReported ! control so we don't repeat again


  If (MyOneTimeSetupFlag) then
    Allocate(AlreadyReported(NumWaterThermalTank))
    AlreadyReported = .FALSE.
    MyOneTimeSetupFlag = .FALSE.
  ENDIF

  If (AlreadyReported(WaterThermalTankNum)) Then ! bail we already did this one
    RETURN
  ENDIF

  WRITE(OutputFileInits,728) TRIM(WaterThermalTank(WaterThermalTankNum)%Type), TRIM(WaterThermalTank(WaterThermalTankNum)%Name), &
      TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%Volume,4)),  &
      TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%UseDesignVolFlowRate,4)),  &
      TRIM(TrimSigDigits(WaterThermalTank(WaterThermalTankNum)%SourceDesignVolFlowRate,4))


 728 FORMAT('Chilled Water Tank Information',5(',',A))

  AlreadyReported(WaterThermalTankNum) = .TRUE.

  RETURN

END SUBROUTINE ReportCWTankInits


FUNCTION FindStratifiedTankSensedTemp(WaterThermalTankNum,ControlLocationType)  RESULT (SensedTemp)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! find tank temperature depending on how sensed

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: WaterThermalTankNum
  INTEGER, INTENT(IN)  :: ControlLocationType
  REAL(r64)            :: SensedTemp

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: StratNodeToUse = 0

  SELECT CASE (ControlLocationType)

  CASE (Heater1HPWHControl)
     StratNodeToUse = WaterThermalTank(WaterThermalTankNum)%HeaterNode1
  CASE (Heater2HPWHControl)
     StratNodeToUse = WaterThermalTank(WaterThermalTankNum)%HeaterNode2
  CASE (SourceInletHPWHControl)
     StratNodeToUse = WaterThermalTank(WaterThermalTankNum)%SourceInletStratNode
  CASE (SourceOutletHPWHControl)
     StratNodeToUse = WaterThermalTank(WaterThermalTankNum)%SourceOutletStratNode
  CASE (UseInletHPWHControl)
     StratNodeToUse = WaterThermalTank(WaterThermalTankNum)%UseInletStratNode
  CASE (UseOutletHPWHControl)
     StratNodeToUse = WaterThermalTank(WaterThermalTankNum)%UseOutletStratNode
  END SELECT

  SensedTemp = WaterThermalTank(WaterThermalTankNum)%Node(StratNodeToUse)%Temp

  RETURN

END FUNCTION FindStratifiedTankSensedTemp

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

END MODULE WaterThermalTanks
