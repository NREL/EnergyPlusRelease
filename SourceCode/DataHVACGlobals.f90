MODULE DataHVACGlobals      ! EnergyPlus Data-Only Module

  ! MODULE INFORMATION:
  !       MODIFIED       Craig Wray 22Aug2010 Added Fan Component Model

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for HVAC variables which are considered
          ! to be "global" in nature in EnergyPlus.


          ! METHODOLOGY EMPLOYED:


          ! REFERENCES:


          ! OTHER NOTES:


          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength


IMPLICIT NONE   ! Enforce explicit typing of all variables


PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.



          ! MODULE PARAMETER DEFINITIONS:

REAL(r64), PARAMETER :: SmallTempDiff = 1.0D-5
REAL(r64), PARAMETER :: SmallMassFlow = .001d0
REAL(r64), PARAMETER :: VerySmallMassFlow = 1.0D-30
REAL(r64), PARAMETER :: SmallLoad     = 1.d0
REAL(r64), PARAMETER :: TempControlTol = 0.1d0 ! temperature control tolerance for packaged equip. [deg C]
REAL(r64), PARAMETER :: SmallAirVolFlow = 0.001d0
REAL(r64), PARAMETER :: SmallWaterVolFlow = 1.d-9
REAL(r64), PARAMETER :: BlankNumeric = -99999.d0 ! indicates numeric input field was blank
REAL(r64), PARAMETER :: RetTempMax = 60.0d0  ! maximum return air temperature [deg C]
REAL(r64), PARAMETER :: RetTempMin = -30.0d0 ! minimum return air temperature [deg C]

! Condenser Type (using same numbering scheme as for chillers)
INTEGER, PARAMETER :: AirCooled        = 1 ! Air-cooled condenser
INTEGER, PARAMETER :: WaterCooled      = 2 ! Water-cooled condenser
INTEGER, PARAMETER :: EvapCooled       = 3 ! Evaporatively-cooled condenser
INTEGER, PARAMETER :: WaterHeater      = 4 ! Condenser heats water (e.g., in water heater tank)

! The following parameters are used for system availability status
INTEGER, PARAMETER :: NoAction = 0
INTEGER, PARAMETER :: ForceOff = 1
INTEGER, PARAMETER :: CycleOn = 2
INTEGER, PARAMETER :: CycleOnZoneFansOnly = 3
! The following parameters describe the setpoint types in TempControlType(ActualZoneNum)
INTEGER, PARAMETER :: SingleHeatingSetPoint = 1
INTEGER, PARAMETER :: SingleCoolingSetPoint = 2
INTEGER, PARAMETER :: SingleHeatCoolSetPoint = 3
INTEGER, PARAMETER :: DualSetPointWithDeadBand = 4
! parameters describing air duct type
INTEGER, PARAMETER :: Main = 1
INTEGER, PARAMETER :: Cooling = 2
INTEGER, PARAMETER :: Heating = 3
INTEGER, PARAMETER :: Other = 4
INTEGER, PARAMETER :: RAB = 5
! parameters describing fan types
INTEGER, PARAMETER :: NumAllFanTypes            = 5 !cpw22Aug2010 (was 4)

! fan types
INTEGER, PARAMETER :: FanType_SimpleConstVolume = 1
INTEGER, PARAMETER :: FanType_SimpleVAV         = 2
INTEGER, PARAMETER :: FanType_SimpleOnOff       = 3
INTEGER, PARAMETER :: FanType_ZoneExhaust       = 4
INTEGER, PARAMETER :: FanType_ComponentModel    = 5 !cpw22Aug2010 (new)
! Fan Minimum Flow Fraction Input Method
INTEGER, PARAMETER :: MinFrac = 1
INTEGER, PARAMETER :: FixedMin = 2
! Fan mode
INTEGER, PARAMETER :: CycFanCycCoil = 1             ! Cycling fan, cycling coil = 1
INTEGER, PARAMETER :: ContFanCycCoil = 2            ! Continuous fan, cycling coil = 2
! Fan placement
INTEGER, PARAMETER :: BlowThru = 1                  ! fan before coil
INTEGER, PARAMETER :: DrawThru = 2                  ! fan after coil
! OA Controller Heat Recovery Bypass Control Types
INTEGER, PARAMETER :: BypassWhenWithinEconomizerLimits   = 0 ! heat recovery controlled by economizer limits
INTEGER, PARAMETER :: BypassWhenOAFlowGreaterThanMinimum = 1 ! heat recovery ON at minimum OA in economizer mode

CHARACTER(len=*), PARAMETER, PUBLIC, DIMENSION(NumAllFanTypes) :: cFanTypes=  &
       (/'Fan:ConstantVolume      ',  &
         'Fan:VariableVolume      ',  &
         'Fan:OnOff               ',  &
         'Fan:ZoneExhaust         ',  & !cpw22Aug2010
         'Fan:ComponentModel      '/)   !cpw22Aug2010 (new)

! parameters describing unitary systems
INTEGER, PARAMETER :: NumUnitarySystemTypes            = 7
! Furnace/Unitary System Types
INTEGER, PARAMETER :: Furnace_HeatOnly = 1
INTEGER, PARAMETER :: Furnace_HeatCool = 2
INTEGER, PARAMETER :: UnitarySys_HeatOnly = 3
INTEGER, PARAMETER :: UnitarySys_HeatCool = 4
INTEGER, PARAMETER :: UnitarySys_HeatPump_AirToAir = 5
INTEGER, PARAMETER :: UnitarySys_HeatPump_WaterToAir = 6
INTEGER, PARAMETER :: UnitarySystem_AnyCoilType = 7
CHARACTER(len=*), PARAMETER, DIMENSION(NumUnitarySystemTypes) :: cFurnaceTypes=  &
       (/'AirLoopHVAC:Unitary:Furnace:HeatOnly  ',  &
         'AirLoopHVAC:Unitary:Furnace:HeatCool  ',  &
         'AirLoopHVAC:UnitaryHeatOnly           ',  &
         'AirLoopHVAC:UnitaryHeatCool           ',  &
         'AirLoopHVAC:UnitaryHeatPump:AirToAir  ',  &
         'AirLoopHVAC:UnitaryHeatPump:WaterToAir',  &
         'AirLoopHVAC:UnitarySystem             '/)

! parameters describing coil types
INTEGER, PARAMETER :: NumAllCoilTypes                = 29

INTEGER, PARAMETER :: CoilDX_CoolingSingleSpeed       = 1
INTEGER, PARAMETER :: CoilDX_HeatingEmpirical         = 2
INTEGER, PARAMETER :: CoilDX_CoolingTwoSpeed          = 3
INTEGER, PARAMETER :: CoilDX_CoolingHXAssisted        = 4
INTEGER, PARAMETER :: CoilDX_CoolingTwoStageWHumControl = 5
INTEGER, PARAMETER :: CoilDX_HeatPumpWaterHeater      = 6
INTEGER, PARAMETER :: CoilDX_MultiSpeedCooling        = 7
INTEGER, PARAMETER :: CoilDX_MultiSpeedHeating        = 8

INTEGER, PARAMETER :: Coil_HeatingGas                 = 9
INTEGER, PARAMETER :: Coil_HeatingGas_MultiStage      = 10
INTEGER, PARAMETER :: Coil_HeatingElectric            = 11
INTEGER, PARAMETER :: Coil_HeatingElectric_MultiStage = 12
INTEGER, PARAMETER :: Coil_HeatingDesuperheater       = 13

INTEGER, PARAMETER :: Coil_CoolingWater               = 14
INTEGER, PARAMETER :: Coil_CoolingWaterDetailed       = 15
INTEGER, PARAMETER :: Coil_HeatingWater               = 16
INTEGER, PARAMETER :: Coil_HeatingSteam               = 17
INTEGER, PARAMETER :: CoilWater_CoolingHXAssisted     = 18

INTEGER, PARAMETER :: Coil_CoolingWaterToAirHP        = 19
INTEGER, PARAMETER :: Coil_HeatingWaterToAirHP        = 20
INTEGER, PARAMETER :: Coil_CoolingWaterToAirHPSimple  = 21
INTEGER, PARAMETER :: Coil_HeatingWaterToAirHPSimple  = 22
INTEGER, PARAMETER :: CoilVRF_Cooling                 = 23
INTEGER, PARAMETER :: CoilVRF_Heating                 = 24

INTEGER, PARAMETER :: CoilDX_PackagedThermalStorageCooling = 25

INTEGER, PARAMETER :: Coil_CoolingWaterToAirHPVSEquationFit  = 26
INTEGER, PARAMETER :: Coil_HeatingWaterToAirHPVSEquationFit  = 27
INTEGER, PARAMETER :: Coil_CoolingAirToAirVariableSpeed  = 28
INTEGER, PARAMETER :: Coil_HeatingAirToAirVariableSpeed  = 29

! Water to air HP coil types
INTEGER, PARAMETER :: WatertoAir_Simple               = 1
INTEGER, PARAMETER :: WatertoAir_ParEst               = 2
INTEGER, PARAMETER :: WatertoAir_VarSpeedEquationFit  = 3
INTEGER, PARAMETER :: WatertoAir_VarSpeedLooUpTable   = 4

! Water to Air HP Water Flow Mode
INTEGER, PARAMETER :: WaterCycling = 1                  ! water flow cycles with compressor
INTEGER, PARAMETER :: WaterConstant = 2                 ! water flow is constant
INTEGER, PARAMETER :: WaterConstantOnDemand = 3         ! water flow is constant whenever the coil is operational - this is the only method used in EP V7.2 and earlier

CHARACTER(len=*), PARAMETER, DIMENSION(NumAllCoilTypes) :: cAllCoilTypes=  &
       (/'Coil:Cooling:DX:SingleSpeed                             ',  &
         'Coil:Heating:DX:SingleSpeed                             ',  &
         'Coil:Cooling:DX:TwoSpeed                                ',  &
         'CoilSystem:Cooling:DX:HeatExchangerAssisted             ',  &
         'Coil:Cooling:DX:TwoStageWithHumidityControlMode         ',  &
         'Coil:WaterHeating:AirToWaterHeatPump                    ',  &
         'Coil:Cooling:DX:MultiSpeed                              ',  &
         'Coil:Heating:DX:MultiSpeed                              ',  &
         'Coil:Heating:Gas                                        ',  &
         'Coil:Heating:Gas:MultiStage                             ',  &
         'Coil:Heating:Electric                                   ',  &
         'Coil:Heating:Electric:MultiStage                        ',  &
         'Coil:Heating:Desuperheater                              ',  &
         'Coil:Cooling:Water                                      ',  &
         'Coil:Cooling:Water:DetailedGeometry                     ',  &
         'Coil:Heating:Water                                      ',  &
         'Coil:Heating:Steam                                      ',  &
         'CoilSystem:Cooling:Water:HeatExchangerAssisted          ',  &
         'Coil:Cooling:WaterToAirHeatPump:ParameterEstimation     ',  &
         'Coil:Heating:WaterToAirHeatPump:ParameterEstimation     ',  &
         'Coil:Cooling:WaterToAirHeatPump:EquationFit             ',  &
         'Coil:Heating:WaterToAirHeatPump:EquationFit             ',  &
         'Coil:Cooling:DX:VariableRefrigerantFlow                 ',  &
         'Coil:Heating:DX:VariableRefrigerantFlow                 ',  &
         'Coil:Cooling:DX:SingleSpeed:ThermalStorage              ',  &
         'Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit',  &
         'Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit',  &
         'Coil:Cooling:DX:VariableSpeed                           ',  &
         'Coil:Heating:DX:VariableSpeed                           '/)


! parameters describing coil performance types
INTEGER, PARAMETER :: CoilPerfDX_CoolByPassEmpirical  = 100





! Parameters describing Heat Exchanger types
INTEGER, PARAMETER :: NumHXTypes            = 3

INTEGER, PARAMETER :: HX_AIRTOAIR_FLATPLATE = 1
INTEGER, PARAMETER :: HX_AIRTOAIR_GENERIC   = 2
INTEGER, PARAMETER :: HX_DESICCANT_BALANCED = 3

CHARACTER(len=*), PARAMETER, DIMENSION(NumHXTypes) :: cHXTypes=  &
         (/'HeatExchanger:AirToAir:FlatPlate           ',  &
           'HeatExchanger:AirToAir:SensibleAndLatent   ',  &
           'HeatExchanger:Desiccant:BalancedFlow       '/)
                      
! Parameters describing air terminal mixers
INTEGER, PARAMETER :: NumATMixerTypes           = 2

INTEGER, PARAMETER :: ATMixer_InletSide         = 1
INTEGER, PARAMETER :: ATMixer_SupplySide        = 2

CHARACTER(len=*), PARAMETER, DIMENSION(NumATMixerTypes) :: cATMixerTypes=  &
         (/'AirTerminal:SingleDuct:InletSideMixer           ',  &
           'AirTerminal:SingleDuct:SupplySideMixer          '/)
LOGICAL, PARAMETER :: ATMixerExists             = .TRUE.

! Parameters describing variable refrigerant flow terminal unit types
INTEGER, PARAMETER :: NumVRFTUTypes         = 1

INTEGER, PARAMETER :: VRFTUType_ConstVolume    = 1

CHARACTER(len=*), PARAMETER, DIMENSION(NumVRFTUTypes) :: cVRFTUTypes=  &
         (/'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow'/)

! VRF Heating Performance Curve Temperature Type
INTEGER, PARAMETER :: NumVRFHeatingPerformanceOATTypes = 2
INTEGER, PARAMETER :: WetBulbIndicator = 1
INTEGER, PARAMETER :: DryBulbIndicator = 2

CHARACTER(len=*), PARAMETER, DIMENSION(NumVRFHeatingPerformanceOATTypes) :: cVRFHeatingPerformanceOATTypes=  &
         (/'WetBulbTemperature',  &
           'DryBulbTemperature'/)

! parameter concerning the amount of change in zone temperature is needed
! for oscillation of zone temperature to be detected.
REAL(r64), PARAMETER    :: OscillateMagnitude = 0.15d0


          ! DERIVED TYPE DEFINITIONS
TYPE ComponentSetPtData
 ! CHARACTER(len=MaxNameLength) :: EquipOperListName
  CHARACTER(len=MaxNameLength) :: EquipmentType        = ' '
  CHARACTER(len=MaxNameLength) :: EquipmentName        = ' '
  INTEGER                      :: NodeNumIn            = 0
  INTEGER                      :: NodeNumOut           = 0
  REAL(r64)                    :: EquipDemand          = 0.0d0
  REAL(r64)                    :: DesignFlowRate       = 0.0d0
  CHARACTER(len=7)             :: HeatOrCool           = ' ' !
  INTEGER                      :: OpType               = 0
END TYPE ComponentSetPtData

TYPE DefineZoneCompAvailMgrs
  INTEGER                                               :: NumAvailManagers =0   ! number of availability managers for this system
  INTEGER                                               :: AvailStatus      =0   ! system availability status
  INTEGER                                               :: StartTime        =0   ! cycle on time (in SimTimeSteps)
  INTEGER                                               :: StopTime         =0   ! cycle off time (in SimTimeSteps)
  CHARACTER(len=MaxNameLength)                          :: AvailManagerListName = ' ' ! name of each availability manager
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: AvailManagerName ! name of each availability manager
  INTEGER,DIMENSION(:),ALLOCATABLE                      :: AvailManagerType ! type of availability manager
  INTEGER, DIMENSION(:), ALLOCATABLE                    :: AvailManagerNum  ! index for availability manager
  INTEGER                                               :: ZoneNum         =0   ! cycle off time (in SimTimeSteps)
  LOGICAL                                               :: Input           = .TRUE.
END TYPE DefineZoneCompAvailMgrs


TYPE ZoneCompTypeData
  TYPE(DefineZoneCompAvailMgrs), &
       ALLOCATABLE, DIMENSION(:)   :: ZoneCompAvailMgrs
  INTEGER                          :: TotalNumComp         =0   ! total number of components of a zone equip type
END TYPE ZoneCompTypeData

!Optimum start indicator -----------------------------------------------------
TYPE OptStartDataType
  INTEGER, DIMENSION(:), ALLOCATABLE   :: ActualZoneNum
  REAL(r64), DIMENSION(:), ALLOCATABLE :: OccStartTime
  LOGICAL, DIMENSION(:), ALLOCATABLE   :: OptStartFlag   
END TYPE OptStartDataType
!--------------------------------------------------------------------------------

TYPE (ZoneCompTypeData), ALLOCATABLE, DIMENSION(:) :: ZoneComp

TYPE (OptStartDataType), SAVE                      :: OptStartData ! For optimum start

          ! INTERFACE BLOCK SPECIFICATIONS


          ! MODULE VARIABLE DECLARATIONS:


LOGICAL  :: FirstTimeStepSysFlag =.false. ! Set to true at the start of each sub-time step

REAL(r64) :: SysUpdateTimeInc     =0.0d0   ! System Update Time Increment - the adaptive time step used by the HVAC simulation
REAL(r64) :: TimeStepSys          =0.0d0   ! System Time Increment - the adaptive time step used by the HVAC simulation (hours)
REAL(r64) :: SysTimeElapsed       =0.0d0   ! elapsed system time in zone timestep (hours)
REAL(r64) :: FracTimeStepZone     =0.0d0   ! System time step divided by the zone time step
LOGICAL   :: ShortenTimeStepSys = .FALSE.! Logical flag that triggers shortening of system time step
INTEGER   :: NumOfSysTimeSteps    = 1    ! for current zone time step, number of system timesteps inside  it
INTEGER   :: NumOfSysTimeStepsLastZoneTimeStep = 1 ! previous zone time step, num of system timesteps inside
INTEGER   :: LimitNumSysSteps = 0

LOGICAL  :: UseZoneTimeStepHistory = .TRUE. ! triggers use of zone time step history, else system time step history, for ZTM1, ZTMx
INTEGER  :: NumPlantLoops    = 0 ! Number of plant loops specified in simulation
INTEGER  :: NumCondLoops     = 0 ! Number of condenser plant loops specified in simulation
INTEGER  :: NumElecCircuits  = 0 ! Number of electric circuits specified in simulation
INTEGER  :: NumGasMeters     = 0 ! Number of gas meters specified in simulation
INTEGER  :: NumPrimaryAirSys = 0 ! Number of primary HVAC air systems
REAL(r64)     :: FanElecPower     = 0.0d0 ! fan power from last fan simulation
REAL(r64)     :: OnOffFanPartLoadFraction = 1.0d0 ! fan part-load fraction (Fan:OnOff)
REAL(r64)     :: DXCoilTotalCapacity  = 0.0d0 ! DX coil total cooling capacity (eio report var for HPWHs)
REAL(r64)     :: DXElecCoolingPower   = 0.0d0 ! Electric power consumed by DX cooling coil last DX simulation
REAL(r64)     :: DXElecHeatingPower   = 0.0d0 ! Electric power consumed by DX heating coil last DX simulation
REAL(r64)     :: ElecHeatingCoilPower = 0.0d0 ! Electric power consumed by electric heating coil
REAL(r64)     :: AirToAirHXElecPower  = 0.0d0 ! Electric power consumed by Heat Exchanger:Air To Air (Generic or Flat Plate)
                                            ! from last simulation in HeatRecovery.f90
REAL(r64)     :: UnbalExhMassFlow = 0.d0     ! unbalanced zone exhaust from a zone equip component [kg/s]
REAL(r64)     :: BalancedExhMassFlow = 0.d0    ! balanced zone exhaust (declared as so by user)  [kg/s]
REAL(r64)     :: PlenumInducedMassFlow = 0.d0 ! secondary air mass flow rate induced from a return plenum [kg/s]
LOGICAL  :: TurnFansOn = .FALSE.       ! If true overrides fan schedule and cycles fans on
LOGICAL  :: TurnFansOff = .FALSE.      ! If True overides fan schedule and TurnFansOn and forces fans off
LOGICAL  :: ZoneCompTurnFansOn = .FALSE.       ! If true overrides fan schedule and cycles fans on
LOGICAL  :: ZoneCompTurnFansOff = .FALSE.      ! If True overides fan schedule and TurnFansOn and forces fans off
LOGICAL  :: SetPointErrorFlag = .FALSE. ! True if any needed setpoints not set; if true, program terminates
LOGICAL  :: DoSetPointTest = .FALSE.    ! True one time only for sensed node setpoint test
LOGICAL  :: NightVentOn = .FALSE.             ! set TRUE in SimAirServingZone if night ventilation is happening


INTEGER  :: NumTempContComps  = 0             !
REAL(r64)     :: HPWHInletDBTemp    =0.0d0    ! Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
REAL(r64)     :: HPWHInletWBTemp    =0.0d0    ! Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
REAL(r64)     :: HPWHCrankcaseDBTemp=0.0d0    ! Used for HEAT PUMP:WATER HEATER crankcase heater ambient temperature calculations
LOGICAL  :: AirLoopInit        =.FALSE.     ! flag for whether InitAirLoops has been called
LOGICAL  :: AirLoopsSimOnce     =.FALSE.    ! True means that the air loops have been simulated once in this environment

! Hybrid ventilation control part
INTEGER  :: NumHybridVentSysAvailMgrs = 0 ! Number of hybrid ventilation control
INTEGER, ALLOCATABLE, DIMENSION(:) :: HybridVentSysAvailAirLoopNum ! Airloop number in hybrid vent availability manager
INTEGER, ALLOCATABLE, DIMENSION(:) :: HybridVentSysAvailVentCtrl ! Ventilation control action in hybrid vent availability manager
INTEGER, ALLOCATABLE, DIMENSION(:) :: HybridVentSysAvailActualZoneNum ! Actual zone num in hybrid vent availability manager
INTEGER, ALLOCATABLE, DIMENSION(:) :: HybridVentSysAvailANCtrlStatus ! AN control status in hybrid vent availability manager
INTEGER, ALLOCATABLE, DIMENSION(:) :: HybridVentSysAvailMaster   ! Master object name: Ventilation for simple; Zone name for AN
REAL(r64), ALLOCATABLE, DIMENSION(:) :: HybridVentSysAvailWindModifier ! Wind modifier for AirflowNetwork
! For multispeed heat pump only
REAL(r64)    :: MSHPMassFlowRateLow  =0.0d0   ! Mass flow rate at low speed
REAL(r64)    :: MSHPMassFlowRateHigh =0.0d0   ! Mass flow rate at high speed
REAL(r64)    :: MSHPWasteHeat        =0.0d0   ! Waste heat
REAL(r64)    :: PreviousTimeStep     =0.0d0   ! The time step length at the previous time step
LOGICAL      :: ShortenTimeStepSysRoomAir = .FALSE.! Logical flag that triggers shortening of system time step

TYPE (ComponentSetPtData), ALLOCATABLE, DIMENSION(:) :: CompSetPtEquip

REAL(r64) :: deviationFromSetPtThresholdHtg = -0.2d0   ! heating threshold for reporting setpoint deviation
REAL(r64) :: deviationFromSetPtThresholdClg = 0.2d0    ! cooling threshold for reporting setpoint deviation

LOGICAL,  PUBLIC :: SimAirLoopsFlag            ! True when the air loops need to be (re)simulated
LOGICAL,  PUBLIC :: SimElecCircuitsFlag        ! True when electic circuits need to be (re)simulated
LOGICAL,  PUBLIC :: SimPlantLoopsFlag          ! True when the main plant loops need to be (re)simulated
LOGICAL,  PUBLIC :: SimZoneEquipmentFlag       ! True when zone equipment components need to be (re)simulated
LOGICAL,  PUBLIC :: SimNonZoneEquipmentFlag       ! True when non-zone equipment components need to be (re)simulated

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


END MODULE DataHVACGlobals
