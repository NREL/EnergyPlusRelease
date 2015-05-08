MODULE CondenserLoopTowers

  ! Module containing the routines dealing with the objects COOLING TOWER:SINGLE SPEED,
  ! COOLING TOWER:TWO SPEED, and COOLING TOWER:VARIABLE SPEED

  ! MODULE INFORMATION:
  !       AUTHOR         Dan Fisher
  !       DATE WRITTEN   April 1998
  !       MODIFIED       Shirey, Raustad: Dec 2000; Shirey, Sept 2002, Raustad Mar 2005
  !                      B Griffith Aug 2006, added water consumption and water system interactions
  !                      T Hong, Aug 2008. Added fluid bypass for single speed cooling tower
  !                      Chandan Sharma, FSEC, February 2010, Added basin heater
  !                      A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Model the performance of cooling towers

  ! METHODOLOGY EMPLOYED:

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, KelvinConv, SecInHour, WarmupFlag, InitConvTemp
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, SetupOutputVariable,  &
                       ShowContinueError, ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
USE DataHVACGlobals
USE DataLoopNode
Use DataEnvironment, ONLY: StdBaroPress, OutDryBulbTemp, OutHumRat, OutBaroPress, OutWetBulbTemp

USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
USE DataPlant,       ONLY: PlantLoop
  ! Use statements for access to subroutines in other modules
USE Psychrometrics, ONLY: PsyWFnTdbTwbPb, PsyRhoAirFnPbTdbW, PsyHFnTdbRhPb, PsyCpAirFnWTdb, &
                          PsyTsatFnHPb, PsyWFnTdbH
USE General, ONLY: TrimSigDigits

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
! Empirical Model Type
INTEGER, PARAMETER             :: CoolToolsXFModel     = 1
! CoolTools counterflow model does not work properly. The empirical model seems flawed since the tower
! operates in the free convection regime on the design day.
! INTEGER, PARAMETER             :: CoolToolsCFModel     = 2
INTEGER, PARAMETER             :: CoolToolsUserDefined = 3
INTEGER, PARAMETER             :: YorkCalcModel        = 4
INTEGER, PARAMETER             :: YorkCalcUserDefined  = 5

INTEGER, PARAMETER             :: EvapLossByUserFactor = 80
INTEGER, PARAMETER             :: EvapLossByMoistTheory = 81

INTEGER, PARAMETER             :: BlowdownByConcentration = 90
INTEGER, PARAMETER             :: BlowdownBySchedule = 91

CHARACTER(len=*), PARAMETER :: cCoolingTower_SingleSpeed  ='CoolingTower:SingleSpeed'
CHARACTER(len=*), PARAMETER :: cCoolingTower_TwoSpeed     ='CoolingTower:TwoSpeed'
CHARACTER(len=*), PARAMETER :: cCoolingTower_VariableSpeed='CoolingTower:VariableSpeed'
CHARACTER(len=*), PARAMETER :: cCoolingTower_VariableSpeedMerkel='CoolingTower:VariableSpeed:Merkel'

INTEGER, PARAMETER :: PIM_NominalCapacity      = 1
INTEGER, PARAMETER :: PIM_UFactor              = 2

INTEGER, PARAMETER :: CoolingTower_SingleSpeed  = 1
INTEGER, PARAMETER :: CoolingTower_TwoSpeed     = 2
INTEGER, PARAMETER :: CoolingTower_VariableSpeed= 3
INTEGER, PARAMETER :: CoolingTower_VariableSpeedMerkel= 4

INTEGER, PARAMETER :: CapacityControl_FanCycling = 1
INTEGER, PARAMETER :: CapacityControl_FluidBypass= 2

INTEGER, PARAMETER :: CellCtrl_MinCell = 1
INTEGER, PARAMETER :: CellCtrl_MaxCell = 2




  ! DERIVED TYPE DEFINITIONS
TYPE Towerspecs
  CHARACTER(len=MaxNameLength) :: Name                   = ' ' ! User identifier
  CHARACTER(len=MaxNameLength) :: TowerType              = ' ' ! Type of cooling tower
  INTEGER                      :: TowerType_Num          = 0
  INTEGER                      :: PerformanceInputMethod_Num = 0 ! Method of entering tower performance: UA and Design Water
                                                               !  Flow Rate, or Nominal Capacity
  CHARACTER(len=MaxNameLength):: ModelCoeffObjectName    = ' ' ! Cooling Tower:Variable Speed Model Coefficient Object name
  LOGICAL    :: Available                    = .TRUE. ! need an array of logicals--load identifiers of available equipment
  LOGICAL    :: ON                           = .TRUE. ! Simulate the machine at it's operating part load ratio
  REAL(r64)  :: DesignWaterFlowRate             = 0.0d0 ! Design water flow rate through the tower [m3/s]
  REAL(r64)  :: DesignWaterFlowPerUnitNomCap    = 0.0d0 ! scalable sizing factor for water flow per capacity [m3/s/W]
  REAL(r64)  :: DesWaterMassFlowRate            = 0.0d0 ! Design water flow rate through the entire tower [kg/s]
  REAL(r64)  :: DesWaterMassFlowRatePerCell     = 0.0d0 ! Design water flow rate per cell [Kg/s]
  REAL(r64)  :: HighSpeedAirFlowRate            = 0.0d0 ! Air flow rate through tower at high speed [m3/s]
  REAL(r64)  :: DesignAirFlowPerUnitNomCap      = 0.0D0 ! scalable sizing factor for air flow per capacity [m3/s/W]
  LOGICAL    :: DefaultedDesignAirFlowScalingFactor = .FALSE. ! true if user left input field blank for DesignAirFlowPerUnitNomCap
  REAL(r64)  :: HighSpeedFanPower               = 0.0d0 ! Fan power at high fan speed [W]
  REAL(r64)  :: DesignFanPowerPerUnitNomCap     = 0.0d0 ! scalable sizing factor for fan power per capacity [W/W]
  LOGICAL    :: UAvaluesCompleted               = .FALSE.
  REAL(r64)  :: HighSpeedTowerUA                = 0.0d0 ! UA of tower at high fan speed [W/C]
  REAL(r64)  :: LowSpeedAirFlowRate             = 0.0d0 ! Air flow rate through tower at low speed [m3/s]
  REAL(r64)  :: LowSpeedAirFlowRateSizingFactor = 0.0d0 ! sizing factor for low speed flow rate [ ]
  REAL(r64)  :: LowSpeedFanPower                = 0.0d0 ! Fan power at low fan speed [W]
  REAL(r64)  :: LowSpeedFanPowerSizingFactor    = 0.0d0 ! sizing factor for low speed fan power []
  REAL(r64)  :: LowSpeedTowerUA                 = 0.0d0 ! UA of tower at low fan speed [W/C]
  REAL(r64)  :: LowSpeedTowerUASizingFactor     = 0.0d0 ! sizing factor for UA at low fan speed []
  REAL(r64)  :: FreeConvAirFlowRate             = 0.0d0 ! Air flow rate through tower with fan off [m3/s]
  REAL(r64)  :: FreeConvAirFlowRateSizingFactor = 0.0d0 ! sizing factor for air flow at free conv []
  REAL(r64)  :: FreeConvTowerUA                 = 0.0d0 ! UA of tower with fan off [W/C]
  REAL(r64)  :: FreeConvTowerUASizingFactor     = 0.0d0 ! sizing factor for UA at fre convection []
  REAL(r64)  :: DesignInletWB                   = 0.0d0 ! Design inlet air wet-bulb temperature (C)
  REAL(r64)  :: DesignApproach                  = 0.0d0 ! Design approach (outlet water temp minus inlet air wet-bulb temp (C)
  REAL(r64)  :: DesignRange                     = 0.0d0 ! Design range temperature (inlet water temp minus outlet water temp (C)
  REAL(r64)  :: MinimumVSAirFlowFrac            = 0.0d0 ! Min air flow ratio (used for VS tower only, point where free conv occurs)
  REAL(r64)  :: CalibratedWaterFlowRate         = 0.0d0 ! Water flow ratio required for model calibration
  REAL(r64)  :: BasinHeaterPowerFTempDiff       = 0.0d0 ! Basin heater capacity per degree C below setpoint (W/C)
  REAL(r64)  :: BasinHeaterSetPointTemp         = 0.0d0 ! setpoint temperature for basin heater operation (C)
  REAL(r64)  :: MakeupWaterDrift                = 0.0d0 ! Makeup water flow rate fraction due to drift
  REAL(r64)  :: FreeConvectionCapacityFraction  = 0.0d0 ! Percentage of tower capacity in free convection regime
  REAL(r64)  :: TowerMassFlowRateMultiplier     = 0.0d0 ! Maximum tower flow rate is this multiplier times design flow rate
  REAL(r64)  :: HeatRejectCapNomCapSizingRatio  = 1.25d0 ! ratio of actual cap to nominal capacity []
  REAL(r64)  :: TowerNominalCapacity            = 0.0d0 ! Nominal capacity of the tower [W] with entering water at 35C (95F),
                                                      !  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
                                                      !  temp and 35C (95F) dry-bulb temp, and water flow
                                                      !  rate of 5.382E-8 m3/s per watt (3 gpm/ton)
  REAL(r64)  :: TowerLowSpeedNomCap             = 0.0d0 ! Nominal capacity of the tower [W] with entering water at 35C (95F),
                                                      !  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
                                                      !  temp and 35C (95F) dry-bulb temp, and water flow
                                                      !  rate of 5.382E-8 m3/s per nominal capacity watt (3 gpm/ton)
  REAL(r64)  :: TowerLowSpeedNomCapSizingFactor = 0.0d0 ! sizing factor for low speed capacity []
  REAL(r64)  :: TowerFreeConvNomCap             = 0.0d0 ! Nominal capacity of the tower [W] with entering water at 35C (95F),
                                                      !  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
                                                      !  temp and 35C (95F) dry-bulb temp, and water flow
                                                      !  rate of 5.382E-8 m3/s per nominal capacity watt (3 gpm/ton)
  REAL(r64)  :: TowerFreeConvNomCapSizingFactor = 0.0d0 ! sizing factor for free conv capacity []
  REAL(r64)  :: SizFac                          = 0.0d0 !  sizing factor
  INTEGER    :: WaterInletNodeNum               = 0   ! Node number on the water inlet side of the tower
  INTEGER    :: WaterOutletNodeNum              = 0   ! Node number on the water outlet side of the tower
  INTEGER    :: OutdoorAirInletNodeNum          = 0   ! Node number of outdoor air inlet for the tower
  INTEGER    :: TowerModelType                  = 0   ! Type of empirical model (1=CoolTools)
  INTEGER    :: VSTower                         = 0   ! Index to a variable speed tower (otherwise = 0)
  INTEGER    :: FanPowerfAirFlowCurve           = 0   ! Index to fan power correlation curve for VS Towers
  INTEGER    :: BlowDownSchedulePtr             = 0   ! Pointer to blow down schedule
  INTEGER    :: BasinHeaterSchedulePtr          = 0   ! Pointer to basin heater schedule
  INTEGER    :: HighMassFlowErrorCount          = 0   ! Counter when mass flow rate is > Design*TowerMassFlowRateMultiplier
  INTEGER    :: HighMassFlowErrorIndex          = 0   ! Index for high mass flow recurring error message
  INTEGER    :: OutletWaterTempErrorCount       = 0   ! Counter when outlet water temperature is < minimum allowed temperature
  INTEGER    :: OutletWaterTempErrorIndex       = 0   ! Index for outlet water temperature recurring error message
  INTEGER    :: SmallWaterMassFlowErrorCount    = 0   ! Counter when water mass flow rate is very small
  INTEGER    :: SmallWaterMassFlowErrorIndex    = 0   ! Index for very small water mass flow rate recurring error message
  INTEGER    :: WMFRLessThanMinAvailErrCount    = 0   ! Counter when water mass flow rate is less than minimum available
  INTEGER    :: WMFRLessThanMinAvailErrIndex    = 0   ! Index for water mass flow rate less than minavail recurring message
  INTEGER    :: WMFRGreaterThanMaxAvailErrCount = 0   ! Counter when water mass flow rate is greater than minimum available
  INTEGER    :: WMFRGreaterThanMaxAvailErrIndex = 0   ! Index for water mass flow rate > minavail recurring message
  INTEGER    :: CoolingTowerAFRRFailedCount     = 0   ! Counter for air flow rate ratio out of bounds error
  INTEGER    :: CoolingTowerAFRRFailedIndex     = 0   ! Index for air flow rate ratio out of bounds error
  INTEGER    :: SpeedSelected                   = 0   ! speed of the two-speed fan selected (0:ON;1:LOW;2:HIGH)
  !fluid bypass
  INTEGER    :: CapacityControl                 = 0   ! Type of capacity control for single speed cooling tower:
                                                      !  0 - FanCycling, 1 - FluidBypass
  REAL(r64)  :: BypassFraction                  = 0.0d0 ! Fraction of fluid bypass as a ratio of total fluid flow
                                                      !  through the tower sump
  !multi cell tower
  INTEGER    :: NumCell                         = 0 ! Number of cells in the cooling tower
  CHARACTER(len=15)  :: CellCtrl                = ' ' ! Cell control type : either MaxCell or MinCell
  INTEGER    :: CellCtrl_Num                    = 0
  INTEGER    :: NumCellON                       = 0   ! number of cells working
  REAL(r64)  :: MinFracFlowRate                 = 0.0d0 ! Minimal fraction of design flow/cell allowable
  REAL(r64)  :: MaxFracFlowRate                 = 0.0d0 ! Maximal ratio of design flow/cell allowable

  !begin water system interactions
  INTEGER    :: EvapLossMode                    = EvapLossByMoistTheory  ! sets how tower water evaporation is modeled
  REAL(r64)  :: UserEvapLossFactor              = 0.0d0 ! simple model [%/Delt C]
  REAL(r64)  :: DriftLossFraction               = 0.0d0
  INTEGER    :: BlowdownMode                    = BlowdownByConcentration ! sets how tower water blowdown is modeled
  REAL(r64)  :: ConcentrationRatio              = 0.0d0 ! ratio of solids in blowdown vs make up water
  INTEGER    :: SchedIDBlowdown                 = 0 ! index "pointer" to schedule of blowdown in [m3/s]
  Logical    :: SuppliedByWaterSystem           = .false.
  INTEGER    :: WaterTankID                     = 0 ! index "pointer" to WaterStorage structure
  INTEGER    :: WaterTankDemandARRID            = 0 ! index "pointer" to demand array inside WaterStorage structure


  !end water system variables

  !loop topology variables
  INTEGER    :: LoopNum     = 0
  INTEGER    :: LoopSideNum = 0
  INTEGER    :: BranchNum   = 0
  INTEGER    :: CompNum     = 0

  !Merkel VS model curves
  INTEGER    :: UAModFuncAirFlowRatioCurvePtr = 0 ! curve index for UA modifier as a function of air flow ratio
  INTEGER    :: UAModFuncWetbulbDiffCurvePtr  = 0 ! curve index for UA modifier as a function of local wetbulb
  INTEGER    :: UAModFuncWaterFlowRatioCurvePtr = 0 ! curve index for UA modifier as a function of water flow ratio
  LOGICAL    :: SetpointIsOnOutlet = .FALSE.  ! if true look to outlet node of tower, if flase look to overall loop setpoint
  INTEGER    :: VSMerkelAFRErrorIter = 0 ! error counter for regula falsi failed with max iterations, vs merkel model
  INTEGER    :: VSMerkelAFRErrorFail = 0 ! error counter for regula falsi failed with limits exceeded, vs merkel model

END TYPE Towerspecs

TYPE TowerInletConds
  REAL(r64) :: WaterTemp      = 0.0d0  ! Tower water inlet temperature (C)
  REAL(r64) :: AirTemp        = 0.0d0  ! Tower air inlet dry-bulb temperature (C)
  REAL(r64) :: AirWetBulb     = 0.0d0  ! Tower air inlet wet-bulb temperature (C)
  REAL(r64) :: AirPress       = 0.0d0  ! Tower air barometric pressure
  REAL(r64) :: AirHumRat      = 0.0d0  ! Tower air inlet humidity ratio (kg/kg)
END TYPE TowerInletConds

TYPE ReportVars
  REAL(r64)    :: InletWaterTemp         = 0.0d0  ! Tower inlet water temperature (C)
  REAL(r64)    :: OutletWaterTemp        = 0.0d0  ! Tower outlet water temperature (C)
  REAL(r64)    :: WaterMassFlowRate      = 0.0d0  ! Tower water mass flow rate (m3/s)
  REAL(r64)    :: Qactual                = 0.0d0  ! Tower heat rejection rate (W)
  REAL(r64)    :: FanPower               = 0.0d0  ! Tower fan power (W)
  REAL(r64)    :: FanEnergy              = 0.0d0  ! Tower fan energy consumption (J)
  REAL(r64)    :: AirFlowRatio           = 0.0d0  ! Air flow ratio through variable speed cooling tower
  REAL(r64)    :: BasinHeaterPower       = 0.0d0  ! Basin heater power (W)
  REAL(r64)    :: BasinHeaterConsumption = 0.0d0  ! Basin heater energy consumption (J)
  REAL(r64)    :: WaterAmountUsed        = 0.0d0  ! Tower make up water usage (m3)
  REAL(r64)    :: FanCyclingRatio        = 0.0d0  ! cycling ratio of tower fan when min fan speed provide too much capacity (for VFD)
  REAL(r64)    :: EvaporationVdot        = 0.0d0  !
  REAL(r64)    :: EvaporationVol         = 0.0d0  !
  REAL(r64)    :: DriftVdot              = 0.0d0  !
  REAL(r64)    :: DriftVol               = 0.0d0  !
  REAL(r64)    :: BlowdownVdot           = 0.0d0  !
  REAL(r64)    :: BlowdownVol            = 0.0d0  !
  REAL(r64)    :: MakeUpVdot             = 0.0d0  !
  REAL(r64)    :: MakeUpVol              = 0.0d0  !
  REAL(r64)    :: TankSupplyVdot         = 0.0d0  !
  REAL(r64)    :: TankSupplyVol          = 0.0d0  !
  REAL(r64)    :: StarvedMakeUpVdot      = 0.0d0  !
  REAL(r64)    :: StarvedMakeUpVol       = 0.0d0  !
  REAL(r64)    :: BypassFraction         = 0.0d0  ! Added for fluid bypass
  INTEGER      :: NumCellON              = 0    ! for multi-cell tower
  INTEGER      :: SpeedSelected            = 0    ! Speed selected for the two speed tower

END TYPE ReportVars

TYPE VSTowerData
! variables specific to variable-speed towers
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Coeff                       !- model coefficients
  LOGICAL                         :: FoundModelCoeff   = .FALSE. !- TRUE if model is calibratable
  REAL(r64)                       :: MinInletAirWBTemp = 0.0d0     !- model limit for min inlet air WB temp
  REAL(r64)                       :: MaxInletAirWBTemp = 0.0d0     !- model limit for max inlet air WB temp
  REAL(r64)                       :: MinRangeTemp      = 0.0d0     !- model limit for min range temp
  REAL(r64)                       :: MaxRangeTemp      = 0.0d0     !- model limit for max range temp
  REAL(r64)                       :: MinApproachTemp   = 0.0d0     !- model limit for min approach temp
  REAL(r64)                       :: MaxApproachTemp   = 0.0d0     !- model limit for max approach temp
  REAL(r64)                       :: MinWaterFlowRatio = 0.0d0     !- model limit for min water flow rate ratio
  REAL(r64)                       :: MaxWaterFlowRatio = 0.0d0     !- model limit for max water flow rate ratio
  REAL(r64)                       :: MaxLiquidToGasRatio = 0.0d0   !- model limit for max liquid to gas ratio
  INTEGER                         :: VSErrorCountFlowFrac  = 0   !- counter if water flow rate ratio limits are exceeded
  INTEGER                         :: VSErrorCountWFRR  = 0       !- counter if water flow rate ratio limits are exceeded
  INTEGER                         :: VSErrorCountIAWB  = 0       !- counter if inlet air wet-bulb temperature limits are exceeded
  INTEGER                         :: VSErrorCountTR    = 0       !- counter if tower range temperature limits are exceeded
  INTEGER                         :: VSErrorCountTA    = 0       !- counter if tower approach temperature limits are exceeded
  INTEGER                         :: ErrIndexFlowFrac  = 0       !- index to recurring error structure for liquid to gas ratio
  INTEGER                         :: ErrIndexWFRR      = 0       !- index to recurring error structure for water flow rate ratio
  INTEGER                         :: ErrIndexIAWB      = 0       !- index to recurring error structure for inlet air WB
  INTEGER                         :: ErrIndexTR        = 0       !- index to recurring error structure for tower range
  INTEGER                         :: ErrIndexTA        = 0       !- index to recurring error structure for tower approach
  INTEGER                         :: ErrIndexLG        = 0       !- index to recurring error structure for tower liquid/gas ratio
                                                                 !- Tr = Range temperature
  CHARACTER(len=220)              :: TrBuffer1         = ' '     !- buffer to print Tr warning messages on following time step
  CHARACTER(len=300)              :: TrBuffer2         = ' '     !- buffer to print Tr warning messages on following time step
  CHARACTER(len=80)               :: TrBuffer3         = ' '     !- buffer to print Tr warning messages on following time step
                                                                 !- Twb = Wet-bulb temperature
  CHARACTER(len=220)              :: TwbBuffer1        = ' '     !- buffer to print Twb warning messages on following time step
  CHARACTER(len=300)              :: TwbBuffer2        = ' '     !- buffer to print Twb warning messages on following time step
  CHARACTER(len=80)               :: TwbBuffer3        = ' '     !- buffer to print Twb warning messages on following time step
                                                                 !- Ta = Approach temperature
  CHARACTER(len=220)              :: TaBuffer1         = ' '     !- buffer to print Ta warning messages on following time step
  CHARACTER(len=300)              :: TaBuffer2         = ' '     !- buffer to print Ta warning messages on following time step
  CHARACTER(len=80)               :: TaBuffer3         = ' '     !- buffer to print Ta warning messages on following time step
                                                                 !- WFRR = Water flow rate ratio
  CHARACTER(len=220)              :: WFRRBuffer1       = ' '     !- buffer to print WFRR warning messages on following time step
  CHARACTER(len=300)              :: WFRRBuffer2       = ' '     !- buffer to print WFRR warning messages on following time step
  CHARACTER(len=80)               :: WFRRBuffer3       = ' '     !- buffer to print WFRR warning messages on following time step
                                                                 !- LG = Liquid to gas ratio
  CHARACTER(len=220)              :: LGBuffer1         = ' '     !- buffer to print LG warning messages on following time step
  CHARACTER(len=300)              :: LGBuffer2         = ' '     !- buffer to print LG warning messages on following time step
  CHARACTER(len=80)               :: LGBuffer3         = ' '     !- buffer to print LG warning messages on following time step
  LOGICAL                         :: PrintTrMessage    = .FALSE. !- flag to print Tr error message
  LOGICAL                         :: PrintTwbMessage   = .FALSE. !- flag to print Twb error message
  LOGICAL                         :: PrintTaMessage    = .FALSE. !- flag to print Ta error message
  LOGICAL                         :: PrintWFRRMessage  = .FALSE. !- flag to print WFRR error message
  LOGICAL                         :: PrintLGMessage    = .FALSE. !- flag to print liquid-gas ratio error message
  REAL(r64)                       :: TrLast            = 0.0d0 ! value of Tr when warning occurred (passed to Recurring Warning)
  REAL(r64)                       :: TwbLast           = 0.0d0 ! value of Twb when warning occurred (passed to Recurring Warning)
  REAL(r64)                       :: TaLast            = 0.0d0 ! value of Ta when warning occurred (passed to Recurring Warning)
  REAL(r64)                       :: WaterFlowRateRatioLast = 0.0d0 ! value of WFRR when warning occurred (passed to Recurring Warn)
  REAL(r64)                       :: LGLast            = 0.0d0      ! value of LG when warning occurred (passed to Recurring Warn)
END TYPE VSTowerData

  ! MODULE VARIABLE DECLARATIONS:
INTEGER           :: NumSimpleTowers          = 0      ! Number of similar towers

!? The following block of variables are used to carry model results for a tower instance
!   across sim, update, and report routines.  Simulation manager must be careful
!   in models with multiple towers.

REAL(r64)         :: InletWaterTemp           = 0.0d0    ! CW temperature at tower inlet
REAL(r64)         :: OutletWaterTemp          = 0.0d0    ! CW temperature at tower outlet
INTEGER           :: WaterInletNode           = 0      ! Node number at tower inlet
INTEGER           :: WaterOutletNode          = 0      ! Node number at tower outlet
REAL(r64)         :: WaterMassFlowRate        = 0.0d0    ! WaterMassFlowRate through tower

REAL(r64)         :: Qactual                  = 0.0d0    ! Tower heat transfer
REAL(r64)         :: CTFanPower               = 0.0d0    ! Tower fan power used
REAL(r64)         :: AirFlowRateRatio         = 0.0d0    ! Ratio of air flow rate through VS cooling tower to design air flow rate
REAL(r64)         :: BasinHeaterPower         = 0.0d0    ! Basin heater power use (W)
REAL(r64)         :: WaterUsage               = 0.0d0    ! Tower water usage (m3/s)
REAL(r64)         :: FanCyclingRatio          = 0.0d0    ! cycling ratio of tower fan when min fan speed provide to much capacity


TYPE (Towerspecs),      ALLOCATABLE, DIMENSION(:) :: SimpleTower         ! dimension to number of machines
TYPE (TowerInletConds), ALLOCATABLE, DIMENSION(:) :: SimpleTowerInlet    ! inlet conditions
TYPE (ReportVars),      ALLOCATABLE, DIMENSION(:) :: SimpleTowerReport   ! report variables
TYPE (VSTowerData),     ALLOCATABLE, DIMENSION(:) :: VSTower             ! model coefficients and specific variables for VS tower

LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE CondenserLoopTowers

          ! Driver/Manager Routines
PUBLIC     SimTowers             ! PlantCondLoopSupplySideManager calls this top level Subroutine

          ! Get Input routines for module
PRIVATE    GetTowerInput         ! Retrieves inputs for specified tower

          ! Initialization routines for module
PRIVATE    InitTower             ! Initializes tower variables
PRIVATE    InitSimVars           ! Initializes model level variables
PRIVATE    SizeTower             ! Automatically sizes the cooling tower; also, calculates UA based on nominal capacity input(s)
PRIVATE    SizeVSMerkelTower     ! alternate sizing routine for VS merkel/shceier model.

          ! Update routines to check convergence and update nodes
PRIVATE    SimSimpleTower        ! Calculates exiting water temperature of tower
PRIVATE    CalcSingleSpeedTower  ! Simulates a single speed tower using SimSimpleTower
PRIVATE    CalcTwoSpeedTower     ! Simulates a two speed tower using SimSimpleTower
PRIVATE    CalcMerkelVariableSpeedTower
PRIVATE    CalcVariableSpeedTower ! Simulates a variable speed tower using SimSimpleTower
PRIVATE    SimpleTowerUAResidual ! Given a design tower load and a UA, calculates a residual
PRIVATE    CalculateWaterUseage  ! calculates water consumed by the different towers
PRIVATE    UpdateTowers          ! Updates node information and checks mass flow rate
PRIVATE    ReportTowers          ! Outputs report variables
PRIVATE    CalcVSTowerApproach   ! Calculates approach temperature for variable speed towers

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of CondenserLoopTowers Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimTowers(TowerType,TowerName, CompIndex, RunFlag,InitLoopEquip, MyLoad, &
                     MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Don Shirey
          !       DATE WRITTEN   Dec. 2000
          !       MODIFIED       Fred Buhl, May 2002; Richard Raustad, FSEC, Feb 2005 (added VS tower)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Main cooling tower driver subroutine.  Gets called from
          ! PlantLoopEquipments.

          ! METHODOLOGY EMPLOYED:
          ! After being called by PlantLoopEquipments, this subroutine
          ! calls GetTowerInput to get all cooling tower input info (one time only),
          ! then calls the appropriate subroutine to calculate tower performance,
          ! update records (node info) and writes output report info.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE DataPlant,      ONLY: PlantSizesOkayToFinalize

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),INTENT(IN) :: TowerType
  CHARACTER(len=*),INTENT(IN) :: TowerName
  INTEGER,INTENT(INOUT)       :: CompIndex
  LOGICAL,INTENT(INOUT)       :: RunFlag
  LOGICAL, INTENT(IN)         :: InitLoopEquip
  REAL(r64),INTENT(INOUT)     :: MyLoad
  REAL(r64),INTENT(INOUT)     :: OptCap
  REAL(r64),INTENT(INOUT)     :: MaxCap
  REAL(r64),INTENT(INOUT)     :: MinCap
  LOGICAL, INTENT(IN)         :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(INOUT)    :: SizingFactor     ! sizing factor

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE          :: GetInput = .TRUE.
  INTEGER               :: TowerNum

          !GET INPUT
  IF (GetInput) THEN
    CALL GetTowerInput
    GetInput = .FALSE.
  END IF

    ! Find the correct CoolingTower
  IF (CompIndex == 0) THEN
    TowerNum=FindItemInList(TowerName,SimpleTower%Name,NumSimpleTowers)
    IF (TowerNum == 0) THEN
      CALL ShowFatalError('SimTowers: Unit not found='//TRIM(TowerName))
    ENDIF
    CompIndex=TowerNum
  ELSE
    TowerNum=CompIndex
    IF (TowerNum > NumSimpleTowers .or. TowerNum < 1) THEN
      CALL ShowFatalError('SimTowers:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(TowerNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumSimpleTowers))//  &
                          ', Entered Unit name='//TRIM(TowerName))
    ENDIF
    IF (CheckEquipName(TowerNum)) THEN
      IF (TowerName /= SimpleTower(TowerNum)%Name) THEN
        CALL ShowFatalError('SimTowers: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(TowerNum))// &
                            ', Unit name='//TRIM(TowerName)//', stored Unit Name for that index='//  &
                            TRIM(SimpleTower(TowerNum)%Name))
      ENDIF
      CheckEquipName(TowerNum)=.false.
    ENDIF
  ENDIF

          !INITIALIZE
  CALL InitSimVars

          !CALCULATE
  TypeOfEquip:   &
    SELECT CASE (SimpleTower(TowerNum)%TowerType_Num)

      CASE (CoolingTower_SingleSpeed)

        IF (InitLoopEquip) THEN
          CALL InitTower(TowerNum, RunFlag)
          CALL SizeTower(TowerNum)
          MinCap = 0.0d0
          MaxCap = SimpleTower(TowerNum)%TowerNominalCapacity &
                      * SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
          OptCap = SimpleTower(TowerNum)%TowerNominalCapacity
          IF (GetSizingFactor) THEN
            SizingFactor = SimpleTower(TowerNum)%SizFac
          END IF
          RETURN
        END IF
        CALL InitTower(TowerNum, RunFlag)
        CALL CalcSingleSpeedTower(TowerNum)
        CALL CalculateWaterUseage(TowerNum)
        CALL UpdateTowers(TowerNum)
        CALL ReportTowers(RunFlag,TowerNum)

      CASE (CoolingTower_TwoSpeed)

        IF (InitLoopEquip) THEN
          CALL InitTower(TowerNum, RunFlag)
          CALL SizeTower(TowerNum)
          MinCap = 0.0d0
          MaxCap = SimpleTower(TowerNum)%TowerNominalCapacity &
                      * SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
          OptCap = SimpleTower(TowerNum)%TowerNominalCapacity
          IF (GetSizingFactor) THEN
            SizingFactor = SimpleTower(TowerNum)%SizFac
          END IF
          RETURN
        END IF
        CALL InitTower(TowerNum, RunFlag)
        CALL CalcTwoSpeedTower(TowerNum)
        CALL CalculateWaterUseage(TowerNum)
        CALL UpdateTowers(TowerNum)
        CALL ReportTowers(RunFlag,TowerNum)

      CASE (CoolingTower_VariableSpeedMerkel)

        IF (InitLoopEquip) THEN
          CALL InitTower(TowerNum, RunFlag)
          CALL SizeVSMerkelTower(TowerNum)
          MinCap = 0.0d0
          MaxCap = SimpleTower(TowerNum)%TowerNominalCapacity &
                      * SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
          OptCap = SimpleTower(TowerNum)%TowerNominalCapacity
          IF (GetSizingFactor) THEN
            SizingFactor = SimpleTower(TowerNum)%SizFac
          END IF
          RETURN
        END IF
        CALL InitTower(TowerNum, RunFlag)
        CALL CalcMerkelVariableSpeedTower(TowerNum, MyLoad)
        CALL CalculateWaterUseage(TowerNum)
        CALL UpdateTowers(TowerNum)
        CALL ReportTowers(RunFlag,TowerNum)

      CASE (CoolingTower_VariableSpeed)

        IF (InitLoopEquip) THEN
          CALL InitTower(TowerNum, RunFlag)
          CALL SizeTower(TowerNum)
          MinCap = 0.0d0
          MaxCap = SimpleTower(TowerNum)%TowerNominalCapacity &
                      * SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
          OptCap = SimpleTower(TowerNum)%TowerNominalCapacity
          IF (GetSizingFactor) THEN
            SizingFactor = SimpleTower(TowerNum)%SizFac
          END IF
          RETURN
        END IF
        CALL InitTower(TowerNum, RunFlag)
        CALL CalcVariableSpeedTower(TowerNum)
        CALL CalculateWaterUseage(TowerNum)
        CALL UpdateTowers(TowerNum)
        CALL ReportTowers(RunFlag,TowerNum)

      CASE DEFAULT
        CALL ShowFatalError('SimTowers: Invalid Tower Type Requested='//TRIM(TowerType))

    END SELECT TypeOfEquip

RETURN
END SUBROUTINE SimTowers

! End CondenserLoopTowers Module Driver Subroutines
!******************************************************************************


! Beginning of CondenserLoopTowers Module Get Input subroutines
!******************************************************************************

SUBROUTINE GetTowerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    April 1998
          !       MODIFIED         Don Shirey, Jan 2001 and Sept/Oct 2002; Richard Raustad, FSEC, Feb 2005 (added VS tower)
          !                        B. Griffith, Aug. 2006 water consumption modeling and water system connections
          !                        T Hong, Aug. 2008: added fluid bypass for single speed tower
          !                        A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for cooling towers and stores it in SimpleTower data structure. Additional structure
          ! (VSTower) stores the coefficients for each VS tower.

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in the data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,     ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, MakeUPPERCase
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE NodeInputManager,   ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataSizing,         ONLY: AutoSize
  USE CurveManager,       ONLY: GetCurveIndex
  USE ScheduleManager,    ONLY: GetScheduleIndex
  USE WaterManager ,      ONLY: SetupTankDemandComponent
  USE OutAirNodeManager,  ONLY: CheckOutAirNodeNumber
  USE General,            ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F5.2)'
    CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                     :: TowerNum                  ! Tower number, reference counter for SimpleTower data array
    INTEGER                     :: NumSingleSpeedTowers      ! Total number of single-speed cooling towers
    INTEGER                     :: SingleSpeedTowerNumber    ! Specific single-speed tower of interest
    INTEGER                     :: NumTwoSpeedTowers         ! Number of two-speed cooling towers
    INTEGER                     :: TwoSpeedTowerNumber       ! Specific two-speed tower of interest
    INTEGER                     :: NumVariableSpeedTowers    ! Number of variable-speed cooling towers
    INTEGER                     :: VariableSpeedTowerNumber  ! Specific variable-speed tower of interest
    INTEGER                     :: NumVSCoolToolsModelCoeffs ! Number of CoolTools VS cooling tower coefficient objects
    INTEGER                     :: NumVSYorkCalcModelCoeffs  ! Number of YorkCalc VS cooling tower coefficient objects
    INTEGER                     :: NumVSMerkelTowers         ! Number of Merkel variable speed cooling towers
    INTEGER                     :: MerkelVSTowerNum          ! specific merkel variable speed tower of interest
    INTEGER                     :: VSModelCoeffNum           ! Specific variable-speed tower coefficient object of interest
    INTEGER                     :: NumAlphas                 ! Number of elements in the alpha array
    INTEGER                     :: NumNums                   ! Number of elements in the numeric array
    INTEGER                     :: NumAlphas2                ! Number of elements in the alpha2 array
    INTEGER                     :: NumNums2                  ! Number of elements in the numeric2 array
    INTEGER                     :: IOStat                    ! IO Status when calling get input subroutine
    INTEGER                     :: CoeffNum                  ! Index for reading user defined VS tower coefficients
    LOGICAL                     :: IsNotOK                   ! Flag to verify name
    LOGICAL                     :: IsBlank                   ! Flag for blank name
    LOGICAL, SAVE               :: ErrorsFound=.false.       ! Logical flag set .true. if errors found while getting input data
    CHARACTER(len=6)            :: OutputChar                ! report variable for warning messages
    CHARACTER(len=6)            :: OutputCharLo              ! report variable for warning messages
    CHARACTER(len=6)            :: OutputCharHi              ! report variable for warning messages
    REAL(r64), DIMENSION(29)         :: NumArray                  ! Numeric input data array
    REAL(r64), DIMENSION(43)         :: NumArray2                 ! Numeric input data array for VS tower coefficients
    CHARACTER(len=MaxNameLength),DIMENSION(15) :: AlphArray   ! Character string input data array
    CHARACTER(len=MaxNameLength),DIMENSION(1) :: AlphArray2  ! Character string input data array for VS tower coefficients

  ! Get number of all cooling towers specified in the input data file (idf)
  NumSingleSpeedTowers   = GetNumObjectsFound(cCoolingTower_SingleSpeed)
  NumTwoSpeedTowers      = GetNumObjectsFound(cCoolingTower_TwoSpeed)
  NumVariableSpeedTowers = GetNumObjectsFound(cCoolingTower_VariableSpeed)
  NumVSMerkelTowers      = GetNumObjectsFound(cCoolingTower_VariableSpeedMerkel)
  NumSimpleTowers        = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + NumVSMerkelTowers

  IF (NumSimpleTowers <=0 ) &
    CALL ShowFatalError('No Cooling Tower objects found in input, however, a branch object has specified a cooling tower. '//&
                        'Search the input for CoolingTower to determine the cause for this error.')

  ! See if load distribution manager has already gotten the input
  IF (ALLOCATED(SimpleTower))RETURN

  ! Allocate data structures to hold tower input data, report data and tower inlet conditions
  ALLOCATE (SimpleTower(NumSimpleTowers))
  ALLOCATE (SimpleTowerReport(NumSimpleTowers))
  ALLOCATE (SimpleTowerInlet(NumSimpleTowers))
  ALLOCATE(CheckEquipName(NumSimpleTowers))
  CheckEquipName=.true.
  ! Allocate variable-speed tower structure with data specific to this type
  IF(NumVariableSpeedTowers .GT. 0)THEN
    ALLOCATE (VSTower(NumVariableSpeedTowers))
  ! Allow users to input model coefficients other than default
    NumVSCoolToolsModelCoeffs = GetNumObjectsFound('CoolingTowerPerformance:CoolTools')
    NumVSYorkCalcModelCoeffs  = GetNumObjectsFound('CoolingTowerPerformance:YorkCalc')
  END IF

  ! Load data structures with cooling tower input data
  cCurrentModuleObject = cCoolingTower_SingleSpeed
  DO SingleSpeedTowerNumber = 1 , NumSingleSpeedTowers
    TowerNum = SingleSpeedTowerNumber
    CALL GetObjectItem(cCurrentModuleObject,SingleSpeedTowerNumber,AlphArray,NumAlphas, &
                       NumArray,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),SimpleTower%Name,TowerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    SimpleTower(TowerNum)%Name                     = AlphArray(1)
    SimpleTower(TowerNum)%TowerType                = TRIM(cCurrentModuleObject)
    SimpleTower(TowerNum)%TowerType_Num            = CoolingTower_SingleSpeed
    SimpleTower(TowerNum)%TowerMassFlowRateMultiplier = 2.5d0
    SimpleTower(TowerNum)%WaterInletNodeNum        = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    SimpleTower(TowerNum)%WaterOutletNodeNum       = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Chilled Water Nodes')
    SimpleTower(TowerNum)%DesignWaterFlowRate      = NumArray(1)
    SimpleTower(TowerNum)%HighSpeedAirFlowRate     = NumArray(2)
    SimpleTower(TowerNum)%HighSpeedFanPower        = NumArray(3)
    SimpleTower(TowerNum)%HighSpeedTowerUA         = NumArray(4)
    SimpleTower(TowerNum)%FreeConvAirFlowRate      = NumArray(5)
    SimpleTower(TowerNum)%FreeConvAirFlowRateSizingFactor = NumArray(6)
    SimpleTower(TowerNum)%FreeConvTowerUA          = NumArray(7)
    SimpleTower(TowerNum)%FreeConvTowerUASizingFactor    = NumArray(8)
    SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio = NumArray(9)
    SimpleTower(TowerNum)%TowerNominalCapacity     = NumArray(10)
    SimpleTower(TowerNum)%TowerFreeConvNomCap      = NumArray(11)
    SimpleTower(TowerNum)%TowerFreeConvNomCapSizingFactor = NumArray(12)
    IF (NumAlphas >= 4) THEN
      IF (SameString(AlphArray(4),'UFactorTimesAreaAndDesignWaterFlowRate')) THEN
        SimpleTower(TowerNum)%PerformanceInputMethod_Num = PIM_UFactor
      ELSEIF (SameString(AlphArray(4),'NominalCapacity')) THEN
        SimpleTower(TowerNum)%PerformanceInputMethod_Num = PIM_NominalCapacity
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('Invalid, '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(AlphArray(4)))
        errorsfound = .true.
      ENDIF
    ELSE
     ! Since Performance Input Method has been omitted then assume it to be UA and DESIGN WATER FLOW RATE
      SimpleTower(TowerNum)%PerformanceInputMethod_Num = PIM_UFactor
    ENDIF

    !   Basin heater power as a function of temperature must be greater than or equal to 0
    SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff = NumArray(13)
    IF(NumArray(13) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                     '" basin heater power as a function of temperature difference must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    SimpleTower(TowerNum)%BasinHeaterSetPointTemp = NumArray(14)

    IF(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 14) THEN
        SimpleTower(TowerNum)%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(simpleTower(TowerNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(SimpleTower(TowerNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(14))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(AlphArray(5) .NE. Blank)THEN
      SimpleTower(TowerNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(AlphArray(5))
      IF(SimpleTower(TowerNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" basin heater schedule name "'//TRIM(AlphArray(5)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF

    ! begin water use and systems get input
    IF (SameString(AlphArray(6),'LossFactor')) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByUserFactor
    ELSEIF (SameString(AlphArray(6), 'SaturatedExit')) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSEIF (AlphArray(6) == Blank) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid, '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(AlphArray(6)))
      errorsfound = .true.
    ENDIF

    SimpleTower(TowerNum)%UserEvapLossFactor = NumArray(15) !  N11 , \field Evaporation Loss Factor
    SimpleTower(TowerNum)%DriftLossFraction = NumArray(16)/100.0d0  !  N12, \field Drift Loss Percent

    If ((NumNums < 16) .and. (SimpleTower(TowerNum)%DriftLossFraction == 0.0d0) ) Then
      ! assume Drift loss not entered and should be defaulted
      SimpleTower(TowerNum)%DriftLossFraction = 0.008d0 /100.0d0
    endif

    SimpleTower(TowerNum)%ConcentrationRatio = NumArray(17) !  N13, \field Blowdown Concentration Ratio
    SimpleTower(TowerNum)%SizFac = NumArray(21)             !  N17  \field Sizing Factor
    IF (SimpleTower(TowerNum)%SizFac <= 0.0d0) SimpleTower(TowerNum)%SizFac = 1.0d0

    If (SameString(AlphArray(7), 'ScheduledRate')) then
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownBySchedule
    ELSEIF (SameString(AlphArray(7), 'ConcentrationRatio')) THEN
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownByConcentration
    ELSEIF (AlphArray(7) == Blank) THEN
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownByConcentration
      If ((NumNums < 17) .and.(SimpleTower(TowerNum)%ConcentrationRatio == 0.0d0) ) THEN
        ! assume Concetratino ratio was omitted and should be defaulted
            SimpleTower(TowerNum)%ConcentrationRatio = 3.0d0
      endif
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid, '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(AlphArray(7)))
      errorsfound = .true.
    ENDIF
    SimpleTower(TowerNum)%SchedIDBlowdown = GetScheduleIndex(AlphArray(8))
    If ((SimpleTower(TowerNum)%SchedIDBlowdown == 0) .AND. (SimpleTower(TowerNum)%BlowdownMode == BlowdownBySchedule))Then
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid, '//TRIM(cAlphaFieldNames(8))//' = '//TRIM(AlphArray(8)))
      errorsfound = .true.
    endif

    IF (AlphArray(9) == Blank) THEN
      SimpleTower(TowerNum)%SuppliedByWaterSystem = .false.
    ELSE ! water from storage tank
      !
      Call SetupTankDemandComponent(AlphArray(1), TRIM(cCurrentModuleObject), AlphArray(9), ErrorsFound, &
                              SimpleTower(TowerNum)%WaterTankID, SimpleTower(TowerNum)%WaterTankDemandARRID)
      SimpleTower(TowerNum)%SuppliedByWaterSystem = .TRUE.
    ENDIF

!   outdoor air inlet node

    IF (lAlphaFieldBlanks(10)) THEN
      SimpleTower(TowerNum)%OutdoorAirInletNodeNum = 0
    ELSE
      SimpleTower(TowerNum)%OutdoorAirInletNodeNum = &
       GetOnlySingleNode(AlphArray(10),ErrorsFound,TRIM(cCurrentModuleObject),SimpleTower(TowerNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
      IF(.not. CheckOutAirNodeNumber(SimpleTower(TowerNum)%OutdoorAirInletNodeNum))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                          '" Outdoor Air Inlet Node Name not valid Outdoor Air Node= '//TRIM(AlphArray(10)))
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound=.true.
      END IF
    ENDIF

!   fluid bypass for single speed tower
    IF (lAlphaFieldBlanks(11).or.AlphArray(11) == Blank) THEN
      SimpleTower(TowerNum)%CapacityControl = CapacityControl_FanCycling     ! FanCycling
    ELSE
      SELECT CASE (MakeUPPERCase(AlphArray(11)))
        CASE ('FANCYCLING')
          SimpleTower(TowerNum)%CapacityControl = CapacityControl_FanCycling
        CASE ('FLUIDBYPASS')
          SimpleTower(TowerNum)%CapacityControl = CapacityControl_FluidBypass
        CASE DEFAULT
          SimpleTower(TowerNum)%CapacityControl = CapacityControl_FanCycling
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" The Capacity Control is not specified correctly. The default Fan Cycling is used.')
      END SELECT
    ENDIF

  !added for multi-cell
    SimpleTower(TowerNum)%NumCell                  = NumArray(18)
    If ((NumNums < 18) .and. (SimpleTower(TowerNum)%NumCell == 0) ) Then
      ! assume Number of Cells not entered and should be defaulted
      SimpleTower(TowerNum)%NumCell = 1
    endif
    SimpleTower(TowerNum)%MinFracFlowRate          = NumArray(19)
    If ((NumNums < 19) .and. (SimpleTower(TowerNum)%MinFracFlowRate == 0.0d0) ) Then
      ! assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
      SimpleTower(TowerNum)%MinFracFlowRate = 0.33d0
    endif
    SimpleTower(TowerNum)%MaxFracFlowRate              = NumArray(20)
    If ((NumNums < 20) .and. (SimpleTower(TowerNum)%MaxFracFlowRate == 0.0d0) ) Then
      ! assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
      SimpleTower(TowerNum)%MaxFracFlowRate = 2.5d0
    endif

    IF (NumAlphas >= 12) THEN
      IF (lAlphaFieldBlanks(12).or.AlphArray(12) == Blank) THEN
      SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
      SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
      ELSE
        IF (SameString(AlphArray(12),'MinimalCell') .OR. &
            SameString(AlphArray(12),'MaximalCell') ) THEN
          IF (SameString(AlphArray(12),'MinimalCell')) THEN
             SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MinCell
             SimpleTower(TowerNum)%CellCtrl='MinimalCell'
          ENDIF
          IF (SameString(AlphArray(12),'MaximalCell'))  THEN
             SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
             SimpleTower(TowerNum)%CellCtrl='MaximalCell'
          ENDIF
        ELSE
          CALL ShowSevereError('Illegal '//TRIM(cAlphaFieldNames(12))//' = '//TRIM(AlphArray(12)))
          CALL ShowContinueError('Occurs in '//SimpleTower(TowerNum)%TowerType//'='//TRIM(SimpleTower(TowerNum)%Name))
          ErrorsFound=.TRUE.
        END IF
      END IF
    ELSE
      !assume Cell Control not entered and should be defaulted
      SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
      SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
    END IF

!   High speed air flow rate must be greater than free convection air flow rate.
!   Can't tell yet if autosized, check later in InitTower.
    IF (SimpleTower(TowerNum)%HighSpeedAirFlowRate <= SimpleTower(TowerNum)%FreeConvAirFlowRate .and. &
        SimpleTower(TowerNum)%HighSpeedAirFlowRate .NE. AutoSize) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                            '". Free convection air flow rate must be less than the design air flow rate.')
       ErrorsFound=.true.
    ENDIF

!   Check various inputs if Performance Input Method = "UA and Design Water Flow Rate"
    IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_UFactor) THEN
      IF (SimpleTower(TowerNum)%DesignWaterFlowRate == 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Tower performance input method requires a design water flow rate greater than zero.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%HighSpeedTowerUA <= SimpleTower(TowerNum)%FreeConvTowerUA .and. &
          SimpleTower(TowerNum)%HighSpeedTowerUA .NE. AutoSize) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Free convection UA must be less than the design tower UA.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%FreeConvTowerUA > 0.0d0 .and. SimpleTower(TowerNum)%FreeConvAirFlowRate == 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                  '". Free convection air flow rate must be greater than zero when free convection UA is greater than zero.')
         ErrorsFound=.true.
      END IF
    ELSEIF(SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
      IF (SimpleTower(TowerNum)%TowerNominalCapacity == 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Tower performance input method requires valid nominal capacity.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%DesignWaterFlowRate .NE. 0.0d0) THEN
         IF (SimpleTower(TowerNum)%DesignWaterFlowRate > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Nominal capacity input method and design water flow rate have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                         '". Nominal capacity input method has been specified and design water flow rate is being autosized.')
         ENDIF
         CALL ShowContinueError('Design water flow rate must be left blank when nominal tower capacity input method is used.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%HighSpeedTowerUA .NE. 0.0d0) THEN
         IF (SimpleTower(TowerNum)%HighSpeedTowerUA > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Nominal tower capacity and design tower UA have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Nominal tower capacity has been specified and design tower UA is being autosized.')
         ENDIF
         CALL ShowContinueError('Design tower UA field must be left blank when nominal tower capacity input method is used.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%FreeConvTowerUA .NE. 0.0d0) THEN
         IF (SimpleTower(TowerNum)%FreeConvTowerUA > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Nominal capacity input method and free convection UA have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                               '". Nominal capacity input method has been specified and free convection UA is being autosized.')
         ENDIF
         CALL ShowContinueError('Free convection UA should be left blank when nominal tower capacity input method is used.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%TowerFreeConvNomCap >= SimpleTower(TowerNum)%TowerNominalCapacity) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Free convection nominal capacity must be less than the nominal (design) tower capacity.')
         ErrorsFound=.true.
      END IF
      IF (SimpleTower(TowerNum)%TowerFreeConvNomCap > 0.0d0 .and. SimpleTower(TowerNum)%FreeConvAirFlowRate == 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                  '". Free convection air flow must be greater than zero when tower free convection capacity is specified.')
         ErrorsFound=.true.
      END IF
    ELSE ! Tower performance input method is not specified as a valid "choice"
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                  '". Tower Performance Input Method must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".')
      CALL ShowContinueError('Tower Performanace Input Method currently specified as: '// &
                            TRIM(AlphArray(4)))
         ErrorsFound=.true.
    ENDIF
  END DO  ! End Single-Speed Tower Loop

  cCurrentModuleObject = cCoolingTower_TwoSpeed
  DO TwoSpeedTowerNumber = 1 , NumTwoSpeedTowers
    TowerNum = NumSingleSpeedTowers + TwoSpeedTowerNumber
    CALL GetObjectItem(cCurrentModuleObject,TwoSpeedTowerNumber,AlphArray,NumAlphas, &
                       NumArray,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),SimpleTower%Name,TowerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    SimpleTower(TowerNum)%Name                  = AlphArray(1)
    SimpleTower(TowerNum)%TowerType             = TRIM(cCurrentModuleObject)
    SimpleTower(TowerNum)%TowerType_Num         = CoolingTower_TwoSpeed
    SimpleTower(TowerNum)%TowerMassFlowRateMultiplier = 2.5d0
    SimpleTower(TowerNum)%WaterInletNodeNum     = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    SimpleTower(TowerNum)%WaterOutletNodeNum    = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Chilled Water Nodes')

    IF (NumAlphas >= 4) THEN
      IF (SameString(AlphArray(4),'UFactorTimesAreaAndDesignWaterFlowRate')) THEN
        SimpleTower(TowerNum)%PerformanceInputMethod_Num = PIM_UFactor
      ELSEIF (SameString(AlphArray(4),'NominalCapacity')) THEN
        SimpleTower(TowerNum)%PerformanceInputMethod_Num = PIM_NominalCapacity
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
        CALL ShowContinueError('Invalid, '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(AlphArray(4)))
        errorsfound = .true.
      ENDIF
    ELSE
     ! Since Performance Input Method has been omitted then assume it to be UA and DESIGN WATER FLOW RATE
      SimpleTower(TowerNum)%PerformanceInputMethod_Num = PIM_UFactor
    ENDIF
    SimpleTower(TowerNum)%DesignWaterFlowRate   = NumArray(1)
    SimpleTower(TowerNum)%HighSpeedAirFlowRate  = NumArray(2)
    SimpleTower(TowerNum)%HighSpeedFanPower     = NumArray(3)
    SimpleTower(TowerNum)%HighSpeedTowerUA      = NumArray(4)
    SimpleTower(TowerNum)%LowSpeedAirFlowRate   = NumArray(5)
    SimpleTower(TowerNum)%LowSpeedAirFlowRateSizingFactor  = NumArray(6)
    SimpleTower(TowerNum)%LowSpeedFanPower      = NumArray(7)
    SimpleTower(TowerNum)%LowSpeedFanPowerSizingFactor  = NumArray(8)
    SimpleTower(TowerNum)%LowSpeedTowerUA       = NumArray(9)
    SimpleTower(TowerNum)%LowSpeedTowerUASizingFactor  = NumArray(10)
    SimpleTower(TowerNum)%FreeConvAirFlowRate   = NumArray(11)
    SimpleTower(TowerNum)%FreeConvAirFlowRateSizingFactor = NumArray(12)
    SimpleTower(TowerNum)%FreeConvTowerUA       = NumArray(13)
    SimpleTower(TowerNum)%FreeConvTowerUASizingFactor  = NumArray(14)
    SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio  = NumArray(15)
    SimpleTower(TowerNum)%TowerNominalCapacity  = NumArray(16)
    SimpleTower(TowerNum)%TowerLowSpeedNomCap   = NumArray(17)
    SimpleTower(TowerNum)%TowerLowSpeedNomCapSizingFactor = NumArray(18)
    SimpleTower(TowerNum)%TowerFreeConvNomCap   = NumArray(19)
    SimpleTower(TowerNum)%TowerFreeConvNomCapSizingFactor  = NumArray(20)

    !   Basin heater power as a function of temperature must be greater than or equal to 0
    SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff = NumArray(21)
    IF(NumArray(21) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                      '" basin heater power as a function of temperature difference must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    SimpleTower(TowerNum)%BasinHeaterSetPointTemp = NumArray(22)
    IF(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 22) THEN
        SimpleTower(TowerNum)%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(simpleTower(TowerNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(SimpleTower(TowerNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(22))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(AlphArray(5) .NE. Blank)THEN
      SimpleTower(TowerNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(AlphArray(5))
      IF(SimpleTower(TowerNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" basin heater schedule name "'//TRIM(AlphArray(5)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF

    ! begin water use and systems get input
    IF (SameString(AlphArray(6),'LossFactor')) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByUserFactor
    ELSEIF (SameString(AlphArray(6), 'SaturatedExit')) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSEIF (lAlphaFieldBlanks(6)) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(AlphArray(6)))
      errorsfound = .true.
    ENDIF

    SimpleTower(TowerNum)%UserEvapLossFactor = NumArray(23) !  N23 , \field Evaporation Loss Factor
    SimpleTower(TowerNum)%DriftLossFraction = NumArray(24) / 100.0d0  !  N24, \field Drift Loss Percent
   If ((NumNums < 24) .and. (SimpleTower(TowerNum)%DriftLossFraction == 0.0d0) ) Then
    ! assume Drift loss not entered and should be defaulted
    SimpleTower(TowerNum)%DriftLossFraction = 0.008d0 /100.0d0
   endif

    SimpleTower(TowerNum)%ConcentrationRatio = NumArray(25) !  N17, \field Blowdown Concentration Ratio
    SimpleTower(TowerNum)%SizFac = NumArray(29)             !  N21  \field Sizing Factor
    IF (SimpleTower(TowerNum)%SizFac <= 0.0d0) SimpleTower(TowerNum)%SizFac = 1.0d0

    If (SameString(AlphArray(7), 'ScheduledRate')) then
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownBySchedule
    ELSEIF (SameString(AlphArray(7), 'ConcentrationRatio')) THEN
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownByConcentration
    ELSEIF (lAlphaFieldBlanks(7)) THEN
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownByConcentration
      If ((NumNums < 25) .and.(SimpleTower(TowerNum)%ConcentrationRatio == 0.0d0) ) THEN
        ! assume Concetration ratio was omitted and should be defaulted
            SimpleTower(TowerNum)%ConcentrationRatio = 3.0d0
      endif
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(AlphArray(7)))
      errorsfound = .true.
    ENDIF
    SimpleTower(TowerNum)%SchedIDBlowdown = GetScheduleIndex(AlphArray(8))
    If ((SimpleTower(TowerNum)%SchedIDBlowdown == 0) .AND. (SimpleTower(TowerNum)%BlowdownMode == BlowdownBySchedule)) Then
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(AlphArray(8)))
      errorsfound = .true.
    endif

  !added for multi-cell
    SimpleTower(TowerNum)%NumCell                  = NumArray(26)
    If ((NumNums < 26) .and. (SimpleTower(TowerNum)%NumCell == 0) ) Then
      ! assume Number of Cells not entered and should be defaulted
      SimpleTower(TowerNum)%NumCell = 1
    endif
    SimpleTower(TowerNum)%MinFracFlowRate          = NumArray(27)
    If ((NumNums < 27) .and. (SimpleTower(TowerNum)%MinFracFlowRate == 0.0d0) ) Then
      ! assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
      SimpleTower(TowerNum)%MinFracFlowRate = 0.33d0
    endif
    SimpleTower(TowerNum)%MaxFracFlowRate              = NumArray(28)
    If ((NumNums < 28) .and. (SimpleTower(TowerNum)%MaxFracFlowRate == 0.0d0) ) Then
      ! assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
      SimpleTower(TowerNum)%MaxFracFlowRate = 2.5d0
    endif

    IF (NumAlphas >= 11) THEN
      IF (lAlphaFieldBlanks(11).or.AlphArray(11) == Blank) THEN
        SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
        SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
      ELSE
        IF (SameString(AlphArray(11),'MinimalCell') .OR. &
            SameString(AlphArray(11),'MaximalCell') ) THEN
          IF (SameString(AlphArray(11),'MinimalCell')) THEN
             SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MinCell
             SimpleTower(TowerNum)%CellCtrl= 'MinimalCell'
          ENDIF
          IF (SameString(AlphArray(11),'MaximalCell')) THEN
             SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
             SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
          ENDIF
        ELSE
          CALL ShowSevereError('Illegal '//TRIM(cAlphaFieldNames(12))//' = '//TRIM(AlphArray(12)))
          CALL ShowContinueError('Occurs in '//SimpleTower(TowerNum)%TowerType//'='//TRIM(SimpleTower(TowerNum)%Name))
          ErrorsFound=.TRUE.
        END IF
      END IF
    ELSE
      !assume Cell Control not entered and should be defaulted
      SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
      SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
    END IF

    IF (lAlphaFieldBlanks(9)) THEN
      SimpleTower(TowerNum)%SuppliedByWaterSystem = .false.
    ELSE ! water from storage tank
      !
      Call SetupTankDemandComponent(AlphArray(1), TRIM(cCurrentModuleObject), AlphArray(9), ErrorsFound, &
                              SimpleTower(TowerNum)%WaterTankID, SimpleTower(TowerNum)%WaterTankDemandARRID)
      SimpleTower(TowerNum)%SuppliedByWaterSystem = .TRUE.
    ENDIF

!   outdoor air inlet node
    IF (lAlphaFieldBlanks(10)) THEN
      SimpleTower(TowerNum)%OutdoorAirInletNodeNum = 0
    ELSE
      SimpleTower(TowerNum)%OutdoorAirInletNodeNum = &
       GetOnlySingleNode(AlphArray(10),ErrorsFound,TRIM(cCurrentModuleObject),SimpleTower(TowerNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
      IF(.not. CheckOutAirNodeNumber(SimpleTower(TowerNum)%OutdoorAirInletNodeNum))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                          '" Outdoor Air Inlet Node Name not valid Outdoor Air Node= '//TRIM(AlphArray(10)))
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound=.true.
      END IF
    ENDIF



!   High speed air flow rate must be greater than low speed air flow rate.
!   Can't tell yet if autosized, check later in InitTower.
    IF (SimpleTower(TowerNum)%HighSpeedAirFlowRate <= SimpleTower(TowerNum)%LowSpeedAirFlowRate .and. &
        SimpleTower(TowerNum)%HighSpeedAirFlowRate .NE. AutoSize) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                            '". Low speed air flow rate must be less than the high speed air flow rate.')
       ErrorsFound=.true.
    ENDIF
!   Low speed air flow rate must be greater than free convection air flow rate.
!   Can't tell yet if autosized, check later in InitTower.
    IF (SimpleTower(TowerNum)%LowSpeedAirFlowRate <= SimpleTower(TowerNum)%FreeConvAirFlowRate .and. &
        SimpleTower(TowerNum)%LowSpeedAirFlowRate .NE. AutoSize) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                            '". Free convection air flow rate must be less than the low speed air flow rate.')
       ErrorsFound=.true.
    ENDIF

!   Check various inputs if Performance Input Method = "UA and Design Water Flow Rate"
    IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_UFactor) THEN
      IF (SimpleTower(TowerNum)%DesignWaterFlowRate == 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Tower performance input method requires a design water flow rate greater than zero.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%HighSpeedTowerUA <= SimpleTower(TowerNum)%LowSpeedTowerUA .and. &
          SimpleTower(TowerNum)%HighSpeedTowerUA .NE. AutoSize) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Tower UA at low fan speed must be less than the tower UA at high fan speed.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%LowSpeedTowerUA <= SimpleTower(TowerNum)%FreeConvTowerUA .and. &
          SimpleTower(TowerNum)%LowSpeedTowerUA .NE. AutoSize) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                            '". Tower UA at free convection air flow rate must be less than the tower UA at low fan speed.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%FreeConvTowerUA > 0.0d0 .and. SimpleTower(TowerNum)%FreeConvAirFlowRate == 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                  '". Free convection air flow rate must be greater than zero when free convection UA is greater than zero.')
         ErrorsFound=.true.
      END IF
    ELSEIF(SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
      IF (SimpleTower(TowerNum)%TowerNominalCapacity == 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Tower performance input method requires valid high-speed nominal capacity.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%TowerLowSpeedNomCap == 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Tower performance input method requires valid low-speed nominal capacity.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%DesignWaterFlowRate .NE. 0.0d0) THEN
         IF (SimpleTower(TowerNum)%DesignWaterFlowRate > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Nominal capacity input method and design water flow rate have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                '". Nominal capacity input method has been specified and design water flow rate is being autosized.')
         ENDIF
         CALL ShowContinueError('Design water flow rate must be left blank when nominal tower capacity input method is used.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%HighSpeedTowerUA .NE. 0.0d0) THEN
         IF (SimpleTower(TowerNum)%HighSpeedTowerUA > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Nominal capacity input method and tower UA at high fan speed have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                '". Nominal capacity input method has been specified and tower UA at high fan speed is being autosized.')
         ENDIF
         CALL ShowContinueError('Tower UA at high fan speed must be left blank when nominal tower capacity input method'// &
                                ' is used.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%LowSpeedTowerUA .NE. 0.0d0) THEN
         IF (SimpleTower(TowerNum)%LowSpeedTowerUA > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Nominal capacity input method and tower UA at low fan speed have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                '". Nominal capacity input method has been specified and tower UA at low fan speed is being autosized.')
         ENDIF
         CALL ShowContinueError('Tower UA at low fan speed must be left blank when nominal tower capacity input method'// &
                                ' is used.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%FreeConvTowerUA .NE. 0.0d0) THEN
         IF (SimpleTower(TowerNum)%FreeConvTowerUA > 0.0d0) THEN
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Nominal capacity input method and free convection UA have been specified.')
         ELSE
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Nominal capacity input method has been specified and free convection UA is being autosized.')
         ENDIF
         CALL ShowContinueError('Free convection UA should be left blank when nominal tower capacity input method is used.')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleTower(TowerNum)%TowerLowSpeedNomCap >= SimpleTower(TowerNum)%TowerNominalCapacity) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Low-speed nominal capacity must be less than the high-speed nominal capacity.')
         ErrorsFound=.true.
      END IF
      IF (SimpleTower(TowerNum)%TowerFreeConvNomCap >= SimpleTower(TowerNum)%TowerLowSpeedNomCap) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Free convection nominal capacity must be less than the low-speed nominal capacity.')
         ErrorsFound=.true.
      END IF
      IF (SimpleTower(TowerNum)%TowerFreeConvNomCap > 0.0d0 .and. SimpleTower(TowerNum)%FreeConvAirFlowRate == 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                  '". Free convection air flow must be greater than zero when tower free convection capacity is specified.')
         ErrorsFound=.true.
      END IF
    ELSE ! Tower performance input method is not specified as a valid "choice"
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                  '". Tower Performance Input Method must be "UFactorTimesAreaAndDesignWaterFlowRate" or "NominalCapacity".')
      CALL ShowContinueError('Tower Performanace Input Method currently specified as: '// &
                            TRIM(AlphArray(4)))
         ErrorsFound=.true.
    ENDIF
  END DO  ! End Two-Speed Tower Loop

  cCurrentModuleObject = cCoolingTower_VariableSpeed
  DO VariableSpeedTowerNumber = 1 , NumVariableSpeedTowers
    TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + VariableSpeedTowerNumber
    CALL GetObjectItem(cCurrentModuleObject,VariableSpeedTowerNumber,AlphArray,NumAlphas, &
                       NumArray,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),SimpleTower%Name,TowerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    SimpleTower(TowerNum)%VSTower            = VariableSpeedTowerNumber
    SimpleTower(TowerNum)%Name               = AlphArray(1)
    SimpleTower(TowerNum)%TowerType          = TRIM(cCurrentModuleObject)
    SimpleTower(TowerNum)%TowerType_Num      = CoolingTower_VariableSpeed
    SimpleTower(TowerNum)%WaterInletNodeNum  = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    SimpleTower(TowerNum)%WaterOutletNodeNum = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Chilled Water Nodes')

    IF((SameString(AlphArray(4),'CoolToolsUserDefined') .OR. SameString(AlphArray(4),'YorkCalcUserDefined')) .AND. &
        lAlphaFieldBlanks(5) )THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" a '//TRIM(cAlphaFieldNames(5))//' must be specified when '//TRIM(cAlphaFieldNames(4))// &
                       ' is specified as CoolToolsUserDefined or YorkCalcUserDefined')
      ErrorsFound = .TRUE.
    ELSEIF((SameString(AlphArray(4),'CoolToolsCrossFlow') .OR. &
            SameString(AlphArray(4),'YorkCalc')) .AND. .NOT. lAlphaFieldBlanks(5) )THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" a Tower Model Coefficient Name is specified and the Tower Model Type'// &
                       ' is not specified as CoolToolsUserDefined or YorkCalcUserDefined. The '// &
                       'CoolingTowerPerformance:CoolTools (orCoolingTowerPerformance:YorkCalc) data object will not be used.')
    ELSE
      SimpleTower(TowerNum)%ModelCoeffObjectName = AlphArray(5)
    END IF

    IF(.NOT. lAlphaFieldBlanks(6) )THEN
     SimpleTower(TowerNum)%FanPowerfAirFlowCurve = GetCurveIndex(AlphArray(6))
     IF(SimpleTower(TowerNum)%FanPowerfAirFlowCurve .EQ. 0)THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" the Fan Power Ratio as a function of Air Flow Rate Ratio Curve Name specified as '//TRIM(AlphArray(6)) &
                       //' was not found. Fan Power as a function of Air Flow Rate Ratio will default to Fan Power = (Air'// &
                       ' Flow Rate Ratio)^3 and the simulation continues.')
     END IF
    END IF

    ALLOCATE (VSTower(VariableSpeedTowerNumber)%Coeff(35))
    VSTower(VariableSpeedTowerNumber)%Coeff = 0.0d0

    IF(SameString(AlphArray(4),'CoolToolsCrossFlow'))THEN
      SimpleTower(TowerNum)%TowerModelType               = CoolToolsXFModel
!     set cross-flow model coefficients
!       Outputs approach in F
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  = -2.1985908408527
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  = -24.3108065555106
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  = 21.9333667825398
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  = -4.94979078884808
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  = 14.6788552214526
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  = -15.4612468065777
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  = 2.83753688605444
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  = 10.0023162199558
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  = 2.70780345372045
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) = -5.91993527180418
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) = 0.194222288920726
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) = 0.142543400927955
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) = -0.0818947291400898
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) = -0.169584760441541
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) = 0.0186741309635284
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) = 0.0536824177590012
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) = -0.00375848174056975
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) = 0.000623763881051551
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) = -0.000709769430542879
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) = 0.0000234697776728891
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) = 2.45541543720225
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) = -0.607566456611435
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) = 0.117339576910507
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) = 1.64648551160799
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) = -0.135898905926974
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) = -0.152577581866506
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) = -0.034055419164321
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(28) = 0.00274052705314173
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(29) = -0.00442366885652332
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(30) = 0.0000687098236486247
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(31) = -0.0416435261408276
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(32) = 0.00263481599534274
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(33) = -0.010325259545311
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(34) = 0.000356999078067433
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(35) = 0.000249188476685273

!       Outputs approach in C
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  =   0.52049709836241d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  = -10.617046395344d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  =  10.7292974722538d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  =  -2.74988377158227d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  =   4.73629943913743d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  =  -8.25759700874711d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  =   1.57640938114136d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  =   6.51119643791324d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  =   1.50433525206692d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) =  -3.2888529287801d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) =   0.0257786145353773d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) =   0.182464289315254d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) =  -0.0818947291400898d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) =  -0.215010003996285d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) =   0.0186741309635284d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) =   0.0536824177590012d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) =  -0.00270968955115031d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) =   0.00112277498589279d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) =  -0.00127758497497718d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) =   0.0000760420796601607d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) =   1.43600088336017d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) =  -0.5198695909109d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) =   0.117339576910507d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) =   1.50492810819924d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) =  -0.135898905926974d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) =  -0.152577581866506d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) =  -0.0533843828114562d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(28) =   0.00493294869565511d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(29) =  -0.00796260394174197d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(30) =   0.000222619828621544d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(31) =  -0.0543952001568055d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(32) =   0.00474266879161693d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(33) =  -0.0185854671815598d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(34) =   0.00115667701293848d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(35) =   0.000807370664460284d0

!       set minimum and maximum boundaries for CoolTools crossflow model input variables
        VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp = -1.0d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp = 26.6667d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp      = 1.1111d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp      = 11.1111d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp   = 1.1111d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp   = 11.1111d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio = 0.75d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio = 1.25d0

!    CoolTools counterflow model does not work properly. The empirical model seems flawed since the tower
!    operates in the free convection regime on the design day.
!    ELSEIF(SameString(AlphArray(5),'COOLTOOLS COUNTERFLOW'))THEN
!      SimpleTower(TowerNum)%TowerModelType               = CoolToolsCFModel
!!     set counter-flow model coefficients
!!       Outputs approach in F
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  = -4.48760943345722
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  = 0.741749875850003
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  = 1.74679844252553
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  = -0.397320959632943
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  = 19.5106208955792
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  = -9.79489761472574
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  = 1.96690857354709
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  = -1.40803729637148
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  = 0.633867141219563
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) = -0.517255742412696
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) = 0.0546335532842876
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) = 0.0468060318806566
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) = -0.0244033403339062
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) = -0.267365212754448
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) = 0.0385664546399435
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) = 0.037765628073743
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) = -0.000928698541521428
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) = -0.000122211107650076
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) = 0.000682937021895334
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) = 0.00000679217734960548
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) = 1.47274732178792
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) = -0.869303590626237
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) = 0.149995781695274
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) = 2.4548219494635
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) = -0.161092120908292
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) = -0.0830303891087807
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) = -0.0251101427687245
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(28) = 0.00430042875730149
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(29) = -0.013969370453107
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(30) = 0.000096171182587938
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(31) = -0.0251558254472348
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(32) = 0.0077094706621763
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(33) = -0.0173842428341529
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(34) = 0.000244578460749651
!!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(35) = 0.000123026859143619
!
!!       Outputs approach in C
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  =  -1.92653164860338
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  =   1.17466595655408
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  =   0.536606417689184
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  =  -0.220733866462746
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  =   6.4745897765876
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  =  -4.75598392569308
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  =   1.09272698530394
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  =  -0.110853998895391
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  =   0.352148411788646
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) =  -0.287364301340387
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) =   0.0160624154449042
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) =   0.0389845209910517
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) =  -0.0244033403339062
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) =  -0.223657243353147
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) =   0.0385664546399435
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) =   0.037765628073743
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) =  -0.000497969128726743
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) =  -0.000219979993770137
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) =   0.0012292866394116
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) =   0.0000220066546127218
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) =   0.767702044158785
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) =  -0.731689870392589
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) =   0.149995781695274
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) =   2.00780209496408
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) =  -0.161092120908292
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) =  -0.0830303891087807
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) =  -0.0341193367495736
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(28) =   0.00774077176314268
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(29) =  -0.0251448668155926
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(30) =   0.000311594631584919
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(31) =  -0.0311927664658427
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(32) =   0.0138770471919173
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(33) =  -0.0312916371014752
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(34) =   0.000792434212828869
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(35) =   0.000398607023625325

!!       set minimum and maximum boundaries for CoolTools counterflow model input variables
!        VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp = -1.0
!        VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp = 26.6667
!        VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp      = 1.1111
!        VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp      = 11.1111
!        VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp   = 1.1111
!        VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp   = 11.1111
!        VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio = 0.75
!        VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio = 1.25

    ELSEIF(SameString(AlphArray(4),'YorkCalc'))THEN
      SimpleTower(TowerNum)%TowerModelType               = YorkCalcModel
!     set counter-flow model coefficients
!       Outputs approach in F
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  = 2.471005863
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  = -0.139855144
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  = 0.001325024
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  = 0.768721437
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  = -0.023370562
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  = 0.000149476
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  = -0.01116139
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  = 0.000325406
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  = -0.00000230183
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) = 9.852803844
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) = -0.173673565
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) = 0.000811069
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) = 1.749920395
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) = 0.004930143
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) = -0.00022193
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) = -0.009865402
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) = -0.000283361
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) = 0.00000466261
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) = 0.09746009
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) = -0.011167959
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) = 0.000138903
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) = -0.135414837
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) = 0.001004747
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) = 0.0000119203
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) = -0.002255673
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) = 0.0000192893
!        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) = -0.000000260086

!       Outputs approach in C
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1)  = -0.359741205d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2)  = -0.055053608d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3)  =  0.0023850432d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4)  =  0.173926877d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5)  = -0.0248473764d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6)  =  0.00048430224d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7)  = -0.005589849456d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8)  =  0.0005770079712d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9)  = -0.00001342427256d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) =  2.84765801111111d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) = -0.121765149d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) =  0.0014599242d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) =  1.680428651d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) = -0.0166920786d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) = -0.0007190532d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) = -0.025485194448d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) =  0.0000487491696d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) =  0.00002719234152d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) = -0.0653766255555556d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) = -0.002278167d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) =  0.0002500254d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) = -0.0910565458d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) =  0.00318176316d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) =  0.000038621772d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) = -0.0034285382352d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) =  0.00000856589904d0
        VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) = -0.000001516821552d0

!       set minimum and maximum boundaries for YorkCalc model input variables
        VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp = -34.4d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp = 29.4444d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp      = 1.1111d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp      = 22.2222d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp   = 1.1111d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp   = 40.0d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio = 0.75d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio = 1.25d0
        VSTower(SimpleTower(TowerNum)%VSTower)%MaxLiquidToGasRatio = 8.0d0

    ELSEIF(SameString(AlphArray(4),'CoolToolsUserDefined'))THEN
      SimpleTower(TowerNum)%TowerModelType                         = CoolToolsUserDefined
      ! Nested Get-input routines below.  Should pull out of here and read in beforehand.
      DO VSModelCoeffNum = 1, NumVSCoolToolsModelCoeffs
        CALL GetObjectItem('CoolingTowerPerformance:CoolTools',VSModelCoeffNum, &
                           AlphArray2,NumAlphas2, NumArray2 ,NumNums2,IOSTAT)
        IF(.NOT. SameString(AlphArray2(1),SimpleTower(TowerNum)%ModelCoeffObjectName))CYCLE
        VSTower(SimpleTower(TowerNum)%VSTower)%FoundModelCoeff     = .TRUE.
        ! verify the correct number of coefficients for the CoolTools model
        IF(NumNums2 /= 43)THEN
          CALL ShowSevereError('CoolingTower:VariableSpeed "'//TRIM(SimpleTower(TowerNum)%Name)//&
                 '". The number of numeric inputs for object CoolingTowerPerformance:CoolTools "' &
                         //TRIM(SimpleTower(TowerNum)%ModelCoeffObjectName)//'" must equal 43.')
          ErrorsFound=.true.
        ELSE

          VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp = NumArray2(1)
          VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp = NumArray2(2)
          VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp      = NumArray2(3)
          VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp      = NumArray2(4)
          VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp   = NumArray2(5)
          VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp   = NumArray2(6)
          VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio = NumArray2(7)
          VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio = NumArray2(8)

          DO CoeffNum = 9, NumNums2
            VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(CoeffNum-8)= NumArray2(CoeffNum)
          END DO
        END IF
        EXIT
      END DO
      IF(.NOT. VSTower(SimpleTower(TowerNum)%VSTower)%FoundModelCoeff)THEN
        CALL ShowSevereError('CoolingTower:VariableSpeed "'//TRIM(SimpleTower(TowerNum)%Name)//&
                      '". User defined name for variable speed cooling tower model coefficients object not found = ' &
                       //TRIM(SimpleTower(TowerNum)%ModelCoeffObjectName))
        ErrorsFound=.true.
      END IF
    ELSEIF(SameString(AlphArray(4),'YorkCalcUserDefined'))THEN
      SimpleTower(TowerNum)%TowerModelType                         = YorkCalcUserDefined
      ! Nested Get-input routines below.  Should pull out of here and read in beforehand.
      DO VSModelCoeffNum = 1, NumVSYorkCalcModelCoeffs
        CALL GetObjectItem('CoolingTowerPerformance:YorkCalc',VSModelCoeffNum, &
                     AlphArray2,NumAlphas2, NumArray2 ,NumNums2,IOSTAT)
        IF(.NOT. SameString(AlphArray2(1),SimpleTower(TowerNum)%ModelCoeffObjectName))CYCLE
        VSTower(SimpleTower(TowerNum)%VSTower)%FoundModelCoeff     = .TRUE.
        ! verify the correct number of coefficients for the YorkCalc model
        IF(NumNums2 /= 36)THEN
          CALL ShowSevereError('CoolingTower:VariableSpeed "'//TRIM(SimpleTower(TowerNum)%Name)//&
                  '". The number of numeric inputs for object CoolingTowerPerformance:YorkCalc "' &
                     //TRIM(SimpleTower(TowerNum)%ModelCoeffObjectName)//'" must equal 36.')
          ErrorsFound=.true.
        ELSE

          VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp   = NumArray2(1)
          VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp   = NumArray2(2)
          VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp        = NumArray2(3)
          VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp        = NumArray2(4)
          VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp     = NumArray2(5)
          VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp     = NumArray2(6)
          VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio   = NumArray2(7)
          VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio   = NumArray2(8)
          VSTower(SimpleTower(TowerNum)%VSTower)%MaxLiquidToGasRatio = NumArray2(9)

          DO CoeffNum = 10, NumNums2
            VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(CoeffNum-9)= NumArray2(CoeffNum)
          END DO
        END IF
        EXIT
      END DO

      IF(.NOT. VSTower(SimpleTower(TowerNum)%VSTower)%FoundModelCoeff)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                      '". User defined name for variable speed cooling tower model coefficients object not found = ' &
                       //TRIM(SimpleTower(TowerNum)%ModelCoeffObjectName))
        ErrorsFound=.true.
      END IF
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                    '". Illegal Tower Model Type = '//TRIM(AlphArray(5)))
      CALL ShowContinueError(' Tower Model Type must be "CoolToolsCrossFlow", "YorkCalc",'// &
                    ' "CoolToolsUserDefined", or "YorkCalcUserDefined.')
      ErrorsFound=.true.
    END IF

    SimpleTower(TowerNum)%TowerMassFlowRateMultiplier = VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio

!   check user defined minimums to be greater than 0
    IF(VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp .LT. 0.0d0)THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                   '". User defined minimum approach temperature must be > 0')
      ErrorsFound=.true.
    END IF
    IF(VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp .LT. 0.0d0)THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                   '". User defined minimum range temperature must be > 0')
      ErrorsFound=.true.
    END IF
    IF(VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio .LT. 0.0d0)THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                   '". User defined minimum water flow rate ratio must be > 0')
      ErrorsFound=.true.
    END IF

!   check that the user defined maximums are greater than the minimums
    IF(VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp)THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                   '". User defined maximum approach temperature must be > the minimum approach temperature')
      ErrorsFound=.true.
    END IF
    IF(VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp)THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                   '". User defined maximum range temperature must be > the minimum range temperature')
      ErrorsFound=.true.
    END IF
    IF(VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio .LT. &
       VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio)THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                   '". User defined maximum water flow rate ratio must be > the minimum water flow rate ratio')
      ErrorsFound=.true.
    END IF

    SimpleTower(TowerNum)%DesignInletWB             = NumArray(1)
     IF(NumArray(1) .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp .OR. &
        NumArray(1) .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp) THEN
        WRITE(OutputChar,OutputFormat)SimpleTower(TowerNum)%DesignInletWB
        WRITE(OutputCharLo,OutputFormat)VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp
        WRITE(OutputCharHi,OutputFormat)VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" the design inlet air wet-bulb temperature of '//TRIM(OutputChar)//' must be within' &
                       //' the model limits of '//TRIM(OutputCharLo)//' and '//TRIM(OutputCharHi)//' degrees C')
       ErrorsFound = .TRUE.
     END IF

    SimpleTower(TowerNum)%DesignApproach            = NumArray(2)
     IF(NumArray(2) .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp .OR. &
        NumArray(2) .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp) THEN
        WRITE(OutputChar,OutputFormat)SimpleTower(TowerNum)%DesignApproach
        WRITE(OutputCharLo,OutputFormat)VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp
        WRITE(OutputCharHi,OutputFormat)VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" the design approach temperature of '//TRIM(OutputChar)//' must be within ' &
                       //' the model limits of '//TRIM(OutputCharLo)//' and '//TRIM(OutputCharHi)//' degrees C')
       ErrorsFound = .TRUE.
     END IF

    SimpleTower(TowerNum)%DesignRange               = NumArray(3)
     IF(NumArray(3) .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp .OR. &
        NumArray(3) .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp) THEN
        WRITE(OutputChar,OutputFormat)SimpleTower(TowerNum)%DesignRange
        WRITE(OutputCharLo,OutputFormat)VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp
        WRITE(OutputCharHi,OutputFormat)VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" the design range temperature of '//TRIM(OutputChar)//' must be within ' &
                       //' the model limits of '//TRIM(OutputCharLo)//' and '//TRIM(OutputCharHi)//' degrees C')
       ErrorsFound = .TRUE.
     END IF

    SimpleTower(TowerNum)%DesignWaterFlowRate       = NumArray(4)
     IF(NumArray(4) .LE. 0.0d0 .AND. NumArray(4) .NE. autosize) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" design water flow rate must be > 0')
       ErrorsFound = .TRUE.
     END IF

    SimpleTower(TowerNum)%HighSpeedAirFlowRate      = NumArray(5)
     IF(NumArray(5) .LE. 0.0d0 .AND. NumArray(5) .NE. autosize) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" design air flow rate must be > 0')
       ErrorsFound = .TRUE.
     END IF

    SimpleTower(TowerNum)%HighSpeedFanPower         = NumArray(6)
     IF(NumArray(6) .LE. 0.0d0 .AND. NumArray(6) .NE. autosize) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" design fan power must be > 0')
       ErrorsFound = .TRUE.
     END IF

!   minimum air flow rate fraction must be >= 0.2 and <= 0.5, below this value the tower fan cycles to maintain the setpoint
    SimpleTower(TowerNum)%MinimumVSAirFlowFrac      = NumArray(7)
    SimpleTower(TowerNum)%MinimumVSAirFlowFrac      = NumArray(7)
     IF(NumArray(7) .LT. 0.2d0 .OR. NumArray(7) .GT. 0.5d0) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" minimum VS air flow rate ratio must be >= 0.2 and <= 0.5')
       ErrorsFound = .TRUE.
     END IF

!   fraction of tower capacity in free convection regime must be >= to 0 and <= 0.2
    SimpleTower(TowerNum)%FreeConvectionCapacityFraction = NumArray(8)
     IF(NumArray(8) .LT. 0.0d0 .OR. NumArray(8) .GT. 0.2d0) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" fraction of tower capacity in free convection regime must be >= 0 and <= 0.2')
       ErrorsFound = .TRUE.
     END IF

!   Basin heater power as a function of temperature must be greater than or equal to 0
    SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff = NumArray(9)
    IF(NumArray(9) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                      '" basin heater power as a function of temperature difference must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    SimpleTower(TowerNum)%BasinHeaterSetPointTemp = NumArray(10)
    IF(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 10) THEN
        SimpleTower(TowerNum)%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(simpleTower(TowerNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(SimpleTower(TowerNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(10))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    ! Performance Input Method for Variable Speed Towers is assigned to be UA AND DESIGN WATER FLOW RATE
    ! for autosizing calculations (see SizeTower)
    SimpleTower(TowerNum)%PerformanceInputMethod_Num = PIM_UFactor

!   Makeup water drift percentage must be greater than or equal to 0
!    SimpleTower(TowerNum)%MakeupWaterDrift          = NumArray(10)/100.0
!     IF(NumArray(10) .LT. 0.0) THEN
!       CALL ShowSevereError('COOLING TOWER:VARIABLE SPEED, "'//TRIM(SimpleTower(TowerNum)%Name)//&
!                       '" Makeup Water Drift as a percentage of design water flow rate must be >= 0')
!       ErrorsFound = .TRUE.
!     END IF


    IF(AlphArray(7) .NE. Blank)THEN
      SimpleTower(TowerNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(AlphArray(7))
      IF(SimpleTower(TowerNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" basin heater schedule name "'//TRIM(AlphArray(7)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF

!    IF(AlphArray(9) .NE. ' ')THEN
!      SimpleTower(TowerNum)%BlowDownSchedulePtr       = GetScheduleIndex(AlphArray(9))
!      IF(SimpleTower(TowerNum)%BlowDownSchedulePtr .EQ. 0)THEN
!        CALL ShowWarningError('COOLING TOWER:VARIABLE SPEED, "'//TRIM(SimpleTower(TowerNum)%Name)//&
!                       '" blowdown schedule name "'//TRIM(AlphArray(9)) &
!                       //'" was not found. Basin blowdown will not be modeled and the simulation continues')
!      END IF
!    END IF

    ! begin water use and systems get input
    IF (SameString(AlphArray(8),'LossFactor')) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByUserFactor
    ELSEIF (SameString(AlphArray(8), 'SaturatedExit')) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSEIF (lAlphaFieldBlanks(8)) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(AlphArray(8)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      errorsfound = .true.
    ENDIF

    SimpleTower(TowerNum)%UserEvapLossFactor = NumArray(11) !  N11 , \field Evaporation Loss Factor
    SimpleTower(TowerNum)%DriftLossFraction = NumArray(12) / 100.0d0 !  N12, \field Drift Loss Percent
    SimpleTower(TowerNum)%ConcentrationRatio = NumArray(13) !  N13, \field Blowdown Concentration Ratio
    SimpleTower(TowerNum)%SizFac             = NumArray(17) !  N14  \field Sizing Factor
    IF (SimpleTower(TowerNum)%SizFac <= 0.0d0) SimpleTower(TowerNum)%SizFac = 1.0d0

    If (SameString(AlphArray(9), 'ScheduledRate')) then
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownBySchedule
    ELSEIF (SameString(AlphArray(9), 'ConcentrationRatio')) THEN
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownByConcentration
    ELSEIF (lAlphaFieldBlanks(9)) THEN
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownByConcentration
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(AlphArray(9)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      errorsfound = .true.
    ENDIF
    SimpleTower(TowerNum)%SchedIDBlowdown = GetScheduleIndex(AlphArray(10))
    If ((SimpleTower(TowerNum)%SchedIDBlowdown == 0) .and. (SimpleTower(TowerNum)%BlowdownMode == BlowdownBySchedule)) Then
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(AlphArray(10)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      errorsfound = .true.
    endif

  !added for multi-cell
    SimpleTower(TowerNum)%NumCell                  = NumArray(14)
    If ((NumNums < 14) .and. (SimpleTower(TowerNum)%NumCell == 0) ) Then
      ! assume Number of Cells not entered and should be defaulted
      SimpleTower(TowerNum)%NumCell = 1
    endif
    SimpleTower(TowerNum)%MinFracFlowRate          = NumArray(15)
    If ((NumNums < 15) .and. (SimpleTower(TowerNum)%MinFracFlowRate == 0.0d0) ) Then
      ! assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
      SimpleTower(TowerNum)%MinFracFlowRate = 0.33d0
    endif
    SimpleTower(TowerNum)%MaxFracFlowRate              = NumArray(16)
    If ((NumNums < 16) .and. (SimpleTower(TowerNum)%MaxFracFlowRate == 0.0d0) ) Then
      ! assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
      SimpleTower(TowerNum)%MaxFracFlowRate = 2.5d0
    endif

    IF (NumAlphas >= 13) THEN
      IF (lAlphaFieldBlanks(13).or.AlphArray(13) == Blank) THEN
        SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
        SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
      ELSE
        IF (SameString(AlphArray(13),'MinimalCell') .OR. &
            SameString(AlphArray(13),'MaximalCell') ) THEN
          IF (SameString(AlphArray(13),'MinimalCell')) THEN
             SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MinCell
             SimpleTower(TowerNum)%CellCtrl= 'MinimalCell'
          ENDIF
          IF (SameString(AlphArray(13),'MaximalCell')) THEN
             SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
             SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
          ENDIF
        ELSE
          CALL ShowSevereError('Illegal '//TRIM(cAlphaFieldNames(13))//' = '//TRIM(AlphArray(13)))
          CALL ShowContinueError('Occurs in '//SimpleTower(TowerNum)%TowerType//'='//TRIM(SimpleTower(TowerNum)%Name))
          ErrorsFound=.TRUE.
        END IF
      END IF
    ELSE
      !assume Cell Control not entered and should be defaulted
      SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
      SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
    END IF

    IF (lAlphaFieldBlanks(11) ) THEN
      SimpleTower(TowerNum)%SuppliedByWaterSystem = .false.
    ELSE ! water from storage tank
      !
      Call SetupTankDemandComponent(AlphArray(1), TRIM(cCurrentModuleObject), AlphArray(11), ErrorsFound, &
                              SimpleTower(TowerNum)%WaterTankID, SimpleTower(TowerNum)%WaterTankDemandARRID)
      SimpleTower(TowerNum)%SuppliedByWaterSystem = .TRUE.
    ENDIF

!   outdoor air inlet node
    IF (lAlphaFieldBlanks(12) ) THEN
      SimpleTower(TowerNum)%OutdoorAirInletNodeNum = 0
    ELSE
      SimpleTower(TowerNum)%OutdoorAirInletNodeNum = &
        GetOnlySingleNode(AlphArray(12),ErrorsFound,TRIM(cCurrentModuleObject),SimpleTower(TowerNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
      IF(.not. CheckOutAirNodeNumber(SimpleTower(TowerNum)%OutdoorAirInletNodeNum))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                          '" Outdoor Air Inlet Node Name not valid Outdoor Air Node= '//TRIM(AlphArray(12)))
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound=.true.
      END IF
    ENDIF

  END DO  ! End Variable-Speed Tower Loop

  cCurrentModuleObject = cCoolingTower_VariableSpeedMerkel
  DO MerkelVSTowerNum= 1, NumVSMerkelTowers
    TowerNum = NumSingleSpeedTowers + NumTwoSpeedTowers + NumVariableSpeedTowers + MerkelVSTowerNum
    CALL GetObjectItem(cCurrentModuleObject,MerkelVSTowerNum,AlphArray,NumAlphas, &
                       NumArray,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks,&
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),SimpleTower%Name,TowerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    SimpleTower(TowerNum)%Name               = AlphArray(1)
    SimpleTower(TowerNum)%TowerType          = TRIM(cCurrentModuleObject)
    SimpleTower(TowerNum)%TowerType_Num      = CoolingTower_VariableSpeedMerkel
    SimpleTower(TowerNum)%WaterInletNodeNum  = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    SimpleTower(TowerNum)%WaterOutletNodeNum = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Chilled Water Nodes')

    IF (SameString(AlphArray(4),'UFactorTimesAreaAndDesignWaterFlowRate')) THEN
      SimpleTower(TowerNum)%PerformanceInputMethod_Num = PIM_UFactor
    ELSEIF (SameString(AlphArray(4),'NominalCapacity')) THEN
      SimpleTower(TowerNum)%PerformanceInputMethod_Num = PIM_NominalCapacity
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid, '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(AlphArray(4)))
      errorsfound = .true.
    ENDIF

    SimpleTower(TowerNum)%FanPowerfAirFlowCurve = GetCurveIndex(AlphArray(5))
    IF (SimpleTower(TowerNum)%FanPowerfAirFlowCurve == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(AlphArray(5)))
      CALL ShowContinueError('Curve name not found.')
      errorsfound = .true.
    ENDIF

    SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio  = NumArray(1)
    SimpleTower(TowerNum)%TowerNominalCapacity            = NumArray(2)
    SimpleTower(TowerNum)%TowerFreeConvNomCap             = NumArray(3)
    SimpleTower(TowerNum)%TowerFreeConvNomCapSizingFactor = NumArray(4)
    SimpleTower(TowerNum)%DesignWaterFlowRate             = NumArray(5)
    SimpleTower(TowerNum)%DesignWaterFlowPerUnitNomCap    = NumArray(6)
    SimpleTower(TowerNum)%HighSpeedAirFlowRate            = NumArray(7)
    IF (lNumericFieldBlanks(8)) THEN
      SimpleTower(TowerNum)%DefaultedDesignAirFlowScalingFactor = .TRUE.
    ELSE
      SimpleTower(TowerNum)%DefaultedDesignAirFlowScalingFactor = .FALSE.
    ENDIF
    SimpleTower(TowerNum)%DesignAirFlowPerUnitNomCap      = NumArray(8)
    SimpleTower(TowerNum)%MinimumVSAirFlowFrac            = NumArray(9)
    SimpleTower(TowerNum)%HighSpeedFanPower               = NumArray(10)
    SimpleTower(TowerNum)%DesignFanPowerPerUnitNomCap     = NumArray(11)
    SimpleTower(TowerNum)%FreeConvAirFlowRate             = NumArray(12)
    SimpleTower(TowerNum)%FreeConvAirFlowRateSizingFactor = NumArray(13)
    SimpleTower(TowerNum)%HighSpeedTowerUA                = NumArray(14)
    SimpleTower(TowerNum)%FreeConvTowerUA                 = NumArray(15)
    SimpleTower(TowerNum)%FreeConvTowerUASizingFactor     = NumArray(16)

    SimpleTower(TowerNum)%UAModFuncAirFlowRatioCurvePtr = GetCurveIndex(AlphArray(6))
    IF (SimpleTower(TowerNum)%UAModFuncAirFlowRatioCurvePtr == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(AlphArray(6)))
      CALL ShowContinueError('Curve name not found.')
      errorsfound = .true.
    ENDIF

    SimpleTower(TowerNum)%UAModFuncWetbulbDiffCurvePtr = GetCurveIndex(AlphArray(7))
    IF (SimpleTower(TowerNum)%UAModFuncWetbulbDiffCurvePtr == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(AlphArray(7)))
      CALL ShowContinueError('Curve name not found.')
      errorsfound = .true.
    ENDIF

    SimpleTower(TowerNum)%UAModFuncWaterFlowRatioCurvePtr = GetCurveIndex(AlphArray(8))
    IF (SimpleTower(TowerNum)%UAModFuncWaterFlowRatioCurvePtr == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(AlphArray(8)))
      CALL ShowContinueError('Curve name not found.')
      errorsfound = .true.
    ENDIF

    !   Basin heater power as a function of temperature must be greater than or equal to 0
    SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff = NumArray(17)
    IF(NumArray(17) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                      '" basin heater power as a function of temperature difference must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    SimpleTower(TowerNum)%BasinHeaterSetPointTemp = NumArray(18)
    IF(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 18) THEN
        SimpleTower(TowerNum)%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(simpleTower(TowerNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(SimpleTower(TowerNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(18))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(AlphArray(9) .NE. Blank)THEN
      SimpleTower(TowerNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(AlphArray(9))
      IF(SimpleTower(TowerNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                       '" basin heater schedule name "'//TRIM(AlphArray(9)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF

    ! begin water use and systems get input
    IF (SameString(AlphArray(10),'LossFactor')) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByUserFactor
    ELSEIF (SameString(AlphArray(10), 'SaturatedExit')) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSEIF (lAlphaFieldBlanks(10)) THEN
       SimpleTower(TowerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(AlphArray(10)))
      errorsfound = .true.
    ENDIF

    SimpleTower(TowerNum)%UserEvapLossFactor = NumArray(19) !  N19 , \field Evaporation Loss Factor
    SimpleTower(TowerNum)%DriftLossFraction = NumArray(20) / 100.0d0  !  N20, \field Drift Loss Percent
    If ((NumNums < 20) .and. (SimpleTower(TowerNum)%DriftLossFraction == 0.0d0) ) THEN
      ! assume Drift loss not entered and should be defaulted
      SimpleTower(TowerNum)%DriftLossFraction = 0.008d0 /100.0d0
    ENDIF

    SimpleTower(TowerNum)%ConcentrationRatio = NumArray(21) !  N21, \field Blowdown Concentration Ratio
    SimpleTower(TowerNum)%SizFac = NumArray(25)             !  N25  \field Sizing Factor
    IF (SimpleTower(TowerNum)%SizFac <= 0.0d0) SimpleTower(TowerNum)%SizFac = 1.0d0

    If (SameString(AlphArray(11), 'ScheduledRate')) then
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownBySchedule
    ELSEIF (SameString(AlphArray(11), 'ConcentrationRatio')) THEN
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownByConcentration
    ELSEIF (lAlphaFieldBlanks(11)) THEN
      SimpleTower(TowerNum)%BlowdownMode  = BlowdownByConcentration
      If ((NumNums < 21) .and.(SimpleTower(TowerNum)%ConcentrationRatio == 0.0d0) ) THEN
        ! assume Concetration ratio was omitted and should be defaulted
            SimpleTower(TowerNum)%ConcentrationRatio = 3.0d0
      endif
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(11))//'='//TRIM(AlphArray(11)))
      errorsfound = .true.
    ENDIF
    SimpleTower(TowerNum)%SchedIDBlowdown = GetScheduleIndex(AlphArray(12))
    If ((SimpleTower(TowerNum)%SchedIDBlowdown == 0) .AND. (SimpleTower(TowerNum)%BlowdownMode == BlowdownBySchedule)) Then
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(AlphArray(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(12))//'='//TRIM(AlphArray(12)))
      errorsfound = .true.
    ENDIF

  !added for multi-cell
    SimpleTower(TowerNum)%NumCell                  = NumArray(22)
    If ((NumNums < 22) .and. (SimpleTower(TowerNum)%NumCell == 0) ) Then
      ! assume Number of Cells not entered and should be defaulted
      SimpleTower(TowerNum)%NumCell = 1
    endif
    SimpleTower(TowerNum)%MinFracFlowRate          = NumArray(23)
    If ((NumNums < 23) .and. (SimpleTower(TowerNum)%MinFracFlowRate == 0.0d0) ) Then
      ! assume Cell Minimum Water Flow Rate Fraction not entered and should be defaulted
      SimpleTower(TowerNum)%MinFracFlowRate = 0.33d0
    endif
    SimpleTower(TowerNum)%MaxFracFlowRate              = NumArray(24)
    If ((NumNums < 24) .and. (SimpleTower(TowerNum)%MaxFracFlowRate == 0.0d0) ) Then
      ! assume Cell Maximum Water Flow Rate Fraction not entered and should be defaulted
      SimpleTower(TowerNum)%MaxFracFlowRate = 2.5d0
    endif
    SimpleTower(TowerNum)%TowerMassFlowRateMultiplier = SimpleTower(TowerNum)%MaxFracFlowRate
    IF (NumAlphas >= 15) THEN
      IF (lAlphaFieldBlanks(15).or.AlphArray(15) == Blank) THEN
        SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
        SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
      ELSE
        IF (SameString(AlphArray(15),'MinimalCell') .OR. &
            SameString(AlphArray(15),'MaximalCell') ) THEN
          IF (SameString(AlphArray(15),'MinimalCell')) THEN
             SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MinCell
             SimpleTower(TowerNum)%CellCtrl= 'MinimalCell'
          ENDIF
          IF (SameString(AlphArray(15),'MaximalCell')) THEN
             SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
             SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
          ENDIF
        ELSE
          CALL ShowSevereError('Illegal '//TRIM(cAlphaFieldNames(15))//' = '//TRIM(AlphArray(15)))
          CALL ShowContinueError('Occurs in '//SimpleTower(TowerNum)%TowerType//'='//TRIM(SimpleTower(TowerNum)%Name))
          ErrorsFound=.TRUE.
        END IF
      END IF
    ELSE
      !assume Cell Control not entered and should be defaulted
      SimpleTower(TowerNum)%CellCtrl= 'MaximalCell'
      SimpleTower(TowerNum)%CellCtrl_Num=CellCtrl_MaxCell
    END IF

    IF (lAlphaFieldBlanks(13)) THEN
      SimpleTower(TowerNum)%SuppliedByWaterSystem = .false.
    ELSE ! water from storage tank
      !
      Call SetupTankDemandComponent(AlphArray(1), TRIM(cCurrentModuleObject), AlphArray(13), ErrorsFound, &
                              SimpleTower(TowerNum)%WaterTankID, SimpleTower(TowerNum)%WaterTankDemandARRID)
      SimpleTower(TowerNum)%SuppliedByWaterSystem = .TRUE.
    ENDIF

!   outdoor air inlet node
    IF (lAlphaFieldBlanks(14)) THEN
      SimpleTower(TowerNum)%OutdoorAirInletNodeNum = 0
    ELSE
      SimpleTower(TowerNum)%OutdoorAirInletNodeNum = &
       GetOnlySingleNode(AlphArray(14),ErrorsFound,TRIM(cCurrentModuleObject),SimpleTower(TowerNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
      IF(.not. CheckOutAirNodeNumber(SimpleTower(TowerNum)%OutdoorAirInletNodeNum))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleTower(TowerNum)%Name)//&
                          '" Outdoor Air Inlet Node Name not valid Outdoor Air Node= '//TRIM(AlphArray(14)))
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound=.true.
      END IF
    ENDIF



  ENDDO ! end merkel vs tower loop

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting cooling tower input.')
  ENDIF

! Set up output variables CurrentModuleObject='CoolingTower:SingleSpeed'
  DO TowerNum = 1, NumSingleSpeedTowers
    CALL SetupOutputVariable('Cooling Tower Inlet Temperature [C]', &
          SimpleTowerReport(TowerNum)%InletWaterTemp,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Outlet Temperature [C]', &
          SimpleTowerReport(TowerNum)%OutletWaterTemp,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Mass Flow Rate [kg/s]', &
          SimpleTowerReport(TowerNum)%WaterMassFlowRate,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Heat Transfer Rate [W]', &
          SimpleTowerReport(TowerNum)%Qactual,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Power [W]', &
          SimpleTowerReport(TowerNum)%FanPower,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Energy [J]', &
          SimpleTowerReport(TowerNum)%FanEnergy,'System','Sum',SimpleTower(TowerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
                       ! Added for fluid bypass
    CALL SetupOutputVariable('Cooling Tower Bypass Fraction []', &
          SimpleTowerReport(TowerNum)%BypassFraction,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Operating Cells Count []',&
          SimpleTowerReport(TowerNum)%NumCellON,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Cycling Ratio []', &
          SimpleTowerReport(TowerNum)%FanCyclingRatio,'System','Average',SimpleTower(TowerNum)%Name)
    IF(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
      CALL SetupOutputVariable('Cooling Tower Basin Heater Electric Power [W]', &
          SimpleTowerReport(TowerNum)%BasinHeaterPower,'System','Average',SimpleTower(TowerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Basin Heater Electric Energy [J]', &
          SimpleTowerReport(TowerNum)%BasinHeaterConsumption,'System','Sum',SimpleTower(TowerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
    END IF
  END DO

  ! CurrentModuleObject='CoolingTower:TwoSpeed'
  DO TowerNum = NumSingleSpeedTowers+1, NumSingleSpeedTowers+NumTwoSpeedTowers
    CALL SetupOutputVariable('Cooling Tower Inlet Temperature [C]', &
          SimpleTowerReport(TowerNum)%InletWaterTemp,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Outlet Temperature [C]', &
          SimpleTowerReport(TowerNum)%OutletWaterTemp,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Mass Flow Rate [kg/s]', &
          SimpleTowerReport(TowerNum)%WaterMassFlowRate,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Heat Transfer Rate [W]', &
          SimpleTowerReport(TowerNum)%Qactual,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Power [W]', &
          SimpleTowerReport(TowerNum)%FanPower,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Energy [J]', &
          SimpleTowerReport(TowerNum)%FanEnergy,'System','Sum',SimpleTower(TowerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
    CALL SetupOutputVariable('Cooling Tower Fan Cycling Ratio []', &
          SimpleTowerReport(TowerNum)%FanCyclingRatio,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Speed Level []',&
          SimpleTowerReport(TowerNum)%SpeedSelected,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Operating Cells Count []',&
          SimpleTowerReport(TowerNum)%NumCellON,'System','Average',SimpleTower(TowerNum)%Name)
    IF(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
      CALL SetupOutputVariable('Cooling Tower Basin Heater Electric Power [W]', &
          SimpleTowerReport(TowerNum)%BasinHeaterPower,'System','Average',SimpleTower(TowerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Basin Heater Electric Energy [J]', &
          SimpleTowerReport(TowerNum)%BasinHeaterConsumption,'System','Sum',SimpleTower(TowerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
    END IF
  END DO

  ! CurrentModuleObject='CoolingTower:VariableSpeed'
  DO TowerNum = NumSingleSpeedTowers+NumTwoSpeedTowers+1, NumSingleSpeedTowers+NumTwoSpeedTowers+NumVariableSpeedTowers
    CALL SetupOutputVariable('Cooling Tower Inlet Temperature [C]', &
          SimpleTowerReport(TowerNum)%InletWaterTemp,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Outlet Temperature [C]', &
          SimpleTowerReport(TowerNum)%OutletWaterTemp,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Mass Flow Rate [kg/s]', &
          SimpleTowerReport(TowerNum)%WaterMassFlowRate,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Heat Transfer Rate [W]', &
          SimpleTowerReport(TowerNum)%Qactual,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Power [W]', &
          SimpleTowerReport(TowerNum)%FanPower,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Energy [J]', &
          SimpleTowerReport(TowerNum)%FanEnergy,'System','Sum',SimpleTower(TowerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
    CALL SetupOutputVariable('Cooling Tower Air Flow Rate Ratio []', &
          SimpleTowerReport(TowerNum)%AirFlowRatio,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Part Load Ratio []', &
          SimpleTowerReport(TowerNum)%FanCyclingRatio,'System','Average',SimpleTower(TowerNum)%Name)
   CALL SetupOutputVariable('Cooling Tower Operating Cells Count []',&
          SimpleTowerReport(TowerNum)%NumCellON,'System','Average',SimpleTower(TowerNum)%Name)
    IF(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
      CALL SetupOutputVariable('Cooling Tower Basin Heater Electric Power [W]', &
          SimpleTowerReport(TowerNum)%BasinHeaterPower,'System','Average',SimpleTower(TowerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Basin Heater Electric Energy [J]', &
          SimpleTowerReport(TowerNum)%BasinHeaterConsumption,'System','Sum',SimpleTower(TowerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
    END IF

!    CALL SetupOutputVariable('Tower Makeup Water Consumption [m3]', &
!          SimpleTowerReport(TowerNum)%WaterAmountUsed,'System','Sum',SimpleTower(TowerNum)%Name, &
!                                  ResourceTypeKey='Water',EndUseKey='HeatRejection',GroupKey='Plant')

  END DO

  ! CurrentModuleObject='CoolingTower:VariableSpeed:Merkel'
  DO TowerNum = NumSingleSpeedTowers+NumTwoSpeedTowers+NumVariableSpeedTowers+1, &
                  NumSingleSpeedTowers+NumTwoSpeedTowers+NumVariableSpeedTowers+NumVSMerkelTowers
    CALL SetupOutputVariable('Cooling Tower Inlet Temperature [C]', &
          SimpleTowerReport(TowerNum)%InletWaterTemp,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Outlet Temperature [C]', &
          SimpleTowerReport(TowerNum)%OutletWaterTemp,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Mass Flow Rate [kg/s]', &
          SimpleTowerReport(TowerNum)%WaterMassFlowRate,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Heat Transfer Rate [W]', &
          SimpleTowerReport(TowerNum)%Qactual,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Power [W]', &
          SimpleTowerReport(TowerNum)%FanPower,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Energy [J]', &
          SimpleTowerReport(TowerNum)%FanEnergy,'System','Sum',SimpleTower(TowerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
    CALL SetupOutputVariable('Cooling Tower Fan Speed Ratio []', &
          SimpleTowerReport(TowerNum)%AirFlowRatio,'System','Average',SimpleTower(TowerNum)%Name)

    CALL SetupOutputVariable('Cooling Tower Operating Cells Count []',&
          SimpleTowerReport(TowerNum)%NumCellON,'System','Average',SimpleTower(TowerNum)%Name)
    IF(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
      CALL SetupOutputVariable('Cooling Tower Basin Heater Electric Power [W]', &
          SimpleTowerReport(TowerNum)%BasinHeaterPower,'System','Average',SimpleTower(TowerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Basin Heater Electric Energy [J]', &
          SimpleTowerReport(TowerNum)%BasinHeaterConsumption,'System','Sum',SimpleTower(TowerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
    END IF
  ENDDO
  ! setup common water reporting for all types of towers.
  Do TowerNum = 1 , NumSingleSpeedTowers+NumTwoSpeedTowers+NumVariableSpeedTowers+NumVSMerkelTowers
    If (SimpleTower(TowerNum)%SuppliedByWaterSystem) THEN
      CALL SetupOutputVariable('Cooling Tower Make Up Water Volume Flow Rate [m3/s]', &
            SimpleTowerReport(TowerNum)%MakeUpVdot,'System','Average',SimpleTower(TowerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Make Up Water Volume [m3]', &
            SimpleTowerReport(TowerNum)%MakeUpVol,'System','Sum',SimpleTower(TowerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]', &
            SimpleTowerReport(TowerNum)%TankSupplyVdot,'System','Average',SimpleTower(TowerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Storage Tank Water Volume [m3]', &
            SimpleTowerReport(TowerNum)%TankSupplyVol,'System','Sum',SimpleTower(TowerNum)%Name, &
            ResourceTypeKey='Water', EndUseKey='HeatRejection', GroupKey='Plant')
      CALL SetupOutputVariable('Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]', &
            SimpleTowerReport(TowerNum)%StarvedMakeUpVdot,'System','Average',SimpleTower(TowerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Starved Storage Tank Water Volume [m3]', &
            SimpleTowerReport(TowerNum)%StarvedMakeUpVol,'System','Sum',SimpleTower(TowerNum)%Name, &
            ResourceTypeKey='Water', EndUseKey='HeatRejection', GroupKey='Plant')
      CALL SetupOutputVariable('Cooling Tower Make Up Mains Water Volume [m3]', &
            SimpleTowerReport(TowerNum)%StarvedMakeUpVol,'System','Sum',SimpleTower(TowerNum)%Name, &
            ResourceTypeKey='MainsWater', EndUseKey='HeatRejection', GroupKey='Plant')
    ELSE ! tower water from mains and gets metered
      CALL SetupOutputVariable('Cooling Tower Make Up Water Volume Flow Rate [m3/s]', &
            SimpleTowerReport(TowerNum)%MakeUpVdot,'System','Average',SimpleTower(TowerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Make Up Water Volume [m3]', &
            SimpleTowerReport(TowerNum)%MakeUpVol,'System','Sum',SimpleTower(TowerNum)%Name, &
            ResourceTypeKey='Water', EndUseKey='HeatRejection', GroupKey='Plant')
      CALL SetupOutputVariable('Cooling Tower Make Up Mains Water Volume [m3]', &
            SimpleTowerReport(TowerNum)%MakeUpVol,'System','Sum',SimpleTower(TowerNum)%Name, &
            ResourceTypeKey='MainsWater', EndUseKey='HeatRejection', GroupKey='Plant')
    ENDIF

    CALL SetupOutputVariable('Cooling Tower Water Evaporation Volume Flow Rate [m3/s]', &
          SimpleTowerReport(TowerNum)%EvaporationVdot,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Evaporation Volume [m3]', &
          SimpleTowerReport(TowerNum)%EvaporationVol,'System','Sum',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Drift Volume Flow Rate [m3/s]', &
          SimpleTowerReport(TowerNum)%DriftVdot,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Drift Volume [m3]', &
          SimpleTowerReport(TowerNum)%DriftVol,'System','Sum',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Blowdown Volume Flow Rate [m3/s]', &
          SimpleTowerReport(TowerNum)%BlowdownVdot,'System','Average',SimpleTower(TowerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Blowdown Volume [m3]', &
          SimpleTowerReport(TowerNum)%BlowdownVol,'System','Sum',SimpleTower(TowerNum)%Name)
  ENDDO ! loop all towers

RETURN
END SUBROUTINE GetTowerInput
! End of Get Input subroutines for the CondenserLoopTowers Module
!******************************************************************************


! Beginning Initialization Section for the CondenserLoopTowers Module
!******************************************************************************

SUBROUTINE InitSimVars

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    October 1998
          !       MODIFIED         Jan 2001, Richard Raustad
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initialize the simulation variables.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na


          !INITIALIZE MODULE LEVEL VARIABLES

    InletWaterTemp           = 0.0d0    ! CW temperature at tower inlet
    OutletWaterTemp          = 0.0d0    ! CW temperature at tower outlet
    WaterInletNode           = 0      ! Node number at tower inlet
    WaterOutletNode          = 0      ! Node number at tower outlet
    WaterMassFlowRate        = 0.0d0    ! WaterMassFlowRate through tower
   ! TowerMassFlowRateMax     = 0.0    ! Max Hardware Mass Flow Rate
   ! TowerMassFlowRateMin     = 0.0    ! Min Hardware Mass Flow Rate
    !LoopMassFlowRateMaxAvail = 0.0    ! Max Loop Mass Flow Rate available
    !LoopMassFlowRateMinAvail = 0.0    ! Min Loop Mass Flow Rate available
    Qactual                  = 0.0d0    ! Tower heat transfer
    CTFanPower               = 0.0d0    ! Tower fan power used
    AirFlowRateRatio         = 0.0d0    ! Ratio of air flow rate through VS cooling tower to design air flow rate
    BasinHeaterPower         = 0.0d0    ! Basin heater power use (W)
    WaterUsage               = 0.0d0    ! Tower water usage (m3/s)
    FanCyclingRatio          = 0.0d0    ! cycling ratio of tower fan when min fan speed provide to much capacity

RETURN
END SUBROUTINE InitSimVars

SUBROUTINE InitTower(TowerNum, RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2002
          !       MODIFIED       Don Shirey Sept/Oct 2002, F Buhl Oct 2002
          !       RE-ENGINEERED  R. Raustad, Oct 2005, moved Max/MinAvail to Init and allowed more than design
          !                      water flow rate to pass through towers (up to 2.5 and 1.25 times the design flow
          !                      for 1 or 2-speed and variable speed towers, respectively). Flow multiplier for
          !                      VS Tower is defaulted to 1.25 and can be reassigned by user.

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Cooling Tower components and for
          ! final checking of tower inputs (post autosizing)

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: BeginEnvrnFlag
  USE Psychrometrics,  ONLY: PsyTwbFnTdbWPb
  USE InputProcessor,  ONLY: SameString
  USE DataPlant,       ONLY: TypeOf_CoolingTower_SingleSpd, TypeOf_CoolingTower_TwoSpd, &
                             TypeOf_CoolingTower_VarSpd, PlantLoop, ScanPlantLoopsForObject, &
                             PlantSizesOkayToFinalize, PlantSizeNotComplete, TypeOf_CoolingTower_VarSpdMerkel
  USE PlantUtilities,  ONLY: InitComponentNodes, SetComponentFlowRate, RegulateCondenserCompFlowReqOp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: TowerNum         ! Number of the current cooling tower being simulated
  LOGICAL, INTENT (IN) :: RunFlag          ! Indication of

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                           :: ErrorsFound=.false. ! Flag if input data errors are found
  LOGICAL, SAVE                           :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: OneTimeFlagForEachTower
!  LOGICAL                                 :: FatalError
  INTEGER  :: TypeOf_Num
  INTEGER  :: LoopNum
  INTEGER  :: LoopSideNum
  INTEGER  :: BranchIndex
  INTEGER  :: CompIndex
  REAL(r64) :: rho ! local density of fluid

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumSimpleTowers))
    ALLOCATE(OneTimeFlagForEachTower(NumSimpleTowers))

    OneTimeFlagForEachTower = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.

  END IF

  IF (OneTimeFlagForEachTower(TowerNum)) THEN

    IF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_SingleSpeed) THEN
      TypeOf_Num = TypeOf_CoolingTower_SingleSpd
    ELSEIF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_TwoSpeed) THEN
      TypeOf_Num = TypeOf_CoolingTower_TwoSpd
    ELSEIF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_VariableSpeed) THEN
      TypeOf_Num = TypeOf_CoolingTower_VarSpd
    ELSEIF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_VariableSpeedMerkel) THEN
      TypeOf_Num = TypeOf_CoolingTower_VarSpdMerkel
    ENDIF

    ! Locate the tower on the plant loops for later usage
    CALL ScanPlantLoopsForObject(SimpleTower(TowerNum)%Name, &
                                 TypeOf_Num, &
                                 SimpleTower(TowerNum)%LoopNum, &
                                 SimpleTower(TowerNum)%LoopSideNum, &
                                 SimpleTower(TowerNum)%BranchNum, &
                                 SimpleTower(TowerNum)%CompNum,  &
                                 errFlag=ErrorsFound)
    IF (ErrorsFound) THEN
      CALL ShowFatalError('InitTower: Program terminated due to previous condition(s).')
    ENDIF

    ! check if setpoint on outlet node
    IF ((Node(SimpleTower(TowerNum)%WaterOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
        (Node(SimpleTower(TowerNum)%WaterOutletNodeNum)%TempSetPointHi == SensedNodeFlagValue) ) THEN
      SimpleTower(TowerNum)%SetpointIsOnOutlet =  .FALSE.
    ELSE
      SimpleTower(TowerNum)%SetpointIsOnOutlet =  .TRUE.
    ENDIF

    OneTimeFlagForEachTower(TowerNum) = .FALSE.

  END IF

  ! Begin environment initializations
  IF(MyEnvrnFlag(TowerNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize) )Then
    IF (PlantSizeNotComplete) THEN
      SELECT CASE (SimpleTower(TowerNum)%TowerType_Num)
      CASE (CoolingTower_SingleSpeed, CoolingTower_TwoSpeed, CoolingTower_VariableSpeed)
        CALL SizeTower(TowerNum)
      CASE (CoolingTower_VariableSpeedMerkel )
        CALL SizeVSMerkelTower(TowerNum)
      ENDSELECT

    ENDIF
    rho = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                'InitTower')

    SimpleTower(TowerNum)%DesWaterMassFlowRate = SimpleTower(TowerNum)%DesignWaterFlowRate * &
                                                  rho
    SimpleTower(TowerNum)%DesWaterMassFlowRatePerCell = SimpleTower(TowerNum)%DesWaterMassFlowRate &
                                                            / SimpleTower(TowerNum)%NumCell
    CALL InitComponentNodes(0.0D0,  SimpleTower(TowerNum)%DesWaterMassFlowRate , &
                                    SimpleTower(TowerNum)%WaterInletNodeNum,     &
                                    SimpleTower(TowerNum)%WaterOutletNodeNum,    &
                                    SimpleTower(TowerNum)%LoopNum,               &
                                    SimpleTower(TowerNum)%LoopSideNum,           &
                                    SimpleTower(TowerNum)%BranchNum,             &
                                    SimpleTower(TowerNum)%CompNum)

    MyEnvrnFlag(TowerNum) = .false.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(TowerNum)=.true.
  ENDIF

  ! Each time initializations
  WaterInletNode = SimpleTower(TowerNum)%WaterInletNodeNum
  SimpleTowerInlet(TowerNum)%WaterTemp  = Node(WaterInletNode)%Temp

  IF (SimpleTower(TowerNum)%OutdoorAirInletNodeNum /= 0) THEN
    SimpleTowerInlet(TowerNum)%AirTemp    = Node(SimpleTower(TowerNum)%OutdoorAirInletNodeNum)%Temp
    SimpleTowerInlet(TowerNum)%AirHumRat  = Node(SimpleTower(TowerNum)%OutdoorAirInletNodeNum)%HumRat
    SimpleTowerInlet(TowerNum)%AirPress   = Node(SimpleTower(TowerNum)%OutdoorAirInletNodeNum)%Press
!    SimpleTowerInlet(TowerNum)%AirWetBulb = PsyTwbFnTdbWPb(SimpleTowerInlet(TowerNum)%AirTemp, &
!                                            SimpleTowerInlet(TowerNum)%AirHumRat,SimpleTowerInlet(TowerNum)%AirPress)
    SimpleTowerInlet(TowerNum)%AirWetBulb = Node(SimpleTower(TowerNum)%OutdoorAirInletNodeNum)%OutAirWetBulb
  ELSE
    SimpleTowerInlet(TowerNum)%AirTemp    = OutDryBulbTemp
    SimpleTowerInlet(TowerNum)%AirHumRat  = OutHumRat
    SimpleTowerInlet(TowerNum)%AirPress   = OutBaroPress
    SimpleTowerInlet(TowerNum)%AirWetBulb = OutWetBulbTemp
  ENDIF

  LoopNum     = SimpleTower(TowerNum)%LoopNum
  LoopSideNum = SimpleTower(TowerNum)%LoopSideNum
  BranchIndex = SimpleTower(TowerNum)%BranchNum
  CompIndex   = SimpleTower(TowerNum)%CompNum

  WaterMassFlowRate = RegulateCondenserCompFlowReqOp(SimpleTower(TowerNum)%LoopNum,               &
                                                SimpleTower(TowerNum)%LoopSideNum,           &
                                                SimpleTower(TowerNum)%BranchNum,             &
                                                SimpleTower(TowerNum)%CompNum,     &
                                                SimpleTower(TowerNum)%DesWaterMassFlowRate &
                                                * SimpleTower(TowerNum)%TowerMassFlowRateMultiplier)


  CALL SetComponentFlowRate(WaterMassFlowRate, &
                                    SimpleTower(TowerNum)%WaterInletNodeNum,     &
                                    SimpleTower(TowerNum)%WaterOutletNodeNum,    &
                                    SimpleTower(TowerNum)%LoopNum,               &
                                    SimpleTower(TowerNum)%LoopSideNum,           &
                                    SimpleTower(TowerNum)%BranchNum,             &
                                    SimpleTower(TowerNum)%CompNum)


    ! Added for fluid bypass. 8/2008
  SimpleTower(TowerNum)%BypassFraction = 0.0D0
  BasinHeaterPower = 0.0D0

  RETURN

END SUBROUTINE InitTower


SUBROUTINE SizeTower(TowerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2002
          !       MODIFIED       Don Shirey, Sept/Oct 2002; Richard Raustad, Feb 2005
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Cooling Tower Components for which capacities and flow rates
          ! have not been specified in the input. This subroutine also calculates tower UA if the user
          ! has specified tower performance via the "Nominal Capacity" method.

          ! METHODOLOGY EMPLOYED:
          ! Obtains condenser flow rate from the plant sizing array. If tower performance is specified
          ! via the "Nominal Capacity" method, the water flow rate is directly proportional to capacity.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,       ONLY: PlantLoop, PlantSizesOkayToFinalize
  USE DataInterfaces,  ONLY: ShowFatalError, ShowContinueError
  USE General,         ONLY: SolveRegulaFalsi
  USE PlantUtilities,  ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE InputProcessor,  ONLY: SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TowerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'
  CHARACTER(len=*), PARAMETER :: OutputFormat2 ='(F9.6)'
  INTEGER, PARAMETER          :: MaxIte = 500       ! Maximum number of iterations
  REAL(r64), PARAMETER :: Acc =  0.0001d0    ! Accuracy of result

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizCondNum          ! Plant Sizing index for condenser loop
  INTEGER             :: SolFla                 ! Flag of solver
  REAL(r64)           :: DesTowerLoad           ! Design tower load [W]
  REAL(r64)           :: UA0                    ! Lower bound for UA [W/C]
  REAL(r64)           :: UA1                    ! Upper bound for UA [W/C]
  REAL(r64)           :: UA                     ! Calculated UA value
  REAL(r64)           :: Twb                    ! tower inlet air wet-bulb temperature [C]
  REAL(r64)           :: Tr                     ! tower range temperature [C]
  REAL(r64)           :: Ta                     ! tower approach temperature [C]
  REAL(r64)           :: WaterFlowRatio         ! tower water flow rate ratio found during model calibration
  REAL(r64)           :: MaxWaterFlowRateRatio  ! maximum water flow rate ratio which yields desired approach temp
  REAL(r64)           :: WaterFlowRateRatio     ! tower water flow rate ratio
  REAL(r64)           :: Tapproach              ! temporary tower approach temp variable [C]
  REAL(r64)           :: ModelWaterFlowRatioMax ! maximum water flow rate ratio used for model calibration
  REAL(r64)           :: FlowRateRatioStep      ! flow rate ratio to determine maximum water flow rate ratio during calibration
  REAL(r64), DIMENSION(6)  :: Par                    ! Parameter array need for RegulaFalsi routine
  LOGICAL             :: ModelCalibrated        ! TRUE if water flow rate ratio is with the specified range
  CHARACTER(len=6)    :: OutputChar             ! report variable for warning messages
  CHARACTER(len=9)    :: OutputChar2            ! report variable for warning messages
  CHARACTER(len=6)    :: OutputCharLo           ! report variable for warning messages
  CHARACTER(len=6)    :: OutputCharHi           ! report variable for warning messages
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           :: Cp  ! local specific heat for fluid
  REAL(r64)           :: rho ! local density for fluid
  REAL(r64)           :: tmpDesignWaterFlowRate ! local temporary for water volume flow rate
  REAL(r64)           :: tmpHighSpeedFanPower !local temporary for high speed fan power
  REAL(r64)           :: tmpHighSpeedAirFlowRate ! local temporary for high speed air flow rate
  REAL(r64)           :: tmpHighSpeedTowerUA ! local temporary for high speed tower UA
  REAL(r64)           :: tmpLowSpeedAirFlowRate !local temporary for low speed air flow rate
  REAL(r64)           :: AssumedDeltaT ! default delta T for nominal capacity of hard sized with UA method
  REAL(r64)           :: AssumedExitTemp ! default for cp fo nominal capacity of hard sized with UA method
  LOGICAL             :: ErrorsFound

  PltSizCondNum = 0
  DesTowerLoad = 0.0d0
  tmpDesignWaterFlowRate  = SimpleTower(TowerNum)%DesignWaterFlowRate
  tmpHighSpeedFanPower    = SimpleTower(TowerNum)%HighSpeedFanPower
  tmpHighSpeedAirFlowRate = SimpleTower(TowerNum)%HighSpeedAirFlowRate
  tmpHighSpeedTowerUA     = SimpleTower(TowerNum)%HighSpeedTowerUA
  tmpLowSpeedAirFlowRate  = SimpleTower(TowerNum)%LowSpeedAirFlowRate

  ! Find the appropriate Plant Sizing object
  PltSizCondNum = PlantLoop(SimpleTower(TowerNum)%LoopNum)%PlantSizNum

  IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_UFactor .and.  &
      SimpleTower(TowerNum)%HighSpeedTowerUA /= AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                PlantSizData(PltSizCondNum)%ExitTemp, &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                'SizeTower')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                               PlantSizData(PltSizCondNum)%ExitTemp,                      &
                               PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                               'SizeTower')
      DesTowerLoad = rho * Cp  &
                      * SimpleTower(TowerNum)%DesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT
      SimpleTower(TowerNum)%TowerNominalCapacity=DesTowerLoad/SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
    ELSE
      AssumedDeltaT = 11.d0
      AssumedExitTemp = 21.d0
      rho = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                AssumedExitTemp, &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                'SizeTower')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                               AssumedExitTemp,                      &
                               PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                               'SizeTower')

      DesTowerLoad = rho * Cp  &
                      * SimpleTower(TowerNum)%DesignWaterFlowRate * AssumedDeltaT
      SimpleTower(TowerNum)%TowerNominalCapacity=DesTowerLoad/SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
    ENDIF
  END IF

  IF (SimpleTower(TowerNum)%DesignWaterFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpDesignWaterFlowRate = PlantSizData(PltSizCondNum)%DesVolFlowRate * SimpleTower(TowerNum)%SizFac
        IF (PlantSizesOkayToFinalize) SimpleTower(TowerNum)%DesignWaterFlowRate = tmpDesignWaterFlowRate
      ELSE
        tmpDesignWaterFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) SimpleTower(TowerNum)%DesignWaterFlowRate = tmpDesignWaterFlowRate
      ENDIF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Design Water Flow Rate [m3/s]', SimpleTower(TowerNum)%DesignWaterFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing error for cooling tower object = '//TRIM(SimpleTower(TowerNum)%Name))
      CALL ShowFatalError('Autosizing of cooling tower condenser flow rate requires a loop Sizing:Plant object.')
    ENDIF
  ENDIF

  IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
    ! Design water flow rate is assumed to be 3 gpm per ton (SI equivalent 5.382E-8 m3/s per watt)
    SimpleTower(TowerNum)%DesignWaterFlowRate = 5.382d-8 * SimpleTower(TowerNum)%TowerNominalCapacity
    tmpDesignWaterFlowRate = SimpleTower(TowerNum)%DesignWaterFlowRate
    IF (SameString(SimpleTower(TowerNum)%TowerType , 'CoolingTower:SingleSpeed')) THEN
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Design water flow rate based on tower nominal capacity [m3/s]', &
                              SimpleTower(TowerNum)%DesignWaterFlowRate)
    ELSEIF (SameString(SimpleTower(TowerNum)%TowerType , 'CoolingTower:TwoSpeed')) THEN
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Design water flow rate based on tower high-speed nominal capacity [m3/s]', &
                              SimpleTower(TowerNum)%DesignWaterFlowRate)
    ENDIF
  ENDIF

  CALL RegisterPlantCompDesignFlow(SimpleTower(TowerNum)%WaterInletNodeNum,tmpDesignWaterFlowRate)

  IF (SimpleTower(TowerNum)%HighSpeedFanPower == AutoSize) THEN
    ! We assume the nominal fan power is 0.0105 times the design load
    IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
      SimpleTower(TowerNum)%HighSpeedFanPower = 0.0105d0 * SimpleTower(TowerNum)%TowerNominalCapacity
    ELSE
      IF (PltSizCondNum > 0) THEN
        IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
          rho   = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                            InitConvTemp, &
                            PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                            'SizeTower')
          Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                               PlantSizData(PltSizCondNum)%ExitTemp,                      &
                               PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                               'SizeTower')
          DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT
          tmpHighSpeedFanPower =  0.0105d0 * DesTowerLoad
          IF (PlantSizesOkayToFinalize) SimpleTower(TowerNum)%HighSpeedFanPower =  tmpHighSpeedFanPower
        ELSE
          tmpHighSpeedFanPower = 0.d0
          IF (PlantSizesOkayToFinalize) SimpleTower(TowerNum)%HighSpeedFanPower = tmpHighSpeedFanPower
        ENDIF
      ELSE
        CALL ShowSevereError('Autosizing of cooling tower fan power requires a loop Sizing:Plant object.')
        CALL ShowFatalError(' Occurs in cooling tower object= '//TRIM(SimpleTower(TowerNum)%Name))
      ENDIF
    ENDIF
    IF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_SingleSpeed .OR. &
        SimpleTower(TowerNum)%TowerType_Num == CoolingTower_VariableSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                             'Fan Power at Design Air Flow Rate [W]', SimpleTower(TowerNum)%HighSpeedFanPower)
    ELSEIF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                             'Fan Power at High Fan Speed [W]', SimpleTower(TowerNum)%HighSpeedFanPower)
    ENDIF
  ENDIF

  IF (SimpleTower(TowerNum)%HighSpeedAirFlowRate == AutoSize) THEN
! Plant Sizing Object is not required to AUTOSIZE this field since its simply a multiple of another field.
    tmpHighSpeedAirFlowRate = tmpHighSpeedFanPower * 0.5d0 &
                                 * (101325.d0/StdBaroPress) / 190.d0
    IF (PlantSizesOkayToFinalize)  SimpleTower(TowerNum)%HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate

    IF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_SingleSpeed .OR. &
        SimpleTower(TowerNum)%TowerType_Num == CoolingTower_VariableSpeed) THEN
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Design Air Flow Rate [m3/s]', SimpleTower(TowerNum)%HighSpeedAirFlowRate)
    ELSEIF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Air Flow Rate at High Fan Speed [m3/s]', SimpleTower(TowerNum)%HighSpeedAirFlowRate)
    ENDIF
  ENDIF

  IF (SimpleTower(TowerNum)%HighSpeedTowerUA == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho   = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                            InitConvTemp, &
                            PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                            'SizeTower')
        Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                               PlantSizData(PltSizCondNum)%ExitTemp,                      &
                               PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                               'SizeTower')
        DesTowerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT

        ! This conditional statement is to trap when the user specified condenser/tower water design setpoint
        !  temperature is less than design inlet air wet bulb temperature of 25.6 C
        IF ( PlantSizData(PltSizCondNum)%ExitTemp <= 25.6d0 ) THEN
          CALL ShowSevereError('Error when autosizing the UA value for cooling tower = '//TRIM(SimpleTower(TowerNum)%Name)// &
                               '.'//' Design Loop Exit Temperature must be greater than 25.6 C when autosizing the tower UA.')
          CALL ShowContinueError('The Design Loop Exit Temperature specified in Sizing:Plant object = '// &
                                  TRIM(PlantSizData(PltSizCondNum)%PlantLoopName))
          CALL ShowContinueError('is less than or equal to the design inlet air wet-bulb temperature of 25.6 C.')
          CALL ShowContinueError('It is recommended that the Design Loop Exit Temperature = 25.6 C plus '// &
                                 'the cooling tower design approach temperature (e.g., 4 C).')
          CALL ShowContinueError('If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field ' // &
                                 'Condenser Water Design Setpoint must be > 25.6 C if autosizing the cooling tower.')
          CALL ShowFatalError('Autosizing of cooling tower fails for tower = '//TRIM(SimpleTower(TowerNum)%Name)//'.')
        ENDIF
        Par(1) = DesTowerLoad
        Par(2) = REAL(TowerNum,r64)
        Par(3) = rho * tmpDesignWaterFlowRate ! design water mass flow rate
        Par(4) = tmpHighSpeedAirFlowRate ! design air volume flow rate
        Par(5) = Cp
        UA0 = 0.0001d0 * DesTowerLoad ! Assume deltaT = 10000K (limit)
        UA1 = DesTowerLoad          ! Assume deltaT = 1K
        SimpleTowerInlet(TowerNum)%WaterTemp = PlantSizData(PltSizCondNum)%ExitTemp + PlantSizData(PltSizCondNum)%DeltaT
        SimpleTowerInlet(TowerNum)%AirTemp = 35.d0
        SimpleTowerInlet(TowerNum)%AirWetBulb = 25.6d0
        SimpleTowerInlet(TowerNum)%AirPress = StdBaroPress
        SimpleTowerInlet(TowerNum)%AirHumRat =   &
           PsyWFnTdbTwbPb(SimpleTowerInlet(TowerNum)%AirTemp,     &
                          SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                          SimpleTowerInlet(TowerNum)%AirPress)
!        SimpleTowerInlet(TowerNum)%AirHumRat = PsyWFnTdbTwbPb(35.,25.6,StdBaroPress)
        CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par)
        IF (SolFla == -1) THEN
          CALL ShowSevereError('Iteration limit exceeded in calculating tower UA')
          CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
        ELSE IF (SolFla == -2) THEN
          CALL ShowSevereError('Bad starting values for UA')
          CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
        ENDIF

        IF (PlantSizesOkayToFinalize) THEN
          SimpleTower(TowerNum)%HighSpeedTowerUA = UA
        ELSE
          tmpHighSpeedTowerUA = UA
        ENDIF
        SimpleTower(TowerNum)%TowerNominalCapacity=DesTowerLoad/SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
      ELSE
        IF (PlantSizesOkayToFinalize) THEN
          SimpleTower(TowerNum)%HighSpeedTowerUA = 0.d0
        ELSE
          tmpHighSpeedTowerUA = 0.d0
        ENDIF
      ENDIF
      IF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_SingleSpeed) THEN
        IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'U-Factor Times Area Value at Design Air Flow Rate [W/C]', &
                              SimpleTower(TowerNum)%HighSpeedTowerUA)
      ELSEIF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_TwoSpeed) THEN
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'U-Factor Times Area Value at High Fan Speed [W/C]', &
                              SimpleTower(TowerNum)%HighSpeedTowerUA)
      ENDIF
    ELSE
      CALL ShowSevereError('Autosizing error for cooling tower object= '//TRIM(SimpleTower(TowerNum)%Name))
      CALL ShowFatalError('Autosizing of cooling tower UA requires a loop Sizing:Plant object.')
    ENDIF
  ENDIF

  IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
    IF (SimpleTower(TowerNum)%DesignWaterFlowRate >= SmallWaterVolFlow) THEN
      ! nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of delivered cooling
      ! but now is a user input
      rho   = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                          29.44d0,     & ! 85F design exiting water temp
                          PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                          'SizeTower')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                             29.44d0,     & ! 85F design exiting water temp
                             PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                             'SizeTower')

      DesTowerLoad = SimpleTower(TowerNum)%TowerNominalCapacity * SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
      Par(1) = DesTowerLoad
      Par(2) = REAL(TowerNum,r64)
      Par(3) = rho * tmpDesignWaterFlowRate ! design water mass flow rate
      Par(4) = tmpHighSpeedAirFlowRate ! design air volume flow rate
      Par(5) = Cp ! 85F design exiting water temp
      UA0 = 0.0001d0 * DesTowerLoad ! Assume deltaT = 10000K (limit)
      UA1 = DesTowerLoad          ! Assume deltaT = 1K
      SimpleTowerInlet(TowerNum)%WaterTemp = 35.d0   ! 95F design inlet water temperature
      SimpleTowerInlet(TowerNum)%AirTemp = 35.d0     ! 95F design inlet air dry-bulb temp
      SimpleTowerInlet(TowerNum)%AirWetBulb = 25.6d0 ! 78F design inlet air wet-bulb temp
      SimpleTowerInlet(TowerNum)%AirPress = StdBaroPress
      SimpleTowerInlet(TowerNum)%AirHumRat =   &
         PsyWFnTdbTwbPb(SimpleTowerInlet(TowerNum)%AirTemp,  &
                        SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                        SimpleTowerInlet(TowerNum)%AirPress)
!      SimpleTowerInlet(TowerNum)%AirHumRat = PsyWFnTdbTwbPb(35.,25.6,StdBaroPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
        CALL ShowSevereError('Iteration limit exceeded in calculating tower UA')
        CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
      ELSE IF (SolFla == -2) THEN
        CALL ShowSevereError('Bad starting values for UA')
        CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
      ENDIF
      SimpleTower(TowerNum)%HighSpeedTowerUA = UA
    ELSE
      SimpleTower(TowerNum)%HighSpeedTowerUA = 0.0d0
    ENDIF
    IF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_SingleSpeed) THEN
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'U-Factor Times Area Value at Design Air Flow Rate [W/C]', &
                              SimpleTower(TowerNum)%HighSpeedTowerUA)
    ELSEIF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'U-Factor Times Area Value at High Fan Speed [W/C]', &
                              SimpleTower(TowerNum)%HighSpeedTowerUA)
    ENDIF
  ENDIF

  IF (SimpleTower(TowerNum)%LowSpeedAirFlowRate == AutoSize ) THEN
    IF (PlantSizesOkayToFinalize) THEN
      SimpleTower(TowerNum)%LowSpeedAirFlowRate = SimpleTower(TowerNum)%LowSpeedAirFlowRateSizingFactor &
                                                   * SimpleTower(TowerNum)%HighSpeedAirFlowRate
      tmpLowSpeedAirFlowRate = SimpleTower(TowerNum)%LowSpeedAirFlowRate
      CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Low Fan Speed Air Flow Rate [m3/s]', SimpleTower(TowerNum)%LowSpeedAirFlowRate)
    ELSE
      tmpLowSpeedAirFlowRate = SimpleTower(TowerNum)%LowSpeedAirFlowRateSizingFactor * tmpHighSpeedAirFlowRate
    ENDIF
  ENDIF

  IF (SimpleTower(TowerNum)%LowSpeedFanPower == AutoSize ) THEN
    IF (PlantSizesOkayToFinalize) THEN
      SimpleTower(TowerNum)%LowSpeedFanPower = SimpleTower(TowerNum)%LowSpeedFanPowerSizingFactor &
                                                * SimpleTower(TowerNum)%HighSpeedFanPower
      CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Fan Power at Low Fan Speed [W]', SimpleTower(TowerNum)%LowSpeedFanPower)
    ENDIF
  ENDIF

  IF (SimpleTower(TowerNum)%LowSpeedTowerUA == AutoSize .AND. PlantSizesOkayToFinalize) THEN
      SimpleTower(TowerNum)%LowSpeedTowerUA = SimpleTower(TowerNum)%LowSpeedTowerUASizingFactor &
                                               * SimpleTower(TowerNum)%HighSpeedTowerUA
      CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'U-Factor Times Area Value at Low Fan Speed [W/K]', SimpleTower(TowerNum)%LowSpeedTowerUA)
  ENDIF

  IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
    IF (SimpleTower(TowerNum)%TowerLowSpeedNomCap == AutoSize) THEN
      IF (PlantSizesOkayToFinalize) THEN
        SimpleTower(TowerNum)%TowerLowSpeedNomCap = SimpleTower(TowerNum)%TowerLowSpeedNomCapSizingFactor &
                                                     * SimpleTower(TowerNum)%TowerNominalCapacity
        CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                'Low Speed Nominal Capacity [W]', &
                                SimpleTower(TowerNum)%TowerLowSpeedNomCap)
      ENDIF
    ENDIF
    IF (SimpleTower(TowerNum)%TowerFreeConvNomCap == AutoSize) THEN
      IF (PlantSizesOkayToFinalize) THEN
        SimpleTower(TowerNum)%TowerFreeConvNomCap = SimpleTower(TowerNum)%TowerFreeConvNomCapSizingFactor &
                                                     * SimpleTower(TowerNum)%TowerNominalCapacity
        CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                'Free Convection Nominal Capacity [W]', &
                                SimpleTower(TowerNum)%TowerFreeConvNomCap)
      ENDIF
    ENDIF

  ENDIF

  IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity .AND. &
      SameString(SimpleTower(TowerNum)%TowerType , 'CoolingTower:TwoSpeed')) THEN
    IF (SimpleTower(TowerNum)%DesignWaterFlowRate >= SmallWaterVolFlow.AND.SimpleTower(TowerNum)%TowerLowSpeedNomCap > 0.0d0) THEN
      ! nominal capacity doesn't include compressor heat; predefined factor wass 1.25 W heat rejection per W of evap cooling
       ! but now is a user input
      rho   = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                          29.44d0,     & ! 85F design exiting water temp
                          PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                          'SizeTower')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                             29.44d0,     & ! 85F design exiting water temp
                             PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                             'SizeTower')
      DesTowerLoad = SimpleTower(TowerNum)%TowerLowSpeedNomCap * SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
      Par(1) = DesTowerLoad
      Par(2) = REAL(TowerNum,r64)
      Par(3) = rho * tmpDesignWaterFlowRate ! design water mass flow rate
      Par(4) = tmpLowSpeedAirFlowRate ! Air volume flow rate at low fan speed
      Par(5) = Cp ! 85F design exiting water temp
      UA0 = 0.0001d0 * DesTowerLoad ! Assume deltaT = 10000K (limit)
      UA1 = DesTowerLoad          ! Assume deltaT = 1K
      SimpleTowerInlet(TowerNum)%WaterTemp = 35.d0   ! 95F design inlet water temperature
      SimpleTowerInlet(TowerNum)%AirTemp = 35.d0     ! 95F design inlet air dry-bulb temp
      SimpleTowerInlet(TowerNum)%AirWetBulb = 25.6d0 ! 78F design inlet air wet-bulb temp
      SimpleTowerInlet(TowerNum)%AirPress = StdBaroPress
      SimpleTowerInlet(TowerNum)%AirHumRat =   &
         PsyWFnTdbTwbPb(SimpleTowerInlet(TowerNum)%AirTemp,  &
                        SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                        SimpleTowerInlet(TowerNum)%AirPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par)
        IF (SolFla == -1) THEN
          CALL ShowSevereError('Iteration limit exceeded in calculating tower UA')
          CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
        ELSE IF (SolFla == -2) THEN
          CALL ShowSevereError('Bad starting values for UA')
          CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
        ENDIF
      SimpleTower(TowerNum)%LowSpeedTowerUA = UA
    ELSE
      SimpleTower(TowerNum)%LowSpeedTowerUA = 0.0d0
    ENDIF
    IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                            'Low Fan Speed U-Factor Times Area Value [W/K]', &
                            SimpleTower(TowerNum)%LowSpeedTowerUA)
  ENDIF

  IF (SimpleTower(TowerNum)%FreeConvAirFlowRate == AutoSize) THEN
    IF (PlantSizesOkayToFinalize) THEN
      SimpleTower(TowerNum)%FreeConvAirFlowRate = SimpleTower(TowerNum)%FreeConvAirFlowRateSizingFactor &
                                                   * SimpleTower(TowerNum)%HighSpeedAirFlowRate
      CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Free Convection Regime Air Flow Rate [m3/s]', SimpleTower(TowerNum)%FreeConvAirFlowRate)
    ENDIF
  ENDIF

  IF (SimpleTower(TowerNum)%FreeConvTowerUA == AutoSize) THEN
    IF (PlantSizesOkayToFinalize) THEN
      SimpleTower(TowerNum)%FreeConvTowerUA = SimpleTower(TowerNum)%FreeConvTowerUASizingFactor &
                                               * SimpleTower(TowerNum)%HighSpeedTowerUA
      CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Free Convection U-Factor Times Area Value [W/K]',   &
                               SimpleTower(TowerNum)%FreeConvTowerUA)
    ENDIF
  ENDIF



  IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN
    IF (SimpleTower(TowerNum)%DesignWaterFlowRate >= SmallWaterVolFlow.AND.SimpleTower(TowerNum)%TowerFreeConvNomCap > 0.0d0) THEN
      ! nominal capacity doesn't include compressor heat; predefined factor was 1.25 W heat rejection per W of evap cooling,
      ! but now user input
      rho   = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                          29.44d0,     & ! 85F design exiting water temp
                          PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                          'SizeTower')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                             29.44d0,     & ! 85F design exiting water temp
                             PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                             'SizeTower')
      DesTowerLoad = SimpleTower(TowerNum)%TowerFreeConvNomCap * SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
      Par(1) = DesTowerLoad
      Par(2) = REAL(TowerNum,r64)
      Par(3) = rho * SimpleTower(TowerNum)%DesignWaterFlowRate ! design water mass flow rate
      Par(4) = SimpleTower(TowerNum)%FreeConvAirFlowRate ! free convection air volume flow rate
      Par(5) = Cp ! 85F design exiting water temp
      UA0 = 0.0001d0 * DesTowerLoad ! Assume deltaT = 10000K (limit)
      UA1 = DesTowerLoad          ! Assume deltaT = 1K
      SimpleTowerInlet(TowerNum)%WaterTemp = 35.d0   ! 95F design inlet water temperature
      SimpleTowerInlet(TowerNum)%AirTemp = 35.d0     ! 95F design inlet air dry-bulb temp
      SimpleTowerInlet(TowerNum)%AirWetBulb = 25.6d0 ! 78F design inlet air wet-bulb temp
      SimpleTowerInlet(TowerNum)%AirPress = StdBaroPress
      SimpleTowerInlet(TowerNum)%AirHumRat =   &
         PsyWFnTdbTwbPb(SimpleTowerInlet(TowerNum)%AirTemp,  &
                        SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                        SimpleTowerInlet(TowerNum)%AirPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
        CALL ShowSevereError('Iteration limit exceeded in calculating tower UA')
        CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
      ELSE IF (SolFla == -2) THEN
        CALL ShowSevereError('Bad starting values for UA')
        CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
      ENDIF
      SimpleTower(TowerNum)%FreeConvTowerUA = UA
    ELSE
      SimpleTower(TowerNum)%FreeConvTowerUA = 0.0d0
    ENDIF
    IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                            'U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]', &
                            SimpleTower(TowerNum)%FreeConvTowerUA)
  ENDIF



! calibrate variable speed tower model based on user input by finding calibration water flow rate ratio that
! yields an approach temperature that matches user input
  IF(SameString(SimpleTower(TowerNum)%TowerType , 'CoolingTower:VariableSpeed')) THEN

    Twb     = SimpleTower(TowerNum)%DesignInletWB
    Tr      = SimpleTower(TowerNum)%DesignRange
    Ta      = SimpleTower(TowerNum)%DesignApproach

    Par(1) = TowerNum ! Index to cooling tower
    Par(2) = 1.0d0      ! air flow rate ratio
    Par(3) = Twb      ! inlet air wet-bulb temperature [C]
    Par(4) = Tr       ! tower range temperature [C]
    Par(5) = Ta       ! design approach temperature [C]
    Par(6) = 0.0d0      ! Calculation FLAG, 0.0 = calc water flow ratio, 1.0 calc air flow ratio

!   check range for water flow rate ratio (make sure RegulaFalsi converges)
    MaxWaterFlowRateRatio = 0.5d0
    Tapproach     = 0.0d0
    FlowRateRatioStep = (VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio - &
                         VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio)/10.0d0
    ModelCalibrated = .TRUE.
    ModelWaterFlowRatioMax = VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio * 4.0d0
!   find a flow rate large enough to provide an approach temperature > than the user defined approach
    DO WHILE (Tapproach < Ta .AND. MaxWaterFlowRateRatio .LE. ModelWaterFlowRatioMax)
      WaterFlowRateRatio = MaxWaterFlowRateRatio
      CALL CalcVSTowerApproach(TowerNum,WaterFlowRateRatio,1.0d0,Twb,Tr,Tapproach)
      IF(Tapproach < Ta)THEN
        MaxWaterFlowRateRatio = MaxWaterFlowRateRatio + FlowRateRatioStep
      END IF
      ! a water flow rate large enough to provide an approach temperature > than the user defined approach does not exist
      ! within the tolerances specified by the user
      IF((MaxWaterFlowRateRatio .EQ. 0.5d0 .AND. Tapproach .GT. Ta) .OR. MaxWaterFlowRateRatio .GE. ModelWaterFlowRatioMax) THEN
        ModelCalibrated = .FALSE.
        EXIT
      END IF
    END DO

    IF(ModelCalibrated)THEN
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, WaterFlowRatio, SimpleTowerApproachResidual,   &
                               constant_pointfive, MaxWaterFlowRateRatio, Par)
      IF (SolFla == -1) THEN
        CALL ShowSevereError('Iteration limit exceeded in calculating tower water flow ratio during calibration')
        CALL ShowContinueError('Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of'// &
                               'water flow rate ratio for this variable-speed cooling tower.')
        CALL ShowFatalError('Cooling tower calibration failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
      ELSE IF (SolFla == -2) THEN
        CALL ShowSevereError('Bad starting values for cooling tower water flow rate ratio calibration.')
        CALL ShowContinueError('Inlet air wet-bulb, range, and/or approach temperature does not allow calibration of'// &
                               'water flow rate ratio for this variable-speed cooling tower.')
        CALL ShowFatalError('Cooling tower calibration failed for tower '//TRIM(SimpleTower(TowerNum)%Name)//'.')
      ENDIF
    ELSE
      WRITE(OutputChar2,OutputFormat2)WaterFlowRateRatio
      WRITE(OutputChar,OutputFormat)Tapproach
      CALL ShowSevereError('Bad starting values for cooling tower water flow rate ratio calibration.')
      CALL ShowContinueError('Design inlet air wet-bulb or range temperature must be modified to achieve the design approach')
      CALL ShowContinueError('A water flow rate ratio of '//TRIM(OutputChar2)//' was calculated to yield an ' &
                             //'approach temperature of '//TRIM(OutputChar)//'.')
      CALL ShowFatalError('Cooling tower calibration failed for tower '//TRIM(SimpleTower(TowerNum)%Name)//'.')
    END IF

    SimpleTower(TowerNum)%CalibratedWaterFlowRate = SimpleTower(TowerNum)%DesignWaterFlowRate/WaterFlowRatio

    IF(WaterFlowRatio .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio .OR. &
       WaterFlowRatio .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio)THEN
      WRITE(OutputChar2,OutputFormat2)WaterFlowRatio
      WRITE(OutputCharLo,OutputFormat)VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio
      WRITE(OutputCharHi,OutputFormat)VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio
      CALL ShowWarningError('CoolingTower:VariableSpeed, "'//TRIM(SimpleTower(TowerNum)%Name)//&
                     '" the calibrated water flow rate ratio is determined to be '//TRIM(OutputChar2) &
                     //'. This is outside the valid range of '//TRIM(OutputCharLo)//' to '//TRIM(OutputCharHi)//'.')
    END IF

    rho   = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                          (Twb+Ta+Tr), &
                          PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                          'SizeTower')
    Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                             (Twb+Ta+Tr),                      &
                             PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                             'SizeTower')

    SimpleTower(TowerNum)%TowerNominalCapacity = ((rho *tmpDesignWaterFlowRate) * &
                                                   Cp * Tr)
    IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                           'Nominal Capacity [W]', SimpleTower(TowerNum)%TowerNominalCapacity)

    SimpleTower(TowerNum)%FreeConvAirFlowRate = SimpleTower(TowerNum)%MinimumVSAirFlowFrac * &
                                                SimpleTower(TowerNum)%HighSpeedAirFlowRate

    IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                           'Air flow rate in free convection regime [m3/s]', SimpleTower(TowerNum)%FreeConvAirFlowRate)

    SimpleTower(TowerNum)%TowerFreeConvNomCap = SimpleTower(TowerNum)%TowerNominalCapacity * &
                                                SimpleTower(TowerNum)%FreeConvectionCapacityFraction

    IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                           'Tower capacity in free convection regime at design conditions [W]' &
                           , SimpleTower(TowerNum)%TowerFreeConvNomCap)

  END IF
  IF (PlantSizesOkayToFinalize) Then
    !create predefined report
    equipName = SimpleTower(TowerNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,SimpleTower(TowerNum)%TowerType)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,SimpleTower(TowerNum)%TowerNominalCapacity)
  ENDIF

  ! input error checking
  ErrorsFound = .FALSE.
  IF (PlantSizesOkayToFinalize) Then
    IF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_SingleSpeed) THEN
      IF (SimpleTower(TowerNum)%DesignWaterFlowRate > 0.0d0) THEN
        IF (SimpleTower(TowerNum)%FreeConvAirFlowRate >= SimpleTower(TowerNum)%HighSpeedAirFlowRate) THEN
          CALL ShowSevereError(cCoolingTower_SingleSpeed//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                               '". Free convection air flow rate must be less than the design air flow rate.')
          ErrorsFound=.true.
        END IF
        IF (SimpleTower(TowerNum)%FreeConvTowerUA >= SimpleTower(TowerNum)%HighSpeedTowerUA) THEN
          CALL ShowSevereError(cCoolingTower_SingleSpeed//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                               '". Free convection UA must be less than the design tower UA.')
          ErrorsFound=.true.
        END IF
      END IF
    END IF

    IF (SimpleTower(TowerNum)%TowerType_Num == CoolingTower_TwoSpeed) THEN
      IF (SimpleTower(TowerNum)%DesignWaterFlowRate > 0.0d0) THEN
        IF (SimpleTower(TowerNum)%HighSpeedAirFlowRate <= SimpleTower(TowerNum)%LowSpeedAirFlowRate) THEN
          CALL ShowSevereError(cCoolingTower_TwoSpeed//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                               '". Low speed air flow rate must be less than the high speed air flow rate.')
          ErrorsFound=.true.
        ENDIF
        IF (SimpleTower(TowerNum)%LowSpeedAirFlowRate <= SimpleTower(TowerNum)%FreeConvAirFlowRate) THEN
           CALL ShowSevereError(cCoolingTower_TwoSpeed//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Free convection air flow rate must be less than the low speed air flow rate.')
           ErrorsFound=.true.
        ENDIF
        IF (SimpleTower(TowerNum)%HighSpeedTowerUA <= SimpleTower(TowerNum)%LowSpeedTowerUA) THEN
           CALL ShowSevereError(cCoolingTower_TwoSpeed//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                                '". Tower UA at low fan speed must be less than the tower UA at high fan speed.')
           ErrorsFound=.true.
        ENDIF
        IF (SimpleTower(TowerNum)%LowSpeedTowerUA <= SimpleTower(TowerNum)%FreeConvTowerUA) THEN
           CALL ShowSevereError(cCoolingTower_TwoSpeed//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
                              '". Tower UA at free convection air flow rate must be less than the tower UA at low fan speed.')
           ErrorsFound=.true.
        ENDIF
      END IF
    END IF
    IF (ErrorsFound) THEN
      CALL ShowFatalError('InitTower: Program terminated due to previous condition(s).')
    ENDIF
  ENDIF

  RETURN
END SUBROUTINE SizeTower

SUBROUTINE SizeVSMerkelTower(TowerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,       ONLY: PlantLoop, PlantSizesOkayToFinalize
  USE General,         ONLY: SolveRegulaFalsi
  USE PlantUtilities,  ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE PlantUtilities,  ONLY: RegisterPlantCompDesignFlow

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TowerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: MaxIte = 500       ! Maximum number of iterations
  REAL(r64), PARAMETER :: Acc =  0.0001d0    ! Accuracy of result

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizCondNum          ! Plant Sizing index for condenser loop
  INTEGER             :: SolFla                 ! Flag of solver
  REAL(r64) :: tmpNomTowerCap
  REAL(r64) :: tmpDesignWaterFlowRate
  REAL(r64) :: tmpTowerFreeConvNomCap
  REAL(r64) :: tmpDesignAirFlowRate
  REAL(r64) :: tmpHighSpeedFanPower
  REAL(r64) :: tmpFreeConvAirFlowRate

  REAL(r64), DIMENSION(6)  :: Par                    ! Parameter array need for RegulaFalsi routine
  REAL(r64)           :: UA0                    ! Lower bound for UA [W/C]
  REAL(r64)           :: UA1                    ! Upper bound for UA [W/C]
  REAL(r64)           :: DesTowerLoad           ! Design tower load [W]
  REAL(r64)           :: Cp  ! local specific heat for fluid
  REAL(r64)           :: rho ! local density for fluid
  REAL(r64)           :: UA                     ! Calculated UA value
  REAL(r64)           :: OutWaterTemp

  ! Find the appropriate Plant Sizing object
  PltSizCondNum = PlantLoop(SimpleTower(TowerNum)%LoopNum)%PlantSizNum

  tmpNomTowerCap = SimpleTower(TowerNum)%TowerNominalCapacity
  tmpDesignWaterFlowRate = SimpleTower(TowerNum)%DesignWaterFlowRate

  tmpTowerFreeConvNomCap = SimpleTower(TowerNum)%TowerFreeConvNomCap
  tmpDesignAirFlowRate   = SimpleTower(TowerNum)%HighSpeedAirFlowRate
  tmpHighSpeedFanPower   = SimpleTower(TowerNum)%HighSpeedFanPower
  tmpFreeConvAirFlowRate = SimpleTower(TowerNum)%FreeConvAirFlowRate

  IF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_NominalCapacity) THEN

    IF (SimpleTower(TowerNum)%TowerNominalCapacity == autosize) THEN
      ! get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
      IF (PltSizCondNum > 0) THEN
        IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
          rho = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                    PlantSizData(PltSizCondNum)%ExitTemp, &
                                    PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                    'SizeTower')
          Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                   PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                   PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                                   'SizeTower')
          DesTowerLoad = rho * Cp  &
                          * PlantSizData(PltSizCondNum)%DesVolFlowRate * PlantSizData(PltSizCondNum)%DeltaT &
                          * SimpleTower(TowerNum)%SizFac
          tmpNomTowerCap  = DesTowerLoad / SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
          IF (PlantSizesOkayToFinalize) THEN
            SimpleTower(TowerNum)%TowerNominalCapacity = tmpNomTowerCap
            CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Nominal Capacity [W]', SimpleTower(TowerNum)%TowerNominalCapacity)
          ENDIF
        ELSE
          tmpNomTowerCap  = 0.d0
          IF (PlantSizesOkayToFinalize) THEN
            SimpleTower(TowerNum)%TowerNominalCapacity = tmpNomTowerCap
            CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Nominal Capacity [W]', SimpleTower(TowerNum)%TowerNominalCapacity)
          ENDIF
        ENDIF

      ELSE
        CALL ShowSevereError('Autosizing error for cooling tower object = '//TRIM(SimpleTower(TowerNum)%Name))
        CALL ShowFatalError('Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.')
      ENDIF

    ENDIF

    IF (SimpleTower(TowerNum)%TowerFreeConvNomCap == autosize) THEN
      tmpTowerFreeConvNomCap = tmpNomTowerCap * SimpleTower(TowerNum)%TowerFreeConvNomCapSizingFactor
      IF (PlantSizesOkayToFinalize) THEN
        SimpleTower(TowerNum)%TowerFreeConvNomCap = tmpTowerFreeConvNomCap
        CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                'Free Convection Nominal Capacity [W]', &
                                SimpleTower(TowerNum)%TowerFreeConvNomCap)
      ENDIF
    ENDIF

    IF (SimpleTower(TowerNum)%DesignWaterFlowRate == autosize) THEN
    ! for nominal cap input method, get design water flow rate from nominal cap and scalable sizing factor
      tmpDesignWaterFlowRate = tmpNomTowerCap * SimpleTower(TowerNum)%DesignWaterFlowPerUnitNomCap
      IF (PlantSizesOkayToFinalize) THEN
        SimpleTower(TowerNum)%DesignWaterFlowRate = tmpDesignWaterFlowRate
        CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Design Water Flow Rate [m3/s]', SimpleTower(TowerNum)%DesignWaterFlowRate)
      ENDIF
    ENDIF

    CALL RegisterPlantCompDesignFlow(SimpleTower(TowerNum)%WaterInletNodeNum,tmpDesignWaterFlowRate)

    IF (SimpleTower(TowerNum)%HighSpeedAirFlowRate ==  autosize) THEN
      IF (SimpleTower(TowerNum)%DefaultedDesignAirFlowScalingFactor) THEN
        tmpDesignAirFlowRate =  tmpNomTowerCap * SimpleTower(TowerNum)%DesignAirFlowPerUnitNomCap * (101325.d0/StdBaroPress)
      ELSE
        tmpDesignAirFlowRate =  tmpNomTowerCap * SimpleTower(TowerNum)%DesignAirFlowPerUnitNomCap
      ENDIF
      IF (PlantSizesOkayToFinalize) THEN
        SimpleTower(TowerNum)%HighSpeedAirFlowRate = tmpDesignAirFlowRate
        CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Design Air Flow Rate [m3/s]', SimpleTower(TowerNum)%HighSpeedAirFlowRate)
      ENDIF
    ENDIF

    IF (SimpleTower(TowerNum)%FreeConvAirFlowRate == autosize) THEN
      tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * SimpleTower(TowerNum)%FreeConvAirFlowRateSizingFactor
      IF (PlantSizesOkayToFinalize) THEN
        SimpleTower(TowerNum)%FreeConvAirFlowRate = tmpFreeConvAirFlowRate
        CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Free Convection Regime Air Flow Rate [m3/s]', SimpleTower(TowerNum)%FreeConvAirFlowRate)
      ENDIF
    ENDIF

    ! now calcuate UA values from nominal capacities and flow rates
    IF (PlantSizesOkayToFinalize .and. (.NOT. SimpleTower(TowerNum)%UAvaluesCompleted)) THEN
      IF (PltSizCondNum > 0) THEN ! user has a plant sizing object
        Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                                'SizeTower')
        SimpleTowerInlet(TowerNum)%WaterTemp = PlantSizData(PltSizCondNum)%ExitTemp + PlantSizData(PltSizCondNum)%DeltaT
      ELSE ! probably no plant sizing object
        Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                InitConvTemp,                      &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                                'SizeTower')
        SimpleTowerInlet(TowerNum)%WaterTemp = 35.d0 ! design condition
      ENDIF
      rho   = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                          InitConvTemp, &
                          PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                          'SizeTower')

      ! full speed fan tower UA
      Par(1) = tmpNomTowerCap*SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
      Par(2) = REAL(TowerNum,r64)
      Par(3) = rho * tmpDesignWaterFlowRate ! design water mass flow rate
      Par(4) = tmpDesignAirFlowRate ! design air volume flow rate
      Par(5) = Cp
      UA0 = 0.0001d0 * Par(1) ! Assume deltaT = 10000K (limit)
      UA1 = Par(1)          ! Assume deltaT = 1K

      SimpleTowerInlet(TowerNum)%AirTemp = 35.d0
      SimpleTowerInlet(TowerNum)%AirWetBulb = 25.6d0
      SimpleTowerInlet(TowerNum)%AirPress = StdBaroPress
      SimpleTowerInlet(TowerNum)%AirHumRat =   &
          PsyWFnTdbTwbPb(SimpleTowerInlet(TowerNum)%AirTemp,     &
                        SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                        SimpleTowerInlet(TowerNum)%AirPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
        CALL ShowSevereError('Iteration limit exceeded in calculating tower UA')
        CALL ShowFatalError('calculating cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
      ELSE IF (SolFla == -2) THEN
        CALL ShowSevereError('Bad starting values for UA')
        CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
      ENDIF
      SimpleTower(TowerNum)%HighSpeedTowerUA = UA

      CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                            'U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]', &
                            SimpleTower(TowerNum)%HighSpeedTowerUA)

      ! free convection tower UA
      Par(1) = tmpTowerFreeConvNomCap*SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
      Par(2) = REAL(TowerNum,r64)
      Par(3) = rho * tmpDesignWaterFlowRate ! design water mass flow rate
      Par(4) = tmpFreeConvAirFlowRate ! design air volume flow rate
      Par(5) = Cp
      UA0 = 0.0001d0 * Par(1) ! Assume deltaT = 10000K (limit)
      UA0 = MAX(UA0, 1.d0) ! limit to 1.0
      UA1 = Par(1)          ! Assume deltaT = 1K

      SimpleTowerInlet(TowerNum)%AirTemp = 35.d0
      SimpleTowerInlet(TowerNum)%AirWetBulb = 25.6d0
      SimpleTowerInlet(TowerNum)%AirPress = StdBaroPress
      SimpleTowerInlet(TowerNum)%AirHumRat =   &
          PsyWFnTdbTwbPb(SimpleTowerInlet(TowerNum)%AirTemp,     &
                        SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                        SimpleTowerInlet(TowerNum)%AirPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
        CALL ShowSevereError('Iteration limit exceeded in calculating tower free convection UA')
        CALL ShowFatalError('calculating cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
      ELSE IF (SolFla == -2) THEN
        CALL ShowSevereError('Bad starting values for UA')
        CALL ShowFatalError('Autosizing of cooling tower UA failed for free convection tower '//TRIM(SimpleTower(TowerNum)%Name))
      ENDIF
      SimpleTower(TowerNum)%FreeConvTowerUA = UA
      CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                            'U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]', &
                            SimpleTower(TowerNum)%FreeConvTowerUA)
      SimpleTower(TowerNum)%UAvaluesCompleted = .TRUE.
    ENDIF

  ELSEIF (SimpleTower(TowerNum)%PerformanceInputMethod_Num == PIM_UFactor) THEN
  !UA input method

    IF (SimpleTower(TowerNum)%DesignWaterFlowRate == autosize) THEN ! get from plant sizing
      ! UA input method using plant sizing for flow rate, whereas Nominal capacity method uses scalable sizing factor per cap
      IF (PltSizCondNum > 0) THEN
        IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
          tmpDesignWaterFlowRate = PlantSizData(PltSizCondNum)%DesVolFlowRate* SimpleTower(TowerNum)%SizFac
          IF (PlantSizesOkayToFinalize) THEN
            SimpleTower(TowerNum)%DesignWaterFlowRate = tmpDesignWaterFlowRate
            CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                  'Design Water Flow Rate [m3/s]', SimpleTower(TowerNum)%DesignWaterFlowRate)
          ENDIF
        ELSE
          tmpDesignWaterFlowRate = 0.d0

        ENDIF

      ELSE
        CALL ShowSevereError('Autosizing error for cooling tower object = '//TRIM(SimpleTower(TowerNum)%Name))
        CALL ShowFatalError('Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.')
      ENDIF
    ENDIF
    CALL RegisterPlantCompDesignFlow(SimpleTower(TowerNum)%WaterInletNodeNum,tmpDesignWaterFlowRate)

    IF ( SimpleTower(TowerNum)%HighSpeedTowerUA ==  autosize) THEN
      ! get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
      IF (PltSizCondNum > 0) THEN
        IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
          rho = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                    PlantSizData(PltSizCondNum)%ExitTemp, &
                                    PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                    'SizeTower')
          Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                   PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                   PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                                   'SizeTower')
          DesTowerLoad = rho * Cp  &
                          * PlantSizData(PltSizCondNum)%DesVolFlowRate * PlantSizData(PltSizCondNum)%DeltaT &
                          * SimpleTower(TowerNum)%SizFac
          tmpNomTowerCap  = DesTowerLoad / SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
          IF (PlantSizesOkayToFinalize) THEN
            SimpleTower(TowerNum)%TowerNominalCapacity = tmpNomTowerCap
            CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Nominal Capacity [W]', SimpleTower(TowerNum)%TowerNominalCapacity)
          ENDIF
        ELSE
          tmpNomTowerCap  = 0.d0
          IF (PlantSizesOkayToFinalize) THEN
            SimpleTower(TowerNum)%TowerNominalCapacity = tmpNomTowerCap
            CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Nominal Capacity [W]', SimpleTower(TowerNum)%TowerNominalCapacity)
          ENDIF
        ENDIF
      ELSE
        CALL ShowSevereError('Autosizing error for cooling tower object = '//TRIM(SimpleTower(TowerNum)%Name))
        CALL ShowFatalError('Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.')
      ENDIF
      IF (SimpleTower(TowerNum)%TowerFreeConvNomCap == autosize) THEN
        tmpTowerFreeConvNomCap = tmpNomTowerCap * SimpleTower(TowerNum)%TowerFreeConvNomCapSizingFactor
        IF (PlantSizesOkayToFinalize) THEN
          SimpleTower(TowerNum)%TowerFreeConvNomCap = tmpTowerFreeConvNomCap
          CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                  'Free Convection Nominal Capacity [W]', &
                                  SimpleTower(TowerNum)%TowerFreeConvNomCap)
        ENDIF
      ENDIF
      IF (SimpleTower(TowerNum)%HighSpeedAirFlowRate ==  autosize) THEN
        IF (SimpleTower(TowerNum)%DefaultedDesignAirFlowScalingFactor) THEN
          tmpDesignAirFlowRate =  tmpNomTowerCap * SimpleTower(TowerNum)%DesignAirFlowPerUnitNomCap * (101325.d0/StdBaroPress)
        ELSE
          tmpDesignAirFlowRate =  tmpNomTowerCap * SimpleTower(TowerNum)%DesignAirFlowPerUnitNomCap
        ENDIF
        IF (PlantSizesOkayToFinalize) THEN
          SimpleTower(TowerNum)%HighSpeedAirFlowRate = tmpDesignAirFlowRate
          CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                'Design Air Flow Rate [m3/s]', SimpleTower(TowerNum)%HighSpeedAirFlowRate)
        ENDIF
      ENDIF
      IF (SimpleTower(TowerNum)%FreeConvAirFlowRate == autosize) THEN
        tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * SimpleTower(TowerNum)%FreeConvAirFlowRateSizingFactor
        IF (PlantSizesOkayToFinalize) THEN
          SimpleTower(TowerNum)%FreeConvAirFlowRate = tmpFreeConvAirFlowRate
          CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                'Free Convection Regime Air Flow Rate [m3/s]', SimpleTower(TowerNum)%FreeConvAirFlowRate)
        ENDIF
      ENDIF
      ! now calcuate UA values from nominal capacities and flow rates
      IF (PlantSizesOkayToFinalize) THEN
        rho   = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                            InitConvTemp, &
                            PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                            'SizeTower')
        Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                                'SizeTower')
        ! full speed fan tower UA
        Par(1) = tmpNomTowerCap* SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
        Par(2) = REAL(TowerNum,r64)
        Par(3) = rho * tmpDesignWaterFlowRate ! design water mass flow rate
        Par(4) = tmpDesignAirFlowRate ! design air volume flow rate
        Par(5) = Cp
        UA0 = 0.0001d0 * Par(1) ! Assume deltaT = 10000K (limit)
        UA1 = Par(1)          ! Assume deltaT = 1K
        SimpleTowerInlet(TowerNum)%WaterTemp = PlantSizData(PltSizCondNum)%ExitTemp + PlantSizData(PltSizCondNum)%DeltaT
        SimpleTowerInlet(TowerNum)%AirTemp = 35.d0
        SimpleTowerInlet(TowerNum)%AirWetBulb = 25.6d0
        SimpleTowerInlet(TowerNum)%AirPress = StdBaroPress
        SimpleTowerInlet(TowerNum)%AirHumRat =   &
            PsyWFnTdbTwbPb(SimpleTowerInlet(TowerNum)%AirTemp,     &
                          SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                          SimpleTowerInlet(TowerNum)%AirPress)
        CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par)
        IF (SolFla == -1) THEN
          CALL ShowSevereError('Iteration limit exceeded in calculating tower UA')
          CALL ShowFatalError('calculating cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
        ELSE IF (SolFla == -2) THEN
          CALL ShowSevereError('Bad starting values for UA')
          CALL ShowFatalError('Autosizing of cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
        ENDIF
        SimpleTower(TowerNum)%HighSpeedTowerUA = UA
        CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                            'U-Factor Times Area Value at Full Speed Air Flow Rate [W/C]', &
                            SimpleTower(TowerNum)%HighSpeedTowerUA)
        ! free convection tower UA
        Par(1) = tmpTowerFreeConvNomCap* SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
        Par(2) = REAL(TowerNum,r64)
        Par(3) = rho * tmpDesignWaterFlowRate ! design water mass flow rate
        Par(4) = tmpFreeConvAirFlowRate ! design air volume flow rate
        Par(5) = Cp
        UA0 = 0.0001d0 * Par(1) ! Assume deltaT = 10000K (limit)
        UA1 = Par(1)          ! Assume deltaT = 1K
        SimpleTowerInlet(TowerNum)%WaterTemp = PlantSizData(PltSizCondNum)%ExitTemp + PlantSizData(PltSizCondNum)%DeltaT
        SimpleTowerInlet(TowerNum)%AirTemp = 35.d0
        SimpleTowerInlet(TowerNum)%AirWetBulb = 25.6d0
        SimpleTowerInlet(TowerNum)%AirPress = StdBaroPress
        SimpleTowerInlet(TowerNum)%AirHumRat =   &
            PsyWFnTdbTwbPb(SimpleTowerInlet(TowerNum)%AirTemp,     &
                          SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                          SimpleTowerInlet(TowerNum)%AirPress)
        CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleTowerUAResidual, UA0, UA1, Par)
        IF (SolFla == -1) THEN
          CALL ShowSevereError('Iteration limit exceeded in calculating tower free convection UA')
          CALL ShowFatalError('calculating cooling tower UA failed for tower '//TRIM(SimpleTower(TowerNum)%Name))
        ELSE IF (SolFla == -2) THEN
          CALL ShowSevereError('Bad starting values for UA')
          CALL ShowFatalError('Autosizing of cooling tower UA failed for free convection tower '//TRIM(SimpleTower(TowerNum)%Name))
        ENDIF
        SimpleTower(TowerNum)%LowSpeedTowerUA = UA
        CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                            'U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]', &
                            SimpleTower(TowerNum)%FreeConvTowerUA)
      ENDIF

    ELSE !full speed UA given

      IF (SimpleTower(TowerNum)%FreeConvTowerUA == autosize) THEN ! determine from scalable sizing factor
        IF (PlantSizesOkayToFinalize) THEN
          SimpleTower(TowerNum)%FreeConvTowerUA = SimpleTower(TowerNum)%HighSpeedTowerUA * &
                                                    SimpleTower(TowerNum)%FreeConvTowerUASizingFactor
          CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                            'U-Factor Times Area Value at Free Convection Air Flow Rate [W/C]', &
                            SimpleTower(TowerNum)%FreeConvTowerUA)
        ENDIF
      ENDIF

      IF (SimpleTower(TowerNum)%HighSpeedAirFlowRate ==  autosize) THEN ! given UA but not air flow rate
       ! need an air flow rate to find capacity from UA but flow rate is scaled off capacity
        ! get nominal capacity from PlantSizData(PltSizCondNum)%DeltaT and PlantSizData(PltSizCondNum)%DesVolFlowRate
        IF (PltSizCondNum > 0) THEN
          IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
            rho = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                      PlantSizData(PltSizCondNum)%ExitTemp, &
                                      PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                      'SizeTower')
            Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                     PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                     PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                                     'SizeTower')
            DesTowerLoad = rho * Cp  &
                            * PlantSizData(PltSizCondNum)%DesVolFlowRate * PlantSizData(PltSizCondNum)%DeltaT
            tmpNomTowerCap  = DesTowerLoad / SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
            IF (PlantSizesOkayToFinalize) THEN
              SimpleTower(TowerNum)%TowerNominalCapacity = tmpNomTowerCap
              CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                'Nominal Capacity [W]', SimpleTower(TowerNum)%TowerNominalCapacity)
            ENDIF
          ELSE
            tmpNomTowerCap  = 0.d0
            IF (PlantSizesOkayToFinalize) THEN
              SimpleTower(TowerNum)%TowerNominalCapacity = tmpNomTowerCap
              CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                'Nominal Capacity [W]', SimpleTower(TowerNum)%TowerNominalCapacity)
            ENDIF
          ENDIF

        ELSE
          CALL ShowSevereError('Autosizing error for cooling tower object = '//TRIM(SimpleTower(TowerNum)%Name))
          CALL ShowFatalError('Autosizing of cooling tower nominal capacity requires a loop Sizing:Plant object.')
        ENDIF


        IF (SimpleTower(TowerNum)%DefaultedDesignAirFlowScalingFactor) THEN
          tmpDesignAirFlowRate =  tmpNomTowerCap * SimpleTower(TowerNum)%DesignAirFlowPerUnitNomCap * (101325.d0/StdBaroPress)
        ELSE
          tmpDesignAirFlowRate =  tmpNomTowerCap * SimpleTower(TowerNum)%DesignAirFlowPerUnitNomCap
        ENDIF
        IF (PlantSizesOkayToFinalize) THEN
          SimpleTower(TowerNum)%HighSpeedAirFlowRate = tmpDesignAirFlowRate
          CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Design Air Flow Rate [m3/s]', SimpleTower(TowerNum)%HighSpeedAirFlowRate)
        ENDIF

      ELSE ! UA and Air flow rate given, so find Nominal Cap from running model

        rho = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                      PlantSizData(PltSizCondNum)%ExitTemp, &
                                      PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                      'SizeTower')
        Cp = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                                'SizeTower')

        SimpleTowerInlet(TowerNum)%WaterTemp = PlantSizData(PltSizCondNum)%ExitTemp + PlantSizData(PltSizCondNum)%DeltaT
        SimpleTowerInlet(TowerNum)%AirTemp = 35.d0
        SimpleTowerInlet(TowerNum)%AirWetBulb = 25.6d0
        SimpleTowerInlet(TowerNum)%AirPress = StdBaroPress
        SimpleTowerInlet(TowerNum)%AirHumRat =   &
            PsyWFnTdbTwbPb(SimpleTowerInlet(TowerNum)%AirTemp,     &
                          SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                          SimpleTowerInlet(TowerNum)%AirPress)
        CALL SimSimpleTower(TowerNum,rho * tmpDesignWaterFlowRate,SimpleTower(TowerNum)%HighSpeedAirFlowRate, &
                                             SimpleTower(TowerNum)%HighSpeedTowerUA,OutWaterTemp)
        tmpNomTowerCap = cp*rho * tmpDesignWaterFlowRate*(SimpleTowerInlet(TowerNum)%WaterTemp - OutWaterTemp)
        tmpNomTowerCap =  tmpNomTowerCap / SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
        IF (PlantSizesOkayToFinalize) THEN
          SimpleTower(TowerNum)%TowerNominalCapacity = tmpNomTowerCap
          CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                            'Nominal Capacity [W]', SimpleTower(TowerNum)%TowerNominalCapacity)
        ENDIF

      ENDIF ! both UA and air flow rate given

      IF (SimpleTower(TowerNum)%FreeConvAirFlowRate == autosize) THEN
        tmpFreeConvAirFlowRate = tmpDesignAirFlowRate * SimpleTower(TowerNum)%FreeConvAirFlowRateSizingFactor
        IF (PlantSizesOkayToFinalize) THEN
          SimpleTower(TowerNum)%FreeConvAirFlowRate = tmpFreeConvAirFlowRate
          CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                              'Free Convection Regime Air Flow Rate [m3/s]', SimpleTower(TowerNum)%FreeConvAirFlowRate)
        ENDIF
      ENDIF

      CALL SimSimpleTower(TowerNum,rho * tmpDesignWaterFlowRate,tmpFreeConvAirFlowRate, &
                                            SimpleTower(TowerNum)%FreeConvTowerUA,OutWaterTemp)
      tmpTowerFreeConvNomCap = cp*rho * tmpDesignWaterFlowRate*(SimpleTowerInlet(TowerNum)%WaterTemp - OutWaterTemp)
      tmpTowerFreeConvNomCap = tmpTowerFreeConvNomCap  / SimpleTower(TowerNum)%HeatRejectCapNomCapSizingRatio
      IF (PlantSizesOkayToFinalize) THEN
        SimpleTower(TowerNum)%TowerFreeConvNomCap = tmpTowerFreeConvNomCap
        CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                                'Free Convection Nominal Capacity [W]', &
                                SimpleTower(TowerNum)%TowerFreeConvNomCap)
      ENDIF

    ENDIF


  ENDIF

  IF (SimpleTower(TowerNum)%HighSpeedFanPower == autosize) THEN
    tmpHighSpeedFanPower   = tmpNomTowerCap * SimpleTower(TowerNum)%DesignFanPowerPerUnitNomCap
    IF (PlantSizesOkayToFinalize) THEN
      SimpleTower(TowerNum)%HighSpeedFanPower = tmpHighSpeedFanPower
      CALL ReportSizingOutput(SimpleTower(TowerNum)%TowerType, SimpleTower(TowerNum)%Name, &
                            'Design Fan Power [W]', SimpleTower(TowerNum)%HighSpeedFanPower)
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE SizeVSMerkelTower


! End Initialization Section for the CondenserLoopTowers Module
!******************************************************************************


! Beginning of the CondenserLoopTowers Module Simulation Subroutines
! *****************************************************************************

SUBROUTINE CalcSingleSpeedTower(TowerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       T Hong, Aug. 2008. Added fluid bypass for single speed cooling tower
          !                      The OutletWaterTemp from SimSimpleTower can be lower than 0 degreeC
          !                      which may not be allowed in practice if water is the tower fluid.
          !                      Chandan Sharma, FSEC, February 2010, Added basin heater
          !                      A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
          !       RE-ENGINEERED  Jan 2001, Richard Raustad

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To simulate the operation of a single-speed fan cooling tower.

          ! METHODOLOGY EMPLOYED:
          !
          ! The cooling tower is modeled using effectiveness-NTU relationships for
          ! counterflow heat exchangers based on Merkel's theory.
          !
          ! The subroutine calculates the period of time required to meet a
          ! leaving water temperature setpoint. It assumes that part-load
          ! operation represents a linear interpolation of two steady-state regimes.
          ! Cyclic losses are neglected. The period of time required to meet the
          ! leaving water temperature setpoint is used to determine the required
          ! fan power and energy. Free convection regime is also modeled. This
          ! occures when the pump is operating and the fan is off. If free convection
          ! regime cooling is all that is required for a given time step, the leaving
          ! water temperature is allowed to fall below the leaving water temperature
          ! setpoint (free cooling). At times when the cooling tower fan is required,
          ! the leaving water temperature is at or above the setpoint.
          !
          ! A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
          ! or schedule, of the cooling tower. If the tower is OFF, outlet water
          ! temperature and flow rate are passed through the model from inlet node to
          ! outlet node without intervention (with the exception of free convection
          ! where water temperature is allowed to float below the outlet water set
          ! point). Reports are also updated with fan power and energy being zero.
          !
          ! When the RunFlag indicates an ON condition for the cooling tower, the
          ! mass flow rate and water temperature are read from the inlet node of the
          ! cooling tower (water-side). The outdoor air wet-bulb temperature is used
          ! as the entering condition to the cooling tower (air-side). Input deck
          ! parameters are read for the free convection regime (pump ON and fan OFF)
          ! and a leaving water temperature is calculated. If the leaving water temperature
          ! is at or below the setpoint, the calculated leaving water temperature is
          ! placed on the outlet node and no fan power is used. If the calculated leaving
          ! water temperature is above the setpoint, the cooling tower fan is turned on
          ! and design parameters are used to again calculate the leaving water temperature.
          ! If the calculated leaving water temperature is below the setpoint, a fan
          ! run-time fraction is calculated and used to determine fan power. The leaving
          ! water temperature setpoint is placed on the outlet node. If the calculated
          ! leaving water temperature is at or above the setpoint, the calculated
          ! leaving water temperature is placed on the outlet node and the fan runs at
          ! full power. Water mass flow rate is passed from inlet node to outlet node
          ! with no intervention.
          !
          ! If a tower has multiple cells, the specified inputs of or the autosized capacity
          !  and air/water flow rates are for the entire tower. The number of cells to operate
          !  is first determined based on the user entered minimal and maximal water flow fractions
          !  per cell. If the loads are not met, more cells (if available) will operate to meet
          !  the loads. Inside each cell, the capacity controls still apply. Each cell operates
          !  in the same way.

          ! REFERENCES:
          ! ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.

          ! USE STATEMENTS:
  USE DataPlant, ONLY : PlantLoop, SingleSetPoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: TowerNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER  :: MaxIteration = 100          ! Maximum fluid bypass iteration calculations
  CHARACTER(len=3), PARAMETER :: MaxItChar  ='100'
  REAL(r64), PARAMETER :: BypassFractionThreshold = 0.01d0   !Threshold to stop bypass iteration
  REAL(r64), PARAMETER :: OWTLowerLimit = 0.0d0       ! The limit of tower exit fluid temperature used in the fluid bypass
                                                     !  calculation to avoid fluid freezing. For water, it is 0 degreeC,
                                                     !  for glycols, it can be much lower. The fluid type is stored at the loop.
                                                     !  Current choices are Water and Steam, needs to expand for glycols

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: AirFlowRate
  REAL(r64)              :: UAdesign            ! UA value at design conditions (entered by user or calculated)
  REAL(r64)              :: OutletWaterTempOFF
  REAL(r64)              :: FanModeFrac
  REAL(r64)              :: DesignWaterFlowRate
  REAL(r64)              :: FanPowerOn
  REAL(r64)              :: CpWater
  REAL(r64)              :: TempSetPoint


  !Added variables for fluid bypass
  INTEGER                :: NumIteration
  INTEGER                :: CapacityControl                  ! Capacity Control (0 - FanCycling, 1 - FluidBypass)
  INTEGER                :: BypassFlag                       ! Flag indicator for fluid bypass (0 - no bypass, 1 - bypass)
  REAL(r64)              :: BypassFraction                   ! Fluid bypass fraction
  REAL(r64)              :: BypassFraction2                  ! Fluid bypass fraction
  REAL(r64)              :: BypassFractionPrev
  REAL(r64)              :: OutletWaterTempPrev

  !Added variables for multicell
  REAL(r64)              :: WaterMassFlowRatePerCellMin
  REAL(r64)              :: WaterMassFlowRatePerCellMax
  INTEGER                :: NumCellMin = 0
  INTEGER                :: NumCellMax = 0
  INTEGER                :: NumCellON = 0
  REAL(r64)              :: WaterMassFlowRatePerCell
  LOGICAL                :: IncrNumCellFlag                   ! determine if yes or no we increase the number of cells

  INTEGER :: LoopNum
  INTEGER :: LoopSideNum

    !set inlet and outlet nodes
    WaterInletNode     = SimpleTower(TowerNum)%WaterInletNodeNum
    WaterOutletNode    = SimpleTower(TowerNum)%WaterOutletNodeNum
    Qactual            = 0.0d0
    CTFanPower         = 0.0d0
    OutletWaterTemp    = Node(WaterInletNode)%Temp
    LoopNum            = SimpleTower(TowerNum)%LoopNum
    LoopSideNum        = SimpleTower(TowerNum)%LoopSideNum
    SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
    CASE (SingleSetPoint)
      IF (SimpleTower(TowerNum)%SetpointIsOnOutlet) THEN
        TempSetPoint     = Node(WaterOutletNode)%TempSetpoint
      ELSE
        TempSetPoint     = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpoint
      ENDIF
    CASE (DualSetPointDeadBand)
      IF (SimpleTower(TowerNum)%SetpointIsOnOutlet) THEN
        TempSetPoint     = Node(WaterOutletNode)%TempSetpointHi
      ELSE
        TempSetPoint     = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpointHi
      ENDIF
    END SELECT

    ! Added for fluid bypass. First assume no fluid bypass
    BypassFlag = 0
    BypassFraction = 0.0d0
    BypassFraction2 = 0.0d0
    SimpleTower(TowerNum)%BypassFraction = 0.0d0
    CapacityControl = SimpleTower(TowerNum)%CapacityControl


    ! Added for multi-cell. Determine the number of cells operating
    IF (SimpleTower(TowerNum)%DesWaterMassFlowRate > 0.0D0) THEN
      WaterMassFlowRatePerCellMin = SimpleTower(TowerNum)%DesWaterMassFlowRate *   &
         SimpleTower(TowerNum)%MinFracFlowRate / SimpleTower(TowerNum)%NumCell
      WaterMassFlowRatePerCellMax = SimpleTower(TowerNum)%DesWaterMassFlowRate *   &
         SimpleTower(TowerNum)%MaxFracFlowRate / SimpleTower(TowerNum)%NumCell

      !round it up to the nearest integer
      NumCellMin = MIN(INT((WaterMassFlowRate / WaterMassFlowRatePerCellMax)+.9999d0),SimpleTower(TowerNum)%NumCell)
      NumCellMax = MIN(INT((WaterMassFlowRate / WaterMassFlowRatePerCellMin)+.9999d0),SimpleTower(TowerNum)%NumCell)
    ENDIF

    ! cap min at 1
    IF(NumCellMin <= 0)NumCellMin = 1
    IF(NumCellMax <= 0)NumCellMax = 1

    IF(SimpleTower(TowerNum)%CellCtrl_Num == CellCtrl_MinCell)THEN
      NumCellON = NumCellMin
    ELSE
      NumCellON = NumCellMax
    END IF

    SimpleTower(TowerNum)%NumCellON = NumCellON
    WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellON

   ! Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.

  ! MassFlowTolerance is a parameter to indicate a no flow condition
    IF(WaterMassFlowRate.LE.MassFlowTolerance)THEN
      ! for multiple cells, we assume that it's a commun bassin
      CALL CalcBasinHeaterPower(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff,&
                                SimpleTower(TowerNum)%BasinHeaterSchedulePtr,&
                                SimpleTower(TowerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
      RETURN
    ENDIF

    IncrNumCellFlag = .true. ! set value to true to enter in the loop

    DO WHILE (IncrNumCellFlag)
      IncrNumCellFlag = .false.

  !   Initialize local variables to the free convection design values
      UAdesign               = SimpleTower(TowerNum)%FreeConvTowerUA / SimpleTower(TowerNum)%NumCell
      AirFlowRate            = SimpleTower(TowerNum)%FreeConvAirFlowRate / SimpleTower(TowerNum)%NumCell
      DesignWaterFlowRate    = SimpleTower(TowerNum)%DesignWaterFlowRate
      OutletWaterTempOFF     = Node(WaterInletNode)%Temp
      OutletWaterTemp        = OutletWaterTempOFF
      FanModeFrac            = 0.0d0

      Call SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRate,UAdesign,OutletWaterTempOFF)

  !   Assume Setpoint was met using free convection regime (pump ON and fan OFF)
      CTFanPower      = 0.0d0
      OutletWaterTemp = OutletWaterTempOFF

      IF(OutletWaterTempOFF > TempSetPoint)THEN
  !     Setpoint was not met (or free conv. not used), turn on cooling tower fan
        UAdesign          = SimpleTower(TowerNum)%HighSpeedTowerUA / SimpleTower(TowerNum)%NumCell
        AirFlowRate       = SimpleTower(TowerNum)%HighSpeedAirFlowRate / SimpleTower(TowerNum)%NumCell

        ! The fan power is for all cells operating
        FanPowerOn        = SimpleTower(TowerNum)%HighSpeedFanPower * NumCellON / SimpleTower(TowerNum)%NumCell

        Call SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRate,UAdesign, OutletWaterTemp)

        IF(OutletWaterTemp .LE. TempSetPoint)THEN
          IF(CapacityControl == CapacityControl_FanCycling .OR. OutletWaterTemp <= OWTLowerLimit)THEN
!           Setpoint was met with pump ON and fan ON, calculate run-time fraction
            FanModeFrac     = (TempSetPoint-OutletWaterTempOFF)/(OutletWaterTemp-OutletWaterTempOFF)
            CTFanPower      = FanModeFrac * FanPowerOn
            OutletWaterTemp = TempSetPoint
          ELSE
            !FluidBypass, fan runs at full speed for the entire time step
            FanModeFrac     = 1.0d0
            CTFanPower      = FanPowerOn
            BypassFlag      = 1
          ENDIF
        ELSE
!         Setpoint was not met, cooling tower ran at full capacity
          FanModeFrac     = 1.0d0
          CTFanPower      = FanPowerOn
          ! if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
          IF (NumCellON .lt. SimpleTower(TowerNum)%NumCell   &
             .and. (WaterMassFlowRate/(NumCellON+1)) .ge. WaterMassFlowRatePerCellMin) THEN
            NumCellON = NumCellON + 1
            WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellON
            IncrNumCellFlag = .true.
          END IF
        END IF
      ELSEIF(OutletWaterTempOFF < TempSetPoint)THEN
        ! Need to bypass in free convection cooling mode if bypass is allowed
        IF(CapacityControl == CapacityControl_FluidBypass)THEN
          IF(OutletWaterTempOFF > OWTLowerLimit)THEN
          BypassFlag  = 1
          ENDIF
        ENDIF
      END IF
    END DO

    ! Calculate bypass fraction since OWTLowerLimit < OutletWaterTemp < TempSetPoint.
    ! The iteraction ends when the numer of iteraction exceeds the limit or the difference
    !  between the new and old bypass fractions is less than the threshold.
    IF (BypassFlag == 1) THEN
      !Inlet water temperature lower than setpoint, assume 100% bypass, tower fan off
      IF(InletWaterTemp <= TempSetPoint)THEN
        CTFanPower = 0.0d0
        BypassFraction = 1.0d0
        SimpleTower(TowerNum)%BypassFraction = 1.0d0
        OutletWaterTemp = InletWaterTemp
      ELSE
          IF(ABS(InletWaterTemp - OutletWaterTemp)<=0.01d0) THEN
            ! Outlet temp is close enough to inlet temp, assume 100% bypass, tower fan off
            BypassFraction = 1.0d0
            SimpleTower(TowerNum)%BypassFraction = 1.0d0
            CTFanPower = 0.0d0
          ELSE
            BypassFraction = (TempSetPoint - OutletWaterTemp) / (InletWaterTemp - OutletWaterTemp)
            IF(BypassFraction >1.0d0 .OR. BypassFraction<0.0d0)THEN
              ! Bypass cannot meet setpoint, assume no bypass
              BypassFlag = 0
              BypassFraction = 0.0d0
              SimpleTower(TowerNum)%BypassFraction = 0.0d0
            ELSE
              NumIteration = 0
              BypassFractionPrev = BypassFraction
              OutletWaterTempPrev = OutletWaterTemp
              DO WHILE (NumIteration < MaxIteration)
                NumIteration = NumIteration + 1
                ! need to iterate for the new OutletWaterTemp while bypassing tower water
                Call SimSimpleTower(TowerNum, WaterMassFlowRatePerCell * (1.0d0-BypassFraction),   &
                   AirFlowRate, UAdesign, OutletWaterTemp)
                ! Calc new BypassFraction based on the new OutletWaterTemp
                IF(ABS(OutletWaterTemp - OWTLowerLimit)<=0.01d0)THEN
                  BypassFraction2 = BypassFraction
                  EXIT
                ELSEIF(OutletWaterTemp < OWTLowerLimit)THEN
                  ! Set OutletWaterTemp = OWTLowerLimit, and use linear interpolation to calculate the bypassFraction
                  BypassFraction2 = BypassFractionPrev - (BypassFractionPrev-BypassFraction)*(OutletWaterTempPrev-OWTLowerLimit) &
                                    /(OutletWaterTempPrev-OutletWaterTemp)
                  Call SimSimpleTower(TowerNum, WaterMassFlowRatePerCell * (1.0d0-BypassFraction2),   &
                     AirFlowRate, UAdesign, OutletWaterTemp)
                  IF (OutletWaterTemp < OWTLowerLimit) THEN
                    !Use previous iteraction values
                    BypassFraction2 = BypassFractionPrev
                    OutletWaterTemp = OutletWaterTempPrev
                  ENDIF
                  EXIT
                ELSE
                  BypassFraction2 = (TempSetPoint-OutletWaterTemp) / (InletWaterTemp - OutletWaterTemp)
                ENDIF

                ! Compare two BypassFraction to determine when to stop
                IF(ABS(BypassFraction2 - BypassFraction) <= BypassFractionThreshold) EXIT
                BypassFractionPrev = BypassFraction
                OutletWaterTempPrev = OutletWaterTemp
                BypassFraction = BypassFraction2
              END DO
              IF(NumIteration > MaxIteration) THEN
                CALL ShowWarningError('Cooling tower fluid bypass iteration ' &
                 //'exceeds maximum limit of '//MaxItChar//' for '//TRIM(SimpleTower(TowerNum)%Name))
              ENDIF
              SimpleTower(TowerNum)%BypassFraction = BypassFraction2
              ! may not meet TempSetPoint due to limit of tower outlet temp to OWTLowerLimit
              OutletWaterTemp = (1.0-BypassFraction2)*OutletWaterTemp + BypassFraction2*InletWaterTemp
            ENDIF
          ENDIF
      ENDIF
    ENDIF

    !output the fraction of the time step the fan is ON
    FanCyclingRatio = FanModeFrac
    ! output the number of cells operating
    SimpleTower(TowerNum)%NumCellON = NumCellON
    !Should this be water inlet node num?????
    CpWater =  GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                               Node(WaterInletNode)%Temp,                                   &
                               PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,         &
                               'CalcSingleSpeedTower')

    Qactual = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)
    AirFlowRateRatio = (AirFlowRate * SimpleTower(TowerNum)%NumCell) / SimpleTower(TowerNum)%HighSpeedAirFlowRate

RETURN
END SUBROUTINE CalcSingleSpeedTower

SUBROUTINE CalcTwoSpeedTower(TowerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !                      A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To simulate the operation of a cooling tower with a two-speed fan.

          ! METHODOLOGY EMPLOYED:
          !
          ! The cooling tower is modeled using effectiveness-NTU relationships for
          ! counterflow heat exchangers based on Merkel's theory.
          !
          ! The subroutine calculates the period of time required to meet a
          ! leaving water temperature setpoint. It assumes that part-load
          ! operation represents a linear interpolation of three steady-state regimes
          ! (high-speed fan operation, low-speed fan operation and free convection regime).
          ! Cyclic losses are neglected. The period of time required to meet the
          ! leaving water temperature setpoint is used to determine the required
          ! fan power and energy. Free convection regime is also modeled. This
          ! occures when the pump is operating and the fan is off. If free convection
          ! regime cooling is all that is required for a given time step, the leaving
          ! water temperature is allowed to fall below the leaving water temperature
          ! setpoint (free cooling). At times when the cooling tower fan is required,
          ! the leaving water temperature is at or above the setpoint.
          !
          ! A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
          ! or schedule, of the cooling tower. If the tower is OFF, outlet water
          ! temperature and flow rate are passed through the model from inlet node to
          ! outlet node without intervention (with the exception of free convection
          ! where water temperature is allowed to float below the outlet water set
          ! point). Reports are also updated with fan power and fan energy being zero.
          !
          ! When the RunFlag indicates an ON condition for the cooling tower, the
          ! mass flow rate and water temperature are read from the inlet node of the
          ! cooling tower (water-side). The outdoor air wet-bulb temperature is used
          ! as the entering condition to the cooling tower (air-side). Input deck
          ! parameters are read for the free convection regime (pump ON and fan OFF)
          ! and a leaving water temperature is calculated. If the leaving water temperature
          ! is at or below the setpoint, the calculated leaving water temperature is
          ! placed on the outlet node and no fan power is used. If the calculated leaving
          ! water temperature is above the setpoint, the cooling tower fan is turned on
          ! and parameters for low fan speed are used to again calculate the leaving
          ! water temperature. If the calculated leaving water temperature is
          ! below the setpoint, a fan run-time fraction (FanModeFrac) is calculated and
          ! used to determine fan power. The leaving water temperature setpoint is placed
          ! on the outlet node. If the calculated leaving water temperature is at or above
          ! the setpoint, the cooling tower fan is turned on 'high speed' and the routine is
          ! repeated. If the calculated leaving water temperature is below the setpoint,
          ! a fan run-time fraction is calculated for the second stage fan and fan power
          ! is calculated as FanModeFrac*HighSpeedFanPower+(1-FanModeFrac)*LowSpeedFanPower.
          ! If the calculated leaving water temperature is above the leaving water temp.
          ! setpoint, the calculated leaving water temperature is placed on the outlet
          ! node and the fan runs at full power (High Speed Fan Power). Water mass flow
          ! rate is passed from inlet node to outlet node with no intervention.
          !
          ! If a tower has multiple cells, the specified inputs of or the autosized capacity
          !  and air/water flow rates are for the entire tower. The number of cells to operate
          !  is first determined based on the user entered minimal and maximal water flow fractions
          !  per cell. If the loads are not met, more cells (if available) will operate to meet
          !  the loads. Each cell operates in same way - same fan speed etc.
          !
          ! REFERENCES:
          ! ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.

          ! USE STATEMENTS:
  USE DataPlant, ONLY: PlantLoop, SingleSetPoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: TowerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: AirFlowRate
  REAL(r64)              :: UAdesign            ! UA value at design conditions (entered by user) [W/C]
  REAL(r64)              :: OutletWaterTempOFF
  REAL(r64)              :: OutletWaterTemp1stStage
  REAL(r64)              :: OutletWaterTemp2ndStage
  REAL(r64)              :: FanModeFrac
  REAL(r64)              :: designWaterFlowRate
  REAL(r64)              :: FanPowerLow
  REAL(r64)              :: FanPowerHigh
  REAL(r64)              :: CpWater
  REAL(r64)              :: TempSetPoint

  INTEGER :: LoopNum
  INTEGER :: LoopSideNum

  INTEGER :: SpeedSel = 0

 !Added variables for multicell
  REAL(r64)              :: WaterMassFlowRatePerCellMin
  REAL(r64)              :: WaterMassFlowRatePerCellMax
  INTEGER                :: NumCellMin = 0
  INTEGER                :: NumCellMax = 0
  INTEGER                :: NumCellON = 0
  REAL(r64)              :: WaterMassFlowRatePerCell
  LOGICAL                :: IncrNumCellFlag                   ! determine if yes or no we increase the number of cells


          !set inlet and outlet nodes

    WaterInletNode      = SimpleTower(TowerNum)%WaterInletNodeNum
    WaterOutletNode     = SimpleTower(TowerNum)%WaterOutletNodeNum
    Qactual             = 0.0d0
    CTFanPower          = 0.0d0
    OutletWaterTemp     = Node(WaterInletNode)%Temp
    LoopNum             = SimpleTower(TowerNum)%LoopNum
    LoopSideNum         = SimpleTower(TowerNum)%LoopSideNum
    SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
    CASE (SingleSetPoint)
      IF (SimpleTower(TowerNum)%SetpointIsOnOutlet) THEN
        TempSetPoint     = Node(WaterOutletNode)%TempSetpoint
      ELSE
        TempSetPoint     = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpoint
      ENDIF
    CASE (DualSetPointDeadBand)
      IF (SimpleTower(TowerNum)%SetpointIsOnOutlet) THEN
        TempSetPoint     = Node(WaterOutletNode)%TempSetpointHi
      ELSE
        TempSetPoint     = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpointHi
      ENDIF
    END SELECT

  ! Do not RETURN here if flow rate is less than SmallMassFlow. Check basin heater and then RETURN.
    IF(PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock .EQ. 0)RETURN
  ! MassFlowTolerance is a parameter to indicate a no flow condition
    IF(WaterMassFlowRate.LE.MassFlowTolerance)THEN
      CALL CalcBasinHeaterPower(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff,&
                                SimpleTower(TowerNum)%BasinHeaterSchedulePtr,&
                                SimpleTower(TowerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
      RETURN
    ENDIF

    ! Added for multi-cell. Determine the number of cells operating
    IF (SimpleTower(TowerNum)%DesWaterMassFlowRate > 0.0D0) THEN
      WaterMassFlowRatePerCellMin = SimpleTower(TowerNum)%DesWaterMassFlowRate * SimpleTower(TowerNum)%MinFracFlowRate /   &
         SimpleTower(TowerNum)%NumCell
      WaterMassFlowRatePerCellMax = SimpleTower(TowerNum)%DesWaterMassFlowRate * SimpleTower(TowerNum)%MaxFracFlowRate /   &
         SimpleTower(TowerNum)%NumCell

      !round it up to the nearest integer
      NumCellMin = MIN(INT((WaterMassFlowRate / WaterMassFlowRatePerCellMax)+.9999d0),SimpleTower(TowerNum)%NumCell)
      NumCellMax = MIN(INT((WaterMassFlowRate / WaterMassFlowRatePerCellMin)+.9999d0),SimpleTower(TowerNum)%NumCell)
    ENDIF

    ! cap min at 1
    IF(NumCellMin <= 0)NumCellMin = 1
    IF(NumCellMax <= 0)NumCellMax = 1

    IF(SimpleTower(TowerNum)%CellCtrl_Num == CellCtrl_MinCell)THEN
      NumCellON = NumCellMin
    ELSE
      NumCellON = NumCellMax
    END IF

    SimpleTower(TowerNum)%NumCellON = NumCellON
    WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellON

    IncrNumCellFlag = .true.

    DO WHILE (IncrNumCellFlag)
      IncrNumCellFlag = .false.

      !set local variable for tower
      UAdesign                = SimpleTower(TowerNum)%FreeConvTowerUA / SimpleTower(TowerNum)%NumCell  ! where is NumCellOn?
      AirFlowRate             = SimpleTower(TowerNum)%FreeConvAirFlowRate / SimpleTower(TowerNum)%NumCell
      DesignWaterFlowRate     = SimpleTower(TowerNum)%DesignWaterFlowRate ! ??useless subroutine variable??
      OutletWaterTempOFF      = Node(WaterInletNode)%Temp
      WaterMassFlowRate       = Node(WaterInletNode)%MassFlowRate
      OutletWaterTemp1stStage = OutletWaterTemp
      OutletWaterTemp2ndStage = OutletWaterTemp
      FanModeFrac             = 0.0d0

      Call SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRate,UAdesign,OutletWaterTempOFF)

  !     Setpoint was met using free convection regime (pump ON and fan OFF)
        CTFanPower    = 0.0d0
        OutletWaterTemp = OutletWaterTempOFF
        SpeedSel = 0

      IF(OutletWaterTempOFF .GT. TempSetPoint)THEN
  !     Setpoint was not met (or free conv. not used),turn on cooling tower 1st stage fan
        UAdesign        = SimpleTower(TowerNum)%LowSpeedTowerUA / SimpleTower(TowerNum)%NumCell
        AirFlowRate     = SimpleTower(TowerNum)%LowSpeedAirFlowRate / SimpleTower(TowerNum)%NumCell
        FanPowerLow     = SimpleTower(TowerNum)%LowSpeedFanPower * NumCellON / SimpleTower(TowerNum)%NumCell

        Call SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRate,UAdesign,OutletWaterTemp1stStage)

        IF(OutletWaterTemp1stStage .LE. TempSetPoint)THEN
!         Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
          FanModeFrac = (TempSetPoint-OutletWaterTempOFF)/(OutletWaterTemp1stStage-OutletWaterTempOFF)
          CTFanPower      = FanModeFrac * FanPowerLow
          OutletWaterTemp = TempSetPoint
          Qactual         = Qactual * FanModeFrac
          SpeedSel = 1
        ELSE
!         Setpoint was not met, turn on cooling tower 2nd stage fan
          UAdesign          = SimpleTower(TowerNum)%HighSpeedTowerUA / SimpleTower(TowerNum)%NumCell
          AirFlowRate       = SimpleTower(TowerNum)%HighSpeedAirFlowRate / SimpleTower(TowerNum)%NumCell
          FanPowerHigh      = SimpleTower(TowerNum)%HighSpeedFanPower * NumCellON / SimpleTower(TowerNum)%NumCell

          Call SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRate,UAdesign,OutletWaterTemp2ndStage)

          IF((OutletWaterTemp2ndStage .LE. TempSetPoint).AND. UAdesign .GT. 0.0d0)THEN
!           Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
            FanModeFrac     = (TempSetPoint-OutletWaterTemp1stStage)/(OutletWaterTemp2ndStage-OutletWaterTemp1stStage)
            CTFanPower      = (FanModeFrac * FanPowerHigh) + (1.d0-FanModeFrac)*FanPowerLow
            OutletWaterTemp = TempSetPoint
            SpeedSel = 2
          ELSE
!           Setpoint was not met, cooling tower ran at full capacity
            OutletWaterTemp = OutletWaterTemp2ndStage
            CTFanPower      = FanPowerHigh
            SpeedSel = 2
            FanModeFrac = 1.0d0
            ! if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
            IF (NumCellON .lt. SimpleTower(TowerNum)%NumCell .and.   &
               (WaterMassFlowRate/(NumCellON+1)) .ge. WaterMassFlowRatePerCellMin)THEN
            NumCellON = NumCellON + 1
            WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellON
            IncrNumCellFlag = .true.
            END IF
          END IF

        END IF

      END IF
    END DO

      !output the fraction of the time step the fan is ON
      FanCyclingRatio = FanModeFrac
      SimpleTower(TowerNum)%SpeedSelected = SpeedSel
      SimpleTower(TowerNum)%NumCellON = NumCellON

      CpWater  = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                               Node(WaterInletNode)%Temp,                                   &
                               PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,         &
                               'CalcTwoSpeedTower')
      Qactual = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)
      AirFlowRateRatio = (AirFlowRate * SimpleTower(TowerNum)%NumCell) / SimpleTower(TowerNum)%HighSpeedAirFlowRate

RETURN
END SUBROUTINE CalcTwoSpeedTower


SUBROUTINE CalcMerkelVariableSpeedTower(TowerNum, MyLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.Griffith
          !       DATE WRITTEN   August 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate varialble speed tower model using Merkel's theory with UA adjustments developed by Scheier

          ! METHODOLOGY EMPLOYED:
          ! Find a fan speed that operates the tower to meet MyLoad

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager,    ONLY: CurveValue
  USE DataPlant,   ONLY : SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance
  USE General, ONLY: SolveRegulaFalsi, RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: TowerNum
  REAL(r64), INTENT(INOUT) :: MyLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: DesignWetBlub = 25.56d0 ! tower outdoor air entering wetblub for design [C]
  INTEGER,   PARAMETER  :: MaxIte    = 500     ! Maximum number of iterations for solver
  REAL(r64), PARAMETER  :: Acc       = 1.d-3   ! Accuracy of solver result
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), DIMENSION(8)  :: Par                 ! Parameter array passed to solver
  INTEGER                  :: SolFla              ! Flag of solver
  REAL(r64) :: CpWater
  INTEGER   :: LoopNum
  INTEGER   :: LoopSideNum
  REAL(r64) :: TempSetpoint
  REAL(r64) :: WaterMassFlowRatePerCellMin
  REAL(r64) :: WaterMassFlowRatePerCellMax
  INTEGER   :: NumCellMin
  INTEGER   :: NumCellMax
  INTEGER   :: NumCellOn
  REAL(r64) :: WaterMassFlowRatePerCell
  REAL(r64) :: UAdesignPerCell
  REAL(r64) :: AirFlowRatePerCell
  REAL(r64) :: OutletWaterTempOff
  REAL(r64) :: FreeConvQdot
  REAL(r64) :: WaterFlowRateRatio
  REAL(r64) :: UAwetbulbAdjFac
  REAL(r64) :: UAairFlowAdjFac
  REAL(r64) :: UAWaterFlowAdjFac
  REAL(r64) :: UAadjustedPerCell
  REAL(r64) :: FullSpeedFanQdot
  LOGICAL   :: IncrNumCellFlag
  REAL(r64) :: MinSpeedFanQdot
  REAL(r64) :: FanPowerAdjustFac




  WaterInletNode      = SimpleTower(TowerNum)%WaterInletNodeNum
  CpWater =  GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                               Node(WaterInletNode)%Temp,                                   &
                               PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,         &
                               'CalcMerkelVariableSpeedTower')
  WaterOutletNode     = SimpleTower(TowerNum)%WaterOutletNodeNum
  Qactual             = 0.0d0
  CTFanPower          = 0.0d0
  OutletWaterTemp     = Node(WaterInletNode)%Temp
  LoopNum             = SimpleTower(TowerNum)%LoopNum
  LoopSideNum         = SimpleTower(TowerNum)%LoopSideNum
  SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetPoint)
    IF (SimpleTower(TowerNum)%SetpointIsOnOutlet) THEN
      TempSetPoint     = Node(WaterOutletNode)%TempSetpoint
    ELSE
      TempSetPoint     = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpoint
    ENDIF
  CASE (DualSetPointDeadBand)
    IF (SimpleTower(TowerNum)%SetpointIsOnOutlet) THEN
      TempSetPoint     = Node(WaterOutletNode)%TempSetpointHi
    ELSE
      TempSetPoint     = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpointHi
    ENDIF
  END SELECT

    ! Added for multi-cell. Determine the number of cells operating
    IF (SimpleTower(TowerNum)%DesWaterMassFlowRate > 0.0D0) THEN
      WaterMassFlowRatePerCellMin = SimpleTower(TowerNum)%DesWaterMassFlowRate *   &
         SimpleTower(TowerNum)%MinFracFlowRate / SimpleTower(TowerNum)%NumCell
      WaterMassFlowRatePerCellMax = SimpleTower(TowerNum)%DesWaterMassFlowRate *   &
         SimpleTower(TowerNum)%MaxFracFlowRate / SimpleTower(TowerNum)%NumCell

      !round it up to the nearest integer
      NumCellMin = MIN(INT((WaterMassFlowRate / WaterMassFlowRatePerCellMax)+.9999d0),SimpleTower(TowerNum)%NumCell)
      NumCellMax = MIN(INT((WaterMassFlowRate / WaterMassFlowRatePerCellMin)+.9999d0),SimpleTower(TowerNum)%NumCell)
    ENDIF

    ! cap min at 1
    IF(NumCellMin <= 0)NumCellMin = 1
    IF(NumCellMax <= 0)NumCellMax = 1

    IF(SimpleTower(TowerNum)%CellCtrl_Num == CellCtrl_MinCell)THEN
      NumCellON = NumCellMin
    ELSE
      NumCellON = NumCellMax
    END IF

    SimpleTower(TowerNum)%NumCellON = NumCellON
    WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellON
  ! MassFlowTolerance is a parameter to indicate a no flow condition
    IF(WaterMassFlowRate.LE.MassFlowTolerance  .OR. (MyLoad > SmallLoad)) THEN
      ! for multiple cells, we assume that it's a common bassin
      CALL CalcBasinHeaterPower(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff,&
                                SimpleTower(TowerNum)%BasinHeaterSchedulePtr,&
                                SimpleTower(TowerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
      RETURN
    ENDIF



    ! first find free convection cooling rate
    UAdesignPerCell     = SimpleTower(TowerNum)%FreeConvTowerUA   / SimpleTower(TowerNum)%NumCell
    AirFlowRatePerCell  = SimpleTower(TowerNum)%FreeConvAirFlowRate  / SimpleTower(TowerNum)%NumCell
    OutletWaterTempOFF      = Node(WaterInletNode)%Temp
    WaterMassFlowRate       = Node(WaterInletNode)%MassFlowRate
    Call SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRatePerCell,UAdesignPerCell,OutletWaterTempOFF)

    FreeConvQdot = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTempOFF)
    CTFanPower          = 0.0d0
    IF (ABS(MyLoad) <= FreeConvQdot) THEN ! can meet load with free convection and fan off
      OutletWaterTemp = OutletWaterTempOFF
      AirFlowRateRatio = 0.d0
      Qactual = FreeConvQdot
      CALL CalcBasinHeaterPower(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff,&
                                SimpleTower(TowerNum)%BasinHeaterSchedulePtr,&
                                SimpleTower(TowerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)

      RETURN
    ENDIF

    ! next find full fan speed cooling rate
    UAdesignPerCell     = SimpleTower(TowerNum)%HighSpeedTowerUA / SimpleTower(TowerNum)%NumCell
    AirFlowRatePerCell  = SimpleTower(TowerNum)%HighSpeedAirFlowRate / SimpleTower(TowerNum)%NumCell
    AirFlowRateRatio = 1.d0
    WaterFlowRateRatio = WaterMassFlowRatePerCell / SimpleTower(TowerNum)%DesWaterMassFlowRatePerCell
    UAwetbulbAdjFac =  CurveValue(SimpleTower(TowerNum)%UAModFuncWetbulbDiffCurvePtr, &
                                  (DesignWetBlub - SimpleTowerInlet(TowerNum)%AirWetBulb))
    UAairflowAdjFac =  CurveValue(SimpleTower(TowerNum)%UAModFuncAirFlowRatioCurvePtr, AirFlowRateRatio)
    UAwaterflowAdjFac = CurveValue(SimpleTower(TowerNum)%UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio)
    UAadjustedPerCell = UAdesignPerCell*UAwetbulbAdjFac*UAairflowAdjFac*UAwaterflowAdjFac
    CALL SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRatePerCell,UAadjustedPerCell, OutletWaterTemp)
    FullSpeedFanQdot = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)

    IF (FullSpeedFanQdot <= ABS(MyLoad)) THEN ! full speed is what we want.


      IF ((FullSpeedFanQdot+ SmallLoad) < ABS(MyLoad) .and. (NumCellON < SimpleTower(TowerNum)%NumCell) &
          .AND.   ((WaterMassFlowRate/(NumCellON+1)) .ge. WaterMassFlowRatePerCellMin)) THEN
        ! If full fan and not meeting setpoint, then increase number of cells until all are used or load is satisfied
        IncrNumCellFlag = .TRUE. ! set value to true to enter in the loop
        DO WHILE (IncrNumCellFlag)
            NumCellON = NumCellON + 1
            SimpleTower(TowerNum)%NumCellON = NumCellON
            WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellON
            WaterFlowRateRatio = WaterMassFlowRatePerCell / SimpleTower(TowerNum)%DesWaterMassFlowRatePerCell
            UAwaterflowAdjFac = CurveValue(SimpleTower(TowerNum)%UAModFuncWaterFlowRatioCurvePtr, WaterFlowRateRatio)
            UAadjustedPerCell = UAdesignPerCell*UAwetbulbAdjFac*UAairflowAdjFac*UAwaterflowAdjFac
            CALL SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRatePerCell,UAadjustedPerCell, OutletWaterTemp)
            IF ((FullSpeedFanQdot+ SmallLoad) < ABS(MyLoad) .and. (NumCellON < SimpleTower(TowerNum)%NumCell) &
                      .AND.   ((WaterMassFlowRate/(NumCellON+1)) .ge. WaterMassFlowRatePerCellMin)) THEN
              IncrNumCellFlag = .TRUE.
            ELSE
              IncrNumCellFlag = .FALSE.
            ENDIF
        ENDDO
        FullSpeedFanQdot = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)
      ENDIF
      Qactual = FullSpeedFanQdot
      CALL CalcBasinHeaterPower(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff,&
                                  SimpleTower(TowerNum)%BasinHeaterSchedulePtr,&
                                  SimpleTower(TowerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
      ! now calculate fan power
      FanPowerAdjustFac = CurveValue(SimpleTower(TowerNum)%FanPowerfAirFlowCurve, AirFlowRateRatio)
      CTFanPower        = SimpleTower(TowerNum)%HighSpeedFanPower * FanPowerAdjustFac * NumCellON /  &
                                                            SimpleTower(TowerNum)%NumCell

      RETURN

    ENDIF

    ! next find minimum air flow ratio cooling rate
    AirFlowRateRatio = SimpleTower(TowerNum)%MinimumVSAirFlowFrac
    AirFlowRatePerCell =AirFlowRateRatio * SimpleTower(TowerNum)%HighSpeedAirFlowRate / SimpleTower(TowerNum)%NumCell
    UAairflowAdjFac =  CurveValue(SimpleTower(TowerNum)%UAModFuncAirFlowRatioCurvePtr, AirFlowRateRatio)
    UAadjustedPerCell = UAdesignPerCell*UAwetbulbAdjFac*UAairflowAdjFac*UAwaterflowAdjFac
    CALL SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRatePerCell,UAadjustedPerCell, OutletWaterTemp)
    MinSpeedFanQdot = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)

    IF (ABS(MyLoad) <= MinSpeedFanQdot) THEN ! min fan speed already exceeds load)
      Qactual = MinSpeedFanQdot
      CALL CalcBasinHeaterPower(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff,&
                                SimpleTower(TowerNum)%BasinHeaterSchedulePtr,&
                                SimpleTower(TowerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
      ! now calculate fan power
      FanPowerAdjustFac = CurveValue(SimpleTower(TowerNum)%FanPowerfAirFlowCurve, AirFlowRateRatio)
      CTFanPower        = SimpleTower(TowerNum)%HighSpeedFanPower * FanPowerAdjustFac * NumCellON /  &
                                                            SimpleTower(TowerNum)%NumCell
      RETURN
    ENDIF

    IF ((MinSpeedFanQdot < ABS(MyLoad)) .AND. (ABS(MyLoad) < FullSpeedFanQdot)) THEN
      ! load can be refined by modulationg fan speed, call regulafalsi


      Par(1) = REAL(TowerNum, r64)
      Par(2) = MyLoad
      Par(3) = WaterMassFlowRatePerCell
      Par(4) = UAdesignPerCell
      Par(5) = UAwetbulbAdjFac
      Par(6) = UAwaterflowAdjFac
      Par(7) = CpWater
      Par(8) = WaterMassFlowRate

      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, AirFlowRateRatio, VSMerkelResidual, &
                             SimpleTower(TowerNum)%MinimumVSAirFlowFrac, 1.0d0, Par)

      IF (SolFla == -1) THEN
        IF(.NOT. WarmupFlag)THEN
          IF (SimpleTower(TowerNum)%VSMerkelAFRErrorIter < 1) THEN
            SimpleTower(TowerNum)%VSMerkelAFRErrorIter = SimpleTower(TowerNum)%VSMerkelAFRErrorIter +1
            CALL ShowWarningError(TRIM(cCoolingTower_VariableSpeedMerkel)// &
                  ' - Iteration limit exceeded calculating variable speed fan ratio for unit = ' &
                  //TRIM(SimpleTower(TowerNum)%Name ) )
            CALL ShowContinueError('Estimated air flow ratio  = '// &
                         RoundSigDigits((ABS(MyLoad) - MinSpeedFanQdot)/(FullSpeedFanQdot - MinSpeedFanQdot ), 4 ) )
            CALL ShowContinueError('Calculated air flow ratio = '//roundSigDigits(AirFlowRateRatio, 4) )
            CALL ShowContinueErrorTimeStamp('The calculated air flow ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd(TRIM(cCoolingTower_VariableSpeedMerkel)//' "' &
                //TRIM(SimpleTower(TowerNum)%Name )//'" - Iteration limit exceeded calculating air flow ratio error continues.' &
                 //' air flow ratio statistics follow.',   &
                    SimpleTower(TowerNum)%VSMerkelAFRErrorIter, AirFlowRateRatio, AirFlowRateRatio)
        ENDIF
      ELSE IF (SolFla == -2) THEN
        AirFlowRateRatio = (ABS(MyLoad) - MinSpeedFanQdot)/(FullSpeedFanQdot - MinSpeedFanQdot )
        IF(.NOT. WarmupFlag)THEN
          IF ( SimpleTower(TowerNum)%VSMerkelAFRErrorFail < 1) THEN
            SimpleTower(TowerNum)%VSMerkelAFRErrorFail = SimpleTower(TowerNum)%VSMerkelAFRErrorFail +1
            CALL ShowWarningError(TRIM(cCoolingTower_VariableSpeedMerkel)// &
                  ' - solver failed calculating variable speed fan ratio for unit = ' &
                  //TRIM(SimpleTower(TowerNum)%Name ) )
            CALL ShowContinueError('Estimated air flow ratio  = '//  RoundSigDigits(AirFlowRateRatio, 4 ) )
            CALL ShowContinueErrorTimeStamp('The estimated air flow ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd(TRIM(cCoolingTower_VariableSpeedMerkel)//' "' &
                //TRIM(SimpleTower(TowerNum)%Name )//'" - solver failed calculating air flow ratio error continues.' &
                 //' air flow ratio statistics follow.',   &
                    SimpleTower(TowerNum)%VSMerkelAFRErrorFail, AirFlowRateRatio, AirFlowRateRatio)
        ENDIF
      ENDIF

      ! now rerun to get peformance with AirFlowRateRatio
      AirFlowRatePerCell = AirFlowRateRatio * SimpleTower(TowerNum)%HighSpeedAirFlowRate / SimpleTower(TowerNum)%NumCell

      UAairflowAdjFac =  CurveValue(SimpleTower(TowerNum)%UAModFuncAirFlowRatioCurvePtr, AirFlowRateRatio)
      UAadjustedPerCell = UAdesignPerCell*UAwetbulbAdjFac*UAairflowAdjFac*UAwaterflowAdjFac

      CALL SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRatePerCell,UAadjustedPerCell, OutletWaterTemp)
      Qactual = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)
      CALL CalcBasinHeaterPower(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff,&
                                SimpleTower(TowerNum)%BasinHeaterSchedulePtr,&
                                SimpleTower(TowerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)

      ! now calculate fan power
      FanPowerAdjustFac = CurveValue(SimpleTower(TowerNum)%FanPowerfAirFlowCurve, AirFlowRateRatio)
      CTFanPower        = SimpleTower(TowerNum)%HighSpeedFanPower * FanPowerAdjustFac * NumCellON /  &
                                                            SimpleTower(TowerNum)%NumCell

    ENDIF


  RETURN

END SUBROUTINE CalcMerkelVariableSpeedTower

FUNCTION VSMerkelResidual(AirFlowRateRatio, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager,  ONLY : CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: AirFlowRateRatio ! fan speed ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = Tower number
                                                  ! par(2) =MyLoad [W] , negative is cooling
                                                  ! par(3) = water mass flow per cell
                                                  ! par(4) = Design UA per cell
                                                  ! par(5) = UA adjust factor for wetbulb
                                                  ! par(6) = UA adjust factor for water flow rate
                                                  ! par(7) = specific heat of water at inlet temp
                                                  ! par(8) = water mass flow rate, total [kg/s]
  REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: TowerNum
  REAL(r64) :: WaterMassFlowRatePerCell
  REAL(r64) :: TargetLoad
  REAL(r64) :: UAdesignPerCell
  REAL(r64) :: UAwetbulbAdjFac
  REAL(r64) :: UAairflowAdjFac
  REAL(r64) :: UAwaterflowAdjFac
  REAL(r64) :: CpWater
  REAL(r64) :: TotalWaterMassFlowRate
  REAL(r64) :: AirFlowRatePerCell
  REAL(r64) :: UAadjustedPerCell
  REAL(r64) :: Qdot
  REAL(r64) :: OutletWaterTempTrial

  TowerNum                 = INT(Par(1))
  TargetLoad               = Par(2)
  WaterMassFlowRatePerCell = Par(3)
  UAdesignPerCell          = Par(4)
  UAwetbulbAdjFac          = Par(5)
  UAwaterflowAdjFac        = Par(6)
  CpWater                  = Par(7)
  TotalWaterMassFlowRate   = Par(8)

  AirFlowRatePerCell = AirFlowRateRatio * SimpleTower(TowerNum)%HighSpeedAirFlowRate / SimpleTower(TowerNum)%NumCell

  UAairflowAdjFac =  CurveValue(SimpleTower(TowerNum)%UAModFuncAirFlowRatioCurvePtr, AirFlowRateRatio)
  UAadjustedPerCell = UAdesignPerCell*UAwetbulbAdjFac*UAairflowAdjFac*UAwaterflowAdjFac

  CALL SimSimpleTower(TowerNum,WaterMassFlowRatePerCell,AirFlowRatePerCell,UAadjustedPerCell, OutletWaterTempTrial)

  Qdot = TotalWaterMassFlowRate * CpWater * (Node(SimpleTower(TowerNum)%WaterInletNodeNum)%Temp - OutletWaterTempTrial)

  Residuum =  ABS(TargetLoad) - Qdot

  RETURN

END FUNCTION VSMerkelResidual




SUBROUTINE CalcVariableSpeedTower(TowerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   Feb 2005
          !       MODIFIED       A Flament, July 2010, added multi-cell capability for the 3 types of cooling tower
          !                      B Griffith, general fluid props
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To simulate the operation of a variable-speed fan cooling tower.

          ! METHODOLOGY EMPLOYED:
          !
          ! For each simulation time step, a desired range temperature (Twater,inlet-Twater,setpoint) and desired approach
          ! temperature (Twater,setpoint-Tair,WB) is calculated which meets the outlet water temperature setpoint. This
          ! desired range and approach temperature also provides a balance point for the empirical model where:
          !
          ! Tair,WB + Twater,range + Tapproach = Node(WaterInletNode)%Temp
          !
          ! Calculation of water outlet temperature uses one of the following equations:
          !
          ! Twater,outlet = Tair,WB + Tapproach          (1)  or
          ! Twater,outlet = Twater,inlet - Twater,range  (2)
          !
          ! If a solution (or balance) is found, these 2 calculation methods are equal. Equation 2 is used to calculate
          ! the outlet water temperature in the free convection regime and at the minimum or maximum fan speed so that
          ! if a solution is not reached, the outlet water temperature is approximately equal to the inlet water temperature
          ! and the tower fan must be varied to meet the setpoint. Equation 1 is used when the fan speed is varied between
          ! the minimum and maximum fan speed to meet the outlet water temperature setpoint.
          !
          ! The outlet water temperature in the free convection regime is first calculated to see if the setpoint is met.
          ! If the setpoint is met, the fan is OFF and the outlet water temperature is allowed to float below the set
          ! point temperature. If the setpoint is not met, the outlet water temperature is re-calculated at the minimum
          ! fan speed. If the setpoint is met, the fan is cycled to exactly meet the outlet water temperature setpoint.
          ! If the setpoint is not met at the minimum fan speed, the outlet water temperature is re-calculated at the
          ! maximum fan speed. If the setpoint at the maximum fan speed is not met, the fan runs at maximum speed the
          ! entire time step. If the setpoint is met at the maximum fan speed, the fan speed is varied to meet the setpoint.
          !
          ! If a tower has multiple cells, the specified inputs of or the autosized capacity
          !  and air/water flow rates are for the entire tower. The number of cells to operate
          !  is first determined based on the user entered minimal and maximal water flow fractions
          !  per cell. If the loads are not met, more cells (if available) will operate to meet
          !  the loads. Inside each cell, the fan speed varies in the same way.
          !
          ! REFERENCES:
          !
          ! Benton, D.J., Bowmand, C.F., Hydeman, M., Miller, P.,
          ! "An Improved Cooling Tower Algorithm for the CoolToolsTM Simulation Model".
          ! ASHRAE Transactions 2002, V. 108, Pt. 1.
          !
          ! York International Corporation, "YORKcalcTM Software, Chiller-Plant Energy-Estimating Program",
          ! Form 160.00-SG2 (0502). © 2002.

          ! USE STATEMENTS:
  USE General,         ONLY: SolveRegulaFalsi
  USE CurveManager,    ONLY: CurveValue
  USE DataEnvironment, ONLY: EnvironmentName, CurMnDy
  USE DataGlobals,     ONLY: CurrentTime
  USE General,         ONLY: CreateSysTimeIntervalString
  USE DataPlant,       ONLY: PlantLoop, SingleSetPoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: TowerNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F5.2)'
  CHARACTER(len=*), PARAMETER :: OutputFormat2 ='(F8.5)'
  INTEGER, PARAMETER          :: MaxIte = 500         ! Maximum number of iterations
  REAL(r64), PARAMETER :: Acc =  0.0001d0      ! Accuracy of result

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: OutletWaterTempOFF   ! Outlet water temperature with fan OFF (C)
  REAL(r64)              :: OutletWaterTempON    ! Outlet water temperature with fan ON at maximum fan speed (C)
  REAL(r64)              :: OutletWaterTempMIN   ! Outlet water temperature with fan at minimum speed (C)
  REAL(r64)              :: CpWater              ! Specific heat of water
  REAL(r64)              :: TempSetPoint         ! Outlet water temperature setpoint (C)
  REAL(r64)              :: FanCurveValue        ! Output of fan power as a func of air flow rate ratio curve
  REAL(r64)              :: AirDensity           ! Density of air [kg/m3]
  REAL(r64)              :: AirMassFlowRate      ! Mass flow rate of air [kg/s]
  REAL(r64)              :: InletAirEnthalpy     ! Enthalpy of entering moist air [J/kg]
  INTEGER                :: SolFla               ! Flag of solver
  REAL(r64), DIMENSION(6)     :: Par                  ! Parameter array for regula falsi solver
  REAL(r64)              :: Twb                  ! inlet air wet-bulb temperature
  REAL(r64)              :: TwbCapped            ! inlet air wet-bulb temp passed to VS tower model
  REAL(r64)              :: Tr                   ! range temperature
  REAL(r64)              :: TrCapped             ! range temp passed to VS tower model
  REAL(r64)              :: Ta                   ! approach temperature
  REAL(r64)              :: TaCapped             ! approach temp passed to VS tower model
  REAL(r64)              :: WaterFlowRateRatio   ! Water flow rate ratio
  REAL(r64)              :: WaterFlowRateRatioCapped ! Water flow rate ratio passed to VS tower model
  REAL(r64)              :: WaterDensity             ! density of inlet water
  REAL(r64)              :: FreeConvectionCapFrac    ! fraction of tower capacity in free convection
  REAL(r64)              :: FlowFraction         ! liquid to gas (L/G) ratio for cooling tower
  CHARACTER(len=6)       :: OutputChar           ! character string used for warning messages
  CHARACTER(len=6)       :: OutputChar2          ! character string used for warning messages
  CHARACTER(len=6)       :: OutputChar3          ! character string used for warning messages
  CHARACTER(len=6)       :: OutputChar4          ! character string used for warning messages
  CHARACTER(len=6)       :: OutputChar5          ! character string used for warning messages
  REAL(r64),SAVE  :: TimeStepSysLast=0.0d0      ! last system time step (used to check for downshifting)
  REAL(r64)  :: CurrentEndTime       ! end time of time step for current simulation time step
  REAL(r64),SAVE  :: CurrentEndTimeLast=0.0d0   ! end time of time step for last simulation time step
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum

   !Added variables for multicell
  REAL(r64)              :: WaterMassFlowRatePerCellMin
  REAL(r64)              :: WaterMassFlowRatePerCellMax
  INTEGER                :: NumCellMin = 0
  INTEGER                :: NumCellMax = 0
  INTEGER                :: NumCellON = 0
  REAL(r64)              :: WaterMassFlowRatePerCell
  LOGICAL                :: IncrNumCellFlag

   ! Added for multi-cell. Determine the number of cells operating
    IF (SimpleTower(TowerNum)%DesWaterMassFlowRate > 0.0D0) THEN
      WaterMassFlowRatePerCellMin = SimpleTower(TowerNum)%DesWaterMassFlowRate * SimpleTower(TowerNum)%MinFracFlowRate /   &
         SimpleTower(TowerNum)%NumCell
      WaterMassFlowRatePerCellMax = SimpleTower(TowerNum)%DesWaterMassFlowRate * SimpleTower(TowerNum)%MaxFracFlowRate /   &
         SimpleTower(TowerNum)%NumCell

      !round it up to the nearest integer
      NumCellMin = MIN(INT((WaterMassFlowRate / WaterMassFlowRatePerCellMax)+.9999d0),SimpleTower(TowerNum)%NumCell)
      NumCellMax = MIN(INT((WaterMassFlowRate / WaterMassFlowRatePerCellMin)+.9999d0),SimpleTower(TowerNum)%NumCell)
    ENDIF

    ! cap min at 1
    IF(NumCellMin <= 0)NumCellMin = 1
    IF(NumCellMax <= 0)NumCellMax = 1

    IF(SimpleTower(TowerNum)%CellCtrl_Num == CellCtrl_MinCell)THEN
      NumCellON = NumCellMin
    ELSE
      NumCellON = NumCellMax
    END IF

    SimpleTower(TowerNum)%NumCellON = NumCellON
    WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellON

  ! Set inlet and outlet nodes and initialize subroutine variables

    WaterInletNode     = SimpleTower(TowerNum)%WaterInletNodeNum
    WaterOutletNode    = SimpleTower(TowerNum)%WaterOutletNodeNum
    Qactual            = 0.0d0
    CTFanPower         = 0.0d0
    OutletWaterTemp    = Node(WaterInletNode)%Temp

    WaterUsage         = 0.0d0
    Twb                = SimpleTowerInlet(TowerNum)%AirWetBulb
    TwbCapped          = SimpleTowerInlet(TowerNum)%AirWetBulb
    LoopNum            = SimpleTower(TowerNum)%LoopNum
    LoopSideNum        = SimpleTower(TowerNum)%LoopSideNum
    SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
    CASE (SingleSetPoint)
      TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpoint
    CASE (DualSetPointDeadBand)
      TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpointHi
    END SELECT

    Tr                 = Node(WaterInletNode)%Temp - TempSetPoint
    Ta                 = TempSetPoint - SimpleTowerInlet(TowerNum)%AirWetBulb

  ! Do not RETURN here if flow rate is less than MassFlowTolerance. Check basin heater and then RETURN.
    IF(PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock .EQ. 0)RETURN
  ! MassFlowTolerance is a parameter to indicate a no flow condition
    IF(WaterMassFlowRate.LE.MassFlowTolerance)THEN
      CALL CalcBasinHeaterPower(SimpleTower(TowerNum)%BasinHeaterPowerFTempDiff,&
                                SimpleTower(TowerNum)%BasinHeaterSchedulePtr,&
                                SimpleTower(TowerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
      RETURN
    ENDIF

    !loop to increment NumCell if we cannot meet the setpoint with the actual number of cells calculated above
    IncrNumCellFlag = .true.
    DO WHILE (IncrNumCellFlag)
      IncrNumCellFlag = .false.
    ! Initialize inlet node water properties
      WaterDensity  = GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                Node(WaterInletNode)%Temp, &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                'CalcVariableSpeedTower')
      WaterFlowRateRatio       = WaterMassFlowRatePerCell / (WaterDensity*SimpleTower(TowerNum)%CalibratedWaterFlowRate &
                                                                          /SimpleTower(TowerNum)%NumCell )

    ! check independent inputs with respect to model boundaries
      Call CheckModelBounds(TowerNum, Twb, Tr, Ta, WaterFlowRateRatio, TwbCapped, TrCapped, TaCapped, WaterFlowRateRatioCapped)

!   determine the free convection capacity by finding the outlet temperature at full air flow and multiplying
!   the tower's full capacity temperature difference by the percentage of tower capacity in free convection
!   regime specified by the user

      AirFlowRateRatio         = 1.0d0
      OutletWaterTempOFF       = Node(WaterInletNode)%Temp
      OutletWaterTempON        = Node(WaterInletNode)%Temp
      OutletWaterTemp          = OutletWaterTempOFF
      FreeConvectionCapFrac    = SimpleTower(TowerNum)%FreeConvectionCapacityFraction

      Call SimVariableTower(TowerNum, WaterFlowRateRatioCapped, AirFlowRateRatio, TwbCapped, OutletWaterTempON)

      IF(OutletWaterTempON .GT. TempSetPoint) THEN
        FanCyclingRatio    = 1.0d0
        AirFlowRateRatio   = 1.0d0
        CTFanPower         = SimpleTower(TowerNum)%HighSpeedFanPower * NumCellON / SimpleTower(TowerNum)%NumCell
        OutletWaterTemp    = OutletWaterTempON
        ! if possible increase the number of cells and do the calculations again with the new water mass flow rate per cell
        IF (NumCellON .lt. SimpleTower(TowerNum)%NumCell .and.   &
           (WaterMassFlowRate/(NumCellON+1)) .gt. WaterMassFlowRatePerCellMin)THEN
          NumCellON = NumCellON + 1
          WaterMassFlowRatePerCell = WaterMassFlowRate / NumCellON
          IncrNumCellFlag = .true.
        END IF
      END IF
    END DO

    ! find the correct air ratio only if full flow is  too much
    IF (OutletWaterTempON .LT. TempSetPoint)THEN
  !   outlet water temperature is calculated in the free convection regime
      OutletWaterTempOFF = Node(WaterInletNode)%Temp - FreeConvectionCapFrac * (Node(WaterInletNode)%Temp - OutletWaterTempON)
  !   fan is OFF
      FanCyclingRatio    = 0.0d0
  !   air flow ratio is assumed to be the fraction of tower capacity in the free convection regime (fan is OFF but air is flowing)
      AirFlowRateRatio   = FreeConvectionCapFrac

      ! Assume setpoint was met using free convection regime (pump ON and fan OFF)
      CTFanPower      = 0.0d0
      OutletWaterTemp = OutletWaterTempOFF

      IF(OutletWaterTempOFF .GT. TempSetPoint)THEN
      ! Setpoint was not met, turn on cooling tower fan at minimum fan speed

        AirFlowRateRatio  = SimpleTower(TowerNum)%MinimumVSAirFlowFrac
        Call SimVariableTower(TowerNum, WaterFlowRateRatioCapped, AirFlowRateRatio, TwbCapped, OutletWaterTempMIN)

        IF(OutletWaterTempMIN .LT. TempSetPoint)THEN
!         if setpoint was exceeded, cycle the fan at minimum air flow to meet the setpoint temperature
          IF(SimpleTower(TowerNum)%FanPowerfAirFlowCurve .EQ. 0)THEN
            CTFanPower      = AirFlowRateRatio**3 * SimpleTower(TowerNum)%HighSpeedFanPower * NumCellON /   &
               SimpleTower(TowerNum)%NumCell
          ELSE
            FanCurveValue   = CurveValue(SimpleTower(TowerNum)%FanPowerfAirFlowCurve,AirFlowRateRatio)
            CTFanPower      = MAX(0.0d0,(SimpleTower(TowerNum)%HighSpeedFanPower * FanCurveValue)) * NumCellON /  &
               SimpleTower(TowerNum)%NumCell
          END IF
  !       fan is cycling ON and OFF at the minimum fan speed. Adjust fan power and air flow rate ratio according to cycling rate
          FanCyclingRatio     = ((OutletWaterTempOFF - TempSetPoint)/(OutletWaterTempOFF - OutletWaterTempMIN))
          CTFanPower          = CTFanPower * FanCyclingRatio
          OutletWaterTemp     = TempSetPoint
          AirFlowRateRatio    = (FanCyclingRatio * SimpleTower(TowerNum)%MinimumVSAirFlowFrac) + &
                                ((1-FanCyclingRatio)*FreeConvectionCapFrac)
        ELSE
  !       if setpoint was not met at minimum fan speed, set fan speed to maximum
          AirFlowRateRatio  = 1.0d0
!         fan will not cycle and runs the entire time step
          FanCyclingRatio   = 1.0d0

          Call SimVariableTower(TowerNum, WaterFlowRateRatioCapped, AirFlowRateRatio, TwbCapped, OutletWaterTemp)

          ! Setpoint was met with pump ON and fan ON at full flow
          ! Calculate the fraction of full air flow to exactly meet the setpoint temperature

            Par(1) = TowerNum                          ! Index to cooling tower
  !         cap the water flow rate ratio and inlet air wet-bulb temperature to provide a stable output
            Par(2) = WaterFlowRateRatioCapped          ! water flow rate ratio
            Par(3) = TwbCapped                         ! Inlet air wet-bulb temperature [C]
  !         do not cap desired range and approach temperature to provide a valid (balanced) output for this simulation time step
            Par(4) = Tr                                ! Tower range temperature [C]
            Par(5) = Ta                                ! desired approach temperature [C]
            Par(6) = 1.0d0                               ! calculate the air flow rate ratio required for a balance

            CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, AirFlowRateRatio, SimpleTowerApproachResidual, &
                                SimpleTower(TowerNum)%MinimumVSAirFlowFrac, 1.0d0, Par)
            IF (SolFla == -1) THEN
              IF(.NOT. WarmUpFlag)CALL ShowWarningError('Cooling tower iteration limit exceeded when calculating air flow ' &
                                  //'rate ratio for tower '//TRIM(SimpleTower(TowerNum)%Name))
!           IF RegulaFalsi cannot find a solution then provide detailed output for debugging
            ELSE IF (SolFla == -2) THEN
              IF(.NOT. WarmUpFlag) THEN
                WRITE(OutputChar,OutputFormat)TwbCapped
                WRITE(OutputChar2,OutputFormat)Tr
                WRITE(OutputChar3,OutputFormat)Ta
                WRITE(OutputChar4,OutputFormat)WaterFlowRateRatioCapped
                WRITE(OutputChar5,OutputFormat)SimpleTower(TowerNum)%MinimumVSAirFlowFrac
                IF(SimpleTower(TowerNum)%CoolingTowerAFRRFailedCount .LT. 1)THEN
                  SimpleTower(TowerNum)%CoolingTowerAFRRFailedCount = SimpleTower(TowerNum)%CoolingTowerAFRRFailedCount + 1
                  CALL ShowWarningError('CoolingTower:VariableSpeed "'//TRIM(SimpleTower(TowerNum)%Name)//'" - ' &
                                      //'Cooling tower air flow rate ratio calculation failed ')
                  CALL ShowContinueError('...with conditions as Twb = '//TRIM(OutputChar)//', Trange = ' &
                                  //TRIM(OutputChar2)//', Tapproach = '//TRIM(OutputChar3)//', and water flow rate ratio = ' &
                                  //TRIM(OutputChar4))
                  CALL ShowContinueError('...a solution could not be found within the valid range ' &
                                  //'of air flow rate ratios')
                  CALL ShowContinueErrorTimeStamp(' '//'...Valid air flow rate ratio range = '//TRIM(OutputChar5)//' to 1.0.')
                  CALL ShowContinueError('...Consider modifying the design approach or design range temperature for this tower.')
                ELSE
                  CALL ShowRecurringWarningErrorAtEnd('CoolingTower:VariableSpeed "'//TRIM(SimpleTower(TowerNum)%Name)// &
                                                   '" - Cooling tower air flow rate ratio calculation failed '// &
                                                   'error continues.', &
                                                   SimpleTower(TowerNum)%CoolingTowerAFRRFailedIndex)
                END IF
              END IF
            ENDIF

  !         Use theoretical cubic for deterination of fan power if user has not specified a fan power ratio curve
            IF(SimpleTower(TowerNum)%FanPowerfAirFlowCurve .EQ. 0)THEN
              CTFanPower      = AirFlowRateRatio**3 * SimpleTower(TowerNum)%HighSpeedFanPower * NumCellON /   &
                 SimpleTower(TowerNum)%NumCell
            ELSE
              FanCurveValue   = CurveValue(SimpleTower(TowerNum)%FanPowerfAirFlowCurve,AirFlowRateRatio)
              CTFanPower      = MAX(0.0d0,(SimpleTower(TowerNum)%HighSpeedFanPower * FanCurveValue)) * NumCellON /   &
                 SimpleTower(TowerNum)%NumCell
            END IF
!           outlet water temperature is calculated as the inlet air wet-bulb temperature plus tower approach temperature
            OutletWaterTemp = Twb + Ta
        END IF   ! IF(OutletWaterTempMIN .LT. TempSetPoint)THEN

      END IF   ! IF(OutletWaterTempOFF .GT. TempSetPoint)THEN
    END IF   ! IF(OutletWaterTempON .LT. TempSetpoint) ie if tower should not run at full capacity

    CpWater  = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                               Node(SimpleTower(TowerNum)%WaterInletNodeNum)%Temp,        &
                               PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,       &
                               'CalcVariableSpeedTower')
    Qactual = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)
    SimpleTower(TowerNum)%NumCellON = NumCellON
  ! Set water and air properties
    AirDensity          = PsyRhoAirFnPbTdbW(SimpleTowerInlet(TowerNum)%AirPress, &
                            SimpleTowerInlet(TowerNum)%AirTemp,SimpleTowerInlet(TowerNum)%AirHumRat)
    AirMassFlowRate     = AirFlowRateRatio*SimpleTower(TowerNum)%HighSpeedAirFlowRate*AirDensity&
                          *SimpleTower(TowerNum)%NumCellON/SimpleTower(TowerNum)%NumCell
    InletAirEnthalpy    =   &
       PsyHFnTdbRhPb(SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                     1.0d0,  &
                     SimpleTowerInlet(TowerNum)%AirPress)

!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
    IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
      IF(VSTower(SimpleTower(TowerNum)%VSTower)%PrintLGMessage)THEN
          VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountFlowFrac = &
                           VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountFlowFrac + 1
!       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
        IF (VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountFlowFrac < 2) THEN
           CALL ShowWarningError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%LGBuffer1))
           CALL ShowContinueError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%LGBuffer2))
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'&
              //TRIM(SimpleTower(TowerNum)%Name)//'" - Liquid to gas ratio is out of range error continues...' &
              ,VSTower(SimpleTower(TowerNum)%VSTower)%ErrIndexLG,VSTower(SimpleTower(TowerNum)%VSTower)%LGLast, &
                                                                 VSTower(SimpleTower(TowerNum)%VSTower)%LGLast)
        END IF
      END IF
    END IF

!   save last system time step and last end time of current time step (used to determine if warning is valid)
    TimeStepSysLast    = TimeStepSys
    CurrentEndTimeLast = CurrentEndTime

!   warn user on first occurrence if flow fraction is greater than maximum for the YorkCalc model, use recurring warning stats
    IF(SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcModel .OR. &
       SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcUserDefined)THEN
       VSTower(SimpleTower(TowerNum)%VSTower)%PrintLGMessage = .FALSE.
!      Do not report error message in free convection regime
       IF(AirFlowRateRatio .GT. SimpleTower(TowerNum)%MinimumVSAirFlowFrac)THEN
         FlowFraction = WaterFlowRateRatioCapped/AirFlowRateRatio
!        Flow fractions greater than a MaxLiquidToGasRatio of 8 are not reliable using the YorkCalc model
         IF(FlowFraction .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxLiquidToGasRatio)THEN
!          Report warnings only during actual simulation
           IF(.NOT. WarmUpFlag)THEN
             VSTower(SimpleTower(TowerNum)%VSTower)%PrintLGMessage = .TRUE.
             WRITE(OutputChar,OutputFormat)FlowFraction
             WRITE(OutputChar2,OutputFormat)VSTower(SimpleTower(TowerNum)%VSTower)%MaxLiquidToGasRatio
             VSTower(SimpleTower(TowerNum)%VSTower)%LGBuffer1 = TRIM(SimpleTower(TowerNum)%TowerType)//' "' &
                                                               //TRIM(SimpleTower(TowerNum)%Name)// &
                '" - Liquid to gas ratio (L/G) is out of range at '//TRIM(OutputChar)//'.'
             VSTower(SimpleTower(TowerNum)%VSTower)%LGBuffer2 = ' '//'...Valid maximum ratio = '//TRIM(OutputChar2)// &
                            '. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                            //TRIM(CreateSysTimeIntervalString())
             VSTower(SimpleTower(TowerNum)%VSTower)%LGLast       = FlowFraction
           END IF
         END IF
       END IF
    END IF


RETURN
END SUBROUTINE CalcVariableSpeedTower

SUBROUTINE SimSimpleTower(TowerNum,WaterMassFlowRate,AirFlowRate,UAdesign,OutletWaterTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  Shirey, Raustad, Jan 2001

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! See purpose for Single Speed or Two Speed tower model

          ! METHODOLOGY EMPLOYED:
          !
          ! See methodology for Single Speed or Two Speed tower model

          ! REFERENCES:
          !
          ! Merkel, F. 1925.  Verduftungskuhlung. VDI Forschungsarbeiten, Nr 275, Berlin.
          ! ASHRAE     1999.  HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculations.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER  , INTENT(IN)   :: TowerNum
  REAL(r64), INTENT(IN)   :: WaterMassFlowRate
  REAL(r64), INTENT(IN)   :: AirFlowRate
  REAL(r64), INTENT(IN)   :: UAdesign
  REAL(r64), INTENT(OUT)  :: OutletWaterTemp


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: IterMax           = 50      ! Maximum number of iterations allowed
  REAL(r64), PARAMETER :: WetBulbTolerance  = 0.00001d0 ! Maximum error for exiting wet-bulb temperature between iterations
                                                        ! [delta K/K]
  REAL(r64), PARAMETER :: DeltaTwbTolerance = 0.001d0   ! Maximum error (tolerance) in DeltaTwb for iteration convergence [C]

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: Iter                 ! Number of iterations completed
  REAL(r64)              :: MdotCpWater          ! Water mass flow rate times the heat capacity [W/K]
  REAL(r64)              :: InletAirTemp         ! Dry-bulb temperature of air entering the tower [C]
  REAL(r64)              :: CpWater              ! Heat capacity of water [J/kg/K]
  REAL(r64)              :: CpAir                ! Heat capacity of air [J/kg/K]
  REAL(r64)              :: AirDensity           ! Density of air [kg/m3]
  REAL(r64)              :: AirMassFlowRate      ! Mass flow rate of air [kg/s]
  REAL(r64)              :: effectiveness        ! Effectiveness of the heat exchanger [-]
  REAL(r64)              :: UAactual             ! UA value at actual conditions [W/C]
  REAL(r64)              :: InletAirEnthalpy     ! Enthalpy of entering moist air [J/kg]
  REAL(r64)              :: InletAirWetBulb      ! Wetbulb temp of entering moist air [C]
  REAL(r64)              :: OutletAirEnthalpy    ! Enthalpy of exiting moist air [J/kg]
  REAL(r64)              :: OutletAirWetBulb     ! Wetbulb temp of exiting moist air [C]
  REAL(r64)              :: OutletAirWetBulbLast ! temporary Wetbulb temp of exiting moist air [C]
  REAL(r64)              :: AirCapacity          ! MdotCp of air through the tower
  REAL(r64)              :: CapacityRatioMin     ! Minimum capacity of airside and waterside
  REAL(r64)              :: CapacityRatioMax     ! Maximum capacity of airside and waterside
  REAL(r64)              :: CapacityRatio        ! Ratio of minimum to maximum capacity
  REAL(r64)              :: NumTransferUnits     ! Number of transfer Units [NTU]
  REAL(r64)              :: WetBulbError      ! Calculated error for exiting wet-bulb temperature between iterations [delta K/K]
  REAL(r64)              :: CpAirside            ! Delta enthalpy of the tower air divides by delta air wet-bulb temp [J/kg/K]
  REAL(r64)              :: Qactual              ! Actual heat transfer rate between tower water and air [W]
  REAL(r64)              :: DeltaTwb             ! Absolute value of difference between inlet and outlet air wet-bulb temp [C]

     ! set inlet and outlet node numbers, and initialize some local variables

    WaterInletNode    = SimpleTower(TowerNum)%WaterInletNodeNum
    WaterOutletNode   = SimpleTower(TowerNum)%WaterOutletNodeNum
    Qactual           = 0.0d0
!    WetBulbTolerance  = 0.00001
    WetBulbError      = 1.0d0
!    IterMax           = 50
    DeltaTwb          = 1.0d0
!    DeltaTwbTolerance = 0.001

    ! set local tower inlet and outlet temperature variables
    InletWaterTemp    = SimpleTowerInlet(TowerNum)%WaterTemp
    OutletWaterTemp   = InletWaterTemp
    InletAirTemp      = SimpleTowerInlet(TowerNum)%AirTemp
    InletAirWetBulb   = SimpleTowerInlet(TowerNum)%AirWetBulb

    IF(UAdesign.EQ.0.0d0)RETURN

    ! set water and air properties
    AirDensity        = PsyRhoAirFnPbTdbW(SimpleTowerInlet(TowerNum)%AirPress,InletAirTemp,SimpleTowerInlet(TowerNum)%AirHumRat)
    AirMassFlowRate   = AirFlowRate*AirDensity
    CpAir             = PsyCpAirFnWTdb(SimpleTowerInlet(TowerNum)%AirHumRat,InletAirTemp)
    CpWater           = GetSpecificHeatGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                               SimpleTowerInlet(TowerNum)%WaterTemp,                      &
                               PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex, &
                               'SimSimpleTower')
     InletAirEnthalpy  =    &
       PsyHFnTdbRhPb(SimpleTowerInlet(TowerNum)%AirWetBulb,  &
                     1.0d0,  &
                     SimpleTowerInlet(TowerNum)%AirPress)

    ! initialize exiting wet bulb temperature before iterating on final solution
    OutletAirWetBulb = InletAirWetBulb + 6.0d0

    ! Calcluate mass flow rates
    IF (WaterMassFlowRate > 0.d0) THEN
      MdotCpWater =   WaterMassFlowRate * CpWater
    ELSE
      OutletWaterTemp = InletWaterTemp
      RETURN
    ENDIF
    Iter = 0
    DO WHILE ((WetBulbError.GT.WetBulbTolerance) .AND. (Iter.LE.IterMax) .AND. (DeltaTwb.gt.DeltaTwbTolerance))
        Iter = Iter + 1
!        OutletAirEnthalpy = PsyHFnTdbRhPb(OutletAirWetBulb,1.0,OutBaroPress)
        OutletAirEnthalpy = PsyHFnTdbRhPb(OutletAirWetBulb,1.0d0,SimpleTowerInlet(TowerNum)%AirPress)
        ! calculate the airside specific heat and capacity
        CpAirside   =   (OutletAirEnthalpy - InletAirEnthalpy)/(OutletAirWetBulb-InletAirWetBulb)
        AirCapacity = AirMassFlowRate * CpAirside
        ! calculate the minimum to maximum capacity ratios of airside and waterside
        CapacityRatioMin = MIN(AirCapacity,MdotCpWater)
        CapacityRatioMax = MAX(AirCapacity,MdotCpWater)
        CapacityRatio    = CapacityRatioMin/CapacityRatioMax
        ! Calculate heat transfer coefficient and number of transfer units (NTU)
        UAactual = UAdesign*CpAirside/CpAir
        NumTransferUnits = UAactual/CapacityRatioMin
        ! calculate heat exchanger effectiveness
        IF (CapacityRatio.LE.0.995d0)THEN
          effectiveness = (1.d0-EXP(-1.0d0*NumTransferUnits*(1.0d0-CapacityRatio)))/ &
                         (1.0d0-CapacityRatio*EXP(-1.0d0*NumTransferUnits*(1.0d0-CapacityRatio)))
        ELSE
          effectiveness = NumTransferUnits/(1.d0+NumTransferUnits)
        ENDIF
        ! calculate water to air heat transfer and store last exiting WB temp of air
        Qactual = effectiveness * CapacityRatioMin * (InletWaterTemp-InletAirWetBulb)
        OutletAirWetBulbLast = OutletAirWetBulb
        ! calculate new exiting wet bulb temperature of airstream
        OutletAirWetBulb = InletAirWetBulb + Qactual/AirCapacity
        ! Check error tolerance and exit if satisfied
        DeltaTwb = ABS(OutletAirWetBulb - InletAirWetBulb)
        ! Add KelvinConv to denominator below convert OutletAirWetBulbLast to Kelvin to avoid divide by zero.
        ! Wet bulb error units are delta K/K
        WetBulbError = ABS((OutletAirWetBulb - OutletAirWetBulbLast)/(OutletAirWetBulbLast+KelvinConv))
    END DO

    IF(Qactual .GE. 0.0d0)THEN
        OutletWaterTemp = InletWaterTemp - Qactual/ MdotCpWater
    ELSE
        OutletWaterTemp = InletWaterTemp
    END IF

RETURN
END SUBROUTINE SimSimpleTower

SUBROUTINE SimVariableTower(TowerNum, WaterFlowRateRatio, AirFlowRateRatio, Twb, OutletWaterTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Feb. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To calculate the leaving water temperature of the variable speed cooling tower.

          ! METHODOLOGY EMPLOYED:
          !
          ! The range temperature is varied to determine balance point where model output (Tapproach),
          ! range temperature and inlet air wet-bulb temperature show a balance as:
          ! Twb + Tapproach + Trange = Node(WaterInletNode)%Temp

          ! REFERENCES:
          !
          ! Benton, D.J., Bowmand, C.F., Hydeman, M., Miller, P.,
          ! "An Improved Cooling Tower Algorithm for the CoolToolsTM Simulation Model".
          ! ASHRAE Transactions 2002, V. 108, Pt. 1.
          !
          ! York International Corporation, "YORKcalcTM Software, Chiller-Plant Energy-Estimating Program",
          ! Form 160.00-SG2 (0502). © 2002.

          ! USE STATEMENTS:
          !
  USE General,         ONLY: SolveRegulaFalsi
  USE DataPlant,  ONLY: SingleSetPoint, DualSetpointDeadband

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)         :: TowerNum              ! variable speed tower index
  REAL(r64),    INTENT(IN)    :: WaterFlowRateRatio    ! current water flow rate ratio (capped if applicable)
  REAL(r64),    INTENT(IN)    :: AirFlowRateRatio      ! current air flow rate ratio
  REAL(r64),    INTENT(IN)    :: Twb                   ! current inlet air wet-bulb temperature (C, capped if applicable)
  REAL(r64),    INTENT(OUT)   :: OutletWaterTemp       ! calculated tower outlet water temperature (C)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte = 500         ! Maximum number of iterations
  REAL(r64), PARAMETER :: Acc =  0.0001d0        ! Accuracy of result

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: SolFla               ! Flag of solver
  REAL(r64), DIMENSION(4)     :: Par                  ! Parameter array for regula falsi solver
  REAL(r64)              :: Tr                   ! range temperature which results in an energy balance
  REAL(r64)  :: TempSetPoint ! local temporary for loop setpoint

!   determine tower outlet water temperature
    Par(1) = TowerNum                  ! Index to cooling tower
    Par(2) = WaterFlowRateRatio        ! water flow rate ratio
    Par(3) = AirFlowRateRatio          ! air flow rate ratio
    Par(4) = Twb                       ! inlet air wet-bulb temperature [C]
    CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, Tr, SimpleTowerTrResidual, 0.001d0, &
                          VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp, Par)

    OutletWaterTemp = SimpleTowerInlet(TowerNum)%WaterTemp - Tr

    IF (SolFla == -1) THEN
      CALL ShowSevereError('Iteration limit exceeded in calculating tower nominal capacity at minimum air flow ratio')
      CALL ShowContinueError('Design inlet air wet-bulb or approach temperature must be modified to achieve an acceptable'// &
                           ' range at the minimum air flow rate')
      CALL ShowContinueError('Cooling tower simulation failed to converge for tower '//TRIM(SimpleTower(TowerNum)%Name))
!    if SolFla = -2, Tr is returned as minimum value (0.001) and outlet temp = inlet temp - 0.001
    ELSE IF (SolFla == -2) THEN ! decide if should run at max flow
      SELECT CASE (PlantLoop(SimpleTower(TowerNum)%LoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetPoint)
        TempSetPoint       = PlantLoop(SimpleTower(TowerNum)%LoopNum)%LoopSide(SimpleTower(TowerNum)%LoopSideNum)%TempSetpoint
      CASE (DualSetPointDeadBand)
        TempSetPoint       = PlantLoop(SimpleTower(TowerNum)%LoopNum)%LoopSide(SimpleTower(TowerNum)%LoopSideNum)%TempSetpointHi
      END SELECT
      IF (SimpleTowerInlet(TowerNum)%WaterTemp >   &
           (TempSetPoint   + VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp ) ) THEN ! run flat out
        OutletWaterTemp = SimpleTowerInlet(TowerNum)%WaterTemp - VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp
      ENDIF
    END IF

RETURN
END SUBROUTINE SimVariableTower

SUBROUTINE CalcVSTowerApproach(TowerNum, PctWaterFlow, AirFlowRatio, Twb, Tr, Approach)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Feb. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate tower approach temperature (e.g. outlet water temp minus inlet air wet-bulb temp)
          ! given air flow ratio, water flow ratio, inlet air wet-bulb temp, and tower range.

          ! METHODOLOGY EMPLOYED:
          ! Calculation method used empirical models from CoolTools or York to determine performance
          ! of variable speed (variable air flow rate) cooling towers.

          ! REFERENCES:
          !
          ! Benton, D.J., Bowmand, C.F., Hydeman, M., Miller, P.,
          ! "An Improved Cooling Tower Algorithm for the CoolToolsTM Simulation Model".
          ! ASHRAE Transactions 2002, V. 108, Pt. 1.
          !
          ! York International Corporation, "YORKcalcTM Software, Chiller-Plant Energy-Estimating Program",
          ! Form 160.00-SG2 (0502). © 2002.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER         , INTENT(IN)  :: TowerNum     ! Index to cooling tower
    REAL(r64)       , INTENT(IN)  :: AirFlowRatio ! Air flow ratio of cooling tower
    REAL(r64)       , INTENT(IN)  :: PctWaterFlow ! Water flow ratio of cooling tower
    REAL(r64)       , INTENT(IN)  :: Twb          ! Inlet air wet-bulb temperature [C]
    REAL(r64)       , INTENT(IN)  :: Tr           ! Cooling tower range (outlet water temp minus inlet air wet-bulb temp) [C]
    REAL(r64)       , INTENT(OUT) :: Approach     ! Calculated approach temperature [C]

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    REAL(r64)        :: Twb                       ! Inlet air wet-bulb temperature [C] (or [F] for CoolTools Model)
!    REAL(r64)        :: Tr                        ! Cooling tower range (outlet water temp minus inlet air wet-bulb temp) [C]
                                                  !   (or [F] for CoolTools Model)
    REAL(r64)        :: PctAirFlow   = 0.0d0        ! air flow rate ratio (fan power ratio in the case of CoolTools model)
    REAL(r64)        :: FlowFactor   = 0.0d0        ! water flow rate to air flow rate ratio (L/G) for YorkCalc model

!    IF(SimpleTower(TowerNum)%TowerModelType .EQ. CoolToolsXFModel .OR. &
!        SimpleTower(TowerNum)%TowerModelType .EQ. CoolToolsCFModel .OR. &
!        SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcModel)THEN
!      Twb        = (TwbIN * 1.8) + 32.0 ! Convert Celsius to Fahrenheit for CoolTools Model
!      Tr         = (TrIN * 1.8)
      ! Convert air flow rate ratio to fan power ratio for CoolTools Model
!      IF(SimpleTower(TowerNum)%TowerModelType .NE. YorkCalcModel)PctAirFlow = (AirFlowRatio)**3.0
!    ELSE
!      Twb        = TwbIN
!      Tr         = TrIN
!      IF(SimpleTower(TowerNum)%TowerModelType .NE. YorkCalcUserDefined)PctAirFlow = (AirFlowRatio)**3.0
!    END IF

    IF(SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcModel .OR. &
       SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcUserDefined)THEN
      PctAirFlow = AirFlowRatio
      FlowFactor = PctWaterFlow / PctAirFlow
      Approach = VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1) + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2) * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3) * Twb * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4) * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5) * Twb * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6) * Twb * Twb * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7) * Tr * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8) * Twb * Tr * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9) * Twb * Twb * Tr * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) * Twb * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) * Twb * Twb * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) * Tr * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) * Twb * Tr * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) * Twb * Twb * Tr * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) * Tr * Tr * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) * Twb * Tr * Tr * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) * Twb * Twb * Tr * Tr * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) * FlowFactor * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) * Twb * FlowFactor * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) * Twb * Twb * FlowFactor * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) * Tr * FlowFactor * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) * Twb * Tr * FlowFactor * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) * Twb * Twb * Tr * FlowFactor * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) * Tr * Tr * FlowFactor * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) * Twb * Tr * Tr * FlowFactor * FlowFactor + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) * Twb * Twb * Tr * Tr * FlowFactor * FlowFactor

    ELSE ! empirical model is CoolTools format

!     the CoolTools model actually uses PctFanPower = AirFlowRatio^3 as an input to the model
      PctAirFlow = (AirFlowRatio)**3
      Approach = VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(1) + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(2) * PctAirFlow + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(3) * PctAirFlow * PctAirFlow + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(4) * PctAirFlow * PctAirFlow * PctAirFlow + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(5) * PctWaterFlow + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(6) * PctAirFlow * PctWaterFlow + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(7) * PctAirFlow * PctAirFlow * PctWaterFlow + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(8) * PctWaterFlow * PctWaterFlow + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(9) * PctAirFlow * PctWaterFlow * PctWaterFlow + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(10) * PctWaterFlow * PctWaterFlow * PctWaterFlow + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(11) * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(12) * PctAirFlow * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(13) * PctAirFlow * PctAirFlow * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(14) * PctWaterFlow * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(15) * PctAirFlow * PctWaterFlow * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(16) * PctWaterFlow * PctWaterFlow * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(17) * Twb * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(18) * PctAirFlow * Twb * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(19) * PctWaterFlow * Twb * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(20) * Twb * Twb * Twb + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(21) * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(22) * PctAirFlow * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(23) * PctAirFlow * PctAirFlow * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(24) * PctWaterFlow * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(25) * PctAirFlow * PctWaterFlow * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(26) * PctWaterFlow * PctWaterFlow * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(27) * Twb * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(28) * PctAirFlow * Twb * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(29) * PctWaterFlow * Twb * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(30) * Twb * Twb * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(31) * Tr * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(32) * PctAirFlow * Tr * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(33) * PctWaterFlow * Tr * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(34) * Twb * Tr * Tr + &
               VSTower(SimpleTower(TowerNum)%VSTower)%Coeff(35) * Tr * Tr * Tr
    END IF
!    capping approach to 0 results in failure of RegulaFalsi routine
!    Approach = MAX(0.0, Approach)

!    IF(SimpleTower(TowerNum)%TowerModelType .EQ. CoolToolsXFModel .OR. &
!        SimpleTower(TowerNum)%TowerModelType .EQ. CoolToolsCFModel .OR. &
!        SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcModel)THEN
!      Approach = (Approach / 1.8)  ! Convert from Fahrenheit to Celsius
!    END IF

END SUBROUTINE CalcVSTowerApproach

SUBROUTINE CheckModelBounds(TowerNum, Twb,Tr, Ta, WaterFlowRateRatio, TwbCapped, TrCapped, TaCapped, WaterFlowRateRatioCapped)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   Feb 2005
          !       MODIFIED       na
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To verify that the empirical model's independent variables are within the limits used during the
          ! developement of the empirical model.

          ! METHODOLOGY EMPLOYED:
          ! The empirical models used for simulating a variable speed cooling tower are based on a limited data set.
          ! Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
          ! The range of each independent variable is provided either by the CoolTools or York model limits, or
          ! specified by the user if the model is User Defined (in either the CoolTools or York model format).
          ! These limits are tested in this subroutine each time step and returned for use by the calling routine.
          ! The independent variables capped here may or may not be passed to the empirical model in the calling
          ! routine depending on their use.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: CurrentTime
  USE DataEnvironment, ONLY: EnvironmentName, CurMnDy
  USE General,         ONLY: CreateSysTimeIntervalString,RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: TowerNum                        ! index to tower
  REAL(r64),    INTENT(IN)  :: Twb                             ! current inlet air wet-bulb temperature (C)
  REAL(r64),    INTENT(IN)  :: Tr                              ! requested range temperature for current time step (C)
  REAL(r64),    INTENT(IN)  :: Ta                              ! requested approach temperature for current time step (C)
  REAL(r64),    INTENT(IN)  :: WaterFlowRateRatio              ! current water flow rate ratio at water inlet node
  REAL(r64),    INTENT(OUT) :: TwbCapped                       ! bounded value of inlet air wet-bulb temperature (C)
  REAL(r64),    INTENT(OUT) :: TrCapped                        ! bounded value of range temperature (C)
  REAL(r64),    INTENT(OUT) :: TaCapped                        ! bounded value of approach temperature (C)
  REAL(r64),    INTENT(OUT) :: WaterFlowRateRatioCapped        ! bounded value of water flow rate ratio

          ! SUBROUTINE PARAMETER DEFINITIONS:
!  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F5.2)'
!  CHARACTER(len=*), PARAMETER :: OutputFormat2 ='(F8.5)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 CHARACTER(len=32)         :: OutputChar         = ' '     ! character string for warning messages
 CHARACTER(len=32)         :: OutputCharLo       = ' '     ! character string for warning messages
 CHARACTER(len=32)         :: OutputCharHi       = ' '     ! character string for warning messages
 CHARACTER(len=32)         :: TrimValue          = ' '     ! character string for warning messages
 REAL(r64),SAVE    :: TimeStepSysLast        = 0.0d0 ! last system time step (used to check for downshifting)
 REAL(r64)    :: CurrentEndTime         = 0.0d0 ! end time of time step for current simulation time step
 REAL(r64),SAVE    :: CurrentEndTimeLast     = 0.0d0 ! end time of time step for last simulation time step
                                                          ! current end time is compared with last to see if time step changed

!   initialize capped variables in case independent variables are in bounds
    TwbCapped = Twb
    TrCapped  = Tr
    TaCapped  = Ta
    WaterFlowRateRatioCapped = WaterFlowRateRatio

!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
    IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
      IF(VSTower(SimpleTower(TowerNum)%VSTower)%PrintTrMessage)THEN
        VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountTR = &
                        VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountTR + 1
        IF (VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountTR < 2) THEN
           CALL ShowWarningError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%TrBuffer1))
           CALL ShowContinueError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%TrBuffer2))
           CALL ShowContinueError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%TrBuffer3))
           CALL ShowContinueError(' ...Range temperatures outside model boundaries may not ' &
                                  //'adversely affect tower performance.')
           CALL ShowContinueError(' ...This is not an unexpected occurrence when simulating actual conditions.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'&
              //TRIM(SimpleTower(TowerNum)%Name)//'" - Tower range temperature is out of range error continues...' &
              ,VSTower(SimpleTower(TowerNum)%VSTower)%ErrIndexTR,VSTower(SimpleTower(TowerNum)%VSTower)%TrLast, &
                                                                 VSTower(SimpleTower(TowerNum)%VSTower)%TrLast)
        END IF
      END IF
      IF(VSTower(SimpleTower(TowerNum)%VSTower)%PrintTwbMessage)THEN
        VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountIAWB = &
                           VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountIAWB + 1
        IF (VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountIAWB < 6) THEN
          CALL ShowWarningError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%TwbBuffer1))
          CALL ShowContinueError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%TwbBuffer2))
          CALL ShowContinueError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%TwbBuffer3))
          CALL ShowContinueError(' ...Wet-bulb temperatures outside model boundaries may not ' &
                                 //'adversely affect tower performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'&
              //TRIM(SimpleTower(TowerNum)%Name)//'" - Inlet air wet-bulb temperature is out of range error continues...' &
              ,VSTower(SimpleTower(TowerNum)%VSTower)%ErrIndexIAWB,VSTower(SimpleTower(TowerNum)%VSTower)%TwbLast, &
                                                                   VSTower(SimpleTower(TowerNum)%VSTower)%TwbLast)
        END IF
      END IF
      IF(VSTower(SimpleTower(TowerNum)%VSTower)%PrintTaMessage)THEN
        VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountTA = &
                           VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountTA + 1
        IF (VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountTA < 2) THEN
          CALL ShowWarningError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%TaBuffer1))
          CALL ShowContinueError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%TaBuffer2))
          CALL ShowContinueError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%TaBuffer3))
          CALL ShowContinueError(' ...Approach temperatures outside model boundaries may not ' &
                                 //'adversely affect tower performance.')
          CALL ShowContinueError(' ...This is not an unexpected occurrence when simulating actual conditions.')
       ELSE
         CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'&
              //TRIM(SimpleTower(TowerNum)%Name)//'" - Tower approach temperature is out of range error continues...' &
              ,VSTower(SimpleTower(TowerNum)%VSTower)%ErrIndexTA,VSTower(SimpleTower(TowerNum)%VSTower)%TaLast, &
                                                                 VSTower(SimpleTower(TowerNum)%VSTower)%TaLast)
       END IF
      END IF
      IF(VSTower(SimpleTower(TowerNum)%VSTower)%PrintWFRRMessage)THEN
        VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountWFRR = &
                           VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountWFRR + 1
        IF (VSTower(SimpleTower(TowerNum)%VSTower)%VSErrorCountWFRR < 6) THEN
          CALL ShowWarningError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%WFRRBuffer1))
          CALL ShowContinueError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%WFRRBuffer2))
          CALL ShowContinueError(TRIM(VSTower(SimpleTower(TowerNum)%VSTower)%WFRRBuffer3))
          CALL ShowContinueError(' ...Water flow rate ratios outside model boundaries may not ' &
                                 //'adversely affect tower performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'&
              //TRIM(SimpleTower(TowerNum)%Name)//'" - Water flow rate ratio is out of range error continues...' &
              ,VSTower(SimpleTower(TowerNum)%VSTower)%ErrIndexWFRR,VSTower(SimpleTower(TowerNum)%VSTower)%WaterFlowRateRatioLast, &
                                                                   VSTower(SimpleTower(TowerNum)%VSTower)%WaterFlowRateRatioLast)
        END IF
      END IF
    END IF

!   save last system time step and last end time of current time step (used to determine if warning is valid)
    TimeStepSysLast    = TimeStepSys
    CurrentEndTimeLast = CurrentEndTime

!   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
    IF(Twb .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp .OR. &
      Twb .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp)THEN
      OutputChar=RoundSigDigits(Twb,2)
      OutputCharLo=RoundSigDigits(VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp,2)
      OutputCharHi=RoundSigDigits(VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp,2)
      IF(Twb .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp)THEN
        TwbCapped = VSTower(SimpleTower(TowerNum)%VSTower)%MinInletAirWBTemp
      END IF
      IF(Twb .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp)THEN
        TwbCapped = VSTower(SimpleTower(TowerNum)%VSTower)%MaxInletAirWBTemp
      END IF
      IF(.NOT. WarmUpFlag)THEN
        VSTower(SimpleTower(TowerNum)%VSTower)%PrintTwbMessage = .TRUE.
        VSTower(SimpleTower(TowerNum)%VSTower)%TwbBuffer1 = TRIM(SimpleTower(TowerNum)%TowerType)//' "' &
                                                          //TRIM(SimpleTower(TowerNum)%Name)// &
           '" - Inlet air wet-bulb temperature is outside model boundaries at '//TRIM(OutputChar)//'.'
        VSTower(SimpleTower(TowerNum)%VSTower)%TwbBuffer2 = ' '//'...Valid range = '//TRIM(OutputCharLo)//' to ' &
                       //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                       //TRIM(CreateSysTimeIntervalString())
        TrimValue=RoundSigDigits(TwbCapped,6)
        VSTower(SimpleTower(TowerNum)%VSTower)%TwbBuffer3 = ' ...Inlet air wet-bulb temperature passed to the model = ' &
                                                            //TRIM(TrimValue)
        VSTower(SimpleTower(TowerNum)%VSTower)%TwbLast       = Twb
      ELSE
        VSTower(SimpleTower(TowerNum)%VSTower)%PrintTwbMessage = .FALSE.
      END IF
    ELSE
      VSTower(SimpleTower(TowerNum)%VSTower)%PrintTwbMessage = .FALSE.
    END IF

    IF(Tr .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp .OR. &
      Tr .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp)THEN
      OutputChar=RoundSigDigits(Tr,2)
      OutputCharLo=RoundSigDigits(VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp,2)
      OutputCharHi=RoundSigDigits(VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp,2)
      IF(Tr .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp)THEN
        TrCapped = VSTower(SimpleTower(TowerNum)%VSTower)%MinRangeTemp
      END IF
      IF(Tr .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp)THEN
        TrCapped = VSTower(SimpleTower(TowerNum)%VSTower)%MaxRangeTemp
      END IF
      IF(.NOT. WarmUpFlag)THEN
        VSTower(SimpleTower(TowerNum)%VSTower)%PrintTrMessage = .TRUE.
        VSTower(SimpleTower(TowerNum)%VSTower)%TrBuffer1 = TRIM(SimpleTower(TowerNum)%TowerType)//' "' &
                                                         //TRIM(SimpleTower(TowerNum)%Name)// &
             '" - Tower range temperature is outside model boundaries at '//TRIM(OutputChar)//'.'
        VSTower(SimpleTower(TowerNum)%VSTower)%TrBuffer2 = ' '//'...Valid range = '//TRIM(OutputCharLo)//' to ' &
                            //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                            //TRIM(CreateSysTimeIntervalString())
        TrimValue=RoundSigDigits(Tr,5)
        VSTower(SimpleTower(TowerNum)%VSTower)%TrBuffer3 = ' ...Tower range temperature passed to the model = '//TRIM(TrimValue)
        VSTower(SimpleTower(TowerNum)%VSTower)%TrLast       = Tr
      ELSE
        VSTower(SimpleTower(TowerNum)%VSTower)%PrintTrMessage = .FALSE.
      END IF
    ELSE
      VSTower(SimpleTower(TowerNum)%VSTower)%PrintTrMessage = .FALSE.
    END IF

    IF(Ta .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp .OR. &
       Ta .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp)THEN
      OutputChar=RoundSigDigits(Ta,2)
      OutputCharLo=RoundSigDigits(VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp,2)
      OutputCharHi=RoundSigDigits(VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp,2)
      IF(Ta .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp)THEN
        TaCapped = VSTower(SimpleTower(TowerNum)%VSTower)%MinApproachTemp
      END IF
      IF(Ta .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp)THEN
        TaCapped = VSTower(SimpleTower(TowerNum)%VSTower)%MaxApproachTemp
      END IF
      IF(.NOT. WarmUpFlag)THEN
        VSTower(SimpleTower(TowerNum)%VSTower)%PrintTaMessage = .TRUE.
        VSTower(SimpleTower(TowerNum)%VSTower)%TaBuffer1 = TRIM(SimpleTower(TowerNum)%TowerType)//' "' &
                                                         //TRIM(SimpleTower(TowerNum)%Name)// &
              '" - Tower approach temperature is outside model boundaries at '//TRIM(OutputChar)//'.'
        VSTower(SimpleTower(TowerNum)%VSTower)%TaBuffer2 = ' '//'...Valid range = '//TRIM(OutputCharLo)//' to ' &
                             //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                            //TRIM(CreateSysTimeIntervalString())
        TrimValue=RoundSigDigits(Ta,5)
        VSTower(SimpleTower(TowerNum)%VSTower)%TaBuffer3 = ' ...Tower approach temperature passed to the model = ' &
                                                          //TRIM(TrimValue)
        VSTower(SimpleTower(TowerNum)%VSTower)%TaLast = Ta
      ELSE
        VSTower(SimpleTower(TowerNum)%VSTower)%PrintTaMessage = .FALSE.
      END IF
    ELSE
      VSTower(SimpleTower(TowerNum)%VSTower)%PrintTaMessage = .FALSE.
    END IF

    IF(SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcModel .OR. &
       SimpleTower(TowerNum)%TowerModelType .EQ. YorkCalcUserDefined)THEN
!     Water flow rate ratio warning not valid for YorkCalc model, print liquid to gas ratio
!     warning instead (bottom of Subroutine VariableSpeedTower)
      VSTower(SimpleTower(TowerNum)%VSTower)%PrintWFRRMessage = .FALSE.
    ELSE
      IF(WaterFlowRateRatio .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio .OR. &
        WaterFlowRateRatio .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio)THEN
        OutputChar=RoundSigDigits(WaterFlowRateRatio,2)
        OutputCharLo=RoundSigDigits(VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio,2)
        OutputCharHi=RoundSigDigits(VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio,2)
        IF(WaterFlowRateRatio .LT. VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio)THEN
           WaterFlowRateRatioCapped = VSTower(SimpleTower(TowerNum)%VSTower)%MinWaterFlowRatio
        END IF
        IF(WaterFlowRateRatio .GT. VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio)THEN
           WaterFlowRateRatioCapped = VSTower(SimpleTower(TowerNum)%VSTower)%MaxWaterFlowRatio
        END IF
        IF(.NOT. WarmUpFlag)THEN
          VSTower(SimpleTower(TowerNum)%VSTower)%PrintWFRRMessage = .TRUE.
          VSTower(SimpleTower(TowerNum)%VSTower)%WFRRBuffer1 = TRIM(SimpleTower(TowerNum)%TowerType)//' "' &
                                                             //TRIM(SimpleTower(TowerNum)%Name)// &
                '" - Water flow rate ratio is outside model boundaries at '//TRIM(OutputChar)//'.'
          VSTower(SimpleTower(TowerNum)%VSTower)%WFRRBuffer2 = ' '//'...Valid range = '//TRIM(OutputCharLo)//' to ' &
                               //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                              //TRIM(CreateSysTimeIntervalString())
          TrimValue=RoundSigDigits(WaterFlowRateRatioCapped,5)
          VSTower(SimpleTower(TowerNum)%VSTower)%WFRRBuffer3 = ' ...Water flow rate ratio passed to the model = '//TRIM(TrimValue)
          VSTower(SimpleTower(TowerNum)%VSTower)%WaterFlowRateRatioLast = WaterFlowRateRatio
        ELSE
          VSTower(SimpleTower(TowerNum)%VSTower)%PrintWFRRMessage = .FALSE.
        END IF
      ELSE
        VSTower(SimpleTower(TowerNum)%VSTower)%PrintWFRRMessage = .FALSE.
      END IF
    END IF

END SUBROUTINE CheckModelBounds

FUNCTION SimpleTowerUAResidual(UA, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Design Tower Load - Tower Cooling Output) / Design Tower Load.
          ! Tower Cooling Output depends on the UA which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Puts UA into the cooling tower data structure, calls SimSimpleTower, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: UA                         ! UA of cooling tower
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = design tower load [W]
                                                    ! par(2) = tower number
                                                    ! par(3) = design water mass flow rate [kg/s]
                                                    ! par(4) = design air volume flow rate [m3/s]
                                                    ! par(5) = water specific heat [J/(kg*C)]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: TowerIndex                             ! index of this tower
  REAL(r64)    :: OutWaterTemp                           ! outlet water temperature [C]
  REAL(r64)    :: CoolingOutput                          ! tower cooling output [W]

  TowerIndex = INT(Par(2))
  CALL SimSimpleTower(TowerIndex,Par(3),Par(4),UA,OutWaterTemp)
  CoolingOutput = Par(5)*Par(3)*(SimpleTowerInlet(TowerIndex)%WaterTemp - OutWaterTemp)
  Residuum = (Par(1) - CoolingOutput) / Par(1)
  RETURN
END FUNCTION SimpleTowerUAResidual

FUNCTION SimpleTowerTrResidual(Trange, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Feb 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (where residual shows a balance point of model and desired performance)
          ! Tower Approach depends on the range temperature which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Varies tower range temperature until a balance point exists where the model output corresponds
          ! to the desired independent variables

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: Trange                     ! cooling tower range temperature [C]
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = tower number
                                                    ! par(2) = water flow ratio
                                                    ! par(3) = air flow ratio
                                                    ! par(4) = inlet air wet-bulb temperature [C]
    REAL(r64)         :: Residuum                   ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: TowerIndex                             ! index of this tower
  REAL(r64) ::    WaterFlowRateRatio                     ! ratio of water flow rate to design water flow rate
  REAL(r64) ::    AirFlowRateRatio                       ! ratio of water flow rate to design water flow rate
  REAL(r64) ::    InletAirWB                             ! inlet air wet-bulb temperature [C]
  REAL(r64) ::    Tapproach                              ! tower approach temperature [C]

  TowerIndex         = INT(Par(1))
  WaterFlowRateRatio = Par(2)
  AirFlowRateRatio   = Par(3)
  InletAirWB         = Par(4)
  Tapproach          = 0.0d0

! call model to determine approach temperature given other independent variables (range temp is being varied to find balance)
  CALL CalcVSTowerApproach(TowerIndex,WaterFlowRateRatio,AirFlowRateRatio,InletAirWB,Trange,Tapproach)
! calculate residual based on a balance where Twb + Ta + Tr = Node(WaterInletNode)%Temp
  Residuum = (InletAirWB+Tapproach+Trange)- &
              Node(SimpleTower(TowerIndex)%WaterInletNodeNum)%Temp

  RETURN
END FUNCTION SimpleTowerTrResidual

FUNCTION SimpleTowerApproachResidual(FlowRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Feb 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Desired Approach - Model Approach Output)
          ! Tower Approach depends on the water (or air) flow rate ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! In SizeTower, calibrates tower water flow rate ratio at an air flow rate ratio of 1.
          ! In VariableSpeedTower, calculates air flow rate ratio at the inlet water flow rate ratio.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: FlowRatio                  ! water or air flow ratio of cooling tower
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = tower number
                                                  ! par(2) = water or air flow ratio (opposite of input variable)
                                                  ! par(3) = inlet air wet-bulb temp [C]
                                                  ! par(4) = tower range [C]
                                                  ! par(5) = desired approach [C]
                                                  ! par(6) = 0.0 to calculate water flow rate ratio, 1.0 for air
  REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: TowerIndex                           ! index of this tower
  REAL(r64) ::    WaterFlowRateRatio                   ! ratio of water flow rate to design water flow rate
  REAL(r64) ::    AirFlowRateRatio                     ! ratio of water flow rate to design water flow rate
  REAL(r64) ::    InletAirWB                           ! inlet air wet-bulb temperature [C]
  REAL(r64) ::    Trange                               ! tower range temperature [C]
  REAL(r64) ::    TapproachActual                      ! actual tower approach temperature [C]
  REAL(r64) ::    TapproachDesired                     ! desired tower approach temperature [C]

  TowerIndex = INT(Par(1))
  IF(Par(6) .EQ. 0.0d0) THEN
    AirFlowRateRatio   = Par(2)
    WaterFlowRateRatio = FlowRatio
  ELSE
    AirFlowRateRatio   = FlowRatio
    WaterFlowRateRatio = Par(2)
  END IF
  InletAirWB           = Par(3)
  Trange               = Par(4)
  TapproachDesired     = Par(5)
  TapproachActual      = 0.0d0

! call model to determine tower approach temperature given other independent variables
  CALL CalcVSTowerApproach(TowerIndex,WaterFlowRateRatio,AirFlowRateRatio,InletAirWB,Trange,TapproachActual)
  Residuum = TapproachDesired - TapproachActual

  RETURN
END FUNCTION SimpleTowerApproachResidual

! End of the CondenserLoopTowers Module Simulation Subroutines

! *****************************************************************************

SUBROUTINE CalculateWaterUseage(TowerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       T Hong, Aug. 2008. Added fluid bypass for single speed cooling tower
          !                      A Flament, July 2010. Added multi-cell capability
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Collect tower water useage calculations for
          ! reuse by all the tower models.


          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Code for this routine started from VariableSpeedTower

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: SecInHour, BeginTimeStepFlag
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataWater      , ONLY: WaterStorage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: TowerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: AirDensity
  REAL(r64) :: AirMassFlowRate
  REAL(r64) :: AvailTankVdot
  REAL(r64) :: BlowDownVdot      =0.0d0
  REAL(r64) :: DriftVdot         =0.0d0
  REAL(r64) :: EvapVdot          =0.0d0
  REAL(r64) :: InletAirEnthalpy
  REAL(r64) :: InSpecificHumRat
  REAL(r64) :: OutSpecificHumRat
  REAL(r64) :: TairAvg
  REAL(r64) :: MakeUpVdot
  REAL(r64) :: OutletAirEnthalpy
  REAL(r64) :: OutletAirHumRatSat
  REAL(r64) :: OutletAirTSat
  REAL(r64) :: StarvedVdot
  REAL(r64) :: TankSupplyVdot
  REAL(r64) :: rho

  REAL(r64) :: AverageWaterTemp

  AverageWaterTemp = (InletWaterTemp + OutletWaterTemp) / 2.0d0

  ! Set water and air properties
  If (SimpleTower(TowerNum)%EvapLossMode == EvapLossByMoistTheory) Then

    AirDensity          = PsyRhoAirFnPbTdbW(SimpleTowerInlet(TowerNum)%AirPress, &
                            SimpleTowerInlet(TowerNum)%AirTemp,SimpleTowerInlet(TowerNum)%AirHumRat)
    AirMassFlowRate     = AirFlowRateRatio*SimpleTower(TowerNum)%HighSpeedAirFlowRate*AirDensity&
                          * SimpleTower(TowerNum)%NumCellON / SimpleTower(TowerNum)%NumCell
    InletAirEnthalpy    =   &
       PsyHFnTdbRhPb(SimpleTowerInlet(TowerNum)%AirWetBulb,  &
          1.0d0,  &
          SimpleTowerInlet(TowerNum)%AirPress)

    IF  (AirMassFlowRate > 0.0d0) Then
      ! Calculate outlet air conditions for determining water usage

      OutletAirEnthalpy   = InletAirEnthalpy + Qactual/AirMassFlowRate
      OutletAirTSat       = PsyTsatFnHPb(OutletAirEnthalpy,SimpleTowerInlet(TowerNum)%AirPress)
      OutletAirHumRatSat  = PsyWFnTdbH(OutletAirTSat,OutletAirEnthalpy)

      ! calculate specific humidity ratios (HUMRAT to mass of moist air not dry air)
       InSpecificHumRat = SimpleTowerInlet(TowerNum)%AirHumRat / ( 1 + SimpleTowerInlet(TowerNum)%AirHumRat)
       OutSpecificHumRat = OutletAirHumRatSat / (1+ OutletAirHumRatSat)

      ! calculate average air temp for density call
       TairAvg = (SimpleTowerInlet(TowerNum)%AirTemp + OutletAirTSat)/2.0d0

      ! Amount of water evaporated, get density water at air temp or 4 C if too cold
       rho =  GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                MAX(TairAvg,4.d0), &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                'CalculateWaterUseage')

       EvapVdot         = (AirMassFlowRate * (OutSpecificHumRat - InSpecificHumRat)) / rho ! [m3/s]
       IF (EvapVdot < 0.0d0) EvapVdot = 0.0d0
     ELSE
       EvapVdot         = 0.0d0
     ENDIF

  ElseIf (SimpleTower(TowerNum)%EvapLossMode == EvapLossByUserFactor) Then
!    EvapVdot   = SimpleTower(TowerNum)%UserEvapLossFactor * (InletWaterTemp - OutletWaterTemp) &
!                     * SimpleTower(TowerNum)%DesignWaterFlowRate
    rho =  GetDensityGlycol(PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidName,  &
                                AverageWaterTemp, &
                                PlantLoop(SimpleTower(TowerNum)%LoopNum)%FluidIndex,&
                                'CalculateWaterUseage')

    EvapVdot   = SimpleTower(TowerNum)%UserEvapLossFactor * (InletWaterTemp - OutletWaterTemp) &
                     * (WaterMassFlowRate / rho )
    IF (EvapVdot < 0.0d0) EvapVdot = 0.0d0
  ELSE
    ! should never come here
  ENDIF

!   amount of water lost due to drift
  DriftVdot               = SimpleTower(TowerNum)%DesignWaterFlowRate * &
                            SimpleTower(TowerNum)%NumCellON / SimpleTower(TowerNum)%NumCell * &
                            SimpleTower(TowerNum)%DriftLossFraction *  AirFlowRateRatio

  If (SimpleTower(TowerNum)%BlowdownMode == BlowdownBySchedule) THEN
    ! Amount of water lost due to blow down (purging contaminants from tower basin)
    IF(SimpleTower(TowerNum)%SchedIDBlowdown .GT. 0)THEN
      BlowDownVdot          = GetCurrentScheduleValue(SimpleTower(TowerNum)%SchedIDBlowdown)
    ELSE
      BlowDownVdot          = 0.0d0
    END IF
  ELSEIF (SimpleTower(TowerNum)%BlowdownMode == BlowdownByConcentration) THEN
      If (SimpleTower(TowerNum)%ConcentrationRatio > 2.0d0) Then ! protect divide by zero
         BlowDownVdot  =  EvapVdot / (SimpleTower(TowerNum)%ConcentrationRatio - 1) - DriftVdot
      ELSE
         BlowDownVdot  = EvapVdot - DriftVdot
      ENDIF
      If ( BlowDownVdot < 0.0d0 ) BlowDownVdot = 0.0d0
  ELSE
   !should never come here
  ENDIF

  ! Added for fluid bypass
  IF (SimpleTower(TowerNum)%CapacityControl == CapacityControl_FluidBypass) THEN
    If (SimpleTower(TowerNum)%EvapLossMode == EvapLossByUserFactor) EvapVdot = EvapVdot * (1 - SimpleTower(TowerNum)%BypassFraction)
    DriftVdot = DriftVdot * (1 - SimpleTower(TowerNum)%BypassFraction)
    BlowDownVdot = BlowDownVdot * (1 - SimpleTower(TowerNum)%BypassFraction)
  ENDIF

  MakeUpVdot = EvapVdot + DriftVdot + BlowDownVdot

  ! set demand request in Water STorage if needed
  StarvedVdot = 0.0d0
  TankSupplyVdot = 0.0d0
  If (SimpleTower(TowerNum)%SuppliedByWaterSystem) Then

     ! set demand request
     WaterStorage(SimpleTower(TowerNum)%WaterTankID)%VdotRequestDemand(SimpleTower(TowerNum)%WaterTankDemandARRID) &
      = MakeUpVdot

     AvailTankVdot = &       ! check what tank can currently provide
     WaterStorage(SimpleTower(TowerNum)%WaterTankID)%VdotAvailDemand(SimpleTower(TowerNum)%WaterTankDemandARRID)

     TankSupplyVdot = MakeUpVdot ! init
     If (AvailTankVdot < MakeUpVdot) Then ! calculate starved flow
        StarvedVdot = MakeUpVdot - AvailTankVdot
        TankSupplyVdot = AvailTankVdot
     ENDIF
  ELSE ! supplied by mains

  ENDIF

  !   total water usage
  ! update report variables
  SimpleTowerReport(TowerNum)%EvaporationVdot   = EvapVdot
  SimpleTowerReport(TowerNum)%EvaporationVol    = EvapVdot       * (TimeStepSys * SecInHour)
  SimpleTowerReport(TowerNum)%DriftVdot         = DriftVdot
  SimpleTowerReport(TowerNum)%DriftVol          = DriftVdot      * (TimeStepSys * SecInHour)
  SimpleTowerReport(TowerNum)%BlowdownVdot      = BlowDownVdot
  SimpleTowerReport(TowerNum)%BlowdownVol       = BlowDownVdot   * (TimeStepSys * SecInHour)
  SimpleTowerReport(TowerNum)%MakeUpVdot        = MakeUpVdot
  SimpleTowerReport(TowerNum)%MakeUpVol         = MakeUpVdot     * (TimeStepSys * SecInHour)
  SimpleTowerReport(TowerNum)%TankSupplyVdot    = TankSupplyVdot
  SimpleTowerReport(TowerNum)%TankSupplyVol     = TankSupplyVdot * (TimeStepSys * SecInHour)
  SimpleTowerReport(TowerNum)%StarvedMakeUpVdot = StarvedVdot
  SimpleTowerReport(TowerNum)%StarvedMakeUpVol  = StarvedVdot    * (TimeStepSys * SecInHour)

  RETURN

END SUBROUTINE CalculateWaterUseage


! Beginning of Record Keeping subroutines for the Tower Module
! *****************************************************************************

SUBROUTINE UpdateTowers(TowerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    October 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for passing results to the outlet water node.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: EnvironmentName, CurMnDy
  USE General, ONLY: TrimSigDigits
  USE DataPlant, ONLY: PlantLoop
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: TowerNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  LowTempFmt="(' ',F6.2)"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=25)      :: CharErrOut
  CHARACTER(len=25)      :: CharLowOutletTemp
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  REAL(r64) :: LoopMinTemp
  ! set node information


  Node(WaterOutletNode)%Temp                   = OutletWaterTemp

  LoopNum = SimpleTower(TowerNum)%LoopNum
  LoopSideNum = SimpleTower(TowerNum)%LoopSideNum
  IF(PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock.EQ.0 .OR. WarmupFlag) RETURN

  !Check flow rate through tower and compare to design flow rate, show warning if greater than Design * Mulitplier
  IF (Node(WaterOutletNode)%MassFlowRate .GT. SimpleTower(TowerNum)%DesWaterMassFlowRate * &
                                              SimpleTower(TowerNum)%TowerMassFlowRateMultiplier) THEN
    SimpleTower(TowerNum)%HighMassFlowErrorCount=SimpleTower(TowerNum)%HighMassFlowErrorCount+1
    IF (SimpleTower(TowerNum)%HighMassFlowErrorCount < 2) THEN
      CALL ShowWarningError (TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//'"')
      CALL ShowContinueError (' Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate.')
      CALL ShowContinueError (' Condenser Loop Mass Flow Rate = '//TrimSigDigits(Node(WaterOutletNode)%MassFlowRate,6))
      CALL ShowContinueError (' Tower Design Mass Flow Rate   = '//TrimSigDigits(SimpleTower(TowerNum)%DesWaterMassFlowRate,6))
      CALL ShowContinueErrorTimeStamp(' ')
    ELSE
      CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
          '"  Condenser Loop Mass Flow Rate is much greater than the towers design mass flow rate error continues...' &
          , SimpleTower(TowerNum)%HighMassFlowErrorIndex, Node(WaterOutletNode)%MassFlowRate, Node(WaterOutletNode)%MassFlowRate)
    ENDIF
  END IF

   ! Check if OutletWaterTemp is below the minimum condenser loop temp and warn user
   LoopMinTemp = PlantLoop(LoopNum)%MinTemp
   IF(OutletWaterTemp.LT.LoopMinTemp .AND. WaterMassFlowRate > 0.0d0) THEN
     SimpleTower(TowerNum)%OutletWaterTempErrorCount = SimpleTower(TowerNum)%OutletWaterTempErrorCount + 1
     WRITE(CharLowOutletTemp,LowTempFmt) LoopMinTemp
     WRITE(CharErrOut,LowTempFmt) OutletWaterTemp
     CharErrOut=ADJUSTL(CharErrOut)
     IF (SimpleTower(TowerNum)%OutletWaterTempErrorCount < 2) THEN
       CALL ShowWarningError (TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//'"')
       CALL ShowContinueError ('Cooling tower water outlet temperature ('//TRIM(CharErrOut)//' C) is '// &
                               'below the specified minimum condenser loop temp of '//TRIM(ADJUSTL(CharLowOutletTemp))//' C')
       CALL ShowContinueErrorTimeStamp(' ')
     ELSE
       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
          '" Cooling tower water outlet temperature is below the specified minimum condenser loop temp error continues...' &
          , SimpleTower(TowerNum)%OutletWaterTempErrorIndex, OutletWaterTemp, OutletWaterTemp)
     END IF
   END IF

   ! Check if water mass flow rate is small (e.g. no flow) and warn user
   IF(WaterMassFlowRate .GT. 0.0d0 .AND. WaterMassFlowRate .LE. MassFlowTolerance)THEN
     SimpleTower(TowerNum)%SmallWaterMassFlowErrorCount = SimpleTower(TowerNum)%SmallWaterMassFlowErrorCount + 1
     IF (SimpleTower(TowerNum)%SmallWaterMassFlowErrorCount < 2) THEN
       CALL ShowWarningError (TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//'"')
       CALL ShowContinueError ('Cooling tower water mass flow rate near zero.')
       CALL ShowContinueErrorTimeStamp(' ')
       CALL ShowContinueError('Actual Mass flow = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
     ELSE
       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
          '"  Cooling tower water mass flow rate near zero error continues...' &
          , SimpleTower(TowerNum)%SmallWaterMassFlowErrorIndex, WaterMassFlowRate, WaterMassFlowRate)
     ENDIF
   END IF

   ! Check if water mass flow rate is lower than loop minimum and warn user
!   IF(WaterMassFlowRate .LT. LoopMassFlowRateMinAvail)THEN
!     SimpleTower(TowerNum)%WMFRLessThanMinAvailErrCount = SimpleTower(TowerNum)%WMFRLessThanMinAvailErrCount + 1
!     IF (SimpleTower(TowerNum)%WMFRLessThanMinAvailErrCount < 2) THEN
!       CALL ShowWarningError (TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//'"')
!       CALL ShowContinueError ('Cooling tower water mass flow below loop minimum.')
!       CALL ShowContinueErrorTimeStamp(' ')
!       CALL ShowContinueError('Actual Mass flow  = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
!       CALL ShowContinueError('Loop Minimum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMinAvail,2)))
!     ELSE
!       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
!          '" Cooling tower water mass flow rate below loop minimum error continues...' &
!          , SimpleTower(TowerNum)%WMFRLessThanMinAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
!     ENDIF
!   END IF

   ! Check if water mass flow rate is greater than loop maximum and warn user
!   IF(WaterMassFlowRate .GT. LoopMassFlowRateMaxAvail)THEN
!     SimpleTower(TowerNum)%WMFRGreaterThanMaxAvailErrCount = SimpleTower(TowerNum)%WMFRGreaterThanMaxAvailErrCount + 1
!     IF (SimpleTower(TowerNum)%WMFRGreaterThanMaxAvailErrCount < 2) THEN
!       CALL ShowWarningError (TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//'"')
!       CALL ShowContinueError ('Cooling Tower water mass flow above loop maximum.')
!       CALL ShowContinueErrorTimeStamp(' ')
!       CALL ShowContinueError('Actual Mass flow='//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
!       CALL ShowContinueError('Loop Maximum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMaxAvail,2)))
!     ELSE
!       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleTower(TowerNum)%TowerType)//' "'//TRIM(SimpleTower(TowerNum)%Name)//&
!          '" Cooling tower water mass flow rate above loop maximum error continues...' &
!          , SimpleTower(TowerNum)%WMFRGreaterThanMaxAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
!     ENDIF
!   END IF

RETURN
END SUBROUTINE UpdateTowers
! End of Record Keeping subroutines for the Tower Module
! *****************************************************************************

! Beginning of Reporting subroutines for the Tower Module
! *****************************************************************************

SUBROUTINE ReportTowers(RunFlag, TowerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    October 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variables for the tower.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: RunFlag
  INTEGER, INTENT(IN) :: TowerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ReportingConstant

  ReportingConstant = TimeStepSys*SecInHour

  IF (.NOT. RunFlag)THEN
    SimpleTowerReport(TowerNum)%InletWaterTemp         = Node(WaterInletNode)%Temp
    SimpleTowerReport(TowerNum)%OutletWaterTemp        = Node(WaterInletNode)%Temp
    SimpleTowerReport(TowerNum)%WaterMassFlowRate      = WaterMassFlowRate
    SimpleTowerReport(TowerNum)%Qactual                = 0.0d0
    SimpleTowerReport(TowerNum)%FanPower               = 0.0d0
    SimpleTowerReport(TowerNum)%FanEnergy              = 0.0d0
    SimpleTowerReport(TowerNum)%AirFlowRatio           = 0.0d0
    SimpleTowerReport(TowerNum)%WaterAmountUsed        = 0.0d0
    SimpleTowerReport(TowerNum)%BasinHeaterPower       = BasinHeaterPower
    SimpleTowerReport(TowerNum)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    SimpleTowerReport(TowerNum)%FanCyclingRatio        = 0.0d0
    SimpleTowerReport(TowerNum)%BypassFraction         = 0.0d0   ! added for fluid bypass
    SimpleTowerReport(TowerNum)%NumCellON              = 0
    SimpleTowerReport(TowerNum)%SpeedSelected          = 0
  ELSE
    SimpleTowerReport(TowerNum)%InletWaterTemp         = Node(WaterInletNode)%Temp
    SimpleTowerReport(TowerNum)%OutletWaterTemp        = OutletWaterTemp
    SimpleTowerReport(TowerNum)%WaterMassFlowRate      = WaterMassFlowRate
    SimpleTowerReport(TowerNum)%Qactual                = Qactual
    SimpleTowerReport(TowerNum)%FanPower               = CTFanPower
    SimpleTowerReport(TowerNum)%FanEnergy              = CTFanPower*ReportingConstant
    SimpleTowerReport(TowerNum)%AirFlowRatio           = AirFlowRateRatio
    SimpleTowerReport(TowerNum)%WaterAmountUsed        = WaterUsage*ReportingConstant
    SimpleTowerReport(TowerNum)%BasinHeaterPower       = BasinHeaterPower
    SimpleTowerReport(TowerNum)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    SimpleTowerReport(TowerNum)%FanCyclingRatio        = FanCyclingRatio
    SimpleTowerReport(TowerNum)%BypassFraction         = SimpleTower(TowerNum)%BypassFraction   ! added for fluid bypass
    SimpleTowerReport(TowerNum)%NumCellON              = SimpleTower(TowerNum)%NumCellON
    SimpleTowerReport(TowerNum)%SpeedSelected          = SimpleTower(TowerNum)%SpeedSelected
  END IF


RETURN
END SUBROUTINE ReportTowers

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

END MODULE CondenserLoopTowers
