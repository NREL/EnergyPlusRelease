MODULE RefrigeratedCase


          ! MODULE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Oct/Nov 2004
          !       MODIFIED       Shirey, FSEC Dec 2004; Hudson, ORNL Feb 2007, July 2007
          !       MODIFIED       Stovall, ORNL, April 2008 added detailed refrigerations systems
          !       MODIFIED       Stovall, ORNL, Fall 2009 added cascade condensers, secondary loops, and walk-ins.
          !       MODIFIED       Griffith, NREL, 2010, Plant upgrade, generalize plant fluid properties.
          !       MODIFIED       Fricke, ORNL, Fall 2011, added detailed transcritical CO2 refrigeration system.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To simulate refrigerated cases,walk-in coolers, secondary loops,
          !             compressor racks, and detailed refrigeration systems.
          ! Case performance is varied based on specific operating conditions within the zone.

          ! METHODOLOGY EMPLOYED:
          ! Refrigerated cases are simulated based on performance information available from manufacturers
          ! and knowing the latent heat ratio of the case cooling load which can be obtained from ASHRAE or
          ! others who have completed research to quantify this value. The sensible case credits
          ! (sensible cooling provided to the surrounding zone air) are calculated during input
          ! processing by subtracting the fan, lighting, and anti-sweat heater energy from the
          ! design sensible capacity (TotCap * (1-LHR) * RTF).  The actual latent cooling provided
          ! by the case (at the actual operating conditions) is calculated based on the latent performance
          ! curve supplied by the user.  The actual sensible cooling provided by the case (at the actual
          ! operating conditions) is calculated by summing all sensible components (fan + light + anti-sweat +
          ! sensible case credit). The case (evaporator) fans are assumed to be disabled during hot-gas or
          ! electric defrost modes. A stocking schedule (W/m) is available for added load if desired.
          !
          ! Walk-In coolers are simulated based on the specified sum of the conductance*area of each wall and door facing
          ! up to three (extendible) zones.  Two types of doors are specified, stock doors and glass doors.  For each category
          ! of doors, schedules for door openings are used with ASHRAE models for infiltration
          ! which are a function of the height of the door.  That
          ! infiltration is used to calculate the latent load on the cooler and the latent case credits for each zone.
          ! The sensible case credit for each zone is the sum of the conductive and sensible infiltration load
          ! for each surface/door facing that zone.  Note that the latent portion of the infiltration is unused
          ! during defrost to be consistent with decision for refrigerated cases.
          !
          ! Compressor racks are simulated by summing the attached refrigerated case and walk-in cooler
          ! capacities.  The energy use of the
          ! compressor rack is then calculated with a simplified model for COP variation with temperature. Condenser fan power
          ! is based on a user-supplied curve object. Racks are not scheduled.
          ! Compressor racks may have indoor (in-zone) or outdoor condensers.  If outdoors, the condensers may be cooled
          ! by dry air cooling or evaporative cooling. A water-cooled condenser is also available by specifying water-cooling
          ! as the Condenser Type.  If a water-cooled condenser is utilized, a second entry to this module will
          ! occur during the HVAC simulation as called from the NonZoneEquipmentManager.
          !
          ! Detailed refrigeration systems model each compressor individually using the manufacturer's rating curves.
          ! A collection of such curves has been added in the datasets library. The curves produce the refrigeration
          ! capacity and power consumption. The capacity needed is based on the sum of the case and walk-in loads (as is done
          ! for the compressor racks).  The compressors are dispatched to meet this load according to the order
          ! prescribed in the compressor list input object. The condenser for each system can be air-cooled,
          ! evaporative-cooled, or water cooled.  For air and evap-cooled condensers, manufacturer's rating data
          ! is input to describe the performance and to determine the required air flow rate, which is used to
          ! calculate the fan power requirements.  The fans can be described as single-speed, two-speed, or variable
          ! speed. The condenser performance data also is used to calculate the condensing temperature, which is a function
          ! of the heat sink temperature and the load on the condenser.  This must be solved iteratively, checking on
          ! the calculated refrigerant mass flow through the compressors.  The solution usually requires less than 5 iterations.
          ! The refrigerant state exiting the compressor group is known so the amount of heat available for
          ! desuperheat reclaim is explicitly known.
          ! The detailed refrigeration model allows the use of subcoolers,secondary loops, and cascade condensers
          ! to transfer load from one suction group to another. This introduces the need for further iterations among
          ! the systems.  Three loops through the
          ! systems are adequate to model these interactions.  The detailed model will also calculate a variable suction
          ! pressure for systems with controls that allow the suction temperature/pressure to float
          ! up when the case loads are less than their design capacity.

          ! Secondary Systems include case and walk-in refrigeration loads.  However, the balance of the system is
          ! made up of a heat exchanger and circulating pump(s) instead of a condenser and compressors.
          ! The total load on the heat exchanger is the sum of the refrigeration loads, any pipe heat gains,
          ! and the portion of the pump power that is absorbed by the circulating fluid. If the total load is
          ! greater than the rated capacity of the secondary loop, the unmet load for any time step is carried over
          ! to the next time step.  Each secondary system appears as a load on a detailed refrigeration system. If
          ! any of the cases or walk-ins served by a secondary are defrosted using hot brine, the primary system
          ! serving the secondary loop receives the defrost energy credits (i.e., heat reclaim used to generate defrost
          ! energy).

          ! Cascade Condensers allow the use of a higher temperature refrigeration system (primary system) to serve as a
          ! heat rejection sink for a lower temperature refrigeration system (secondary system). The condensing
          ! temperature can be fixed or can be allowed to float according to the minimum required evaporating temperature
          ! for other loads upon the primary system. For cases and walk-ins served by cascade condensers, energy associated
          ! with hot gas defrost is reclaimed from the primary system.  The refrigeration load the cascade condenser
          ! places upon the primary system is the sum of all case and walk-in loads served by the secondary system plus
          ! the sum of the secondary loop’s compressor power. The same name used to identify the condenser in the
          ! secondary loop is used to identify the cascade load on the primary system.

          ! Detailed transcritical CO2 refrigeration systems model each compressor individually using the manufacturer's
          ! performance data. A collection of CO2 compressor performance curves has been added in the datasets library.
          ! The curves produce the refrigeration capacity and power consumption. The capacity required is based on the sum
          ! of the case and walk-in loads and the compressors are dispatched to meet this load according to the order
          ! prescribed in the compressor list input object. Currently, an air-cooled gas cooler is modeled, and
          ! manufacturer's rating data is input to describe the performance and to determine the required fan power
          ! requirements. The gas cooler fans can be described as single-speed, two-speed, or variable speed. During
          ! transcritical operation, the optimal gas cooler pressure, which maximizes the system's COP, is determined as
          ! a function of the ambient air temperature. During subcritical operation, the condensing pressure is allowed to
          ! float with ambient temperature in order to acheive maximum performance.

          !This module was designed to be accessed once for each time step.  It uses several accumulating variables
          !  to carry unmet loads from one time step to the next (cases/walk-ins and compressors.  Also, it meets
          !  heat reclaim needs with the loads from the previous time step (because they are unknown for the current
          !  zone time step).  However, the loads time step may be repeated, such as when a demand manager is used.
          !  For that purpose, the values for these accumulating variables are saved at the start of each time step
          !  and reset whenever the time step is repeated.  (see the init subroutine.)
          !This correction is also applied when working on the system time step for coil-type loads by setting the saved values
          !  at the start of each system time step to the value at the end of the previous time step. They are reset
          !  to that value at each sys time step iteration. (see InitRefrigeration)

          ! REFERENCES:
          ! Specific references are provided for the equipment simulation subroutines below.

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:

USE DataPrecisionGlobals
USE DataHeatBalance,  ONLY: RefrigCaseCredit, HeatReclaimRefrigeratedRack, NumRefrigeratedRacks, &
                            NumRefrigSystems, HeatReclaimRefrigCondenser, NumRefrigCondensers, &
                            NumRefrigChillerSets, RefrigSystemTypeDetailed, RefrigSystemTypeRack, &
                            RefrigCondenserTypeAir, RefrigCondenserTypeEvap, RefrigCondenserTypeWater, &
                            RefrigCondenserTypeCascade

USE DataHVACGlobals,  ONLY: TimeStepSys  !used when operating for warehouse coil
USE DataGlobals  !includes LOGICAL :: BeginTimeStepFlag =.false.
                 ! True at the start of each time step, False after first subtime step of time step
                 ! includes CurrentTime, in fractional hours, from start of day. Uses Loads time step.
                 ! includes NumOfZones
USE DataInterfaces
USE ScheduleManager
USE DataLoopNode
USE DataEnvironment
USE FluidProperties

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE   ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:

! Anti-sweat heater control type
INTEGER, PARAMETER :: ASNone                = 0
INTEGER, PARAMETER :: ASConstant            = 1
INTEGER, PARAMETER :: ASLinear              = 2
INTEGER, PARAMETER :: ASDewPoint            = 3
INTEGER, PARAMETER :: ASHeatBalance         = 4
! Refrigerated display case defrost type
INTEGER, PARAMETER :: DefNone               = 0
INTEGER, PARAMETER :: DefOffCycle           = 1
INTEGER, PARAMETER :: DefHotFluid             = 2
INTEGER, PARAMETER :: DefHotFluidOnDemand     = 3
INTEGER, PARAMETER :: DefHotFluidTerm         = 4
INTEGER, PARAMETER :: DefElectric           = 5
INTEGER, PARAMETER :: DefElectricOnDemand   = 6
INTEGER, PARAMETER :: DefElectricTerm       = 7

! Refrigerated display case rack heat rejection location
INTEGER, PARAMETER :: LocationOutdoors      = 1
INTEGER, PARAMETER :: LocationZone          = 2
! Condenser cooling type -- See DataHeatBalance - RefrigxxxTypexxx
!INTEGER, PARAMETER :: CondenserCoolingAir   = 1
!INTEGER, PARAMETER :: CondenserCoolingEvap  = 2
!INTEGER, PARAMETER :: CondenserCoolingWater = 3
!INTEGER, PARAMETER :: CondenserCascade      = 4
! Air- and evap-cooled condenser fan speed control types
INTEGER, PARAMETER :: FanVariableSpeed       = 1
INTEGER, PARAMETER :: FanConstantSpeedLinear = 2
INTEGER, PARAMETER :: FanTwoSpeed            = 3
INTEGER, PARAMETER :: FanConstantSpeed       = 4
! Water-cooled condenser loop flow type
INTEGER, PARAMETER :: VariableFlow          = 1
INTEGER, PARAMETER :: ConstantFlow          = 2
! Condenser evap cooling water supply
INTEGER, PARAMETER :: WaterSupplyFromMains = 101
INTEGER, PARAMETER :: WaterSupplyFromTank  = 102
! Cascade condenser temperature control types
INTEGER, PARAMETER :: CascadeTempSet    = 1
INTEGER, PARAMETER :: CascadeTempFloat  = 2
! Refrigerated display case energy equation form
INTEGER, PARAMETER :: None                  = 0
INTEGER, PARAMETER :: CaseTemperatureMethod = 1
INTEGER, PARAMETER :: RHCubic               = 2
INTEGER, PARAMETER :: DPCubic               = 3
! Secondary loop parameters
INTEGER, PARAMETER :: SecFluidTypeAlwaysLiquid = 1
INTEGER, PARAMETER :: SecFluidTypePhaseChange  = 2
INTEGER, PARAMETER :: SecPumpControlConstant   = 1
INTEGER, PARAMETER :: SecPumpControlVariable   = 2
! Walk In Cooler Defrost type
INTEGER, PARAMETER :: WalkInDefrostFluid       = 1
INTEGER, PARAMETER :: WalkInDefrostElec        = 2
INTEGER, PARAMETER :: WalkInDefrostNone        = 3
INTEGER, PARAMETER :: WalkInDefrostOffCycle    = 4
! Walk In Cooler Defrost Control type
INTEGER, PARAMETER :: DefrostControlSched     = 1
INTEGER, PARAMETER :: DefrostContTempTerm  = 2
! Walk In Cooler Stock Door Protection types
INTEGER, PARAMETER :: WIStockDoorNone = 1
INTEGER, PARAMETER :: WIStockDoorAirCurtain = 2
INTEGER, PARAMETER :: WIStockDoorStripCurtain =3
! Subcooler type
INTEGER, PARAMETER :: LiquidSuction         = 1
INTEGER, PARAMETER :: Mechanical            = 2
! Compressor suction pressure control
INTEGER, PARAMETER :: FloatSuctionTemperature    =1
INTEGER, PARAMETER :: ConstantSuctionTemperature =2
!Compressor rating types
INTEGER, PARAMETER :: RatedSuperheat =1
INTEGER, PARAMETER :: RatedReturnGasTemperature =2
INTEGER, PARAMETER :: RatedSubcooling =1
INTEGER, PARAMETER :: RatedLiquidTemperature =2
! System service types (applies to system, cascade condenser, and secondary loops)
!INTEGER, PARAMETER :: SupermarketService =1
!INTEGER, PARAMETER :: WarehouseService   =2
! Warehouse coil Defrost type
INTEGER, PARAMETER :: DefrostFluid       = 1
INTEGER, PARAMETER :: DefrostElec        = 2
INTEGER, PARAMETER :: DefrostNone        = 3
INTEGER, PARAMETER :: DefrostOffCycle    = 4


INTEGER, PARAMETER :: RatedCapacityTotal    = 1
INTEGER, PARAMETER :: EuropeanSC1Std        = 2
INTEGER, PARAMETER :: EuropeanSC1Nom        = 3
INTEGER, PARAMETER :: EuropeanSC2Std        = 4
INTEGER, PARAMETER :: EuropeanSC2Nom        = 5
INTEGER, PARAMETER :: EuropeanSC3Std        = 6
INTEGER, PARAMETER :: EuropeanSC3Nom        = 7
INTEGER, PARAMETER :: EuropeanSC4Std        = 8
INTEGER, PARAMETER :: EuropeanSC4Nom        = 9
INTEGER, PARAMETER :: EuropeanSC5Std        = 10
INTEGER, PARAMETER :: EuropeanSC5Nom        = 11
INTEGER, PARAMETER :: UnitLoadFactorSens    =12
INTEGER, PARAMETER :: SHR60        = 1
INTEGER, PARAMETER :: QUADRATICSHR    = 2
INTEGER, PARAMETER :: EUROPEAN     = 3
INTEGER, PARAMETER :: TABULARRH_DT1_TRoom      = 4
INTEGER, PARAMETER :: Ceiling        = 1
INTEGER, PARAMETER :: Middle    = 2
INTEGER, PARAMETER :: Floor     = 3
INTEGER, PARAMETER :: DetailedSystem    = 1
INTEGER, PARAMETER :: SecondarySystem    = 2

! Following constant approp for R22, future may make f(refrigerant)
REAL(r64), PARAMETER ::CaseSuperheat      = 4.0d0 ! case superheat used to control thermal expansion valve, ASHRAE 2006 p 44.6 (C)
REAL(r64), PARAMETER ::TransCaseSuperheat = 10.0d0 ! case superheat for transcritical CO2 systems (C)
! Next two constants used to autosize evap condenser
REAL(r64), PARAMETER ::  CondPumpRatePower  = 0.004266d0  ! evap condenser pump rated, Wpump/Wcapacity (15 W/ton)
REAL(r64), PARAMETER ::  AirVolRateEvapCond = 0.000144d0  ! evap cond air flow rate for autosize, equiv 850 cfm/ton (m3/W-s)
REAL(r64), PARAMETER ::  EvapCutOutTdb      = 4.0d0       ! shut off evap water flow if outdoor drybulb < evapcutOutTdb (C)
! Miscellaneous constants
REAL(r64), PARAMETER ::  MyLargeNumber      = 1.0d9
REAL(r64), PARAMETER ::  MySmallNumber      = 1.0d-9
REAL(r64), PARAMETER ::  Rair               = 0.3169d0 ! Air resistance used with Heat Balance anti-sweat (AS) heater
REAL(r64), PARAMETER ::  IceMeltEnthalpy      = 335000.d0   ! heat of fusion of water J/kg
REAL(r64), PARAMETER ::  TempTooHotToFrost    = 5.0d0       ! C, used to check for frosting conditions on evaporator coils
REAL(r64), PARAMETER ::  IcetoVaporEnthalpy   = 2833000.0d0 ! J/kg to freeze water vapor to ice
REAL(r64), PARAMETER ::  WatertoVaporEnthalpy = 2.5d6       ! at 0C
REAL(r64), PARAMETER ::  SpecificHeatIce      = 2000.d0     ! in the likely range (2040 at 0C and 1950 at -20C) (J/kg-C)
REAL(r64), PARAMETER :: CondAirVolExponentDry = 1.58d0     !exponent for forced air over a cylinder, = 1/.633
                                                            !per ASHRAE 2005 (page 3.15)
REAL(r64), PARAMETER :: CondAirVolExponentEvap= 1.32d0     !exponent for evap condenser air vol flow, = 1/.76
                                                            !per Manske, 1999
REAL(r64), PARAMETER :: EvaporatorAirVolExponent= 1.54d0     !exponent for evapaporator air vol flow, = 1/.65
                                                            !per Manske, 1999, page 35

REAL(r64), PARAMETER :: FanHalfSpeedRatio =  0.1768d0  ! = 1/(2**2.5) for power step for two speed fan
REAL(r64), PARAMETER :: CapFac60Percent   =  0.60d0    ! = 60%, load served by half power 2-speed fan

  REAL(r64), DIMENSION(5) , PARAMETER ::EuropeanWetCoilFactor = (/1.35D0, 1.15D0,  1.05D0,  1.01D0,   1.0D0/)
  REAL(r64), DIMENSION(5) , PARAMETER ::EuropeanAirInletTemp  = (/10.0D0,  0.0D0, -18.0D0, -25.0D0, -34.0D0/)
  REAL(r64), DIMENSION(5) , PARAMETER ::EuropeanEvapTemp      = (/ 0.0D0, -8.0D0, -25.0D0, -31.0D0, -40.0D0/)
  REAL(r64), DIMENSION(5) , PARAMETER ::EuropeanDT1           = (/10.0D0,  8.0D0,   7.0D0,   7.0D0,   6.0D0/)

          ! DERIVED TYPE DEFINITIONS:

TYPE, PRIVATE :: RefrigCaseData
  CHARACTER(len=MaxNameLength) :: Name=' '                   ! Name of refrigerated display case
 ! CHARACTER(len=MaxNameLength) :: Schedule=' '               ! Display case availability schedule name
  CHARACTER(len=MaxNameLength) :: ZoneName=' '               ! Zone or Location of Display Case
  INTEGER          :: NumSysAttach=0             ! Number of systems attached to case, error if /=1
  INTEGER          :: SchedPtr=0                 ! Index to the correct availability schedule
  INTEGER          :: ZoneNodeNum =0             ! Index to Zone Node
  INTEGER          :: ActualZoneNum =0           ! Index to Zone
  INTEGER          :: ZoneRANode = 0             ! Node number of return node in zone
  REAL(r64)        :: RatedAmbientTemp=0.0d0       ! Rated ambient (zone) temperature
  REAL(r64)        :: RatedAmbientRH=0.0d0         ! Rated ambient (zone) relative humidity
  REAL(r64)        :: RatedAmbientDewPoint=0.0d0   ! Rated ambient (zone) dew point temperature
  REAL(r64)        :: RateTotCapPerLength=0.0d0            ! Gross total cooling capacity at rated conditions [W/m]
  REAL(r64)        :: RatedLHR=0.0d0               ! Latent heat ratio (lat cap/total cap) at rated conditions
  REAL(r64)        :: RatedRTF=0.0d0               ! Run time fraction at rated conditions
  INTEGER          :: LatCapCurvePtr=0           ! Index for latent case credit modifier curve
  INTEGER          :: DefCapCurvePtr=0           ! Index for defrost load modifier curve
  INTEGER          :: LatentEnergyCurveType=0    ! Type of latent case credit curve:
                                                 ! 1=Case Temperature Method, 2=RH cubic, 3=DP cubic
  INTEGER          :: DefrostEnergyCurveType=0   ! Type of defrost energy curve:
                                                 ! 1=Case Temperature Method, 2=RH cubic, 3=DP cubic
  REAL(r64)        :: STDFanPower=0.0d0            ! Standard power of case fan [W/m] for case credit calc
  REAL(r64)        :: OperatingFanPower=0.0d0      ! Operating power of refrigerated case fan [W/m]
  REAL(r64)        :: RatedLightingPower=0.0d0  ! Rated (consis w RateTotCapPerLength) power of refrigerated case lights [W/m]
  REAL(r64)        :: LightingPower=0.0d0          ! Installed power of refrigerated case lights [W/m]
  INTEGER          :: LightingSchedPtr=0         ! Index to the correct case lighting schedule
  REAL(r64)        :: AntiSweatPower=0.0d0         ! Rated power of refrigerated case anti-sweat heaters [W/m]
  REAL(r64)        :: MinimumASPower=0.0d0         ! Minimum power output of case anti-sweat heaters [W/m]
  INTEGER          :: AntiSweatControlType=0     ! Type of anti-sweat heater control:
                                                 ! 0=None,1=Constant,2=Linear,3=DewPoint,4=HeatBalance
  REAL(r64)        :: HumAtZeroAS=0.0d0            ! Relative humidity for zero AS heater output using linear control
  REAL(r64)        :: Height=0.0d0                 ! case height for AS heater with heat balance control
  INTEGER          :: DefrostType=0              ! Case defrost control type, Off-cycle,Timed,Hot-gas,Electric
  REAL(r64)        :: DefrostPower=0.0d0           ! Rated power of refrigerated case defrost [W/m]
  INTEGER          :: DefrostSchedPtr=0          ! Index to the correct defrost schedule
  INTEGER          :: DefrostDripDownSchedPtr=0  ! Index to the correct fail-safe schedule
  REAL(r64)        :: Length=0.0d0                 ! Length of refrigerated case [m]
  REAL(r64)        :: Temperature=0.0d0            ! Rated case temperature [C]
  REAL(r64)        :: RAFrac=0.0d0                 ! HVAC under case return air fraction [0-1]
  INTEGER          :: StockingSchedPtr=0         ! Index to the correct product stocking schedule
  REAL(r64)        :: LightingFractionToCase=0.0d0 ! Fraction of lighting energy that directly contributes to the
                                                 ! case cooling load. The remainder contributes to the zone load
                                                 ! (air heat balance).
  REAL(r64)        :: ASHeaterFractionToCase=0.0d0 ! Fraction of anti-sweat heater energy that results in a direct
                                                 ! heat load to the case. The remainder is a heating load
                                                 ! to the zone where the refrigerated case is located.
  REAL(r64)        :: DesignSensCaseCredit=0.0d0   ! Design sensible case credit applied to zone load
  REAL(r64)        :: EvapTempDesign=0.0d0         ! Design evaporator temperature
  REAL(r64)        :: RefrigInventory=0.0d0        ! Design refrigerant inventory [kg/m]
  REAL(r64)        :: DesignRefrigInventory=0.0d0  ! Design refrigerant inventory [kg total for the case]

  REAL(r64)        :: DesignRatedCap=0.0d0         ! Design total case capacity=RatedTotCap*Length [W]
  REAL(r64)        :: DesignLatentCap=0.0d0        ! Design latent case capacity=DesignRAtedCap*LatentHeatRatio*RTF [W]
  REAL(r64)        :: DesignDefrostCap=0.0d0       ! Design defrost case capacity=DefrostPower*Length [W]
  REAL(r64)        :: DesignLighting=0.0d0         ! Design case lighting=LightingPower*Length [W]
  REAL(r64)        :: DesignFanPower=0.0d0         ! Design power of case fan=Operatingpower*Length [W]
  REAL(r64)        :: StoredEnergy=0.0d0           ! Cumulative Stored Energy not met by evaporator [J]
  REAL(r64)        :: StoredEnergySaved=0.0d0      ! Cumulative Stored Energy not met by evaporator [J]
  INTEGER          :: CaseCreditFracSchedPtr=0   ! Index to the case credit reduction schedule

! Report Variables
  REAL(r64)        :: TotalCoolingLoad=0.0d0 ! Refrigerated case total cooling rate (W)
  REAL(r64)        :: TotalCoolingEnergy=0.0d0     ! Refrigerated case total cooling energy (J)
  REAL(r64)        :: SensCoolingEnergyRate=0.0d0  ! Refrigerated case sensible cooling rate (W)
  REAL(r64)        :: SensCoolingEnergy=0.0d0      ! Refrigerated case sensible cooling energy (J)
  REAL(r64)        :: LatCoolingEnergyRate=0.0d0   ! Refrigerated case latent cooling rate (W)
  REAL(r64)        :: LatCoolingEnergy=0.0d0       ! Refrigerated case latent cooling energy (J)

  REAL(r64)        :: SensZoneCreditRate=0.d0    ! Refrigerated case sensible zone credit rate (W)
  REAL(r64)        :: SensZoneCreditCoolRate=0.0d0 ! Refrigerated case sensible cooling zone credit rate (W)
  REAL(r64)        :: SensZoneCreditCool=0.0d0     ! Refrigerated case sensible cooling zone credit energy (J)
  REAL(r64)        :: SensZoneCreditHeatRate=0.0d0 ! Refrigerated case sensible heating zone credit rate (W)
  REAL(r64)        :: SensZoneCreditHeat=0.0d0     ! Refrigerated case sensible heating zone credit energy (J)
  REAL(r64)        :: LatZoneCreditRate=0.0d0      ! Refrigerated case latent zone credit rate (W)
  REAL(r64)        :: LatZoneCredit=0.0d0          ! Refrigerated case latent zone credit energy (J)
  REAL(r64)        :: SensHVACCreditRate=0.d0    ! Refrigerated case sensible HVAC credit rate (W)
  REAL(r64)        :: SensHVACCreditCoolRate=0.0d0 ! Refrigerated case sensible cooling HVAC credit rate (W)
  REAL(r64)        :: SensHVACCreditCool=0.0d0     ! Refrigerated case sensible cooling HVAC credit energy (J)
  REAL(r64)        :: SensHVACCreditHeatRate=0.0d0 ! Refrigerated case sensible heating HVAC credit rate (W)
  REAL(r64)        :: SensHVACCreditHeat=0.0d0     ! Refrigerated case sensible heating HVAC credit energy (J)
  REAL(r64)        :: LatHVACCreditRate=0.0d0      ! Refrigerated case latent HVAC credit rate (W)
  REAL(r64)        :: LatHVACCredit=0.0d0          ! Refrigerated case latent HVAC credit energy (J)

  REAL(r64)        :: ElecAntiSweatPower=0.0d0     ! Refrigerated case anti-sweat heater rate (W)
  REAL(r64)        :: ElecAntiSweatConsumption=0.0d0 ! Refrigerated case anti-sweat heater energy (J)
  REAL(r64)        :: ElecFanPower=0.0d0           ! Refrigerated case fan electric power (W)
  REAL(r64)        :: ElecFanConsumption=0.0d0     ! Refrigerated case fan electric energy (J)
  REAL(r64)        :: ElecLightingPower=0.0d0      ! Refrigerated case lighting electric power (W)
  REAL(r64)        :: ElecLightingConsumption=0.0d0 ! Refrigerated case lighting electric energy (J)
  REAL(r64)        :: ElecDefrostPower=0.0d0       ! Refrigerated case defrost rate (W)
  REAL(r64)        :: ElecDefrostConsumption=0.0d0 ! Refrigerated case defrost energy (J)
  REAL(r64)        :: DefEnergyCurveValue=0.0d0    ! Refrigerated case defrost capacity modifier
  REAL(r64)        :: LatEnergyCurveValue=0.0d0    ! Refrigerated case latent capacity modifier
  REAL(r64)        :: MaxKgFrost=0.0d0             ! Amount of frost formation to initiate defrost for On Demand
  REAL(r64)        :: Rcase=0.0d0                  ! Case wall resistance for AS heater calc (h-sqm-C/W)
  REAL(r64)        :: DefrostEnergy = 0.0d0        ! Refrigerated case defrost energy (J)
  REAL(r64)        :: StockingEnergy = 0.0d0       ! Refrigerated case product stocking energy (J)
  REAL(r64)        :: WarmEnvEnergy = 0.0d0        ! Refrigerated case extra sensible energy due to warm zone ambient (J)
  REAL(r64)        :: KgFrost = 0.0d0              ! Amount of frost on case evaporator (Kg)
  REAL(r64)        :: DefrostEnergySaved = 0.0d0   ! Refrigerated case defrost energy (J)
  REAL(r64)        :: StockingEnergySaved = 0.0d0  ! Refrigerated case product stocking energy (J)
  REAL(r64)        :: WarmEnvEnergySaved = 0.0d0   ! Refrigerated case extra sensible energy due to warm zone ambient (J)
  REAL(r64)        :: KgFrostSaved = 0.0d0         ! Amount of frost on case evaporator (Kg)
  REAL(r64)        :: HotDefrostCondCredit = 0.0d0 ! Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
  REAL(r64)        :: DeltaDefrostEnergy = 0.0d0   ! Used to reverse accumulation if the zone/load time step is repeated (J)
END TYPE RefrigCaseData

TYPE, PRIVATE :: RefrigRackData
  LOGICAL    :: CoilFlag            = .FALSE.        ! Flag to show if coil type load on rack
  CHARACTER(len=MaxNameLength) :: Name=' '                   ! Name of Refrigeration Compressor rack
  CHARACTER(len=MaxNameLength) :: SupplyTankName=' '         ! Evap water supply tank name
  CHARACTER(len=MaxNameLength) :: EndUseSubcategory='General'! Rack end-use subcategory
 ! Index of refrigerated case (1 to NumCases) connected to rack #X
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CaseNum
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CoilNum
  INTEGER, ALLOCATABLE, DIMENSION(:) :: WalkInNum
  INTEGER          :: HeatRejectionLocation = 0  ! Refrigeration Compressor Rack heat rejection location
                                                 ! (1=LocationOutdoors or 2=LocationZone)
  INTEGER          :: CondenserType = 0          ! Specifies cooling mode for outdoor condenser
                                                 ! (1=Dry air, 2=Evap cooling, 3=Water-cooled)
  REAL(r64)        :: LaggedUsedWaterHeater      ! Heat reclaim used to heat water in previous zone/load time step(W)
  REAL(r64)        :: LaggedUsedHVACCoil         ! Heat reclaim used to heat HVAC coil in previous zone/load time step(W)
  REAL(r64)        :: EvapEffect=0.9d0             ! Effectiveness of evaporative condenser
  REAL(r64)        :: CondenserAirFlowRate=0.0d0   ! Evaporative condenser air volume flow rate (m3/s)
  REAL(r64)        :: EvapPumpPower=0.0d0          ! Evaporative cooling water pump power (W)
  REAL(r64)        :: ActualEvapPumpPower=0.0d0    ! Evaporative cooling water pump power, if adjusted (W)
  REAL(r64)        :: EvapPumpConsumption=0.0d0    ! Evaporative cooling water pump electric consumption (J)
  REAL(r64)        :: EvapWaterConsumpRate=0.0d0   ! Evaporative condenser water consumption rate (m3/s)
  REAL(r64)        :: EvapWaterConsumption=0.0d0   ! Evaporative condenser water consumption (m3)
  INTEGER          :: EvapSchedPtr=0             ! Index to the correct evap condenser availability schedule
  REAL(r64)        :: BasinHeaterPowerFTempDiff= 0.0d0 ! Basin heater capacity per degree K below setpoint (W/K)
  REAL(r64)        :: BasinHeaterSetPointTemp= 2.0d0 ! Setpoint temperature for basin heater operation (C)
  REAL(r64)        :: BasinHeaterPower=0.0d0       ! Power demand from basin heater (W)
  REAL(r64)        :: BasinHeaterConsumption=0.0d0 ! Electric consumption from basin heater (J)
  REAL(r64)        :: RatedCOP=0.0d0               ! Rated coefficient of performance for compressor rack (W/W)
  INTEGER          :: COPFTempPtr=0              ! Index to the correct COP curve object
  INTEGER          :: NumCases=0                 ! Total number of refrigerated cases attached to each rack
  INTEGER          :: NumCoils=0                 ! Total number of air chillers attached to each rack
  INTEGER          :: NumWalkIns=0               ! Total number of walk-ins attached to each rack
  INTEGER          :: EvapWaterSupplyMode = WaterSupplyFromMains !  Source of water for evap condenser cooling
  INTEGER          :: EvapWaterSupTankID = 0     ! TankID when evap condenser uses water from storage tank
  INTEGER          :: EvapWaterTankDemandARRID=0 ! Demand index when evap condenser uses water from storage tank
  INTEGER          :: OutsideAirNodeNum  = 0     ! Outside air node number
  INTEGER          :: HeatRejectionZoneNum = 0   ! Heat rejection zone number used when walk-ins present and ht rej to zone
  INTEGER          :: HeatRejectionZoneNodeNum = 0 ! Heat rejection zone node number used when walk-ins present and ht rej to zone
  REAL(r64)        :: TotalRackLoad=0.0d0          ! Total capacity of all refrigerated cases on rack
  REAL(r64)        :: RackCompressorCOP=0.0d0      ! Rack compressor COP at specific operating conditions
  REAL(r64)        :: RackCompressorPower=0.0d0    ! Total rack compressor power (W)
  REAL(r64)        :: RackElecConsumption =0.0d0   ! Total rack compressor electric consumption (J)
  REAL(r64)        :: RackCapacity=0.0d0           ! Total rack delivered capacity (W)
  REAL(r64)        :: RackCoolingEnergy=0.0d0      ! Total rack delivered energy (J)
  REAL(r64)        :: CondenserFanPower=0.0d0      ! Condenser fan power (W)
  INTEGER          :: TotCondFTempPtr=0          ! Index for condenser fan power modifier curve
                                                 ! (function of outdoor temperature)
  REAL(r64)        :: ActualCondenserFanPower=0.0d0  ! Rack condenser fan power (W)
  REAL(r64)        :: CondenserFanConsumption=0.0d0  ! Rack condenser fan electric consumption (J)
  REAL(r64)        :: SensZoneCreditHeatRate=0.0d0 ! Rack sensible heating zone credit rate (W)
  REAL(r64)        :: SensZoneCreditHeat=0.0d0     ! Rack sensible heating zone credit energy (J)
  REAL(r64)        :: SensHVACCreditHeatRate=0.0d0 ! Rack sensible heating HVAC credit rate (W)
  REAL(r64)        :: SensHVACCreditHeat=0.0d0     ! Rack sensible heating HVAC credit energy (J)
  INTEGER          :: EvapFreezeWarnIndex = 0    ! Recurring freeze warning index
  INTEGER          :: NoFlowWarnIndex = 0        ! No cooling water when needed warning index
  INTEGER          :: HighTempWarnIndex = 0      ! Water outlet high temp warning index
  INTEGER          :: LowTempWarnIndex = 0       ! Water outlet low temp warning index
  INTEGER          :: HighFlowWarnIndex = 0      ! Water outlet high flow warning index
  INTEGER          :: HighInletWarnIndex = 0     ! Water inlet high temp warning index
  INTEGER          :: InletNode = 0              ! Water-cooled condenser inlet node number
  REAL(r64)        :: InletTemp = 0.0d0            ! Water-cooling condenser inlet temperature (C)
  INTEGER          :: OutletNode = 0             ! Water-cooled condenser outlet node number
  INTEGER          :: PlantTypeOfNum = 0         ! Water-cooled condenser plant equipment type
  INTEGER          :: PlantLoopNum = 0           ! Water-cooled condenser plant loop number
  INTEGER          :: PlantLoopSideNum = 0       ! Water-cooled condenser plant loop side number
  INTEGER          :: PlantBranchNum = 0         ! Water-cooled condenser plant branch number
  INTEGER          :: PlantCompNum   = 0         ! Water-cooled condenser plant component number

  REAL(r64)        :: OutletTemp = 0.0d0           ! Water-cooling condenser outlet temperature (C)

  INTEGER          :: OutletTempSchedPtr = 0     ! Schedule pointer for condenser outlet temp setting
  REAL(r64)        :: VolFlowRate = 0.0d0          ! Water-cooled condenser volumetric flow rate (m3/s)
  REAL(r64)        :: DesVolFlowRate = 0.0d0       ! Water-cooled condenser design volumetric flow rate (m3/s)
  REAL(r64)        :: MassFlowRate = 0.0d0         ! Water-cooled condenser mass flow rate (kg/s)
  REAL(r64)        :: CondLoad = 0.0d0             ! Total condenser load (W)
  REAL(r64)        :: CondEnergy = 0.0d0           ! Condenser energy (J)
  INTEGER          :: FlowType = 1               ! Water-cooled condenser loop flow type
  REAL(r64)        :: VolFlowRateMax = 0.0d0       ! Maximum condenser volumetric flow rate (m3/s)
  REAL(r64)        :: MassFlowRateMax = 0.0d0      ! Maximum condenser mass flow rate (kg/s)
  REAL(r64)        :: InletTempMin = 10.0d0      ! Minimum condenser water inlet temperature (C)
  REAL(r64)        :: OutletTempMax = 55.0d0     ! Maximum condenser water outlet temperature (C)
  REAL(r64)        :: TotalCoolingLoad = 0.0d0
END TYPE RefrigRackData

TYPE, PRIVATE :: RefrigSystemData
  CHARACTER(len=MaxNameLength) :: Name=' '              ! Name of refrigeration system
  CHARACTER(len=MaxNameLength) :: RefrigerantName=' '   ! Name of refrigerant, must match name in FluidName
                                                        !    (see fluidpropertiesrefdata.idf)
  CHARACTER(len=MaxNameLength) :: EndUseSubcategory=' ' ! Used for reporting purposes
  LOGICAL    :: SystemRejectHeatToZone = .FALSE.        ! Flag to show air-cooled condenser located inside zone
  LOGICAL    :: CoilFlag               = .FALSE.        ! Flag to show if coil type load on system (even if below in a secondary)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CascadeLoadNum  ! absolute index  of condensers placing load (allocated NumCondensers)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CaseNum         ! absolute Index of cases (allocated NumCases)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CoilNum         ! absolute Index of coils (allocated NumCoils)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CompressorNum   ! absolute Index of compressors (allocated NumCompressors)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CondenserNum    ! absolute Index of condensers removing load (allocated NumCondensers)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: GasCoolerNum    ! absolute Index of gas cooler
  INTEGER, ALLOCATABLE, DIMENSION(:) :: HiStageCompressorNum  ! absolute Index of high-stage compressors (allocated NumHiStageCompressors)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: SecondaryNum    ! absolute Index of seocndary loops (allocated NumSecondarys)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: SubcoolerNum    ! Absolute Index of subcoolers (allocated NumSubcoolers)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: WalkInNum       ! absolute Index of walk ins (allocated NumWalkIns)
  INTEGER     :: CompSuctControl  = 2       ! Index to suction control
                                            ! 2 =fixed, 1=floating
  !INTEGER     :: ServiceType      = 1       ! Index to warehouse or supermarket
                                            ! 1 = supermarket, 2=warehouse
  INTEGER     :: HiStageWarnIndex1 = 0      ! Recurring warning index when hi stage compressors unable to meet coil loads
  INTEGER     :: HiStageWarnIndex2 = 0      ! Recurring warning index when hi stage compressors unable to meet coil loads
  INTEGER     :: InsuffCapWarn  = 0         ! Recurring warning index when refrigeration system unable to meet coil loads
  INTEGER     :: IntercoolerType = 0        ! Intercooler type (0=none, 1=flash intercooler, 2=shell-and-coil intercooler)
  INTEGER     :: NumCases       = 0         ! Number of cases on this system
  INTEGER     :: NumCoils       = 0         ! Number of cases on this system
  INTEGER     :: NumCompressors = 0         ! Number of compressors on this system for single-stage systems
                                            ! or number of low-stage compressors on this system for two-stage systems
  INTEGER     :: NumCondensers  = 1         ! Number of condensers on this system
  INTEGER     :: NumGasCoolers = 0          ! Number of gas coolers on this system
  INTEGER     :: NumHiStageCompressors = 0  ! Number of high-stage compressors on this system (two-stage systems only)
  INTEGER     :: NumSecondarys = 0          ! Number of secondary loops on this system
  INTEGER     :: NumStages = 1              ! Number of compressor stages
  INTEGER     :: NumSubcoolers  = 0         ! Number of subcoolers on this system
  INTEGER     :: NumWalkIns     = 0         ! Number of walk in coolers on this system
  INTEGER     :: NumMechSCServed  = 0       ! Number of mech subcoolers served/powered by compressor/cond on this system
  INTEGER     :: NumNonCascadeLoads= 0      ! Sum of NumCases, NumWalk-Ins, NumCoils, and NumSecondarys
  INTEGER     :: NumCascadeLoads= 0         ! Number of cascade condensers cooled by this system
  INTEGER     :: NumTransferLoads  = 0      ! Sum of NumCascadeLoads and NumSecondarys
  INTEGER     :: RefIndex         = 0       ! Index number of refrigerant, automatically assigned on first call to fluid property
                                            !   and used thereafter
  INTEGER     :: SuctionPipeActualZoneNum  = 0   ! ID number for zone where suction pipes gain heat
  INTEGER     :: SuctionPipeZoneNodeNum    = 0   ! ID number for zone node where suction pipes gain heat


  REAL(r64), ALLOCATABLE, DIMENSION(:) :: MechSCLoad  ! Mechanical subcooler load on system from other systems(W)
  REAL(r64)   :: AverageCompressorCOP=0.0d0   ! Average COP for compressers on this system (W)
  REAL(r64)   :: CpSatLiqCond = 0.0d0         ! Spec Heat of sat liquid at condensing pressure  (J/kg-C)
  REAL(r64)   :: CpSatVapEvap = 0.0d0         ! Spec Heat of saturated vapor exiting evaporator (J/kg-C)
  REAL(r64)   :: FlowRatioIntercooler = 0.0d0 ! Refrigerant mass flow ratio through coil-side of shell-and-coil intercooler
  REAL(r64)   :: HCaseIn = 0.0d0              ! Case inlet enthalpy (after subcoolers and pipe P drops) (J/kg)
  REAL(r64)   :: HCompIn = 0.0d0              ! Compressor inlet enthalpy  (J/kg)
  REAL(r64)   :: HCompOut = 0.0d0             ! Compressor outlet enthalpy (J/kg)
  REAL(r64)   :: HSatLiqCond = 0.0d0          ! Enthalpy of sat liquid at condensing pressure  (J/kg)
  REAL(r64)   :: HCaseOut = 0.0d0             ! Enthalpy of refrigerant leaving cases, after superheat (J/kg)
  REAL(r64)   :: IntercoolerEffectiveness=0.0d0  ! Shell-and-coil intercooler effectiveness
  REAL(r64)   :: LSHXTrans = 0.0d0            ! Liquid suction subcooler load transferred within same suction group, W
  REAL(r64)   :: LSHXTransEnergy = 0.0d0      ! Liquid suction subcooler load transferred within same suction group, J
  REAL(r64)   :: NetHeatRejectLoad = 0.0d0    ! Portion of TotalCondenser load due to this system (after heat recovery) W
  REAL(r64)   :: NetHeatRejectEnergy = 0.0d0  ! Portion of TotalCondenser energy due to this system (after heat recovery) J
  REAL(r64)   :: Pintercooler = 0.0d0         ! Pressure in the intercooler (two-stage systems only) (Pa)
  REAL(r64)   :: PipeHeatLoad = 0.0d0         ! Total suction pipe heat gains, optional (W)
  REAL(r64)   :: PipeHeatEnergy = 0.0d0       ! Total suction pipe heat gains, optional (J)
  REAL(r64)   :: RefMassFlowtoLoads=0.0d0     ! Total system refrigerant mass flow through cases(kg/s)
  REAL(r64)   :: RefMassFlowComps=0.0d0       ! Total system refrigerant mass flow through compressors(kg/s)
  REAL(r64)   :: RefMassFlowHiStageComps=0.0d0  ! Total system refrigerant mass flow through high-stage compressors(two-stage systems only) (kg/s)
  REAL(r64)   :: RefInventory=0.0d0           ! Approximate refrigerant inventory entered by user (kg)
  REAL(r64)   :: SumMechSCLoad = 0.0d0        ! Total cooling load of all mech subcoolers served by suction group (W)
  REAL(r64)   :: SumMechSCBenefit= 0.0d0      ! Total cooling provided by mech subcoolers cooling liquid condensate in this system (W)
  REAL(r64)   :: SumCascadeCondCredit = 0.0d0 ! Sum of cond cred for hot brine/gas defrost on cases etc served by
                                            !    cascade condenser cooled by this system (W)
  REAL(r64)   :: SumCascadeLoad = 0.0d0       ! Total cooling load of all cascade condensers served by suction group (W)
  REAL(r64)   :: SumSecondaryLoopLoad = 0.0d0 ! Total cooling loads for all secondary loops served by this suction group (W)
  REAL(r64)   :: SumUASuctionPiping   =0.0d0  ! Sum of U*A for system suction piping (W/C)
  REAL(r64)   :: TCaseOut = 0.0d0             ! Case out temperature including case superheat (C)
  REAL(r64)   :: TCondense = 0.0d0            ! Condensing temperature (Tsat for P discharge) (C)
  REAL(r64)   :: TCompIn = 0.0d0              ! Compressor inlet temperature (after case and LSHX superheat and pipe delta P) (C)
  REAL(r64)   :: TCondenseMin = 0.d0         ! Minimum allowed condensing temperature (C)
  REAL(r64)   :: TCondenseMinInput = 0.d0    ! Minimum allowed condensing temperature, user's original input value (C)
  LOGICAL     :: EMSOverrideOnTCondenseMin = .FALSE. ! if true, EMS is calling to override minimum allowed condensing temperature
  REAL(r64)   :: EMSOverrideValueTCondenseMin = 0.d0 ! value to use when EMS override is true [C]
  REAL(r64)   :: TEvapDesign = 0.0d0          ! Min (on sys) design case/walkin/secondary evap temp
                                            !  (also basis for floating evap T calc) (C)
  REAL(r64)   :: TEvapNeeded = 0.0d0          ! Max Case evap temperature to maintain lowest case T on system (C)
  REAL(r64)   :: Tintercooler = 0.0d0         ! Temperature in the intercooler (two-stage systems only) (Pa)
  REAL(r64)   :: TLiqInActual = 0.0d0         ! Actual liquid temperature entering TXV after subcooling (C)
  REAL(r64)   :: TotalCondDefrostCredit=0.0d0 ! sum of heat reclaimed for hot gas and hot brine defrost for
                                            !    cases/WI/sec served directly [W]
  REAL(r64)   :: TotalCoolingEnergy=0.0d0     ! Total energy of all refrigerated cases and walkins served directly (J)
  REAL(r64)   :: TotalCoolingLoad=0.0d0       ! Total load of all refrigerated cases and walkins served directly (W)
  REAL(r64)   :: TotalSystemLoad = 0.0d0      ! Includes cases, walk-ins, and transfer loads (cascade, second, subcooler), W
  REAL(r64)   :: TotCompPower=0.0d0           ! Total power for compressers on this system (for single-stage systems) or
                                            ! total power for low-stage compressors on this system (for two-stage systems) (W)
  REAL(r64)   :: TotCompElecConsump=0.0d0     ! Total Elec consump for compressers on this system (for single-stage systems) or
                                            ! total elec consump for low-stage compressors on this system (for two-stage systems) (J)
  REAL(r64)   :: TotCompCapacity=0.0d0        ! Total design capacity for compressers on this system (for single-stage systems) or
                                            ! total design capacity for low-stage compressors on this system (for two-stage systems) (W)
  REAL(r64)   :: TotCompCoolingEnergy=0.0d0   ! Total cooling energy from compressers on this system (for single-stage systems) or
                                            ! total cooling energy from low-stage compressors on this system (for two-stage systems) (J)
  REAL(r64)   :: TotHiStageCompCapacity=0.0d0 ! Total design capacity for high-stage compressers on this system (two-stage systems only) (W)
  REAL(r64)   :: TotHiStageCompCoolingEnergy=0.0d0 ! Total cooling energy from high-stage compressers on this system (two-stage systems only) (J)
  REAL(r64)   :: TotHiStageCompElecConsump=0.0d0  ! Total Elec consump for high-stage compressers on this system (two-stage systems only) (J)
  REAL(r64)   :: TotHiStageCompPower=0.0d0    ! Total power for high-stage compressers on this system (two-stage systems only) (W)
  REAL(r64)   :: TotCompElecConsumpTwoStage=0.0d0  ! Total Elec consump for the low- and high-stage compressors on this system (two-stage systems only) (J)
  REAL(r64)   :: TotRejectHeatRecovered = 0.0d0 ! Total reject heat recovered for hot gas or hot brine defrost or
                                            !     desuperheater coils (W)
  REAL(r64)   :: TotTransferLoad = 0.0d0      ! Total load from other systems transferred to this sytem, incl mech subcoolers,
                                            ! cascade, and secondary loops (W)
  REAL(r64)   :: TotTransferEnergy= 0.0d0     ! Total energy from other systems transferred to this sytem, incl mech subcoolers,
                                            ! cascade, and secondary loops (J)
  REAL(r64)   :: UnmetEnergy=0.0d0            ! Accumulative loads unmet by total compressors (for single-stage systems) or
                                            ! by low-stage compressors (for two-stage systems) on this system (J)
  REAL(r64)   :: UnmetHiStageEnergy=0.0d0   ! Accumulative loads unmet by total high-stage compressors (two-stage systems only) on this system (J)
  REAL(r64)   :: UnmetEnergySaved=0.0d0       ! Accumulative loads unmet by total compressors (for single-stage systems) on this system (J)
END TYPE RefrigSystemData

TYPE, PRIVATE :: TransRefrigSystemData
  CHARACTER(len=MaxNameLength) :: Name=' '               ! Name of transcritical CO2 refrigeration system
  CHARACTER(len=MaxNameLength) :: RefrigerantName=' '    ! Name of refrigerant, must match name in FluidName
                                                         !    (see fluidpropertiesrefdata.idf)
  CHARACTER(len=MaxNameLength) :: EndUseSubcategory=' '  ! Used for reporting purposes
  LOGICAL    :: SystemRejectHeatToZone = .FALSE.         ! Flag to show air-cooled gas cooler located inside zone
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CaseNumMT        ! absolute Index of medium temperature cases (allocated NumCasesMT)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CaseNumLT        ! absolute Index of low temperature cases (allocated NumCasesLT)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CompressorNumHP  ! absolute Index of high pressure compressors (allocated NumCompressorsHP)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CompressorNumLP  ! absolute Index of low pressure compressors (allocated NumCompressorsLP)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: GasCoolerNum     ! absolute Index of gas cooler
  INTEGER, ALLOCATABLE, DIMENSION(:) :: WalkInNumMT      ! absolute Index of medium temperature walk ins (allocated NumWalkInsMT)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: WalkInNumLT      ! absolute Index of low temperature walk ins (allocated NumWalkInsLT)
  INTEGER     :: NumCasesLT        = 0        ! Number of low temperature cases on this system
  INTEGER     :: NumCasesMT        = 0        ! Number of medium temperature cases on this system
  INTEGER     :: NumCompressorsHP  = 0        ! Number of high pressure compressors on this system
  INTEGER     :: NumCompressorsLP  = 0        ! Number of low pressure compressors on this system
  INTEGER     :: NumGasCoolers     = 1        ! Number of gas coolers on this system
  INTEGER     :: NumWalkInsLT      = 0        ! Number of low temperature walk in coolers on this system
  INTEGER     :: NumWalkInsMT      = 0        ! Number of medium temperature walk in coolers on this system
  INTEGER     :: RefIndex          = 0        ! Index number of refrigerant, automatically assigned on first call to fluid property
                                              !   and used thereafter
  INTEGER     :: SuctionPipeActualZoneNumMT  = 0   ! ID number for zone where medium temperature suction pipes gain heat
  INTEGER     :: SuctionPipeZoneNodeNumMT    = 0   ! ID number for zone node where medium temperature suction pipes gain heat
  INTEGER     :: SuctionPipeActualZoneNumLT  = 0   ! ID number for zone where medium temperature suction pipes gain heat
  INTEGER     :: SuctionPipeZoneNodeNumLT    = 0   ! ID number for zone node where medium temperature suction pipes gain heat
  INTEGER     :: TransSysType = 0             ! Transcritical refrigeration system type: SingleStage = 1, TwoStage=2
  REAL(r64)   :: AverageCompressorCOP=0.0d0   ! Average COP for compressers on this system (W)
  REAL(r64)   :: CpSatLiqCond = 0.0d0         ! Spec Heat of sat liquid at condensing pressure  (J/kg-C)
  REAL(r64)   :: CpSatVapEvapMT = 0.0d0       ! Spec Heat of saturated vapor exiting medium temperature evaporator (J/kg-C)
  REAL(r64)   :: CpSatVapEvapLT = 0.0d0       ! Spec Heat of saturated vapor exiting low temperature evaporator (J/kg-C)
  REAL(r64)   :: CpSatLiqReceiver = 0.0d0     ! Spec Heat of saturated liquid in receiver (J/kg-C)
  REAL(r64)   :: DelHSubcoolerDis = 0.0d0     ! Change in enthalpy across subcooler, hot side (J/kg)
  REAL(r64)   :: DelHSubcoolerSuc = 0.0d0     ! Change in enthalpy across subcooler, cold side (J/kg)
  REAL(r64)   :: HCaseInMT = 0.0d0            ! Medium temperature case inlet enthalpy (after subcoolers and pipe P drops) (J/kg)
  REAL(r64)   :: HCaseInLT = 0.0d0            ! Low temperature case inlet enthalpy (after pipe P drops) (J/kg)
  REAL(r64)   :: HCompInHP = 0.0d0            ! High pressure compressor inlet enthalpy  (J/kg)
  REAL(r64)   :: HCompInLP = 0.0d0            ! Low pressure compressor inlet enthalpy  (J/kg)
  REAL(r64)   :: HCompOutHP = 0.0d0           ! High pressure compressor outlet enthalpy (J/kg)
  REAL(r64)   :: HCompOutLP = 0.0d0           ! Low pressure compressor outlet enthalpy (J/kg)
  REAL(r64)   :: HSatLiqCond = 0.0d0          ! Enthalpy of sat liquid at condensing pressure  (J/kg)
  REAL(r64)   :: HSatLiqReceiver = 0.0d0      ! Enthalpy of sat liquid in receiver (J/kg)
  REAL(r64)   :: HCaseOutMT = 0.0d0           ! Enthalpy of refrigerant leaving medium temperature cases, after superheat (J/kg)
  REAL(r64)   :: HCaseOutLT = 0.0d0           ! Enthalpy of refrigerant leaving low temperature cases, after superheat (J/kg)
  REAL(r64)   :: NetHeatRejectLoad = 0.0d0    ! Portion of TotalCondenser load due to this system (after heat recovery) W
  REAL(r64)   :: NetHeatRejectEnergy = 0.0d0  ! Portion of TotalCondenser energy due to this system (after heat recovery) J
  REAL(r64)   :: PipeHeatLoadMT = 0.0d0       ! Total medium temperature suction pipe heat gains, optional (W)
  REAL(r64)   :: PipeHeatLoadLT = 0.0d0       ! Total low temperature suction pipe heat gains, optional (W)
  REAL(r64)   :: PipeHeatEnergy = 0.0d0       ! Total suction pipe heat gains, optional (J)
  REAL(r64)   :: PipeHeatEnergyMT = 0.0d0     ! Total medium temperature suction pipe heat gains, optional (J)
  REAL(r64)   :: PipeHeatEnergyLT = 0.0d0     ! Total low temperature suction pipe heat gains, optional (J)
  REAL(r64)   :: RefMassFlowtoMTLoads = 0.0d0 ! Refrigerant mass flow through medium temperature cases(kg/s)
  REAL(r64)   :: RefMassFlowtoLTLoads = 0.0d0 ! Refrigerant mass flow through low temperature cases(kg/s)
  REAL(r64)   :: RefMassFlowCompsHP = 0.0d0   ! Total system refrigerant mass flow through high pressue compressors(kg/s)
  REAL(r64)   :: RefMassFlowCompsLP = 0.0d0   ! Total system refrigerant mass flow through low pressue compressors(kg/s)
  REAL(r64)   :: RefMassFlowComps = 0.0d0     ! Total system refrigerant mass flow through all compressors (kg/s)
  REAL(r64)   :: RefMassFlowReceiverByPass = 0.0d0  ! Refrigerant mass flow through receiver bypass (kg/s)
  REAL(r64)   :: RefInventory = 0.0d0         ! Approximate refrigerant inventory entered by user (kg)
  REAL(r64)   :: SCEffectiveness = 0.0d0      ! Heat exchanger effectiveness of the subcooler
  REAL(r64)   :: SumUASuctionPipingMT = 0.0d0 ! Sum of U*A for medium temperature suction piping (W/C)
  REAL(r64)   :: SumUASuctionPipingLT = 0.0d0 ! Sum of U*A for low temperature suction piping (W/C)
  REAL(r64)   :: TCaseOutMT = 0.0d0           ! Medium temperature case out temperature including case superheat (C)
  REAL(r64)   :: TCaseOutLT = 0.0d0           ! Low temperature case out temperature including case superheat (C)
  REAL(r64)   :: TCondense = 0.0d0            ! Condensing temperature (Tsat for P discharge) (C)
  REAL(r64)   :: TReceiver = 0.0d0            ! Temperature in receiver (Tsat for P receiver) (C)
  REAL(r64)   :: PReceiver = 0.0d0            ! Pressure in receiver (Psat for T receiver) (C)
  REAL(r64)   :: TCompInHP = 0.0d0            ! High pressure compressor inlet temperature (after case and LSHX superheat and pipe delta P) (C)
  REAL(r64)   :: TCompInLP = 0.0d0            ! Low pressure compressor inlet temperature (after case and pipe delta P) (C)
  REAL(r64)   :: TCondenseMin = 0.0d0         ! Minimum allowed condensing temperature (C)
  REAL(r64)   :: TEvapDesignMT = 0.0d0        ! Min (on sys) design medium temperature case/walkin/secondary evap temp
  REAL(r64)   :: TEvapDesignLT = 0.0d0        ! Min (on sys) design low temperature case/walkin/secondary evap temp
  REAL(r64)   :: TEvapNeededMT = 0.0d0        ! Max MT Case evap temperature to maintain lowest case T on system (C)
  REAL(r64)   :: TEvapNeededLT = 0.0d0        ! Max LT Case evap temperature to maintain lowest case T on system (C)
  REAL(r64)   :: TLiqInActual = 0.0d0         ! Actual liquid temperature entering TXV after subcooling (C)
  REAL(r64)   :: TotalCondDefrostCredit = 0.0d0  ! sum of heat reclaimed for hot gas and hot brine defrost for cases/WI served directly [W]
  REAL(r64)   :: TotalCoolingEnergy = 0.0d0   ! Total energy of all refrigerated cases and walkins served directly (J)
  REAL(r64)   :: TotalCoolingEnergyMT = 0.0d0 ! Total energy of all medium temperature refrigerated cases and walkins served directly (J)
  REAL(r64)   :: TotalCoolingEnergyLT = 0.0d0 ! Total energy of all low temperature refrigerated cases and walkins served directly (J)
  REAL(r64)   :: TotalCoolingLoadMT = 0.0d0   ! Total medium temperature load of all refrigerated cases and walkins served directly (W)
  REAL(r64)   :: TotalCoolingLoadLT = 0.0d0   ! Total low temperature load of all refrigerated cases and walkins served directly (W)
  REAL(r64)   :: TotalSystemLoad = 0.0d0      ! Sum of MT and LT loads, W
  REAL(r64)   :: TotalSystemLoadMT = 0.0d0    ! Includes medium temperature cases and walk-ins, W
  REAL(r64)   :: TotalSystemLoadLT = 0.0d0    ! Includes low temperature cases and walk-ins, W
  REAL(r64)   :: TotCompPowerHP = 0.0d0       ! Total power for high pressure compressers on this system (W)
  REAL(r64)   :: TotCompPowerLP = 0.0d0       ! Total power for low pressure compressers on this system (W)
  REAL(r64)   :: TotCompElecConsump = 0.0d0   ! Total Elec consump for compressers on this system (J)
  REAL(r64)   :: TotCompElecConsumpHP = 0.0d0 ! Total Elec consumption for high pressure compressors on this system (J)
  REAL(r64)   :: TotCompElecConsumpLP = 0.0d0 ! Total Elec consumption for low pressure compressors on this system (J)
  REAL(r64)   :: TotCompCapacity = 0.0d0      ! Sum of HP and LP compressor capacity (W)
  REAL(r64)   :: TotCompCapacityHP = 0.0d0    ! Total design capacity for high pressure compressers on this system (W)
  REAL(r64)   :: TotCompCapacityLP = 0.0d0    ! Total design capacity for low pressure compressers on this system (W)
  REAL(r64)   :: TotCompCoolingEnergy = 0.0d0 ! Total cooling energy from compressers on this system (J)
  REAL(r64)   :: TotCompCoolingEnergyHP = 0.0d0  ! Total cooling energy from high pressure compressers on this system (J)
  REAL(r64)   :: TotCompCoolingEnergyLP = 0.0d0  ! Total cooling energy from low pressure compressers on this system (J)
  REAL(r64)   :: TotRejectHeatRecovered = 0.0d0  ! Total reject heat recovered for hot gas or hot brine defrost (W)
  REAL(r64)   :: UnmetEnergy = 0.0d0          ! Accumulative loads unmet by the LP and HP compressors on this system (J)
  REAL(r64)   :: UnmetEnergyMT = 0.0d0        ! Accumulative loads unmet by total HP compressors on this system (J)
  REAL(r64)   :: UnmetEnergyLT = 0.0d0        ! Accumulative loads unmet by total LP compressors on this system (J)
  REAL(r64)   :: UnmetEnergySaved =0.0d0      ! Accumulative loads unmet by the LP and HP compressors on this system (J)
  REAL(r64)   :: UnmetEnergySavedMT = 0.0d0   ! Accumulative loads unmet by total HP compressors on this system (J)
  REAL(r64)   :: UnmetEnergySavedLT = 0.0d0   ! Accumulative loads unmet by total LP compressors on this system (J)
END TYPE TransRefrigSystemData

TYPE, PRIVATE ::  CaseAndWalkInListDef                         ! Derived Type for CaseAndWalkIn Lists
     CHARACTER(len=MaxNameLength)   :: Name            =' '    ! Name of this CaseAndWalkIn List
     INTEGER                        :: NumCases   = 0    ! Number of Cases in this CaseAndWalkIn List
     INTEGER                        :: NumCoils   = 0    ! Number of Coils in this CaseAndWalkIn List
     INTEGER                        :: NumWalkIns = 0    ! Number of WalkIns in this CaseAndWalkIn List
     INTEGER, ALLOCATABLE, DIMENSION(:) :: CaseItemNum  ! List of Item numbers that correspond to each Case
     INTEGER, ALLOCATABLE, DIMENSION(:) :: CoilItemNum  ! List of Item numbers that correspond to each Coil
     INTEGER, ALLOCATABLE, DIMENSION(:) :: WalkInItemNum ! List of Item numbers that correspond to each WalkIn
END TYPE CaseAndWalkInListDef


TYPE, PRIVATE ::  CompressorListDef                           ! Derived Type for Compressor Lists
     CHARACTER(len=MaxNameLength)   :: Name             =' ' ! Name of this Compressor List
     INTEGER                        :: NumCompressors = 0  ! Number of Compressors in this Node List
     INTEGER, ALLOCATABLE, DIMENSION(:) :: CompItemNum       ! List of Item numbers that correspond to the compressors
END TYPE  CompressorLISTDef

TYPE, PRIVATE :: RefrigCondenserData
  CHARACTER(len=MaxNameLength) :: Name=' '  ! Name of condenser
  CHARACTER(len=MaxNameLength) :: SupplyTankName=' '         ! Evap water supply tank name
  CHARACTER(len=MaxNameLength) :: EndUseSubcategory='General'! Rack end-use subcategory
  LOGICAL    :: CondenserRejectHeatToZone = .FALSE.        ! Flag to show air-cooled condenser located inside zone
  LOGICAL    :: CoilFlag               = .FALSE. ! Flag to show if coil type load on system served by condenser
  INTEGER, ALLOCATABLE, DIMENSION(:) :: SysNum ! absolute Index of system placing load (allocated NumRefrigSystems)
  INTEGER     :: NumSysAttach=0             ! Number of systems attached to condenser, error if /=1
  INTEGER     :: CondenserType = 0          ! Specifies cooling mode for condenser
                                            ! (1=Dry air, 2=Evap cooling, 3=Water-cooled, 4=Cascade)
  INTEGER     :: EvapFreezeWarnIndex = 0    ! Recurring freeze warning index
  INTEGER     :: FlowType = 1               ! Water-cooled condenser loop flow type
  INTEGER     :: CondCreditWarnIndex1 = 0   ! Used to count warnings
  INTEGER     :: CondCreditWarnIndex2 = 0   ! Used to count warnings
  INTEGER     :: CondCreditWarnIndex3 = 0   ! Used to count warnings
  INTEGER     :: CondCreditWarnIndex4 = 0   ! Used to count warnings
  INTEGER     :: CondCreditWarnIndex5 = 0   ! Used to count warnings
  INTEGER     :: CondCreditWarnIndex6 = 0   ! Used to count warnings
  INTEGER     :: CondCreditWarnIndex7 = 0   ! Used to count warnings
  INTEGER     :: NoFlowWarnIndex = 0        ! No cooling water when needed warning index
  INTEGER     :: HighTempWarnIndex = 0      ! Water outlet high temp warning index
  INTEGER     :: LowTempWarnIndex = 0       ! Water outlet low temp warning index
  INTEGER     :: HighFlowWarnIndex = 0      ! Water outlet high flow warning index
  INTEGER     :: HighInletWarnIndex = 0     ! Water inlet high temp warning index
  INTEGER     :: InletNode = 0              ! Water-cooled condenser inlet node number
  INTEGER     :: EvapSchedPtr=0             ! Index to the correct evap condenser availability schedule
  INTEGER     :: EvapWaterSupplyMode = WaterSupplyFromMains !  Source of water for evap condenser cooling
  INTEGER     :: EvapWaterSupTankID = 0     ! TankID when evap condenser uses water from storage tank
  INTEGER     :: EvapWaterTankDemandARRID=0 ! Demand index when evap condenser uses water from storage tank
  INTEGER     :: OutletNode = 0             ! Water-cooled condenser outlet node number
  INTEGER     :: PlantTypeOfNum = 0         ! Water-cooled condenser plant equipment type
  INTEGER     :: PlantLoopNum = 0           ! Water-cooled condenser plant loop number
  INTEGER     :: PlantLoopSideNum = 0       ! Water-cooled condenser plant loop side number
  INTEGER     :: PlantBranchNum = 0         ! Water-cooled condenser plant branch number
  INTEGER     :: PlantCompNum   = 0         ! Water-cooled condenser plant component number
  INTEGER     :: OutletTempSchedPtr = 0     ! Schedule pointer for condenser outlet temp setting
  INTEGER     :: InletAirNodeNum  = 0       ! Inlet air node number, can be outside or in a zone
  INTEGER     :: InletAirZoneNum  = 0       ! Inlet air zone number, if located in a zone
  INTEGER     :: FanSpeedControlType  = 0   ! fixed, two-speed, or variable
  INTEGER     :: CapCurvePtr  = 0           ! capcity curve pointer for air-cooled condensers
  INTEGER     :: CascadeSysID  = 0          ! System ID number for system rejecting heat to cascade condenser
  INTEGER     :: CascadeTempControl  = 0    ! Determines whether cascade condenser evaporating temperature set by
                                            ! Tevap for other loads on system (=2) or set at a constant (= 1)
  INTEGER     :: CascadeSinkSystemID= 0     ! System ID number for system absorbing condenser heat
  !INTEGER     :: ServiceType      = 1       ! Index to warehouse or supermarket (only applies to cascade condensers)
                                            ! 1 = supermarket, 2=warehouse
  REAL(r64)   :: CascadeRatedEvapTemp = 0.0d0 ! Rated evaporating temperature in cascade condenser
  REAL(r64)   :: MinCondLoad = 0.0d0          ! minimun condenser load for air-cooled cond (W)
  REAL(r64)   :: TempSlope = 0.0d0            ! slope for deltaT as function of heat rej for air-cooled cond (C/W)
  REAL(r64)   :: EvapEffect=0.9d0           ! Effectiveness of evaporative condenser
  REAL(r64)   :: RatedAirFlowRate=0.0d0       ! Evaporative condenser air volume flow rate (m3/s)
  REAL(r64)   :: EvapPumpPower=0.0d0          ! Evaporative cooling water pump power (W)
  REAL(r64)   :: ActualEvapPumpPower=0.0d0    ! Evaporative cooling water pump power, if adjusted (W)
  REAL(r64)   :: EvapPumpConsumption=0.0d0    ! Evaporative cooling water pump electric consumption (J)
  REAL(r64)   :: EvapWaterConsumpRate=0.0d0   ! Evaporative condenser water consumption rate (m3/s)
  REAL(r64)   :: EvapWaterConsumption=0.0d0   ! Evaporative condenser water consumption (m3)
  REAL(r64)   :: BasinHeaterPowerFTempDiff= 0.0d0 ! Basin heater capacity per degree K below setpoint (W/K)
  REAL(r64)   :: BasinHeaterSetPointTemp= 2.0d0 ! Setpoint temperature for basin heater operation (C)
  REAL(r64)   :: BasinHeaterPower=0.0d0       ! Power demand from basin heater (W)
  REAL(r64)   :: BasinHeaterConsumption=0.0d0 ! Electric consumption from basin heater (J)
  REAL(r64)   :: FanMinAirFlowRatio = 0.0d0   ! Minimum power fraction for fan (dimensionless between 0 and 1.0)
  REAL(r64)   :: RatedFanPower=0.0d0          ! Rated Condenser fan power (W)
  REAL(r64)   :: ActualFanPower=0.0d0         ! Condenser fan power (W)
  REAL(r64)   :: FanElecEnergy=0.0d0          ! Condenser fan electric consumption (J)
  REAL(r64)   :: InletTemp = 0.0d0            ! Water-cooling condenser inlet temperature (C)
  REAL(r64)   :: OutletTemp = 0.0d0           ! Water-cooling condenser outlet temperature (C)
  REAL(r64)   :: VolFlowRate = 0.0d0          ! Water-cooled condenser volumetric flow rate (m3/s)
  REAL(r64)   :: DesVolFlowRate = 0.0d0       ! Water-cooled condenser design volumetric flow rate (m3/s)
  REAL(r64)   :: MassFlowRate = 0.0d0         ! Water-cooled condenser water mass flow rate (kg/s)
  REAL(r64)   :: RatedTCondense = 0.0d0       ! Condenser rated saturated condensing Temperature (C)
  REAL(r64)   :: CondLoad = 0.0d0             ! Total condenser load (W)
  REAL(r64)   :: CondEnergy = 0.0d0           ! Condenser energy (J)
  REAL(r64)   :: VolFlowRateMax = 0.0d0       ! Maximum condenser volumetric flow rate (m3/s)
  REAL(r64)   :: MassFlowRateMax = 0.0d0      ! Maximum condenser mass flow rate (kg/s)
  REAL(r64)   :: InletTempMin = 10.0d0      ! Minimum condenser water inlet temperature (C)
  REAL(r64)   :: OutletTempMax = 55.0d0     ! Maximum condenser water outlet temperature (C)
  REAL(r64)   :: RatedSubcool = 0.d0        ! Subcooling included in capacity rating curves (C)
  REAL(r64)   :: RatedDelT = 0.d0           ! Rated difference between Tcondense and Tdrybulb for air-cooled (C)
                                            ! Rated difference between Tcondense and Twetbulb for evap-cooled (C)
  REAL(r64)   :: RatedCapacity = 0.d0       ! Rated heat rejection capacity (W)
  REAL(r64)   :: RatedWaterInletT = 0.d0    ! Rated water inlet temperature (C)
  REAL(r64)   :: RatedApproachT = 0.d0      ! Rated approach temperature difference for water-cooled or cascade condenser(C)
  REAL(r64)   :: MinCapFacEvap = 0.d0       ! HRCF equation limit
  REAL(r64)   :: MaxCapFacEvap = 0.d0       ! HRCF equation limit
  REAL(r64)   :: EvapCoeff1=0.d0            ! First coefficienct in evap condenser approach T difference equn (C)
  REAL(r64)   :: EvapCoeff2=0.d0            ! Second coefficienct in evap condenser approach T difference equn (C)
  REAL(r64)   :: EvapCoeff3=0.d0            ! Third coefficienct in evap condenser approach T difference equn (C)
  REAL(r64)   :: EvapCoeff4=0.d0            ! Fourth coefficienct in evap condenser approach T difference equn (dimensionless)
  REAL(r64)   :: EvapElevFact=1.0d0         ! Elevation correction factor for evap condensers
  REAL(r64)   :: RefOpCharge=0.d0           ! Condenser refrigerant operating charge, kg
  REAL(r64)   :: RefReceiverInventory =0.d0 ! Condensate receiver refrigerant inventory, kg
  REAL(r64)   :: RefPipingInventory =0.d0   ! Condensate piping refrigerant inventory, kg
  REAL(r64)   :: TotalHeatRecoveredEnergy =0.d0 ! All recovered heat for external loads and defrost purposes, J
  REAL(r64)   :: TotalHeatRecoveredLoad =0.d0  ! All recovered heat for external loads and defrost purposes [W]
  REAL(r64)   :: ExternalEnergyRecovered =0.d0 ! ExternalHeatRecovered, J
  REAL(r64)   :: InternalEnergyRecovered =0.d0 ! InternalHeatRecovered, J
  REAL(r64)   :: ExternalHeatRecoveredLoad =0.d0  ! Sum of LaggedUsedWaterHeater and LaggedUsedHVACCoil [W]
  REAL(r64)   :: InternalHeatRecoveredLoad =0.d0  ! Sum of all heat recovered for defrost purposes [W]
  REAL(r64)   :: LaggedUsedWaterHeater =0.d0      ! Heat reclaim used to heat water in previous zone/load time step(W)
  REAL(r64)   :: LaggedUsedHVACCoil =0.d0   ! Heat reclaim used to heat HVAC coil in previous zone/load time step(W)
END TYPE RefrigCondenserData

TYPE, PRIVATE :: RefrigGasCoolerData
  CHARACTER(len=MaxNameLength) :: Name=' '                    ! Name of gas cooler
  CHARACTER(len=MaxNameLength) :: EndUseSubcategory='General' ! Gas cooler end-use subcategory
  LOGICAL    :: GasCoolerRejectHeatToZone = .FALSE.           ! Flag to show gas cooler located inside zone
  LOGICAL    :: TransOpFlag = .FALSE.              ! Flag to show transcritical (vs subcritical) operation of the refrigeration system
  INTEGER, ALLOCATABLE, DIMENSION(:) :: SysNum     ! absolute Index of system placing load (allocated NumRefrigSystems)
  INTEGER     :: CapCurvePtr = 0                   ! capcity curve pointer for gas cooler
  INTEGER     :: FanSpeedControlType = 0           ! fixed, two-speed, or variable
  INTEGER     :: GasCoolerCreditWarnIndex = 0      ! Used to count warnings
  INTEGER     :: InletAirNodeNum = 0               ! Inlet air node number, can be outside or in a zone
  INTEGER     :: InletAirZoneNum = 0               ! Inlet air zone number, if located in a zone
  INTEGER     :: NumSysAttach=0                    ! Number of systems attached to gas cooler
  REAL(r64)   :: ActualFanPower = 0.0d0            ! Actual gas cooler fan power (W)
  REAL(r64)   :: CpGasCoolerOut = 0.0d0            ! Saturated liquid specific heat at gas cooler outlet (J/kg-C)
  REAL(r64)   :: FanElecEnergy = 0.0d0             ! Gas cooler fan electric consumption (J)
  REAL(r64)   :: FanMinAirFlowRatio = 0.0d0        ! Minimum power fraction for fan (dimensionless between 0 and 1.0)
  REAL(r64)   :: GasCoolerApproachT = 3.0d0        ! Gas cooler approach temperature (C)
  REAL(r64)   :: GasCoolerEnergy = 0.0d0           ! Gas cooler energy (J)
  REAL(r64)   :: GasCoolerLoad = 0.0d0             ! Total gas cooler load (W)
  REAL(r64)   :: HGasCoolerOut = 0.0d0             ! Specific enthalpy at the gas cooler outlet (C)
  REAL(r64)   :: InternalEnergyRecovered = 0.0d0   ! InternalHeatRecovered, J
  REAL(r64)   :: InternalHeatRecoveredLoad = 0.0d0 ! Sum of all heat recovered for defrost purposes [W]
  REAL(r64)   :: MinCondLoad = 0.0d0               ! minimun gas cooler load for air-cooled gas cooler (W)
  REAL(r64)   :: MinCondTemp = 1.0d1               ! Minimum condensing temperature during subcritical operation (C)
  REAL(r64)   :: PGasCoolerOut = 0.0d0             ! Optimum pressure at the gas cooler outlet (C)
  REAL(r64)   :: RatedApproachT = 3.0d0            ! Rated approach temperature difference(C)
  REAL(r64)   :: RatedCapacity = 0.0d0             ! Rated heat rejection capacity (W)
  REAL(r64)   :: RatedFanPower = 0.0d0             ! Rated gas cooler fan power (W)
  REAL(r64)   :: RatedOutletP = 9.0d6              ! Rated gas cooler outlet pressure (Pa)
  REAL(r64)   :: RatedOutletT = 38.0d0             ! Rated gas cooler outlet temperature (C)
  REAL(r64)   :: RefOpCharge = 0.0d0               ! Gas cooler refrigerant operating charge, kg
  REAL(r64)   :: RefPipingInventory =0.0d0         ! Gas cooler outlet piping refrigerant inventory, kg
  REAL(r64)   :: RefReceiverInventory = 0.0d0      ! Gas cooler receiver refrigerant inventory, kg
  REAL(r64)   :: SubcriticalTempDiff = 1.0d1       ! Temperature difference for subcritical operation (C)
  REAL(r64)   :: TempSlope = 0.0d0                 ! slope for deltaT as function of heat rej for gas cooler (C/W)
  REAL(r64)   :: TGasCoolerOut = 0.0d0             ! Temperature at the gas cooler outlet (C)
  REAL(r64)   :: TotalHeatRecoveredEnergy = 0.0d0  ! All recovered heat for defrost purposes, J
  REAL(r64)   :: TotalHeatRecoveredLoad = 0.0d0    ! All recovered heat for defrost purposes [W]
  REAL(r64)   :: TransitionTemperature = 0.0d0     ! Transition temperature between subcritical and transcritical operation (C)
!  REAL(r64)   :: ExternalEnergyRecovered = 0.0d0   ! ExternalHeatRecovered, J
!  REAL(r64)   :: ExternalHeatRecoveredLoad = 0.0d0 ! Sum of LaggedUsedWaterHeater and LaggedUsedHVACCoil [W]
!  REAL(r64)   :: LaggedUsedWaterHeater =0.0d0      ! Heat reclaim used to heat water in previous zone/load time step(W)
!  REAL(r64)   :: LaggedUsedHVACCoil =0.0d0         ! Heat reclaim used to heat HVAC coil in previous zone/load time step(W)
END TYPE RefrigGasCoolerData

TYPE, PRIVATE :: RefrigCompressorData
  LOGICAL    :: CoilFlag         = .FALSE.  ! Flag to show if coil type load on system served by compressor
  CHARACTER(len=MaxNameLength) :: Name=' '  ! Name of compressor
  INTEGER  :: CapacityCurvePtr   = 0      ! Index to the capacity curve object
  INTEGER  :: ElecPowerCurvePtr  = 0      ! Index to the electrical power curve object
  INTEGER  :: MassFlowCurvePtr   = 0      ! Index to the mass flow curve object
  INTEGER  :: TransElecPowerCurvePtr=0    ! Index to the transcritical electrical power curve object
  INTEGER  :: TransCapacityCurvePtr=0     ! Index to the transcritical capacity curve object
  INTEGER  :: NumSysAttach       = 0      ! Number of systems attached to compressor, error if /=1
  INTEGER  :: SuperheatRatingType= 0      ! Type of manufacturer's rating info re superheat
  INTEGER  :: SubcoolRatingType  = 0      ! Type of manufacturer's rating info re subcooling
  REAL(r64) :: Capacity          = 0.0d0    ! Comprssor delivered capacity (W)
  REAL(r64) :: CoolingEnergy     = 0.0d0    ! Compressor delivered energy (J)
  REAL(r64) :: Efficiency        = 0.0d0    ! Compressor efficiency (0 to 1)
  REAL(r64) :: ElecConsumption   = 0.0d0    ! Compressor electric consumption (J)
  REAL(r64) :: LoadFactor        = 0.0d0    ! Fraction of the time the compressor runs to meet the load (0 to 1)
  REAL(r64) :: MassFlow          = 0.0d0    ! Compressor mass flow (kg/s)
  REAL(r64) :: NomCap            = 0.0d0    ! Nominal compressor capacity at ARI 540 rating conditions
  REAL(r64) :: Power             = 0.0d0    ! Compressor power (W)
  REAL(r64) :: RatedSuperheat    = 0.0d0    ! Rated Superheat at compressor suction (C)
  REAL(r64) :: RatedSubcool      = 0.0d0    ! Rated Subcooling, note may not match condenser rating (C)
  CHARACTER(len=MaxNameLength) :: EndUseSubcategory='General'! Compressor end-use subcategory
  LOGICAL   :: TransFlag = .FALSE.          ! Flag to indicate if compressor can operate in transcritical region

END TYPE RefrigCompressorData


TYPE, PRIVATE :: CaseRAFractionData
  REAL(r64)                    :: TotalCaseRAFraction = 0.0d0  ! Sum case return air fraction for error checking
  CHARACTER(len=MaxNameLength) :: ZoneName=' '               ! Zone or Location of Refrigerated Case
END TYPE CaseRAFractionData

TYPE, PRIVATE :: SubcoolerData
  LOGICAL    :: CoilFlag            = .FALSE.        ! Flag to show if coil type load on system served by subcooler
  CHARACTER(len=MaxNameLength)         :: Name=' '          ! Name of Subcooler
  CHARACTER(len=MaxNameLength)         :: MechSourceSys=' ' ! Name of refrigeration system providing
                                          ! cool liquid to mechanical, needed for character comparison after systems read
  INTEGER   :: SubcoolerType       = 0    ! Specifies subcooler type(0=liquid suction heat exchanger,1=mechanical)
  INTEGER   :: MechSourceSysID     = 0    ! ID number of refrigeration system providing cool liquid to mechanical
  REAL(r64) :: MechSCTransLoad     = 0.0d0  ! Mechanical subcooler load transferred between suction groups, W
  REAL(r64) :: MechSCTransEnergy   = 0.0d0  ! Mechanical subcooler energy transferred between suction groups, W
  REAL(r64) :: LiqSuctDesignDelT   = 0.0d0  ! Liquid suction subcooler design subcooling, C
  REAL(r64) :: LiqSuctDesignTliqIn = 0.0d0  ! Liquid suction subcooler design inlet temperature liquid, C
  REAL(r64) :: LiqSuctDesignTvapIn = 0.0d0  ! Liquid suction subcooler design inlet temperature vapor, C
  REAL(r64) :: MechControlTliqOut  = 0.0d0  ! Mechanical subcooler design outlet temperature subcooled liquid, C
END TYPE SubcoolerData

TYPE, PRIVATE :: SecondaryLoopData
  LOGICAL    :: CoilFlag            = .FALSE.        ! Flag to show if coil type load on secondary system
  CHARACTER(len=MaxNameLength) :: Name=' '           ! Name of refrigeration system
  CHARACTER(len=MaxNameLength) :: FluidName=' '      ! Name of circulating fluid
  CHARACTER(len=MaxNameLength) :: EndUseSubcategory=' ' ! Used for reporting purposes
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CaseNum    ! Absolute Index of cases (dimensioned 1 to NumCases)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CoilNum    ! Absolute Index of coils (dimensioned 1 to NumCoils)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: WalkInNum    ! Absolute Index of walk-ins (dimensioned 1 to NumWalkIns)
  INTEGER     :: DistPipeZoneNum    = 0   ! ID number for zone where distribution pipe gain heat
  INTEGER     :: DistPipeZoneNodeNum  = 0 ! ID number for zone node where distribution pipe gain heat
  REAL(r64)   :: DistPipeZoneHeatGain = 0.d0 !! sensible heat gain rate to zone with pipe
  INTEGER     :: FluidType          = 0   ! Indicates whether fluid always liquid or undergoes phase change
  INTEGER     :: FluidID            = 0   ! Numerical ID used for calls to properties subroutine
  INTEGER     :: NumSysAttach       = 0   ! Used to check for non-unique and unused secondary loops
  INTEGER     :: NumPumps           = 0   ! Number of pumps (or pump stages) serving this system
  INTEGER     :: NumCases           = 0   ! Number of Cases served by this secondary loop
  INTEGER     :: NumCoils           = 0   ! Number of Cases served by this secondary loop
  INTEGER     :: NumWalkIns         = 0   ! Number of Walk-Ins served by this secondary loop
  INTEGER     :: PumpControlType    = 0   ! Constant speed or variable speed
  INTEGER     :: ReceiverZoneNum    = 0   ! ID number for zone where receiver gains heat
  INTEGER     :: ReceiverZoneNodeNum = 0  ! ID number for zone node where receiver gains heat
  REAL(r64)   :: ReceiverZoneHeatGain= 0.d0 ! sensible heat gain rate to zone with receiver
  !INTEGER     :: ServiceType        = 1   ! Index to warehouse or supermarket
                                          ! 1 = supermarket, 2=warehouse
  INTEGER     :: VarSpeedCurvePtr   =0    ! Pointer for variable speed pump power curve
  REAL(r64)   :: AvailLoadCoils     =0.0d0  ! Used to determine amount of avail heat for warehouse coils
  REAL(r64)   :: CpBrineRated       =0.0d0  ! Specific heat of secondary loop fluid at rated average
                                          !    brine temperature (J/kg-C)
  REAL(r64)   :: ChillerRefInventory = 0.0d0 ! Refrigerant inventory on cold side of loop heat exchanger
  REAL(r64)   :: CircRate           = 0.0d0   ! For PhaseChange loop = mass flow at pump/mass gas out load (dimensionless)
  REAL(r64)   :: CoolingLoadRated   =0.0d0  ! Rated capacity of heat exchanger serving secondary loop (W)
  REAL(r64)   :: DensityBrineRated  =0.0d0  ! Density of secondary loop fluid at
                                          !    rated average brine temperature (J/kg-C)
  REAL(r64)   :: DistPipeHeatGain   =0.0d0  ! Secondary fluid distribution piping heat gain (W)
  REAL(r64)   :: DistPipeHeatGainEnergy   =0.0d0  ! Secondary fluid distribution piping heat gain (J)
  REAL(r64)   :: FlowVolActual     =0.0d0  ! Actual Mass flow rate of circ fluid(kg/s)
  REAL(r64)   :: HotDefrostCondCredit = 0.0d0 ! Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
  REAL(r64)   :: HeatExchangeEta    =0.0d0  ! Heat exchanger effectiveness (dimensionless)
  REAL(r64)   :: MaxVolFlow         =0.0d0  ! Defined by minimum of chiller or pump ratings (m3/s)
  REAL(r64)   :: MaxLoad            =0.0d0  ! Defined by minimum of chiller rating or loat at MaxVolFlow (W)
  REAL(r64)   :: PumpTotRatedPower  =0.0d0  ! Total pump rated power on loop (W)
  REAL(r64)   :: PumpPowerToHeat    =0.0d0  ! Fraction of pump power converted to heat in circ fluid (dimensionless)
  REAL(r64)   :: PumpIncrementFlowVol=0.0d0 ! Circ fluid flow for each pump or pump stage (m3/s)
  REAL(r64)   :: PumpIncrementPower =0.0d0 ! Pump power for each pump or pump stage (W)
  REAL(r64)   :: PumpPowerTotal     =0.0d0  ! Total Pump Power Secondary Loop (report variable)(W)
  REAL(r64)   :: PumpElecEnergyTotal =0.0d0 ! Total pump energy secondary loop (report variable)(W)
  REAL(r64)   :: ReceiverHeatGain   =0.0d0  ! Secondary fluid Receiver heat gain (W)
  REAL(r64)   :: ReceiverHeatGainEnergy  =0.0d0  ! Secondary fluid Receiver heat gain (J)
  REAL(r64)   :: RefInventory       =0.0d0  ! Approximate refrigerant inventory entered by user (kg)
  REAL(r64)   :: SumUADistPiping    =0.0d0  ! Sum of U*A for secondary fluid dist piping (W/C)
  REAL(r64)   :: SumUAReceiver      =0.0d0  ! Sum of U*A for secondary fluid receiver (W/C)
  REAL(r64)   :: TBrineAverage      =0.0d0  ! (C)
  REAL(r64)   :: TBrineInRated      =0.0d0  ! Entering brine temperature based upon rated range,approach,
                                          !    and evap Temp (C)
  REAL(r64)   :: TCondense          =0.0d0  ! Rated condensing temperature for heat exchanger serving
                                          !    secondary loop with phase change(C)
  REAL(r64)   :: TEvapDesign        =0.0d0  ! Rated evaporating temperature for heat exchanger serving
                                          !    secondary loop (C)
  REAL(r64)   :: TApproachDifRated  =0.0d0  ! Rated approach temperature diff for heat exchanger serving
                                          !    secondary loop (C)
  REAL(r64)   :: TRangeDifRated     =0.0d0  ! Rated range temperature diff for heat exchanger serving
                                          !    secondary loop (C)
  REAL(r64)   :: TMinNeeded         =0.0d0  ! Lowest Tbrine to case or walk-in needed on loop (C)
  REAL(r64)   :: TotalCoolingLoad   =0.0d0  ! Total load (cases + walk-ins + pump heat + distribution pipe heat gain)
                                          !     on this system (W)
  REAL(r64)   :: TotalCoolingEnergy =0.0d0  ! Total energy (cases + walk-ins + pump heat + distribution pipe heat gain)
                                          !    on this system (J)
  REAL(r64)   :: TotalRefrigLoad    =0.0d0  ! Total load (cases + walk-ins) on this system (W)
  REAL(r64)   :: TotalRefrigEnergy  =0.0d0  ! Total energy (cases + walk-ins) on this system (J)
  REAL(r64)   :: UnMetEnergy        =0.0d0  ! Load that is greater than capacity of loop heat exchanger, accumulates (J)
  REAL(r64)   :: UnMetEnergySaved   =0.0d0  ! Load that is greater than capacity of loop heat exchanger, accumulates (J)
END TYPE SecondaryLoopData

TYPE, PRIVATE ::  TransferLoadListDef                          ! Derived Type for Transfer Load (Secondary and Cascade) Lists
     CHARACTER(len=MaxNameLength)   :: Name            =' '    ! Name of this TransferLoad List
     INTEGER                        :: NumSecondarys   = 0     ! Number of Secondary Loops in this TransferLoad List
     INTEGER                        :: NumCascadeLoads = 0     ! Number of Cascade condenser loads in this TransferLoad List
     INTEGER, ALLOCATABLE, DIMENSION(:) :: CascadeLoadItemNum  ! List of Item numbers that correspond to the Cascade Condenser
     INTEGER, ALLOCATABLE, DIMENSION(:) :: SecondaryItemNum    ! List of Item numbers that correspond to the Secondary
END TYPE TransferLoadListDef

TYPE, PRIVATE ::  WalkInData
  CHARACTER(len=MaxNameLength) :: Name=' '       ! Name of walk in cooler
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ZoneName
                                                 ! Names of zones exchanging energy with cooler
  INTEGER          :: CircFanSchedPtr=0               ! Index to the correct availability schedule
  INTEGER          :: DefrostDripDownSchedPtr=0  ! Index to the correct fail-safe schedule
  INTEGER          :: DefrostSchedPtr=0          ! Index to the correct defrost schedule
  INTEGER          :: DefrostControlType=0       ! WalkIn defrost control type, Timed,Frost level
  INTEGER          :: DefrostType=0              ! WalkIn defrost type, Hot-gas,Electric, Hot-brine
  INTEGER          :: HeaterSchedPtr=0               ! Index to the correct availability schedule
  INTEGER          :: LightingSchedPtr=0         ! Index to the correct WalkIn lighting schedule
  INTEGER          :: NumSysAttach=0             ! Number of systems attached to WalkIn, error if /=1
  INTEGER          :: NumZones =0                ! Number of zones exchanging energy with WalkIn
  INTEGER          :: SchedPtr=0                 ! Index to the correct availability schedule
  INTEGER          :: StockingSchedPtr=0         ! Index to the correct product stocking schedule
  INTEGER, ALLOCATABLE, DIMENSION(:) :: GlassDoorOpenSchedPtr   ! Index to the door opening schedule
  INTEGER, ALLOCATABLE, DIMENSION(:) :: StockDoorOpenSchedPtr   ! Index to the door opening schedule
  INTEGER, ALLOCATABLE, DIMENSION(:) :: StockDoorProtectType    ! Index to door protection type
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneNodeNum             ! Index to Zone Node
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneNum                 ! Index to Zone
  REAL(r64)        :: CircFanConsumption ! Operating energy of  Walk In fan [J]
  REAL(r64)        :: CircFanPower=0.0d0   ! Operating power of  Walk In fan [W]
  REAL(r64)        :: CoilFanPower=0.0d0   ! Operating power of  Walk In evap coil fan [W]
  REAL(r64)        :: IceTemp= 0.0d0      ! Temperature of Ice Mass [C]
  REAL(r64)        :: IceTempSaved = 0.0d0 ! Temperature of Ice Mass [C]
  REAL(r64)        :: DefrostCapacity=0.0d0       ! Design defrost WalkIn capacity [W]
  REAL(r64)        :: DeltaFreezeKgFrost = 0.0d0  ! Used to reverse accumulation if the zone/load time step is repeated (kg)
  REAL(r64)        :: DefEnergyFraction=0.0d0     ! Portion of defrost energy available to melt ice,
                                                !    used with fluid defrost with temp termination (dimensionless)
  REAL(r64)        :: DesignFanPower=0.0d0        ! Design power of fans [W]
  REAL(r64)        :: DesignLighting=0.0d0        ! Design  lighting (includes task and display lights)[W]
  REAL(r64)        :: DesignRatedCap=0.0d0        ! Design total capacity [W]
  REAL(r64)        :: DesignRefrigInventory=0.0d0 ! Design refrigerant inventory [kg]
  REAL(r64)        :: FloorArea=0.0d0             ! Floor area of  Walk In [m2]
  REAL(r64)        :: FloorUvalue=0.0d0           ! U-value of Walk In floor [W/m2-C]
  REAL(r64)        :: HeaterPower=0.0d0           ! Rated power of  Walk In   heaters [W/m]
  REAL(r64)        :: HotDefrostCondCredit = 0.0d0 ! Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
  REAL(r64)        :: KgFrost = 0.0d0             ! Amount of frost on WalkIn evaporator (Kg)
  REAL(r64)        :: StoredEnergy = 0.0d0        ! Cumulative Stored Energy not met by evaporator [J]
  REAL(r64)        :: KgFrostSaved = 0.0d0        ! Amount of frost on WalkIn evaporator (Kg)
  REAL(r64)        :: StoredEnergySaved = 0.0d0   ! Cumulative Stored Energy not met by evaporator [J]
  REAL(r64)        :: Temperature=0.0d0           ! Rated temperature [C]
  REAL(r64)        :: TEvapDesign=0.0d0           ! Design evaporator temperature (or brine inlet T) [C]
  REAL(r64)        :: TotalFanPower=0.0d0         ! Sum of coil and circ fan power  [W]
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: AreaGlassDr
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: UValueGlassDr
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HeightGlassDr
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: AreaStockDr
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: UValueStockDr
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: HeightStockDr
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SurfaceArea
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: UValue

! Report Variables
  REAL(r64)        :: ElecHeaterPower=0.0d0         !  Walk In heater rate (W)
  REAL(r64)        :: ElecHeaterConsumption=0.0d0   !  Walk In heater energy (J)
  REAL(r64)        :: ElecFanPower=0.0d0            !  Walk In fan electric power (W)
  REAL(r64)        :: ElecFanConsumption=0.0d0      !  Walk In fan electric energy (J)
  REAL(r64)        :: ElecLightingPower=0.0d0       !  Walk In lighting electric power (W)
  REAL(r64)        :: ElecLightingConsumption=0.0d0 !  Walk In lighting electric energy (J)
  REAL(r64)        :: ElecDefrostPower=0.0d0        !  Walk In defrost rate (W)
  REAL(r64)        :: ElecDefrostConsumption=0.0d0  !  Walk In defrost energy (J)
  REAL(r64)        :: TotalCoolingLoad =0.0d0       !  Walk In total cooling rate (W)
  REAL(r64)        :: TotalCoolingEnergy=0.0d0     !  Walk In total cooling energy (J)
  REAL(r64)        :: TotalElecPower=0.0d0          !  Walk In total electric
                                                  !   (fans, heaters, lighting, and elec defrost) rate (W)
  REAL(r64)        :: TotalElecConsumption=0.0d0    !  Walk In total electric energy (J)
  REAL(r64)        :: TotLatCoolingEnergyRate=0.0d0   !  Walk In latent cooling rate (W)
  REAL(r64)        :: TotLatCoolingEnergy=0.0d0       !  Walk In latent cooling energy (J)
  REAL(r64)        :: TotSensCoolingEnergyRate=0.0d0  !  Walk In sensible cooling rate (W)
  REAL(r64)        :: TotSensCoolingEnergy=0.0d0      !  Walk In sensible cooling energy (J)

  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LatZoneCreditRate       !Amount of latent energy provided to zone(W)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LatZoneCredit           !Amount of latent energy provided to zone(J)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SensZoneCreditRate      !Amount of sensible heat gain to zone, pos and neg (W)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SensZoneCreditCoolRate  !Amount of sensible cooling provided to the zone (W)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SensZoneCreditCool      !Amount of sensible cooling provided to the zone (J)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SensZoneCreditHeatRate  !Amount of sensible heat provided to the zone (W)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SensZoneCreditHeat      !Amount of sensible heat provided to the zone (J)
END TYPE  WalkInData
TYPE CaseWIZoneReportData
  REAL(r64) :: LatCoolingToZoneRate   = 0.0d0 ! Positive for reporting Net latent credit to zone on sys time step from cases/walkins (W)
  REAL(r64) :: LatCoolingToZoneEnergy = 0.0d0
  REAL(r64) :: SenCoolingToZoneRate   = 0.0d0 ! Positive for reporting Net sensible cooling to zone on sys time step from cases/walkins (W)
  REAL(r64) :: SenCoolingToZoneEnergy = 0.0d0
  REAL(r64) :: HeatingToZoneRate   = 0.0d0 ! Positive for reporting Net sensible credit to zone on sys time step from cases/walkins (W)
  REAL(r64) :: HeatingToZoneEnergy = 0.0d0
  REAL(r64) :: TotCoolingToZoneRate   = 0.0d0 ! Positive for reporting Net total cooling credit to zone from cases/walkins (W)
  REAL(r64) :: TotCoolingToZoneEnergy = 0.0d0
  REAL(r64) :: TotHtXferToZoneRate   = 0.0d0 ! Gives negative for cooling, positive for heating net to zone from cases/walkins (W)
  REAL(r64) :: TotHtXferToZoneEnergy   = 0.0d0
  REAL(r64) :: SenCaseCreditToZoneEnergy   = 0.0d0 ! Negative (heat out zone) positive (heat into zone) (rate found in CaseCreditData) (J)
END TYPE CaseWIZoneReportData

TYPE, PRIVATE ::  WarehouseCoilData
  CHARACTER(len=MaxNameLength) :: Name=' '       ! Name of Warehouse Coil
  CHARACTER(len=MaxNameLength) :: ZoneName       ! Names of zone cooled by coil
  LOGICAL    :: SecStatusFirst = .FALSE.         ! Flag to show if this is the first coil on a particular secondary
  LOGICAL    :: SecStatusLast  = .FALSE.         ! Flag to show if this is the last coil on a particular secondary
  LOGICAL    :: SysStatusFirst = .FALSE.         ! Flag to show if this is the first coil on a particular primary
  LOGICAL    :: SysStatusLast  = .FALSE.         ! Flag to show if this is the last coil on a particular primary
  INTEGER          :: CoilFanSchedPtr=0          ! Index to the correct availability schedule
  INTEGER          :: DefrostDripDownSchedPtr=0  ! Index to the correct fail-safe schedule
  INTEGER          :: DefrostSchedPtr=0          ! Index to the correct defrost schedule
  INTEGER          :: DefrostControlType=0       ! Coil defrost control type, Timed,Frost level
  INTEGER          :: DefrostType=0              ! Coil defrost type, Hot-gas,Electric, Hot-brine
  INTEGER          :: FanType    =0              ! Index to coil fan type (fixed, two-speed, etc.)
  INTEGER          :: HeaterSchedPtr=0           ! Index to the correct availability schedule
  !INTEGER          :: NodeNumInlet=0            ! Node number for inlet to coil
  !INTEGER          :: NodeNumOutlet=0           ! Node number for outlet from coil
  INTEGER          :: NumSysAttach=0             ! Number of refrigerating systems cooling this coil (error check purpose)
  INTEGER          :: RatingType=0               ! Indicates which type of manufacturer's rating is used
  INTEGER          :: SchedPtr=0                 ! Index to the correct availability schedule
  INTEGER          :: SCIndex = 0                ! IDs which of European standard conditions is used for rating
  INTEGER          :: SecServeID=0               ! Index to the refrigeration system serving this coil
  INTEGER          :: SHRCorrectionType          ! Index to type of correction for sensible heat ratio
  INTEGER          :: SHRCorrectionCurvePtr      ! Index to Sensible heat ratio correction curve
  INTEGER          :: SysServeID=0               ! Index to the secondary system serving this coil
  INTEGER          :: VerticalLocation = 0       ! Index to coil location, floor, ceiling, or middle
  INTEGER          :: ZoneNodeNum=0              ! Index to the zone node for the zone served by this coil
  INTEGER          :: ZoneNum=0                  ! Index to the zone served by this coil
  REAL(r64)        :: CorrMaterial=0.0d0           ! Correction factor from manufacturer's rating for coil material, default 1.0
  REAL(r64)        :: CorrRefrigerant=0.0d0        ! Correction factor from manufacturer's rating for refrigerant, default 1.0
  REAL(r64)        :: DefrostCapacity=0.0d0        ! Design defrost Coil capacity [W]
  REAL(r64)        :: DefrostPower=0.0d0           ! Defrost power for electric defrost (W)
  REAL(r64)        :: DeltaFreezeKgFrost = 0.0d0   ! Used to reverse accumulation if the zone/load time step is repeated (kg)
  REAL(r64)        :: DefEnergyFraction=0.0d0      ! Portion of defrost energy available to melt ice,
                                                 !    used with fluid defrost with temp termination (dimensionless)
  REAL(r64)        :: DesignRefrigInventory=0.0d0  ! Design refrigerant inventory [kg]
  REAL(r64)        :: FanMinAirFlowRatio = 0.0d0   ! Minimum air flow ratio set to preserve fan motor, dimensionless
  REAL(r64)        :: HeaterPower=0.0d0            ! Rated power of  coil heaters [W/m]
  REAL(r64)        :: HotDefrostCondCredit = 0.0d0 ! Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
  REAL(r64)        :: IceTemp= 0.0d0               ! Temperature of Ice Mass [C]
  REAL(r64)        :: IceTempSaved = 0.0d0         ! Temperature of Ice Mass [C]
  REAL(r64)        :: KgFrost = 0.0d0              ! Amount of frost on coil evaporator (Kg)
  REAL(r64)        :: KgFrostSaved = 0.0d0         ! Amount of frost on coil evaporator (Kg)
  REAL(r64)        :: MaxTemperatureDif = 0.0d0    ! Maximum difference between Tevap and Tair inlet, limits capacity during initial pull-down (deltaC)
  REAL(r64)        :: RatedAirVolumeFlow = 0.0d0   ! Rated air flow through coil (m3/s)
  REAL(r64)        :: RatedCapTotal=0.0d0          ! Rated total heat capacity, both latent and sensible [W]
  REAL(r64)        :: RatedFanPower=0.0d0          ! Rated power of fans [W]
  REAL(r64)        :: RatedRH = 0.0d0              ! Rated RH corresponding to RatedCapacityTotal [decimal 0 to 1]
  REAL(r64)        :: RatedSensibleCap=0.0d0       ! Rated total capacity at sensible heat ratio of 1.0 [W]
  REAL(r64)        :: RatedTemperatureDif=0.0d0    ! Rated temperature difference DT1, T air in minus evaporating temperature [W]
  REAL(r64)        :: ReqLoad = 0.0d0              ! Load requested to meet zone load [W]
  REAL(r64)        :: SensHeatRatio = 0.0d0        ! Sensible heat ratio (sensible/total), dimensionless
  REAL(r64)        :: SHRCorrection60=0.0d0        ! Correction factor corresponding to sensible heat ratio of 0.6 [ dimensionless]
  REAL(r64)        :: Temperature=0.0d0            ! Rated temperature [C]
  REAL(r64)        :: TEvapDesign=0.0d0            ! Design evaporator temperature (or brine inlet T) [C]
  REAL(r64)        :: ThermalDefrostPower=0.0d0    ! Thermal defrost load used to communicate with derate routine even if not electric defrost [W]
  REAL(r64)        :: UnitLoadFactorSens=0.0d0     ! Rated sensible capacity [W/C]

! Report Variables
  REAL(r64)        :: ElecHeaterPower=0.0d0         ! Coil heater rate (W)
  REAL(r64)        :: ElecHeaterConsumption=0.0d0   ! Coil heater energy (J)
  REAL(r64)        :: ElecFanPower=0.0d0            ! Coil fan electric power (W)
  REAL(r64)        :: ElecFanConsumption=0.0d0      ! Coil fan electric energy (J)
  REAL(r64)        :: ElecDefrostPower=0.0d0        ! Coil defrost rate (W)
  REAL(r64)        :: ElecDefrostConsumption=0.0d0  ! Coil defrost energy (J)
  REAL(r64)        :: LatCreditRate= 0.0d0          ! Latent heat removed from the zone [W]
  REAL(r64)        :: LatLoadServed=0.0d0           ! Latent load met by coil (J)
  REAL(r64)        :: LatKgPerS_ToZone =0.0d0       ! Latent load met by coil (kg/s)
  REAL(r64)        :: LatCreditEnergy= 0.0d0        ! Latent heat removed from the zone [J]
  REAL(r64)        :: ReportSensCoolCreditRate=0.0d0   ! Coil cooling credit to zone (net) [W]
  REAL(r64)        :: ReportHeatingCreditRate=0.0d0    ! Coil heating credit to zone (net) [J]
  REAL(r64)        :: ReportSensCoolCreditEnergy=0.0d0 ! Coil cooling credit to zone (net) [W]
  REAL(r64)        :: ReportHeatingCreditEnergy=0.0d0  ! Coil heating credit to zone (net) [J]
  REAL(r64)        :: ReportTotalCoolCreditRate =0.0d0 ! Coil cooling sens + latent credit to zone[W]
  REAL(r64)        :: ReportTotalCoolCreditEnergy =0.0d0 ! Coil cooling sens + latent credit to zone[J]
  REAL(r64)        :: SensCreditRate = 0.0d0        ! Net Sensible heat removed from the zone [W]
  REAL(r64)        :: SensCreditEnergy = 0.0d0      ! Net Sensible heat removed from the zone [J]
  REAL(r64)        :: SensCoolingEnergyRate=0.0d0   ! Gross Coil sensible cooling rate (W)
  REAL(r64)        :: SensCoolingEnergy=0.0d0       ! Gross Coil sensible cooling energy (J)
  REAL(r64)        :: TotalCoolingLoad=0.0d0        ! Gross total cooling rate (W)
  REAL(r64)        :: TotalCoolingEnergy=0.0d0      ! Gross total cooling energy (J)
  REAL(r64)        :: TotalElecPower=0.0d0          ! Coil total electric
                                                  !   (fans, heaters, and elec defrost) rate (W)
  REAL(r64)        :: TotalElecConsumption=0.0d0    ! Coil total electric energy (J)
END TYPE  WarehouseCoilData


TYPE  AirChillerSetData
  CHARACTER(len=MaxNameLength)       :: Name=' '  ! Name of Chiller Set
  !CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: CoilName   ! Name of Individual Chiller in set
  CHARACTER(len=MaxNameLength)       :: ZoneName  ! Name of zone where chiller set is located
  INTEGER, ALLOCATABLE, DIMENSION(:) :: CoilNum   ! ID number of Individual Chiller in set
  INTEGER   :: ChillerSetID = 0           ! ID number for this set of chillers (all serving one zone,
                                          !                       but can be chilled by mult systems)
  INTEGER   :: SchedPtr = 0               ! Schedule to take whole set off-line if needed
  INTEGER   :: NodeNumInlet = 0           ! Node ID Number of inlet for chiller set as a whole, not identified for specific coils
  INTEGER   :: NodeNumOutlet = 0          ! Node ID Number of outlet for chiller set as a whole, not identified for specific coils
  INTEGER   :: NumCoils = 0               ! Number of individual chillers in set
  INTEGER   :: ZoneNum = 0                ! ID number of zone where chiller set is located
  INTEGER   :: ZoneNodeNum = 0            ! ID number of zone node giving mixed conditions of zone where chiller set is located
  REAL(r64) :: QZnReqSens = 0.0d0           ! Sensible heat needed by the zone to reach setpoint [W]
END TYPE  AirChillerSetData

TYPE CoilCreditData  !used to sum impact of all coils within a zone
  REAL(r64) :: LatCreditToZoneRate   = 0.0d0 ! Net latent credit to zone on sys time step from coil (W)
  REAL(r64) :: LatCreditToZoneEnergy = 0.0d0
  REAL(r64) :: LatKgPerS_ToZoneRate  = 0.0d0 ! Latent water to zone on sys time step from coils, neg when water removed (kg/s)
  REAL(r64) :: SenCreditToZoneRate   = 0.0d0 ! Net sensible credit to zone on sys time step from coil (W)
  REAL(r64) :: SenCreditToZoneEnergy = 0.0d0
  REAL(r64) :: ReportH20RemovedKgPerS_FromZoneRate  = 0.0d0 ! same but positive for reporting purposes (kg/s)
  REAL(r64) :: ReportLatCreditToZoneRate   = 0.0d0 ! Positive for reporting Net latent credit to zone on sys time step from coil (W)
  REAL(r64) :: ReportLatCreditToZoneEnergy = 0.0d0
  REAL(r64) :: ReportHeatingToZoneRate   = 0.0d0 ! Positive for reporting Net sensible credit to zone on sys time step from coil (W)
  REAL(r64) :: ReportHeatingToZoneEnergy = 0.0d0
  REAL(r64) :: ReportSenCoolingToZoneRate   = 0.0d0 ! Positive for reporting Net sensible credit to zone on sys time step from coil (W)
  REAL(r64) :: ReportSenCoolingToZoneEnergy = 0.0d0
  REAL(r64) :: ReportTotCoolingToZoneRate   = 0.0d0 ! Positive for reporting Net total cooling credit to zone from chillers (W)
  REAL(r64) :: ReportTotCoolingToZoneEnergy = 0.0d0
END TYPE CoilCreditData

          ! MODULE VARIABLE DECLARATIONS:

TYPE (RefrigCaseData) ,        PRIVATE, ALLOCATABLE, DIMENSION(:) :: RefrigCase
TYPE (RefrigRackData) ,        PRIVATE, ALLOCATABLE, DIMENSION(:) :: RefrigRack
TYPE (CaseRAFractionData) ,    PRIVATE, ALLOCATABLE, DIMENSION(:) :: CaseRAFraction
TYPE (RefrigSystemData) ,      PRIVATE, ALLOCATABLE, DIMENSION(:) :: System
TYPE (TransRefrigSystemData) , PRIVATE, ALLOCATABLE, DIMENSION(:) :: TransSystem
TYPE (RefrigCondenserData) ,   PRIVATE, ALLOCATABLE, DIMENSION(:) :: Condenser
TYPE (RefrigCompressorData) ,  PRIVATE, ALLOCATABLE, DIMENSION(:) :: Compressor
TYPE (RefrigGasCoolerData) ,   PRIVATE, ALLOCATABLE, DIMENSION(:) :: GasCooler
TYPE (SubcoolerData) ,         PRIVATE, ALLOCATABLE, DIMENSION(:) :: Subcooler
TYPE (CaseandWalkInListDef),   PRIVATE,ALLOCATABLE, DIMENSION(:)  :: CaseandWalkInList
TYPE (CompressorLISTDef),      PRIVATE,ALLOCATABLE, DIMENSION(:)  :: CompressorLists
TYPE (SecondaryLoopData) ,     PRIVATE, ALLOCATABLE, DIMENSION(:) :: Secondary
TYPE (TransferLoadListDef) ,   PRIVATE, ALLOCATABLE, DIMENSION(:) :: TransferLoadList
TYPE (WalkInData) ,            PRIVATE, ALLOCATABLE, DIMENSION(:) :: WalkIn
TYPE (WarehouseCoilData) ,     PRIVATE, ALLOCATABLE, DIMENSION(:) :: WarehouseCoil
TYPE (AirChillerSetData) ,     PRIVATE, ALLOCATABLE, DIMENSION(:) :: AirChillerSet
TYPE (CoilCreditData) ,        PRIVATE, ALLOCATABLE, DIMENSION(:) :: CoilSysCredit
TYPE (CaseWIZoneReportData) ,  PRIVATE, ALLOCATABLE, DIMENSION(:) :: CaseWIZoneReport

INTEGER, PRIVATE :: NumSimulationCondAir        = 0       ! Number of air-cooled condensers in simulation
INTEGER, PRIVATE :: NumSimulationCondEvap       = 0       ! Number of evaporative condensers in simulation
INTEGER, PRIVATE :: NumSimulationCondWater      = 0       ! Number of water-cooled condensers in simulation
INTEGER, PRIVATE :: NumSimulationCascadeCondensers   = 0    ! Total number of Cascade condensers in IDF
INTEGER, PRIVATE :: NumSimulationGasCooler      = 0       ! Number of gas coolers in simulation
INTEGER, PRIVATE :: NumSimulationSharedGasCoolers = 0     ! Total number of gas coolers that serve more than one system
INTEGER, PRIVATE :: NumTransRefrigSystems       = 0       ! Total number of transcritical CO2 refrigeration systems
INTEGER, PRIVATE :: NumSimulationSharedCondensers    = 0  ! Total number of condensers that serve more than one system
INTEGER, PRIVATE :: NumSimulationCases          = 0       ! Number of refrigerated cases in simulation
INTEGER, PRIVATE :: NumSimulationCaseAndWalkInLists  = 0    ! Total number of CaseAndWalkIn Lists in IDF
INTEGER, PRIVATE :: NumSimulationWalkIns             = 0    ! Number of walk in coolers in simulation
INTEGER, PRIVATE :: NumSimulationCompressors   = 0        ! Number of refrigeration compressors in simulation
INTEGER, PRIVATE :: NumSimulationSubcoolers    = 0        ! Number of refrigeration subcoolers in simulation
INTEGER, PRIVATE :: NumSimulationMechSubcoolers    = 0    ! Number of mechanical subcoolers in simulation
INTEGER, PRIVATE :: NumSimulationRefrigAirChillers  = 0   ! Number of individual Air Chillers/coils in simulation
INTEGER, PRIVATE :: NumSimulationSecondarySystems    = 0    ! Number of Secondary loops in simulation
INTEGER, PRIVATE :: NumSimulationTransferLoadLists    = 0    ! Number of Secondary Lists in simulation
INTEGER, PRIVATE :: NumunusedRefrigCases = 0    ! Number of refrigerated cases not connected to a rack or system
INTEGER, PRIVATE :: NumUnUsedCoils       = 0    ! Number of refrigeration air coils not connected to a rack or system
INTEGER, PRIVATE :: NumunusedCondensers  = 0    ! Number of refrigeration condensors not connected to a system
INTEGER, PRIVATE :: NumunusedGasCoolers  = 0    ! Number of refrigeration gas coolers not connected to a system
INTEGER, PRIVATE :: NumunusedCompressors = 0    ! Number of refrigeration compressors not connected to a system
INTEGER, PRIVATE :: NumunusedSecondarys  = 0    ! Number of refrigeration secondarys not connected to a system
INTEGER, PRIVATE :: NumunusedWalkIns     = 0    ! Number of refrigeration compressors not connected to a system
LOGICAL          :: MyReferPlantScanFlag = .TRUE.

! Refrigerated case variables
REAL(r64)      :: CaseRAFactor        =0.0d0   ! Factor determining case credit allocation (e.g. % to zone or HVAC)
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowStockingWarning ! Used for one-time warning message for possible case
                                                         ! input error regarding stocking
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowFrostWarning    ! Used for one-time warning message for possible case
                                                         ! input error regarding frost
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowStoreEnergyWarning ! Used for one-time warning message for possible case
                                                         ! input error regarding defrost or stocking
!  Walk In variables
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowUnMetWIEnergyWarning ! Used for one-time warning message
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowWIFrostWarning ! Used for one-time warning message

! Refrigeration compressor rack variables
REAL(r64)      :: TotalRackDeliveredCapacity =0.0d0 ! Total capacity of all refrigerated cases attached to rack (W)
REAL(r64)      :: TotalCompressorPower       =0.0d0 ! Total compressor electric power (W)
REAL(r64)      :: CompressorCOPactual        =0.0d0 ! Compressor coefficient of performance at specific operating conditions (W/W)
REAL(r64)      :: RackSenCreditToZone        =0.0d0 ! Amount of condenser heat applied to zone load (W)
REAL(r64)      :: RackSenCreditToHVAC        =0.0d0 ! Amount of condenser heat applied to HVAC RA duct (W)
INTEGER        :: InletNode                  =0   ! Water-cooled condenser inlet node number
INTEGER        :: OutletNode                 =0   ! Water-cooled condenser outlet node number
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowCOPWarning      ! Used for one-time warning message for possible rack
                                                         ! input error regarding COP
! Refrigeration condenser variables (used for both racks and detailed systems)
REAL(r64)      :: TotalCondenserFanPower     =0.0d0 ! Total condenser fan electric power (W)
REAL(r64)      :: TotalCondenserPumpPower    =0.0d0 ! Total condenser pump electric power (W)
REAL(r64)      :: TotalCondenserHeat         =0.0d0 ! Total condenser heat from compressor rack (W)
REAL(r64)      :: TotalBasinHeatPower        =0.0d0 ! Total condenser basin water heater power (W)
REAL(r64)      :: TotalEvapWaterUseRate      =0.0d0 ! Total condenser water use rate (m3/s)

! Refrigeration system variables
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowUnmetEnergyWarning ! Used for one-time warning message for possible
                                          !compressor input error regarding total system compressor capacity
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowHiStageUnmetEnergyWarning ! Used for one-time warning message for possible
                                          !high-stage compressor input error regarding high-stage compressor capacity

! Transcritical refrigeration system variables
LOGICAL, PRIVATE :: TransCritSysFlag = .FALSE.  !  Used to indicate whether or not a transcritical refrigeration system has been defined.
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowUnmetEnergyWarningTrans ! Used for one-time warning message for possible
                                          !compressor input error regarding total system compressor capacity

! Refrigeration Secondary Loop variables
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowUnmetSecondEnergyWarning ! Used for one-time warning message for possible
                                          !compressor input error regarding secondary loop heat exchanger capacity
!Refrigerated warehouse coil variables
LOGICAL, ALLOCATABLE,DIMENSION(:) :: CheckChillerName !used when simrefrigcoil called for a zone
!LOGICAL, ALLOCATABLE,DIMENSION(:) :: CheckZoneNum  !used when simrefrigcoil called for a zone
LOGICAL, ALLOCATABLE,DIMENSION(:) :: ShowCoilFrostWarning ! Used for one-time warning message if defrost cycles insufficient to melt ice

! Refrigeration Plant connections checks
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipNameRackWaterCondenser
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipNameWaterCondenser

!Control variables
  LOGICAL, ALLOCATABLE,DIMENSION(:) :: RefrigPresentInZone ! Used when translating rate to energy for reporting
                                                         !  total refrigeration impact on a zone
  LOGICAL, ALLOCATABLE, DIMENSION(:)  :: CheckChillerSetName    ! used when sim chiller set called form zone equip manager

  LOGICAL,SAVE :: GetRefrigerationInputFlag = .TRUE.  ! Flag to show case input should be read
  LOGICAL,SAVE :: HaveRefrigRacks = .TRUE.       ! Is initialized as TRUE and remains true when
                                                 ! refrigerated racks exist in the input deck
  LOGICAL,SAVE :: HaveDetailedRefrig = .TRUE.    ! Is initialized as TRUE and remains true when
                                                 ! detailed refrigeration systems exist in the input deck
  LOGICAL,SAVE :: HaveDetailedTransRefrig = .TRUE.  ! Is initialized as TRUE and remains true when
                                                    ! detailed transcritical CO2 refrigeration systems exist in the input deck
  LOGICAL,SAVE :: ManageRefrigeration = .TRUE.   ! Is initialized as TRUE and remains true when
                                                 ! refrigerated racks or detailed systems exist in the input deck
  LOGICAL,SAVE :: UseSysTimeStep = .FALSE.       ! Flag is true IF working on a system that includes a coil cooling a controlled zone on the system time step,
                                                 ! All other refrigeration calculations for case and walkin systems done on the load time step
  LOGICAL,SAVE :: HaveCasesOrWalkins = .TRUE.    ! Is initialized as TRUE and remains true when
                                                 ! refrigerated cases or walkins exist in the input deck
  LOGICAL,SAVE :: HaveChillers = .TRUE.          ! Is initialized as TRUE and remains true when
                                                 ! chillers exist in the input deck
! SUBROUTINE SPECIFICATIONS FOR MODULE RefrigeratedCase:

PUBLIC  CheckRefrigerationInput
PUBLIC  GetRefrigeratedRackIndex
PUBLIC  ManageRefrigeratedCaseRacks
PUBLIC  SimRefrigCondenser
PUBLIC  SimAirChillerSet
PRIVATE GetRefrigerationInput
PRIVATE InitRefrigeration
PRIVATE InitRefrigerationPlantConnections
PRIVATE UpdateRefrigCondenser
PRIVATE SimulateDetailedRefrigerationSystems
PRIVATE SimulateDetailedTransRefrigSystems
PRIVATE CalcDetailedSystem
PRIVATE CalcDetailedTransSystem
PRIVATE CalcRackSystem
PRIVATE CalculateCase
PRIVATE CalculateWalkIn
PRIVATE CalculateAirChillerSets
PRIVATE FinalRateCoils
PRIVATE CalculateCoil
PRIVATE CalculateCompressors
PRIVATE CalculateTransCompressors
PRIVATE CalculateCondensers
PRIVATE CalcGasCooler
PRIVATE CalculateSubcoolers
PRIVATE CalculateSecondary
PRIVATE SetupReportInput
PRIVATE ReportRackSystem
PRIVATE ReportRefrigerationComponents
PRIVATE SumZoneImpacts
PUBLIC  FigureRefrigerationZoneGains
PRIVATE ZeroHVACValues

CONTAINS

SUBROUTINE ManageRefrigeratedCaseRacks

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Oct/Nov 2004
          !       MODIFIED       Shirey, FSEC Dec 2004, Stovall, ORNL, May 2008
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called from HVACManager.f90, subroutine ManageHVAC to
          ! manage refrigerated cases and associated compressor racks on zone time step
          ! OR from SimAirChillerSet in this module on sys time step (Stovall 2011)

          ! METHODOLOGY EMPLOYED:
          ! Each compressor rack is modeled by first simulating the attached refrigeration loads. The
          ! loads can include refrigerated cases, walk-in coolers, and secondary fluid chillers.  The sum
          ! of the total heat transfer for all attached loads determines the load on the compressor rack.
          ! For the refrigeration rack approach, a simple model for variation of COP with
          ! condensing temperature is used to determine rack power and energy consumption.
          ! For the detailed system approach, the compressors and condensers are modeled individually
          ! using manufacturer's data and rated performance curves.
          !
          ! Inter-system heat transfer via subcoolers and cascade condensers can be accomodated.
          ! Secondary refrigeration cycles are also available.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER       :: RackNum                       ! Index to the refrigerated compressor rack being modeled
  LOGICAL,SAVE  :: MyOneTimeFlag = .TRUE.     ! flag to skip first pass on next begin environment flag


  IF(.NOT. ManageRefrigeration)RETURN

  CALL CheckRefrigerationInput

  CALL InitRefrigeration()

  !ManageRefrigeratedCaseRacks is called on each zone time step.
  !  However, if have chillers, ManageRefrigeration will be .TRUE. and will
  !  need to bounce back. (InitRefrig has to be called anyway to zero values at zone time step.)
  !  Therefore...
  IF((.NOT. HaveCasesOrWalkins) .AND. (.NOT. UseSysTimeStep))THEN
     !Zero requests for cooling water from plant or tank
     CALL ZeroHVACValues
     RETURN
  END IF
  !Following case should never occur, but just for completeness:
  IF((.NOT. HaveChillers) .AND. (UseSysTimeStep)) RETURN


  ! Refrigerated cases are not simulated the first time through, replicate this on beginning of next environment
  IF(BeginEnvrnFlag .AND. MyOnetimeFlag)THEN
    MyOneTimeFlag = .FALSE.
    RETURN
  END IF
  IF(.NOT. BeginEnvrnFlag)MyOneTimeFlag = .TRUE.

  IF (HaveRefrigRacks) THEN
    DO RackNum = 1, NumRefrigeratedRacks
      CALL CalcRackSystem(RackNum)
      CALL ReportRackSystem(RackNum)
    END DO
  END IF

  IF (HaveDetailedRefrig) CALL SimulateDetailedRefrigerationSystems
  IF (HaveDetailedTransRefrig) CALL SimulateDetailedTransRefrigSystems

  RETURN

END SUBROUTINE ManageRefrigeratedCaseRacks

!***************************************************************************************************

SUBROUTINE GetRefrigerationInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Oct/Nov 2004
          !       MODIFIED       Shirey, FSEC Dec 2004; Hudson, ORNL Feb 2007, July 2007
          !       MODIFIED       Stovall, ORNL April 2008, Assisted by Hugh Henderson
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! GetObjectItem is called to read refrigerated case, rack, compressor, and condenser information
          ! Lists of cases and compressors are then correlated to the appropriate system.
          ! The nominal ratings of all components are then compared and a warning is printed if the system is not balanced


          ! METHODOLOGY EMPLOYED:
          ! GetObjectItem is called to read refrigerated case information

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE BranchNodeConnections, ONLY: TestCompSet
  USE CurveManager,      ONLY : GetCurveIndex, GetCurveType, GetCurveMinMaxValues, CurveValue
  USE DataHeatBalance,   ONLY: Zone, NumRefrigeratedRacks,NumRefrigSystems !, &
!unused                               IntGainTypeOf_RefrigerationCompressorRack, &
!unused                               IntGainTypeOf_RefrigerationCase
  USE DataZoneEquipment, ONLY: GetSystemNodeNumberForZone, GetReturnAirNodeForZone
  USE DataEnvironment,   ONLY: StdBaroPress
  USE General,           ONLY: RoundSigDigits
  USE FluidProperties,   ONLY: GetSupHeatEnthalpyRefrig
  USE PlantUtilities,    ONLY: RegisterPlantCompDesignFlow
  USE InputProcessor,    ONLY: GetNumObjectsFound, GetObjectItem, GetObjectItemNum, VerifyName,  &
                               FindItemInList, SameString, GetObjectDefMaxArgs
  USE NodeInputManager,  ONLY: GetOnlySingleNode
  USE OutAirNodeManager, ONLY: CheckOutAirNodeNumber
  USE Psychrometrics,    ONLY: PsyWFnTdbRhPb, PsyTdpFnWPb
 ! USE ScheduleManager,   ONLY: CheckScheduleValueMinMax
  USE WaterManager,      ONLY: SetupTankDemandComponent
  USE DataGlobals,       ONLY: AnyEnergyManagementSystemInModel

  !USE FluidProperties,   ONLY: GetDensityGlycol, GetSpecificHeatGlycol


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
 CHARACTER(len=*),Parameter   :: TrackMessage = 'from refrigerated case'
 CHARACTER(len=*),Parameter   :: RoutineName = 'GetRefrigerationInput: '
 INTEGER, Parameter   ::  AlwaysOn = -1        ! -1 pointer sent to schedule manager returns a value of 1.0
!unused INTEGER, Parameter   ::  InputTypeSecond = 1  ! Indicator that flow used to specify capacity of secondary heat exchanger
!unused INTEGER, Parameter   ::  InputTypeFirst = 2   ! Indicator that capacity of secondary heat exchanger is input directly
!unused INTEGER, Parameter   ::  InputTypeBoth = 3    ! Indicator that capacity of secondary heat exchanger is
                                               !     input in both watts and flow rate
 INTEGER, Parameter   ::  NumWIAlphaFieldsBeforeZoneInput = 9 ! Used to cycle through zones on input for walk in coolers
 INTEGER, Parameter   ::  NumWIAlphaFieldsPerZone     = 4  !  Used to cycle through zones on input for walk in coolers
 INTEGER, Parameter   ::  NumWINumberFieldsBeforeZoneInput = 12 ! Used to cycle through zones on input for walk in coolers
 INTEGER, Parameter   ::  NumWINumberFieldsPerZone    = 8  ! Used to cycle through zones on input for walk in coolers
 REAL(r64), PARAMETER ::  CondARI460DelT     = 16.7d0      ! Rated sat cond temp - dry bulb air T for air-cooled Condensers, ARI460
 REAL(r64), PARAMETER ::  CondARI460Tcond    = 51.7d0      ! Rated sat cond temp for air-cooled cond, ARI 460
 REAL(r64), PARAMETER ::  CondARI490DelT     = 15.0d0      ! Rated sat cond temp - wet bulb air T for evap-cooled Cond w R22, ARI490
 REAL(r64), PARAMETER ::  CondARI490Tcond    = 40.6d0      ! Rated sat cond temp for evap-cooled cond with R22, ARI 490
 REAL(r64), PARAMETER ::  DelEvapTDefault    = 5.0d0       ! default difference between case T and evap T (C)
 REAL(r64), PARAMETER ::  HoursPerDay        = 24.d0
 REAL(r64), PARAMETER ::  SecondsPerHour     = 3600.d0
 REAL(r64), PARAMETER ::  DefaultCascadeCondApproach =3.0d0   !Cascade condenser approach temperature difference (deltaC)
 REAL(r64), PARAMETER ::  DefaultCircRate    = 2.5d0          !Phase change liquid overfeed circulating rate (ASHRAE definition)
!unused REAL(r64), PARAMETER ::  DefaultVarSpdCoeffA    =  0.9d0     !Variable speed pump power curve coefficients based
!unused REAL(r64), PARAMETER ::  DefaultVarSpdCoeffB    = -0.1d0     !      upon paper by John Bittner of Hill Phoenix
!unused REAL(r64), PARAMETER ::  DefaultVarSpdCoeffC    =  0.2d0     !      A is cube term, B square term, C linear term
 REAL(r64), Parameter ::  DefaultWISurfaceUValue = 0.3154d0   !equiv R18 in Archaic American units (W/m2-delta T)
 REAL(r64), Parameter ::  DefaultWIUValueGlassDr = 1.136d0    !equiv R5 in Archaic American units (W/m2-delta T)
 REAL(r64), Parameter ::  DefaultWIUValueStockDr = 0.3785d0   ! equiv R15 in Archaic American units (W/m2-delta T)
 REAL(r64), Parameter ::  DefaultWIHeightGlassDr = 1.5d0      ! glass door height in walk-in cooler (m)
 REAL(r64), Parameter ::  DefaultWIHeightStockDr = 3.0d0      ! stock door height in walk-in cooler (m)
 REAL(r64), Parameter ::  PumpImpellerEfficiency = 0.78d0     ! same as used in pump auto-sizing, dimensionless
 REAL(r64), Parameter ::  PumpMotorEfficiency    = 0.85d0     ! suggested as average value in ITT/Gould pump references,
                                                              !     dimensionless
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas           ! Alpha items for object
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames   ! Alpha field names (from input processor)
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames ! Numeric field names (from input processor)
  CHARACTER(len=MaxNameLength)    :: CurrentModuleObject    = ' '! Object type for getting and error messages

  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks             ! Logic array, alpha input blank = .TRUE.
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks           ! Logic array, numeric input blank = .TRUE.
  LOGICAL   :: CaseLoads    = .FALSE. ! Flag to help verify load type with loads served by systems cooled by cascade condensers
  LOGICAL   :: ErrorsFound  = .FALSE. ! Set to true if errors in input, fatal at end of routine
  LOGICAL   :: IsNotOK      = .FALSE. ! Flag to verify name
  LOGICAL   :: IsBlank      = .FALSE. ! Flag for blank name
  LOGICAL   :: StartCycle   = .FALSE. ! Flag for counting defrost cycles

  INTEGER    :: AlphaListNum      = 0       ! Index of Names in Case, Compressor et al Lists
  INTEGER    :: AlphaNum          = 0       ! Used to cycle through input
  INTEGER    :: AlphaStartList           = 0       !
  INTEGER    :: AStart            = 0       ! Used to cycle through zones on input for walk in coolers
  !INTEGER    :: CascadeCondenserID= 0       ! Used to match load on system to Condenser absolute index
  INTEGER    :: CascadeLoadNum    = 0       ! counters while associating cascade loads with systems
  INTEGER    :: CascadeLoadIndex  = 0       ! Counters while inputting cascade loads
  INTEGER    :: CaseID            = 0       ! ID of refrigerated case in rack
  INTEGER    :: CaseIndex         = 0       ! Index of refrigerated case attached to a system
  INTEGER    :: CaseNum           = 0       ! Index of refrigerated case
  INTEGER    :: CaseAndWalkInListNum       = 0       ! ID of refrigerated CaseAndWalkInList
  INTEGER    :: ChillerIndex           = 0       !
  INTEGER    :: CoilID         = 0       ! Index of warehouse coil attached to a system
  INTEGER    :: CoilIndex         = 0       ! Index of warehouse coil attached to a system
  INTEGER    :: CoilNum           = 0       ! Index of warehouse coil
  INTEGER    :: CompIndex         = 0       ! Index  of refrigeration compressor attached to a system
  INTEGER    :: CompNum           = 0       ! Index of refrigeration compressor
  INTEGER    :: CondID            = 0       ! Condenser ID used when associating condenser as a cascade load
  INTEGER    :: CondIndex         = 0       ! Index  of refrigeration condenser attached to a system
  INTEGER    :: CondNum           = 0       ! Index of refrigeration condenser
  INTEGER    :: DefType           = 0       ! Local value for case defrost type
  !INTEGER    :: FlowIndex         = 0       ! Index of pump flow numeric field
  INTEGER    :: GCNum             = 0       ! Index of refrigeration gas cooler
  INTEGER    :: HRNum             = 0       ! Counter for hours in day
  INTEGER    :: IOStatus          = 0       ! Used in GetObjectItem
  INTEGER    :: ListNum           = 0       ! Index of Lists of cases, compressors, and subcoolers
  INTEGER    :: LoadCascadeNum    = 0       ! Used to read transfer load list
  INTEGER    :: LoadCount         = 0       ! check for blank case and walkin names in caseand alkinlist
  INTEGER    :: LoadSecondaryNum = 0        ! Used to read transfer load list
  INTEGER    :: LoadWalkInNum     = 0       ! Used to read CaseAndWalkInList
  INTEGER    :: LoadCaseNum       = 0       ! Used to read CaseAndWalkInList
  INTEGER    :: LoadCoilNum       = 0       ! Used to read CaseAndWalkInList
  INTEGER    :: MaxNumAlphasRack  = 0       ! Maximum number of alphas for rack object
  INTEGER    :: MaxNumAlphasAirChiller= 0   ! Maximum number of alphas for air chiller
  INTEGER    :: MaxNumAlphasAll   = 0       ! Maximum number of alphas for all objects
  INTEGER    :: MaxNumAlphasSys   = 0       ! Maximum number of alphas for system object
  INTEGER    :: MaxNumAlphasTransSys = 0    ! Maximum number of alphas for transcritical system object
  INTEGER    :: MaxNumAlphasChillerSet= 0   ! Maximum number of alphas for chiller set
  INTEGER    :: MaxNumAlphasConda = 0       ! Maximum number of alphas for air-cooled condenser object
  INTEGER    :: MaxNumAlphasConde = 0       ! Maximum number of alphas for evap-cooled condenser object
  INTEGER    :: MaxNumAlphasCondw = 0       ! Maximum number of alphas for water-cooled condenser object
  INTEGER    :: MaxNumAlphasGasCoolera = 0  ! Maximum number of alphas for air-cooled gas cooler object
  INTEGER    :: MaxNumAlphasComp  = 0       ! Maximum number of alphas for compressor object
  INTEGER    :: MaxNumAlphasCompressorList  = 0 ! Maximum number of alphas for compressor list objects
  INTEGER    :: MaxNumAlphasCase  = 0       ! Maximum number of alphas for case object
  INTEGER    :: MaxNumAlphasCaseAndWalkInList= 0     ! Maximum number of alphas in CaseAndWalkInList
  INTEGER    :: MaxNumAlphasWalkIn  = 0     ! Maximum number of alphas for walkin object
  !INTEGER    :: MaxNumAlphasWalkInList  = 0 ! Maximum number of alphas for walkin list object
  INTEGER    :: MaxNumAlphasSecond  = 0     ! Maximum number of alphas for air chiller object
  INTEGER    :: MaxNumNumbersAirChiller= 0  ! Maximum number of numbers for air chiller object
  INTEGER    :: MaxNumNumbersSecond = 0     ! Maximum number of numbers for secondary system object
  INTEGER    :: MaxNumNumbersWalkIn = 0     ! Maximum number of numbers for walkin object
  INTEGER    :: MaxNumNumbersCase = 0       ! Maximum number of numbers for case object
  INTEGER    :: MaxNumNumbersCaseAndWalkInList= 0    ! Maximum number of numbers in CaseAndWalkInList
  INTEGER    :: MaxNumNumbersRack = 0       ! Maximum number of numbers for rack object
  INTEGER    :: MaxNumNumbersAll  = 0       ! Maximum number of numeric inputs for all objects
  INTEGER    :: MaxNumNumbersSys  = 0       ! Maximum number of numbers for system object
  INTEGER    :: MaxNumNumbersTransSys = 0   ! Maximum number of numbers for transcritical system object
  INTEGER    :: MaxNumNumbersChillerSet= 0     ! Maximum number of numbers for chiller set object
  INTEGER    :: MaxNumNumbersConda = 0      ! Maximum number of numbers for air-cooled condenser object
  INTEGER    :: MaxNumNumbersConde = 0      ! Maximum number of numbers for evap-cooled condenser object
  INTEGER    :: MaxNumNumbersCondw = 0      ! Maximum number of numbers for water-cooled condenser object
  INTEGER    :: MaxNumNumbersGasCoolera = 0 ! Maximum number of numbers for air-cooled gas cooler object
  INTEGER    :: MaxNumNumbersComp  = 0      ! Maximum number of numbers for compressor object
  INTEGER    :: MaxNumNumbersCompressorList = 0 ! Maximum number of numbers
  INTEGER    :: MaxNumArgs        = 0       ! Max number of alphas and numbers (arguments) for rack object
  INTEGER    :: NStart            = 0       ! Used to cycle through zones on input for walk in coolers
  INTEGER    :: NumAlphas         = 0       ! Number of Alphas for each GetObjectItem call
  INTEGER    :: NumCascadeLoad   = 0        ! Number of Cascade Loads on current system
  INTEGER    :: NumCompressorsSys = 0       ! Number of compressors on current system
  INTEGER    :: NumHiStageCompressorsSys = 0  ! Number of high-stage compressors on current system
  INTEGER    :: NumCondensers     = 0       ! Counter for condensers in GETInput do loop
  INTEGER    :: NumGasCoolers     = 0       ! Counter for gas coolers in GetInput do loop
  INTEGER    :: NumDefCycles      = 0       ! Number of defrost cycles per day
  INTEGER    :: NumPumps          = 0       ! Number of pumps on a secondary loop
  INTEGER    :: NumCases          = 0       ! Number of refrigerated cases for single system
  INTEGER    :: NumCasesMT        = 0       ! Number of medium temperature cases on a single transcritical system
  INTEGER    :: NumCasesLT        = 0       ! Number of low temperature cases on a single transcritical system
  INTEGER    :: NumCoils          = 0       ! Number of warehouse coils for single system
  INTEGER    :: NumSubcooler      = 0       ! Number of subcoolers on current system
  INTEGER    :: NumNameMatches    = 0       ! Used to check for uniqueness of name for transfer loads
  INTEGER    :: NumNum            = 0       ! Used to cycle through input
  INTEGER    :: NumNumbers        = 0       ! Number of Numbers for each GetObjectItem call
  INTEGER    :: NumCompressorLists=0        ! Total number of Compressor Lists in IDF
  INTEGER    :: NumDisplayCases   = 0       ! Counter for refrigerated cases in GetInput do loop
  INTEGER    :: NumSecondary      = 0       ! Number of secondary loops
  INTEGER    :: NumWalkIns        = 0       ! Number of walk ins
  INTEGER    :: NumWalkInsMT      = 0       ! Number of medium temperature walk-ins on a single transcritical system
  INTEGER    :: NumWalkInsLT      = 0       ! Number of low temperature walk-ins on a single transcritical system
  INTEGER    :: NumWIFieldsPerZone = 0      ! Used to calculate number of zones exposed to each walkin
  INTEGER    :: NumWIFieldsTotal  = 0       ! Used to calculate number of zones exposed to each walkin
  INTEGER    :: NumZones          = 0       ! Used to cycle through zones on input for walk in coolers
  INTEGER    :: NumTotalLoadsOnList= 0      ! Used to read transfer load and caseandWalkIn lists
  INTEGER    :: NumSecondarysOnList   = 0   ! Used to read transfer load lists
  INTEGER    :: NumCascadeLoadsChecked= 0   ! Used when checking for consistency of coil loads/time steps
  INTEGER    :: NumCascadeLoadsOnList = 0   ! Used to read transfer load lists
  INTEGER    :: NumLoad = 0                 ! Used to read transfer loadand caseandWalkIn lists
  INTEGER    :: NumCasesOnList    = 0       ! Used to read caseandWalkIn lists
  INTEGER    :: NumChillersInSet           = 0       !
  INTEGER    :: NumCoilsOnList    = 0       ! Used to read caseandWalkIn lists
  INTEGER    :: NumWalkInsOnList  = 0       ! Used to read caseandWalkIn lists
  INTEGER    :: RackNum           = 0       ! Index of refrigerated display case compressor rack
  INTEGER    :: RefrigIndex       = 0       ! Index used in fluid property routines
  INTEGER    :: RefrigSysNum      = 0       ! Index of refrigeration system
  INTEGER    :: TransRefrigSysNum = 0       ! Index of transcritical CO2 refrigeration system
  INTEGER    :: SecondaryIndex    = 0       ! Index of secondary loops
  INTEGER    :: SecondaryID       = 0       ! Index of secondary loops
  INTEGER    :: SetID             = 0       ! Index of refrigerated chilller SETS
  INTEGER    :: SecondaryNum      = 0       ! Index of secondary loops
  !INTEGER    :: TransferLoadListIndex = 0      ! Index of TransferLoad lists
  !INTEGER    :: TransferLoadListID    = 0      ! Index of TransferLoad lists
  INTEGER    :: TransferLoadListNum   = 0      ! Index of TransferLoad lists
!  INTEGER    :: InputType         = 0       ! Type of inlet, capcity in W or brine flow rate in m3/s
  INTEGER    :: SubcoolerNum      = 0       ! Index of subcooler
  INTEGER    :: TSNum             = 0       ! Counter for time steps in hour
  INTEGER    :: WalkInIndex       = 0       ! Index of walk ins
  INTEGER    :: WalkInID          = 0       ! Index of walk ins
  INTEGER    :: WalkInNum         = 0       ! Index of walk ins
  INTEGER    :: TotFields         = 0       ! Used to calc number of zones on input for walk in coolers
  INTEGER    :: ZoneID            = 0       ! Index to zone
  INTEGER    :: ZoneIndex         = 0       ! Index to zone
  INTEGER    :: ZoneNum           = 0       ! Index to zone
  REAL(r64)  :: CalcCircRate      = 0.0d0     ! Calculted circ rate in secondary phase change loop, dimensionless
  REAL(r64)  :: CalcTotFlowVol    = 0.0d0     ! Secondary loop flow in phase change liquid overfeed system (m3/s)
  REAL(r64)  :: CaseHeatGain      = 0.0d0     ! Case sensible heat gain used for error messages
  REAL(r64)  :: CapacityAtMaxVolFlow = 0.0d0  ! Secondary loop capacity (W)
  REAL(r64)  :: CpBrineRated      = 0.0d0     ! specific heat of circ fluid in secondary loop
  REAL(r64)  :: Capmin             =0.0d0     ! min heat rej for heat rej curve for air cooled condenser (W)
  REAL(r64)  :: Capmax             =0.0d0     ! max heat rej for heat rej curve for air cooled condenser (W)
  REAL(r64)  :: DeltaCap1          =0.0d0     ! fraction dif in capacity for input error check
  REAL(r64)  :: DeltaCap2          =0.0d0     ! fraction dif in capacity for input error check
  REAL(r64)  :: DeltaHPhaseChange  = 0.0d0    ! Secondary loop enthalpy change in condenser w overfeed system (J/g)
  REAL(r64)  :: DelTempMin         =0.0d0     ! min temperature for heat rej curve for air cooled condenser (C)
  REAL(r64)  :: DelTempMax         =0.0d0     ! max temperature for heat rej curve for air cooled condenser (C)
  REAL(r64)  :: DensityBrineRated  = 0.0d0    ! density of circ fluid in secondary loop
  REAL(r64)  :: DensityPhaseChange = 0.0d0    ! Secondary loop density at condensing temperature w overfeed system (g/m3)
  REAL(r64)  :: DesignSensibleCap  = 0.0d0    ! Case sensible capacity used for error messages
  REAL(r64)  :: DiffCircRates      =0.0d0     ! Difference between calculated and specified circ rates, fraction
  REAL(r64)  :: ErrSecondPumpPower = 0.0d0    ! Used to check consistency when both head and power input
  REAL(r64)  :: FlowMassRated      =0.0d0     ! Design mass flow rate of circ fluid in secondary loop(kg/s)
  REAL(r64)  :: GCOutletH          = 0.0d0  ! Gas cooler outlet enthalpy (J/kg)
  REAL(r64)  :: NominalSecondaryCapacity = 0.0d0  ! Rated Capacity from input data, W
  REAL(r64)  :: NominalSecondaryRefLoad = 0.0d0   ! Load from all connected cases and walkins, W
  REAL(r64)  :: NominalTotalCascadeLoad = 0.0d0   ! Load from all connected cascade condensers, W
  REAL(r64)  :: NominalTotalCaseCap=0.0d0     ! Total of nominal case capacities, used for rough input check (W)
  REAL(r64)  :: NominalTotalCoilCap=0.0d0     ! Total of nominal case capacities, used for rough input check (W)
  REAL(r64)  :: NominalTotalWalkInCap=0.0d0   ! Total of nominal walk-in capacities, used for rough input check (W)
  REAL(r64)  :: NominalTotalSecondaryCap=0.0d0    ! Total of nominal secondary capacities, used for rough input check (W)
  REAL(r64)  :: NominalTotalCaseCapMT=0.0d0   ! Total of nominal medium temperature case capacities, used for rough input check (W) (Transcritical CO2)
  REAL(r64)  :: NominalTotalCaseCapLT=0.0d0   ! Total of nominal low temperature case capacities, used for rough input check (W) (Transcritical CO2)
  REAL(r64)  :: NominalTotalWalkInCapMT=0.0d0 ! Total of nominal medium temperature walk-in capacities, used for rough input check (W) (Transcritical CO2)
  REAL(r64)  :: NominalTotalWalkInCapLT=0.0d0 ! Total of nominal low temperature walk-in capacities, used for rough input check (W) (Transcritical CO2)
  REAL(r64)  :: NominalTotalCoolingCap=0.0d0  ! Total of nominal load capacities, used for rough input check (W)
  REAL(r64)  :: NominalTotalCompCap=0.0d0     ! Total of nominal compressor capacities, used for rough input check (W)
  REAL(r64)  :: NominalTotalHiStageCompCap=0.0d0  ! Total of nominal high-stage compressor capacities, used for rough input check (W)
  REAL(r64)  :: NominalTotalCompCapHP=0.0d0   ! Total of nominal high pressure compressor capacities, used for rough input check (W) (Transcritical CO2)
  REAL(r64)  :: NominalTotalCompCapLP=0.0d0   ! Total of nominal low pressure compressor capacities, used for rough input check (W) (Transcritical CO2)
  REAL(r64)  :: NominalCondCap     =0.0d0     ! Nominal Condenser capacity, used for rough input check (W)
  REAL(r64)  :: Pcond = 0.0d0                 ! Condensing Pressure (Pa)
  REAL(r64)  :: Pevap = 0.0d0                 ! Evaporating Pressure (Pa)
  REAL(r64)  :: PumpTotRatedHead   =0.0d0     ! Total pump rated head on secondary loop (Pa)
  REAL(r64)  :: PumpTotRatedFlowVol = 0.0d0   ! Rated flow from input pump data, m3/s
  REAL(r64)  :: Rcase             = 0.0d0     ! Case thermal resistance used with anti-sweat heater control
  REAL(r64)  :: RcaseDenom        = 0.0d0     ! Denominator of case thermal resistance calculation for anti-sweat
  REAL(r64)  :: SecondaryFlowVolRated= 0.0d0  ! Rated flow of secondary fluid, used to calculate capacity (m3/s)
  REAL(r64)  :: TBrineOutRated =  0.0d0       ! Rated temperature of circ fluid LEAVING heat exchanger,C
  REAL(r64)  :: TBrineInRated  =  0.0d0       ! Rated temperature of circ fluid going INTO heat exchanger, C
  REAL(r64)  :: TBrineAverage  =  0.0d0       ! Rated average of inlet and outlet temps, used for property look up, C
  REAL(r64)  :: TempRAFraction    = 0.0d0     ! Temporary sum of Return Air fraction per zone for reporting
  REAL(r64)  :: TestDelta      =  0.0d0      ! Used to compare secondary loop rated capacity to calculated capacity, fraction
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers                ! Numeric items for object
  REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: DayValues            ! Array of schedule values


  NumSimulationCascadeCondensers=GetNumObjectsFound('Refrigeration:Condenser:Cascade')
  NumSimulationCases=GetNumObjectsFound('Refrigeration:Case')
  NumSimulationCaseAndWalkInLists=GetNumObjectsFound('Refrigeration:CaseAndWalkInList')
  NumRefrigeratedRacks=GetNumObjectsFound('Refrigeration:CompressorRack')
  NumSimulationSecondarySystems=GetNumObjectsFound('Refrigeration:SecondarySystem')
  NumSimulationTransferLoadLists=GetNumObjectsFound('Refrigeration:TransferLoadList')
  NumSimulationWalkIns=GetNumObjectsFound('Refrigeration:WalkIn')
  NumRefrigSystems=GetNumObjectsFound('Refrigeration:System')
  NumTransRefrigSystems=GetNumObjectsFound('Refrigeration:TranscriticalSystem')
  NumSimulationCondAir=GetNumObjectsFound('Refrigeration:Condenser:AirCooled')
  NumSimulationCondEvap=GetNumObjectsFound('Refrigeration:Condenser:EvaporativeCooled')
  NumSimulationCondWater=GetNumObjectsFound('Refrigeration:Condenser:WaterCooled')
  NumSimulationGasCooler=GetNumObjectsFound('Refrigeration:GasCooler:AirCooled')
  NumRefrigCondensers=NumSimulationCondAir + NumSimulationCondEvap + NumSimulationCondWater + &
                      NumSimulationCascadeCondensers
  NumSimulationCompressors=GetNumObjectsFound('Refrigeration:Compressor')
  NumSimulationSubcoolers=GetNumObjectsFound('Refrigeration:Subcooler')
  NumCompressorLists=GetNumObjectsFound('Refrigeration:CompressorList')
  NumRefrigChillerSets=GetNumObjectsFound('ZoneHVAC:RefrigerationChillerSet')
  NumSimulationRefrigAirChillers=GetNumObjectsFound('Refrigeration:AirChiller')

! Set flags used later to avoid unnecessary steps.
  IF(NumRefrigeratedRacks == 0)  HaveRefrigRacks = .FALSE.
  IF(NumRefrigSystems == 0)      HaveDetailedRefrig = .FALSE.
  IF(NumTransRefrigSystems == 0) HaveDetailedTransRefrig = .FALSE.
  IF(NumSimulationCases == 0 .AND. NumSimulationWalkIns == 0)HaveCasesOrWalkins = .FALSE.
  IF(NumSimulationRefrigAirChillers == 0)HaveChillers = .FALSE.

  IF(NumRefrigeratedRacks > 0)THEN
    ALLOCATE(RefrigRack(NumRefrigeratedRacks))
    ALLOCATE(HeatReclaimRefrigeratedRack(NumRefrigeratedRacks))
    ALLOCATE(ShowCOPWarning(NumRefrigeratedRacks))
    ShowCOPWarning = .TRUE.
  END IF
  IF(NumRefrigSystems > 0)THEN
    ALLOCATE(System(NumRefrigSystems))
    ALLOCATE(ShowUnmetEnergyWarning(NumRefrigSystems))
    ALLOCATE(ShowHiStageUnmetEnergyWarning(NumRefrigSystems))
    ShowUnmetEnergyWarning = .TRUE.
    ShowHiStageUnmetEnergyWarning = .TRUE.
  END IF
  IF(NumTransRefrigSystems > 0 )THEN
    ALLOCATE(TransSystem(NumTransRefrigSystems))
    ALLOCATE(ShowUnmetEnergyWarningTrans(NumTransRefrigSystems))
    ShowUnmetEnergyWarningTrans = .TRUE.
  END IF
  IF(NumRefrigChillerSets > 0) ALLOCATE(AirChillerSet(NumRefrigChillerSets))
  IF(NumRefrigCondensers > 0)THEN
    ALLOCATE(HeatReclaimRefrigCondenser(NumRefrigCondensers))
    ALLOCATE(Condenser(NumRefrigCondensers))
  END IF
  IF(NumSimulationGasCooler > 0) THEN
    ALLOCATE(GasCooler(NumSimulationGasCooler))
  END IF
  IF(NumSimulationCases > 0)THEN
    ALLOCATE(CaseRAFraction(NumOfZones))
    ALLOCATE(RefrigCase(NumSimulationCases))
    ALLOCATE(ShowStockingWarning(NumSimulationCases))
    ShowStockingWarning = .TRUE.
    ALLOCATE(ShowFrostWarning(NumSimulationCases))
    ShowFrostWarning = .TRUE.
    ALLOCATE(ShowStoreEnergyWarning(NumSimulationCases))
    ShowStoreEnergyWarning = .TRUE.
  END IF
  IF(NumSimulationWalkIns > 0)THEN
    ALLOCATE(WalkIn(NumSimulationWalkIns))
    ALLOCATE(ShowUnMetWIEnergyWarning(NumSimulationWalkIns))
    ShowUnMetWIEnergyWarning = .TRUE.
    ALLOCATE(ShowWIFrostWarning(NumSimulationWalkIns))
    ShowWIFrostWarning = .TRUE.
  END IF
  IF((NumSimulationWalkIns > 0) .OR. (NumSimulationCases > 0))THEN
    ALLOCATE(CaseWIZoneReport(NumOfZones))
  ELSE
    UseSysTimeStep = .TRUE.
    !needed to avoid accessing unallocated caseWIZoneReport on early call to SumZones
  END IF
  IF(NumSimulationSecondarySystems > 0)THEN
    ALLOCATE(Secondary(NumSimulationSecondarySystems))
    ALLOCATE(ShowUnmetSecondEnergyWarning(NumSimulationSecondarySystems))
    ShowUnmetSecondEnergyWarning = .TRUE.
  END IF
  IF(NumSimulationRefrigAirChillers > 0)THEN
    ALLOCATE(WareHouseCoil(NumSimulationRefrigAirChillers))
    ALLOCATE(ShowCoilFrostWarning(NumSimulationRefrigAirChillers))
    ALLOCATE(CoilSysCredit(NumOfZones))
    ShowCoilFrostWarning = .TRUE.
  END IF
  IF(NumSimulationCompressors > 0)ALLOCATE(Compressor(NumSimulationCompressors))
  IF(NumSimulationSubcoolers > 0)ALLOCATE(Subcooler(NumSimulationSubcoolers))
  IF(NumSimulationCaseAndWalkInLists > 0)ALLOCATE(CaseAndWalkInList(NumSimulationCaseAndWalkInLists))
  IF(NumCompressorLists > 0)ALLOCATE(CompressorLists(NumCompressorLists))
  IF(NumSimulationTransferLoadLists > 0)ALLOCATE(TransferLoadList(NumSimulationTransferLoadLists))

  ALLOCATE(DayValues(24,NumOfTimeStepInHour))
  ALLOCATE(RefrigPresentInZone(NumOfZones))
    RefrigPresentInZone = .FALSE.

  CALL GetObjectDefMaxArgs('Refrigeration:Case',MaxNumArgs,MaxNumAlphasCase,MaxNumNumbersCase)
  CALL GetObjectDefMaxArgs('Refrigeration:CaseAndWalkInList',MaxNumArgs,MaxNumAlphasCaseAndWalkInList, &
                               MaxNumNumbersCaseAndWalkInList)
  CALL GetObjectDefMaxArgs('Refrigeration:CompressorRack',MaxNumArgs,MaxNumAlphasRack,MaxNumNumbersRack)
  CALL GetObjectDefMaxArgs('Refrigeration:System',MaxNumArgs,MaxNumAlphasSys,MaxNumNumbersSys)
  CALL GetObjectDefMaxArgs('Refrigeration:TranscriticalSystem',MaxNumArgs,MaxNumAlphasTransSys, &
                            MaxNumNumbersTransSys)
  CALL GetObjectDefMaxArgs('Refrigeration:Condenser:AirCooled',MaxNumArgs,MaxNumAlphasConda, &
                            MaxNumNumbersConda)
  CALL GetObjectDefMaxArgs('Refrigeration:Condenser:EvaporativeCooled',MaxNumArgs, &
                            MaxNumAlphasConde,MaxNumNumbersConde)
  CALL GetObjectDefMaxArgs('Refrigeration:Condenser:WaterCooled',MaxNumArgs,MaxNumAlphasCondw, &
                            MaxNumNumbersCondw)
  CALL GetObjectDefMaxArgs('Refrigeration:GasCooler:AirCooled',MaxNumArgs,MaxNumAlphasGasCoolera, &
                            MaxNumNumbersGasCoolera)
  CALL GetObjectDefMaxArgs('Refrigeration:Compressor',MaxNumArgs,MaxNumAlphasComp,MaxNumNumbersComp)
  CALL GetObjectDefMaxArgs('Refrigeration:CompressorList',MaxNumArgs, &
                               MaxNumAlphasCompressorList,MaxNumNumbersCompressorList)
  CALL GetObjectDefMaxArgs('Refrigeration:WalkIn',MaxNumArgs,MaxNumAlphasWalkIn, &
                               MaxNumNumbersWalkIn)
  CALL GetObjectDefMaxArgs('Refrigeration:SecondarySystem',MaxNumArgs,MaxNumAlphasSecond, &
                               MaxNumNumbersSecond)
  CALL GetObjectDefMaxArgs('ZoneHVAC:RefrigerationChillerSet',MaxNumArgs,MaxNumAlphasChillerSet, &
                               MaxNumNumbersChillerSet)
  CALL GetObjectDefMaxArgs('Refrigeration:AirChiller',MaxNumArgs,MaxNumAlphasAirChiller, &
                               MaxNumNumbersAirChiller)

  MaxNumAlphasAll = MAX(MaxNumAlphasCase,MaxNumAlphasCaseAndWalkInList,MaxNumAlphasRack,&
                        MaxNumAlphasSys,MaxNumAlphasTransSys, MaxNumAlphasConda,MaxNumAlphasConde, &
                          MaxNumAlphasCondw,MaxNumAlphasGasCoolera,MaxNumAlphasComp, &
                          MaxNumAlphasCompressorList, &
                          MaxNumAlphasSecond,MaxNumAlphasWalkIn, MaxNumAlphasChillerSet,MaxNumAlphasAirChiller)
  MaxNumNumbersAll = MAX(MaxNumNumbersCase,MaxNumNumbersCaseAndWalkInList,MaxNumNumbersRack,&
                         MaxNumNumbersSys,MaxNumNumbersTransSys, MaxNumNumbersConda,MaxNumNumbersConde, &
                          MaxNumNumbersCondw,MaxNumNumbersGasCoolera,MaxNumNumbersComp, &
                          MaxNumNumbersCompressorList,MaxNumNumbersSecond, &
                          MaxNumNumbersWalkIn,  MaxNumNumbersChillerSet,  MaxNumNumbersAirChiller)

  ALLOCATE(Alphas(MaxNumAlphasAll))
  Alphas=' '
  ALLOCATE(Numbers(MaxNumNumbersAll))
  Numbers=0.0d0
  ALLOCATE(cAlphaFieldNames(MaxNumAlphasAll))
  cAlphaFieldNames=' '
  ALLOCATE(cNumericFieldNames(MaxNumNumbersAll))
  cNumericFieldNames=' '
  ALLOCATE(lAlphaBlanks(MaxNumAlphasAll))
  lAlphaBlanks=.TRUE.
  ALLOCATE(lNumericBlanks(MaxNumNumbersAll))
  lNumericBlanks=.TRUE.
 !bbb stovall note for future - for all curve entries, see if need fail on type or if can allow table input
 IF(NumSimulationCases > 0 ) THEN
  CurrentModuleObject='Refrigeration:Case'
  DO CaseNum=1,NumSimulationCases
    CALL GetObjectItem(CurrentModuleObject,CaseNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    NumDisplayCases = NumDisplayCases+1
    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    AlphaNum=1
    CALL VerifyName(Alphas(AlphaNum),RefrigCase%Name,CaseNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(AlphaNum))//&
                           '", invalid '//TRIM(cAlphaFieldNames(AlphaNum))//'+"'//TRIM(Alphas(AlphaNum)))
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    RefrigCase(CaseNum)%Name          = Alphas(AlphaNum)

    AlphaNum=2
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      RefrigCase(CaseNum)%SchedPtr = GetScheduleIndex(Alphas(AlphaNum))  ! convert schedule name to pointer
      IF (RefrigCase(CaseNum)%SchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                         '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      END IF ! ptr == 0
    ELSE  ! no schedule specified
          RefrigCase(CaseNum)%SchedPtr = AlwaysOn
    END IF ! not blank

!   check availability schedule for values between 0 and 1
    IF (RefrigCase(CaseNum)%SchedPtr > 0)THEN
      IF (.NOT. CheckScheduleValueMinMax(RefrigCase(CaseNum)%SchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//'"')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//' = '//TRIM(Alphas(AlphaNum)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.TRUE.
      END IF
    END IF

    !Get the Zone node number from the zone name entered by the user
    RefrigCase(CaseNum)%ZoneName      = Alphas(3)
    RefrigCase(CaseNum)%ActualZoneNum = FindItemInList(Alphas(3),Zone%Name,NumOfZones)

    IF (RefrigCase(CaseNum)%ActualZoneNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//  &
                           '", invalid  '//TRIM(cAlphaFieldNames(3))//' not valid: '//TRIM(Alphas(3)))
      ErrorsFound=.TRUE.
    ELSE
      RefrigPresentInZone(RefrigCase(CaseNum)%ActualZoneNum) = .TRUE.
    ENDIF

    RefrigCase(CaseNum)%ZoneNodeNum   = GetSystemNodeNumberForZone(RefrigCase(CaseNum)%ZoneName)
    RefrigCase(CaseNum)%ZoneRANode    = GetReturnAirNodeForZone(RefrigCase(CaseNum)%ZoneName)

    IF (RefrigCase(CaseNum)%ActualZoneNum >= 0) THEN
      IF (RefrigCase(CaseNum)%ZoneNodeNum == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//  &
                             '", System Node Number not found for '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(Alphas(3)))
        CALL ShowContinueError('..Refrigerated cases must reference a controlled Zone (appear'//  &
               ' in a ZoneHVAC:EquipmentConnections object).')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF

    RefrigCase(CaseNum)%RatedAmbientTemp = Numbers(1)
    IF(Numbers(1) <= 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(1))//' must be greater than 0 C')
      ErrorsFound = .TRUE.
    END IF

    RefrigCase(CaseNum)%RatedAmbientRH   = Numbers(2)
    IF(Numbers(2) <= 0.0d0 .OR. Numbers(2) >= 100.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(2))//' must be greater than 0% and less than 100%')
      ErrorsFound = .TRUE.
    END IF
    RefrigCase(CaseNum)%RatedAmbientDewPoint = &
           PsyTdpFnWPb(PsyWFnTdbRhPb(RefrigCase(CaseNum)%RatedAmbientTemp, &
                                     (RefrigCase(CaseNum)%RatedAmbientRH / 100.0d0), &
                                      StdBaroPress),StdBaroPress)

    RefrigCase(CaseNum)%RateTotCapPerLength      = Numbers(3)
    IF(Numbers(3) <= 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(3))//' must be greater than 0 W/m')
      ErrorsFound = .TRUE.
    END IF

    RefrigCase(CaseNum)%RatedLHR         = Numbers(4)
    IF(Numbers(4) < 0.0d0 .OR. Numbers(4) > 1.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(4))//' must be >= 0 and <= 1')
      ErrorsFound = .TRUE.
    END IF

    RefrigCase(CaseNum)%RatedRTF         = Numbers(5)
    IF(Numbers(5) <= 0.0d0 .OR. Numbers(5) > 1.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(5))//' must be > 0 and <= to 1')
      ErrorsFound = .TRUE.
    END IF

    RefrigCase(CaseNum)%Length           = Numbers(6)
    IF(Numbers(6) <= 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(6))//' must be greater than 0 m')
      ErrorsFound = .TRUE.
    END IF

    RefrigCase(CaseNum)%Temperature      = Numbers(7)
    IF(RefrigCase(CaseNum)%Temperature >= RefrigCase(CaseNum)%RatedAmbientTemp) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                        '", '//TRIM(cNumericFieldNames(7))//' must be below '//trim(cNumericFieldNames(1)))
                              ErrorsFound=.TRUE.
    END IF

    IF (SameString(Alphas(4),'CaseTemperatureMethod')) THEN
      RefrigCase(CaseNum)%LatentEnergyCurveType = CaseTemperatureMethod
    ELSEIF (SameString(Alphas(4),'RelativeHumidityMethod')) THEN
      RefrigCase(CaseNum)%LatentEnergyCurveType = RHCubic
    ELSEIF (SameString(Alphas(4),'DewpointMethod')) THEN
      RefrigCase(CaseNum)%LatentEnergyCurveType = DPCubic
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(Alphas(4))//'".')
      ErrorsFound=.TRUE.
    END IF

    RefrigCase(CaseNum)%LatCapCurvePtr   = GetCurveIndex(Alphas(5)) ! convert curve name to number
    IF (RefrigCase(CaseNum)%LatCapCurvePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(5))//' not found:'//TRIM(Alphas(5)))
      ErrorsFound = .TRUE.
    END IF

    IF(.NOT. SameString(GetCurveType(RefrigCase(CaseNum)%LatCapCurvePtr),'CUBIC')) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(5))//' object must be of type Cubic.')
      ErrorsFound = .TRUE.
    END IF

    NumNum = 8
    IF (.NOT. lNumericBlanks(NumNum)) THEN
      RefrigCase(CaseNum)%STDFanPower        = Numbers(NumNum)
      IF(Numbers(NumNum) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than or equal to 0 W/m')
        ErrorsFound = .TRUE.
      END IF
    ELSE !blank use default of 75 W/m
      RefrigCase(CaseNum)%STDFanPower        = 75.d0
    END IF ! blank input

    NumNum = 9
    IF (.NOT. lNumericBlanks(NumNum)) THEN
      RefrigCase(CaseNum)%OperatingFanPower  = Numbers(NumNum)
      IF(Numbers(NumNum) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than or equal to 0 W/m')
        ErrorsFound = .TRUE.
      END IF
    ELSE ! if blank set = to std fan power
      RefrigCase(CaseNum)%OperatingFanPower  = RefrigCase(CaseNum)%STDFanPower
    END IF ! if blank

    NumNum = 10
    IF (.NOT. lNumericBlanks(NumNum)) THEN
      RefrigCase(CaseNum)%RatedLightingPower       = Numbers(NumNum)
      IF(Numbers(NumNum) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than or equal to 0 W/m')
        ErrorsFound = .TRUE.
      END IF
    ELSE !blank input - use default of 90 W/m
      RefrigCase(CaseNum)%RatedLightingPower       = 90.d0
    END IF ! blank input

    NumNum = 11
    IF (.NOT. lNumericBlanks(NumNum)) THEN
      RefrigCase(CaseNum)%LightingPower      = Numbers(NumNum)
      IF(Numbers(NumNum) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than or equal to 0 W/m')
        ErrorsFound = .TRUE.
      END IF
    ELSE ! blank input so set lighting power equal to rated/std lighting power
      RefrigCase(CaseNum)%LightingPower = RefrigCase(CaseNum)%RatedLightingPower
    END IF ! blank input

    IF (.NOT. lAlphaBlanks(6)) THEN
    RefrigCase(CaseNum)%LightingSchedPtr   = GetScheduleIndex(Alphas(6))  ! convert schedule name to pointer
      IF (RefrigCase(CaseNum)%LightingSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(6))//' not found: '//TRIM(Alphas(6)))
        ErrorsFound=.TRUE.
      END IF ! ptr == 0
    ELSE  ! no schedule specified
      RefrigCase(CaseNum)%LightingSchedPtr = AlwaysOn
    END IF ! not blank

!   check lighting schedule for values between 0 and 1
    IF (RefrigCase(CaseNum)%LightingSchedPtr > 0)THEN
      IF (.NOT. CheckScheduleValueMinMax(RefrigCase(CaseNum)%LightingSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//'"')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(Alphas(6)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.true.
      END IF
    END IF

    NumNum = 12
    RefrigCase(CaseNum)%LightingFractionToCase = 1.d0 !default value
    IF (.NOT. lNumericBlanks(NumNum)) THEN
      RefrigCase(CaseNum)%LightingFractionToCase = Numbers(NumNum)
    END IF ! blank input lighting fraction to case
!   check lighting fraction to case input
    IF (RefrigCase(CaseNum)%LightingFractionToCase < 0.0d0 .OR. &
        RefrigCase(CaseNum)%LightingFractionToCase  > 1.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(NumNum))//' has a value outside the valid range')
        CALL ShowContinueError('  Minimum should be >= 0.0 and Maximum should be <= 1.0')
        ErrorsFound=.TRUE.
    END IF

    NumNum = 13
    RefrigCase(CaseNum)%AntiSweatPower  = Numbers(NumNum)
    IF(Numbers(NumNum) < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than or equal to 0 W/m')
      ErrorsFound = .TRUE.
    END IF

    NumNum = 14
    RefrigCase(CaseNum)%MinimumASPower  = Numbers(NumNum)
    IF(Numbers(NumNum) < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than or equal to 0 W/m')
      ErrorsFound = .TRUE.
    END IF

    IF (SameString(Alphas(7),'None')) THEN
      RefrigCase(CaseNum)%AntiSweatControlType = ASNone
      RefrigCase(CaseNum)%AntiSweatPower = 0.0d0
    ELSEIF (SameString(Alphas(7),'Constant')) THEN
      RefrigCase(CaseNum)%AntiSweatControlType = ASConstant
    ELSEIF (SameString(Alphas(7),'Linear')) THEN
      RefrigCase(CaseNum)%AntiSweatControlType = ASLinear
    ELSEIF (SameString(Alphas(7),'DewpointMethod')) THEN
      RefrigCase(CaseNum)%AntiSweatControlType = ASDewPoint
    ELSEIF (SameString(Alphas(7),'HeatBalanceMethod')) THEN
      RefrigCase(CaseNum)%AntiSweatControlType = ASHeatBalance
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(Alphas(7))//'".')
      ErrorsFound=.TRUE.
    END IF

!   Assure that case temperature is below the rated dew point when anti-sweat heater control type is dew point method
    IF(RefrigCase(CaseNum)%Temperature >= RefrigCase(CaseNum)%RatedAmbientDewPoint .AND. &
       RefrigCase(CaseNum)%AntiSweatControlType == ASDewPoint) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                      '", '//TRIM(cNumericFieldNames(7))//' must be below the Rated Ambient Dew Point when '//&
                      TRIM(cAlphaFieldNames(7))//' is Dew Point Method')
        ErrorsFound=.TRUE.
    END IF

    NumNum = 15
!  negative values for minimum humidity are allowed
    RefrigCase(CaseNum)%HumAtZeroAS   = Numbers(NumNum)

!   check minimum humidity when linear AS control type is used
    IF (RefrigCase(CaseNum)%HumAtZeroAS >= &
        RefrigCase(CaseNum)%RatedAmbientRH .AND. &
        RefrigCase(CaseNum)%AntiSweatControlType == ASLinear) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                      '", '//TRIM(cNumericFieldNames(NumNum))//' must be less than '//TRIM(cNumericFieldNames(2)))
        CALL ShowContinueError(' for Linear '//TRIM(cAlphaFieldNames(7))//'.')
        ErrorsFound=.TRUE.
    END IF

    NumNum = 16
    RefrigCase(CaseNum)%Height        = Numbers(NumNum)
    IF(Numbers(NumNum) < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than or equal to 0 m')
      ErrorsFound = .TRUE.
    END IF

    IF (RefrigCase(CaseNum)%Height <= 0.0d0 .AND. &
        RefrigCase(CaseNum)%AntiSweatControlType == ASHeatBalance) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                    '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than 0 when '//TRIM(cAlphaFieldNames(7))// &
                    ' is Heat Balance Method.')
        CALL ShowContinueError('..given '//TRIM(cNumericFieldNames(NumNum))//' was: '//&
                               TRIM(RoundSigDigits(RefrigCase(CaseNum)%Height,3)))
        ErrorsFound=.TRUE.
    END IF

!   initialize case resistance for anti-sweat heater control type = Heat Balance Method
    IF(RefrigCase(CaseNum)%AntiSweatControlType == ASHeatBalance) THEN
       IF(RefrigCase(CaseNum)%Height == 0.0d0) THEN
         Rcase = 0.0d0
       ELSE
         RcaseDenom = ((RefrigCase(CaseNum)%AntiSweatPower / &
                        RefrigCase(CaseNum)%Height) - &
                       (RefrigCase(CaseNum)%RatedAmbientDewPoint- &
                        RefrigCase(CaseNum)%RatedAmbientTemp)/Rair)
         Rcase = (RefrigCase(CaseNum)%RatedAmbientDewPoint - &
                  RefrigCase(CaseNum)%Temperature) / RcaseDenom
       END IF
       RefrigCase(CaseNum)%Rcase = MAX(0.0d0,Rcase)
       IF(RefrigCase(CaseNum)%Rcase == 0.0d0) THEN
         CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                                 '" A case thermal resistance of 0 was calculated for anti-sweat heater performance using the')
         CALL ShowContinueError(' Heat Balance Method control type. Anti-sweat heater performance cannot be calculated '// &
                                 'and '//TRIM(cAlphaFieldNames(7))//' will be set to None and simulation continues.')
         CALL ShowContinueError(' See Engineering Documentation for anti-sweat heater control of refrigerated cases.')
       END IF
    END IF

    NumNum = 17
    RefrigCase(CaseNum)%ASHeaterFractionToCase = Numbers(NumNum)
    IF(Numbers(NumNum) < 0.0d0 .OR. Numbers(NumNum) > 1.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                         '", '//TRIM(cNumericFieldNames(NumNum))//' must be >= 0 and <= 1')
      ErrorsFound = .TRUE.
    END IF

    IF (SameString(Alphas(8),'None')) THEN
      RefrigCase(CaseNum)%DefrostType = DefNone
    ELSEIF (SameString(Alphas(8),'OffCycle')) THEN
      RefrigCase(CaseNum)%DefrostType = DefOffCycle
    ELSEIF ((SameString(Alphas(8),'HotFluid')) .OR. &
            (SameString(Alphas(8),'HotGas'  ))) THEN
      RefrigCase(CaseNum)%DefrostType = DefHotFluid
    ELSEIF ((SameString(Alphas(8),'HotFluidWithTemperatureTermination')) .OR. &
            (SameString(Alphas(8),'HotGasWithTemperatureTermination'  ))) THEN
      RefrigCase(CaseNum)%DefrostType = DefHotFluidTerm
!   ELSEIF (SameString(Alphas(8),'Hot-Fluid On Demand')) THEN
!     RefrigCase(CaseNum)%DefrostType = DefHotFluidOnDemand
    ELSEIF (SameString(Alphas(8),'Electric')) THEN
      RefrigCase(CaseNum)%DefrostType = DefElectric
    ELSEIF (SameString(Alphas(8),'ElectricWithTemperatureTermination')) THEN
      RefrigCase(CaseNum)%DefrostType = DefElectricTerm
!   ELSEIF (SameString(Alphas(8),'Electric On Demand')) THEN
!     RefrigCase(CaseNum)%DefrostType = DefElectricOnDemand
    ELSE
      CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                          '", invalid  '//TRIM(cAlphaFieldNames(8))//'="'//TRIM(Alphas(8))//'".')
      CALL ShowContinueError('Simulation will default to '//TRIM(cAlphaFieldNames(8))//'="None" and continue.')
      RefrigCase(CaseNum)%DefrostType = DefNone
    END IF

    DefType = RefrigCase(CaseNum)%DefrostType
    NumNum = 18
    IF (.NOT. lNumericBlanks(NumNum)) THEN
      RefrigCase(CaseNum)%DefrostPower  = Numbers(NumNum)
      IF(Numbers(NumNum) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than or equal to 0 W/m')
        ErrorsFound = .TRUE.
      END IF
      !   disregard defrost power for Off-Cycle or None defrost types
      IF((DefType==DefOffCycle .OR. DefType==DefOffCycle) .AND.(RefrigCase(CaseNum)%DefrostPower > 0.0d0)) THEN
        RefrigCase(CaseNum)%DefrostPower=0.0d0
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' for '//TRIM(cAlphaFieldNames(8))// &
                           ' None or Off-Cycle will be set to 0 and simulation continues.')
      END IF
    ELSE
      RefrigCase(CaseNum)%DefrostPower  = 0.0d0
    END IF

    !defrost power needed to calculate heat gain to case even if not needed for electric consumption
    IF((DefType==DefHotFluid   .OR. DefType==DefHotFluidTerm   .OR. &
        DefType==DefElectric .OR. DefType==DefElectricTerm ) .AND. &
        RefrigCase(CaseNum)%DefrostPower <= 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than 0 W/m'//' for '//&
                           TRIM(cAlphaFieldNames(8))//' '//TRIM(Alphas(8)))
        ErrorsFound = .TRUE.
    END IF

    RefrigCase(CaseNum)%DefrostSchedPtr   = GetScheduleIndex(Alphas(9))  ! convert schedule name to pointer
    IF (RefrigCase(CaseNum)%DefrostSchedPtr == 0 .AND. &
        RefrigCase(CaseNum)%DefrostType /= DefNone) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(9))//' not found: '//TRIM(Alphas(9)))
      CALL ShowContinueError('required when '//trim(cAlphaFieldNames(8))//'="'//trim(Alphas(8))//'".')
      ErrorsFound=.TRUE.
    END IF

!   check defrost schedule for values between 0 and 1
    IF (RefrigCase(CaseNum)%DefrostSchedPtr > 0)THEN
      IF (.NOT. CheckScheduleValueMinMax(RefrigCase(CaseNum)%DefrostSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//'".')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(9))//' = '//TRIM(Alphas(9)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.true.
      END IF
    END IF
!   Note that next section counting number cycles and setting maxkgfrost not used now, but may be in the future.
!   count the number of defrost cycles
    StartCycle = .FALSE.
    NumDefCycles = 0
    DayValues = 0.0d0
    CALL GetScheduleValuesForDay(RefrigCase(CaseNum)%DefrostSchedPtr,DayValues,1)
    DO HRNum = 1,24
      DO TSNum = 1,NumOfTimeStepInHour
        IF(DayValues(HRNum,TSNum) > 0.0d0) THEN
          IF(.NOT. StartCycle) THEN
            NumDefCycles = NumDefCycles + 1
            StartCycle = .TRUE.
          END IF
        ELSE
          StartCycle = .FALSE.
        END IF
      END DO
    END DO

    IF(NumDefCycles > 0) THEN
!     calculate maximum frost formation based on defrost schedule, heat of vaporization+fusion for water = 2833.0 kJ/kg
      RefrigCase(CaseNum)%MaxKgFrost  = (RefrigCase(CaseNum)%RateTotCapPerLength * RefrigCase(CaseNum)%RatedLHR * &
                                         RefrigCase(CaseNum)%RatedRTF * SecondsPerHour * HoursPerDay / 1000.0d0 / 2833.0d0) &
                                         /(NumDefCycles)
    ELSE
      RefrigCase(CaseNum)%MaxKgFrost  = 9999999.9d0
    END IF

!   some defrost types do not use drip-down schedules, use same defrost schedule pointer in that case
    IF (.NOT. lAlphaBlanks(10)) THEN
      RefrigCase(CaseNum)%DefrostDripDownSchedPtr = GetScheduleIndex(Alphas(10))  ! convert schedule name to pointer
      IF (RefrigCase(CaseNum)%DefrostDripDownSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(10))//' not found: '//TRIM(Alphas(10)))
        ErrorsFound=.TRUE.
      END IF
    ELSE
      RefrigCase(CaseNum)%DefrostDripDownSchedPtr = RefrigCase(CaseNum)%DefrostSchedPtr
    END IF

!   check defrost drip-down schedule for values between 0 and 1
    IF (RefrigCase(CaseNum)%DefrostDripDownSchedPtr > 0 .AND. (.NOT. lAlphaBlanks(10)))THEN
      IF (.NOT. CheckScheduleValueMinMax(RefrigCase(CaseNum)%DefrostDripDownSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//'".')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(10))//' = '//TRIM(Alphas(10)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.TRUE.
      END IF
    END IF

    IF (SameString(Alphas(11),'CaseTemperatureMethod')) THEN
      RefrigCase(CaseNum)%DefrostEnergyCurveType = CaseTemperatureMethod
    ELSEIF (SameString(Alphas(11),'RelativeHumidityMethod')) THEN
      RefrigCase(CaseNum)%DefrostEnergyCurveType = RHCubic
    ELSEIF (SameString(Alphas(11),'DewpointMethod')) THEN
      RefrigCase(CaseNum)%DefrostEnergyCurveType = DPCubic
    ELSEIF (SameString(Alphas(11),'None')) THEN
      RefrigCase(CaseNum)%DefrostEnergyCurveType = None
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(11))//'="'//TRIM(Alphas(11))//'".')
      ErrorsFound=.TRUE.
    END IF

    RefrigCase(CaseNum)%DefCapCurvePtr   = GetCurveIndex(Alphas(12)) ! convert curve name to number
    IF((RefrigCase(CaseNum)%DefrostType == DefElectricTerm .OR. &
        RefrigCase(CaseNum)%DefrostType == DefHotFluidTerm) .AND. &
        (RefrigCase(CaseNum)%DefCapCurvePtr == 0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(12))//' not found:'//TRIM(Alphas(12)))
        ErrorsFound = .TRUE.
    END IF

    IF (RefrigCase(CaseNum)%DefCapCurvePtr > 0) THEN
      IF(.NOT. SameString(GetCurveType(RefrigCase(CaseNum)%DefCapCurvePtr),'CUBIC')) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(12))//' must be of type Cubic.')
        ErrorsFound = .TRUE.
      END IF
    END IF

!  warn user if defrost energy curve is entered that it is only used for temperature termination types
    IF(RefrigCase(CaseNum)%DefCapCurvePtr > 0)THEN
      IF(  RefrigCase(CaseNum)%DefrostType /= DefElectricTerm .AND. &
           RefrigCase(CaseNum)%DefrostType /= DefHotFluidTerm ) THEN
          CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                          '", invalid  '//TRIM(cAlphaFieldNames(12))// &
                          ' is only applicable to Defrost Temperature Termination types.')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(12))//' will be disregarded and simulation continues.')
      END IF
    END IF

    NumNum = 19
    RefrigCase(CaseNum)%RAFrac        = Numbers(NumNum)
    IF(Numbers(NumNum) < 0.0d0 .OR. Numbers(NumNum) > 1.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be >= 0 or <= 1 ')
      ErrorsFound = .TRUE.
    END IF

    ! set flag in Zone Data if RAFrac > 0
    IF (RefrigCase(CaseNum)%RAFrac > 0.0d0) THEN
      Zone(RefrigCase(CaseNum)%ActualZoneNum)%RefrigCaseRA = .TRUE.
    END IF

!   Make sure RA node exists for display cases with under case HVAC returns
    IF(RefrigCase(CaseNum)%ZoneRANode == 0 .AND. &
       RefrigCase(CaseNum)%RAFrac > 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                         '", '//TRIM(cNumericFieldNames(18))//' not applicable to zones without return air systems.')
      ErrorsFound=.TRUE.
    END IF

    IF(RefrigCase(CaseNum)%ActualZoneNum /= 0) THEN
      CaseRAFraction(RefrigCase(CaseNum)%ActualZoneNum)%TotalCaseRAFraction = &
        CaseRAFraction(RefrigCase(CaseNum)%ActualZoneNum)%TotalCaseRAFraction + &
        RefrigCase(CaseNum)%RAFrac
      CaseRAFraction(RefrigCase(CaseNum)%ActualZoneNum)%ZoneName = &
        RefrigCase(CaseNum)%ZoneName
    END IF

    RefrigCase(CaseNum)%StockingSchedPtr = GetScheduleIndex(Alphas(13))  ! convert schedule name to pointer
    IF (.NOT. lAlphaBlanks(13)) THEN
      IF (RefrigCase(CaseNum)%StockingSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(13))//' not found: '//TRIM(Alphas(13)))
        ErrorsFound=.TRUE.
      END IF
    ELSE
      RefrigCase(CaseNum)%StockingSchedPtr = 0
    END IF

!   calculate sensible case load at design conditions
    DesignSensibleCap = RefrigCase(CaseNum)%RateTotCapPerLength * &
                        (1.0d0-RefrigCase(CaseNum)%RatedLHR) * &
                        RefrigCase(CaseNum)%RatedRTF    * &
                        RefrigCase(CaseNum)%Length

!   calculate case heat gain = lights + fans + anti-sweat
    CaseHeatGain      = ((RefrigCase(CaseNum)%RatedLightingPower *       &
                          RefrigCase(CaseNum)%LightingFractionToCase) + &
                         (RefrigCase(CaseNum)%AntiSweatPower *      &
                          RefrigCase(CaseNum)%ASHeaterFractionToCase) + &
                          RefrigCase(CaseNum)%STDFanPower)           * &
                          RefrigCase(CaseNum)%Length

!   sensible case credits are calculated as the difference between the design sensible capacity and the case heat gain
    RefrigCase(CaseNum)%DesignSensCaseCredit = DesignSensibleCap - CaseHeatGain

!   compare case loads to design capacity
    IF (DesignSensibleCap < CaseHeatGain) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                       TRIM(RefrigCase(CaseNum)%Name)//'", the sum of '//&
                       'lighting, fan, and anti-sweat heater energy is greater than refrigerated case sensible capacity')
      ErrorsFound=.TRUE.
    END IF

    RefrigCase(CaseNum)%CaseCreditFracSchedPtr = GetScheduleIndex(Alphas(14))  ! convert schedule name to pointer
    IF (.NOT. lAlphaBlanks(14)) THEN
      IF (RefrigCase(CaseNum)%CaseCreditFracSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(14))//' not found: '//TRIM(Alphas(14)))
        ErrorsFound=.TRUE.
      END IF
    ELSE
      RefrigCase(CaseNum)%CaseCreditFracSchedPtr = 0
    END IF

!   check case credit fraction schedule for values between 0 and 1
    IF (RefrigCase(CaseNum)%CaseCreditFracSchedPtr > 0) THEN
      IF (.NOT. CheckScheduleValueMinMax(RefrigCase(CaseNum)%CaseCreditFracSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//'".')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(14))//' = '//TRIM(Alphas(14)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.true.
      END IF
    END IF

      RefrigCase(CaseNum)%DesignRatedCap  = RefrigCase(CaseNum)%RateTotCapPerLength  * RefrigCase(CaseNum)%Length
      RefrigCase(CaseNum)%DesignLatentCap = RefrigCase(CaseNum)%DesignRatedCap * RefrigCase(CaseNum)%RatedLHR *&
                                             RefrigCase(CaseNum)%RatedRTF
      RefrigCase(CaseNum)%DesignDefrostCap= RefrigCase(CaseNum)%DefrostPower * RefrigCase(CaseNum)%Length
      RefrigCase(CaseNum)%DesignLighting  = RefrigCase(CaseNum)%LightingPower * RefrigCase(CaseNum)%Length
      RefrigCase(CaseNum)%DesignFanPower  = RefrigCase(CaseNum)%OperatingFanPower * RefrigCase(CaseNum)%Length

    !Design evaporating temperature:  for a DX system, saturated temperature for pressure leaving case
    !                              :  for a liquid system, liquid temperature entering case
    NumNum = 20
    IF (.NOT. lNumericBlanks(NumNum)) THEN
      RefrigCase(CaseNum)%EvapTempDesign = Numbers(NumNum)
      IF(RefrigCase(CaseNum)%EvapTempDesign >= RefrigCase(CaseNum)%Temperature) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                        '" '//TRIM(cNumericFieldNames(NumNum))//' must be below '//TRIM(cNumericFieldNames(7)))
        ErrorsFound=.TRUE.
      END IF
    ELSE
      RefrigCase(CaseNum)%EvapTempDesign = RefrigCase(CaseNum)%Temperature - DelEvapTDefault
      !    default 5C less than case operating temperature
    END IF

    NumNum = 21
    IF (.NOT. lNumericBlanks(NumNum)) THEN
       RefrigCase(Casenum)%RefrigInventory = Numbers(NumNum)
       RefrigCase(Casenum)%DesignRefrigInventory =RefrigCase(Casenum)%RefrigInventory * RefrigCase(CaseNum)%Length
       IF(RefrigCase(Casenum)%RefrigInventory < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigCase(CaseNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(NumNum))//' must be a positive number.')
        ErrorsFound = .TRUE.
       END IF
     ELSE
       RefrigCase(Casenum)%RefrigInventory = 0.0d0
     END IF

  ENDDO  !Individual refrigerated cases
 END IF  !(NumSimulationCases > 0 )

!************ START WALK IN COOLER INPUT **************

 IF(NumSimulationWalkIns > 0 ) THEN
  CurrentModuleObject='Refrigeration:WalkIn'
  DO  WalkInID=1,NumSimulationWalkIns
    CALL GetObjectItem(CurrentModuleObject, WalkInID,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1), WalkIn%Name, WalkInID-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')

    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined name="'//&
                           TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    WalkIn( WalkInID)%Name          = Alphas(1)

    IF (.NOT. lAlphaBlanks(2)) THEN
      WalkIn( WalkInID)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
      IF ( WalkIn( WalkInID)%SchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                             '", invalid  '//TRIM(cAlphaFieldNames(2))//' not found: '//TRIM(Alphas(2)))
        ErrorsFound=.TRUE.
      END IF ! ptr == 0
    ELSE  ! no schedule specified
       WalkIn( WalkInID)%SchedPtr = AlwaysOn
    END IF ! not blank

!   check availability schedule for values between 0 and 1
    IF ( WalkIn( WalkInID)%SchedPtr > 0)THEN
      IF (.NOT. CheckScheduleValueMinMax( WalkIn( WalkInID)%SchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//'"')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(Alphas(2)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.TRUE.
      END IF
    END IF

    WalkIn(WalkInID)%DesignRatedCap      = Numbers(1)
    IF(Numbers(1) <= 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(1))//' must be greater than 0 W')
      ErrorsFound = .TRUE.
    END IF

    IF (.NOT. lNumericBlanks(2))THEN
     WalkIn( WalkInID)%Temperature     = Numbers(2)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(2))//' must be input ')
      ErrorsFound=.TRUE.
    END IF

    IF (.NOT. lNumericBlanks(3))THEN
     WalkIn( WalkInID)%TEvapDesign     = Numbers(3)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(3))//' must be input')
      ErrorsFound=.TRUE.
    END IF

    IF (.NOT. lNumericBlanks(4))THEN
     WalkIn( WalkInID)%HeaterPower     = Numbers(4)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(4))//' must be input ')
      ErrorsFound=.TRUE.
    END IF

    AlphaNum=3
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      WalkIn( WalkInID)%HeaterSchedPtr = GetScheduleIndex(Alphas(AlphaNum))  ! convert heater schedule name to pointer
      IF ( WalkIn( WalkInID)%HeaterSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                             '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      END IF ! ptr == 0
    ELSE  ! no schedule specified
      WalkIn( WalkInID)%HeaterSchedPtr = AlwaysOn
    END IF ! not blank

!   check heater schedule for values between 0 and 1
    IF ( WalkIn( WalkInID)%HeaterSchedPtr > 0)THEN
      IF (.NOT. CheckScheduleValueMinMax( WalkIn( WalkInID)%HeaterSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//'"')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//' = '//TRIM(Alphas(AlphaNum)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.TRUE.
      END IF
    END IF

    IF (.NOT. lNumericBlanks(5) .AND. Numbers(5) > 0.d0)THEN
      WalkIn( WalkInID)%CoilFanPower     = Numbers(5)
    ELSE
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
             '", '//TRIM(cNumericFieldNames(5))//' was not input or was less than 0 and default of 375.0 W will be used ')
      WalkIn( WalkInID)%CoilFanPower     = 375.d0 !default value = 1/2 hp
    END IF

    IF (lNumericBlanks(6))THEN
      WalkIn( WalkInID)%CircFanPower  = 0.0d0
    ELSE
      WalkIn( WalkInID)%CircFanPower  = Numbers(6)
      IF(Numbers(7) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(6))//' must be greater than >= 0 W')
        ErrorsFound = .TRUE.
      END IF
    END IF

    IF (.NOT. lNumericBlanks(7))THEN
     WalkIn( WalkInID)%DesignLighting     = Numbers(7)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '" '//TRIM(cNumericFieldNames(7))//' must be input ')
      ErrorsFound=.TRUE.
    END IF

    AlphaNum=4
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      WalkIn( WalkInID)%LightingSchedPtr = GetScheduleIndex(Alphas(AlphaNum))  ! convert lighting schedule name to pointer
      IF ( WalkIn( WalkInID)%LightingSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      END IF ! ptr == 0
    ELSE  ! no schedule specified
      WalkIn( WalkInID)%LightingSchedPtr = AlwaysOn
    END IF ! schedule name not blank
!   check Lighting schedule for values between 0 and 1
    IF ( WalkIn( WalkInID)%LightingSchedPtr > 0)THEN
      IF (.NOT. CheckScheduleValueMinMax( WalkIn( WalkInID)%LightingSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//'"')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//' = '//TRIM(Alphas(AlphaNum)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.TRUE.
      END IF
    END IF

!Input walk-in cooler defrost information
    AlphaNum=5
    IF(lAlphaBlanks(AlphaNum)) THEN
      WalkIn( WalkInID)%DefrostType = WalkInDefrostElec
    ELSEIF (SameString(Alphas(AlphaNum),'Electric')) THEN
       WalkIn( WalkInID)%DefrostType = WalkInDefrostElec
    ELSEIF (SameString(Alphas(AlphaNum),'HotFluid')) THEN
       WalkIn( WalkInID)%DefrostType = WalkInDefrostFluid
    ELSEIF (SameString(Alphas(AlphaNum),'None')) THEN
       WalkIn( WalkInID)%DefrostType = WalkInDefrostNone
    ELSEIF (SameString(Alphas(AlphaNum),'OffCycle')) THEN
       WalkIn( WalkInID)%DefrostType = WalkInDefrostOffCycle
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//'="'//TRIM(Alphas(AlphaNum))//'".')
      ErrorsFound=.TRUE.
    END IF

    AlphaNum=6
    IF(lAlphaBlanks(AlphaNum)) THEN
        WalkIn( WalkInID)%DefrostControlType = DefrostControlSched
    ELSEIF (SameString(Alphas(AlphaNum),'TimeSchedule'))THEN
        WalkIn( WalkInID)%DefrostControlType = DefrostControlSched
    ELSEIF (SameString(Alphas(AlphaNum),'TemperatureTermination')) THEN
       WalkIn( WalkInID)%DefrostControlType = DefrostContTempTerm
    ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
    END IF ! defrost control type

    ! convert defrost schedule name to pointer
    AlphaNum=7
    WalkIn( WalkInID)%DefrostSchedPtr = GetScheduleIndex(Alphas(AlphaNum))
    IF ( WalkIn( WalkInID)%DefrostSchedPtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
      ErrorsFound=.TRUE.
    END IF
!   check defrost schedule for values between 0 and 1
    IF ( WalkIn( WalkInID)%DefrostSchedPtr > 0)THEN
      IF (.NOT. CheckScheduleValueMinMax( WalkIn( WalkInID)%DefrostSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = "'//TRIM( WalkIn( WalkInID)%Name)//'"')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//'='//TRIM(Alphas(AlphaNum)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.TRUE.
      END IF
    END IF

    ! convert defrost drip-down schedule name to pointer
    ! some defrost types do not use drip-down schedules, use same defrost schedule pointer in that case
    AlphaNum=8
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      WalkIn( WalkInID)%DefrostDripDownSchedPtr = GetScheduleIndex(Alphas(AlphaNum))
      IF ( WalkIn( WalkInID)%DefrostDripDownSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WalkIn( WalkInID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      END IF
      ! check schedule for values between 0 and 1
      IF ( WalkIn( WalkInID)%DefrostDripDownSchedPtr > 0)THEN
        IF (.NOT. CheckScheduleValueMinMax( WalkIn( WalkInID)%DefrostDripDownSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//'"')
          CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//' = '//TRIM(Alphas(AlphaNum)))
          CALL ShowContinueError('schedule values must be (>=0., <=1.)')
          ErrorsFound=.TRUE.
        END IF
      END IF
    ELSE  !blank input so use drip down schedule for defrost
      WalkIn( WalkInID)%DefrostDripDownSchedPtr = WalkIn( WalkInID)%DefrostSchedPtr
    END IF

    IF (WalkIn( WalkInID)%DefrostType == WalkInDefrostOffCycle .OR. &
        WalkIn( WalkInID)%DefrostType == WalkInDefrostNone) THEN
        WalkIn( WalkInID)%DefrostCapacity = 0.d0
        !Don't even need to read N8 or N9 for those two defrost types.
    ELSE !have electric or hot gas/brine defrost
       IF ((lNumericBlanks(8)) .OR. (Numbers(8) <= 0.0d0))THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(8))//' must be input and greater than or equal to 0 W'//' for '//&
                           TRIM(cAlphaFieldNames(5))//' '//TRIM(Alphas(5)))
           ErrorsFound = .TRUE.
       ELSE
         WalkIn( WalkInID)%DefrostCapacity = Numbers(8)
       END IF !Blank  or negative N8

       !defaults for defrost energy fraction are 0.7 for elec defrost and 0.3 for warm fluid
       !note this value is only used for temperature terminated defrost control type
       IF (WalkIn( WalkInID)%DefrostType == WalkInDefrostElec)  WalkIn(WalkInID)%DefEnergyFraction = 0.7d0
       IF (WalkIn( WalkInID)%DefrostType == WalkInDefrostFluid) WalkIn(WalkInID)%DefEnergyFraction = 0.3d0
       IF (.NOT. lNumericBlanks (9)) THEN
         IF ((Numbers(9) > 1.0d0) .OR. (Numbers(9) < 0.0d0))THEN
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WalkIn( WalkInID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(9))//' must be between 0 and 1, default values will be used.')
         ELSE
           WalkIn(WalkInID)%DefEnergyFraction = Numbers(9)
         END IF ! number out of range
       END IF !lnumericblanks
    END IF ! defrost type

     ! convert restocking schedule name to pointer, default of 0.1 is assigned inside walkin subroutine if blank
    AlphaNum=9
    IF (lAlphaBlanks(AlphaNum)) THEN
      WalkIn( WalkInID)%StockingSchedPtr = 0
    ELSE
      WalkIn( WalkInID)%StockingSchedPtr = GetScheduleIndex(Alphas(AlphaNum))
      IF ( WalkIn( WalkInID)%StockingSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WalkIn( WalkInID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      END IF
    END IF !blank

    WalkIn( WalkInID)%DesignRefrigInventory = 0.0d0
    IF (.NOT. lNumericBlanks(10))  WalkIn( WalkInID)%DesignRefrigInventory = Numbers(10)

    IF (.NOT. lNumericBlanks(11))THEN
      WalkIn(WalkInID)%FloorArea     = Numbers(11)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WalkIn( WalkInID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(11))//' must be input' )
      ErrorsFound=.TRUE.
    END IF

    IF (lNumericBlanks(12))THEN
      WalkIn( WalkInID)%FloorUValue = DefaultWISurfaceUValue
    ELSE
      WalkIn( WalkInID)%FloorUValue = Numbers(12)
      IF(Numbers(12) <= 0.d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(12))//' must be > 0.')
        ErrorsFound=.TRUE.
      END IF
    END IF

!Calculate the number of zones exposed to walk-in based on number of input fields, all integer math,
! This approach used because last zone could have less than NumWIFieldsPerZone due to optional values
TotFields = NumNumbers + NumAlphas
NumWIFieldsPerZone =   NumWIAlphaFieldsPerZone + NumWINumberFieldsPerZone
NumWIFieldsTotal = TotFields - NumWIAlphaFieldsBeforeZoneInput - NumWINumberfieldsBeforeZoneInput
NumZones = 1
IF (NumWIFieldsTotal > NumWIFieldsPerZone) NumZones = 2
IF (NumWIFieldsTotal > (2 * NumWIFieldsPerZone)) NumZones = 3
IF (NumWIFieldsTotal > (3 * NumWIFieldsPerZone)) NumZones = 4
IF (NumWIFieldsTotal > (4 * NumWIFieldsPerZone)) NumZones = 5
IF (NumWIFieldsTotal > (5 * NumWIFieldsPerZone)) NumZones = 6
WalkIn(WalkInID)%NumZones = NumZones

! All variables for walk-in/zone interactions need to be allocated after know number of zones
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%ZoneName))ALLOCATE(WalkIn( WalkInID)%ZoneName(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%ZoneNum))ALLOCATE(WalkIn( WalkInID)%ZoneNum(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%ZoneNodeNum))ALLOCATE(WalkIn( WalkInID)%ZoneNodeNum(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%SurfaceArea))ALLOCATE(WalkIn( WalkInID)%SurfaceArea(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%UValue))ALLOCATE(WalkIn( WalkInID)%UValue(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%UValueGlassDr))ALLOCATE(WalkIn( WalkInID)%UValueGlassDr(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%GlassDoorOpenSchedPtr))ALLOCATE(WalkIn( WalkInID)%GlassDoorOpenSchedPtr(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%AreaGlassDr))ALLOCATE(WalkIn( WalkInID)%AreaGlassDr(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%HeightGlassDr))ALLOCATE(WalkIn( WalkInID)%HeightGlassDr(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%UValueStockDr))ALLOCATE(WalkIn( WalkInID)%UValueStockDr(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%StockDoorOpenSchedPtr))ALLOCATE(WalkIn( WalkInID)%StockDoorOpenSchedPtr(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%StockDoorProtectType))ALLOCATE(WalkIn( WalkInID)%StockDoorProtectType(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%AreaStockDr))ALLOCATE(WalkIn( WalkInID)%AreaStockDr(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%HeightStockDr))ALLOCATE(WalkIn( WalkInID)%HeightStockDr(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%SensZoneCreditRate))ALLOCATE(WalkIn( WalkInID)%SensZoneCreditRate(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%SensZoneCreditCoolRate))ALLOCATE(WalkIn( WalkInID)%SensZoneCreditCoolRate(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%SensZoneCreditCool))ALLOCATE(WalkIn( WalkInID)%SensZoneCreditCool(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%SensZoneCreditHeatRate))ALLOCATE(WalkIn( WalkInID)%SensZoneCreditHeatRate(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%SensZoneCreditHeat))ALLOCATE(WalkIn( WalkInID)%SensZoneCreditHeat(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%LatZoneCreditRate))ALLOCATE(WalkIn( WalkInID)%LatZoneCreditRate(NumZones))
IF(.NOT. ALLOCATED(WalkIn( WalkInID)%LatZoneCredit))ALLOCATE(WalkIn( WalkInID)%LatZoneCredit(NumZones))

AStart = NumWIAlphaFieldsBeforeZoneInput +1
NStart = NumWINumberFieldsBeforeZoneInput +1
    DO ZoneID = 1,NumZones
      !Get the Zone node number from the zone name
      !The Zone Node is needed to get the zone's ambient conditions, NumOfZones from dataglobals
      WalkIn( WalkInID)%ZoneName(ZoneID)= Alphas(AStart)
      WalkIn( WalkInID)%ZoneNum(ZoneID) = FindItemInList(Alphas(AStart),Zone%Name,NumOfZones)

      IF ( WalkIn( WalkInID)%ZoneNum(ZoneID) == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//  &
                           '", invalid  '//TRIM(cAlphaFieldNames(AStart))//' not valid: '//TRIM(Alphas(AStart)))
        ErrorsFound=.TRUE.
      ELSE
        RefrigPresentInZone(WalkIn( WalkInID)%ZoneNum(ZoneID)) = .TRUE.
      ENDIF
      WalkIn( WalkInID)%ZoneNodeNum(ZoneID)   = &
               GetSystemNodeNumberForZone( WalkIn( WalkInID)%ZoneName(ZoneID))
      IF ( WalkIn( WalkInID)%ZoneNum(ZoneID) >= 0) THEN
          IF ( WalkIn( WalkInID)%ZoneNodeNum(ZoneID) == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//  &
                             '" System Node Number not found for '//TRIM(cAlphaFieldNames(AStart))// &
                             ' = '//TRIM(Alphas(AStart)))
            CALL ShowContinueError('.. Walk Ins must reference a controlled Zone (appear'//  &
               ' in a ZoneHVAC:EquipmentConnections object.')
            ErrorsFound=.TRUE.
          ENDIF
      ENDIF

    IF (.NOT. lNumericBlanks(NStart))THEN
      WalkIn( WalkInID)%SurfaceArea(ZoneID)     = Numbers(NStart)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(NStart))//' must be input for Zone: ' &
                        //TRIM( WalkIn( WalkInID)%ZoneName(ZoneID)))
      ErrorsFound=.TRUE.
    END IF

    IF (lNumericBlanks(NStart+1))THEN
      WalkIn( WalkInID)%UValue(ZoneID) = DefaultWISurfaceUValue
    ELSE
      WalkIn( WalkInID)%UValue(ZoneID) = Numbers(NStart+1)
      IF(Numbers(Nstart + 1) <= 0.d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '", Zone="'//TRIM( WalkIn( WalkInID)%ZoneName(ZoneID))//'", '//&
                        TRIM(cNumericFieldNames(NStart+1))//' must be > 0.')
        ErrorsFound=.TRUE.
      END IF
    END IF

    !start IF set for glass doors in this zone
    WalkIn( WalkInID)%AreaGlassDr(ZoneID) = 0.0d0
    WalkIn( WalkInID)%HeightGlassDr(ZoneID) = 0.0d0
    WalkIn( WalkInID)%UValueGlassDr(ZoneID) = 0.0d0
    IF (.NOT. lNumericBlanks(NStart+2))THEN
      WalkIn( WalkInID)%AreaGlassDr(ZoneID) = Numbers(NStart+2)

      WalkIn( WalkInID)%HeightGlassDr(ZoneID) = DefaultWIHeightGlassDr
      IF (.NOT. lNumericBlanks(NStart+3)) WalkIn( WalkInID)%HeightGlassDr(ZoneID) = Numbers(NStart+3)

      WalkIn( WalkInID)%UValueGlassDr(ZoneID) = DefaultWIUValueGlassDr
      IF (.NOT. lNumericBlanks(NStart+4))WalkIn( WalkInID)%UValueGlassDr(ZoneID) = Numbers(NStart+4)

     ! convert door opening schedule name to pointer, default of 0.1 is assigned inside walkin subroutine if blank
      IF (lAlphaBlanks(AStart + 1)) THEN
        WalkIn( WalkInID)%GlassDoorOpenSchedPtr(ZoneID) = 0
      ELSE
        WalkIn( WalkInID)%GlassDoorOpenSchedPtr(ZoneID) = GetScheduleIndex(Alphas(AStart + 1))
        IF ( WalkIn( WalkInID)%GlassDoorOpenSchedPtr(ZoneID) == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WalkIn( WalkInID)%Name)//&
                           '", Zone="'//TRIM( WalkIn( WalkInID)%ZoneName(ZoneID))// &
                        '", invalid  '//TRIM(cAlphaFieldNames(AStart + 1))//' not found: '//TRIM(Alphas(AStart + 1)))
          ErrorsFound=.TRUE.
        ELSE
!       check schedule for values between 0 and 1
          IF (.NOT. CheckScheduleValueMinMax( WalkIn( WalkInID)%GlassDoorOpenSchedPtr(ZoneID),'>=',0.0d0,'<=',1.0d0)) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WalkIn( WalkInID)%Name)//&
                        '", Zone="'//TRIM( WalkIn( WalkInID)%ZoneName(ZoneID))//'"')
            CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AStart + 1))// &
                              ' = '//TRIM(Alphas(AStart + 1)))
            CALL ShowContinueError('schedule values must be (>=0., <=1.)')
            ErrorsFound=.TRUE.
          END IF !schedule values outside range
        END IF ! have schedule pointer
       END IF !blank on door opening schedule (AStart + 1)
    END IF  ! have glassdoor area facing zone (blank on lNumericBlanks(NStart+2))

   !start IF set for stock doors in this zone
   WalkIn( WalkInID)%AreaStockDr(ZoneID) = 0.0d0
   WalkIn( WalkInID)%HeightStockDr(ZoneID) = 0.0d0
   WalkIn( WalkInID)%UValueStockDr(ZoneID) = 0.0d0
   IF (.NOT. lNumericBlanks(NStart+5))THEN
      WalkIn( WalkInID)%AreaStockDr(ZoneID) = Numbers(NStart+5)

      WalkIn( WalkInID)%HeightStockDr(ZoneID) = DefaultWIHeightStockDr
      IF (.NOT. lNumericBlanks(NStart+6)) WalkIn( WalkInID)%HeightStockDr(ZoneID) = Numbers(NStart+6)

      WalkIn( WalkInID)%UValueStockDr(ZoneID) = DefaultWIUValueStockDr
      IF (.NOT. lNumericBlanks(NStart+7)) WalkIn( WalkInID)%UValueStockDr(ZoneID) = Numbers(NStart+7)

      ! convert door opening schedule name to pointer, default of 0.1 is assigned inside walkin subroutine if blank
      IF (lAlphaBlanks(AStart + 2)) THEN
        WalkIn( WalkInID)%StockDoorOpenSchedPtr(ZoneID) = 0
      ELSE
        WalkIn( WalkInID)%StockDoorOpenSchedPtr(ZoneID) = GetScheduleIndex(Alphas(AStart + 2))
        IF ( WalkIn( WalkInID)%StockDoorOpenSchedPtr(ZoneID) == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '", Zone="'//TRIM( WalkIn( WalkInID)%ZoneName(ZoneID))// &
                        '", invalid  '//TRIM(cAlphaFieldNames(AStart + 2))//' not found: '//TRIM(Alphas(AStart + 2)))
          ErrorsFound=.TRUE.
        ELSE
!       check schedule for values between 0 and 1
          IF (.NOT. CheckScheduleValueMinMax( WalkIn( WalkInID)%StockDoorOpenSchedPtr(ZoneID),'>=',0.0d0,'<=',1.0d0)) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                        '", Zone="'//TRIM( WalkIn( WalkInID)%ZoneName(ZoneID))//'"')
            CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AStart + 2))// &
                              ' = '//TRIM(Alphas(AStart + 2)))
            CALL ShowContinueError('schedule values must be (>=0., <=1.)')
            ErrorsFound=.TRUE.
          END IF !schedule values outside range
        END IF ! have schedule pointer
      END IF !blank on door opening schedule (AStart + 2)

      IF(lAlphaBlanks(AStart + 3)) THEN
         !default air curtain
         WalkIn( WalkInID)%StockDoorProtectType(ZoneID) = WIStockDoorAirCurtain
      ELSEIF (SameString(Alphas(AStart + 3),'None')) THEN
         WalkIn( WalkInID)%StockDoorProtectType(ZoneID) = WIStockDoorNone
      ELSEIF (SameString(Alphas(AStart + 3),'AirCurtain')) THEN
         WalkIn( WalkInID)%StockDoorProtectType(ZoneID) = WIStockDoorAirCurtain
      ELSEIF (SameString(Alphas(AStart + 3),'StripCurtain')) THEN
         WalkIn( WalkInID)%StockDoorProtectType(ZoneID) = WIStockDoorStripCurtain
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WalkIn( WalkInID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AStart + 3))//'="'//TRIM(Alphas(AStart + 3))//'".')
        ErrorsFound=.TRUE.
      END IF !stock door protection (AStart + 3) blank
    END IF  ! have Stockdoor area facing zone

  AStart = AStart + NumWIAlphaFieldsPerZone
  NStart = NStart + NumWINumberFieldsPerZone
 END DO  !Zones for Walk Ins
 END DO  !Individual Walk Ins
 END IF  !(NumSimulationWalkIns > 0 )

!************* Start Indiv Refrig Air Chillers

 IF(NumSimulationRefrigAirChillers > 0 ) THEN
  CurrentModuleObject='Refrigeration:AirChiller'
  DO  CoilID=1,NumSimulationRefrigAirChillers
!A1
    AlphaNum = 1
    CALL GetObjectItem(CurrentModuleObject, CoilID,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(AlphaNum), WarehouseCoil%Name, CoilID-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')

    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined name="'//&
                           TRIM(Alphas(AlphaNum))//'".')
      IF (IsBlank) Alphas(AlphaNum)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    WarehouseCoil(CoilID)%Name          = Alphas(AlphaNum)

!A2
    AlphaNum = AlphaNum + 1
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      WarehouseCoil(CoilID)%SchedPtr = GetScheduleIndex(Alphas(AlphaNum))  ! convert schedule name to pointer
      IF ( WarehouseCoil(CoilID)%SchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                             '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      END IF ! ptr == 0
    ELSE  ! no schedule specified
       WarehouseCoil(CoilID)%SchedPtr = AlwaysOn
    END IF ! not blank

!   check availability schedule for values between 0 and 1
    IF ( WarehouseCoil(CoilID)%SchedPtr > 0)THEN
      IF (.NOT. CheckScheduleValueMinMax( WarehouseCoil(CoilID)%SchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//'"')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//' = '//TRIM(Alphas(AlphaNum)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.TRUE.
      END IF
    END IF

    !Input capacity rating type
    !bbbbb input values (DT1 or DTM type)translate DT1 to DTm here because node will give avg temp?
    !      ask whether ceiling or floor mounted? - effects translation from DT1 to DTM
    !      input coil condition, wet or dry OR input rating basis, European SC1, SC2 etc., have to combine with refrigerant factor)
    !      rated capacity, BAC give W/C, European gives W
    !      fin material factor, default 1
    !      refrigerant factor (factor of both refrigerant and Tevap)

!A3
    AlphaNum = AlphaNum + 1
    IF (lAlphaBlanks(AlphaNum)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                             TRIM(cAlphaFieldNames(AlphaNum))//' is required and not found.')
        ErrorsFound=.TRUE.
    ELSEIF (SameString(Alphas(AlphaNum),'UnitLoadFactorSensibleOnly')) THEN
       WarehouseCoil(CoilID)%RatingType = UnitLoadFactorSens
    ELSEIF (SameString(Alphas(AlphaNum),'CapacityTotalSpecificConditions')) THEN
       WarehouseCoil(CoilID)%RatingType = RatedCapacityTotal
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC1Standard')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC1Std
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC1NominalWet')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC1Nom
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC2Standard')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC2Std
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC2NominalWet')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC2Nom
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC3Standard')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC3Std
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC3NominalWet')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC3Nom
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC4Standard')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC4Std
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC4NominalWet')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC4Nom
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC5Standard')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC5Std
    ELSEIF (SameString(Alphas(AlphaNum),'EuropeanSC5NominalWet')) THEN
       WarehouseCoil(CoilID)%RatingType = EuropeanSC5Nom
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//'="'//TRIM(Alphas(AlphaNum))//'".')
      ErrorsFound=.TRUE.
    END IF

   !Here have to do select case with one numeric field with units of W and the second with units of W/deltaC,
   !  (RatedRH field only used for RatedCapacityTotal type)
   SELECTCASE (WarehouseCoil(CoilID)%RatingType)
     CASE (UnitLoadFactorSens)
!N1
       NumNum = 1
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%UnitLoadFactorSens = Numbers(NumNum)
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W/C')
         ErrorsFound = .TRUE.
       END IF
     CASE (RatedCapacityTotal)
!N2
       NumNum = 2 !advance past rating in W/C to N2 with rating in W
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedCapTotal = Numbers(NumNum)
!N3
         NumNum = 3 !read rated RH only for this type of rating at N3
         IF(lNumericBlanks(NumNum)) THEN
          WarehouseCoil(CoilID)%RatedRH = 0.85d0
         ELSE
           IF(Numbers(NumNum) <= 0.0d0 .OR. Numbers(NumNum) >= 100.0d0) THEN
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be greater than 0% and less than 100%')
             ErrorsFound = .TRUE.
           END IF
           WarehouseCoil(CoilID)%RatedRH = Numbers(NumNum)/100.d0
         END IF
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC1Std)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)
         WarehouseCoil(CoilID)%SCIndex = 1
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC1Nom)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedCapTotal = Numbers(NumNum)
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)/EuropeanWetCoilFactor(1)
         WarehouseCoil(CoilID)%SCIndex = 1
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC2Std)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)
         WarehouseCoil(CoilID)%SCIndex = 2
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC2Nom)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedCapTotal = Numbers(NumNum)
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)/EuropeanWetCoilFactor(2)
         WarehouseCoil(CoilID)%SCIndex = 2
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC3Std)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)
         WarehouseCoil(CoilID)%SCIndex = 3
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC3Nom)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedCapTotal = Numbers(NumNum)
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)/EuropeanWetCoilFactor(3)
         WarehouseCoil(CoilID)%SCIndex = 3
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC4Std)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)
         WarehouseCoil(CoilID)%SCIndex = 4
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC4Nom)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedCapTotal = Numbers(NumNum)
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)/EuropeanWetCoilFactor(4)
         WarehouseCoil(CoilID)%SCIndex = 4
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC5Std)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)
         WarehouseCoil(CoilID)%SCIndex = 5
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
     CASE (EuropeanSC5Nom)
!N2
       NumNum = 2 !advance past rating in W/C to rating in W at N2
       IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.0d0) THEN
         WarehouseCoil(CoilID)%RatedCapTotal = Numbers(NumNum)
         WarehouseCoil(CoilID)%RatedSensibleCap = Numbers(NumNum)/EuropeanWetCoilFactor(5)
         WarehouseCoil(CoilID)%SCIndex = 5
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and be greater than 0 W')
         ErrorsFound = .TRUE.
       END IF
   END SELECT !WarehouseCoil(CoilID)%RatingType

!N4
    NumNum = 4
    IF (.NOT. lNumericBlanks(NumNum))THEN
     WarehouseCoil(CoilID)%TEvapDesign    = Numbers(NumNum) !also used to rep inlet brine T later when add that option
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(NumNum))//' must be input')
      ErrorsFound=.TRUE.
    END IF

    NumNum = NumNum + 1  !N5
    IF (.NOT. lNumericBlanks(NumNum))THEN
     WarehouseCoil(CoilID)%RatedTemperatureDif = Numbers(NumNum)
     ! INLET temperature - evaporating temperature, NOT room temp - evap temp
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(NumNum))//' must be input')
      ErrorsFound=.TRUE.
    END IF

    NumNum = NumNum + 1  !N6
    IF (.NOT. lNumericBlanks(NumNum))THEN
     WarehouseCoil(CoilID)%MaxTemperatureDif = Numbers(NumNum)
     ! Maximum difference between INLET temperature - evaporating temperature, NOT room temp - evap temp
     ! Important when cooling down space at start of environment or if large stocking loads imposed.
    ELSE
      WarehouseCoil(CoilID)%MaxTemperatureDif = 1.3d0 * WarehouseCoil(CoilID)%RatedTemperatureDif
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
            '", '//TRIM(cNumericFieldNames(NumNum))//' not entered, default 1.3 times rated temperature difference will be used.')
    END IF

    ! Correction factor from manufacturer's rating for coil material, default 1.0
    NumNum = NumNum + 1 !N7
    WarehouseCoil(CoilID)%CorrMaterial    = 1.d0 !default value
     IF (.NOT. lNumericBlanks(NumNum)) WarehouseCoil(CoilID)%CorrMaterial    = Numbers(NumNum)

    ! Correction factor from manufacturer's rating for refrigerant, default 1.0
    NumNum = NumNum + 1 !N8
    WarehouseCoil(CoilID)%CorrRefrigerant    = 1.0d0 !default value
     IF (.NOT. lNumericBlanks(NumNum)) WarehouseCoil(CoilID)%CorrRefrigerant    = Numbers(NumNum)
     !ONLY used if the Capacity Rating Type is CapacityTotalSpecificConditions

    !Convert all European sensible capacities to sensible load factors
    IF((WarehouseCoil(CoilID)%RatingType /= UnitLoadFactorSens) .AND. &
       (WarehouseCoil(CoilID)%RatingType /= RatedCapacityTotal)) &
        WarehouseCoil(CoilID)%UnitLoadFactorSens = WarehouseCoil(CoilID)%RatedSensibleCap/ &
                                                   WarehouseCoil(CoilID)%RatedTemperatureDif
       !Now have UnitLoadFactorSens for all except RatingType == RatedCapacityTotal

    !Apply material and refrigerant correction factors to sensible load factors
    IF((WarehouseCoil(CoilID)%RatingType /= RatedCapacityTotal)) &
          WarehouseCoil(CoilID)%UnitLoadFactorSens = WarehouseCoil(CoilID)%UnitLoadFactorSens * &
                                                     WarehouseCoil(CoilID)%CorrMaterial * &
                                                     WarehouseCoil(CoilID)%CorrRefrigerant
          !First calc of ratedsensiblecap for type type unitloadfactorsens
          WarehouseCoil(CoilID)%RatedSensibleCap   = WarehouseCoil(CoilID)%UnitLoadFactorSens *  &
                                                     WarehouseCoil(CoilID)%RatedTemperatureDif
!A4    Enter capacity correction curve type
    AlphaNum = 4
    IF((lAlphaBlanks(AlphaNum)) .AND. (WarehouseCoil(CoilID)%RatingType /= RatedCapacityTotal)) THEN
      ! For all except RatedCapacityTotal - default to linear capacity factor approximating Nelson August 2010 ASHRAE journal
      WarehouseCoil(CoilID)%SHRCorrectionType = SHR60
    ELSEIF (WarehouseCoil(CoilID)%RatingType == RatedCapacityTotal) THEN
      ! For RatedCapacityTotal, the manufacturer's coil performance map is required
      ! Specify the performance map with TabularRHxDT1xTRoom
      WarehouseCoil(CoilID)%SHRCorrectionType = TabularRH_DT1_TRoom
      IF (.NOT.(SameString(Alphas(AlphaNum),'TabularRHxDT1xTRoom'))) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
              '", invalid '//TRIM(cAlphaFieldNames(AlphaNum))//'="'//TRIM(Alphas(AlphaNum))//'".')
        CALL ShowContinueError('The "CapacityTotalSpecificConditions" Capacity Rating Type has been specified '//&
              'for this air chiller.  This rating type requires ')
        CALL ShowContinueError('the "TabularRHxDT1xTRoom" correction curve.  Verify that a valid '//&
              '"TabularRHxDT1xTRoom" curve is specified in "'//TRIM(cAlphaFieldNames(AlphaNum+1))//'".')
      ENDIF
    ELSEIF (SameString(Alphas(AlphaNum),'LinearSHR60')) THEN
       WarehouseCoil(CoilID)%SHRCorrectionType = SHR60
    ELSEIF (SameString(Alphas(AlphaNum),'QuadraticSHR')) THEN
       WarehouseCoil(CoilID)%SHRCorrectionType = QuadraticSHR
    ELSEIF (SameString(Alphas(AlphaNum),'European')) THEN
       WarehouseCoil(CoilID)%SHRCorrectionType = European
    ELSEIF (SameString(Alphas(AlphaNum),'TabularRHxDT1xTRoom')) THEN
       WarehouseCoil(CoilID)%SHRCorrectionType = TabularRH_DT1_TRoom
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//'="'//TRIM(Alphas(AlphaNum))//'".')
      ErrorsFound=.TRUE.
    END IF

    AlphaNum = AlphaNum + 1 !A5
    NumNum = NumNum + 1     !N9
    SELECT CASE (WarehouseCoil(CoilID)%SHRCorrectionType)
      CASE (SHR60)
        WarehouseCoil(CoilID)%SHRCorrection60 = 1.48d0 ! reference Nelson, ASHRAE journal August 2010 Fig 2
        IF (.NOT. lNumericBlanks(NumNum)) WarehouseCoil(CoilID)%SHRCorrection60 = Numbers(NumNum)
        !(1.66667 would be a perfect effectiveness, 1.0 would be artificial coil that does only sensible)
        IF (WarehouseCoil(CoilID)%SHRCorrection60 > 1.67d0)THEN
          WarehouseCoil(CoilID)%SHRCorrection60 = 1.67d0
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(NumNum))//' must be between 1 and 1.67, 1.67 will be used.')
        END IF
        IF (WarehouseCoil(CoilID)%SHRCorrection60 < 1.d0) THEN
          WarehouseCoil(CoilID)%SHRCorrection60 = 1.0d0
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(NumNum))//' must be between 1 and 1.67, 1.00 will be used.')
        END IF
      CASE (European)
        !WarehouseCoil(CoilID)%SHRCorrectionCurvePtr = GetCurveIndex('ChillerEuropeanWetCoilFactor')
        ! This is a place holder, currently use embedded constants for European ratings, future may want a curve
      CASE (QuadraticSHR)
        WarehouseCoil(CoilID)%SHRCorrectionCurvePtr = GetCurveIndex(Alphas(AlphaNum)) ! convert curve name to number
        IF (lAlphaBlanks(AlphaNum)) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' is blank, required.')
          ErrorsFound=.true.
        ELSEIF (WarehouseCoil(CoilID)%SHRCorrectionCurvePtr == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                           '", invalid  ')
          CALL ShowContinueError('...invalid curve '//TRIM(cAlphaFieldNames(AlphaNum))//'="'//trim(Alphas(AlphaNum))//'".')
          ErrorsFound=.true.
        ENDIF
        !error checks for curve type entered and curve name
        IF(.NOT. SameString(GetCurveType(WarehouseCoil(CoilID)%SHRCorrectionCurvePtr),'QUADRATIC')) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' must be of type Quadratic.')
          ErrorsFound = .TRUE.
        END IF
      CASE (TabularRH_DT1_TRoom)
        WarehouseCoil(CoilID)%SHRCorrectionCurvePtr = GetCurveIndex(Alphas(AlphaNum)) ! convert curve name to number
        IF (lAlphaBlanks(AlphaNum)) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' is blank, required.')
          ErrorsFound=.true.
        ELSEIF (WarehouseCoil(CoilID)%SHRCorrectionCurvePtr == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                           '", invalid  ')
          CALL ShowContinueError('...invalid curve '//TRIM(cAlphaFieldNames(AlphaNum))//'="'//trim(Alphas(AlphaNum))//'".')
          ErrorsFound=.true.
        ENDIF
!        IF(WarehouseCoil(CoilID)%SHRCorrectionCurvePtr == 0) THEN
!          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
!                           '", not found  '//TRIM(cAlphaFieldNames(AlphaNum)))
!          ErrorsFound = .TRUE.
!        END IF !valid table name
    END SELECT  !SHRCorrectionType

    NumNum = NumNum + 1 !N10
    IF (.NOT. lNumericBlanks(NumNum))THEN
     WarehouseCoil(CoilID)%HeaterPower     = Numbers(NumNum)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(NumNum))//' must be input ')
      ErrorsFound=.TRUE.
    END IF

    AlphaNum = AlphaNum + 1 !A6
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      WarehouseCoil(CoilID)%HeaterSchedPtr = GetScheduleIndex(Alphas(AlphaNum))  ! convert heater schedule name to pointer
      IF ( WarehouseCoil(CoilID)%HeaterSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                             '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      ELSE !   check heater schedule for values between 0 and 1
        IF (.NOT. CheckScheduleValueMinMax( WarehouseCoil(CoilID)%HeaterSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//'"')
          CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//' = '//TRIM(Alphas(AlphaNum)))
          CALL ShowContinueError('schedule values must be (>=0., <=1.)')
          ErrorsFound=.TRUE.
        END IF !heater schedule ptr == 0
      END IF !htr sched == 0
    ELSE  ! lalphaBlanks, no schedule specified
      WarehouseCoil(CoilID)%HeaterSchedPtr = AlwaysOn
    END IF ! not blank

 !Input fan control type
    AlphaNum = AlphaNum + 1 !A7
    IF(lAlphaBlanks(AlphaNum)) THEN
      WarehouseCoil(CoilID)%FanType = FanConstantSpeed
    ELSEIF (SameString(Alphas(AlphaNum),'Fixed')) THEN
       WarehouseCoil(CoilID)%FanType = FanConstantSpeed
    ELSEIF (SameString(Alphas(AlphaNum),'FixedLinear')) THEN
       WarehouseCoil(CoilID)%FanType = FanConstantSpeedLinear
    ELSEIF (SameString(Alphas(AlphaNum),'VariableSpeed')) THEN
       WarehouseCoil(CoilID)%FanType = FanVariableSpeed
    ELSEIF (SameString(Alphas(AlphaNum),'TwoSpeed')) THEN
       WarehouseCoil(CoilID)%FanType = FanTwoSpeed
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//'="'//TRIM(Alphas(AlphaNum))//'".')
      ErrorsFound=.TRUE.
    END IF !fan control type

    NumNum = NumNum + 1  !N11
    IF (.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.d0)THEN
      WarehouseCoil(CoilID)%RatedFanPower     = Numbers(NumNum)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
             '", '//TRIM(cNumericFieldNames(NumNum))//' was not input or was less than 0 ')
      ErrorsFound=.TRUE.
    END IF !coil fan power

    NumNum = NumNum + 1 !N12
    IF (.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.d0)THEN
      WarehouseCoil(CoilID)%RatedAirVolumeFlow     = Numbers(NumNum)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
             '", '//TRIM(cNumericFieldNames(NumNum))//' is required and was not input or was less than 0  ')
    END IF !air volume flow

    NumNum = NumNum + 1 !N13
    WarehouseCoil(CoilID)%FanMinAirFlowRatio  = 0.2d0   !default value
    IF(.NOT. lNumericBlanks(NumNum) .AND. Numbers(NumNum) > 0.d0) WarehouseCoil(CoilID)%FanMinAirFlowRatio  = Numbers(NumNum)

!Input defrost type
    AlphaNum = AlphaNum + 1 !A8
    IF(lAlphaBlanks(AlphaNum)) THEN
      WarehouseCoil(CoilID)%DefrostType = DefrostElec
    ELSEIF (SameString(Alphas(AlphaNum),'Electric')) THEN
       WarehouseCoil(CoilID)%DefrostType = DefrostElec
    ELSEIF (SameString(Alphas(AlphaNum),'HotFluid')) THEN
       WarehouseCoil(CoilID)%DefrostType = DefrostFluid
    ELSEIF (SameString(Alphas(AlphaNum),'None')) THEN
       WarehouseCoil(CoilID)%DefrostType = DefrostNone
    ELSEIF (SameString(Alphas(AlphaNum),'OffCycle')) THEN
       WarehouseCoil(CoilID)%DefrostType = DefrostOffCycle
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//'="'//TRIM(Alphas(AlphaNum))//'".')
      ErrorsFound=.TRUE.
    END IF !defrost type

    AlphaNum = AlphaNum + 1 !A9
    IF(lAlphaBlanks(AlphaNum)) THEN
        WarehouseCoil(CoilID)%DefrostControlType = DefrostControlSched
    ELSEIF (SameString(Alphas(AlphaNum),'TimeSchedule'))THEN
        WarehouseCoil(CoilID)%DefrostControlType = DefrostControlSched
    ELSEIF (SameString(Alphas(AlphaNum),'TemperatureTermination')) THEN
       WarehouseCoil(CoilID)%DefrostControlType = DefrostContTempTerm
    ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
    END IF ! defrost control type

    ! convert defrost schedule name to pointer
    AlphaNum = AlphaNum + 1 !A10
    WarehouseCoil(CoilID)%DefrostSchedPtr = GetScheduleIndex(Alphas(AlphaNum))
    IF ( WarehouseCoil(CoilID)%DefrostSchedPtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
      ErrorsFound=.TRUE.
    ELSE !   check defrost schedule for values between 0 and 1
      IF (.NOT. CheckScheduleValueMinMax( WarehouseCoil(CoilID)%DefrostSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = "'//TRIM( WarehouseCoil(CoilID)%Name)//'"')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//'='//TRIM(Alphas(AlphaNum)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.TRUE.
      END IF !checkschedulevalueMinMax
    END IF   !check for valid schedule name

    ! convert defrost drip-down schedule name to pointer
    ! some defrost types do not use drip-down schedules, use same defrost schedule pointer in that case
    AlphaNum = AlphaNum + 1 !A11
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      WarehouseCoil(CoilID)%DefrostDripDownSchedPtr = GetScheduleIndex(Alphas(AlphaNum))
      IF ( WarehouseCoil(CoilID)%DefrostDripDownSchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      ELSE   ! check schedule for values between 0 and 1
        IF (.NOT. CheckScheduleValueMinMax( WarehouseCoil(CoilID)%DefrostDripDownSchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//'"')
          CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//' = '//TRIM(Alphas(AlphaNum)))
          CALL ShowContinueError('schedule values must be (>=0., <=1.)')
          ErrorsFound=.TRUE.
        END IF  !Check schedule value between 0 and 1
      END IF    ! Check if drip down schedule name is valid
    ELSE  ! .not. lAlphaBlanks  so use drip down schedule for defrost
      WarehouseCoil(CoilID)%DefrostDripDownSchedPtr = WarehouseCoil(CoilID)%DefrostSchedPtr
    END IF  ! .not. lAlphaBlanks

    NumNum = NumNum + 1 !N14
    IF (WarehouseCoil(CoilID)%DefrostType == DefrostOffCycle .OR. &
        WarehouseCoil(CoilID)%DefrostType == DefrostNone) THEN
        WarehouseCoil(CoilID)%DefrostCapacity = 0.d0
        !Don't even need to read Defrost capacity for those two defrost types.
    ELSE !have electric or hot gas/brine defrost
       IF ((lNumericBlanks(NumNum)) .OR. (Numbers(NumNum) <= 0.0d0))THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(NumNum))//' must be input and greater than or equal to 0 W'//' for '//&
                           TRIM(cAlphaFieldNames(AlphaNum))//' '//TRIM(Alphas(AlphaNum)))
           ErrorsFound = .TRUE.
       ELSE
         WarehouseCoil(CoilID)%DefrostCapacity = Numbers(NumNum)
       END IF !Blank  or negative Defrost Capacity

       !defaults for defrost energy fraction are 0.7 for elec defrost and 0.3 for warm fluid
       !note this value is only used for temperature terminated defrost control type
       IF (WarehouseCoil(CoilID)%DefrostType == DefrostElec)  WarehouseCoil(CoilID)%DefEnergyFraction = 0.7d0
       IF (WarehouseCoil(CoilID)%DefrostType == DefrostFluid) WarehouseCoil(CoilID)%DefEnergyFraction = 0.3d0

       NumNum = NumNum + 1 !N15
       IF (.NOT. lNumericBlanks (NumNum)) THEN
         IF ((Numbers(NumNum) > 1.0d0) .OR. (Numbers(NumNum) < 0.0d0))THEN
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(WarehouseCoil(CoilID)%Name)//&
                        '", '//TRIM(cNumericFieldNames(NumNum))//' must be between 0 and 1, default values will be used.')
         ELSE
           WarehouseCoil(CoilID)%DefEnergyFraction = Numbers(NumNum)
         END IF ! number out of range
       END IF !lnumericblanks
    END IF ! defrost type

    AlphaNum = AlphaNum + 1 !A12
    IF(lAlphaBlanks(AlphaNum)) THEN
        WarehouseCoil(CoilID)%VerticalLocation = Middle !default position
    ELSEIF (SameString(Alphas(AlphaNum),'Ceiling'))THEN
        WarehouseCoil(CoilID)%VerticalLocation = Ceiling
    ELSEIF (SameString(Alphas(AlphaNum),'Middle')) THEN
       WarehouseCoil(CoilID)%VerticalLocation = Middle
    ELSEIF (SameString(Alphas(AlphaNum),'Floor')) THEN
       WarehouseCoil(CoilID)%VerticalLocation = Floor
    ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( WarehouseCoil(CoilID)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
    END IF ! Vertical location class


    NumNum = NumNum + 1 !N16
    WarehouseCoil(CoilID)%DesignRefrigInventory = 0.0d0
    IF (.NOT. lNumericBlanks(NumNum))  WarehouseCoil(CoilID)%DesignRefrigInventory = Numbers(NumNum)
  END DO !NumRefrigAirChillers
END IF !NumRefrigerationAirChillers > 0

!************ START Warehouse Coil SET INPUT **************
! One Set allowed per zone, but indiv coils within zone can be served by different compressor/condenser systems


 IF(NumRefrigChillerSets > 0 ) THEN

  ALLOCATE(CheckChillerSetName(NumRefrigChillerSets))
  CheckChillerSetName=.true.

  CurrentModuleObject='ZoneHVAC:RefrigerationChillerSet'
  DO  SetID=1,NumRefrigChillerSets
    CALL GetObjectItem(CurrentModuleObject, SetID,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    AlphaNum = 1
    CALL VerifyName(Alphas(AlphaNum), AirChillerSet%Name, SetID-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')

    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined name="'//&
                           TRIM(Alphas(AlphaNum))//'".')
      IF (IsBlank) Alphas(AlphaNum)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    AirChillerSet(SetID)%Name          = Alphas(AlphaNum)

    AlphaNum = 2
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      AirChillerSet(SetID)%SchedPtr = GetScheduleIndex(Alphas(AlphaNum))  ! convert schedule name to pointer
      IF ( AirChillerSet(SetID)%SchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( AirChillerSet(SetID)%Name)//&
                             '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      END IF ! ptr == 0
    ELSE  ! no schedule specified
       AirChillerSet(SetID)%SchedPtr = AlwaysOn
    END IF ! not blank

!   check availability schedule for values between 0 and 1
    IF ( AirChillerSet(SetID)%SchedPtr > 0)THEN
      IF (.NOT. CheckScheduleValueMinMax( AirChillerSet(SetID)%SchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( AirChillerSet(SetID)%Name)//'"')
        CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(AlphaNum))//' = '//TRIM(Alphas(AlphaNum)))
        CALL ShowContinueError('schedule values must be (>=0., <=1.)')
        ErrorsFound=.TRUE.
      END IF
    END IF

    AlphaNum = AlphaNum + 1
    AirChillerSet(SetID)%ZoneName= Alphas(AlphaNum)
    AirChillerSet(SetID)%ZoneNum = FindItemInList(Alphas(AlphaNum),Zone%Name,NumOfZones)

    IF ( AirChillerSet(SetID)%ZoneNum == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( AirChillerSet(SetID)%Name)//  &
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not valid: '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
    ENDIF
    AirChillerSet(SetID)%ZoneNodeNum = GetSystemNodeNumberForZone( AirChillerSet(SetID)%ZoneName)
    IF ( AirChillerSet(SetID)%ZoneNodeNum == 0) THEN
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( AirChillerSet(SetID)%Name)//  &
                             '" System Node Number not found for '//TRIM(cAlphaFieldNames(AlphaNum))// &
                             ' = '//TRIM(Alphas(AlphaNum)))
       CALL ShowContinueError('.. Refrigeration chillers must reference a controlled Zone (appear'//  &
               ' in a ZoneHVAC:EquipmentConnections object.')
       ErrorsFound=.TRUE.
     ENDIF
     RefrigPresentInZone(AirChillerSet(SetID)%ZoneNum) = .TRUE.

    AlphaNum = AlphaNum + 1
      IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
        CALL ShowMessage(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( AirChillerSet(SetID)%Name)//  &
                             '" '//TRIM(cAlphaFieldNames(AlphaNum))//' is not used. This is not an error. '// &
                             ' Energy is exchanged directly with the zone independent of any air system. ')
        ! Node identification reserved for future use.  Currently exchange energy directly with zone outside any air system
        !AirChillerSet(SetID)%NodeNumInlet = &
        !       GetOnlySingleNode(Alphas(AlphaNum),ErrorsFound,TRIM(CurrentModuleObject), &
        !                    AirChillerSet(SetID)%Name,NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      END IF

    AlphaNum = AlphaNum + 1
      IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
        CALL ShowMessage(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM( AirChillerSet(SetID)%Name)//  &
                             '" '//TRIM(cAlphaFieldNames(AlphaNum))//' is not used. This is not an error. '// &
                             ' Energy is exchanged directly with the zone independent of any air system. ')
      ! Node identification reserved for future use.  Currently exchange energy directly with zone outside any air system
      !AirChillerSet(SetID)%NodeNumOutlet = &
      !         GetOnlySingleNode(Alphas(AlphaNum),ErrorsFound,TRIM(CurrentModuleObject), &
      !                      AirChillerSet(SetID)%Name,NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
      END IF


    !An extensible list is used to enter the individual names of each chiller in the set.
    !These chillers will be dispatched in this list order to meet the required zone load
    NumChillersInSet = NumAlphas - AlphaNum
    AlphaStartList = AlphaNum !+ 1
    AirChillerSet(SetID)%NumCoils = NumChillersInSet
    IF(.NOT. ALLOCATED(AirChillerSet(SetID)%CoilNum))ALLOCATE(AirChillerSet(SetID)%CoilNum(NumChillersInSet))
    DO ChillerIndex = 1,NumChillersInSet
      AlphaListNum = AlphaStartList + ChillerIndex
      IF(.NOT. lAlphaBlanks(AlphaListNum)) THEN
        CoilNum = FindItemInList(Alphas(AlphaListNum),WarehouseCoil%Name,NumSimulationRefrigAirChillers)
        IF(CoilNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)// &
                '="'//TRIM(AirChillerSet(SetID)%Name)//'", has an invalid '//&
                TRIM(cAlphaFieldNames(AlphaListNum))//' defined as '//TRIM(Alphas(AlphaListNum)))
          ErrorsFound = .TRUE.
        END IF ! == 0
        AirChillerSet(SetID)%CoilNum(ChillerIndex)= CoilNum
        WarehouseCoil(CoilNum)%ZoneName    = AirChillerSet(SetID)%ZoneName
        WarehouseCoil(CoilNum)%ZoneNum     = AirChillerSet(SetID)%ZoneNum
        WarehouseCoil(CoilNum)%ZoneNodeNum = AirChillerSet(SetID)%ZoneNodeNum
      END IF  ! ! = alphablanks
    END DO !CoilID over NumChillersInSet
  END DO ! NumChillerSets
 END IF ! NumChillerSets > 0
!************* End Air Chiller Sets


!**** Read CaseAndWalkIn Lists **********************************************************
  IF(NumSimulationCaseAndWalkInLists > 0) THEN
  CurrentModuleObject='Refrigeration:CaseAndWalkInList'
  DO ListNum=1,NumSimulationCaseAndWalkInLists
    CALL GetObjectItem(CurrentModuleObject,ListNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),CaseAndWalkInList%Name,ListNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF

    CaseAndWalkInList(ListNum)%Name=Alphas(1)

    ! CaseAndWalkInList alphas include CaseAndWalkInList name and one name for each Case or WalkIn in list
    ! below allocates larger than needed (each allocated to sum of both), but avoids two loops through input fields
    NumTotalLoadsOnList  = NumAlphas - 1
    IF(.NOT. ALLOCATED(CaseAndWalkInList(ListNum)%WalkInItemNum)) &
              ALLOCATE(CaseAndWalkInList(ListNum)%WalkInItemNum(NumTotalLoadsOnList))
    IF(.NOT. ALLOCATED(CaseAndWalkInList(ListNum)%CaseItemNum)) &
              ALLOCATE(CaseAndWalkInList(ListNum)%CaseItemNum(NumTotalLoadsOnList))
    IF(.NOT. ALLOCATED(CaseAndWalkInList(ListNum)%CoilItemNum)) &
              ALLOCATE(CaseAndWalkInList(ListNum)%CoilItemNum(NumTotalLoadsOnList))

    NumCasesOnList   = 0
    NumCoilsOnList   = 0
    NumWalkInsOnList = 0
    LoadCount = 0
    DO NumLoad = 1, NumTotalLoadsOnList
      AlphaListNum= 1 + NumLoad
      IF (.NOT. lAlphaBlanks(alphalistnum)) THEN
        LoadCount = Loadcount + 1
        LoadWalkInNum =  0
        LoadCaseNum   =  0
        LoadCoilNum   =  0
        IF(NumSimulationWalkIns > 0) &
                    LoadWalkInNum =  FindItemInList(Alphas(AlphaListNum),WalkIn%Name,NumSimulationWalkIns)
        IF(NumSimulationCases > 0) &
                     LoadCaseNum   =  FindItemInList(Alphas(AlphaListNum),RefrigCase%Name,NumSimulationCases)
        IF(NumSimulationRefrigAirChillers > 0) &
                     LoadCoilNum   =  FindItemInList(Alphas(AlphaListNum),WarehouseCoil%Name,NumSimulationRefrigAirChillers)
         IF((LoadWalkInNum == 0) .AND. (LoadCaseNum == 0).AND. (LoadCoilNum == 0)) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                             TRIM(cAlphaFieldNames(AlphaListNum))//'", has an invalid '//&
                             'value of '//TRIM(Alphas(AlphaListNum)))
            ErrorsFound = .TRUE.
         ELSEIF((LoadWalkInNum /= 0) .AND. (LoadCaseNum /= 0).AND. (LoadCoilNum /= 0)) THEN
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(cAlphaFieldNames(AlphaListNum))//&
                              '", '//TRIM(Alphas(AlphaListNum))//  &
                              ' Case and WalkIns and Refrigerated Coils cannot have the same name.')
           ErrorsFound = .TRUE.
         ELSEIF (LoadWalkInNum /= 0) THEN
           NumWalkInsOnList = NumWalkInsOnList + 1
           CaseAndWalkInList(ListNum)%WalkInItemNum(NumWalkInsOnList) = LoadWalkInNum
         ELSEIF (LoadCaseNum /= 0) THEN
           NumCasesOnList = NumCasesOnList + 1
           CaseAndWalkInList(ListNum)%CaseItemNum(NumCasesOnList) = LoadCaseNum
         ELSEIF (LoadCoilNum /= 0) THEN
           NumCoilsOnList = NumCoilsOnList + 1
           CaseAndWalkInList(ListNum)%CoilItemNum(NumCoilsOnList) = LoadCoilNum
         END IF
       END IF !lalphablanks
    END DO !Num Total Loads on List
    IF(LoadCount == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)// &
       ', "'//TRIM(CaseAndWalkInList(ListNum)%Name)//'" : degenerate list '//&
       'All entries were blank.')
       ErrorsFound = .TRUE.
     END IF !loadcount == 0
     CaseAndWalkInList(ListNum)%NumCases   = NumCasesOnList
     CaseAndWalkInList(ListNum)%NumCoils   = NumCoilsOnList
     CaseAndWalkInList(ListNum)%NumWalkIns = NumWalkInsOnList
  END DO   !ListNum=1,NumSimulationCaseAndWalkInLists
  END IF   !(NumSimulationCaseAndWalkInLists > 0)

 !**** End read CaseAndWalkIn Lists **********************************************************


!************** Start RefrigerationRacks

IF(NumRefrigeratedRacks > 0) THEN

  CurrentModuleObject='Refrigeration:CompressorRack'

  DO RackNum=1,NumRefrigeratedRacks

    CALL GetObjectItem(CurrentModuleObject,RackNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),RefrigRack%Name,RackNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    RefrigRack(RackNum)%Name = Alphas(1)
    HeatReclaimRefrigeratedRack(RackNum)%Name = Alphas(1)
    HeatReclaimRefrigeratedRack(RackNum)%SourceType = CurrentModuleObject
    IF (SameString(Alphas(2),'Outdoors')) THEN
      RefrigRack(RackNum)%HeatRejectionLocation = LocationOutdoors
    ELSEIF (SameString(Alphas(2),'Zone')) THEN
      RefrigRack(RackNum)%HeatRejectionLocation = LocationZone
      ! don't need to set RefrigPresentInZone to .TRUE. here because only allowed to reject heat to zone
      ! holding all served cases,  so already set when case read in
    ELSE
      RefrigRack(RackNum)%HeatRejectionLocation = LocationOutdoors
      CALL ShowWarningError(TRIM(CurrentModuleObject)//', '//TRIM(cAlphaFieldNames(1))//' = "'//TRIM(RefrigRack(RackNum)%Name)//  &
              '": '//TRIM(cAlphaFieldNames(2))//' defined as '//TRIM(Alphas(2))//  &
              ' not found. Will assume '//TRIM(cAlphaFieldNames(2))//' is OUTDOORS and simulation continues.')
    END IF

    RefrigRack(RackNum)%RatedCOP                   = Numbers(1)

    IF(RefrigRack(RackNum)%RatedCOP <= 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(1))//' must be greater than 0.0')
      ErrorsFound = .TRUE.
    END IF

    RefrigRack(RackNum)%COPFTempPtr   = GetCurveIndex(Alphas(3)) ! convert curve name to number
    IF (RefrigRack(RackNum)%COPFTempPtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(3))//' not found:'//TRIM(Alphas(3)))
      ErrorsFound = .TRUE.
    END IF

    IF(.NOT. SameString(GetCurveType(RefrigRack(RackNum)%COPFTempPtr),'CUBIC')) THEN
      IF(.NOT. SameString(GetCurveType(RefrigRack(RackNum)%COPFTempPtr),'QUADRATIC')) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(3))//' object must be of type cubic or quadratic.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    RefrigRack(RackNum)%CondenserFanPower = Numbers(2)
    IF(Numbers(2) < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(2))//' must be greater than or equal to 0 Watts.')
      ErrorsFound = .TRUE.
    END IF

    RefrigRack(RackNum)%TotCondFTempPtr   = GetCurveIndex(Alphas(4)) ! convert curve name to number
    IF ((.NOT. lAlphaBlanks(4)) .AND. RefrigRack(RackNum)%TotCondFTempPtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(4))//' not found:'//TRIM(Alphas(4)))
      ErrorsFound = .TRUE.
    END IF

    IF (.NOT. lAlphaBlanks(4)) THEN
      IF(.NOT. SameString(GetCurveType(RefrigRack(RackNum)%TotCondFTempPtr),'CUBIC')) THEN
        IF(.NOT. SameString(GetCurveType(RefrigRack(RackNum)%TotCondFTempPtr),'QUADRATIC')) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(4))//' object must be of type cubic or quadratic.')
          ErrorsFound = .TRUE.
        END IF
      END IF
    END IF

    IF (SameString(Alphas(5),'EvaporativelyCooled')) THEN
        RefrigRack(RackNum)%CondenserType = RefrigCondenserTypeEvap
        IF (RefrigRack(RackNum)%HeatRejectionLocation==LocationZone) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
          '" Evap cooled '//TRIM(cAlphaFieldNames(5))//' not available with '//TRIM(cAlphaFieldNames(2))//' = Zone.')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(5))//' reset to Air Cooled and simulation continues.')
        RefrigRack(RackNum)%CondenserType = RefrigCondenserTypeAir
        END IF
    ELSEIF (SameString(Alphas(5),'WaterCooled')) THEN
        RefrigRack(RackNum)%CondenserType = RefrigCondenserTypeWater
        IF (RefrigRack(RackNum)%HeatRejectionLocation==LocationZone) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
          '" Water cooled '//TRIM(cAlphaFieldNames(5))//' not available with '//TRIM(cAlphaFieldNames(2))//' = Zone.')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(5))//' reset to Air Cooled and simulation continues.')
        RefrigRack(RackNum)%CondenserType = RefrigCondenserTypeAir
        END IF
    ELSE
        RefrigRack(RackNum)%CondenserType = RefrigCondenserTypeAir
    END IF
    ! Get water-cooled condenser input, if applicable
    IF (RefrigRack(RackNum)%CondenserType == RefrigCondenserTypeWater) THEN
      RefrigRack(RackNum)%InletNode = GetOnlySingleNode(Alphas(6),ErrorsFound,CurrentModuleObject, &
               Alphas(1),NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
      RefrigRack(RackNum)%OutletNode = GetOnlySingleNode(Alphas(7),ErrorsFound,CurrentModuleObject, &
               Alphas(1),NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
     ! Check node connections
     CALL TestCompSet(CurrentModuleObject,Alphas(1),Alphas(6),Alphas(7),'RefrigRack Nodes')
     ! Get loop flow type
     IF (SameString(Alphas(8),'VariableFlow')) THEN
     RefrigRack(RackNum)%FlowType = VariableFlow
     ELSE IF (SameString(Alphas(8),'ConstantFlow')) THEN
     RefrigRack(RackNum)%FlowType = ConstantFlow
     ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                 '", invalid  '//TRIM(cAlphaFieldNames(8))//' not recognized: '//TRIM(Alphas(8)))
         CALL ShowContinueError('Check input value choices.')
         ErrorsFound=.TRUE.
      END IF
     ! Get outlet temperature schedule for variable flow case
     IF (RefrigRack(RackNum)%FlowType==VariableFlow) THEN
     IF (lAlphaBlanks(9)) THEN
       RefrigRack(RackNum)%OutletTempSchedPtr = 0
     ELSE
       RefrigRack(RackNum)%OutletTempSchedPtr = GetScheduleIndex(Alphas(9))  ! convert schedule name to pointer
     END IF
       IF (RefrigRack(RackNum)%OutletTempSchedPtr == 0) THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                 '", invalid  '//TRIM(cAlphaFieldNames(9))//' : '//TRIM(Alphas(9)))
         CALL ShowContinueError('A schedule with this name is not defined in this input data file.')
         ErrorsFound=.TRUE.
       END IF
    END IF
    ! Get volumetric flow rate if applicable
    IF (RefrigRack(RackNum)%FlowType==ConstantFlow) THEN
      RefrigRack(RackNum)%DesVolFlowRate = Numbers(3)
      RefrigRack(RackNum)%VolFlowRate = Numbers(3)
    END IF
    ! Get maximum flow rates
    RefrigRack(RackNum)%VolFlowRateMax = Numbers(4)

    ! Check constant flow for max violation, if applicable
    IF (RefrigRack(RackNum)%FlowType==ConstantFlow .AND. RefrigRack(RackNum)%VolFlowRate>Numbers(4)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                 '" '//TRIM(cNumericFieldNames(3))//' > '//TRIM(cNumericFieldNames(4))//'.')
         CALL ShowContinueError('Revise flow rates.')
         ErrorsFound=.TRUE.
    END IF
    ! Get max/min allowed water temps
    RefrigRack(RackNum)%OutletTempMax = Numbers(5)
    RefrigRack(RackNum)%InletTempMin = Numbers(6)
    ! set hardware limits on Node data structure for plant interactions
    !Node(RefrigRack(RackNum)%InletNode)%MassFlowRateMax = RefrigRack(RackNum)%MassFlowRateMax      !CR7425
    !Node(RefrigRack(RackNum)%InletNode)%MassFlowRateMin = 0.0D0                                    !CR7435
    ! set flow request for plant sizing.
    CALL RegisterPlantCompDesignFlow(RefrigRack(RackNum)%InletNode, RefrigRack(RackNum)%VolFlowRateMax)
    END IF  !Water cooled condenser data

    ! Get evaporative cooled condenser input
    IF (lAlphaBlanks(10)) THEN
       RefrigRack(RackNum)%EvapSchedPtr = 0
    ELSE
       RefrigRack(RackNum)%EvapSchedPtr = GetScheduleIndex(Alphas(10))  ! convert schedule name to pointer
       !   check availability schedule for values >= 0
       IF (RefrigRack(RackNum)%EvapSchedPtr > 0)THEN
         IF (.NOT. CheckScheduleValueMinMax(RefrigRack(RackNum)%EvapSchedPtr,'>=',0.0d0)) THEN
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//'" .')
           CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(10))//' = '//TRIM(Alphas(10)))
           CALL ShowContinueError('schedule values must be (>=0.).')
           ErrorsFound=.TRUE.
         END IF
       ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                 '", invalid  '//TRIM(cAlphaFieldNames(10))//' = '//TRIM(Alphas(10)))
         CALL ShowContinueError('A schedule with this name is not defined in this input data file.')
         ErrorsFound=.TRUE.
       END IF
    END IF

    RefrigRack(RackNum)%EvapEffect= Numbers(7)
    IF (RefrigRack(RackNum)%EvapEffect < 0.0d0 .OR. RefrigRack(RackNum)%EvapEffect > 1.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
          '" '//TRIM(cNumericFieldNames(7))//' cannot be less than zero or greater than 1.0.')
          ErrorsFound = .TRUE.
    END IF

    RefrigRack(RackNum)%CondenserAirFlowRate = Numbers(8)
    IF (RefrigRack(RackNum)%CondenserType == RefrigCondenserTypeEvap .AND. RefrigRack(RackNum)%CondenserAirFlowRate <= 0.0d0 &
       .AND. RefrigRack(RackNum)%CondenserAirFlowRate /= AutoCalculate) THEN
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(8))//' cannot be less than or equal to zero.')
       ErrorsFound = .TRUE.
    END IF

!   Basin heater power as a function of temperature must be greater than or equal to 0
    RefrigRack(RackNum)%BasinHeaterPowerFTempDiff = Numbers(9)
       IF(RefrigRack(RackNum)%CondenserType == RefrigCondenserTypeEvap .AND. Numbers(9) < 0.0d0) THEN
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="' //TRIM(RefrigRack(RackNum)%Name)//&
                     '", '//TRIM(cNumericFieldNames(9))//' must be >= 0')
           ErrorsFound = .TRUE.
       END IF

    RefrigRack(RackNum)%BasinHeaterSetPointTemp = Numbers(10)
    IF(RefrigRack(RackNum)%CondenserType == RefrigCondenserTypeEvap .AND. RefrigRack(RackNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
       CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(10))//' is less than 2 deg C. Freezing could occur.')
    END IF

    RefrigRack(RackNum)%EvapPumpPower = Numbers(11)
    IF(RefrigRack(RackNum)%CondenserType == RefrigCondenserTypeEvap .AND. RefrigRack(RackNum)%EvapPumpPower < 0.0d0 &
       .AND. RefrigRack(RackNum)%EvapPumpPower /= AutoCalculate) THEN
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(11))//' cannot be less than zero.')
       ErrorsFound = .TRUE.
    END IF

    ! Get Water System tank connections
    RefrigRack(RackNum)%SupplyTankName = Alphas(11)
    IF (lAlphaBlanks(11)) THEN
       RefrigRack(RackNum)%EvapWaterSupplyMode = WaterSupplyFromMains
    ELSE
    RefrigRack(RackNum)%EvapWaterSupplyMode = WaterSupplyFromTank
    CALL SetupTankDemandComponent(RefrigRack(RackNum)%Name, CurrentModuleObject, &
               RefrigRack(RackNum)%SupplyTankName, ErrorsFound, RefrigRack(RackNum)%EvapWaterSupTankID, &
               RefrigRack(RackNum)%EvapWaterTankDemandARRID)
    ENDIF

    ! Check condenser air inlet node connection
    IF (lAlphaBlanks(12)) THEN
      RefrigRack(RackNum)%OutsideAirNodeNum = 0
    ELSE
      RefrigRack(RackNum)%OutsideAirNodeNum = &
           GetOnlySingleNode(Alphas(12),ErrorsFound,CurrentModuleObject,Alphas(1), &
           NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsParent)
      IF(.not. CheckOutAirNodeNumber(RefrigRack(RackNum)%OutsideAirNodeNum))THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                '", '//TRIM(cAlphaFieldNames(12))//' not found: '//TRIM(Alphas(12)))
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound = .TRUE.
     END IF
    ENDIF

    IF (.NOT. lAlphaBlanks(13))  RefrigRack(RackNum)%EndUseSubcategory = Alphas(13)

!Read all loads on this rack: cases and walk-ins and coils
    NumCases = 0
    NumCoils = 0
    NumWalkIns = 0
    RefrigRack(RackNum)%NumCases = 0
    RefrigRack(RackNum)%NumCoils = 0
    RefrigRack(RackNum)%NumWalkIns = 0
    RefrigRack(RackNum)%TotalRackLoad = 0.0d0

!   Read display case and walkin assignments for this rack
    AlphaNum = 14
    IF(lAlphaBlanks(AlphaNum) ) THEN
       !No cases or walkins or coils specified, ie, rack has no load
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)// &
                    '" : has no loads, must have at least one of: '//TRIM(cAlphaFieldNames(14)))
        ErrorsFound = .TRUE.
    ELSE  ! (.NOT. lAlphaBlanks(AlphaNum))
    ! Entry for Alphas(AlphaNum) can be either a Case, WalkIn, Coil, or CaseAndWalkInList name
      CaseAndWalkInListNum=0
      CaseNum=0
      WalkInNum=0
      CoilNum=0
      IF(NumSimulationCaseAndWalkInLists > 0) &
            CaseAndWalkInListNum=FindItemInList(Alphas(AlphaNum),CaseAndWalkInList%Name,NumSimulationCaseAndWalkInLists)
      IF(NumSimulationCases > 0)CaseNum=FindItemInList(Alphas(AlphaNum),RefrigCase%Name,NumSimulationCases)
      IF(NumSimulationWalkIns > 0)WalkInNum=FindItemInList(Alphas(AlphaNum),WalkIn%Name,NumSimulationWalkIns)
      IF(NumSimulationRefrigAirChillers > 0)  &
         CoilNum=FindItemInList(Alphas(AlphaNum),WarehouseCoil%Name,NumSimulationRefrigAirChillers)
      NumNameMatches = 0
      IF(CaseAndWalkInListNum /= 0)NumNameMatches = NumNameMatches +1
      IF(CaseNum /= 0)       NumNameMatches = NumNameMatches +1
      IF(WalkInNum /= 0)     NumNameMatches = NumNameMatches +1
      IF(CoilNum /= 0)       NumNameMatches = NumNameMatches +1

      IF (NumNameMatches /= 1) THEN !name must uniquely point to a list or a single case or walkin
          ErrorsFound = .TRUE.
          IF(NumNameMatches == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//&
                   '" : has an invalid '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          ELSEIF(NumNameMatches > 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                    TRIM(RefrigRack(RackNum)%Name)//'" : has a non-unique name '//&
                    'that could be either a '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          END IF !num matches = 0 or > 1
      ELSEIF(CaseAndWalkInListNum /= 0)  THEN  !Name points to a CaseAndWalkInList
         NumCoils   = CaseAndWalkInList(CaseAndWalkInListNum)%NumCoils
         NumCases   = CaseAndWalkInList(CaseAndWalkInListNum)%NumCases
         NumWalkIns = CaseAndWalkInList(CaseAndWalkInListNum)%NumWalkIns
         RefrigRack(RackNum)%NumCoils   = NumCoils
         RefrigRack(RackNum)%NumCases   = NumCases
         RefrigRack(RackNum)%NumWalkIns = NumWalkIns
         IF(.NOT. ALLOCATED(RefrigRack(RackNum)%CoilNum))ALLOCATE(RefrigRack(RackNum)%CoilNum(NumCoils))
         RefrigRack(RackNum)%CoilNum(1:NumCoils) = CaseAndWalkInList(CaseAndWalkInListNum)%CoilItemNum(1:NumCoils)
         IF(.NOT. ALLOCATED(RefrigRack(RackNum)%CaseNum))ALLOCATE(RefrigRack(RackNum)%CaseNum(NumCases))
         RefrigRack(RackNum)%CaseNum(1:NumCases) = CaseAndWalkInList(CaseAndWalkInListNum)%CaseItemNum(1:NumCases)
         IF(.NOT. ALLOCATED(RefrigRack(RackNum)%WalkInNum))ALLOCATE(RefrigRack(RackNum)%WalkInNum(NumWalkIns))
         RefrigRack(RackNum)%WalkInNum(1:NumWalkIns) = CaseAndWalkInList(CaseAndWalkInListNum)%WalkInItemNum(1:NumWalkIns)
      ELSEIF (CoilNum /= 0) THEN     !Name points to a coil
         NumCoils = 1
         RefrigRack(RackNum)%NumCoils = 1
         IF(.NOT. ALLOCATED(RefrigRack(RackNum)%CoilNum)) ALLOCATE(RefrigRack(RackNum)%CoilNum(NumCoils))
         RefrigRack(RackNum)%CoilNum(NumCoils)=CoilNum
      ELSEIF (CaseNum /= 0) THEN     !Name points to a case
         NumCases = 1
         RefrigRack(RackNum)%NumCases = 1
         IF(.NOT. ALLOCATED(RefrigRack(RackNum)%CaseNum)) ALLOCATE(RefrigRack(RackNum)%CaseNum(NumCases))
         RefrigRack(RackNum)%CaseNum(NumCases)=CaseNum
      ELSEIF (WalkInNum /= 0) THEN   !Name points to a walkin
         NumWalkIns = 1
         RefrigRack(RackNum)%NumWalkIns = 1
         IF(.NOT. ALLOCATED(RefrigRack(RackNum)%WalkInNum)) &
                  ALLOCATE(RefrigRack(RackNum)%WalkInNum(NumWalkIns))
         RefrigRack(RackNum)%WalkInNum(NumWalkIns)=WalkInNum
      END IF  !NumNameMatches /= 1
    END IF !blank input for loads on rack

    IF (NumCases > 0) THEN
      DO CaseIndex = 1, NumCases
         CaseID=RefrigRack(RackNum)%CaseNum(CaseIndex)
         !mark all cases on rack as used by this system (checking for unused or non-unique cases)
         RefrigCase(CaseID)%NumSysAttach = RefrigCase(CaseID)%NumSysAttach + 1
         !determine total capacity on rack
         RefrigRack(RackNum)%TotalRackLoad =  RefrigRack(RackNum)%TotalRackLoad +  RefrigCase(CaseID)%DesignRatedCap
      END DO  !CaseIndex=1,NumCases
!     check that all refrigerated cases attached to a rack are to the same zone if heat rejection location is "Zone"
!     however, won't matter if walk-in specified
      IF(RefrigRack(RackNum)%HeatRejectionLocation == LocationZone .AND. RefrigRack(RackNum)%NumCases > 1 .AND. &
                 RefrigCase(RefrigRack(RackNum)%CaseNum(1))%ActualZoneNum /= 0 .AND. NumWalkIns < 1 .AND. &
                 NumCoils < 1 ) THEN
        ZoneNum = RefrigCase(RefrigRack(RackNum)%CaseNum(1))%ActualZoneNum
        DO CaseIndex = 2,RefrigRack(RackNum)%NumCases
          IF(RefrigCase(RefrigRack(RackNum)%CaseNum(CaseIndex))%ActualZoneNum == ZoneNum)CYCLE
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                            TRIM(RefrigRack(RackNum)%Name)//'" : All cases '//&
                            'attached to a rack must be in the same zone when '//TRIM(cAlphaFieldNames(2))//&
                            ' equals "Zone".')
          ErrorsFound = .TRUE.
          EXIT
        END DO
      END IF ! heat rejection location is zone
    END IF  ! numcases > 0

    IF (NumCoils > 0) THEN
      RefrigRack(RackNum)%CoilFlag = .TRUE.
      DO CoilIndex = 1, NumCoils
         CoilNum=RefrigRack(RackNum)%CoilNum(CoilIndex)
         !mark all Coils on rack as used by this system (checking for unused or non-unique Coils)
         WarehouseCoil(CoilNum)%NumSysAttach = WarehouseCoil(CoilNum)%NumSysAttach + 1
         !determine total capacity on rack
         RefrigRack(RackNum)%TotalRackLoad =  RefrigRack(RackNum)%TotalRackLoad + WarehouseCoil(CoilNum)%RatedSensibleCap
      END DO  !CoilIndex=1,NumCoils
    END IF  !numcoils > 0

    IF (NumWalkIns > 0) THEN
      DO WalkInIndex = 1, NumWalkIns
         WalkInID=RefrigRack(RackNum)%WalkInNum(WalkInIndex)
         !mark all WalkIns on rack as used by this system (checking for unused or non-unique WalkIns)
         WalkIn(WalkInID)%NumSysAttach = WalkIn(WalkInID)%NumSysAttach + 1
         !determine total capacity on rack
         RefrigRack(RackNum)%TotalRackLoad =  RefrigRack(RackNum)%TotalRackLoad +  WalkIn(WalkInID)%DesignRatedCap
      END DO  !WalkInIndex=1,NumWalkIns
    END IF !NumWalkins

    IF (NumWalkIns > 0 .OR. NumCoils > 0) THEN
      !Get the heat rejection Zone node number from the zone name entered by the user (if heatrej location = zone)
      IF (RefrigRack(RackNum)%HeatRejectionLocation==LocationZone) THEN
        IF ( lalphablanks(15)) THEN
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//  &
                     TRIM(cAlphaFieldNames(15))//&
                     ' must be input if walkins or AirChillers connected to rack and heat rejection location = zone.')
             ErrorsFound=.TRUE.
        ELSE ! alpha (15) not blank
          RefrigRack(RackNum)%HeatRejectionZoneNum      = FindItemInList(Alphas(15),Zone%Name,NumOfZones)
          RefrigRack(RackNum)%HeatRejectionZoneNodeNum  = GetSystemNodeNumberForZone(Alphas(15))
          IF (RefrigRack(RackNum)%HeatRejectionZoneNum == 0) THEN
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(RefrigRack(RackNum)%Name)//  &
                           '", invalid  '//TRIM(cAlphaFieldNames(15))//' not valid: '//TRIM(Alphas(15)))
             ErrorsFound=.TRUE.
          ELSE
            RefrigPresentInZone(RefrigRack(RackNum)%HeatRejectionZoneNum) = .TRUE.
          END IF !zonenum == 0
        END IF ! alpha 15 blank
      END IF ! zone heat rej and walk-ins or coils present, must input heat rejection zone
    END IF  !numwalkins or coils > 0

 ! set condenser air flow and evap water pump power if autocalculated
 ! autocalculate condenser evap water pump if needed
        IF(RefrigRack(RackNum)%CondenserType==RefrigCondenserTypeEvap .AND. RefrigRack(RackNum)%EvapPumpPower==AutoCalculate) THEN
          RefrigRack(RackNum)%EvapPumpPower = CondPumpRatePower * RefrigRack(RackNum)%TotalRackLoad
        END IF
 ! autocalculate evap condenser air volume flow rate if needed
        IF(RefrigRack(RackNum)%CondenserType==RefrigCondenserTypeEvap .AND.   &
                          RefrigRack(RackNum)%CondenserAirFlowRate==AutoCalculate) THEN
          RefrigRack(RackNum)%CondenserAirFlowRate = AirVolRateEvapCond * RefrigRack(RackNum)%TotalRackLoad
        END IF

  END DO  !RackNum=1,NumRefrigeratedRacks

  ALLOCATE(CheckEquipNameRackWaterCondenser(NumRefrigeratedRacks))
  CheckEquipNameRackWaterCondenser = .TRUE.
END IF !(NumRefrigeratedRacks > 0)

IF(NumRefrigSystems > 0 .OR. NumTransRefrigSystems >0) THEN

  IF(NumRefrigSystems > 0 .AND. NumRefrigCondensers == 0) THEN
    CALL ShowSevereError('Refrigeration:System objects were found during input processing, however '// &
                        'no Rrefrigeration condenser objects (which may be either: ')
    CALL ShowContinueError(' Refrigeration:Condenser:AirCooled, Refrigeration:Condenser:WaterCooled, '// &
                           ' Refrigeration:Condenser:EvaporativeCooled,or Refrigeration:Condenser:CascadeCooled) were found.')
    ErrorsFound = .TRUE.
  END IF
  IF(NumTransRefrigSystems > 0 .AND. NumSimulationGasCooler == 0) THEN
    CALL ShowSevereError('Refrigeration:TranscriticalSystem objects were found during input processing, however '// &
                        'no Refrigeration gas cooler objects (Refrigeration:GasCooler:AirCooled) were found.')
    ErrorsFound = .TRUE.
  END IF
  IF(NumSimulationCompressors == 0) THEN
    CALL ShowSevereError('Refrigeration:System objects were found during input processing, however '// &
                        'no Refrigeration:Compressor objects were found.')
    ErrorsFound = .TRUE.
  END IF

!************ START CONDENSER INPUT  **************

  IF(NumSimulationCondAir > 0) THEN
  CurrentModuleObject='Refrigeration:Condenser:AirCooled'
  DO CondNum=1,NumSimulationCondAir
    CALL GetObjectItem(CurrentModuleObject,CondNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),Condenser%Name,CondNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//' = '//TRIM(Alphas(1)))
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF     !IsNotOK on Verify Name
    Condenser(CondNum)%Name = Alphas(1)
    HeatReclaimRefrigCondenser(CondNum)%Name=Alphas(1)
    Condenser(CondNum)%CapCurvePtr = GetCurveIndex(Alphas(2)) ! convert curve name to number
    IF (Condenser(CondNum)%CapCurvePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
           '", invalid  '//TRIM(cAlphaFieldNames(2))//' not found:'//TRIM(Alphas(2)))
      ErrorsFound = .TRUE.
    END IF

    !set start of count for number of systems attached to this condenser
    Condenser(CondNum)%NumSysAttach  = 0
    IF(.NOT. ALLOCATED(Condenser(CondNum)%SysNum))&
             ALLOCATE(Condenser(CondNum)%SysNum(NumRefrigSystems))

    !set CondenserType and rated temperature difference (51.7 - 35)C per ARI 460
    Condenser(CondNum)%CondenserType = RefrigCondenserTypeAir
    HeatReclaimRefrigCondenser(CondNum)%SourceType=RefrigCondenserTypeAir
    Condenser(CondNum)%RatedDelT     = CondARI460DelT  != 16.7d0 ,Rated sat cond temp - dry bulb air T for air-cooled Condensers, ARI460
    Condenser(CondNum)%RatedTCondense= CondARI460Tcond
    if (Condenser(CondNum)%CapCurvePtr > 0) then
      Condenser(CondNum)%RatedCapacity=CurveValue(Condenser(CondNum)%CapCurvePtr,CondARI460DelT)
    endif
    !elevation capacity correction on air-cooled condensers, Carrier correlation more conservative than Trane
    Condenser(CondNum)%RatedCapacity = Condenser(CondNum)%RatedCapacity*(1.d0 - 7.17D-5*Elevation)
    IF(Condenser(CondNum)%RatedCapacity > 0.d0) THEN
      CALL GetCurveMinMaxValues(Condenser(CondNum)%CapCurvePtr,DelTempMin,DelTempMax)
      Capmin=CurveValue(Condenser(CondNum)%CapCurvePtr,DelTempMin)*(1.d0 - 7.17D-5*Elevation)!Mar 2011 bug fix
      Capmax=CurveValue(Condenser(CondNum)%CapCurvePtr,DelTempMax)*(1.d0 - 7.17D-5*Elevation)!Mar 2011 bug
      Condenser(CondNum)%TempSlope=(DelTempMax-DelTempMin)/((Capmax-Capmin))!*(1.d0 - 7.17D-5*Elevation))!Mar 2011 bug fix
      Condenser(CondNum)%MinCondLoad=Capmax-DelTempMax/Condenser(CondNum)%TempSlope
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
       '" Condenser capacity curve per ARI 460 must be input and must be greater than 0 Watts at 16.7C'// &
       ' temperature difference.')
      ErrorsFound = .TRUE.
    END IF

    Condenser(CondNum)%RatedSubcool  = 0.0d0   !default value
    IF(.NOT. lNumericBlanks(1)) Condenser(CondNum)%RatedSubcool  = Numbers(1)

    ! Get fan control type
    IF (SameString(Alphas(3),'Fixed')) THEN
        Condenser(CondNum)%FanSpeedControlType = FanConstantSpeed
    ELSE IF (SameString(Alphas(3),'FixedLinear')) THEN
        Condenser(CondNum)%FanSpeedControlType = FanConstantSpeedLinear
    ELSE IF (SameString(Alphas(3),'VariableSpeed')) THEN
        Condenser(CondNum)%FanSpeedControlType = FanVariableSpeed
    ELSE IF (SameString(Alphas(3),'TwoSpeed')) THEN
        Condenser(CondNum)%FanSpeedControlType = FanTwoSpeed
    ELSE
       Condenser(CondNum)%FanSpeedControlType = FanConstantSpeed  !default
    END IF   !Set fan control type

    IF(.NOT. lNumericBlanks(2)) Condenser(CondNum)%RatedFanPower = Numbers(2)
    IF((lNumericBlanks(2)) .OR. (Numbers(2) < 0.0d0)) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(2))//' must be input greater than or equal to 0 Watts.')
      ErrorsFound = .TRUE.
    END IF

    Condenser(CondNum)%FanMinAirFlowRatio  = 0.2d0   !default value
    IF(.NOT. lNumericBlanks(3)) Condenser(CondNum)%FanMinAirFlowRatio  = Numbers(3)

    ! Check condenser air inlet node connection
    ! Jan 2011 - added ability to reject heat to a zone from air-cooled condenser
    Condenser(CondNum)%CondenserRejectHeatToZone = .FALSE.
    IF (lAlphaBlanks(4)) THEN
      Condenser(CondNum)%InletAirNodeNum = 0
    ELSE !see if it's an outside air node name or an indoor zone name,
      !have to check inside first because outside check automatically generates an error message
      Condenser(CondNum)%InletAirZoneNum = FindItemInList(Alphas(4),Zone%Name,NumOfZones)
      !need to clearly id node number for air inlet conditions and zone number for casecredit assignment
      IF(Condenser(CondNum)%InletAirZoneNum /= 0) THEN
        !set condenser flag (later used to set system flag) and zone flag
        Condenser(CondNum)%InletAirNodeNum = GetSystemNodeNumberForZone(Alphas(4))
        Condenser(CondNum)%CondenserRejectHeatToZone = .TRUE.
        RefrigPresentInZone(Condenser(CondNum)%InletAirZoneNum) = .TRUE.
      ELSE   ! not in a conditioned zone, so see if it's outside
        Condenser(CondNum)%InletAirNodeNum = &
           GetOnlySingleNode(Alphas(4),ErrorsFound,CurrentModuleObject,Alphas(1), &
           NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsParent)
        IF(.NOT. CheckOutAirNodeNumber(Condenser(CondNum)%InletAirNodeNum))THEN
          ! not outside and not a zone
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                '", '//TRIM(cAlphaFieldNames(4))//' not found: '//TRIM(Alphas(4)))
          CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node or as a Zone.')
          ErrorsFound = .TRUE.
        END IF !checkoutairnodenumber
      END IF   !InletAirZoneNum \=0
    END IF  ! Condenser air inlet node connection

    Condenser(CondNum)%EndUseSubcategory =' '
    IF (.NOT. lAlphaBlanks(5))  Condenser(CondNum)%EndUseSubcategory = Alphas(5)

    Condenser(CondNum)%RefOpCharge = 0.0d0
    Condenser(CondNum)%RefReceiverInventory = 0.0d0
    Condenser(CondNum)%RefPipingInventory = 0.0d0
    IF (.NOT. lNumericBlanks(4))  Condenser(CondNum)%RefOpCharge = Numbers(4)
    IF (.NOT. lNumericBlanks(5))  Condenser(CondNum)%RefReceiverInventory = Numbers(5)
    IF (.NOT. lNumericBlanks(6))  Condenser(CondNum)%RefPipingInventory = Numbers(6)

  END DO   ! Read input for REFRIGERATION:Condenser:AirCooled
  END IF   ! NumSimulationCondAir > 0

  IF(NumSimulationCondEvap > 0)THEN
  CurrentModuleObject='Refrigeration:Condenser:EvaporativeCooled'
  DO CondIndex=1,NumSimulationCondEvap
    CondNum=CondIndex + NumSimulationCondAir
    CALL GetObjectItem(CurrentModuleObject,CondIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),Condenser%Name,CondNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF     !IsNotOK on Verify Name
    Condenser(CondNum)%Name = Alphas(1)
    HeatReclaimRefrigCondenser(CondNum)%Name=Alphas(1)

    !set start of count for number of systems attached to this condenser
    Condenser(CondNum)%NumSysAttach  = 0
    IF(.NOT. ALLOCATED(Condenser(CondNum)%SysNum))&
             ALLOCATE(Condenser(CondNum)%SysNum(NumRefrigSystems))

    !set CondenserType and rated Heat Rejection per ARI 490 rating
    Condenser(CondNum)%CondenserType = RefrigCondenserTypeEvap
    HeatReclaimRefrigCondenser(CondNum)%SourceType=RefrigCondenserTypeEvap
    Condenser(CondNum)%RatedTCondense= CondARI490Tcond
    Condenser(CondNum)%RatedDelT     = CondARI490DelT

    IF ((.NOT. lNumericBlanks(1)).AND.(Numbers(1)> 0.0d0)) THEN
      Condenser(CondNum)%RatedCapacity = Numbers(1)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
           '" '//TRIM(cNumericFieldNames(1))//' per ARI 490 must be input and must be greater than 0 Watts.')
      ErrorsFound = .TRUE.
    END IF
    !Calculate capacity elevation derate factor per ARI 490 barometric pressure correction factor
    Condenser(CondNum)%EvapElevFact=1.d0-3.074D-5*Elevation

    Condenser(CondNum)%RatedSubcool  = 0.0d0   !default value
    IF((.NOT. lNumericBlanks(2)).AND.(Numbers(2)> 0.0d0)) Condenser(CondNum)%RatedSubcool  = Numbers(2)

    ! Get fan control type
    IF (SameString(Alphas(2),'Fixed')) THEN
        Condenser(CondNum)%FanSpeedControlType = FanConstantSpeed
    ELSE IF (SameString(Alphas(3),'FixedLinear')) THEN
        Condenser(CondNum)%FanSpeedControlType = FanConstantSpeedLinear
    ELSE IF (SameString(Alphas(2),'VariableSpeed')) THEN
        Condenser(CondNum)%FanSpeedControlType = FanVariableSpeed
    ELSE IF (SameString(Alphas(2),'TwoSpeed')) THEN
        Condenser(CondNum)%FanSpeedControlType = FanTwoSpeed
    ELSE
       Condenser(CondNum)%FanSpeedControlType = FanConstantSpeed  !default
    END IF   !Set fan control type

    Condenser(CondNum)%RatedFanPower = Numbers(3)
    IF(Numbers(3) < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(3))//' must be greater than or equal to 0 Watts.')
      ErrorsFound = .TRUE.
    END IF

    Condenser(CondNum)%FanMinAirFlowRatio  = 0.2d0   !default value
    IF(.NOT. lNumericBlanks(4)) Condenser(CondNum)%FanMinAirFlowRatio  = Numbers(4)


    !Enter min and max and default coefficients for evap condenser HRCF correlation
    !Defaults taken from 2008 BAC equipment for R22, R134a, series CXV
    !Correlation coefficients for other manufacturers are very similar per Hugh Henderson's work
    Condenser(CondNum)%EvapCoeff1=6.63d0
    Condenser(CondNum)%EvapCoeff2=0.468d0
    Condenser(CondNum)%EvapCoeff3=17.93d0
    Condenser(CondNum)%EvapCoeff4=-.322d0
    Condenser(CondNum)%MinCapFacEvap=0.5d0
    Condenser(CondNum)%MaxCapFacEvap=5.0d0
    NumNum = 5  !added warnings if below not blank but unused due to limits
    IF(.NOT. lNumericBlanks(NumNum)) THEN
      IF(Numbers(NumNum)>= 0.0d0) THEN
        Condenser(CondNum)%EvapCoeff1=Numbers(NumNum)
      ELSE
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(NumNum))//' is less than 0 and was not used. Default was used.')
      ENDIF
    ENDIF
    NumNum = 6  ! EvapCoeff2 can't be equal to 0 because used in a denominator
    IF(.NOT. lNumericBlanks(NumNum)) THEN
      IF (Numbers(NumNum)>  0.0d0) THEN
        Condenser(CondNum)%EvapCoeff2=Numbers(NumNum)
      ELSE
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(NumNum))//' is less than or equal to 0 and was not used. Default was used.')
      ENDIF
    ENDIF
    NumNum = 7
    IF(.NOT. lNumericBlanks(NumNum)) THEN
      IF(Numbers(NumNum)>= 0.0d0) THEN
        Condenser(CondNum)%EvapCoeff3=Numbers(NumNum)
      ELSE
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(NumNum))//' is less than 0 and was not used. Default was used.')
      ENDIF
    ENDIF
    NumNum = 8
    IF(.NOT. lNumericBlanks(NumNum)) THEN
      IF(Numbers(NumNum)>= -20.0d0) THEN
        Condenser(CondNum)%EvapCoeff4=Numbers(NumNum)
      ELSE
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(NumNum))//' is less than -20 and was not used. Default was used.')
      ENDIF
    ENDIF
    NumNum = 9
    IF(.NOT. lNumericBlanks(NumNum)) THEN
      IF(Numbers(NumNum)>= 0.0d0)THEN
        Condenser(CondNum)%MinCapFacEvap=Numbers(NumNum)
      ELSE
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(NumNum))//' is less than 0 and was not used. Default was used.')
      ENDIF
    ENDIF
    NumNum = 10
    IF(.NOT. lNumericBlanks(NumNum)) THEN
      IF(Numbers(NumNum)>= 0.0d0) THEN
        Condenser(CondNum)%MaxCapFacEvap=Numbers(NumNum)
      ELSE
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(NumNum))//' is less than 0 and was not used. Default was used.')
      ENDIF
    ENDIF

    ! Check condenser air inlet node connection
    IF (lAlphaBlanks(3)) THEN
      Condenser(CondNum)%InletAirNodeNum = 0
    ELSE
      Condenser(CondNum)%InletAirNodeNum = &
           GetOnlySingleNode(Alphas(3),ErrorsFound,CurrentModuleObject,Alphas(1), &
           NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsParent)
      IF(.not. CheckOutAirNodeNumber(Condenser(CondNum)%InletAirNodeNum))THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                '", '//TRIM(cAlphaFieldNames(3))//' not found: '//TRIM(Alphas(3)))
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound = .TRUE.
      END IF
    END IF  ! Condenser air inlet node connection

    NumNum = 11
    Condenser(CondNum)%RatedAirFlowRate = Numbers(NumNum)
    ! Note the autocalculate feature for this value takes place in the system section because
    !  it is a function of the total cooling capacity of the cases served by the condenser

    ! Evaporative condensers basin heater power as a function of temperature must be greater than or equal to 0
    NumNum = 12
    Condenser(CondNum)%BasinHeaterPowerFTempDiff = Numbers(NumNum)
    IF(Numbers(NumNum) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="' //TRIM(Condenser(CondNum)%Name)//&
              '", '//TRIM(cNumericFieldNames(NumNum))//' must be >= 0')
        ErrorsFound = .TRUE.
    END IF

    NumNum = 13
    Condenser(CondNum)%BasinHeaterSetPointTemp = 2.0d0  !default
    IF(.NOT. lNumericBlanks(NumNum)) Condenser(CondNum)%BasinHeaterSetPointTemp = Numbers(NumNum)
    IF(Condenser(CondNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
         CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
            '", '//TRIM(cNumericFieldNames(NumNum))//' is less than 2 deg C. Freezing could occur.')
    END IF

    NumNum = 14
    Condenser(CondNum)%EvapPumpPower = 1000.d0    !default
    IF(.NOT. lNumericBlanks(NumNum)) Condenser(CondNum)%EvapPumpPower = Numbers(NumNum)
    ! Note the autocalculate feature for this value takes place in the system section because
    !  it is a function of the total cooling capacity of the cases served by the condenser

    ! Get Evaporative Water System tank connections
    Condenser(CondNum)%SupplyTankName = Alphas(4)
    IF (lAlphaBlanks(4)) THEN
      Condenser(CondNum)%EvapWaterSupplyMode = WaterSupplyFromMains
    ELSE
      Condenser(CondNum)%EvapWaterSupplyMode = WaterSupplyFromTank
      CALL SetupTankDemandComponent(Condenser(CondNum)%Name,CurrentModuleObject, &
               Condenser(CondNum)%SupplyTankName, ErrorsFound, Condenser(CondNum)%EvapWaterSupTankID, &
               Condenser(CondNum)%EvapWaterTankDemandARRID)
    END IF

    IF (lAlphaBlanks(5)) THEN
         Condenser(CondNum)%EvapSchedPtr = 0
    ELSE
       Condenser(CondNum)%EvapSchedPtr = GetScheduleIndex(Alphas(5))  ! convert schedule name to pointer
       !   check availability schedule for values >= 0
       IF (Condenser(CondNum)%EvapSchedPtr > 0)THEN
         IF (.NOT. CheckScheduleValueMinMax(Condenser(CondNum)%EvapSchedPtr,'>=',0.0d0)) THEN
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                   '" .')
             CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(Alphas(5)))
             CALL ShowContinueError('schedule values must be (>=0.).')
             ErrorsFound=.TRUE.
         END IF
       ELSE
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                 '", invalid  '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(Alphas(5)))
           CALL ShowContinueError('A schedule with this name is not defined in this input data file.')
           ErrorsFound=.TRUE.
       END IF
    END IF    ! Set Evap Schedule Pointer

    Condenser(CondNum)%EndUseSubcategory =' '
    IF (.NOT. lAlphaBlanks(6))  Condenser(CondNum)%EndUseSubcategory = Alphas(6)

    Condenser(CondNum)%RefOpCharge = 0.0d0
    Condenser(CondNum)%RefReceiverInventory = 0.0d0
    Condenser(CondNum)%RefPipingInventory = 0.0d0
    NumNum = 15
    IF (.NOT. lNumericBlanks(NumNum))  Condenser(CondNum)%RefOpCharge = Numbers(NumNum)
    NumNum = 16
    IF (.NOT. lNumericBlanks(NumNum))  Condenser(CondNum)%RefReceiverInventory = Numbers(NumNum)
    NumNum = 17
    IF (.NOT. lNumericBlanks(NumNum))  Condenser(CondNum)%RefPipingInventory = Numbers(NumNum)
  END DO   ! Read input for CONDENSER:REFRIGERATION:EVAPorativeCooled
  END IF   ! If NumSimulationCondEvap > 0

  IF(NumSimulationCondWater > 0)THEN
  CurrentModuleObject='Refrigeration:Condenser:WaterCooled'
  DO CondIndex=1,NumSimulationCondWater
    CondNum=CondIndex + NumSimulationCondAir + NumSimulationCondEvap
    CALL GetObjectItem(CurrentModuleObject,CondIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,&
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),Condenser%Name,CondNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF     !IsNotOK on Verify Name
    Condenser(CondNum)%Name = Alphas(1)
    HeatReclaimRefrigCondenser(CondNum)%Name=Alphas(1)

    !set start of count for number of systems attached to this condenser
    Condenser(CondNum)%NumSysAttach  = 0
    IF(.NOT. ALLOCATED(Condenser(CondNum)%SysNum))&
             ALLOCATE(Condenser(CondNum)%SysNum(NumRefrigSystems))

    !set CondenserType and rated Heat Rejection per ARI 450 rating
    Condenser(CondNum)%CondenserType = RefrigCondenserTypeWater
    HeatReclaimRefrigCondenser(CondNum)%SourceType=RefrigCondenserTypeWater
    IF ((.NOT. lNumericBlanks(1)).AND.(Numbers(1)> 0.0d0)) THEN
      Condenser(CondNum)%RatedCapacity = Numbers(1)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
           '" '//TRIM(cNumericFieldNames(1))//' per ARI 450 must be input and must be greater than 0 Watts.')
      ErrorsFound = .TRUE.
    END IF

    IF ((.NOT. lNumericBlanks(2)).AND.(Numbers(2)> 0.0d0)) THEN
      Condenser(CondNum)%RatedTCondense = Numbers(2)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
           '" '//TRIM(cNumericFieldNames(2))//' per ARI 450 must be input and must be greater than 0 C.')
      ErrorsFound = .TRUE.
    END IF

    IF (.NOT. lNumericBlanks(3)) THEN
      IF(Numbers(3)>=0.0d0)THEN
        Condenser(CondNum)%RatedSubcool = Numbers(3)
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
           '" '//TRIM(cNumericFieldNames(3))//' must be greater than or equal to zero.')
        ErrorsFound = .TRUE.
      END IF
    ELSE
      Condenser(CondNum)%RatedSubcool = 0.0d0   !default value
    END IF

    IF ((.NOT. lNumericBlanks(4)).AND.(Numbers(4)> 0.0d0)) THEN
        Condenser(CondNum)%RatedWaterInletT = Numbers(4)
        Condenser(CondNum)%RatedApproachT = Condenser(CondNum)%RatedTCondense-Numbers(4)
    ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
           '" '//TRIM(cNumericFieldNames(4))//' must be input and greater than zero.')
        ErrorsFound = .TRUE.
    END IF

    Condenser(CondNum)%InletNode = GetOnlySingleNode(Alphas(2),&
               ErrorsFound,CurrentModuleObject, &
               Alphas(1),NodeType_Water,NodeConnectionType_Inlet,&
               1, ObjectIsNotParent)
    Condenser(CondNum)%OutletNode = GetOnlySingleNode(Alphas(3),&
               ErrorsFound,CurrentModuleObject, &
               Alphas(1),NodeType_Water,NodeConnectionType_Outlet, &
               1, ObjectIsNotParent)
    ! Check node connections
    CALL TestCompSet(CurrentModuleObject,Alphas(1),Alphas(2),Alphas(3),&
                      'Water Cooled Condenser Nodes')
    ! Get loop flow type
    IF (SameString(Alphas(4),'VariableFlow')) THEN      !set FlowType
        Condenser(CondNum)%FlowType = VariableFlow
    ELSE IF (SameString(Alphas(4),'ConstantFlow')) THEN
        Condenser(CondNum)%FlowType = ConstantFlow
    ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                 '", invalid  '//TRIM(cAlphaFieldNames(4))//' not recognized: '//TRIM(Alphas(4)))
         CALL ShowContinueError('Check input value choices.')
         ErrorsFound=.TRUE.
    END IF   !Set FlowType

    ! Get outlet temperature schedule for variable flow case
    IF (Condenser(CondNum)%FlowType==VariableFlow) THEN
        IF (lAlphaBlanks(5)) THEN
          Condenser(CondNum)%OutletTempSchedPtr = 0
        ELSE
          Condenser(CondNum)%OutletTempSchedPtr = GetScheduleIndex(Alphas(5))  ! convert schedule name to pointer
        END IF
        IF (Condenser(CondNum)%OutletTempSchedPtr == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                 '", invalid  '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(Alphas(5)))
          CALL ShowContinueError('A schedule with this name is not defined in this input data file.')
          ErrorsFound=.TRUE.
        END IF
    END IF     ! Outlet temperature schedule

      ! Get volumetric flow rate if applicable
    IF (Condenser(CondNum)%FlowType==ConstantFlow) THEN
        IF((.NOT. lNumericBlanks(5)).AND.(Numbers(5) > 0.0d0))THEN
          Condenser(CondNum)%DesVolFlowRate = Numbers(5)
          Condenser(CondNum)%VolFlowRate = Numbers(5)
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                 '" '//TRIM(cNumericFieldNames(5))//' must be greater than zero.')
          CALL ShowContinueError('Revise flow rates.')
          ErrorsFound=.TRUE.
        END IF
    END IF

    ! Get maximum flow rates
    IF(Numbers(6) > 0.0d0)THEN
      Condenser(CondNum)%VolFlowRateMax = Numbers(6)
      ! Check constant flow for max violation, if applicable
      IF (Condenser(CondNum)%FlowType==ConstantFlow .AND. Condenser(CondNum)%VolFlowRate>Numbers(6)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                 '" '//TRIM(cNumericFieldNames(5))//' > '//TRIM(cNumericFieldNames(6))//' .')
        CALL ShowContinueError('Revise flow rates.')
        ErrorsFound=.TRUE.
      END IF   !Error check on max flow rate
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                 '" '//TRIM(cNumericFieldNames(6))//' must be greater than zero.')
      ErrorsFound=.TRUE.
    END IF

    ! Get max/min allowed water temps
    Condenser(CondNum)%OutletTempMax = Numbers(7)
    Condenser(CondNum)%InletTempMin  = Numbers(8)

    Condenser(CondNum)%EndUseSubcategory =' '
    IF (.NOT. lAlphaBlanks(6))  Condenser(CondNum)%EndUseSubcategory = Alphas(6)

    Condenser(CondNum)%RefOpCharge = 0.0d0
    Condenser(CondNum)%RefReceiverInventory = 0.0d0
    Condenser(CondNum)%RefPipingInventory = 0.0d0
    IF (.NOT. lNumericBlanks(9))   Condenser(CondNum)%RefOpCharge = Numbers(9)
    IF (.NOT. lNumericBlanks(10))  Condenser(CondNum)%RefReceiverInventory = Numbers(10)
    IF (.NOT. lNumericBlanks(11))  Condenser(CondNum)%RefPipingInventory = Numbers(11)

  END DO   ! Read input for CONDENSER:REFRIGERATION:WaterCooled

  ALLOCATE(CheckEquipNameWaterCondenser(NumRefrigCondensers))
  CheckEquipNameWaterCondenser = .TRUE.
  END IF   ! NumSimulationCondWater > 0

  !cascade condensers assumed to provide zero subcooling
  IF(NumSimulationCascadeCondensers > 0)THEN
  CurrentModuleObject='Refrigeration:Condenser:Cascade'
  DO CondIndex=1,NumSimulationCascadeCondensers
    CondNum=CondIndex + NumSimulationCondAir + NumSimulationCondEvap + NumSimulationCondWater
    CALL GetObjectItem(CurrentModuleObject,CondIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,&
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),Condenser%Name,CondNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF     !IsNotOK on Verify Name
    Condenser(CondNum)%Name = Alphas(1)
    HeatReclaimRefrigCondenser(CondNum)%Name=Alphas(1)

    !set start of count for number of systems attached to this condenser
    Condenser(CondNum)%NumSysAttach  = 0
    IF(.NOT. ALLOCATED(Condenser(CondNum)%SysNum))&
             ALLOCATE(Condenser(CondNum)%SysNum(NumRefrigSystems))

    !set CondenserType
    Condenser(CondNum)%CondenserType = RefrigCondenserTypeCascade

    IF (.NOT. lNumericBlanks(1)) THEN
      Condenser(CondNum)%RatedTCondense = Numbers(1)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
           '" '//TRIM(cNumericFieldNames(1))//' must be input.')
      ErrorsFound = .TRUE.
    END IF

    IF (.NOT. lNumericBlanks(2)) THEN
      IF(Numbers(2)>=0.0d0)THEN
        Condenser(CondNum)%RatedApproachT = Numbers(2)
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
           '" '//TRIM(cNumericFieldNames(2))//' must be greater than or equal to zero.')
        ErrorsFound = .TRUE.
      END IF
    ELSE
      Condenser(CondNum)%RatedApproachT = DefaultCascadeCondApproach
    END IF

    IF ((.NOT. lNumericBlanks(3)).AND.(Numbers(3)> 0.0d0)) THEN
      Condenser(CondNum)%RatedCapacity = Numbers(3)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
           '" '//TRIM(cNumericFieldNames(3))//' must be in put and must be greater than or equal to zero.')
      ErrorsFound = .TRUE.
    END IF

    ! Get condensing temperature type, either fixed by design or allowed to float to match other loads on supply system
    IF (.NOT. lAlphaBlanks(2)) THEN
      IF (SameString(Alphas(2),'Fixed')) THEN      !set Condenser Temperature Control Type
        Condenser(CondNum)%CascadeTempControl = CascadeTempSet
      ELSE IF (SameString(Alphas(2),'Float')) THEN
        Condenser(CondNum)%CascadeTempControl = CascadeTempFloat
      ELSE
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
                 '", invalid  '//TRIM(cAlphaFieldNames(2))//' not recognized: '//TRIM(Alphas(2)))
         CALL ShowContinueError('Check input value choices.')
         ErrorsFound=.TRUE.
      END IF   !string comparison to key choices
    ELSE   ! default is fixed/cascadetempset
      Condenser(CondNum)%CascadeTempControl = CascadeTempSet
    END IF ! not blank

    Condenser(CondNum)%CascadeRatedEvapTemp = Condenser(CondNum)%RatedTCondense - Condenser(CondNum)%RatedApproachT

    !future - add refrigerant inventory on system side accepting reject heat (as was done for secondary)
    Condenser(CondNum)%RefOpCharge = 0.0d0
    Condenser(CondNum)%RefReceiverInventory = 0.0d0
    Condenser(CondNum)%RefPipingInventory = 0.0d0
    IF (.NOT. lNumericBlanks(4))   Condenser(CondNum)%RefOpCharge = Numbers(4)
    IF (.NOT. lNumericBlanks(5))  Condenser(CondNum)%RefReceiverInventory = Numbers(5)
    IF (.NOT. lNumericBlanks(6))  Condenser(CondNum)%RefPipingInventory = Numbers(6)

  END DO   ! Read input for CONDENSER:REFRIGERATION:Cascade
  END IF   ! NumSimulationCascadeCondensers > 0



!************ END CONDENSER INPUT   **************


!**********  START GAS COOLER INPUT  **********

  IF(NumSimulationGasCooler > 0) THEN
  CurrentModuleObject='Refrigeration:GasCooler:AirCooled'
  DO GCNum=1,NumSimulationGasCooler
    CALL GetObjectItem(CurrentModuleObject,GCNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),GasCooler%Name,GCNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)// &
                           ', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//' = '//TRIM(Alphas(1)))
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF     !IsNotOK on Verify Name
    GasCooler(GCNum)%Name = Alphas(1)

    GasCooler(GCNum)%CapCurvePtr = GetCurveIndex(Alphas(2)) ! convert curve name to number
    IF (GasCooler(GCNum)%CapCurvePtr == 0) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
           '", invalid '//TRIM(cAlphaFieldNames(2))//' not found:'//TRIM(Alphas(2)))
      ErrorsFound = .TRUE.
    END IF

    !set start of count for number of systems attached to this gas cooler
    GasCooler(GCNum)%NumSysAttach  = 0
    IF(.NOT. ALLOCATED(GasCooler(GCNum)%SysNum))&
             ALLOCATE(GasCooler(GCNum)%SysNum(NumTransRefrigSystems))

      GasCooler(GCNum)%RatedApproachT = 3.0d0   ! rated CO2 gas cooler approach temperature
      if (GasCooler(GCNum)%CapCurvePtr > 0) then
        GasCooler(GCNum)%RatedCapacity=CurveValue(GasCooler(GCNum)%CapCurvePtr,GasCooler(GCNum)%RatedApproachT)
      endif
      ! elevation capacity correction on air-cooled condensers, Carrier correlation more conservative than Trane
      GasCooler(GCNum)%RatedCapacity = GasCooler(GCNum)%RatedCapacity*(1.d0 - 7.17D-5*Elevation)
      IF(GasCooler(GCNum)%RatedCapacity > 0.0d0) THEN
        CALL GetCurveMinMaxValues(GasCooler(GCNum)%CapCurvePtr,DelTempMin,DelTempMax)
        Capmin=CurveValue(GasCooler(GCNum)%CapCurvePtr,DelTempMin)*(1.d0 - 7.17D-5*Elevation)
        Capmax=CurveValue(GasCooler(GCNum)%CapCurvePtr,DelTempMax)*(1.d0 - 7.17D-5*Elevation)
        GasCooler(GCNum)%TempSlope=(DelTempMax-DelTempMin)/((Capmax-Capmin))
        GasCooler(GCNum)%MinCondLoad=Capmax-DelTempMax/GasCooler(GCNum)%TempSlope
      ELSE
        CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
         '" Gas Cooler capacity curve must be input and must be greater than 0 Watts at 3C'// &
         ' temperature difference.')
        ErrorsFound = .TRUE.
    END IF

    ! Get fan control type
    IF (SameString(Alphas(3),'Fixed')) THEN
        GasCooler(GCNum)%FanSpeedControlType = FanConstantSpeed
    ELSE IF (SameString(Alphas(3),'FixedLinear')) THEN
        GasCooler(GCNum)%FanSpeedControlType = FanConstantSpeedLinear
    ELSE IF (SameString(Alphas(3),'VariableSpeed')) THEN
        GasCooler(GCNum)%FanSpeedControlType = FanVariableSpeed
    ELSE IF (SameString(Alphas(3),'TwoSpeed')) THEN
        GasCooler(GCNum)%FanSpeedControlType = FanTwoSpeed
    ELSE
       GasCooler(GCNum)%FanSpeedControlType = FanConstantSpeed  !default
    END IF   !Set fan control type

    ! Gas cooler fan power
    GasCooler(GCNum)%RatedFanPower = 5000.0d0  ! default value
    IF(.NOT. lNumericBlanks(1)) GasCooler(GCNum)%RatedFanPower = Numbers(1)
    IF(Numbers(1) < 0.0d0) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(1))//' must be input greater than or equal to 0 Watts.')
      ErrorsFound = .TRUE.
    END IF

    ! Gas cooler minimum fan air flow ratio
    GasCooler(GCNum)%FanMinAirFlowRatio  = 0.2d0   !default value
    IF(.NOT. lNumericBlanks(2)) GasCooler(GCNum)%FanMinAirFlowRatio  = Numbers(2)
    IF((GasCooler(GCNum)%FanMinAirFlowRatio < 0.0d0).OR.(GasCooler(GCNum)%FanMinAirFlowRatio > 1.0d0)) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(2))//' must be a value between zero and one.  The default value (0.2) '//&
                            'will be used.')
      GasCooler(GCNum)%FanMinAirFlowRatio  = 0.2d0
    END IF

    ! Gas cooler transition temperature
    GasCooler(GCNum)%TransitionTemperature = 2.7d1    ! default value
    IF(.NOT. lNumericBlanks(3)) GasCooler(GCNum)%TransitionTemperature = Numbers(3)
    IF(GasCooler(GCNum)%TransitionTemperature < 2.5d1) THEN
      CALL ShowWarningError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
                            '" '//TRIM(cNumericFieldNames(3))//' is low (less than 25C).  Consider raising the '//&
                            'transition temperature to operate for longer periods of time in the subcritical region.')
    END IF
    IF(GasCooler(GCNum)%TransitionTemperature > 30.978d0) THEN
      CALL ShowWarningError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
                            '" '//TRIM(cNumericFieldNames(3))//' is greater than the critical temperature of carbon '//&
                            'dioxide.  The default value (27C) will be used.')
      GasCooler(GCNum)%TransitionTemperature = 2.7d1
    END IF

    ! Gas cooler approach temperature for transcritical operation
    GasCooler(GCNum)%GasCoolerApproachT = 3.0d0    ! default value
    IF(.NOT. lNumericBlanks(4)) GasCooler(GCNum)%GasCoolerApproachT = Numbers(4)
    IF(GasCooler(GCNum)%GasCoolerApproachT < 0.0d0) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
                            '" '//TRIM(cNumericFieldNames(4))//' must be greater than 0C.')
      ErrorsFound = .TRUE.
    END IF

    ! Gas cooler temperature difference for subcritical operation
    GasCooler(GCNum)%SubcriticalTempDiff = 1.0d1    ! default value
    IF(.NOT. lNumericBlanks(5)) GasCooler(GCNum)%SubcriticalTempDiff = Numbers(5)
    IF(GasCooler(GCNum)%SubcriticalTempDiff < 0.0d0) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(5))//' must be greater than 0C.')
      ErrorsFound = .TRUE.
    END IF

    ! Gas cooler minimum condensing temperature for subcritical operation
    GasCooler(GCNum)%MinCondTemp = 1.0d1    ! default value
    IF(.NOT. lNumericBlanks(6)) GasCooler(GCNum)%MinCondTemp = Numbers(6)
    IF(GasCooler(GCNum)%MinCondTemp > 30.9d0) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
                            '" '//TRIM(cNumericFieldNames(6))//' must be less than the critical temperature of carbon '//&
                            'dioxide (31C).')
      ErrorsFound = .TRUE.
    END IF

    ! Check GasCooler air inlet node connection
    GasCooler(GCNum)%GasCoolerRejectHeatToZone = .FALSE.
    IF (lAlphaBlanks(4)) THEN
      GasCooler(GCNum)%InletAirNodeNum = 0
    ELSE !see if it's an outside air node name or an indoor zone name,
      !have to check inside first because outside check automatically generates an error message
      GasCooler(GCNum)%InletAirZoneNum = FindItemInList(Alphas(4),Zone%Name,NumOfZones)
      !need to clearly id node number for air inlet conditions and zone number for casecredit assignment
      IF(GasCooler(GCNum)%InletAirZoneNum /= 0) THEN
        !set condenser flag (later used to set system flag) and zone flag
        GasCooler(GCNum)%InletAirNodeNum = GetSystemNodeNumberForZone(Alphas(4))
        GasCooler(GCNum)%GasCoolerRejectHeatToZone = .TRUE.
        RefrigPresentInZone(GasCooler(GCNum)%InletAirZoneNum) = .TRUE.
      ELSE   ! not in a conditioned zone, so see if it's outside
        GasCooler(GCNum)%InletAirNodeNum = &
           GetOnlySingleNode(Alphas(4),ErrorsFound,CurrentModuleObject,Alphas(1), &
           NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsParent)
        IF(.NOT. CheckOutAirNodeNumber(GasCooler(GCNum)%InletAirNodeNum))THEN
          ! not outside and not a zone
          CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(GasCooler(GCNum)%Name)//&
                '", '//TRIM(cAlphaFieldNames(4))//' not found: '//TRIM(Alphas(4)))
          CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node or as a Zone.')
          ErrorsFound = .TRUE.
        END IF !checkoutairnodenumber
      END IF   !InletAirZoneNum \=0
    END IF  ! Gas cooler air inlet node connection

    GasCooler(GCNum)%EndUseSubcategory =' '
    IF (.NOT. lAlphaBlanks(5))  GasCooler(GCNum)%EndUseSubcategory = Alphas(5)

    GasCooler(GCNum)%RefOpCharge = 0.0d0
    GasCooler(GCNum)%RefReceiverInventory = 0.0d0
    GasCooler(GCNum)%RefPipingInventory = 0.0d0
    IF (.NOT. lNumericBlanks(7))  GasCooler(GCNum)%RefOpCharge = Numbers(7)
    IF (.NOT. lNumericBlanks(8))  GasCooler(GCNum)%RefReceiverInventory = Numbers(8)
    IF (.NOT. lNumericBlanks(9))  GasCooler(GCNum)%RefPipingInventory = Numbers(9)

  END DO   ! Read input for REFRIGERATION:GasCooler:AirCooled
  END IF   ! NumSimulationGasCooler > 0

!**********  END GAS COOLER INPUT  **********


!************ START SECONDARY LOOP INPUT (before system input) **************
  IF (NumSimulationSecondarySystems > 0) THEN
  CurrentModuleObject='Refrigeration:SecondarySystem'
  DO SecondaryNum=1,NumSimulationSecondarySystems
    CALL GetObjectItem(CurrentModuleObject,SecondaryNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),Secondary%Name,SecondaryNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    Secondary(SecondaryNum)%Name = Alphas(1)

!   Find the loads on the secondary loop: can be input in form of case or walkin or CaseAndWalkInList names
    NominalTotalCaseCap = 0.0d0
    NumCases = 0
    NominalTotalCoilCap = 0.0d0
    NumCoils = 0
    NumWalkIns = 0
    NominalTotalWalkInCap = 0.0d0
    Secondary(SecondaryNum)%RefInventory=0.0d0

!   Read display case and walkin assignments for this secondary
    AlphaNum = 2
    IF(lAlphaBlanks(AlphaNum) ) THEN
       !No cases or walkins specified, ie, secondary has no load
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)// &
                    '", has no loads, must have at least one of: '//TRIM(cAlphaFieldNames(Alphanum)))
        ErrorsFound = .TRUE.
    ELSE  ! (.NOT. lAlphaBlanks(AlphaNum))

    ! Entry for Alphas(AlphaNum) can be either a Case, WalkIn Coil, or CaseAndWalkInList name
      CaseAndWalkInListNum=0
      CaseNum=0
      WalkInNum=0
      CoilNum = 0
      IF(NumSimulationCaseAndWalkInLists > 0) &
            CaseAndWalkInListNum=FindItemInList(Alphas(AlphaNum),CaseAndWalkInList%Name,NumSimulationCaseAndWalkInLists)
      IF(NumSimulationCases > 0)CaseNum=FindItemInList(Alphas(AlphaNum),RefrigCase%Name,NumSimulationCases)
      IF(NumSimulationWalkIns > 0)WalkInNum=FindItemInList(Alphas(AlphaNum),WalkIn%Name,NumSimulationWalkIns)
      IF(NumSimulationRefrigAirChillers > 0)  &
         CoilNum=FindItemInList(Alphas(AlphaNum),WarehouseCoil%Name,NumSimulationRefrigAirChillers)
      NumNameMatches = 0
      IF(CaseAndWalkInListNum /= 0)NumNameMatches = NumNameMatches +1
      IF(CaseNum /= 0)             NumNameMatches = NumNameMatches +1
      IF(WalkInNum /= 0)           NumNameMatches = NumNameMatches +1
      IF(CoilNum /= 0)             NumNameMatches = NumNameMatches +1

      IF (NumNameMatches /= 1) THEN !name must uniquely point to a list or a single case or walkin or coil
          ErrorsFound = .TRUE.
          IF(NumNameMatches == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                   '", has an invalid '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          ELSEIF(NumNameMatches > 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                    TRIM(Secondary(SecondaryNum)%Name)//'", has a non-unique name '//&
                    'that could be either a '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          END IF !num matches = 0 or > 1
      ELSEIF(CaseAndWalkInListNum /= 0)  THEN  !Name points to a CaseAndWalkInList
         NumCoils   = CaseAndWalkInList(CaseAndWalkInListNum)%NumCoils
         NumCases   = CaseAndWalkInList(CaseAndWalkInListNum)%NumCases
         NumWalkIns = CaseAndWalkInList(CaseAndWalkInListNum)%NumWalkIns
         Secondary(SecondaryNum)%NumCases   = NumCases
         Secondary(SecondaryNum)%NumCoils   = NumCoils
         Secondary(SecondaryNum)%NumWalkIns = NumWalkIns
         IF(.NOT. ALLOCATED(Secondary(SecondaryNum)%CaseNum))ALLOCATE(Secondary(SecondaryNum)%CaseNum(NumCases))
         Secondary(SecondaryNum)%CaseNum(1:NumCases) = CaseAndWalkInList(CaseAndWalkInListNum)%CaseItemNum(1:NumCases)
         IF(.NOT. ALLOCATED(Secondary(SecondaryNum)%CoilNum))ALLOCATE(Secondary(SecondaryNum)%CoilNum(NumCoils))
         Secondary(SecondaryNum)%CoilNum(1:NumCoils) = CaseAndWalkInList(CaseAndWalkInListNum)%CoilItemNum(1:NumCoils)
         IF(.NOT. ALLOCATED(Secondary(SecondaryNum)%WalkInNum))ALLOCATE(Secondary(SecondaryNum)%WalkInNum(NumWalkIns))
         Secondary(SecondaryNum)%WalkInNum(1:NumWalkIns) = CaseAndWalkInList(CaseAndWalkInListNum)%WalkInItemNum(1:NumWalkIns)
      ELSEIF (CaseNum /= 0) THEN     !Name points to a case
         NumCases = 1
         Secondary(SecondaryNum)%NumCases = 1
         IF(.NOT. ALLOCATED(Secondary(SecondaryNum)%CaseNum)) ALLOCATE(Secondary(SecondaryNum)%CaseNum(NumCases))
         Secondary(SecondaryNum)%CaseNum(NumCases)=CaseNum
      ELSEIF (CoilNum /= 0) THEN     !Name points to a coil
         NumCoils = 1
         Secondary(SecondaryNum)%NumCoils = 1
         IF(.NOT. ALLOCATED(Secondary(SecondaryNum)%CoilNum)) ALLOCATE(Secondary(SecondaryNum)%CoilNum(NumCoils))
         Secondary(SecondaryNum)%CoilNum(NumCoils)=CoilNum
      ELSEIF (WalkInNum /= 0) THEN   !Name points to a walkin
         NumWalkIns = 1
         Secondary(SecondaryNum)%NumWalkIns = 1
         IF(.NOT. ALLOCATED(Secondary(SecondaryNum)%WalkInNum)) &
                  ALLOCATE(Secondary(SecondaryNum)%WalkInNum(NumWalkIns))
         Secondary(SecondaryNum)%WalkInNum(NumWalkIns)=WalkInNum
      END IF  !NumNameMatches /= 1
    END IF !blank input for loads on secondary

    IF (NumCases > 0) THEN
       ! Find lowest design T loop fluid out of secondary chiller
       ! Sum rated capacity of all cases on Secondary
       DO CaseIndex = 1, NumCases
          !mark all cases on Secondary as used by this Secondary - checking for unused or non-unique cases
          CaseNum=Secondary(SecondaryNum)%CaseNum(CaseIndex)
          RefrigCase(CaseNum)%NumSysAttach = RefrigCase(CaseNum)%NumSysAttach + 1
          NominalTotalCaseCap = NominalTotalCaseCap + RefrigCase(CaseNum)%DesignRatedCap*RefrigCase(CaseNum)%RatedRTF
          Secondary(SecondaryNum)%RefInventory=Secondary(SecondaryNum)%RefInventory + &
                                              RefrigCase(Casenum)%DesignRefrigInventory
          IF(CaseIndex == 1) THEN  !look for lowest case design evap T for Secondary
              Secondary(SecondaryNum)%TMinNeeded=RefrigCase(CaseNum)%EvapTempDesign
          ELSE
              Secondary(SecondaryNum)%TMinNeeded = &
                 MIN(RefrigCase(CaseNum)%EvapTempDesign,Secondary(SecondaryNum)%TMinNeeded)
          END IF
       END DO  !CaseIndex=1,NumCases
    END IF  !Numcases > 0

    IF (NumCoils > 0) THEN
       ! Find lowest design T loop fluid out of secondary chiller
       ! Sum rated capacity of all Coils on Secondary
       DO CoilIndex = 1, NumCoils
          !mark all Coils on Secondary as used by this Secondary - checking for unused or non-unique Coils
          CoilNum=Secondary(SecondaryNum)%CoilNum(CoilIndex)
          WarehouseCoil(CoilNum)%NumSysAttach = WarehouseCoil(CoilNum)%NumSysAttach + 1
          NominalTotalCoilCap = NominalTotalCoilCap + WarehouseCoil(CoilNum)%RatedSensibleCap
          Secondary(SecondaryNum)%RefInventory=Secondary(SecondaryNum)%RefInventory + &
                                              WarehouseCoil(Coilnum)%DesignRefrigInventory
          IF((CoilIndex == 1).AND. (NumCases == 0)) THEN  !look for lowest Coil design evap T for Secondary
              Secondary(SecondaryNum)%TMinNeeded=WarehouseCoil(CoilNum)%TEvapDesign
          ELSE
              Secondary(SecondaryNum)%TMinNeeded = &
                 MIN(WarehouseCoil(CoilNum)%TEvapDesign,Secondary(SecondaryNum)%TMinNeeded)
          END IF
       END DO  !CoilIndex=1,NumCoils
    END IF  !NumCoils > 0

    IF(NumWalkIns > 0) THEN
       ! Find lowest design T loop fluid out of secondary chiller
       ! Sum rated capacity of all WalkIns on Secondary
       DO WalkInIndex = 1, NumWalkIns
          !mark all WalkIns on Secondary as used by this Secondary - checking for unused or non-unique WalkIns
          WalkInID=Secondary(SecondaryNum)%WalkInNum(WalkInIndex)
          WalkIn(WalkInID)%NumSysAttach = WalkIn(WalkInID)%NumSysAttach + 1
          NominalTotalWalkInCap = NominalTotalWalkInCap + WalkIn(WalkInID)%DesignRatedCap
          Secondary(SecondaryNum)%RefInventory=Secondary(SecondaryNum)%RefInventory + &
                                              WalkIn(WalkInID)%DesignRefrigInventory
          IF((WalkInIndex == 1).AND. (NumCases == 0) .AND. (NumCoils == 0)) THEN  !look for lowest load design evap T for Secondary
              Secondary(SecondaryNum)%TMinNeeded=WalkIn(WalkInID)%TEvapDesign
          ELSE
              Secondary(SecondaryNum)%TMinNeeded = &
                 MIN(Secondary(SecondaryNum)%TMinNeeded,WalkIn(WalkInID)%TEvapDesign)
          END IF
       END DO  !WalkInIndex=1,NumWalkIns
    END IF  ! Numwalkins > 0

    ! Get circulating fluid type
    AlphaNum=3
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      IF (SameString(Alphas(AlphaNum),'FluidAlwaysLiquid')) THEN
        Secondary(SecondaryNum)%FluidType = SecFluidTypeAlwaysLiquid
      ELSE IF (SameString(Alphas(AlphaNum),'FluidPhaseChange')) THEN
        Secondary(SecondaryNum)%FluidType = SecFluidTypePhaseChange
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                 '"  '//TRIM(cAlphaFieldNames(AlphaNum))//' not recognized = '//TRIM(Alphas(AlphaNum)))
        CALL ShowContinueError('Input value choices should be FluidAlwaysLiquid or FluidPhaseChange.')
        ErrorsFound=.TRUE.
      END IF   !Set FluidType
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                           '" '//TRIM(cAlphaFieldNames(AlphaNum))//' must be specified.')
      ErrorsFound = .TRUE.
    END IF  ! blank on cir fluid type

    AlphaNum=4
    Secondary(SecondaryNum)%FluidName = Alphas(AlphaNum)
       ! Error messages for refrigerants and glycols already found in fluidproperties

! Note remainder of inputs for secondary don't follow IDD input order because of different interpretations
!   and intermediate calculations used to assign default values for brine type vs. liquid overfeed/phase change loops

    IF (.NOT. lNumericBlanks(3)) THEN
       Secondary(SecondaryNum)%TEvapDesign =  Numbers(3)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(3))//' must be specified.')
      ErrorsFound = .TRUE.
    END IF  ! blank on N3

    IF (.NOT. lNumericBlanks(4)) THEN
       Secondary(SecondaryNum)%TApproachDifRated =  Numbers(4)
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                           '" '//TRIM(cNumericFieldNames(4))//' must be specified.')
      ErrorsFound = .TRUE.
    END IF  ! blank on N4

!^^^^^^^Now look at input and once-only calculations required only for liquid/brine secondary loops^^^^^^^^^^^^^^^^^^^^^^
!   Ensure that required input data is not missing prior to performing the following once-only calculations
    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                 '", Program terminated due to previous condition(s).')
    END IF  ! ErrorsFound

IF( Secondary(SecondaryNum)%FluidType == SecFluidTypeAlwaysLiquid) THEN
   IF (.NOT. lNumericBlanks(5)) THEN
    Secondary(SecondaryNum)%TRangeDifRated =  Numbers(5)
   ELSE
     CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                        '", '//TRIM(cNumericFieldNames(5))//' must be specified.')
     CALL ShowContinueError('...when '//trim(cAlphaFieldNames(3))//'="FluidAlwaysLiquid".')
     ErrorsFound = .TRUE.
   END IF  ! blank on N5

  ! Get fluid properties at rated conditions, will be used to calculate ht exchgr effectiveness
  TBrineOutRated = Secondary(SecondaryNum)%TEvapDesign + Secondary(SecondaryNum)%TApproachDifRated
  TBrineInRated = TBrineOutRated + Secondary(SecondaryNum)%TRangeDifRated
  TBrineAverage= (TbrineOutRated + TBrineInRated)/2.0d0
  Secondary(SecondaryNum)%TBrineAverage = TBrineAverage
  DensityBrineRated = GetDensityGlycol(Secondary(SecondaryNum)%FluidName,&
                                     TBrineAverage,Secondary(SecondaryNum)%FluidID,TrackMessage)
  Secondary(SecondaryNum)%DensityBrineRated = DensityBrineRated
  CpBrineRated = GetSpecificHeatGlycol(Secondary(SecondaryNum)%FluidName,&
                                     TBrineAverage,Secondary(SecondaryNum)%FluidID,TrackMessage)
  Secondary(SecondaryNum)%CpBrineRated = CpBrineRated

  !Users can input either design brine flow (m3/s), or capacity in W, or both.  Now have
  !  temperatures needed to calculate either the loop cooling capacity or fluid flow rate, if one was not input
  !  Don't need to save as a flow vol as a permanent var because calc whichever is missing here
  IF ((.NOT. lNumericBlanks(1)) .AND. (.NOT. lNumericBlanks(2))) THEN
      !Both values input, check for approximate agreement
      Secondary(SecondaryNum)%CoolingLoadRated =  Numbers(1)
      SecondaryFlowVolRated  =  Numbers(2)
      FlowMassRated = SecondaryFlowVolRated*DensityBrineRated
      NominalSecondaryCapacity = FlowMassRated * CpBrineRated * Secondary(SecondaryNum)%TRangeDifRated
      TestDelta = (NominalSecondaryCapacity-Secondary(SecondaryNum)%CoolingLoadRated)/NominalSecondaryCapacity
      IF (ABS(TestDelta) > 0.2d0) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
             ' You may wish to check the system definition. Based upon the design flow rate'//&
             ' and range temperature difference, '//&
             ' The nominal secondary loop heat exchanger capacity is, '// &
             TRIM(RoundSigDigits(NominalSecondaryCapacity,0))// &
             ' but the specified design capacity is,  '//&
             TRIM(RoundSigDigits(Secondary(SecondaryNum)%CoolingLoadRated,0)))
      END IF
  ELSEIF  (.NOT. lNumericBlanks(1)) THEN
      Secondary(SecondaryNum)%CoolingLoadRated =  Numbers(1)
      !Calc flow vol rated
      FlowMassRated=Secondary(SecondaryNum)%CoolingLoadRated/(CpBrineRated* &
                          Secondary(SecondaryNum)%TRangeDifRated)
      SecondaryFlowVolRated=FlowMassRated/DensityBrineRated
  ELSEIF  (.NOT. lNumericBlanks(2)) THEN
      SecondaryFlowVolRated =  Numbers(2)
      !Calc rated load
      FlowMassRated=SecondaryFlowVolRated*DensityBrineRated
      Secondary(SecondaryNum)%CoolingLoadRated=FlowMassRated*CpBrineRated* &
                          Secondary(SecondaryNum)%TRangeDifRated
  ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                 '", Either "'//TRIM(cNumericFieldNames(1))//'" OR "'// &
                 TRIM(cNumericFieldNames(2))//'" must be input.')
      ErrorsFound=.TRUE.
  END IF   ! Capacity Input via either or both options

  IF (.not. ErrorsFound) THEN
    !Calculate heat exchanger effectiveness based on rated flow and temperature differences
    Secondary(SecondaryNum)%HeatExchangeEta = Secondary(SecondaryNum)%CoolingLoadRated / &
         (FlowMassRated*CpBrineRated* (TBrineInRated - Secondary(SecondaryNum)%TEvapDesign))
    Secondary(SecondaryNum)%TBrineInRated = TBrineInRated
    IF (Secondary(SecondaryNum)%HeatExchangeEta > 0.99d0) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
             ' You may wish to check the system definition. '//&
             ' The heat exchanger effectiveness is, '// &
             TRIM(RoundSigDigits(Secondary(SecondaryNum)%HeatExchangeEta,2)))
        Secondary(SecondaryNum)%HeatExchangeEta = 0.99d0
    END IF
  ELSE
    CALL ShowContinueError('...remainder of this object input skipped due to previous errors')
    CYCLE
  ENDIF

  PumpTotRatedFlowVol=SecondaryFlowVolRated
  IF (.NOT. lNumericBlanks(7)) PumpTotRatedFlowVol = Numbers(7)

ELSE !FluidType = FluidTypePhaseChange     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  IF (.NOT. lNumericBlanks(1)) THEN
      Secondary(SecondaryNum)%CoolingLoadRated =  Numbers(1)
  ELSE
      Secondary(SecondaryNum)%CoolingLoadRated =  NominalTotalCaseCap + NominalTotalWalkInCap
      ! first estimate, will later be adjusted to include pump power
  END IF !input capacity

  Secondary(SecondaryNum)%TCondense = Secondary(SecondaryNum)%TEvapDesign + &
                                      Secondary(SecondaryNum)%TApproachDifRated
  Secondary(SecondaryNum)%CircRate = DefaultCircRate
  IF (.NOT. lNumericBlanks(10)) Secondary(SecondaryNum)%CircRate = Numbers(10)

  DensityPhaseChange = GetSatDensityRefrig(Secondary(SecondaryNum)%FluidName,&
                                         Secondary(SecondaryNum)%TCondense,0.0d0, &
                                         Secondary(SecondaryNum)%FluidID, &
                                         'GetInput in RefrigeratedCase')
  DeltaHPhaseChange = GetSatEnthalpyRefrig(Secondary(SecondaryNum)%FluidName,&
                                         Secondary(SecondaryNum)%TCondense, 1.0d0, &
                                         Secondary(SecondaryNum)%FluidID, &
                                         'GetInput in RefrigeratedCase') - &
                    GetSatEnthalpyRefrig(Secondary(SecondaryNum)%FluidName,&
                                         Secondary(SecondaryNum)%TCondense, 0.0d0, &
                                         Secondary(SecondaryNum)%FluidID, &
                                         'GetInput in RefrigeratedCase')

  !TotRatedFlowVol= capacity*circrate/deltahphasechange/density
  CalcTotFlowVol =  Secondary(SecondaryNum)%CoolingLoadRated * Secondary(SecondaryNum)%CircRate / &
                    (DensityPhaseChange * DeltaHPhaseChange)
  PumpTotRatedFlowVol = CalcTotFlowVol
  IF (.NOT. lNumericBlanks(7)) THEN
    PumpTotRatedFlowVol = Numbers(7)
    CalcCircRate = DensityPhaseChange * DeltaHPhaseChange * PumpTotRatedFlowVol / &
                   Secondary(SecondaryNum)%CoolingLoadRated
    DiffCircRates = (CalcCircRate - Secondary(SecondaryNum)%CircRate)/Secondary(SecondaryNum)%CircRate
    IF(ABS(DiffCircRates) > .3d0)THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
             ' '//TRIM(cNumericFieldNames(7))//' Produces a circulating rate of '//&
             TRIM(RoundSigDigits(CalcCircRate,2))//' ; '//' A circulating rate of '//&
             TRIM(RoundSigDigits(Secondary(SecondaryNum)%CircRate,2))//' would need a '//&
             ' '//TRIM(cNumericFieldNames(7))//' of '//TRIM(RoundSigDigits(CalcTotFlowVol,2))//&
             ' m3/s')
    END IF ! warning check on pump flow rate vs circ rate input
  END IF !blank pump flow rate
  SecondaryFlowVolRated = PumpTotRatedFlowVol

END IF !fluid type AlwaysLiquid or PhaseChange ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  !Read number of pumps (or pump stages) in secondary loop
  NumPumps = 1  !default value
  IF ((.NOT. lNumericBlanks(6)) .AND. (Numbers(6) >= 1) )  NumPumps =  Numbers(6)
  Secondary(SecondaryNum)%NumPumps = NumPumps
  ! Get pump power (users can input either power in W or head in Pa or both)
  ! Assume pump impeller efficiency is 0.78 (consistent with E+ Pump auto sizing assumption)
  ! Assume pump motor efficiency is 0.85 (Goulds Pumps motor data sheet)
  ! It is important that tot rated head must be for specific fluid
    IF ((.NOT. lNumericBlanks(8)) .AND. (.NOT. lNumericBlanks(9))) THEN
      Secondary(SecondaryNum)%PumpTotRatedPower =  Numbers(8)
      PumpTotRatedHead  =  Numbers(9)
      ErrSecondPumpPower =  (Secondary(SecondaryNum)%PumpTotRatedPower - &
                   PumpTotRatedFlowVol*PumpTotRatedHead/(PumpImpellerEfficiency*PumpMotorEfficiency))/ &
                   Secondary(SecondaryNum)%PumpTotRatedPower
      IF (ABS(ErrSecondPumpPower) > 0.35d0) & !generous diff allowed because comparing to my assumed impeller and motor effs
             CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
             ' Input value for '//TRIM(cNumericFieldNames(9))//' not consistent with input value for '//&
             TRIM(cNumericFieldNames(8))//'. '//TRIM(cNumericFieldNames(8))//' will be used')
    ELSEIF  (.NOT. lNumericBlanks(8)) THEN
      Secondary(SecondaryNum)%PumpTotRatedPower =  Numbers(8)
    ELSEIF  (.NOT. lNumericBlanks(9)) THEN
      PumpTotRatedHead  =  Numbers(9)
      Secondary(SecondaryNum)%PumpTotRatedPower =  PumpTotRatedFlowVol*PumpTotRatedHead/(PumpImpellerEfficiency*PumpMotorEfficiency)
    ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)// &
                 '", Either "'//TRIM(cNumericFieldNames(8))//'" OR "'// &
                 TRIM(cNumericFieldNames(9))//'" must be input.')
        ErrorsFound=.TRUE.
    END IF   !Either or pump power Input variations (head or power)



    ! Get pump drive type
    AlphaNum=5
    Secondary(SecondaryNum)%PumpControlType = SecPumpControlConstant  !default
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
      IF (SameString(Alphas(AlphaNum),'Constant')) THEN
        Secondary(SecondaryNum)%PumpControlType = SecPumpControlConstant
      ELSE IF (SameString(Alphas(AlphaNum),'Variable')) THEN
        Secondary(SecondaryNum)%PumpControlType = SecPumpControlVariable
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                 '"  '//TRIM(cAlphaFieldNames(AlphaNum))//' not recognized = '//TRIM(Alphas(AlphaNum)))
        CALL ShowContinueError('Check input value choices.')
        ErrorsFound=.TRUE.
      END IF   !Set PumpControlType
    END IF  ! blank on pump drive control type

    !  Print warning if Pump Control = Constant and Variable Speed Curve is specified.
    IF ((Secondary(SecondaryNum)%PumpControlType == SecPumpControlConstant) .AND. &
      (.NOT. lAlphaBlanks(AlphaNum+1))) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
               '", A '//TRIM(cAlphaFieldNames(AlphaNum+1))//' is specified even though '//TRIM(cAlphaFieldNames(AlphaNum))//&
               ' is "CONSTANT".')
        CALL ShowContinueError('The secondary loop pump(s) will be modeled as constant speed and the '//&
               TRIM(cAlphaFieldNames(AlphaNum+1))//' will be ignored.')
    END IF

  IF(Secondary(SecondaryNum)%PumpControlType == SecPumpControlConstant) THEN
    !Set incremental flow and power amounts for pump dispatch
    Secondary(SecondaryNum)%PumpIncrementFlowVol = PumpTotRatedFlowVol/NumPumps
    Secondary(SecondaryNum)%PumpIncrementPower = Secondary(SecondaryNum)%PumpTotRatedPower/NumPumps
  ELSE !Variable speed drive need to read in power curve
    AlphaNum = 6
    Secondary(SecondaryNum)%VarSpeedCurvePtr   = GetCurveIndex(Alphas(AlphaNum)) ! convert curve name to number
    IF (Secondary(SecondaryNum)%VarSpeedCurvePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found:'//TRIM(Alphas(AlphaNum)))
      ErrorsFound = .TRUE.
    END IF
    IF(.NOT. SameString(GetCurveType(Secondary(SecondaryNum)%VarSpeedCurvePtr),'CUBIC')) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
                           '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' object must be of type cubic.')
      ErrorsFound = .TRUE.
    END IF
  END IF ! input power conditions/levels for constant or variable speed pump drives

  !Default non-hermetic motor eff at 85% and all shaft power goes to heat in fluid
  ! In a semi-hermetic motor, assume all power to motor goes to heat in fluid
  Secondary(SecondaryNum)%PumpPowerToHeat=PumpMotorEfficiency
  NumNum = 11
  IF(.NOT. lNumericBlanks(NumNum))THEN
    IF((0.5d0 <= Numbers(NumNum)).AND.(1.0d0 >= Numbers(NumNum)))THEN
      Secondary(SecondaryNum)%PumpPowerToHeat=Numbers(NumNum)
    ELSE
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
               '" '//TRIM(cNumericFieldNames(NumNum))//&
               ' must be between 0.5 and 1.0. Default value of : '//TRIM(RoundSigDigits(PumpMotorEfficiency,3))//&
               ' will be used')
     END IF !range of pump moter heat to fluid
  END IF !blank input for pumppowertoheat

  !Distribution piping heat gain - optional
  !  Input UA and Zone containing the bulk of the secondary coolant distribution piping
  !  This Zone ID will be used to determine the temperature used for distribution piping heat gain.
  !  Zone Id is only required if Sum UA Distribution Piping >0.0
  !  Get the Zone node number from the zone name entered by the user
  Secondary(SecondaryNum)%SumUADistPiping=0.d0
  AlphaNum=7
  NumNum = 12
  IF(.NOT. lNumericBlanks(NumNum) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
    Secondary(SecondaryNum)%SumUADistPiping= Numbers(NumNum)
    Secondary(SecondaryNum)%DistPipeZoneNum = FindItemInList(Alphas(AlphaNum),Zone%Name,NumOfZones)
    Secondary(SecondaryNum)%DistPipeZoneNodeNum = GetSystemNodeNumberForZone(Alphas(AlphaNum))

    IF (Secondary(SecondaryNum)%DistPipeZoneNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                TRIM(Secondary(SecondaryNum)%Name)//  &
                '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not valid: '//TRIM(Alphas(AlphaNum)))
      ErrorsFound=.TRUE.
    ELSE
        RefrigPresentInZone(Secondary(SecondaryNum)%DistPipeZoneNum) = .TRUE.
    ENDIF

    IF ( Secondary(SecondaryNum)%DistPipeZoneNodeNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
               '" System Node Number not found for '//TRIM(cAlphaFieldNames(AlphaNum))// &
               ' = '//TRIM(Alphas(AlphaNum))//' even though '//TRIM(cNumericFieldNames(NumNum))//&
               ' is greater than zero. Distribution piping heat gain cannot be calculated unless a '//&
               ' controlled Zone (appear in a ZoneHVAC:EquipmentConnections object.)'//&
               ' is defined to determine the environmental temperature surrounding the piping.')
      ErrorsFound=.TRUE.
    ENDIF
  ELSEIF(.NOT. lNumericBlanks(NumNum) .AND. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
               '", '//TRIM(cAlphaFieldNames(AlphaNum))//' not found even though '//TRIM(cNumericFieldNames(NumNum))//&
               ' is greater than zero. Distribution piping heat gain will not be calculated unless a Zone'//&
               ' is defined to deterimine the environmental temperature surrounding the piping.')
  ELSEIF(lNumericBlanks(NumNum) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
               '", '//TRIM(cAlphaFieldNames(AlphaNum))//' will not be used and distribution piping heat gain will'//&
               ' not be calculated because '//TRIM(cNumericFieldNames(NumNum))//&
               ' was blank.')
  END IF  !distribution piping

  !Separator/receiver heat gain - optional
  !  Input UA and Zone containing the Separator/receiver
  !  This Zone ID will be used to determine the temperature used for Separator/receiver heat gain.
  !  Zone Id is only required if Sum UA Separator/receiver >0.0
  !  Get the Zone node number from the zone name entered by the user
  Secondary(SecondaryNum)%SumUAReceiver=0.d0
  AlphaNum=8
  NumNum = 13
  IF(.NOT. lNumericBlanks(NumNum) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
    Secondary(SecondaryNum)%SumUAReceiver= Numbers(NumNum)
    Secondary(SecondaryNum)%ReceiverZoneNum = FindItemInList(Alphas(AlphaNum),Zone%Name,NumOfZones)
    Secondary(SecondaryNum)%ReceiverZoneNodeNum = GetSystemNodeNumberForZone(Alphas(AlphaNum))

    IF (Secondary(SecondaryNum)%ReceiverZoneNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                TRIM(Secondary(SecondaryNum)%Name)//  &
                '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not valid: '//TRIM(Alphas(AlphaNum)))
      ErrorsFound=.TRUE.
    ELSE
        RefrigPresentInZone(Secondary(SecondaryNum)%ReceiverZoneNum) = .TRUE.
    ENDIF
    IF ( Secondary(SecondaryNum)%ReceiverZoneNodeNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
               '" System Node Number not found for '//TRIM(cAlphaFieldNames(AlphaNum))// &
               ' = '//TRIM(Alphas(AlphaNum))//' even though '//TRIM(cNumericFieldNames(NumNum))//&
               ' is greater than zero. Receiver heat gain cannot be calculated unless a '//&
               ' controlled Zone (appear in a ZoneHVAC:EquipmentConnections object.)'//&
               ' is defined to determine the environmental temperature surrounding the Receiver.')
      ErrorsFound=.TRUE.
    ENDIF
  ELSEIF(.NOT. lNumericBlanks(NumNum) .AND. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
               '", '//TRIM(cAlphaFieldNames(AlphaNum))//' not found even though '//TRIM(cNumericFieldNames(NumNum))//&
               ' is greater than zero. Receiver heat gain will not be calculated unless a Zone'//&
               ' is defined to deterimine the environmental temperature surrounding the Receiver.')
  ELSEIF(lNumericBlanks(NumNum) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
               '", '//TRIM(cAlphaFieldNames(AlphaNum))//' will not be used and Receiver heat gain will'//&
               ' not be calculated because '//TRIM(cNumericFieldNames(NumNum))//&
               ' was blank.')
  END IF  !Receiver

NumNum = 14
Secondary(SecondaryNum)%ChillerRefInventory = 0.d0
IF (.NOT. lNumericBlanks(NumNum))Secondary(SecondaryNum)%ChillerRefInventory = Numbers(NumNum)
IF (Secondary(SecondaryNum)%ChillerRefInventory < 0.0d0) THEN
    Secondary(SecondaryNum)%ChillerRefInventory = 0.d0
    CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
               '", The value specified for '//TRIM(cNumericFieldNames(NumNum))//' is less than zero. The default'//&
               ' value of zero will be used.')
END IF

AlphaNum=9
IF (.NOT. lAlphaBlanks(AlphaNum))  Secondary(SecondaryNum)%EndUseSubcategory = Alphas(AlphaNum)

!Error checks on secondary loop:
! Note, rated capacities can be far off from operating capacities, but rough checks here
!       (don't include dist piping or receiver heat gains).
! Load limit logic here (maxvolflow and maxload used in calcs later)
Secondary(SecondaryNum)%MaxVolFlow = MIN(SecondaryFlowVolRated,PumpTotRatedFlowVol)
NominalSecondaryRefLoad = NominalTotalCaseCap + NominalTotalWalkInCap + &
                          Secondary(SecondaryNum)%PumpTotRatedPower

IF( Secondary(SecondaryNum)%FluidType == SecFluidTypeAlwaysLiquid) THEN
  IF(TBrineOutRated > (Secondary(SecondaryNum)%TMinNeeded + 0.5d0)) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
          ' The design brine temperature to the refrigeration loads: '//&
          TRIM(RoundSigDigits(TBrineOutRated,1))//' ;')
      CALL ShowContinueError(' is greater than the design inlet temperature for at least one of the cases or walkins: '//&
          TRIM(RoundSigDigits(Secondary(SecondaryNum)%TMinNeeded,1)))
      CALL ShowContinueError(' Compare your Approach and Evaporating Temperature to'//&
                              ' the design inlet temperatures needed for the loads.')
      !ErrorsFound = .TRUE.
  END IF !Tbrine out warning
  CapacityAtMaxVolFlow = Secondary(SecondaryNum)%MaxVolFlow * Secondary(SecondaryNum)%HeatExchangeEta* &
                       (cpBrineRated * DensityBrineRated) * &
                       (TbrineInRated - Secondary(SecondaryNum)%TEvapDesign)
  Secondary(SecondaryNum)%MaxLoad = MIN(Secondary(SecondaryNum)%CoolingLoadRated,CapacityAtMaxVolFlow)
  DeltaCap1 = ABS((Secondary(SecondaryNum)%CoolingLoadRated -CapacityAtMaxVolFlow)/&
                 Secondary(SecondaryNum)%CoolingLoadRated)
  IF(DeltaCap1 > (0.3d0))THEN  !diff between chiller rating and capacity at max flow > 30%
    CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
             '" You may wish to check the system sizing. '//&
             '  The nominal secondary loop heat exchanger capacity is '//&
             TRIM(RoundSigDigits(Secondary(SecondaryNum)%CoolingLoadRated,0))// &
             ' But the capacity based upon the maximum flow rate is '//&
             TRIM(RoundSigDigits(CapacityAtMaxVolFlow,0)))
  END IF ! DeltaCap1 > .3
ELSE ! Fluid type phase change                !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  IF(lNumericBlanks(1)) THEN   ! Chiller/evaporator capacity was not specified
    IF(lNumericBlanks(7)) THEN ! Pump power was not input, calc based on flow and head
      !need to refine because capacity calculated, but needs to include pump power (which was prev
      !   estimated based upon capacity which had been estimated as sum of case and walk-in capacities)
      PumpTotRatedFlowVol =  NominalSecondaryRefLoad * Secondary(SecondaryNum)%CircRate / &
                        (DensityPhaseChange * DeltaHPhaseChange)
      Secondary(SecondaryNum)%PumpTotRatedPower =  PumpTotRatedFlowVol*PumpTotRatedHead/&
                                                   (PumpImpellerEfficiency*PumpMotorEfficiency)
      !need to recalc nominal load with new pump power value
      NominalSecondaryRefLoad =  NominalTotalCaseCap + NominalTotalWalkInCap + &
                          Secondary(SecondaryNum)%PumpTotRatedPower
      IF(Secondary(SecondaryNum)%PumpControlType == SecPumpControlConstant) THEN
        !Set incremental flow and power amounts for pump dispatch
        Secondary(SecondaryNum)%PumpIncrementFlowVol = PumpTotRatedFlowVol/NumPumps
        Secondary(SecondaryNum)%PumpIncrementPower = Secondary(SecondaryNum)%PumpTotRatedPower/NumPumps
      END IF ! constant speed pump
    END IF   ! Pump power was not specified
    Secondary(SecondaryNum)%CoolingLoadRated = NominalSecondaryRefLoad
  END IF ! Chiller/evap capacity was not specified
  Secondary(SecondaryNum)%MaxLoad = Secondary(SecondaryNum)%CoolingLoadRated
END IF ! SecFluidType

DeltaCap2 = ABS((Secondary(SecondaryNum)%CoolingLoadRated -NominalSecondaryRefLoad)/&
                 Secondary(SecondaryNum)%CoolingLoadRated)
IF(DeltaCap2 > (0.3d0))THEN  !diff between chiller rating and sum of nominal loads > 30%
    CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
             '" You may wish to check the system sizing. Total nominal refrigerating load is '//&
             TRIM(RoundSigDigits(NominalSecondaryRefLoad,0))// &
             ' (Including cases, walk-ins, and pump heat). '//&
             ' The nominal secondary loop heat exchanger capacity is '// &
             TRIM(RoundSigDigits(Secondary(SecondaryNum)%CoolingLoadRated,0)))
END IF
!compare rated xt xchanger brine flow to the total rated pump flow
IF(SecondaryFlowVolRated > (1.1d0 * PumpTotRatedFlowVol)) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(Secondary(SecondaryNum)%Name)//&
             '" You may wish to check the pump sizing. Total nominal brine flow is '//&
             TRIM(RoundSigDigits(SecondaryFlowVolRated,0))// &
             ' m3/s, but the total nominal pump flow rate is:  '//&
             TRIM(RoundSigDigits(PumpTotRatedFlowVol,0))// &
             ' m3/s. ')
END IF

END DO  ! Secondary Loops
END IF  !(  IF (NumSimulationSecondarySystems > 0)


!************ END SECONDARY SYSTEM INPUT  **************

!************ START Compressor INPUT  **************

  CurrentModuleObject='Refrigeration:Compressor'
  DO CompNum=1,NumSimulationCompressors
    CALL GetObjectItem(CurrentModuleObject,CompNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus,&
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),Compressor%Name,CompNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    Compressor(CompNum)%Name = Alphas(1)

    Compressor(CompNum)%ElecPowerCurvePtr = GetCurveIndex(Alphas(2)) ! convert curve name to number
    IF ((.NOT. lAlphaBlanks(2)) .AND. Compressor(CompNum)%ElecPowerCurvePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Compressor(CompNum)%Name)//&
           '", invalid  '//TRIM(cAlphaFieldNames(2))//' not found = '//TRIM(Alphas(2)))
      ErrorsFound = .TRUE.
    END IF

    Compressor(CompNum)%CapacityCurvePtr = GetCurveIndex(Alphas(3)) ! convert curve name to number
    IF ((.NOT. lAlphaBlanks(3)) .AND. Compressor(CompNum)%CapacityCurvePtr == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Compressor(CompNum)%Name)//&
           '", invalid  '//TRIM(cAlphaFieldNames(3))//' not found = '//TRIM(Alphas(3)))
      ErrorsFound = .TRUE.
    END IF

    ! Get superheat rating type (Either N1 or N2 Must be input)
    IF (  ((.NOT. lNumericBlanks(1)) .AND. (.NOT. lNumericBlanks(2))) .OR. &
          (lNumericBlanks(1) .AND. lNumericBlanks(2))  )     THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Compressor(CompNum)%Name)//&
                 '"One, and Only One of '//TRIM(cNumericFieldNames(1))//' or '//TRIM(cNumericFieldNames(2)))
         CALL ShowContinueError('Must Be Entered. Check input value choices.')
         ErrorsFound=.TRUE.
    ELSE IF (.NOT. lNumericBlanks(1))THEN
        Compressor(CompNum)%SuperheatRatingType = RatedSuperheat
        Compressor(CompNum)%RatedSuperheat = Numbers(1)
    ELSE IF (.NOT. lNumericBlanks(2))THEN
        Compressor(CompNum)%SuperheatRatingType = RatedReturnGasTemperature
        Compressor(CompNum)%RatedSuperheat = Numbers(2)
    END IF   !Set SuperheatRatingType

    ! Get subcool rating type (Either N3 or N4 Must be input)
    IF (  ((.NOT. lNumericBlanks(3)) .AND. (.NOT. lNumericBlanks(4))) .OR. &
          (lNumericBlanks(3) .AND. lNumericBlanks(4))  )     THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Compressor(CompNum)%Name)//&
                 '" One, and Only One of '//TRIM(cNumericFieldNames(3))//' or '//TRIM(cNumericFieldNames(4)))
         CALL ShowContinueError('Must Be Entered. Check input value choices.')
         ErrorsFound=.TRUE.
    ELSE IF (.NOT. lNumericBlanks(3))THEN
        Compressor(CompNum)%SubcoolRatingType = RatedLiquidTemperature
        Compressor(CompNum)%RatedSubcool = Numbers(3)
    ELSE IF (.NOT. lNumericBlanks(4))THEN
        Compressor(CompNum)%SubcoolRatingType = RatedSubcooling
        Compressor(CompNum)%RatedSubcool = Numbers(4)
    END IF   !Set SubcoolRatingType

    Compressor(CompNum)%EndUseSubcategory ='General'
    IF (.NOT. lAlphaBlanks(4))  Compressor(CompNum)%EndUseSubcategory = Alphas(4)

    !  If the compressor is a transcritical CO compressor, get transcritical power and capacity curves
    IF (SameString(Alphas(5),'Transcritical')) THEN     ! Mode of Operation = Transcritical
      Compressor(CompNum)%TransFlag = .TRUE.
      Compressor(CompNum)%TransElecPowerCurvePtr = GetCurveIndex(Alphas(6)) ! convert curve name to number
      IF (lAlphaBlanks(6) .AND. Compressor(CompNum)%TransElecPowerCurvePtr == 0) THEN
        CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'='//TRIM(Compressor(CompNum)%Name)//': ' &
             //TRIM(cAlphaFieldNames(6))//' not found.')
        ErrorsFound = .TRUE.
      END IF
      Compressor(CompNum)%TransCapacityCurvePtr = GetCurveIndex(Alphas(7)) ! convert curve name to number
      IF (lAlphaBlanks(7) .AND. Compressor(CompNum)%TransCapacityCurvePtr == 0) THEN
        CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'='//TRIM(Compressor(CompNum)%Name)//': ' &
             //TRIM(cAlphaFieldNames(7))//' not found.')
        ErrorsFound = .TRUE.
      END IF
    ELSE IF ((SameString(Alphas(5),'Subcritical')).OR.(lAlphaBlanks(5))) THEN     ! Mode of Operation = Subcritical
      Compressor(CompNum)%TransFlag = .FALSE.
      IF ((.NOT.lAlphaBlanks(6)).OR.(.NOT.lAlphaBlanks(7))) THEN     ! Transcritical compressor curves specified for subcritical compressor
        CALL ShowWarningError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'='//TRIM(Compressor(CompNum)%Name)// &
             ' is specified to be a subcritical compressor, however transcritical compressor curve(s) are given.')
        CALL ShowContinueError('The compressor will be modeled as a subcritical compressor and the transcritical '// &
             'compressor curve(s) will be ignored.')
      END IF
    ELSE     ! Invalid Mode of Operation
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//': '//TRIM(cAlphaFieldNames(5))// &
                           ' for '//TRIM(Compressor(CompNum)%Name)//'='//TRIM(Alphas(5))// &
                           ' is invalid. Valid choices are "Subcritical" or "Transcritical".')
      ErrorsFound = .TRUE.
    END IF

  END DO   ! RefrigCompressor

  !************ END Compressor INPUT         **************

  !************ START Subcooler INPUT        **************
  IF (NumSimulationSubcoolers > 0) THEN
  CurrentModuleObject='Refrigeration:Subcooler'
  NumSimulationMechSubcoolers=0
  DO SubcoolerNum=1,NumSimulationSubcoolers
    CALL GetObjectItem(CurrentModuleObject,SubcoolerNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),Subcooler%Name,SubcoolerNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    Subcooler(SubcoolerNum)%Name = Alphas(1)

    ! Get subcooler type
    Subcooler(SubcoolerNum)%subcoolertype = LiquidSuction      !default subcooler type
    IF (SameString(Alphas(2),'Mechanical')) THEN      !set subcooler type
      Subcooler(SubcoolerNum)%subcoolertype = Mechanical
      NumSimulationMechSubcoolers=NumSimulationMechSubcoolers + 1
    ELSE IF (SameString(Alphas(2),'LiquidSuction')) THEN
      Subcooler(SubcoolerNum)%subcoolertype = LiquidSuction
    ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Subcooler(SubcoolerNum)%Name)//&
                 '", '//TRIM(cAlphaFieldNames(2))//' not recognized = '//TRIM(Alphas(2)))
        CALL ShowContinueError('Check input value choices.')
        ErrorsFound=.TRUE.
    END IF   !Set Subcooler Type

    SELECT CASE(Subcooler(SubcoolerNum)%subcoolertype)

    CASE (LiquidSuction)
      Subcooler(SubcoolerNum)%LiqSuctDesignDelT=10.d0   !default value
      IF (.NOT. lNumericBlanks(1))Subcooler(SubcoolerNum)%LiqSuctDesignDelT= Numbers(1)
      IF (Subcooler(SubcoolerNum)%LiqSuctDesignDelT < 0.0d0 ) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Subcooler(SubcoolerNum)%Name)//&
          '" '//TRIM(cNumericFieldNames(1))//' cannot be less than zero.')
          ErrorsFound = .TRUE.
      END IF

      IF (.NOT. lNumericBlanks(2)) THEN
        Subcooler(SubcoolerNum)%LiqSuctDesignTliqIn= Numbers(2)
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Subcooler(SubcoolerNum)%Name)//&
          '" '//TRIM(cNumericFieldNames(2))//' must be specified.')
          ErrorsFound = .TRUE.
      END IF

      IF (.NOT. lNumericBlanks(3)) THEN
        Subcooler(SubcoolerNum)%LiqSuctDesignTvapIn= Numbers(3)
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Subcooler(SubcoolerNum)%Name)//&
          '" '//TRIM(cNumericFieldNames(3))//' must be specified.')
          ErrorsFound = .TRUE.
      END IF
      IF (Subcooler(SubcoolerNum)%LiqSuctDesignTvapIn > Subcooler(SubcoolerNum)%LiqSuctDesignTliqIn) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Subcooler(SubcoolerNum)%Name)//&
          '" '//TRIM(cNumericFieldNames(3))//' cannot be greater than '//TRIM(cNumericFieldNames(2))//'.')
          ErrorsFound = .TRUE.
      END IF !error check

    CASE (Mechanical)
      Subcooler(SubcoolerNum)%MechSourceSys=Alphas(3)
      !Error check on system name comes later after systems have been read

      IF (.NOT. lNumericBlanks(4)) THEN
        Subcooler(SubcoolerNum)%MechControlTliqOut= Numbers(4)
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Subcooler(SubcoolerNum)%Name)//&
          '" '//TRIM(cNumericFieldNames(4))//' must be specified.')
          ErrorsFound = .TRUE.
      END IF !error check

    END SELECT

  END DO   ! Subcooler Input
  END IF   ! If there are subcoolers

  ! ********END SUBCOOLER INPUTS ************

!**** Read TransferLoad Lists **********************************************************
  IF(NumSimulationTransferLoadLists > 0) THEN
  CurrentModuleObject='Refrigeration:TransferLoadList'
  DO ListNum=1,NumSimulationTransferLoadLists
    CALL GetObjectItem(CurrentModuleObject,ListNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),TransferLoadList%Name,ListNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF

    TransferLoadList(ListNum)%Name=Alphas(1)

    ! Transfer load list alphas include TransferLoadList name and one name for each Secondary or Cascade Condenser in list
    ! below allocates larger than needed (each allocated to sum of both), but avoids two loops through input fields
    NumTotalLoadsOnList  = NumAlphas - 1
    IF(.NOT. ALLOCATED(TransferLoadList(ListNum)%CascadeLoadItemNum))&
             ALLOCATE(TransferLoadList(ListNum)%CascadeLoadItemNum(NumTotalLoadsOnList))
    IF(.NOT. ALLOCATED(TransferLoadList(ListNum)%SecondaryItemNum))&
              ALLOCATE(TransferLoadList(ListNum)%SecondaryItemNum(NumTotalLoadsOnList))

    NumSecondarysOnList   = 0
    NumCascadeLoadsOnList = 0
    DO NumLoad = 1, NumTotalLoadsOnList
      AlphaListNum= 1 + NumLoad
      LoadCascadeNum   =  0
      LoadSecondaryNum =  0
      IF(NumRefrigCondensers > 0) &
             LoadCascadeNum   =  FindItemInList(Alphas(AlphaListNum),Condenser%Name,NumRefrigCondensers)
      IF(NumSimulationSecondarySystems > 0) &
             LoadSecondaryNum =  FindItemInList(Alphas(AlphaListNum),Secondary%Name,NumSimulationSecondarySystems)
       IF((LoadCascadeNum == 0) .AND. (LoadSecondaryNum == 0)) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                             TRIM(cAlphaFieldNames(AlphaListNum))//'" : has an invalid '//&
                             'value of '//TRIM(Alphas(AlphaListNum)))
          ErrorsFound = .TRUE.
       ELSEIF((LoadCascadeNum /= 0) .AND. (LoadSecondaryNum /= 0)) THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                             TRIM(cAlphaFieldNames(AlphaListNum))//'" : has a non-unique name '//&
                             ': '//TRIM(Alphas(AlphaListNum)))
         ErrorsFound = .TRUE.
       ELSEIF (LoadCascadeNum /= 0) THEN
         IF(Condenser(LoadCascadeNum)%CondenserType /= RefrigCondenserTypeCascade) THEN
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)&
                             //'" : has a condenser listed as a transfer load that is not a cascade condenser: '//&
                             TRIM(Alphas(AlphaListNum)))
           ErrorsFound = .TRUE.
         ELSE
           NumCascadeLoadsOnList = NumCascadeLoadsOnList + 1
           TransferLoadList(ListNum)%CascadeLoadItemNum(NumCascadeLoadsOnList) = LoadCascadeNum
         END IF ! /= condenser cascade type
       ELSEIF (LoadSecondaryNum /= 0) THEN
           NumSecondarysOnList = NumSecondarysOnList + 1
           TransferLoadList(ListNum)%SecondaryItemNum(NumSecondarysOnList) = LoadSecondaryNum
       END IF
      TransferLoadList(ListNum)%NumSecondarys   = NumSecondarysOnList
      TransferLoadList(ListNum)%NumCascadeLoads = NumCascadeLoadsOnList
    END DO !Num Total Loads on List
  END DO   !ListNum=1,NumSimulationTransferLoadLists
  END IF   !(NumSimulationTransferLoadLists > 0)


 !**** End read transfer load Lists **********************************************************



!**** Read Compressor Lists **********************************************************
  CurrentModuleObject='Refrigeration:CompressorList'
  DO ListNum=1,NumCompressorLists
    CALL GetObjectItem(CurrentModuleObject,ListNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CompressorLists(ListNum)%NumCompressors=NumAlphas - 1
    CALL VerifyName(Alphas(1),CompressorLists%Name,ListNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    CompressorLists(ListNum)%Name=Alphas(1)
    IF(.NOT. ALLOCATED(CompressorLists(ListNum)%CompItemNum))&
             ALLOCATE(CompressorLists(ListNum)%CompItemNum(CompressorLists(ListNum)%NumCompressors))

    DO CompIndex = 1, CompressorLists(ListNum)%NumCompressors
      AlphaListNum = CompIndex+1     !same as do loop from 2 to end of list
      IF(.NOT. lAlphaBlanks(AlphaListNum)) THEN
        CompressorLists(ListNum)%CompItemNum(CompIndex)= &
                       FindItemInList(Alphas(AlphaListNum),Compressor%Name,NumSimulationCompressors)
        IF(CompressorLists(ListNum)%CompItemNum(CompIndex) == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)// &
                '="'//TRIM(CompressorLists(ListNum)%Name)//'", has an invalid '//&
                TRIM(cAlphaFieldNames(AlphaListNum))//' defined as '//TRIM(Alphas(AlphaListNum)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END DO !NumCompressors in CompressorList

  END DO   !NumCompressorLists

  ! ********READ REFRIGERATION SYSTEMS  ***********

  CurrentModuleObject='Refrigeration:System'
  DO RefrigSysNum=1,NumRefrigSystems

    CALL GetObjectItem(CurrentModuleObject,RefrigSysNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK =.FALSE.
    IsBlank =.FALSE.

    CALL VerifyName(Alphas(1),System%Name,RefrigSysNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    System(RefrigSysNum)%Name = Alphas(1)

!Read all loads on this System: cases, walk-ins, cascade loads, and secondary loops
    IF(lAlphaBlanks(2) .AND. lAlphaBlanks(3) ) THEN
       !No cases, walkins, cascade loads, or secondary loops specified, ie, System has no load
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)// &
                    '", has no loads, must have at least one of: '//TRIM(cAlphaFieldNames(2))// &
                    ' or '//TRIM(cAlphaFieldNames(3))//' objects attached.')
        ErrorsFound = .TRUE.
    END IF
    NumCases = 0
    System(RefrigSysNum)%NumCases = 0
    NumCoils = 0
    System(RefrigSysNum)%NumCoils = 0
    NumWalkIns = 0
    System(RefrigSysNum)%NumWalkIns = 0
    NumSecondary = 0
    System(RefrigSysNum)%NumSecondarys = 0
    NumCascadeLoad = 0
    System(RefrigSysNum)%NumCascadeLoads = 0
    System(RefrigSysNum)%NumNonCascadeLoads = 0
    NominalTotalCaseCap      = 0.0d0
    NominalTotalCoilCap      = 0.0d0
    NominalTotalWalkInCap    = 0.0d0
    NominalTotalSecondaryCap = 0.0d0
    NominalTotalCoolingCap   = 0.0d0
    NominalTotalCascadeLoad  = 0.0d0
    System(RefrigSysNum)%RefInventory=0.0d0

   !   Check for case or walkin or CaseAndWalkInList names
    AlphaNum = 2
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN

    ! Entry for Alphas(AlphaNum) can be either a Case, WalkIn or CaseAndWalkInList name
      CaseAndWalkInListNum=0
      CaseNum=0
      WalkInNum=0
      CoilNum=0
      IF(NumSimulationCaseAndWalkInLists > 0) &
            CaseAndWalkInListNum=FindItemInList(Alphas(AlphaNum),CaseAndWalkInList%Name,NumSimulationCaseAndWalkInLists)
      IF(NumSimulationCases > 0)  CaseNum=  FindItemInList(Alphas(AlphaNum),RefrigCase%Name,NumSimulationCases)
      IF(NumSimulationWalkIns > 0)WalkInNum=FindItemInList(Alphas(AlphaNum),WalkIn%Name,NumSimulationWalkIns)
      IF(NumSimulationRefrigAirChillers > 0)        &
         CoilNum=  FindItemInList(Alphas(AlphaNum),WarehouseCoil%Name,NumSimulationRefrigAirChillers)
      NumNameMatches = 0
      IF(CaseAndWalkInListNum /= 0)NumNameMatches = NumNameMatches +1
      IF(CaseNum /= 0)       NumNameMatches = NumNameMatches +1
      IF(WalkInNum /= 0)     NumNameMatches = NumNameMatches +1
      IF(CoilNum /= 0)       NumNameMatches = NumNameMatches +1

      IF (NumNameMatches /= 1) THEN !name must uniquely point to a list or a single case or walkin or coil
          ErrorsFound = .TRUE.
          IF(NumNameMatches == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                   '", has an invalid '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          ELSEIF(NumNameMatches > 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                    TRIM(System(RefrigSysNum)%Name)//'",  has a non-unique name '//&
                    'that could be either a '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          END IF !num matches = 0 or > 1
      ELSEIF(CaseAndWalkInListNum /= 0)  THEN  !Name points to a CaseAndWalkInList
         NumCases   = CaseAndWalkInList(CaseAndWalkInListNum)%NumCases
         NumWalkIns = CaseAndWalkInList(CaseAndWalkInListNum)%NumWalkIns
         NumCoils   = CaseAndWalkInList(CaseAndWalkInListNum)%NumCoils
         System(RefrigSysNum)%NumCases   = NumCases
         System(RefrigSysNum)%NumWalkIns = NumWalkIns
         System(RefrigSysNum)%NumCoils   = NumCoils
         IF(NumCases > 0) THEN
           IF(.NOT. ALLOCATED(System(RefrigSysNum)%CaseNum))ALLOCATE(System(RefrigSysNum)%CaseNum(NumCases))
           System(RefrigSysNum)%CaseNum(1:NumCases) = CaseAndWalkInList(CaseAndWalkInListNum)%CaseItemNum(1:NumCases)
         END IF
         IF(NumCoils > 0) THEN
           IF(.NOT. ALLOCATED(System(RefrigSysNum)%CoilNum))ALLOCATE(System(RefrigSysNum)%CoilNum(NumCoils))
           System(RefrigSysNum)%CoilNum(1:NumCoils) = CaseAndWalkInList(CaseAndWalkInListNum)%CoilItemNum(1:NumCoils)
         END IF
         IF(NumWalkIns > 0) THEN
           IF(.NOT. ALLOCATED(System(RefrigSysNum)%WalkInNum))ALLOCATE(System(RefrigSysNum)%WalkInNum(NumWalkIns))
           System(RefrigSysNum)%WalkInNum(1:NumWalkIns) = CaseAndWalkInList(CaseAndWalkInListNum)%WalkInItemNum(1:NumWalkIns)
         END IF
      ELSEIF (CaseNum /= 0) THEN     !Name points to a case
         NumCases = 1
         System(RefrigSysNum)%NumCases = 1
         IF(.NOT. ALLOCATED(System(RefrigSysNum)%CaseNum)) ALLOCATE(System(RefrigSysNum)%CaseNum(NumCases))
         System(RefrigSysNum)%CaseNum(NumCases)=CaseNum
      ELSEIF (CoilNum /= 0) THEN     !Name points to a coil
         NumCoils = 1
         System(RefrigSysNum)%NumCoils = 1
         IF(.NOT. ALLOCATED(System(RefrigSysNum)%CoilNum)) ALLOCATE(System(RefrigSysNum)%CoilNum(NumCoils))
         System(RefrigSysNum)%CoilNum(NumCoils)=CoilNum
      ELSEIF (WalkInNum /= 0) THEN   !Name points to a walkin
         NumWalkIns = 1
         System(RefrigSysNum)%NumWalkIns = 1
         IF(.NOT. ALLOCATED(System(RefrigSysNum)%WalkInNum)) &
                  ALLOCATE(System(RefrigSysNum)%WalkInNum(NumWalkIns))
         System(RefrigSysNum)%WalkInNum(NumWalkIns)=WalkInNum
      END IF  !NumNameMatches /= 1
    END IF !blank input for cases, walkins, or caseandwalkinlist

   IF(NumCases > 0) THEN
      ! Find lowest design evap T
       ! Sum rated capacity of all cases on system
       DO CaseIndex = 1, NumCases
          !mark all cases on system as used by this system - checking for unused or non-unique cases
          CaseNum=System(RefrigSysNum)%CaseNum(CaseIndex)
          RefrigCase(CaseNum)%NumSysAttach = RefrigCase(CaseNum)%NumSysAttach + 1
          NominalTotalCaseCap = NominalTotalCaseCap + RefrigCase(CaseNum)%DesignRatedCap
          System(RefrigSysNum)%RefInventory=System(RefrigSysNum)%RefInventory + &
                                              RefrigCase(Casenum)%DesignRefrigInventory
          IF(CaseIndex == 1) THEN  !look for lowest case design evap T for system
              System(RefrigSysNum)%TEvapDesign=RefrigCase(CaseNum)%EvapTempDesign
          ELSE
              System(RefrigSysNum)%TEvapDesign = &
                 MIN(RefrigCase(CaseNum)%EvapTempDesign,System(RefrigSysNum)%TEvapDesign)
          END IF
       END DO  !CaseIndex=1,NumCases
       System(RefrigSysNum)%NumNonCascadeLoads = System(RefrigSysNum)%NumNonCascadeLoads + System(RefrigSysNum)%NumCases
   END IF  !Numcases > 0

   IF(NumCoils > 0) THEN
      ! Find lowest design evap T
       ! Sum rated capacity of all Coils on system
       DO CoilIndex = 1, NumCoils
          !mark all Coils on system as used by this system - checking for unused or non-unique Coils
          CoilNum=System(RefrigSysNum)%CoilNum(CoilIndex)
          WarehouseCoil(CoilNum)%NumSysAttach = WarehouseCoil(CoilNum)%NumSysAttach + 1
          NominalTotalCoilCap = NominalTotalCoilCap + WarehouseCoil(CoilNum)%RatedSensibleCap
          System(RefrigSysNum)%RefInventory=System(RefrigSysNum)%RefInventory + &
                                              WarehouseCoil(Coilnum)%DesignRefrigInventory
          IF((CoilIndex == 1) .AND. (System(RefrigSysNum)%NumCases ==0)) THEN  !look for lowest Coil design evap T for system
              System(RefrigSysNum)%TEvapDesign=WarehouseCoil(CoilNum)%TEvapDesign
          ELSE
              System(RefrigSysNum)%TEvapDesign = &
                 MIN(WarehouseCoil(CoilNum)%TEvapDesign,System(RefrigSysNum)%TEvapDesign)
          END IF
       END DO  !CoilIndex=1,NumCoils
       System(RefrigSysNum)%NumNonCascadeLoads = System(RefrigSysNum)%NumNonCascadeLoads + System(RefrigSysNum)%NumCoils
    END IF  !NumCoils > 0

    IF (NumWalkIns > 0) THEN
      DO WalkInIndex = 1, NumWalkIns
         WalkInID=System(RefrigSysNum)%WalkInNum(WalkInIndex)
         !mark all WalkIns on rack as used by this system (checking for unused or non-unique WalkIns)
         WalkIn(WalkInID)%NumSysAttach = WalkIn(WalkInID)%NumSysAttach + 1
         NominalTotalWalkInCap = NominalTotalWalkInCap + WalkIn(WalkInID)%DesignRatedCap
         System(RefrigSysNum)%RefInventory=System(RefrigSysNum)%RefInventory + &
                                              WalkIn(WalkInID)%DesignRefrigInventory
         !Defrost capacity is treated differently by compressor racks and detailed systems,
         !  so this value may be adjusted (or warnings issued) after the walkin is assigned
         !  to either the rack or system.
         !for walkins served by detailed system, need capacity for both fluid and electric types.
         IF (WalkIn( WalkInID)%DefrostCapacity <= -98.d0) THEN
            ! - 99 used as a flag for blank input error message for detailed systems
            CALL ShowSevereError(RoutineName//'Refrigeration:WalkIn="'//TRIM( WalkIn( WalkInID)%Name)//&
                           '", Defrost capacity must be greater than or equal to 0 W' //&
                           ' for electric and hotfluid defrost types')
            ErrorsFound = .TRUE.
         END IF
         ! Find design evaporating temperature for system by getting min design evap for ALL loads
         IF ((WalkInIndex == 1) .AND. (System(RefrigSysNum)%NumCases ==0) .AND. &
                                      (System(RefrigSysNum)%NumCoils ==0) )THEN
                !note use walk in index, not walkinid here to get
                !first walkin on this suction group/system
           System(RefrigSysNum)%TEvapDesign=WalkIn(WalkInID)%TEvapDesign
         ELSE
           System(RefrigSysNum)%TEvapDesign=MIN(WalkIn(WalkInID)%TEvapDesign,System(RefrigSysNum)%TEvapDesign)
         END IF
       END DO  !WalkInIndex=1,NumWalkIns
       System(RefrigSysNum)%NumNonCascadeLoads = System(RefrigSysNum)%NumNonCascadeLoads + System(RefrigSysNum)%NumWalkIns
    END IF  !numwalkins > 0

    AlphaNum = 3
    ! Read Transfer Loads (Secondary and Cascade) assignments for this System ,
    !     already allow more than one mech subcooler to load onto a system so they don't need to go in list
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN

    ! Entry for Alphas(AlphaNum) can be either a Secondary, CascadeLoad name or a TransferLoadList name
      TransferLoadListNum=0
      SecondaryNum=0
      CascadeLoadNum=0
      IF(NumSimulationTransferLoadLists > 0) &
             TransferLoadListNum=FindItemInList(Alphas(AlphaNum),TransferLoadList%Name,NumSimulationTransferLoadLists)
      IF(NumSimulationSecondarySystems > 0) &
             SecondaryNum=FindItemInList(Alphas(AlphaNum),Secondary%Name,NumSimulationSecondarySystems)
      IF(NumRefrigCondensers > 0) &
             CascadeLoadNum=FindItemInList(Alphas(AlphaNum),Condenser%Name,NumRefrigCondensers)
      NumNameMatches = 0
      IF(TransferLoadListNum /= 0)NumNameMatches = NumNameMatches +1
      IF(SecondaryNum /= 0)       NumNameMatches = NumNameMatches +1
      IF(CascadeLoadNum /= 0)     NumNameMatches = NumNameMatches +1

      IF (NumNameMatches /= 1) THEN !name must uniquely point to a list or a single transfer load
          ErrorsFound = .TRUE.
          IF(NumNameMatches == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                   '", has an invalid '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          ELSEIF(NumNameMatches > 1) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//&
                    TRIM(System(RefrigSysNum)%Name)//'", has a non-unique name '//&
                    'that could be either a '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          END IF !num matches = 0 or > 1
      ELSEIF(TransferLoadListNum /= 0)  THEN  !Name points to a transferLoad list
         NumSecondary = TransferLoadList(TransferLoadListNum)%NumSecondarys
         NumCascadeLoad = TransferLoadList(TransferLoadListNum)%NumCascadeLoads
         System(RefrigSysNum)%NumSecondarys = NumSecondary
         System(RefrigSysNum)%NumCascadeLoads = NumCascadeLoad
         IF(.NOT. ALLOCATED(System(RefrigSysNum)%SecondaryNum))ALLOCATE(System(RefrigSysNum)%SecondaryNum(NumSecondary))
         System(RefrigSysNum)%SecondaryNum(1:NumSecondary) = TransferLoadList(TransferLoadListNum)%SecondaryItemNum(1:NumSecondary)
         IF(.NOT. ALLOCATED(System(RefrigSysNum)%CascadeLoadNum))ALLOCATE(System(RefrigSysNum)%CascadeLoadNum(NumCascadeLoad))
         System(RefrigSysNum)%CascadeLoadNum(1:NumCascadeLoad) = &
                                TransferLoadList(TransferLoadListNum)%CascadeLoadItemNum(1:NumCascadeLoad)
      ELSEIF (SecondaryNum /= 0) THEN     !Name points to a secondary loop load
         NumSecondary = 1
         System(RefrigSysNum)%NumSecondarys = 1
         IF(.NOT. ALLOCATED(System(RefrigSysNum)%SecondaryNum)) ALLOCATE(System(RefrigSysNum)%SecondaryNum(NumSecondary))
         System(RefrigSysNum)%Secondarynum(NumSecondary)=SecondaryNum
      ELSEIF (CascadeLoadNum /= 0) THEN   !Name points to a cascade condenser load
         NumCascadeLoad = 1
         System(RefrigSysNum)%NumCascadeLoads = 1
         IF(.NOT. ALLOCATED(System(RefrigSysNum)%CascadeLoadNum)) &
                  ALLOCATE(System(RefrigSysNum)%CascadeLoadNum(NumCascadeLoad))
         System(RefrigSysNum)%CascadeLoadnum(NumCascadeLoad)=CascadeLoadNum
      END IF  !NumNameMatches /= 1

 System(RefrigSysNum)%CoilFlag = .FALSE.
 ! Now need to loop through all transfer loads to see if they change the minimum required system evaporating temperature
    IF (NumSecondary > 0) THEN
         DO SecondaryIndex = 1, NumSecondary
           SecondaryID=System(RefrigSysNum)%SecondaryNum(SecondaryIndex)
           IF(SecondaryIndex == 1)THEN ! check for consistency of loads (coils calc on sys time step, all others on zone time step)
             IF(Secondary(SecondaryID)%CoilFlag)System(RefrigSysNum)%CoilFlag = .TRUE.
           ELSEIF(Secondary(SecondaryID)%CoilFlag .neqv. System(RefrigSysNum)%CoilFlag) THEN
               CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//  &
                '="'//TRIM(System(RefrigSysNum)%Name)//&
                '", Serves an inconsistent mixture of loads. Coil-type loads are served on a '//&
                ' different time step than case or walkin loads. Compare loads on system served by '//&
                ' secondary loop "'//&
                TRIM(Secondary(SecondaryID)%Name))
              ErrorsFound = .TRUE.
           ENDIF ! check for consistency of loads (coils calc on sys time step, all others on zone time step)
           !mark all Secondarys on system as used by this system (checking for unused or non-unique Secondarys)
           Secondary(SecondaryID)%NumSysAttach = Secondary(SecondaryID)%NumSysAttach + 1
           NominalTotalSecondaryCap = NominalTotalSecondaryCap + Secondary(SecondaryID)%CoolingLoadRated
           System(RefrigSysNum)%RefInventory=System(RefrigSysNum)%RefInventory + &
                                              Secondary(SecondaryID)%ChillerRefInventory
            ! Find design evaporating temperature for system by getting min design evap for ALL loads
           IF ((SecondaryIndex == 1) .AND. (System(RefrigSysNum)%NumCases ==0) .AND. &
               (System(RefrigSysNum)%NumCoils ==0) .AND. (System(RefrigSysNum)%NumWalkIns == 0 ))THEN
                !note use secondary index above, not secondaryid here to get
                !first secondary on this suction group/system
                !note - TMinNeeded on secondary defined by cases and walkins served by secondary, not by
                !       the secondary's rated evaporating temperature (which is used to calc secondary heat
                !       exchanger effectiveness with other rated values)
             System(RefrigSysNum)%TEvapDesign=Secondary(SecondaryID)%TMinNeeded
           ELSE
             System(RefrigSysNum)%TEvapDesign=MIN(Secondary(SecondaryID)%TMinNeeded,System(RefrigSysNum)%TEvapDesign)
           END IF
         END DO  !SecondaryIndex=1,NumSecondary
         System(RefrigSysNum)%NumNonCascadeLoads = System(RefrigSysNum)%NumNonCascadeLoads + System(RefrigSysNum)%NumSecondarys
    END IF !numsecondary > 0

    IF (NumCascadeLoad > 0 ) THEN
      DO CascadeLoadIndex = 1, NumCascadeLoad
         CondID=System(RefrigSysNum)%CascadeLoadNum(CascadeLoadIndex)
         IF(Condenser(CondID)%CondenserType /= RefrigCondenserTypeCascade) THEN
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)&
                             //'", has a  '//&
                             TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum))//&
                             ' cascade load that is not a cascade condenser.')
           ErrorsFound = .TRUE.
         END IF
         ! For a cascade condenser, need to identify the system absorbing the heat
         Condenser(CondID)%CascadeSinkSystemID=RefrigSysNum
         NominalTotalCascadeLoad = NominalTotalCascadeLoad + Condenser(CondID)%RatedCapacity
          ! Find design evaporating temperature for system by getting min design evap for ALL loads
         IF (System(RefrigSysNum)%NumNonCascadeLoads == 0 ) THEN
            IF (CascadeLoadIndex == 1) THEN
                !note use cascadeload index above, not condid here to get
                !first cascade condenser served by this suction group/system
                System(RefrigSysNum)%TEvapDesign=Condenser(CondID)%CascadeRatedEvapTemp
            ELSE
                System(RefrigSysNum)%TEvapDesign=MIN(Condenser(CondID)%CascadeRatedEvapTemp,System(RefrigSysNum)%TEvapDesign)
            END IF ! CascadeLoadIndex == 1
         ELSE  ! (NumNonCascadeLoads > 0 so initial TEvapDesign set above with those other loads)
            IF (Condenser(CondID)%CascadeTempControl == CascadeTempSet) & ! other wise TEvapDesign set by other loads
              System(RefrigSysNum)%TEvapDesign=MIN(Condenser(CondID)%CascadeRatedEvapTemp,System(RefrigSysNum)%TEvapDesign)
         END IF
     END DO  !CascadeLoadIndex=1,NumCascadeLoad
    END IF  !CascadeLoadNum > 0
  END IF  !yes/no blank input for transfer loads

  ! check for consistency of loads (coils calc on sys time step, all others on zone time step, so can't mix on one system)
  IF(System(RefrigSysNum)%CoilFlag) THEN !could already be true if serving secondary that serves coils
    IF ((System(RefrigSysNum)%NumCases > 0) .OR.(System(RefrigSysNum)%NumWalkIns > 0))THEN
               CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//  &
                '="'//TRIM(System(RefrigSysNum)%Name)//&
                '", Serves an inconsistent mixture of loads. Coil-type loads are served on a '//&
                ' different time step than case or walkin loads.')
              ErrorsFound = .TRUE.
    END IF
  ELSE !no coils on secondary or no secondary
    IF(System(RefrigSysNum)%NumCoils > 0) THEN !(note, coilflag set to .false. for all systems as default above
      System(RefrigSysNum)%CoilFlag = .TRUE.
      IF((System(RefrigSysNum)%NumCases > 0) .OR.(System(RefrigSysNum)%NumWalkIns > 0))THEN
               CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//  &
                '="'//TRIM(System(RefrigSysNum)%Name)//&
                '", Serves an inconsistent mixture of loads. Coil-type loads are served on a '//&
                ' different time step than case or walkin loads.')
              ErrorsFound = .TRUE.
      END IF
    END IF ! NumCoils > 0
  END IF !Coil flag already true due to secondary coil loads

   NominalTotalCoolingCap = NominalTotalCaseCap + NominalTotalWalkInCap + NominalTotalSecondaryCap + &
                            NominalTotalCascadeLoad

  ! read condenser
  ! currently assumes one condenser per refrigeration system and but multiple systems allowed per condenser
  AlphaNum = 4
  NumCondensers = 1
  IF(.NOT. ALLOCATED(System(RefrigSysNum)%CondenserNum)) &
           ALLOCATE(System(RefrigSysNum)%CondenserNum(NumCondensers))
  System(RefrigSysNum)%NumCondensers = 1
    !Find condenser number, note condensers were read in one of four objects, but all read into same list
    CondNum = FindItemInList(Alphas(AlphaNum),Condenser%Name,NumRefrigCondensers)
    IF(CondNum == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                           '", has an invalid '// &
                           TRIM(cAlphaFieldNames(AlphaNum))//' defined as '//TRIM(Alphas(AlphaNum)))
      ErrorsFound = .TRUE.
    ELSE
      System(RefrigSysNum)%CondenserNum(NumCondensers) = CondNum
      !Now take care of case where multiple systems share a condenser
      Condenser(CondNum)%NumSysAttach = Condenser(CondNum)%NumSysAttach + 1
      Condenser(CondNum)%SysNum(Condenser(CondNum)%NumSysAttach) = RefrigSysNum
    END IF

    System(RefrigSysNum)%RefInventory=System(RefrigSysNum)%RefInventory + Condenser(CondNum)%RefReceiverInventory + &
               Condenser(CondNum)%RefPipingInventory +  Condenser(CondNum)%RefOpCharge
    IF(Condenser(CondNum)%CondenserType == RefrigCondenserTypeCascade)Condenser(CondNum)%CascadeSysID=RefrigSysNum
    IF((Condenser(Condnum)%CondenserType==RefrigCondenserTypeAir).AND. &
       ( Condenser(Condnum)%CondenserRejectHeatToZone))System(RefrigSysNum)%SystemRejectHeatToZone = .TRUE.

    !Now do evaporative condenser auto sizing because it is a function of the system's cooling load
    IF(Condenser(CondNum)%CondenserType == RefrigCondenserTypeEvap)THEN
      IF(Condenser(CondNum)%RatedAirFlowRate==AutoCalculate)    THEN
        Condenser(CondNum)%RatedAirFlowRate = AirVolRateEvapCond * Condenser(CondNum)%RatedCapacity
      END IF
      IF (Condenser(CondNum)%RatedAirFlowRate <= 0.0d0 ) THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
            '", Evaporative Condenser Air Volume Flow Rate cannot be less than or equal to zero.')
         ErrorsFound = .TRUE.
      END IF
      IF(Condenser(CondNum)%EvapPumpPower==AutoCalculate) THEN
        Condenser(CondNum)%EvapPumpPower = CondPumpRatePower * Condenser(CondNum)%RatedCapacity
      END IF
      IF(Condenser(CondNum)%EvapPumpPower < 0.0d0 ) THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Condenser(CondNum)%Name)//&
            '", Design Evaporative Condenser Water Pump Power cannot be less than zero.')
         ErrorsFound = .TRUE.
      END IF
    END IF

  ! Read the compressor data.
  ! If the system consists of two stages of compression, these compressors will be the low-stage compressors.
    AlphaNum=5
    NumCompressorsSys = 0
    IF(lAlphaBlanks(AlphaNum)) THEN
       !blank input where must have compressor or compressor list input.
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' '//TRIM(cAlphaFieldNames(AlphaNum))//'" : '//&
                            'must be input.')
        ErrorsFound = .TRUE.
    ELSE  !     Entry for Alphas(AlphaNum) can be either a compressor name or a compressorlist name
       IF (NumCompressorLists > 0 ) THEN
         ListNum=FindItemInList(Alphas(AlphaNum),CompressorLists%Name,NumCompressorLists)
       ELSE
         ListNum=0
       ENDIF
       IF (NumSimulationCompressors > 0) THEN
         CompNum=FindItemInList(Alphas(AlphaNum),Compressor%Name,NumSimulationCompressors)
       ELSE
         CompNum=0
       ENDIF
       IF((ListNum == 0) .AND. (CompNum == 0)) THEN  ! name doesn't match either a compressor or a compressor list
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' '//&
                             TRIM(cAlphaFieldNames(AlphaNum))//', has an invalid '//&
                             'or undefined value="'//TRIM(Alphas(AlphaNum))//'".')
          ErrorsFound = .TRUE.
       ELSEIF((ListNum /= 0) .AND. (CompNum /= 0)) THEN  !have compressor list and compressor with same name
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' '//&
                             TRIM(cAlphaFieldNames(AlphaNum))//', has a non-unique name '//&
                             ' used for both Compressor and CompressorList name: "'//TRIM(Alphas(AlphaNum))//'".')
         ErrorsFound = .TRUE.
       ELSE IF(ListNum /= 0)  THEN
           NumCompressorsSys                   = CompressorLists(ListNum)%NumCompressors
           System(RefrigSysNum)%NumCompressors = NumCompressorsSys
           IF(.NOT. ALLOCATED(System(RefrigSysNum)%CompressorNum))&
                 ALLOCATE(System(RefrigSysNum)%CompressorNum(NumCompressorsSys))
           System(RefrigSysNum)%CompressorNum(1:NumCompressorsSys) = CompressorLists(ListNum)%CompItemNum(1:NumCompressorsSys)
       ELSEIF (CompNum /= 0) THEN
           NumCompressorsSys = 1
           System(RefrigSysNum)%NumCompressors = 1
           IF(.NOT. ALLOCATED(System(RefrigSysNum)%CompressorNum))&
                    ALLOCATE(System(RefrigSysNum)%CompressorNum(NumCompressorsSys))
           System(RefrigSysNum)%CompressorNum(NumCompressorsSys)=CompNum
       END IF
  ENDIF

  IF (.NOT. lNumericBlanks(1)) THEN
      System(RefrigSysNum)%TCondenseMin= Numbers(1)
      System(RefrigSysNum)%TCondenseMinInput = System(RefrigSysNum)%TCondenseMin
      IF (AnyEnergyManagementSystemInModel) THEN
        CALL SetupEMSActuator('Refrigeration:System', System(RefrigSysNum)%Name, &
                          'Minimum Condensing Temperature' , '[C]', &
                           System(RefrigSysNum)%EMSOverrideOnTCondenseMin,  &
                           System(RefrigSysNum)%EMSOverrideValueTCondenseMin )
      ENDIF
  ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
          '", '//TRIM(cNumericFieldNames(1))//' must be defined.')
          ErrorsFound = .TRUE.
  END IF
  IF ((Condenser(CondNum)%CondenserType == RefrigCondenserTypeCascade).AND. &
      (System(RefrigSysNum)%TCondenseMin > Condenser(CondNum)%RatedTCondense)) &
      CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                          '", The system specified minimum condensing temperature is greater than '//&
                          ' the rated condensing temperature for the cascade condenser. ')

  AlphaNum=6
  System(RefrigSysNum)%RefrigerantName = Alphas(AlphaNum)
     !error messages for refrigerants already found in fluidproperties

  AlphaNum=7
  IF(.NOT. lAlphaBlanks(AlphaNum)) THEN
      IF (SameString(Alphas(AlphaNum),'ConstantSuctionTemperature')) THEN
        System(RefrigSysNum)%CompSuctControl = ConstantSuctionTemperature
      ELSEIF (SameString(Alphas(AlphaNum),'FloatSuctionTemperature')) THEN
        System(RefrigSysNum)%CompSuctControl = FloatSuctionTemperature
        IF(System(RefrigSysNum)%CoilFlag)THEN
          CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                          '", The system specified a FloatSuctionTemperature, but that is not '//&
                          'available with air chiller loads so ConstantSuctionTemperature will be used. ')
        END IF !coilflag
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)// &
                             '", invalid  '//TRIM(cAlphaFieldNames(AlphaNum))//' not found = '//TRIM(Alphas(AlphaNum)))
        ErrorsFound=.TRUE.
      END IF
  ELSE
      System(RefrigSysNum)%CompSuctControl = ConstantSuctionTemperature !Default for blank
  END IF

  !Count subcoolers on system and allocate
  AlphaNum=8
  System(RefrigSysNum)%NumSubcoolers=0
  IF(.NOT. lAlphaBlanks(AlphaNum)) THEN
    System(RefrigSysNum)%NumSubcoolers = System(RefrigSysNum)%NumSubcoolers + 1
  END IF
  IF(.NOT. lAlphaBlanks(AlphaNum+1)) THEN
    System(RefrigSysNum)%NumSubcoolers = System(RefrigSysNum)%NumSubcoolers + 1
  END IF

  IF(System(RefrigSysNum)%NumSubcoolers > 0)THEN
    IF(.NOT. ALLOCATED(System(RefrigSysNum)%SubcoolerNum)) &
             ALLOCATE(System(RefrigSysNum)%SubcoolerNum(System(RefrigSysNum)%NumSubcoolers))
    NumSubcooler=1
        IF(.NOT. lAlphaBlanks(AlphaNum)) THEN
          System(RefrigSysNum)%SubcoolerNum(NumSubcooler)= GetObjectItemNum('Refrigeration:Subcooler',Alphas(AlphaNum))
          IF(System(RefrigSysNum)%SubcoolerNum(NumSubcooler) <= 0) THEN
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                           '", has an invalid '//TRIM(cAlphaFieldNames(AlphaNum))//' defined as "'//TRIM(Alphas(AlphaNum))//'".')
             ErrorsFound = .TRUE.
          ELSE
            Subcooler(System(RefrigSysNum)%SubcoolerNum(NumSubcooler))%CoilFlag=System(RefrigSysNum)%CoilFlag
          END IF
          NumSubcooler=NumSubcooler+1
        END IF
        IF(.NOT. lAlphaBlanks(AlphaNum+1)) THEN
          System(RefrigSysNum)%SubcoolerNum(NumSubcooler)= GetObjectItemNum('Refrigeration:Subcooler',Alphas(AlphaNum+1))
          IF(System(RefrigSysNum)%SubcoolerNum(NumSubcooler) <= 0) THEN
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                  '", has an invalid '//TRIM(cAlphaFieldNames(AlphaNum+1))//' defined as "'//TRIM(Alphas(AlphaNum+1))//'".')
             ErrorsFound = .TRUE.
          ELSE
            Subcooler(System(RefrigSysNum)%SubcoolerNum(NumSubcooler))%CoilFlag=System(RefrigSysNum)%CoilFlag
          END IF
        END IF
  END IF

  !Suction piping heat gain - optional
  !  Input UA and identify the Zone containing the bulk of the suction piping
  !  This Zone ID will be used to determine the temperature used for suction piping heat gain.
  !  The pipe heat gains are also counted as cooling credit for the zone.
  !  Zone Id is only required if Sum UA Suction Piping >0.0
  !  Get the Zone and zone node numbers from the zone name entered by the user
  AlphaNum=10
  System(RefrigSysNum)%SumUASuctionPiping=0.d0
  IF(.NOT. lNumericBlanks(2) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
    System(RefrigSysNum)%SumUASuctionPiping= Numbers(2)
    System(RefrigSysNum)%SuctionPipeActualZoneNum = FindItemInList(Alphas(AlphaNum),Zone%Name,NumOfZones)
    System(RefrigSysNum)%SuctionPipeZoneNodeNum = GetSystemNodeNumberForZone(Alphas(AlphaNum))
      IF ( System(RefrigSysNum)%SuctionPipeZoneNodeNum == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
               '", System Node Number not found for '//TRIM(cAlphaFieldNames(AlphaNum))// &
               ' = '//TRIM(Alphas(AlphaNum))//' even though '//TRIM(cNumericFieldNames(2))//&
               ' is greater than zero. Suction piping heat gain cannot be calculated unless a Zone'//&
               ' is defined to deterimine the environmental temperature surrounding the piping.')
        ErrorsFound=.TRUE.
      ELSE
        RefrigPresentInZone(System(RefrigSysNum)%SuctionPipeActualZoneNum) = .TRUE.
      ENDIF
  ELSEIF(.NOT. lNumericBlanks(2) .AND. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
               '" '//TRIM(cAlphaFieldNames(AlphaNum))//' not found even though '//TRIM(cNumericFieldNames(2))//&
               ' is greater than zero. Suction piping heat gain will not be calculated unless a Zone'//&
               ' is defined to determine the environmental temperature surrounding the piping.')
  ELSEIF(lNumericBlanks(2) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
               '" '//TRIM(cAlphaFieldNames(AlphaNum))//' will not be used and suction piping heat gain will '//&
               ' not be calculated because '//TRIM(cNumericFieldNames(2))//&
               ' was blank.')
  END IF  !suction piping heat gains

 AlphaNum=11
 IF (.NOT. lAlphaBlanks(AlphaNum))  System(RefrigSysNum)%EndUseSubcategory = Alphas(AlphaNum)

  ! Single-stage or two-stage compression system
  IF(.NOT. lNumericBlanks(3)) THEN
    System(RefrigSysNum)%NumStages = Numbers(3)
    IF(System(RefrigSysNum)%NumStages<1 .OR. System(RefrigSysNum)%NumStages>2) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                           '", '//TRIM(cNumericFieldNames(3))//' has an invalid '//&
                           'value.  Only "1" or "2" compressor stages are allowed.')
      ErrorsFound=.TRUE.
    END IF
  ELSE
    System(RefrigSysNum)%NumStages = 1 !Default for blank
  END IF

  ! Intercooler type
  ! None (0) for single-stage compression systems
  ! Flash intercooler (1) or coil-and-shell intercooler (2) for two-stage compression systems
  AlphaNum=12
  IF (.NOT. lAlphaBlanks(AlphaNum)) THEN
     IF (SameString(Alphas(AlphaNum),'None')) THEN
        System(RefrigSysNum)%IntercoolerType = 0
     ELSEIF (SameString(Alphas(AlphaNum),'Flash Intercooler')) THEN
        System(RefrigSysNum)%IntercoolerType = 1
     ELSEIF (SameString(Alphas(AlphaNum),'Shell-and-Coil Intercooler')) THEN
        System(RefrigSysNum)%IntercoolerType = 2
     ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                 '", Invalid '//TRIM(cAlphaFieldNames(AlphaNum))//' specified.')
        CALL ShowContinueError('"'//TRIM(Alphas(AlphaNum))//'" is not a recognized intercooler type.')
        ErrorsFound=.TRUE.
     END IF
  ELSE
      System(RefrigSysNum)%IntercoolerType = 0 !Default for blank
  END IF

  IF (System(RefrigSysNum)%NumStages==1 .AND. &
     (System(RefrigSysNum)%IntercoolerType==1 .OR. System(RefrigSysNum)%IntercoolerType==2)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                 '", A single-stage compression system')
        CALL ShowContinueError('has been specified with an intercooler.  Verify that the number of compressor stages')
        CALL ShowContinueError('and the intercooler type are consistent.')
        ErrorsFound=.TRUE.
  ELSEIF (System(RefrigSysNum)%NumStages==2 .AND. System(RefrigSysNum)%IntercoolerType==0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                 '", A two-stage compression system')
        CALL ShowContinueError('has been specified without an intercooler.  Verify that the number of compressor stages')
        CALL SHowContinueError('and the intercooler type are consistent.')
        ErrorsFound=.TRUE.
  END IF

  ! Shell-and-coil intercooler effectiveness
  IF(.NOT. lNumericBlanks(4)) THEN
    System(RefrigSysNum)%IntercoolerEffectiveness = Numbers(4)
    IF(System(RefrigSysNum)%IntercoolerEffectiveness<0.0d0 .OR. System(RefrigSysNum)%IntercoolerEffectiveness>1.0d0) THEN
       CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
               '", The specified value for the')
       CALL ShowContinueError(TRIM(cNumericFieldNames(4))//' = '//&
       TRIM(RoundSigDigits(System(RefrigSysNum)%IntercoolerEffectiveness,2))//' is invalid.  This value must be')
       CALL ShowContinueError('between 0.0 and 1.0.  The default value of 0.8 will be used.')
       System(RefrigSysNum)%IntercoolerEffectiveness = 0.8d0
    END IF
  ELSE
    System(RefrigSysNum)%IntercoolerEffectiveness = 0.8d0
  END IF

  ! Read the high-stage compressor info, if two-stage compression has been specified.
  AlphaNum=13
  NumHiStageCompressorsSys = 0
  IF(System(RefrigSysNum)%NumStages==2) THEN
    IF(lAlphaBlanks(AlphaNum)) THEN
       !blank input where must have high-stage compressor or compressor list input.
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                '", '//TRIM(cAlphaFieldNames(AlphaNum))//' must be input for two-stage compression systems.')
        ErrorsFound = .TRUE.
    ELSE  !     Entry for Alphas(AlphaNum) can be either a compressor name or a compressorlist name
       ListNum=FindItemInList(Alphas(AlphaNum),CompressorLists%Name,NumCompressorLists)
       CompNum=FindItemInList(Alphas(AlphaNum),Compressor%Name,NumSimulationCompressors)
       IF((ListNum == 0) .AND. (CompNum == 0)) THEN  ! name doesn't match either a compressor or a compressor list
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                '", '//TRIM(cAlphaFieldNames(AlphaNum))//' has an invalid or undefined value="'//&
                TRIM(Alphas(AlphaNum))//'".')
          ErrorsFound = .TRUE.
       ELSEIF((ListNum /= 0) .AND. (CompNum /= 0)) THEN  !have compressor list and compressor with same name
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                 '", '//TRIM(cAlphaFieldNames(AlphaNum))//' has a non-unique name used for both Compressor '//&
                 'and CompressorList name: "'//TRIM(Alphas(AlphaNum))//'".')
         ErrorsFound = .TRUE.
       ELSE IF(ListNum /= 0)  THEN
           NumHiStageCompressorsSys                   = CompressorLists(ListNum)%NumCompressors
           System(RefrigSysNum)%NumHiStageCompressors = NumHiStageCompressorsSys
           IF(.NOT. ALLOCATED(System(RefrigSysNum)%HiStageCompressorNum))&
                 ALLOCATE(System(RefrigSysNum)%HiStageCompressorNum(NumHiStageCompressorsSys))
           System(RefrigSysNum)%HiStageCompressorNum(1:NumHiStageCompressorsSys) = &
                CompressorLists(ListNum)%CompItemNum(1:NumHiStageCompressorsSys)
       ELSEIF (CompNum /= 0) THEN
           NumHiStageCompressorsSys = 1
           System(RefrigSysNum)%NumHiStageCompressors = 1
           IF(.NOT. ALLOCATED(System(RefrigSysNum)%HiStageCompressorNum))&
                    ALLOCATE(System(RefrigSysNum)%HiStageCompressorNum(NumHiStageCompressorsSys))
           System(RefrigSysNum)%HiStageCompressorNum(NumHiStageCompressorsSys)=CompNum
       END IF
    ENDIF
  ENDIF

  ! Determine intercooler pressure and temperature at design conditions
  IF(System(RefrigSysNum)%NumStages==2) THEN
    Pcond=GetSatPressureRefrig(System(RefrigSysNum)%RefrigerantName, &
                               Condenser(System(RefrigSysNum)%CondenserNum(1))%RatedTCondense, &
                               System(RefrigSysNum)%RefIndex, TRIM(RoutineName))
    Pevap=GetSatPressureRefrig(System(RefrigSysNum)%RefrigerantName, &
                               System(RefrigSysNum)%TEvapDesign, &
                               System(RefrigSysNum)%RefIndex, TRIM(RoutineName))
    System(RefrigSysNum)%PIntercooler=SQRT(Pcond*Pevap)
    System(RefrigSysNum)%TIntercooler=GetSatTemperatureRefrig(System(RefrigSysNum)%RefrigerantName, &
                               System(RefrigSysNum)%PIntercooler, &
                               System(RefrigSysNum)%RefIndex, TRIM(RoutineName))
  END IF  ! NumStages

  ! Sum capacity of single-stage compressors or low-stage compressors if two-stage system
  NominalTotalCompCap = 0.0d0
  DO CompIndex=1,NumCompressorsSys
    CompNum = System(RefrigSysNum)%CompressorNum(CompIndex)
    IF (.NOT. Compressor(CompNum)%TransFlag) THEN     !  Subcritical Compressor
      IF (System(RefrigSysNum)%NumStages==1) THEN      !  Single-stage compression
          Compressor(CompNum)%NomCap = CurveValue(Compressor(CompNum)%CapacityCurvePtr,&
               System(RefrigSysNum)%TEvapDesign,Condenser(System(RefrigSysNum)%CondenserNum(1))%RatedTCondense)
          NominalTotalCompCap = NominalTotalCompCap + Compressor(CompNum)%NomCap
          Compressor(CompNum)%NumSysAttach = Compressor(CompNum)%NumSysAttach + 1
      ELSE     !  Two-stage compression, low-stage compressors
          Compressor(CompNum)%NomCap = CurveValue(Compressor(CompNum)%CapacityCurvePtr,&
               System(RefrigSysNum)%TEvapDesign,System(RefrigSysNum)%TIntercooler)
          NominalTotalCompCap = NominalTotalCompCap + Compressor(CompNum)%NomCap
          Compressor(CompNum)%NumSysAttach = Compressor(CompNum)%NumSysAttach + 1
      END IF     ! NumStages
    ELSE     !  Transcritical compressor attached to subcritical refigeration cycle
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'. '//&
           'A transcritical compressor is attached to a subcritical refrigeration system.')
      CALL ShowContinueError('Check input to ensure that subcritical compressors are '//  &
         'connected only to subcritical systems and'// &
         ' transcritical compressors are connected only to transcritical systems.')
      ErrorsFound = .TRUE.
    END IF     ! .NOT. Compressor(CompNum)%TransFlag
  END DO

  ! Sum capacity of high-stage compressors if two stage system
  IF (System(RefrigSysNum)%NumStages==2) THEN
    DO CompIndex=1,NumHiStageCompressorsSys
      CompNum = System(RefrigSysNum)%HiStageCompressorNum(CompIndex)
      IF (.NOT. Compressor(CompNum)%TransFlag) THEN     !  Subcritical Compressor
          Compressor(CompNum)%NomCap = CurveValue(Compressor(CompNum)%CapacityCurvePtr,&
               System(RefrigSysNum)%TIntercooler,Condenser(System(RefrigSysNum)%CondenserNum(1))%RatedTCondense)
          NominalTotalHiStageCompCap = NominalTotalHiStageCompCap + Compressor(CompNum)%NomCap
          Compressor(CompNum)%NumSysAttach = Compressor(CompNum)%NumSysAttach + 1
      ELSE     !  Transcritical compressor attached to subcritical refigeration cycle
        CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'. '//&
             'A transcritical compressor is attached to a subcritical refrigeration system.')
        CALL ShowContinueError('Check input to ensure that subcritical compressors are '//  &
           'connected only to subcritical systems and'// &
           ' transcritical compressors are connected only to transcritical systems.')
        ErrorsFound = .TRUE.
      END IF
    END DO
  ENDIF     ! NumStages

!Compare the rated capacity of compressor, condenser, and cases.
! Note, rated capacities can be far off from operating capacities, but rough check.
   NominalCondCap=Condenser(System(RefrigSysNum)%CondenserNum(1))%RatedCapacity
   IF(System(RefrigSysNum)%SystemRejectHeatToZone)NominalCondCap=NominalCondCap*2.d0
   IF(System(RefrigSysNum)%NumStages==1) THEN  ! Single-stage system
     IF((NominalTotalCompCap < (0.7d0*NominalTotalCoolingCap)) .OR. &
        (NominalCondCap < (1.3d0*NominalTotalCoolingCap)))  THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                             '", You may wish to check the system sizing. Total nominal cooling capacity is '//&
                             TRIM(RoundSigDigits(NominalTotalCoolingCap,0))//'W. Condenser capacity is '//&
                             TRIM(RoundSigDigits(NominalCondCap,0))//'W. Nominal compressor capacity is '//&
                             TRIM(RoundSigDigits(NominalTotalCompCap,0))//'W.')
     END IF
   ELSE IF (System(RefrigSysNum)%NumStages==2) THEN  ! Two-stage system
      IF((NominalTotalHiStageCompCap < (0.7d0*NominalTotalCoolingCap)) .OR. &
        (NominalCondCap < (1.3d0*NominalTotalCoolingCap)))  THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(System(RefrigSysNum)%Name)//&
                             '", You may wish to check the system sizing. Total nominal cooling capacity is '//&
                             TRIM(RoundSigDigits(NominalTotalCoolingCap,0))//'W. Condenser capacity is '//&
                             TRIM(RoundSigDigits(NominalCondCap,0))//'W. Nominal compressor capacity is '//&
                             TRIM(RoundSigDigits(NominalTotalCompCap,0))//'W.')
     END IF
   END IF  ! NumStages

 END DO  ! Refrigeration systems

  ! Assign coilflags to compressors, condensers, and subcoolers (coils calc on sys time step, all other refrig loads on zone time step, so can't mix on one system)
  ! need to do here once again after all cascade condensers and cascade sink systems have been identified
  DO RefrigSysNum=1,NumRefrigSystems
    !assign flags to all condensers to match system below condenser (system rejecting heat to cascade condenser)
    CondNum = System(RefrigSysNum)%CondenserNum(1)  ! right now only have one condenser per system
    Condenser(CondNum)%CoilFlag = System(RefrigSysNum)%CoilFlag
    DO CompIndex=1,System(RefrigSysNum)%NumCompressors
         CompNum = System(RefrigSysNum)%CompressorNum(CompIndex)
         Compressor(CompNum)%CoilFlag = System(RefrigSysNum)%CoilFlag
    END DO

  END DO !assign coil flags to all condensers

  !Finished setting cascade condenser coilflags to match system rejecting heat to the cascade condenser
  ! Now have to see if there's a mismatch in the coilflag with the system absorbing heat from the cascade condenser
  ! Note a system can cool multiple cascade condensers.  If so, need to be sure all are consistent - all coil or all non-coil(called case here)
  ! check for consistency of loads (coils calc on sys time step, all others on zone time step, so can't mix on one system)
  DO RefrigSysNum=1,NumRefrigSystems  !check flags for systems reflect all cascade loads
    IF (System(RefrigSysNum)%NumCascadeLoads == 0) CYCLE
    IF(System(RefrigSysNum)%CoilFlag) THEN !system already identified as serving coils
      DO CondID = 1,NumRefrigCondensers
        IF (Condenser(CondID)%CondenserType /= RefrigCondenserTypeCascade)CYCLE
        IF (RefrigSysNum /= Condenser(CondID)%CascadeSinkSystemID) CYCLE  !this condenser is not a cascade load on this system
        IF (.NOT. Condenser(CondID)%CoilFlag )THEN
            !would mean system already serving coil loads and this condenser cooling system with case-type loads
               CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//  &
                '="'//TRIM(System(RefrigSysNum)%Name)//&
                '", Serves an inconsistent mixture of loads. Coil-type loads are served on a '//&
                ' different time step than case or walkin loads. Compare loads on system served by '//&
                ' cascade condenser "'//&
                TRIM(Condenser(CondID)%Name))
              ErrorsFound = .TRUE.
        END IF
      END DO !CondID
    ELSE ! %coilflag == false, so no coil loads prev identified directly or through secondary loop
      CaseLoads = .FALSE.
      NumCascadeLoadsChecked = 0
      DO CondID = 1,NumRefrigCondensers !look at All cascade condenser loads on system
        IF (Condenser(CondID)%CondenserType /= RefrigCondenserTypeCascade)CYCLE
        IF (RefrigSysNum /= Condenser(CondID)%CascadeSinkSystemID)CYCLE !this condenser is not a cascade load on this system
        NumCascadeLoadsChecked = NumCascadeLoadsChecked + 1
        IF ((CaseLoads) .AND. (.NOT. Condenser(CondID)%CoilFlag).AND.(.NOT. System(RefrigSysNum)%CoilFlag)) CYCLE
           !all loads to date are case-type and properly flagged with consistent coilflags
           !(note caseloads could be true if prev cascade load checked is serving a case-type system)
        IF (NumCascadeLoadsChecked == 1)THEN
          IF (Condenser(CondID)%CoilFlag) THEN
             System(RefrigSysNum)%CoilFlag = .TRUE.
             !setting system coilflag if 1st cascade condenser served has coils (system has no case-type loads up to this point)
          ELSE  !condenser is not serving coils, but case-type loads
             CaseLoads = .TRUE.
             !system coilflag already set to false
          END IF !Condenser%CoilFlag
        ELSE !numcascadeloadschecked > 1
            IF(System(RefrigSysNum)%CoilFlag .neqv. Condenser(CondID)%CoilFlag)THEN
               CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//  &
                '="'//TRIM(System(RefrigSysNum)%Name)//&
                '", Serves an inconsistent mixture of loads. Coil-type loads are served on a '//&
                ' different time step than case or walkin loads. Compare loads on system served by '//&
                ' cascade condenser "'//&
                TRIM(Condenser(CondID)%Name))
              ErrorsFound = .TRUE.
            END IF
        END IF !numcascadeloadschecked > 1
      END DO ! CondID
    END IF !(System%coilflag)
  END DO  ! Refrigeration systems checking coilflag consistency with cascade condenser loads

END IF  !(NumRefrigSystems > 0)

!after the systems have been read, can finish the mechanical subcooler/system interactions
 !System%NumMechSCServed=0
 IF (NumSimulationSubcoolers > 0) THEN
 DO SubcoolerNum=1, NumSimulationSubcoolers
   IF(Subcooler(SubcoolerNum)%Subcoolertype == LiquidSuction)CYCLE
   Subcooler(SubcoolerNum)%MechSourceSysID = &
          GetObjectItemNum('Refrigeration:System',Subcooler(SubcoolerNum)%MechSourceSys)
   IF (Subcooler(SubcoolerNum)%MechSourceSysID == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//  &
                '="'//TRIM(Subcooler(SubcoolerNum)%Name)//&
             '", Mechanical Subcooler has an invalid Source Refrigeration:System="'//&
              TRIM(Subcooler(SubcoolerNum)%MechSourceSys)//'".')
          ErrorsFound = .TRUE.
   ELSE
     IF (System(Subcooler(SubcoolerNum)%MechSourceSysID)%CoilFlag .neqv. Subcooler(SubcoolerNum)%CoilFlag )THEN
               CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//  &
                '="'//TRIM(System(RefrigSysNum)%Name)//&
                '", Serves an inconsistent mixture of loads. Coil-type loads are served on a '//&
                ' different time step than case or walkin loads. Compare loads on system served by '//&
                ' mechanical subcooler "'//&
                TRIM(Subcooler(SubcoolerNum)%Name))
              ErrorsFound = .TRUE.
        END IF
   END IF  !error check
 END DO ! numsubcoolers

 DO RefrigSysNum=1,NumRefrigSystems
    DO SubcoolerNum=1,NumSimulationSubcoolers
      IF(Subcooler(SubcoolerNum)%Subcoolertype == LiquidSuction)CYCLE
      IF(Subcooler(SubcoolerNum)%MechSourceSysID == RefrigSysNum)THEN
        System(RefrigSysNum)%NumMechSCServed=System(RefrigSysNum)%NumMechSCServed + 1
      END IF
    END DO
    IF(System(RefrigSysNum)%NumMechSCServed > 0) THEN
       IF(.NOT. ALLOCATED(System(RefrigSysNum)%MechSCLoad)) &
                ALLOCATE(System(RefrigSysNum)%MechSCLoad(NumSimulationSubcoolers))
    END IF
 END DO
 END IF ! NumSimulationSubcoolers > 0

! **********  READ TRANSCRITICAL REFRIGERATION SYSTEMS  **********

IF (NumTransRefrigSystems >0) THEN
  CurrentModuleObject='Refrigeration:TranscriticalSystem'
  DO TransRefrigSysNum=1,NumTransRefrigSystems
    CALL GetObjectItem(CurrentModuleObject,TransRefrigSysNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK =.FALSE.
    IsBlank =.FALSE.
    CALL VerifyName(Alphas(1),TransSystem%Name,RefrigSysNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//', has an invalid or undefined '// &
                           TRIM(cAlphaFieldNames(1))//'="'//TRIM(Alphas(1))//'".')
      IF (IsBlank) Alphas(1)='xxxxx'
      ErrorsFound=.TRUE.
    ENDIF
    TransSystem(TransRefrigSysNum)%Name = Alphas(1)

  ! Read refrigerant for this system
  AlphaNum=8
  TransSystem(TransRefrigSysNum)%RefrigerantName = Alphas(AlphaNum)
  !error messages for refrigerants already found in fluidproperties

! Read Transcritical System Type:  SingleStage or TwoStage
    IF(lAlphaBlanks(2)) THEN
       ! No system type specified
       CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)// &
                    '", has no system type specified.')
       CALL ShowContinueError('  System type must be specified as "SingleStage" or "TwoStage".')
       ErrorsFound = .TRUE.
    END IF
    IF (SameString(Alphas(2),'SingleStage')) THEN
       TransSystem(TransRefrigSysNum)%TransSysType = 1
    ELSE IF (SameString(Alphas(2),'TwoStage')) THEN
       TransSystem(TransRefrigSysNum)%TransSysType = 2
    ELSE
       CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)// &
                    '", has an incorrect System Type specified as "'//TRIM(Alphas(2))//'".')
       CALL ShowContinueError('  System type must be specified as "SingleStage" or "TwoStage".')
       ErrorsFound = .TRUE.
    END IF

! Read all loads (display cases and walk-ins) on this Transcritical System
    IF(lAlphaBlanks(3) .AND. lAlphaBlanks(4)) THEN
       ! No loads specified - display error
        CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)// &
                    '", has no loads.')
        CALL ShowContinueError('  The system must have at least one of: '//TRIM(cAlphaFieldNames(3))//' or '// &
                               TRIM(cAlphaFieldNames(4))//' objects attached.')
        ErrorsFound = .TRUE.
    ELSE IF (lAlphaBlanks(3) .AND. TransSystem(TransRefrigSysNum)%TransSysType == 1) THEN
       ! No medium temperature loads specified for a SingleStage system - display error
        CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)// &
                    '", is a "SingleStage" system but no medium temperature loads are specified.')
        CALL ShowContinueError('  The system must have at least one '//TRIM(cAlphaFieldNames(3))//' object attached.')
        ErrorsFound = .TRUE.
    ELSE IF (lAlphaBlanks(4) .AND. TransSystem(TransRefrigSysNum)%TransSysType == 2) THEN
       ! No low temperature loads specified for a TwoStage system - display error
        CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)// &
                    '", is a "TwoStage" system but no low temperature loads are specified.')
        CALL ShowContinueError('  The system must have at least one '//TRIM(cAlphaFieldNames(4))//' object attached.')
        ErrorsFound = .TRUE.
    END IF

    NumCasesMT   = 0
    TransSystem(TransRefrigSysNum)%NumCasesMT   = 0
    NumCasesLT   = 0
    TransSystem(TransRefrigSysNum)%NumCasesLT   = 0
    NumWalkInsMT = 0
    TransSystem(TransRefrigSysNum)%NumWalkInsMT = 0
    NumWalkInsLT = 0
    TransSystem(TransRefrigSysNum)%NumWalkInsLT = 0
    NominalTotalCaseCapMT   = 0.0d0
    NominalTotalCaseCapLT   = 0.0d0
    NominalTotalWalkInCapMT = 0.0d0
    NominalTotalWalkInCapLT = 0.0d0
    NominalTotalCoolingCap  = 0.0d0
    TransSystem(TransRefrigSysNum)%RefInventory = 0.0d0

   !   Check for Medium Temperature Case or Walk-In or CaseAndWalkInList names
    AlphaNum = 3

    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN

    ! Entry for Alphas(AlphaNum) can be either a Case, WalkIn or CaseAndWalkInList name
      CaseAndWalkInListNum=0
      CaseNum=0
      WalkInNum=0
      IF(NumSimulationCaseAndWalkInLists > 0) &
            CaseAndWalkInListNum=FindItemInList(Alphas(AlphaNum),CaseAndWalkInList%Name,NumSimulationCaseAndWalkInLists)
      IF(NumSimulationCases > 0)  CaseNum=  FindItemInList(Alphas(AlphaNum),RefrigCase%Name,NumSimulationCases)
      IF(NumSimulationWalkIns > 0)WalkInNum=FindItemInList(Alphas(AlphaNum),WalkIn%Name,NumSimulationWalkIns)
      NumNameMatches = 0
      IF(CaseAndWalkInListNum /= 0)NumNameMatches = NumNameMatches +1
      IF(CaseNum /= 0)       NumNameMatches = NumNameMatches +1
      IF(WalkInNum /= 0)     NumNameMatches = NumNameMatches +1

      IF (NumNameMatches /= 1) THEN !name must uniquely point to a list or a single case or walkin or coil
          ErrorsFound = .TRUE.
          IF(NumNameMatches == 0) THEN
            CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
                   '", has an invalid '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          ELSEIF(NumNameMatches > 1) THEN
            CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//&
                    TRIM(TransSystem(TransRefrigSysNum)%Name)//'",  has a non-unique name '//&
                    'that could be either a '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          END IF !num matches = 0 or > 1
      ELSEIF(CaseAndWalkInListNum /= 0)  THEN  !Name points to a CaseAndWalkInList
         NumCasesMT   = CaseAndWalkInList(CaseAndWalkInListNum)%NumCases
         NumWalkInsMT = CaseAndWalkInList(CaseAndWalkInListNum)%NumWalkIns
         TransSystem(TransRefrigSysNum)%NumCasesMT   = NumCasesMT
         TransSystem(TransRefrigSysNum)%NumWalkInsMT = NumWalkInsMT
         IF(NumCasesMT > 0) THEN
           IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%CaseNumMT))  &
              ALLOCATE(TransSystem(TransRefrigSysNum)%CaseNumMT(NumCasesMT))
           TransSystem(TransRefrigSysNum)%CaseNumMT(1:NumCasesMT) =   &
              CaseAndWalkInList(CaseAndWalkInListNum)%CaseItemNum(1:NumCasesMT)
         END IF
         IF(NumWalkInsMT > 0) THEN
           IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%WalkInNumMT))  &
              ALLOCATE(TransSystem(TransRefrigSysNum)%WalkInNumMT(NumWalkInsMT))
           TransSystem(TransRefrigSysNum)%WalkInNumMT(1:NumWalkInsMT) =   &
              CaseAndWalkInList(CaseAndWalkInListNum)%WalkInItemNum(1:NumWalkInsMT)
         END IF
      ELSEIF (CaseNum /= 0) THEN     !Name points to a case
         NumCasesMT = 1
         TransSystem(TransRefrigSysNum)%NumCasesMT = 1
         IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%CaseNumMT))   &
            ALLOCATE(TransSystem(TransRefrigSysNum)%CaseNumMT(NumCasesMT))
         TransSystem(TransRefrigSysNum)%CaseNumMT(NumCases)=CaseNum
      ELSEIF (WalkInNum /= 0) THEN   !Name points to a walkin
         NumWalkInsMT = 1
         TransSystem(TransRefrigSysNum)%NumWalkInsMT = 1
         IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%WalkInNumMT)) &
                  ALLOCATE(TransSystem(TransRefrigSysNum)%WalkInNumMT(NumWalkInsMT))
         TransSystem(TransRefrigSysNum)%WalkInNumMT(NumWalkIns)=WalkInNum
      END IF  !NumNameMatches /= 1
    END IF !blank input for cases, walkins, or caseandwalkinlist

   IF(NumCasesMT > 0) THEN
       ! Find lowest design evap T
       ! Sum rated capacity of all MT cases on system
       DO CaseIndex = 1, NumCasesMT
          !mark all cases on system as used by this system - checking for unused or non-unique cases
          CaseNum=TransSystem(TransRefrigSysNum)%CaseNumMT(CaseIndex)
          RefrigCase(CaseNum)%NumSysAttach = RefrigCase(CaseNum)%NumSysAttach + 1
          NominalTotalCaseCapMT = NominalTotalCaseCapMT + RefrigCase(CaseNum)%DesignRatedCap
          TransSystem(TransRefrigSysNum)%RefInventory=TransSystem(TransRefrigSysNum)%RefInventory + &
                                                      RefrigCase(Casenum)%DesignRefrigInventory
          IF(CaseIndex == 1) THEN  !look for lowest case design evap T for system
              TransSystem(TransRefrigSysNum)%TEvapDesignMT=RefrigCase(CaseNum)%EvapTempDesign
          ELSE
              TransSystem(TransRefrigSysNum)%TEvapDesignMT = &
                 MIN(RefrigCase(CaseNum)%EvapTempDesign,TransSystem(TransRefrigSysNum)%TEvapDesignMT)
          END IF
       END DO  !CaseIndex=1,NumCases
   END IF  !NumcasesMT > 0

    IF (NumWalkInsMT > 0) THEN
      DO WalkInIndex = 1, NumWalkInsMT
         WalkInID=TransSystem(TransRefrigSysNum)%WalkInNumMT(WalkInIndex)
         !mark all WalkIns on rack as used by this system (checking for unused or non-unique WalkIns)
         WalkIn(WalkInID)%NumSysAttach = WalkIn(WalkInID)%NumSysAttach + 1
         NominalTotalWalkInCapMT = NominalTotalWalkInCapMT + WalkIn(WalkInID)%DesignRatedCap
         TransSystem(TransRefrigSysNum)%RefInventory=TransSystem(TransRefrigSysNum)%RefInventory + &
                                                     WalkIn(WalkInID)%DesignRefrigInventory
         !Defrost capacity is treated differently by compressor racks and detailed systems,
         !  so this value may be adjusted (or warnings issued) after the walkin is assigned
         !  to either the rack or system.
         !for walkins served by detailed system, need capacity for both fluid and electric types.
         IF (WalkIn(WalkInID)%DefrostCapacity <= -98.d0) THEN
            ! - 99 used as a flag for blank input error message for detailed systems
            CALL ShowSevereError(RoutineName//'Refrigeration:WalkIn="'//TRIM(WalkIn(WalkInID)%Name)//&
                           '", Defrost capacity must be greater than or equal to 0 W' //&
                           ' for electric and hotfluid defrost types')
            ErrorsFound = .TRUE.
         END IF
         ! Find design evaporating temperature for system by getting min design evap for ALL loads
         IF ((WalkInIndex == 1) .AND. (TransSystem(TransRefrigSysNum)%NumCasesMT ==0)) THEN
                !note use walk in index, not walkinid here to get
                !first walkin on this suction group/system
           TransSystem(TransRefrigSysNum)%TEvapDesignMT=WalkIn(WalkInID)%TEvapDesign
         ELSE
           TransSystem(TransRefrigSysNum)%TEvapDesignMT=  &
              MIN(WalkIn(WalkInID)%TEvapDesign,TransSystem(TransRefrigSysNum)%TEvapDesignMT)
         END IF
       END DO  !WalkInIndex=1,NumWalkIns
    END IF  !NumWalkInsMT > 0

   !   Check for Low Temperature Case or Walk-In or CaseAndWalkInList names
    AlphaNum = 4
    IF (.NOT. lAlphaBlanks(AlphaNum)) THEN

    ! Entry for Alphas(AlphaNum) can be either a Case, WalkIn or CaseAndWalkInList name
      CaseAndWalkInListNum=0
      CaseNum=0
      WalkInNum=0
      IF(NumSimulationCaseAndWalkInLists > 0) &
            CaseAndWalkInListNum=FindItemInList(Alphas(AlphaNum),CaseAndWalkInList%Name,NumSimulationCaseAndWalkInLists)
      IF(NumSimulationCases > 0)  CaseNum=  FindItemInList(Alphas(AlphaNum),RefrigCase%Name,NumSimulationCases)
      IF(NumSimulationWalkIns > 0)WalkInNum=FindItemInList(Alphas(AlphaNum),WalkIn%Name,NumSimulationWalkIns)
      NumNameMatches = 0
      IF(CaseAndWalkInListNum /= 0)NumNameMatches = NumNameMatches +1
      IF(CaseNum /= 0)       NumNameMatches = NumNameMatches +1
      IF(WalkInNum /= 0)     NumNameMatches = NumNameMatches +1

      IF (NumNameMatches /= 1) THEN !name must uniquely point to a list or a single case or walkin or coil
          ErrorsFound = .TRUE.
          IF(NumNameMatches == 0) THEN
            CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
                   '", has an invalid '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          ELSEIF(NumNameMatches > 1) THEN
            CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//&
                    TRIM(TransSystem(TransRefrigSysNum)%Name)//'",  has a non-unique name '//&
                    'that could be either a '//TRIM(cAlphaFieldNames(AlphaNum))//': '//TRIM(Alphas(AlphaNum)))
          END IF !num matches = 0 or > 1
      ELSEIF(CaseAndWalkInListNum /= 0)  THEN  !Name points to a CaseAndWalkInList
         NumCasesLT   = CaseAndWalkInList(CaseAndWalkInListNum)%NumCases
         NumWalkInsLT = CaseAndWalkInList(CaseAndWalkInListNum)%NumWalkIns
         TransSystem(TransRefrigSysNum)%NumCasesLT   = NumCasesLT
         TransSystem(TransRefrigSysNum)%NumWalkInsLT = NumWalkInsLT
         IF(NumCasesLT > 0) THEN
           IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%CaseNumLT))  &
              ALLOCATE(TransSystem(TransRefrigSysNum)%CaseNumLT(NumCasesLT))
           TransSystem(TransRefrigSysNum)%CaseNumLT(1:NumCasesLT) =   &
              CaseAndWalkInList(CaseAndWalkInListNum)%CaseItemNum(1:NumCasesLT)
         END IF
         IF(NumWalkInsLT > 0) THEN
           IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%WalkInNumLT))  &
              ALLOCATE(TransSystem(TransRefrigSysNum)%WalkInNumLT(NumWalkInsLT))
           TransSystem(TransRefrigSysNum)%WalkInNumLT(1:NumWalkInsLT) =   &
              CaseAndWalkInList(CaseAndWalkInListNum)%WalkInItemNum(1:NumWalkInsLT)
         END IF
      ELSEIF (CaseNum /= 0) THEN     !Name points to a case
         NumCasesLT = 1
         TransSystem(TransRefrigSysNum)%NumCasesLT = 1
         IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%CaseNumLT))   &
            ALLOCATE(TransSystem(TransRefrigSysNum)%CaseNumLT(NumCasesLT))
         TransSystem(TransRefrigSysNum)%CaseNumLT(NumCases)=CaseNum
      ELSEIF (WalkInNum /= 0) THEN   !Name points to a walkin
         NumWalkInsLT = 1
         TransSystem(TransRefrigSysNum)%NumWalkInsLT = 1
         IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%WalkInNumLT)) &
                  ALLOCATE(TransSystem(TransRefrigSysNum)%WalkInNumLT(NumWalkInsLT))
         TransSystem(TransRefrigSysNum)%WalkInNumLT(NumWalkIns)=WalkInNum
      END IF  !NumNameMatches /= 1
    END IF !blank input for cases, walkins, or caseandwalkinlist

   IF(NumCasesLT > 0) THEN
       ! Find lowest design evap T
       ! Sum rated capacity of all LT cases on system
       DO CaseIndex = 1, NumCasesLT
          !mark all cases on system as used by this system - checking for unused or non-unique cases
          CaseNum=TransSystem(TransRefrigSysNum)%CaseNumLT(CaseIndex)
          RefrigCase(CaseNum)%NumSysAttach = RefrigCase(CaseNum)%NumSysAttach + 1
          NominalTotalCaseCapLT = NominalTotalCaseCapLT + RefrigCase(CaseNum)%DesignRatedCap
          TransSystem(TransRefrigSysNum)%RefInventory=TransSystem(TransRefrigSysNum)%RefInventory + &
                                                      RefrigCase(Casenum)%DesignRefrigInventory
          IF(CaseIndex == 1) THEN  !look for lowest case design evap T for system
              TransSystem(TransRefrigSysNum)%TEvapDesignLT=RefrigCase(CaseNum)%EvapTempDesign
          ELSE
              TransSystem(TransRefrigSysNum)%TEvapDesignLT = &
                 MIN(RefrigCase(CaseNum)%EvapTempDesign,TransSystem(TransRefrigSysNum)%TEvapDesignLT)
          END IF
       END DO  !CaseIndex=1,NumCases
   END IF  !NumcasesLT > 0

    IF (NumWalkInsLT > 0) THEN
      DO WalkInIndex = 1, NumWalkInsLT
         WalkInID=TransSystem(TransRefrigSysNum)%WalkInNumLT(WalkInIndex)
         !mark all WalkIns on rack as used by this system (checking for unused or non-unique WalkIns)
         WalkIn(WalkInID)%NumSysAttach = WalkIn(WalkInID)%NumSysAttach + 1
         NominalTotalWalkInCapLT = NominalTotalWalkInCapLT + WalkIn(WalkInID)%DesignRatedCap
         TransSystem(TransRefrigSysNum)%RefInventory=TransSystem(TransRefrigSysNum)%RefInventory + &
                                                     WalkIn(WalkInID)%DesignRefrigInventory
         !Defrost capacity is treated differently by compressor racks and detailed systems,
         !  so this value may be adjusted (or warnings issued) after the walkin is assigned
         !  to either the rack or system.
         !for walkins served by detailed system, need capacity for both fluid and electric types.
         IF (WalkIn(WalkInID)%DefrostCapacity <= -98.d0) THEN
            ! - 99 used as a flag for blank input error message for detailed systems
            CALL ShowSevereError(RoutineName//'Refrigeration:WalkIn="'//TRIM(WalkIn(WalkInID)%Name)//&
                           '", Defrost capacity must be greater than or equal to 0 W' //&
                           ' for electric and hotfluid defrost types')
            ErrorsFound = .TRUE.
         END IF
         ! Find design evaporating temperature for system by getting min design evap for ALL loads
         IF ((WalkInIndex == 1) .AND. (TransSystem(TransRefrigSysNum)%NumCasesLT ==0)) THEN
                !note use walk in index, not walkinid here to get
                !first walkin on this suction group/system
           TransSystem(TransRefrigSysNum)%TEvapDesignLT=WalkIn(WalkInID)%TEvapDesign
         ELSE
           TransSystem(TransRefrigSysNum)%TEvapDesignLT=  &
              MIN(WalkIn(WalkInID)%TEvapDesign,TransSystem(TransRefrigSysNum)%TEvapDesignLT)
         END IF
       END DO  !WalkInIndex=1,NumWalkIns
    END IF  !NumWalkInsMT > 0

   NominalTotalCoolingCap = NominalTotalCaseCapMT + NominalTotalCaseCapLT + NominalTotalWalkInCapMT + &
                            NominalTotalWalkInCapLT

  ! Read Gas Cooler
  ! currently assumes one gas cooler per refrigeration system and but multiple systems allowed per gas cooler
  AlphaNum = 5
  NumGasCoolers = 1
  IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%GasCoolerNum)) &
           ALLOCATE(TransSystem(TransRefrigSysNum)%GasCoolerNum(NumGasCoolers))
  TransSystem(TransRefrigSysNum)%NumGasCoolers = 1
    !Find gascooler number
    GCNum = FindItemInList(Alphas(AlphaNum),GasCooler%Name,NumSimulationGasCooler)

    IF(GCNum == 0) THEN           !  Invalid Gas Cooler attached to Transcritical Refrigeration System
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
                           '", has an invalid '// &
                           TRIM(cAlphaFieldNames(AlphaNum))//' defined as "'//TRIM(Alphas(AlphaNum))//'".')
      ErrorsFound = .TRUE.
    ELSE IF (GCNum /= 0) THEN     !  Gas Cooler attached to Transcritical Refrigeration System
      TransSystem(TransRefrigSysNum)%GasCoolerNum(NumGasCoolers) = GCNum
      TransSystem(TransRefrigSysNum)%NumGasCoolers = 1
      !Now take care of case where multiple systems share a gas cooler
      GasCooler(GCNum)%NumSysAttach = GasCooler(GCNum)%NumSysAttach + 1
      GasCooler(GCNum)%SysNum(GasCooler(GCNum)%NumSysAttach) = TransRefrigSysNum
      TransSystem(TransRefrigSysNum)%RefInventory=TransSystem(TransRefrigSysNum)%RefInventory +   &
         GasCooler(GCNum)%RefReceiverInventory + &
                 GasCooler(GCNum)%RefPipingInventory +  GasCooler(GCNum)%RefOpCharge
      IF(GasCooler(GCNum)%GasCoolerRejectHeatToZone)TransSystem(TransRefrigSysNum)%SystemRejectHeatToZone = .TRUE.
    END IF

    ! Read High Pressure Compressor
    AlphaNum=6
    NumCompressorsSys = 0
    IF(lAlphaBlanks(AlphaNum)) THEN
       !blank input where must have compressor or compressor list input.
        CALL ShowSevereError(Trim(RoutineName)//TRIM(CurrentModuleObject)//' '//TRIM(cAlphaFieldNames(AlphaNum))//'" : '//&
                            'must be input.')
        ErrorsFound = .TRUE.
    ELSE  !     Entry for Alphas(AlphaNum) can be either a compressor name or a compressorlist name
       ListNum=FindItemInList(Alphas(AlphaNum),CompressorLists%Name,NumCompressorLists)
       CompNum=FindItemInList(Alphas(AlphaNum),Compressor%Name,NumSimulationCompressors)
       IF((ListNum == 0) .AND. (CompNum == 0)) THEN  ! name doesn't match either a compressor or a compressor list
          CALL ShowSevereError(Trim(RoutineName)//TRIM(CurrentModuleObject)//', "'//&
                             TRIM(cAlphaFieldNames(AlphaNum))//'", has an invalid '//&
                             'or undefined value="'//TRIM(Alphas(AlphaNum))//'".')
          ErrorsFound = .TRUE.
       ELSEIF((ListNum /= 0) .AND. (CompNum /= 0)) THEN  !have compressor list and compressor with same name
         CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//' '//&
                             TRIM(cAlphaFieldNames(AlphaNum))//', has a non-unique name '//&
                             ' used for both Compressor and CompressorList name: "'//TRIM(Alphas(AlphaNum))//'".')
         ErrorsFound = .TRUE.
       ELSE IF(ListNum /= 0)  THEN
           NumCompressorsSys                   = CompressorLists(ListNum)%NumCompressors
           TransSystem(TransRefrigSysNum)%NumCompressorsHP = NumCompressorsSys
           IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%CompressorNumHP))&
                 ALLOCATE(TransSystem(TransRefrigSysNum)%CompressorNumHP(NumCompressorsSys))
           TransSystem(TransRefrigSysNum)%CompressorNumHP(1:NumCompressorsSys) =   &
              CompressorLists(ListNum)%CompItemNum(1:NumCompressorsSys)
       ELSEIF (CompNum /= 0) THEN
           NumCompressorsSys = 1
           TransSystem(TransRefrigSysNum)%NumCompressorsHP = 1
           IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%CompressorNumHP))&
                    ALLOCATE(TransSystem(TransRefrigSysNum)%CompressorNumHP(NumCompressorsSys))
           TransSystem(TransRefrigSysNum)%CompressorNumHP(NumCompressorsSys)=CompNum
       END IF
       ! Sum rated capacity of all HP compressors on system
       NominalTotalCompCapHP = 0.0d0
       DO CompIndex=1,NumCompressorsSys
         CompNum = TransSystem(TransRefrigSysNum)%CompressorNumHP(CompIndex)

         IF (Compressor(CompNum)%TransFlag) THEN    !  Calculate nominal capacity of transcritical Compressor
            GCOutletH = GetSupHeatEnthalpyRefrig(TransSystem(TransRefrigSysNum)%RefrigerantName, &
                                                GasCooler(TransSystem(TransRefrigSysNum)%GasCoolerNum(1))%RatedOutletT, &
                                                GasCooler(TransSystem(TransRefrigSysNum)%GasCoolerNum(1))%RatedOutletP, &
                                                RefrigIndex,'GetRefrigerationInput')
            Compressor(CompNum)%NomCap = CurveValue(Compressor(CompNum)%TransCapacityCurvePtr,&
                TransSystem(TransRefrigSysNum)%TEvapDesignMT,GCOutletH)
            NominalTotalCompCapHP = NominalTotalCompCapHP + Compressor(CompNum)%NomCap
            Compressor(CompNum)%NumSysAttach = Compressor(CompNum)%NumSysAttach + 1
         ELSE     !  Subcritical compressor attached to transcritical system - show error
           CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//', '// &
                'No transcritical CO2 compressors are attached to the transcritical refrigeration system, "'// &
                TRIM(TransSystem(TransRefrigSysNum)%Name)//'".')
           ErrorsFound = .TRUE.
         END IF
      END DO
  ENDIF

    ! Read Low Pressure Compressor
    AlphaNum=7
    NumCompressorsSys = 0
    IF((lAlphaBlanks(AlphaNum)) .AND. (TransSystem(TransRefrigSysNum)%TransSysType == 2)) THEN
       ! TwoStage system type is specified but low pressure compressor input is blank
        CALL ShowSevereError(Trim(RoutineName)//TRIM(CurrentModuleObject)//', '// &
             'The transcritical refrigeration system, "'//TRIM(TransSystem(TransRefrigSysNum)%Name)//'", is specified to be '// &
             '"TwoStage", however, the "'//TRIM(cAlphaFieldNames(AlphaNum))//'" '//'is not given.')
        ErrorsFound = .TRUE.
    ELSE IF((.NOT.(lAlphaBlanks(AlphaNum))) .AND. (TransSystem(TransRefrigSysNum)%TransSysType == 1)) THEN
       ! SingleStage system type with low pressure compressors specified. Ignore low pressure compressors
       CALL ShowWarningError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//', '// &
             'The transcritical refrigeration system, "'//TRIM(TransSystem(TransRefrigSysNum)%Name)//'", is specified to be '// &
             '"SingleStage", however, a"'//TRIM(cAlphaFieldNames(AlphaNum))//'" '//  &
                'was found.  The low pressure compressors will be '// &
             'ignored and will not simulated.')
    ELSE IF((.NOT.(lAlphaBlanks(AlphaNum))) .AND. (TransSystem(TransRefrigSysNum)%TransSysType == 2)) THEN
       ! TwoStage system with low pressure compressors specified
       ListNum=FindItemInList(Alphas(AlphaNum),CompressorLists%Name,NumCompressorLists)
       CompNum=FindItemInList(Alphas(AlphaNum),Compressor%Name,NumSimulationCompressors)
       IF((ListNum == 0) .AND. (CompNum == 0)) THEN  ! name doesn't match either a compressor or a compressor list
          CALL ShowSevereError(Trim(RoutineName)//TRIM(CurrentModuleObject)//', "'//&
                             TRIM(cAlphaFieldNames(AlphaNum))//'", has an invalid '//&
                             'or undefined value="'//TRIM(Alphas(AlphaNum))//'".')
          ErrorsFound = .TRUE.
       ELSEIF((ListNum /= 0) .AND. (CompNum /= 0)) THEN  !have compressor list and compressor with same name
         CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//' '//&
                             TRIM(cAlphaFieldNames(AlphaNum))//', has a non-unique name '//&
                             ' used for both Compressor and CompressorList name: "'//TRIM(Alphas(AlphaNum))//'".')
         ErrorsFound = .TRUE.
       ELSE IF(ListNum /= 0)  THEN
           NumCompressorsSys                   = CompressorLists(ListNum)%NumCompressors
           TransSystem(TransRefrigSysNum)%NumCompressorsLP = NumCompressorsSys
           IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%CompressorNumLP))&
                 ALLOCATE(TransSystem(TransRefrigSysNum)%CompressorNumLP(NumCompressorsSys))
           TransSystem(TransRefrigSysNum)%CompressorNumLP(1:NumCompressorsSys) =   &
              CompressorLists(ListNum)%CompItemNum(1:NumCompressorsSys)
       ELSEIF (CompNum /= 0) THEN
           NumCompressorsSys = 1
           TransSystem(TransRefrigSysNum)%NumCompressorsLP = 1
           IF(.NOT. ALLOCATED(TransSystem(TransRefrigSysNum)%CompressorNumLP))&
                    ALLOCATE(TransSystem(TransRefrigSysNum)%CompressorNumLP(NumCompressorsSys))
           TransSystem(TransRefrigSysNum)%CompressorNumLP(NumCompressorsSys)=CompNum
       END IF
       ! Sum rated capacity of all LP compressors on system
       NominalTotalCompCapLP = 0.0d0
       DO CompIndex=1,NumCompressorsSys
         CompNum = TransSystem(TransRefrigSysNum)%CompressorNumLP(CompIndex)
         IF (TransSystem(TransRefrigSysNum)%TransSysType == 2) THEN    !  Calculate capacity of LP compressors
            Compressor(CompNum)%NomCap = CurveValue(Compressor(CompNum)%CapacityCurvePtr,&
                TransSystem(TransRefrigSysNum)%TEvapDesignLT,TransSystem(TransRefrigSysNum)%TEvapDesignMT)
            NominalTotalCompCapLP = NominalTotalCompCapLP + Compressor(CompNum)%NomCap
            Compressor(CompNum)%NumSysAttach = Compressor(CompNum)%NumSysAttach + 1
         END IF
      END DO
  ENDIF

  ! Read Receiver Pressure
  IF (.NOT. lNumericBlanks(1)) THEN
      TransSystem(TransRefrigSysNum)%PReceiver = Numbers(1)
  ELSE  ! Default value receiver pressure = 4000000 Pa
      TransSystem(TransRefrigSysNum)%PReceiver = 4.0d6
  END IF

  ! Check receiver temperature against minimum condensing temperature (from gas cooler input) and design evaporator temperatures
  TransSystem(TransRefrigSysNum)%TReceiver = GetSatTemperatureRefrig(TransSystem(TransRefrigSysNum)%RefrigerantName, &
                                             TransSystem(TransRefrigSysNum)%PReceiver,RefrigIndex,'GetRefrigerationInput')
  IF (TransSystem(TransRefrigSysNum)%TReceiver >   &
         GasCooler(TransSystem(TransRefrigSysNum)%GasCoolerNum(NumGasCoolers))%MinCondTemp) THEN
     CALL ShowWarningError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
             ': The receiver temperature ('//TRIM(RoundSigDigits(TransSystem(TransRefrigSysNum)%TReceiver,2))//&
             'C) is greater than the minimum condensing temperature specified for subcritical operation ('//&
             TRIM(RoundSigDigits(GasCooler(TransSystem(TransRefrigSysNum)%GasCoolerNum(NumGasCoolers))%MinCondTemp,2))//'C).')
     CALL ShowContinueError('  The minimum condensing temperature will be set at 5C greater than the receiver temperature.')
     GasCooler(TransSystem(TransRefrigSysNum)%GasCoolerNum(NumGasCoolers))%MinCondTemp =   &
        TransSystem(TransRefrigSysNum)%TReceiver + 5.0d0
  END IF
  IF (NominalTotalCompCapLP > 0.0d0) THEN
    IF (TransSystem(TransRefrigSysNum)%TReceiver <= TransSystem(TransRefrigSysNum)%TEvapDesignLT) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
                          ': The receiver temperature ('//TRIM(RoundSigDigits(TransSystem(TransRefrigSysNum)%TReceiver,2))//&
                          'C) is less than the design evaporator temperature for the low temperature loads ('//&
                          TRIM(RoundSigDigits(TransSystem(TransRefrigSysNum)%TEvapDesignLT,2))//'C).')
      CALL ShowContinueError('  Ensure that the receiver temperature is sufficiently greater than the design evaporator '//&
                             'temperature for the low temperature loads.')
      CALL ShowContinueError('  A receiver pressure between 3.0 MPa to 4.0 MPa will '//  &
         'typically result in an adequate receiver temperature.')
      ErrorsFound=.TRUE.
    END IF
  END IF
  IF (NominalTotalCompCapHP > 0.0d0) THEN
    IF (TransSystem(TransRefrigSysNum)%TReceiver <= TransSystem(TransRefrigSysNum)%TEvapDesignMT) THEN
      CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
                          ': The receiver temperature ('//TRIM(RoundSigDigits(TransSystem(TransRefrigSysNum)%TReceiver,2))//&
                          'C) is less than the design evaporator temperature for the medium temperature loads ('//&
                          TRIM(RoundSigDigits(TransSystem(TransRefrigSysNum)%TEvapDesignMT,2))//'C).')
      CALL ShowContinueError('  Ensure that the receiver temperature is sufficiently greater than the design evaporator '//&
                             'temperature for the medium temperature loads.')
      CALL ShowContinueError('  A receiver pressure between 3.0 MPa to 4.0 MPa will '//  &
         'typically result in an adequate receiver temperature.')
      ErrorsFound=.TRUE.
    END IF
  END IF

  ! Read subcooler effectiveness
  IF (.NOT. lNumericBlanks(2)) THEN
      TransSystem(TransRefrigSysNum)%SCEffectiveness = Numbers(2)
  ELSE  ! Default value effectiveness = 0.4
      TransSystem(TransRefrigSysNum)%PReceiver = 0.4d0
  END IF
  ! Check subcooler effectiveness value, must be value between 0 and 1
  IF ((TransSystem(TransRefrigSysNum)%SCEffectiveness < 0).OR. &
     (TransSystem(TransRefrigSysNum)%SCEffectiveness > 1)) THEN
     CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
                         ': The value for subcooler effectivness is invalid.  The subcooler effectivenss must be a value'//&
                         ' greater than or equal to zero and less than or equal to one.')
     ErrorsFound=.TRUE.
  END IF

  !Suction piping heat gain - optional
  !  Input UA and identify the Zone containing the bulk of the suction piping
  !  This Zone ID will be used to determine the temperature used for suction piping heat gain.
  !  The pipe heat gains are also counted as cooling credit for the zone.
  !  Zone Id is only required if Sum UA Suction Piping >0.0
  !  Get the Zone and zone node numbers from the zone name entered by the user
  AlphaNum=9  ! Medium temperature suction piping
  TransSystem(TransRefrigSysNum)%SumUASuctionPipingMT=0.d0
  IF(.NOT. lNumericBlanks(3) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
    TransSystem(TransRefrigSysNum)%SumUASuctionPipingMT= Numbers(3)
    TransSystem(TransRefrigSysNum)%SuctionPipeActualZoneNumMT = FindItemInList(Alphas(AlphaNum),Zone%Name,NumOfZones)
    TransSystem(TransRefrigSysNum)%SuctionPipeZoneNodeNumMT = GetSystemNodeNumberForZone(Alphas(AlphaNum))
      IF (TransSystem(TransRefrigSysNum)%SuctionPipeZoneNodeNumMT == 0) THEN
        CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
               '", System Node Number not found for '//TRIM(cAlphaFieldNames(AlphaNum))// &
               ' = "'//TRIM(Alphas(AlphaNum))//'" even though '//TRIM(cNumericFieldNames(3))//&
               ' is greater than zero.')
        CALL ShowContinueError('  The medium temperature suction piping heat gain cannot be calculated unless a Zone is defined '//&
                               'to deterimine the environmental temperature surrounding the piping.')
        ErrorsFound=.TRUE.
      ELSE
        RefrigPresentInZone(TransSystem(TransRefrigSysNum)%SuctionPipeActualZoneNumMT) = .TRUE.
      ENDIF
  ELSEIF(.NOT. lNumericBlanks(3) .AND. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
               '" '//TRIM(cAlphaFieldNames(AlphaNum))//' not found even though '//TRIM(cNumericFieldNames(3))//&
               ' is greater than zero.')
     CALL ShowContinueError('  The medium temperature suction piping heat gain will not be calculated unless a Zone is defined '//&
                            'to determine the environmental temperature surrounding the piping.')
  ELSEIF(lNumericBlanks(3) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
               '" '//TRIM(cAlphaFieldNames(AlphaNum))//' will not be used and suction piping heat gain will'//&
               ' not be calculated because '//TRIM(cNumericFieldNames(3))//&
               ' was blank.')
  END IF  ! Medium temperature suction piping heat gains

  AlphaNum=10  ! Low temperature suction piping
  TransSystem(TransRefrigSysNum)%SumUASuctionPipingLT=0.d0
  IF(.NOT. lNumericBlanks(4) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
    TransSystem(TransRefrigSysNum)%SumUASuctionPipingLT= Numbers(4)
    TransSystem(TransRefrigSysNum)%SuctionPipeActualZoneNumLT = FindItemInList(Alphas(AlphaNum),Zone%Name,NumOfZones)
    TransSystem(TransRefrigSysNum)%SuctionPipeZoneNodeNumLT = GetSystemNodeNumberForZone(Alphas(AlphaNum))
      IF (TransSystem(TransRefrigSysNum)%SuctionPipeZoneNodeNumLT == 0) THEN
        CALL ShowSevereError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
               '", System Node Number not found for '//TRIM(cAlphaFieldNames(AlphaNum))// &
               ' = "'//TRIM(Alphas(AlphaNum))//'" even though '//TRIM(cNumericFieldNames(4))//&
               ' is greater than zero.')
        CALL ShowContinueError('  The low temperature suction piping heat gain cannot be calculated unless a Zone is defined '//&
                               'to deterimine the environmental temperature surrounding the piping.')
        ErrorsFound=.TRUE.
      ELSE
        RefrigPresentInZone(TransSystem(TransRefrigSysNum)%SuctionPipeActualZoneNumLT) = .TRUE.
      ENDIF
  ELSEIF(.NOT. lNumericBlanks(4) .AND. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
               '" '//TRIM(cAlphaFieldNames(AlphaNum))//' not found even though '//TRIM(cNumericFieldNames(4))//&
               ' is greater than zero.')
     CALL ShowContinueError('  The low temperature suction piping heat gain will not be calculated unless a Zone is defined '//&
                            'to determine the environmental temperature surrounding the piping.')
  ELSEIF(lNumericBlanks(4) .AND. .NOT. lAlphaBlanks(AlphaNum)) THEN
     CALL ShowWarningError(TRIM(RoutineName)//TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
               '" '//TRIM(cAlphaFieldNames(AlphaNum))//' will not be used and suction piping heat gain will'//&
               ' not be calculated because '//TRIM(cNumericFieldNames(4))//&
               ' was blank.')
  END IF  ! Low temperature suction piping heat gains

 AlphaNum=11
 IF (.NOT. lAlphaBlanks(AlphaNum))  TransSystem(TransRefrigSysNum)%EndUseSubcategory = Alphas(AlphaNum)

!Compare the rated capacity of compressor, condenser, and cases.
! Note, rated capacities can be far off from operating capacities, but rough check.
   NominalCondCap=GasCooler(TransSystem(TransRefrigSysNum)%GasCoolerNum(1))%RatedCapacity
   NominalTotalCompCap=NominalTotalCompCapHP + NominalTotalCompCapLP
   IF((NominalTotalCompCap < (0.7d0*NominalTotalCoolingCap)) .OR. &
      (NominalCondCap < (1.3d0*NominalTotalCoolingCap)))  THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(TransSystem(TransRefrigSysNum)%Name)//&
                          '", You may wish to check the system sizing.')
    CALL ShowContinueError('Total nominal cooling capacity is '//&
                          TRIM(RoundSigDigits(NominalTotalCoolingCap,0))//'W. Condenser capacity is '//&
                          TRIM(RoundSigDigits(NominalCondCap,0))//'W. Nominal compressor capacity is '//&
                          TRIM(RoundSigDigits(NominalTotalCompCap,0))//'W.')

   END IF

 END DO  ! Transcritical refrigeration systems

END IF  !(NumTransRefrigSystems > 0)

  DEALLOCATE(DayValues)
  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFieldNames)
  DEALLOCATE(cNumericFieldNames)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

 IF(NumSimulationCases > 0) THEN
 ! Find unused and non-unique display case objects to report in eio and err file and sum
 !    all HVAC RA fractions and write error message if greater than 1 for any zone
  DO ZoneIndex = 1,NumOfZones  !numofzones from dataglobals
    TempRAFraction = CaseRAFraction(ZoneIndex)%TotalCaseRAFraction
    DO CaseNum = 1,NumSimulationCases
      ! TempRaFraction already includes contributions from ALL cases in zone
      ! Want to delete portion from unused cases (numsysattach = 0)that will never be simulated
      IF(RefrigCase(CaseNum)%ActualZoneNum /= ZoneIndex .OR. &
         RefrigCase(CaseNum)%NumSysAttach > 0) CYCLE
         TempRAFraction = TempRAFraction - RefrigCase(CaseNum)%RAFrac
    END DO !NumSimulationCases
    IF (TempRAFraction > 1.0d0) THEN
      CALL ShowSevereError(TRIM(RoutineName)//': Refrigeration:Case'// &
           ', Refrigerated case return air fraction for all cases in zone="' &
         //TRIM(CaseRAFraction(ZoneIndex)%ZoneName)//'" is greater than 1.0.')
      !check in comment, can't use "currentModuleObject" because not in get input subroutine where that is known
      ErrorsFound = .TRUE.
    END IF
  END DO !ZoneIndex=1,NumofZones

  DEALLOCATE(CaseRAFraction)  !only used for input check just completed
!check for cases not connected to systems and cases connected
!more than once (twice in a system or to more than one system)

  NumunusedRefrigCases = 0
  DO CaseNum = 1,NumSimulationCases
      IF(RefrigCase(CaseNum)%NumSysAttach == 1) CYCLE
      IF(RefrigCase(CaseNum)%NumSysAttach < 1) THEN
        NumunusedRefrigCases = NumunusedRefrigCases + 1
        IF (DisplayExtraWarnings) THEN
          !  individual case names listed if DisplayExtraWarnings option selected
          CALL ShowWarningError(TRIM(RoutineName)//': Refrigeration:Case="'// &
               TRIM(RefrigCase(CaseNum)%Name)//'" unused. ')
        END IF !display extra warnings - give a list of unused cases
      END IF !unused case
      IF(RefrigCase(CaseNum)%NumSysAttach > 1)THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError(TRIM(RoutineName)//': Refrigeration:Case="'//&
               TRIM(RefrigCase(CaseNum)%Name)//'", Same refrigerated case name referenced ')
          CALL ShowContinueError(' by more than one refrigeration system and/or compressor rack.')
      END IF ! if looking for same case attached to multiple systems/racks
  END DO !NumSimulationCases

  IF((NumunusedRefrigCases > 0) .AND. (.NOT. DisplayExtraWarnings)) THEN
    !  write to error file,
    !  summary number of unused cases given if DisplayExtraWarnings option not selected
    CALL ShowWarningError('Refrigeration:Case -> '//TRIM(RoundSigDigits(NumunusedRefrigCases))//&
                          ' unused refrigerated case(s) found during input processing.')
    CALL ShowContinueError('  These refrigerated cases are in the input file but are not connected to a ')
    CALL ShowContinueError('  Refrigeration:CompressorRack, Refrigeration:System, or Refrigeration:SecondarySystem object.')
    CALL ShowContinueError('  These unused refrigeration cases will not be simulated.')
    CALL ShowContinueError('  Use Output:Diagnostics,DisplayUnusedObjects; to see them. ')
  END IF  !NumunusedRefrigCases
END IF !numsimulation cases > 0

IF(NumSimulationCompressors > 0) THEN
!check for compressors not connected to systems and compressors connected more than once
! (twice in a system or to more than one system)
  NumunusedCompressors = 0
  DO CompNum=1,NumSimulationCompressors
      IF(Compressor(CompNum)%NumSysAttach == 1) CYCLE
      IF(Compressor(CompNum)%NumSysAttach < 1)THEN
        NumunusedCompressors = NumunusedCompressors + 1
        IF (DisplayExtraWarnings) THEN
          !  individual compressor names listed if DisplayExtraWarnings option selected
          CALL ShowWarningError(TRIM(RoutineName)//': Refrigeration:Compressor="'// &
               TRIM(Compressor(CompNum)%Name)//'" unused. ')
        END IF !display extra warnings - give a list of unused compressors
      END IF !unused compressor
      IF(Compressor(CompNum)%NumSysAttach > 1)THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError(TRIM(RoutineName)//': Refrigeration:Compressor="'// &
                TRIM(Compressor(CompNum)%Name)//'", Same refrigeration compressor name referenced')
          CALL ShowContinueError(' by more than one refrigeration system.')
      END IF ! looking for same compressor attached to multiple systems/racks
  END DO !NumSimulationCompressors

  IF((NumunusedCompressors > 0) .AND. (.NOT. DisplayExtraWarnings))  THEN
    !  write to error file,
    !  summary number of unused compressors given if DisplayExtraWarnings option not selected
    CALL ShowWarningError('Refrigeration:Compressor -> '//TRIM(RoundSigDigits(NumunusedCompressors))//&
                          ' unused refrigeration compressor(s) found during input processing.')
    CALL ShowContinueError('  Those refrigeration compressors are in the input file but are not connected to a '// &
                           'Refrigeration:System object.')
    CALL ShowContinueError('   These unused refrigeration compressors will not be simulated.')
    CALL ShowContinueError('   Use Output:Diagnostics,DisplayUnusedObjects; to see them. ')
  END IF  !NumunusedCompressors
END IF !NumSimulationCompressors > 0

IF(NumSimulationWalkIns > 0) THEN
!check for refrigeration WalkIns not connected to any systems and
!  refrigeration WalkIns connected more than once
  NumunusedWalkIns = 0
  DO WalkInNum=1,NumSimulationWalkIns
      IF(WalkIn(WalkInNum)%NumSysAttach == 1) CYCLE
      IF(WalkIn(WalkInNum)%NumSysAttach < 1)THEN
        NumunusedWalkIns = NumunusedWalkIns + 1
        IF (DisplayExtraWarnings) THEN
          !  individual walkin names listed if DisplayExtraWarnings option selected
          CALL ShowWarningError(TRIM(RoutineName)//': Refrigeration:WalkIn="'// &
               TRIM(WalkIn(WalkInNum)%Name)//'" unused. ')
        END IF !display extra warnings - give a list of unused WalkIns
      END IF !unused walkin
      IF(WalkIn(WalkInNum)%NumSysAttach > 1)THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError(TRIM(RoutineName)//': Refrigeration:WalkIn="'// &
               TRIM(WalkIn(WalkInNum)%Name)//'", Same Refrigeration WalkIn name referenced')
          CALL ShowContinueError(' by more than one refrigeration system and/or compressor rack.')
      END IF ! if looking for same walk in attached to multiple systems/racks
  END DO !NumSimulationWalkIns

  IF((NumunusedWalkIns > 0) .AND. (.NOT. DisplayExtraWarnings))  THEN
    !  write to error file,
    !  summary number of unused walkins given if DisplayExtraWarnings option not selected
    CALL ShowWarningError(TRIM(RoutineName)//'Refrigeration:WalkIn -> '//TRIM(RoundSigDigits(NumunusedWalkIns))//&
                          ' unused refrigeration WalkIns found during input processing.')
    CALL ShowContinueError('   Those refrigeration WalkIns are in the input file but are not connected to a ')
    CALL ShowContinueError('   Refrigeration:CompressorRack, Refrigeration:System or Refrigeration:SecondarySystem object.')
    CALL ShowContinueError('   These unused refrigeration WalkIns will not be simulated.')
    CALL ShowContinueError('   Use Output:Diagnostics,DisplayUnusedObjects; to see them. ')
  END IF  !NumunusedWalkIns
END IF !NumSimulationWalkIns > 0

IF(NumSimulationRefrigAirChillers > 0) THEN
!check for air chillers not connected to any systems and
!  air chillers connected more than once
  NumunusedCoils = 0
  DO CoilNum=1,NumSimulationRefrigAirChillers
      IF(WarehouseCoil(CoilNum)%NumSysAttach == 1) CYCLE
      IF(WarehouseCoil(CoilNum)%NumSysAttach < 1)THEN
        NumunusedWalkIns = NumunusedWalkIns + 1
        IF (DisplayExtraWarnings) THEN
          !  individual walkin names listed if DisplayExtraWarnings option selected
          CALL ShowWarningError(TRIM(RoutineName)//': Refrigeration:AirChiller="'// &
               TRIM(WarehouseCoil(CoilNum)%Name)//'" unused. ')
        END IF !display extra warnings - give a list of unused chillers
      END IF !unused chiller
      IF(WarehouseCoil(CoilNum)%NumSysAttach > 1)THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError(TRIM(RoutineName)//': Refrigeration:AirChiller="'// &
               TRIM(WarehouseCoil(CoilNum)%Name)//'", Same Refrigeration Air Chiller name referenced')
          CALL ShowContinueError(' by more than one refrigeration system and/or compressor rack.')
      END IF ! if looking for same walk in attached to multiple systems/racks
  END DO !NumSimulationRefrigAirchillers

  IF((NumunusedCoils > 0) .AND. (.NOT. DisplayExtraWarnings))  THEN
    !  write to error file,
    !  summary number of unused air chillers given if DisplayExtraWarnings option not selected
    CALL ShowWarningError(RoutineName//'Refrigeration:AirChiller -> '//TRIM(RoundSigDigits(NumunusedCoils))//&
                          ' unused refrigeration air chillers found during input processing.')
    CALL ShowContinueError('   Those refrigeration air chillers are in the input file but are not connected to a ')
    CALL ShowContinueError('   Refrigeration:CompressorRack, Refrigeration:System or Refrigeration:SecondarySystem object.')
    CALL ShowContinueError('   These unused refrigeration air chillers will not be simulated.')
    CALL ShowContinueError('   Use Output:Diagnostics,DisplayUnusedObjects; to see them. ')
  END IF  !NumunusedAirChllerss
END IF !NumSimulationAirChillers > 0

IF(NumSimulationSecondarySystems > 0) THEN
!check for refrigeration Secondarys not connected to detailed systems and
!  refrigeration Secondarys connected more than once
  NumunusedSecondarys = 0
  DO SecondaryNum=1,NumSimulationSecondarySystems
      IF(Secondary(SecondaryNum)%NumSysAttach == 1) CYCLE
      IF(Secondary(SecondaryNum)%NumSysAttach < 1) THEN
        NumunusedSecondarys = NumunusedSecondarys + 1
        IF (DisplayExtraWarnings) THEN
          !  individual secondary names listed if DisplayExtraWarnings option selected
          CALL ShowWarningError(TRIM(RoutineName)//': Refrigeration:Secondary="'// &
               TRIM(Secondary(SecondaryNum)%Name)//'" unused. ')
        END IF !display extra warnings - give a list of unused Secondaries
      END IF !unused secondary
      IF(Secondary(SecondaryNum)%NumSysAttach > 1)THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError(TRIM(RoutineName)//': Refrigeration:Secondary="'// &
               TRIM(Secondary(SecondaryNum)%Name)//'", Same Refrigeration Secondary name referenced')
          CALL ShowContinueError('   by more than one refrigeration system')
      END IF ! looking for same secondary loop attached to multiple systems/racks
  END DO !NumSimulationSecondarys

  IF((NumunusedSecondarys > 0) .AND. (.NOT. DisplayExtraWarnings))  THEN
    !  write to error file,
    !  summary number of unused secondaries given if DisplayExtraWarnings option not selected
    CALL ShowWarningError(RoutineName//'Refrigeration:Secondary -> '//TRIM(RoundSigDigits(NumunusedSecondarys))//&
                          ' unused refrigeration Secondary Loops found during input processing.')
    CALL ShowContinueError('  Those refrigeration Secondary Loops are in the input file but are not connected to a'//&
                          ' refrigeration system.')
    CALL ShowContinueError('   These unused refrigeration secondaries will not be simulated.')
    CALL ShowContinueError('   Use Output:Diagnostics,DisplayUnusedObjects; to see them. ')
  END IF  !NumunusedSecondarys
END IF !NumSimulationSecondarySystems > 0

IF(NumRefrigCondensers > 0) THEN
  !Check for presence of shared condensers and for unused condensers
  !     - determines number of loops through refrigeration simulation
  !       because of dependence of performance on total condenser load
  NumSimulationSharedCondensers = 0
  NumunusedCondensers = 0
  DO CondNum = 1,NumRefrigCondensers
    IF(Condenser(CondNum)%NumSysAttach == 1) CYCLE
    IF(Condenser(CondNum)%NumSysAttach < 1) THEN
      NumunusedCondensers = NumunusedCondensers + 1
      IF (DisplayExtraWarnings) THEN
        !  individual condenser names listed if DisplayExtraWarnings option selected
        CALL ShowWarningError(TRIM(RoutineName)//': Refrigeration:Condenser="'// &
               TRIM(Condenser(CondNum)%Name)//'" unused. ')
      END IF !display extra warnings - give a list of unused condensers
    END IF !unused condenser
    IF(Condenser(CondNum)%NumSysAttach > 1) THEN
       NumSimulationSharedCondensers = NumSimulationSharedCondensers + 1
    END IF ! looking for shared condensers
  END DO !CondNum

  IF((NumunusedCondensers > 0) .AND. (.NOT. DisplayExtraWarnings))  THEN
    !  write to error file,
    !  summary number of unused condensers given if DisplayExtraWarnings option not selected
    CALL ShowWarningError(TRIM(RoutineName)//'Refrigeration condenser -> '//TRIM(RoundSigDigits(NumunusedCondensers))//&
                          ' unused refrigeration condensers found during input processing.')
    CALL ShowContinueError('  Those refrigeration condensers are in the input file but are not connected to a'//&
                          ' refrigeration system.')
    CALL ShowContinueError('   These unused refrigeration condensers will not be simulated.')
    CALL ShowContinueError('   Use Output:Diagnostics,DisplayUnusedObjects; to see them. ')
  END IF  !NumunusedCondensers and displayextra warnings
END IF !NumRefrigCondensers > 0

IF(NumSimulationGasCooler > 0) THEN
  !Check for presence of shared gas coolers and for unused gas coolers
  NumSimulationSharedGasCoolers = 0
  NumunusedGasCoolers = 0
  DO GCNum = 1,NumSimulationGasCooler
    IF(GasCooler(GCNum)%NumSysAttach == 1) CYCLE
    IF(GasCooler(GCNum)%NumSysAttach < 1) THEN
      NumunusedGasCoolers = NumunusedGasCoolers + 1
      IF (DisplayExtraWarnings) THEN
        !  individual gas cooler names listed if DisplayExtraWarnings option selected
        CALL ShowWarningError(TRIM(RoutineName)//': Refrigeration:GasCooler="'// &
               TRIM(GasCooler(GCNum)%Name)//'" unused. ')
      END IF !display extra warnings - give a list of unused gas coolers
    END IF !unused gas cooler
    IF(GasCooler(GCNum)%NumSysAttach > 1) THEN
       NumSimulationSharedGasCoolers = NumSimulationSharedGasCoolers + 1
    END IF ! looking for shared gas coolers
  END DO !GCNum

  IF((NumunusedGasCoolers > 0) .AND. (.NOT. DisplayExtraWarnings))  THEN
    !  write to error file,
    !  summary number of unused gas coolers given if DisplayExtraWarnings option not selected
    CALL ShowWarningError(TRIM(RoutineName)//'Refrigeration gas cooler -> '// &
                          TRIM(RoundSigDigits(NumunusedGasCoolers))//&
                          ' unused refrigeration gas cooler(s) found during input processing.')
    CALL ShowContinueError('  These refrigeration gas coolers are in the input file but are not connected to a'//&
                          ' refrigeration system.')
    CALL ShowContinueError('  These unused refrigeration gas coolers will not be simulated.')
    CALL ShowContinueError('  Use Output:Diagnostics,DisplayUnusedObjects; to see them. ')
  END IF  !NumunusedGasCoolers and displayextra warnings
END IF !NumSimulationGasCooler > 0

!echo input to eio file.
CALL ReportRefrigerationComponents()

IF (ErrorsFound) THEN
  CALL ShowFatalError(TRIM(RoutineName)//' Previous errors cause program termination')
ENDIF

RETURN
END SUBROUTINE GetRefrigerationInput


!***************************************************************************************************
!***************************************************************************************************


SUBROUTINE SetupReportInput
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Oct/Nov 2004
          !       MODIFIED       Hudson, ORNL July 2007, Stovall, ORNL, 2008 and 09
          !       MODIFIED       Fricke, ORNL, Fall 2011, added transcritical CO2 refrigeration system variables
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set up the report variables.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces
  USE DataHeatBalance,   ONLY: Zone, IntGainTypeOf_RefrigerationCase, &
                               IntGainTypeOf_RefrigerationSystemSuctionPipe, &
                               IntGainTypeOf_RefrigerationCompressorRack, &
                               IntGainTypeOf_RefrigerationSystemAirCooledCondenser, &
                               IntGainTypeOf_RefrigerationSecondaryReceiver, &
                               IntGainTypeOf_RefrigerationSecondaryPipe, &
                               IntGainTypeOf_RefrigerationWalkIn, &
                               IntGainTypeOf_RefrigerationTransSysAirCooledGasCooler, &
                               IntGainTypeOf_RefrigerationTransSysSuctionPipeMT, &
                               IntGainTypeOf_RefrigerationTransSysSuctionPipeLT


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 ! LOGICAL, SAVE :: MyBeginEnvrnFlag = .TRUE.
 ! INTEGER       :: SystemID
INTEGER   :: CaseNum = 0
INTEGER   :: CoilNum = 0
INTEGER   :: SecondNum = 0
INTEGER   :: WalkInNum = 0
INTEGER   :: RackNum = 0
INTEGER   :: RefrigSysNum = 0
INTEGER   :: CompNum = 0
INTEGER   :: CompIndex = 0
INTEGER   :: CondNum = 0
INTEGER   :: GCNum = 0
INTEGER   :: SubcoolNum = 0
INTEGER   :: ZoneID = 0
CHARACTER(len=MaxNameLength)   :: Walkin_and_zone_name =' ' ! concat name for walk-in/zone credit reporting

IF(NumSimulationCases > 0) THEN
  ! Setup Report Variables for simulated Refrigerated Case (do not report unused cases)
  ! CurrentModuleObject='Refrigeration:Case'
  DO CaseNum=1,NumSimulationCases
   IF(RefrigCase(CaseNum)%NumSysAttach == 1) THEN
    CALL SetupOutputVariable('Refrigeration Case Evaporator Total Cooling Rate [W]', &
                              RefrigCase(CaseNum)%TotalCoolingLoad,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Evaporator Total Cooling Energy [J]', &
                              RefrigCase(CaseNum)%TotalCoolingEnergy,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name,ResourceTypeKey='ENERGYTRANSFER', &
                              EndUseKey='REFRIGERATION',GroupKey='Building',ZoneKey=RefrigCase(CaseNum)%ZoneName)
    CALL SetupOutputVariable('Refrigeration Case Evaporator Sensible Cooling Rate [W]', &
                              RefrigCase(CaseNum)%SensCoolingEnergyRate,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Evaporator Sensible Cooling Energy [J]', &
                              RefrigCase(CaseNum)%SensCoolingEnergy,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Evaporator Latent Cooling Rate [W]', &
                              RefrigCase(CaseNum)%LatCoolingEnergyRate,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Evaporator Latent Cooling Energy [J]', &
                              RefrigCase(CaseNum)%LatCoolingEnergy,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name)

    CALL SetupOutputVariable('Refrigeration Case Zone Sensible Cooling Rate [W]', &
                              RefrigCase(CaseNum)%SensZoneCreditCoolRate,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Zone Sensible Cooling Energy [J]', &
                              RefrigCase(CaseNum)%SensZoneCreditCool,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Zone Sensible Heating Rate [W]', &
                              RefrigCase(CaseNum)%SensZoneCreditHeatRate,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Zone Sensible Heating Energy [J]', &
                              RefrigCase(CaseNum)%SensZoneCreditHeat,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name)

    CALL SetupOutputVariable('Refrigeration Case Zone Latent Rate [W]', &
                              RefrigCase(CaseNum)%LatZoneCreditRate,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Zone Latent Energy [J]', &
                              RefrigCase(CaseNum)%LatZoneCredit,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name)

    CALL SetupOutputVariable('Refrigeration Case Return Air Sensible Cooling Rate [W]', &
                              RefrigCase(CaseNum)%SensHVACCreditCoolRate,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Return Air Sensible Cooling Energy [J]', &
                              RefrigCase(CaseNum)%SensHVACCreditCool,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Return Air Sensible Heating Rate [W]', &
                              RefrigCase(CaseNum)%SensHVACCreditHeatRate,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Return Air Sensible Heating Energy [J]', &
                              RefrigCase(CaseNum)%SensHVACCreditHeat,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name)

    CALL SetupOutputVariable('Refrigeration Case Return Air Latent Rate [W]', &
                              RefrigCase(CaseNum)%LatHVACCreditRate,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Return Air Latent Energy [J]', &
                              RefrigCase(CaseNum)%LatHVACCredit,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name)

    CALL SetupOutputVariable('Refrigeration Case Evaporator Fan Electric Power [W]', &
                              RefrigCase(CaseNum)%ElecFanPower,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Evaporator Fan Electric Energy [J]', &
                              RefrigCase(CaseNum)%ElecFanConsumption,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name,ResourceTypeKey='ELECTRICITY', &
                              EndUseKey='REFRIGERATION',GroupKey='Building',ZoneKey=RefrigCase(CaseNum)%ZoneName, &
                              EndUseSubKey='General')
    CALL SetupOutputVariable('Refrigeration Case Lighting Electric Power [W]', &
                              RefrigCase(CaseNum)%ElecLightingPower,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
    CALL SetupOutputVariable('Refrigeration Case Lighting Electric Energy [J]', &
                              RefrigCase(CaseNum)%ElecLightingConsumption,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name,ResourceTypeKey='ELECTRICITY', &
                              EndUseKey='REFRIGERATION',GroupKey='Building',ZoneKey=RefrigCase(CaseNum)%ZoneName, &
                              EndUseSubKey='General')

! Report defrost energy curve value only for cases having electric or hot-gas defrost with temperature termination
    IF(RefrigCase(CaseNum)%DefrostType == DefElectricTerm .OR. &
       RefrigCase(CaseNum)%DefrostType == DefHotFluidTerm )THEN
      CALL SetupOutputVariable('Refrigeration Case Defrost Energy Correction Curve Value []', &
                              RefrigCase(CaseNum)%DefEnergyCurveValue,'Zone','Average',RefrigCase(CaseNum)%Name)
    END IF

    CALL SetupOutputVariable('Refrigeration Case Latent Credit Curve Value []', RefrigCase(CaseNum)%LatEnergyCurveValue, &
                             'Zone','Average',RefrigCase(CaseNum)%Name)

! Report only for cases having anti-sweat heaters
    IF (RefrigCase(CaseNum)%AntiSweatControlType > ASNone) THEN
      CALL SetupOutputVariable('Refrigeration Case Anti Sweat Electric Power [W]', &
                              RefrigCase(CaseNum)%ElecAntiSweatPower,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
      CALL SetupOutputVariable('Refrigeration Case Anti Sweat Electric Energy [J]', &
                              RefrigCase(CaseNum)%ElecAntiSweatConsumption,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name,ResourceTypeKey='ELECTRICITY', &
                              EndUseKey='REFRIGERATION',GroupKey='Building',ZoneKey=RefrigCase(CaseNum)%ZoneName, &
                              EndUseSubKey='General')
    END IF

! Report only for cases using electric defrost

    IF (RefrigCase(CaseNum)%DefrostType == DefElectric .OR. RefrigCase(CaseNum)%DefrostType == DefElectricOnDemand .OR. &
              RefrigCase(CaseNum)%DefrostType == DefElectricTerm ) THEN
      CALL SetupOutputVariable('Refrigeration Case Defrost Electric Power [W]', &
                              RefrigCase(CaseNum)%ElecDefrostPower,'Zone','Average',&
                              RefrigCase(CaseNum)%Name)
      CALL SetupOutputVariable('Refrigeration Case Defrost Electric Energy [J]', &
                              RefrigCase(CaseNum)%ElecDefrostConsumption,'Zone','Sum',&
                              RefrigCase(CaseNum)%Name,ResourceTypeKey='ELECTRICITY', &
                              EndUseKey='REFRIGERATION',GroupKey='Building',ZoneKey=RefrigCase(CaseNum)%ZoneName, &
                              EndUseSubKey='General')
    END IF

    !register refrigeration case credits as internal gains
    IF (RefrigCase(CaseNum)%ActualZoneNum > 0) THEN
       CALL SetupZoneInternalGain(RefrigCase(CaseNum)%ActualZoneNum, &
                     'Refrigeration:Case',  &
                     RefrigCase(CaseNum)%Name , &
                     IntGainTypeOf_RefrigerationCase,         &
                     ConvectionGainRate          = RefrigCase(CaseNum)%SensZoneCreditRate , &
                     ReturnAirConvectionGainRate = RefrigCase(CaseNum)%SensHVACCreditRate , &
                     LatentGainRate              = RefrigCase(CaseNum)%LatZoneCreditRate  , &
                     ReturnAirLatentGainRate     = RefrigCase(CaseNum)%LatHVACCreditRate )
    ENDIF
   END IF !END IF (.NOT. RefrigCase(CaseNum)%unusedCase)
  END DO
END IF !NumSimulationCases > 0

IF(NumSimulationWalkIns > 0) THEN
  ! Setup Report Variables for simulated  Walk In (do not report unused WalkIns)
  ! CurrentModuleObject='Refrigeration:WalkIn'
  DO  WalkInNum=1,NumSimulationWalkIns
   IF( WalkIn( WalkInNum)%NumSysAttach == 1) THEN  !ensure no unuseds reported
    CALL SetupOutputVariable('Refrigeration Walk In Evaporator Total Cooling Rate [W]', &
                               WalkIn( WalkInNum)%TotalCoolingLoad,'Zone','Average',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Evaporator Total Cooling Energy [J]', &
                               WalkIn( WalkInNum)%TotalCoolingEnergy,'Zone','Sum',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Evaporator Sensible Cooling Rate [W]', &
                               WalkIn( WalkInNum)%TotSensCoolingEnergyRate,'Zone','Average',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Evaporator Sensible Cooling Energy [J]', &
                               WalkIn( WalkInNum)%TotSensCoolingEnergy,'Zone','Sum',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Evaporator Latent Cooling Rate [W]', &
                               WalkIn( WalkInNum)%TotLatCoolingEnergyRate,'Zone','Average',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Evaporator Latent Cooling Energy [J]', &
                               WalkIn( WalkInNum)%TotLatCoolingEnergy,'Zone','Sum',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Ancillary Electric Power [W]', &
                               WalkIn( WalkInNum)%TotalElecPower,'Zone','Average',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Ancillary Electric Energy [J]', &
                               WalkIn( WalkInNum)%TotalElecConsumption,'Zone','Sum',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Fan Electric Power [W]', &
                               WalkIn( WalkInNum)%ElecFanPower,'Zone','Average',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Fan Electric Energy [J]', &
                               WalkIn( WalkInNum)%ElecFanConsumption,'Zone','Sum',&
                               WalkIn( WalkInNum)%Name,ResourceTypeKey='ELECTRICITY', &
                              EndUseKey='REFRIGERATION',GroupKey='Building', &
                              EndUseSubKey='General')
    CALL SetupOutputVariable('Refrigeration Walk In Lighting Electric Power [W]', &
                               WalkIn( WalkInNum)%ElecLightingPower,'Zone','Average',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Lighting Electric Energy [J]', &
                               WalkIn( WalkInNum)%ElecLightingConsumption,'Zone','Sum',&
                               WalkIn( WalkInNum)%Name,ResourceTypeKey='ELECTRICITY', &
                               EndUseKey='REFRIGERATION',GroupKey='Building', &
                               EndUseSubKey='General')
    CALL SetupOutputVariable('Refrigeration Walk In Heater Electric Power [W]', &
                               WalkIn( WalkInNum)%ElecHeaterPower,'Zone','Average',&
                               WalkIn( WalkInNum)%Name)
    CALL SetupOutputVariable('Refrigeration Walk In Heater Electric Energy [J]', &
                               WalkIn( WalkInNum)%ElecHeaterConsumption,'Zone','Sum',&
                               WalkIn( WalkInNum)%Name,ResourceTypeKey='ELECTRICITY', &
                               EndUseKey='REFRIGERATION',GroupKey='Building', &
                               EndUseSubKey='General')

! Report only for WalkIns using electric defrost
    IF ( WalkIn( WalkInNum)%DefrostType == WalkInDefrostElec) THEN
      CALL SetupOutputVariable('Refrigeration Walk In Defrost Electric Power [W]', &
                               WalkIn( WalkInNum)%ElecDefrostPower,'Zone','Average',&
                               WalkIn( WalkInNum)%Name)
      CALL SetupOutputVariable('Refrigeration Walk In Defrost Electric Energy [J]', &
                               WalkIn( WalkInNum)%ElecDefrostConsumption,'Zone','Sum',&
                               WalkIn( WalkInNum)%Name,ResourceTypeKey='ELECTRICITY', &
                              EndUseKey='REFRIGERATION',GroupKey='Building', &
                              EndUseSubKey='General')
    END IF

! Report walkin variables that are specified for each zone exposed to the walkin
! For "IDOut" variable in SetupOutputVariable, need to create a single name that includes
!    both the walk-in name and the zone name - see "Walkin_and_zone_name" concatination
!    This new variable name is important if using an rvi file!
    DO ZoneID = 1,WalkIn(WalkInNum)%NumZones

      Walkin_and_zone_name = TRIM(WalkIn(WalkInNum)%Name)//'InZone'//TRIM(WalkIn(WalkInNum)%ZoneName(ZoneID))

      CALL SetupOutputVariable('Refrigeration Walk In Zone Sensible Cooling Rate [W]', &
                               WalkIn( WalkInNum)%SensZoneCreditCoolRate(ZoneID),'Zone','Average',&
                               Walkin_and_zone_name)
      CALL SetupOutputVariable('Refrigeration Walk In Zone Sensible Cooling Energy [J]', &
                               WalkIn( WalkInNum)%SensZoneCreditCool(ZoneID),'Zone','Sum',&
                               Walkin_and_zone_name)
      CALL SetupOutputVariable('Refrigeration Walk In Zone Sensible Heating Rate [W]', &
                               WalkIn( WalkInNum)%SensZoneCreditHeatRate(ZoneID),'Zone','Average',&
                               Walkin_and_zone_name)
      CALL SetupOutputVariable('Refrigeration Walk In Zone Sensible Heating Energy [J]', &
                               WalkIn( WalkInNum)%SensZoneCreditHeat(ZoneID),'Zone','Sum',&
                               Walkin_and_zone_name)
      CALL SetupOutputVariable('Refrigeration Walk In Zone Latent Rate [W]', &
                               WalkIn( WalkInNum)%LatZoneCreditRate(ZoneID),'Zone','Average',&
                               Walkin_and_zone_name)
      CALL SetupOutputVariable('Refrigeration Walk In Zone Latent Energy [J]', &
                               WalkIn( WalkInNum)%LatZoneCredit(ZoneID),'Zone','Sum',&
                               Walkin_and_zone_name)

      IF(WalkIn(WalkInNum)%ZoneNum(ZoneID) > 0)&
        CALL SetupZoneInternalGain(WalkIn(WalkInNum)%ZoneNum(ZoneID) , &
             'Refrigeration:WalkIn',  &
             Walkin_and_zone_name , &
             IntGainTypeOf_RefrigerationWalkIn,         &
             ConvectionGainRate   = WalkIn(WalkInNum)%SensZoneCreditRate(ZoneID), &
             LatentGainRate       = WalkIn(WalkInNum)%LatZoneCreditRate(ZoneID) )

    END DO ! ZoneID
  END IF !(.NOT.  WalkIn( WalkInNum)%unusedWalkIn)
  END DO ! NumSimulationWalkIns
END IF ! NumSimulationWalkIns > 0

IF(NumSimulationRefrigAirChillers > 0) THEN
  ! Setup Report Variables for simulated Warehouse coils (do not report unused warehouse coils)
  ! CurrentModuleObject='Refrigeration:AirChiller'
  DO  CoilNum=1,NumSimulationRefrigAirChillers
   IF( WarehouseCoil(CoilNum)%NumSysAttach == 1) THEN  !ensure no unuseds reported
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Total Cooling Rate [W]', &
                               WarehouseCoil(CoilNum)%TotalCoolingLoad,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Total Cooling Energy [J]', &
                               WarehouseCoil(CoilNum)%TotalCoolingEnergy,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Sensible Cooling Rate [W]', &
                               WarehouseCoil(CoilNum)%SensCoolingEnergyRate,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Sensible Cooling Energy [J]', &
                               WarehouseCoil(CoilNum)%SensCoolingEnergy,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Latent Cooling Rate [W]', &
                               WarehouseCoil(CoilNum)%LatCreditRate,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Latent Cooling Energy [J]', &
                               WarehouseCoil(CoilNum)%LatCreditEnergy,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Water Removed Mass Flow Rate [kg/s]', &
                               WarehouseCoil(CoilNum)%LatKgPerS_ToZone,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Total Electric Power [W]', &
                               WarehouseCoil(CoilNum)%TotalElecPower,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Total Electric Energy [J]', &
                               WarehouseCoil(CoilNum)%TotalElecConsumption,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name) !components are metered seperately
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Fan Electric Power [W]', &
                               WarehouseCoil(CoilNum)%ElecFanPower,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Fan Electric Energy [J]', &
                               WarehouseCoil(CoilNum)%ElecFanConsumption,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name,ResourceTypeKey='ELECTRICITY', &
                              EndUseKey='REFRIGERATION',GroupKey='Building', &
                              EndUseSubKey='General')
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Heater Electric Power [W]', &
                               WarehouseCoil(CoilNum)%ElecHeaterPower,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Heater Electric Energy [J]', &
                               WarehouseCoil(CoilNum)%ElecHeaterConsumption,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name,ResourceTypeKey='ELECTRICITY', &
                               EndUseKey='REFRIGERATION',GroupKey='Building', &
                               EndUseSubKey='General')
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Sensible Heat Ratio []', &
                               WarehouseCoil(CoilNum)%SensHeatRatio,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Frost Accumulation Mass [Kg]', &
                               WarehouseCoil(CoilNum)%KgFrost,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Zone Total Cooling Rate [W]', &
                               WarehouseCoil(CoilNum)%ReportTotalCoolCreditRate,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Zone Total Cooling Energy [J]', &
                               WarehouseCoil(CoilNum)%ReportTotalCoolCreditEnergy,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Zone Sensible Cooling Rate [W]', &
                               WarehouseCoil(CoilNum)%ReportSensCoolCreditRate,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Zone Sensible Cooling Energy [J]', &
                               WarehouseCoil(CoilNum)%ReportSensCoolCreditEnergy,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Zone Heating Rate [W]', &
                               WarehouseCoil(CoilNum)%ReportHeatingCreditRate,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
    CALL SetupOutputVariable('Refrigeration Zone Air Chiller Zone Heating Energy [J]', &
                               WarehouseCoil(CoilNum)%ReportHeatingCreditEnergy,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name)

    ! Report only for Warehouse coils using electric defrost
    IF ( WarehouseCoil(CoilNum)%DefrostType == DefrostElec) THEN
      CALL SetupOutputVariable('Refrigeration Zone Air Chiller Defrost Electric Power [W]', &
                               WarehouseCoil(CoilNum)%ElecDefrostPower,'HVAC','Average',&
                               WarehouseCoil(CoilNum)%Name)
      CALL SetupOutputVariable('Refrigeration Zone Air Chiller Defrost Electric Energy [J]', &
                               WarehouseCoil(CoilNum)%ElecDefrostConsumption,'HVAC','Sum',&
                               WarehouseCoil(CoilNum)%Name,ResourceTypeKey='ELECTRICITY', &
                              EndUseKey='REFRIGERATION',GroupKey='Building', &
                              EndUseSubKey='General')
    END IF ! electric defrost coil
   END IF !(.NOT.  WarehouseCoil(CoilNum)%unusedWarehouseCoil)
  END DO ! NumSimulationWarehouseCoils
END IF ! NumSimulationRefrigAirChillers > 0

!There are no report variables for Chiller sets because they are
! used to pass the demand to the coils, but are NOT used to provide the
! cooling energy to the zone (because more than one set may cool a zone)

! Report sum of all refrigeration interactions with each zone

  DO ZoneID = 1,NumOfZones
    IF (RefrigPresentInZone(ZoneID))THEN
      IF(HaveCasesOrWalkins)THEN
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Total Sensible Cooling Rate [W]', &
                                RefrigCaseCredit(ZoneID)%SenCaseCreditToZone,'Zone','Average', &
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Total Sensible Cooling Energy [J]', &
                                CaseWIZoneReport(ZoneID)%SenCaseCreditToZoneEnergy,'Zone','Sum',&
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Heating Rate [W]', &
                                CaseWIZoneReport(ZoneID)%HeatingToZoneRate,'Zone','Average', &
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Heating Energy [J]', &
                                CaseWIZoneReport(ZoneID)%HeatingToZoneEnergy,'Zone','Sum',&
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Sensible Cooling Rate [W]', &
                                CaseWIZoneReport(ZoneID)%SenCoolingToZoneRate,'Zone','Average', &
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Sensible Cooling Energy [J]', &
                                CaseWIZoneReport(ZoneID)%SenCoolingToZoneEnergy,'Zone','Sum',&
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Total Latent Cooling Rate [W]', &
                                CaseWIZoneReport(ZoneID)%LatCoolingToZoneRate,'Zone','Average',&
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Total Latent Cooling Energy [J]', &
                                CaseWIZoneReport(ZoneID)%LatCoolingToZoneEnergy,'Zone','Sum',&
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Total Cooling Rate [W]', &
                                CaseWIZoneReport(ZoneID)%TotCoolingToZoneRate,'Zone','Average',&
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Total Cooling Energy [J]', &
                                CaseWIZoneReport(ZoneID)%TotCoolingToZoneEnergy,'Zone','Sum',&
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Total Heat Transfer Rate [W]', &
                                CaseWIZoneReport(ZoneID)%TotHtXferToZoneRate,'Zone','Average',&
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Case and Walk In Total Heat Transfer Energy [J]', &
                                CaseWIZoneReport(ZoneID)%TotHtXferToZoneEnergy,'Zone','Sum',&
                                Zone(ZoneID)%Name)
    END IF !HaveCasesOrWalkIns

      IF(HaveChillers)THEN
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Sensible Heat Transfer Rate [W]', &
                                CoilSysCredit(ZoneID)%SenCreditToZoneRate,'HVAC','Average', &
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Sensible Heat Transfer Energy [J]', &
                                CoilSysCredit(ZoneID)%SenCreditToZoneEnergy,'HVAC','Sum',Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Sensible Cooling Rate [W]', &
                                CoilSysCredit(ZoneID)%ReportSenCoolingToZoneRate,'HVAC','Average', &
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Sensible Cooling Energy [J]', &
                                CoilSysCredit(ZoneID)%ReportSenCoolingToZoneEnergy,'HVAC','Sum',Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Latent Cooling Rate [W]', &
                                CoilSysCredit(ZoneID)%ReportLatCreditToZoneRate,'HVAC','Average',Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Latent Cooling Energy [J]', &
                                CoilSysCredit(ZoneID)%ReportLatCreditToZoneEnergy,'HVAC','Sum',Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Water Removed Mass Flow Rate [Kg/s]', &
                                CoilSysCredit(ZoneID)%ReportH20RemovedKgPerS_FromZoneRate,'HVAC','Average',Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Total Cooling Rate [W]', &
                                CoilSysCredit(ZoneID)%ReportTotCoolingToZoneRate,'HVAC','Average',Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Total Cooling Energy [J]', &
                                CoilSysCredit(ZoneID)%ReportTotCoolingToZoneEnergy,'HVAC','Sum',Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Heating Rate [W]', &
                                CoilSysCredit(ZoneID)%ReportHeatingToZoneRate,'HVAC','Average', &
                                Zone(ZoneID)%Name)
        CALL SetupOutputVariable('Refrigeration Zone Air Chiller Heating Energy [J]', &
                                CoilSysCredit(ZoneID)%ReportHeatingToZoneEnergy,'HVAC','Sum',Zone(ZoneID)%Name)
      END IF !HaveChillers
    END IF !RefrigPresentInZone(ZoneID)
  END DO ! ZoneID

 IF(NumSimulationSecondarySystems > 0)THEN
    ! CurrentModuleObject='Refrigeration:SecondarySystem'
    DO SecondNum=1,NumSimulationSecondarySystems
    IF(Secondary(SecondNum)%NumSysAttach == 1) THEN
      IF(Secondary(SecondNum)%CoilFlag) THEN  !secondary system serves chillers and is solved on HVAC time step
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Pump Electric Power [W]', &
                       Secondary(SecondNum)%PumpPowerTotal,'HVAC','Average',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Pump Electric Energy [J]', &
                       Secondary(SecondNum)%PumpElecEnergyTotal,'HVAC','Sum',&
                       Secondary(SecondNum)%Name, &
                       ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                       EndUseSubKey=Secondary(SecondNum)%EndUseSubcategory)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Load Heat Transfer Rate [W]', &
                       Secondary(SecondNum)%TotalRefrigLoad,'HVAC','Average',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Load Heat Transfer Energy [J]', &
                       Secondary(SecondNum)%TotalRefrigEnergy,'HVAC','Sum',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Total Heat Transfer Rate [W]', &
                       Secondary(SecondNum)%TotalCoolingLoad,'HVAC','Average',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Total Heat Transfer Energy [J]', &
                       Secondary(SecondNum)%TotalCoolingEnergy,'HVAC','Sum',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Estimated Refrigerant Inventory Mass [kg]', &
                     Secondary(SecondNum)%RefInventory,'HVAC','Average',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Volume Flow Rate [m3/s]', &
                     Secondary(SecondNum)%FlowVolActual,'HVAC','Average',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Pipe Heat Gain Rate [W]', &
                     Secondary(SecondNum)%DistPipeHeatGain,'HVAC','Average',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Pipe Heat Gain Energy [J]', &
                     Secondary(SecondNum)%DistPipeHeatGainEnergy,'HVAC','Sum',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Receiver Heat Gain Rate [W]', &
                     Secondary(SecondNum)%ReceiverHeatGain,'HVAC','Average',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Secondary Loop Receiver Heat Gain Energy [J]', &
                     Secondary(SecondNum)%ReceiverHeatGainEnergy,'HVAC','Sum',&
                     Secondary(SecondNum)%Name)
      ELSE  !Secondary loop serves cases and walk-ins on zone(load) time step
        CALL SetupOutputVariable('Refrigeration Secondary Loop Pump Electric Power [W]', &
                       Secondary(SecondNum)%PumpPowerTotal,'Zone','Average',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Pump Electric Energy [J]', &
                       Secondary(SecondNum)%PumpElecEnergyTotal,'Zone','Sum',&
                       Secondary(SecondNum)%Name, &
                       ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                       EndUseSubKey=Secondary(SecondNum)%EndUseSubcategory)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Load Heat Transfer Rate [W]', &
                       Secondary(SecondNum)%TotalRefrigLoad,'Zone','Average',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Load Heat Transfer Energy [J]', &
                       Secondary(SecondNum)%TotalRefrigEnergy,'Zone','Sum',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Total Heat Transfer Rate [W]', &
                       Secondary(SecondNum)%TotalCoolingLoad,'Zone','Average',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Total Heat Transfer Energy [J]', &
                       Secondary(SecondNum)%TotalCoolingEnergy,'Zone','Sum',&
                       Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Estimated Refrigerant Inventory Mass [kg]', &
                     Secondary(SecondNum)%RefInventory,'Zone','Average',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Volume Flow Rate [m3/s]', &
                     Secondary(SecondNum)%FlowVolActual,'Zone','Average',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Pipe Heat Gain Rate [W]', &
                     Secondary(SecondNum)%DistPipeHeatGain,'Zone','Average',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Pipe Heat Gain Energy [J]', &
                     Secondary(SecondNum)%DistPipeHeatGainEnergy,'Zone','Sum',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Receiver Heat Gain Rate [W]', &
                     Secondary(SecondNum)%ReceiverHeatGain,'Zone','Average',&
                     Secondary(SecondNum)%Name)
        CALL SetupOutputVariable('Refrigeration Secondary Loop Receiver Heat Gain Energy [J]', &
                     Secondary(SecondNum)%ReceiverHeatGainEnergy,'Zone','Sum',&
                     Secondary(SecondNum)%Name)
      END IF !NOT coilflag so on Zone timestep
      IF (Secondary(SecondNum)%ReceiverZoneNum > 0) THEN
        CALL SetupZoneInternalGain(Secondary(SecondNum)%ReceiverZoneNum, &
             'Refrigeration:SecondarySystem:Receiver',  &
             Secondary(SecondNum)%Name , &
             IntGainTypeOf_RefrigerationSecondaryReceiver,         &
             ConvectionGainRate      = Secondary(SecondNum)%ReceiverZoneHeatGain )
      ENDIF
      IF (Secondary(SecondNum)%DistPipeZoneNum > 0 ) THEN
        CALL SetupZoneInternalGain(Secondary(SecondNum)%DistPipeZoneNum, &
             'Refrigeration:SecondarySystem:Pipe',  &
             Secondary(SecondNum)%Name , &
             IntGainTypeOf_RefrigerationSecondaryPipe,         &
             ConvectionGainRate      = Secondary(SecondNum)%DistPipeZoneHeatGain )
      ENDIF
     END IF ! not an unused
     END DO   ! NumSimulationSecondarySystems
   END IF  ! NumSimulationSecondarySystems > 0

    ! Setup Report Variables for Refrigeration Compressor Rack
  IF(NumRefrigeratedRacks > 0) THEN
  ! CurrentModuleObject='Refrigeration:CompressorRack'
  DO RackNum=1,NumRefrigeratedRacks
    IF(RefrigRack(RackNum)%CoilFlag) THEN  !rack serves chillers and is solved on HVAC time step
      CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Electric Power [W]', &
                              RefrigRack(RackNum)%RackCompressorPower,'HVAC','Average',&
                              RefrigRack(RackNum)%Name)
      CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Electric Energy [J]', &
                              RefrigRack(RackNum)%RackElecConsumption,'HVAC','Sum',&
                              RefrigRack(RackNum)%Name, &
                              ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                              EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
      CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Condenser Fan Electric Power [W]', &
                              RefrigRack(RackNum)%ActualCondenserFanPower,'HVAC','Average',&
                              RefrigRack(RackNum)%Name)
      CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Condenser Fan Electric Energy [J]', &
                              RefrigRack(RackNum)%CondenserFanConsumption,'HVAC','Sum',&
                              RefrigRack(RackNum)%Name, &
                              ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                              EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
      CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Total Heat Transfer Rate [W]', &
                              RefrigRack(RackNum)%RackCapacity,'HVAC','Average',&
                              RefrigRack(RackNum)%Name)
      CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Total Heat Transfer Energy [J]', &
                              RefrigRack(RackNum)%RackCoolingEnergy,'HVAC','Sum',&
                              RefrigRack(RackNum)%Name, &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                              EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
      CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack COP [W/W]', RefrigRack(RackNum)%RackCompressorCOP, &
                             'HVAC','Average',RefrigRack(RackNum)%Name)

      IF(RefrigRack(RackNum)%CondenserType==RefrigCondenserTypeEvap) THEN
        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Evaporative Condenser Pump Electric Power [W]', &
                                RefrigRack(RackNum)%ActualEvapPumpPower,'HVAC','Average',&
                                RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Evaporative Condenser Pump Electric Energy [J]', &
                                RefrigRack(RackNum)%EvapPumpConsumption,'HVAC','Sum',&
                                RefrigRack(RackNum)%Name, &
                                ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                                EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Evaporative Condenser Basin Heater Electric Power [W]',&
                                RefrigRack(RackNum)%BasinHeaterPower,'HVAC','Average',&
                                RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Evaporative Condenser '//  &
                                'Basin Heater Electric Energy [J]', &
                                RefrigRack(RackNum)%BasinHeaterConsumption,'HVAC','Sum',&
                                RefrigRack(RackNum)%Name, &
                                ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                                EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Evaporative Condenser Water Volume Flow Rate [m3/s]', &
                                RefrigRack(RackNum)%EvapWaterConsumpRate,'HVAC','Average',&
                                RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Evaporative Condenser Water Volume [m3]', &
                                RefrigRack(RackNum)%EvapWaterConsumption,'HVAC','Sum',&
                                RefrigRack(RackNum)%Name, &
                                ResourceTypeKey='Water',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                                EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
      END IF !Evap condenser

      IF(RefrigRack(RackNum)%HeatRejectionLocation == LocationZone) THEN
        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Zone Sensible Heating Rate [W]', &
                              RefrigRack(RackNum)%SensZoneCreditHeatRate,'HVAC','Average',&
                              RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Zone Sensible Heating Energy [J]', &
                              RefrigRack(RackNum)%SensZoneCreditHeat,'HVAC','Sum',&
                              RefrigRack(RackNum)%Name)

        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Return Air Sensible Heating Rate [W]', &
                              RefrigRack(RackNum)%SensHVACCreditHeatRate,'HVAC','Average',&
                              RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller Compressor Rack Return Air Sensible Heating Energy [J]', &
                              RefrigRack(RackNum)%SensHVACCreditHeat,'HVAC','Sum',&
                              RefrigRack(RackNum)%Name)

        CALL SetupZoneInternalGain(RefrigCase(RefrigRack(RackNum)%CaseNum(1))%ActualZoneNum, &
                     'Refrigeration:CompressorRack',  &
                     RefrigRack(RackNum)%Name , &
                     IntGainTypeOf_RefrigerationCompressorRack,         &
                     ConvectionGainRate          = RefrigRack(RackNum)%SensZoneCreditHeatRate , &
                     ReturnAirConvectionGainRate = RefrigRack(RackNum)%SensHVACCreditHeatRate )

      END IF !LocationZone

   ELSE ! Rack serves cases and walkins on zone (load) time step

      CALL SetupOutputVariable('Refrigeration Compressor Rack Electric Power [W]', &
                              RefrigRack(RackNum)%RackCompressorPower,'Zone','Average',&
                              RefrigRack(RackNum)%Name)
      CALL SetupOutputVariable('Refrigeration Compressor Rack Electric Energy [J]', &
                              RefrigRack(RackNum)%RackElecConsumption,'Zone','Sum',&
                              RefrigRack(RackNum)%Name, &
                              ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                              EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
      CALL SetupOutputVariable('Refrigeration Compressor Rack Condenser Fan Electric Power [W]', &
                              RefrigRack(RackNum)%ActualCondenserFanPower,'Zone','Average',&
                              RefrigRack(RackNum)%Name)
      CALL SetupOutputVariable('Refrigeration Compressor Rack Condenser Fan Electric Energy [J]', &
                              RefrigRack(RackNum)%CondenserFanConsumption,'Zone','Sum',&
                              RefrigRack(RackNum)%Name, &
                              ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                              EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
      CALL SetupOutputVariable('Refrigeration Compressor Rack Total Heat Transfer Rate [W]', &
                              RefrigRack(RackNum)%RackCapacity,'Zone','Average',&
                              RefrigRack(RackNum)%Name)
      CALL SetupOutputVariable('Refrigeration Compressor Rack Total Heat Transfer Energy [J]', &
                              RefrigRack(RackNum)%RackCoolingEnergy,'Zone','Sum',&
                              RefrigRack(RackNum)%Name, &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                              EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
      CALL SetupOutputVariable('Refrigeration Compressor Rack COP [W/W]', RefrigRack(RackNum)%RackCompressorCOP, &
                             'Zone','Average',RefrigRack(RackNum)%Name)

      IF(RefrigRack(RackNum)%CondenserType==RefrigCondenserTypeEvap) THEN
        CALL SetupOutputVariable('Refrigeration Compressor Rack Evaporative Condenser Pump Electric Power [W]', &
                                RefrigRack(RackNum)%ActualEvapPumpPower,'Zone','Average',&
                                RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Compressor Rack Evaporative Condenser Pump Electric Energy [J]', &
                                RefrigRack(RackNum)%EvapPumpConsumption,'Zone','Sum',&
                                RefrigRack(RackNum)%Name, &
                                ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                                EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
        CALL SetupOutputVariable('Refrigeration Compressor Rack Evaporative Condenser Basin Heater Electric Power [W]', &
                                RefrigRack(RackNum)%BasinHeaterPower,'Zone','Average',&
                                RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Compressor Rack Evaporative Condenser Basin Heater Electric Energy [J]', &
                                RefrigRack(RackNum)%BasinHeaterConsumption,'Zone','Sum',&
                                RefrigRack(RackNum)%Name, &
                                ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                                EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
        CALL SetupOutputVariable('Refrigeration Compressor Rack Evaporative Condenser Water Volume Flow Rate [m3/s]', &
                                RefrigRack(RackNum)%EvapWaterConsumpRate,'Zone','Average',&
                                RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Compressor Rack Evaporative Condenser Water Volume [m3]', &
                                RefrigRack(RackNum)%EvapWaterConsumption,'Zone','Sum',&
                                RefrigRack(RackNum)%Name, &
                                ResourceTypeKey='Water',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                                EndUseSubKey=RefrigRack(RackNum)%EndUseSubcategory)
      END IF !condenser evap

      IF(RefrigRack(RackNum)%HeatRejectionLocation == LocationZone) THEN
        CALL SetupOutputVariable('Refrigeration Compressor Rack Zone Sensible Heating Rate [W]', &
                              RefrigRack(RackNum)%SensZoneCreditHeatRate,'Zone','Average',&
                              RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Compressor Rack Zone Sensible Heating Energy [J]', &
                              RefrigRack(RackNum)%SensZoneCreditHeat,'Zone','Sum',&
                              RefrigRack(RackNum)%Name)

        CALL SetupOutputVariable('Refrigeration Compressor Rack Return Air Sensible Heating Rate [W]', &
                              RefrigRack(RackNum)%SensHVACCreditHeatRate,'Zone','Average',&
                              RefrigRack(RackNum)%Name)
        CALL SetupOutputVariable('Refrigeration Compressor Rack Return Air Sensible Heating Energy [J]', &
                              RefrigRack(RackNum)%SensHVACCreditHeat,'Zone','Sum',&
                              RefrigRack(RackNum)%Name)
        CALL SetupZoneInternalGain(RefrigCase(RefrigRack(RackNum)%CaseNum(1))%ActualZoneNum, &
                     'Refrigeration:CompressorRack',  &
                     RefrigRack(RackNum)%Name , &
                     IntGainTypeOf_RefrigerationCompressorRack,         &
                     ConvectionGainRate          = RefrigRack(RackNum)%SensZoneCreditHeatRate , &
                     ReturnAirConvectionGainRate = RefrigRack(RackNum)%SensHVACCreditHeatRate )

      END IF !location zone
    END IF ! Serves coils or case/walkin loads

    IF(RefrigRack(RackNum)%CondenserType==RefrigCondenserTypeWater) THEN !on HVAC time step no matter what
        CALL SetupOutputVariable('Refrigeration Compressor Rack Condenser Mass Flow Rate [kg/s]', &
                       RefrigRack(RackNum)%MassFlowRate, 'HVAC','Average',RefrigRack(RackNum)%Name)

        CALL SetupOutputVariable('Refrigeration Compressor Rack Condenser Heat Transfer Rate [W]', &
                       RefrigRack(RackNum)%CondLoad, 'HVAC','Average',RefrigRack(RackNum)%Name)

        CALL SetupOutputVariable('Refrigeration Compressor Rack Condenser Heat Transfer Energy [J]', &
                       RefrigRack(RackNum)%CondEnergy, 'HVAC','Sum',RefrigRack(RackNum)%Name, &
                       ResourceTypeKey='ENERGYTRANSFER',EndUseKey='Heating',GroupKey='Plant')


    END IF !Condenser cooling water
  END DO  !Refrigerated Racks
  END IF  !NumRefrigeratedRacks > 0

  IF(NumRefrigSystems > 0)THEN
    ! CurrentModuleObject='Refrigeration:System'
    DO RefrigSysNum=1,NumRefrigSystems
      IF(System(RefrigSysNum)%CoilFlag) THEN  !system serves chillers and is solved on HVAC time step
        IF (System(RefrigSysNum)%NumStages == 1) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total Compressor Electric Power [W]', &
                         System(RefrigSysNum)%TotCompPower,'HVAC','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total Compressor Electric Energy [J]', &
                         System(RefrigSysNum)%TotCompElecConsump,'HVAC','Sum',&
                         System(RefrigSysNum)%Name)
        ELSE IF (System(RefrigSysNum)%NumStages == 2) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total Low Stage Compressor Electric Power [W]', &
                         System(RefrigSysNum)%TotCompPower,'HVAC','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total Low Stage Compressor Electric Energy [J]', &
                         System(RefrigSysNum)%TotCompElecConsump,'HVAC','Sum',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total High Stage Compressor Electric Power [W]', &
                         System(RefrigSysNum)%TotHiStageCompPower,'HVAC','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total High Stage Compressor Electric Energy [J]', &
                         System(RefrigSysNum)%TotHiStageCompElecConsump,'HVAC','Sum',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total Low and High Stage Compressor Electric Energy [J]', &
                         System(RefrigSysNum)%TotCompElecConsumpTwoStage,'HVAC','Sum',&
                         System(RefrigSysNum)%Name)
        END IF  ! NumStages
        CALL SetupOutputVariable('Refrigeration Air Chiller System Average Compressor COP [W/W]', &
                       System(RefrigSysNum)%AverageCompressorCOP,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Total Air Chiller Heat Transfer Rate [W]', &
                       System(RefrigSysNum)%TotalCoolingLoad,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Total Case and Walk In Heat Transfer Energy [J]', &
                       System(RefrigSysNum)%TotalCoolingEnergy,'HVAC','Sum',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Total Transferred Load Heat Transfer Rate [W]', &
                       System(RefrigSysNum)%TotTransferLoad,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Total Transferred Load Heat Transfer Energy [J]', &
                       System(RefrigSysNum)%TotTransferEnergy,'HVAC','Sum',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Total Suction Pipe Heat Gain Rate [W]', &
                       System(RefrigSysNum)%PipeHeatLoad,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Total Suction Pipe Heat Gain Energy [J]', &
                       System(RefrigSysNum)%PipeHeatEnergy,'HVAC','Sum',&
                       System(RefrigSysNum)%Name)
        IF (System(RefrigSysNum)%NumStages == 1) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total Compressor Heat Transfer Rate [W]', &
                         System(RefrigSysNum)%TotCompCapacity,'HVAC','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total Compressor Heat Transfer Energy [J]', &
                         System(RefrigSysNum)%TotCompCoolingEnergy,'HVAC','Sum',&
                         System(RefrigSysNum)%Name) !indiv compressors go to meter, not system sum
        ELSE IF (System(RefrigSysNum)%NumStages == 2) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total Low Stage Compressor Heat Transfer Rate [W]', &
                         System(RefrigSysNum)%TotCompCapacity,'HVAC','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total Low Stage Compressor Heat Transfer Energy [J]', &
                         System(RefrigSysNum)%TotCompCoolingEnergy,'HVAC','Sum',&
                         System(RefrigSysNum)%Name) !indiv compressors go to meter, not system sum
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total High Stage Compressor Heat Transfer Rate [W]', &
                         System(RefrigSysNum)%TotHiStageCompCapacity,'HVAC','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Total High Stage Compressor Heat Transfer Energy [J]', &
                         System(RefrigSysNum)%TotHiStageCompCoolingEnergy,'HVAC','Sum',&
                         System(RefrigSysNum)%Name) !indiv compressors go to meter, not system sum
        END IF  ! NumStages
        CALL SetupOutputVariable('Refrigeration Air Chiller System Net Rejected Heat Transfer Rate [W]', &
                       System(RefrigSysNum)%NetHeatRejectLoad,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Net Rejected Heat Transfer Energy [J]', &
                       System(RefrigSysNum)%NetHeatRejectEnergy,'HVAC','Sum',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Estimated Refrigerant Inventory Mass [kg]', &
                     System(RefrigSysNum)%RefInventory,'HVAC','Average',&
                     System(RefrigSysNum)%Name)
        IF (System(RefrigSysNum)%NumStages == 1) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Estimated Refrigerant Mass Flow Rate [kg/s]', &
                       System(RefrigSysNum)%RefMassFlowComps,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
        ELSE IF (System(RefrigSysNum)%NumStages == 2) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Estimated Low Stage Refrigerant Mass Flow Rate [kg/s]', &
                       System(RefrigSysNum)%RefMassFlowComps,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Estimated High Stage Refrigerant Mass Flow Rate [kg/s]', &
                       System(RefrigSysNum)%RefMassFlowHiStageComps,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
        END IF  ! NumStages
        IF(System(RefrigSysNum)%NumStages == 2) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Intercooler Temperature [C]', &
                       System(RefrigSysNum)%TIntercooler,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Intercooler Pressure [Pa]', &
                       System(RefrigSysNum)%PIntercooler,'HVAC','Average',&
                       System(RefrigSysNum)%Name)
        END IF
        CALL SetupOutputVariable('Refrigeration Air Chiller System Condensing Temperature [C]', &
                     System(RefrigSysNum)%TCondense,'HVAC','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Evaporating Temperature [C]', &
                     System(RefrigSysNum)%TEvapNeeded,'HVAC','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Suction Temperature [C]', &
                     System(RefrigSysNum)%TCompIn,'HVAC','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System TXV Liquid Temperature [C]', &
                     System(RefrigSysNum)%TLiqInActual,'HVAC','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Liquid Suction Subcooler Heat Transfer Rate [W]', &
                     System(RefrigSysNum)%LSHXTrans,'HVAC','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Liquid Suction Subcooler Heat Transfer Energy [J]', &
                       System(RefrigSysNum)%LSHXTransEnergy,'HVAC','Sum',&
                       System(RefrigSysNum)%Name)
      ELSE  ! NOT System(SysNum)%CoilFlag, so serving loads on zone timestep
        IF (System(RefrigSysNum)%NumStages == 1) THEN
          CALL SetupOutputVariable('Refrigeration System Total Compressor Electric Power [W]', &
                         System(RefrigSysNum)%TotCompPower,'Zone','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Total Compressor Electric Energy [J]', &
                         System(RefrigSysNum)%TotCompElecConsump,'Zone','Sum',&
                         System(RefrigSysNum)%Name)
        ELSE IF (System(RefrigSysNum)%NumStages == 2)THEN
          CALL SetupOutputVariable('Refrigeration System Total Low Stage Compressor Electric Power [W]', &
                         System(RefrigSysNum)%TotCompPower,'Zone','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Total Low Stage Compressor Electric Energy [J]', &
                         System(RefrigSysNum)%TotCompElecConsump,'Zone','Sum',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Total High Stage Compressor Electric Power [W]', &
                         System(RefrigSysNum)%TotHiStageCompPower,'Zone','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Total High Stage Compressor Electric Energy [J]', &
                         System(RefrigSysNum)%TotHiStageCompElecConsump,'Zone','Sum',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Total Low and High Stage Compressor Electric Energy [J]', &
                         System(RefrigSysNum)%TotCompElecConsumpTwoStage,'Zone','Sum',&
                         System(RefrigSysNum)%Name)
        END IF  ! NumStages
        CALL SetupOutputVariable('Refrigeration System Average Compressor COP [W/W]', &
                       System(RefrigSysNum)%AverageCompressorCOP,'Zone','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Total Cases and Walk Ins Heat Transfer Rate [W]', &
                       System(RefrigSysNum)%TotalCoolingLoad,'Zone','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Total Cases and Walk Ins Heat Transfer Energy [J]', &
                       System(RefrigSysNum)%TotalCoolingEnergy,'Zone','Sum',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Total Transferred Load Heat Transfer Rate [W]', &
                       System(RefrigSysNum)%TotTransferLoad,'Zone','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Total Transferred Load Heat Transfer Energy [J]', &
                       System(RefrigSysNum)%TotTransferEnergy,'Zone','Sum',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Total Suction Pipe Heat Gain Rate [W]', &
                       System(RefrigSysNum)%PipeHeatLoad,'Zone','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Total Suction Pipe Heat Gain Energy [J]', &
                       System(RefrigSysNum)%PipeHeatEnergy,'Zone','Sum',&
                       System(RefrigSysNum)%Name)
        IF (System(RefrigSysNum)%NumStages == 1) THEN
          CALL SetupOutputVariable('Refrigeration System Total Compressor Heat Transfer Rate [W]', &
                         System(RefrigSysNum)%TotCompCapacity,'Zone','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Total Compressor Heat Transfer Energy [J]', &
                         System(RefrigSysNum)%TotCompCoolingEnergy,'Zone','Sum',&
                         System(RefrigSysNum)%Name) !indiv compressors go to meter, not system sum
        ELSE IF (System(RefrigSysNum)%NumStages == 2)THEN
          CALL SetupOutputVariable('Refrigeration System Total Low Stage Compressor Heat Transfer Rate [W]', &
                         System(RefrigSysNum)%TotCompCapacity,'Zone','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Total Low Stage Compressor Heat Transfer Energy [J]', &
                         System(RefrigSysNum)%TotCompCoolingEnergy,'Zone','Sum',&
                         System(RefrigSysNum)%Name) !indiv compressors go to meter, not system sum
          CALL SetupOutputVariable('Refrigeration System Total High Stage Compressor Heat Transfer Rate [W]', &
                         System(RefrigSysNum)%TotHiStageCompCapacity,'Zone','Average',&
                         System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Total High Stage Compressor Heat Transfer Energy [J]', &
                         System(RefrigSysNum)%TotHiStageCompCoolingEnergy,'Zone','Sum',&
                         System(RefrigSysNum)%Name) !indiv compressors go to meter, not system sum
        END IF  ! NumStages
        CALL SetupOutputVariable('Refrigeration System Net Rejected Heat Transfer Rate [W]', &
                       System(RefrigSysNum)%NetHeatRejectLoad,'Zone','Average',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Net Rejected Heat Transfer Energy [J]', &
                       System(RefrigSysNum)%NetHeatRejectEnergy,'Zone','Sum',&
                       System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Estimated Refrigerant Inventory Mass [kg]', &
                     System(RefrigSysNum)%RefInventory,'Zone','Average',&
                     System(RefrigSysNum)%Name)
        IF (System(RefrigSysNum)%NumStages == 1) THEN
          CALL SetupOutputVariable('Refrigeration System Estimated Refrigerant Mass Flow Rate [kg/s]', &
                       System(RefrigSysNum)%RefMassFlowComps,'Zone','Average',&
                       System(RefrigSysNum)%Name)
        ELSE IF (System(RefrigSysNum)%NumStages == 2)THEN
          CALL SetupOutputVariable('Refrigeration System Estimated Low Stage Refrigerant Mass Flow Rate [kg/s]', &
                       System(RefrigSysNum)%RefMassFlowComps,'Zone','Average',&
                       System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Estimated High Stage Refrigerant Mass Flow Rate [kg/s]', &
                       System(RefrigSysNum)%RefMassFlowHiStageComps,'Zone','Average',&
                       System(RefrigSysNum)%Name)
        END IF  ! NumStages
        IF(System(RefrigSysNum)%NumStages == 2) THEN
          CALL SetupOutputVariable('Refrigeration System Intercooler Temperature [C]', &
                       System(RefrigSysNum)%TIntercooler,'Zone','Average',&
                       System(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Intercooler Pressure [Pa]', &
                       System(RefrigSysNum)%PIntercooler,'Zone','Average',&
                       System(RefrigSysNum)%Name)
        END IF
        CALL SetupOutputVariable('Refrigeration System Condensing Temperature [C]', &
                     System(RefrigSysNum)%TCondense,'Zone','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Evaporating Temperature [C]', &
                     System(RefrigSysNum)%TEvapNeeded,'Zone','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Suction Pipe Suction Temperature [C]', &
                     System(RefrigSysNum)%TCompIn,'Zone','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Thermostatic Expansion Valve Liquid Temperature [C]', &
                     System(RefrigSysNum)%TLiqInActual,'Zone','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Liquid Suction Subcooler Heat Transfer Rate [W]', &
                     System(RefrigSysNum)%LSHXTrans,'Zone','Average',&
                     System(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration System Liquid Suction Subcooler Heat Transfer Energy [J]', &
                       System(RefrigSysNum)%LSHXTransEnergy,'Zone','Sum',&
                       System(RefrigSysNum)%Name)
      END IF !system(coilflag)

      IF (System(RefrigSysNum)%SystemRejectHeatToZone) THEN
        IF(Condenser( System(RefrigSysNum)%CondenserNum(1) )%InletAirZoneNum > 0) &
          CALL SetupZoneInternalGain(Condenser( System(RefrigSysNum)%CondenserNum(1) )%InletAirZoneNum, &
             'Refrigeration:System:Condenser:AirCooled',  &
             System(RefrigSysNum)%Name , &
             IntGainTypeOf_RefrigerationSystemAirCooledCondenser,         &
             ConvectionGainRate       = System(RefrigSysNum)%NetHeatRejectLoad )

        IF (System(RefrigSysNum)%SuctionPipeActualZoneNum > 0) &
          CALL SetupZoneInternalGain(System(RefrigSysNum)%SuctionPipeActualZoneNum, &
             'Refrigeration:System:SuctionPipe',  &
             System(RefrigSysNum)%Name , &
             IntGainTypeOf_RefrigerationSystemSuctionPipe,         &
             ConvectionGainRate       = System(RefrigSysNum)%PipeHeatLoad )

      ENDIF
    END DO   ! numrefrigsystems

    !Report Compressor ENERGY here, not on system level for meters.
    DO CompNum=1,NumSimulationCompressors
     ! CurrentModuleObject='Refrigeration:Compressor'
     IF(Compressor(CompNum)%NumSysAttach == 1) THEN  !only set up reports for compressors that are used once and only once
      IF(Compressor(CompNum)%CoilFlag) THEN !Compressor serving system with chillers on HVAC time step
        CALL SetupOutputVariable('Refrigeration Air Chiller System Compressor Electric Power [W]', &
                       Compressor(CompNum)%Power,'HVAC','Average',&
                       Compressor(CompNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Compressor Electric Energy [J]', &
                       Compressor(CompNum)%ElecConsumption,'HVAC','Sum',&
                       Compressor(CompNum)%Name, &
                       ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                       EndUseSubKey=Compressor(CompNum)%EndUseSubcategory)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Compressor Heat Transfer Rate [W]', &
                       Compressor(CompNum)%Capacity,'HVAC','Average',&
                       Compressor(CompNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Compressor Heat Transfer Energy [J]', &
                       Compressor(CompNum)%CoolingEnergy,'HVAC','Sum',&
                       Compressor(CompNum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Compressor Runtime Fraction []', &
                       Compressor(CompNum)%LoadFactor, 'HVAC','Average',&
                       Compressor(CompNum)%Name)
      ELSE ! serve cases/walkins on zone time step
        CALL SetupOutputVariable('Refrigeration Compressor Electric Power [W]', &
                       Compressor(CompNum)%Power,'Zone','Average',&
                       Compressor(CompNum)%Name)
        CALL SetupOutputVariable('Refrigeration Compressor Electric Energy [J]', &
                       Compressor(CompNum)%ElecConsumption,'Zone','Sum',&
                       Compressor(CompNum)%Name, &
                       ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                       EndUseSubKey=Compressor(CompNum)%EndUseSubcategory)
        CALL SetupOutputVariable('Refrigeration Compressor Heat Transfer Rate [W]', &
                       Compressor(CompNum)%Capacity,'Zone','Average',&
                       Compressor(CompNum)%Name)
        CALL SetupOutputVariable('Refrigeration Compressor Heat Transfer Energy [J]', &
                       Compressor(CompNum)%CoolingEnergy,'Zone','Sum',&
                       Compressor(CompNum)%Name)
        CALL SetupOutputVariable('Refrigeration Compressor Runtime Fraction []', &
                       Compressor(CompNum)%LoadFactor, 'Zone','Average',&
                       Compressor(CompNum)%Name)
       END IF ! Serve coils on HVAC time step or cases/walkins on Zone time step
     END IF ! NumSysAttach
    END DO !Compnum on NumSimulationCompressors

    ! Report Variables for Refrigeration Condensers
    DO CondNum=1,NumRefrigCondensers
    ! CurrentModuleObject='Refrigeration:Condenser:*'
      IF(Condenser(Condnum)%CoilFlag) THEN !Condenser serving system with chillers on HVAC time step
        CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Heat Transfer Rate [W]', &
                         Condenser(Condnum)%CondLoad, 'HVAC','Average',Condenser(Condnum)%Name)
        CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Heat Transfer Energy [J]', &
                         Condenser(Condnum)%CondEnergy, 'HVAC','Sum',Condenser(Condnum)%Name)

        IF(Condenser(CondNum)%CondenserType /= RefrigCondenserTypeCascade)THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Rate [W]', &
                         Condenser(Condnum)%TotalHeatRecoveredLoad, 'HVAC','Average',Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Total Recovered Heat Transfer Energy [J]', &
                         Condenser(Condnum)%TotalHeatRecoveredEnergy, 'HVAC','Sum',Condenser(Condnum)%Name)
          CALL SetupOutputVariable(  &
             'Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]', &
                         Condenser(Condnum)%ExternalHeatRecoveredLoad, 'HVAC','Average',Condenser(Condnum)%Name)
          CALL SetupOutputVariable(  &
             'Refrigeration Air Chiller System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]', &
                         Condenser(Condnum)%ExternalEnergyRecovered, 'HVAC','Sum',Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Rate [W]', &
                         Condenser(Condnum)%InternalHeatRecoveredLoad, 'HVAC','Average',Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Defrost Recovered Heat Transfer Energy [J]', &
                         Condenser(Condnum)%InternalEnergyRecovered, 'HVAC','Sum',Condenser(Condnum)%Name)
        END IF !not cascade because recovered energy on cascade systems passed up to higher temperature system

        IF(Condenser(Condnum)%CondenserType==RefrigCondenserTypeAir) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Fan Electric Power [W]', &
                         Condenser(Condnum)%ActualFanPower,'HVAC','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Fan Electric Energy [J]', &
                         Condenser(Condnum)%FanElecEnergy,'HVAC','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
        END IF  !Air cooled

        IF(Condenser(Condnum)%CondenserType==RefrigCondenserTypeEvap) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Fan Electric Power [W]', &
                         Condenser(Condnum)%ActualFanPower,'HVAC','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Fan Electric Energy [J]', &
                         Condenser(Condnum)%FanElecEnergy,'HVAC','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Pump Electric Power [W]', &
                         Condenser(Condnum)%ActualEvapPumpPower,'HVAC','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Pump Electric Energy [J]', &
                         Condenser(Condnum)%EvapPumpConsumption,'HVAC','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Basin Heater Electric Power [W]', &
                         Condenser(Condnum)%BasinHeaterPower,'HVAC','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Basin Heater Electric Energy [J]', &
                         Condenser(Condnum)%BasinHeaterConsumption,'HVAC','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Evaporated Water Volume Flow Rate [m3/s]', &
                         Condenser(Condnum)%EvapWaterConsumpRate,'HVAC','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Evaporated Water Volume [m3]', &
                         Condenser(Condnum)%EvapWaterConsumption,'HVAC','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='Water',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
        END IF !Evaporative Condenser Variables

        IF(Condenser(Condnum)%CondenserType==RefrigCondenserTypeWater) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Condenser Fluid Mass Flow Rate [kg/s]', &
                         Condenser(Condnum)%MassFlowRate, 'HVAC','Average',Condenser(Condnum)%Name)

         END IF !Water-cooled Condenser variables

    ELSE !Serving loads/systems with cases and walkins on zone time step

        CALL SetupOutputVariable('Refrigeration System Condenser Heat Transfer Rate [W]', &
                         Condenser(Condnum)%CondLoad, 'Zone','Average',Condenser(Condnum)%Name)
        CALL SetupOutputVariable('Refrigeration System Condenser Heat Transfer Energy [J]', &
                         Condenser(Condnum)%CondEnergy, 'Zone','Sum',Condenser(Condnum)%Name)

        IF(Condenser(CondNum)%CondenserType /= RefrigCondenserTypeCascade)THEN
          CALL SetupOutputVariable('Refrigeration System Condenser Total Recovered Heat Transfer Rate [W]', &
                         Condenser(Condnum)%TotalHeatRecoveredLoad, 'Zone','Average',Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Total Recovered Heat Transfer Energy [J]', &
                         Condenser(Condnum)%TotalHeatRecoveredEnergy, 'Zone','Sum',Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Rate [W]', &
                         Condenser(Condnum)%ExternalHeatRecoveredLoad, 'Zone','Average',Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Non Refrigeration Recovered Heat Transfer Energy [J]', &
                         Condenser(Condnum)%ExternalEnergyRecovered, 'Zone','Sum',Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Defrost Recovered Heat Transfer Rate [W]', &
                         Condenser(Condnum)%InternalHeatRecoveredLoad, 'Zone','Average',Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Defrost Recovered Heat Transfer Energy [J]', &
                         Condenser(Condnum)%InternalEnergyRecovered, 'Zone','Sum',Condenser(Condnum)%Name)
        END IF !not cascade because recovered energy on cascade systems passed up to higher temperature system

        IF(Condenser(Condnum)%CondenserType==RefrigCondenserTypeAir) THEN
          CALL SetupOutputVariable('Refrigeration System Condenser Fan Electric Power [W]', &
                         Condenser(Condnum)%ActualFanPower,'Zone','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Fan Electric Energy [J]', &
                         Condenser(Condnum)%FanElecEnergy,'Zone','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
        END IF  !Air cooled

        IF(Condenser(Condnum)%CondenserType==RefrigCondenserTypeEvap) THEN
          CALL SetupOutputVariable('Refrigeration System Condenser Fan Electric Power [W]', &
                         Condenser(Condnum)%ActualFanPower,'Zone','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Fan Electric Energy [J]', &
                         Condenser(Condnum)%FanElecEnergy,'Zone','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
          CALL SetupOutputVariable('Refrigeration System Condenser Pump Electric Power [W]', &
                         Condenser(Condnum)%ActualEvapPumpPower,'Zone','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Pump Electric Energy [J]', &
                         Condenser(Condnum)%EvapPumpConsumption,'Zone','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
          CALL SetupOutputVariable('Refrigeration System Condenser Basin Heater Electric Power [W]', &
                         Condenser(Condnum)%BasinHeaterPower,'Zone','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Basin Heater Electric Energy [J]', &
                         Condenser(Condnum)%BasinHeaterConsumption,'Zone','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
          CALL SetupOutputVariable('Refrigeration System Condenser Evaporated Water Volume Flow Rate [m3/s]', &
                         Condenser(Condnum)%EvapWaterConsumpRate,'Zone','Average',&
                         Condenser(Condnum)%Name)
          CALL SetupOutputVariable('Refrigeration System Condenser Evaporated Water Volume [m3]', &
                         Condenser(Condnum)%EvapWaterConsumption,'Zone','Sum',&
                         Condenser(Condnum)%Name, &
                         ResourceTypeKey='Water',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=Condenser(Condnum)%EndUseSubcategory)
        END IF !Evaporative Condenser Variables

        IF(Condenser(Condnum)%CondenserType==RefrigCondenserTypeWater) THEN
          CALL SetupOutputVariable('Refrigeration System Condenser Water Mass Flow Rate [kg/s]', &
                         Condenser(Condnum)%MassFlowRate, 'HVAC','Average',Condenser(Condnum)%Name)

         END IF !Water-cooled Condenser variables
     END IF ! Condenser%CoilFlag to distinguish HVAC vs Zone time steps
   END DO !Condnum on Numrefrigcondensers

   IF (NumSimulationSubcoolers > 0) THEN
    DO SubcoolNum=1,NumSimulationSubcoolers
      ! CurrentModuleObject='Refrigeration:Subcooler'
       IF(Subcooler(SubcoolNum)%CoilFlag) THEN !Subcooler serving system with chillers on HVAC time step
        IF(Subcooler(SubcoolNum)%Subcoolertype == Mechanical) THEN
          CALL SetupOutputVariable('Refrigeration Air Chiller System Mechanical Subcooler Heat Transfer Rate [W]', &
                       Subcooler(SubcoolNum)%MechSCTransLoad,'Zone','Average',&
                       Subcooler(SubcoolNum)%Name)
          CALL SetupOutputVariable('Refrigeration Air Chiller System Mechanical Subcooler Heat Transfer Energy [J]', &
                       Subcooler(SubcoolNum)%MechSCTransEnergy,'Zone','Sum',&
                       Subcooler(SubcoolNum)%Name)
        END IF
      ELSE ! Subcooler on system serving cases and/or walkins
        IF(Subcooler(SubcoolNum)%Subcoolertype == Mechanical) THEN
          CALL SetupOutputVariable('Refrigeration System Mechanical Subcooler Heat Transfer Rate [W]', &
                       Subcooler(SubcoolNum)%MechSCTransLoad,'HVAC','Average',&
                       Subcooler(SubcoolNum)%Name)
          CALL SetupOutputVariable('Refrigeration System Mechanical Subcooler Heat Transfer Energy [J]', &
                       Subcooler(SubcoolNum)%MechSCTransEnergy,'HVAC','Sum',&
                       Subcooler(SubcoolNum)%Name)
        END IF
      END IF !Subcoolers on system serving chillers
    END DO !Subcoolnum on NumSimulationSubcoolers
    END IF ! NumSimulationSubcoolers > 0

  END IF  !NumRefrigSystems > 0

  IF(NumTransRefrigSystems > 0)THEN
    ! CurrentModuleObject='Refrigeration:TranscriticalSystem'
    DO RefrigSysNum=1,NumTransRefrigSystems
        ! for both SingleStage and TwoStage systems (medium temperature loads present)
        CALL SetupOutputVariable('Refrigeration Transcritical System Total High Pressure Compressor Electric Power [W]', &
                       TransSystem(RefrigSysNum)%TotCompPowerHP,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Total High Pressure Compressor Electric Energy [J]', &
                       TransSystem(RefrigSysNum)%TotCompElecConsumpHP,'Zone','Sum',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Total Compressor Electric Energy [J]', &
                       TransSystem(RefrigSysNum)%TotCompElecConsump,'Zone','Sum',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Average COP [W/W]', &
                       TransSystem(RefrigSysNum)%AverageCompressorCOP,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable( &
                       'Refrigeration Transcritical System Medium Temperature Cases and Walk Ins Heat Transfer Rate [W]', &
                       TransSystem(RefrigSysNum)%TotalCoolingLoadMT,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable( &
                       'Refrigeration Transcritical System Medium Temperature Cases and Walk Ins Heat Transfer Energy [J]', &
                       TransSystem(RefrigSysNum)%TotalCoolingEnergyMT,'Zone','Sum',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Total Cases and Walk Ins Heat Transfer Energy [J]', &
                       TransSystem(RefrigSysNum)%TotalCoolingEnergy,'Zone','Sum',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Medium Temperature Suction Pipe Heat Transfer Rate [W]', &
                       TransSystem(RefrigSysNum)%PipeHeatLoadMT,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Medium Temperature Suction Pipe Heat Transfer Energy [J]', &
                       TransSystem(RefrigSysNum)%PipeHeatEnergyMT,'Zone','Sum',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System High Pressure Compressor Heat Transfer Rate [W]', &
                       TransSystem(RefrigSysNum)%TotCompCapacityHP,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System High Pressure Compressor Heat Transfer Energy [J]', &
                       TransSystem(RefrigSysNum)%TotCompCoolingEnergyHP,'Zone','Sum',&
                       TransSystem(RefrigSysNum)%Name) !indiv compressors go to meter, not system sum
        CALL SetupOutputVariable('Refrigeration Transcritical System Net Rejected Heat Transfer Rate [W]', &
                       TransSystem(RefrigSysNum)%NetHeatRejectLoad,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Net Rejected Heat Transfer Energy [J]', &
                       TransSystem(RefrigSysNum)%NetHeatRejectEnergy,'Zone','Sum',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Estimated Refrigerant Inventory Mass [kg]', &
                       TransSystem(RefrigSysNum)%RefInventory,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Refrigerant Mass Flow Rate [kg/s]', &
                       TransSystem(RefrigSysNum)%RefMassFlowComps,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Medium Temperature Evaporating Temperature [C]', &
                       TransSystem(RefrigSysNum)%TEvapNeededMT,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        CALL SetupOutputVariable('Refrigeration Transcritical System Medium Temperature Suction Temperature [C]', &
                       TransSystem(RefrigSysNum)%TCompInHP,'Zone','Average',&
                       TransSystem(RefrigSysNum)%Name)
        IF (TransSystem(RefrigSysNum)%TransSysType == 2 ) THEN   ! for TwoStage system only (low temperature loads present)
          CALL SetupOutputVariable('Refrigeration Transcritical System Low Pressure Compressor Electric Power [W]', &
                         TransSystem(RefrigSysNum)%TotCompPowerLP,'Zone','Average',&
                         TransSystem(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Transcritical System Low Pressure Compressor Electric Energy [J]', &
                         TransSystem(RefrigSysNum)%TotCompElecConsumpLP,'Zone','Sum',&
                         TransSystem(RefrigSysNum)%Name)
          CALL SetupOutputVariable( &
                         'Refrigeration Transcritical System Low Temperature Cases and Walk Ins Heat Transfer Rate [W]', &
                         TransSystem(RefrigSysNum)%TotalCoolingLoadLT,'Zone','Average',&
                         TransSystem(RefrigSysNum)%Name)
          CALL SetupOutputVariable( &
                         'Refrigeration Transcritical System Low Temperature Cases and Walk Ins Heat Transfer Energy [J]', &
                         TransSystem(RefrigSysNum)%TotalCoolingEnergyLT,'Zone','Sum',&
                         TransSystem(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Transcritical System Low Temperature Suction Pipe Heat Transfer Rate [W]', &
                         TransSystem(RefrigSysNum)%PipeHeatLoadLT,'Zone','Average',&
                         TransSystem(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Transcritical System Low Temperature Suction Pipe Heat Transfer Energy [J]', &
                         TransSystem(RefrigSysNum)%PipeHeatEnergyLT,'Zone','Sum',&
                         TransSystem(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Transcritical System Low Pressure Compressor Heat Transfer Rate [W]', &
                         TransSystem(RefrigSysNum)%TotCompCapacityLP,'Zone','Average',&
                         TransSystem(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Transcritical System Low Pressure Compressor Heat Transfer Energy [J]', &
                         TransSystem(RefrigSysNum)%TotCompCoolingEnergyLP,'Zone','Sum',&
                         TransSystem(RefrigSysNum)%Name) !indiv compressors go to meter, not system sum
          CALL SetupOutputVariable('Refrigeration Transcritical System Low Temperature Evaporating Temperature [C]', &
                         TransSystem(RefrigSysNum)%TEvapNeededLT,'Zone','Average',&
                         TransSystem(RefrigSysNum)%Name)
          CALL SetupOutputVariable('Refrigeration Transcritical System Low Temperature Suction Temperature [C]', &
                         TransSystem(RefrigSysNum)%TCompInLP,'Zone','Average',&
                         TransSystem(RefrigSysNum)%Name)
        END IF  ! (TransSystem(RefrigSysNum)%TransSysType == 2)

        IF (TransSystem(RefrigSysNum)%SystemRejectHeatToZone) THEN
          IF(GasCooler(TransSystem(RefrigSysNum)%GasCoolerNum(1))%InletAirZoneNum > 0) &
            CALL SetupZoneInternalGain(GasCooler(TransSystem(RefrigSysNum)%GasCoolerNum(1))%InletAirZoneNum, &
               'Refrigeration:TranscriticalSystem:GasCooler:AirCooled', &
               TransSystem(RefrigSysNum)%Name, &
               IntGainTypeOf_RefrigerationTransSysAirCooledGasCooler, &
               ConvectionGainRate = TransSystem(RefrigSysNum)%NetHeatRejectLoad)
        END IF  ! (TransSystem(RefrigSysNum)%SystemRejectHeatToZone)
        IF (TransSystem(RefrigSysNum)%SuctionPipeActualZoneNumMT > 0) THEN
          CALL SetupZoneInternalGain(TransSystem(RefrigSysNum)%SuctionPipeActualZoneNumMT, &
               'Refrigeration:TranscriticalSystem:SuctionPipeMT', &
               TransSystem(RefrigSysNum)%Name, &
               IntGainTypeOf_RefrigerationTransSysSuctionPipeMT, &
               ConvectionGainRate = TransSystem(RefrigSysNum)%PipeHeatLoadMT)
        END IF  ! TransSystem(RefrigSysNum)%SuctionPipeActualZoneNumMT > 0
        IF (TransSystem(RefrigSysNum)%SuctionPipeActualZoneNumLT > 0) THEN
          CALL SetupZoneInternalGain(TransSystem(RefrigSysNum)%SuctionPipeActualZoneNumLT, &
               'Refrigeration:TranscriticalSystem:SuctionPipeLT', &
               TransSystem(RefrigSysNum)%Name, &
               IntGainTypeOf_RefrigerationTransSysSuctionPipeLT, &
               ConvectionGainRate = TransSystem(RefrigSysNum)%PipeHeatLoadLT)
        END IF  ! TransSystem(RefrigSysNum)%SuctionPipeActualZoneNumLT > 0

      !Report Compressor ENERGY here, not on system level for meters.
      ! LP compressors
      DO CompIndex=1,TransSystem(RefrigSysNum)%NumCompressorsLP
         CompNum=TransSystem(RefrigSysNum)%CompressornumLP(CompIndex)
         ! CurrentModuleObject='Refrigeration:Compressor'
         IF(Compressor(CompNum)%NumSysAttach == 1) THEN  !only set up reports for compressors that are used once and only once
            CALL SetupOutputVariable('Refrigeration Compressor Electric Power [W]', &
                           Compressor(CompNum)%Power,'Zone','Average',&
                           Compressor(CompNum)%Name)
            CALL SetupOutputVariable('Refrigeration Compressor Electric Energy [J]', &
                           Compressor(CompNum)%ElecConsumption,'Zone','Sum',&
                           Compressor(CompNum)%Name, &
                           ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                           EndUseSubKey=Compressor(CompNum)%EndUseSubcategory)
            CALL SetupOutputVariable('Refrigeration Compressor Heat Transfer Rate [W]', &
                           Compressor(CompNum)%Capacity,'Zone','Average',&
                           Compressor(CompNum)%Name)
            CALL SetupOutputVariable('Refrigeration Compressor Heat Transfer Energy [J]', &
                           Compressor(CompNum)%CoolingEnergy,'Zone','Sum',&
                           Compressor(CompNum)%Name)
            CALL SetupOutputVariable('Refrigeration Compressor Runtime Fraction []', &
                           Compressor(CompNum)%LoadFactor, 'Zone','Average',&
                           Compressor(CompNum)%Name)
         END IF ! NumSysAttach
      END DO !TransSystem(RefrigSysNum)%NumCompressorsLP

      ! HP compressors
      DO CompIndex=1,TransSystem(RefrigSysNum)%NumCompressorsHP
         CompNum=TransSystem(RefrigSysNum)%CompressornumHP(CompIndex)
         ! CurrentModuleObject='Refrigeration:Compressor'
         IF(Compressor(CompNum)%NumSysAttach == 1) THEN  !only set up reports for compressors that are used once and only once
            CALL SetupOutputVariable('Refrigeration Compressor Electric Power [W]', &
                           Compressor(CompNum)%Power,'Zone','Average',&
                           Compressor(CompNum)%Name)
            CALL SetupOutputVariable('Refrigeration Compressor Electric Energy [J]', &
                           Compressor(CompNum)%ElecConsumption,'Zone','Sum',&
                           Compressor(CompNum)%Name, &
                           ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                           EndUseSubKey=Compressor(CompNum)%EndUseSubcategory)
            CALL SetupOutputVariable('Refrigeration Compressor Heat Transfer Rate [W]', &
                           Compressor(CompNum)%Capacity,'Zone','Average',&
                           Compressor(CompNum)%Name)
            CALL SetupOutputVariable('Refrigeration Compressor Heat Transfer Energy [J]', &
                           Compressor(CompNum)%CoolingEnergy,'Zone','Sum',&
                           Compressor(CompNum)%Name)
            CALL SetupOutputVariable('Refrigeration Compressor Runtime Fraction []', &
                           Compressor(CompNum)%LoadFactor, 'Zone','Average',&
                           Compressor(CompNum)%Name)
         END IF ! NumSysAttach
      END DO !TransSystem(RefrigSysNum)%NumCompressorsHP

    END DO      ! NumTransRefrigSystems
  END IF        ! (NumTransRefrigSystems > 0)

  IF (NumSimulationGasCooler > 0) THEN
    DO GCNum=1,NumSimulationGasCooler
       ! CurrentModuleObject='Refrigeration:GasCooler:AirCooled'
       CALL SetupOutputVariable('Refrigeration Transcritical System Gas Cooler Heat Transfer Rate [W]', &
                         GasCooler(GCNum)%GasCoolerLoad, 'Zone','Average',GasCooler(GCNum)%Name)
       CALL SetupOutputVariable('Refrigeration Transcritical System Gas Cooler Heat Transfer Energy [J]', &
                         GasCooler(GCNum)%GasCoolerEnergy, 'Zone','Sum',GasCooler(GCNum)%Name)
       CALL SetupOutputVariable('Refrigeration Transcritical System Gas Cooler Fan Electric Power [W]', &
                         GasCooler(GCNum)%ActualFanPower,'Zone','Average',&
                         GasCooler(GCNum)%Name)
       CALL SetupOutputVariable('Refrigeration Transcritical System Gas Cooler Fan Electric Energy [J]', &
                         GasCooler(GCNum)%FanElecEnergy,'Zone','Sum',&
                         GasCooler(GCNum)%Name, &
                         ResourceTypeKey='ELECTRICITY',EndUseKey='REFRIGERATION',GroupKey='Plant', &
                         EndUseSubKey=GasCooler(GCNum)%EndUseSubcategory)
       CALL SetupOutputVariable('Refrigeration Transcritical System Gas Cooler Outlet Temperature [C]', &
                         GasCooler(GCNum)%TGasCoolerOut,'Zone','Average',GasCooler(GCNum)%Name)
       CALL SetupOutputVariable('Refrigeration Transcritical System Gas Cooler Outlet Pressure [Pa]', &
                         GasCooler(GCNum)%PGasCoolerOut,'Zone','Average',GasCooler(GCNum)%Name)
       CALL SetupOutputVariable( &
              'Refrigeration Transcritical System Gas Cooler Defrost Recovered Heat Transfer Rate [W]', &
                         GasCooler(GCNum)%InternalHeatRecoveredLoad, 'Zone','Average',GasCooler(GCNum)%Name)
       CALL SetupOutputVariable( &
              'Refrigeration Transcritical System Gas Cooler Defrost Recovered Heat Transfer Energy [J]', &
                         GasCooler(GCNum)%InternalEnergyRecovered, 'Zone','Sum',GasCooler(GCNum)%Name)
    END DO ! GCNum on NumSimulationGasCooler
  END IF   ! (NumSimulationGasCooler >0)

  RETURN

  END SUBROUTINE SetupReportInput

!***************************************************************************************************
!***************************************************************************************************
SUBROUTINE InitRefrigeration
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Oct/Nov 2004
          !       MODIFIED       Hudson, ORNL July 2007, Stovall, ORNL, 2008
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initialize (zero) global variables before simulating compressor racks and refrigerated cases
          !
          ! Several variables in this module are accumulative.  For example, unmet compressor loads are carried over
          ! to the next time step. Ice loads are accumulated until melted by a defrost.  Because this module can be
          ! called multiple times during any single time step, these summations need to be saved ONLY on the last time
          ! through any given time step.

          ! It is necessary to decrease the condenser load by the amount of heat used elsewhere
          !   via desuperheating water heaters and heating coils.
          !   Because the refrigeration system is solved before the HVAC time step loops, the
          !   refrigeration system must use the values lagged from the previous time step. In
          !   terms of energy, this should balance out and is preferable to not making the correction,
          !   in which case the condenser cooling water/air/fan energy are charged with energy
          !   loads that have been accounted elsewhere.  For consistency, the lagged value must be used,
          !   even if the Zone time step is repeated.  Therefore, the lagged variables are saved
          !   here for use during successive iterations of same zone/load time step.

          ! METHODOLOGY EMPLOYED:
          ! Global variables for Case Credit are located in DataHeatBalance. To Zone variables are used in the Air Heat
          ! Balance in ZoneTempPredictorCorrector to calculate the zone load. To HVAC variables are used in
          ! ZoneEquipmentManager to add the portion of case credits attributed to the HVAC system to the zone return air node.

          ! Because we can't know apriori whether or not the time step will be repeated, we save the most recent
          ! addition/subtraction to/from each accumulating variable.  If the time step is repeated,
          ! this most recent addition/subtraction is reversed before the rest of the refrigeration simulation begins.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DATAHVACGlobals, ONLY : SysTimeElapsed
  USE DataGlobals,     ONLY : AnyEnergyManagementSystemInModel

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: MyBeginEnvrnFlag = .TRUE.
  INTEGER       :: SystemID         =  0
  INTEGER       :: CaseID           =  0
  INTEGER       :: WalkInID         =  0
  INTEGER       :: CoilID           =  0
  INTEGER       :: ICond            =  0
  INTEGER       :: IRack            =  0
  INTEGER       :: SecondID         =  0
  !Used to adjust accumulative variables when time step is repeated
  REAL(r64)         :: MyCurrentTime         =  0.0d0    ! Used to determine whether the zone time step is a repetition
  REAL(r64),SAVE    :: MyCurrentTimeSaved    =  0.0d0    ! Used to determine whether the zone time step is a repetition
  REAL(r64),SAVE    :: MyStepStartTime       =  0.0d0    ! Used to determine whether the system time step is a repetition
  REAL(r64),SAVE    :: MyStepStartTimeSaved  =  0.0d0    ! Used to determine whether the system time step is a repetition
  REAL(r64)         :: TimeStepFraction      = 0.0d0   ! Used to calculate my current time

! Zero display case, air-coil, and walk-in cooler credits (summed by zone)
! to 0 each zone or sys time step
! These 'casecredit' variables are also used to transfer energy from zone-located
! compressor-rack condenser heat rejection, heat absorption by distribution piping,
! suction piping, and receiver shells to zone
  IF (NumOfZones > 0 ) THEN
    IF(UseSysTimeStep) THEN
      CoilSysCredit%SenCreditToZoneRate   = 0.0d0
      CoilSysCredit%ReportSenCoolingToZoneRate   = 0.0d0
      CoilSysCredit%SenCreditToZoneEnergy = 0.0d0
      CoilSysCredit%ReportSenCoolingToZoneEnergy = 0.0d0
      CoilSysCredit%LatCreditToZoneRate   = 0.0d0
      CoilSysCredit%ReportLatCreditToZoneRate   = 0.0d0
      CoilSysCredit%LatCreditToZoneEnergy = 0.0d0
      CoilSysCredit%ReportLatCreditToZoneEnergy = 0.0d0
      CoilSysCredit%LatKgPerS_ToZoneRate  = 0.0d0
      CoilSysCredit%ReportH20RemovedKgPerS_FromZoneRate  = 0.0d0
      CoilSysCredit%ReportTotCoolingToZoneRate     = 0.0d0
      CoilSysCredit%ReportTotCoolingToZoneEnergy = 0.0d0
    END IF !usesystimestep = .true.

    !Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .false.
    IF((.NOT. UseSysTimeStep).AND.((NumSimulationCases > 0).OR.( NumSimulationWalkIns > 0)))THEN
      RefrigCaseCredit%SenCaseCreditToZone = 0.0d0
      RefrigCaseCredit%LatCaseCreditToZone = 0.0d0
      RefrigCaseCredit%SenCaseCreditToHVAC = 0.0d0
      RefrigCaseCredit%LatCaseCreditToHVAC = 0.0d0
      CaseWIZoneReport%SenCaseCreditToZoneEnergy = 0.0d0
      CaseWIZoneReport%LatCoolingToZoneRate   = 0.0d0
      CaseWIZoneReport%LatCoolingToZoneRate   = 0.0d0
      CaseWIZoneReport%LatCoolingToZoneEnergy = 0.0d0
      CaseWIZoneReport%SenCoolingToZoneRate   = 0.0d0
      CaseWIZoneReport%SenCoolingToZoneEnergy = 0.0d0
      CaseWIZoneReport%HeatingToZoneRate   = 0.0d0
      CaseWIZoneReport%HeatingToZoneEnergy = 0.0d0
      CaseWIZoneReport%TotCoolingToZoneRate   = 0.0d0
      CaseWIZoneReport%TotCoolingToZoneEnergy = 0.0d0
      CaseWIZoneReport%TotHtXferToZoneRate = 0.0d0
      CaseWIZoneReport%TotHtXferToZoneEnergy = 0.0d0
    ENDIF
  ENDIF

  IF (NumSimulationCases > 0) THEN
    !RefrigCase ALLOCATED to NumSimulationCases
    RefrigCase%TotalCoolingLoad         = 0.0d0
    RefrigCase%TotalCoolingEnergy       = 0.0d0
    RefrigCase%SensCoolingEnergyRate    = 0.0d0
    RefrigCase%SensCoolingEnergy        = 0.0d0
    RefrigCase%LatCoolingEnergyRate     = 0.0d0
    RefrigCase%LatCoolingEnergy         = 0.0d0
    RefrigCase%SensZoneCreditRate       = 0.d0
    RefrigCase%SensZoneCreditCoolRate   = 0.0d0
    RefrigCase%SensZoneCreditCool       = 0.0d0
    RefrigCase%SensZoneCreditHeatRate   = 0.0d0
    RefrigCase%SensZoneCreditHeat       = 0.0d0
    RefrigCase%LatZoneCreditRate        = 0.0d0
    RefrigCase%LatZoneCredit            = 0.0d0
    RefrigCase%SensHVACCreditRate       = 0.0d0
    RefrigCase%SensHVACCreditCoolRate   = 0.0d0
    RefrigCase%SensHVACCreditCool       = 0.0d0
    RefrigCase%SensHVACCreditHeatRate   = 0.0d0
    RefrigCase%SensHVACCreditHeat       = 0.0d0
    RefrigCase%LatHVACCreditRate        = 0.0d0
    RefrigCase%LatHVACCredit            = 0.0d0
    RefrigCase%ElecFanPower             = 0.0d0
    RefrigCase%ElecFanConsumption       = 0.0d0
    RefrigCase%ElecAntiSweatPower       = 0.0d0
    RefrigCase%ElecAntiSweatConsumption = 0.0d0
    RefrigCase%ElecLightingPower        = 0.0d0
    RefrigCase%ElecLightingConsumption  = 0.0d0
    RefrigCase%ElecDefrostPower         = 0.0d0
    RefrigCase%ElecDefrostConsumption   = 0.0d0
    RefrigCase%DefEnergyCurveValue      = 0.0d0
    RefrigCase%LatEnergyCurveValue      = 0.0d0
    RefrigCase%HotDefrostCondCredit     = 0.0d0
  ENDIF ! NumSimulationCases

  IF (NumSimulationWalkIns > 0) THEN
    !WalkIn ALLOCATED to NumSimulationWalkIns
    WalkIn%HotDefrostCondCredit     = 0.0d0
    WalkIn%TotalCoolingLoad         = 0.0d0
    WalkIn%TotalCoolingEnergy       = 0.0d0
    WalkIn%TotSensCoolingEnergyRate = 0.0d0
    WalkIn%TotSensCoolingEnergy     = 0.0d0
    WalkIn%TotLatCoolingEnergyRate  = 0.0d0
    WalkIn%TotLatCoolingEnergy      = 0.0d0
    WalkIn%ElecFanPower             = 0.0d0
    WalkIn%ElecFanConsumption       = 0.0d0
    WalkIn%ElecHeaterPower          = 0.0d0
    WalkIn%ElecHeaterConsumption    = 0.0d0
    WalkIn%ElecLightingPower        = 0.0d0
    WalkIn%ElecLightingConsumption  = 0.0d0
    WalkIn%TotalElecPower           = 0.0d0
    WalkIn%TotalElecConsumption     = 0.0d0
    WalkIn%ElecDefrostPower         = 0.0d0
    WalkIn%ElecDefrostConsumption   = 0.0d0
  ENDIF

  IF (HaveChillers) THEN
    !HaveChillers is TRUE when NumSimulationRefrigAirChillers > 0
    !WarehouseCoil ALLOCATED to NumSimulationRefrigAirChillers
    WarehouseCoil%HotDefrostCondCredit     = 0.0d0
    WarehouseCoil%TotalCoolingLoad         = 0.0d0
    WarehouseCoil%TotalCoolingEnergy       = 0.0d0
    WarehouseCoil%SensCoolingEnergyRate    = 0.0d0
    WarehouseCoil%SensCoolingEnergy        = 0.0d0
    WarehouseCoil%SensCreditRate           = 0.0d0
    WarehouseCoil%LatKgPerS_ToZone         = 0.0d0
    WarehouseCoil%SensHeatRatio            = 0.0d0
    WarehouseCoil%LatCreditEnergy          = 0.0d0
    WarehouseCoil%LatCreditRate            = 0.0d0
    WarehouseCoil%ElecFanPower             = 0.0d0
    WarehouseCoil%ElecFanConsumption       = 0.0d0
    WarehouseCoil%ElecHeaterPower          = 0.0d0
    WarehouseCoil%ElecHeaterConsumption    = 0.0d0
    WarehouseCoil%TotalElecPower           = 0.0d0
    WarehouseCoil%TotalElecConsumption     = 0.0d0
    WarehouseCoil%ElecDefrostPower         = 0.0d0
    WarehouseCoil%ElecDefrostConsumption   = 0.0d0
    WarehouseCoil%ReportTotalCoolCreditRate   = 0.0d0
    WarehouseCoil%ReportTotalCoolCreditEnergy   = 0.0d0
    WarehouseCoil%ReportSensCoolCreditRate = 0.d0
    WarehouseCoil%ReportHeatingCreditRate  = 0.d0
    WarehouseCoil%ReportSensCoolCreditEnergy = 0.d0
    WarehouseCoil%ReportHeatingCreditEnergy  = 0.d0
  ENDIF

  IF (HaveRefrigRacks) THEN
    !HaveRefrigRacks TRUE when NumRefrigeratedRacks > 0
    !RefrigRack ALLOCATED to NumRefrigeratedRacks
    RefrigRack%SensHVACCreditHeatRate    = 0.0d0
    RefrigRack%SensHVACCreditHeat        = 0.0d0
    RefrigRack%SensZoneCreditHeatRate    = 0.0d0
    RefrigRack%SensZoneCreditHeat        = 0.0d0
    RefrigRack%CondLoad                  = 0.0d0
    RefrigRack%CondEnergy                = 0.0d0
    RefrigRack%MassFlowRate              = 0.0d0
    HeatReclaimRefrigeratedRack%AvailCapacity = 0.0d0
    RefrigRack%RackElecConsumption       = 0.d0
    RefrigRack%CondenserFanConsumption   = 0.d0
    RefrigRack%EvapPumpConsumption       = 0.d0
    RefrigRack%RackCompressorPower       = 0.d0
    RefrigRack%ActualCondenserFanPower   = 0.d0
    RefrigRack%ActualEvapPumpPower       = 0.d0
    !Note don't reset basin heat to zero when no load because heater would remain on
    !RefrigRack%BasinHeaterPower          = 0.d0
    !RefrigRack%BasinHeaterConsumption    = 0.d0
  ENDIF

  IF (NumRefrigCondensers > 0) THEN
    !Condenser ALLOCATED to NumRefrigCondensers
    Condenser%CondLoad                  = 0.0d0
    Condenser%CondEnergy                = 0.0d0
    Condenser%MassFlowRate              = 0.0d0
    Condenser%ActualFanPower           = 0.0d0
    Condenser%FanElecEnergy            = 0.0d0
    Condenser%EvapWaterConsumpRate     = 0.0d0
    Condenser%EvapWaterConsumption     = 0.0d0
    Condenser%ActualEvapPumpPower      = 0.0d0
    Condenser%EvapPumpConsumption      = 0.0d0
    Condenser%ExternalHeatRecoveredLoad= 0.0d0
    Condenser%ExternalEnergyRecovered  = 0.0d0
    Condenser%InternalHeatRecoveredLoad= 0.0d0
    Condenser%InternalEnergyRecovered  = 0.0d0
    Condenser%TotalHeatRecoveredLoad   = 0.0d0
    Condenser%TotalHeatRecoveredEnergy = 0.0d0
    !   Condenser%LowTempWarn               = 0
    !N don't reset basin heat to zero when no load because heater would remain on
    HeatReclaimRefrigCondenser%AvailCapacity = 0.0d0
    HeatReclaimRefrigCondenser%AvailTemperature = 0.0d0
  ENDIF

  IF (NumSimulationGasCooler > 0) THEN
    !GasCooler ALLOCATED to NumSimulationGasCooler
    GasCooler%GasCoolerLoad             = 0.0d0
    GasCooler%GasCoolerEnergy           = 0.0d0
    GasCooler%ActualFanPower            = 0.0d0
    GasCooler%FanElecEnergy             = 0.0d0
    GasCooler%InternalHeatRecoveredLoad = 0.0d0
    GasCooler%InternalEnergyRecovered   = 0.0d0
    GasCooler%TotalHeatRecoveredLoad    = 0.0d0
    GasCooler%TotalHeatRecoveredEnergy  = 0.0d0
  ENDIF

  IF (NumSimulationCompressors > 0) THEN
    !Compressor ALLOCATED to NumSimulationCompressors
    Compressor%ElecConsumption           = 0.d0
    Compressor%Power                    = 0.d0
  ENDIF

  IF (HaveDetailedRefrig) THEN
    !HaveDetailedRefrig is TRUE when NumRefrigSystems > 0
    !System is ALLOCATED to NumRefrigSystems
    System%TotalCoolingLoad                = 0.0d0
    System%TotalCondDefrostCredit          = 0.0d0
    System%SumSecondaryLoopLoad            = 0.0d0
    System%SumMechSCBenefit                = 0.0d0
    System%NetHeatRejectLoad               = 0.0d0
    System%NetHeatRejectEnergy             = 0.0d0
    System%AverageCompressorCOP            = 0.0d0
    System%TotCompCapacity                 = 0.0d0
    System%TotHiStageCompCapacity          = 0.0d0
    System%TotCompElecConsump              = 0.0d0
    System%TotHiStageCompElecConsump       = 0.0d0
    System%TotCompElecConsumpTwoStage      = 0.0d0
    System%TotCompPower                    = 0.0d0
    System%TotHiStageCompPower             = 0.0d0
    System%TotCompCoolingEnergy            = 0.0d0
    System%TotHiStageCompCoolingEnergy     = 0.0d0
  ENDIF

  IF (HaveDetailedTransRefrig) THEN
    !HaveDetailedTransRefrig is TRUE when NumTransRefrigSystems > 0
    !TransSystem is ALLOCATED to NumTransRefrigSystems
    TransSystem%TotalCoolingLoadMT         = 0.0d0
    TransSystem%TotalCoolingLoadLT         = 0.0d0
    TransSystem%TotalCondDefrostCredit     = 0.0d0
    TransSystem%NetHeatRejectLoad          = 0.0d0
    TransSystem%NetHeatRejectEnergy        = 0.0d0
    TransSystem%AverageCompressorCOP       = 0.0d0
    TransSystem%TotCompCapacityHP          = 0.0d0
    TransSystem%TotCompCapacityLP          = 0.0d0
    TransSystem%TotCompElecConsump         = 0.0d0
    TransSystem%TotCompPowerHP             = 0.0d0
    TransSystem%TotCompPowerLP             = 0.0d0
    TransSystem%TotCompCoolingEnergy       = 0.0d0
  ENDIF

  IF (NumSimulationSecondarySystems > 0) THEN
    !Secondary is ALLOCATED to NumSimulationSecondarySystems
    Secondary%TotalCoolingLoad      = 0.0d0
    Secondary%PumpPowerTotal        = 0.0d0
    Secondary%PumpElecEnergyTotal   = 0.d0
    Secondary%ReceiverZoneHeatGain  = 0.d0
    Secondary%DistPipeZoneHeatGain  = 0.d0
  ENDIF

  !Accumulative and carry-over variables are not zeroed at start of each time step, only at begining of environment
  IF(BeginEnvrnFlag .AND. MyBeginEnvrnFlag)THEN
        IF (NumSimulationCases > 0) THEN
          RefrigCase%DefrostEnergy  = 0.0d0
          RefrigCase%StockingEnergy = 0.0d0
          RefrigCase%WarmEnvEnergy  = 0.0d0
          RefrigCase%KgFrost        = 0.0d0
          RefrigCase%StoredEnergy   = 0.0d0
        ENDIF
        IF (NumRefrigSystems > 0) THEN
          System%UnmetEnergy    = 0.0d0
        ENDIF
        IF (NumSimulationWalkIns > 0) THEN
          WalkIn%KgFrost         = 0.0d0
          WalkIn%StoredEnergy    =0.0d0
          DO WalkInID = 1, NumsimulationWalkIns
            WalkIn(WalkInID)%IceTemp = WalkIn(WalkInID)%TEvapDesign
          END DO
        ENDIF
        IF (NumSimulationRefrigAirChillers > 0) THEN
          WarehouseCoil%KgFrost         = 0.0d0
          WarehouseCoil%KgFrostSaved    = 0.0d0
          DO CoilID = 1, NumSimulationRefrigAirChillers
            WarehouseCoil(CoilID)%IceTemp      = WarehouseCoil(CoilID)%TEvapDesign
            WarehouseCoil(CoilID)%IceTempSaved = WarehouseCoil(CoilID)%TEvapDesign
          END DO
        ENDIF
        IF (NumSimulationSecondarySystems > 0) THEN
            Secondary%UnMetEnergy        = 0.0d0
        END IF
        IF (NumRefrigeratedRacks > 0) THEN
          HeatReclaimRefrigeratedRack%UsedHVACCoil    = 0.0d0
          HeatReclaimRefrigeratedRack%UsedWaterHeater = 0.0d0
          RefrigRack%LaggedUsedWaterHeater            = 0.d0
          RefrigRack%LaggedUsedHVACCoil               = 0.d0
        ENDIF
        IF (NumRefrigCondensers > 0) THEN
          HeatReclaimRefrigCondenser%UsedHVACCoil     = 0.0d0
          HeatReclaimRefrigCondenser%UsedWaterHeater  = 0.0d0
          Condenser%LaggedUsedWaterHeater             = 0.d0
          Condenser%LaggedUsedHVACCoil                = 0.d0
        ENDIF
        DO SystemID=1,NumRefrigSystems
          IF (ALLOCATED(System(SystemID)%MechSCLoad))System(SystemID)%MechSCLoad = 0.0d0
          System(SystemID)%LSHXTrans  = 0.0d0
          System(SystemID)%LSHXTransEnergy  = 0.0d0
        END DO

        IF (NumOfTimeStepInHour > 0.0d0) TimeStepFraction=1.0d0/REAL(NumOfTimeStepInHour,r64)
        MyBeginEnvrnFlag = .FALSE.

  END IF !(BeginEnvrnFlag .AND. MyBeginEnvrnFlag)

  IF(.NOT. BeginEnvrnFlag)MyBeginEnvrnFlag = .TRUE.

  !Avoid multiplying accumulation if go through zone/load time step more than once.
  IF(.NOT. WarmUpFlag)THEN   !because no accumulation is done during warm up
    !Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .false.
    IF((.NOT. UseSysTimeStep).AND.((NumSimulationCases > 0).OR.( NumSimulationWalkIns > 0)))THEN
      MyCurrentTime=(HourOfDay-1)+Timestep*TimeStepFraction
      IF(ABS(MyCurrentTime - MyCurrentTimeSaved) < MySmallNumber) THEN
      ! If the time step is repeated, need to return to correct values at start of time step
        IF (NumSimulationCases > 0) THEN
          DO CaseID = 1, NumSimulationCases
            RefrigCase(CaseID)%DefrostEnergy  = RefrigCase(CaseID)%DefrostEnergySaved
            RefrigCase(CaseID)%StockingEnergy = RefrigCase(CaseID)%StockingEnergySaved
            RefrigCase(CaseID)%WarmEnvEnergy  = RefrigCase(CaseID)%WarmEnvEnergySaved
            RefrigCase(CaseID)%KgFrost        = RefrigCase(CaseID)%KgFrostSaved
            RefrigCase(CaseID)%StoredEnergy   = RefrigCase(CaseID)%StoredEnergySaved
          END DO !caseid
        ENDIF    !numsimulationcases
        IF (NumSimulationWalkIns > 0) THEN
          DO WalkInID = 1, NumsimulationWalkIns
            WalkIn(WalkInID)%KgFrost          = WalkIn(WalkInID)%KgFrostSaved
            WalkIn(WalkInID)%StoredEnergy     = WalkIn(WalkInID)%StoredEnergySaved
            WalkIn(WalkinID)%IceTemp          = WalkIn(WalkInID)%IceTempSaved
          END DO
        ENDIF
        IF (NumRefrigSystems > 0) THEN
          DO SystemID = 1,NumRefrigSystems
            IF(System(SystemID)%CoilFlag)CYCLE
            System(SystemID)%UnmetEnergy      = System(SystemID)%UnmetEnergySaved
          END DO
        ENDIF
        IF (NumTransRefrigSystems > 0) THEN
          DO SystemID = 1,NumTransRefrigSystems
            TransSystem(SystemID)%UnmetEnergyMT      = TransSystem(SystemID)%UnmetEnergySavedMT
            TransSystem(SystemID)%UnmetEnergyLT      = TransSystem(SystemID)%UnmetEnergySavedLT
          END DO
        ENDIF
        IF (NumSimulationSecondarySystems > 0) THEN
          DO SecondID = 1,NumSimulationSecondarySystems
            IF(Secondary(SecondID)%CoilFlag)CYCLE
            Secondary(SecondID)%UnMetEnergy        = Secondary(SecondID)%UnMetEnergySaved
          END DO
        END IF

    ELSE
      ! First time through this Zone time step, so set saved values to those in place at start of this time step
      MyCurrentTimeSaved=MyCurrentTime
        IF (NumSimulationCases > 0) THEN
          DO CaseID = 1, NumSimulationCases
            RefrigCase(CaseID)%DefrostEnergySaved  = RefrigCase(CaseID)%DefrostEnergy
            RefrigCase(CaseID)%StockingEnergySaved = RefrigCase(CaseID)%StockingEnergy
            RefrigCase(CaseID)%WarmEnvEnergySaved  = RefrigCase(CaseID)%WarmEnvEnergy
            RefrigCase(CaseID)%KgFrostSaved        = RefrigCase(CaseID)%KgFrost
            RefrigCase(CaseID)%StoredEnergySaved   = RefrigCase(CaseID)%StoredEnergy
          END DO !caseid
        ENDIF    !numsimulationcases
        IF (NumSimulationWalkIns > 0) THEN
          DO WalkInID = 1, NumsimulationWalkIns
            WalkIn(WalkInID)%KgFrostSaved          = WalkIn(WalkInID)%KgFrost
            WalkIn(WalkInID)%StoredEnergySaved     = WalkIn(WalkInID)%StoredEnergy
            WalkIn(WalkInID)%IceTempSaved          = WalkIn(WalkinID)%IceTemp
          END DO
        ENDIF
        IF (NumRefrigSystems > 0) THEN
          DO SystemID = 1,NumRefrigSystems
            IF(System(SystemID)%CoilFlag)CYCLE
            System(SystemID)%UnmetEnergySaved      = System(SystemID)%UnmetEnergy
          END DO
        ENDIF
        IF (NumTransRefrigSystems > 0) THEN
          DO SystemID = 1,NumTransRefrigSystems
            TransSystem(SystemID)%UnmetEnergySavedMT    = TransSystem(SystemID)%UnmetEnergyMT
            TransSystem(SystemID)%UnmetEnergySavedLT    = TransSystem(SystemID)%UnmetEnergyLT
          END DO
        ENDIF
        IF (NumSimulationSecondarySystems > 0) THEN
          DO SecondID = 1,NumSimulationSecondarySystems
            IF(Secondary(SecondID)%CoilFlag)CYCLE
            Secondary(SecondID)%UnMetEnergySaved   = Secondary(SecondID)%UnMetEnergy
          END DO
        END IF
        !Following lagged variables set for consistency to value calculated prev time through HVAC time step loops
        IF(ALLOCATED(HeatReclaimRefrigeratedRack)) THEN
          DO IRack = 1,NumRefrigeratedRacks
            RefrigRack(IRack)%LaggedUsedHVACCoil    = HeatReclaimRefrigeratedRack(IRack)%UsedHVACCoil
            RefrigRack(IRack)%LaggedUsedWaterHeater = HeatReclaimRefrigeratedRack(IRack)%UsedWaterHeater
          END DO
        ENDIF
        IF (ALLOCATED(HeatReclaimRefrigCondenser)) THEN
          DO ICond = 1,NumRefrigCondensers
            Condenser(ICond)%LaggedUsedHVACCoil    = HeatReclaimRefrigCondenser(ICond)%UsedHVACCoil
            Condenser(ICond)%LaggedUsedWaterHeater = HeatReclaimRefrigCondenser(ICond)%UsedWaterHeater
          END DO
       END IF
   END IF !repeating same time step

 ELSE   ! using UseSysTimeStep as a flag for a chiller system

  MyStepStartTime = CurrentTime - TimeStepZone + SysTimeElapsed
  IF(ABS(MyStepStartTime - MyStepStartTimeSaved) < MySmallNumber) THEN
      ! If the time step is repeated, need to return to correct values at start of time step
        IF (NumSimulationRefrigAirChillers > 0) THEN
          DO CoilID = 1, NumSimulationRefrigAirChillers
            WarehouseCoil(CoilID)%KgFrost     = WarehouseCoil(CoilID)%KgFrostSaved
            WarehouseCoil(CoilID)%IceTemp     = WarehouseCoil(CoilID)%IceTempSaved
          END DO
        ENDIF
  ELSE   ! First time through this system time step or hvac loop,
         ! so set saved values to those in place at start of this time step
        MyStepStartTimeSaved = MyStepStartTime
        IF (NumSimulationRefrigAirChillers > 0) THEN
          DO CoilID = 1, NumSimulationRefrigAirChillers
            WarehouseCoil(CoilID)%KgFrostSaved = WarehouseCoil(CoilID)%KgFrost
            WarehouseCoil(CoilID)%IceTempSaved = WarehouseCoil(CoilID)%IceTemp
          END DO
        ENDIF
        !Following lagged variables set for consistency to value calculated prev time through HVAC time step loops
        IF(ALLOCATED(HeatReclaimRefrigeratedRack)) THEN
          DO IRack = 1,NumRefrigeratedRacks
            RefrigRack(IRack)%LaggedUsedHVACCoil    = HeatReclaimRefrigeratedRack(IRack)%UsedHVACCoil
            RefrigRack(IRack)%LaggedUsedWaterHeater = HeatReclaimRefrigeratedRack(IRack)%UsedWaterHeater
          END DO
        ENDIF
        IF (ALLOCATED(HeatReclaimRefrigCondenser)) THEN
          DO ICond = 1,NumRefrigCondensers
            Condenser(ICond)%LaggedUsedHVACCoil    = HeatReclaimRefrigCondenser(ICond)%UsedHVACCoil
            Condenser(ICond)%LaggedUsedWaterHeater = HeatReclaimRefrigCondenser(ICond)%UsedWaterHeater
          END DO
       END IF
    END IF ! if first time
 END IF !(.NOT. UseSysTimeStep)

END IF !warm up flag

IF (AnyEnergyManagementSystemInModel) THEN
  IF (NumRefrigSystems > 0) THEN
    DO SystemID = 1,NumRefrigSystems
      IF (System(SystemID)%EMSOverrideOnTCondenseMin) THEN
        System(SystemID)%TCondenseMin = System(SystemID)%EMSOverrideValueTCondenseMin
      ELSE
        System(SystemID)%TCondenseMin = System(SystemID)%TCondenseMinInput
      ENDIF
    END DO
  ENDIF
ENDIF

 RETURN

END SUBROUTINE InitRefrigeration

SUBROUTINE InitRefrigerationPlantConnections

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! do inits that should only occur when component model routines
          ! are entered from plant, for water cooled Condensers and Refrigeration Racks

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,  ONLY:PlantLoop, ScanPlantLoopsForObject, TypeOf_RefrigSystemWaterCondenser, &
                       TypeOf_RefrigerationWaterCoolRack
  USE PlantUtilities, ONLY: InitComponentNodes
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: MyBeginEnvrnFlag = .TRUE.
  LOGICAL       :: errFlag          = .FALSE.
  INTEGER       :: RefCondLoop      = 0 ! loop over Condenser
  INTEGER       :: RefCompRackLoop  = 0 ! loop over RefrigRack
  REAL(r64)     :: rho  ! local fluid property for cooling water


    !initialize plant topology information, if applicable
  IF (MyReferPlantScanFlag .AND. ALLOCATED(PlantLoop)) THEN
    DO RefCondLoop = 1, NumRefrigCondensers
      IF (.NOT. Condenser(RefCondLoop)%CondenserType == RefrigCondenserTypeWater) CYCLE

      errFlag   = .FALSE.
      CALL ScanPlantLoopsForObject(  Condenser(RefCondLoop)%Name, &
                                     TypeOf_RefrigSystemWaterCondenser, &
                                     Condenser(RefCondLoop)%PlantLoopNum, &
                                     Condenser(RefCondLoop)%PlantLoopSideNum, &
                                     Condenser(RefCondLoop)%PlantBranchNum, &
                                     Condenser(RefCondLoop)%PlantCompNum, &
                                     errFlag = errFlag )
      IF (errFlag) THEN
         CALL ShowFatalError('InitRefrigerationPlantConnections: Program terminated due to previous condition(s).')
      ENDIF

      rho = GetDensityGlycol( PlantLoop(Condenser(RefCondLoop)%PlantLoopNum)%FluidName, &
                              20.D0, &
                              PlantLoop(Condenser(RefCondLoop)%PlantLoopNum)%FluidIndex, &
                              'InitRefrigeration')

      IF ( Condenser(RefCondLoop)%FlowType    == ConstantFlow ) THEN
        Condenser(RefCondLoop)%MassFlowRateMax = Condenser(RefCondLoop)%DesVolFlowRate * rho
      ELSEIF (Condenser(RefCondLoop)%FlowType == VariableFlow) THEN
        Condenser(RefCondLoop)%MassFlowRateMax = Condenser(RefCondLoop)%VolFlowRateMax * rho
      ENDIF

    ENDDO

    DO RefCompRackLoop = 1, NumRefrigeratedRacks
      IF (.NOT. RefrigRack(RefCompRackLoop)%CondenserType == RefrigCondenserTypeWater) CYCLE

      errFlag   = .FALSE.
      CALL ScanPlantLoopsForObject(   RefrigRack(RefCompRackLoop)%Name, &
                                      TypeOf_RefrigerationWaterCoolRack, &
                                      RefrigRack(RefCompRackLoop)%PlantLoopNum, &
                                      RefrigRack(RefCompRackLoop)%PlantLoopSideNum, &
                                      RefrigRack(RefCompRackLoop)%PlantBranchNum, &
                                      RefrigRack(RefCompRackLoop)%PlantCompNum, &
                                      errFlag = errFlag )
      IF (errFlag) THEN
         CALL ShowFatalError('InitRefrigerationPlantConnections: Program terminated due to previous condition(s).')
      ENDIF

      rho = GetDensityGlycol( PlantLoop(RefrigRack(RefCompRackLoop)%PlantLoopNum)%FluidName, &
                              20.D0, &
                              PlantLoop(RefrigRack(RefCompRackLoop)%PlantLoopNum)%FluidIndex, &
                              'InitRefrigeration')

      IF ( RefrigRack(RefCompRackLoop)%FlowType     == ConstantFlow ) THEN
        RefrigRack(RefCompRackLoop)%MassFlowRateMax = RefrigRack(RefCompRackLoop)%DesVolFlowRate * rho
      ELSEIF ( RefrigRack(RefCompRackLoop)%FlowType == VariableFlow) THEN
        RefrigRack(RefCompRackLoop)%MassFlowRateMax = RefrigRack(RefCompRackLoop)%VolFlowRateMax * rho
      ENDIF

    ENDDO

    MyReferPlantScanFlag = .FALSE.
  ELSEIF (MyReferPlantScanFlag .AND. .NOT. AnyPlantInModel) THEN
    MyReferPlantScanFlag = .FALSE.
  ENDIF

  IF(BeginEnvrnFlag .AND. MyBeginEnvrnFlag)THEN

        ! do plant inits, if applicable
        IF (.NOT. MyReferPlantScanFlag) THEN
          DO RefCondLoop = 1, NumRefrigCondensers
            IF (.NOT. Condenser(RefCondLoop)%CondenserType == RefrigCondenserTypeWater) CYCLE

            rho = GetDensityGlycol(PlantLoop(Condenser(RefCondLoop)%PlantLoopNum)%FluidName, &
                                   20.d0, &
                                   PlantLoop(Condenser(RefCondLoop)%PlantLoopNum)%FluidIndex, &
                                   'InitRefrigeration')

            IF ( Condenser(RefCondLoop)%FlowType    == ConstantFlow ) THEN
              Condenser(RefCondLoop)%MassFlowRateMax = Condenser(RefCondLoop)%DesVolFlowRate * rho
            ELSEIF (Condenser(RefCondLoop)%FlowType == VariableFlow) THEN
              Condenser(RefCondLoop)%MassFlowRateMax = Condenser(RefCondLoop)%VolFlowRateMax * rho
            ENDIF

            CALL InitComponentNodes(0.d0, Condenser(RefCondLoop)%MassFlowRateMax, &
                                          Condenser(RefCondLoop)%InletNode, &
                                          Condenser(RefCondLoop)%OutletNode, &
                                          Condenser(RefCondLoop)%PlantLoopNum, &
                                          Condenser(RefCondLoop)%PlantLoopSideNum, &
                                          Condenser(RefCondLoop)%PlantBranchNum, &
                                          Condenser(RefCondLoop)%PlantCompNum)
          ENDDO
          DO RefCompRackLoop = 1, NumRefrigeratedRacks
            IF (.NOT. RefrigRack(RefCompRackLoop)%CondenserType == RefrigCondenserTypeWater) CYCLE

            rho = GetDensityGlycol(PlantLoop(RefrigRack(RefCompRackLoop)%PlantLoopNum)%FluidName, &
                                   20.d0, &
                                   PlantLoop(RefrigRack(RefCompRackLoop)%PlantLoopNum)%FluidIndex, &
                                   'InitRefrigeration')

            IF ( RefrigRack(RefCompRackLoop)%FlowType     == ConstantFlow ) THEN
              RefrigRack(RefCompRackLoop)%MassFlowRateMax = RefrigRack(RefCompRackLoop)%DesVolFlowRate * rho
            ELSEIF ( RefrigRack(RefCompRackLoop)%FlowType == VariableFlow) THEN
              RefrigRack(RefCompRackLoop)%MassFlowRateMax = RefrigRack(RefCompRackLoop)%VolFlowRateMax * rho
            ENDIF

            CALL InitComponentNodes(0.d0, RefrigRack(RefCompRackLoop)%MassFlowRateMax, &
                                          RefrigRack(RefCompRackLoop)%InletNode, &
                                          RefrigRack(RefCompRackLoop)%OutletNode, &
                                          RefrigRack(RefCompRackLoop)%PlantLoopNum, &
                                          RefrigRack(RefCompRackLoop)%PlantLoopSideNum, &
                                          RefrigRack(RefCompRackLoop)%PlantBranchNum, &
                                          RefrigRack(RefCompRackLoop)%PlantCompNum)
          ENDDO

        ENDIF
        MyBeginEnvrnFlag = .FALSE.

  END IF !(BeginEnvrnFlag .AND. MyBeginEnvrnFlag)

  IF(.NOT. BeginEnvrnFlag)MyBeginEnvrnFlag = .TRUE.


  RETURN

END SUBROUTINE InitRefrigerationPlantConnections


!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalcRackSystem(RackNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Oct/Nov 2004
          !       MODIFIED       Shirey, FSEC Dec 2004; Hudson, ORNL Feb 2007, July 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate compressor rack load, power, energy consumption, and condenser fan/pump power and consumption

          ! METHODOLOGY EMPLOYED:
          ! Loop through cases attached to each rack and determine total load on compressor rack

          ! REFERENCES:
          ! "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
          !  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

          ! USE STATEMENTS:
  USE CurveManager,      ONLY : CurveValue
  USE Psychrometrics,    ONLY: PsyRhoAirFnPbTdbW,RhoH2O,PsyWFnTdbTwbPb,PsyTwbFnTdbWPb
  USE DataEnvironment,   ONLY: OutBaroPress,OutHumRat,OutDryBulbTemp
  USE DataHVACGlobals,   ONLY: TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: RackNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: CaseID                      ! Index to absolute case ID
  INTEGER     :: CaseNum                     ! Index to refrigerated case attached to rack
  !INTEGER     :: SecondID                    ! Index to absolute secondary loop ID
  !INTEGER     :: SecondIndex                 ! Index to secondary loop attached to rack
  INTEGER     :: WalkInID                    ! Index to absolute walk-in ID
  INTEGER     :: WalkInIndex                 ! Index to walk-in attached to rack
  INTEGER     :: NumCases                    ! Total number of refrigerated cases attached to rack
  REAL(r64)   :: COPFTempOutput              ! Curve value for COPFTemp curve object
  REAL(r64)   :: CondenserFrac               ! Fraction of condenser power as a function of outdoor temperature
  REAL(r64)   :: TotalHeatRejectedToZone     ! Total compressor and condenser fan heat rejected to zone (based on CaseRAFactor)
  INTEGER     :: HeatRejectZoneNum           ! Index to zone where heat is rejected
  INTEGER     :: HeatRejectZoneNodeNum       ! Index to zone where heat is rejected
  REAL(r64)   :: OutWbTemp                   ! Outdoor wet bulb temp at condenser air inlet node [C]
  REAL(r64)   :: OutDbTemp                   ! Outdoor dry bulb temp at condenser air inlet node [C]
  REAL(r64)   :: EffectTemp                  ! Effective outdoor temp when using evap condenser cooling [C]
  REAL(r64)   :: HumRatIn                    ! Humidity ratio of inlet air to condenser [kg/kg]
  REAL(r64)   :: HumRatOut                   ! Humidity ratio of outlet air from condenser (assumed saturated) [kg/kg]
  REAL(r64)   :: BPress                      ! Barometric pressure at condenser air inlet node [Pa]
  LOGICAL     :: EvapAvail                   ! Control for evap condenser availability
  REAL(r64)   :: LocalTimeStep = 0.0d0       !TimeStepZone for case/walkin systems, TimeStepSys for coil systems
  INTEGER     :: CoilSetIndex  = 0           ! Index to set of coils in a single zone
  INTEGER     :: CoilSetID     = 0           ! Index to set of coils in a single zone (shared inlet and outlet nodes)
  INTEGER     :: CoilIndex     = 0           ! Index to a single air chiller/coil
  INTEGER     :: CoilID        = 0           ! Index to a single air chiller/coil

  NumCases               = RefrigRack(RackNum)%NumCases
  TotalRackDeliveredCapacity   = 0.0d0
  CompressorCOPactual          = 0.0d0
  TotalCompressorPower         = 0.0d0
  TotalCondenserFanPower       = 0.0d0
  TotalCondenserPumpPower      = 0.0d0
  TotalBasinHeatPower          = 0.0d0
  TotalCondenserHeat           = 0.0d0
  TotalHeatRejectedToZone      = 0.0d0
  TotalEvapWaterUseRate        = 0.0d0
  RackSenCreditToZone          = 0.0d0
  RackSenCreditToHVAC          = 0.0d0
  CondenserFrac                = 0.0d0
  EvapAvail                    = .TRUE.
  HeatRejectZoneNum            = 0
  HeatRejectZoneNodeNum        = 0

LocalTimeStep = TimeStepZone
IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

!Loads for chiller sets are set in call to zone equipment element "SimAirChillerSet"
! (all chiller coils within a set are located in the same zone)
! (note non-zone, such as refrigeration, and zone equip, such as airchillersets, called at diff times)
! Loads are then defined for each chiller coil within the set in "CalculateAirChillerSet"
! In that subroutine, dispatch coils within each set in order specified for each zone
!  Below will assign loads to refrigeration system or secondary loop
!Note that this routine will go through all refrigeration systems, but loads for multiple systems
! with interactions will not be known for the intitial calls with first HVAC time step. They will,
! however, be repeated when the last chiller set is called from ZoneEquipmentManager
! that's why important where init goes, don't want to zero out data should keep
  IF(UseSysTimeStep) THEN
    DO CoilSetIndex=1,NumRefrigChillerSets
     CoilSetID = CoilSetIndex
     CALL CalculateAirChillerSets(CoilSetID)
    END DO
  END IF

  IF(RefrigRack(RackNum)%NumCoils > 0) THEN
  DO CoilIndex=1,RefrigRack(RackNum)%NumCoils
     CoilID=RefrigRack(RackNum)%CoilNum(CoilIndex)
     ! already CALLed CalculateCoil(CoilID) in CoilSet specified order
     ! increment TotalCoolingLoad for Compressors/condenser on each system
     TotalRackDeliveredCapacity = TotalRackDeliveredCapacity + WarehouseCoil(CoilID)%TotalCoolingLoad
!     System(SysNum)%TotalCondDefrostCredit=System(SysNum)%TotalCondDefrostCredit + WarehouseCoil(CoilID)%HotDefrostCondCredit
  END DO !NumCoils systems
  END IF !System(SysNum)%NumCoils > 0

  IF (NumCases > 0) THEN
  DO CaseNum = 1, NumCases
    CaseID = RefrigRack(RackNum)%CaseNum(CaseNum)
    CALL CalculateCase(CaseID)

!   add evaporator load for all cases connected to rack
    TotalRackDeliveredCapacity = TotalRackDeliveredCapacity + RefrigCase(CaseID)%TotalCoolingLoad

!   sensible and latent case credits already calculated in "CalculateCase"
!   Now need to calculate amount of condenser heat rejection that should be applied to zone
!                                     (used when HeatRejectionLocation = LocationZone)
!   if walk-ins are served by rack, user must specify heat rejection zone and 100% of heat
!   rejection goes to that zone - that is, no heat rejection goes to the HVAC return air
    IF (RefrigRack(RackNum)%HeatRejectionLocation == LocationZone) THEN
      IF (RefrigRack(RackNum)%NumWalkIns == 0)THEN
        TotalHeatRejectedToZone = TotalHeatRejectedToZone + &
                                 RefrigCase(CaseID)%TotalCoolingLoad * (1.0d0 - CaseRAFactor)
        !  CaseRAFactor is a module variable calculated in CalculateCase
        !   find zone number of first case on rack (all cases are in the same zone
        !  if HeatRejectionLocation = LocationZone and no walk-ins)
        HeatRejectZoneNum      = RefrigCase(RefrigRack(RackNum)%CaseNum(1))%ActualZoneNum
        HeatRejectZoneNodeNum  = RefrigCase(RefrigRack(RackNum)%CaseNum(1))%ZoneNodeNum
      ELSE ! have walk ins so no reduction in condenser heat rejection for caseRA factor
        TotalHeatRejectedToZone = TotalHeatRejectedToZone + RefrigCase(CaseID)%TotalCoolingLoad
      END IF ! no walk ins
    END IF
  END DO  !NumCases
  END IF !Numcases on rack > 0

  IF(RefrigRack(RackNum)%NumWalkIns > 0) THEN
    DO WalkInIndex=1,RefrigRack(RackNum)%NumWalkIns
       WalkInID=RefrigRack(RackNum)%WalkInNum(WalkInIndex)
       CALL CalculateWalkIn(WalkInID)
       TotalRackDeliveredCapacity = TotalRackDeliveredCapacity + WalkIn(WalkInID)%TotalCoolingLoad
       IF (RefrigRack(RackNum)%HeatRejectionLocation == LocationZone) THEN
         TotalHeatRejectedToZone    = TotalHeatRejectedToZone + WalkIn(WalkInID)%TotalCoolingLoad
         HeatRejectZoneNum     = RefrigRack(RackNum)%HeatRejectionZoneNum
         HeatRejectZoneNodeNum = RefrigRack(RackNum)%HeatRejectionZoneNodeNum
       END IF !reject heat to zone
     END DO !WalkInIndex
  END IF !NumWalkIns>0

  IF(RefrigRack(RackNum)%HeatRejectionLocation == LocationZone) THEN
    COPFTempOutput   = CurveValue(RefrigRack(RackNum)%COPFTempPtr,Node(HeatRejectZoneNodeNum)%Temp)
    EvapAvail = .FALSE.
  ELSE
    IF (RefrigRack(RackNum)%OutsideAirNodeNum /= 0) THEN
      OutDbTemp = Node(RefrigRack(RackNum)%OutsideAirNodeNum)%Temp
      BPress = Node(RefrigRack(RackNum)%OutsideAirNodeNum)%Press
    ELSE
      OutDbTemp=OutDryBulbTemp
      BPress=OutBaroPress
    ENDIF
    EffectTemp = OutDbTemp


  ! IF schedule exists, evap condenser can be scheduled OFF
  ! Check schedule to determine evap condenser availability
    IF(RefrigRack(RackNum)%EvapSchedPtr > 0 .AND. &
     GetCurrentScheduleValue(RefrigRack(RackNum)%EvapSchedPtr)== 0) EvapAvail = .FALSE.

  ! Evaporative condensers will have their water flow shut off in cold months to avoid
  !  'spectacular' icing problems.  Ideally, the user will use the evaporative schedule input
  !  to set such a schedule.  However, sometimes, users will use a single input deck to model
  !  one building in multiple climates, and may not think to put in such a schedule in the colder
  !  climates.  To accomodate such applications, the variable EvapCutOutTdb is used as an extra
  !  check.
    IF(OutDbTemp < EvapCutOutTdb)EvapAvail = .FALSE.

    IF (RefrigRack(RackNum)%CondenserType==RefrigCondenserTypeEvap .AND. EvapAvail) THEN
    ! determine temps for evap cooling
      IF (RefrigRack(RackNum)%OutsideAirNodeNum /= 0) THEN
        HumRatIn = Node(RefrigRack(RackNum)%OutsideAirNodeNum)%HumRat
      ELSE
        HumRatIn = OutHumRat
      ENDIF   !outsideairnode
      OutWbTemp= PsyTwbFnTdbWPb(OutDbTemp,HumRatIn,BPress)
      EffectTemp= OutWbTemp + (1.0d0-RefrigRack(RackNum)%EvapEffect)*(OutDbTemp-OutWbTemp)
    END IF    !evapAvail

    ! Obtain water-cooled condenser inlet/outlet temps
    IF (RefrigRack(RackNum)%CondenserType==RefrigCondenserTypeWater) THEN
    InletNode = RefrigRack(RackNum)%InletNode
    OutletNode = RefrigRack(RackNum)%OutletNode
    RefrigRack(RackNum)%InletTemp = Node(InletNode)%Temp
    EffectTemp = Node(InletNode)%Temp + 5.0d0  ! includes approach temp
      IF (RefrigRack(RackNum)%InletTemp < RefrigRack(RackNum)%InletTempMin) THEN
    !   RefrigRack(RackNum)%LowTempWarn = RefrigRack(RackNum)%LowTempWarn +1
       IF (RefrigRack(RackNum)%LowTempWarnIndex == 0) THEN
         CALL ShowWarningMessage('Refrigeration:CompressorRack: '//TRIM(RefrigRack(RackNum)%Name))
         CALL ShowContinueError('Water-cooled condenser inlet temp lower than minimum allowed temp. '// &
            'Check returning water temperature and/or minimum temperature setpoints.')
       END IF !LowTempWarnIndex
       CALL ShowRecurringWarningErrorAtEnd('Refrigeration Compressor Rack '// TRIM(RefrigRack(RackNum)%Name) // &
        ' - Condenser inlet temp lower than minimum allowed ... continues',&
            RefrigRack(RackNum)%LowTempWarnIndex)
       !END IF  !LowTempWarn
      END IF   !InletTempMin
    END IF     !RefrigCondenserTypeWater

    COPFTempOutput           = CurveValue(RefrigRack(RackNum)%COPFTempPtr,EffectTemp)
  END IF       !Location Zone

  CompressorCOPactual        = RefrigRack(RackNum)%RatedCOP * COPFTempOutput

  IF(CompressorCOPactual > 0.0d0) THEN
    TotalCompressorPower               = TotalRackDeliveredCapacity / CompressorCOPactual
    TotalCondenserHeat                 = TotalCompressorPower + TotalRackDeliveredCapacity
  ELSE
    IF(ShowCOPWarning(RackNum)) THEN
      CALL ShowWarningError('Refrigeration:CompressorRack: '//TRIM(RefrigRack(RackNum)%Name))
      CALL ShowContinueError(' The calculated COP has a value of zero or is negative. Refer to Engineering Documentation for')
      CALL ShowContinueError(' further explanation of Compressor Rack COP as a Function of Temperature Curve.')
      ShowCOPWarning(RackNum) = .FALSE.
    END IF
  END IF

  !calculate condenser fan usage here if not water-cooled; if water-cooled, fan is in separate tower object
  ! fan loads > 0 only if the connected cases are operating
  IF(TotalRackDeliveredCapacity > 0.0d0 .AND. RefrigRack(RackNum)%CondenserType /= RefrigCondenserTypeWater) THEN
    IF(RefrigRack(RackNum)%TotCondFTempPtr /= 0) THEN
      IF(RefrigRack(RackNum)%HeatRejectionLocation == LocationZone) THEN
        CondenserFrac            = MAX(0.0d0,MIN(1.0d0,CurveValue(RefrigRack(RackNum)%TotCondFTempPtr, &
                                       Node(HeatRejectZoneNodeNum)%Temp)))
        TotalCondenserFanPower   = RefrigRack(RackNum)%CondenserFanPower * CondenserFrac
        RefrigCaseCredit(HeatRejectZoneNum)%SenCaseCreditToZone = &
                               RefrigCaseCredit(HeatRejectZoneNum)%SenCaseCreditToZone + &
                               RefrigRack(RackNum)%CondenserFanPower * CondenserFrac
      ELSE
        CondenserFrac            = MAX(0.0d0,MIN(1.0d0,CurveValue(RefrigRack(RackNum)%TotCondFTempPtr,EffectTemp)))
        TotalCondenserFanPower   = RefrigRack(RackNum)%CondenserFanPower * CondenserFrac
      END IF  !location zone
    ELSE
      CondenserFrac              = 1.0d0
      TotalCondenserFanPower     = RefrigRack(RackNum)%CondenserFanPower * CondenserFrac
    END IF    !TotCondFTempPtr
  END IF      !Cooling Water type

  ! calculate evap water use and water pump power, if applicable
  ! assumes pump runs whenever evap cooling is available to minimize scaling
    IF(RefrigRack(RackNum)%CondenserType==RefrigCondenserTypeEvap .AND. EvapAvail) THEN
        TotalCondenserPumpPower  = RefrigRack(RackNum)%EvapPumpPower
        HumRatOut = PsyWFnTdbTwbPb(EffectTemp,OutWbTemp,BPress)
        TotalEvapWaterUseRate = RefrigRack(RackNum)%CondenserAirFlowRate* CondenserFrac * &
         PsyRhoAirFnPbTdbW(BPress,OutDbTemp,HumRatIn) * (HumRatOut-HumRatIn) / RhoH2O(EffectTemp)
    END IF   !evapAvail
  ! calculate basin water heater load
  IF(RefrigRack(RackNum)%CondenserType == RefrigCondenserTypeEvap) THEN
    IF ((TotalRackDeliveredCapacity == 0.0d0) .AND. &
       (EvapAvail)                          .AND. &
       (OutDbTemp < RefrigRack(RackNum)%BasinHeaterSetPointTemp)) THEN
          TotalBasinHeatPower = MAX(0.0d0,RefrigRack(RackNum)%BasinHeaterPowerFTempDiff * &
               (RefrigRack(RackNum)%BasinHeaterSetPointTemp - OutDbTemp))
          ! provide warning if no heater power exists
          IF (TotalBasinHeatPower == 0.0d0) THEN
            !RefrigRack(RackNum)%EvapFreezeWarn = RefrigRack(RackNum)%EvapFreezeWarn + 1
            IF (RefrigRack(RackNum)%EvapFreezeWarnIndex == 0) THEN
              CALL ShowWarningMessage('Refrigeration Compressor Rack '// TRIM(RefrigRack(RackNum)%Name) // &
                                    ' - Evap cooling of condenser underway with no basin heater power')
              CALL ShowContinueError('and condenser inlet air dry-bulb temp at or below the basin heater setpoint temperature.' )
              CALL ShowContinueErrorTimeStamp('Continuing simulation.')
            END IF !EvapFreezeWarnIndex == 0
            CALL ShowRecurringWarningErrorAtEnd('Refrigeration Compressor Rack '// TRIM(RefrigRack(RackNum)%Name) // &
                                  ' - Evap cooling of condenser underway with no basin heater power ... continues',&
                     RefrigRack(RackNum)%EvapFreezeWarnIndex)
            !END IF
          END IF ! TotalBasinHeatPower == 0 when at outdoor freezing conditions
    END IF ! cap
  END IF !evap condenser type

! add in compressor and condenser fan power to rack heat rejection variables if the heat rejection location is to the zone
!   if walk-ins are served by rack, user must specify heat rejection zone and 100% of heat
!   rejection goes to that zone - that is, no condenser heat rejection goes to the HVAC return air
  IF (RefrigRack(RackNum)%HeatRejectionLocation == LocationZone) THEN
    TotalCondenserHeat = TotalRackDeliveredCapacity + TotalCompressorPower + TotalCondenserFanPower
    IF(HeatRejectZoneNum > 0 .AND. TotalRackDeliveredCapacity > 0.0d0) THEN
      IF (RefrigRack(RackNum)%NumWalkIns == 0)THEN
!       rack report variables for condenser heat to Zone and/or HVAC
!       The difference between TotalHeatRejectedToZone and TotalRackDeliveredCapacity is the heat sent to return air
        RackSenCreditToZone = TotalCondenserHeat * (TotalHeatRejectedToZone / TotalRackDeliveredCapacity)
        RackSenCreditToHVAC = TotalCondenserHeat - RackSenCreditToZone
      ELSE ! walkins present and no rack heat rejection goes to return air
        RackSenCreditToZone = TotalCondenserHeat
        RackSenCreditToHVAC = 0.d0
      END IF !walkins present
!     Update globals for use in Air Heat Balance and Zone Equipment Manager
      RefrigCaseCredit(HeatRejectZoneNum)%SenCaseCreditToZone = &
                              RefrigCaseCredit(HeatRejectZoneNum)%SenCaseCreditToZone + RackSenCreditToZone

      RefrigCaseCredit(HeatRejectZoneNum)%SenCaseCreditToHVAC = &
                              RefrigCaseCredit(HeatRejectZoneNum)%SenCaseCreditToHVAC + RackSenCreditToHVAC
    END IF !zone # > 0 and tot del cap > 0
  END IF  !rack heat rejection to zone

  RETURN

END SUBROUTINE CalcRackSystem

!***************************************************************************************************

SUBROUTINE ReportRackSystem(RackNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Oct/Nov 2004
          !       MODIFIED       Hudson, ORNL Feb 2007, July 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To report compressor rack variables

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataWater,   ONLY: WaterStorage
  USE DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: RackNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)   :: LocalTimeStep = 0.0d0    !TimeStepZone for case/walkin systems, TimeStepSys for coil systems
  INTEGER      :: DemandARRID   = 0     ! Index to water tank Demand used for evap condenser on rack
  INTEGER      :: RackTankID   = 0      ! Index to water tank used for evap condenser on rack

  LocalTimeStep = TimeStepZone
  IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

  RefrigRack(RackNum)%RackCompressorPower      = TotalCompressorPower
  RefrigRack(RackNum)%RackElecConsumption      = TotalCompressorPower * LocalTimeStep * SecInHour
  RefrigRack(RackNum)%ActualCondenserFanPower  = TotalCondenserFanPower
  RefrigRack(RackNum)%CondenserFanConsumption  = TotalCondenserFanPower * LocalTimeStep * SecInHour
  RefrigRack(RackNum)%RackCapacity             = TotalRackDeliveredCapacity
  RefrigRack(RackNum)%RackCoolingEnergy        = TotalRackDeliveredCapacity * LocalTimeStep * SecInHour
  RefrigRack(RackNum)%RackCompressorCOP        = CompressorCOPactual
  RefrigRack(RackNum)%SensHVACCreditHeatRate   = RackSenCreditToHVAC
  RefrigRack(RackNum)%SensHVACCreditHeat       = RackSenCreditToHVAC * LocalTimeStep * SecInHour
  RefrigRack(RackNum)%SensZoneCreditHeatRate   = RackSenCreditToZone
  RefrigRack(RackNum)%SensZoneCreditHeat       = RackSenCreditToZone * LocalTimeStep * SecInHour
  RefrigRack(RackNum)%EvapWaterConsumpRate     = TotalEvapWaterUseRate
  RefrigRack(RackNum)%EvapWaterConsumption     = TotalEvapWaterUseRate * LocalTimeStep * SecInHour
  RefrigRack(RackNum)%ActualEvapPumpPower      = TotalCondenserPumpPower
  RefrigRack(RackNum)%EvapPumpConsumption      = TotalCondenserPumpPower * LocalTimeStep * SecInHour
  RefrigRack(RackNum)%BasinHeaterPower         = TotalBasinHeatPower
  RefrigRack(RackNum)%BasinHeaterConsumption   = TotalBasinHeatPower * LocalTimeStep * SecInHour
  RefrigRack(RackNum)%CondLoad                 = TotalCondenserHeat
  RefrigRack(RackNum)%CondEnergy               = TotalCondenserHeat * LocalTimeStep * SecInHour
! Set total rack heat rejection used for heat reclaim. Do not allow heat reclaim on stand alone (indoor) display cases.
  IF(RefrigRack(RackNum)%HeatRejectionLocation == LocationZone) THEN
    HeatReclaimRefrigeratedRack(RackNum)%AvailCapacity = 0.0d0
  ELSE
    HeatReclaimRefrigeratedRack(RackNum)%AvailCapacity = TotalRackDeliveredCapacity * (1.0d0 + 1.0d0/CompressorCOPactual)
  END IF

  !set water system demand request (if needed)
  IF (RefrigRack(RackNum)%EvapWaterSupplyMode == WaterSupplyFromTank) THEN
    DemandARRID = RefrigRack(RackNum)%EvapWaterTankDemandARRID
    RackTankID  = RefrigRack(RackNum)%EvapWaterSupTankID
    WaterStorage(RackTankID)%VdotRequestDemand(DemandARRID) = RefrigRack(RackNum)%EvapWaterConsumpRate
  END IF

  CALL SumZoneImpacts

  RETURN

END SUBROUTINE ReportRackSystem

!***************************************************************************************************

SUBROUTINE CalculateCase(CaseID)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad and Don Shirey, FSEC
          !       DATE WRITTEN   Oct/Nov 2004
          !       MODIFIED       Therese Stovall, ORNL, May 2008
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To model refrigerated cases.

          ! METHODOLOGY EMPLOYED:
          ! Case performance is based on a latent component calculated using a user input curve object. The sensible
          ! component is made up of all equipment loads (fan, light, anti-sweat) and the sensible case credit
          ! calculated during initialization. A master schedule is used for the refrigerated case operation and
          ! additional schedules control the lights and defrost operation.
          ! The fan is assumed to be off for Hot-Gas and Electric defrost.

          ! Unmet loads are accumulated to be met the following time step.  This usually occurs only during the
          ! defrost period, so the case calls for full capacity at the end of defrost to make up for the sensible
          ! case gains during the defrost period. This feature is also used if needed for restocking loads.

          ! REFERENCES:

          ! "Calculation of Humidity Effects on Energy Requirements of Refrigerated Display Cases",
          !  R. H. Howell, Ph. D., P.E., ASHRAE Paper, 3687 (CH-93-16-4) (RP-596)

          ! "Effects of Store Relative Humidity on Refrigerated Display Case Performance",
          !  R. H. Howell, Ph. D., P.E., ASHRAE Paper, 3686 (CH-93-16-1) (RP-596)

          ! "Analysis of Supermarket Dehumidification Alternatives",
          !  Electric Power Research Institute, EPRI TR-100352, Project 2891-03 Final Report, Nov. 1992.

          ! "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
          !  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataLoopNode
  USE DataEnvironment, ONLY:OutBaroPress !, Month
  USE Psychrometrics, ONLY: PsyRhFnTdbWPb, PsyTdpFnWPb

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: CaseID      ! Absolute pointer to refrigerated case

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER      :: ActualZoneNum   = 0           ! Index to zone
INTEGER      :: DefCapCurvePtr = 0
INTEGER      :: DefrostEnergyCurveType = 0
INTEGER      :: DefrostType = 0
INTEGER      :: ZoneNodeNum  = 0             ! Zone node number
REAL(r64)    :: CapAvail                =0.0d0 ! capacity available to meet current and stored load (W)
REAL(r64)    :: CaseRAFraction          =0.0d0 ! Fraction of case credits applied to return air
REAL(r64)    :: CaseCreditFraction      =0.0d0 ! Reduction in case credits due to e.g., reduced door openings at night
REAL(r64)    :: CaseSenCreditToZone     =0.0d0 ! Amount of sensible case credit applied to zone load (W)
REAL(r64)    :: CaseLatCreditToZone     =0.0d0 ! Amount of latent case credit applied to zone load (W)
REAL(r64)    :: CaseSenCreditToHVAC     =0.0d0 ! Amount of sensible case credit applied to HVAC RA duct (W)
REAL(r64)    :: CaseLatCreditToHVAC     =0.0d0 ! Amount of latent case credit applied to HVAC RA duct (W)
REAL(r64)    :: CaseSchedule            =0.0d0 ! Current value of case operating (availability) schedule
REAL(r64)    :: DefrostEnergy           =0.0d0 ! Energy form of defrost capacity (J)
REAL(r64)    :: DefrostSchedule         =0.0d0 ! Display case defrost schedule
REAL(r64)    :: DefrostDripDownSchedule =0.0d0 ! Display case drip-down schedule (allows coil to drain after defrost)
REAL(r64)    :: DefCapModFrac           =0.0d0 ! Defrost capacity modifier curve based on case operating temperature
REAL(r64)    :: DefrostRatio            =0.0d0 !ratio of defrost energy at current zone temp/humrat to defrost
                                             !    capacity at design condition
REAL(r64)    :: DefrostLoad_Actual      =0.0d0 ! heat load on case due to defrost (W)
REAL(r64)    :: DefrostCap_Actual       =0.0d0 ! power used to defrost (W)
REAL(r64)    :: DeltaFreezeKgFrost      =0.0d0 ! change in frost on coils (kg)
REAL(r64)    :: DeltaStockingEnergy     =0.0d0 ! Used to keep track of problems if sizing not consistent (J)
REAL(r64)    :: DeltaWarmEnvEnergy      =0.0d0 ! Used to keep track of problems if sizing not consistent (J)
REAL(r64)    :: DesignRatedCap          =0.0d0 ! Design rated capacity of display case (W)
REAL(r64)    :: DesignDefrostCap        =0.0d0 ! Design defrost capacity of display case (W)
REAL(r64)    :: DesignLatentCap         =0.0d0 ! Design latent capacity of display case (W)
REAL(r64)    :: DesignLighting          =0.0d0 ! Total design display case lighting power (W)
REAL(r64)    :: FrostMeltedKg           =0.0d0 ! Frost melted by defrost during a time step (kg)
REAL(r64)    :: LatentLoad              =0.0d0 ! Latent load placed on case at actual zone conditions (W)
REAL(r64)    :: LatentRatio             =0.0d0 !ratio of latent capacity at current zone temp/humrat to
                                             !    latent capacity at design condition
REAL(r64)    :: LatentCap_Actual        =0.0d0 ! Refrigerated case latent capacity at specific operating conditions
REAL(r64)    :: LatentCaseCredit        =0.0d0 ! Latent case credit delivered to zone (W)
REAL(r64)    :: LatCapModFrac           =0.0d0 ! Latent capacity modifier curve based on case operating temperature
REAL(r64)    :: LightingSchedule        =0.0d0 ! Display case lighting schedule
REAL(r64)    :: Length                  =0.0d0 ! Length of display case (m)
REAL(r64)    :: LoadRequested           =0.0d0 ! TotalLoad_Actual  + StoredEnergyRate
REAL(r64)    :: RatedAmbientRH          =0.0d0 ! Local variable for the RH corresponding to case rating conditions
REAL(r64)    :: SensibleCaseCredit      =0.0d0 ! Sensible case credit delivered to zone (W)
REAL(r64)    :: SensibleCap_Actual      =0.0d0 ! Refrigerated case sensible capacity at specific operating conditions
!REAL(r64)    :: SensibleFraction        =0.0d0 ! Portion of total load due to sensible load
REAL(r64)    :: SensibleLoadPrime       =0.0d0 ! Sensible load due to cond, conv, rad, infil (W)
REAL(r64)    :: SensibleLoadAux         =0.0d0 ! Sensible load due to heaters, lighting (W)
REAL(r64)    :: SensibleLoadTotal       =0.0d0 ! Total sensible load on case, may not = capacity applied (W)
REAL(r64)    :: StockingSchedule        =0.0d0 ! Current value of product stocking schedule (W/m)
REAL(r64)    :: StockingLoad            =0.0d0 ! Total load due to stocking case product (W)
REAL(r64)    :: StoredEnergyRate        =0.0d0 ! Rate needed to serve all stored energy during single time step (W)
REAL(r64)    :: TotalLoad_Actual        =0.0d0 ! total load on case at zone conditions (W)
REAL(r64)    :: StartFrostKg            =0.0d0 ! frost load at start of time step (kg of ice)
REAL(r64)    :: TotalCap_Actual         =0.0d0 ! Refrigerated case total capacity at specific operating conditions
REAL(r64)    :: TotalLightingLoad       =0.0d0 ! Total lighting energy rate (W)
REAL(r64)    :: TotalFan                =0.0d0 ! Total fan energy rate (W)
REAL(r64)    :: TotalAntiSweat          =0.0d0 ! Total anti-sweat heater energy rate (W)
REAL(r64)    :: TotalLightToCase        =0.0d0 ! Lighting energy to case
REAL(r64)    :: TotalASHeaterToCase     =0.0d0 ! Anti-sweat heater energy to case
REAL(r64)    :: TotalLightToZone        =0.0d0 ! Lighting energy to zone
REAL(r64)    :: TotalASHeaterToZone     =0.0d0 ! Anti-sweat heater energy to zone
REAL(r64)    :: TCase                   =0.0d0 ! Display case operating temperature
REAL(r64)    :: ZoneRHPercent           =0.0d0 ! Zone relative humidity (%)
REAL(r64)    :: ZoneDewPoint            =0.0d0 ! Zone dew point (C)
REAL(r64)    :: ZoneTempFactor          =0.0d0 ! used to look at extra sensible load due to excursions in zone T

! Refrigerated display case defrost type (parameters)
! DefNone             = 0
! DefOffCycle         = 1
! DefHotFluid           = 2
! DefHotFluidOnDemand   = 3 (not available)
! DefHotFluidTerm       = 4
! DefElectric         = 5
! DefElectricOnDemand = 6 (not available)
! DefElectricTerm     = 7

  !Initialize this case for this time step (
  !     All report variables prev set to zero for case when schedule for case is 'off')
  TotalCap_Actual     = 0.0d0
  LatentCap_Actual    = 0.0d0
  SensibleCap_Actual  = 0.0d0
  SensibleLoadTotal  = 0.0d0
  SensibleLoadPrime   = 0.0d0
  SensibleLoadAux     = 0.0d0
  DefrostLoad_Actual  = 0.0d0
  DefrostCap_Actual   = 0.0d0

  DefrostRatio        = 0.0d0
  LatentRatio         = 0.0d0
  StartFrostKg        = 0.0d0

  SensibleCaseCredit  = 0.0d0
  LatentCaseCredit    = 0.0d0

  CaseSenCreditToZone = 0.0d0
  CaseLatCreditToZone = 0.0d0
  CaseSenCreditToHVAC = 0.0d0
  CaseLatCreditToHVAC = 0.0d0
  CaseRAFactor        = 0.0d0

  TotalLightingLoad   = 0.0d0
  TotalAntiSweat      = 0.0d0
  TotalFan            = 0.0d0

  !Set local subroutine variables for convenience
  ActualZoneNum             = RefrigCase(CaseID)%ActualZoneNum
  ZoneNodeNum               = RefrigCase(CaseID)%ZoneNodeNum
  ZoneRHPercent             = PsyRhFnTdbWPb(Node(ZoneNodeNum)%Temp,Node(ZoneNodeNum)%Humrat,OutBaroPress)*100.0d0
  ZoneDewPoint              = PsyTdpFnWPb(Node(ZoneNodeNum)%Humrat,OutBaroPress)
  Length                    = RefrigCase(CaseID)%Length
  TCase                     = RefrigCase(CaseID)%Temperature
  DesignRatedCap            = RefrigCase(CaseID)%DesignRatedCap
  DesignLatentCap           = RefrigCase(CaseID)%DesignLatentCap
  DesignDefrostCap          = RefrigCase(CaseID)%DesignDefrostCap
  DesignLighting            = RefrigCase(CaseID)%DesignLighting
  DefCapCurvePtr            = RefrigCase(CaseID)%DefCapCurvePtr
  DefrostEnergyCurveType    = RefrigCase(CaseID)%DefrostEnergyCurveType
  DefrostType               = RefrigCase(CaseID)%DefrostType
  RatedAmbientRH            = RefrigCase(CaseID)%RatedAmbientRH

! GET ALL SCHEDULES (note all schedules can be fractions if on/off a portion of time step)
  ! case schedule should be coincident with the zone time step otherwise the simulation proceeds
  CaseSchedule = GetCurrentScheduleValue(RefrigCase(CaseID)%SchedPtr)
  IF (CaseSchedule <= 0) RETURN
  ! get defrost schedule
  IF(DefrostType > DefNone) THEN
    DefrostSchedule         = GetCurrentScheduleValue(RefrigCase(CaseID)%DefrostSchedPtr)
    DefrostDripDownSchedule = GetCurrentScheduleValue(RefrigCase(CaseID)%DefrostDripDownSchedPtr)
    !next statement In case user doesn't understand concept of drip down schedule
    DefrostDripDownSchedule = MAX(DefrostDripDownSchedule,DefrostSchedule)
  ELSE
    DefrostSchedule         = 0.0d0
    DefrostDripDownSchedule = 0.0d0
  END IF
! get product stocking schedule and load due to product stocking, if no schedule exists load is 0
  IF (RefrigCase(CaseID)%StockingSchedPtr > 0) THEN
    StockingSchedule        = GetCurrentScheduleValue(RefrigCase(CaseID)%StockingSchedPtr)
  ELSE
    StockingSchedule        = 0.0d0
  END IF
  ! get lighting schedule and total load due to lighting
  LightingSchedule          = GetCurrentScheduleValue(RefrigCase(CaseID)%LightingSchedPtr)

  ! if case credit reduction fraction schedule exists, modify both sensible and latent case credits
  ! according to schedule - used to account for variable case envelope, such as night covers.
  IF(RefrigCase(CaseID)%CaseCreditFracSchedPtr /= 0) THEN
    CaseCreditFraction      = GetCurrentScheduleValue(RefrigCase(CaseID)%CaseCreditFracSchedPtr)
  ELSE
    CaseCreditFraction      = 1.0d0
  END IF

! CALCULATE AUX LOADS DUE TO LIGHTS, FAN AND STOCKING
  TotalLightingLoad         = DesignLighting * LightingSchedule
  TotalLightToCase          = TotalLightingLoad*RefrigCase(CaseID)%LightingFractionToCase
  TotalLightToZone          = TotalLightingLoad - TotalLightToCase
  ! cycle fan according to defrost schedule
  ! turn fan on for none or off-cycle defrost types
  IF(DefrostType == DefNone .OR. DefrostType == DefOffCycle) THEN
    TotalFan                = RefrigCase(CaseID)%DesignFanPower
  ELSE
    TotalFan                = RefrigCase(CaseID)%DesignFanPower * ( 1.0d0 - DefrostDripDownSchedule )
  END IF
  ! get  load due to product stocking
  ! accumulate stocking loads for reporting to help evaluate any cumulative unmet loads problems
  ! only accumulate energy during actual simulation (so same if DD's are switched)
  StockingLoad             = StockingSchedule * Length
  IF(.NOT. WarmUpFlag) THEN
    DeltaStockingEnergy = (StockingLoad * TimeStepZone * SecInHour)
    RefrigCase(CaseID)%StockingEnergy =  RefrigCase(CaseID)%StockingEnergy + DeltaStockingEnergy
  END IF !warm up
! CALCULTE ALL LOADS INFLUENCED BY ZONE TEMPERATURE AND RH
  ! Anti-sweat heater capacity
  SELECT CASE (RefrigCase(CaseID)%AntiSweatControlType)
   CASE (ASNone)
     TotalAntiSweat         = 0.0d0
   CASE (ASConstant)
     TotalAntiSweat         = RefrigCase(CaseID)%AntiSweatPower
   CASE (ASLinear)
     TotalAntiSweat         = RefrigCase(CaseID)%AntiSweatPower * &
            MIN(1.0d0, MAX(0.0d0,1.0d0-(RatedAmbientRH-ZoneRHPercent)/ &
           (RatedAmbientRH-RefrigCase(CaseID)%HumAtZeroAS)))
     TotalAntiSweat         = MAX(RefrigCase(CaseID)%MinimumASPower, TotalAntiSweat)
   CASE (ASDewPoint)
     TotalAntiSweat         = RefrigCase(CaseID)%AntiSweatPower * &
             MIN(1.0d0, MAX(0.0d0,(ZoneDewPoint-TCase)/(RefrigCase(CaseID)%RatedAmbientDewPoint-TCase)))
     TotalAntiSweat         = MAX(RefrigCase(CaseID)%MinimumASPower, TotalAntiSweat)
   CASE (ASHeatBalance)
     IF(RefrigCase(CaseID)%Rcase > 0.0d0) THEN
       TotalAntiSweat = (((ZoneDewPoint-Node(ZoneNodeNum)%Temp)*RefrigCase(CaseID)%Height &
                          /Rair) + &
                         ((ZoneDewPoint-Tcase)*RefrigCase(CaseID)%Height &
                          /RefrigCase(CaseID)%Rcase))
       TotalAntiSweat = MIN(RefrigCase(CaseID)%AntiSweatPower,MAX(RefrigCase(CaseID)%MinimumASPower, &
                                                               TotalAntiSweat))
     ELSE
       TotalAntiSweat = 0.0d0
     END IF
   CASE DEFAULT
   ! should never execute this CASE statement
     TotalAntiSweat   = 0.0d0
  END SELECT
  TotalAntiSweat      = TotalAntiSweat * Length
  TotalASHeaterToCase = RefrigCase(CaseID)%ASHeaterFractionToCase * TotalAntiSweat
  TotalASHeaterToZone = TotalAntiSweat - TotalASHeaterToCase

  ! latent capacity correction term at off-design conditions
  SELECT CASE (RefrigCase(CaseID)%LatentEnergyCurveType)
    CASE (CaseTemperatureMethod)
    LatCapModFrac   = CurveValue(RefrigCase(CaseID)%LatCapCurvePtr,TCase)
    LatentRatio     = MAX(0.0d0,(1.0d0 - (RatedAmbientRH - ZoneRHPercent)*LatCapModFrac))
    CASE (RHCubic)
    LatentRatio     = MAX(0.0d0,CurveValue(RefrigCase(CaseID)%LatCapCurvePtr,ZoneRHPercent))
    CASE ( DPCubic)
    LatentRatio     = MAX(0.0d0,CurveValue(RefrigCase(CaseID)%LatCapCurvePtr,ZoneDewPoint))
  END SELECT

  ! calculate latent case load (assumes no moisture load due to stocking)
  ! assume sensible case credits continue to accumulate in case during defrost/dripdown,
  !    but latent credits/load and capacity only applied outside dripdownschedule
  LatentLoad         = DesignLatentCap * LatentRatio * CaseCreditFraction * ( 1.0d0 - DefrostDripDownSchedule )
  LatentCaseCredit   = -LatentLoad
  ! adjust sensible loads and case credit for actual zone temperature
  ! If zone temp rises above rated ambient temperature, total load can exceed case design capacity,
  ! so unmet cooling loads are accumulated to meet in the next time step. (Case credit fraction allows
  !  extra insulation, e.g. night covers, or imitating a better insulated walk-in cooler)
  ZoneTempFactor     = (Node(ZoneNodeNum)%Temp-Tcase)/(RefrigCase(CaseID)%RatedAmbientTemp-Tcase)
  SensibleLoadPrime  = RefrigCase(CaseID)%DesignSensCaseCredit * ZoneTempFactor * CaseCreditFraction
  SensibleLoadAux    = TotalLightToCase + TotalASHeaterToCase + TotalFan + StockingLoad
  SensibleLoadTotal  = SensibleLoadPrime + SensibleLoadAux
  ! include lighting and anti-sweat power not attributed to case load to sensible case credit
  SensibleCaseCredit = TotalLightToZone + TotalASHeaterToZone - SensibleLoadPrime

  ! FROST:  keep track of frost build up on evaporator coil
  !avoid accumulation during warm-up to avoid reverse dd test problem
  IF(.NOT. WarmUpFlag) THEN
    DeltaFreezeKgFrost = LatentLoad * TimeStepZone * SecInHour/IcetoVaporEnthalpy
    RefrigCase(CaseID)%KgFrost = RefrigCase(CaseID)%KgFrost + DeltaFreezeKgFrost
  END IF

  IF(Tcase > TempTooHotToFrost) RefrigCase(CaseID)%KgFrost = 0.0d0


!DEFROST CALCULATIONS
  IF(DefrostSchedule > 0.0d0) THEN
    IF(DefrostType /= DefNone .AND. DefrostType /= DefOffCycle) THEN
       DefrostCap_Actual = DesignDefrostCap * DefrostSchedule
       IF(DefrostType == DefElectricTerm .OR. DefrostType == DefHotFluidTerm )THEN
              ! calculate correction term for temperature termination defrost control
              SELECT CASE (DefrostEnergyCurveType)
                CASE(CaseTemperatureMethod)
                  DefCapModFrac   = CurveValue(DefCapCurvePtr,TCase)
                  DefrostRatio    = MAX(0.0d0,(1.0d0 - (RatedAmbientRH - ZoneRHPercent)*DefCapModFrac))
                CASE(RHCubic)
                   DefrostRatio    = MAX(0.0d0,CurveValue(DefCapCurvePtr,ZoneRHPercent))
                CASE(DPCubic)
                  DefrostRatio    = MAX(0.0d0,CurveValue(DefCapCurvePtr,ZoneDewPoint))
                CASE(None)
                  DefrostRatio    = 1.0d0
              END SELECT
              DefrostCap_Actual = DefrostCap_Actual * DefrostRatio
       END IF
       StartFrostKg = RefrigCase(CaseID)%KgFrost
       DefrostEnergy = DefrostCap_Actual*TimeStepZone*SecInHour
       FrostMeltedKg = MIN(DefrostEnergy/IceMeltEnthalpy,StartFrostKg)
       RefrigCase(CaseID)%KgFrost = RefrigCase(CaseID)%KgFrost - FrostMeltedKg

       !Reduce defrost heat load on case by amount of ice melted during time step
       !However, don't reduce the defrost capacity applied

       DefrostLoad_Actual = DefrostCap_Actual - FrostMeltedKg*IceMeltEnthalpy/TimeStepZone/SecInHour

      IF(.NOT. WarmUpFlag)THEN   !avoid reverse dd test problems
         ! keep running total of defrost energy above that needed to melt frost for use in evaluating
         !      problems of excessive unmet loads
         RefrigCase(CaseID)%DeltaDefrostEnergy = MAX(0.0D0,(DefrostEnergy -(FrostMeltedKg*IceMeltEnthalpy)))
         RefrigCase(CaseID)%DefrostEnergy = RefrigCase(CaseID)%DefrostEnergy + RefrigCase(CaseID)%DeltaDefrostEnergy
       END IF
       ! If hot brine or hot gas is used for defrost, need to reduce condenser load
       ! Note this condenser credit is not applied in compressor-rack systems.
       IF (DefrostType /= DefElectric .AND. DefrostType /= DefElectricOnDemand .AND. &
              DefrostType /= DefElectricTerm ) RefrigCase(CaseID)%HotDefrostCondCredit=DefrostCap_Actual*DefrostSchedule
    ELSE  !no defrost or off-cycle defrost
       DefrostCap_Actual   = 0.0d0
       DefrostLoad_Actual  = 0.0d0
       RefrigCase(CaseID)%KgFrost = 0.0d0
       ! Off-Cycle defrost is assumed to melt all the ice
    END IF ! defrost type

  ELSE    !DefrostSchedule = 0, so no defrost load or capacity
    DefrostLoad_Actual  = 0.0d0
    DefrostCap_Actual   = 0.0d0
  END IF     !Defrost calculations

!*** See if capacity meets load and manage accumulated stored energy ***********************************
  TotalLoad_Actual   = SensibleLoadTotal + LatentLoad + DefrostLoad_Actual
  StoredEnergyRate   = RefrigCase(CaseID)%StoredEnergy/TimeStepZone/SecInHour
  LoadRequested      = TotalLoad_Actual  + StoredEnergyRate

  ! prorate available cooling capacity for portion of time off due to drip down.
  CapAvail  = DesignRatedCap * (1.0d0 - DefrostDripDownSchedule)
  IF(CapAvail >= LoadRequested) THEN
      !Have more at least as much capacity available as needed, even counting stored energy
      TotalCap_Actual    = LoadRequested
      SensibleCap_Actual = SensibleLoadTotal + StoredEnergyRate
      LatentCap_Actual   = LatentLoad
      RefrigCase(CaseID)%StoredEnergy = 0.0d0
  ELSE
      !Don't have as much capacity as needed (during dripdown or period following dripdown)
      TotalCap_Actual    = CapAvail
      LatentCap_Actual   = MIN(LatentLoad,CapAvail)  !Latent load should never be > capavail, but just in case...
      SensibleCap_Actual = TotalCap_Actual - LatentCap_Actual
      IF(.NOT. WarmUpFlag) RefrigCase(CaseID)%StoredEnergy = RefrigCase(CaseID)%StoredEnergy + &
                                        (TotalLoad_Actual - CapAvail)*TimeStepZone*SecInHour
  END IF !CapAvail vs Load requested

! Reset DefrostLoad_Actual to zero for non-electric defrost types, for reporting purposes
  IF (DefrostType /= DefElectric .AND. DefrostType /= DefElectricOnDemand .AND. &
              DefrostType /= DefElectricTerm ) DefrostCap_Actual = 0.0d0

  CaseRAFraction            = MIN(0.8d0, RefrigCase(CaseID)%RAFrac)
  CaseRAFactor              = (1.0d0 - ((0.8d0 - CaseRAFraction) / 0.8d0)) * 0.5d0

! Update globals for use in ZoneTemperaturePredictorCorrector (Air Heat Balance) and
!   Zone Equipment Manager. Sum case credits to zone and case credits to HVAC

!** this needs a moisture variable NonAirSystemMoistureResponse (equivalent of NonAirSystemResponse) to properly
!** allocate moisture to the zone when the HVAC system is off.

      CaseSenCreditToZone     = SensibleCaseCredit * (1.0d0 - CaseRAFactor)
      CaseLatCreditToZone     = LatentCaseCredit * (1.0d0 - CaseRAFactor)
      CaseSenCreditToHVAC     = SensibleCaseCredit * CaseRAFactor
      CaseLatCreditToHVAC     = LatentCaseCredit * CaseRAFactor

      RefrigCaseCredit(ActualZoneNum)%SenCaseCreditToZone = &
             RefrigCaseCredit(ActualZoneNum)%SenCaseCreditToZone + CaseSenCreditToZone
      RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToZone = &
             RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToZone + CaseLatCreditToZone
      RefrigCaseCredit(ActualZoneNum)%SenCaseCreditToHVAC = &
             RefrigCaseCredit(ActualZoneNum)%SenCaseCreditToHVAC + CaseSenCreditToHVAC
      RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToHVAC = &
             RefrigCaseCredit(ActualZoneNum)%LatCaseCreditToHVAC + CaseLatCreditToHVAC

 ! ReportRefrigeratedCase(CaseID)
  RefrigCase(CaseID)%TotalCoolingLoad         = TotalCap_Actual
  RefrigCase(CaseID)%TotalCoolingEnergy       = TotalCap_Actual * TimeStepZone * SecInHour
  RefrigCase(CaseID)%SensCoolingEnergyRate    = SensibleCap_Actual
  RefrigCase(CaseID)%SensCoolingEnergy        = SensibleCap_Actual * TimeStepZone * SecInHour
  RefrigCase(CaseID)%LatCoolingEnergyRate     = LatentCap_Actual
  RefrigCase(CaseID)%LatCoolingEnergy         = LatentCap_Actual * TimeStepZone * SecInHour

  RefrigCase(CaseID)%SensZoneCreditRate       = CaseSenCreditToZone ! both positive or negative
! This rate can be positive or negative, split into separate output variables and always report positive value
  IF(CaseSenCreditToZone <= 0.0d0) THEN
    RefrigCase(CaseID)%SensZoneCreditCoolRate = -CaseSenCreditToZone
    RefrigCase(CaseID)%SensZoneCreditCool     = -CaseSenCreditToZone * TimeStepZone * SecInHour
    RefrigCase(CaseID)%SensZoneCreditHeatRate = 0.0d0
    RefrigCase(CaseID)%SensZoneCreditHeat     = 0.0d0
  ELSE
    RefrigCase(CaseID)%SensZoneCreditHeatRate = CaseSenCreditToZone
    RefrigCase(CaseID)%SensZoneCreditHeat     = CaseSenCreditToZone * TimeStepZone * SecInHour
    RefrigCase(CaseID)%SensZoneCreditCoolRate = 0.0d0
    RefrigCase(CaseID)%SensZoneCreditCool     = 0.0d0
  END IF

! This rate should always be negative
  RefrigCase(CaseID)%LatZoneCreditRate        = CaseLatCreditToZone
  RefrigCase(CaseID)%LatZoneCredit            = CaseLatCreditToZone * TimeStepZone * SecInHour

  RefrigCase(CaseID)%SensHVACCreditRate       = CaseSenCreditToHVAC
! This rate can be positive or negative, split into separate output variables and always report positive value
  IF(CaseSenCreditToHVAC <= 0.0d0) THEN
    RefrigCase(CaseID)%SensHVACCreditCoolRate = -CaseSenCreditToHVAC
    RefrigCase(CaseID)%SensHVACCreditCool     = -CaseSenCreditToHVAC * TimeStepZone * SecInHour
    RefrigCase(CaseID)%SensHVACCreditHeatRate = 0.0d0
    RefrigCase(CaseID)%SensHVACCreditHeat     = 0.0d0
  ELSE
    RefrigCase(CaseID)%SensHVACCreditHeatRate = CaseSenCreditToHVAC
    RefrigCase(CaseID)%SensHVACCreditHeat     = CaseSenCreditToHVAC * TimeStepZone * SecInHour
    RefrigCase(CaseID)%SensHVACCreditCoolRate = 0.0d0
    RefrigCase(CaseID)%SensHVACCreditCool     = 0.0d0
  END IF

! This rate should always be negative
  RefrigCase(CaseID)%LatHVACCreditRate        = CaseLatCreditToHVAC
  RefrigCase(CaseID)%LatHVACCredit            = CaseLatCreditToHVAC * TimeStepZone * SecInHour

  RefrigCase(CaseID)%ElecFanPower             = TotalFan
  RefrigCase(CaseID)%ElecFanConsumption       = TotalFan * TimeStepZone * SecInHour
  RefrigCase(CaseID)%ElecAntiSweatPower       = TotalAntiSweat
  RefrigCase(CaseID)%ElecAntiSweatConsumption = TotalAntiSweat * TimeStepZone * SecInHour
  RefrigCase(CaseID)%ElecLightingPower        = TotalLightingLoad
  RefrigCase(CaseID)%ElecLightingConsumption  = TotalLightingLoad * TimeStepZone * SecInHour
  RefrigCase(CaseID)%ElecDefrostPower         = DefrostCap_Actual
  RefrigCase(CaseID)%ElecDefrostConsumption   = DefrostCap_Actual * TimeStepZone * SecInHour

  RefrigCase(CaseID)%DefEnergyCurveValue      = DefrostRatio
  RefrigCase(CaseID)%LatEnergyCurveValue      = LatentRatio



!**************************************************************************************************
! Cap Energy and Kg Frost to avoid floating overflow errors
! 1-time warning is issued. It should be rare but could happen with unrealistic inputs.

  !Collect extra sensible load above design for possible warning if that is determining factor in
  ! excessively large stored energy
  IF ((ZoneTempFactor*CaseCreditFraction) > 1.0d0) THEN
    IF(.NOT. WarmUpFlag) THEN
      DeltaWarmEnvEnergy = (SensibleLoadPrime - RefrigCase(CaseID)%DesignSensCaseCredit) * TimeStepZone * SecInHour
      RefrigCase(CaseID)%WarmEnvEnergy = RefrigCase(CaseID)%WarmEnvEnergy + DeltaWarmEnvEnergy
    END IF
  END IF

  IF(RefrigCase(CaseID)%DefrostEnergy > MyLargeNumber)RefrigCase(CaseID)%DefrostEnergy=MyLargeNumber
  IF(RefrigCase(CaseID)%WarmEnvEnergy > MyLargeNumber)RefrigCase(CaseID)%WarmEnvEnergy=MyLargeNumber
  IF(RefrigCase(CaseID)%StockingEnergy > MyLargeNumber)RefrigCase(CaseID)%StockingEnergy=MyLargeNumber
  IF(RefrigCase(CaseID)%StoredEnergy > MyLargeNumber) THEN
      RefrigCase(CaseID)%StoredEnergy=MyLargeNumber
      IF(ShowStoreEnergyWarning(CaseID)) THEN
        CALL ShowWarningError('Refrigeration:Case: '//TRIM(RefrigCase(CaseID)%Name))
        IF(RefrigCase(CaseID)%StockingEnergy >= RefrigCase(CaseID)%DefrostEnergy) THEN
          IF(RefrigCase(CaseID)%StockingEnergy >= RefrigCase(CaseID)%WarmEnvEnergy) THEN
            CALL ShowContinueError(' This case has insufficient capacity to meet excess energy associated'// &
                             ' with stocking.')
            CALL ShowContinueError(' Refer to documentation for further explanation of product stocking requirements and')
            CALL ShowContinueError(' Total Cooling Capacity.')
          ELSE
            CALL ShowContinueError(' This case has insufficient capacity to meet excess energy associated'// &
                             ' with a zone enviroment temperature greater than the design ambient for the case.')
            CALL ShowContinueError(' Refer to documentation for further explanation of ')
            CALL ShowContinueError(' Total Cooling Capacity.')
          END IF ! Stocking energy > warm environment energy
        ELSE
          IF(RefrigCase(CaseID)%DefrostEnergy >= RefrigCase(CaseID)%WarmEnvEnergy) THEN
            CALL ShowContinueError(' This case has insufficient capacity to meet excess energy associated'// &
                             ' with defrost.')
            CALL ShowContinueError(' Refer to documentation for further explanation of defrost control requirements and')
            CALL ShowContinueError(' recommendations regarding Total Cooling Capacity, Sensible Heat Ratio, and Defrost Capacity.')
          ELSE
            CALL ShowContinueError(' This case has insufficient capacity to meet excess energy associated'// &
                             ' with a zone enviroment temperature greater than the design ambient for the case.')
            CALL ShowContinueError(' Refer to documentation for further explanation of ')
            CALL ShowContinueError(' Total Cooling Capacity.')
          END IF ! defrost energy > warm environment energy
        END IF  ! stock > defrost ELSE
        ShowStoreEnergyWarning(CaseID) = .FALSE.  ! only give this warning once for any one case
      END IF  !showstoreenergy warning true
    END IF    ! stored energy > large number

  IF(RefrigCase(CaseID)%KgFrost > MyLargeNumber) THEN
    RefrigCase(CaseID)%KgFrost=MyLargeNumber
    IF(ShowFrostWarning(CaseID)) THEN
      CALL ShowWarningError('Refrigeration:Case: '//TRIM(RefrigCase(CaseID)%Name))
      CALL ShowContinueError(' This case has insufficient defrost capacity to remove the excess frost accumulation.')
      CALL ShowContinueError(' Refer to documentation for further explanation of product stocking requirements and')
      CALL ShowContinueError(' recommendations regarding Total Cooling Capacity, Sensible Heat Ratio, and Latent Heat Ratio.')
      ShowFrostWarning(CaseID) = .FALSE.
    END IF
  END IF

  RETURN

END SUBROUTINE CalculateCase


!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE SimRefrigCondenser(SysType, CompName, CompIndex, FirstHVACIteration, InitLoopEquip )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Randy Hudson, ORNL
          !       DATE WRITTEN   July 2007
          !       MODIFIED       Therese Stovall, ORNL May 2008
          !                      Brent Griffith, NREL Oct 2010, generalize fluid properties
          !                        plant upgrades, moved where called from to SimPlantEquip from ManageNonZoneEquipment
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates the water-cooled refrigeration condenser object.
          ! Modified to add condensers for detailed refrigeration systems and to
          ! avoid double-counting heat rejection that has been used in desuperheater
          ! hvac coils or water heaters.

          ! METHODOLOGY EMPLOYED:
          ! Called from SimPlantEquip in PlantLoopEquipment , previously was called from Non-Zone Equipment Manager
          ! Flow is requested and the actual available flow is set.  The outlet temperature is calculated.

          ! USE STATEMENTS:
  USE PlantUtilities,  ONLY : SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE InputProcessor,  ONLY : FindItemInList
  USE General       ,  ONLY : TrimSigDigits
  USE DataPlant,       ONLY : TypeOf_RefrigSystemWaterCondenser, TypeOf_RefrigerationWaterCoolRack, PlantLoop

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(IN)    :: SysType
  INTEGER, INTENT(INOUT) :: CompIndex
  LOGICAL, INTENT(IN) :: FirstHVACIteration
  LOGICAL, INTENT(IN) :: InitLoopEquip


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: DeltaT = 0.0d0
  REAL(r64) :: InletTemp = 0.0d0
  REAL(r64) :: DesVolFlowRate = 0.0d0
  REAL(r64) :: MassFlowRate = 0.0d0
  REAL(r64) :: MassFlowRateMax = 0.0d0
  REAL(r64) :: OutletTempMax = 0.0d0
  REAL(r64) :: VolFlowRate = 0.0d0
  REAL(r64) :: OutletTemp = 0.0d0
  INTEGER   :: FlowType = 0
  ! INTEGER   :: HighFlowWarn = 0
  ! INTEGER   :: HighTempWarn = 0
  INTEGER   :: NoFlowWarnIndex   = 0
  INTEGER   :: HighFlowWarnIndex = 0
  INTEGER   :: HighInletWarnIndex = 0
  INTEGER   :: HighTempWarnIndex = 0
  CHARACTER(len=MaxNameLength) :: Name = ' '
  CHARACTER(len=MaxNameLength) :: TypeName = ' '
  CHARACTER(len=MaxNameLength) :: ErrIntro = ' '
  INTEGER   :: PlantInletNode
  INTEGER   :: PlantOutletNode
  INTEGER   :: PlantLoopIndex
  INTEGER   :: PlantLoopSideIndex
  INTEGER   :: PlantBranchIndex
  INTEGER   :: PlantCompIndex
  INTEGER   :: Num ! local index
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: Cp  ! local fluid specific heat


  IF (CompIndex == 0) THEN
    SELECT CASE (SysType)
      CASE (TypeOf_RefrigerationWaterCoolRack)
        Num = FindItemInList(CompName, RefrigRack%Name, NumRefrigeratedRacks)

      CASE (TypeOf_RefrigSystemWaterCondenser)
        Num = FindItemInList(CompName, Condenser%Name,  NumRefrigCondensers)
      CASE DEFAULT
        CALL ShowFatalError('SimRefrigCondenser: invalid system type passed')
    END SELECT

    IF (Num == 0) THEN
      CALL ShowFatalError('SimRefrigCondenser: Specified refrigeration condenser not Valid ='//TRIM(CompName))
    ENDIF
    CompIndex = Num
  ELSE
    Num = CompIndex

    SELECT CASE (SysType)
     CASE (TypeOf_RefrigerationWaterCoolRack)
       IF( Num > NumRefrigeratedRacks .OR. Num < 1) THEN
         CALL ShowFatalError('SimRefrigCondenser: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(Num))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumRefrigeratedRacks))//  &
                          ', Entered Unit name='//TRIM(CompName))
       ENDIF
       IF (CheckEquipNameRackWaterCondenser(Num)) THEN
         IF (CompName /= RefrigRack(Num)%Name) THEN
           CALL ShowFatalError('SimRefrigCondenser: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(Num))// &
                          ', Entered Unit name='//TRIM(CompName)//', stored Unit name for that index='// &
                          TRIM(RefrigRack(Num)%Name) )
         ENDIF
         CheckEquipNameRackWaterCondenser(Num) = .FALSE.
       ENDIF

     CASE (TypeOf_RefrigSystemWaterCondenser)
       IF (Num > NumRefrigCondensers .OR. Num < 1) THEN
         CALL ShowFatalError('SimRefrigCondenser: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(Num))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumRefrigCondensers))//  &
                          ', Entered Unit name='//TRIM(CompName))
       ENDIF
       IF (CheckEquipNameWaterCondenser(Num)) THEN
         IF (CompName /= Condenser(Num)%Name) THEN
           CALL ShowFatalError('SimRefrigCondenser: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(Num))// &
                          ', Entered Unit name='//TRIM(CompName)//', stored Unit name for that index='// &
                          TRIM(Condenser(Num)%Name) )
         ENDIF
         CheckEquipNameWaterCondenser(Num) = .FALSE.
       ENDIF

    END SELECT
  ENDIF


! this next block may not be necessary, should only get called from plant now.
!  SELECT CASE (SysType)
!   CASE (TypeOf_RefrigerationWaterCoolRack)
!       IF(RefrigRack(Num)%CondenserType/=RefrigCondenserTypeWater) RETURN
!   CASE (TypeOf_RefrigSystemWaterCondenser)
!       IF(Condenser(Num)%CondenserType/=RefrigCondenserTypeWater) RETURN
!  END SELECT
  ! Return if not water cooled condenser

  IF (InitLoopEquip) THEN
    CALL InitRefrigeration
    CALL InitRefrigerationPlantConnections
    RETURN
  ENDIF

  CALL InitRefrigerationPlantConnections

 !set variables depending upon system type
  SELECT CASE (SysType)
   CASE (TypeOf_RefrigerationWaterCoolRack)
     PlantInletNode     = RefrigRack(Num)%InletNode
     PlantOutletNode    = RefrigRack(Num)%OutletNode
     PlantLoopIndex     = RefrigRack(Num)%PlantLoopNum
     PlantLoopSideIndex = RefrigRack(Num)%PlantLoopSideNum
     PlantBranchIndex   = RefrigRack(Num)%PlantBranchNum
     PlantCompIndex     = RefrigRack(Num)%PlantCompNum

     TotalCondenserHeat = HeatReclaimRefrigeratedRack(Num)%AvailCapacity - &
                          RefrigRack(Num)%LaggedUsedWaterHeater - &
                          RefrigRack(Num)%LaggedUsedHVACCoil
     FlowType = RefrigRack(Num)%FlowType
     InletTemp = RefrigRack(Num)%InletTemp
     ! HighFlowWarn = RefrigRack(Num)%HighFlowWarn
     ! HighTempWarn = RefrigRack(Num)%HighTempWarn
     DesVolFlowRate = RefrigRack(Num)%DesVolFlowRate

     !DSU? init mass flow here?
!     MassFlowRate = RefrigRack(Num)%MassFlowRate
     MassFlowRateMax = RefrigRack(Num)%MassFlowRateMax
     OutletTempMax = RefrigRack(Num)%OutletTempMax
     Name = RefrigRack(Num)%Name
     TypeName = 'Refrigeration:CompressorRack:'
     ErrIntro = 'Condenser for refrigeration rack '
     NoFlowWarnIndex   = RefrigRack(Num)%NoFlowWarnIndex
     HighFlowWarnIndex = RefrigRack(Num)%HighFlowWarnIndex
     HighTempWarnIndex = RefrigRack(Num)%HighTempWarnIndex
     HighInletWarnIndex = RefrigRack(Num)%HighInletWarnIndex
   CASE (TypeOf_RefrigSystemWaterCondenser)
    !InletNode = Condenser(Num)%InletNode
     PlantInletNode     = Condenser(Num)%InletNode
     PlantOutletNode    = Condenser(Num)%OutletNode
     PlantLoopIndex     = Condenser(Num)%PlantLoopNum
     PlantLoopSideIndex = Condenser(Num)%PlantLoopSideNum
     PlantBranchIndex   = Condenser(Num)%PlantBranchNum
     PlantCompIndex     = Condenser(Num)%PlantCompNum

     TotalCondenserHeat = Condenser(Num)%CondLoad
     FlowType = Condenser(Num)%FlowType
     InletTemp = Condenser(Num)%InletTemp
     ! HighFlowWarn = Condenser(Num)%HighFlowWarn
     ! HighTempWarn = Condenser(Num)%HighTempWarn
     DesVolFlowRate = Condenser(Num)%DesVolFlowRate
!     MassFlowRate = Condenser(Num)%MassFlowRate
     MassFlowRateMax = Condenser(Num)%MassFlowRateMax
     OutletTempMax = Condenser(Num)%OutletTempMax
     Name = Condenser(Num)%Name
     TypeName = 'Refrigeration:Condenser:WaterCooled'
     ErrIntro = 'Condenser for refrigeration system '
     NoFlowWarnIndex = Condenser(Num)%NoFlowWarnIndex
     HighFlowWarnIndex = Condenser(Num)%HighFlowWarnIndex
     HighTempWarnIndex = Condenser(Num)%HighTempWarnIndex
     HighInletWarnIndex = Condenser(Num)%HighInletWarnIndex
  END SELECT

  ! Current condenser is water cooled
  ! Make demand request on first HVAC iteration

  !get cooling fluid properties
  rho =GetDensityGlycol(PlantLoop(PlantLoopIndex)%FluidName, &
                        InletTemp, &
                        PlantLoop(PlantLoopIndex)%FluidIndex, &
                        'SimRefrigCondenser')
  Cp = GetSpecificHeatGlycol(PlantLoop(PlantLoopIndex)%FluidName, &
                        InletTemp, &
                        PlantLoop(PlantLoopIndex)%FluidIndex, &
                        'SimRefrigCondenser')

! first determine desired flow
  IF (FlowType == VariableFlow .AND. TotalCondenserHeat > 0.d0 ) THEN
    IF (SysType == TypeOf_RefrigerationWaterCoolRack) THEN
       OutletTemp = GetCurrentScheduleValue(RefrigRack(Num)%OutletTempSchedPtr)
    ELSEIF (SysType == TypeOf_RefrigSystemWaterCondenser) THEN
       OutletTemp = GetCurrentScheduleValue(Condenser(Num)%OutletTempSchedPtr)
    END IF
    IF (OutletTemp == InletTemp) THEN

      IF (HighInletWarnIndex == 0) THEN
        CALL ShowSevereError(ErrIntro//', "'//TRIM(Name)// &
                      '" : has inlet water temp equal to desired outlet temp. Excessive flow resulting. ')
        CALL ShowContinueError('cooling water is not cold enough to reach desired outlet temperature')

      ENDIF
      CALL ShowRecurringWarningErrorAtEnd(ErrIntro//', "'//TRIM(Name)// &
                      '" : has inlet water temp equal to desired outlet temp.... continues. ', &
                       HighInletWarnIndex)
      VolFlowRate = 9999.d0
      MassFlowRate =  VolFlowRate * rho
    ELSE
      DeltaT = OutletTemp - InletTemp
      MassFlowRate = TotalCondenserHeat/Cp/DeltaT
       ! Check for maximum flow in the component
      IF (MassFlowRate > MassFlowRateMax) THEN
          !HighFlowWarn = HighFlowWarn +1
        IF (HighFlowWarnIndex == 0) THEN
          CALL ShowWarningMessage(TypeName//TRIM(Name))
          CALL ShowContinueError('Requested condenser water mass flow rate greater than maximum allowed value. ')
          CALL ShowContinueError('Flow reset to maximum value.')
        END IF !HighFlowWarnIndex
        CALL ShowRecurringWarningErrorAtEnd(ErrIntro// TRIM(Name) // &
            ' - Flow rate higher than maximum allowed ... continues',HighFlowWarnIndex)
          !END IF
        MassFlowRate = MassFlowRateMax
      END IF
    END IF  !compare outlet T to inlet T

  ELSEIF(FlowType == ConstantFlow .AND. TotalCondenserHeat > 0.d0 ) THEN
           ! this part for constant flow condition
    VolFlowRate = DesVolFlowRate
    MassFlowRate =  VolFlowRate * rho

  ELSEIF (TotalCondenserHeat == 0.d0) THEN
    MassFlowRate =  0.d0

  END IF  !on flow type
  ! check against plant, might get changed.
  CALL SetComponentFlowRate(MassFlowRate, &
                             PlantInletNode, PlantOutletNode, &
                             PlantLoopIndex, PlantLoopSideIndex, &
                             PlantBranchIndex, PlantCompIndex )

  VolFlowRate = MassFlowRate / rho

  IF (MassFlowRate > 0) THEN
    OutletTemp = TotalCondenserHeat/(MassFlowRate*Cp) &
        + Node(PlantInletNode)%Temp
  ELSE
    OutletTemp = InletTemp
    IF ((TotalCondenserHeat > 0.0d0) .AND. (.NOT. FirstHVACIteration)) THEN

      CALL ShowRecurringWarningErrorAtEnd(TypeName//TRIM(Name)//&
            'Water-cooled condenser has no cooling water flow. '// &
            'Heat is not being rejected from compressor rack condenser.',NoFlowWarnIndex)
    END IF
  END IF
  ! Check outlet water temp for max value
  IF (OutletTemp > OutletTempMax) THEN
    ! HighTempWarn = HighTempWarn +1
    IF (HighTempWarnIndex == 0) THEN
      CALL ShowWarningMessage(TypeName//TRIM(Name))
      CALL ShowContinueError('Water-cooled condenser outlet temp higher than maximum allowed temp. '// &
            'Check flow rates and/or temperature setpoints.')
    END IF
    CALL ShowRecurringWarningErrorAtEnd(ErrIntro// TRIM(Name) // &
        ' - Condenser outlet temp higher than maximum allowed ... continues',&
            HighTempWarnIndex)
  END IF

 !set up output variables
 SELECT CASE (SysType)
   CASE (TypeOf_RefrigerationWaterCoolRack)
     !RefrigRack(Num)%HighFlowWarn = HighFlowWarn
     !RefrigRack(Num)%HighTempWarn = HighTempWarn
     RefrigRack(Num)%MassFlowRate = MassFlowRate
     RefrigRack(Num)%VolFlowRate = VolFlowRate
     RefrigRack(Num)%OutletTemp = OutletTemp
     RefrigRack(Num)%HighFlowWarnIndex = HighFlowWarnIndex
     RefrigRack(Num)%HighTempWarnIndex = HighTempWarnIndex
     RefrigRack(Num)%HighInletWarnIndex = HighInletWarnIndex
     RefrigRack(Num)%NoFlowWarnIndex = NoFlowWarnIndex
   CASE (TypeOf_RefrigSystemWaterCondenser)
     !Condenser(Num)%HighFlowWarn = HighFlowWarn
     !Condenser(Num)%HighTempWarn = HighTempWarn
     Condenser(Num)%MassFlowRate = MassFlowRate
     Condenser(Num)%VolFlowRate = VolFlowRate
     Condenser(Num)%OutletTemp = OutletTemp
     Condenser(Num)%HighFlowWarnIndex = HighFlowWarnIndex
     Condenser(Num)%HighTempWarnIndex = HighTempWarnIndex
     Condenser(Num)%NoFlowWarnIndex = NoFlowWarnIndex
     Condenser(Num)%HighInletWarnIndex = HighInletWarnIndex
  END SELECT

  CALL UpdateRefrigCondenser(Num,SysType)

  RETURN

END SUBROUTINE SimRefrigCondenser

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE UpdateRefrigCondenser(Num,Systype)

           ! SUBROUTINE INFORMATION:
          !       AUTHOR         Randy Hudson, ORNL
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates the node variables with local variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataLoopNode,   ONLY: Node
  USE PlantUtilities, ONLY: SafeCopyPlantNode
  USE DataPlant,      ONLY: TypeOf_RefrigerationWaterCoolRack, &
                            TypeOf_RefrigSystemWaterCondenser

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Num
  INTEGER, INTENT(IN) :: SysType

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW:
   SELECT CASE (SysType)
     CASE (TypeOf_RefrigerationWaterCoolRack)
       InletNode  = RefrigRack(Num)%InletNode
       OutletNode = RefrigRack(Num)%OutletNode
     CASE (TypeOf_RefrigSystemWaterCondenser)
       InletNode  = Condenser(Num)%InletNode
       OutletNode = Condenser(Num)%OutletNode
   END SELECT

  ! Pass all variables from inlet to outlet node
  CALL SafeCopyPlantNode(InletNode, OutletNode) !  Node(OutletNode) = Node(InletNode)

  ! Set outlet node variables that are possibly changed
   SELECT CASE (SysType)
     CASE (TypeOf_RefrigerationWaterCoolRack)
       Node(OutletNode)%Temp = RefrigRack(Num)%OutletTemp
     CASE (TypeOf_RefrigSystemWaterCondenser)
       Node(OutletNode)%Temp = Condenser(Num)%OutletTemp
   END SELECT

  RETURN

END SUBROUTINE UpdateRefrigCondenser
!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE SimulateDetailedRefrigerationSystems

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL, Assisted by Hugh Henderson
          !       DATE WRITTEN   Spring 2008
          !       Based upon ManageRefrigeratedCaseRacks by Richard Raustad, FSEC
          !          Oct/Nov 2004, and MODIFIED by Shirey, FSEC Dec 2004
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called to simulate detailed refrigeration systems

          ! METHODOLOGY EMPLOYED:
          ! Each refrigeration system is modeled by first simulating the attached refrigerated cases.  The sum
          ! of the total heat transfer for all attached cases determines the load on the compressor rack.
          ! Iterations are used here to account for load transfer between independent refrigeration systems
          ! via mechanical subcoolers.
          ! The logical variable, UseSysTimeStep, determines whether we are evaluating only systems driven by
          ! ZoneEquipmentManager on the system time step, or only system driven by HVACManager on the zone time step.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: SysNum                        ! Index to the detailed refrigerated system being modeled
  LOGICAL     :: DeRate = .FALSE.         ! If true, need to derate aircoils because load can't be met by system
  LOGICAL     :: FirstSCLoop =.TRUE. ! Flag first time through multi-system loop used when mech subcoolers present

  INTEGER     :: StartMechSubcoolLoop = 3       ! if no mechanical subcoolers transfer energy between system,
                                                !    don't loop
  INTEGER     :: LoopNum        = 0             ! Index to overall repeat necessary for mechanical subcoolers
  INTEGER     :: SubcoolID      = 0             ! Subcooler ID number
  INTEGER     :: SubcoolerIndex = 0             ! Subcooler ID number
  INTEGER     :: CaseID         = 0             ! Absolute reference to case
  INTEGER     :: CaseIndex      = 0             ! Index to case
  INTEGER     :: CoilID         = 0             ! Index to a single air chiller/coil
  INTEGER     :: CoilIndex      = 0             ! Index to a single air chiller/coil
  INTEGER     :: CoilSetID      = 0             ! Index to set of coils in a single zone (shared inlet and outlet nodes)
  INTEGER     :: CoilSetIndex   = 0             ! Index to set of coils in a single zone
  INTEGER     :: CondInletAirZoneNum= 0         ! Index used to assign zone credits
  INTEGER     :: SecondID       = 0             ! Absolute reference to Secondary Loop
  INTEGER     :: SecondIndex    = 0             ! Index to Secondary Loop
  INTEGER     :: SuctionPipeActualZoneNum = 0   ! Index to zone exchanging heat with suction pipes
  INTEGER     :: WalkInID       = 0             ! Absolute reference to WalkIn
  INTEGER     :: WalkInIndex    = 0             ! Index to WalkIn
  INTEGER     :: ZoneNum        =0              ! counter when assigning zone case credits
  INTEGER     :: CascadeLoadIndex  = 0          ! Index to Cascade Condenser Load
  INTEGER     :: CascadeLoadID  = 0             ! Absolute reference to Cascade Condenser
  REAL(r64)   :: LoadFrac       = 1.d0          ! case load/design case load
  REAL(r64)   :: LocalTimeStep  = 0.0d0         ! Set equal to either TimeStepSys or TimeStepZone
  REAL(r64)   :: CurrentLoads   = 0.0d0         ! current loads on compressor, exclusive of unmet loads from prev time steps
  REAL(r64)   :: CurrentHiStageLoads = 0.0d0    ! Current loads on high-stage compressor, exclusive of unmet loads from
                                                ! prev time steps (two-stage systems only)
  REAL(r64)   :: MaxTEvap       = 0.0d0         ! Maximum evaporating temperature that can still meet load
  REAL(r64)   :: MaxDelTFloatFrac = 0.5d0       ! max fraction allowed for difference between case and evaporator temperature
                                                !    relative to design temperature difference
  REAL(r64)   :: SuctionPipeZoneTemp            ! Temperature for zone identified as environment for suction pipe heat gains, C

LocalTimeStep = TimeStepZone
IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

!Cascade condenser assumes a constant approach delta T (Tcond - Tevap), not f(load)

!Loads for chiller sets are set in call to zone equipment element "SimAirChillerSet"
! (all chiller coils within a set are located in the same zone)
! (note non-zone, such as refrigeration, and zone equip, such as airchillersets, called at diff times)
! Loads are then defined for each chiller coil within the set in "CalculateAirChillerSet"
! In that subroutine, dispatch coils within each set in order specified for each zone
!  Below will assign loads to refrigeration system or secondary loop
!Note that this routine will go through all refrigeration systems, but loads for multiple systems
! with interactions will not be known for the intitial calls with first HVAC time step. They will,
! however, be repeated when the last chiller set is called from ZoneEquipmentManager
! that's why important where init goes, don't want to zero out data should keep
  IF(UseSysTimeStep) THEN
    DO CoilSetIndex=1,NumRefrigChillerSets
     CoilSetID = CoilSetIndex
     CALL CalculateAirChillerSets(CoilSetID)
    END DO
  END IF

!Do refrigeration system loop outside of iterative solution to initialize time step and
!  calculate case, walk-in, and secondary loop loads (that won't change during balance
!  of refrigeration system iterations) and prepare initial estimates for the iterative system solution
  DO SysNum = 1, NumRefrigSystems
  !Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
  IF(((.NOT. UseSysTimeStep).AND.(.NOT. System(SysNum)%CoilFlag)).OR.((UseSysTimeStep).AND.(System(SysNum)%CoilFlag)))THEN
    IF(System(SysNum)%NumCases > 0) THEN
    DO CaseIndex=1,System(SysNum)%NumCases
       CaseID=System(Sysnum)%Casenum(CaseIndex)
       CALL CalculateCase(CaseID)
       !  TevapDesign calc in Get Input to meet lowest evap temp of any load on the system.
       !  Tevap needed is either fixed at this design value,
       !  or allowed to float to meet lowest T needed among all loads served by the system
       !  (Floating Tevap = Design Tevap unless load <= Design cap)
       IF (System(SysNum)%CompSuctControl == ConstantSuctionTemperature) THEN
         System(Sysnum)%TEvapNeeded=System(SysNum)%TEvapDesign
       ELSE  ! calculate floating T evap
         LoadFrac = Min(1.d0,(RefrigCase(CaseID)%TotalCoolingLoad/RefrigCase(CaseID)%DesignRatedCap))
         MaxTEvap = RefrigCase(CaseID)%Temperature - &
                (RefrigCase(CaseID)%Temperature - RefrigCase(CaseID)%EvapTempDesign)*max(LoadFrac,MaxDelTFloatFrac)
         !Compare Tevap for this case to max allowed for all previous cases on this suction group and set at the MINIMUM of the two
         IF (CaseIndex == 1)THEN  !note use case index, not caseid here to get first case on this suction group/system
           System(Sysnum)%TEvapNeeded=MaxTEvap
         ELSE
           System(Sysnum)%TEvapNeeded=MIN(MaxTEvap,System(Sysnum)%TEvapNeeded)
         END IF
       END IF  !floating or constant evap temperature
       ! increment TotalCoolingLoad for Compressors/condenser on each system and defrost condenser credits for heat recovery
       System(SysNum)%TotalCoolingLoad = System(SysNum)%TotalCoolingLoad + RefrigCase(CaseID)%TotalCoolingLoad
       System(SysNum)%TotalCondDefrostCredit=System(SysNum)%TotalCondDefrostCredit + RefrigCase(CaseID)%HotDefrostCondCredit
    END DO !NumCases
    END IF  !Num of cases > 0

    IF(System(SysNum)%NumWalkIns > 0) THEN
    DO WalkInIndex=1,System(SysNum)%NumWalkIns
       WalkInID=System(Sysnum)%WalkInNum(WalkInIndex)
       CALL CalculateWalkIn(WalkInID)
       IF (System(SysNum)%CompSuctControl == ConstantSuctionTemperature) THEN
         System(Sysnum)%TEvapNeeded=System(SysNum)%TEvapDesign
       ELSE  ! calculate floating T evap
         LoadFrac = Min(1.d0,(WalkIn(WalkInID)%TotalCoolingLoad/WalkIn(WalkInID)%DesignRatedCap))
         MaxTEvap = WalkIn(WalkInID)%Temperature - &
                (WalkIn(WalkInID)%Temperature - WalkIn(WalkInID)%TEvapDesign)*max(LoadFrac,MaxDelTFloatFrac)
         !  Compare maxTevap for this walk in to max allowed for cases and for all
         !  previous walk ins on this suction group and set at the MINIMUM of the two
         IF (WalkInIndex == 1 .AND. System(SysNum)%NumCases ==0  )THEN
           System(Sysnum)%TEvapNeeded=MaxTEvap
         ELSE
           System(Sysnum)%TEvapNeeded=MIN(MaxTEvap,System(Sysnum)%TEvapNeeded)
         END IF
       END IF  !floating or constant evap temperature
       ! increment TotalCoolingLoad for Compressors/condenser on each system
       System(SysNum)%TotalCoolingLoad = System(SysNum)%TotalCoolingLoad + WalkIn(WalkInID)%TotalCoolingLoad
       System(SysNum)%TotalCondDefrostCredit=System(SysNum)%TotalCondDefrostCredit + WalkIn(WalkInID)%HotDefrostCondCredit
    END DO !NumWalkIns systems
    END IF !System(SysNum)%NumWalkIns > 0

    IF(System(SysNum)%NumCoils > 0) THEN
    DO CoilIndex=1,System(SysNum)%NumCoils
       CoilID=System(Sysnum)%CoilNum(CoilIndex)
       ! already CALLed CalculateCoil(CoilID) in CoilSet specified order
       IF (System(SysNum)%CompSuctControl == ConstantSuctionTemperature) THEN
         System(Sysnum)%TEvapNeeded=System(SysNum)%TEvapDesign
       ELSE  ! calculate floating T evap
         ! for now, override floating Tevap if coils on system, warning was printed in input to let user know
         System(Sysnum)%TEvapNeeded=System(SysNum)%TEvapDesign
       END IF  !floating or constant evap temperature
       ! increment TotalCoolingLoad for Compressors/condenser on each system
       System(SysNum)%TotalCoolingLoad = System(SysNum)%TotalCoolingLoad + WarehouseCoil(CoilID)%TotalCoolingLoad
       System(SysNum)%TotalCondDefrostCredit=System(SysNum)%TotalCondDefrostCredit + WarehouseCoil(CoilID)%HotDefrostCondCredit
    END DO !NumCoils systems
    END IF !System(SysNum)%NumCoils > 0

   IF(System(SysNum)%NumSecondarys > 0) THEN
    DO SecondIndex=1,System(SysNum)%NumSecondarys
       SecondID=System(Sysnum)%Secondarynum(SecondIndex)
       CALL CalculateSecondary(SecondID)
       IF (System(SysNum)%CompSuctControl == ConstantSuctionTemperature) THEN
         System(Sysnum)%TEvapNeeded=System(SysNum)%TEvapDesign
       ELSE  ! check for lowest T evap design among the secondary systems and
             !  Compare Tevap for this second to max allowed for cases, walk ins, and
             !  for all previous secondary loops on this suction group and set
             !  at the MINIMUM (note secondary loops control capacity with
             !  brine flow rate, so don't float above their design evap temperature)
         IF (SecondIndex == 1 .AND. System(SysNum)%NumNonCascadeLoads ==0 )  THEN
           System(Sysnum)%TEvapNeeded=Secondary(SecondID)%TEvapDesign
         ELSE
           System(Sysnum)%TEvapNeeded=MIN(Secondary(SecondID)%TEvapDesign,System(Sysnum)%TEvapNeeded)
         END IF
       END IF  !floating or constant evap temperature
       ! increment TotalCoolingLoad for Compressors/condenser on each system
       System(SysNum)%SumSecondaryLoopLoad = System(SysNum)%SumSecondaryLoopLoad + Secondary(SecondID)%TotalCoolingLoad
       System(SysNum)%TotalCondDefrostCredit=System(SysNum)%TotalCondDefrostCredit + Secondary(SecondID)%HotDefrostCondCredit
    END DO !NumSecondarys systems
    END IF !System(SysNum)%NumSecondarys > 0


    !add suction pipe heat gains (W) if input by user
    !Suction pipe heat gains aren't included in the reported total system load, but are heat gains that must be met in
    !  condenser and compressor loads. However, secondary dist piping and receiver gains are included
    !  in the total secondary system loads.
    System(SysNum)%PipeHeatLoad = 0.d0
    IF(System(SysNum)%SumUASuctionPiping > mysmallnumber) THEN
      SuctionPipeZoneTemp = Node(System(SysNum)%SuctionPipeZoneNodeNum)%Temp
      System(SysNum)%PipeHeatLoad = System(SysNum)%SumUASuctionPiping * &
                      (SuctionPipeZoneTemp - System(Sysnum)%TEvapNeeded)
      ! pipe heat load is a positive number (ie. heat absorbed by pipe, so needs to be subtracted
      !     from refrigcasecredit (- for cooling zone, + for heating zone)
      SuctionPipeActualZoneNum = System(SysNum)%SuctionPipeActualZoneNum
      IF(UseSysTimeStep) THEN
        CoilSysCredit(SuctionPipeActualZoneNum)%SenCreditToZoneRate = &
            CoilSysCredit(SuctionPipeActualZoneNum)%SenCreditToZoneRate -  System(SysNum)%PipeHeatLoad
        CoilSysCredit(SuctionPipeActualZoneNum)%ReportSenCoolingToZoneRate = &
            - CoilSysCredit(SuctionPipeActualZoneNum)%SenCreditToZoneRate
      END IF
      !Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .false.
      IF((.NOT. UseSysTimeStep).AND.((NumSimulationCases > 0).OR.( NumSimulationWalkIns > 0)))THEN
        RefrigCaseCredit(SuctionPipeActualZoneNum)%SenCaseCreditToZone = &
            RefrigCaseCredit(SuctionPipeActualZoneNum)%SenCaseCreditToZone - System(SysNum)%PipeHeatLoad
      END IF !UseSysTimeStep
   END IF
  END IF  !(((.NOT. UseSysTimeStep).AND.(.NOT. System(SysNum)%CoilFlag)).OR.((UseSysTimeStep).AND.(System(SysNum)%CoilFlag)))
  END DO  ! SysNum

! Need to know if mechanical subcoolers or cascade condensers or shared condensers
!    are present. If so, energy transfer between
!    detailed refrigeration systems requires additional iteration at this level.

  StartMechSubcoolLoop=3
  If ((NumSimulationMechSubcoolers > 0) .OR. (NumSimulationCascadeCondensers > 0) &
                                        .OR. (NumSimulationSharedCondensers  > 0) &
                                        .OR. (NumSimulationRefrigAirChillers > 0)) StartMechSubcoolLoop=1

  FirstSCLoop=.TRUE.
  Do Loopnum= StartMechSubcoolLoop,3  !Note, for cascade cond loads compared requiring 5 iterations to 3, no difference.

    DO SysNum = 1, NumRefrigSystems
    !Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
    IF(((.NOT. UseSysTimeStep).AND.(.NOT. System(SysNum)%CoilFlag)).OR.((UseSysTimeStep).AND.(System(SysNum)%CoilFlag))) THEN
      System(SysNum)%SumMechSCLoad       =0.d0
      System(SysNum)%SumCascadeLoad      =0.d0
      System(SysNum)%SumCascadeCondCredit=0.d0
      System(SysNum)%SumMechSCBenefit    =0.d0

      IF((NumSimulationMechSubcoolers > 0).AND. (.NOT. FirstSCLoop)) THEN
        !This loop places load on system providing mechanical subcooling
        DO SubCoolID = 1,NumSimulationSubcoolers
          IF(Subcooler(SubcoolID)%SubcoolerType == LiquidSuction)CYCLE
          IF(Subcooler(SubcoolID)%MechSourceSysID /= SysNum)CYCLE
            !don't have summechscload until second subcooler pass, set to zero on first pass
            System(SysNum)%SumMechSCLoad=System(SysNum)%SumMechSCLoad+System(SysNum)%MechSCLoad(SubCoolID)
            !subcooler should not drive Tevap for supplying system,
            !    but check to see if T controlled can be met or if Tevap is at a higher temperature
          IF(Subcooler(SubcoolID)%MechControlTliqOut < System(SysNum)%TEvapNeeded) THEN
            CALL ShowWarningError('Refrigeration:System: '//TRIM(System(SysNum)%Name))
            CALL ShowContinueError(' Evaporating temperature greater than the controlled ')
            CALL ShowContinueError(' liquid outlet temperature for SUBCOOLER:'//TRIM(Subcooler(SubcoolID)%Name))
          END IF
        END DO !SubCoolId

        IF (System(SysNum)%NumSubcoolers > 0) THEN
          DO SubcoolerIndex=1,System(SysNum)%NumSubcoolers
             SubcoolID=System(Sysnum)%Subcoolernum(SubcoolerIndex)
             IF(Subcooler(SubcoolID)%SubcoolerType == LiquidSuction)CYCLE
             System(SysNum)%SumMechSCBenefit = Subcooler(SubcoolID)%MechSCTransLoad
          END DO !subcoolerindex
        END IF ! system(sysid)%numsubcoolers > 0
      END IF   !NumSimulationMechSubcoolers > 0 and not first loop

      !This loop places load on system absorbing heat from cascade condenser and &
      !     condenser heat reclaim credits from hot gas/brine defrosts
      IF((System(SysNum)%NumCascadeLoads > 0).AND. (.NOT. FirstSCLoop)) THEN
          DO CascadeLoadIndex=1,System(SysNum)%NumCascadeLoads
            CascadeLoadID=System(Sysnum)%CascadeLoadNum(CascadeLoadIndex)
            IF (System(SysNum)%CompSuctControl == ConstantSuctionTemperature) THEN
                     System(Sysnum)%TEvapNeeded=System(SysNum)%TEvapDesign
            ELSE  ! check for lowest T evap design among the CascadeLoad systems and
             !  Compare Tevap for this Cascade to max allowed for cases, walk ins, and
             !  for all previous CascadeLoad loops on this suction group and set
             !  at the MINIMUM
               IF(Condenser(CascadeLoadID)%CascadeTempControl == CascadeTempSet) THEN
                 !if float then set tevap based upon other loads
                 IF (CascadeLoadIndex == 1 .AND. System(SysNum)%NumNonCascadeLoads == 0 )THEN
                   System(Sysnum)%TEvapNeeded=Condenser(CascadeLoadID)%CascadeRatedEvapTemp
                 ELSE
                   System(Sysnum)%TEvapNeeded=MIN(Condenser(CascadeLoadID)%CascadeRatedEvapTemp,System(Sysnum)%TEvapNeeded)
                 END IF
               END IF
            END IF  !floating or constant system evap temperature
          ! increment Cascade condenser Loads for Compressors/condenser on each system
          ! place any defrost credits on the same system absorbing the cascade condenser load
          ! (CascadeSysID identifies the condenser producing the defrost credits, that is, the lower temp system)
          System(SysNum)%SumCascadeLoad = System(SysNum)%SumCascadeLoad + Condenser(CascadeLoadID)%CondLoad
          System(SysNum)%SumCascadeCondCredit = System(SysNum)%SumCascadeCondCredit  + &
                                              System(Condenser(CascadeLoadID)%CascadeSysID)%TotalCondDefrostCredit

          END DO !NumCascadeLoads
        END IF !System(SysNum)%NumCascadeLoads > 0

      !only calc detailed system if have load (could be zero first time through if only load is cascade condenser)
      System(SysNum)%TotalSystemLoad = System(SysNum)%TotalCoolingLoad + System(SysNum)%SumSecondaryLoopLoad + &
                        System(SysNum)%SumMechSCLoad + System(SysNum)%SumCascadeLoad
      IF (System(SysNum)%TotalSystemLoad > 0.d0)  THEN
         System(SysNum)%CpSatVapEvap = GetSatSpecificHeatRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TEvapNeeded,&
                                                           1.0d0,System(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
         System(SysNum)%HCaseOut=GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(Sysnum)%TEvapNeeded, &
                                               1.0d0,System(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')  +  &
                                               System(SysNum)%CpSatVapEvap*CaseSuperheat
         !Establish estimates to start solution loop
         SELECT CASE (Condenser(System(SysNum)%CondenserNum(1))%CondenserType) !only one condenser allowed now
           CASE (RefrigCondenserTypeAir)
           System(SysNum)%TCondense = OutDryBulbTemp + 16.7d0
           !16.7C is delta T at rating point for air-cooled condensers, just estimate, so ok for zone-located condensers
           CASE (RefrigCondenserTypeEvap)
           System(SysNum)%TCondense = OutDryBulbTemp + 15.0d0
           !15C is delta T at rating point for evap-cooled condensers
           CASE (RefrigCondenserTypeWater)
           !define starting estimate at temperature of water exiting condenser
           System(SysNum)%TCondense = Node(Condenser(System(SysNum)%CondenserNum(1))%OutletNode)%Temp
           CASE (RefrigCondenserTypeCascade)
           !?Don't need estimate for cascade condenser because it doesn't iterate?
        END SELECT

       !Produce first time step estimates, assume no subcoolers
       System(SysNum)%HSatLiqCond=GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,0.0d0,&
                                                System(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
       System(SysNum)%CpSatLiqCond = GetSatSpecificHeatRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,&
                                                           0.0d0,System(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
       System(SysNum)%HCaseIn = System(SysNum)%HSatLiqCond - System(SysNum)%CpSatLiqCond* &
                             Condenser(System(SysNum)%CondenserNum(1))%RatedSubcool
       System(SysNum)%RefMassFlowtoLoads=System(SysNum)%TotalSystemLoad/(System(SysNum)%HCaseOut-System(SysNum)%HCaseIn)
       System(SysNum)%RefMassFlowComps=System(SysNum)%RefMassFlowtoLoads

       IF(System(SysNum)%NumStages==2) THEN  ! Two-stage compression system
         ! Initial guess for high-stage mass flow rate in two-stage compression systems
         System(SysNum)%RefMassFlowHiStageComps=System(SysNum)%RefMassFlowComps/0.65
       END IF

       CALL CalcDetailedSystem(SysNum)
       DeRate = .FALSE.

       !With air chiller coils, don't use unmet energy, instead reduce capacity on coils to match avail compressor/cond capacity
       CurrentLoads = System(SysNum)%TotalSystemLoad + &
                   System(SysNum)%LSHXTrans  !because compressor capacity rated from txv to comp inlet
       IF((System(SysNum)%CoilFlag).AND.(CurrentLoads > (System(SysNum)%TotCompCapacity*1.001d0))) THEN
         DeRate = .TRUE.
         CALL FinalRateCoils(Derate,DetailedSystem,SysNum,CurrentLoads, &
                             System(SysNum)%TotCompCapacity)
         System(SysNum)%TotalCoolingLoad       = 0.d0
         System(SysNum)%TotalCondDefrostCredit = 0.d0
         DO CoilIndex=1,System(SysNum)%NumCoils
           CoilID=System(Sysnum)%CoilNum(CoilIndex)
           ! already CALLed CalculateCoil(CoilID) in CoilSet specified order
           IF (System(SysNum)%CompSuctControl == ConstantSuctionTemperature) THEN
             System(Sysnum)%TEvapNeeded=System(SysNum)%TEvapDesign
           ELSE  ! calculate floating T evap
             System(Sysnum)%TEvapNeeded=System(SysNum)%TEvapDesign
             CALL ShowWarningError('Refrigeration:System: '//TRIM(System(SysNum)%Name))
             CALL ShowContinueError(' Floating evaporator temperature model not yet available for warehouse coil systems. ')
           END IF  !floating or constant evap temperature
           ! increment TotalCoolingLoad for Compressors/condenser on each system
           System(SysNum)%TotalCoolingLoad = System(SysNum)%TotalCoolingLoad + WarehouseCoil(CoilID)%TotalCoolingLoad
           System(SysNum)%TotalCondDefrostCredit=System(SysNum)%TotalCondDefrostCredit + WarehouseCoil(CoilID)%HotDefrostCondCredit
         END DO !NumCoils systems
         IF(System(SysNum)%NumStages==2 .AND.&
            System(SysNum)%TotHiStageCompCapacity < (System(SysNum)%TotalCoolingLoad + System(SysNum)%LSHXTrans + &
            System(SysNum)%TotCompPower))THEN
               CALL ShowRecurringWarningErrorAtEnd('Refrigeration:System: '//TRIM(System(SysNum)%Name)//&
                    ':The specified high-stage compressors for this system are unable to meet '// &
                    ' the sum of the refrigeration loads, ', System(SysNum)%HiStageWarnIndex1)
               CALL ShowRecurringContinueErrorAtEnd(' subcooler loads (if any), and low-stage compressor loads for this sytem.',&
                    System(SysNum)%HiStageWarnIndex2)
         END IF  !Hi-stage capacity<(load+LSHX load + lo-stage compressor load)
       END IF !CoilFlag (Numcoils > 0) and load > capacity

     END IF  !System(SysNum)%TotalSystemLoad > 0
  END IF  !(((.NOT. UseSysTimeStep).AND.(.NOT. System(SysNum)%CoilFlag)).OR.((UseSysTimeStep).AND.(System(SysNum)%CoilFlag)))
  END DO  !Sysnum over NumRefrigSystems
    FirstSCLoop=.FALSE.
  END DO    !Loopnum, three times for buildings with multiple detailed systems connected with mechanical subcoolers
            ! or cascade condensers or shared condensers or warehouse coils that might need to be de-rated

 ! Dealing with unmet load has to be done outside iterative loop
 DO SysNum = 1, NumRefrigSystems
  !Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
  IF((((.NOT. UseSysTimeStep).AND.(.NOT. System(SysNum)%CoilFlag)).OR.((UseSysTimeStep).AND.(System(SysNum)%CoilFlag))).AND. &
  (.NOT. WarmUpFlag)) THEN
    CurrentLoads = System(SysNum)%TotalSystemLoad + &
                   System(SysNum)%LSHXTrans  !because compressor capacity rated from txv to comp inlet
    IF(System(SysNum)%NumStages==2) THEN
      CurrentHiStageLoads = CurrentLoads + System(SysNum)%TotCompPower
    END IF  ! NumStages==2
   IF(System(SysNum)%CoilFlag) THEN
     ! don't use 'unmet energy' with air chillers, see 'derate'
     System(SysNum)%UnmetEnergy=0.D0
     System(SysNum)%UnmetHiStageEnergy=0.0d0
   ELSE
     ! Meeting current and possibly some portion of the previously unmet energy
     ! perhaps future interest in reporting percent of installed capacity used(or number of compressors) ?
     ! If the system compressors were unable to meet the current loads, save energy to be met in succeeding time step
     ! Note the unmet energy is turned into a rate and applied to the system load at the start of calccompressor
     System(SysNum)%UnmetEnergy=System(SysNum)%UnmetEnergy + (CurrentLoads - System(SysNum)%TotCompCapacity)* &
                 TimeStepZone*SecInHour
     IF(System(SysNum)%NumStages==2) THEN
       System(SysNum)%UnmetHiStageEnergy=System(SysNum)%UnmetHiStageEnergy + (CurrentHiStageLoads - &
                                         System(SysNum)%TotHiStageCompCapacity)*TimeStepZone*SecInHour
     END IF
     IF(System(SysNum)%UnmetEnergy > MyLargeNumber)THEN
        System(SysNum)%UnmetEnergy = MyLargeNumber
        IF(ShowUnmetEnergyWarning(SysNum))THEN
          CALL ShowWarningError('Refrigeration:System: '//TRIM(System(SysNum)%Name))
          CALL ShowContinueError(' The specified compressors for this system are unable to meet ')
          CALL ShowContinueError(' the sum of the refrigerated case loads and subcooler loads (if any) for this sytem.')
          ShowUnmetEnergyWarning(SysNum) = .FALSE.
        END IF  !show warning
     END IF    ! > mylarge number
     IF(System(SysNum)%UnmetHiStageEnergy > MyLargeNumber)THEN
        System(SysNum)%UnmetHiStageEnergy = MyLargeNumber
        IF(ShowHiStageUnmetEnergyWarning(SysNum))THEN
          CALL ShowWarningError('Refrigeration:System: '//TRIM(System(SysNum)%Name))
          CALL ShowContinueError(' The specified high-stage compressors for this system are unable to meet ')
          CALL ShowContinueError(' the sum of the refrigerated case loads, subcooler loads (if any) and ')
          CALL ShowContinueError(' low-stage compressor loads for this sytem.')
          ShowHiStageUnmetEnergyWarning(SysNum) = .FALSE.
        END IF  !show warning
     END IF    ! > mylarge number
   END IF   ! numcoils > 0

   !Zone-located air-cooled condenser reject heat also has to be outside iterative loop
     IF (System(SysNum)%SystemRejectHeatToZone)THEN
       CondInletAirZoneNum = Condenser(System(SysNum)%CondenserNum(1))%InletAirZoneNum
       IF(UseSysTimeStep) THEN
         CoilSysCredit(CondInletAirZoneNum)%SenCreditToZoneRate = &
           CoilSysCredit(CondInletAirZoneNum)%SenCreditToZoneRate + &
           System(SysNum)%NetHeatRejectLoad    !Adding heat is positive
         CoilSysCredit(CondInletAirZoneNum)%ReportSenCoolingToZoneRate = &
           - CoilSysCredit(CondInletAirZoneNum)%SenCreditToZoneRate
       END IF
       !Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .false.
       IF((.NOT. UseSysTimeStep).AND.((NumSimulationCases > 0).OR.( NumSimulationWalkIns > 0)))THEN
         RefrigCaseCredit(CondInletAirZoneNum)%SenCaseCreditToZone = &
           RefrigCaseCredit(CondInletAirZoneNum)%SenCaseCreditToZone + &
           System(SysNum)%NetHeatRejectLoad    !Adding heat is positive
       END IF !UseSystimestep
     END IF !Reject heat to zone

    ! Report variables
    System(SysNum)%TotTransferLoad = System(SysNum)%SumMechSCLoad - System(SysNum)%SumMechSCBenefit &
                                  + System(SysNum)%SumSecondaryLoopLoad + System(SysNum)%SumCascadeLoad
    System(SysNum)%TotTransferEnergy  = System(SysNum)%TotTransferLoad  * LocalTimeStep * SecInHour
    System(SysNum)%PipeHeatEnergy     = System(SysNum)%PipeHeatLoad     * LocalTimeStep * SecInHour
    System(SysNum)%TotalCoolingEnergy = System(SysNum)%TotalCoolingLoad * LocalTimeStep * SecInHour
   END IF  !(((.NOT. UseSysTimeStep).AND.(.NOT. System(SysNum)%CoilFlag)).OR.((UseSysTimeStep).AND.(System(SysNum)%CoilFlag))).and.not warmupflag
 END DO     ! Sysnum = 1,NumRefrigSystems

! Update for sending to zone equipment manager. (note report variables are summed elsewhere)
!   LatOutputProvided = CoilSysCredit(ZoneNum)%LatKgPerS_ToZoneRate
!   SysOutputProvided = CoilSysCredit(ZoneNum)%SenCreditToZoneRate
! Note that case credit is negative for cooling, thus subtract positive value calculated for coil
!   Note this is done whether or not the coils are derated.
  IF(UseSysTimeStep) THEN
    DO ZoneNum = 1,NumOfZones
      DO CoilID = 1,NumSimulationRefrigAirChillers
        IF(WarehouseCoil(CoilID)%ZoneNum /= ZoneNum)CYCLE
        CoilSysCredit(ZoneNum)%SenCreditToZoneRate  = CoilSysCredit(ZoneNum)%SenCreditToZoneRate - &
                                                      WarehouseCoil(CoilID)%SensCreditRate
        CoilSysCredit(ZoneNum)%SenCreditToZoneEnergy = CoilSysCredit(ZoneNum)%SenCreditToZoneRate &
                                                      * LocalTimeStep * SecInHour
        CoilSysCredit(ZoneNum)%LatKgPerS_ToZoneRate =   CoilSysCredit(ZoneNum)%LatKgPerS_ToZoneRate - &
                                                      WarehouseCoil(CoilID)%LatKgPerS_ToZone
        CoilSysCredit(ZoneNum)%LatCreditToZoneRate   = CoilSysCredit(ZoneNum)%LatCreditToZoneRate - &
                                                       WarehouseCoil(CoilID)%LatCreditRate
        CoilSysCredit(ZoneNum)%LatCreditToZoneEnergy = CoilSysCredit(ZoneNum)%LatCreditToZoneEnergy - &
                                                       WarehouseCoil(CoilID)%LatCreditEnergy
      END DO
    END DO
  END IF

 CALL SumZoneImpacts

 RETURN

END SUBROUTINE SimulateDetailedRefrigerationSystems

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE SimulateDetailedTransRefrigSystems

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brian A. Fricke, ORNL
          !       DATE WRITTEN   Fall 2011
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called to simulate detailed transcritical CO2 refrigeration systems

          ! METHODOLOGY EMPLOYED:
          ! Each refrigeration system is modeled by first simulating the attached refrigerated cases and
          ! walk-ins. The sum of the total heat transfer for all attached cases and walk-ins determines
          ! the load on the compressors. Iterations are used here to account for sharing of gas coolers
          ! between independent refrigeration systems.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: SysNum                         ! Index to the detailed transcritical refrigeration system being modeled
  LOGICAL     :: FirstSCLoop =.TRUE.            ! Flag first time through multi-system loop used when mech subcoolers present
  INTEGER     :: StartMechSubcoolLoop = 3       ! if no mechanical subcoolers transfer energy between system, don't loop
  INTEGER     :: LoopNum        = 0             ! Index to overall repeat necessary for mechanical subcoolers
  INTEGER     :: CaseID         = 0             ! Absolute reference to case
  INTEGER     :: CaseIndex      = 0             ! Index to case
  INTEGER     :: CondInletAirZoneNum= 0         ! Index used to assign zone credits
  INTEGER     :: SuctionPipeActualZoneNum = 0   ! Index to zone exchanging heat with suction pipes
  INTEGER     :: WalkInID       = 0             ! Absolute reference to WalkIn
  INTEGER     :: WalkInIndex    = 0             ! Index to WalkIn
  REAL(r64)   :: LocalTimeStep  = 0.0d0         ! Set equal to either TimeStepSys or TimeStepZone
  REAL(r64)   :: CurrentLoads   = 0.0d0         ! current loads on compressor, exclusive of unmet loads from prev time steps
  REAL(r64)   :: SuctionPipeZoneTemp            ! Temperature for zone identified as environment for suction pipe heat gains, C


LocalTimeStep = TimeStepZone
IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

!  Do transcritical CO2 refrigeration system loop outside of iterative solution to initialize time step and
!  calculate case and and walk-ins (that won't change during balance of refrigeration system iterations)
!  and prepare initial estimates for the iterative system solution

!  TransCritSysFlag = .TRUE.
  DO SysNum = 1, NumTransRefrigSystems
  !Only do those systems appropriate for this analysis, supermarket type on load time step
    IF(TransSystem(SysNum)%NumCasesMT > 0) THEN
      DO CaseIndex=1,TransSystem(SysNum)%NumCasesMT
         CaseID=TransSystem(Sysnum)%CasenumMT(CaseIndex)
         CALL CalculateCase(CaseID)
         !  TEvapDesignMT calc in Get Input to meet lowest evap temp of any MT load on the system.
         !  TEvapNeededMT is fixed at this design value.
         TransSystem(Sysnum)%TEvapNeededMT=TransSystem(SysNum)%TEvapDesignMT
         ! increment TotalCoolingLoad for Compressors/gas cooler on each system and defrost gas cooler credits for heat recovery
         TransSystem(SysNum)%TotalCoolingLoadMT = TransSystem(SysNum)%TotalCoolingLoadMT + RefrigCase(CaseID)%TotalCoolingLoad
         TransSystem(SysNum)%TotalCondDefrostCredit=TransSystem(SysNum)%TotalCondDefrostCredit +   &
            RefrigCase(CaseID)%HotDefrostCondCredit
      END DO !NumCasesMT
    END IF  !Num of MT cases > 0

    IF(TransSystem(SysNum)%NumCasesLT > 0) THEN
      DO CaseIndex=1,TransSystem(SysNum)%NumCasesLT
         CaseID=TransSystem(Sysnum)%CasenumLT(CaseIndex)
         CALL CalculateCase(CaseID)
         !  TEvapDesignLT calc in Get Input to meet lowest evap temp of any LT load on the system.
         !  TEvapNeededLT is fixed at this design value.
         TransSystem(Sysnum)%TEvapNeededLT=TransSystem(SysNum)%TEvapDesignLT
         ! increment TotalCoolingLoad for Compressors/gas cooler on each system and defrost gas cooler credits for heat recovery
         TransSystem(SysNum)%TotalCoolingLoadLT = TransSystem(SysNum)%TotalCoolingLoadLT + RefrigCase(CaseID)%TotalCoolingLoad
         TransSystem(SysNum)%TotalCondDefrostCredit=TransSystem(SysNum)%TotalCondDefrostCredit +   &
            RefrigCase(CaseID)%HotDefrostCondCredit
      END DO !NumCasesLT
    END IF  !Num of LT cases > 0

    IF(TransSystem(SysNum)%NumWalkInsMT > 0) THEN
      DO WalkInIndex=1,TransSystem(SysNum)%NumWalkInsMT
         WalkInID=TransSystem(Sysnum)%WalkInNumMT(WalkInIndex)
         CALL CalculateWalkIn(WalkInID)
         !  TEvapDesignMT calc in Get Input to meet lowest evap temp of any MT load on the system.
         !  TEvapNeededMT is fixed at this design value.
         TransSystem(Sysnum)%TEvapNeededMT=TransSystem(SysNum)%TEvapDesignMT
         ! increment TotalCoolingLoad for Compressors/gas cooler on each system
         TransSystem(SysNum)%TotalCoolingLoadMT = TransSystem(SysNum)%TotalCoolingLoadMT + WalkIn(WalkInID)%TotalCoolingLoad
         TransSystem(SysNum)%TotalCondDefrostCredit=TransSystem(SysNum)%TotalCondDefrostCredit +   &
            WalkIn(WalkInID)%HotDefrostCondCredit
      END DO !NumWalkInsMT systems
    END IF !TransSystem(SysNum)%NumWalkInsMT > 0

    IF(TransSystem(SysNum)%NumWalkInsLT > 0) THEN
      DO WalkInIndex=1,TransSystem(SysNum)%NumWalkInsLT
         WalkInID=TransSystem(Sysnum)%WalkInNumLT(WalkInIndex)
         CALL CalculateWalkIn(WalkInID)
         !  TEvapDesignLT calc in Get Input to meet lowest evap temp of any LT load on the system.
         !  TEvapNeeded is fixed at this design value.
         TransSystem(Sysnum)%TEvapNeededLT=TransSystem(SysNum)%TEvapDesignLT
         ! increment TotalCoolingLoad for Compressors/gas cooler on each system
         TransSystem(SysNum)%TotalCoolingLoadLT = TransSystem(SysNum)%TotalCoolingLoadLT + WalkIn(WalkInID)%TotalCoolingLoad
         TransSystem(SysNum)%TotalCondDefrostCredit=TransSystem(SysNum)%TotalCondDefrostCredit +   &
            WalkIn(WalkInID)%HotDefrostCondCredit
      END DO !NumWalkInsLT systems
    END IF !TransSystem(SysNum)%NumWalkInsLT > 0

    !add suction pipe heat gains (W) if input by user
    !Suction pipe heat gains aren't included in the reported total system load, but are heat gains that must be met in
    !  gas cooler and compressor loads.
    TransSystem(SysNum)%PipeHeatLoadMT = 0.d0
    IF(TransSystem(SysNum)%SumUASuctionPipingMT > mysmallnumber) THEN
      SuctionPipeZoneTemp = Node(TransSystem(SysNum)%SuctionPipeZoneNodeNumMT)%Temp
      TransSystem(SysNum)%PipeHeatLoadMT = TransSystem(SysNum)%SumUASuctionPipingMT * &
                      (SuctionPipeZoneTemp - TransSystem(Sysnum)%TEvapNeededMT)
      ! pipe heat load is a positive number (ie. heat absorbed by pipe, so needs to be subtracted
      !   from refrigcasecredit (- for cooling zone, + for heating zone)
      SuctionPipeActualZoneNum = TransSystem(SysNum)%SuctionPipeActualZoneNumMT
      !Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .false.
      IF((.NOT. UseSysTimeStep).AND.((NumSimulationCases > 0).OR.( NumSimulationWalkIns > 0)))THEN
        RefrigCaseCredit(SuctionPipeActualZoneNum)%SenCaseCreditToZone = &
            RefrigCaseCredit(SuctionPipeActualZoneNum)%SenCaseCreditToZone - TransSystem(SysNum)%PipeHeatLoadMT
      END IF !UseSysTimeStep
    END IF

    TransSystem(SysNum)%PipeHeatLoadLT = 0.d0
    IF(TransSystem(SysNum)%SumUASuctionPipingLT > mysmallnumber) THEN
      SuctionPipeZoneTemp = Node(TransSystem(SysNum)%SuctionPipeZoneNodeNumLT)%Temp
      TransSystem(SysNum)%PipeHeatLoadLT = TransSystem(SysNum)%SumUASuctionPipingLT * &
                      (SuctionPipeZoneTemp - TransSystem(Sysnum)%TEvapNeededLT)
      ! pipe heat load is a positive number (ie. heat absorbed by pipe, so needs to be subtracted
      !   from refrigcasecredit (- for cooling zone, + for heating zone)
      SuctionPipeActualZoneNum = TransSystem(SysNum)%SuctionPipeActualZoneNumLT
      !Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .false.
      IF((.NOT. UseSysTimeStep).AND.((NumSimulationCases > 0).OR.( NumSimulationWalkIns > 0)))THEN
        RefrigCaseCredit(SuctionPipeActualZoneNum)%SenCaseCreditToZone = &
            RefrigCaseCredit(SuctionPipeActualZoneNum)%SenCaseCreditToZone - TransSystem(SysNum)%PipeHeatLoadLT
      END IF !UseSysTimeStep
    END IF

  END DO  ! SysNum

! Need to know if shared gas coolers are present. If so, energy
! transfer between detailed transcritical refrigeration systems
! requires additional iteration at this level.

  StartMechSubcoolLoop=3
  If (NumSimulationSharedGasCoolers > 0) StartMechSubcoolLoop=1

  FirstSCLoop=.TRUE.
  Do Loopnum= StartMechSubcoolLoop,3

    DO SysNum = 1, NumTransRefrigSystems
      !Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
      !only calc detailed system if have load
      TransSystem(SysNum)%TotalSystemLoadMT = TransSystem(SysNum)%TotalCoolingLoadMT
      IF (TransSystem(SysNum)%TransSysType == 2) THEN
        TransSystem(SysNum)%TotalSystemLoadLT = TransSystem(SysNum)%TotalCoolingLoadLT
      END IF
      TransSystem(SysNum)%TotalSystemLoad = TransSystem(SysNum)%TotalSystemLoadLT + TransSystem(SysNum)%TotalSystemLoadMT
      IF (TransSystem(SysNum)%TotalSystemLoad > 0.d0)  THEN
         IF (TransSystem(SysNum)%TransSysType == 2) THEN
            TransSystem(SysNum)%CpSatVapEvapLT =   &
               GetSatSpecificHeatRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(SysNum)%TEvapNeededLT,&
                                                 1.0d0,TransSystem(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
            TransSystem(SysNum)%HCaseOutLT =   &
               GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(Sysnum)%TEvapNeededLT, &
                                             1.0d0,TransSystem(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')  +  &
                                             TransSystem(SysNum)%CpSatVapEvapLT*TransCaseSuperheat
         END IF
         TransSystem(SysNum)%CpSatVapEvapMT =   &
            GetSatSpecificHeatRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(SysNum)%TEvapNeededMT,&
                                              1.0d0,TransSystem(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
         TransSystem(SysNum)%HCaseOutMT =   &
            GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(Sysnum)%TEvapNeededMT, &
                                          1.0d0,TransSystem(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')  +  &
                                          TransSystem(SysNum)%CpSatVapEvapMT*TransCaseSuperheat

       !Produce first time step estimates.
       !Assume no subcoolers and neglect flow through bypass.
       TransSystem(SysNum)%TReceiver = GetSatTemperatureRefrig(TransSystem(SysNum)%RefrigerantName, &
                                       TransSystem(SysNum)%PReceiver,TransSystem(SysNum)%RefIndex, &
                                       'SimulateDetailedRefrigerationSystems')
       TransSystem(SysNum)%HSatLiqReceiver =   &
          GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(SysNum)%TReceiver, &
                                             0.0d0,TransSystem(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
       TransSystem(SysNum)%CpSatLiqReceiver =   &
          GetSatSpecificHeatRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(SysNum)%TReceiver,&
                                              0.0d0,TransSystem(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
       TransSystem(SysNum)%HCaseInMT = TransSystem(SysNum)%HSatLiqReceiver
       TransSystem(SysNum)%HCaseInLT = TransSystem(SysNum)%HSatLiqReceiver
       TransSystem(SysNum)%RefMassFlowtoLTLoads=0.0d0
       TransSystem(SysNum)%RefMassFlowCompsLP=0.0d0
       TransSystem(SysNum)%DelHSubcoolerDis = 0.0d0
       TransSystem(SysNum)%DelHSubcoolerSuc = 0.0d0
       IF (TransSystem(SysNum)%TransSysType == 2) THEN
          TransSystem(SysNum)%RefMassFlowtoLTLoads=TransSystem(SysNum)%TotalSystemLoadLT/ &
                                                   (TransSystem(SysNum)%HCaseOutLT-TransSystem(SysNum)%HCaseInLT)
          TransSystem(SysNum)%RefMassFlowCompsLP=TransSystem(SysNum)%RefMassFlowtoLTLoads
       END IF  ! (TransSystem(SysNum)%TransSysType == 2)
       TransSystem(SysNum)%RefMassFlowtoMTLoads=TransSystem(SysNum)%TotalSystemLoadMT/ &
                                                (TransSystem(SysNum)%HCaseOutMT-TransSystem(SysNum)%HCaseInMT)
       TransSystem(SysNum)%RefMassFlowCompsHP=TransSystem(SysNum)%RefMassFlowtoLTLoads+TransSystem(SysNum)%RefMassFlowtoMTLoads

       CALL CalcDetailedTransSystem(SysNum)
!       TransCritSysFlag = .FALSE.

     END IF  !TransSystem(SysNum)%TotalSystemLoad > 0
  END DO  !Sysnum over NumRefrigSystems
    FirstSCLoop=.FALSE.
  END DO    !Loopnum, three times for buildings with multiple detailed systems connected with shared gas coolers

! Unmet load is done outside iterative loop
 DO SysNum = 1, NumTransRefrigSystems
  !Only do those systems appropriate for this analysis, supermarket type on load time step or coil type on sys time step
  IF((.NOT. UseSysTimeStep) .AND. (.NOT. WarmUpFlag)) THEN
    CurrentLoads = TransSystem(SysNum)%TotalSystemLoad
    ! Meeting current and possibly some portion of the previously unmet energy
    ! perhaps future interest in reporting percent of installed capacity used(or number of compressors) ?
    ! If the system compressors were unable to meet the current loads, save energy to be met in succeeding time step
    ! Note the unmet energy is turned into a rate and applied to the system load at the start of calccompressor
    TransSystem(SysNum)%UnmetEnergy=TransSystem(SysNum)%UnmetEnergy + (CurrentLoads - TransSystem(SysNum)%TotCompCapacity)* &
                 TimeStepZone*SecInHour!

    IF (TransSystem(SysNum)%UnmetEnergy > MyLargeNumber) THEN
        TransSystem(SysNum)%UnmetEnergy = MyLargeNumber
        IF (ShowUnmetEnergyWarningTrans(SysNum)) Then
           CALL ShowWarningError('Refrigeration:TranscriticalSystem: '//TRIM(TransSystem(SysNum)%Name))
           CALL ShowContinueError(' The specified compressors for this system are unable to meet ')
           CALL ShowContinueError(' the sum of the refrigerated case loads and subcooler loads (if any) for this sytem.')
           ShowUnmetEnergyWarningTrans(SysNum) = .FALSE.
        END IF  !show warning
    END IF    ! > mylarge number

   !Zone-located air-cooled gas cooler reject heat also has to be outside iterative loop
    IF (TransSystem(SysNum)%SystemRejectHeatToZone) THEN
       CondInletAirZoneNum = GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%InletAirZoneNum
       !Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .false.
       IF((.NOT. UseSysTimeStep).AND.((NumSimulationCases > 0).OR.( NumSimulationWalkIns > 0)))THEN
         RefrigCaseCredit(CondInletAirZoneNum)%SenCaseCreditToZone = &
           RefrigCaseCredit(CondInletAirZoneNum)%SenCaseCreditToZone + &
           TransSystem(SysNum)%NetHeatRejectLoad    !Adding heat is positive
       END IF !UseSystimestep
    END IF !Reject heat to zone

    ! Report variables
    TransSystem(SysNum)%PipeHeatEnergy     = (TransSystem(SysNum)%PipeHeatLoadMT + TransSystem(SysNum)%PipeHeatLoadLT) &
                                             * LocalTimeStep * SecInHour
    TransSystem(SysNum)%TotalCoolingEnergy = (TransSystem(SysNum)%TotalCoolingLoadMT + TransSystem(SysNum)%TotalCoolingLoadMT) &
                                             * LocalTimeStep * SecInHour
   END IF  !(.NOT. UseSysTimeStep).AND. (.not. warmupflag)
 END DO     ! Sysnum = 1,NumTransRefrigSystems

! Update for sending to zone equipment manager. (note report variables are summed elsewhere)

 CALL SumZoneImpacts

 RETURN

END SUBROUTINE SimulateDetailedTransRefrigSystems

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalcDetailedSystem(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL, Assisted by Hugh Henderson
          !       DATE WRITTEN   Spring 2008
          !       Using condenser solution algorithms written by Richard Raustad, FSEC
          !          Oct/Nov 2004, and MODIFIED by Shirey, FSEC Dec 2004, and Hudson, ORNL in 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the power and energy needed to meet the refrigeration loads for a particular detailed
          ! refrigeration system comprised of multiple cases, one condenser, and multiple compressors.

          ! METHODOLOGY EMPLOYED:
          ! Sum the refrigeration loads on the system and determine the required evaporating temperature.
          ! Using the initial estimate for condensing temperature, dispatch the compressors to
          ! determine the needed power, energy consumption, and refrigerant mass flow.
          ! Calculate the condenser fan/pump power and consumption.
          ! Calculate the condensing temperature as a function of environment and load.
          ! Resolve the impact of subcooler heat transfer between and among systems
          ! Iterate until the calculated refrigerant mass flow through the compressors converges, which
          ! typically requires less than 5 iterations. This was found to be more sensitive than converging
          ! upon the calculated condensing temperature.

          ! REFERENCES:
          ! "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
          !  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

          ! Kyle A. Manske, Performance Optimization of Industrial Refrigeration Systems,
          !  A thesis submitted in partial fulfillment of the requirements for the degree of
          !  Master of Science, University of Wisconsin-Madison, 1999

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER ::ErrorTol       = 0.001d0 !Iterative solution tolerance

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        ::  NumIter
  LOGICAL        ::  NotBalanced
  REAL(r64)      ::  TCondStart
  REAL(r64)      ::  MassFlowCompsStart = 0.0d0         ! Mass flow through (low-stage) compressors (single- or two-stage systems)
  REAL(r64)      ::  MassFlowHiStageCompsStart = 0.0d0  ! Mass flow through high-stage comrpessors (two-stage systems only)
  REAL(r64)      ::  ErrorMassFlowComps =0.0d0          ! Error in calculated (low stage) compressor mass flow (single- or two-stage systems)
  REAL(r64)      ::  ErrorMassFlowHiStageComps =0.0d0   ! Error in calculated high-stage compressor mass flow (two-stage systems only)

!Balance This Refrigeration System using calculated refrigerant flow
NotBalanced=.TRUE.
NumIter = 0

DO WHILE (NotBalanced)
  !Set values for iteration convergence tolerance check
  NumIter=NumIter+1
  TCondStart=System(SysNum)%TCondense
  MassFlowCompsStart=System(SysNum)%RefMassFlowComps
  IF(System(SysNum)%NumStages == 2) THEN  ! Two-stage systems
    MassFlowHiStageCompsStart = System(SysNum)%RefMassFlowHiStageComps
  END IF

  IF (System(SysNum)%NumSubcoolers > 0)CALL CalculateSubcoolers(Sysnum)
  CALL CalculateCompressors(Sysnum)
  CALL CalculateCondensers(Sysnum)
  System(SysNum)%RefMassFlowtoLoads=System(SysNum)%TotalSystemLoad/(System(SysNum)%HCaseOut-System(SysNum)%HCaseIn)
  IF(NumIter < 2)CYCLE
  !Previously did error check on calculated Tcondense, but not sensitive enough
  IF((System(SysNum)%RefMassFlowtoLoads ==0.0d0)  .OR. & !.OR. (MassFlowCasesStart == 0.0)
      (MassFlowCompsStart == 0.0d0)) THEN
      CALL ShowWarningError('Refrigeration:System: '//TRIM(System(SysNum)%Name)//&
                             ' showing zero refrigeration flow.')
  ELSE
     ErrorMassFlowComps=ABS(MassFlowCompsStart-System(SysNum)%RefMassFlowComps)/MassFlowCompsStart
     IF(System(SysNum)%NumStages == 2) THEN  ! Two-stage systems
       ErrorMassFlowHiStageComps=ABS(MassFlowHiStageCompsStart-System(SysNum)%RefMassFlowHiStageComps)/ &
                                 MassFlowCompsStart
     END IF
  END IF !denominator zero check
  IF(NumIter > 20)EXIT
  IF(ErrorMassFlowComps < ErrorTol) THEN
    IF(System(SysNum)%NumStages == 1) THEN
      NotBalanced=.FALSE.
    ELSE IF(System(SysNum)%NumStages == 2 .AND. ErrorMassFlowHiStageComps < ErrorTol) THEN
      NotBalanced=.FALSE.
    END IF
  END IF
END DO !error check

RETURN

END SUBROUTINE CalcDetailedSystem

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalcDetailedTransSystem(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brian A. Fricke, ORNL
          !       DATE WRITTEN   Fall 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the power and energy needed to meet the refrigeration loads for a detailed transcritical
          ! CO2 refrigeration system comprised of multiple cases and walk-ins, one gas cooler, and
          ! multiple compressors.

          ! METHODOLOGY EMPLOYED:
          ! Sum the refrigeration loads on the system and determine the required evaporating temperature.
          ! Dispatch the compressors to determine the needed power, energy consumption, and refrigerant
          ! mass flow. Calculate the gas cooler fan power and consumption. Calculate the gas cooler
          ! outlet temperature and pressure as a function of ambient temperature. Iterate until the
          ! calculated refrigerant mass flow through the receiver bypass converges, which typically
          ! requires less than 5 iterations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)   :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64), PARAMETER ::ErrorTol       = 0.001d0     ! Iterative solution tolerance

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER     ::  NumIter                ! Iteration counter
LOGICAL     ::  NotBalanced            ! Flag to indicate convergence, based on system balance
REAL(r64)   ::  MassFlowStart          ! Initial refrigerant mass flow through receiver bypass
REAL(r64)   ::  ErrorMassFlow          ! Error in calculated refrigerant mass flow trhough receiver bypass


!Balance this refrigeration system using calculated refrigerant flow
NotBalanced=.TRUE.
NumIter = 0

  ! Set initial guess for receiver bypass refrigerant flow rate
  MassFlowStart = 0.5d0
  DO WHILE (NotBalanced)
    NumIter=NumIter+1

    IF (TransSystem(SysNum)%NumGasCoolers >= 1) CALL CalcGasCooler(Sysnum)
    CALL CalculateTransCompressors(Sysnum)
    IF(NumIter < 2)CYCLE
    IF ((TransSystem(SysNum)%RefMassFlowReceiverBypass == 0.0d0) .OR. &
        (MassFlowStart == 0.0d0)) THEN
        CALL ShowSevereError('Refrigeration:TranscriticalSystem: '//TRIM(TransSystem(SysNum)%Name)//&
                             ' showing zero refrigerant flow through receiver bypass.')
        CALL ShowContinueError('Receiver Bypass Flow = '//TRIM(RoundSigDigits(TransSystem(SysNum)%RefMassFlowReceiverBypass,6)))
        CALL ShowContinueError('Check input file to ensure that refrigeration loads on this system are not zero.')
    ELSE
       ErrorMassFlow = ABS(MassFlowStart-TransSystem(SysNum)%RefMassFlowReceiverBypass)/MassFlowStart
       MassFlowStart = TransSystem(SysNum)%RefMassFlowReceiverBypass
    END IF !denominator zero check
    IF(NumIter > 20)EXIT
    IF(ErrorMassFlow < ErrorTol) NotBalanced=.FALSE.
  END DO !error check

RETURN

END SUBROUTINE CalcDetailedTransSystem

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalculateCondensers(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall and C. R. Hudson, ORNL, Assisted by Hugh Henderson
          !       DATE WRITTEN   Spring 2008
          !       Using condenser solution algorithms written by Richard Raustad, FSEC
          !          Oct/Nov 2004, and MODIFIED by Shirey, FSEC Dec 2004, and Hudson, ORNL in 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the condenser heat rejection for a particular detailed
          ! refrigeration system and condensing temperature (part of iterative soln for cond temp).

          ! METHODOLOGY EMPLOYED:
          ! Calculate the condenser fan/pump power and consumption
          ! using manufacturer's rating data and fan power correlations
          ! from ASHRAE and evaporative effectiveness based on enthalpy
          ! similar to work done by Manske.

          ! From Heejin Cho, Re variable frequency drive fans,
          ! "From HVAC forums, I learned that it is common practice to set a
          ! minimum frequency at 15 or 20 Hz to protect motors from overheating. The
          ! full speed is at 60 Hz. The ratio of minimum and maximum frequencies
          ! will correspond to the ratio of minimum and maximum flow rates."

          ! REFERENCES:
          ! "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
          !  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

          ! Kyle A. Manske, Performance Optimization of Industrial Refrigeration Systems,
          !  A thesis submitted in partial fulfillment of the requirements for the degree of
          !  Master of Science, University of Wisconsin-Madison, 1999

          ! Lawrence Berkeley Laboratory and Resource Dynamics, Improving Fan Systrem Performance,
          !   A Sourcebook for Industry, DOE/GO-102003-1294, April 2003

          ! USE STATEMENTS:
  USE CurveManager,      ONLY : CurveValue
  USE Psychrometrics,    ONLY: PsyRhoAirFnPbTdbW,RhoH2O,PsyWFnTdbTwbPb,PsyTwbFnTdbWPb,&
                               PsyHFnTdbW,PsyTsatFnHPb, PsyWFnTdpPb,PsyHFnTdbRhPb
  USE DataEnvironment,   ONLY: OutBaroPress,OutHumRat,OutDryBulbTemp
  USE DataWater,         ONLY: WaterStorage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
     REAL(r64), PARAMETER :: BleedRateConstant = 5.0D-10    !water purge rate for evaporative
                                 !  condensers (m3/W-s) equal to 3 GPM per 100 tons (BAC Engineering Reference)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: CondID       ! Condenser Number
  INTEGER     :: CondCreditWarnIndex1 !Used to sum up warning count
  INTEGER     :: CondCreditWarnIndex2 !Used to sum up warning count
  INTEGER     :: CondCreditWarnIndex3 !Used to sum up warning count
  INTEGER     :: CondCreditWarnIndex4 !Used to sum up warning count
  INTEGER     :: CondCreditWarnIndex5 !Used to sum up warning count
  INTEGER     :: CondCreditWarnIndex6 !Used to sum up warning count
  INTEGER     :: CondCreditWarnIndex7 !Used to sum up warning count
  INTEGER     :: Sysloop      ! counter over number of systems attached to this condenser
  INTEGER     :: SystemID     ! System number rejecting heat to this condenser
  LOGICAL     :: EvapAvail    ! Control for evap condenser availability

  REAL(r64)   :: AirVolRatio   ! Ratio of air volume needed to remove load relative to design load
  REAL(r64)   :: AirDensity     ! Density of air at condenser inlet [kg/m3]
  REAL(r64)   :: AirDensityDry  ! Density of dry air at condenser inlet temperature [kg/m3]
  REAL(r64)   :: ActualFanPower ! Fan power after adjustments for partially loaded condenser [W]
  REAL(r64)   :: BPress        ! Barometric pressure at condenser air inlet node [Pa]
  REAL(r64)   :: CapFac        ! Capacity Factor
  REAL(r64)   :: Effectiveness  ! for evap condenser, =capacity/max cap, where max cap is cap if Tairout equal Tcondense
  REAL(r64)   :: EnthalpyAtTcond ! enthalpy of saturated air at Tcondense
  REAL(r64)   :: EnthalpyAirIn   ! Enthalpy of air entering condenser [J/kg]
  REAL(r64)   :: EnthalpyAirOut  !Enthalpy of air leaving condenser [J/kg]
  REAL(r64)   :: FanMinAirFlowRatio    ! Minimum fan air flow ratio
  REAL(r64)   :: FanPowerRatio   ! Calculated fan power ratio
  REAL(r64)   :: HRCF            ! Heat Rejection Capacity Factor (convention for evap condensers)
  REAL(r64)   :: HRCFFullFlow    ! Heat Rejection Capacity Factor at full air flow
  REAL(r64)   :: HumRatIn        ! Humidity ratio of inlet air to condenser [kg/kg]
  REAL(r64)   :: HumRatOut       ! Humidity ratio of outlet air from condenser (assumed saturated) [kg/kg]
  REAL(r64)   :: LocalTimeStep   ! Set equal to either TimeStepSys or TimeStepZone
  REAL(r64)   :: OutWbTemp       ! Outdoor wet bulb temp at condenser air inlet node [C]
  REAL(r64)   :: OutDbTemp       ! Outdoor dry bulb temp at condenser air inlet node [C]
  REAL(r64)   :: PurgeRate       ! Rate of water blow-down/bleed/purge in evap condenseer (m3/s)
  REAL(r64)   :: RatedFanPower   ! local variable equal to input condenser value
  REAL(r64)   :: RatedAirFlowRate !local variable equal to input condenser value
  REAL(r64)   :: QuadBterm       ! calculation step in solving quadratic equation for hrcf
  REAL(r64)   :: Sqrtterm        ! calculation step in solving quadratic equation for hrcf
  REAL(r64)   :: SinkTemp        ! Heat sink temperature used to derate fan power at reduced loads [C]
  REAL(r64)   :: TAirOut         ! Temperature of air leaving condenser [C]
  REAL(r64)   :: TcondCalc       ! Calculated Condensing temperature
  REAL(r64)   :: TotalCondDefrostCredit ! total cond credit from hot gas/brine defr for cases etc. served
                                        !     directly by all systems served by this condenser [W]
  REAL(r64)   :: TotalCondDefCredfromSysID ! cond credit for single system [W]
  REAL(r64)   :: TotalLoadFromSysID     ! total heat rejection load from a single detailed system [W]
  REAL(r64)   :: TotalLoadFromThisSystem ! total heat rejection load from the detailed system id'd in subroutine call [W]
  REAL(r64)   :: TotalLoadFromSystems   ! total heat rejection load from all systems served by this condenser [W]
  REAL(r64)   :: NomCap          ! ne "design" capacity when operating evap condenser at reduced air flow [W]
  REAL(r64)   :: CurMaxCapacity  ! current maximum condenser capacity at delta T present for minimum condensing temperature [W]


LocalTimeStep = TimeStepZone
IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

!Initialize this condenser for this time step
  TotalCondenserPumpPower      = 0.0d0
  TotalBasinHeatPower          = 0.0d0
  TotalCondenserHeat           = 0.0d0
  TotalEvapWaterUseRate        = 0.0d0
  AirVolRatio                  = 1.0d0
  ActualFanPower               = 0.0d0
  TotalCondDefrostCredit       = 0.0d0
  TotalLoadFromSystems         = 0.0d0
  EvapAvail                    = .TRUE.
  CondID                       = System(SysNum)%CondenserNum(1)
  RatedFanPower                = Condenser(CondID)%RatedFanPower
  RatedAirFlowRate             = Condenser(CondID)%RatedAirFlowRate
  FanMinAirFlowRatio           = Condenser(CondID)%FanMinAirFlowRatio
  CondCreditWarnIndex1          = Condenser(CondID)%CondCreditWarnIndex1
  CondCreditWarnIndex2          = Condenser(CondID)%CondCreditWarnIndex2
  CondCreditWarnIndex3          = Condenser(CondID)%CondCreditWarnIndex3
  CondCreditWarnIndex4          = Condenser(CondID)%CondCreditWarnIndex4
  CondCreditWarnIndex5          = Condenser(CondID)%CondCreditWarnIndex5
  CondCreditWarnIndex6          = Condenser(CondID)%CondCreditWarnIndex6
  CondCreditWarnIndex7          = Condenser(CondID)%CondCreditWarnIndex7

 !Sum total condenser load and defrost credits for all systems connected to this condenser
 !  The system values will match the last time that system was solved, so some of the values may be
 !  from the previous overall solution iteration.  However, solution goes through 3 iterations if
 !  there are any shared condensers, so that's ok.
 DO Sysloop = 1,Condenser(CondID)%NumSysAttach
   SystemID = Condenser(CondID)%SysNum(Sysloop)
   TotalCondDefCredfromSysID = System(SystemID)%TotalCondDefrostCredit + System(SystemID)%SumCascadeCondCredit
   TotalCondDefrostCredit    = TotalCondDefrostCredit + TotalCondDefCredfromSysID
   TotalLoadFromSysID        = System(SystemID)%TotalSystemLoad + System(SystemID)%TotCompPower + &
                               System(SystemID)%TotHiStageCompPower + System(SystemID)%PipeHeatLoad
   TotalLoadFromSystems      = TotalLoadFromSystems + TotalLoadFromSysID
   IF(SystemID == SysNum)TotalLoadFromThisSystem = TotalLoadFromSysID
 END DO ! Sysloop over every system connected to this condenser

 ! for cascade condensers, condenser defrost credit gets passed on to the primary system condenser
 IF(Condenser(CondID)%CondenserType == RefrigCondenserTypeCascade) TotalCondDefrostCredit       = 0.0d0

  ! Calculate Total Heat rejection needed.  Assume hermetic compressors - conservative assumption
  ! Note that heat rejection load carried by desuperheater hvac coils or water heaters is the
  ! lagged variable from the previous time step because these are calculated after the refrigeration
  ! system is solved.
Condenser(CondID)%ExternalHeatRecoveredLoad =  Condenser(CondID)%LaggedUsedWaterHeater + &
                                               Condenser(CondID)%LaggedUsedHVACCoil
Condenser(CondID)%InternalHeatRecoveredLoad =  TotalCondDefrostCredit
Condenser(CondID)%TotalHeatRecoveredLoad    =  Condenser(CondID)%ExternalHeatRecoveredLoad + &
                                               TotalCondDefrostCredit

TotalCondenserHeat   =   TotalLoadFromSystems - TotalCondDefrostCredit - &
                         Condenser(CondID)%ExternalHeatRecoveredLoad
IF (TotalCondenserHeat < 0.0d0) THEN

  TotalCondenserHeat = 0.d0
  IF( .not. warmupflag) THEN
    CALL ShowRecurringWarningErrorAtEnd('Refrigeration:System: '//TRIM(System(SysNum)%Name)//&
       ':heat reclaimed(defrost,other purposes) >current condenser load. ',&
       CondCreditWarnIndex1)
    CALL ShowRecurringContinueErrorAtEnd('For heat recovered for defrost: ASHRAE rule of thumb: <= 25% of the load on a rack ',&
       CondCreditWarnIndex2)
    CALL ShowRecurringContinueErrorAtEnd('should be in defrost at the same time. Consider diversifying defrost schedules.',&
       CondCreditWarnIndex3)
    CALL ShowRecurringContinueErrorAtEnd('For heat recovered for other purposes: this warning may be '//&
       'an artifact of refrigeration calculation at the load',CondCreditWarnIndex4)
    CALL ShowRecurringContinueErrorAtEnd('time step and heat recovery at the system time step. '//&
       'In that case, and ONLY if it occurs a large number of times',CondCreditWarnIndex5)
    CALL ShowRecurringContinueErrorAtEnd('(relative to the number of time steps in the '//&
       'simulation), there may be a mis-match between the',CondCreditWarnIndex6)
    CALL ShowRecurringContinueErrorAtEnd('operating schedules of the refrigeration system and the '//&
       'heat recovery load.',CondCreditWarnIndex7)
  END IF !not warmup
END IF !total condenser heat < 0

! Water side of water-cooled condensers simulated in SimRefrigCondenser,
!   Here, we just need load and condensing temperatures.
!   Condensing temperature a fixed delta (the rated approach temperature) from inlet water temp so long as above minimum.
!   Note, if condensing temperature falls below minimum, get warning and reset but no change in water-side calculations.
IF (Condenser(CondID)%CondenserType == RefrigCondenserTypeWater) THEN
      ! Obtain water-cooled condenser inlet/outlet temps
      InletNode = Condenser(CondID)%InletNode
      Condenser(CondID)%InletTemp = Node(InletNode)%Temp
      TCondCalc = Node(InletNode)%Temp +  Condenser(CondID)%RatedApproachT
        IF ((Condenser(CondID)%InletTemp < Condenser(CondID)%InletTempMin) &
           .OR. (TCondCalc < System(SysNum)%TCondenseMin)) THEN
           System(SysNum)%TCondense=System(SysNum)%TCondenseMin
           !Condenser(CondID)%LowTempWarn = Condenser(CondID)%LowTempWarn +1
           IF (Condenser(CondID)%LowTempWarnIndex == 0) THEN
             CALL ShowWarningMessage('Refrigeration:Condenser:WaterCooled '//TRIM(Condenser(CondID)%Name))
             CALL ShowContinueError('Water-cooled condenser inlet temp lower than minimum allowed temp. '// &
              'Check returning water temperature and/or minimum temperature setpoints '// &
              ' relative to minimum allowed condensing temperature.')
           END IF
             CALL ShowRecurringWarningErrorAtEnd('Refrigeration:Condenser:WaterCooled '// TRIM(Condenser(CondID)%Name) // &
              ' - Condenser inlet temp lower than minimum allowed ... continues',&
              Condenser(CondID)%LowTempWarnIndex)
           !END IF
        ELSE
          System(SysNum)%TCondense=TCondCalc
        END IF

ELSEIF ((Condenser(CondID)%CondenserType == RefrigCondenserTypeAir) .OR. &
        (Condenser(CondID)%CondenserType == RefrigCondenserTypeEvap)) THEN
        !Condensing Temp, fan and other aux loads for air-cooled or evap-cooled

  !The rated capacity of air-cooled condenser was adjusted for elevation in get input step
  CapFac =  TotalCondenserHeat/Condenser(CondID)%RatedCapacity
  ! See whether condenser is at ground level or if other air conditions(ie node) have been specified.
  !    Note that air-cooled condensers can draw air from, and reject heat to, a conditioned zone
  !    But evaporative condensers cannot.
  ! Provides effective condensing temperature for air-cooled condenser (or evap if evap is scheduled off)
  IF (Condenser(CondID)%InletAirNodeNum /= 0) THEN
    OutDbTemp = Node(Condenser(CondID)%InletAirNodeNum)%Temp
    BPress = Node(Condenser(CondID)%InletAirNodeNum)%Press
    HumRatIn = Node(Condenser(CondID)%InletAirNodeNum)%HumRat
  ELSE
    OutDbTemp=OutDryBulbTemp
    BPress=OutBaroPress
    HumRatIn = OutHumRat
  ENDIF
  AirDensity=PsyRhoAirFnPbTdbW(BPress,OutDbTemp,HumRatIn)
  AirDensityDry=PsyRhoAirFnPbTdbW(BPress,OutDbTemp,0.0d0)
  ! Evaporative condensers will have their water flow shut off in cold months to avoid
  !  'spectacular' icing problems.  Ideally, the user will use the evaporative schedule input
  !  to set such a schedule.  However, sometimes, users will use a single input deck to model
  !  one building in multiple climates, and may not think to put in such a schedule in the colder
  !  climates.  To accomodate such applications, the variable EvapCutOutTdb is used as an extra
  !  check.

    IF(OutDbTemp < EvapCutOutTdb)EvapAvail = .FALSE.

  ! Check schedule to determine evap condenser availability
  ! IF schedule exists, evap condenser can be scheduled OFF
  IF((Condenser(CondID)%CondenserType == RefrigCondenserTypeEvap) .AND. &
     (Condenser(CondID)%EvapSchedPtr > 0) .AND. &
     (GetCurrentScheduleValue(Condenser(CondID)%EvapSchedPtr)== 0)) EvapAvail = .FALSE.

  !Calculate condensing temperatures for air-cooled and evap-cooled
  IF (Condenser(CondID)%CondenserType == RefrigCondenserTypeEvap)THEN
      !Manufacturer's HRCF regressed to produce a function of the form:
      ! (Tcondense-Twb)=A1 + A2*hrcf + A3/hrcf + A4*Twb
      ! HRCF defined as rated capacity divided by load
      ! Apply ARI490 elevation correction factor here for evap condenser, then apply hrcf limits
      IF(CapFac > 0.0d0) THEN
         HRCF   =  Condenser(CondID)%EvapElevFact/CapFac
         !Condenser(CondNum)%EvapElevFact=1.d0-3.074D-5*Elevation
      ELSE
        HRCF   =   MyLargeNumber
      END IF
      HRCF = MIN(HRCF,Condenser(CondID)%MaxCapFacEvap)
      HRCF = MAX(HRCF,Condenser(CondID)%MinCapFacEvap)
      IF(EvapAvail) THEN
        OutWbTemp = PsyTwbFnTdbWPb(OutDbTemp,HumRatIn,BPress)
        SinkTemp  = OutWbTemp
      ELSE   !evaporative condenser with water spray scheduled off so use Tdb
        HRCF      = HRCF/3.0d0  !reference Menske, cap of evap cond operating dry about 1/3 of rated cap
        HRCF      = MAX(HRCF,Condenser(CondID)%MinCapFacEvap)
        SinkTemp  = OutDbTemp
      END IF  !evap avail, still in evap condenser
      TcondCalc = Condenser(CondID)%EvapCoeff1 + Condenser(CondID)%EvapCoeff2*HRCF + &
                Condenser(CondID)%EvapCoeff3/HRCF + (1.d0 + Condenser(CondID)%EvapCoeff4)*SinkTemp
  ELSE  !air-cooled condenser
      ! MinCondLoad and TempSlope came from condenser capacity curve, using curve backwards
      TcondCalc = OutDbTemp + &
                  (TotalCondenserHeat-Condenser(CondID)%MinCondLoad)*Condenser(CondID)%TempSlope
      SinkTemp = OutDbTemp
  END IF ! if evap-cooled condenser

  ! Fan energy calculations apply to both air- and evap-cooled condensers
  ! Compare calculated condensing temps to minimum allowed to determine fan power/operating mode
  IF (TcondCalc >= System(SysNum)%TCondenseMin) THEN
     System(SysNum)%TCondense=TcondCalc
     ActualFanPower=RatedFanPower
     AirVolRatio=1.0d0

  ELSE   !need to reduce fan speed to reduce air flow and keep Tcond at or above Tcond min
     System(SysNum)%TCondense=System(SysNum)%TCondenseMin
     TcondCalc = System(SysNum)%TCondenseMin
     ! recalculate CapFac at current delta T
     IF (Condenser(CondID)%CondenserType == RefrigCondenserTypeAir) THEN
       CurMaxCapacity = CurveValue(Condenser(CondID)%CapCurvePtr,(System(SysNum)%TCondenseMin - OutDbTemp))
       CapFac = TotalCondenserHeat / CurMaxCapacity
       AirVolRatio = MAX(FanMinAirFlowRatio,CapFac**CondAirVolExponentDry) !Fans limited by minimum air flow ratio
       AirVolRatio = MIN(AirVolRatio, 1.d0)
     ELSE  ! Condenser(CondID)%CondenserType == RefrigCondenserTypeEvap
       HRCFFullFlow=HRCF
       !if evap condenser need to back calculate the operating capacity using HRCF relationship, given known Tcond
       QuadBterm=Condenser(CondID)%EvapCoeff1-(System(SysNum)%TCondense-SinkTemp)+ &
                                                Condenser(CondID)%EvapCoeff4*SinkTemp
       Sqrtterm =QuadBterm**2.d0 - 4.d0*Condenser(CondID)%EvapCoeff2*Condenser(CondID)%EvapCoeff3
       IF(Sqrtterm < 0.d0)THEN   ! only happens for very high wet bulb temps
         HRCF=Condenser(CondID)%EvapElevFact*Condenser(CondID)%MaxCapFacEvap
         IF(.NOT. EvapAvail) HRCF = HRCF/3.0d0
         HRCF = MAX(HRCF,Condenser(CondID)%MinCapFacEvap)
       ELSE
         HRCF=Condenser(CondID)%EvapElevFact*(-QuadBterm-SQRT(Sqrtterm))/ (2.d0*Condenser(CondID)%EvapCoeff2)
         IF(.NOT. EvapAvail) HRCF = HRCF/3.0d0
         HRCF = MIN(HRCF,Condenser(CondID)%MaxCapFacEvap)
         HRCF = MAX(HRCF,Condenser(CondID)%MinCapFacEvap)
       END IF !sqrtterm
       CapFac=HRCF/HRCFFullFlow   !note, HRCFFullFlow previously limited between min and max,so can't be zero
       IF(EvapAvail) THEN
         AirVolRatio = MAX(FanMinAirFlowRatio,CapFac**CondAirVolExponentEvap) !Fans limited by minimum air flow ratio
       ELSE  !evap not available
         AirVolRatio = MAX(FanMinAirFlowRatio,CapFac**CondAirVolExponentDry) !Fans limited by minimum air flow ratio
       END IF !evap available
       AirVolRatio = MIN(AirVolRatio, 1.d0)
     END IF  ! condenser type = RefrigCondenserTypeAir with else for evap

     SELECT CASE (Condenser(CondID)%FanSpeedControlType)
       CASE(FanVariableSpeed)  !fan power law, adjusted for reality, applies
         FanPowerRatio=AirVolRatio**2.5d0
         ActualFanPower=FanPowerRatio*RatedFanPower
       CASE(FanConstantSpeed)
         ActualFanPower=AirVolRatio*EXP(1.d0-AirVolRatio)*RatedFanPower
       CASE(FanConstantSpeedLinear)
         ActualFanPower=AirVolRatio*RatedFanPower
       CASE(FanTwoSpeed)
         !low speed setting of 1/2 fan speed can give up to 60% of capacity.
         !1/2 speed corresonds to ~1/8 power consumption (FanHalfSpeedRatio = 1/(2**2.5) = 0.1768)
         !dampers are used to control flow within those two ranges as in FanConstantSpeed
         ActualFanPower=AirVolRatio*EXP(1.d0-AirVolRatio)*RatedFanPower
         IF(CapFac < CapFac60Percent) &
            ActualFanPower=((AirVolRatio+0.4d0)*(FanHalfSpeedRatio))*EXP(1.d0-AirVolRatio)*RatedFanPower
     END SELECT  ! fan speed control type
  END IF   !Tcondense >= Tcondense minimum

  IF ((Condenser(CondID)%CondenserType == RefrigCondenserTypeEvap) .AND. (EvapAvail)) THEN
      ! calculate evap water use,  need to include bleed down/purge water as well as water
      ! actually evaporated.  Use BAC Engineering Reference value of 3 gpm/100 tons because it's more
      ! conservative than the ASHRAE value.
      !  Also, based on experience, running the evap water when outdoor T near freezing
      !  leads to 'spectacular' ice, so schedule evap off when Tdb <=4 C.
      !Calculate bleed/purge rate of water loss as a function of capacity, 3 gpm/100 tons refrigeration
      PurgeRate=TotalCondenserHeat*BleedRateConstant
      EnthalpyAirIn=PsyHFnTdbW(OutDbTemp,HumRatIn)
      !calculate effectiveness at rated conditions, so use Tcondcalc)
      EnthalpyAtTcond = PsyHFnTdbRhPb(TcondCalc,1.0d0,BPress)
      Effectiveness = TotalCondenserHeat/(RatedAirFlowRate*AirDensity*(EnthalpyAtTcond-EnthalpyAirIn))
      !need to limit max effectiveness for errors due to working beyond limits of HRCF in manuf data
      Effectiveness = Min(Effectiveness,0.9d0)
      EnthalpyAirOut=EnthalpyAirIn + Effectiveness*(EnthalpyAtTcond-EnthalpyAirIn)
      !Air leaving the evaporative condenser is saturated
      TAirOut=PsyTsatFnHPb(EnthalpyAirOut,BPress)
      HumRatOut=PsyWFnTdpPb(TAirOut,BPress)
      TotalEvapWaterUseRate = PurgeRate + RatedAirFlowRate* AirVolRatio * &
          AirDensityDry * (HumRatOut-HumRatIn) / RhoH2O(OutWbTemp)
      ! assumes evap water pump runs whenever evap cooling is available to minimize scaling
      TotalCondenserPumpPower  = Condenser(CondID)%EvapPumpPower
      ! calculate basin water heater load
      IF(TotalCondenserHeat == 0.0d0 .AND.OutDbTemp < Condenser(CondID)%BasinHeaterSetPointTemp) THEN
        TotalBasinHeatPower = MAX(0.0d0,Condenser(CondID)%BasinHeaterPowerFTempDiff * &
               (Condenser(CondID)%BasinHeaterSetPointTemp - OutDbTemp))
        ! provide warning if no heater power exists
        IF (TotalBasinHeatPower == 0.0d0) THEN
          !Condenser(CondID)%EvapFreezeWarn = Condenser(CondID)%EvapFreezeWarn + 1
          IF (Condenser(CondID)%EvapFreezeWarnIndex == 0) THEN
            CALL ShowWarningMessage('Refrigeration Condenser '// TRIM(Condenser(CondID)%Name) // &
                              ' - Evap cooling of condenser underway with no basin heater power')
            CALL ShowContinueError('and condenser inlet air dry-bulb temp at or below the basin heater setpoint temperature.' )
            CALL ShowContinueErrorTimeStamp('Continuing simulation.')
          END IF
          CALL ShowRecurringWarningErrorAtEnd('Refrigeration Condenser '// TRIM(Condenser(CondID)%Name) // &
                     ' - Evap cooling of condenser underway with no basin heater power ... continues',&
                     Condenser(CondID)%EvapFreezeWarnIndex)
          !END IF  !freeze warnings <= 5
        END IF    ! basin power == 0
      END IF      ! no load and cold outside
  END IF   !EvapAvail

ELSEIF (Condenser(CondID)%CondenserType == RefrigCondenserTypeCascade) THEN ! continuing Condenser type = water, (evap or air), or cascade
  !Cascade condenser does not iterate.  Condensing temperature specified as a load on higher temp system
  !    or floats to meet other loads on that system
  !therese ** future - here and for new phase change heat exchanger - need to handle unmet loads!

  System(SysNum)%TCondense = Condenser(CondID)%RatedTCondense

  IF ((System(SysNum)%NumNonCascadeLoads > 0) .AND. (Condenser(CondID)%CascadeTempControl == CascadeTempFloat)) THEN
      System(SysNum)%TCondense = System(Condenser(CondID)%CascadeSinkSystemID)%TEvapNeeded + &
      Condenser(CondID)%RatedApproachT
      IF (System(SysNum)%TCondense < System(SysNum)%TCondenseMin)THEN
        System(SysNum)%TCondense=System(SysNum)%TCondenseMin
        CALL ShowRecurringWarningErrorAtEnd('Refrigeration Condenser '// TRIM(Condenser(CondID)%Name) // &
              ' - Cascade condenser floating condensing temperature less than specified minimum '// &
              ' condensing temperature. Minimum specified temperature used for system below cascade condenser.'// &
              '  No correction made for system absorbing heat rejected by the cascade condenser.',&
              Condenser(CondID)%EvapFreezeWarnIndex)
      END IF !floating condensing temperature less than specified min for system
  END IF !floating temperature
END IF  ! Condenser type = water, (evap or air), or cascade

  Condenser(CondID)%ActualFanPower           = ActualFanPower
  Condenser(CondID)%FanElecEnergy            = ActualFanPower * LocalTimeStep * SecInHour
  Condenser(CondID)%EvapWaterConsumpRate     = TotalEvapWaterUseRate
  Condenser(CondID)%EvapWaterConsumption     = TotalEvapWaterUseRate * LocalTimeStep * SecInHour
  Condenser(CondID)%ActualEvapPumpPower      = TotalCondenserPumpPower
  Condenser(CondID)%EvapPumpConsumption      = TotalCondenserPumpPower * LocalTimeStep * SecInHour
  Condenser(CondID)%BasinHeaterPower         = TotalBasinHeatPower
  Condenser(CondID)%BasinHeaterConsumption   = TotalBasinHeatPower * LocalTimeStep * SecInHour
  Condenser(CondID)%CondLoad                 = TotalCondenserHeat
  Condenser(CondID)%CondEnergy               = TotalCondenserHeat * LocalTimeStep * SecInHour
  Condenser(CondID)%CondCreditWarnIndex1     = CondCreditWarnIndex1
  Condenser(CondID)%CondCreditWarnIndex2     = CondCreditWarnIndex2
  Condenser(CondID)%CondCreditWarnIndex3     = CondCreditWarnIndex3
  Condenser(CondID)%CondCreditWarnIndex4     = CondCreditWarnIndex4
  Condenser(CondID)%CondCreditWarnIndex5     = CondCreditWarnIndex5
  Condenser(CondID)%CondCreditWarnIndex6     = CondCreditWarnIndex6
  Condenser(CondID)%CondCreditWarnIndex7     = CondCreditWarnIndex7
  Condenser(CondID)%ExternalEnergyRecovered  = Condenser(CondID)%ExternalHeatRecoveredLoad * LocalTimeStep * SecInHour
  Condenser(CondID)%InternalEnergyRecovered  = Condenser(CondID)%InternalHeatRecoveredLoad * LocalTimeStep * SecInHour
  Condenser(CondID)%TotalHeatRecoveredEnergy = Condenser(CondID)%TotalHeatRecoveredLoad * LocalTimeStep * SecInHour
  System(SysNum)%NetHeatRejectLoad           = TotalCondenserHeat*TotalLoadFromThisSystem/TotalLoadFromSystems
  System(SysNum)%NetHeatRejectEnergy         = System(SysNum)%NetHeatRejectLoad * LocalTimeStep * SecInHour

  !set water system demand request (if needed)
  IF (Condenser(CondID)%EvapWaterSupplyMode == WaterSupplyFromTank) THEN
    WaterStorage(Condenser(CondID)%EvapWaterSupTankID)%VdotRequestDemand(Condenser(CondID)%EvapWaterTankDemandARRID) &
       = Condenser(CondID)%EvapWaterConsumpRate
  END IF

END SUBROUTINE CalculateCondensers

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalcGasCooler(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brian A. Fricke, ORNL
          !       DATE WRITTEN   Fall 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the gas cooler outlet temperature, the optimum gas cooler pressure, heat rejection,
          ! fan power, and fan energy for a detailed transcritical CO2 refrigeration system.

          ! METHODOLOGY EMPLOYED:
          ! For a specified gas cooler outlet temperature in transcritical operation, there is an optimal gas cooler
          ! pressure which produces the highest COP. A curve-fit equation similar to that presented by Ge and Tassou
          ! (2011) and Sawalha (2008) is used to determine the optimum gas cooler pressure for a given gas cooler
          ! outlet temperature. In subcritical operation, the condensing temperature and pressure are allowed to
          ! float with ambient conditions, above the minimum condensing temperature.

          ! REFERENCES:
          ! Ge, Y.T., and S.A. Tassou. 2011. Performance evaluation and optimal design of supermarket refrigeration
          !     systems with supermarket model "SuperSim", Part I: Model description and validation. International
          !     Journal of Refrigeration 34: 527-539.
          ! Ge, Y.T., and S.A. Tassou. 2011. Performance evaluation and optimal design of supermarket refrigeration
          !     systems with supermarket model "SuperSim", Part II: Model applications. International Journal of
          !     Refrigeration 34: 540-549.
          ! Sawalha, S. 2008. Theoretical evaluation of trans-critical CO2 systems in supermarket refrigeration,
          !     Part I: Modeling, simulation and optimization of two system solutions. International Journal of
          !     Refrigeration 31: 516-524.
          ! Sawalha, S. 2008. Theoretical evaluation of trans-critical CO2 systems in supermarket refrigeration,
          !     Part II: System modifications and comparisons of different solutions. International Journal of
          !     Refrigeration 31: 525-534.

          ! USE STATEMENTS:
  USE DataEnvironment,   ONLY : OutDryBulbTemp

IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: SysNum

! SUBROUTINE ARGUMENT DEFINITIONS:
! na

! SUBROUTINE PARAMETER DEFINITIONS:
! na

! INTERFACE BLOCK SPECIFICATIONS:
! na

! DERIVED TYPE DEFINITIONS:
! na

! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER   :: GasCoolerCreditWarnIndex   ! Warning counter
INTEGER   :: GasCoolerID                ! Gas cooler number
INTEGER   :: Sysloop                    ! Counter over number of systems attached to this gas cooler
INTEGER   :: SystemID                   ! System number rejecting heat to this gas cooler
REAL(r64) :: ActualFanPower             ! Fan power after adjustments for partially loaded gas cooler [W]
REAL(r64) :: AirVolRatio                ! Ratio of air volume needed to remove load relative to design load
REAL(r64) :: CapFac                     ! Capacity factor
REAL(r64) :: FanMinAirFlowRatio         ! Minimum fan air flow ratio
REAL(r64) :: FanPowerRatio              ! Calculated fan power ratio
REAL(r64) :: LocalTimeStep              ! Set equal to either TimeStepSys or TimeStepZone
REAL(r64) :: OutDbTemp                  ! Outdoor dry bulb temperature at gas cooler air inlet node [C]
REAL(r64) :: RatedFanPower              ! Rated fan power for this gas cooler [W]
REAL(r64) :: TotalCondDefCredfromSysID  ! Gas cooler defrost credit for single system [W]
REAL(r64) :: TotalCondDefrostCredit     ! Total gas cooler credit from hot gas/brine defrost for cases etc. served
                                        !     directly by all systems served by this gas cooler [W]
REAL(r64) :: TotalGasCoolerHeat         ! Total gas cooler heat from system [W]
REAL(r64) :: TotalLoadFromSysID         ! Total heat rejection load from a single detailed system [W]
REAL(r64) :: TotalLoadFromSystems       ! Total heat rejection load from all systems served by this condenser [W]
REAL(r64) :: TotalLoadFromThisSystem    ! Total heat rejection load from the detailed system identified in subroutine call [W]


LocalTimeStep = TimeStepZone
IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

!!Initialize this gas cooler for this time step
TotalGasCoolerHeat           = 0.0d0
AirVolRatio                  = 1.0d0
ActualFanPower               = 0.0d0
TotalCondDefrostCredit       = 0.0d0
TotalLoadFromSystems         = 0.0d0
GasCoolerID                  = TransSystem(SysNum)%GasCoolerNum(1)
RatedFanPower                = GasCooler(GasCoolerID)%RatedFanPower
FanMinAirFlowRatio           = GasCooler(GasCoolerID)%FanMinAirFlowRatio
GasCoolerCreditWarnIndex     = GasCooler(GasCoolerID)%GasCoolerCreditWarnIndex

DO Sysloop = 1,GasCooler(GasCoolerID)%NumSysAttach
   SystemID = GasCooler(GasCoolerID)%SysNum(Sysloop)
   TotalCondDefCredfromSysID = TransSystem(SystemID)%TotalCondDefrostCredit
   TotalCondDefrostCredit    = TotalCondDefrostCredit + TotalCondDefCredfromSysID
   TotalLoadFromSysID        = TransSystem(SystemID)%TotalSystemLoadLT + TransSystem(SystemID)%TotalSystemLoadMT + &
                               TransSystem(SystemID)%TotCompPowerLP + TransSystem(SystemID)%TotCompPowerHP + &
                               TransSystem(SystemID)%PipeHeatLoadLT + TransSystem(SystemID)%PipeHeatLoadMT
   TotalLoadFromSystems      = TotalLoadFromSystems + TotalLoadFromSysID
   IF(SystemID == SysNum)TotalLoadFromThisSystem = TotalLoadFromSysID
END DO ! Sysloop over every system connected to this gas cooler

! Calculate Total Heat rejection needed.
GasCooler(GasCoolerID)%InternalHeatRecoveredLoad = TotalCondDefrostCredit
GasCooler(GasCoolerID)%TotalHeatRecoveredLoad    = TotalCondDefrostCredit
TotalGasCoolerHeat = TotalLoadFromSystems - TotalCondDefrostCredit

IF (TotalGasCoolerHeat < 0.0d0) THEN
  TotalGasCoolerHeat = 0.0d0
  IF( .not. warmupflag) &
    CALL ShowRecurringWarningErrorAtEnd('Refrigeration:TranscriticalSystem: '//TRIM(TransSystem(SysNum)%Name)//&
       ':heat reclaimed (defrost,other purposes) is greater than current gas cooler load. '//&
       'ASHRAE rule of thumb: <= 25% of the load on a system '//&
       'should be in defrost at the same time. '//&
       'Consider diversifying defrost schedules.',&
       GasCoolerCreditWarnIndex)
END IF !total gas cooler heat < 0

!The rated capacity of air-cooled gas cooler was adjusted for elevation in get input step
CapFac =  TotalGasCoolerHeat/GasCooler(GasCoolerID)%RatedCapacity
! See whether gas cooler is at ground level or if other air conditions (ie node) have been specified.
! Note that air-cooled gas coolers can draw air from, and reject heat to, a conditioned zone.
IF (GasCooler(GasCoolerID)%InletAirNodeNum /= 0) THEN
  OutDbTemp = Node(GasCooler(GasCoolerID)%InletAirNodeNum)%Temp
ELSE
  OutDbTemp=OutDryBulbTemp
ENDIF
!
! Determine gas cooler outlet temperature and pressure
! Transcritical:  Gas cooler outlet temperature based on ambient temperature and approach temperature.
!                 Determine optimum gas cooler pressure to maximize COP.
! Subcritical:  Allow condensing temperature and pressure to float between minimum condensing temperature and
!               transition temperature.
IF (OutDbTemp > GasCooler(GasCoolerID)%TransitionTemperature) THEN  ! Gas cooler in transcritical operation
   GasCooler(GasCoolerID)%TGasCoolerOut = OutDbTemp + GasCooler(GasCoolerID)%GasCoolerApproachT
   GasCooler(GasCoolerID)%PGasCoolerOut = 1.0d5*(2.3083d0*OutDryBulbTemp+11.9d0)
   IF (GasCooler(GasCoolerID)%PGasCoolerOut < 7.5d6) THEN    ! Ensure gas cooler pressure is at least 7.5 MPa for transcritical operation
      GasCooler(GasCoolerID)%PGasCoolerOut = 7.5d6
   END IF
   GasCooler(GasCoolerID)%HGasCoolerOut = GetSupHeatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName, &
                                          GasCooler(GasCoolerID)%TGasCoolerOut,GasCooler(GasCoolerID)%PGasCoolerOut, &
                                          TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalcGasCooler')
   GasCooler(GasCoolerID)%TransOpFlag = .TRUE.
ELSE  ! Gas cooler in subcritical operation
   GasCooler(GasCoolerID)%TGasCoolerOut = OutDbTemp + GasCooler(GasCoolerID)%SubcriticalTempDiff
   IF (GasCooler(GasCoolerID)%TGasCoolerOut > 30.978d0) THEN    !  Gas temperature should be less than critical temperature
      GasCooler(GasCoolerID)%PGasCoolerOut = 7.2d6            !  Fix the pressure to be subcritical
      GasCooler(GasCoolerID)%TGasCoolerOut = GetSatTemperatureRefrig(TransSystem(SysNum)%RefrigerantName, &
                                             GasCooler(GasCoolerID)%PGasCoolerOut,TransSystem(SysNum)%RefIndex, &
                                             'RefrigeratedCase:CalcGasCooler')
   ELSE IF (GasCooler(GasCoolerID)%TGasCoolerOut > GasCooler(GasCoolerID)%MinCondTemp) THEN    !  Allow condensing temperature to float above the minimum
      GasCooler(GasCoolerID)%PGasCoolerOut = GetSatPressureRefrig(TransSystem(SysNum)%RefrigerantName, &
                                             GasCooler(GasCoolerID)%TGasCoolerOut,TransSystem(SysNum)%RefIndex, &
                                             'RefrigeratedCase:CalcGasCooler')
   ELSE    !  Don't allow condensing temperature to drop below minimum
      GasCooler(GasCoolerID)%TGasCoolerOut = GasCooler(GasCoolerID)%MinCondTemp
      GasCooler(GasCoolerID)%PGasCoolerOut = GetSatPressureRefrig(TransSystem(SysNum)%RefrigerantName, &
                                             GasCooler(GasCoolerID)%TGasCoolerOut,TransSystem(SysNum)%RefIndex, &
                                             'RefrigeratedCase:CalcGasCooler')
   END IF
   GasCooler(GasCoolerID)%HGasCoolerOut = GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName, &
                                          GasCooler(GasCoolerID)%TGasCoolerOut,0.0d0,TransSystem(SysNum)%RefIndex, &
                                          'RefrigeratedCase:CalcGasCooler')
   GasCooler(GasCoolerID)%TransOpFlag = .FALSE.
END IF    ! (OutDbTemp > TransitionTemperature)

IF (GasCooler(GasCoolerID)%TGasCoolerOut < 30.978d0) THEN
  GasCooler(GasCoolerID)%CpGasCoolerOut = GetSatSpecificHeatRefrig(TransSystem(SysNum)%RefrigerantName, &
                                          GasCooler(GasCoolerID)%TGasCoolerOut,0.0d0,TransSystem(SysNum)%RefIndex, &
                                          'RefrigeratedCase:CalcGasCooler')
ELSE
  GasCooler(GasCoolerID)%CpGasCoolerOut = 0.0d0
END IF

! Gas cooler fan energy calculations
AirVolRatio=MAX(FanMinAirFlowRatio,CapFac**CondAirVolExponentDry) !Fans limited by minimum air flow ratio

SELECT CASE (GasCooler(GasCoolerID)%FanSpeedControlType)
  CASE(FanVariableSpeed)  !fan power law, adjusted for reality, applies
    FanPowerRatio=AirVolRatio**2.5d0
    ActualFanPower=FanPowerRatio*RatedFanPower
  CASE(FanConstantSpeed)
    ActualFanPower=AirVolRatio*EXP(1.d0-AirVolRatio)*RatedFanPower
  CASE(FanConstantSpeedLinear)
    ActualFanPower=AirVolRatio*RatedFanPower
  CASE(FanTwoSpeed)
    !low speed setting of 1/2 fan speed can give up to 60% of capacity.
    !1/2 speed corresonds to ~1/8 power consumption (FanHalfSpeedRatio = 1/(2**2.5) = 0.1768)
    !dampers are used to control flow within those two ranges as in FanConstantSpeed
    ActualFanPower=AirVolRatio*EXP(1.d0-AirVolRatio)*RatedFanPower
    IF(CapFac < CapFac60Percent) &
      ActualFanPower=((AirVolRatio+0.4d0)*(FanHalfSpeedRatio))*EXP(1.d0-AirVolRatio)*RatedFanPower
END SELECT  ! fan speed control type

GasCooler(GasCoolerID)%ActualFanPower           = ActualFanPower
GasCooler(GasCoolerID)%FanElecEnergy            = ActualFanPower * LocalTimeStep * SecInHour
GasCooler(GasCoolerID)%GasCoolerLoad            = TotalGasCoolerHeat
GasCooler(GasCoolerID)%GasCoolerEnergy          = TotalGasCoolerHeat * LocalTimeStep * SecInHour
GasCooler(GasCoolerID)%GasCoolerCreditWarnIndex = GasCoolerCreditWarnIndex
GasCooler(GasCoolerID)%InternalEnergyRecovered  = GasCooler(GasCoolerID)%InternalHeatRecoveredLoad * LocalTimeStep * SecInHour
GasCooler(GasCoolerID)%TotalHeatRecoveredEnergy = GasCooler(GasCoolerID)%TotalHeatRecoveredLoad * LocalTimeStep * SecInHour
TransSystem(SysNum)%NetHeatRejectLoad           = TotalGasCoolerHeat*TotalLoadFromThisSystem/TotalLoadFromSystems
TransSystem(SysNum)%NetHeatRejectEnergy         = TransSystem(SysNum)%NetHeatRejectLoad * LocalTimeStep * SecInHour

RETURN

END SUBROUTINE CalcGasCooler

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalculateCompressors(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL, Assisted by Hugh Henderson
          !       DATE WRITTEN   Spring 2008
          !       MODIFIED       Brian Fricke, ORNL, March 2012, added two-stage compression
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the Compressor power, energy, capacity, and efficiency for a particular detailed
          ! refrigeration system.  Routine is capable of modeling single-stage and two-stage
          ! compression refrigeration systems.

          ! METHODOLOGY EMPLOYED:
          ! USe ARI compressor performance curves, the evaporating temperature and condensing temperature

          ! REFERENCES:
          ! "Impact of ASHRAE Standard 62-1989 on Florida Supermarkets",
          !  Florida Solar Energy Center, FSEC-CR-910-96, Final Report, Oct. 1996

          ! ARI Standard 540, 2004, Standard for Performance Rating of Positive Displacement Refrigerant
          !  Comprssors and Compressor Units, Air-Conditionig & Refrigeration Institute,Arlington VA

          ! USE STATEMENTS:
  USE CurveManager,      ONLY : CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
! Following constants approp for R22, R134a, R404a, R507, R410a, R407c, future allow input?
!   May want to allow input to reflect larger pipes selected to reduce delta P and increase compressor efficiency.
!NOTE, these DelT...Pipes reflect the decrease in Pressure in the pipes, NOT thermal transfer through the pipe walls.
REAL(r64), PARAMETER ::DelTSuctPipes  = 1.0d0 ! Tsat drop corresponding to P drop in suction pipes, ASHRAE 2006 p 2.4 (C)
REAL(r64), PARAMETER ::DelTDischPipes = 0.5d0 ! Tsat drop corresponding to P drop in discharge pipes, ASHRAE 2006 p 2.5 (C)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: CompIndex           ! Compressor index within system
  INTEGER     :: CompID              ! Compressor index within all compressors
  INTEGER     :: CondID              ! Condenser index for this refrigeration system
  REAL(r64)   :: AccumLoad           ! Load due to previously unmet compressor loads
  REAL(r64)   :: CaseEnthalpyChangeRated  ! Enthalpy change in cases at compressor rated cond, J/kg
  REAL(r64)   :: CapacityCorrection       ! Capacity at existing subcool/superheat over cap at rated conditions
  REAL(r64)   :: CpSatVapCondense    ! Specific heat of vapor at cond temp J/kg-C
  REAL(r64)   :: DensityRated        ! Density of inlet gas at rated superheat, m3/kg
  REAL(r64)   :: DensityActual       ! Density of superheated gas at compressor inlet, m3/kg
  REAL(r64)   :: HCompinRated        ! Enthalpy entering compressor at rated superheat, J/kg
  REAL(r64)   :: HCaseInRated        ! Enthalpy entering cases at rated subcooling, J/kg
  REAL(r64)   :: HSatVapCondense     ! Enthalpy of saturated vapor at T condense, J/kg
  REAL(r64)   :: HsatVaporforTevapneeded ! Enthalpy saturated vapor at temperature needed at evaporator
  REAL(r64)   :: LFLastComp          ! Load factor for last compressor dispatched
  REAL(r64)   :: LocalTimeStep = 0.0d0 !TimeStepZone for case/walkin systems, TimeStepSys for coil systems
  REAL(r64)   :: MassCorrection      ! Mass flow at existing subcool/superheat over cap at rated conditions
  REAL(r64)   :: NeededCapacity      ! Sum of case loads and mech subcooler loads on suction group
  REAL(r64)   :: Psuction            ! Suction Pressure
  REAL(r64)   :: Pcond               ! Condensing pressure
  REAL(r64)   :: Pevap               ! Evaporating pressure
  REAL(r64)   :: TCompOutEstimate    ! Estimated temperature out of the compressor, used to flag whether heat reclaim is reasonable, C
  REAL(r64)   :: TempInRated         ! Temperature entering compressor at rated superheat, C
  REAL(r64)   :: TotalEnthalpyChangeActual ! Actual enthalpy change in cases and cold side of LSHX, J/kg
  REAL(r64)   :: TsatforPsuct            ! Tsat for Psuction, C
  REAL(r64)   :: TsatforPdisch           ! Tsat for Pdischarge, c
  INTEGER     :: StageIndex              ! Compression stage index
  INTEGER     :: NumComps                ! Number of low-stage or high-stage compressors in system
  REAL(r64)   :: HHiStageCompIn          ! Enthalpy at inlet of high-stage compressor (J/kg)

  LocalTimeStep = TimeStepZone
  IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

  CondID    = System(SysNum)%CondenserNum(1)
  AccumLoad = MAX(0.d0,(System(SysNum)%UnmetEnergy/LocalTimeStep/SecInHour))

  !Before dispatching compressors, zero sum of compressor outputs and zero each compressor
  System(SysNum)%TotCompCapacity  = 0.d0
  System(SysNum)%RefMassFlowComps = 0.d0
  System(SysNum)%TotCompPower     = 0.d0
  IF(System(SysNum)%NumStages==2) THEN
    System(SysNum)%TotHiStageCompCapacity  = 0.d0
    System(SysNum)%RefMassFlowHiStageComps = 0.d0
    System(SysNum)%TotHiStageCompPower     = 0.d0
  END IF

  DO CompIndex=1,System(SysNum)%NumCompressors
    CompID=System(SysNum)%Compressornum(CompIndex)
    Compressor(CompID)%Power    = 0.d0
    Compressor(CompID)%MassFlow = 0.d0
    Compressor(CompID)%Capacity = 0.d0
    Compressor(CompID)%ElecConsumption = 0.d0
    Compressor(CompID)%CoolingEnergy   = 0.d0
    Compressor(CompID)%LoadFactor      = 0.d0
  END DO
  IF(System(SysNum)%NumStages==2) THEN
    DO CompIndex=1,System(SysNum)%NumHiStageCompressors
      CompID=System(SysNum)%HiStageCompressorNum(CompIndex)
      Compressor(CompID)%Power    = 0.d0
      Compressor(CompID)%MassFlow = 0.d0
      Compressor(CompID)%Capacity = 0.d0
      Compressor(CompID)%ElecConsumption = 0.d0
      Compressor(CompID)%CoolingEnergy   = 0.d0
      Compressor(CompID)%LoadFactor      = 0.d0
    END DO
  END IF

! Determine properties at case inlet and compressor inlet
stageloop:  DO StageIndex=1,2
    IF (StageIndex==2 .AND. System(SysNum)%NumStages==1) THEN
      EXIT stageloop  ! don't need to do two-stage calculations for a single-stage system
    END IF
    IF (StageIndex==1) THEN  ! Do single-stage or low-stage calculations
      IF (System(SysNum)%NumStages==1) THEN  ! Single-stage system
        NeededCapacity = System(SysNum)%TotalSystemLoad + AccumLoad + &
                         System(SysNum)%PipeHeatLoad + System(SysNum)%LSHXTrans  !because compressor capacity rated from txv to comp inlet
        TsatforPdisch = System(Sysnum)%TCondense + DelTDischPipes  !need (Psat of (Tcond + delT corresponding to delP disch Pipes))
        TsatforPsuct = System(Sysnum)%TEvapNeeded - DelTSuctPipes  !need (Psat of (Tevap - delT corresponding to del P suct Pipes))
        HsatVaporforTevapneeded = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(Sysnum)%TEvapNeeded, &
                                  1.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
        System(SysNum)%HSatLiqCond = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,0.0d0,&
                                     System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
        System(SysNum)%CpSatLiqCond = GetSatSpecificHeatRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,&
                                      0.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
        !HCaseIn is a function of the condenser rated subcooling, not the compressor rated subcooling
        !TCompIn needs to include case superheat as well as Temp change from lshx subcoolers
        !Calculate both here unless set previously by subcooler subroutine
        !HCaseOut corresponds to (tevapneeded + case superheat)
        !future - visit how parameter 'casesuperheat' applies when using walk-ins or transfer loads
        IF(System(SysNum)%NumSubcoolers == 0) THEN  ! No subcooler on this system
          System(SysNum)%HCaseIn = System(SysNum)%HSatLiqCond - System(SysNum)%CpSatLiqCond* &
                                   Condenser(System(SysNum)%CondenserNum(1))%RatedSubcool
          System(SysNum)%TCompIn = System(SysNum)%TEvapNeeded + CaseSuperheat !+
          System(SysNum)%TLiqInActual = System(Sysnum)%TCondense-Condenser(System(Sysnum)%CondenserNum(1))%RatedSubcool
          System(SysNum)%HCompIn = System(SysNum)%HCaseOut
        ELSE  !subcooler subroutine has been called to calc TCompIn and HCaseIn
          System(SysNum)%HCompIn = System(SysNum)%HCaseOut + System(SysNum)%CpSatVapEvap * &
                                   (System(SysNum)%TCompIn-(System(Sysnum)%TEvapNeeded+CaseSuperheat))
        END IF ! whether or not subcooler routine used
        Psuction = GetSatPressureRefrig(System(SysNum)%RefrigerantName,TsatforPsuct,  &
                   System(SysNum)%RefIndex,'RefrigeratedCase:CalcCompressors')
        NumComps = System(SysNum)%NumCompressors
      ELSE  ! Low-stage side of two-stage system
        Pcond = GetSatPressureRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense, &
                System(SysNum)%RefIndex, 'RefrigeratedCase:CalculateCompressors')
        Pevap = GetSatPressureRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TEvapNeeded, &
                System(SysNum)%RefIndex, 'RefrigeratedCase:CalculateCompressors')
        System(SysNum)%PIntercooler = SQRT(Pcond*Pevap)
        System(SysNum)%TIntercooler = GetSatTemperatureRefrig(System(SysNum)%RefrigerantName,System(SysNum)%PIntercooler, &
                                System(SysNum)%RefIndex, 'RefrigeratedCase:CalculateCompressors')
        NeededCapacity = System(SysNum)%TotalSystemLoad + AccumLoad + &
                         System(SysNum)%PipeHeatLoad + System(SysNum)%LSHXTrans  !because compressor capacity rated from txv to comp inlet
        TsatforPdisch = System(SysNum)%TIntercooler + DelTDischPipes  !need (Psat of (Tinter + delT corresponding to delP disch Pipes))
        TsatforPsuct = System(Sysnum)%TEvapNeeded - DelTSuctPipes  !need (Psat of (Tevap - delT corresponding to del P suct Pipes))
        HsatVaporforTevapneeded = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(Sysnum)%TEvapNeeded, &
                                  1.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
        System(SysNum)%HSatLiqCond = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,0.0d0,&
                                     System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
        System(SysNum)%CpSatLiqCond = GetSatSpecificHeatRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,&
                                      0.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
        !HCaseIn is a function of the condenser rated subcooling, not the compressor rated subcooling
        !TCompIn needs to include case superheat as well as Temp change from lshx subcoolers
        !Calculate both here unless set previously by subcooler subroutine
        !HCaseOut corresponds to (tevapneeded + case superheat)
        IF(System(SysNum)%NumSubcoolers == 0) THEN  ! No subcooler on this system
          IF(System(SysNum)%IntercoolerType==1) THEN       ! Flash Intercooler
            System(SysNum)%HCaseIn = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TIntercooler,0.0d0,&
                                     System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
            System(SysNum)%TLiqInActual = System(Sysnum)%TIntercooler
          ELSE IF(System(SysNum)%IntercoolerType==2) THEN  ! Shell-and-Coil Intercooler
            System(SysNum)%TLiqInActual = System(SysNum)%TCondense - Condenser(System(Sysnum)%CondenserNum(1))%RatedSubcool- &
                                          System(SysNum)%IntercoolerEffectiveness*(System(SysNum)%TCondense - &
                                          Condenser(System(Sysnum)%CondenserNum(1))%RatedSubcool - System(SysNum)%TIntercooler)
            System(SysNum)%HCaseIn = System(SysNum)%HSatLiqCond - System(SysNum)%CpSatLiqCond*(System(SysNum)%TCondense - &
                                     System(SysNum)%TLiqInActual)
          END IF  ! IntercoolerType
          System(SysNum)%TCompIn = System(SysNum)%TEvapNeeded + CaseSuperheat !+
          System(SysNum)%HCompIn = System(SysNum)%HCaseOut
        ELSE  !subcooler subroutine has been called to calc TCompIn and HCaseIn
          System(SysNum)%HCompIn = System(SysNum)%HCaseOut + System(SysNum)%CpSatVapEvap * &
                                   (System(SysNum)%TCompIn-(System(Sysnum)%TEvapNeeded+CaseSuperheat))
        END IF ! whether or not subcooler routine used
        Psuction = GetSatPressureRefrig(System(SysNum)%RefrigerantName,TsatforPsuct,  &
                   System(SysNum)%RefIndex,'RefrigeratedCase:CalcCompressors')
        NumComps = System(SysNum)%NumCompressors
      END IF  ! NumStages
    ELSE  ! Two-stage system, high-stage side
      NeededCapacity = System(SysNum)%TotalSystemLoad + AccumLoad + &
                       System(SysNum)%PipeHeatLoad + System(SysNum)%LSHXTrans + System(SysNum)%TotCompPower
      TsatforPdisch = System(Sysnum)%TCondense + DelTDischPipes
      TsatforPsuct = System(SysNum)%TIntercooler
      HsatVaporforTevapneeded = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(Sysnum)%TIntercooler, &
                                1.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
      System(SysNum)%HSatLiqCond = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,0.0d0,&
                                   System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
      System(SysNum)%CpSatLiqCond = GetSatSpecificHeatRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,&
                                    0.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
      System(SysNum)%HCaseIn = System(SysNum)%HSatLiqCond - System(SysNum)%CpSatLiqCond* &
                               Condenser(System(SysNum)%CondenserNum(1))%RatedSubcool
      System(SysNum)%TCompIn = System(SysNum)%TIntercooler
!      System(SysNum)%TLiqInActual = System(Sysnum)%TCondense-Condenser(System(Sysnum)%CondenserNum(1))%RatedSubcool
      System(SysNum)%HCompIn = HsatVaporforTevapneeded
      Psuction = GetSatPressureRefrig(System(SysNum)%RefrigerantName,TsatforPsuct,  &
                 System(SysNum)%RefIndex,'RefrigeratedCase:CalcCompressors')
      NumComps = System(SysNum)%NumHiStageCompressors
    END IF  ! StageIndex

    !dispatch compressors to meet load, note they were listed in compressor list in dispatch order
    DO CompIndex=1,NumComps
      IF (StageIndex==1) THEN
        CompID=System(SysNum)%CompressorNum(CompIndex)
      ELSE
        CompID=System(SysNum)%HiStageCompressorNum(CompIndex)
      END IF  ! StageIndex

      !need to use indiv compressor's rated subcool and superheat to adjust capacity to actual conditions
      SELECT CASE (Compressor(CompID)%SubcoolRatingType)
        CASE (RatedSubcooling)
          IF(System(SysNum)%NumStages==1) THEN  ! Single-stage system
            HCaseInRated = System(SysNum)%HSatLiqCond -  &
                           System(SysNum)%CpSatLiqCond*Compressor(CompID)%RatedSubcool
          ELSE IF(System(SysNum)%NumStages==2 .AND. StageIndex==1) THEN  ! Two-stage system, low-stage side
            HCaseInRated = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TIntercooler, &
                           0.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalcCompressors') - &
                           System(SysNum)%CpSatLiqCond*Compressor(CompID)%RatedSubcool
          ELSE IF(System(SysNum)%NumStages==2 .AND. StageIndex==2) THEN  ! Two-stage system, high-stage side
            HCaseInRated = System(SysNum)%HSatLiqCond -  &
                           System(SysNum)%CpSatLiqCond*Compressor(CompID)%RatedSubcool
          END IF  ! NumStages
        CASE (RatedLiquidTemperature)  !have rated liquid temperature stored in "RatedSubcool"
          IF(System(SysNum)%NumStages==1) THEN  ! Single-stage system
            HCaseInRated = System(SysNum)%HSatLiqCond -  &
                           System(SysNum)%CpSatLiqCond*(System(Sysnum)%TCondense-Compressor(CompID)%RatedSubcool)
          ELSE IF(System(SysNum)%NumStages==2 .AND. StageIndex==1) THEN  ! Two-stage system, low-stage side
            HCaseInRated = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TIntercooler, &
                           0.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalcCompressors') - &
                           System(SysNum)%CpSatLiqCond*(System(Sysnum)%TIntercooler-Compressor(CompID)%RatedSubcool)
          ELSE IF(System(SysNum)%NumStages==2 .AND. StageIndex==2) THEN  ! Two-stage system, high-stage side
            HCaseInRated = System(SysNum)%HSatLiqCond -  &
                           System(SysNum)%CpSatLiqCond*(System(Sysnum)%TCondense-Compressor(CompID)%RatedSubcool)
          END IF  ! NumStages
      END SELECT  ! Compressor SubcoolRatingType
      SELECT CASE(Compressor(CompID)%SuperheatRatingType)
        CASE (RatedSuperheat)
          IF(System(SysNum)%NumStages==1) THEN  ! Single-stage system
              HCompInRated = HsatVaporforTevapneeded + System(SysNum)%CpSatVapEvap*Compressor(CompID)%RatedSuperheat
              TempInRated = System(Sysnum)%TEvapNeeded + Compressor(CompID)%RatedSuperheat
          ELSE IF(System(SysNum)%NumStages==2 .AND. StageIndex==1) THEN  ! Two-stage system, low-stage side
              HCompInRated = HsatVaporforTevapneeded + System(SysNum)%CpSatVapEvap*Compressor(CompID)%RatedSuperheat
              TempInRated = System(Sysnum)%TEvapNeeded + Compressor(CompID)%RatedSuperheat
          ELSE IF(System(SysNum)%NumStages==2 .AND. StageIndex==2) THEN  ! Two-stage system, high-stage side
              HCompInRated = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TIntercooler, &
                             1.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalcCompressors') + &
                             System(SysNum)%CpSatVapEvap*Compressor(CompID)%RatedSuperheat
              TempInRated = System(SysNum)%TIntercooler + Compressor(CompID)%RatedSuperheat
          END IF  ! NumStages
        CASE (RatedReturnGasTemperature)  !have rated compressor inlet temperature stored in "RatedSuperheat"
          IF(System(SysNum)%NumStages==1) THEN  ! Single-stage system
              TempInRated = Compressor(CompID)%RatedSuperheat
              HCompInRated = HsatVaporforTevapneeded +     &
                             System(SysNum)%CpSatVapEvap*(TempInRated-System(Sysnum)%TEvapNeeded)
          ELSE IF(System(SysNum)%NumStages==2 .AND. StageIndex==1) THEN  ! Two-stage system, low-stage side
              TempInRated = Compressor(CompID)%RatedSuperheat
              HCompInRated = HsatVaporforTevapneeded +     &
                             System(SysNum)%CpSatVapEvap*(TempInRated-System(Sysnum)%TEvapNeeded)
          ELSE IF(System(SysNum)%NumStages==2 .AND. StageIndex==2) THEN  ! Two-stage system, high-stage side
              TempInRated = Compressor(CompID)%RatedSuperheat
              HCompInRated = HsatVaporforTevapneeded +     &
                             System(SysNum)%CpSatVapEvap*(TempInRated-System(Sysnum)%TIntercooler)
          END IF  ! NumStages
      END SELECT  ! Compressor SuperheatRatingType

      DensityActual=GetSupHeatDensityRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCompIn,&
                    PSuction,System(SysNum)%RefIndex,'RefrigeratedCase:CalcCompressors')
      TotalEnthalpyChangeActual=System(SysNum)%HCompIn-System(SysNum)%HCaseIn

      CaseEnthalpyChangeRated=HCompInRated-HCaseInRated
      DensityRated=GetSupHeatDensityRefrig(System(SysNum)%RefrigerantName,TempInRated,&
                   PSuction,System(SysNum)%RefIndex,'RefrigeratedCase:CalcCompressors')
      !  Adjust capacity and mass flow to reflect the specific volume change due to superheating and
      !  the increase in capacity due to extra subcooling
      MassCorrection = DensityActual/DensityRated
      CapacityCorrection = MassCorrection*TotalEnthalpyChangeActual/CaseEnthalpyChangeRated
      Compressor(CompID)%Power = CurveValue(Compressor(CompID)%ElecPowerCurvePtr,TsatforPsuct,TsatforPdisch)
      Compressor(CompID)%Capacity = CapacityCorrection*CurveValue(Compressor(CompID)%CapacityCurvePtr,TsatforPsuct,TsatforPdisch)
      Compressor(CompID)%MassFlow = Compressor(CompID)%Capacity/TotalEnthalpyChangeActual
      Compressor(CompID)%ElecConsumption = Compressor(CompID)%Power * LocalTimeStep * SecInHour
      Compressor(CompID)%CoolingEnergy   = Compressor(CompID)%Capacity * LocalTimeStep * SecInHour
      Compressor(CompID)%LoadFactor      = 1.d0

      ! calculate load factor for last compressor addded
      ! assumes either cycling or part load eff = full load eff for last compressor
      IF (StageIndex==1) THEN  ! Single-stage or low-stage compressors
        IF ((System(SysNum)%TotCompCapacity + Compressor(CompID)%Capacity) >= NeededCapacity) THEN
          LFLastComp=(NeededCapacity-System(SysNum)%TotCompCapacity)/Compressor(CompID)%Capacity
          Compressor(CompID)%Power = LFLastComp*Compressor(CompID)%Power
          Compressor(CompID)%MassFlow = LFLastComp*Compressor(CompID)%MassFlow
          Compressor(CompID)%Capacity = LFLastComp*Compressor(CompID)%Capacity
          System(SysNum)%TotCompCapacity = System(SysNum)%TotCompCapacity + Compressor(CompID)%Capacity
          System(SysNum)%RefMassFlowComps = System(SysNum)%RefMassFlowComps + Compressor(CompID)%MassFlow
          System(SysNum)%TotCompPower = System(SysNum)%TotCompPower + Compressor(CompID)%Power
          Compressor(CompID)%ElecConsumption = Compressor(CompID)%Power * LocalTimeStep * SecInHour
          Compressor(CompID)%CoolingEnergy   = Compressor(CompID)%Capacity * LocalTimeStep * SecInHour
          Compressor(CompID)%LoadFactor      = LFLastComp
          EXIT !numcomps do
        ELSE !>= needed capacity
          System(SysNum)%TotCompCapacity = System(SysNum)%TotCompCapacity + Compressor(CompID)%Capacity
          System(SysNum)%RefMassFlowComps = System(SysNum)%RefMassFlowComps + Compressor(CompID)%MassFlow
          System(SysNum)%TotCompPower = System(SysNum)%TotCompPower + Compressor(CompID)%Power
        END IF !>= needed capacity
      ELSE  ! high-stage compressors (for two-stage systems only)
        IF ((System(SysNum)%TotHiStageCompCapacity + Compressor(CompID)%Capacity) >= &
              NeededCapacity) THEN
          LFLastComp=(NeededCapacity-System(SysNum)%TotHiStageCompCapacity)/&
                     Compressor(CompID)%Capacity
          Compressor(CompID)%Power = LFLastComp*Compressor(CompID)%Power
          Compressor(CompID)%MassFlow = LFLastComp*Compressor(CompID)%MassFlow
          Compressor(CompID)%Capacity = LFLastComp*Compressor(CompID)%Capacity
          System(SysNum)%TotHiStageCompCapacity = System(SysNum)%TotHiStageCompCapacity + Compressor(CompID)%Capacity
          System(SysNum)%RefMassFlowHiStageComps = System(SysNum)%RefMassFlowHiStageComps + &
                                                   Compressor(CompID)%MassFlow
          System(SysNum)%TotHiStageCompPower = System(SysNum)%TotHiStageCompPower + Compressor(CompID)%Power
          System(SysNum)%FlowRatioIntercooler = System(SysNum)%RefMassFlowComps/ &
                                                System(SysNum)%RefMassFlowHiStageComps
          Compressor(CompID)%ElecConsumption = Compressor(CompID)%Power * LocalTimeStep * SecInHour
          Compressor(CompID)%CoolingEnergy   = Compressor(CompID)%Capacity * LocalTimeStep * SecInHour
          Compressor(CompID)%LoadFactor      = LFLastComp
          EXIT !numcomps do
        ELSE !>= needed capacity
          System(SysNum)%TotHiStageCompCapacity = System(SysNum)%TotHiStageCompCapacity + Compressor(CompID)%Capacity
          System(SysNum)%RefMassFlowHiStageComps = System(SysNum)%RefMassFlowHiStageComps + Compressor(CompID)%MassFlow
          System(SysNum)%TotHiStageCompPower = System(SysNum)%TotHiStageCompPower + Compressor(CompID)%Power
        END IF !>= needed capacity
      END IF ! StageIndex
    END DO ! NumComps
  END DO stageloop  ! StageIndex

  !Calculate enthalpy at compressor discharge
  IF (System(SysNum)%NumStages==1) THEN  ! Single-stage or low-stage compressors
    System(SysNum)%HCompOut=System(SysNum)%HCompIn + &
         System(SysNum)%TotCompPower/System(SysNum)%RefMassFlowComps
         !error found 9/19/2011, was System(SysNum)%TotCompPower*LocalTimeStep*SecInHour/System(SysNum)%RefMassFlowComps
  ELSE  ! High-stage compressors (only for two-stage systems)
    HHiStageCompIn=GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TIntercooler,1.0d0,&
            System(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
    System(SysNum)%HCompOut=HHiStageCompIn + System(SysNum)%TotHiStageCompPower/System(SysNum)%RefMassFlowHiStageComps
  END IF

  !Calculate superheat energy available for desuperheaters
  HSatVapCondense=GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(Sysnum)%TCondense, &
                             1.0d0,System(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
  CpSatVapCondense=GetSatSpecificHeatRefrig(System(SysNum)%RefrigerantName,System(Sysnum)%TCondense, &
                             1.0d0,System(SysNum)%RefIndex,'SimulateDetailedRefrigerationSystems')
  IF (System(SysNum)%NumStages==1) THEN  ! Single-stage systems
    HeatReclaimRefrigCondenser(CondID)%AvailCapacity = System(SysNum)%RefMassFlowComps * &
                               (System(SysNum)%HCompOut - HSatVapCondense)
  ELSE  ! Two-stage systems
    HeatReclaimRefrigCondenser(CondID)%AvailCapacity = System(SysNum)%RefMassFlowHiStageComps * &
                               (System(SysNum)%HCompOut - HSatVapCondense)
  END IF  ! NumStages

  !No function available to get Tout as f(Pout, Hout), so use estimate based on constant cp in superheat range...
  !  Use average of Tcondense and Tout of condenser as check for whether heat reclaim is reasonable.
  TCompOutEstimate = System(Sysnum)%TCondense + (System(SysNum)%HCompOut - HSatVapCondense)/CpSatVapCondense

  HeatReclaimRefrigCondenser(CondID)%AvailTemperature = (TsatforPdisch + TCompOutEstimate)/2.d0
  System(SysNum)%AverageCompressorCOP = System(SysNum)%TotCompCapacity/ &
                                        (System(SysNum)%TotCompPower+System(SysNum)%TotHiStageCompPower)
  System(SysNum)%TotCompElecConsump   = System(SysNum)%TotCompPower * LocalTimeStep * SecInHour
  IF(System(SysNum)%NumStages==2)THEN
    System(SysNum)%TotHiStageCompElecConsump = System(SysNum)%TotHiStageCompPower * LocalTimeStep * SecInHour
    System(SysNum)%TotCompElecConsumpTwoStage = System(SysNum)%TotCompElecConsump + System(SysNum)%TotHiStageCompElecConsump
  END IF
  System(SysNum)%TotCompCoolingEnergy = System(SysNum)%TotCompCapacity * LocalTimeStep * SecInHour
  System(SysNum)%TotHiStageCompCoolingEnergy = System(SysNum)%TotHiStageCompCapacity * LocalTimeStep * SecInHour

RETURN
END SUBROUTINE CalculateCompressors

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalculateTransCompressors(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brian A. Fricke, ORNL
          !       DATE WRITTEN   Fall 2011
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the compressor power, energy, capacity, and efficiency for a detailed transcritical CO2
          ! refrigeration system.

          ! METHODOLOGY EMPLOYED:
          ! Use AHRI compressor performance curves for subcritical compressor operation, AHRI-style compressor
          ! performance curves for transcritical compressor operation, the evaporating temperature of the
          ! medium- and low-temperature loads, and the gas cooler outlet conditions (temperature, pressure
          ! and enthalpy).

          ! REFERENCES:
          ! ANSI/AHRI. 2004. Standard 540, Standard for Performance Rating of Positive Displacement Refrigerant
          !     Comprssors and Compressor Units. Arlington, VA: Air-Conditioning, Heating, and Refrigeration
          !     Institute.
          ! Ge, Y.T., and S.A. Tassou. 2011. Performance evaluation and optimal design of supermarket refrigeration
          !     systems with supermarket model "SuperSim", Part I: Model description and validation. International
          !     Journal of Refrigeration 34: 527-539.
          ! Ge, Y.T., and S.A. Tassou. 2011. Performance evaluation and optimal design of supermarket refrigeration
          !     systems with supermarket model "SuperSim", Part II: Model applications. International Journal of
          !     Refrigeration 34: 540-549.
          ! Sawalha, S. 2008. Theoretical evaluation of trans-critical CO2 systems in supermarket refrigeration,
          !     Part I: Modeling, simulation and optimization of two system solutions. International Journal of
          !     Refrigeration 31: 516-524.
          ! Sawalha, S. 2008. Theoretical evaluation of trans-critical CO2 systems in supermarket refrigeration,
          !     Part II: System modifications and comparisons of different solutions. International Journal of
          !     Refrigeration 31: 525-534.

          ! USE STATEMENTS:
  USE CurveManager,      ONLY : CurveValue
!unused  USE DataEnvironment,   ONLY : OutDryBulbTemp

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
! Following constants approp for R22, R134a, R404a, R507, R410a, R407c.
! For the same pressure drop, CO2 has a corresponding temperature penalty 5 to 10 times smaller than
! ammonia and R-134a (ASHRAE Handbook of Refrigeration, 2010, p. 3.7).  Ignore pressure drop for CO2 calculations.
! NOTE, these DelT...Pipes reflect the decrease in Pressure in the pipes, NOT thermal transfer through the pipe walls.
!REAL(r64), PARAMETER ::DelTSuctPipes  = 1.0d0 ! Tsat drop corresponding to P drop in suction pipes, ASHRAE 2006 p 2.4 (C)
!REAL(r64), PARAMETER ::DelTDischPipes = 0.5d0 ! Tsat drop corresponding to P drop in discharge pipes, ASHRAE 2006 p 2.5 (C)
REAL(r64), PARAMETER ::ErrorTol       = 0.001d0     ! Iterative solution tolerance

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: CompIndex                    ! Compressor index within system
  INTEGER     :: CompID                       ! Compressor index within all compressors
  INTEGER     :: GasCoolerID                  ! Gas cooler index for this refrigeration system
  INTEGER     :: Iter                         ! Iteration counter
  REAL(r64)   :: AccumLoadMT = 0.0d0            ! Load due to previously unmet medium temperature compressor loads (transcritical system)
  REAL(r64)   :: AccumLoadLT = 0.0d0            ! Load due to previously unmet low temperature compressor loads (transcritical system)
  REAL(r64)   :: CapacityCorrectionLT         ! Capacity at existing subcool/superheat over cap at rated conditions for LT loads
  REAL(r64)   :: CapacityCorrectionMT         ! Capacity at existing subcool/superheat over cap at rated conditions for MT loads
  REAL(r64)   :: CaseEnthalpyChangeRatedMT    ! Enthalpy change in medium temperature cases at compressor rated cond, J/kg
  REAL(r64)   :: CaseEnthalpyChangeRatedLT    ! Enthalpy change in low temperature cases at compressor rated cond, J/kg
  REAL(r64)   :: DensityActualLT              ! Density of superheated gas at LP compressor inlet, m3/kg
  REAL(r64)   :: DensityActualMT              ! Density of superheated gas at HP compressor inlet, m3/kg
  REAL(r64)   :: DensityRatedHP               ! Density of high pressure compressor inlet gas at rated superheat, m3/kg
  REAL(r64)   :: DensityRatedLP               ! Density of low pressure compressor inlet gas at rated superheat, m3/kg
  REAL(r64)   :: HCaseInRatedLT               ! Enthalpy entering low temperature cases at rated subcooling, J/kg
  REAL(r64)   :: HCaseInRatedMT               ! Enthalpy entering medium temperature cases at rated subcooling, J/kg
  REAL(r64)   :: HCaseOutLTMT = 0.0d0           ! Combined enthalpy from the outlets of the LP compressor and MT loads, J/kg
  REAL(r64)   :: HCompInRatedHP               ! Enthalpy entering high pressure compressor at rated superheat, J/kg
  REAL(r64)   :: HCompInRatedLP               ! Enthalpy entering low pressure compressor at rated superheat, J/kg
  REAL(r64)   :: HGCOutlet                    ! Enthalpy at gas cooler outlet, J/kg
  REAL(R64)   :: HIdeal                       ! Ideal enthalpy at subcooler (for 100% effectiveness)
  REAL(r64)   :: Hnew                         ! Calucalted enthalpy, J/kg
  REAL(r64)   :: HReceiverBypass = 0.0d0        ! Enthalpy at the receiver bypass, J/kg
  REAL(r64)   :: HsatLiqforTevapNeededMT      ! Enthalpy of saturated liquid at MT evaporator, J/kg
  REAL(r64)   :: HsatVaporforTevapneededMT    ! Enthlapy of saturated vapor at MT evaporator (transcritical cycle), J/kg
  REAL(r64)   :: HsatVaporforTevapneededLT    ! Enthlapy of saturated vapor at LT evaporator (transcritical cycle), J/kg
  REAL(r64)   :: LFLastComp                   ! Load factor for last compressor dispatched
  REAL(r64)   :: LocalTimeStep = 0.0d0          ! TimeStepZone for case/walkin systems, TimeStepSys for coil systems
  REAL(r64)   :: MassCorrectionLT             ! Mass flow at existing subcool/superheat over cap at rated conditions for LT loads
  REAL(r64)   :: MassCorrectionMT             ! Mass flow at existing subcool/superheat over cap at rated conditions for MT loads
  REAL(r64)   :: NeededCapacityLT             ! Sum of LT case loads and mech subcooler loads (transcritical cycle), W
  REAL(r64)   :: NeededCapacityMT             ! Sum of MT case loads and mech subcooler loads (transcritical cycle), W
  REAL(r64)   :: PsuctionLT                   ! Suction pressure in low temperature cases, Pa
  REAL(r64)   :: PsuctionMT                   ! Suction pressure in medium temperature cases, Pa
  REAL(r64)   :: PGCOutlet                    ! Gas cooler outlet pressure, Pa
  REAL(r64)   :: QualityReceiver              ! Refrigerant quality in the receiver
  REAL(r64)   :: SubCoolEffect                ! Heat exchanger effectiveness of the subcooler
  REAL(r64)   :: TempInRatedHP                ! Temperature entering high pressure compressor at rated superheat, C
  REAL(r64)   :: TempInRatedLP                ! Temperature entering low pressure compressor at rated superheat, C
  REAL(r64)   :: TsatforPdisLT                ! Low temperature saturated discharge temperature (transcritical cycle), C
  REAL(r64)   :: TsatforPdisMT                ! Medium temperature saturated discharge temperature (transcritical cycle), C
  REAL(r64)   :: TsatforPsucLT                ! Low temperature saturated suction temperature (transcritical cycle), C
  REAL(r64)   :: TsatforPsucMT                ! Medium temperature saturated suction temperature (transcritical cycle), C
  REAL(r64)   :: TSubCoolerColdIn             ! Suction gas temperature at the inlet of the subcooler, C
  REAL(r64)   :: TotalEnthalpyChangeActualLT  ! Actual enthalpy change in LT cases, J/kg
  REAL(r64)   :: TotalEnthalpyChangeActualMT  ! Actual enthalpy change in MT cases, J/kg
  REAL(r64)   :: TotalRefMassFlow             ! Total mass flow through high pressure side of system, kg/s
  REAL(r64)   :: Xu                           ! Initial upper guess for iterative search
  REAL(r64)   :: Xl                           ! Initial lower guess for iterative search
  REAL(r64)   :: Xnew                         ! New guess for iterative search


  LocalTimeStep = TimeStepZone
  IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

  GasCoolerID = TransSystem(SysNum)%GasCoolerNum(1)

  ! Determine refrigerating capacity needed
  AccumLoadLT = 0.0d0
  NeededCapacityLT = 0.0d0
  IF (TransSystem(SysNum)%TransSysType == 2) THEN
    AccumLoadLT = MAX(0.0d0,(TransSystem(SysNum)%UnmetEnergyLT/LocalTimeStep/SecInHour))
    NeededCapacityLT = TransSystem(SysNum)%TotalSystemLoadLT + AccumLoadLT + TransSystem(SysNum)%PipeHeatLoadLT
  END IF  ! (TransSystem(SysNum)%TransSysType == 2)
  AccumLoadMT = MAX(0.0d0,(TransSystem(SysNum)%UnmetEnergyMT/LocalTimeStep/SecInHour))
  NeededCapacityMT = TransSystem(SysNum)%TotalSystemLoadMT + AccumLoadMT + TransSystem(SysNum)%PipeHeatLoadMT

  ! Determine refrigerant properties at receiver
  TransSystem(SysNum)%CpSatLiqReceiver =   &
     GetSatSpecificHeatRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(SysNum)%TReceiver,&
                              0.0d0,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
  HReceiverByPass = GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(Sysnum)%TReceiver, &
                    1.0d0,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')

  ! Determine refrigerant properties at low temperature (LT) loads (if present)
  ! Dispatch low pressure (LP) compressors as necessary
  IF (TransSystem(SysNum)%TransSysType == 2) THEN  ! LT side of TwoStage transcritical system
     TransSystem(SysNum)%HCaseInLT = TransSystem(SysNum)%HSatLiqReceiver
  ! TCompInLP and HCompInLP include case superheat plus effect of suction line heat gain
     TransSystem(SysNum)%TCompInLP = TransSystem(SysNum)%TEvapNeededLT + TransCaseSuperheat + TransSystem(SysNum)%PipeHeatLoadLT &
                                     /(TransSystem(SysNum)%CpSatVapEvapLT*TransSystem(SysNum)%RefMassFlowtoLTLoads)
     TransSystem(SysNum)%HCompInLP = TransSystem(SysNum)%HCaseOutLT + TransSystem(SysNum)%PipeHeatLoadLT &
                                     /TransSystem(SysNum)%RefMassFlowtoLTLoads
     TsatforPsucLT = TransSystem(SysNum)%TEvapNeededLT
     TsatforPdisLT = TransSystem(SysNum)%TEvapNeededMT
     HsatVaporforTevapneededLT = GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(Sysnum)%TEvapNeededLT, &
                                 1.0d0,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
     HsatLiqforTevapNeededMT = GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(Sysnum)%TEvapNeededMT, &
                               0.0d0,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
     PsuctionLT = GetSatPressureRefrig(TransSystem(SysNum)%RefrigerantName,TsatforPsucLT, &
                  TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
     DensityActualLT = GetSupHeatDensityRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(SysNum)%TCompInLP, &
                       PsuctionLT,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
     TotalEnthalpyChangeActualLT=TransSystem(SysNum)%HCompInLP-TransSystem(SysNum)%HCaseInLT

     !Dispatch low pressure (LP) compressors
     !Before dispatching LP compressors, zero sum of compressor outputs and zero each compressor
     TransSystem(SysNum)%TotCompCapacityLP  = 0.d0
     TransSystem(SysNum)%RefMassFlowCompsLP = 0.d0
     TransSystem(SysNum)%TotCompPowerLP     = 0.d0

     DO CompIndex=1,TransSystem(SysNum)%NumCompressorsLP
        CompID=TransSystem(SysNum)%CompressornumLP(CompIndex)
        Compressor(CompID)%Power    = 0.d0
        Compressor(CompID)%MassFlow = 0.d0
        Compressor(CompID)%Capacity = 0.d0
        Compressor(CompID)%ElecConsumption = 0.d0
        Compressor(CompID)%CoolingEnergy   = 0.d0
        Compressor(CompID)%LoadFactor      = 0.d0
     END DO

     DO CompIndex=1,TransSystem(SysNum)%NumCompressorsLP
        CompID=TransSystem(SysNum)%CompressornumLP(CompIndex)
        !need to use indiv compressor's rated subcool and superheat to adjust capacity to actual conditions
        SELECT CASE (Compressor(CompID)%SubcoolRatingType)
           CASE (RatedSubcooling)
             HCaseInRatedLT = HsatLiqforTevapNeededMT -  &
                              TransSystem(SysNum)%CpSatLiqReceiver*Compressor(CompID)%RatedSubcool
           CASE (RatedLiquidTemperature)  !have rated liquid temperature stored in "RatedSubcool"
              HCaseInRatedLT = GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,Compressor(CompID)%RatedSubcool, &
                               0.0d0,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
        END SELECT
        SELECT CASE(Compressor(CompID)%SuperheatRatingType)
           CASE (RatedSuperheat)
             HCompInRatedLP = HsatVaporforTevapneededLT + TransSystem(SysNum)%CpSatVapEvapLT*Compressor(CompID)%RatedSuperheat
             TempInRatedLP  = TransSystem(Sysnum)%TEvapNeededLT + Compressor(CompID)%RatedSuperheat
           CASE (RatedReturnGasTemperature)  !have rated compressor inlet temperature stored in "RatedSuperheat"
             TempInRatedLP  = Compressor(CompID)%RatedSuperheat
             HCompInRatedLP = GetSupHeatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,Compressor(CompID)%RatedSuperheat, &
                              PsuctionLT,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
        END SELECT

        CaseEnthalpyChangeRatedLT=HCompInRatedLP-HCaseInRatedLT
        DensityRatedLP = GetSupHeatDensityRefrig(TransSystem(SysNum)%RefrigerantName,TempInRatedLP,&
                         PSuctionLT,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')

        !  Adjust capacity and mass flow to reflect the specific volume change due to superheating and
        !  the increase in capacity due to extra subcooling
        MassCorrectionLT = DensityActualLT/DensityRatedLP
        CapacityCorrectionLT = MassCorrectionLT*TotalEnthalpyChangeActualLT/CaseEnthalpyChangeRatedLT
        Compressor(CompID)%Power    = CurveValue(Compressor(CompID)%ElecPowerCurvePtr,TsatforPsucLT,TsatforPdisLT)
        Compressor(CompID)%Capacity = CapacityCorrectionLT*  &
           CurveValue(Compressor(CompID)%CapacityCurvePtr,TsatforPsucLT,TsatforPdisLT)
        Compressor(CompID)%MassFlow = Compressor(CompID)%Capacity/TotalEnthalpyChangeActualLT
        Compressor(CompID)%ElecConsumption = Compressor(CompID)%Power * LocalTimeStep * SecInHour
        Compressor(CompID)%CoolingEnergy   = Compressor(CompID)%Capacity * LocalTimeStep * SecInHour
        Compressor(CompID)%LoadFactor      = 1.d0
        IF ((TransSystem(SysNum)%TotCompCapacityLP + Compressor(CompID)%Capacity) >= NeededCapacityLT) THEN
           LFLastComp = (NeededCapacityLT-TransSystem(SysNum)%TotCompCapacityLP)/Compressor(CompID)%Capacity
           Compressor(CompID)%Power    = LFLastComp*Compressor(CompID)%Power
           Compressor(CompID)%MassFlow = LFLastComp*Compressor(CompID)%MassFlow
           Compressor(CompID)%Capacity = LFLastComp*Compressor(CompID)%Capacity
           TransSystem(SysNum)%TotCompCapacityLP  = TransSystem(SysNum)%TotCompCapacityLP + Compressor(CompID)%Capacity
           TransSystem(SysNum)%RefMassFlowCompsLP = TransSystem(SysNum)%RefMassFlowCompsLP + Compressor(CompID)%MassFlow
           TransSystem(SysNum)%TotCompPowerLP     = TransSystem(SysNum)%TotCompPowerLP + Compressor(CompID)%Power
           Compressor(CompID)%ElecConsumption     = Compressor(CompID)%Power * LocalTimeStep * SecInHour
           Compressor(CompID)%CoolingEnergy       = Compressor(CompID)%Capacity * LocalTimeStep * SecInHour
           Compressor(CompID)%LoadFactor          = LFLastComp
           EXIT
        ELSE
           TransSystem(SysNum)%TotCompCapacityLP  = TransSystem(SysNum)%TotCompCapacityLP + Compressor(CompID)%Capacity
           TransSystem(SysNum)%RefMassFlowCompsLP = TransSystem(SysNum)%RefMassFlowCompsLP + Compressor(CompID)%MassFlow
           TransSystem(SysNum)%TotCompPowerLP     = TransSystem(SysNum)%TotCompPowerLP + Compressor(CompID)%Power
        END IF
    END DO ! NumCompressorsLP
    TransSystem(SysNum)%HCompOutLP = TransSystem(SysNum)%HCompInLP + &
                                     TransSystem(SysNum)%TotCompPowerLP/TransSystem(SysNum)%RefMassFlowCompsLP
  END IF ! (TransSystem(SysNum)%TransSysType == 2)

  ! Determine refrigerant properties at medium temperature (MT) loads
  ! Dispatch high pressure (HP) compressors as necessary
  TsatforPsucMT = TransSystem(SysNum)%TEvapNeededMT
  IF (GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%TransOpFlag) THEN  ! Transcritical system is operating in transcritical region
    HGCOutlet = GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%HGasCoolerOut
  ELSE  ! Transcritical system is operating in subcritical region
    TsatforPdisMT = GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%TGasCoolerOut
  END IF
  PsuctionMT = GetSatPressureRefrig(TransSystem(SysNum)%RefrigerantName,TsatforPsucMT, &
               TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
  PGCOutlet = GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%PGasCoolerOut
  HsatVaporforTevapneededMT = GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(Sysnum)%TEvapNeededMT, &
                              1.0d0,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculatetransCompressors')
  TransSystem(SysNum)%HCaseInMT = TransSystem(SysNum)%HSatLiqReceiver
  ! Enthalpy of refrigerant after leaving medium temperature loads and low pressure compressors
  HCaseOutLTMT = (TransSystem(SysNum)%RefMassFlowtoLTLoads*TransSystem(SysNum)%HCompOutLP + &
                 TransSystem(SysNum)%RefMassFlowtoMTLoads*TransSystem(SysNum)%HCaseOutMT + TransSystem(SysNum)%PipeHeatLoadMT)/ &
                 (TransSystem(SysNum)%RefMassFlowtoLTLoads + TransSystem(SysNum)%RefMassFlowtoMTLoads)

  ! Total refrigerant flow rate is total flow from LT and MT loads divided by (1-x) where x is the quality of the
  ! refrigerant entering the receiver.  The receiver bypass flow rate is (x)*(Total Flow).
  ! Iterate to find the quality of the refrigerant entering the receiver.
  Xu=1.0d0    ! upper bound on quality
  Xl=0.0d0    ! lower bound on quality
  IF ((GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%HGasCoolerOut + TransSystem(SysNum)%DelHSubCoolerDis) > &
       TransSystem(SysNum)%HSatLiqReceiver) THEN
    DO Iter=1,15  ! Maximum of 15 iterations to find receiver quality
      QualityReceiver=(Xu+Xl)/2.0d0
      Hnew = GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(SysNum)%TReceiver,QualityReceiver, &
             TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')

       ! estimated QualityReceiver is too high
      IF (Hnew > (GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%HGasCoolerOut + TransSystem(SysNum)%DelHSubCoolerDis)) THEN
        Xu=QualityReceiver
      ELSE ! estimated QualityReceiver is too low
        Xl=QualityReceiver
      END IF
      IF ( ABS((Hnew-(GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%HGasCoolerOut+ &
                TransSystem(SysNum)%DelHSubCoolerDis))/Hnew) < ErrorTol ) EXIT
    END DO
    TotalRefMassFlow = (TransSystem(SysNum)%RefMassFlowtoLTLoads + TransSystem(SysNum)%RefMassFlowtoMTLoads)/ &
                       (1.0d0-QualityReceiver)
    TransSystem(SysNum)%RefMassFlowReceiverByPass = QualityReceiver*TotalRefMassFlow
  ELSE
    TransSystem(SysNum)%RefMassFlowReceiverBypass = 0.0d0
    TotalRefMassFlow = (TransSystem(SysNum)%RefMassFlowtoLTLoads + TransSystem(SysNum)%RefMassFlowtoMTLoads)
  END IF  ! %HGasCoolerOut > TransSystem(SysNum)%HSatLiqReceiver)

  TransSystem(SysNum)%HCompInHP = (HCaseOutLTMT*(TransSystem(SysNum)%RefMassFlowtoLTLoads + &
                                  TransSystem(SysNum)%RefMassFlowtoMTLoads) + &
                                  HReceiverByPass*TransSystem(SysNum)%RefMassFlowReceiverBypass)/ &
                                  (TransSystem(SysNum)%RefMassFlowtoLTLoads + TransSystem(SysNum)%RefMassFlowtoMTLoads + &
                                  TransSystem(SysNum)%RefMassFlowReceiverBypass)

  ! Iterate to find the suction temperature entering subcooler
  Xl = GetSatTemperatureRefrig(TransSystem(SysNum)%RefrigerantName,PsuctionMT,TransSystem(SysNum)%RefIndex, &
       'RefrigeratedCase:CalculateTransCompressors')
  Xu = Xl + 50.0d0
  DO Iter=1,15  ! Maximum of 15 iterations
    Xnew=(Xu+Xl)/2.0d0
    Hnew=GetSupHeatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,Xnew,PsuctionMT,TransSystem(SysNum)%RefIndex, &
         'RefrigeratedCase:CalculateTransCompressors')
    IF (Hnew > TransSystem(SysNum)%HCompInHP) THEN ! xnew is too high
      Xu=Xnew
    ELSE ! xnew is too low
      Xl=Xnew
    END IF
    IF ( ABS((Hnew-TransSystem(SysNum)%HCompInHP)/Hnew) < ErrorTol ) EXIT
  END DO
  TSubCoolerColdIn = Xnew

  ! Modify receiver inlet enthlapy and HP compressor inlet enthalpy to account for subcooler
  HIdeal = GetSupHeatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName, &
                                    GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%TGasCoolerOut, &
                                    PsuctionMT,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
  ! Only use subcooler if suction gas inlet temperature less than gas cooler outlet temperature
  IF(TSubCoolerColdIn < GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%TGasCoolerOut) THEN
    SubCoolEffect = TransSystem(SysNum)%SCEffectiveness
  ELSE
    SubCoolEffect = 0.0d0
  END IF    ! (TSubCoolerColdIn < GasCooler(SysNum)%TGasCoolerOut)
  TransSystem(SysNum)%DelHSubcoolerSuc = SubCoolEffect*(HIdeal - TransSystem(SysNum)%HCompInHP)
  TransSystem(SysNum)%HCompInHP = TransSystem(SysNum)%HCompInHP + TransSystem(SysNum)%DelHSubcoolerSuc
  TransSystem(SysNum)%DelHSubcoolerDis = -TransSystem(SysNum)%DelHSubcoolerSuc

  ! Iterate to find the temperature at the inlet of the high pressure (HP) compressors
  Xl = GetSatTemperatureRefrig(TransSystem(SysNum)%RefrigerantName,PsuctionMT,TransSystem(SysNum)%RefIndex, &
       'RefrigeratedCase:CalculateTransCompressors')
  Xu = Xl + 50.0d0
  DO Iter=1,15  ! Maximum of 15 iterations
    Xnew=(Xu+Xl)/2.0d0
    Hnew=GetSupHeatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,Xnew,PsuctionMT,TransSystem(SysNum)%RefIndex, &
         'RefrigeratedCase:CalculateTransCompressors')
    IF (Hnew > TransSystem(SysNum)%HCompInHP) THEN ! xnew is too high
      Xu=Xnew
    ELSE ! xnew is too low
      Xl=Xnew
    END IF
    IF ( ABS((Hnew-TransSystem(SysNum)%HCompInHP)/Hnew) < ErrorTol ) EXIT
  END DO
  TransSystem(SysNum)%TCompInHP = Xnew

!  For capacity correction of HP compressors, consider subcooler, receiver, MT loads, LT loads and LP compressors
!  to constitute the "load".  The actual and rated conditions at the exit of the gas cooler and the inlet of the
!  HP compressors are used for capacity correction calculations.
  DensityActualMT = GetSupHeatDensityRefrig(TransSystem(SysNum)%RefrigerantName,TransSystem(SysNum)%TCompInHP, &
                    PsuctionMT,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
  TotalEnthalpyChangeActualMT=TransSystem(SysNum)%HCompInHP-GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%HGasCoolerOut

  !Dispatch HP compressors
  !Before dispatching HP compressors, zero sum of compressor outputs and zero each compressor
  TransSystem(SysNum)%TotCompCapacityHP  = 0.d0
  TransSystem(SysNum)%RefMassFlowCompsHP = 0.d0
  TransSystem(SysNum)%TotCompPowerHP     = 0.d0

  DO CompIndex=1,TransSystem(SysNum)%NumCompressorsHP
    CompID=TransSystem(SysNum)%CompressornumHP(CompIndex)
    Compressor(CompID)%Power    = 0.d0
    Compressor(CompID)%MassFlow = 0.d0
    Compressor(CompID)%Capacity = 0.d0
    Compressor(CompID)%ElecConsumption = 0.d0
    Compressor(CompID)%CoolingEnergy   = 0.d0
    Compressor(CompID)%LoadFactor      = 0.d0
  END DO

  ! Dispatch High Pressure compressors to meet load, note they were listed in compressor list in dispatch order
  DO CompIndex=1,TransSystem(SysNum)%NumCompressorsHP
    CompID=TransSystem(SysNum)%CompressornumHP(CompIndex)

    ! Need to use indiv compressor's rated subcool and superheat to adjust capacity to actual conditions
    ! Transcritical operation requires rated superheat
    ! Subcritical operation requires rated subcool and rated superheat
    SELECT CASE (Compressor(CompID)%SubcoolRatingType)
      CASE (RatedSubcooling)
        IF (.NOT.GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%TransOpFlag) THEN  ! Subcritical operation
          HCaseInRatedMT = GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%HGasCoolerOut - &
                           GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%CpGasCoolerOut*Compressor(CompID)%RatedSubcool
        ELSE  ! Transcritical operation
          HCaseInRatedMT = GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%HGasCoolerOut
        END IF  ! (.NOT.GasCooler(SysNum)%TransOpFlag)
      CASE (RatedLiquidTemperature)  !have rated liquid temperature stored in "RatedSubcool"
        IF (.NOT.GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%TransOpFlag) THEN  ! Subcritical operation
          HCaseInRatedMT = GetSatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,Compressor(CompID)%RatedSubcool, &
                           0.0d0,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
        ELSE  ! Transcritical operation
          HCaseInRatedMT = GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%HGasCoolerOut
        END IF  ! (.NOT.GasCooler(SysNum)%TransOpFlag)
    END SELECT
    SELECT CASE(Compressor(CompID)%SuperheatRatingType)
      CASE (RatedSuperheat)
        HCompInRatedHP = HsatVaporforTevapneededMT + TransSystem(SysNum)%CpSatVapEvapMT*Compressor(CompID)%RatedSuperheat
        TempInRatedHP  = TransSystem(Sysnum)%TEvapNeededMT + Compressor(CompID)%RatedSuperheat
      CASE (RatedReturnGasTemperature)  !have rated compressor inlet temperature stored in "RatedSuperheat"
        TempInRatedHP  = Compressor(CompID)%RatedSuperheat
        HCompInRatedHP = GetSupHeatEnthalpyRefrig(TransSystem(SysNum)%RefrigerantName,Compressor(CompID)%RatedSuperheat, &
                         PsuctionMT,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
    END SELECT

    CaseEnthalpyChangeRatedMT=HCompInRatedHP-HCaseInRatedMT
    DensityRatedHP = GetSupHeatDensityRefrig(TransSystem(SysNum)%RefrigerantName,TempInRatedHP,&
                     PSuctionMT,TransSystem(SysNum)%RefIndex,'RefrigeratedCase:CalculateTransCompressors')
    !  Adjust capacity and mass flow to reflect the specific volume change due to superheating and
    !  the increase in capacity due to extra subcooling
    MassCorrectionMT = DensityActualMT/DensityRatedHP
    CapacityCorrectionMT = MassCorrectionMT*TotalEnthalpyChangeActualMT/CaseEnthalpyChangeRatedMT

    IF (GasCooler(TransSystem(SysNum)%GasCoolerNum(1))%TransOpFlag) THEN  ! System is operating in transcritical region
      Compressor(CompID)%Power    = CurveValue(Compressor(CompID)%TransElecPowerCurvePtr,TsatforPsucMT,PGCOutlet)
      Compressor(CompID)%Capacity = CapacityCorrectionMT*CurveValue(Compressor(CompID)%TransCapacityCurvePtr, &
                                    TsatforPsucMT,HGCOutlet)
    ELSE ! System is operating in subcritical region
      Compressor(CompID)%Power    = CurveValue(Compressor(CompID)%ElecPowerCurvePtr,TsatforPsucMT,TsatforPdisMT)
      Compressor(CompID)%Capacity = CapacityCorrectionMT*CurveValue(Compressor(CompID)%CapacityCurvePtr,TsatforPsucMT,TsatforPdisMT)
    END IF  ! (GasCooler(SysNum)%TransOpFlag)
    !  Mass flow through HP compressors is HP compressor refrigerating capacity divided by MT load, LT load and LP compressor power
    Compressor(CompID)%MassFlow = TotalRefMassFlow*Compressor(CompID)%Capacity/ &
                                  (NeededCapacityMT + NeededCapacityLT + TransSystem(SysNum)%TotCompPowerLP)
    Compressor(CompID)%ElecConsumption = Compressor(CompID)%Power * LocalTimeStep * SecInHour
    Compressor(CompID)%CoolingEnergy   = Compressor(CompID)%Capacity * LocalTimeStep * SecInHour
    Compressor(CompID)%LoadFactor      = 1.d0
    ! calculate load factor for last compressor addded
    ! assumes either cycling or part load eff = full load eff for last compressor
    IF ((TransSystem(SysNum)%TotCompCapacityHP + Compressor(CompID)%Capacity) >= &
        (NeededCapacityMT + NeededCapacityLT + TransSystem(SysNum)%TotCompPowerLP)) THEN
      LFLastComp = ((NeededCapacityMT+NeededCapacityLT+TransSystem(SysNum)%TotCompPowerLP) - &
                   TransSystem(SysNum)%TotCompCapacityHP)/Compressor(CompID)%Capacity
      Compressor(CompID)%Power    = LFLastComp*Compressor(CompID)%Power
      Compressor(CompID)%MassFlow = LFLastComp*Compressor(CompID)%MassFlow
      Compressor(CompID)%Capacity = LFLastComp*Compressor(CompID)%Capacity
      TransSystem(SysNum)%TotCompCapacityHP  = TransSystem(SysNum)%TotCompCapacityHP + Compressor(CompID)%Capacity
      TransSystem(SysNum)%RefMassFlowCompsHP = TransSystem(SysNum)%RefMassFlowCompsHP + Compressor(CompID)%MassFlow
      TransSystem(SysNum)%TotCompPowerHP     = TransSystem(SysNum)%TotCompPowerHP + Compressor(CompID)%Power
      Compressor(CompID)%ElecConsumption     = Compressor(CompID)%Power * LocalTimeStep * SecInHour
      Compressor(CompID)%CoolingEnergy       = Compressor(CompID)%Capacity * LocalTimeStep * SecInHour
      Compressor(CompID)%LoadFactor          = LFLastComp
      EXIT
    ELSE
      TransSystem(SysNum)%TotCompCapacityHP  = TransSystem(SysNum)%TotCompCapacityHP + Compressor(CompID)%Capacity
      TransSystem(SysNum)%RefMassFlowCompsHP = TransSystem(SysNum)%RefMassFlowCompsHP + Compressor(CompID)%MassFlow
      TransSystem(SysNum)%TotCompPowerHP     = TransSystem(SysNum)%TotCompPowerHP + Compressor(CompID)%Power
    END IF

  END DO ! NumCompressorsHP

  TransSystem(SysNum)%HCompOutHP           = TransSystem(SysNum)%HCompInHP + &
                                             TransSystem(SysNum)%TotCompPowerHP/TransSystem(SysNum)%RefMassFlowCompsHP
  TransSystem(SysNum)%RefMassFlowComps     = TransSystem(SysNum)%RefMassFlowCompsLP + TransSystem(SysNum)%RefMassFlowCompsHP
  TransSystem(SysNum)%TotCompCapacity      = TransSystem(SysNum)%TotCompCapacityHP + TransSystem(SysNum)%TotCompCapacityLP
  TransSystem(SysNum)%AverageCompressorCOP = (TransSystem(SysNum)%TotCompCapacityHP-TransSystem(SysNum)%TotCompPowerLP) &
                                             /(TransSystem(SysNum)%TotCompPowerLP+TransSystem(SysNum)%TotCompPowerHP)
  TransSystem(SysNum)%TotCompElecConsump   = (TransSystem(SysNum)%TotCompPowerLP+TransSystem(SysNum)%TotCompPowerHP) &
                                             * LocalTimeStep * SecInHour
  TransSystem(SysNum)%TotCompCoolingEnergy = (TransSystem(SysNum)%TotCompCapacityLP+TransSystem(SysNum)%TotCompCapacityHP) &
                                             * LocalTimeStep * SecInHour

RETURN

END SUBROUTINE CalculateTransCompressors

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalculateSubcoolers(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL, Assisted by Hugh Henderson
          !       DATE WRITTEN   Spring 2008
          !       MODIFIED       Brian Fricke, ORNL, March 2012, added two-stage compression
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the subcooler energy exchange and refrigerant states for a particular detailed
          ! refrigeration system. Use the refrigerant property subroutines in FluidProperties.f90

          ! METHODOLOGY EMPLOYED:
          ! Use refrigerant properties and heat exchanger correlations.  NOTE:  Assumes any Mech subcooler
          ! immediately follows condenser outlet (after pipe loss) and comes before any LSHX

          ! REFERENCES:
          ! ASHRAE 1006 Section 2: Refrigeration Accessories

          ! USE STATEMENTS:
  USE CurveManager,      ONLY : CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: SubcoolerIndex      ! Counter through number of subcoolers on this system
  INTEGER     :: SubcoolerID         ! ID number for each unique subcooler
  INTEGER     :: SysProvideID        ! ID number of system providing refrigeration effect (ie compressor work) for mech sc
  REAL(r64)   :: CpLiquid            ! specific heat at liquid Tsat, best avail in Fluid Properties
  REAL(r64)   :: CpVapor             ! specific heat at vapor Tsat, best avail in Fluid Properties
  REAL(r64)   :: ControlTliqOut      ! Controlled temperature of liquid leaving Mechanical subcooler
  REAL(r64)   :: DelTempActual       ! Actual subcooling, T liquid in - T liquid out
  REAL(r64)   :: DelTLiqDes          ! Design Temperature Change on liquid side of LSHX
  REAL(r64)   :: LocalTimeStep = 0.0d0 !TimeStepZone for case/walkin systems, TimeStepSys for coil systems
  REAL(r64)   :: LSHXeffectiveness   ! EFfectiveness of liquid suction heat exchanger (LSHX)
  REAL(r64)   :: MechSCLoad          ! Mechanical subcooler load
  REAL(r64)   :: SubcoolLoad       ! energy transferred from warmer liquid to cool vapor
  REAL(r64)   :: SubcoolerSupHeat    ! additional superheat added to vapor going to compressor from LSHX
  REAL(r64)   :: TVapInDes           ! Design Vapor Inlet temperature for LSHX
  REAL(r64)   :: TLiqInDes           ! Design Liquid Inlet temperature for LSHX
  REAL(r64)   :: TLiqInActual        ! Liquid T in, after condenser, before any mechanical subcooler
  REAL(r64)   :: TVapInActual        ! Vapor T in, after any superheat added by case control

  LocalTimeStep = TimeStepZone
  IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

  !HCaseIn has to be recalculated as the starting point for the subcoolers here because
  !  of the multiple number of iterations through this subroutine and because Tcondense is evolving.
  IF(System(SysNum)%NumStages==1) THEN  ! Single-stage compression system
    System(SysNum)%HSatLiqCond=GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,0.0d0,&
                                                  System(SysNum)%RefIndex,'CalculateSubcoolers')
    System(SysNum)%CpSatLiqCond = GetSatSpecificHeatRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,&
                                                             0.0d0,System(SysNum)%RefIndex,'CalculateSubcoolers')
    System(SysNum)%HCaseIn = System(SysNum)%HSatLiqCond - System(SysNum)%CpSatLiqCond* &
                               Condenser(System(SysNum)%CondenserNum(1))%RatedSubcool

      ! Two-stage compression with flash intercooler
  ELSE IF(System(SysNum)%NumStages==2 .AND. System(SysNum)%IntercoolerType==1) THEN
    System(SysNum)%HSatLiqCond = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,0.0d0,&
                                 System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
    System(SysNum)%CpSatLiqCond = GetSatSpecificHeatRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,&
                                  0.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
    System(SysNum)%HCaseIn = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TIntercooler,0.0d0,&
                             System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')

      ! Two-stage compression with shell-and-coil intercooler
  ELSE IF(System(SysNum)%NumStages==2 .AND. System(SysNum)%IntercoolerType==2) THEN
    TLiqInActual = System(SysNum)%TCondense-Condenser(System(Sysnum)%CondenserNum(1))%RatedSubcool-&
                   System(SysNum)%IntercoolerEffectiveness*(System(SysNum)%TCondense-&
                   Condenser(System(Sysnum)%CondenserNum(1))%RatedSubcool-System(SysNum)%TIntercooler)
    System(SysNum)%HSatLiqCond = GetSatEnthalpyRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,0.0d0,&
                                 System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
    System(SysNum)%CpSatLiqCond = GetSatSpecificHeatRefrig(System(SysNum)%RefrigerantName,System(SysNum)%TCondense,&
                                  0.0d0,System(SysNum)%RefIndex,'RefrigeratedCase:CalculateCompressors')
    System(SysNum)%HCaseIn = System(SysNum)%HSatLiqCond - System(SysNum)%CpSatLiqCond*(System(SysNum)%TCondense - &
                             TLiqInActual)
  END IF  ! NumStages and IntercoolerType

  DO SubcoolerIndex=1,System(SysNum)%NumSubcoolers
    SubcoolerID=System(Sysnum)%Subcoolernum(SubcoolerIndex)
    !set up local variables for convenience
    DelTLiqDes     = Subcooler(SubcoolerID)%LiqSuctDesignDelT
    TVapInDes      = Subcooler(SubcoolerID)%LiqSuctDesignTvapIn
    TLiqInDes      = Subcooler(SubcoolerID)%LiqSuctDesignTliqIn
    ControlTLiqOut = Subcooler(SubcoolerID)%MechControlTliqOut
    CpLiquid       = System(Sysnum)%CpSatLiqCond
    CpVapor        = System(SysNum)%CpSatVapEvap
    IF(System(SysNum)%NumStages==1) THEN  ! Single-stage compression system
      TLiqInActual = System(Sysnum)%TCondense-Condenser(System(Sysnum)%CondenserNum(1))%RatedSubcool

        ! Two-stage compression with flash intercooler
    ELSE IF(System(SysNum)%NumStages==2 .AND. System(SysNum)%IntercoolerType==1) THEN
      TLiqInActual = System(SysNum)%TIntercooler

        ! Two-stage compression with shell-and-coil intercooler
    ELSE IF(System(SysNum)%NumStages==2 .AND. System(SysNum)%IntercoolerType==2) THEN
      TLiqInActual = System(SysNum)%TCondense-Condenser(System(Sysnum)%CondenserNum(1))%RatedSubcool-&
                     System(SysNum)%IntercoolerEffectiveness*(System(SysNum)%TCondense-&
                     Condenser(System(Sysnum)%CondenserNum(1))%RatedSubcool-System(SysNum)%TIntercooler)
    END IF  ! NumStages and IntercoolerType

    SELECT CASE (Subcooler(SubcoolerID)%SubcoolerType)
      !Mechanical subcoolers required to come first in order to take advantage of delT
      !  from lshx. taken care of because subcooler ID assigned in that order in input.

      CASE (Mechanical)
         MechSCLoad             = System(SysNum)%RefMassFlowtoLoads*CpLiquid*(TLiqInActual - ControlTLiqOut)
         System(SysNum)%HCaseIn = System(SysNum)%HCaseIn - CpLiquid*(TLiqInActual - ControlTLiqOut)
         !refrigeration benefit to system(sysnum)
         !refrigeration load must be assigned properly according to input
         SysProvideID           = Subcooler(SubcoolerID)%MechSourceSysID
         System(SysProvideID)%MechSCLoad(SubcoolerID) = MechSCLoad
         Subcooler(SubcoolerID)%MechSCTransLoad=MechSCLoad
         Subcooler(SubcoolerID)%MechSCTransEnergy=MechSCLoad * LocalTimeStep * SecInHour
         ! Reset inlet temperature for any LSHX that follows this mech subcooler
         TLiqInActual           = ControlTLiqOut
         System(SysNum)%TcompIn = System(SysNum)%TEvapNeeded + CaseSuperheat

      CASE (LiquidSuction)
          LSHXeffectiveness = DelTLiqDes/(TLiqInDes - TVapInDes)
          TVapInActual      = System(SysNum)%TEvapNeeded + CaseSuperheat
          DelTempActual     = LSHXeffectiveness * (TLiqInActual - TVapInActual)
          TLiqInActual      = TLiqInActual-DelTempActual
          SubcoolLoad       = System(SysNum)%RefMassFlowtoLoads*CpLiquid*DelTempActual
          SubcoolerSupHeat  = SubcoolLoad/CpVapor/System(SysNum)%RefMassFlowComps
          System(SysNum)%TcompIn = TVapInActual+SubcoolerSupHeat
          System(SysNum)%HCaseIn = System(SysNum)%HCaseIn - SubcoolLoad/System(SysNum)%RefMassFlowtoLoads
          System(SysNum)%LSHXTrans = SubcoolLoad
          System(SysNum)%LSHXTransEnergy = SubcoolLoad * LocalTimeStep * SecInHour
    END SELECT

    System(SysNum)%TLiqInActual=TLiqInActual
    END DO

END SUBROUTINE CalculateSubcoolers

!***************************************************************************************************


!***************************************************************************************************

SUBROUTINE GetRefrigeratedRackIndex(Name,IndexPtr,SysType,ErrorsFound,ThisObjectType,SuppressWarning)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   June 2007
          !       MODIFIED       Therese Stovall May 2008
          !       RE-ENGINEERED  na
          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets an index for a given refrigerated rack or refrigeration condenser
          !  -- issues error message if the rack or condenser is not found.
          ! METHODOLOGY EMPLOYED:
          ! na
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  !USE DataGlobals,    ONLY: ShowSevereError


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Name
  INTEGER, INTENT(INOUT)       :: IndexPtr
  INTEGER, INTENT(IN   )       :: SysType
  LOGICAL, INTENT(INOUT)       :: ErrorsFound
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ThisObjectType
  LOGICAL, INTENT(IN), OPTIONAL :: SuppressWarning

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

CALL CheckRefrigerationInput

SELECT CASE (SysType)
  CASE (RefrigSystemTypeRack)
    IndexPtr = FindItemInList(Name,RefrigRack%Name,NumRefrigeratedRacks)
    IF (IndexPtr == 0) THEN
      IF(PRESENT(SuppressWarning)) THEN
  !     No warning printed if only searching for the existence of a refrigerated rack
      ELSE
        IF (PRESENT(ThisObjectType)) THEN
          CALL ShowSevereError(TRIM(ThisObjectType)//', GetRefrigeratedRackIndex: Rack not found='//TRIM(Name))
        ELSE
          CALL ShowSevereError('GetRefrigeratedRackIndex: Rack not found='//TRIM(Name))
        ENDIF
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF
 CASE (RefrigSystemTypeDetailed)
    IndexPtr = FindItemInList(Name,Condenser%Name,NumRefrigCondensers)
    IF (IndexPtr == 0) THEN
      IF(PRESENT(SuppressWarning)) THEN
  !     No warning printed if only searching for the existence of a refrigeration Condenser
      ELSE
        IF (PRESENT(ThisObjectType)) THEN
          CALL ShowSevereError(TRIM(ThisObjectType)//', GetRefrigeratedRackIndex: Condenser not found='//TRIM(Name))
        ELSE
          CALL ShowSevereError('GetRefrigeratedRackIndex: Condenser not found='//TRIM(Name))
        ENDIF
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF
END SELECT
  RETURN

END SUBROUTINE GetRefrigeratedRackIndex

SUBROUTINE ReportRefrigerationComponents

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   October 2004
          !       MODIFIED       Shirey, FSEC Dec 2004; Lawrie, Mar 2008 - Node names, not numbers.
          !       MODIFIED       Stovall - 2008 to 2010, new refrig variables and move orphan reporting to input.
          !       MODIFIED       Fricke, ORNL, Fall 2011, added transcritical CO2 refrigeration system variables
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To report information from the input deck for refrigerated cases and racks to the eio and err file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: NodeID
  USE General, ONLY: RoundSigDigits

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
  INTEGER CascadeLoadID
  INTEGER CascadeLoadNum
  INTEGER CaseID
  INTEGER CaseNum
  INTEGER ChillerSetNum
  INTEGER CoilID
  INTEGER CoilNum
  INTEGER CompID
  INTEGER CompressorNum
  INTEGER CondID
  INTEGER CountSecPhase
  INTEGER CountSecBrine
  INTEGER GasCoolerID
  INTEGER RackNum
  INTEGER SecondaryID
  INTEGER SecondaryNum
  INTEGER SubcoolerNum
  INTEGER SubcoolerID
  INTEGER SystemNum
  INTEGER TransSystemNum
  INTEGER WalkInID
  INTEGER WalkInNum
  INTEGER ZoneNum
  INTEGER ZoneID
  CHARACTER(len=15) ChrOut
  CHARACTER(len=15) ChrOut2

 101 FORMAT(A)
 102 FORMAT(4X,A)
 103 FORMAT(2X,A)
 104 FORMAT('! <Refrigeration Compressor Rack>,Compressor Rack Name,',&
        ' # Refrigerated Cases Connected,# WalkIn Coolers Connected, Heat Rejection Location, ',&
        'Condenser Type, COP')
 105 FORMAT('!',2x,'<Refrigeration Case>,Refrigeration Case Number, Refrigeration Case Name,Zone Name,', &
        'Zone Node #,Zone Node Name,Capacity (W/m),LHR,Temperature (C),Length (m),Fan (W/m),',&
        'Installed Lighting (W/m),Anti-Sweat (W/m),Defrost (W/m)')
 108 FORMAT('!',2x,'<Refrigeration Compressor>,Compressor Number,Compressor Name,Nominal Capacity (W)')
 109 FORMAT('! <#Refrigeration Compressor Racks>,Number of Refrigeration Compressor Racks')
! 111 FORMAT(',',1X,F6.3) ! compressor rack output line
! 112 FORMAT(',',1X,F16.0)! compressor output line
! 113 FORMAT(',',1X,F7.1,',',1X,F6.2,6(',',1X,F6.1)) !refrigerated case output line
 114 FORMAT(',',1X,F7.1) ! detailed system output line
 117 FORMAT('! <#Detailed Refrigeration Systems>,Number of Detailed Refrigeration Systems')
 118 FORMAT('! <Detailed Refrigeration System>,Refrigeration System Name,Refrigerant Used', &
                ', # Refrigerated Cases Connected, # WalkInCoolers Connected, #Air Chillers Connected', &
                ', # Secondary Loops Served, # Cascade Condensers Served', &
                ', # Mechanical Subcoolers Served, # Compressors Connected', &
                ', # Compression Stages, Intercooler Type, Intercooler Effectiveness', &
                ', # Subcoolers Connected, Minimum Condensing Temperature (C)')
 119 FORMAT('!',2x,'<Refrigeration Walk In Cooler>, Walk In Number, Walk In Name,', &
         'Capacity (W),Temperature (C),Coil Fan (W), Circulating Fan (W), ',&
         'Lighting (W),Heaters (W),Defrost (W), # Zones')
 120 FORMAT('! <#Detailed Transcritical Refrigeration Systems>,Number of Detailed Transcritical Refrigeration Systems')
 121 FORMAT('! <Detailed Transcritical Refrigeration System>,Transcritical Refrigeration System Name,Refrigerant Used', &
                ', # Medium Temperature Refrigerated Cases Connected, # Low Temperature Refrigerated Cases Connected', &
                ', # Medium Temperature WalkInCoolers Connected, # Low Temperature WalkInCoolers Connected', &
                ', # High Pressure Compressors Connected, # Low Pressure Compressors Connected', &
                ', Minimum Condensing Temperature (C)')
 123 FORMAT('!',2x,'<Secondary Load>, Secondary System Served Name, Secondary Number')
 126 FORMAT('!',2x,'<Refrigeration Mechanical Subcooler>, Subcooler Number, Subcooler Name, ', &
                'Name of System Providing Cooling, Design Outlet Temperature (C)')
 127 FORMAT('!',2x,'<Refrigeration Liquid Suction Subcooler>, Subcooler Number, Subcooler Name, ', &
                'Design Subcooling (DeltaC),',&
            'Design liquid inlet temperature (C), Design vapor inlet temperature (C)')
 128 FORMAT('!',2x,'<Cascade Load>, System Name Connected, Condenser Number, Condenser Name')
 129 FORMAT('!',2x,'<Refrigeration Condenser:Air-Cooled>,Condenser Number,Condenser Name,Rated Condensing Temperature (C),', &
            'Rated Capacity (W), Rated Fan Power (W)')
 130 FORMAT('!',2x,'<Refrigeration Condenser:Water-Cooled>,Condenser Number,Condenser Name,Rated Condensing Temperature (C),', &
            'Rated Capacity (W), Rated Water Inlet Temperature (C), Rated Water Flow Rate (m3/s)')
 131 FORMAT('!',2x,'<Refrigeration Condenser:Evaporative-Cooled>,Condenser Number,Condenser Name,', &
            'Rated Capacity (W), Rated Fan Power (W)')
 132 FORMAT('!',2x,'<Refrigeration Condenser:Cascade>, Condenser Number, Condenser Name,', &
         ' Condensing Temperature Control Type, Rated Condensing Temperature (C),', &
         ' Capacity (W), Approach Temperature Difference (DeltaC)')
 133 FORMAT('! <Secondary Refrigeration System: Fluid Always Liquid>, Secondary Number, Secondary Name,', &
         ' # Refrigerated Cases Connected, # WalkIn Coolers Connected,', &
         ' Fluid Name, Capacity (W),Evap Temperature in Secondary Evaporator (C),',&
         ' Approach Temperature Difference (DeltaC), Temperature Range (DeltaC), TotalPumpPower (W)')
 134 FORMAT('!',6x,'<Walk-In Surfaces Facing Zone>, ZoneName,',&
            ' Wall/Ceiling Area (m2), UValue (W/m2-C), AreaStockDoors (m2), HeightStockDoors,',&
            ' UValueStockDoors (W/m2-C), AreaGlassDoors (m2), HeightGlassDoors (m), ',&
            ' UValueGlassDoors (W/m2-C)')
! 135 FORMAT (6x,6(',',1X,F16.1),',',2x,I5)
! 136 FORMAT (6x,8(',',1X,F16.1))
! 137 FORMAT (2x, 2(',',1X,F12.1))!condenser output
! 138 FORMAT (2x, 3(',',1X,F12.1))!condenser output
! 139 FORMAT (2x, 4(',',1X,F12.1))!condenser output
! 140 FORMAT(7(',',1X,F8.1),1X,',',I5) !walkin output line
 141 FORMAT('!',2x,'<Mechanical Subcooler Load>, Subcooler Number, Subcooler Name')
 142 FORMAT('! <#Secondary Refrigeration Systems>,Number of Secondary Refrigeration Systems')
! 143 FORMAT(',',1X,F8.1,',',1X,F8.4,2(1X,',',F8.2,',',1x,F8.2,',',1x,F8.4)) !walkin/zone output line
! 144 FORMAT(',',1X,F7.1) !mech subcooler output line
! 145 FORMAT(3(',',1X,F7.1)) !lshx output line
 146 FORMAT('! <Secondary Refrigeration System: Liquid Overfeed>, Secondary Number, Secondary Name,', &
         ' # Refrigerated Cases Connected, # WalkIn Coolers Connected, #Air Coils Connected', &
         ' Fluid Name, Capacity (W),Evap Temperature in Secondary Evaporator (C),',&
         ' Approach Temperature Difference (DeltaC), Circulating Rate, TotalPumpPower (W)')
! 147 FORMAT(',',1X,F7.1,1X,2(',',F6.2),2(',',1X,F9.3)) !secondary system output line
 148 FORMAT('! <#ZoneHVAC/Refrigeration Air Chiller Sets>,Number of ZoneHVAC/Refrigeration Air Chiller Sets')
 149 FORMAT('! <ZoneHVAC/Refrigeration Air Chiller Set>,Chiller Set Name,',&
        ' # Air Chillers Connected, Zone Location')
 !150 FORMAT('! <#Refrigeration Air Chiller>,Number of Refrigeration Air Chillers')
 151 FORMAT('!',2x,'<Refrigeration Air Chiller>,Refrigeration Chiller Number, Refrigeration Chiller Name,Zone Name,', &
        'Zone Node #,Zone Node Name,Sensible Capacity (W/C),Sensible Capacity (W),Evaporating Temperature (C),DT1 (C),',&
        'Fan Power (W),Heater (W),Defrost (W), Air Flow Rate (m3/s)')
 152 FORMAT('!',2x,'<Air Chiller Load>, Air Chiller Name, Air Chiller Number, Zone Name,')
 160 FORMAT('!',2x,'<Refrigeration GasCooler:Air-Cooled>,Gas Cooler Number, Gas Cooler Name, Rated Outlet Pressure (Pa),', &
         'Rated Outlet Temperature (C), Rated Approach Temperature (C), Rated Capacity (W), Rated Fan Power (W)')


        !write all headers applicable to this simulation
 IF(NumRefrigeratedRacks > 0) THEN
  WRITE(OutputFileInits,109) ! Intro to refrigeration case racks
  WRITE(OutputFileInits,104) ! Refrigeration Rack header
 END IF !(NumRefrigeratedRacks > 0)
IF(NumRefrigSystems > 0)THEN
  WRITE(OutputFileInits,117) ! Intro to detailed systems
  WRITE(OutputFileInits,118) ! Detailed system header
  WRITE(OutputFileInits,108) ! Compressor header (Always have compressor if have detailed system)
END IF !(NumRefrigSystems > 0)
IF(NumSimulationSecondarySystems > 0)THEN
  WRITE(OutputFileInits,142) ! Intro to Secondary systems
  CountSecPhase = 0
  CountSecBrine = 0
  DO SecondaryID=1,NumSimulationSecondarySystems
    IF((Secondary(SecondaryID)%FluidType == SecFluidTypeAlwaysLiquid).AND. (CountSecBrine ==0))THEN
      WRITE(OutputFileInits,133) ! Secondary system header for brine type systems
      CountSecBrine = CountSecBrine + 1
    END IF
    IF((Secondary(SecondaryID)%FluidType == SecFluidTypePhaseChange).AND. (CountSecPhase ==0))THEN
      WRITE(OutputFileInits,146) ! Secondary system header for liquid overfeed/phase change systems
      CountSecPhase = CountSecPhase +1
    END IF
  END DO
  WRITE(OutputFileInits,123) !  Secondary system load header
END IF !(NumSimulationSecondarySystems > 0)
IF(NumRefrigChillerSets > 0)THEN
  WRITE(OutputFileInits,148) ! Intro to Chiller set
  WRITE(OutputFileInits,149) ! Chiller set header
  WRITE(OutputFileInits,151) ! Intro to Air Chiller
  WRITE(OutputFileInits,152) ! Air chiller header
END IF !(NumRefrigSystems > 0)
IF(NumSimulationCases > 0)THEN
  WRITE(OutputFileInits,105) !  Case header
END IF !(NumSimulationCases > 0)
IF(NumSimulationWalkIns > 0)THEN
  WRITE(OutputFileInits,119) !  Walk-in header
  WRITE(OutputFileInits,134) !  Walk-in zone-specific header
END IF !(NumSimulationWalkIns > 0)
IF(NumSimulationCondAir > 0)THEN
  WRITE(OutputFileInits,129) !  Condenser, Air-Cooled header
END IF !(NumSimulationCondAir > 0)
IF(NumSimulationCondEvap > 0)THEN
  WRITE(OutputFileInits,131) !  Condenser, Evaporative-Cooled header
END IF !(NumSimulationCondEvap > 0)
IF(NumSimulationCondWater > 0)THEN
  WRITE(OutputFileInits,130) !  Condenser, Water-Cooled header
END IF !(NumSimulationCondWater > 0)
IF(NumSimulationCascadeCondensers > 0)THEN
  WRITE(OutputFileInits,132) !  Condenser, Cascade header
  WRITE(OutputFileInits,128) !  Cascade Load header
END IF !(NumSimulationCascadeCondensers > 0)
IF(NumSimulationMechSubcoolers > 0)THEN
  WRITE(OutputFileInits,141) !  Mech subcooler loads served header
  WRITE(OutputFileInits,126) !  Mechanical Subcooler header
END IF !(NumSimulationMechSubcoolers > 0)
IF((NumSimulationSubcoolers - NumSimulationMechSubcoolers) > 0)THEN
  WRITE(OutputFileInits,127) !  LSHX Subcooler header
END IF !((NumSimulationSubcoolers - NumSimulationMechSubcoolers) > 0)

IF(NumTransRefrigSystems > 0)THEN
  WRITE(OutputFileInits,120) ! Intro to detailed transcriticial refrigeration system
  WRITE(OutputFileInits,121) ! Detailed system header
  IF(NumSimulationCases > 0)THEN
    WRITE(OutputFileInits,105) !  Case header
  END IF !(NumSimulationCases > 0)
  IF(NumSimulationWalkIns > 0)THEN
    WRITE(OutputFileInits,119) !  Walk-in header
    WRITE(OutputFileInits,134) !  Walk-in zone-specific header
  END IF !(NumSimulationWalkIns > 0)
  WRITE(OutputFileInits,108) ! Compressor header (Always have compressor if have detailed system)
  IF(NumSimulationGasCooler > 0)THEN
    WRITE(OutputFileInits,160) !  Gas Cooler, Air-Cooled header
  END IF !(NumSimulationGasCooler > 0)
END IF !(NumTransRefrigSystems > 0)

 IF(NumRefrigeratedRacks > 0) THEN
  WRITE(OutputFileInits,101) '#Refrigeration Compressor Racks, '//TRIM(RoundSigDigits(NumRefrigeratedRacks))
  DO RackNum=1,NumRefrigeratedRacks
    IF(RefrigRack(RackNum)%HeatRejectionLocation == LocationOutdoors ) THEN
      ChrOut='Outdoors'
    ELSE
      ChrOut='Zone'
    END IF
    SELECT CASE (RefrigRack(RackNum)%CondenserType)
      CASE(RefrigCondenserTypeAir)
      ChrOut2='Air-Cooled'
      CASE(RefrigCondenserTypeEvap)
      ChrOut2='Evap-Cooled'
      CASE(RefrigCondenserTypeWater)
      ChrOut2='Water-Cooled'
    END SELECT
    WRITE(OutputFileInits,101) ' Refrigeration Compressor Rack,'//TRIM(RefrigRack(RackNum)%Name)//','// &
            TRIM(RoundSigDigits(RefrigRack(RackNum)%NumCases))//','// &
            TRIM(RoundSigDigits(RefrigRack(RackNum)%NumWalkIns))//','// &
            TRIM(ChrOut)//','// TRIM(ChrOut2)//','//trim(RoundSigDigits(RefrigRack(RackNum)%RatedCOP,3))
    DO CaseNum=1,RefrigRack(RackNum)%NumCases
      CaseID = RefrigRack(RackNum)%CaseNum(CaseNum)
      WRITE(OutputFileInits,103) ' Refrigeration Case,'//  &
            TRIM(RoundSigDigits(CaseID))//','//  &
            TRIM(RefrigCase(CaseID)%Name)//','//  &
            TRIM(RefrigCase(CaseID)%ZoneName)//','//   &
            TRIM(RoundSigDigits(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            TRIM(NodeID(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            trim(RoundSigDigits(RefrigCase(CaseID)%RateTotCapPerLength,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%RatedLHR,2))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Temperature,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Length,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%OperatingFanPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%LightingPower,1))//','// &  !Installed lighting power, may not be rated power
            trim(RoundSigDigits(RefrigCase(CaseID)%AntiSweatPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%DefrostPower,1))
    ENDDO !numcases

    DO  WalkInNum=1,RefrigRack(RackNum)%NumWalkIns
      WalkInID = RefrigRack(RackNum)% WalkInNum( WalkInNum)
      WRITE(OutputFileInits,103) ' Refrigeration Walk In Cooler,  '//  &
            TRIM(RoundSigDigits(WalkInID))//','//  &
            TRIM( WalkIn(WalkInID)%Name)//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignRatedCap,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%Temperature,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CoilFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CircFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%ElecFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignLighting,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeaterPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DefrostCapacity,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%NumZones))
      DO ZoneID=1, WalkIn( WalkInID)%NumZones
         WRITE(OutputFileInits,102) '  Walk-In Surfaces Facing Zone, '//  &
            TRIM( WalkIn(WalkInID)%ZoneName(ZoneID))//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%SurfaceArea(ZoneID),1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValue(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueStockDr(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueGlassDr(ZoneID),4))
      ENDDO !zones for walk ins on rack
    ENDDO ! walk ins on rack

    DO CoilNum=1,RefrigRack(RackNum)%NumCoils
      CoilID = RefrigRack(RackNum)%CoilNum(CoilNum)
      WRITE(OutputFileInits,103) ' Air Chiller Load,'//  &
            TRIM(WareHouseCoil(CoilID)%Name)//','//  &
            TRIM(RoundSigDigits(CoilID))//','//  &
            TRIM(WareHouseCoil(CoilID)%ZoneName)
    ENDDO !numairchillers
  ENDDO !numracks
END IF !(NumRefrigeratedRacks > 0)

IF(NumRefrigSystems > 0)THEN
  WRITE(OutputFileInits,101) '#Detailed Refrigeration Systems,'//TRIM(RoundSigDigits(NumRefrigSystems))
  DO SystemNum=1,NumRefrigSystems
    WRITE(OutputFileInits,101) ' Detailed Refrigeration System,'//&
          TRIM(System(SystemNum)%Name)//','//TRIM(System(SystemNum)%RefrigerantName)//','//&
          TRIM(RoundSigDigits(System(SystemNum)%NumCases))//','// &
          TRIM(RoundSigDigits(System(SystemNum)%NumWalkIns))//','// &
          TRIM(RoundSigDigits(System(SystemNum)%NumCoils))//','// &
          TRIM(RoundSigDigits(System(SystemNum)%NumSecondarys))//','// &
          TRIM(RoundSigDigits(System(SystemNum)%NumCascadeLoads))//','//&
          TRIM(RoundSigDigits(System(SystemNum)%NumMechSCServed))//','//&
          TRIM(RoundSigDigits(System(SystemNum)%NumCompressors + System(SystemNum)%NumHiStageCompressors))//','//&
          TRIM(RoundSigDigits(System(SystemNum)%NumStages))//','// &
          TRIM(RoundSigDigits(System(SystemNum)%IntercoolerType))//','// &
          TRIM(RoundSigDigits(System(SystemNum)%IntercoolerEffectiveness,2))//','// &
          TRIM(RoundSigDigits(System(SystemNum)%NumSubcoolers))//','//  &
          trim(RoundSigDigits(System(SystemNum)%TCondenseMin,1))

    DO CaseNum=1,System(SystemNum)%NumCases
      CaseID=System(SystemNum)%CaseNum(CaseNum)
      WRITE(OutputFileInits,103) ' Refrigeration Case,'//  &
            TRIM(RoundSigDigits(CaseID))//','//  &
            TRIM(RefrigCase(CaseID)%Name)//','//  &
            TRIM(RefrigCase(CaseID)%ZoneName)//','//   &
            TRIM(RoundSigDigits(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            TRIM(NodeID(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            trim(RoundSigDigits(RefrigCase(CaseID)%RateTotCapPerLength,1))//','//  &
            trim(RoundSigDigits(RefrigCase(CaseID)%RatedLHR,2))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Temperature,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Length,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%OperatingFanPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%LightingPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%AntiSweatPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%DefrostPower,1))
    ENDDO !NumCases on system
    DO  WalkInNum=1,System(SystemNum)%NumWalkIns
      WalkInID=System(SystemNum)%WalkInNum(WalkInNum)
      WRITE(OutputFileInits,103) ' Refrigeration Walk In Cooler,'//  &
            TRIM(RoundSigDigits(WalkInID))//','//  &
            TRIM( WalkIn(WalkInID)%Name)//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignRatedCap,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%Temperature,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CoilFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CircFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignLighting,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeaterPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DefrostCapacity,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%NumZones))
       DO ZoneID=1, WalkIn( WalkInID)%NumZones
         WRITE(OutputFileInits,102) '  Walk-In Surfaces Facing Zone, '//  &
            TRIM( WalkIn(WalkInID)%ZoneName(ZoneID))//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%SurfaceArea(ZoneID),1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValue(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueStockDr(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueGlassDr(ZoneID),4))
    ENDDO !Num zones for each walk in on system
    ENDDO !NumWalkIns on system

    DO CoilNum=1,System(SystemNum)%NumCoils
    CoilID = System(SystemNum)%CoilNum(CoilNum)
      WRITE(OutputFileInits,103) ' Air Chiller Load,'//  &
            TRIM(WareHouseCoil(CoilID)%Name)//','//  &
            TRIM(RoundSigDigits(CoilID))//','//  &
            TRIM(WareHouseCoil(CoilID)%ZoneName)
    ENDDO !numairchillers

  DO CascadeLoadNum=1,System(SystemNum)%NumCascadeLoads
      CascadeLoadID = System(SystemNum)%CascadeLoadNum(CascadeLoadNum)
      WRITE(OutputFileInits,103) ' Cascade Load,'//  &
            TRIM(System(Condenser(CascadeLoadID)%CascadeSysID)%Name)//','// &
            TRIM(RoundSigDigits(CascadeLoadID))//','//TRIM(Condenser(CascadeLoadID)%Name)
  ENDDO !cascade load on detailed system

  DO SecondaryNum=1,System(SystemNum)%NumSecondarys
      SecondaryID = System(SystemNum)% SecondaryNum( SecondaryNum)
      WRITE(OutputFileInits,103) ' Secondary Load,'//  &
            TRIM(Secondary(SecondaryID)%Name)//','//TRIM(RoundSigDigits(SecondaryID))
  ENDDO !secondary load on detailed system

  DO SubcoolerNum=1,NumSimulationSubcoolers
    IF (Subcooler(SubcoolerNum)%MechSourceSysID /=SystemNum)CYCLE
    WRITE(OutputFileInits,103)' Mechanical Subcooler Load, '// TRIM(RoundSigDigits(SubcoolerNum))//','//  &
             TRIM(Subcooler(SubcoolerNum)%Name)
  ENDDO !Num sim subcoolers, looking only for NumSMech Subcoolers served by this system

    IF (System(SystemNum)%NumStages == 1) THEN  ! Single-stage compression system
       DO CompressorNum=1,System(SystemNum)%NumCompressors
        CompID=System(SystemNum)%CompressorNum(CompressorNum)
        WRITE(OutputFileInits,103) ' Refrigeration Compressor,'//  &
              TRIM(RoundSigDigits(CompID))//','//  &
              TRIM(Compressor(CompID)%Name)//','//  &
              TRIM(RoundSigDigits(Compressor(CompID)%NomCap,0))
       ENDDO !NumCompressors
    ELSE IF (System(SystemNum)%NumStages == 2) THEN  ! Two-stage compression system
       ! Low-stage compressors
       DO CompressorNum=1,System(SystemNum)%NumCompressors
        CompID=System(SystemNum)%CompressorNum(CompressorNum)
        WRITE(OutputFileInits,103) ' Refrigeration Low-Stage Compressor,'//  &
              TRIM(RoundSigDigits(CompID))//','//  &
              TRIM(Compressor(CompID)%Name)//','//  &
              TRIM(RoundSigDigits(Compressor(CompID)%NomCap,0))
       ENDDO !NumCompressors
       ! High-stage compressors
       DO CompressorNum=1,System(SystemNum)%NumHiStageCompressors
        CompID=System(SystemNum)%HiStageCompressorNum(CompressorNum)
        WRITE(OutputFileInits,103) ' Refrigeration High-Stage Compressor,'//  &
              TRIM(RoundSigDigits(CompID))//','//  &
              TRIM(Compressor(CompID)%Name)//','//  &
              TRIM(RoundSigDigits(Compressor(CompID)%NomCap,0))
       ENDDO !NumHiStageCompressors
    END IF  !NumStages

    CondID=System(SystemNum)%CondenserNum(1)
    SELECT CASE (Condenser(CondID)%CondenserType)
      CASE(RefrigCondenserTypeAir)
        WRITE(OutputFileInits,103) ' Refrigeration Condenser:Air-Cooled,'//  &
            TRIM(RoundSigDigits(CondID))//','// TRIM(Condenser(CondID)%Name)//','//  &
            trim(RoundSigDigits(Condenser(CondID)%RatedTCondense,1))//','//  &
            trim(RoundSigDigits(Condenser(CondID)%RatedCapacity,1))//','//   &
            trim(RoundSigDigits(Condenser(CondID)%RatedFanPower,1))
      CASE(RefrigCondenserTypeEvap)
        WRITE(OutputFileInits,103) ' Refrigeration Condenser:Evaporative-Cooled,'//  &
            TRIM(RoundSigDigits(CondID))//','// TRIM(Condenser(CondID)%Name)//','//  &
            trim(RoundSigDigits(Condenser(CondID)%RatedCapacity,1))//','//  &
            trim(RoundSigDigits(Condenser(CondID)%RatedFanPower,1))
      CASE(RefrigCondenserTypeWater)
        WRITE(OutputFileInits,103) ' Refrigeration Condenser:Water-Cooled,'//  &
            TRIM(RoundSigDigits(CondID))//','// TRIM(Condenser(CondID)%Name)//','//  &
            trim(RoundSigDigits(Condenser(CondID)%RatedTCondense,1))//','//  &
            trim(RoundSigDigits(Condenser(CondID)%RatedCapacity,1))//','//   &
            trim(RoundSigDigits(Condenser(CondID)%InletTemp,1))//','//  &
            trim(RoundSigDigits(Condenser(CondID)%DesVolFlowRate,1))
      CASE(RefrigCondenserTypeCascade)

        SELECT CASE (Condenser(CondID)%CascadeTempControl)
          CASE(CascadeTempSet)
            ChrOut = 'Fixed'
          CASE(CascadeTempFloat)
            ChrOut = 'Floating'
        END SELECT ! cascade temperature control
        WRITE(OutputFileInits,103) ' Refrigeration Condenser:Cascade,'//  &
            TRIM(RoundSigDigits(CondID))//','// TRIM(Condenser(CondID)%Name)//','// TRIM(Chrout)//','//  &
            trim(RoundSigDigits(Condenser(CondID)%RatedTCondense,1))//','//  &
            trim(RoundSigDigits(Condenser(CondID)%RatedCapacity,1))//','//   &
            trim(RoundSigDigits(Condenser(CondID)%RatedApproachT,1))
    END SELECT !condenser type

    DO SubcoolerNum=1,System(SystemNum)%NumSubcoolers
      SubcoolerID=System(SystemNum)%SubcoolerNum(SubcoolerNum)
      SELECT CASE(Subcooler(SubcoolerID)%SubcoolerType)
        CASE(LiquidSuction)
          WRITE(OutputFileInits,103) ' Refrigeration Liquid Suction Subcooler,'//  &
             TRIM(RoundSigDigits(SubcoolerID))//','//  &
             TRIM(Subcooler(SubcoolerID)%Name)//','//  &
             trim(RoundSigDigits(Subcooler(SubcoolerID)%LiqSuctDesignDelT,1))//','//  &
             trim(RoundSigDigits(Subcooler(SubcoolerID)%LiqSuctDesignTliqIn,1))//','// &
             trim(RoundSigDigits(Subcooler(SubcoolerID)%LiqSuctDesignTvapIn,1))
        CASE(Mechanical)
          WRITE(OutputFileInits,103) ' Refrigeration Mechanical Subcooler,'//  &
             TRIM(RoundSigDigits(SubcoolerID))//','//  &
             TRIM(Subcooler(SubcoolerID)%Name)//','//TRIM(Subcooler(SubcoolerID)%MechSourceSys)//','//  &
             trim(RoundSigDigits(Subcooler(SubcoolerID)%MechControlTliqOut,1))
      END SELECT
    ENDDO !NumSubcoolers

  ENDDO  !NumRefrigSystems
END IF !(NumRefrigSystems > 0)

IF(NumTransRefrigSystems > 0)THEN
  WRITE(OutputFileInits,101) '#Detailed Transcritical Refrigeration Systems,'//TRIM(RoundSigDigits(NumTransRefrigSystems))
  DO TransSystemNum=1,NumTransRefrigSystems
    WRITE(OutputFileInits,101) ' Detailed Transcritical Refrigeration System,'//&
          TRIM(TransSystem(TransSystemNum)%Name)//','//TRIM(TransSystem(TransSystemNum)%RefrigerantName)//','//&
          TRIM(RoundSigDigits(TransSystem(TransSystemNum)%NumCasesMT))//','// &
          TRIM(RoundSigDigits(TransSystem(TransSystemNum)%NumCasesLT))//','// &
          TRIM(RoundSigDigits(TransSystem(TransSystemNum)%NumWalkInsMT))//','// &
          TRIM(RoundSigDigits(TransSystem(TransSystemNum)%NumWalkInsLT))//','// &
          TRIM(RoundSigDigits(TransSystem(TransSystemNum)%NumCompressorsHP))//','//&
          TRIM(RoundSigDigits(TransSystem(TransSystemNum)%NumCompressorsLP))//','//&
          trim(RoundSigDigits(GasCooler(TransSystem(TransSystemNum)%GasCoolerNum(1))%MinCondTemp,1))

    DO CaseNum=1,TransSystem(TransSystemNum)%NumCasesMT
      CaseID=TransSystem(TransSystemNum)%CaseNumMT(CaseNum)
      WRITE(OutputFileInits,103) ' Medium Temperature Refrigeration Case,'//  &
            TRIM(RoundSigDigits(CaseID))//','//  &
            TRIM(RefrigCase(CaseID)%Name)//','//  &
            TRIM(RefrigCase(CaseID)%ZoneName)//','//   &
            TRIM(RoundSigDigits(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            TRIM(NodeID(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            trim(RoundSigDigits(RefrigCase(CaseID)%RateTotCapPerLength,1))//','//  &
            trim(RoundSigDigits(RefrigCase(CaseID)%RatedLHR,2))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Temperature,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Length,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%OperatingFanPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%LightingPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%AntiSweatPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%DefrostPower,1))
    ENDDO !NumCasesMT on system
    DO CaseNum=1,TransSystem(TransSystemNum)%NumCasesLT
      CaseID=TransSystem(TransSystemNum)%CaseNumLT(CaseNum)
      WRITE(OutputFileInits,103) ' Low Temperature Refrigeration Case,'//  &
            TRIM(RoundSigDigits(CaseID))//','//  &
            TRIM(RefrigCase(CaseID)%Name)//','//  &
            TRIM(RefrigCase(CaseID)%ZoneName)//','//   &
            TRIM(RoundSigDigits(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            TRIM(NodeID(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            trim(RoundSigDigits(RefrigCase(CaseID)%RateTotCapPerLength,1))//','//  &
            trim(RoundSigDigits(RefrigCase(CaseID)%RatedLHR,2))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Temperature,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Length,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%OperatingFanPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%LightingPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%AntiSweatPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%DefrostPower,1))
    ENDDO !NumCasesLT on system
    DO  WalkInNum=1,TransSystem(TransSystemNum)%NumWalkInsMT
       WalkInID=TransSystem(TransSystemNum)%WalkInNumMT(WalkInNum)
      WRITE(OutputFileInits,103) ' Medium Temperature Refrigeration Walk In Cooler,'//  &
            TRIM(RoundSigDigits(WalkInID))//','//  &
            TRIM( WalkIn(WalkInID)%Name)//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignRatedCap,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%Temperature,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CoilFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CircFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignLighting,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeaterPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DefrostCapacity,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%NumZones))
       DO ZoneID=1, WalkIn( WalkInID)%NumZones
         WRITE(OutputFileInits,102) '   Walk-In Surfaces Facing Zone,'//  &
            TRIM( WalkIn(WalkInID)%ZoneName(ZoneID))//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%SurfaceArea(ZoneID),1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValue(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueStockDr(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueGlassDr(ZoneID),4))
    ENDDO !Num zones for each walk in on system
    ENDDO !NumWalkInsMT on system
    DO  WalkInNum=1,TransSystem(TransSystemNum)%NumWalkInsLT
       WalkInID=TransSystem(TransSystemNum)%WalkInNumLT(WalkInNum)
      WRITE(OutputFileInits,103) ' Low Temperature Refrigeration Walk In Cooler,'//  &
            TRIM(RoundSigDigits(WalkInID))//','//  &
            TRIM( WalkIn(WalkInID)%Name)//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignRatedCap,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%Temperature,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CoilFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CircFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignLighting,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeaterPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DefrostCapacity,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%NumZones))
       DO ZoneID=1, WalkIn( WalkInID)%NumZones
         WRITE(OutputFileInits,102) '   Walk-In Surfaces Facing Zone,'//  &
            TRIM( WalkIn(WalkInID)%ZoneName(ZoneID))//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%SurfaceArea(ZoneID),1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValue(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueStockDr(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueGlassDr(ZoneID),4))
    ENDDO !Num zones for each walk in on system
    ENDDO !NumWalkInsLT on system

    DO CompressorNum=1,TransSystem(TransSystemNum)%NumCompressorsHP
      CompID=TransSystem(TransSystemNum)%CompressorNumHP(CompressorNum)
      WRITE(OutputFileInits,103) ' High Pressure Refrigeration Compressor,'//  &
            TRIM(RoundSigDigits(CompID))//','//  &
            TRIM(Compressor(CompID)%Name)//','//  &
            trim(RoundSigDigits(Compressor(CompID)%NomCap,0))
    ENDDO !NumCompressorsHP
    DO CompressorNum=1,TransSystem(TransSystemNum)%NumCompressorsLP
      CompID=TransSystem(TransSystemNum)%CompressorNumLP(CompressorNum)
      WRITE(OutputFileInits,103) ' Low Pressure Refrigeration Compressor,'//  &
            TRIM(RoundSigDigits(CompID))//','//  &
            TRIM(Compressor(CompID)%Name)//','//  &
            trim(RoundSigDigits(Compressor(CompID)%NomCap,0))
    ENDDO !NumCompressorsLP

    IF(TransSystem(TransSystemNum)%NumGasCoolers >= 1)THEN
      GasCoolerID=TransSystem(TransSystemNum)%GasCoolerNum(1)
      WRITE(OutputFileInits,103) ' Refrigeration GasCooler:Air-Cooled,'//  &
        TRIM(RoundSigDigits(GasCoolerID))//','// TRIM(GasCooler(GasCoolerID)%Name)//','//  &
        TRIM(RoundSigDigits(GasCooler(GasCoolerID)%RatedOutletP,1))//','// &
        TRIM(RoundSigDigits(GasCooler(GasCoolerID)%RatedOutletT,1))//','// &
        TRIM(RoundSigDigits(GasCooler(GasCoolerID)%RatedApproachT,1))//','// &
        TRIM(RoundSigDigits(GasCooler(GasCoolerID)%RatedCapacity,1))//','// &
        TRIM(RoundSigDigits(GasCooler(GasCoolerID)%RatedFanPower,1))
    END IF  ! System(SystemNum)%NumGasCoolers >= 1

  ENDDO  !NumTransRefrigSystems
END IF !(NumTransRefrigSystems > 0)

IF(NumSimulationSecondarySystems > 0)THEN
  WRITE(OutputFileInits,101) '#Secondary Refrigeration Systems,'//TRIM(RoundSigDigits(NumSimulationSecondarySystems))
  DO SecondaryID=1,NumSimulationSecondarySystems
    SELECT CASE (Secondary(SecondaryID)%FluidType)
      CASE(SecFluidTypeAlwaysLiquid)
        WRITE(OutputFileInits,101) 'Secondary Refrigeration System: Fluid Always Liquid,'//  &
            TRIM(RoundSigDigits(SecondaryID))//','//  &
            TRIM(Secondary(SecondaryID)%Name)//','//  &
            TRIM(RoundSigDigits(Secondary(SecondaryID)%NumCases))//','//  &
            TRIM(RoundSigDigits(Secondary(SecondaryID)%NumWalkIns))//','//  &
            TRIM(Secondary(SecondaryID)%FluidName)//','//  &
            trim(RoundSigDigits(Secondary(SecondaryID)%CoolingLoadRated,1))//','// &
            trim(RoundSigDigits(Secondary(SecondaryID)%TEvapDesign,2))//','// &
            trim(RoundSigDigits(Secondary(SecondaryID)%TApproachDifRated,2))//','// &
            trim(RoundSigDigits(Secondary(SecondaryID)%TRangeDifRated,3))//','// &
            trim(RoundSigDigits(Secondary(SecondaryID)%PumpTotRatedPower,3))
      CASE(SecFluidTypePhaseChange)
        WRITE(OutputFileInits,101) 'Secondary Refrigeration System: Liquid Overfeed,'//  &
            TRIM(RoundSigDigits(SecondaryID))//','//  &
            TRIM(Secondary(SecondaryID)%Name)//','//  &
            TRIM(RoundSigDigits(Secondary(SecondaryID)%NumCases))//','//  &
            TRIM(RoundSigDigits(Secondary(SecondaryID)%NumWalkIns))//','//  &
            TRIM(Secondary(SecondaryID)%FluidName)//','//  &
            trim(RoundSigDigits(Secondary(SecondaryID)%CoolingLoadRated,1))//','// &
            trim(RoundSigDigits(Secondary(SecondaryID)%TEvapDesign,2))//','// &
            trim(RoundSigDigits(Secondary(SecondaryID)%TApproachDifRated,2))//','// &
            trim(RoundSigDigits(Secondary(SecondaryID)%CircRate,3))//','// &
            trim(RoundSigDigits(Secondary(SecondaryID)%PumpTotRatedPower,3))
      END SELECT
    DO CaseNum=1,Secondary(SecondaryID)%NumCases
      CaseID=Secondary(SecondaryID)%CaseNum(CaseNum)
      WRITE(OutputFileInits,103) 'Refrigeration Case,'//  &
            TRIM(RoundSigDigits(CaseID))//','//  &
            TRIM(RefrigCase(CaseID)%Name)//','//  &
            TRIM(RefrigCase(CaseID)%ZoneName)//','//   &
            TRIM(RoundSigDigits(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            TRIM(NodeID(RefrigCase(CaseID)%ZoneNodeNum))//','//  &
            trim(RoundSigDigits(RefrigCase(CaseID)%RateTotCapPerLength,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%RatedLHR,2))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Temperature,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%Length,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%OperatingFanPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%LightingPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%AntiSweatPower,1))//','// &
            trim(RoundSigDigits(RefrigCase(CaseID)%DefrostPower,1))
    ENDDO !NumCases on secondary on secondary system

    DO  WalkInNum=1,Secondary(SecondaryID)%NumWalkIns
      WalkInID = Secondary(SecondaryID)% WalkInNum( WalkInNum)
      WRITE(OutputFileInits,103) 'Walk In,'//  &
            TRIM(RoundSigDigits(WalkInID))//','//  &
            TRIM( WalkIn(WalkInID)%Name)//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignRatedCap,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%Temperature,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CoilFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn( WalkInID)%CircFanPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DesignLighting,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeaterPower,1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%DefrostCapacity,1))
       DO ZoneID=1, WalkIn(WalkInID)%NumZones
         WRITE(OutputFileInits,102) 'Walk In Surfaces Facing Zone,'//  &
            TRIM( WalkIn(WalkInID)%ZoneName(ZoneID))//','//  &
            trim(RoundSigDigits(WalkIn(WalkInID)%SurfaceArea(ZoneID),1))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValue(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightStockDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueStockDr(ZoneID),4))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%AreaGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%HeightGlassDr(ZoneID),2))//','// &
            trim(RoundSigDigits(WalkIn(WalkInID)%UValueGlassDr(ZoneID),4))
       ENDDO !zones for walk ins on secondary
     ENDDO ! walk ins on secondary

    DO CoilNum=1,Secondary(SecondaryID)%NumCoils
    CoilID = Secondary(SecondaryID)%CoilNum(CoilNum)
      WRITE(OutputFileInits,103) ' Air Chiller Load,'//  &
            TRIM(WareHouseCoil(CoilID)%Name)//','//  &
            TRIM(RoundSigDigits(CoilID))//','//  &
            TRIM(WareHouseCoil(CoilID)%ZoneName)
    ENDDO !numairchillers
  ENDDO !secondary
END IF !numsimulationsecondarys

IF(NumRefrigChillerSets > 0)THEN
  WRITE(OutputFileInits,101) '#ZoneHVAC/Refrigeration Air Chiller Sets,'//TRIM(RoundSigDigits(NumRefrigChillerSets))
  DO ChillerSetNum=1,NumRefrigChillerSets
    WRITE(OutputFileInits,101) 'ZoneHVAC/Refrigeration Air Chiller Set,'//  &
            TRIM(AirChillerSet(ChillerSetNum)%Name)//','//  &
            TRIM(RoundSigDigits(ChillerSetNum))//','//  &
            TRIM(RoundSigDigits(AirChillerSet(ChillerSetNum)%NumCoils))//','//  &
            TRIM(AirChillerSet(ChillerSetNum)%ZoneName)

    DO CoilNum=1,AirChillerSet(ChillerSetNum)%NumCoils
    CoilID = AirChillerSet(ChillerSetNum)%CoilNum(CoilNum)
    WRITE(OutputFileInits,103) ' Refrigeration Air Chiller,'//  &
            TRIM(RoundSigDigits(CoilID))//','//  &
            TRIM(WareHouseCoil(CoilID)%Name)//','//  &
            TRIM(WareHouseCoil(CoilID)%ZoneName)//','//   &
            TRIM(RoundSigDigits(WareHouseCoil(CoilID)%ZoneNodeNum))//','//  &
            TRIM(NodeID(WareHouseCoil(CoilID)%ZoneNodeNum))//','//  &
            trim(RoundSigDigits(WareHouseCoil(CoilID)%UnitLoadFactorSens,1))//','// &
            trim(RoundSigDigits(WareHouseCoil(CoilID)%RatedSensibleCap,2))//','// &
            trim(RoundSigDigits(WareHouseCoil(CoilID)%TEvapDesign,1))//','// &
            trim(RoundSigDigits(WareHouseCoil(CoilID)%RatedTemperatureDif,1))//','// &
            trim(RoundSigDigits(WareHouseCoil(CoilID)%RatedFanPower,1))//','// &
            trim(RoundSigDigits(WareHouseCoil(CoilID)%HeaterPower,1))//','// &
            trim(RoundSigDigits(WareHouseCoil(CoilID)%DefrostCapacity,1))//','// &
            trim(RoundSigDigits(WareHouseCoil(CoilID)%RatedAirVolumeFlow,1))
    ENDDO !numairchillers
  ENDDO !numrefrigchillersets
END IF !numrefrigchillersets

RETURN

END SUBROUTINE ReportRefrigerationComponents

!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalculateWalkIn(WalkInID)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL, May 2009
          !       DATE WRITTEN   Oct/Nov 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To model Walk In Coolers.

          ! METHODOLOGY EMPLOYED:
          ! Walk-in cooler performance is based on the ASHRAE load model, which includes
          ! infiltration through door openings and sensible loss through walls/ceilings identified
          ! by the user as sum of UA for each zone. A sub-floor heater is assumed to be sized so that
          ! the temperature of the slab beneath the floor insulation is the same as the ground
          ! temperature (to avoid ground freezing and heaving).
          ! All equipment loads (fan, light, heaters) are modeled as well.  Sensible and latent
          ! exchange with multiple adjoining zones is included. A master schedule is used for the Walk In operation and
          ! additional schedules control the lights, defrost, and heater operation.

          ! The fan is assumed to be off for Hot-Gas, Hot-Brine, and Electric defrost. The user can choose
          ! to include the load due to bringing the coil mass up from the evaporating temperature to the melting temperature
          !  if they choose.  Otherwise this factor is set to zero.

          ! Unmet loads are accumulated to be met the following time step.  This usually occurs during defrost and
          ! restocking.

          ! REFERENCES:
          ! ASHRAE 2006 Handbook, chapters 13 and 14.
          ! Gosney, W.B., Olama, G.A.-L., Heat and Enthalpy Gains through Cold Room Doorways,
          !     Proceedings of the Institute of Refrigeration, vol. 72, pp 31-41, 1975

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataLoopNode
  USE Psychrometrics,    ONLY: PsyRhoAirFnPbTdbW,RhoH2O,PsyWFnTdbTwbPb,PsyTwbFnTdbWPb,CPHW,&
                               PsyHFnTdbW,PsyTsatFnHPb, PsyWFnTdpPb,PsyHFnTdbRhPb, PsyRhFnTdbWPb, &
                               PsyTdpFnWPb, PsyWFnTdbH
!  USE DataEnvironment, ONLY:   OutBaroPress, OutDryBulbTemp
  USE DataEnvironment, ONLY:   OutBaroPress
  USE General,         ONLY:   CreateSysTimeIntervalString


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: WalkInID      ! Absolute pointer to  Walk In

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), Parameter ::  DefaultWalkInDoorOpenFactor = 0.05d0 ! walk in door open factor (fraction time open)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER      :: ZoneNodeNum             =0   ! Zone node number
INTEGER      :: ZoneNum                 =0   ! Index to zone
INTEGER      :: ZoneID                  =0   ! Index to zone
REAL(r64)    :: CapApplied              =0.0d0 !  Walk In total capacity at specific operating conditions
REAL(r64)    :: CircFanSchedule         =0.0d0
REAL(r64)    :: Conv                    =0.0d0 ! conversion factor in gravity equation
REAL(r64)    :: DefrostCap              =0.0d0 ! Design defrost capacity of WalkIn (W)
REAL(r64)    :: DefrostEnergy           =0.0d0 ! (J)
REAL(r64)    :: DefEnergyFraction = 0.0d0 !dimensionless
REAL(r64)    :: AvailDefrostEnergy = 0.0d0 !available to melt ice with temp term control (J)
REAL(r64)    :: DefrostLoad             =0.0d0 ! (W)
REAL(r64)    :: DefrostSchedule         =0.0d0 ! WalkIn defrost schedule, between 0 and 1
REAL(r64)    :: DefrostDripDownSchedule =0.0d0 ! WalkIn drip-down schedule (allows coil to drain after defrost)
REAL(r64)    :: DefrostEnergyNeeded     =0.0d0 ! Energy needed to melt all ice, used with temperature termination (J)
REAL(r64)    :: DensityAirWalkIn        =0.0d0 ! Density at Walk in temperature and 90% assumed RH
REAL(r64)    :: DensityZoneAir          =0.0d0 ! Density of the air in a particular zone (kg/m3)
REAL(r64)    :: DensityFactorFm         =0.0d0 ! called Fm in ASHRAE 2010 eq 13 page 24.5 for door infiltration
REAL(r64)    :: DensitySqRtFactor       =0.0d0 ! from ASHRAE 2010 eq 12 page 24.4 for door infiltration
REAL(r64)    :: DelTemp                 =0.0d0 ! Difference between zone and walk in temperatures (C)
REAL(r64)    :: DesignLighting          =0.0d0 ! Total design display WalkIn lighting power (W)
REAL(r64)    :: DesignRatedCap          =0.0d0 ! Rated capacity of walk in cooler coil (W)
REAL(r64)    :: DoorFlowFactor          =0.0d0 ! Derate compared to fully developed flow through 100% open door
REAL(r64)    :: DoorOpenFactor          =0.0d0 ! Derate based upon fraction time door opened
REAL(r64)    :: DoorProtectEff          =0.0d0 ! Door protection effectiveness
REAL(r64)    :: DrHeight                =0.0d0 ! Door height (m)
REAL(r64)    :: DrArea                  =0.0d0 ! Door area (m2)
REAL(r64)    :: EnthalpyAirWalkIn       =0.0d0 ! Enthalpy of air corresponding to walk in temperatuer and 90% assumed RH (J/kg)
REAL(r64)    :: EnthalpyZoneAir         =0.0d0 ! Enthalpy of the air in a particular zone (J/kg)
REAL(r64)    :: FanLoad                 =0.0d0 ! Total fan energy rate (W)
REAL(r64)    :: FloorLoad               =0.0d0 ! Total floor energy rate (W)
REAL(r64)    :: FrostChangekg           =0.0d0 ! Amount of frost added or melted  (kg)
REAL(r64)    :: FullFlowInfLoad         =0.0d0 ! Total load (lat + sens) due to 100% open doors w/ fully developed flow (W)
REAL(r64)    :: GlassDoorInfLoad        =0.0d0 ! infiltration through glass reach-in doors in a particular zone (W)
REAL(r64)    :: GlassDoorSensHeat       =0.0d0 ! sensible heat gain through glass reach-in doors (UA*delta T) (W)
REAL(r64)    :: GlassDoorArea           =0.0d0 ! facing a particular zone (m2)
REAL(r64)    :: Gravity                 =0.0d0 !
REAL(r64)    :: HeaterSchedule          =0.0d0 ! zero to one
REAL(r64)    :: HeaterLoad              =0.0d0 ! Total heater (except defrost) energy rate (W)
REAL(r64)    :: HumRatioAirWalkIn       =0.0d0 ! corresponds to walk in temp and 90% assumed RH(kg water/kg dry air)
REAL(r64)    :: HumRatioZoneAir         =0.0d0 ! Humidity Ratio of the air in a particular zone (kg water/kg dry air)
REAL(r64)    :: IceSensHeatNeeded       =0.0d0 ! Energy to raise frost temperature to 0C, used w/ temp termination (J)
REAL(r64)    :: LatentCapApplied        =0.0d0 ! Walk In latent capacity at specific operating conditions
REAL(r64)    :: LatentLoadTotal         =0.0d0 ! total latent load on WalkIn over all zones (W)
REAL(r64)    :: LightLoad               =0.0d0 ! Total lighting energy rate (W)
REAL(r64)    :: LightingSchedule        =0.0d0 ! WalkIn lighting schedule
REAL(r64)    :: LoadRequested           =0.0d0 ! Load necessary to meet current and all stored energy needs (W)
REAL(r64)    :: LoadTotal               =0.0d0 ! total load in time step (W)
REAL(r64)    :: MassDryAirRate          =0.0d0 ! Mass dry air infiltrating into/out-of walkin through doors (kg/s)
REAL(r64)    :: MaxCap                  =0.0d0 ! Design chilling capacity reduced according to drip-down schedule (W)
REAL(r64)    :: SensibleCapApplied      =0.0d0 ! Walk In sensible capacity at specific operating conditions
REAL(r64)    :: SensibleLoadTotal       =0.0d0 ! Total sensible load on WalkIn over all zones (W)
REAL(r64)    :: StoredEnergyRate        =0.0d0 ! Rate needed to serve all stored energy during single time step (W)
REAL(r64)    :: StartIceTemp            =0.0d0 ! Frost temperature at start of time step [C]
REAL(r64)    :: StartFrostkg            =0.0d0 ! frost load at start of time step (kg of ice)
REAL(r64)    :: StockDoorInfLoad        =0.0d0 ! infiltration through stock doors in a particular zone (W)
REAL(r64)    :: StockDoorSensHeat       =0.0d0 ! sensible heat gain through stock doors (UA*delta T) (W)
REAL(r64)    :: StockDoorArea           =0.0d0 ! (m2)
REAL(r64)    :: StockingLoad            =0.0d0 ! Total load due to stocking WalkIn product (W)
REAL(r64)    :: TWalkIn                 =0.0d0 ! WalkIn operating temperature (C)
REAL(r64)    :: UAOtherSurfaces         =0.0d0 ! UA for non-door surfaces facing a certain zone (W/C)
REAL(r64)    :: WalkInSchedule          =0.0d0 ! Current value of WalkIn operating (availability) schedule
REAL(r64)    :: WalkInSensLoad          =0.0d0 ! Walk in cooler sensible load facing particular zone (W)
REAL(r64)    :: WalkInLatLoad           =0.0d0 ! Walk in cooler latent load facing particular zone (W)
REAL(r64)    :: WaterRemovRate          =0.0d0 ! Walk in cooler removes water at this rate in this zone (kg/s)
REAL(r64)    :: ZoneDryBulb             =0.0d0 ! Dry Bulb Temperature of adjacent zone
REAL(r64)    :: ZoneSensLoad            =0.0d0 ! Sensible WalkIn credit delivered to a particular zone (W)
REAL(r64)    :: ZoneLatentLoad          =0.0d0 ! Latent WalkIn credit delivered to zone (W)
REAL(r64)    :: ZoneRHFrac              =0.0d0 ! Zone relative humidity fraction (decimal)
REAL(r64)    :: ZoneInfilLoad           =0.0d0 ! Walk in cooler infiltration load (sens + latent) in certain zone (W)
REAL(r64)    :: ZinfilSensLoad          =0.0d0 ! Sensible load due to infiltration in one zone
REAL(r64)    :: ZdoorSensLoad           =0.0d0 ! Sensible load due to UA delta T through closed door in one zone

  WalkInSchedule = GetCurrentScheduleValue( WalkIn( WalkInID)%SchedPtr)
  IF ( WalkInSchedule <= 0) RETURN
  ! GET OTHER SCHEDULES
  DefrostSchedule         = GetCurrentScheduleValue( WalkIn( WalkInID)%DefrostSchedPtr)
  DefrostDripDownSchedule = GetCurrentScheduleValue( WalkIn( WalkInID)%DefrostDripDownSchedPtr)
  !next statement In case user doesn't understand concept of drip down schedule
  DefrostDripDownSchedule = MAX(DefrostDripDownSchedule,DefrostSchedule)

  !next four values optional, so set to default before checking for schedule
  StockingLoad     = 0.0d0
  LightingSchedule = 1.0d0
  HeaterSchedule   = 1.0d0
  CircFanSchedule  = 1.0d0
  IF (WalkIn(WalkInID)%StockingSchedPtr > 0)StockingLoad    = GetCurrentScheduleValue(WalkIn(WalkInID)%StockingSchedPtr)
  IF (WalkIn(WalkInID)%LightingSchedPtr > 0)LightingSchedule= GetCurrentScheduleValue(WalkIn(WalkInID)%LightingSchedPtr)
  IF (WalkIn(WalkInID)%HeaterSchedPtr > 0)  HeaterSchedule  = GetCurrentScheduleValue(WalkIn(WalkInID)%HeaterSchedPtr)
  IF (WalkIn(WalkInID)%CircFanSchedPtr > 0) CircFanSchedule = GetCurrentScheduleValue(WalkIn(WalkInID)%CircFanSchedPtr)

  !Set local subroutine variables for convenience
  TWalkIn             =  WalkIn( WalkInID)%Temperature
  DesignRatedCap      =  WalkIn( WalkInID)%DesignRatedCap
  DefrostCap          =  WalkIn( WalkInID)%DefrostCapacity
      ! %DefrostCapacity already set to zero for WalkInDefrostNone , WalkInDefrostOffCycle
  DesignLighting      =  WalkIn( WalkInID)%DesignLighting
  EnthalpyAirWalkIn   =  PsyHFnTdbRhPb(TWalkIn,0.9d0,OutBaroPress)!assume 90%RH in cooler
  HumRatioAirWalkIn   =  PsyWFnTdbH(TWalkIn,EnthalpyAirWalkIn)
  DensityAirWalkIn    =  PsyRhoAirFnPbTdbW(OutBaroPress,TWalkIn,HumRatioAirWalkIn)
  Conv                =  Latitude*2.d0*PI/360.d0   !Convert Latitude to radians
  Gravity=9.780373d0*(1.d0+.0052891d0*(SIN(CONV))**2-.0000059d0*(SIN(2.d0*CONV))**2)

! CALCULATE ALL LOADS INFLUENCED BY ZONE TEMPERATURE AND RH
  !set to zero before summing over zones
  SensibleLoadTotal = 0.d0
  LatentLoadTotal = 0.d0
  WalkIn(WalkInID)%SensZoneCreditRate   = 0.0d0
  WalkIn(WalkInID)%SensZoneCreditCoolRate   = 0.0d0
  WalkIn(WalkInID)%SensZoneCreditCool       = 0.0d0
  WalkIn(WalkInID)%SensZoneCreditHeatRate   = 0.0d0
  WalkIn(WalkInID)%SensZoneCreditHeat       = 0.0d0
  WalkIn(WalkInID)%LatZoneCreditRate        = 0.0d0

 !Start zone loop:
 DO ZoneID = 1,WalkIn(WalkInID)%NumZones
  ZoneSensLoad      = 0.d0
  GlassDoorSensHeat = 0.d0
  StockDoorSensHeat = 0.d0
  ZoneNum         =  WalkIn(WalkInID)%ZoneNum(ZoneID)
  ZoneNodeNum     =  WalkIn(WalkInID)%ZoneNodeNum(ZoneID)
  ZoneDryBulb     =  Node(ZoneNodeNum)%Temp
  DelTemp         =  ZoneDryBulb - TWalkIn
  StockDoorArea   =  WalkIn(WalkInID)%AreaStockDr(ZoneID)
  GlassDoorArea   =  WalkIn(WalkInID)%AreaGlassDr(ZoneID)
  UAOtherSurfaces =  WalkIn(WalkInID)%SurfaceArea(ZoneID)*WalkIn(WalkInID)%UValue(ZoneID)
  DoorFlowFactor = 0.8d0  !see ASHRAE Refrigeration, p13.5, 2006
  IF (Deltemp <= 11.d0) DoorFlowFactor = 1.1d0  ! from ASHRAE Refrigeration Loads

  !Get infiltration loads if either type of door is present in this zone
  IF(StockDoorArea > 0.d0 .OR. GlassDoorArea > 0.d0) THEN
    ZoneRHFrac      =  PsyRhFnTdbWPb(Node(ZoneNodeNum)%Temp,Node(ZoneNodeNum)%Humrat,OutBaroPress,'CalculateWalkIn')
    EnthalpyZoneAir =  PsyHFnTdbRhPb(ZoneDryBulb,ZoneRHFrac,OutBaroPress,'CalculateWalkIn')
    HumRatioZoneAir =  PsyWFnTdbH(ZoneDryBulb,EnthalpyZoneAir,'CalculateWalkIn')
    DensityZoneAir  =  PsyRhoAirFnPbTdbW(OutBaroPress,ZoneDryBulb,HumRatioZoneAir,'CalculateWalkIn')
    IF (DensityZoneAir < DensityAirWalkIn)THEN !usual case when walk in is colder than zone
      DensitySqRtFactor = (1.d0 - DensityZoneAir/DensityAirWalkIn)**0.5d0
      DensityFactorFm = (2.d0/(1.d0 + (DensityAirWalkIn/DensityZoneAir)**0.333d0))**1.5d0
    ELSE !temperature inversion with zone colder and/or drier than walk-in, infiltration in reverse direction
      !The enthalpy difference will show whether the energy transport is reversed
      !(same air mass exchange in either direction )
      !That is, these factors establish the magnitude of the exchange air flow, not direction
      DensitySqRtFactor = (1.d0 - DensityAirWalkIn/DensityZoneAir)**0.5d0
      DensityFactorFm = (2.d0/(1.d0 + (DensityZoneAir/DensityAirWalkIn)**0.333d0))**1.5d0
    END IF ! check for density in zone and in walk-in to avoid taking sqrt of neg number
    GlassDoorInfLoad = 0.d0
    StockDoorInfLoad = 0.d0
    StockDoorSensHeat = 0.d0
    GlassDoorSensHeat = 0.d0
    IF(StockDoorArea > 0.d0) THEN
      SELECT CASE (WalkIn(WalkInID)%StockDoorProtectType(ZoneID))
        !Values from ASHRAE Ref p 13.6
        CASE (WIStockDoorNone)
          DoorProtectEff = 0.0d0
        CASE (WIStockDoorAirCurtain)
          DoorProtectEff = 0.5d0
        CASE (WIStockDoorStripCurtain)
          DoorProtectEff = 0.9d0
      END SELECT
      DrHeight = WalkIn(WalkInID)%HeightStockDr(ZoneID)
      DrArea   = StockDoorArea
      ! if exists, get Stock Door Zone schedule
      DoorOpenFactor = DefaultWalkInDoorOpenFactor
      IF (WalkIn(WalkInID)%StockDoorOpenSchedPtr(ZoneID) > 0)&
          DoorOpenFactor = GetCurrentScheduleValue( WalkIn( WalkInID)%StockDoorOpenSchedPtr(ZoneID))

      FullFlowInfLoad = 0.221d0*DrArea*(EnthalpyZoneAir - EnthalpyAirWalkIn)* &
                        DensityAirWalkIn * DensitySqRtFactor * &
                        ((Gravity*DrHeight)**0.5d0)*DensityFactorFm
      StockDoorInfLoad = FullFlowInfLoad*DoorOpenFactor*DoorFlowFactor*(1.d0 - DoorProtectEff)
      StockDoorSensHeat = DrArea*WalkIn(WalkInID)%UValueStockDr(ZoneID)*DelTemp
    END IF !have stock doors

    IF(GlassDoorArea > 0.d0) THEN
      DoorProtectEff = 0.5d0 ! Assume glass doors have air curtain
      DrHeight = WalkIn(WalkInID)%HeightGlassDr(ZoneID)
      DrArea = GlassDoorArea
      ! get Glass Door Zone schedule
      DoorOpenFactor   = DefaultWalkInDoorOpenFactor  !default value
      IF ( WalkIn( WalkInID)%GlassDoorOpenSchedPtr(ZoneID) > 0) &
        DoorOpenFactor = GetCurrentScheduleValue( WalkIn( WalkInID)%GlassDoorOpenSchedPtr(ZoneID))

      FullFlowInfLoad = 0.221d0*DrArea*(EnthalpyZoneAir - EnthalpyAirWalkIn)* &
                        DensityAirWalkIn * DensitySqRtFactor * &
                        ((Gravity*DrHeight)**0.5d0)*DensityFactorFm
      GlassDoorInfLoad = FullFlowInfLoad*DoorOpenFactor*DoorFlowFactor*(1.d0 - DoorProtectEff)
      GlassDoorSensHeat = DrArea*WalkIn(WalkInID)%UValueGlassDr(ZoneID)*DelTemp
    END IF !have Glass doors

    !assume mass dry air infiltrating into walk-in == mass out into zone,
    !                       that is, equal air exchange (ASHRAE 2006 Refrigeration)
    ZoneInfilLoad  = -StockDoorInfLoad - GlassDoorInfLoad
    MassDryAirRate = -ZoneInfilLoad/(EnthalpyZoneAir - EnthalpyAirWalkIn)
    WaterRemovRate = MassDryAirRate*(HumRatioZoneAir - HumRatioAirWalkIn)
      ! Just as with cases,  we assume no latent credit (water removal = 0) to zone or load on cooler during dripdown
      ! To be consistent with the treatment of refrigerated cases, latent load
      !  and latent credit are bothbased on reducing the infiltrating vapor to ice.  (This is
      !  slightly greater than if the latent credit were based upon condensing out the water as liquid.)
      !  then it would be: ZoneLatentLoad = -WaterRemovRate * WaterToVaporEnthalpy * (1.d0-DefrostDripDownSchedule)
    ZoneLatentLoad = -WaterRemovRate * IcetoVaporEnthalpy * (1.d0-DefrostDripDownSchedule)
    ZInfilSensLoad = ZoneInfilLoad - (-WaterRemovRate * IcetoVaporEnthalpy) !done to avoid moving latent to sens during dripdown
    ZdoorSensLoad  = - GlassDoorSensHeat - StockDoorSensHeat
    WalkInLatLoad  = -ZoneLatentLoad
    IF (WalkIn( WalkInID)%TEvapDesign <= 0.d0) THEN ! water turned to ice on coil
      WalkInLatLoad = WaterRemovRate*IcetoVaporEnthalpy * (1.d0-DefrostDripDownSchedule)
      ! FROST:  keep track of frost build up on evaporator coil
      !         avoid accumulation during warm-up to avoid reverse dd test problem
      IF(.NOT. WarmUpFlag) THEN
        FrostChangekg = (WaterRemovRate * TimeStepZone * SecInHour) * (1.0d0-DefrostDripDownSchedule)
        WalkIn(WalkInID)%KgFrost = WalkIn(WalkInID)%KgFrost + FrostChangekg
      END IF
    END IF !water to ice
  END IF !No doors

  ZoneSensLoad   = ZinfilSensLoad + ZdoorSensLoad -  UAOtherSurfaces*Deltemp
  WalkInSensLoad = -ZoneSensLoad

! Update globals for use in ZoneTemperaturePredictorCorrector (Air Heat Balance) and
!   Zone Equipment Manager. Sum walk-in credits to zone using existing 'casecredit' variable
!   No return air fractions are applied to walk-ins, and no latent in stocking -

   RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone = &
            RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone + ZoneSensLoad
   RefrigCaseCredit(ZoneNum)%LatCaseCreditToZone = &
            RefrigCaseCredit(ZoneNum)%LatCaseCreditToZone + ZoneLatentLoad

 ! Set up report variables for each zone for this walk-in
 ! Sensible heat exchange can be positive or negative, split into separate output variables and always report positive value
  WalkIn(WalkInID)%SensZoneCreditRate(ZoneID) = ZoneSensLoad
  IF(ZoneSensLoad <= 0.0d0) THEN
     WalkIn(WalkInID)%SensZoneCreditCoolRate(ZoneID) = -ZoneSensLoad
     WalkIn(WalkInID)%SensZoneCreditCool(ZoneID)     = -ZoneSensLoad * TimeStepZone * SecInHour
     WalkIn(WalkInID)%SensZoneCreditHeatRate(ZoneID) = 0.0d0
     WalkIn(WalkInID)%SensZoneCreditHeat(ZoneID)     = 0.0d0
  ELSE
     WalkIn(WalkInID)%SensZoneCreditHeatRate(ZoneID) = ZoneSensLoad
     WalkIn(WalkInID)%SensZoneCreditHeat(ZoneID)     = ZoneSensLoad * TimeStepZone * SecInHour
     WalkIn(WalkInID)%SensZoneCreditCoolRate(ZoneID) = 0.0d0
     WalkIn(WalkInID)%SensZoneCreditCool(ZoneID)     = 0.0d0
  END IF
  ! This rate should always be negative
  WalkIn(WalkInID)%LatZoneCreditRate(ZoneID)        = ZoneLatentLoad
  WalkIn(WalkInID)%LatZoneCredit(ZoneID)            = ZoneLatentLoad * TimeStepZone * SecInHour

  !Running total over all zones, use later to dispatch capacity
  SensibleLoadTotal = SensibleLoadTotal  + WalkInSensLoad
  LatentLoadTotal   = LatentLoadTotal  + WalkInLatLoad

END DO !Do loop over zones for zone-condition-related sensible and latent loads

  !cooling coil fan power default is 375W, = 1/2 HP (Tyler showed 1/3 to 3/4 hp)

  ! CALCULATE AUX LOADS DUE TO LIGHTS, FANS AND HEATERS
  LightLoad         = DesignLighting * LightingSchedule
  ! turn coil fan off during defrost/drip - down period
  FanLoad    =  WalkIn(WalkInID)%CircFanPower*CircFanSchedule + &
                 WalkIn( WalkInID)%CoilFanPower * ( 1.0d0 - DefrostDripDownSchedule )
  HeaterLoad = WalkIn(WalkInID)%HeaterPower * HeaterSchedule
  ! Calculate floor load - using 'GroundTemp' assigned in weather manager (can be entered by user if desired)
  !    Default value is 18C.
  FloorLoad = WalkIn(WalkInID)%FloorArea * WalkIn( WalkInID)%FloorUValue * (GroundTemp - TWalkIn)


!DEFROST CALCULATIONS
  IF((DefrostSchedule > 0.0d0).AND. &
      (WalkIn(WalkInID)%DefrostType /= WalkInDefrostNone) .AND. &
      (WalkIn(WalkInID)%DefrostType /=  WalkInDefrostOffCycle)) THEN
      DefrostLoad = DefrostCap*DefrostSchedule  !W
      StartFrostKg =  WalkIn(WalkInID)%KgFrost
      DefrostEnergy = DefrostLoad * TimeStepZone*SecInHour !Joules
      IF (WalkIn(WalkInID)%DefrostControlType == DefrostContTempTerm) THEN
        !  Need to turn defrost system off early if controlled by temperature and all ice melted
        !  For temperature termination, need to recognize not all defrost heat goes to melt ice
        !  Some goes to misc losses (for fluid defrost, some coil areas bare earlier than
        !  others and xfer heat to environment)
        !  Assume full ice melting satisfies temperature control.
        !      (defaults for DefEnergyFraction are :=0.7 for elec, =0.3 for fluids)
        DefEnergyFraction = WalkIn(WalkInID)%DefEnergyFraction
        AvailDefrostEnergy = DefEnergyFraction * DefrostEnergy !Joules avail to melt ice
        IceSensHeatNeeded = 0.d0
        IF(StartFrostKg > 0.0d0) THEN
          IF(WalkIn(WalkInID)%IceTemp < 0.d0) THEN
            StartIceTemp  = WalkIn(WalkInID)%IceTemp
            IceSensHeatNeeded = StartFrostKg * SpecificHeatIce * (0.0d0 - StartIceTemp)!Joules
            IF (AvailDefrostEnergy >= IceSensHeatNeeded) THEN
              WalkIn(WalkInID)%IceTemp = 0.d0
              AvailDefrostEnergy = AvailDefrostEnergy - IceSensHeatNeeded !Joules
            ELSE   !DefrostEnergy < IceSensHeatNeeded
              WalkIn(WalkInID)%IceTemp = StartIceTemp + AvailDefrostEnergy/(SpecificHeatIce * StartFrostKg)
              AvailDefrostEnergy = 0.d0
            END IF ! AvailDefrostEnergy >= IceSensHeatNeeded
          END IF   ! IceTemp < 0,  need to raise temperature of ice
          !Reduce defrost heat load on walkin by amount of ice melted during time step
          FrostChangekg = MIN(AvailDefrostEnergy/IceMeltEnthalpy,StartFrostKg)
          IF(FrostChangekg < StartFrostKg) THEN
            DefrostLoad = DefrostLoad - FrostChangekg*IceMeltEnthalpy/TimeStepZone/SecInHour
            IF(.NOT. WarmUpFlag)WalkIn( WalkInID)%KgFrost  = StartFrostKg  - FrostChangekg
            !DefrostSchedule not changed
          ELSE     ! all frost melted during time step, so need to terminate defrost
                   !  see Aug 8 page 3 notes
            WalkIn( WalkInID)%KgFrost  = 0.d0
            DefrostEnergyNeeded = (IceSensHeatNeeded + (FrostChangekg*IceMeltEnthalpy))/ &
                                  DefEnergyFraction  !Joules - energy needed including E unavail to melt ice
            DefrostSchedule  = MIN(DefrostSchedule,(DefrostEnergyNeeded/(DefrostCap*TimeStepZone*SecInHour)))
            ! reduce load on walkin by energy put into ice melting
            DefrostLoad = MAX(0.d0,(DefrostSchedule*DefrostCap -  &
                         (IceSensHeatNeeded + (FrostChangekg*IceMeltEnthalpy))/(TimeStepZone*SecInHour)))
          WalkIn(WalkInID)%IceTemp = WalkIn(WalkInID)%TEvapDesign

          END IF   ! frost melted during time step less than amount of ice at start
        ELSE       ! no frost present so terminate defrost and reset ice temperature for start of next defrost
          DefrostLoad   = 0.d0
          DefrostSchedule = 0.d0
          WalkIn(WalkInID)%IceTemp = WalkIn(WalkInID)%TEvapDesign
        END IF     ! have frost present

      ELSE !Not temperature control type
        FrostChangekg = MIN(DefrostEnergy/IceMeltEnthalpy,StartFrostKg)
        !Reduce defrost heat load on walkin by amount of ice melted during time step
        DefrostLoad = DefrostLoad - FrostChangekg*IceMeltEnthalpy/TimeStepZone/SecInHour
        IF(.NOT. WarmUpFlag)WalkIn( WalkInID)%KgFrost  = StartFrostKg  - FrostChangekg
        !DefrostSchedule not changed
      END IF       !Temperature termination control type

  ELSE    !DefrostSchedule <= 0 or have None or OffCycle
    DefrostLoad   = 0.0d0
  END IF     !Defrost calculations

  IF(WalkIn(WalkInID)%DefrostType == WalkInDefrostElec) THEN
        WalkIn( WalkInID)%ElecDefrostConsumption = DefrostCap*DefrostSchedule*TimeStepZone*SecInHour
        WalkIn(WalkInID)%ElecDefrostPower = DefrostCap*DefrostSchedule
  ELSE
        WalkIn( WalkInID)%ElecDefrostConsumption = 0.d0
        WalkIn(WalkInID)%ElecDefrostPower = 0.d0
  END IF

    ! If hot brine or hot gas is used for defrost, need to reduce condenser load by heat reclaimed for defrost
    IF(WalkIn(WalkInID)%DefrostType == WalkInDefrostFluid)WalkIn(WalkInID)%HotDefrostCondCredit=DefrostCap*DefrostSchedule

  ! loads reflects that walk ins continue to accumulate loads, even during defrost
  ! but cap is used to report portion met by active system while operating

   !*** See if capacity meets load and manage accumulated stored energy ***********************************
  SensibleLoadTotal = SensibleLoadTotal + LightLoad + HeaterLoad + FanLoad + &
                      StockingLoad + DefrostLoad + FloorLoad
  LoadTotal         = SensibleLoadTotal + LatentLoadTotal

!
  !Account for difference between load and capacity. Assume rack or system able to provide
  ! rated capacity.  If it can't, that unmet energy will be stored and discharged at the system level.
  !  Here we are calculating the load the walk-in cooler places on the refrigeration compressor systems.
  !  Meet current load to the extent possible.  If extra capacity available,
  !  apply it to previously unmet/stored loads.  If capacity less than current load,
  !  (e.g. as it is during defrost cycles) save the unmet/stored load to be met in
  !  succeeding time steps. This is an artificial way of recognizing that the internal
  !  temperature will increase by a small amount during defrost and the system will have to
  !  run full out until the temperature is brought back down.

  StoredEnergyRate  =  WalkIn( WalkInID)%StoredEnergy/TimeStepZone/SecInHour
  LoadRequested      = LoadTotal  + StoredEnergyRate

  ! prorate available cooling capacity for portion of time off due to drip down.
  MaxCap = DesignRatedCap*(1.d0 - DefrostDripDownSchedule)
  IF(MaxCap >= LoadRequested) THEN
      !Have more at least as much capacity available as needed, even counting stored energy
      CapApplied    = LoadRequested
      SensibleCapApplied = SensibleLoadTotal + StoredEnergyRate
      LatentCapApplied   = LatentLoadTotal
      WalkIn( WalkInID)%StoredEnergy = 0.0d0
  ELSE
      !Don't have as much capacity as needed (during dripdown or period following dripdown)
      CapApplied    = MaxCap
      LatentCapApplied   = MIN(LatentLoadTotal,MaxCap)  !Latent load should never be > capavail, but just in case...
      SensibleCapApplied = CapApplied - LatentCapApplied
      IF(.NOT. WarmUpFlag) &
        WalkIn( WalkInID)%StoredEnergy = WalkIn( WalkInID)%StoredEnergy + &
                                        (LoadTotal - MaxCap)*TimeStepZone*SecInHour
  END IF !CapAvail vs Load requested

 ! ReportWalkIn( WalkInID)
   WalkIn( WalkInID)%TotalCoolingLoad            = CapApplied
   WalkIn( WalkInID)%TotalCoolingEnergy          = CapApplied * TimeStepZone * SecInHour
   WalkIn( WalkInID)%TotSensCoolingEnergyRate    = SensibleCapApplied
   WalkIn( WalkInID)%TotSensCoolingEnergy        = SensibleCapApplied * TimeStepZone * SecInHour
   WalkIn( WalkInID)%TotLatCoolingEnergyRate     = LatentCapApplied
   WalkIn( WalkInID)%TotLatCoolingEnergy         = LatentCapApplied * TimeStepZone * SecInHour

   WalkIn( WalkInID)%ElecFanPower             = FanLoad
   WalkIn( WalkInID)%ElecFanConsumption       = FanLoad * TimeStepZone * SecInHour
   WalkIn( WalkInID)%ElecHeaterPower          = HeaterLoad
   WalkIn( WalkInID)%ElecHeaterConsumption    = HeaterLoad * TimeStepZone * SecInHour
   WalkIn( WalkInID)%ElecLightingPower        = LightLoad
   WalkIn( WalkInID)%ElecLightingConsumption  = LightLoad * TimeStepZone * SecInHour
   WalkIn( WalkInID)%TotalElecPower           = FanLoad + HeaterLoad + LightLoad + WalkIn( WalkInID)%ElecDefrostPower
   WalkIn( WalkInID)%TotalElecConsumption     = WalkIn( WalkInID)%TotalElecPower * TimeStepZone * SecInHour


!**************************************************************************************************
! Cap Energy and Kg Frost to avoid floating overflow errors
! 1-time warning is issued. It should be rare but could happen with unrealistic inputs.
  IF( WalkIn( WalkInID)%StoredEnergy > MyLargeNumber) THEN
       WalkIn( WalkInID)%StoredEnergy=MyLargeNumber
    IF(ShowUnMetWIEnergyWarning(WalkInID)) THEN
        CALL ShowWarningError('Refrigeration:WalkIn: '//TRIM( WalkIn( WalkInID)%Name))
        CALL ShowContinueError(' This walk-in cooler has insufficient capacity to meet the loads')
        CALL ShowContinueError('... Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                     //TRIM(CreateSysTimeIntervalString()))
        CALL ShowContinueError(' Refer to documentation for further explanation of Total Cooling Capacity.')
        ShowUnMetWIEnergyWarning(WalkInID) = .FALSE.
    END IF  ! ShowStoreEnergyWarning
 END IF ! stored energy > large number
  IF( WalkIn( WalkInID)%KgFrost > MyLargeNumber) THEN
     WalkIn( WalkInID)%KgFrost=MyLargeNumber
     IF(ShowWIFrostWarning( WalkInID)) THEN
      CALL ShowWarningError('Refrigeration:WalkIn: '//TRIM( WalkIn( WalkInID)%Name))
      CALL ShowContinueError(' This walkin cooler has insufficient defrost capacity to remove the excess frost accumulation.')
      CALL ShowContinueError(' Check the defrost schedule or defrost capacity. ')
      CALL ShowContinueError('... Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                     //TRIM(CreateSysTimeIntervalString()))
      ShowWIFrostWarning( WalkInID) = .FALSE.
    END IF
  END IF

  RETURN

END SUBROUTINE CalculateWalkIn


!***************************************************************************************************
!***************************************************************************************************

SUBROUTINE CalculateSecondary(SecondaryNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL
          !       DATE WRITTEN   Spring 2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the total cooling load, pump power, and needed primary refrigerant supply temperature
          ! for a secondary system.

          ! METHODOLOGY EMPLOYED:
          ! Sum the loads for the cases and walk-ins supplied by a secondary loop.
          ! Calculate the pumping power.
          ! Assume that the fluid supply and return temperatures are fixed and the
          ! fluid flow rate is varied to meed the variable load.
          ! User has been told in IO and Eng ref: for secondary systems/pumps: pump energy is f(viscosity),
          !        but since specifying Tcircfluid as steady
          !        state in loop, specify power for fluid and system head/resistance at that temp
          !ashrae 2006 p4.1 supports 78% eff for pump impellers
          !  all power into heat because it would otherwise not be counted in zone
          !  if use semihermetic motor, also need to add motor ineff as heat

          ! REFERENCES:
          ! SCE report
          !  others

          ! USE STATEMENTS:
  USE CurveManager,      ONLY : CurveValue
  USE Psychrometrics,    ONLY: PsyRhoAirFnPbTdbW,PsyWFnTdbTwbPb,PsyTwbFnTdbWPb,&
                               PsyHFnTdbW,PsyTsatFnHPb, PsyWFnTdpPb,PsyHFnTdbRhPb
!unused  USE DataWater,         ONLY: WaterStorage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: SecondaryNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER ::ErrorTol       = 0.001d0 !Iterative solution tolerance

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL     :: AtPartLoad        ! Whether or not need to iterate on pump power
  LOGICAL     :: DeRate            ! If true, need to derate aircoils because don't carry over unmet energy
  Integer     :: CaseID            ! used in summing case loads on loop
  Integer     :: CaseNum           ! used in summing case loads on loop
  Integer     :: CoilID            ! used in summing coil loads on loop
  Integer     :: CoilIndex         ! used in summing coil loads on loop
  Integer     :: DistPipeZoneNum   ! used to assign case credit to zone
  Integer     :: Iter              ! loop counter
  Integer     :: NumPumps          ! number of pumps (or stages if used to approx var speed) on loop
  Integer     :: PumpID            ! loop counter
  Integer     :: ReceiverZoneNum   ! used to assign case credit it zone
  Integer     :: WalkInID          ! used in summing walk-in loads on loop
  Integer     :: WalkInIndex       ! used in summing walk-in loads on loop
  Integer     :: ZoneNodeNum       ! used to establish environmental temperature for dist piping heat gains
  REAL(r64)   :: CpBrine           ! Specific heat (W/kg)
  REAL(r64)   :: CircRatio         ! Per ASHRAE definition = mass flow at pump/mass flow to condenser
  REAL(r64)   :: DensityBrine      ! Density (kg/m3)
  REAL(r64)   :: DiffTemp          ! (C)
  REAL(r64)   :: DistPipeHeatGain  ! Optional (W)
  REAL(r64)   :: Error             ! Used in iterative soln for pumps needed to meet load (that has to include pump energy)
  REAL(r64)   :: Eta               ! Secondary loop heat exchanger eta, dimensionless
  REAL(r64)   :: FlowVolNeeded     ! Flow rate needed to meet load (m3/s)
  REAL(r64)   :: LoadRequested           =0.0d0 ! Load necessary to meet current and all stored energy needs (W)
  REAL(r64)   :: LocalTimeStep = 0.0d0 !TimeStepZone for case/walkin systems, TimeStepSys for coil systems
  REAL(r64)   :: MaxLoad           ! Secondary loop capacity can be limited by heat exchanger or pumps (W)
  REAL(r64)   :: MaxVolFlow        ! Flow can be limited by either total pump capacity or heat exchanger design (m3/s)
  REAL(r64)   :: PartLdFrac        ! Used to ratio pump power
  REAL(r64)   :: PartPumpFrac      ! Used to see if part pumps dispatched meets part pump load
  !REAL(r64)   :: PartPower         ! Used to ratio power for last pump added to loop
  REAL(r64)   :: PrevTotalLoad     ! Used in pump energy convergence test
  REAL(r64)   :: ReceiverHeatGain  ! Optional (W)
  REAL(r64)   :: RefrigerationLoad ! Load for cases and walk-ins served by loop, does not include pump energy (W)
  REAL(r64)   :: StoredEnergyRate  ! Used to meet loads unmet in previous time step (related to defrost cycles
                                   !     on cases/walk-ins served)(W)
  REAL(r64)   :: TBrineAverage     ! (C)
  REAL(r64)   :: TBrineIn          ! Brine temperature going to heat exchanger, C
  REAL(r64)   :: TCondense         ! Condensing temperature for a phase change secondary loop, C
  REAL(r64)   :: TEvap             ! Evaporating temperature in secondary loop heat exchanger (C)
  REAL(r64)   :: TotalCoolingLoad  ! Cooling load reported back to compressor rack or detailed system (W)
  REAL(r64)   :: TotalHotDefrostCondCredit ! Used to credit condenser when heat reclaim used for hot gas/brine defrost (W)
  REAL(r64)   :: TotalPumpPower    ! Total Pumping power for loop, W
  REAL(r64)   :: TotalLoad         ! Total Cooling Load on secondary loop, W
  REAL(r64)   :: TPipesReceiver    ! Temperature used for contents of pipes and/or receiver in calculating shell losses (C)
  REAL(r64)   :: VarFrac           ! Pump power fraction for variable speed pump, dimensionless
  REAL(r64)   :: VolFlowRate       ! Used in dispatching pumps to meet load (m3/s)
  REAL(r64)   :: UnmetEnergy       ! Cumulative, grows and shrinks with defrost cycles on loads served by loop (J)

LocalTimeStep = TimeStepZone
IF(UseSysTimeStep) LocalTimeStep = TimeStepSys

 NumPumps     = Secondary(SecondaryNum)%NumPumps
 TEvap        = Secondary(SecondaryNum)%TEvapDesign
 MaxVolFlow   = Secondary(SecondaryNum)%MaxVolFlow
 MaxLoad      = Secondary(SecondaryNum)%MaxLoad
 UnMetEnergy  = Secondary(SecondaryNum)%UnMetEnergy
 SELECT CASE (Secondary(SecondaryNum)%FluidType)
    CASE(SecFluidTypeAlwaysLiquid)
      CpBrine      = Secondary(SecondaryNum)%CpBrineRated
      DensityBrine = Secondary(SecondaryNum)%DensityBrineRated
      Eta          = Secondary(SecondaryNum)%HeatExchangeEta
      TBrineAverage= Secondary(SecondaryNum)%TBrineAverage
      TBrineIn     = Secondary(SecondaryNum)%TBrineInRated
      TPipesReceiver = TBrineAverage
    CASE(SecFluidTypePhaseChange)
      CircRatio = Secondary(SecondaryNum)%CircRate
      TCondense = Secondary(SecondaryNum)%TCondense
      TPipesReceiver = TCondense
 END SELECT ! Fluid type

    !Initialize this secondary for this time step
    TotalLoad                    = 0.0d0
    TotalPumpPower               = 0.0d0
    RefrigerationLoad            = 0.0d0
    TotalHotDefrostCondCredit    = 0.0d0
    FlowVolNeeded                = 0.0d0
    DeRate                       = .FALSE.

!SCE page 28 gives a delta T for pipe heat gains
!         (.25F each for supply and discharge) for use with mdot*cp.
!          However, another author shows this as a major diff between dx and secondary
!          So - allow the user to include this in his total load, even though he has to do
!          most of the calculations before the input (to get to SumUADistPiping)).
  DistPipeHeatGain = 0.0d0
  IF (Secondary(SecondaryNum)%SumUADistPiping > mysmallnumber) THEN
    ZoneNodeNum = Secondary(SecondaryNum)%DistPipeZoneNodeNum
    DiffTemp = Node(ZoneNodeNum)%Temp - TPipesReceiver
    DistPipeHeatGain = DiffTemp * Secondary(SecondaryNum)%SumUADistPiping
    DistPipeZoneNum = Secondary(SecondaryNum)%DistPipeZoneNum
    ! pipe heat load is a positive number (ie. heat absorbed by pipe, so needs to be subtracted
    !     from refrigcasecredit (- for cooling zone, + for heating zone)
    Secondary(SecondaryNum)%DistPipeZoneHeatGain = - DistPipeHeatGain
    RefrigCaseCredit(DistPipeZoneNum)%SenCaseCreditToZone = &
            RefrigCaseCredit(DistPipeZoneNum)%SenCaseCreditToZone - DistPipeHeatGain
  END IF !calc distribution piping heat gains

  ReceiverHeatGain = 0.0d0
  IF (Secondary(SecondaryNum)%SumUAReceiver > mysmallnumber) THEN
    ZoneNodeNum = Secondary(SecondaryNum)%ReceiverZoneNodeNum
    DiffTemp = Node(ZoneNodeNum)%Temp - TPipesReceiver
    ReceiverHeatGain = DiffTemp * Secondary(SecondaryNum)%SumUAReceiver
    ReceiverZoneNum = Secondary(SecondaryNum)%ReceiverZoneNum
    ! receiver heat load is a positive number (ie. heat absorbed by receiver, so needs to be subtracted
    !     from refrigcasecredit (- for cooling zone, + for heating zone)
    Secondary(SecondaryNum)%ReceiverZoneHeatGain = - ReceiverHeatGain
    RefrigCaseCredit(ReceiverZoneNum)%SenCaseCreditToZone = &
             RefrigCaseCredit(ReceiverZoneNum)%SenCaseCreditToZone - ReceiverHeatGain
  END IF !calc receiver heat gains


   !Sum up all the case and walk-in loads served by the secondary loop
    IF(Secondary(SecondaryNum)%NumCases > 0) THEN
    DO CaseNum = 1, Secondary(SecondaryNum)%NumCases
      CaseID = Secondary(SecondaryNum)%CaseNum(CaseNum)
      CALL CalculateCase(CaseID)
       ! increment TotalCoolingLoad Hot gas/brine defrost credits for each secondary loop
       RefrigerationLoad = RefrigerationLoad + RefrigCase(CaseID)%TotalCoolingLoad
       TotalHotDefrostCondCredit = TotalHotDefrostCondCredit + RefrigCase(CaseID)%HotDefrostCondCredit
    END DO  !CaseNum
    END IF !NumCases > 0
    IF(Secondary(SecondaryNum)%NumWalkIns > 0) THEN
      DO WalkInIndex=1,Secondary(SecondaryNum)%NumWalkIns
       WalkInID=Secondary(SecondaryNum)%WalkInNum(WalkInIndex)
       CALL CalculateWalkIn(WalkInID)
       ! increment TotalCoolingLoad for  each system
       RefrigerationLoad = RefrigerationLoad + WalkIn(WalkInID)%TotalCoolingLoad
       TotalHotDefrostCondCredit = TotalHotDefrostCondCredit + WalkIn(WalkInID)%HotDefrostCondCredit
      END DO !NumWalkIns systems
    END IF !Secondary(SecondaryNum)%NumWalkIns > 0

     IF(Secondary(SecondaryNum)%NumCoils > 0) THEN
      DO CoilIndex=1,Secondary(SecondaryNum)%NumCoils
       CoilID=Secondary(SecondaryNum)%CoilNum(CoilIndex)
       ! already CALL CalculateCoil(CoilID) for each coil, dispatched in coilset order for each zone
       ! increment TotalCoolingLoad for each system
       !  here will find out if secondary can serve total load, if not will derate coil outout/case credits
       RefrigerationLoad = RefrigerationLoad + WarehouseCoil(CoilID)%TotalCoolingLoad
       TotalHotDefrostCondCredit = TotalHotDefrostCondCredit + WarehouseCoil(CoilID)%HotDefrostCondCredit
      END DO !NumCoils on seocndary system
    END IF !Secondary(SecondaryNum)%NumCoils > 0

TotalLoad  = RefrigerationLoad + DistPipeHeatGain + ReceiverHeatGain
AtPartLoad = .TRUE.
!Check to see if load is already >+ maxload without pump heat
  IF(Secondary(SecondaryNum)%FluidType == SecFluidTypeAlwaysLiquid) THEN  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    FlowVolNeeded = TotalLoad/eta/(cpBrine*DensityBrine*(TBrineIn  - TEvap))
    ! For brine/glycol systems, find flow volume needed to meet load
    ! Per ashrae 2006, p4.1, eval mass flow rate to pump at brine return (to chiller) temp
    !   because pumps located in return piping
    IF(FlowVolNeeded >= MaxVolFlow) THEN
      !Don't need to iterate on pumps, just set to max.  Will have unmet load this time step (unless coils present)
      VolFlowRate    = MaxVolFlow
      TotalPumpPower = Secondary(SecondaryNum)%PumpTotRatedPower
      TotalLoad      = TotalLoad +  TotalPumpPower*Secondary(SecondaryNum)%PumpPowertoHeat
      AtPartLoad     = .FALSE.
      IF(Secondary(SecondaryNum)%NumCoils > 0) DeRate = .TRUE.
    END IF !flowvolneeded >= maxvolflow
  ELSE ! have SecFluidTypePhaseChange !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    IF(TotalLoad >= MaxLoad) THEN
      TotalPumpPower = Secondary(SecondaryNum)%PumpTotRatedPower
      TotalLoad      = TotalLoad +  TotalPumpPower*Secondary(SecondaryNum)%PumpPowertoHeat
      VolFlowRate    = MaxVolFlow
      AtPartLoad     = .FALSE.
      IF(Secondary(SecondaryNum)%NumCoils > 0)DeRate = .TRUE.
    END IF
  END IF !fluid type check for max load or max flow       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 ! If totalLoad < maxload, then need to calculate partial pump load
 ! Need an iterative soln for pump energy needed to meet total load
 !  because that total load has to include pump energy
 IF(AtPartLoad) THEN
   DO Iter=1,10
     IF(TotalLoad<=0.0d0) THEN
        ! Load on secondary loop is zero (or negative).
        ! Set volumetric flow rate and pump power to be zero.
        VolFlowRate=0.0d0
        TotalPumpPower=0.0d0
        EXIT
     END IF
     PrevTotalLoad = TotalLoad
     IF(Secondary(SecondaryNum)%FluidType == SecFluidTypeAlwaysLiquid) THEN
       FlowVolNeeded = TotalLoad/eta/(cpBrine*DensityBrine*(TBrineIn  - TEvap))
       PartLdFrac = FlowVolNeeded/MaxVolFlow
     ELSE
       PartLdFrac    = TotalLoad/MaxLoad
     END IF
     IF(Secondary(SecondaryNum)%PumpControlType == SecPumpControlConstant) THEN
       VolFlowRate    = 0.d0
       TotalPumpPower = 0.d0
       DO PumpID = 1, NumPumps  !dispatch pumps to meet needed flow rate
         IF(Secondary(SecondaryNum)%FluidType == SecFluidTypeAlwaysLiquid) THEN  !>>>>>>>>>>>>>>>>>>>>>
           VolFlowRate    = VolFlowRate    + Secondary(SecondaryNum)%PumpIncrementFlowVol
           TotalPumpPower = TotalPumpPower + Secondary(SecondaryNum)%PumpIncrementPower
           IF(VolFlowRate >= FlowVolNeeded)EXIT
         ELSE ! fluid type phase change >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
           VolFlowRate    = VolFlowRate    + Secondary(SecondaryNum)%PumpIncrementFlowVol
           TotalPumpPower = TotalPumpPower + Secondary(SecondaryNum)%PumpIncrementPower
           PartPumpFrac = TotalPumpPower/Secondary(SecondaryNum)%PumpTotRatedPower
           IF(PartPumpFrac >= PartLdFrac)EXIT
         END IF !fluid type              >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       END DO !Dispatching pumps until fluid flow need is met
     ELSE ! pump type variable
       VarFrac = MAX(0.1d0,CurveValue(Secondary(SecondaryNum)%VarSpeedCurvePtr,PartLdFrac))
       TotalPumpPower = Secondary(SecondaryNum)%PumpTotRatedPower * VarFrac
       VolFlowRate    = MaxVolFlow * PartLdFrac
     END IF ! pump type

     TotalLoad = RefrigerationLoad + DistPipeHeatGain  + ReceiverHeatGain &
               + TotalPumpPower*Secondary(SecondaryNum)%PumpPowertoHeat
     Error = DABS((TotalLoad-PrevTotalLoad)/PrevTotalLoad)
     IF (Error < Errortol) EXIT
   END DO  !end iteration on pump energy convergence

!   IF (Iter >=10 .AND. .NOT. WarmupFlag)THEN
!     If( .not. warmupflag) Then
!      Write(OutputFileDebug,707)Month, CurrentTime, Iter, TotalLoad, TotalPumpPower
!     End If
!707 format(' in iter loop at 707: ',1x,I2,1x,F5.2,1x,I5,7(F10.5,1x))
!    END IF  !didn't converge
 END IF  !(AtPartLoad)

   !If only loads are cases and walk-ins, that is, no air coils:
   !  Account for difference between load and capacity on secondary loop. Assume system able to provide
   !  rated capacity.  If it can't, that unmet energy will be stored and discharged at the system level.
   !  Meet current load to the extent possible.  If extra capacity available,
   !  apply it to previously unmet/stored loads.  If capacity less than current load,
   !  (e.g. as it may be following defrost cycles on cases or walk-ins served by secondary loop)
   !  save the unmet/stored load to be met in succeeding time steps.
 IF(Secondary(SecondaryNum)%NumCoils == 0) THEN
   StoredEnergyRate  =  MAX(0.d0,(UnMetEnergy/TimeStepZone/SecInHour))
   LoadRequested = TotalLoad + StoredEnergyRate
   IF(MaxLoad > LoadRequested) THEN
     ! Have at least as much capacity avail as needed, even counting stored energy
     TotalCoolingLoad = LoadRequested
     RefrigerationLoad = RefrigerationLoad + StoredEnergyRate
     UnMetEnergy = 0.0d0
   ELSE
     !Don't have as much capacity as needed (likely following defrost periods)
     TotalCoolingLoad = MaxLoad
     RefrigerationLoad = RefrigerationLoad - (TotalLoad - Maxload)
     IF(.NOT. WarmUpFlag) UnMetEnergy=UnMetEnergy + ((TotalLoad - Maxload)* TimeStepZone * SecInHour)
   END IF ! load requested greater than MaxLoad
   IF(Secondary(SecondaryNum)%UnMetEnergy > MyLargeNumber) THEN
     Secondary(SecondaryNum)%UnMetEnergy=MyLargeNumber
     IF(ShowUnmetSecondEnergyWarning(SecondaryNum)) THEN
       CALL ShowWarningError('Secondary Refrigeration Loop: '//TRIM(Secondary(SecondaryNum)%Name))
       CALL ShowContinueError(' This secondary system has insufficient capacity to meet the refrigeration loads.')
       ShowUnmetSecondEnergyWarning(SecondaryNum) = .FALSE.
     END IF
   END IF !>my large number

  Secondary(SecondaryNum)%UnMetEnergy          = UnMetEnergy
ELSE ! air coils on secondary loop, no "unmet" energy accounting, just reduce amount of cooling provided to zone by coils
  DeRate = .FALSE.
  IF(TotalLoad > MaxLoad)DeRate = .TRUE.
   !  TotalLoad = RefrigerationLoad + DistPipeHeatGain  + ReceiverHeatGain &
    !           + TotalPumpPower*Secondary(SecondaryNum)%PumpPowertoHeat
  CALL FinalRateCoils(DeRate,SecondarySystem,SecondaryNum,TotalLoad,MaxLoad) !assign case credits for coils on this loop
END IF ! no air coils on secondary loop
  Secondary(SecondaryNum)%PumpPowerTotal       = TotalPumpPower
  Secondary(SecondaryNum)%PumpElecEnergyTotal  = TotalPumpPower * LocalTimeStep * SecInHour
  Secondary(SecondaryNum)%TotalRefrigLoad      = RefrigerationLoad
  Secondary(SecondaryNum)%TotalRefrigEnergy    = RefrigerationLoad * LocalTimeStep * SecInHour
  Secondary(SecondaryNum)%TotalCoolingLoad     = TotalCoolingLoad
  Secondary(SecondaryNum)%TotalCoolingEnergy   = TotalCoolingLoad * LocalTimeStep * SecInHour
  Secondary(SecondaryNum)%FlowVolActual        = VolFlowRate
  Secondary(SecondaryNum)%HotDefrostCondCredit = TotalHotDefrostCondCredit
  Secondary(SecondaryNum)%DistPipeHeatGain     = DistPipeHeatGain
  Secondary(SecondaryNum)%DistPipeHeatGainEnergy = DistPipeHeatGain * LocalTimeStep * SecInHour
  Secondary(SecondaryNum)%ReceiverHeatGain     = ReceiverHeatGain
  Secondary(SecondaryNum)%ReceiverHeatGainEnergy = ReceiverHeatGain * LocalTimeStep * SecInHour



RETURN

END SUBROUTINE CalculateSecondary

SUBROUTINE SumZoneImpacts

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL
          !       DATE WRITTEN   Spring 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the total impact of all refrigeration systems on each zone.

          ! METHODOLOGY EMPLOYED:
          ! Calculate the energy from refrigerated case credits arising from interaction between the zone and:
          !   refrigerated cases and walk-ins
          !   heat rejection from zone-located compressor-racks and zone-located air-cooled condensers
          !   heat absorbed by suction piping, secondary loop distribution piping, and
          !   secondary receiver shells

          ! REFERENCES:

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: ZoneNum = 0         ! used calculating total refrigeration interactions for zone


  IF(UseSysTimeStep) THEN  ! air chillers
    DO ZoneNum = 1,NumOfZones
        CoilSysCredit(ZoneNum)%ReportH20RemovedKgPerS_FromZoneRate = &
               - CoilSysCredit(ZoneNum)%LatKgPerS_ToZoneRate
        CoilSysCredit(ZoneNum)%ReportLatCreditToZoneRate = &
              - CoilSysCredit(ZoneNum)%LatCreditToZoneRate
        CoilSysCredit(ZoneNum)%ReportLatCreditToZoneEnergy = &
             - CoilSysCredit(ZoneNum)%LatCreditToZoneEnergy
       ! Sensible rate can be positive or negative, split into separate output variables and
       !   always report positive value
       IF(CoilSysCredit(ZoneNum)%SenCreditToZoneRate <= 0.0d0) THEN
         CoilSysCredit(ZoneNum)%ReportSenCoolingToZoneRate = &
              - CoilSysCredit(ZoneNum)%SenCreditToZoneRate
         CoilSysCredit(ZoneNum)%ReportSenCoolingToZoneEnergy = &
               - CoilSysCredit(ZoneNum)%SenCreditToZoneEnergy
         CoilSysCredit(ZoneNum)%ReportHeatingToZoneRate = 0.0d0
         CoilSysCredit(ZoneNum)%ReportHeatingToZoneEnergy = 0.0d0
       ELSE
         CoilSysCredit(ZoneNum)%ReportSenCoolingToZoneRate = 0.0d0
         CoilSysCredit(ZoneNum)%ReportSenCoolingToZoneEnergy =0.0d0
         CoilSysCredit(ZoneNum)%ReportHeatingToZoneRate = &
               CoilSysCredit(ZoneNum)%SenCreditToZoneRate
         CoilSysCredit(ZoneNum)%ReportHeatingToZoneEnergy = &
               - CoilSysCredit(ZoneNum)%SenCreditToZoneEnergy
       END IF
        CoilSysCredit(ZoneNum)%ReportTotCoolingToZoneRate = &
              CoilSysCredit(ZoneNum)%ReportLatCreditToZoneRate + &
              CoilSysCredit(ZoneNum)%ReportSenCoolingToZoneRate
        CoilSysCredit(ZoneNum)%ReportTotCoolingToZoneEnergy = &
              CoilSysCredit(ZoneNum)%ReportLatCreditToZoneEnergy + &
              CoilSysCredit(ZoneNum)%ReportSenCoolingToZoneEnergy
    END DO
  END IF !UseSysTimeStep signals run for air chillers

  !Can arrive here when load call to refrigeration looks for cases/walkin systems and usetimestep is .false.
  IF((.NOT. UseSysTimeStep).AND.((NumSimulationCases > 0).OR.( NumSimulationWalkIns > 0)))THEN
    DO ZoneNum = 1,NumOfZones
       CaseWIZoneReport(ZoneNum)%SenCaseCreditToZoneEnergy = &
                  RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone * &
                  TimeStepZone * SecInHour
       ! Latent always negative
       CaseWIZoneReport(ZoneNum)%LatCoolingToZoneRate   = &
                  - RefrigCaseCredit(ZoneNum)%LatCaseCreditToZone
       CaseWIZoneReport(ZoneNum)%LatCoolingToZoneEnergy = &
                  CaseWIZoneReport(ZoneNum)%LatCoolingToZoneRate * &
                  TimeStepZone * SecInHour
       ! Sensible rate can be positive or negative, split into separate output variables and
       !   always report positive value
       IF(RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone <= 0.0d0) THEN
         CaseWIZoneReport(ZoneNum)%SenCoolingToZoneRate = &
                  - RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone
         CaseWIZoneReport(ZoneNum)%SenCoolingToZoneEnergy  = &
                  - RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone * &
                  TimeStepZone * SecInHour
         CaseWIZoneReport(ZoneNum)%HeatingToZoneRate   = 0.0d0
         CaseWIZoneReport(ZoneNum)%HeatingToZoneEnergy = 0.0d0
       ELSE
         CaseWIZoneReport(ZoneNum)%SenCoolingToZoneRate =  0.0d0
         CaseWIZoneReport(ZoneNum)%SenCoolingToZoneEnergy  =  0.0d0
         CaseWIZoneReport(ZoneNum)%HeatingToZoneRate   = &
                  RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone
         CaseWIZoneReport(ZoneNum)%HeatingToZoneEnergy = &
                  RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone * &
                  TimeStepZone * SecInHour
       END IF
       CaseWIZoneReport(ZoneNum)%TotCoolingToZoneRate   = &
                CaseWIZoneReport(ZoneNum)%SenCoolingToZoneRate + &
                CaseWIZoneReport(ZoneNum)%LatCoolingToZoneRate
       CaseWIZoneReport(ZoneNum)%TotCoolingToZoneEnergy = &
                CaseWIZoneReport(ZoneNum)%SenCoolingToZoneEnergy + &
                CaseWIZoneReport(ZoneNum)%LatCoolingToZoneEnergy
       CaseWIZoneReport(ZoneNum)%TotHtXferToZoneRate = &
                 RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone + &
                 RefrigCaseCredit(ZoneNum)%LatCaseCreditToZone
       CaseWIZoneReport(ZoneNum)%TotHtXferToZoneEnergy = &
                 CaseWIZoneReport(ZoneNum)%TotHtXferToZoneRate* &
                 TimeStepZone * SecInHour
    END DO ! over zones for cases and walkins
  END IF

RETURN

END SUBROUTINE SumZoneImpacts

SUBROUTINE CheckRefrigerationInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Sep 2010 - mining function
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provides the structure to get Refrigeration input so that
          ! it can be called from internally or outside the module.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetRefrigerationInputFlag) THEN

    CALL GetRefrigerationInput()
    CALL SetupReportInput()
    GetRefrigerationInputFlag = .FALSE.

    IF((.NOT. HaveCasesOrWalkins) .AND. (.NOT. HaveChillers)) THEN
      ManageRefrigeration = .FALSE.
      RETURN
    END IF
    IF((.NOT. HaveDetailedRefrig) .AND. (.NOT. HaveRefrigRacks) .AND. &
      (.NOT. HaveDetailedTransRefrig)) THEN
      ManageRefrigeration = .FALSE.
      RETURN
    END IF
  END IF !GetRefrigerationInputFlag

  RETURN

END SUBROUTINE CheckRefrigerationInput

!***************************************************************************************************
!***************************************************************************************************
SUBROUTINE SimAirChillerSet(AirChillerSetName, ZoneNum,  FirstHVACIteration,  &
                                  SysOutputProvided,LatOutputProvided,AirChillerSetPtr)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL
          !       DATE WRITTEN   January 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Transfers the load requested from the zone to the refrigeration module.
          ! The load is met, partially met, or not met in the call to the detailed system solution
          !
          ! METHODOLOGY EMPLOYED:
          ! Called from Zone Equipment Manager.
          !

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
  USE InputProcessor,    ONLY: FindItemInList
  USE DataHeatBalFanSys, ONLY: TempControlType
  USE DataHVACGlobals,   ONLY: SingleHeatingSetPoint
  USE General,           ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN) :: AirChillerSetName
  LOGICAL, INTENT(IN)     :: FirstHVACIteration
  INTEGER, INTENT(IN)     :: ZoneNum
  INTEGER, INTENT(INOUT)  :: AirChillerSetPtr  !from ZoneEquipList(CurZoneEqNum)%EquipIndex(EquipPtr)
  REAL(r64), INTENT(OUT)  :: LatOutputProvided
  REAL(r64), INTENT(OUT)  :: SysOutputProvided

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: ChillerSetID
  REAL(r64)    :: RemainingOutputToCoolingSP       ! Remaining requested load in zone

  CALL CheckRefrigerationInput

  ! Find the correct Chiller set
  IF (AirChillerSetPtr == 0) THEN
    ChillerSetID = FindItemInList(AirChillerSetName, AirChillerSet%Name, NumRefrigChillerSets)
    IF (ChillerSetID == 0) THEN
      CALL ShowFatalError('SimAirChillerSet: Unit not found='//TRIM(AirChillerSetName))
    ENDIF ! chillersetid ==0 because not in list
    AirChillerSetPtr=ChillerSetID
  ELSE  !airchllersetpointer passed in call to subroutine not ==0
      ChillerSetID=AirChillerSetPtr
      IF (ChillerSetID > NumRefrigChillerSets .or. ChillerSetID < 1) THEN
        CALL ShowFatalError('SimAirChillerSet:  Invalid AirChillerSetPtr passed='//  &
                            TRIM(TrimSigDigits(ChillerSetID,0))// &
                            ', Number of Units='//TRIM(TrimSigDigits(NumRefrigChillerSets))//  &
                            ', Entered Unit name='//TRIM(AirChillerSetName))
      ENDIF !ChillerSetID makes no sense
      IF (CheckChillerSetName(ChillerSetID)) THEN
        IF (AirChillerSetName /= AirChillerSet(ChillerSetID)%Name) THEN
          CALL ShowFatalError('SimAirChillerSet:  Invalid AirChillerSetPtr passed='//  &
                              TRIM(TrimSigDigits(ChillerSetID))// &
                              ', Unit name='//TRIM(AirChillerSetName)//', stored Unit Name for that index='//  &
                              TRIM(AirChillerSet(ChillerSetID)%Name))
        ENDIF !name not equal correct name
      CheckChillerSetName(ChillerSetID)=.false.
      ENDIF !CheckChillerSetName logical test
    ENDIF !(AirChillerSetPtr == 0 or else not == 0

  IF(FirstHVACIteration) THEN
    DO ChillerSetID = 1,NumRefrigChillerSets   !bbb what point of do loop, only set one (airchillersetptr) to zero
      AirChillerSet(AirChillerSetPtr)%QZnReqSens = 0.0D0
    END DO
  END IF !FirstHVACIteration

  RemainingOutputToCoolingSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
  !RemainingOutputToCoolingSP in Watts, < 0 for cooling demand

  IF(RemainingOutputToCoolingSP .LT. 0.0D0 .and. TempControlType(ZoneNum) .NE. SingleHeatingSetPoint)THEN
    AirChillerSet(AirChillerSetPtr)%QZnReqSens = RemainingOutputToCoolingSP
  ELSE
    AirChillerSet(AirChillerSetPtr)%QZnReqSens = 0.0D0
  END IF

  UseSysTimeStep = .TRUE.

  CALL ManageRefrigeratedCaseRacks

  UseSysTimeStep = .FALSE.

  ! Return values to Zone Equipment Manager.
  LatOutputProvided = CoilSysCredit(ZoneNum)%LatKgPerS_ToZoneRate
  SysOutputProvided = CoilSysCredit(ZoneNum)%SenCreditToZoneRate

RETURN
END SUBROUTINE SimAirChillerSet

!***************************************************************************************************

SUBROUTINE CalculateAirChillerSets(AirChillerSetID)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL
          !       DATE WRITTEN   January 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Transfers the load requested from the zone to the refrigeration module.
          ! The load is met, partially met, or not met in the next time step when the refrigerated case
          ! module is called via case credits. Therefore, by definition, the sensible and latent
          ! output provided are zero.
          !
          ! METHODOLOGY EMPLOYED:
          ! Called from Zone Equipment Manager.
          !
          !       have however done the variable definitions for in and out.

          ! USE STATEMENTS:
!unused  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
!unused  USE InputProcessor,    ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: AirChillerSetID

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER      :: CoilID              =0   ! Index to coil
INTEGER      :: CoilIndex           =0   ! rank of coils within system
REAL(r64)    :: AirChillerSetSchedule     =0.0d0   ! Schedule value for air chiller SET
REAL(r64)    :: QZNReqSens          =0.0d0   ! Amount of sensible heat needed by the zone, NEGATIVE when cooling needed [W]
REAL(r64)    :: RemainQZNReqSens    =0.0d0   ! Remaiing amount of sensible heat needed by the zone [W]


  !Note, all coils in a coil set are in the same zone
  ! the coils may be served by different detailed systems
  ! The coils are dispatched to meet the load specified in the previous time step in order listed in coilset object
  AirChillerSetSchedule = GetCurrentScheduleValue(AirChillerSet( AirChillerSetID)%SchedPtr)

  IF ( AirChillerSetSchedule <= 0.d0) RETURN
  QZNReqSens = AirChillerSet( AirChillerSetID)%QZnReqSens
  RemainQZNReqSens = QZNReqSens


  DO CoilIndex = 1,AirChillerSet(AirChillerSetID)%NumCoils
    CoilID = AirChillerSet(AirChillerSetID)%CoilNum(CoilIndex)

    CALL CalculateCoil(CoilID,RemainQZNReqSens)
    RemainQZNReqSens = RemainQZNReqSens + WarehouseCoil(CoilID)%SensCreditRate
    !should be a negative minus a negative, so a smaller negative, that is, going toward zero, but senscoolingenergyrate expressed as positive
    !Need to go over all the coils so that the defrosts occur on schedule, even when the chiller isn't called for at that particular time step
    !IF(RemainQZNReqSens >=0.d0)EXIT  !shouldn't be > 0 because limited by request in calculatecoil
    IF(RemainQZNReqSens > 0.d0)RemainQZNReqSens = 0.d0
  END DO ! CoilIndex

  RETURN

END SUBROUTINE CalculateAirChillerSets

!***************************************************************************************************
SUBROUTINE FinalRateCoils(Derate,SystemSourceType,SystemID,InitialTotalLoad,AvailableTotalLoad)!or unmet load and available load?

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL
          !       DATE WRITTEN   January 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! When compressor system, or secondary loop capacity is insufficient to meet coil loads
          !   Come back here and derate the coil case credits to show unmet load impact
          !   Note that the coil fan, heater, and defrost would be unaffected because they
          !   would still be running at level calculated previously

          ! METHODOLOGY EMPLOYED:

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataLoopNode
  USE Psychrometrics,    ONLY: PsyRhoAirFnPbTdbW,RhoH2O,PsyWFnTdbTwbPb,PsyTwbFnTdbWPb,CPHW,&
                               PsyHFnTdbW,PsyTsatFnHPb, PsyWFnTdpPb,PsyHFnTdbRhPb, PsyRhFnTdbWPb, &
                               PsyTdpFnWPb, PsyWFnTdbH
  USE General,         ONLY:   CreateSysTimeIntervalString


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  LOGICAL, INTENT(IN)     :: Derate              !True if compressor rack or secondary ht exchanger unable to provide capacity
  INTEGER, INTENT(IN)     :: SystemSourceType    !Secondarysystem or DetailedSystem
  INTEGER, INTENT(IN)     :: SystemID            !ID for Secondary loop or detailed system calling for derate
  REAL(r64), INTENT(IN)   :: InitialTotalLoad    !Load on system or secondary loop as initially calculated [W]
  REAL(r64), INTENT(IN)   :: AvailableTotalLoad  !Load that system or secondary loop is able to serve [W]
          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER      :: NumCoils             =0   ! Number of coils on this system or secondary loop
INTEGER      :: CoilID               =0   ! Index to coil
INTEGER      :: CoilIndex            =0   ! rank of coils within system
REAL(r64)    :: DeRateFactor         =0.0d0 ! Ratio of energy available from system or secondary loop
REAL(r64)    :: InitLatCreditEnergy  =0.0d0 ! Latent credit energy before derate [W]
REAL(r64)    :: InitKgFrost          =0.0d0 ! Initial amount of frost on coils based on latent load before derate [kg]
REAL(r64)    :: FrostReduction       =0.0d0 ! Change in frost on coils based on derated latent load [kg]

SELECT CASE (SystemSourceType)
CASE (DetailedSystem)
    NumCoils = System(SystemID)%NumCoils
CASE (SecondarySystem)
    NumCoils = Secondary(SystemID)%NumCoils
END SELECT !DeRateCoils

IF(DeRate) THEN
  CALL ShowRecurringWarningErrorAtEnd('Refrigeration:System chilling WarehouseCoils '// &
        TRIM(System(SystemID)%Name) // &
        ' - Refrigeration system unable to meet load of warehouse coils chilled by system ... continues by derating coil load',&
        System(SystemID)%InsuffCapWarn)

  DeRateFactor = AvailableTotalLoad/InitialTotalLoad
  DO CoilIndex = 1,NumCoils
    CoilID = System(SystemID)%CoilNum(CoilIndex)

    !need to adjust ice on coil due to reduction in latent load met by coil
    InitLatCreditEnergy = WarehouseCoil(CoilID)%LatCreditEnergy
    InitKgFrost         = WarehouseCoil(CoilID)%KgFrost

    WarehouseCoil(CoilID)%TotalCoolingLoad         = DeRateFactor * WarehouseCoil(CoilID)%TotalCoolingLoad
    WarehouseCoil(CoilID)%TotalCoolingEnergy       = DeRateFactor * WarehouseCoil(CoilID)%TotalCoolingEnergy
    WarehouseCoil(CoilID)%SensCoolingEnergyRate    = DeRateFactor * WarehouseCoil(CoilID)%SensCoolingEnergyRate
    WarehouseCoil(CoilID)%SensCoolingEnergy        = DeRateFactor * WarehouseCoil(CoilID)%SensCoolingEnergy
    WarehouseCoil(CoilID)%LatCreditRate            = DeRateFactor * WarehouseCoil(CoilID)%LatCreditRate
    WarehouseCoil(CoilID)%LatCreditEnergy          = DeRateFactor * WarehouseCoil(CoilID)%LatCreditEnergy
    WarehouseCoil(CoilID)%LatKgPerS_ToZone         = DeRateFactor * WarehouseCoil(CoilID)%LatKgPerS_ToZone
    WarehouseCoil(CoilID)%SensCreditRate           = WarehouseCoil(CoilID)%SensCoolingEnergyRate -   &
                             WarehouseCoil(CoilID)%ElecFanPower - WarehouseCoil(CoilID)%ElecHeaterPower -   &
                             WarehouseCoil(CoilID)%ThermalDefrostPower
    WarehouseCoil(CoilID)%SensCreditEnergy         = WarehouseCoil(CoilID)%SensCreditRate * TimeStepSys * SecInHour

    FrostReduction = (InitLatCreditEnergy - WarehouseCoil(CoilID)%LatCreditEnergy)/IcetoVaporEnthalpy
    WarehouseCoil(CoilID)%KgFrost = Max(0.d0,(WarehouseCoil(CoilID)%KgFrost - FrostReduction))

    IF(WarehouseCoil(CoilID)%SensCreditRate >= 0.d0) THEN
      WarehouseCoil(CoilID)%ReportSensCoolCreditRate = WarehouseCoil(CoilID)%SensCreditRate
      WarehouseCoil(CoilID)%ReportHeatingCreditRate = 0.d0
    ELSE
      WarehouseCoil(CoilID)%ReportSensCoolCreditRate = 0.d0
      WarehouseCoil(CoilID)%ReportHeatingCreditRate = - WarehouseCoil(CoilID)%SensCreditRate
    END IF
    WarehouseCoil(CoilID)%ReportSensCoolCreditEnergy = WarehouseCoil(CoilID)%ReportSensCoolCreditRate * &
                                                      TimeStepSys * SecInHour
    WarehouseCoil(CoilID)%ReportHeatingCreditEnergy = WarehouseCoil(CoilID)%ReportHeatingCreditRate * &
                                                      TimeStepSys * SecInHour
    WarehouseCoil(CoilID)%ReportTotalCoolCreditRate = WarehouseCoil(CoilID)%ReportSensCoolCreditRate + &
                                                      WarehouseCoil(CoilID)%LatCreditRate
    WarehouseCoil(CoilID)%ReportTotalCoolCreditEnergy = WarehouseCoil(CoilID)%ReportSensCoolCreditEnergy + &
                                                      WarehouseCoil(CoilID)%LatCreditEnergy
  END DO
END IF !Derate logical true





RETURN
END SUBROUTINE FinalRateCoils
!***************************************************************************************************




!***************************************************************************************************
SUBROUTINE CalculateCoil(CoilID,QZnReq)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Therese Stovall, ORNL
          !       DATE WRITTEN   January 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates the refrigerated warehouse coil object.
          ! Note QZnReq < 0 corresponds to cooling needed

          ! METHODOLOGY EMPLOYED:
          ! Called from Calculate Air Chiller Set.
          ! Air chillers are used to model the type of equipment typically used in
          ! refrigerated warehouses. For that reason, there is a major difference
          ! between the air chiller model and those for refrigerated cases or walk-ins.
          ! For cases and walk-ins, a portion of the model is directed toward
          ! calculating the amount of refrigeration needed to maintain the refrigerated
          ! volume at the desired temperature due to heat exchange with the surrounding
          ! zone, and that zone is conditioned to a nearly constant temperature.
          ! In a refrigerated warehouse, the refrigeration load is caused by heat exchange
          ! with a variable external environment.  For that reason, the loads for these
          ! zones are calculated by the usual EnergyPlus zone heat balance.
          ! The amount of refrigeration needed to maintain the specified temperature
          ! setpoints is then passed to the air chiller model, in a similar fashion
          ! to the load passed to a window air conditioner model. The air chillers
          ! are therefore solved using the system time step, not the zone time step
          ! used for cases and walk-ins.
          !
          ! The air chiller performance is based on three types of manufacturers ratings,
          ! Unit Load Factor, Total Capacity Map, or a set of European standards.
          ! Correction factors for material and refrigerant are applied to all of these ratings.

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataLoopNode
  USE Psychrometrics,    ONLY: PsyRhoAirFnPbTdbW,RhoH2O,PsyWFnTdbTwbPb,PsyTwbFnTdbWPb,CPHW,&
                               PsyHFnTdbW,PsyTsatFnHPb, PsyWFnTdpPb,PsyHFnTdbRhPb, PsyRhFnTdbWPb, &
                               PsyTdpFnWPb, PsyWFnTdbH, PsyCpAirFnWTdb
  USE General,         ONLY:   CreateSysTimeIntervalString


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: CoilID  !
  REAL(r64), INTENT(IN)     :: QZnReq              ! sensible load required

          ! SUBROUTINE PARAMETER DEFINITIONS:
!unused  REAL(r64), PARAMETER ::ErrorTol       = 0.001d0 !Iterative solution tolerance
  CHARACTER(len=MaxNameLength),Parameter   :: TrackMessage = 'from RefrigeratedCase:CalculateCoil'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!INTEGER      :: Iter                    =0   ! counter for loop to solve for total coil capacity as a function of inlet air conditions
INTEGER      :: FanSpeedControlType     =0   ! from input
INTEGER      :: ShrCorrectionCurvePtr   =0   ! Points to curve entered by user to specify total/sensible capacity as a function of SHR
INTEGER      :: SHRCorrectionType       =0   ! SHR60, QUADRATICSHR, EUROPEAN, or TABULARRH_DT1_TRoom
INTEGER      :: ZoneNodeNum             =0   ! Zone node number
REAL(r64)    :: AirVolRatio             =0.0d0 ! used when operating at part load
REAL(r64)    :: AirVolumeFlowMax        =0.0d0 ! Coil air flow limited by drip down schedule (m3/s)
REAL(r64)    :: AirVolumeFlowRated      =0.0d0 ! Coil rated air flow (m3/s)
REAL(r64)    :: AvailDefrostEnergy      =0.0d0 ! available to melt ice with temp term control (J)
REAL(r64)    :: CapFac                  =0.0d0 ! used to reduce fan power when don't need full coil capacity
REAL(r64)    :: CoilCapTotal            =0.0d0 ! Sensible plus latent load (W)
REAL(r64)    :: CoilCapTotEstimate      =0.0d0 ! Part of loop to solve for total coil capacity as a function of inlet air conditions (W)
REAL(r64)    :: CoilInletCp             =0.0d0 ! Coil air inlet specific heat (J/kg-deltaC)
REAL(r64)    :: CoilInletDensity        =0.0d0 ! Coil air inlet density (kg/m3)
REAL(r64)    :: CoilInletDryAirCp       =0.0d0 ! Dry air specific heat at coil inlet temperature (J/kg-C)
REAL(r64)    :: CoilInletDryAirDensity  =0.0d0 ! Dry Air density at coil inlet temperature (kg/m3)
REAL(r64)    :: CoilInletHumRatio       =0.0d0 ! Coil air inlet humidity ratio (kg water/kg air)
REAL(r64)    :: CoilInletTemp           =0.0d0 ! Inlet temperature of air to coil, not mixed zone temperature unless "middle" location selected (C)
REAL(r64)    :: CoilInletEnthalpy       =0.0d0 ! Coil inlet air enthalpy (J/kg)
REAL(r64)    :: CoilInletRHFrac         =0.0d0 ! Coil inlet air relative humidity expressed as a fraction (0 to 1)
REAL(r64)    :: CoilSchedule            =0.0d0 ! Current value of Coil operating (availability) schedule
REAL(r64)    :: CoolingLoadNet          =0.0d0 ! Cooling capacity of the coil minus fan, heater, and defrost loads (W)
REAL(r64)    :: DefrostCap              =0.0d0 ! Design defrost capacity of Coil (W)
REAL(r64)    :: DefrostEnergy           =0.0d0 ! (J)
REAL(r64)    :: DefEnergyFraction       =0.0d0 ! dimensionless
REAL(r64)    :: DefrostLoad             =0.0d0 ! Part of the defrost that is a heat load on the zone (W)
REAL(r64)    :: DefrostSchedule         =0.0d0 ! Coil defrost schedule, between 0 and 1
REAL(r64)    :: DefrostDripDownSchedule =0.0d0 ! Coil drip-down schedule (allows coil to drain after defrost)
REAL(r64)    :: DefrostEnergyNeeded     =0.0d0 ! Energy needed to melt all ice, used with temperature termination (J)
REAL(r64)    :: DefrostRateNeeded       =0.0d0 ! Defrost load that actually goes to melting ice (W)
REAL(r64)    :: DryAirMassFlowMax       =0.0d0 ! Rated volume flow rate times dry air density adjusted for schedules (kg/s)
REAL(r64)    :: DryAirMassFlowRated     =0.0d0 ! Rated volume flow rate times dry air density
!REAL(r64)    :: Error                   =0.0d0 ! Used in iterative solution for sensible heat ratio
REAL(r64)    :: ExitHumRatio            =0.0d0 ! kg water/kg air
REAL(r64)    :: ExitTemperature         =0.0d0 ! Air temperature leaving the coil (C)
REAL(r64)    :: ExitTemperatureEstimate =0.0d0 ! Estimated Air temperature leaving the coil (C)
REAL(r64)    :: ExitEnthalpy            =0.0d0 ! Air enthalpy leaving the coil (J/kg)
REAL(r64)    :: ExitEnthalpyEstimate    =0.0d0 ! Estimated Air enthalpy leaving the coil (J/kg)
REAL(r64)    :: FanMinAirFlowRatio      =0.0d0 ! From input
REAL(r64)    :: FanPowerActual          =0.0d0 ! (W)
REAL(r64)    :: FanPowerRated           =0.0d0 ! (W)
REAL(r64)    :: FanPowerMax             =0.0d0 ! Total fan energy rate, limited by dripdown period (W)
REAL(r64)    :: FanPowerRatio           =0.0d0 ! Used for variable speed fans, dimensionless
REAL(r64)    :: FrostChangekg           =0.0d0 ! Amount of frost added or melted  (kg)
REAL(r64)    :: HeaterSchedule          =0.0d0 ! zero to one
REAL(r64)    :: HeaterLoad              =0.0d0 ! Total heater (except defrost) energy rate (W)
REAL(r64)    :: IceSensHeatNeeded       =0.0d0 ! Energy to raise frost temperature to 0C, used w/ temp termination (J)
REAL(r64)    :: LatLoadServed           =0.0d0 ! Energy rate used to remove water from zone air (W)
REAL(r64)    :: MaxTemperatureDif       =0.0d0 ! Used to limit capacity during initial pulldown (deltaC)
REAL(r64)    :: SensibleCapacityMax     =0.0d0 ! Sensible capacity adjusted for any time in dripdown state (W)
!REAL(r64)    :: SensibleLoad            =0.0d0 ! Sensible load provided by coil (W)
REAL(r64)    :: SensLoadRequested       =0.0d0 ! Sensible load requested by zone balance (W)
REAL(r64)    :: SensLoadFromZone        =0.0d0 ! Net sensible load removed from zone after accounting for heaters, fans, defrost [W]
REAL(r64)    :: SensLoadRequestedGross  =0.0d0 ! Gross sensible load removed by coil
REAL(r64)    :: SensLoadGross           =0.0d0 ! Sensible load met by coil (W)
REAL(r64)    :: SHR                     =0.0d0 ! Sensible heat ratio, sensible load/total load
REAL(r64)    :: SHRCorrection           =0.0d0 ! Actual total/sensible load, NOT = Inverse SHR (unless coil efficiency = 1.0),
                                             ! but function of SHR, which is why iteration needed
REAL(r64)    :: ShrCorrection60         =0.0d0 ! Total capacity as a fraction of sensible capacity at a SHR of 0.6, entered by user
REAL(r64)    :: Slope                   =0.0d0 ! Part of linear SHR60 correction factor, dimensionless
REAL(r64)    :: StartIceTemp            =0.0d0 ! Frost temperature at start of time step [C]
REAL(r64)    :: StartFrostkg            =0.0d0 ! frost load at start of time step (kg of ice)
REAL(r64)    :: TemperatureDif          =0.0d0 ! difference between inlet air and evaporating temperature (deltaC)
REAL(r64)    :: TEvap                   =0.0d0 ! Evaporating temperature in the coil (C)
REAL(r64)    :: UnitLoadFactorSens      =0.0d0 ! Rated capacity divided by rated DT1 (T air in - Tevap) (W/delta C)
REAL(r64)    :: WaterRemovRate          =0.0d0 ! Walk in cooler removes water at this rate in this zone (kg/s)
REAL(r64)    :: Yint                    =0.0d0 !Part of linear SHR60 correction factor, dimensionless
REAL(r64)    :: ZoneDryAirDensity       =0.0d0 ! Dry air density at mixed zone conditions
REAL(r64)    :: ZoneMixedAirCp          =0.0d0 ! J/kg-deltaC
REAL(r64)    :: ZoneMixedAirDensity     =0.0d0 ! kg/m3
REAL(r64)    :: ZoneMixedAirDrybulb     =0.0d0 ! (C)
REAL(r64)    :: ZoneMixedAirRHfrac      =0.0d0 ! relative humidity of mixed air in the zone expressed as a fraction from 0 to 1
REAL(r64)    :: ZoneMixedAirEnthalpy    =0.0d0 ! J/kg
REAL(r64)    :: ZoneMixedAirHumRatio    =0.0d0 ! kg water/kg air in the zone mixed air

  ! GET SCHEDULES
  CoilSchedule            = GetCurrentScheduleValue(WarehouseCoil(CoilID)%SchedPtr)

  IF ( CoilSchedule <= 0.d0) RETURN

  DefrostSchedule         = GetCurrentScheduleValue(WarehouseCoil(CoilID)%DefrostSchedPtr)
  DefrostDripDownSchedule = GetCurrentScheduleValue(WarehouseCoil(CoilID)%DefrostDripDownSchedPtr)
  !next statement In case user doesn't understand concept of drip down schedule
  DefrostDripDownSchedule = MAX(DefrostDripDownSchedule,DefrostSchedule)
  !next value optional, so set to default before checking for schedule
  HeaterSchedule          = 1.0d0
  IF (WarehouseCoil(CoilID)%HeaterSchedPtr > 0)  HeaterSchedule  = &
                            GetCurrentScheduleValue(WarehouseCoil(CoilID)%HeaterSchedPtr)

   AirVolRatio           = 0.d0
   AirVolumeFlowMax      = 0.d0
   CapFac                = 0.d0
   CoilCapTotal          = 0.d0
   CoilCapTotEstimate    = 0.d0
   CoolingLoadNet        = 0.d0
   DefrostLoad           = 0.d0
   DryAirMassFlowMax     = 0.d0
   ExitEnthalpyEstimate  = 0.d0
   ExitEnthalpy          = 0.d0
   ExitTemperature       = 0.d0
   ExitHumRatio          = 0.d0
   FanPowerActual        = 0.d0
   HeaterLoad            = 0.d0
   LatLoadServed         = 0.d0
   FanPowerRatio         = 0.d0
   FrostChangekg         = 0.d0
   SensLoadFromZone      = 0.d0
   SensLoadGross         = 0.d0
   SensibleCapacityMax   = 0.d0
   SHR                   = 0.d0
   WaterRemovRate        = 0.d0

!Set local subroutine variables for convenience
ZoneNodeNum           = WarehouseCoil(CoilID)%ZoneNodeNum
AirVolumeFlowRated    = WarehouseCoil(CoilID)%RatedAirVolumeFlow
FanPowerRated         = WarehouseCoil(CoilID)%RatedFanPower
HeaterLoad            = WarehouseCoil(CoilID)%HeaterPower * HeaterSchedule
UnitLoadFactorSens    = WarehouseCoil(CoilID)%UnitLoadFactorSens
DefrostCap            = WarehouseCoil(CoilID)%DefrostCapacity
TEvap                 = WarehouseCoil(CoilID)%TEvapDesign
SHRCorrectionType     = WarehouseCoil(CoilID)%SHRCorrectionType
SHRCorrection60       = WarehouseCoil(CoilID)%SHRCorrection60
SHRCorrectionCurvePtr = WarehouseCoil(CoilID)%SHRCorrectionCurvePtr
FanMinAirFlowRatio    = WarehouseCoil(CoilID)%FanMinAirFlowRatio
FanSpeedControlType   = WarehouseCoil(CoilID)%FanType
MaxTemperatureDif     = WarehouseCoil(CoilID)%MaxTemperatureDif

IF (DefrostDripDownSchedule == 1.0d0) THEN
  AirVolumeFlowMax        = 0.d0
  DryAirMassFlowMax       = 0.d0
ELSE ! DefrostDripDownSchedule < 1.0d0, cooling will occur at least part of the time step
  SensLoadRequested    =   - QZnReq !here let cooling demand be positive within subroutine
  IF(SensLoadRequested <= 0.d0)THEN !No load so assume control keeps off, except that scheduled defrost still occurs
    AirVolumeFlowMax = 0.d0
    DryAirMassFlowMax       = 0.d0
  ELSE
    SensLoadRequestedGross = SensLoadRequested + HeaterLoad + FanPowerRated
    ZoneMixedAirDryBulb  =  Node(ZoneNodeNum)%Temp
    ZoneMixedAirHumRatio =  Node(ZoneNodeNum)%Humrat
    ZoneMixedAirRHFrac   =  PsyRhFnTdbWPb(ZoneMixedAirDryBulb,ZoneMixedAirHumRatio,OutBaroPress,TrackMessage)
    ZoneMixedAirEnthalpy =  PsyHFnTdbRhPb(ZoneMixedAirDryBulb,ZoneMixedAirRHFrac,OutBaroPress,TrackMessage)
    ZoneMixedAirDensity  =  PsyRhoAirFnPbTdbW(OutBaroPress,ZoneMixedAirDryBulb,ZoneMixedAirHumRatio,TrackMessage)
    ZoneDryAirDensity    =  PsyRhoAirFnPbTdbW(OutBaroPress,ZoneMixedAirDryBulb,0.d0,TrackMessage)
    ZoneMixedAirCp       =  PsyCpAirFnWTdb(ZoneMixedAirHumRatio,ZoneMixedAirDryBulb,TrackMessage)
    DryAirMassFlowRated  =  AirVolumeFlowRated * ZoneDryAirDensity
    !calc t inlet to coil assuming at middle/mixed point in room  bbb -
    !    later need to do for hottest/coolest in room where Tin /= Tzonemixed
    !calc RH inlet to coil assuming at middle/mixed point in room
    !calc coilcap, sens and latent, available as f(inlet T,RH)
    SELECT CASE(WarehouseCoil(CoilID)%VerticalLocation)
    CASE(Middle)
      CoilInletTemp     = ZoneMixedAirDryBulb
      CoilInletEnthalpy = ZoneMixedAirEnthalpy
      CoilInletRHFrac   = ZoneMixedAirRHFrac
      CoilInletDensity  = ZoneMixedAirDensity
      CoilInletCp       = ZoneMixedAirCp
      CoilInletHumRatio = ZoneMixedAirHumRatio
      CoilInletDryAirDensity = ZoneDryAirDensity
      CoilInletDryAirCp = PsyCpAirFnWTdb(0.0d0,CoilInletTemp,TrackMessage)
    CASE(Floor)
    CASE(Ceiling)
    END SELECT
    AirVolumeFlowMax  = AirVolumeFlowRated * (1.d0-DefrostDripDownSchedule) * CoilSchedule
    DryAirMassFlowMax = DryAirMassFlowRated *(1.d0-DefrostDripDownSchedule) * CoilSchedule

  END IF !Sens load requested is non-zero
END IF   ! DefrostDripDownSchedule == 1.0d0

IF(AirVolumeFlowMax > 0.d0) THEN

  TemperatureDif       = MIN(MaxTemperatureDif,(CoilInletTemp-TEvap))

  IF (WarehouseCoil(CoilID)%RatingType == RatedCapacityTotal) THEN
    ! RatingType = CapacityTotalSpecificConditions, will be doing a table lookup
    !    based upon RHInlet, DT1, CoilInletTemperature - see excel files from B. Nelson, CoilCom
    !    In the table, X1== inlet air dry bulb temperature
    !                  X2== Difference between inlet T and evap T
    !                  X3== RH expressed as decimal
    CoilCapTotEstimate = CurveValue(SHRCorrectionCurvePtr,CoilInletTemp,TemperatureDif,&
                         CoilInletRHFrac) * WarehouseCoil(CoilID)%RatedCapTotal * &
                         (1.d0-DefrostDripDownSchedule) * CoilSchedule

  ELSE   !work with unit load factor (sensible only), function of DT1 (Tair in drybulb-Tevap)
    SensibleCapacityMax  = WarehouseCoil(CoilID)%UnitLoadFactorSens*TemperatureDif * &
                              (1.d0-DefrostDripDownSchedule) * CoilSchedule

    IF (SensibleCapacityMax > 0.d0)  THEN
      ExitTemperatureEstimate = CoilInletTemp - &
            (SensibleCapacityMax/(DryAirMassFlowMax * CoilInletDryAirCp))
      IF(ExitTemperatureEstimate <= TEvap) THEN
        CALL ShowWarningError(TrackMessage//'Refrigeration:AirCoil: '//TRIM( WarehouseCoil(CoilID)%Name))
        CALL ShowContinueError(' The estimated air outlet temperature is less than the evaporating temperature.')
      END IF
      ExitEnthalpyEstimate = PsyHFnTdbRhPb(ExitTemperatureEstimate,1.0d0,OutBaroPress,TrackMessage)
      IF(ExitEnthalpyEstimate <= CoilInletEnthalpy) THEN
        CoilCapTotEstimate   = (CoilInletEnthalpy - ExitEnthalpyEstimate) * AirVolumeFlowMax * CoilInletDensity
      ELSE
        ! Assume no water is extracted from flow
        ExitEnthalpyEstimate = PsyHFnTdbW(ExitTemperatureEstimate,CoilInletHumRatio,TrackMessage)
        CoilCapTotEstimate   = (CoilInletEnthalpy - ExitEnthalpyEstimate) * AirVolumeFlowMax * CoilInletDensity
      END IF
      IF (SensibleCapacityMax > CoilCapTotEstimate)SensibleCapacityMax = CoilCapTotEstimate
      IF(ABS(CoilCapTotEstimate) > 0.d0) THEN
        SHR = SensibleCapacityMax/(CoilCapTotEstimate)
      ELSE
        ! will occur whenever defrost or dripdown
        SHR = 0.d0
      END IF

      SELECT CASE (SHRCorrectionType)
        CASE( SHR60)
          !line from y = SHRCorrection60 value to 1. as x(SHR) goes from .6 to 1, from B. Nelson, ASHRAE August 2010
          Slope = (SHRCorrection60 - 1.D0)/(.6D0-1.D0)
          yint  = SHRCorrection60-(Slope*.6D0)
          SHRCorrection =Slope*SHR + yint
        CASE(QuadraticSHR)
          SHRCorrection = CurveValue(SHRCorrectionCurvePtr,SHR)
        CASE(European)
          !With European ratings, either start with rated total sensible capacity or rated total capacity
          !    If rated total capacity is used, 'get input'
          !    translated it to rated total sensible capacity using
          !    PARAMETER ::EuropeanWetCoilFactor = (/1.35D0, 1.15D0,  1.05D0,  1.01D0,   1.0D0/)
          !    That sensible capacity rating was then turned to a rated UnitLoadFactor using
          !    the rated temperature difference. That sensible rating was also corrected
          !    for refrigerant and fin material in 'get input' and is given as UnitLoadFactor
          !  The total (sens + latent) capacity is equal to that * DT1 * WetCoilFactor(TcoilIn)
          !    Sensible capacity max already has DT1, just need WetCoilFactor(TcoilIn)
          !PARAMETER ::EuropeanWetCoilFactor = (/1.35D0, 1.15D0,  1.05D0,  1.01D0,   1.0D0/)
          !PARAMETER ::EuropeanAirInletTemp  = (/10.0D0,  0.0D0, -18.0D0, -25.0D0, -34.0D0/)
          !PARAMETER ::EuropeanEvapTemp      = (/ 0.0D0, -8.0D0, -25.0D0, -31.0D0, -40.0D0/)
          !PARAMETER ::EuropeanDT1           = (/10.0D0,  8.0D0,   7.0D0,   7.0D0,   6.0D0/)
          IF (CoilInletTemp <= -25.d0) THEN
            SHRCorrection = 1.0d0
          ELSE IF(CoilInletTemp > -25.d0 .and. CoilInletTemp <=0.0d0) THEN
            SHRCorrection = (EuropeanWetCoilFactor(2)-EuropeanWetCoilFactor(4))/ &
                            (EuropeanAirInletTemp(2) - EuropeanAirInletTemp(4)) * &
                            (EuropeanAirInletTemp(2) - CoilInletTemp) + &
                            EuropeanWetCoilFactor(4)
          ELSE IF(CoilInletTemp > 0.d0 .and. CoilInletTemp <=5.0d0) THEN
            SHRCorrection = (EuropeanWetCoilFactor(1)-EuropeanWetCoilFactor(2))/ &
                            (EuropeanAirInletTemp(1) - EuropeanAirInletTemp(2)) * &
                            (EuropeanAirInletTemp(1) - CoilInletTemp) + &
                            EuropeanWetCoilFactor(2)
          ELSE IF(CoilInletTemp > 5.d0) THEN
            SHRCorrection = EuropeanWetCoilFactor(1)
          END IF ! calc correction as a function of coil inlet temperature
      END SELECT
      CoilCapTotEstimate = SHRCorrection * SensibleCapacityMax
    ELSE ! NOT (SensibleCapacityMax > 0.d0)
      CoilCapTotEstimate = 0.d0
    END IF !  (SensibleCapacityMax > 0.d0)
  END IF ! Rating type : CapacityTotalSpecificConditions or Sensible Unit Load Factor

  IF (CoilCapTotEstimate > 0.0d0) THEN
      ExitEnthalpy    = CoilInletEnthalpy - (CoilCapTotEstimate/(AirVolumeFlowMax*CoilInletDensity))
      ExitTemperature = PsyTsatFnHPb(ExitEnthalpy,OutBaroPress,TrackMessage) !RH =1.0 at Tsat
      ExitHumRatio    = PsyWFnTdbH(ExitTemperature,ExitEnthalpy,TrackMessage)
      IF (ExitHumRatio > CoilInletHumRatio) ExitHumRatio = CoilInletHumRatio
      WaterRemovRate  = DryAirMassFlowMax *(CoilInletHumRatio - ExitHumRatio)
      LatLoadServed   = WaterRemovRate * IcetoVaporEnthalpy
      SensLoadGross   = CoilCapTotEstimate - LatLoadServed
      FanPowerActual  = FanPowerRated
      IF(SensLoadGross < 0.d0) THEN
        ! Could rarely happen during initial cooldown of a warm environment
        SensLoadGross = 0.d0
        LatLoadServed  = CoilCapTotEstimate
        WaterRemovRate = LatLoadServed / IcetoVaporEnthalpy
      END IF !SensLoadGross < 0
  ELSE ! NOT (SensibleCapacityMax > 0.d0)
      WaterRemovRate     = 0.d0
      LatLoadServed      = 0.d0
      SensLoadGross      = 0.d0
      FanPowerActual     = 0.d0
  END IF !(CoilCapTotEstimate > 0.d0)

  FanPowerMax = FanPowerRated*(1.0d0 - DefrostDripDownSchedule)
  IF(SensLoadGross > SensLoadRequestedGross) THEN !part load operation
    !don't need full chiller power, reduce fan speed to reduce air flow
    ! move fan to part power if need to
    CapFac =  SensLoadRequestedGross/SensLoadGross
    AirVolRatio=MAX(FanMinAirFlowRatio,CapFac**EvaporatorAirVolExponent)
                !Fans limited by minimum air flow ratio

    SELECT CASE (FanSpeedControlType)
      CASE(FanVariableSpeed)  !fan power law, adjusted for reality, applies
        FanPowerRatio=AirVolRatio**2.5d0
        FanPowerActual=FanPowerRatio*FanPowerMax
      CASE(FanConstantSpeed)
        FanPowerActual=AirVolRatio*EXP(1.d0-AirVolRatio)*FanPowerMax
      CASE(FanConstantSpeedLinear) !e.g., on-off control
        FanPowerActual=AirVolRatio*FanPowerMax
      CASE(FanTwoSpeed)
        !low speed setting of 1/2 fan speed can give up to 60% of capacity.
        !1/2 speed corresonds to ~1/8 power consumption (FanHalfSpeedRatio = 1/(2**2.5) = 0.1768)
        !dampers are used to control flow within those two ranges as in FanConstantSpeed
        IF(CapFac < CapFac60Percent) THEN
           FanPowerActual=((AirVolRatio+0.4d0)*(FanHalfSpeedRatio))*EXP(1.d0-AirVolRatio)*FanPowerMax
        ELSE
          FanPowerActual=AirVolRatio*EXP(1.d0-AirVolRatio)*FanPowerMax
        ENDIF !capfac60percent
      END SELECT  ! fan speed control type

    !reduce latent capacity according to value called for for sensible  - recalc latent.
    !   recalc coilcaptotal
    WaterRemovRate = WaterRemovRate*AirVolRatio
    LatLoadServed  = WaterRemovRate * IcetoVaporEnthalpy
    SensLoadGross = SensLoadRequestedGross
  ELSE  ! at full load
    FanPowerActual = FanPowerMax
  END IF ! part load and sensload served > 0.

  CoilCapTotal   = SensLoadGross + LatLoadServed
  IF(CoilCapTotal > 0.d0) THEN
    SHR = SensLoadGross / CoilCapTotal
  ELSE
    SHR = 0.d0
  END IF !(CoilCapTotal > 0.)

  !now handle ice on coil and defrost because defrost energy not into melting ice goes into sensible load
  ! FROST:  keep track of frost build up on evaporator coil
  !         avoid accumulation during warm-up to avoid reverse dd test problem
  IF(.NOT. WarmUpFlag) THEN
        FrostChangekg = (WaterRemovRate * TimeStepSys * SecInHour)
        WarehouseCoil(CoilID)%KgFrost = WarehouseCoil(CoilID)%KgFrost + FrostChangekg
  END IF

ELSE ! NOT (AirVolumeFlowMax > 0.d0)
  CoilCapTotEstimate = 0.d0
  WaterRemovRate     = 0.d0
  LatLoadServed      = 0.d0
  FrostChangekg      = 0.d0
  SensLoadGross      = 0.d0
  FanPowerActual     = 0.d0
END IF !(AirVolumeFlowMax > 0.d0)

!DEFROST CALCULATIONS   ***** need to reduce sensible heat to zone from
!                     defrost by amount used to melt ice. Last two elements
!                     in starting IF are there to mimic temperature override
!                     on the coils that stops defrost if the coils get above
!                     a certain temperature (such as when there's no load and no ice)
  IF((DefrostSchedule > 0.0d0).AND. &
      (WarehouseCoil(CoilID)%DefrostType /= DefrostNone) .AND. &
      (WarehouseCoil(CoilID)%DefrostType /=  DefrostOffCycle))THEN
      DefrostLoad   = DefrostCap * DefrostSchedule  !W
      DefrostEnergy = DefrostLoad * TimeStepSys * SecInHour !Joules
      StartFrostKg  = WarehouseCoil(CoilID)%KgFrost


      IF (WarehouseCoil(CoilID)%DefrostControlType == DefrostContTempTerm) THEN
        !  Need to turn defrost system off early if controlled by temperature and all ice melted
        !  For temperature termination, need to recognize not all defrost heat goes to melt ice
        !  Some goes to misc losses (for fluid defrost, some coil areas bare earlier than
        !  others and xfer heat to environment)
        !  Assume full ice melting satisfies temperature control.
        !      (defaults for DefEnergyFraction are :=0.7 for elec, =0.3 for fluids)
        DefEnergyFraction = WarehouseCoil(CoilID)%DefEnergyFraction
        AvailDefrostEnergy = DefEnergyFraction * DefrostEnergy !Joules avail to melt ice
        IceSensHeatNeeded = 0.d0
        IF(StartFrostKg > 0.0d0) THEN
          IF(WarehouseCoil(CoilID)%IceTemp < 0.d0) THEN
            StartIceTemp  = WarehouseCoil(CoilID)%IceTemp
            IceSensHeatNeeded = StartFrostKg * SpecificHeatIce * (0.0d0 - StartIceTemp)!Joules
            IF (AvailDefrostEnergy >= IceSensHeatNeeded) THEN
              WarehouseCoil(CoilID)%IceTemp = 0.d0
              AvailDefrostEnergy = AvailDefrostEnergy - IceSensHeatNeeded !Joules
            ELSE   !DefrostEnergy < IceSensHeatNeeded
              WarehouseCoil(CoilID)%IceTemp = &
                  StartIceTemp + AvailDefrostEnergy/(SpecificHeatIce * StartFrostKg)
              AvailDefrostEnergy = 0.d0
            END IF ! AvailDefrostEnergy >= IceSensHeatNeeded
          END IF   ! IceTemp < 0,  need to raise temperature of ice
          !Reduce defrost heat load on walkin by amount of ice melted during time step
          FrostChangekg = MIN(AvailDefrostEnergy/IceMeltEnthalpy,StartFrostKg)
          IF(FrostChangekg < StartFrostKg) THEN
            DefrostLoad = DefrostLoad - FrostChangekg*IceMeltEnthalpy/TimeStepSys/SecInHour
            IF(.NOT. WarmUpFlag)WarehouseCoil(CoilID)%KgFrost  = StartFrostKg  - FrostChangekg
            !DefrostSchedule not changed because ice not all melted, temp term not triggered
          ELSE     ! all frost melted during time step, so need to terminate defrost
                   !  see Aug 8 2010 page 3 notes
            WarehouseCoil(CoilID)%KgFrost  = 0.d0
            DefrostEnergyNeeded = (IceSensHeatNeeded + (FrostChangekg*IceMeltEnthalpy))/ &
                                  DefEnergyFraction  !Joules - energy needed including E unavail to melt ice
            DefrostSchedule  = MIN(DefrostSchedule,(DefrostEnergyNeeded/(DefrostCap*TimeStepSys*SecInHour)))
            ! reduce heat load on warehouse by energy put into ice melting
            DefrostRateNeeded = (IceSensHeatNeeded + (FrostChangekg*IceMeltEnthalpy))/(TimeStepSys*SecInHour)
            DefrostLoad = MAX(0.d0,(DefrostSchedule*DefrostCap - DefrostRateNeeded))
            WarehouseCoil(CoilID)%IceTemp = WarehouseCoil(CoilID)%TEvapDesign
          END IF   ! frost melted during time step less than amount of ice at start
        ELSE
          ! no frost present so terminate defrost and reset ice temperature for start of next defrost
          ! However, dripdown schedule still prevents/limits cooling capacity during time step
          DefrostLoad   = 0.d0
          DefrostSchedule = 0.d0
          WarehouseCoil(CoilID)%IceTemp = WarehouseCoil(CoilID)%TEvapDesign
        END IF     ! have frost present


      ELSE
        !Not temperature control type, controlled only by schedule
        !Reduce defrost heat load on the zone by amount of ice melted during time step
        !But DefrostSchedule not changed
        FrostChangekg = MAX(0.d0,MIN((DefrostEnergy/IceMeltEnthalpy),StartFrostKg))
        DefrostLoad = DefrostLoad - FrostChangekg*IceMeltEnthalpy/TimeStepSys/SecInHour
        IF(.NOT. WarmUpFlag)WarehouseCoil(CoilID)%KgFrost  = StartFrostKg  - FrostChangekg
      END IF       !Temperature termination vs. time-clock control type

  ELSE    !DefrostSchedule <= 0 or have None or OffCycle
    DefrostLoad   = 0.0d0
  END IF     !Defrost calculations

   SensLoadFromZone = SensLoadGross - HeaterLoad - DefrostLoad - FanPowerActual
   CoolingLoadNet   = SensLoadFromZone + LatLoadServed

  ! ReportWarehouseCoil(CoilID)
   WarehouseCoil(CoilID)%ThermalDefrostPower = DefrostLoad
   IF(WarehouseCoil(CoilID)%DefrostType == DefrostElec) THEN
        WarehouseCoil(CoilID)%ElecDefrostConsumption = DefrostCap*DefrostSchedule*TimeStepSys*SecInHour
        WarehouseCoil(CoilID)%ElecDefrostPower = DefrostCap*DefrostSchedule
   ELSE
        WarehouseCoil(CoilID)%ElecDefrostConsumption = 0.d0
        WarehouseCoil(CoilID)%ElecDefrostPower = 0.d0
   END IF

   ! If hot brine or hot gas is used for defrost, need to reduce condenser load by heat reclaimed for defrost
   IF(WarehouseCoil(CoilID)%DefrostType == DefrostFluid)WarehouseCoil(CoilID)%HotDefrostCondCredit=DefrostCap*DefrostSchedule
   ! LatentLoadServed is positive for latent heat removed from zone
   ! SensLoadFromZone positive for heat REMOVED from zone, switch when do credit to zone
   WarehouseCoil(CoilID)%SensCreditRate           = SensLoadFromZone
   WarehouseCoil(CoilID)%SensCreditEnergy         = SensLoadFromZone * TimeStepSys * SecInHour
   WarehouseCoil(CoilID)%LatCreditRate            = LatLoadServed
   WarehouseCoil(CoilID)%LatCreditEnergy          = LatLoadServed * TimeStepSys * SecInHour
   WarehouseCoil(CoilID)%LatKgPerS_ToZone         = WaterRemovRate
   WarehouseCoil(CoilID)%TotalCoolingLoad         = CoilCapTotal
   WarehouseCoil(CoilID)%TotalCoolingEnergy       = CoilCapTotal * TimeStepSys * SecInHour
   WarehouseCoil(CoilID)%SensCoolingEnergyRate    = SensLoadGross
   WarehouseCoil(CoilID)%SensCoolingEnergy        = SensLoadGross * TimeStepSys * SecInHour
   WarehouseCoil(CoilID)%SensHeatRatio            = SHR
   WarehouseCoil(CoilID)%ElecFanPower             = FanPowerActual
   WarehouseCoil(CoilID)%ElecFanConsumption       = FanPowerActual * TimeStepSys * SecInHour
   WarehouseCoil(CoilID)%ElecHeaterPower          = HeaterLoad
   WarehouseCoil(CoilID)%ElecHeaterConsumption    = HeaterLoad * TimeStepSys * SecInHour

   WarehouseCoil(CoilID)%TotalElecPower           = FanPowerActual + HeaterLoad + WarehouseCoil(CoilID)%ElecDefrostPower
   WarehouseCoil(CoilID)%TotalElecConsumption     = WarehouseCoil(CoilID)%TotalElecPower * TimeStepSys * SecInHour

   IF(WarehouseCoil(CoilID)%SensCreditRate >= 0.d0) THEN
     WarehouseCoil(CoilID)%ReportSensCoolCreditRate = WarehouseCoil(CoilID)%SensCreditRate
     WarehouseCoil(CoilID)%ReportHeatingCreditRate = 0.d0
   ELSE
     WarehouseCoil(CoilID)%ReportSensCoolCreditRate = 0.d0
     WarehouseCoil(CoilID)%ReportHeatingCreditRate = - WarehouseCoil(CoilID)%SensCreditRate
   END IF
   WarehouseCoil(CoilID)%ReportSensCoolCreditEnergy = WarehouseCoil(CoilID)%ReportSensCoolCreditRate * &
                                                      TimeStepSys * SecInHour
   WarehouseCoil(CoilID)%ReportHeatingCreditEnergy = WarehouseCoil(CoilID)%ReportHeatingCreditRate * &
                                                      TimeStepSys * SecInHour
   WarehouseCoil(CoilID)%ReportTotalCoolCreditRate = WarehouseCoil(CoilID)%ReportSensCoolCreditRate + &
                                                      WarehouseCoil(CoilID)%LatCreditRate
   WarehouseCoil(CoilID)%ReportTotalCoolCreditEnergy = WarehouseCoil(CoilID)%ReportSensCoolCreditEnergy + &
                                                      WarehouseCoil(CoilID)%LatCreditEnergy

!**************************************************************************************************
! Cap Kg Frost to avoid floating overflow errors
! 1-time warning is issued. It should be rare but could happen with unrealistic inputs.

  IF( WarehouseCoil(CoilID)%KgFrost > MyLargeNumber) THEN
     WarehouseCoil(CoilID)%KgFrost=MyLargeNumber
     IF(ShowCoilFrostWarning(CoilID)) THEN
      CALL ShowWarningError('Refrigeration:AirCoil: '//TRIM( WarehouseCoil(CoilID)%Name))
      CALL ShowContinueError(' This refrigerated air coil has insufficient defrost capacity '//  &
         'to remove the excess frost accumulation.')
      CALL ShowContinueError(' Check the defrost schedule or defrost capacity. ')
      CALL ShowContinueErrorTimeStamp('... Occurrence info ')
      ShowCoilFrostWarning( CoilID) = .FALSE.
    END IF
  END IF

  RETURN

END SUBROUTINE CalculateCoil

!***************************************************************************************************

SUBROUTINE FigureRefrigerationZoneGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! initialize zone gain terms at begin environment

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: BeginEnvrnFlag

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: MyEnvrnFlag = .TRUE.
  INTEGER  :: loop

  CALL CheckRefrigerationInput

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN

    IF (NumRefrigSystems > 0) THEN
      System%PipeHeatLoad       = 0.d0
      System%NetHeatRejectLoad  = 0.d0
    ENDIF

    IF (NumTransRefrigSystems > 0) THEN
      TransSystem%PipeHeatLoadMT    = 0.d0
      TransSystem%PipeHeatLoadLT    = 0.d0
      TransSystem%NetHeatRejectLoad = 0.d0
    ENDIF

    IF (NumRefrigeratedRacks > 0) THEN
      RefrigRack%SensZoneCreditHeatRate = 0.d0
      RefrigRack%SensHVACCreditHeatRate = 0.d0
    ENDIF

    IF (NumSimulationSecondarySystems > 0) THEN
      Secondary%DistPipeZoneHeatGain = 0.d0
      Secondary%ReceiverZoneHeatGain = 0.d0
    ENDIF

    IF (NumSimulationWalkIns > 0) THEN
      DO loop=1, NumSimulationWalkIns
        WalkIn(loop)%SensZoneCreditRate = 0.d0
        WalkIn(loop)%LatZoneCreditRate  = 0.d0
      ENDDO
    ENDIF
    IF (NumSimulationCases > 0) THEN
      RefrigCase%SensZoneCreditRate    = 0.d0
      RefrigCase%SensHVACCreditRate    = 0.d0
      RefrigCase%LatZoneCreditRate     = 0.d0
      RefrigCase%LatHVACCreditRate     = 0.d0
    ENDIF
    MyEnvrnFlag = .FALSE.
  ENDIF
  IF( .NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

  RETURN

  ! should model above terms for use during sizing here
!  IF(DoingSizing)THEN


!  ENDIF

END SUBROUTINE FigureRefrigerationZoneGains

!***************************************************************************************************

SUBROUTINE ZeroHVACValues

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         T. Stovall
          !       DATE WRITTEN   Aug 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Reset all values that communicate outside module for HVAC steps
          ! to zero when called on zone timestep. Otherwise, values may be held over when
          ! no HVAC load calls module during that zone time step.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataWater,   ONLY: WaterStorage
  USE PlantUtilities,  ONLY : SetComponentFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER      :: DemandARRID = 0     ! Index to water tank Demand used for evap condenser
  INTEGER      :: TankID    = 0       ! Index to water tank used for evap condenser
  INTEGER      :: RackNum   = 0       ! Index to refrigerated rack
  INTEGER      :: CondID     = 0      ! Index to condenser
  INTEGER      :: PlantInletNode = 0  ! Used to zero request for cooling water for condenser
  INTEGER      :: PlantOutletNode = 0 ! Used to zero request for cooling water for condenser
  INTEGER      :: PlantLoopIndex = 0  ! Used to zero request for cooling water for condenser
  INTEGER      :: PlantLoopSideIndex = 0  ! Used to zero request for cooling water for condenser
  INTEGER      :: PlantBranchIndex = 0  ! Used to zero request for cooling water for condenser
  INTEGER      :: PlantCompIndex   = 0  ! Used to zero request for cooling water for condenser
  REAL(r64)    :: MassFlowRate     =0.0d0 ! Used to zero request for cooling water for condenser


  IF(HaveRefrigRacks) THEN
    !HaveRefrigRacks is TRUE when NumRefrigeratedRAcks > 0
    !RefrigRack ALLOCATED to NumRefrigeratedRacks
    DO RackNum = 1,NumRefrigeratedRacks
      IF (RefrigRack(RackNum)%CondenserType == RefrigCondenserTypeWater) THEN
        PlantInletNode     = RefrigRack(RackNum)%InletNode
        PlantOutletNode    = RefrigRack(RackNum)%OutletNode
        PlantLoopIndex     = RefrigRack(RackNum)%PlantLoopNum
        PlantLoopSideIndex = RefrigRack(RackNum)%PlantLoopSideNum
        PlantBranchIndex   = RefrigRack(RackNum)%PlantBranchNum
        PlantCompIndex     = RefrigRack(RackNum)%PlantCompNum
        MassFlowRate       = 0.d0
        CALL SetComponentFlowRate(MassFlowRate, &
                             PlantInletNode, PlantOutletNode, &
                             PlantLoopIndex, PlantLoopSideIndex, &
                             PlantBranchIndex, PlantCompIndex )
      END IF
      IF (RefrigRack(RackNum)%CondenserType == RefrigCondenserTypeEvap) THEN
         IF (RefrigRack(RackNum)%EvapWaterSupplyMode == WaterSupplyFromTank) THEN
           DemandARRID = RefrigRack(RackNum)%EvapWaterTankDemandARRID
           TankID = RefrigRack(RackNum)%EvapWaterSupTankID
           WaterStorage(TankID)%VdotRequestDemand(DemandARRID) = 0.d0
         END IF
      END IF
    END DO ! RackNum
  END IF !HaveRefrigRacks

  IF(NumRefrigCondensers.GT.0) THEN
    !Condenser ALLOCATED to NumRefrigCondensers
    DO CondID = 1,NumRefrigCondensers
      IF (Condenser(CondID)%CondenserType == RefrigCondenserTypeWater) THEN
        PlantInletNode     = Condenser(CondID)%InletNode
        PlantOutletNode    = Condenser(CondID)%OutletNode
        PlantLoopIndex     = Condenser(CondID)%PlantLoopNum
        PlantLoopSideIndex = Condenser(CondID)%PlantLoopSideNum
        PlantBranchIndex   = Condenser(CondID)%PlantBranchNum
        PlantCompIndex     = Condenser(CondID)%PlantCompNum
        MassFlowRate       = 0.d0
        CALL SetComponentFlowRate(MassFlowRate, &
                             PlantInletNode, PlantOutletNode, &
                             PlantLoopIndex, PlantLoopSideIndex, &
                             PlantBranchIndex, PlantCompIndex )
      END IF
      IF (Condenser(CondID)%CondenserType == RefrigCondenserTypeEvap) THEN
        IF (Condenser(CondID)%EvapWaterSupplyMode == WaterSupplyFromTank) THEN
           DemandARRID = Condenser(CondID)%EvapWaterTankDemandARRID
           TankID = Condenser(CondID)%EvapWaterSupTankID
           WaterStorage(TankID)%VdotRequestDemand(DemandARRID) = 0.d0
        END IF
      END IF
    END DO ! ICond
  END IF ! NumRefrigCondensers>0

  RETURN

END SUBROUTINE ZeroHVACValues

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

END MODULE RefrigeratedCase
