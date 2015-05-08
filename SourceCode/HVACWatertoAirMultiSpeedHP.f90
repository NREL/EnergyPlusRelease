MODULE VariableSpeedCoils

 ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataHVACGlobals
USE Psychrometrics
Use DataEnvironment, ONLY: StdBaroPress, EnvironmentName, CurMnDy, OutDryBulbTemp, OutHumRat, OutBaroPress, OutWetBulbTemp
USE DataInterfaces
USE DataSizing
USE DataPlant,       ONLY: TypeOf_CoilVSWAHPHeatingEquationFit, TypeOf_CoilVSWAHPCoolingEquationFit
USE General,         ONLY: RoundSigDigits

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS

REAL(r64), PARAMETER ::    RatedInletAirTemp   = 26.6667d0   ! 26.6667C or 80F
REAL(r64), PARAMETER ::    RatedInletWetbulbTemp = 19.44d0   ! 19.44 or 67F, cooling mode
REAL(r64), PARAMETER ::    RatedInletAirHumRat = 0.01125d0   ! Humidity ratio corresponding to 80F dry bulb/67F wet bulb
REAL(r64), PARAMETER ::    RatedInletWaterTemp = 29.4d0      ! 85 F cooling mode
REAL(r64), PARAMETER ::    RatedAmbAirTemp = 35.0d0      ! 95 F cooling mode
REAL(r64), PARAMETER ::    RatedInletAirTempHeat = 21.11d0   ! 21.11C or 70F, heating mode
REAL(r64), PARAMETER ::    RatedInletWaterTempHeat = 21.11d0  ! 21.11C or 70F, heating mode
REAL(r64), PARAMETER ::    RatedAmbAirTempHeat = 8.33d0  ! 8.33 or 47F, heating mode
REAL(r64), PARAMETER ::    RatedAmbAirWBHeat = 6.11d0   !  8.33 or 43F, heating mode, rated wet bulb temperature

! Airflow per total capacity range
REAL(r64), PARAMETER ::    MaxRatedVolFlowPerRatedTotCap = 0.00006041d0 ! m3/s per watt = 450 cfm/ton
REAL(r64), PARAMETER ::    MinRatedVolFlowPerRatedTotCap = 0.00004027d0 ! m3/s per watt = 300 cfm/ton
REAL(r64), PARAMETER ::    MaxHeatVolFlowPerRatedTotCap  = 0.00008056d0 ! m3/s per watt = 600 cfm/ton
REAL(r64), PARAMETER ::    MaxCoolVolFlowPerRatedTotCap  = 0.00006713d0 ! m3/s per watt = 500 cfm/ton
REAL(r64), PARAMETER ::    MinOperVolFlowPerRatedTotCap  = 0.00002684d0 ! m3/s per watt = 200 cfm/ton

!Water Systems
INTEGER, PARAMETER :: CondensateDiscarded = 1001 ! default mode where water is "lost"
INTEGER, PARAMETER :: CondensateToTank    = 1002 ! collect coil condensate from air and store in water storage tank

INTEGER, PARAMETER :: WaterSupplyFromMains = 101
INTEGER, PARAMETER :: WaterSupplyFromTank  = 102

! Curve Types
INTEGER, PARAMETER :: Linear      = 1
INTEGER, PARAMETER :: Bilinear    = 2
INTEGER, PARAMETER :: Quadratic   = 3
INTEGER, PARAMETER :: Biquadratic = 4
INTEGER, PARAMETER :: Cubic       = 5

! Defrost strategy (heat pump only)
INTEGER, PARAMETER :: ReverseCycle     = 1 ! uses reverse cycle defrost strategy
INTEGER, PARAMETER :: Resistive        = 2 ! uses electric resistance heater for defrost
! Defrost control  (heat pump only)
INTEGER, PARAMETER :: Timed            = 1 ! defrost cycle is timed
INTEGER, PARAMETER :: OnDemand         = 2 ! defrost cycle occurs only when required

INTEGER, PUBLIC, PARAMETER :: MaxSpedLevels = 10  ! Maximum number of speed that supports

  ! DERIVED TYPE DEFINITIONS
TYPE, PUBLIC :: VariableSpeedCoilData ! variable speed coil
  CHARACTER(len=MaxNameLength) :: Name           =' '    ! Name of the  Coil
  CHARACTER(len=MaxNameLength) :: VarSpeedCoilType     =' '    ! type of coil

  INTEGER :: NumOfSpeeds    =2   ! Number of speeds
  INTEGER :: NormSpedLevel    =MaxSpedLevels   ! Nominal speed level

  REAL(r64) :: RatedWaterVolFlowRate           =AUTOSIZE  ! Rated/Ref Water Volumetric Flow Rate [m3/s]
  REAL(r64) :: RatedWaterMassFlowRate          =AUTOSIZE  ! Rated/Ref Water Volumetric Flow Rate [m3/s]
  REAL(r64) :: RatedAirVolFlowRate             =AUTOSIZE  ! Rated/Ref Air Volumetric Flow Rate [m3/s]
  REAL(r64) :: RatedCapHeat                    =AUTOSIZE  ! Rated/Ref Heating Capacity [W]
  REAL(r64) :: RatedCapCoolTotal               =AUTOSIZE  ! Rated/Ref Total Cooling Capacity [W]

  REAL(r64):: MaxONOFFCyclesperHour = 0.0d0 ! Maximum ON/OFF cycles per hour for the compressor (cycles/hour)

  REAL(r64):: Twet_Rated = 0.0d0      ! Nominal time for condensate to begin leaving the coil's
                                                   ! condensate drain line (sec)
  REAL(r64):: Gamma_Rated = 0.0d0     ! Initial moisture evaporation rate divided by steady-state
                                                   ! AC latent capacity (dimensionless)
  INTEGER :: HOTGASREHEATFLG = 0    !whether to use hot gas reheat
  REAL(r64) :: HPTimeConstant                  =0.0d0  ! Heat pump time constant [s]

  INTEGER:: PLFFPLR = 0     ! index of part load curve as a function of part load ratio

  CHARACTER(len=MaxNameLength) :: CoolHeatType=' ' ! Type of WatertoAirHP ie. Heating or Cooling
  INTEGER   :: VSCoilTypeOfNum              = 0     ! type of component in plant
  LOGICAL   :: Simflag                         =.false. ! Heat Pump Simulation Flag
  REAL(r64) :: DesignWaterMassFlowRate         =0.0d0  ! design water mass flow rate [kg/s]
  REAL(r64) :: DesignWaterVolFlowRate          =0.0d0  ! design water volumetric flow rate [m3/s]
  REAL(r64) :: DesignAirMassFlowRate           =0.0d0  ! Design Air Mass Flow Rate [kg/s]
  REAL(r64) :: DesignAirVolFlowRate            =0.0d0  ! Design Air Volumetric Flow Rate [m3/s]
  REAL(r64) :: AirVolFlowRate                  =0.0d0  ! Air Volumetric Flow Rate[m3/s], real time
  REAL(r64) :: AirMassFlowRate                 =0.0d0  ! Air Mass Flow Rate[kg/s], real time
  REAL(r64) :: InletAirPressure                =0.0d0 !air inlet pressure [pa]
  REAL(r64) :: InletAirDBTemp                  =0.0d0  ! Inlet Air Dry Bulb Temperature [C], real time
  REAL(r64) :: InletAirHumRat                  =0.0d0  ! Inlet Air Humidity Ratio [kg/kg], real time
  REAL(r64) :: InletAirEnthalpy                =0.0d0  ! Inlet Air Enthalpy [J/kg], real time
  REAL(r64) :: OutletAirDBTemp                 =0.0d0  ! Outlet Air Dry Bulb Temperature [C], real time
  REAL(r64) :: OutletAirHumRat                 =0.0d0  ! Outlet Air Humidity Ratio [kg/kg], real time
  REAL(r64) :: OutletAirEnthalpy               =0.0d0  ! Outlet Air Enthalpy [J/kg], real time
  REAL(r64) :: WaterVolFlowRate                =0.0d0  ! Water Volumetric Flow Rate [m3/s], real time
  REAL(r64) :: WaterMassFlowRate               =0.0d0  ! Water Mass Flow Rate [kg/s], real time
  REAL(r64) :: InletWaterTemp                  =0.0d0  ! Inlet Water Temperature [C]
  REAL(r64) :: InletWaterEnthalpy              =0.0d0  ! Inlet Water Enthalpy [J/kg]
  REAL(r64) :: OutletWaterTemp                 =0.0d0  ! Outlet Water Temperature [C]
  REAL(r64) :: OutletWaterEnthalpy             =0.0d0  ! Outlet Water Enthalpy [J/kg]
  REAL(r64) :: Power                           =0.0d0  ! Power Consumption [W]
  REAL(r64) :: QLoadTotal                      =0.0d0  ! Load Side Total Heat Transfer Rate [W]
  REAL(r64) :: QSensible                       =0.0d0  ! Sensible Load Side Heat Transfer Rate [W]
  REAL(r64) :: QLatent                         =0.0d0  ! Latent Load Side Heat Transfer Rate [W]
  REAL(r64) :: QSource                         =0.0d0  ! Source Side Heat Transfer Rate [W]
  REAL(r64) :: QWasteHeat                      =0.0d0  ! Recoverable waste Heat Transfer Rate [W]
  REAL(r64) :: Energy                          =0.0d0  ! Energy Consumption [J]
  REAL(r64) :: EnergyLoadTotal                 =0.0d0  ! Load Side Total Heat Transferred [J]
  REAL(r64) :: EnergySensible                  =0.0d0  ! Sensible Load Side Heat Transferred [J]
  REAL(r64) :: EnergyLatent                    =0.0d0  ! Latent Load Side Heat Transferred [J]
  REAL(r64) :: EnergySource                    =0.0d0  ! Source Side Heat Transferred [J]
  REAL(r64) :: COP                             =0.0d0  ! Heat Pump Coefficient of Performance [-]
  REAL(r64) :: RunFrac                         =0.0d0  ! Duty Factor
  REAL(r64) :: PartLoadRatio                   =0.0d0  ! Part Load Ratio

  REAL(r64) :: RatedPowerHeat                  =0.0d0  ! Rated/Ref Heating Power Consumption[W]
  REAL(r64) :: RatedCOPHeat                    =0.0d0  ! Rated/Ref Heating COP [W/W]
  REAL(r64) :: RatedCapCoolSens                =0.0d0  ! Rated/Ref Sensible Cooling Capacity [W]
  REAL(r64) :: RatedPowerCool                  =0.0d0  ! Rated/Ref Cooling Power Consumption[W]
  REAL(r64) :: RatedCOPCool                    =0.0d0  ! Rated/Ref Cooling COP [W/W]

  INTEGER      :: AirInletNodeNum              =0    ! Node Number of the Air Inlet
  INTEGER      :: AirOutletNodeNum             =0     ! Node Number of the Air Outlet
  INTEGER      :: WaterInletNodeNum            =0     ! Node Number of the Water Onlet
  INTEGER      :: WaterOutletNodeNum           =0     ! Node Number of the Water Outlet
  INTEGER      :: LoopNum                      =0    ! plant loop index for water side
  INTEGER      :: LoopSide                     =0    ! plant loop side index
  INTEGER      :: BranchNum                    =0    ! plant branch index
  INTEGER      :: CompNum                      =0    ! plant component index
! set by parent object and "pushed" to this structure in SetVSWSHPData subroutine

  LOGICAL      :: FindCompanionUpStreamCoil    = .TRUE.    ! Flag to get the companion coil in Init.
  INTEGER      :: CompanionCoolingCoilNum      =0           ! Heating coil companion cooling coil index
  INTEGER      :: CompanionHeatingCoilNum      =0           ! Cooling coil companion heating coil index

  REAL(r64) :: FanDelayTime                    =0.0d0  ! Fan delay time, time delay for the HP's fan to

  ! beginning for multispeed coil type
  INTEGER       :: MSErrIndex(MaxSpedLevels) = 0                ! index flag for num speeds/recurring messages
  REAL(r64)     :: MSRatedPercentTotCap(MaxSpedLevels) = 0.0d0    ! Percentage to the total cooling capacity for MS heat pump at the highest speed [dimensionless]
  REAL(r64)     :: MSRatedTotCap(MaxSpedLevels) = 0.0d0           ! Rated cooling capacity for MS heat pump [W]
  REAL(r64)     :: MSRatedSHR(MaxSpedLevels) = 0.0d0              ! Rated SHR for MS heat pump [dimensionless]
  REAL(r64)     :: MSRatedCOP(MaxSpedLevels) = 0.0d0              ! Rated COP for MS heat pump [dimensionless]

  REAL(r64)     :: MSRatedAirVolFlowPerRatedTotCap(MaxSpedLevels) = 0.0d0
  ! Rated Air volume flow rate per total capacity through unit at rated conditions [m^3/w]
  REAL(r64)     :: MSRatedAirVolFlowRate(MaxSpedLevels) = 0.0d0
  ! Air volume flow rate through unit at rated conditions [m3/s]
  REAL(r64)     :: MSRatedAirMassFlowRate(MaxSpedLevels) = 0.0d0
  ! Air mass flow rate through unit at rated conditions [kg/s]

  REAL(r64)     :: MSRatedWaterVolFlowPerRatedTotCap(MaxSpedLevels) = 0.0d0
  ! Rated water volume flow rate per total  capacity through unit at rated conditions [m^3/w]
  REAL(r64)     :: MSRatedWaterVolFlowRate(MaxSpedLevels) = 0.0d0
  ! Water volume flow rate through unit at rated conditions [m3/s]
  REAL(r64)     :: MSRatedWaterMassFlowRate(MaxSpedLevels) = 0.0d0
  ! Water mass flow rate through unit at rated conditions [kg/s]

  REAL(r64)     :: MSRatedCBF(MaxSpedLevels) = 0.0d0
  ! rated coil bypass factor
  REAL(r64)     :: MSEffectiveAo(MaxSpedLevels) = 0.0d0
  ! effective heat transfer surface at each speed

  INTEGER       :: MSCCapFTemp(MaxSpedLevels) = 0
  ! index of total capacity modifier curve
  INTEGER       :: MSCCapAirFFlow(MaxSpedLevels) = 0
  ! index of total capacity modifier curve as a function of air flow
  INTEGER       :: MSCCapWaterFFlow(MaxSpedLevels) = 0
  ! index of total capacity modifier curve as a function of water flow
  INTEGER       :: MSEIRFTemp(MaxSpedLevels) = 0
  ! index of energy input ratio modifier curve as a function of temperature
  INTEGER       :: MSEIRAirFFlow(MaxSpedLevels) = 0
  ! index of energy input ratio modifier curve as a function of air flow fraction
  INTEGER       :: MSEIRWaterFFlow(MaxSpedLevels) = 0
  ! index of energy input ratio modifier curve as a function of water flow fraction
  INTEGER       :: MSWasteHeat(MaxSpedLevels) = 0
  ! index of waste heat as a function of temperature
  REAL(r64)     :: MSWasteHeatFrac(MaxSpedLevels) = 0.0d0
  ! Waste heat fraction

  REAL(r64):: SpeedNumReport = 0.0d0
  !speed number for output
  REAL(r64):: SpeedRatioReport = 0.0d0
  !speed ratio for output between two neighboring speeds
  ! End of multispeed water source coil input

  !----------------------------------------------------------------
  !added variables and arrays for variable speed air-source heat pump
  !defrosting
  INTEGER :: DefrostStrategy        = 0   ! defrost strategy; 1=reverse-cycle, 2=resistive
  INTEGER :: DefrostControl         = 0   ! defrost control; 1=timed, 2=on-demand
  INTEGER :: EIRFPLR                = 0   ! index of energy input ratio vs part-load ratio curve
  INTEGER :: DefrostEIRFT           = 0   ! index of defrost mode total cooling capacity for reverse cycle heat pump
  REAL(r64) :: MinOATCompressor       =0.0d0  ! Minimum OAT for heat pump compressor operation
  REAL(r64) :: OATempCompressorOn   = 0.0d0  ! The outdoor tempearture when the compressor is automatically turned back on,
                                           ! if applicable, following automatic shut off. This field is used only for
                                           ! HSPF calculation.
  REAL(r64) :: MaxOATDefrost          =0.0d0  ! Maximum OAT for defrost operation
  REAL(r64) :: DefrostTime            =0.0d0  ! Defrost time period in hours
  REAL(r64) :: DefrostCapacity        =0.0d0  ! Resistive defrost to nominal capacity (at 21.11C/8.33C) ratio
  REAL(r64) :: HPCompressorRuntime    =0.0d0  ! keep track of compressor runtime
  REAL(r64) :: HPCompressorRuntimeLast=0.0d0  ! keep track of last time step compressor runtime (if simulation downshifts)
  REAL(r64) :: TimeLeftToDefrost      =0.0d0  ! keep track of time left to defrost heat pump
  REAL(r64) :: DefrostPower           =0.0d0  ! power used during defrost
  REAL(r64) :: DefrostConsumption     =0.0d0  ! energy used during defrost

  !crankcase heater
  LOGICAL :: ReportCoolingCoilCrankcasePower =.true. ! logical determines if the cooling coil crankcase heater power is reported
  REAL(r64) :: CrankcaseHeaterCapacity =0.0d0 ! total crankcase heater capacity [W]
  REAL(r64) :: CrankcaseHeaterPower    =0.0d0 ! report variable for average crankcase heater power [W]
  REAL(r64) :: MaxOATCrankcaseHeater   =0.0d0 ! maximum OAT for crankcase heater operation [C]
  REAL(r64) :: CrankcaseHeaterConsumption  = 0.0d0 ! report variable for total crankcase heater energy consumption [J]

  !condenser evaporative precooling
  INTEGER :: CondenserInletNodeNum = 0  ! Node number of outdoor condenser
  INTEGER :: CondenserType = AirCooled ! Type of condenser for DX cooling coil: AIR COOLED or EVAP COOLED
  LOGICAL :: ReportEvapCondVars =.false. ! true if any performance mode includes an evap condenser
  REAL(r64) :: EvapCondPumpElecNomPower =0.0d0  ! Nominal power input to the evap condenser water circulation pump [W]
  REAL(r64) :: EvapCondPumpElecPower =0.0d0    ! Average power consumed by the evap condenser water circulation pump over
                                           ! the time step [W]
  REAL(r64) :: EvapWaterConsumpRate =0.0d0 ! Evap condenser water consumption rate [m3/s]
  REAL(r64) :: EvapCondPumpElecConsumption =0.0d0 ! Electric energy consumed by the evap condenser water circulation pump [J]
  REAL(r64) :: EvapWaterConsump =0.0d0 ! Evap condenser water consumption [m3]
  REAL(r64) :: BasinHeaterConsumption    = 0.0d0  ! Basin heater energy consumption (J)


  REAL(r64) :: BasinHeaterPowerFTempDiff   = 0.0d0 ! Basin heater capacity per degree C below setpoint (W/C)
  REAL(r64) :: BasinHeaterSetPointTemp     = 0.0d0 ! setpoint temperature for basin heater operation (C)
  REAL(r64) :: BasinHeaterPower          = 0.0d0  ! Basin heater power (W)
  INTEGER   :: BasinHeaterSchedulePtr  = 0      ! Pointer to basin heater schedule

  REAL(r64) :: EvapCondAirFlow(MaxSpedLevels)=0.0d0  ! Air flow rate through the evap condenser at high speed, volumetric flow rate
                                                 ! for water use calcs [m3/s]
  REAL(r64) :: EvapCondEffect(MaxSpedLevels) =0.0d0  ! effectiveness of the evaporatively cooled condenser
                                            ! [high speed for multi-speed unit] (-)
  REAL(r64) :: MSRatedEvapCondVolFlowPerRatedTotCap(MaxSpedLevels) = 0.0d0 !evap condenser air flow ratio to capacity

  !begin variables for Water System interactions
  INTEGER ::EvapWaterSupplyMode                   = WaterSupplyFromMains !  where does water come from
  CHARACTER(len=MaxNameLength) :: EvapWaterSupplyName = ' ' ! name of water source e.g. water storage tank
  INTEGER ::EvapWaterSupTankID                    = 0 !
  INTEGER ::EvapWaterTankDemandARRID              = 0 !
  INTEGER ::CondensateCollectMode                 = CondensateDiscarded !  where does water come from
  CHARACTER(len=MaxNameLength) :: CondensateCollectName = ' ' ! name of water source e.g. water storage tank
  INTEGER ::CondensateTankID                      = 0 !
  INTEGER ::CondensateTankSupplyARRID             = 0 !

  REAL(r64)   :: CondensateVdot = 0.0d0 ! rate of water condensation from air stream [m3/s]
  REAL(r64)   :: CondensateVol  = 0.0d0 ! amount of water condensed from air stream [m3]

  REAL(r64)   :: CondInletTemp  =0.0d0            ! Evap condenser inlet temperature [C], report variable

  REAL(r64)   :: SourceAirMassFlowRate = 0.0d0 ! source air mass flow rate [kg/s]
  REAL(r64)   :: InletSourceAirTemp = 0.0d0 !source air temperature entering the outdoor coil [C]
  REAL(r64)   :: InletSourceAirEnthalpy = 0.0d0  !source air enthalpy entering the outdoor coil [J/kg]

  !end variables for water system interactions

END TYPE VariableSpeedCoilData


  ! MODULE VARIABLE DECLARATIONS:
  ! Identifier is VarSpeedCoil
INTEGER        :: NumWatertoAirHPs  = 0        ! The Number of Water to Air Heat Pumps found in the Input

LOGICAL        :: GetCoilsInputFlag = .TRUE.       ! Flag set to make sure you get input once
TYPE (VariableSpeedCoilData) , PUBLIC, ALLOCATABLE, DIMENSION(:) :: VarSpeedCoil
! LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag

REAL(r64) :: SourceSideMassFlowRate =0.0d0 ! Source Side Mass flow rate [Kg/s]
REAL(r64) :: SourceSideInletTemp    =0.0d0 ! Source Side Inlet Temperature [C]
REAL(r64) :: SourceSideInletEnth    =0.0d0 ! Source Side Inlet Enthalpy [J/kg]
REAL(r64) :: LoadSideMassFlowRate   =0.0d0 ! Load Side Mass flow rate [Kg/s]
REAL(r64) :: LoadSideInletDBTemp    =0.0d0 ! Load Side Inlet Dry Bulb Temp [C]
REAL(r64) :: LoadSideInletWBTemp    =0.0d0 ! Load Side Inlet Wet Bulb Temp [C]
REAL(r64) :: LoadSideInletHumRat    =0.0d0 ! Load Side Outlet Humidity ratio
REAL(r64) :: LoadSideInletEnth      =0.0d0 ! Load Side Inlet Enthalpy [J/kg]
REAL(r64) :: LoadSideOutletDBTemp   =0.0d0 ! Load Side Outlet Dry Bulb Temp [C]
REAL(r64) :: LoadSideOutletHumRat   =0.0d0 ! Load Side Outlet Humidity ratio
REAL(r64) :: LoadSideOutletEnth     =0.0d0 ! Load Side Outlet Enthalpy [J/kg]
REAL(r64) :: QSensible              =0.0d0 ! Load side sensible heat transfer rate [W]
REAL(r64) :: QLoadTotal             =0.0d0 ! Load side total heat transfer rate [W]
REAL(r64) :: QLatRated              =0.0d0 ! Latent Capacity [W] rated at entering air conditions [Tdb=26.7C Twb=19.4C]
REAL(r64) :: QLatActual             =0.0d0 ! Actual Latent Capacity [W]
REAL(r64) :: QSource                =0.0d0 ! Source side heat transfer rate [W]
REAL(r64) :: Winput                 =0.0d0 ! Power Consumption [W]
REAL(r64) :: PLRCorrLoadSideMdot    =0.0d0 ! Load Side Mdot corrected for Part Load Ratio of the unit

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

          ! Driver/Manager Routines
PUBLIC SimVariableSpeedCoils

          ! Get Input routines for module
PRIVATE GetVarSpeedCoilInput

          ! Initialization routines for module
PRIVATE InitVarSpeedCoil
PRIVATE SizeVarSpeedCoil

          ! Update routines to check convergence and update nodes
PUBLIC  CalcVarSpeedCoilCooling
PUBLIC  CalcVarSpeedCoilHeating

          ! Update routine
PRIVATE UpdateVarSpeedCoil

          ! Utility routines
PUBLIC  GetCoilIndexVariableSpeed
PUBLIC  GetCoilCapacityVariableSpeed
PUBLIC  GetCoilInletNodeVariableSpeed
PUBLIC  GetCoilOutletNodeVariableSpeed
PUBLIC  GetCoilAirFlowRateVariableSpeed
PUBLIC  GetVSCoilCondenserInletNode
PUBLIC  GetVSCoilMinOATCompressor
PUBLIC  GetVSCoilNumOfSpeeds
PUBLIC  SetVarSpeedCoilData
        !SHR, bypass factor routines
PRIVATE CalcEffectiveSHR
PRIVATE CalcTotCapSHR_VSWSHP
PRIVATE CalcCBF
PRIVATE AdjustCBF

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimVariableSpeedCoils(CompName,CompIndex,&
           CyclingScheme,MaxONOFFCyclesperHour, &
           HPTimeConstant,FanDelayTime,CompOp, PartLoadFrac, OnOffAirFlowRat,SpeedNum, SpeedRatio, &
           SensLoad, LatentLoad)

          !       AUTHOR         Bo Shen, ORNL
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages variable-speed Water to Air Heat Pump component simulation.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! N/A



          ! USE STATEMENTS:
  USE InputProcessor,       ONLY: FindItemInList
  USE DataHVACGlobals,      ONLY: TimestepSys
  USE FluidProperties,      ONLY: FindGlycol
  USE General,              ONLY: TrimSigDigits, SolveRegulaFalsi

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  CHARACTER(len=*), INTENT(IN)      :: CompName                     ! Coil Name
  INTEGER, INTENT(INOUT)            :: CompIndex                    ! Index for Component name
  INTEGER, INTENT(IN)               :: CyclingScheme                ! Continuous fan OR cycling compressor
  REAL(r64), INTENT (INOUT)         :: MaxONOFFCyclesperHour        ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64), INTENT (INOUT)         :: HPTimeConstant               ! Heat pump time constant [s]
  REAL(r64), INTENT (INOUT)         :: FanDelayTime                 ! Fan delay time, time delay for the HP's fan to
                                                                    ! shut off after compressor cycle off  [s]
  INTEGER, INTENT(IN)               :: CompOp                       ! compressor on/off. 0 = off; 1= on
  REAL(r64), INTENT(IN)             :: PartLoadFrac
  ! part-load ratio = load/total capacity, passed in by the parent object
  REAL(r64), OPTIONAL, INTENT(IN)   :: OnOffAirFlowRat              ! ratio of comp on to comp off air flow rate
  REAL(r64), INTENT(IN)             :: SensLoad                     ! Sensible demand load [W]
  REAL(r64), INTENT(IN)             :: LatentLoad                   ! Latent demand load [W]
  REAL(r64), INTENT(IN)             :: SpeedRatio                   ! compressor speed ratio
  INTEGER, INTENT(IN)               :: SpeedNum                     ! compressor speed number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: DXCoilNum                         ! The WatertoAirHP that you are currently loading input into
  REAL(r64)                    :: OnOffAirFlowRatio                 ! ratio of comp on to comp off air flow rate
  REAL(r64)                    :: RuntimeFrac                       ! run time fraction
  INTEGER                      :: SpeedCal                          ! variable for error proof speed input

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (CompIndex == 0) THEN
    DXCoilNum = FindItemInList(CompName,VarSpeedCoil%Name,NumWatertoAirHPs )
    IF (DXCoilNum == 0) THEN
      CALL ShowFatalError('WaterToAirHPVSWEquationFit not found='//TRIM(CompName))
    ENDIF
    CompIndex=DXCoilNum
  ELSE
    DXCoilNum=CompIndex
    IF (DXCoilNum > NumWatertoAirHPs  .or. DXCoilNum < 1) THEN
      CALL ShowFatalError('SimVariableSpeedCoils: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DXCoilNum))// &
                          ', Number of Water to Air HPs='//TRIM(TrimSigDigits(NumWatertoAirHPs))//  &
                          ', WaterToAir HP name='//TRIM(CompName))
    ENDIF
    IF (CompName /= Blank .AND. CompName /= VarSpeedCoil(DXCoilNum)%Name) THEN
      CALL ShowFatalError('SimVariableSpeedCoils: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DXCoilNum))// &
                          ', WaterToAir HP name='//TRIM(CompName)//', stored WaterToAir HP Name for that index='//  &
                          TRIM(VarSpeedCoil(DXCoilNum)%Name))
    ENDIF
  ENDIF

  IF(PRESENT(OnOffAirFlowRat))THEN
    OnOffAirFlowRatio = OnOffAirFlowRat
  ELSE
    OnOffAirFlowRatio = 1.0d0
  END IF

 !ERROR PROOF
 IF(SpeedNum < 1) THEN
    SpeedCal = 1
 ELSE
    SpeedCal = SpeedNum
 END IF

 IF((VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit) &
    .OR. (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==Coil_CoolingAirToAirVariableSpeed) )THEN
    ! Cooling mode
    CALL InitVarSpeedCoil(DXCoilNum,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime,SensLoad,LatentLoad,CyclingScheme, &
                                  OnOffAirFlowRatio, SpeedRatio, SpeedCal)
    CALL CalcVarSpeedCoilCooling(DXCoilNum,CyclingScheme,RuntimeFrac,&
                SensLoad,LatentLoad,CompOp,PartLoadFrac,OnOffAirFlowRatio, SpeedRatio, SpeedCal)
    CALL UpdateVarSpeedCoil(DXCoilNum)
 ELSEIF((VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit)  &
    .OR. (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==Coil_HeatingAirToAirVariableSpeed) ) THEN
    ! Heating mode
    CALL InitVarSpeedCoil(DXCoilNum,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime,SensLoad,LatentLoad,CyclingScheme, &
                                  OnOffAirFlowRatio, SpeedRatio, SpeedCal)
    CALL CalcVarSpeedCoilHeating(DXCoilNum,CyclingScheme,RuntimeFrac,&
                SensLoad,CompOp,PartLoadFrac,OnOffAirFlowRatio, SpeedRatio, SpeedCal)
    CALL UpdateVarSpeedCoil(DXCoilNum)
 ELSE
    CALL ShowFatalError ('SimVariableSpeedCoils: WatertoAir heatpump not in either HEATING or COOLING mode')
 ENDIF

 ! two additional output variables
 VarSpeedCoil(DXCoilNum)%SpeedNumReport = SpeedCal
 VarSpeedCoil(DXCoilNum)%SpeedRatioReport = SpeedRatio

 RETURN

END SUBROUTINE SimVariableSpeedCoils

SUBROUTINE GetVarSpeedCoilInput

       ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for HPs and stores it in HP data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! n/a

          ! USE STATEMENTS:
    USE InputProcessor
    USE NodeInputManager
    USE BranchNodeConnections, ONLY: TestCompSet
    USE GlobalNames,     ONLY: VerifyUniqueCoilName
    USE OutputReportPredefined
    USE General,               ONLY: TrimSigDigits
    USE CurveManager,          ONLY: GetCurveIndex, GetCurveType, CurveValue, SetCurveOutputMinMaxValues
    USE OutAirNodeManager,     ONLY: CheckOutAirNodeNumber
    USE WaterManager,          ONLY: SetupTankDemandComponent, SetupTankSupplyComponent
    USE ScheduleManager,       ONLY: GetScheduleIndex

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER (len=*), PARAMETER   :: RoutineName='GetVarSpeedCoilInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: DXCoilNum                    ! The Water to Air HP that you are currently loading input into
    INTEGER :: NumCool                      ! Counter for cooling coil, water source
    INTEGER :: NumCoolAS                    ! Counter for cooling coil, air source
    INTEGER :: NumHeat                      ! Counter for heating coil, water source
    INTEGER :: NumHeatAS                    ! Counter for heating coil, air source
    INTEGER :: WatertoAirHPNum              ! Counter
    INTEGER :: I                            ! Loop index increment
    INTEGER :: NumAlphas                    ! Number of variables in String format
    INTEGER :: NumNums                      ! Number of variables in Numeric format
    INTEGER :: NumParams                    ! Total number of input fields
    INTEGER :: MaxNums=0                    ! Maximum number of numeric input fields
    INTEGER :: MaxAlphas=0                  ! Maximum number of alpha input fields
    INTEGER :: IOSTAT
    INTEGER :: AlfaFieldIncre               !increment number of Alfa field
    LOGICAL :: ErrorsFound = .FALSE.        ! If errors detected in input
    LOGICAL       :: IsNotOK                ! Flag to verify name
    LOGICAL       :: IsBlank                ! Flag for blank name
    LOGICAL       :: errflag
    REAL(r64) :: CurveVal                   ! Used to verify modifier curves equal 1 at rated conditions
    CHARACTER (len=MaxNameLength)  :: CurrentModuleObject                       ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray        ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields     ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields   ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray                            ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks                        ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks                      ! Logical array, numeric field input BLANK = .true.

    NumCool   = GetNumObjectsFound('COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT')
    NumHeat   = GetNumObjectsFound('COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT')
    NumCoolAS  = GetNumObjectsFound('COIL:COOLING:DX:VARIABLESPEED')
    NumHeatAS   = GetNumObjectsFound('COIL:HEATING:DX:VARIABLESPEED')
    NumWatertoAirHPs = NumCool+NumHeat+NumCoolAS+NumHeatAS
    DXCoilNum=0

    IF(NumWatertoAirHPs <= 0) THEN
      CALL ShowSevereError('No Equipment found in SimWatertoAirHPSimple')
      ErrorsFound=.TRUE.
    END IF

   ! Allocate Arrays
    IF (NumWatertoAirHPs.GT.0) THEN
      ALLOCATE(VarSpeedCoil(NumWatertoAirHPs))
    ENDIF

    CALL GetObjectDefMaxArgs('COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

    CALL GetObjectDefMaxArgs('COIL:COOLING:DX:VARIABLESPEED',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('COIL:HEATING:DX:VARIABLESPEED',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

    ALLOCATE(AlphArray(MaxAlphas))
    AlphArray=' '
    ALLOCATE(cAlphaFields(MaxAlphas))
    cAlphaFields=' '
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.TRUE.
    ALLOCATE(cNumericFields(MaxNums))
    cNumericFields=' '
    ALLOCATE(lNumericBlanks(MaxNums))
    lNumericBlanks=.TRUE.
    ALLOCATE(NumArray(MaxNums))
    NumArray=0.0d0

    ! Get the data for cooling coil, WATER SOURCE
    CurrentModuleObject = 'Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit' !for reporting

    DO WatertoAirHPNum = 1, NumCool

        DXCoilNum= DXCoilNum + 1
        AlfaFieldIncre = 1

        CALL GetObjectItem(CurrentModuleObject,DXCoilNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.

        CALL VerifyName(AlphArray(1),VarSpeedCoil%Name,DXCoilNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        VarSpeedCoil(DXCoilNum)%Name     = TRIM(AlphArray(1))
        VarSpeedCoil(DXCoilNum)%CoolHeatType  = 'COOLING'
        VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum  = TypeOf_CoilVSWAHPCoolingEquationFit
        VarSpeedCoil(DXCoilNum)%NumOfSpeeds = INT(NumArray(1))
        VarSpeedCoil(DXCoilNum)%NormSpedLevel = INT(NumArray(2))
        VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal=NumArray(3)
        VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate = NumArray(4)
        VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate=NumArray(5)
        VarSpeedCoil(DXCoilNum)%Twet_Rated=NumArray(6)
        VarSpeedCoil(DXCoilNum)%Gamma_Rated=NumArray(7)
        VarSpeedCoil(DXCoilNum)%HOTGASREHEATFLG=INT(NumArray(8))
        VarSpeedCoil(DXCoilNum)%CondenserType = WaterCooled

        VarSpeedCoil(DXCoilNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
        VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
        VarSpeedCoil(DXCoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        VarSpeedCoil(DXCoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(4),AlphArray(5),'Air Nodes')


     !   If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 2) Then
      If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 1) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(1))//' must be >= 1.'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(1),0)))
          ErrorsFound=.TRUE.
        End If

        If (VarSpeedCoil(DXCoilNum)%NormSpedLevel > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) THEN
            VarSpeedCoil(DXCoilNum)%NormSpedLevel = VarSpeedCoil(DXCoilNum)%NumOfSpeeds
        END IF

        If ((VarSpeedCoil(DXCoilNum)%NormSpedLevel > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) &
            .OR. (VarSpeedCoil(DXCoilNum)%NormSpedLevel <= 0)) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(2))//' must be valid speed level'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(2),0)))
          ErrorsFound=.TRUE.
        End If

        !part load curve
        VarSpeedCoil(DXCoilNum)%PLFFPLR = GetCurveIndex(AlphArray(6)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%PLFFPLR .EQ. 0) THEN
          IF (lAlphaBlanks(6)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(6))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(6))//'="'//TRIM(AlphArray(6))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%PLFFPLR,1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)&
                        //'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(6))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF
        END IF

        Do I=1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds
            VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I) = NumArray(9+(I-1)*6)
            VarSpeedCoil(DXCoilNum)%MSRatedSHR(I)    = NumArray(10+(I-1)*6)
            VarSpeedCoil(DXCoilNum)%MSRatedCOP(I)    = NumArray(11+(I-1)*6)
            VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(I) = NumArray(12+(I-1)*6)
            VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(I) = NumArray(13+(I-1)*6)
            VarSpeedCoil(DXCoilNum)%MSWasteHeatFrac(I) = NumArray(14+(I-1)*6)

            AlfaFieldIncre = 7+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))&
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is BiQuadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I),RatedInletWetbulbTemp,RatedInletWaterTemp)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                            //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 8+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre ))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'&
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre ))//'="' &
                        //TRIM(AlphArray(AlfaFieldIncre ))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)// &
                        '="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre ))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName// &
                        trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre ))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 9+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                            //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre =10+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//&
                        TRIM(cAlphaFields(AlfaFieldIncre))//'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Biquadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I),RatedInletWetbulbTemp,RatedInletWaterTemp)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(1))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre =11+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))&
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                            //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject) &
                        //'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                      TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

           AlfaFieldIncre =12+(I-1)*7
           VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                            //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                      TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre =13+(I-1)*7
            ! Read waste heat modifier curve name
            VarSpeedCoil(DXCoilNum)%MSWasteHeat(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSWasteHeat(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal types are BiQuadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSWasteHeat(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSWasteHeat(I),RatedInletWaterTemp,RatedInletAirTemp)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSWasteHeat(I))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

        END DO

        Do I=1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds
            VarSpeedCoil(DXCoilNum)%MSRatedPercentTotCap(I) =VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)/ &
                    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(VarSpeedCoil(DXCoilNum)%NumOfSpeeds)
            VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(I) = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(I)/ &
                    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)
            VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowPerRatedTotCap(I) = VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(I)/ &
                    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)
        END DO

        CALL SetupOutputVariable('Cooling Coil Electric Energy [J]', &
             VarSpeedCoil(DXCoilNum)%Energy,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='Electric',EndUseKey='Cooling',GroupKey='System')

        CALL SetupOutputVariable('Cooling Coil Total Cooling Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergyLoadTotal,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')

        CALL SetupOutputVariable('Cooling Coil Sensible Cooling Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergySensible,'System','Summed',VarSpeedCoil(DXCoilNum)%Name)

        CALL SetupOutputVariable('Cooling Coil Latent Cooling Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergyLatent,'System','Summed',VarSpeedCoil(DXCoilNum)%Name)

        CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergySource,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,   &
             ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')

        !for table output, being consistent with outher water-to-air coils
!        IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal /= AutoSize) THEN
!            VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
!                *VarSpeedCoil(DXCoilNum)%MSRatedSHR(VarSpeedCoil(DXCoilNum)%NormSpedLevel)
!        ELSE
!            VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = AUTOSIZE
!        END IF

        VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = AUTOSIZE !always auto-sized, to be determined in the sizing calculation

        ! BAN Sept 30 2103, CR9322, commented out, now it is redundant, it is reported from sizing routine
        !!create predefined report entries
        !CALL PreDefTableEntry(pdchCoolCoilType,VarSpeedCoil(DXCoilNum)%Name,CurrentModuleObject)
        !CALL PreDefTableEntry(pdchCoolCoilTotCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
        !CALL PreDefTableEntry(pdchCoolCoilSensCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
        !CALL PreDefTableEntry(pdchCoolCoilLatCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
        !                         - VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
        !CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens &
        !                         / VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
        !CALL PreDefTableEntry(pdchCoolCoilNomEff,VarSpeedCoil(DXCoilNum)%Name,&
        !        VarSpeedCoil(DXCoilNum)%MSRatedCOP(VarSpeedCoil(DXCoilNum)%NormSpedLevel))
   END DO

      !-------------------------AIR SOURCE, COOLING---BEGIN
   ! Get the data for cooling coil, AIR SOURCE
     CurrentModuleObject = 'Coil:Cooling:DX:VariableSpeed' !for reporting

    DO WatertoAirHPNum = 1, NumCoolAS

        DXCoilNum= DXCoilNum + 1
        AlfaFieldIncre = 1

        CALL GetObjectItem(CurrentModuleObject,DXCoilNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.

        CALL VerifyName(AlphArray(1),VarSpeedCoil%Name,DXCoilNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        VarSpeedCoil(DXCoilNum)%Name     = TRIM(AlphArray(1))
        VarSpeedCoil(DXCoilNum)%CoolHeatType  = 'COOLING'
        VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum  = Coil_CoolingAirToAirVariableSpeed
        VarSpeedCoil(DXCoilNum)%NumOfSpeeds = INT(NumArray(1))
        VarSpeedCoil(DXCoilNum)%NormSpedLevel = INT(NumArray(2))
        VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal=NumArray(3)
        VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate = NumArray(4)
        VarSpeedCoil(DXCoilNum)%Twet_Rated=NumArray(5)
        VarSpeedCoil(DXCoilNum)%Gamma_Rated=NumArray(6)

        VarSpeedCoil(DXCoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        VarSpeedCoil(DXCoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Air Nodes')

        If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 1) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(1))//' must be >= 1.'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(1),0)))
          ErrorsFound=.TRUE.
        End If

        If (VarSpeedCoil(DXCoilNum)%NormSpedLevel > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) THEN
            VarSpeedCoil(DXCoilNum)%NormSpedLevel = VarSpeedCoil(DXCoilNum)%NumOfSpeeds
        END IF

        If ((VarSpeedCoil(DXCoilNum)%NormSpedLevel > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) &
            .OR. (VarSpeedCoil(DXCoilNum)%NormSpedLevel <= 0)) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(2))//' must be valid speed level'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(2),0)))
          ErrorsFound=.TRUE.
        End If

        !part load curve
        VarSpeedCoil(DXCoilNum)%PLFFPLR = GetCurveIndex(AlphArray(4)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%PLFFPLR .EQ. 0) THEN
          IF (lAlphaBlanks(4)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(6))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(4))//'="'//TRIM(AlphArray(4))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%PLFFPLR,1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)&
                        //'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(4))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF
        END IF

! outdoor condenser node
       IF (lAlphaBlanks(5)) THEN
         VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum = 0
       ELSE
        VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum = &
           GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),VarSpeedCoil(DXCoilNum)%Name, &
                             NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)

         IF (.not. CheckOutAirNodeNumber(VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum)) THEN
          CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//  &
             '", may be invalid')
          CALL ShowContinueError(TRIM(cAlphaFields(10))//'="'//TRIM(AlphArray(5))// &
                                   '", node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
          CALL ShowContinueError('This node needs to be included in an air system or the coil model will not be valid' &
                                 //', and the simulation continues')
         END IF
       ENDIF

       IF ((SameString(AlphArray(6),'AirCooled')) .OR. lAlphaBlanks(6)) THEN
         VarSpeedCoil(DXCoilNum)%CondenserType = AirCooled
       ELSEIF (SameString(AlphArray(6),'EvaporativelyCooled')) THEN
         VarSpeedCoil(DXCoilNum)%CondenserType = EvapCooled
         VarSpeedCoil(DXCoilNum)%ReportEvapCondVars = .TRUE.
       ELSE
         CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
         CALL ShowContinueError('...'//TRIM(cAlphaFields(6))//'="'//TRIM(AlphArray(6))//'":')
         CALL ShowContinueError('...must be AirCooled or EvaporativelyCooled.')
         ErrorsFound = .TRUE.
       END IF

      VarSpeedCoil(DXCoilNum)%EvapCondPumpElecNomPower = NumArray(7)

      IF (VarSpeedCoil(DXCoilNum)%EvapCondPumpElecNomPower .LT. 0.0d0) THEN
         CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
         CALL ShowContinueError('...'//trim(cNumericFields(7))//' cannot be < 0.0.')
         CALL ShowContinueError('...entered value=['//trim(TrimSigDigits(NumArray(7),2))//'].')
         ErrorsFound = .TRUE.
      END IF

  !Set crankcase heater capacity
       VarSpeedCoil(DXCoilNum)%CrankcaseHeaterCapacity = NumArray(8)
       IF (VarSpeedCoil(DXCoilNum)%CrankcaseHeaterCapacity .LT. 0.0d0) THEN
         CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
         CALL ShowContinueError('...'//trim(cNumericFields(8))//' cannot be < 0.0.')
         CALL ShowContinueError('...entered value=['//trim(TrimSigDigits(NumArray(8),2))//'].')
         ErrorsFound = .TRUE.
       END IF

  !Set crankcase heater cutout temperature
      VarSpeedCoil(DXCoilNum)%MaxOATCrankcaseHeater = NumArray(9)

  ! Get Water System tank connections
  !  A7, \field Name of Water Storage Tank for Supply
     VarSpeedCoil(DXCoilNum)%EvapWaterSupplyName = AlphArray(7)
     IF (lAlphaBlanks(7)) THEN
       VarSpeedCoil(DXCoilNum)%EvapWaterSupplyMode = WaterSupplyFromMains
     ELSE
       VarSpeedCoil(DXCoilNum)%EvapWaterSupplyMode = WaterSupplyFromTank
       CALL SetupTankDemandComponent(VarSpeedCoil(DXCoilNum)%Name,TRIM(CurrentModuleObject), &
                 VarSpeedCoil(DXCoilNum)%EvapWaterSupplyName, ErrorsFound, VarSpeedCoil(DXCoilNum)%EvapWaterSupTankID, &
                 VarSpeedCoil(DXCoilNum)%EvapWaterTankDemandARRID )
     ENDIF

  !A8; \field Name of Water Storage Tank for Condensate Collection
     VarSpeedCoil(DXCoilNum)%CondensateCollectName = AlphArray(8)
     IF (lAlphaBlanks(8)) THEN
       VarSpeedCoil(DXCoilNum)%CondensateCollectMode = CondensateDiscarded
     ELSE
       VarSpeedCoil(DxCoilNum)%CondensateCollectMode = CondensateToTank
       CALL SetupTankSupplyComponent(VarSpeedCoil(DXCoilNum)%Name,TRIM(CurrentModuleObject), &
                 VarSpeedCoil(DXCoilNum)%CondensateCollectName, ErrorsFound, VarSpeedCoil(DXCoilNum)%CondensateTankID, &
                 VarSpeedCoil(DXCoilNum)%CondensateTankSupplyARRID )
     END IF

  !   Basin heater power as a function of temperature must be greater than or equal to 0
     VarSpeedCoil(DxCoilNum)%BasinHeaterPowerFTempDiff = NumArray(10)
     IF(NumArray(10) .LT. 0.0d0) THEN
       CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
       CALL ShowContinueError('...'//trim(cNumericFields(10))//' must be >= 0.0.')
       CALL ShowContinueError('...entered value=['//trim(TrimSigDigits(NumArray(10),2))//'].')
       ErrorsFound = .TRUE.
     END IF

    VarSpeedCoil(DxCoilNum)%BasinHeaterSetPointTemp = NumArray(11)
    IF(VarSpeedCoil(DxCoilNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(VarSpeedCoil(DxCoilNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//  &
           '", freeze possible')
        CALL ShowContinueError('...'//trim(cNumericFields(11))//' is < 2 {C}. Freezing could occur.')
        CALL ShowContinueError('...entered value=['//trim(TrimSigDigits(NumArray(11),2))//'].')
      END IF
    END IF

    IF(.NOT. lAlphaBlanks(9))THEN
      VarSpeedCoil(DxCoilNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(AlphArray(9))
      IF(VarSpeedCoil(DxCoilNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
        CALL ShowContinueError('...not found '//TRIM(cAlphaFields(14))//'="'//TRIM(AlphArray(9))//'".')
        CALL ShowContinueError('Basin heater will be available to operate throughout the simulation.')
      END IF
    END IF

    Do I=1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds
        VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I) = NumArray(12+(I-1)*6)
        VarSpeedCoil(DXCoilNum)%MSRatedSHR(I)    = NumArray(13+(I-1)*6)
        VarSpeedCoil(DXCoilNum)%MSRatedCOP(I)    = NumArray(14+(I-1)*6)
        VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(I) = NumArray(15+(I-1)*6)
        VarSpeedCoil(DXCoilNum)%EvapCondAirFlow(I) = NumArray(16+(I-1)*6)

        VarSpeedCoil(DXCoilNum)%EvapCondEffect(I) = NumArray(17+(I-1)*6)
        IF (VarSpeedCoil(DXCoilNum)%EvapCondEffect(I) .LT. 0.0d0 .OR. VarSpeedCoil(DXCoilNum)%EvapCondEffect(I) .GT. 1.0d0) THEN
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//trim(cNumericFields(17+(I-1)*6))//' cannot be < 0.0 or > 1.0.')
          CALL ShowContinueError('...entered value=['//trim(TrimSigDigits(NumArray(17+(I-1)*6),2))//'].')
          ErrorsFound = .TRUE.
        END IF

        AlfaFieldIncre = 10+(I-1)*4
        VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I) .EQ. 0) THEN
          IF (lAlphaBlanks(AlfaFieldIncre)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))&
                    //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
          ! Verify Curve Object, only legal type is BiQuadratic
          SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I)))

          CASE('BIQUADRATIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I),RatedInletWetbulbTemp,RatedAmbAirTemp)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                 TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I))))
            CALL ShowContinueError('Curve type must be BiQuadratic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF

        AlfaFieldIncre = 11+(I-1)*4
        VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I) .EQ. 0) THEN
          IF (lAlphaBlanks(AlfaFieldIncre)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre ))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'&
                    //trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre ))//'="' &
                    //TRIM(AlphArray(AlfaFieldIncre ))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
          ! Verify Curve Object, only legal type is Quadratic
          SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I)))

          CASE('QUADRATIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)// &
                    '="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre ))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE('CUBIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName// &
                    trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre ))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                 TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I))))
            CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF

        AlfaFieldIncre =12+(I-1)*4
        VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I) .EQ. 0) THEN
          IF (lAlphaBlanks(AlfaFieldIncre)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//&
                    TRIM(cAlphaFields(AlfaFieldIncre))//'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
          ! Verify Curve Object, only legal type is Biquadratic
          SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I)))

          CASE('BIQUADRATIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I),RatedInletWetbulbTemp,RatedAmbAirTemp)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                    //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                 TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(1))))
            CALL ShowContinueError('Curve type must be BiQuadratic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF

        AlfaFieldIncre =13+(I-1)*4
        VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I) .EQ. 0) THEN
          IF (lAlphaBlanks(AlfaFieldIncre)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))&
                    //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
          ! Verify Curve Object, only legal type is Quadratic
          SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I)))

          CASE('QUADRATIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE('CUBIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject) &
                    //'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                  TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I))))
            CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF
      END DO

      Do I=1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds
            VarSpeedCoil(DXCoilNum)%MSRatedPercentTotCap(I) =VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)/ &
                    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(VarSpeedCoil(DXCoilNum)%NumOfSpeeds)
            VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(I) = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(I)/ &
                    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)
            VarSpeedCoil(DXCoilNum)%MSRatedEvapCondVolFlowPerRatedTotCap(I) = VarSpeedCoil(DXCoilNum)%EvapCondAirFlow(I)/ &
                    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)
      END DO

      CALL SetupOutputVariable('Cooling Coil Electric Energy [J]', &
             VarSpeedCoil(DXCoilNum)%Energy,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='Electric',EndUseKey='Cooling',GroupKey='System')

      CALL SetupOutputVariable('Cooling Coil Total Cooling Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergyLoadTotal,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')

      CALL SetupOutputVariable('Cooling Coil Sensible Cooling Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergySensible,'System','Summed',VarSpeedCoil(DXCoilNum)%Name)

      CALL SetupOutputVariable('Cooling Coil Latent Cooling Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergyLatent,'System','Summed',VarSpeedCoil(DXCoilNum)%Name)

      CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergySource,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,   &
             ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')


      VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = AUTOSIZE !always auto-sized, to be determined in the sizing calculation

      ! BAN Sept 30 2103, CR9322, commented out, now it is redundant, it is reported from sizing routine
        !create predefined report entries
      !CALL PreDefTableEntry(pdchCoolCoilType,VarSpeedCoil(DXCoilNum)%Name,CurrentModuleObject)
      !CALL PreDefTableEntry(pdchCoolCoilTotCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
      !CALL PreDefTableEntry(pdchCoolCoilSensCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
      !CALL PreDefTableEntry(pdchCoolCoilLatCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
      !                           - VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
      !CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens &
      !                           / VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
      !CALL PreDefTableEntry(pdchCoolCoilNomEff,VarSpeedCoil(DXCoilNum)%Name, &
      !      VarSpeedCoil(DXCoilNum)%MSRatedCOP(VarSpeedCoil(DXCoilNum)%NormSpedLevel))

   END DO

   !-------------------------AIR SOURCE COOLING---END

   ! Get the data for heating coil, WATER SOURCE
   CurrentModuleObject = 'Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit'

   DO WatertoAirHPNum = 1, NumHeat

        DXCoilNum= DXCoilNum + 1

        CALL GetObjectItem(CurrentModuleObject,WatertoAirHPNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.

        CALL VerifyName(AlphArray(1),VarSpeedCoil%Name,DXCoilNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        VarSpeedCoil(DXCoilNum)%Name     = TRIM(AlphArray(1))
        VarSpeedCoil(DXCoilNum)%CoolHeatType  = 'HEATING'
        VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum  = TypeOf_CoilVSWAHPHeatingEquationFit
        VarSpeedCoil(DXCoilNum)%NumOfSpeeds = INT(NumArray(1))
        VarSpeedCoil(DXCoilNum)%NormSpedLevel = INT(NumArray(2))
        VarSpeedCoil(DXCoilNum)%RatedCapHeat=NumArray(3)
        VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate = NumArray(4)
        VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate=NumArray(5)
        VarSpeedCoil(DXCoilNum)%CondenserType = WaterCooled

        VarSpeedCoil(DXCoilNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
        VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
        VarSpeedCoil(DXCoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        VarSpeedCoil(DXCoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(4),AlphArray(5),'Air Nodes')


 !       If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 2) Then
        If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 1) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(1))//' must be >= 1.'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(1),0)))
          ErrorsFound=.TRUE.
        End If

        If (VarSpeedCoil(DXCoilNum)%NormSpedLevel > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) THEN
            VarSpeedCoil(DXCoilNum)%NormSpedLevel = VarSpeedCoil(DXCoilNum)%NumOfSpeeds
        END IF

        If ((VarSpeedCoil(DXCoilNum)%NormSpedLevel > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) &
            .OR. (VarSpeedCoil(DXCoilNum)%NormSpedLevel <= 0)) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(2))//' must be valid speed level'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(2),0)))
          ErrorsFound=.TRUE.
        End If

        !part load curve
        VarSpeedCoil(DXCoilNum)%PLFFPLR = GetCurveIndex(AlphArray(6)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%PLFFPLR .EQ. 0) THEN
          IF (lAlphaBlanks(6)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(6))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(6))//'="'//TRIM(AlphArray(6))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%PLFFPLR,1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                    //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(6))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF
        END IF

        Do I=1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds
            VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I) = NumArray(6+(I-1)*5)
            VarSpeedCoil(DXCoilNum)%MSRatedCOP(I)    = NumArray(7+(I-1)*5)
            VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(I) = NumArray(8+(I-1)*5)
            VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(I) = NumArray(9+(I-1)*5)
            VarSpeedCoil(DXCoilNum)%MSWasteHeatFrac(I) = NumArray(10+(I-1)*5)

            AlfaFieldIncre = 7+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                            //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is BiQuadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I),RatedInletAirTempHeat,RatedInletWaterTempHeat)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 8+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))//'="' &
                        //TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 9+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))//'="'//TRIM(AlphArray(14+(I-1)*6))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                            //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                            //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 10+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Biquadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I),RatedInletAirTempHeat,RatedInletWaterTempHeat)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(1))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 11+(I-1)*7
            VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))//'="'// &
                    TRIM(AlphArray(16+(I-1)*6))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject) &
                        //'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                      TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

           AlfaFieldIncre = 12+(I-1)*7
           VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))// &
                        '="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)&
                        //'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                      TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 13+(I-1)*7
            ! Read waste heat modifier curve name
            VarSpeedCoil(DXCoilNum)%MSWasteHeat(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (VarSpeedCoil(DXCoilNum)%MSWasteHeat(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal types are BiQuadratic
              SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSWasteHeat(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSWasteHeat(I),RatedInletAirTempHeat,RatedInletWaterTempHeat)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSWasteHeat(I))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

        END DO

        Do I=1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds
            VarSpeedCoil(DXCoilNum)%MSRatedPercentTotCap(I) =VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)/ &
                    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(VarSpeedCoil(DXCoilNum)%NumOfSpeeds)
            VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(I) = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(I)/ &
                        VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)
            VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowPerRatedTotCap(I) = VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(I)/ &
                        VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)
        END DO

        CALL SetupOutputVariable('Heating Coil Electric Energy [J]', &
             VarSpeedCoil(DXCoilNum)%Energy,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='Electric',EndUseKey='Heating',GroupKey='System')

        CALL SetupOutputVariable('Heating Coil Heating Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergyLoadTotal,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')

        CALL SetupOutputVariable('Heating Coil Source Side Heat Transfer Energy [J]', &
             VarSpeedCoil(DXCoilNum)%EnergySource,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='HEATINGCOILS',GroupKey='System')

        !create predefined report entries
        CALL PreDefTableEntry(pdchHeatCoilType,VarSpeedCoil(DXCoilNum)%Name,CurrentModuleObject)
        CALL PreDefTableEntry(pdchHeatCoilNomCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapHeat)
        CALL PreDefTableEntry(pdchHeatCoilNomEff,VarSpeedCoil(DXCoilNum)%Name,&
                    VarSpeedCoil(DXCoilNum)%MSRatedCOP(VarSpeedCoil(DXCoilNum)%NormSpedLevel))

   END DO

   !-------------------------AIR SOURCE, HEATING---BEGIN
   ! Get the data for heating coil, AIR SOURCE
   CurrentModuleObject = 'COIL:HEATING:DX:VARIABLESPEED'

   DO WatertoAirHPNum = 1, NumHeatAS

      DXCoilNum= DXCoilNum + 1

      CALL GetObjectItem(CurrentModuleObject,WatertoAirHPNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.

      CALL VerifyName(AlphArray(1),VarSpeedCoil%Name,DXCoilNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
         ErrorsFound=.TRUE.
         IF (IsBlank) AlphArray(1)='xxxxx'
      ENDIF
      CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
      IF (errflag) THEN
        ErrorsFound=.true.
      ENDIF

      VarSpeedCoil(DXCoilNum)%Name     = TRIM(AlphArray(1))
      VarSpeedCoil(DXCoilNum)%CoolHeatType  = 'HEATING'
      VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum  = Coil_HeatingAirToAirVariableSpeed
      VarSpeedCoil(DXCoilNum)%NumOfSpeeds = INT(NumArray(1))
      VarSpeedCoil(DXCoilNum)%NormSpedLevel = INT(NumArray(2))
      VarSpeedCoil(DXCoilNum)%RatedCapHeat=NumArray(3)
      VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate = NumArray(4)

      VarSpeedCoil(DXCoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      VarSpeedCoil(DXCoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Air Nodes')


      If (VarSpeedCoil(DXCoilNum)%NumOfSpeeds .LT. 1) Then
        CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
        CALL ShowContinueError('...'//TRIM(cNumericFields(1))//' must be >= 1.'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(1),0)))
        ErrorsFound=.TRUE.
      End If

      If (VarSpeedCoil(DXCoilNum)%NormSpedLevel > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) THEN
        VarSpeedCoil(DXCoilNum)%NormSpedLevel = VarSpeedCoil(DXCoilNum)%NumOfSpeeds
      END IF

      If ((VarSpeedCoil(DXCoilNum)%NormSpedLevel > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) &
            .OR. (VarSpeedCoil(DXCoilNum)%NormSpedLevel <= 0)) Then
        CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
        CALL ShowContinueError('...'//TRIM(cNumericFields(2))//' must be valid speed level'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(2),0)))
        ErrorsFound=.TRUE.
      End If

      !part load curve
      VarSpeedCoil(DXCoilNum)%PLFFPLR = GetCurveIndex(AlphArray(4)) ! convert curve name to number
      IF (VarSpeedCoil(DXCoilNum)%PLFFPLR .EQ. 0) THEN
        IF (lAlphaBlanks(4)) THEN
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
          CALL ShowContinueError('...required '//trim(cAlphaFields(4))//' is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...not found '//TRIM(cAlphaFields(4))//'="'//TRIM(AlphArray(4))//'".')
        END IF
        ErrorsFound = .TRUE.
      ELSE
          CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%PLFFPLR,1.0d0)
          IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
            CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                  //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
            CALL ShowContinueError('...'//TRIM(cAlphaFields(4))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
            CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
          END IF
      END IF

      VarSpeedCoil(DXCoilNum)%DefrostEIRFT = GetCurveIndex(AlphArray(5)) ! convert curve name to number

      IF (SameString(AlphArray(6),'ReverseCycle')) THEN
        IF (VarSpeedCoil(DXCoilNum)%DefrostEIRFT .EQ. 0) THEN
          IF (lAlphaBlanks(5)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(5))//' is blank.')
            CALL ShowContinueError('...field is required because '//trim(cAlphaFields(6))//' is "ReverseCycle".')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(5))//'="'//TRIM(AlphArray(5))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
          ! Verify Curve Object, only legal type is BiQuadratic
          SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%DefrostEIRFT))

            CASE('BIQUADRATIC')

            CASE DEFAULT
              CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
              CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(5))//' type for this object = '// &
                                 TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%DefrostEIRFT)))
              CALL ShowContinueError('Curve type must be BiQuadratic.')
              ErrorsFound=.TRUE.
          END SELECT
        END IF
     END IF

    IF (SameString(AlphArray(6),'ReverseCycle'))  VarSpeedCoil(DXCoilNum)%DefrostStrategy = ReverseCycle
    IF (SameString(AlphArray(6),'Resistive')) VarSpeedCoil(DXCoilNum)%DefrostStrategy = Resistive
    IF (VarSpeedCoil(DXCoilNum)%DefrostStrategy .EQ.0) THEN
       CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
       CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(6))//'="'//TRIM(AlphArray(6))//'".')
       CALL ShowContinueError('...valid values for this field are ReverseCycle or Resistive.')
       ErrorsFound = .TRUE.
    END IF

    IF (SameString(AlphArray(7),'Timed'))     VarSpeedCoil(DXCoilNum)%DefrostControl = Timed
    IF (SameString(AlphArray(7),'OnDemand'))  VarSpeedCoil(DXCoilNum)%DefrostControl = OnDemand
    IF (VarSpeedCoil(DXCoilNum)%DefrostControl .EQ.0) THEN
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
      CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(7))//'="'//TRIM(AlphArray(7))//'".')
      CALL ShowContinueError('...valid values for this field are Timed or OnDemand.')
      ErrorsFound = .TRUE.
    END IF

  !Set minimum OAT for heat pump compressor operation
   VarSpeedCoil(DXCoilNum)%MinOATCompressor = NumArray(5)

   ! reserved for HSPF calculation
   VarSpeedCoil(DXCoilNum)%OATempCompressorOn = NumArray(6)

   !Set maximum outdoor temp for defrost to occur
   VarSpeedCoil(DXCoilNum)%MaxOATDefrost = NumArray(7)

  !Set crankcase heater capacity
   VarSpeedCoil(DXCoilNum)%CrankcaseHeaterCapacity = NumArray(8)
   IF (VarSpeedCoil(DXCoilNum)%CrankcaseHeaterCapacity .LT. 0.0d0) THEN
     CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
     CALL ShowContinueError('...'//TRIM(cNumericFields(9))//' cannot be < 0.0.')
     CALL ShowContinueError('...entered value=['//trim(TrimSigDigits(NumArray(9),2))//'].')
     ErrorsFound = .TRUE.
   END IF

   !Set crankcase heater cutout temperature
   VarSpeedCoil(DXCoilNum)%MaxOATCrankcaseHeater = NumArray(9)

   !Set defrost time period
   VarSpeedCoil(DXCoilNum)%DefrostTime = NumArray(10)
   IF(VarSpeedCoil(DXCoilNum)%DefrostTime .EQ. 0.0d0 .AND. VarSpeedCoil(DXCoilNum)%DefrostControl .EQ. 1) THEN
     CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", ')
     CALL ShowContinueError('...'//TRIM(cNumericFields(5))//' = 0.0 for defrost control = TIMED.')
   END IF

  !Set defrost capacity (for resistive defrost)
   VarSpeedCoil(DXCoilNum)%DefrostCapacity = NumArray(11)
   IF(VarSpeedCoil(DXCoilNum)%DefrostCapacity .EQ. 0.0d0 .AND. VarSpeedCoil(DXCoilNum)%DefrostStrategy .EQ. 2) THEN
     CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", ')
     CALL ShowContinueError('...'//TRIM(cNumericFields(6))//' = 0.0 for defrost strategy = RESISTIVE.')
   END IF

   Do I=1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds
        VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I) = NumArray(12+(I-1)*3)
        VarSpeedCoil(DXCoilNum)%MSRatedCOP(I)    = NumArray(13+(I-1)*3)
        VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(I) = NumArray(14+(I-1)*3)

        IF (VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I) < 1.d-10) THEN
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid value')
          CALL ShowContinueError('...too small '//trim(cNumericFields(12+(I-1)*3))//'=['//  &
            trim(RoundSigDigits(VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I),2))//'].')
          ErrorsFound=.true.
        ENDIF

        AlfaFieldIncre = 8+(I-1)*4
        VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I) .EQ. 0) THEN
          IF (lAlphaBlanks(AlfaFieldIncre)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
          ! Verify Curve Object, only legal type is BiQuadratic
          SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I)))

          CASE('BIQUADRATIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I),RatedInletAirTempHeat,RatedAmbAirTempHeat)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                    //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                 TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(I))))
            CALL ShowContinueError('Curve type must be BiQuadratic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF

        AlfaFieldIncre = 9+(I-1)*4
        VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I) .EQ. 0) THEN
          IF (lAlphaBlanks(AlfaFieldIncre)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))//'="' &
                    //TRIM(AlphArray(AlfaFieldIncre))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
          ! Verify Curve Object, only legal type is Quadratic
          SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I)))

          CASE('QUADRATIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                    //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE('CUBIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                    //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                 TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(I))))
            CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF

        AlfaFieldIncre = 10+(I-1)*4
        VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I) .EQ. 0) THEN
          IF (lAlphaBlanks(AlfaFieldIncre)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                    //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
          ! Verify Curve Object, only legal type is Biquadratic
          SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I)))

          CASE('BIQUADRATIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(I),RatedInletAirTempHeat,RatedAmbAirTempHeat)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                    //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                 TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(1))))
            CALL ShowContinueError('Curve type must be BiQuadratic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF

        AlfaFieldIncre = 11+(I-1)*4
        VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
        IF (VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I) .EQ. 0) THEN
          IF (lAlphaBlanks(AlfaFieldIncre)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))//'="'//TRIM(AlphArray(16+(I-1)*6))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
          ! Verify Curve Object, only legal type is Quadratic
          SELECT CASE(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I)))

          CASE('QUADRATIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                    //trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE('CUBIC')
            CurveVal = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject) &
                    //'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(VarSpeedCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                  TRIM(GetCurveType(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(I))))
            CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
            ErrorsFound=.TRUE.
          END SELECT
        END IF
    END DO

    IF (ErrorsFound) CYCLE

    Do I=1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds
        VarSpeedCoil(DXCoilNum)%MSRatedPercentTotCap(I) =VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)/ &
                VarSpeedCoil(DXCoilNum)%MSRatedTotCap(VarSpeedCoil(DXCoilNum)%NumOfSpeeds)
        VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(I) = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(I)/ &
                    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(I)
    END DO

    CALL SetupOutputVariable('Heating Coil Electric Energy [J]', &
         VarSpeedCoil(DXCoilNum)%Energy,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
         ResourceTypeKey='Electric',EndUseKey='Heating',GroupKey='System')

    CALL SetupOutputVariable('Heating Coil Heating Energy [J]', &
         VarSpeedCoil(DXCoilNum)%EnergyLoadTotal,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
         ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')

    CALL SetupOutputVariable('Heating Coil Source Side Heat Transfer Energy [J]', &
         VarSpeedCoil(DXCoilNum)%EnergySource,'System','Summed',VarSpeedCoil(DXCoilNum)%Name,  &
         ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='HEATINGCOILS',GroupKey='System')

    !create predefined report entries
    CALL PreDefTableEntry(pdchHeatCoilType,VarSpeedCoil(DXCoilNum)%Name,CurrentModuleObject)
    CALL PreDefTableEntry(pdchHeatCoilNomCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapHeat)
    CALL PreDefTableEntry(pdchHeatCoilNomEff,VarSpeedCoil(DXCoilNum)%Name,&
                VarSpeedCoil(DXCoilNum)%MSRatedCOP(VarSpeedCoil(DXCoilNum)%NormSpedLevel))

  END DO

   !-------------------------AIR SOURCE HEATING---END

  DEALLOCATE(AlphArray)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lNumericBlanks)
  DEALLOCATE(NumArray)

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found getting input. Program terminates.')
  ENDIF

  DO DXCoilNum=1,NumWatertoAirHPs
    IF((VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) .OR. &
        (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed)) THEN
        ! Setup Report variables for the Heat Pump

      !cooling and heating coils separately
      IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
       ! air source cooling coils
        CALL SetupOutputVariable('Cooling Coil Air Mass Flow Rate [kg/s]', &
              VarSpeedCoil(DXCoilNum)%AirMassFlowRate,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Air Inlet Temperature [C]', &
              VarSpeedCoil(DXCoilNum)%InletAirDBTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]', &
              VarSpeedCoil(DXCoilNum)%InletAirHumRat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Latent Cooling Rate [W]', &
              VarSpeedCoil(DXCoilNum)%QLatent,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Air Outlet Temperature [C]', &
              VarSpeedCoil(DXCoilNum)%OutletAirDBTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]', &
              VarSpeedCoil(DXCoilNum)%OutletAirHumRat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Sensible Cooling Rate [W]', &
              VarSpeedCoil(DXCoilNum)%QSensible,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Total Cooling Rate [W]', &
              VarSpeedCoil(DXCoilNum)%QLoadTotal,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Part Load Ratio []', &
              VarSpeedCoil(DXCoilNum)%PartLoadRatio,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Electric Power [W]', &
              VarSpeedCoil(DXCoilNum)%Power,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Runtime Fraction []', &
              VarSpeedCoil(DXCoilNum)%RunFrac,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Rate [W]', &
              VarSpeedCoil(DXCoilNum)%QSource,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Upper Speed Level []', &
              VarSpeedCoil(DXCoilNum)%SpeedNumReport,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Neighboring Speed Levels Ratio []', &
              VarSpeedCoil(DXCoilNum)%SpeedRatioReport,'System','Average',VarSpeedCoil(DXCoilNum)%Name)

        IF (VarSpeedCoil(DXCoilNum)%CondensateCollectMode == CondensateToTank) THEN
          CALL SetupOutputVariable('Cooling Coil Condensate Volume Flow Rate [m3/s]', &
              VarSpeedCoil(DXCoilNum)%CondensateVdot, 'System','Average', VarSpeedCoil(DXCoilNum)%Name)
          CALL SetupOutputVariable('Cooling Coil Condensate Volume [m3]',VarSpeedCoil(DXCoilNum)%CondensateVol,&
                                 'System','Sum', VarSpeedCoil(DXCoilNum)%Name,  &
                                 ResourceTypeKey='OnSiteWater', &
                                 EndUseKey='Condensate', GroupKey='System')
        ENDIF

        IF (VarSpeedCoil(DXCoilNum)%ReportEvapCondVars) THEN
          CALL SetupOutputVariable('Cooling Coil Condenser Inlet Temperature [C]', &
                                      VarSpeedCoil(DXCoilNum)%CondInletTemp,'System','Average', VarSpeedCoil(DXCoilNum)%Name)
          CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Water Volume [m3]',&
                            VarSpeedCoil(DXCoilNum)%EvapWaterConsump, &
                                     'System','Sum',VarSpeedCoil(DXCoilNum)%Name, &
                                      ResourceTypeKey='Water',EndUseKey='Cooling',GroupKey='System')
          CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Mains Water Volume [m3]', &
                                        VarSpeedCoil(DXCoilNum)%EvapWaterConsump, &
                                     'System','Sum',VarSpeedCoil(DXCoilNum)%Name, &
                                      ResourceTypeKey='MainsWater',EndUseKey='Cooling',GroupKey='System')
          CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Pump Electric Power [W]',&
                                        VarSpeedCoil(DXCoilNum)%EvapCondPumpElecPower, &
                                     'System','Average',VarSpeedCoil(DXCoilNum)%Name)
          CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Pump Electric Energy [J]', &
                                      VarSpeedCoil(DXCoilNum)%EvapCondPumpElecConsumption,'System','Sum',&
                                      VarSpeedCoil(DXCoilNum)%Name, &
                                      ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
          IF(VarSpeedCoil(DXCoilNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
            CALL SetupOutputVariable('Cooling Coil Basin Heater Electric Power [W]', &
                VarSpeedCoil(DXCoilNum)%BasinHeaterPower,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
            CALL SetupOutputVariable('Cooling Coil Basin Heater Electric Energy [J]', &
                VarSpeedCoil(DXCoilNum)%BasinHeaterConsumption,'System','Sum',VarSpeedCoil(DXCoilNum)%Name, &
                ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
          END IF
        END IF

        CALL SetupOutputVariable('Cooling Coil Crankcase Heater Electric Power [W]', &
                            VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower,'System',&
                           'Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Crankcase Heater Electric Energy [J]', &
                                    VarSpeedCoil(DXCoilNum)%CrankcaseHeaterConsumption,  &
                                    'System','Sum',VarSpeedCoil(DXCoilNum)%Name, &
                                    ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')
      ELSE
      ! air source heating coils
        CALL SetupOutputVariable('Heating Coil Air Mass Flow Rate [kg/s]', &
              VarSpeedCoil(DXCoilNum)%AirMassFlowRate,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Air Inlet Temperature [C]', &
              VarSpeedCoil(DXCoilNum)%InletAirDBTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]', &
              VarSpeedCoil(DXCoilNum)%InletAirHumRat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Air Outlet Temperature [C]', &
              VarSpeedCoil(DXCoilNum)%OutletAirDBTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]', &
              VarSpeedCoil(DXCoilNum)%OutletAirHumRat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Sensible Heating Rate [W]', &
              VarSpeedCoil(DXCoilNum)%QSensible,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Heating Rate [W]', &
              VarSpeedCoil(DXCoilNum)%QLoadTotal,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Part Load Ratio []', &
              VarSpeedCoil(DXCoilNum)%PartLoadRatio,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Electric Power [W]', &
              VarSpeedCoil(DXCoilNum)%Power,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Runtime Fraction []', &
              VarSpeedCoil(DXCoilNum)%RunFrac,'System','Average',VarSpeedCoil(DXCoilNum)%Name)

        CALL SetupOutputVariable('Heating Coil Source Side Heat Transfer Rate [W]', &
              VarSpeedCoil(DXCoilNum)%QSource,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Upper Speed Level []', &
              VarSpeedCoil(DXCoilNum)%SpeedNumReport,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Neighboring Speed Levels Ratio []', &
              VarSpeedCoil(DXCoilNum)%SpeedRatioReport,'System','Average',VarSpeedCoil(DXCoilNum)%Name)

        CALL SetupOutputVariable('Heating Coil Defrost Electric Power [W]', &
                              VarSpeedCoil(DXCoilNum)%DefrostPower,'System','Average',&
                              VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Defrost Electric Energy [J]', &
                              VarSpeedCoil(DXCoilNum)%DefrostConsumption,'System',&
                             'Sum',VarSpeedCoil(DXCoilNum)%Name, &
                             ResourceTypeKey='Electric',EndUseKey='HEATING',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Crankcase Heater Electric Power [W]', &
                              VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower,'System',&
                             'Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Crankcase Heater Electric Energy [J]', &
                                      VarSpeedCoil(DXCoilNum)%CrankcaseHeaterConsumption,  &
                                      'System','Sum',VarSpeedCoil(DXCoilNum)%Name, &
                                      ResourceTypeKey='Electric',EndUseKey='HEATING',GroupKey='System')

      END IF
    ELSE

      IF (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum == TypeOf_CoilVSWAHPCoolingEquationFit) THEN
       ! cooling WAHP coil
            ! Setup Report variables for water source Heat Pump
        CALL SetupOutputVariable('Cooling Coil Electric Power [W]', &
            VarSpeedCoil(DXCoilNum)%Power,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Total Cooling Rate [W]', &
            VarSpeedCoil(DXCoilNum)%QLoadTotal,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Sensible Cooling Rate [W]', &
            VarSpeedCoil(DXCoilNum)%QSensible,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Latent Cooling Rate [W]', &
            VarSpeedCoil(DXCoilNum)%QLatent,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Rate [W]', &
            VarSpeedCoil(DXCoilNum)%QSource,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Part Load Ratio []', &
            VarSpeedCoil(DXCoilNum)%PartLoadRatio,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Runtime Fraction []', &
            VarSpeedCoil(DXCoilNum)%RunFrac,'System','Average',VarSpeedCoil(DXCoilNum)%Name)

        CALL SetupOutputVariable('Cooling Coil Air Mass Flow Rate [kg/s]', &
            VarSpeedCoil(DXCoilNum)%AirMassFlowRate,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Air Inlet Temperature [C]', &
            VarSpeedCoil(DXCoilNum)%InletAirDBTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]', &
            VarSpeedCoil(DXCoilNum)%InletAirHumRat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Air Outlet Temperature [C]', &
            VarSpeedCoil(DXCoilNum)%OutletAirDBTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]', &
            VarSpeedCoil(DXCoilNum)%OutletAirHumRat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Source Side Mass Flow Rate [kg/s]', &
            VarSpeedCoil(DXCoilNum)%WaterMassFlowRate,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Source Side Inlet Temperature [C]', &
            VarSpeedCoil(DXCoilNum)%InletWaterTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Source Side Outlet Temperature [C]', &
            VarSpeedCoil(DXCoilNum)%OutletWaterTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)


        CALL SetupOutputVariable('Cooling Coil Upper Speed Level []', &
            VarSpeedCoil(DXCoilNum)%SpeedNumReport,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Neighboring Speed Levels Ratio []', &
            VarSpeedCoil(DXCoilNum)%SpeedRatioReport,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Cooling Coil Recoverable Heat Transfer Rate [W]', &
            VarSpeedCoil(DXCoilNum)%QWasteHeat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
      ELSEIF (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum == TypeOf_CoilVSWAHPHeatingEquationFit) THEN
       ! heating WAHP coil
            ! Setup Report variables for water source Heat Pump
        CALL SetupOutputVariable('Heating Coil Electric Power [W]', &
            VarSpeedCoil(DXCoilNum)%Power,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Heating Rate [W]', &
            VarSpeedCoil(DXCoilNum)%QLoadTotal,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Sensible Heating Rate [W]', &
            VarSpeedCoil(DXCoilNum)%QSensible,'System','Average',VarSpeedCoil(DXCoilNum)%Name)

        CALL SetupOutputVariable('Heating Coil Source Side Heat Transfer Rate [W]', &
            VarSpeedCoil(DXCoilNum)%QSource,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Part Load Ratio []', &
            VarSpeedCoil(DXCoilNum)%PartLoadRatio,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Runtime Fraction []', &
            VarSpeedCoil(DXCoilNum)%RunFrac,'System','Average',VarSpeedCoil(DXCoilNum)%Name)

        CALL SetupOutputVariable('Heating Coil Air Mass Flow Rate [kg/s]', &
            VarSpeedCoil(DXCoilNum)%AirMassFlowRate,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Air Inlet Temperature [C]', &
            VarSpeedCoil(DXCoilNum)%InletAirDBTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]', &
            VarSpeedCoil(DXCoilNum)%InletAirHumRat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Air Outlet Temperature [C]', &
            VarSpeedCoil(DXCoilNum)%OutletAirDBTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]', &
            VarSpeedCoil(DXCoilNum)%OutletAirHumRat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Source Side Mass Flow Rate [kg/s]', &
            VarSpeedCoil(DXCoilNum)%WaterMassFlowRate,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Source Side Inlet Temperature [C]', &
            VarSpeedCoil(DXCoilNum)%InletWaterTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Source Side Outlet Temperature [C]', &
            VarSpeedCoil(DXCoilNum)%OutletWaterTemp,'System','Average',VarSpeedCoil(DXCoilNum)%Name)


        CALL SetupOutputVariable('Heating Coil Upper Speed Level []', &
            VarSpeedCoil(DXCoilNum)%SpeedNumReport,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Neighboring Speed Levels Ratio []', &
            VarSpeedCoil(DXCoilNum)%SpeedRatioReport,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Recoverable Heat Transfer Rate [W]', &
            VarSpeedCoil(DXCoilNum)%QWasteHeat,'System','Average',VarSpeedCoil(DXCoilNum)%Name)
      ENDIF
    END IF
   END DO

  IF (ErrorsFound) THEN
     CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input.  '//&
                      'Preceding condition(s) causes termination.')
  END IF

  RETURN

END SUBROUTINE GetVarSpeedCoilInput

 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitVarSpeedCoil(DXCoilNum,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime,SensLoad,LatentLoad,CyclingScheme, &
                                  OnOffAirFlowRatio, SpeedRatio, SpeedNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on  MODULE WatertoAirHeatPumpSimple:InitSimpleWatertoAirHP
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the variable speed Water to Air HP Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE Psychrometrics,       ONLY:  PsyRhoAirFnPbTdbW
  USE DataGlobals,          ONLY: SysSizingCalc
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,       ONLY : ScanPlantLoopsForObject, PlantLoop
  USE PlantUtilities,  ONLY : InitComponentNodes, SetComponentFlowRate
  USE General,         ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(IN) :: DXCoilNum                  ! Current DXCoilNum under simulation
  REAL(r64),    INTENT(IN) :: MaxONOFFCyclesperHour ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64),    INTENT(IN) :: HPTimeConstant        ! Heat pump time constant [s]
  REAL(r64),    INTENT(IN) :: FanDelayTime          ! Fan delay time, time delay for the HP's fan to
                                                    ! shut off after compressor cycle off  [s]
  REAL(r64), INTENT(IN) :: SensLoad                 ! Control zone sensible load[W]
  REAL(r64), INTENT(IN) :: LatentLoad               ! Control zone latent load[W]
  INTEGER,   INTENT(IN) :: CyclingScheme            ! fan operating mode
  REAL(r64), INTENT(IN) :: OnOffAirFlowRatio        ! ratio of compressor on flow to average flow over time step
  REAL(r64), INTENT(IN) :: SpeedRatio               ! compressor speed ratio
  INTEGER, INTENT(IN)   :: SpeedNum                 ! compressor speed number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: AirInletNode        ! Node Number of the air inlet
  INTEGER                :: WaterInletNode      ! Node Number of the Water inlet
  LOGICAL, SAVE          :: MyOneTimeFlag = .TRUE.  ! one time allocation flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! used for initializations each begin environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeFlag  ! used for sizing PTHP inputs one time
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  REAL(r64)   :: rho                                    ! local fluid density
  REAL(r64)   :: Cp                                     ! local fluid specific heat
  INTEGER :: SpeedCal                                   ! calculated speed level
  LOGICAL     :: errFlag
  LOGICAL :: ErrorsFound=.FALSE.                        ! TRUE when errors found, air loop initialization error
  REAL(r64) :: RatedVolFlowPerRatedTotCap               ! Rated Air Volume Flow Rate divided by Rated Total Capacity [m3/s-W)
  INTEGER :: Mode                                       ! Performance mode for MultiMode DX coil; Always 1 for other coil types
  REAL(r64) :: RatedHeatPumpIndoorAirTemp
  ! Indoor dry-bulb temperature to heat pump evaporator at rated conditions [C]
  REAL(r64) :: RatedHeatPumpIndoorHumRat                ! Inlet humidity ratio to heat pump evaporator at rated conditions [kg/kg]
  REAL(r64) :: WaterFlowScale                           ! water flow scaling factor match rated flow rate

            ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64) :: SmallDifferenceTest=0.00000001d0
  CHARACTER(len=*), PARAMETER :: RoutineName='InitVarSpeedCoil'


  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MySizeFlag(NumWatertoAirHPs))
    ALLOCATE(MyEnvrnFlag(NumWatertoAirHPs))
    ALLOCATE(MyPlantScanFlag(NumWatertoAirHPs))
    MySizeFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyPlantScanFlag = .TRUE.
    MyOneTimeFlag = .FALSE.

  END IF

! water source
  IF( (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit).OR. &
    (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit)) THEN
      IF (MyPlantScanFlag(DXCoilNum) .AND. ALLOCATED(PlantLoop)) THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject(VarSpeedCoil(DXCoilNum)%Name,  &
                                     VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum, &
                                     VarSpeedCoil(DXCoilNum)%LoopNum, &
                                     VarSpeedCoil(DXCoilNum)%LoopSide, &
                                     VarSpeedCoil(DXCoilNum)%BranchNum, &
                                     VarSpeedCoil(DXCoilNum)%CompNum,   &
                                     errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitVarSpeedCoil: Program terminated for previous conditions.')
        ENDIF
        MyPlantScanFlag(DXCoilNum) = .FALSE.
      ENDIF
  ELSE
    MyPlantScanFlag(DXCoilNum) = .FALSE.
  END IF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(DXCoilNum) .AND. .NOT. MyPlantScanFlag(DXCoilNum) ) THEN
    ! for each furnace, do the sizing once.
    CALL SizeVarSpeedCoil(DXCoilNum)

    MySizeFlag(DXCoilNum) = .FALSE.

    ! Multispeed Cooling
    IF ((VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit ) .OR. &
        (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==Coil_CoolingAirToAirVariableSpeed))THEN
      Do Mode = 1, VarSpeedCoil(DXCoilNum)%NumOfSpeeds
        ! Check for zero capacity or zero max flow rate
        IF (VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode) <= 0.0d0) THEN
          CALL ShowSevereError('Sizing: '//TRIM(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType)//' '//  &
             TRIM(VarSpeedCoil(DXCoilNum)%Name)// &
                            ' has zero rated total capacity at speed '//Trim(TrimSigDigits(Mode)))
          ErrorsFound=.TRUE.
        END IF
        IF (VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode) <= 0.0d0) THEN
          CALL ShowSevereError('Sizing: '//TRIM(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType)//' '//  &
             TRIM(VarSpeedCoil(DXCoilNum)%Name)// &
                            ' has zero rated air flow rate at speed '//Trim(TrimSigDigits(Mode)))
          ErrorsFound=.TRUE.
        END IF
        IF (ErrorsFound) THEN
          CALL ShowFatalError('Preceding condition causes termination.')
        ENDIF
        !
        ! Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
        !
        RatedVolFlowPerRatedTotCap = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)/  &
                                                 VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode)
       !note: variable-speed HP can exceed the flow rate restrictions at low speed levels
!        IF (((MinRatedAirVolFlowPerRatedTotCap - RatedVolFlowPerRatedTotCap) > SmallDifferenceTest).OR. &
!           ((RatedVolFlowPerRatedTotCap - MaxRatedAirVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
!          CALL ShowSevereError ('Sizing: '//TRIM(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType) &
!           // ' "'//TRIM(VarSpeedCoil(DXCoilNum)%Name)//  &
!                '": Rated air volume flow rate per watt of rated total '// &
!                'cooling capacity is out of range at speed '//TRIM(TrimSigDigits(Mode)))
!          CALL ShowContinueError &
!           ('Min Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(MinRatedAirVolFlowPerRatedTotCap,3))//'], '// &
!           'Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(RatedVolFlowPerRatedTotCap,3))//'],  &
!           Max Rated Vol Flow Per Watt=['// &
!           TRIM(TrimSigDigits(MaxRatedAirVolFlowPerRatedTotCap,3))//']. See Input-Output Reference Manual for valid range.')
!        END IF
!        VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode) = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)* &
!          PsyRhoAirFnPbTdbW(OutBaroPress,RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
!        ! get high speed rated coil bypass factor
!        VarSpeedCoil(DXCoilNum)%MSRatedCBF(Mode) = CalcCBF(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType, &
!               VarSpeedCoil(DXCoilNum)%Name,&
!                                           RatedInletAirTemp,RatedInletAirHumRat,VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode),&
!                                           VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode), &
!                           VarSpeedCoil(DXCoilNum)%MSRatedSHR(Mode))
      END DO
    END IF

    ! Multispeed Heating
    IF ((VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit) .OR. &
        (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==Coil_HeatingAirToAirVariableSpeed))THEN
      RatedHeatPumpIndoorAirTemp = 21.11d0  ! 21.11C or 70F
      RatedHeatPumpIndoorHumRat = 0.00881d0 ! Humidity ratio corresponding to 70F dry bulb/60F wet bulb
      Do Mode = 1, VarSpeedCoil(DXCoilNum)%NumOfSpeeds

        VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode) = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)* &
         PsyRhoAirFnPbTdbW(OutBaroPress,RatedHeatPumpIndoorAirTemp,RatedHeatPumpIndoorHumRat,RoutineName)
        ! Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
        !
        RatedVolFlowPerRatedTotCap = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)/  &
                                                  VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode)
        !note: variable-speed HP can exceed the flow rate restrictions at low speed levels
!        IF (((MinRatedAirVolFlowPerRatedTotCap - RatedVolFlowPerRatedTotCap) > SmallDifferenceTest).OR. &
!            ((RatedVolFlowperRatedTotCap - MaxRatedAirVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
!          CALL ShowSevereError ('Coil:Heating:DX:MultiSpeed '//TRIM(VarSpeedCoil(DXCoilNum)%Name)//  &
!                              ': Rated air volume flow rate per watt of rated total '// &
!                'heating capacity is out of range at speed '//TRIM(TrimSigDigits(Mode)))
!          CALL ShowContinueError('Min Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits &
!           (MinRatedAirVolFlowPerRatedTotCap,3))//'], '// &
!           'Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(RatedVolFlowPerRatedTotCap,3))//'],  &
!               Max Rated Vol Flow Per Watt=['// &
!           TRIM(TrimSigDigits(MaxRatedAirVolFlowPerRatedTotCap,3))//']. See Input-Output Reference  &
!                Manual for valid range.')
!        END IF
      End Do
    END IF

  END IF

  IF(SpeedNum > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) THEN
      SpeedCal = VarSpeedCoil(DXCoilNum)%NumOfSpeeds
  ELSE IF(SpeedNum < 1) THEN
     SpeedCal = 1
  ELSE
     SpeedCal = SpeedNum
  END IF

  IF((SpeedNum <= 1) .OR.(SpeedNum > VarSpeedCoil(DXCoilNum)%NumOfSpeeds) ) THEN
    VarSpeedCoil(DXCoilNum)%DesignAirMassFlowRate = VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(SpeedCal)
    VarSpeedCoil(DXCoilNum)%DesignAirVolFlowRate = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(SpeedCal)
    VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate = VarSpeedCoil(DXCoilNum)%MSRatedWaterMassFlowRate(SpeedCal)
    VarSpeedCoil(DXCoilNum)%DesignWaterVolFlowRate = VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(SpeedCal)
  ELSE
    VarSpeedCoil(DXCoilNum)%DesignAirMassFlowRate = VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(SpeedCal)*SpeedRatio &
            + (1.0d0 - SpeedRatio ) * VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(SpeedCal - 1)
    VarSpeedCoil(DXCoilNum)%DesignAirVolFlowRate = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(SpeedCal)*SpeedRatio &
            + (1.0d0 - SpeedRatio ) * VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(SpeedCal - 1)
    VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate = VarSpeedCoil(DXCoilNum)%MSRatedWaterMassFlowRate(SpeedCal)*SpeedRatio &
            + (1.0d0 - SpeedRatio ) * VarSpeedCoil(DXCoilNum)%MSRatedWaterMassFlowRate(SpeedCal - 1)
    VarSpeedCoil(DXCoilNum)%DesignWaterVolFlowRate = VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(SpeedCal)*SpeedRatio &
            + (1.0d0 - SpeedRatio ) * VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(SpeedCal - 1)
  END IF


  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(DXCoilNum) .AND. .NOT. MyPlantScanFlag(DXCoilNum)) THEN
      ! Do the initializations to start simulation

    AirInletNode   = VarSpeedCoil(DXCoilNum)%AirInletNodeNum

    !Initialize all report variables to a known state at beginning of simulation
    VarSpeedCoil(DXCoilNum)%AirVolFlowRate=0.0d0
    VarSpeedCoil(DXCoilNum)%InletAirDBTemp=0.0d0
    VarSpeedCoil(DXCoilNum)%InletAirHumRat=0.0d0
    VarSpeedCoil(DXCoilNum)%OutletAirDBTemp=0.0d0
    VarSpeedCoil(DXCoilNum)%OutletAirHumRat=0.0d0
    VarSpeedCoil(DXCoilNum)%WaterVolFlowRate=0.0d0
    VarSpeedCoil(DXCoilNum)%WaterMassFlowRate=0.0d0
    VarSpeedCoil(DXCoilNum)%InletWaterTemp=0.0d0
    VarSpeedCoil(DXCoilNum)%InletWaterEnthalpy = 0.0d0
    VarSpeedCoil(DXCoilNum)%OutletWaterEnthalpy = 0.0d0
    VarSpeedCoil(DXCoilNum)%OutletWaterTemp=0.0d0
    VarSpeedCoil(DXCoilNum)%Power=0.0d0
    VarSpeedCoil(DXCoilNum)%QLoadTotal=0.0d0
    VarSpeedCoil(DXCoilNum)%QSensible=0.0d0
    VarSpeedCoil(DXCoilNum)%QLatent=0.0d0
    VarSpeedCoil(DXCoilNum)%QSource=0.0d0
    VarSpeedCoil(DXCoilNum)%Energy=0.0d0
    VarSpeedCoil(DXCoilNum)%EnergyLoadTotal=0.0d0
    VarSpeedCoil(DXCoilNum)%EnergySensible=0.0d0
    VarSpeedCoil(DXCoilNum)%EnergyLatent=0.0d0
    VarSpeedCoil(DXCoilNum)%EnergySource=0.0d0
    VarSpeedCoil(DXCoilNum)%COP=0.0d0
    VarSpeedCoil(DXCoilNum)%RunFrac=0.0d0
    VarSpeedCoil(DXCoilNum)%PartLoadRatio=0.0d0

    VarSpeedCoil(DXCoilNum)%MaxONOFFCyclesperHour=MaxONOFFCyclesperHour
    VarSpeedCoil(DXCoilNum)%HPTimeConstant=HPTimeConstant
    VarSpeedCoil(DXCoilNum)%FanDelayTime=FanDelayTime


    IF ((VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit) .OR. &
        (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit))THEN
        WaterInletNode = VarSpeedCoil(DXCoilNum)%WaterInletNodeNum

        rho = GetDensityGlycol(PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidName, &
                                  InitConvTemp, &
                                  PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                                  'InitSimpleWatertoAirHP')
        Cp  = GetSpecificHeatGlycol(PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidName, &
                                  InitConvTemp, &
                                  PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                                  'InitSimpleWatertoAirHP')

    !    VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate= &
    !                             rho * VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate

        CALL InitComponentNodes(0.d0, VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate, &
                                      VarSpeedCoil(DXCoilNum)%WaterInletNodeNum,  &
                                      VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum , &
                                      VarSpeedCoil(DXCoilNum)%LoopNum, &
                                      VarSpeedCoil(DXCoilNum)%LoopSide, &
                                      VarSpeedCoil(DXCoilNum)%BranchNum, &
                                      VarSpeedCoil(DXCoilNum)%CompNum )

        Node(WaterInletNode)%Temp          = 5.0d0
        Node(WaterInletNode)%Enthalpy      = Cp* Node(WaterInletNode)%Temp
        Node(WaterInletNode)%Quality       = 0.0d0
        Node(WaterInletNode)%Press         = 0.0d0
        Node(WaterInletNode)%HumRat        = 0.0d0

        Node(VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum)%Temp          = 5.0d0
        Node(VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum)%Enthalpy      = Cp* Node(WaterInletNode)%Temp
        Node(VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum)%Quality       = 0.0d0
        Node(VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum)%Press         = 0.0d0
        Node(VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum)%HumRat        = 0.0d0
    END IF

    VarSpeedCoil(DXCoilNum)%SimFlag = .TRUE.

    MyEnvrnFlag(DXCoilNum) = .FALSE.

  END IF  ! End If for the Begin Environment initializations

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(DXCoilNum)=.TRUE.
  ENDIF

  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.
  ! First set the conditions for the air into the heat pump model

    ! Set water and air inlet nodes

    AirInletNode = VarSpeedCoil(DXCoilNum)%AirInletNodeNum
    WaterInletNode = VarSpeedCoil(DXCoilNum)%WaterInletNodeNum

  IF ((SensLoad .NE. 0.0d0 .OR. LatentLoad .NE. 0.0d0).AND.(Node(AirInletNode)%MassFlowRate > 0.0d0)) THEN

    IF(VarSpeedCoil(DXCoilNum)%MSRatedWaterMassFlowRate(VarSpeedCoil(DXCoilNum)%NormSpedLevel) > 0.0d0) THEN
        WaterFlowScale = VarSpeedCoil(DXCoilNum)%RatedWaterMassFlowRate/ &
        VarSpeedCoil(DXCoilNum)%MSRatedWaterMassFlowRate(VarSpeedCoil(DXCoilNum)%NormSpedLevel)
        VarSpeedCoil(DXCoilNum)%WaterMassFlowRate =   VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate*WaterFlowScale
    ELSE
        VarSpeedCoil(DXCoilNum)%WaterMassFlowRate = 0.0d0
    END IF

    IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
       VarSpeedCoil(DXCoilNum)%AirMassFlowRate   = Node(AirInletNode)%MassFlowRate
!    VarSpeedCoil(DXCoilNum)%AirMassFlowRate   = VarSpeedCoil(DXCoilNum)%DesignAirVolFlowRate*  &
!             PsyRhoAirFnPbTdbW(OutBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)
        !If air flow is less than 25% rated flow. Then set air flow to the 25% of rated conditions
        IF(VarSpeedCoil(DXCoilNum)%AirMassFlowRate.LT.  &
             0.25d0*VarSpeedCoil(DXCoilNum)%DesignAirVolFlowRate*  &
                 PsyRhoAirFnPbTdbW(OutBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)) THEN
            VarSpeedCoil(DXCoilNum)%AirMassFlowRate =   &
              0.25d0*VarSpeedCoil(DXCoilNum)%DesignAirVolFlowRate* &
                  PsyRhoAirFnPbTdbW(OutBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)
        END IF
    ELSE !CYCLIC FAN, NOT CORRECTION, WILL BE PROCESSED IN THE FOLLOWING SUBROUTINES
      VarSpeedCoil(DXCoilNum)%AirMassFlowRate   = Node(AirInletNode)%MassFlowRate
    END IF

  ELSE !heat pump is off
    VarSpeedCoil(DXCoilNum)%WaterMassFlowRate = 0.d0
    VarSpeedCoil(DXCoilNum)%AirMassFlowRate   = 0.d0
  ENDIF

  IF ((VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit) .OR. &
        (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit))THEN
      CALL SetComponentFlowRate(VarSpeedCoil(DXCoilNum)%WaterMassFlowRate, &
                                     VarSpeedCoil(DXCoilNum)%WaterInletNodeNum , &
                                     VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum, &
                                     VarSpeedCoil(DXCoilNum)%LoopNum, &
                                     VarSpeedCoil(DXCoilNum)%LoopSide, &
                                     VarSpeedCoil(DXCoilNum)%BranchNum, &
                                     VarSpeedCoil(DXCoilNum)%CompNum )

       VarSpeedCoil(DXCoilNum)%InletWaterTemp       = Node(WaterInletNode)%Temp
       VarSpeedCoil(DXCoilNum)%InletWaterEnthalpy   = Node(WaterInletNode)%Enthalpy
   ELSE
       VarSpeedCoil(DXCoilNum)%InletWaterTemp       = 0.0d0
       VarSpeedCoil(DXCoilNum)%InletWaterEnthalpy   = 0.0d0
   END IF

   VarSpeedCoil(DXCoilNum)%InletAirDBTemp       = Node(AirInletNode)%Temp
   VarSpeedCoil(DXCoilNum)%InletAirHumRat       = Node(AirInletNode)%HumRat
   VarSpeedCoil(DXCoilNum)%InletAirEnthalpy     = Node(AirInletNode)%Enthalpy

   VarSpeedCoil(DXCoilNum)%MaxONOFFCyclesperHour= MaxONOFFCyclesperHour
   VarSpeedCoil(DXCoilNum)%HPTimeConstant       = HPTimeConstant
   VarSpeedCoil(DXCoilNum)%FanDelayTime         = FanDelayTime

   VarSpeedCoil(DXCoilNum)%InletAirPressure    = OutBaroPress !temporary
   ! Outlet variables
   VarSpeedCoil(DXCoilNum)%Power=0.0d0
   VarSpeedCoil(DXCoilNum)%QLoadTotal=0.0d0
   VarSpeedCoil(DXCoilNum)%QSensible=0.0d0
   VarSpeedCoil(DXCoilNum)%QLatent=0.0d0
   VarSpeedCoil(DXCoilNum)%QSource=0.0d0
   VarSpeedCoil(DXCoilNum)%QWasteHeat = 0.0d0
   VarSpeedCoil(DXCoilNum)%Energy=0.0d0
   VarSpeedCoil(DXCoilNum)%EnergyLoadTotal=0.0d0
   VarSpeedCoil(DXCoilNum)%EnergySensible=0.0d0
   VarSpeedCoil(DXCoilNum)%EnergyLatent=0.0d0
   VarSpeedCoil(DXCoilNum)%EnergySource=0.0d0
   VarSpeedCoil(DXCoilNum)%COP=0.0d0

   VarSpeedCoil(DXCoilNum)%OutletAirDBTemp=0.0d0
   VarSpeedCoil(DXCoilNum)%OutletWaterTemp=0.0d0
   VarSpeedCoil(DXCoilNum)%OutletAirHumRat=0.0d0
   VarSpeedCoil(DXCoilNum)%OutletAirEnthalpy = 0.0d0
   VarSpeedCoil(DXCoilNum)%OutletWaterEnthalpy = 0.0d0

   ! bug fix, must set zeros to the variables below, otherwise can't pass switch DD test
   VarSpeedCoil(DXCoilNum)%CrankcaseHeaterConsumption = 0.0d0
   VarSpeedCoil(DXCoilNum)%EvapWaterConsump = 0.0d0
   VarSpeedCoil(DXCoilNum)%BasinHeaterConsumption = 0.0d0
   VarSpeedCoil(DXCoilNum)%EvapCondPumpElecConsumption = 0.0d0
   VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower = 0.0d0
   VarSpeedCoil(DXCoilNum)%DefrostConsumption = 0.0d0
   VarSpeedCoil(DXCoilNum)%CondensateVdot = 0.0d0
   VarSpeedCoil(DXCoilNum)%CondensateVol  = 0.0d0
   VarSpeedCoil(DXCoilNum)%QWasteHeat = 0.0d0

  RETURN

END SUBROUTINE InitVarSpeedCoil

SUBROUTINE SizeVarSpeedCoil(DXCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:SizeHVACWaterToAir
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing WSHP Components for which nominal capacities
          ! and flow rates have not been specified in the input

          ! METHODOLOGY EMPLOYED:
          ! Obtains heating capacities and flow rates from the zone or system sizing arrays.
          !
          ! NOTE: For WSHP's we are sizing the heating capacity to be
          ! equal to the cooling capacity.  Thus the cooling and
          ! and heating capacities of a DX heat pump system will be identical. In real life the ARI
          ! heating and cooling capacities are close but not identical.



          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics
  USE DataPlant,          ONLY: PlantLoop, MyPlantSizingIndex
  USE DataHVACGlobals,    ONLY: SmallAirVolFlow, SmallLoad
  USE General,            ONLY: RoundSigDigits, TrimSigDigits
  USE PlantUtilities,     ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataAirSystems,     ONLY: PrimaryAirSystem
  USE OutputReportPredefined
  USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE CurveManager, ONLY: CurveValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: DXCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  RoutineName='SizeVarSpeedCoil'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: rhoair
  REAL(r64) :: CpAir
  REAL(r64) :: MixTemp
  REAL(r64) :: MixHumRat
  REAL(r64) :: MixEnth
  REAL(r64) :: MixWetBulb
  REAL(r64) :: SupTemp
  REAL(r64) :: SupHumRat
  REAL(r64) :: SupEnth
  REAL(r64) :: OutTemp
  REAL(r64) :: OutAirFrac
  REAL(r64) :: VolFlowRate
  REAL(r64) :: CoolCapAtPeak
  REAL(r64) :: TotCapTempModFac
  REAL(r64) :: SensCapAtPeak
  INTEGER   :: TimeStepNumAtMax
  INTEGER   :: DDNum
  INTEGER   :: PltSizNum
  LOGICAL   :: RatedCapCoolTotalAutosized
  LOGICAL   :: RatedCapCoolSensAutosized
  LOGICAL   :: RatedCapHeatAutosized
  LOGICAL   :: RatedAirFlowAutosized
  LOGICAL   :: RatedWaterFlowAutosized
  LOGICAL   :: ErrorsFound
  REAL(r64) :: SystemCapacity
  REAL(r64) :: rho
  REAL(r64) :: cp
  INTEGER   :: NormSpeed !norminal speed level
  INTEGER   :: UpperSpeed !highest speed level
  INTEGER   :: Mode !speed level
  REAL(r64) :: rhoW !water density
  REAL(r64) :: rhoA !air density
  REAL(r64) :: SHR  !sensible heat transfer ratio
  REAL(r64) :: RatedAirMassFlowRate  !rated air mass flow rate
  REAL(r64) :: CBFRated  !bypass factor at the rated condition, considering difference in flow rates
  REAL(r64) :: RatedInletEnth !rated inlet air enthalpy
  REAL(r64) :: QLoadTotal1 !placeholder for calculating SHR
  REAL(r64) :: QLoadTotal2 !placeholder for calculating SHR
  REAL(r64) :: QLoadTotal  !placeholder for calculating SHR
  REAL(r64) :: AirMassFlowRatio !air mass flow ratio
  REAL(r64) :: WaterMassFlowRatio !water mass flow rate
  REAL(r64) :: RatedSourceTempCool !rated source temperature, space cooling mode
  CHARACTER(len=MaxNameLength) :: CurrentObjSubfix   ! Object subfix type for printing
  LOGICAL   :: IsAutosize                   ! Indicator to autosize
  LOGICAL   :: HardSizeNoDesRun             ! Indicator to hardsize withouth sizing runs
  LOGICAL   :: HardSizeNoDesRunAirFlow      ! Indicator to hardsize withouth sizing runs for rated air flow field
  REAL(r64) :: RatedAirVolFlowRateDes       ! Autosized rated air flow for reporting
  REAL(r64) :: RatedAirVolFlowRateUser      ! Hardsized rated air flow for reporting
  REAL(r64) :: RatedCapCoolTotalDes         ! Autosized rated total cooling capacity for reporting
  REAL(r64) :: RatedCapCoolTotalUser        ! Hardsized rated total cooling capacity for reporting
  REAL(r64) :: RatedCapHeatDes              ! Autosized rated heating capacity for reporting
  REAL(r64) :: RatedCapHeatUser             ! Hardsized rated heating capacity for reporting
  REAL(r64) :: RatedWaterVolFlowRateDes     ! Autosized rated water flow for reporting
  REAL(r64) :: RatedWaterVolFlowRateUser    ! Hardsized rated water flow for reporting
  REAL(r64) :: RatedCapCoolSensDes          ! Autosized rated sensible cooling capacity for reporting
  REAL(r64) :: RatedCapCoolSensUser         ! Hardsized rated sensible cooling capacity for reporting
  REAL(r64) :: EvapCondPumpElecNomPowerDes  ! Autosized evaporative condenser pump power for reporting
  REAL(r64) :: EvapCondPumpElecNomPowerUser ! Hardsized evaporative condenser pump power for reporting
  REAL(r64) :: DefrostCapacityDes           ! Autosized resistive defrost heater capacity for reporting
  REAL(r64) :: DefrostCapacityUser          ! Hardsized resistive defrost heater capacity for reporting
  LOGICAL :: SizingDesRunThisAirSys            ! true if a particular air system had a Sizing:System object and system sizing done
  LOGICAL :: SizingDesRunThisZone              ! true if a particular zone had a Sizing:Zone object and zone sizing was done

  UpperSpeed = VarSpeedCoil(DXCoilNum)%NumOfSpeeds
  NormSpeed = VarSpeedCoil(DXCoilNum)%NormSpedLevel
  PltSizNum = 0
  ErrorsFound = .FALSE.
  RatedAirFlowAutosized = .FALSE.
  RatedWaterFlowAutosized = .FALSE.
  RatedCapHeatAutosized = .FALSE.
  IsAutosize = .FALSE.
    IF (SysSizingRunDone .OR. ZoneSizingRunDone) THEN
    HardSizeNoDesRun = .FALSE.
  ELSE
    HardSizeNoDesRun = .TRUE.
  ENDIF
  IF (CurSysNum > 0) THEN
    CALL CheckThisAirSystemForSizing(CurSysNum, SizingDesRunThisAirSys )
  ELSE
    SizingDesRunThisAirSys =  .FALSE.
  ENDIF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
  ELSE
    SizingDesRunThisZone =  .FALSE.
  ENDIF
  HardSizeNoDesRunAirFlow = .FALSE.
  RatedAirVolFlowRateDes = 0.0d0
  RatedAirVolFlowRateUser = 0.0d0
  RatedCapCoolTotalDes = 0.0d0
  RatedCapCoolTotalUser = 0.0d0
  RatedCapHeatDes = 0.0d0
  RatedCapHeatUser = 0.0d0
  RatedWaterVolFlowRateDes = 0.0d0
  RatedWaterVolFlowRateUser = 0.0d0
  RatedCapCoolSensDes = 0.0d0
  RatedCapCoolSensUser = 0.0d0
  EvapCondPumpElecNomPowerDes = 0.0d0
  EvapCondPumpElecNomPowerUser = 0.0d0
  DefrostCapacityDes = 0.0d0
  DefrostCapacityUser = 0.0d0

  IF  (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit .OR. &
  VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit ) THEN
    CurrentObjSubfix = ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT'
  ELSE
   CurrentObjSubfix = ':DX:VARIABLESPEED'
  END IF

  IF (VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate == AutoSize) THEN
    RatedAirFlowAutosized = .TRUE.
  END IF

  IF (CurSysNum > 0) THEN
    IF (.NOT. RatedAirFlowAutosized .AND. .NOT. SizingDesRunThisAirSys) THEN ! Simulation continue
      HardSizeNoDesRunAirFlow = .TRUE.
      IF (VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'User-Specified Rated Air Flow Rate [m3/s]', &
                             VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate)
      END IF
    ELSE
      CALL CheckSysSizing('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                          VarSpeedCoil(DXCoilNum)%Name)
      IF (FinalSysSizing(CurSysNum)%DesMainVolFlow >= SmallAirVolFlow) THEN
        RatedAirVolFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
      ELSE
        RatedAirVolFlowRateDes = 0.0d0
      END IF
    END IF
  END IF

  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. RatedAirFlowAutosized .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      HardSizeNoDesRunAirFlow = .TRUE.
      IF (VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'User-Specified Rated Air Flow Rate [m3/s]', &
                             VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate)
      END IF
    ELSE
      CALL CheckZoneSizing('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                          VarSpeedCoil(DXCoilNum)%Name)
      RatedAirVolFlowRateDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                               FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (RatedAirVolFlowRateDes < SmallAirVolFlow) THEN
        RatedAirVolFlowRateDes = 0.0d0
      END IF
    END IF
  END IF

  IF (RatedAirFlowAutosized) VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate = RatedAirVolFlowRateDes
!    CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
!                             VarSpeedCoil(DXCoilNum)%Name, &
!                            'Rated Air Flow Rate [m3/s]', &
!                             VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate)

    !RatedAirFlowAutosized = .TRUE.

  RatedCapCoolTotalAutosized = .FALSE.
  RatedCapCoolSensAutosized  = .FALSE.

! size rated total cooling capacity
  IsAutosize = .FALSE.
  IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal == AutoSize .AND. &
      VarSpeedCoil(DXCoilNum)%CoolHeatType == 'COOLING') THEN
    RatedCapCoolTotalAutosized = .TRUE.
  END IF
  IF (SizingDesRunThisZone .OR. SizingDesRunThisAirSys) HardSizeNoDesRun = .FALSE.
  IF (CurSysNum > 0) THEN
    IF (.NOT. RatedCapCoolTotalAutosized .AND. .NOT. SizingDesRunThisAirSys) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.
      IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix),&
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'User-Specified Rated Total Cooling Capacity [W]', &
                             VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
      END IF
    ELSE
      CALL CheckSysSizing('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                          VarSpeedCoil(DXCoilNum)%Name)
      VolFlowRate = VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate
      IF (VolFlowRate >= SmallAirVolFlow) THEN
        IF (CurOASysNum > 0) THEN ! coil is in the OA stream
          MixTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
          MixHumRat = FinalSysSizing(CurSysNum)%CoolOutHumRat
          SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
          SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
        ELSE ! coil is on the main air loop
          SupTemp = FinalSysSizing(CurSysNum)%CoolSupTemp
          SupHumRat = FinalSysSizing(CurSysNum)%CoolSupHumRat
          IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
            MixTemp = FinalSysSizing(CurSysNum)%CoolMixTemp
            MixHumRat = FinalSysSizing(CurSysNum)%CoolMixHumRat
          ELSE ! there is precooling of OA stream
            IF (VolFlowRate > 0.0d0) THEN
              OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / VolFlowRate
            ELSE
              OutAirFrac = 1.0d0
            END IF
            OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                        (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetTemp
            MixHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                          (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetHumRat
          END IF
        END IF
        OutTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
        rhoair = PsyRhoAirFnPbTdbW(OutBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,OutBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)

        TotCapTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(VarSpeedCoil(DXCoilNum)%NormSpedLevel), &
                    MixWetBulb,RatedInletWaterTemp)
!       The mixed air temp for zone equipment without an OA mixer is 0.
!       This test avoids a negative capacity until a solution can be found.
        IF(MixEnth .GT. SupEnth)THEN
          CoolCapAtPeak = rhoair * VolFlowRate * (MixEnth-SupEnth)
        ELSE
          CoolCapAtPeak = rhoair * VolFlowRate * (48000.0d0-SupEnth)
        END IF
        CoolCapAtPeak = MAX(0.0d0, CoolCapAtPeak)
        IF(TotCapTempModFac .GT. 0.0d0)THEN
          RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac
        ELSE
          RatedCapCoolTotalDes = CoolCapAtPeak
        END IF
      ELSE
        RatedCapCoolTotalDes = 0.0d0
      END IF
    END IF

  ELSE IF (CurZoneEqNum > 0) THEN
    IF (.NOT. RatedCapCoolTotalAutosized .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.
      IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix),&
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'User-Specified Rated Total Cooling Capacity [W]', &
                             VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
      END IF
    ELSE
      CALL CheckZoneSizing('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                          VarSpeedCoil(DXCoilNum)%Name)
      VolFlowRate = VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate
      IF (VolFlowRate >= SmallAirVolFlow) THEN
        IF(ZoneEqDXCoil)THEN
          IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0d0) THEN
            MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
            MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
          ELSE
            MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtCoolPeak
            MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
          END IF
        ELSE
          MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
          MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
        END IF
        SupTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
        SupHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
        TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtCoolMax
        DDNum = FinalZoneSizing(CurZoneEqNum)%CoolDDNum
        IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
          OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
        ELSE
          OutTemp = 0.0d0
        ENDIF
        rhoair = PsyRhoAirFnPbTdbW(OutBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,OutBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)

        TotCapTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(VarSpeedCoil(DXCoilNum)%NormSpedLevel), &
                MixWetBulb,RatedInletWaterTemp)
!       The mixed air temp for zone equipment without an OA mixer is 0.
!       This test avoids a negative capacity until a solution can be found.
        IF(MixEnth .GT. SupEnth)THEN
          CoolCapAtPeak = rhoair * VolFlowRate * (MixEnth-SupEnth)
        ELSE
          CoolCapAtPeak = rhoair * VolFlowRate * (48000.0d0-SupEnth)
        END IF
        CoolCapAtPeak = MAX(0.0d0, CoolCapAtPeak)
        IF(TotCapTempModFac .GT. 0.0d0)THEN
          RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac
        ELSE
          RatedCapCoolTotalDes = CoolCapAtPeak
        END IF
      ELSE
        RatedCapCoolTotalDes = 0.0d0
      END IF
    END IF
  END IF
  IF (RatedCapCoolTotalDes < SmallLoad) THEN
    RatedCapCoolTotalDes = 0.0d0
  END IF
  IF (.NOT. HardSizeNoDesRun) THEN
    IF (RatedCapCoolTotalAutosized) THEN
      VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal = RatedCapCoolTotalDes
      CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix),&
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Rated Total Cooling Capacity [W]', &
                             VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
      CALL PreDefTableEntry(pdchCoolCoilTotCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
      CALL PreDefTableEntry(pdchCoolCoilLatCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
                                 - VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
      IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal /= 0.0d0) THEN
        CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens &
                                   / VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
        CALL PreDefTableEntry(pdchCoolCoilNomEff,VarSpeedCoil(DXCoilNum)%Name,&
                VarSpeedCoil(DXCoilNum)%MSRatedCOP(NormSpeed))
      ELSE
        CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,0.0d0)
        CALL PreDefTableEntry(pdchCoolCoilNomEff,VarSpeedCoil(DXCoilNum)%Name,0.0d0)
      ENDIF
      CALL addFootNoteSubTable(pdstCoolCoil, 'Nominal values are gross at rated conditions, i.e., the supply air fan' &
                                          //' heat and electric power NOT accounted for.')
    ELSE
      IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal > 0.0d0 .AND. RatedCapCoolTotalDes > 0.0d0) THEN
        RatedCapCoolTotalUser = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal
        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix),&
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Rated Total Cooling Capacity [W]', &
                             RatedCapCoolTotalDes, &
                            'User-Specified Rated Total Cooling Capacity [W]', &
                             RatedCapCoolTotalUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(RatedCapCoolTotalDes - RatedCapCoolTotalUser)/RatedCapCoolTotalUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeVarSpeedCoil: Potential issue with equipment sizing for ' &
                               //TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//' '//TRIM(CurrentObjSubfix))
            CALL ShowContinueError('Coil Name ='//TRIM(VarSpeedCoil(DXCoilNum)%Name))
            CALL ShowContinueError('User-Specified Rated Total Cooling Capacity of '// &
                                      TRIM(RoundSigDigits(RatedCapCoolTotalUser,2))// ' [W]')
            CALL ShowContinueError('differs from Design Size Rated Total Cooling Capacity of ' // &
                                      TRIM(RoundSigDigits(RatedCapCoolTotalDes,2))// ' [W]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF
      END IF
    END IF
  END IF

  ! Set the global DX cooling coil capacity variable for use by other objects
  IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'COOLING') THEN
    DXCoolCap = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal
  END IF

! size rated heating capacity
  IF (VarSpeedCoil(DXCoilNum)%RatedCapHeat == AutoSize .AND. &
      VarSpeedCoil(DXCoilNum)%CoolHeatType == 'HEATING') THEN
    RatedCapHeatAutosized = .TRUE.
  END IF
!   simply set heating capacity equal to the cooling capacity
    !VarSpeedCoil(DXCoilNum)%RatedCapHeat = DXCoolCap
  IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'HEATING') THEN
    IF(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum > 0) THEN
      RatedCapHeatDes = VarSpeedCoil(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal
      VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal = RatedCapHeatDes !AVOID BEING ZERO
    ELSE
      RatedCapHeatDes = DXCoolCap !previous code, can be risky
    END IF
  !END IF
    IF (RatedCapHeatAutosized) THEN
      IF(RatedCapHeatDes == Autosize)THEN
        CALL ShowWarningError('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType) &
                      //':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'// &
                              TRIM(VarSpeedCoil(DXCoilNum)%Name)//'"')
        CALL ShowContinueError(RoutineName//': Heating coil could not be autosized since cooling coil was not previously sized.')
        CALL ShowContinueError('... Cooling coil must be upstream of heating coil.')
        CALL ShowContinueError('... Manually sizing this heating coil will be required.')
      END IF
    END IF
    IF (RatedCapHeatDes < SmallLoad) THEN
      RatedCapHeatDes = 0.0d0
    END IF
  END IF
  IF (RatedCapHeatAutosized) THEN
    VarSpeedCoil(DXCoilNum)%RatedCapHeat = RatedCapHeatDes
    CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix),&
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Nominal Heating Capacity [W]', &
                             RatedCapHeatDes)
    CALL PreDefTableEntry(pdchHeatCoilNomCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapHeat)
    IF (VarSpeedCoil(DXCoilNum)%RatedCapHeat /= 0.0d0) THEN
      CALL PreDefTableEntry(pdchHeatCoilNomEff,VarSpeedCoil(DXCoilNum)%Name,&
                VarSpeedCoil(DXCoilNum)%MSRatedCOP(NormSpeed))
    ELSE
      CALL PreDefTableEntry(pdchHeatCoilNomEff,VarSpeedCoil(DXCoilNum)%Name,0.0d0)
    ENDIF
    CALL addFootNoteSubTable(pdstHeatCoil, 'Nominal values are gross at rated conditions, i.e., the supply air fan' &
                                          //' heat and electric power NOT accounted for.')
  ELSE
    IF (VarSpeedCoil(DXCoilNum)%RatedCapHeat > 0.0d0 .AND. RatedCapHeatDes > 0.0d0) THEN
      RatedCapHeatUser = VarSpeedCoil(DXCoilNum)%RatedCapHeat
      CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix),&
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Nominal Heating Capacity [W]', &
                             RatedCapHeatDes, &
                            'User-Specified Nominal Heating Capacity [W]', &
                             RatedCapHeatUser)
      IF (DisplayExtraWarnings) THEN
        IF ((ABS(RatedCapHeatDes - RatedCapHeatUser)/RatedCapHeatUser) > AutoVsHardSizingThreshold) THEN
          CALL ShowMessage('SizeVarSpeedCoil: Potential issue with equipment sizing for ' &
                            //TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//' '//TRIM(CurrentObjSubfix))
          CALL ShowContinueError('Coil Name ='//TRIM(VarSpeedCoil(DXCoilNum)%Name))
          CALL ShowContinueError('User-Specified Rated Total Heating Capacity of '// &
                                      TRIM(RoundSigDigits(RatedCapHeatUser,2))// ' [W]')
          CALL ShowContinueError('differs from Design Size Rated Total Heating Capacity of ' // &
                                      TRIM(RoundSigDigits(RatedCapHeatDes,2))// ' [W]')
          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
        END IF
      ENDIF
    END IF
  END IF

  ! FORCE BACK TO THE RATED AIR FLOW RATE WITH THE SAME RATIO DEFINED BY THE CATLOG DATA
  IF (.NOT. HardSizeNoDesRunAirFlow) THEN
    IF((RatedCapCoolTotalAutosized) .AND. &
        (RatedAirFlowAutosized))   THEN
      RatedAirVolFlowRateDes = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
                             *VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(NormSpeed)
    ELSE IF((RatedCapHeatAutosized) .AND. &
        (RatedAirFlowAutosized))   THEN
      RatedAirVolFlowRateDes = VarSpeedCoil(DXCoilNum)%RatedCapHeat &
                               *VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(NormSpeed)
    END IF

    ! write the air flow sizing output
    IF(RatedAirFlowAutosized) THEN
      VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate = RatedAirVolFlowRateDes
      CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Rated Air Flow Rate [m3/s]', &
                             RatedAirVolFlowRateDes)
    ELSE
      IF (VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate > 0.0d0 .AND. RatedAirVolFlowRateDes > 0.0d0) THEN
        RatedAirVolFlowRateUser = VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate
        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Rated Air Flow Rate [m3/s]', &
                             RatedAirVolFlowRateDes, &
                            'User-Specified Rated Air Flow Rate [m3/s]', &
                             RatedAirVolFlowRateUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(RatedAirVolFlowRateDes - RatedAirVolFlowRateUser)/RatedAirVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeVarSpeedCoil: Potential issue with equipment sizing for' &
                                 //TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//' '//TRIM(CurrentObjSubfix))
            CALL ShowContinueError('Coil Name ='//TRIM(VarSpeedCoil(DXCoilNum)%Name))
            CALL ShowContinueError('User-Specified Rated Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(RatedAirVolFlowRateUser,5))// ' [m3/s]')
            CALL ShowContinueError('differs from Design Size Rated Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(RatedAirVolFlowRateDes,5))// ' [m3/s]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF
      END IF
    END IF
  END IF

  ! Check that heat pump heating capacity is within 20% of cooling capacity. Check only for heating coil and report both.
  IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'HEATING' .AND. &
      VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum .GT. 0)THEN

    IF(VarSpeedCoil(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal .GT. 0.0D0)THEN

      IF(ABS(VarSpeedCoil(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal- &
           VarSpeedCoil(DXCoilNum)%RatedCapHeat)/ &
           VarSpeedCoil(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal .GT. 0.2d0) THEN

        CALL ShowWarningError('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)// &
                            ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'//TRIM(VarSpeedCoil(DXCoilNum)%Name)//'"')
        CALL ShowContinueError('...used with COIL:'// &
               TRIM(VarSpeedCoil(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum)%CoolHeatType)// &
                            ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'// &
               TRIM(VarSpeedCoil(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum)%Name)//'"')
        CALL ShowContinueError('...heating capacity is disproportionate (> 20% different) to total cooling capacity')
        CALL ShowContinueError('...heating capacity = '//TRIM(TrimSigDigits(VarSpeedCoil(DXCoilNum)%RatedCapHeat,3))//' W')
        CALL ShowContinueError('...cooling capacity = '// &
           TRIM(TrimSigDigits(VarSpeedCoil(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal,3))//' W')
      END IF
    END IF
  END IF

!ASSIGN CAPACITY
 IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'COOLING') THEN
    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(UpperSpeed) = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal/ &
            VarSpeedCoil(DXCoilNum)%MSRatedPercentTotCap(NormSpeed)
  ELSE IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'HEATING') THEN
    VarSpeedCoil(DXCoilNum)%MSRatedTotCap(UpperSpeed) = VarSpeedCoil(DXCoilNum)%RatedCapHeat/ &
            VarSpeedCoil(DXCoilNum)%MSRatedPercentTotCap(NormSpeed)
  END IF

  rhoA = PsyRhoAirFnPbTdbW(OutBaroPress,RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
  Do Mode = VarSpeedCoil(DXCoilNum)%NumOfSpeeds,1,-1
     VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode) = VarSpeedCoil(DXCoilNum)%MSRatedTotCap(UpperSpeed) * &
                VarSpeedCoil(DXCoilNum)%MSRatedPercentTotCap(Mode)
     VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode) = VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode) * &
                VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(Mode)
     VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode) = VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)* &
                rhoA
    ! EVAPORATIVE PRECOOLING CONDENSER AIR FLOW RATE
    VarSpeedCoil(DXCoilNum)%EvapCondAirFlow(Mode) = VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode) * &
                VarSpeedCoil(DXCoilNum)%MSRatedEvapCondVolFlowPerRatedTotCap(Mode)
  END DO

! size rated power
  IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'COOLING') THEN

    VarSpeedCoil(DXCoilNum)%RatedCOPCool = VarSpeedCoil(DXCoilNum)%MSRatedCOP(VarSpeedCoil(DXCoilNum)%NormSpedLevel)
    VarSpeedCoil(DXCoilNum)%RatedPowerCool = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal/VarSpeedCoil(DXCoilNum)%RatedCOPCool

  ELSE IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'HEATING') THEN
    VarSpeedCoil(DXCoilNum)%RatedCOPHeat = VarSpeedCoil(DXCoilNum)%MSRatedCOP(VarSpeedCoil(DXCoilNum)%NormSpedLevel)
    VarSpeedCoil(DXCoilNum)%RatedPowerHeat = VarSpeedCoil(DXCoilNum)%RatedCapHeat/VarSpeedCoil(DXCoilNum)%RatedCOPHeat
    VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal = VarSpeedCoil(DXCoilNum)%RatedCapHeat
  END IF

! Size water volumetric flow rate
  IF ((VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate == AutoSize) .AND. &
    (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit .OR. &
    VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit ))  THEN
    RatedWaterFlowAutosized = .TRUE.
  END IF

!!   if not found on a plant loop, check condenser loop and warn user if not found
!    IF(PltSizNum == 0) THEN
!
!      PltSizNum = &
!          MyCondPlantSizingIndex('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)// &
!               TRIM(CurrentObjSubfix), &
!                                 VarSpeedCoil(DXCoilNum)%Name, &
!                                 VarSpeedCoil(DXCoilNum)%WaterInletNodeNum, &
!                                 VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum, ErrorsFound)
!    END IF

  !   WSHP condenser can be on either a plant loop or condenser loop. Test each to find plant sizing number.
!   first check to see if coil is connected to a plant loop, no warning on this CALL
  IF (RatedWaterFlowAutosized) THEN
    IF(VarSpeedCoil(DXCoilNum)%CondenserType == WaterCooled)PltSizNum = &
        MyPlantSizingIndex('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                           VarSpeedCoil(DXCoilNum)%Name, &
                           VarSpeedCoil(DXCoilNum)%WaterInletNodeNum, &
                           VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum, ErrorsFound, .FALSE.)

    IF (PltSizNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidName, &
                             PlantSizData(PltSizNum)%ExitTemp, &
                             PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                             'SizeHVACWaterToAir')
      Cp  = GetSpecificHeatGlycol(PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidName, &
                                  PlantSizData(PltSizNum)%ExitTemp, &
                                  PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                                 'SizeHVACWaterToAir')

      IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'HEATING') THEN

        RatedWaterVolFlowRateDes = VarSpeedCoil(DXCoilNum)%RatedCapHeat / &
                                 ( PlantSizData(PltSizNum)%DeltaT * Cp * rho )


!        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//&
!                                TRIM(CurrentObjSubfix), &
!                                  VarSpeedCoil(DXCoilNum)%Name, &
!                                  'Rated Water Flow Rate [m3/s]', VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate)

      ELSEIF(VarSpeedCoil(DXCoilNum)%CoolHeatType == 'COOLING') THEN

!       use companion heating coil capacity to calculate volumetric flow rate
        IF(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum .GT. 0)THEN
          SystemCapacity = VarSpeedCoil(VarSpeedCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapHeat
        ELSE
          SystemCapacity = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal
        END IF

        RatedWaterVolFlowRateDes = &
                                SystemCapacity / &
                                ( PlantSizData(PltSizNum)%DeltaT * Cp * rho )

!        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)&
!                                //TRIM(CurrentObjSubfix), &
!                                  VarSpeedCoil(DXCoilNum)%Name, &
!                                  'Rated Water Flow Rate [m3/s]', VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate)
      END IF
    ELSE
      CALL ShowSevereError('Autosizing of water flow requires a loop Sizing:Plant object')
      CALL ShowContinueError('Autosizing also requires physical connection to a plant or condenser loop.')
      CALL ShowContinueError('Occurs in ' // &
                 'COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//&
                        TRIM(CurrentObjSubfix) // ' Object=' &
                //TRIM(VarSpeedCoil(DXCoilNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

    !WRITE THE WATER SIZING OUTPUT
  IF (RatedWaterFlowAutosized) THEN
      ! FORCE BACK TO THE RATED WATER FLOW RATE WITH THE SAME RATIO DEFINED BY THE CATLOG DATA
    IF(RatedCapCoolTotalAutosized)   THEN
       RatedWaterVolFlowRateDes = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
                        *VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowPerRatedTotCap(NormSpeed)
    ELSE IF(RatedCapHeatAutosized)  THEN
      RatedWaterVolFlowRateDes = VarSpeedCoil(DXCoilNum)%RatedCapHeat &
                        *VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowPerRatedTotCap(NormSpeed)
    END IF
    VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate = RatedWaterVolFlowRateDes
      CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType) &
                              //TRIM(CurrentObjSubfix), &
                              VarSpeedCoil(DXCoilNum)%Name, &
                              'Design Size Rated Water Flow Rate [m3/s]', RatedWaterVolFlowRateDes)
      ! Ensure water flow rate at lower speed must be lower or
      ! equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
    Do Mode = 1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds-1
      If (VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(Mode) .GT. &
      VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(Mode+1) * 1.05d0) Then
        CALL ShowWarningError('SizeDXCoil: '//TRIM(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType) &
                //' '//TRIM(VarSpeedCoil(DXCoilNum)%Name)//', '// &
          'Speed '//Trim(TrimSigDigits(Mode))//' Rated Air Flow Rate must be less than or equal to '//&
          'Speed '//Trim(TrimSigDigits(Mode+1))//' Rated Air Flow Rate.')
        CALL ShowContinueError('Instead, '//TRIM(RoundSigDigits(VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode),2))//' > '//  &
                  TRIM(RoundSigDigits(VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode+1),2)))
        CALL ShowFatalError('Preceding conditions cause termination.')
      End If
    End Do
  ELSE
    IF (VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate > 0.0d0 .AND. RatedWaterVolFlowRateDes > 0.0d0) THEN
      RatedWaterVolFlowRateUser = VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate
      CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix), &
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Rated Water Flow Rate [m3/s]', &
                             RatedWaterVolFlowRateDes, &
                            'User-Specified Rated Water Flow Rate [m3/s]', &
                             RatedWaterVolFlowRateUser)
      IF (DisplayExtraWarnings) THEN
        IF ((ABS(RatedWaterVolFlowRateDes - RatedWaterVolFlowRateUser)/RatedWaterVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
          CALL ShowMessage('SizeVarSpeedCoil: Potential issue with equipment sizing for' &
                               //TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//' '//TRIM(CurrentObjSubfix))
          CALL ShowContinueError('Coil Name ='//TRIM(VarSpeedCoil(DXCoilNum)%Name))
          CALL ShowContinueError('User-Specified Rated Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(RatedWaterVolFlowRateUser,5))// ' [m3/s]')
          CALL ShowContinueError('differs from Design Size Rated Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(RatedWaterVolFlowRateDes,5))// ' [m3/s]')
          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
        END IF
      ENDIF
    END IF
  END IF

! Save component design water volumetric flow rate.
! Use 1/2 flow since both cooling and heating coil will save flow yet only 1 will operate at a time
  IF(VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate .GT. 0.0d0)THEN
    CALL RegisterPlantCompDesignFlow(VarSpeedCoil(DXCoilNum)%WaterInletNodeNum,  &
       0.5d0*VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate)
  END IF

  IF (VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit .OR. &
  VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit ) THEN

    RatedSourceTempCool = RatedInletWaterTemp

    IF (PltSizNum > 0) THEN
      rhoW = rho
    ELSE IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit) THEN
      rhoW = GetDensityGlycol(PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidName, &
                                     RatedInletWaterTemp, &
                                     PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                                     'SizeVarSpeedCoil')
    ELSE
      rhoW = GetDensityGlycol(PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidName, &
                                     RatedInletWaterTempHeat, &
                                     PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                                     'SizeVarSpeedCoil')
    END IF

    VarSpeedCoil(DXCoilNum)%RatedWaterMassFlowRate = VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate * rhoW
    Do Mode = VarSpeedCoil(DXCoilNum)%NumOfSpeeds,1,-1
      VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(Mode) = VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode) * &
                    VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowPerRatedTotCap(Mode)
      VarSpeedCoil(DXCoilNum)%MSRatedWaterMassFlowRate(Mode) = &
                VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(Mode) * rhoW
    END DO
  ELSE
      RatedSourceTempCool = RatedAmbAirTemp
  END IF

    ! Ensure air flow rate at lower speed must be lower or
    ! equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
  Do Mode = 1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds-1
      If (VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode) .GT. VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode+1)) Then
        CALL ShowWarningError('SizeDXCoil: '//TRIM(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType) &
                //' '//TRIM(VarSpeedCoil(DXCoilNum)%Name)//', '// &
          'Speed '//Trim(TrimSigDigits(Mode))//' Rated Air Flow Rate must be less than or equal to '//&
          'Speed '//Trim(TrimSigDigits(Mode+1))//' Rated Air Flow Rate.')
        CALL ShowContinueError('Instead, '//TRIM(RoundSigDigits(VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode),2))//' > '//  &
                  TRIM(RoundSigDigits(VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode+1),2)))
        CALL ShowFatalError('Preceding conditions cause termination.')
      End If
  End Do

   ! Ensure capacity at lower speed must be lower or equal to the capacity at higher speed.
  Do Mode = 1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds-1
      If (VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode) .GT. VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode+1)) Then
        CALL ShowWarningError('SizeDXCoil: '//TRIM(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType)&
                //' '//TRIM(VarSpeedCoil(DXCoilNum)%Name)//', '// &
          'Speed '//Trim(TrimSigDigits(Mode))//' Rated Total Cooling Capacity must be less than or equal to '//&
          'Speed '//Trim(TrimSigDigits(Mode+1))//' Rated Total Cooling Capacity.')
        CALL ShowContinueError('Instead, '//TRIM(RoundSigDigits(VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode),2))//' > '//  &
                  TRIM(RoundSigDigits(VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode+1),2)))
        CALL ShowFatalError('Preceding conditions cause termination.')
      End If
  End Do

  !convert SHR to rated Bypass factor and effective air side surface area
  IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'COOLING') THEN
    Do Mode = 1,VarSpeedCoil(DXCoilNum)%NumOfSpeeds
        VarSpeedCoil(DXCoilNum)%MSRatedCBF(Mode) = &
        CalcCBF(VarSpeedCoil(DXCoilNum)%VarSpeedCoilType,VarSpeedCoil(DXCoilNum)%Name,&
                                           RatedInletAirTemp,RatedInletAirHumRat,VarSpeedCoil(DXCoilNum)%MSRatedTotCap(Mode),&
                                           VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode),&
                                           VarSpeedCoil(DXCoilNum)%MSRatedSHR(Mode))
        IF ( VarSpeedCoil(DXCoilNum)%MSRatedCBF(Mode) .gt. 0.0d0) THEN
            VarSpeedCoil(DXCoilNum)%MSEffectiveAo(Mode) = -log( VarSpeedCoil(DXCoilNum)%MSRatedCBF(Mode))* &
            VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode)
        ELSE
            VarSpeedCoil(DXCoilNum)%MSEffectiveAo(Mode) = 0.0d0
        END IF
    End Do
  END IF

  ! size rated sensible cooling capacity
  RatedCapCoolSensAutosized  = .TRUE.  !always do that

  IF (VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate >= SmallAirVolFloW .AND. VarSpeedCoil(DXCoilNum)%CoolHeatType == 'COOLING') THEN
     RatedAirMassFlowRate = VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate* &
           PsyRhoAirFnPbTdbW(StdBaroPress,RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
     RatedInletEnth = PsyHFnTdbW(RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
     CBFRated  = AdjustCBF(VarSpeedCoil(DXCoilNum)%MSRatedCBF(NormSpeed),  &
        VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(NormSpeed), &
                RatedAirMassFlowRate)
    IF(CBFRated > 0.999d0) CBFRated = 0.999d0
    AirMassFlowRatio = VarSpeedCoil(DXCoilNum)%RatedAirVolFlowRate/ VarSpeedCoil(DXCoilNum)%MSRatedAirVolFlowRate(NormSpeed)

    IF(VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(NormSpeed).GT. 1.0d-10) THEN
      WaterMassFlowRatio = VarSpeedCoil(DXCoilNum)%RatedWaterVolFlowRate/  &
                           VarSpeedCoil(DXCoilNum)%MSRatedWaterVolFlowRate(NormSpeed)
    ELSE
      WaterMassFlowRatio = 1.0d0
    END IF

    CALL CalcTotCapSHR_VSWSHP(RatedInletAirTemp,RatedInletAirHumRat,RatedInletEnth,RatedInletWetbulbTemp, &
                     AirMassFlowRatio, WaterMassFlowRatio, &
                     RatedAirMassFlowRate,CBFRated, &
                     VarSpeedCoil(DXCoilNum)%MSRatedTotCap(NormSpeed),VarSpeedCoil(DXCoilNum)%MSCCapFTemp(NormSpeed), &
                     VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(NormSpeed), VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(NormSpeed),&
                     0.0d0,0,0, 0,&
                     QLoadTotal1, QLoadTotal2, QLoadTotal,SHR,RatedSourceTempCool, &
                     StdBaroPress, 0.0d0, 1)

    RatedCapCoolSensDes = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal * SHR
  ELSE
    RatedCapCoolSensDes = 0.0d0
  END IF

  IF (RatedCapCoolSensDes < SmallLoad) THEN
    RatedCapCoolSensDes = 0.0d0
  END IF

  IF (VarSpeedCoil(DXCoilNum)%CoolHeatType == 'COOLING') THEN !always report for cooling mode
    IF(RatedCapCoolTotalAutosized) THEN
      VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = RatedCapCoolSensDes
      CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix),&
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Rated Sensible Cooling Capacity [W]', &
                             VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
      !CALL PreDefTableEntry(pdchCoolCoilSensCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
      !CALL PreDefTableEntry(pdchCoolCoilLatCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
      !                           - VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
      !IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal /= 0.0d0) THEN
      !  CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens &
      !                           / VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
      !ELSE
      !  CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,0.0d0)
      !ENDIF
    ELSE
      ! sensible capacity does not have an input field
      IF (RatedCapCoolSensDes > 0.0d0 .AND. RatedCapCoolSensDes > 0.0d0) THEN
        VarSpeedCoil(DXCoilNum)%RatedCapCoolSens = RatedCapCoolSensDes
        CALL ReportSizingOutput('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//TRIM(CurrentObjSubfix),&
                             VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Rated Sensible Cooling Capacity [W]', &
                             RatedCapCoolSensDes) !, &
!                            'User-Specified Rated Sensible Cooling Capacity [W]', &
!                             RatedCapCoolSensUser)
!       IF (DisplayExtraWarnings) THEN
  !       IF ((ABS(RatedCapCoolSensDes - RatedCapCoolSensUser)/RatedCapCoolSensUser) > AutoVsHardSizingThreshold) THEN
  !          CALL ShowMessage('SizeVarSpeedCoil: Potential issue with equipment sizing for:' &
  !                      //TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//' '//TRIM(CurrentObjSubfix))
  !          CALL ShowContinueError('Coil Name ='//TRIM(VarSpeedCoil(DXCoilNum)%Name))
  !          CALL ShowContinueError('User-Specified Rated Sensible Cooling Capacity of '// &
  !                                    TRIM(RoundSigDigits(RatedCapCoolSensUser,2))// ' [W]')
  !          CALL ShowContinueError('differs from Design Size Rated Sensible Cooling Capacity of ' // &
  !                                    TRIM(RoundSigDigits(RatedCapCoolSensDes,2))// ' [W]')
  !          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
  !          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
  !        END IF
!        ENDIF
      END IF
    END IF
    CALL PreDefTableEntry(pdchCoolCoilTotCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
    CALL PreDefTableEntry(pdchCoolCoilSensCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
    CALL PreDefTableEntry(pdchCoolCoilLatCap,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal &
                                - VarSpeedCoil(DXCoilNum)%RatedCapCoolSens)
    IF (VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal /= 0.0d0) THEN
       CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,VarSpeedCoil(DXCoilNum)%RatedCapCoolSens &
                                / VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)
    ELSE
       CALL PreDefTableEntry(pdchCoolCoilSHR,VarSpeedCoil(DXCoilNum)%Name,0.0d0)
    ENDIF
    CALL PreDefTableEntry(pdchCoolCoilNomEff,VarSpeedCoil(DXCoilNum)%Name,&
                          VarSpeedCoil(DXCoilNum)%MSRatedCOP(VarSpeedCoil(DXCoilNum)%NormSpedLevel))
    CALL addFootNoteSubTable(pdstCoolCoil, 'Nominal values are gross at rated conditions, i.e., the supply air fan' &
                                            //' heat and electric power NOT accounted for.')
  END IF

 ! START SIZING EVAP PRECOOLING PUMP POWER
  IsAutosize = .FALSE.
  IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed)    THEN
    IF (VarSpeedCoil(DXCoilNum)%EvapCondPumpElecNomPower == AutoSize) THEN
      IsAutosize = .TRUE.
    END IF
   !     Auto size high speed evap condenser pump power to Total Capacity * 0.004266 w/w (15 w/ton)
    EvapCondPumpElecNomPowerDes = VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal*0.004266d0
    IF (IsAutosize) THEN
      VarSpeedCoil(DXCoilNum)%EvapCondPumpElecNomPower = EvapCondPumpElecNomPowerDes
      CALL ReportSizingOutput("AS VS COOLING COIL", VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Evaporative Condenser Pump Rated Power Consumption [W]',   &
                             EvapCondPumpElecNomPowerDes)
    ELSE
      IF (VarSpeedCoil(DXCoilNum)%EvapCondPumpElecNomPower > 0.0d0 .AND. EvapCondPumpElecNomPowerDes > 0.0d0) THEN
        EvapCondPumpElecNomPowerUser = VarSpeedCoil(DXCoilNum)%EvapCondPumpElecNomPower
        CALL ReportSizingOutput("AS VS COOLING COIL", VarSpeedCoil(DXCoilNum)%Name, &
                            'Design Size Evaporative Condenser Pump Rated Power Consumption [W]',   &
                             EvapCondPumpElecNomPowerDes, &
                            'User-Specified Evaporative Condenser Pump Rated Power Consumption [W]',   &
                             EvapCondPumpElecNomPowerUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(EvapCondPumpElecNomPowerDes - EvapCondPumpElecNomPowerUser)/EvapCondPumpElecNomPowerUser) &
                      > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeVarSpeedCoil: Potential issue with equipment sizing for ' &
                               //TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//' '//TRIM(CurrentObjSubfix))
            CALL ShowContinueError('Coil Name ='//TRIM(VarSpeedCoil(DXCoilNum)%Name))
            CALL ShowContinueError('User-Specified Evaporative Condenser Pump Rated Power Consumption of '// &
                                      TRIM(RoundSigDigits(EvapCondPumpElecNomPowerUser,2))// ' [W]')
            CALL ShowContinueError('differs from Design Size Evaporative Condenser Pump Rated Power Consumption of ' // &
                                      TRIM(RoundSigDigits(EvapCondPumpElecNomPowerDes,2))// ' [W]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF
      END IF
    END IF
  END IF
   ! END SIZING EVAP PRE-COOLING PUMP POWER

   !SIZE DEFROST HEATER

    ! Resistive Defrost Heater Capacity = capacity at the first stage
  IsAutosize = .FALSE.
  IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed)    THEN
    IF (VarSpeedCoil(DXCoilNum)%DefrostCapacity == AutoSize) THEN
      IsAutosize = .TRUE.
    END IF
    IF (VarSpeedCoil(DXCoilNum)%DefrostStrategy == Resistive) THEN
      DefrostCapacityDes = VarSpeedCoil(DXCoilNum)%RatedCapHeat
    ELSE
      DefrostCapacityDes = 0.0d0
    END IF
    IF (IsAutosize) THEN
      VarSpeedCoil(DXCoilNum)%DefrostCapacity = DefrostCapacityDes
      CALL ReportSizingOutput("AS VS HEATING COIL", VarSpeedCoil(DXCoilNum)%Name, &
                              'Design Size Resistive Defrost Heater Capacity [W]', DefrostCapacityDes)
    ELSE
      IF (VarSpeedCoil(DXCoilNum)%DefrostCapacity > 0.0d0 .AND. DefrostCapacityDes > 0.0d0 &
           .AND. .NOT. HardSizeNoDesRun) THEN
        DefrostCapacityUser = VarSpeedCoil(DXCoilNum)%DefrostCapacity
        CALL ReportSizingOutput("AS VS HEATING COIL", VarSpeedCoil(DXCoilNum)%Name, &
                              'Design Size Resistive Defrost Heater Capacity [W]', DefrostCapacityDes, &
                              'User-Specified Resistive Defrost Heater Capacity [W]', DefrostCapacityUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(DefrostCapacityDes - DefrostCapacityUser)/DefrostCapacityUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeVarSpeedCoil: Potential issue with equipment sizing for ' &
                                  //TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)//' '//TRIM(CurrentObjSubfix))
            CALL ShowContinueError('Coil Name ='//TRIM(VarSpeedCoil(DXCoilNum)%Name))
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
  END IF
   !END SIZING DEFROST HEATER

! test autosized sensible and total cooling capacity for total > sensible
  IF(RatedCapCoolSensAutosized .AND. RatedCapCoolTotalAutosized .OR. &
     RatedCapCoolSensAutosized)THEN
    IF(VarSpeedCoil(DXCoilNum)%RatedCapCoolSens .GT. VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)THEN
        CALL ShowWarningError('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)// &
                ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'// &
                               TRIM(VarSpeedCoil(DXCoilNum)%Name)//'"')
        CALL ShowContinueError(RoutineName//': Rated Sensible Cooling Capacity > Rated Total Cooling Capacity')
        CALL ShowContinueError('Each of these capacity inputs have been autosized.')
        CALL ShowContinueError('Rated Sensible Cooling Capacity = '// &
                                TRIM(TrimSigDigits(VarSpeedCoil(DXCoilNum)%RatedCapCoolSens,2))//' W')
        CALL ShowContinueError('Rated Total Cooling Capacity    = '// &
                                TRIM(TrimSigDigits(VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal,2))//' W')
        CALL ShowContinueError('See eio file for further details.')
        CALL ShowContinueError('Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate.')
        CALL ShowContinueError('Check Zone and System Sizing objects to verify sizing inputs.')
        CALL ShowContinueError('Sizing statistics:')
        CALL ShowContinueError('Entering Air Dry-Bulb Temperature = '//TRIM(TrimSigDigits(MixTemp,3))//' C')
        CALL ShowContinueError('Entering Air Wet-Bulb Temperature = '//TRIM(TrimSigDigits(MixWetBulb,3))//' C')
        CALL ShowContinueError('Entering Condenser Water Temperature used = 24.4444 C')
        CALL ShowContinueError('Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)')
        CALL ShowContinueError('ratioTDB = '//TRIM(TrimSigDigits(((MixTemp+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('ratioTWB = '//TRIM(TrimSigDigits(((MixWetBulb+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('ratioTS  = '//TRIM(TrimSigDigits(((85.0d0+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('Rated Sensible Cooling Capacity = Rated Total Cooling Capacity * Sensible Heat Ratio')
        CALL ShowContinueError('Total Cooling Capacity Modifier = '//TRIM(TrimSigDigits(TotCapTempModFac,5)))
        CALL ShowContinueError('...Rated Total Cooling Capacity = Total Design Load / Total Cooling Capacity Modifier')
        CALL ShowContinueError('Carefully review the Load Side Total, Sensible, and Latent heat transfer rates')
        CALL ShowContinueError('... to ensure they meet the expected manufacturers performance specifications.')
    END IF
  ELSE If(RatedCapCoolTotalAutosized)THEN
    IF(VarSpeedCoil(DXCoilNum)%RatedCapCoolSens .GT. VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal)THEN
        CALL ShowWarningError('COIL:'//TRIM(VarSpeedCoil(DXCoilNum)%CoolHeatType)&
                        //':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'// &
                               TRIM(VarSpeedCoil(DXCoilNum)%Name)//'"')
        CALL ShowContinueError(RoutineName//': Rated Sensible Cooling Capacity > Rated Total Cooling Capacity')
        CALL ShowContinueError('Only the rated total capacity input is autosized, consider autosizing both inputs.')
        CALL ShowContinueError('Rated Sensible Cooling Capacity = '// &
                                TRIM(TrimSigDigits(VarSpeedCoil(DXCoilNum)%RatedCapCoolSens,2))//' W')
        CALL ShowContinueError('Rated Total Cooling Capacity    = '// &
                                TRIM(TrimSigDigits(VarSpeedCoil(DXCoilNum)%RatedCapCoolTotal,2))//' W')
        CALL ShowContinueError('See eio file for further details.')
        CALL ShowContinueError('Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate.')
        CALL ShowContinueError('Check Zone and System Sizing objects to verify sizing inputs.')
        CALL ShowContinueError('Sizing statistics for Total Cooling Capacity:')
        CALL ShowContinueError('Entering Air Wet-Bulb Temperature = '//TRIM(TrimSigDigits(MixWetBulb,3))//' C')
        CALL ShowContinueError('Entering Condenser Water Temperature used = 24.4444 C')
        CALL ShowContinueError('Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)')
        CALL ShowContinueError('ratioTWB = '//TRIM(TrimSigDigits(((MixWetBulb+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('ratioTS  = '//TRIM(TrimSigDigits(((85.0d0+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('Rated Sensible Cooling Capacity = Rated Total Cooling Capacity * Sensible Heat Ratio')
        CALL ShowContinueError('Carefully review the Load Side Total, Sensible, and Latent heat transfer rates')
        CALL ShowContinueError('... to ensure they meet the expected manufacturers performance specifications.')
    END IF
  END IF

  RETURN

END SUBROUTINE SizeVarSpeedCoil


SUBROUTINE  CalcVarSpeedCoilCooling(DXCoilNum,CyclingScheme, &
            RuntimeFrac,SensDemand,LatentDemand,CompOp,PartLoadRatio,OnOffAirFlowRatio, &
            SpeedRatio, SpeedNum)


          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:CalcHPCoolingSimple
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for simulating the cooling mode of the Variable-Speed Water to Air HP Simple

          ! METHODOLOGY EMPLOYED:
          ! Simulate the heat pump performance using the coefficients and rated conditions, interpolating between speed levels
          !
          ! If the LatDegradModelSimFlag is enabled, the coil will be simulated twice:
          ! (1)first simulation at the rated conditions (2) second simulation at the
          ! actual operating conditions. Then call CalcEffectiveSHR and the effective SHR
          ! is adjusted.
          !
          ! If the LatDegradModelSimFlag is disabled, the cooling coil is only simulated
          ! once at the actual operating conditions.
          !
          ! Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
          ! and RuntimeFrac.

          ! REFERENCES:
          ! n/a

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals,      ONLY: TimeStepSys, DXElecCoolingPower
  USE Psychrometrics,       ONLY: PsyWFnTdbTwbPb,PsyCpAirFnWTdb,PsyHFnTdbW,PsyRhoAirFnPbTdbW,  &
                                  PsyTwbFnTdbWPb,PsyTdbFnHW,PsyWFnTdbH
  USE FluidProperties,      ONLY: GetSpecificHeatGlycol
  USE DataPlant,            ONLY: PlantLoop
  USE DataWater,       ONLY: WaterStorage

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER,   INTENT(IN) :: DXCoilNum              ! Heat Pump Number
  INTEGER,   INTENT(IN) :: CyclingScheme      ! Fan/Compressor cycling scheme indicator
  REAL(r64), INTENT(IN) :: SensDemand         ! Cooling Sensible Demand [W] !unused1208
  REAL(r64), INTENT(IN) :: LatentDemand       ! Cooling Latent Demand [W]
  INTEGER,   INTENT(IN) :: CompOp             ! compressor operation flag
  REAL(r64), INTENT(IN) :: PartLoadRatio      ! compressor part load ratio
  REAL(r64), INTENT(INOUT) :: RuntimeFrac     ! Runtime Fraction of compressor or percent on time (on-time/cycle time)
  REAL(r64), INTENT(IN) :: OnOffAirFlowRatio  ! ratio of compressor on flow to average flow over time step
  REAL(r64), INTENT(IN) :: SpeedRatio         ! SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
  INTEGER, INTENT(IN)  :: SpeedNum            ! Speed number, high bound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: Tref=283.15d0      ! Reference Temperature for performance curves,10C [K]
  CHARACTER(len=*), PARAMETER :: RoutineName='CalcMultiSpeedVarSpeedCoilCooling'


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: Twet_rated             ! Twet at rated conditions (coil air flow rate and air temperatures), sec
  REAL(r64) :: Gamma_rated            ! Gamma at rated conditions (coil air flow rate and air temperatures)

  REAL(r64) :: SHRss                  ! Sensible heat ratio at steady state
  REAL(r64) :: SHReff                 ! Effective sensible heat ratio at part-load condition
  REAL(r64) :: CpSource                ! Specific heat of water [J/kg_C]
  REAL(r64) :: CpAir                  ! Specific heat of air [J/kg_C]
  REAL(r64) :: ReportingConstant

  LOGICAL :: LatDegradModelSimFlag  ! Latent degradation model simulation flag
  INTEGER :: NumIteration           ! Iteration Counter
  INTEGER, SAVE :: Count=0          ! No idea what this is for.
  LOGICAL, SAVE    :: FirstTime = .true.
  REAL(r64), SAVE  :: LoadSideInletDBTemp_Init ! rated conditions
  REAL(r64), SAVE  :: LoadSideInletWBTemp_Init ! rated conditions
  REAL(r64), SAVE  :: LoadSideInletHumRat_Init ! rated conditions
  REAL(r64), SAVE  :: LoadSideInletEnth_Init ! rated conditions
  REAL(r64), SAVE  :: CpAir_Init                ! rated conditions
  REAL(r64)        :: LoadSideInletDBTemp_Unit ! calc conditions for unit
  REAL(r64)        :: LoadSideInletWBTemp_Unit ! calc conditions for unit
  REAL(r64)        :: LoadSideInletHumRat_Unit ! calc conditions for unit
  REAL(r64)        :: LoadSideInletEnth_Unit ! calc conditions for unit
  REAL(r64)        :: CpAir_Unit                ! calc conditions for unit
  REAL(r64) :: AirMassFlowRatio   ! airflow ratio at low speed
  REAL(r64) :: WaterMassFlowRatio   ! airflow ratio at high speed
  REAL(r64) :: TotCapAirFFModFac !air flow fraction modification
  REAL(r64) :: TotCapWaterFFModFac !water flow fraction modification
  REAL(r64) :: TotCapTempModFac !total capacity temperature correctio fraction
  REAL(r64) :: EIRAirFFModFac !air flow fraction modification
  REAL(r64) :: EIRWaterFFModFac !water flow fraction modification
  REAL(r64) :: EIRTempModFac !total capacity temperature correctio fraction
  REAL(r64) :: CBFSpeed !total capacity temperature correctio fraction
  REAL(r64) :: SHR !total capacity temperature correctio fraction
  REAL(r64) :: EIR !total capacity temperature correctio fraction
  INTEGER :: MaxSpeed           ! maximum speed level
  INTEGER :: SpeedCal           ! calculated speed level
  REAL(r64) :: AoEff !effective air side surface area
  REAL(r64) :: QLoadTotal1 !total capacity at low speed
  REAL(r64) :: QLoadTotal2 !total capacity at high speed
  REAL(r64) :: Winput1 !power consumption at low speed
  REAL(r64) :: Winput2 !power consumption at high speed
  REAL(r64) :: QWasteHeat !recoverable waste heat
  REAL(r64) :: QWasteHeat1 !recoverable waste heat at low speed
  REAL(r64) :: QWasteHeat2 !recoverable waste heat at high speed
  REAL(r64) :: PLF  !part-load function
  REAL(r64) :: MaxHumRat  !max possible humidity
  REAL(r64) :: MaxOutletEnth !max possible outlet enthalpy

  ! ADDED VARIABLES FOR air source coil
  REAL(r64)     :: OutdoorCoilT  = 0.0d0              ! Outdoor coil temperature (C)
  REAL(r64)     :: OutdoorCoildw  = 0.0d0             ! Outdoor coil delta w assuming coil temp of OutdoorCoilT (kg/kg)
  REAL(r64)     :: OutdoorDryBulb   = 0.0d0           ! Outdoor dry-bulb temperature at condenser (C)
  REAL(r64)     :: OutdoorWetBulb   = 0.0d0           ! Outdoor wet-bulb temperature at condenser (C)
  REAL(r64)     :: OutdoorHumRat   = 0.0d0            ! Outdoor humidity ratio at condenser (kg/kg)
  REAL(r64)     :: OutdoorPressure   = 0.0d0          ! Outdoor barometric pressure at condenser (Pa)
  REAL(r64)     :: CrankcaseHeatingPower  = 0.0d0     ! power due to crankcase heater
  REAL(r64) :: CompAmbTemp = 0.0d0     ! Ambient temperature at compressor
  REAL(r64) :: CondInletTemp         ! Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
                                 ! Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
  REAL(r64) :: CondInletHumrat       ! Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
                                 ! For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
  REAL(r64) :: CondAirMassFlow       ! Condenser air mass flow rate [kg/s]
  REAL(r64) :: RhoSourceAir         ! Density of air [kg/m3]
  REAL(r64) :: RhoEvapCondWater         ! Density of water used for evaporative condenser [kg/m3]
  REAL(r64) :: EvapCondEffectSped      ! condenser evaporative effectiveness at the speed level
  REAL(r64) :: rhoWater             ! condensed water density
  REAL(r64) :: SpecHumIn   !inlet air specific humidity
  REAL(r64) :: SpecHumOut   !outlet air specific humidity

  IF (FirstTime) THEN
    !Set indoor air conditions to the rated condition
    LoadSideInletDBTemp_Init = 26.7d0
    LoadSideInletHumRat_Init = 0.0111d0
    LoadSideInletEnth_Init = PsyHFnTdbW(LoadSideInletDBTemp_Init,LoadSideInletHumRat_Init,RoutineName//':Init')
    CpAir_Init = PsyCpAirFnWTdb(LoadSideInletHumRat_Init,LoadSideInletDBTemp_Init,RoutineName//':Init')
    FirstTime=.false.
  ENDIF
  LoadSideInletWBTemp_Init = PsyTwbFnTdbWPb(LoadSideInletDBTemp_Init,LoadSideInletHumRat_Init,OutBaroPress,RoutineName)

  MaxSpeed = VarSpeedCoil(DXCoilNum)%NumofSpeeds

 ! must be placed inside the loop, otherwise cause bug in release mode, need to be present at two places
 IF(SpeedNum > MaxSpeed) THEN
   SpeedCal = MaxSpeed
 ELSE
    SpeedCal = SpeedNum
 END IF


 !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  IF (.NOT. (CyclingScheme .EQ. ContFanCycCoil) .AND. PartLoadRatio > 0.0d0) THEN
     VarSpeedCoil(DXCoilNum)%AirMassFlowRate   = Node(VarSpeedCoil(DXCoilNum)%AirInletNodeNum)%MassFlowRate/PartLoadRatio
  END IF

  Twet_rated             = VarSpeedCoil(DXCoilNum)%Twet_rated
  Gamma_rated            = VarSpeedCoil(DXCoilNum)%Gamma_rated

  LoadSideMassFlowRate   = VarSpeedCoil(DXCoilNum)%AirMassFlowRate

  IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
  ! Get condenser outdoor node info from DX COOLING Coil
      IF (VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum /= 0) THEN
        OutdoorDryBulb  = Node(VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum)%Temp
        OutdoorHumRat   = Node(VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum)%HumRat
        OutdoorPressure = Node(VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum)%Press
        OutdoorWetBulb  = Node(VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum)%OutAirWetBulb
      ELSE
        OutdoorDryBulb  = OutDryBulbTemp
        OutdoorHumRat   = OutHumRat
        OutdoorPressure = OutBaroPress
        OutdoorWetBulb  = OutWetBulbTemp
      ENDIF

     RhoSourceAir = PsyRhoAirFnPbTdbW(OutdoorPressure,OutdoorDryBulb,OutdoorHumRat)

     IF((SpeedNum == 1) .OR.(SpeedNum > MaxSpeed).OR. (SpeedRatio == 1.0d0)) THEN
         CondAirMassFlow =  RhoSourceAir * VarSpeedCoil(DXCoilNum)%EvapCondAirFlow(SpeedCal)
     ELSE
         CondAirMassFlow =  RhoSourceAir *(VarSpeedCoil(DXCoilNum)%EvapCondAirFlow(SpeedCal)*SpeedRatio &
            + (1.0d0 - SpeedRatio ) * VarSpeedCoil(DXCoilNum)%EvapCondAirFlow(SpeedCal - 1))
     ENDIF

     ! AIR COOL OR EVAP COOLED CONDENSER
     IF (VarSpeedCoil(DXCoilNum)%CondenserType == EvapCooled) THEN
      IF((SpeedNum == 1) .OR.(SpeedNum > MaxSpeed).OR. (SpeedRatio == 1.0d0)) THEN
         EvapCondEffectSped = VarSpeedCoil(DXCoilNum)%EvapCondEffect(SpeedCal)
      ELSE
         EvapCondEffectSped =  VarSpeedCoil(DXCoilNum)%EvapCondEffect(SpeedCal)*SpeedRatio &
            + (1.0d0 - SpeedRatio ) * VarSpeedCoil(DXCoilNum)%EvapCondEffect(SpeedCal - 1)
      ENDIF
     ! (Outdoor wet-bulb temp from DataEnvironment) + (1.0-EvapCondEffectiveness) * (drybulb - wetbulb)
      CondInletTemp   = OutdoorWetBulb + (OutdoorDryBulb-OutdoorWetBulb)*(1.0d0 - EvapCondEffectSped)
      CondInletHumrat = PsyWFnTdbTwbPb(CondInletTemp,OutdoorWetBulb,OutdoorPressure)
      CompAmbTemp     = CondInletTemp
     ELSE !AIR COOLED CONDENSER
      CondInletTemp   = OutdoorDryBulb ! Outdoor dry-bulb temp
      CompAmbTemp     = OutdoorDryBulb
      CondInletHumrat = OutHumRat
     END IF

     SourceSideMassFlowRate = CondAirMassFlow
     SourceSideInletTemp    = CondInletTemp
     SourceSideInletEnth    = PsyHFnTdbW(CondInletTemp,CondInletHumrat,RoutineName)
     CpSource               = PsyCpAirFnWTdb(CondInletHumrat,CondInletTemp,RoutineName)
     VarSpeedCoil(DXCoilNum)%CondInletTemp = CondInletTemp

    ! If used in a heat pump, the value of MaxOAT in the heating coil overrides that in the cooling coil (in GetInput)
     ! Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
     IF (OutdoorDryBulb .LT. VarSpeedCoil(DXCoilNum)%MaxOATCrankcaseHeater)THEN
       CrankcaseHeatingPower = VarSpeedCoil(DXCoilNum)%CrankcaseHeaterCapacity
     ELSE
       CrankcaseHeatingPower = 0.0d0
     END IF
  ELSE
      SourceSideMassFlowRate = VarSpeedCoil(DXCoilNum)%WaterMassFlowRate
      SourceSideInletTemp    = VarSpeedCoil(DXCoilNum)%InletWaterTemp
      SourceSideInletEnth    = VarSpeedCoil(DXCoilNum)%InletWaterEnthalpy
      CpSource = GetSpecificHeatGlycol(PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidName, &
                                       SourceSideInletTemp, &
                                       PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidIndex,  &
                                      'CalcVSHPCoolingSimple:SourceSideInletTemp')
  END IF

   !Check for flows, do not perform simulation if no flow in load side or source side.
  IF (SourceSideMassFlowRate <= 0.0d0 .OR. LoadSideMassFlowRate <= 0.0d0)THEN
    
     IF((VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum  == Coil_CoolingAirToAirVariableSpeed) &
        .AND. (VarSpeedCoil(DXCoilNum)%CondenserType == AirCooled) & 
         .AND. (LoadSideMassFlowRate > 0.0d0)) THEN    
        !ALLOW SIMULATION IF AIR-COOLED CONDENSER COIL
         VarSpeedCoil(DXCoilNum)%SimFlag = .TRUE.         
     ELSE         
        VarSpeedCoil(DXCoilNum)%SimFlag = .FALSE.
        RETURN
     END IF
  ELSE
     VarSpeedCoil(DXCoilNum)%SimFlag = .TRUE.
  ENDIF

  IF (CompOp .EQ. 0) THEN
     VarSpeedCoil(DXCoilNum)%SimFlag = .FALSE.
     RETURN
  ENDIF

  !Loop the calculation at least once depending whether the latent degradation model
  !is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
  !and 2nd iteration to calculate the  QLatent(actual)
  IF((PartLoadRatio < 1d-10) .OR. (Twet_rated .LE. 0.0d0) .OR. (Gamma_rated .LE. 0.0d0) &
     .OR. (SpeedNum > 1.0d0)) THEN
    LatDegradModelSimFlag = .FALSE.
    !Set NumIteration=1 so that latent model would quit after 1 simulation with the actual condition
    NumIteration=1
  ELSE
    LatDegradModelSimFlag = .TRUE.
    !Set NumIteration=0 so that latent model would simulate twice with rated and actual condition
    NumIteration=0
  END IF


  !Set indoor air conditions to the actual condition
  LoadSideInletDBTemp_Unit = VarSpeedCoil(DXCoilNum)%InletAirDBTemp
  LoadSideInletHumRat_Unit = VarSpeedCoil(DXCoilNum)%InletAirHumRat
  LoadSideInletWBTemp_Unit = PsyTwbFnTdbWPb(LoadSideInletDBTemp_Unit,LoadSideInletHumRat_Unit,OutBaroPress,RoutineName)
  LoadSideInletEnth_Unit = VarSpeedCoil(DXCoilNum)%InletAirEnthalpy
  CpAir_Unit = PsyCpAirFnWTdb(LoadSideInletHumRat_Unit,LoadSideInletDBTemp_Unit)

  RuntimeFrac = 1.0d0
  VarSpeedCoil(DXCoilNum)%RunFrac = 1.0d0
  IF((SpeedNum == 1) .AND. (PartLoadRatio < 1.0d0)) THEN
    PLF = CurveValue(VarSpeedCoil(DXCoilNum)%PLFFPLR,PartLoadRatio)
    IF (PLF < 0.7d0) THEN
     PLF = 0.7d0
    END IF
    ! calculate the run time fraction
    VarSpeedCoil(DXCoilNum)%RunFrac = PartLoadRatio / PLF
    VarSpeedCoil(DXCoilNum)%PartLoadRatio    = PartLoadRatio

    IF ( VarSpeedCoil(DXCoilNum)%RunFrac > 1.0d0 ) THEN
      VarSpeedCoil(DXCoilNum)%RunFrac = 1.0d0 ! Reset coil runtime fraction to 1.0
    ELSE IF ( VarSpeedCoil(DXCoilNum)%RunFrac < 0.0d0 ) THEN
      VarSpeedCoil(DXCoilNum)%RunFrac = 0.0d0
    END IF

    RuntimeFrac = VarSpeedCoil(DXCoilNum)%RunFrac
  END IF


LOOP: DO
    NumIteration=NumIteration+1
    IF (NumIteration.EQ.1) THEN
    !Set indoor air conditions to the rated conditions
        LoadSideInletDBTemp = LoadSideInletDBTemp_Init
        LoadSideInletHumRat = LoadSideInletHumRat_Init
        LoadSideInletWBTemp = LoadSideInletWBTemp_Init
        LoadSideInletEnth = LoadSideInletEnth_Init
        CpAir = CpAir_Init
    ELSE
    !Set indoor air conditions to the actual condition
        LoadSideInletDBTemp = LoadSideInletDBTemp_Unit
        LoadSideInletHumRat = LoadSideInletHumRat_Unit
        LoadSideInletWBTemp = LoadSideInletWBTemp_Unit
        LoadSideInletEnth = LoadSideInletEnth_Unit
        CpAir = CpAir_Unit
    END IF

    ! must be placed inside the loop, otherwise cause bug in release mode
    IF(SpeedNum > MaxSpeed) THEN
       SpeedCal = MaxSpeed
    ELSE
       SpeedCal = SpeedNum
    END IF

    IF((SpeedNum == 1) .OR.(SpeedNum > MaxSpeed).OR. (SpeedRatio == 1.0d0)) THEN
        AirMassFlowRatio = LoadSideMassFlowRate/ VarSpeedCoil(DXCoilNum)%DesignAirMassFlowRate

        IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
            WaterMassFlowRatio = 1.0d0
        ELSE
            WaterMassFlowRatio = SourceSideMassFlowRate/VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate
        END IF

        EIRTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletWBTemp,SourceSideInletTemp)
        EIRAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)

        IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
            EIRWaterFFModFac = 1.0d0
        ELSE
            EIRWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
        END IF

        EIR = (1.0/VarSpeedCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac

        CBFSpeed  = AdjustCBF(VarSpeedCoil(DXCoilNum)%MSRatedCBF(SpeedCal),&
                VarSpeedCoil(DXCoilNum)%MSRatedAirMassFlowRate(SpeedCal),LoadSideMassFlowRate)

        IF(CBFSpeed > 0.999d0) CBFSpeed = 0.999d0

        CALL CalcTotCapSHR_VSWSHP(LoadSideInletDBTemp,LoadSideInletHumRat,LoadSideInletEnth,LoadSideInletWBTemp, &
                         AirMassFlowRatio, WaterMassFlowRatio, &
                         LoadSideMassFlowRate,CBFSpeed, &
                         VarSpeedCoil(DXCoilNum)%MSRatedTotCap(SpeedCal),VarSpeedCoil(DXCoilNum)%MSCCapFTemp(SpeedCal), &
                         VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal), VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),&
                         0.0d0,0,0, 0,&
                         QLoadTotal1, QLoadTotal2, QLoadTotal,SHR,SourceSideInletTemp, &
                         VarSpeedCoil(DXCoilNum)%InletAirPressure, 0.0d0, 1)

        Winput = QLoadTotal * EIR

        IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
            QWasteHeat = 0.0d0
        ELSE
            QWasteHeat =  Winput * VarSpeedCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
            QWasteHeat = QWasteHeat * CurveValue(VarSpeedCoil(DXCoilNum)%MSWasteHeat(SpeedCal),LoadSideInletWBTemp, &
                        SourceSideInletTemp)
        END IF
    ELSE
        AirMassFlowRatio = LoadSideMassFlowRate/ VarSpeedCoil(DXCoilNum)%DesignAirMassFlowRate

        IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
            WaterMassFlowRatio = 1.0d0
        ELSE
            WaterMassFlowRatio = SourceSideMassFlowRate/VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate
        END IF

        AoEff = VarSpeedCoil(DXCoilNum)%MSEffectiveAo(SpeedCal)*SpeedRatio &
            + (1.0d0 - SpeedRatio ) * VarSpeedCoil(DXCoilNum)%MSEffectiveAo(SpeedCal - 1)

        CBFSpeed  = exp(-AoEff/LoadSideMassFlowRate)

        IF(CBFSpeed > 0.999d0) CBFSpeed = 0.999d0

        CALL CalcTotCapSHR_VSWSHP(LoadSideInletDBTemp,LoadSideInletHumRat,LoadSideInletEnth,LoadSideInletWBTemp, &
                 AirMassFlowRatio, WaterMassFlowRatio, &
                 LoadSideMassFlowRate,CBFSpeed, &
                 VarSpeedCoil(DXCoilNum)%MSRatedTotCap(SpeedCal - 1),VarSpeedCoil(DXCoilNum)%MSCCapFTemp(SpeedCal - 1), &
                 VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal - 1), VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal - 1),&
                 VarSpeedCoil(DXCoilNum)%MSRatedTotCap(SpeedCal),VarSpeedCoil(DXCoilNum)%MSCCapFTemp(SpeedCal), &
                 VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal), VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),&
                 QLoadTotal1, QLoadTotal2, QLoadTotal,SHR,SourceSideInletTemp, &
                 VarSpeedCoil(DXCoilNum)%InletAirPressure, SpeedRatio, 2)

        SpeedCal =  SpeedNum - 1
        EIRTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletWBTemp,SourceSideInletTemp)
        EIRAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)

        IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
            EIRWaterFFModFac = 1.0d0
        ELSE
            EIRWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
        END IF

        EIR = (1.0/VarSpeedCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
        Winput1 = QLoadTotal1 * EIR

        IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
            QWasteHeat1 = 0.0d0
        ELSE
            QWasteHeat1 =  Winput1 * VarSpeedCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
            QWasteHeat1 = QWasteHeat1 * CurveValue(VarSpeedCoil(DXCoilNum)%MSWasteHeat(SpeedCal), &
                LoadSideInletWBTemp,SourceSideInletTemp)
        END IF

        SpeedCal =  SpeedNum
        EIRTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletWBTemp,SourceSideInletTemp)
        EIRAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)

        IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
            EIRWaterFFModFac = 1.0d0
        ELSE
            EIRWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
        END IF

        EIR = (1.0/VarSpeedCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
        Winput2 = QLoadTotal2 * EIR

        IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
            QWasteHeat2 = 0.0d0
        ELSE
            QWasteHeat2 =  Winput2 * VarSpeedCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
            QWasteHeat2 = QWasteHeat2 * CurveValue(VarSpeedCoil(DXCoilNum)%MSWasteHeat(SpeedCal), &
                    LoadSideInletWBTemp,SourceSideInletTemp)
        END IF

        Winput = Winput2*SpeedRatio + (1.0d0 - SpeedRatio ) * Winput1
        QWasteHeat = QWasteHeat2*SpeedRatio + (1.0d0 - SpeedRatio ) * QWasteHeat1
    END IF

    QSensible = QLoadTotal * SHR

    Qsource =  QLoadTotal + Winput - QWasteHeat

    IF(Qsource < 0) THEN
      Qsource = 0.0d0
      QWasteHeat =  QLoadTotal + Winput
    END IF

  !Check if the Sensible Load is greater than the Total Cooling Load
    IF(QSensible.GT.QLoadTotal) THEN
      QSensible = QLoadTotal
    END IF

    IF(LatDegradModelSimFlag) THEN
  !Calculate for SHReff using the Latent Degradation Model
      IF(NumIteration.EQ.1) THEN
          QLatRated=QLoadTotal-QSensible
      ELSEIF(NumIteration.EQ.2) THEN
          QLatActual=QLoadTotal-QSensible
          SHRss=QSensible/QLoadTotal
          SHReff = CalcEffectiveSHR(DXCoilNum, SHRss,CyclingScheme, RuntimeFrac, &
                 QLatRated, QLatActual, LoadSideInletDBTemp, LoadSideInletWBTemp)
!       Update sensible capacity based on effective SHR
          QSensible = QLoadTotal * SHReff
          EXIT LOOP
      END IF
    ELSE
  !Assume SHReff=SHRss
      SHReff = QSensible/QLoadTotal
      EXIT LOOP
    END IF
  END DO LOOP

  ! considering hot gas reheat here
  IF(VarSpeedCoil(DXCoilNum)%HOTGASREHEATFLG > 0) THEN
    QLoadTotal = QLoadTotal - QWasteHeat
    QSensible = QSensible -QWasteHeat
    SHReff = QSensible/QLoadTotal
  END IF

  VarSpeedCoil(DXCoilNum)%BasinHeaterPower = 0.0d0
  VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower = 0.0d0

  IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
      IF (VarSpeedCoil(DXCoilNum)%CondenserType == EvapCooled) THEN
          !******************
          !             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
          !             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
          !                                /RhoWater [kg H2O/m3 H2O]
          !******************
             RhoEvapCondWater = RhoH2O(OutdoorDryBulb)
             VarSpeedCoil(DXCoilNum)%EvapWaterConsumpRate =  (CondInletHumrat - OutdoorHumRat) *  &
                      CondAirMassFlow/RhoEvapCondWater * RuntimeFrac
             VarSpeedCoil(DXCoilNum)%EvapCondPumpElecPower = VarSpeedCoil(DXCoilNum)%EvapCondPumpElecNomPower * &
                                                       RuntimeFrac
          ! Calculate basin heater power
            CALL CalcBasinHeaterPower(VarSpeedCoil(DXCoilNum)%BasinHeaterPowerFTempDiff,&
                                      VarSpeedCoil(DXCoilNum)%BasinHeaterSchedulePtr,&
                                      VarSpeedCoil(DXCoilNum)%BasinHeaterSetPointTemp,VarSpeedCoil(DXCoilNum)%BasinHeaterPower)
           VarSpeedCoil(DXCoilNum)%BasinHeaterPower = VarSpeedCoil(DXCoilNum)%BasinHeaterPower * &
                                                 (1.d0 - RuntimeFrac)
       END IF

       VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower = CrankcaseHeatingPower *(1.0d0 - RuntimeFrac)

        !set water system demand request (if needed)
       IF ( VarSpeedCoil(DXCoilNum)%EvapWaterSupplyMode == WaterSupplyFromTank) THEN
           WaterStorage(VarSpeedCoil(DXCoilNum)%EvapWaterSupTankID)%VdotRequestDemand(&
           VarSpeedCoil(DXCoilNum)%EvapWaterTankDemandARRID)= VarSpeedCoil(DXCoilNum)%EvapWaterConsumpRate
       ENDIF

  END IF

  !calculate coil outlet state variables
  LoadSideOutletEnth   = LoadSideInletEnth - QLoadTotal/LoadSideMassFlowRate
  LoadSideOutletDBTemp = LoadSideInletDBTemp - QSensible/(LoadSideMassFlowRate * CpAir)

  MaxHumRat = PsyWFnTdbRhPb(LoadSideOutletDBTemp,0.9999d0,VarSpeedCoil(DXCoilNum)%InletAirPressure,RoutineName)
  MaxOutletEnth = PsyHFnTdbW(LoadSideOutletDBTemp,MaxHumRat,RoutineName)
  IF(LoadSideOutletEnth > MaxOutletEnth) THEN
    LoadSideOutletEnth = MaxOutletEnth
    !QLoadTotal = LoadSideMassFlowRate * (LoadSideInletEnth - LoadSideOutletEnth)
  END IF
  LoadsideOutletHumRat = PsyWFnTdbH(LoadSideOutletDBTemp,LoadSideOutletEnth,RoutineName)
  IF(LoadsideOutletHumRat > MaxHumRat) THEN
    LoadsideOutletHumRat = MaxHumRat
  END IF

  Count = Count + 1
  !Actual outlet conditions are "average" for time step
  IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
    VarSpeedCoil(DXCoilNum)%OutletAirEnthalpy = PartLoadRatio*LoadSideOutletEnth + &
                                                  (1.0d0-PartLoadRatio)*LoadSideInletEnth
    VarSpeedCoil(DXCoilNum)%OutletAirHumRat   = PartLoadRatio*LoadsideOutletHumRat + &
                                                  (1.0d0-PartLoadRatio)*LoadSideInletHumRat
    VarSpeedCoil(DXCoilNum)%OutletAirDBTemp   = PsyTdbFnHW(VarSpeedCoil(DXCoilNum)%OutletAirEnthalpy,  &
                                                             VarSpeedCoil(DXCoilNum)%OutletAirHumRat,    &
                                                             RoutineName)
    PLRCorrLoadSideMdot = LoadSideMassFlowRate
  ELSE
    ! default to cycling fan, cycling compressor
    VarSpeedCoil(DXCoilNum)%OutletAirEnthalpy = LoadSideOutletEnth
    VarSpeedCoil(DXCoilNum)%OutletAirHumRat   = LoadsideOutletHumRat
    VarSpeedCoil(DXCoilNum)%OutletAirDBTemp   = LoadSideOutletDBTemp
    PLRCorrLoadSideMdot = LoadSideMassFlowRate*PartLoadRatio
  END IF

   ! scale heat transfer rates to PLR and power to RTF
  QLoadTotal = QLoadTotal*PartLoadRatio
  QSensible  = QSensible*PartLoadRatio
  ! count the powr separately
  Winput     = Winput*RuntimeFrac  !+ VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower &
                !+ VarSpeedCoil(DXCoilNum)%BasinHeaterPower + VarSpeedCoil(DXCoilNum)%EvapCondPumpElecPower
  QSource    = QSource*PartLoadRatio
  QWasteHeat = QWasteHeat * PartLoadRatio

!  Add power to global variable so power can be summed by parent object
  DXElecCoolingPower = Winput

  ReportingConstant=TimeStepSys*SecInHour
  !Update heat pump data structure
  VarSpeedCoil(DXCoilNum)%Power               = Winput
  VarSpeedCoil(DXCoilNum)%QLoadTotal          = QLoadTotal
  VarSpeedCoil(DXCoilNum)%QSensible           = QSensible
  VarSpeedCoil(DXCoilNum)%QLatent             = QLoadTotal - QSensible
  VarSpeedCoil(DXCoilNum)%QSource             = QSource
  VarSpeedCoil(DXCoilNum)%Energy=Winput*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergyLoadTotal=QLoadTotal*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergySensible=QSensible*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergyLatent=(QLoadTotal - QSensible)*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergySource=QSource*ReportingConstant
  VarSpeedCoil(DXCoilNum)%CrankcaseHeaterConsumption = VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EvapWaterConsump = VarSpeedCoil(DXCoilNum)%EvapWaterConsumpRate*ReportingConstant
  VarSpeedCoil(DXCoilNum)%BasinHeaterConsumption = VarSpeedCoil(DXCoilNum)%BasinHeaterPower*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EvapCondPumpElecConsumption = VarSpeedCoil(DXCoilNum)%EvapCondPumpElecPower*ReportingConstant
  IF(RunTimeFrac == 0.0d0) THEN
    VarSpeedCoil(DXCoilNum)%COP = 0.0d0
  ELSE
    VarSpeedCoil(DXCoilNum)%COP = QLoadTotal/Winput
  END IF
  VarSpeedCoil(DXCoilNum)%RunFrac             = RuntimeFrac
  VarSpeedCoil(DXCoilNum)%PartLoadRatio       = PartLoadRatio
  VarSpeedCoil(DXCoilNum)%AirMassFlowRate     = PLRCorrLoadSideMdot

  IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_CoolingAirToAirVariableSpeed) THEN
      VarSpeedCoil(DXCoilNum)%WaterMassFlowRate   = 0.0d0
      VarSpeedCoil(DXCoilNum)%OutletWaterTemp     = 0.0d0
      VarSpeedCoil(DXCoilNum)%OutletWaterEnthalpy = 0.0d0
  ELSE
      VarSpeedCoil(DXCoilNum)%WaterMassFlowRate   = SourceSideMassFlowRate
      VarSpeedCoil(DXCoilNum)%OutletWaterTemp     = SourceSideInletTemp + QSource/(SourceSideMassFlowRate * CpSource)
      VarSpeedCoil(DXCoilNum)%OutletWaterEnthalpy = SourceSideInletEnth + QSource/SourceSideMassFlowRate
  END IF

  VarSpeedCoil(DXCoilNum)%QWasteHeat =  QWasteHeat

  IF (VarSpeedCoil(DXCoilNum)%CondensateCollectMode == CondensateToTank) THEN
      ! calculate and report condensation rates  (how much water extracted from the air stream)
      ! water flow of water in m3/s for water system interactions
      rhoWater = RhoH2O(( VarSpeedCoil(DXCoilNum)%InletAirDBTemp + VarSpeedCoil(DXCoilNum)%OutletAirDBTemp)/2.0d0)
!     CR9155 Remove specific humidity calculations
      SpecHumIn = LoadSideInletHumRat
      SpecHumOut = LoadSideOutletHumRat
      !  mdot * del HumRat / rho water
      VarSpeedCoil(DXCoilNum)%CondensateVdot = MAX(0.0d0, (LoadSideMassFlowRate *   &
                (SpecHumIn - SpecHumOut) / rhoWater) )
      VarSpeedCoil(DXCoilNum)%CondensateVol  = VarSpeedCoil(DXCoilNum)%CondensateVdot *ReportingConstant
  ENDIF

  RETURN
END SUBROUTINE CalcVarSpeedCoilCooling

SUBROUTINE  CalcVarSpeedCoilHeating(DXCoilNum,CyclingScheme,RuntimeFrac, &
        SensDemand,CompOp,PartLoadRatio,OnOffAirFlowRatio, SpeedRatio, SpeedNum)


          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:CalcHPHeatingSimple
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for simulating the heating mode of the Variable Speed Water to Air HP Simple

          ! METHODOLOGY EMPLOYED:
          ! Simulate the heat pump performance using the coefficients and rated conditions
          !
          ! Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
          ! and RuntimeFrac.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals,      ONLY:TimeStepSys, DXElecHeatingPower
  USE Psychrometrics,       ONLY:PsyWFnTdbTwbPb,PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyTwbFnTdbWPb,  &
                                 PsyTdbFnHW,PsyWFnTdbH,PsyHFnTdbW
  USE FluidProperties,      ONLY:GetSpecificHeatGlycol
  USE DataPlant,            ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER,   INTENT(IN) :: DXCoilNum              ! Heat Pump Number
  INTEGER,   INTENT(IN) :: CyclingScheme      ! Fan/Compressor cycling scheme indicator
  REAL(r64), INTENT(IN) :: SensDemand         ! Cooling Sensible Demand [W] !unused1208
  INTEGER,   INTENT(IN) :: CompOp             ! compressor operation flag
  REAL(r64), INTENT(IN) :: PartLoadRatio      ! compressor part load ratio
  REAL(r64), INTENT(INOUT) :: RuntimeFrac        ! Runtime Fraction of compressor or percent on time (on-time/cycle time)
  REAL(r64), INTENT(IN) :: OnOffAirFlowRatio  ! ratio of compressor on flow to average flow over time step
  REAL(r64), INTENT(IN) :: SpeedRatio        ! SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
  INTEGER, INTENT(IN)  :: SpeedNum      ! Speed number, high bound, i.e. SpeedNum - 1 is the other side

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: Tref=283.15d0      ! Reference Temperature for performance curves,10C [K]
  CHARACTER(len=*), PARAMETER :: RoutineName='CalcVarSpeedCoilHeating'


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CpSource                ! Specific heat of water [J/kg_C]
  REAL(r64) :: CpAir                  ! Specific heat of air [J/kg_C]

  REAL(r64) :: AirMassFlowRatio   ! airflow ratio at low speed
  REAL(r64) :: WaterMassFlowRatio   ! airflow ratio at high speed
  REAL(r64) :: TotCapAirFFModFac !air flow fraction modification
  REAL(r64) :: TotCapWaterFFModFac !water flow fraction modification
  REAL(r64) :: TotCapTempModFac !total capacity temperature correctio fraction
  REAL(r64) :: EIRAirFFModFac !air flow fraction modification
  REAL(r64) :: EIRWaterFFModFac !water flow fraction modification
  REAL(r64) :: EIRTempModFac !total capacity temperature correctio fraction
  REAL(r64) :: EIR !total capacity temperature correctio fraction
  INTEGER :: MaxSpeed           ! maximum speed level
  INTEGER :: SpeedCal           ! calculated speed level
  REAL(r64) :: QLoadTotal1      ! heating capacit at low speed
  REAL(r64) :: QLoadTotal2      ! heating capacity at high speed
  REAL(r64) :: Winput1          ! power consumption at low speed
  REAL(r64) :: Winput2          ! power consumption at high speed
  REAL(r64) :: QWasteHeat       !recoverable waste heat
  REAL(r64) :: QWasteHeat1      !recoverable waste heat at low speed
  REAL(r64) :: QWasteHeat2      !recoverable waste heat at high speed
  REAL(r64) :: PLF  !part-load function
  REAL(r64) :: ReportingConstant

 ! ADDED VARIABLES FOR air source coil
  REAL(r64)     :: OutdoorCoilT  = 0.0d0              ! Outdoor coil temperature (C)
  REAL(r64)     :: OutdoorCoildw  = 0.0d0             ! Outdoor coil delta w assuming coil temp of OutdoorCoilT (kg/kg)
  REAL(r64)     :: OutdoorDryBulb   = 0.0d0           ! Outdoor dry-bulb temperature at condenser (C)
  REAL(r64)     :: OutdoorWetBulb   = 0.0d0           ! Outdoor wet-bulb temperature at condenser (C)
  REAL(r64)     :: OutdoorHumRat   = 0.0d0            ! Outdoor humidity ratio at condenser (kg/kg)
  REAL(r64)     :: OutdoorPressure   = 0.0d0          ! Outdoor barometric pressure at condenser (Pa)
  REAL(r64)     :: FractionalDefrostTime  = 0.0d0     ! Fraction of time step system is in defrost
  REAL(r64)     :: HeatingCapacityMultiplier  = 0.0d0 ! Multiplier for heating capacity when system is in defrost
  REAL(r64)     :: InputPowerMultiplier   = 0.0d0     ! Multiplier for power when system is in defrost
  REAL(r64)     :: LoadDueToDefrost   = 0.0d0         ! Additional load due to defrost
  REAL(r64)     :: CrankcaseHeatingPower  = 0.0d0     ! power due to crankcase heater
  REAL(r64)     :: DefrostEIRTempModFac   = 0.0d0     ! EIR modifier for defrost (function of entering wetbulb, outside drybulb)
  REAL(r64)     :: TotRatedCapacity   = 0.0d0         ! total rated capacity at the given speed and speed ratio for defrosting 

  MaxSpeed = VarSpeedCoil(DXCoilNum)%NumofSpeeds

 !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  IF (.NOT. (CyclingScheme .EQ. ContFanCycCoil) .AND. PartLoadRatio > 0.0d0) THEN
     VarSpeedCoil(DXCoilNum)%AirMassFlowRate   = Node(VarSpeedCoil(DXCoilNum)%AirInletNodeNum)%MassFlowRate/PartLoadRatio
  END IF

  LoadSideMassFlowRate   = VarSpeedCoil(DXCoilNum)%AirMassFlowRate
  LoadSideInletDBTemp    = VarSpeedCoil(DXCoilNum)%InletAirDBTemp
  LoadSideInletHumRat    = VarSpeedCoil(DXCoilNum)%InletAirHumRat

  LoadSideInletWBTemp    = PsyTwbFnTdbWPb(LoadSideInletDBTemp,LoadSideInletHumRat,OutBaroPress,RoutineName)
  LoadSideInletEnth      = VarSpeedCoil(DXCoilNum)%InletAirEnthalpy
  CpAir                  = PsyCpAirFnWTdb(LoadSideInletHumRat,LoadSideInletDBTemp,RoutineName)

  IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
    ! Get condenser outdoor node info from DX Heating Coil
      IF (VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum /= 0) THEN
        OutdoorDryBulb  = Node(VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum)%Temp
        OutdoorHumRat   = Node(VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum)%HumRat
        OutdoorPressure = Node(VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum)%Press
        OutdoorWetBulb  = Node(VarSpeedCoil(DXCoilNum)%CondenserInletNodeNum)%OutAirWetBulb
      ELSE
        OutdoorDryBulb  = OutDryBulbTemp
        OutdoorHumRat   = OutHumRat
        OutdoorPressure = OutBaroPress
        OutdoorWetBulb  = OutWetBulbTemp
      ENDIF
      SourceSideMassFlowRate = 1.0d0 ! not used and avoid divided by zero
      SourceSideInletTemp    = OutdoorDryBulb
      SourceSideInletEnth    = PsyHFnTdbW(OutdoorDryBulb,OutdoorHumRat,RoutineName)
      CpSource               = PsyCpAirFnWTdb(OutHumRat, OutdoorDryBulb,RoutineName)

      ! Initialize crankcase heater, operates below OAT defined in input deck for HP DX heating coil
     IF (OutdoorDryBulb .LT. VarSpeedCoil(DXCoilNum)%MaxOATCrankcaseHeater)THEN
       CrankcaseHeatingPower = VarSpeedCoil(DXCoilNum)%CrankcaseHeaterCapacity
     ELSE
       CrankcaseHeatingPower = 0.0d0
     END IF
  ELSE
      SourceSideMassFlowRate = VarSpeedCoil(DXCoilNum)%WaterMassFlowRate
      SourceSideInletTemp    = VarSpeedCoil(DXCoilNum)%InletWaterTemp
      SourceSideInletEnth    = VarSpeedCoil(DXCoilNum)%InletWaterEnthalpy
      CpSource               = GetSpecificHeatGlycol(PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidName, &
                                       SourceSideInletTemp, &
                                       PlantLoop(VarSpeedCoil(DXCoilNum)%LoopNum)%FluidIndex,  &
                                       RoutineName//':SourceSideInletTemp')
  END IF

 !Check for flows, do not perform simulation if no flow in load side or source side.
  IF ((SourceSideMassFlowRate <= 0.0d0) .OR. (LoadSideMassFlowRate <= 0.0d0) )THEN
    VarSpeedCoil(DXCoilNum)%SimFlag = .FALSE.
    RETURN
  ELSE
    VarSpeedCoil(DXCoilNum)%SimFlag = .TRUE.
  ENDIF

  IF((VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) &
     .AND. (OutdoorDryBulb < VarSpeedCoil(DXCoilNum)%MinOATCompressor))  THEN
    VarSpeedCoil(DXCoilNum)%SimFlag = .FALSE.
    RETURN
  END IF

  IF (CompOp .EQ. 0) THEN
    VarSpeedCoil(DXCoilNum)%SimFlag = .FALSE.
    RETURN
  ENDIF

  IF(SpeedNum > MaxSpeed) THEN
      SpeedCal = MaxSpeed
  ELSE
      SpeedCal = SpeedNum
  END IF

  RuntimeFrac = 1.0d0
  VarSpeedCoil(DXCoilNum)%RunFrac = 1.0d0
  IF((SpeedNum == 1) .AND. (PartLoadRatio < 1.0d0)) THEN
    PLF = CurveValue(VarSpeedCoil(DXCoilNum)%PLFFPLR,PartLoadRatio)
    IF (PLF < 0.7d0) THEN
     PLF = 0.7d0
    END IF
    ! calculate the run time fraction
    VarSpeedCoil(DXCoilNum)%RunFrac = PartLoadRatio / PLF
    VarSpeedCoil(DXCoilNum)%PartLoadRatio    = PartLoadRatio

    IF ( VarSpeedCoil(DXCoilNum)%RunFrac > 1.0d0 ) THEN
      VarSpeedCoil(DXCoilNum)%RunFrac = 1.0d0 ! Reset coil runtime fraction to 1.0
    ELSE IF( VarSpeedCoil(DXCoilNum)%RunFrac < 0.0d0 ) THEN
      VarSpeedCoil(DXCoilNum)%RunFrac = 0.0d0
    END IF

    RuntimeFrac = VarSpeedCoil(DXCoilNum)%RunFrac
  END IF

  IF((SpeedNum == 1) .OR.(SpeedNum > MaxSpeed) .OR. (SpeedRatio == 1.0d0)) THEN
    AirMassFlowRatio = LoadSideMassFlowRate/ VarSpeedCoil(DXCoilNum)%DesignAirMassFlowRate

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        WaterMassFlowRatio = 1.0d0
    ELSE
        WaterMassFlowRatio = SourceSideMassFlowRate/VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate
    END IF

    TotCapTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    TotCapAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal),AirMassFlowRatio)

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        TotCapWaterFFModFac = 1.0d0
    ELSE
        TotCapWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),WaterMassFlowRatio)
    END IF

    QLoadTotal = VarSpeedCoil(DXCoilNum)%MSRatedTotCap(SpeedCal) * TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac
    TotRatedCapacity = VarSpeedCoil(DXCoilNum)%MSRatedTotCap(SpeedCal) ! for defrosting power cal

    EIRTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    EIRAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        EIRWaterFFModFac = 1.0D0
    ELSE
        EIRWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
    END IF

    EIR = (1.0/VarSpeedCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
    Winput = QLoadTotal * EIR

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        QWasteHeat = 0.0d0
    ELSE
        QWasteHeat =  Winput * VarSpeedCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
        QWasteHeat = QWasteHeat * CurveValue(VarSpeedCoil(DXCoilNum)%MSWasteHeat(SpeedCal), &
                    LoadSideInletDBTemp,SourceSideInletTemp)
    END IF

  ELSE
    AirMassFlowRatio = LoadSideMassFlowRate/ VarSpeedCoil(DXCoilNum)%DesignAirMassFlowRate

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        WaterMassFlowRatio = 1.0d0
    ELSE
        WaterMassFlowRatio = SourceSideMassFlowRate/VarSpeedCoil(DXCoilNum)%DesignWaterMassFlowRate
    END IF

    SpeedCal =  SpeedNum - 1
    TotCapTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    TotCapAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal),AirMassFlowRatio)

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        TotCapWaterFFModFac = 1.0d0
    ELSE
        TotCapWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),WaterMassFlowRatio)
    END IF

    QLoadTotal1 = VarSpeedCoil(DXCoilNum)%MSRatedTotCap(SpeedCal) * TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac

    EIRTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    EIRAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        EIRWaterFFModFac = 1.0d0
    ELSE
        EIRWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
    END IF

    EIR = (1.0/VarSpeedCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
    Winput1 = QLoadTotal1 * EIR

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        QWasteHeat1 = 0.0d0
    ELSE
        QWasteHeat1 =  Winput1 * VarSpeedCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
        QWasteHeat1 = QWasteHeat1 * CurveValue(VarSpeedCoil(DXCoilNum)%MSWasteHeat(SpeedCal), &
                LoadSideInletDBTemp,SourceSideInletTemp)
    END IF

    SpeedCal =  SpeedNum
    TotCapTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    TotCapAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal),AirMassFlowRatio)

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        TotCapWaterFFModFac = 1.0d0
    ELSE
        TotCapWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),WaterMassFlowRatio)
    END IF

    QLoadTotal2 = VarSpeedCoil(DXCoilNum)%MSRatedTotCap(SpeedCal) * TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac

    EIRTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    EIRAirFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        EIRWaterFFModFac = 1.0d0
    ELSE
        EIRWaterFFModFac = CurveValue(VarSpeedCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
    END IF

    EIR = (1.0/VarSpeedCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
    Winput2 = QLoadTotal2 * EIR

    IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
        QWasteHeat2 = 0.0d0
    ELSE
        QWasteHeat2 =  Winput2 * VarSpeedCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
        QWasteHeat2 = QWasteHeat2 * CurveValue(VarSpeedCoil(DXCoilNum)%MSWasteHeat(SpeedCal), &
                LoadSideInletDBTemp,SourceSideInletTemp)
    END IF

    QLoadTotal = QLoadTotal2*SpeedRatio + (1.0d0 - SpeedRatio ) * QLoadTotal1
    Winput = Winput2*SpeedRatio + (1.0d0 - SpeedRatio ) * Winput1
    QWasteHeat = QWasteHeat2*SpeedRatio + (1.0d0 - SpeedRatio ) * QWasteHeat1
    TotRatedCapacity =VarSpeedCoil(DXCoilNum)%MSRatedTotCap(SpeedCal)*SpeedRatio + (1.0d0 - SpeedRatio ) * &
                       VarSpeedCoil(DXCoilNum)%MSRatedTotCap(SpeedCal - 1) 
  END IF

  VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower = 0.0d0 !necessary to clear zero for water source coils
  VarSpeedCoil(DXCoilNum)%DefrostPower =  0.0d0 !clear the defrost power 
  IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
    ! Calculating adjustment factors for defrost
    ! Calculate delta w through outdoor coil by assuming a coil temp of 0.82*DBT-9.7(F) per DOE2.1E
      OutdoorCoilT = 0.82d0 * OutdoorDryBulb - 8.589d0
      OutdoorCoildw = MAX(1.0d-6,(OutdoorHumRat - PsyWFnTdpPb(OutdoorCoilT,OutdoorPressure)))

    ! Initializing defrost adjustment factors
      LoadDueToDefrost = 0.0d0
      HeatingCapacityMultiplier = 1.0d0
      FractionalDefrostTime = 0.0d0
      InputPowerMultiplier = 1.0d0
    !
    ! Check outdoor temperature to determine of defrost is active
      IF (OutdoorDryBulb .LE. VarSpeedCoil(DXCoilNum)%MaxOATDefrost) THEN
    ! Calculate defrost adjustment factors depending on defrost control type
         IF (VarSpeedCoil(DXCoilNum)%DefrostControl .EQ. Timed) THEN
           FractionalDefrostTime = VarSpeedCoil(DXCoilNum)%DefrostTime
           HeatingCapacityMultiplier = 0.909d0 - 107.33d0 * OutdoorCoildw
           InputPowerMultiplier = 0.90d0 - 36.45d0*OutdoorCoildw
         ELSE !else defrost control is on-demand
           FractionalDefrostTime = 1.0d0 / (1.0d0 + 0.01446d0 / OutdoorCoildw)
           HeatingCapacityMultiplier = 0.875d0 * ( 1.0d0 - FractionalDefrostTime)
           InputPowerMultiplier = 0.954d0 * ( 1.0d0 - FractionalDefrostTime)
         END IF    
         ! correction fractional defrost time shorten by runtime fraction
         FractionalDefrostTime = RuntimeFrac * FractionalDefrostTime
         
         IF (FractionalDefrostTime .GT. 0.0d0) THEN
    ! Calculate defrost adjustment factors depending on defrost control strategy
           IF (VarSpeedCoil(DXCoilNum)%DefrostStrategy .EQ. ReverseCycle) THEN
             LoadDueToDefrost = (0.01d0 * FractionalDefrostTime) * &
                                (7.222d0 - OutdoorDryBulb) * &
                                (TotRatedCapacity/1.01667d0)
             DefrostEIRTempModFac = CurveValue(VarSpeedCoil(DXCoilNum)%DefrostEIRFT,&
                                    MAX(15.555d0,LoadSideInletWBTemp),MAX(15.555d0,OutdoorDryBulb))
             VarSpeedCoil(DXCoilNum)%DefrostPower =  DefrostEIRTempModFac * &
                                               (TotRatedCapacity &
                                               /1.01667d0)* FractionalDefrostTime
           ELSE ! Defrost strategy is resistive
             VarSpeedCoil(DXCoilNum)%DefrostPower = VarSpeedCoil(DXCoilNum)%DefrostCapacity &
                                              * FractionalDefrostTime
           END IF
         ELSE ! Defrost is not active because (OutDryBulbTemp .GT. DXCoil(DXCoilNum)%MaxOATDefrost)
           VarSpeedCoil(DXCoilNum)%DefrostPower =  0.0d0
         END IF
      END IF

      VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower = CrankcaseHeatingPower *(1.0d0 - RuntimeFrac)
    !! Modify total heating capacity based on defrost heating capacity multiplier
    !! MaxHeatCap passed from parent object VRF Condenser and is used to limit capacity of TU's to that available from condenser
    !  IF(PRESENT(MaxHeatCap))THEN
    !    TotCap = MIN(MaxHeatCap,TotCap * HeatingCapacityMultiplier)
    !  ELSE
    !    TotCap = TotCap * HeatingCapacityMultiplier
    !  END IF
      QLoadTotal = QLoadTotal * HeatingCapacityMultiplier - LoadDueToDefrost
      ! count the powr separately
      Winput = Winput * InputPowerMultiplier !+ VarSpeedCoil(DXCoilNum)%DefrostPower

  END IF

  Qsource = QLoadTotal+ QWasteHeat -Winput
  QSensible = QLoadTotal

  IF(Qsource < 0) THEN
    Qsource = 0.0d0
    QWasteHeat =  Winput - QLoadTotal
  END IF

  ! calculate coil outlet state variables
  LoadSideOutletEnth   = LoadSideInletEnth + QLoadTotal/LoadSideMassFlowRate
  LoadSideOutletDBTemp = LoadSideInletDBTemp + QSensible/(LoadSideMassFlowRate * CpAir)
  LoadsideOutletHumRat = PsyWFnTdbH(LoadSideOutletDBTemp,LoadSideOutletEnth,RoutineName)

  ! Actual outlet conditions are "average" for time step
  IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
    VarSpeedCoil(DXCoilNum)%OutletAirEnthalpy = PartLoadRatio*LoadSideOutletEnth + &
                                                  (1.d0-PartLoadRatio)*LoadSideInletEnth
    VarSpeedCoil(DXCoilNum)%OutletAirHumRat   = PartLoadRatio*LoadsideOutletHumRat + &
                                                  (1.d0-PartLoadRatio)*LoadSideInletHumRat
    VarSpeedCoil(DXCoilNum)%OutletAirDBTemp   = PsyTdbFnHW(VarSpeedCoil(DXCoilNum)%OutletAirEnthalpy,  &
                                                               VarSpeedCoil(DXCoilNum)%OutletAirHumRat,RoutineName)
    PLRCorrLoadSideMdot = LoadSideMassFlowRate
  ELSE
    ! default to cycling fan, cycling compressor
    VarSpeedCoil(DXCoilNum)%OutletAirEnthalpy = LoadSideOutletEnth
    VarSpeedCoil(DXCoilNum)%OutletAirHumRat   = LoadsideOutletHumRat
    VarSpeedCoil(DXCoilNum)%OutletAirDBTemp   = LoadSideOutletDBTemp
    PLRCorrLoadSideMdot = LoadSideMassFlowRate*PartLoadRatio
  END IF


   ! scale heat transfer rates to PLR and power to RTF
  QLoadTotal = QLoadTotal*PartLoadRatio
  QSensible  = QSensible*PartLoadRatio
  ! count the powr separately
  Winput     = Winput*RuntimeFrac !+ VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower
  QSource    = QSource*PartLoadRatio
  QWasteHeat = QWasteHeat*PartLoadRatio

!  Add power to global variable so power can be summed by parent object
  DXElecHeatingPower = Winput

  ReportingConstant=TimeStepSys*SecInHour
  !Update heat pump data structure
  VarSpeedCoil(DXCoilNum)%Power               = Winput
  VarSpeedCoil(DXCoilNum)%QLoadTotal          = QLoadTotal
  VarSpeedCoil(DXCoilNum)%QSensible           = QSensible
  VarSpeedCoil(DXCoilNum)%QSource             = QSource
  VarSpeedCoil(DXCoilNum)%Energy=Winput*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergyLoadTotal=QLoadTotal*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergySensible=QSensible*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergyLatent=0.0d0
  VarSpeedCoil(DXCoilNum)%EnergySource=QSource*ReportingConstant
  VarSpeedCoil(DXCoilNum)%CrankcaseHeaterConsumption = VarSpeedCoil(DXCoilNum)%CrankcaseHeaterPower*ReportingConstant
  VarSpeedCoil(DXCoilNum)%DefrostConsumption = VarSpeedCoil(DXCoilNum)%DefrostPower*ReportingConstant
  IF(RunTimeFrac == 0.0d0) THEN
    VarSpeedCoil(DXCoilNum)%COP = 0.0d0
  ELSE
    VarSpeedCoil(DXCoilNum)%COP = QLoadTotal/Winput
  END IF
  VarSpeedCoil(DXCoilNum)%RunFrac             = RuntimeFrac
  VarSpeedCoil(DXCoilNum)%PartLoadRatio       = PartLoadRatio
  VarSpeedCoil(DXCoilNum)%AirMassFlowRate     = PLRCorrLoadSideMdot

  IF(VarSpeedCoil(DXCoilNum)%VSCoilTypeOfNum .EQ. Coil_HeatingAirToAirVariableSpeed) THEN
    VarSpeedCoil(DXCoilNum)%WaterMassFlowRate   = 0.0d0
    VarSpeedCoil(DXCoilNum)%OutletWaterTemp     = 0.0d0
    VarSpeedCoil(DXCoilNum)%OutletWaterEnthalpy = 0.0d0
  ELSE
    VarSpeedCoil(DXCoilNum)%WaterMassFlowRate   = SourceSideMassFlowRate
    VarSpeedCoil(DXCoilNum)%OutletWaterTemp     = SourceSideInletTemp - QSource/(SourceSideMassFlowRate * CpSource)
    VarSpeedCoil(DXCoilNum)%OutletWaterEnthalpy = SourceSideInletEnth - QSource/SourceSideMassFlowRate
  END IF

  VarSpeedCoil(DXCoilNum)%QWasteHeat =  QWasteHeat
  RETURN
END SUBROUTINE CalcVarSpeedCoilHeating

FUNCTION GetCoilCapacityVariableSpeed(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilCapacity
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the rated coil capacity at the nominal speed level for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: CoilCapacity ! returned capacity of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (SameString(CoilType,'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') .OR.   &
      SameString(CoilType,'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') .OR. &
      SameString(CoilType,'COIL:COOLING:DX:VARIABLESPEED') .OR.   &
      SameString(CoilType,'COIL:HEATING:DX:VARIABLESPEED')) THEN
    WhichCoil=FindItemInList(CoilName,VarSpeedCoil%Name,NumWaterToAirHPs)
    IF (WhichCoil /= 0) THEN
      IF (CoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' .OR. &
        CoilType == 'COIL:HEATING:DX:VARIABLESPEED'  ) THEN
        CoilCapacity=VarSpeedCoil(WhichCoil)%RatedCapHeat
      ELSE
        CoilCapacity=VarSpeedCoil(WhichCoil)%RatedCapCoolTotal
      ENDIF
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilCapacityVariableSpeed: Could not find CoilType="'// &
                          TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CoilCapacity=-1000.0d0
  ENDIF

  RETURN

END FUNCTION GetCoilCapacityVariableSpeed

FUNCTION GetCoilIndexVariableSpeed(CoilType,CoilName,ErrorsFound) RESULT(IndexNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilIndex
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the coil index for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and index is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: IndexNum     ! returned index of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IndexNum=FindItemInList(CoilName,VarSpeedCoil%Name,NumWaterToAirHPs)

  IF (IndexNum == 0) THEN
    CALL ShowSevereError('GetCoilIndexVariableSpeed: Could not find CoilType="'// &
                          TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetCoilIndexVariableSpeed

FUNCTION GetCoilAirFlowRateVariableSpeed(CoilType,CoilName,ErrorsFound) RESULT(CoilAirFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilAirFlowRate
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the max coil air flow rate for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: CoilAirFlowRate ! returned air volume flow rate of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (SameString(CoilType,'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') .OR.   &
      SameString(CoilType,'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') .OR. &
      SameString(CoilType,'COIL:COOLING:DX:VARIABLESPEED') .OR.   &
      SameString(CoilType,'COIL:HEATING:DX:VARIABLESPEED')) THEN
    WhichCoil=FindItemInList(CoilName,VarSpeedCoil%Name,NumWaterToAirHPs)
    IF (WhichCoil /= 0) THEN
      !CoilAirFlowRate=VarSpeedCoil(WhichCoil)%RatedAirVolFlowRate
      IF(VarSpeedCoil(WhichCoil)%RatedAirVolFlowRate == AUTOSIZE) THEN !means autosize
        CoilAirFlowRate=VarSpeedCoil(WhichCoil)%RatedAirVolFlowRate
      ELSE
        CoilAirFlowRate=VarSpeedCoil(WhichCoil)%MSRatedAirVolFlowRate(VarSpeedCoil(WhichCoil)%NumOfSpeeds)/ &
            VarSpeedCoil(WhichCoil)%MSRatedAirVolFlowRate(VarSpeedCoil(WhichCoil)%NormSpedLevel) * &
             VarSpeedCoil(WhichCoil)%RatedAirVolFlowRate
      END IF ! use largest air flow rate
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilAirFlowRateVariableSpeed: Could not find CoilType="'// &
                          TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CoilAirFlowRate=-1000.0d0
  ENDIF

  RETURN

END FUNCTION GetCoilAirFlowRateVariableSpeed


FUNCTION GetCoilInletNodeVariableSpeed(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilInletNode
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the inlet node.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched coil
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  WhichCoil=FindItemInList(CoilName,VarSpeedCoil%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    NodeNumber=VarSpeedCoil(WhichCoil)%AirInletNodeNum
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilInletNodeVariableSpeed: Could not find CoilType="'// &
                          TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilInletNodeVariableSpeed


FUNCTION GetCoilOutletNodeVariableSpeed(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilOutletNode
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the outlet node.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  WhichCoil=FindItemInList(CoilName,VarSpeedCoil%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    NodeNumber=VarSpeedCoil(WhichCoil)%AirOutletNodeNum
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilOutletNodeVariableSpeed: Could not find CoilType="'// &
                          TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilOutletNodeVariableSpeed

FUNCTION GetVSCoilCondenserInletNode(CoilName,ErrorsFound) RESULT(CondNode)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on DXCoil:GetCoilCondenserInletNode
          !       DATE WRITTEN   July 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the condenser inlet node.  If
          ! incorrect coil  name is given, errorsfound is returned as true.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: CondNode     ! returned condenser node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

   WhichCoil=FindItemInList(CoilName,VarSpeedCoil%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    CondNode=VarSpeedCoil(WhichCoil)%CondenserInletNodeNum
   ELSE
    CALL ShowSevereError('GetCoilCondenserInletNode: Invalid VS DX Coil, Type= VS DX Cooling Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CondNode=0
  ENDIF

  RETURN

END FUNCTION GetVSCoilCondenserInletNode


FUNCTION GetVSCoilMinOATCompressor(CoilName,ErrorsFound) RESULT(MinOAT)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen
          !       DATE WRITTEN   July 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns min OAT for compressor operation.  If
          ! incorrect coil  name is given, errorsfound is returned as true.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)            :: MinOAT     ! returned min OAT for compressor operation

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

   WhichCoil=FindItemInList(CoilName,VarSpeedCoil%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    MinOAT = VarSpeedCoil(WhichCoil)%MinOATCompressor
   ELSE
    CALL ShowSevereError('GetVSCoilMinOATCompressor: Invalid VS DX Coil, Type= VS DX Cooling Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    MinOAT = -1000.0d0
  ENDIF

  RETURN

END FUNCTION GetVSCoilMinOATCompressor


FUNCTION GetVSCoilNumOfSpeeds(CoilName,ErrorsFound) RESULT(Speeds)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns number of speeds.  If
          ! incorrect coil name is given, errorsfound is returned as true.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER            :: Speeds     ! returned number of speeds

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
    GetCoilsInputFlag=.FALSE.
  End If

   WhichCoil=FindItemInList(CoilName,VarSpeedCoil%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    Speeds = VarSpeedCoil(WhichCoil)%NumOfSpeeds
   ELSE
    CALL ShowSevereError('GetVSCoilNumOfSpeeds: Invalid VS DX Coil, Type= VS DX Cooling Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    Speeds = 0
  ENDIF

  RETURN

END FUNCTION GetVSCoilNumOfSpeeds


SUBROUTINE SetVarSpeedCoilData(WSHPNum,ErrorsFound,CompanionCoolingCoilNum,CompanionHeatingCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:SetWSHPData
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine was designed to "push" information from a parent object to
          ! this WSHP coil object.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: TrimSigDigits
  USE InputProcessor,  ONLY: FindItemInList, SameString
  USE FluidProperties, ONLY: FindGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: WSHPNum  ! Number of OA Controller
  LOGICAL, INTENT(INOUT) :: ErrorsFound    ! Set to true if certain errors found
  INTEGER, OPTIONAL      :: CompanionCoolingCoilNum  ! Index to cooling coil for heating coil = SimpleWSHPNum
  INTEGER, OPTIONAL      :: CompanionHeatingCoilNum  ! Index to heating coil for cooling coil = SimpleWSHPNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetVarSpeedCoilInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (WSHPNum <= 0 .or. WSHPNum > NumWatertoAirHPs) THEN
    CALL ShowSevereError('SetVarSpeedCoilData: called with VS WSHP Coil Number out of range='//  &
         TRIM(TrimSigDigits(WSHPNum))//' should be >0 and <'//TRIM(TrimSigDigits(NumWatertoAirHPs)))
    ErrorsFound=.true.
    RETURN
  ENDIF

  IF (PRESENT(CompanionCoolingCoilNum)) THEN
    VarSpeedCoil(WSHPNum)%CompanionCoolingCoilNum=CompanionCoolingCoilNum
    VarSpeedCoil(WSHPNum)%FindCompanionUpStreamCoil = .TRUE.
    VarSpeedCoil(CompanionCoolingCoilNum)%CompanionHeatingCoilNum=WSHPNum
  ENDIF

  IF (PRESENT(CompanionHeatingCoilNum)) THEN
    VarSpeedCoil(WSHPNum)%CompanionHeatingCoilNum=CompanionHeatingCoilNum
    VarSpeedCoil(CompanionHeatingCoilNum)%CompanionCoolingCoilNum=WSHPNum
  ENDIF

  RETURN

END SUBROUTINE SetVarSpeedCoilData

SUBROUTINE UpdateVarSpeedCoil(DXCoilNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:UpdateSimpleWSHP
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the Water to Air Heat Pump outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! Data is moved from the HP data structure to the HP outlet nodes.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: TimeStepSys
  USe PlantUtilities,  ONLY: SafeCopyPlantNode
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(In) :: DXCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER              :: AirInletNode
  INTEGER              :: WaterInletNode
  INTEGER              :: AirOutletNode
  INTEGER              :: WaterOutletNode
  REAL(r64)            :: ReportingConstant


  !WatertoAirHP(DXCoilNum)%Simflag=.FALSE.
  IF(.NOT. VarSpeedCoil(DXCoilNum)%Simflag)THEN
    ! Heatpump is off; just pass through conditions
    VarSpeedCoil(DXCoilNum)%Power               = 0.0d0
    VarSpeedCoil(DXCoilNum)%QLoadTotal          = 0.0d0
    VarSpeedCoil(DXCoilNum)%QSensible           = 0.0d0
    VarSpeedCoil(DXCoilNum)%QLatent             = 0.0d0
    VarSpeedCoil(DXCoilNum)%QSource             = 0.0d0
    VarSpeedCoil(DXCoilNum)%Energy              = 0.0d0
    VarSpeedCoil(DXCoilNum)%EnergyLoadTotal     = 0.0d0
    VarSpeedCoil(DXCoilNum)%EnergySensible      = 0.0d0
    VarSpeedCoil(DXCoilNum)%EnergyLatent        = 0.0d0
    VarSpeedCoil(DXCoilNum)%EnergySource        = 0.0d0
    VarSpeedCoil(DXCoilNum)%COP                 = 0.0d0
    VarSpeedCoil(DXCoilNum)%RunFrac             = 0.0d0
    VarSpeedCoil(DXCoilNum)%PartLoadRatio       = 0.0d0

    VarSpeedCoil(DXCoilNum)%OutletAirDBTemp     = VarSpeedCoil(DXCoilNum)%InletAirDBTemp
    VarSpeedCoil(DXCoilNum)%OutletAirHumRat     = VarSpeedCoil(DXCoilNum)%InletAirHumRat
    VarSpeedCoil(DXCoilNum)%OutletAirEnthalpy   = VarSpeedCoil(DXCoilNum)%InletAirEnthalpy
    VarSpeedCoil(DXCoilNum)%OutletWaterTemp     = VarSpeedCoil(DXCoilNum)%InletWaterTemp
    VarSpeedCoil(DXCoilNum)%OutletWaterEnthalpy = VarSpeedCoil(DXCoilNum)%InletWaterEnthalpy
  END IF

  AirInletNode    = VarSpeedCoil(DXCoilNum)%AirInletNodeNum
  WaterInletNode  = VarSpeedCoil(DXCoilNum)%WaterInletNodeNum
  AirOutletNode   = VarSpeedCoil(DXCoilNum)%AirOutletNodeNum
  WaterOutletNode = VarSpeedCoil(DXCoilNum)%WaterOutletNodeNum


  ! Set the air outlet  nodes of the WatertoAirHPSimple
  Node(AirOutletNode)%MassFlowRate          = Node(AirInletNode)%MassFlowRate     !LoadSideMassFlowRate
  Node(AirOutletNode)%Temp                  = VarSpeedCoil(DXCoilNum)%OutletAirDBTemp
  Node(AirOutletNode)%HumRat                = VarSpeedCoil(DXCoilNum)%OutletAirHumRat
  Node(AirOutletNode)%Enthalpy              = VarSpeedCoil(DXCoilNum)%OutletAirEnthalpy

   ! Set the air outlet nodes for properties that just pass through & not used
  Node(AirOutletNode)%Quality               = Node(AirInletNode)%Quality
  Node(AirOutletNode)%Press                 = Node(AirInletNode)%Press
  Node(AirOutletNode)%MassFlowRateMin       = Node(AirInletNode)%MassFlowRateMin
  Node(AirOutletNode)%MassFlowRateMax       = Node(AirInletNode)%MassFlowRateMax    !LoadSideMassFlowRate
  Node(AirOutletNode)%MassFlowRateMinAvail  = Node(AirInletNode)%MassFlowRateMinAvail
  Node(AirOutletNode)%MassFlowRateMaxAvail  = Node(AirInletNode)%MassFlowRateMaxAvail     !LoadSideMassFlowRate

   ! Set the water outlet node of the WatertoAirHPSimple
   ! Set the water outlet nodes for properties that just pass through & not used
  IF(WaterInletNode /=0 .AND. WaterOutletNode /=0) THEN
    CALL SafeCopyPlantNode(WaterInletNode , WaterOutletNode)
    Node(WaterOutletNode)%Temp                = VarSpeedCoil(DXCoilNum)%OutletWaterTemp
    Node(WaterOutletNode)%Enthalpy            = VarSpeedCoil(DXCoilNum)%OutletWaterEnthalpy
  END IF

  ReportingConstant                         = TimeStepSys*SecInHour
  VarSpeedCoil(DXCoilNum)%Energy          = VarSpeedCoil(DXCoilNum)%Power*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergyLoadTotal = VarSpeedCoil(DXCoilNum)%QLoadTotal*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergySensible  = VarSpeedCoil(DXCoilNum)%QSensible*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergyLatent    = VarSpeedCoil(DXCoilNum)%QLatent*ReportingConstant
  VarSpeedCoil(DXCoilNum)%EnergySource    = VarSpeedCoil(DXCoilNum)%QSource*ReportingConstant

   IF (Contaminant%CO2Simulation) Then
     Node(AirOutletNode)%CO2 = Node(AirInletNode)%CO2
   End If

  RETURN
END SUBROUTINE UpdateVarSpeedCoil

FUNCTION CalcEffectiveSHR(DXCoilNum,SHRss, CyclingScheme, RTF, QLatRated, QLatActual, EnteringDB, EnteringWB) RESULT(SHReff)

        ! FUNCTION INFORMATION:
        !    AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:CalcEffectiveSHR
        !    DATE WRITTEN   March 2012
        !    MODIFIED       na
        !    RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        !    Adjust sensible heat ratio to account for degradation of DX coil latent
        !    capacity at part-load (cycling) conditions.

        ! METHODOLOGY EMPLOYED:
        !    With model parameters entered by the user, the part-load latent performance
        !    of a DX cooling coil is determined for a constant air flow system with
        !    a cooling coil that cycles on/off. The model calculates the time
        !    required for condensate to begin falling from the cooling coil.
        !    Runtimes greater than this are integrated to a "part-load" latent
        !    capacity which is used to determine the "part-load" sensible heat ratio.
        !    See reference below for additional details (linear decay model, Eq. 8b).

        !    For cycling fan operation, a modified version of Henderson and Rengarajan (1996)
        !    model is used by ultilizing the fan delay time as the time-off (or time duration
        !    for the re-evaporation of moisture from time coil). Refer to Tang, C.C. (2005)

        ! REFERENCES:
        ! na


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DXCoilNum         ! Index number for cooling coil
  INTEGER, INTENT (IN) :: CyclingScheme ! Fan/compressor cycling scheme indicator
  REAL(r64), INTENT (IN) :: SHRss         ! Steady-state sensible heat ratio
  REAL(r64), INTENT (IN) :: RTF           ! Compressor run-time fraction
  REAL(r64), INTENT (IN) :: QLatRated     ! Rated latent capacity
  REAL(r64), INTENT (IN) :: QLatActual    ! Actual latent capacity
  REAL(r64), INTENT (IN) :: EnteringDB    ! Entering air dry-bulb temperature
  REAL(r64), INTENT (IN) :: EnteringWB    ! Entering air wet-bulb temperature
  REAL(r64)            :: SHReff        ! Effective sensible heat ratio, includes degradation due to cycling effects

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Twet                 ! Nominal time for condensate to begin leaving the coil's condensate drain line
                                    ! at the current operating conditions (sec)
  REAL(r64) :: Gamma                ! Initial moisture evaporation rate divided by steady-state AC latent capacity
                                    ! at the current operating conditions
  REAL(r64) :: Twet_rated           ! Twet at rated conditions (coil air flow rate and air temperatures), sec
  REAL(r64) :: Gamma_rated          ! Gamma at rated conditions (coil air flow rate and air temperatures)
  REAL(r64) :: Twet_max             ! Maximum allowed value for Twet
  REAL(r64) :: MaxONOFFCyclesperHour  ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64) :: HPTimeConstant       ! Heat pump time constant [s]
  REAL(r64) :: FanDelayTime         ! Fan delay time, time delay for the HP's fan to
                                    ! shut off after compressor cycle off  [s]
  REAL(r64) :: Ton                  ! Coil on time (sec)
  REAL(r64) :: Toff                 ! Coil off time (sec)
  REAL(r64) :: Toffa                ! Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
  REAL(r64) :: aa                   ! Intermediate variable
  REAL(r64) :: To1                  ! Intermediate variable (first guess at To). To = time to the start of moisture removal
  REAL(r64) :: To2                  ! Intermediate variable (second guess at To). To = time to the start of moisture removal
  REAL(r64) :: Error                ! Error for iteration (DO) loop
  REAL(r64) :: LHRmult              ! Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult

   Twet_rated               = VarSpeedCoil(DXCoilNum)%Twet_Rated
   Gamma_rated              = VarSpeedCoil(DXCoilNum)%Gamma_Rated
   MaxONOFFCyclesperHour    = VarSpeedCoil(DXCoilNum)%MaxONOFFCyclesperHour
   HPTimeConstant           = VarSpeedCoil(DXCoilNum)%HPTimeConstant
   FanDelayTime             = VarSpeedCoil(DXCoilNum)%FanDelayTime

!  No moisture evaporation (latent degradation) occurs for runtime fraction of 1.0
!  All latent degradation model parameters cause divide by 0.0 if not greater than 0.0
!  Latent degradation model parameters initialize to 0.0 meaning no evaporation model used.
   IF((RTF.GE.1.0d0) .OR. (QLatRated.EQ.0.0d0) .OR. (QLatActual.EQ.0.0d0) .OR. (Twet_rated.LE.0.0d0) .OR. &
      (Gamma_rated.LE.0.0d0) .OR. (MaxONOFFCyclesperHour.LE.0.0d0) .OR. (HPTimeConstant.LE.0.0d0) .OR. (RTF.LE. 0.0d0)) THEN
     SHReff = SHRss
     RETURN
   ENDIF

   Twet_max   = 9999.0d0 ! high limit for Twet

!  Calculate the model parameters at the actual operating conditions
   Twet    = MIN(Twet_rated*QLatRated /(QLatActual+1.d-10),Twet_max)
   Gamma   = Gamma_rated*QLatRated*(EnteringDB-EnteringWB)/((26.7d0-19.4d0)*QLatActual+1.d-10)

!  Calculate the compressor on and off times using a converntional thermostat curve
   Ton  = 3600.d0/(4.d0*MaxONOFFCyclesperHour*(1.d0-RTF))   ! duration of cooling coil on-cycle (sec)

   IF ((CyclingScheme .EQ. CycFanCycCoil).AND.(FanDelayTime.NE.0.0d0)) THEN
    ! For CycFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
    ! until the fan cycle off. Assume no evaporation from the coil after the fan shuts off.
        Toff = FanDelayTime
   ELSE
    ! For ContFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
    ! for the entire heat pump off-cycle.
        Toff = 3600.d0/(4.d0*MaxONOFFCyclesperHour*RTF)        ! duration of cooling coil off-cycle (sec)
   END IF

!  Cap Toff to meet the equation restriction
   IF(Gamma .GT. 0.0d0)THEN
     Toffa = MIN(Toff, 2.d0*Twet/Gamma)
   ELSE
     Toffa = Toff
   END IF

!  Use sucessive substitution to solve for To
   aa = (Gamma*Toffa) - (0.25d0/Twet)*(Gamma**2)*(Toffa**2)

   To1 = aa+HPTimeConstant
   Error = 1.0d0
   DO WHILE (Error .gt. 0.001d0)
       To2 = aa-HPTimeConstant*(EXP(-To1/HPTimeConstant)-1.0d0)
       Error = ABS((To2-To1)/To1)
       To1 = To2
   END DO

!  Adjust Sensible Heat Ratio (SHR) using Latent Heat Ratio (LHR) multiplier
!  Floating underflow errors occur when -Ton/HPTimeConstant is a large negative number.
!  Cap lower limit at -700 to avoid the underflow errors.
   aa = EXP(MAX(-700.0d0,-Ton/HPTimeConstant))
!  Calculate latent heat ratio multiplier
   LHRmult = MAX(((Ton-To2)/(Ton+HPTimeConstant*(aa-1.0d0))),0.0d0)

!  Calculate part-load or "effective" sensible heat ratio
   SHReff = 1.0-(1.0-SHRss)*LHRmult

   IF (SHReff .LT. SHRss) SHReff = SHRss ! Effective SHR can be less than the steady-state SHR
   IF (SHReff .GT. 1.0d0) SHReff=1.0d0 ! Effective sensible heat ratio can't be greater than 1.0

 RETURN

END FUNCTION CalcEffectiveSHR

SUBROUTINE CalcTotCapSHR_VSWSHP(InletDryBulb,InletHumRat,InletEnthalpy,InletWetBulb,AirMassFlowRatio, WaterMassFlowRatio, &
                         AirMassFlow,CBF, TotCapNom1,CCapFTemp1,CCapAirFFlow1, CCapWaterFFlow1,&
                          TotCapNom2,CCapFTemp2,CCapAirFFlow2, CCapWaterFFlow2,&
                         TotCap1, TotCap2, TotCapSpeed,SHR,CondInletTemp, Pressure, SpeedRatio, NumSpeeds)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, , based on DX:CalcTotCapSHR, introducing two speed levels
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates total capacity and sensible heat ratio of a DX coil at the specified conditions

          ! METHODOLOGY EMPLOYED:
          ! With the rated performance data entered by the user, the model employs some of the
          ! DOE-2.1E curve fits to adjust the capacity and SHR of the unit as a function
          ! of entering air temperatures and supply air flow rate (actual vs rated flow). The model
          ! does NOT employ the exact same methodology to calculate performance as DOE-2, although
          ! some of the DOE-2 curve fits are employed by this model.

          ! The model checks for coil dryout conditions, and adjusts the calculated performance
          ! appropriately.

          ! REFERENCES:
          ! ASHRAE HVAC 2 Toolkit page 4-81.
          !
          ! Henderson, H.I. Jr., K. Rengarajan and D.B. Shirey, III. 1992.The impact of comfort
          ! control on air conditioner energy use in humid climates. ASHRAE Transactions 98(2):
          ! 104-113.
          !
          ! Henderson, H.I. Jr., Danny Parker and Y.J. Huang. 2000.Improving DOE-2's RESYS routine:
          ! User Defined Functions to Provide More Accurate Part Load Energy Use and Humidity
          ! Predictions. Proceedings of ACEEE Conference.


          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT (IN) :: InletDryBulb       ! inlet air dry bulb temperature [C]
  REAL(r64), INTENT (IN) :: InletHumRat        ! inlet air humidity ratio [kg water / kg dry air]
  REAL(r64), INTENT (IN) :: InletEnthalpy      ! inlet air specific enthalpy [J/kg]
  REAL(r64), INTENT (IN) :: InletWetBulb       ! inlet air wet bulb temperature [C]
  REAL(r64), INTENT (IN) :: AirMassFlowRatio   ! Ratio of actual air mass flow to nominal air mass flow
  REAL(r64), INTENT (IN) :: WaterMassFlowRatio   ! Ratio of actual water mass flow to nominal water mass flow
  REAL(r64), INTENT (IN) :: AirMassFlow        ! actual mass flow for capacity and SHR calculation
  REAL(r64), INTENT (IN) :: CBF                ! coil bypass factor
  INTEGER, INTENT (IN) :: NumSpeeds            ! number of speeds for input

  REAL(r64), INTENT (IN) :: TotCapNom1         ! nominal total capacity at low speed [W]
  INTEGER, INTENT (IN) :: CCapFTemp1           ! capacity modifier curve index, function of entering wetbulb at low speed
  INTEGER, INTENT (IN) :: CCapAirFFlow1        ! capacity modifier curve, function of actual air flow vs rated flow at low speed
  INTEGER, INTENT (IN) :: CCapWaterFFlow1      ! capacity modifier curve, function of actual water flow vs rated flow at low speed

  REAL(r64), INTENT (IN) :: TotCapNom2         ! nominal total capacity at high speed [W]
  INTEGER, INTENT (IN) :: CCapFTemp2           ! capacity modifier curve index, function of entering wetbulb at high speed
  INTEGER, INTENT (IN) :: CCapAirFFlow2        ! capacity modifier curve, function of actual air flow vs rated flow at high speed
  INTEGER, INTENT (IN) :: CCapWaterFFlow2      ! capacity modifier curve, function of actual water flow vs rated flow at high speed


  REAL(r64), INTENT (OUT)   :: TotCap1           ! total capacity at the given conditions [W] at low speed
  REAL(r64), INTENT (OUT)   :: TotCap2           ! total capacity at the given conditions [W] at high speed
  REAL(r64), INTENT (OUT)   :: TotCapSpeed       ! integrated total capacity corresponding to the speed ratio

  REAL(r64), INTENT (OUT)   :: SHR               ! sensible heat ratio at the given conditions
  REAL(r64), INTENT (IN) :: CondInletTemp      ! Condenser inlet temperature [C]
  REAL(r64), INTENT (IN) :: Pressure           ! air pressure [Pa]
  REAL(r64), INTENT (IN) :: SpeedRatio         ! from 0.0 to 1.0


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='CalcTotCapSHR_VSWSHP'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: InletWetBulbCalc    ! calculated inlet wetbulb temperature used for finding dry coil point [C]
  REAL(r64) :: InletHumRatCalc     ! calculated inlet humidity ratio used for finding dry coil point [kg water / kg dry air]
  REAL(r64) :: TotCapTempModFac1
  ! Total capacity modifier (function of entering wetbulb, outside water inlet temp) at low speed
  REAL(r64) :: TotCapAirFlowModFac1
  ! Total capacity modifier (function of actual supply air flow vs nominal flow) at low speed
  REAL(r64) :: TotCapWaterFlowModFac1
  ! Total capacity modifier (function of actual supply water flow vs nominal flow) at low speed
  REAL(r64) :: TotCapTempModFac2
  ! Total capacity modifier (function of entering wetbulb, outside water inlet temp) at high speed
  REAL(r64) :: TotCapAirFlowModFac2
  ! Total capacity modifier (function of actual supply air flow vs nominal flow) at high speed
  REAL(r64) :: TotCapWaterFlowModFac2
  ! Total capacity modifier (function of actual supply water flow vs nominal flow) at high speed
  REAL(r64) :: hDelta              ! Change in air enthalpy across the cooling coil [J/kg]
  REAL(r64) :: hADP                ! Apparatus dew point enthalpy [J/kg]
  REAL(r64) :: tADP                ! Apparatus dew point temperature [C]
  REAL(r64) :: wADP                ! Apparatus dew point humidity ratio [kg/kg]
  REAL(r64) :: hTinwADP            ! Enthalpy at inlet dry-bulb and wADP [J/kg]
  REAL(r64) :: SHRCalc             ! temporary calculated value of SHR
  REAL(r64) :: TotCapCalc          ! temporary calculated value of total capacity [W]
  REAL(r64) :: TotCapCalc1          ! temporary calculated value of total capacity [W] at low speed
  REAL(r64) :: TotCapCalc2          ! temporary calculated value of total capacity [W] at high speed
  INTEGER :: Counter             ! Counter for dry evaporator iterations
  INTEGER :: MaxIter             ! Maximum number of iterations for dry evaporator calculations
  REAL(r64) :: RF                  ! Relaxation factor for dry evaporator iterations
  REAL(r64) :: Tolerance           ! Error tolerance for dry evaporator iterations
  REAL(r64) :: werror              ! Deviation of humidity ratio in dry evaporator iteration loop
  LOGICAL   :: LoopOn = .TRUE.     ! flag to control the loop iteration

  MaxIter = 30
  RF = 0.4d0
  Counter = 0
  Tolerance = 0.01d0
  werror = 0.0d0

  InletWetBulbCalc = InletWetBulb
  InletHumRatCalc = InletHumRat
  LoopOn = .TRUE.

!  DO WHILE (ABS(werror) .gt. Tolerance .OR. Counter == 0)
!   Get capacity modifying factor (function of inlet wetbulb & outside drybulb) for off-rated conditions
  DO WHILE(LoopOn)
    TotCapTempModFac1 = CurveValue(CCapFTemp1,InletWetBulbCalc,CondInletTemp)
!   Get capacity modifying factor (function of mass flow) for off-rated conditions
    TotCapAirFlowModFac1 = CurveValue(CCapAirFFlow1,AirMassFlowRatio)
    !Get capacity modifying factor (function of mass flow) for off-rated conditions
    IF(CCapWaterFFlow1 .EQ. 0 ) THEN
        TotCapWaterFlowModFac1 = 1.0d0
    ELSE
        TotCapWaterFlowModFac1 = CurveValue(CCapWaterFFlow1,WaterMassFlowRatio)
    END IF

!   Get total capacity
    IF( NumSpeeds  < 2 ) THEN !ONLY ONE SPEED
        TotCapCalc = TotCapNom1 * TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1
        TotCapCalc1 = TotCapCalc
        TotCapCalc2 = 0.0d0
    ELSE
        TotCapTempModFac2 = CurveValue(CCapFTemp2,InletWetBulbCalc,CondInletTemp)
        TotCapAirFlowModFac2 = CurveValue(CCapAirFFlow2,AirMassFlowRatio)

        IF(CCapWaterFFlow2 .EQ. 0) THEN
            TotCapWaterFlowModFac2 = 1.0d0
        ELSE
            TotCapWaterFlowModFac2 = CurveValue(CCapWaterFFlow2,WaterMassFlowRatio)
        END IF

        TotCapCalc1 = TotCapNom1 * TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1
        TotCapCalc2 = TotCapNom2 * TotCapAirFlowModFac2 * TotCapWaterFlowModFac2 * TotCapTempModFac2

        TotCapCalc = TotCapCalc2*SpeedRatio + (1.0d0 - SpeedRatio ) * TotCapCalc1

    END IF

!   Calculate apparatus dew point conditions using TotCap and CBF
    hDelta = TotCapCalc/AirMassFlow
    hADP = InletEnthalpy - hDelta/(1.d0-CBF)
    tADP = PsyTsatFnHPb(hADP,Pressure)
    wADP = PsyWFnTdbH(tADP,hADP)
    hTinwADP = PsyHFnTdbW(InletDryBulb,wADP)
    SHRCalc = MIN((hTinwADP-hADP)/(InletEnthalpy-hADP),1.d0)
!
!   Check for dry evaporator conditions (win < wadp)
!
    IF (wADP .gt. InletHumRatCalc .or. (Counter .ge. 1 .and. Counter .lt. MaxIter)) THEN
      If(InletHumRatCalc == 0.0d0)InletHumRatCalc=0.00001d0
      werror = (InletHumRatCalc - wADP)/InletHumRatCalc
!
!     Increase InletHumRatCalc at constant inlet air temp to find coil dry-out point. Then use the
!     capacity at the dry-out point to determine exiting conditions from coil. This is required
!     since the TotCapTempModFac doesn't work properly with dry-coil conditions.
!
      InletHumRatCalc = RF*wADP + (1.d0-RF)*InletHumRatCalc
      InletWetBulbCalc = PsyTwbFnTdbWPb(InletDryBulb,InletHumRatCalc,Pressure)
      Counter = Counter + 1
      IF (ABS(werror) .gt. Tolerance) THEN
        LoopOn = .TRUE. !go to 50   ! Recalculate with modified inlet conditions
      ELSE
        LoopOn = .FALSE.
      END IF
    ELSE
      LoopOn = .FALSE.
    END IF
  END DO

! END DO

!  Calculate full load output conditions
  IF (SHRCalc .gt. 1.d0 .OR. Counter .gt. 0) SHRCalc = 1.d0

  SHR = SHRCalc
  TotCap1 = TotCapCalc1
  TotCap2 = TotCapCalc2
  TotCapSpeed = TotCapCalc

 ! IF(SHR < 0.3d0) SHR = 0.3d0

  RETURN
END SUBROUTINE CalcTotCapSHR_VSWSHP

FUNCTION AdjustCBF(CBFNom,AirMassFlowRateNom,AirMassFlowRate) RESULT(CBFAdj)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl using Don Shirey's code
          !       DATE WRITTEN   September 2002
          !       Modified       BOs March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !    Adjust coil bypass factor for actual air flow rate.

          ! METHODOLOGY EMPLOYED:
          ! Uses relation CBF = exp(-NTU) whereNTU = A0/(m*cp). Relationship models the cooling coil
          ! as a heat exchanger with Cmin/Cmax = 0.
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT (IN) :: CBFNom                ! nominal coil bypass factor
  REAL(r64), INTENT (IN) :: AirMassFlowRateNom    ! nominal air mass flow rate [kg/s]
  REAL(r64), INTENT (IN) :: AirMassFlowRate       ! actual air mass flow rate [kg/s]
  REAL(r64)         :: CBFAdj                ! the result - the adjusted coil bypass factor


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: A0  ! intermediate variable
  REAL(r64) :: ADiff  ! intermediate variable

  IF (CBFNom .gt. 0.0d0) THEN
     A0 = -log(CBFNom)*AirMassFlowRateNom
  ELSE
     A0 = 0.0d0
  END IF
  ADiff=-A0/AirMassFlowRate
  IF (ADiff >= EXP_LowerLimit) THEN
     CBFAdj = exp(ADiff)
  ELSE
     CBFAdj = 0.0d0
  END IF

  RETURN
END FUNCTION AdjustCBF

FUNCTION CalcCBF(UnitType,UnitName,InletAirTemp,InletAirHumRat,TotCap,AirMassFlowRate,SHR) RESULT(CBF)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl using Don Shirey's code
          !       DATE WRITTEN   September 2002
          !       Modified by BOS Mark 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the coil bypass factor for a coil given the total capacity at the entering conditions,
          ! air mass flow rate at the entering conditions, and the sensible heat ratio (SHR) at the
          ! entering conditions.

          ! METHODOLOGY EMPLOYED:
          ! calculate SlopeRated (deltahumrat/deltaT) using rated unit information provided by
          ! user. Then hunt along saturation curve of psychrometric chart until the slope of the line
          ! between the saturation point and rated inlet air humidity ratio and T is the same as SlopeRated.
          ! When the slopes are equal, then we have located the apparatus dewpoint of the coil at rated
          ! conditions. From this information, coil bypass factor is calculated.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DataEnvironment, ONLY: StdRhoAir

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN) :: UnitType
  CHARACTER(len=*), INTENT (IN) :: UnitName
  REAL(r64), INTENT (IN) :: InletAirTemp          ! inlet air temperature [C]
  REAL(r64), INTENT (IN) :: InletAirHumRat        ! inlet air humidity ratio [kg water / kg dry air]
  REAL(r64), INTENT (IN) :: TotCap                ! total cooling  capacity [Watts]
  REAL(r64), INTENT (IN) :: AirMassFlowRate       ! the air mass flow rate at the given capacity [kg/s]
  REAL(r64), INTENT (IN) :: SHR                   ! sensible heat ratio at the given capacity and flow rate
  REAL(r64)         :: CBF                   ! the result - the coil bypass factor


          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  RoutineName='CalcCBF'
  REAL(r64) :: SmallDifferenceTest=0.00000001d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: InletAirEnthalpy  ! Enthalpy of inlet air to evaporator at given conditions [J/kg]
  REAL(r64) :: DeltaH            ! Enthalpy drop across evaporator at given conditions [J/kg]
  REAL(r64) :: DeltaT            ! Temperature drop across evaporator at given conditions [C]
  REAL(r64) :: DeltaHumRat       ! Humidity ratio drop across evaporator at given conditions [kg/kg]
  REAL(r64) :: OutletAirTemp     ! Outlet dry-bulb temperature from evaporator at given conditions [C]
  REAL(r64) :: OutletAirEnthalpy ! Enthalpy of outlet air at given conditions [J/kg]
  REAL(r64) :: OutletAirHumRat   ! Outlet humidity ratio from evaporator at given conditions [kg/kg]
  REAL(r64) :: OutletAirRH       ! relative humidity of the outlet air
  REAL(r64) :: Error                ! Error term used in given coil bypass factor (CBF) calculations
  REAL(r64) :: ErrorLast            ! Error term, from previous iteration
  INTEGER :: Iter                 ! Iteration loop counter in CBF calculations
  INTEGER :: IterMax              ! Maximum number of iterations in CBF calculations
  REAL(r64) :: ADPTemp              ! Apparatus dewpoint temperature used in CBF calculations [C]
  REAL(r64) :: ADPHumRat            ! Apparatus dewpoint humidity used in CBF calculations [kg/kg]
  REAL(r64) :: ADPEnthalpy          ! Air enthalpy at apparatus dew point [J/kg]
  REAL(r64) :: DeltaADPTemp         ! Change in Apparatus Dew Point used in CBF calculations [C]
  REAL(r64) :: SlopeAtConds          ! Slope (DeltaHumRat/DeltaT) at given conditions
  REAL(r64) :: Slope                ! Calculated Slope used while hunting for Tadp
  REAL(r64) :: Tolerance            ! Convergence tolerance for CBF calculations
  REAL(r64) :: HTinHumRatOut        ! Air enthalpy at inlet air temp and outlet air humidity ratio [J/kg]
  LOGICAL :: CBFErrors=.false.    ! Set to true if errors in CBF calculation, fatal at end of routine

  DeltaH = 0.0d0
  DeltaT = 0.0d0
  DeltaHumRat = 0.0d0
  OutletAirTemp =  InletAirTemp
  OutletAirHumRat = InletAirHumRat
  SlopeAtConds = 0.0d0
  Slope = 0.0d0
  IterMax = 50
  CBFErrors=.false.

  DeltaH = TotCap/AirMassFlowRate
  InletAirEnthalpy = PsyHFnTdbW(InletAirTemp,InletAirHumRat)
  HTinHumRatOut = InletAirEnthalpy - (1.0d0-SHR)*DeltaH
  OutletAirHumRat = PsyWFnTdbH(InletAirTemp,HTinHumRatOut)
  DeltaHumRat = InletAirHumRat - OutletAirHumRat
  OutletAirEnthalpy = InletAirEnthalpy - DeltaH
  OutletAirTemp = PsyTdbFnHW(OutletAirEnthalpy,OutletAirHumRat)
!  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
!  Pressure will have to be pass into this subroutine to fix this one
  OutletAirRH = PsyRhFnTdbWPb(OutletAirTemp,OutletAirHumRat,StdBaroPress,'CalcCBF')
  IF (OutletAirRH .ge. 1.0d0) THEN
     CALL ShowSevereError ('For object = '//TRIM(UnitType)// ', name = "'//TRIM(UnitName)// '"')
     CALL ShowContinueError ('Calculated outlet air relative humidity greater than 1. The combination of')
     CALL ShowContinueError ('rated air volume flow rate, total cooling capacity and sensible heat ratio yields coil exiting')
     CALL ShowContinueError ('air conditions above the saturation curve. Possible fixes are to reduce the rated total cooling')
     CALL ShowContinueError ('capacity, increase the rated air volume flow rate, or reduce the rated sensible heat'// &
                             ' ratio for this coil.')
     CALL ShowContinueError ('If autosizing, it is recommended that all three of these values be autosized.')
     CALL ShowContinueError('...Inputs used for calculating cooling coil bypass factor.')
     CALL ShowContinueError('...Inlet Air Temperature     = '//TRIM(RoundSigDigits(InletAirTemp,2))//' C')
     CALL ShowContinueError('...Outlet Air Temperature    = '//TRIM(RoundSigDigits(OutletAirTemp,2))//' C')
     CALL ShowContinueError('...Inlet Air Humidity Ratio  = '//TRIM(RoundSigDigits(InletAirHumRat,6))//' kgWater/kgDryAir')
     CALL ShowContinueError('...Outlet Air Humidity Ratio = '//TRIM(RoundSigDigits(OutletAirHumRat,6))//' kgWater/kgDryAir')
     CALL ShowContinueError('...Total Cooling Capacity used in calculation = '//TRIM(RoundSigDigits(TotCap,2))//' W')
     CALL ShowContinueError('...Air Mass Flow Rate used in calculation     = '//TRIM(RoundSigDigits(AirMassFlowRate,6))//' kg/s')
     CALL ShowContinueError('...Air Volume Flow Rate used in calculation   = '// &
       TRIM(RoundSigDigits(AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName),6))//' m3/s')
     IF(TotCap .GT. 0.d0)THEN
       IF (((MinRatedVolFlowPerRatedTotCap - AirMassFlowRate/ &
            PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap) > SmallDifferenceTest).OR. &
           ((AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap &
             - MaxRatedVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
         CALL ShowContinueError('...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = '// &
                                TRIM(RoundSigDigits(AirMassFlowRate/ &
                                PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap,7))//' m3/s/W')
       END IF
     END IF
     CALL ShowContinueErrorTimeStamp(' ')
     CALL ShowFatalError ('Check and revise the input data for this coil before rerunning the simulation.')
  END IF
  DeltaT = InletAirTemp - OutletAirTemp
  IF (DeltaT .LE. 0.0d0) THEN
     CALL ShowSevereError ('For object = '//TRIM(UnitType)// ', name = "'//TRIM(UnitName)// '"')
     CALL ShowContinueError ('Calculated coil delta T is less than or equal to 0. The combination of')
     CALL ShowContinueError ('rated air volume flow rate, total cooling capacity and sensible heat ratio yields coil exiting')
     CALL ShowContinueError ('air conditions that are not reasonable. Possible fixes are to adjust the rated total cooling')
     CALL ShowContinueError ('capacity, rated air volume flow rate, or rated sensible heat'// &
                             ' ratio for this coil.')
     CALL ShowContinueError ('If autosizing, it is recommended that all three of these values be autosized.')
     CALL ShowContinueError('...Inputs used for calculating cooling coil bypass factor.')
     CALL ShowContinueError('...Inlet Air Temperature     = '//TRIM(RoundSigDigits(InletAirTemp,2))//' C')
     CALL ShowContinueError('...Outlet Air Temperature    = '//TRIM(RoundSigDigits(OutletAirTemp,2))//' C')
     CALL ShowContinueError('...Inlet Air Humidity Ratio  = '//TRIM(RoundSigDigits(InletAirHumRat,6))//' kgWater/kgDryAir')
     CALL ShowContinueError('...Outlet Air Humidity Ratio = '//TRIM(RoundSigDigits(OutletAirHumRat,6))//' kgWater/kgDryAir')
     CALL ShowContinueError('...Total Cooling Capacity used in calculation = '//TRIM(RoundSigDigits(TotCap,2))//' W')
     CALL ShowContinueError('...Air Mass Flow Rate used in calculation     = '//TRIM(RoundSigDigits(AirMassFlowRate,6))//' kg/s')
     CALL ShowContinueError('...Air Volume Flow Rate used in calculation   = '// &
       TRIM(RoundSigDigits(AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName),6))//' m3/s')
     IF(TotCap .GT. 0.d0)THEN
       IF (((MinRatedVolFlowPerRatedTotCap - AirMassFlowRate/ &
            PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap) > SmallDifferenceTest).OR. &
           ((AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap &
             - MaxRatedVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
         CALL ShowContinueError('...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = '// &
                                TRIM(RoundSigDigits(AirMassFlowRate/ &
                                PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap,7))//' m3/s/W')
       END IF
     END IF
     CALL ShowContinueErrorTimeStamp(' ')
     CALL ShowFatalError ('Check and revise the input data for this coil before rerunning the simulation.')
  END IF
  ! Calculate slope at given conditions
  IF (DeltaT .gt. 0.0d0) SlopeAtConds = DeltaHumRat/DeltaT

!  IF (SlopeAtConds .le. .0000001d0 .or. OutletAirHumRat .le. 0.0d0) THEN
  IF (SlopeAtConds .lt. 0.0d0 .or. OutletAirHumRat .le. 0.0d0) THEN
!   Invalid conditions, slope can't be less than zero (SHR > 1) or
!   outlet air humidity ratio can't be less than zero.
    CALL ShowSevereError(TRIM(UnitType)//' "'//TRIM(UnitName)//'"')
    CALL ShowContinueError('...Invalid slope or outlet air condition when calculating cooling coil bypass factor.')
    CALL ShowContinueError('...Slope = '//TRIM(RoundSigDigits(SlopeAtConds,8)))
    CALL ShowContinueError('...Inlet Air Temperature     = '//TRIM(RoundSigDigits(InletAirTemp,2))//' C')
    CALL ShowContinueError('...Outlet Air Temperature    = '//TRIM(RoundSigDigits(OutletAirTemp,2))//' C')
    CALL ShowContinueError('...Inlet Air Humidity Ratio  = '//TRIM(RoundSigDigits(InletAirHumRat,6))//' kgWater/kgDryAir')
    CALL ShowContinueError('...Outlet Air Humidity Ratio = '//TRIM(RoundSigDigits(OutletAirHumRat,6))//' kgWater/kgDryAir')
    CALL ShowContinueError('...Total Cooling Capacity used in calculation = '//TRIM(RoundSigDigits(TotCap,2))//' W')
    CALL ShowContinueError('...Air Mass Flow Rate used in calculation     = '//TRIM(RoundSigDigits(AirMassFlowRate,6))//' kg/s')
     CALL ShowContinueError('...Air Volume Flow Rate used in calculation   = '// &
       TRIM(RoundSigDigits(AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName),6))//' m3/s')
    IF(TotCap .GT. 0.d0)THEN
       IF (((MinRatedVolFlowPerRatedTotCap - AirMassFlowRate/ &
            PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap) > SmallDifferenceTest).OR. &
           ((AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap &
             - MaxRatedVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
         CALL ShowContinueError('...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = '// &
                                TRIM(RoundSigDigits(AirMassFlowRate/ &
                                PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap,7))//' m3/s/W')
       END IF
    END IF
    CALL ShowContinueErrorTimeStamp(' ')
    CBFErrors=.true.
    CBF = 0.0d0 !Objexx:Return Suppress static testing from complaining about return value not set
  ELSE

!   First guess for Tadp is outlet air dew point
!  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
!  Pressure will have to be pass into this subroutine to fix this one
    ADPTemp = PsyTdpFnWPb(OutletAirHumRat,StdBaroPress)

    Tolerance = 1.d0         ! initial conditions for iteration
    ErrorLast = 100.d0
    Iter = 0
    DeltaADPTemp = 5.0d0
    DO WHILE ((Iter .le. IterMax).and.(Tolerance .gt. .001d0))
!     Do for IterMax iterations or until the error gets below .1%
      IF (Iter .gt. 0) ADPTemp = ADPTemp + DeltaADPTemp
      Iter = Iter + 1

!     Find new slope using guessed Tadp

!  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
!  Pressure will have to be pass into this subroutine to fix this one
      ADPHumRat = PsyWFnTdpPb(ADPTemp,StdBaroPress)
      Slope     = (InletAirHumRat-ADPHumRat)/(InletAirTemp-ADPTemp)

!     check for convergence (slopes are equal to within error tolerance)

      Error     = (Slope-SlopeAtConds)/SlopeAtConds
      IF ((Error .gt. 0.0d0).and.(ErrorLast .lt. 0.0d0)) DeltaADPTemp = -DeltaADPTemp/2.d0
      IF ((Error .lt. 0.0d0).and.(ErrorLast .gt. 0.0d0)) DeltaADPTemp = -DeltaADPTemp/2.d0
      ErrorLast = Error

      Tolerance = ABS(Error)

    END DO

!   Calculate Bypass Factor from Enthalpies

    InletAirEnthalpy=PsyHFnTdbW(InletAirTemp,InletAirHumRat)
    OutletAirEnthalpy=PsyHFnTdbW(OutletAirTemp,OutletAirHumRat)
    ADPEnthalpy=PsyHFnTdbW(ADPTemp,ADPHumRat)
    CBF = (OutletAirEnthalpy-ADPEnthalpy)/(InletAirEnthalpy-ADPEnthalpy)
    IF (Iter .gt. IterMax) THEN
      CALL ShowSevereError(TRIM(UnitType)//' "'//TRIM(UnitName)//&
                          '" -- coil bypass factor calculation did not converge after max iterations.')
      CALL ShowContinueError('The RatedSHR of ['//TRIM(RoundSigDigits(SHR,3))//  &
         '], entered by the user or autosized (see *.eio file),')
      CALL ShowContinueError('may be causing this. The line defined by the coil rated inlet air conditions')
      CALL ShowContinueError('(26.7C drybulb and 19.4C wetbulb) and the RatedSHR (i.e., slope of the line) must intersect')
      CALL ShowContinueError('the saturation curve of the psychrometric chart. If the RatedSHR is too low, then this')
      CALL ShowContinueError('intersection may not occur and the coil bypass factor calculation will not converge.')
      CALL ShowContinueError('If autosizing the SHR, recheck the design supply air humidity ratio and design supply air')
      CALL ShowContinueError('temperature values in the Sizing:System and Sizing:Zone objects. In general, the temperatures')
      CALL ShowContinueError('and humidity ratios specified in these two objects should be the same for each system')
      CALL ShowContinueError('and the zones that it serves.')
      CALL ShowContinueErrorTimeStamp(' ')
      CBFErrors=.true.  ! Didn't converge within MaxIter iterations
    ENDIF
    IF (CBF .lt. 0.0d0) THEN
      CALL ShowSevereError(TRIM(UnitType)//' "'//TRIM(UnitName)//'" -- negative coil bypass factor calculated.')
      CALL ShowContinueErrorTimeStamp(' ')
      CBFErrors=.true. ! Negative CBF not valid
    ENDIF
  END IF

! Show fatal error for specific coil that caused a CBF error
  IF (CBFErrors) THEN
    CALL ShowFatalError(TRIM(UnitType)//' "'//TRIM(UnitName)//&
                        '" Errors found in calculating coil bypass factors')
  END IF

  RETURN
END FUNCTION CalcCBF

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

END MODULE VariableSpeedCoils


