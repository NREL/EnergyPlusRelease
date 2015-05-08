
MODULE PlantChillers

          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   September 2000
          !       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002
          !                      Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  Edwin: Merged Four Chiller Modules Into One

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of the Electric vapor
          ! compression Chillers, Gas Turbine Chillers, Engine Drivent chillers, and
          ! Constant COP chillers

          ! METHODOLOGY EMPLOYED:
          ! Called by plantloopequipment, model accepts inputs, and calculates a
          ! thermal response using new plant routines such as SetComponentFlowRate

          ! REFERENCES:
          ! 1. BLAST Users Manual

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals ,    ONLY : MaxNameLength, NumOfTimeStepInHour, InitConvTemp, WarmupFlag
USE DataInterfaces
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE DataPlant,       ONLY: DeltaTemptol, TypeOf_Chiller_EngineDriven, TypeOf_Chiller_Electric, &
                                         TypeOf_Chiller_CombTurbine,  TypeOf_Chiller_ConstCOP
USE General,         ONLY: TrimSigDigits

IMPLICIT NONE
PRIVATE

          !MODULE PARAMETER DEFINITIONS:
! Parameters for use in Chillers
INTEGER, PARAMETER :: AirCooled = 1
INTEGER, PARAMETER :: WaterCooled = 2
INTEGER, PARAMETER :: EvapCooled = 3
REAL(r64), PARAMETER   :: KJtoJ = 1000.d0        !convert Kjoules to joules

!chiller flow modes
INTEGER, PARAMETER :: FlowModeNotSet           = 200
INTEGER, PARAMETER :: ConstantFlow             = 201
INTEGER, PARAMETER :: NotModulated             = 202
INTEGER, PARAMETER :: LeavingSetpointModulated = 203

          ! MODULE VARIABLE DECLARATIONS:
INTEGER, SAVE ,PUBLIC    :: NumElectricChillers    =0   ! number of Electric chillers specified in input
REAL(r64)                :: CondMassFlowRate       =0.0d0 ! Kg/s - condenser mass flow rate, water side
REAL(r64)                :: EvapMassFlowRate       =0.0d0 ! Kg/s - evaporator mass flow rate, water side
REAL(r64)                :: CondOutletTemp         =0.0d0 ! C - condenser outlet temperature, air or water side
REAL(r64)                :: CondOutletHumRat       =0.0d0 ! kg/kg - condenser outlet humditiy ratio, air side
REAL(r64)                :: EvapOutletTemp         =0.0d0 ! C - evaporator outlet temperature, water side
REAL(r64)                :: Power                  =0.0d0 ! W - rate of chiller energy use
REAL(r64)                :: QEvaporator            =0.0d0 ! W - rate of heat transfer to the evaporator coil
REAL(r64)                :: QCondenser             =0.0d0 ! W - rate of heat transfer to the condenser coil
REAL(r64)                :: Energy                 =0.0d0 ! J - chiller energy use
REAL(r64)                :: EvaporatorEnergy       =0.0d0 ! J - rate of heat transfer to the evaporator coil
REAL(r64)                :: CondenserEnergy        =0.0d0 ! J - rate of heat transfer to the condenser coil
REAL(r64)                :: QHeatRecovered         =0.0d0 ! W - rate of heat transfer to the Heat Recovery coil
REAL(r64)                :: HeatRecOutletTemp      =0.0d0 ! C - Heat Rec outlet temperature, water side
REAL(r64)                :: AvgCondSinkTemp        =0.d0!  condenser temperature value for use in curves [C]
REAL(r64)                :: ChillerCyclingRatio    =0.0d0 ! Cycling ratio for chiller when load is below MinPLR
REAL(r64)                :: BasinHeaterPower       =0.0d0 ! Basin heater power (W)

!engine driven:
INTEGER                  :: NumEngineDrivenChillers =0   ! number of EngineDriven chillers specified in input
REAL(r64)                :: HeatRecInletTemp        =0.0d0 ! Inlet Temperature of the heat recovery fluid
REAL(r64)                :: HeatRecMdotActual       =0.0d0 ! reporting: Heat Recovery Loop Mass flow rate
REAL(r64)                :: HeatRecMdotDesign       =0.0d0
REAL(r64)                :: QTotalHeatRecovered     =0.0d0 ! total heat recovered (W)
REAL(r64)                :: QJacketRecovered        =0.0d0 ! heat recovered from jacket (W)
REAL(r64)                :: QLubeOilRecovered       =0.0d0 ! heat recovered from lube (W)
REAL(r64)                :: QExhaustRecovered       =0.0d0 ! exhaust gas heat recovered (W)
REAL(r64)                :: FuelEnergyUseRate       =0.0d0 ! Fuel Energy used (W)
REAL(r64)                :: TotalHeatEnergyRec      =0.0d0 ! total heat recovered (J)
REAL(r64)                :: JacketEnergyRec         =0.0d0 ! heat recovered from jacket (J)
REAL(r64)                :: LubeOilEnergyRec        =0.0d0 ! heat recovered from lube (J)
REAL(r64)                :: ExhaustEnergyRec        =0.0d0 ! exhaust gas heat recovered (J)
REAL(r64)                :: FuelEnergy              =0.0d0 ! Fuel Energy used (J)
REAL(r64)                :: FuelMdot                =0.0d0 ! Fuel Amount used (Kg/s)
REAL(r64)                :: ExhaustStackTemp        =0.0d0 ! Exhaust Stack Temperature (C)

!gas turbine
INTEGER                  :: NumGTChillers    =0   ! number of GT chillers specified in input

! const COP
INTEGER                  :: NumConstCOPChillers =0
REAL(r64)                :: EvapInletTemp       =0.0d0
REAL(r64)                :: CondInletTemp       =0.0d0

TYPE BaseChillerSpecs
  CHARACTER(len=MaxNameLength) :: Name     =' ' ! user identifier
  INTEGER           :: CondenserType       =0    ! Type of Condenser - Air or Water Cooled
  REAL(r64)         :: NomCap            =0.0d0 ! design nominal capacity of chiller
  REAL(r64)         :: COP               =0.0d0 ! COP
  INTEGER           :: FlowMode          = FlowModeNotSet ! one of 3 modes for componet flow during operation
  LOGICAL           :: ModulatedFlowSetToLoop= .FALSE. ! True if the setpoint is missing at the outlet node
  LOGICAL           :: ModulatedFlowErrDone  = .FALSE.  ! true if setpoint warning issued
  LOGICAL           :: HRSPErrDone          = .FALSE.  ! TRUE if set point warning issued for heat recovery loop
  INTEGER           :: EvapInletNodeNum  =0   ! Node number on the inlet side of the plant
  INTEGER           :: EvapOutletNodeNum =0   ! Node number on the outlet side of the plant
  INTEGER           :: CondInletNodeNum  =0   ! Node number on the inlet side of the condenser
  INTEGER           :: CondOutletNodeNum =0   ! Node number on the outlet side of the condenser
  REAL(r64)         :: EvapVolFlowRate     =0.0d0 ! m**3/s - design nominal water volumetric flow rate through the evaporator
  REAL(r64)         :: EvapMassFlowRateMax =0.0d0 ! kg/s - design water mass flow rate through evaporator
  REAL(r64)         :: CondVolFlowRate     =0.0d0 ! m**3/s - design nominal water volumetric flow rate through the condenser
  REAL(r64)         :: CondMassFlowRateMax =0.0d0 ! kg/s - design water mass flow rate through condenser
  INTEGER           :: CWLoopNum     = 0  ! chilled water plant loop index number
  INTEGER           :: CWLoopSideNum = 0  ! chilled water plant loop side index
  INTEGER           :: CWBranchNum   = 0  ! chilled water plant loop branch index
  INTEGER           :: CWCompNum     = 0  ! chilled water plant loop component index
  INTEGER           :: CDLoopNum     = 0  ! condenser water plant loop index number
  INTEGER           :: CDLoopSideNum = 0  ! condenser water plant loop side index
  INTEGER           :: CDBranchNum   = 0  ! condenser water plant loop branch index
  INTEGER           :: CDCompNum     = 0  ! condenser water plant loop component index

  REAL(r64)         :: SizFac                    = 0.0d0 ! sizing factor
  REAL(r64)         :: BasinHeaterPowerFTempDiff = 0.0d0 ! Basin heater capacity per degree C below setpoint (W/C)
  REAL(r64)         :: BasinHeaterSetPointTemp   = 0.0d0 ! Setpoint temperature for basin heater operation (C)
  INTEGER           :: BasinHeaterSchedulePtr  = 0   ! Pointer to basin heater schedule

  INTEGER           :: ErrCount1 = 0      ! for recurring error messages
  INTEGER           :: ErrCount2 = 0      ! for recurring error messages
  CHARACTER(len=220):: MsgBuffer1    = ' ' !- buffer to print warning messages on following time step
  CHARACTER(len=300):: MsgBuffer2    = ' ' !- buffer to print warning messages on following time step
  REAL(r64)         :: MsgDataLast   = 0.0d0 ! value of data when warning occurred (passed to Recurring Warn)
  LOGICAL           :: PrintMessage  = .FALSE. ! logical to determine if message is valid
  INTEGER           :: MsgErrorCount = 0   ! number of occurrences of warning

  LOGICAL           :: CheckEquipName = .TRUE.
  LOGICAL           :: PossibleSubCooling = .FALSE. ! flag to indicate chiller is doing less cooling that requested
  INTEGER           :: CondMassFlowIndex = 0
END TYPE

TYPE, PUBLIC             :: ElectricChillerSpecs
  TYPE(BaseChillerSpecs) :: Base

  REAL(r64)         :: MinPartLoadRat      =0.0d0 ! (Electric MIN) min allowed operating frac full load
  REAL(r64)         :: MaxPartLoadRat      =0.0d0 ! (Electric MAX) max allowed operating frac full load
  REAL(r64)         :: OptPartLoadRat      =0.0d0 ! (Electric BEST) optimal operating frac full load
  REAL(r64)         :: TempDesCondIn       =0.0d0 ! C - (Electric ADJTC(1)The design secondary loop fluid
                                                ! temperature at the chiller condenser side inlet
  REAL(r64)         :: TempRiseCoef        =0.0d0 ! (Electric ADJTC(2)) correction factor for off ChillDesign oper.
  REAL(r64)         :: TempDesEvapOut      =0.0d0 ! C - (Electric ADJTC(3)The design primary loop fluid
                                                ! temperature at the chiller evaporator side outlet
  REAL(r64),DIMENSION(3) :: CapRatCoef          =0.0d0 ! (Electric RCAVC() ) coeff of cap ratio poly fit
  REAL(r64),DIMENSION(3) :: PowerRatCoef        =0.0d0 ! (Electric ADJEC() ) coeff of power rat poly fit
  REAL(r64),DIMENSION(3) :: FullLoadCoef        =0.0d0 ! (Electric RPWRC() ) coeff of full load poly. fit
  REAL(r64)         :: TempLowLimitEvapOut =0.0d0 ! C - low temperature shut off
  REAL(r64)         :: DesignHeatRecVolFlowRate = 0.0d0 ! m3/s, Design Water mass flow rate through heat recovery loop
  REAL(r64)         :: DesignHeatRecMassFlowRate = 0.0d0 ! kg/s, Design Water mass flow rate through heat recovery loop
  LOGICAL           :: HeatRecActive = .False.    ! True entered Heat Rec Vol Flow Rate >0
  INTEGER           :: HeatRecInletNodeNum = 0    ! Node number on the heat recovery inlet side of the condenser
  INTEGER           :: HeatRecOutletNodeNum = 0   ! Node number on the heat recovery outlet side of the condenser
  REAL(r64)         :: HeatRecCapacityFraction   = 0.d0 ! user input for heat recovery capacity fraction []
  REAL(r64)         :: HeatRecMaxCapacityLimit   = 0.d0 ! Capacity limit for Heat recovery, one time calc [W]
  INTEGER           :: HeatRecSetpointNodeNum    = 0    ! index for system node with the heat recover leaving setpoint
  INTEGER           :: HeatRecInletLimitSchedNum = 0    ! index for schedule for the inlet high limit for heat recovery operation
  INTEGER           :: HRLoopNum     = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRLoopSideNum = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRBranchNum   = 0  ! heat recovery water plant loop branch index
  INTEGER           :: HRCompNum     = 0  ! heat recovery water plant loop component index
END TYPE ElectricChillerSpecs

TYPE EngineDrivenChillerSpecs
  TYPE(BaseChillerSpecs) :: Base
  CHARACTER(len=MaxNameLength) :: FuelType  =' '  ! Type of Fuel - DIESEL, GASOLINE, GAS


  REAL(r64)         :: MinPartLoadRat       =0.0d0 ! (EngineDriven MIN) min allowed operating frac full load
  REAL(r64)         :: MaxPartLoadRat       =0.0d0 ! (EngineDriven MAX) max allowed operating frac full load
  REAL(r64)         :: OptPartLoadRat       =0.0d0 ! (EngineDriven BEST) optimal operating frac full load
  REAL(r64)         :: TempDesCondIn        =0.0d0 ! C - (EngineDriven ADJTC(1)The design secondary loop fluid
                                                 ! temperature at the chiller condenser side inlet
  REAL(r64)         :: TempRiseCoef         =0.0d0 ! (EngineDriven ADJTC(2)) correction factor for off ChillDesign oper.
  REAL(r64)         :: TempDesEvapOut       =0.0d0 ! C - (EngineDriven ADJTC(3)The design primary loop fluid
                                                 ! temperature at the chiller evaporator side outlet
  REAL(r64),DIMENSION(3) :: CapRatCoef           =0.0d0 ! (EngineDriven RCAVC() ) coeff of cap ratio poly fit
  REAL(r64),DIMENSION(3) :: PowerRatCoef         =0.0d0 ! (EngineDriven ADJEC() ) coeff of power rat poly fit
  REAL(r64),DIMENSION(3) :: FullLoadCoef         =0.0d0 ! (EngineDriven RPWRC() ) coeff of full load poly. fit
  REAL(r64)         :: TempLowLimitEvapOut  =0.0d0 ! C - low temperature shut off

  INTEGER           :: ClngLoadtoFuelCurve   =0   !Coeff of Shaft Power to Fuel Energy Input Coeff Poly Fit
  INTEGER           :: RecJacHeattoFuelCurve =0   !Curve Index for Ratio of Recoverable Jacket Heat to
  INTEGER           :: RecLubeHeattoFuelCurve=0   !Curve Index for Ratio of Recoverable Lube Oil Heat to
  INTEGER           :: TotExhausttoFuelCurve =0   !Curve Index for Total Exhaust heat Input to Fuel Energy Input Coeffs Poly Fit
  REAL(r64)         :: ExhaustTemp           =0.0d0 !(TEXDC) Exhaust Gas Temp to Fuel Energy Input
  INTEGER           :: ExhaustTempCurve      =0   !Curve Index for Exhaust Gas Temp to Fuel Energy Input Coeffs Poly Fit
  REAL(r64)         :: UA                    =0.0d0 !(UACDC) exhaust gas Heat Exchanger UA to Capacity
  REAL(r64),DIMENSION(2) :: UACoef                =0.0d0   !Heat Exchanger UA Coeffs Poly Fit
  REAL(r64)         :: MaxExhaustperPowerOutput =0.0d0 !MAX EXHAUST FLOW PER W DSL POWER OUTPUT COEFF
  REAL(r64)         :: DesignMinExitGasTemp     =0.0d0 !Steam Saturation Temperature
  REAL(r64)         :: FuelHeatingValue         =0.0d0 ! Heating Value of Fuel in kJ/kg
  REAL(r64)         :: DesignHeatRecVolFlowRate =0.0d0 ! m3/s, Design Water mass flow rate through heat recovery loop
  REAL(r64)         :: DesignHeatRecMassFlowRate=0.0d0 ! kg/s, Design Water mass flow rate through heat recovery loop
  LOGICAL           :: HeatRecActive        =.false. ! True entered Heat Rec Vol Flow Rate >0
  INTEGER           :: HeatRecInletNodeNum  =0  ! Node number on the heat recovery inlet side of the condenser
  INTEGER           :: HeatRecOutletNodeNum =0  ! Node number on the heat recovery outlet side of the condenser
  REAL(r64)         :: HeatRecMaxTemp       =0.0d0 !Max Temp that can be produced in heat recovery
  INTEGER           :: HRLoopNum     = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRLoopSideNum = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRBranchNum   = 0  ! heat recovery water plant loop branch index
  INTEGER           :: HRCompNum     = 0  ! heat recovery water plant loop component index
END TYPE EngineDrivenChillerSpecs

TYPE GTChillerSpecs
  TYPE(BaseChillerSpecs) :: Base

  CHARACTER(len=MaxNameLength) :: FuelType =' '  ! Type of Fuel - DIESEL, GASOLINE, GAS

  REAL(r64)         :: MinPartLoadRat      =0.0d0  ! (GT MIN) min allowed operating frac full load
  REAL(r64)         :: MaxPartLoadRat      =0.0d0  ! (GT MAX) max allowed operating frac full load
  REAL(r64)         :: OptPartLoadRat      =0.0d0  ! (GT BEST) optimal operating frac full load
  REAL(r64)         :: TempDesCondIn       =0.0d0  ! C - (GT ADJTC(1)The design secondary loop fluid
                                                 ! temperature at the chiller condenser side inlet
  REAL(r64)         :: TempRiseCoef        =0.0d0  ! (GT ADJTC(2)) correction factor for off ChillDesign oper.
  REAL(r64)         :: TempDesEvapOut      =0.0d0  ! C - (GT ADJTC(3)The design primary loop fluid
                                                 ! temperature at the chiller evaporator side outlet
  REAL(r64),DIMENSION(3) :: CapRatCoef     =0.0d0  ! (GT RCAVC() ) coeff of cap ratio poly fit
  REAL(r64),DIMENSION(3) :: PowerRatCoef   =0.0d0  ! (GT ADJEC() ) coeff of power rat poly fit
  REAL(r64),DIMENSION(3) :: FullLoadCoef   =0.0d0  ! (GT RPWRC() ) coeff of full load poly. fit
  REAL(r64)         :: TempLowLimitEvapOut =0.0d0  ! C - low temperature shut off

  ! "special" GT chiller input parameters
  REAL(r64)         :: FuelEnergyIn            =0.0d0 !(EFUEL) Amount of Fuel Energy Required to run gas turbine
  REAL(r64),DIMENSION(3) :: PLBasedFuelInputCoef    =0.0d0 !(FUL1GC) Part Load Ratio Based Fuel Input Coefficients Poly Fit
  REAL(r64),DIMENSION(3) :: TempBasedFuelInputCoef  =0.0d0 !(FUL2GC) Ambient Temperature Based Fuel Input Coeff Poly Fit

  REAL(r64)         :: ExhaustFlow             =0.0d0  !(FEX) Exhaust Gas Flow Rate cubic meters per second
  REAL(r64),DIMENSION(3) :: ExhaustFlowCoef    =0.0d0  !(FEXGC) Exhaust Gas Flow Rate Input Coef Poly Fit

  REAL(r64)         :: ExhaustTemp             =0.0d0  !(TEX) Exhaust Gas Temperature in C
  REAL(r64),DIMENSION(3) :: PLBasedExhaustTempCoef  =0.0d0  !(TEX1GC) Part Load Ratio Based Exhaust Temperature Input Coeffs Poly Fit
  REAL(r64),DIMENSION(3) :: TempBasedExhaustTempCoef=0.0d0  !(TEX2GC) Ambient Temperature Based Exhaust Gas Temp to
                                                          ! Fuel Energy Input Coeffs Poly Fit

  REAL(r64)         :: HeatRecLubeEnergy       =0.0d0  !(ELUBE) Recoverable Lube Oil Energy
  REAL(r64)         :: HeatRecLubeRate         =0.0d0  !(ELUBE) Recoverable Lube Oil Rate of Rwecovery (W)
  REAL(r64),DIMENSION(3) :: HeatRecLubeEnergyCoef   =0.0d0  !(ELUBEGC)  Recoverable Lube Oil Energy Input Coef Poly Fit

  REAL(r64)         :: UAtoCapRat              =0.0d0  !(UACGC) Heat Exchanger UA to Capacity
  REAL(r64),DIMENSION(3) :: UAtoCapCoef        =0.0d0  !Heat Exchanger UA to Capacity Coeffs Poly Fit

  REAL(r64)         :: GTEngineCapacity        =0.0d0  ! Capacity of GT Unit attached to Chiller
  REAL(r64)         :: MaxExhaustperGTPower    =0.0d0  !Max Exhaust Flow per KW Power Out
  REAL(r64)         :: DesignSteamSatTemp      =0.0d0  !Steam Saturation Temperature
  REAL(r64)         :: ExhaustStackTemp        =0.0d0  !Temperature of Exhaust Gases

  INTEGER           :: HeatRecInletNodeNum     =0    ! Node number on the heat recovery inlet side of the condenser
  INTEGER           :: HeatRecOutletNodeNum    =0    ! Node number on the heat recovery outlet side of the condenser

  REAL(r64)         :: HeatRecInletTemp        =0.0d0  !Inlet Temperature of the heat recovery fluid
  REAL(r64)         :: HeatRecOutletTemp       =0.0d0  !Outlet Temperature of the heat recovery fluid
  REAL(r64)         :: HeatRecMdot             =0.0d0  ! reporting: Heat Recovery Loop Mass flow rate
  REAL(r64)         :: DesignHeatRecVolFlowRate=0.0d0    ! m3/s, Design Water mass flow rate through heat recovery loop
  REAL(r64)         :: DesignHeatRecMassFlowRate=0.0d0   ! kg/s, Design Water mass flow rate through heat recovery loop
  LOGICAL           :: HeatRecActive        =.false. ! True entered Heat Rec Vol Flow Rate >0
  REAL(r64)         :: FuelHeatingValue        =0.0d0   !Heating Value of Fuel in kJ/kg
  REAL(r64)         :: HeatRecMaxTemp          =0.0d0  !Max Temp that can be produced in heat recovery
  INTEGER           :: HRLoopNum     = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRLoopSideNum = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRBranchNum   = 0  ! heat recovery water plant loop branch index
  INTEGER           :: HRCompNum     = 0  ! heat recovery water plant loop component index
END TYPE GTChillerSpecs

  ! DERIVED TYPE DEFINITIONS
TYPE ConstCOPChillerSpecs
   TYPE(BaseChillerSpecs) :: Base
END TYPE ConstCOPChillerSpecs

TYPE BaseReportVars
  REAL(r64)    :: Power          = 0.0d0 !
  REAL(r64)    :: QEvap          = 0.0d0 !
  REAL(r64)    :: QCond          = 0.0d0 !
  REAL(r64)    :: Energy         = 0.0d0 !
  REAL(r64)    :: EvapEnergy     = 0.0d0 !
  REAL(r64)    :: CondEnergy     = 0.0d0 !
  REAL(r64)    :: CondInletTemp  = 0.0d0 !
  REAL(r64)    :: EvapInletTemp  = 0.0d0 !
  REAL(r64)    :: CondOutletTemp = 0.0d0 !
  REAL(r64)    :: EvapOutletTemp = 0.0d0 !
  REAL(r64)    :: Evapmdot       = 0.0d0 !
  REAL(r64)    :: Condmdot       = 0.0d0 !
  REAL(r64)    :: BasinHeaterPower       = 0.0d0  ! Basin heater power (W)
  REAL(r64)    :: BasinHeaterConsumption = 0.0d0  ! Basin heater energy consumption (J)
END TYPE BaseReportVars

TYPE ElectricReportVars
  TYPE(BaseReportVars) :: Base
  REAL(r64)    :: ActualCOP          = 0.d0 !
  REAL(r64)    :: QHeatRecovery      = 0.d0
  REAL(r64)    :: EnergyHeatRecovery = 0.d0
  REAL(r64)    :: HeatRecInletTemp   = 0.d0
  REAL(r64)    :: HeatRecOutletTemp  = 0.d0
  REAL(r64)    :: HeatRecMassFlow    = 0.d0
  REAL(r64)    :: ChillerCondAvgTemp = 0.d0 ! the effective condenser temperature for chiller performance [C]
END TYPE ElectricReportVars

TYPE EngineDrivenReportVars
  TYPE(BaseReportVars) :: Base
  REAL(r64)    :: QJacketRecovered       = 0.0d0 ! reporting: Heat Recovered from Jacket (W)
  REAL(r64)    :: QLubeOilRecovered      = 0.0d0 ! reporting: Heat Recovered from Lubricant (W)
  REAL(r64)    :: QExhaustRecovered      = 0.0d0 ! reporting: exhaust gas heat recovered (W)
  REAL(r64)    :: QTotalHeatRecovered    = 0.0d0 ! reporting: Total Heat Recovered (W)
  REAL(r64)    :: TotalHeatEnergyRec     = 0.0d0 ! reporting: total heat recovered (J)
  REAL(r64)    :: JacketEnergyRec        = 0.0d0 ! reporting: heat recovered from jacket (J)
  REAL(r64)    :: LubeOilEnergyRec       = 0.0d0 ! reporting: heat recovered from lube (J)
  REAL(r64)    :: ExhaustEnergyRec       = 0.0d0 ! reporting: exhaust gas heat recovered (J)
  REAL(r64)    :: FuelEnergy             = 0.0d0 ! reporting: Fuel Energy used (J)
  REAL(r64)    :: FuelEnergyUseRate      = 0.0d0 ! reporting: Fuel Energy used (W)
  REAL(r64)    :: FuelMdot               = 0.0d0 ! reporting: Fuel used (Kg/s)
  REAL(r64)    :: ExhaustStackTemp       = 0.0d0 ! reporting: Exhaust Stack Temperature (C)
  REAL(r64)    :: HeatRecInletTemp       = 0.0d0 ! reporting: Heat Recovery Loop Inlet Temperature (C)
  REAL(r64)    :: HeatRecOutletTemp      = 0.0d0 ! reporting: Heat Recovery Loop Outlet Temperature (C)
  REAL(r64)    :: HeatRecMdot            = 0.0d0 ! reporting: Heat Recovery Loop Mass flow rate (kg/s)
  REAL(r64)    :: FuelCOP                = 0.0d0 ! reporting: Fuel COP [delivered cooling rate/fuel energy input rate] (W/W)
END TYPE EngineDrivenReportVars

TYPE GasTurbineReportVars
  TYPE(BaseReportVars) :: Base
  REAL(r64)    :: HeatRecLubeEnergy    = 0.0d0 ! reporting: Heat Recovered from Lubricant(J)
  REAL(r64)    :: HeatRecLubeRate      = 0.0d0 ! reporting: Recoverable Lube Oil Rate of Rwecovery (W)
  REAL(r64)    :: FuelEnergyUsed       = 0.0d0 ! reporting: Fuel Energy used
  REAL(r64)    :: FuelEnergyUsedRate   = 0.0d0 ! reporting: Fuel energy used rate (fuel consumption rate)
  REAL(r64)    :: FuelMassUsed         = 0.0d0 ! reporting: Fuel Amount used
  REAL(r64)    :: FuelMassUsedRate     = 0.0d0 ! reporting: Fuel amount used (fuel Mass consumption rate)
  REAL(r64)    :: ExhaustStackTemp     = 0.0d0 ! reporting: Exhaust Stack Temperature
  REAL(r64)    :: HeatRecInletTemp     = 0.0d0 ! reporting: Heat Recovery Loop Inlet Temperature
  REAL(r64)    :: HeatRecOutletTemp    = 0.0d0 ! reporting: Heat Recovery Loop Outlet Temperature
  REAL(r64)    :: HeatRecMdot          = 0.0d0 ! reporting: Heat Recovery Loop Mass flow rate
  REAL(r64)    :: FuelCOP              = 0.0d0 ! reporting: Fuel coefficient of performance (Qevap/FuelEnergyUsedRate)
END TYPE GasTurbineReportVars

TYPE ConstCOPReportVars
    TYPE(BaseReportVars) :: Base
  REAL(r64)    :: ActualCOP      = 0.0d0 !
END TYPE ConstCOPReportVars

TYPE (ElectricChillerSpecs), ALLOCATABLE, PUBLIC, DIMENSION(:)  :: ElectricChiller  !dimension to number of machines
TYPE(ElectricReportVars), ALLOCATABLE, DIMENSION(:) ::ElectricChillerReport

TYPE (EngineDrivenChillerSpecs), ALLOCATABLE, DIMENSION(:)  :: EngineDrivenChiller  !dimension to number of machines
TYPE(EngineDrivenReportVars), ALLOCATABLE, DIMENSION(:) ::EngineDrivenChillerReport

TYPE (GTChillerSpecs), ALLOCATABLE, DIMENSION(:)  :: GTChiller  !dimension to number of machines
TYPE (GasTurbineReportVars), ALLOCATABLE, DIMENSION(:) ::GTChillerReport

TYPE (ConstCOPChillerSpecs), ALLOCATABLE, DIMENSION(:)  :: ConstCOPChiller         !dimension to number of machines
TYPE (ConstCOPReportVars), ALLOCATABLE,DIMENSION(:) :: ConstCOPChillerReport

LOGICAL     :: GetEngineDrivenInput = .TRUE.! then TRUE, calls subroutine to read input file.
LOGICAL     :: GetElectricInput = .TRUE.! then TRUE, calls subroutine to read input file.
LOGICAL     :: GetGasTurbineInput = .TRUE.! then TRUE, calls subroutine to read input file.
LOGICAL     :: GetConstCOPInput = .TRUE.

!Merged routines
PUBLIC     SimChiller


          ! Electric Chiller
PRIVATE    CalcElectricChillerModel
PRIVATE    GetElectricChillerInput
PRIVATE    InitElectricChiller
PRIVATE    SizeElectricChiller
PRIVATE    UpdateElectricChillerRecords
PRIVATE    CalcElectricChillerHeatRecovery

          ! Engine Driven Chiller
PRIVATE    CalcEngineDrivenChillerModel
PRIVATE    CalcEngineChillerHeatRec
PRIVATE    GetEngineDrivenChillerInput
PRIVATE    InitEngineDrivenChiller
PRIVATE    SizeEngineDrivenChiller
PRIVATE    UpdateEngineDrivenChiller

          ! Gas Turbine Chiller
PRIVATE    CalcGTChillerModel
PRIVATE    GetGTChillerInput
PRIVATE    InitGTChiller
PRIVATE    SizeGTChiller
PRIVATE    UpdateGTChillerRecords

          ! Const COP
PRIVATE    CalcConstCOPChillerModel
PRIVATE    GetConstCOPChillerInput
PRIVATE    InitConstCOPChiller
PRIVATE    UpdateConstCOPChillerRecords
PRIVATE    SizeConstCOPChiller

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Electric Chiller Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimChiller(LoopNum, LoopSide, ChillerType,ChillerName,EquipFlowCtrl,CompIndex,RunFlag,FirstHVACIteration, &
                              InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor,TempCondInDesign,  &
                              TempEvapOutDesign)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       April 1999, May 200-Taecheol Kim
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the Electric chiller model driver.  It
               ! gets the input for the models, initializes simulation variables, call
               ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE PlantUtilities, ONLY: UpdateChillerComponentCondenserSide, UpdateComponentHeatRecoverySide

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: LoopNum  ! Flow control mode for the equipment
  INTEGER, INTENT(IN)      :: LoopSide            ! chiller number pointer
  INTEGER, INTENT(IN)  :: ChillerType   ! type of chiller
  CHARACTER(len=*), INTENT(IN)  :: ChillerName   ! user specified name of chiller
  INTEGER, INTENT(IN)       :: EquipFlowCtrl  ! Flow control mode for the equipment
  INTEGER, INTENT(INOUT)    :: CompIndex            ! chiller number pointer
  LOGICAL , INTENT(IN)      :: RunFlag             ! simulate chiller when TRUE
  LOGICAL , INTENT(IN)      :: FirstHVACIteration      ! initialize variables when TRUE
  LOGICAL, INTENT(INOUT)    :: InitLoopEquip            ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)    :: MyLoad              ! loop demand component will meet
  REAL(r64)         :: MinCap           ! W - minimum operating capacity of chiller
  REAL(r64)         :: MaxCap           ! W - maximum operating capacity of chiller
  REAL(r64)         :: OptCap           ! W - optimal operating capacity of chiller
  LOGICAL, INTENT(IN)         :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(INOUT)    :: SizingFactor     ! sizing factor
  REAL(r64), INTENT(INOUT)    :: TempCondInDesign     !design condenser inlet temperature, water side
  REAL(r64), INTENT(INOUT)    :: TempEvapOutDesign    !design evaporator outlet temperature, water side
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ChillNum            ! chiller number pointer

  SELECT CASE (ChillerType)

  CASE (TypeOf_Chiller_Electric)

            !Get chiller data from input file
      IF (GetElectricInput) THEN
        CALL GetElectricChillerInput
        GetElectricInput = .FALSE.
      END IF

        ! Find the correct Chiller
      IF (CompIndex == 0) THEN
        ChillNum = FindItemInList(ChillerName,ElectricChiller%Base%Name,NumElectricChillers)
        IF (ChillNum == 0) THEN
          CALL ShowFatalError('SimElectricChiller: Specified Chiller not one of Valid Electric Chillers='//TRIM(ChillerName))
        ENDIF
        CompIndex=ChillNum
      ELSE
        ChillNum=CompIndex
        IF (ChillNum > NumElectricChillers .or. ChillNum < 1) THEN
          CALL ShowFatalError('SimElectricChiller:  Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(ChillNum))// &
                              ', Number of Units='//TRIM(TrimSigDigits(NumElectricChillers))//  &
                              ', Entered Unit name='//TRIM(ChillerName))
        ENDIF
        IF (ElectricChiller(ChillNum)%Base%CheckEquipName) THEN
          IF (ChillerName /= ElectricChiller(ChillNum)%Base%Name) THEN
            CALL ShowFatalError('SimElectricChiller: Invalid CompIndex passed='//  &
                                TRIM(TrimSigDigits(ChillNum))// &
                                ', Unit name='//TRIM(ChillerName)//', stored Unit Name for that index='//  &
                                TRIM(ElectricChiller(ChillNum)%Base%Name))
          ENDIF
          ElectricChiller(ChillNum)%Base%CheckEquipName=.false.
        ENDIF
      ENDIF

      IF (InitLoopEquip) THEN
        TempEvapOutDesign  = ElectricChiller(ChillNum)%TempDesEvapOut
        TempCondInDesign   = ElectricChiller(ChillNum)%TempDesCondIn
        CALL InitElectricChiller(ChillNum,RunFlag,MyLoad)
        CALL SizeElectricChiller(ChillNum)
        IF (LoopNum == ElectricChiller(ChillNum)%Base%CWLoopNum) THEN ! chilled water loop
          MinCap = ElectricChiller(ChillNum)%Base%NomCap*ElectricChiller(ChillNum)%MinPartLoadRat
          MaxCap = ElectricChiller(ChillNum)%Base%NomCap*ElectricChiller(ChillNum)%MaxPartLoadRat
          OptCap = ElectricChiller(ChillNum)%Base%NomCap*ElectricChiller(ChillNum)%OptPartLoadRat
        ELSE
          MinCap = 0.d0
          MaxCap = 0.d0
          OptCap = 0.d0
        ENDIF
        IF (GetSizingFactor) THEN
          SizingFactor = ElectricChiller(ChillNum)%Base%SizFac
        END IF
        RETURN
      END IF

        ! calculate model depending on where called from
      IF (LoopNum == ElectricChiller(ChillNum)%Base%CWLoopNum) THEN ! chilled water loop

        CALL InitElectricChiller(ChillNum,RunFlag,MyLoad)
        CALL CalcElectricChillerModel(ChillNum,MyLoad,EquipFlowCtrl,Runflag)
        CALL UpdateElectricChillerRecords(MyLoad,RunFlag,ChillNum)

      ELSEIF (LoopNum == ElectricChiller(ChillNum)%Base%CDLoopNum) THEN ! condenser loop
        CALL UpdateChillerComponentCondenserSide(ElectricChiller(ChillNum)%Base%CDLoopNum, &
                                         ElectricChiller(ChillNum)%Base%CDLoopSideNum,     &
                                         TypeOf_Chiller_Electric,                     &
                                         ElectricChiller(ChillNum)%Base%CondInletNodeNum,  &
                                         ElectricChiller(ChillNum)%Base%CondOutletNodeNum, &
                                         ElectricChillerReport(ChillNum)%Base%QCond,             &
                                         ElectricChillerReport(ChillNum)%Base%CondInletTemp,     &
                                         ElectricChillerReport(ChillNum)%Base%CondOutletTemp,    &
                                         ElectricChillerReport(ChillNum)%Base%Condmdot,          &
                                         FirstHVACIteration)
      ELSEIF (LoopNum == ElectricChiller(ChillNum)%HRLoopNum) THEN  ! heat recovery loop
        CALL UpdateComponentHeatRecoverySide(ElectricChiller(ChillNum)%HRLoopNum,               &
                                        ElectricChiller(ChillNum)%HRLoopSideNum,           &
                                        TypeOf_Chiller_Electric,                           &
                                        ElectricChiller(ChillNum)%HeatRecInletNodeNum,     &
                                        ElectricChiller(ChillNum)%HeatRecOutletNodeNum,    &
                                        ElectricChillerReport(ChillNum)%QHeatRecovery,     &
                                        ElectricChillerReport(ChillNum)%HeatRecInletTemp,  &
                                        ElectricChillerReport(ChillNum)%HeatRecOutletTemp, &
                                        ElectricChillerReport(ChillNum)%HeatRecMassFlow ,  &
                                        FirstHVACIteration)
      ENDIF

  CASE (TypeOf_Chiller_EngineDriven)

        IF (GetEngineDrivenInput) THEN
        CALL GetEngineDrivenChillerInput
        GetEngineDrivenInput = .FALSE.
      END IF

        ! Find the correct Chiller
      IF (CompIndex == 0) THEN
        ChillNum = FindItemInList(ChillerName,EngineDrivenChiller%Base%Name,NumEngineDrivenChillers)
        IF (ChillNum == 0) THEN
          CALL ShowFatalError('SimEngineDrivenChiller: Specified Chiller not one of Valid EngineDriven Chillers='//  &
             TRIM(ChillerName))
        ENDIF
        CompIndex=ChillNum
      ELSE
        ChillNum=CompIndex
        IF (ChillNum > NumEngineDrivenChillers .or. ChillNum < 1) THEN
          CALL ShowFatalError('SimEngineDrivenChiller:  Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(ChillNum))// &
                              ', Number of Units='//TRIM(TrimSigDigits(NumEngineDrivenChillers))//  &
                              ', Entered Unit name='//TRIM(ChillerName))
        ENDIF
        IF (EngineDrivenChiller(ChillNum)%Base%CheckEquipName) THEN
          IF (ChillerName /= EngineDrivenChiller(ChillNum)%Base%Name) THEN
            CALL ShowFatalError('SimEngineDrivenChiller: Invalid CompIndex passed='//  &
                                TRIM(TrimSigDigits(ChillNum))// &
                                ', Unit name='//TRIM(ChillerName)//', stored Unit Name for that index='//  &
                                TRIM(EngineDrivenChiller(ChillNum)%Base%Name))
          ENDIF
          EngineDrivenChiller(ChillNum)%Base%CheckEquipName=.false.
        ENDIF
      ENDIF

      IF (InitLoopEquip) THEN
        TempEvapOutDesign  = EngineDrivenChiller(ChillNum)%TempDesEvapOut
        TempCondInDesign   = EngineDrivenChiller(ChillNum)%TempDesCondIn
        CALL InitEngineDrivenChiller(ChillNum, RunFlag, MyLoad)
        CALL SizeEngineDrivenChiller(ChillNum)
        IF (LoopNum == EngineDrivenChiller(ChillNum)%Base%CWLoopNum) THEN
          MinCap = EngineDrivenChiller(ChillNum)%Base%NomCap*EngineDrivenChiller(ChillNum)%MinPartLoadRat
          MaxCap = EngineDrivenChiller(ChillNum)%Base%NomCap*EngineDrivenChiller(ChillNum)%MaxPartLoadRat
          OptCap = EngineDrivenChiller(ChillNum)%Base%NomCap*EngineDrivenChiller(ChillNum)%OptPartLoadRat
        ELSE
          MinCap = 0.d0
          MaxCap = 0.d0
          OptCap = 0.d0
        ENDIF
        IF (GetSizingFactor) THEN
          SizingFactor = EngineDrivenChiller(ChillNum)%Base%SizFac
        END IF
        RETURN
      END IF

      ! calculate model depending on where called from
      IF (LoopNum == EngineDrivenChiller(ChillNum)%Base%CWLoopNum) THEN ! chilled water loop
        CALL InitEngineDrivenChiller(ChillNum, RunFlag, MyLoad)
        CALL CalcEngineDrivenChillerModel(ChillNum,MyLoad,Runflag,EquipFlowCtrl)
        CALL UpdateEngineDrivenChiller(MyLoad,RunFlag,ChillNum)
      ELSEIF (LoopNum == EngineDrivenChiller(ChillNum)%Base%CDLoopNum) THEN ! condenser loop
        CALL UpdateChillerComponentCondenserSide(EngineDrivenChiller(ChillNum)%Base%CDLoopNum, &
                                         EngineDrivenChiller(ChillNum)%Base%CDLoopSideNum,     &
                                         TypeOf_Chiller_EngineDriven,                     &
                                         EngineDrivenChiller(ChillNum)%Base%CondInletNodeNum,  &
                                         EngineDrivenChiller(ChillNum)%Base%CondOutletNodeNum, &
                                         EngineDrivenChillerReport(ChillNum)%Base%QCond,             &
                                         EngineDrivenChillerReport(ChillNum)%Base%CondInletTemp,     &
                                         EngineDrivenChillerReport(ChillNum)%Base%CondOutletTemp,    &
                                         EngineDrivenChillerReport(ChillNum)%Base%Condmdot,          &
                                         FirstHVACIteration)
      ELSEIF (LoopNum == EngineDrivenChiller(ChillNum)%HRLoopNum) THEN  ! heat recovery loop
        CALL UpdateComponentHeatRecoverySide(EngineDrivenChiller(ChillNum)%HRLoopNum,             &
                                        EngineDrivenChiller(ChillNum)%HRLoopSideNum,              &
                                        TypeOf_Chiller_EngineDriven,                              &
                                        EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum,        &
                                        EngineDrivenChiller(ChillNum)%HeatRecOutletNodeNum,       &
                                        EngineDrivenChillerReport(ChillNum)%QTotalHeatRecovered,  &
                                        EngineDrivenChillerReport(ChillNum)%HeatRecInletTemp,     &
                                        EngineDrivenChillerReport(ChillNum)%HeatRecOutletTemp,    &
                                        EngineDrivenChillerReport(ChillNum)%HeatRecMdot ,         &
                                        FirstHVACIteration)
      ENDIF

  CASE (TypeOf_Chiller_CombTurbine)

        IF (GetGasTurbineInput) THEN
        CALL GetGTChillerInput
        GetGasTurbineInput = .FALSE.
      END IF

      IF (CompIndex == 0) THEN
        ChillNum = FindItemInList(ChillerName,GTChiller%Base%Name,NumGTChillers)
        IF (ChillNum == 0) THEN
          CALL ShowFatalError('SimGTChiller: Specified Chiller not one of Valid Gas Turbine Chillers='//TRIM(ChillerName))
        ENDIF
        CompIndex=ChillNum
      ELSE
        ChillNum=CompIndex
        IF (ChillNum > NumGTChillers .or. ChillNum < 1) THEN
          CALL ShowFatalError('SimGTChiller:  Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(ChillNum))// &
                              ', Number of Units='//TRIM(TrimSigDigits(NumGTChillers))//  &
                              ', Entered Unit name='//TRIM(ChillerName))
        ENDIF
        IF (GTChiller(ChillNum)%Base%CheckEquipName) THEN
          IF (ChillerName /= GTChiller(ChillNum)%Base%Name) THEN
            CALL ShowFatalError('SimGTChiller: Invalid CompIndex passed='//  &
                                TRIM(TrimSigDigits(ChillNum))// &
                                ', Unit name='//TRIM(ChillerName)//', stored Unit Name for that index='//  &
                                TRIM(GTChiller(ChillNum)%Base%Name))
          ENDIF
          GTChiller(ChillNum)%Base%CheckEquipName=.false.
        ENDIF
      ENDIF



      IF (InitLoopEquip) THEN
        TempEvapOutDesign  = GTChiller(ChillNum)%TempDesEvapOut
        TempCondInDesign   = GTChiller(ChillNum)%TempDesCondIn
        CALL InitGTChiller(ChillNum,RunFlag, MyLoad)
        CALL SizeGTChiller(ChillNum)
        IF (LoopNum == GTChiller(ChillNum)%Base%CWLoopNum) THEN
          MinCap = GTChiller(ChillNum)%Base%NomCap*GTChiller(ChillNum)%MinPartLoadRat
          MaxCap = GTChiller(ChillNum)%Base%NomCap*GTChiller(ChillNum)%MaxPartLoadRat
          OptCap = GTChiller(ChillNum)%Base%NomCap*GTChiller(ChillNum)%OptPartLoadRat
        ELSE
          MinCap = 0.d0
          MaxCap = 0.d0
          OptCap = 0.d0
        ENDIF
        IF (GetSizingFactor) THEN
          SizingFactor = GTChiller(ChillNum)%Base%SizFac
        END IF
        RETURN
      END IF

        ! calculate model depending on where called from
      IF (LoopNum == GTChiller(ChillNum)%Base%CWLoopNum) THEN ! chilled water loop

        CALL InitGTChiller(ChillNum,RunFlag, MyLoad)
        CALL CalcGTChillerModel(ChillNum,MyLoad,Runflag,EquipFlowCtrl)
        CALL UpdateGTChillerRecords(MyLoad,RunFlag,ChillNum)

      ELSEIF (LoopNum == GTChiller(ChillNum)%Base%CDLoopNum) THEN ! condenser loop
        CALL UpdateChillerComponentCondenserSide(GTChiller(ChillNum)%Base%CDLoopNum, &
                                         GTChiller(ChillNum)%Base%CDLoopSideNum,     &
                                         TypeOf_Chiller_CombTurbine,                     &
                                         GTChiller(ChillNum)%Base%CondInletNodeNum,  &
                                         GTChiller(ChillNum)%Base%CondOutletNodeNum, &
                                         GTChillerReport(ChillNum)%Base%QCond,             &
                                         GTChillerReport(ChillNum)%Base%CondInletTemp,     &
                                         GTChillerReport(ChillNum)%Base%CondOutletTemp,    &
                                         GTChillerReport(ChillNum)%Base%Condmdot,          &
                                         FirstHVACIteration)
      ELSEIF (LoopNum == GTChiller(ChillNum)%HRLoopNum) THEN  ! heat recovery loop
        CALL UpdateComponentHeatRecoverySide(GTChiller(ChillNum)%HRLoopNum,               &
                                        GTChiller(ChillNum)%HRLoopSideNum,           &
                                        TypeOf_Chiller_CombTurbine,                           &
                                        GTChiller(ChillNum)%HeatRecInletNodeNum,     &
                                        GTChiller(ChillNum)%HeatRecOutletNodeNum,    &
                                        GTChillerReport(ChillNum)%HeatRecLubeRate,      &
                                        GTChillerReport(ChillNum)%HeatRecInletTemp,  &
                                        GTChillerReport(ChillNum)%HeatRecOutletTemp, &
                                        GTChillerReport(ChillNum)%HeatRecMdot ,  &
                                        FirstHVACIteration)
      ENDIF

  CASE (TypeOf_Chiller_ConstCOP)

            !GET INPUT
      IF (GetConstCOPInput)  THEN
        CALL GetConstCOPChillerInput
        GetConstCOPInput = .FALSE.
      END IF

        ! Find the correct Chiller
      IF (CompIndex == 0) THEN
        ChillNum = FindItemInList(ChillerName,ConstCOPChiller%Base%Name,NumConstCOPChillers)
        IF (ChillNum == 0) THEN
          CALL ShowFatalError('SimConstCOPChiller: Specified Chiller not one of Valid Constant COP Chillers='//TRIM(ChillerName))
        ENDIF
        CompIndex=ChillNum
      ELSE
        ChillNum=CompIndex
        IF (ChillNum > NumConstCOPChillers .or. ChillNum < 1) THEN
          CALL ShowFatalError('SimConstCOPChiller:  Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(ChillNum))// &
                              ', Number of Units='//TRIM(TrimSigDigits(NumConstCOPChillers))//  &
                              ', Entered Unit name='//TRIM(ChillerName))
        ENDIF
        IF (ConstCOPChiller(ChillNum)%Base%CheckEquipName) THEN
          IF (ChillerName /= ConstCOPChiller(ChillNum)%Base%Name) THEN
            CALL ShowFatalError('SimConstCOPChiller: Invalid CompIndex passed='//  &
                                TRIM(TrimSigDigits(ChillNum))// &
                                ', Unit name='//TRIM(ChillerName)//', stored Unit Name for that index='//  &
                                TRIM(ConstCOPChiller(ChillNum)%Base%Name))
          ENDIF
          ConstCOPChiller(ChillNum)%Base%CheckEquipName=.false.
        ENDIF
      ENDIF

      IF (InitLoopEquip) THEN
        TempEvapOutDesign  = 0.0d0
        TempCondInDesign   = 0.0d0
        CALL InitConstCOPChiller(ChillNum,RunFlag,MyLoad)
        CALL SizeConstCOPChiller(ChillNum)
        IF (LoopNum == ConstCOPChiller(ChillNum)%Base%CWLoopNum) THEN
          MinCap = 0.0d0
          MaxCap = ConstCOPChiller(ChillNum)%Base%NomCap
          OptCap = ConstCOPChiller(ChillNum)%Base%NomCap
        ELSE
          MinCap = 0.d0
          MaxCap = 0.d0
          OptCap = 0.d0
        ENDIF
        IF (GetSizingFactor) THEN
          SizingFactor = ConstCOPChiller(ChillNum)%Base%SizFac
        END IF
        RETURN
      END IF

      IF (LoopNum == ConstCOPChiller(ChillNum)%Base%CWLoopNum) THEN
     ! Calculate Load
       ! IF MinPlr, MaxPlr, OptPlr are not defined, assume min = 0, max=opt=Nomcap
        CALL InitConstCOPChiller(ChillNum,RunFlag,MyLoad)
        CALL CalcConstCOPChillerModel(ChillNum,MyLoad,Runflag,EquipFlowCtrl)
        CALL UpdateConstCOPChillerRecords(MyLoad,RunFlag,ChillNum)
      ELSEIF (LoopNum == ConstCOPChiller(ChillNum)%Base%CDLoopNum) THEN
        CALL UpdateChillerComponentCondenserSide(ConstCOPChiller(ChillNum)%Base%CDLoopNum, &
                                         ConstCOPChiller(ChillNum)%Base%CDLoopSideNum,     &
                                         TypeOf_Chiller_ConstCOP,                     &
                                         ConstCOPChiller(ChillNum)%Base%CondInletNodeNum,  &
                                         ConstCOPChiller(ChillNum)%Base%CondOutletNodeNum, &
                                         ConstCOPChillerReport(ChillNum)%Base%QCond,        &
                                         ConstCOPChillerReport(ChillNum)%Base%CondInletTemp,     &
                                         ConstCOPChillerReport(ChillNum)%Base%CondOutletTemp,    &
                                         ConstCOPChillerReport(ChillNum)%Base%Condmdot,  &
                                         FirstHVACIteration)
      ENDIF

  END SELECT

RETURN
END SUBROUTINE SimChiller

SUBROUTINE GetElectricChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the Electric Chiller model.


            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE General,           ONLY: RoundSigDigits
  USE PlantUtilities,    ONLY: RegisterPlantCompDesignFlow
  USE ScheduleManager,    ONLY: GetScheduleIndex
  USE DataSizing,        ONLY: Autosize
  USE DataGlobals,       ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE !

            ! PARAMETERS
  CHARACTER(len=*), PARAMETER :: RoutineName='GetElectricChillerInput: ' ! include trailing blank space
            !LOCAL VARIABLES
  INTEGER                     :: ChillerNum !chiller counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
!  CHARACTER(len=MaxNameLength),DIMENSION(9)   :: AlphArray !character string data
!  REAL(r64),                        DIMENSION(22)  :: NumArray  !numeric data
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: errflag
  LOGICAL       :: Okay
!  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject  ! for ease in renaming.

         !FLOW
  cCurrentModuleObject = 'Chiller:Electric'
  NumElectricChillers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumElectricChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' Equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

            !See if load distribution manager has already gotten the input
    IF (ALLOCATED(ElectricChiller))RETURN

         !ALLOCATE ARRAYS
  ALLOCATE (ElectricChiller(NumElectricChillers))
  ALLOCATE (ElectricChillerReport(NumElectricChillers))

         !LOAD ARRAYS WITH Electric CURVE FIT CHILLER DATA
  DO ChillerNum = 1 , NumElectricChillers
    CALL GetObjectItem(cCurrentModuleObject,ChillerNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                    NumericFieldNames=cNumericFieldNames,AlphaFieldnames=cAlphaFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ElectricChiller%Base%Name,ChillerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    ElectricChiller(ChillerNum)%Base%Name                = cAlphaArgs(1)

    IF (cAlphaArgs(2) == 'AIRCOOLED' ) THEN
      ElectricChiller(ChillerNum)%Base%CondenserType       = AirCooled
    ELSEIF (cAlphaArgs(2) == 'WATERCOOLED' ) THEN
      ElectricChiller(ChillerNum)%Base%CondenserType       = WaterCooled
    ELSEIF (cAlphaArgs(2) == 'EVAPORATIVELYCOOLED' ) THEN
      ElectricChiller(ChillerNum)%Base%CondenserType       = EvapCooled
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF


    ElectricChiller(ChillerNum)%Base%NomCap              = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    ElectricChiller(ChillerNum)%Base%COP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    ElectricChiller(ChillerNum)%Base%EvapInletNodeNum    = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    ElectricChiller(ChillerNum)%Base%EvapOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Chilled Water Nodes')

    IF (ElectricChiller(ChillerNum)%Base%CondenserType == AirCooled .or.   &
       ElectricChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
      ! Connection not required for air or evap cooled condenser
       !If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
      !  since it is not used elsewhere for connection
      ! for transition purposes, add this node if not there.
      IF(lAlphaFieldBlanks(5))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 21) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(5) = TRIM(cAlphaArgs(1))//' CONDENSER INLET NODE'
        ELSE
          cAlphaArgs(5) = TRIM(cAlphaArgs(1)(1:79))//' CONDENSER INLET NODE'
        ENDIF
      End If
      IF(lAlphaFieldBlanks(6) )THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 22) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(6) = TRIM(cAlphaArgs(1))//' CONDENSER OUTLET NODE'
        ELSE
          cAlphaArgs(6) = TRIM(cAlphaArgs(1)(1:78))//' CONDENSER OUTLET NODE'
        ENDIF
      END IF

      ElectricChiller(ChillerNum)%Base%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      CALL CheckAndAddAirNodeNumber(ElectricChiller(ChillerNum)%Base%CondInletNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(5)))
      ENDIF

      ElectricChiller(ChillerNum)%Base%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
    ELSEIF (ElectricChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
      ElectricChiller(ChillerNum)%Base%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      ElectricChiller(ChillerNum)%Base%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser Water Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ElseIf ( lAlphaFieldBlanks(6) ) Then
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ELSE
      ElectricChiller(ChillerNum)%Base%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      ElectricChiller(ChillerNum)%Base%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser (unknown?) Nodes')
      !Condenser Inlet node name is necessary
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ElseIf ( lAlphaFieldBlanks(6) ) Then
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ENDIF


    ElectricChiller(ChillerNum)%MinPartLoadRat      = rNumericArgs(3)
    ElectricChiller(ChillerNum)%MaxPartLoadRat      = rNumericArgs(4)
    ElectricChiller(ChillerNum)%OptPartLoadRat      = rNumericArgs(5)
    ElectricChiller(ChillerNum)%TempDesCondIn       = rNumericArgs(6)
    ElectricChiller(ChillerNum)%TempRiseCoef        = rNumericArgs(7)
    ElectricChiller(ChillerNum)%TempDesEvapOut      = rNumericArgs(8)
    ElectricChiller(ChillerNum)%Base%EvapVolFlowRate     = rNumericArgs(9)
    ElectricChiller(ChillerNum)%Base%CondVolFlowRate     = rNumericArgs(10)
    ElectricChiller(ChillerNum)%CapRatCoef(1)       = rNumericArgs(11)
    ElectricChiller(ChillerNum)%CapRatCoef(2)       = rNumericArgs(12)
    ElectricChiller(ChillerNum)%CapRatCoef(3)       = rNumericArgs(13)
    IF ((rNumericArgs(11)+rNumericArgs(12)+rNumericArgs(13)) == 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Sum of Capacity Ratio Coef = 0.0, chiller='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    ElectricChiller(ChillerNum)%PowerRatCoef(1)     = rNumericArgs(14)
    ElectricChiller(ChillerNum)%PowerRatCoef(2)     = rNumericArgs(15)
    ElectricChiller(ChillerNum)%PowerRatCoef(3)     = rNumericArgs(16)
    ElectricChiller(ChillerNum)%FullLoadCoef(1)     = rNumericArgs(17)
    ElectricChiller(ChillerNum)%FullLoadCoef(2)     = rNumericArgs(18)
    ElectricChiller(ChillerNum)%FullLoadCoef(3)     = rNumericArgs(19)
    ElectricChiller(ChillerNum)%TempLowLimitEvapOut = rNumericArgs(20)
    ElectricChiller(ChillerNum)%Base%SizFac              = rNumericArgs(22)
    IF (ElectricChiller(ChillerNum)%Base%SizFac <= 0.0d0) ElectricChiller(ChillerNum)%Base%SizFac = 1.0d0

    SELECT CASE (TRIM(cAlphaArgs(7)))
    CASE ( 'CONSTANTFLOW' )
      ElectricChiller(ChillerNum)%Base%FlowMode = ConstantFlow
    CASE ( 'VARIABLEFLOW' )
      ElectricChiller(ChillerNum)%Base%FlowMode = LeavingSetpointModulated
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
      CALL ShowContinueError('Key choice is now called "LeavingSetpointModulated" and the simulation continues')
    CASE ('LEAVINGSETPOINTMODULATED')
      ElectricChiller(ChillerNum)%Base%FlowMode = LeavingSetpointModulated
    CASE ('NOTMODULATED')
      ElectricChiller(ChillerNum)%Base%FlowMode = NotModulated
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
      CALL ShowContinueError('Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated')
      CALL ShowContinueError('Flow mode NotModulated is assumed and the simulation continues.')
      ElectricChiller(ChillerNum)%Base%FlowMode = NotModulated
    END SELECT

   ! These are the Heat Recovery Inputs
    ElectricChiller(ChillerNum)%DesignHeatRecVolFlowRate = rNumericArgs(21)
    IF ((ElectricChiller(ChillerNum)%DesignHeatRecVolFlowRate > 0.0d0) &
        .OR. (ElectricChiller(ChillerNum)%DesignHeatRecVolFlowRate == Autosize ) ) THEN
      ElectricChiller(ChillerNum)%HeatRecActive=.true.
      ElectricChiller(ChillerNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
      IF (ElectricChiller(ChillerNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      ElectricChiller(ChillerNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
      IF (ElectricChiller(ChillerNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(8),cAlphaArgs(9),'Heat Recovery Nodes')
      IF ( ElectricChiller(ChillerNum)%DesignHeatRecVolFlowRate > 0.d0) THEN
        CALL RegisterPlantCompDesignFlow(ElectricChiller(ChillerNum)%HeatRecInletNodeNum, &
                                ElectricChiller(ChillerNum)%DesignHeatRecVolFlowRate )
      ENDIF
      ! Condenser flow rate must be specified for heat reclaim
      IF (ElectricChiller(ChillerNum)%Base%CondenserType == AirCooled .OR. &
          ElectricChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        IF(ElectricChiller(ChillerNum)%Base%CondVolFlowRate .LE. 0.0d0)THEN
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(10))//'='//TRIM(RoundSigDigits(rNumericArgs(10),6)))
          CALL ShowSevereError('Condenser fluid flow rate must be specified for Heat Reclaim applications.')
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        END IF
      END IF

      IF(NumNums > 24) THEN
        IF ( .NOT. lNumericFieldBlanks(25)) THEN
          ElectricChiller(ChillerNum)%HeatRecCapacityFraction = rNumericArgs(25)
        ELSE
          ElectricChiller(ChillerNum)%HeatRecCapacityFraction = 1.d0
        ENDIF
      ELSE
        ElectricChiller(ChillerNum)%HeatRecCapacityFraction = 1.d0
      ENDIF

      IF (NumAlphas > 10) THEN
        IF ( .NOT. lAlphaFieldBlanks(11)) THEN
          ElectricChiller(ChillerNum)%HeatRecInletLimitSchedNum = GetScheduleIndex(cAlphaArgs(11))
          IF (ElectricChiller(ChillerNum)%HeatRecInletLimitSchedNum  == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(11))//'='//TRIM(cAlphaArgs(11)))
            ErrorsFound=.True.
          ENDIF
        ELSE
          ElectricChiller(ChillerNum)%HeatRecInletLimitSchedNum = 0
        ENDIF
      ELSE
        ElectricChiller(ChillerNum)%HeatRecInletLimitSchedNum = 0
      ENDIF

      IF (NumAlphas > 11) THEN
        IF( .NOT. lAlphaFieldBlanks(12)) THEN
          ElectricChiller(ChillerNum)%HeatRecSetpointNodeNum = &
              GetOnlySingleNode(cAlphaArgs(12), ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                           NodeType_Water,NodeConnectionType_Sensor, 1, ObjectIsNotParent)
        ELSE
          ElectricChiller(ChillerNum)%HeatRecSetpointNodeNum = 0
        ENDIF
      ELSE
        ElectricChiller(ChillerNum)%HeatRecSetpointNodeNum = 0
      ENDIf

    ELSE
      ElectricChiller(ChillerNum)%HeatRecActive=.false.
      ElectricChiller(ChillerNum)%DesignHeatRecMassFlowRate = 0.0d0
      ElectricChiller(ChillerNum)%HeatRecInletNodeNum   = 0
      ElectricChiller(ChillerNum)%HeatRecOutletNodeNum   = 0
      ! if heat recovery is not used, don't care about condenser flow rate for air/evap-cooled equip.
      IF (ElectricChiller(ChillerNum)%Base%CondenserType == AirCooled .OR. &
          ElectricChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        ElectricChiller(ChillerNum)%Base%CondVolFlowRate = 0.0011d0  ! set to avoid errors in calc routine
      END IF
      IF ((.NOT. lAlphaFieldBlanks(8))  .OR. (.NOT. lAlphaFieldBlanks(9))) THEN
        CALL ShowWarningError('Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for '//  &
                             TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('However, Node names were specified for Heat Recovery inlet or outlet nodes')
      END IF

    END IF
    !   Basin heater power as a function of temperature must be greater than or equal to 0
    ElectricChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff = rNumericArgs(23)
    IF(rNumericArgs(23) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(ElectricChiller(ChillerNum)%Base%Name)//&
                     '" TRIM(cNumericFieldNames(23)) must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    ElectricChiller(ChillerNum)%Base%BasinHeaterSetPointTemp = rNumericArgs(24)

    IF(ElectricChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 24) THEN
        ElectricChiller(ChillerNum)%Base%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(ElectricChiller(ChillerNum)%Base%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(ElectricChiller(ChillerNum)%Base%Name)//&
           '", '//TRIM(cNumericFieldNames(24))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(.NOT. lAlphaFieldBlanks(10))THEN
      ElectricChiller(ChillerNum)%Base%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(10))
      IF(ElectricChiller(ChillerNum)%Base%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(ElectricChiller(ChillerNum)%Base%Name)//&
                       '" TRIM(cAlphaFieldNames(10)) "'//TRIM(cAlphaArgs(10)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '// TRIM(cCurrentModuleObject) )
  ENDIF

  DO ChillerNum = 1, NumElectricChillers
     CALL SetupOutputVariable('Chiller Electric Power [W]', &
          ElectricChillerReport(ChillerNum)%Base%Power,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Electric Energy [J]', &
          ElectricChillerReport(ChillerNum)%Base%Energy,'System','Sum',ElectricChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Evaporator Cooling Rate [W]', &
          ElectricChillerReport(ChillerNum)%Base%QEvap,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Cooling Energy [J]', &
          ElectricChillerReport(ChillerNum)%Base%EvapEnergy,'System','Sum',ElectricChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evaporator Inlet Temperature [C]', &
          ElectricChillerReport(ChillerNum)%Base%EvapInletTemp,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Outlet Temperature [C]', &
          ElectricChillerReport(ChillerNum)%Base%EvapOutletTemp,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Mass Flow Rate [kg/s]', &
          ElectricChillerReport(ChillerNum)%Base%Evapmdot,'System','Average',ElectricChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Rate [W]', &
          ElectricChillerReport(ChillerNum)%Base%QCond,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Energy [J]', &
          ElectricChillerReport(ChillerNum)%Base%CondEnergy,'System','Sum',ElectricChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller COP [W/W]', &
          ElectricChillerReport(ChillerNum)%ActualCOP,'System','Average',ElectricChiller(ChillerNum)%Base%Name)

        !Condenser mass flow and outlet temp are valid for water cooled
     IF (ElectricChiller(ChillerNum)%Base%CondenserType == WaterCooled)THEN
       CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
            ElectricChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
       CALL SetupOutputVariable('Chiller Condenser Outlet Temperature [C]', &
            ElectricChillerReport(ChillerNum)%Base%CondOutletTemp,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
       CALL SetupOutputVariable('Chiller Condenser Mass Flow Rate [kg/s]', &
            ElectricChillerReport(ChillerNum)%Base%Condmdot,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
     ELSEIF (ElectricChiller(ChillerNum)%Base%CondenserType == AirCooled) THEN
       CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
            ElectricChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
     ELSEIF (ElectricChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
       CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
            ElectricChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
       IF(ElectricChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
         CALL SetupOutputVariable('Chiller Basin Heater Electric Power [W]', &
          ElectricChillerReport(ChillerNum)%Base%BasinHeaterPower,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
         CALL SetupOutputVariable('Chiller Basin Heater Electric Energy [J]', &
          ElectricChillerReport(ChillerNum)%Base%BasinHeaterConsumption,'System','Sum',ElectricChiller(ChillerNum)%Base%Name, &
          ResourceTypeKey='Electric',EndUseKey='CHILLERS',GroupKey='Plant')
       END IF
     ENDIF

     !If heat recovery is active then setup report variables
     IF (ElectricChiller(ChillerNum)%HeatRecActive) THEN
         CALL SetupOutputVariable('Chiller Total Recovered Heat Rate [W]', &
           ElectricChillerReport(ChillerNum)%QHeatRecovery,'System','Average',ElectricChiller(ChillerNum)%Base%Name)
         CALL SetupOutputVariable('Chiller Total Recovered Heat Energy [J]', &
           ElectricChillerReport(ChillerNum)%EnergyHeatRecovery,'System','Sum',ElectricChiller(ChillerNum)%Base%Name,  &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')
         CALL SetupOutputVariable('Chiller Heat Recovery Inlet Temperature [C]', &
           ElectricChillerReport(ChillerNum)%HeatRecInletTemp,'System','Average',ElectricChiller(ChillerNum)%Base%Name)

         CALL SetupOutputVariable('Chiller Heat Recovery Outlet Temperature [C]', &
           ElectricChillerReport(ChillerNum)%HeatRecOutletTemp,'System','Average',ElectricChiller(ChillerNum)%Base%Name)

         CALL SetupOutputVariable('Chiller Heat Recovery Mass Flow Rate [kg/s]', &
           ElectricChillerReport(ChillerNum)%HeatRecMassFlow,'System','Average',ElectricChiller(ChillerNum)%Base%Name)

         CALL SetupOutputVariable('Chiller Effective Heat Rejection Temperature [C]', &
           ElectricChillerReport(ChillerNum)%ChillerCondAvgTemp, 'System','Average',ElectricChiller(ChillerNum)%Base%Name)

     ENDIF
     IF (AnyEnergyManagementSystemInModel) THEN
       CALL SetupEMSInternalVariable('Chiller Nominal Capacity', ElectricChiller(ChillerNum)%Base%Name, '[W]', &
                                     ElectricChiller(ChillerNum)%Base%NomCap  )
     ENDIF
  END DO



RETURN
END SUBROUTINE GetElectricChillerInput


SUBROUTINE GetEngineDrivenChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000
            !
            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the EngineDriven Chiller model.


            ! METHODOLOGY EMPLOYED:

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE CurveManager,   ONLY : GetCurveIndex
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE General,           ONLY: RoundSigDigits
  USE PlantUtilities,    ONLY: RegisterPlantCompDesignFlow
  USE ScheduleManager,   ONLY: GetScheduleIndex
  USE DataGlobals,       ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE !

            ! PARAMETERS
  CHARACTER(len=*), PARAMETER :: RoutineName='GetEngineDrivenChillerInput: ' ! include trailing blank space
            !LOCAL VARIABLES
  INTEGER                     :: ChillerNum !chiller counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: errflag
  LOGICAL       :: Okay

         !FLOW
  cCurrentModuleObject = 'Chiller:EngineDriven'
  NumEngineDrivenChillers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumEngineDrivenChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF
            !See if load distribution manager has already gotten the input
  IF (ALLOCATED(EngineDrivenChiller))RETURN

         !ALLOCATE ARRAYS
  ALLOCATE (EngineDrivenChiller(NumEngineDrivenChillers))
  ALLOCATE (EngineDrivenChillerReport(NumEngineDrivenChillers))

         !LOAD ARRAYS WITH EngineDriven CURVE FIT CHILLER DATA
  DO ChillerNum = 1 , NumEngineDrivenChillers
    CALL GetObjectItem(cCurrentModuleObject,ChillerNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),EngineDrivenChiller%Base%Name,ChillerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    EngineDrivenChiller(ChillerNum)%Base%Name                = cAlphaArgs(1)

    EngineDrivenChiller(ChillerNum)%Base%NomCap              = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    EngineDrivenChiller(ChillerNum)%Base%COP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    IF (cAlphaArgs(2) == 'AIRCOOLED' ) THEN
      EngineDrivenChiller(ChillerNum)%Base%CondenserType       = AirCooled
    ELSEIF (cAlphaArgs(2) == 'WATERCOOLED' ) THEN
      EngineDrivenChiller(ChillerNum)%Base%CondenserType       = WaterCooled
    ELSEIF (cAlphaArgs(2) == 'EVAPORATIVELYCOOLED' ) THEN
      EngineDrivenChiller(ChillerNum)%Base%CondenserType       = EvapCooled
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF


    EngineDrivenChiller(ChillerNum)%Base%EvapInletNodeNum    =   &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    EngineDrivenChiller(ChillerNum)%Base%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Chilled Water Nodes')

    IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == AirCooled .or.  &
        EngineDrivenChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
      ! Connection not required for air or evap cooled condenser
       !If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
      !  since it is not used elsewhere for connection
      IF(lAlphaFieldBlanks(5))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 21) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(5) = TRIM(cAlphaArgs(1))//' CONDENSER INLET NODE'
        ELSE
          cAlphaArgs(5) = TRIM(cAlphaArgs(1)(1:79))//' CONDENSER INLET NODE'
        ENDIF
      End If
      IF(lAlphaFieldBlanks(6) )THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 22) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(6) = TRIM(cAlphaArgs(1))//' CONDENSER OUTLET NODE'
        ELSE
          cAlphaArgs(6) = TRIM(cAlphaArgs(1)(1:78))//' CONDENSER OUTLET NODE'
        ENDIF
      END IF

      EngineDrivenChiller(ChillerNum)%Base%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      CALL CheckAndAddAirNodeNumber(EngineDrivenChiller(ChillerNum)%Base%CondInletNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(5)))
      ENDIF

      EngineDrivenChiller(ChillerNum)%Base%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      !CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser (Air) Nodes')
    ELSEIF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
      EngineDrivenChiller(ChillerNum)%Base%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      EngineDrivenChiller(ChillerNum)%Base%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser Water Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(6) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ELSE
      EngineDrivenChiller(ChillerNum)%Base%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      EngineDrivenChiller(ChillerNum)%Base%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
     CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser (unknown?) Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(6) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ENDIF


    EngineDrivenChiller(ChillerNum)%MinPartLoadRat      = rNumericArgs(3)
    EngineDrivenChiller(ChillerNum)%MaxPartLoadRat      = rNumericArgs(4)
    EngineDrivenChiller(ChillerNum)%OptPartLoadRat      = rNumericArgs(5)
    EngineDrivenChiller(ChillerNum)%TempDesCondIn       = rNumericArgs(6)
    EngineDrivenChiller(ChillerNum)%TempRiseCoef        = rNumericArgs(7)
    EngineDrivenChiller(ChillerNum)%TempDesEvapOut      = rNumericArgs(8)
    EngineDrivenChiller(ChillerNum)%Base%EvapVolFlowRate     = rNumericArgs(9)
    EngineDrivenChiller(ChillerNum)%Base%CondVolFlowRate     = rNumericArgs(10)
    EngineDrivenChiller(ChillerNum)%CapRatCoef(1)       = rNumericArgs(11)
    EngineDrivenChiller(ChillerNum)%CapRatCoef(2)       = rNumericArgs(12)
    EngineDrivenChiller(ChillerNum)%CapRatCoef(3)       = rNumericArgs(13)
    IF ((rNumericArgs(11)+rNumericArgs(12)+rNumericArgs(13)) == 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Sum of Capacity Ratio Coef = 0.0, chiller='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    EngineDrivenChiller(ChillerNum)%PowerRatCoef(1)     = rNumericArgs(14)
    EngineDrivenChiller(ChillerNum)%PowerRatCoef(2)     = rNumericArgs(15)
    EngineDrivenChiller(ChillerNum)%PowerRatCoef(3)     = rNumericArgs(16)
    EngineDrivenChiller(ChillerNum)%FullLoadCoef(1)     = rNumericArgs(17)
    EngineDrivenChiller(ChillerNum)%FullLoadCoef(2)     = rNumericArgs(18)
    EngineDrivenChiller(ChillerNum)%FullLoadCoef(3)     = rNumericArgs(19)
    EngineDrivenChiller(ChillerNum)%TempLowLimitEvapOut = rNumericArgs(20)


!Load Special EngineDriven Chiller Curve Fit Inputs
    EngineDrivenChiller(ChillerNum)%ClngLoadtoFuelCurve = GetCurveIndex(cAlphaArgs(7)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%ClngLoadtoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%RecJacHeattoFuelCurve = GetCurveIndex(cAlphaArgs(8)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%RecJacHeattoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%RecLubeHeattoFuelCurve = GetCurveIndex(cAlphaArgs(9)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%RecLubeHeattoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%TotExhausttoFuelCurve = GetCurveIndex(cAlphaArgs(10)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%TotExhausttoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%ExhaustTempCurve = GetCurveIndex(cAlphaArgs(11)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%ExhaustTempCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(11))//'='//TRIM(cAlphaArgs(11)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%UACoef(1) = rNumericArgs(21)
    EngineDrivenChiller(ChillerNum)%UACoef(2) = rNumericArgs(22)

    EngineDrivenChiller(ChillerNum)%MaxExhaustperPowerOutput = rNumericArgs(23)
    EngineDrivenChiller(ChillerNum)%DesignMinExitGasTemp = rNumericArgs(24)

    EngineDrivenChiller(ChillerNum)%FuelType = TRIM(cAlphaArgs(12))

    SELECT CASE (cAlphaArgs(12))

    CASE ('Gas','NATURALGAS','NATURAL GAS')
      EngineDrivenChiller(ChillerNum)%FuelType = 'Gas'

    CASE ('DIESEL')
      EngineDrivenChiller(ChillerNum)%FuelType = 'Diesel'

    CASE ('GASOLINE')
      EngineDrivenChiller(ChillerNum)%FuelType = 'Gasoline'

    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
       EngineDrivenChiller(ChillerNum)%FuelType = 'FuelOil#1'

    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
       EngineDrivenChiller(ChillerNum)%FuelType = 'FuelOil#2'

    CASE ('Propane','LPG','PROPANEGAS','PROPANE GAS')
       EngineDrivenChiller(ChillerNum)%FuelType = 'Propane'

    CASE ('OTHERFUEL1')
       EngineDrivenChiller(ChillerNum)%FuelType = 'OtherFuel1'

    CASE ('OTHERFUEL2')
       EngineDrivenChiller(ChillerNum)%FuelType = 'OtherFuel2'

    CASE DEFAULT
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(12))//'='//TRIM(cAlphaArgs(12)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Valid choices are Electricity, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2,'//  &
       'OtherFuel1 or OtherFuel2')
      ErrorsFound=.true.
    END SELECT


    EngineDrivenChiller(ChillerNum)%FuelHeatingValue = rNumericArgs(25)
    EngineDrivenChiller(ChillerNum)%DesignHeatRecVolFlowRate = rNumericArgs(26)
    IF (EngineDrivenChiller(ChillerNum)%DesignHeatRecVolFlowRate > 0.0d0) THEN
      EngineDrivenChiller(ChillerNum)%HeatRecActive=.true.
      EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(13),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
      IF (EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(13))//'='//TRIM(cAlphaArgs(13)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      EngineDrivenChiller(ChillerNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(14),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
      IF (EngineDrivenChiller(ChillerNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(14))//'='//TRIM(cAlphaArgs(14)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(13),cAlphaArgs(14),'Heat Recovery Nodes')
        CALL RegisterPlantCompDesignFlow(EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum, &
                                EngineDrivenChiller(ChillerNum)%DesignHeatRecVolFlowRate)
      ! Condenser flow rate must be specified for heat reclaim
      IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == AirCooled .OR. &
          EngineDrivenChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        IF(EngineDrivenChiller(ChillerNum)%Base%CondVolFlowRate .LE. 0.0d0)THEN
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(10))//'='//TRIM(RoundSigDigits(rNumericArgs(10),6)))
          CALL ShowSevereError('Condenser fluid flow rate must be specified for Heat Reclaim applications.')
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        END IF
      END IF

      ELSE



      EngineDrivenChiller(ChillerNum)%HeatRecActive=.false.
      EngineDrivenChiller(ChillerNum)%DesignHeatRecMassFlowRate = 0.0d0
      EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum   = 0
      EngineDrivenChiller(ChillerNum)%HeatRecOutletNodeNum   = 0
      ! if heat recovery is not used, don't care about condenser flow rate for air/evap-cooled equip.
      IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == AirCooled .OR. &
          EngineDrivenChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        EngineDrivenChiller(ChillerNum)%Base%CondVolFlowRate = 0.0011d0  ! set to avoid errors in calc routine
      END IF
      IF ((.NOT. lAlphaFieldBlanks(13))  .OR. (.NOT. lAlphaFieldBlanks(14))) THEN
        CALL ShowWarningError('Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for '// &
                    TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('However, Node names were specified for Heat Recovery inlet or outlet nodes')
      ENDIF
    ENDIF

    SELECT CASE (TRIM(cAlphaArgs(15)))
    CASE ( 'CONSTANTFLOW' )
      EngineDrivenChiller(ChillerNum)%Base%FlowMode = ConstantFlow
    CASE ( 'VARIABLEFLOW' )
      EngineDrivenChiller(ChillerNum)%Base%FlowMode = LeavingSetpointModulated
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(15))//'='//TRIM(cAlphaArgs(15)))
      CALL ShowContinueError('Key choice is now called "LeavingSetpointModulated" and the simulation continues')
    CASE ('LEAVINGSETPOINTMODULATED')
      EngineDrivenChiller(ChillerNum)%Base%FlowMode = LeavingSetpointModulated
    CASE ('NOTMODULATED')
      EngineDrivenChiller(ChillerNum)%Base%FlowMode = NotModulated
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(15))//'='//TRIM(cAlphaArgs(15)))
      CALL ShowContinueError('Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated')
      CALL ShowContinueError('Flow mode NotModulated is assumed and the simulation continues.')
      EngineDrivenChiller(ChillerNum)%Base%FlowMode = NotModulated
    END SELECT

    EngineDrivenChiller(ChillerNum)%HeatRecMaxTemp = rNumericArgs(27)
    EngineDrivenChiller(ChillerNum)%Base%SizFac = rNumericArgs(28)
    IF (EngineDrivenChiller(ChillerNum)%Base%SizFac <= 0.0d0) EngineDrivenChiller(ChillerNum)%Base%SizFac = 1.0d0

    !   Basin heater power as a function of temperature must be greater than or equal to 0
    EngineDrivenChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff = rNumericArgs(29)
    IF(rNumericArgs(29) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(EngineDrivenChiller(ChillerNum)%Base%Name)//&
                     '" TRIM(cNumericFieldNames(29)) must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSetPointTemp = rNumericArgs(30)

    IF(EngineDrivenChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 30) THEN
        EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(EngineDrivenChiller(ChillerNum)%Base%Name)//&
           '", '//TRIM(cNumericFieldNames(30))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(.NOT. lAlphaFieldBlanks(16))THEN
      EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(16))
      IF(EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(EngineDrivenChiller(ChillerNum)%Base%Name)//&
                       '" TRIM(cAlphaFieldNames(16)) "'//TRIM(cAlphaArgs(16)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF


  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '// TRIM(cCurrentModuleObject) )
  ENDIF

  DO ChillerNum = 1, NumEngineDrivenChillers
     CALL SetupOutputVariable('Chiller Drive Shaft Power [W]', &
          EngineDrivenChillerReport(ChillerNum)%Base%Power,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Drive Shaft Energy [J]', &
          EngineDrivenChillerReport(ChillerNum)%Base%Energy,'System','Sum',EngineDrivenChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Evaporator Cooling Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%Base%QEvap,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Cooling Energy [J]', &
          EngineDrivenChillerReport(ChillerNum)%Base%EvapEnergy,'System','Sum',EngineDrivenChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evaporator Inlet Temperature [C]', &
          EngineDrivenChillerReport(ChillerNum)%Base%EvapInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Outlet Temperature [C]', &
          EngineDrivenChillerReport(ChillerNum)%Base%EvapOutletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Mass Flow Rate [kg/s]', &
          EngineDrivenChillerReport(ChillerNum)%Base%Evapmdot,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%Base%QCond,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Energy [J]', &
          EngineDrivenChillerReport(ChillerNum)%Base%CondEnergy,'System','Sum',EngineDrivenChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')

       !Condenser mass flow and outlet temp are valid for Water Cooled
     IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == WaterCooled)THEN
       CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
            EngineDrivenChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
       CALL SetupOutputVariable('Chiller Condenser Outlet Temperature [C]', &
            EngineDrivenChillerReport(ChillerNum)%Base%CondOutletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
       CALL SetupOutputVariable('Chiller Condenser Mass Flow Rate [kg/s]', &
            EngineDrivenChillerReport(ChillerNum)%Base%Condmdot,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
     ELSEIF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == AirCooled) THEN
       CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
            EngineDrivenChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
     ELSEIF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
       CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
            EngineDrivenChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
       IF(EngineDrivenChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
         CALL SetupOutputVariable('Chiller Basin Heater Electric Power [W]', &
          EngineDrivenChillerReport(ChillerNum)%Base%BasinHeaterPower,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
         CALL SetupOutputVariable('Chiller Basin Heater Electric Energy [J]', &
          EngineDrivenChillerReport(ChillerNum)%Base%BasinHeaterConsumption,'System','Sum',  &
             EngineDrivenChiller(ChillerNum)%Base%Name, &
          ResourceTypeKey='Electric',EndUseKey='CHILLERS',GroupKey='Plant')
       END IF
     End IF



     CALL SetupOutputVariable('Chiller ' // TRIM(EngineDrivenChiller(ChillerNum)%FuelType) //' Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%FuelEnergyUseRate,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller ' // TRIM(EngineDrivenChiller(ChillerNum)%FuelType) //' Energy [J]', &
          EngineDrivenChillerReport(ChillerNum)%FuelEnergy,'System','Sum',EngineDrivenChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey=EngineDrivenChiller(ChillerNum)%FuelType,EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller COP [W/W]', &
          EngineDrivenChillerReport(ChillerNum)%FuelCOP,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller ' // TRIM(EngineDrivenChiller(ChillerNum)%FuelType) //' Mass Flow Rate [kg/s]', &
          EngineDrivenChillerReport(ChillerNum)%FuelMdot,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Exhaust Temperature [C]', &
          EngineDrivenChillerReport(ChillerNum)%ExhaustStackTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Heat Recovery Mass Flow Rate [kg/s]', &
          EngineDrivenChillerReport(ChillerNum)%HeatRecMdot,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)

     IF (EngineDrivenChiller(ChillerNum)%HeatRecActive) THEN
  ! need to only report if heat recovery active
       CALL SetupOutputVariable('Chiller Jacket Recovered Heat Rate [W]', &
            EngineDrivenChillerReport(ChillerNum)%QJacketRecovered,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
       CALL SetupOutputVariable('Chiller Jacket Recovered Heat Energy [J]', &
            EngineDrivenChillerReport(ChillerNum)%JacketEnergyRec,'System','Sum',EngineDrivenChiller(ChillerNum)%Base%Name,  &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

       CALL SetupOutputVariable('Chiller Lube Recovered Heat Rate [W]', &
            EngineDrivenChillerReport(ChillerNum)%QLubeOilRecovered,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
       CALL SetupOutputVariable('Chiller Lube Recovered Heat Energy [J]', &
            EngineDrivenChillerReport(ChillerNum)%LubeOilEnergyRec,'System','Sum',EngineDrivenChiller(ChillerNum)%Base%Name,  &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

       CALL SetupOutputVariable('Chiller Exhaust Recovered Heat Rate [W]', &
            EngineDrivenChillerReport(ChillerNum)%QExhaustRecovered,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
       CALL SetupOutputVariable('Chiller Exhaust Recovered Heat Energy [J]', &
            EngineDrivenChillerReport(ChillerNum)%ExhaustEnergyRec,'System','Sum',EngineDrivenChiller(ChillerNum)%Base%Name,  &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

       CALL SetupOutputVariable('Chiller Total Recovered Heat Rate [W]', &
            EngineDrivenChillerReport(ChillerNum)%QTotalHEatRecovered,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)
       CALL SetupOutputVariable('Chiller Total Recovered Heat Energy [J]', &
            EngineDrivenChillerReport(ChillerNum)%TotalHeatEnergyRec,'System','Sum',EngineDrivenChiller(ChillerNum)%Base%Name)

       CALL SetupOutputVariable('Chiller Heat Recovery Inlet Temperature [C]', &
          EngineDrivenChillerReport(ChillerNum)%HeatRecInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)

       CALL SetupOutputVariable('Chiller Heat Recovery Outlet Temperature [C]', &
          EngineDrivenChillerReport(ChillerNum)%HeatRecOutletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Base%Name)


     ENDIF
     IF (AnyEnergyManagementSystemInModel) THEN
       CALL SetupEMSInternalVariable('Chiller Nominal Capacity', EngineDrivenChiller(ChillerNum)%Base%Name, '[W]', &
                                     EngineDrivenChiller(ChillerNum)%Base%NomCap  )
     ENDIF
  END DO

RETURN
END SUBROUTINE GetEngineDrivenChillerInput

SUBROUTINE GetGTChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the GT Chiller model.


            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE General,           ONLY: RoundSigDigits
  USE PlantUtilities,    ONLY: RegisterPlantCompDesignFlow
  USE ScheduleManager,   ONLY: GetScheduleIndex
  USE DataGlobals,       ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE !

            ! PARAMETERS
  CHARACTER(len=*), PARAMETER :: RoutineName='GetGTChillerInput: ' ! include trailing blank space
            !LOCAL VARIABLES
  INTEGER                     :: ChillerNum !chiller counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: errflag
  LOGICAL       :: Okay

         !FLOW
  cCurrentModuleObject = 'Chiller:CombustionTurbine'
  NumGTChillers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumGTChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF
            !See if load distribution manager has already gotten the input
  IF (ALLOCATED(GTChiller))RETURN

         !ALLOCATE ARRAYS
  ALLOCATE (GTChiller(NumGTChillers))
  ALLOCATE (GTChillerReport(NumGTChillers))

  DO ChillerNum = 1 , NumGTChillers
    CALL GetObjectItem(cCurrentModuleObject,ChillerNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),GTChiller%Base%Name,ChillerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    GTChiller(ChillerNum)%Base%Name                = cAlphaArgs(1)

    GTChiller(ChillerNum)%Base%NomCap              = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    GTChiller(ChillerNum)%Base%COP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    IF (cAlphaArgs(2) == 'AIRCOOLED' ) THEN
      GTChiller(ChillerNum)%Base%CondenserType       = AirCooled
    ELSEIF (cAlphaArgs(2) == 'WATERCOOLED') THEN
      GTChiller(ChillerNum)%Base%CondenserType       = WaterCooled
    ELSEIF (cAlphaArgs(2) == 'EVAPORATIVELYCOOLED') THEN
      GTChiller(ChillerNum)%Base%CondenserType       = EvapCooled
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF


    GTChiller(ChillerNum)%Base%EvapInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    GTChiller(ChillerNum)%Base%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Chilled Water Nodes')

    IF (GTChiller(ChillerNum)%Base%CondenserType == AirCooled .or. GTChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
      ! Connection not required for air or evap cooled condenser
      ! If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
      ! since it is not used elsewhere for connection
      IF(lAlphaFieldBlanks(5))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 21) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(5) = TRIM(cAlphaArgs(1))//' CONDENSER INLET NODE'
        ELSE
          cAlphaArgs(5) = TRIM(cAlphaArgs(1)(1:79))//' CONDENSER INLET NODE'
        ENDIF
      End If
      IF(lAlphaFieldBlanks(6) )THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 22) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(6) = TRIM(cAlphaArgs(1))//' CONDENSER OUTLET NODE'
        ELSE
          cAlphaArgs(6) = TRIM(cAlphaArgs(1)(1:78))//' CONDENSER OUTLET NODE'
        ENDIF
      END IF

      GTChiller(ChillerNum)%Base%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      CALL CheckAndAddAirNodeNumber(GTChiller(ChillerNum)%Base%CondInletNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(5)))
      ENDIF

      GTChiller(ChillerNum)%Base%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
    ELSE ! WaterCooled CondenserType
      GTChiller(ChillerNum)%Base%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      GTChiller(ChillerNum)%Base%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser (unknown?) Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(6) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ENDIF

    GTChiller(ChillerNum)%MinPartLoadRat      = rNumericArgs(3)
    GTChiller(ChillerNum)%MaxPartLoadRat      = rNumericArgs(4)
    GTChiller(ChillerNum)%OptPartLoadRat      = rNumericArgs(5)
    GTChiller(ChillerNum)%TempDesCondIn       = rNumericArgs(6)
    GTChiller(ChillerNum)%TempRiseCoef        = rNumericArgs(7)
    GTChiller(ChillerNum)%TempDesEvapOut      = rNumericArgs(8)
    GTChiller(ChillerNum)%Base%EvapVolFlowRate     = rNumericArgs(9)
    GTChiller(ChillerNum)%Base%CondVolFlowRate     = rNumericArgs(10)
    GTChiller(ChillerNum)%CapRatCoef(1)       = rNumericArgs(11)
    GTChiller(ChillerNum)%CapRatCoef(2)       = rNumericArgs(12)
    GTChiller(ChillerNum)%CapRatCoef(3)       = rNumericArgs(13)
    IF ((rNumericArgs(11)+rNumericArgs(12)+rNumericArgs(13)) == 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Sum of Capacity Ratio Coef = 0.0, chiller='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    GTChiller(ChillerNum)%PowerRatCoef(1)     = rNumericArgs(14)
    GTChiller(ChillerNum)%PowerRatCoef(2)     = rNumericArgs(15)
    GTChiller(ChillerNum)%PowerRatCoef(3)     = rNumericArgs(16)
    GTChiller(ChillerNum)%FullLoadCoef(1)     = rNumericArgs(17)
    GTChiller(ChillerNum)%FullLoadCoef(2)     = rNumericArgs(18)
    GTChiller(ChillerNum)%FullLoadCoef(3)     = rNumericArgs(19)
    GTChiller(ChillerNum)%TempLowLimitEvapOut = rNumericArgs(20)


    !Load Special GT Chiller Input

    GTChiller(ChillerNum)%PLBasedFuelInputCoef(1) = rNumericArgs(21)
    GTChiller(ChillerNum)%PLBasedFuelInputCoef(2) = rNumericArgs(22)
    GTChiller(ChillerNum)%PLBasedFuelInputCoef(3) = rNumericArgs(23)

    GTChiller(ChillerNum)%TempBasedFuelInputCoef(1) = rNumericArgs(24)
    GTChiller(ChillerNum)%TempBasedFuelInputCoef(2) = rNumericArgs(25)
    GTChiller(ChillerNum)%TempBasedFuelInputCoef(3) = rNumericArgs(26)

    GTChiller(ChillerNum)%ExhaustFlowCoef(1) = rNumericArgs(27)
    GTChiller(ChillerNum)%ExhaustFlowCoef(2) = rNumericArgs(28)
    GTChiller(ChillerNum)%ExhaustFlowCoef(3) = rNumericArgs(29)

    GTChiller(ChillerNum)%PLBasedExhaustTempCoef(1) = rNumericArgs(30)
    GTChiller(ChillerNum)%PLBasedExhaustTempCoef(2) = rNumericArgs(31)
    GTChiller(ChillerNum)%PLBasedExhaustTempCoef(3) = rNumericArgs(32)

    GTChiller(ChillerNum)%TempBasedExhaustTempCoef(1) = rNumericArgs(33)
    GTChiller(ChillerNum)%TempBasedExhaustTempCoef(2) = rNumericArgs(34)
    GTChiller(ChillerNum)%TempBasedExhaustTempCoef(3) = rNumericArgs(35)

    GTChiller(ChillerNum)%HeatRecLubeEnergyCoef(1) = rNumericArgs(36)
    GTChiller(ChillerNum)%HeatRecLubeEnergyCoef(2) = rNumericArgs(37)
    GTChiller(ChillerNum)%HeatRecLubeEnergyCoef(3) = rNumericArgs(38)

    GTChiller(ChillerNum)%UAtoCapCoef(1) = rNumericArgs(39)
    GTChiller(ChillerNum)%UAtoCapCoef(2) = rNumericArgs(40)

    GTChiller(ChillerNum)%GTEngineCapacity = rNumericArgs(41)
    GTChiller(ChillerNum)%MaxExhaustperGTPower = rNumericArgs(42)
    GTChiller(ChillerNum)%DesignSteamSatTemp = rNumericArgs(43)
    GTChiller(ChillerNum)%FuelHeatingValue = rNumericArgs(44)

    !Get the Heat Recovery information
    GTChiller(ChillerNum)%DesignHeatRecVolFlowRate = rNumericArgs(45)
    IF (GTChiller(ChillerNum)%DesignHeatRecVolFlowRate > 0.0d0) THEN
      GTChiller(ChillerNum)%HeatRecActive=.true.
      GTChiller(ChillerNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
      IF (GTChiller(ChillerNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      GTChiller(ChillerNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
      IF (GTChiller(ChillerNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(7),cAlphaArgs(8),'Heat Recovery Nodes')

        CALL RegisterPlantCompDesignFlow(GTChiller(ChillerNum)%HeatRecInletNodeNum, &
                                GTChiller(ChillerNum)%DesignHeatRecVolFlowRate )
       ! Condenser flow rate must be specified for heat reclaim
      IF (GTChiller(ChillerNum)%Base%CondenserType == AirCooled .OR. &
          GTChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        IF(GTChiller(ChillerNum)%Base%CondVolFlowRate .LE. 0.0d0)THEN
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(10))//'='//TRIM(RoundSigDigits(rNumericArgs(10),6)))
          CALL ShowSevereError('Condenser fluid flow rate must be specified for Heat Reclaim applications.')
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.True.
      ENDIF

      ENDIf

    ELSE
      GTChiller(ChillerNum)%HeatRecActive=.false.
      GTChiller(ChillerNum)%DesignHeatRecMassFlowRate = 0.0d0
      GTChiller(ChillerNum)%HeatRecInletNodeNum    = 0
      GTChiller(ChillerNum)%HeatRecOutletNodeNum   = 0
      IF ((.NOT. lAlphaFieldBlanks(7))  .OR. (.NOT. lAlphaFieldBlanks(8))) THEN
        CALL ShowWarningError('Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for '// &
                    TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('However, Node names were specified for heat recovery inlet or outlet nodes')
      ENDIF
      IF (GTChiller(ChillerNum)%Base%CondenserType == AirCooled .OR. &
          GTChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        GTChiller(ChillerNum)%Base%CondVolFlowRate = 0.0011d0  ! set to avoid errors in calc routine
      END IF
    ENDIF

    SELECT CASE (TRIM(cAlphaArgs(9)))
    CASE ( 'CONSTANTFLOW' )
      GTChiller(ChillerNum)%Base%FlowMode = ConstantFlow
    CASE ( 'VARIABLEFLOW' )
      GTChiller(ChillerNum)%Base%FlowMode = LeavingSetpointModulated
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
      CALL ShowContinueError('Key choice is now called "LeavingSetpointModulated" and the simulation continues')
    CASE ('LEAVINGSETPOINTMODULATED')
      GTChiller(ChillerNum)%Base%FlowMode = LeavingSetpointModulated
    CASE ('NOTMODULATED')
      GTChiller(ChillerNum)%Base%FlowMode = NotModulated
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
      CALL ShowContinueError('Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated')
      CALL ShowContinueError('Flow mode NotModulated is assumed and the simulation continues.')
      GTChiller(ChillerNum)%Base%FlowMode = NotModulated
    END SELECT

    !Fuel Type Case Statement
    SELECT CASE (cAlphaArgs(10))
    CASE ('GAS','NATURALGAS','NATURAL GAS')
      GTChiller(ChillerNum)%FuelType = 'Gas'

    CASE ('DIESEL')
      GTChiller(ChillerNum)%FuelType = 'Diesel'

    CASE ('GASOLINE')
      GTChiller(ChillerNum)%FuelType = 'Gasoline'

    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
       GTChiller(ChillerNum)%FuelType = 'FuelOil#1'

    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
       GTChiller(ChillerNum)%FuelType = 'FuelOil#2'

    CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
       GTChiller(ChillerNum)%FuelType = 'Propane'

    CASE ('OTHERFUEL1')
       GTChiller(ChillerNum)%FuelType = 'OtherFuel1'

    CASE ('OTHERFUEL2')
       GTChiller(ChillerNum)%FuelType = 'OtherFuel2'

    CASE DEFAULT
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Valid choices are Electricity, NaturalGas, PropaneGas, Diesel, Gasoline, FuelOil#1, FuelOil#2,'//  &
       'OtherFuel1 or OtherFuel2')
      ErrorsFound=.true.
    END SELECT


    GTChiller(ChillerNum)%HeatRecMaxTemp = rNumericArgs(46)
    GTChiller(ChillerNum)%Base%SizFac = rNumericArgs(47)
    IF (GTChiller(ChillerNum)%Base%SizFac <= 0.0d0) GTChiller(ChillerNum)%Base%SizFac = 1.0d0

    !   Basin heater power as a function of temperature must be greater than or equal to 0
    GTChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff = rNumericArgs(48)
    IF(rNumericArgs(48) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(GTChiller(ChillerNum)%Base%Name)//&
                     '"'//TRIM(cNumericFieldNames(48))//' must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    GTChiller(ChillerNum)%Base%BasinHeaterSetPointTemp = rNumericArgs(49)

    IF(GTChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 49) THEN
        GTChiller(ChillerNum)%Base%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(GTChiller(ChillerNum)%Base%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(GTChiller(ChillerNum)%Base%Name)//&
           '", '//TRIM(cNumericFieldNames(49))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(.NOT. lAlphaFieldBlanks(11))THEN
      GTChiller(ChillerNum)%Base%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(11))
      IF(GTChiller(ChillerNum)%Base%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(GTChiller(ChillerNum)%Base%Name)//&
                       '" TRIM(cAlphaFieldNames(11)) "'//TRIM(cAlphaArgs(11)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '// TRIM(cCurrentModuleObject) )
  ENDIF

  DO ChillerNum = 1, NumGTChillers
     CALL SetupOutputVariable('Chiller Drive Shaft Power [W]', &
          GTChillerReport(ChillerNum)%Base%Power,'System','Average',GTChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Drive Shaft Energy [J]', &
          GTChillerReport(ChillerNum)%Base%Energy,'System','Sum',GTChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Evaporator Cooling Rate [W]', &
          GTChillerReport(ChillerNum)%Base%QEvap,'System','Average',GTChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Cooling Energy [J]', &
          GTChillerReport(ChillerNum)%Base%EvapEnergy,'System','Sum',GTChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evaporator Inlet Temperature [C]', &
          GTChillerReport(ChillerNum)%Base%EvapInletTemp,'System','Average',GTChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Outlet Temperature [C]', &
          GTChillerReport(ChillerNum)%Base%EvapOutletTemp,'System','Average',GTChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Mass Flow Rate [kg/s]', &
          GTChillerReport(ChillerNum)%Base%Evapmdot,'System','Average',GTChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Rate [W]', &
          GTChillerReport(ChillerNum)%Base%QCond,'System','Average',GTChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Energy [J]', &
          GTChillerReport(ChillerNum)%Base%CondEnergy,'System','Sum',GTChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')

        !Condenser mass flow and outlet temp are valid for water cooled
     IF (GTChiller(ChillerNum)%Base%CondenserType == WaterCooled)THEN
        CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
             GTChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',GTChiller(ChillerNum)%Base%Name)
        CALL SetupOutputVariable('Chiller Condenser Outlet Temperature [C]', &
             GTChillerReport(ChillerNum)%Base%CondOutletTemp,'System','Average',GTChiller(ChillerNum)%Base%Name)
        CALL SetupOutputVariable('Chiller Condenser Mass Flow Rate [kg/s]', &
             GTChillerReport(ChillerNum)%Base%Condmdot,'System','Average',GTChiller(ChillerNum)%Base%Name)
     ELSEIF (GTChiller(ChillerNum)%Base%CondenserType == AirCooled) THEN
        CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
             GTChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',GTChiller(ChillerNum)%Base%Name)
     ELSEIF (GTChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
             GTChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',GTChiller(ChillerNum)%Base%Name)
        IF(GTChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
          CALL SetupOutputVariable('Chiller Basin Heater Electric Power [W]', &
               GTChillerReport(ChillerNum)%Base%BasinHeaterPower,'System','Average',GTChiller(ChillerNum)%Base%Name)
          CALL SetupOutputVariable('Chiller Basin Heater Electric Energy [J]', &
               GTChillerReport(ChillerNum)%Base%BasinHeaterConsumption,'System','Sum',GTChiller(ChillerNum)%Base%Name, &
               ResourceTypeKey='Electric',EndUseKey='CHILLERS',GroupKey='Plant')
        END IF
     ENDIF

     CALL SetupOutputVariable('Chiller Lube Recovered Heat Rate [W]', &
          GTChillerReport(ChillerNum)%HeatRecLubeRate,'System','Average',GTChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Lube Recovered Heat Energy [J]', &
          GTChillerReport(ChillerNum)%HeatRecLubeEnergy,'System','Sum',GTChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HeatRecovery',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller '// TRIM(GTChiller(ChillerNum)%FuelType)//' Rate [W]', &
          GTChillerReport(ChillerNum)%FuelEnergyUsedRate,'System','Average',GTChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller '// TRIM(GTChiller(ChillerNum)%FuelType)//' Energy [J]', &
          GTChillerReport(ChillerNum)%FuelEnergyUsed,'System','Sum',GTChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey=GTChiller(ChillerNum)%FuelType,EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller '// TRIM(GTChiller(ChillerNum)%FuelType)//' Mass Flow Rate [kg/s]', &
          GTChillerReport(ChillerNum)%FuelMassUsedRate,'System','Average',GTChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller '// TRIM(GTChiller(ChillerNum)%FuelType)//' Mass [kg]', &
          GTChillerReport(ChillerNum)%FuelMassUsed,'System','Sum',GTChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Exhaust Temperature [C]', &
          GTChillerReport(ChillerNum)%ExhaustStackTemp,'System','Average',GTChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Heat Recovery Inlet Temperature [C]', &
          GTChillerReport(ChillerNum)%HeatRecInletTemp,'System','Average',GTChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Heat Recovery Outlet Temperature [C]', &
          GTChillerReport(ChillerNum)%HeatRecOutletTemp,'System','Average',GTChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Heat Recovery Mass Flow Rate [kg/s]', &
          GTChillerReport(ChillerNum)%HeatRecMdot,'System','Average',GTChiller(ChillerNum)%Base%Name)


     CALL SetupOutputVariable('Chiller COP [W/W]', &
          GTChillerReport(ChillerNum)%FuelCOP,'System','Average',GTChiller(ChillerNum)%Base%Name)

     IF (AnyEnergyManagementSystemInModel) THEN
       CALL SetupEMSInternalVariable('Chiller Nominal Capacity', GTChiller(ChillerNum)%Base%Name, '[W]', &
                                     GTChiller(ChillerNum)%Base%NomCap  )
     ENDIF

  END DO

RETURN
END SUBROUTINE GetGTChillerInput

SUBROUTINE GetConstCOPChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    April 1998

            ! PURPOSE OF THIS SUBROUTINE:!This routine will get the input
                         !required by the PrimaryPlantLoopManager.  As such
                         !it will interact with the Input Scanner to retrieve
                         !information from the input file, count the number of
                         !heating and cooling loops and begin to fill the
                         !arrays associated with the type PlantLoopProps.


            ! METHODOLOGY EMPLOYED: to be determined...
            ! REFERENCES:

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutputReportPredefined
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE General, ONLY: RoundSigDigits
  USE ScheduleManager,    ONLY: GetScheduleIndex
  USE DataGlobals,        ONLY: AnyEnergyManagementSystemInModel

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetConstCOPChillerInput: ' ! include trailing blank space

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                        :: ChillerNum
  INTEGER                     :: NumAlphas ! Number of elements in the alpha array
  INTEGER                     :: NumNums   ! Number of elements in the numeric array
  INTEGER                     :: IOStat    ! IO Status when calling get input subroutine
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: errflag
  LOGICAL       :: Okay

            !GET NUMBER OF ALL EQUIPMENT TYPES
  cCurrentModuleObject = 'Chiller:ConstantCOP'
  NumConstCOPChillers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumConstCOPChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

            !See if load distribution manager has already gotten the input
  IF (ALLOCATED(ConstCOPChiller))RETURN

  ALLOCATE (ConstCOPChiller(NumConstCOPChillers))
  ALLOCATE (ConstCOPChillerReport(NumConstCOPChillers))

             !LOAD ARRAYS WITH BLAST ConstCOP CHILLER DATA
  DO ChillerNum = 1 , NumConstCOPChillers
    CALL GetObjectItem(cCurrentModuleObject,ChillerNum,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ConstCOPChiller%Base%Name,ChillerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    ConstCOPChiller(ChillerNum)%Base%Name                = cAlphaArgs(1)
    ConstCOPChiller(ChillerNum)%Base%NomCap              = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    ConstCOPChiller(ChillerNum)%Base%COP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    !Set the Condenser Type from input
    IF (cAlphaArgs(6) == 'AIRCOOLED' ) THEN
      ConstCOPChiller(ChillerNum)%Base%CondenserType       = AirCooled
    ELSEIF (cAlphaArgs(6) == 'EVAPORATIVELYCOOLED') THEN
      ConstCOPChiller(ChillerNum)%Base%CondenserType       = EvapCooled
    ELSEIF (cAlphaArgs(6) == 'WATERCOOLED' ) THEN
      ConstCOPChiller(ChillerNum)%Base%CondenserType       = WaterCooled
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    ConstCOPChiller(ChillerNum)%Base%EvapVolFlowRate     = rNumericArgs(3)
    IF (ConstCOPChiller(ChillerNum)%Base%CondenserType == AirCooled .OR. &
        ConstCOPChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN ! Condenser flow rate not used for these cond types
      ConstCOPChiller(ChillerNum)%Base%CondVolFlowRate   = 0.0011d0
    ELSE
      ConstCOPChiller(ChillerNum)%Base%CondVolFlowRate   = rNumericArgs(4)
    ENDIF
    ConstCOPChiller(ChillerNum)%Base%SizFac              = rNumericArgs(5)

    ConstCOPChiller(ChillerNum)%Base%EvapInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    ConstCOPChiller(ChillerNum)%Base%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Chilled Water Nodes')

    IF (ConstCOPChiller(ChillerNum)%Base%CondenserType == AirCooled .or.   &
       ConstCOPChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
      ! Connection not required for air or evap cooled condenser
       !If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
      !  since it is not used elsewhere for connection
      IF(lAlphaFieldBlanks(4))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 21) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(4) = TRIM(cAlphaArgs(1))//' CONDENSER INLET NODE'
        ELSE
          cAlphaArgs(4) = TRIM(cAlphaArgs(1)(1:79))//' CONDENSER INLET NODE'
        ENDIF
      End If
      IF(lAlphaFieldBlanks(5) )THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 22) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(5) = TRIM(cAlphaArgs(1))//' CONDENSER OUTLET NODE'
        ELSE
          cAlphaArgs(5) = TRIM(cAlphaArgs(1)(1:78))//' CONDENSER OUTLET NODE'
        ENDIF
      END IF

      ConstCOPChiller(ChillerNum)%Base%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      CALL CheckAndAddAirNodeNumber(ConstCOPChiller(ChillerNum)%Base%CondInletNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(4)))
      ENDIF

      ConstCOPChiller(ChillerNum)%Base%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
    ELSEIF (ConstCOPChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
      ConstCOPChiller(ChillerNum)%Base%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      ConstCOPChiller(ChillerNum)%Base%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Condenser Water Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(4) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ELSE
      ConstCOPChiller(ChillerNum)%Base%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      ConstCOPChiller(ChillerNum)%Base%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Condenser (unknown?) Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(4) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ENDIF

    SELECT CASE (TRIM(cAlphaArgs(7)))
    CASE ( 'CONSTANTFLOW' )
      ConstCOPChiller(ChillerNum)%Base%FlowMode = ConstantFlow
    CASE ( 'VARIABLEFLOW' )
      ConstCOPChiller(ChillerNum)%Base%FlowMode = LeavingSetpointModulated
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
      CALL ShowContinueError('Key choice is now called "LeavingSetpointModulated" and the simulation continues')
    CASE ('LEAVINGSETPOINTMODULATED')
      ConstCOPChiller(ChillerNum)%Base%FlowMode = LeavingSetpointModulated
    CASE ('NOTMODULATED')
      ConstCOPChiller(ChillerNum)%Base%FlowMode = NotModulated
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
      CALL ShowContinueError('Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated')
      CALL ShowContinueError('Flow mode NotModulated is assumed and the simulation continues.')
      ConstCOPChiller(ChillerNum)%Base%FlowMode = NotModulated
    END SELECT

    !   Basin heater power as a function of temperature must be greater than or equal to 0
    ConstCOPChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff = rNumericArgs(6)
    IF(rNumericArgs(6) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(ConstCOPChiller(ChillerNum)%Base%Name)//&
                     '" TRIM(cNumericFieldNames(6)) must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    ConstCOPChiller(ChillerNum)%Base%BasinHeaterSetPointTemp = rNumericArgs(7)

    IF(ConstCOPChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 7) THEN
        ConstCOPChiller(ChillerNum)%Base%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(ConstCOPChiller(ChillerNum)%Base%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(ConstCOPChiller(ChillerNum)%Base%Name)//&
           '", '//TRIM(cNumericFieldNames(7))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(.NOT. lAlphaFieldBlanks(8))THEN
      ConstCOPChiller(ChillerNum)%Base%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(8))
      IF(ConstCOPChiller(ChillerNum)%Base%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(ConstCOPChiller(ChillerNum)%Base%Name)//&
                       '" TRIM(cAlphaFieldNames(8)) "'//TRIM(cAlphaArgs(8)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF


  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  ENDIF

  DO ChillerNum = 1, NumConstCOPChillers
     CALL SetupOutputVariable('Chiller Electric Power [W]', &
          ConstCOPChillerReport(ChillerNum)%Base%Power,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Electric Energy [J]', &
          ConstCOPChillerReport(ChillerNum)%Base%Energy,'System','Sum',ConstCOPChiller(ChillerNum)%Base%Name, &
          ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Evaporator Cooling Rate [W]', &
          ConstCOPChillerReport(ChillerNum)%Base%QEvap,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Cooling Energy [J]', &
          ConstCOPChillerReport(ChillerNum)%Base%EvapEnergy,'System','Sum',ConstCOPChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evaporator Inlet Temperature [C]', &
          ConstCOPChillerReport(ChillerNum)%Base%EvapInletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Outlet Temperature [C]', &
          ConstCOPChillerReport(ChillerNum)%Base%EvapOutletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Evaporator Mass Flow Rate [kg/s]', &
          ConstCOPChillerReport(ChillerNum)%Base%Evapmdot,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller COP [W/W]', &
          ConstCOPChillerReport(ChillerNum)%ActualCOP,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)

     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Rate [W]', &
          ConstCOPChillerReport(ChillerNum)%Base%QCond,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Energy [J]', &
          ConstCOPChillerReport(ChillerNum)%Base%CondEnergy,'System','Sum',ConstCOPChiller(ChillerNum)%Base%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')

        !Condenser mass flow and outlet temp are valid for water cooled
     IF (ConstCOPChiller(ChillerNum)%Base%CondenserType == WaterCooled)THEN
        CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
             ConstCOPChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
        CALL SetupOutputVariable('Chiller Condenser Outlet Temperature [C]', &
             ConstCOPChillerReport(ChillerNum)%Base%CondOutletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
        CALL SetupOutputVariable('Chiller Condenser Mass Flow Rate [kg/s]', &
             ConstCOPChillerReport(ChillerNum)%Base%Condmdot,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
     ELSEIF (ConstCOPChiller(ChillerNum)%Base%CondenserType == AirCooled) THEN
        CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
             ConstCOPChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
     ELSEIF (ConstCOPChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
             ConstCOPChillerReport(ChillerNum)%Base%CondInletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
        IF(ConstCOPChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
          CALL SetupOutputVariable('Chiller Basin Heater Electric Power [W]', &
               ConstCOPChillerReport(ChillerNum)%Base%BasinHeaterPower,'System','Average',ConstCOPChiller(ChillerNum)%Base%Name)
          CALL SetupOutputVariable('Chiller Basin Heater Electric Energy [J]', &
               ConstCOPChillerReport(ChillerNum)%Base%BasinHeaterConsumption,'System','Sum',ConstCOPChiller(ChillerNum)%Base%Name, &
               ResourceTypeKey='Electric',EndUseKey='CHILLERS',GroupKey='Plant')
        END IF
     ENDIF
     IF (AnyEnergyManagementSystemInModel) THEN
       CALL SetupEMSInternalVariable('Chiller Nominal Capacity', ConstCOPChiller(ChillerNum)%Base%Name, '[W]', &
                                     ConstCOPChiller(ChillerNum)%Base%NomCap  )
     ENDIF
  END DO

RETURN
END SUBROUTINE GetConstCOPChillerInput

SUBROUTINE InitElectricChiller(ChillNum,RunFlag, MyLoad )


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Electric Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_Electric, ScanPlantLoopsForObject, PlantSizesOkayToFinalize, &
                              PlantSizeNotComplete, LoopFlowStatus_NeedyIfLoopOn, SingleSetpoint, DualSetpointDeadband
  USE DataEnvironment, ONLY : StdBaroPress
  USE Psychrometrics,  ONLY : PsyRhoAirFnPbTdbW
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current electric chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='InitElectricChiller'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag
  INTEGER :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER :: CondOutletNode     ! node number of water outlet node from the condenser
  INTEGER :: EvapInletNode
  INTEGER :: EvapOutletNode
  INTEGER :: HeatRecInNode
  INTEGER :: HeatRecOutNode
  LOGICAL :: errFlag
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: mdot ! local mass flow rate
  REAL(r64) :: mdotCond ! local mass flow rate for condenser
  REAL(r64) :: THeatRecSetpoint ! tests set point node for proper set point value

  INTEGER :: InletNode
  INTEGER :: OutletNode
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  INTEGER :: BranchIndex
  INTEGER :: CompIndex
  LOGICAL :: FatalError
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumElectricChillers))
    ALLOCATE(MyEnvrnFlag(NumElectricChillers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  CondInletNode  = ElectricChiller(ChillNum)%Base%CondInletNodeNum
  CondOutletNode = ElectricChiller(ChillNum)%Base%CondOutletNodeNum
  EvapInletNode  = ElectricChiller(ChillNum)%Base%EvapInletNodeNum
  EvapOutletNode = ElectricChiller(ChillNum)%Base%EvapOutletNodeNum

  IF (ElectricChiller(ChillNum)%HeatRecActive ) THEN
    HeatRecInNode = ElectricChiller(ChillNum)%HeatRecInletNodeNum
    HeatRecOutNode = ElectricChiller(ChillNum)%HeatRecOutletNodeNum
  ENDIF

  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(ElectricChiller(ChillNum)%Base%Name, &
                                 TypeOf_Chiller_Electric, &
                                 ElectricChiller(ChillNum)%Base%CWLoopNum, &
                                 ElectricChiller(ChillNum)%Base%CWLoopSideNum, &
                                 ElectricChiller(ChillNum)%Base%CWBranchNum, &
                                 ElectricChiller(ChillNum)%Base%CWCompNum, &
                                 LowLimitTemp = ElectricChiller(ChillNum)%TempLowLimitEvapOut, &
                                 InletNodeNumber = ElectricChiller(ChillNum)%Base%EvapInletNodeNum,  &
                                 errFlag=errFlag)
    IF (ElectricChiller(ChillNum)%Base%CondenserType /= AirCooled .AND. &
        ElectricChiller(ChillNum)%Base%CondenserType /= EvapCooled) THEN
      CALL ScanPlantLoopsForObject(ElectricChiller(ChillNum)%Base%Name, &
                                   TypeOf_Chiller_Electric, &
                                   ElectricChiller(ChillNum)%Base%CDLoopNum, &
                                   ElectricChiller(ChillNum)%Base%CDLoopSideNum, &
                                   ElectricChiller(ChillNum)%Base%CDBranchNum, &
                                   ElectricChiller(ChillNum)%Base%CDCompNum, &
                                   InletNodeNumber = ElectricChiller(ChillNum)%Base%CondInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ElectricChiller(ChillNum)%Base%CWLoopNum,      &
                                          ElectricChiller(ChillNum)%Base%CWLoopSideNum,  &
                                          ElectricChiller(ChillNum)%Base%CDLoopNum,      &
                                          ElectricChiller(ChillNum)%Base%CDLoopSideNum,  &
                                          TypeOf_Chiller_Electric, .TRUE. )
    ENDIF
    IF (ElectricChiller(ChillNum)%HeatRecActive) THEN
      CALL ScanPlantLoopsForObject(ElectricChiller(ChillNum)%Base%Name, &
                                   TypeOf_Chiller_Electric, &
                                   ElectricChiller(ChillNum)%HRLoopNum, &
                                   ElectricChiller(ChillNum)%HRLoopSideNum, &
                                   ElectricChiller(ChillNum)%HRBranchNum, &
                                   ElectricChiller(ChillNum)%HRCompNum, &
                                   InletNodeNumber = ElectricChiller(ChillNum)%HeatRecInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ElectricChiller(ChillNum)%Base%CWLoopNum,      &
                                          ElectricChiller(ChillNum)%Base%CWLoopSideNum,  &
                                          ElectricChiller(ChillNum)%HRLoopNum,      &
                                          ElectricChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_Electric, .TRUE.  )
    ENDIF

    IF (ElectricChiller(ChillNum)%Base%CondenserType /= AirCooled  .AND. &
        ElectricChiller(ChillNum)%Base%CondenserType /= EvapCooled .AND. &
        ElectricChiller(ChillNum)%HeatRecActive) THEN
      CALL InterConnectTwoPlantLoopSides( ElectricChiller(ChillNum)%Base%CDLoopNum,      &
                                          ElectricChiller(ChillNum)%Base%CDLoopSideNum,  &
                                          ElectricChiller(ChillNum)%HRLoopNum,      &
                                          ElectricChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_Electric, .FALSE. )
    ENDIF

    IF (errFlag) THEN
      CALL ShowFatalError('InitElectricChiller: Program terminated due to previous condition(s).')
    ENDIF

    IF (ElectricChiller(ChillNum)%Base%FlowMode == ConstantFlow ) Then
      ! reset flow priority
      PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%LoopSide(ElectricChiller(ChillNum)%Base%CWLoopSideNum)% &
          Branch(ElectricChiller(ChillNum)%Base%CWBranchNum)%Comp(ElectricChiller(ChillNum)%Base%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    ENDIF

    IF (ElectricChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) Then
      ! reset flow priority
      PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%LoopSide(ElectricChiller(ChillNum)%Base%CWLoopSideNum)% &
          Branch(ElectricChiller(ChillNum)%Base%CWBranchNum)%Comp(ElectricChiller(ChillNum)%Base%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn

      ! check if setpoint on outlet node
      IF ((Node(ElectricChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint   == SensedNodeFlagValue) .AND. &
          (Node(ElectricChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi == SensedNodeFlagValue)) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. ElectricChiller(ChillNum)%Base%ModulatedFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(ElectricChiller(ChillNum)%Base%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            ElectricChiller(ChillNum)%Base%ModulatedFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(ElectricChiller(ChillNum)%Base%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. ElectricChiller(ChillNum)%Base%ModulatedFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(ElectricChiller(ChillNum)%Base%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              ElectricChiller(ChillNum)%Base%ModulatedFlowErrDone = .TRUE.
            ENDIF
          ENDIF


        ENDIF
        ElectricChiller(ChillNum)%Base%ModulatedFlowSetToLoop = .TRUE.
        SELECT CASE (PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
        CASE (SingleSetpoint)
          Node(ElectricChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint =                        &
            Node(PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
        CASE (DualSetpointDeadband)
          Node(ElectricChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi =                        &
            Node(PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
        END SELECT
      ENDIF
    ENDIF
    MyFlag(ChillNum)=.FALSE.
  ENDIF

  IF( MyEnvrnFlag(ChillNum) .AND. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete) CALL SizeElectricChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%FluidIndex,&
                                RoutineName)

    ElectricChiller(ChillNum)%Base%EvapMassFlowRateMax = rho * ElectricChiller(ChillNum)%Base%EvapVolFlowRate
    CALL InitComponentNodes(0.0D0,ElectricChiller(ChillNum)%Base%EvapMassFlowRateMax,  &
                         EvapInletNode,        &
                         EvapOutletNode,       &
                         ElectricChiller(ChillNum)%Base%CWLoopNum,               &
                         ElectricChiller(ChillNum)%Base%CWLoopSideNum,           &
                         ElectricChiller(ChillNum)%Base%CWBranchNum,             &
                         ElectricChiller(ChillNum)%Base%CWCompNum)

          !init maximum available condenser flow rate
    IF (ElectricChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN

      Node(CondInletNode)%Temp = ElectricChiller(ChillNum)%TempDesCondIn  !DSU? old behavior, still want?

      rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidIndex,&
                                  RoutineName)

      ElectricChiller(ChillNum)%Base%CondMassFlowRateMax = rho * ElectricChiller(ChillNum)%Base%CondVolFlowRate

      CALL InitComponentNodes(0.0D0,  ElectricChiller(ChillNum)%Base%CondMassFlowRateMax,  &
                         CondInletNode,        &
                         CondOutletNode,       &
                         ElectricChiller(ChillNum)%Base%CDLoopNum,               &
                         ElectricChiller(ChillNum)%Base%CDLoopSideNum,           &
                         ElectricChiller(ChillNum)%Base%CDBranchNum,             &
                         ElectricChiller(ChillNum)%Base%CDCompNum)
    ELSE ! air or evap-air

      rho = PsyRhoAirFnPbTdbW(StdBaroPress,ElectricChiller(ChillNum)%TempDesCondIn,0.0D0,RoutineName)
      ElectricChiller(ChillNum)%Base%CondMassFlowRateMax = rho * ElectricChiller(ChillNum)%Base%CondVolFlowRate

      Node(CondInletNode)%MassFlowRate        = ElectricChiller(ChillNum)%Base%CondMassFlowRateMax
      Node(CondOutletNode)%MassFlowrate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0d0
      Node(CondInletNode)%MassFlowRateMin       = 0.0d0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0d0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0d0
    END IF

    IF (ElectricChiller(ChillNum)%HeatRecActive) THEN
      rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%FluidIndex,&
                                  RoutineName)
      ElectricChiller(ChillNum)%DesignHeatRecMassFlowRate = rho * &
                                         ElectricChiller(ChillNum)%DesignHeatRecVolFlowRate

      CALL InitComponentNodes(0.0D0, ElectricChiller(ChillNum)%DesignHeatRecMassFlowRate ,  &
                         ElectricChiller(ChillNum)%HeatRecInletNodeNum,        &
                         ElectricChiller(ChillNum)%HeatRecOutletNodeNum,       &
                         ElectricChiller(ChillNum)%HRLoopNum,               &
                         ElectricChiller(ChillNum)%HRLoopSideNum,           &
                         ElectricChiller(ChillNum)%HRBranchNum,             &
                         ElectricChiller(ChillNum)%HRCompNum)
      ElectricChiller(ChillNum)%HeatRecMaxCapacityLimit = ElectricChiller(ChillNum)%HeatRecCapacityFraction &
                           *(ElectricChiller(ChillNum)%Base%NomCap + ElectricChiller(ChillNum)%Base%NomCap &
                                                                 /ElectricChiller(ChillNum)%Base%COP  )

      IF(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum > 0)THEN
        SELECT CASE (PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetPoint)
            THeatRecSetpoint = Node(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum)%TempSetPoint
          CASE (DualSetPointDeadBand)
            THeatRecSetpoint = Node(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum)%TempSetPointHi
        END SELECT
        IF(THeatRecSetpoint == SensedNodeFlagValue)THEN
          IF (.NOT. AnyEnergyManagementSystemInModel) THEN
            IF (.NOT. ElectricChiller(ChillNum)%Base%HRSPErrDone) THEN
              CALL ShowWarningError('Missing heat recovery temperature setpoint for chiller named ' // &
                                          TRIM(ElectricChiller(ChillNum)%Base%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the heat recovery leaving temperature ' // &
                                             'setpoint node specified, use a SetpointManager')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for heat recovery. The simulation continues ...')
              ElectricChiller(ChillNum)%HeatRecSetpointNodeNum = PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%TempSetPointNodeNum
                ElectricChiller(ChillNum)%Base%HRSPErrDone = .TRUE.
            ENDIF
          ELSE
           ! need call to EMS to check node
            FatalError = .FALSE. ! but not really fatal yet, but should be.
            CALL CheckIfNodeSetpointManagedByEMS(ElectricChiller(ChillNum)%Base%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
            IF (FatalError) THEN
              IF (.NOT. ElectricChiller(ChillNum)%Base%HRSPErrDone) THEN
                CALL ShowWarningError('Missing heat recovery temperature setpoint for chiller named ' // &
                                          TRIM(ElectricChiller(ChillNum)%Base%Name) )
                CALL ShowContinueError('  A temperature setpoint is needed at the heat recovery leaving temperature ' // &
                                     'setpoint node specified, use a SetpointManager to establish a setpoint')
                CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at this node ')
                CALL ShowContinueError('  The overall loop setpoint will be assumed '//  &
                   'for heat recovery. The simulation continues ...')
                ElectricChiller(ChillNum)%HeatRecSetpointNodeNum =   &
                   PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%TempSetPointNodeNum
                ElectricChiller(ChillNum)%Base%HRSPErrDone = .TRUE.
              ENDIF
            ENDIF
          END IF ! IF (.NOT. AnyEnergyManagementSystemInModel) THEN
        END IF ! IF(THeatRecSetpoint == SensedNodeFlagValue)THEN
      END IF ! IF(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum > 0)THEN
    ENDIF ! IF (ElectricChiller(ChillNum)%HeatRecActive) THEN

    MyEnvrnFlag(ChillNum) = .FALSE.
  END IF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ChillNum)=.true.
  ENDIF

  IF ( (ElectricChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) &
       .AND. (ElectricChiller(ChillNum)%Base%ModulatedFlowSetToLoop)) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    SELECT CASE (PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
    CASE (SingleSetpoint)
      Node(ElectricChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint =                        &
        Node(PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    CASE (DualSetpointDeadband)
      Node(ElectricChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi =                        &
        Node(PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
    END SELECT

  ENDIF

!

  IF ((MyLoad < 0.d0) .AND. RunFlag)  THEN
    ! request full then take what can get
    mdot     = ElectricChiller(ChillNum)%Base%EvapMassFlowRateMax
    mdotCond = ElectricChiller(ChillNum)%Base%CondMassFlowRateMax
  ELSE
    mdot     = 0.d0
    mdotCond = 0.d0
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              ElectricChiller(ChillNum)%Base%CWLoopNum,     &
                              ElectricChiller(ChillNum)%Base%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%Base%CWBranchNum,   &
                              ElectricChiller(ChillNum)%Base%CWCompNum)
  IF (ElectricChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,         &
                                ElectricChiller(ChillNum)%Base%CDLoopNum,     &
                                ElectricChiller(ChillNum)%Base%CDLoopSideNum, &
                                ElectricChiller(ChillNum)%Base%CDBranchNum,   &
                                ElectricChiller(ChillNum)%Base%CDCompNum)
  ENDIF


  ! Initialize heat recovery flow rates at node
  IF (ElectricChiller(ChillNum)%HeatRecActive ) THEN

    InletNode    =  ElectricChiller(ChillNum)%HeatRecInletNodeNum
    OutletNode   =  ElectricChiller(ChillNum)%HeatRecOutletNodeNum
    LoopNum      =  ElectricChiller(ChillNum)%HRLoopNum
    LoopSideNum  =  ElectricChiller(ChillNum)%HRLoopSideNum
    BranchIndex  =  ElectricChiller(ChillNum)%HRBranchNum
    CompIndex    =  ElectricChiller(ChillNum)%HRCompNum

    If ( RunFlag ) Then
      mdot = ElectricChiller(ChillNum)%DesignHeatRecMassFlowRate
    ELSE
      mdot = 0.d0
    ENDIF

    CALL SetComponentFlowRate(mdot,InletNode,OutletNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

  END IF

  IF (ElectricChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
    BasinHeaterPower       = 0.0d0
  ENDIF

  RETURN

END SUBROUTINE InitElectricChiller


SUBROUTINE InitEngineDrivenChiller(ChillNum,RunFlag,MyLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Engine Driven Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_EngineDriven, ScanPlantLoopsForObject, &
                              PlantSizesOkayToFinalize, PlantSizeNotComplete, LoopFlowStatus_NeedyIfLoopOn
  USE DataEnvironment, ONLY : StdBaroPress
  USE Psychrometrics,  ONLY : PsyRhoAirFnPbTdbW
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current engine driven chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='InitEngineDrivenChiller'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyFlag
  INTEGER :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER :: CondOutletNode
  INTEGER :: EvapInletNode
  INTEGER :: EvapOutletNode
  INTEGER :: HeatRecInNode
  INTEGER :: HeatRecOutNode
  REAL(r64) :: rho       ! local fluid density
  REAL(r64) :: mdot      ! local mass flow rate
  REAL(r64) :: mdotCond  ! local mass flow rate for condenser
  LOGICAL :: errFlag
  INTEGER :: InletNode
  INTEGER :: OutletNode
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  INTEGER :: BranchIndex
  INTEGER :: CompIndex
  LOGICAL :: FatalError

          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumEngineDrivenChillers))
    ALLOCATE(MyEnvrnFlag(NumEngineDrivenChillers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  !Load inputs to local structure
  CondInletNode  = EngineDrivenChiller(ChillNum)%Base%CondInletNodeNum
  CondOutletNode = EngineDrivenChiller(ChillNum)%Base%CondOutletNodeNum
  EvapInletNode  = EngineDrivenChiller(ChillNum)%Base%EvapInletNodeNum
  EvapOutletNode = EngineDrivenChiller(ChillNum)%Base%EvapOutletNodeNum

  IF (EngineDrivenChiller(ChillNum)%HeatRecActive) THEN
    HeatRecInNode = EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum
    HeatRecOutNode = EngineDrivenChiller(ChillNum)%HeatRecOutletNodeNum
  ENDIF

  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(EngineDrivenChiller(ChillNum)%Base%Name, &
                                 TypeOf_Chiller_EngineDriven, &
                                 EngineDrivenChiller(ChillNum)%Base%CWLoopNum, &
                                 EngineDrivenChiller(ChillNum)%Base%CWLoopSideNum, &
                                 EngineDrivenChiller(ChillNum)%Base%CWBranchNum, &
                                 EngineDrivenChiller(ChillNum)%Base%CWCompNum, &
                                 LowLimitTemp = EngineDrivenChiller(ChillNum)%TempLowLimitEvapOut, &
                                 InletNodeNumber = EngineDrivenChiller(ChillNum)%Base%EvapInletNodeNum,  &
                                 errFlag=errFlag)
    IF (EngineDrivenChiller(ChillNum)%Base%CondenserType /= AirCooled .AND. &
        EngineDrivenChiller(ChillNum)%Base%CondenserType /= EvapCooled) THEN
      CALL ScanPlantLoopsForObject(EngineDrivenChiller(ChillNum)%Base%Name, &
                                   TypeOf_Chiller_EngineDriven, &
                                   EngineDrivenChiller(ChillNum)%Base%CDLoopNum, &
                                   EngineDrivenChiller(ChillNum)%Base%CDLoopSideNum, &
                                   EngineDrivenChiller(ChillNum)%Base%CDBranchNum, &
                                   EngineDrivenChiller(ChillNum)%Base%CDCompNum, &
                                   InletNodeNumber = EngineDrivenChiller(ChillNum)%Base%CondInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( EngineDrivenChiller(ChillNum)%Base%CWLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%Base%CWLoopSideNum,  &
                                          EngineDrivenChiller(ChillNum)%Base%CDLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%Base%CDLoopSideNum,  &
                                          TypeOf_Chiller_EngineDriven, .TRUE. )
    ENDIF
    IF (EngineDrivenChiller(ChillNum)%HeatRecActive ) THEN
      CALL ScanPlantLoopsForObject(EngineDrivenChiller(ChillNum)%Base%Name, &
                                   TypeOf_Chiller_EngineDriven, &
                                   EngineDrivenChiller(ChillNum)%HRLoopNum, &
                                   EngineDrivenChiller(ChillNum)%HRLoopSideNum, &
                                   EngineDrivenChiller(ChillNum)%HRBranchNum, &
                                   EngineDrivenChiller(ChillNum)%HRCompNum, &
                                   InletNodeNumber = EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( EngineDrivenChiller(ChillNum)%Base%CWLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%Base%CWLoopSideNum,  &
                                          EngineDrivenChiller(ChillNum)%HRLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_EngineDriven , .TRUE. )
    ENDIF
    MyFlag(ChillNum)=.FALSE.
    IF (EngineDrivenChiller(ChillNum)%Base%CondenserType /= AirCooled .AND. &
        EngineDrivenChiller(ChillNum)%Base%CondenserType /= EvapCooled .AND. &
        EngineDrivenChiller(ChillNum)%HeatRecActive)  THEN
      CALL InterConnectTwoPlantLoopSides( EngineDrivenChiller(ChillNum)%Base%CDLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%Base%CDLoopSideNum,  &
                                          EngineDrivenChiller(ChillNum)%HRLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_EngineDriven, .FALSE. )
    ENDIF
    IF (errFlag) THEN
      CALL ShowFatalError('InitEngineDrivenChiller: Program terminated due to previous condition(s).')
    ENDIF

    IF (EngineDrivenChiller(ChillNum)%Base%FlowMode == ConstantFlow) THEN
      ! reset flow priority
      PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%LoopSide(EngineDrivenChiller(ChillNum)%Base%CWLoopSideNum)% &
          Branch(EngineDrivenChiller(ChillNum)%Base%CWBranchNum)%Comp(EngineDrivenChiller(ChillNum)%Base%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    ENDIF

    IF (EngineDrivenChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) THEN
      ! reset flow priority
      PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%LoopSide(EngineDrivenChiller(ChillNum)%Base%CWLoopSideNum)% &
          Branch(EngineDrivenChiller(ChillNum)%Base%CWBranchNum)%Comp(EngineDrivenChiller(ChillNum)%Base%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
      ! check if setpoint on outlet node
      IF ((Node(EngineDrivenChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
          (Node(EngineDrivenChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi == SensedNodeFlagValue)) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. EngineDrivenChiller(ChillNum)%Base%ModulatedFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(EngineDrivenChiller(ChillNum)%Base%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            EngineDrivenChiller(ChillNum)%Base%ModulatedFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(EngineDrivenChiller(ChillNum)%Base%EvapOutletNodeNum,  &
             iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. EngineDrivenChiller(ChillNum)%Base%ModulatedFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(EngineDrivenChiller(ChillNum)%Base%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              EngineDrivenChiller(ChillNum)%Base%ModulatedFlowErrDone = .TRUE.
            ENDIF
          ENDIF


        ENDIF
        EngineDrivenChiller(ChillNum)%Base%ModulatedFlowSetToLoop = .TRUE.
        Node(EngineDrivenChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint =                        &
          Node(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
        Node(EngineDrivenChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi =                        &
          Node(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
      ENDIF
    ENDIF

    MyFlag(ChillNum)=.FALSE.
  ENDIF

          !Initialize critical Demand Side Variables
!  IF((MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag) &
!     .OR. (Node(CondInletNode)%MassFlowrate <= 0.0 .AND. RunFlag)) THEN
  IF(MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete)  CALL SizeEngineDrivenChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%FluidIndex,&
                                RoutineName)

    EngineDrivenChiller(ChillNum)%Base%EvapMassFlowRateMax = rho * EngineDrivenChiller(ChillNum)%Base%EvapVolFlowRate
    CALL InitComponentNodes(0.0D0,EngineDrivenChiller(ChillNum)%Base%EvapMassFlowRateMax,  &
                         EvapInletNode,        &
                         EvapOutletNode,       &
                         EngineDrivenChiller(ChillNum)%Base%CWLoopNum,               &
                         EngineDrivenChiller(ChillNum)%Base%CWLoopSideNum,           &
                         EngineDrivenChiller(ChillNum)%Base%CWBranchNum,             &
                         EngineDrivenChiller(ChillNum)%Base%CWCompNum)

          !init maximum available condenser flow rate

    IF (EngineDrivenChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN

      Node(CondInletNode)%Temp = EngineDrivenChiller(ChillNum)%TempDesCondIn

      rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(EngineDrivenChiller(ChillNum)%Base%CDLoopNum)%FluidIndex,&
                                  RoutineName)

      EngineDrivenChiller(ChillNum)%Base%CondMassFlowRateMax = rho * EngineDrivenChiller(ChillNum)%Base%CondVolFlowRate

      CALL InitComponentNodes(0.0D0,  EngineDrivenChiller(ChillNum)%Base%CondMassFlowRateMax,  &
                         CondInletNode,        &
                         CondOutletNode,       &
                         EngineDrivenChiller(ChillNum)%Base%CDLoopNum,               &
                         EngineDrivenChiller(ChillNum)%Base%CDLoopSideNum,           &
                         EngineDrivenChiller(ChillNum)%Base%CDBranchNum,             &
                         EngineDrivenChiller(ChillNum)%Base%CDCompNum)
    ELSE ! air or evap-air
      Node(CondInletNode)%MassFlowRate        = EngineDrivenChiller(ChillNum)%Base%CondVolFlowRate * &
        PsyRhoAirFnPbTdbW(StdBaroPress,EngineDrivenChiller(ChillNum)%TempDesCondIn,0.0D0,RoutineName)

      Node(CondOutletNode)%MassFlowrate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0d0
      Node(CondInletNode)%MassFlowRateMin       = 0.0d0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0d0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0d0
    END IF

    IF (EngineDrivenChiller(ChillNum)%HeatRecActive) THEN
      rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%HRLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(EngineDrivenChiller(ChillNum)%HRLoopNum)%FluidIndex,&
                                  RoutineName)
      EngineDrivenChiller(ChillNum)%DesignHeatRecMassFlowRate = rho * &
                                         EngineDrivenChiller(ChillNum)%DesignHeatRecVolFlowRate

      CALL InitComponentNodes(0.0D0, EngineDrivenChiller(ChillNum)%DesignHeatRecMassFlowRate ,  &
                         EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum,        &
                         EngineDrivenChiller(ChillNum)%HeatRecOutletNodeNum,       &
                         EngineDrivenChiller(ChillNum)%HRLoopNum,               &
                         EngineDrivenChiller(ChillNum)%HRLoopSideNum,           &
                         EngineDrivenChiller(ChillNum)%HRBranchNum,             &
                         EngineDrivenChiller(ChillNum)%HRCompNum)
    ENDIF

    MyEnvrnFlag(ChillNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ChillNum)=.true.
  ENDIF

  IF ((EngineDrivenChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) &
       .AND. (EngineDrivenChiller(ChillNum)%Base%ModulatedFlowSetToLoop)) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(EngineDrivenChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(EngineDrivenChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi =                        &
         Node(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
  ENDIF

  IF ((ABS(MyLoad) > 0.d0) .AND. RunFlag)  THEN
    mdot     = EngineDrivenChiller(ChillNum)%Base%EvapMassFlowRateMax
    mdotCond = EngineDrivenChiller(ChillNum)%Base%CondMassFlowRateMax
  ELSE
    mdot     = 0.d0
    mdotCond = 0.d0
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              EngineDrivenChiller(ChillNum)%Base%CWLoopNum,     &
                              EngineDrivenChiller(ChillNum)%Base%CWLoopSideNum, &
                              EngineDrivenChiller(ChillNum)%Base%CWBranchNum,   &
                              EngineDrivenChiller(ChillNum)%Base%CWCompNum)
  IF (EngineDrivenChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,        &
                              EngineDrivenChiller(ChillNum)%Base%CDLoopNum,     &
                              EngineDrivenChiller(ChillNum)%Base%CDLoopSideNum, &
                              EngineDrivenChiller(ChillNum)%Base%CDBranchNum,   &
                              EngineDrivenChiller(ChillNum)%Base%CDCompNum)
  ENDIF

  ! Initialize heat recovery flow rates at node
  IF (EngineDrivenChiller(ChillNum)%HeatRecActive ) THEN
    InletNode    =  EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum
    OutletNode   =  EngineDrivenChiller(ChillNum)%HeatRecOutletNodeNum
    LoopNum      =  EngineDrivenChiller(ChillNum)%HRLoopNum
    LoopSideNum  =  EngineDrivenChiller(ChillNum)%HRLoopSideNum
    BranchIndex  =  EngineDrivenChiller(ChillNum)%HRBranchNum
    CompIndex    =  EngineDrivenChiller(ChillNum)%HRCompNum

    If ( RunFlag ) Then
      mdot = EngineDrivenChiller(ChillNum)%DesignHeatRecMassFlowRate
    ELSE
      mdot = 0.d0
    ENDIF

    CALL SetComponentFlowRate(mdot,InletNode,OutletNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

  END IF
  IF (EngineDrivenChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
    BasinHeaterPower       = 0.0d0
  ENDIF

  RETURN

END SUBROUTINE InitEngineDrivenChiller

SUBROUTINE InitGTChiller(ChillNum,RunFlag, MyLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   November 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Gas Turbine Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_CombTurbine, ScanPlantLoopsForObject, &
                              PlantSizeNotComplete, PlantSizesOkayToFinalize, LoopFlowStatus_NeedyIfLoopOn
  USE DataEnvironment, ONLY : StdBaroPress
  USE Psychrometrics,  ONLY : PsyRhoAirFnPbTdbW
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current engine driven chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='InitGTChiller'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyFlag
  INTEGER   :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER   :: CondOutletNode     ! node number of water outlet node from the condenser
  INTEGER   :: EvapInletNode
  INTEGER   :: EvapOutletNode
  INTEGER   :: HeatRecInNode
  INTEGER   :: HeatRecOutNode
  REAL(r64) :: rho       ! local fluid density
  REAL(r64) :: mdot      ! local mass flow rate
  REAL(r64) :: mdotCond  ! local mass flow rate for condenser
  INTEGER   :: InletNode
  INTEGER   :: OutletNode
  INTEGER   :: LoopNum
  INTEGER   :: LoopSideNum
  INTEGER   :: BranchIndex
  INTEGER   :: CompIndex
  LOGICAL   :: FatalError
  LOGICAL :: errFlag
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumGTChillers))
    ALLOCATE(MyEnvrnFlag(NumGTChillers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  CondInletNode  = GTChiller(ChillNum)%Base%CondInletNodeNum
  CondOutletNode = GTChiller(ChillNum)%Base%CondOutletNodeNum
  EvapInletNode  = GTChiller(ChillNum)%Base%EvapInletNodeNum
  EvapOutletNode = GTChiller(ChillNum)%Base%EvapOutletNodeNum

  IF (GTChiller(ChillNum)%HeatRecActive) THEN
    HeatRecInNode  = GTChiller(ChillNum)%HeatRecInletNodeNum
    HeatRecOutNode = GTChiller(ChillNum)%HeatRecOutletNodeNum
  ENDIF

  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(GTChiller(ChillNum)%Base%Name, &
                                 TypeOf_Chiller_CombTurbine, &
                                 GTChiller(ChillNum)%Base%CWLoopNum, &
                                 GTChiller(ChillNum)%Base%CWLoopSideNum, &
                                 GTChiller(ChillNum)%Base%CWBranchNum, &
                                 GTChiller(ChillNum)%Base%CWCompNum, &
                                 LowLimitTemp = GTChiller(ChillNum)%TempLowLimitEvapOut , &
                                 InletNodeNumber = GTChiller(ChillNum)%Base%EvapInletNodeNum,  &
                                 errFlag=errFlag)
    IF (GTChiller(ChillNum)%Base%CondenserType /= AirCooled .AND. &
        GTChiller(ChillNum)%Base%CondenserType /= EvapCooled) THEN
      CALL ScanPlantLoopsForObject(GTChiller(ChillNum)%Base%Name, &
                                   TypeOf_Chiller_CombTurbine, &
                                   GTChiller(ChillNum)%Base%CDLoopNum, &
                                   GTChiller(ChillNum)%Base%CDLoopSideNum, &
                                   GTChiller(ChillNum)%Base%CDBranchNum, &
                                   GTChiller(ChillNum)%Base%CDCompNum, &
                                   InletNodeNumber = GTChiller(ChillNum)%Base%CondInletNodeNum,  &
                                   errFlag=errFlag)
       CALL InterConnectTwoPlantLoopSides( GTChiller(ChillNum)%Base%CWLoopNum,     &
                                          GTChiller(ChillNum)%Base%CWLoopSideNum,  &
                                          GTChiller(ChillNum)%Base%CDLoopNum,      &
                                          GTChiller(ChillNum)%Base%CDLoopSideNum,  &
                                          TypeOf_Chiller_CombTurbine, .TRUE. )
    ENDIF
    IF (GTChiller(ChillNum)%HeatRecActive) THEN
      CALL ScanPlantLoopsForObject(GTChiller(ChillNum)%Base%Name, &
                                   TypeOf_Chiller_CombTurbine, &
                                   GTChiller(ChillNum)%HRLoopNum, &
                                   GTChiller(ChillNum)%HRLoopSideNum, &
                                   GTChiller(ChillNum)%HRBranchNum, &
                                   GTChiller(ChillNum)%HRCompNum, &
                                   InletNodeNumber = GTChiller(ChillNum)%HeatRecInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( GTChiller(ChillNum)%Base%CWLoopNum,      &
                                          GTChiller(ChillNum)%Base%CWLoopSideNum,  &
                                          GTChiller(ChillNum)%HRLoopNum,     &
                                          GTChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_CombTurbine , .TRUE. )
    ENDIF

    IF (GTChiller(ChillNum)%Base%CondenserType /= AirCooled  .AND. &
        GTChiller(ChillNum)%Base%CondenserType /= EvapCooled .AND. &
        GTChiller(ChillNum)%HeatRecActive) THEN
      CALL InterConnectTwoPlantLoopSides( GTChiller(ChillNum)%Base%CDLoopNum,      &
                                          GTChiller(ChillNum)%Base%CDLoopSideNum,  &
                                          GTChiller(ChillNum)%HRLoopNum,     &
                                          GTChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_CombTurbine, .FALSE. )
    ENDIF
    IF (errFlag) THEN
      CALL ShowFatalError('InitGTChiller: Program terminated due to previous condition(s).')
    ENDIF

    IF (GTChiller(ChillNum)%Base%FlowMode == ConstantFlow) THEN
      ! reset flow priority
      PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%LoopSide(GTChiller(ChillNum)%Base%CWLoopSideNum)% &
          Branch(GTChiller(ChillNum)%Base%CWBranchNum)%Comp(GTChiller(ChillNum)%Base%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    ENDIF

    IF (GTChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) THEN
      ! reset flow priority
      PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%LoopSide(GTChiller(ChillNum)%Base%CWLoopSideNum)% &
          Branch(GTChiller(ChillNum)%Base%CWBranchNum)%Comp(GTChiller(ChillNum)%Base%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn

    ! check if setpoint on outlet node
      IF ((Node(GTChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
          (Node(GTChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi == SensedNodeFlagValue))  THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. GTChiller(ChillNum)%Base%ModulatedFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(GTChiller(ChillNum)%Base%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            GTChiller(ChillNum)%Base%ModulatedFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(GTChiller(ChillNum)%Base%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. GTChiller(ChillNum)%Base%ModulatedFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(GTChiller(ChillNum)%Base%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              GTChiller(ChillNum)%Base%ModulatedFlowErrDone = .TRUE.
            ENDIF
          ENDIF


        ENDIF
        GTChiller(ChillNum)%Base%ModulatedFlowSetToLoop = .TRUE.
        Node(GTChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint =                        &
          Node(PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
        Node(GTChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi =                        &
          Node(PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
      ENDIF
    ENDIF
    MyFlag(ChillNum)=.FALSE.
  ENDIF

  IF(MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete) CALL SizeGTChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%FluidIndex,&
                                RoutineName)

    GTChiller(ChillNum)%Base%EvapMassFlowRateMax = rho * GTChiller(ChillNum)%Base%EvapVolFlowRate
    CALL InitComponentNodes(0.0D0,GTChiller(ChillNum)%Base%EvapMassFlowRateMax,  &
                         EvapInletNode,        &
                         EvapOutletNode,       &
                         GTChiller(ChillNum)%Base%CWLoopNum,               &
                         GTChiller(ChillNum)%Base%CWLoopSideNum,           &
                         GTChiller(ChillNum)%Base%CWBranchNum,             &
                         GTChiller(ChillNum)%Base%CWCompNum)

          !init maximum available condenser flow rate
    IF (GTChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN

      Node(CondInletNode)%Temp = GTChiller(ChillNum)%TempDesCondIn

      rho = GetDensityGlycol(PlantLoop(GTChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(GTChiller(ChillNum)%Base%CDLoopNum)%FluidIndex,&
                                  RoutineName)

      GTChiller(ChillNum)%Base%CondMassFlowRateMax = rho * GTChiller(ChillNum)%Base%CondVolFlowRate

      CALL InitComponentNodes(0.0D0,  GTChiller(ChillNum)%Base%CondMassFlowRateMax,  &
                         CondInletNode,        &
                         CondOutletNode,       &
                         GTChiller(ChillNum)%Base%CDLoopNum,               &
                         GTChiller(ChillNum)%Base%CDLoopSideNum,           &
                         GTChiller(ChillNum)%Base%CDBranchNum,             &
                         GTChiller(ChillNum)%Base%CDCompNum)
    ELSE ! air or evap-air
      Node(CondInletNode)%MassFlowRate        = GTChiller(ChillNum)%Base%CondVolFlowRate * &
        PsyRhoAirFnPbTdbW(StdBaroPress,GTChiller(ChillNum)%TempDesCondIn,0.0D0,RoutineName)

      Node(CondOutletNode)%MassFlowrate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0d0
      Node(CondInletNode)%MassFlowRateMin       = 0.0d0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0d0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0d0
    END IF

    IF (GTChiller(ChillNum)%HeatRecActive) THEN
      rho = GetDensityGlycol(PlantLoop(GTChiller(ChillNum)%HRLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(GTChiller(ChillNum)%HRLoopNum)%FluidIndex,&
                                  RoutineName)
      GTChiller(ChillNum)%DesignHeatRecMassFlowRate = rho * &
                                         GTChiller(ChillNum)%DesignHeatRecVolFlowRate

      CALL InitComponentNodes(0.0D0, GTChiller(ChillNum)%DesignHeatRecMassFlowRate ,  &
                         GTChiller(ChillNum)%HeatRecInletNodeNum,        &
                         GTChiller(ChillNum)%HeatRecOutletNodeNum,       &
                         GTChiller(ChillNum)%HRLoopNum,                  &
                         GTChiller(ChillNum)%HRLoopSideNum,              &
                         GTChiller(ChillNum)%HRBranchNum,                &
                         GTChiller(ChillNum)%HRCompNum)

    ENDIF

    MyEnvrnFlag(ChillNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ChillNum)=.true.
  ENDIF

  IF ( (GTChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) &
      .AND. (GTChiller(ChillNum)%Base%ModulatedFlowSetToLoop)) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(GTChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(GTChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi =                        &
         Node(PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
  ENDIF

  IF ((ABS(MyLoad) > 0.d0) .AND. RunFlag)  THEN
    mdot     = GTChiller(ChillNum)%Base%EvapMassFlowRateMax
    mdotCond = GTChiller(ChillNum)%Base%CondMassFlowRateMax
  ELSE
    mdot     = 0.d0
    mdotCond = 0.d0
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              GTChiller(ChillNum)%Base%CWLoopNum,     &
                              GTChiller(ChillNum)%Base%CWLoopSideNum, &
                              GTChiller(ChillNum)%Base%CWBranchNum,   &
                              GTChiller(ChillNum)%Base%CWCompNum)
  IF (GTChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,         &
                              GTChiller(ChillNum)%Base%CDLoopNum,     &
                              GTChiller(ChillNum)%Base%CDLoopSideNum, &
                              GTChiller(ChillNum)%Base%CDBranchNum,   &
                              GTChiller(ChillNum)%Base%CDCompNum)
  ENDIF

  ! Initialize heat recovery flow rates at node
  IF (GTChiller(ChillNum)%HeatRecActive ) THEN

    InletNode    =  GTChiller(ChillNum)%HeatRecInletNodeNum
    OutletNode   =  GTChiller(ChillNum)%HeatRecOutletNodeNum
    LoopNum      =  GTChiller(ChillNum)%HRLoopNum
    LoopSideNum  =  GTChiller(ChillNum)%HRLoopSideNum
    BranchIndex  =  GTChiller(ChillNum)%HRBranchNum
    CompIndex    =  GTChiller(ChillNum)%HRCompNum

    IF ( RunFlag ) THEN
      mdot = GTChiller(ChillNum)%DesignHeatRecMassFlowRate
    ELSE
      mdot = 0.d0
    ENDIF

    CALL SetComponentFlowRate(mdot,InletNode,OutletNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

  END IF
  IF (GTChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
    BasinHeaterPower       = 0.0d0
  ENDIF
  RETURN

END SUBROUTINE InitGTChiller

SUBROUTINE InitConstCOPChiller(ChillNum,RunFlag, MyLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Electric Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! Based on InitElectricChiller from Fred Buhl

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, ScanPlantLoopsForObject, &
                              PlantSizeNotComplete, PlantSizesOkayToFinalize, LoopFlowStatus_NeedyIfLoopOn
  USE DataEnvironment, ONLY : StdBaroPress
  USE Psychrometrics,  ONLY : PsyRhoAirFnPbTdbW
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current electric chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='InitConstCOPChiller'
  REAL(r64), parameter        :: TempDesCondIn = 25.d0        ! Design condenser inlet temp. C


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: OneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvironFlag
  INTEGER :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER :: CondOutletNode     ! node number of water outlet node from the condenser
  INTEGER :: EvapInletNode
  INTEGER :: EvapOutletNode
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: mdot ! local mass flow rate
  REAL(r64) :: mdotCond ! local mass flow rate for condenser
  LOGICAL :: FatalError
  LOGICAL :: errFlag
          !FLOW
  ! Do the one time initializations
  IF (OneTimeFlag) THEN
    ALLOCATE(MyFlag(NumConstCOPChillers))
    ALLOCATE(MyEnvironFlag(NumConstCOPChillers))
    MyFlag = .TRUE.
    MyEnvironFlag = .TRUE.
    OneTimeFlag = .false.
  END IF

  EvapInletNode            = ConstCOPChiller(ChillNum)%Base%EvapInletNodeNum
  EvapOutletNode           = ConstCOPChiller(ChillNum)%Base%EvapOutletNodeNum
  CondInletNode            = ConstCOPChiller(ChillNum)%Base%CondInletNodeNum
  CondOutletNode           = ConstCOPChiller(ChillNum)%Base%CondOutletNodeNum

  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(ConstCOPChiller(ChillNum)%Base%Name, &
                                 TypeOf_Chiller_ConstCOP, &
                                 ConstCOPChiller(ChillNum)%Base%CWLoopNum, &
                                 ConstCOPChiller(ChillNum)%Base%CWLoopSideNum, &
                                 ConstCOPChiller(ChillNum)%Base%CWBranchNum, &
                                 ConstCOPChiller(ChillNum)%Base%CWCompNum,  &
                                 InletNodeNumber = ConstCOPChiller(ChillNum)%Base%EvapInletNodeNum,  &
                                 errFlag=errFlag)
    IF (ConstCOPChiller(ChillNum)%Base%CondenserType /= AirCooled .AND. &
        ConstCOPChiller(ChillNum)%Base%CondenserType /= EvapCooled) THEN
      CALL ScanPlantLoopsForObject(ConstCOPChiller(ChillNum)%Base%Name, &
                                   TypeOf_Chiller_ConstCOP, &
                                   ConstCOPChiller(ChillNum)%Base%CDLoopNum, &
                                   ConstCOPChiller(ChillNum)%Base%CDLoopSideNum, &
                                   ConstCOPChiller(ChillNum)%Base%CDBranchNum, &
                                   ConstCOPChiller(ChillNum)%Base%CDCompNum,  &
                                   InletNodeNumber = ConstCOPChiller(ChillNum)%Base%CondInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ConstCOPChiller(ChillNum)%Base%CWLoopNum,      &
                                          ConstCOPChiller(ChillNum)%Base%CWLoopSideNum,  &
                                          ConstCOPChiller(ChillNum)%Base%CDLoopNum,      &
                                          ConstCOPChiller(ChillNum)%Base%CDLoopSideNum,  &
                                          TypeOf_Chiller_ConstCOP, .TRUE. )
    ENDIF

    IF (errFlag) THEN
      CALL ShowFatalError('CalcConstCOPChillerModel: Program terminated due to previous condition(s).')
    ENDIF
    IF (ConstCOPChiller(ChillNum)%Base%FlowMode == ConstantFlow ) THEN
      ! reset flow priority
      PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%LoopSide(ConstCOPChiller(ChillNum)%Base%CWLoopSideNum)% &
          Branch(ConstCOPChiller(ChillNum)%Base%CWBranchNum)%Comp(ConstCOPChiller(ChillNum)%Base%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    ENDIF

    IF (ConstCOPChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated ) THEN
      ! reset flow priority
      PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%LoopSide(ConstCOPChiller(ChillNum)%Base%CWLoopSideNum)% &
          Branch(ConstCOPChiller(ChillNum)%Base%CWBranchNum)%Comp(ConstCOPChiller(ChillNum)%Base%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn

      ! check if setpoint on outlet node
      IF ((Node(ConstCOPChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
          (Node(ConstCOPChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi == SensedNodeFlagValue)) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. ConstCOPChiller(ChillNum)%Base%ModulatedFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(ConstCOPChiller(ChillNum)%Base%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            ConstCOPChiller(ChillNum)%Base%ModulatedFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(ConstCOPChiller(ChillNum)%Base%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. ConstCOPChiller(ChillNum)%Base%ModulatedFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(ConstCOPChiller(ChillNum)%Base%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              ConstCOPChiller(ChillNum)%Base%ModulatedFlowErrDone = .TRUE.
            ENDIF
          ENDIF
        ENDIF
        ConstCOPChiller(ChillNum)%Base%ModulatedFlowSetToLoop = .TRUE.
        Node(ConstCOPChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint =                        &
          Node(PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
        Node(ConstCOPChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi =                        &
          Node(PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
      ENDIF
    ENDIF
    MyFlag(ChillNum)=.FALSE.
  ENDIF

     !Initialize critical Demand Side Variables at the beginning of each environment
  IF(MyEnvironFlag(ChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize))Then
    IF (PlantSizeNotComplete) CALL SizeConstCOPChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%FluidIndex,&
                                RoutineName)
    ConstCOPChiller(ChillNum)%Base%EvapMassFlowRateMax = ConstCOPChiller(ChillNum)%Base%EvapVolFlowRate * rho
    CALL InitComponentNodes(0.0D0,ConstCOPChiller(ChillNum)%Base%EvapMassFlowRateMax,  &
                         EvapInletNode,        &
                         EvapOutletNode,       &
                         ConstCOPChiller(ChillNum)%Base%CWLoopNum,               &
                         ConstCOPChiller(ChillNum)%Base%CWLoopSideNum,           &
                         ConstCOPChiller(ChillNum)%Base%CWBranchNum,             &
                         ConstCOPChiller(ChillNum)%Base%CWCompNum)

          !init maximum available condenser flow rate
    IF (ConstCOPChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN

      Node(CondInletNode)%Temp = TempDesCondIn

      rho = GetDensityGlycol(PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)%FluidIndex,&
                                  RoutineName)

      ConstCOPChiller(ChillNum)%Base%CondMassFlowRateMax = rho * ConstCOPChiller(ChillNum)%Base%CondVolFlowRate

      CALL InitComponentNodes(0.0D0,  ConstCOPChiller(ChillNum)%Base%CondMassFlowRateMax,  &
                         CondInletNode,        &
                         CondOutletNode,       &
                         ConstCOPChiller(ChillNum)%Base%CDLoopNum,               &
                         ConstCOPChiller(ChillNum)%Base%CDLoopSideNum,           &
                         ConstCOPChiller(ChillNum)%Base%CDBranchNum,             &
                         ConstCOPChiller(ChillNum)%Base%CDCompNum)
    ELSE ! air or evap-air
      Node(CondInletNode)%MassFlowRate        = ConstCOPChiller(ChillNum)%Base%CondVolFlowRate * &
        PsyRhoAirFnPbTdbW(StdBaroPress,TempDesCondIn,0.0D0,RoutineName)

      Node(CondOutletNode)%MassFlowrate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0d0
      Node(CondInletNode)%MassFlowRateMin       = 0.0d0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0d0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0d0
    END IF
    MyEnvironFlag(ChillNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvironFlag(ChillNum)=.true.
  ENDIF
  IF ((ConstCOPChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) &
      .AND. (ConstCOPChiller(ChillNum)%Base%ModulatedFlowSetToLoop)) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(ConstCOPChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(ConstCOPChiller(ChillNum)%Base%EvapOutletNodeNum)%TempSetPointHi =                        &
         Node(PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
  ENDIF

  IF ((MyLoad < 0.d0) .AND. RunFlag)  THEN
    mdot     = ConstCOPChiller(ChillNum)%Base%EvapMassFlowRateMax
    mdotCond = ConstCOPChiller(ChillNum)%Base%CondMassFlowRateMax
  ELSE
    mdot     = 0.d0
    mdotCond = 0.d0
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              ConstCOPChiller(ChillNum)%Base%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%Base%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%Base%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%Base%CWCompNum)
  IF (ConstCOPChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,         &
                                ConstCOPChiller(ChillNum)%Base%CDLoopNum,     &
                                ConstCOPChiller(ChillNum)%Base%CDLoopSideNum, &
                                ConstCOPChiller(ChillNum)%Base%CDBranchNum,   &
                                ConstCOPChiller(ChillNum)%Base%CDCompNum)
  ENDIF

  IF (ConstCOPChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
    BasinHeaterPower       = 0.0d0
  ENDIF

END SUBROUTINE InitConstCOPChiller

SUBROUTINE SizeElectricChiller(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, April 2011, allow repeated sizing calls, finish when ready to do so

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Electric Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined

  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  LOGICAL             :: LoopErrorsFound
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           ::  rho ! local fluid density
  REAL(r64)           ::  Cp  ! local fluid specific heat
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate
  REAL(r64)           :: tmpHeatRecVolFlowRate ! local heat recovery design volume flow rate

  PltSizNum = 0
  PltSizCondNum = 0
  ErrorsFound = .FALSE.
  ! init local temporary version in case of partial/mixed autosizing
  tmpNomCap          = ElectricChiller(ChillNum)%Base%NomCap
  tmpEvapVolFlowRate = ElectricChiller(ChillNum)%Base%EvapVolFlowRate
  tmpCondVolFlowRate = ElectricChiller(ChillNum)%Base%CondVolFlowRate

  IF (ElectricChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    IF (ElectricChiller(ChillNum)%Base%CondVolFlowRate == AutoSize) THEN
      PltSizCondNum = PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%PlantSizNum
    END IF
  END IF

  PltSizNum = PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%PlantSizNum

  IF (ElectricChiller(ChillNum)%Base%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%FluidIndex,&
                                  'SizeElectricChiller')
        Cp = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                 InitConvTemp,                      &
                                 PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%FluidIndex, &
                                 'SizeElectricChiller')
        tmpNomCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                                    * PlantSizData(PltSizNum)%DesVolFlowRate * ElectricChiller(ChillNum)%Base%SizFac
        IF (PlantSizesOkayToFinalize) ElectricChiller(ChillNum)%Base%NomCap = tmpNomCap

      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) ElectricChiller(ChillNum)%Base%NomCap = tmpNomCap
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Electric', ElectricChiller(ChillNum)%Base%Name, &
                                                        'Nominal Capacity [W]', ElectricChiller(ChillNum)%Base%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Electric Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Electric Chiller object='//TRIM(ElectricChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (ElectricChiller(ChillNum)%Base%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * ElectricChiller(ChillNum)%Base%SizFac
        IF (PlantSizesOkayToFinalize) ElectricChiller(ChillNum)%Base%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize)  ElectricChiller(ChillNum)%Base%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput('Chiller:Electric', ElectricChiller(ChillNum)%Base%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              ElectricChiller(ChillNum)%Base%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Electric Chiller object='//TRIM(ElectricChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(ElectricChiller(ChillNum)%Base%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF (ElectricChiller(ChillNum)%Base%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                  ElectricChiller(ChillNum)%TempDesCondIn, &
                                  PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidIndex,&
                                  'SizeElectricChiller')

        Cp = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                 ElectricChiller(ChillNum)%TempDesCondIn,                      &
                                 PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidIndex, &
                                 'SizeElectricChiller')
        tmpCondVolFlowRate = tmpNomCap *   (1.d0 + 1.d0/ElectricChiller(ChillNum)%Base%COP) / &
                                             ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize) ElectricChiller(ChillNum)%Base%CondVolFlowRate = tmpCondVolFlowRate

      ELSE
        tmpCondVolFlowRate = 0.0d0
        IF (PlantSizesOkayToFinalize)  ElectricChiller(ChillNum)%Base%CondVolFlowRate = 0.d0
      END IF
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput('Chiller:Electric', ElectricChiller(ChillNum)%Base%Name, &
                              'Design Condenser Water Flow Rate [m3/s]', &
                              ElectricChiller(ChillNum)%Base%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Electric Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Electric Chiller object='//TRIM(ElectricChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (ElectricChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    CALL RegisterPlantCompDesignFlow(ElectricChiller(ChillNum)%Base%CondInletNodeNum,tmpCondVolFlowRate)
  ENDIF
  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  IF (ElectricChiller(ChillNum)%HeatRecActive) THEN
    tmpHeatRecVolFlowRate = ElectricChiller(ChillNum)%DesignHeatRecVolFlowRate
    IF ( ElectricChiller(ChillNum)%DesignHeatRecVolFlowRate == Autosize) THEN
      tmpHeatRecVolFlowRate = tmpCondVolFlowRate * ElectricChiller(ChillNum)%HeatRecCapacityFraction
      IF (PlantSizesOkayToFinalize) THEN
        ElectricChiller(ChillNum)%DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate
        CALL ReportSizingOutput('Chiller:Electric', ElectricChiller(ChillNum)%Base%Name, &
                              'Design Heat Recovery Fluid Flow Rate [m3/s]', &
                              ElectricChiller(ChillNum)%DesignHeatRecVolFlowRate)
      ENDIF
    ENDIF
    ! save the reference heat recovery fluid volumetric flow rate
    CALL RegisterPlantCompDesignFlow(ElectricChiller(ChillNum)%HeatRecInletNodeNum,tmpHeatRecVolFlowRate)
  ENDIF

  IF (PlantSizesOkayToFinalize) Then
    !create predefined report
    equipName = ElectricChiller(ChillNum)%Base%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:Electric')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,ElectricChiller(ChillNum)%Base%COP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,ElectricChiller(ChillNum)%Base%NomCap)
  ENDIF

  RETURN
END SUBROUTINE SizeElectricChiller

SUBROUTINE SizeEngineDrivenChiller(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Engine Driven Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  LOGICAL             :: LoopErrorsFound
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           ::  rho ! local fluid density
  REAL(r64)           ::  Cp  ! local fluid specific heat
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate

  PltSizNum = 0
  PltSizCondNum = 0
  ErrorsFound = .FALSE.
  tmpNomCap          = EngineDrivenChiller(ChillNum)%Base%NomCap
  tmpEvapVolFlowRate = EngineDrivenChiller(ChillNum)%Base%EvapVolFlowRate
  tmpCondVolFlowRate = EngineDrivenChiller(ChillNum)%Base%CondVolFlowRate

  IF (EngineDrivenChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    IF (EngineDrivenChiller(ChillNum)%Base%CondVolFlowRate == AutoSize) THEN
      PltSizCondNum = PlantLoop(EngineDrivenChiller(ChillNum)%Base%CDLoopNum)%PlantSizNum
    END IF
  END IF

  PltSizNum = PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%PlantSizNum

  IF (EngineDrivenChiller(ChillNum)%Base%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%FluidIndex,&
                                  'SizeEngineDrivenChiller')
        Cp = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                 InitConvTemp,                      &
                                 PlantLoop(EngineDrivenChiller(ChillNum)%Base%CWLoopNum)%FluidIndex, &
                                 'SizeEngineDrivenChiller')
        tmpNomCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                              * PlantSizData(PltSizNum)%DesVolFlowRate * EngineDrivenChiller(ChillNum)%Base%SizFac
        IF (PlantSizesOkayToFinalize) EngineDrivenChiller(ChillNum)%Base%NomCap =  tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize)  EngineDrivenChiller(ChillNum)%Base%NomCap = tmpNomCap
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:EngineDriven', EngineDrivenChiller(ChillNum)%Base%Name, &
                              'Nominal Capacity [W]', EngineDrivenChiller(ChillNum)%Base%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Engine Driven Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Engine Driven Chiller object='//TRIM(EngineDrivenChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (EngineDrivenChiller(ChillNum)%Base%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate *   &
                                    EngineDrivenChiller(ChillNum)%Base%SizFac
        IF (PlantSizesOkayToFinalize) EngineDrivenChiller(ChillNum)%Base%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) EngineDrivenChiller(ChillNum)%Base%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:EngineDriven', EngineDrivenChiller(ChillNum)%Base%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              EngineDrivenChiller(ChillNum)%Base%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Engine Driven Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Engine Driven Chiller object='//TRIM(EngineDrivenChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(EngineDrivenChiller(ChillNum)%Base%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF (EngineDrivenChiller(ChillNum)%Base%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                  EngineDrivenChiller(ChillNum)%TempDesCondIn, &
                                  PlantLoop(EngineDrivenChiller(ChillNum)%Base%CDLoopNum)%FluidIndex,&
                                  'SizeEngineDrivenChiller')

        Cp = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                 EngineDrivenChiller(ChillNum)%TempDesCondIn,                      &
                                 PlantLoop(EngineDrivenChiller(ChillNum)%Base%CDLoopNum)%FluidIndex, &
                                 'SizeEngineDrivenChiller')
        tmpCondVolFlowRate = tmpNomCap *   (1.d0 + 1.d0/EngineDrivenChiller(ChillNum)%Base%COP) / &
                                                 ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize)  EngineDrivenChiller(ChillNum)%Base%CondVolFlowRate = tmpCondVolFlowRate

      ELSE
        tmpCondVolFlowRate = 0.0d0
        IF (PlantSizesOkayToFinalize)  EngineDrivenChiller(ChillNum)%Base%CondVolFlowRate = tmpCondVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput('Chiller:EngineDriven', EngineDrivenChiller(ChillNum)%Base%Name, &
                              'Design Condenser Water Flow Rate [m3/s]', &
                              EngineDrivenChiller(ChillNum)%Base%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of EngineDriven Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in EngineDriven Chiller object='//TRIM(EngineDrivenChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (EngineDrivenChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    CALL RegisterPlantCompDesignFlow(EngineDrivenChiller(ChillNum)%Base%CondInletNodeNum, tmpCondVolFlowRate)
  ENDIF

  IF (PlantSizesOkayToFinalize) Then
    !create predefined report
    equipName = EngineDrivenChiller(ChillNum)%Base%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:EngineDriven')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,EngineDrivenChiller(ChillNum)%Base%COP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,EngineDrivenChiller(ChillNum)%Base%NomCap)
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeEngineDrivenChiller

SUBROUTINE SizeGTChiller(ChillNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Gas Turbine Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  LOGICAL             :: LoopErrorsFound
  REAL(r64)           :: EngineEff     ! this should be an input! needed to autosize the engine capacity.
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           ::  rho  ! local fluid density
  REAL(r64)           ::  Cp   ! local fluid specific heat
  REAL(r64)           ::  tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           ::  tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           ::  tmpCondVolFlowRate ! local condenser design volume flow rate

  PltSizNum = 0
  PltSizCondNum = 0
  EngineEff = 0.35d0
  ErrorsFound = .FALSE.
  tmpNomCap          = GTChiller(ChillNum)%Base%NomCap
  tmpEvapVolFlowRate = GTChiller(ChillNum)%Base%EvapVolFlowRate
  tmpCondVolFlowRate = GTChiller(ChillNum)%Base%CondVolFlowRate

  IF (GTChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    IF (GTChiller(ChillNum)%Base%CondVolFlowRate == AutoSize) THEN
      PltSizCondNum = PlantLoop(GTChiller(ChillNum)%Base%CDLoopNum)%PlantSizNum
    END IF
  END IF

  PltSizNum = PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%PlantSizNum

  IF (GTChiller(ChillNum)%Base%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%FluidIndex,&
                                  'SizeGTChiller')
        Cp = GetSpecificHeatGlycol(PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                   InitConvTemp,                      &
                                   PlantLoop(GTChiller(ChillNum)%Base%CWLoopNum)%FluidIndex, &
                                   'SizeGTChiller')
        tmpNomCap = Cp * Rho * PlantSizData(PltSizNum)%DeltaT &
                                        * PlantSizData(PltSizNum)%DesVolFlowRate * GTChiller(ChillNum)%Base%SizFac
        IF (PlantSizesOkayToFinalize)  GTChiller(ChillNum)%Base%NomCap = tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%Base%NomCap = tmpNomCap
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:CombustionTurbine', GTChiller(ChillNum)%Base%Name, &
                              'Nominal Capacity [W]', GTChiller(ChillNum)%Base%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Gas Turbine Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Gas Turbine Chiller object='//TRIM(GTChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (GTChiller(ChillNum)%Base%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * GTChiller(ChillNum)%Base%SizFac
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%Base%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%Base%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:CombustionTurbine', GTChiller(ChillNum)%Base%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              GTChiller(ChillNum)%Base%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Gas Turbine Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Gas Turbine Chiller object='//TRIM(GTChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(GTChiller(ChillNum)%Base%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF (GTChiller(ChillNum)%Base%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(GTChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                  GTChiller(ChillNum)%TempDesCondIn, &
                                  PlantLoop(GTChiller(ChillNum)%Base%CDLoopNum)%FluidIndex,&
                                  'SizeGTChiller')

        Cp = GetSpecificHeatGlycol(PlantLoop(GTChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                 GTChiller(ChillNum)%TempDesCondIn,                      &
                                 PlantLoop(GTChiller(ChillNum)%Base%CDLoopNum)%FluidIndex, &
                                 'SizeGTChiller')
        tmpCondVolFlowRate = tmpNomCap *  (1.0d0 + 1.0d0/GTChiller(ChillNum)%Base%COP) / &
                                ( PlantSizData(PltSizCondNum)%DeltaT * Cp * Rho)
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%Base%CondVolFlowRate = tmpCondVolFlowRate
      ELSE
        tmpCondVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%Base%CondVolFlowRate = tmpCondVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:CombustionTurbine', GTChiller(ChillNum)%Base%Name, &
                              'Design Condenser Water Flow Rate [m3/s]', &
                              GTChiller(ChillNum)%Base%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Gas Turbine Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Gas Turbine Chiller object='//TRIM(GTChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF
  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (GTChiller(ChillNum)%Base%CondenserType == WaterCooled) &
    CALL RegisterPlantCompDesignFlow(GTChiller(ChillNum)%Base%CondInletNodeNum,tmpCondVolFlowRate)

  IF (GTChiller(ChillNum)%GTEngineCapacity  == AutoSize .and. PlantSizesOkayToFinalize ) THEN
    GTChiller(ChillNum)%GTEngineCapacity = GTChiller(ChillNum)%Base%NomCap * EngineEff &
                                                    / GTChiller(ChillNum)%Base%COP
  IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:CombustionTurbine', GTChiller(ChillNum)%Base%Name, &
                            'Gas Turbine Engine Capacity [W]', GTChiller(ChillNum)%GTEngineCapacity)
  END IF


  IF (PlantSizesOkayToFinalize) Then
    !create predefined report
    equipName = GTChiller(ChillNum)%Base%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:CombustionTurbine')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,GTChiller(ChillNum)%Base%COP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,GTChiller(ChillNum)%Base%NomCap)
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizeGTChiller

SUBROUTINE SizeConstCOPChiller(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Constabt COP Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  LOGICAL             :: LoopErrorsFound
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           ::  rho ! local fluid density
  REAL(r64)           ::  Cp  ! local fluid specific heat
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate

  PltSizNum = 0
  PltSizCondNum = 0
  ErrorsFound = .FALSE.
  tmpNomCap          = ConstCOPChiller(ChillNum)%Base%NomCap
  tmpEvapVolFlowRate = ConstCOPChiller(ChillNum)%Base%EvapVolFlowRate
  tmpCondVolFlowRate = ConstCOPChiller(ChillNum)%Base%CondVolFlowRate

  IF (ConstCOPChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    IF (ConstCOPChiller(ChillNum)%Base%CondVolFlowRate == AutoSize) THEN
      PltSizCondNum = PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)%PlantSizNum
    END IF
  END IF

  PltSizNum = PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%PlantSizNum

  IF (ConstCOPChiller(ChillNum)%Base%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%FluidIndex,&
                                  'SizeConstCOPChiller')
        Cp = GetSpecificHeatGlycol(PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                                   InitConvTemp,                      &
                                   PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%FluidIndex, &
                                 'SizeConstCOPChiller')
        tmpNomCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                              * PlantSizData(PltSizNum)%DesVolFlowRate * ConstCOPChiller(ChillNum)%Base%SizFac
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%Base%NomCap = tmpNomCap

      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%Base%NomCap = tmpNomCap
      END IF
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:ConstantCOP', ConstCOPChiller(ChillNum)%Base%Name, &
                              'Nominal Capacity [W]', ConstCOPChiller(ChillNum)%Base%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Constant COP Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:ConstantCOP object='//TRIM(ConstCOPChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (ConstCOPChiller(ChillNum)%Base%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * ConstCOPChiller(ChillNum)%Base%SizFac
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%Base%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize)  ConstCOPChiller(ChillNum)%Base%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:ConstantCOP', ConstCOPChiller(ChillNum)%Base%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              ConstCOPChiller(ChillNum)%Base%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Constant COP Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:ConstantCOP object='//TRIM(ConstCOPChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(ConstCOPChiller(ChillNum)%Base%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF ((ConstCOPChiller(ChillNum)%Base%CondenserType == WaterCooled) .AND. &
      (ConstCOPChiller(ChillNum)%Base%CondVolFlowRate == AutoSize)) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                               29.44d0, &
                               PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)%FluidIndex,&
                               'SizeConstCOPChiller')

        Cp = GetSpecificHeatGlycol(PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                   29.44d0,                      &
                                   PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)%FluidIndex, &
                                   'SizeConstCOPChiller')
        tmpCondVolFlowRate  = tmpNomCap *  (1.d0 + 1.d0/ConstCOPChiller(ChillNum)%Base%COP) &
                                            /( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%Base%CondVolFlowRate = tmpCondVolFlowRate

      ELSE
        tmpCondVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%Base%CondVolFlowRate = tmpCondVolFlowRate
      END IF
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:ConstantCOP', ConstCOPChiller(ChillNum)%Base%Name, &
                              'Design Condenser Water Flow Rate [m3/s]', &
                              ConstCOPChiller(ChillNum)%Base%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Constant COP Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:ConstantCOP object='//TRIM(ConstCOPChiller(ChillNum)%Base%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (ConstCOPChiller(ChillNum)%Base%CondenserType == WaterCooled) &
      CALL RegisterPlantCompDesignFlow(ConstCOPChiller(ChillNum)%Base%CondInletNodeNum, tmpCondVolFlowRate)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  !create predefined report
  IF (PlantSizesOkayToFinalize) THEN
    equipName = ConstCOPChiller(ChillNum)%Base%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:ConstantCOP')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,ConstCOPChiller(ChillNum)%Base%COP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,ConstCOPChiller(ChillNum)%Base%NomCap)
  ENDIF

  RETURN
END SUBROUTINE SizeConstCOPChiller

SUBROUTINE CalcElectricChillerModel(ChillNum,MyLoad,EquipFlowCtrl,Runflag)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a vapor compression chiller using the Electric model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:

          ! REFERENCES:
          ! 1. BLAST Users Manual
          ! 2. CHILLER User Manual

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals,     ONLY : BeginEnvrnFlag, SecInHour, outputfiledebug, CurrentTime
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys, SysTimeElapsed
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_Electric, CompSetPtBasedSchemeType, &
                              CriteriaType_MassFlowRate, SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY : ControlType_SeriesActive, MassFlowTolerance
  USE DataEnvironment, ONLY : EnvironmentName, CurMnDy
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger
  USE Psychrometrics,  ONLY : PsyCpAirFnWTdb, PsyWFnTdbTwbPb

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: ChillNum        ! chiller number
  REAL(r64)              :: MyLoad          ! operating load
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when chiller operating
  !INTEGER, INTENT(IN)    :: FlowLock        ! TRUE when flow resolver has calculated branch flow
  INTEGER, INTENT(IN)    :: EquipFlowCtrl  ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), DIMENSION(3)     :: CapacityRat           ! intermediate result:  capacity ratio
  REAL(r64), DIMENSION(3)     :: PowerRat              ! intermediate result:  power ratio
  REAL(r64), DIMENSION(3)     :: FullLoadFactor        ! intermediate result:  full load factor
  REAL(r64)              :: MinPartLoadRat        ! min allowed operating frac full load
  REAL(r64)              :: MaxPartLoadRat        ! max allowed operating frac full load
  REAL(r64)              :: TempCondInDesign      ! C - (Electric ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempRiseRat           ! intermediate result:  temperature rise ratio
  REAL(r64)              :: EvapInletTemp         ! C - evaporator inlet temperature, water side
  REAL(r64)              :: CondInletTemp         ! C - condenser inlet temperature, water side
  REAL(r64)              :: TempEvapOut           ! C - evaporator outlet temperature, water side
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  REAL(r64)              :: TempEvapOutDesign     ! design evaporator outlet temperature, water side
  REAL(r64)              :: ChillerNomCap         ! chiller nominal capacity
  REAL(r64)              :: AvailChillerCap       ! chiller available capacity
  REAL(r64)              :: RatedCOP              ! rated coefficient of performance, from user input
  REAL(r64)              :: FracFullLoadPower     ! fraction of full load power
  REAL(r64)              :: EvapDeltaTemp         ! C - evaporator temperature difference, water side
  REAL(r64)              :: DeltaTemp             ! C - intermediate result: condenser/evaporator temp diff
  REAL(r64)              :: AvailNomCapRat        ! intermediate result: available nominal capacity ratio
  REAL(r64)              :: FullLoadPowerRat      ! intermediate result: full load power ratio
  REAL(r64)              :: PartLoadRat           ! part load ratio for efficiency calculation
  REAL(r64)              :: OperPartLoadRat       ! Actual Operating PLR
  REAL(r64)              :: TempLowLimitEout      ! C - Evaporator low temp. limit cut off
  REAL(r64)              :: EvapMassFlowRateMax ! Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
  INTEGER                :: EvapInletNode         ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode        ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode         ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode        ! condenser outlet node number, water side
  REAL(r64)              :: FRAC
!  LOGICAL,SAVE           :: PossibleSubCooling=.false.
  INTEGER                :: PlantLoopNum
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum
  INTEGER                :: BranchNum
  INTEGER                :: CompNum
  REAL(r64),SAVE  :: TimeStepSysLast=0.0d0     ! last system time step (used to check for downshifting)
  REAL(r64)       :: CurrentEndTime          ! end time of time step for current simulation time step
  REAL(r64),SAVE  :: CurrentEndTimeLast=0.0d0  ! end time of time step for last simulation time step
  CHARACTER(len=6):: OutputChar = ' '        ! character string for warning messages
  REAL(r64)       :: Cp ! local for fluid specif heat, for evaporator
  REAL(r64)       :: CpCond ! local for fluid specif heat, for condenser

          !set module level inlet and outlet nodes
  EvapMassFlowRate           = 0.0d0
  CondMassFlowRate           = 0.0d0
  Power                      = 0.0d0
  Energy                     = 0.0d0
  QCondenser                 = 0.0d0
  QEvaporator                = 0.0d0
  CondenserEnergy            = 0.0d0
  EvaporatorEnergy           = 0.0d0
  QHeatRecovered             = 0.0d0
  EvapInletNode  = ElectricChiller(ChillNum)%Base%EvapInletNodeNum
  EvapOutletNode = ElectricChiller(ChillNum)%Base%EvapOutletNodeNum
  CondInletNode  = ElectricChiller(ChillNum)%Base%CondInletNodeNum
  CondOutletNode = ElectricChiller(ChillNum)%Base%CondOutletNodeNum
  FRAC               = 1.0d0
  LoopNum            = ElectricChiller(ChillNum)%Base%CWLoopNum
  LoopSideNum        = ElectricChiller(ChillNum)%Base%CWLoopSideNum
  BranchNum          = ElectricChiller(ChillNum)%Base%CWBranchNum
  CompNum            = ElectricChiller(ChillNum)%Base%CWCompNum
  EvapInletTemp      = Node(EvapInletNode)%Temp

!   calculate end time of current time step
  CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
    IF(ElectricChiller(ChillNum)%Base%PrintMessage)THEN
        ElectricChiller(ChillNum)%Base%MsgErrorCount = &
                         ElectricChiller(ChillNum)%Base%MsgErrorCount + 1
!       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
      IF (ElectricChiller(ChillNum)%Base%MsgErrorCount < 2) THEN
         CALL ShowWarningError(TRIM(ElectricChiller(ChillNum)%Base%MsgBuffer1)//'.')
         CALL ShowContinueError(TRIM(ElectricChiller(ChillNum)%Base%MsgBuffer2))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(ElectricChiller(ChillNum)%Base%MsgBuffer1)//' error continues.', &
           ElectricChiller(ChillNum)%Base%ErrCount1,ReportMaxOf=ElectricChiller(ChillNum)%Base%MsgDataLast,  &
           ReportMinOf=ElectricChiller(ChillNum)%Base%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
      END IF
    END IF
  END IF

!   save last system time step and last end time of current time step (used to determine if warning is valid)
  TimeStepSysLast    = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime

          !If no loop demand or chiller OFF, return
   !If Chiller load is 0 or chiller is not running then leave the subroutine.
  IF(MyLoad >= 0.d0 .OR. .NOT. RunFlag) THEN
     ! call for zero flow before leaving
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    ELSE
      EvapMassFlowRate           = 0.0d0
      CALL SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%Base%CWLoopNum,     &
                              ElectricChiller(ChillNum)%Base%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%Base%CWBranchNum,   &
                              ElectricChiller(ChillNum)%Base%CWCompNum)
    ENDIF
    IF (ElectricChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
      IF ( PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)% &
            LoopSide(ElectricChiller(ChillNum)%Base%CDLoopSideNum)% &
              Branch(ElectricChiller(ChillNum)%Base%CDBranchNum)%  &
                Comp(ElectricChiller(ChillNum)%Base%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate           = Node(CondInletNode)%MassFlowrate
      ELSE
        CondMassFlowRate           = 0.0d0
        CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              ElectricChiller(ChillNum)%Base%CDLoopNum, &
                              ElectricChiller(ChillNum)%Base%CDLoopSideNum, &
                              ElectricChiller(ChillNum)%Base%CDBranchNum, &
                              ElectricChiller(ChillNum)%Base%CDCompNum)
      ENDIF
    ENDIF

    IF (ElectricChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(ElectricChiller(ChillNum)%Base%BasinHeaterPowerFTempDiff,&
                                ElectricChiller(ChillNum)%Base%BasinHeaterSchedulePtr,&
                                ElectricChiller(ChillNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    ElectricChiller(ChillNum)%Base%PrintMessage = .FALSE.
    RETURN
  END IF



   ! If not air or evap cooled then set to the condenser node that is attached to a cooling tower
  CondInletTemp  = Node(CondInletNode)%Temp

        !Set mass flow rates
IF (ElectricChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
  CondMassFlowRate = ElectricChiller(ChillNum)%Base%CondMassFlowRateMax
  CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                            ElectricChiller(ChillNum)%Base%CDLoopNum, &
                            ElectricChiller(ChillNum)%Base%CDLoopSideNum, &
                            ElectricChiller(ChillNum)%Base%CDBranchNum, &
                            ElectricChiller(ChillNum)%Base%CDCompNum)
  CALL PullCompInterconnectTrigger(ElectricChiller(ChillNum)%Base%CWLoopNum, &
                                   ElectricChiller(ChillNum)%Base%CWLoopSideNum, &
                                   ElectricChiller(ChillNum)%Base%CWBranchNum, &
                                   ElectricChiller(ChillNum)%Base%CWCompNum, &
                                   ElectricChiller(ChillNum)%Base%CondMassFlowIndex,              &
                                   ElectricChiller(ChillNum)%Base%CDLoopNum, &
                                   ElectricChiller(ChillNum)%Base%CDLoopSideNum,   &
                                   CriteriaType_MassFlowRate, &
                                   CondMassFlowRate)
  IF (CondMassFlowRate < MassFlowTolerance) RETURN
END IF

      !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  CapacityRat        = ElectricChiller(ChillNum)%CapRatCoef
  PowerRat           = ElectricChiller(ChillNum)%PowerRatCoef
  FullLoadFactor     = ElectricChiller(ChillNum)%FullLoadCoef
  MinPartLoadRat     = ElectricChiller(ChillNum)%MinPartLoadRat
  PartLoadRat        = MinPartLoadRat
  MaxPartLoadRat     = ElectricChiller(ChillNum)%MaxPartLoadRat
  TempCondInDesign   = ElectricChiller(ChillNum)%TempDesCondIn
  TempRiseRat        = ElectricChiller(ChillNum)%TempRiseCoef
  TempEvapOutDesign  = ElectricChiller(ChillNum)%TempDesEvapOut
  ChillerNomCap      = ElectricChiller(ChillNum)%Base%NomCap
  RatedCOP           = ElectricChiller(ChillNum)%Base%COP
  TempEvapOut        = Node(ElectricChiller(ChillNum)%Base%EvapOutletNodeNum)%Temp
  TempLowLimitEout   = ElectricChiller(ChillNum)%TempLowLimitEvapOut
  EvapMassFlowRateMax    = ElectricChiller(ChillNum)%Base%EvapMassFlowRateMax
  PlantLoopNum       = ElectricChiller(ChillNum)%Base%CWLoopNum

  LoopNum            = ElectricChiller(ChillNum)%Base%CWLoopNum
  LoopSideNum        = ElectricChiller(ChillNum)%Base%CWLoopSideNum

! initialize outlet air humidity ratio of air or evap cooled chillers
  CondOutletHumRat = Node(CondInletNode)%HumRat

  IF (ElectricChiller(ChillNum)%Base%CondenserType == AirCooled) THEN !Condenser inlet temp = outdoor temp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirDryBulb
      !  Warn user if entering condenser temperature falls below 0C
      IF(Node(CondInletNode)%Temp .LT. 0.0d0 .and. .not. WarmupFlag) THEN
        ElectricChiller(ChillNum)%Base%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        ElectricChiller(ChillNum)%Base%MsgBuffer1 = 'CalcElectricChillerModel - Chiller:Electric "' &
                             //TRIM(ElectricChiller(ChillNum)%Base%Name)// &
                             '" - Air Cooled Condenser Inlet Temperature below 0C'
        ElectricChiller(ChillNum)%Base%MsgBuffer2 = '... Outdoor Dry-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        ElectricChiller(ChillNum)%Base%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        ElectricChiller(ChillNum)%Base%PrintMessage = .FALSE.
      ENDIF
  Else IF (ElectricChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN !Condenser inlet temp = (outdoor wet bulb)
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirWetBulb
!  line above assumes evaporation pushes condenser inlet air humidity ratio to saturation
    CondOutletHumRat = PsyWFnTdbTwbPb(Node(CondInletNode)%Temp,Node(CondInletNode)%Temp,Node(CondInletNode)%Press)
!  Warn user if evap condenser wet bulb temperature falls below 10C
    IF(Node(CondInletNode)%Temp .LT. 10.0d0 .and. .not. WarmupFlag) THEN
      ElectricChiller(ChillNum)%Base%PrintMessage = .TRUE.
      WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
      ElectricChiller(ChillNum)%Base%MsgBuffer1 = 'CalcElectricChillerModel - Chiller:Electric "' &
                           //TRIM(ElectricChiller(ChillNum)%Base%Name)// &
                           '" - Evap Cooled Condenser Inlet Temperature below 10C'
      ElectricChiller(ChillNum)%Base%MsgBuffer2 = '... Outdoor Wet-bulb Condition = '//TRIM(OutputChar)// &
                 ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                 //TRIM(CreateSysTimeIntervalString())
      ElectricChiller(ChillNum)%Base%MsgDataLast = Node(CondInletNode)%Temp
    ELSE
      ElectricChiller(ChillNum)%Base%PrintMessage = .FALSE.
    ENDIF
  ENDIF ! End of the Air Cooled/Evap Cooled Logic block

  CondInletTemp  = Node(CondInletNode)%Temp

  ! correct inlet temperature if using heat recovery
  IF (ElectricChiller(ChillNum)%HeatRecActive) THEN
    IF ((ElectricChillerReport(ChillNum)%QHeatRecovery + &
         ElectricChillerReport(ChillNum)%Base%QCond) > 0.d0) THEN
      AvgCondSinkTemp = (ElectricChillerReport(ChillNum)%QHeatRecovery &
                           * ElectricChillerReport(ChillNum)%HeatRecInletTemp &
                         + ElectricChillerReport(ChillNum)%Base%QCond &
                           * ElectricChillerReport(ChillNum)%Base%CondInletTemp) &
                        / ( ElectricChillerReport(ChillNum)%QHeatRecovery &
                            + ElectricChillerReport(ChillNum)%Base%QCond)
    ELSE
      AvgCondSinkTemp = CondInletTemp
    ENDIF
  ELSE
    AvgCondSinkTemp = CondInletTemp
  ENDIF

  !Calculate chiller performance from this set of performance equations.
  !  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))

  DeltaTemp= (AvgCondSinkTemp   -  TempCondInDesign) / TempRiseRat &
         - (TempEvapOut -  TempEvapOutDesign)

  ! model should have bounds on DeltaTemp and check them (also needs engineering ref content)
  !  from BLAST...RCAV=RCAVC(1)+RCAVC(2)*Z+RCAVC(3)*Z**2
  AvailNomCapRat =   CapacityRat(1)                   &
                   + CapacityRat(2) * DeltaTemp       &
                   + CapacityRat(3) * DeltaTemp ** 2.d0

  AvailChillerCap = ChillerNomCap*AvailNomCapRat

  ! from BLAST...G=ADJEC(1)+ADJEC(2)*RCAV+ADJEC(3)*RCAV**2.
  FullLoadPowerRat=   PowerRat(1)                         &
                    + PowerRat(2) * AvailNomCapRat      &
                    + PowerRat(3) * AvailNomCapRat ** 2.d0

  !  from BLAST...RCLOAD=AMAX1(MINCHFR(I,IPLCTR),AMIN1(CHLRLOAD(I)/CHLROCAP(I) &
  !         /RCAV,MAXCHFR(I,IPLCTR)))

  !Calculate the PLR. When there is Min PLR and the load is less than Min PLR then the Frac Full load Power
  !is calculated at Min PLR, while all other calculations are based on the actual PLR. So in that case once
  !FracFullLoadPower is calculated the PLR should be recalculated
  IF (AvailChillerCap > 0.0d0) THEN
    PartLoadRat = MAX(MinPartLoadRat, MIN(ABS(MyLoad)/AvailChillerCap,MaxPartLoadRat))
  ENDIF

 ! from BLAST...RPOWER=RPWRC(1)+RPWRC(2)*RCLOAD+RPWRC(3)*RCLOAD**2
  FracFullLoadPower = FullLoadFactor(1)                      &
                    + FullLoadFactor(2) * PartLoadRat      &
                    + FullLoadFactor(3) * PartLoadRat ** 2

  !If the PLR is less than Min PLR calculate the actual PLR for calculations. The power will then adjust for
  !the cycling.
  IF (AvailChillerCap > 0.0d0) THEN
    IF(ABS(MyLoad)/AvailChillerCap .LT. MinPartLoadRat) THEN
     OperPartLoadRat = ABS(MyLoad)/AvailChillerCap
    ELSE
     OperPartLoadRat = PartLoadRat
    END IF
  ELSE
    OperPartLoadRat = 0.0d0
  ENDIF

  Cp = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                         Node(EvapInletNode)%Temp,                      &
                         PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%FluidIndex, &
                         'CalcElectricChillerModel')

        ! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        ! condenser side outlet temperature.
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==0) THEN

       !ElectricChiller(ChillNum)%PossibleSubCooling = .FALSE.
       !PossibleSubCooling = .NOT. PlantLoop(PlantLoopNum)%TempSetPtCtrl
       IF(PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                    == CompSetPtBasedSchemeType)THEN
         ElectricChiller(ChillNum)%Base%PossibleSubCooling = .FALSE.
       ELSE
         ElectricChiller(ChillNum)%Base%PossibleSubCooling = .TRUE.
       ENDIF
       QEvaporator = AvailChillerCap * OperPartLoadRat
       IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
        FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
       ELSE
        FRAC = 1.0d0
       END IF
       Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap/RatedCOP * FRAC

       ! Either set the flow to the Constant value or caluclate the flow for the variable volume
       IF ( (ElectricChiller(ChillNum)%Base%FlowMode == ConstantFlow)  &
            .OR. (ElectricChiller(ChillNum)%Base%FlowMode == NotModulated)) THEN

          ! Start by assuming max (design) flow
          EvapMassFlowRate = EvapMassFlowRateMax
          ! Use SetComponentFlowRate to decide actual flow
          CALL SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%Base%CWLoopNum,     &
                              ElectricChiller(ChillNum)%Base%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%Base%CWBranchNum,   &
                              ElectricChiller(ChillNum)%Base%CWCompNum)
          ! Evaluate delta temp based on actual flow rate
          IF (EvapMassFlowRate /= 0.0D0) THEN
            EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
          ELSE
            EvapDeltaTemp = 0.0D0
          ENDIF
          ! Evaluate outlet temp based on delta
          EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp

       ELSE IF (ElectricChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) THEN

          ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
          SELECT CASE (PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetpoint)
            EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint
          CASE (DualSetpointDeadband)
            EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPointHi
          END SELECT

          IF (EvapDeltaTemp /= 0.0d0) THEN

            ! Calculate desired flow to request based on load
            EvapMassFlowRate = ABS(QEvaporator/Cp/EvapDeltaTemp)
            !Check to see if the Maximum is exceeded, if so set to maximum
            IF((EvapMassFlowRate - EvapMassFlowRateMax) .GT. MassFlowTolerance) &
                     ElectricChiller(ChillNum)%Base%PossibleSubCooling = .TRUE.
            EvapMassFlowRate = MIN(EvapMassFlowRateMax, EvapMassFlowRate)
            ! Use SetComponentFlowRate to decide actual flow
            Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%Base%CWLoopNum,     &
                              ElectricChiller(ChillNum)%Base%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%Base%CWBranchNum,   &
                              ElectricChiller(ChillNum)%Base%CWCompNum)
            SELECT CASE (PlantLoop(ElectricChiller(ChillNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
            CASE (SingleSetpoint)
              EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
            CASE (DualSetpointDeadband)
              EvapOutletTemp = Node(EvapOutletNode)%TempSetPointHi
            END SELECT

          ELSE

            ! Try to request zero flow
            EvapMassFlowRate=0.0d0
            ! Use SetComponentFlowRate to decide actual flow
            Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%Base%CWLoopNum,     &
                              ElectricChiller(ChillNum)%Base%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%Base%CWBranchNum,   &
                              ElectricChiller(ChillNum)%Base%CWCompNum)
            ! No deltaT since component is not running
            EvapOutletTemp = Node(EvapInletNode)%Temp

          END IF

       End If  !End of Constant Variable Flow If Block

  ELSE  ! If FlowLock is True

    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%Base%CWLoopNum,     &
                              ElectricChiller(ChillNum)%Base%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%Base%CWBranchNum,   &
                              ElectricChiller(ChillNum)%Base%CWCompNum)

!       Some other component set the flow to 0. No reason to continue with calculations.
    IF(EvapMassFlowRate == 0.0d0)THEN
      MyLoad = 0.0d0
      IF (ElectricChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
        CALL CalcBasinHeaterPower(ElectricChiller(ChillNum)%Base%BasinHeaterPowerFTempDiff,&
                            ElectricChiller(ChillNum)%Base%BasinHeaterSchedulePtr,&
                            ElectricChiller(ChillNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
      ENDIF
      ElectricChiller(ChillNum)%Base%PrintMessage = .FALSE.
      RETURN
    END IF
    !Flow resolver might have given less flow or control scheme have provided more load, which may
    !result in subcooling.
    IF(ElectricChiller(ChillNum)%Base%PossibleSubCooling) THEN
      QEvaporator = ABS(MyLoad)
      EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
    ELSE  !No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

      SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetpoint)
        IF ((ElectricChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated ) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
          TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
        ELSE
          TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
        ENDIF
      CASE (DualSetpointDeadband)
        IF ((ElectricChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPointHi /= SensedNodeFlagValue) ) THEN
          TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPointHi
        ELSE
          TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPointHi
        ENDIF
      END SELECT
      EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
      QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
      EvapOutletTemp = TempEvapOutSetpoint
    END IF
    !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
    IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
      IF((Node(EvapInletNode)%Temp - TempLowLimitEout) .GT. DeltaTemptol) THEN
        EvapOutletTemp = TempLowLimitEout
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
      IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTemptol) THEN
        EvapOutletTemp = Node(EvapOutletNode)%TempMin
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF

    ! If load exceeds the distributed load set to the distributed load
    IF(QEvaporator > ABS(MyLoad)) Then
      If(EvapMassFlowRate > MassFlowTolerance) THEN
        QEvaporator = ABS(MyLoad)
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      ELSE
        QEvaporator = 0.0d0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End IF

    ! Checks QEvaporator on the basis of the machine limits.
    If(QEvaporator > (AvailChillerCap * MaxPartLoadRat))Then
      If(EvapMassFlowRate > MassFlowTolerance) THEN
        QEvaporator = AvailChillerCap * OperPartLoadRat
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0d0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End If

    IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
      FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
    ELSE
      FRAC = 1.0d0
    END IF

    ! set the module level variable used for reporting FRAC
    ChillerCyclingRatio = FRAC

    ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap /RatedCOP * FRAC

    IF(EvapMassFlowRate == 0.0d0) THEN
     QEvaporator = 0.0d0
     EvapOutletTemp = Node(EvapInletNode)%Temp
     Power = 0.0d0
     ElectricChiller(ChillNum)%Base%PrintMessage = .FALSE.
    END IF
    IF(QEvaporator == 0.0d0 .AND. ElectricChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(ElectricChiller(ChillNum)%Base%BasinHeaterPowerFTempDiff,&
                                  ElectricChiller(ChillNum)%Base%BasinHeaterSchedulePtr,&
                                  ElectricChiller(ChillNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
    END IF
  END IF  !This is the end of the FlowLock Block

  !QCondenser is calculated the same for each type, but the power consumption should be different
  !  depending on the performance coefficients used for the chiller model.
  QCondenser = Power + QEvaporator

  IF (ElectricChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    IF (CondMassFlowRate > MassFlowTolerance) THEN
      ! If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
      If(ElectricChiller(ChillNum)%HeatRecActive) Call CalcElectricChillerHeatRecovery(ChillNum,QCondenser, &
                                                               CondMassFlowRate,CondInletTemp,QHeatRecovered)
      CpCond = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                         CondInletTemp,                      &
                                         PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidIndex, &
                                         'CalcElectricChillerModel')
       CondOutletTemp = QCondenser/CondMassFlowRate/CpCond + CondInletTemp
    ELSE
      CALL ShowSevereError('CalcElectricChillerModel: Condenser flow = 0, for ElectricChiller='//  &
                            TRIM(ElectricChiller(ChillNum)%Base%Name))
      CALL ShowContinueErrorTimeStamp(' ')

    END IF
  ELSE !Air Cooled or Evap Cooled

    IF(QCondenser > 0.0d0) THEN
      CondMassFlowRate = ElectricChiller(ChillNum)%Base%CondMassFlowRateMax * OperPartLoadRat
    ELSE
      CondMassFlowRate = 0.0d0
    END IF

    ! If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
    If(ElectricChiller(ChillNum)%HeatRecActive) Call CalcElectricChillerHeatRecovery(ChillNum,QCondenser, &
                                                 CondMassFlowRate,CondInletTemp,QHeatRecovered)
    IF(CondMassFlowRate .GT. 0.0d0)THEN
      CpCond = PsyCpAirFnWTdb(Node(CondInletNode)%HumRat,CondInletTemp,'CalcElectricChillerModel')
      CondOutletTemp = CondInletTemp + QCondenser/CondMassFlowRate/CpCond
    ELSE
      CondOutletTemp = CondInletTemp
    END IF
  END IF


    !Calculate Energy
  CondenserEnergy  = QCondenser*TimeStepSys*SecInHour
  Energy           = Power*TimeStepSys*SecInHour
  EvaporatorEnergy = QEvaporator*TimeStepSys*SecInHour

 !check for problems BG 9/12/06 (deal with observed negative energy results)
  IF (Energy < 0.0d0) then  ! there is a serious problem

    IF (ElectricChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
     ! first check for run away condenser loop temps (only reason yet to be observed for this?)
      IF (CondInletTemp > 70.0d0 )  then
        CALL ShowSevereError('CalcElectricChillerModel: Condenser loop inlet temperatures over 70.0 C for ElectricChiller='//  &
                            TRIM(ElectricChiller(ChillNum)%Base%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Condenser loop water temperatures are too high at'//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL ShowContinueError('Check input for condenser plant loop, especially cooling tower')
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )

        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    IF(.NOT.WarmupFlag)THEN
      If (AvailNomCapRat < 0.0d0 ) then     ! apparently the real reason energy goes negative
        CALL ShowSevereError('CalcElectricChillerModel: Capacity ratio below zero for ElectricChiller='//  &
                              TRIM(ElectricChiller(ChillNum)%Base%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Check input for Capacity Ratio Curve')
        CALL showContinueError('Condenser inlet temperature: '//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )
        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    ! If makes it here, set limits, chiller can't have negative energy/power
    ! proceeding silently for now but may want to throw error here
    Power = 0.0d0
    Energy = 0.0d0
  ENDIF
  RETURN
END SUBROUTINE CalcElectricChillerModel


SUBROUTINE CalcEngineDrivenChillerModel(ChillerNum,MyLoad,Runflag,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a vapor compression chiller using the EngineDriven model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:

          ! REFERENCES:
          ! 1. BLAST Users Manual
          ! 2. CHILLER User Manual

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, SecInHour, CurrentTime
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys, SysTimeElapsed
  USE CurveManager,    ONLY : CurveValue
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_EngineDriven, CompSetPtBasedSchemeType, &
                              CriteriaType_MassFlowRate, SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY : ControlType_SeriesActive, MassFlowTolerance
  USE DataEnvironment, ONLY : EnvironmentName, CurMnDy
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: ChillerNum      ! chiller number
  REAL(r64)              :: MyLoad          ! operating load
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when chiller operating
 ! INTEGER, INTENT(IN)    :: FlowLock        ! TRUE when flow resolver has calculated branch flow
  INTEGER, INTENT(IN) :: EquipFlowCtrl  ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), PARAMETER   :: ExhaustCP = 1.047d0    !Exhaust Gas Specific Heat (J/kg-K)
  REAL(r64), PARAMETER   :: ReferenceTemp = 25.0d0 !Reference temperature by which lower heating
                                                   ! value is reported.  This should be subtracted
                                                   ! off of when calculated exhaust energies.
  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), DIMENSION(3)     :: CapacityRat         ! intermediate result:  capacity ratio
  REAL(r64), DIMENSION(3)     :: PowerRat            ! intermediate result:  power ratio
  REAL(r64), DIMENSION(3)     :: FullLoadFactor      ! intermediate result:  full load factor
  REAL(r64)              :: MinPartLoadRat      ! min allowed operating frac full load
  REAL(r64)              :: MaxPartLoadRat      ! max allowed operating frac full load
  REAL(r64)              :: TempCondIn          ! C - (EngineDriven ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempCondInDesign    ! C - (EngineDriven ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempRiseRat         ! intermediate result:  temperature rise ratio
  REAL(r64)              :: EvapInletTemp       ! C - evaporator inlet temperature, water side
  REAL(r64)              :: CondInletTemp       ! C - condenser inlet temperature, water side
  REAL(r64)              :: TempEvapOut         ! C - evaporator outlet temperature, water side
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  REAL(r64)              :: TempEvapOutDesign   ! design evaporator outlet temperature, water side
  REAL(r64)              :: ChillerNomCap       ! chiller nominal capacity
  REAL(r64)              :: AvailChillerCap     ! chiller available capacity
  REAL(r64)              :: COP                 ! coefficient of performance
  REAL(r64)              :: FracFullLoadPower   ! fraction of full load power
  REAL(r64)              :: EvapDeltaTemp       ! C - evaporator temperature difference, water side
  REAL(r64)              :: DeltaTemp             ! C - intermediate result: condenser/evaporator temp diff
  REAL(r64)              :: AvailNomCapRat        ! intermediate result: available nominal capacity ratio
  REAL(r64)              :: FullLoadPowerRat      ! intermediate result: full load power ratio
  REAL(r64)              :: PartLoadRat           ! part load ratio for efficiency
  REAL(r64)              :: OperPartLoadRat     ! Actual operating PLR
  INTEGER                :: EvapInletNode       ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode      ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode       ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode      ! condenser outlet node number, water side
  REAL(r64)              :: EvapMassFlowRateMax ! Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
  REAL(r64)              :: TempLowLimitEout    ! C - Evaporator low temp. limit cut off
  REAL(r64)              :: FRAC
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum
  REAL(r64),SAVE  :: TimeStepSysLast=0.0d0     ! last system time step (used to check for downshifting)
  REAL(r64)       :: CurrentEndTime          ! end time of time step for current simulation time step
  REAL(r64),SAVE  :: CurrentEndTimeLast=0.0d0  ! end time of time step for last simulation time step
  CHARACTER(len=6):: OutputChar = ' '        ! character string for warning messages
  REAL(r64)       :: Cp                      ! local for fluid specif heat, for evaporator
  REAL(r64)       :: CpCond                  ! local for fluid specif heat, for condenser

! Special variables for EngineDriven Chiller
  REAL(r64)    :: MaxExhaustperPowerOutput !curve fit parameter
  REAL(r64)    :: ClngLoadFuelRat      !(RELDC) Ratio of Shaft Power to Fuel Energy Input
  REAL(r64)    :: RecJacHeattoFuelRat  !(RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
  REAL(r64)    :: RecLubeHeattoFuelRat !(RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
  REAL(r64)    :: TotExhausttoFuelRat  !(REXDC) Total Exhaust Energy Input to Fuel Energy Input
  REAL(r64)    :: TotalExhaustEnergy
  REAL(r64)    :: ExhaustTemp          !(TEX) Exhaust Gas Temp
  REAL(r64)    :: ExhaustGasFlow       !exhaust gas mass flow rate
  REAL(r64)    :: DesignMinExitGasTemp
  REAL(r64)    :: UA                   !(UACDC) exhaust gas Heat Exchanger UA
  REAL(r64)    :: HeatRecCp            !Specific Heat of the Heat Recovery Fluid (J/kg-K)
  REAL(r64)    :: EngineDrivenFuelEnergy
  REAL(r64)    :: HeatRecRatio              !When Max Temp is reached the amount of recovered heat has to be reduced.
!  LOGICAL,SAVE :: PossibleSubCooling=.FALSE.

      !set module level inlet and outlet nodes
  EvapMassFlowRate           = 0.0d0
  CondMassFlowRate           = 0.0d0
  Power                      = 0.0d0
  QCondenser                 = 0.0d0
  QEvaporator                = 0.0d0
  Energy                     = 0.0d0
  CondenserEnergy            = 0.0d0
  EvaporatorEnergy           = 0.0d0
  HeatRecCp                  = 0.0d0
  HeatRecMdotActual          = 0.0d0
  QTotalHeatRecovered        = 0.0d0
  QJacketRecovered           = 0.0d0
  QLubeOilRecovered          = 0.0d0
  QExhaustRecovered          = 0.0d0
  EngineDrivenFuelEnergy     = 0.0d0
  FuelEnergyUseRate          = 0.0d0
  TotalHeatEnergyRec         = 0.0d0
  JacketEnergyRec            = 0.0d0
  LubeOilEnergyRec           = 0.0d0
  ExhaustEnergyRec           = 0.0d0
  FuelEnergy                 = 0.0d0
  FuelMdot                   = 0.0d0
  ExhaustStackTemp           = 0.0d0
  FRAC                       = 1.0d0

  IF (EngineDrivenChiller(ChillerNum)%HeatRecActive) THEN
     HeatRecInletTemp           = Node(EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum)%Temp
     HeatRecOutletTemp          = Node(EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum)%Temp
     HeatRecMdotDesign          = EngineDrivenChiller(ChillerNum)%DesignHeatRecMassFlowRate
  ENDIF

  EvapInletNode  = EngineDrivenChiller(ChillerNum)%Base%EvapInletNodeNum
  EvapOutletNode = EngineDrivenChiller(ChillerNum)%Base%EvapOutletNodeNum
  CondInletNode  = EngineDrivenChiller(ChillerNum)%Base%CondInletNodeNum
  CondOutletNode = EngineDrivenChiller(ChillerNum)%Base%CondOutletNodeNum
  LoopNum        = EngineDrivenChiller(ChillerNum)%Base%CWLoopNum
  LoopSideNum    = EngineDrivenChiller(ChillerNum)%Base%CWLoopSideNum
  EvapInletTemp  = Node(EvapInletNode)%Temp

!   calculate end time of current time step
  CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
    IF(EngineDrivenChiller(ChillerNum)%Base%PrintMessage)THEN
        EngineDrivenChiller(ChillerNum)%Base%MsgErrorCount = &
                         EngineDrivenChiller(ChillerNum)%Base%MsgErrorCount + 1
!     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
      IF (EngineDrivenChiller(ChillerNum)%Base%MsgErrorCount < 2) THEN
         CALL ShowWarningError(TRIM(EngineDrivenChiller(ChillerNum)%Base%MsgBuffer1)//'.')
         CALL ShowContinueError(TRIM(EngineDrivenChiller(ChillerNum)%Base%MsgBuffer2))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(EngineDrivenChiller(ChillerNum)%Base%MsgBuffer1)//' error continues.', &
           EngineDrivenChiller(ChillerNum)%Base%ErrCount1,ReportMaxOf=EngineDrivenChiller(ChillerNum)%Base%MsgDataLast,  &
           ReportMinOf=EngineDrivenChiller(ChillerNum)%Base%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
      END IF
    END IF
  END IF

! save last system time step and last end time of current time step (used to determine if warning is valid)
  TimeStepSysLast    = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime

   !If Chiller load is 0 or chiller is not running then leave the subroutine.
  IF(MyLoad >= 0.d0 .OR. .NOT. RunFlag) THEN
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    ELSE
      EvapMassFlowRate           = 0.d0

      CALL SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          EngineDrivenChiller(ChillerNum)%Base%CWLoopNum,     &
                          EngineDrivenChiller(ChillerNum)%Base%CWLoopSideNum, &
                          EngineDrivenChiller(ChillerNum)%Base%CWBranchNum,   &
                          EngineDrivenChiller(ChillerNum)%Base%CWCompNum)
    ENDIF

    IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
      IF ( PlantLoop(EngineDrivenChiller(ChillerNum)%Base%CDLoopNum)% &
            LoopSide(EngineDrivenChiller(ChillerNum)%Base%CDLoopSideNum)% &
              Branch(EngineDrivenChiller(ChillerNum)%Base%CDBranchNum)%  &
                Comp(EngineDrivenChiller(ChillerNum)%Base%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate           = Node(CondInletNode)%MassFlowrate
      ELSE
        CondMassFlowRate           = 0.d0
        CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                                EngineDrivenChiller(ChillerNum)%Base%CDLoopNum, &
                                EngineDrivenChiller(ChillerNum)%Base%CDLoopSideNum, &
                                EngineDrivenChiller(ChillerNum)%Base%CDBranchNum, &
                                EngineDrivenChiller(ChillerNum)%Base%CDCompNum)
      ENDIF
    ENDIF

    IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(EngineDrivenChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff,&
                                EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSchedulePtr,&
                                EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    EngineDrivenChiller(ChillerNum)%Base%PrintMessage = .FALSE.
    RETURN
  END IF

  IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == AirCooled) THEN !Condenser inlet temp = outdoor temp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirDryBulb
!  Warn user if entering condenser temperature falls below 0C
      IF(Node(CondInletNode)%Temp .LT. 0.0d0 .and. .not. WarmupFlag) THEN
        EngineDrivenChiller(ChillerNum)%Base%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        EngineDrivenChiller(ChillerNum)%Base%MsgBuffer1 = 'CalcEngineDrivenChillerModel - Chiller:EngineDriven "' &
                             //TRIM(EngineDrivenChiller(ChillerNum)%Base%Name)// &
                             '" - Air Cooled Condenser Inlet Temperature below 0C'
        EngineDrivenChiller(ChillerNum)%Base%MsgBuffer2 = '... Outdoor Dry-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        EngineDrivenChiller(ChillerNum)%Base%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        EngineDrivenChiller(ChillerNum)%Base%PrintMessage = .FALSE.
      ENDIF
  Else IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN !Condenser inlet temp = (outdoor wet bulb)
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirWetBulb
!  Warn user if evap condenser wet bulb temperature falls below 10C
      IF(Node(CondInletNode)%Temp .LT. 10.0d0 .and. .not. WarmupFlag) THEN
        EngineDrivenChiller(ChillerNum)%Base%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        EngineDrivenChiller(ChillerNum)%Base%MsgBuffer1 = 'CalcEngineDrivenChillerModel - Chiller:EngineDriven "' &
                             //TRIM(EngineDrivenChiller(ChillerNum)%Base%Name)// &
                             '" - Evap Cooled Condenser Inlet Temperature below 10C'
        EngineDrivenChiller(ChillerNum)%Base%MsgBuffer2 = '... Outdoor Wet-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        EngineDrivenChiller(ChillerNum)%Base%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        EngineDrivenChiller(ChillerNum)%Base%PrintMessage = .FALSE.
      ENDIF
  ENDIF ! End of the Air Cooled/Evap Cooled Logic block

  ! If not air or evap cooled then set to the condenser node that is attached to a cooling tower
  CondInletTemp  = Node(CondInletNode)%Temp

        !Set mass flow rates
  IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
    CondMassFlowRate = EngineDrivenChiller(ChillerNum)%Base%CondMassFlowRateMax
    CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              EngineDrivenChiller(ChillerNum)%Base%CDLoopNum, &
                              EngineDrivenChiller(ChillerNum)%Base%CDLoopSideNum, &
                              EngineDrivenChiller(ChillerNum)%Base%CDBranchNum, &
                              EngineDrivenChiller(ChillerNum)%Base%CDCompNum)
    CALL PullCompInterconnectTrigger(EngineDrivenChiller(ChillerNum)%Base%CWLoopNum, &
                                     EngineDrivenChiller(ChillerNum)%Base%CWLoopSideNum, &
                                     EngineDrivenChiller(ChillerNum)%Base%CWBranchNum, &
                                     EngineDrivenChiller(ChillerNum)%Base%CWCompNum, &
                                     EngineDrivenChiller(ChillerNum)%Base%CondMassFlowIndex,              &
                                     EngineDrivenChiller(ChillerNum)%Base%CDLoopNum, &
                                     EngineDrivenChiller(ChillerNum)%Base%CDLoopSideNum,   &
                                     CriteriaType_MassFlowRate, &
                                     CondMassFlowRate)
    IF (CondMassFlowRate < MassFlowTolerance) RETURN

  END IF

  !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  CapacityRat        = EngineDrivenChiller(ChillerNum)%CapRatCoef
  PowerRat           = EngineDrivenChiller(ChillerNum)%PowerRatCoef
  FullLoadFactor     = EngineDrivenChiller(ChillerNum)%FullLoadCoef
  MinPartLoadRat     = EngineDrivenChiller(ChillerNum)%MinPartLoadRat
  MaxPartLoadRat     = EngineDrivenChiller(ChillerNum)%MaxPartLoadRat
  TempCondInDesign   = EngineDrivenChiller(ChillerNum)%TempDesCondIn
  TempRiseRat        = EngineDrivenChiller(ChillerNum)%TempRiseCoef
  TempEvapOutDesign  = EngineDrivenChiller(ChillerNum)%TempDesEvapOut
  ChillerNomCap      = EngineDrivenChiller(ChillerNum)%Base%NomCap
  COP                = EngineDrivenChiller(ChillerNum)%Base%COP
  TempCondIn         = Node(EngineDrivenChiller(ChillerNum)%Base%CondInletNodeNum)%Temp
  TempEvapOut        = Node(EngineDrivenChiller(ChillerNum)%Base%EvapOutletNodeNum)%Temp
  TempLowLimitEout   = EngineDrivenChiller(ChillerNum)%TempLowLimitEvapOut
  MaxExhaustperPowerOutput  = EngineDrivenChiller(ChillerNum)%MaxExhaustperPowerOutput
  LoopNum            = EngineDrivenChiller(ChillerNum)%Base%CWLoopNum
  LoopSideNum        = EngineDrivenChiller(ChillerNum)%Base%CWLoopSideNum
  EvapMassFlowRateMax    = EngineDrivenChiller(ChillerNum)%Base%EvapMassFlowRateMax

!*********************************


  !Calculate chiller performance from this set of performance equations.
  !  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))
    DeltaTemp= (TempCondIn   -  TempCondInDesign) / TempRiseRat &
           - (TempEvapOut -  TempEvapOutDesign)

  !  from BLAST...RCAV=RCAVC(1)+RCAVC(2)*Z+RCAVC(3)*Z**2
    AvailNomCapRat =   CapacityRat(1)                   &
                     + CapacityRat(2) * DeltaTemp       &
                     + CapacityRat(3) * DeltaTemp ** 2

    AvailChillerCap = ChillerNomCap*AvailNomCapRat

   ! from BLAST...G=ADJEC(1)+ADJEC(2)*RCAV+ADJEC(3)*RCAV**2.
    FullLoadPowerRat=   PowerRat(1)                         &
                      + PowerRat(2) * AvailNomCapRat      &
                      + PowerRat(3) * AvailNomCapRat ** 2

  !  from BLAST...RCLOAD=AMAX1(MINCHFR(I,IPLCTR),AMIN1(CHLRLOAD(I)/CHLROCAP(I) &
  !         /RCAV,MAXCHFR(I,IPLCTR)))
 IF (AvailChillerCap > 0.0d0) THEN
   PartLoadRat = MAX(MinPartLoadRat, MIN(ABS(MyLoad)/AvailChillerCap,MaxPartLoadRat))
 ENDIF
   ! from BLAST...RPOWER=RPWRC(1)+RPWRC(2)*RCLOAD+RPWRC(3)*RCLOAD**2
    FracFullLoadPower = FullLoadFactor(1)                      &
                      + FullLoadFactor(2) * PartLoadRat      &
                      + FullLoadFactor(3) * PartLoadRat ** 2

 IF (AvailChillerCap > 0.0d0) THEN
   IF(ABS(MyLoad)/AvailChillerCap .LT. MinPartLoadRat) THEN
     OperPartLoadRat = ABS(MyLoad)/AvailChillerCap
   ELSE
     OperPartLoadRat = PartLoadRat
   END IF
 ELSE
   OperPartLoadRat = 0.0d0
 ENDIF
!*********************************
  Cp = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillerNum)%Base%CWLoopNum)%FluidName,  &
                         Node(EvapInletNode)%Temp,                      &
                         PlantLoop(EngineDrivenChiller(ChillerNum)%Base%CWLoopNum)%FluidIndex, &
                         'CalcEngineDrivenChillerModel')

  ! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
  ! condenser side outlet temperature.
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==0) THEN
    EngineDrivenChiller(ChillerNum)%Base%PossibleSubCooling = .FALSE.
    QEvaporator = AvailChillerCap * OperPartLoadRat
    IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
     FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
    ELSE
     FRAC = 1.0d0
    END IF
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap/COP * FRAC

    ! Either set the flow to the Constant value or caluclate the flow for the variable volume
    If ((EngineDrivenChiller(ChillerNum)%Base%FlowMode == ConstantFlow)  &
        .OR. (EngineDrivenChiller(ChillerNum)%Base%FlowMode == NotModulated)) THEN
      ! Start by assuming max (design) flow
      EvapMassFlowRate = EvapMassFlowRateMax
      ! Use SetComponentFlowRate to decide actual flow
      Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          EngineDrivenChiller(ChillerNum)%Base%CWLoopNum,     &
                          EngineDrivenChiller(ChillerNum)%Base%CWLoopSideNum, &
                          EngineDrivenChiller(ChillerNum)%Base%CWBranchNum,   &
                          EngineDrivenChiller(ChillerNum)%Base%CWCompNum)
      ! Evaluate delta temp based on actual flow rate
      IF (EvapMassFlowRate /= 0.0D0) THEN
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      ELSE
        EvapDeltaTemp = 0.0D0
      ENDIF
      ! Evaluate outlet temp based on delta
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp

    ELSE IF (EngineDrivenChiller(ChillerNum)%Base%FlowMode == LeavingSetpointModulated ) THEN

      ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
      SELECT CASE (PlantLoop(EngineDrivenChiller(ChillerNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetpoint)
        EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint
      CASE (DualSetpointDeadband)
        EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPointHi
      END SELECT

      IF (EvapDeltaTemp /= 0) THEN
        EvapMassFlowRate = ABS(QEvaporator/Cp/EvapDeltaTemp)
        IF((EvapMassFlowRate - EvapMassFlowRateMax) .GT. MassFlowTolerance) &
             EngineDrivenChiller(ChillerNum)%Base%PossibleSubCooling = .TRUE.
        !Check to see if the Maximum is exceeded, if so set to maximum
        EvapMassFlowRate = MIN(EvapMassFlowRateMax, EvapMassFlowRate)
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          EngineDrivenChiller(ChillerNum)%Base%CWLoopNum,     &
                          EngineDrivenChiller(ChillerNum)%Base%CWLoopSideNum, &
                          EngineDrivenChiller(ChillerNum)%Base%CWBranchNum,   &
                          EngineDrivenChiller(ChillerNum)%Base%CWCompNum)
        SELECT CASE (PlantLoop(EngineDrivenChiller(ChillerNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
        CASE (SingleSetpoint)
          EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
        CASE (DualSetpointDeadband)
          EvapOutletTemp = Node(EvapOutletNode)%TempSetPointHi
        END SELECT
      ELSE
        ! Try to request zero flow
        EvapMassFlowRate=0.0d0
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          EngineDrivenChiller(ChillerNum)%Base%CWLoopNum,     &
                          EngineDrivenChiller(ChillerNum)%Base%CWLoopSideNum, &
                          EngineDrivenChiller(ChillerNum)%Base%CWBranchNum,   &
                          EngineDrivenChiller(ChillerNum)%Base%CWCompNum)
        ! No deltaT since component is not running
        EvapOutletTemp = Node(EvapInletNode)%Temp

      END IF
    End If  !End of Constant Variable Flow If Block
  ELSE  ! If FlowLock is True

    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              EngineDrivenChiller(ChillerNum)%Base%CWLoopNum,     &
                              EngineDrivenChiller(ChillerNum)%Base%CWLoopSideNum, &
                              EngineDrivenChiller(ChillerNum)%Base%CWBranchNum,   &
                              EngineDrivenChiller(ChillerNum)%Base%CWCompNum)
    ! Some other component set the flow to 0. No reason to continue with calculations.
    IF(EvapMassFlowRate == 0.0d0)THEN
      MyLoad = 0.0d0
      IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        CALL CalcBasinHeaterPower(EngineDrivenChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff,&
                            EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSchedulePtr,&
                            EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
      ENDIF
      EngineDrivenChiller(ChillerNum)%Base%PrintMessage = .FALSE.
      RETURN
    END IF

    IF(EngineDrivenChiller(ChillerNum)%Base%PossibleSubCooling) THEN
      QEvaporator = ABS(MyLoad)
      EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
        EvapOutletTemp = Node(EvapOutletNode)%TempMin
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
      END IF
    ELSE !No subcooling in this case.No recalculation required.Still need to check chiller low temp limit

      SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetpoint)
        IF ((EngineDrivenChiller(ChillerNum)%Base%FlowMode == LeavingSetpointModulated) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(EngineDrivenChiller(ChillerNum)%Base%CWBranchNum) &
              %Comp(EngineDrivenChiller(ChillerNum)%Base%CWCompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
          TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
        ELSE
          TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
        ENDIF
      CASE (DualSetpointDeadband)
        IF ((EngineDrivenChiller(ChillerNum)%Base%FlowMode == LeavingSetpointModulated) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(EngineDrivenChiller(ChillerNum)%Base%CWBranchNum) &
              %Comp(EngineDrivenChiller(ChillerNum)%Base%CWCompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPointHi /= SensedNodeFlagValue) ) THEN
          TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPointHi
        ELSE
          TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPointHi
        ENDIF
      END SELECT
      EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
      QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
      EvapOutletTemp = TempEvapOutSetpoint
    END IF
    !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
    IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
      IF((Node(EvapInletNode)%Temp - TempLowLimitEout) .GT. DeltaTemptol) THEN
        EvapOutletTemp = TempLowLimitEout
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
      IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTemptol) THEN
        EvapOutletTemp = Node(EvapOutletNode)%TempMin
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    ! If load exceeds the distributed load set to the distributed load
    If(QEvaporator > ABS(MyLoad)) Then
      If(EvapMassFlowRate > MassFlowTolerance) THEN
        QEvaporator = ABS(MyLoad)
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0d0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End IF

    ! Checks QEvaporator on the basis of the machine limits.
    If(QEvaporator > (AvailChillerCap * MaxPartLoadRat))Then
      If(EvapMassFlowRate > MassFlowTolerance) THEN
        QEvaporator = AvailChillerCap * OperPartLoadRat
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0d0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End If

   IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
     FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
   ELSE
     FRAC = 1.0d0
   END IF

  ! set the module level variable used for reporting FRAC
   ChillerCyclingRatio = FRAC

  ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap /COP * FRAC

    IF(EvapMassFlowRate == 0.0d0) THEN
      QEvaporator = 0.0d0
      EvapOutletTemp = Node(EvapInletNode)%Temp
      Power = 0.0d0
      EngineDrivenChiller(ChillerNum)%Base%PrintMessage = .FALSE.
    END IF
    IF(QEvaporator == 0.0d0 .AND. EngineDrivenChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(EngineDrivenChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff,&
                                EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSchedulePtr,&
                                EngineDrivenChiller(ChillerNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
    END IF
  END IF  !This is the end of the FlowLock Block


!Now determine Cooling
    !QCondenser is calculated the same for each type, but the power consumption should be different
    !  depending on the performance coefficients used for the chiller model.
  QCondenser = Power + QEvaporator

  IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN

    IF (CondMassFlowRate > MassFlowTolerance) THEN
      CpCond = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillerNum)%Base%CDLoopNum)%FluidName,  &
                                         CondInletTemp,                      &
                                         PlantLoop(EngineDrivenChiller(ChillerNum)%Base%CDLoopNum)%FluidIndex, &
                                         'CalcEngineDrivenChillerModel')
      CondOutletTemp = QCondenser/CondMassFlowRate/CpCond + CondInletTemp
    ELSE
      CALL ShowSevereError('CalcEngineDrivenChillerModel: Condenser flow = 0, for EngineDrivenChiller='//  &
                           TRIM(EngineDrivenChiller(ChillerNum)%Base%Name))
      CALL ShowContinueErrorTimeStamp(' ')
    END IF

  ELSE !Air Cooled or Evap Cooled

    !don't care about outlet temp for Air-Cooled or Evap Cooled
    CondOutletTemp = CondInletTemp
  END IF

! EngineDriven Portion of the Engine Driven Chiller:

!DETERMINE FUEL CONSUMED AND AVAILABLE WASTE HEAT

!Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
!energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/cooling load (J/s).
  IF (PartLoadRat == 0)THEN
    EngineDrivenFuelEnergy = 0.0d0
  ELSE
    PartLoadRat = MAX(MinPartLoadRat,PartLoadRat)
    ClngLoadFuelRat = CurveValue(EngineDrivenChiller(ChillerNum)%ClngLoadtoFuelCurve, PartLoadRat)
    EngineDrivenFuelEnergy = QEvaporator / ClngLoadFuelRat
  END IF
!Use Curve fit to determine energy recovered in the water jacket.  This curve calculates the water jacket energy recovered (J/s) by
!multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the water jacket at that
!particular part load.

  RecJacHeattoFuelRat = CurveValue(EngineDrivenChiller(ChillerNum)%RecJacHeattoFuelCurve, PartLoadRat)
  QJacketRecovered = EngineDrivenFuelEnergy * RecJacHeattoFuelRat

!Use Curve fit to determine Heat Recovered Lubricant Energy.  This curve calculates the lube energy recovered (J/s) by
!multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the lube oil at that
!particular part load.
  RecLubeHeattoFuelRat = CurveValue(EngineDrivenChiller(ChillerNum)%RecLubeHeattoFuelCurve, PartLoadRat)
  QLubeOilRecovered = EngineDrivenFuelEnergy * RecLubeHeattoFuelRat

!Use Curve fit to determine Heat Recovered from the exhaust.  This curve calculates the  energy recovered (J/s) by
!multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the exhaust at that
!particular part load.
  TotExhausttoFuelRat = CurveValue(EngineDrivenChiller(ChillerNum)%TotExhausttoFuelCurve, PartLoadRat)
  TotalExhaustEnergy = EngineDrivenFuelEnergy * TotExhausttoFuelRat


!Use Curve fit to determine Exhaust Temperature in C.  The temperature is simply a curve fit
!of the exhaust temperature in C to the part load ratio.
  IF (PartLoadRat /= 0)THEN
    ExhaustTemp = CurveValue(EngineDrivenChiller(ChillerNum)%ExhaustTempCurve, PartLoadRat)
    ExhaustGasFlow = TotalExhaustEnergy / (ExhaustCP*(ExhaustTemp-ReferenceTemp))


!Use Curve fit to determine stack temp after heat recovery
    UA = EngineDrivenChiller(ChillerNum)%UACoef(1) * ChillerNomCap **  &
                   EngineDrivenChiller(ChillerNum)%UACoef(2)

    DesignMinExitGasTemp = EngineDrivenChiller(ChillerNum)%DesignMinExitGasTemp
    ExhaustStackTemp = DesignMinExitGasTemp + (ExhaustTemp - DesignMinExitGasTemp) / &
                         EXP(UA/(MAX(ExhaustGasFlow, MaxExhaustperPowerOutput * ChillerNomCap) * ExhaustCP))

    QExhaustRecovered = MAX(ExhaustGasFlow*ExhaustCP*(ExhaustTemp-ExhaustStackTemp),0.0d0)
  ELSE
    QExhaustRecovered = 0.0d0
  END IF


  QTotalHeatRecovered = QExhaustRecovered + QLubeOilRecovered + QJacketRecovered

  !Update Heat Recovery temperatures
  IF (EngineDrivenChiller(ChillerNum)%HeatRecActive) THEN
    CALL CalcEngineChillerHeatRec(ChillerNum,QTotalHeatRecovered,HeatRecRatio)
    QExhaustRecovered = QExhaustRecovered*HeatRecRatio
    QLubeOilRecovered = QLubeOilRecovered*HeatRecRatio
    QJacketRecovered  = QJacketRecovered*HeatRecRatio

  ENDIF

      !Calculate Energy
  CondenserEnergy  = QCondenser*TimeStepSys*SecInHour
  Energy           = Power*TimeStepSys*SecInHour
  EvaporatorEnergy = QEvaporator*TimeStepSys*SecInHour
  FuelEnergyUseRate = EngineDrivenFuelEnergy
  FuelEnergy       = FuelEnergyUseRate*TimeStepSys*SecInHour
  JacketEnergyRec      = QJacketRecovered*TimeStepSys*SecInHour
  LubeOilEnergyRec     = QLubeOilRecovered*TimeStepSys*SecInHour
  ExhaustEnergyRec     = QExhaustRecovered*TimeStepSys*SecInHour
  QTotalHeatRecovered = QExhaustRecovered + QLubeOilRecovered + QJacketRecovered
  TotalHeatEnergyRec  = ExhaustEnergyRec + LubeOilEnergyRec + JacketEnergyRec
  FuelEnergyUseRate   = ABS(FuelEnergyUseRate)
  FuelEnergy          = ABS(FuelEnergy)
  FuelMdot      =  ABS(FuelEnergyUseRate)/(EngineDrivenChiller(ChillerNum)%FuelHeatingValue * KJtoJ)

 !check for problems BG 9/12/06 (deal with observed negative energy results)
  IF (Energy < 0.0d0) then  ! there is a serious problem
    IF (EngineDrivenChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
     ! first check for run away condenser loop temps (only reason yet to be observed for this?)
      IF (CondInletTemp > 70.0d0 )  then
        CALL ShowSevereError('CalcEngineDrivenChillerModel: Condenser loop inlet temperatures '//  &
           '> 70.0 C for EngineDrivenChiller='//  &
           TRIM(EngineDrivenChiller(ChillerNum)%Base%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Condenser loop water temperatures are too high at'//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL ShowContinueError('Check input for condenser plant loop, especially cooling tower')
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )

        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    IF(.NOT.WarmupFlag)THEN
      If (AvailNomCapRat < 0.0d0 ) then     ! apparently the real reason energy goes negative
        CALL ShowSevereError('CalcEngineDrivenChillerModel: Capacity ratio below zero for EngineDrivenChiller='//  &
                              TRIM(EngineDrivenChiller(ChillerNum)%Base%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Check input for Capacity Ratio Curve')
        CALL showContinueError('Condenser inlet temperature: '//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )
        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    ! If makes it here, set limits, chiller can't have negative energy/power
    ! proceeding silently for now but may want to throw error here
    Power = 0.0d0
    Energy = 0.0d0
  ENDIF

  RETURN
END SUBROUTINE CalcEngineDrivenChillerModel

SUBROUTINE CalcGTChillerModel(ChillerNum,MyLoad,Runflag,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a vapor compression chiller using the GT model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:

          ! REFERENCES:
          ! 1. BLAST Users Manual
          ! 2. CHILLER User Manual

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, SecInHour, CurrentTime
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys, SysTimeElapsed
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_CombTurbine, CompSetPtBasedSchemeType, &
                              CriteriaType_MassFlowRate, SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY : ControlType_SeriesActive, MassFlowTolerance
  USE DataEnvironment, ONLY : OutDryBulbTemp, EnvironmentName, CurMnDy
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: ChillerNum        ! chiller number
  REAL(r64)              :: MyLoad          ! operating load
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when chiller operating
  INTEGER, INTENT(IN) :: EquipFlowCtrl  ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), PARAMETER        :: ExhaustCP = 1.047d0 !Exhaust Gas Specific Heat
  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), DIMENSION(3)     :: CapacityRat           ! intermediate result:  capacity ratio
  REAL(r64), DIMENSION(3)     :: PowerRat              ! intermediate result:  power ratio
  REAL(r64), DIMENSION(3)     :: FullLoadFactor        ! intermediate result:  full load factor
  REAL(r64)              :: MinPartLoadRat        ! min allowed operating frac full load
  REAL(r64)              :: MaxPartLoadRat        ! max allowed operating frac full load
  REAL(r64)              :: TempCondIn            ! C - (GT ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempCondInDesign      ! C - (GT ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempRiseRat           ! intermediate result:  temperature rise ratio
  REAL(r64)              :: EvapInletTemp         ! C - evaporator inlet temperature, water side
  REAL(r64)              :: CondInletTemp         ! C - condenser inlet temperature, water side
  REAL(r64)              :: TempEvapOut           ! C - evaporator outlet temperature, water side
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  REAL(r64)              :: TempEvapOutDesign     ! design evaporator outlet temperature, water side
  REAL(r64)              :: ChillerNomCap         ! chiller nominal capacity
  REAL(r64)              :: AvailChillerCap       ! chiller available capacity
  REAL(r64)              :: COP                   ! coefficient of performance
  REAL(r64)              :: FracFullLoadPower     ! fraction of full load power
  REAL(r64)              :: EvapDeltaTemp         ! C - evaporator temperature difference, water side
  REAL(r64)              :: DeltaTemp             ! C - intermediate result: condenser/evaporator temp diff
  REAL(r64)              :: AvailNomCapRat        ! intermediate result: available nominal capacity ratio
  REAL(r64)              :: FullLoadPowerRat      ! intermediate result: full load power ratio
  REAL(r64)              :: PartLoadRat           ! part load ratio for efficiency calculations
  REAL(r64)              :: OperPartLoadRat       ! Actual Operating PLR
  INTEGER                :: EvapInletNode         ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode        ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode         ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode        ! condenser outlet node number, water side
  REAL(r64), SAVE             :: EvapMassFlowRateMax=0.0d0   ! Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
  REAL(r64)              :: TempLowLimitEout      ! C - Evaporator low temp. limit cut off
! Special variables for GT Chiller
  REAL(r64)              :: RPLoad
  REAL(r64)              :: PLoad
  REAL(r64)              :: GTEngineCapacity      ! Capacity of GT Unit attached to Chiller
  REAL(r64)              :: MaxExhaustperGTPower  ! Maximum Exhaust Flow per KW Power Out
  REAL(r64)              :: RL
  REAL(r64)              :: RL2

  REAL(r64)              :: FuelEnergyIn          !(EFUEL) Amount of Fuel Energy Required to run gas turbine
  REAL(r64)              :: ExhaustFlow           !(FEX) Exhaust Gas Flow Rate cubic meters per second
  REAL(r64)              :: ExhaustTemp           !(TEX) Exhaust Gas Temperature in C
  REAL(r64)              :: QHeatRecLube          !(ELUBE) Recoverable Lube Oil Energy (W)
  REAL(r64)              :: UAtoCapRat            !(UACGC) Heat Exchanger UA to Capacity
  REAL(r64)              :: AmbientDeltaT         !(ATAIR) Difference between ambient actual and ambient design temperatures
  REAL(r64)         :: DesignSteamSatTemp         !Saturization Temperature of Steam in Stack
  REAL(r64)         :: ExhaustStackTemp           !Temperature of Exhaust Gases
  REAL(r64),SAVE  :: TimeStepSysLast=0.0d0     ! last system time step (used to check for downshifting)
  REAL(r64)       :: CurrentEndTime          ! end time of time step for current simulation time step
  REAL(r64),SAVE  :: CurrentEndTimeLast=0.0d0  ! end time of time step for last simulation time step
  CHARACTER(len=6):: OutputChar = ' '        ! character string for warning messages

  INTEGER           :: HeatRecInNode     !Heat Recovery Fluid Inlet Node Num
  INTEGER           :: HeatRecOutNode    !Heat Recovery Fluid Outlet Node Num
  REAL(r64)         :: HeatRecInTemp     !Heat Recovery Fluid Inlet Temperature
  REAL(r64)         :: HeatRecOutTemp    !Heat Recovery Fluid Outlet Temperature
  REAL(r64)         :: HeatRecMdot       !Heat Recovery Fluid Mass FlowRate
  REAL(r64)         :: HeatRecCp         !Specific Heat of the Heat Recovery Fluid
  REAL(r64)         :: FuelHeatingValue  !Heating Value of Fuel in kJ/kg
  REAL(r64)         :: MinHeatRecMdot    !Mass Flow rate that keeps from exceeding max temp
  REAL(r64)         :: HeatRecRatio      !Reduced ratio to multiply recovered heat terms by
  REAL(r64)         :: FRAC
!  LOGICAL,SAVE      :: PossibleSubCooling=.FALSE.

  INTEGER     :: LoopNum
  INTEGER     :: LoopSideNum
  REAL(r64)   :: Cp      ! local for fluid specif heat, for evaporator
  REAL(r64)   :: CpCond  ! local for fluid specif heat, for condenser

          !set module level inlet and outlet nodes
  EvapMassFlowRate           = 0.0d0
  CondMassFlowRate           = 0.0d0
  Power                      = 0.0d0
  QCondenser                 = 0.0d0
  QEvaporator                = 0.0d0
  Energy                     = 0.0d0
  CondenserEnergy            = 0.0d0
  EvaporatorEnergy           = 0.0d0
  EvapInletNode  = GTChiller(ChillerNum)%Base%EvapInletNodeNum
  EvapOutletNode = GTChiller(ChillerNum)%Base%EvapOutletNodeNum
  CondInletNode  = GTChiller(ChillerNum)%Base%CondInletNodeNum
  CondOutletNode = GTChiller(ChillerNum)%Base%CondOutletNodeNum
  HeatRecInNode  = GTChiller(ChillerNum)%HeatRecInletNodeNum
  HeatRecOutNode = GTChiller(ChillerNum)%HeatRecOutletNodeNum
  QHeatRecLube   = 0.0d0
  FRAC           = 1.0d0
  LoopNum        = GTChiller(ChillerNum)%Base%CWLoopNum
  LoopSideNum    = GTChiller(ChillerNum)%Base%CWLoopSideNum
  EvapInletTemp  = Node(EvapInletNode)%Temp

! calculate end time of current time step
  CurrentEndTime = CurrentTime + SysTimeElapsed

! Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
! Wait for next time step to print warnings. If simulation iterates, print out
! the warning for the last iteration only. Must wait for next time step to accomplish this.
! If a warning occurs and the simulation down shifts, the warning is not valid.
  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
    IF(GTChiller(ChillerNum)%Base%PrintMessage)THEN
      GTChiller(ChillerNum)%Base%MsgErrorCount = &
                         GTChiller(ChillerNum)%Base%MsgErrorCount + 1
     ! Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
      IF (GTChiller(ChillerNum)%Base%MsgErrorCount < 2) THEN
        CALL ShowWarningError(TRIM(GTChiller(ChillerNum)%Base%MsgBuffer1)//'.')
        CALL ShowContinueError(TRIM(GTChiller(ChillerNum)%Base%MsgBuffer2))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(GTChiller(ChillerNum)%Base%MsgBuffer1)//' error continues.', &
          GTChiller(ChillerNum)%Base%ErrCount1,ReportMaxOf=GTChiller(ChillerNum)%Base%MsgDataLast,  &
          ReportMinOf=GTChiller(ChillerNum)%Base%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
      END IF
    END IF
  END IF

! save last system time step and last end time of current time step (used to determine if warning is valid)
  TimeStepSysLast    = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime

! If Chiller load is 0 or chiller is not running then leave the subroutine.Before leaving
! if the component control is SERIESACTIVE we set the component flow to inlet flow so that
! flow resolver will not shut down the branch
  IF(MyLoad >= 0.d0 .OR. .NOT. RunFlag) THEN
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    ELSE
      EvapMassFlowRate           = 0.d0

      CALL SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          GTChiller(ChillerNum)%Base%CWLoopNum,     &
                          GTChiller(ChillerNum)%Base%CWLoopSideNum, &
                          GTChiller(ChillerNum)%Base%CWBranchNum,   &
                          GTChiller(ChillerNum)%Base%CWCompNum)
    ENDIF
    IF (GTChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
      IF ( PlantLoop(GTChiller(ChillerNum)%Base%CDLoopNum)% &
            LoopSide(GTChiller(ChillerNum)%Base%CDLoopSideNum)% &
              Branch(GTChiller(ChillerNum)%Base%CDBranchNum)%  &
                Comp(GTChiller(ChillerNum)%Base%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate         = Node(CondInletNode)%MassFlowrate
      ELSE
        CondMassFlowRate           = 0.d0
        CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                                GTChiller(ChillerNum)%Base%CDLoopNum, &
                                GTChiller(ChillerNum)%Base%CDLoopSideNum, &
                                GTChiller(ChillerNum)%Base%CDBranchNum, &
                                GTChiller(ChillerNum)%Base%CDCompNum)
      ENDIF
    ENDIF

    IF (GTChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(GTChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff,&
                                GTChiller(ChillerNum)%Base%BasinHeaterSchedulePtr,&
                                GTChiller(ChillerNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    GTChiller(ChillerNum)%Base%PrintMessage = .FALSE.
    RETURN
  END IF

  IF (GTChiller(ChillerNum)%Base%CondenserType == AirCooled) THEN !Condenser inlet temp = outdoor temp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirDryBulb
!  Warn user if entering condenser temperature falls below 0C
    IF(Node(CondInletNode)%Temp .LT. 0.0d0 .and. .not. WarmupFlag) THEN
      GTChiller(ChillerNum)%Base%PrintMessage = .TRUE.
      WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
      GTChiller(ChillerNum)%Base%MsgBuffer1 = 'CalcGasTurbineChillerModel - Chiller:CombustionTurbine "' &
                           //TRIM(GTChiller(ChillerNum)%Base%Name)// &
                           '" - Air Cooled Condenser Inlet Temperature below 0C'
      GTChiller(ChillerNum)%Base%MsgBuffer2 = '... Outdoor Dry-bulb Condition = '//TRIM(OutputChar)// &
                 ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                 //TRIM(CreateSysTimeIntervalString())
      GTChiller(ChillerNum)%Base%MsgDataLast = Node(CondInletNode)%Temp
    ELSE
      GTChiller(ChillerNum)%Base%PrintMessage = .FALSE.
    ENDIF
  Else IF (GTChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN !Condenser inlet temp = (outdoor wet bulb)
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirWetBulb
!  Warn user if evap condenser wet bulb temperature falls below 10C
    IF(Node(CondInletNode)%Temp .LT. 10.0d0 .and. .not. WarmupFlag) THEN
      GTChiller(ChillerNum)%Base%PrintMessage = .TRUE.
      WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
      GTChiller(ChillerNum)%Base%MsgBuffer1 = 'CalcGasTurbineChillerModel - Chiller:CombustionTurbine "' &
                           //TRIM(GTChiller(ChillerNum)%Base%Name)// &
                           '" - Evap Cooled Condenser Inlet Temperature below 10C'
      GTChiller(ChillerNum)%Base%MsgBuffer2 = '... Outdoor Wet-bulb Condition = '//TRIM(OutputChar)// &
                 ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                 //TRIM(CreateSysTimeIntervalString())
      GTChiller(ChillerNum)%Base%MsgDataLast = Node(CondInletNode)%Temp
    ELSE
      GTChiller(ChillerNum)%Base%PrintMessage = .FALSE.
    ENDIF
  ENDIF ! End of the Air Cooled/Evap Cooled Logic block

  ! If not air or evap cooled then set to the condenser node that is attached to a cooling tower
  CondInletTemp  = Node(CondInletNode)%Temp

  !Set mass flow rates
  IF (GTChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
    CondMassFlowRate = GTChiller(ChillerNum)%Base%CondMassFlowRateMax
    CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              GTChiller(ChillerNum)%Base%CDLoopNum, &
                              GTChiller(ChillerNum)%Base%CDLoopSideNum, &
                              GTChiller(ChillerNum)%Base%CDBranchNum, &
                              GTChiller(ChillerNum)%Base%CDCompNum)
    CALL PullCompInterconnectTrigger(GTChiller(ChillerNum)%Base%CWLoopNum, &
                                     GTChiller(ChillerNum)%Base%CWLoopSideNum, &
                                     GTChiller(ChillerNum)%Base%CWBranchNum, &
                                     GTChiller(ChillerNum)%Base%CWCompNum, &
                                     GTChiller(ChillerNum)%Base%CondMassFlowIndex,              &
                                     GTChiller(ChillerNum)%Base%CDLoopNum, &
                                     GTChiller(ChillerNum)%Base%CDLoopSideNum,   &
                                     CriteriaType_MassFlowRate, &
                                     CondMassFlowRate)

    IF (CondMassFlowRate < MassFlowTolerance) RETURN

  END IF

  !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  CapacityRat        = GTChiller(ChillerNum)%CapRatCoef
  PowerRat           = GTChiller(ChillerNum)%PowerRatCoef
  FullLoadFactor     = GTChiller(ChillerNum)%FullLoadCoef
  MinPartLoadRat     = GTChiller(ChillerNum)%MinPartLoadRat
  MaxPartLoadRat     = GTChiller(ChillerNum)%MaxPartLoadRat
  TempCondInDesign   = GTChiller(ChillerNum)%TempDesCondIn
  TempRiseRat        = GTChiller(ChillerNum)%TempRiseCoef
  TempEvapOutDesign  = GTChiller(ChillerNum)%TempDesEvapOut
  ChillerNomCap      = GTChiller(ChillerNum)%Base%NomCap
  COP                = GTChiller(ChillerNum)%Base%COP
  TempCondIn         = Node(GTChiller(ChillerNum)%Base%CondInletNodeNum)%Temp
  TempEvapOut        = Node(GTChiller(ChillerNum)%Base%EvapOutletNodeNum)%Temp
  TempLowLimitEout   = GTChiller(ChillerNum)%TempLowLimitEvapOut
  EvapMassFlowRateMax = GTChiller(ChillerNum)%Base%EvapMassFlowRateMax
  LoopNum            = GTChiller(ChillerNum)%Base%CWLoopNum
  LoopSideNum        = GTChiller(ChillerNum)%Base%CWLoopSideNum

!*********************************

  !Calculate chiller performance from this set of performance equations.
  !  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))
  DeltaTemp= (TempCondIn   -  TempCondInDesign) / TempRiseRat &
           - (TempEvapOut -  TempEvapOutDesign)

  !  from BLAST...RCAV=RCAVC(1)+RCAVC(2)*Z+RCAVC(3)*Z**2
  AvailNomCapRat =   CapacityRat(1)                   &
                     + CapacityRat(2) * DeltaTemp       &
                     + CapacityRat(3) * DeltaTemp ** 2

  AvailChillerCap = ChillerNomCap*AvailNomCapRat

  ! from BLAST...G=ADJEC(1)+ADJEC(2)*RCAV+ADJEC(3)*RCAV**2.
  FullLoadPowerRat=   PowerRat(1)                         &
                      + PowerRat(2) * AvailNomCapRat      &
                      + PowerRat(3) * AvailNomCapRat ** 2

  !  from BLAST...RCLOAD=AMAX1(MINCHFR(I,IPLCTR),AMIN1(CHLRLOAD(I)/CHLROCAP(I) &
  !         /RCAV,MAXCHFR(I,IPLCTR)))
  IF (AvailChillerCap > 0.0d0) THEN
    PartLoadRat = MAX(MinPartLoadRat, MIN(ABS(MyLoad)/AvailChillerCap,MaxPartLoadRat))
  ENDIF

  ! from BLAST...RPOWER=RPWRC(1)+RPWRC(2)*RCLOAD+RPWRC(3)*RCLOAD**2
  FracFullLoadPower = FullLoadFactor(1)                      &
                      + FullLoadFactor(2) * PartLoadRat      &
                      + FullLoadFactor(3) * PartLoadRat ** 2

  IF (AvailChillerCap > 0.0d0) THEN
    IF(ABS(MyLoad)/AvailChillerCap .LT. MinPartLoadRat) THEN
     OperPartLoadRat = ABS(MyLoad)/AvailChillerCap
    ELSE
     OperPartLoadRat = PartLoadRat
    END IF
  ELSE
    OperPartLoadRat = 0.0d0
  ENDIF
!*********************************
  Cp = GetSpecificHeatGlycol(PlantLoop(GTChiller(ChillerNum)%Base%CWLoopNum)%FluidName,  &
                         Node(EvapInletNode)%Temp,                      &
                         PlantLoop(GTChiller(ChillerNum)%Base%CWLoopNum)%FluidIndex, &
                         'CalcGTChillerModel')
! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
! condenser side outlet temperature.
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==0) THEN
    GTChiller(ChillerNum)%Base%PossibleSubCooling  =.FALSE.
    QEvaporator = AvailChillerCap * OperPartLoadRat
    IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
     FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
    ELSE
     FRAC = 1.0d0
    END IF
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap/COP * FRAC

    ! Either set the flow to the Constant value or caluclate the flow for the variable volume
    IF ((GTChiller(ChillerNum)%Base%FlowMode == ConstantFlow) &
        .OR. (GTChiller(ChillerNum)%Base%FlowMode == NotModulated )) THEN
      ! Start by assuming max (design) flow
      EvapMassFlowRate = EvapMassFlowRateMax
      ! Use SetComponentFlowRate to decide actual flow
      Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          GTChiller(ChillerNum)%Base%CWLoopNum,     &
                          GTChiller(ChillerNum)%Base%CWLoopSideNum, &
                          GTChiller(ChillerNum)%Base%CWBranchNum,   &
                          GTChiller(ChillerNum)%Base%CWCompNum)
      ! Evaluate delta temp based on actual flow rate
      IF (EvapMassFlowRate /= 0.0D0) THEN
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      ELSE
        EvapDeltaTemp = 0.0D0
      ENDIF
      ! Evaluate outlet temp based on delta
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
    ELSE IF (GTChiller(ChillerNum)%Base%FlowMode == LeavingSetpointModulated) THEN
      ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
      SELECT CASE (PlantLoop(GTChiller(ChillerNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetpoint)
        EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint
      CASE (DualSetpointDeadband)
        EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPointHi
      END SELECT
      IF (EvapDeltaTemp /= 0) THEN
        ! Calculate desired flow to request based on load
        EvapMassFlowRate = ABS(QEvaporator/Cp/EvapDeltaTemp)
        IF((EvapMassFlowRate - EvapMassFlowRateMax) .GT. MassFlowTolerance) &
               GTChiller(ChillerNum)%Base%PossibleSubCooling = .TRUE.
        !Check to see if the Maximum is exceeded, if so set to maximum
        EvapMassFlowRate = MIN(EvapMassFlowRateMax, EvapMassFlowRate)
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          GTChiller(ChillerNum)%Base%CWLoopNum,     &
                          GTChiller(ChillerNum)%Base%CWLoopSideNum, &
                          GTChiller(ChillerNum)%Base%CWBranchNum,   &
                          GTChiller(ChillerNum)%Base%CWCompNum)
        SELECT CASE (PlantLoop(GTChiller(ChillerNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
        CASE (SingleSetpoint)
          EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
        CASE (DualSetpointDeadband)
          EvapOutletTemp = Node(EvapOutletNode)%TempSetPointHi
        END SELECT
      ELSE
        ! Try to request zero flow
        EvapMassFlowRate=0.0d0
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          GTChiller(ChillerNum)%Base%CWLoopNum,     &
                          GTChiller(ChillerNum)%Base%CWLoopSideNum, &
                          GTChiller(ChillerNum)%Base%CWBranchNum,   &
                          GTChiller(ChillerNum)%Base%CWCompNum)
        ! No deltaT since component is not running
        EvapOutletTemp = Node(EvapInletNode)%Temp

      END IF
    End If  !End of Constant Variable Flow If Block
  ELSE  ! If FlowLock is True

    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              GTChiller(ChillerNum)%Base%CWLoopNum,     &
                              GTChiller(ChillerNum)%Base%CWLoopSideNum, &
                              GTChiller(ChillerNum)%Base%CWBranchNum,   &
                              GTChiller(ChillerNum)%Base%CWCompNum)
!       Some other component set the flow to 0. No reason to continue with calculations.
    IF(EvapMassFlowRate == 0.0d0)THEN
      MyLoad = 0.0d0
      IF (GTChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
        CALL CalcBasinHeaterPower(GTChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff,&
                            GTChiller(ChillerNum)%Base%BasinHeaterSchedulePtr,&
                            GTChiller(ChillerNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
      ENDIF
      GTChiller(ChillerNum)%Base%PrintMessage = .FALSE.
      RETURN
    END IF

    IF(GTChiller(ChillerNum)%Base%PossibleSubCooling) THEN
      QEvaporator = ABS(MyLoad)
      EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
    ELSE  !No subcooling in this case.No recalculation required.Still need to check chiller low temp limit
      SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetpoint)
        IF ((GTChiller(ChillerNum)%Base%FlowMode == LeavingSetpointModulated) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(GTChiller(ChillerNum)%Base%CWBranchNum) &
              %Comp(GTChiller(ChillerNum)%Base%CWCompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
          TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
        ELSE
          TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
        ENDIF
      CASE (DualSetpointDeadband)
        IF ((GTChiller(ChillerNum)%Base%FlowMode == LeavingSetpointModulated) .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(GTChiller(ChillerNum)%Base%CWBranchNum) &
              %Comp(GTChiller(ChillerNum)%Base%CWCompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
            (Node(EvapOutletNode)%TempSetPointHi /= SensedNodeFlagValue) ) THEN
          TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPointHi
        ELSE
          TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPointHi
        ENDIF
      END SELECT
      EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
      QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
      EvapOutletTemp = TempEvapOutSetpoint
    END IF
    !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
    IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
      IF((Node(EvapInletNode)%Temp - TempLowLimitEout) .GT. DeltaTempTol) THEN
        EvapOutletTemp = TempLowLimitEout
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
      IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTempTol) THEN
        EvapOutletTemp = Node(EvapOutletNode)%TempMin
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    ! If load exceeds the distributed load set to the distributed load
    If(QEvaporator > ABS(MyLoad)) Then
      If(EvapMassFlowRate > MassFlowTolerance) THEN
        QEvaporator = ABS(MyLoad)
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0d0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End IF

    ! Checks QEvaporator on the basis of the machine limits.
    If(QEvaporator > (AvailChillerCap * MaxPartLoadRat))Then
      If(EvapMassFlowRate > MassFlowTolerance) THEN
        QEvaporator = AvailChillerCap * PartLoadRat
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0d0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End If

    IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
      FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
    ELSE
      FRAC = 1.0d0
    END IF

    ! set the module level variable used for reporting FRAC
    ChillerCyclingRatio = FRAC

    ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap /COP * FRAC

    IF(EvapMassFlowRate == 0.0d0) THEN
     QEvaporator = 0.0d0
     EvapOutletTemp = Node(EvapInletNode)%Temp
     Power = 0.0d0
     GTChiller(ChillerNum)%Base%PrintMessage = .FALSE.
    END IF
    IF(QEvaporator == 0.0d0 .AND. GTChiller(ChillerNum)%Base%CondenserType == EvapCooled) THEN
       CALL CalcBasinHeaterPower(GTChiller(ChillerNum)%Base%BasinHeaterPowerFTempDiff,&
                                 GTChiller(ChillerNum)%Base%BasinHeaterSchedulePtr,&
                                 GTChiller(ChillerNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
    END IF

    END IF  !This is the end of the FlowLock Block

    !Now determine Cooling
    !QCondenser is calculated the same for each type, but the power consumption should be different
    !  depending on the performance coefficients used for the chiller model.
    QCondenser = Power + QEvaporator

    IF (GTChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN

      IF (CondMassFlowRate > MassFlowTolerance) THEN
        CpCond = GetSpecificHeatGlycol(PlantLoop(GTChiller(ChillerNum)%Base%CDLoopNum)%FluidName,  &
                                       CondInletTemp,                      &
                                       PlantLoop(GTChiller(ChillerNum)%Base%CDLoopNum)%FluidIndex, &
                                       'CalcGTChillerModel')
        CondOutletTemp = QCondenser/CondMassFlowRate/CpCond + CondInletTemp
      ELSE
        CALL ShowSevereError('CalcGasTurbineChillerModel: Condenser flow = 0, for GasTurbineChiller='//  &
                             TRIM(GTChiller(ChillerNum)%Base%Name))
        CALL ShowContinueErrorTimeStamp(' ')

      END IF

    ELSE !Air Cooled or Evap Cooled

      !don't care about outlet temp for Air-Cooled or Evap Cooled and there is no CondMassFlowRate and would divide by zero
      CondOutletTemp = CondInletTemp
    END IF



    !Special GT Chiller Variables
    ! Gas Turbine Driven Portion of the Chiller:

    GTEngineCapacity = GTChiller(ChillerNum)%GTEngineCapacity
    MaxExhaustperGTPower = GTChiller(ChillerNum)%MaxExhaustperGTPower


!Note: All Old Blast Code comments begin at left.

!D                                   COMPUTE TOWER CLOAD
!               ETOWER(TypeIndex) = PREQD + CHLRLOAD(TypeIndex)
!               RPLOAD = PREQD/CHLROCAP(TypeIndex)
!
!               IF (RFLAGS(81)) WRITE (OUTPUT,703) PREQD,ETOWER(TypeIndex),RPLOAD
!               IF (PREQD .GT. 0.0d0) THEN
    IF (AvailChillerCap >0)THEN
      RPLoad = Power / AvailChillerCap
    ELSE
      RPLoad = 0.0d0
    END IF

    IF (Power > 0) THEN
!D$                               FOR EACH CHILLER OPERATING
!                  MAXSZ = NUMCHSIZ(TypeIndex,IPLCTR)
!                  DO IS = 1,MAXSZ
!
!                     NUMOPR = CHLRIOPR(IS,TypeIndex)
!                     IF (NUMOPR.GT.0) THEN
!
!                        PLOAD = CHNOMCAP(IS,TypeIndex,IPLCTR) * RPLOAD

    PLoad = ChillerNomCap * RPLoad

!
!D$                                COMPUTE FUEL AND WASTE HEAT
!
!     TEX IS CALCULATED USING COEFFICIENTS TEX2GC( ) TO RESULT IN TEMP.
!     DEGREES ACTUAL, HENCE THE NECESSARY CONVERSION ?-273.?
!
!                        RLOAD=AMAX1(PLOAD/CHLROCAP(TypeIndex),MINCHFR(TypeIndex,IPLCTR))
!                        RLD2 = RLOAD**2

     ! RL = MAX(PLoad/GTEngineCapacity, MinPartLoadRat * ChillerNomCap)
    RL = MAX(PLoad/ChillerNomCap, MinPartLoadRat)
    RL2 = RL**2

!     ATAIR = DELTA TEMPERATURE. ACTUAL - 25 DEG.C (77 DEG.F)
!                                RATING POINT
!                        ATAIR = ODB - 25.
!                        TAR2=ATAIR**2

    ! ??? Not sure about this Ambient Actual Temp - also do we need to have design ambient as input?

    IF (GTChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
      AmbientDeltaT = OutDryBulbTemp - 25.d0
    ELSE  ! air or evap cooled
      AmbientDeltaT = Node(CondInletNode)%OutAirDryBulb - 25.d0
    ENDIF


!                        EFUEL=PLOAD*(FUL1GC(1,IPLCTR)+FUL1GC(2,IPLCTR)*  &
!                              RLOAD+FUL1GC(3,IPLCTR)*RLD2)*              &
!                              (FUL2GC(1,IPLCTR)+FUL2GC(2,IPLCTR)*ATAIR+  &
!                              FUL2GC(3,IPLCTR)*TAR2)

    FuelEnergyIn = PLoad * (GTChiller(ChillerNum)%PLBasedFuelInputCoef(1) +         &
                               GTChiller(ChillerNum)%PLBasedFuelInputCoef(2)*RL +    &
                                  GTChiller(ChillerNum)%PLBasedFuelInputCoef(3)*RL2)  &
                           * (GTChiller(ChillerNum)%TempBasedFuelInputCoef(1) +       &
                                GTChiller(ChillerNum)%TempBasedFuelInputCoef(2)*AmbientDeltaT + &
                                  GTChiller(ChillerNum)%TempBasedFuelInputCoef(3)*AmbientDeltaT**2)


!                        FEX=GTDSLCAP(IS,TypeIndex,IPLCTR)*(FEXGC(1,IPLCTR)+      &
!                            FEXGC(2,IPLCTR)*ATAIR+FEXGC(3,IPLCTR)*TAR2)

    ExhaustFlow = GTEngineCapacity * (GTChiller(ChillerNum)%ExhaustFlowCoef(1) +      &
                                          GTChiller(ChillerNum)%ExhaustFlowCoef(2) * AmbientDeltaT  + &
                                            GTChiller(ChillerNum)%ExhaustFlowCoef(3) * AmbientDeltaT**2)

!                        TEX=(TEX1GC(1,IPLCTR)+TEX1GC(2,IPLCTR)*RLOAD+    &
!                            TEX1GC(3,IPLCTR)*RLD2)*(TEX2GC(1,IPLCTR)+    &
!                            TEX2GC(2,IPLCTR)*ATAIR+TEX2GC(3,IPLCTR)*     &
!                            TAR2)-273.

    ExhaustTemp = (GTChiller(ChillerNum)%PLBasedExhaustTempCoef(1) +          &
                       GTChiller(ChillerNum)%PLBasedExhaustTempCoef(2)*RL +     &
                         GTChiller(ChillerNum)%PLBasedExhaustTempCoef(3)*RL2)   &
                  * (GTChiller(ChillerNum)%TempBasedExhaustTempCoef(1) +        &
                       GTChiller(ChillerNum)%TempBasedExhaustTempCoef(2)*AmbientDeltaT + &
                         GTChiller(ChillerNum)%TempBasedExhaustTempCoef(3)*AmbientDeltaT**2) - 273



!                        UAG=UACGC(1,IPLCTR)*GTDSLCAP(IS,TypeIndex,IPLCTR)**      &
!                            UACGC(2,IPLCTR)
    IF (PLoad /= 0.0d0)THEN
      UAtoCapRat = GTChiller(ChillerNum)%UAtoCapCoef(1) * GTEngineCapacity **  &
                       GTChiller(ChillerNum)%UAtoCapCoef(2)

!     TSTACK = EXHAUST STACK TEMPERATURE, C.
!
!                        TSTACK=TSATUR(IPLCTR)+(TEX-TSATUR(IPLCTR))/      &
!                               EXP(UAG/(AMAX1(FEX,RMXKGC(IPLCTR)*        &
!                               GTDSLCAP(IS,TypeIndex,IPLCTR)) * 1.047))

      DesignSteamSatTemp = GTChiller(ChillerNum)%DesignSteamSatTemp
      ExhaustStackTemp = DesignSteamSatTemp + (ExhaustTemp - DesignSteamSatTemp) / &
                           EXP(UAtoCapRat/(MAX(ExhaustFlow, MaxExhaustperGTPower * GTEngineCapacity) * ExhaustCP))


!                        EEX = AMAX1 ( FEX*1.047*(TEX-TSTACK),0.0d0)
!                        ELUBE=PLOAD*(ELUBEGC(1,IPLCTR)+ELUBEGC(2,IPLCTR) &
!                              *RLOAD+ELUBEGC(3,IPLCTR)*RLD2 )
    END IF

    IF (GTChiller(ChillerNum)%HeatRecActive) THEN
      QHeatRecLube = PLoad * (GTChiller(ChillerNum)%HeatRecLubeEnergyCoef(1) +      &
                              GTChiller(ChillerNum)%HeatRecLubeEnergyCoef(2)*RL + &
                              GTChiller(ChillerNum)%HeatRecLubeEnergyCoef(3)*RL2)

    ELSE
      QHeatRecLube = 0.0d0
    End If

!                        CHLRFUEL(TypeIndex) = CHLRFUEL(TypeIndex) + EFUEL * NUMOPR
!                        EEXGC = EEXGC + EEX * NUMOPR
!                        ELBEGC = ELBEGC + ELUBE * NUMOPR
!


!Heat Recovery Loop -  lube recovered heat
!   If lube is not present, then the energy should be 0 at this point
! Thigh = Energy / (Mdot*Cp) + Tlow

    !Need to set the HeatRecRatio to 1.0 if it is not modified
    HeatRecRatio= 1.0d0

    IF (GTChiller(ChillerNum)%HeatRecActive) THEN
       !This mdot is input specified mdot "Desired Flowrate", already set at node in init routine
      HeatRecMdot = Node(HeatRecInNode)%MassFlowRate
      HeatRecInTemp = Node(HeatRecInNode)%Temp
      HeatRecCp = GetSpecificHeatGlycol(PlantLoop(GTChiller(ChillerNum)%HRLoopNum)%FluidName,  &
                                        HeatRecInTemp,                      &
                                        PlantLoop(GTChiller(ChillerNum)%HRLoopNum)%FluidIndex, &
                                        'ChillerHeatRecovery')

      !Don't divide by zero
      IF ((HeatRecMdot .GT. 0) .AND. (HeatRecCp .GT. 0)) THEN
        HeatRecOutTemp = (QHeatRecLube)/(HeatRecMdot * HeatRecCp) + HeatRecInTemp
      ELSE
        HeatRecOutTemp = HeatRecInTemp
      END IF

      !Now verify that the design flowrate was large enough to prevent phase change
      IF(HeatRecOutTemp > GTChiller(ChillerNum)%HeatRecMaxTemp) THEN
        IF(GTChiller(ChillerNum)%HeatRecMaxTemp /= HeatRecInTemp)THEN
          MinHeatRecMdot = (QHeatRecLube)/(HeatRecCp * (GTChiller(ChillerNum)%HeatRecMaxTemp - HeatRecInTemp))
          If(MinHeatRecMdot < 0.0d0) MinHeatRecMdot = 0.0d0
        END IF

        !Recalculate Outlet Temperature, with adjusted flowrate
        IF ((MinHeatRecMdot .GT. 0.0d0) .AND. (HeatRecCp .GT. 0.0d0)) THEN
          HeatRecOutTemp = (QHeatRecLube)/(MinHeatRecMdot * HeatRecCp) + HeatRecInTemp
          HeatRecRatio = HeatRecMdot/MinHeatRecMdot
        ELSE
          HeatRecOutTemp = HeatRecInTemp
          HeatRecRatio = 0.0d0
        END IF
      End If

      QHeatRecLube = QHeatRecLube*HeatRecRatio
    ELSE
      HeatRecInTemp=0.0d0
      HeatRecMDot=0.0d0
      HeatRecCp=0.0d0
      HeatRecOutTemp=0.0d0
    ENDIF

  END IF

   GTChiller(ChillerNum)%HeatRecInletTemp = HeatRecInTemp
   GTChiller(ChillerNum)%HeatRecOutletTemp = HeatRecOutTemp
   GTChiller(ChillerNum)%HeatRecMdot = HeatRecMdot
   GTChiller(ChillerNum)%HeatRecLubeEnergy = QHeatRecLube*(TimeStepSys*SecInHour)
   GTChiller(ChillerNum)%HeatRecLubeRate = QHeatRecLube
   GTChiller(ChillerNum)%FuelEnergyIn = ABS(FuelEnergyIn)

   FuelHeatingValue = GTChiller(ChillerNum)%FuelHeatingValue

   GTChillerReport(ChillerNum)%FuelMassUsedRate =  ABS(FuelEnergyIn)/(FuelHeatingValue * KJtoJ)

   GTChiller(ChillerNum)%ExhaustStackTemp = ExhaustStackTemp

      !Calculate Energy
   CondenserEnergy  = QCondenser*TimeStepSys*SecInHour
   Energy           = Power*TimeStepSys*SecInHour
   EvaporatorEnergy = QEvaporator*TimeStepSys*SecInHour

 !check for problems BG 9/12/06 (deal with observed negative energy results)
  IF (Energy < 0.0d0) then  ! there is a serious problem

    IF (GTChiller(ChillerNum)%Base%CondenserType == WaterCooled) THEN
     ! first check for run away condenser loop temps (only reason yet to be observed for this?)
      IF (CondInletTemp > 70.0d0 )  then
        CALL ShowSevereError('CalcGTChillerModel: Condenser loop inlet temperatures over 70.0 C for GTChiller='//  &
                            TRIM(GTChiller(ChillerNum)%Base%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Condenser loop water temperatures are too high at'//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL ShowContinueError('Check input for condenser plant loop, especially cooling tower')
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )

        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    IF(.NOT.WarmupFlag)THEN
      If (AvailNomCapRat < 0.0d0 ) then     ! apparently the real reason energy goes negative
        CALL ShowSevereError('CalcGTChillerModel: Capacity ratio below zero for GTChiller='//  &
                              TRIM(GTChiller(ChillerNum)%Base%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Check input for Capacity Ratio Curve')
        CALL showContinueError('Condenser inlet temperature: '//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )
        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    ! If makes it here, set limits, chiller can't have negative energy/power
    ! proceeding silently for now but may want to throw error here
    Power = 0.0d0
    Energy = 0.0d0
  ENDIF
  RETURN
END SUBROUTINE CalcGTChillerModel

SUBROUTINE CalcConstCOPChillerModel(ChillNum,MyLoad,Runflag,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002,
          !                      Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour, CurrentTime
  USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, &
                         ShowContinueErrorTimeStamp, ShowContinueError
  USE DataHVACGlobals, ONLY : TimeStepSys, SysTimeElapsed
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE DataPlant,       ONLY : PlantLoop, SimPlantEquipTypes, CompSetPtBasedSchemeType, &
                              CriteriaType_MassFlowRate, SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY : ControlType_SeriesActive, MassFlowTolerance
  USE DataEnvironment, ONLY : EnvironmentName, CurMnDy
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: ChillNum
  REAL(r64)              :: MyLoad
  LOGICAL                :: RunFlag
  INTEGER, INTENT(IN) :: EquipFlowCtrl  ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), parameter        :: DeltaTemptol=0.0001d0          ! C - minimum significant mass flow rate
  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: EvapDeltaTemp
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  INTEGER                :: EvapInletNode
  INTEGER                :: EvapOutletNode
  INTEGER                :: CondInletNode
  INTEGER                :: CondOutletNode
!  LOGICAL,SAVE           :: PossibleSubCooling=.FALSE.
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum
  REAL(r64),SAVE  :: TimeStepSysLast=0.0d0     ! last system time step (used to check for downshifting)
  REAL(r64)       :: CurrentEndTime          ! end time of time step for current simulation time step
  REAL(r64),SAVE  :: CurrentEndTimeLast=0.0d0  ! end time of time step for last simulation time step
  CHARACTER(len=6):: OutputChar = ' '        ! character string for warning messages
  REAL(r64)       :: Cp      ! local for fluid specif heat, for evaporator
  REAL(r64)       :: CpCond  ! local for fluid specif heat, for condenser

  EvapInletNode            = ConstCOPChiller(ChillNum)%Base%EvapInletNodeNum
  EvapOutletNode           = ConstCOPChiller(ChillNum)%Base%EvapOutletNodeNum
  CondInletNode            = ConstCOPChiller(ChillNum)%Base%CondInletNodeNum
  CondOutletNode           = ConstCOPChiller(ChillNum)%Base%CondOutletNodeNum

          !set module level chiller inlet and temperature variables
  LoopNum                  = ConstCOPChiller(ChillNum)%Base%CWLoopNum
  LoopSideNum              = ConstCOPChiller(ChillNum)%Base%CWLoopSideNum
  SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetpoint)
    IF ((ConstCOPChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) .OR. &
        (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(ConstCOPChiller(ChillNum)%Base%CWBranchNum) &
          %Comp(ConstCOPChiller(ChillNum)%Base%CWCompNum)%CurOpSchemeType &
             == CompSetPtBasedSchemeType)          .OR. &
        (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
      TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
    ELSE
      TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
    ENDIF
  CASE (DualSetpointDeadband)
    IF ((ConstCOPChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) .OR. &
        (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(ConstCOPChiller(ChillNum)%Base%CWBranchNum) &
          %Comp(ConstCOPChiller(ChillNum)%Base%CWCompNum)%CurOpSchemeType &
             == CompSetPtBasedSchemeType)          .OR. &
        (Node(EvapOutletNode)%TempSetPointHi /= SensedNodeFlagValue) ) THEN
      TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPointHi
    ELSE
      TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPointHi
    ENDIF
  END SELECT
  EvapDeltaTemp            = ABS(Node(EvapInletNode)%Temp - TempEvapOutSetpoint)
  EvapInletTemp            = Node(EvapInletNode)%Temp


          !If no component demand, or chiller OFF, or Chiller type set to 'Passive' by free
          !cooling heat exchanger, then set condenser side flow and heat transfer rates set to zero
  IF (MyLoad >= 0.d0 .OR. .NOT. Runflag) THEN

   !If Chiller load is 0 or greater or chiller is not running then leave the subroutine.Before leaving
   !if the component control is SERIESACTIVE we set the component flow to inlet flow so that
   !flow resolver will not shut down the branch
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    ELSE
      EvapMassFlowRate = 0.d0
      CALL SetComponentFlowRate( EvapMassFlowRate,  &
                                EvapInletNode , EvapOutletNode  , &
                                ConstCOPChiller(ChillNum)%Base%CWLoopNum,     &
                                ConstCOPChiller(ChillNum)%Base%CWLoopSideNum, &
                                ConstCOPChiller(ChillNum)%Base%CWBranchNum,   &
                                ConstCOPChiller(ChillNum)%Base%CWCompNum)
    ENDIF
    IF (ConstCOPChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
      IF ( PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)% &
            LoopSide(ConstCOPChiller(ChillNum)%Base%CDLoopSideNum)% &
              Branch(ConstCOPChiller(ChillNum)%Base%CDBranchNum)%  &
                Comp(ConstCOPChiller(ChillNum)%Base%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate           = Node(CondInletNode)%MassFlowrate
      ELSE
        CondMassFlowRate = 0.d0
        CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                                ConstCOPChiller(ChillNum)%Base%CDLoopNum, &
                                ConstCOPChiller(ChillNum)%Base%CDLoopSideNum, &
                                ConstCOPChiller(ChillNum)%Base%CDBranchNum, &
                                ConstCOPChiller(ChillNum)%Base%CDCompNum)
      ENDIF
    ENDIF

    EvapOutletTemp       = Node(EvapInletNode)%Temp
    CondOutletTemp       = Node(CondInletNode)%Temp

    Power                = 0.d0
    QEvaporator          = 0.d0
    QCondenser           = 0.d0
    Energy               = 0.d0
    EvaporatorEnergy     = 0.d0
    CondenserEnergy      = 0.d0

    IF (ConstCOPChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(ConstCOPChiller(ChillNum)%Base%BasinHeaterPowerFTempDiff,&
                            ConstCOPChiller(ChillNum)%Base%BasinHeaterSchedulePtr,&
                            ConstCOPChiller(ChillNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    ConstCOPChiller(ChillNum)%Base%PrintMessage = .FALSE.
    RETURN
  END IF

!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
    IF(ConstCOPChiller(ChillNum)%Base%PrintMessage)THEN
        ConstCOPChiller(ChillNum)%Base%MsgErrorCount = &
                         ConstCOPChiller(ChillNum)%Base%MsgErrorCount + 1
!       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
      IF (ConstCOPChiller(ChillNum)%Base%MsgErrorCount < 2) THEN
         CALL ShowWarningError(TRIM(ConstCOPChiller(ChillNum)%Base%MsgBuffer1)//'.')
         CALL ShowContinueError(TRIM(ConstCOPChiller(ChillNum)%Base%MsgBuffer2))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(ConstCOPChiller(ChillNum)%Base%MsgBuffer1)//' error continues.', &
           ConstCOPChiller(ChillNum)%Base%ErrCount1,ReportMaxOf=ConstCOPChiller(ChillNum)%Base%MsgDataLast,  &
           ReportMinOf=ConstCOPChiller(ChillNum)%Base%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
      END IF
    END IF
  END IF

!   save last system time step and last end time of current time step (used to determine if warning is valid)
  TimeStepSysLast    = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime



!otherwise the chiller is running...

  IF (ConstCOPChiller(ChillNum)%Base%CondenserType == AirCooled) THEN !Condenser inlet temp = outdoor temp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirDryBulb
!  Warn user if entering condenser temperature falls below 0C
      IF(Node(CondInletNode)%Temp .LT. 0.0d0 .and. .not. WarmupFlag) THEN
        ConstCOPChiller(ChillNum)%Base%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        ConstCOPChiller(ChillNum)%Base%MsgBuffer1 = 'CalcConstCOPChillerModel - Chiller:ConstantCOP "' &
                             //TRIM(ConstCOPChiller(ChillNum)%Base%Name)// &
                             '" - Air Cooled Condenser Inlet Temperature below 0C'
        ConstCOPChiller(ChillNum)%Base%MsgBuffer2 = '... Outdoor Dry-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        ConstCOPChiller(ChillNum)%Base%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        ConstCOPChiller(ChillNum)%Base%PrintMessage = .FALSE.
      ENDIF
  Else IF (ConstCOPChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN !Condenser inlet temp = (outdoor wet bulb)
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirWetBulb
!  Warn user if evap condenser wet bulb temperature falls below 10C
      IF(Node(CondInletNode)%Temp .LT. 10.0d0 .and. .not. WarmupFlag) THEN
        ConstCOPChiller(ChillNum)%Base%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        ConstCOPChiller(ChillNum)%Base%MsgBuffer1 = 'CalcConstCOPChillerModel - Chiller:ConstantCOP "' &
                             //TRIM(ConstCOPChiller(ChillNum)%Base%Name)// &
                             '" - Evap Cooled Condenser Inlet Temperature below 10C'
        ConstCOPChiller(ChillNum)%Base%MsgBuffer2 = '... Outdoor Wet-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        ConstCOPChiller(ChillNum)%Base%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        ConstCOPChiller(ChillNum)%Base%PrintMessage = .FALSE.
      ENDIF
  ENDIF ! End of the Air Cooled/Evap Cooled Logic block

     ! If not air or evap cooled then set to the condenser node that is attached to a cooling tower
  CondInletTemp            = Node(CondInletNode)%Temp

        !Set condenser flow rate
  IF (ConstCOPChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
    CondMassFlowRate = ConstCOPChiller(ChillNum)%Base%CondMassFlowRateMax
    CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              ConstCOPChiller(ChillNum)%Base%CDLoopNum, &
                              ConstCOPChiller(ChillNum)%Base%CDLoopSideNum, &
                              ConstCOPChiller(ChillNum)%Base%CDBranchNum, &
                              ConstCOPChiller(ChillNum)%Base%CDCompNum)
    CALL PullCompInterconnectTrigger(ConstCOPChiller(ChillNum)%Base%CWLoopNum, &
                                     ConstCOPChiller(ChillNum)%Base%CWLoopSideNum, &
                                     ConstCOPChiller(ChillNum)%Base%CWBranchNum, &
                                     ConstCOPChiller(ChillNum)%Base%CWCompNum, &
                                     ConstCOPChiller(ChillNum)%Base%CondMassFlowIndex,              &
                                     ConstCOPChiller(ChillNum)%Base%CDLoopNum, &
                                     ConstCOPChiller(ChillNum)%Base%CDLoopSideNum,   &
                                     CriteriaType_MassFlowRate, &
                                     CondMassFlowRate)

    IF (CondMassFlowRate < MassFlowTolerance) RETURN

  END IF

    ! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
    ! condenser side outlet temperature.

  Cp = GetSpecificHeatGlycol(PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%FluidName,  &
                         Node(EvapInletNode)%Temp,                      &
                         PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%FluidIndex, &
                         'CalcConstCOPChillerModel')

  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==0) THEN
     ConstCOPChiller(ChillNum)%Base%PossibleSubCooling = .FALSE.
     QEvaporator = ABS(MyLoad)
     Power = ABS(MyLoad) / ConstCOPChiller(ChillNum)%Base%COP

     ! Either set the flow to the Constant value or caluclate the flow for the variable volume
     IF ((ConstCOPChiller(ChillNum)%Base%FlowMode == ConstantFlow)  &
        .OR. (ConstCOPChiller(ChillNum)%Base%FlowMode == NotModulated)) THEN

          ! Start by assuming max (design) flow
        EvapMassFlowRate = ConstCOPChiller(ChillNum)%Base%EvapMassFlowRateMax
          ! Use SetComponentFlowRate to decide actual flow
          Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ConstCOPChiller(ChillNum)%Base%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%Base%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%Base%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%Base%CWCompNum)
          ! Evaluate delta temp based on actual flow rate
        IF (EvapMassFlowRate /= 0.0D0) THEN
          EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        ELSE
          EvapDeltaTemp = 0.0D0
        ENDIF
          ! Evaluate outlet temp based on delta
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp

     ELSE IF(ConstCOPChiller(ChillNum)%Base%FlowMode == LeavingSetpointModulated) THEN

        ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
        SELECT CASE (PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
        CASE (SingleSetpoint)
          EvapDeltaTemp = ABS(Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint)
        CASE (DualSetpointDeadband)
          EvapDeltaTemp = ABS(Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPointHi)
        END SELECT

        IF (EvapDeltaTemp > DeltaTemptol) THEN
          EvapMassFlowRate = ABS(QEvaporator/Cp/EvapDeltaTemp)
          IF((EvapMassFlowRate - ConstCOPChiller(ChillNum)%Base%EvapMassFlowRateMax) .GT. MassFlowTolerance) &
                    ConstCOPChiller(ChillNum)%Base%PossibleSubCooling = .TRUE.
          !Check to see if the Maximum is exceeded, if so set to maximum
          EvapMassFlowRate = MIN(ConstCOPChiller(ChillNum)%Base%EvapMassFlowRateMax, EvapMassFlowRate)
            ! Use SetComponentFlowRate to decide actual flow
            Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ConstCOPChiller(ChillNum)%Base%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%Base%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%Base%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%Base%CWCompNum)
          SELECT CASE (PlantLoop(ConstCOPChiller(ChillNum)%Base%CWLoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetpoint)
            EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
          CASE (DualSetpointDeadband)
            EvapOutletTemp = Node(EvapOutletNode)%TempSetPointHi
          END SELECT
        ELSE
            ! Try to request zero flow
          EvapMassFlowRate=0.0d0
            ! Use SetComponentFlowRate to decide actual flow
            Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ConstCOPChiller(ChillNum)%Base%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%Base%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%Base%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%Base%CWCompNum)
            ! No deltaT since component is not running
          EvapOutletTemp = Node(EvapInletNode)%Temp

        END IF
     End If  !End of Constant or Variable Flow If Block for FlowLock = 0 (or making a flow request)
  ELSE  ! If FlowLock is True

    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    CALL SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ConstCOPChiller(ChillNum)%Base%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%Base%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%Base%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%Base%CWCompNum)
!   Some other component set the flow to 0. No reason to continue with calculations.
     IF(EvapMassFlowRate == 0.0d0)THEN
       MyLoad = 0.0d0
       IF (ConstCOPChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
         CALL CalcBasinHeaterPower(ConstCOPChiller(ChillNum)%Base%BasinHeaterPowerFTempDiff,&
                              ConstCOPChiller(ChillNum)%Base%BasinHeaterSchedulePtr,&
                              ConstCOPChiller(ChillNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
       ENDIF
       ConstCOPChiller(ChillNum)%Base%PrintMessage = .FALSE.
       RETURN
     END IF

     !Recalculate the Delts Temp
        IF(ConstCOPChiller(ChillNum)%Base%PossibleSubCooling) THEN
         QEvaporator = ABS(MyLoad)
         EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
         EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
         IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
          EvapOutletTemp = Node(EvapOutletNode)%TempMin
          EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
          QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
         END IF
        ELSE
         EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
          !Calculate the evaporator heat transfer at the specified flow which could have changed
          !  in the Flow Resolution step.
         QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
         EvapOutletTemp = TempEvapOutSetpoint
       END IF
        !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
         IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
          IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTempTol) THEN
           EvapOutletTemp = Node(EvapOutletNode)%TempMin
           EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
           QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
          ELSE
           EvapOutletTemp = Node(EvapInletNode)%Temp
           EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
           QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
          END IF
         END IF
     ! If load exceeds the distributed load set to the distributed load
     If(QEvaporator > ABS(MyLoad)) Then
       If(EvapMassFlowRate > MassFlowTolerance) THEN
           QEvaporator = ABS(MyLoad)
           EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
           EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
       Else
           QEvaporator = 0.0d0
           EvapOutletTemp = Node(EvapInletNode)%Temp
       End If
     End IF

     ! Checks QEvaporator on the basis of the machine limits.
     If(QEvaporator > ConstCOPChiller(ChillNum)%Base%NomCap)Then
       If(EvapMassFlowRate > MassFlowTolerance) THEN
           QEvaporator = ConstCOPChiller(ChillNum)%Base%NomCap
           EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
           EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
       Else
           QEvaporator = 0.0d0
           EvapOutletTemp = Node(EvapInletNode)%Temp
       End If
     End If
      !Calculate the Power consumption of the Const COP chiller which is a simplified calculation
     Power = QEvaporator / ConstCOPChiller(ChillNum)%Base%COP
     IF(EvapMassFlowRate == 0.0d0) THEN
      QEvaporator = 0.0d0
      EvapOutletTemp = Node(EvapInletNode)%Temp
      Power = 0.0d0
      ConstCOPChiller(ChillNum)%Base%PrintMessage = .FALSE.
     END IF
     IF(QEvaporator == 0.0d0 .AND. ConstCOPChiller(ChillNum)%Base%CondenserType == EvapCooled) THEN
        CALL CalcBasinHeaterPower(ConstCOPChiller(ChillNum)%Base%BasinHeaterPowerFTempDiff,&
                                  ConstCOPChiller(ChillNum)%Base%BasinHeaterSchedulePtr,&
                                  ConstCOPChiller(ChillNum)%Base%BasinHeaterSetPointTemp,BasinHeaterPower)
     END IF

  END IF !This is the end of the FlowLock Block

    !QCondenser is calculated the same for each type, but the power consumption should be different
    !  depending on the performance coefficients used for the chiller model.
    QCondenser = Power + QEvaporator

    IF (ConstCOPChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
       CpCond = GetSpecificHeatGlycol(PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                      CondInletTemp,                      &
                                      PlantLoop(ConstCOPChiller(ChillNum)%Base%CDLoopNum)%FluidIndex, &
                                      'CalcConstCOPChillerModel')
       IF (CondMassFlowRate > MassFlowTolerance) THEN
         CondOutletTemp = QCondenser/CondMassFlowRate/CpCond + CondInletTemp
       ELSE
         CALL ShowSevereError('CalcConstCOPChillerModel: Condenser flow = 0, for CONST COP Chiller='//  &
                              TRIM(ConstCOPChiller(ChillNum)%Base%Name))
         CALL ShowContinueErrorTimeStamp(' ')

       END IF
    ELSE ! Air Cooled or Evap Cooled
         !  Set condenser outlet temp to condenser inlet temp for Air Cooled or Evap Cooled
         !  since there is no CondMassFlowRate and would divide by zero
      CondOutletTemp = CondInletTemp
    END IF

        !Calculate Energy
   CondenserEnergy  = QCondenser*TimeStepSys*SecInHour
   Energy           = Power*TimeStepSys*SecInHour
   EvaporatorEnergy = QEvaporator*TimeStepSys*SecInHour

 !check for problems BG 9/12/06 (deal with observed negative energy results)
  IF (Energy < 0.0d0) then  ! there is a serious problem

    IF (ConstCOPChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
     ! first check for run away condenser loop temps (only reason yet to be observed for this?)
      IF (CondInletTemp > 70.0d0 )  then
        CALL ShowSevereError('CalcConstCOPChillerModel: Condenser loop inlet temperatures over 70.0 C for ConstCOPChiller='//  &
                            TRIM(ConstCOPChiller(ChillNum)%Base%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Condenser loop water temperatures are too high at'//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL ShowContinueError('Check input for condenser plant loop, especially cooling tower')
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )

        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    ! If makes it here, set limits, chiller can't have negative energy/power
    ! proceeding silently for now but may want to throw error here
    Power = 0.0d0
    Energy = 0.0d0

  ENDIF
RETURN
END SUBROUTINE CalcConstCOPChillerModel

SUBROUTINE CalcElectricChillerHeatRecovery(ChillNum,QCond,CondMassFlow,CondInletTemp,QHeatRec)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Richard Liesen
            !       DATE WRITTEN:    January 2004

            ! PURPOSE OF THIS SUBROUTINE:
            ! Calculate the heat recovered from the chiller condenser


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS:
USE Psychrometrics, ONLY: PsyCpAirFnWTdb
USE FluidProperties, ONLY: GetSpecificHeatGlycol
USE DataPlant,       ONLY: PlantLoop, SingleSetpoint, DualSetPointDeadBand
USE ScheduleManager, ONLY: GetCurrentScheduleValue

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)     :: ChillNum      ! number of the current electric chiller being simulated
  REAL(r64),INTENT(INOut)       :: QCond         ! current condenser load
  REAL(r64),INTENT(Out)         :: QHeatRec      ! amount of heat recovered
  REAL(r64),INTENT(IN)          :: CondMassFlow  ! current condenser Mass Flow
  REAL(r64),INTENT(IN)          :: CondInletTemp ! current condenser Inlet Temp

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CondInletNode     ! condenser inlet node number, water side
  INTEGER :: CondOutletNode    ! condenser outlet node number, water side
  INTEGER :: HeatRecInNode
  INTEGER :: HeatRecOutNode
  REAL(r64)    :: QTotal
  REAL(r64)    :: QCondTmp
  REAL(r64)    :: HeatRecInletTemp
  REAL(r64)    :: HeatRecMassFlowRate
  REAL(r64)    :: FracHeatRec
  REAL(r64)    :: TAvgIn
  REAL(r64)    :: TAvgOut
  REAL(r64)    :: CpHeatRec
  REAL(r64)    :: CpCond
  REAL(r64)    :: THeatRecSetpoint
  REAL(r64)    :: QHeatRecToSetpoint
  REAL(r64)    :: HeatRecHighInletLimit


  ! Begin routine
  HeatRecInNode  = ElectricChiller(ChillNum)%HeatRecInletNodeNum
  HeatRecOutNode = ElectricChiller(ChillNum)%HeatRecOutletNodeNum
  CondInletNode  = ElectricChiller(ChillNum)%Base%CondInletNodeNum
  CondOutletNode = ElectricChiller(ChillNum)%Base%CondOutletNodeNum
  HeatRecInletTemp  = Node(HeatRecInNode)%Temp
  HeatRecMassFlowRate = Node(HeatRecInNode)%MassFlowRate

  CpHeatRec = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%FluidName,  &
                                   HeatRecInletTemp,                      &
                                   PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%FluidIndex, &
                                   'ChillerHeatRecovery')

  IF(ElectricChiller(ChillNum)%Base%CondenserType == WaterCooled) THEN
     CpCond = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidName,  &
                                   CondInletTemp,                      &
                                   PlantLoop(ElectricChiller(ChillNum)%Base%CDLoopNum)%FluidIndex, &
                                   'ChillerHeatRecovery')
  ELSE
    CpCond = PsyCpAirFnWTdb(Node(CondInletNode)%HumRat,CondInletTemp,'ElecChillerHeatRecovery')
  END IF

  ! Before we modify the QCondenser, the total or original value is transferred to QTot
  QTotal = QCond

  IF (ElectricChiller(ChillNum)%HeatRecSetpointNodeNum == 0) THEN ! use original algorithm that blends temps
    TAvgIn = (HeatRecMassFlowRate*CpHeatRec*HeatRecInletTemp + CondMassFlow*CpCond*CondInletTemp)/  &
               (HeatRecMassFlowRate*CpHeatRec + CondMassFlow*CpCond)

    TAvgOut = QTotal/(HeatRecMassFlowRate*CpHeatRec + CondMassFlow*CpCond) + TAvgIn

    QHeatRec = HeatRecMassFlowRate * CpHeatRec * (TAvgOut - HeatRecInletTemp)
    QHeatRec = MAX(QHeatRec, 0.d0) ! ensure non negative
   !check if heat flow too large for physical size of bundle
    QHeatRec = MIN(QHeatRec, ElectricChiller(ChillNum)%HeatRecMaxCapacityLimit)
  ELSE ! use new algorithm to meet setpoint
    SELECT CASE (PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%LoopDemandCalcScheme)

    CASE (SingleSetPoint)
      THeatRecSetpoint = Node(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum)%TempSetPoint
    CASE (DualSetPointDeadBand)
      THeatRecSetpoint = Node(ElectricChiller(ChillNum)%HeatRecSetpointNodeNum)%TempSetPointHi
    END SELECT

    QHeatRecToSetpoint = HeatRecMassFlowRate *  CpHeatRec * (THeatRecSetpoint - HeatRecInletTemp)
    QHeatRecToSetpoint = MAX(QHeatRecToSetpoint, 0.d0)
    QHeatRec = MIN(QTotal,QHeatRecToSetpoint)
     !check if heat flow too large for physical size of bundle
    QHeatRec = MIN(QHeatRec, ElectricChiller(ChillNum)%HeatRecMaxCapacityLimit)

  ENDIF
   ! check if limit on inlet is present and exceeded.
  IF (ElectricChiller(ChillNum)%HeatRecInletLimitSchedNum > 0) THEN
    HeatRecHighInletLimit =  GetCurrentScheduleValue(ElectricChiller(ChillNum)%HeatRecInletLimitSchedNum)
    IF ( HeatRecInletTemp > HeatRecHighInletLimit) THEN ! shut down heat recovery
      QHeatRec = 0.d0
    ENDIF
  ENDIF

  QCond = QTotal - QHeatRec

  ! Calculate a new Heat Recovery Coil Outlet Temp
  IF (HeatRecMassFlowRate > 0.0d0) THEN
    HeatRecOutletTemp = QHeatRec/(HeatRecMassFlowRate*CpHeatRec) + HeatRecInletTemp
  Else
    HeatRecOutletTemp = HeatRecInletTemp
  End If


 RETURN
END SUBROUTINE CalcElectricChillerHeatRecovery

SUBROUTINE CalcEngineChillerHeatRec(ChillerNum, EnergyRecovered, HeatRecRatio)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Brandon Anderson
            !       DATE WRITTEN:    November 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! To perform heat recovery calculations and node updates


            ! METHODOLOGY EMPLOYED: This routine is required for the heat recovery loop.
            ! It works in conjunction with the Heat Recovery Manager, and the PlantWaterHeater.
            ! The chiller sets the flow on the loop first by the input design flowrate and then
            ! performs a check to verify that

            ! REFERENCES: na

            ! USE STATEMENTS: na
USE Psychrometrics,  ONLY: PsyCpAirFnWTdb
USE FluidProperties, ONLY: GetSpecificHeatGlycol
USE DataPlant,       ONLY: PlantLoop

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT(IN)          :: ChillerNum          ! Chiller number
  REAL(r64), INTENT(IN)       :: EnergyRecovered     ! Amount of heat recovered
  REAL(r64),INTENT(INOUT)     :: HeatRecRatio        ! Max Heat recovery ratio


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: HeatRecInNode
  INTEGER                  :: HeatRecOutNode
  REAL(r64)                :: HeatRecMdot
  REAL(r64)                :: MinHeatRecMdot
  REAL(r64)                :: HeatRecInTemp
  REAL(r64)                :: HeatRecOutTemp
  REAL(r64)                :: HeatRecCp

  !Load inputs to local structure
  HeatRecInNode  = EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum
  HeatRecOutNode = EngineDrivenChiller(ChillerNum)%HeatRecOutletNodeNum

  !Need to set the HeatRecRatio to 1.0 if it is not modified
  HeatRecRatio= 1.0d0

!  !This mdot is input specified mdot "Desired Flowrate", already set in init routine
  HeatRecMdot = Node(HeatRecInNode)%MassFlowRate

  HeatRecInTemp = Node(HeatRecInNode)%Temp
  HeatRecCp = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillerNum)%HRLoopNum)%FluidName,  &
                                   HeatRecInletTemp,                      &
                                   PlantLoop(EngineDrivenChiller(ChillerNum)%HRLoopNum)%FluidIndex, &
                                   'ChillerHeatRecovery')


  !Don't divide by zero - Note This also results in no heat recovery when
  !  design Mdot for Heat Recovery - Specified on Chiller Input - is zero
  !  In order to see what minimum heat recovery flow rate is for the design temperature
  !  The design heat recovery flow rate can be set very small, but greater than zero.
  IF ((HeatRecMdot .GT. 0) .AND. (HeatRecCp .GT. 0)) THEN
    HeatRecOutTemp = (EnergyRecovered)/(HeatRecMdot * HeatRecCp) + HeatRecInTemp
  ELSE
    HeatRecOutTemp = HeatRecInTemp
  END IF

  !Now verify that the design flowrate was large enough to prevent phase change
  IF(HeatRecOutTemp > EngineDrivenChiller(ChillerNum)%HeatRecMaxTemp) THEN
   IF(EngineDrivenChiller(ChillerNum)%HeatRecMaxTemp /= HeatRecInTemp)THEN
      MinHeatRecMdot = (EnergyRecovered)/(HeatRecCp * (EngineDrivenChiller(ChillerNum)%HeatRecMaxTemp - HeatRecInTemp))
      If(MinHeatRecMdot < 0.0d0) MinHeatRecMdot = 0.0d0
    END IF

    !Recalculate Outlet Temperature, with adjusted flowrate
    IF ((MinHeatRecMdot .GT. 0.0d0) .AND. (HeatRecCp .GT. 0.0d0)) THEN
      HeatRecOutTemp = (EnergyRecovered)/(MinHeatRecMdot * HeatRecCp) + HeatRecInTemp
      HeatRecRatio = HeatRecMdot/MinHeatRecMdot
    ELSE
      HeatRecOutTemp = HeatRecInTemp
      HeatRecRatio = 0.0d0
    END IF

  END IF


  !Update global variables for reporting later
  HeatRecInletTemp  = HeatRecInTemp
  HeatRecOutletTemp = HeatRecOutTemp
  HeatRecMdotActual = HeatRecMdot

END SUBROUTINE CalcEngineChillerHeatRec

SUBROUTINE UpdateElectricChillerRecords(MyLoad,RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na
  USE DataGlobals,     ONLY : SecInHour
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE PlantUtilities,  ONLY : SetComponentFlowRate, SafeCopyPlantNode
  USE Psychrometrics,  ONLY : PsyHFnTdbW


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)          :: MyLoad    ! current load
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if chiller operating
  INTEGER, INTENT(IN)      :: Num       ! chiller number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: EvapInletNode         ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode        ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode         ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode        ! condenser outlet node number, water side
  INTEGER                :: HeatRecInNode
  INTEGER                :: HeatRecOutNode
  REAL(r64)              :: ReportingConstant     ! Number of seconds per HVAC system time step, to convert from W (J/s) to J

  ReportingConstant = TimeStepSys*SecInHour

  EvapInletNode  = ElectricChiller(Num)%Base%EvapInletNodeNum
  EvapOutletNode = ElectricChiller(Num)%Base%EvapOutletNodeNum
  CondInletNode  = ElectricChiller(Num)%Base%CondInletNodeNum
  CondOutletNode = ElectricChiller(Num)%Base%CondOutletNodeNum
  HeatRecInNode  = ElectricChiller(Num)%HeatRecInletNodeNum
  HeatRecOutNode = ElectricChiller(Num)%HeatRecOutletNodeNum

  IF (MyLoad >= 0.d0 .OR. .NOT. RunFlag)THEN !Chiller not running so pass inlet states to outlet states
          !set node temperatures
    Node(EvapOutletNode)%Temp     = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp     = Node(CondInletNode)%Temp
    IF(ElectricChiller(Num)%Base%CondenserType /= WaterCooled) THEN
      Node(CondOutletNode)%HumRat   = Node(CondInletNode)%HumRat
      Node(CondOutletNode)%Enthalpy = Node(CondInletNode)%Enthalpy
    END IF


    ElectricChillerReport(Num)%Base%Power            = 0.0d0
    ElectricChillerReport(Num)%Base%QEvap            = 0.0d0
    ElectricChillerReport(Num)%Base%QCond            = 0.0d0
    ElectricChillerReport(Num)%Base%Energy           = 0.0d0
    ElectricChillerReport(Num)%Base%EvapEnergy       = 0.0d0
    ElectricChillerReport(Num)%Base%CondEnergy       = 0.0d0
    ElectricChillerReport(Num)%Base%EvapInletTemp    = Node(EvapInletNode)%Temp
    ElectricChillerReport(Num)%Base%CondInletTemp    = Node(CondInletNode)%Temp
    ElectricChillerReport(Num)%Base%CondOutletTemp   = Node(CondOutletNode)%Temp
    ElectricChillerReport(Num)%Base%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    ElectricChillerReport(Num)%Base%Evapmdot         = EvapMassFlowRate
    ElectricChillerReport(Num)%Base%Condmdot         = CondMassFlowRate
    ElectricChillerReport(Num)%ActualCOP        = 0.0d0
    IF (ElectricChiller(Num)%Base%CondenserType == EvapCooled) THEN
      ElectricChillerReport(Num)%Base%BasinHeaterPower       = BasinHeaterPower
      ElectricChillerReport(Num)%Base%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF

    If(ElectricChiller(Num)%HeatRecActive) Then

      CALL SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode)

      ElectricChillerReport(Num)%QHeatRecovery = 0.0d0
      ElectricChillerReport(Num)%EnergyHeatRecovery = 0.0d0
      ElectricChillerReport(Num)%HeatRecInletTemp   = Node(HeatRecInNode)%Temp
      ElectricChillerReport(Num)%HeatRecOutletTemp  = Node(HeatRecOutNode)%Temp
      ElectricChillerReport(Num)%HeatRecMassFlow    = Node(HeatRecInNode)%MassFlowRate

      ElectricChillerReport(Num)%ChillerCondAvgTemp = AvgCondSinkTemp
    End If

  ELSE !Chiller is running, so pass calculated values
          !set node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp
    IF(ElectricChiller(Num)%Base%CondenserType /= WaterCooled) THEN
      Node(CondOutletNode)%HumRat   = CondOutletHumRat
      Node(CondOutletNode)%Enthalpy = PsyHFnTdbW(CondOutletTemp, CondOutletHumRat)
    END IF
          !set node flow rates;  for these load based models
          !assume that the sufficient evaporator flow rate available
    ElectricChillerReport(Num)%Base%Power            = Power
    ElectricChillerReport(Num)%Base%QEvap            = QEvaporator
    ElectricChillerReport(Num)%Base%QCond            = QCondenser
    ElectricChillerReport(Num)%Base%Energy           = Energy
    ElectricChillerReport(Num)%Base%EvapEnergy       = EvaporatorEnergy
    ElectricChillerReport(Num)%Base%CondEnergy       = CondenserEnergy
    ElectricChillerReport(Num)%Base%EvapInletTemp    = Node(EvapInletNode)%Temp
    ElectricChillerReport(Num)%Base%CondInletTemp    = Node(CondInletNode)%Temp
    ElectricChillerReport(Num)%Base%CondOutletTemp   = Node(CondOutletNode)%Temp
    ElectricChillerReport(Num)%Base%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    ElectricChillerReport(Num)%Base%Evapmdot         = EvapMassFlowRate
    ElectricChillerReport(Num)%Base%Condmdot         = CondMassFlowRate
    IF (ElectricChiller(Num)%Base%CondenserType == EvapCooled) THEN
      ElectricChillerReport(Num)%Base%BasinHeaterPower       = BasinHeaterPower
      ElectricChillerReport(Num)%Base%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF
    IF (Power .NE. 0.0d0) THEN
       ElectricChillerReport(Num)%ActualCOP     = QEvaporator/Power
    ELSE
       ElectricChillerReport(Num)%ActualCOP     = 0.0d0
    END IF

    If(ElectricChiller(Num)%HeatRecActive) Then

       CALL SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode)
       ElectricChillerReport(Num)%QHeatRecovery = QHeatRecovered
       ElectricChillerReport(Num)%EnergyHeatRecovery = QHeatRecovered*TimeStepSys*SecInHour
       Node(HeatRecOutNode)%Temp = HeatRecOutletTemp
       ElectricChillerReport(Num)%HeatRecInletTemp   = Node(HeatRecInNode)%Temp
       ElectricChillerReport(Num)%HeatRecOutletTemp  = Node(HeatRecOutNode)%Temp
       ElectricChillerReport(Num)%HeatRecMassFlow    = Node(HeatRecInNode)%MassFlowRate
       ElectricChillerReport(Num)%ChillerCondAvgTemp = AvgCondSinkTemp
    End If

  END IF
RETURN
END SUBROUTINE UpdateElectricChillerRecords










! End of EngineDriven Chiller Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the EngineDriven Chiller Module
! *****************************************************************************

SUBROUTINE UpdateEngineDrivenChiller(MyLoad,RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na

USE DataGlobals,     ONLY: SecInHour
USE DataHVACGlobals, ONLY: TimeStepSys
USE PlantUtilities,  ONLY: SafeCopyPlantNode

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)     :: MyLoad    ! current load
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if chiller operating
  INTEGER, INTENT(IN)      :: Num       ! chiller number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: EvapInletNode       ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode      ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode       ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode      ! condenser outlet node number, water side
  INTEGER                :: HeatRecInletNode
  INTEGER                :: HeatRecOutletNode
  REAL(r64)              :: ReportingConstant   ! Number of seconds per HVAC system time step, to convert from W (J/s) to J

  ReportingConstant = TimeStepSys*SecInHour

    EvapInletNode  = EngineDrivenChiller(Num)%Base%EvapInletNodeNum
    EvapOutletNode = EngineDrivenChiller(Num)%Base%EvapOutletNodeNum
    CondInletNode  = EngineDrivenChiller(Num)%Base%CondInletNodeNum
    CondOutletNode = EngineDrivenChiller(Num)%Base%CondOutletNodeNum

    HeatRecInletNode = EngineDrivenChiller(Num)%HeatRecInletNodeNum
    HeatRecOutletNode = EngineDrivenChiller(Num)%HeatRecOutletNodeNum

  IF (MyLoad >=0.d0 .OR. .NOT. RunFlag)THEN !Chiller not running
          !set node temperatures
    Node(EvapOutletNode)%Temp     = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp     = Node(CondInletNode)%Temp

    EngineDrivenChillerReport(Num)%Base%Power            = 0.0d0
    EngineDrivenChillerReport(Num)%Base%QEvap            = 0.0d0
    EngineDrivenChillerReport(Num)%Base%QCond            = 0.0d0
    EngineDrivenChillerReport(Num)%Base%Energy           = 0.0d0
    EngineDrivenChillerReport(Num)%Base%EvapEnergy       = 0.0d0
    EngineDrivenChillerReport(Num)%Base%CondEnergy       = 0.0d0
    EngineDrivenChillerReport(Num)%Base%EvapInletTemp  = Node(EvapInletNode)%Temp
    EngineDrivenChillerReport(Num)%Base%CondInletTemp  = Node(CondInletNode)%Temp
    EngineDrivenChillerReport(Num)%Base%CondOutletTemp   = Node(CondOutletNode)%Temp
    EngineDrivenChillerReport(Num)%Base%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    EngineDrivenChillerReport(Num)%Base%Evapmdot         = EvapMassFlowRate
    EngineDrivenChillerReport(Num)%Base%Condmdot         = CondMassFlowRate
    EngineDrivenChillerReport(Num)%FuelCOP          = 0.0d0
    IF (EngineDrivenChiller(Num)%Base%CondenserType == EvapCooled) THEN
      EngineDrivenChillerReport(Num)%Base%BasinHeaterPower       = BasinHeaterPower
      EngineDrivenChillerReport(Num)%Base%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF
  ELSE !Chiller is running
          !set node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp

    EngineDrivenChillerReport(Num)%Base%Power            = Power
    EngineDrivenChillerReport(Num)%Base%QEvap            = QEvaporator
    EngineDrivenChillerReport(Num)%Base%QCond            = QCondenser
    EngineDrivenChillerReport(Num)%Base%Energy           = Energy
    EngineDrivenChillerReport(Num)%Base%EvapEnergy       = EvaporatorEnergy
    EngineDrivenChillerReport(Num)%Base%CondEnergy       = CondenserEnergy
    EngineDrivenChillerReport(Num)%Base%EvapInletTemp    = Node(EvapInletNode)%Temp
    EngineDrivenChillerReport(Num)%Base%CondInletTemp    = Node(CondInletNode)%Temp
    EngineDrivenChillerReport(Num)%Base%CondOutletTemp   = Node(CondOutletNode)%Temp
    EngineDrivenChillerReport(Num)%Base%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    EngineDrivenChillerReport(Num)%Base%Evapmdot         = EvapMassFlowRate
    EngineDrivenChillerReport(Num)%Base%Condmdot         = CondMassFlowRate
    IF (FuelEnergyUseRate .NE. 0.0d0) THEN
      EngineDrivenChillerReport(Num)%FuelCOP          = QEvaporator/FuelEnergyUseRate
    ELSE
      EngineDrivenChillerReport(Num)%FuelCOP          = 0.0d0
    END IF
    IF (EngineDrivenChiller(Num)%Base%CondenserType == EvapCooled) THEN
      EngineDrivenChillerReport(Num)%Base%BasinHeaterPower       = BasinHeaterPower
      EngineDrivenChillerReport(Num)%Base%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF
  END IF

! Update Heat Recovery Stuff whether running or not, variables should be set correctly
  EngineDrivenChillerReport(Num)%QJacketRecovered     = QJacketRecovered
  EngineDrivenChillerReport(Num)%QLubeOilRecovered    = QLubeOilRecovered
  EngineDrivenChillerReport(Num)%QExhaustRecovered    = QExhaustRecovered
  EngineDrivenChillerReport(Num)%QTotalHeatRecovered  = QTotalHeatRecovered
  EngineDrivenChillerReport(Num)%FuelEnergyUseRate    = FuelEnergyUseRate
  EngineDrivenChillerReport(Num)%JacketEnergyRec      = JacketEnergyRec
  EngineDrivenChillerReport(Num)%LubeOilEnergyRec     = LubeOilEnergyRec
  EngineDrivenChillerReport(Num)%ExhaustEnergyRec     = ExhaustEnergyRec
  EngineDrivenChillerReport(Num)%TotalHeatEnergyRec   = TotalHeatEnergyRec
  EngineDrivenChillerReport(Num)%FuelEnergy           = FuelEnergy
  EngineDrivenChillerReport(Num)%FuelMdot             = FuelMdot
  EngineDrivenChillerReport(Num)%ExhaustStackTemp     = ExhaustStackTemp
  EngineDrivenChillerReport(Num)%HeatRecInletTemp     = HeatRecInletTemp
  EngineDrivenChillerReport(Num)%HeatRecOutletTemp    = HeatRecOutletTemp
  EngineDrivenChillerReport(Num)%HeatRecMdot          = HeatRecMdotActual


  !Update the Heat Recovery outlet
  IF (EngineDrivenChiller(Num)%HeatRecActive) THEN
    CALL SafeCopyPlantNode(HeatRecInletNode, HeatRecOutletNode)
    Node(HeatRecOutletNode)%Temp = HeatRecOutletTemp
  ENDIF

RETURN
END SUBROUTINE UpdateEngineDrivenChiller



SUBROUTINE UpdateGTChillerRecords(MyLoad,RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na
  USE DataGlobals,     ONLY : SecInHour
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE PlantUtilities,  ONLY : SafeCopyPlantNode

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)     :: MyLoad    ! current load
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if chiller operating
  INTEGER, INTENT(IN)      :: Num       ! chiller number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: EvapInletNode         ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode        ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode         ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode        ! condenser outlet node number, water side

  INTEGER                :: HeatRecInletNode
  INTEGER                :: HeatRecOutletNode
  REAL(r64)              :: ReportingConstant     ! Number of seconds per HVAC system time step, to convert from W (J/s) to J

  ReportingConstant = TimeStepSys*SecInHour

  EvapInletNode  = GTChiller(Num)%Base%EvapInletNodeNum
  EvapOutletNode = GTChiller(Num)%Base%EvapOutletNodeNum
  CondInletNode  = GTChiller(Num)%Base%CondInletNodeNum
  CondOutletNode = GTChiller(Num)%Base%CondOutletNodeNum
  IF (GTChiller(Num)%HeatRecActive) THEN
    HeatRecInletNode = GTChiller(Num)%HeatRecInletNodeNum
    HeatRecOutletNode = GTChiller(Num)%HeatRecOutletNodeNum
  ENDIF

  IF (MyLoad >= 0.d0 .OR. .NOT. RunFlag)THEN !Chiller not running so pass inlet states to outlet states
          !set node temperatures
    Node(EvapOutletNode)%Temp     = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp     = Node(CondInletNode)%Temp


    IF (GTChiller(Num)%HeatRecActive) THEN
      CALL SafeCopyPlantNode( HeatRecOutletNode, HeatRecInletNode)
      GTChillerReport(Num)%HeatRecInletTemp     = Node(HeatRecInletNode)%Temp
      GTChillerReport(Num)%HeatRecOutletTemp    = Node(HeatRecOutletNode)%Temp
    ENDIF


    GTChillerReport(Num)%Base%Power            = 0.0d0
    GTChillerReport(Num)%Base%QEvap            = 0.0d0
    GTChillerReport(Num)%Base%QCond            = 0.0d0
    GTChillerReport(Num)%Base%Energy           = 0.0d0
    GTChillerReport(Num)%Base%EvapEnergy       = 0.0d0
    GTChillerReport(Num)%Base%CondEnergy       = 0.0d0
    GTChillerReport(Num)%Base%EvapInletTemp  = Node(EvapInletNode)%Temp
    GTChillerReport(Num)%Base%CondInletTemp  = Node(CondInletNode)%Temp
    GTChillerReport(Num)%Base%CondOutletTemp   = Node(CondOutletNode)%Temp
    GTChillerReport(Num)%Base%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    GTChillerReport(Num)%Base%Evapmdot         = EvapMassFlowRate
    GTChillerReport(Num)%Base%Condmdot         = CondMassFlowRate
    GTChillerReport(Num)%FuelEnergyUsedRate = 0.0d0
    GTChillerReport(Num)%FuelMassUsedRate   = 0.0d0
    GTChillerReport(Num)%FuelEnergyUsed     = 0.0d0
    GTChillerReport(Num)%FuelMassUsed       = 0.0d0

    GTChillerReport(Num)%HeatRecLubeEnergy    = 0.0d0
    GTChillerReport(Num)%HeatRecLubeRate      = 0.0d0
    GTChillerReport(Num)%ExhaustStackTemp     = 0.0d0
    GTChillerReport(Num)%HeatRecMdot          = GTChiller(Num)%HeatRecMdot
    GTChillerReport(Num)%FuelCOP              = 0.0d0
    IF (GTChiller(Num)%Base%CondenserType == EvapCooled) THEN
      GTChillerReport(Num)%Base%BasinHeaterPower       = BasinHeaterPower
      GTChillerReport(Num)%Base%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF

  ELSE !Chiller is running so report calculated values
          !set node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp

    IF (GTChiller(Num)%HeatRecActive) THEN
      CALL SafeCopyPlantNode( HeatRecOutletNode, HeatRecInletNode)
      Node(HeatRecOutletNode)%Temp = GTChiller(Num)%HeatRecOutletTemp
    ENDIF

    GTChillerReport(Num)%Base%Power            = Power
    GTChillerReport(Num)%Base%QEvap            = QEvaporator
    GTChillerReport(Num)%Base%QCond            = QCondenser
    GTChillerReport(Num)%Base%Energy           = Energy
    GTChillerReport(Num)%Base%EvapEnergy       = EvaporatorEnergy
    GTChillerReport(Num)%Base%CondEnergy        = CondenserEnergy
    GTChillerReport(Num)%Base%EvapInletTemp    = Node(EvapInletNode)%Temp
    GTChillerReport(Num)%Base%CondInletTemp    = Node(CondInletNode)%Temp
    GTChillerReport(Num)%Base%CondOutletTemp   = Node(CondOutletNode)%Temp
    GTChillerReport(Num)%Base%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    GTChillerReport(Num)%Base%Evapmdot         = EvapMassFlowRate
    GTChillerReport(Num)%Base%Condmdot         = CondMassFlowRate

    GTChillerReport(Num)%HeatRecLubeEnergy    = GTChiller(Num)%HeatRecLubeEnergy
    GTChillerReport(Num)%HeatRecLubeRate      = GTChiller(Num)%HeatRecLubeRate
    GTChillerReport(Num)%FuelEnergyUsedRate   = GTChiller(Num)%FuelEnergyIn
    GTChillerReport(Num)%FuelMassUsedRate     = GTChillerReport(Num)%FuelMassUsedRate
    GTChillerReport(Num)%FuelEnergyUsed       = GTChillerReport(Num)%FuelEnergyUsedRate*TimeStepSys*SecInHour
    GTChillerReport(Num)%FuelMassUsed         = GTChillerReport(Num)%FuelMassUsedRate*TimeStepSys*SecInHour
    GTChillerReport(Num)%ExhaustStackTemp     = GTChiller(Num)%ExhaustStackTemp
    GTChillerReport(Num)%HeatRecInletTemp     = GTChiller(Num)%HeatRecInletTemp
    GTChillerReport(Num)%HeatRecOutletTemp    = GTChiller(Num)%HeatRecOutletTemp
    GTChillerReport(Num)%HeatRecMdot          = GTChiller(Num)%HeatRecMdot
    IF (GTChillerReport(Num)%FuelEnergyUsedRate .NE. 0.0d0) THEN
      GTChillerReport(Num)%FuelCOP              = GTChillerReport(Num)%Base%QEvap/GTChillerReport(Num)%FuelEnergyUsedRate
    ELSE
      GTChillerReport(Num)%FuelCOP              = 0.0d0
    END IF
    IF (GTChiller(Num)%Base%CondenserType == EvapCooled) THEN
      GTChillerReport(Num)%Base%BasinHeaterPower       = BasinHeaterPower
      GTChillerReport(Num)%Base%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF
  END IF
RETURN
END SUBROUTINE UpdateGTChillerRecords

SUBROUTINE UpdateConstCOPChillerRecords(MyLoad,RunFlag,Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    October 1998

            ! PURPOSE OF THIS SUBROUTINE:


            ! METHODOLOGY EMPLOYED:
            ! REFERENCES:

            ! USE STATEMENTS:
USE DataGlobals,     ONLY: SecInHour
USE DataHVACGlobals, ONLY: TimeStepSys

IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64)            :: MyLoad !unused1208
  LOGICAL              :: RunFlag !unused1208
  INTEGER              :: Num

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: EvapInletNode
  INTEGER                  :: EvapOutletNode
  INTEGER                  :: CondInletNode
  INTEGER                  :: CondOutletNode
  REAL(r64)                :: ReportingConstant  ! Number of seconds per HVAC system time step, to convert from W (J/s) to J

  ReportingConstant = TimeStepSys*SecInHour

  EvapInletNode            = ConstCOPChiller(Num)%Base%EvapInletNodeNum
  EvapOutletNode           = ConstCOPChiller(Num)%Base%EvapOutletNodeNum
  CondInletNode            = ConstCOPChiller(Num)%Base%CondInletNodeNum
  CondOutletNode           = ConstCOPChiller(Num)%Base%CondOutletNodeNum


  IF (MyLoad >= 0.d0 .OR. .NOT. RunFlag)THEN !Chiller not running so pass inlet states to outlet states
    ConstCOPChillerReport(Num)%Base%Power            = 0.0d0
    ConstCOPChillerReport(Num)%Base%QEvap            = 0.0d0
    ConstCOPChillerReport(Num)%Base%QCond            = 0.0d0
    ConstCOPChillerReport(Num)%Base%Energy           = 0.0d0
    ConstCOPChillerReport(Num)%Base%EvapEnergy       = 0.0d0
    ConstCOPChillerReport(Num)%Base%CondEnergy       = 0.0d0
    ConstCOPChillerReport(Num)%Base%CondInletTemp    = Node(CondInletNode)%Temp
    ConstCOPChillerReport(Num)%Base%EvapInletTemp    = Node(EvapInletNode)%Temp
    ConstCOPChillerReport(Num)%Base%CondOutletTemp   = Node(CondInletNode)%Temp
    ConstCOPChillerReport(Num)%Base%EvapOutletTemp   = Node(EvapInletNode)%Temp
    ConstCOPChillerReport(Num)%Base%Evapmdot         = EvapMassFlowRate
    ConstCOPChillerReport(Num)%Base%Condmdot         = CondMassFlowRate
    ConstCOPChillerReport(Num)%ActualCOP        = 0.0d0
    IF (ConstCOPChiller(Num)%Base%CondenserType == EvapCooled) THEN
      ConstCOPChillerReport(Num)%Base%BasinHeaterPower       = BasinHeaterPower
      ConstCOPChillerReport(Num)%Base%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF


          !set outlet node temperatures
    Node(EvapOutletNode)%Temp                   = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp                   = Node(CondInletNode)%Temp

  ELSE
    ConstCOPChillerReport(Num)%Base%Power            = Power
    ConstCOPChillerReport(Num)%Base%QEvap            = QEvaporator
    ConstCOPChillerReport(Num)%Base%QCond            = QCondenser
    ConstCOPChillerReport(Num)%Base%Energy           = Energy
    ConstCOPChillerReport(Num)%Base%EvapEnergy       = EvaporatorEnergy
    ConstCOPChillerReport(Num)%Base%CondEnergy       = CondenserEnergy
    ConstCOPChillerReport(Num)%Base%CondInletTemp    = Node(CondInletNode)%Temp
    ConstCOPChillerReport(Num)%Base%EvapInletTemp    = Node(EvapInletNode)%Temp
    ConstCOPChillerReport(Num)%Base%CondOutletTemp   = CondOutletTemp
    ConstCOPChillerReport(Num)%Base%EvapOutletTemp   = EvapOutletTemp
    ConstCOPChillerReport(Num)%Base%Evapmdot         = EvapMassFlowRate
    ConstCOPChillerReport(Num)%Base%Condmdot         = CondMassFlowRate
    IF (Power .ne. 0.0d0) THEN
       ConstCOPChillerReport(Num)%ActualCOP = QEvaporator/Power
    ELSE
       ConstCOPChillerReport(Num)%ActualCOP = 0.0d0
    END IF
    IF (ConstCOPChiller(Num)%Base%CondenserType == EvapCooled) THEN
      ConstCOPChillerReport(Num)%Base%BasinHeaterPower       = BasinHeaterPower
      ConstCOPChillerReport(Num)%Base%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF

          !set outlet node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp
  END IF

RETURN
END SUBROUTINE UpdateConstCOPChillerRecords

! End of Record Keeping subroutines for the Const COP Chiller Module
! *****************************************************************************


END MODULE PlantChillers



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
