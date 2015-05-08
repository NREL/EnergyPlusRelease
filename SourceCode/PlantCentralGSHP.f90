! Contents:
! CentralHeatPumpSystem (CGSHP) System
! ChillerHeaterPerformance:Electric:EIR
!
MODULE PlantCentralGSHP

          ! MODULE INFORMATION:
          !       AUTHOR         PNNL
          !       DATE WRITTEN   Feb 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of the Central Plant GSHP systems
          ! It currently includes one object: ChillerHeaterPerformance:Electric:EIR.
          ! The other object available for this central CGSHP system such as HeatPumpPerformance:WaterToWater:EIR
          !      will be impletemented later.
          !
          ! METHODOLOGY EMPLOYED:
          !  Once the PlantLoopManager determines that the Central Plant GSHP
          !  is available to meet a loop cooling and heating demands, it calls SimCentralGroundSourceHeatPump
          !  which in turn calls the electric PlantCentralGSHP model. The PlantCentralGSHP model is based on
          !  polynomial fits of chiller/heater or heat pump performance data.
          !
          ! REFERENCES:
          !
          ! USE STATEMENTS:

USE DataPrecisionGlobals
USE DataLoopNode
USE DataInterfaces
USE Psychrometrics,  ONLY: PsyCpAirFnWTdb, PsyRhoAirFnPbTdbW
USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
USE DataPlant,       ONLY: PlantLoop

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

PRIVATE
          ! MODULE PARAMETER DEFINITIONS:
! Chiller type parameters: Only water cooled chiller is allowed
INTEGER, PARAMETER :: WaterCooled  = 2
INTEGER, PARAMETER :: SmartMixing  = 1
INTEGER, PARAMETER :: FullyMixed   = 2
LOGICAL, SAVE :: SimulClgDominant = .FALSE.
LOGICAL, SAVE :: SimulHtgDominant = .FALSE.

          ! MODULE VARIABLE DECLARATIONS:
LOGICAL   :: GetInputWrapper   = .TRUE.  ! When TRUE, calls subroutine to read input file.
INTEGER   :: NumWrappers               = 0   ! Number of Wrappers specified in input
INTEGER   :: NumChillerheaters         = 0   ! Number of Chiller/heaters specified in input
REAL(r64) :: CondenserFanPower         = 0.0d0 ! Condenser Fan Power (fan cycles with compressor) [W]
REAL(r64) :: ChillerCapFT              = 0.0d0 ! Chiller/heater capacity fraction (evaluated as a function of temperature)
REAL(r64) :: ChillerEIRFT              = 0.0d0 ! Chiller/heater electric input ratio (EIR = 1 / COP) as a function of temperature
REAL(r64) :: ChillerEIRFPLR            = 0.0d0 ! Chiller/heater EIR as a function of part-load ratio (PLR)
REAL(r64) :: ChillerPartLoadRatio      = 0.0d0 ! Chiller/heater part-load ratio (PLR)
REAL(r64) :: ChillerCyclingRatio       = 0.0d0 ! Chiller/heater cycling ratio
REAL(r64) :: ChillerFalseLoadRate      = 0.0d0 ! Chiller/heater false load over and above the water-side load [W]

  ! Type defining the component specifications
TYPE CGSHPNodeData
   REAL(r64) :: Temp                  = 0.d0 ! {C}
   REAL(r64) :: TempMin               = 0.d0 ! {C}
   REAL(r64) :: TempSetPoint          = 0.d0 ! SensedNodeFlagValue ! {C}
   REAL(r64) :: MassFlowRate          = 0.d0 ! {kg/s}
   REAL(r64) :: MassFlowRateMin       = 0.d0 ! {kg/s}
   REAL(r64) :: MassFlowRateMax       = 0.d0 ! SensedNodeFlagValue ! {kg/s}
   REAL(r64) :: MassFlowRateMinAvail  = 0.d0 ! {kg/s}
   REAL(r64) :: MassFlowRateMaxAvail  = 0.d0 ! {kg/s}
   REAL(r64) :: MassFlowRateSetPoint  = 0.d0 ! {kg/s}
   REAL(r64) :: MassFlowRateRequest   = 0.d0 ! {kg/s}
END TYPE CGSHPNodeData

Type WrapperComponentSpecs
  CHARACTER(len=MaxNameLength) :: WrapperPerformanceObjectType   =''  ! Component type
  CHARACTER(len=MaxNameLength) :: WrapperComponentName           =''  ! Component name
  INTEGER                      :: WrapperPerformanceObjectIndex  = 0  ! Component index in the input array
  CHARACTER(len=MaxNameLength) :: WrapperPerformanceObjectSch    =''  ! Component operation schedule
  INTEGER                      :: WrapperIdenticalObjectNum      = 0  ! Number of identical objects
  INTEGER                      :: CHSchedPtr                     = 0  ! Index to schedule
End TYPE WrapperComponentSpecs

TYPE ChillerheaterSpecs
  CHARACTER(len=MaxNameLength) :: Name            = ' '  ! Name of the Chiller Heater object
  CHARACTER(len=MaxNameLength) :: CondModeCooling = ' '  ! Cooling mode temperature curve input variable
  CHARACTER(len=MaxNameLength) :: CondModeHeating = ' '  ! Clg/Htg mode temperature curve input variable
  CHARACTER(len=MaxNameLength) :: CondMode        = ' '  ! Current mode temperature curve input variable
  LOGICAL   :: ConstantFlow               =.FALSE.  ! True if this is a Constant Flow Chiller
  LOGICAL   :: VariableFlow               =.FALSE.  ! True if this is a Variable Flow Chiller
  LOGICAL   :: CoolSetpointSetToLoop      =.FALSE.  ! True if the setpoint is missing at the outlet node
  LOGICAL   :: HeatSetpointSetToLoop      =.FALSE.  ! True if the setpoint is missing at the outlet node
  LOGICAL   :: CoolSetpointErrDone        =.FALSE.  ! true if setpoint warning issued
  LOGICAL   :: HeatSetpointErrDone        =.FALSE.  ! true if setpoint warning issued
  LOGICAL   :: PossibleSubCooling         = .FALSE. ! flag to indicate chiller is doing less cooling that requested
  INTEGER   :: ChillerHeaterNum             = 1     ! Chiller heater number
  INTEGER   :: CondenserType                = 0     ! Type of Condenser - only water cooled is allowed
  INTEGER   :: ChillerCapFTCooling          = 0     ! Cooling capacity function of temperature curve index
  INTEGER   :: ChillerEIRFTCooling          = 0     ! Elec Input to Cooling Output ratio function of temperature curve index
  INTEGER   :: ChillerEIRFPLRCooling        = 0     ! Elec Input to cooling output ratio function of PLR curve index
  INTEGER   :: ChillerCapFTHeating          = 0     ! Clg/Htg capacity function of temperature curve index
  INTEGER   :: ChillerEIRFTHeating          = 0     ! Elec Input to Clg/Htg Output ratio function of temperature curve index
  INTEGER   :: ChillerEIRFPLRHeating        = 0     ! Elec Input to Clg/Htg output ratio function of PLR curve index
  INTEGER   :: ChillerCapFT                 = 0     ! Capacity function of temperature curve index
  INTEGER   :: ChillerEIRFT                 = 0     ! Elec Input to demand output ratio function of temperature curve index
  INTEGER   :: ChillerEIRFPLR               = 0     ! Elec Input to demand output ratio function of PLR curve index
  INTEGER   :: EvapInletNodeNum             = 0     ! Node number on the inlet side of the plant (evaporator side)
  INTEGER   :: EvapOutletNodeNum            = 0     ! Node number on the outlet side of the plant (evaporator side)
  INTEGER   :: CondInletNodeNum             = 0     ! Node number on the inlet side of the condenser
  INTEGER   :: CondOutletNodeNum            = 0     ! Node number on the outlet side of the condenser
  INTEGER   :: ChillerCapFTError            = 0     ! Used for negative capacity as a function of temp warnings
  INTEGER   :: ChillerCapFTErrorIndex       = 0     ! Used for negative capacity as a function of temp warnings
  INTEGER   :: ChillerEIRFTError            = 0     ! Used for negative EIR as a function of temp warnings
  INTEGER   :: ChillerEIRFTErrorIndex       = 0     ! Used for negative EIR as a function of temp warnings
  INTEGER   :: ChillerEIRFPLRError          = 0     ! Used for negative EIR as a function of PLR warnings
  INTEGER   :: ChillerEIRFPLRErrorIndex     = 0     ! Used for negative EIR as a function of PLR warnings
  INTEGER   :: ChillerEIRRefTempErrorIndex  = 0     ! Used for reference temperature problems
  INTEGER   :: DeltaTErrCount               = 0     ! Evaporator delta T equals 0 for variable flow chiller warning messages
  INTEGER   :: DeltaTErrCountIndex          = 0     ! Index to evaporator delta T = 0 for variable flow chiller warning messages
  INTEGER   :: CondMassFlowIndex            = 0     ! Index to condenser mass flow rate
  REAL(r64) :: RefCapCooling                = 0.0d0   ! Reference cooling-mode evaporator capacity [W]
  REAL(r64) :: RefCOPCooling                = 0.0d0   ! Reference cooling-mode COP
  REAL(r64) :: TempRefEvapOutCooling        = 0.0d0   ! Reference cooling-mode evaporator leaving temperature [C]
  REAL(r64) :: TempRefCondInCooling         = 0.0d0   ! Reference cooling-mode condenser entering temperature [C]
  REAL(r64) :: TempRefCondOutCooling        = 0.0d0   ! Reference cooling-mode condenser leaving temperature [C]
  REAL(r64) :: MaxPartLoadRatCooling        = 0.0d0   ! Maximum Part load ratio in cooling mode
  REAL(r64) :: OptPartLoadRatCooling        = 0.0d0   ! Optimum Part load ratio in cooling mode
  REAL(r64) :: MinPartLoadRatCooling        = 0.0d0   ! minimum Part load ratio in cooling mode
  REAL(r64) :: ClgHtgToCoolingCapRatio      = 0.0d0   ! ratio of clg/htg-mode evaporator capacity to cooling-mode evap. cap
  REAL(r64) :: ClgHtgtoCogPowerRatio        = 0.0d0   ! ratio of clg/htg-mode evaporator power to cooling-mode evap. power
  REAL(r64) :: RefCapClgHtg                 = 0.0d0   ! Reference clg/htg-mode evaporator capacity [W]
  REAL(r64) :: RefCOPClgHtg                 = 0.0d0   ! Reference clg/htg-mode COP
  REAL(r64) :: RefPowerClgHtg               = 0.0d0   ! Reference clg/htg-mode evaporator power [W]
  REAL(r64) :: TempRefEvapOutClgHtg         = 0.0d0   ! Reference clg/htg-mode evaporator leaving temperature [C]
  REAL(r64) :: TempRefCondInClgHtg          = 0.0d0   ! Reference clg/htg-mode condenser entering temperature [C]
  REAL(r64) :: TempRefCondOutClgHtg         = 0.0d0   ! Reference clg/htg-mode condenser leaving temperature [C]
  REAL(r64) :: TempLowLimitEvapOut          = 0.0d0   ! Low temperature shut off [C]
  REAL(r64) :: MaxPartLoadRatClgHtg         = 0.0d0   ! Maximum Part load ratio in simultaneous heating/cooling mode
  REAL(r64) :: OptPartLoadRatClgHtg         = 0.0d0   ! Optimum Part load ratio in simultaneous heating/cooling mode
  REAL(r64) :: MinPartLoadRatClgHtg         = 0.0d0   ! minimum Part load ratio in simultaneous heating/cooling mode

  TYPE (CGSHPNodeData) :: EvapInletNode             ! Chiller heater evaperator inlet node
  TYPE (CGSHPNodeData) :: EvapOutletNode            ! Chiller heater evaperator outlet node
  TYPE (CGSHPNodeData) :: CondInletNode             ! Chiller heater condenser inlet node
  TYPE (CGSHPNodeData) :: CondOutletNode            ! Chiller heater condenser outlet node

  REAL(r64) :: EvapVolFlowRate              = 0.0d0   ! Reference water volumetric flow rate through the evaporator [m3/s]
  REAL(r64) :: tmpEvapVolFlowRate           = 0.0d0   ! temporary ref water vol flow rate for intermediate sizing [m3/s]
  REAL(r64) :: CondVolFlowRate              = 0.0d0   ! Reference water volumetric flow rate through the condenser [m3/s]
  REAL(r64) :: tmpCondVolFlowRate           = 0.0d0   ! temporary ref water vol flow rate for intermediate sizing [m3/s]
  REAL(r64) :: CondMassFlowRateMax          = 0.0d0   ! Reference water mass flow rate through condenser [kg/s]
  REAL(r64) :: EvapMassFlowRateMax          = 0.0d0   ! Reference water mass flow rate through evaporator [kg/s]
  REAL(r64) :: Evapmdot                     = 0.0d0   ! Evaporator mass flow rate [kg/s]
  REAL(r64) :: Condmdot                     = 0.0d0   ! Condenser mass flow rate [kg/s]
  REAL(r64) :: DesignHotWaterVolFlowRate    = 0.0d0   ! Design hot water volumetric flow rate through the condenser [m3/s]
  REAL(r64) :: OpenMotorEff                 = 0.0d0   ! Open chiller motor efficiency [fraction, 0 to 1]
  REAL(r64) :: SizFac                       = 0.0d0   ! sizing factor
  REAL(r64) :: RefCap                       = 0.0d0   ! Reference evaporator capacity [W]
  REAL(r64) :: RefCOP                       = 0.0d0   ! Reference COP
  REAL(r64) :: TempRefEvapOut               = 0.0d0   ! Reference evaporator leaving temperature [C]
  REAL(r64) :: TempRefCondIn                = 0.0d0   ! Reference condenser entering temperature [C]
  REAL(r64) :: TempRefCondOut               = 0.0d0   ! Reference condenser leaving temperature [C]
  REAL(r64) :: OptPartLoadRat               = 0.0d0   ! Optimal operating fraction of full load
  REAL(r64) :: ChillerEIRFPLRMin            = 0.0d0   ! Minimum value of PLR from EIRFPLR curve
  REAL(r64) :: ChillerEIRFPLRMax            = 0.0d0   ! Maximum value of PLR from EIRFPLR curve
END TYPE ChillerheaterSpecs

TYPE CHReportVars
  INTEGER   :: CurrentMode                  = 0     ! 0-off; 1-cooling only; 2-heating-only; 3-simutaneouls heat/cool
  REAL(r64) :: ChillerPartLoadRatio         = 0.0d0   ! Chiller PLR (Load/Capacity)
  REAL(r64) :: ChillerCyclingRatio          = 0.0d0   ! Chiller cycling ratio (time on/time step)
  REAL(r64) :: ChillerFalseLoad             = 0.0d0   ! Chiller false load over and above water side load [J]
  REAL(r64) :: ChillerFalseLoadRate         = 0.0d0   ! Chiller false load rate over and above water side load [W]
  REAL(r64) :: CoolingPower                 = 0.0d0   ! Chiller power, W
  REAL(r64) :: HeatingPower                 = 0.0d0   ! Chiller power, W
  REAL(r64) :: QEvap                        = 0.0d0   ! Evaporator heat transfer rate [W]
  REAL(r64) :: QCond                        = 0.0d0   ! Condenser heat transfer rate [W]
  REAL(r64) :: CoolingEnergy                = 0.0d0   ! Chiller electric consumption [J]
  REAL(r64) :: HeatingEnergy                = 0.0d0   ! Chiller electric consumption [J]
  REAL(r64) :: EvapEnergy                   = 0.0d0   ! Evaporator heat transfer energy [J]
  REAL(r64) :: CondEnergy                   = 0.0d0   ! Condenser heat transfer energy [J]
  REAL(r64) :: CondInletTemp                = 0.0d0   ! Condenser inlet temperature [C]
  REAL(r64) :: EvapInletTemp                = 0.0d0   ! Evaporator inlet temperature [C]
  REAL(r64) :: CondOutletTemp               = 0.0d0   ! Condenser outlet temperature [C]
  REAL(r64) :: EvapOutletTemp               = 0.0d0   ! Evaporator outlet temperature [C]
  REAL(r64) :: Evapmdot                     = 0.0d0   ! Evaporator mass flow rate [kg/s]
  REAL(r64) :: Condmdot                     = 0.0d0   ! Condenser mass flow rate [kg/s]
  REAL(r64) :: ActualCOP                    = 0.0d0   ! Coefficient of performance
  REAL(r64) :: ChillerCapFT                 = 0.0d0   ! Chiller capacity curve output value
  REAL(r64) :: ChillerEIRFT                 = 0.0d0   ! Chiller EIRFT curve output value
  REAL(r64) :: ChillerEIRFPLR               = 0.0d0   ! Chiller EIRFPLR curve output value
  REAL(r64) :: CondenserFanPowerUse         = 0.0d0   ! Air-cooled condenser fan power [W]
  REAL(r64) :: CondenserFanEnergy           = 0.0d0   ! Air-cooled condenser fan energy [J]
  REAL(r64) :: CondenserFanEnergyConsumption= 0.0d0   ! ""Should be checked"" For now, leave it
  REAL(r64) :: ChillerPartLoadRatioSimul    = 0.0d0   ! Chiller PLR (Load/Capacity) for simul clg/htg mode
  REAL(r64) :: ChillerCyclingRatioSimul     = 0.0d0   ! Chiller cycling ratio (time on/time step) for simul clg/htg mode
  REAL(r64) :: ChillerFalseLoadSimul        = 0.0d0   ! Chiller false load for simul clg/htg mode [J]
  REAL(r64) :: ChillerFalseLoadRateSimul    = 0.0d0   ! Chiller false load rate for simul clg/htg mode [W]
  REAL(r64) :: CoolingPowerSimul            = 0.0d0   ! Chiller power for simul clg/htg mode [W]
  REAL(r64) :: QEvapSimul                   = 0.0d0   ! Evaporator heat transfer rate for simul clg/htg mode [W]
  REAL(r64) :: QCondSimul                   = 0.0d0   ! Evaporator heat transfer rate for simul clg/htg mode [W]
  REAL(r64) :: CoolingEnergySimul           = 0.0d0   ! Chiller electric consumption for simul clg/htg mode [J]
  REAL(r64) :: EvapEnergySimul              = 0.0d0   ! Evaporator heat transfer energy for simul clg/htg mode [J]
  REAL(r64) :: CondEnergySimul              = 0.0d0   ! Condenser heat transfer energy for simul clg/htg mode [J]
  REAL(r64) :: EvapInletTempSimul           = 0.0d0   ! Evaporator inlet temperature for simul clg/htg mode [C]
  REAL(r64) :: EvapOutletTempSimul          = 0.0d0   ! Evaporator outlet temperature for simul clg/htg mode [C]
  REAL(r64) :: EvapmdotSimul                = 0.0d0   ! Evaporator mass flow rate for simul clg/htg mode [kg/s]
  REAL(r64) :: CondInletTempSimul           = 0.0d0   ! Condenser inlet temperature for simul clg/htg mode [C]
  REAL(r64) :: CondOutletTempSimul          = 0.0d0   ! Condenser outlet temperature for simul clg/htg mode [C]
  REAL(r64) :: CondmdotSimul                = 0.0d0   ! Condenser mass flow rate for simul clg/htg mode [kg/s]
  REAL(r64) :: ChillerCapFTSimul            = 0.0d0   ! Chiller capacity curve output value for simul clg/htg mode
  REAL(r64) :: ChillerEIRFTSimul            = 0.0d0   ! Chiller EIRFT curve output value for simul clg/htg mode
  REAL(r64) :: ChillerEIRFPLRSimul          = 0.0d0   ! Chiller EIRFPLR curve output value for simul clg/htg mode
END TYPE CHReportVars

TYPE WrapperSpecs  ! This will be used for Wrapper Object. This object will decide the mode of Chiller
  CHARACTER(len=MaxNameLength) :: Name                        = ' ' ! User identifier
  CHARACTER(len=MaxNameLength) :: AncilliaryPwSchedule        =''   ! Ancilliary Power Schedule Name
  LOGICAL   :: VariableFlowCH = .FALSE.      ! True if all chiller heters are variable flow control
  INTEGER   :: SchedPtr                = 0   ! Schedule value for ancilliar power control
  INTEGER   :: CHSchedPtr              = 0   ! Schedule value for individual chiller heater control
  INTEGER   :: ControlMode             = 0   ! SmartMixing or FullyMixing
  INTEGER   :: CHWInletNodeNum         = 0   ! Node number on the inlet side of the plant (Chilled Water side)
  INTEGER   :: CHWOutletNodeNum        = 0   ! Node number on the outlet side of the plant (Chilled Water side)
  INTEGER   :: HWInletNodeNum          = 0   ! Node number on the inlet side of the plant (Hot Water side)
  INTEGER   :: HWOutletNodeNum         = 0   ! Node number on the outlet side of the plant (Hot Water side)
  INTEGER   :: GLHEInletNodeNum        = 0   ! Node number on the inlet side of the plant (GLHE Water side)
  INTEGER   :: GLHEOutletNodeNum       = 0   ! Node number on the outlet side of the plant (GLHE Water side)
  INTEGER   :: NumOfComp               = 0   ! Number of Components under the wrapper
  REAL(r64) :: CHWMassFlowRate         = 0.0d0 ! Chilled water mass flow rate
  REAL(r64) :: HWMassFlowRate          = 0.0d0 ! Hot water mass flow rate
  REAL(r64) :: GLHEMassFlowRate        = 0.0d0 ! Condenser water mass flow rate
  REAL(r64) :: CHWMassFlowRateMax      = 0.0d0 ! Maximum chilled water mass flow rate
  REAL(r64) :: HWMassFlowRateMax       = 0.0d0 ! Maximum hot water mass flow rate
  REAL(r64) :: GLHEMassFlowRateMax     = 0.0d0 ! Maximum condenser water mass flow rate
  REAL(r64) :: WrapperCoolingLoad      = 0.0d0 ! Cooling demand for the central heat pump system
  REAL(r64) :: WrapperHeatingLoad      = 0.0d0 ! Heating demand for the central heat pump system
  REAL(r64) :: AncilliaryPower         = 0.0d0 ! Wrapper Ancilliary Power

  Type (WrapperComponentSpecs), ALLOCATABLE, DIMENSION(:)  :: WrapperComp
  TYPE (ChillerheaterSpecs),    ALLOCATABLE, DIMENSION(:)  :: Chillerheater       ! Dimension to number of machines
  TYPE (CHReportVars),          ALLOCATABLE, DIMENSION(:)  :: ChillerheaterReport ! Dimension to number of machines

  LOGICAL   :: CoolSetpointErrDone     =.FALSE. ! true if setpoint warning issued
  LOGICAL   :: HeatSetpointErrDone     =.FALSE. ! true if setpoint warning issued
  LOGICAL   :: CoolSetpointSetToLoop   =.FALSE. ! True if the setpoint is missing at the outlet node
  LOGICAL   :: HeatSetpointSetToLoop   =.FALSE. ! True if the setpoint is missing at the outlet node
  INTEGER   :: ChillerHeaterNums          ! Total number of chiller heater units
  INTEGER   :: CWLoopNum          = 0     ! Chilled water plant loop index number
  INTEGER   :: CWLoopSideNum      = 0     ! Chilled water plant loop side index
  INTEGER   :: CWBranchNum        = 0     ! Chilled water plant loop branch index
  INTEGER   :: CWCompNum          = 0     ! Chilled water plant loop component index
  INTEGER   :: HWLoopNum          = 0     ! Hot water plant loop index number
  INTEGER   :: HWLoopSideNum      = 0     ! Hot water plant loop side index
  INTEGER   :: HWBranchNum        = 0     ! Hot water plant loop branch index
  INTEGER   :: HWCompNum          = 0     ! Hot water plant loop component index
  INTEGER   :: GLHELoopNum        = 0     ! Geo-field water plant loop index number
  INTEGER   :: GLHELoopSideNum    = 0     ! Geo-field water plant loop side index
  INTEGER   :: GLHEBranchNum      = 0     ! Geo-field water plant loop branch index
  INTEGER   :: GLHECompNum        = 0     ! Geo-field water plant loop component index
  INTEGER   :: CHWMassFlowIndex   = 0     ! Chilled water flow index
  INTEGER   :: HWMassFlowIndex    = 0     ! Hot water flow index
  INTEGER   :: GLHEMassFlowIndex  = 0     ! Condenser side flow index
  REAL(r64) :: SizingFactor       = 1.0d0   ! Sizing factor to adjust the capacity
  REAL(r64) :: CHWVolFlowRate     = 0.0d0   ! Chilled water volume flow rate [kg/s]
  REAL(r64) :: HWVolFlowRate      = 0.0d0   ! Hot water volume flow rate [kg/s]
  REAL(r64) :: GLHEVolFlowRate    = 0.0d0   ! Geo-field volume flow rate [kg/s]
END TYPE WrapperSpecs

TYPE WrapperReportVars
  REAL(r64) :: Power                   = 0.0d0   ! Wrapper power, W
  REAL(r64) :: QCHW                    = 0.0d0   ! Chilled water heat transfer rate [W]
  REAL(r64) :: QHW                     = 0.0d0   ! Hot Water heat transfer rate [W]
  REAL(r64) :: QGLHE                   = 0.0d0   ! Geo-field heat transfer rate [W]
  REAL(r64) :: TotElecCooling          = 0.0d0   ! Wrapper cooling electric consumption [J]
  REAL(r64) :: TotElecHeating          = 0.0d0   ! Wrapper heating electric consumption [J]
  REAL(r64) :: CoolingEnergy           = 0.0d0   ! Chilled water heat transfer energy [J]
  REAL(r64) :: HeatingEnergy           = 0.0d0   ! Hot Water heat transfer energy [J]
  REAL(r64) :: GLHEEnergy              = 0.0d0   ! Geo-field heat transfer energy [J]
  REAL(r64) :: TotElecCoolingPwr       = 0.0d0   ! Wrapper cooling electric consumption rate [W]
  REAL(r64) :: TotElecHeatingPwr       = 0.0d0   ! Wrapper heating electric consumption rate [W]
  REAL(r64) :: CoolingRate             = 0.0d0   ! Chilled water heat transfer rate [W]
  REAL(r64) :: HeatingRate             = 0.0d0   ! Hot Water heat transfer rate [W]
  REAL(r64) :: GLHERate                = 0.0d0   ! Geo-field heat transfer rate [W]
  REAL(r64) :: CHWInletTemp            = 0.0d0   ! Chilled water inlet temperature [C]
  REAL(r64) :: HWInletTemp             = 0.0d0   ! Hot water inlet temperature [C]
  REAL(r64) :: GLHEInletTemp           = 0.0d0   ! Geo-field inlet temperature [C]
  REAL(r64) :: CHWOutletTemp           = 0.0d0   ! Chilled water Outlet temperature [C]
  REAL(r64) :: HWOutletTemp            = 0.0d0   ! Hot water Outlet temperature [C]
  REAL(r64) :: GLHEOutletTemp          = 0.0d0   ! Geo-field Outlet temperature [C]
  REAL(r64) :: CHWmdot                 = 0.0d0   ! Chilled water mass flow rate [kg/s]
  REAL(r64) :: HWmdot                  = 0.0d0   ! Hot water mass flow rate [kg/s]
  REAL(r64) :: GLHEmdot                = 0.0d0   ! Geo-field mass flow rate [kg/s]
  REAL(r64) :: TotElecCoolingSimul     = 0.0d0   ! Wrapper cooling electric consumption [J]
  REAL(r64) :: CoolingEnergySimul      = 0.0d0   ! Chilled water heat transfer energy [J]
  REAL(r64) :: TotElecCoolingPwrSimul  = 0.0d0   ! Wrapper cooling electric consumption rate [W]
  REAL(r64) :: CoolingRateSimul        = 0.0d0   ! Chilled water heat transfer rate [W]
  REAL(r64) :: CHWInletTempSimul       = 0.0d0   ! Chilled water inlet temperature [C]
  REAL(r64) :: GLHEInletTempSimul      = 0.0d0   ! Geo-field inlet temperature [C]
  REAL(r64) :: CHWOutletTempSimul      = 0.0d0   ! Chilled water Outlet temperature [C]
  REAL(r64) :: GLHEOutletTempSimul     = 0.0d0   ! Geo-field Outlet temperature [C]
  REAL(r64) :: CHWmdotSimul            = 0.0d0   ! Chilled water mass flow rate [kg/s]
  REAL(r64) :: GLHEmdotSimul           = 0.0d0   ! Geo-field mass flow rate [kg/s]
END TYPE WrapperReportVars

TYPE (WrapperSpecs),       ALLOCATABLE, DIMENSION(:)  :: Wrapper
TYPE (ChillerheaterSpecs), ALLOCATABLE, DIMENSION(:)  :: Chillerheater
TYPE(CHReportVars), ALLOCATABLE, DIMENSION(:) ::ChillerheaterReport
TYPE(WrapperReportVars), ALLOCATABLE, DIMENSION(:) ::WrapperReport

LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CHCheckEquipName
LOGICAL, ALLOCATABLE, DIMENSION(:) :: HPCheckEquipName

  ! SUBROUTINE SPECIFICATIONS FOR MODULE ChillerElectricEIR
PUBLIC     SimCentralGroundSourceHeatPump
PRIVATE    GetWrapperInput
PRIVATE    GetChillerheaterInput
PRIVATE    SizeWrapper
PRIVATE    InitWrapper
PRIVATE    CalcWrapperModel
PRIVATE    CalcChillerModel
PRIVATE    CalcChillerHeaterModel
PRIVATE    UpdateChillerRecords
PRIVATE    UpdateChillerheaterRecords

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Chiller/Heater Module Driver Subroutine
!*************************************************************************
SUBROUTINE SimCentralGroundSourceHeatPump(WrapperName,EquipFlowCtrl, CompIndex,LoopNum, RunFlag,FirstIteration, &
                              InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)

  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, FindItemInList
  USE DataIPShortCuts
  USE CurveManager,          ONLY: GetCurveIndex
  USE CurveManager,          ONLY: CurveValue
  USE DataPlant,             ONLY: TypeOf_CentralGroundSourceHeatPump
  USE DataSizing,            ONLY: CurLoopNum
  USE DataGlobals,           ONLY: Outputfiledebug, DayOfsim, HourOfDay, Warmupflag
  USE General,               ONLY: TrimSigDigits, RoundSigDigits
  USE PlantUtilities,        ONLY: UpdateChillerComponentCondenserSide
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: WrapperName      ! User specified name of wrapper
  INTEGER, INTENT(IN)          :: EquipFlowCtrl    ! Flow control mode for the equipment
  INTEGER, INTENT(INOUT)       :: CompIndex        ! Chiller number pointer
  INTEGER, INTENT(IN)          :: LoopNum          ! plant loop index pointer
  LOGICAL, INTENT(IN)          :: RunFlag          ! Simulate chiller when TRUE
  LOGICAL, INTENT(IN)          :: FirstIteration   ! Initialize variables when TRUE
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip    ! If not zero, calculate the max load for operating conditions
  LOGICAL, INTENT(IN)          :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(INOUT)     :: MyLoad           ! Loop demand component will meet [W]
  REAL(r64), INTENT(OUT)       :: MaxCap           ! Maximum operating capacity of chiller [W]
  REAL(r64), INTENT(OUT)       :: MinCap           ! Minimum operating capacity of chiller [W]
  REAL(r64), INTENT(OUT)       :: OptCap           ! Optimal operating capacity of chiller [W]
  REAL(r64), INTENT(INOUT)     :: SizingFactor     ! sizing factor
  LOGICAL    :: SimulCoolingDominant = .FALSE.     ! Simultaneous clg/htg mode - cooling dominant
  LOGICAL    :: SimulCoolingHeating = .FALSE.      ! Simultaneous clg/htg mode - heating dominant
  INTEGER    :: OpMode                             ! Operation mode
  INTEGER    :: WrapperNum                         ! Wrapper number pointer
  INTEGER    :: NumChillerHeater                   ! Chiller heater number pointer
  INTEGER    :: LoopSide                           ! Plant loop side
  INTEGER    :: ChillerHeaterNum                   ! Chiller heater number
  REAL(r64)  :: SimulLoadRatio                     ! Cooling/heating ratio to determine a load domination

    ! Get user input values
  IF (GetInputWrapper) THEN
    CALL GetWrapperInput
    GetInputWrapper = .FALSE.
  END IF

    ! Find the correct wrapper
  IF (CompIndex == 0) THEN
    WrapperNum = FindItemInList(WrapperName,Wrapper%Name,NumWrappers)
    IF (WrapperNum == 0) THEN
      CALL ShowFatalError('SimCentralGroundSourceHeatPump: Specified Wrapper not one of Valid Wrappers='//TRIM(WrapperName))
    ENDIF
    CompIndex=WrapperNum
  ELSE
    WrapperNum=CompIndex
    IF (WrapperNum > NumWrappers .or. WrapperNum < 1) THEN
      CALL ShowFatalError('SimCentralGroundSourceHeatPump:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(WrapperNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumWrappers))//  &
                          ', Entered Unit name='//TRIM(WrapperName))
    ENDIF
    IF (CheckEquipName(WrapperNum)) THEN
      IF (WrapperName /= Wrapper(WrapperNum)%Name) THEN
        CALL ShowFatalError('SimCentralGroundSourceHeatPump:  Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(WrapperNum))// &
                            ', Unit name='//TRIM(WrapperName)//', stored Unit Name for that index='//  &
                            TRIM(Wrapper(WrapperNum)%Name))
      ENDIF
      CheckEquipName(WrapperNum)=.false.
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN ! Initializagion loop if not done
    CALL InitWrapper(WrapperNum,RunFlag,FirstIteration,MyLoad,LoopNum)
        MinCap = 0.0d0
        MaxCap = 0.0d0
        OptCap = 0.0d0
    IF (LoopNum == Wrapper(WrapperNum)%CWLoopNum) THEN ! Chilled water loop
      IF (Wrapper(WrapperNum)%ControlMode == SmartMixing) THEN ! control mode is SmartMixing
        DO NumChillerHeater = 1 , Wrapper(WrapperNum)%ChillerHeaterNums
           MaxCap = Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%RefCapCooling * &
                    Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%MaxPartLoadRatCooling + MaxCap

           OptCap = Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%RefCapCooling * &
                    Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%OptPartLoadRatCooling + OptCap

           MinCap = Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%RefCapCooling * &
                    Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%MinPartLoadRatCooling + MinCap
        END DO
      END IF
    ELSE IF (LoopNum == Wrapper(WrapperNum)%HWLoopNum ) THEN ! Hot water loop
      IF (Wrapper(WrapperNum)%ControlMode == SmartMixing) THEN ! control mode is SmartMixing
        DO NumChillerHeater = 1 , Wrapper(WrapperNum)%ChillerHeaterNums
          MaxCap = Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%RefCapClgHtg * &
                       Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%MaxPartLoadRatClgHtg + MaxCap

          OptCap = Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%RefCapClgHtg * &
                      Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%OptPartLoadRatClgHtg + OptCap

          MinCap = Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%RefCapClgHtg * &
                      Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%MinPartLoadRatClgHtg + MinCap
        END DO
      END IF  ! End of control mode determination
    END IF ! End of loop determination

    IF (GetSizingFactor) THEN
        SizingFactor = 1.0d0  ! Always equal to one now. The conponent may have its own sizing factor
    END IF

    RETURN

  ENDIF ! End of initialization

  IF (LoopNum /= Wrapper(WrapperNum)%GLHELoopNum) THEN

    CALL InitWrapper(WrapperNum,RunFlag,FirstIteration,MyLoad,LoopNum)
    CALL CalcWrapperModel(WrapperNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,LoopNum)

  ELSE IF (LoopNum == Wrapper(WrapperNum)%GLHELoopNum) THEN
    LoopSide = Wrapper(WrapperNum)%GLHELoopSideNum
    CALL UpdateChillerComponentCondenserSide(LoopNum, LoopSide, TypeOf_CentralGroundSourceHeatPump, &
                               Wrapper(WrapperNum)%GLHEInletNodeNum,     &
                               Wrapper(WrapperNum)%GLHEOutletNodeNum,    &
                               WrapperReport(WrapperNum)%GLHERate,     &
                               WrapperReport(WrapperNum)%GLHEInletTemp,  &
                               WrapperReport(WrapperNum)%GLHEOutletTemp, &
                               WrapperReport(WrapperNum)%GLHEmdot, FirstIteration)

      ! Use the first chiller heater's evaporator capacity ratio to determine dominant load
    SimulClgDominant = .FALSE.
    SimulHtgDominant = .FALSE.  
    IF (Wrapper(WrapperNum)%WrapperCoolingLoad > 0 .AND. Wrapper(WrapperNum)%WrapperHeatingLoad > 0) THEN
      SimulLoadRatio = Wrapper(WrapperNum)%WrapperCoolingLoad / Wrapper(WrapperNum)%WrapperHeatingLoad
      IF (SimulLoadRatio > Wrapper(WrapperNum)%Chillerheater(1)%ClgHtgToCoolingCapRatio) THEN
          SimulClgDominant = .TRUE.
          SimulHtgDominant = .FALSE.
      ELSE
         SimulHtgDominant = .TRUE.
         SimulClgDominant = .FALSE.
      END IF
    END IF

  END IF

  RETURN

END SUBROUTINE SimCentralGroundSourceHeatPump

SUBROUTINE SizeWrapper(WrapperNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Yunzhi Huang, PNNL
          !       DATE WRITTEN   Feb 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine is for sizing all the components under each 'CentralHeatPumpSystem' object,
          !  for which capacities and flow rates have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          !  Obtains evaporator flow rate from the plant sizing array. Calculates reference capacity from
          !  the evaporator (or load side) flow rate and the chilled water loop design delta T. The condenser
          !  flow (or sourse side) rate is calculated from the reference capacity, the COP, and the condenser
          !  loop design delta T.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,           ONLY: PlantSizesOkayToFinalize
  USE PlantUtilities,      ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataHVACGlobals,     ONLY: SmallWaterVolFlow
  USE DataGlobals,         ONLY: InitConvTemp
  USE OutputReportPredefined

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WrapperNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizNum            ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum        ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound          ! If errors detected in input
  LOGICAL             :: LoopErrorsFound      !
  LOGICAL             :: errFlag              ! error flag for node connection
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           :: rho
  REAL(r64)           :: Cp
  REAL(r64)           :: tmpNomCap            ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate   ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate   ! local condenser design volume flow rate
  REAL(r64)           :: tmpLoadVolFlowRate   ! local load design volume flow rate
  REAL(r64)           :: tmpSourceVolFlowRate ! local source design volume flow rate
  INTEGER             :: NumChillerHeater     ! Number of Chiller heater pointer
  INTEGER             :: CHWInletNodeNum      ! Chilled water inlet node index number
  INTEGER             :: CHWOutletNodeNum     ! Chilled water outlet node index number
  INTEGER             :: GLHEInletNodeNum     ! Geo-field water inlet node index number
  INTEGER             :: GLHEOutletNodeNum    ! Geo-field water outlet node index number
  INTEGER             :: HWInletNodeNum       ! Hot water inlet node index number
  INTEGER             :: HWOutletNodeNum      ! Hot water outlet node index number
  INTEGER             :: EvapInletNode        ! Chiller heater evaporator side inlet node index number
  INTEGER             :: EvapOutletNode       ! Chiller heater evaporator side outlet node index number
  INTEGER             :: CondInletNode        ! Chiller heater condenser side inlet node index number
  INTEGER             :: CondOutletNode       ! Chiller heater condenser side outlet node index number
  INTEGER             :: LoadSideInletNode    ! Heat pump load side inlet node index number
  INTEGER             :: LoadSideOutletNode   ! Heat pump load side outlet node index number
  INTEGER             :: SourceSideInletNode  ! Heat pump source side inlet node index number
  INTEGER             :: SourceSideOutletNode ! Heat pump source side outlet node index number
  INTEGER             :: DummyInletNode       ! Dummy inlet node index number
  INTEGER             :: DummyOutletNode      ! Dummy outlet node index number
  REAL(r64)           :: TotalEvapVolFlowRate
  REAL(r64)           :: TotalCondVolFlowRate
  REAL(r64)           :: TotalHotWaterVolFlowRate

    ! get all the nodes' indices
  CHWInletNodeNum   = Wrapper(WrapperNum)%CHWInletNodeNum
  CHWOutletNodeNum  = Wrapper(WrapperNum)%CHWOutletNodeNum
  GLHEInletNodeNum  = Wrapper(WrapperNum)%GLHEInletNodeNum
  GLHEOutletNodeNum = Wrapper(WrapperNum)%GLHEOutletNodeNum
  HWInletNodeNum    = Wrapper(WrapperNum)%HWInletNodeNum
  HWOutletNodeNum   = Wrapper(WrapperNum)%HWOutletNodeNum

    ! auto-size the chiller heater components
  IF (Wrapper(WrapperNum)%ControlMode == SmartMixing) THEN
    DO NumChillerHeater = 1 , Wrapper(WrapperNum)%ChillerHeaterNums
      PltSizNum = 0
      PltSizCondNum = 0
      ErrorsFound = .FALSE.

        ! find the appropriate Plant Sizing object
      PltSizNum = PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%PlantSizNum

      IF (Wrapper(WrapperNum)%Chillerheater(NumChillerHeater)%CondVolFlowRate == AutoSize) THEN
          PltSizCondNum = PlantLoop(Wrapper(WrapperNum)%GLHELoopNum)%PlantSizNum
      END IF

      tmpNomCap          = Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCapCooling
      tmpEvapVolFlowRate = Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%EvapVolFlowRate
      tmpCondVolFlowRate = Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%CondVolFlowRate

        ! auto-size the Evaporator Flow Rate
      IF (Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%EvapVolFlowRate == AutoSize) THEN
        IF (PltSizNum > 0) THEN
          IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
            tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate *   &
                                 Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%SizFac
            Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%tmpEvapVolFlowRate = tmpEvapVolFlowRate
            IF (PlantSizesOkayToFinalize) Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%EvapVolFlowRate = &
              tmpEvapVolFlowRate
          ELSE
            tmpEvapVolFlowRate = 0.d0
            Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%tmpEvapVolFlowRate = tmpEvapVolFlowRate
            IF (PlantSizesOkayToFinalize) Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%EvapVolFlowRate = &
               tmpEvapVolFlowRate
          END IF
          IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('ChillerHeaterPerformance:Electric:EIR', &
                                   Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%Name, &
                                   'Reference Chilled Water Flow Rate [m3/s]', &
                                   Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%EvapVolFlowRate)
        ELSE
          CALL ShowSevereError('Autosizing of CGSHP Chiller Heater evap flow rate requires a loop Sizing:Plant object')
          CALL ShowContinueError('Occurs in CGSHP Chiller Heater Performance object='// &
               TRIM(Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%Name))
          ErrorsFound = .TRUE.
        END IF
      END IF

         ! auto-size the Reference Cooling Capacity
         ! each individual chiller heater module is sized to be capable of supporting the total load on the wrapper
      IF (Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCapCooling  == AutoSize) THEN
        IF (PltSizNum > 0) THEN
          IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
            Cp  = GetSpecificHeatGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                                 InitConvTemp,                      &
                                 PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex, &
                                 'SizeCGSHPChillerHeater')

            rho  = GetDensityGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex,&
                                'SizeCGSHPChillerHeater')
            tmpNomCap =  Cp * rho * PlantSizData(PltSizNum)%DeltaT  * tmpEvapVolFlowRate
            IF (PlantSizesOkayToFinalize) Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCapCooling = tmpNomCap
          ELSE
            tmpNomCap = 0.d0
            IF (PlantSizesOkayToFinalize) Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCapCooling = tmpNomCap
          END IF
          IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('ChillerHeaterPerformance:Electric:EIR', &
                             Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%Name, &
                             'Reference Capacity [W]', &
                             Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCapCooling)
        ELSE
          CALL ShowSevereError('SizeExhaustAbsorber: ChillerHeaterPerformance:Electric:EIR="'//  &
             trim(Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%Name)//'", autosize error.')
          CALL ShowContinueError('Autosizing of CGSHP Chiller Heater reference capacity requires')
          CALL ShowContinueError('a cooling loop Sizing:Plant object.')
          ErrorsFound = .TRUE.
        END IF
        Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCapClgHtg = &
                                Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCapCooling * &
                                Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%ClgHtgToCoolingCapRatio
      END IF

        ! auto-size the condenser volume flow rate
        ! each individule chiller heater module is sized to be capable of supporting the total load on the wrapper
      IF (Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%CondVolFlowRate == AutoSize) THEN
        IF (PltSizCondNum > 0) THEN
          IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
            rho  = GetDensityGlycol(PlantLoop(Wrapper(WrapperNum)%GLHELoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(Wrapper(WrapperNum)%GLHELoopNum)%FluidIndex,&
                                'SizeCGSHPChillerHeater')
            Cp  = GetSpecificHeatGlycol(PlantLoop(Wrapper(WrapperNum)%GLHELoopNum)%FluidName, &
                                Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%TempRefCondIn, &
                                PlantLoop(Wrapper(WrapperNum)%GLHELoopNum)%FluidIndex, &
                                'SizeCGSHPChillerHeater')
            tmpCondVolFlowRate = tmpNomCap * &
                        (1.0d0 + (1.0d0/Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCOPCooling) * &
                        Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%OpenMotorEff) / &
                       ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
            Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%tmpCondVolFlowRate = tmpCondVolFlowRate
            IF (PlantSizesOkayToFinalize) Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%CondVolFlowRate = &
               tmpCondVolFlowRate
          ELSE
            tmpCondVolFlowRate = 0.d0
            Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%tmpCondVolFlowRate = tmpCondVolFlowRate
            IF (PlantSizesOkayToFinalize) Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%CondVolFlowRate = &
               tmpCondVolFlowRate
          END IF
          IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('ChillerHeaterPerformance:Electric:EIR', &
                              Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%Name, &
                              'Reference Condenser Water Flow Rate [m3/s]', &
                              Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%CondVolFlowRate)
        ELSE
          CALL ShowSevereError('SizeExhaustAbsorber: ChillerHeaterPerformance:Electric:EIR="'//  &
             trim(Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%Name)//'", autosize error.')
          CALL ShowContinueError('Autosizing of CGSHP Chiller Heater condenser flow rate requires')
          CALL ShowContinueError('a condenser loop Sizing:Plant object.')
          ErrorsFound = .TRUE.
        END IF
      END IF


      IF (PlantSizesOkayToFinalize) THEN
            !create predefined report
        equipName = Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%Name
        CALL PreDefTableEntry(pdchMechType,equipName,'ChillerHeaterPerformance:Electric:EIR')
        CALL PreDefTableEntry(pdchMechNomEff,equipName,Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCOPCooling)
        CALL PreDefTableEntry(pdchMechNomCap,equipName,Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%RefCapCooling)
      END IF

      IF (ErrorsFound) THEN
        CALL ShowFatalError('Preceding sizing errors cause program termination')
      END IF

    END DO

    ! sum individual volume flows and register wrapper inlets
    TotalEvapVolFlowRate = 0.d0
    TotalCondVolFlowRate = 0.d0
    TotalHotWaterVolFlowRate = 0.d0
    DO NumChillerHeater = 1 , Wrapper(WrapperNum)%ChillerHeaterNums
      TotalEvapVolFlowRate = TotalEvapVolFlowRate + Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%tmpEvapVolFlowRate
      TotalCondVolFlowRate = TotalCondVolFlowRate + Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%tmpCondVolFlowRate
      TotalHotWaterVolFlowRate = TotalHotWaterVolFlowRate &
                             + Wrapper(WrapperNum)%ChillerHeater(NumChillerHeater)%DesignHotWaterVolFlowRate
    ENDDO

    CALL RegisterPlantCompDesignFlow(Wrapper(WrapperNum)%CHWInletNodeNum,TotalEvapVolFlowRate)
    CALL RegisterPlantCompDesignFlow(Wrapper(WrapperNum)%HWInletNodeNum,TotalHotWaterVolFlowRate)
        ! save the reference condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
    CALL RegisterPlantCompDesignFlow(Wrapper(WrapperNum)%GLHEInletNodeNum,TotalCondVolFlowRate)

    RETURN

  END IF

END SUBROUTINE SizeWrapper

SUBROUTINE GetWrapperInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Yunzhi Huang and Daeho Kang, PNNL
            !       DATE WRITTEN:    Feb 2013

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will get the input required by the Wrapper model.

            ! METHODOLOGY EMPLOYED:
            !

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString,FindItemInList
  USE DataIPShortCuts
  USE BranchNodeConnections, ONLY: TestCompSet,SetUpCompSets
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE CurveManager,          ONLY: GetCurveIndex
  USE DataGlobals,           ONLY: ScheduleAlwaysOn
  USE CurveManager,          ONLY: CurveValue
  USE ScheduleManager,       ONLY: GetScheduleIndex
  USE DataSizing,            ONLY: Autosize
  USE InputProcessor,        ONLY: MakeUPPERCase
  USE General,               ONLY: TrimSigDigits, RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! PARAMETERS
            ! na

            ! LOCAL VARIABLES
  CHARACTER(len=MaxNameLength) :: CompName = ''             ! component name
  CHARACTER(len=MaxNameLength) :: temp_char                 ! temporary character variable
  CHARACTER(len=MaxNameLength) :: temp_char1                ! temporary character variable
  CHARACTER(len=MaxNameLength) :: temp_char2                ! temporary character variable
  CHARACTER(len=MaxNameLength) :: EvapInletNodeName         ! virtual evaporator inlet node name for chiller heater
  CHARACTER(len=MaxNameLength) :: EvapOutletNodeName        ! virtual evaporator outlet node name for chiller heater
  CHARACTER(len=MaxNameLength) :: CondInletNodeName         ! virtual condenser inlet node name for chiller heater
  CHARACTER(len=MaxNameLength) :: CondOutletNodeName        ! virtual condenser outlet node name for chiller heater
  CHARACTER(len=MaxNameLength) :: LoadsideInletNodeName     ! virtual load inlet node name for heat pump
  CHARACTER(len=MaxNameLength) :: LoadsideOutletNodeName    ! virtual load outlet node name for heat pump
  CHARACTER(len=MaxNameLength) :: SourcesideInletNodeName   ! virtual source inlet node name for heat pump
  CHARACTER(len=MaxNameLength) :: SourcesideOutletNodeName  ! virtual source outlet node name for heat pump
  CHARACTER(len=MaxNameLength) :: DummyInletNodeName        ! virtual dummy inlet node name
  CHARACTER(len=MaxNameLength) :: DummyOutletNodeName       ! virtual dummy inlet node name
  LOGICAL, SAVE  :: ErrorsFound          = .false. ! True when input errors are found
  LOGICAL        :: IsNotOK                        ! Flag to verify name
  LOGICAL        :: IsBlank                        ! Flag for blank name
  LOGICAL, SAVE  :: AllocatedFlag        = .FALSE. ! True when arrays are allocated
  LOGICAL, SAVE  :: CHAllocatedFlag      = .FALSE. ! True when arrays are allocated
  LOGICAL, SAVE  :: HPAllocatedFlag      = .FALSE. ! True when arrays are allocated
  LOGICAL, SAVE  :: CHDEAllocatedFlag    = .FALSE. ! True when arrays are allocated
  LOGICAL, SAVE  :: HPDEAllocatedFlag    = .FALSE. ! True when arrays are allocated
  INTEGER :: NumAlphas                 ! Number of elements in the alpha array
  INTEGER :: NumNums                   ! Number of elements in the numeric array
  INTEGER :: IOStat                    ! IO Status when calling get input subroutine
  INTEGER :: i_CH                      ! chiller heater index pointer
  INTEGER :: i_HP                      ! heat pump index pointer
  INTEGER :: WrapperNum           = 0  ! wrapper number
  INTEGER :: NumberOfComp         = 0  ! number of components under each wrapper
  INTEGER :: comp                 = 0  ! an index number for input all the components
  INTEGER :: loop                 = 0  ! an index number for read in all the parameters of a component
  INTEGER :: CompIndex            = 0  ! component index in the sequence of internal input array
  INTEGER :: NumCHFound           = 0  ! number of Chiller heaters found in internal array
  INTEGER :: NumHPFound           = 0  ! number of heat pump found in the internal array
  INTEGER :: TotalNumCH           = 0  ! total number of chiller heaters (with identical multiplier)
  INTEGER :: TotalNumHP           = 0  ! total number of heat pumps (with identical multiplier)
  INTEGER :: NumChillerheaters    = 0  ! total number of chiller heater (without identical multiplier)
  INTEGER :: ChillerHeaterNum     = 1  ! chiller heater index pointer for current wrapper object
  INTEGER :: HeatPumpNum          = 1  ! heat pump index pointer
  INTEGER :: NumChHtrPerWrapper   = 0  ! total number of chiller heaters (including identical units) per wrapper

  IF (AllocatedFlag) RETURN
    cCurrentModuleObject = 'CentralHeatPumpSystem'
    NumWrappers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumWrappers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  END IF

    ! ALLOCATE ARRAYS
  ALLOCATE (Wrapper(NumWrappers))
  ALLOCATE (WrapperReport(NumWrappers))
  ALLOCATE (CheckEquipName(NumWrappers))
  CheckEquipName = .TRUE.
  AllocatedFlag  = .TRUE.

    ! Load arrays with electric EIR chiller data
  DO WrapperNum = 1 , NumWrappers
    CALL GetObjectItem(cCurrentModuleObject,WrapperNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    Wrapper(WrapperNum)%Name = cAlphaArgs(1)

    ! intialize nth chiller heater index (including identical units) for current wrapper
    NumChHtrPerWrapper = 0

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),Wrapper%Name,WrapperNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')

    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
      CYCLE
    END IF

    IF (cAlphaArgs(2) == 'SMARTMIXING') THEN
      Wrapper(WrapperNum)%ControlMode         = SmartMixing
    END IF

    Wrapper(WrapperNum)%CHWInletNodeNum    = &  !node name : connection should be careful!
              GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
              NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    Wrapper(WrapperNum)%CHWOutletNodeNum   = &
              GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
              NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Chilled Water Nodes')

    Wrapper(WrapperNum)%GLHEInletNodeNum    = &  !node name : connection should be careful!
              GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
              NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
    Wrapper(WrapperNum)%GLHEOutletNodeNum   = &
              GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
              NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'GLHE Nodes')

    Wrapper(WrapperNum)%HWInletNodeNum    = &  !node name : connection should be careful!
              GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
              NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
    Wrapper(WrapperNum)%HWOutletNodeNum   = &
              GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
              NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(7),cAlphaArgs(8),'Hot Water Nodes')

    Wrapper(WrapperNum)%AncilliaryPower      = rNumericArgs(1)
    Wrapper(WrapperNum)%AncilliaryPwSchedule = cAlphaArgs(9)
    IF (lAlphaFieldBlanks(9)) THEN
      Wrapper(WrapperNum)%SchedPtr  = 0
    ELSE
      Wrapper(WrapperNum)%SchedPtr  = GetScheduleIndex(cAlphaArgs(9))
    END IF

    NumberOfComp = (NumAlphas-9)/3
    Wrapper(WrapperNum)%NumOfComp = NumberOfComp
    ALLOCATE(Wrapper(WrapperNum)%WrapperComp(NumberOfComp))

    IF (Wrapper(WrapperNum)%NumOfComp == 0) THEN
      CALL ShowSevereError('GetWrapperInput: No component names on '//  &
          TRIM(cCurrentModuleObject)//'='//TRIM(Wrapper(WrapperNum)%Name))
      ErrorsFound=.true.
    ELSE
      Comp=0
      DO Loop=10,NumAlphas,3
        Comp = Comp + 1
        Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperPerformanceObjectType=cAlphaArgs(Loop)
        Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperComponentName=cAlphaArgs(Loop+1)
        Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperPerformanceObjectSch=cAlphaArgs(Loop+2)
          IF (lAlphaFieldBlanks(Loop+2)) THEN
              Wrapper(WrapperNum)%WrapperComp(Comp)%CHSchedPtr = ScheduleAlwaysOn
          ELSE
              Wrapper(WrapperNum)%WrapperComp(Comp)%CHSchedPtr = GetScheduleIndex(cAlphaArgs(Loop+2))
          END IF
        Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperIdenticalObjectNum=rNumericArgs(1+Comp)
        IF (Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperPerformanceObjectType == &
          MakeUPPERCase('ChillerHeaterPerformance:Electric:EIR')) THEN

          ! count number of chiller heaters (including identical units) for current wrapper
          IF (Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperIdenticalObjectNum > 1) THEN
              NumChHtrPerWrapper = NumChHtrPerWrapper+Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperIdenticalObjectNum
          ELSE
              NumChHtrPerWrapper = NumChHtrPerWrapper+1
          END IF

          ! count total number of chiller heaters (not including identical units) for ALL wrappers
          NumChillerheaters = NumChillerheaters + 1

        END IF
      END DO

      Wrapper(WrapperNum)%ChillerHeaterNums = NumChHtrPerWrapper
    END IF

    IF (ErrorsFound) THEN
      CALL ShowFatalError('GetWrapperInput: Invalid '//TRIM(cCurrentModuleObject)//  &
             ' Input, preceding condition(s) cause termination.')
    ENDIF

      ! ALLOCATE ARRAYS
    IF ((NumChillerheaters == 0) .AND. (Wrapper(WrapperNum)%ControlMode == SmartMixing)) THEN
      CALL ShowFatalError('SmartMixing Control Mode in object '//TRIM(cCurrentModuleObject)//' : ' &
        //TRIM(Wrapper(WrapperNum)%Name)//' need to apply to ChillerHeaterPerformance:Electric:EIR object(s).')
    ENDIF

  END DO

  IF (NumChillerHeaters>0) THEN
    IF (CHAllocatedFlag) RETURN

      DO WrapperNum = 1 , NumWrappers
        ALLOCATE (Wrapper(WrapperNum)%Chillerheater(Wrapper(WrapperNum)%ChillerHeaterNums))
        ALLOCATE (Wrapper(WrapperNum)%ChillerheaterReport(Wrapper(WrapperNum)%ChillerHeaterNums))
      END DO
        CALL GetChillerheaterInput
        CHAllocatedFlag = .TRUE.
  END IF

  DO WrapperNum = 1 , NumWrappers
    ChillerHeaterNum = 0  ! intialize nth chiller heater index (including identical units) for current wrapper
    DO comp=1, Wrapper(WrapperNum)%NumOfComp
      IF (Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperPerformanceObjectType == &
                            MakeUPPERCase('ChillerHeaterPerformance:Electric:EIR')) THEN
        CompName=Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperComponentName
        CompIndex = FindItemInList(CompName,Chillerheater%Name,UBound(Chillerheater%Name,1) )
          ! User may enter invalid name rather than selecting one from the object list
        IF (CompIndex <= 0) THEN
          CALL ShowSevereError('GetWrapperInput: Invalid Chiller Heater Modules Performance Component Name =' //TRIM(compname))
          CALL ShowContinueError('Select the name of ChillerHeaterPerformance:Electric:EIR object(s) from the object list.')
          CALL ShowFatalError('Program terminates due to preceding condition.')
        ENDIF
        Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperPerformanceObjectIndex=CompIndex
        IF (Chillerheater(CompIndex)%VariableFlow) THEN
          Wrapper(WrapperNum)%VariableFlowCH = .TRUE.
        END IF
        DO i_CH=1 , Wrapper(WrapperNum)%WrapperComp(Comp)%WrapperIdenticalObjectNum
          ! increment nth chiller heater index (including identical units) for current wrapper
          ChillerHeaterNum = ChillerHeaterNum+1
          Wrapper(WrapperNum)%ChillerHeater(ChillerHeaterNum)       = Chillerheater(CompIndex)
          Wrapper(WrapperNum)%ChillerHeaterReport(ChillerHeaterNum) = ChillerheaterReport(CompIndex)
        END DO
      ENDIF
    END DO
  END DO

    !Release memory from temporary arrays; values now copied into their associated Wrapper in above loop
  IF (ALLOCATED(Chillerheater)) DEALLOCATE(Chillerheater)
  IF (ALLOCATED(ChillerheaterReport)) DEALLOCATE(ChillerheaterReport)

    !Set up output variables
  DO WrapperNum = 1 , NumWrappers
    CALL SetupOutputVariable('Chiller Heater System Cooling Electric Energy [J]', &
          WrapperReport(WrapperNum)%TotElecCooling,'System','Sum',Wrapper(WrapperNum)%Name,  &
                                ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater System Heating Electric Energy [J]', &
          WrapperReport(WrapperNum)%TotElecHeating,'System','Sum',Wrapper(WrapperNum)%Name,  &
                                ResourceTypeKey='ELECTRICITY',EndUseKey='Heating',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater System Cooling Electric Power [W]', &
      WrapperReport(WrapperNum)%TotElecCoolingPwr, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Heating Electric Power [W]', &
          WrapperReport(WrapperNum)%TotElecHeatingPwr,'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Cooling Energy [J]', &
          WrapperReport(WrapperNum)%CoolingEnergy, 'System','Sum',Wrapper(WrapperNum)%Name,  &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater System Heating Energy [J]', &
          WrapperReport(WrapperNum)%HeatingEnergy,'System','Sum',Wrapper(WrapperNum)%Name,  &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BOILER',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater System Source Heat Transfer Energy [J]', &
          WrapperReport(WrapperNum)%GLHEEnergy,'System','Sum',Wrapper(WrapperNum)%Name,  &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')

    CALL SetupOutputVariable('Chiller Heater System Cooling Rate [W]', &
          WrapperReport(WrapperNum)%CoolingRate, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Heating Rate [W]', &
          WrapperReport(WrapperNum)%HeatingRate,'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Source Heat Transfer Rate [W]', &
          WrapperReport(WrapperNum)%GLHERate,'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Cooling Mass Flow Rate [kg/s]', &
          WrapperReport(WrapperNum)%CHWmdot, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Heating Mass Flow Rate [kg/s]', &
          WrapperReport(WrapperNum)%HWmdot, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Source Mass Flow Rate [kg/s]', &
          WrapperReport(WrapperNum)%GLHEmdot, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Cooling Inlet Temperature [C]', &
          WrapperReport(WrapperNum)%CHWInletTemp, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Heating Inlet Temperature [C]', &
          WrapperReport(WrapperNum)%HWInletTemp, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Source Inlet Temperature [C]', &
          WrapperReport(WrapperNum)%GLHEInletTemp, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Cooling Outlet Temperature [C]', &
          WrapperReport(WrapperNum)%CHWOutletTemp, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Heating Outlet Temperature [C]', &
          WrapperReport(WrapperNum)%HWOutletTemp, 'System','Average',Wrapper(WrapperNum)%Name)

    CALL SetupOutputVariable('Chiller Heater System Source Outlet Temperature [C]', &
          WrapperReport(WrapperNum)%GLHEOutletTemp, 'System','Average',Wrapper(WrapperNum)%Name)

    IF (Wrapper(WrapperNum)%ChillerHeaterNums > 0) THEN

      DO ChillerHeaterNum = 1, Wrapper(WrapperNum)%ChillerHeaterNums

        CALL SetupOutputVariable('Chiller Heater Operation Mode Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' []', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

        CALL SetupOutputVariable('Chiller Heater Part Load Ratio Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' []', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerPartLoadRatio, &
            'System','Average',Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

        CALL SetupOutputVariable('Chiller Heater Cycling Ratio Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' []', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCyclingRatio, &
            'System','Average',Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

        CALL SetupOutputVariable('Chiller Heater Cooling Electric Power Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [W]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPower,'System','Average', &
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

        CALL SetupOutputVariable('Chiller Heater Heating Electric Power Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [W]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower,'System','Average', &
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

        CALL SetupOutputVariable('Chiller Heater Cooling Electric Energy Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [J]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingEnergy,'System','Sum', &
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

        CALL SetupOutputVariable('Chiller Heater Heating Electric Energy Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [J]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingEnergy,'System','Sum', &
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

        CALL SetupOutputVariable('Chiller Heater Cooling Rate Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [W]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap,'System','Average', &
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Cooling Energy Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [J]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergy,'System','Sum', &
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater False Load Heat Transfer Rate Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [W]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadRate, &
            'System','Average',Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater False Load Heat Transfer Energy Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [J]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoad,'System','Sum',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Evaporator Inlet Temperature Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [C]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapInletTemp,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Evaporator Outlet Temperature Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [C]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTemp,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Evaporator Mass Flow Rate Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [kg/s]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Condenser Heat Transfer Rate Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [W]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Condenser Heat Transfer Energy Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [J]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergy,'System','Sum',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater COP Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [W/W]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ActualCOP,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Capacity Temperature Modifier Multiplier Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' []', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCapFT,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater EIR Temperature Modifier Multiplier Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' []', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFT,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater EIR Part Load Modifier Multiplier Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' []', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFPLR,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Condenser Inlet Temperature Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [C]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondInletTemp,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Condenser Outlet Temperature Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [C]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTemp,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)

       CALL SetupOutputVariable('Chiller Heater Condenser Mass Flow Rate Unit '//  &
            TRIM(TrimSigDigits(ChillerHeaterNum))//' [kg/s]', &
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot,'System','Average',&
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)
      END DO ! End of individual chiller heater count for current wrapper

    END IF ! End of individual chiller heater output

  END DO ! End of wrapper count

  RETURN

END SUBROUTINE GetWrapperInput


SUBROUTINE GetChillerheaterInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Kyung Tae Yun, Mississippi State University
            !       DATE WRITTEN:    Feb 2013

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will get the input required by the ChillerHeaterPerformance:Electric:EIR model.

            ! METHODOLOGY EMPLOYED:
            !

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE DataIPShortCuts
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE CurveManager,          ONLY: GetCurveIndex, GetCurveMinMaxValues
  USE CurveManager,          ONLY: CurveValue
  USE PlantUtilities,        ONLY: RegisterPlantCompDesignFlow
  USE ScheduleManager,       ONLY: GetScheduleIndex
  USE General,               ONLY: TrimSigDigits, RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! PARAMETERS
            ! na

            ! LOCAL VARIABLES
  CHARACTER(len=MaxNameLength)  :: EvapInletNodeName       ! Evaporator inlet node name
  CHARACTER(len=MaxNameLength)  :: EvapOutletNodeName      ! Evaporator outlet node name
  CHARACTER(len=MaxNameLength)  :: CondInletNodeName       ! Condenser inlet node name
  CHARACTER(len=MaxNameLength)  :: CondOutletNodeName      ! Condenser outlet node name
  CHARACTER(len=MaxNameLength)  :: temp_char               ! temporary character variable
  CHARACTER(len=MaxNameLength)  :: StringVar               ! Used for EIRFPLR warning messages
  LOGICAL, SAVE                 :: CHErrorsFound=.false.   ! True when input errors are found
  LOGICAL, SAVE                 :: AllocatedFlag =.FALSE.  ! True when arrays are allocated
  LOGICAL                       :: IsNotOK                 ! Flag to verify name
  LOGICAL                       :: IsBlank                 ! Flag for blank name
  LOGICAL                       :: FoundNegValue = .FALSE. ! Used to evaluate PLFFPLR curve objects
  LOGICAL                       :: errflag                 ! Used to tell if a unique chiller name has been specified
  INTEGER                       :: CurveValPtr             ! Index to EIRFPLR curve output
  INTEGER                       :: CurveCheck = 0          ! Used to evaluate PLFFPLR curve objects
  INTEGER                       :: ChillerHeaterNum        ! Chiller counter
  INTEGER                       :: NumAlphas               ! Number of elements in the alpha array
  INTEGER                       :: NumNums                 ! Number of elements in the numeric array
  INTEGER                       :: IOStat                  ! IO Status when calling get input subroutine
  REAL(r64)                     :: CurveVal                ! Used to verify EIR-FT and CAP-FT curves
  REAL(r64), DIMENSION(11)      :: CurveValArray           ! Used to evaluate PLFFPLR curve objects
  REAL(r64)                     :: CurveValTmp             ! Used to evaluate PLFFPLR curve objects

  cCurrentModuleObject = 'ChillerHeaterPerformance:Electric:EIR'
  NumChillerheaters = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumChillerheaters <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    CHErrorsFound=.true.
  END IF

      ! Allocate temporary Chillerheater and ChillerheaterReport arrays
  IF (ALLOCATED(Chillerheater)) DEALLOCATE (Chillerheater)
  IF (ALLOCATED(ChillerheaterReport)) DEALLOCATE (ChillerheaterReport)
  ALLOCATE (Chillerheater(NumChillerheaters))
  ALLOCATE (ChillerheaterReport(NumChillerheaters))

      ! Load arrays with electric EIR chiller data
  DO ChillerHeaterNum = 1 , NumChillerheaters
    CALL GetObjectItem(cCurrentModuleObject,ChillerHeaterNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    Chillerheater(ChillerHeaterNum)%Name = cAlphaArgs(1)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),Chillerheater%Name,ChillerHeaterNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      CHErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    END IF

    Chillerheater(ChillerHeaterNum)%CondModeCooling = cAlphaArgs(4)

      ! Performance curves
    Chillerheater(ChillerHeaterNum)%ChillerCapFTCooling = GetCurveIndex(cAlphaArgs(5))
    IF (Chillerheater(ChillerHeaterNum)%ChillerCapFTCooling == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
      CHErrorsFound = .TRUE.
    END IF

    Chillerheater(ChillerHeaterNum)%ChillerEIRFTCooling = GetCurveIndex(cAlphaArgs(6))
    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFTCooling == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
      CHErrorsFound = .TRUE.
    END IF

    Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRCooling = GetCurveIndex(cAlphaArgs(7))
    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRCooling == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
      CHErrorsFound = .TRUE.
    END IF

    Chillerheater(ChillerHeaterNum)%CondModeHeating = cAlphaArgs(8)

      ! Performance curves
    Chillerheater(ChillerHeaterNum)%ChillerCapFTHeating = GetCurveIndex(cAlphaArgs(9))
    IF (Chillerheater(ChillerHeaterNum)%ChillerCapFTHeating .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
      CHErrorsFound = .TRUE.
    END IF

    Chillerheater(ChillerHeaterNum)%ChillerEIRFTHeating = GetCurveIndex(cAlphaArgs(10))
    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFTHeating .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
      CHErrorsFound = .TRUE.
    END IF

    Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRHeating = GetCurveIndex(cAlphaArgs(11))
    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRHeating .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cAlphaFieldNames(11))//'='//TRIM(cAlphaArgs(11)))
      CHErrorsFound = .TRUE.
    END IF

    IF(cAlphaArgs(2) == 'CONSTANTFLOW') THEN
       Chillerheater(ChillerHeaterNum)%ConstantFlow = .True.
       Chillerheater(ChillerHeaterNum)%VariableFlow = .False.
    ELSEIF(cAlphaArgs(2) == 'VARIABLEFLOW') THEN
       Chillerheater(ChillerHeaterNum)%ConstantFlow = .False.
       Chillerheater(ChillerHeaterNum)%VariableFlow = .True.
    ELSE  ! Assume a constant flow chiller if none is specified
       Chillerheater(ChillerHeaterNum)%ConstantFlow = .True.
       Chillerheater(ChillerHeaterNum)%VariableFlow = .False.
       CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
       CALL ShowContinueError('Entered in '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
       CALL ShowContinueError('simulation assumes CONSTANTFLOW and continues..')
    END IF

    IF(ChillerHeaterNum > 1) THEN
      IF(Chillerheater(ChillerHeaterNum)%ConstantFlow .NEQV. Chillerheater(ChillerHeaterNum-1)%ConstantFlow) THEN
         Chillerheater(ChillerHeaterNum)%ConstantFlow = .True.
      CALL ShowWarningError('Water flow mode is different from the other chiller heater(s) ' &
                                         //TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
         CALL ShowContinueError('Entered in '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
         CALL ShowContinueError('Simulation assumes CONSTANTFLOW and continues..')
      END IF
    END IF

    IF (SameString(cAlphaArgs(3),'WaterCooled')) THEN
        Chillerheater(ChillerHeaterNum)%CondenserType = WaterCooled
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
        CALL ShowContinueError('Valid entries is WaterCooled')
        CHErrorsFound=.TRUE.
    END IF

      ! Chiller rated performance data
    Chillerheater(ChillerHeaterNum)%RefCapCooling = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CHErrorsFound=.true.
    END IF
    Chillerheater(ChillerHeaterNum)%RefCOPCooling = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      CHErrorsFound=.true.
    END IF

    Chillerheater(ChillerHeaterNum)%TempRefEvapOutCooling = rNumericArgs(3)
    Chillerheater(ChillerHeaterNum)%TempRefCondInCooling  = rNumericArgs(4)
    Chillerheater(ChillerHeaterNum)%TempRefCondOutCooling = rNumericArgs(5)
    Chillerheater(ChillerHeaterNum)%ClgHtgToCoolingCapRatio = rNumericArgs(6)
    Chillerheater(ChillerHeaterNum)%RefCapClgHtg = rNumericArgs(6) * Chillerheater(ChillerHeaterNum)%RefCapCooling

    IF (rNumericArgs(6) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cNumericFieldNames(6))//'='//TRIM(RoundSigDigits(rNumericArgs(6),2)))
      CHErrorsFound=.true.
    END IF

    Chillerheater(ChillerHeaterNum)%ClgHtgtoCogPowerRatio = rNumericArgs(7)
    Chillerheater(ChillerHeaterNum)%RefPowerClgHtg = Chillerheater(ChillerHeaterNum)%RefCapCooling  / &
                                                     Chillerheater(ChillerHeaterNum)%RefCOPCooling * rNumericArgs(7)
    Chillerheater(ChillerHeaterNum)%RefCOPClgHtg = Chillerheater(ChillerHeaterNum)%RefCapClgHtg / &
                                                   Chillerheater(ChillerHeaterNum)%RefPowerClgHtg

    IF (rNumericArgs(7) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Entered in '//TRIM(cNumericFieldNames(7))//'='//TRIM(RoundSigDigits(rNumericArgs(7),2)))
      CHErrorsFound=.true.
    END IF

    Chillerheater(ChillerHeaterNum)%TempRefEvapOutClgHtg      = rNumericArgs(8)
    Chillerheater(ChillerHeaterNum)%TempRefCondOutClgHtg      = rNumericArgs(9)
    Chillerheater(ChillerHeaterNum)%TempRefCondInClgHtg       = rNumericArgs(10)
    Chillerheater(ChillerHeaterNum)%TempLowLimitEvapOut       = rNumericArgs(11)
    Chillerheater(ChillerHeaterNum)%EvapVolFlowRate           = rNumericArgs(12)
    Chillerheater(ChillerHeaterNum)%CondVolFlowRate           = rNumericArgs(13)
    Chillerheater(ChillerHeaterNum)%DesignHotWaterVolFlowRate = rNumericArgs(14)
    Chillerheater(ChillerHeaterNum)%OpenMotorEff              = rNumericArgs(15)
    Chillerheater(ChillerHeaterNum)%OptPartLoadRatCooling     = rNumericArgs(16)
    Chillerheater(ChillerHeaterNum)%OptPartLoadRatClgHtg      = rNumericArgs(17)
    Chillerheater(ChillerHeaterNum)%SizFac                    = rNumericArgs(18)


    IF (Chillerheater(ChillerHeaterNum)%SizFac <= 0.0d0) Chillerheater(ChillerHeaterNum)%SizFac = 1.0d0

    IF(Chillerheater(ChillerHeaterNum)%OpenMotorEff .LT. 0.0d0 .OR. &
       Chillerheater(ChillerHeaterNum)%OpenMotorEff .GT. 1.0d0) THEN
       CALL ShowSevereError('GetCurveInput: For '//TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaArgs(1)))
       CALL ShowContinueError(TRIM(cNumericFieldNames(14))//' = '//TRIM(RoundSigDigits(rNumericArgs(14),3)) )
       CALL ShowContinueError(TRIM(cNumericFieldNames(14))//' must be greater than or equal to zero' )
       CALL ShowContinueError(TRIM(cNumericFieldNames(14))//' must be less than or equal to one' )
       CHErrorsFound=.true.
    END IF

      ! Check the CAP-FT, EIR-FT, and PLR curves and warn user if different from 1.0 by more than +-10%
    IF (Chillerheater(ChillerHeaterNum)%ChillerCAPFTCooling > 0) THEN
      CurveVal = CurveValue(Chillerheater(ChillerHeaterNum)%ChillerCAPFTCooling, &
                          Chillerheater(ChillerHeaterNum)%TempRefEvapOutCooling, &
                          Chillerheater(ChillerHeaterNum)%TempRefCondInCooling)
      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError('Capacity ratio as a function of temperature curve output is not equal to 1.0')
        CALL ShowContinueError('(+ or - 10%) at reference conditions for '//TRIM(cCurrentModuleObject)//'= '//  &
                        TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
    END IF

    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFTCooling > 0) THEN
      CurveVal = CurveValue(Chillerheater(ChillerHeaterNum)%ChillerEIRFTCooling, &
                          Chillerheater(ChillerHeaterNum)%TempRefEvapOutCooling, &
                          Chillerheater(ChillerHeaterNum)%TempRefCondInCooling)
      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError('Energy input ratio as a function of temperature curve output is not equal to 1.0')
        CALL ShowContinueError('(+ or - 10%) at reference conditions for '//TRIM(cCurrentModuleObject)//'= '//  &
                        TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
    END IF

    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRCooling > 0) THEN
      CurveVal = CurveValue(Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRCooling, 1.0d0)

      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError('Energy input ratio as a function of part-load ratio curve output is not equal to 1.0')
        CALL ShowContinueError('(+ or - 10%) at reference conditions for '//TRIM(cCurrentModuleObject)//'= '//  &
                        TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
    END IF

    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRCooling > 0) THEN
      FoundNegValue = .FALSE.
      DO CurveCheck = 0, 10, 1
        CurveValTmp = CurveValue(Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRCooling, REAL(CurveCheck/10.0d0,r64))
        IF(CurveValTmp .LT. 0.0d0) FoundNegValue = .TRUE.
        CurveValArray(CurveCheck+1) = INT(CurveValTmp*100.0d0)/100.0d0
      END DO
      IF(FoundNegValue)THEN
        CALL ShowWarningError('Energy input ratio as a function of part-load ratio curve shows negative values ')
        CALL ShowContinueError('for '//TRIM(cCurrentModuleObject)//'= '//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('EIR as a function of PLR curve output at various part-load ratios shown below:')
        CALL ShowContinueError('PLR   =  0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00')
        WRITE(StringVar,530)(CurveValArray(CurveValPtr), CurveValPtr = 1, 11)
530     FORMAT('Curve Output = ',11(F7.2))
        CALL ShowContinueError(TRIM(StringVar))
        CHErrorsFound = .TRUE.
      END IF
    END IF

    IF (Chillerheater(ChillerHeaterNum)%ChillerCAPFTHeating > 0) THEN
      CurveVal = CurveValue(Chillerheater(ChillerHeaterNum)%ChillerCAPFTHeating, &
                          Chillerheater(ChillerHeaterNum)%TempRefEvapOutClgHtg,Chillerheater(ChillerHeaterNum)%TempRefCondInClgHtg)
      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError('Capacity ratio as a function of temperature curve output is not equal to 1.0')
        CALL ShowContinueError('(+ or - 10%) at reference conditions for '//TRIM(cCurrentModuleObject)//'= '//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
    END IF

    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFTHeating > 0) THEN
      CurveVal = CurveValue(Chillerheater(ChillerHeaterNum)%ChillerEIRFTHeating, &
                          Chillerheater(ChillerHeaterNum)%TempRefEvapOutClgHtg,Chillerheater(ChillerHeaterNum)%TempRefCondInClgHtg)
      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError('Energy input ratio as a function of temperature curve output is not equal to 1.0')
        CALL ShowContinueError('(+ or - 10%) at reference conditions for '//TRIM(cCurrentModuleObject)//'= '//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
    END IF

    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRHeating > 0) THEN
      CurveVal = CurveValue(Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRHeating, 1.0d0)

      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError('Energy input ratio as a function of part-load ratio curve output is not equal to 1.0')
        CALL ShowContinueError('(+ or - 10%) at reference conditions for '//TRIM(cCurrentModuleObject)//'= '//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
    END IF

    IF (Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRHeating > 0) THEN
      FoundNegValue = .FALSE.
      DO CurveCheck = 0, 10, 1
        CurveValTmp = CurveValue(Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRHeating, REAL(CurveCheck/10.0d0,r64))
        IF(CurveValTmp .LT. 0.0d0) FoundNegValue = .TRUE.
        CurveValArray(CurveCheck+1) = INT(CurveValTmp*100.0d0)/100.0d0
      END DO
      IF(FoundNegValue)THEN
        CALL ShowWarningError('Energy input ratio as a function of part-load ratio curve shows negative values ')
        CALL ShowContinueError('for '//TRIM(cCurrentModuleObject)//'= '//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('EIR as a function of PLR curve output at various part-load ratios shown below:')
        CALL ShowContinueError('PLR          =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00')
        WRITE(StringVar,550)(CurveValArray(CurveValPtr), CurveValPtr = 1, 11)
550     FORMAT('Curve Output = ',11(F7.2))
        CALL ShowContinueError(TRIM(StringVar))
        CHErrorsFound = .TRUE.
      END IF
    END IF

    CALL GetCurveMinMaxValues(Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRHeating,&
                              Chillerheater(ChillerHeaterNum)%MinPartLoadRatClgHtg, &
                              Chillerheater(ChillerHeaterNum)%MaxPartLoadRatClgHtg)

    CALL GetCurveMinMaxValues(Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRCooling,&
                              Chillerheater(ChillerHeaterNum)%MinPartLoadRatCooling, &
                              Chillerheater(ChillerHeaterNum)%MaxPartLoadRatCooling)

  END DO

  IF (CHErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  END IF

  RETURN

END SUBROUTINE GetChillerheaterInput


SUBROUTINE InitWrapper(WrapperNum,RunFlag,FirstIteration,myLoad,LoopNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang, PNNL
          !       DATE WRITTEN   Feb 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine is for initializations of the CentralHeatPumpSystem variables

          ! METHODOLOGY EMPLOYED:
          !  Uses the status flags to trigger initializations.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel, DayOfSim, &
                              HourOfDay, NumOfTimeStepInHour, outputfiledebug, InitConvTemp
  USE DataPlant,       ONLY : PlantLoop, TypeOf_CentralGroundSourceHeatPump, ScanPlantLoopsForObject, &
                              PlantSizesOkayToFinalize, PlantSizeNotComplete, LoopFlowStatus_NeedyIfLoopOn
  USE InputProcessor,  ONLY : SameString
  USE DataEnvironment, ONLY : StdBaroPress
  USE Psychrometrics,  ONLY : PsyRhoAirFnPbTdbW
  USE CurveManager,    ONLY : GetCurveMinMaxValues
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: WrapperNum       ! Number of the current wrapper being simulated
  LOGICAL, INTENT(IN)   :: RunFlag          ! TRUE when chiller operating
  LOGICAL, INTENT(IN)   :: FirstIteration   ! Initialize variables when TRUE
  REAL(r64), INTENT(IN) :: MyLoad           ! Demand Load
  INTEGER, INTENT(IN)   :: LoopNum          ! Loop Number Index

    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na

    ! INTERFACE BLOCK SPECIFICATIONS:
    !  na

    ! DERIVED TYPE DEFINITIONS:
    !  na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL,SAVE                             :: MyWrapperOneTimeFlag = .true. ! Flag used to execute code only once
    LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyWrapperFlag                 ! TRUE in order to set component location
    LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyWrapperEnvrnFlag            ! TRUE when new environment is started
    INTEGER      :: WrapperCondInletNode   ! Node number for condenser water inlet node
    INTEGER      :: WrapperCondOutletNode  ! Node number for condenser water outlet node
    INTEGER      :: HeatRecInNode          ! Node number for heat recovery water inlet node
    INTEGER      :: HeatRecOutNode         ! Node number for heat recovery water outlet node
    INTEGER      :: Comp                   ! component index
    INTEGER      :: ChillerHeaterNum       ! Chiller Heater index
    INTEGER      :: GSHeatPumpNum          ! Ground Source Heat Pump index
    LOGICAL      :: errFlag                ! Err flag
    LOGICAL      :: FatalError             ! Fatal error indicator
    INTEGER      :: CHWInletNodeNum        ! Chilled water inlet node number
    INTEGER      :: CHWOutletNodeNum       ! Chilled water outlet node number
    INTEGER      :: HWInletNodeNum         ! Hot water inlet node number
    INTEGER      :: HWOutletNodeNum        ! Hot water outlet node number
    INTEGER      :: GLHEInletNodeNum       ! Condenser water inlet node number
    INTEGER      :: GLHEOutletNodeNum      ! Condenser water outlet node number
    REAL(r64)    :: rho                    ! local fluid density
    REAL(r64)    :: mdotCHW                ! Chilled water mass flow rate
    REAL(r64)    :: mdotHW                 ! Hot water mass flow rate
    REAL(r64)    :: mdotGLHE               ! Condenser water mass flow rate
    REAL(r64)    :: CHWMassFlowRateMax     ! Maximum chilled water mass flow rate
    REAL(r64)    :: HWMassFlowRateMax      ! Maximum hot water mass flow rate
    REAL(r64)    :: GLHEMassFlowRateMax    ! Maximum condenser water mass flow rate
    REAL(r64)    :: mdotCHWAvail           ! Maximum available chillled water mass flow rate
    REAL(r64)    :: mdotHWAvail            ! Maximum available hot water mass flow ratre
    REAL(r64)    :: mdotGLHEAvail          ! Maximum available condenser mass flow rate

    ! Do the one time initializations
    IF (MyWrapperOneTimeFlag) THEN
        ALLOCATE(MyWrapperEnvrnFlag(NumWrappers))
        ALLOCATE(MyWrapperFlag(NumWrappers))
        MyWrapperEnvrnFlag = .TRUE.
        MyWrapperFlag = .TRUE.
        MyWrapperOneTimeFlag = .FALSE.
    END IF

  IF (MyWrapperFlag(WrapperNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(Wrapper(WrapperNum)%Name, &
                             TypeOf_CentralGroundSourceHeatPump, &
                             Wrapper(WrapperNum)%CWLoopNum, &
                             Wrapper(WrapperNum)%CWLoopSideNum, &
                             Wrapper(WrapperNum)%CWBranchNum,&
                             Wrapper(WrapperNum)%CWCompNum, &
                             InletNodeNumber = Wrapper(WrapperNum)%CHWInletNodeNum, &
                             errFlag=errFlag)

    CALL ScanPlantLoopsForObject(Wrapper(WrapperNum)%Name, &
                             TypeOf_CentralGroundSourceHeatPump, &
                             Wrapper(WrapperNum)%HWLoopNum, &
                             Wrapper(WrapperNum)%HWLoopSideNum, &
                             Wrapper(WrapperNum)%HWBranchNum,&
                             Wrapper(WrapperNum)%HWCompNum, &
                             InletNodeNumber = Wrapper(WrapperNum)%HWInletNodeNum, &
                             errFlag=errFlag)

    CALL ScanPlantLoopsForObject(Wrapper(WrapperNum)%Name, &
                             TypeOf_CentralGroundSourceHeatPump, &
                             Wrapper(WrapperNum)%GLHELoopNum, &
                             Wrapper(WrapperNum)%GLHELoopSideNum, &
                             Wrapper(WrapperNum)%GLHEBranchNum,&
                             Wrapper(WrapperNum)%GLHECompNum, &
                             InletNodeNumber = Wrapper(WrapperNum)%GLHEInletNodeNum, &
                             errFlag=errFlag)

    CALL InterConnectTwoPlantLoopSides( Wrapper(WrapperNum)%CWLoopNum, &
                             Wrapper(WrapperNum)%CWLoopSideNum,    &
                             Wrapper(WrapperNum)%GLHELoopNum,      &
                             Wrapper(WrapperNum)%GLHELoopSideNum,  &
                             TypeOf_CentralGroundSourceHeatPump, .TRUE. )

    CALL InterConnectTwoPlantLoopSides( Wrapper(WrapperNum)%HWLoopNum, &
                             Wrapper(WrapperNum)%HWLoopSideNum,    &
                             Wrapper(WrapperNum)%GLHELoopNum,      &
                             Wrapper(WrapperNum)%GLHELoopSideNum,  &
                             TypeOf_CentralGroundSourceHeatPump, .TRUE. )

    CALL InterConnectTwoPlantLoopSides( Wrapper(WrapperNum)%CWLoopNum, &
                             Wrapper(WrapperNum)%CWLoopSideNum,    &
                             Wrapper(WrapperNum)%HWLoopNum,      &
                             Wrapper(WrapperNum)%HWLoopSideNum,  &
                             TypeOf_CentralGroundSourceHeatPump, .TRUE. )

    IF (Wrapper(WrapperNum)%VariableFlowCH) THEN
        ! Reset flow priority
      IF(LoopNum == Wrapper(WrapperNum)%CWLoopNum) THEN
        PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%LoopSide(Wrapper(WrapperNum)%CWLoopSideNum)% &
            Branch(Wrapper(WrapperNum)%CWBranchNum)%Comp(Wrapper(WrapperNum)%CWCompNum)%FlowPriority &
                = LoopFlowStatus_NeedyIfLoopOn
      ELSE IF(LoopNum == Wrapper(WrapperNum)%HWLoopNum) THEN
        PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%LoopSide(Wrapper(WrapperNum)%HWLoopSideNum)% &
            Branch(Wrapper(WrapperNum)%HWBranchNum)%Comp(Wrapper(WrapperNum)%HWCompNum)%FlowPriority &
                = LoopFlowStatus_NeedyIfLoopOn
      END IF

      ! check if setpoint on outlet node - chilled water loop
      IF (Node(Wrapper(WrapperNum)%CHWOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. Wrapper(WrapperNum)%CoolSetpointErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint on cooling side for CentralHeatPumpSystem named ' // &
                                          TRIM(Wrapper(WrapperNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem ' // &
                                          ', use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for CentralHeatPumpSystem. '// &
                                          'The simulation continues ... ')
            Wrapper(WrapperNum)%CoolSetpointErrDone = .TRUE.
          ENDIF
        ELSE
          ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(Wrapper(WrapperNum)%CHWOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. Wrapper(WrapperNum)%CoolSetpointErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint on cooling side for CentralHeatPumpSystem named ' // &
                                          TRIM(Wrapper(WrapperNum)%Name) )
              CALL ShowContinueError('A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem ')
              CALL ShowContinueError('use a Setpoint Manager to establish a setpoint at the chiller side outlet node ')
              CALL ShowContinueError('or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('The overall loop setpoint will be assumed for chiller side. ' // &
                                          'The simulation continues ... ')
              Wrapper(WrapperNum)%CoolSetpointErrDone = .TRUE.
            ENDIF
          ENDIF
        ENDIF
        Wrapper(WrapperNum)%CoolSetpointSetToLoop = .TRUE.
        Node(Wrapper(WrapperNum)%CHWOutletNodeNum)%TempSetPoint = &
                    Node(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      ENDIF

      IF (Node(Wrapper(WrapperNum)%HWOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. Wrapper(WrapperNum)%HeatSetpointErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint on heating side for CentralHeatPumpSystem named ' // &
                                          TRIM(Wrapper(WrapperNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem ' // &
                                          ', use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for CentralHeatPumpSystem. '// &
                                          'The simulation continues ... ')
            Wrapper(WrapperNum)%HeatSetpointErrDone = .TRUE.
          ENDIF
        ELSE
          ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(Wrapper(WrapperNum)%HWOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. Wrapper(WrapperNum)%HeatSetpointErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint on heating side for CentralHeatPumpSystem named ' // &
                                          TRIM(Wrapper(WrapperNum)%Name) )
              CALL ShowContinueError('A temperature setpoint is needed at the outlet node of a CentralHeatPumpSystem ')
              CALL ShowContinueError('use a Setpoint Manager to establish a setpoint at the chiller side outlet node ')
              CALL ShowContinueError('or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('The overall loop setpoint will be assumed for chiller side. ' // &
                                          'The simulation continues ... ')
              Wrapper(WrapperNum)%HeatSetpointErrDone = .TRUE.
            ENDIF
          ENDIF
        ENDIF
        Wrapper(WrapperNum)%HeatSetpointSetToLoop = .TRUE.
        Node(Wrapper(WrapperNum)%HWOutletNodeNum)%TempSetPoint = &
                    Node(PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      ENDIF
    ENDIF
    MyWrapperFlag(WrapperNum)=.FALSE.
  ENDIF

  CHWInletNodeNum = Wrapper(WrapperNum)%CHWInletNodeNum
  CHWOutletNodeNum = Wrapper(WrapperNum)%CHWOutletNodeNum
  HWInletNodeNum = Wrapper(WrapperNum)%HWInletNodeNum
  HWOutletNodeNum = Wrapper(WrapperNum)%HWOutletNodeNum
  GLHEInletNodeNum = Wrapper(WrapperNum)%GLHEInletNodeNum
  GLHEOutletNodeNum = Wrapper(WrapperNum)%GLHEOutletNodeNum

  IF (MyWrapperEnvrnFlag(WrapperNum) .AND. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete) CALL SizeWrapper(WrapperNum)
      IF (Wrapper(WrapperNum)%ControlMode==SmartMixing) THEN

          Wrapper(WrapperNum)%CHWVolFlowRate = 0.0d0
          Wrapper(WrapperNum)%HWVolFlowRate = 0.0d0
          Wrapper(WrapperNum)%GLHEVolFlowRate = 0.0d0

        DO ChillerHeaterNum = 1, Wrapper(WrapperNum)%ChillerHeaterNums
          Wrapper(WrapperNum)%CHWVolFlowRate = Wrapper(WrapperNum)%CHWVolFlowRate + &
                                        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapVolFlowRate
          Wrapper(WrapperNum)%HWVolFlowRate = Wrapper(WrapperNum)%HWVolFlowRate + &
                                        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%DesignHotWaterVolFlowRate
          Wrapper(WrapperNum)%GLHEVolFlowRate = Wrapper(WrapperNum)%GLHEVolFlowRate + &
                                              Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondVolFlowRate
        END DO

        rho = GetDensityGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex,&
                                  'InitCGSHPHeatPump')

        Wrapper(WrapperNum)%CHWMassFlowRateMax = Wrapper(WrapperNum)%CHWVolFlowRate * rho
        Wrapper(WrapperNum)%HWMassFlowRateMax = Wrapper(WrapperNum)%HWVolFlowRate * rho
        Wrapper(WrapperNum)%GLHEMassFlowRateMax = Wrapper(WrapperNum)%GLHEVolFlowRate * rho

           CALL InitComponentNodes(0.d0,  Wrapper(WrapperNum)%CHWMassFlowRateMax,  &
                          CHWInletNodeNum,        &
                          CHWOutletNodeNum,       &
                          Wrapper(WrapperNum)%CWLoopNum,      &
                          Wrapper(WrapperNum)%CWLoopSideNum,  &
                          Wrapper(WrapperNum)%CWBranchNum,    &
                          Wrapper(WrapperNum)%CWCompNum)
           CALL InitComponentNodes(0.d0,  Wrapper(WrapperNum)%HWMassFlowRateMax,  &
                          HWInletNodeNum,        &
                          HWOutletNodeNum,       &
                          Wrapper(WrapperNum)%HWLoopNum,      &
                          Wrapper(WrapperNum)%HWLoopSideNum,  &
                          Wrapper(WrapperNum)%HWBranchNum,    &
                          Wrapper(WrapperNum)%HWCompNum)
           CALL InitComponentNodes(0.d0,  Wrapper(WrapperNum)%GLHEMassFlowRateMax,  &
                          GLHEInletNodeNum,        &
                          GLHEOutletNodeNum,       &
                          Wrapper(WrapperNum)%GLHELoopNum,      &
                          Wrapper(WrapperNum)%GLHELoopSideNum,  &
                          Wrapper(WrapperNum)%GLHEBranchNum,    &
                          Wrapper(WrapperNum)%GLHECompNum)

           ! Initialize nodes for individual chiller heaters
        DO ChillerHeaterNum = 1 , Wrapper(WrapperNum)%ChillerHeaterNums
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%MassFlowRateMin      = 0.d0
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%MassFlowRateMinAvail = 0.d0
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%MassFlowRateMax      = &
                                          rho * Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapVolFlowRate
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%MassFlowRateMaxAvail = &
                                          rho * Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapVolFlowRate
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%MassFlowRate         = 0.d0
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%MassFlowRateMin      = 0.d0
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%MassFlowRateMinAvail = 0.d0
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%MassFlowRateMax = &
                                        rho * Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapVolFlowRate
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%MassFlowRateMaxAvail = &
                                        rho * Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapVolFlowRate
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%MassFlowRate         = 0.d0
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%MassFlowRateRequest  = 0.d0

        END DO

      END IF
    MyWrapperEnvrnFlag(WrapperNum) = .FALSE.
  ENDIF

  IF (.not. BeginEnvrnFlag) THEN
    MyWrapperEnvrnFlag(WrapperNum) = .TRUE.
  ENDIF

  IF (Wrapper(WrapperNum)%CoolSetpointSetToLoop) THEN
    !IF (CurCoolingLoad > 0.d0) THEN
      Node(Wrapper(WrapperNum)%CHWOutletNodeNum)%TempSetPoint =  &
                Node(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
  ENDIF
    !IF (CurHeatingLoad > 0.d0) THEN
  IF (Wrapper(WrapperNum)%HeatsetpointSetToLoop) THEN
      Node(Wrapper(WrapperNum)%HWOutletNodeNum)%TempSetPoint =  &
                Node(PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    !ENDIF
  ENDIF

    ! Switch over the mass flow rate to the condenser loop, i.e., ground heat exchanger
  IF   (LoopNum == Wrapper(WrapperNum)%CWLoopNum)  THEN ! called for on cooling loop
    IF (MyLoad < -1.d0) THEN ! calling for cooling
      mdotCHW = Node(CHWInletNodeNum)%MassFlowRateMax
    ELSE
      mdotCHW = 0.d0
    ENDIF
    IF ( Wrapper(WrapperNum)%WrapperHeatingLoad > 1.d0) THEN
      mdotHW = Node(HWInletNodeNum)%MassFlowRateMax
    ELSE
      mdotHW   = 0.d0
    ENDIF
    IF ((MyLoad < -1.d0) .OR. (Wrapper(WrapperNum)%WrapperHeatingLoad > 1.d0)) THEN
      mdotGLHE = Node(GLHEInletNodeNum)%MassFlowRateMax
    ELSE
      mdotGLHE = 0.d0
    ENDIF

  ELSEIF (LoopNum == Wrapper(WrapperNum)%HWLoopNum) THEN
    IF (MyLoad > 1.d0) THEN
      mdotHW = Node(HWInletNodeNum)%MassFlowRateMax
    ELSE
      mdotHW = 0.d0
    ENDIF
    IF (Wrapper(WrapperNum)%WrapperCoolingLoad > 1.d0) THEN
      mdotCHW = Node(CHWInletNodeNum)%MassFlowRateMax
    ELSE
      mdotCHW = 0.d0
    ENDIF
    IF ((MyLoad > 1.d0) .OR. (Wrapper(WrapperNum)%WrapperCoolingLoad > 1.d0)) THEN
      mdotGLHE = Node(GLHEInletNodeNum)%MassFlowRateMax
    ELSE
      mdotGLHE = 0.d0
    ENDIF

  ELSEIF (LoopNum == Wrapper(WrapperNum)%GLHELoopNum) THEN
    IF (Wrapper(WrapperNum)%WrapperCoolingLoad > 1.d0) THEN
      mdotCHW = Node(CHWInletNodeNum)%MassFlowRateMax
    ELSE
      mdotCHW = 0.d0
    ENDIF
    IF (Wrapper(WrapperNum)%WrapperHeatingLoad > 1.d0) THEN
      mdotHW = Node(HWInletNodeNum)%MassFlowRateMax
    ELSE
      mdotHW = 0.d0
    ENDIF
    IF ((Wrapper(WrapperNum)%WrapperHeatingLoad > 1.d0) .OR. &
       (Wrapper(WrapperNum)%WrapperCoolingLoad  > 1.d0))  THEN
      mdotGLHE = Node(GLHEInletNodeNum)%MassFlowRateMax
    ELSE
      mdotGLHE = 0.d0
    ENDIF
  ENDIF

  CALL SetComponentFlowRate(mdotCHW, CHWInletNodeNum, CHWOutletNodeNum, &
            Wrapper(WrapperNum)%CWLoopNum,       &
            Wrapper(WrapperNum)%CWLoopSideNum,   &
            Wrapper(WrapperNum)%CWBranchNum,     &
            Wrapper(WrapperNum)%CWCompNum)

  CALL SetComponentFlowRate( mdotHW, HWInletNodeNum, HWOutletNodeNum,  &
            Wrapper(WrapperNum)%HWLoopNum,     &
            Wrapper(WrapperNum)%HWLoopSideNum, &
            Wrapper(WrapperNum)%HWBranchNum,   &
            Wrapper(WrapperNum)%HWCompNum)

  CALL SetComponentFlowRate( mdotGLHE, GLHEInletNodeNum, GLHEOutletNodeNum, &
            Wrapper(WrapperNum)%GLHELoopNum,        &
            Wrapper(WrapperNum)%GLHELoopSideNum,    &
            Wrapper(WrapperNum)%GLHEBranchNum,      &
            Wrapper(WrapperNum)%GLHECompNum)

RETURN

END SUBROUTINE InitWrapper


SUBROUTINE CalcChillerModel(WrapperNum,OpMode,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,LoopNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang, PNNL
          !       DATE WRITTEN   Feb 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Simulate a ChillerHeaterPerformance:Electric:EIR using curve fit

          ! METHODOLOGY EMPLOYED:
          !  Use empirical curve fits to model performance at off-reference conditions

          ! REFERENCES:
          ! 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : WarmupFlag, CurrentTime,outputfiledebug, InitConvTemp
  USE DataHVACGlobals, ONLY : SmallLoad, TimeStepSys
  USE CurveManager,    ONLY : CurveValue,GetCurveMinMaxValues
  USE DataPlant,       ONLY : DeltaTemptol
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE InputProcessor,  ONLY: MakeUPPERCase
  USE General,         ONLY: TrimSigDigits, RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: WrapperNum        ! Number of wrapper
  INTEGER, INTENT(IN)    :: OpMode            ! Operation mode
  REAL(r64)              :: MyLoad            ! Operating load
  LOGICAL, INTENT(IN)    :: FirstIteration    ! TRUE when first iteration of timestep
  LOGICAL, INTENT(IN)    :: RunFlag           ! TRUE when chiller operating
  INTEGER, INTENT(IN)    :: EquipFlowCtrl     ! Flow control mode for the equipment
  INTEGER, INTENT(IN)    :: LoopNum           ! Plant loop number

          ! SUBROUTINE PARAMETER DEFINITIONS:

  !!CHARACTER(len=*), PARAMETER :: OutputFormat  = '(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE :: PossibleSubCooling
  LOGICAL   :: IsLoadCoolRemaining = .TRUE.
  LOGICAL   :: NextCompIndicator = .FALSE.  ! Component indicator when identical chiller heaters exist
  INTEGER   :: EvapInletNode           ! Evaporator inlet node number
  INTEGER   :: EvapOutletNode          ! Evaporator outlet node number
  INTEGER   :: CondInletNode           ! Condenser inlet node number
  INTEGER   :: CondOutletNode          ! Condenser outlet node number
  INTEGER   :: PlantLoopNum            ! Plant loop which contains the current chiller
  INTEGER   :: LoopSideNum             ! Plant loop side which contains the current chiller (usually supply side)
  INTEGER   :: BranchNum               ! Loop branch number
  INTEGER   :: CompNum                 ! Component number in the loop  REAL(r64) :: FRAC
  INTEGER   :: ChillerHeaterNum        ! Chiller heater number
  INTEGER   :: CurrentMode             ! Current operational mode, cooling or simultaneous cooling and heating mode
  INTEGER   :: IdenticalUnitCounter    ! Pointer to count number of identical unit passed
  INTEGER   :: IdenticalUnitRemaining  ! Pointer to count number of identical unit available for a component
  REAL(r64) :: FRAC                    ! Chiller cycling ratio
  REAL(r64) :: MinPartLoadRat          ! Min allowed operating fraction of full load
  REAL(r64) :: MaxPartLoadRat          ! Max allowed operating fraction of full load
  REAL(r64) :: EvapInletTemp           ! Evaporator inlet temperature [C]
  REAL(r64) :: CondInletTemp           ! Condenser inlet temperature [C]
  REAL(r64) :: EvapOutletTempSetpoint  ! Evaporator outlet temperature setpoint [C]
  REAL(r64) :: AvailChillerCap         ! Chiller available capacity at current operating conditions [W]
  REAL(r64) :: ChillerRefCap           ! Chiller reference capacity
  REAL(r64) :: EvapDeltaTemp           ! Evaporator temperature difference [C]
  REAL(r64) :: ReferenceCOP            ! Reference coefficient of performance, from user input
  REAL(r64) :: PartLoadRat             ! Operating part load ratio
  REAL(r64) :: TempLowLimitEout        ! Evaporator low temp. limit cut off [C]
  REAL(r64) :: EvapMassFlowRateMax     ! Max reference evaporator mass flow rate converted from volume flow rate [kg/s]
  REAL(r64) :: Cp                      ! Local fluid specific heat
  REAL(r64) :: CondTempforCurve        ! Condenser temp used for performance curve
  REAL(r64) :: RemainingEvapMassPrevCH ! Bypass water from the previous variable chiller heater
  REAL(r64) :: MinLoadToMeet           ! Part load this chiller should meet
  REAL(r64) :: CoolingLoadToMeet       ! Remaining cooling load the other chiller heaters should meet
  REAL(r64) :: GLHEDensityRatio        ! Fraction between starndarized density and local density in the condenser side
  REAL(r64) :: CHWDensityRatio         ! Fraction between starndarized density and local density in the chilled water side
  REAL(r64) :: EvaporatorCapMin        ! Minimum capacity of the evaporator
  REAL(r64) :: EvaporatorLoad          ! Cooling load evaporator should meet
  REAL(r64) :: HeatingPower            ! Electric power use for heating
  REAL(r64) :: CHWInletMassFlowRate    ! Chilled water inlet mass flow rate
  REAL(r64) :: CurAvailCHWMassFlowRate ! Maximum available mass flow rate for current chiller heater
  REAL(r64) :: EvapMassFlowRateCalc    ! Evaporator mass flow rate calculated
  REAL(r64) :: EvapDeltaTempCalc       ! Evaporator temperature difference calculated
  REAL(r64) :: EvapOutletTempCalc      ! Evaporator outlet temperature calculated
  REAL(r64) :: EvapMassFlowRate        ! Actual evaporator mass flow rate
  REAL(r64) :: CondMassFlowRate        ! Condenser mass flow rate
  REAL(r64) :: EvapOutletTemp          ! Evaporator outlet temperature
  REAL(r64) :: CondOutletTemp          ! Condenser outlet temperature
  REAL(r64) :: QCondenser              ! Condenser heat transfer rate
  REAL(r64) :: QEvaporator             ! Evaporator heat transfer rate
  REAL(r64) :: CHPower                 ! Evaporator power rate
  REAL(r64) :: InitDensity             ! Water density at the initial temperature
  REAL(r64) :: EvapDensity             ! Evaporator water density
  REAL(r64) :: CondDensity             ! Condenser water density
  REAL(r64) :: ActualCOP               ! Actual performance of individual chiller heater

  EvaporatorLoad = 0.0d0
  EvaporatorLoad = Wrapper(WrapperNum)%WrapperCoolingLoad
  LoopSideNum = Wrapper(WrapperNum)%CWLoopSideNum
  CHWInletMassFlowRate = Node(Wrapper(WrapperNum)%CHWInletNodeNum)%MassFlowRate
  CurAvailCHWMassFlowRate = 0.d0
  CompNum = 0

  DO ChillerHeaterNum=1, Wrapper(WrapperNum)%ChillerHeaterNums

      ! Initialize local variables for each chiller heater
    CurrentMode    = 0
    ChillerCapFT   = 0.d0
    ChillerEIRFT   = 0.d0
    ChillerEIRFPLR = 0.d0
    CoolingLoadToMeet    = 0.d0
    ChillerPartLoadRatio = 0.d0
    ChillerCyclingRatio  = 0.d0
    ChillerFalseLoadRate = 0.d0
    EvapMassFlowRate     = 0.d0
    CondMassFlowRate     = 0.d0
    CHPower              = 0.d0
    HeatingPower         = 0.d0
    QCondenser           = 0.d0
    QEvaporator          = 0.d0
    CondenserFanPower    = 0.d0
    FRAC                 = 1.d0
    EvapDeltaTemp        = 0.d0
    ActualCOP            = 0.d0
    RemainingEvapMassPrevCH = 0.d0
    EvapInletTemp  = Node(Wrapper(WrapperNum)%CHWInletNodeNum)%Temp
    CondInletTemp  = Node(Wrapper(WrapperNum)%GLHEInletNodeNum)%Temp
    EvapOutletTemp = EvapInletTemp
    CondOutletTemp = CondInletTemp
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode = 0

      ! Find proper schedule values
    IF (Wrapper(WrapperNum)%NumOfComp /= Wrapper(WrapperNum)%ChillerHeaterNums) THEN ! Identical units exist
      IF (ChillerHeaterNum == 1) THEN
        IdenticalUnitCounter = 0
        IdenticalUnitRemaining = 0
        NextCompIndicator = .FALSE.
        CompNum = ChillerHeaterNum
      END IF
      IF (NextCompIndicator) THEN
        CompNum = CompNum + 1
      END IF
      IF (CompNum == 1) THEN
        IF (ChillerHeaterNum /=  Wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum) THEN
          NextCompIndicator = .FALSE.
        ELSE iF (ChillerHeaterNum == Wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum) THEN
          NextCompIndicator = .TRUE.
        END IF
      ELSE IF (CompNum > 1) THEN
        IF ((ChillerHeaterNum - ((ChillerHeaterNum-1) - IdenticalUnitCounter)) /= &
             wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum) THEN
          NextCompIndicator = .FALSE.
        ELSE IF ((ChillerHeaterNum - ((ChillerHeaterNum-1) - IdenticalUnitCounter)) ==  &
                 wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum) THEN
          NextCompIndicator = .TRUE.
        END IF
      END IF
      IdenticalUnitCounter = IdenticalUnitCounter + 1
      IdenticalUnitRemaining = Wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum - IdenticalUnitCounter
        IF (IdenticalUnitRemaining == 0) IdenticalUnitCounter = 0
    ELSE IF (Wrapper(WrapperNum)%NumOfComp == Wrapper(WrapperNum)%ChillerHeaterNums) THEN
      CompNum = CompNum + 1
    END IF

    IF (CompNum > Wrapper(WrapperNum)%NumOfComp) THEN
      CALL ShowSevereError('CalcChillerModel: ChillerHeater="'//trim(Wrapper(WrapperNum)%Name)//  &
         '", calculated component number too big.')
      CALL ShowContinueError('Max number of components=['//trim(RoundSigDigits(Wrapper(WrapperNum)%NumOfComp))//  &
         '], indicated component number=['//trim(RoundSigDigits(CompNum))//'].')
      CALL ShowFatalError('Program terminates due to preceding condition.')
    ENDIF

      ! Check whether this chiller heater needs to run
    IF (EvaporatorLoad > 0.0d0 .AND. (GetCurrentScheduleValue(Wrapper(WrapperNum)%WrapperComp(CompNum)%CHSchedPtr) > 0.0d0)) THEN
        IsLoadCoolRemaining = .TRUE.

        ! Calculate density ratios to adjust mass flow rates from initialized ones
        ! Hot water temperature is known, but evaporator mass flow rates will be adjusted in the following "Do" loop
      InitDensity = GetDensityGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                              InitConvTemp, &
                              PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex, &
                             'CalcChillerHeaterModel')
      EvapDensity = GetDensityGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                              EvapInletTemp, &
                              PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex, &
                             'CalcChillerHeaterModel')
      CondDensity = GetDensityGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                              CondInletTemp, &
                              PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex, &
                             'CalcChillerHeaterModel')

        ! Calculate density ratios to adjust mass flow rates from initialized ones
      CHWDensityRatio = EvapDensity / InitDensity
      GLHEDensityRatio = CondDensity / InitDensity
      CondMassFlowRate = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%MassFlowRateMaxAvail
      EvapMassFlowRate = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%MassFlowRateMaxAvail
      EvapMassFlowRate = EvapMassFlowRate * CHWDensityRatio
      CondMassFlowRate = CondMassFlowRate * GLHEDensityRatio

        ! Check available flows from plant and then adjust as necessary
      IF (CurAvailCHWMassFlowRate == 0) THEN ! The very first chiller heater to operate
        CurAvailCHWMassFlowRate = CHWInletMassFlowRate
      ELSE IF (ChillerHeaterNum > 1) THEN
        CurAvailCHWMassFlowRate = CurAvailCHWMassFlowRate - &
                                  Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum-1)%EvapOutletNode%MassFlowRate
      END IF
      EvapMassFlowRate = MIN(CurAvailCHWMassFlowRate,EvapMassFlowRate)
    ELSE
      IsLoadCoolRemaining = .FALSE.
      EvapMassFlowRate = 0.d0
      CondMassFlowRate = 0.d0
      CurrentMode = 0
    END IF

      ! Chiller heater is on when cooling load for this chiller heater remains and chilled water available
    IF (IsLoadCoolRemaining .AND. (EvapMassFlowRate > 0) .AND.  &
       (GetCurrentScheduleValue(Wrapper(WrapperNum)%WrapperComp(CompNum)%CHSchedPtr) > 0)) THEN
        ! Indicate current mode is cooling-only mode. Simulataneous clg/htg mode will be set later
      CurrentMode = 1

        ! Assign proper performance curve information depending on the control mode
        ! Cooling curve is used only for cooling-only mode, and the others (Simulataneous and heating) read the heating curve
      IF (SimulClgDominant .OR. SimulHtgDominant) THEN
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCap =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCapClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCOP =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCOPClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefEvapOut =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefEvapOutClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondOut =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondOutClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%OptPartLoadRat =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%OptPartLoadRatClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondMode =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondModeHeating
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFT =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTHeating
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFT =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFTHeating
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLR =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRHeating
      ELSE
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCap =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCapCooling
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCOP =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCOPCooling
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefEvapOut =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefEvapOutCooling
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondIn =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondInCooling
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondOut =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondOutCooling
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%OptPartLoadRat =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%OptPartLoadRatCooling
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondMode =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondModeCooling
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFT =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTCooling
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFT =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFTCooling
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLR =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRCooling
      END IF

        ! Only used to read curve values
      CondOutletTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondOutCooling
      IF (TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondMode) == MakeUPPERCase('EnteringCondenser')) THEN
        CondTempforCurve =  CondInletTemp
      ELSE IF (TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondMode) == MakeUPPERCase('LeavingCondenser')) THEN
        CondTempforCurve = CondOutletTemp
      ELSE
        CALL ShowWarningError('ChillerHeaterPerformance:Electric:EIR "'// &
                         TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)//'":')
        CALL ShowContinueError('Chiller condensor temperature for curve fit are not decided, defalt value= cond_leaving ('// &
                         TRIM(RoundSigDigits(ChillerCapFT,3))//').')
        CondTempforCurve = CondOutletTemp
      ENDIF

        ! Bind local variables from the curve
      CALL GetCurveMinMaxValues (Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLR,MinPartLoadRat,MaxPartLoadRat)
      ChillerRefCap    = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCap
      ReferenceCOP     = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCOP
      EvapOutletTemp   = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%Temp
      TempLowLimitEout = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempLowLimitEvapOut
      EvapOutletTempSetpoint = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefEvapOutCooling
      ChillerCapFT = CurveValue(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFT, &
                     EvapOutletTempSetpoint,CondTempforCurve)

      IF(ChillerCapFT .LT. 0)THEN
        IF(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError .LT. 1 .AND. .NOT. WarmupFlag)THEN
           Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError = &
                             Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError + 1
          CALL ShowWarningError('ChillerHeaterPerformance:Electric:EIR "'//&
                        TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)//'":')
          CALL ShowContinueError(' ChillerHeater Capacity as a Function of Temperature curve output is negative ('// &
                        TRIM(RoundSigDigits(ChillerCapFT,3))//').')
          CALL ShowContinueError(' Negative value occurs using an Evaporator Outlet Temp of ' // &
                        TRIM(RoundSigDigits(EvapOutletTempSetpoint,1))// &
                        ' and a Condenser Inlet Temp of '//TRIM(RoundSigDigits(CondInletTemp,1))//'.')
          CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
        ELSE IF(.NOT. WarmupFlag)THEN
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError = &
                        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError + 1
          CALL ShowRecurringWarningErrorAtEnd('ChillerHeaterPerformance:Electric:EIR "' &
                        //TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)//'":'//&
            ' ChillerHeater Capacity as a Function of Temperature curve output is negative warning continues...' &
            , Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTErrorIndex, ChillerCapFT, ChillerCapFT)
        END IF
        ChillerCapFT = 0.0d0
      END IF

        ! Calculate the specific heat of chilled water
      Cp  = GetSpecificHeatGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                              EvapInletTemp, &
                              PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex, &
                             'CalcChillerHeaterModel')

        ! Calculate cooling load this chiller should meet and the other chillers are demanded
      EvapOutletTempSetpoint = Node(PlantLoop(wrapper(wrappernum)%CWloopnum)%TempSetPointNodeNum)%TempSetPoint
      EvaporatorCapMin = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%MinPartLoadRatCooling * &
                         Wrapper(WrapperNum)%ChillerHeater(ChillerHeaterNum)%RefCapCooling
      CoolingLoadToMeet = MIN(Wrapper(WrapperNum)%ChillerHeater(ChillerHeaterNum)%RefCapCooling, &
                          MAX(ABS(EvaporatorLoad),EvaporatorCapMin))

        ! Available chiller capacity as a function of temperature
      AvailChillerCap = ChillerRefCap * ChillerCapFT

        ! Part load ratio based on load and available chiller capacity, cap at max part load ratio
      IF(AvailChillerCap .GT. 0)THEN
        PartLoadRat = MAX(0.0d0, MIN(CoolingLoadToMeet/AvailChillerCap,MaxPartLoadRat))
      ELSE
        PartLoadRat = 0.0d0
      END IF

      IF(Wrapper(WrapperNum)%ChillerHeater(ChillerHeaterNum)%PossibleSubCooling) THEN
        QEvaporator = CoolingLoadToMeet
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = EvapInletTemp - EvapDeltaTemp
      END IF

        ! Set load this chiller heater should meet
      QEvaporator = Min(CoolingLoadToMeet,(AvailChillerCap * MaxPartLoadRat))
      EvapOutletTemp = EvapOutletTempSetpoint
      EvapDeltaTemp = EvapInletTemp - EvapOutletTemp

        ! Calculate temperatures for constant flow and mass flow rates for variable flow
      IF(EvapMassFlowRate > MassFlowTolerance) THEN
        IF (SimulHtgDominant) THEN  ! Evaporator operates at full capacity for heating
          PartLoadRat = MAX(0.0d0, MIN((ChillerRefCap/AvailChillerCap),MaxPartLoadRat))
          QEvaporator = AvailChillerCap * PartLoadRat
          EvapDeltaTemp = QEvaporator/EvapMassFlowRate/CP
          EvapOutletTemp = EvapInletTemp - EvapDeltaTemp
        ELSE ! Cooling only mode or cooling dominant simultaneous htg/clg mode
          IF (Wrapper(WrapperNum)%VariableFlowCH) THEN ! Variable flow
            EvapMassFlowRateCalc = QEvaporator/EvapDeltaTemp/Cp
            IF (EvapMassFlowRateCalc > EvapMassFlowRate) THEN
              EvapMassFlowRateCalc = EvapMassFlowRate
              EvapDeltaTempCalc = QEvaporator/EvapMassFlowRate/Cp
              EvapOutletTemp = EvapInletTemp - EvapDeltaTempCalc
              IF (EvapDeltaTempCalc > EvapDeltaTemp) THEN
                EvapDeltaTempCalc = EvapDeltaTemp
                QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp
              END IF
            END IF
            EvapMassFlowRate = EvapMassFlowRateCalc
          ELSE ! Constant Flow
            EvapDeltaTempCalc = QEvaporator/EvapMassFlowRate/Cp
            EvapOutletTempCalc = EvapInletTemp - EvapDeltaTemp
            IF(EvapOutletTempCalc > EvapOutletTemp) THEN ! Load to meet should be adjusted
               EvapOutletTempCalc = EvapOutletTemp
               QEvaporator = EvapMassFlowRate * Cp * EvapDeltaTemp
            END IF
            EvapOutletTemp = EvapOutletTempCalc
          END IF ! End of flow control decision
        END IF ! End of operation mode
      ELSE
        QEvaporator = 0.0d0
        EvapOutletTemp = EvapInletTemp
      END IF

        ! Check evaporator temperature low limit and adjust capacity if needed
      IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
        IF((EvapInletTemp - TempLowLimitEout) .GT. DeltaTemptol) THEN
           EvapOutletTemp = TempLowLimitEout
           EvapDeltaTemp = EvapInletTemp - EvapOutletTemp
           QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
        ELSE
          QEvaporator = 0.0d0
          EvapOutletTemp = EvapInletTemp
        END IF
      END IF

         ! Check if the outlet temperature exceeds the node minimum temperature and adjust capacity if needed
      IF(EvapOutletTemp .LT. Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin) THEN
        IF((Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp - &
          Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin) .GT. DeltaTemptol) THEN
          EvapOutletTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin
          EvapDeltaTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin - EvapOutletTemp
          QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
        ELSE
          QEvaporator = 0.0d0
          EvapOutletTemp = EvapInletTemp
        END IF
      END IF

        ! Calculate part load once more since evaporator capacity might be modified
      IF(AvailChillerCap .GT. 0.0d0)THEN
        PartLoadRat = MAX(0.0d0,MIN((QEvaporator/AvailChillerCap),MaxPartLoadRat))
      ELSE
        PartLoadRat = 0.0d0
      END IF

        ! Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
      IF (PartLoadRat .LT. MinPartLoadRat) FRAC = MIN(1.0d0,(PartLoadRat/MinPartLoadRat))

        ! set the module level variable used for reporting FRAC
      ChillerCyclingRatio = FRAC

        ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
      IF(AvailChillerCap .GT. 0.0d0)THEN
        PartLoadRat = Max(PartLoadRat,MinPartLoadRat)
      ELSE
        PartLoadRat = 0.0d0
      END IF

        ! set the module level variable used for reporting PLR
      ChillerPartLoadRatio = PartLoadRat

      ! calculate the load due to false loading on chiller over and above water side load
      ChillerFalseLoadRate = (AvailChillerCap * PartLoadRat * FRAC) - QEvaporator
      IF(ChillerFalseLoadRate .LT. SmallLoad) THEN
         ChillerFalseLoadRate = 0.0d0
      END IF

        ! Determine chiller compressor power and transfer heat calculation
      ChillerEIRFT = MAX(0.d0,CurveValue(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFT,  &
                            EvapOutletTemp,CondTempforCurve))
      ChillerEIRFPLR  = MAX(0.d0,CurveValue(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLR,PartLoadRat))
      CHPower = (AvailChillerCap/ReferenceCOP) * ChillerEIRFPLR * ChillerEIRFT * FRAC
      QCondenser = CHPower*Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%OpenMotorEff +  &
                    QEvaporator + ChillerFalseLoadRate
      ActualCOP = (QEvaporator+ChillerFalseLoadRate)/CHPower

      IF (CondMassFlowRate > MassFlowTolerance) THEN
        Cp  = GetSpecificHeatGlycol(PlantLoop(Wrapper(WrapperNum)%GLHELoopNum)%FluidName,  &
                                  CondInletTemp,                      &
                                  PlantLoop(Wrapper(WrapperNum)%GLHELoopNum)%FluidIndex, &
                                 'CalcElectricEIRChillerModel')
        CondOutletTemp = QCondenser/CondMassFlowRate/Cp + CondInletTemp
      ELSE
        CALL ShowSevereError('CalcChillerheaterModel: Condenser flow = 0, for Chillerheater='//  &
                           TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      END IF

        ! Determine load next chillers should meet
      IF (EvaporatorLoad < QEvaporator) THEN
        EvaporatorLoad = 0.d0 ! No remaining load so the rest will be off
      ELSE
        EvaporatorLoad = EvaporatorLoad - QEvaporator
      END IF

        ! Initialize reporting variable when this chiller doesn't need to operate
      IF (QEvaporator == 0.d0) THEN
        CurrentMode = 0
        ChillerPartLoadRatio = 0.d0
        ChillerCyclingRatio  = 0.d0
        ChillerFalseLoadRate = 0.d0
        EvapMassFlowRate     = 0.d0
        CondMassFlowRate     = 0.d0
        CHPower              = 0.d0
        QCondenser           = 0.d0
        CondenserFanPower    = 0.d0
        EvapOutletTemp = EvapInletTemp
        CondOutletTemp = CondInletTemp
        EvaporatorLoad = 0.d0
      END IF

    END IF ! End of calculation for cooling

      ! Set variables to the arrays
    Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%MassFlowRate = EvapMassFlowRate
    Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondOutletNode%MassFlowRate = CondMassFlowRate
    Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%Temp = EvapOutletTemp
    Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp = EvapInletTemp
    Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondOutletNode%Temp = CondOutletTemp
    Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%Temp = CondInletTemp
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode = CurrentMode
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerPartLoadRatio = ChillerPartLoadRatio
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCyclingRatio  = ChillerCyclingRatio
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadRate = ChillerFalseLoadRate
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCapFT = ChillerCapFT
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFT = ChillerEIRFT
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFPLR = ChillerEIRFPLR
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPower = CHPower
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower = HeatingPower
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap = QEvaporator
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond = QCondenser
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTemp = EvapOutletTemp
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapInletTemp = EvapInletTemp
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTemp = CondOutletTemp
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondInletTemp = CondInletTemp
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot = EvapMassFlowRate
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot = CondMassFlowRate
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ActualCOP = ActualCOP

    IF (SimulClgDominant .OR. SimulHtgDominant) THEN ! Store for using these cooling side data in the hot water loop
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode = CurrentMode
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerPartLoadRatioSimul = ChillerPartLoadRatio
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCyclingRatioSimul  = ChillerCyclingRatio
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadRateSimul = ChillerFalseLoadRate
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCapFTSimul = ChillerCapFT
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFTSimul = ChillerEIRFT
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFPLRSimul = ChillerEIRFPLR
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPowerSimul = CHPower
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvapSimul = QEvaporator
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTempSimul = EvapOutletTemp
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapInletTempSimul = EvapInletTemp
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapmdotSimul = EvapMassFlowRate
      IF (SimulClgDominant) THEN
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCondSimul = QCondenser
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTempSimul = CondOutletTemp
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondInletTempSimul = CondInletTemp
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondmdotSimul = CondMassFlowRate
      END IF
    END IF
  END DO

  RETURN

END SUBROUTINE CalcChillerModel


SUBROUTINE CalcChillerHeaterModel(WrapperNum,OpMode,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,LoopNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang, PNNL
          !       DATE WRITTEN   Feb 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Simulate a ChillerHeaterPerformance:Electric:EIR using curve fit

          ! METHODOLOGY EMPLOYED:
          !  Use empirical curve fits to model performance at off-reference conditions

          ! REFERENCES:
          ! 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : WarmupFlag, CurrentTime, outputfiledebug, InitConvTemp
  USE DataHVACGlobals, ONLY : SmallLoad, TimeStepSys
  USE CurveManager,    ONLY : CurveValue,GetCurveMinMaxValues
  USE DataPlant,       ONLY : DeltaTemptol
  USE ScheduleManager, ONLY : GetCurrentScheduleValue
  USE InputProcessor,  ONLY : MakeUPPERCase
  USE General,         ONLY : TrimSigDigits, RoundSigDigits
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: WrapperNum      ! Wrapper number pointor
  INTEGER, INTENT(IN)    :: OpMode          ! Operation mode
  REAL(r64)              :: MyLoad          ! Heating load plant should meet
  LOGICAL, INTENT(IN)    :: FirstIteration  ! TRUE when first iteration of timestep
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when chiller operating
  INTEGER, INTENT(IN)    :: EquipFlowCtrl   ! Flow control mode for the equipment
  INTEGER, INTENT(IN)    :: LoopNum         ! Loop number

          ! SUBROUTINE PARAMETER DEFINITIONS:

  !!CHARACTER(len=*), PARAMETER :: OutputFormat  = '(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL      :: ErrorsFound=.false.       ! True when input errors are found
  LOGICAL,SAVE :: PossibleSubCooling        ! Flag to indicate chiller is doing less cooling that requested
  LOGICAL   :: IsLoadHeatRemaining = .TRUE. ! Ture if heating load remains for this chiller heater
  LOGICAL   :: NextCompIndicator = .FALSE.  ! Component indicator when identical chiller heaters exist
  INTEGER   :: EvapInletNode                ! Evaporator inlet node number
  INTEGER   :: EvapOutletNode               ! Evaporator outlet node number
  INTEGER   :: CondInletNode                ! Condenser inlet node number
  INTEGER   :: CondOutletNode               ! Condenser outlet node number
  INTEGER   :: PlantLoopNum                 ! Plant loop which contains the current chiller
  INTEGER   :: LoopSideNum                  ! Plant loop side which contains the current chiller (usually supply side)
  INTEGER   :: BranchNum                    ! Branch number
  INTEGER   :: CompNum                      ! Component number
  INTEGER   :: ChillerHeaterNum             ! Chiller heater number
  INTEGER   :: CurrentMode                  ! Current operational mode, heating or simultaneous cooling and heating mode
  INTEGER   :: IdenticalUnitCounter         ! Pointer to count number of identical unit passed
  INTEGER   :: IdenticalUnitRemaining       ! Pointer to count number of identical unit available for a component
  REAL(r64) :: Cp                           ! Local fluid specific heat
  REAL(r64) :: CondTempforCurve             ! Reference condenser temperature for the performance curve reading
  REAL(r64) :: FRAC                         ! Chiller cycling ratio
  REAL(r64) :: MinPartLoadRat               ! Min allowed operating fraction of full load
  REAL(r64) :: MaxPartLoadRat               ! Max allowed operating fraction of full load
  REAL(r64) :: EvapInletTemp                ! Evaporator inlet temperature [C]
  REAL(r64) :: CondInletTemp                ! Condenser inlet temperature [C]
  REAL(r64) :: EvapOutletTempSetpoint       ! Condenser outlet temperature setpoint [C]
  REAL(r64) :: CondOutletTempSetpoint       ! Condenser outlet temperature setpoint [C]
  REAL(r64) :: AvailChillerCap              ! Chiller available capacity at current operating conditions [W]
  REAL(r64) :: ChillerRefCap                ! Chiller reference capacity
  REAL(r64) :: EvapDeltaTemp                ! Evaporator temperature difference [C]
  REAL(r64) :: CondDeltaTemp                ! Condenser temperature difference [C]
  REAL(r64) :: ReferenceCOP                 ! Reference coefficient of performance, from user input
  REAL(r64) :: PartLoadRat                  ! Operating part load ratio
  REAL(r64) :: TempLowLimitEout             ! Evaporator low temp. limit cut off [C]
  REAL(r64) :: EvapMassFlowRateMax          ! Maximum reference evaporator mass flow rate [kg/s]
  REAL(r64) :: CondenserLoad                ! Remaining heating load that this wrapper should meet
  REAL(r64) :: HeatingLoadToMeet            ! Heating load that this chiller heater should meet
  REAL(r64) :: GLHEDensityRatio             ! The density ratio of source water to the initialized source water
  REAL(r64) :: HWDensityRatio               ! The density ratio of hot water to the initialized hot water
  REAL(r64) :: PartLoadRatHeat              ! Condenser part load ratio
  REAL(r64) :: CondenserCapMin              ! Minimum condenser capacity
  REAL(r64) :: CoolingPower                 ! Evaporator cooling power to produce heat for heating
  REAL(r64) :: HWInletMassFlowRate          ! Hot water inlet mass flow rate
  REAL(r64) :: CurAvailHWMassFlowRate       ! Maximum available hot water mass within the wrapper bank
  REAL(r64) :: CondDeltaTempCalc            ! Temperature differnece between condenser inlet and outlet calculated
  REAL(r64) :: CondOutletTempCalc           ! Condenser outlet temperature calculated
  REAL(r64) :: CondMassFlowRateCalc         ! Condenser mass flow rate calculated
  REAL(r64) :: EvapMassFlowRate             ! Evaporator mass flow rate through this chiller heater
  REAL(r64) :: CondMassFlowRate             ! Condenser mass flow rate through this chiller heater
  REAL(r64) :: EvapOutletTemp               ! Evaporator outlet temperature
  REAL(r64) :: CondOutletTemp               ! Condenser outlet temperature
  REAL(r64) :: QCondenser                   ! Condenser heat transfer rate
  REAL(r64) :: QEvaporator                  ! Evaporator heat transfer rate
  REAL(r64) :: CHPower                      ! Evaporator compressor power added to heating power
  REAL(r64) :: InitDensity                  ! Water density at the initial temperature
  REAL(r64) :: EvapDensity                  ! Evaporator water density
  REAL(r64) :: CondDensity                  ! Condenser water density
  REAL(r64) :: ActualCOP                    ! Actual performance of individual chiller heater

  CondenserLoad = 0.d0
  CondenserLoad = Wrapper(WrapperNum)%WrapperHeatingLoad
  LoopSideNum = Wrapper(WrapperNum)%HWLoopSideNum
  HWInletMassFlowRate = Node(Wrapper(WrapperNum)%HWInletNodeNum)%MassFlowRate
  CurAvailHWMassFlowRate = 0.d0
  CompNum = 0

      ! Flow
  DO ChillerHeaterNum=1, Wrapper(WrapperNum)%ChillerHeaterNums

      ! Set module level inlet and outlet nodes and initialize other local variables
    CurrentMode = 0
    HeatingLoadToMeet        = 0.d0
    ChillerPartLoadRatio     = 0.d0
    ChillerCyclingRatio      = 0.d0
    ChillerFalseLoadRate     = 0.d0
    EvapMassFlowRate         = 0.d0
    CondMassFlowRate         = 0.d0
    CHPower                  = 0.d0
    QCondenser               = 0.d0
    QEvaporator              = 0.d0
    CondenserFanPower        = 0.d0
    FRAC                     = 1.d0
    CondDeltaTemp            = 0.d0
    EvapDeltaTemp            = 0.d0
    CoolingPower             = 0.d0
    ActualCOP                = 0.d0
    EvapInletTemp = Node(Wrapper(WrapperNum)%GLHEInletNodeNum)%Temp
    CondInletTemp = Node(Wrapper(WrapperNum)%HWInletNodeNum)%Temp
    EvapOutletTemp = EvapInletTemp
    CondOutletTemp = CondInletTemp

      ! Find proper schedule values
    IF (Wrapper(WrapperNum)%NumOfComp /= Wrapper(WrapperNum)%ChillerHeaterNums) THEN ! Identical units exist
      IF (ChillerHeaterNum == 1) THEN
        IdenticalUnitCounter = 0
        IdenticalUnitRemaining = 0
        NextCompIndicator = .FALSE.
        CompNum = ChillerHeaterNum
      END IF
      IF (NextCompIndicator) THEN
        CompNum = CompNum + 1
      END IF
      IF (CompNum == 1) THEN
        IF (ChillerHeaterNum /=  Wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum) THEN
          NextCompIndicator = .FALSE.
        ELSE iF (ChillerHeaterNum == Wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum) THEN
          NextCompIndicator = .TRUE.
        END IF
      ELSE IF (CompNum > 1) THEN
        IF ((ChillerHeaterNum - ((ChillerHeaterNum-1) - IdenticalUnitCounter)) /= &
             wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum) THEN
          NextCompIndicator = .FALSE.
        ELSE IF ((ChillerHeaterNum - ((ChillerHeaterNum-1) - IdenticalUnitCounter)) ==  &
                 wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum) THEN
          NextCompIndicator = .TRUE.
        END IF
      END IF
      IdenticalUnitCounter = IdenticalUnitCounter + 1
      IdenticalUnitRemaining = Wrapper(WrapperNum)%WrapperComp(CompNum)%WrapperIdenticalObjectNum - IdenticalUnitCounter
        IF (IdenticalUnitRemaining == 0) IdenticalUnitCounter = 0
    ELSE IF (Wrapper(WrapperNum)%NumOfComp == Wrapper(WrapperNum)%ChillerHeaterNums) THEN
      CompNum = CompNum + 1
    END IF

      ! Check to see if this chiiller heater needs to run
    IF (CondenserLoad > 0.d0 .AND. (GetCurrentScheduleValue(Wrapper(WrapperNum)%WrapperComp(CompNum)%CHSchedPtr) > 0)) THEN
      IsLoadHeatRemaining = .TRUE.

        ! Calculate density ratios to adjust mass flow rates from initialized ones
        ! Hot water temperature is known, but condenser mass flow rates will be adjusted in the following "Do" loop
      InitDensity = GetDensityGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                              InitConvTemp, &
                              PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex, &
                             'CalcChillerHeaterModel')
      EvapDensity = GetDensityGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                              EvapInletTemp, &
                              PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex, &
                             'CalcChillerHeaterModel')
      CondDensity = GetDensityGlycol(PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidName,  &
                              CondInletTemp, &
                              PlantLoop(Wrapper(WrapperNum)%CWLoopNum)%FluidIndex, &
                             'CalcChillerHeaterModel')

        ! Calculate density ratios to adjust mass flow rates from initialized ones
      HWDensityRatio = CondDensity / InitDensity
      GLHEDensityRatio = EvapDensity / InitDensity
      EvapMassFlowRate = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%MassFlowRateMaxAvail
      CondMassFlowRate = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%MassFlowRateMaxAvail
      EvapMassFlowRate = EvapMassFlowRate * GLHEDensityRatio
      CondMassFlowRate = CondMassFlowRate * HWDensityRatio

        ! Check flows from plant to adjust as necessary
      IF (CurAvailHWMassFlowRate == 0) THEN ! First chiller heater which is on
        CurAvailHWMassFlowRate = HWInletMassFlowRate
      ELSE IF (ChillerHeaterNum > 1) THEN
        CurAvailHWMassFlowRate = CurAvailHWMassFlowRate - &
                                 Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum-1)%CondOutletNode%MassFlowRate
      END IF
      CondMassFlowRate = MIN(CurAvailHWMassFlowRate,CondMassFlowRate)

        ! It is not enforced to be the smaller of CH max temperature and plant temp setpoint.
        ! Hot water temperatures at the individual CHs' outlet may be greater than plant setpoint temp,
        ! but should be lower than the CHs max temp
      CondOutletTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondOutClgHtg
      CondDeltaTemp = CondOutletTemp - CondInletTemp

      IF(CondDeltaTemp < 0.d0) THEN ! Hot water temperature is greater than the maximum
        IF (Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRRefTempErrorIndex == 0) THEN
          CALL ShowSevereMessage('CalcChillerHeaterModel: ChillerHeaterPerformance:Electric:EIR="'// &
               TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)//'", DeltaTemp < 0')
          CALL ShowContinueError(' Reference Simultaneous Cooling-Heating Mode Leaving Condenser Water Temperature ['//  &
             trim(RoundSigDigits(CondOutletTemp,1))//']')
          CALL ShowContinueError('is below condenser inlet temperature of [' // TRIM(RoundSigDigits(CondInletTemp,1))//'].')
          CALL ShowContinueErrorTimeStamp(' ')
          CALL ShowContinueError(' Reset reference temperature to one greater than the inlet temperature ')
        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('ChillerHeaterPerformance:Electric:EIR="'// &
             TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)//'": Reference temperature problems continue.',   &
             Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRRefTempErrorIndex,  &
             ReportMaxOf=CondDeltaTemp,ReportMinOf=CondDeltaTemp,  &
             ReportMaxUnits='deltaC',ReportMinUnits='deltaC')
        QCondenser = 0.d0
        IsLoadHeatRemaining = .FALSE.
      END IF

      IF (ChillerHeaterNum > 1 ) THEN
          ! Operation mode needs to be set in a simultaneous clg/htg mode
          ! Always off even heating load remains if this CH is assumed to be off in the loop 1
        IF (SimulClgDominant) THEN
          IF (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvapSimul == 0.d0) THEN
            CurrentMode = 0
            ISLoadHeatRemaining = .FALSE.
          ELSE ! Heat recovery
            CurrentMode = 3
          END IF
        END IF
      END IF ! End of simulataneous clg/htg mode detemination

    ELSE ! chiller heater is off
      IsLoadHeatRemaining = .FALSE.
      CondMassFlowRate = 0.d0
      EvapMassFlowRate = 0.d0
      CurrentMode = 0
      IF (SimulClgDominant) THEN
        IF (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvapSimul > 0.d0) THEN
          CurrentMode = 4   ! Simultaneous cooling dominant mode: 4
        END IF
      END IF ! End of mode determination
    END IF ! End of system operation determinatoin

    IF (IsLoadHeatRemaining .AND. CondMassFlowRate > 0.d0 .AND.  &
        (GetCurrentScheduleValue(Wrapper(WrapperNum)%WrapperComp(CompNum)%CHSchedPtr) > 0)) THEN ! System is on
        ! Operation mode
      IF (SimulHtgDominant) THEN
        IF (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvapSimul == 0.d0) THEN
          CurrentMode = 5 ! No cooling necessary
        ELSE ! Heat recovery mode. Both chilled water and hot water loops are connected. No condenser flow.
          CurrentMode = 3
        END IF
      END IF

        ! Mode 3 and 5 use cooling side data stored from the chilled water loop
        ! Mode 4 uses all data from the chilled water loop due to no heating demand
      IF(SimulClgDominant .OR. CurrentMode == 3) THEN
        CurrentMode = 3
        Cp  = GetSpecificHeatGlycol(PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%FluidName,  &
                              CondInletTemp, &
                              PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%FluidIndex, &
                             'CalcChillerHeaterModel')

        QCondenser = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCondSimul

        IF (Wrapper(WrapperNum)%VariableFlowCH) THEN ! Variable flow
          CondMassFlowRateCalc = QCondenser/CondDeltaTemp/Cp
          IF (CondMassFlowRateCalc > CondMassFlowRate) THEN
              CondMassFlowRateCalc = CondMassFlowRate
              CondDeltaTempCalc = QCondenser/CondMassFlowRate/Cp
              IF(CondDeltaTempCalc > CondDeltaTemp) THEN ! Load to meet should be adjusted
                 CondDeltaTempCalc = CondDeltaTemp
                 QCondenser = CondMassFlowRate * Cp * CondDeltaTemp
              END IF
          END IF
          CondMassFlowRate = CondMassFlowRateCalc
        ELSE ! Constant flow control
          CondDeltaTempCalc = QCondenser/CondMassFlowRate/Cp
          CondOutletTempCalc = CondDeltaTempCalc + CondInletTemp
          IF (CondOutletTempCalc > CondOutletTemp) THEN
            CondOutletTempCalc = CondOutletTemp
            QCondenser = CondMassFlowRate * Cp * CondDeltaTemp
          END IF
          CondOutletTemp = CondOutletTempCalc
        END IF

      ELSE    ! Either Mode 2 or 3 or 5
        IF(SimulHtgDominant) THEN
          CurrentMode = 5
        ELSE
          CurrentMode = 2
        END IF

        ChillerCapFT = 0.0d0
        ChillerEIRFT = 0.0d0
        ChillerEIRFPLR = 0.0d0

          ! Assign curve values to local data array
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCap =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCapClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCOP =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCOPClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefEvapOut =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefEvapOutClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondOut =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondOutClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%OptPartLoadRat =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%OptPartLoadRatClgHtg
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondMode =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondModeHeating
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFT =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTHeating
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFT =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFTHeating
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLR =  &
                                Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLRHeating

        IF (TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondMode) == MakeUPPERCase('EnteringCondenser')) THEN
          CondTempforCurve =  CondInletTemp
        ELSEIF (TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondMode) == MakeUPPERCase('LeavingCondenser')) THEN
          CondTempforCurve = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefCondOutClgHtg  !!CondOutletTemp
        ELSE
          CALL ShowWarningError('ChillerHeaterPerformance:Electric:EIR "'//  &
                             TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)//'":')
          CALL ShowContinueError('Chiller condensor temperature for curve fit are not decided, defalt value= cond_leaving ('// &
                             TRIM(RoundSigDigits(ChillerCapFT,3))//').')
          CondTempforCurve = Node(PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%TempSetPointNodeNum)%TempSetPoint
        ENDIF

        CALL GetCurveMinMaxValues (Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLR,  &
                                    MinPartLoadRat,MaxPartLoadRat)
        ChillerRefCap = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCap
        ReferenceCOP = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%RefCOP
        EvapOutletTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefEvapOutClgHtg
        TempLowLimitEout = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempLowLimitEvapOut
        EvapOutletTempSetpoint = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%TempRefEvapOutClgHtg
        ChillerCapFT = CurveValue(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFT, &
                       EvapOutletTempSetpoint,CondTempforCurve)

        IF(ChillerCapFT .LT. 0)THEN
          IF(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError .LT. 1 .AND. .NOT. WarmupFlag)THEN
             Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError = &
                        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError + 1
            CALL ShowWarningError('ChillerHeaterPerformance:Electric:EIR "'//&
                        TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)//'":')
            CALL ShowContinueError(' ChillerHeater Capacity as a Function of Temperature curve output is negative ('// &
                        TRIM(RoundSigDigits(ChillerCapFT,3))//').')
            CALL ShowContinueError(' Negative value occurs using an Evaporator Outlet Temp of ' // &
                        TRIM(RoundSigDigits(EvapOutletTempSetpoint,1))// &
                        ' and a Condenser Inlet Temp of '//TRIM(RoundSigDigits(CondInletTemp,1))//'.')
            CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
          ELSE IF(.NOT. WarmupFlag)THEN
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError = &
                         Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTError + 1
            CALL ShowRecurringWarningErrorAtEnd('ChillerHeaterPerformance:Electric:EIR "' &
                        //TRIM(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%Name)//'":'//&
             ' ChillerHeater Capacity as a Function of Temperature curve output is negative warning continues...' &
            , Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerCapFTErrorIndex, ChillerCapFT, ChillerCapFT)
          END IF
          ChillerCapFT = 0.0d0
        END IF

          ! Available chiller capacity as a function of temperature
        AvailChillerCap = ChillerRefCap*ChillerCapFT

          ! Part load ratio based on reference capacity and available chiller capacity
        IF(AvailChillerCap > 0)THEN
          PartLoadRat = MAX(0.0d0, MIN((ChillerRefCap/AvailChillerCap),MaxPartLoadRat))
        ELSE
          PartLoadRat = 0.0d0
        END IF

        Cp  = GetSpecificHeatGlycol(PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%FluidName,  &
                              Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp, &
                              PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%FluidIndex, &
                             'CalcChillerHeaterModel')

          ! Calculate evaporator heat transfer
        IF(EvapMassFlowRate > MassFlowTolerance) THEN
           QEvaporator = AvailChillerCap * PartLoadRat
           EvapDeltaTemp = QEvaporator/EvapMassFlowRate/CP
           EvapOutletTemp = EvapInletTemp - EvapDeltaTemp
        END IF

          ! Check that the evaporator outlet temp honors both plant loop temp low limit and also the chiller low limit
        IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
          IF((Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp - TempLowLimitEout) > DeltaTemptol) THEN
            EvapOutletTemp = TempLowLimitEout
            EvapDeltaTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp - EvapOutletTemp
            QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
          ELSE
            EvapOutletTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp
            EvapDeltaTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp - EvapOutletTemp
            QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
          END IF
        END IF

        IF(EvapOutletTemp .LT. Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin) THEN
          IF((Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp - &
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin) .GT. DeltaTemptol) THEN
            EvapOutletTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin
            EvapDeltaTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin - EvapOutletTemp
            QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
          ELSE
            EvapOutletTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin
            EvapDeltaTemp = Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%TempMin - EvapOutletTemp
            QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
          END IF
        END IF

          ! Evaporator operates at full load
        IF(AvailChillerCap .GT. 0.0d0)THEN
          PartLoadRat = MAX(0.0d0,MIN((QEvaporator/AvailChillerCap),MaxPartLoadRat))
        ELSE
          PartLoadRat = 0.0d0
        END IF

          ! Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
        IF (PartLoadRat .LT. MinPartLoadRat) FRAC = MIN(1.0d0,(PartLoadRat/MinPartLoadRat))
          IF (FRAC <= 0.0d0) FRAC = 1.0 ! CR 9303 COP reporting issue, it should be greater than zero in this routine
            ChillerCyclingRatio = FRAC

          ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
        IF(AvailChillerCap .GT. 0.0d0)THEN
           PartLoadRat = Max(PartLoadRat,MinPartLoadRat)
        ELSE
          PartLoadRat = 0.0d0
        END IF
          ! Evaporator part load ratio
        ChillerPartLoadRatio = PartLoadRat

          ! calculate the load due to false loading on chiller over and above water side load
        ChillerFalseLoadRate = (AvailChillerCap * PartLoadRat * FRAC) - QEvaporator
        IF(ChillerFalseLoadRate .LT. SmallLoad) THEN
           ChillerFalseLoadRate = 0.0d0
        END IF

        ChillerEIRFT = MAX(0.d0,CurveValue(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFT,  &
                            EvapOutletTemp,CondTempforCurve))
        ChillerEIRFPLR = MAX(0.d0,CurveValue(Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%ChillerEIRFPLR,PartLoadRat))
        CHPower = (AvailChillerCap/ReferenceCOP) * ChillerEIRFPLR * ChillerEIRFT * FRAC
        ActualCOP = (QEvaporator + ChillerFalseLoadRate)/CHPower
        QCondenser = CHPower*Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%OpenMotorEff +  &
                     QEvaporator + ChillerFalseLoadRate

          ! Determine heating load for this heater and pass the remaining load to the next chiller heater
        CondenserCapMin = QCondenser * MinPartLoadRat
        HeatingLoadToMeet = MIN(QCondenser,MAX(ABS(CondenserLoad),CondenserCapMin))

          ! Set load this chiller heater should meet and temperatures given
        QCondenser = Min(HeatingLoadToMeet,QCondenser)

        Cp  = GetSpecificHeatGlycol(PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%FluidName,  &
                                 CondInletTemp, &
                                 PlantLoop(Wrapper(WrapperNum)%HWLoopNum)%FluidIndex, &
                                 'CalcElectricEIRChillerModel')

          ! Calculate temperatures for constant flow and mass flow rate for variable flow
          ! Limit mass for this chiller heater to the available mass at given temperature conditions
          ! when mass calculated to meet the load is greater than the maximum available
          ! then recalculate heating load this chiller heater can meet
        IF (CurrentMode == 2 .OR. SimulHtgDominant) THEN
          IF(CondMassFlowRate > MassFlowTolerance .AND. CondDeltaTemp > 0.0d0) THEN
            IF (Wrapper(WrapperNum)%VariableFlowCH) THEN ! Variable flow
              CondMassFlowRateCalc = QCondenser/CondDeltaTemp/Cp
              IF (CondMassFlowRateCalc > CondMassFlowRate) THEN
                CondMassFlowRateCalc = CondMassFlowRate
                CondDeltaTempCalc = QCondenser/CondMassFlowRate/Cp
                  IF(CondDeltaTempCalc > CondDeltaTemp) THEN ! Load to meet should be adjusted
                    CondDeltaTempCalc = CondDeltaTemp
                    QCondenser = CondMassFlowRate * Cp * CondDeltaTemp
                  END IF
              END IF
              CondMassFlowRate = CondMassFlowRateCalc
            ELSE ! Constant Flow at a fixed flow rate and capacity
              CondDeltaTempCalc = QCondenser/CondMassFlowRate/Cp
              CondOutletTempCalc = CondDeltaTempCalc + CondInletTemp
              IF(CondOutletTempCalc > CondOutletTemp) THEN ! Load to meet should be adjusted
                CondOutletTempCalc = CondOutletTemp
                QCondenser = CondMassFlowRate * Cp * CondDeltaTemp
              END IF
              CondOutletTemp = CondOutletTempCalc
            END IF
          ELSE
            QCondenser = 0.0d0
            CondOutletTemp = CondInletTemp
          END IF
        END IF

      END IF  ! End of calculaton dependiong on the modes

          ! Determine load next chiller heater meets
        IF (CondenserLoad < QCondenser) THEN ! Heating load is met by this chiller heater
          CondenserLoad = 0.d0
        ELSE
          CondenserLoad = CondenserLoad - QCondenser
        END IF

        IF (QCondenser == 0.d0) THEN
            CurrentMode = 0
            ChillerPartLoadRatio = 0.d0
            ChillerCyclingRatio  = 0.d0
            ChillerFalseLoadRate = 0.d0
            EvapMassFlowRate     = 0.d0
            CondMassFlowRate     = 0.d0
            CHPower              = 0.d0
            QEvaporator          = 0.d0
            CondenserFanPower    = 0.d0
            EvapOutletTemp = EvapInletTemp
            CondOutletTemp = CondInletTemp
            CondenserLoad = 0.d0
        END IF

          ! Heat recovery or cooling dominant modes need to use the evaporator side information
        IF (CurrentMode == 3 .OR. CurrentMode == 4) THEN
          ChillerPartLoadRatio = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerPartLoadRatioSimul
          ChillerCyclingRatio = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCyclingRatioSimul
          ChillerFalseLoadRate = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadRateSimul
          ChillerCapFT = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCapFTSimul
          ChillerEIRFT = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFTSimul
          ChillerEIRFPLR = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFPLRSimul
          QEvaporator = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvapSimul
          EvapOutletTemp = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTempSimul
          EvapInletTemp = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapInletTempSimul
          EvapMassFlowRate = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapmdotSimul
          IF (SimulClgDominant) THEN
            CHPower = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPowerSimul
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower = 0.d0
          END IF
        END IF
      END IF

        ! Check if it is mode 4, then skip binding local variables
      IF (CurrentMode == 4) THEN
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode = CurrentMode
      ELSE
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%MassFlowRate = EvapMassFlowRate
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondOutletNode%MassFlowRate = CondMassFlowRate
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%Temp = EvapOutletTemp
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp = EvapInletTemp
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondOutletNode%Temp = CondOutletTemp
        Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%Temp = CondInletTemp
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode = CurrentMode
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerPartLoadRatio = ChillerPartLoadRatio
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCyclingRatio  = ChillerCyclingRatio
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadRate = ChillerFalseLoadRate
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCapFT = ChillerCapFT
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFT = ChillerEIRFT
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFPLR = ChillerEIRFPLR
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPower = CoolingPower
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower = CHPower
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap = QEvaporator
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond = QCondenser
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTemp = EvapOutletTemp
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapInletTemp = EvapInletTemp
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTemp = CondOutletTemp
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondInletTemp = CondInletTemp
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot = EvapMassFlowRate
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot = CondMassFlowRate
        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ActualCOP = ActualCOP
      END IF

  ENDDO

  RETURN

END SUBROUTINE CalcChillerheaterModel

SUBROUTINE CalcWrapperModel(WrapperNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,LoopNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang, PNNL
          !       DATE WRITTEN   Feb 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Calculate node information connected to plnat & condenser loop

          ! METHODOLOGY EMPLOYED:
          !  Use empirical curve fits to model performance at off-reference conditions

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : WarmupFlag, CurrentTime, DayOfSim, HourOfDay, TimeStep, outputfiledebug
  USE DataHVACGlobals, ONLY : SmallLoad, TimeStepSys, SmallMassFlow
  USE CurveManager,    ONLY : CurveValue
  USE DataPlant,       ONLY : DeltaTemptol, TypeOf_CentralGroundSourceHeatPump
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE PlantUtilities,  ONLY : SetComponentFlowRate
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: WrapperNum
  REAL(r64), INTENT(INOUT)     :: MyLoad
  LOGICAL, INTENT(IN)          :: RunFlag
  LOGICAL, INTENT(IN)          :: FirstIteration
  INTEGER, INTENT(IN)          :: EquipFlowCtrl
  INTEGER, INTENT(IN)          :: LoopNum

          ! LOCAL VARIABLES
  INTEGER   :: ChillerHeaterNum        ! Chiller heater number
  INTEGER   :: GSHeatPumpNum           ! Heat pump number
  INTEGER   :: CHWInletNodeNum         ! Chiller heater bank chilled water inlet node number
  INTEGER   :: CHWOutletNodeNum        ! Chiller heater bank chilled water Outlet node number
  INTEGER   :: GLHEInletNodeNum        ! Chiller heater bank condenser water inlet node number
  INTEGER   :: GLHEOutletNodeNum       ! Chiller heater bank condenser water outlet node number
  INTEGER   :: HWInletNodeNum          ! Chiller heater bank hot water inlet node number
  INTEGER   :: HWOutletNodeNum         ! Chiller heater bank hot water outlet node number
  INTEGER   :: EvapInletNode           ! Individual chiller heater evaporator inlet node
  INTEGER   :: EvapOutletNode          ! Individual chiller heater evaporator outlet node
  INTEGER   :: CondInletNode           ! Individual chiller heater condenser inlet node
  INTEGER   :: LoopSideNum             ! Loop side number
  INTEGER   :: LoopSide                ! Loop side
  INTEGER   :: BranchNum               ! Branch number
  INTEGER   :: CompNum                 ! Component number
  INTEGER   :: OpMode                  ! Operation mode
  INTEGER   :: ChillerHeaterNums       ! Total number of chiller heaters
  REAL(r64) :: CurCoolingLoad          ! Total cooling load chiller heater bank (wrapper) meets
  REAL(r64) :: CurHeatingLoad          ! Total heating load chiller heater bank (wrapper) meets
  REAL(r64) :: CHWInletTemp            ! Chiller heater bank chilled water inlet temperature
  REAL(r64) :: CHWOutletTemp           ! Chiller heater bank chilled water outlet temperature
  REAL(r64) :: CHWInletMassFlowRate    ! Chiller heater bank chilled water inlet mass flow rate
  REAL(r64) :: CHWOutletMassFlowRate   ! Chiller heater bank chilled water outlet mass flow rate
  REAL(r64) :: CHWBypassMassFlowRate   ! Chiller heater bank chilled water bypass mass flow rate
  REAL(r64) :: HWInletTemp             ! Chiller heater bank hot water inlet temperature
  REAL(r64) :: HWOutletTemp            ! Chiller heater bank hot water outlet temperature
  REAL(r64) :: HWInletMassFlowRate     ! Chiller heater bank hot water inlet mass flow rate
  REAL(r64) :: HWOutletMassFlowRate    ! Chiller heater bank hot water outlet mass flow rate
  REAL(r64) :: HWBypassMassFlowRate    ! Chiller heater bank hot water bypass mass flow rate
  REAL(r64) :: GLHEInletTemp           ! Chiller heater bank condenser loop inlet temperature
  REAL(r64) :: GLHEOutletTemp          ! Chiller heater bank condenser loop outlet temperature
  REAL(r64) :: GLHEInletMassFlowRate   ! Chiller heater bank condenser loop intlet mass flow rate
  REAL(r64) :: GLHEOutletMassFlowRate  ! Chiller heater bank condenser loop outlet mass flow rate
  REAL(r64) :: GLHEBypassMassFlowRate  ! Chiller heater bank condenser loop bypass mass flow rate
  REAL(r64) :: CHWMassFlowBypass       ! Chilled water bypass flow rate
  REAL(r64) :: HWMassFlowBypass        ! Hot water bypass flow rate
  REAL(r64) :: GLHEMassFlowBypass      ! Condenser loop bypass flow rate
  REAL(r64) :: WrapperElecPowerCool    ! Chiller heater bank total cooling electricity [W]
  REAL(r64) :: WrapperElecPowerHeat    ! Chiller heater bank total heating electricity [W]
  REAL(r64) :: WrapperCoolRate         ! Chiller heater bank total cooling rate [W]
  REAL(r64) :: WrapperHeatRate         ! Chiller heater bank total heating rate [W]
  REAL(r64) :: WrapperGLHERate         ! Chiller heater bank total condenser heat transfer rate [W]
  REAL(r64) :: WrapperElecEnergyCool   ! Chiller heater bank total electric cooling energy [J]
  REAL(r64) :: WrapperElecEnergyHeat   ! Chiller heater bank total electric heating energy [J]
  REAL(r64) :: WrapperCoolEnergy       ! Chiller heater bank total cooling energy [J]
  REAL(r64) :: WrapperHeatEnergy       ! Chiller heater bank total heating energy [J]
  REAL(r64) :: WrapperGLHEEnergy       ! Chiller heater bank total condenser heat transfer energy [J]
  INTEGER   :: CurrentMode             ! Current operation mode indicator

    ! Read note information
  CHWInletNodeNum  = Wrapper(WrapperNum)%CHWInletNodeNum
  CHWOutletNodeNum = Wrapper(WrapperNum)%CHWOutletNodeNum
  HWInletNodeNum  = Wrapper(WrapperNum)%HWInletNodeNum
  HWOutletNodeNum = Wrapper(WrapperNum)%HWOutletNodeNum
  GLHEInletNodeNum  = Wrapper(WrapperNum)%GLHEInletNodeNum
  GLHEOutletNodeNum = Wrapper(WrapperNum)%GLHEOutletNodeNum

  CHWInletMassFlowRate  = 0.d0
  HWInletMassFlowRate   = 0.d0
  GLHEInletMassFlowRate = 0.d0
  CHWInletTemp = Node(CHWInletNodeNum)%Temp
  HWInletTemp  = Node(HWInletNodeNum)%Temp
  GLHEInletTemp = Node(GLHEInletNodeNum)%Temp

  ChillerHeaterNums = Wrapper(WrapperNum)%ChillerheaterNums

    ! Initiate loads and inlet temperatures each loop
  IF (LoopNum == Wrapper(WrapperNum)%CWLoopNum) THEN
    CHWInletMassFlowRate  = Node(CHWInletNodeNum)%MassFlowRateMaxAvail
    HWInletMassFlowRate   = Node(HWInletNodeNum)%MassFlowRate
    GLHEInletMassFlowRate = Node(GLHEInletNodeNum)%MassFlowRateMaxAvail
    LoopSideNum = Wrapper(WrapperNum)%CWLoopSideNum
    LoopSide = Wrapper(WrapperNum)%CWLoopSideNum
    CurCoolingLoad = 0.d0
    Wrapper(WrapperNum)%WrapperCoolingLoad = 0.d0
    CurCoolingLoad = ABS(MyLoad)
    Wrapper(WrapperNum)%WrapperCoolingLoad = CurCoolingLoad
      ! Set actual mass flow rate at the nodes when it's locked
    IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      CHWInletMassFlowRate = Node(CHWInletNodeNum)%MassFlowRate
    END IF
    IF(CHWInletMassFlowRate == 0.d0) GLHEInletMassFlowRate = 0.d0

  ELSEIF (LoopNum == Wrapper(WrapperNum)%HWLoopNum) THEN
    CHWInletMassFlowRate  = Node(CHWInletNodeNum)%MassFlowRate
    HWInletMassFlowRate   = Node(HWInletNodeNum)%MassFlowRateMaxAvail
    GLHEInletMassFlowRate = Node(GLHEInletNodeNum)%MassFlowRateMaxAvail
    LoopSideNum = Wrapper(WrapperNum)%HWLoopSideNum
    CurHeatingLoad = 0.d0
    Wrapper(WrapperNum)%WrapperHeatingLoad = 0.d0
    CurHeatingLoad = MyLoad
    Wrapper(WrapperNum)%WrapperHeatingLoad = CurHeatingLoad
      ! Set actual mass flow rate at the nodes when it's locked
    IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      HWInletMassFlowRate = Node(HWInletNodeNum)%MassFlowRate
    END IF
    IF(HWInletMassFlowRate == 0.d0) GLHEInletMassFlowRate = 0.d0
  END IF

    ! Initialize local variables
  WrapperElecPowerCool = 0.0d0
  WrapperElecPowerHeat = 0.0d0
  WrapperCoolRate = 0.0d0
  WrapperHeatRate = 0.0d0
  WrapperGLHERate = 0.0d0
  WrapperElecEnergyCool = 0.0d0
  WrapperElecEnergyHeat = 0.0d0
  WrapperCoolEnergy = 0.0d0
  WrapperHeatEnergy = 0.0d0
  WrapperGLHEEnergy = 0.0d0

  IF (LoopNum == Wrapper(WrapperNum)%CWLoopNum) THEN
    IF (Wrapper(WrapperNum)%ControlMode == SmartMixing) THEN
      IF (CurCoolingLoad > 0.d0 .AND. CHWInletMassFlowRate > 0.d0 .AND. GLHEInletMassFlowRate > 0 ) THEN

        CALL CalcChillerModel(WrapperNum,OpMode,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,LoopNum)
        CALL UpdateChillerRecords(WrapperNum)

          ! Initialize local variables only for calculating mass-weighed temperatures
        CHWOutletTemp  = 0.0d0
        HWOutletTemp   = 0.0d0
        GLHEOutletTemp = 0.0d0
        CHWOutletMassFlowRate  = 0.0d0
        HWOutletMassFlowRate   = 0.0d0
        GLHEOutletMassFlowRate = 0.0d0

        DO ChillerHeaterNum=1,ChillerHeaterNums

            ! Calculated mass flow rate used by individual chiller heater and bypasses
          CHWOutletMassFlowRate = CHWOutletMassFlowRate + &
                                  Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot
          CHWOutletTemp = CHWOutletTemp + &
                          Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTemp * &
                          (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot/CHWInletMassFlowRate)
          WrapperElecPowerCool = WrapperElecPowerCool + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPower
          WrapperCoolRate = WrapperCoolRate + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap
          WrapperElecEnergyCool = WrapperElecEnergyCool + &
                                  Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingEnergy
          WrapperCoolEnergy = WrapperCoolEnergy + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergy
          IF (GLHEInletMassFlowRate > 0.d0) THEN
            GLHEOutletMassFlowRate = GLHEOutletMassFlowRate + &
                                     Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot
              IF (GLHEOutletMassFlowRate > GLHEInletMassFlowRate) GLHEOutletMassFlowRate = GLHEInletMassFlowRate
            GLHEOutletTemp = GLHEOutletTemp + &
                             Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTemp * &
                             (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot/GLHEInletMassFlowRate)
            WrapperGLHERate = WrapperGLHERate + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond
            WrapperGLHEEnergy = WrapperGLHEEnergy + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergy
          ELSE
            GLHEInletMassFlowRate = 0.d0
            GLHEOutletMassFlowRate = 0.d0
            GLHEOutletTemp = GLHEInletTemp
            WrapperGLHERate = 0.d0
            WrapperGLHEEnergy = 0.d0
          END IF
        END DO ! End of summation of mass flow rates and mass weighted temperatrue

          ! Calculate temperatures for the mixed flows in the chiller bank
        CHWBypassMassFlowRate = CHWInletMassFlowRate - CHWOutletMassFlowRate
          IF(CHWBypassMassFlowRate > 0.d0) THEN
            CHWOutletTemp = CHWOutletTemp + CHWInletTemp * CHWBypassMassFlowRate/CHWInletMassFlowRate
          ELSE
            CHWOutletTemp = CHWOutletTemp
          END IF

          IF(GLHEInletMassFlowRate > 0.d0) THEN
            GLHEBypassMassFlowRate = GLHEInletMassFlowRate - GLHEOutletMassFlowRate
              IF (GLHEBypassMassFlowRate > 0.d0) THEN
                GLHEOutletTemp = GLHEOutletTemp + GLHEInletTemp * GLHEBypassMassFlowRate/GLHEInletMassFlowRate
              ELSE
                GLHEOutletTemp = GLHEOutletTemp
              END IF
          ELSE
            GLHEOutletTemp = GLHEInletTemp
          END IF

        HWOutletTemp = HWInletTemp

        IF (GetCurrentScheduleValue(Wrapper(WrapperNum)%SchedPtr) > 0) THEN
          WrapperElecPowerCool = WrapperElecPowerCool + (Wrapper(WrapperNum)%AncilliaryPower *  &
                                                        GetCurrentScheduleValue(Wrapper(WrapperNum)%SchedPtr))
        END IF

        Node(CHWOutletNodeNum)%Temp  = CHWOutletTemp
        Node(HWOutletNodeNum)%Temp   = HWOutletTemp
        Node(GLHEOutletNodeNum)%Temp = GLHEOutletTemp

      ELSE

          ! Initialize local variables
        CHWOutletTemp = CHWInletTemp
        HWOutletTemp = HWInletTemp
        GLHEOutletTemp = GLHEInletTemp

        DO ChillerHeaterNum=1, ChillerHeaterNums
           Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%MassFlowRate = 0.d0
           Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondOutletNode%MassFlowRate = 0.d0
           Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%Temp = CHWInletTemp
           Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp = CHWInletTemp
           Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondOutletNode%Temp = GLHEInletTemp
           Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%Temp = GLHEInletTemp
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode = 0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerPartLoadRatio = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCyclingRatio  = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadRate = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCapFT = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFT = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFPLR = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPower = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTemp = CHWOutletTemp
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapInletTemp = CHWInletTemp
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTemp = GLHEOutletTemp
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondInletTemp = GLHEInletTemp
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoad = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingEnergy = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingEnergy = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergy = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergy = 0.d0
           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ActualCOP = 0.d0
        END DO

      END IF

      IF (SimulHtgDominant .OR. SimulClgDominant) THEN
        Node(CHWOutletNodeNum)%Temp = CHWOutletTemp
        WrapperReport(WrapperNum)%CHWInletTempSimul = CHWInletTemp
        WrapperReport(WrapperNum)%CHWOutletTempSimul = CHWOutletTemp
        WrapperReport(WrapperNum)%CHWmdotSimul = CHWInletMassFlowRate
        WrapperReport(WrapperNum)%GLHEInletTempSimul = GLHEInletTemp
        WrapperReport(WrapperNum)%GLHEOutletTempSimul = GLHEOutletTemp
        WrapperReport(WrapperNum)%GLHEmdotSimul = GLHEInletMassFlowRate
        WrapperReport(WrapperNum)%TotElecCoolingSimul = WrapperElecEnergyCool
        WrapperReport(WrapperNum)%CoolingEnergySimul = WrapperCoolEnergy
        WrapperReport(WrapperNum)%TotElecCoolingPwrSimul = WrapperElecPowerCool
        WrapperReport(WrapperNum)%CoolingRateSimul = WrapperCoolRate

      ELSE

      Node(CHWOutletNodeNum)%Temp  = CHWOutletTemp
      Node(HWOutletNodeNum)%Temp   = HWOutletTemp
      Node(GLHEOutletNodeNum)%Temp = GLHEOutletTemp
      WrapperReport(WrapperNum)%CHWInletTemp = CHWInletTemp
      WrapperReport(WrapperNum)%CHWOutletTemp = CHWOutletTemp
      WrapperReport(WrapperNum)%HWInletTemp = HWInletTemp
      WrapperReport(WrapperNum)%HWOutletTemp = HWOutletTemp
      WrapperReport(WrapperNum)%GLHEInletTemp = GLHEInletTemp
      WrapperReport(WrapperNum)%GLHEOutletTemp = GLHEOutletTemp
      WrapperReport(WrapperNum)%CHWmdot = CHWInletMassFlowRate
      WrapperReport(WrapperNum)%HWmdot = HWInletMassFlowRate
      WrapperReport(WrapperNum)%GLHEmdot = GLHEInletMassFlowRate
      WrapperReport(WrapperNum)%TotElecCooling = WrapperElecEnergyCool
      WrapperReport(WrapperNum)%TotElecHeating = WrapperElecEnergyHeat
      WrapperReport(WrapperNum)%CoolingEnergy = WrapperCoolEnergy
      WrapperReport(WrapperNum)%HeatingEnergy = WrapperHeatEnergy
      WrapperReport(WrapperNum)%GLHEEnergy = WrapperGLHEEnergy
      WrapperReport(WrapperNum)%TotElecCoolingPwr = WrapperElecPowerCool
      WrapperReport(WrapperNum)%TotElecHeatingPwr = WrapperElecPowerHeat
      WrapperReport(WrapperNum)%CoolingRate = WrapperCoolRate
      WrapperReport(WrapperNum)%HeatingRate = WrapperHeatRate
      WrapperReport(WrapperNum)%GLHERate = WrapperGLHERate

      END IF
        CALL SetComponentFlowRate(CHWInletMassFlowRate, CHWInletNodeNum, CHWOutletNodeNum, &
             Wrapper(WrapperNum)%CWLoopNum,       &
             Wrapper(WrapperNum)%CWLoopSideNum,   &
             Wrapper(WrapperNum)%CWBranchNum,     &
             Wrapper(WrapperNum)%CWCompNum)

        CALL SetComponentFlowRate(HWInletMassFlowRate, HWInletNodeNum, HWOutletNodeNum,  &
             Wrapper(WrapperNum)%HWLoopNum,     &
             Wrapper(WrapperNum)%HWLoopSideNum, &
             Wrapper(WrapperNum)%HWBranchNum,   &
             Wrapper(WrapperNum)%HWCompNum)

        CALL SetComponentFlowRate(GLHEInletMassFlowRate, GLHEInletNodeNum, GLHEOutletNodeNum, &
             Wrapper(WrapperNum)%GLHELoopNum,        &
             Wrapper(WrapperNum)%GLHELoopSideNum,    &
             Wrapper(WrapperNum)%GLHEBranchNum,      &
             Wrapper(WrapperNum)%GLHECompNum)

    END IF ! End of cooling

  ELSE IF (LoopNum == Wrapper(WrapperNum)%HWLoopNum) THEN ! Hot water loop
    IF (Wrapper(WrapperNum)%ControlMode == SmartMixing) THEN ! Chiller heater component
      IF (CurHeatingLoad > 0.d0 .AND. HWInletMassFlowRate > 0.0d0) THEN

        CALL CalcChillerHeaterModel(WrapperNum,OpMode,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,LoopNum)
        CALL UpdateChillerHeaterRecords(WrapperNum)

          ! Calculate individual CH units's temperatures and mass flow rates
        CHWOutletTemp  = 0.0d0
        HWOutletTemp   = 0.0d0
        GLHEOutletTemp = 0.0d0
        CHWOutletMassFlowRate  = 0.0d0
        HWOutletMassFlowRate   = 0.0d0
        GLHEOutletMassFlowRate = 0.0d0

        IF (SimulHtgDominant .OR. SimulClgDominant) THEN
          IF (SimulClgDominant) THEN
            DO ChillerHeaterNum=1,ChillerHeaterNums
              CurrentMode = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode
              CHWInletTemp = WrapperReport(WrapperNum)%CHWInletTempSimul
              GLHEInletTemp = WrapperReport(WrapperNum)%GLHEInletTempSimul
              CHWInletMassFlowRate = WrapperReport(WrapperNum)%CHWmdotSimul
              GLHEInletMassFlowRate = WrapperReport(WrapperNum)%GLHEmdotSimul

              IF (CurrentMode /= 0) THEN  ! This chiller heater unit is on
                IF (CurrentMode == 3) THEN ! Heat recovery mode. Both chilled water and hot water connections
                  CHWOutletMassFlowRate = CHWOutletMassFlowRate + &  ! Wrapper evaporator side to plant chilled water loop
                                          Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapmdotSimul
                  HWOutletMassFlowRate = HWOutletMassFlowRate + &  ! Wrapper condenser side to plant hot water loop
                                         Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot
                  IF (HWInletMassFlowRate > 0.d0) THEN
                    HWOutletTemp = HWOutletTemp + &  ! Only calculate in the heat recovery mode
                             Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTemp * &
                             (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot/HWInletMassFlowRate)
                  ELSE
                    HWOutletTemp = HWInletTemp
                  END IF
                ELSE ! Mode 4. Cooling-only mode with other heat recovery units. Condenser flows.
                  CHWOutletMassFlowRate = CHWOutletMassFlowRate + &  ! Wrapper evaporator side to plant chilled water loop
                                          Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapmdotSimul
                    ! Sum condenser node mass flow rates and mass weighed temperatures
                  IF (GLHEInletMassFlowRate > 0.d0) THEN
                    GLHEOutletMassFlowRate = GLHEOutletMassFlowRate + &
                                     Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondmdotSimul
                      IF (GLHEOutletMassFlowRate > GLHEInletMassFlowRate) GLHEOutletMassFlowRate = GLHEInletMassFlowRate
                    GLHEOutletTemp = GLHEOutletTemp + &
                             Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTempSimul * &
                             (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondmdotSimul/GLHEInletMassFlowRate)
                    WrapperGLHERate = WrapperGLHERate + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCondSimul
                    WrapperGLHEEnergy = WrapperGLHEEnergy + &
                                        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergySimul
                  ELSE
                    GLHEInletMassFlowRate = 0.d0
                    GLHEOutletMassFlowRate = 0.d0
                    GLHEOutletTemp = GLHEInletTemp
                    WrapperGLHERate = 0.d0
                    WrapperGLHEEnergy = 0.d0
                  END IF
                END IF
              ELSE ! This chiller heater is off
                  ! Check if any unit is cooling only mode
                IF (ChillerHeaterNum == ChillerHeaterNums) THEN ! All units are heat revocery mode. No condenser flow
                  GLHEOutletMassFlowRate = 0.d0
                  GLHEInletMassFlowRate = 0.d0
                  GLHEOutletTemp = GLHEInletTemp
                ELSE ! At leaset, one of chiller heater units is cooling-only mode
                  GLHEOutletMassFlowRate = GLHEOutletMassFlowRate
                  GLHEOutletTemp = GLHEOutletTemp
                END IF
              END IF
                ! Calculate mass weighed chilled water temperatures
              IF (CHWInletMassFlowRate > 0.d0) THEN
                CHWOutletTemp = CHWOutletTemp + &
                        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTempSimul * &
                        (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapmdotSimul/CHWInletMassFlowRate)
              ELSE
                CHWOutletTemp = CHWInletTemp
              END IF

              WrapperElecPowerCool = WrapperElecPowerCool +  &  ! Cooling electricity
                                Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPowerSimul
              WrapperCoolRate = WrapperCoolRate + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvapSimul
              WrapperElecEnergyCool = WrapperElecEnergyCool +  &
                                Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingEnergySimul
              WrapperCoolEnergy = WrapperCoolEnergy + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergySimul
                ! Avoid double counting wrapper energy use
              WrapperElecPowerHeat = 0.d0
              WrapperHeatRate = 0.d0
              WrapperElecEnergyHeat = 0.d0
              WrapperHeatEnergy = 0.d0

            END DO

               ! Calculate chilled water temperature
            IF (CHWInletMassFlowRate > 0.d0) THEN
              CHWBypassMassFlowRate = CHWInletMassFlowRate - CHWOutletMassFlowRate
                IF(CHWBypassMassFlowRate > 0.d0) THEN
                  CHWOutletTemp = CHWOutletTemp + CHWInletTemp * CHWBypassMassFlowRate/CHWInletMassFlowRate
                ELSE ! No bypass withnin a wrapper
                  CHWOutletTemp = CHWOutletTemp
                END IF
            ELSE
              CHWOutletTemp = CHWInletTemp
            END IF
              ! Calculate hot water outlet temperature
            IF (HWInletMassFlowRate > 0.d0) THEN
              HWBypassMassFlowRate = HWInletMassFlowRate - HWOutletMassFlowRate
                IF (HWBypassMassFlowRate > 0.d0) THEN
                  HWOutletTemp = HWOutletTemp + HWInletTemp *  HWBypassMassFlowRate/HWInletMassFlowRate
                ELSE
                  HWOutletTemp = HWOutletTemp
                END IF
            ELSE
              HWOutletTemp = HWInletTemp
            END IF
              ! Calculate condenser outlet temperature
            IF(GLHEInletMassFlowRate > 0.d0) THEN
              GLHEBypassMassFlowRate = GLHEInletMassFlowRate - GLHEOutletMassFlowRate
                IF (GLHEBypassMassFlowRate > 0.d0) THEN
                  GLHEOutletTemp = GLHEOutletTemp + GLHEInletTemp * GLHEBypassMassFlowRate/GLHEInletMassFlowRate
                ELSE
                  GLHEOutletTemp = GLHEOutletTemp
                END IF
            ELSE
              GLHEOutletTemp = GLHEInletTemp
            END IF

              ! Add ancilliary power if scheduled
            IF (GetCurrentScheduleValue(Wrapper(WrapperNum)%SchedPtr) > 0) THEN
              WrapperElecPowerCool = WrapperElecPowerCool + (Wrapper(WrapperNum)%AncilliaryPower *  &
                                                            GetCurrentScheduleValue(Wrapper(WrapperNum)%SchedPtr))
            END IF

              ! Electricity should be counted once for cooling in this mode
            WrapperElecEnergyHeat = 0.d0

          ELSE IF (SimulHtgDominant) THEN ! Heating dominant simultaneous clg/htg mode

            DO ChillerHeaterNum=1,ChillerHeaterNums
                ! Set temperatures and mass flow rates for the cooling side
              CurrentMode = Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode
              CHWInletTemp = WrapperReport(WrapperNum)%CHWInletTempSimul
              CHWInletMassFlowRate = WrapperReport(WrapperNum)%CHWmdotSimul

              IF (CurrentMode /= 0) THEN ! This chiller heater unit is on
                IF (CurrentMode == 3) THEN ! Heat recovery mode. Both chilled water and hot water connections
                  CHWOutletMassFlowRate = CHWOutletMassFlowRate + &  ! Wrapper evaporator side to plant chilled water loop
                                          Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapmdotSimul
                  HWOutletMassFlowRate = HWOutletMassFlowRate + &  ! Wrapper condenser side to plant hot water loop
                                         Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot
                  IF (CHWInletMassFlowRate > 0.d0) THEN
                    CHWOutletTemp = CHWOutletTemp + &  ! Only need to calculate in the heat recovery mode
                             Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTempSimul * &
                             (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapmdotSimul/CHWInletMassFlowRate)
                  ELSE
                    CHWOutletTemp = CHWInletTemp
                  END IF
                ELSE ! Mode 5. Heating only mode with other heat recovery units
                  HWOutletMassFlowRate = HWOutletMassFlowRate + &  ! Wrapper condenser side to plant hot water loop
                                         Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot
                  IF (GLHEInletMassFlowRate > 0.d0) THEN
                    GLHEOutletMassFlowRate = GLHEOutletMassFlowRate + &  ! Wrapper evaporator side to plant condenser loop
                                     Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot
                      IF (GLHEOutletMassFlowRate > GLHEInletMassFlowRate) GLHEOutletMassFlowRate = GLHEInletMassFlowRate
                    GLHEOutletTemp = GLHEOutletTemp + &
                             Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTemp * &
                             (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot/GLHEInletMassFlowRate)
                    WrapperGLHERate = WrapperGLHERate + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap
                    WrapperGLHEEnergy = WrapperGLHEEnergy + &
                                        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergy
                  ELSE
                    GLHEInletMassFlowRate = 0.d0
                    GLHEOutletMassFlowRate = 0.d0
                    GLHEOutletTemp = GLHEInletTemp
                    WrapperGLHERate = 0.d0
                    WrapperGLHEEnergy = 0.d0
                  END IF
                END IF ! End of heat recovery mode

              ELSE ! This chiller heater is off

                  ! Check if any unit is heating only mode
                IF (ChillerHeaterNum == ChillerHeaterNums) THEN ! All are heat revocery mode. No condenser flow
                  GLHEOutletMassFlowRate = 0.d0
                  GLHEInletMassFlowRate = 0.d0
                  GLHEOutletTemp = GLHEInletTemp
                ELSE ! At leaset, one of chiller heater units is heating only mode
                  GLHEOutletMassFlowRate = GLHEOutletMassFlowRate
                  GLHEOutletTemp = GLHEOutletTemp
                END IF
              END IF

                ! Calculate mass weighed hot water temperatures
              IF (HWInletMassFlowRate > 0.d0) THEN
                HWOutletTemp = HWOutletTemp + &  ! Always heating as long as heating load remains
                         Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTemp * &
                         (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot/HWInletMassFlowRate)
              ELSE
                HWOutletTemp = HWInletTemp
              END IF

              WrapperElecPowerHeat = WrapperElecPowerHeat +  &
                                 Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower
              WrapperHeatRate = WrapperHeatRate + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond
              WrapperElecEnergyHeat = WrapperElecEnergyHeat +  &
                                  Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingEnergy
              WrapperHeatEnergy = WrapperHeatEnergy + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergy

                ! Avoid double counting wrapper energy use
              WrapperElecPowerCool = 0.d0
              WrapperCoolRate = 0.d0
              WrapperElecEnergyCool = 0.d0
              WrapperElecEnergyCool = 0.d0
            END DO
              ! Calculate chilled water outlet temperature
            IF (CHWInletMassFlowRate > 0.d0) THEN
              CHWBypassMassFlowRate = CHWInletMassFlowRate - CHWOutletMassFlowRate
                IF(CHWBypassMassFlowRate > 0.d0) THEN
                  CHWOutletTemp = CHWOutletTemp + CHWInletTemp * CHWBypassMassFlowRate/CHWInletMassFlowRate
                ELSE ! No bypass withnin a wrapper
                  CHWOutletTemp = CHWOutletTemp
                END IF
            ELSE
              CHWOutletTemp = CHWInletTemp
            END IF
              ! Calculate hot water outlet temperature
            IF (HWInletMassFlowRate > 0.d0) THEN
              HWBypassMassFlowRate = HWInletMassFlowRate - HWOutletMassFlowRate
                IF (HWBypassMassFlowRate > 0.d0) THEN
                  HWOutletTemp = HWOutletTemp + HWInletTemp *  HWBypassMassFlowRate/HWInletMassFlowRate
                ELSE
                  HWOutletTemp = HWOutletTemp
                END IF
            ELSE
              HWOutletTemp = HWInletTemp
            END IF
              ! Calculate condenser outlet temperature
            IF(GLHEInletMassFlowRate > 0.d0) THEN
              GLHEBypassMassFlowRate = GLHEInletMassFlowRate - GLHEOutletMassFlowRate
                IF (GLHEBypassMassFlowRate > 0.d0) THEN
                  GLHEOutletTemp = GLHEOutletTemp + GLHEInletTemp * GLHEBypassMassFlowRate/GLHEInletMassFlowRate
                ELSE
                  GLHEOutletTemp = GLHEOutletTemp
                END IF
            ELSE
              GLHEOutletTemp = GLHEInletTemp
            END IF

              ! Check if ancilliary power is used
            IF (GetCurrentScheduleValue(Wrapper(WrapperNum)%SchedPtr) > 0) THEN
              WrapperElecPowerHeat = WrapperElecPowerHeat + (Wrapper(WrapperNum)%AncilliaryPower *  &
                                                            GetCurrentScheduleValue(Wrapper(WrapperNum)%SchedPtr))
            END IF

                 ! Electricity should be counted once
              WrapperElecEnergyCool = 0.d0

          END IF  ! End of simultaneous clg/htg mode calculations

        ELSE ! Heating only mode (mode 2)

          DO ChillerHeaterNum=1,ChillerHeaterNums
            HWOutletMassFlowRate = HWOutletMassFlowRate + &
                                   Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot
            HWOutletTemp = HWOutletTemp + &
                           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTemp * &
                           Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot/HWInletMassFlowRate
            WrapperElecPowerHeat = WrapperElecPowerHeat +  &
                                   Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower
            WrapperHeatRate = WrapperHeatRate + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond
            WrapperElecEnergyHeat = WrapperElecEnergyHeat +  &
                                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingEnergy
            WrapperHeatEnergy = WrapperHeatEnergy + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergy

            IF (GLHEInletMassFlowRate > 0.d0) THEN
              GLHEOutletMassFlowRate = GLHEOutletMassFlowRate + &
                                     Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot
                IF (GLHEOutletMassFlowRate > GLHEInletMassFlowRate) GLHEOutletMassFlowRate = GLHEInletMassFlowRate
              GLHEOutletTemp = GLHEOutletTemp + &
                             Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTemp * &
                             (Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot/GLHEInletMassFlowRate)
              WrapperGLHERate = WrapperGLHERate + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap
              WrapperGLHEEnergy = WrapperGLHEEnergy + Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergy
            ELSE ! No source water flow
              GLHEOutletMassFlowRate = 0.d0
              GLHEInletMassFlowRate = 0.d0
              GLHEOutletTemp = GLHEInletTemp
              WrapperGLHERate = 0.d0
              WrapperGLHEEnergy = 0.d0
            END IF
          END DO

            ! Calculate hot water outlet temperature
          IF (HWInletMassFlowRate > 0.d0) THEN
            HWBypassMassFlowRate = HWInletMassFlowRate - HWOutletMassFlowRate
              IF (HWBypassMassFlowRate > 0.d0) THEN
                HWOutletTemp = HWOutletTemp + HWInletTemp *  HWBypassMassFlowRate/HWInletMassFlowRate
              ELSE
                HWOutletTemp = HWOutletTemp
                  IF (HWOutletTemp > HWInletTemp) HWOutletTemp = HWInletTemp
              END IF
          ELSE
            HWOutletTemp = HWInletTemp
          END IF

          ! Calculate condenser outlet temperature
          IF(GLHEInletMassFlowRate > 0.d0) THEN
            GLHEBypassMassFlowRate = GLHEInletMassFlowRate - GLHEOutletMassFlowRate
              IF (GLHEBypassMassFlowRate > 0.d0) THEN
                GLHEOutletTemp = GLHEOutletTemp + GLHEInletTemp * GLHEBypassMassFlowRate/GLHEInletMassFlowRate
              ELSE
                GLHEOutletTemp = GLHEOutletTemp
              END IF
          ELSE
            GLHEOutletTemp = GLHEInletTemp
          END IF

          CHWOutletTemp = CHWInletTemp

            ! Add ancilliary power if necessary
          IF (GetCurrentScheduleValue(Wrapper(WrapperNum)%SchedPtr) > 0) THEN
            WrapperElecPowerHeat = WrapperElecPowerHeat + (Wrapper(WrapperNum)%AncilliaryPower *  &
                                                          GetCurrentScheduleValue(Wrapper(WrapperNum)%SchedPtr))
          END IF

        END IF ! End of calculations

        CALL SetComponentFlowRate(CHWInletMassFlowRate, CHWInletNodeNum, CHWOutletNodeNum, &
             Wrapper(WrapperNum)%CWLoopNum,       &
             Wrapper(WrapperNum)%CWLoopSideNum,   &
             Wrapper(WrapperNum)%CWBranchNum,     &
             Wrapper(WrapperNum)%CWCompNum)

        CALL SetComponentFlowRate(HWInletMassFlowRate, HWInletNodeNum, HWOutletNodeNum,  &
             Wrapper(WrapperNum)%HWLoopNum,       &
             Wrapper(WrapperNum)%HWLoopSideNum,   &
             Wrapper(WrapperNum)%HWBranchNum,     &
             Wrapper(WrapperNum)%HWCompNum)

        CALL SetComponentFlowRate(GLHEInletMassFlowRate, GLHEInletNodeNum, GLHEOutletNodeNum, &
             Wrapper(WrapperNum)%GLHELoopNum,     &
             Wrapper(WrapperNum)%GLHELoopSideNum, &
             Wrapper(WrapperNum)%GLHEBranchNum,   &
             Wrapper(WrapperNum)%GLHECompNum)

          ! Local variables
        WrapperReport(WrapperNum)%CHWInletTemp = CHWInletTemp
        WrapperReport(WrapperNum)%CHWOutletTemp = CHWOutletTemp
        WrapperReport(WrapperNum)%HWInletTemp = HWInletTemp
        WrapperReport(WrapperNum)%HWOutletTemp = HWOutletTemp
        WrapperReport(WrapperNum)%GLHEInletTemp = GLHEInletTemp
        WrapperReport(WrapperNum)%GLHEOutletTemp = GLHEOutletTemp
        WrapperReport(WrapperNum)%CHWmdot = CHWInletMassFlowRate
        WrapperReport(WrapperNum)%HWmdot = HWInletMassFlowRate
        WrapperReport(WrapperNum)%GLHEmdot = GLHEInletMassFlowRate
        WrapperReport(WrapperNum)%TotElecCooling = WrapperElecEnergyCool
        WrapperReport(WrapperNum)%TotElecHeating = WrapperElecEnergyHeat
        WrapperReport(WrapperNum)%CoolingEnergy = WrapperCoolEnergy
        WrapperReport(WrapperNum)%HeatingEnergy = WrapperHeatEnergy
        WrapperReport(WrapperNum)%GLHEEnergy = WrapperGLHEEnergy
        WrapperReport(WrapperNum)%TotElecCoolingPwr = WrapperElecPowerCool
        WrapperReport(WrapperNum)%TotElecHeatingPwr = WrapperElecPowerHeat
        WrapperReport(WrapperNum)%CoolingRate = WrapperCoolRate
        WrapperReport(WrapperNum)%HeatingRate = WrapperHeatRate
        WrapperReport(WrapperNum)%GLHERate = WrapperGLHERate

        Node(CHWOutletNodeNum)%Temp  = CHWOutletTemp
        Node(HWOutletNodeNum)%Temp   = HWOutletTemp
        Node(GLHEOutletNodeNum)%Temp = GLHEOutletTemp

      ELSE  ! Central chiller heater system is off

        CHWOutletTemp = CHWInletTemp
        HWOutletTemp = HWInletTemp
        GLHEOutletTemp = GLHEInletTemp
        Node(CHWOutletNodeNum)%Temp  = CHWOutletTemp
        Node(HWOutletNodeNum)%Temp   = HWOutletTemp
        Node(GLHEOutletNodeNum)%Temp = GLHEOutletTemp

        IF (Wrapper(WrapperNum)%WrapperCoolingLoad == 0.d0 .AND. .NOT. SimulHtgDominant) THEN

          DO ChillerHeaterNum=1, ChillerHeaterNums
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%MassFlowRate = 0.d0
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondOutletNode%MassFlowRate = 0.d0
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapOutletNode%Temp = CHWInletTemp
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%EvapInletNode%Temp = CHWInletTemp
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondOutletNode%Temp = GLHEInletTemp
            Wrapper(WrapperNum)%Chillerheater(ChillerHeaterNum)%CondInletNode%Temp = GLHEInletTemp
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CurrentMode = 0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerPartLoadRatio = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCyclingRatio  = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadRate = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerCapFT = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFT = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerEIRFPLR = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPower = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapOutletTemp = CHWOutletTemp
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapInletTemp = CHWInletTemp
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondOutletTemp = GLHEOutletTemp
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondInletTemp = GLHEInletTemp
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Evapmdot = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%Condmdot = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoad = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingEnergy = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingEnergy = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergy = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergy = 0.d0
            Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ActualCOP = 0.d0
          END DO

          WrapperReport(WrapperNum)%CHWInletTemp = CHWInletTemp
          WrapperReport(WrapperNum)%CHWOutletTemp = CHWOutletTemp
          WrapperReport(WrapperNum)%HWInletTemp = HWInletTemp
          WrapperReport(WrapperNum)%HWOutletTemp = HWOutletTemp
          WrapperReport(WrapperNum)%GLHEInletTemp = GLHEInletTemp
          WrapperReport(WrapperNum)%GLHEOutletTemp = GLHEOutletTemp
          WrapperReport(WrapperNum)%CHWmdot = CHWInletMassFlowRate
          WrapperReport(WrapperNum)%HWmdot = HWInletMassFlowRate
          WrapperReport(WrapperNum)%GLHEmdot = GLHEInletMassFlowRate
          WrapperReport(WrapperNum)%TotElecCooling = WrapperElecEnergyCool
          WrapperReport(WrapperNum)%TotElecHeating = WrapperElecEnergyHeat
          WrapperReport(WrapperNum)%CoolingEnergy = WrapperCoolEnergy
          WrapperReport(WrapperNum)%HeatingEnergy = WrapperHeatEnergy
          WrapperReport(WrapperNum)%GLHEEnergy = WrapperGLHEEnergy
          WrapperReport(WrapperNum)%TotElecCoolingPwr = WrapperElecPowerCool
          WrapperReport(WrapperNum)%TotElecHeatingPwr = WrapperElecPowerHeat
          WrapperReport(WrapperNum)%CoolingRate = WrapperCoolRate
          WrapperReport(WrapperNum)%HeatingRate = WrapperHeatRate
          WrapperReport(WrapperNum)%GLHERate = WrapperGLHERate

          CALL SetComponentFlowRate(CHWInletMassFlowRate, CHWInletNodeNum, CHWOutletNodeNum, &
               Wrapper(WrapperNum)%CWLoopNum,       &
               Wrapper(WrapperNum)%CWLoopSideNum,   &
               Wrapper(WrapperNum)%CWBranchNum,     &
               Wrapper(WrapperNum)%CWCompNum)

          CALL SetComponentFlowRate(HWInletMassFlowRate, HWInletNodeNum, HWOutletNodeNum,  &
               Wrapper(WrapperNum)%HWLoopNum,     &
               Wrapper(WrapperNum)%HWLoopSideNum, &
               Wrapper(WrapperNum)%HWBranchNum,   &
               Wrapper(WrapperNum)%HWCompNum)

          CALL SetComponentFlowRate(GLHEInletMassFlowRate, GLHEInletNodeNum, GLHEOutletNodeNum, &
               Wrapper(WrapperNum)%GLHELoopNum,        &
               Wrapper(WrapperNum)%GLHELoopSideNum,    &
               Wrapper(WrapperNum)%GLHEBranchNum,      &
               Wrapper(WrapperNum)%GLHECompNum)
        END IF

      END IF ! Heating loop calculation

      !!Node(CHWOutletNodeNum)%Temp  = CHWOutletTemp
      !!Node(HWOutletNodeNum)%Temp   = HWOutletTemp
      !!Node(GLHEOutletNodeNum)%Temp = GLHEOutletTemp

    END IF

  END IF

  RETURN

END SUBROUTINE CalcWrapperModel

SUBROUTINE UpdateChillerRecords(WrapperNum)

            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Daeho Kang, PNNL
            !       DATE WRITTEN:    Feb 2013

            ! PURPOSE OF THIS SUBROUTINE:
            !  Update cihller heater variables

            ! METHODOLOGY EMPLOYED:
            !  na

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:
  USE DataGlobals,     ONLY : SecInHour,HVACTSReporting,OutputFileDebug
  USE DataHVACGlobals, ONLY : TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(IN)      :: WrapperNum        ! Wrapper number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: SecInTimeStep      ! Number of seconds per HVAC system time step, to convert from W (J/s) to J
  INTEGER   :: ChillerHeaterNum   ! Chiller heater number

  SecInTimeStep = TimeStepSys*SecInHour

    Do ChillerHeaterNum=1, Wrapper(WrapperNum)%ChillerHeaterNums
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoad = &
                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadRate * SecInTimeStep
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingEnergy = &
                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPower * SecInTimeStep
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingEnergy = &
                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower * SecInTimeStep
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergy = &
                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap * SecInTimeStep
      Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergy = &
                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond  * SecInTimeStep
      IF (SimulClgDominant .OR. SimulHtgDominant) THEN
         Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadSimul =  &
                                        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoad
         Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingEnergySimul =  &
                                        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingEnergy
         Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergySimul =  &
                                        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergy
         Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergySimul =  &
                                        Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergy
      END IF
    END DO

  RETURN

END SUBROUTINE UpdateChillerRecords


SUBROUTINE UpdateChillerheaterRecords (WrapperNum)

            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Daeho Kang, PNNL
            !       DATE WRITTEN:    Feb 2013

            ! PURPOSE OF THIS SUBROUTINE:
            !  Reporting

            ! METHODOLOGY EMPLOYED:
            !  na

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:
  USE DataGlobals,     ONLY : SecInHour,HVACTSReporting,OutputFileDebug
  USE DataHVACGlobals, ONLY : TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(IN)      :: WrapperNum        ! Wrapper number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: SecInTimeStep     ! Number of seconds per HVAC system time step, to convert from W (J/s) to J
  INTEGER   :: ChillerHeaterNum  ! Chiller heater number

  SecInTimeStep = TimeStepSys*SecInHour

    Do ChillerHeaterNum=1, Wrapper(WrapperNum)%ChillerHeaterNums
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoad = &
                  Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%ChillerFalseLoadRate * SecInTimeStep
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingEnergy = &
                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CoolingPower * SecInTimeStep
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingEnergy = &
                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%HeatingPower * SecInTimeStep
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%EvapEnergy = &
                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QEvap * SecInTimeStep
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondEnergy = &
                    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%QCond  * SecInTimeStep
    Wrapper(WrapperNum)%ChillerheaterReport(ChillerHeaterNum)%CondenserFanEnergyConsumption = &
                                                                                 CondenserFanPower*SecInTimeStep
    END DO

RETURN

END SUBROUTINE UpdateChillerheaterRecords

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

END MODULE PlantCentralGSHP