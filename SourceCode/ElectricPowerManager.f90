MODULE ManageElectricPower

          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       B. Griffith  Feb. 2008 add thermal following operating scheme
          !                      B. Griffith May 2009 add EMS
          !       RE-ENGINEERED  B. Griffith  June 2008 add support for DC centers and different inverter models
          !       MODIFIED       B. Brannon July 2009 Charge and Draw objects removed from line 1825
          !                      B. Brannon Nullified Charge during Drawing 2298. Nullified Draw during Charging 2305.
          !                                 to prevent simultaneous Drawing and Charging
          !                      B. Nigusse  Feb. 2010 Modifed the load center requested power calculation procedure
          !                                            used to manage the electric load center electrical storage units.
          !                      W. Wang     June-July, 2010 Add a transformer model
          !                      Y. KyungTae and W. Wang July-August, 2011 Added a battery model

          ! PURPOSE OF THIS MODULE:
          ! This module manages electric power generation and distribution
          ! equipment.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! OTHER NOTES: na


          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,         ONLY : MaxNameLength, NumOfTimeStepInHour, TimeStepZone, SecInHour, ScheduleAlwaysOn
USE DataInterfaces,      ONLY : ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueError, &
                                ShowRecurringSevereErrorAtEnd
USE DataHVACGlobals,     ONLY : TimeStepSys
USE DataGlobalConstants, ONLY: iGeneratorICEngine,iGeneratorCombTurbine,iGeneratorPV,  &
                               iGeneratorFuelCell, iGeneratorMicroCHP, &
                               iGeneratorMicroturbine, iGeneratorWindTurbine


IMPLICIT NONE
PRIVATE

          ! MODULE PARAMETER DEFINITIONS:

 INTEGER, PARAMETER :: iOpSchemeBaseLoad        =1 ! Electric load center dispatch mode
 INTEGER, PARAMETER :: iOpSchemeDemandLimit     =2 ! Electric load center dispatch mode
 INTEGER, PARAMETER :: iOpSchemeTrackElectrical =3 ! Electric load center dispatch mode
 INTEGER, PARAMETER :: iOpSchemeTrackSchedule   =4 ! Electric load center dispatch mode
 INTEGER, PARAMETER :: iOpSchemeTrackMeter      =5 ! Electric load center dispatch mode
 INTEGER, PARAMETER :: iOpSchemeThermalFollow   =6 ! Electric load center dispatch mode
 INTEGER, PARAMETER :: iOpSchemeThermalFollowLimitElectrical = 7 ! Electric load center dispatch mode

 INTEGER, PARAMETER :: ACBuss                = 100 !Electic load center buss and power conditioning mode
 INTEGER, PARAMETER :: ACBussStorage         = 101 !Electic load center buss and power conditioning mode
 INTEGER, PARAMETER :: DCBussInverter        = 102 !Electic load center buss and power conditioning mode
 INTEGER, PARAMETER :: DCBussInverterDCStorage = 103 !Electic load center buss and power conditioning mode
 INTEGER, PARAMETER :: DCBussInverterACStorage = 104 !Electic load center buss and power conditioning mode

 INTEGER, PARAMETER :: CECLookUpTableModel   = 201 ! inverter model mode
 INTEGER, PARAMETER :: CurveFuncOfPower      = 202 ! inverter model mode
 INTEGER, PARAMETER :: SimpleConstantEff     = 203 ! inverter model mode

 INTEGER, PARAMETER :: ZoneGains             = 301 ! power conditioning equipment thermal loss destination
 INTEGER, PARAMETER :: LostToOutside         = 302 ! power conditioning equipment thermal loss destination

 INTEGER, PARAMETER :: SimpleBucketStorage   = 401 ! storage model mode (1 of 2)
 INTEGER, PARAMETER :: KiBaMBattery          = 402 ! storage model mode (2 of 2)

 INTEGER, PARAMETER :: PowerInFromGrid       = 501 ! Transformer usage: power in from grid
 INTEGER, PARAMETER :: PowerOutFromBldg      = 502 ! Transformer usage: power out from onsite generation
 INTEGER, PARAMETER :: LossesMethod          = 521 ! Transformer performance input methos: RatedLosses
 INTEGER, PARAMETER :: EfficiencyMethod      = 522 ! Transformer performance input methos: NominalEfficiency

 INTEGER, PARAMETER :: Battery_LifeCalculation_Yes = 1
 INTEGER, PARAMETER :: Battery_LifeCalculation_No  = 2

          ! DERIVED TYPE DEFINITIONS:
TYPE GenData
  CHARACTER(len=MaxNameLength) :: Name               = ' ' ! user identifier
  CHARACTER(len=MaxNameLength) :: TypeOf             = ' ' ! equipment type
  INTEGER                      :: CompType_Num       = 0  ! Numeric designator for CompType (TypeOf)
  INTEGER                      :: GeneratorIndex     = 0
  REAL(r64)                    :: MaxPowerOut        = 0.0d0 ! Maximum Power Output (W)
  CHARACTER(len=MaxNameLength) :: AvailSched         = ' ' ! Operation Schedule.
  INTEGER                      :: AvailSchedPtr      = 0   ! pointer to operation schedule
  REAL(r64)                    :: PowerRequestThisTimestep   = 0.0d0 ! Current Demand on Equipment (W)
  LOGICAL                      :: ONThisTimestep     =.false. !Indicator whether Generator on
  REAL(r64)                    :: EMSPowerRequest    = 0.0D0 ! EMS actuator for current demand on equipment (W)
  LOGICAL                      :: EMSRequestOn       = .FALSE. ! EMS actuating On if true.

  LOGICAL                      :: PlantInfoFound     = .false.
  INTEGER                      :: PlantLoopNum       = 0 ! Cogen: pointer to plant loop data structure
  INTEGER                      :: LoopSideNum        = 0 ! Cogen: pointer to plant loop data structure
  INTEGER                      :: BranchNum          = 0 ! Cogen: pointer to plant loop data structure
  INTEGER                      :: CompNum            = 0 ! Cogen: pointer to plant loop data structure
  REAL(r64)                    :: NominalThermElectRatio  = 0.0d0 ! Cogen: nominal ratio of thermal to elect production
  !results of component models for load center reporting
  REAL(r64)                    :: DCElectricityProd    = 0.0d0 ! Current DC Electric Produced from Equipment (J)
  REAL(r64)                    :: DCElectProdRate      = 0.0d0 ! Current DC Electric Production Rate from Equipment (W)
  REAL(r64)                    :: ElectricityProd      = 0.0d0 ! Current AC Electric Produced from Equipment (J)
  REAL(r64)                    :: ElectProdRate        = 0.0d0 ! Current AC Electric Production Rate from Equipment (W)
  REAL(r64)                    :: ThermalProd          = 0.0d0 ! Current Thermal energy Produced from Equipment (J)
  REAL(r64)                    :: ThermalProdRate      = 0.0d0 ! Current Thermal energy Production Rate from Equipment (W)

END TYPE GenData

TYPE ElectricPowerLoadCenter
  CHARACTER(len=MaxNameLength) :: Name               = ' ' ! user identifier

  CHARACTER(len=MaxNameLength) :: GeneratorList      = ' ' ! List name of available generators
  INTEGER                      :: OperationScheme    = 0   ! Name of Operation Scheme
  CHARACTER(len=MaxNameLength) :: DemandMeterName    = ' ' ! Name of Demand Energy Meter for "on demand" operation
  INTEGER                      :: DemandMeterPtr     = 0   ! "pointer" to Meter for electrical Demand to meet
  CHARACTER(len=MaxNameLength) :: GenerationMeterName= ' ' ! Name of Generated Energy Meter for "on demand" operation
  INTEGER                      :: NumGenerators      = 0   ! Number of Generators
  TYPE(GenData), ALLOCATABLE, DIMENSION(:) :: ElecGen          ! pointer to generator
  REAL(r64)                    :: DemandLimit        = 0.0d0 ! Demand Limit in Watts(W) which the generator will operate above
  INTEGER                      :: TrackSchedPtr      = 0   ! "pointer" to schedule for electrical demand to meet.

  INTEGER                      :: BussType           = 0   ! is this load center powered by AC or DC generators
  LOGICAL                      :: InverterPresent    = .FALSE.
  CHARACTER(len=MaxNameLength) :: InverterName       = ' ' ! hold name for verificaton and error messages
  INTEGER                      :: InverterModelNum   = 0   ! simulation model parameter type
  REAL(r64)                    :: DCElectricityProd  = 0.0D0 ! Current DC Elect produced (J) (if buss type DCbussInverter)
  REAL(r64)                    :: DCElectProdRate    = 0.0D0 ! Current DC Elect power produced (W) (if buss type DCbussInverter)
  REAL(r64)                    :: DCpowerConditionLosses = 0.0D0 ! current DC to AC inverter losses (W) (if DCbussInverter)

  LOGICAL                      :: StoragePresent     = .FALSE.
  CHARACTER(len=MaxNameLength) :: StorageName        = ' ' ! hold name for verificaton and error messages
  INTEGER                      :: StorageModelNum    = 0   ! simulation model parameter type

  LOGICAL                      :: TransformerPresent = .FALSE.
  CHARACTER(len=MaxNameLength) :: TransformerName    = ' ' ! hold name for verificaton and error messages
  INTEGER                      :: TransformerModelNum= 0   ! simulation model parameter type

  REAL(r64)                    :: ElectricityProd    = 0.0d0 ! Current AC Electric Produced from Equipment (J)
  REAL(r64)                    :: ElectProdRate      = 0.0d0 ! Current Electric Production Rate from Equipment (W)
  REAL(r64)                    :: ThermalProd        = 0.0d0 ! Current Thermal energy Produced from Equipment (J)
  REAL(r64)                    :: ThermalProdRate    = 0.0d0 ! Current Thermal energy Production Rate from Equipment (W)
  REAL(r64)                    :: TotalPowerRequest  = 0.0d0 ! Total electric power request from the load center (W)
  REAL(r64)                    :: TotalThermalPowerRequest  = 0.0d0 ! Total thermal power request from the load center (W)
  REAL(r64)                    :: ElectDemand        = 0.0d0 ! Current electric power demand on the load center (W)
END TYPE ElectricPowerLoadCenter

TYPE CECInverterLookUpTableData
  REAL(r64) :: NightTareLossPower = 0.0D0
  REAL(r64) :: NominalVoltage = 0.0D0
  REAL(r64), DIMENSION(6) :: NomVoltEfficiencyARR = 0.0D0  ! eff at 10, 20, 30, 50, 75, & 100% power and Nominal voltage
END TYPE CECInverterLookUpTableData

TYPE DCtoACInverterStruct
  CHARACTER(len=MaxNameLength) :: Name       = ' ' ! user identifier
  INTEGER                      :: ModelType  = 0   ! type of inverter model used
  INTEGER                      :: AvailSchedPtr = 0 ! number for availability schedule.
  INTEGER                      :: HeatLossesDestination = 0 !
  INTEGER                      :: ZoneNum = 0 ! destination zone for heat losses from inverter.
  REAL(r64)                    :: ZoneRadFract = 0.0D0 ! radiative fraction for thermal losses to zone
  Type (CECInverterLookUpTableData) :: LUTable
  INTEGER                      :: CurveNum = 0 ! curve index for eff as func of power
  REAL(r64)                    :: RatedPower = 0.0D0 ! rated, max continuous power output level for inverter
  REAL(r64)                    :: MinPower   = 0.0D0
  REAL(r64)                    :: MaxPower   = 0.0D0
  REAL(r64)                    :: MinEfficiency    = 0.0D0
  REAL(r64)                    :: MaxEfficiency    = 0.0D0
  REAL(r64)                    :: StandbyPower     = 0.0D0
  !results and reporting
  REAL(r64)                    :: Efficiency       = 0.0D0
  REAL(r64)                    :: DCPowerIn        = 0.0D0
  REAL(r64)                    :: ACPowerOut       = 0.0D0
  REAL(r64)                    :: DCEnergyIn       = 0.0D0
  REAL(r64)                    :: ACEnergyOut      = 0.0D0
  REAL(r64)                    :: ThermLossRate    = 0.0D0
  REAL(r64)                    :: ThermLossEnergy  = 0.0D0
  REAL(r64)                    :: QdotconvZone     = 0.0D0
  REAL(r64)                    :: QdotRadZone      = 0.0D0
  REAL(r64)                    :: AncillACuseRate  = 0.0D0
  REAL(r64)                    :: AncillACuseEnergy = 0.0D0
END TYPE DCtoACInverterStruct

TYPE, PUBLIC :: ElecStorageDataStruct
  !user defined variables
  CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this electrical storage module
  INTEGER                      :: StorageModelMode          = 0 ! type of model parameter, SimpleBucketStorage
  INTEGER                      :: AvailSchedPtr             = 0 ! availability schedule index.
  INTEGER                      :: HeatLossesDestination     = 0 ! mode for where thermal losses go
  INTEGER                      :: ZoneNum                   = 0 ! destination zone for heat losses from inverter.
  REAL(r64)                    :: ZoneRadFract              = 0.0D0 ! radiative fraction for thermal losses to zone
  REAL(r64)                    :: StartingEnergyStored      = 0.0D0 ![J] joules inside at beginning of environment period
  REAL(r64)                    :: EnergeticEfficCharge      = 0.0D0 ! [ ] efficiency of charging
  REAL(r64)                    :: EnergeticEfficDischarge   = 0.0D0 ! [ ] efficiency of discharging
  REAL(r64)                    :: MaxPowerDraw              = 0.0D0 ! [W] max rate of discharge
  REAL(r64)                    :: MaxPowerStore             = 0.0D0 ! [W] max rate of charge
  REAL(r64)                    :: MaxEnergyCapacity         = 0.0D0 ! [J] max storage capacity
  INTEGER                      :: ParallelNum               = 0     ! [ ] number of battery modules in parallel
  INTEGER                      :: SeriesNum                 = 0     ! [ ] number of battery modules in series
  INTEGER                      :: ChargeCurveNum            = 0     ! [ ] voltage change curve index number for charging
  INTEGER                      :: DischargeCurveNum         = 0     ! [ ] voltage change curve index number for discharging
  INTEGER                      :: CycleBinNum               = 0     ! [ ] number of cycle bins
  REAL(r64)                    :: StartingSOC               = 0.0D0 ! [ ] initial fractional state of charge
  REAL(r64)                    :: MaxAhCapacity             = 0.0D0 ! [Ah]maximum capacity
  REAL(r64)                    :: AvailableFrac             = 0.0D0 ! [ ] fraction of available charge capacity
  REAL(r64)                    :: ChargeConversionRate      = 0.0D0 ! [1/h]change rate from bound charge energy to available charge
  REAL(r64)                    :: ChargedOCV                = 0.0D0 ! [V] fully charged oppen circuit voltage
  REAL(r64)                    :: DischargedOCV             = 0.0D0 ! [V] fully discharged open circuit voltage
  REAL(r64)                    :: InternalR                 = 0.0D0 ! [ohm]internal electric resistance
  REAL(r64)                    :: MaxDischargeI             = 0.0D0 ! [A] maximum discharging current
  REAL(r64)                    :: CutoffV                   = 0.0D0 ! [V] cut-off voltage
  REAL(r64)                    :: MaxChargeRate             = 0.0D0 ! [1/h]charge rate limit
  INTEGER                      :: LifeCalculation           = 0     ! [ ]battery life calculation: Yes or No
  INTEGER                      :: LifeCurveNum              = 0     ! [ ]battery life curve name index number
  !calculated and from elsewhere vars
  REAL(r64)                    :: ThisTimeStepStateOfCharge = 0.0D0 ! [J]
  REAL(r64)                    :: LastTimeStepStateOfCharge = 0.0D0 ! [J]
  REAL(r64)                    :: PelNeedFromStorage        = 0.0D0 ! [W]
  REAL(r64)                    :: PelFromStorage            = 0.0D0 ! [W]
  LOGICAL                      :: EMSOverridePelFromStorage = .FALSE.  ! if true, EMS calling for override
  REAL(r64)                    :: EMSValuePelFromStorage    = 0.0D0 ! value EMS is directing to use, power from storage [W]
  REAL(r64)                    :: PelIntoStorage            = 0.0D0 ! [W]
  LOGICAL                      :: EMSOverridePelIntoStorage = .FALSE. ! if true, EMS calling for override
  REAL(r64)                    :: EMSValuePelIntoStorage    = 0.0D0  ! value EMS is directing to use, power into storage [W]
  REAL(r64)                    :: QdotConvZone              = 0.0D0 ! [W]
  REAL(r64)                    :: QdotRadZone               = 0.0D0 ! [W]
  REAL(r64)                    :: TimeElapsed               = 0.0D0 ! [h]
  REAL(r64)                    :: ThisTimeStepAvailable     = 0.0D0 ! [Ah] available charge at the current timestep
  REAL(r64)                    :: ThisTimeStepBound         = 0.0D0 ! [Ah] bound charge at the current timestep
  REAL(r64)                    :: LastTimeStepAvailable     = 0.0D0 ! [Ah] available charge at the previous timestep
  REAL(r64)                    :: LastTimeStepBound         = 0.0D0 ! [Ah] bound charge at the previous timestep
  REAL(r64)                    :: LastTwoTimeStepAvailable  = 0.0D0 ! [Ah] available charge at the previous two timesteps
  REAL(r64)                    :: LastTwoTimeStepBound      = 0.0D0 ! [Ah] bound charge at the previous two timesteps
  !battery life calculation variables
  INTEGER                      :: count0                    = 0
  REAL(r64), DIMENSION(:), ALLOCATABLE :: B10
  REAL(r64), DIMENSION(:), ALLOCATABLE :: X0
  REAL(r64), DIMENSION(:), ALLOCATABLE :: Nmb0
  REAL(r64), DIMENSION(:), ALLOCATABLE :: OneNmb0
  !report
  REAL(r64)                    :: ElectEnergyinStorage      = 0.0D0 ! [J] state of charge
  REAL(r64)                    :: StoredPower               = 0.0D0 ! [W]
  REAL(r64)                    :: StoredEnergy              = 0.0D0 ! [J]
  REAL(r64)                    :: DecrementedEnergyStored   = 0.0D0 ! [J] this is the negative of StoredEnergy
  REAL(r64)                    :: DrawnPower                = 0.0D0 ! [W]
  REAL(r64)                    :: DrawnEnergy               = 0.0D0 ! [J]
  REAL(r64)                    :: ThermLossRate             = 0.0D0 ! [W]
  REAL(r64)                    :: ThermLossEnergy           = 0.0D0 ! [J]
  INTEGER                      :: StorageMode               = 0     ! [ ] mode of operation 0 for idle, 1 for discharging, 2 for charging
  REAL(r64)                    :: AbsoluteSOC               = 0.0D0 ! [Ah] total state of charge
  REAL(r64)                    :: FractionSOC               = 0.0D0 ! [ ] fractional state of charge
  REAL(r64)                    :: BatteryCurrent            = 0.0D0 ! [A] total current
  REAL(r64)                    :: BatteryVoltage            = 0.0D0 ! [V] total voltage
  REAL(r64)                    :: BatteryDamage             = 0.0D0 ! [ ] fractional battery damage


END TYPE ElecStorageDataStruct

TYPE ElectricTransformer
  ! user defined variables
  CHARACTER(len=MaxNameLength) :: Name               = ' '  ! user identifier
  INTEGER                      :: AvailSchedPtr = 0         ! availability schedule index.
  INTEGER                      :: UsageMode = 0             ! mode for transformer usage
  INTEGER                      :: HeatLossesDestination = 0 ! mode for where thermal losses go
  INTEGER                      :: ZoneNum = 0               ! destination zone for heat losses from inverter.
  REAL(r64)                    :: ZoneRadFrac = 0.0D0       ! radiative fraction for thermal losses to zone
  REAL(r64)                    :: RatedCapacity = 0.0D0     ! rated capacity [VA]
  INTEGER                      :: Phase = 0                 ! phase
  REAL(r64)                    :: FactorTempCoeff = 0.0D0   ! thermal coefficient of resistance for winding material
  REAL(r64)                    :: TempRise = 0.0D0          ! full load temperature rise [C]
  REAL(r64)                    :: EddyFrac = 0.0D0          ! fraction of eddy current losses []
  INTEGER                      :: PerformanceInputMode = 0  ! performance input method
  REAL(r64)                    :: RatedEfficiency = 0.0D0   ! nameplate efficiency []
  REAL(r64)                    :: RatedPUL = 0.0D0          ! per unit load for nameplate efficiency []
  REAL(r64)                    :: RatedTemp = 0.0D0         ! reference temperature for nameplate efficiency [C]
  REAL(r64)                    :: MaxPUL = 0.0D0            ! per unit load for maximum efficiency []
  LOGICAL                      :: ConsiderLosses = .TRUE.   ! if true, consider transformer lossses in metering
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: WiredMeterNames ! names of the meters wired to transformer
  INTEGER, ALLOCATABLE, DIMENSION(:) :: WiredMeterPtrs      ! array of "pointers" to meters wired to transformer
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: SpecialMeter        ! indicates whether a meter needs special consideration
                                                            ! Electricity:Facility and Electricity:HVAC are two special
                                                            ! meters because tranformer loss is part of them
  !calculated and from elsewhere vars
  REAL(r64)                    :: RatedNL = 0.0D0           ! rated no load losses, user input or calculated [W]
  REAL(r64)                    :: RatedLL = 0.0D0           ! rated load losses, user input or calculated [W]
  INTEGER                      :: LoadCenterNum = 0         ! number of load centers served by the transformer
  Integer, ALLOCATABLE, DIMENSION(:) :: LoadCenterIndexes   ! index array of load centers served by the transformer
  INTEGER                      :: OverloadErrorIndex = 0    ! used for warning message when transformer is overloaded
  !results and reporting
  REAL(r64)                    :: Efficiency       = 0.0D0  ! transformer efficiency
  REAL(r64)                    :: PowerIn          = 0.0D0  ! [W]
  REAL(r64)                    :: EnergyIn         = 0.0D0  ! [J]
  REAL(r64)                    :: PowerOut         = 0.0D0  ! [W]
  REAL(r64)                    :: EnergyOut        = 0.0D0  ! [J]
  REAL(r64)                    :: NoLoadLossRate   = 0.0D0  ! [W]
  REAL(r64)                    :: NoLoadLossEnergy = 0.0D0  ! [J]
  REAL(r64)                    :: LoadLossRate     = 0.0D0  ! [W]
  REAL(r64)                    :: LoadLossEnergy   = 0.0D0  ! [J]
  REAL(r64)                    :: ThermalLossRate  = 0.0D0  ! [W]
  REAL(r64)                    :: ThermalLossEnergy= 0.0D0  ! [J]
  REAL(r64)                    :: ElecUseUtility   = 0.0D0  ! [J] Energy consumption for a utility transformer (power in)
                                                            ! Positive values
  REAL(r64)                    :: ElecProducedCoGen= 0.0D0  ! [J] Energy consumption for a cogeneration transformer (power out)
                                                            ! Negative values
  REAL(r64)                    :: QdotconvZone     = 0.0D0  ! [W]
  REAL(r64)                    :: QdotRadZone      = 0.0D0  ! [W]
END TYPE ElectricTransformer


Type WholeBuildingElectricPowerSummary
  CHARACTER(len=MaxNameLength) :: Name = 'Whole Building'
  REAL(r64)                    :: ElectricityProd    = 0.0D0 ! Current Electric Produced from Equipment (J)
  REAL(r64)                    :: ElectProdRate      = 0.0D0 ! Current Electric Production Rate from Equipment (W)
  REAL(r64)                    :: ElectricityPurch   = 0.0D0 ! Current Purchased Electric (J)
  REAL(r64)                    :: ElectPurchRate      = 0.0D0 ! Current Electric Purhcased Rate (W)
  REAL(r64)                    :: ElectSurplusRate    = 0.0D0 ! Current excess power (W)
  REAL(r64)                    :: ElectricitySurplus  = 0.0D0 ! Current excess energy (J)
  REAL(r64)                    :: ElectricityNetRate  = 0.0D0 ! Net elect rate, + is Purchased, - is Surplus (W)
  REAL(r64)                    :: ElectricityNet      = 0.0D0 ! Net energy, + is Purchased, - is Surplus (J)
  REAL(r64)                    :: TotalBldgElecDemand = 0.0D0 ! Current Total Building Electric Demand (W)
  REAL(r64)                    :: TotalHVACElecDemand = 0.0D0 ! Current Total HVAC Electric Demand (W)
  REAL(r64)                    :: TotalElectricDemand = 0.0D0 ! Current Total Electric Demand (W)
  REAL(r64)                    :: ElecProducedPVRate  = 0.0D0 ! Current Rate of PV Produced from the Arrays (W)
  REAL(r64)                    :: ElecProducedWTRate  = 0.0D0 ! Current Rate of Wind Turbine Produced (W)
end type WholeBuildingElectricPowerSummary


          ! MODULE VARIABLE DECLARATIONS:
LOGICAL,SAVE    :: GetInput = .TRUE.   ! When TRUE, calls subroutine to read input file.
INTEGER :: NumLoadCenters         =0
INTEGER :: NumInverters           =0
INTEGER, PUBLIC :: NumElecStorageDevices  =0
INTEGER :: NumTransformers        =0


INTEGER :: ElecProducedCoGenIndex =0
INTEGER :: ElecProducedPVIndex    =0
INTEGER :: ElecProducedWTIndex    =0

INTEGER :: MAXRainflowArrayBounds =100
INTEGER :: MAXRainFlowArrayInc    =100
TYPE (ElecStorageDataStruct), ALLOCATABLE, PUBLIC, DIMENSION(:)    :: ElecStorage
TYPE (DCtoACInverterStruct), ALLOCATABLE, DIMENSION(:)     :: Inverter
TYPE (ElectricPowerLoadCenter), ALLOCATABLE, DIMENSION(:)  :: ElecLoadCenter  !dimension to number of machines
TYPE (ElectricTransformer), ALLOCATABLE, DIMENSION(:)      :: Transformer
TYPE (WholeBuildingElectricPowerSummary),SAVE :: WholeBldgElectSummary=  &
           WholeBuildingElectricPowerSummary('Whole Building',0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,  &
                          0.0d0,0.0d0,0.0d0,0.0d0,0.0d0)

          ! SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
PUBLIC    ManageElectricLoadCenters
PRIVATE   GetPowerManagerInput
PRIVATE   UpdateWholeBuildingRecords
PRIVATE   UpdateLoadCenterRecords
PUBLIC    VerifyCustomMetersElecPowerMgr
PRIVATE   CalcLoadCenterThermalLoad
PUBLIC    FigureInverterZoneGains
PUBLIC    FigureElectricalStorageZoneGains
PRIVATE   ManageElectCenterStorageInteractions
PRIVATE   ManageInverter
PRIVATE   GeneratorPowerOutput
PRIVATE   ManageTransformers
PUBLIC    FigureTransformerZoneGains

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Electric Power Manager Subroutines
!*************************************************************************

SUBROUTINE ManageElectricLoadCenters(FirstHVACIteration,SimElecCircuits, UpdateMetersOnly)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 2000
          !       RE-ENGINEERED  Richard Liesen, Feb 2003
          !       RE-ENGINEERED  Bereket Nigusse, Jan/Feb 2010   Generator dispatch is based on actual
          !                                                      power produced by the previous generator(s)
          !       MODIFIED       Weimin Wang, July 2010          Consider transformer

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the electric load centers by matching demand and
          ! generator power output.

          ! METHODOLOGY EMPLOYED:
          ! Generators are dispatched in the sequence sepecified in the idf. The generators
          ! in the first electric load center are dispatched first. Generators are called
          ! right after dispatch and the remaining building load due for dispatch is updated
          ! using the actual generated power output not the requested value from the
          ! ElectricLoadCenter:Generators object(s).
          !
          ! REFERENCES: na

          ! USE STATEMENTS:

  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE General,         ONLY: TrimSigDigits
  USE DataGlobals,     ONLY: dooutputreporting, MetersHaveBeenInitialized, warmupflag, &
                             doingsizing, currenttime, BeginEnvrnFlag
  USE DataEnvironment, ONLY: Month, DayOfMonth
  USE DataHVACGlobals, only: SysTimeElapsed

  IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)   :: FirstHVACIteration
  LOGICAL, INTENT(INOUT) :: SimElecCircuits ! simulation convergence flag
  LOGICAL, INTENT (IN)   :: UpdateMetersOnly ! if true then don't resimulate generators, just update meters.

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: GenNum = 0          ! Generator number counter
  INTEGER           :: LoadCenterNum = 0   ! Load center number counter
  INTEGER           :: TransfNum = 0       ! Transformer number counter
  INTEGER           :: MeterNum = 0        ! A transformer's meter number counter
  INTEGER           :: MeterIndex = 0      ! Meter index number from GetMeterIndex
  INTEGER, SAVE     :: ElecFacilityIndex = 0
  REAL(r64)         :: ElecFacilityBldg = 0.0D0
  REAL(r64)         :: ElecFacilityHVAC = 0.0D0
  REAL(r64)         :: ElecProducedPV = 0.0D0
  REAL(r64)         :: ElecProducedWT = 0.0D0
  REAL(r64)         :: RemainingLoad = 0.0D0          ! Remaining electric power load to be met by a load center
  REAL(r64)         :: WholeBldgRemainingLoad = 0.0D0 ! Remaining electric power load for the building
  REAL(r64)         :: RemainingThermalLoad = 0.0D0   ! Remaining thermal load to be met
  LOGICAL,SAVE      :: MyOneTimeFlag = .true.
  REAL(r64)         :: CustomMeterDemand  = 0.0D0 ! local variable for Custom metered elec demand
  INTEGER, external      :: GetMeterIndex
  REAL(r64), external    :: GetInstantMeterValue
  REAL(r64), external    :: GetCurrentMeterValue
  CHARACTER(len=MaxNameLength), external :: GetMeterResourceType
  LOGICAL, SAVE :: MyEnvrnFlag=.true.

  REAL(r64)         :: ElectricProdRate = 0.0d0    ! Electric Power Production Rate of Generators
  REAL(r64)         :: ThermalProdRate = 0.0d0     ! Thermal Power Production Rate of Generators
  REAL(r64)         :: ExcessThermalPowerRequest = 0.0d0   ! Excess Thermal Power Request

  REAL(r64)         :: LoadCenterElectricLoad = 0.0D0 ! Load center electric load to be dispatched
  REAL(r64)         :: LoadCenterThermalLoad = 0.0D0  ! Load center thermal load to be dispatched
  REAL(r64)         :: StorageDrawnPower = 0.0D0      ! Electric Power Draw Rate from storage units
  REAL(r64)         :: StorageStoredPower = 0.0D0     ! Electric Power Store Rate from storage units


  ! Get Generator data from input file
  IF (GetInput) THEN
    CALL GetPowerManagerInput
    GetInput = .FALSE.
  END IF

  ! Setting up the Internal Meters and getting their indexes is done only once
  IF(MetersHaveBeenInitialized .and. MyOneTimeFlag) THEN
    ElecFacilityIndex      = GetMeterIndex('Electricity:Facility')
    ElecProducedCoGenIndex = GetMeterIndex('Cogeneration:ElectricityProduced')
    ElecProducedPVIndex    = GetMeterIndex('Photovoltaic:ElectricityProduced')
    ElecProducedWTIndex    = GetMeterIndex('WindTurbine:ElectricityProduced')

    DO LoadCenterNum = 1, NumLoadCenters
      ElecLoadCenter(LoadCenterNum)%DemandMeterPtr =  GetMeterIndex(ElecLoadCenter(LoadCenterNum)%DemandMeterName)
    ENDDO

    DO TransfNum = 1, NumTransformers    !NumTransformers is a module variable
      IF(Transformer(TransfNum)%UsageMode == PowerInFromGrid) THEN
        DO MeterNum = 1, SIZE(Transformer(TransfNum)%WiredMeterNames)
          MeterIndex = GetMeterIndex(Transformer(TransfNum)%WiredMeterNames(MeterNum))
          Transformer(TransfNum)%WiredMeterPtrs(MeterNum) = MeterIndex

          !Check whether the meter is an electricity meter
          !Index function is used here because some resource types are not Electricity but strings containing
          ! Electricity such as ElectricityPurchased and ElectricityProduced.
          !It is not proper to have this check in GetInput routine because the meter index may have not been defined
          IF( INDEX(GetMeterResourceType(MeterIndex),'Electricity') == 0) THEN
            CALL ShowFatalError('Non-electricity meter used for '// Transformer(TransfNum)%Name)
          END IF
        END DO
      ENDIF
    END DO

    MyOneTimeFlag = .FALSE.

  END IF

  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    WholeBldgElectSummary%ElectricityProd      = 0.d0
    WholeBldgElectSummary%ElectProdRate        = 0.d0
    WholeBldgElectSummary%ElectricityPurch     = 0.d0
    WholeBldgElectSummary%ElectPurchRate       = 0.d0
    WholeBldgElectSummary%ElectSurplusRate     = 0.d0
    WholeBldgElectSummary%ElectricitySurplus   = 0.d0
    WholeBldgElectSummary%ElectricityNetRate   = 0.d0
    WholeBldgElectSummary%ElectricityNet       = 0.d0
    WholeBldgElectSummary%TotalBldgElecDemand  = 0.d0
    WholeBldgElectSummary%TotalHVACElecDemand  = 0.d0
    WholeBldgElectSummary%TotalElectricDemand  = 0.d0
    WholeBldgElectSummary%ElecProducedPVRate   = 0.d0
    WholeBldgElectSummary%ElecProducedWTRate   = 0.d0

    IF (NumLoadCenters > 0) THEN
      ElecLoadCenter%DCElectricityProd           = 0.d0
      ElecLoadCenter%DCElectProdRate             = 0.d0
      ElecLoadCenter%DCpowerConditionLosses      = 0.d0
      ElecLoadCenter%ElectricityProd             = 0.d0
      ElecLoadCenter%ElectProdRate               = 0.d0
      ElecLoadCenter%ThermalProd                 = 0.d0
      ElecLoadCenter%ThermalProdRate             = 0.d0
      ElecLoadCenter%TotalPowerRequest           = 0.d0
      ElecLoadCenter%TotalThermalPowerRequest    = 0.d0
      ElecLoadCenter%ElectDemand                 = 0.d0
    ENDIF

    DO LoadCenterNum = 1, NumLoadCenters
      IF (ElecLoadCenter(LoadCenterNum)%NumGenerators == 0) CYCLE
      ElecLoadCenter(LoadCenterNum)%ElecGen%ONThisTimestep      = .FALSE.
      ElecLoadCenter(LoadCenterNum)%ElecGen%DCElectricityProd   = 0.d0
      ElecLoadCenter(LoadCenterNum)%ElecGen%DCElectProdRate     = 0.d0
      ElecLoadCenter(LoadCenterNum)%ElecGen%ElectricityProd     = 0.d0
      ElecLoadCenter(LoadCenterNum)%ElecGen%ElectProdRate       = 0.d0
      ElecLoadCenter(LoadCenterNum)%ElecGen%ThermalProd         = 0.d0
      ElecLoadCenter(LoadCenterNum)%ElecGen%ThermalProdRate     = 0.d0
    ENDDO

    IF (NumInverters > 0) THEN
      Inverter%AncillACuseRate                   = 0.d0
      Inverter%AncillACuseEnergy                 = 0.d0
      Inverter%QdotconvZone                      = 0.d0
      Inverter%QdotRadZone                       = 0.d0
    ENDIF

    IF (NumElecStorageDevices > 0) THEN
      ElecStorage%PelNeedFromStorage             = 0.d0
      ElecStorage%PelFromStorage                 = 0.d0
      ElecStorage%PelIntoStorage                 = 0.d0
      ElecStorage%QdotConvZone                   = 0.d0
      ElecStorage%QdotRadZone                    = 0.d0
      ElecStorage%TimeElapsed                    = 0.d0
      ElecStorage%ElectEnergyinStorage           = 0.d0
      ElecStorage%StoredPower                    = 0.d0
      ElecStorage%StoredEnergy                   = 0.d0
      ElecStorage%DecrementedEnergyStored        = 0.d0
      ElecStorage%DrawnPower                     = 0.d0
      ElecStorage%DrawnEnergy                    = 0.d0
      ElecStorage%ThermLossRate                  = 0.d0
      ElecStorage%ThermLossEnergy                = 0.d0
    ENDIF
    MyEnvrnFlag=.false.
  ENDIF
  IF (.not. BeginEnvrnFlag) MyEnvrnFlag=.TRUE.

 ! Determine the demand from the simulation for Demand Limit and Track Electrical and Reporting
  ElecFacilityBldg=GetInstantMeterValue(ElecFacilityIndex,1)
  ElecFacilityHVAC=GetInstantMeterValue(ElecFacilityIndex,2)
  ! deprecate this PV stuff?
  ElecProducedPV = GetInstantMeterValue(ElecProducedPVIndex,2)
  ElecProducedWT = GetInstantMeterValue(ElecProducedWTIndex,2)

  WholeBldgElectSummary%TotalBldgElecDemand = ElecFacilityBldg/(TimeStepZone*SecInHour)
  WholeBldgElectSummary%TotalHVACElecDemand = ElecFacilityHVAC/(TimeStepSys*SecInHour)
  WholeBldgElectSummary%TotalElectricDemand = WholeBldgElectSummary%TotalBldgElecDemand +  &
                                               WholeBldgElectSummary%TotalHVACElecDemand
  WholeBldgElectSummary%ElecProducedPVRate  = ElecProducedPV/(TimeStepSys*SecInHour)
  WholeBldgElectSummary%ElecProducedWTRate  = ElecProducedWT/(TimeStepSys*SecInHour)

  WholeBldgRemainingLoad = WholeBldgElectSummary%TotalElectricDemand !- WholeBldgElectSummary%ElecProducedPVRate


  IF (UpdateMetersOnly) THEN ! just update record keeping, don't resimulate load centers
    CALL ManageTransformers()

    CALL UpdateWholeBuildingRecords
    RETURN
  ENDIF


  ! dispatch across load centers and generators keeping track of remaining whole building load.

  DO LoadCenterNum = 1, NumLoadCenters

    IF ((ElecLoadCenter(LoadCenterNum)%DemandMeterPtr == 0 ) .AND.  &
      (ElecLoadCenter(LoadCenterNum)%OperationScheme == iOpSchemeTrackMeter) ) THEN ! keep trying to setup
        ElecLoadCenter(LoadCenterNum)%DemandMeterPtr =  GetMeterIndex(ElecLoadCenter(LoadCenterNum)%DemandMeterName)
    ENDIF

    ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = 0.0d0
    ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest = 0.0d0

   ! Check Operation Scheme and assign power generation load
   ! Both the Demand Limit and Track Electrical schemes will sequentially load the available generators.  All demand
   ! not met by available generator capacity will be met by purchased electrical.
   ! If a generator is needed in the simulation for a small load and it is less than the minimum part load ratio
   ! the generator will operate at the minimum part load ratio and the excess will either reduce demand or
   ! be available for storage or sell back to the power company.
    TypeOfEquip: SELECT CASE (ElecLoadCenter(LoadCenterNum)%OperationScheme)

    CASE (iOpSchemeBaseLoad)  ! 'BASELOAD'

    LoadCenterElectricLoad = WholeBldgRemainingLoad

      DO GenNum = 1, ElecLoadCenter(LoadCenterNum)%NumGenerators

        IF(GetCurrentScheduleValue(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%AvailSchedPtr) .gt. 0.0D0) THEN
          ! Set the Operation Flag
          ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
          ! Set the electric generator load request
          ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut
        ELSE
          ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
          ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = 0.0D0
        END IF

        ! now handle EMS override
        IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
          ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
             MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
          IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
            ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
          ELSE
            ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
          ENDIF
        ENDIF

        ! Get generator's actual electrical and thermal power outputs
        CALL GeneratorPowerOutput(LoadCenterNum,GenNum,FirstHVACIteration,ElectricProdRate,ThermalProdRate)

        ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest    &
                                + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep

        WholeBldgRemainingLoad = WholeBldgRemainingLoad - ElectricProdRate  ! Update whole building remaining load
      END DO

    CASE (iOpSchemeDemandLimit)  ! 'DEMAND LIMIT'
     ! The Demand Limit scheme tries to have the generators meet all of the demand above the purchased Electric
     !  limit set by the user.
     RemainingLoad = WholeBldgRemainingLoad - ElecLoadCenter(LoadCenterNum)%DemandLimit
     LoadCenterElectricLoad = RemainingLoad

     DO GenNum = 1, ElecLoadCenter(LoadCenterNum)%NumGenerators

       IF(GetCurrentScheduleValue(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%AvailSchedPtr) .gt. 0.0D0 &
          .and. RemainingLoad > 0.0D0) THEN
         ! Set the Operation Flag
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.

         ! Set the electric generator load
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                 MIN(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut,RemainingLoad)

         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF

       ELSE
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = 0.0D0

         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF
       END IF

       ! Get generator's actual electrical and thermal power outputs
       CALL GeneratorPowerOutput(LoadCenterNum,GenNum,FirstHVACIteration,ElectricProdRate,ThermalProdRate)

       IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest &
                                 + MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
       ELSE
          IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
              ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest  &
                                                    + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut
              ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =  &
                                 MIN(LoadCenterElectricLoad,ElecLoadCenter(LoadCenterNum)%TotalPowerRequest)
          ENDIF
       ENDIF
       RemainingLoad = RemainingLoad - ElectricProdRate ! Update remaining load to be met by this load center
       WholeBldgRemainingLoad = WholeBldgRemainingLoad - ElectricProdRate ! Update whole building remaining load

     END DO

   CASE (iOpSchemeTrackElectrical)  ! 'TRACK ELECTRICAL'
     !The Track Electrical scheme tries to have the generators meet all of the electrical demand for the building.
     RemainingLoad = WholeBldgRemainingLoad
     LoadCenterElectricLoad = RemainingLoad

     DO GenNum = 1, ElecLoadCenter(LoadCenterNum)%NumGenerators

       IF(GetCurrentScheduleValue(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%AvailSchedPtr) .gt. 0.0D0 &
          .and. RemainingLoad > 0.0D0) THEN
         ! Set the Operation Flag
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.

         ! Set the electric generator load
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                 MIN(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut,RemainingLoad)

         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF

       ELSE
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = 0.0D0

         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF
       END IF

       ! Get generator's actual electrical and thermal power outputs
       CALL GeneratorPowerOutput(LoadCenterNum,GenNum,FirstHVACIteration,ElectricProdRate,ThermalProdRate)

       IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest &
                                 + MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
       ELSE
          IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
              ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest  &
                                                    + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut
              ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =  &
                                 MIN(LoadCenterElectricLoad,ElecLoadCenter(LoadCenterNum)%TotalPowerRequest)
          ENDIF
       ENDIF
       RemainingLoad = RemainingLoad - ElectricProdRate ! Update remaining load to be met by this load center
       WholeBldgRemainingLoad = WholeBldgRemainingLoad - ElectricProdRate ! Update whole building remaining load

     END DO

   CASE (iOpSchemeTrackSchedule) ! 'TRACK SCHEDULE'
     ! The Track Schedule scheme tries to have the generators meet the electrical demand determined from a schedule.
     !  Code is very similar to 'Track Electrical' except for initial RemainingLoad is replaced by SchedElecDemand
     !  and PV production is ignored.
     RemainingLoad = GetCurrentScheduleValue(ElecLoadCenter(LoadCenterNum)%TrackSchedPtr)
     LoadCenterElectricLoad = RemainingLoad

     DO GenNum = 1, ElecLoadCenter(LoadCenterNum)%NumGenerators

       IF(GetCurrentScheduleValue(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%AvailSchedPtr) .gt. 0.0D0 &
          .and. RemainingLoad > 0.0D0) THEN
         ! Set the Operation Flag
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.

         ! Set the electric generator load
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                 MIN(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut,RemainingLoad)

         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF
       ELSE
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = 0.0D0

         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF
       END IF

       ! Get generator's actual electrical and thermal power outputs
       CALL GeneratorPowerOutput(LoadCenterNum,GenNum,FirstHVACIteration,ElectricProdRate,ThermalProdRate)

       IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest &
                                 + MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
       ELSE
          IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
              ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest  &
                                                    + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut
              ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =  &
                                 MIN(LoadCenterElectricLoad,ElecLoadCenter(LoadCenterNum)%TotalPowerRequest)
          ENDIF
       ENDIF
       RemainingLoad = RemainingLoad - ElectricProdRate ! Update remaining load to be met by this load center
       WholeBldgRemainingLoad = WholeBldgRemainingLoad - ElectricProdRate ! Update whole building remaining load

     END DO

   CASE (iOpSchemeTrackMeter)  ! 'TRACK METER'
     ! The TRACK CUSTOM METER scheme tries to have the generators meet all of the
     !   electrical demand from a meter, it can also be a user-defined Custom Meter
     !   and PV is ignored.
     CustomMeterDemand =  GetInstantMeterValue(ElecLoadCenter(LoadCenterNum)%DemandMeterPtr,1)/ (TimeStepZone * SecInHour) &
                          + GetInstantMeterValue(ElecLoadCenter(LoadCenterNum)%DemandMeterPtr,2)/ (TimeStepSys * SecInHour)

     RemainingLoad = CustomMeterDemand
     LoadCenterElectricLoad = RemainingLoad

     DO GenNum = 1, ElecLoadCenter(LoadCenterNum)%NumGenerators

       IF(GetCurrentScheduleValue(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%AvailSchedPtr) .gt. 0.0D0 &
          .and. RemainingLoad > 0.0D0) THEN
         ! Set the Operation Flag
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
         ! Set the electric generator load
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                 MIN(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut,RemainingLoad)

         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF

       ELSE
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = 0.0D0

         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF
       END IF

       ! Get generator's actual electrical and thermal power outputs
       CALL GeneratorPowerOutput(LoadCenterNum,GenNum,FirstHVACIteration,ElectricProdRate,ThermalProdRate)

       IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest &
                                 + MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
       ELSE
          IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
              ElecLoadCenter(LoadCenterNum)%TotalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest  &
                                                    + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut
              ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =  &
                                 MIN(LoadCenterElectricLoad,ElecLoadCenter(LoadCenterNum)%TotalPowerRequest)
          ENDIF
       ENDIF
       RemainingLoad = RemainingLoad - ElectricProdRate ! Update remaining load to be met by this load center
       WholeBldgRemainingLoad = WholeBldgRemainingLoad - ElectricProdRate ! Update whole building remaining load

     END DO

   CASE (iOpSchemeThermalFollow)
     ! Turn thermal load into an electrical load for cogenerators controlled to follow heat loads
     RemainingThermalLoad = 0.0D0

     CALL CalcLoadCenterThermalLoad(FirstHVACIteration, LoadCenterNum, RemainingThermalLoad)
     LoadCenterThermalLoad = RemainingThermalLoad

     DO GenNum = 1, ElecLoadCenter(LoadCenterNum)%NumGenerators

       IF(GetCurrentScheduleValue(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%AvailSchedPtr) .gt. 0.0D0 &
          .and. RemainingThermalLoad > 0.0D0) THEN

         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio > 0.0D0) THEN

           RemainingLoad = RemainingThermalLoad / ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio

           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                                         MIN(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut,RemainingLoad)

           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ! now handle EMS override
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                      MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
             IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
               ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
             ELSE
               ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
             ENDIF
           ENDIF

         ENDIF
       ELSE
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = 0.0D0

         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF
       ENDIF

       ! Get generator's actual electrical and thermal power outputs
       CALL GeneratorPowerOutput(LoadCenterNum,GenNum,FirstHVACIteration,ElectricProdRate,ThermalProdRate)

       IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN

          ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest =                                       &
                             ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest                      &
                          + (MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0))  &
                          * ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio
          ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =                                              &
                               ElecLoadCenter(LoadCenterNum)%TotalPowerRequest                           &
                               + (MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0))
       ELSE

          IF (ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest <  LoadCenterThermalLoad .AND. &
              ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0d0) THEN

             ExcessThermalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest         &
                                     + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut        &
                                * ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio  &
                                - LoadCenterThermalLoad

             IF ( ExcessThermalPowerRequest < 0.0d0 ) THEN
                ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest =                                  &
                                ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest                    &
                                + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut               &
                                * ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio
                ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =                                         &
                                  ElecLoadCenter(LoadCenterNum)%TotalPowerRequest                         &
                                + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut
             ELSE
               ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest = LoadCenterThermalLoad
               IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio > 0.0d0) THEN
                   ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =                                      &
                                  ElecLoadCenter(LoadCenterNum)%TotalPowerRequest                         &
                                + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut               &
                                - (ExcessThermalPowerRequest                                              &
                                / ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio)
               ENDIF
             ENDIF

          ENDIF

       ENDIF

       RemainingThermalLoad = RemainingThermalLoad - ThermalProdRate  ! Update remaining load to be met
                                                                      ! by this load center
       WholeBldgRemainingLoad = WholeBldgRemainingLoad - ElectricProdRate ! Update whole building remaining load

     ENDDO

   CASE (iOpSchemeThermalFollowLimitElectrical)
     !  Turn a thermal load into an electrical load for cogenerators controlled to follow heat loads.
     !  Add intitialization of RemainingThermalLoad as in the ThermalFollow operating scheme above.
     CALL CalcLoadCenterThermalLoad(FirstHVACIteration,LoadCenterNum, RemainingThermalLoad)
     ! Total current electrical demand for the building is a secondary limit.
     RemainingLoad = WholeBldgRemainingLoad
     LoadCenterElectricLoad = WholeBldgRemainingLoad
     LoadCenterThermalLoad = RemainingThermalLoad

     DO GenNum = 1, ElecLoadCenter(LoadCenterNum)%NumGenerators

       IF((GetCurrentScheduleValue(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%AvailSchedPtr) .gt. 0.0D0) &
          .and. (RemainingThermalLoad > 0.0D0) .and. (RemainingLoad > 0.0D0) ) THEN

         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio > 0.0D0) THEN

           RemainingLoad = MIN(WholeBldgRemainingLoad, RemainingThermalLoad /   &
                                      ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio)

           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                                         MIN(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut,RemainingLoad)

           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ! now handle EMS override
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                      MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
             IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
               ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
             ELSE
               ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
             ENDIF
           ENDIF

         ENDIF

       ELSE

         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
         ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = 0.0D0
         ! now handle EMS override
         IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN
           ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep = &
                    MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0)
           IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0D0) THEN
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .TRUE.
           ELSE
             ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep = .FALSE.
           ENDIF
         ENDIF

       ENDIF

       ! Get generator's actual electrical and thermal power outputs
       CALL GeneratorPowerOutput(LoadCenterNum,GenNum,FirstHVACIteration,ElectricProdRate,ThermalProdRate)

       IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSRequestOn) THEN

           ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest =                                       &
                              ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest                      &
                           + (MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0))  &
                           * ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio
           ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =                                              &
                                ElecLoadCenter(LoadCenterNum)%TotalPowerRequest                           &
                                + (MAX(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%EMSPowerRequest, 0.0D0))
       ELSE

          IF (ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest <  LoadCenterThermalLoad .AND. &
              ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep > 0.0d0) THEN

             ExcessThermalPowerRequest = ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest         &
                                     + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut        &
                                * ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio  &
                                - LoadCenterThermalLoad

             IF ( ExcessThermalPowerRequest < 0.0d0 ) THEN
                ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest =                                  &
                                ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest                    &
                                + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut               &
                                * ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio
                ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =                                         &
                                  ElecLoadCenter(LoadCenterNum)%TotalPowerRequest                         &
                                + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut
             ELSE
               ElecLoadCenter(LoadCenterNum)%TotalThermalPowerRequest = LoadCenterThermalLoad
               IF (ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio > 0.0d0) THEN
                   ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =                                      &
                                  ElecLoadCenter(LoadCenterNum)%TotalPowerRequest                         &
                                + ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%MaxPowerOut               &
                                - (ExcessThermalPowerRequest                                              &
                                / ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%NominalThermElectRatio)
               ENDIF
             ENDIF

             ElecLoadCenter(LoadCenterNum)%TotalPowerRequest =  &
                              MIN(LoadCenterElectricLoad,ElecLoadCenter(LoadCenterNum)%TotalPowerRequest)

          ENDIF

       ENDIF

       RemainingThermalLoad = RemainingThermalLoad - ThermalProdRate  ! Update remaining thermal load to
                                                                      ! be met by this load center

       WholeBldgRemainingLoad = WholeBldgRemainingLoad - ElectricProdRate ! Update whole building remaining
                                                                          ! electric load
     ENDDO

   CASE (0) ! This case allows for the reporting to be done without generators specified.

   CASE DEFAULT
     CALL ShowFatalError('Invalid operation scheme type for Electric Load Center='  &
                         //TRIM(ElecLoadCenter(LoadCenterNum)%Name))


   END SELECT TypeOfEquip

   ElecLoadCenter(LoadCenterNum)%ElectDemand = LoadCenterElectricLoad !To obtain the load for transformer

    IF ( (ElecLoadCenter(LoadCenterNum)%StoragePresent)  .AND. &
         (ElecLoadCenter(LoadCenterNum)%BussType  == DCBussInverterDCStorage) ) THEN
      CALL ManageElectCenterStorageInteractions(LoadCenterNum,StorageDrawnPower,StorageStoredPower)
!     Adjust whole building electric demand based on storage inputs and outputs
      WholeBldgRemainingLoad = WholeBldgRemainingLoad - StorageDrawnPower + StorageStoredPower
    ENDIF

    IF (ElecLoadCenter(LoadCenterNum)%InverterPresent) CALL ManageInverter(LoadCenterNum)
    IF ( (ElecLoadCenter(LoadCenterNum)%StoragePresent)  .AND. &
         ( (ElecLoadCenter(LoadCenterNum)%BussType  == DCBussInverterACStorage) &
           .OR. (ElecLoadCenter(LoadCenterNum)%BussType  == ACBussStorage) ) ) THEN
      CALL ManageElectCenterStorageInteractions(LoadCenterNum,StorageDrawnPower,StorageStoredPower)
      WholeBldgRemainingLoad = WholeBldgRemainingLoad - StorageDrawnPower + StorageStoredPower
    ENDIF

    CALL UpdateLoadCenterRecords(LoadCenterNum)

  ENDDO  !End of Load Center Do Loop

! The transformer call should be put outside of the "Load Center" loop because
! 1) A transformer may be for utility, not for load center
! 2) A tansformer may be shared by multiple load centers
  CALL ManageTransformers()

  CALL UpdateWholeBuildingRecords

  ! Need to simulate through the Elec Manager at least twice to ensure that Heat Recovery information is included.
  ! recheck this, may not be needed now that load centers are called more often.
  !  Does the IF condition also need to check if any thermal following strategies have been specified?
  !  That is, if only electrical following schemes, don't need to resimulate?
  IF(FirstHVACIteration)THEN
     SimElecCircuits = .TRUE.
  ELSE
     SimElecCircuits = .FALSE.
  END IF

RETURN

END SUBROUTINE ManageElectricLoadCenters


SUBROUTINE GetPowerManagerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   September 2000
          !       MODIFIED       B. Griffith, 2008 multiple load centers, inverter, storage
          !                      W. Wang, 2010 transformer
          !                      Y. KyungTae & W. Wang July-August, 2011 Add a battery model
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the load center data
          ! attributes from the input file

          ! METHODOLOGY EMPLOYED:
          ! calls the Input Processor to retrieve data from input file.
          ! The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
          ! following keywords is reflected exactly in this subroutine:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, &
                             SameString, MakeUPPERCase, GetObjectDefMaxArgs
  USE DataIPShortCuts
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE CurveManager,    ONLY: GetCurveIndex, GetCurveType
  Use DataHeatBalance, ONLY: Zone, IntGainTypeOf_ElectricLoadCenterInverterSimple, &
                             IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, &
                             IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, &
                             IntGainTypeOf_ElectricLoadCenterStorageSimple, &
                             IntGainTypeOf_ElectricLoadCenterStorageBattery, &
                             IntGainTypeOf_ElectricLoadCenterTransformer
  USE DataGlobals    , ONLY: NumOfZones, AnyEnergyManagementSystemInModel, ScheduleAlwaysOn
  USE DataInterfaces
  USE General,         ONLY: RoundSigDigits
  USE OutputReportPredefined

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '
  CHARACTER(len=*), PARAMETER :: RoutineName='GetPowerManagerInput: '
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, external :: GetMeterIndex

  INTEGER :: NumGenLists    ! Number of generator lists
  INTEGER :: AlphaCount     ! alpha input index
  INTEGER :: GenCount       ! generator counter index
  INTEGER :: Count          ! loop index
  INTEGER :: ListNum        ! List number index
  INTEGER :: NumAlphas      ! Number of elements in the alpha array
  INTEGER :: NumNums        ! Number of elements in the numeric array
  INTEGER :: IOStat         ! IO Status when calling get input subroutine
  !CHARACTER(len=MaxNameLength),DIMENSION(:), ALLOCATABLE :: Alpha !dimension to num of alpha fields in input
  !REAL(r64), DIMENSION(:), ALLOCATABLE   :: Num                   !dimension to num of numeric data fields in input
  LOGICAL               :: ErrorsFound=.false.  ! error in input
  LOGICAL               :: IsNotOK              ! Flag to verify name
  LOGICAL               :: IsBlank              ! Flag for blank name

  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: ListName
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: InverterNames
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: StorageNames
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)  :: TransformerNames
  INTEGER  :: AnyElectricityPresent ! local test for presence of Electricty in Facility
  INTEGER  :: NumGenerators ! local number of generators per electric load center
  LOGICAL  :: SetupWholeBldgReports
  !unused1208  INTEGER  :: MaxNumAlphas
  !unused1208  INTEGER  :: MaxNumArgs
  !unused1208  INTEGER  :: MaxNumNumbers
  INTEGER  :: NumofCECinverters
  INTEGER  :: NumofCurveInverters
  INTEGER  :: NumofSimpleInverters
  INTEGER  :: NumofSimpleElecStorage
  INTEGER  :: NumofKiBaMElecStorage
  INTEGER  :: InvertNum
  INTEGER  :: StorNum
  INTEGER  :: TransfNum
  INTEGER  :: Found
  INTEGER  :: NumAlphaBeforeMeter !Number of Alpha fields before the extensible meters
                                  !Used to derive the number of meters wired to a transformer
  INTEGER  :: NumWiredMeters      !Number of electric meters wired to a transformer
  INTEGER  :: LCofTransformer     !Index of load center served by a transformer
  INTEGER  :: LoopCount           !loop counter
  REAL(r64) :: pvTotalCapacity = 0.0d0    ! for LEED report
  REAL(r64) :: windTotalCapacity = 0.0d0   ! for LEED report

  NumAlphaBeforeMeter   = 7       !Hard coded. Changes might be needed if the transformer input structure gets changed
  LCofTransformer       = 0

  !FLOW:
  SetupWholeBldgReports = .FALSE.

  !first read in any inverters that might be associated with a load center
  NumofCECinverters    = GetNumObjectsFound('ElectricLoadCenter:Inverter:LookUpTable')
  NumofCurveInverters  = GetNumObjectsFound('ElectricLoadCenter:Inverter:FunctionOfPower')
  NumofSimpleInverters = GetNumObjectsFound('ElectricLoadCenter:Inverter:Simple')
  NumInverters         = NumofCECinverters + NumofCurveInverters + NumofSimpleInverters

  If (NumInverters > 0) Then
    Allocate(Inverter(NumInverters))
    Allocate(InverterNames(NumInverters))

    If (NumofCECinverters > 0) Then
      cCurrentModuleObject = 'ElectricLoadCenter:Inverter:LookUpTable'
      Do InvertNum = 1, NumofCECinverters
        CALL GetObjectItem(cCurrentModuleObject,InvertNum,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),InverterNames,InvertNum-1,IsNotOK,IsBlank,trim(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        InverterNames(InvertNum) =TRIM(cAlphaArgs(1))
        Inverter(InvertNum)%name = cAlphaArgs(1)
        Inverter(InvertNum)%ModelType = CECLookUpTableModel

        IF (lAlphaFieldBlanks(2)) THEN
          Inverter(InvertNum)%AvailSchedPtr = ScheduleAlwaysOn
        ELSE
          Inverter(InvertNum)%AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2))
          If ( Inverter(InvertNum)%AvailSchedPtr == 0 ) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
            ErrorsFound=.true.
          ENDIF
        ENDIF

        Inverter(InvertNum)%ZoneNum = FindItemInList(cAlphaArgs(3), Zone%Name, NumOfZones)
        IF (Inverter(InvertNum)%ZoneNum > 0) Inverter(InvertNum)%HeatLossesDestination  = ZoneGains
        IF (Inverter(InvertNum)%ZoneNum == 0) THEN
          IF (lAlphaFieldBlanks(3)) THEN
            Inverter(InvertNum)%HeatLossesDestination = LostToOutside
          ELSE
            Inverter(InvertNum)%HeatLossesDestination = LostToOutside
            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
            CAll ShowContinueError('Zone name not found. Inverter heat losses will not be added to a zone' )
            ! continue with simulation but inverter losses not sent to a zone.
          ENDIF
        ENDIF
        Inverter(InvertNum)%ZoneRadFract               = rNumericArgs(1)
        Inverter(InvertNum)%RatedPower                 = rNumericArgs(2)
        Inverter(InvertNum)%StandbyPower               = rNumericArgs(3)
        Inverter(InvertNum)%LUtable%NightTareLossPower = rNumericArgs(3)
        Inverter(InvertNum)%LUtable%NominalVoltage     = rNumericArgs(4)

        Inverter(InvertNum)%LUtable%NomVoltEfficiencyARR(1) = rNumericArgs(5)
        Inverter(InvertNum)%LUtable%NomVoltEfficiencyARR(2) = rNumericArgs(6)
        Inverter(InvertNum)%LUtable%NomVoltEfficiencyARR(3) = rNumericArgs(7)
        Inverter(InvertNum)%LUtable%NomVoltEfficiencyARR(4) = rNumericArgs(8)
        Inverter(InvertNum)%LUtable%NomVoltEfficiencyARR(5) = rNumericArgs(9)
        Inverter(InvertNum)%LUtable%NomVoltEfficiencyARR(6) = rNumericArgs(10)

      ENDDO
    ENDIF

    IF (NumofCurveInverters >0) THEN
      cCurrentModuleObject = 'ElectricLoadCenter:Inverter:FunctionOfPower'
      Do InvertNum = NumofCECinverters + 1, NumofCECinverters + NumofCurveInverters
        CALL GetObjectItem(cCurrentModuleObject,InvertNum,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),InverterNames,InvertNum-1,IsNotOK,IsBlank,trim(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        InverterNames(InvertNum) =TRIM(cAlphaArgs(1))
        Inverter(InvertNum)%ModelType = CurveFuncOfPower
        Inverter(InvertNum)%Name = cAlphaArgs(1)

        IF (lAlphaFieldBlanks(2)) THEN
          Inverter(InvertNum)%AvailSchedPtr = ScheduleAlwaysOn
        ELSE
          Inverter(InvertNum)%AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2))
          If ( Inverter(InvertNum)%AvailSchedPtr == 0 ) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
            ErrorsFound=.true.
          ENDIF
        ENDIF

        Inverter(InvertNum)%ZoneNum = FindItemInList(cAlphaArgs(3), Zone%Name, NumOfZones)
        IF (Inverter(InvertNum)%ZoneNum > 0) Inverter(InvertNum)%HeatLossesDestination  = ZoneGains
        IF (Inverter(InvertNum)%ZoneNum == 0) THEN
          IF (lAlphaFieldBlanks(3)) THEN
            Inverter(InvertNum)%HeatLossesDestination = LostToOutside
          ELSE
            Inverter(InvertNum)%HeatLossesDestination = LostToOutside
            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
                        ! continue with simulation but inverter losses not sent to a zone.
            CAll ShowContinueError('Zone name not found. Inverter heat losses will not be added to a zone' )
          ENDIF
        ENDIF
        Inverter(InvertNum)%CurveNum =  GetCurveIndex(cAlphaArgs(4))
        If (Inverter(InvertNum)%CurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(cAlphaArgs(4)) )
          CAll ShowContinueError('Curve was not found')
          ErrorsFound=.true.
        ENDIF
        Inverter(InvertNum)%ZoneRadFract  = rNumericArgs(1)
        Inverter(InvertNum)%RatedPower    = rNumericArgs(2)
        Inverter(InvertNum)%MinEfficiency = rNumericArgs(3)
        Inverter(InvertNum)%MaxEfficiency = rNumericArgs(4)
        Inverter(InvertNum)%MinPower      = rNumericArgs(5)
        Inverter(InvertNum)%MaxPower      = rNumericArgs(6)
        Inverter(InvertNum)%StandbyPower  = rNumericArgs(7)

      ENDDO
    ENDIF

    IF (NumofSimpleInverters > 0) THEN
      cCurrentModuleObject = 'ElectricLoadCenter:Inverter:Simple'
      DO InvertNum = NumofCECinverters + NumofCurveInverters +1, NumInverters
        CALL GetObjectItem(cCurrentModuleObject,InvertNum,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK = .false.
        IsBlank = .false.
        CALL VerifyName(cAlphaArgs(1),InverterNames,InvertNum-1,IsNotOK,IsBlank,trim(cCurrentModuleObject)//' Name')
        IF (IsNotOK) Then
          errorsFound = .true.
          If (IsBlank) cAlphaArgs(1)='xxxx'
        ENDIF
        InverterNames(InvertNum) = Trim(cAlphaArgs(1))
        Inverter(InvertNum)%Name = Trim(cAlphaArgs(1))
        Inverter(InvertNum)%ModelType = SimpleConstantEff

        IF (lAlphaFieldBlanks(2)) THEN
          Inverter(InvertNum)%AvailSchedPtr = ScheduleAlwaysOn
        ELSE
          Inverter(InvertNum)%AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2))
          If ( Inverter(InvertNum)%AvailSchedPtr == 0 ) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
            ErrorsFound=.true.
          ENDIF
        ENDIF

        Inverter(InvertNum)%ZoneNum = FindItemInList(cAlphaArgs(3), Zone%Name, NumOfZones)
        IF (Inverter(InvertNum)%ZoneNum > 0) Inverter(InvertNum)%HeatLossesDestination  = ZoneGains
        IF (Inverter(InvertNum)%ZoneNum == 0) THEN
          IF (lAlphaFieldBlanks(3)) THEN
            Inverter(InvertNum)%HeatLossesDestination = LostToOutside
          ELSE
            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
            CAll ShowContinueError('Zone name not found. Inverter heat losses will not be added to a zone' )
            ! continue with simulation but inverter losses not sent to a zone.
          ENDIF
        ENDIF
        Inverter(InvertNum)%ZoneRadFract  = rNumericArgs(1)
        Inverter(InvertNum)%Efficiency    = rNumericArgs(2)
      ENDDO
    ENDIF

    !setup reports for all inverters
    Do InvertNum = 1, NumInverters
      Call SetupOutputVariable('Inverter DC to AC Efficiency []', &
           Inverter(InvertNum)%Efficiency, 'System', 'Average', Inverter(InvertNum)%Name )
      Call SetupOutputVariable('Inverter DC Input Electric Power [W]', &
           Inverter(InvertNum)%DCPowerIn, 'System', 'Average', Inverter(InvertNum)%Name )
      Call SetupOutputVariable('Inverter DC Input Electric Energy [J]', &
           Inverter(InvertNum)%DCEnergyIn, 'System', 'Sum', Inverter(InvertNum)%Name )
      Call SetupOutputVariable('Inverter AC Output Electric Power [W]', &
           Inverter(InvertNum)%ACPowerOut, 'System', 'Average', Inverter(InvertNum)%Name )
      Call SetupOutputVariable('Inverter AC Output Electric Energy [J]', &
           Inverter(InvertNum)%ACEnergyOut, 'System', 'Sum', Inverter(InvertNum)%Name , &
           ResourceTypeKey='ElectricityProduced',EndUseKey='Photovoltaics',GroupKey='Plant') ! right now PV is the only DC source
      Call SetupOutputVariable('Inverter Thermal Loss Rate [W]', &
           Inverter(InvertNum)%ThermLossRate, 'System', 'Average', Inverter(InvertNum)%Name )
      Call SetupOutputVariable('Inverter Thermal Loss Energy [J]', &
           Inverter(InvertNum)%ThermLossEnergy, 'System', 'Sum', Inverter(InvertNum)%Name )
      Call SetupOutputVariable('Inverter Ancillary AC Electric Power [W]', &
           Inverter(InvertNum)%AncillACuseRate , 'System', 'Average', Inverter(InvertNum)%Name )
      Call SetupOutputVariable('Inverter Ancillary AC Electric Energy [J]', &
           Inverter(InvertNum)%AncillACuseEnergy , 'System', 'Sum', Inverter(InvertNum)%Name , &
           ResourceTypeKey='Electricity',EndUseKey='Cogeneration',GroupKey='Plant') ! called cogeneration for end use table
      IF (Inverter(InvertNum)%ZoneNum > 0) THEN
        SELECT CASE (Inverter(InvertNum)%ModelType)
        CASE (SimpleConstantEff)
          CALL SetupZoneInternalGain(Inverter(InvertNum)%ZoneNum, &
                     'ElectricLoadCenter:Inverter:Simple',  &
                     Inverter(InvertNum)%Name , &
                     IntGainTypeOf_ElectricLoadCenterInverterSimple,    &
                     ConvectionGainRate    =    Inverter(InvertNum)%QdotconvZone, &
                     ThermalRadiationGainRate = Inverter(InvertNum)%QdotRadZone )
        CASE (CurveFuncOfPower)
          CALL SetupZoneInternalGain(Inverter(InvertNum)%ZoneNum, &
                     'ElectricLoadCenter:Inverter:FunctionOfPower',  &
                     Inverter(InvertNum)%Name , &
                     IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower,    &
                     ConvectionGainRate    =    Inverter(InvertNum)%QdotconvZone, &
                     ThermalRadiationGainRate = Inverter(InvertNum)%QdotRadZone )
        CASE (CECLookUpTableModel)
          CALL SetupZoneInternalGain(Inverter(InvertNum)%ZoneNum, &
                     'ElectricLoadCenter:Inverter:LookUpTable',  &
                     Inverter(InvertNum)%Name , &
                     IntGainTypeOf_ElectricLoadCenterInverterLookUpTable,         &
                     ConvectionGainRate    =    Inverter(InvertNum)%QdotconvZone, &
                     ThermalRadiationGainRate = Inverter(InvertNum)%QdotRadZone)
        END SELECT
      ENDIF

    ENDDO

  ENDIF !any inverters

  !read in any electrical storage devices that may be associated with load centers.
  NumofSimpleElecStorage = GetNumObjectsFound('ElectricLoadCenter:Storage:Simple')
  NumofKiBaMElecStorage = GetNumObjectsFound('ElectricLoadCenter:Storage:Battery')
  NumElecStorageDevices  = NumofSimpleElecStorage + NumofKiBaMElecStorage
  IF (NumElecStorageDevices > 0) THEN
    ALLOCATE(ElecStorage(NumElecStorageDevices))
    ALLOCATE(StorageNames(NumElecStorageDevices))

    IF (NumofSimpleElecStorage > 0) THEN
      cCurrentModuleObject = 'ElectricLoadCenter:Storage:Simple'
      DO StorNum = 1, NumofSimpleElecStorage
        CALL GetObjectItem(cCurrentModuleObject, StorNum, cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1), StorageNames, StorNum-1, IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        If (IsNotOK) Then
          ErrorsFound = .true.
          If (IsBlank) cAlphaArgs(1) = 'xxxx'
        endif
        StorageNames(StorNum)=trim(cAlphaArgs(1))
        ElecStorage(StorNum)%Name =  cAlphaArgs(1)

        IF (lAlphaFieldBlanks(2)) THEN
          ElecStorage(StorNum)%AvailSchedPtr = ScheduleAlwaysOn
        ELSE
          ElecStorage(StorNum)%AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2))
          If ( ElecStorage(StorNum)%AvailSchedPtr == 0 ) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
            ErrorsFound=.true.
          ENDIF
        ENDIF

        ElecStorage(StorNum)%ZoneNum = FindItemInList(cAlphaArgs(3), Zone%Name, NumOfZones)
        IF (ElecStorage(StorNum)%ZoneNum > 0) ElecStorage(StorNum)%HeatLossesDestination  = ZoneGains
        IF (ElecStorage(StorNum)%ZoneNum == 0) THEN
          IF (lAlphaFieldBlanks(3)) THEN
            ElecStorage(StorNum)%HeatLossesDestination = LostToOutside
          ELSE
            ElecStorage(StorNum)%HeatLossesDestination = LostToOutside
            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
            CAll ShowContinueError('Zone name not found. Electrical storage heat losses will not be added to a zone' )
            !continue with simulation but storage losses not sent to a zone.
          ENDIF
        ENDIF

        ElecStorage(StorNum)%StorageModelMode        = SimpleBucketStorage
        ElecStorage(StorNum)%ZoneRadFract            = rNumericArgs(1)
        ElecStorage(StorNum)%EnergeticEfficCharge    = rNumericArgs(2)
        ElecStorage(StorNum)%EnergeticEfficDischarge = rNumericArgs(3)
        ElecStorage(StorNum)%MaxEnergyCapacity       = rNumericArgs(4)
        ElecStorage(StorNum)%MaxPowerDraw            = rNumericArgs(5)
        ElecStorage(StorNum)%MaxPowerStore           = rNumericArgs(6)
        ElecStorage(StorNum)%StartingEnergyStored    = rNumericArgs(7)

        CALL SetupOutputVariable('Electric Storage Charge State [J]', &
             ElecStorage(StorNum)%ElectEnergyinStorage, 'System', 'Average', ElecStorage(StorNum)%Name ) !? 'Sum'

      ENDDO

    ENDIF !any simple storage


    IF (NumofKiBaMElecStorage > 0) THEN
      cCurrentModuleObject = 'ElectricLoadCenter:Storage:Battery'
      DO StorNum = 1+NumofSimpleElecStorage, NumofKiBaMElecStorage
        CALL GetObjectItem(cCurrentModuleObject, StorNum, cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1), StorageNames, StorNum-1, IsNotOK,IsBlank,trim(cCurrentModuleObject)//' Name')
        If (IsNotOK) Then
          ErrorsFound = .true.
          If (IsBlank) cAlphaArgs(1) = 'xxxx'
        endif
        StorageNames(StorNum)=trim(cAlphaArgs(1))
        ElecStorage(StorNum)%Name =  cAlphaArgs(1)

        IF (lAlphaFieldBlanks(2)) THEN
          ElecStorage(StorNum)%AvailSchedPtr = ScheduleAlwaysOn
        ELSE
          ElecStorage(StorNum)%AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2))
          If ( ElecStorage(StorNum)%AvailSchedPtr == 0 ) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
            ErrorsFound=.true.
          ENDIF
        ENDIF

        ElecStorage(StorNum)%ZoneNum = FindItemInList(cAlphaArgs(3), Zone%Name, NumOfZones)
        IF (ElecStorage(StorNum)%ZoneNum > 0) ElecStorage(StorNum)%HeatLossesDestination  = ZoneGains
        IF (ElecStorage(StorNum)%ZoneNum == 0) THEN
          IF (lAlphaFieldBlanks(3)) THEN
            ElecStorage(StorNum)%HeatLossesDestination = LostToOutside
          ELSE
            ElecStorage(StorNum)%HeatLossesDestination = LostToOutside
            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
            CAll ShowContinueError('Zone name not found. Electrical storage heat losses will not be added to a zone')
            !continue with simulation but storage losses not sent to a zone.
          ENDIF
        ENDIF


        ElecStorage(StorNum)%ChargeCurveNum = GetCurveIndex(cAlphaArgs(4)) !voltage calculation for charging
          IF(ElecStorage(StorNum)%ChargeCurveNum.EQ. 0 .and. .not. lAlphaFieldBlanks(4))THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
              ErrorsFound=.true.
          ELSEIF (lAlphaFieldBlanks(4)) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//' cannot be blank. But no entry found.')
              ErrorsFound=.true.
          ELSEIF (.not. SameString(GetCurveType(ElecStorage(StorNum)%ChargeCurveNum),'RectangularHyperbola2')) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
              CALL ShowContinueError('Curve Type must be RectangularHyperbola2 but was '//  &
                 trim(GetCurveType(ElecStorage(StorNum)%ChargeCurveNum)))
              ErrorsFound=.true.
          ENDIF
        ElecStorage(StorNum)%DischargeCurveNum = GetCurveIndex(cAlphaArgs(5)) ! voltage calculation for discharging
          IF(ElecStorage(StorNum)%DischargeCurveNum.EQ. 0 .and. .not. lAlphaFieldBlanks(5))THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
              ErrorsFound=.true.
          ELSEIF (lAlphaFieldBlanks(5)) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(5))//' cannot be blank. But no entry found.')
              ErrorsFound=.true.
          ELSEIF (.not. SameString(GetCurveType(ElecStorage(StorNum)%DischargeCurveNum),'RectangularHyperbola2')) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
              CALL ShowContinueError('Curve Type must be RectangularHyperbola2 but was '//  &
                 trim(GetCurveType(ElecStorage(StorNum)%DischargeCurveNum)))
              ErrorsFound=.true.
          ENDIF

        IF (SameString(cAlphaArgs(6),'Yes')) THEN
            ElecStorage(StorNum)%LifeCalculation = Battery_LifeCalculation_Yes
        ELSEIF(SameString(cAlphaArgs(6),'No')) THEN
            ElecStorage(StorNum)%LifeCalculation = Battery_LifeCalculation_No
        ELSE
            CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6)) )
            CAll ShowContinueError('Yes or No should be selected. Default value No is used to continue simulation')
            ElecStorage(StorNum)%LifeCalculation = Battery_LifeCalculation_No
        ENDIF

        IF(ElecStorage(StorNum)%LifeCalculation == Battery_LifeCalculation_Yes) THEN
            ElecStorage(StorNum)%LifeCurveNum = GetCurveIndex(cAlphaArgs(7)) !Battery life calculation
            IF(ElecStorage(StorNum)%LifeCurveNum.EQ. 0 .and. .not. lAlphaFieldBlanks(7))THEN
               CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
               CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
               ErrorsFound=.true.
            ELSEIF (lAlphaFieldBlanks(7)) THEN
               CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
               CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//' cannot be blank when '//  &
                  trim(cAlphaArgs(6))//' = Yes. But no entry found.')
               ErrorsFound=.true.
            ELSEIF (.not. SameString(GetCurveType(ElecStorage(StorNum)%LifeCurveNum),'DoubleExponentialDecay')) THEN
              CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
              CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
              CALL ShowContinueError('Curve Type must be DoubleExponentialDecay but was '//  &
                 trim(GetCurveType(ElecStorage(StorNum)%LifeCurveNum)))
              ErrorsFound=.true.
            ENDIF
            
            ElecStorage(StorNum)%CycleBinNum= rNumericArgs(14)
            
            IF (.not. ErrorsFound) THEN  ! life cycle calculation for this battery, allocate arrays for degradation calculation
              ALLOCATE(ElecStorage(StorNum)%B10(1:MAXRainflowArrayBounds+1))
              ALLOCATE(ElecStorage(StorNum)%X0(1:MAXRainflowArrayBounds+1))
              ALLOCATE(ElecStorage(StorNum)%Nmb0(1:ElecStorage(StorNum)%CycleBinNum))
              ALLOCATE(ElecStorage(StorNum)%OneNmb0(1:ElecStorage(StorNum)%CycleBinNum))
              ElecStorage(StorNum)%B10=0.0d0
              ElecStorage(StorNum)%X0=0.0d0
              ElecStorage(StorNum)%Nmb0=0.0d0
              ElecStorage(StorNum)%OneNmb0=0.0d0
            ENDIF
        ENDIF


        ElecStorage(StorNum)%StorageModelMode            = KiBaMBattery
        ElecStorage(StorNum)%ZoneRadFract                = rNumericArgs(1)
        ElecStorage(StorNum)%ParallelNum                 = rNumericArgs(2)
        ElecStorage(StorNum)%SeriesNum                   = rNumericArgs(3)
        ElecStorage(StorNum)%MaxAhCapacity               = rNumericArgs(4)
        ElecStorage(StorNum)%StartingSOC                 = rNumericArgs(5)
        ElecStorage(StorNum)%AvailableFrac               = rNumericArgs(6)
        ElecStorage(StorNum)%ChargeConversionRate        = rNumericArgs(7)
        ElecStorage(StorNum)%ChargedOCV                  = rNumericArgs(8)
        ElecStorage(StorNum)%DischargedOCV               = rNumericArgs(9)
        ElecStorage(StorNum)%InternalR                   = rNumericArgs(10)
        ElecStorage(StorNum)%MaxDischargeI               = rNumericArgs(11)
        ElecStorage(StorNum)%CutoffV                     = rNumericArgs(12)
        ElecStorage(StorNum)%MaxChargeRate               = rNumericArgs(13)



        CALL SetupOutputVariable('Electric Storage Operating Mode Index []', &
             ElecStorage(StorNum)%StorageMode, 'System', 'Average', ElecStorage(StorNum)%Name )
        CALL SetupOutputVariable('Electric Storage Charge State [Ah]', &
             ElecStorage(StorNum)%AbsoluteSOC, 'System', 'Average', ElecStorage(StorNum)%Name )
        CALL SetupOutputVariable('Electric Storage Charge Fraction []', &
             ElecStorage(StorNum)%FractionSOC, 'System', 'Average', ElecStorage(StorNum)%Name )
        CALL SetupOutputVariable('Electric Storage Total Current [A]', &
             ElecStorage(StorNum)%BatteryCurrent, 'System', 'Average', ElecStorage(StorNum)%Name)
        CALL SetupOutputVariable('Electric Storage Total Voltage [V]', &
             ElecStorage(StorNum)%BatteryVoltage, 'System', 'Average', ElecStorage(StorNum)%Name)

        IF(ElecStorage(StorNum)%LifeCalculation == Battery_LifeCalculation_Yes) THEN
            CALL SetupOutputVariable('Electric Storage Degradation Fraction []', &
                 ElecStorage(StorNum)%BatteryDamage, 'System', 'Average', ElecStorage(StorNum)%Name)
        ENDIF

      ENDDO

    ENDIF !any kibam storage

    !For any battery
    DO StorNum = 1, NumofSimpleElecStorage+NumofKiBaMElecStorage
        CALL SetupOutputVariable('Electric Storage Charge Power [W]', &
             ElecStorage(StorNum)%StoredPower, 'System', 'Average', ElecStorage(StorNum)%Name )
        CALL SetupOutputVariable('Electric Storage Charge Energy [J]', &
             ElecStorage(StorNum)%StoredEnergy, 'System', 'Sum', ElecStorage(StorNum)%Name )
        CALL SetupOutputVariable('Electric Storage Production Decrement Energy [J]', &
             ElecStorage(StorNum)%DecrementedEnergyStored, 'System', 'Sum', ElecStorage(StorNum)%Name ,&
             ResourceTypeKey='ElectricityProduced',EndUseKey='COGENERATION',GroupKey='Plant')
        CALL SetupOutputVariable('Electric Storage Discharge Power [W]', &
             ElecStorage(StorNum)%DrawnPower, 'System', 'Average', ElecStorage(StorNum)%Name )
        CALL SetupOutputVariable('Electric Storage Discharge Energy [J]', &
             ElecStorage(StorNum)%DrawnEnergy, 'System', 'Sum', ElecStorage(StorNum)%Name ,&
              ResourceTypeKey='ElectricityProduced',EndUseKey='COGENERATION',GroupKey='Plant')
        CALL SetupOutputVariable('Electric Storage Thermal Loss Rate [W]', &
             ElecStorage(StorNum)%ThermLossRate, 'System', 'Average', ElecStorage(StorNum)%Name )
        CALL SetupOutputVariable('Electric Storage Thermal Loss Energy [J]', &
             ElecStorage(StorNum)%ThermLossEnergy, 'System', 'Sum', ElecStorage(StorNum)%Name )
        IF ( AnyEnergyManagementSystemInModel) THEN
          IF(ElecStorage(StorNum)%StorageModelMode == SimpleBucketStorage) THEN
              CALL SetupEMSInternalVariable('Electrical Storage Maximum Capacity', ElecStorage(StorNum)%Name, '[J]', &
                     ElecStorage(StorNum)%MaxEnergyCapacity  )
          ELSEIF(ElecStorage(StorNum)%StorageModelMode == KiBaMBattery) THEN
              CALL SetupEMSInternalVariable('Electrical Storage Maximum Capacity', ElecStorage(StorNum)%Name, '[Ah]', &
                     ElecStorage(StorNum)%MaxAhCapacity  )
          ENDIF

          CALL SetupEMSActuator('Electrical Storage', ElecStorage(StorNum)%Name, 'Power Draw Rate' , '[W]', &
                     ElecStorage(StorNum)%EMSOverridePelFromStorage, ElecStorage(StorNum)%EMSValuePelFromStorage )
          CALL SetupEMSActuator('Electrical Storage', ElecStorage(StorNum)%Name, 'Power Charge Rate' , '[W]', &
                     ElecStorage(StorNum)%EMSOverridePelIntoStorage, ElecStorage(StorNum)%EMSValuePelIntoStorage )
        ENDIF

        IF (ElecStorage(StorNum)%ZoneNum > 0) THEN
          SELECT CASE (ElecStorage(StorNum)%StorageModelMode)

          CASE (SimpleBucketStorage)
            CALL SetupZoneInternalGain(ElecStorage(StorNum)%ZoneNum, &
                     'ElectricLoadCenter:Storage:Simple',  &
                     ElecStorage(StorNum)%Name , &
                     IntGainTypeOf_ElectricLoadCenterStorageSimple,         &
                     ConvectionGainRate    =    ElecStorage(StorNum)%QdotconvZone, &
                     ThermalRadiationGainRate = ElecStorage(StorNum)%QdotRadZone)
          CASE (KiBaMBattery)
            CALL SetupZoneInternalGain(ElecStorage(StorNum)%ZoneNum, &
                     'ElectricLoadCenter:Storage:Battery',  &
                     ElecStorage(StorNum)%Name , &
                     IntGainTypeOf_ElectricLoadCenterStorageBattery,         &
                     ConvectionGainRate    =    ElecStorage(StorNum)%QdotconvZone, &
                     ThermalRadiationGainRate = ElecStorage(StorNum)%QdotRadZone)
          END SELECT
        ENDIF

     ENDDO

  ENDIF !any storage at all


  !read in any electrical transformers that may be associated with load centers.
  NumTransformers = GetNumObjectsFound('ElectricLoadCenter:Transformer')
  IF (NumTransformers > 0) THEN
    ALLOCATE(Transformer(NumTransformers))
    ALLOCATE(TransformerNames(NumTransformers))

    cCurrentModuleObject = 'ElectricLoadCenter:Transformer'
    DO TransfNum = 1, NumTransformers
      CALL GetObjectItem(cCurrentModuleObject, TransfNum, cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                         NumBlank=lNumericFieldBlanks, &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1), TransformerNames, TransfNum-1, IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .true.
        IF (IsBlank) cAlphaArgs(1) = 'xxxx'  !Actually, this line is not necessary because name is a required field
      ENDIF
      TransformerNames(TransfNum) = TRIM(cAlphaArgs(1))
      Transformer(TransfNum)%Name =  cAlphaArgs(1)

      IF (lAlphaFieldBlanks(2)) THEN
        Transformer(TransfNum)%AvailSchedPtr = ScheduleAlwaysOn
      ELSE
        Transformer(TransfNum)%AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2))
        IF ( Transformer(TransfNum)%AvailSchedPtr == 0 ) THEN
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
          ErrorsFound=.true.
        ENDIF
      ENDIF

      IF (SameString(cAlphaArgs(3) ,    'PowerInFromGrid') )    THEN
        Transformer(TransfNum)%UsageMode     = PowerInFromGrid
      ELSEIF (SameString(cAlphaArgs(3), 'PowerOutFromOnsiteGeneration' ) )   THEN
        Transformer(TransfNum)%UsageMode     = PowerOutFromBldg
      ELSE
        CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
        ErrorsFound=.true.
      ENDIF

      Transformer(TransfNum)%ZoneNum = FindItemInList(cAlphaArgs(4), Zone%Name, NumOfZones)
      IF (Transformer(TransfNum)%ZoneNum > 0) Transformer(TransfNum)%HeatLossesDestination  = ZoneGains
      IF (Transformer(TransfNum)%ZoneNum == 0) THEN
        IF (lAlphaFieldBlanks(4)) THEN
          Transformer(TransfNum)%HeatLossesDestination = LostToOutside
        ELSE
          Transformer(TransfNum)%HeatLossesDestination = LostToOutside
          CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(cAlphaArgs(4)) )
          CAll ShowContinueError('Zone name not found. Transformer heat losses will not be added to a zone' )
          !continue with simulation but storage losses not sent to a zone.
        ENDIF
      ENDIF

      Transformer(TransfNum)%ZoneRadFrac           = rNumericArgs(1)
      Transformer(TransfNum)%RatedCapacity         = rNumericArgs(2)
      Transformer(TransfNum)%Phase                 = rNumericArgs(3)

      IF (SameString(cAlphaArgs(5) ,    'Copper') )    THEN
        Transformer(TransfNum)%FactorTempCoeff     = 234.5d0
      ELSEIF (SameString(cAlphaArgs(5), 'Aluminum' ) )   THEN
        Transformer(TransfNum)%FactorTempCoeff     = 225.0d0
      ELSE
        CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5)) )
        ErrorsFound=.true.
      ENDIF

      Transformer(TransfNum)%TempRise              = rNumericArgs(4)
      Transformer(TransfNum)%EddyFrac              = rNumericArgs(5)

      IF (SameString(cAlphaArgs(6) ,    'RatedLosses') )    THEN
        Transformer(TransfNum)%PerformanceInputMode     = LossesMethod
      ELSEIF (SameString(cAlphaArgs(6), 'NominalEfficiency' ) )   THEN
        Transformer(TransfNum)%PerformanceInputMode     = EfficiencyMethod
      ELSE
        CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6)) )
        ErrorsFound=.true.
      ENDIF

      Transformer(TransfNum)%RatedNL           = rNumericArgs(6)
      Transformer(TransfNum)%RatedLL           = rNumericArgs(7)
      Transformer(TransfNum)%RatedEfficiency   = rNumericArgs(8)
      Transformer(TransfNum)%RatedPUL          = rNumericArgs(9)
      Transformer(TransfNum)%RatedTemp         = rNumericArgs(10)
      Transformer(TransfNum)%MaxPUL            = rNumericArgs(11)

      !Check the input for MaxPUL if the performance input method is EfficiencyMethod
      !Other inputs do not need to be checked because they are handled by the IDD procedure.
      IF (Transformer(TransfNum)%PerformanceInputMode == EfficiencyMethod) THEN
        IF (lNumericFieldBlanks(11)) THEN
          Transformer(TransfNum)%MaxPUL = Transformer(TransfNum)%RatedPUL
        ELSEIF(Transformer(TransfNum)%MaxPUL <= 0 .OR. Transformer(TransfNum)%MaxPUL > 1) THEN
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(11))//'=['//trim(RoundSigDigits(rNumericArgs(11),3))//'].')
          CALL ShowContinueError('Entered value must be > 0 and <= 1.')
          ErrorsFound=.true.
        ENDIF
      ENDIF

      IF (SameString(cAlphaArgs(7) ,    'Yes') )    THEN
        Transformer(TransfNum)%ConsiderLosses     = .TRUE.
      ELSEIF (SameString(cAlphaArgs(7), 'No' ) )   THEN
        Transformer(TransfNum)%ConsiderLosses     = .FALSE.
      ELSE
        IF(Transformer(TransfNum)%UsageMode == PowerInFromGrid) THEN
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(cAlphaArgs(7)) )
          ErrorsFound=.true.
        ENDIF
      ENDIF

      NumWiredMeters = NumAlphas - NumAlphaBeforeMeter

      IF(Transformer(TransfNum)%UsageMode == PowerInFromGrid) THEN

        !Provide warning if no meter is wired to a transformer used to get power from the grid
        IF(NumWiredMeters <= 0) THEN
          CALL ShowWarningError(RoutineName//'ElectricLoadCenter:Transformer="'//TRIM(Transformer(TransfNum)%Name)//'":')
          CALL ShowContinueError('ISOLATED Transformer: No meter wired to a transformer used to input power from grid' )
        END IF

        ALLOCATE(Transformer(TransfNum)%WiredMeterNames(NumWiredMeters))
        ALLOCATE(Transformer(TransfNum)%WiredMeterPtrs(NumWiredMeters))
        ALLOCATE(Transformer(TransfNum)%SpecialMeter(NumWiredMeters))

        !Meter check deferred because they may have not been "loaded" yet,
        DO LoopCount = 1, NumWiredMeters
          Transformer(TransfNum)%WiredMeterNames(LoopCount) = MakeUPPERCase(cAlphaArgs(LoopCount+NumAlphaBeforeMeter))
          !Assign SpecialMeter as TRUE if the meter name is Electricity:Facility or Electricity:HVAC
          IF(SameString(Transformer(TransfNum)%WiredMeterNames(LoopCount), 'Electricity:Facility') .OR.  &
             SameString(Transformer(TransfNum)%WiredMeterNames(LoopCount), 'Electricity:HVAC') ) THEN
            Transformer(TransfNum)%SpecialMeter(LoopCount) = .TRUE.
          ELSE
            Transformer(TransfNum)%SpecialMeter(LoopCount) = .FALSE.
          ENDIF
        END DO
      ENDIF


      CALL SetupOutputVariable('Transformer Efficiency []', &
           Transformer(TransfNum)%Efficiency, 'System', 'Average', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer Input Electric Power [W]', &
           Transformer(TransfNum)%PowerIn, 'System', 'Average', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer Input Electric Energy [J]', &
           Transformer(TransfNum)%EnergyIn, 'System', 'Sum', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer Output Electric Power [W]', &
           Transformer(TransfNum)%PowerOut, 'System', 'Average', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer Output Electric Energy [J]', &
           Transformer(TransfNum)%EnergyOut, 'System', 'Sum', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer No Load Loss Rate [W]', &
           Transformer(TransfNum)%NoLoadLossRate, 'System', 'Average', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer No Load Loss Energy [J]', &
           Transformer(TransfNum)%NoLoadLossEnergy, 'System', 'Sum', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer Load Loss Rate [W]', &
           Transformer(TransfNum)%LoadLossRate, 'System', 'Average', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer Load Loss Energy [J]', &
           Transformer(TransfNum)%LoadLossEnergy, 'System', 'Sum', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer Thermal Loss Rate [W]', &
           Transformer(TransfNum)%ThermalLossRate, 'System', 'Average', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer Thermal Loss Energy [J]', &
           Transformer(TransfNum)%ThermalLossEnergy, 'System', 'Sum', Transformer(TransfNum)%Name )
      CALL SetupOutputVariable('Transformer Distribution Electric Loss Energy [J]', &
           Transformer(TransfNum)%ElecUseUtility, 'System', 'Sum', Transformer(TransfNum)%Name, &
           ResourceTypeKey='Electricity', GroupKey='System')
      CALL SetupOutputVariable('Transformer Cogeneration Electric Loss Energy [J]', &
           Transformer(TransfNum)%ElecProducedCoGen, 'System', 'Sum', Transformer(TransfNum)%Name ,&
           ResourceTypeKey='ElectricityProduced',EndUseKey='COGENERATION',GroupKey='System')

      IF (Transformer(TransfNum)%ZoneNum > 0) THEN
        CALL SetupZoneInternalGain(Transformer(TransfNum)%ZoneNum, &
                     'ElectricLoadCenter:Transformer',  &
                     Transformer(TransfNum)%Name , &
                     IntGainTypeOf_ElectricLoadCenterTransformer,         &
                     ConvectionGainRate    =    Transformer(TransfNum)%QdotconvZone, &
                     ThermalRadiationGainRate = Transformer(TransfNum)%QdotRadZone)
      ENDIF


    ENDDO ! End loop for get transformer inputs
  ENDIF


  !Get the number of electric load centers  (now allowing more than 1 per simulation)
  NumLoadCenters = GetNumObjectsFound('ElectricLoadCenter:Distribution')

  IF (NumLoadCenters > 0 )THEN
    IF(.NOT. ALLOCATED(ElecLoadCenter) ) &
             ALLOCATE(ElecLoadCenter(NumLoadCenters))
    DO LoopCount = 1, NumTransformers
      IF(.NOT. ALLOCATED(Transformer(LoopCount)%LoadCenterIndexes) ) THEN
        ALLOCATE(Transformer(LoopCount)%LoadCenterIndexes(NumLoadCenters))
      ENDIF
    END DO
  ELSE
    ! set up one load center anyway for consistent access to report variables.
    IF(.NOT. ALLOCATED(ElecLoadCenter) ) &
             ALLOCATE (ElecLoadCenter(1))
  ENDIF

!First get the number of electric load center generator and make a list of names
  cCurrentModuleObject = 'ElectricLoadCenter:Generators'
  NumGenLists = GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(ListName(NumGenLists))
  DO Count = 1, NumGenLists
    CALL GetObjectItem(cCurrentModuleObject,Count,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ListName,Count-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
    ENDIF
    ListName(Count) =TRIM(cAlphaArgs(1))
  END DO


  DO Count = 1, NumLoadCenters

    !Get the data for electric load centers
    cCurrentModuleObject = 'ElectricLoadCenter:Distribution'
    CALL GetObjectItem(cCurrentModuleObject,Count,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ElecLoadCenter%Name,Count-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
!Load the Power Center Name.
    ElecLoadCenter(Count)%Name                = cAlphaArgs(1)

!Load the Power Center Generator List .
    ElecLoadCenter(Count)%GeneratorList       = cAlphaArgs(2)

!Load the Power Center Operation Scheme
    IF (SameString(cAlphaArgs(3) ,    'Baseload') )         THEN
      ElecLoadCenter(Count)%OperationScheme     = iOpSchemeBaseload
    ELSEIF (SameString(cAlphaArgs(3), 'DemandLimit' ) )     THEN
      ElecLoadCenter(Count)%OperationScheme     = iOpSchemeDemandLimit
    ELSEIF (SameString(cAlphaArgs(3), 'TrackElectrical' ) ) THEN
      ElecLoadCenter(Count)%OperationScheme     = iOpSchemeTrackElectrical
    ELSEIF (SameString(cAlphaArgs(3), 'TrackSchedule' ) )   THEN
      ElecLoadCenter(Count)%OperationScheme     = iOpSchemeTrackSchedule
    ELSEIF (SameString(cAlphaArgs(3), 'TrackMeter' ) )      THEN
      ElecLoadCenter(Count)%OperationScheme     = iOpSchemeTrackMeter
    ELSEIF (SameString(cAlphaArgs(3), 'FollowThermal' ) )   THEN
      ElecLoadCenter(Count)%OperationScheme     = iOpSchemeThermalFollow
    ELSEIF (SameString(cAlphaArgs(3), 'FollowThermalLimitElectrical' ) ) THEN
      ElecLoadCenter(Count)%OperationScheme     = iOpSchemeThermalFollowLimitElectrical
    ELSE
      CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
      ErrorsFound=.true.
    ENDIF

    !Load the Purchaed Electric Demand Limit for the Demand Limit scheme only. Is not used for other schemes.
    ElecLoadCenter(Count)%DemandLimit = rNumericArgs(1)

    ElecLoadCenter(Count)%TrackSchedPtr = GetScheduleIndex(cAlphaArgs(4))
    ! test if schedule valid and 'TRACK SCHEDULE'
    IF ((ElecLoadCenter(Count)%TrackSchedPtr == 0 ) .and.  &
       (ElecLoadCenter(Count)%OperationScheme == iOpSchemeTrackSchedule) ) THEN ! throw error
       IF (.not. lAlphaFieldBlanks(4)) THEN
         CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
         CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(cAlphaArgs(4)) )
       ELSE
         CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
         CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//' = blank field.')
       ENDIF
       CAll ShowContinueError('Schedule not found; Must be entered and valid when Operation Scheme=TrackSchedule')
       errorsFound = .TRUE.
    ENDIF

    ElecLoadCenter(Count)%DemandMeterName = MakeUPPERCase(cAlphaArgs(5))

    ! meters may not be "loaded" yet, defered check to later subroutine
    IF (SameString(cAlphaArgs(6) , 'AlternatingCurrent')) THEN
       ElecLoadCenter(Count)%BussType = ACBuss
       cAlphaArgs(6)='AlternatingCurrent'
    ELSEIF (SameString(cAlphaArgs(6) , 'DirectCurrentWithInverter')) THEN
       ElecLoadCenter(Count)%BussType = DCBussInverter
       ElecLoadCenter(Count)%InverterPresent = .TRUE.
       cAlphaArgs(6)='DirectCurrentWithInverter'
    ELSEIF (SameString(cAlphaArgs(6) , 'AlternatingCurrentWithStorage') ) THEN
       ElecLoadCenter(Count)%BussType = ACBussStorage
       ElecLoadCenter(Count)%StoragePresent  = .TRUE.
       cAlphaArgs(6)='AlternatingCurrentWithStorage'
    ELSEIF (SameString(cAlphaArgs(6) , 'DirectCurrentWithInverterDCStorage') ) THEN
       ElecLoadCenter(Count)%BussType = DCBussInverterDCStorage
       ElecLoadCenter(Count)%InverterPresent = .TRUE.
       ElecLoadCenter(Count)%StoragePresent  = .TRUE.
       cAlphaArgs(6)='DirectCurrentWithInverterDCStorage'
    ELSEIF (SameString(cAlphaArgs(6) , 'DirectCurrentWithInverterACStorage') ) THEN
       ElecLoadCenter(Count)%BussType = DCBussInverterACStorage
       ElecLoadCenter(Count)%InverterPresent = .TRUE.
       ElecLoadCenter(Count)%StoragePresent  = .TRUE.
       cAlphaArgs(6)='DirectCurrentWithInverterACStorage'
    ELSEIF (SameString(cAlphaArgs(6), Blank )) Then
       ElecLoadCenter(Count)%BussType = ACBuss
       cAlphaArgs(6)='AlternatingCurrent (field was blank)'
    ELSE
       CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
       CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6)) )
       errorsFound = .TRUE.
    ENDIF

    If (ElecLoadCenter(Count)%InverterPresent) Then
      ElecLoadCenter(Count)%InverterModelNum = FindItemInList(cAlphaArgs(7), InverterNames, NumInverters)
      If (ElecLoadCenter(Count)%InverterModelNum <= 0) Then
        IF (.not. lAlphaFieldBlanks(7)) THEN
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(cAlphaArgs(7)) )
        ELSE
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//' = blank field.')
        ENDIF
        CAll ShowContinueError('Inverter object was not found; Must have and be valid when Buss Type="'//  &
           trim(cAlphaArgs(6))//'".')
        errorsFound = .TRUE.
      ELSE
        ! check if previous elec load center already uses this inverter.
        IF (Count-1  > 0) THEN
          Found=FindItemInList(cAlphaArgs(7),ElecLoadCenter%InverterName,Count-1)
          IF (Found /= 0) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(cAlphaArgs(7)) )
            CALL ShowContinueError('Inverter object has already been used by another '//TRIM(cCurrentModuleObject))
            errorsFound = .TRUE.
          ENDIF
        ENDIF
      ENDIF
      ElecLoadCenter(Count)%InverterName = Trim(cAlphaArgs(7))
    ENDIF

    IF (ElecLoadCenter(Count)%StoragePresent) THEN
      ElecLoadCenter(Count)%StorageModelNum = FindItemInList(cAlphaArgs(8), StorageNames, NumElecStorageDevices)
      IF (ElecLoadCenter(Count)%StorageModelNum <= 0) THEN
        IF (.not. lAlphaFieldBlanks(8)) THEN
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(8))//' = '//TRIM(cAlphaArgs(8)) )
        ELSE
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(8))//' = blank field.')
        ENDIF
        CALL ShowContinueError('Electrical storage object was not found; Must have and be valid when Buss Type="'//  &
           trim(cAlphaArgs(6))//'".')
        errorsFound = .TRUE.
      ELSE
        ! check if previous elec load center already uses this storage.
        IF (Count-1  > 0) THEN
          Found=FindItemInList(cAlphaArgs(8),ElecLoadCenter%StorageName,Count-1)
          IF (Found /= 0) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(8))//' = '//TRIM(cAlphaArgs(8)) )
            CALL ShowContinueError('Storage object has already been used by another '//TRIM(cCurrentModuleObject))
            errorsFound = .TRUE.
          ENDIF
        ENDIF
      ENDIF
      ElecLoadCenter(Count)%StorageName = Trim(cAlphaArgs(8))
    ENDIF

!    If a transformer is used in an electric load center, the program needs to 1) update the number of
!    electric load centers connected to that transformer; 2) bookkeep the load center index in the transformer
!    data structure so that the transformer knows which load center is connected.
    IF(NumAlphas >= 9 .AND. (.not. lAlphaFieldBlanks(9)) ) THEN
      ElecLoadCenter(Count)%TransformerModelNum = FindItemInList(cAlphaArgs(9), TransformerNames, NumTransformers)
        IF (ElecLoadCenter(Count)%TransformerModelNum <= 0) THEN
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(9))//' = '//TRIM(cAlphaArgs(9)) )
          ErrorsFound = .TRUE.
        ELSE
        ! It is allowed that a transformer can serve multiple load centers.
        ! This differs from inverters and batteries (electrical storage) implemented previously
          ElecLoadCenter(Count)%TransformerName = TRIM(cAlphaArgs(9))
          LCofTransformer = Transformer(ElecLoadCenter(Count)%TransformerModelNum)%LoadCenterNum + 1
          Transformer(ElecLoadCenter(Count)%TransformerModelNum)%LoadCenterNum = LCofTransformer
          Transformer(ElecLoadCenter(Count)%TransformerModelNum)%LoadCenterIndexes(LCofTransformer) = Count
        ENDIF
    ENDIF


    !Setup general output variables for reporting in the electric load center
    SetupWholeBldgReports = .TRUE.

    CALL SetupOutputVariable('Electric Load Center Produced Electric Power [W]', &
         ElecLoadCenter(Count)%ElectProdRate,'System','Average',ElecLoadCenter(Count)%Name)

    CALL SetupOutputVariable('Electric Load Center Produced Electric Energy [J]', &
           ElecLoadCenter(Count)%ElectricityProd,'System','Sum',ElecLoadCenter(Count)%Name)

    CALL SetupOutputVariable('Electric Load Center Produced Thermal Rate [W]', &
         ElecLoadCenter(Count)%ThermalProdRate,'System','Average',ElecLoadCenter(Count)%Name)

    CALL SetupOutputVariable('Electric Load Center Produced Thermal Energy [J]', &
           ElecLoadCenter(Count)%ThermalProd,'System','Sum',ElecLoadCenter(Count)%Name)

    If(Trim(ElecLoadCenter(Count)%GeneratorList) .ne. '')Then
      ListNum=FindItemInList(ElecLoadCenter(Count)%GeneratorList,ListName,NumGenLists)
      IF (ListNum == 0) THEN

        CALL ShowSevereError('Requested Generator List='//TRIM(ElecLoadCenter(Count)%GeneratorList)// &
                             ', not found.  Load Center='//TRIM(ElecLoadCenter(Count)%Name))
        ErrorsFound=.true.
        CYCLE
      ENDIF

      cCurrentModuleObject = 'ElectricLoadCenter:Generators'
      CALL GetObjectItem(cCurrentModuleObject,ListNum,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  !Calculate the number of generators in list
      NumGenerators = NumNums / 2 ! note IDD needs Min Fields = 6  can this be more robust?
      IF (MOD((NumAlphas-1+NumNums),5) /= 0) NumGenerators=NumGenerators+1
      alphacount =2
  !Allocate the pointer array
      ALLOCATE(ElecLoadCenter(Count)%ElecGen(NumGenerators))
      ElecLoadCenter(Count)%NumGenerators = NumGenerators
      pvTotalCapacity = 0.0d0    ! for LEED report
      windTotalCapacity = 0.0d0   ! for LEED report
      DO GenCount = 1, ElecLoadCenter(Count)%NumGenerators
  !Load the Power Center Generator List Name
        ElecLoadCenter(Count)%ElecGen(GenCount)%Name            = cAlphaArgs(alphacount)
        alphacount =alphacount+1
  !Load the Type of Generator
        ElecLoadCenter(Count)%ElecGen(GenCount)%TypeOf          = cAlphaArgs(alphacount)
        IF (SameString( cAlphaArgs(alphacount) , 'Generator:InternalCombustionEngine') ) THEN
          ElecLoadCenter(Count)%ElecGen(GenCount)%CompType_Num  = iGeneratorICEngine
        ELSEIF (SameString(cAlphaArgs(alphacount) , 'Generator:CombustionTurbine') ) THEN
          ElecLoadCenter(Count)%ElecGen(GenCount)%CompType_Num  = iGeneratorCombTurbine
        ELSEIF (SameString(cAlphaArgs(alphacount) , 'Generator:MicroTurbine') )  THEN
          ElecLoadCenter(Count)%ElecGen(GenCount)%CompType_Num  = iGeneratorMicroturbine
        ELSEIF (SameString(cAlphaArgs(alphacount) , 'Generator:Photovoltaic') ) THEN
          ElecLoadCenter(Count)%ElecGen(GenCount)%CompType_Num  = iGeneratorPV
        ELSEIF (SameString(cAlphaArgs(alphacount) , 'Generator:FuelCell') ) THEN
          ElecLoadCenter(Count)%ElecGen(GenCount)%CompType_Num  = iGeneratorFuelCell
        ELSEIF (SameString(cAlphaArgs(alphacount) , 'Generator:MicroCHP') ) THEN
          ElecLoadCenter(Count)%ElecGen(GenCount)%CompType_Num  = iGeneratorMicroCHP
        ELSEIF (SameString(cAlphaArgs(alphacount) , 'Generator:WindTurbine') ) THEN
          ElecLoadCenter(Count)%ElecGen(GenCount)%CompType_Num  = iGeneratorWindTurbine
        ELSE
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(alphacount))//' = '//TRIM(cAlphaArgs(alphacount)) )
          ErrorsFound=.true.
        ENDIF
        CALL ValidateComponent(ElecLoadCenter(Count)%ElecGen(GenCount)%TypeOf,ElecLoadCenter(Count)%ElecGen(GenCount)%Name,  &
                               IsNotOK,'Generator')
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        ENDIF

        alphacount =alphacount+1

        ElecLoadCenter(Count)%ElecGen(GenCount)%MaxPowerOut     = rNumericArgs(2*GenCount-1)
        ElecLoadCenter(Count)%ElecGen(GenCount)%NominalThermElectRatio = rNumericArgs(2*GenCount)

        IF (AnyEnergyManagementSystemInModel) Then
          CALL SetupEMSInternalVariable('Generator Nominal Maximum Power', ElecLoadCenter(Count)%ElecGen(GenCount)%Name , &
                                     '[W]',  ElecLoadCenter(Count)%ElecGen(GenCount)%MaxPowerOut )
          CALL SetupEMSInternalVariable('Generator Nominal Thermal To Electric Ratio',   &
             ElecLoadCenter(Count)%ElecGen(GenCount)%Name , &
                                     '[ratio]',  ElecLoadCenter(Count)%ElecGen(GenCount)%NominalThermElectRatio )
        ENDIF

  !Load the Power CenterElectric Generation Meter Name
        ElecLoadCenter(Count)%ElecGen(GenCount)%AvailSched      = cAlphaArgs(alphacount)
        IF (lAlphaFieldBlanks(alphacount)) THEN
          ElecLoadCenter(Count)%ElecGen(GenCount)%AvailSchedPtr   = ScheduleAlwaysOn
        ELSE
          ElecLoadCenter(Count)%ElecGen(GenCount)%AvailSchedPtr   = GetScheduleIndex(cAlphaArgs(alphacount))
          IF (ElecLoadCenter(Count)%ElecGen(GenCount)%AvailSchedPtr <= 0) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(alphacount))//' = '//TRIM(cAlphaArgs(alphacount)) )
            CAll ShowContinueError('Schedule was not found ')
            errorsFound = .true.
          ENDIF
        ENDIF
        alphacount =alphacount+1


        CALL SetupOutputVariable('Generator Requested Electric Power [W]', &
               ElecLoadCenter(Count)%ElecGen(GenCount)%PowerRequestThisTimestep, &
                'System','Average',ElecLoadCenter(Count)%ElecGen(GenCount)%Name)
        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('On-Site Generator Control', ElecLoadCenter(Count)%ElecGen(GenCount)%Name, &
                                'Requested Power', '[W]',    &
                                ElecLoadCenter(Count)%ElecGen(GenCount)%EMSRequestOn  , &
                                ElecLoadCenter(Count)%ElecGen(GenCount)%EMSPowerRequest )


        ENDIF


      ENDDO !End of the NumGenerators Loop


    End If

    CALL SetupOutputVariable('Electric Load Center Requested Electric Power [W]', &
                  ElecLoadCenter(Count)%TotalPowerRequest,'System','Average', &
                  ElecLoadCenter(Count)%Name)

  ENDDO  ! loop over number of load centers

  ! LEED report
  pvTotalCapacity = 0.0d0
  windTotalCapacity = 0.0d0
  DO Count = 1, NumLoadCenters
    IF(Trim(ElecLoadCenter(Count)%GeneratorList) .NE. '')Then
      DO GenCount = 1, ElecLoadCenter(Count)%NumGenerators
        IF (ElecLoadCenter(Count)%ElecGen(GenCount)%CompType_Num  .EQ. iGeneratorPV) THEN
          pvTotalCapacity = pvTotalCapacity + ElecLoadCenter(Count)%ElecGen(GenCount)%MaxPowerOut
        ENDIF
        IF (ElecLoadCenter(Count)%ElecGen(GenCount)%CompType_Num  .EQ. iGeneratorWindTurbine) THEN
          windTotalCapacity = windTotalCapacity + ElecLoadCenter(Count)%ElecGen(GenCount)%MaxPowerOut
        ENDIF
      END DO
    END IF
  END DO
  !put in total capacity for PV and Wind for LEED report
  CALL PreDefTableEntry(pdchLeedRenRatCap,'Photovoltaic',pvTotalCapacity/1000,2)
  CALL PreDefTableEntry(pdchLeedRenRatCap,'Wind',windTotalCapacity/1000,2)


  IF (NumLoadCenters == 0) THEN
  ! if user input did not include an Electric Load center, create a simple default one here for reporting purposes
  !   but only if there are any other electricity components set up (yet) for metering
    AnyElectricityPresent = GetMeterIndex('ELECTRICITY:FACILITY')

    If (AnyElectricityPresent > 0) Then

      NumLoadCenters = 1
      ElecLoadCenter(1)%Name  = 'Electrical Service'
      ElecLoadCenter(1)%OperationScheme = iOpSchemeTrackElectrical

      SetupWholeBldgReports = .TRUE.

    ENDIF ! any electricity metered
  ENDIF ! no user electric load center

  If (SetupWholeBldgReports) then

    CALL SetupOutputVariable('Facility Total Purchased Electric Power [W]', &
         WholeBldgElectSummary%ElectPurchRate,'System','Average',WholeBldgElectSummary%Name)
    CALL SetupOutputVariable('Facility Total Purchased Electric Energy [J]', &
         WholeBldgElectSummary%ElectricityPurch,'System','Sum',WholeBldgElectSummary%Name, &
                           ResourceTypeKey='ElectricityPurchased',EndUseKey='COGENERATION',GroupKey='Plant')

    CALL SetupOutputVariable('Facility Total Surplus Electric Energy [J]', &
         WholeBldgElectSummary%ElectricitySurplus,'System','Sum',WholeBldgElectSummary%Name , &
                           ResourceTypeKey='ElectricitySurplusSold',EndUseKey='COGENERATION',GroupKey='Plant')

    CALL SetupOutputVariable('Facility Net Purchased Electric Power [W]', &
         WholeBldgElectSummary%ElectricityNetRate,'System','Average',WholeBldgElectSummary%Name)
    CALL SetupOutputVariable('Facility Net Purchased Electric Energy [J]', &
         WholeBldgElectSummary%ElectricityNet,'System','Sum',WholeBldgElectSummary%Name, &
                           ResourceTypeKey='ElectricityNet',EndUseKey='COGENERATION',GroupKey='Plant')

    CALL SetupOutputVariable('Facility Total Building Electric Demand Power [W]', &
         WholeBldgElectSummary%TotalBldgElecDemand,'System','Average',WholeBldgElectSummary%Name)
    CALL SetupOutputVariable('Facility Total HVAC Electric Demand Power [W]', &
         WholeBldgElectSummary%TotalHVACElecDemand,'System','Average',WholeBldgElectSummary%Name)
    CALL SetupOutputVariable('Facility Total Electric Demand Power [W]', &
         WholeBldgElectSummary%TotalElectricDemand,'System','Average',WholeBldgElectSummary%Name)

    CALL SetupOutputVariable('Facility Total Produced Electric Power [W]', &
         WholeBldgElectSummary%ElectProdRate,'System','Average',WholeBldgElectSummary%Name)
    CALL SetupOutputVariable('Facility Total Produced Electric Energy [J]', &
         WholeBldgElectSummary%ElectricityProd,'System','Sum',WholeBldgElectSummary%Name)
  ENDIF


  !Check whether a transformer connects to a load center if it is used to output power to the grid
  !Issue warning if no load center is connected to that transformer
  !This has to be done after reading in all load centers
  DO TransfNum = 1, NumTransformers
    IF(Transformer(TransfNum)%UsageMode == PowerOutFromBldg .AND. Transformer(TransfNum)%LoadCenterNum == 0) THEN
      CALL ShowSevereError(RoutineName//'ElectricLoadCenter:Transformer="'//TRIM(Transformer(TransfNum)%Name)//'", invalid entry.')
      CALL ShowContinueError('ISOLATED Transformer: No load center connects to a transformer used to output power' )
    END IF
  END DO


  DEALLOCATE(ListName)


  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Preceding errors terminate program.')
  ENDIF

RETURN

END SUBROUTINE GetPowerManagerInput


SUBROUTINE GeneratorPowerOutput(LoadCenterNum,GenNum,FirstHVACIteration,ElectricPowerOutput,ThermalPowerOutput)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Nigusse
          !       DATE WRITTEN   December 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Simulates generator to get the actual electric power output based on load request

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ICEngineElectricGenerator,     ONLY: SimICEngineGenerator, GetICEGeneratorResults
  USE CTElectricGenerator,           ONLY: SimCTGenerator, GetCTGeneratorResults
  USE MicroturbineElectricGenerator, ONLY: SimMTGenerator, GetMTGeneratorResults
  USE Photovoltaics,                 ONLY: SimPVGenerator, GetPVGeneratorResults
  USE FuelCellElectricGenerator,     ONLY: SimFuelCellGenerator, GetFuelCellGeneratorResults
  USE MicroCHPElectricGenerator,     ONLY: SimMicroCHPGenerator, GetMicroCHPGeneratorResults
  USE WindTurbine,                   ONLY: SimWindTurbine, GetWTGeneratorResults
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: GenNum           ! Generator number counter
  INTEGER, INTENT(IN) :: LoadCenterNum    ! Load Center number counter
  LOGICAL, INTENT(IN) :: FirstHVACIteration ! Unused 2010 JANUARY
  REAL(r64), INTENT(OUT) :: ElectricPowerOutput ! Actual generator electric power output
  REAL(r64), INTENT(OUT) :: ThermalPowerOutput  ! Actual generator thermal power output

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: GeneratorName    ! User-specified name of generator
  INTEGER                       :: GeneratorType    ! Type of generator
  LOGICAL                       :: RunFlag          ! Simulate generator when TRUE
  REAL(r64)                     :: MyLoad           ! Generator load request (W)


  GeneratorType = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%CompType_Num
  GeneratorName = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%Name
  Runflag = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ONThisTimeStep
  Myload = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%PowerRequestThisTimestep

      ! Select and call models and also collect results for load center power conditioning and reporting
      TypeOfEquip: SELECT CASE (GeneratorType)

        CASE (iGeneratorICEngine)    ! 'Generator:InternalCombustionEngine'
          CALL SimICEngineGenerator(GeneratorType,GeneratorName,ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex,  &
                                          Runflag,MyLoad,FirstHVACIteration)
          CALL GetICEGeneratorResults(GeneratorType, ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate,  &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectricityProd, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProd)
          ElectricPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate
          ThermalPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate

        CASE (iGeneratorCombTurbine) ! 'Generator:CombustionTurbine'
          CALL SimCTGenerator(GeneratorType,GeneratorName,ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex,  &
                                          Runflag,MyLoad,FirstHVACIteration)
          CALL GetCTGeneratorResults(GeneratorType, ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate,  &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectricityProd, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProd)
          ElectricPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate
          ThermalPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate

        CASE (iGeneratorMicroturbine)! 'Generator:MicroTurbine'
          CALL SimMTGenerator(GeneratorType,GeneratorName,ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                          Runflag,MyLoad,FirstHVACIteration)
          CALL GetMTGeneratorResults(GeneratorType, ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectricityProd, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProd)
          ElectricPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate
          ThermalPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate

        CASE (iGeneratorPV)          ! 'Generator:Photovoltaic'
          CALL SimPVGenerator(GeneratorType,GeneratorName,ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                          Runflag,MyLoad)
          CALL GetPVGeneratorResults(GeneratorType,ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%DCElectProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%DCElectricityProd, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProd)
          ElectricPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%DCElectProdRate
          ThermalPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate

        CASE (iGeneratorFuelCell)    ! 'Generator:FuelCell'
          CALL SimFuelCellGenerator(GeneratorType,GeneratorName,ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                          Runflag,MyLoad,FirstHVACIteration)
          CALL GetFuelCellGeneratorResults(GeneratorType, ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectricityProd, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProd)
          ElectricPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate
          ThermalPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate

        CASE(iGeneratorMicroCHP)     ! 'Generator:MicroCHP'
           CALL SimMicroCHPGenerator(GeneratorType,GeneratorName,ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                          Runflag, .false., MyLoad, constant_zero, FirstHVACIteration)
           CALL GetMicroCHPGeneratorResults(GeneratorType, ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectricityProd, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProd)
          ElectricPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate
          ThermalPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate

        CASE (iGeneratorWindTurbine) ! 'Generator:WindTurbine'
          CALL SimWindTurbine(GeneratorType,GeneratorName,ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                          Runflag,MyLoad)
          CALL GetWTGeneratorResults(GeneratorType,ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%GeneratorIndex, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectricityProd, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate, &
                                     ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProd)
          ElectricPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ElectProdRate
          ThermalPowerOutput = ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%ThermalProdRate

        CASE DEFAULT
          CALL ShowSevereError('ManageElectricPower: Invalid Generator Type found= '//  &
                                TRIM(ElecLoadCenter(LoadCenterNum)%ElecGen(GenNum)%TypeOf))
          CALL ShowContinueError('.. Generator Name='//TRIM(GeneratorName))
          CALL ShowFatalError('.. preceding error causes termination.')

      END SELECT TypeOfEquip

RETURN
END SUBROUTINE GeneratorPowerOutput


SUBROUTINE CalcLoadCenterThermalLoad(FirstHVACIteration, LoadCenterNum, ThermalLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:


          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant
  USE DataHVACGlobals, ONLY: NumPlantLoops, SysTimeElapsed
  USE InputProcessor, ONLY: SameString

  USE DataGlobals,     ONLY: dooutputreporting, MetersHaveBeenInitialized, warmupflag, &
                             doingsizing, currenttime

 USE General, ONLY: TrimSigDigits
 USE DataEnvironment, ONLY: Month, DayOfMonth

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)   :: FirstHVACIteration !unused1208
  INTEGER, INTENT (IN)   :: LoadCenterNum    ! Load Center number counter
  REAL(r64),    INTENT (OUT)  :: ThermalLoad ! heat rate called for from cogenerator(watts)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE  :: MyOneTimeSetupFlag = .true.
  LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:) :: MyCoGenSetupFlag
  INTEGER :: FoundCount
  INTEGER :: i
  INTEGER :: j
  INTEGER :: k
  INTEGER :: m
  INTEGER :: LoopID
  INTEGER :: SideID
  INTEGER :: BranchID
  INTEGER :: CompID
  CHARACTER(len=MaxNameLength) :: thisName
!unused  INTEGER :: ThisTypeNum

  !debugstuff
!unused    REAL(r64) :: ActualTime
!unused    CHARACTER*300 :: OutputString
!unused    CHARACTER*5 :: strFirstHVACIteration


 ! need to do initial setups
 IF (MyOneTimeSetupFlag) THEN
   ALLOCATE(MyCoGenSetupFlag(NumLoadCenters))
   MyCoGenSetupFlag = .true.
   ThermalLoad = 0.0d0
   MyOneTimeSetupFlag = .false.
   return
 ENDIF

 FoundCount = 0
 IF (MyCoGenSetupFlag(LoadCenterNum)) THEN
   If (Allocated(PlantLoop)) then
     ! loop across generators and find match
     Do i = 1, ElecLoadCenter(LoadCenterNum)%NumGenerators
       thisName = ElecLoadCenter(LoadCenterNum)%ElecGen(i)%Name

       Do j = 1, NumPlantLoops

         Do k = 1, plantLoop(j)%LoopSide(SupplySide)%TotalBranches

           Do m = 1, plantLoop(j)%LoopSide(SupplySide)%Branch(k)%TotalComponents
             If (SameString(plantLoop(j)%LoopSide(SupplySide)%Branch(k)%Comp(m)%Name, thisName)) then
               ElecLoadCenter(LoadCenterNum)%ElecGen(i)%PlantLoopNum = j
               ElecLoadCenter(LoadCenterNum)%ElecGen(i)%LoopSideNum = SupplySide
               ElecLoadCenter(LoadCenterNum)%ElecGen(i)%BranchNum = k
               ElecLoadCenter(LoadCenterNum)%ElecGen(i)%CompNum  = m

               MyCoGenSetupFlag(LoadCenterNum) = .false.
               FoundCount = FoundCount +1
               ElecLoadCenter(LoadCenterNum)%ElecGen(i)%PlantInfoFound = .true.
             ENDIF

           ENDDO ! components

         ENDDO ! branches

       ENDDO ! plant loops

     ENDDO ! generators in load center
   ENDIF

 ENDIF

 ! sum up "MyLoad" for all generators on this load center from plant structure
 ThermalLoad = 0.0d0
 Do i = 1, ElecLoadCenter(LoadCenterNum)%NumGenerators
   If (ElecLoadCenter(LoadCenterNum)%ElecGen(i)%PlantInfoFound) then
     LoopID   = ElecLoadCenter(LoadCenterNum)%ElecGen(i)%PlantLoopNum
     SideID   = ElecLoadCenter(LoadCenterNum)%ElecGen(i)%LoopSideNum
     BranchID = ElecLoadCenter(LoadCenterNum)%ElecGen(i)%BranchNum
     CompID   = ElecLoadCenter(LoadCenterNum)%ElecGen(i)%CompNum
     ThermalLoad = ThermalLoad + &
                   PlantLoop(LoopID)%LoopSide(SideID)%Branch(BranchID)%Comp(CompID)%MyLoad

   ENDIF
 ENDDO

 ThermalLoad = ThermalLoad

END SUBROUTINE CalcLoadCenterThermalLoad

SUBROUTINE VerifyCustomMetersElecPowerMgr

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! check user input for custom meters
          !  need special call to use once CUSTOM METERs are "Made"

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE InputProcessor,  ONLY: SameString
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
  integer :: loop !
  INTEGER, external :: GetMeterIndex


   do loop = 1, NumLoadCenters
     ElecLoadCenter(loop)%DemandMeterPtr =  GetMeterIndex(ElecLoadCenter(loop)%DemandMeterName)
     If ((ElecLoadCenter(loop)%DemandMeterPtr == 0 ) .AND.  &
       (ElecLoadCenter(loop)%OperationScheme == iOpSchemeTrackMeter) ) Then ! throw error
        CAll ShowFatalError('Did not find Meter named: '//trim(ElecLoadCenter(loop)%DemandMeterName) &
                   //' in '//trim(ElecLoadCenter(loop)%Name))
     ENDIF
   ENDDO

  RETURN

END SUBROUTINE VerifyCustomMetersElecPowerMgr

SUBROUTINE ManageInverter(LoadCenterNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   June 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! manage inveter models and fill AC variables

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: LoadCenterNum    ! Load Center number counter

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)   :: NormalizedPower
  INTEGER     :: InvertNum
  INTEGER     :: StorNum
  REAL(r64)   :: tmpEffic
  REAL(r64)   :: tempACpower

  !model inverters
  InvertNum = ElecLoadCenter(LoadCenterNum)%InverterModelNum

  If (InvertNum <= 0) Return

    !first figure DC into inverter
  SELECT CASE (ElecLoadCenter(LoadCenterNum)%BussType)

  CASE(DCBussInverter, DCBussInverterACStorage)
    ElecLoadCenter(LoadCenterNum)%DCElectProdRate = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%DCElectProdRate)
    Inverter(InvertNum)%DCPowerIn = ElecLoadCenter(LoadCenterNum)%DCElectProdRate
    Inverter(InvertNum)%DCEnergyIn = Inverter(InvertNum)%DCPowerIn * (TimeStepSys * SecInHour)
  CASE(DCBussInverterDCStorage)
    StorNum = ElecLoadCenter(LoadCenterNum)%StorageModelNum
    IF (StorNum > 0) Then
      ElecLoadCenter(LoadCenterNum)%DCElectProdRate = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%DCElectProdRate)

      Inverter(InvertNum)%DCPowerIn  = ElecLoadCenter(LoadCenterNum)%DCElectProdRate
      Inverter(InvertNum)%DCEnergyIn = Inverter(InvertNum)%DCPowerIn * (TimeStepSys * SecInHour)
    ELSE ! throw error
      CALL ShowFatalError('Electric load center is not set up properly, check electrical storage object for ' &
                                 //TRIM(ElecLoadCenter(LoadCenterNum)%Name))
    ENDIF


  END SELECT

  ! check availability schedule
  IF (GetCurrentScheduleValue(Inverter(InvertNum)%AvailSchedPtr) > 0.0d0) THEN

    ! now calculate Inverter based on model type
    SELECT CASE (Inverter(InvertNum)%ModelType)

    CASE(CECLookUpTableModel) !interpolation model from test data

      ! we don't model voltage, so use nominal voltage
      NormalizedPower = Inverter(InvertNum)%DCPowerIn / Inverter(InvertNum)%RatedPower

      ! get efficiency
      IF (NormalizedPower <= 0.1D0) THEN
        ! extrapolate or fix at 10% value? fix it for now
        tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(1)
      ELSEIF ((NormalizedPower > 0.1D0) .and. (NormalizedPower <0.20D0)) THEN
       tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(1) &
                  + ((NormalizedPower - 0.1D0)/(0.2D0 - 0.1D0)) &
                    * (Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(2) &
                        - Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(1) )
      ELSEIF (NormalizedPower == 0.2D0) THEN
        tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(2)
      ELSEIF ((NormalizedPower > 0.2D0) .and. (NormalizedPower <0.30D0)) THEN
       tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(2) &
                  + ((NormalizedPower - 0.2D0)/(0.3D0 - 0.2D0)) &
                    * (Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(3) &
                        - Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(2) )
      ELSEIF (NormalizedPower == 0.3D0) THEN
        tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(3)
      ELSEIF ((NormalizedPower > 0.3D0) .and. (NormalizedPower <0.50D0)) THEN
       tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(3) &
                  + ((NormalizedPower - 0.3D0)/(0.5D0 - 0.3D0)) &
                    * (Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(4) &
                        - Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(3) )
      ELSEIF (NormalizedPower == 0.5D0) THEN
        tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(4)
      ELSEIF ((NormalizedPower > 0.5D0) .and. (NormalizedPower <0.75D0)) THEN
       tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(4) &
                  + ((NormalizedPower - 0.5D0)/(0.75D0 - 0.5D0)) &
                    * (Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(5) &
                        - Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(4) )
      ELSEIF (NormalizedPower == 0.75D0) THEN
        tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(5)
      ELSEIF ((NormalizedPower > 0.75D0) .and. (NormalizedPower <1.0D0)) THEN
       tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(5) &
                  + ((NormalizedPower - 0.75D0)/(1.0D0 - 0.75D0)) &
                    * (Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(6) &
                        - Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(5) )
      ELSEIF (NormalizedPower >= 1.0D0) THEN
        tmpEffic = Inverter(InvertNum)%LUTable%NomVoltEfficiencyARR(6)
      ENDIF

      Inverter(InvertNum)%Efficiency = Max(tmpEffic, 0.0D0)
      Inverter(InvertNum)%Efficiency = Min(Inverter(InvertNum)%Efficiency, 1.0D0)

      tempACpower = Inverter(InvertNum)%Efficiency * Inverter(InvertNum)%DCPowerIn

    CASE(CurveFuncOfPower)
      NormalizedPower = Inverter(InvertNum)%DCPowerIn / Inverter(InvertNum)%RatedPower

      tmpEffic = CurveValue(Inverter(InvertNum)%CurveNum, NormalizedPower)

      Inverter(InvertNum)%Efficiency = Max(tmpEffic, Inverter(InvertNum)%MinEfficiency)
      Inverter(InvertNum)%Efficiency = Min(Inverter(InvertNum)%Efficiency, Inverter(InvertNum)%MaxEfficiency)

      tempACpower = Inverter(InvertNum)%Efficiency * Inverter(InvertNum)%DCPowerIn
      If (tempACpower < Inverter(InvertNum)%MinPower) Then  ! not enough to produce any AC power.  all lost. also standby mode
        tempACpower = 0.0D0

      ELSEIF (tempACpower > Inverter(InvertNum)%MaxPower) then ! too much DC for inverter to handle, excess is lost
        tempACpower = Inverter(InvertNum)%MaxPower
        Inverter(InvertNum)%AncillACuseRate   = 0.0D0
        Inverter(InvertNum)%AncillACuseEnergy = 0.0D0
      ELSE
        Inverter(InvertNum)%AncillACuseRate   = 0.0D0
        Inverter(InvertNum)%AncillACuseEnergy = 0.0D0
      ENDIF

    CASE(SimpleConstantEff)

      tempACpower = Inverter(InvertNum)%Efficiency* Inverter(InvertNum)%DCPowerIn

    END SELECT

    Inverter(InvertNum)%ACPowerOut = tempACpower
    Inverter(InvertNum)%ACEnergyOut = tempACpower * (TimeStepSys * SecInHour)

    IF (tempACpower == 0.0D0) THEN
      Inverter(InvertNum)%AncillACuseEnergy = Inverter(InvertNum)%StandbyPower * (TimeStepSys * SecInHour)
      Inverter(InvertNum)%AncillACuseRate   = Inverter(InvertNum)%StandbyPower
    ENDIF
  ELSE ! not available per schedule, inverter is dead.
  !  assume thermal shunt for DC in, but no standby electricity
    Inverter(InvertNum)%ACPowerOut        = 0.0D0
    Inverter(InvertNum)%ACEnergyOut       =  0.0D0
    Inverter(InvertNum)%AncillACuseRate   = 0.0D0
    Inverter(InvertNum)%AncillACuseEnergy = 0.0D0

  ENDIF

  !update report variables
  Inverter(InvertNum)%ThermLossRate   = Inverter(InvertNum)%DCPowerIn - Inverter(InvertNum)%ACPowerOut +   &
     Inverter(InvertNum)%StandbyPower
  Inverter(InvertNum)%ThermLossEnergy = Inverter(InvertNum)%ThermLossRate * (TimeStepSys * SecInHour)
  Inverter(InvertNum)%QdotconvZone    = Inverter(InvertNum)%ThermLossRate * ( 1.0D0 - Inverter(InvertNum)%ZoneRadFract)
  Inverter(InvertNum)%QdotRadZone     = Inverter(InvertNum)%ThermLossRate * Inverter(InvertNum)%ZoneRadFract


  RETURN

END SUBROUTINE ManageInverter


SUBROUTINE UpdateLoadCenterRecords(LoadCenterNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          B. Griffith
            !       DATE WRITTEN:    February 2008
            !       MODIFIED
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na


  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: LoadCenterNum    ! Load Center index

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: InvertNum      ! inverter index
  INTEGER     :: StorNum        ! electrical storage index
        ! Flow
  IF (ElecLoadCenter(LoadCenterNum)%NumGenerators <= 0) RETURN

  SELECT CASE (ElecLoadCenter(LoadCenterNum)%BussType)

  CASE (ACBuss)
    ElecLoadCenter(LoadCenterNum)%ElectProdRate   = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%ElectProdRate)
    ElecLoadCenter(LoadCenterNum)%ElectricityProd = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%ElectricityProd)

  CASE (ACBussStorage)
    StorNum = ElecLoadCenter(LoadCenterNum)%StorageModelNum
    If (StorNum > 0) Then
      ElecLoadCenter(LoadCenterNum)%ElectProdRate  = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%ElectProdRate)  &
                                                    + ElecStorage(StorNum)%DrawnPower  - ElecStorage(StorNum)%StoredPower

      ElecLoadCenter(LoadCenterNum)%ElectricityProd = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%ElectricityProd) &
                                                    + ElecStorage(StorNum)%DrawnEnergy  - ElecStorage(StorNum)%StoredEnergy
    ENDIF

  CASE (DCBussInverter, DCBussInverterDCStorage)
    InvertNum = ElecLoadCenter(LoadCenterNum)%InverterModelNum
    IF (InvertNum > 0) THEN
      ElecLoadCenter(LoadCenterNum)%ElectProdRate   = Inverter(InvertNum)%ACPowerOut
      ElecLoadCenter(LoadCenterNum)%ElectricityProd = Inverter(InvertNum)%ACEnergyOut
    ENDIF

  CASE (DCBussInverterACStorage)
    StorNum = ElecLoadCenter(LoadCenterNum)%StorageModelNum
    InvertNum = ElecLoadCenter(LoadCenterNum)%InverterModelNum
    IF ((InvertNum > 0) .AND. (StorNum > 0)) THEN
      ElecLoadCenter(LoadCenterNum)%ElectProdRate   = Inverter(InvertNum)%ACPowerOut &
                                                   + ElecStorage(StorNum)%DrawnPower  - ElecStorage(StorNum)%StoredPower
      ElecLoadCenter(LoadCenterNum)%ElectricityProd = Inverter(InvertNum)%ACEnergyOut &
                                                 + ElecStorage(StorNum)%DrawnEnergy  - ElecStorage(StorNum)%StoredEnergy
    ENDIF
  END SELECT

  ElecLoadCenter(LoadCenterNum)%ThermalProdRate = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%ThermalProdRate)
  ElecLoadCenter(LoadCenterNum)%ThermalProd = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%ThermalProd)


RETURN
END SUBROUTINE UpdateLoadCenterRecords

SUBROUTINE UpdateWholeBuildingRecords

            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Richard Liesen
            !       DATE WRITTEN:    February 2003
            !       MODIFIED       B. Griffith  Mar. 2008 multiple load centers.
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: formerly called UpdateRecords when there was only one load center
            !  now this reporting is for the entire model.

            ! USE STATEMENTS: na

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), external    :: GetInstantMeterValue

  REAL(r64) :: ElecProducedCoGen = 0.0d0
  REAL(r64) :: ElecProducedFacility = 0.0d0
  REAL(r64) :: TotalPurchased = 0.0d0
  REAL(r64) :: TotalSurplus = 0.0d0
  REAL(r64) :: NetPurchased = 0.0d0

        ! Flow


  ElecProducedCoGen=GetInstantMeterValue(ElecProducedCoGenIndex,2)  !whole building
  ElecProducedFacility = ElecProducedCoGen + WholeBldgElectSummary%ElecProducedPVRate*TimeStepSys*SecInHour & !whole building
                                            + WholeBldgElectSummary%ElecProducedWTRate*TimeStepSys*SecInHour

  WholeBldgElectSummary%ElectricityProd = ElecProducedFacility   !whole building
  WholeBldgElectSummary%ElectProdRate = ElecProducedFacility/(TimeStepSys*SecInHour)   !whole building



  !Report the Total Electric Power Purchased [W], If negative then there is extra power to be sold or stored.
  TotalPurchased = WholeBldgElectSummary%TotalElectricDemand - WholeBldgElectSummary%ElectProdRate
  !Check this value against a tolerance to aid in reporting.
  If(ABS(TotalPurchased) .lt. 0.0001d0) TotalPurchased = 0.0d0
  If (TotalPurchased < 0.0d0) totalPurchased = 0.0d0  ! don't want negative purchased...
  WholeBldgElectSummary%ElectPurchRate = TotalPurchased
  !Report the Total Electric Energy Purchased [J]
  WholeBldgElectSummary%ElectricityPurch = WholeBldgElectSummary%ElectPurchRate*TimeStepSys*SecInHour

  !report the total electric surplus....
  TotalSurplus =WholeBldgElectSummary%ElectProdRate -  WholeBldgElectSummary%TotalElectricDemand
  If(ABS(TotalSurplus) .lt. 0.0001d0) TotalSurplus = 0.0d0
  If (TotalSurplus < 0.0d0) TotalSurplus = 0.0d0  ! don't want negative surplus

  WholeBldgElectSummary%ElectSurplusRate = TotalSurplus
  WholeBldgElectSummary%ElectricitySurplus =TotalSurplus*TimeStepSys*SecInHour

  !report the net electricity , + is purchased, - is surplus
  NetPurchased = WholeBldgElectSummary%TotalElectricDemand - WholeBldgElectSummary%ElectProdRate
  WholeBldgElectSummary%ElectricityNetRate = NetPurchased

  WholeBldgElectSummary%ElectricityNet = WholeBldgElectSummary%ElectricityNetRate*TimeStepSys*SecInHour

RETURN
END SUBROUTINE UpdateWholeBuildingRecords

SUBROUTINE FigureInverterZoneGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Couple inverter skin losses to zone heat gains

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: BeginEnvrnFlag
  USE DataHeatBalance, ONLY: ZoneIntGain

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
  INTEGER       :: InvertNum = 0
  INTEGER       :: ZoneNum = 0

  IF (NumInverters == 0) RETURN

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN
    Inverter%QdotconvZone = 0.0D0
    Inverter%QdotRadZone  = 0.0D0
    MyEnvrnFlag = .FALSE.
  ENDIF
  IF( .NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

!  IF(DoingSizing)THEN

!  ENDIF

  RETURN

END SUBROUTINE FigureInverterZoneGains

!***********************************************************************************************************************************

SUBROUTINE ManageElectCenterStorageInteractions(LoadCenterNum,StorageDrawnPower,StorageStoredPower)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June-August 2008
          !       MODIFIED       BG May 2009, added EMS
          !                      BN (FSEC) Feb 2010 (pass out two storage values)
          !                      Y. KyungTae & W. Wang July-August, 2011 Added a battery model
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! manage controls and calculations related to electrical storage in Electric load center

          ! METHODOLOGY EMPLOYED:
          ! started from fuel cell storage routine and modified for general use in load center
          ! needs lots of work

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: ReallocateRealArray
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals, ONLY: TimeStepSys,SysTimeElapsed
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataGlobals    , ONLY: TimeStep, TimeStepZone, SecInHour, BeginEnvrnFlag, WarmupFlag, HourOfDay

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,      INTENT(IN)  :: LoadCenterNum ! load center number, index for structure
  REAL(r64), INTENT(OUT)    :: StorageDrawnPower       ! Electric Power Draw Rate from storage units
  REAL(r64), INTENT(OUT)    :: StorageStoredPower      ! Electric Power Store Rate from storage units

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: tmpPdraw    = 0.0D0    ! power draw from storage, working var
  REAL(r64)    :: tmpPcharge  = 0.0D0    ! power charge to storage, working var
  LOGICAL      :: drawing     = .false.  ! true if drawing power
  LOGICAL      :: charging    = .false.  ! true if charging
  INTEGER      :: ElecStorNum = 0
  REAL(r64)    :: Pgensupply  = 0.0D0
  REAL(r64)    :: Pdemand     = 0.0D0  !
  REAL(r64)    :: PpcuLosses  = 0.0D0  !
  REAL(r64)    :: Pstorage    = 0.0D0
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyWarmupFlag ! flag for init after warmup complete
  REAL(r64)    :: TimeElapsed         ! Fraction of the current hour that has elapsed (h)
  INTEGER         :: BinNum                     = 0
  REAL(r64)       :: Input0                     = 0.0D0
  REAL(r64)    I0     ! initial guess of current
  REAL(r64)    T0     ! initial guess of T(I)
  REAL(r64)    q0     ! initial charge
  REAL(r64)    qmax   ! maximum capacity
  REAL(r64)    qmaxf  ! maximum capacity, a function of current(I)
  REAL(r64)    k      ! change rate from chemically bound charge to available charge
  REAL(r64)    c      ! fraction available charge capacity to total capacity
  REAL(r64)    TotalSOC ! total charge (ThisTimeStepAvailable+ThisTimeStepBound)
  REAL(r64)    Ef     ! effective internal voltage source, V
  REAL(r64)    E0c    ! fully charged internal battery voltage
  REAL(r64)    Volt   ! terminal voltage
  REAL(r64)    Pw     ! power required
  REAL(r64)    E0d    ! fully discharged internal battery voltage
  REAL(r64)    InternalR  ! internal resistance
  REAL(r64)    XF     ! normalized maximum capacity at the given current
  REAL(r64)    X      ! normalized maximum capacity at the given current
  REAL(r64)    Inew   ! converged current
  REAL(r64)    Tnew   ! charge of discharge time, defined by T=qmaxf/I
  REAL(r64)    Imax   ! maximum current
  REAL(r64)    Numpar          ! number of battery in parallel
  REAL(r64)    Numser          ! number of battery in series
  REAL(r64)    Numbattery      ! number of battery (Numpar*Numser)
  REAL(r64)    initialCharge   ! initial charge in Ah
  REAL(r64)    dividend        !
  REAL(r64)    divisor         !
  REAL(r64)    newAvailable    ! new available charge in Ah
  REAL(r64)    newBound        ! new bound charge in Ah
  REAL(r64)    ::error=0.0D0   ! error in iterative process
  REAL(r64)    ::Pactual=0.0D0 ! actual Power output
  REAL(r64)    ::RHS=0.0D0     ! right hand side of a equation
  REAL(r64)    ::I=0.0D0       ! current
  REAL(r64)    ::DeltaSOC1     ! difference of fractional SOC between this time step and last time step
  REAL(r64)    ::DeltaSOC2     ! difference of fractional SOC between last time step and last two time step
  Integer      ::SaveArrayBounds !Maximum size for the arrays used for rainflow counting

  If ( .NOT. (ElecLoadCenter(LoadCenterNum)%StoragePresent)) RETURN

  ElecStorNum = ElecLoadCenter(LoadCenterNum)%StorageModelNum

   !_____________________________________
   ! begin initalizations
  ! perform the one time initializations
  IF (MyOneTimeFlag) THEN
  ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumElecStorageDevices))
    ALLOCATE(MyWarmupFlag(NumElecStorageDevices))
    MyEnvrnFlag = .true.
    MyOneTimeFlag = .false.
    MyWarmupFlag   = .FALSE.
  END IF

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(ElecStorNum)) THEN
    IF(ElecStorage(ElecStorNum)%StorageModelMode == SimpleBucketStorage) THEN
       ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge = ElecStorage(ElecStorNum)%StartingEnergyStored
       ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge = ElecStorage(ElecStorNum)%StartingEnergyStored
       ElecStorage(ElecStorNum)%PelNeedFromStorage        = 0.d0
       ElecStorage(ElecStorNum)%PelFromStorage            = 0.d0
       ElecStorage(ElecStorNum)%PelIntoStorage            = 0.d0
       ElecStorage(ElecStorNum)%QdotConvZone              = 0.d0
       ElecStorage(ElecStorNum)%QdotRadZone               = 0.d0
       ElecStorage(ElecStorNum)%TimeElapsed               = 0.d0
       ElecStorage(ElecStorNum)%ElectEnergyinStorage      = 0.d0
       ElecStorage(ElecStorNum)%StoredPower               = 0.d0
       ElecStorage(ElecStorNum)%StoredEnergy              = 0.d0
       ElecStorage(ElecStorNum)%DecrementedEnergyStored   = 0.d0
       ElecStorage(ElecStorNum)%DrawnPower                = 0.d0
       ElecStorage(ElecStorNum)%DrawnEnergy               = 0.d0
       ElecStorage(ElecStorNum)%ThermLossRate             = 0.d0
       ElecStorage(ElecStorNum)%ThermLossEnergy           = 0.d0


    ELSEIF(ElecStorage(ElecStorNum)%StorageModelMode == KiBaMBattery) THEN
       initialCharge = Elecstorage(ElecStorNum)%MaxAhCapacity * Elecstorage(ElecStorNum)%StartingSOC
       Elecstorage(ElecStorNum)%LastTwoTimeStepAvailable = initialCharge * Elecstorage(ElecStorNum)%AvailableFrac
       Elecstorage(ElecStorNum)%LastTwoTimeStepBound = initialCharge * (1.0d0-Elecstorage(ElecStorNum)%AvailableFrac)
       Elecstorage(ElecStorNum)%LastTimeStepAvailable = initialCharge * Elecstorage(ElecStorNum)%AvailableFrac
       Elecstorage(ElecStorNum)%LastTimeStepBound = initialCharge * (1.0d0-Elecstorage(ElecStorNum)%AvailableFrac)
       Elecstorage(ElecStorNum)%ThisTimeStepAvailable = initialCharge * Elecstorage(ElecStorNum)%AvailableFrac
       Elecstorage(ElecStorNum)%ThisTimeStepBound = initialCharge * (1.0d0-Elecstorage(ElecStorNum)%AvailableFrac)
       IF (ElecStorage(ElecStorNum)%LifeCalculation .eq. Battery_LifeCalculation_Yes) THEN
          ElecStorage(ElecStorNum)%count0  = 2                                     ! Index 1 is for initial SOC, so new input starts from index 2.
          ElecStorage(ElecStorNum)%B10(1)  = Elecstorage(ElecStorNum)%StartingSOC  ! the initial fractional SOC is stored as the reference
          ElecStorage(ElecStorNum)%X0      = 0.0D0
          ElecStorage(ElecStorNum)%OneNmb0 = 0.0D0
          ElecStorage(ElecStorNum)%Nmb0    = 0.0D0
          ElecStorage(ElecStorNum)%BatteryDamage = 0.0D0
       ENDIF
    ENDIF
    MyEnvrnFlag(ElecStorNum)  = .FALSE.
    MyWarmupFlag(ElecStorNum) = .TRUE.
  ENDIF
  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlag(ElecStorNum) = .TRUE.

  IF (MyWarmupFlag(ElecStorNum) .AND. (.NOT. WarmUpFlag) ) THEN
    ! need to reset initial state of charge at beginning of environment but after warm up is complete
    IF(ElecStorage(ElecStorNum)%StorageModelMode == SimpleBucketStorage) THEN
       ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge = ElecStorage(ElecStorNum)%StartingEnergyStored
       ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge = ElecStorage(ElecStorNum)%StartingEnergyStored
    ELSEIF(ElecStorage(ElecStorNum)%StorageModelMode == KiBaMBattery) THEN
       initialCharge = Elecstorage(ElecStorNum)%MaxAhCapacity * Elecstorage(ElecStorNum)%StartingSOC
       Elecstorage(ElecStorNum)%LastTwoTimeStepAvailable = initialCharge * Elecstorage(ElecStorNum)%AvailableFrac
       Elecstorage(ElecStorNum)%LastTwoTimeStepBound = initialCharge * (1.0d0-Elecstorage(ElecStorNum)%AvailableFrac)
       Elecstorage(ElecStorNum)%LastTimeStepAvailable = initialCharge * Elecstorage(ElecStorNum)%AvailableFrac
       Elecstorage(ElecStorNum)%LastTimeStepBound = initialCharge * (1.0d0-Elecstorage(ElecStorNum)%AvailableFrac)
       Elecstorage(ElecStorNum)%ThisTimeStepAvailable = initialCharge * Elecstorage(ElecStorNum)%AvailableFrac
       Elecstorage(ElecStorNum)%ThisTimeStepBound = initialCharge * (1.0d0-Elecstorage(ElecStorNum)%AvailableFrac)
       IF (ElecStorage(ElecStorNum)%LifeCalculation .eq. Battery_LifeCalculation_Yes) THEN
          ElecStorage(ElecStorNum)%count0  = 2                                     ! Index 1 is for initial SOC, so new input starts from index 2.
          ElecStorage(ElecStorNum)%B10(1)  = Elecstorage(ElecStorNum)%StartingSOC  ! the initial fractional SOC is stored as the reference
          ElecStorage(ElecStorNum)%X0      = 0.0D0
          ElecStorage(ElecStorNum)%OneNmb0 = 0.0D0
          ElecStorage(ElecStorNum)%Nmb0    = 0.0D0
          ElecStorage(ElecStorNum)%BatteryDamage = 0.0D0
       ENDIF
    ENDIF
    MyWarmupFlag(ElecStorNum) = .FALSE.
  ENDIF

  TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed
  If (ElecStorage(ElecStorNum)%TimeElapsed /= TimeElapsed) Then !time changed, update last with "current" result from previous time
!   When program comes here, this means the first iteration of a new system time step. Battery life calculation needs to be considered
!   for the Kinetic battery model.
    IF (ElecStorage(ElecStorNum)%StorageModelMode == KiBaMBattery .AND. &
        ElecStorage(ElecStorNum)%LifeCalculation.eq.Battery_LifeCalculation_Yes) THEN
!    At this point, the current values, last time step values and last two time step values have not been updated, hence:
!    "ThisTimeStep*" actually points to the previous one time step
!    "LastTimeStep*" actually points to the previous two time steps
!    "LastTwoTimeStep" actually points to the previous three time steps

!      Calculate the fractional SOC change between the "current" time step and the "previous one" time step
       DeltaSOC1 = ElecStorage(ElecStorNum)%ThisTimeStepAvailable + ElecStorage(ElecStorNum)%ThisTimeStepBound &
                   - ElecStorage(ElecStorNum)%LastTimeStepAvailable - ElecStorage(ElecStorNum)%LastTimeStepBound
       DeltaSOC1 = DeltaSOC1/Elecstorage(ElecStorNum)%MaxAhCapacity

!      Calculate the fractional SOC change between the "previous one" time step and the "previous two" time steps
       DeltaSOC2 = ElecStorage(ElecStorNum)%LastTimeStepAvailable + ElecStorage(ElecStorNum)%LastTimeStepBound &
                   - ElecStorage(ElecStorNum)%LastTwoTimeStepAvailable - ElecStorage(ElecStorNum)%LastTwoTimeStepBound
       DeltaSOC2 = DeltaSOC2/Elecstorage(ElecStorNum)%MaxAhCapacity

!     DeltaSOC2 = 0 may occur at the begining of each simulation environment.
!     DeltaSOC1 * DeltaSOC2 means that the SOC from "LastTimeStep" is a peak or valley. Only peak or valley needs
!     to call the rain flow algorithm
       IF ( (DeltaSOC2 == 0) .OR.  ((DeltaSOC1 * DeltaSOC2) .lt. 0) ) THEN
!     Because we cannot determine whehter "ThisTimeStep" is a peak or valley (next time step is unknown yet), we
!     use the "LastTimeStep" value for battery life calculation.
         Input0 = (ElecStorage(ElecStorNum)%LastTimeStepAvailable + ElecStorage(ElecStorNum)%LastTimeStepBound)/  &
                  Elecstorage(ElecStorNum)%MaxAhCapacity
         ElecStorage(ElecStorNum)%B10(ElecStorage(ElecStorNum)%count0)   = input0

!        The arrary size needs to be increased when count = MAXRainflowArrayBounds. Please note that (MAXRainflowArrayBounds +1)
!        is the index used in the subroutine RainFlow. So we cannot reallocate array size until count = MAXRainflowArrayBounds +1.
         IF (ElecStorage(ElecStorNum)%count0 == MAXRainflowArrayBounds) THEN
            SaveArrayBounds=MAXRainflowArrayBounds+1
            CALL ReallocateRealArray(ElecStorage(ElecStorNum)%B10,SaveArrayBounds,MAXRainFlowArrayInc)
            SaveArrayBounds=MAXRainflowArrayBounds+1
            CALL ReallocateRealArray(ElecStorage(ElecStorNum)%X0,SaveArrayBounds,MAXRainFlowArrayInc)
!           Decrement by 1 is needed because the last input parameter of RainFlow is MAXRainflowArrayBounds+1
            MAXRainflowArrayBounds=SaveArrayBounds-1  
         ENDIF
           
         Call Rainflow(ElecStorage(ElecStorNum)%CycleBinNum,input0,ElecStorage(ElecStorNum)%B10,  &
            ElecStorage(ElecStorNum)%X0,ElecStorage(ElecStorNum)%count0,  &
            ElecStorage(ElecStorNum)%Nmb0,ElecStorage(ElecStorNum)%OneNmb0,MAXRainflowArrayBounds+1)

         ElecStorage(ElecStorNum)%BatteryDamage=0.0d0

         DO BinNum = 1, ElecStorage(ElecStorNum)%CycleBinNum
!       Battery damage is calculated by accumulating the impact from each cycle.
          ElecStorage(ElecStorNum)%BatteryDamage = ElecStorage(ElecStorNum)%OneNmb0(BinNum)/  &
              Curvevalue(ElecStorage(ElecStorNum)%LifeCurveNum,   &
               (Real(BinNum,r64)/Real(ElecStorage(ElecStorNum)%CycleBinNum,r64))) &
               + ElecStorage(ElecStorNum)%BatteryDamage
         ENDDO
       ENDIF
    ENDIF

!   Updating "LastTimeStep" and "LastTwoTimeStep" should be done after calling the rain flow algorithm.
!   Otherwise, it is not possible to determine whehter "LastTimeStep" is a peak or valley value.

    ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge = ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge
    Elecstorage(ElecStorNum)%LastTwoTimeStepAvailable = Elecstorage(ElecStorNum)%LastTimeStepAvailable
    Elecstorage(ElecStorNum)%LastTwoTimeStepBound = Elecstorage(ElecStorNum)%LastTimeStepBound
    Elecstorage(ElecStorNum)%LastTimeStepAvailable=Elecstorage(ElecStorNum)%ThisTimeStepAvailable
    Elecstorage(ElecStorNum)%LastTimeStepBound=Elecstorage(ElecStorNum)%ThisTimeStepBound
    ElecStorage(ElecStorNum)%TimeElapsed = TimeElapsed
  ENDIF

  ! end Inits

  !Begin determine Power demand request

  If (ElecLoadCenter(LoadCenterNum)%InverterPresent) Then
   ! this will be lagged from last calc.
    PpcuLosses = Inverter(ElecLoadCenter(LoadCenterNum)%InverterModelNum)%ThermLossRate
  ELSE
    PpcuLosses = 0.0D0
  ENDIF

  !  Pdemand = Sum (ElecLoadCenter(LoadCenterNum)%ElecGen%PowerRequestThisTimestep) + PpcuLosses
  !  Modified when improved electric load center disptach procedure was implemented
    Pdemand = ElecLoadCenter(LoadCenterNum)%TotalPowerRequest + PpcuLosses
  ! END determine Power demand request.

  ! BEGIN determine available generation supply
  SELECT CASE (ElecLoadCenter(LoadCenterNum)%BussType)

  CASE (ACBussStorage)
    Pgensupply = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%ElectProdRate)
  CASE (DCBussInverterDCStorage)
    ElecLoadCenter(LoadCenterNum)%DCElectProdRate = Sum(ElecLoadCenter(LoadCenterNum)%ElecGen%DCElectProdRate)
    Pgensupply =  ElecLoadCenter(LoadCenterNum)%DCElectProdRate
  CASE (DCBussInverterACStorage)

    Pgensupply = Inverter(ElecLoadCenter(LoadCenterNum)%InverterModelNum)%ACPowerOut
  END SELECT
  ! End determine available generation

   !initialize locals
  tmpPdraw   = 0.d0
  tmpPcharge = 0.d0
  drawing  = .false.
  charging = .false.
  Pstorage = 0.d0

  IF (ElecStorage(ElecStorNum)%StorageModelMode == KiBaMBattery) THEN
      Numpar     = ElecStorage(ElecStorNum)%ParallelNum
      Numser     = ElecStorage(ElecStorNum)%SeriesNum
      Numbattery = Numpar*Numser
      InternalR  = Elecstorage(ElecStorNum)%InternalR
      qmax       = Elecstorage(ElecStorNum)%MaxAhCapacity
      E0c        = ElecStorage(ElecStorNum)%ChargedOCV
      E0d        = ElecStorage(ElecStorNum)%DischargedOCV
      k          = ElecStorage(ElecStorNum)%ChargeConversionRate
      c          = ElecStorage(ElecStorNum)%AvailableFrac
  ENDIF



!*****************************************************************************************************************

  ! step 1 figure out what is desired of electrical storage system

  If (Pgensupply < (Pdemand )) THEN
    !draw from storage
    tmpPdraw = Pdemand  - Pgensupply
    tmpPcharge = 0.d0
    drawing = .true.
    charging = .false.

  ELSEIF (Pgensupply > (Pdemand )) THEN
    !add to storage
    tmpPcharge = Pgensupply - Pdemand
    tmpPdraw = 0.d0
    charging = .true.
    drawing = .false.

  ELSEIF (Pgensupply == (Pdemand )) THEN
    !do nothing
    tmpPcharge = 0.d0
    tmpPdraw = 0.d0
    charging = .false.
    drawing = .false.
  ENDIF

  ! EMS override -- intent to draw, charge or hold?
  IF ((ElecStorage(ElecStorNum)%EMSOverridePelFromStorage)  &
      .AND. (.NOT. ElecStorage(ElecStorNum)%EMSOverridePelIntoStorage)  ) THEN
    ! EMS is calling for specific discharge rate
    tmpPdraw = MAX(ElecStorage(ElecStorNum)%EMSValuePelFromStorage, 0.0D0)
    tmpPcharge = 0.0D0
    drawing  = .TRUE.
    charging = .FALSE.
  ELSEIF((.NOT. ElecStorage(ElecStorNum)%EMSOverridePelFromStorage) &
         .AND. (ElecStorage(ElecStorNum)%EMSOverridePelIntoStorage)) THEN
    ! EMS is calling for specific charge rate
    tmpPcharge= MAX(ElecStorage(ElecStorNum)%EMSValuePelIntoStorage, 0.0D0)
    tmpPdraw = 0.0D0
    drawing  = .FALSE.
    charging = .TRUE.
  ELSEIF ((ElecStorage(ElecStorNum)%EMSOverridePelFromStorage)  &
      .AND. (ElecStorage(ElecStorNum)%EMSOverridePelIntoStorage)  ) THEN
    ! EMS is asking to override both
    IF ( ElecStorage(ElecStorNum)%EMSValuePelIntoStorage > ElecStorage(ElecStorNum)%EMSValuePelFromStorage) THEN
      tmpPcharge= ElecStorage(ElecStorNum)%EMSValuePelIntoStorage - ElecStorage(ElecStorNum)%EMSValuePelFromStorage
      tmpPdraw = 0.0D0
      drawing  = .FALSE.
      charging = .TRUE.
    ELSEIF ( ElecStorage(ElecStorNum)%EMSValuePelIntoStorage < ElecStorage(ElecStorNum)%EMSValuePelFromStorage) THEN
      tmpPdraw = ElecStorage(ElecStorNum)%EMSValuePelFromStorage - ElecStorage(ElecStorNum)%EMSValuePelIntoStorage
      tmpPcharge = 0.0D0
      drawing  = .TRUE.
      charging = .FALSE.
    ELSE !they equal just hold
      tmpPdraw = 0.0D0
      tmpPcharge = 0.0D0
      drawing  = .FALSE.
      charging = .FALSE.
    ENDIF
  ENDIF

!*****************************************************************************************************************


  !  step 2, figure out what is possible for electrical storage draws/charges

  If (charging) then

    IF (ElecStorage(ElecStorNum)%StorageModelMode == SimpleBucketStorage) THEN

      IF (ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge >= ElecStorage(ElecStorNum)%MaxEnergyCapacity) THEN
          ! storage full!  no more allowed!
          tmpPcharge = 0.d0
   !       Constrained = .true.
          charging  = .FALSE.
      ENDIF
      IF (tmpPcharge > ElecStorage(ElecStorNum)%MaxPowerStore) THEN
          tmpPcharge = ElecStorage(ElecStorNum)%MaxPowerStore
  !        Constrained = .true.
      ENDIF


      !now add energy to storage from charging
     IF ((ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge &
         + tmpPcharge *TimeStepSys*SecInHour*ElecStorage(ElecStorNum)%EnergeticEfficCharge) &
          < ElecStorage(ElecStorNum)%MaxEnergyCapacity) THEN

         ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge =  ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge &
                               + tmpPcharge *TimeStepSys*SecInHour*ElecStorage(ElecStorNum)%EnergeticEfficCharge
     ELSE ! would over charge this time step

        tmpPcharge = (ElecStorage(ElecStorNum)%MaxEnergyCapacity - ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge) &
                     /(TimeStepSys*SecInHour*ElecStorage(ElecStorNum)%EnergeticEfficCharge)
   !     Constrained = .true.
        ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge =  ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge &
                               + tmpPcharge *TimeStepSys*SecInHour*ElecStorage(ElecStorNum)%EnergeticEfficCharge
     ENDIF
    Pstorage = tmpPcharge

   ENDIF

    IF (ElecStorage(ElecStorNum)%StorageModelMode == KiBaMBattery) THEN
    !*************************************************
    !The sign of power and current is negative in charging
    !*************************************************
      Pw = -tmpPcharge/Numbattery
      q0 = ElecStorage(ElecStorNum)%LastTimeStepAvailable + ElecStorage(ElecStorNum)%LastTimeStepBound

      I0    = 1.0d0         ! Initial assumption
      T0    = abs(qmax/I0) ! Initial Assumption
      qmaxf = qmax*k*c*T0/(1.0d0-exp(-k*T0)+c*(k*T0-1.0d0+exp(-k*T0))) !Initial calculation of a function qmax(I)
      Xf    = q0/qmaxf
      Ef    = E0d + CurveValue(ElecStorage(ElecStorNum)%ChargeCurveNum,Xf) !E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
      Volt  = Ef-I0*InternalR
      Inew  = Pw/Volt
      Tnew  = qmaxf/abs(Inew)
      error = 1.0d0

      DO WHILE (error.gt.0.0001d0)    !Iteration process to get converged current(I)
        I0    = Inew
        T0    = Tnew
        qmaxf = qmax*k*c*T0/(1.0d0-exp(-k*T0)+c*(k*T0-1.0d0+exp(-k*T0)))
        Xf    = q0/qmaxf
        Ef    = E0d+CurveValue(ElecStorage(ElecStorNum)%ChargeCurveNum,Xf) !E0d+Ac*Xf+Cc*Xf/(Dc-Xf) (use curve)
        Volt  = Ef-I0*InternalR
        Inew  = Pw/Volt
        Tnew  = abs(qmaxf/Inew) ! ***Always positive here
        error = abs(Inew-I0)
      ENDDO

      dividend = -k*c*qmax + k*ElecStorage(ElecStorNum)%LastTimeStepAvailable*exp(-k*TimeStepSys) +   &
                      q0*k*c*(1.0d0-exp(-k*TimeStepSys))
      divisor  = 1.0d0 - exp(-k*TimeStepSys) + c*(k*TimeStepSys-1+exp(-k*TimeStepSys))
      Imax  = dividend/divisor
      ! Below: This is the limit of charging current from Charge Rate Limit (input)
      Imax  = Max(Imax,-(qmax-q0)*ElecStorage(ElecStorNum)%MaxChargeRate)

      IF (abs(I0).le.abs(Imax)) THEN
         I0 = Pw/Volt
         Pactual = I0*Volt
      ELSE
         I0    = Imax
         qmaxf = 80.0d0 !Initial assumption to solve the equation using iterative method
         error = 10.0d0 !Initial assumption ...
         DO WHILE (error.gt.0.001d0)
            ! *** I0(current) should be positive for this calculation
            RHS=(qmax*k*c*qmaxf/abs(I0))/(1.0d0-exp(-k*qmaxf/abs(I0))+c*(k*qmaxf/abs(I0)-1.0d0+exp(-k*qmaxf/abs(I0))))
            error=abs(qmaxf-RHS)
            qmaxf=RHS
         ENDDO
      ENDIF

    ENDIF  ! end KiBaM charging

  ENDIF  !charging

  IF (drawing) THEN
    IF (ElecStorage(ElecStorNum)%StorageModelMode == SimpleBucketStorage) THEN

      IF (ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge <= 0.d0) THEN
          ! storage empty  no more allowed!
          tmpPdraw = 0.0d0
          drawing = .FALSE.
      ENDIF
      IF (tmpPdraw > ElecStorage(ElecStorNum)%MaxPowerDraw) THEN
          tmpPdraw = ElecStorage(ElecStorNum)%MaxPowerDraw
      ENDIF

      !now take energy from storage by drawing  (amplified by energetic effic)
      IF ((ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge  &
          - tmpPdraw *TimeStepSys*SecInHour/ElecStorage(ElecStorNum)%EnergeticEfficDischarge) > 0.0d0 ) Then

        ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge =  ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge &
                                - tmpPdraw *TimeStepSys*SecInHour/ElecStorage(ElecStorNum)%EnergeticEfficDischarge
      ELSE !would over drain storage this timestep so reduce tmpPdraw
        tmpPdraw = ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge * ElecStorage(ElecStorNum)%EnergeticEfficDischarge &
                   /(TimeStepSys*SecInHour)
        ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge =  ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge &
                                - tmpPdraw *TimeStepSys*SecInHour/ElecStorage(ElecStorNum)%EnergeticEfficDischarge
        ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge = MAX(ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge, 0.0D0)
      ENDIF

      ElecStorage(ElecStorNum)%ThermLossRate  = tmpPdraw * (1.0d0/ElecStorage(ElecStorNum)%EnergeticEfficDischarge - 1.0d0)
      Pstorage = - tmpPdraw
    ENDIF ! simple discharging

    IF (ElecStorage(ElecStorNum)%StorageModelMode == KiBaMBattery) THEN

    !**********************************************
    !The sign of power and current is positive in discharging
    !**********************************************

      Pw    = tmpPdraw/Numbattery
      q0    = ElecStorage(ElecStorNum)%LastTimeStepAvailable+ElecStorage(ElecStorNum)%LastTimeStepBound
      I0    = 10.0d0    ! Initial assumption
      T0    = qmax/I0   ! Initial Assumption
      qmaxf = qmax*k*c*T0/(1.0d0-exp(-k*T0)+c*(k*T0-1.0d0+exp(-k*T0))) !Initial calculation of a function qmax(I)
      Xf    = (qmax-q0)/qmaxf
      Ef    = E0c + CurveValue(ElecStorage(ElecStorNum)%DischargeCurveNum,Xf) !E0d+Ac*Xf+Cc*X/(Dc-Xf)
      Volt  = Ef-I0*InternalR
      Inew  = Pw/Volt
      Tnew  = qmaxf/Inew
      error = 1.0d0

      DO WHILE (error.gt.0.0001d0) !Iteration process to get converged current(I)
        I0   = Inew
        T0   = Tnew
        qmaxf= qmax*k*c*T0/(1.0d0-exp(-k*T0)+c*(k*T0-1.0d0+exp(-k*T0)))
        Xf   = (qmax-q0)/qmaxf
        Ef   = E0c + CurveValue(ElecStorage(ElecStorNum)%DischargeCurveNum,Xf) !E0c+Ad*Xf+Cd*X/(Dd-Xf)
        Volt = Ef - I0*InternalR
        Inew = Pw/Volt
        Tnew = qmaxf/Inew
        error= abs(Inew-I0)
      ENDDO

      dividend = k*ElecStorage(ElecStorNum)%LastTimeStepAvailable*exp(-k*TimeStepSys) +   &
                q0*k*c*(1.0d0-exp(-k*TimeStepSys))
      divisor  = 1.0d0 - exp(-k*TimeStepSys) + c*(k*TimeStepSys-1.0d0+exp(-k*TimeStepSys))
      Imax     = dividend/divisor
      Imax     = Min(Imax,ElecStorage(ElecStorNum)%MaxDischargeI)
      IF (abs(I0).le.Imax) THEN
         I0 = Pw/Volt
         Pactual = I0*Volt
      ELSE
         I0    = Imax
         qmaxf = 10.0d0     !Initial assumption to solve the equation using iterative method
         error = 10.0d0     !Initial assumption ...
         DO WHILE (error.gt.0.001d0)
            RHS = (qmax*k*c*qmaxf/I0)/(1.0d0-exp(-k*qmaxf/I0)+c*(k*qmaxf/I0-1+exp(-k*qmaxf/I0)))
            error = abs(qmaxf-RHS)
            qmaxf = RHS
         ENDDO
         Xf   = (qmax-q0)/qmaxf
         Ef   = E0c+CurveValue(ElecStorage(ElecStorNum)%DischargeCurveNum,Xf)
         Volt = Ef-I0*InternalR
      ENDIF

      IF (Volt.lt.ElecStorage(ElecStorNum)%CutoffV) THEN
         I0 = 0.d0
      ENDIF
    ENDIF  ! end KiBaM discharging

  ENDIF !drawing

  ! correct if not available.
  IF (GetCurrentScheduleValue(ElecStorage(ElecStorNum)%AvailSchedPtr) == 0.d0) THEN
    IF ((.NOT. ElecStorage(ElecStorNum)%EMSOverridePelFromStorage) &
        .AND. (.NOT. ElecStorage(ElecStorNum)%EMSOverridePelIntoStorage)) THEN
      charging = .FALSE.
      drawing  = .FALSE.
    ENDIF
  ENDIF

 IF (ElecStorage(ElecStorNum)%StorageModelMode == SimpleBucketStorage) THEN
  IF ((.NOT. charging) .AND. ( .NOT. drawing)) THEN

     ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge = ElecStorage(ElecStorNum)%LastTimeStepStateOfCharge
     ElecStorage(ElecStorNum)%PelIntoStorage = 0.d0
     ElecStorage(ElecStorNum)%PelFromStorage = 0.d0
     Pstorage = 0.d0
  ENDIF

  IF (Pstorage >= 0.d0) THEN

    ElecStorage(ElecStorNum)%PelIntoStorage = Pstorage
    ElecStorage(ElecStorNum)%PelFromStorage = 0.0d0
  ENDIF

  IF (Pstorage < 0.d0) THEN

    ElecStorage(ElecStorNum)%PelIntoStorage = 0.d0
    ElecStorage(ElecStorNum)%PelFromStorage = - Pstorage

  ENDIF

  ElecStorage(ElecStorNum)%ElectEnergyinStorage  = ElecStorage(ElecStorNum)%ThisTimeStepStateOfCharge
  ElecStorage(ElecStorNum)%StoredPower           = ElecStorage(ElecStorNum)%PelIntoStorage
  ElecStorage(ElecStorNum)%StoredEnergy          = ElecStorage(ElecStorNum)%PelIntoStorage * TimeStepSys * SecInHour
  ElecStorage(ElecStorNum)%DecrementedEnergyStored = -1.0D0 * ElecStorage(ElecStorNum)%StoredEnergy
  ElecStorage(ElecStorNum)%DrawnPower            = ElecStorage(ElecStorNum)%PelFromStorage
  ElecStorage(ElecStorNum)%DrawnEnergy           = ElecStorage(ElecStorNum)%PelFromStorage * TimeStepSys * SecInHour
  ElecStorage(ElecStorNum)%ThermLossRate         =  MAX(ElecStorage(ElecStorNum)%StoredPower &
                                                       *  (1.0D0 - ElecStorage(ElecStorNum)%EnergeticEfficCharge ) , &
                                                        ElecStorage(ElecStorNum)%DrawnPower &
                                                       *  (1.0D0 - ElecStorage(ElecStorNum)%EnergeticEfficDischarge  ) )
  ElecStorage(ElecStorNum)%ThermLossEnergy       = ElecStorage(ElecStorNum)%ThermLossRate * TimeStepSys * SecInHour

  IF (ElecStorage(ElecStorNum)%ZoneNum > 0) THEN ! set values for zone heat gains
    ElecStorage(ElecStorNum)%QdotconvZone = (1.0D0 - ElecStorage(ElecStorNum)%ZoneRadFract)* ElecStorage(ElecStorNum)%ThermLossRate
    ElecStorage(ElecStorNum)%QdotRadZone  = (ElecStorage(ElecStorNum)%ZoneRadFract) * ElecStorage(ElecStorNum)%ThermLossRate
  ENDIF

  StorageStoredPower=ElecStorage(ElecStorNum)%StoredPower
  StorageDrawnPower=ElecStorage(ElecStorNum)%DrawnPower
 ENDIF ! Outputs for simple battery model

 IF (ElecStorage(ElecStorNum)%StorageModelMode == KiBaMBattery) THEN

    IF ((.NOT. charging) .AND. ( .NOT. drawing)) THEN
        ElecStorage(ElecStorNum)%ThisTimeStepAvailable = ElecStorage(ElecStorNum)%LastTimeStepAvailable
        ElecStorage(ElecStorNum)%ThisTimeStepBound = ElecStorage(ElecStorNum)%LastTimeStepBound
        I0   = 0.d0
        Volt = 0.d0
        q0   = ElecStorage(ElecStorNum)%LastTimeStepAvailable+ElecStorage(ElecStorNum)%LastTimeStepBound
    ELSE
        newAvailable = ElecStorage(ElecStorNum)%LastTimeStepAvailable*exp(-k*TimeStepSys) +   &
           (q0*k*c-I0)*(1.0d0-exp(-k*TimeStepSys))/k - I0*c*(k*TimeStepSys-1.0d0+exp(-k*TimeStepSys))/k
        newBound     = ElecStorage(ElecStorNum)%LastTimeStepBound*exp(-k*TimeStepSys)+q0*(1.0d0-c)*  &
           (1.0d0-exp(-k*TimeStepSys))-I0*(1.0d0-c)*(k*TimeStepSys-1.0d0+exp(-k*TimeStepSys))/k
        ElecStorage(ElecStorNum)%ThisTimeStepAvailable = Max(0.0d0 , newAvailable)
        ElecStorage(ElecStorNum)%ThisTimeStepBound = Max(0.0d0 , newBound)
    ENDIF

    Pactual  = I0*Volt
    TotalSOC = ElecStorage(ElecStorNum)%ThisTimeStepAvailable + ElecStorage(ElecStorNum)%ThisTimeStepBound

  !output1
    IF (TotalSOC.gt.q0) THEN
       ElecStorage(ElecStorNum)%StorageMode  = 2
       ElecStorage(ElecStorNum)%StoredPower  = Volt*I0*Numbattery
       ElecStorage(ElecStorNum)%StoredEnergy = Volt*I0*Numbattery*TimeStepSys*SecInHour
       ElecStorage(ElecStorNum)%DecrementedEnergyStored = -1.0D0 * ElecStorage(ElecStorNum)%StoredEnergy
       ElecStorage(ElecStorNum)%DrawnPower  = 0.0d0
       ElecStorage(ElecStorNum)%DrawnEnergy = 0.0d0

    ELSEIF (TotalSOC.lt.q0) THEN
       ElecStorage(ElecStorNum)%StorageMode  = 1
       ElecStorage(ElecStorNum)%StoredPower  = 0.0d0
       ElecStorage(ElecStorNum)%StoredEnergy = 0.0d0
       ElecStorage(ElecStorNum)%DecrementedEnergyStored = 0.0d0
       ElecStorage(ElecStorNum)%Drawnpower   = Volt*I0*Numbattery
       ElecStorage(ElecStorNum)%Drawnenergy  = Volt*I0*Numbattery*TimeStepSys*SecInHour

    ELSE
       ElecStorage(ElecStorNum)%StorageMode  = 0
       ElecStorage(ElecStorNum)%StoredPower  = 0.0d0
       ElecStorage(ElecStorNum)%StoredEnergy = 0.0d0
       ElecStorage(ElecStorNum)%DecrementedEnergyStored = 0.0d0
       ElecStorage(ElecStorNum)%DrawnPower   = 0.0d0
       ElecStorage(ElecStorNum)%DrawnEnergy  = 0.0d0
    ENDIF

    ElecStorage(ElecStorNum)%AbsoluteSOC          = TotalSOC*Numbattery
    ElecStorage(ElecStorNum)%FractionSOC          = TotalSOC/qmax
    ElecStorage(ElecStorNum)%BatteryCurrent       = I0*Numpar
    ElecStorage(ElecStorNum)%BatteryVoltage       = Volt*Numser
    ElecStorage(ElecStorNum)%ThermLossRate        = InternalR*I0**2*Numbattery
    ElecStorage(ElecStorNum)%ThermLossEnergy      = InternalR*I0**2*TimeStepSys*SecInHour*Numbattery

  IF (ElecStorage(ElecStorNum)%ZoneNum > 0) THEN ! set values for zone heat gains
    ElecStorage(ElecStorNum)%QdotconvZone = ( (1.0D0 - ElecStorage(ElecStorNum)%ZoneRadFract) *  &
       ElecStorage(ElecStorNum)%ThermLossRate) * Numbattery
    ElecStorage(ElecStorNum)%QdotRadZone  = ((ElecStorage(ElecStorNum)%ZoneRadFract) *   &
       ElecStorage(ElecStorNum)%ThermLossRate)*Numbattery
  ENDIF

  StorageStoredPower=ElecStorage(ElecStorNum)%StoredPower
  StorageDrawnPower=ElecStorage(ElecStorNum)%DrawnPower

 ENDIF ! Outputs for kibam model

 RETURN

END SUBROUTINE ManageElectCenterStorageInteractions

!*****************************************************************************************************************

SUBROUTINE FigureElectricalStorageZoneGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Couple electrical storage skin losses to zone heat gains

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
  INTEGER       :: StorNum = 0
  INTEGER       :: ZoneNum = 0

  IF (NumElecStorageDevices == 0) RETURN

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN
    ElecStorage%QdotconvZone = 0.0D0
    ElecStorage%QdotRadZone  = 0.0D0
    MyEnvrnFlag = .FALSE.
  ENDIF
  IF( .NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

!  IF(DoingSizing)THEN

!  ENDIF

  RETURN

END SUBROUTINE FigureElectricalStorageZoneGains


SUBROUTINE ManageTransformers()

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Weimin Wang
          !       DATE WRITTEN   June 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! manage transformer models

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE DataGlobals    , ONLY: TimeStepZone, SecInHour, MetersHaveBeenInitialized
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataHeatBalance, ONLY: ZnAirRpt
  USE InputProcessor,  ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),PARAMETER   :: AmbTempRef=20.0d0       !reference ambient temperature (C)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER     :: TransfNum        !transformer number counter
  INTEGER     :: MeterNum         !counter for the meters wired to a transformer
  INTEGER     :: MeterPtr         !pointer to the meters wired to a transformer
  INTEGER     :: LCNum            !counter for the load centers served by a transformer
  INTEGER     :: ZoneNum          !pointer to the zone where transformer is located

  REAL(r64), external  :: GetInstantMeterValue
  REAL(r64), external  :: GetCurrentMeterValue

  REAL(r64)   :: FactorTempCorr   !temperature correction factor

  REAL(r64)   :: ResRef           !winding resistance at reference temperature (full load)
  REAL(r64)   :: ResSpecified     !winding resistance at specified temperature (specified load)
  REAL(r64)   :: ResRatio         !ratio of winding resistance = ResSpecified/ResRef
  REAL(r64)   :: TempChange       !winding temperature rise (C)
  REAL(r64)   :: AmbTemp          !ambient temperature (C)
  REAL(r64)   :: Capacity         !transformer nameplate capacity(VA)
  REAL(r64)   :: PUL              !per unit load
  REAL(r64)   :: SurplusPower     !surplus power for an electric load center
  REAL(r64)   :: ElecLoad         !transformer load which may be power in or out depending on the usage mode
  REAL(r64)   :: PastElecLoad     !transformer load at the previous timestep
  REAL(r64)   :: TotalLossRate    !the sum of no load loss rate and load loss rate

  REAL(r64)   :: Numerator        !intermediate variable for numerator
  REAL(r64)   :: Denominator      !intermediate variable for denominator

  LOGICAL, SAVE :: MyOneTimeFlag = .TRUE.


  IF(NumTransformers <= 0) RETURN

  IF(MyOneTimeFlag) THEN
  ! calculate rated no load losses and rated load losses if the performance input method is based on
  ! nominal efficiency. This calculation is done only once
    DO TransfNum = 1, NumTransformers
      IF(Transformer(TransfNum)%PerformanceInputMode == EfficiencyMethod) THEN
        ResRef = Transformer(TransfNum)%FactorTempCoeff + Transformer(TransfNum)%TempRise + AmbTempRef
        ResSpecified = Transformer(TransfNum)%FactorTempCoeff + Transformer(TransfNum)%RatedTemp
        ResRatio = ResSpecified/ResRef
        FactorTempCorr = (1.0D0-Transformer(TransfNum)%EddyFrac) * ResRatio +  &
                          Transformer(TransfNum)%EddyFrac * (1.0D0/ResRatio)

        Capacity = Transformer(TransfNum)%RatedCapacity
        Numerator = Capacity * Transformer(TransfNum)%RatedPUL * (1.0D0-Transformer(TransfNum)%RatedEfficiency)
        Denominator = Transformer(TransfNum)%RatedEfficiency * (1.0D0 + &
                       (Transformer(TransfNum)%RatedPUL / Transformer(TransfNum)%MaxPUL)**2)

        Transformer(TransfNum)%RatedNL = Numerator / Denominator
        Transformer(TransfNum)%RatedLL = Transformer(TransfNum)%RatedNL /   &
                                        (FactorTempCorr * (Transformer(TransfNum)%MaxPUL)**2 )

      ENDIF
    END DO

    MyOneTimeFlag = .FALSE.

  ENDIF


  DO TransfNum = 1, NumTransformers
    ElecLoad = 0.0D0
    PastElecLoad = 0.0D0

    IF(Transformer(TransfNum)%UsageMode == PowerInFromGrid) THEN

      DO MeterNum = 1, SIZE(Transformer(TransfNum)%WiredMeterNames)

        IF(MetersHaveBeenInitialized) THEN
          MeterPtr = Transformer(TransfNum)%WiredMeterPtrs(MeterNum)
          ElecLoad = ElecLoad + GetInstantMeterValue(MeterPtr,1)/ (TimeStepZone * SecInHour) &
                          + GetInstantMeterValue(MeterPtr,2)/ (TimeStepSys * SecInHour)
          ! PastElecLoad store the metered value in the previous time step. This value will be used to check whether
          ! a transformer is overloaded or not.
          PastElecLoad = PastElecLoad + GetCurrentMeterValue(MeterPtr)/ (TimeStepZone * SecInHour)
        ELSE
          ElecLoad =0.0D0
          PastElecLoad = 0.0D0
        ENDIF

        ! Because transformer loss has been accounted for by Electricity:Facility and Electricity:HVAC, the transformer
        ! loss needs to be deducted from the metered value. Otherwise, double counting (circular relationship) occurs.
        IF(Transformer(TransfNum)%SpecialMeter(MeterNum) ) THEN
          ElecLoad = ElecLoad - Transformer(TransfNum)%LoadLossRate - Transformer(TransfNum)%NoLoadLossRate

          IF (ElecLoad < 0) ElecLoad = 0.0d0      !Essential check.
        END IF

      END DO

      Transformer(TransfNum)%PowerOut = ElecLoad   !the metered value is transformer's output in PowerInFromGrid mode
    ELSE   !Usage mode is PowerOutFromBldg
      DO LCNum = 1, Transformer(TransfNum)%LoadCenterNum
        SurplusPower = ElecLoadCenter(LCNum)%ElectProdRate - ElecLoadCenter(LCNum)%ElectDemand

        IF(SurplusPower < 0) SurplusPower = 0.0d0

        ElecLoad = ElecLoad + SurplusPower
        PastElecLoad = ElecLoad
      END DO

      Transformer(TransfNum)%PowerIn = ElecLoad    !surplus power is transformer's input in PowerOutFromBldg mode
    END IF


    ! check availability schedule
    IF (GetCurrentScheduleValue(Transformer(TransfNum)%AvailSchedPtr) > 0.0d0) THEN
      Capacity = Transformer(TransfNum)%RatedCapacity
      PUL = ElecLoad / Capacity

      IF (PUL > 1.0D0) THEN
          PUL = 1.0D0
      END IF
     
      !Originally, PUL was used to check whether a transformer is overloaded (PUL > 1.0 or not). However, it was
      !found that ElecLoad obtained from GetInstantMeterVlaue() might refer to intermideiate values before
      !convergence. The intermediate values may issue false warning. This the reason why PastElecLoad obtained
      !by GetCurrentMeterValue() is used here to check overload issue.
      IF( (PastElecLoad/Capacity) > 1.0d0) THEN
        IF(Transformer(TransfNum)%OverloadErrorIndex == 0) THEN
          CALL ShowSevereError('Transformer Overloaded' )
          CALL ShowContinueError('Entered in ElectricLoadCenter:Transformer ='//TRIM(Transformer(TransfNum)%Name) )
        END IF
        CALL ShowRecurringSevereErrorAtEnd('Transformer Overloaded: ' //  &
                                'Entered in ElectricLoadCenter:Transformer ='//TRIM(Transformer(TransfNum)%Name), &
                                 Transformer(TransfNum)%OverloadErrorIndex)
      END IF

      TempChange = (PUL ** 1.6d0) * Transformer(TransfNum)%TempRise

      IF(Transformer(TransfNum)%HeatLossesDestination == ZoneGains) THEN
        ZoneNum = Transformer(TransfNum)%ZoneNum
        AmbTemp = ZnAirRpt(ZoneNum)%MeanAirTemp
      ELSE
         AmbTemp = 20.0D0
      ENDIF

      ResRef         = Transformer(TransfNum)%FactorTempCoeff + Transformer(TransfNum)%TempRise + AmbTempRef
      ResSpecified   = Transformer(TransfNum)%FactorTempCoeff + TempChange + AmbTemp
      ResRatio       = ResSpecified/ResRef
      FactorTempCorr = (1.0D0-Transformer(TransfNum)%EddyFrac) * ResRatio +  &
                          Transformer(TransfNum)%EddyFrac * (1.0D0/ResRatio)

      Transformer(TransfNum)%LoadLossRate   = Transformer(TransfNum)%RatedLL * (PUL **2) * FactorTempCorr
      Transformer(TransfNum)%NoLoadLossRate = Transformer(TransfNum)%RatedNL
    ELSE !Transformer is not available.
      Transformer(TransfNum)%LoadLossRate   = 0.0D0
      Transformer(TransfNum)%NoLoadLossRate = 0.0D0
    ENDIF

    TotalLossRate = Transformer(TransfNum)%LoadLossRate + Transformer(TransfNum)%NoLoadLossRate

    IF(Transformer(TransfNum)%UsageMode == PowerInFromGrid) THEN
      Transformer(TransfNum)%PowerIn    = ElecLoad + TotalLossRate

      !Transformer losses are wired to the meter via the variable "%ElecUseUtility" only if transformer losses
      !are considered in utility cost. If transformer losses are not considered in utility cost, 0 is assigned
      !to the variable "%ElecUseUtility".
      IF(Transformer(TransfNum)%ConsiderLosses) THEN
        Transformer(TransfNum)%ElecUseUtility = TotalLossRate * TimeStepSys * SecInHour
      ELSE
        Transformer(TransfNum)%ElecUseUtility = 0.0D0
      ENDIF

      !Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
      !is assigned 0
      Transformer(TransfNum)%ElecProducedCoGen   = 0.0D0
    ELSE   !Usage mode is PowerOutFromBldg
      Transformer(TransfNum)%PowerOut   = ElecLoad - TotalLossRate

      IF(Transformer(TransfNum)%PowerOut < 0) Transformer(TransfNum)%PowerOut = 0.0D0

      Transformer(TransfNum)%ElecProducedCoGen   = -1.0D0 * TotalLossRate * TimeStepSys * SecInHour

      !Transformer has two modes.If it works in one mode, the variable for meter output in the other mode
      !is assigned 0
      Transformer(TransfNum)%ElecUseUtility = 0.0D0
    ENDIF

    IF( Transformer(TransfNum)%PowerIn <= 0) THEN
      Transformer(TransfNum)%Efficiency      = 0.0D0
    ELSE
      Transformer(TransfNum)%Efficiency      = Transformer(TransfNum)%PowerOut / Transformer(TransfNum)%PowerIn
    ENDIF

    Transformer(TransfNum)%NoLoadLossEnergy  = Transformer(TransfNum)%NoLoadLossRate * TimeStepSys * SecInHour
    Transformer(TransfNum)%LoadLossEnergy  = Transformer(TransfNum)%LoadLossRate * TimeStepSys * SecInHour

    Transformer(TransfNum)%EnergyIn          = Transformer(TransfNum)%PowerIn * TimeStepSys * SecInHour
    Transformer(TransfNum)%EnergyOut         = Transformer(TransfNum)%PowerOut * TimeStepSys * SecInHour

!   Thermal loss rate may not be equal to Total loss rate. This is the case when surplus power is less than the
!    calculated total loss rate for a cogeneration transformer. That is why "PowerIn - PowerOut" is used below.
    Transformer(TransfNum)%ThermalLossRate   = Transformer(TransfNum)%PowerIn - Transformer(TransfNum)%PowerOut
    Transformer(TransfNum)%ThermalLossEnergy = TotalLossRate * TimeStepSys * SecInHour

    IF (Transformer(TransfNum)%ZoneNum > 0) THEN ! set values for zone heat gains
      Transformer(TransfNum)%QdotconvZone = (1.0D0 - Transformer(TransfNum)%ZoneRadFrac)* Transformer(TransfNum)%ThermalLossRate
      Transformer(TransfNum)%QdotRadZone  = (Transformer(TransfNum)%ZoneRadFrac) * Transformer(TransfNum)%ThermalLossRate
    ENDIF
  END DO   ! End TransfNum Loop

END SUBROUTINE ManageTransformers



SUBROUTINE FigureTransformerZoneGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Weimin Wang
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Couple transformer losses to zone heat gains

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: BeginEnvrnFlag
  USE DataHeatBalance, ONLY: ZoneIntGain

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
  INTEGER       :: TransfNum   = 0
  INTEGER       :: ZoneNum     = 0

  IF (NumTransformers == 0) RETURN

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN
    Transformer%QdotconvZone = 0.0D0
    Transformer%QdotRadZone  = 0.0D0
    MyEnvrnFlag = .FALSE.
  ENDIF

  IF( .NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

!  IF(DoingSizing)THEN

!  ENDIF

  RETURN

END SUBROUTINE FigureTransformerZoneGains

SUBROUTINE Rainflow(numbin,input,B1,X,count,Nmb,OneNmb,dim)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Y. KyungTae & W. Wang
          !       DATE WRITTEN   July-August, 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Rainflow cycle counting for battery life calculation

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Ariduru S. 2004. Fatigue life calculation by rainflow cycle counting method.
          !                  Master Thesis, Middle East Technical University.

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: dim               ! end dimension of array
  INTEGER :: numbin  ! numbin = constant value
  REAL(r64) :: input  ! input = input value from other object (battery model)
 !Array B1 stores the value of points
 !Array X stores the value of two data points' difference.
  REAL(r64), DIMENSION(1:dim) :: B1  ! stores values of points, calculated here - stored for next timestep
  REAL(r64), DIMENSION(1:dim) :: X   ! stores values of two data point difference, calculated here - stored for next timestep
  INTEGER :: count   ! calculated here - stored for next timestep in main loop
  REAL(r64), DIMENSION(1:numbin) :: Nmb  ! calculated here - stored for next timestep in main loop
  REAL(r64), DIMENSION(1:numbin) :: OneNmb  ! calculated here - stored for next timestep in main loop

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 REAL(r64) var1,var2
 INTEGER num
 INTEGER i,k

 X(count)=input-B1(count-1) ! calculate the difference between two data (current and previous)

! Get rid of the data if it is not peak nor valley
! The value of count means the number of peak or valley points added to the arrary B10/B1, not including the
! first point B10(0)/B1(0). Therefore, even if count =2, B1(count-2) is still valid.
 IF(count .ge. 3) THEN
!  The following check on peak or valley may be not necessary in most times because the same check is made in the
!  upper-level subroutine. However, it does not hurt to leave it here.
   IF (X(count)*X(count-1) .ge. 0) THEN
     X(count-1) = B1(count)-B1(count-2)
     CALL shift(B1,count-1,count,B1,MAXRainflowArrayBounds+1)  ! Get rid of (count-1) row in B1
     CALL shift(X,count,count,X,MAXRainflowArrayBounds+1)
     count=count-1  ! If the value keep increasing or decreasing, get rid of the middle point.
   ENDIF            ! Only valley and peak will be stored in the matrix, B1

   IF ((count == 3) .AND. (abs(X(2)).le.abs(X(3))) ) THEN
!  This means the starting point is included in X(2), a half cycle is counted according to the rain flow
!  algorithm specified in the reference (Ariduru S. 2004)
     num = NINT((abs(X(2))*numbin*10+5)/10) ! Count half cycle
     Nmb(num) = Nmb(num)+0.5d0
     B1 = EOSHIft(B1,shift=1)              ! Once counting a half cycle, get rid of the value.
     X = EOSHIft(X,shift=1)
     count=count-1                      ! The number of matrix, B1 and X1 decrease.
   ENDIF
 ENDIF ! Counting cyle end
 !*** Note: The value of "count" changes in the upper "IF LOOP"

 IF (count .ge. 4) THEN !count 1 cycle
   DO WHILE (abs(X(count)) .gt. abs(X(count-1)))
!  This means that the starting point is not included in X(count-1). a cycle is counted according to the rain flow
!  algorithm specified in the reference (Ariduru S. 2004)
     num = NINT((abs(X(count-1))*numbin*10+5)/10)
     Nmb(num)=Nmb(num)+1.0d0

!     X(count-2) = abs(X(count))-abs(X(count-1))+abs(X(count-2))
     X(count-2) = B1(count)-B1(count-3) ! Updating X needs to be done before shift operation below

     CALL shift(B1,count-1,count,B1,MAXRainflowArrayBounds+1)   ! Get rid of two data points one by one
     CALL shift(B1,count-2,count,B1,MAXRainflowArrayBounds+1)   ! Delete one point

     CALL shift(X,count,count,X,MAXRainflowArrayBounds)       ! Get rid of two data points one by one
     CALL shift(X,count-1,count,X,MAXRainflowArrayBounds)     ! Delete one point

     count = count-2       ! If one cycle is counted, two data points are deleted.
     IF (count .lt. 4) Exit  ! When only three data points exists, one cycle cannot be counted.
   ENDDO
 ENDIF

 count=count+1

! Check the rest of the half cycles every time step
 OneNmb = Nmb     ! Array Nmb (Bins) will be used for the next time step later.
                     ! OneNmb is used to show the current output only.
! Ideally, the following clean-up counting is needed at the last system time step in each simulation environemnt.
! Because of the difficulty in knowing the above information, the clean-up counting is skipped. Skipping this has
! little impact on the simulation results.
!   DO k = 1, count-1
!     num = NINT((abs(X(k))*numbin*10+5)/10) !Bin number
!     OneNmb(num) = OneNmb(num)+0.5d0
!   ENDDO
END SUBROUTINE

SUBROUTINE shift(A,m,n,B,dim)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Y. KyungTae & W. Wang
          !       DATE WRITTEN   July-August, 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Utility subroutine for rainflow cycle counting

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: dim ! end dimension of arrays
  REAL(r64), DIMENSION(0:dim) :: A
  REAL(r64), DIMENSION(0:dim) :: B
  INTEGER m,n

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ShiftNum  ! Loop variable

  DO ShiftNum=1,m-1
    B(ShiftNum)=A(ShiftNum)
  ENDDO

  DO ShiftNum=m,n
    B(ShiftNum)=A(ShiftNum+1)
  ENDDO
END SUBROUTINE

!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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


END MODULE ManageElectricPower


