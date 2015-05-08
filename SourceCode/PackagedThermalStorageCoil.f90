MODULE PackagedThermalStorageCoil

          ! Module containing the routines dealing with the packaged thermal storage cooling

          ! MODULE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! encapsulate the data and algorithms for modeling packaged thermals storage cooling coils

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataHVACGlobals
USE Psychrometrics
USE DataInterfaces
USE DataEnvironment, ONLY: StdBaroPress, EnvironmentName, CurMnDy, OutDryBulbTemp, OutHumRat, OutBaroPress, OutWetBulbTemp
USE CurveManager

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
! control types
INTEGER, PARAMETER :: ScheduledOpModes   = 1 ! control over TES modes is via local schedule
INTEGER, PARAMETER :: EMSActuatedOpModes = 2 ! control over TES modes is via EMS

! Control Modes
INTEGER, PARAMETER :: OffMode                 = 0 !
INTEGER, PARAMETER :: CoolingOnlyMode         = 1
INTEGER, PARAMETER :: CoolingAndChargeMode    = 2
INTEGER, PARAMETER :: CoolingAndDischargeMode = 3
INTEGER, PARAMETER :: ChargeOnlyMode          = 4
INTEGER, PARAMETER :: DischargeOnlyMode       = 5

! storage media
INTEGER, PARAMETER :: FluidBased = 101
INTEGER, PARAMETER :: IceBased   = 102
!INTEGER, PARAMETER :: UserDefinedFluid = 103



!Water Systems
INTEGER, PARAMETER :: CondensateDiscarded = 1001 ! default mode where water is "lost"
INTEGER, PARAMETER :: CondensateToTank    = 1002 ! collect coil condensate from air and store in water storage tank

INTEGER, PARAMETER :: WaterSupplyFromMains = 101
INTEGER, PARAMETER :: WaterSupplyFromTank  = 102


          ! DERIVED TYPE DEFINITIONS:
TYPE  :: PackagedTESCoolingCoilStruct

  CHARACTER(len=MaxNameLength) :: Name           =' '    ! Name of TES cooling package
  INTEGER   :: AvailSchedNum       = 0 ! pointer to availability schedule
  INTEGER   :: ModeControlType     = 0 ! how are operation modes controlled
  INTEGER   :: ControlModeSchedNum = 0 ! pointer to control schedule if used
  LOGICAL   :: EMSControlModeOn = .FALSE. ! if true, then EMS actuator has been used
  REAL(r64) :: EMSControlModeValue = 0.d0 ! value to use from EMS actuator for control mode
  INTEGER   :: CurControlMode      = OffMode
  INTEGER   :: ControlModeErrorIndex = 0 !

  REAL(r64) :: RatedEvapAirVolFlowRate = 0.d0 ! [m3/s]
  REAL(r64) :: RatedEvapAirMassFlowRate = 0.d0 ! [kg/s]
  INTEGER   :: EvapAirInletNodeNum = 0 ! evaporator inlet node pointer
  INTEGER   :: EvapAirOutletNodeNum = 0 ! evaporator outlet node pointer
  ! Cooling Only Mode
  LOGICAL   :: CoolingOnlyModeIsAvailable =  .FALSE. !
  REAL(r64) :: CoolingOnlyRatedTotCap = 0.d0 ! gross total cooling capacity at rating conditions [W]
  REAL(r64) :: CoolingOnlyRatedSHR    = 0.d0 ! Sensible heat ratio (sens cap/total cap) at rating conditions [W/W]
  REAL(r64) :: CoolingOnlyRatedCOP    = 0.d0 ! Coefficient of performance at rating conditions [W/W]
  INTEGER   :: CoolingOnlyCapFTempCurve = 0 ! curve index for total cooling capacity modifier curve
                                            ! (function of entering wetbulb, outside drybulb)
  INTEGER   :: CoolingOnlyCapFTempObjectNum = 0 ! type of object used for curve input
  INTEGER   :: CoolingOnlyCapFFlowCurve = 0 ! curve index for total cooling capacity modifier curve
                                            ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingOnlyCapFFlowObjectNum = 0 ! type of object used for curve input
  INTEGER   :: CoolingOnlyEIRFTempCurve = 0 ! curve index for energy input ratio modifier curve
                                            ! (function of entering wetbulb, outside drybulb)
  INTEGER   :: CoolingOnlyEIRFTempObjectNum = 0 ! type of object used for curve input
  INTEGER   :: CoolingOnlyEIRFFlowCurve = 0 ! curve index for energy input ratio modifier curve
                                            ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingOnlyEIRFFlowObjectNum = 0 ! type of object used for curve input
  INTEGER   :: CoolingOnlyPLFFPLRCurve  = 0 ! curve index for part-load fact vs part load ratio,EIR modifier
  INTEGER   :: CoolingOnlyPLFFPLRObjectNum = 0 ! type of object used for curve input
  INTEGER   :: CoolingOnlySHRFTempCurve = 0 ! curve index for sensible heat ratio modifier curve
                                            ! (function of entering wetbulb and drybulb)
  INTEGER   :: CoolingOnlySHRFTempObjectNum = 0 ! type of object used for curve input
  INTEGER   :: CoolingOnlySHRFFlowCurve = 0 ! curve index for sensible heat ratio modifer curve
                                            ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingOnlySHRFFlowObjectNum = 0 !

  ! cooling and charge mode
  LOGICAL   :: CoolingAndChargeModeAvailable    = .FALSE.
  REAL(r64) :: CoolingAndChargeRatedTotCap      = 0.d0  ! gross total evaporator cooling capacity at rating conditions [W]
  REAL(r64) :: CoolingAndChargeRatedTotCapSizingFactor = 0.d0 !sizing factor for gross total evaporator [ ]
  REAL(r64) :: CoolingAndChargeRatedChargeCap   = 0.d0  !net storage charging capacity at rating conditions [W]
  REAL(r64) :: CoolingAndChargeRatedChargeCapSizingFactor = 0.d0 !sizing factor for charging capacity [ ]
  REAL(r64) :: CoolingAndChargeRatedSHR         = 0.d0  ! Sensible heat ratio (sens cap/total cap) at rating conditions [W/W]
  REAL(r64) :: CoolingAndChargeCoolingRatedCOP  = 0.d0  ! Coefficient of performance at rating conditions, for cooling [W/W]
  REAL(r64) :: CoolingAndChargeChargingRatedCOP = 0.d0  ! Coefficient of performance at rating conditions, for charging [W/W]
  INTEGER   :: CoolingAndChargeCoolingCapFTempCurve = 0 ! curve index for total cooling capacity modifier curve
                                                        ! (function of entering wetbulb, outside drybulb, state of TES)
  INTEGER   :: CoolingAndChargeCoolingCapFTempObjectNum = 0
  INTEGER   :: CoolingAndChargeCoolingCapFFlowCurve = 0 ! curve index for total cooling capacity modifier curve
                                                        ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingAndChargeCoolingCapFFlowObjectNum = 0 !
  INTEGER   :: CoolingAndChargeCoolingEIRFTempCurve = 0 ! curve index for cooling energy input ratio modifier curve
                                                        ! (function of entering wetbulb, outside drybulb, state of TES)
  INTEGER   :: CoolingAndChargeCoolingEIRFTempObjectNum = 0 !
  INTEGER   :: CoolingAndChargeCoolingEIRFFlowCurve = 0 ! curve index for cooling energy input ratio modifier curve
                                                        ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingAndChargeCoolingEIRFFlowObjectNum = 0
  INTEGER   :: CoolingAndChargeCoolingPLFFPLRCurve  = 0 ! curve index for cooling part-load fact vs part load ratio, EIR modifier
                                                        ! (function of evaporator part load)
  INTEGER   :: CoolingAndChargeCoolingPLFFPLRObjectNum = 0  !
  INTEGER   :: CoolingAndChargeChargingCapFTempCurve = 0 ! curve index for charging capacity modifier curve
                                                         ! (function of entering wetbulb, outside drybulb, state of TES)
  INTEGER   :: CoolingAndChargeChargingCapFTempObjectNum = 0 !
  INTEGER   :: CoolingAndChargeChargingCapFEvapPLRCurve = 0 ! curve index for charging capacity modifier curve
                                                         ! function of evaporator part load ratio
  INTEGER   :: CoolingAndChargeChargingCapFEvapPLRObjectNum = 0 !
  INTEGER   :: CoolingAndChargeChargingEIRFTempCurve = 0 ! curve index for charging energy input ratio modifier curve
                                                         ! (function of entering wetbulb, outside drybulb, state of TES)
  INTEGER   :: CoolingAndChargeChargingEIRFTempObjectNum = 0
  INTEGER   :: CoolingAndChargeChargingEIRFFLowCurve = 0 ! curve index for charging energy input ratio modifier curve
                                                        ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingAndChargeChargingEIRFFLowObjectNum = 0
  INTEGER   :: CoolingAndChargeChargingPLFFPLRCurve  = 0 ! curve index for chargine part-load fact vs part load ratio, EIR modif
                                                        ! (function of evaporator part load)
  INTEGER   :: CoolingAndChargeChargingPLFFPLRObjectNum = 0 !
  INTEGER   :: CoolingAndChargeSHRFTempCurve = 0       ! curve index for sensible heat ratio modifier curve
                                                       ! (function of entering wetbulb and drybulb)
  INTEGER   :: CoolingAndChargeSHRFTempObjectNum = 0 !
  INTEGER   :: CoolingAndChargeSHRFFlowCurve = 0       ! curve index for sensible heat ratio modifer curve
                                                       ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingAndChargeSHRFFlowObjectNum = 0

  !cooling and discharge mode
  LOGICAL   :: CoolingAndDischargeModeAvailable = .FALSE.
  REAL(r64) :: CoolingAndDischargeRatedTotCap      = 0.d0  ! gross total evaporator cooling capacity at rating conditions [W]
  REAL(r64) :: CoolingAndDischargeRatedTotCapSizingFactor = 0.d0 !sizing factor gross total cooling capacity []
  REAL(r64) :: CoolingAndDischargeRatedDischargeCap   = 0.d0  !net storage discharging capacity at rating conditions [W]
  REAL(r64) :: CoolingAndDischargeRatedDischargeCapSizingFactor = 0.d0 !sizing factor discharging capacity []
  REAL(r64) :: CoolingAndDischargeRatedSHR         = 0.d0  ! Sensible heat ratio (sens cap/total cap) at rating conditions [W/W]
  REAL(r64) :: CoolingAndDischargeCoolingRatedCOP  = 0.d0  ! Coefficient of performance at rating conditions, for cooling [W/W]
  REAL(r64) :: CoolingAndDischargeDischargingRatedCOP = 0.d0  ! Coefficient of performance at rating conditions, for charging [W/W]
  INTEGER   :: CoolingAndDischargeCoolingCapFTempCurve = 0 ! curve index for total cooling capacity modifier curve
                                                        ! (function of entering wetbulb, outside drybulb, state of TES)
  INTEGER   :: CoolingAndDischargeCoolingCapFTempObjectNum = 0
  INTEGER   :: CoolingAndDischargeCoolingCapFFlowCurve = 0 ! curve index for total cooling capacity modifier curve
                                                        ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingAndDischargeCoolingCapFFlowObjectNum = 0 !
  INTEGER   :: CoolingAndDischargeCoolingEIRFTempCurve = 0 ! curve index for cooling energy input ratio modifier curve
                                                        ! (function of entering wetbulb, outside drybulb, state of TES)
  INTEGER   :: CoolingAndDischargeCoolingEIRFTempObjectNum = 0 !
  INTEGER   :: CoolingAndDischargeCoolingEIRFFlowCurve = 0 ! curve index for cooling energy input ratio modifier curve
                                                        ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingAndDischargeCoolingEIRFFlowObjectNum = 0 !
  INTEGER   :: CoolingAndDischargeCoolingPLFFPLRCurve  = 0 ! curve index for cooling part-load fact vs part load ratio,
                                                        ! EIR modifier (function of evaporator part load)
  INTEGER   :: CoolingAndDischargeCoolingPLFFPLRObjectNum = 0
  INTEGER   :: CoolingAndDischargeDischargingCapFTempCurve = 0 ! curve index for discharging capacity modifier curve
                                                         ! (function of entering wetbulb, outside drybulb, state of TES)
  INTEGER   :: CoolingAndDischargeDischargingCapFTempObjectNum = 0 !
  INTEGER   :: CoolingAndDischargeDischargingCapFFlowCurve = 0 ! curve index for discharging capacity modifier curve
                                                         ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingAndDischargeDischargingCapFFlowObjectNum = 0 !
  INTEGER   :: CoolingAndDischargeDischargingCapFEvapPLRCurve = 0 ! curve index for discharging capacity modifier curve
                                                         ! function of evaporator part load ratio
  INTEGER   :: CoolingAndDischargeDischargingCapFEvapPLRObjectNum = 0
  INTEGER   :: CoolingAndDischargeDischargingEIRFTempCurve = 0 ! curve index for discharging energy input ratio modifier curve
                                                         ! (function of entering wetbulb, outside drybulb, state of TES)
  INTEGER   :: CoolingAndDischargeDischargingEIRFTempObjectNum = 0 !
  INTEGER   :: CoolingAndDischargeDischargingEIRFFLowCurve = 0 ! curve index for discharging energy input ratio modifier curve
                                                        ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingAndDischargeDischargingEIRFFLowObjectNum = 0 !
  INTEGER   :: CoolingAndDischargeDischargingPLFFPLRCurve  = 0 ! curve index for discharging part-load fact vs part load ratio
                                                        !  EIR modifier (function of evaporator part load)
  INTEGER   :: CoolingAndDischargeDischargingPLFFPLRObjectNum = 0 !
  INTEGER   :: CoolingAndDischargeSHRFTempCurve = 0       ! curve index for sensible heat ratio modifier curve
                                                       ! (function of entering wetbulb and drybulb)
  INTEGER   :: CoolingAndDischargeSHRFTempObjectNum = 0 !
  INTEGER   :: CoolingAndDischargeSHRFFlowCurve = 0       ! curve index for sensible heat ratio modifer curve
                                                       ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: CoolingAndDischargeSHRFFlowObjectNum = 0
  ! Charge Only Mode
  LOGICAL   :: ChargeOnlyModeAvailable = .FALSE.
  REAL(r64) :: ChargeOnlyRatedCapacity = 0.d0 ! net storage charging capacity at rating conditions [W]
  REAL(r64) :: ChargeOnlyRatedCapacitySizingFactor = 0.d0 !sizing factor for charging capacity []
  REAL(r64) :: ChargeOnlyRatedCOP      = 0.d0 ! coefficient of performance at rating conditions [W/W]
  INTEGER   :: ChargeOnlyChargingCapFTempCurve = 0 ! curve index for charging capacity modifier curve
                                                   ! function of outside drybulb and state of TES
  INTEGER   :: ChargeOnlyChargingCapFTempObjectNum = 0 !
  INTEGER   :: ChargeOnlyChargingEIRFTempCurve = 0 ! curve index for charging energy input ratio modifier curve
                                                   ! function of outside drybulb and state of TES
  INTEGER   :: ChargeOnlyChargingEIRFTempObjectNum = 0
  ! Discharge Only mode
  LOGICAL   :: DischargeOnlyModeAvailable = .FALSE.
  REAL(r64) :: DischargeOnlyRatedDischargeCap = 0.d0 ! gross total evaporator cooling capacity at rating conditions [W]
  REAL(r64) :: DischargeOnlyRatedDischargeCapSizingFactor = 0.d0 ! sizing factor for cooling capacity []
  REAL(r64) :: DischargeOnlyRatedSHR          = 0.d0 ! sensible heat ratio (sens cap/total cap) at rating conditions
  REAL(r64) :: DischargeOnlyRatedCOP          = 0.d0 ! coefficient of performance at rating conditions for discharging [W/W]
  INTEGER   :: DischargeOnlyCapFTempCurve     = 0    ! curve index for total cooling capacity modifier curve
                                                     ! function of entering wetbulb and state of TES
  INTEGER   :: DischargeOnlyCapFTempObjectNum = 0
  INTEGER   :: DischargeOnlyCapFFlowCurve     = 0    ! curve index for tot cooling capacity modifier curve
                                                     ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: DischargeOnlyCapFFlowObjectNum = 0
  INTEGER   :: DischargeOnlyEIRFTempCurve     = 0    ! curve index for energy input ratio modifier curve
                                                     ! function of entering wetbulb and state of TES
  INTEGER   :: DischargeOnlyEIRFTempObjectNum = 0
  INTEGER   :: DischargeOnlyEIRFFlowCurve     = 0    ! curve index for energy input ratio modifier curve
                                                     ! (function of actual supply air flow vs rated air flow)
  INTEGER   :: DischargeOnlyEIRFFlowObjectNum = 0
  INTEGER   :: DischargeOnlyPLFFPLRCurve      = 0    ! curve index for part-load fact vs evaporator part load ratio
  INTEGER   :: DischargeOnlyPLFFPLRObjectNum  = 0
  INTEGER   :: DischargeOnlySHRFTempCurve     = 0    ! curve index for sensible heat ratio modifier curve
                                                     ! (function of entering wetbulb and drybulb)
  INTEGER   :: DischargeOnlySHRFTempObjectNum = 0
  INTEGER   :: DischargeOnlySHRFFLowCurve     = 0    ! curve index for
  INTEGER   :: DischargeOnlySHRFFLowObjectNum = 0
  ! other inputs
  REAL(r64) :: AncillaryControlsPower         = 0.d0 ! standby and controls electric power, draws when available [W]
  REAL(r64) :: ColdWeatherMinimumTempLimit    = 0.d0 ! temperature limit for cold weather operation mode [C]
  REAL(r64) :: ColdWeatherAncillaryPower      = 0.d0 ! electrical power draw during cold weather [W]
  INTEGER   :: CondAirInletNodeNum            = 0    ! Condenser air inlet node num pointer
  INTEGER   :: CondAirOutletNodeNum           = 0    ! condenser air outlet node num pointer
  INTEGER   :: CondenserType                  = AirCooled ! Type of condenser for DX cooling coil: AIR COOLED or EVAP COOLED
  REAL(r64) :: CondenserAirVolumeFlow         = 0.d0  ! design air flow rate thru condenser [m3/s]
  REAL(r64) :: CondenserAirFlowSizingFactor   = 0.d0  ! scale condenser air flow relative to evap air flow when autosizing
  REAL(r64) :: CondenserAirMassFlow           = 0.d0  ! design air flow rate thru condenser [kg/s]
  REAL(r64) :: EvapCondEffect                 = 0.d0  ! effectiveness of the evaporatively cooled condenser
  REAL(r64) :: CondInletTemp                  = 0.d0  ! air temperature drybulb entering condenser section after evap cooling [C]
  REAL(r64) :: EvapCondPumpElecNomPower       = 0.d0  ! Nominal power input to the evap condenser water circulation pump [W]
  REAL(r64) :: EvapCondPumpElecEnergy         = 0.d0  ! Electric energy used by condenser water circulation pump [J]
  REAL(r64) :: BasinHeaterPowerFTempDiff      = 0.d0  ! Basin heater power for evaporatively cooled condensers [W/K]
  INTEGER   :: BasinHeaterAvailSchedNum       = 0     ! basin heater availability schedule pointer num
  REAL(r64) :: BasinHeaterSetpointTemp        = 0.d0  ! evap water basin temperature setpoint [C]
  INTEGER   :: EvapWaterSupplyMode            = WaterSupplyFromMains !  where does evap water come from
  CHARACTER(len=MaxNameLength) :: EvapWaterSupplyName = ' ' ! name of water source e.g. water storage tank
  INTEGER   :: EvapWaterSupTankID             = 0 ! supply tank index, if any
  INTEGER   :: EvapWaterTankDemandARRID       = 0 ! evap water demand array index
  INTEGER   :: CondensateCollectMode          = CondensateDiscarded !  where does condensate  water go to
  CHARACTER(len=MaxNameLength) :: CondensateCollectName = ' ' ! name of water source e.g. water storage tank
  INTEGER   :: CondensateTankID               = 0
  INTEGER   :: CondensateTankSupplyARRID      = 0

  ! TES tank
  INTEGER   :: StorageMedia         = 0 ! water/fluid or ice based TES
  CHARACTER(len=MaxNameLength) :: StorageFluidName = ' ' ! if user defined, name of fluid type
  INTEGER   :: StorageFluidIndex     = 0 ! if user defined, index of fluid type
  REAL(r64) :: FluidStorageVolume    = 0.d0 ! volume of water in storage tank for water systems [m3/s]
  REAL(r64) :: IceStorageCapacity    = 0.d0 ! capacity of storage in J
  REAL(r64) :: StorageCapacitySizingFactor = 0.d0 ! storage time used to autocalculate capacity [hr]
  REAL(r64) :: MinimumFluidTankTempLimit = 0.d0  ! optional inputs [C]
  REAL(r64) :: MaximumFluidTankTempLimit = 100.d0  ! optional inputs [C]
  REAL(r64) :: RatedFluidTankTemp = 0.d0 ! rating point condition for fluid storage tanks [C]
  INTEGER   :: StorageAmbientNodeNum = 0 ! node "pointer" for ambient conditions exposed to TES

  REAL(r64) :: StorageUA           = 0.d0 ! overall heat transfer coefficient for TES to ambient [W/k]
  LOGICAL   :: TESPlantConnectionAvailable = .FALSE.
  INTEGER   :: TESPlantInletNodeNum         = 0  ! plant loop inlet node index
  INTEGER   :: TESPlantOutletNodeNum        = 0  ! plant loop outlet node index
  INTEGER   :: TESPlantLoopNum              = 0  ! plant loop connection index
  INTEGER   :: TESPlantLoopSideNum          = 0  ! plant loop side connection index
  INTEGER   :: TESPlantBranchNum            = 0  ! plant loop branch connection index
  INTEGER   :: TESPlantCompNum              = 0  ! plant loop component connection index
  REAL(r64) :: TESPlantDesignVolumeFlowRate = 0.d0 ! plant connection design mass flow rate [m3/s]
  REAL(r64) :: TESPlantDesignMassFlowRate   = 0.d0 ! [kg/s]
  REAL(r64) :: TESPlantEffectiveness       = 0.d0  !
  REAL(r64) :: TimeElapsed  =0.d0 !
  REAL(r64) :: IceFracRemain = 0.d0  ! state of storage for current time step [0..1.0]
  REAL(r64) :: IceFracRemainLastTimestep = 0.d0 ! state of storage for previous time step [0..1.0]
  REAL(r64) :: FluidTankTempFinal = 0.d0
  REAL(r64) :: FluidTankTempFinalLastTimestep = 0.d0

  ! dynamic calculated data
  REAL(r64) :: QdotPlant  = 0.d0  ! heat exchange rate for plant connection to TES tank [W]
  REAL(r64) :: Q_Plant    = 0.d0  !  heat exchange energy for plant connection to TES tank [J]
  REAL(r64) :: QdotAmbient = 0.d0 ! heat exchange rate for skin losses/gains for TES tank to surroundings [W]
  REAL(r64) :: Q_Ambient = 0.d0 ! heat exchange enegy for skin losses/gains for TES tank to surroundings [J]
  REAL(r64) :: QdotTES  = 0.d0    ! heat exchange rate by mechanical systems to charge or discharge TES [W]
  REAL(r64) :: Q_TES    = 0.d0 ! heat exchange energy by mechanical systems to charge or discharge TES [J]

  REAL(r64) :: ElecCoolingPower       = 0.d0  ! electric power for cooling [W]
  REAL(r64) :: ElecCoolingEnergy      = 0.d0  ! electric energy for cooling [J], metered
  REAL(r64) :: EvapTotCoolingRate     = 0.d0  ! evaporator coil total cooling rate [W]
  REAL(r64) :: EvapTotCoolingEnergy   = 0.d0  ! evaporatory coil total cooling energy [J], metered
  REAL(r64) :: EvapSensCoolingRate    = 0.d0
  REAL(r64) :: EvapSensCoolingEnergy  = 0.d0
  REAL(r64) :: EvapLatCoolingRate     = 0.d0
  REAL(r64) :: EvapLatCoolingEnergy   = 0.d0
  REAL(r64) :: RuntimeFraction        = 0.d0
  REAL(r64) :: CondenserRuntimeFraction = 0.d0 !
  REAL(r64) :: ElectColdWeatherPower  = 0.d0  ! electric power for cold weather protection [W]
  REAL(r64) :: ElectColdWeatherEnergy = 0.d0  ! electric energy for cold weather protection [J], metered
  REAL(r64) :: ElectEvapCondBasinHeaterPower = 0.d0
  REAL(r64) :: ElectEvapCondBasinHeaterEnergy = 0.d0


  REAL(r64) :: EvapWaterConsumpRate   = 0.d0 ! Evap Water Consumption rate in m3/sec
  REAL(r64) :: EvapWaterConsump       = 0.d0 ! Evap Water Consumption in m3
  REAL(r64) :: EvapWaterStarvMakupRate= 0.d0 ! Evap water consumed but not really available from tank m3/s
  REAL(r64) :: EvapWaterStarvMakup    = 0.d0 ! Evap water consumed but not really available from tank m3
  REAL(r64) :: EvapCondPumpElecPower  = 0.d0
  REAL(r64) :: EvapCondPumpElecConsumption = 0.d0

ENDTYPE

          ! MODULE VARIABLE DECLARATIONS:
TYPE (PackagedTESCoolingCoilStruct), ALLOCATABLE, DIMENSION(:) :: TESCoil

INTEGER   :: NumTESCoils
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL   :: GetTESInputFlag = .TRUE.
          ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

PUBLIC SimTESCoil

PRIVATE GetTESCoilInput
PRIVATE InitTESCoil
PRIVATE SizeTESCoil
PRIVATE CalcTESCoilOffMode
PUBLIC  CalcTESCoilCoolingOnlyMode
PUBLIC  CalcTESCoilCoolingAndChargeMode
PUBLIC  CalcTESCoilCoolingAndDischargeMode
PUBLIC  CalcTESCoilDischargeOnlyMode
PRIVATE CalcTESCoilChargeOnlyMode
PRIVATE UpdateTEStorage
PRIVATE UpdateColdWeatherProtection
PRIVATE CalcTESWaterStorageTank
PRIVATE CalcTESIceStorageTank
PRIVATE UpdateEvaporativeCondenserWaterUse
PUBLIC  GetTESCoilIndex

CONTAINS

SUBROUTINE SimTESCoil(CompName,  CompIndex,  FanOpMode, TESOpMode, PartLoadRatio)

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
  USE InputProcessor, ONLY: FindItemInList
  USE General , ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)           :: CompName            ! name of the fan coil unit
  INTEGER         , INTENT (INOUT)        :: CompIndex
  INTEGER         , INTENT (IN)           :: FanOpMode  ! allows parent object to control fan mode
  INTEGER         , INTENT (OUT)          :: TESOpMode
  REAL(r64)       , INTENT (IN), OPTIONAL :: PartLoadRatio       ! part load ratio (for single speed cycling unit)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: TESCoilNum


  IF (GetTESInputFlag) THEN
    CALL GetTESCoilInput
    GetTESInputFlag = .FALSE.
  ENDIF

  IF (CompIndex == 0) THEN
    TESCoilNum = FindItemInList(CompName,TESCoil%Name,NumTESCoils)
    IF (TESCoilNum == 0) THEN
      CALL ShowFatalError('Thermal Energy Storage Cooling Coil not found='//TRIM(CompName))
    ENDIF
    CompIndex=TESCoilNum
  ELSE
    TESCoilNum=CompIndex
    IF (TESCoilNum > NumTESCoils .or. TESCoilNum < 1) THEN
      CALL ShowFatalError('SimTESCoil: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(TESCoilNum))// &
                          ', Number of Thermal Energy Storage Cooling Coil Coils='//TRIM(TrimSigDigits(NumTESCoils))//  &
                          ', Coil name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(TESCoilNum)) THEN
      IF (CompName /= Blank .AND. CompName /= TESCoil(TESCoilNum)%Name) THEN
        CALL ShowFatalError('SimTESCoil: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(TESCoilNum))// &
                            ', Coil name='//TRIM(CompName)//', stored Coil Name for that index='//  &
                            TRIM(TESCoil(TESCoilNum)%Name))
      ENDIF
      CheckEquipName(TESCoilNum)=.FALSE.
    ENDIF
  ENDIF

  TESOpMode = 1

  CALL InitTESCoil(TESCoilNum)

  TESOpMode = TESCoil(TESCoilNum)%CurControlMode
  SELECT CASE (TESOpMode)
  CASE (OffMode)
    CALL CalcTESCoilOffMode( TESCoilNum )
  CASE (CoolingOnlyMode)
    CALL CalcTESCoilCoolingOnlyMode(TESCoilNum, FanOpMode, PartLoadRatio)
  CASE (CoolingAndChargeMode)
    CALL CalcTESCoilCoolingAndChargeMode(TESCoilNum, FanOpMode, PartLoadRatio)
  CASE (CoolingAndDischargeMode)
    CALL CalcTESCoilCoolingAndDischargeMode(TESCoilNum, FanOpMode, PartLoadRatio)
  CASE (ChargeOnlyMode)
    CALL CalcTESCoilChargeOnlyMode(TESCoilNum)
  CASE (DischargeOnlyMode)
    CALL CalcTESCoilDischargeOnlyMode(TESCoilNum, PartLoadRatio)
  END SELECT

  RETURN

END SUBROUTINE SimTESCoil



SUBROUTINE GetTESCoilInput

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
  USE DataIPShortCuts
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, GetObjectItemNum, VerifyName, SameString,GetObjectDefMaxArgs
  USE WaterManager,          ONLY: SetupTankDemandComponent, SetupTankSupplyComponent
  USE GlobalNames,           ONLY: VerifyUniqueCoilName
  USE DataSizing,            ONLY: AutoSize
  USE OutAirNodeManager,     ONLY: CheckOutAirNodeNumber
  USE ScheduleManager,       ONLY: GetScheduleIndex
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE FluidProperties,       ONLY: CheckFluidPropertyName, FindGlycol, GetFluidDensityTemperatureLimits, &
                                   GetFluidSpecificHeatTemperatureLimits
  USE DataZoneEquipment,     ONLY: FindControlledZoneIndexFromSystemNodeNumberForZone
  USE DataHeatBalance,       ONLY: IntGainTypeOf_PackagedTESCoilTank

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetTESCoilInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: item  ! do loop counter
  INTEGER :: NumAlphas            ! Number of alphas in input
  INTEGER :: NumNumbers           ! Number of numeric items in input
  INTEGER :: IOStatus             ! Input status returned from GetObjectItem
  LOGICAL :: IsNotOK              ! Flag to verify name
  LOGICAL :: IsBlank              ! Flag for blank name
  LOGICAL :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL :: errflag
  REAL(r64) :: TminRho
  REAL(r64) :: TmaxRho
  REAL(r64) :: TminCp
  REAL(r64) :: TmaxCp
  INTEGER   :: ZoneIndexTrial


  cCurrentModuleObject = 'Coil:Cooling:DX:SingleSpeed:ThermalStorage'
  NumTESCoils = GetNumObjectsFound(cCurrentModuleObject)

  ALLOCATE( TESCoil (NumTESCoils))
  ALLOCATE( CheckEquipName(NumTESCoils))
  CheckEquipName = .TRUE.


  DO item = 1, NumTESCoils
    CALL GetObjectItem(cCurrentModuleObject, item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, &
                       IOStatus, NumBlank=lNumericFieldBlanks, AlphaBlank=lAlphaFieldBlanks,        &
                       AlphaFieldNames= cAlphaFieldNames  , NumericFieldNames= cNumericFieldNames )
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(cAlphaArgs(1), TESCoil%Name, item - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueCoilName(cCurrentModuleObject, cAlphaArgs(1), errflag, TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    TESCoil(item)%Name = cAlphaArgs(1)
    IF (lAlphaFieldBlanks(2)) THEN
      TESCoil(item)%AvailSchedNum = ScheduleAlwaysOn
    ELSE
      TESCoil(item)%AvailSchedNum = GetScheduleIndex(cAlphaArgs(2))
      IF (TESCoil(item)%AvailSchedNum == 0) THEN
        CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(TESCoil(item)%Name)//'", invalid')
        CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF
    SELECT CASE (cAlphaArgs(3))
    CASE ('SCHEDULEDMODES')
      TESCoil(item)%ModeControlType = ScheduledOpModes
    CASE ('EMSCONTROLLED')
      TESCoil(item)%ModeControlType = EMSActuatedOpModes
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
      CALL ShowContinueError('Available choices are ScheduledModes or EMSControlled')
      ErrorsFound=.TRUE.
    END SELECT
    IF (lAlphaFieldBlanks(4)) THEN
      IF (TESCoil(item)%ModeControlType == ScheduledOpModes) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//' is blank but a schedule is needed')
        ErrorsFound=.TRUE.
      ENDIF
    ELSE
      TESCoil(item)%ControlModeSchedNum = GetScheduleIndex(cAlphaArgs(4))
      IF (TESCoil(item)%ControlModeSchedNum == 0 .AND. TESCoil(item)%ModeControlType == ScheduledOpModes) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
        CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF
    SELECT CASE (cAlphaArgs(5))
    CASE ('ICE')
      TESCoil(item)%StorageMedia = IceBased
    CASE ('WATER')
      TESCoil(item)%StorageMedia = FluidBased
      TESCoil(item)%StorageFluidName = 'WATER'
      TESCoil(item)%StorageFluidIndex = FindGlycol('WATER')
    CASE ('USERDEFINEDFLUIDTYPE' )
      TESCoil(item)%StorageMedia = FluidBased
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
      CALL ShowContinueError('Available choices are Ice, Water, or UserDefindedFluidType')
      ErrorsFound=.TRUE.
    END SELECT

    IF (SameString(cAlphaArgs(5), 'USERDEFINEDFLUIDTYPE' )) THEN
      IF (.NOT. (lAlphaFieldBlanks(6))) THEN
        TESCoil(item)%StorageFluidName = cAlphaArgs(6)
        IF(CheckFluidPropertyName(cAlphaArgs(6)) == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", missing fluid data')
          CALL ShowContinueError('Check that fluid property data have been input for fluid name = '//trim(cAlphaArgs(6)) )
          ErrorsFound=.TRUE.
        ELSE
          TESCoil(item)%StorageFluidIndex = FindGlycol(cAlphaArgs(6))
          IF (TESCoil(item)%StorageFluidIndex == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid fluid data')
            CALL ShowContinueError('Check that correct fluid property data have been input for fluid name = '//trim(cAlphaArgs(6)) )
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF

      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
        CALL ShowContinueError('Storage Type is set to UserDefinedFluidType but no name of fluid was entered.' )
        ErrorsFound=.TRUE.
      ENDIF


    ENDIF

    IF ((TESCoil(item)%StorageMedia == FluidBased) .AND. (.not. lNumericFieldBlanks(1))) THEN
      TESCoil(item)%FluidStorageVolume = rNumericArgs(1)
    ELSEIF ((TESCoil(item)%StorageMedia == FluidBased)   .AND. ( lNumericFieldBlanks(1))) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' cannot be blank for Water storage type')
      CALL ShowContinueError('Enter fluid storage tank volume in m3/s.')
      ErrorsFound=.TRUE.
    ENDIF

    IF ((TESCoil(item)%StorageMedia == IceBased) .AND. (.not. lNumericFieldBlanks(2))) THEN
      IF (rNumericArgs(2) == AutoCalculate) THEN
        TESCoil(item)%IceStorageCapacity = rNumericArgs(2)
      ELSE
        TESCoil(item)%IceStorageCapacity = rNumericArgs(2) * 1.d+09 ! input in giga joules, used as joules internally
      ENDIF
    ELSEIF ((TESCoil(item)%StorageMedia == IceBased) .AND. (lNumericFieldBlanks(2))) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' cannot be blank for Ice storage type')
      CALL ShowContinueError('Enter ice storage tank capacity in GJ.')
      ErrorsFound=.TRUE.
    ENDIF

    TESCoil(item)%StorageCapacitySizingFactor = rNumericArgs(3)

    TESCoil(item)%StorageAmbientNodeNum = GetOnlySingleNode(cAlphaArgs(7), ErrorsFound, TRIM(cCurrentModuleObject), &
                                           cAlphaArgs(1), NodeType_Air, NodeConnectionType_Sensor,  &
                                          1, ObjectIsNotParent)

    ZoneIndexTrial = FindControlledZoneIndexFromSystemNodeNumberForZone(TESCoil(item)%StorageAmbientNodeNum)
    IF (ZoneIndexTrial > 0) THEN ! tank is inside a zone so setup internal gains
      CALL SetupZoneInternalGain(ZoneIndexTrial,          &
            'Coil:Cooling:DX:SingleSpeed:ThermalStorage', &
            TESCoil(item)%Name,                           &
            IntGainTypeOf_PackagedTESCoilTank,            &
            ConvectionGainRate    =   TESCoil(item)%QdotAmbient )
    ENDIF

    TESCoil(item)%StorageUA               =  rNumericArgs(4)
    TESCoil(item)%RatedFluidTankTemp      =  rNumericArgs(5)
    TESCoil(item)%RatedEvapAirVolFlowRate =  rNumericArgs(6)

    TESCoil(item)%EvapAirInletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    TESCoil(item)%EvapAirOutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject), cAlphaArgs(1), cAlphaArgs(8), cAlphaArgs(9), 'Air Nodes')

    SELECT CASE (cAlphaArgs(10))
    CASE ('YES')
      TESCoil(item)%CoolingOnlyModeIsAvailable = .TRUE.
    CASE ('NO')
      TESCoil(item)%CoolingOnlyModeIsAvailable = .FALSE.
    CASE DEFAULT
      TESCoil(item)%CoolingOnlyModeIsAvailable = .FALSE.
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(10))//'="'//TRIM(cAlphaArgs(10))//'".')
      CALL ShowContinueError('Available choices are Yes or No.')
      ErrorsFound=.TRUE.
    END SELECT

    TESCoil(item)%CoolingOnlyRatedTotCap = rNumericArgs(7)
    IF (TESCoil(item)%CoolingOnlyModeIsAvailable) THEN ! get input data for this mode

      TESCoil(item)%CoolingOnlyRatedSHR    = rNumericArgs(8)
      TESCoil(item)%CoolingOnlyRatedCOP    = rNumericArgs(9)

      TESCoil(item)%CoolingOnlyCapFTempCurve = GetCurveIndex( cAlphaArgs(11) )
      IF (TESCoil(item)%CoolingOnlyCapFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(11)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(11))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(11))//'="'//TRIM(cAlphaArgs(11))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyCapFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV)
          TESCoil(item)%CoolingOnlyCapFTempObjectNum = GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyCapFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(11))//'="'//TRIM(cAlphaArgs(11))//'".')
          CALL ShowContinueError('Choose a curve or table with two independent variables, x and y.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingOnlyCapFFlowCurve = GetCurveIndex( cAlphaArgs(12) )
      IF (TESCoil(item)%CoolingOnlyCapFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(12)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(12))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(12))//'="'//TRIM(cAlphaArgs(12))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyCapFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingOnlyCapFFlowObjectNum = GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyCapFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(12))//'="'//TRIM(cAlphaArgs(12))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingOnlyEIRFTempCurve = GetCurveIndex( cAlphaArgs(13) )
      IF (TESCoil(item)%CoolingOnlyEIRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(13)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(13))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(13))//'="'//TRIM(cAlphaArgs(13))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyEIRFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV)
          TESCoil(item)%CoolingOnlyEIRFTempObjectNum = GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyEIRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(13))//'="'//TRIM(cAlphaArgs(13))//'".')
          CALL ShowContinueError('Choose a curve or table with two independent variables, x and y.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingOnlyEIRFFlowCurve = GetCurveIndex( cAlphaArgs(14) )
      IF (TESCoil(item)%CoolingOnlyEIRFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(14)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(14))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(14))//'="'//TRIM(cAlphaArgs(14))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyEIRFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingOnlyEIRFFlowObjectNum = GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyEIRFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(14))//'="'//TRIM(cAlphaArgs(14))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingOnlyPLFFPLRCurve = GetCurveIndex( cAlphaArgs(15) )
      IF (TESCoil(item)%CoolingOnlyPLFFPLRCurve == 0) THEN
        IF (lAlphaFieldBlanks(15)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(15))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(15))//'="'//TRIM(cAlphaArgs(15))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyPLFFPLRCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingOnlyPLFFPLRObjectNum = GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlyPLFFPLRCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(15))//'="'//TRIM(cAlphaArgs(15))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingOnlySHRFTempCurve = GetCurveIndex( cAlphaArgs(16) )
      IF (TESCoil(item)%CoolingOnlySHRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(16)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(16))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(16))//'="'//TRIM(cAlphaArgs(16))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlySHRFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV)
          TESCoil(item)%CoolingOnlySHRFTempObjectNum = GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlySHRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(16))//'="'//TRIM(cAlphaArgs(16))//'".')
          CALL ShowContinueError('Choose a curve or table with two independent variables, x and y.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingOnlySHRFFlowCurve = GetCurveIndex( cAlphaArgs(17) )
      IF (TESCoil(item)%CoolingOnlySHRFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(17)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(17))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(17))//'="'//TRIM(cAlphaArgs(17))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlySHRFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingOnlySHRFFlowObjectNum = GetCurveObjectTypeNum(TESCoil(item)%CoolingOnlySHRFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(17))//'="'//TRIM(cAlphaArgs(17))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

    ENDIF

    SELECT CASE (cAlphaArgs(18))
    CASE ('YES')
      TESCoil(item)%CoolingAndChargeModeAvailable = .TRUE.
    CASE ('NO')
      TESCoil(item)%CoolingAndChargeModeAvailable = .FALSE.
    CASE DEFAULT
      TESCoil(item)%CoolingAndChargeModeAvailable = .FALSE.
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(18))//'="'//TRIM(cAlphaArgs(18))//'".')
      CALL ShowContinueError('Available choices are Yes or No.')
      ErrorsFound=.TRUE.
    END SELECT

    IF (TESCoil(item)%CoolingAndChargeModeAvailable ) THEN

      TESCoil(item)%CoolingAndChargeRatedTotCap      = rNumericArgs(10) ! gross total evaporator cooling capacity [W]
      TESCoil(item)%CoolingAndChargeRatedTotCapSizingFactor = rNumericArgs(11) !sizing factor for gross total evaporator [ ]
      TESCoil(item)%CoolingAndChargeRatedChargeCap   = rNumericArgs(12)  !net storage charging capacity at rating conditions [W]
      TESCoil(item)%CoolingAndChargeRatedChargeCapSizingFactor = rNumericArgs(13) !sizing factor for charging capacity [ ]
      TESCoil(item)%CoolingAndChargeRatedSHR         = rNumericArgs(14) ! Sensible heat ratio (sens cap/total cap)  [W/W]
      TESCoil(item)%CoolingAndChargeCoolingRatedCOP  = rNumericArgs(15) ! Coefficient of performance , for cooling [W/W]
      TESCoil(item)%CoolingAndChargeChargingRatedCOP = rNumericArgs(16) ! Coefficient of performance , for charging [W/W]

      TESCoil(item)%CoolingAndChargeCoolingCapFTempCurve = GetCurveIndex( cAlphaArgs(19) )
      IF (TESCoil(item)%CoolingAndChargeCoolingCapFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(19)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(19))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(19))//'="'//TRIM(cAlphaArgs(19))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingCapFTempCurve) )
        CASE (CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndChargeCoolingCapFTempObjectNum = &
                                   GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingCapFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(19))//'="'//TRIM(cAlphaArgs(19))//'".')
          CALL ShowContinueError('Choose a curve or table with three independent variables -- x, y, and z.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeCoolingCapFFlowCurve = GetCurveIndex( cAlphaArgs(20) )
      IF (TESCoil(item)%CoolingAndChargeCoolingCapFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(20)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(20))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(20))//'="'//TRIM(cAlphaArgs(20))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingCapFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndChargeCoolingCapFFlowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingCapFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(20))//'="'//TRIM(cAlphaArgs(20))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeCoolingEIRFTempCurve = GetCurveIndex( cAlphaArgs(21) )
      IF (TESCoil(item)%CoolingAndChargeCoolingEIRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(21)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(21))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(21))//'="'//TRIM(cAlphaArgs(21))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingEIRFTempCurve) )
        CASE (CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndChargeCoolingEIRFTempObjectNum = &
                         GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingEIRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(21))//'="'//TRIM(cAlphaArgs(21))//'".')
          CALL ShowContinueError('Choose a curve or table with three independent variables -- x, y, and z.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeCoolingEIRFFlowCurve = GetCurveIndex( cAlphaArgs(22) )
      IF (TESCoil(item)%CoolingAndChargeCoolingEIRFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(22)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(22))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(22))//'="'//TRIM(cAlphaArgs(22))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingEIRFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndChargeCoolingEIRFFlowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingEIRFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(22))//'="'//TRIM(cAlphaArgs(22))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeCoolingPLFFPLRCurve = GetCurveIndex( cAlphaArgs(23) )
      IF (TESCoil(item)%CoolingAndChargeCoolingPLFFPLRCurve == 0) THEN
        IF (lAlphaFieldBlanks(23)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(23))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(23))//'="'//TRIM(cAlphaArgs(23))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingPLFFPLRCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndChargeCoolingPLFFPLRObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeCoolingPLFFPLRCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(23))//'="'//TRIM(cAlphaArgs(23))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeChargingCapFTempCurve = GetCurveIndex( cAlphaArgs(24) )
      IF (TESCoil(item)%CoolingAndChargeChargingCapFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(24)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(24))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(24))//'="'//TRIM(cAlphaArgs(24))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingCapFTempCurve) )
        CASE (CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndChargeCoolingEIRFTempObjectNum = &
                                  GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingCapFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(24))//'="'//TRIM(cAlphaArgs(24))//'".')
          CALL ShowContinueError('Choose a curve or table with three independent variables -- x, y, and z.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeChargingCapFEvapPLRCurve = GetCurveIndex( cAlphaArgs(25) )
      IF (TESCoil(item)%CoolingAndChargeChargingCapFEvapPLRCurve == 0) THEN
        IF (lAlphaFieldBlanks(25)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(25))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(25))//'="'//TRIM(cAlphaArgs(25))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingCapFEvapPLRCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndChargeChargingCapFEvapPLRObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingCapFEvapPLRCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(25))//'="'//TRIM(cAlphaArgs(25))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeChargingEIRFTempCurve = GetCurveIndex( cAlphaArgs(26) )
      IF (TESCoil(item)%CoolingAndChargeChargingEIRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(26)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(26))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(26))//'="'//TRIM(cAlphaArgs(26))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingEIRFTempCurve) )
        CASE (CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndChargeChargingEIRFTempObjectNum = &
                                  GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingEIRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(26))//'="'//TRIM(cAlphaArgs(26))//'".')
          CALL ShowContinueError('Choose a curve or table with three independent variables -- x, y, and z.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeChargingEIRFFLowCurve = GetCurveIndex( cAlphaArgs(27) )
      IF (TESCoil(item)%CoolingAndChargeChargingEIRFFLowCurve == 0) THEN
        IF (lAlphaFieldBlanks(27)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(27))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(27))//'="'//TRIM(cAlphaArgs(27))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingEIRFFLowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndChargeChargingEIRFFLowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingEIRFFLowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(27))//'="'//TRIM(cAlphaArgs(27))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeChargingPLFFPLRCurve = GetCurveIndex( cAlphaArgs(28) )
      IF (TESCoil(item)%CoolingAndChargeChargingPLFFPLRCurve == 0) THEN
        IF (lAlphaFieldBlanks(28)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(28))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(28))//'="'//TRIM(cAlphaArgs(28))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingPLFFPLRCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndChargeChargingPLFFPLRObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeChargingPLFFPLRCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(28))//'="'//TRIM(cAlphaArgs(28))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeSHRFTempCurve = GetCurveIndex( cAlphaArgs(29) )
      IF (TESCoil(item)%CoolingAndChargeSHRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(29)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(29))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(29))//'="'//TRIM(cAlphaArgs(29))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeSHRFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV, &
                 CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndChargeSHRFTempObjectNum = GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeSHRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(29))//'="'//TRIM(cAlphaArgs(29))//'".')
          CALL ShowContinueError('Choose a curve or table with two or three independent variables.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndChargeSHRFFlowCurve = GetCurveIndex( cAlphaArgs(30) )
      IF (TESCoil(item)%CoolingAndChargeSHRFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(30)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(30))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(30))//'="'//TRIM(cAlphaArgs(30))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeSHRFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndChargeSHRFFlowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndChargeSHRFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(30))//'="'//TRIM(cAlphaArgs(30))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF


    ENDIF ! Cooling and Charge Mode available

    SELECT CASE (cAlphaArgs(31))
    CASE ('YES')
      TESCoil(item)%CoolingAndDischargeModeAvailable = .TRUE.
    CASE ('NO')
      TESCoil(item)%CoolingAndDischargeModeAvailable = .FALSE.
    CASE DEFAULT
      TESCoil(item)%CoolingAndDischargeModeAvailable = .FALSE.
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(31))//'="'//TRIM(cAlphaArgs(31))//'".')
      CALL ShowContinueError('Available choices are Yes or No.')
      ErrorsFound=.TRUE.
    END SELECT

    IF ( TESCoil(item)%CoolingAndDischargeModeAvailable ) THEN

      TESCoil(item)%CoolingAndDischargeRatedTotCap         = rNumericArgs(17) ! gross total evaporator cooling capacity  [W]
      TESCoil(item)%CoolingAndDischargeRatedTotCapSizingFactor = rNumericArgs(18) !sizing factor gross total cooling capacity []
      TESCoil(item)%CoolingAndDischargeRatedDischargeCap   = rNumericArgs(19)  !net storage discharging capacity  [W]
      TESCoil(item)%CoolingAndDischargeRatedDischargeCapSizingFactor = rNumericArgs(20) !sizing factor discharging capacity []
      TESCoil(item)%CoolingAndDischargeRatedSHR            = rNumericArgs(21)  ! Sensible heat ratio (sens cap/total cap) [W/W]
      TESCoil(item)%CoolingAndDischargeCoolingRatedCOP     = rNumericArgs(22)  ! Coefficient of performance , for cooling [W/W]
      TESCoil(item)%CoolingAndDischargeDischargingRatedCOP = rNumericArgs(23)  ! Coefficient of performance , for charging [W/W]

      TESCoil(item)%CoolingAndDischargeCoolingCapFTempCurve = GetCurveIndex( cAlphaArgs(32) )
      IF (TESCoil(item)%CoolingAndDischargeCoolingCapFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(32)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(32))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(32))//'="'//TRIM(cAlphaArgs(32))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingCapFTempCurve) )
        CASE (CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndDischargeCoolingCapFTempObjectNum = &
                                   GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingCapFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(32))//'="'//TRIM(cAlphaArgs(32))//'".')
          CALL ShowContinueError('Choose a curve or table with three independent variables -- x, y, and z.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeCoolingCapFFlowCurve = GetCurveIndex( cAlphaArgs(33) )
      IF (TESCoil(item)%CoolingAndDischargeCoolingCapFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(33)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(33))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(33))//'="'//TRIM(cAlphaArgs(33))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingCapFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndDischargeCoolingCapFFlowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingCapFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(33))//'="'//TRIM(cAlphaArgs(33))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeCoolingEIRFTempCurve = GetCurveIndex( cAlphaArgs(34) )
      IF (TESCoil(item)%CoolingAndDischargeCoolingEIRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(34)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(34))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(34))//'="'//TRIM(cAlphaArgs(34))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingEIRFTempCurve) )
        CASE (CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndDischargeCoolingEIRFTempObjectNum = &
                                   GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingEIRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(34))//'="'//TRIM(cAlphaArgs(34))//'".')
          CALL ShowContinueError('Choose a curve or table with three independent variables -- x, y, and z.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeCoolingEIRFFlowCurve = GetCurveIndex( cAlphaArgs(35) )
      IF (TESCoil(item)%CoolingAndDischargeCoolingEIRFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(35)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(35))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(35))//'="'//TRIM(cAlphaArgs(35))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingEIRFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndDischargeCoolingEIRFFlowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingEIRFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(35))//'="'//TRIM(cAlphaArgs(35))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeCoolingPLFFPLRCurve = GetCurveIndex( cAlphaArgs(36) )
      IF (TESCoil(item)%CoolingAndDischargeCoolingPLFFPLRCurve == 0) THEN
        IF (lAlphaFieldBlanks(36)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(36))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(36))//'="'//TRIM(cAlphaArgs(36))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingPLFFPLRCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndDischargeCoolingPLFFPLRObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeCoolingPLFFPLRCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(36))//'="'//TRIM(cAlphaArgs(36))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeDischargingCapFTempCurve = GetCurveIndex( cAlphaArgs(37) )
      IF (TESCoil(item)%CoolingAndDischargeDischargingCapFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(37)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(37))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(37))//'="'//TRIM(cAlphaArgs(37))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingCapFTempCurve) )
        CASE (CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndDischargeDischargingCapFTempObjectNum = &
                                   GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingCapFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(37))//'="'//TRIM(cAlphaArgs(37))//'".')
          CALL ShowContinueError('Choose a curve or table with three independent variables -- x, y, and z.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeDischargingCapFFlowCurve = GetCurveIndex( cAlphaArgs(38) )
      IF (TESCoil(item)%CoolingAndDischargeDischargingCapFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(38)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(38))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(38))//'="'//TRIM(cAlphaArgs(38))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingCapFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndDischargeDischargingCapFFlowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingCapFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(38))//'="'//TRIM(cAlphaArgs(38))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeDischargingCapFEvapPLRCurve = GetCurveIndex( cAlphaArgs(39) )
      IF (TESCoil(item)%CoolingAndDischargeDischargingCapFEvapPLRCurve == 0) THEN
        IF (lAlphaFieldBlanks(39)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(39))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(39))//'="'//TRIM(cAlphaArgs(39))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingCapFEvapPLRCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndDischargeDischargingCapFEvapPLRObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingCapFEvapPLRCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(39))//'="'//TRIM(cAlphaArgs(39))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeDischargingEIRFTempCurve = GetCurveIndex( cAlphaArgs(40) )
      IF (TESCoil(item)%CoolingAndDischargeDischargingEIRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(40)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(40))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(40))//'="'//TRIM(cAlphaArgs(40))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingEIRFTempCurve) )
        CASE (CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndDischargeDischargingEIRFTempObjectNum = &
                                   GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingEIRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(40))//'="'//TRIM(cAlphaArgs(40))//'".')
          CALL ShowContinueError('Choose a curve or table with three independent variables -- x, y, and z.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeDischargingEIRFFLowCurve = GetCurveIndex( cAlphaArgs(41) )
      IF (TESCoil(item)%CoolingAndDischargeDischargingEIRFFLowCurve == 0) THEN
        IF (lAlphaFieldBlanks(41)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(41))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(41))//'="'//TRIM(cAlphaArgs(41))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingEIRFFLowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndDischargeDischargingEIRFFLowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingEIRFFLowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(41))//'="'//TRIM(cAlphaArgs(41))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeDischargingPLFFPLRCurve = GetCurveIndex( cAlphaArgs(42) )
      IF (TESCoil(item)%CoolingAndDischargeDischargingPLFFPLRCurve == 0) THEN
        IF (lAlphaFieldBlanks(42)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(42))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(42))//'="'//TRIM(cAlphaArgs(42))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingPLFFPLRCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndDischargeDischargingPLFFPLRObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeDischargingPLFFPLRCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(42))//'="'//TRIM(cAlphaArgs(42))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeSHRFTempCurve = GetCurveIndex( cAlphaArgs(43) )
      IF (TESCoil(item)%CoolingAndDischargeSHRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(43)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(43))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(43))//'="'//TRIM(cAlphaArgs(43))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeSHRFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV, &
                 CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%CoolingAndDischargeSHRFTempObjectNum = &
                     GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeSHRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(43))//'="'//TRIM(cAlphaArgs(43))//'".')
          CALL ShowContinueError('Choose a curve or table with two or three independent variables.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%CoolingAndDischargeSHRFFlowCurve = GetCurveIndex( cAlphaArgs(44) )
      IF (TESCoil(item)%CoolingAndDischargeSHRFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(44)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(44))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(44))//'="'//TRIM(cAlphaArgs(44))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeSHRFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%CoolingAndDischargeSHRFFlowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%CoolingAndDischargeSHRFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(44))//'="'//TRIM(cAlphaArgs(44))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

    ENDIF ! cooling and discharge mode available

    SELECT CASE (cAlphaArgs(45))
    CASE ('YES')
      TESCoil(item)%ChargeOnlyModeAvailable = .TRUE.
    CASE ('NO')
      TESCoil(item)%ChargeOnlyModeAvailable = .FALSE.
    CASE DEFAULT
      TESCoil(item)%ChargeOnlyModeAvailable = .FALSE.
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(45))//'="'//TRIM(cAlphaArgs(45))//'".')
      CALL ShowContinueError('Available choices are Yes or No.')
      ErrorsFound=.TRUE.
    END SELECT

    IF ( TESCoil(item)%ChargeOnlyModeAvailable ) THEN

      TESCoil(item)%ChargeOnlyRatedCapacity = rNumericArgs(24) ! net storage charging capacity at rating conditions [W]
      TESCoil(item)%ChargeOnlyRatedCapacitySizingFactor  = rNumericArgs(25) !sizing factor for charging capacity []
      TESCoil(item)%ChargeOnlyRatedCOP      = rNumericArgs(26)  ! coefficient of performance at rating conditions [W/W]

      TESCoil(item)%ChargeOnlyChargingCapFTempCurve = GetCurveIndex( cAlphaArgs(46) )
      IF (TESCoil(item)%ChargeOnlyChargingCapFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(46)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(46))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(46))//'="'//TRIM(cAlphaArgs(46))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%ChargeOnlyChargingCapFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV)
          TESCoil(item)%ChargeOnlyChargingCapFTempObjectNum = &
                     GetCurveObjectTypeNum(TESCoil(item)%ChargeOnlyChargingCapFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(46))//'="'//TRIM(cAlphaArgs(46))//'".')
          CALL ShowContinueError('Choose a curve or table with two independent variables, x and y.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%ChargeOnlyChargingEIRFTempCurve = GetCurveIndex( cAlphaArgs(47) )
      IF (TESCoil(item)%ChargeOnlyChargingEIRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(47)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(47))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(47))//'="'//TRIM(cAlphaArgs(47))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%ChargeOnlyChargingEIRFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV)
          TESCoil(item)%ChargeOnlyChargingEIRFTempObjectNum = &
                     GetCurveObjectTypeNum(TESCoil(item)%ChargeOnlyChargingEIRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(47))//'="'//TRIM(cAlphaArgs(47))//'".')
          CALL ShowContinueError('Choose a curve or table with two independent variables, x and y.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF


    ENDIF ! Charge only mode available

    SELECT CASE (cAlphaArgs(48))
    CASE ('YES')
      TESCoil(item)%DischargeOnlyModeAvailable = .TRUE.
    CASE ('NO')
      TESCoil(item)%DischargeOnlyModeAvailable = .FALSE.
    CASE DEFAULT
      TESCoil(item)%DischargeOnlyModeAvailable = .FALSE.
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(48))//'="'//TRIM(cAlphaArgs(48))//'".')
      CALL ShowContinueError('Available choices are Yes or No.')
      ErrorsFound=.TRUE.
    END SELECT

    IF ( TESCoil(item)%DischargeOnlyModeAvailable ) THEN
      TESCoil(item)%DischargeOnlyRatedDischargeCap = rNumericArgs(27) ! gross total evaporator cooling capacity  [W]
      TESCoil(item)%DischargeOnlyRatedDischargeCapSizingFactor = rNumericArgs(28) ! sizing factor for cooling capacity []
      TESCoil(item)%DischargeOnlyRatedSHR          = rNumericArgs(29)! sensible heat ratio (sens cap/total cap)
      TESCoil(item)%DischargeOnlyRatedCOP          = rNumericArgs(30)! coefficient of performance  for discharging [W/W]

      TESCoil(item)%DischargeOnlyCapFTempCurve = GetCurveIndex( cAlphaArgs(49) )
      IF (TESCoil(item)%DischargeOnlyCapFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(49)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(49))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(49))//'="'//TRIM(cAlphaArgs(49))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyCapFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV)
          TESCoil(item)%DischargeOnlyCapFTempObjectNum = &
                     GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyCapFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(49))//'="'//TRIM(cAlphaArgs(49))//'".')
          CALL ShowContinueError('Choose a curve or table with two independent variables, x and y.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%DischargeOnlyCapFFlowCurve = GetCurveIndex( cAlphaArgs(50) )
      IF (TESCoil(item)%DischargeOnlyCapFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(50)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(50))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(50))//'="'//TRIM(cAlphaArgs(50))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyCapFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%DischargeOnlyCapFFlowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyCapFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(50))//'="'//TRIM(cAlphaArgs(50))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%DischargeOnlyEIRFTempCurve = GetCurveIndex( cAlphaArgs(51) )
      IF (TESCoil(item)%DischargeOnlyEIRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(51)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(51))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(51))//'="'//TRIM(cAlphaArgs(51))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyEIRFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV)
          TESCoil(item)%DischargeOnlyEIRFTempObjectNum = &
                     GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyEIRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(51))//'="'//TRIM(cAlphaArgs(51))//'".')
          CALL ShowContinueError('Choose a curve or table with two independent variables, x and y.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%DischargeOnlyEIRFFlowCurve = GetCurveIndex( cAlphaArgs(52) )
      IF (TESCoil(item)%DischargeOnlyEIRFFlowCurve == 0) THEN
        IF (lAlphaFieldBlanks(52)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(52))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(52))//'="'//TRIM(cAlphaArgs(52))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyEIRFFlowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%DischargeOnlyEIRFFlowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyEIRFFlowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(52))//'="'//TRIM(cAlphaArgs(52))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%DischargeOnlyPLFFPLRCurve = GetCurveIndex( cAlphaArgs(53) )
      IF (TESCoil(item)%DischargeOnlyPLFFPLRCurve == 0) THEN
        IF (lAlphaFieldBlanks(53)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(53))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(53))//'="'//TRIM(cAlphaArgs(53))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyPLFFPLRCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%DischargeOnlyPLFFPLRObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlyPLFFPLRCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(53))//'="'//TRIM(cAlphaArgs(53))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%DischargeOnlySHRFTempCurve = GetCurveIndex( cAlphaArgs(54) )
      IF (TESCoil(item)%DischargeOnlySHRFTempCurve == 0) THEN
        IF (lAlphaFieldBlanks(54)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(54))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(54))//'="'//TRIM(cAlphaArgs(54))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlySHRFTempCurve) )
        CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV, &
                 CurveType_TriQuadratic, CurveType_TableMultiIV)
          TESCoil(item)%DischargeOnlySHRFTempObjectNum = &
                     GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlySHRFTempCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(54))//'="'//TRIM(cAlphaArgs(54))//'".')
          CALL ShowContinueError('Choose a curve or table with two or three independent variables')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

      TESCoil(item)%DischargeOnlySHRFFLowCurve = GetCurveIndex( cAlphaArgs(55) )
      IF (TESCoil(item)%DischargeOnlySHRFFLowCurve == 0) THEN
        IF (lAlphaFieldBlanks(55)) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Required '//TRIM(cAlphaFieldNames(55))//'is blank.')
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError('Not found '//TRIM(cAlphaFieldNames(55))//'="'//TRIM(cAlphaArgs(55))//'".')
        ENDIF
        ErrorsFound=.TRUE.
      ELSE
        ! Verify Curve Object, any curve with just x as single independent variable
        SELECT CASE( GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlySHRFFLowCurve) )
        CASE (CurveType_Linear, CurveType_Quadratic, CurveType_Cubic, CurveType_Quartic,&
              CurveType_Exponent, CurveType_TableOneIV, CurveType_ExponentialSkewNormal, &
              CurveType_Sigmoid,CurveType_RectangularHyperbola1, CurveType_RectangularHyperbola2, &
              CurveType_ExponentialDecay, CurveType_DoubleExponentialDecay)
          TESCoil(item)%DischargeOnlySHRFFLowObjectNum = &
                        GetCurveObjectTypeNum(TESCoil(item)%DischargeOnlySHRFFLowCurve)
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(55))//'="'//TRIM(cAlphaArgs(55))//'".')
          CALL ShowContinueError('Choose a curve or table with one independent variable, x.')
          ErrorsFound=.TRUE.
        END SELECT
      ENDIF

    ENDIF ! Discharge Only mode available

    TESCoil(item)%AncillaryControlsPower      = rNumericArgs(31)
    TESCoil(item)%ColdWeatherMinimumTempLimit = rNumericArgs(32)
    TESCoil(item)%ColdWeatherAncillaryPower   = rNumericArgs(33)
    TESCoil(item)%CondAirInletNodeNum = GetOnlySingleNode(cAlphaArgs(56),ErrorsFound, &
                         TRIM(cCurrentModuleObject),TESCoil(item)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
    TESCoil(item)%CondAirOutletNodeNum = GetOnlySingleNode(cAlphaArgs(57),ErrorsFound, &
                         TRIM(cCurrentModuleObject),TESCoil(item)%Name, &
                         NodeType_Air,NodeConnectionType_ReliefAir,1,ObjectIsNotParent)

    TESCoil(item)%CondenserAirVolumeFlow   = rNumericArgs(34)
    TESCoil(item)%CondenserAirFlowSizingFactor  = rNumericArgs(35)
    SELECT CASE ( cAlphaArgs(58) )

    CASE ('AIRCOOLED')
      TESCoil(item)%CondenserType = AirCooled
    CASE ('EVAPORATIVELYCOOLED')
      TESCoil(item)%CondenserType = EvapCooled
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(TESCoil(item)%Name)//'", invalid')
      CALL ShowContinueError(TRIM(cAlphaFieldNames(58))//'="'//TRIM(cAlphaArgs(58))//'".')
      CALL ShowContinueError('Available choices are AirCooled or EvaporativelyCooled.')
      ErrorsFound=.TRUE.
    END SELECT
    TESCoil(item)%EvapCondEffect            = rNumericArgs(36)
    TESCoil(item)%EvapCondPumpElecNomPower  = rNumericArgs(37)
    TESCoil(item)%BasinHeaterPowerFTempDiff = rNumericArgs(38)
    TESCoil(item)%BasinHeaterSetpointTemp   = rNumericArgs(39)

    IF (lAlphaFieldBlanks(59)) THEN
      TESCoil(item)%BasinHeaterAvailSchedNum = ScheduleAlwaysOn
    ELSE
      TESCoil(item)%BasinHeaterAvailSchedNum = GetScheduleIndex(cAlphaArgs(59))
      IF (TESCoil(item)%BasinHeaterAvailSchedNum == 0) THEN
        CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(TESCoil(item)%Name)//'", invalid')
        CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(59))//'="'//TRIM(cAlphaArgs(59))//'".')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF

    IF (lAlphaFieldBlanks(60)) THEN
      TESCoil(item)%EvapWaterSupplyMode = WaterSupplyFromMains
    ELSE
      TESCoil(item)%EvapWaterSupplyName = cAlphaArgs(60)
      TESCoil(item)%EvapWaterSupplyMode = WaterSupplyFromTank
      CALL SetupTankDemandComponent(TESCoil(item)%Name,TRIM(cCurrentModuleObject), &
                 TESCoil(item)%EvapWaterSupplyName, ErrorsFound, TESCoil(item)%EvapWaterSupTankID, &
                 TESCoil(item)%EvapWaterTankDemandARRID )
    ENDIF

    IF (lAlphaFieldBlanks(61)) THEN
      TESCoil(item)%CondensateCollectMode  = CondensateDiscarded
    ELSE
      TESCoil(item)%CondensateCollectName = cAlphaArgs(61)
      TESCoil(item)%CondensateCollectMode = CondensateToTank
      CALL SetupTankSupplyComponent(TESCoil(item)%Name,TRIM(cCurrentModuleObject), &
                 TESCoil(item)%CondensateCollectName, ErrorsFound, TESCoil(item)%CondensateTankID, &
                 TESCoil(item)%CondensateTankSupplyARRID )
    ENDIF

    IF (.NOT. lAlphaFieldBlanks(62)) THEN
      TESCoil(item)%TESPlantInletNodeNum =  &
               GetOnlySingleNode(cAlphaArgs(62),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, 2, ObjectIsNotParent)

      TESCoil(item)%TESPlantConnectionAvailable = .TRUE.
    ELSE
      TESCoil(item)%TESPlantConnectionAvailable = .FALSE.
    ENDIF
    IF (.NOT. lAlphaFieldBlanks(63)) THEN
      TESCoil(item)%TESPlantOutletNodeNum =  &
               GetOnlySingleNode(cAlphaArgs(63),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, 2, ObjectIsNotParent)
    ELSE
      IF (TESCoil(item)%TESPlantConnectionAvailable) THEN
        CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(TESCoil(item)%Name)//'", invalid')
        CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(63))//' cannot be blank.')
        ErrorsFound=.TRUE.
      ENDIF

    ENDIF
    IF (TESCoil(item)%TESPlantConnectionAvailable) THEN
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(62),  cAlphaArgs(63),'Water Nodes')
    ENDIF

    IF (.NOT. lNumericFieldBlanks(40) ) THEN
      TESCoil(item)%TESPlantDesignVolumeFlowRate = rNumericArgs(40)
    ENDIF
    IF (.NOT. lNumericFieldBlanks(41) ) THEN
      TESCoil(item)%TESPlantEffectiveness = rNumericArgs(41)
    ENDIF
    IF (TESCoil(item)%StorageMedia == FluidBased)  THEN
      IF (.NOT. lNumericFieldBlanks(42) ) THEN
        TESCoil(item)%MinimumFluidTankTempLimit = rNumericArgs(42)
      ELSE

        Call GetFluidDensityTemperatureLimits(TESCoil(item)%StorageFluidIndex, TminRho, TmaxRho)
        CALL GetFluidSpecificHeatTemperatureLimits(TESCoil(item)%StorageFluidIndex, TminCp, TmaxCp)
        TESCoil(item)%MinimumFluidTankTempLimit = MAX(TminRho, TminCp)

      ENDIF
      IF (.NOT. lNumericFieldBlanks(43) ) THEN
        TESCoil(item)%MaximumFluidTankTempLimit = rNumericArgs(43)
      ELSE
        Call GetFluidDensityTemperatureLimits(TESCoil(item)%StorageFluidIndex, TminRho, TmaxRho)
        CALL GetFluidSpecificHeatTemperatureLimits(TESCoil(item)%StorageFluidIndex, TminCp, TmaxCp)
        TESCoil(item)%MaximumFluidTankTempLimit = MIN(TmaxRho, TmaxCp)

      ENDIF
    ENDIF

  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(cCurrentModuleObject)//' input. '//&
                        'Preceding condition(s) causes termination.')
  END IF

  ! setup reporting
  DO item = 1, NumTESCoils
    CALL SetupOutputVariable('Cooling Coil Operating Mode Index []', &
                                    TESCoil(item)%CurControlMode, 'System', 'Average', TESCoil(item)%Name)

    CALL SetupOutputVariable('Cooling Coil Total Cooling Rate [W]', &
                                    TESCoil(item)%EvapTotCoolingRate, 'System', 'Average', TESCoil(item)%Name)
    CALL SetupOutputVariable('Cooling Coil Total Cooling Energy [J]', &
                                    TESCoil(item)%EvapTotCoolingEnergy, 'System', 'Sum', TESCoil(item)%Name, &
                                    ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Cooling Coil Sensible Cooling Rate [W]', &
                                    TESCoil(item)%EvapSensCoolingRate, 'System', 'Average', TESCoil(item)%Name)
    CALL SetupOutputVariable('Cooling Coil Sensible Cooling Energy [J]', &
                                    TESCoil(item)%EvapSensCoolingEnergy, 'System', 'Sum', TESCoil(item)%Name)
    CALL SetupOutputVariable('Cooling Coil Latent Cooling Rate [W]', &
                                    TESCoil(item)%EvapLatCoolingRate, 'System', 'Average', TESCoil(item)%Name)
    CALL SetupOutputVariable('Cooling Coil Latent Cooling Energy [J]', &
                                    TESCoil(item)%EvapLatCoolingEnergy, 'System', 'Sum', TESCoil(item)%Name)
    CALL SetupOutputVariable('Cooling Coil Electric Power [W]', &
                                    TESCoil(item)%ElecCoolingPower, 'System', 'Average', TESCoil(item)%Name)
    CALL SetupOutputVariable('Cooling Coil Electric Energy [J]', &
                                    TESCoil(item)%ElecCoolingEnergy, 'System', 'Sum', TESCoil(item)%Name, &
                                    ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')

    CALL SetupOutputVariable('Cooling Coil Runtime Fraction []', &
                                    TESCoil(item)%RuntimeFraction, 'System', 'Average', TESCoil(item)%Name)
    CALL SetupOutputVariable('Cooling Coil Cold Weather Protection Electric Energy [J]', &
                                    TESCoil(item)%ElectColdWeatherEnergy, 'System','Sum',TESCoil(item)%Name, &
                                    ResourceTypeKey='Electric',EndUseKey='COOLING',EndUseSubKey='Thermal Protection', &
                                    GroupKey='System')
    CALL SetupOutputVariable('Cooling Coil Cold Weather Protection Electric Power [W]', &
                                    TESCoil(item)%ElectColdWeatherPower, 'System', 'Average', TESCoil(item)%Name)

    CALL SetupOutputVariable('Cooling Coil Thermal Storage Mechanical Heat Transfer Rate [W]', &
                                    TESCoil(item)%QdotTES, 'System', 'Average', TESCoil(item)%Name)

    CALL SetupOutputVariable('Cooling Coil Thermal Storage Mechanical Heat Transfer Energy [J]', &
                                    TESCoil(item)%Q_TES, 'System', 'Sum', TESCoil(item)%Name)

    CALL SetupOutputVariable('Cooling Coil Thermal Storage Ambient Heat Transfer Rate [W]', &
                                    TESCoil(item)%QdotAmbient, 'System', 'Average', TESCoil(item)%Name)

    CALL SetupOutputVariable('Cooling Coil Thermal Storage Ambient Heat Transfer Energy [J]', &
                                    TESCoil(item)%Q_Ambient, 'System', 'Sum', TESCoil(item)%Name)

    IF (TESCoil(item)%TESPlantConnectionAvailable) THEN
      CALL SetupOutputVariable('Cooling Coil Thermal Storage Plant Heat Transfer Rate [W]', &
                                    TESCoil(item)%QdotPlant, 'System', 'Average', TESCoil(item)%Name)
      CALL SetupOutputVariable('Cooling Coil Thermal Storage Plant Heat Transfer Energy [J]', &
                                    TESCoil(item)%Q_Plant, 'System', 'Sum', TESCoil(item)%Name)

    ENDIF

    IF (TESCoil(item)%CondenserType == EvapCooled) THEN
      CALL SetupOutputVariable('Cooling Coil Condenser Inlet Temperature [C]', &
                               TESCoil(item)%CondInletTemp,'System','Average',  TESCoil(item)%Name)

      IF (TESCoil(item)%EvapWaterSupplyMode == WaterSupplyFromMains) THEN
        CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Water Volume [m3]',TESCoil(item)%EvapWaterConsump, &
                                 'System','Sum',TESCoil(item)%Name, &
                                  ResourceTypeKey='Water',EndUseKey='Cooling',GroupKey='System')
        CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Mains Supply Water Volume [m3]',  &
                                  TESCoil(item)%EvapWaterConsump, &
                                 'System','Sum',TESCoil(item)%Name, &
                                  ResourceTypeKey='MainsWater',EndUseKey='Cooling',GroupKey='System')
      ELSEIF (TESCoil(item)%EvapWaterSupplyMode == WaterSupplyFromTank) THEN
        CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Storage Tank Water Volume [m3]',&
                            TESCoil(item)%EvapWaterConsump, &
                            'System','Sum',TESCoil(item)%Name, &
                             ResourceTypeKey='Water',EndUseKey='Cooling' , GroupKey='System')
        CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Starved Water Volume [m3]', &
                             TESCoil(item)%EvapWaterStarvMakup, &
                            'System','Sum',TESCoil(item)%Name, &
                             ResourceTypeKey='Water',EndUseKey='Cooling', GroupKey='System')
        CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Starved Mains Water Volume [m3]',&
                             TESCoil(item)%EvapWaterStarvMakup, &
                            'System','Sum',TESCoil(item)%Name, &
                             ResourceTypeKey='MainsWater',EndUseKey='Cooling', GroupKey='System')

      ENDIf

      CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Pump Electric Power [W]',TESCoil(item)%EvapCondPumpElecPower, &
                               'System','Average',TESCoil(item)%Name)
      CALL SetupOutputVariable('Cooling Coil Evaporative Condenser Pump Electric Energy [J]', &
                                TESCoil(item)%EvapCondPumpElecConsumption,'System','Sum',TESCoil(item)%Name, &
                                ResourceTypeKey='Electric',EndUseKey='COOLING',GroupKey='System')

      CALL SetupOutputVariable('Cooling Coil Basin Heater Electric Power [W]', &
                                    TESCoil(item)%ElectEvapCondBasinHeaterPower, 'System','Average',TESCoil(item)%Name)
      CALL SetupOutputVariable('Cooling Coil Basin Heater Electric Energy [J]', &
                                    TESCoil(item)%ElectEvapCondBasinHeaterEnergy, 'System','Sum',TESCoil(item)%Name, &
                                    ResourceTypeKey='Electric',EndUseKey='COOLING',EndUseSubKey='Thermal Protection', &
                                    GroupKey='System')

    ENDIF

    IF (TESCoil(item)%StorageMedia == FluidBased) THEN
      CALL SetupOutputVariable('Cooling Coil Fluid Thermal Storage End Temperature [C]', &
                                    TESCoil(item)%FluidTankTempFinal, 'System','Average',TESCoil(item)%Name)

    ELSEIF (TESCoil(item)%StorageMedia == IceBased) THEN
      CALL SetupOutputVariable('Cooling Coil Ice Thermal Storage End Fraction []', &
                                    TESCoil(item)%IceFracRemain, 'System','Average',TESCoil(item)%Name)
    ENDIF

  ENDDO


  IF (AnyEnergyManagementSystemInModel) THEN
    DO item = 1, NumTESCoils
    ! setup EMS actuator for control mode
      CALL SetupEMSActuator('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(item)%Name, &
                            'Operating Mode' , '[ ]', &
                            TESCoil(item)%EMSControlModeOn, &
                            TESCoil(item)%EMSControlModeValue )
    ENDDO
  ENDIF



  RETURN

END SUBROUTINE GetTESCoilInput

SUBROUTINE InitTESCoil(TESCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
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
  USE DataPlant, ONLY: TypeOf_PackagedTESCoolingCoil, PlantLoop, ScanPlantLoopsForObject
  USE General,   ONLY: RoundSigDigits
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: TESCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyFlag  ! One time environment flag
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MySizeFlag   ! One time sizing flag
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyEnvrnFlag ! flag for init once at start of environment
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyWarmupFlag ! flag for init after warmup complete
  LOGICAL,SAVE :: MyOneTimeFlag = .TRUE.                  ! One time flag used to allocate MyEnvrnFlag and MySizeFlag
  LOGICAL      :: errFlag
  INTEGER      :: plloopnum
  INTEGER      :: lsnum
  INTEGER      :: brnum
  INTEGER      :: cpnum
  REAL(r64)    :: tmpSchedValue

  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MyFlag(NumTESCoils))
    ALLOCATE(MySizeFlag(NumTESCoils))
    ALLOCATE(MyEnvrnFlag(NumTESCoils))
    ALLOCATE(MyWarmupFlag(NumTESCoils))
    MyFlag        = .TRUE.
    MySizeFlag    = .TRUE.
    MyEnvrnFlag   = .TRUE.
    MyOneTimeFlag = .FALSE.
    MyWarmupFlag  = .FALSE.

  END IF


  IF (MyFlag(TESCoilNum)) THEN

    IF (TESCoil(TESCoilNum)%TESPlantConnectionAvailable) THEN
      errFlag = .FALSE.
      CALL ScanPlantLoopsForObject(TESCoil(TESCoilNum)%Name, &
                                   TypeOf_PackagedTESCoolingCoil, &
                                   plloopnum, &
                                   lsnum, &
                                   brnum, &
                                   cpnum)


      ! double check node names match
      IF (errFlag) THEN
        CALL ShowFatalError('InitTESCoil: Program terminated due to previous condition(s).')
      ENDIF
      TESCoil(TESCoilNum)%TESPlantLoopNum = plloopnum
      TESCoil(TESCoilNum)%TESPlantLoopSideNum = lsnum
      TESCoil(TESCoilNum)%TESPlantBranchNum = brnum
      TESCoil(TESCoilNum)%TESPlantCompNum   = cpnum

      IF ((PlantLoop(plloopnum)%LoopSide(lsnum)%Branch(brnum)%Comp(cpnum)%NodeNumIn /= &
             TESCoil(TESCoilNum)%TESPlantInletNodeNum )  .OR. &
          (PlantLoop(plloopnum)%LoopSide(lsnum)%Branch(brnum)%Comp(cpnum)%NodeNumOut /= &
             TESCoil(TESCoilNum)%TESPlantOutletNodeNum) ) THEN
        CALL ShowSevereError('InitTESCoil: Coil:Cooling:DX:SingleSpeed:ThermalStorage ="'//  &
          TRIM(TESCoil(TESCoilNum)%Name)//'", non-matching plant nodes.')
        CALL ShowContinueError('...in Branch="'//TRIM(PlantLoop(TESCoil(TESCoilNum)%TESPlantLoopNum)% &
                           LoopSide(TESCoil(TESCoilNum)%TESPlantLoopSideNum)% &
                             Branch(TESCoil(TESCoilNum)%TESPlantBranchNum)%Name)//  &
                             '", Component referenced with:')
        CALL ShowContinueError('...Inlet Node="'//  &
           TRIM(NodeID(PlantLoop(plloopnum)%LoopSide(lsnum)%Branch(brnum)%Comp(cpnum)%NodeNumIn)))
        CALL ShowContinueError('...Outlet Node="'//  &
           TRIM(NodeID(PlantLoop(plloopnum)%LoopSide(lsnum)%Branch(brnum)%Comp(cpnum)%NodeNumOut)))
        CALL ShowContinueError('...TES Inlet Node="'//TRIM(NodeID(TESCoil(TESCoilNum)%TESPlantInletNodeNum)))
        CALL ShowContinueError('...TES Outlet Node="'//TRIM(NodeID(TESCoil(TESCoilNum)%TESPlantOutletNodeNum)))
        errflag=.true.
      ENDIF
      IF (errFlag) THEN
        CALL ShowFatalError('InitTESCoil: Program terminated due to previous condition(s).')
      ENDIF

    ENDIF ! any plant connection to TES
    MyFlag(TESCoilNum) = .FALSE.
  ENDIF

  IF (MySizeFlag(TESCoilNum)) THEN

    CALL SizeTESCoil(TESCoilNum)

    MySizeFlag(TESCoilNum) = .FALSE.
  ENDIF

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(TESCoilNum)) THEN
    TESCoil(TESCoilNum)%CurControlMode      = OffMode
    TESCoil(TESCoilNum)%QdotPlant = 0.d0
    TESCoil(TESCoilNum)%Q_Plant = 0.d0
    TESCoil(TESCoilNum)%QdotAmbient = 0.d0
    TESCoil(TESCoilNum)%Q_Ambient = 0.d0
    TESCoil(TESCoilNum)%QdotTES = 0.d0
    TESCoil(TESCoilNum)%Q_TES   = 0.d0
    TESCoil(TESCoilNum)%TimeElapsed = 0.d0
    TESCoil(TESCoilNum)%IceFracRemain = 0.d0
    TESCoil(TESCoilNum)%IceFracRemainLastTimestep = 0.d0
    TESCoil(TESCoilNum)%FluidTankTempFinal            = TESCoil(TESCoilNum)%RatedFluidTankTemp
    TESCoil(TESCoilNum)%FluidTankTempFinalLastTimestep = TESCoil(TESCoilNum)%RatedFluidTankTemp
    TESCoil(TESCoilNum)%ElecCoolingPower       = 0.d0  ! electric power for cooling [W]
    TESCoil(TESCoilNum)%ElecCoolingEnergy      = 0.d0  ! electric energy for cooling [J], metered
    TESCoil(TESCoilNum)%EvapTotCoolingRate     = 0.d0  ! evaporator coil total cooling rate [W]
    TESCoil(TESCoilNum)%EvapTotCoolingEnergy   = 0.d0  ! evaporatory coil total cooling energy [J], metered
    TESCoil(TESCoilNum)%EvapSensCoolingRate    = 0.d0
    TESCoil(TESCoilNum)%EvapSensCoolingEnergy  = 0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingRate     = 0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingEnergy   = 0.d0
    TESCoil(TESCoilNum)%RuntimeFraction        = 0.d0
    TESCoil(TESCoilNum)%ElectColdWeatherPower  = 0.d0  ! electric power for cold weather protection [W]
    TESCoil(TESCoilNum)%ElectColdWeatherEnergy = 0.d0  ! electric energy for cold weather protection [J], metered
    TESCoil(TESCoilNum)%ElectEvapCondBasinHeaterPower = 0.d0
    TESCoil(TESCoilNum)%ElectEvapCondBasinHeaterEnergy = 0.d0

    MyEnvrnFlag(TESCoilNum) = .FALSE.
  ENDIF

  IF (.NOT. BeginEnvrnFlag)  MyEnvrnFlag(TESCoilNum) = .TRUE.

  IF ( MyWarmupFlag(TESCoilNum) .and. (.not. WarmUpFlag)) THEN
    !reset to initial condition once warm up is over.
    TESCoil(TESCoilNum)%IceFracRemain = 0.d0
    TESCoil(TESCoilNum)%IceFracRemainLastTimestep = 0.d0
    TESCoil(TESCoilNum)%FluidTankTempFinal            = TESCoil(TESCoilNum)%RatedFluidTankTemp
    TESCoil(TESCoilNum)%FluidTankTempFinalLastTimestep = TESCoil(TESCoilNum)%RatedFluidTankTemp
    MyWarmupFlag(TESCoilNum) = .FALSE.
  ENDIF

  IF (WarmUpFlag ) MyWarmupFlag(TESCoilNum) = .TRUE.

  ! determine control mode
  IF (GetCurrentScheduleValue(TESCoil(TESCoilNum)%AvailSchedNum) /= 0.d0) THEN
    IF (TESCoil(TESCoilNum)%ModeControlType == ScheduledOpModes) THEN
      tmpSchedValue = GetCurrentScheduleValue(TESCoil(TESCoilNum)%ControlModeSchedNum)
      TESCoil(TESCoilNum)%CurControlMode = INT(tmpSchedValue)
      ! check if value is valid
      SELECT CASE (TESCoil(TESCoilNum)%CurControlMode)
      CASE (OffMode, CoolingOnlyMode, CoolingAndChargeMode, CoolingAndDischargeMode, ChargeOnlyMode, DischargeOnlyMode)
        ! do nothing, these are okay
      CASE DEFAULT
        TESCoil(TESCoilNum)%CurControlMode = OffMode
        IF (TESCoil(TESCoilNum)%ControlModeErrorIndex == 0) THEN
          CALL ShowSevereMessage('InitTESCoil: Invalid control schedule value for operating mode')
          CALL ShowContinueError('Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = ' &
                            //TRIM(TESCoil(TESCoilNum)%Name) )
          CALL ShowContinueError('Value returned from schedule =' &
              //TRIM(RoundSigDigits(tmpSchedValue, 8)) )
          CALL ShowContinueError('Operating mode will be set to Off, and the simulation continues')
        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('InitTESCoil: Invalid control schedule value for TES operating mode, set to Off', &
                                  TESCoil(TESCoilNum)%ControlModeErrorIndex, &
                                  ReportMaxOf = tmpSchedValue, &
                                  ReportMinOf = tmpSchedValue)
      END SELECT

    ELSEIF (TESCoil(TESCoilNum)%ModeControlType == EMSActuatedOpModes) THEN
      IF (TESCoil(TESCoilNum)%EMSControlModeOn) THEN
        TESCoil(TESCoilNum)%CurControlMode = FLOOR(TESCoil(TESCoilNum)%EMSControlModeValue)
      ! check if value is valid
        SELECT CASE (TESCoil(TESCoilNum)%CurControlMode)
        CASE ( OffMode )

        CASE ( CoolingOnlyMode )
          IF (.NOT. ( TESCoil(TESCoilNum)%CoolingOnlyModeIsAvailable)) THEN
            CALL ShowSevereMessage('InitTESCoil: Invalid control value for operating mode')
            CALL ShowContinueError('Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = ' &
                              //TRIM(TESCoil(TESCoilNum)%Name) )
            CALL ShowContinueError('Value returned from EMS indicates Cooling Only Mode but that mode is not available.')
            CALL ShowContinueError('Operating mode will be set to Off, and the simulation continues')
            TESCoil(TESCoilNum)%CurControlMode = OffMode
          ENDIF
        CASE ( CoolingAndChargeMode )
          IF (.NOT. ( TESCoil(TESCoilNum)%CoolingAndChargeModeAvailable)) THEN
            CALL ShowSevereMessage('InitTESCoil: Invalid control value for operating mode')
            CALL ShowContinueError('Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = ' &
                              //TRIM(TESCoil(TESCoilNum)%Name) )
            CALL ShowContinueError('Value returned from EMS indicates Cooling And Charge Mode but that mode is not available.')
            CALL ShowContinueError('Operating mode will be set to Off, and the simulation continues')
            TESCoil(TESCoilNum)%CurControlMode = OffMode
          ENDIF
        CASE ( CoolingAndDischargeMode )
          IF (.NOT. ( TESCoil(TESCoilNum)%CoolingAndDischargeModeAvailable)) THEN
            CALL ShowSevereMessage('InitTESCoil: Invalid control value for operating mode')
            CALL ShowContinueError('Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = ' &
                              //TRIM(TESCoil(TESCoilNum)%Name) )
            CALL ShowContinueError('Value returned from EMS indicates Cooling And Discharge Mode but that mode is not available.')
            CALL ShowContinueError('Operating mode will be set to Off, and the simulation continues')
            TESCoil(TESCoilNum)%CurControlMode = OffMode
          ENDIF
        CASE ( ChargeOnlyMode )
          IF (.NOT. ( TESCoil(TESCoilNum)%ChargeOnlyModeAvailable)) THEN
            CALL ShowSevereMessage('InitTESCoil: Invalid control value for operating mode')
            CALL ShowContinueError('Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = ' &
                              //TRIM(TESCoil(TESCoilNum)%Name) )
            CALL ShowContinueError('Value returned from EMS indicates Charge Only Mode but that mode is not available.')
            CALL ShowContinueError('Operating mode will be set to Off, and the simulation continues')
            TESCoil(TESCoilNum)%CurControlMode = OffMode
          ENDIF
        CASE ( DischargeOnlyMode)
          IF (.NOT. ( TESCoil(TESCoilNum)%DischargeOnlyModeAvailable)) THEN
            CALL ShowSevereMessage('InitTESCoil: Invalid control value for operating mode')
            CALL ShowContinueError('Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = ' &
                              //TRIM(TESCoil(TESCoilNum)%Name) )
            CALL ShowContinueError('Value returned from EMS indicates Discharge Only Mode but that mode is not available.' )
            CALL ShowContinueError('Operating mode will be set to Off, and the simulation continues')
            TESCoil(TESCoilNum)%CurControlMode = OffMode
          ENDIF
        CASE DEFAULT
          TESCoil(TESCoilNum)%CurControlMode = OffMode
          IF (TESCoil(TESCoilNum)%ControlModeErrorIndex == 0) THEN
            CALL ShowSevereMessage('InitTESCoil: Invalid control value for operating mode')
            CALL ShowContinueError('Occurs for Coil:Cooling:DX:SingleSpeed:ThermalStorage name = ' &
                              //TRIM(TESCoil(TESCoilNum)%Name) )
            CALL ShowContinueError('Value returned from EMS =' &
                //TRIM(RoundSigDigits(TESCoil(TESCoilNum)%EMSControlModeValue, 8)) )
            CALL ShowContinueError('Operating mode will be set to Off, and the simulation continues')
          ENDIF
          CALL ShowRecurringSevereErrorAtEnd('InitTESCoil: Invalid control schedule value for TES operating mode, set to Off', &
                                    TESCoil(TESCoilNum)%ControlModeErrorIndex, &
                                    ReportMaxOf = TESCoil(TESCoilNum)%EMSControlModeValue, &
                                    ReportMinOf = TESCoil(TESCoilNum)%EMSControlModeValue)
        END SELECT
      ELSE
        TESCoil(TESCoilNum)%CurControlMode = OffMode
      ENDIF
    ENDIF
  ELSE
    TESCoil(TESCoilNum)%CurControlMode = OffMode
  ENDIF

  TESCoil(TESCoilNum)%QdotPlant  = 0.d0  ! heat exchange rate for plant connection to TES tank [W]
  TESCoil(TESCoilNum)%Q_Plant    = 0.d0  !  heat exchange energy for plant connection to TES tank [J]
  TESCoil(TESCoilNum)%QdotAmbient = 0.d0 ! heat exchange rate for skin losses/gains for TES tank to surroundings [W]
  TESCoil(TESCoilNum)%Q_Ambient = 0.d0 ! heat exchange enegy for skin losses/gains for TES tank to surroundings [J]
  TESCoil(TESCoilNum)%QdotTES  = 0.d0    ! heat exchange rate by mechanical systems to charge or discharge TES [W]
  TESCoil(TESCoilNum)%Q_TES    = 0.d0 ! heat exchange energy by mechanical systems to charge or discharge TES [J]

  ! dynamic calculated data
  TESCoil(TESCoilNum)%ElecCoolingPower       = 0.d0  ! electric power for cooling [W]
  TESCoil(TESCoilNum)%ElecCoolingEnergy      = 0.d0  ! electric energy for cooling [J], metered
  TESCoil(TESCoilNum)%EvapTotCoolingRate     = 0.d0  ! evaporator coil total cooling rate [W]
  TESCoil(TESCoilNum)%EvapTotCoolingEnergy   = 0.d0  ! evaporatory coil total cooling energy [J], metered
  TESCoil(TESCoilNum)%EvapSensCoolingRate    = 0.d0
  TESCoil(TESCoilNum)%EvapSensCoolingEnergy  = 0.d0
  TESCoil(TESCoilNum)%EvapLatCoolingRate     = 0.d0
  TESCoil(TESCoilNum)%EvapLatCoolingEnergy   = 0.d0
  TESCoil(TESCoilNum)%RuntimeFraction        = 0.d0
  TESCoil(TESCoilNum)%CondenserRuntimeFraction = 0.d0 !
  TESCoil(TESCoilNum)%ElectColdWeatherPower  = 0.d0  ! electric power for cold weather protection [W]
  TESCoil(TESCoilNum)%ElectColdWeatherEnergy = 0.d0  ! electric energy for cold weather protection [J], metered
  TESCoil(TESCoilNum)%ElectEvapCondBasinHeaterPower = 0.d0
  TESCoil(TESCoilNum)%ElectEvapCondBasinHeaterEnergy = 0.d0

  RETURN

END SUBROUTINE InitTESCoil

SUBROUTINE SizeTESCoil(TESCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   April 2013
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
  USE DataAirSystems, ONLY: PrimaryAirSystem
  USE DataEnvironment,   ONLY: StdRhoAir
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE CurveManager, ONLY: CurveValue
  USE DataGlobals,  ONLY: SecInHour, InitConvTemp
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: TESCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  RoutineName='SizeTESCoil '
  REAL(r64), PARAMETER        :: FluidTankSizingDeltaT = 10.d0
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
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
  INTEGER   :: TimeStepNumAtMax
  INTEGER   :: DDNum
  REAL(r64) :: rhoair
  REAL(r64) :: rho
  REAL(r64) :: deltaT
  REAL(r64) :: Cp

  IF (TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate == AutoSize) THEN

    IF (CurSysNum > 0) THEN
      CALL CheckSysSizing('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name )
      IF (CurOASysNum > 0) THEN
        TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
      ELSE
        TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
      ENDIF
    ELSE IF (CurZoneEqNum > 0) THEN
      CALL CheckZoneSizing('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name)
      TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate = &
               MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow,FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
    ENDIF

    IF (TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate < SmallAirVolFlow) THEN
      TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate = 0.d0
    ENDIF
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                                  'Rated Evaporator Air Flow Rate [m3/s]', TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate )
  ENDIF

  TESCoil(TESCoilNum)%RatedEvapAirMassFlowRate = StdRhoAir * TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate

  IF (TESCoil(TESCoilNum)%CondenserAirVolumeFlow == AutoCalculate) THEN
    TESCoil(TESCoilNum)%CondenserAirVolumeFlow = TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate &
                                   * TESCoil(TESCoilNum)%CondenserAirFlowSizingFactor
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                                  'Condenser Air Flow Rate [m3/s]', TESCoil(TESCoilNum)%CondenserAirVolumeFlow )
  ENDIF

  TESCoil(TESCoilNum)%CondenserAirMassFlow = StdRhoAir * TESCoil(TESCoilNum)%CondenserAirVolumeFlow


  IF (TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap == AutoSize) THEN
    IF (CurSysNum > 0) THEN
      CALL CheckSysSizing('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name )
      VolFlowRate = TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate
      IF (VolFlowRate >= SmallAirVolFlow) THEN
        IF (CurOASysNum > 0) THEN ! coil is in the OA stream
          MixTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
          MixHumRat = FinalSysSizing(CurSysNum)%CoolOutHumRat
          SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
          SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
        ELSE ! coil is on the main air loop
      !     MixTemp = FinalSysSizing(CurSysNum)%CoolMixTemp
      !     MixHumRat = FinalSysSizing(CurSysNum)%CoolMixHumRat
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
        rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
        TotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlyCapFTempCurve,MixWetBulb,OutTemp)
        CoolCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (MixEnth-SupEnth)))
        IF(TotCapTempModFac .GT. 0.d0)THEN
          TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap = CoolCapAtPeak / TotCapTempModFac
        ELSE
          TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap = CoolCapAtPeak
        END IF

      ELSE
        TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap = 0.0d0
      END IF
    ELSE IF (CurZoneEqNum > 0) THEN
      CALL CheckZoneSizing('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name)
      VolFlowRate = TESCoil(TESCoilNum)%RatedEvapAirVolFlowRate
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
        rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
        TotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlyCapFTempCurve,MixWetBulb,OutTemp)
        CoolCapAtPeak = MAX(0.d0, (rhoair * VolFlowRate * (MixEnth-SupEnth)))
        IF(TotCapTempModFac .GT. 0.d0)THEN
          TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap = CoolCapAtPeak / TotCapTempModFac
        ELSE
          TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap = CoolCapAtPeak
        END IF

      ELSE
        TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap= 0.d0
      END IF
    ENDIF

    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Cooling Only Mode Rated Total Evaporator Cooling Capacity [W]', &
                             TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap )

  ENDIF


  IF ( TESCoil(TESCoilNum)%CoolingAndChargeModeAvailable  &
      .AND. (TESCoil(TESCoilNum)%CoolingAndChargeRatedTotCap == AutoCalculate)) THEN
    TESCoil(TESCoilNum)%CoolingAndChargeRatedTotCap = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap &
                   * TESCoil(TESCoilNum)%CoolingAndChargeRatedTotCapSizingFactor
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Cooling And Charge Mode Rated Total Evaporator Cooling Capacity [W]', &
                             TESCoil(TESCoilNum)%CoolingAndChargeRatedTotCap )
  ENDIF

  IF ( TESCoil(TESCoilNum)%CoolingAndChargeModeAvailable  &
      .AND. (TESCoil(TESCoilNum)%CoolingAndChargeRatedChargeCap == AutoCalculate)) THEN
    TESCoil(TESCoilNum)%CoolingAndChargeRatedChargeCap = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap &
                   * TESCoil(TESCoilNum)%CoolingAndChargeRatedChargeCapSizingFactor
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Cooling And Charge Mode Rated Storage Charging Capacity [W]', &
                             TESCoil(TESCoilNum)%CoolingAndChargeRatedChargeCap )
  ENDIF

  IF ( TESCoil(TESCoilNum)%CoolingAndDischargeModeAvailable  &
      .AND. (TESCoil(TESCoilNum)%CoolingAndDischargeRatedTotCap == AutoCalculate)) THEN
    TESCoil(TESCoilNum)%CoolingAndDischargeRatedTotCap = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap &
                   * TESCoil(TESCoilNum)%CoolingAndDischargeRatedTotCapSizingFactor
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Cooling And Discharge Mode Rated Total Evaporator Cooling Capacity [W]', &
                             TESCoil(TESCoilNum)%CoolingAndDischargeRatedTotCap )
  ENDIF

  IF ( TESCoil(TESCoilNum)%CoolingAndDischargeModeAvailable  &
      .AND. (TESCoil(TESCoilNum)%CoolingAndDischargeRatedDischargeCap == AutoCalculate)) THEN
    TESCoil(TESCoilNum)%CoolingAndDischargeRatedDischargeCap = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap &
                   * TESCoil(TESCoilNum)%CoolingAndDischargeRatedDischargeCapSizingFactor
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Cooling And Discharge Mode Rated Storage Discharging Capacity [W]', &
                             TESCoil(TESCoilNum)%CoolingAndDischargeRatedDischargeCap )
  ENDIF

  IF ( TESCoil(TESCoilNum)%ChargeOnlyModeAvailable  &
        .AND. (TESCoil(TESCoilNum)%ChargeOnlyRatedCapacity == AutoCalculate)) THEN
    TESCoil(TESCoilNum)%ChargeOnlyRatedCapacity = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap &
                   * TESCoil(TESCoilNum)%ChargeOnlyRatedCapacitySizingFactor
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Charge Only Mode Rated Storage Charging Capacity [W]', &
                             TESCoil(TESCoilNum)%ChargeOnlyRatedCapacity )
  ENDIF

  IF ( TESCoil(TESCoilNum)%DischargeOnlyModeAvailable  &
        .AND. (TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCap == AutoCalculate)) THEN
    TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCap = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap &
                   * TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCapSizingFactor
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Discharge Only Mode Rated Storage Discharging Capacity [W]', &
                             TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCap )
  ENDIF

  IF (( TESCoil(TESCoilNum)%StorageMedia == FluidBased) &
      .AND. (TESCoil(TESCoilNum)%FluidStorageVolume == AutoCalculate)) THEN
    ! for fluid tanks, assume a 10C deltaT or diff between max and min, whichever is smaller
    deltaT = MIN( FluidTankSizingDeltaT, &
                 (TESCoil(TESCoilNum)%MaximumFluidTankTempLimit - TESCoil(TESCoilNum)%MinimumFluidTankTempLimit) )

    rho = GetDensityGlycol(TESCoil(TESCoilNum)%StorageFluidName, InitConvTemp, &
                           TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESWaterStorageTank')
    Cp  = GetSpecificHeatGlycol(TESCoil(TESCoilNum)%StorageFluidName, InitConvTemp, &
                                              TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESWaterStorageTank')
    IF (TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCap > 0.d0 .AND. TESCoil(TESCoilNum)%DischargeOnlyModeAvailable) THEN
      TESCoil(TESCoilNum)%FluidStorageVolume = (TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCap * &
                                               TESCoil(TESCoilNum)%StorageCapacitySizingFactor * SecInHour) &
                                               / (rho * Cp * deltaT)
    ELSE
      TESCoil(TESCoilNum)%FluidStorageVolume = (TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap * &
                                               TESCoil(TESCoilNum)%StorageCapacitySizingFactor * SecInHour) &
                                               / (rho * Cp * deltaT)
    ENDIF
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Fluid Storage Volume [m3]', &
                             TESCoil(TESCoilNum)%FluidStorageVolume )
  ENDIF
  IF (( TESCoil(TESCoilNum)%StorageMedia == IceBased) &
      .AND. (TESCoil(TESCoilNum)%IceStorageCapacity == AutoCalculate)) THEN

    IF (TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCap > 0.d0 .AND. TESCoil(TESCoilNum)%DischargeOnlyModeAvailable) THEN
      TESCoil(TESCoilNum)%IceStorageCapacity = TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCap &
                              * TESCoil(TESCoilNum)%StorageCapacitySizingFactor * SecInHour
    ELSE
      TESCoil(TESCoilNum)%IceStorageCapacity = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap &
                              * TESCoil(TESCoilNum)%StorageCapacitySizingFactor * SecInHour
    ENDIF
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Ice Storage Capacity [GJ]', &
                             TESCoil(TESCoilNum)%IceStorageCapacity / 1.d+09 )
  ENDIF

  IF ((TESCoil(TESCoilNum)%CondenserType == EvapCooled) .AND. (TESCoil(TESCoilNum)%EvapCondPumpElecNomPower == AutoSize)) THEN
    TESCoil(TESCoilNum)%EvapCondPumpElecNomPower = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap * 0.004266d0 ! w/w (15 w/ton)
    CALL ReportSizingOutput('Coil:Cooling:DX:SingleSpeed:ThermalStorage', TESCoil(TESCoilNum)%Name , &
                            'Evaporative Condenser Pump Rated Power Consumption [W]', &
                             TESCoil(TESCoilNum)%EvapCondPumpElecNomPower )

  ENDIF

  CALL PreDefTableEntry(pdchCoolCoilType,TESCoil(TESCoilNum)%Name,'Coil:Cooling:DX:SingleSpeed:ThermalStorage')

  CALL PreDefTableEntry(pdchCoolCoilTotCap,TESCoil(TESCoilNum)%Name,TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap)
  CALL PreDefTableEntry(pdchCoolCoilSensCap,TESCoil(TESCoilNum)%Name,TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap &
                          * TESCoil(TESCoilNum)%CoolingOnlyRatedSHR)
  CALL PreDefTableEntry(pdchCoolCoilLatCap,TESCoil(TESCoilNum)%Name,TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap &
                    - TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap * TESCoil(TESCoilNum)%CoolingOnlyRatedSHR)
  CALL PreDefTableEntry(pdchCoolCoilSHR,TESCoil(TESCoilNum)%Name,TESCoil(TESCoilNum)%CoolingOnlyRatedSHR)
  CALL PreDefTableEntry(pdchCoolCoilNomEff,TESCoil(TESCoilNum)%Name,TESCoil(TESCoilNum)%CoolingOnlyRatedCOP)


  RETURN

END SUBROUTINE SizeTESCoil

SUBROUTINE CalcTESCoilOffMode(TESCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TESCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: StandbyAncillaryPower

!coil is off; just pass through conditions
  IF (GetCurrentScheduleValue(TESCoil(TESCoilNum)%AvailSchedNum) /= 0.d0) THEN
    StandbyAncillaryPower = TESCoil(TESCoilNum)%AncillaryControlsPower
  ELSE
    StandbyAncillaryPower = 0.d0
  ENDIF

  TESCoil(TESCoilNum)%ElecCoolingPower   = StandbyAncillaryPower
  TESCoil(TESCoilNum)%ElecCoolingEnergy  = StandbyAncillaryPower * TimeStepSys * SecInHour

  Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
  Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
  Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
  Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum)%Enthalpy      = &
                                                          PsyHFnTdbW(Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp, &
                                                                      Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                                                                      'CalcTESCoilOffMode')

  Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
  Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
  Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate = 0.d0
  Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%MassFlowRate
  Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Enthalpy     = &
                                                          PsyHFnTdbW(Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp, &
                                                                      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat, &
                                                                      'CalcTESCoilOffMode')
  TESCoil(TESCoilNum)%RuntimeFraction      =  0.d0
  TESCoil(TESCoilNum)%EvapTotCoolingRate   =  0.d0
  TESCoil(TESCoilNum)%EvapTotCoolingEnergy =  0.d0
  TESCoil(TESCoilNum)%EvapSensCoolingRate  =  0.d0
  TESCoil(TESCoilNum)%EvapSensCoolingEnergy=  0.d0
  TESCoil(TESCoilNum)%EvapLatCoolingRate   =  0.d0
  TESCoil(TESCoilNum)%EvapLatCoolingEnergy =  0.d0

  TESCoil(TESCoilNum)%QdotTES              =  0.d0
  TESCoil(TESCoilNum)%Q_TES                =  0.d0

  CALL UpdateTEStorage(TESCoilNum)

  TESCoil(TESCoilNum)%CondInletTemp = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp

  CALL UpdateColdWeatherProtection(TESCoilNum)

  IF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CALL UpdateEvaporativeCondenserBasinHeater(TESCoilNum)
  ENDIF

  RETURN

END SUBROUTINE CalcTESCoilOffMode

SUBROUTINE CalcTESCoilCoolingOnlyMode(TESCoilNum, FanOpMode, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT (IN) :: TESCoilNum
  INTEGER , INTENT (IN) :: FanOpMode
  REAL(r64) , INTENT (IN) :: PartLoadRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER :: MaxIter = 30
  REAL(r64), PARAMETER :: RelaxationFactor = 0.4d0
  REAL(r64), PARAMETER :: Tolerance = 0.1d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CondInletTemp         ! Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
                                   ! Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
  REAL(r64) :: CondInletHumrat      ! Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
                                    ! For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
  REAL(r64) :: CondAirMassFlow      ! Condenser air mass flow rate [kg/s]
  REAL(r64) :: CondInletEnthalpy    ! condenser inlet enthalpy [J/kg]
  REAL(r64) :: CondAirSidePressure  ! Outdoor barometric pressure at condenser (Pa)
  REAL(r64) :: QdotCond  ! condenser total heat rejection rate [W]
  REAL(r64) :: CondOutletEnthalpy   !condesner outlet enthalpy [J/kg]
  REAL(r64) :: OutdoorDryBulb  ! outdoor air dry bulb local variable [C]
  REAL(r64) :: OutdoorHumRat   ! outdoor air humidity ratio local [kg/kg]
  REAL(r64) :: OutdoorWetBulb  ! outdoor air wetbulb local [C]
  REAL(r64) :: EvapAirMassFlow   ! local for evaporator air mass flow [kg/s]
  REAL(r64) :: EvapInletDryBulb ! evaporator inlet air drybulb [C]
  REAL(r64) :: EvapInletHumRat ! evaporator inlet air humidity ratio [kg/kg]
  REAL(r64) :: EvapInletWetBulb ! evaporator inlet air wetbulb [C]
  REAL(r64) :: EvapInletEnthalpy ! evaporator inlet air enthalpy [J/kg]
  REAL(r64) :: AirMassFlowRatio ! evaporator inlet air mass flow divided by design mass flow [ ]
  REAL(r64) :: TotCapTempModFac ! total coolin capacity modification factor due to temps []
  REAL(r64) :: TotCapFlowModFac !Total cooling capacity modification factor due to flow []
  REAL(r64) :: TotCap ! total cooling capacity
  REAL(r64) :: SHRTempFac ! sensible heat ratio modification factor due to temps []
  REAL(r64) :: SHRFlowFac ! sensible heat ratio modification factor due to flow []
  REAL(r64) :: SHR ! sensible heat ratio
  REAL(r64) :: PLF   ! part load factor
  REAL(r64) :: RuntimeFraction ! compressor running time divided by full time of timestep.
  REAL(r64) :: FullLoadOutAirEnth ! evaporator outlet full load enthalpy [J/kg]
  REAL(r64) :: hTinwout   ! Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
  REAL(r64) :: FullLoadOutAirHumRat ! evaporator outlet humidity ratio at full load
  REAL(r64) :: FullLoadOutAirTemp  !evaporator outlet air temperature at full load [C]
  REAL(r64) :: EvapOutletAirEnthalpy ! evaporator outlet air enthalpy [J/kg]
  REAL(r64) :: EvapOutletAirHumRat !evaporator outlet air humidity ratio [kg/kg]
  REAL(r64) :: EvapOutletAirTemp !evaporator outlet dryblub [C]
  REAL(r64) :: EIRTempModFac ! energy input ratio modification factor due to temperatures []
  REAL(r64) :: EIRFlowModFac !energy input ratio modification factor due to flow []
  REAL(r64) :: EIR ! energy input ratio
  REAL(r64) :: ElecCoolingPower ! compressor electric power
  REAL(r64) :: MinAirHumRat ! minimum air humidity ratio
  REAL(r64) :: PartLoadOutAirEnth ! local leaving enthalpy at part load
  REAL(r64) :: PartLoadDryCoilOutAirTemp ! local leaving drybulb if coil were dry
  LOGICAL   :: CoilMightBeDry
  INTEGER   :: Counter
  LOGICAL   :: Converged
  REAL(r64) :: DryCoilTestEvapInletHumRat
  REAL(r64) :: DryCoilTestEvapInletWetBulb
  REAL(r64) :: hADP
  REAL(r64) :: tADP
  REAL(r64) :: wADP
  REAL(r64) :: hTinwADP
  REAL(r64) :: SHRadp
  REAL(r64) :: werror

  ! first deal with condenser
  IF (TESCoil(TESCoilNum)%CondenserType == AirCooled) THEN
    CondAirSidePressure = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Press
    IF(CondAirSidePressure == DefaultNodeValues%Press)THEN
      CondInletTemp    = OutDryBulbTemp
      CondInletHumrat  = OutHumRat
      CondAirSidePressure     = OutBaroPress
    ELSE
     CondInletTemp = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
     CondInletHumrat = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat

    ENDIF
    CondAirMassFlow = TESCoil(TESCoilNum)%CondenserAirMassFlow
  ELSEIF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CondAirSidePressure = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Press
    IF(CondAirSidePressure == DefaultNodeValues%Press)THEN
      OutdoorDryBulb    = OutDryBulbTemp
      OutdoorHumRat  = OutHumRat
      CondAirSidePressure     = OutBaroPress
      OutdoorWetBulb   = OutWetBulbTemp
    ELSE
     OutdoorDryBulb = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
     OutdoorHumRat = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
     OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, 'CalcTESCoilCoolingOnlyMode')
    ENDIF
    CondAirMassFlow = TESCoil(TESCoilNum)%CondenserAirMassFlow
    ! direct evap cool model
    CondInletTemp   = OutdoorWetBulb + (OutdoorDryBulb-OutdoorWetBulb)*(1.0d0 - TESCoil(TESCoilNum)%EvapCondEffect)
    CondInletHumrat = PsyWFnTdbTwbPb(CondInletTemp,OutdoorWetBulb,CondAirSidePressure, 'CalcTESCoilCoolingOnlyMode')
  ENDIF

  EvapAirMassFlow = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
  EvapInletDryBulb = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
  EvapInletHumRat  = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
  EvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, EvapInletHumRat, OutBaroPress, 'CalcTESCoilCoolingOnlyMode')
  EvapInletEnthalpy = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Enthalpy
  CoilMightBeDry = .FALSE.

  IF ((EvapAirMassFlow > SmallMassFlow) .AND. (PartLoadRatio > 0.d0)) THEN ! coil is running

    AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum)%RatedEvapAirMassFlowRate
    TotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlyCapFTempCurve, EvapInletWetBulb, CondInletTemp)
    TotCapTempModFac = MAX(0.d0, TotCapTempModFac) ! could warn if negative, DXcoil does
    TotCapFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlyCapFFlowCurve, AirMassFlowRatio)
    TotCapFlowModFac = MAX(0.d0, TotCapFlowModFac)  ! could warn if negative, DXcoil does
    TotCap = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap * TotCapTempModFac * TotCapFlowModFac
    ! now see if coil might be running dry
    PartLoadOutAirEnth =  EvapInletEnthalpy - (TotCap * PartLoadRatio) / EvapAirMassFlow
    PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat,'CalcTESCoilCoolingOnlyMode')
    IF (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(PartLoadOutAirEnth,OutBaroPress, 'CalcTESCoilCoolingOnlyMode')) THEN
      CoilMightBeDry = .TRUE.
      ! find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
      DryCoilTestEvapInletHumRat = EvapInletHumRat
      DryCoilTestEvapInletWetBulb = EvapInletWetBulb
      counter = 0
      Converged = .FALSE.
      DO While (.NOT. Converged)
        TotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlyCapFTempCurve, DryCoilTestEvapInletWetBulb, CondInletTemp)
        TotCapTempModFac = MAX(0.d0, TotCapTempModFac) ! could warn if negative, DXcoil does
        TotCapFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlyCapFFlowCurve, AirMassFlowRatio)
        TotCapFlowModFac = MAX(0.d0, TotCapFlowModFac)  ! could warn if negative, DXcoil does
        TotCap = TESCoil(TESCoilNum)%CoolingOnlyRatedTotCap * TotCapTempModFac * TotCapFlowModFac

        ! coil bypass factor = 0.0
        hADP = EvapInletEnthalpy - (TotCap / EvapAirMassFlow)
        tADP = PsyTsatFnHPb(hADP, OutBaroPress, 'CalcTESCoilCoolingOnlyMode')
        wADP = MIN(EvapInletHumRat, PsyWfnTdbH(tADP, hADP, 'CalcTESCoilCoolingOnlyMode') )
        hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP, 'CalcTESCoilCoolingOnlyMode')
        IF ((EvapInletEnthalpy - hADP) > 1.d-10) THEN
          SHRadp = MIN((hTinwADP-hADP)/(EvapInletEnthalpy-hADP),1.d0)
        ELSE
          SHRadp = 1.d0
        ENDIF

        IF ((wADP > DryCoilTestEvapInletHumRat) .or. (Counter .ge. 1 .and. Counter .lt. MaxIter)) THEN
          IF (DryCoilTestEvapInletHumRat <= 0.d0) DryCoilTestEvapInletHumRat = 0.00001d0
          werror = (DryCoilTestEvapInletHumRat -  wADP)/DryCoilTestEvapInletHumRat

          DryCoilTestEvapInletHumRat = RelaxationFactor*wADP + (1.d0 - RelaxationFactor)*DryCoilTestEvapInletHumRat
          DryCoilTestEvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb , DryCoilTestEvapInletHumRat, OutBaroPress, &
                                                    'CalcTESCoilCoolingOnlyMode')

          Counter = Counter + 1
          IF (ABS(werror) <= Tolerance) THEN
            Converged = .TRUE.
          ELSE
            Converged = .FALSE.
          ENDIF
        ELSE
          Converged = .TRUE.
        ENDIF

      ENDDO
    ENDIF

    SHRTempFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlySHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb)
    SHRFlowFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlySHRFFlowCurve, AirMassFlowRatio)
    SHR = TESCoil(TESCoilNum)%CoolingOnlyRatedSHR * SHRTempFac * SHRFlowFac
    SHR = MIN(SHR, 1.d0) ! warn maybe
    SHR = MAX(SHR, 0.d0) ! warn maybe
    IF ( CoilMightBeDry) THEN
      IF ((EvapInletHumRat < DryCoilTestEvapInletHumRat) .AND. (SHRadp > SHR)) THEN ! coil is dry for sure
        SHR = 1.0d0
      ELSEIF (SHRadp > SHR) THEN
        SHR = SHRadp
      ENDIF
    ENDIF
    PLF = CurveValue(TESCoil(TESCoilNum)%CoolingOnlyPLFFPLRCurve, PartLoadRatio)
    IF (PLF >= PartLoadRatio .and. PLF > 0.d0 ) THEN
      RuntimeFraction = PartLoadRatio / PLF
    ELSE
      RuntimeFraction = 1.d0 ! warn maybe
    ENDIF
    !  Calculate full load output conditions
    FullLoadOutAirEnth = EvapInletEnthalpy - TotCap / EvapAirMassFlow

    hTinwout = EvapInletEnthalpy - (1.0d0-SHR)* (TotCap / EvapAirMassFlow)
    !The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
    FullLoadOutAirHumRat = PsyWFnTdbH(EvapInletDryBulb,hTinwout, 'CalcTESCoilCoolingOnlyMode', SuppressWarnings = .TRUE.)
    FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth,FullLoadOutAirHumRat, 'CalcTESCoilCoolingOnlyMode')
    ! Check for saturation error and modify temperature at constant enthalpy
    IF(FullLoadOutAirTemp .LT. PsyTsatFnHPb(FullLoadOutAirEnth,OutBaroPress, 'CalcTESCoilCoolingOnlyMode')) THEN
      FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,OutBaroPress, 'CalcTESCoilCoolingOnlyMode')
      FullLoadOutAirHumRat  = PsyWFnTdbH(FullLoadOutAirTemp,FullLoadOutAirEnth, 'CalcTESCoilCoolingOnlyMode')
    ENDIF

    ! Continuous fan, cycling compressor
    EvapOutletAirEnthalpy = ((PartLoadRatio)*FullLoadOutAirEnth + &
                                            (1.d0-(PartLoadRatio ))*EvapInletEnthalpy)
    EvapOutletAirHumRat = ((PartLoadRatio)*FullLoadOutAirHumRat + &
                                            (1.d0-(PartLoadRatio ))*EvapInletHumRat)
    EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy,EvapOutletAirHumRat)
    IF(EvapOutletAirTemp .LT. PsyTsatFnHPb(EvapOutletAirEnthalpy,OutBaroPress, 'CalcTESCoilCoolingOnlyMode')) THEN
      EvapOutletAirTemp = PsyTsatFnHPb(EvapOutletAirEnthalpy,OutBaroPress, 'CalcTESCoilCoolingOnlyMode')
      EvapOutletAirHumRat  = PsyWFnTdbH(EvapOutletAirTemp,EvapOutletAirEnthalpy, 'CalcTESCoilCoolingOnlyMode')
    ENDIF
    ! Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
    EIRTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlyEIRFTempCurve, EvapInletWetBulb, CondInletTemp)
    EIRTempModFac = MAX(EIRTempModFac, 0.d0)
    EIRFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingOnlyEIRFFlowCurve, AirMassFlowRatio)
    EIRFlowModFac = MAX(EIRFlowModFac, 0.d0)
    EIR = EIRTempModFac * EIRFlowModFac / TESCoil(TESCoilNum)%CoolingOnlyRatedCOP

    ElecCoolingPower = TotCap * EIR * RuntimeFraction

    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = EvapOutletAirTemp
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = EvapOutletAirHumRat
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Enthalpy     = EvapOutletAirEnthalpy
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = EvapAirMassFlow

    ! determine condenser leaving conditions
    QdotCond = TotCap* RuntimeFraction + ElecCoolingPower
    Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
    CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat , 'CalcTESCoilCoolingOnlyMode')
    CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum)%CondenserAirMassFlow
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Temp = PsyTdbFnHW( CondOutletEnthalpy, CondInletHumrat, &
                                                                        'CalcTESCoilCoolingOnlyMode')
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%HumRat   = CondInletHumrat
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Enthalpy = CondOutletEnthalpy

    TESCoil(TESCoilNum)%ElecCoolingPower = ElecCoolingPower + TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour

    TESCoil(TESCoilNum)%RuntimeFraction = RuntimeFraction
    TESCoil(TESCoilNum)%CondenserRuntimeFraction    = RuntimeFraction
    TESCoil(TESCoilNum)%EvapTotCoolingRate = TotCap* RuntimeFraction ! double check this
    TESCoil(TESCoilNum)%EvapTotCoolingEnergy = TotCap* RuntimeFraction * TimeStepSys  *SecInHour
    MinAirHumRat = MIN( Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                        Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat )
    TESCoil(TESCoilNum)%EvapSensCoolingRate  = EvapAirMassFlow *&
                      (PsyHFnTdbW(EvapInletDryBulb , MinAirHumRat, 'CalcTESCoilCoolingOnlyMode') - &
                        PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat, 'CalcTESCoilCoolingOnlyMode') )
    IF (TESCoil(TESCoilNum)%EvapSensCoolingRate  > TESCoil(TESCoilNum)%EvapTotCoolingRate) THEN
      TESCoil(TESCoilNum)%EvapSensCoolingRate = TESCoil(TESCoilNum)%EvapTotCoolingRate
    ENDIF
    TESCoil(TESCoilNum)%EvapSensCoolingEnergy=  TESCoil(TESCoilNum)%EvapSensCoolingRate * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%EvapLatCoolingRate   = TESCoil(TESCoilNum)%EvapTotCoolingRate &
                                              - TESCoil(TESCoilNum)%EvapSensCoolingRate
    TESCoil(TESCoilNum)%EvapLatCoolingEnergy =  TESCoil(TESCoilNum)%EvapLatCoolingRate * TimeStepSys  *SecInHour

  ELSE !coil is off; just pass through conditions
    TESCoil(TESCoilNum)%ElecCoolingPower   = TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy  = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%RuntimeFraction    = 0.d0
    TESCoil(TESCoilNum)%CondenserRuntimeFraction    = 0.d0
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum)%Enthalpy      = &
                                                            PsyHFnTdbW(Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp, &
                                                                       Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                                                                       'CalcTESCoilCoolingOnlyMode')

    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
    Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate = 0.d0
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%MassFlowRate
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Enthalpy     = &
                                                            PsyHFnTdbW(Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp, &
                                                                       Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat, &
                                                                       'CalcTESCoilCoolingOnlyMode')
    TESCoil(TESCoilNum)%EvapTotCoolingRate   =  0.d0
    TESCoil(TESCoilNum)%EvapTotCoolingEnergy =  0.d0
    TESCoil(TESCoilNum)%EvapSensCoolingRate  =  0.d0
    TESCoil(TESCoilNum)%EvapSensCoolingEnergy=  0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingRate   =  0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingEnergy =  0.d0



  ENDIF

  TESCoil(TESCoilNum)%QdotTES              =  0.d0
  TESCoil(TESCoilNum)%Q_TES                =  0.d0

  CALL UpdateTEStorage(TESCoilNum)

  TESCoil(TESCoilNum)%CondInletTemp = CondInletTemp

  CALL UpdateColdWeatherProtection(TESCoilNum)

  IF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CALL UpdateEvaporativeCondenserBasinHeater(TESCoilNum)
    CALL UpdateEvaporativeCondenserWaterUse(TESCoilNum, CondInletHumrat, TESCoil(TESCoilNum)%CondAirInletNodeNum )
  ENDIF

  RETURN

END SUBROUTINE CalcTESCoilCoolingOnlyMode

SUBROUTINE CalcTESCoilCoolingAndChargeMode(TESCoilNum, FanOpMode, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE FluidProperties,     ONLY: GetSpecificHeatGlycol, GetDensityGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT (IN) :: TESCoilNum
  INTEGER , INTENT (IN) :: FanOpMode
  REAL(r64) , INTENT (IN) :: PartLoadRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER :: MaxIter = 30
  REAL(r64), PARAMETER :: RelaxationFactor = 0.4d0
  REAL(r64), PARAMETER :: Tolerance = 0.1d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CondInletTemp         ! Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
                                   ! Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
  REAL(r64) :: CondInletHumrat      ! Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
                                    ! For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
  REAL(r64) :: CondAirMassFlow      ! Condenser air mass flow rate [kg/s]
  REAL(r64) :: CondInletEnthalpy    ! condenser inlet enthalpy [J/kg]
  REAL(r64) :: CondAirSidePressure  ! Outdoor barometric pressure at condenser (Pa)
  REAL(r64) :: QdotCond  ! condenser total heat rejection rate [W]
  REAL(r64) :: CondOutletEnthalpy   !condesner outlet enthalpy [J/kg]
  REAL(r64) :: OutdoorDryBulb  ! outdoor air dry bulb local variable [C]
  REAL(r64) :: OutdoorHumRat   ! outdoor air humidity ratio local [kg/kg]
  REAL(r64) :: OutdoorWetBulb  ! outdoor air wetbulb local [C]
  REAL(r64) :: EvapAirMassFlow   ! local for evaporator air mass flow [kg/s]
  REAL(r64) :: EvapInletDryBulb ! evaporator inlet air drybulb [C]
  REAL(r64) :: EvapInletHumRat ! evaporator inlet air humidity ratio [kg/kg]
  REAL(r64) :: EvapInletWetBulb ! evaporator inlet air wetbulb [C]
  REAL(r64) :: EvapInletEnthalpy ! evaporator inlet air enthalpy [J/kg]
  REAL(r64) :: AirMassFlowRatio ! evaporator inlet air mass flow divided by design mass flow [ ]
  REAL(r64) :: EvapTotCapTempModFac ! total coolin capacity modification factor due to temps []
  REAL(r64) :: EvapTotCapFlowModFac !Total cooling capacity modification factor due to flow []
  REAL(r64) :: EvapTotCap ! total cooling capacity
  REAL(r64) :: SHRTempFac ! sensible heat ratio modification factor due to temps []
  REAL(r64) :: SHRFlowFac ! sensible heat ratio modification factor due to flow []
  REAL(r64) :: SHR ! sensible heat ratio
  REAL(r64) :: PLF   ! part load factor
  REAL(r64) :: EvapRuntimeFraction ! compressor running time divided by full time of timestep.
  REAL(r64) :: FullLoadOutAirEnth ! evaporator outlet full load enthalpy [J/kg]
  REAL(r64) :: hTinwout   ! Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
  REAL(r64) :: FullLoadOutAirHumRat ! evaporator outlet humidity ratio at full load
  REAL(r64) :: FullLoadOutAirTemp  !evaporator outlet air temperature at full load [C]
  REAL(r64) :: EvapOutletAirEnthalpy ! evaporator outlet air enthalpy [J/kg]
  REAL(r64) :: EvapOutletAirHumRat !evaporator outlet air humidity ratio [kg/kg]
  REAL(r64) :: EvapOutletAirTemp !evaporator outlet dryblub [C]
  REAL(r64) :: EIRTempModFac ! energy input ratio modification factor due to temperatures []
  REAL(r64) :: EIRFlowModFac !energy input ratio modification factor due to flow []
  REAL(r64) :: EIR ! energy input ratio
  REAL(r64) :: EvapElecCoolingPower ! compressor electric power
  REAL(r64) :: MinAirHumRat ! minimum air humidity ratio
  REAL(r64) :: sTES ! stat of Thermal energy storage [C or fraction of ice]
  LOGICAL   :: TESCanBeCharged
  REAL(r64) :: rho
  REAL(r64) :: TankMass            ! Mass of fluid in tank (kg)
  REAL(r64) :: CpTank              ! Specific heat of water in tank (J/kg K)
  REAL(r64) :: QdotChargeLimit ! limit for charge cooling power to hit limit of storage.
  REAL(r64) :: ChargeCapModFac
  REAL(r64) :: ChargeCapPLRModFac
  REAL(r64) :: TotChargeCap
  REAL(r64) :: ChargeEIRTempModFac
  REAL(r64) :: ChargeEIRFlowModFac
  REAL(r64) :: ChargeEIR
  REAL(r64) :: ChargeElectricCoolingPower
  REAL(r64) :: ChargeRuntimeFraction
  REAL(r64) :: PartLoadOutAirEnth ! local leaving enthalpy at part load
  REAL(r64) :: PartLoadDryCoilOutAirTemp ! local leaving drybulb if coil were dry
  LOGICAL   :: CoilMightBeDry
  INTEGER   :: Counter
  LOGICAL   :: Converged
  REAL(r64) :: DryCoilTestEvapInletHumRat
  REAL(r64) :: DryCoilTestEvapInletWetBulb
  REAL(r64) :: hADP
  REAL(r64) :: tADP
  REAL(r64) :: wADP
  REAL(r64) :: hTinwADP
  REAL(r64) :: SHRadp
  REAL(r64) :: werror

  ! first deal with condenser
  IF (TESCoil(TESCoilNum)%CondenserType == AirCooled) THEN
    CondAirSidePressure = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Press
    IF(CondAirSidePressure == DefaultNodeValues%Press)THEN
      CondInletTemp    = OutDryBulbTemp
      CondInletHumrat  = OutHumRat
      CondAirSidePressure     = OutBaroPress
    ELSE
     CondInletTemp = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
     CondInletHumrat = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat

    ENDIF
    CondAirMassFlow = TESCoil(TESCoilNum)%CondenserAirMassFlow
  ELSEIF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CondAirSidePressure = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Press
    IF(CondAirSidePressure == DefaultNodeValues%Press)THEN
      OutdoorDryBulb    = OutDryBulbTemp
      OutdoorHumRat  = OutHumRat
      CondAirSidePressure     = OutBaroPress
      OutdoorWetBulb   = OutWetBulbTemp
    ELSE
     OutdoorDryBulb = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
     OutdoorHumRat = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
     OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, 'CalcTESCoilCoolingAndChargeMode')
    ENDIF
    CondAirMassFlow = TESCoil(TESCoilNum)%CondenserAirMassFlow
    ! direct evap cool model
    CondInletTemp   = OutdoorWetBulb + (OutdoorDryBulb-OutdoorWetBulb)*(1.0d0 - TESCoil(TESCoilNum)%EvapCondEffect)
    CondInletHumrat = PsyWFnTdbTwbPb(CondInletTemp,OutdoorWetBulb,CondAirSidePressure, 'CalcTESCoilCoolingAndChargeMode')
  ENDIF

  EvapAirMassFlow = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
  EvapInletDryBulb = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
  EvapInletHumRat  = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
  EvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, EvapInletHumRat, OutBaroPress, 'CalcTESCoilCoolingAndChargeMode')
  EvapInletEnthalpy = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Enthalpy
  CoilMightBeDry = .FALSE.

  IF (TESCoil(TESCoilNum)%StorageMedia == FluidBased) THEN
    sTES = TESCoil(TESCoilNum)%FluidTankTempFinalLastTimestep
    IF ((sTES > TESCoil(TESCoilNum)%MinimumFluidTankTempLimit) .AND. (sTES < TESCoil(TESCoilNum)%MaximumFluidTankTempLimit)) THEN
      TESCanBeCharged = .TRUE.
      !find charge limit to reach limits
      rho             = GetDensityGlycol(TESCoil(TESCoilNum)%StorageFluidName, sTES, &
                                         TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESCoilCoolingAndChargeMode')
      TankMass        = rho * TESCoil(TESCoilNum)%FluidStorageVolume
      CpTank          = GetSpecificHeatGlycol(TESCoil(TESCoilNum)%StorageFluidName, sTES, &
                                              TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESCoilCoolingAndChargeMode')
      !simple linear approximation of DT/Dt term in McpDT/Dt
      QdotChargeLimit = TankMass * CpTank * (sTES - TESCoil(TESCoilNum)%MinimumFluidTankTempLimit) &
                                                  / (TimeStepSys * SecInHour)
    ELSE
      TESCanBeCharged = .FALSE.
    ENDIF
  ELSEIF (TESCoil(TESCoilNum)%StorageMedia == IceBased) THEN
    sTES = TESCoil(TESCoilNum)%IceFracRemainLastTimestep
    If (sTES < 1.d0 ) THEN
      TESCanBeCharged = .TRUE.
      !find charge limit to reach limit
      QdotChargeLimit = (1.d0 - sTES) * TESCoil(TESCoilNum)%IceStorageCapacity / (TimeStepSys * SecInHour)
    ELSE
      TESCanBeCharged = .FALSE.
    ENDIF
  ENDIF


  IF ((EvapAirMassFlow > SmallMassFlow) .AND. (PartLoadRatio > 0.d0)) THEN ! coil is running

    AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum)%RatedEvapAirMassFlowRate
    EvapTotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeCoolingCapFTempCurve, &
                                           EvapInletWetBulb, CondInletTemp, sTES)
    EvapTotCapTempModFac = MAX(0.d0, EvapTotCapTempModFac) ! could warn if negative, DXcoil does
    EvapTotCapFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeCoolingCapFFlowCurve, AirMassFlowRatio)
    EvapTotCapFlowModFac = MAX(0.d0, EvapTotCapFlowModFac)  ! could warn if negative, DXcoil does
    EvapTotCap = TESCoil(TESCoilNum)%CoolingAndChargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac
    ! now see if coil is running dry
    PartLoadOutAirEnth =  EvapInletEnthalpy - (EvapTotCap * PartLoadRatio) / EvapAirMassFlow
    PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat,'CalcTESCoilCoolingAndChargeMode')
    IF (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(PartLoadOutAirEnth,OutBaroPress, 'CalcTESCoilCoolingAndChargeMode')) THEN
      CoilMightBeDry = .TRUE.
      ! find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
      DryCoilTestEvapInletHumRat = EvapInletHumRat
      DryCoilTestEvapInletWetBulb = EvapInletWetBulb
      counter = 0
      Converged = .FALSE.
      DO While (.NOT. Converged)
        EvapTotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeCoolingCapFTempCurve, &
                                               DryCoilTestEvapInletWetBulb, CondInletTemp, sTES)
        EvapTotCapTempModFac = MAX(0.d0, EvapTotCapTempModFac) ! could warn if negative, DXcoil does
        EvapTotCapFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeCoolingCapFFlowCurve, AirMassFlowRatio)
        EvapTotCapFlowModFac = MAX(0.d0, EvapTotCapFlowModFac)  ! could warn if negative, DXcoil does
        EvapTotCap = TESCoil(TESCoilNum)%CoolingAndChargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac
        ! coil bypass factor = 0.0
        hADP = EvapInletEnthalpy - (EvapTotCap / EvapAirMassFlow)
        tADP = PsyTsatFnHPb(hADP, OutBaroPress, 'CalcTESCoilCoolingAndChargeMode')
        wADP = MIN(EvapInletHumRat, PsyWfnTdbH(tADP, hADP, 'CalcTESCoilCoolingAndChargeMode') )
        hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP, 'CalcTESCoilCoolingAndChargeMode')
        IF ((EvapInletEnthalpy - hADP) > 1.d-10) THEN
          SHRadp = MIN((hTinwADP-hADP)/(EvapInletEnthalpy-hADP),1.d0)
        ELSE
          SHRadp = 1.d0
        ENDIF

        IF ((wADP > DryCoilTestEvapInletHumRat) .or. (Counter .ge. 1 .and. Counter .lt. MaxIter)) THEN
          IF (DryCoilTestEvapInletHumRat <= 0.d0) DryCoilTestEvapInletHumRat = 0.00001d0
          werror = (DryCoilTestEvapInletHumRat -  wADP)/DryCoilTestEvapInletHumRat

          DryCoilTestEvapInletHumRat = RelaxationFactor*wADP + (1.d0 - RelaxationFactor)*DryCoilTestEvapInletHumRat
          DryCoilTestEvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb , DryCoilTestEvapInletHumRat, OutBaroPress, &
                                                    'CalcTESCoilCoolingAndChargeMode')

          Counter = Counter + 1
          IF (ABS(werror) <= Tolerance) THEN
            Converged = .TRUE.
          ELSE
            Converged = .FALSE.
          ENDIF
        ELSE
          Converged = .TRUE.
        ENDIF

      ENDDO

    ENDIF
    SELECT CASE (TESCoil(TESCoilNum)%CoolingAndChargeSHRFTempObjectNum)
    CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV )
      SHRTempFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeSHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb)
    CASE (CurveType_TriQuadratic, CurveType_TableMultiIV )
      SHRTempFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeSHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb, sTES)
    END SELECT
    SHRFlowFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeSHRFFlowCurve, AirMassFlowRatio)
    SHR = TESCoil(TESCoilNum)%CoolingAndChargeRatedSHR * SHRTempFac * SHRFlowFac
    SHR = MIN(SHR, 1.d0) ! warn maybe
    SHR = MAX(SHR, 0.d0) ! warn maybe
    IF ( CoilMightBeDry) THEN
      IF ((EvapInletHumRat < DryCoilTestEvapInletHumRat) .AND. (SHRadp > SHR)) THEN ! coil is dry for sure
        SHR = 1.0d0
      ELSEIF (SHRadp > SHR) THEN
        SHR = SHRadp
      ENDIF
    ENDIF
    PLF = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeCoolingPLFFPLRCurve, PartLoadRatio)
    IF (PLF >= PartLoadRatio .and. PLF > 0.d0 ) THEN
      EvapRuntimeFraction = PartLoadRatio / PLF
    ELSE
      EvapRuntimeFraction = 1.d0 ! warn maybe
    ENDIF

    ! Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
    EIRTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeCoolingEIRFTempCurve, EvapInletWetBulb, CondInletTemp, sTES)
    EIRTempModFac = MAX(EIRTempModFac, 0.d0)
    EIRFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeCoolingEIRFFlowCurve, AirMassFlowRatio)
    EIRFlowModFac = MAX(EIRFlowModFac, 0.d0)
    EIR = EIRTempModFac * EIRFlowModFac / TESCoil(TESCoilNum)%CoolingAndChargeCoolingRatedCOP

    EvapElecCoolingPower = EvapTotCap * EIR * EvapRuntimeFraction

    IF (TESCanBeCharged) THEN
      ChargeCapModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeChargingCapFTempCurve, &
                                        EvapInletWetBulb, CondInletTemp , sTES)
      ChargeCapModFac = MAX(0.d0, ChargeCapModFac)

      ChargeCapPLRModFac= CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeChargingCapFEvapPLRCurve, PartLoadRatio)
      ChargeCapPLRModFac = MAX(0.d0, ChargeCapPLRModFac)

      TotChargeCap = TESCoil(TESCoilNum)%CoolingAndChargeRatedChargeCap * ChargeCapModFac * ChargeCapPLRModFac
      IF (TotChargeCap > QdotChargeLimit) THEN
        ChargeRuntimeFraction = QdotChargeLimit / TotChargeCap
        TotChargeCap = MIN(TotChargeCap, QdotChargeLimit)
      ELSE
        ChargeRuntimeFraction = 1.d0
      ENDIF
      ChargeEIRTempModFac = CurveValue( TESCoil(TESCoilNum)%CoolingAndChargeChargingEIRFTempCurve,&
                                        EvapInletWetBulb,CondInletTemp , sTES)
      ChargeEIRTempModFac = MAX(0.d0, ChargeEIRTempModFac )
      ChargeEIRFlowModFac = CurveValue( TESCoil(TESCoilNum)%CoolingAndChargeChargingEIRFFLowCurve, AirMassFlowRatio)
      ChargeEIRFlowModFac = MAX (0.d0,ChargeEIRFlowModFac)
      ChargeEIR = (ChargeEIRTempModFac * ChargeEIRFlowModFac) /   TESCoil(TESCoilNum)%CoolingAndChargeChargingRatedCOP
      ChargeElectricCoolingPower = TotChargeCap * ChargeEIR
      TESCoil(TESCoilNum)%QdotTES = - TotChargeCap
    ELSE
      TotChargeCap = 0.d0
      ChargeElectricCoolingPower =0.d0
      TESCoil(TESCoilNum)%QdotTES = 0.d0
      ChargeRuntimeFraction = 0.d0
    ENDIF

    !  Calculate full load output conditions
    FullLoadOutAirEnth = EvapInletEnthalpy - EvapTotCap / EvapAirMassFlow

    hTinwout = EvapInletEnthalpy - (1.0d0-SHR)* (EvapTotCap / EvapAirMassFlow)
    !The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
    FullLoadOutAirHumRat = PsyWFnTdbH(EvapInletDryBulb,hTinwout, 'CalcTESCoilCoolingAndChargeMode', SuppressWarnings = .TRUE.)
    FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth,FullLoadOutAirHumRat, 'CalcTESCoilCoolingAndChargeMode')
    ! Check for saturation error and modify temperature at constant enthalpy
    IF(FullLoadOutAirTemp .LT. PsyTsatFnHPb(FullLoadOutAirEnth,OutBaroPress, 'CalcTESCoilCoolingAndChargeMode')) THEN
      FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,OutBaroPress, 'CalcTESCoilCoolingAndChargeMode')
      FullLoadOutAirHumRat  = PsyWFnTdbH(FullLoadOutAirTemp,FullLoadOutAirEnth, 'CalcTESCoilCoolingAndChargeMode')
    ENDIF

    ! Continuous fan, cycling compressor
    EvapOutletAirEnthalpy = ((PartLoadRatio)*FullLoadOutAirEnth + &
                                            (1.d0-(PartLoadRatio ))*EvapInletEnthalpy)
    EvapOutletAirHumRat = ((PartLoadRatio)*FullLoadOutAirHumRat + &
                                            (1.d0-(PartLoadRatio ))*EvapInletHumRat)
    EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy,EvapOutletAirHumRat)
    IF(EvapOutletAirTemp .LT. PsyTsatFnHPb(EvapOutletAirEnthalpy,OutBaroPress, 'CalcTESCoilCoolingAndChargeMode')) THEN
      EvapOutletAirTemp = PsyTsatFnHPb(EvapOutletAirEnthalpy,OutBaroPress, 'CalcTESCoilCoolingAndChargeMode')
      EvapOutletAirHumRat  = PsyWFnTdbH(EvapOutletAirTemp,EvapOutletAirEnthalpy, 'CalcTESCoilCoolingAndChargeMode')
    ENDIF

    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = EvapOutletAirTemp
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = EvapOutletAirHumRat
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Enthalpy     = EvapOutletAirEnthalpy
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = EvapAirMassFlow

    ! determine condenser leaving conditions
    QdotCond = EvapTotCap* EvapRuntimeFraction + EvapElecCoolingPower + TotChargeCap + ChargeElectricCoolingPower
    Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
    CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat , 'CalcTESCoilCoolingAndChargeMode')
    CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum)%CondenserAirMassFlow
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Temp = PsyTdbFnHW( CondOutletEnthalpy, CondInletHumrat, &
                                                                        'CalcTESCoilCoolingAndChargeMode')
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%HumRat   = CondInletHumrat
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Enthalpy = CondOutletEnthalpy


    TESCoil(TESCoilNum)%ElecCoolingPower = EvapElecCoolingPower + ChargeElectricCoolingPower &
                                           + TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour

    TESCoil(TESCoilNum)%RuntimeFraction = EvapRuntimeFraction
    IF (ChargeRuntimeFraction > 0.d0) THEN
      TESCoil(TESCoilNum)%CondenserRuntimeFraction = MAX(ChargeRuntimeFraction, EvapRuntimeFraction)
    ELSE
      TESCoil(TESCoilNum)%CondenserRuntimeFraction = EvapRuntimeFraction
    ENDIF

    TESCoil(TESCoilNum)%EvapTotCoolingRate = EvapTotCap* EvapRuntimeFraction ! double check this
    TESCoil(TESCoilNum)%EvapTotCoolingEnergy = EvapTotCap* EvapRuntimeFraction * TimeStepSys  *SecInHour
    MinAirHumRat = MIN( Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                        Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat )
    TESCoil(TESCoilNum)%EvapSensCoolingRate  = EvapAirMassFlow *&
                      (PsyHFnTdbW(EvapInletDryBulb , MinAirHumRat, 'CalcTESCoilCoolingAndChargeMode') - &
                        PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat, 'CalcTESCoilCoolingAndChargeMode') )
    IF (TESCoil(TESCoilNum)%EvapSensCoolingRate  > TESCoil(TESCoilNum)%EvapTotCoolingRate) THEN
      TESCoil(TESCoilNum)%EvapSensCoolingRate = TESCoil(TESCoilNum)%EvapTotCoolingRate
    ENDIF
    TESCoil(TESCoilNum)%EvapSensCoolingEnergy=  TESCoil(TESCoilNum)%EvapSensCoolingRate * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%EvapLatCoolingRate   = TESCoil(TESCoilNum)%EvapTotCoolingRate &
                                              - TESCoil(TESCoilNum)%EvapSensCoolingRate
    TESCoil(TESCoilNum)%EvapLatCoolingEnergy =  TESCoil(TESCoilNum)%EvapLatCoolingRate * TimeStepSys  *SecInHour

  ELSE ! Evap off, but may still charge
    IF (TESCanBeCharged) THEN ! coil is running to charge but not to cool at evaporator
      AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum)%RatedEvapAirMassFlowRate
      ChargeCapModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeChargingCapFTempCurve, &
                                        EvapInletWetBulb, CondInletTemp , sTES)
      ChargeCapModFac = MAX(0.d0, ChargeCapModFac)

      ChargeCapPLRModFac= CurveValue(TESCoil(TESCoilNum)%CoolingAndChargeChargingCapFEvapPLRCurve, PartLoadRatio)
      ChargeCapPLRModFac = MAX(0.d0, ChargeCapPLRModFac)

      TotChargeCap = TESCoil(TESCoilNum)%CoolingAndChargeRatedChargeCap * ChargeCapModFac * ChargeCapPLRModFac
      IF (TotChargeCap > QdotChargeLimit) THEN
        ChargeRuntimeFraction = QdotChargeLimit / TotChargeCap
        TotChargeCap = MIN(TotChargeCap, QdotChargeLimit)
      ELSE
        ChargeRuntimeFraction = 1.d0
      ENDIF
      ChargeEIRTempModFac = CurveValue( TESCoil(TESCoilNum)%CoolingAndChargeChargingEIRFTempCurve, &
                                         EvapInletWetBulb,CondInletTemp , sTES)
      ChargeEIRTempModFac = MAX(0.d0, ChargeEIRTempModFac )
      ChargeEIRFlowModFac = CurveValue( TESCoil(TESCoilNum)%CoolingAndChargeChargingEIRFFLowCurve, AirMassFlowRatio)
      ChargeEIRFlowModFac = MAX (0.d0,ChargeEIRFlowModFac)
      ChargeEIR = (ChargeEIRTempModFac * ChargeEIRFlowModFac) /   TESCoil(TESCoilNum)%CoolingAndChargeChargingRatedCOP
      ChargeElectricCoolingPower = TotChargeCap * ChargeEIR
      TESCoil(TESCoilNum)%QdotTES = - TotChargeCap
    ELSE
      TotChargeCap = 0.d0
      ChargeElectricCoolingPower =0.d0
      TESCoil(TESCoilNum)%QdotTES = 0.d0
      ChargeRuntimeFraction = 0.d0
    ENDIF

    TESCoil(TESCoilNum)%ElecCoolingPower   =  ChargeElectricCoolingPower + TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour

    TESCoil(TESCoilNum)%RuntimeFraction    = 0.d0
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum)%Enthalpy      = &
                                                            PsyHFnTdbW(Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp, &
                                                                       Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                                                                       'CalcTESCoilCoolingOnlyMode')

    TESCoil(TESCoilNum)%EvapTotCoolingRate   =  0.d0
    TESCoil(TESCoilNum)%EvapTotCoolingEnergy =  0.d0
    TESCoil(TESCoilNum)%EvapSensCoolingRate  =  0.d0
    TESCoil(TESCoilNum)%EvapSensCoolingEnergy=  0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingRate   =  0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingEnergy =  0.d0

    IF (TotChargeCap == 0.d0) THEN
      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
      Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate = 0.d0
      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%MassFlowRate
      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Enthalpy     = &
                                                              PsyHFnTdbW(Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp, &
                                                                         Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat, &
                                                                         'CalcTESCoilCoolingOnlyMode')
      TESCoil(TESCoilNum)%CondenserRuntimeFraction    = 0.d0
    ELSE

      ! determine condenser leaving conditions
      QdotCond = TotChargeCap + ChargeElectricCoolingPower
      Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
      CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat , 'CalcTESCoilCoolingAndChargeMode')
      CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum)%CondenserAirMassFlow
      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Temp = PsyTdbFnHW( CondOutletEnthalpy, CondInletHumrat, &
                                                                          'CalcTESCoilCoolingAndChargeMode')
      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%HumRat   = CondInletHumrat
      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Enthalpy = CondOutletEnthalpy
      TESCoil(TESCoilNum)%CondenserRuntimeFraction    = 1.d0
    ENDIF


  ENDIF

  TESCoil(TESCoilNum)%QdotTES = - TotChargeCap
  TESCoil(TESCoilNum)%Q_TES   = TESCoil(TESCoilNum)%QdotTES * TimeStepSys * SecInHour

  CALL UpdateTEStorage(TESCoilNum)

  TESCoil(TESCoilNum)%CondInletTemp = CondInletTemp

  CALL UpdateColdWeatherProtection(TESCoilNum)

  IF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CALL UpdateEvaporativeCondenserBasinHeater(TESCoilNum)
    CALL UpdateEvaporativeCondenserWaterUse(TESCoilNum, CondInletHumrat, TESCoil(TESCoilNum)%CondAirInletNodeNum)
  ENDIF

  RETURN

END SUBROUTINE CalcTESCoilCoolingAndChargeMode

SUBROUTINE CalcTESCoilCoolingAndDischargeMode(TESCoilNum, FanOpMode, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE FluidProperties,     ONLY: GetSpecificHeatGlycol, GetDensityGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
  INTEGER , INTENT (IN) :: TESCoilNum
  INTEGER , INTENT (IN) :: FanOpMode
  REAL(r64) , INTENT (IN) :: PartLoadRatio

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER :: MaxIter = 30
  REAL(r64), PARAMETER :: RelaxationFactor = 0.4d0
  REAL(r64), PARAMETER :: Tolerance = 0.1d0


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CondInletTemp         ! Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
                                   ! Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
  REAL(r64) :: CondInletHumrat      ! Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
                                    ! For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
  REAL(r64) :: CondAirMassFlow      ! Condenser air mass flow rate [kg/s]
  REAL(r64) :: CondInletEnthalpy    ! condenser inlet enthalpy [J/kg]
  REAL(r64) :: CondAirSidePressure  ! Outdoor barometric pressure at condenser (Pa)
  REAL(r64) :: CondOutletEnthalpy   !condesner outlet enthalpy [J/kg]
  REAL(r64) :: OutdoorDryBulb  ! outdoor air dry bulb local variable [C]
  REAL(r64) :: OutdoorHumRat   ! outdoor air humidity ratio local [kg/kg]
  REAL(r64) :: OutdoorWetBulb  ! outdoor air wetbulb local [C]
  REAL(r64) :: EvapAirMassFlow   ! local for evaporator air mass flow [kg/s]
  REAL(r64) :: EvapInletDryBulb ! evaporator inlet air drybulb [C]
  REAL(r64) :: EvapInletHumRat ! evaporator inlet air humidity ratio [kg/kg]
  REAL(r64) :: EvapInletWetBulb ! evaporator inlet air wetbulb [C]
  REAL(r64) :: EvapInletEnthalpy ! evaporator inlet air enthalpy [J/kg]
  REAL(r64) :: sTES ! stat of Thermal energy storage [C or fraction of ice]
  LOGICAL   :: TESHasSomeCharge ! some charge available for discharge
  REAL(r64) :: rho
  REAL(r64) :: TankMass            ! Mass of fluid in tank (kg)
  REAL(r64) :: CpTank              ! Specific heat of water in tank (J/kg K)
  REAL(r64) :: QdotDischargeLimit ! limit for charge cooling power to hit limit of storage.
  REAL(r64) :: AirMassFlowRatio ! evaporator inlet air mass flow divided by design mass flow [ ]
  REAL(r64) :: EvapTotCapTempModFac ! total coolin capacity modification factor due to temps []
  REAL(r64) :: EvapTotCapFlowModFac !Total cooling capacity modification factor due to flow []
  REAL(r64) :: EvapTotCap ! total cooling capacity
  REAL(r64) :: SHRTempFac ! sensible heat ratio modification factor due to temps []
  REAL(r64) :: SHRFlowFac ! sensible heat ratio modification factor due to flow []
  REAL(r64) :: SHR ! sensible heat ratio
  REAL(r64) :: PLF   ! part load factor
  REAL(r64) :: EvapRuntimeFraction ! compressor running time divided by full time of timestep.
  REAL(r64) :: EIRTempModFac ! energy input ratio modification factor due to temperatures []
  REAL(r64) :: EIRFlowModFac !energy input ratio modification factor due to flow []
  REAL(r64) :: EIR ! energy input ratio
  REAL(r64) :: DischargePLF
  REAL(r64) :: DischargeRuntimeFraction
  REAL(r64) :: TotDischargeCap
  REAL(r64) :: DischargeCapTempModFac
  REAL(r64) :: DischargeCapFlowModFac
  REAL(r64) :: DischargeEIRTempModFac
  REAL(r64) :: DischargeEIRFlowModFac
  REAL(r64) :: DischargeEIR
  REAL(r64) :: EvapElecCoolingPower ! compressor electric power
  REAL(r64) :: DischargeElectricCoolingPower
  REAL(r64) :: TotCap
  REAL(r64) :: FullLoadOutAirEnth ! evaporator outlet full load enthalpy [J/kg]
  REAL(r64) :: hTinwout   ! Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
  REAL(r64) :: FullLoadOutAirHumRat ! evaporator outlet humidity ratio at full load
  REAL(r64) :: FullLoadOutAirTemp  !evaporator outlet air temperature at full load [C]
  REAL(r64) :: EvapOutletAirEnthalpy ! evaporator outlet air enthalpy [J/kg]
  REAL(r64) :: EvapOutletAirHumRat !evaporator outlet air humidity ratio [kg/kg]
  REAL(r64) :: EvapOutletAirTemp !evaporator outlet dryblub [C]
  REAL(r64) :: QdotCond ! heat rejection rate at condenser [W]
  REAL(r64) :: MinAirHumRat ! minimum air humidity ratio
  REAL(r64) :: PartLoadOutAirEnth ! local leaving enthalpy at part load
  REAL(r64) :: PartLoadDryCoilOutAirTemp ! local leaving drybulb if coil were dry
  LOGICAL   :: CoilMightBeDry
  INTEGER   :: Counter
  LOGICAL   :: Converged
  REAL(r64) :: DryCoilTestEvapInletHumRat
  REAL(r64) :: DryCoilTestEvapInletWetBulb
  REAL(r64) :: hADP
  REAL(r64) :: tADP
  REAL(r64) :: wADP
  REAL(r64) :: hTinwADP
  REAL(r64) :: SHRadp
  REAL(r64) :: werror

  ! first deal with condenser
  IF (TESCoil(TESCoilNum)%CondenserType == AirCooled) THEN
    CondAirSidePressure = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Press
    IF(CondAirSidePressure == DefaultNodeValues%Press)THEN
      CondInletTemp    = OutDryBulbTemp
      CondInletHumrat  = OutHumRat
      CondAirSidePressure     = OutBaroPress
    ELSE
     CondInletTemp = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
     CondInletHumrat = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat

    ENDIF
    CondAirMassFlow = TESCoil(TESCoilNum)%CondenserAirMassFlow
  ELSEIF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CondAirSidePressure = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Press
    IF(CondAirSidePressure == DefaultNodeValues%Press)THEN
      OutdoorDryBulb    = OutDryBulbTemp
      OutdoorHumRat  = OutHumRat
      CondAirSidePressure     = OutBaroPress
      OutdoorWetBulb   = OutWetBulbTemp
    ELSE
     OutdoorDryBulb = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
     OutdoorHumRat = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
     OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, 'CalcTESCoilCoolingAndDischargeMode')
    ENDIF
    CondAirMassFlow = TESCoil(TESCoilNum)%CondenserAirMassFlow
    ! direct evap cool model
    CondInletTemp   = OutdoorWetBulb + (OutdoorDryBulb-OutdoorWetBulb)*(1.0d0 - TESCoil(TESCoilNum)%EvapCondEffect)
    CondInletHumrat = PsyWFnTdbTwbPb(CondInletTemp,OutdoorWetBulb,CondAirSidePressure, 'CalcTESCoilCoolingAndDischargeMode')

  ENDIF
  EvapAirMassFlow = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
  EvapInletDryBulb = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
  EvapInletHumRat  = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
  EvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, EvapInletHumRat, OutBaroPress, 'CalcTESCoilCoolingAndDischargeMode')
  EvapInletEnthalpy = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Enthalpy
  CoilMightBeDry = .FALSE.

  IF (TESCoil(TESCoilNum)%StorageMedia == FluidBased)  THEN
    sTES = TESCoil(TESCoilNum)%FluidTankTempFinalLastTimestep
    IF ((sTES >= TESCoil(TESCoilNum)%MinimumFluidTankTempLimit) .AND. (sTES < TESCoil(TESCoilNum)%MaximumFluidTankTempLimit)) THEN
      TESHasSomeCharge = .TRUE.
      rho             = GetDensityGlycol(TESCoil(TESCoilNum)%StorageFluidName, sTES, &
                                         TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESCoilCoolingAndDischargeMode')
      TankMass        = rho * TESCoil(TESCoilNum)%FluidStorageVolume
      CpTank          = GetSpecificHeatGlycol(TESCoil(TESCoilNum)%StorageFluidName, sTES, &
                                              TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESCoilCoolingAndDischargeMode')
      !simple linear approximation of DT/Dt term in McpDT/Dt
      QdotDischargelimit = TankMass * CpTank * (TESCoil(TESCoilNum)%MaximumFluidTankTempLimit - sTES)/ (TimeStepSys * SecInHour)
    ELSE
      TESHasSomeCharge = .FALSE.
    ENDIF
  ELSEIF (TESCoil(TESCoilNum)%StorageMedia == IceBased) THEN
    sTES = TESCoil(TESCoilNum)%IceFracRemainLastTimestep
    If (sTES > 0.d0 ) THEN
      TESHasSomeCharge = .TRUE.
      ! discharge limit
      QdotDischargelimit = (sTES) * TESCoil(TESCoilNum)%IceStorageCapacity / (TimeStepSys * SecInHour)
    ELSE
      TESHasSomeCharge = .FALSE.
    ENDIF
  ENDIF

  IF ((EvapAirMassFlow > SmallMassFlow) .AND. (PartLoadRatio > 0.d0)) THEN ! coil is running

    AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum)%RatedEvapAirMassFlowRate
    EvapTotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeCoolingCapFTempCurve, &
                                           EvapInletWetBulb, CondInletTemp, sTES)
    EvapTotCapTempModFac = MAX(0.d0, EvapTotCapTempModFac) ! could warn if negative, DXcoil does
    EvapTotCapFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeCoolingCapFFlowCurve, AirMassFlowRatio)
    EvapTotCapFlowModFac = MAX(0.d0, EvapTotCapFlowModFac)  ! could warn if negative, DXcoil does
    EvapTotCap = TESCoil(TESCoilNum)%CoolingAndDischargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac
    ! now see if coil is running dry
    PartLoadOutAirEnth =  EvapInletEnthalpy - (EvapTotCap * PartLoadRatio) / EvapAirMassFlow
    PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat,'CalcTESCoilCoolingAndDischargeMode')
    IF (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(PartLoadOutAirEnth,OutBaroPress, 'CalcTESCoilCoolingAndDischargeMode')) THEN
      CoilMightBeDry = .TRUE.
      ! find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
      DryCoilTestEvapInletHumRat = EvapInletHumRat
      DryCoilTestEvapInletWetBulb = EvapInletWetBulb
      counter = 0
      Converged = .FALSE.
      DO While (.NOT. Converged)
        EvapTotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeCoolingCapFTempCurve, &
                                               DryCoilTestEvapInletWetBulb, CondInletTemp, sTES)
        EvapTotCapTempModFac = MAX(0.d0, EvapTotCapTempModFac) ! could warn if negative, DXcoil does
        EvapTotCapFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeCoolingCapFFlowCurve, AirMassFlowRatio)
        EvapTotCapFlowModFac = MAX(0.d0, EvapTotCapFlowModFac)  ! could warn if negative, DXcoil does
        EvapTotCap = TESCoil(TESCoilNum)%CoolingAndDischargeRatedTotCap * EvapTotCapTempModFac * EvapTotCapFlowModFac
        ! coil bypass factor = 0.0
        hADP = EvapInletEnthalpy - (EvapTotCap / EvapAirMassFlow)
        tADP = PsyTsatFnHPb(hADP, OutBaroPress, 'CalcTESCoilCoolingAndDischargeMode')
        wADP = MIN(EvapInletHumRat, PsyWfnTdbH(tADP, hADP, 'CalcTESCoilCoolingAndDischargeMode') )
        hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP, 'CalcTESCoilCoolingAndDischargeMode')
        IF ((EvapInletEnthalpy - hADP) > 1.d-10) THEN
          SHRadp = MIN((hTinwADP-hADP)/(EvapInletEnthalpy-hADP),1.d0)
        ELSE
          SHRadp = 1.d0
        ENDIF

        IF ((wADP > DryCoilTestEvapInletHumRat) .or. (Counter .ge. 1 .and. Counter .lt. MaxIter)) THEN
          IF (DryCoilTestEvapInletHumRat <= 0.d0) DryCoilTestEvapInletHumRat = 0.00001d0
          werror = (DryCoilTestEvapInletHumRat -  wADP)/DryCoilTestEvapInletHumRat

          DryCoilTestEvapInletHumRat = RelaxationFactor*wADP + (1.d0 - RelaxationFactor)*DryCoilTestEvapInletHumRat
          DryCoilTestEvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb , DryCoilTestEvapInletHumRat, OutBaroPress, &
                                                    'CalcTESCoilCoolingAndDischargeMode')

          Counter = Counter + 1
          IF (ABS(werror) <= Tolerance) THEN
            Converged = .TRUE.
          ELSE
            Converged = .FALSE.
          ENDIF
        ELSE
          Converged = .TRUE.
        ENDIF

      ENDDO
    ENDIF
    SELECT CASE (TESCoil(TESCoilNum)%CoolingAndDischargeSHRFTempObjectNum )
    CASE (CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV )
      SHRTempFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeSHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb)
    CASE (CurveType_TriQuadratic, CurveType_TableMultiIV)
      SHRTempFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeSHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb, sTES)
    END SELECT
    SHRFlowFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeSHRFFlowCurve, AirMassFlowRatio)
    SHR = TESCoil(TESCoilNum)%CoolingAndDischargeRatedSHR * SHRTempFac * SHRFlowFac
    SHR = MIN(SHR, 1.d0) ! warn maybe
    SHR = MAX(SHR, 0.d0) ! warn maybe
    IF ( CoilMightBeDry) THEN
      IF ((EvapInletHumRat < DryCoilTestEvapInletHumRat) .AND. (SHRadp > SHR)) THEN ! coil is dry for sure
        SHR = 1.0d0
      ELSEIF (SHRadp > SHR) THEN
        SHR = SHRadp
      ENDIF
    ENDIF
    PLF = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeCoolingPLFFPLRCurve, PartLoadRatio)
    IF (PLF >= PartLoadRatio .and. PLF > 0.d0 ) THEN
      EvapRuntimeFraction = PartLoadRatio / PLF
    ELSE
      EvapRuntimeFraction = 1.d0 ! warn maybe
    ENDIF
    ! Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
    EIRTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeCoolingEIRFTempCurve, EvapInletWetBulb, CondInletTemp, sTES)
    EIRTempModFac = MAX(EIRTempModFac, 0.d0)
    EIRFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeCoolingEIRFFlowCurve, AirMassFlowRatio)
    EIRFlowModFac = MAX(EIRFlowModFac, 0.d0)
    EIR = EIRTempModFac * EIRFlowModFac / TESCoil(TESCoilNum)%CoolingAndDischargeCoolingRatedCOP

    EvapElecCoolingPower = EvapTotCap * EIR * EvapRuntimeFraction

    IF (TESHasSomeCharge) THEN
      DischargeCapTempModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeDischargingCapFTempCurve, &
                                        EvapInletWetBulb, CondInletTemp , sTES)
      DischargeCapTempModFac = MAX(0.d0, DischargeCapTempModFac)
      DischargeCapFlowModFac = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeDischargingCapFFlowCurve, AirMassFlowRatio)
      DischargeCapFlowModFac = MAX(0.d0, DischargeCapFlowModFac)

      DischargePLF = CurveValue(TESCoil(TESCoilNum)%CoolingAndDischargeDischargingCapFEvapPLRCurve, PartLoadRatio)
      IF (DischargePLF >= PartLoadRatio .and. DischargePLF > 0.d0 ) THEN
        DischargeRuntimeFraction = PartLoadRatio / DischargePLF
      ELSE
        DischargeRuntimeFraction = 1.d0 ! warn maybe
      ENDIF

      TotDischargeCap = TESCoil(TESCoilNum)%CoolingAndDischargeRatedDischargeCap * DischargeCapTempModFac * &
                                   DischargeCapFlowModFac * DischargeRuntimeFraction
      IF (TotDischargeCap > QdotDischargelimit) THEN
        TotDischargeCap = MIN(TotDischargeCap, QdotDischargelimit)
      ENDIF
      DischargeEIRTempModFac = CurveValue( TESCoil(TESCoilNum)%CoolingAndDischargeDischargingEIRFTempCurve,EvapInletWetBulb, &
                                           CondInletTemp , sTES)
      DischargeEIRTempModFac = MAX(0.d0, DischargeEIRTempModFac )
      DischargeEIRFlowModFac = CurveValue( TESCoil(TESCoilNum)%CoolingAndDischargeDischargingEIRFFLowCurve, AirMassFlowRatio)
      DischargeEIRFlowModFac = MAX(0.d0, DischargeEIRFlowModFac)

      DischargeEIR = (DischargeEIRTempModFac * DischargeEIRFlowModFac ) / TESCoil(TESCoilNum)%CoolingAndDischargeDischargingRatedCOP
      DischargeElectricCoolingPower = TotDischargeCap * DischargeEIR * DischargeRuntimeFraction
      TESCoil(TESCoilNum)%QdotTES = TotDischargeCap
    ELSE
      TotDischargeCap = 0.d0
      DischargeElectricCoolingPower = 0.d0
      TESCoil(TESCoilNum)%QdotTES = 0.d0
    ENDIF

    TotCap = EvapTotCap + TotDischargeCap
    !  Calculate full load output conditions
    FullLoadOutAirEnth = EvapInletEnthalpy - TotCap / EvapAirMassFlow

    hTinwout = EvapInletEnthalpy - (1.0d0-SHR)* (TotCap / EvapAirMassFlow)
    !The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
    FullLoadOutAirHumRat = PsyWFnTdbH(EvapInletDryBulb,hTinwout, 'CalcTESCoilCoolingAndDischargeMode', SuppressWarnings = .TRUE.)
    FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth,FullLoadOutAirHumRat, 'CalcTESCoilCoolingAndDischargeMode')
    ! Check for saturation error and modify temperature at constant enthalpy
    IF(FullLoadOutAirTemp .LT. PsyTsatFnHPb(FullLoadOutAirEnth,OutBaroPress, 'CalcTESCoilCoolingAndDischargeMode')) THEN
      FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,OutBaroPress, 'CalcTESCoilCoolingAndDischargeMode')
      FullLoadOutAirHumRat  = PsyWFnTdbH(FullLoadOutAirTemp,FullLoadOutAirEnth, 'CalcTESCoilCoolingAndDischargeMode')
    ENDIF
    ! Continuous fan, cycling compressor
    EvapOutletAirEnthalpy = ((PartLoadRatio)*FullLoadOutAirEnth + &
                                            (1.d0-(PartLoadRatio ))*EvapInletEnthalpy)
    EvapOutletAirHumRat = ((PartLoadRatio)*FullLoadOutAirHumRat + &
                                            (1.d0-(PartLoadRatio ))*EvapInletHumRat)
    EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy,EvapOutletAirHumRat)
    IF(EvapOutletAirTemp .LT. PsyTsatFnHPb(EvapOutletAirEnthalpy,OutBaroPress, 'CalcTESCoilCoolingAndDischargeMode')) THEN
      EvapOutletAirTemp = PsyTsatFnHPb(EvapOutletAirEnthalpy,OutBaroPress, 'CalcTESCoilCoolingAndDischargeMode')
      EvapOutletAirHumRat  = PsyWFnTdbH(EvapOutletAirTemp,EvapOutletAirEnthalpy, 'CalcTESCoilCoolingAndDischargeMode')
    ENDIF

    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = EvapOutletAirTemp
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = EvapOutletAirHumRat
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Enthalpy     = EvapOutletAirEnthalpy
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = EvapAirMassFlow

    ! determine condenser leaving conditions
    QdotCond = EvapTotCap* EvapRuntimeFraction + EvapElecCoolingPower
    Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
    CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat , 'CalcTESCoilCoolingAndDischargeMode')
    CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum)%CondenserAirMassFlow
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Temp = PsyTdbFnHW( CondOutletEnthalpy, CondInletHumrat, &
                                                                        'CalcTESCoilCoolingAndDischargeMode')
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%HumRat   = CondInletHumrat
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Enthalpy = CondOutletEnthalpy

    TESCoil(TESCoilNum)%ElecCoolingPower = EvapElecCoolingPower + DischargeElectricCoolingPower &
                                           + TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%RuntimeFraction = (EvapTotCap* EvapRuntimeFraction + TotDischargeCap * DischargeRuntimeFraction) &
                                          / (EvapTotCap + TotDischargeCap)

    TESCoil(TESCoilNum)%EvapTotCoolingRate = EvapTotCap* EvapRuntimeFraction + TotDischargeCap * DischargeRuntimeFraction
    TESCoil(TESCoilNum)%EvapTotCoolingEnergy = TESCoil(TESCoilNum)%EvapTotCoolingRate * TimeStepSys * SecInHour
    MinAirHumRat = MIN( Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                        Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat )
    TESCoil(TESCoilNum)%EvapSensCoolingRate  = EvapAirMassFlow *&
                      (PsyHFnTdbW(EvapInletDryBulb , MinAirHumRat, 'CalcTESCoilCoolingAndDischargeMode') - &
                        PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat, 'CalcTESCoilCoolingAndDischargeMode') )
    IF (TESCoil(TESCoilNum)%EvapSensCoolingRate  > TESCoil(TESCoilNum)%EvapTotCoolingRate) THEN
      TESCoil(TESCoilNum)%EvapSensCoolingRate = TESCoil(TESCoilNum)%EvapTotCoolingRate
    ENDIF
    TESCoil(TESCoilNum)%EvapSensCoolingEnergy=  TESCoil(TESCoilNum)%EvapSensCoolingRate * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%EvapLatCoolingRate   = TESCoil(TESCoilNum)%EvapTotCoolingRate &
                                              - TESCoil(TESCoilNum)%EvapSensCoolingRate
    TESCoil(TESCoilNum)%EvapLatCoolingEnergy =  TESCoil(TESCoilNum)%EvapLatCoolingRate * TimeStepSys  *SecInHour

  ELSE !coil is off; just pass through conditions
    TESCoil(TESCoilNum)%QdotTES  = 0.d0

    TESCoil(TESCoilNum)%ElecCoolingPower   = TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy  = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%RuntimeFraction    = 0.d0

    TESCoil(TESCoilNum)%RuntimeFraction      =  0.d0
    TESCoil(TESCoilNum)%EvapTotCoolingRate   =  0.d0
    TESCoil(TESCoilNum)%EvapTotCoolingEnergy =  0.d0
    TESCoil(TESCoilNum)%EvapSensCoolingRate  =  0.d0
    TESCoil(TESCoilNum)%EvapSensCoolingEnergy=  0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingRate   =  0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingEnergy =  0.d0

    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum)%Enthalpy      = &
                                                            PsyHFnTdbW(Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp, &
                                                                       Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                                                                       'CalcTESCoilCoolingAndDischargeMode')
    !nothing happens at condenser
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
    Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate   = 0.d0
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%MassFlowRate
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Enthalpy      = &
                                                            PsyHFnTdbW(Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp, &
                                                                        Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat, &
                                                                        'CalcTESCoilCoolingAndDischargeMode')
    TESCoil(TESCoilNum)%CondInletTemp = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
  ENDIF
  TESCoil(TESCoilNum)%Q_TES   = TESCoil(TESCoilNum)%QdotTES * TimeStepSys * SecInHour
  CALL UpdateTEStorage(TESCoilNum)

  CALL UpdateColdWeatherProtection(TESCoilNum)

  IF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CALL UpdateEvaporativeCondenserBasinHeater(TESCoilNum)
    CALL UpdateEvaporativeCondenserWaterUse(TESCoilNum, CondInletHumrat, TESCoil(TESCoilNum)%CondAirInletNodeNum)
  ENDIF

  RETURN

END SUBROUTINE CalcTESCoilCoolingAndDischargeMode

SUBROUTINE CalcTESCoilChargeOnlyMode(TESCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE FluidProperties,     ONLY: GetSpecificHeatGlycol, GetDensityGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TESCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: sTES ! local state of Thermal Energy Storage (C or ice fraction)
  REAL(r64) :: CondInletTemp         ! Condenser inlet temperature (C). Outdoor dry-bulb temp for air-cooled condenser.
                                   ! Outdoor Wetbulb +(1 - effectiveness)*(outdoor drybulb - outdoor wetbulb) for evap condenser.
  REAL(r64) :: CondInletHumrat      ! Condenser inlet humidity ratio (kg/kg). Zero for air-cooled condenser.
                                    ! For evap condenser, its the humidity ratio of the air leaving the evap cooling pads.
  REAL(r64) :: CondAirMassFlow      ! Condenser air mass flow rate [kg/s]
  REAL(r64) :: CondInletEnthalpy    ! condenser inlet enthalpy [J/kg]
  REAL(r64) :: CondAirSidePressure  ! Outdoor barometric pressure at condenser (Pa)
  REAL(r64) :: QdotCond  ! condenser total heat rejection rate [W]
  REAL(r64) :: CondOutletEnthalpy   !condesner outlet enthalpy [J/kg]
  REAL(r64) :: OutdoorDryBulb  ! outdoor air dry bulb local variable [C]
  REAL(r64) :: OutdoorHumRat   ! outdoor air humidity ratio local [kg/kg]
  REAL(r64) :: OutdoorWetBulb  ! outdoor air wetbulb local [C]
  REAL(r64) :: CapModFac  ! local capacity modifying factor
  REAL(r64) :: TotCap ! total cooling (charging) capacity
  REAL(r64) :: EIRModFac ! local energy input ratio modifying factor
  REAL(r64) :: EIR ! energy input ratio
  REAL(r64) :: ElecCoolingPower ! compressor electric power
  LOGICAL   :: TESCanBeCharged ! true if room for tank to be charged.
  REAL(r64) :: QdotChargeLimit ! limit for charge cooling power to hit limit of storage.
  REAL(r64) :: rho                 ! density of fluid in tank (kg/m3)
  REAL(r64) :: TankMass            ! Mass of fluid in tank (kg)
  REAL(r64) :: CpTank              ! Specific heat of water in tank (J/kg K)

 ! nothing happens at Evaporator
  Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
  Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
  Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
  Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum)%Enthalpy      = &
                                                          PsyHFnTdbW(Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp, &
                                                                      Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                                                                      'CalcTESCoilChargeOnlyMode')


  ! first deal with condenser
  IF (TESCoil(TESCoilNum)%CondenserType == AirCooled) THEN
    CondAirSidePressure = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Press
    IF(CondAirSidePressure == DefaultNodeValues%Press)THEN
      CondInletTemp    = OutDryBulbTemp
      CondInletHumrat  = OutHumRat
      CondAirSidePressure     = OutBaroPress
    ELSE
     CondInletTemp = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
     CondInletHumrat = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat

    ENDIF
    CondAirMassFlow = TESCoil(TESCoilNum)%CondenserAirMassFlow
  ELSEIF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CondAirSidePressure = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Press
    IF(CondAirSidePressure == DefaultNodeValues%Press)THEN
      OutdoorDryBulb    = OutDryBulbTemp
      OutdoorHumRat  = OutHumRat
      CondAirSidePressure     = OutBaroPress
      OutdoorWetBulb   = OutWetBulbTemp
    ELSE
     OutdoorDryBulb = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
     OutdoorHumRat = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
     OutdoorWetBulb  = PsyTwbFnTdbWPb(OutdoorDryBulb, OutdoorHumRat, CondAirSidePressure, 'CalcTESCoilChargeOnlyMode')
    ENDIF
    CondAirMassFlow = TESCoil(TESCoilNum)%CondenserAirMassFlow
    ! direct evap cool model
    CondInletTemp   = OutdoorWetBulb + (OutdoorDryBulb-OutdoorWetBulb)*(1.0d0 - TESCoil(TESCoilNum)%EvapCondEffect)
    CondInletHumrat = PsyWFnTdbTwbPb(CondInletTemp,OutdoorWetBulb,CondAirSidePressure, 'CalcTESCoilChargeOnlyMode')
  ENDIF

  IF (TESCoil(TESCoilNum)%StorageMedia == FluidBased) THEN
    sTES = TESCoil(TESCoilNum)%FluidTankTempFinalLastTimestep
    IF ((sTES > TESCoil(TESCoilNum)%MinimumFluidTankTempLimit) .AND. (sTES < TESCoil(TESCoilNum)%MaximumFluidTankTempLimit)) THEN
      TESCanBeCharged = .TRUE.
      !find charge limit to reach limits
      rho             = GetDensityGlycol(TESCoil(TESCoilNum)%StorageFluidName, sTES, &
                                         TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESCoilChargeOnlyMode')
      TankMass        = rho * TESCoil(TESCoilNum)%FluidStorageVolume
      CpTank          = GetSpecificHeatGlycol(TESCoil(TESCoilNum)%StorageFluidName, sTES, &
                                              TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESCoilChargeOnlyMode')
      !simple linear approximation of DT/Dt term in McpDT/Dt
      QdotChargeLimit = TankMass * CpTank * (sTES - TESCoil(TESCoilNum)%MinimumFluidTankTempLimit) &
                                                  / (TimeStepSys * SecInHour)
    ELSE
      TESCanBeCharged = .FALSE.
    ENDIF
  ELSEIF (TESCoil(TESCoilNum)%StorageMedia == IceBased) THEN
    sTES = TESCoil(TESCoilNum)%IceFracRemainLastTimestep
    If (sTES < 1.d0 ) THEN
      TESCanBeCharged = .TRUE.
      !find charge limit to reach limit
      QdotChargeLimit = (1.d0 - sTES) * TESCoil(TESCoilNum)%IceStorageCapacity / (TimeStepSys * SecInHour)
    ELSE
      TESCanBeCharged = .FALSE.
    ENDIF
  ENDIF

  IF (TESCanBeCharged) THEN ! coil is running
    CapModFac = CurveValue(TESCoil(TESCoilNum)%ChargeOnlyChargingCapFTempCurve,CondInletTemp , sTES)
    CapModFac = MAX(0.d0, CapModFac)
    TotCap = TESCoil(TESCoilNum)%ChargeOnlyRatedCapacity * CapModFac
    IF (TotCap > QdotChargeLimit) THEN
      TESCoil(TESCoilNum)%RuntimeFraction  = QdotChargeLimit / TotCap
      TotCap = MIN(TotCap, QdotChargeLimit)
    ELSE
      TESCoil(TESCoilNum)%RuntimeFraction  = 1.d0
    ENDIF
    EIRModFac = CurveValue( TESCoil(TESCoilNum)%ChargeOnlyChargingEIRFTempCurve,CondInletTemp , sTES)
    EIRModFac = MAX(0.d0, EIRModFac )
    EIR = EIRModFac /   TESCoil(TESCoilNum)%ChargeOnlyRatedCOP
    ElecCoolingPower = TotCap * EIR
    QdotCond = TotCap + ElecCoolingPower
    Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate  = TESCoil(TESCoilNum)%CondenserAirMassFlow
    CondInletEnthalpy = PsyHFnTdbW(CondInletTemp, CondInletHumRat , 'CalcTESCoilChargeOnlyMode')
    CondOutletEnthalpy = CondInletEnthalpy + QdotCond / TESCoil(TESCoilNum)%CondenserAirMassFlow
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Temp = PsyTdbFnHW( CondOutletEnthalpy, CondInletHumrat, &
                                                                        'CalcTESCoilChargeOnlyMode')
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%HumRat   = CondInletHumrat
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum  )%Enthalpy = CondOutletEnthalpy

    TESCoil(TESCoilNum)%ElecCoolingPower  = ElecCoolingPower + TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour


    TESCoil(TESCoilNum)%QdotTES = - TotCap ! negative for cooling

  ELSE !not running
    TESCoil(TESCoilNum)%ElecCoolingPower   = TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy  = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%RuntimeFraction    = 0.d0
    TESCoil(TESCoilNum)%QdotTES            = 0.d0
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
    Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate = 0.d0
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%MassFlowRate
    Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Enthalpy      = &
                                                            PsyHFnTdbW(Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp, &
                                                                       Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat, &
                                                                       'CalcTESCoilChargeOnlyMode')
  ENDIF
  TESCoil(TESCoilNum)%Q_TES   = TESCoil(TESCoilNum)%QdotTES * TimeStepSys * SecInHour

  TESCoil(TESCoilNum)%EvapTotCoolingRate = 0.d0
  TESCoil(TESCoilNum)%EvapTotCoolingEnergy = 0.d0
  TESCoil(TESCoilNum)%EvapSensCoolingRate = 0.d0
  TESCoil(TESCoilNum)%EvapSensCoolingEnergy = 0.d0
  TESCoil(TESCoilNum)%EvapLatCoolingRate = 0.d0
  TESCoil(TESCoilNum)%EvapLatCoolingEnergy = 0.d0


  CALL UpdateTEStorage(TESCoilNum)

  CALL UpdateColdWeatherProtection(TESCoilNum)

  IF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CALL UpdateEvaporativeCondenserBasinHeater(TESCoilNum)
    CALL UpdateEvaporativeCondenserWaterUse(TESCoilNum, CondInletHumrat, TESCoil(TESCoilNum)%CondAirInletNodeNum)
  ENDIF

  RETURN

END SUBROUTINE CalcTESCoilChargeOnlyMode

SUBROUTINE CalcTESCoilDischargeOnlyMode( TESCoilNum , PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE FluidProperties,     ONLY: GetSpecificHeatGlycol, GetDensityGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT (IN) :: TESCoilNum
  REAL(r64) , INTENT (IN) :: PartLoadRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER :: MaxIter = 30
  REAL(r64), PARAMETER :: RelaxationFactor = 0.4d0
  REAL(r64), PARAMETER :: Tolerance = 0.1d0


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: AirMassFlowRatio ! evaporator inlet air mass flow divided by design mass flow [ ]
  REAL(r64) :: EvapAirMassFlow   ! local for evaporator air mass flow [kg/s]
  REAL(r64) :: EvapInletDryBulb ! evaporator inlet air drybulb [C]
  REAL(r64) :: EvapInletHumRat ! evaporator inlet air humidity ratio [kg/kg]
  REAL(r64) :: EvapInletWetBulb ! evaporator inlet air wetbulb [C]
  REAL(r64) :: EvapInletEnthalpy ! evaporator inlet air enthalpy [J/kg]
  REAL(r64) :: sTES ! state of charge of Thermal Energy Storage
  REAL(r64) :: TotCapTempModFac ! total coolin capacity modification factor due to temps []
  REAL(r64) :: TotCapFlowModFac !Total cooling capacity modification factor due to flow []
  REAL(r64) :: TotCap ! total cooling capacity
  REAL(r64) :: SHRTempFac ! sensible heat ratio modification factor due to temps []
  REAL(r64) :: SHRFlowFac ! sensible heat ratio modification factor due to flow []
  REAL(r64) :: SHR ! sensible heat ratio
  REAL(r64) :: PLF   ! part load factor
  REAL(r64) :: PLR   ! part load ratio
  REAL(r64) :: RuntimeFraction ! compressor running time divided by full time of timestep.
  REAL(r64) :: FullLoadOutAirEnth ! evaporator outlet full load enthalpy [J/kg]
  REAL(r64) :: FullLoadOutAirHumRat ! evaporator outlet humidity ratio at full load
  REAL(r64) :: FullLoadOutAirTemp  !evaporator outlet air temperature at full load [C]
  REAL(r64) :: hTinwout   ! Enthalpy at inlet dry-bulb and outlet humidity ratio [J/kg]
  REAL(r64) :: EvapOutletAirEnthalpy ! evaporator outlet air enthalpy [J/kg]
  REAL(r64) :: EvapOutletAirHumRat !evaporator outlet air humidity ratio [kg/kg]
  REAL(r64) :: EvapOutletAirTemp !evaporator outlet dryblub [C]
  REAL(r64) :: EIRTempModFac ! energy input ratio modification factor due to temperatures []
  REAL(r64) :: EIRFlowModFac !energy input ratio modification factor due to flow []
  REAL(r64) :: EIR ! energy input ratio
  REAL(r64) :: ElecCoolingPower ! compressor electric power
  REAL(r64) :: MinAirHumRat ! minimum air humidity ratio
  LOGICAL   :: TESHasSomeCharge ! true when there is something avaiable in storage
  REAL(r64) :: QdotDischargelimit ! limit for how much storage can be discharged without overshooting
  REAL(r64) :: rho                 ! density of water in tank (kg/m3)
  REAL(r64) :: TankMass            ! Mass of water in tank (kg)
  REAL(r64) :: CpTank              ! Specific heat of water in tank (J/kg K)
  REAL(r64) :: QdotTEStest
  REAL(r64) :: RuntimeFractionLimit
  REAL(r64) :: PartLoadOutAirEnth ! local leaving enthalpy at part load
  REAL(r64) :: PartLoadDryCoilOutAirTemp ! local leaving drybulb if coil were dry
  LOGICAL   :: CoilMightBeDry
  INTEGER   :: Counter
  LOGICAL   :: Converged
  REAL(r64) :: DryCoilTestEvapInletHumRat
  REAL(r64) :: DryCoilTestEvapInletWetBulb
  REAL(r64) :: hADP
  REAL(r64) :: tADP
  REAL(r64) :: wADP
  REAL(r64) :: hTinwADP
  REAL(r64) :: SHRadp
  REAL(r64) :: werror

  PLR = PartLoadRatio

  EvapAirMassFlow = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
  EvapInletDryBulb = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
  EvapInletHumRat  = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
  EvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb, EvapInletHumRat, OutBaroPress, 'CalcTESCoilDischargeOnlyMode')
  EvapInletEnthalpy = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Enthalpy
  CoilMightBeDry = .FALSE.

  IF (TESCoil(TESCoilNum)%StorageMedia == FluidBased)  THEN
    sTES = TESCoil(TESCoilNum)%FluidTankTempFinalLastTimestep
    IF ((sTES >= TESCoil(TESCoilNum)%MinimumFluidTankTempLimit) .AND. (sTES < TESCoil(TESCoilNum)%MaximumFluidTankTempLimit)) THEN
      TESHasSomeCharge = .TRUE.
      rho             = GetDensityGlycol(TESCoil(TESCoilNum)%StorageFluidName, sTES, &
                                         TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESWaterStorageTank')
      TankMass        = rho * TESCoil(TESCoilNum)%FluidStorageVolume
      CpTank          = GetSpecificHeatGlycol(TESCoil(TESCoilNum)%StorageFluidName, sTES, &
                                              TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESWaterStorageTank')
      !simple linear approximation of DT/Dt term in McpDT/Dt
      QdotDischargelimit = TankMass * CpTank * (TESCoil(TESCoilNum)%MaximumFluidTankTempLimit - sTES)/ (TimeStepSys * SecInHour)
    ELSE
      TESHasSomeCharge = .FALSE.
    ENDIF
  ELSEIF (TESCoil(TESCoilNum)%StorageMedia == IceBased) THEN
    sTES = TESCoil(TESCoilNum)%IceFracRemainLastTimestep
    If (sTES > 0.d0 ) THEN
      TESHasSomeCharge = .TRUE.
      ! discharge limit
      QdotDischargelimit = (sTES) * TESCoil(TESCoilNum)%IceStorageCapacity / (TimeStepSys * SecInHour)
    ELSE
      TESHasSomeCharge = .FALSE.
    ENDIF
  ENDIF


  IF ((EvapAirMassFlow > SmallMassFlow) .AND. (PLR > 0.d0) .AND. TESHasSomeCharge ) THEN ! coil is running
    AirMassFlowRatio = EvapAirMassFlow / TESCoil(TESCoilNum)%RatedEvapAirMassFlowRate

    TotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%DischargeOnlyCapFTempCurve, EvapInletWetBulb, sTES)
    TotCapTempModFac = MAX(0.d0, TotCapTempModFac)
    TotCapFlowModFac = CurveValue(TESCoil(TESCoilNum)%DischargeOnlyCapFFlowCurve, AirMassFlowRatio)
    TotCapFlowModFac = MAX(0.d0, TotCapFlowModFac)
    TotCap =  TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCap  * TotCapTempModFac * TotCapFlowModFac

    PLF = CurveValue(TESCoil(TESCoilNum)%DischargeOnlyPLFFPLRCurve, PLR)
    IF (PLF >= PLR .and. PLF > 0.d0 ) THEN
      RuntimeFraction = PLR / PLF
    ELSE
      RuntimeFraction = 1.d0 ! warn maybe
    ENDIF
    ! Calculate electricity consumed. First, get EIR modifying factors for off-rated conditions
    EIRTempModFac = CurveValue(TESCoil(TESCoilNum)%DischargeOnlyEIRFTempCurve, EvapInletWetBulb, sTES)
    EIRTempModFac = MAX(EIRTempModFac, 0.d0)
    EIRFlowModFac = CurveValue(TESCoil(TESCoilNum)%DischargeOnlyEIRFFlowCurve, AirMassFlowRatio)
    EIRFlowModFac = MAX(EIRFlowModFac, 0.d0)
    EIR = EIRTempModFac * EIRFlowModFac / TESCoil(TESCoilNum)%DischargeOnlyRatedCOP

    ElecCoolingPower = TotCap * EIR * RuntimeFraction
    QdotTEStest      = TotCap*RuntimeFraction + ElecCoolingPower

    IF (QdotTEStest > QdotDischargelimit) THEN
      RuntimeFractionLimit = QdotDischargelimit / (TotCap +  TotCap * EIR)
      RuntimeFraction = MIN(RuntimeFraction, RuntimeFractionLimit)
      PLR = RuntimeFraction * PLF
      ElecCoolingPower = TotCap * EIR * RuntimeFraction
    ENDIF
    ! now see if coil is running dry
    PartLoadOutAirEnth =  EvapInletEnthalpy - (TotCap * PartLoadRatio) / EvapAirMassFlow
    PartLoadDryCoilOutAirTemp = PsyTdbFnHW(PartLoadOutAirEnth, EvapInletHumRat,'CalcTESCoilDischargeOnlyMode')
    IF (PartLoadDryCoilOutAirTemp > PsyTsatFnHPb(PartLoadOutAirEnth,OutBaroPress, 'CalcTESCoilDischargeOnlyMode')) THEN
      CoilMightBeDry = .TRUE.
      ! find wADP, humidity ratio at apparatus dewpoint and inlet hum rat that would have dry coil
      DryCoilTestEvapInletHumRat = EvapInletHumRat
      DryCoilTestEvapInletWetBulb = EvapInletWetBulb
      counter = 0
      Converged = .FALSE.
      DO While (.NOT. Converged)
        TotCapTempModFac = CurveValue(TESCoil(TESCoilNum)%DischargeOnlyCapFTempCurve, DryCoilTestEvapInletWetBulb, sTES)
        TotCapTempModFac = MAX(0.d0, TotCapTempModFac)
        TotCapFlowModFac = CurveValue(TESCoil(TESCoilNum)%DischargeOnlyCapFFlowCurve, AirMassFlowRatio)
        TotCapFlowModFac = MAX(0.d0, TotCapFlowModFac)
        TotCap =  TESCoil(TESCoilNum)%DischargeOnlyRatedDischargeCap  * TotCapTempModFac * TotCapFlowModFac
        ! coil bypass factor = 0.0
        hADP = EvapInletEnthalpy - (TotCap / EvapAirMassFlow)
        tADP = PsyTsatFnHPb(hADP, OutBaroPress, 'CalcTESCoilDischargeOnlyMode')
        wADP = MIN(EvapInletHumRat, PsyWfnTdbH(tADP, hADP, 'CalcTESCoilDischargeOnlyMode') )
        hTinwADP = PsyHFnTdbW(EvapInletDryBulb, wADP, 'CalcTESCoilDischargeOnlyMode')
        IF ((EvapInletEnthalpy - hADP) > 1.d-10) THEN
          SHRadp = MIN((hTinwADP-hADP)/(EvapInletEnthalpy-hADP),1.d0)
        ELSE
          SHRadp = 1.d0
        ENDIF

        IF ((wADP > DryCoilTestEvapInletHumRat) .or. (Counter .ge. 1 .and. Counter .lt. MaxIter)) THEN
          IF (DryCoilTestEvapInletHumRat <= 0.d0) DryCoilTestEvapInletHumRat = 0.00001d0
          werror = (DryCoilTestEvapInletHumRat -  wADP)/DryCoilTestEvapInletHumRat

          DryCoilTestEvapInletHumRat = RelaxationFactor*wADP + (1.d0 - RelaxationFactor)*DryCoilTestEvapInletHumRat
          DryCoilTestEvapInletWetBulb = PsyTwbFnTdbWPb(EvapInletDryBulb , DryCoilTestEvapInletHumRat, OutBaroPress, &
                                                    'CalcTESCoilDischargeOnlyMode')

          Counter = Counter + 1
          IF (ABS(werror) <= Tolerance) THEN
            Converged = .TRUE.
          ELSE
            Converged = .FALSE.
          ENDIF
        ELSE
          Converged = .TRUE.
        ENDIF

      ENDDO
    ENDIF ! coil will be wet so use SHR curves
    SELECT CASE ( TESCoil(TESCoilNum)%DischargeOnlySHRFTempObjectNum )
    CASE ( CurveType_BiCubic, CurveType_BiQuadratic, CurveType_QuadraticLinear, CurveType_TableTwoIV )
      SHRTempFac = CurveValue(TESCoil(TESCoilNum)%DischargeOnlySHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb)
    CASE ( CurveType_TriQuadratic, CurveType_TableMultiIV )
      SHRTempFac = CurveValue(TESCoil(TESCoilNum)%DischargeOnlySHRFTempCurve, EvapInletWetBulb, EvapInletDryBulb, sTES)
    END SELECT

    SHRFlowFac = CurveValue(TESCoil(TESCoilNum)%DischargeOnlySHRFFLowCurve, AirMassFlowRatio)
    SHR =  TESCoil(TESCoilNum)%DischargeOnlyRatedSHR * SHRTempFac * SHRFlowFac
    SHR = MIN(SHR, 1.d0) ! warn maybe
    SHR = MAX(SHR, 0.d0) ! warn maybe
    IF ( CoilMightBeDry) THEN
      IF ((EvapInletHumRat < DryCoilTestEvapInletHumRat) .AND. (SHRadp > SHR)) THEN ! coil is dry for sure
        SHR = 1.0d0
      ELSEIF (SHRadp > SHR) THEN
        SHR = SHRadp
      ENDIF
    ENDIF
    !  Calculate full load output conditions
    FullLoadOutAirEnth = EvapInletEnthalpy - TotCap / EvapAirMassFlow

    hTinwout = EvapInletEnthalpy - (1.0d0-SHR)* (TotCap / EvapAirMassFlow)
    !The following will often throw psych warnings for neg w, suppress warnings because error condition is handled in next IF
    FullLoadOutAirHumRat = PsyWFnTdbH(EvapInletDryBulb,hTinwout, 'CalcTESCoilDischargeOnlyMode', SuppressWarnings = .TRUE.)
    FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth,FullLoadOutAirHumRat, 'CalcTESCoilDischargeOnlyMode')
    ! Check for saturation error and modify temperature at constant enthalpy
    IF(FullLoadOutAirTemp .LT. PsyTsatFnHPb(FullLoadOutAirEnth,OutBaroPress, 'CalcTESCoilDischargeOnlyMode')) THEN
      FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,OutBaroPress, 'CalcTESCoilDischargeOnlyMode')
      FullLoadOutAirHumRat  = PsyWFnTdbH(FullLoadOutAirTemp,FullLoadOutAirEnth, 'CalcTESCoilDischargeOnlyMode')
    ENDIF

    ! Continuous fan, cycling compressor
    EvapOutletAirEnthalpy = ((PLR)*FullLoadOutAirEnth + &
                                            (1.d0-(PLR ))*EvapInletEnthalpy)
    EvapOutletAirHumRat = ((PLR)*FullLoadOutAirHumRat + &
                                            (1.d0-(PLR ))*EvapInletHumRat)
    EvapOutletAirTemp = PsyTdbFnHW(EvapOutletAirEnthalpy,EvapOutletAirHumRat)
    IF(EvapOutletAirTemp .LT. PsyTsatFnHPb(EvapOutletAirEnthalpy,OutBaroPress, 'CalcTESCoilDischargeOnlyMode')) THEN
      EvapOutletAirTemp = PsyTsatFnHPb(EvapOutletAirEnthalpy,OutBaroPress, 'CalcTESCoilDischargeOnlyMode')
      EvapOutletAirHumRat  = PsyWFnTdbH(EvapOutletAirTemp,EvapOutletAirEnthalpy, 'CalcTESCoilDischargeOnlyMode')
    ENDIF


    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = EvapOutletAirTemp
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = EvapOutletAirHumRat
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Enthalpy     = EvapOutletAirEnthalpy
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = EvapAirMassFlow
    TESCoil(TESCoilNum)%ElecCoolingPower = ElecCoolingPower +  TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%RuntimeFraction = RuntimeFraction
    TESCoil(TESCoilNum)%EvapTotCoolingRate = TotCap* RuntimeFraction ! double check this
    TESCoil(TESCoilNum)%EvapTotCoolingEnergy = TotCap* RuntimeFraction * TimeStepSys  *SecInHour
    MinAirHumRat = MIN( Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                        Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat )
    TESCoil(TESCoilNum)%EvapSensCoolingRate  = EvapAirMassFlow *&
                      (PsyHFnTdbW(EvapInletDryBulb , MinAirHumRat, 'CalcTESCoilDischargeOnlyMode') - &
                        PsyHFnTdbW(EvapOutletAirTemp, MinAirHumRat, 'CalcTESCoilDischargeOnlyMode') )
    IF (TESCoil(TESCoilNum)%EvapSensCoolingRate  > TESCoil(TESCoilNum)%EvapTotCoolingRate) THEN
      TESCoil(TESCoilNum)%EvapSensCoolingRate = TESCoil(TESCoilNum)%EvapTotCoolingRate
    ENDIF
    TESCoil(TESCoilNum)%EvapSensCoolingEnergy=  TESCoil(TESCoilNum)%EvapSensCoolingRate * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%EvapLatCoolingRate   = TESCoil(TESCoilNum)%EvapTotCoolingRate &
                                              - TESCoil(TESCoilNum)%EvapSensCoolingRate
    TESCoil(TESCoilNum)%EvapLatCoolingEnergy =  TESCoil(TESCoilNum)%EvapLatCoolingRate * TimeStepSys  *SecInHour

    TESCoil(TESCoilNum)%QdotTES  = TotCap*RuntimeFraction + ElecCoolingPower ! all heat rejection into storage

  ELSE !coil is off; just pass through conditions
    TESCoil(TESCoilNum)%QdotTES  = 0.d0

    TESCoil(TESCoilNum)%ElecCoolingPower   = TESCoil(TESCoilNum)%AncillaryControlsPower
    TESCoil(TESCoilNum)%ElecCoolingEnergy  = TESCoil(TESCoilNum)%ElecCoolingPower * TimeStepSys  *SecInHour
    TESCoil(TESCoilNum)%RuntimeFraction    = 0.d0

    TESCoil(TESCoilNum)%RuntimeFraction      =  0.d0
    TESCoil(TESCoilNum)%EvapTotCoolingRate   =  0.d0
    TESCoil(TESCoilNum)%EvapTotCoolingEnergy =  0.d0
    TESCoil(TESCoilNum)%EvapSensCoolingRate  =  0.d0
    TESCoil(TESCoilNum)%EvapSensCoolingEnergy=  0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingRate   =  0.d0
    TESCoil(TESCoilNum)%EvapLatCoolingEnergy =  0.d0

    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%Temp
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%HumRat
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%EvapAirInletNodeNum)%MassFlowRate
    Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum)%Enthalpy      = &
                                                            PsyHFnTdbW(Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%Temp, &
                                                                       Node(TESCoil(TESCoilNum)%EvapAirOutletNodeNum )%HumRat, &
                                                                       'CalcTESCoilCoolingOnlyMode')
  ENDIF

  !nothing happens at condenser
  Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp         = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
  Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat       = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat
  Node(TESCoil(TESCoilNum)%CondAirInletNodeNum  )%MassFlowRate   = 0.d0
  Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%MassFlowRate = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%MassFlowRate
  Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Enthalpy      = &
                                                          PsyHFnTdbW(Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%Temp, &
                                                                      Node(TESCoil(TESCoilNum)%CondAirOutletNodeNum )%HumRat, &
                                                                      'CalcTESCoilCoolingOnlyMode')
  TESCoil(TESCoilNum)%CondInletTemp = Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%Temp
  TESCoil(TESCoilNum)%Q_TES   = TESCoil(TESCoilNum)%QdotTES * TimeStepSys * SecInHour
  CALL UpdateTEStorage(TESCoilNum)

  CALL UpdateColdWeatherProtection(TESCoilNum)

  IF (TESCoil(TESCoilNum)%CondenserType == EvapCooled) THEN
    CALL UpdateEvaporativeCondenserBasinHeater(TESCoilNum)
    CALL UpdateEvaporativeCondenserWaterUse(TESCoilNum, Node(TESCoil(TESCoilNum)%CondAirInletNodeNum)%HumRat, &
                                            TESCoil(TESCoilNum)%CondAirInletNodeNum )
  ENDIF

  RETURN

END SUBROUTINE CalcTESCoilDischargeOnlyMode


SUBROUTINE UpdateTEStorage(TESCoilNum)

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
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TESCoilNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  IF ( TESCoil(TESCoilNum)%StorageMedia == FluidBased) THEN
    CALL CalcTESWaterStorageTank( TESCoilNum )
  ELSEIF (TESCoil(TESCoilNum)%StorageMedia == IceBased) THEN
    CALL CalcTESIceStorageTank( TESCoilNum )
  ENDIF

  RETURN

END SUBROUTINE UpdateTEStorage

SUBROUTINE CalcTESWaterStorageTank( TESCoilNum )

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
  USE WaterThermalTanks ,  ONLY: CalcTankTemp, CalcTempIntegral
  USE DataHVACGlobals,     ONLY: SysTimeElapsed, TimeStepSys
  USE DataGlobals,         ONLY: TimeStep, TimeStepZone, HourOfDay
  USE FluidProperties,     ONLY: GetSpecificHeatGlycol, GetDensityGlycol
  USE DataPlant,           ONLY: PlantLoop
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TESCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64)  :: TimeElapsed         ! Fraction of the current hour that has elapsed (h)
  REAL(r64)           :: AmbientTemp         ! Current ambient air temperature around tank (C)
  REAL(r64)           :: TankMass            ! Mass of water in tank (kg)
  REAL(r64)           :: LossCoeff           ! Loss coefficient to ambient environment (W/K)
  REAL(r64)           :: TankTemp            ! Instantaneous tank temperature (C)
  REAL(r64)           :: NewTankTemp         ! Predicted new tank temperature (C)
  REAL(r64)           :: CpTank              ! Specific heat of water in tank (J/kg K)
  REAL(r64)           :: UseInletTemp        ! Use side inlet temperature (C)
  REAL(r64)           :: UseMassFlowRate     ! Use side flow rate, including effectiveness factor (kg/s)
  REAL(r64)           :: SourceInletTemp     ! Source side inlet temperature (C)
  REAL(r64)           :: SourceMassFlowRate  ! Source side flow rate, including effectiveness factor (kg/s)
  REAL(r64)           :: TimeRemaining       ! Time remaining in the current timestep (s)
  REAL(r64)           :: CpPlantConnection   ! Specific heat of fluid in plant connection (J/kg K)
  REAL(r64)           :: deltaTsum           ! Change in integrated tank temperature, dividing by time gives the average (C s)
  REAL(r64)           :: SecInTimeStep       ! Seconds in one timestep (s)
  REAL(r64)           :: rho                 ! density of water in tank (kg/m3)
  REAL(r64)           :: QdotTES             ! heat exchange directly into tank from charging system [W]
  REAL(r64)           :: NewOutletTemp       ! calculated new tankoutlet temp (C)

  SecInTimeStep = TimeStepSys * SecInHour
  TimeRemaining = SecInTimeStep

  TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed

  IF (TESCoil(TESCoilNum)%TimeElapsed /= TimeElapsed) THEN
    TESCoil(TESCoilNum)%FluidTankTempFinalLastTimestep = TESCoil(TESCoilNum)%FluidTankTempFinal
    TESCoil(TESCoilNum)%TimeElapsed = TimeElapsed
  ENDIF

  TankTemp        = TESCoil(TESCoilNum)%FluidTankTempFinalLastTimestep
  AmbientTemp     = Node(TESCoil(TESCoilNum)%StorageAmbientNodeNum)%Temp
  UseInletTemp    = Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum )%Temp
  SourceInletTemp = TESCoil(TESCoilNum)%FluidTankTempFinalLastTimestep
  rho             = GetDensityGlycol(TESCoil(TESCoilNum)%StorageFluidName, TankTemp, &
                                     TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESWaterStorageTank')
  TankMass        = rho * TESCoil(TESCoilNum)%FluidStorageVolume
  CpTank          = GetSpecificHeatGlycol(TESCoil(TESCoilNum)%StorageFluidName, TankTemp, &
                                           TESCoil(TESCoilNum)%StorageFluidIndex, 'CalcTESWaterStorageTank')

  IF (TESCoil(TESCoilNum)%TESPlantConnectionAvailable) THEN
    UseMassFlowRate = Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%MassFlowRate * &
                        TESCoil(TESCoilNum)%TESPlantEffectiveness
  ELSE
    UseMassFlowRate = 0.d0
  ENDIF
  SourceMassFlowRate = 0.d0
  LossCoeff      = TESCoil(TESCoilNum)%StorageUA
  QdotTES        = TESCoil(TESCoilNum)%QdotTES


  NewTankTemp = CalcTankTemp(TankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, &
                                     CpTank, UseMassFlowRate, SourceMassFlowRate, LossCoeff, QdotTES, TimeRemaining)

  TESCoil(TESCoilNum)%FluidTankTempFinal = NewTankTemp

  IF (TESCoil(TESCoilNum)%TESPlantConnectionAvailable) THEN
    CpPlantConnection = GetSpecificHeatGlycol(PlantLoop(TESCoil(TESCoilNum)%TESPlantLoopNum)%FluidName,            &
              Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%Temp, &
              PlantLoop(TESCoil(TESCoilNum)%TESPlantLoopNum)%FluidIndex, &
              'CalcTESIceStorageTank')

    TESCoil(TESCoilNum)%QdotPlant = Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%MassFlowRate * CpPlantConnection * &
                                      TESCoil(TESCoilNum)%TESPlantEffectiveness * &
                                     (UseInletTemp    - NewTankTemp )
    TESCoil(TESCoilNum)%Q_Plant = TESCoil(TESCoilNum)%QdotPlant * TimeStepSys * SecInHour
    ! now get correct outlet temp with actual massflow (not modified by effectiveness)
    IF (Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%MassFlowRate > MassFlowTolerance) THEN
      NewOutletTemp = UseInletTemp - TESCoil(TESCoilNum)%QdotPlant / &
                                  (Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%MassFlowRate * CpPlantConnection)
    ELSE
      NewOutletTemp = UseInletTemp
    ENDIF
    Node(TESCoil(TESCoilNum)%TESPlantOutletNodeNum)%Temp = NewOutletTemp
  ENDIF

  deltaTsum = CalcTempIntegral(TankTemp, NewTankTemp, AmbientTemp, UseInletTemp, SourceInletTemp, TankMass, CpTank, &
                                 UseMassFlowRate, SourceMassFlowRate, LossCoeff, QdotTES, TimeRemaining)
  TESCoil(TESCoilNum)%QdotAmbient = (LossCoeff * (AmbientTemp * TimeRemaining - deltaTsum))/SecInTimeStep
  TESCoil(TESCoilNum)%Q_Ambient =  TESCoil(TESCoilNum)%QdotAmbient * TimeStepSys * SecInHour


  RETURN

END SUBROUTINE CalcTESWaterStorageTank

SUBROUTINE CalcTESIceStorageTank(TESCoilNum)

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
  USE DataPlant, ONLY: PlantLoop
  USE FluidProperties,    ONLY:  GetSpecificHeatGlycol
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE DataGlobals,     ONLY: TimeStep, TimeStepZone, HourOfDay
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TESCoilNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64) :: FreezingTemp = 0.d0 ! zero degrees C

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: Cp ! local specific heat
  REAL(r64)  :: QdotIce ! local rate of heat transfer to ice (negative cooling) [W]
  REAL(r64)  :: TimeElapsed         ! Fraction of the current hour that has elapsed (h)
  REAL(r64)           :: NewOutletTemp       ! calculated new tankoutlet temp (C)

  TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed

  IF (TESCoil(TESCoilNum)%TimeElapsed /= TimeElapsed) THEN
    TESCoil(TESCoilNum)%IceFracRemainLastTimestep = TESCoil(TESCoilNum)%IceFracRemain
    TESCoil(TESCoilNum)%TimeElapsed = TimeElapsed
  ENDIF

  !update plant connection (if any)
  IF (TESCoil(TESCoilNum)%TESPlantConnectionAvailable) THEN
    Cp = GetSpecificHeatGlycol(PlantLoop(TESCoil(TESCoilNum)%TESPlantLoopNum)%FluidName,            &
              Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%Temp, &
              PlantLoop(TESCoil(TESCoilNum)%TESPlantLoopNum)%FluidIndex, &
              'CalcTESIceStorageTank')

    TESCoil(TESCoilNum)%QdotPlant = Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%MassFlowRate * Cp * &
                                      TESCoil(TESCoilNum)%TESPlantEffectiveness * &
                                     (Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%Temp  &
                                       - FreezingTemp )
    TESCoil(TESCoilNum)%Q_Plant = TESCoil(TESCoilNum)%QdotPlant * TimeStepSys * SecInHour
    ! now get correct outlet temp with actual massflow (not modified by effectiveness)
    IF (Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%MassFlowRate > MassFlowTolerance) THEN
      NewOutletTemp = Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%Temp + TESCoil(TESCoilNum)%QdotPlant / &
                                  (Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%MassFlowRate * Cp)
    ELSE
      NewOutletTemp = Node(TESCoil(TESCoilNum)%TESPlantInletNodeNum)%Temp
    ENDIF
    Node(TESCoil(TESCoilNum)%TESPlantOutletNodeNum)%Temp = NewOutletTemp
  ELSE
    TESCoil(TESCoilNum)%QdotPlant = 0.d0
    TESCoil(TESCoilNum)%Q_Plant = 0.d0
  ENDIF

  ! update ambient heat transfer

  TESCoil(TESCoilNum)%QdotAmbient = TESCoil(TESCoilNum)%StorageUA * (Node(TESCoil(TESCoilNum)%StorageAmbientNodeNum)%Temp &
                                            - FreezingTemp)
  TESCoil(TESCoilNum)%Q_Ambient =   TESCoil(TESCoilNum)%QdotAmbient * TimeStepSys * SecInHour

  QdotIce = TESCoil(TESCoilNum)%QdotPlant + TESCoil(TESCoilNum)%QdotAmbient + TESCoil(TESCoilNum)%QdotTES

  IF (QdotIce < 0.d0) THEN ! charging ice level
    TESCoil(TESCoilNum)%IceFracRemain = TESCoil(TESCoilNum)%IceFracRemainLastTimestep + &
                                          ABS(QdotIce)/( TESCoil(TESCoilNum)%IceStorageCapacity/(TimeStepSys*SecInHour)  )
    IF (TESCoil(TESCoilNum)%IceFracRemain > 1.d0) TESCoil(TESCoilNum)%IceFracRemain = 1.d0
  ELSE ! not charging,but discharging
    TESCoil(TESCoilNum)%IceFracRemain = TESCoil(TESCoilNum)%IceFracRemainLastTimestep - &
                                          QdotIce/( TESCoil(TESCoilNum)%IceStorageCapacity/(TimeStepSys*SecInHour)  )
    IF (TESCoil(TESCoilNum)%IceFracRemain < 0.d0) TESCoil(TESCoilNum)%IceFracRemain = 0.d0
  ENDIF

  RETURN

END SUBROUTINE CalcTESIceStorageTank


SUBROUTINE UpdateColdWeatherProtection(TESCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TESCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na


  IF ((Node(TESCoil(TESCoilNum)%StorageAmbientNodeNum)%Temp < TESCoil(TESCoilNum)%ColdWeatherMinimumTempLimit) &
      .AND. (GetCurrentScheduleValue(TESCoil(TESCoilNum)%AvailSchedNum) /= 0.d0)) THEN
    TESCoil(TESCoilNum)%ElectColdWeatherPower = TESCoil(TESCoilNum)%ColdWeatherAncillaryPower

  ELSE
    TESCoil(TESCoilNum)%ElectColdWeatherPower = 0.d0
  ENDIF
  TESCoil(TESCoilNum)%ElectColdWeatherEnergy = TESCoil(TESCoilNum)%ElectColdWeatherPower * TimeStepSys  *SecInHour

  RETURN

END SUBROUTINE UpdateColdWeatherProtection

SUBROUTINE UpdateEvaporativeCondenserBasinHeater(TESCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! determine basin heater electrical power and energy

          ! METHODOLOGY EMPLOYED:
          ! call general worker routine

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals , ONLY : TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TESCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  CALL CalcBasinHeaterPower(TESCoil(TESCoilNum)%BasinHeaterPowerFTempDiff,   &
                            TESCoil(TESCoilNum)%BasinHeaterAvailSchedNum,    &
                            TESCoil(TESCoilNum)%BasinHeaterSetpointTemp,     &
                            TESCoil(TESCoilNum)%ElectEvapCondBasinHeaterPower)

  TESCoil(TESCoilNum)%ElectEvapCondBasinHeaterEnergy = TESCoil(TESCoilNum)%ElectEvapCondBasinHeaterPower &
                                                        * TimeStepSys  *SecInHour

  RETURN

END SUBROUTINE UpdateEvaporativeCondenserBasinHeater

SUBROUTINE UpdateEvaporativeCondenserWaterUse(TESCoilNum, HumRatAfterEvap, InletNodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update and calculate water consumption for evaporatively cooled condensers

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataWater,       ONLY: WaterStorage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: TESCoilNum
  REAL(r64), INTENT(IN) :: HumRatAfterEvap
  INTEGER,   INTENT(IN) :: InletNodeNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: AvailWaterRate
  REAL(r64) :: RhoWater


  RhoWater = RhoH2O(Node(InletNodeNum)%Temp)
  TESCoil(TESCoilNum)%EvapWaterConsumpRate =  &
             (HumRatAfterEvap - Node(InletNodeNum)%HumRat) *  &
              Node(InletNodeNum)%MassFlowRate/RhoWater * TESCoil(TESCoilNum)%CondenserRuntimeFraction

  ! Set the demand request for supply water from water storage tank (if needed)
  IF (TESCoil(TESCoilNum)%EvapWaterSupplyMode == WaterSupplyFromTank) THEN
    WaterStorage(TESCoil(TESCoilNum)%EvapWaterSupTankID)%VdotRequestDemand(TESCoil(TESCoilNum)%EvapWaterTankDemandARRID) &
      = TESCoil(TESCoilNum)%EvapWaterConsumpRate
  ENDIF

  !check if should be starved by restricted flow from tank
  IF (TESCoil(TESCoilNum)%EvapWaterSupplyMode == WaterSupplyFromTank) THEN
    AvailWaterRate = &
      WaterStorage(TESCoil(TESCoilNum)%EvapWaterSupTankID)%VdotAvailDemand(TESCoil(TESCoilNum)%EvapWaterTankDemandARRID)
    IF (AvailWaterRate < TESCoil(TESCoilNum)%EvapWaterConsumpRate) THEN
      TESCoil(TESCoilNum)%EvapWaterStarvMakupRate = TESCoil(TESCoilNum)%EvapWaterConsumpRate - AvailWaterRate
      TESCoil(TESCoilNum)%EvapWaterConsumpRate = AvailWaterRate
    ELSE
      TESCoil(TESCoilNum)%EvapWaterStarvMakupRate = 0.d0
    ENDIF
  ENDIF

  TESCoil(TESCoilNum)%EvapWaterConsump = TESCoil(TESCoilNum)%EvapWaterConsumpRate * TimeStepSys * SecInHour
  TESCoil(TESCoilNum)%EvapWaterStarvMakup = TESCoil(TESCoilNum)%EvapWaterStarvMakupRate * TimeStepSys * SecInHour
  TESCoil(TESCoilNum)%EvapCondPumpElecPower = TESCoil(TESCoilNum)%EvapCondPumpElecNomPower * &
                                                   TESCoil(TESCoilNum)%CondenserRuntimeFraction
  TESCoil(TESCoilNum)%EvapCondPumpElecConsumption = TESCoil(TESCoilNum)%EvapCondPumpElecPower * TimeStepSys * SecInHour

  RETURN

END SUBROUTINE UpdateEvaporativeCondenserWaterUse

SUBROUTINE GetTESCoilIndex(CoilName,CoilIndex,ErrorsFound,CurrentModuleObject)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   August 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets an index for a given TES Cooling Coil -- issues error message if that
          ! coil is not a legal TES Cooling Coil.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem
  USE DataInterfaces,    ONLY: ShowSevereError


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilName
  INTEGER, INTENT(INOUT)       :: CoilIndex
  LOGICAL, INTENT(INOUT)       :: ErrorsFound
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: CurrentModuleObject


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and allocates TESCoil related parameters from input file
  IF (GetTESInputFlag) THEN  ! First time subroutine has been called, get input data
    CALL GetTESCoilInput
    GetTESInputFlag=.FALSE. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(NumTESCoils .GT. 0)THEN
    CoilIndex = FindItem(CoilName,TESCoil%Name,NumTESCoils)
  ELSE
    CoilIndex = 0
  END IF

  IF (CoilIndex == 0) THEN
    IF (PRESENT(CurrentModuleObject)) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', GetTESCoilIndex: TES Cooling Coil not found='//TRIM(CoilName))
    ELSE
      CALL ShowSevereError('GetTESCoilIndex: TES Cooling Coil not found='//TRIM(CoilName))
    ENDIF
    ErrorsFound = .TRUE.
  ENDIF


  RETURN


END SUBROUTINE GetTESCoilIndex

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


END MODULE PackagedThermalStorageCoil

