MODULE EvaporativeFluidCoolers

  ! Module containing the routines dealing with the objects EvaporativeFluidCooler:SingleSpeed and
  ! EvaporativeFluidCooler:TwoSpeed

  ! MODULE INFORMATION:
  !       AUTHOR         Chandan Sharma
  !       DATE WRITTEN   May 2009
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Model the performance of evaporative fluid coolers

  ! METHODOLOGY EMPLOYED:
  ! Based on cooling tower by Shirey, Raustad: Dec 2000; Shirey, Sept 2002

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals,    ONLY: MaxNameLength, KelvinConv, SecInHour, WarmupFlag, InitConvTemp
USE DataInterfaces
USE DataHVACGlobals
USE DataLoopNode
USE DataEnvironment, ONLY: StdBaroPress, OutDryBulbTemp, OutHumRat, OutBaroPress, OutWetBulbTemp
USE General,         ONLY: TrimSigDigits
USE DataPlant,       ONLY: PlantLoop
USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  ! Use statements for access to subroutines in other modules
USE Psychrometrics,  ONLY: PsyWFnTdbTwbPb, PsyRhoAirFnPbTdbW, PsyHFnTdbRhPb, PsyCpAirFnWTdb, &
                           PsyTsatFnHPb, PsyWFnTdbH
USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS

CHARACTER(len=*), PARAMETER :: cEvapFluidCooler_SingleSpeed = 'EvaporativeFluidCooler:SingleSpeed'
CHARACTER(len=*), PARAMETER :: cEvapFluidCooler_TwoSpeed = 'EvaporativeFluidCooler:TwoSpeed'

INTEGER, PARAMETER :: EvapLossByUserFactor = 80
INTEGER, PARAMETER :: EvapLossByMoistTheory = 81

INTEGER, PARAMETER :: BlowdownByConcentration = 90
INTEGER, PARAMETER :: BlowdownBySchedule = 91

INTEGER, PARAMETER :: PIM_StandardDesignCapacity      = 1
INTEGER, PARAMETER :: PIM_UFactor                     = 2
INTEGER, PARAMETER :: PIM_UserSpecifiedDesignCapacity = 3

INTEGER, PARAMETER :: EvapFluidCooler_SingleSpeed = 1
INTEGER, PARAMETER :: EvapFluidCooler_TwoSpeed    = 2

CHARACTER(Len=1), PARAMETER    :: DummyString = ' '

  ! DERIVED TYPE DEFINITIONS
TYPE EvapFluidCoolerspecs
  CHARACTER(len=MaxNameLength) :: Name                       = ' ' ! User identifier
  CHARACTER(len=MaxNameLength) :: EvapFluidCoolerType        = ' ' ! Type of evaporative fluid cooler
  INTEGER                      :: EvapFluidCoolerType_Num    = 0
  INTEGER                      :: PerformanceInputMethod_Num = 0
  LOGICAL    :: Available                  = .TRUE. ! need an array of logicals--load identifiers of available equipment
  LOGICAL    :: ON                         = .TRUE. ! Simulate the machine at it's operating part load ratio
  REAL(r64)  :: DesignWaterFlowRate           = 0.0d0 ! Design water flow rate through the evaporative fluid cooler [m3/s]
  REAL(r64)  :: DesignSprayWaterFlowRate      = 0.0d0 ! Design spray water flow rate through the evaporative fluid cooler [m3/s]
  REAL(r64)  :: DesWaterMassFlowRate          = 0.0d0 ! Design water flow rate through the evaporative fluid cooler [kg/s]
  REAL(r64)  :: HighSpeedAirFlowRate          = 0.0d0 ! Air flow rate through evaporative fluid cooler at high speed [m3/s]
  REAL(r64)  :: HighSpeedFanPower             = 0.0d0 ! Fan power at high fan speed [W]
  REAL(r64)  :: HighSpeedEvapFluidCoolerUA    = 0.0d0 ! UA of evaporative fluid cooler at high fan speed [W/C]
  REAL(r64)  :: LowSpeedAirFlowRate           = 0.0d0 ! Air flow rate through evaporative fluid cooler at low speed [m3/s]
  REAL(r64)  :: LowSpeedAirFlowRateSizingFactor =0.0d0 ! sizing factor for low speed air flow rate []
  REAL(r64)  :: LowSpeedFanPower              = 0.0d0 ! Fan power at low fan speed [W]
  REAL(r64)  :: LowSpeedFanPowerSizingFactor  = 0.0d0 ! Sizing factor for low speed fan power []
  REAL(r64)  :: LowSpeedEvapFluidCoolerUA     = 0.0d0 ! UA of evaporative fluid cooler at low fan speed [W/C]
  REAL(r64)  :: LowSpeedEvapFluidCoolerUASizingFactor = 0.0d0 ! sizing factor for low speed UA []
  REAL(r64)  :: DesignEnteringWaterTemp       = 0.0d0 ! Entering water temperature at design conditions
  REAL(r64)  :: DesignEnteringAirTemp         = 0.0d0 ! Design inlet air dry-bulb temperature (C)
  REAL(r64)  :: DesignEnteringAirWetbulbTemp  = 0.0d0 ! Design inlet air wet-bulb temperature (C)
  REAL(r64)  :: EvapFluidCoolerMassFlowRateMultiplier     = 0.0d0 ! Maximum evaporative fluid cooler flow rate is
                                                                ! this multiplier times design flow rate
  REAL(r64)  :: HeatRejectCapNomCapSizingRatio  = 0.0d0 ! ratio of actual cap to nominal capacity []
  REAL(r64)  :: HighSpeedStandardDesignCapacity = 0.0d0 ! Standard Design Capacity of the evaporative fluid cooler [W]
                                                      ! with entering water at 35C (95F),
                                                      !  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
                                                      !  temp and 35C (95F) dry-bulb temp, and water flow
                                                      !  rate of 5.382E-8 m3/s per watt (3 gpm/ton)
  REAL(r64)  :: LowSpeedStandardDesignCapacity = 0.0d0  ! Standard Design Capacity of the evaporative fluid cooler [W]
                                                      ! with entering water at 35C (95F),
                                                      !  leaving water at 29.44C (85F), entering air at 25.56C (78F) wet-bulb
                                                      !  temp and 35C (95F) dry-bulb temp, and water flow
                                                      !  rate of 5.382E-8 m3/s per watt (3 gpm/ton)
  REAL(r64)  :: LowSpeedStandardDesignCapacitySizingFactor = 0.0d0 ! sizing factor for low speed capacity []
  REAL(r64)  :: HighSpeedUserSpecifiedDesignCapacity = 0.0d0 ! User specified design capacity [W]
  REAL(r64)  :: LowSpeedUserSpecifiedDesignCapacity  = 0.0d0 ! User specified design capacity for at low speed for
                                                           ! two speed fluid cooler[W]
  REAL(r64)  :: LowSpeedUserSpecifiedDesignCapacitySizingFactor = 0.0d0 ! sizing factor for low speed user capacity []
  REAL(r64)  :: Concentration = 0.0d0 ! fluid/glycol concentration - percent
  INTEGER    :: FluidIndex    = 0   ! Index to Property arrays
  REAL(r64)  :: SizFac        = 0.0d0                       !  sizing factor
  INTEGER    :: WaterInletNodeNum               = 0   ! Node number on the water inlet side of the evaporative fluid cooler
  INTEGER    :: WaterOutletNodeNum              = 0   ! Node number on the water outlet side of the evaporative fluid cooler
  INTEGER    :: OutdoorAirInletNodeNum          = 0   ! Node number of outdoor air inlet for the evaporative fluid cooler
  INTEGER    :: BlowDownSchedulePtr             = 0   ! Pointer to blow down schedule
  INTEGER    :: HighMassFlowErrorCount          = 0   ! Counter when mass flow rate is >
                                                      ! Design*EvapFluidCoolerMassFlowRateMultiplier
  INTEGER    :: HighMassFlowErrorIndex          = 0   ! Index for high mass flow recurring error message
  INTEGER    :: OutletWaterTempErrorCount       = 0   ! Counter when outlet water temperature is < minimum allowed temperature
  INTEGER    :: OutletWaterTempErrorIndex       = 0   ! Index for outlet water temperature recurring error message
  INTEGER    :: SmallWaterMassFlowErrorCount    = 0   ! Counter when water mass flow rate is very small
  INTEGER    :: SmallWaterMassFlowErrorIndex    = 0   ! Index for very small water mass flow rate recurring error message
  INTEGER    :: WMFRLessThanMinAvailErrCount    = 0   ! Counter when water mass flow rate is less than minimum available
  INTEGER    :: WMFRLessThanMinAvailErrIndex    = 0   ! Index for water mass flow rate less than minavail recurring message
  INTEGER    :: WMFRGreaterThanMaxAvailErrCount = 0   ! Counter when water mass flow rate is greater than minimum available
  INTEGER    :: WMFRGreaterThanMaxAvailErrIndex = 0   ! Index for water mass flow rate > minavail recurring message
  INTEGER    :: EvapFluidCoolerAFRRFailedCount  = 0   ! Counter for air flow rate ratio out of bounds error
  INTEGER    :: EvapFluidCoolerAFRRFailedIndex  = 0   ! Index for air flow rate ratio out of bounds error

  !fluid bypass
  INTEGER    :: CapacityControl                 = 0   ! Type of capacity control for single speed cooling tower:
                                                      !  0 - FanCycling, 1 - FluidBypass
  REAL(r64)  :: BypassFraction                  = 0.0d0 ! Fraction of fluid bypass as a ratio of total fluid flow
                                                      !  through the tower sump


  !begin water system interactions
  INTEGER    :: EvapLossMode = EvapLossByMoistTheory   ! sets how evaporative fluid cooler water evaporation is modeled
  INTEGER    :: BlowdownMode = BlowdownByConcentration ! sets how evaporative fluid cooler water blowdown is modeled
  INTEGER    :: SchedIDBlowdown       = 0              ! index "pointer" to schedule of blowdown in [m3/s]
  INTEGER    :: WaterTankID           = 0              ! index "pointer" to WaterStorage structure
  INTEGER    :: WaterTankDemandARRID  = 0              ! index "pointer" to demand array inside WaterStorage structure
  REAL(r64)  :: UserEvapLossFactor    = 0.0d0            ! simple model [%/Delt C]
  REAL(r64)  :: DriftLossFraction     = 0.0d0
  REAL(r64)  :: ConcentrationRatio    = 0.0d0            ! ratio of solids in blowdown vs make up water
  LOGICAL    :: SuppliedByWaterSystem = .false.


  !end water system variables

  !loop topology variables
  INTEGER    :: LoopNum     = 0
  INTEGER    :: LoopSideNum = 0
  INTEGER    :: BranchNum   = 0
  INTEGER    :: CompNum     = 0

END TYPE EvapFluidCoolerspecs

TYPE EvapFluidCoolerInletConds
  REAL(r64) :: WaterTemp      = 0.0d0  ! Evaporative fluid cooler water inlet temperature (C)
  REAL(r64) :: AirTemp        = 0.0d0  ! Evaporative fluid cooler air inlet dry-bulb temperature (C)
  REAL(r64) :: AirWetBulb     = 0.0d0  ! Evaporative fluid cooler air inlet wet-bulb temperature (C)
  REAL(r64) :: AirPress       = 0.0d0  ! Evaporative fluid cooler air barometric pressure
  REAL(r64) :: AirHumRat      = 0.0d0  ! Evaporative fluid cooler air inlet humidity ratio (kg/kg)
END TYPE EvapFluidCoolerInletConds

TYPE ReportVars
  REAL(r64)    :: InletWaterTemp         = 0.0d0  ! Evaporative fluid cooler inlet water temperature (C)
  REAL(r64)    :: OutletWaterTemp        = 0.0d0  ! Evaporative fluid cooler outlet water temperature (C)
  REAL(r64)    :: WaterMassFlowRate      = 0.0d0  ! Evaporative fluid cooler water mass flow rate (m3/s)
  REAL(r64)    :: Qactual                = 0.0d0  ! Evaporative fluid cooler heat rejection rate (W)
  REAL(r64)    :: FanPower               = 0.0d0  ! Evaporative fluid cooler fan power (W)
  REAL(r64)    :: FanEnergy              = 0.0d0  ! Evaporative fluid cooler fan energy consumption (J)
  REAL(r64)    :: AirFlowRatio           = 0.0d0  ! Air flow ratio through variable speed evaporative fluid cooler
  REAL(r64)    :: WaterAmountUsed        = 0.0d0  ! Evaporative fluid cooler make up water usage (m3)
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
END TYPE ReportVars

  ! MODULE VARIABLE DECLARATIONS:
INTEGER           :: NumSimpleEvapFluidCoolers          = 0      ! Number of similar evaporative fluid coolers

! The following block of variables are used to carry model results for a evaporative fluid cooler instance
!   across sim, update, and report routines.  Simulation manager must be careful
!   in models with multiple evaporative fluid coolers.

REAL(r64)         :: InletWaterTemp           = 0.0d0    ! CW temperature at evaporative fluid cooler inlet
REAL(r64)         :: OutletWaterTemp          = 0.0d0    ! CW temperature at evaporative fluid cooler outlet
INTEGER           :: WaterInletNode           = 0      ! Node number at evaporative fluid cooler inlet
INTEGER           :: WaterOutletNode          = 0      ! Node number at evaporative fluid cooler outlet
REAL(r64)         :: WaterMassFlowRate        = 0.0d0    ! WaterMassFlowRate through evaporative fluid cooler
!DSU this is plant level stuff now REAL(r64)   :: EvapFluidCoolerMassFlowRateMax     = 0.0d0    ! Max Hardware Mass Flow Rate
!DSU this is plant level stuff now REAL(r64)   :: EvapFluidCoolerMassFlowRateMin     = 0.0d0    ! Min Hardware Mass Flow Rate
!DSU this is plant level stuff now REAL(r64)   :: LoopMassFlowRateMaxAvail = 0.0d0    ! Max Loop Mass Flow Rate available
!DSU this is plant level stuff now REAL(r64)   :: LoopMassFlowRateMinAvail = 0.0d0    ! Min Loop Mass Flow Rate available
REAL(r64)         :: Qactual                  = 0.0d0    ! Evaporative fluid cooler heat transfer
REAL(r64)         :: FanPower                 = 0.0d0    ! Evaporative fluid cooler fan power used
REAL(r64)         :: AirFlowRateRatio         = 0.0d0    ! Ratio of air flow rate through VS evaporative fluid cooler
                                                       ! to design air flow rate
REAL(r64)         :: WaterUsage               = 0.0d0    ! Evaporative fluid cooler water usage (m3/s)


TYPE (EvapFluidCoolerspecs),      ALLOCATABLE, DIMENSION(:) :: SimpleEvapFluidCooler       ! dimension to number of machines
TYPE (EvapFluidCoolerInletConds), ALLOCATABLE, DIMENSION(:) :: SimpleEvapFluidCoolerInlet  ! inlet conditions
TYPE (ReportVars),      ALLOCATABLE, DIMENSION(:) :: SimpleEvapFluidCoolerReport   ! report variables
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE EvaporativeFluidCoolers

          ! Driver/Manager Routines
PUBLIC     SimEvapFluidCoolers      ! PlantCondLoopSupplySideManager calls this top level Subroutine

          ! Get Input routines for module
PRIVATE    GetEvapFluidCoolerInput  ! Retrieves inputs for specified evaporative fluid cooler

          ! Initialization routines for module
PRIVATE    InitEvapFluidCooler      ! Initializes evaporative fluid cooler variables
PRIVATE    InitSimVars              ! Initializes model level variables
PRIVATE    SizeEvapFluidCooler      ! Automatically sizes the evaporative fluid cooler;
                                    ! also, calculates UA based on Standard Design Capacity input(s)

          ! Update routines to check convergence and update nodes
PRIVATE    SimSimpleEvapFluidCooler        ! Calculates exiting water temperature of evaporative fluid cooler
PRIVATE    CalcSingleSpeedEvapFluidCooler  ! Simulates a single speed evaporative fluid cooler using SimSimpleEvapFluidCooler
PRIVATE    CalcTwoSpeedEvapFluidCooler     ! Simulates a two speed evaporative fluid cooler using SimSimpleEvapFluidCooler
PRIVATE    SimpleEvapFluidCoolerUAResidual ! Given a design evaporative fluid cooler load and a UA, calculates a residual
PRIVATE    CalculateWaterUseage            ! calculates water consumed by the different evaporative fluid coolers
PRIVATE    UpdateEvapFluidCooler           ! Updates node information and checks mass flow rate
PRIVATE    ReportEvapFluidCooler           ! Outputs report variables

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of EvaporativeFluidCoolers Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimEvapFluidCoolers(EvapFluidCoolerType,EvapFluidCoolerName, CompIndex,  &
   RunFlag,InitLoopEquip, MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Main evaporative fluid cooler driver subroutine.  Gets called from
          ! PlantCondLoopSupplySideManager.

          ! METHODOLOGY EMPLOYED:
          ! After being called by PlantCondLoopSupplySideManager, this subroutine
          ! calls GetEvapFluidCoolerInput to get all evaporative fluid cooler input info (one time only),
          ! then calls the appropriate subroutine to calculate evaporative fluid cooler performance,
          ! update records (node info) and writes output report info.

          ! REFERENCES:
          ! Based on SimTowers subroutine by Fred Buhl, May 2002

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),INTENT(IN) :: EvapFluidCoolerType
  CHARACTER(len=*),INTENT(IN) :: EvapFluidCoolerName
  INTEGER,INTENT(INOUT)       :: CompIndex
  LOGICAL,INTENT(INOUT)       :: RunFlag
  LOGICAL, INTENT(IN)         :: InitLoopEquip
  REAL(r64),INTENT(INOUT)     :: OptCap
  REAL(r64),INTENT(INOUT)     :: MaxCap
  REAL(r64),INTENT(INOUT)     :: MinCap
  LOGICAL, INTENT(IN)         :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64),INTENT(INOUT)     :: SizingFactor     ! sizing factor

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE   :: GetInput = .TRUE.
  INTEGER         :: EvapFluidCoolerNum !Pointer to EvapFluidCooler

          !GET INPUT
  IF (GetInput) THEN
    CALL GetEvapFluidCoolerInput
    GetInput = .FALSE.
  END IF

    ! Find the correct EvapCooler
  IF (CompIndex == 0) THEN
    EvapFluidCoolerNum = FindItemInList(EvapFluidCoolerName,SimpleEvapFluidCooler%Name,NumSimpleEvapFluidCoolers)
    IF (EvapFluidCoolerNum == 0) THEN
      CALL ShowFatalError('SimEvapFluidCoolers: Unit not found = '//TRIM(EvapFluidCoolerName))
    ENDIF
    CompIndex=EvapFluidCoolerNum
  ELSE
    EvapFluidCoolerNum=CompIndex
    IF (EvapFluidCoolerNum > NumSimpleEvapFluidCoolers .or. EvapFluidCoolerNum < 1) THEN
      CALL ShowFatalError('SimEvapFluidCoolers:  Invalid CompIndex passed = '//  &
                          TRIM(TrimSigDigits(EvapFluidCoolerNum))// &
                          ', Number of Units = '//TRIM(TrimSigDigits(NumSimpleEvapFluidCoolers))//  &
                          ', Entered Unit name = '//TRIM(EvapFluidCoolerName))
    ENDIF
    IF (CheckEquipName(EvapFluidCoolerNum)) THEN
      IF (EvapFluidCoolerName /= SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name) THEN
        CALL ShowFatalError('SimEvapFluidCoolers: Invalid CompIndex passed = '//  &
                            TRIM(TrimSigDigits(EvapFluidCoolerNum))// &
                            ', Unit name = '//TRIM(EvapFluidCoolerName)//', stored Unit Name for that index = '//  &
                            TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
      ENDIF
      CheckEquipName(EvapFluidCoolerNum)=.false.
    ENDIF
  ENDIF

          !INITIALIZE
  CALL InitSimVars

          !CALCULATE
  TypeOfEquip:   &
    SELECT CASE (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num)

      CASE (EvapFluidCooler_SingleSpeed)

        IF (InitLoopEquip) THEN
          CALL InitEvapFluidCooler(EvapFluidCoolerNum, RunFlag)
          CALL SizeEvapFluidCooler(EvapFluidCoolerNum)
          MinCap = 0.0d0 ! signifies non-load based model (i.e. forward
          MaxCap = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity &
                                  * SimpleEvapFluidCooler(EvapFluidCoolerNum)%HeatRejectCapNomCapSizingRatio
          OptCap = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity
          IF (GetSizingFactor) THEN
            SizingFactor = SimpleEvapFluidCooler(EvapFluidCoolerNum)%SizFac
          END IF
          RETURN
        END IF
        CALL InitEvapFluidCooler(EvapFluidCoolerNum, RunFlag)
        CALL CalcSingleSpeedEvapFluidCooler(EvapFluidCoolerNum)
        CALL CalculateWaterUseage(EvapFluidCoolerNum)
        CALL UpdateEvapFluidCooler(EvapFluidCoolerNum)
        CALL ReportEvapFluidCooler(RunFlag,EvapFluidCoolerNum)

      CASE (EvapFluidCooler_TwoSpeed)
        IF (GetSizingFactor) THEN
          SizingFactor = SimpleEvapFluidCooler(EvapFluidCoolerNum)%SizFac
          RETURN
        END IF
        IF (InitLoopEquip) THEN
          CALL InitEvapFluidCooler(EvapFluidCoolerNum, RunFlag)
          CALL SizeEvapFluidCooler(EvapFluidCoolerNum)
          MinCap = 0.0d0 ! signifies non-load based model (i.e. forward
          MaxCap = 0.0d0 ! heat exhanger model)
          OptCap = 0.0d0
          RETURN
        END IF
        CALL InitEvapFluidCooler(EvapFluidCoolerNum, RunFlag)
        CALL CalcTwoSpeedEvapFluidCooler(EvapFluidCoolerNum)
        CALL CalculateWaterUseage(EvapFluidCoolerNum)
        CALL UpdateEvapFluidCooler(EvapFluidCoolerNum)
        CALL ReportEvapFluidCooler(RunFlag,EvapFluidCoolerNum)

      CASE DEFAULT
        CALL ShowFatalError('SimEvapFluidCoolers: Invalid evaporative fluid cooler Type Requested = '&
                                                                           //TRIM(EvapFluidCoolerType))

    END SELECT TypeOfEquip

RETURN
END SUBROUTINE SimEvapFluidCoolers

! End EvaporativeFluidCoolers Module Driver Subroutines
!******************************************************************************


! Beginning of EvaporativeFluidCoolers Module Get Input subroutines
!******************************************************************************

SUBROUTINE GetEvapFluidCoolerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Chandan Sharma
          !       DATE WRITTEN:    May 2009
          !       MODIFIED         Chandan Sharma, April 2010
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for evaporative fluid coolers and stores it in SimpleEvapFluidCooler data structure.

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in the data.

          ! REFERENCES:
          ! Based on GetTowerInput subroutine from Don Shirey, Jan 2001 and Sept/Oct 2002
          ! B.A. Qureshi and S.M. Zubair , Prediction of evaporation losses in evaporative fluid coolers
          ! Applied thermal engineering 27 (2007) 520-527

          ! USE STATEMENTS:
  USE DataSizing
  USE DataLoopNode
!  USE DataPlant,          ONLY: PlantLoop
  USE InputProcessor,     ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, MakeUPPERCase
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE NodeInputManager,   ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE CurveManager,       ONLY: GetCurveIndex
  USE ScheduleManager,    ONLY: GetScheduleIndex
  USE WaterManager ,      ONLY: SetupTankDemandComponent
  USE OutAirNodeManager,  ONLY: CheckOutAirNodeNumber
  USE General,            ONLY: TrimSigDigits
  USE FluidProperties,    ONLY: CheckFluidPropertyName, FindGlycol, GetGlycolNameByIndex
  USE DataEnvironment,    ONLY: OutWetBulbTemp,OutDryBulbTemp,OutRelHumValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                   :: EvapFluidCoolerNum                ! Evaporative fluid cooler number,
                                                                   ! reference counter for SimpleEvapFluidCooler data array
    INTEGER                   :: NumSingleSpeedEvapFluidCoolers    ! Total number of single-speed evaporative fluid coolers
    INTEGER                   :: SingleSpeedEvapFluidCoolerNumber  ! Specific single-speed evaporative fluid cooler of interest
    INTEGER                   :: NumTwoSpeedEvapFluidCoolers       ! Number of two-speed evaporative fluid coolers
    INTEGER                   :: TwoSpeedEvapFluidCoolerNumber     ! Specific two-speed evaporative fluid cooler of interest
    INTEGER                   :: NumAlphas                 ! Number of elements in the alpha array
    INTEGER                   :: NumNums                   ! Number of elements in the numeric array
    INTEGER                   :: IOStat                    ! IO Status when calling get input subroutine
    LOGICAL                   :: IsNotOK                   ! Flag to verify name
    LOGICAL                   :: IsBlank                   ! Flag for blank name
    LOGICAL, SAVE             :: ErrorsFound=.false.       ! Logical flag set .true. if errors found while getting input data
    REAL(r64), DIMENSION(25)  :: NumArray                  ! Numeric input data array
    CHARACTER(len=MaxNameLength),DIMENSION(13) :: AlphArray  ! Character string input data array
    CHARACTER(len=MaxNameLength) :: FluidName

  ! Get number of all evaporative fluid coolers specified in the input data file (idf)
  NumSingleSpeedEvapFluidCoolers   = GetNumObjectsFound(cEvapFluidCooler_SingleSpeed)
  NumTwoSpeedEvapFluidCoolers      = GetNumObjectsFound(cEvapFluidCooler_TwoSpeed)
  NumSimpleEvapFluidCoolers        = NumSingleSpeedEvapFluidCoolers + NumTwoSpeedEvapFluidCoolers

  IF (NumSimpleEvapFluidCoolers <=0 ) &
    CALL ShowFatalError('No evaporative fluid cooler objects found in input, however, '//  &
      'a branch object has specified an evaporative fluid cooler. '//&
      'Search the input for evaporative fluid cooler to determine the cause for this error.')

  ! See if load distribution manager has already gotten the input
  IF (ALLOCATED(SimpleEvapFluidCooler))RETURN

  ! Allocate data structures to hold evaporative fluid cooler input data,
  ! report data and evaporative fluid cooler inlet conditions
  ALLOCATE (SimpleEvapFluidCooler(NumSimpleEvapFluidCoolers))
  ALLOCATE (SimpleEvapFluidCoolerReport(NumSimpleEvapFluidCoolers))
  ALLOCATE (SimpleEvapFluidCoolerInlet(NumSimpleEvapFluidCoolers))
  ALLOCATE (CheckEquipName(NumSimpleEvapFluidCoolers))
  CheckEquipName=.true.

  ! Load data structures with evaporative fluid cooler input data
  cCurrentModuleObject = cEvapFluidCooler_SingleSpeed
  DO SingleSpeedEvapFluidCoolerNumber = 1 , NumSingleSpeedEvapFluidCoolers
    EvapFluidCoolerNum = SingleSpeedEvapFluidCoolerNumber
    CALL GetObjectItem(cCurrentModuleObject,SingleSpeedEvapFluidCoolerNumber,AlphArray,NumAlphas, &
                       NumArray,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),SimpleEvapFluidCooler%Name,EvapFluidCoolerNum-1,IsNotOK,IsBlank,  &
       TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name                     = AlphArray(1)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType = TRIM(cCurrentModuleObject)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num = EvapFluidCooler_SingleSpeed
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerMassFlowRateMultiplier = 2.5d0
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterInletNodeNum        = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterOutletNodeNum       = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Chilled Water Nodes')
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate                 = NumArray(1)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower                    = NumArray(2)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignSprayWaterFlowRate             = NumArray(3)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HeatRejectCapNomCapSizingRatio       = NumArray(4)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity      = NumArray(5)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA           = NumArray(6)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate                  = NumArray(7)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedUserSpecifiedDesignCapacity = NumArray(8)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp              = NumArray(9)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirTemp                = NumArray(10)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetbulbTemp         = NumArray(11)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%FluidIndex = PlantLoop(CurLoopNum)%FluidIndex
    FluidName  = GetGlycolNameByIndex(SimpleEvapFluidCooler(EvapFluidCoolerNum)%FluidIndex)

    IF (lAlphaFieldBlanks(4).or.AlphArray(4) == Blank) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                          '" Performance input method is not specified. ')
      ErrorsFound=.true.
    ENDIF
    IF (SameString(AlphArray(4),'STANDARDDESIGNCAPACITY')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num           = PIM_StandardDesignCapacity
      IF (FluidName.NE. 'WATER') THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                            '". StandardDesignCapacity performance input method is only valid for fluid type = "Water".')
          CALL ShowContinueError('Currently, Fluid Type = '//TRIM(FluidName)//' in CondenserLoop = '// &
                                  TRIM(PlantLoop(CurLoopNum)%Name))
          ErrorsFound=.true.
      ENDIF
    ENDIF

    !outdoor air inlet node
    IF (lAlphaFieldBlanks(5)) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum = 0
    ELSE
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum = &
       GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(cCurrentModuleObject),SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
      IF(.not. CheckOutAirNodeNumber(SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                          '" Outdoor Air Inlet Node Name not valid Outdoor Air Node= '//TRIM(AlphArray(5)))
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound=.true.
      END IF
    ENDIF

!   fluid bypass for single speed evaporative fluid cooler
    IF (lAlphaFieldBlanks(6).or.AlphArray(6) == Blank) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%CapacityControl = 0     ! FanCycling
    ELSE
      SELECT CASE (MakeUPPERCase(AlphArray(6)))
        CASE ('FANCYCLING')
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%CapacityControl = 0
        CASE ('FLUIDBYPASS')
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%CapacityControl = 1
        CASE DEFAULT
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%CapacityControl = 0
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                       '" The Capacity Control is not specified correctly. The default Fan Cycling is used.')
      END SELECT
    ENDIF

    SimpleEvapFluidCooler(EvapFluidCoolerNum)%SizFac = NumArray(12)             !  N11  \field Sizing Factor
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%SizFac <= 0.0d0) SimpleEvapFluidCooler(EvapFluidCoolerNum)%SizFac = 1.0d0

    ! begin water use and systems get input
    IF (SameString(AlphArray(7),'LossFactor')) THEN
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapLossMode = EvapLossByUserFactor
    ELSEIF (SameString(AlphArray(7), 'SaturatedExit')) THEN
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSEIF (AlphArray(7) == Blank) THEN
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSE
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(AlphArray(7)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(AlphArray(1)))
      errorsfound = .true.
    ENDIF

    SimpleEvapFluidCooler(EvapFluidCoolerNum)%UserEvapLossFactor = NumArray(13) !  N13 , \field Evaporation Loss Factor
    IF ((NumNums < 13) .and. (SimpleEvapFluidCooler(EvapFluidCoolerNum)%UserEvapLossFactor == 0.0d0) ) Then
      ! assume Evaporation loss factor not entered and should be calculated
      IF ((OutRelHumValue.GE.0.1d0).AND.(OutRelHumValue.LE.0.7d0)) THEN
        !Use correlation by B.A. Qureshi and S.M. Zubair if within these limits
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%UserEvapLossFactor = &
                                   (113.0d0 - 8.417d0 * OutRelHumValue + 1.6147d0 * OutDryBulbTemp) * 1.0d-5
      ELSE  ! Inlet conditions are out of the limit of correlation; An approximate default value of loss factor is used
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%UserEvapLossFactor = 0.2d0
      ENDIF
    ENDIF

    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DriftLossFraction = NumArray(14)/100.0d0  !  N14, \field Drift Loss Percent

    If ((NumNums < 13) .and. (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DriftLossFraction == 0.0d0) ) Then
      ! assume Drift loss not entered and should be defaulted
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%DriftLossFraction = 0.008d0 /100.0d0
    endif
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%ConcentrationRatio = NumArray(15) !  N15, \field Blowdown Concentration Ratio

    If (SameString(AlphArray(8), 'ScheduledRate')) then
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode  = BlowdownBySchedule
    ELSEIF (SameString(AlphArray(8), 'ConcentrationRatio')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode  = BlowdownByConcentration
    ELSEIF (AlphArray(8) == Blank) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode  = BlowdownByConcentration
      If ((NumNums < 15) .and.(SimpleEvapFluidCooler(EvapFluidCoolerNum)%ConcentrationRatio == 0.0d0) ) THEN
        ! assume Concetration ratio was omitted and should be defaulted
            SimpleEvapFluidCooler(EvapFluidCoolerNum)%ConcentrationRatio = 3.0d0
      endif
    ELSE
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(8))//' = '//TRIM(AlphArray(8)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' ='//TRIM(AlphArray(1)))
      errorsfound = .true.
    ENDIF

    SimpleEvapFluidCooler(EvapFluidCoolerNum)%SchedIDBlowdown = GetScheduleIndex(AlphArray(9))
    IF ((SimpleEvapFluidCooler(EvapFluidCoolerNum)%SchedIDBlowdown == 0) .AND.   &
       (SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode == BlowdownBySchedule))Then
      CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(9))//' = '//TRIM(AlphArray(9)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' ='//TRIM(AlphArray(1)))
      errorsfound = .true.
    ENDIF

    IF (AlphArray(10) == Blank) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%SuppliedByWaterSystem = .false.
    ELSE ! water from storage tank
      CALL SetupTankDemandComponent(AlphArray(1), TRIM(cCurrentModuleObject), AlphArray(10), ErrorsFound, &
                                                   SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterTankID, &
                                                   SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterTankDemandARRID)
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%SuppliedByWaterSystem = .TRUE.
    ENDIF

!   Check various inputs to ensure that all the required variables are specified.

    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignSprayWaterFlowRate <= 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
              '". Evaporative fluid cooler input requires a design spray water flow rate greater than zero '//  &
                 'for all performance input methods.')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate <= 0.0d0 .AND. &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(1))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower <= 0.0d0 .AND. &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(2))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF

    IF (SameString(AlphArray(4),'UFACTORTIMESAREAANDDESIGNWATERFLOWRATE')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num           = PIM_UFactor
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA <= 0.0d0 .AND. &
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(6))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate <= 0.0d0 .AND. &
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(7))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
    ELSEIF(SameString(AlphArray(4),'STANDARDDESIGNCAPACITY')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num           = PIM_StandardDesignCapacity
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(5))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
    ELSEIF(SameString(AlphArray(4),'USERSPECIFIEDDESIGNCAPACITY')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num           = PIM_UserSpecifiedDesignCapacity
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate <= 0.0d0 .AND. &
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(7))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedUserSpecifiedDesignCapacity <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(8))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
         ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'", invalid data for "'//   &
          TRIM(cNumericFieldNames(9))//'", entered value <= 0.0, but must be >0 for '//  &
          TRIM(cAlphaFieldNames(4))//' = "'//TRIM(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirTemp <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'", invalid data for "'//   &
          TRIM(cNumericFieldNames(10))//'", entered value <= 0.0, but must be >0 for '//  &
          TRIM(cAlphaFieldNames(4))//' = "'//TRIM(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetbulbTemp <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'", invalid data for "'//   &
          TRIM(cNumericFieldNames(11))//'", entered value <= 0.0, but must be >0 for '//  &
          TRIM(cAlphaFieldNames(4))//' = "'//TRIM(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp <= &
                  SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetbulbTemp) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'", '//  &
                             TRIM(cNumericFieldNames(9))//' must be greater than '//  TRIM(cNumericFieldNames(11))//'.')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirTemp <= &
                  SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetbulbTemp) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'", '//  &
                             TRIM(cNumericFieldNames(10))//' must be greater than '//  TRIM(cNumericFieldNames(11))//'.')
        ErrorsFound=.true.
      ENDIF
    ELSE ! Evaporative fluid cooler performance input method is not specified as a valid "choice"
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
             '". Evaporative fluid cooler Performance Input Method must be "UFactorTimesAreaAndDesignWaterFlowRate" '//  &
                'or "StandardDesignCapacity" or "UserSpecifiedDesignCapacity".')
      CALL ShowContinueError('Evaporative fluid cooler Performance Input Method currently specified as: '// &
                            TRIM(AlphArray(4)))
      ErrorsFound=.true.
    ENDIF
  END DO  ! End Single-Speed Evaporative Fluid Cooler Loop

  cCurrentModuleObject = cEvapFluidCooler_TwoSpeed
  DO TwoSpeedEvapFluidCoolerNumber = 1 , NumTwoSpeedEvapFluidCoolers
    EvapFluidCoolerNum = NumSingleSpeedEvapFluidCoolers + TwoSpeedEvapFluidCoolerNumber
    CALL GetObjectItem(cCurrentModuleObject,TwoSpeedEvapFluidCoolerNumber,AlphArray,NumAlphas, &
                       NumArray,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),SimpleEvapFluidCooler%Name,EvapFluidCoolerNum-1,IsNotOK,IsBlank,  &
       TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name                  = AlphArray(1)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType = TRIM(cCurrentModuleObject)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num = EvapFluidCooler_TwoSpeed
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerMassFlowRateMultiplier = 2.5d0
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterInletNodeNum     = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterOutletNodeNum    = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(cCurrentModuleObject),AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Chilled Water Nodes')

    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate                = NumArray(1)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower                   = NumArray(2)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate                 = NumArray(3)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRateSizingFactor     = NumArray(4)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPower                    = NumArray(5)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPowerSizingFactor        = NumArray(6)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignSprayWaterFlowRate            = NumArray(7)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HeatRejectCapNomCapSizingRatio      = NumArray(8)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity     = NumArray(9)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedStandardDesignCapacity      = NumArray(10)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedStandardDesignCapacitySizingFactor = NumArray(11)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA          = NumArray(12)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA           = NumArray(13)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUASizingFactor = NumArray(14)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate                 = NumArray(15)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedUserSpecifiedDesignCapacity= NumArray(16)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedUserSpecifiedDesignCapacity = NumArray(17)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedUserSpecifiedDesignCapacitySizingFactor = NumArray(18)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp             = NumArray(19)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirTemp               = NumArray(20)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetbulbTemp        = NumArray(21)
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%FluidIndex= PlantLoop(CurLoopNum)%FluidIndex
    FluidName  = GetGlycolNameByIndex(SimpleEvapFluidCooler(EvapFluidCoolerNum)%FluidIndex)

    IF (lAlphaFieldBlanks(4)) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                          '" Performance input method is not specified. ')
      ErrorsFound=.true.
    ENDIF

    IF (SameString(AlphArray(4),'STANDARDDESIGNCAPACITY')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num           = PIM_StandardDesignCapacity
      IF (FluidName.NE. 'WATER') THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                            '". StandardDesignCapacity performance input method is only valid for fluid type = "Water".')
          CALL ShowContinueError('Currently, Fluid Type = '//TRIM(FluidName)//' in CondenserLoop = '// &
                                  TRIM(PlantLoop(CurLoopNum)%Name))
        ErrorsFound=.true.
      ENDIF
    ENDIF

    ! outdoor air inlet node
    IF (lAlphaFieldBlanks(5)) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum = 0
    ELSE
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum = &
       GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(cCurrentModuleObject),SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
      IF(.not. CheckOutAirNodeNumber(SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum))THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                          '" Outdoor Air Inlet Node Name not valid Outdoor Air Node= '//TRIM(AlphArray(5)))
        CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
        ErrorsFound=.true.
      END IF
    ENDIF

    SimpleEvapFluidCooler(EvapFluidCoolerNum)%SizFac = NumArray(22)             !  N16  \field Sizing Factor
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%SizFac <= 0.0d0) SimpleEvapFluidCooler(EvapFluidCoolerNum)%SizFac = 1.0d0

    ! begin water use and systems get input
    IF (SameString(AlphArray(6),'LossFactor')) THEN
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapLossMode = EvapLossByUserFactor
    ELSEIF (SameString(AlphArray(6), 'SaturatedExit')) THEN
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSEIF (lAlphaFieldBlanks(6)) THEN
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapLossMode = EvapLossByMoistTheory
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(AlphArray(6)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(AlphArray(1)))
      errorsfound = .true.
    ENDIF

    SimpleEvapFluidCooler(EvapFluidCoolerNum)%UserEvapLossFactor = NumArray(23) !  N23 , \field Evaporation Loss Factor
    If ((NumNums < 23) .and. (SimpleEvapFluidCooler(EvapFluidCoolerNum)%UserEvapLossFactor == 0.0d0) ) Then
      ! assume Evaporation loss factor not entered and should be calculated
      IF ((OutRelHumValue.GE.0.1d0).AND. (OutRelHumValue.LE.0.7d0)) THEN
        !Use correlation by B.A. Qureshi and S.M. Zubair if within these limits
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%UserEvapLossFactor = &
                                   (113.0d0 - 8.417d0 * OutRelHumValue + 1.6147d0 * OutDryBulbTemp) * 1.0d-5
      ELSE  ! Inlet conditions are out of the limit of correlation; An approximate default value of loss factor is used
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%UserEvapLossFactor = 0.2d0
      ENDIF
    ENDIF
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DriftLossFraction = NumArray(24) / 100.0d0  !  N24, \field Drift Loss Percent
   IF ((NumNums < 24) .and. (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DriftLossFraction == 0.0d0) ) Then
    ! assume Drift loss not entered and should be defaulted
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DriftLossFraction = 0.008d0 /100.0d0
   ENDIF

    SimpleEvapFluidCooler(EvapFluidCoolerNum)%ConcentrationRatio = NumArray(25) !  N25, \field Blowdown Concentration Ratio


    If (SameString(AlphArray(7), 'ScheduledRate')) then
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode  = BlowdownBySchedule
    ELSEIF (SameString(AlphArray(7), 'ConcentrationRatio')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode  = BlowdownByConcentration
    ELSEIF (lAlphaFieldBlanks(7)) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode  = BlowdownByConcentration
      If ((NumNums < 25) .and.(SimpleEvapFluidCooler(EvapFluidCoolerNum)%ConcentrationRatio == 0.0d0) ) THEN
        ! assume Concetration ratio was omitted and should be defaulted
            SimpleEvapFluidCooler(EvapFluidCoolerNum)%ConcentrationRatio = 3.0d0
      endif
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(AlphArray(7)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(AlphArray(1)))
      errorsfound = .true.
    ENDIF
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%SchedIDBlowdown = GetScheduleIndex(AlphArray(8))
    IF ((SimpleEvapFluidCooler(EvapFluidCoolerNum)%SchedIDBlowdown == 0) .AND.   &
       (SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode == BlowdownBySchedule)) Then
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//' = '//TRIM(AlphArray(8)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(AlphArray(1)))
      errorsfound = .true.
    ENDIF

    IF (lAlphaFieldBlanks(9)) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%SuppliedByWaterSystem = .false.
    ELSE ! water from storage tank
      !
      Call SetupTankDemandComponent(AlphArray(1), TRIM(cCurrentModuleObject), AlphArray(9), ErrorsFound, &
                                     SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterTankID, &
                                       SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterTankDemandARRID)
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%SuppliedByWaterSystem = .TRUE.
    ENDIF

!   Check various inputs to ensure that all the required variables are specified.

    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignSprayWaterFlowRate <= 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
              '". Evaporative fluid cooler input requires a design spray water flow rate greater than zero '//  &
                 'for all performance input methods.')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate <= 0.0d0 .AND. &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate .NE. AutoSize) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
             '". Evaporative fluid cooler input requires design air flow rate at high fan speed to be '//  &
                 'greater than zero for all performance input methods.')
       ErrorsFound=.true.
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate <= 0.0d0 .AND. &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate .NE. AutoSize) THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
             '". Evaporative fluid cooler input requires design air flow rate at low fan speed to be '//  &
                 'greater than zero for all performance input methods.')
       ErrorsFound=.true.
    ENDIF
!   High speed air flow rate must be greater than low speed air flow rate.
!   Can't tell yet if autosized, check later in InitEvapFluidCooler.
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate <=   &
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate .and. &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
         '". Evaporative fluid cooler air flow rate at low fan speed must be less than the'//&
         ' air flow rate at high fan speed.')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower <= 0.0d0 .AND. &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(2))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPower <= 0.0d0 .AND. &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPower .NE. AutoSize) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
        trim(cNumericFieldNames(5))//'", entered value <= 0.0, but must be > 0 for '//  &
        trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower <=   &
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPower .and. &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
       '". Evaporative fluid cooler low speed fan power must be less than the'//&
       ' high speed fan power .')
      ErrorsFound=.true.
    ENDIF

    IF (SameString(AlphArray(4),'UFACTORTIMESAREAANDDESIGNWATERFLOWRATE')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num           = PIM_UFactor
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA <= 0.0d0 .AND. &
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(12))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA <= 0.0d0 .AND. &
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(13))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA <=   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA .and. &
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA .NE. AutoSize) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
         '". Evaporative fluid cooler U-factor Times Area Value at Low Fan Speed must be less than the'//&
         ' U-factor Times Area Value at High Fan Speed.')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate <= 0.0d0 .AND. &
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(15))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
    ELSEIF(SameString(AlphArray(4),'STANDARDDESIGNCAPACITY')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num           = PIM_StandardDesignCapacity
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(9))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedStandardDesignCapacity <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(10))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedStandardDesignCapacity >=   &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
              '". Low-Speed Standard Design Capacity must be less than the High-Speed Standard Design Capacity.')
        ErrorsFound=.true.
      END IF
    ELSEIF(SameString(AlphArray(4),'USERSPECIFIEDDESIGNCAPACITY')) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num           = PIM_UserSpecifiedDesignCapacity
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate <= 0.0d0 .AND. &
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate .NE. AutoSize) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(15))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedUserSpecifiedDesignCapacity <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(16))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedUserSpecifiedDesignCapacity <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(17))//'", entered value <= 0.0, but must be > 0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA .NE. 0.0d0) THEN
        IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA > 0.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                               '". UserSpecifiedDesignCapacity performance input method and evaporative fluid cooler UA '// &
                                  'at high fan speed have been specified.')
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
               '". UserSpecifiedDesignCapacity performance input method has been specified and '// &
                  'evaporative fluid cooler UA at high fan speed is being autosized.')
        ENDIF
        CALL ShowContinueError('Evaporative fluid cooler UA at high fan speed must be left blank when '//  &
                               'UserSpecifiedDesignCapacity performance input method is used.')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA .NE. 0.0d0) THEN
        IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA > 0.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                 '". UserSpecifiedDesignCapacity performance input method and evaporative fluid cooler UA at '// &
                 ' low fan speed have been specified.')
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
               '". UserSpecifiedDesignCapacity performance input method has been specified and evaporative fluid cooler '// &
                 'UA at low fan speed is being autosized.')
        ENDIF
        CALL ShowContinueError('Evaporative fluid cooler UA at low fan speed must be left blank '//  &
                               'when UserSpecifiedDesignCapacity performance input method is used.')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedUserSpecifiedDesignCapacity >= &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedUserSpecifiedDesignCapacity) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                              '". Low-Speed User Specified Design Capacity must be less than '//  &
                                 'the High-Speed User Specified Design Dapacity.')
        ErrorsFound=.true.
      END IF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(19))//'", entered value <= 0.0, but must be >0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirTemp <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(20))//'", entered value <= 0.0, buy must be >0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetbulbTemp <= 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = "'//trim(AlphArray(1))//'", invalid data for "'//   &
          trim(cNumericFieldNames(21))//'", entered value <= 0.0, but must be >0 for '//  &
          trim(cAlphaFieldNames(4))//' = "'//trim(AlphArray(4))//'".')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp <= &
                  SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetbulbTemp) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'", '//  &
                             TRIM(cNumericFieldNames(19))//' must be greater than '//  TRIM(cNumericFieldNames(15))//'.')
        ErrorsFound=.true.
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirTemp <= &
                  SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetbulbTemp) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'", '//  &
                             TRIM(cNumericFieldNames(20))//' must be greater than '//  TRIM(cNumericFieldNames(15))//'.')
        ErrorsFound=.true.
      ENDIF
    ELSE ! Evaporative fluid cooler performance input method is not specified as a valid "choice"
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
           '". Evaporative fluid cooler Performance Input Method must be "UFactorTimesAreaAndDesignWaterFlowRate" '//  &
              'or "StandardDesignCapacity" or "UserSpecifiedDesignCapacity".')
      CALL ShowContinueError('Evaporative fluid cooler Performanace Input Method currently specified as: '// &
                            TRIM(AlphArray(4)))
      ErrorsFound=.true.
    ENDIF
  END DO  ! End Two-Speed Evaporative Fluid Cooler Loop

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting evaporative fluid cooler input.')
  ENDIF

! Set up output variables
! CurrentModuleObject='EvaporativeFluidCooler:SingleSpeed'
  DO EvapFluidCoolerNum = 1, NumSingleSpeedEvapFluidCoolers
    CALL SetupOutputVariable('Cooling Tower Inlet Temperature [C]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%InletWaterTemp,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Outlet Temperature [C]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%OutletWaterTemp,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Mass Flow Rate [kg/s]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%WaterMassFlowRate,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Heat Transfer Rate [W]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%Qactual,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Power [W]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%FanPower,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Energy [J]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%FanEnergy,'System','Sum',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')
          ! Added for fluid bypass
    CALL SetupOutputVariable('Cooling Tower Bypass Fraction []', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%BypassFraction,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
  END DO

! CurrentModuleObject='EvaporativeFluidCooler:TwoSpeed'
  DO EvapFluidCoolerNum = NumSingleSpeedEvapFluidCoolers+1, NumSingleSpeedEvapFluidCoolers+NumTwoSpeedEvapFluidCoolers
    CALL SetupOutputVariable('Cooling Tower Inlet Temperature [C]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%InletWaterTemp,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Outlet Temperature [C]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%OutletWaterTemp,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Mass Flow Rate [kg/s]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%WaterMassFlowRate,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Heat Transfer Rate [W]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%Qactual,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Power [W]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%FanPower,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Fan Electric Energy [J]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%FanEnergy,'System','Sum',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='HeatRejection',GroupKey='Plant')

  END DO

  ! setup common water reporting for all types of evaporative fluid coolers.
  ! CurrentModuleObject='EvaporativeFluidCooler:*'
  DO EvapFluidCoolerNum = 1 , NumSingleSpeedEvapFluidCoolers + NumTwoSpeedEvapFluidCoolers
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%SuppliedByWaterSystem) THEN
      CALL SetupOutputVariable('Cooling Tower Make Up Water Volume Flow Rate [m3/s]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%MakeUpVdot,'System','Average',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Make Up Water Volume [m3]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%MakeUpVol,'System','Sum',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Storage Tank Water Volume Flow Rate [m3/s]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%TankSupplyVdot,'System','Average',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Storage Tank Water Volume [m3]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%TankSupplyVol,'System','Sum',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
            ResourceTypeKey='Water', EndUseKey='HeatRejection', GroupKey='Plant')
      CALL SetupOutputVariable('Cooling Tower Starved Storage Tank Water Volume Flow Rate [m3/s]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%StarvedMakeUpVdot,'System','Average',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Starved Storage Tank Water Volume [m3]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%StarvedMakeUpVol,'System','Sum',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
            ResourceTypeKey='Water', EndUseKey='HeatRejection', GroupKey='Plant')
      CALL SetupOutputVariable('Cooling Tower Make Up Mains Water Volume [m3]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%StarvedMakeUpVol,'System','Sum',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
            ResourceTypeKey='MainsWater', EndUseKey='HeatRejection', GroupKey='Plant')
    ELSE ! Evaporative fluid cooler water from mains and gets metered
      CALL SetupOutputVariable('Cooling Tower Make Up Water Volume Flow Rate [m3/s]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%MakeUpVdot,'System','Average',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
      CALL SetupOutputVariable('Cooling Tower Make Up Water Volume [m3]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%MakeUpVol,'System','Sum',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
            ResourceTypeKey='Water', EndUseKey='HeatRejection', GroupKey='Plant')
      CALL SetupOutputVariable('Cooling Tower Make Up Mains Water Volume [m3]', &
            SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%MakeUpVol,'System','Sum',  &
               SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
            ResourceTypeKey='MainsWater', EndUseKey='HeatRejection', GroupKey='Plant')
    ENDIF

    CALL SetupOutputVariable('Cooling Tower Water Evaporation Volume Flow Rate [m3/s]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%EvaporationVdot,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Evaporation Volume [m3]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%EvaporationVol,'System','Sum',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Drift Volume Flow Rate [m3/s]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%DriftVdot,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Drift Volume [m3]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%DriftVol,'System','Sum',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Blowdown Volume Flow Rate [m3/s]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%BlowdownVdot,'System','Average',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
    CALL SetupOutputVariable('Cooling Tower Water Blowdown Volume [m3]', &
          SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%BlowdownVol,'System','Sum',  &
             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)
  ENDDO ! loop all evaporative fluid coolers

RETURN
END SUBROUTINE GetEvapFluidCoolerInput
! End of Get Input subroutines for the Evaporative Fluid Cooler Module
!******************************************************************************


! Beginning Initialization Section for the Evaporative Fluid Coolers Module
!******************************************************************************

SUBROUTINE InitSimVars

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Chandan Sharma
          !       DATE WRITTEN:    May 2009
          !       MODIFIED         na
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

    InletWaterTemp           = 0.0d0    ! CW temperature at evaporative fluid cooler inlet
    OutletWaterTemp          = 0.0d0    ! CW temperature at evaporative fluid cooler outlet
    WaterInletNode           = 0      ! Node number at evaporative fluid cooler inlet
    WaterOutletNode          = 0      ! Node number at evaporative fluid cooler outlet
    WaterMassFlowRate        = 0.0d0    ! WaterMassFlowRate through evaporative fluid cooler
!    EvapFluidCoolerMassFlowRateMax     = 0.0    ! Max Hardware Mass Flow Rate
!    EvapFluidCoolerMassFlowRateMin     = 0.0    ! Min Hardware Mass Flow Rate
!    LoopMassFlowRateMaxAvail = 0.0    ! Max Loop Mass Flow Rate available
!    LoopMassFlowRateMinAvail = 0.0    ! Min Loop Mass Flow Rate available
    Qactual                  = 0.0d0    ! Evaporative fluid cooler heat transfer
    FanPower                 = 0.0d0    ! Evaporative fluid cooler fan power used
    AirFlowRateRatio         = 0.0d0    ! Ratio of air flow rate through VS Evaporative fluid cooler to design air flow rate
    WaterUsage               = 0.0d0    ! Evaporative fluid cooler water usage (m3/s)

RETURN
END SUBROUTINE InitSimVars

SUBROUTINE InitEvapFluidCooler(EvapFluidCoolerNum, RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the evaporative fluid cooler components and for
          ! final checking of evaporative fluid cooler inputs (post autosizing)

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! Based on InitTower subroutine by Don Shirey Sept/Oct 2002, F Buhl Oct 2002

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: BeginEnvrnFlag
  USE Psychrometrics,  ONLY: PsyTwbFnTdbWPb
  USE InputProcessor,  ONLY: SameString
!  USE FluidProperties, ONLY : GetDensityGlycol
  USE DataPlant,       ONLY: TypeOf_EvapFluidCooler_SingleSpd, TypeOf_EvapFluidCooler_TwoSpd, &
                             ScanPlantLoopsForObject, PlantSizeNotComplete, PlantSizesOkayToFinalize
  USE PlantUtilities,  ONLY: InitComponentNodes, SetComponentFlowRate, RegulateCondenserCompFlowReqOp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: EvapFluidCoolerNum   ! Number of the current evaporative fluid cooler being simulated
  LOGICAL, INTENT (IN) :: RunFlag              ! Indication of

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                           :: ErrorsFound=.false. ! Flag if input data errors are found
  LOGICAL, SAVE                           :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: OneTimeFlagForEachEvapFluidCooler
  INTEGER   :: TypeOf_Num
  INTEGER   :: LoopNum
  INTEGER   :: LoopSideNum
  INTEGER   :: BranchIndex
  INTEGER   :: CompIndex
  REAL(r64) :: rho           ! local density of fluid
!  LOGICAL   :: FatalError

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumSimpleEvapFluidCoolers))
    ALLOCATE(OneTimeFlagForEachEvapFluidCooler(NumSimpleEvapFluidCoolers))

    OneTimeFlagForEachEvapFluidCooler = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.

  END IF

  IF (OneTimeFlagForEachEvapFluidCooler(EvapFluidCoolerNum)) THEN

    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed) THEN
      TypeOf_Num = TypeOf_EvapFluidCooler_SingleSpd
    ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
      TypeOf_Num = TypeOf_EvapFluidCooler_TwoSpd
    ENDIF
    ErrorsFound = .false.
    ! Locate the tower on the plant loops for later usage
    CALL ScanPlantLoopsForObject(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                                 TypeOf_Num, &
                                 SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum, &
                                 SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopSideNum, &
                                 SimpleEvapFluidCooler(EvapFluidCoolerNum)%BranchNum, &
                                 SimpleEvapFluidCooler(EvapFluidCoolerNum)%CompNum,  &
                                 errFlag=ErrorsFound)

    IF (ErrorsFound) THEN
      CALL ShowFatalError('InitEvapFluidCooler: Program terminated due to previous condition(s).')
    ENDIF



    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate > 0.0d0) THEN
        IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate <=   &
           SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate) THEN
          CALL ShowSevereError('EvaporativeFluidCooler:TwoSpeed "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                               '". Low speed air flow rate must be less than the high speed air flow rate.')
          ErrorsFound=.true.
        ENDIF
        IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA <=   &
           SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA) THEN
           CALL ShowSevereError('EvaporativeFluidCooler:TwoSpeed "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
                        '". Evaporative fluid cooler UA at low fan speed must be less than the evaporative fluid cooler'// &
                        ' UA at high fan speed.')
           ErrorsFound=.true.
        ENDIF
      END IF
    END IF

    IF (ErrorsFound) THEN
      CALL ShowFatalError('InitEvapFluidCooler: Program terminated due to previous condition(s).')
    ENDIF

    OneTimeFlagForEachEvapFluidCooler(EvapFluidCoolerNum) = .FALSE.

  END IF

  ! Begin environment initializations
  IF(MyEnvrnFlag(EvapFluidCoolerNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize) )Then
    IF (PlantSizeNotComplete)  CALL SizeEvapFluidCooler(EvapFluidCoolerNum)

    rho = GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                                'InitEvapFluidCooler')
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesWaterMassFlowRate =   &
           SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate * rho
    CALL InitComponentNodes(0.0D0,  SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesWaterMassFlowRate , &
                                    SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterInletNodeNum,     &
                                    SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterOutletNodeNum,    &
                                    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum,               &
                                    SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopSideNum,           &
                                    SimpleEvapFluidCooler(EvapFluidCoolerNum)%BranchNum,             &
                                    SimpleEvapFluidCooler(EvapFluidCoolerNum)%CompNum)
    MyEnvrnFlag(EvapFluidCoolerNum) = .false.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(EvapFluidCoolerNum)=.true.
  ENDIF

  ! Each time initializations
  WaterInletNode = SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterInletNodeNum
  SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp  = Node(WaterInletNode)%Temp

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum /= 0) THEN
    SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp    =   &
       Node(SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum)%Temp
    SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat  =   &
       Node(SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum)%HumRat
    SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress   =   &
       Node(SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum)%Press
    SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb =   &
       Node(SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutdoorAirInletNodeNum)%OutAirWetBulb
  ELSE
    SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp    = OutDryBulbTemp
    SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat  = OutHumRat
    SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress   = OutBaroPress
    SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb = OutWetBulbTemp
  ENDIF

    LoopNum     = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum
    LoopSideNum = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopSideNum
    BranchIndex = SimpleEvapFluidCooler(EvapFluidCoolerNum)%BranchNum
    CompIndex   = SimpleEvapFluidCooler(EvapFluidCoolerNum)%CompNum

    WaterMassFlowRate = RegulateCondenserCompFlowReqOp(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum,               &
                                                SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopSideNum,           &
                                                SimpleEvapFluidCooler(EvapFluidCoolerNum)%BranchNum,             &
                                                SimpleEvapFluidCooler(EvapFluidCoolerNum)%CompNum,     &
                                                SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesWaterMassFlowRate * &
                                                SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerMassFlowRateMultiplier)


    CALL SetComponentFlowRate(WaterMassFlowRate, &
                              SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterInletNodeNum,     &
                              SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterOutletNodeNum,    &
                              SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum,               &
                              SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopSideNum,           &
                              SimpleEvapFluidCooler(EvapFluidCoolerNum)%BranchNum,             &
                              SimpleEvapFluidCooler(EvapFluidCoolerNum)%CompNum)
  RETURN
END SUBROUTINE InitEvapFluidCooler


SUBROUTINE SizeEvapFluidCooler(EvapFluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   May 2009
          !       MODIFIED       Chandan Sharma, April 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing evaporative fluid cooler Components for which capacities and flow rates
          ! have not been specified in the input. This subroutine also calculates evaporative fluid cooler UA if the user
          ! has specified evaporative fluid cooler performance via the "Standard Design Capacity" method.

          ! METHODOLOGY EMPLOYED:
          ! Obtains condenser flow rate from the plant sizing array. If evaporative fluid cooler performance is specified
          ! via the "Standard Design Capacity" method, the water flow rate is directly proportional to capacity.

          ! REFERENCES:
          ! Based on SizeTower by Don Shirey, Sept/Oct 2002; Richard Raustad, Feb 2005

          ! USE STATEMENTS:
  USE DataSizing
  USE General,         ONLY: SolveRegulaFalsi,  RoundSigDigits
  USE PlantUtilities,  ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE InputProcessor,  ONLY: SameString
  USE DataPlant,       ONLY: PlantSizesOkayToFinalize

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: EvapFluidCoolerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte = 500        ! Maximum number of iterations
  REAL(r64), PARAMETER        :: Acc    = 0.0001d0   ! Accuracy of result
  CHARACTER(len=*), PARAMETER :: CalledFrom = 'SizeEvapFluidCooler'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizCondNum            ! Plant Sizing index for condenser loop
  INTEGER             :: SolFla                   ! Flag of solver
  REAL(r64)           :: DesEvapFluidCoolerLoad   ! Design evaporative fluid cooler load [W]
  REAL(r64)           :: UA0                      ! Lower bound for UA [W/C]
  REAL(r64)           :: UA1                      ! Upper bound for UA [W/C]
  REAL(r64)           :: UA                       ! Calculated UA value [W/C]
  REAL(r64)           :: OutWaterTempAtUA0        ! Water outlet temperature at UA0
  REAL(r64)           :: OutWaterTempAtUA1        ! Water outlet temperature at UA1
  REAL(r64)           :: DesignEnteringAirWetBulb ! Intermediate variable to check that design exit
                                                  ! temperature specified in the plant:sizing object
                                                  ! is higher than the design entering air wet-bulb temp
                                                  ! when autosize feature is used
  REAL(r64), DIMENSION(6)      :: Par             ! Parameter array need for RegulaFalsi routine
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)                    :: Cp              ! local specific heat for fluid
  REAL(r64)                    :: rho             ! local density for fluid
  REAL(r64)           :: tmpDesignWaterFlowRate ! local temporary for water volume flow rate
  REAL(r64)           :: tmpHighSpeedFanPower  !local temporary for high speed fan power
  REAL(r64)           :: tmpHighSpeedAirFlowRate ! local temporary for high speed air flow rate
  REAL(r64)           :: tmpHighSpeedEvapFluidCoolerUA ! local temporary for high speed cooler UA


  DesEvapFluidCoolerLoad = 0.0d0
  tmpDesignWaterFlowRate  = SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate
  tmpHighSpeedFanPower    = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower
  tmpHighSpeedAirFlowRate = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate
  tmpHighSpeedEvapFluidCoolerUA = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA

  PltSizCondNum = PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%PlantSizNum


  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate == AutoSize .AND. &
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num .NE. PIM_StandardDesignCapacity) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpDesignWaterFlowRate = &
           PlantSizData(PltSizCondNum)%DesVolFlowRate * SimpleEvapFluidCooler(EvapFluidCoolerNum)%SizFac
        IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate = &
                                         tmpDesignWaterFlowRate

      ELSE
        tmpDesignWaterFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate = &
                                         tmpDesignWaterFlowRate
      ENDIF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType,  &
                                                            SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                                                           'Design Water Flow Rate [m3/s]', &
                                                            SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing error for evaporative fluid cooler object = '//  &
                            TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
      CALL ShowFatalError('Autosizing of evaporative fluid cooler condenser flow rate requires a loop Sizing:Plant object.')
    ENDIF
    ! Check when the user specified Condenser/Evaporative Fluid Cooler water design setpoint
    ! temperature is less than design inlet air wet bulb temperature
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_UFactor) THEN
      DesignEnteringAirWetBulb = 25.6d0
    ELSE
      DesignEnteringAirWetBulb = SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetBulbTemp
    ENDIF
    IF ( PlantSizData(PltSizCondNum)%ExitTemp <= DesignEnteringAirWetBulb) THEN
      CALL ShowSevereError('Error when autosizing the UA value for Evaporative Fluid Cooler = '//&
                             TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//'.' )
      CALL ShowContinueError('Design Loop Exit Temperature ('// &
                             TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)) // &
                             ' C) must be greater than design entering air wet-bulb temperature ('// &
                             TRIM(RoundSigDigits(DesignEnteringAirWetBulb,2)) //&
                             ' C) when autosizing the Evaporative Fluid Cooler UA.')
      CALL ShowContinueError('It is recommended that the Design Loop Exit Temperature = Design Entering Air '// &
                             'Wet-bulb Temp plus the Evaporative Fluid Cooler design approach temperature (e.g., 4 C).')
      CALL ShowContinueError('If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field ' // &
                             'Condenser Water Design Setpoint must be > Design Entering Air Wet-bulb Temp ' // &
                             'if autosizing the Evaporative Fluid Cooler.')
      CALL ShowFatalError('Review and revise design input values as appropriate.')
    ENDIF
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_UFactor .and.  &
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA /= AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                                'SizeEvapFluidCooler')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                               PlantSizData(PltSizCondNum)%ExitTemp,                      &
                               PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex, &
                               'SizeEvapFluidCooler')
      DesEvapFluidCoolerLoad = rho * Cp &
                                 * tmpDesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity = DesEvapFluidCoolerLoad &
                                         / SimpleEvapFluidCooler(EvapFluidCoolerNum)%HeatRejectCapNomCapSizingRatio
    ELSE
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity = 0.0d0
    ENDIF
  END IF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_StandardDesignCapacity) THEN
    ! Design water flow rate is assumed to be 3 gpm per ton (SI equivalent 5.382E-8 m3/s per watt)
    tmpDesignWaterFlowRate=   &
       5.382d-8 * SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate = tmpDesignWaterFlowRate
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed) THEN
      IF (PlantSizesOkayToFinalize)   &
         CALL ReportSizingOutput(cEvapFluidCooler_SingleSpeed,SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                        'Design water flow rate based on evaporative fluid cooler Standard Design Capacity [m3/s]', &
                         SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate)
    ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize)   &
         CALL ReportSizingOutput(cEvapFluidCooler_TwoSpeed,SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                    'Design water flow rate based on evaporative fluid cooler high-speed Standard Design Capacity [m3/s]', &
                      SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate)
    ENDIF
  ENDIF

  CALL RegisterPlantCompDesignFlow(SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterInletNodeNum,  tmpDesignWaterFlowRate)

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower == AutoSize) THEN
    ! We assume the nominal fan power is 0.0105 times the design load
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_StandardDesignCapacity) THEN
      tmpHighSpeedFanPower = 0.0105d0 * SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity
      IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower =   tmpHighSpeedFanPower
    ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_UserSpecifiedDesignCapacity) THEN
      tmpHighSpeedFanPower = 0.0105d0 * SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedUserSpecifiedDesignCapacity
      IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower =  tmpHighSpeedFanPower
    ELSE
      IF (DesEvapFluidCoolerLoad .GT. 0) THEN
        tmpHighSpeedFanPower =  0.0105d0 * DesEvapFluidCoolerLoad
        IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower = tmpHighSpeedFanPower
      ELSEIF (PltSizCondNum > 0) THEN
        IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
          rho = GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                 InitConvTemp, &
                                 PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                                 'SizeEvapFluidCooler')
          Cp = GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                     PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                     PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex, &
                                     'SizeEvapFluidCooler')
          DesEvapFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT
          tmpHighSpeedFanPower = 0.0105d0 * DesEvapFluidCoolerLoad
          IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower = tmpHighSpeedFanPower
        ELSE
          tmpHighSpeedFanPower = 0.d0
          IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower = tmpHighSpeedFanPower
        ENDIF
      ELSE
        CALL ShowSevereError('Autosizing of evaporative fluid cooler fan power requires a loop Sizing:Plant object.')
        CALL ShowFatalError(' Occurs in evaporative fluid cooler object= '//&
                                              TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
      ENDIF
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_SingleSpeed,&
                         SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                        'Fan Power at Design Air Flow Rate [W]', SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower)
    ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_TwoSpeed, &
                         SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                        'Fan Power at High Fan Speed [W]', SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower)
    ENDIF
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate == AutoSize) THEN
! Plant Sizing Object is not required to AUTOSIZE this field since its simply a multiple of another field.

    tmpHighSpeedAirFlowRate = tmpHighSpeedFanPower * 0.5d0  * (101325.d0/StdBaroPress) / 190.d0
    IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate = tmpHighSpeedAirFlowRate

    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_SingleSpeed,&
                                                           SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                                                          'Design Air Flow Rate [m3/s]', &
                                                           SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate)
    ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_TwoSpeed, &
                                                           SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                                                          'Air Flow Rate at High Fan Speed [m3/s]',   &
                                                           SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate)
    ENDIF
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA == AutoSize .AND. &
            SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_UFactor) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizCondNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        ! This conditional statement is to trap when the user specified Condenser/Evaporative Fluid Cooler water design setpoint
        ! temperature is less than design inlet air wet bulb temperature of 25.6 C
        IF ( PlantSizData(PltSizCondNum)%ExitTemp <= 25.6d0 ) THEN
          CALL ShowSevereError('Error when autosizing the UA value for Evaporative Fluid Cooler = '//&
                                  TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//'.')
          CALL ShowContinueError('Design Loop Exit Temperature ('// &
                                  TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)) // &
                                  ' C) must be greater than 25.6 C when autosizing the Evaporative Fluid Cooler UA.')
          CALL ShowContinueError('The Design Loop Exit Temperature specified in Sizing:Plant object = '// &
                                  TRIM(PlantSizData(PltSizCondNum)%PlantLoopName))
          CALL ShowContinueError('It is recommended that the Design Loop Exit Temperature = 25.6 C plus '// &
                                 'the Evaporative Fluid Cooler design approach temperature (e.g., 4 C).')
          CALL ShowContinueError('If using HVACTemplate:Plant:ChilledWaterLoop, then check that input field ' // &
                                 'Condenser Water Design Setpoint must be > 25.6 C if autosizing the Evaporative Fluid Cooler.')
          CALL ShowFatalError('Review and revise design input values as appropriate.')
        ENDIF
        rho = GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                               InitConvTemp, &
                               PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                               'SizeEvapFluidCooler')
        Cp = GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                   PlantSizData(PltSizCondNum)%ExitTemp,                      &
                                   PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex, &
                                   'SizeEvapFluidCooler')
        DesEvapFluidCoolerLoad = rho * Cp * tmpDesignWaterFlowRate * PlantSizData(PltSizCondNum)%DeltaT
        Par(1) = DesEvapFluidCoolerLoad
        Par(2) = REAL(EvapFluidCoolerNum,r64)
        Par(3) = rho * tmpDesignWaterFlowRate   ! Design water mass flow rate
        Par(4) = tmpHighSpeedAirFlowRate      ! Design air volume flow rate
        Par(5) = Cp
        UA0 = 0.0001d0 * DesEvapFluidCoolerLoad ! Assume deltaT = 10000K (limit)
        UA1 = DesEvapFluidCoolerLoad            ! Assume deltaT = 1K
        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp = PlantSizData(PltSizCondNum)%ExitTemp +   &
                                                                   PlantSizData(PltSizCondNum)%DeltaT
        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp    = 35.d0
        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb = 25.6d0
        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress   = StdBaroPress
        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat  =   &
           PsyWFnTdbTwbPb(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp,     &
                          SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb,  &
                          SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress)
        CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par)
        IF (SolFla == -1) THEN
          CALL ShowWarningError('Iteration limit exceeded in calculating evaporative fluid cooler UA.')
          CALL ShowContinueError('Autosizing of fluid cooler UA failed for evaporative fluid cooler = '//&
                                 TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
          CALL ShowContinueError('The final UA value = '//TRIM(RoundSigDigits(UA,2))// &
                                 'W/C, and the simulation continues...')
        ELSEIF (SolFla == -2) THEN
          CALL SimSimpleEvapFluidCooler(INT(Par(2)),Par(3),Par(4),UA0,OutWaterTempAtUA0)
          CALL SimSimpleEvapFluidCooler(INT(Par(2)),Par(3),Par(4),UA1,OutWaterTempAtUA1)
          CALL ShowSevereError(CalledFrom//': The combination of design input values did not allow the calculation of a ')
          CALL ShowContinueError('reasonable UA value. Review and revise design input values as appropriate. Specifying hard')
          CALL ShowContinueError('sizes for some "autosizable" fields while autosizing other "autosizable" fields may be '// &
                                 'contributing to this problem.')
          CALL ShowContinueError('This model iterates on UA to find the heat transfer required to provide the design outlet ')
          CALL ShowContinueError('water temperature. Initially, the outlet water temperatures at high and low UA values are ')
          CALL ShowContinueError('calculated. The Design Exit Water Temperature should be between the outlet water ')
          CALL ShowContinueError('temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ')
          CALL ShowContinueError('out of this range, the solution will not converge and UA will not be calculated. ')
          CALL ShowContinueError('The possible solutions could be to manually input adjusted water and/or air flow rates ')
          CALL ShowContinueError('based on the autosized values shown below or to adjust design evaporative fluid cooler ' // &
                                 'air inlet wet-bulb temperature.')
          CALL ShowContinueError('Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).')
          CALL ShowContinueError('Inputs to the evaporative fluid cooler object:')
          CALL ShowContinueError('Design Evaporative Fluid Cooler Load [W]                      = '// &
                                  TRIM(RoundSigDigits(Par(1),2)))
          CALL ShowContinueError('Design Evaporative Fluid Cooler Water Volume Flow Rate [m3/s] = '// &
                                  TRIM(RoundSigDigits(SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate,6)))
          CALL ShowContinueError('Design Evaporative Fluid Cooler Air Volume Flow Rate [m3/s]   = '// &
                                  TRIM(RoundSigDigits(Par(4),2)))
          CALL ShowContinueError('Design Evaporative Fluid Cooler Air Inlet Wet-bulb Temp [C]   = '// &
                                  TRIM(RoundSigDigits(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb,2)))
          CALL ShowContinueError('Design Evaporative Fluid Cooler Water Inlet Temp [C]          = '// &
                                  TRIM(RoundSigDigits(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp,2)))
          CALL ShowContinueError('Inputs to the plant sizing object:')
          CALL ShowContinueError('Design Exit Water Temp [C]                                    = '// &
                                  TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)))
          CALL ShowContinueError('Loop Design Temperature Difference [C]                        = '// &
                                  TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%DeltaT,2)))
          CALL ShowContinueError('Design Evaporative Fluid Cooler Water Inlet Temp [C]          = '// &
                                  TRIM(RoundSigDigits(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp,2)))
          CALL ShowContinueError('Calculated water outlet temperature at low UA [C](UA = '// &
                                  TRIM(RoundSigDigits(UA0,2))//' W/C)  = '//TRIM(RoundSigDigits(OutWaterTempAtUA0,2)))
          CALL ShowContinueError('Calculated water outlet temperature at high UA [C](UA = '// &
                                  TRIM(RoundSigDigits(UA1,2))//' W/C)  = '//TRIM(RoundSigDigits(OutWaterTempAtUA1,2)))
          CALL ShowFatalError('Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = '//  &
                                  TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
        ENDIF
        tmpHighSpeedEvapFluidCoolerUA = UA
        IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA = &
                                                                            tmpHighSpeedEvapFluidCoolerUA
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity = DesEvapFluidCoolerLoad &
                     / SimpleEvapFluidCooler(EvapFluidCoolerNum)%HeatRejectCapNomCapSizingRatio
      ELSE
        tmpHighSpeedEvapFluidCoolerUA = 0.d0
        IF (PlantSizesOkayToFinalize) SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA = &
                                                                            tmpHighSpeedEvapFluidCoolerUA
      ENDIF
      IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed) THEN
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_SingleSpeed, &
                        SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                       'U-Factor Times Area Value at Design Air Flow Rate [W/C]', &
                       SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA)
      ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_TwoSpeed, &
                                                              SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                                                             'U-Factor Times Area Value at High Fan Speed [W/C]', &
                                               SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA)
      ENDIF
    ELSE
      CALL ShowSevereError('Autosizing error for evaporative fluid cooler object = '//  &
         TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
      CALL ShowFatalError('Autosizing of evaporative fluid cooler UA requires a loop Sizing:Plant object.')
    ENDIF
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_StandardDesignCapacity) THEN
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate >= SmallWaterVolFlow) THEN
      ! Standard Design Capacity doesn't include compressor heat;
      ! predefined factor was 1.25 W heat rejection per W of delivered cooling, now a user input with 1.25 default
      rho = GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                             InitConvTemp, &
                             PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                             'SizeEvapFluidCooler')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                 35.d0,                      &
                                 PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex, &
                                 'SizeEvapFluidCooler')
      DesEvapFluidCoolerLoad = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity &
                                   * SimpleEvapFluidCooler(EvapFluidCoolerNum)%HeatRejectCapNomCapSizingRatio
      Par(1) = DesEvapFluidCoolerLoad
      Par(2) = REAL(EvapFluidCoolerNum,r64)
      Par(3) = rho * SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate ! Design water mass flow rate
      Par(4) = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate        ! Design air volume flow rate
      Par(5) = Cp
      UA0 = 0.0001d0 * DesEvapFluidCoolerLoad ! Assume deltaT = 10000K (limit)
      UA1 = DesEvapFluidCoolerLoad            ! Assume deltaT = 1K
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp  = 35.d0   ! 95F design inlet water temperature
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp    = 35.d0   ! 95F design inlet air dry-bulb temp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb = 25.6d0  ! 78F design inlet air wet-bulb temp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress   = StdBaroPress
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat  =   &
         PsyWFnTdbTwbPb(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp,  &
                        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb,  &
                        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
        CALL ShowWarningError('Iteration limit exceeded in calculating evaporative fluid cooler UA.')
        CALL ShowContinueError('Autosizing of fluid cooler UA failed for evaporative fluid cooler = '//&
                               TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
        CALL ShowContinueError('The final UA value = '//TRIM(RoundSigDigits(UA,2))// &
                               'W/C, and the simulation continues...')
      ELSE IF (SolFla == -2) THEN
        CALL ShowSevereError(CalledFrom//': The combination of design input values did not allow the calculation of a ')
        CALL ShowContinueError('reasonable UA value. Review and revise design input values as appropriate. ')
        CALL ShowFatalError('Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = '//  &
                                TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
      ENDIF
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA = UA
    ELSE
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA = 0.0d0
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_SingleSpeed,&
                       SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                      'U-Factor Times Area Value at Design Air Flow Rate [W/C]', &
                      SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA)
    ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_TwoSpeed, &
                       SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                      'U-Factor Times Area Value at High Fan Speed [W/C]', &
                       SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA)
    ENDIF
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_UserSpecifiedDesignCapacity) THEN
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate >= SmallWaterVolFlow) THEN
      rho = GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                             InitConvTemp, &
                             PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                             'SizeEvapFluidCooler')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                 SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp,                      &
                                 PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex, &
                                 'SizeEvapFluidCooler')
      DesEvapFluidCoolerLoad = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedUserSpecifiedDesignCapacity
      Par(1) = DesEvapFluidCoolerLoad
      Par(2) = REAL(EvapFluidCoolerNum,r64)
      Par(3) = rho * tmpDesignWaterFlowRate ! Design water mass flow rate
      Par(4) = tmpHighSpeedAirFlowRate        ! Design air volume flow rate
      Par(5) = Cp
      UA0 = 0.0001d0 * DesEvapFluidCoolerLoad ! Assume deltaT = 10000K (limit)
      UA1 = DesEvapFluidCoolerLoad            ! Assume deltaT = 1K

      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp  =   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp    =   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirTemp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb =   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetBulbTemp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress   = StdBaroPress
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat  =   &
         PsyWFnTdbTwbPb(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp,  &
                        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb,  &
                        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
        CALL ShowWarningError('Iteration limit exceeded in calculating evaporative fluid cooler UA.')
        CALL ShowContinueError('Autosizing of fluid cooler UA failed for evaporative fluid cooler = '//&
                               TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
        CALL ShowContinueError('The final UA value = '//TRIM(RoundSigDigits(UA,2))// &
                               'W/C, and the simulation continues...')
      ELSE IF (SolFla == -2) THEN
        CALL SimSimpleEvapFluidCooler(INT(Par(2)),Par(3),Par(4),UA0,OutWaterTempAtUA0)
        CALL SimSimpleEvapFluidCooler(INT(Par(2)),Par(3),Par(4),UA1,OutWaterTempAtUA1)
        CALL ShowSevereError(CalledFrom//': The combination of design input values did not allow the calculation of a ')
        CALL ShowContinueError('reasonable UA value. Review and revise design input values as appropriate. Specifying hard')
        CALL ShowContinueError('sizes for some "autosizable" fields while autosizing other "autosizable" fields may be '// &
                               'contributing to this problem.')
        CALL ShowContinueError('This model iterates on UA to find the heat transfer required to provide the design outlet ')
        CALL ShowContinueError('water temperature. Initially, the outlet water temperatures at high and low UA values are ')
        CALL ShowContinueError('calculated. The Design Exit Water Temperature should be between the outlet water ')
        CALL ShowContinueError('temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ')
        CALL ShowContinueError('out of this range, the solution will not converge and UA will not be calculated. ')
        CALL ShowContinueError('The possible solutions could be to manually input adjusted water and/or air flow rates ')
        CALL ShowContinueError('based on the autosized values shown below or to adjust design evaporative fluid cooler ' // &
                               'air inlet wet-bulb temperature.')
        CALL ShowContinueError('Plant:Sizing object inputs also influence these results (e.g. DeltaT and ExitTemp).')
        CALL ShowContinueError('Inputs to the evaporative fluid cooler object:')
        CALL ShowContinueError('Design Evaporative Fluid Cooler Load [W]                      = '// &
                                TRIM(RoundSigDigits(Par(1),2)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Water Volume Flow Rate [m3/s] = '// &
                                TRIM(RoundSigDigits(SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate,6)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Air Volume Flow Rate [m3/s]   = '// &
                                TRIM(RoundSigDigits(Par(4),2)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Air Inlet Wet-bulb Temp [C]   = '// &
                                TRIM(RoundSigDigits(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb,2)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Water Inlet Temp [C]          = '// &
                                TRIM(RoundSigDigits(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp,2)))
        CALL ShowContinueError('Inputs to the plant sizing object:')
        CALL ShowContinueError('Design Exit Water Temp [C]                                    = '// &
                                TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)))
        CALL ShowContinueError('Loop Design Temperature Difference [C]                        = '// &
                                TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%DeltaT,2)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Water Inlet Temp [C]          = '// &
                                TRIM(RoundSigDigits(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp,2)))
        CALL ShowContinueError('Calculated water outlet temperature at low UA [C](UA = '// &
                                TRIM(RoundSigDigits(UA0,2))//' W/C)  = '//TRIM(RoundSigDigits(OutWaterTempAtUA0,2)))
        CALL ShowContinueError('Calculated water outlet temperature at high UA [C](UA = '// &
                                TRIM(RoundSigDigits(UA1,2))//' W/C)  = '//TRIM(RoundSigDigits(OutWaterTempAtUA1,2)))
        CALL ShowFatalError('Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = '//  &
                                TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
      ENDIF
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA = UA
    ELSE
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA = 0.0d0
    ENDIF
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_SingleSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_SingleSpeed, &
                       SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                      'U-Factor Times Area Value at Design Air Flow Rate [W/C]', &
                       SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA)
    ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cEvapFluidCooler_TwoSpeed, &
                       SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                      'U-Factor Times Area Value at High Fan Speed [W/C]', &
                       SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA)
    ENDIF
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate == AutoSize .AND. PlantSizesOkayToFinalize ) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate =   &
            SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRateSizingFactor &
              *SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate
      CALL ReportSizingOutput(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType,   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
        'Air Flow Rate at Low Fan Speed [m3/s]', SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate)
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPower == AutoSize .AND. PlantSizesOkayToFinalize ) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPower =   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPowerSizingFactor &
             *SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower
      CALL ReportSizingOutput(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType,   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                              'Fan Power at Low Fan Speed [W]', SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPower)
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA == AutoSize .AND. PlantSizesOkayToFinalize ) THEN
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA =   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUASizingFactor &
           *SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA
      CALL ReportSizingOutput(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType,   &
                              SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                             'U-Factor Times Area Value at Low Fan Speed [W/C]', &
                              SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA)
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_StandardDesignCapacity .AND. &
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate >=   &
       SmallWaterVolFlow.AND.SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedStandardDesignCapacity > 0.0d0) THEN
      ! Standard design capacity doesn't include compressor heat;
      ! predefined factor was 1.25 W heat rejection per W of delivered cooling, now user input with default 1.25
      rho = GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                             InitConvTemp, &
                             PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                             'SizeEvapFluidCooler')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                 SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp,                      &
                                 PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex, &
                                 'SizeEvapFluidCooler')
      DesEvapFluidCoolerLoad = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedStandardDesignCapacity &
                                 * SimpleEvapFluidCooler(EvapFluidCoolerNum)%HeatRejectCapNomCapSizingRatio
      Par(1) = DesEvapFluidCoolerLoad
      Par(2) = REAL(EvapFluidCoolerNum,r64)
      Par(3) = rho * tmpDesignWaterFlowRate ! Design water mass flow rate
      Par(4) = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate  ! Air volume flow rate at low fan speed
      Par(5) = Cp
      UA0 = 0.0001d0 * DesEvapFluidCoolerLoad ! Assume deltaT = 10000K (limit)
      UA1 = DesEvapFluidCoolerLoad            ! Assume deltaT = 1K
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp  = 35.d0   ! 95F design inlet water temperature
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp    = 35.d0   ! 95F design inlet air dry-bulb temp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb = 25.6d0  ! 78F design inlet air wet-bulb temp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress   = StdBaroPress
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat  =   &
         PsyWFnTdbTwbPb(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp,  &
                        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb,  &
                        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
        CALL ShowWarningError('Iteration limit exceeded in calculating evaporative fluid cooler UA.')
        CALL ShowContinueError('Autosizing of fluid cooler UA failed for evaporative fluid cooler = '//&
                               TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
        CALL ShowContinueError('The final UA value = '//TRIM(RoundSigDigits(UA,2))// &
                               'W/C, and the simulation continues...')
      ELSE IF (SolFla == -2) THEN
        CALL ShowSevereError(CalledFrom//': The combination of design input values did not allow the calculation of a ')
        CALL ShowContinueError('reasonable low-speed UA value. Review and revise design input values as appropriate. ')
        CALL ShowFatalError('Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = '//  &
                                TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
      ENDIF
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA = UA
    ELSE
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA = 0.0d0
    ENDIF
    IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType,   &
                             SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                            'U-Factor Times Area Value at Low Fan Speed [W/C]', &
                             SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA)
  ENDIF

  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%PerformanceInputMethod_Num == PIM_UserSpecifiedDesignCapacity .AND. &
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType_Num == EvapFluidCooler_TwoSpeed) THEN
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignWaterFlowRate >=   &
       SmallWaterVolFlow.AND.SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedUserSpecifiedDesignCapacity > 0.0d0) THEN
      rho = GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                             InitConvTemp, &
                             PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                             'SizeEvapFluidCooler')
      Cp = GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                 SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp,                      &
                                 PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex, &
                                 'SizeEvapFluidCooler')
      DesEvapFluidCoolerLoad = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedUserSpecifiedDesignCapacity
      Par(1) = DesEvapFluidCoolerLoad
      Par(2) = REAL(EvapFluidCoolerNum,r64)
      Par(3) = rho * tmpDesignWaterFlowRate ! Design water mass flow rate
      Par(4) = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate     ! Air volume flow rate at low fan speed
      Par(5) = Cp
      UA0 = 0.0001d0 * DesEvapFluidCoolerLoad ! Assume deltaT = 10000K (limit)
      UA1 = DesEvapFluidCoolerLoad            ! Assume deltaT = 1K
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp  =   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringWaterTemp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp    =   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirTemp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb =   &
         SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignEnteringAirWetBulbTemp
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress   = StdBaroPress
      SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat  =   &
      PsyWFnTdbTwbPb(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp,  &
                        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb,  &
                        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress)
      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, SimpleEvapFluidCoolerUAResidual, UA0, UA1, Par)
      IF (SolFla == -1) THEN
        CALL ShowSevereError('Iteration limit exceeded in calculating EvaporativeFluidCooler UA')
        CALL ShowFatalError('Autosizing of EvaporativeFluidCooler UA failed for EvaporativeFluidCooler '//  &
           TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
      ELSE IF (SolFla == -2) THEN
        CALL SimSimpleEvapFluidCooler(INT(Par(2)),Par(3),Par(4),UA0,OutWaterTempAtUA0)
        CALL SimSimpleEvapFluidCooler(INT(Par(2)),Par(3),Par(4),UA1,OutWaterTempAtUA1)
        CALL ShowSevereError(CalledFrom//': The combination of design input values did not allow the calculation of a ')
        CALL ShowContinueError('reasonable UA value. Review and revise design input values as appropriate. Specifying hard')
        CALL ShowContinueError('sizes for some "autosizable" fields while autosizing other "autosizable" fields may be '// &
                               'contributing to this problem.')
        CALL ShowContinueError('This model iterates on UA to find the heat transfer required to provide the design outlet ')
        CALL ShowContinueError('water temperature. Initially, the outlet water temperatures at high and low UA values are ')
        CALL ShowContinueError('calculated. The Design Exit Water Temperature should be between the outlet water ')
        CALL ShowContinueError('temperatures calculated at high and low UA values. If the Design Exit Water Temperature is ')
        CALL ShowContinueError('out of this range, the solution will not converge and UA will not be calculated. ')
        CALL ShowContinueError('Inputs to the Evaporative Fluid Cooler model are:')
        CALL ShowContinueError('Design Evaporative Fluid Cooler Load                    = '//TRIM(RoundSigDigits(Par(1),2)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Water Volume Flow Rate  = '//TRIM(RoundSigDigits(Par(3),2)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Air Volume Flow Rate    = '//TRIM(RoundSigDigits(Par(4),2)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Air Inlet Wet-bulb Temp = '// &
                                TRIM(RoundSigDigits(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb,2)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Water Inlet Temp        = '// &
                                TRIM(RoundSigDigits(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp,2)))
        CALL ShowContinueError('Design Exit Water Temp                                  = '// &
                                TRIM(RoundSigDigits(PlantSizData(PltSizCondNum)%ExitTemp,2)))
        CALL ShowContinueError('Design Evaporative Fluid Cooler Water Inlet Temp [C]    = '// &
                                TRIM(RoundSigDigits(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp,2)))
        CALL ShowContinueError('Calculated water outlet temperature at low UA('//TRIM(RoundSigDigits(UA0,2))//')  = '// &
                                TRIM(RoundSigDigits(OutWaterTempAtUA0,2)))
        CALL ShowContinueError('Calculated water outlet temperature at high UA('//TRIM(RoundSigDigits(UA1,2))//')  = '// &
                                TRIM(RoundSigDigits(OutWaterTempAtUA1,2)))
        CALL ShowFatalError('Autosizing of Evaporative Fluid Cooler UA failed for Evaporative Fluid Cooler = '//  &
                                TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
      ENDIF
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA = UA
    ELSE
      SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA = 0.0d0
    ENDIF
    IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType,   &
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name, &
                            'U-Factor Times Area Value at Low Fan Speed [W/C]', &
                            SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA)
  ENDIF

  IF (PlantSizesOkayToFinalize) THEN
    !create predefined report
    equipName = SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedStandardDesignCapacity)
  ENDIF

  RETURN
END SUBROUTINE SizeEvapFluidCooler

! End Initialization Section for the EvaporativeFluidCoolers Module
!******************************************************************************

! Beginning of the EvaporativeFluidCoolers Module Simulation Subroutines
! *****************************************************************************

SUBROUTINE CalcSingleSpeedEvapFluidCooler(EvapFluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To simulate the operation of a single-speed fan evaporative fluid cooler.

          ! METHODOLOGY EMPLOYED:
          ! The evaporative fluid cooler is modeled using effectiveness-NTU relationships for
          ! counterflow heat exchangers based on Merkel's theory.
          !
          ! The subroutine calculates the period of time required to meet a
          ! leaving water temperature setpoint. It assumes that part-load
          ! operation represents a linear interpolation of two steady-state regimes.
          ! Cyclic losses are neglected. The period of time required to meet the
          ! leaving water temperature setpoint is used to determine the required
          ! fan power and energy.
          !
          ! A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
          ! or schedule, of the evaporative fluid cooler. If the evaporative fluid cooler is OFF, outlet water
          ! temperature and flow rate are passed through the model from inlet node to
          ! outlet node without intervention. Reports are also updated with fan power and energy being zero.
          !
          ! When the RunFlag indicates an ON condition for the evaporative fluid cooler, the
          ! mass flow rate and water temperature are read from the inlet node of the
          ! evaporative fluid cooler (water-side). The outdoor air wet-bulb temperature is used
          ! as the entering condition to the evaporative fluid cooler (air-side).
          ! The evaporative fluid cooler fan is turned on and design parameters are used
          ! to calculate the leaving water temperature.
          ! If the calculated leaving water temperature is below the setpoint, a fan
          ! run-time fraction is calculated and used to determine fan power. The leaving
          ! water temperature setpoint is placed on the outlet node. If the calculated
          ! leaving water temperature is at or above the setpoint, the calculated
          ! leaving water temperature is placed on the outlet node and the fan runs at
          ! full power. Water mass flow rate is passed from inlet node to outlet node
          ! with no intervention.
          !
          !
          ! REFERENCES:
          ! ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.

          ! Based on SingleSpeedTower subroutine by Dan Fisher ,Sept 1998
          ! Dec. 2008. BG. added RunFlag logic per original methodology

          ! USE STATEMENTS:
!  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop, SingleSetpoint, DualSetpointDeadband

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: EvapFluidCoolerNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER  :: MaxIteration = 100          ! Maximum fluid bypass iteration calculations
  CHARACTER(len=3), PARAMETER :: MaxItChar  = '100'
  REAL(r64), PARAMETER :: BypassFractionThreshold = 0.01d0   !Threshold to stop bypass iteration
  REAL(r64), PARAMETER :: OWTLowerLimit = 0.0d0      ! The limit of evaporative fluid cooler exit fluid temperature used
                                                     ! in the fluid bypass calculation to avoid fluid freezing. For water,
                                                     ! it is 0 degreeC and for glycols, it can be much lower. The fluid type
                                                     ! is stored at the loop. Current choices are Water and Steam,
                                                     ! needs to expand for glycols

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: AirFlowRate
  REAL(r64)              :: UAdesign            ! UA value at design conditions (entered by user or calculated)
  REAL(r64)              :: InletWaterTemp
  REAL(r64)              :: FanModeFrac
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
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum

    !set inlet and outlet nodes
    WaterInletNode     = SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterInletNodeNum
    WaterOutletNode    = SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterOutletNodeNum
    Qactual            = 0.0d0
    FanPower           = 0.0d0
    InletWaterTemp     = Node(WaterInletNode)%Temp
    OutletWaterTemp    = InletWaterTemp
    LoopNum            = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum
    LoopSideNum        = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopSideNum
    AirFlowRate        = 0.0d0
    SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
    CASE (SingleSetPoint)
      TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpoint
    CASE (DualSetPointDeadBand)
      TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpointHi
    END SELECT

    ! Added for fluid bypass. First assume no fluid bypass
    BypassFlag = 0
    BypassFraction = 0.0d0
    BypassFraction2 = 0.0d0
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%BypassFraction = 0.0d0
    CapacityControl = SimpleEvapFluidCooler(EvapFluidCoolerNum)%CapacityControl

!   MassFlowTol is a parameter to indicate a no flow condition
    IF(WaterMassFlowRate .LE. MassFlowTolerance .OR. PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock .EQ. 0) RETURN

    IF(InletWaterTemp > TempSetPoint)THEN
!     Turn on evaporative fluid cooler fan
      UAdesign          = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA
      AirFlowRate       = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate
      FanPowerOn        = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower

      Call SimSimpleEvapFluidCooler(EvapFluidCoolerNum,WaterMassFlowRate,AirFlowRate,UAdesign, OutletWaterTemp)

      IF(OutletWaterTemp .LE. TempSetPoint)THEN
        IF(CapacityControl == 0 .OR. OutletWaterTemp <= OWTLowerLimit)THEN
!         Setpoint was met with pump ON and fan ON, calculate run-time fraction
          FanModeFrac     = (TempSetPoint-InletWaterTemp)/(OutletWaterTemp-InletWaterTemp)
          FanPower        = FanModeFrac * FanPowerOn
          OutletWaterTemp = TempSetPoint
        ELSE
          !FluidBypass, fan runs at full speed for the entire time step
          FanModeFrac     = 1.0d0
          FanPower        = FanPowerOn
          BypassFlag      = 1
        ENDIF
      ELSE
!       Setpoint was not met, evaporative fluid cooler ran at full capacity
        FanModeFrac     = 1.0d0
        FanPower      = FanPowerOn
      END IF
    ELSEIF(InletWaterTemp <=TempSetPoint)THEN
      !Inlet water temperature lower than setpoint, assume 100% bypass, evaporative fluid cooler fan off
      IF(CapacityControl == 1)THEN
        IF(InletWaterTemp > OWTLowerLimit)THEN
          FanPower = 0.0d0
          BypassFraction = 1.0d0
          SimpleEvapFluidCooler(EvapFluidCoolerNum)%BypassFraction = 1.0d0
          OutletWaterTemp = InletWaterTemp
        ENDIF
      ENDIF
    END IF

    ! Calculate bypass fraction since OWTLowerLimit < OutletWaterTemp < TempSetPoint.
    ! The iteration ends when the numer of iteration exceeds the limit or the difference
    !  between the new and old bypass fractions is less than the threshold.
    IF (BypassFlag == 1) THEN
      BypassFraction = (TempSetPoint - OutletWaterTemp) / (InletWaterTemp - OutletWaterTemp)
      IF(BypassFraction >1.0d0 .OR. BypassFraction<0.0d0)THEN
        ! Bypass cannot meet setpoint, assume no bypass
        BypassFlag = 0
        BypassFraction = 0.0d0
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%BypassFraction = 0.0d0
        AirFlowRate = 0.0d0
      ELSE
        NumIteration = 0
        BypassFractionPrev = BypassFraction
        OutletWaterTempPrev = OutletWaterTemp
        DO WHILE (NumIteration < MaxIteration)
          NumIteration = NumIteration + 1
          ! need to iterate for the new OutletWaterTemp while bypassing evaporative fluid cooler water
          Call SimSimpleEvapFluidCooler(EvapFluidCoolerNum, WaterMassFlowRate * (1.0d0-BypassFraction),   &
             AirFlowRate, UAdesign, OutletWaterTemp)
          ! Calc new BypassFraction based on the new OutletWaterTemp
          IF(ABS(OutletWaterTemp - OWTLowerLimit)<=0.01d0)THEN
            BypassFraction2 = BypassFraction
            EXIT
          ELSEIF(OutletWaterTemp < OWTLowerLimit)THEN
            ! Set OutletWaterTemp = OWTLowerLimit, and use linear interpolation to calculate the bypassFraction
            BypassFraction2 = BypassFractionPrev - (BypassFractionPrev-BypassFraction)*(OutletWaterTempPrev-OWTLowerLimit) &
                                 /(OutletWaterTempPrev-OutletWaterTemp)
            Call SimSimpleEvapFluidCooler(EvapFluidCoolerNum, WaterMassFlowRate * (1.0d0-BypassFraction2),   &
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
          CALL ShowWarningError('Evaporative fluid cooler fluid bypass iteration ' &
               //'exceeds maximum limit of '//MaxItChar//' for '//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name))
        ENDIF
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%BypassFraction = BypassFraction2
        ! may not meet TempSetPoint due to limit of evaporative fluid cooler outlet temp to OWTLowerLimit
        OutletWaterTemp = (1.0-BypassFraction2)*OutletWaterTemp + BypassFraction2*InletWaterTemp
      ENDIF
    ENDIF

    !Should this be water inlet node num?????
    CpWater =  GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                     Node(WaterInletNode)%Temp,                                   &
                                     PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,  &
                                     'CalcSingleSpeedEvapFluidCooler')
    Qactual = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)
    AirFlowRateRatio = AirFlowRate / SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate

RETURN
END SUBROUTINE CalcSingleSpeedEvapFluidCooler

SUBROUTINE CalcTwoSpeedEvapFluidCooler(EvapFluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To simulate the operation of a evaporative fluid cooler with a two-speed fan.

          ! METHODOLOGY EMPLOYED:
          ! The evaporative fluid cooler is modeled using effectiveness-NTU relationships for
          ! counterflow heat exchangers based on Merkel's theory.
          !
          ! The subroutine calculates the period of time required to meet a
          ! leaving water temperature setpoint. It assumes that part-load
          ! operation represents a linear interpolation of three steady-state regimes
          ! (high-speed fan operation and low-speed fan operation ).
          ! Cyclic losses are neglected. The period of time required to meet the
          ! leaving water temperature setpoint is used to determine the required
          ! fan power and energy. When the leaving water temperature is at or above the setpoint
          ! the evaporative fluid cooler fan is turned on,
          ! .
          !
          ! A RunFlag is passed by the upper level manager to indicate the ON/OFF status,
          ! or schedule, of the evaporative fluid cooler. If the evaporative fluid cooler is OFF, outlet water
          ! temperature and flow rate are passed through the model from inlet node to
          ! outlet node without intervention. Reports are also updated with fan power and fan energy being zero.
          !
          ! When the RunFlag indicates an ON condition for the evaporative fluid cooler, the
          ! mass flow rate and water temperature are read from the inlet node of the
          ! evaporative fluid cooler (water-side). The outdoor air wet-bulb temperature is used
          ! as the entering condition to the evaporative fluid cooler (air-side). If the incoming
          ! water temperature is above the setpoint, the evaporative fluid cooler fan is turned on
          ! and parameters for low fan speed are used to again calculate the leaving
          ! water temperature. If the calculated leaving water temperature is
          ! below the setpoint, a fan run-time fraction (FanModeFrac) is calculated and
          ! used to determine fan power. The leaving water temperature setpoint is placed
          ! on the outlet node. If the calculated leaving water temperature is at or above
          ! the setpoint, the evaporative fluid cooler fan is turned on 'high speed' and the routine is
          ! repeated. If the calculated leaving water temperature is below the setpoint,
          ! a fan run-time fraction is calculated for the second stage fan and fan power
          ! is calculated as FanModeFrac*HighSpeedFanPower+(1-FanModeFrac)*LowSpeedFanPower.
          ! If the calculated leaving water temperature is above the leaving water temp.
          ! setpoint, the calculated leaving water temperature is placed on the outlet
          ! node and the fan runs at full power (High Speed Fan Power). Water mass flow
          ! rate is passed from inlet node to outlet node with no intervention.
          !
          !
          ! REFERENCES:
          ! ASHRAE HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculation. 1999.
          ! Based on TwoSpeedTower by Dan Fisher ,Sept. 1998
          ! Dec. 2008. BG. added RunFlag logic per original methodology

          ! USE STATEMENTS:
!  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : SingleSetpoint, DualSetpointDeadband

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: EvapFluidCoolerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: AirFlowRate
  REAL(r64)              :: UAdesign            ! UA value at design conditions (entered by user) [W/C]
  REAL(r64)              :: InletWaterTemp
  REAL(r64)              :: OutletWaterTemp1stStage
  REAL(r64)              :: OutletWaterTemp2ndStage
  REAL(r64)              :: FanModeFrac
  REAL(r64)              :: FanPowerLow
  REAL(r64)              :: FanPowerHigh
  REAL(r64)              :: CpWater
  REAL(r64)              :: TempSetPoint
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum

          !set inlet and outlet nodes

    WaterInletNode      = SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterInletNodeNum
    WaterOutletNode     = SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterOutletNodeNum
    Qactual             = 0.0d0
    FanPower            = 0.0d0
    InletWaterTemp      = Node(WaterInletNode)%Temp
    OutletWaterTemp     = InletWaterTemp

    OutletWaterTemp1stStage = OutletWaterTemp
    OutletWaterTemp2ndStage = OutletWaterTemp
    FanModeFrac             = 0.0d0
    AirFlowRate             = 0.0d0
    LoopNum                 = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum
    LoopSideNum             = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopSideNum
    SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
    CASE (SingleSetPoint)
      TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpoint
    CASE (DualSetPointDeadBand)
      TempSetPoint       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetpointHi
    END SELECT

!   MassFlowTol is a parameter to indicate a no flow condition
    IF (WaterMassFlowRate .LE. MassFlowTolerance .OR. PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock .EQ. 0) RETURN

    IF (InletWaterTemp .GT. TempSetPoint) THEN
!     Setpoint was not met ,turn on evaporative fluid cooler 1st stage fan
      UAdesign        = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedEvapFluidCoolerUA
      AirFlowRate     = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedAirFlowRate
      FanPowerLow     = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LowSpeedFanPower

      Call SimSimpleEvapFluidCooler(EvapFluidCoolerNum,WaterMassFlowRate,AirFlowRate,UAdesign,OutletWaterTemp1stStage)

        IF(OutletWaterTemp1stStage .LE. TempSetPoint)THEN
!         Setpoint was met with pump ON and fan ON 1st stage, calculate fan mode fraction
          FanModeFrac     = (TempSetPoint-InletWaterTemp)/(OutletWaterTemp1stStage-InletWaterTemp)
          FanPower        = FanModeFrac * FanPowerLow
          OutletWaterTemp = TempSetPoint
          Qactual         = Qactual * FanModeFrac
        ELSE
!         Setpoint was not met, turn on evaporative fluid cooler 2nd stage fan
          UAdesign          = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedEvapFluidCoolerUA
          AirFlowRate       = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate
          FanPowerHigh      = SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedFanPower

          Call SimSimpleEvapFluidCooler(EvapFluidCoolerNum,WaterMassFlowRate,AirFlowRate,UAdesign,OutletWaterTemp2ndStage)

          IF((OutletWaterTemp2ndStage .LE. TempSetPoint).AND. UAdesign .GT. 0.0d0)THEN
!           Setpoint was met with pump ON and fan ON 2nd stage, calculate fan mode fraction
            FanModeFrac     = (TempSetPoint-OutletWaterTemp1stStage)/(OutletWaterTemp2ndStage-OutletWaterTemp1stStage)
            FanPower      = (FanModeFrac * FanPowerHigh) + (1.d0-FanModeFrac)*FanPowerLow
            OutletWaterTemp = TempSetPoint
          ELSE
!           Setpoint was not met, evaporative fluid cooler ran at full capacity
            OutletWaterTemp = OutletWaterTemp2ndStage
            FanPower      = FanPowerHigh
          END IF

        END IF

    END IF

    !Should this be water inlet node num??
    CpWater =  GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                     Node(WaterInletNode)%Temp,                                   &
                                     PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,  &
                                     'CalcTwoSpeedEvapFluidCooler')
    Qactual = WaterMassFlowRate * CpWater * (Node(WaterInletNode)%Temp - OutletWaterTemp)
    AirFlowRateRatio = AirFlowRate / SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate

RETURN
END SUBROUTINE CalcTwoSpeedEvapFluidCooler

SUBROUTINE SimSimpleEvapFluidCooler(EvapFluidCoolerNum,WaterMassFlowRate,AirFlowRate,UAdesign,OutletWaterTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! See purpose for single speed or Two speed evaporative fluid cooler model

          ! METHODOLOGY EMPLOYED:
          !
          ! See methodology for single speed or two speed evaporative fluid cooler model

          ! REFERENCES:
          ! Based on SimTower subroutine by Dan Fisher Sept. 1998
          ! Merkel, F. 1925.  Verduftungskuhlung. VDI Forschungsarbeiten, Nr 275, Berlin.
          ! ASHRAE     1999.  HVAC1KIT: A Toolkit for Primary HVAC System Energy Calculations.

          ! USE STATEMENTS:
!  USE FluidProperties, ONLY : GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: EvapFluidCoolerNum
  REAL(r64)              :: WaterMassFlowRate
  REAL(r64)              :: AirFlowRate
  REAL(r64)              :: UAdesign
  REAL(r64)              :: OutletWaterTemp


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: IterMax           = 50        ! Maximum number of iterations allowed
  REAL(r64), PARAMETER :: WetBulbTolerance  = 0.00001d0 ! Maximum error for exiting wet-bulb temperature between iterations
                                                        ! [delta K/K]
  REAL(r64), PARAMETER :: DeltaTwbTolerance = 0.001d0   ! Maximum error (tolerance) in DeltaTwb for iteration convergence [C]

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER            :: Iter                 ! Number of iterations completed
  REAL(r64)          :: MdotCpWater          ! Water mass flow rate times the heat capacity [W/K]
  REAL(r64)          :: InletAirTemp         ! Dry-bulb temperature of air entering the evaporative fluid cooler [C]
  REAL(r64)          :: CpWater              ! Heat capacity of water [J/kg/K]
  REAL(r64)          :: CpAir                ! Heat capacity of air [J/kg/K]
  REAL(r64)          :: AirDensity           ! Density of air [kg/m3]
  REAL(r64)          :: AirMassFlowRate      ! Mass flow rate of air [kg/s]
  REAL(r64)          :: effectiveness        ! Effectiveness of the heat exchanger [-]
  REAL(r64)          :: UAactual             ! UA value at actual conditions [W/C]
  REAL(r64)          :: InletAirEnthalpy     ! Enthalpy of entering moist air [J/kg]
  REAL(r64)          :: InletAirWetBulb      ! Wetbulb temp of entering moist air [C]
  REAL(r64)          :: OutletAirEnthalpy    ! Enthalpy of exiting moist air [J/kg]
  REAL(r64)          :: OutletAirWetBulb     ! Wetbulb temp of exiting moist air [C]
  REAL(r64)          :: OutletAirWetBulbLast ! temporary Wetbulb temp of exiting moist air [C]
  REAL(r64)          :: AirCapacity          ! MdotCp of air through the evaporative fluid cooler
  REAL(r64)          :: CapacityRatioMin     ! Minimum capacity of airside and waterside
  REAL(r64)          :: CapacityRatioMax     ! Maximum capacity of airside and waterside
  REAL(r64)          :: CapacityRatio        ! Ratio of minimum to maximum capacity
  REAL(r64)          :: NumTransferUnits     ! Number of transfer Units [NTU]
  REAL(r64)          :: WetBulbError         ! Calculated error for exiting wet-bulb temperature between iterations [delta K/K]
  REAL(r64)          :: CpAirside            ! Delta enthalpy of the evaporative fluid cooler air /
                                             ! delta air wet-bulb temp [J/kg/K]
  REAL(r64)          :: Qactual              ! Actual heat transfer rate between evaporative fluid cooler water and air [W]
  REAL(r64)          :: DeltaTwb             ! Absolute value of difference between inlet and outlet air wet-bulb temp [C]

  ! set inlet and outlet node numbers, and initialize some local variables

  WaterInletNode    = SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterInletNodeNum
  WaterOutletNode   = SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterOutletNodeNum
  Qactual           = 0.0d0
  WetBulbError      = 1.0d0
  DeltaTwb          = 1.0d0

  ! set local evaporative fluid cooler inlet and outlet temperature variables
  InletWaterTemp    = SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%WaterTemp
  OutletWaterTemp   = InletWaterTemp
  InletAirTemp      = SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp
  InletAirWetBulb   = SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb

  IF(UAdesign.EQ.0.0d0)RETURN

  ! set water and air properties
  AirDensity        = PsyRhoAirFnPbTdbW(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress,InletAirTemp,  &
                                        SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat)
  AirMassFlowRate   = AirFlowRate * AirDensity
  CpAir             = PsyCpAirFnWTdb(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat,InletAirTemp)
  CpWater =  GetSpecificHeatGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                                   InletWaterTemp,                                   &
                                   PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,         &
                                   'SimSimpleEvapFluidCooler')
  InletAirEnthalpy  = PsyHFnTdbRhPb(InletAirWetBulb, 1.0d0, SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress)

  ! initialize exiting wet bulb temperature before iterating on final solution
  OutletAirWetBulb = InletAirWetBulb + 6.0d0

  ! Calcluate mass flow rates
  MdotCpWater =   WaterMassFlowRate * CpWater
  Iter = 0
  DO WHILE ((WetBulbError.GT.WetBulbTolerance) .AND. (Iter.LE.IterMax) .AND. (DeltaTwb.GT.DeltaTwbTolerance))
    Iter = Iter + 1
    OutletAirEnthalpy = PsyHFnTdbRhPb(OutletAirWetBulb,1.0d0,SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress)
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
END SUBROUTINE SimSimpleEvapFluidCooler

FUNCTION SimpleEvapFluidCoolerUAResidual(UA, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Design evaporative fluid cooler load - evaporative fluid cooler cooling output)
          !                                    / Design evaporative fluid cooler load.
          ! Evaporative fluid cooler Cooling Output depends on the UA which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Puts UA into the evaporative fluid cooler data structure, calls SimSimpleEvapFluidCooler, and calculates
          ! the residual as defined above.

          ! REFERENCES:
          ! Based on SimpleTowerUAResidual by Fred Buhl, May 2002

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: UA                         ! UA of evaporative fluid cooler
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = design evaporative fluid cooler load [W]
                                                    ! par(2) = Evaporative fluid cooler number
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
  INTEGER      :: EvapFluidCoolerIndex            ! index of this evaporative fluid cooler
  REAL(r64)    :: OutWaterTemp                    ! outlet water temperature [C]
  REAL(r64)    :: CoolingOutput                   ! Evaporative fluid cooler cooling output [W]

  EvapFluidCoolerIndex = INT(Par(2))
  CALL SimSimpleEvapFluidCooler(EvapFluidCoolerIndex,Par(3),Par(4),UA,OutWaterTemp)
  CoolingOutput = Par(5)*Par(3)*(SimpleEvapFluidCoolerInlet(EvapFluidCoolerIndex)%WaterTemp - OutWaterTemp)
  Residuum = (Par(1) - CoolingOutput) / Par(1)
  RETURN
END FUNCTION SimpleEvapFluidCoolerUAResidual

! End of the EvaporativeFluidCoolers Module Simulation Subroutines
! *****************************************************************************

SUBROUTINE CalculateWaterUseage(EvapFluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Collect evaporative fluid cooler water useage calculations for
          ! reuse by all the evaporative fluid cooler models.


          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Based on CalculateWaterUseage subroutine for cooling tower by B. Griffith, August 2006

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: SecInHour
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataWater      , ONLY: WaterStorage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: EvapFluidCoolerNum

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
  If (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapLossMode == EvapLossByMoistTheory) Then

    AirDensity          = PsyRhoAirFnPbTdbW(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress, &
          SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp,SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat)
    AirMassFlowRate     = AirFlowRateRatio*SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighSpeedAirFlowRate*AirDensity
    InletAirEnthalpy    =   &
       PsyHFnTdbRhPb(SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirWetBulb,  &
          1.0d0,  &
          SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress)

    IF  (AirMassFlowRate > 0.0d0) Then
      ! Calculate outlet air conditions for determining water usage

      OutletAirEnthalpy   = InletAirEnthalpy + Qactual/AirMassFlowRate
      OutletAirTSat       = PsyTsatFnHPb(OutletAirEnthalpy,SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirPress)
      OutletAirHumRatSat  = PsyWFnTdbH(OutletAirTSat,OutletAirEnthalpy)

      ! calculate specific humidity ratios (HUMRAT to mass of moist air not dry air)
       InSpecificHumRat = SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat /   &
          ( 1 + SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirHumRat)
       OutSpecificHumRat = OutletAirHumRatSat / (1+ OutletAirHumRatSat)

      ! calculate average air temp for density call
       TairAvg = (SimpleEvapFluidCoolerInlet(EvapFluidCoolerNum)%AirTemp + OutletAirTSat)/2.0d0

      ! Amount of water evaporated
       rho =  GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                               TairAvg, &
                               PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                               'CalculateWaterUseage')
       EvapVdot         = (AirMassFlowRate * (OutSpecificHumRat - InSpecificHumRat)) / rho ! [m3/s]
       IF (EvapVdot < 0.0d0) EvapVdot = 0.0d0
     ELSE
       EvapVdot         = 0.0d0
     ENDIF

  ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapLossMode == EvapLossByUserFactor) Then
    rho =  GetDensityGlycol(PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidName,  &
                            AverageWaterTemp, &
                            PlantLoop(SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum)%FluidIndex,&
                            'CalculateWaterUseage')
    EvapVdot   = SimpleEvapFluidCooler(EvapFluidCoolerNum)%UserEvapLossFactor * (InletWaterTemp - OutletWaterTemp) &
                     * (WaterMassFlowRate / rho )
    IF (EvapVdot < 0.0d0) EvapVdot = 0.0d0
  ELSE
    ! should never come here
  ENDIF

!   amount of water lost due to drift
  DriftVdot = SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesignSprayWaterFlowRate *   &
     SimpleEvapFluidCooler(EvapFluidCoolerNum)%DriftLossFraction * &
                          AirFlowRateRatio

  If (SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode == BlowdownBySchedule) THEN
    ! Amount of water lost due to blow down (purging contaminants from evaporative fluid cooler basin)
    IF(SimpleEvapFluidCooler(EvapFluidCoolerNum)%SchedIDBlowdown .GT. 0)THEN
      BlowDownVdot          = GetCurrentScheduleValue(SimpleEvapFluidCooler(EvapFluidCoolerNum)%SchedIDBlowdown)
    ELSE
      BlowDownVdot          = 0.0d0
    END IF
  ELSEIF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%BlowdownMode == BlowdownByConcentration) THEN
      If (SimpleEvapFluidCooler(EvapFluidCoolerNum)%ConcentrationRatio > 2.0d0) Then ! protect divide by zero
         BlowDownVdot  =  EvapVdot / (SimpleEvapFluidCooler(EvapFluidCoolerNum)%ConcentrationRatio - 1) - DriftVdot
      ELSE
         BlowDownVdot  = EvapVdot - DriftVdot
      ENDIF
      If ( BlowDownVdot < 0.0d0 ) BlowDownVdot = 0.0d0
  ELSE
   !should never come here
  ENDIF

  ! Added for fluid bypass
  IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%CapacityControl == 1) THEN
    If (SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapLossMode == EvapLossByUserFactor)   &
       EvapVdot = EvapVdot * (1 - SimpleEvapFluidCooler(EvapFluidCoolerNum)%BypassFraction)
    DriftVdot = DriftVdot * (1 - SimpleEvapFluidCooler(EvapFluidCoolerNum)%BypassFraction)
    BlowDownVdot = BlowDownVdot * (1 - SimpleEvapFluidCooler(EvapFluidCoolerNum)%BypassFraction)
  ENDIF

  MakeUpVdot = EvapVdot + DriftVdot + BlowDownVdot

  ! set demand request in Water STorage if needed
  StarvedVdot = 0.0d0
  TankSupplyVdot = 0.0d0
  If (SimpleEvapFluidCooler(EvapFluidCoolerNum)%SuppliedByWaterSystem) Then

     ! set demand request
     WaterStorage(SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterTankID)%VdotRequestDemand(  &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterTankDemandARRID) &
      = MakeUpVdot

     AvailTankVdot = &       ! check what tank can currently provide
     WaterStorage(SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterTankID)%VdotAvailDemand(  &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%WaterTankDemandARRID)

     TankSupplyVdot = MakeUpVdot ! init
     If (AvailTankVdot < MakeUpVdot) Then ! calculate starved flow
        StarvedVdot = MakeUpVdot - AvailTankVdot
        TankSupplyVdot = AvailTankVdot
     ENDIF
  ELSE ! supplied by mains

  ENDIF

  !   total water usage
  ! update report variables
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%EvaporationVdot   = EvapVdot
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%EvaporationVol    = EvapVdot       * (TimeStepSys * SecInHour)
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%DriftVdot         = DriftVdot
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%DriftVol          = DriftVdot      * (TimeStepSys * SecInHour)
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%BlowdownVdot      = BlowDownVdot
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%BlowdownVol       = BlowDownVdot   * (TimeStepSys * SecInHour)
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%MakeUpVdot        = MakeUpVdot
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%MakeUpVol         = MakeUpVdot     * (TimeStepSys * SecInHour)
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%TankSupplyVdot    = TankSupplyVdot
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%TankSupplyVol     = TankSupplyVdot * (TimeStepSys * SecInHour)
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%StarvedMakeUpVdot = StarvedVdot
  SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%StarvedMakeUpVol  = StarvedVdot    * (TimeStepSys * SecInHour)

  RETURN

END SUBROUTINE CalculateWaterUseage



! Beginning of Record Keeping subroutines for the EvaporativeFluidCooler Module
! *****************************************************************************

SUBROUTINE UpdateEvapFluidCooler(EvapFluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Chandan Sharma
          !       DATE WRITTEN:    May 2009
          !       MODIFIED         na
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for passing results to the outlet water node.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!unused0909  USE DataEnvironment, ONLY: EnvironmentName, CurMnDy
  USE General, ONLY: TrimSigDigits
!  USE FluidProperties, ONLY : GetDensityGlycol
!  USE DataPlant, ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: EvapFluidCoolerNum


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  LowTempFmt="(' ',F6.2)"
  REAL(r64), PARAMETER :: TempAllowance = 0.02d0    ! Minimum difference b/w fluid cooler water outlet temp and
                                                    ! minimum condenser loop temp [C]

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=25)      :: CharErrOut
  CHARACTER(len=25)      :: CharLowOutletTemp
  REAL(r64)              :: TempDifference
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum
  REAL(r64)              :: LoopMinTemp

  ! set node information

  Node(WaterOutletNode)%Temp                   = OutletWaterTemp

  LoopNum = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopNum
  LoopSideNum = SimpleEvapFluidCooler(EvapFluidCoolerNum)%LoopSideNum
  IF(PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock.EQ.0 .OR. WarmupFlag)RETURN

  ! Check flow rate through evaporative fluid cooler and compare to design flow rate,
  ! show warning if greater than Design * Mulitplier
  IF (Node(WaterOutletNode)%MassFlowRate .GT. SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesWaterMassFlowRate * &
                                      SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerMassFlowRateMultiplier) THEN
    SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighMassFlowErrorCount=  &
       SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighMassFlowErrorCount+1
    IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighMassFlowErrorCount < 2) THEN
      CALL ShowWarningError (TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//' "'//  &
         TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//'"')
      CALL ShowContinueError   &
         (' Condenser Loop Mass Flow Rate is much greater than the evaporative fluid coolers design mass flow rate.')
      CALL ShowContinueError (' Condenser Loop Mass Flow Rate = '//TrimSigDigits(Node(WaterOutletNode)%MassFlowRate,6))
      CALL ShowContinueError (' Evaporative Fluid Cooler Design Mass Flow Rate   = '//  &
                                TrimSigDigits(SimpleEvapFluidCooler(EvapFluidCoolerNum)%DesWaterMassFlowRate,6))
      CALL ShowContinueErrorTimeStamp(' ')
    ELSE
      CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
         ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
          '"  Condenser Loop Mass Flow Rate is much greater than the evaporative fluid coolers design mass flow rate error' &
          , SimpleEvapFluidCooler(EvapFluidCoolerNum)%HighMassFlowErrorIndex,   &
             Node(WaterOutletNode)%MassFlowRate, Node(WaterOutletNode)%MassFlowRate)
    ENDIF
  END IF

   ! Check if OutletWaterTemp is below the minimum condenser loop temp and warn user
   LoopMinTemp    = PlantLoop(LoopNum)%MinTemp
   TempDifference = PlantLoop(LoopNum)%MinTemp - OutletWaterTemp
   IF (TempDifference.GT.TempAllowance .AND. WaterMassFlowRate > 0.0d0) THEN
     SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutletWaterTempErrorCount =   &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutletWaterTempErrorCount + 1
     WRITE(CharLowOutletTemp,LowTempFmt) LoopMinTemp
     WRITE(CharErrOut,LowTempFmt) OutletWaterTemp
     CharErrOut=ADJUSTL(CharErrOut)
     IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutletWaterTempErrorCount < 2) THEN
       CALL ShowWarningError (TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
          ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//'"')
       CALL ShowContinueError ('Evaporative fluid cooler water outlet temperature ('//TRIM(CharErrOut)//' C) is '// &
                               'below the specified minimum condenser loop temp of '//TRIM(ADJUSTL(CharLowOutletTemp))//' C')
       CALL ShowContinueErrorTimeStamp(' ')
     ELSE
       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
        ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
        '" Evaporative fluid cooler water outlet temperature is below the specified minimum condenser loop temp error' &
        , SimpleEvapFluidCooler(EvapFluidCoolerNum)%OutletWaterTempErrorIndex, OutletWaterTemp, OutletWaterTemp)
     END IF
   END IF

   ! Check if water mass flow rate is small (e.g. no flow) and warn user
   IF(WaterMassFlowRate .GT. 0.0d0 .AND. WaterMassFlowRate .LE. MassFlowTolerance)THEN
     SimpleEvapFluidCooler(EvapFluidCoolerNum)%SmallWaterMassFlowErrorCount =   &
        SimpleEvapFluidCooler(EvapFluidCoolerNum)%SmallWaterMassFlowErrorCount + 1
     IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%SmallWaterMassFlowErrorCount < 2) THEN
       CALL ShowWarningError (TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
          ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//'"')
       CALL ShowContinueError ('Evaporative fluid cooler water mass flow rate near zero.')
       CALL ShowContinueErrorTimeStamp(' ')
       CALL ShowContinueError('Actual Mass flow = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
     ELSE
       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
          ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
          '" Evaporative fluid cooler water mass flow rate near zero error continues...' &
          , SimpleEvapFluidCooler(EvapFluidCoolerNum)%SmallWaterMassFlowErrorIndex, WaterMassFlowRate, WaterMassFlowRate)
     ENDIF
   END IF

!   ! Check if water mass flow rate is lower than loop minimum and warn user
!   IF(WaterMassFlowRate .LT. LoopMassFlowRateMinAvail)THEN
!     SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRLessThanMinAvailErrCount =   &
!        SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRLessThanMinAvailErrCount + 1
!     IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRLessThanMinAvailErrCount < 2) THEN
!       CALL ShowWarningError (TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
!          ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//'"')
!       CALL ShowContinueError ('Evaporative fluid cooler water mass flow below loop minimum.')
!       CALL ShowContinueErrorTimeStamp(' ')
!       CALL ShowContinueError('Actual Mass flow  = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
!       CALL ShowContinueError('Loop Minimum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMinAvail,2)))
!     ELSE
!       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
!          ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
!          '" Evaporative fluid cooler water mass flow rate below loop minimum error continues...' &
!          , SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRLessThanMinAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
!     ENDIF
!   END IF
!
!   ! Check if water mass flow rate is greater than loop maximum and warn user
!   IF(WaterMassFlowRate .GT. LoopMassFlowRateMaxAvail)THEN
!     SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRGreaterThanMaxAvailErrCount =   &
!        SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRGreaterThanMaxAvailErrCount + 1
!     IF (SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRGreaterThanMaxAvailErrCount < 2) THEN
!       CALL ShowWarningError (TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//  &
!          ' "'//TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//'"')
!       CALL ShowContinueError ('Evaporative fluid cooler water mass flow above loop maximum.')
!       CALL ShowContinueErrorTimeStamp(' ')
!       CALL ShowContinueError('Actual Mass flow = '//TRIM(TrimSigDigits(WaterMassFlowRate,2)))
!       CALL ShowContinueError('Loop Maximum flow = '//TRIM(TrimSigDigits(LoopMassFlowRateMaxAvail,2)))
!     ELSE
!       CALL ShowRecurringWarningErrorAtEnd(TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%EvapFluidCoolerType)//' "'//  &
!           TRIM(SimpleEvapFluidCooler(EvapFluidCoolerNum)%Name)//&
!          '" Evaporative fluid cooler water mass flow rate above loop maximum error continues...' &
!          , SimpleEvapFluidCooler(EvapFluidCoolerNum)%WMFRGreaterThanMaxAvailErrIndex, WaterMassFlowRate, WaterMassFlowRate)
!     ENDIF
!   END IF

RETURN
END SUBROUTINE UpdateEvapFluidCooler
! End of Record Keeping subroutines for the EvaporativeFluidCooler Module
! *****************************************************************************

! Beginning of Reporting subroutines for the EvaporativeFluidCooler Module
! *****************************************************************************

SUBROUTINE ReportEvapFluidCooler(RunFlag, EvapFluidCoolerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Chandan Sharma
          !       DATE WRITTEN:    May 2009
          !       MODIFIED         na
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variables for the evaporative fluid cooler.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: RunFlag
  INTEGER, INTENT(IN) :: EvapFluidCoolerNum

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
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%InletWaterTemp    = Node(WaterInletNode)%Temp
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%OutletWaterTemp   = Node(WaterInletNode)%Temp
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%WaterMassFlowRate = WaterMassFlowRate
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%Qactual           = 0.0d0
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%FanPower          = 0.0d0
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%FanEnergy         = 0.0d0
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%AirFlowRatio      = 0.0d0
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%WaterAmountUsed   = 0.0d0
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%BypassFraction    = 0.0d0   ! added for fluid bypass
  ELSE
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%InletWaterTemp    = Node(WaterInletNode)%Temp
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%OutletWaterTemp   = OutletWaterTemp
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%WaterMassFlowRate = WaterMassFlowRate
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%Qactual           = Qactual
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%FanPower          = FanPower
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%FanEnergy         = FanPower*ReportingConstant
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%AirFlowRatio      = AirFlowRateRatio
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%WaterAmountUsed   = WaterUsage*ReportingConstant
       ! added for fluid bypass
    SimpleEvapFluidCoolerReport(EvapFluidCoolerNum)%BypassFraction    = SimpleEvapFluidCooler(EvapFluidCoolerNum)%BypassFraction
  END IF

 ! set

RETURN
END SUBROUTINE ReportEvapFluidCooler

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

END MODULE EvaporativeFluidCoolers
