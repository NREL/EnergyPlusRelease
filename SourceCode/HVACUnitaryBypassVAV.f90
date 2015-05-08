MODULE HVACUnitaryBypassVAV

  ! Module containing the routines for modeling changeover-bypass VAV systems

  ! MODULE INFORMATION:
  !       AUTHOR         Richard Raustad
  !       DATE WRITTEN   July 2006
  !       MODIFIED       B. Nigusse, FSEC - January 2012 - Added steam and hot water heating coils
  !
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms needed to simulate changeover-bypass
  ! variable-air-volume (CBVAV) systems, which are considered "Air Loop Equipment" in EnergyPlus

  ! METHODOLOGY EMPLOYED:
  ! Units are modeled as a collection of components: outside air mixer,
  ! supply air fan, DX cooing coil, DX/gas/elec heating coil, and variable volume boxes.
  ! Control is accomplished by calculating the load in all zones to determine a mode of operation.
  ! The system will either cool, heat, or operate based on fan mode selection.

  ! The CBVAV system is initialized with no load (coils off) to determine the outlet temperature.
  ! A setpoint temperature is calculated on FirstHVACIteration = TRUE to force one VAV box fully open.
  ! Once the setpoint is calculated, the inlet node mass flow rate on FirstHVACIteration = FALSE is used to
  ! determine the bypass fraction. The simulation converges quickly on mass flow rate. If the zone
  ! temperatures float in the deadband, additional iterations are required to converge on mass flow rate.

  ! REFERENCES:
  ! "Temp & VVT Commercial Comfort Systems," Engineering Training Manual, Technical Development Program, Carrier Corp., 1995.
  ! "VariTrac Changeover Bypass VAV (Tracker System CB)," VAV-PRC003-EN, Trane Company, June 2004.
  ! "Ventilation for Changeover-Bypass VAV Systems," D. Stanke, ASHRAE Journal Vol. 46, No. 11, November 2004.
  !  Lawrence Berkeley Laboratory. Nov. 1993. DOE-2 Supplement Version 2.1E, Winklemann et.al.

  ! OTHER NOTES: None

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, MaxNameLength, SysSizingCalc, WarmupFlag, DoingSizing, SecInHour, ScheduleAlwaysOn
USE DataInterfaces,  ONLY: SetupOutputVariable, ShowWarningError, ShowFatalError, ShowSevereError, &
                           ShowContinueError, ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd, &
                           ShowWarningMessage
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad, FanElecPower, DXElecCoolingPower, DXElecHeatingPower, &
                           OnOffFanPartLoadFraction, ElecHeatingCoilPower, SmallAirVolFlow, &
                           CycFanCycCoil, ContFanCycCoil, DrawThru, BlowThru, Coil_HeatingWater, Coil_HeatingSteam
USE DataEnvironment, ONLY: StdBaroPress, OutDryBulbTemp, OutBaroPress, StdRhoAir
USE DXCoils,         ONLY: DXCoilPartLoadRatio

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
! Compressor operation
INTEGER, PARAMETER :: On             = 1                ! Normal compressor operation
INTEGER, PARAMETER :: Off            = 0                ! Signal DXCoil that compressor should not run

! DX Coils supported in this module
INTEGER, PARAMETER :: CoilDX_CoolingSingleSpeed       = 1 ! Coil:DX:CoolingBypassFactorEmpirical
INTEGER, PARAMETER :: CoilDX_CoolingHXAssisted        = 2 ! Coil:DX:HeatExchangerAssisted
INTEGER, PARAMETER :: CoilDX_CoolingTwoStageWHumControl = 3 ! Coil:Cooling:DX:TwoStageWithHumidityControlMode
                                                 ! formerly (v3 and beyond) Coil:DX:MultiMode:CoolingEmpirical
INTEGER, PARAMETER :: CoilDX_HeatingEmpirical         = 4 ! Coil:DX:HeatingEmpirical
INTEGER, PARAMETER :: Coil_HeatingGas                 = 5 ! Coil:Gas:Heating
INTEGER, PARAMETER :: Coil_HeatingElectric            = 6 ! Coil:Electric:Heating

! Dehumidification control modes (DehumidControlMode) for Multimode units only
INTEGER, PARAMETER :: DehumidControl_None       = 0
INTEGER, PARAMETER :: DehumidControl_Multimode  = 1
INTEGER, PARAMETER :: DehumidControl_CoolReheat = 2

! Mode of operation
 INTEGER, PARAMETER :: CoolingMode         = 1            ! System operating mode is cooling
 INTEGER, PARAMETER :: HeatingMode         = 2            ! System operating mode is heating

! Priority control mode (prioritized thermostat signal)
INTEGER, PARAMETER :: CoolingPriority      = 1            ! Controls CBVAV system based on cooling priority
INTEGER, PARAMETER :: HeatingPriority      = 2            ! Controls CBVAV system based on heating priority
INTEGER, PARAMETER :: ZonePriority         = 3            ! Controls CBVAV system based on zone priority

! Airflow control for contant fan mode
INTEGER, PARAMETER :: UseCompressorOnFlow  = 1            ! Set compressor OFF air flow rate equal to compressor ON air flow rate
INTEGER, PARAMETER :: UseCompressorOffFlow = 2            ! Set compressor OFF air flow rate equal to user defined value

  ! DERIVED TYPE DEFINITIONS
TYPE CBVAVData
  ! input data
 CHARACTER(len=MaxNameLength) :: Name                     = ' ' ! Name of unit
 CHARACTER(len=MaxNameLength) :: UnitType                 = ' ' ! Type of unit
 CHARACTER(len=MaxNameLength) :: Sched                    = ' ' ! Availability schedule name
 INTEGER                      :: SchedPtr                 = 0   ! Index number to availability schedule
 REAL(r64)                    :: MaxCoolAirVolFlow        = 0.0d0 ! System air volumetric flow rate during cooling operation [m3/s]
 REAL(r64)                    :: MaxHeatAirVolFlow        = 0.0d0 ! System air volumetric flow rate during heating operation [m3/s]
 REAL(r64)                    :: MaxNoCoolHeatAirVolFlow  = 0.0d0 ! System air volumetric flow rate when no cooling or heating [m3/s]
 REAL(r64)                    :: MaxCoolAirMassFlow       = 0.0d0 ! System air mass flow rate during cooling operation [kg/s]
 REAL(r64)                    :: MaxHeatAirMassFlow       = 0.0d0 ! System air mass flow rate during heating operation [kg/s]
 REAL(r64)                    :: MaxNoCoolHeatAirMassFlow = 0.0d0 ! System air mass flow rate when no cooling or heating [kg/s]
 REAL(r64)                    :: CoolOutAirVolFlow        = 0.0d0 ! OA volumetric flow rate during cooling operation [m3/s]
 REAL(r64)                    :: HeatOutAirVolFlow        = 0.0d0 ! OA volumetric flow rate during heating operation [m3/s]
 REAL(r64)                    :: NoCoolHeatOutAirVolFlow  = 0.0d0 ! OA volumetric flow rate when no cooling or heating [m3/s]
 REAL(r64)                    :: CoolOutAirMassFlow       = 0.0d0 ! OA mass flow rate during cooling operation [kg/s]
 REAL(r64)                    :: HeatOutAirMassFlow       = 0.0d0 ! OA mass flow rate during heating operation [kg/s]
 REAL(r64)                    :: NoCoolHeatOutAirMassFlow = 0.0d0 ! OA mass flow rate when no cooling or heating [kg/s]
 INTEGER                      :: OutAirSchPtr             = 0   ! Index number to outside air multiplier schedule
 INTEGER                      :: AirInNode                = 0   ! Inlet air node number for CBVAV unit
 INTEGER                      :: AirOutNode               = 0   ! Outlet air node number for CBVAV unit
 INTEGER                      :: CondenserNodeNum         = 0   ! DX Coil condenser air inlet node number
 INTEGER                      :: MixerOutsideAirNode      = 0   ! Outside air node number for OA mixer
 INTEGER                      :: MixerMixedAirNode        = 0   ! Mixed air node number for OA mixer
 INTEGER                      :: MixerReliefAirNode       = 0   ! Relief air node number for OA mixer
 INTEGER                      :: MixerInletAirNode        = 0   ! Return air node number for OA mixer
 INTEGER                      :: SplitterOutletAirNode    = 0   ! Air node number for splitter (last component outlet node)
 CHARACTER(len=MaxNameLength) :: OAMixType                = ' ' ! type of outside air mixer
 CHARACTER(len=MaxNameLength) :: OAMixName                = ' ' ! Name of OA mixer
 INTEGER                      :: OAMixIndex               = 0   ! Index to OA mixer
 CHARACTER(len=MaxNameLength) :: FanName                  = ' ' ! Name of fan
 CHARACTER(len=MaxNameLength) :: FanType                  = ' ' ! Type of fan
 INTEGER                      :: FanPlace                 = 0   ! Fan placement is either blowthru (1) or drawthru (2)
 INTEGER                      :: FanType_Num              = 0   ! Fan type number (see DataHVACGlobals)
 INTEGER                      :: FanIndex                 = 0   ! Index number to fan
 INTEGER                      :: FanOpModeSchedPtr        = 0   ! Fan operating mode schedule pointer
 REAL(r64)                    :: FanVolFlow               = 0.0d0 ! Volumetric flow rate of system supply air fan [m3/s]
 REAL(r64)                    :: HeatingSpeedRatio        = 1.0d0 ! Fan speed ratio in heating mode
 REAL(r64)                    :: CoolingSpeedRatio        = 1.0d0 ! Fan speed ratio in cooling mode
 REAL(r64)                    :: NoHeatCoolSpeedRatio     = 1.0d0 ! Fan speed ratio when no cooling or heating
 LOGICAL                      :: CheckFanFlow          = .TRUE. ! Check fan volumetric flow versus system flow in init routine.
 CHARACTER(len=MaxNameLength) :: DXCoolCoilName           = ' ' ! Name of DX cooling coil
 CHARACTER(len=MaxNameLength) :: DXCoolCoilType           = ' ' ! Type of DX cooling coil,Coil:DX:CoolingBypassFactorEmpirical or
                                                                !               CoilSystem:Cooling:DX:HeatExchangerAssisted
 INTEGER                      :: DXCoolCoilType_Num       = 0   ! Numeric equivalent for DX cooling coil type
 INTEGER                      :: CoolCoilCompIndex        = 0   ! cooling coil component index number
 INTEGER                      :: DXCoolCoilIndexNum       = 0   ! actual DX cooling coil index number
 INTEGER                      :: DXHeatCoilIndexNum       = 0   ! actual DX heating coil index number
 CHARACTER(len=MaxNameLength) :: HeatCoilName             = ' ' ! Name of heating coil
 CHARACTER(len=MaxNameLength) :: HeatCoilType             = ' ' ! Type of heating coil,Coil:DX:HeatingEmpirical
                                                                ! Coil:Heater:Gas, Coil:Heater:Electric, Coil:Heater:Water
                                                                ! Coil:Heater:Steam
 INTEGER                      :: HeatCoilType_Num         = 0   ! Numeric equivalent for DX heating coil type
 INTEGER                      :: HeatCoilIndex            = 0   ! DX heating coil index number
 INTEGER                      :: OpMode                   =0    ! mode of operation; 1=cycling fan, cycling compressor
                                                                !                    2=continuous fan, cycling compresor
 INTEGER                      :: CoilControlNode          = 0   ! heating coil hot water or steam inlet node
 INTEGER                      :: CoilOutletNode      = 0   ! outlet node for hot water and steam coil
 INTEGER                      :: LoopNum                  = 0   ! plant loop index for water heating coil
 INTEGER                      :: LoopSide                 = 0   ! plant loop side  index for water heating coil
 INTEGER                      :: BranchNum                = 0   ! plant loop branch index for water heating coil
 INTEGER                      :: CompNum                  = 0   ! plant loop component index for water heating coil
 Integer                      :: HotWaterCoilMaxIterIndex  = 0  ! Index to recurring warning message
 Integer                      :: HotWaterCoilMaxIterIndex2 = 0  ! Index to recurring warning message
 REAL(r64)                    :: MaxHeatCoilFluidFlow     = 0.0d0 ! water or steam mass flow rate for heating coil [kg/s]
 REAL(r64)                    :: DesignHeatingCapacity    = 0.0d0 ! design heating capacity of the heating coil
 REAL(r64)                    :: DesignSuppHeatingCapacity= 0.0d0 ! Operating capacity of supplemental Heating Coil [W]
 REAL(r64)                    :: MinOATCompressor         = 0.0d0 ! Minimum OAT for compressor operation [C]
 REAL(r64)                    :: MinLATCooling            = 0.0d0 ! Minimum leaving air temp for compressor cooling operation [C]
 REAL(r64)                    :: MaxLATHeating            = 0.0d0 ! Maximum leaving air temp for heating operation [C]
 ! Report data
 REAL(r64)                    :: TotHeatEnergyRate        = 0.0d0 ! Total heating output [W]
 REAL(r64)                    :: TotHeatEnergy            = 0.0d0 ! Total heating output [J]
 REAL(r64)                    :: TotCoolEnergyRate        = 0.0d0 ! Total cooling output [W]
 REAL(r64)                    :: TotCoolEnergy            = 0.0d0 ! Total cooling output [J]
 REAL(r64)                    :: SensHeatEnergyRate       = 0.0d0 ! Sensible heating output [W]
 REAL(r64)                    :: SensHeatEnergy           = 0.0d0 ! Sensible heating output [J]
 REAL(r64)                    :: SensCoolEnergyRate       = 0.0d0 ! Sensible cooling output [W]
 REAL(r64)                    :: SensCoolEnergy           = 0.0d0 ! Sensible cooling output [J]
 REAL(r64)                    :: LatHeatEnergyRate        = 0.0d0 ! Latent heating output [W]
 REAL(r64)                    :: LatHeatEnergy            = 0.0d0 ! Latent heating output [J]
 REAL(r64)                    :: LatCoolEnergyRate        = 0.0d0 ! Latent cooling output [W]
 REAL(r64)                    :: LatCoolEnergy            = 0.0d0 ! Latent cooling output [J]
 REAL(r64)                    :: ElecPower                = 0.0d0 ! Electricity consumed [W]
 REAL(r64)                    :: ElecConsumption          = 0.0d0 ! Electricity consumed [J]
 REAL(r64)                    :: FanPartLoadRatio         = 0.0d0 ! Fan part-load ratio for time step
 REAL(r64)                    :: CompPartLoadRatio        = 0.0d0 ! Compressor part-load ratio for time step
 INTEGER                      :: LastMode                 = 0   ! Last mode of operation, coolingmode or heatingmode
 INTEGER                      :: AirFlowControl           = 0   ! Fan control mode, UseCompressorOnFlow or UseCompressorOffFlow
 REAL(r64)                    :: CompPartLoadFrac         = 0.0d0 ! Compressor part load ratio
 INTEGER                      :: AirLoopNumber            = 0   ! Air loop served by the CBVAV system
 INTEGER                      :: NumControlledZones       = 0
 INTEGER, DIMENSION(:), ALLOCATABLE :: ControlledZoneNum        ! Index to controlled zones
 INTEGER, DIMENSION(:), ALLOCATABLE :: ActualZoneNum            ! Actual zone number of controlled zone
 INTEGER, DIMENSION(:), ALLOCATABLE :: ActualZoneNodeNum        ! Actual zone node num of controlled zone
 INTEGER, DIMENSION(:), ALLOCATABLE :: CBVAVBoxOutletNode       ! Outlet node of CBVAV Box in controlled zone
 INTEGER, DIMENSION(:), ALLOCATABLE :: ZoneSequenceCoolingNum ! Index to cooling sequence/priority for this zone
 INTEGER, DIMENSION(:), ALLOCATABLE :: ZoneSequenceHeatingNum ! Index to heating sequence/priority for this zone

 INTEGER                      :: PriorityControl          = 0   ! Control mode - CoolingPriority, HeatingPriority, or ZonePriority
 INTEGER                      :: NumZonesCooled           = 0   ! Number of zones requesting cooling
 INTEGER                      :: NumZonesHeated           = 0   ! Number of zones requesting heating
 INTEGER                      :: PLRMaxIter               = 0   ! Counter for recurring warning message
 INTEGER                      :: PLRMaxIterIndex          = 0   ! Index to recurring warning message
 INTEGER                      :: DXCoilInletNode          = 0   ! Inlet node number of DX cooling coil
 INTEGER                      :: DXCoilOutletNode         = 0   ! Outlet node number of DX cooling coil
 INTEGER                      :: HeatingCoilInletNode     = 0   ! Inlet node of heating coil
 INTEGER                      :: HeatingCoilOutletNode    = 0   ! Outlet node of heating coil
 INTEGER                      :: FanInletNodeNum          = 0   ! fan inlet node number
 REAL(r64)                    :: OutletTempSetpoint       = 0.0d0 ! Oulet node temperature setpoint [C]
 REAL(r64)                    :: CoilTempSetpoint         = 0.0d0 ! Coil oulet node temperature setpoint (inc. fan heat) [C]
 INTEGER                      :: HeatCoolMode             = 0   ! System operating mode (0 = floating, 1 = cooling, 2 = heating)
 REAL(r64)                    :: BypassMassFlowRate       = 0.0d0 ! Bypass mass flow rate report variable [m3/s]
 INTEGER                      :: DehumidificationMode     = 0   ! Dehumidification mode (0=normal, 1=enhanced)
 INTEGER                      :: DehumidControlType       = 0   ! Dehumidification control type (currently only for multimode coil)
 LOGICAL                      :: HumRatMaxCheck         =.TRUE. ! Used in Init for warning messages
 INTEGER                      :: DXIterationExceeded      = 0   ! Counter for DX coil messages
 INTEGER                      :: DXIterationExceededIndex = 0   ! Counter for DX coil messages
 INTEGER                      :: DXIterationFailed        = 0   ! Counter for DX coil messages
 INTEGER                      :: DXIterationFailedIndex   = 0   ! Counter for DX coil messages
 INTEGER                      :: HXDXIterationExceeded    = 0   ! Counter for HX assisted DX coil messages
 INTEGER                      :: HXDXIterationExceededIndex= 0  ! Counter for HX assisted DX coil messages
 INTEGER                      :: HXDXIterationFailed      = 0   ! Counter for HX assisted DX coil messages
 INTEGER                      :: HXDXIterationFailedIndex = 0   ! Counter for HX assisted DX coil messages
 INTEGER                      :: MMDXIterationExceeded    = 0   ! Counter for multimode DX coil messages
 INTEGER                      :: MMDXIterationExceededIndex= 0  ! Counter for multimode DX coil messages
 INTEGER                      :: MMDXIterationFailed      = 0   ! Counter for multimode DX coil messages
 INTEGER                      :: MMDXIterationFailedIndex = 0   ! Counter for multimode DX coil messages
 INTEGER                      :: DMDXIterationExceeded    = 0   ! Counter for dehumidifying multimode DX coil messages
 INTEGER                      :: DMDXIterationExceededIndex= 0  ! Counter for dehumidifying multimode DX coil messages
 INTEGER                      :: DMDXIterationFailed      = 0   ! Counter for dehumidifying multimode DX coil messages
 INTEGER                      :: DMDXIterationFailedIndex = 0   ! Counter for dehumidifying multimode DX coil messages
 INTEGER                      :: CRDXIterationExceeded    = 0   ! Counter for cool reheat multimode DX coil messages
 INTEGER                      :: CRDXIterationExceededIndex= 0  ! Counter for cool reheat multimode DX coil messages
 INTEGER                      :: CRDXIterationFailed      = 0   ! Counter for cool reheat multimode DX coil messages
 INTEGER                      :: CRDXIterationFailedIndex = 0   ! Counter for cool reheat multimode DX coil messages
END TYPE CBVAVData

  ! MODULE VARIABLE DECLARATIONS:
TYPE (CBVAVData), ALLOCATABLE, DIMENSION(:) :: CBVAV

INTEGER      :: NumCBVAV               = 0     ! Number of CBVAV systems in input file
REAL(r64)    :: CompOnMassFlow         = 0.0d0   ! System air mass flow rate w/ compressor ON
REAL(r64)    :: OACompOnMassFlow       = 0.0d0   ! OA mass flow rate w/ compressor ON
REAL(r64)    :: CompOffMassFlow        = 0.0d0   ! System air mass flow rate w/ compressor OFF
REAL(r64)    :: OACompOffMassFlow      = 0.0d0   ! OA mass flow rate w/ compressor OFF
REAL(r64)    :: CompOnFlowRatio        = 0.0d0   ! fan flow ratio when coil on
REAL(r64)    :: CompOffFlowRatio       = 0.0d0   ! fan flow ratio when coil off
REAL(r64)    :: FanSpeedRatio          = 0.0d0   ! ratio of air flow ratio passed to fan object
REAL(r64)    :: ByPassDuctFlowFraction = 0.0d0   ! Fraction of unit mass flow that returns to inlet of CBVAV unit through bypass duct
REAL(r64)    :: PartLoadFrac           = 0.0d0   ! Compressor part-load fraction
REAL(r64)    :: SaveCompressorPLR      = 0.0d0   ! Holds DX compressor PLR from active DX coil
REAL(r64)    :: TempSteamIn            = 100.0d0 ! steam coil steam inlet temperature
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

PUBLIC  SimUnitaryBypassVAV
PRIVATE SimCBVAV
PRIVATE GetCBVAV
PRIVATE InitCBVAV
PRIVATE SizeCBVAV
PRIVATE ControlCBVAVOutput
PRIVATE CalcCBVAV
PRIVATE SetAverageAirFlow
PRIVATE ReportCBVAV
PRIVATE CalcNonDXHeatingCoils

CONTAINS

SUBROUTINE SimUnitaryBypassVAV(CompName, FirstHVACIteration, AirLoopNum, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of a changeover-bypass VAV system. Called from SimAirServingZones.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,        ONLY: TrimSigDigits
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT    (IN) :: CompName            ! Name of the CBVAV system
  LOGICAL,          INTENT    (IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system time step
  INTEGER,          INTENT    (IN) :: AirLoopNum          ! air loop index
  INTEGER,          INTENT (INOUT) :: CompIndex           ! Index to changeover-bypass VAV system

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: CBVAVNum                ! Index of CBVAV system being simulated
  LOGICAL,SAVE :: GetInputFlag = .TRUE.  ! First time, input is "gotten"
  REAL(r64)    :: OnOffAirFlowRatio      ! Ratio of compressor ON airflow to average airflow over timestep
  REAL(r64)    :: QUnitOut               ! Sensible capacity delivered by this air loop system
  REAL(r64)    :: QZnLoad                ! Zone load required by all zones served by this air loop system
  LOGICAL      :: HXUnitOn          ! flag to enable heat exchanger

          ! FLOW

  ! First time SimUnitaryBypassVAV is called, get the input for all the CBVAVs
  IF (GetInputFlag) THEN
    CALL GetCBVAV
    GetInputFlag = .FALSE.
  END IF

  ! Find the correct changeover-bypass VAV unit
  IF (CompIndex == 0) THEN
    CBVAVNum = FindItemInList(CompName,CBVAV%Name,NumCBVAV)
    IF (CBVAVNum == 0) THEN
      CALL ShowFatalError('SimUnitaryBypassVAV: Unit not found='//TRIM(CompName))
    END IF
    CompIndex=CBVAVNum
  ELSE
    CBVAVNum=CompIndex
    IF (CBVAVNum > NumCBVAV .or. CBVAVNum < 1) THEN
      CALL ShowFatalError('SimUnitaryBypassVAV:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(CBVAVNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumCBVAV))//  &
                          ', Entered Unit name='//TRIM(CompName))
    END IF
    IF (CheckEquipName(CBVAVNum)) THEN
      IF (CompName /= CBVAV(CBVAVNum)%Name) THEN
        CALL ShowFatalError('SimUnitaryBypassVAV: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(CBVAVNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(CBVAV(CBVAVNum)%Name))
      END IF
      CheckEquipName(CBVAVNum)=.false.
    ENDIF
  END IF

  OnOffAirFlowRatio = 0.0d0
  HXUnitOn = .TRUE.

  ! Initialize the changeover-bypass VAV system
  CALL InitCBVAV(CBVAVNum, FirstHVACIteration, AirLoopNum, QZnLoad, OnOffAirFlowRatio, HXUnitOn)

  ! Simulate the unit
  CALL SimCBVAV(CBVAVNum, FirstHVACIteration, QZnLoad, QUnitOut, OnOffAirFlowRatio, HXUnitOn)

  ! Report the result of the simulation
  CALL ReportCBVAV(CBVAVNum)

  RETURN
END SUBROUTINE SimUnitaryBypassVAV

SUBROUTINE SimCBVAV(CBVAVNum,FirstHVACIteration,QZnReq,QSensUnitOut,OnOffAirFlowRatio, HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a changeover-bypass VAV system.

          ! METHODOLOGY EMPLOYED:
          ! Calls ControlCBVAVOutput to obtain the desired unit output

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics,  ONLY: PsyHFnTdbW

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)    :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)    :: CBVAVNum            ! Index of the current CBVAV system being simulated
  REAL(r64),    INTENT (INOUT) :: QZnReq             ! Zone load for all zones served by this air loop system
  REAL(r64),    INTENT (OUT)   :: QSensUnitOut       ! Sensible delivered capacity [W]
  REAL(r64),    INTENT (INOUT) :: OnOffAirFlowRatio  ! Ratio of compressor ON airflow to AVERAGE airflow over timestep
  LOGICAL, INTENT (INOUT) :: HXUnitOn           ! flag to enable heat exchanger

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: UnitOn            ! TRUE if unit is on
  INTEGER :: OutletNode        ! CBVAV air outlet node
  INTEGER :: InletNode         ! CBVAV air inlet node
  REAL(r64)    :: QTotUnitOut       ! Total delivered capacity [W]
  REAL(r64)    :: AirMassFlow       ! Air mass flow rate [kg/s]
  REAL(r64)    :: HeatingPower      ! Power consumption of DX heating coil or electric heating coil [W]
  REAL(r64)    :: MinOutletHumRat   ! Minimum of inlet and outlet air humidity ratio [kg/kg]
  REAL(r64)    :: PartLoadFrac

  ! zero the fan and DX coils electricity consumption
  FanElecPower         = 0.0d0
  DXElecCoolingPower   = 0.0d0
  DXElecHeatingPower   = 0.0d0
  ElecHeatingCoilPower = 0.0d0
  SaveCompressorPLR    = 0.0d0

  ! initialize local variables
  UnitOn       = .TRUE.
  QSensUnitOut = 0.0d0
  OutletNode   = CBVAV(CBVAVNum)%AirOutNode
  InletNode    = CBVAV(CBVAVNum)%AirInNode
  AirMassFlow  = Node(InletNode)%MassFlowRate
  PartLoadFrac = 0.0d0

  ! set the on/off flags
  IF (CBVAV(CBVAVNum)%OPMode == CycFanCycCoil) THEN
    ! cycling unit only runs if there is a cooling or heating load.
     IF (CBVAV(CBVAVNum)%HeatCoolMode .EQ. 0 .OR. AirMassFlow < SmallMassFlow) THEN
       UnitOn = .FALSE.
     END IF
  ELSE IF  (CBVAV(CBVAVNum)%OPMode == ContFanCycCoil) THEN
    ! continuous unit: fan runs if scheduled on; coil runs only if there is a cooling or heating load
    IF (AirMassFlow.LT.SmallMassFlow) THEN
      UnitOn = .FALSE.
    END IF
  END IF

  OnOffFanPartLoadFraction = 1.0d0

  IF(UnitOn)THEN
    CALL ControlCBVAVOutput(CBVAVNum,FirstHVACIteration,QZnReq,PartLoadFrac,OnOffAirFlowRatio, HXUnitOn)
  ELSE
    CALL CalcCBVAV(CBVAVNum, FirstHVACIteration, PartLoadFrac, QSensUnitOut, QZnReq, OnOffAirFlowRatio, HXUnitOn)
  END IF

  ! calculate delivered capacity
  AirMassFlow = Node(OutletNode)%MassFlowRate

  QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(InletNode)%Enthalpy)

  MinOutletHumRat = MIN(Node(InletNode)%HumRat,Node(OutletNode)%HumRat)

  QSensUnitOut = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinOutletHumRat) - &
                                PsyHFnTdbW(Node(InletNode)%Temp,MinOutletHumRat))

  ! report variables
  CBVAV(CBVAVNum)%CompPartLoadRatio = SaveCompressorPLR
  IF (UnitOn) THEN
    CBVAV(CBVAVNum)%FanPartLoadRatio = 1.0d0
  ELSE
    CBVAV(CBVAVNum)%FanPartLoadRatio = 0.0d0
  END IF

  CBVAV(CBVAVNum)%TotCoolEnergyRate  = ABS(MIN(0.0d0, QTotUnitOut))
  CBVAV(CBVAVNum)%TotHeatEnergyRate  = ABS(MAX(0.0d0, QTotUnitOut))
  CBVAV(CBVAVNum)%SensCoolEnergyRate = ABS(MIN(0.0d0, QSensUnitOut))
  CBVAV(CBVAVNum)%SensHeatEnergyRate = ABS(MAX(0.0d0, QSensUnitOut))
  CBVAV(CBVAVNum)%LatCoolEnergyRate  = ABS(MIN(0.0d0, (QTotUnitOut - QSensUnitOut)))
  CBVAV(CBVAVNum)%LatHeatEnergyRate  = ABS(MAX(0.0d0, (QTotUnitOut - QSensUnitOut)))

  IF(CBVAV(CBVAVNum)%HeatCoilType_Num .EQ. CoilDX_HeatingEmpirical)THEN
    HeatingPower = DXElecHeatingPower
  ELSEIF(CBVAV(CBVAVNum)%HeatCoilType_Num .EQ. Coil_HeatingElectric)THEN
    HeatingPower = ElecHeatingCoilPower
  ELSE
    HeatingPower = 0.0d0
  END IF

  CBVAV(CBVAVNum)%ElecPower = FanElecPower + DXElecCoolingPower + HeatingPower

  RETURN
END SUBROUTINE SimCBVAV

SUBROUTINE GetCBVAV

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       Bereket Nigusse, FSEC, April 2011: added OA Mixer object type
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for changeover-bypass VAV systems and stores it in CBVAV data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,                  ONLY: GetFanType, GetFanIndex, GetFanVolFlow, GetFanInletNode, GetFanOutletNode
  USE General,               ONLY: TrimSigDigits
  USE DXCoils,               ONLY: GetMinOATDXCoilCompressor => GetMinOATCompressor, &
                                   GetDXCoilInletNode=>GetCoilInletNode, GetDXCoilOutletNode=>GetCoilOutletNode, &
                                   GetDXCoilCondenserInletNode=>GetCoilCondenserInletNode, GetDXCoolCoilIndex=>GetDXCoilIndex
  USE MixedAir,              ONLY: GetOAMixerNodeNumbers
  USE DataSizing,            ONLY: AutoSize
  USE DataAirLoop,           ONLY: AirToZoneNodeInfo
  USE HeatingCoils,          ONLY: GetCoilInletNode, GetCoilOutletNode
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE DataAirSystems,        ONLY: PrimaryAirSystem
  USE ScheduleManager,       ONLY: CheckScheduleValueMinMax, GetScheduleIndex
  USE DataHVACGlobals,       ONLY: FanType_SimpleConstVolume, FanType_SimpleVAV, FanType_SimpleOnOff, FanType_ZoneExhaust, &
                                   NumPrimaryAirSys
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE DataZoneEquipment,     ONLY: ZoneEquipConfig, ZoneEquipList,AirDistUnit_Num, DirectAir_Num
  USE BranchNodeConnections, ONLY: SetUpCompSets, TestCompSet
  USE DataZoneControls,      ONLY: TempControlledZone, NumTempControlledZones
  USE HVACHXAssistedCoolingCoil,  ONLY: GetHXDXCoilName, GetHXDXCoilInletNode=>GetCoilInletNode, &
                                        GetHXDXCoilOutletNode=>GetCoilOutletNode

  USE SteamCoils,            ONLY: GetSteamCoilAirInletNode=>GetCoilAirInletNode, GetSteamCoilIndex, &
                                   GetSteamCoilAirOutletNode=>GetCoilAirOutletNode, &
                                   GetSteamCoilSteamInletNode=>GetCoilSteamInletNode, &
                                   GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, GetTypeOfCoil, ZoneLoadControl
  USE WaterCoils,            ONLY: GetCoilWaterInletNode, GetCoilMaxWaterFlowRate, &
                                   GetWaterCoilInletNode=>GetCoilInletNode,GetWaterCoilOutletNode=>GetCoilOutletNode
  USE FluidProperties,       ONLY: GetSatDensityRefrig

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
  INTEGER                        :: CBVAVIndex        ! Loop index
  INTEGER                        :: CBVAVNum          ! Current CBVAV number
  CHARACTER(len=MaxNameLength), &
                   DIMENSION(19) :: Alphas            ! Alpha items for object
  REAL(r64),       DIMENSION(11) :: Numbers           ! Numeric items for object
  CHARACTER(len=MaxNameLength)   :: CompSetFanInlet   ! Used in SetUpCompSets call
  CHARACTER(len=MaxNameLength)   :: CompSetCoolInlet  ! Used in SetUpCompSets call
  CHARACTER(len=MaxNameLength)   :: CompSetFanOutlet  ! Used in SetUpCompSets call
  CHARACTER(len=MaxNameLength)   :: CompSetCoolOutlet ! Used in SetUpCompSets call
  INTEGER                        :: NumAlphas         ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers        ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus          ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.false.    ! Set to true if errors in input, fatal at end of routine
  LOGICAL                        :: DXErrorsFound=.false.  ! Set to true if errors in get coil input
!unused0509  LOGICAL                        :: FanErrorsFound=.false. ! Set to true if errors in get fan input
  LOGICAL                        :: IsNotOK           ! Flag to verify name
  LOGICAL                        :: IsBlank           ! Flag for blank name
  CHARACTER(len=MaxNameLength)   :: CurrentModuleObject    ! Object type for getting and error messages
  LOGICAL                        :: FanErrFlag = .FALSE. ! Error flag returned during CALL to GetFanType
  LOGICAL                        :: ErrFlag = .FALSE. ! Error flag returned during CALL to mining functions
  INTEGER                        :: FanIndex          ! Index to CBVAV's supply air fan
  REAL(r64)                      :: FanVolFlow        ! Maximum air volumetric flow rate of fan
  INTEGER                        :: AirLoopNum        ! Index to air loop served by this system
  INTEGER                        :: AirLoopZoneNum    ! Index to controlled zone
  INTEGER                        :: BranchNum         ! Index to branch containing this system
  INTEGER                        :: CompNum           ! Index to this system
  INTEGER, DIMENSION(4)          :: OANodeNums        ! Node numbers of OA mixer (OA, EA, RA, MA)
  CHARACTER(len=MaxNameLength)   :: HXDXCoolCoilName  ! Name of DX cooling coil used with Heat Exchanger Assisted Cooling Coil
  CHARACTER(len=MaxNameLength)   :: MixerInletNodeName     ! Name of mixer inlet node
  CHARACTER(len=MaxNameLength)   :: SplitterOutletNodeName ! Name of splitter outlet node
  INTEGER                        :: ControlNodeNum    ! Number of control zone when humidity control is specified
  INTEGER                        :: TstatZoneNum      ! Index to thermostats
  LOGICAL                        :: FoundTstatZone    ! TRUE if thermostat found in controlled zone
  LOGICAL                        :: OANodeErrFlag     ! TRUE if DX Coil condenser node is not found
  LOGICAL                        :: DXCoilErrFlag     ! used in warning messages
  CHARACTER(Len=MaxNameLength), DIMENSION(19) :: cAlphaFields   ! Alpha field names
  CHARACTER(Len=MaxNameLength), DIMENSION(11) :: cNumericFields ! Numeric field names
  LOGICAL,                      DIMENSION(19) :: lAlphaBlanks   ! Logical array, alpha field input BLANK = .true.
  LOGICAL,                      DIMENSION(11) :: lNumericBlanks ! Logical array, numeric field input BLANK = .true.
  INTEGER                        :: EquipNum = 0                ! local do loop index for equipment listed for a zone
  INTEGER                        :: HeatCoilInletNodeNum        ! Heating coil air inlet node number
  INTEGER                        :: HeatCoilOutletNodeNum       ! Heating coil air outlet node number
  INTEGER                        :: TempNodeNum                 ! HW coil water inlet node
  INTEGER                        :: SteamIndex                  ! steam coil index
  REAL(r64)                      :: SteamDensity                ! steam coil steam density

  Alphas = ' '
  Numbers = 0.0d0
  cAlphaFields=' '
  cNumericFields=' '
  lAlphaBlanks=.true.
  lNumericBlanks=.true.

  ! find the number of each type of CBVAV unit
  CurrentModuleObject = 'AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass'
  NumCBVAV = GetNumObjectsFound(CurrentModuleObject)

  ! allocate the data structures
  ALLOCATE(CBVAV(NumCBVAV))
  ALLOCATE(CheckEquipName(NumCBVAV))
  CheckEquipName=.true.

  ! loop over CBVAV units; get and load the input data
  DO CBVAVIndex = 1,NumCBVAV
    HeatCoilInletNodeNum = 0
    HeatCoilOutletNodeNum = 0
    CALL GetObjectItem(CurrentModuleObject,CBVAVIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    CBVAVNum = CBVAVIndex
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),CBVAV%Name,CBVAVNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    CBVAV(CBVAVNum)%Name = Alphas(1)
    CBVAV(CBVAVNum)%UnitType = CurrentModuleObject
    CBVAV(CBVAVNum)%Sched = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      CBVAV(CBVAVNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      CBVAV(CBVAVNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer (index number)
      IF (CBVAV(CBVAVNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' '//TRIM(cAlphaFields(2))//' not found = '//TRIM(Alphas(2)))
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        ErrorsFound=.TRUE.
      END IF
    ENDIF

    CBVAV(CBVAVNum)%MaxCoolAirVolFlow       = Numbers(1)
    IF (CBVAV(CBVAVNum)%MaxCoolAirVolFlow .LE. 0.0d0 .AND. CBVAV(CBVAVNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(1))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(1),7)))
      CALL ShowContinueError(TRIM(cNumericFields(1))//' must be greater than zero.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    CBVAV(CBVAVNum)%MaxHeatAirVolFlow       = Numbers(2)
    IF (CBVAV(CBVAVNum)%MaxHeatAirVolFlow .LE. 0.0d0 .AND. CBVAV(CBVAVNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(2))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(2),7)))
      CALL ShowContinueError(TRIM(cNumericFields(2))//' must be greater than zero.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow = Numbers(3)
    IF (CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .LT. 0.0d0 .AND. CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(3))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(3),7)))
      CALL ShowContinueError(TRIM(cNumericFields(3))//' must be greater than or equal to zero.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    CBVAV(CBVAVNum)%CoolOutAirVolFlow       = Numbers(4)
    IF (CBVAV(CBVAVNum)%CoolOutAirVolFlow .LT. 0.0d0 .AND. CBVAV(CBVAVNum)%CoolOutAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(4))//' = ' &
                         //TRIM(TrimSigDigits(Numbers(4),7)))
      CALL ShowContinueError(TRIM(cNumericFields(4))//' must be greater than or equal to zero.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    CBVAV(CBVAVNum)%HeatOutAirVolFlow       = Numbers(5)
    IF (CBVAV(CBVAVNum)%HeatOutAirVolFlow .LT. 0.0d0 .AND. CBVAV(CBVAVNum)%HeatOutAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(5))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(5),7)))
      CALL ShowContinueError(TRIM(cNumericFields(5))//' must be greater than or equal to zero.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow = Numbers(6)
    IF (CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow .LT. 0.0d0 .AND. CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow .NE. AutoSize) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cNumericFields(6))//' = ' &
                           //TRIM(TrimSigDigits(Numbers(6),7)))
      CALL ShowContinueError(TRIM(cNumericFields(6))//' must be greater than or equal to zero.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    CBVAV(CBVAVNum)%OutAirSchPtr = GetScheduleIndex(Alphas(3))  ! convert schedule name to pointer (index number)
    IF (CBVAV(CBVAVNum)%OutAirSchPtr .NE. 0) THEN
      IF(.NOT. CheckScheduleValueMinMax(CBVAV(CBVAVNum)%OutAirSchPtr,'<', 0.0d0, '>', 1.0d0))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
        CALL ShowContinueError('The schedule values in '//TRIM(cAlphaFields(3))//' must be 0 to 1.')
        ErrorsFound=.TRUE.
      END IF
    END IF

    CBVAV(CBVAVNum)%AirInNode =  GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                                                   NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

    MixerInletNodeName       = Alphas(5)
    SplitterOutletNodeName   = Alphas(6)

    CBVAV(CBVAVNum)%AirOutNode = GetOnlySingleNode(Alphas(7),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                                                   NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

    CBVAV(CBVAVNum)%MixerInletAirNode = GetOnlySingleNode(MixerInletNodeName,ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                                                          NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent)

    CBVAV(CBVAVNum)%MixerInletAirNode = &
               GetOnlySingleNode(MixerInletNodeName,ErrorsFound,TRIM(CurrentModuleObject),TRIM(Alphas(1)//'_Mixer'), &
                                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

    CBVAV(CBVAVNum)%SplitterOutletAirNode = &
               GetOnlySingleNode(SplitterOutletNodeName,ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                                 NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent)

    CBVAV(CBVAVNum)%SplitterOutletAirNode = &
               GetOnlySingleNode(SplitterOutletNodeName,ErrorsFound,TRIM(CurrentModuleObject),TRIM(Alphas(1)//'_Splitter'), &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

    CBVAV(CBVAVNum)%OAMixType  = Alphas(8)
    CBVAV(CBVAVNum)%OAMixName  = Alphas(9)

    ErrFlag = .false.
    CALL ValidateComponent(CBVAV(CBVAVNum)%OAMixType,CBVAV(CBVAVNum)%OAMixName,ErrFlag,TRIM(CurrentModuleObject))
    IF (ErrFlag) THEN
       CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(CBVAV(CBVAVNum)%Name)//'".')
       ErrorsFound = .TRUE.
    ELSE
       ! Get OA Mixer node numbers
       OANodeNums = GetOAMixerNodeNumbers(CBVAV(CBVAVNum)%OAMixName, ErrFlag)
       IF (ErrFlag) THEN
         CALL ShowContinueError('that was specified in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
         CALL ShowContinueError('..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.')
         ErrorsFound=.true.
       ELSE
         CBVAV(CBVAVNum)%MixerOutsideAirNode = OANodeNums(1)
         CBVAV(CBVAVNum)%MixerReliefAirNode  = OANodeNums(2)
         ! CBVAV(CBVAVNum)%MixerInletAirNode  = OANodeNums(3)
         CBVAV(CBVAVNum)%MixerMixedAirNode   = OANodeNums(4)
       ENDIF
    ENDIF

    IF(CBVAV(CBVAVNum)%MixerInletAirNode .NE. OANodeNums(3))THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(5))//' = '//TRIM(MixerInletNodeName)//'.')
      CALL ShowContinueError(TRIM(cAlphaFields(5))//' must be the same as'// &
                             ' the return air stream node specified in the OutdoorAir:Mixer object.')
      ErrorsFound = .TRUE.
    END IF

    IF(CBVAV(CBVAVNum)%MixerInletAirNode .EQ. CBVAV(CBVAVNum)%AirInNode)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(5))//' = '//TRIM(MixerInletNodeName)//'.')
      CALL ShowContinueError(TRIM(cAlphaFields(5))//' must be different than the '//TRIM(cAlphaFields(4))//'.')
      ErrorsFound = .TRUE.
    END IF

    IF(CBVAV(CBVAVNum)%SplitterOutletAirNode .EQ. CBVAV(CBVAVNum)%AirOutNode)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(SplitterOutletNodeName)//'.')
      CALL ShowContinueError(TRIM(cAlphaFields(6))//' must be different than the '//TRIM(cAlphaFields(7))//'.')
      ErrorsFound = .TRUE.
    END IF

    CBVAV(CBVAVNum)%FanType       = Alphas(10)
    CBVAV(CBVAVNum)%FanName       = Alphas(11)

    IF(SameString(Alphas(12),'BlowThrough'))THEN
      CBVAV(CBVAVNum)%FanPlace    = BlowThru
    ELSEIF(SameString(Alphas(12),'DrawThrough'))THEN
      CBVAV(CBVAVNum)%FanPlace    = DrawThru
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(12))//' = '//TRIM(Alphas(12)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    IF(CBVAV(CBVAVNum)%FanPlace == DrawThru)THEN
      IF(CBVAV(CBVAVNum)%SplitterOutletAirNode .NE. &
           GetFanOutletNode(CBVAV(CBVAVNum)%FanType,CBVAV(CBVAVNum)%FanName,ErrorsFound))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
        CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(SplitterOutletNodeName)//'.')
        CALL ShowContinueError(TRIM(cAlphaFields(6))//' must be the same as the fan outlet node specified in ' &
                               //TRIM(cAlphaFields(10))//' = '//TRIM(CBVAV(CBVAVNum)%FanType)//': '// &
                               TRIM(CBVAV(CBVAVNum)%FanName)//' when draw through '//TRIM(cAlphaFields(11))//' is selected.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    CALL GetFanType(CBVAV(CBVAVNum)%FanName,CBVAV(CBVAVNum)%FanType_Num,FanErrFlag,CurrentModuleObject,CBVAV(CBVAVNum)%Name)
    IF(FanErrFlag)THEN
      CALL ShowContinueError(' illegal '//TRIM(cAlphaFields(11))//'="'//TRIM(Alphas(11))//'".')
      ErrorsFound = .TRUE.
      FanVolFlow = 9999.0d0
    ELSE
      CBVAV(CBVAVNum)%FanInletNodeNum=GetFanInletNode(CBVAV(CBVAVNum)%FanType,CBVAV(CBVAVNum)%FanName,FanErrFlag)
      CALL GetFanIndex(CBVAV(CBVAVNum)%FanName,FanIndex,FanErrFlag)
      CALL GetFanVolFlow(FanIndex,CBVAV(CBVAVNum)%FanVolFlow)
      FanVolFlow = CBVAV(CBVAVNum)%FanVolFlow
    END IF

    IF(FanVolFlow .NE. AutoSize)THEN
      IF(FanVolFlow .LT. CBVAV(CBVAVNum)%MaxCoolAirVolFlow .AND. CBVAV(CBVAVNum)%MaxCoolAirVolFlow .NE. AutoSize)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = '//TRIM(TrimSigDigits(FanVolFlow,7))// &
                              ' in '//TRIM(cAlphaFields(11))//' = '//TRIM(CBVAV(CBVAVNum)%FanName)//' is less than the '// &
                              TRIM(cNumericFields(1)))
        CALL ShowContinueError(' '//TRIM(cNumericFields(1))//' is reset to the'// &
                               ' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%MaxCoolAirVolFlow = FanVolFlow
      END IF
      IF(FanVolFlow .LT. CBVAV(CBVAVNum)%MaxHeatAirVolFlow .AND. CBVAV(CBVAVNum)%MaxHeatAirVolFlow .NE. AutoSize)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = '//TRIM(TrimSigDigits(FanVolFlow,7))// &
                              ' in '//TRIM(cAlphaFields(11))//' = '//TRIM(CBVAV(CBVAVNum)%FanName)//' is less than the '// &
                              TRIM(cNumericFields(2)))
        CALL ShowContinueError(' '//TRIM(cNumericFields(2))//' is reset to the'// &
                               ' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%MaxHeatAirVolFlow = FanVolFlow
      END IF
    END IF

!   only check that OA flow in cooling is >= SA flow in cooling when they are not autosized
    IF (CBVAV(CBVAVNum)%CoolOutAirVolFlow .GT. CBVAV(CBVAVNum)%MaxCoolAirVolFlow .AND. &
        CBVAV(CBVAVNum)%CoolOutAirVolFlow .NE. AutoSize .AND. CBVAV(CBVAVNum)%MaxCoolAirVolFlow .NE. AutoSize) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': '//TRIM(cNumericFields(4))//' cannot be greater than '// &
                           TRIM(cNumericFields(1)))
      CALL ShowContinueError(' '//TRIM(cNumericFields(4))//' is reset to the'// &
                             ' fan flow rate and the simulation continues.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      CBVAV(CBVAVNum)%CoolOutAirVolFlow = FanVolFlow
    END IF

!   only check that SA flow in heating is >= OA flow in heating when they are not autosized
    IF (CBVAV(CBVAVNum)%HeatOutAirVolFlow .GT. CBVAV(CBVAVNum)%MaxHeatAirVolFlow .AND. &
        CBVAV(CBVAVNum)%HeatOutAirVolFlow .NE. AutoSize .AND. CBVAV(CBVAVNum)%MaxHeatAirVolFlow .NE. AutoSize) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': '//TRIM(cNumericFields(5))//' cannot be greater than '// &
                           TRIM(cNumericFields(2)))
      CALL ShowContinueError(' '//TRIM(cNumericFields(5))//' is reset to the'// &
                             ' fan flow rate and the simulation continues.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      CBVAV(CBVAVNum)%HeatOutAirVolFlow = FanVolFlow
    END IF

    IF(SameString(Alphas(14),'Coil:Cooling:DX:SingleSpeed') .OR. &
       SameString(Alphas(14),'CoilSystem:Cooling:DX:HeatExchangerAssisted') .OR. &
       SameString(Alphas(14),'Coil:Cooling:DX:TwoStageWithHumidityControlMode')) THEN

      CBVAV(CBVAVNum)%DXCoolCoilType = Alphas(14)
      CBVAV(CBVAVNum)%DXCoolCoilName = Alphas(15)

      IF (SameString(Alphas(14),'Coil:Cooling:DX:SingleSpeed')) THEN
        CBVAV(CBVAVNum)%DXCoolCoilType_Num = CoilDX_CoolingSingleSpeed
        CBVAV(CBVAVNum)%DXCoilInletNode = &
                      GetDXCoilInletNode(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,DXErrorsFound)
        CBVAV(CBVAVNum)%DXCoilOutletNode = &
                      GetDXCoilOutletNode(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,DXErrorsFound)
        IF(DXErrorsFound)THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
          CALL ShowContinueError('Coil:Cooling:DX:SingleSpeed "'//TRIM(CBVAV(CBVAVNum)%DXCoolCoilName)//'" not found.')
          ErrorsFound = .TRUE.
        ELSE

          DXCoilErrFlag=.false.
          CALL GetDXCoolCoilIndex(CBVAV(CBVAVNum)%DXCoolCoilName,CBVAV(CBVAVNum)%DXCoolCoilIndexNum, &
                              DXCoilErrFlag, CBVAV(CBVAVNum)%DXCoolCoilType)
          IF(DXCoilErrFlag)CALL ShowContinueError('...occurs in '//TRIM(CBVAV(CBVAVNum)%UnitType)// &
                                                  ' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')

!         Mine outdoor condenser node from DX coil object
          OANodeErrFlag = .FALSE.
          CBVAV(CBVAVNum)%CondenserNodeNum = &
            GetDXCoilCondenserInletNode(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,OANodeErrFlag)
          IF(OANodeErrFlag)CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        END IF
      ELSEIF (SameString(Alphas(14),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
        CBVAV(CBVAVNum)%DXCoolCoilType_Num = CoilDX_CoolingHXAssisted
        HXDXCoolCoilName = GetHXDXCoilName(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,DXErrorsFound)
        CBVAV(CBVAVNum)%DXCoilInletNode = &
                      GetHXDXCoilInletNode(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,DXErrorsFound)
        CBVAV(CBVAVNum)%DXCoilOutletNode = &
                      GetHXDXCoilOutletNode(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,DXErrorsFound)
        IF(DXErrorsFound)THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
          CALL ShowContinueError('CoilSystem:Cooling:DX:HeatExchangerAssisted "'//TRIM(CBVAV(CBVAVNum)%DXCoolCoilName)//  &
             '" not found.')
          ErrorsFound = .TRUE.
        ELSE

          DXCoilErrFlag=.false.
          CALL GetDXCoolCoilIndex( &
               GetHXDXCoilName(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,DXCoilErrFlag), &
               CBVAV(CBVAVNum)%DXCoolCoilIndexNum, DXCoilErrFlag, 'Coil:Cooling:DX:SingleSpeed')
          IF(DXCoilErrFlag)CALL ShowContinueError('...occurs in '//TRIM(CBVAV(CBVAVNum)%UnitType)// &
                                                  ' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')

!         Mine outdoor condenser node from DX coil through HXAssistedDXCoil object
          OANodeErrFlag = .FALSE.
          CBVAV(CBVAVNum)%CondenserNodeNum = &
            GetDXCoilCondenserInletNode('Coil:Cooling:DX:SingleSpeed', HXDXCoolCoilName, OANodeErrFlag)
          IF(OANodeErrFlag)CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        END IF
      ELSEIF (SameString(Alphas(14),'Coil:Cooling:DX:TwoStageWithHumidityControlMode')) THEN
        CBVAV(CBVAVNum)%DXCoolCoilType_Num = CoilDX_CoolingTwoStageWHumControl
        CBVAV(CBVAVNum)%DXCoilInletNode = &
                      GetDXCoilInletNode(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,DXErrorsFound)
        CBVAV(CBVAVNum)%DXCoilOutletNode = &
                      GetDXCoilOutletNode(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,DXErrorsFound)
        IF(DXErrorsFound)THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
          CALL ShowContinueError('Coil:Cooling:DX:TwoStageWithHumidityControlMode "'//TRIM(CBVAV(CBVAVNum)%DXCoolCoilName)// &
                                 '" not found.')
          ErrorsFound = .TRUE.
        ELSE

          DXCoilErrFlag=.false.
          CALL GetDXCoolCoilIndex(CBVAV(CBVAVNum)%DXCoolCoilName,CBVAV(CBVAVNum)%DXCoolCoilIndexNum, &
                              DXCoilErrFlag, CBVAV(CBVAVNum)%DXCoolCoilType)
          IF(DXCoilErrFlag)CALL ShowContinueError('...occurs in '//TRIM(CBVAV(CBVAVNum)%UnitType)// &
                                                  ' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')

!         Mine outdoor condenser node from multimode DX coil object
          OANodeErrFlag = .FALSE.
          CBVAV(CBVAVNum)%CondenserNodeNum = &
            GetDXCoilCondenserInletNode(CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,OANodeErrFlag)
          IF(OANodeErrFlag)CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        END IF
      END IF

    ELSE
       CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
       CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(14))//' = '//TRIM(Alphas(14)))
       ErrorsFound = .TRUE.
    END IF

    CBVAV(CBVAVNum)%FanOpModeSchedPtr = GetScheduleIndex(Alphas(13))  ! convert schedule name to pointer (index number)
    IF (CBVAV(CBVAVNum)%FanOpModeSchedPtr .NE. 0) THEN
      IF(.NOT. CheckScheduleValueMinMax(CBVAV(CBVAVNum)%FanOpModeSchedPtr, '<', 0.0d0, '>', 1.0d0))THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
        CALL ShowContinueError('The schedule values in '//TRIM(cAlphaFields(13))//' must be 0 to 1.')
        CALL ShowContinueError('A value of 0 represents cycling fan mode, any other value up to 1 represents constant fan mode.')
        ErrorsFound=.TRUE.
      END IF

!     Check supply air fan operating mode for cycling fan, if NOT cycling fan set AirFlowControl
      IF(.NOT. CheckScheduleValueMinMax(CBVAV(CBVAVNum)%FanOpModeSchedPtr,'>=',0.0d0,'<=',0.0d0))THEN !Objexx Range is 0 to 0?
!       set air flow control mode,
!       UseCompressorOnFlow  = operate at last cooling or heating air flow requested when compressor is off
!       UseCompressorOffFlow = operate at value specified by user (no input for this object type, UseCompONFlow)
!       AirFlowControl only valid if fan opmode = ContFanCycCoil
        IF (CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .EQ. 0.0d0) THEN
            CBVAV(CBVAVNum)%AirFlowControl = UseCompressorOnFlow
        ELSE
          CBVAV(CBVAVNum)%AirFlowControl = UseCompressorOffFlow
        END IF
      END IF

    ELSE
      IF(.NOT. lAlphaBlanks(13))THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
        CALL ShowContinueError(TRIM(cAlphaFields(13))//' = '//TRIM(Alphas(13))// &
                               ' not found. Supply air fan operating mode set to constant operation and simulation continues.')
      END IF
      CBVAV(CBVAVNum)%OpMode = ContFanCycCoil
      IF (CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .EQ. 0.0d0) THEN
          CBVAV(CBVAVNum)%AirFlowControl = UseCompressorOnFlow
      ELSE
        CBVAV(CBVAVNum)%AirFlowControl = UseCompressorOffFlow
      END IF
    END IF

!   Check FanVolFlow, must be >= CBVAV flow
    IF(FanVolFlow .NE. AutoSize)THEN
      IF(FanVolFlow .LT. CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .AND. CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize &
         .AND. CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .NE. 0.0d0)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = '//TRIM(TrimSigDigits(FanVolFlow,7))// &
                              ' in '//TRIM(cAlphaFields(11))//' = '//TRIM(CBVAV(CBVAVNum)%FanName)//' is less than '// &
                              TRIM(cNumericFields(3)))
        CALL ShowContinueError(' '//TRIM(cNumericFields(3))//' is reset to the'// &
                               ' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow = FanVolFlow
      END IF
    END IF
!   only check that OA flow when compressor is OFF is >= SA flow when compressor is OFF when both are not autosized and
!   that MaxNoCoolHeatAirVolFlow is /= 0 (trigger to use compressor ON flow, see AirFlowControl variable initialization above)
    IF (CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow .GT. CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .AND. &
        CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow .NE. AutoSize .AND. CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .NE. AutoSize .AND. &
        CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .NE. 0.0d0) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//': '//TRIM(cNumericFields(6))//' cannot be greater than '//  &
                             TRIM(cNumericFields(3)))
        CALL ShowContinueError(' '//TRIM(cNumericFields(6))//' is reset to'// &
                               ' the fan flow rate and the simulation continues.')
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow = FanVolFlow
    END IF

    IF(SameString(Alphas(16),'Coil:Heating:DX:SingleSpeed') .OR. &
       SameString(Alphas(16),'Coil:Heating:Gas') .OR. &
       SameString(Alphas(16),'Coil:Heating:Electric').OR. &
       SameString(Alphas(16),'Coil:Heating:Water').OR. &
       SameString(Alphas(16),'Coil:Heating:Steam')) THEN
       CBVAV(CBVAVNum)%HeatCoilType   = Alphas(16)
       CBVAV(CBVAVNum)%HeatCoilName   = Alphas(17)

       IF (SameString(Alphas(16),'Coil:Heating:DX:SingleSpeed')) THEN
         CBVAV(CBVAVNum)%HeatCoilType_Num = CoilDX_HeatingEmpirical
         CBVAV(CBVAVNum)%MinOATCompressor        = &
           GetMinOATDXCoilCompressor(CBVAV(CBVAVNum)%HeatCoilType,CBVAV(CBVAVNum)%HeatCoilName,ErrorsFound)
         CBVAV(CBVAVNum)%HeatingCoilInletNode = &
                       GetDXCoilInletNode(CBVAV(CBVAVNum)%HeatCoilType,CBVAV(CBVAVNum)%HeatCoilName,ErrorsFound)
         CBVAV(CBVAVNum)%HeatingCoilOutletNode = &
                       GetDXCoilOutletNode(CBVAV(CBVAVNum)%HeatCoilType,CBVAV(CBVAVNum)%HeatCoilName,ErrorsFound)
         CALL GetDXCoolCoilIndex(CBVAV(CBVAVNum)%HeatCoilName,CBVAV(CBVAVNum)%DXHeatCoilIndexNum, &
                                 DXCoilErrFlag, CBVAV(CBVAVNum)%HeatCoilType)
         IF(DXCoilErrFlag)CALL ShowContinueError('...occurs in '//TRIM(CBVAV(CBVAVNum)%UnitType)// &
                                                 ' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')

       ELSEIF (SameString(Alphas(16),'Coil:Heating:Gas')) THEN
         CBVAV(CBVAVNum)%HeatCoilType_Num = Coil_HeatingGas
         CBVAV(CBVAVNum)%MinOATCompressor        = -999.9d0
         CBVAV(CBVAVNum)%HeatingCoilInletNode = &
                       GetCoilInletNode(CBVAV(CBVAVNum)%HeatCoilType,CBVAV(CBVAVNum)%HeatCoilName,ErrorsFound)
         CBVAV(CBVAVNum)%HeatingCoilOutletNode = &
                       GetCoilOutletNode(CBVAV(CBVAVNum)%HeatCoilType,CBVAV(CBVAVNum)%HeatCoilName,ErrorsFound)
       ELSEIF (SameString(Alphas(16),'Coil:Heating:Electric')) THEN
         CBVAV(CBVAVNum)%HeatCoilType_Num = Coil_HeatingElectric
         CBVAV(CBVAVNum)%MinOATCompressor        = -999.9d0
         CBVAV(CBVAVNum)%HeatingCoilInletNode = &
                       GetCoilInletNode(CBVAV(CBVAVNum)%HeatCoilType,CBVAV(CBVAVNum)%HeatCoilName,ErrorsFound)
         CBVAV(CBVAVNum)%HeatingCoilOutletNode = &
                       GetCoilOutletNode(CBVAV(CBVAVNum)%HeatCoilType,CBVAV(CBVAVNum)%HeatCoilName,ErrorsFound)
       ELSEIF (SameString(Alphas(16),'Coil:Heating:Water')) THEN
         CBVAV(CBVAVNum)%HeatCoilType_Num = Coil_HeatingWater
         ErrFlag = .FALSE.
         CBVAV(CBVAVNum)%CoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water',  &
                                                 CBVAV(CBVAVNum)%HeatCoilName,ErrFlag)
         CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                CBVAV(CBVAVNum)%HeatCoilName,ErrFlag)
         HeatCoilInletNodeNum = GetWaterCoilInletNode('Coil:Heating:Water', &
                                CBVAV(CBVAVNum)%HeatCoilName,ErrFlag)
         CBVAV(CBVAVNum)%HeatingCoilInletNode = HeatCoilInletNodeNum
         HeatCoilOutletNodeNum = GetWaterCoilOutletNode('Coil:Heating:Water', &
                                 CBVAV(CBVAVNum)%HeatCoilName,ErrFlag)
         CBVAV(CBVAVNum)%HeatingCoilOutletNode = HeatCoilOutletNodeNum
         IF(ErrFlag)THEN
           CALL ShowContinueError('...occurs in '//TRIM(CBVAV(CBVAVNum)%UnitType)// &
                                             ' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')
           ErrorsFound = .TRUE.
         END IF
       ELSEIF (SameString(Alphas(16),'COIL:HEATING:STEAM')) THEN
         CBVAV(CBVAVNum)%HeatCoilType_Num = Coil_HeatingSteam
         ErrFlag = .FALSE.
         CBVAV(CBVAVNum)%HeatCoilIndex      = GetSTeamCoilIndex('COIL:HEATING:STEAM',CBVAV(CBVAVNum)%HeatCoilName,ErrFlag)

         HeatCoilInletNodeNum = GetSteamCoilAirInletNode(CBVAV(CBVAVNum)%HeatCoilIndex, &
                                                CBVAV(CBVAVNum)%HeatCoilName,ErrFlag)
         CBVAV(CBVAVNum)%HeatingCoilInletNode  =  HeatCoilInletNodeNum
         CBVAV(CBVAVNum)%CoilControlNode = GetSteamCoilSteamInletNode(CBVAV(CBVAVNum)%HeatCoilIndex, &
                                                CBVAV(CBVAVNum)%HeatCoilName, ErrFlag)
         CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate(CBVAV(CBVAVNum)%HeatCoilIndex,ErrFlag)
         SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
         SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'GetUnitaryHeatCool:VAVChangeoverBypass')
         IF(CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
           CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow = &
                              GetCoilMaxSteamFlowRate(CBVAV(CBVAVNum)%HeatCoilIndex,ErrFlag) * SteamDensity
         END IF
         HeatCoilOutletNodeNum = GetSteamCoilAirOutletNode(CBVAV(CBVAVNum)%HeatCoilIndex,CBVAV(CBVAVNum)%HeatCoilName,ErrFlag)
         CBVAV(CBVAVNum)%HeatingCoilOutletNode = HeatCoilOutletNodeNum
         IF(ErrFlag)THEN
           CALL ShowContinueError('...occurs in '//TRIM(CBVAV(CBVAVNum)%UnitType)// &
                                             ' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')
           ErrorsFound = .TRUE.
         END IF
       END IF
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(16))//' = '//TRIM(Alphas(16)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    IF(CBVAV(CBVAVNum)%DXCoilOutletNode .NE. CBVAV(CBVAVNum)%HeatingCoilInletNode)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal coil placement. Cooling coil must be upstream of heating coil.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      ErrorsFound = .TRUE.
    END IF

    IF(CBVAV(CBVAVNum)%FanPlace == BlowThru) THEN
      IF(CBVAV(CBVAVNum)%SplitterOutletAirNode .NE. CBVAV(CBVAVNum)%HeatingCoilOutletNode)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
        CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(SplitterOutletNodeName)//'.')
        CALL ShowContinueError(TRIM(cAlphaFields(6))//' must be the same as the outlet node specified'// &
                            ' in the heating coil object = '//TRIM(CBVAV(CBVAVNum)%HeatCoilType)//': '// &
                            TRIM(CBVAV(CBVAVNum)%HeatCoilName)//' when blow through '//TRIM(cAlphaFields(12))//' is selected.')
        ErrorsFound = .TRUE.
      END IF
      IF(CBVAV(CBVAVNum)%MixerMixedAirNode .NE. CBVAV(CBVAVNum)%FanInletNodeNum)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
        CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(11))//'. The fan inlet node name must be the same as the mixed'// &
                               ' air node specified in the '//TRIM(cAlphaFields(9))//' = '//TRIM(CBVAV(CBVAVNum)%OAMixName)// &
                               ' when blow through '//TRIM(cAlphaFields(12))//' is selected.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    IF(CBVAV(CBVAVNum)%FanPlace == DrawThru) THEN
      IF(CBVAV(CBVAVNum)%MixerMixedAirNode .NE. CBVAV(CBVAVNum)%DXCoilInletNode)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//': '//TRIM(CBVAV(CBVAVNum)%Name))
        CALL ShowContinueError('Illegal cooling coil placement. The cooling coil inlet node name must be the same as the mixed' &
                               //' air node specified in the '//TRIM(cAlphaFields(9))//' = '//TRIM(CBVAV(CBVAVNum)%OAMixName) &
                               //' when draw through '//TRIM(cAlphaFields(12))//' is selected.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    IF(SameString(Alphas(18),'CoolingPriority'))THEN
      CBVAV(CBVAVNum)%PriorityControl    = CoolingPriority
    ELSEIF(SameString(Alphas(18),'HeatingPriority'))THEN
      CBVAV(CBVAVNum)%PriorityControl    = HeatingPriority
    ELSEIF(SameString(Alphas(18),'ZonePriority'))THEN
      CBVAV(CBVAVNum)%PriorityControl    = ZonePriority
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(18))//' = '//TRIM(Alphas(18)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      CALL ShowContinueError('Valid choices are CoolingPriority, HeatingPriority, or ZonePriority.')
      ErrorsFound = .TRUE.
    END IF

    IF(Numbers(7) .GT. 0.0d0)THEN
      CBVAV(CBVAVNum)%MinLATCooling = Numbers(7)
    ELSE
      CBVAV(CBVAVNum)%MinLATCooling = 10.0d0
    END IF

    IF(Numbers(8) .GT. 0.0d0)THEN
      CBVAV(CBVAVNum)%MaxLATHeating = Numbers(8)
    ELSE
      CBVAV(CBVAVNum)%MaxLATHeating = 50.0d0
    END IF

    IF (CBVAV(CBVAVNum)%MinLATCooling .GT. CBVAV(CBVAVNum)%MaxLATHeating) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': illegal leaving air temperature specified.')
      CALL ShowContinueError('Resetting '//TRIM(cNumericFields(7))//' equal to '// &
                             TRIM(cNumericFields(8))//' and the simulation continues.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      CBVAV(CBVAVNum)%MinLATCooling = CBVAV(CBVAVNum)%MaxLATHeating
    END IF

    ! Dehumidification control mode
    IF (SameString(Alphas(19),'None')) THEN
      CBVAV(CBVAVNum)%DehumidControlType=DehumidControl_None
    ELSEIF (SameString(Alphas(19),' ')) THEN
      CBVAV(CBVAVNum)%DehumidControlType=DehumidControl_None
    ELSEIF (SameString(Alphas(19),'Multimode')) THEN
      IF (CBVAV(CBVAVNum)%DXCoolCoilType_Num == CoilDX_CoolingTwoStageWHumControl) THEN
          CBVAV(CBVAVNum)%DehumidControlType=DehumidControl_Multimode
      ELSE
        CALL ShowWarningError('Invalid '//TRIM(cAlphaFields(19))//' = '//TRIM(Alphas(19)))
        CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' "'//TRIM(CBVAV(CBVAVNum)%Name)//'".')
        CALL ShowContinueError('Valid only with '//TRIM(cAlphaFields(14))//' = Coil:Cooling:DX:TwoStageWithHumidityControlMode.')
        CALL ShowContinueError('Setting '//TRIM(cAlphaFields(19))//' to "None" and the simulation continues.')
        CBVAV(CBVAVNum)%DehumidControlType=DehumidControl_None
      END IF
    ELSEIF (SameString(Alphas(19),'CoolReheat')) THEN
      IF (CBVAV(CBVAVNum)%DXCoolCoilType_Num == CoilDX_CoolingTwoStageWHumControl) THEN
          CBVAV(CBVAVNum)%DehumidControlType=DehumidControl_CoolReheat
      ELSE
        CALL ShowWarningError('Invalid '//TRIM(cAlphaFields(19))//' = '//TRIM(Alphas(19)))
        CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' "'//TRIM(CBVAV(CBVAVNum)%Name)//'".')
        CALL ShowContinueError('Valid only with '//TRIM(cAlphaFields(14))//' = Coil:Cooling:DX:TwoStageWithHumidityControlMode.')
        CALL ShowContinueError('Setting '//TRIM(cAlphaFields(19))//' to "None" and the simulation continues.')
        CBVAV(CBVAVNum)%DehumidControlType=DehumidControl_None
      END IF
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFields(19))//' ='//TRIM(Alphas(19)))
      CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' "'//TRIM(CBVAV(CBVAVNum)%Name)//'".')
    END IF

    IF (CBVAV(CBVAVNum)%DXCoolCoilType_Num .GT. 0)THEN
      ControlNodeNum = GetOnlySingleNode(Alphas(7),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                                         NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsParent)
    END IF

!   Initialize last mode of compressor operation
    CBVAV(CBVAVNum)%LastMode = HeatingMode

    IF(CBVAV(CBVAVNum)%FanType_Num .NE. FanType_SimpleOnOff .AND. CBVAV(CBVAVNum)%FanType_Num .NE. FanType_SimpleConstVolume)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(10))//' in fan object = '// &
                            TRIM(CBVAV(CBVAVNum)%FanName))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
      CALL ShowContinueError(' The fan object type must be Fan:OnOff or Fan:ConstantVolume.')
      ErrorsFound = .TRUE.
    ELSE IF(CBVAV(CBVAVNum)%FanType_Num == FanType_SimpleOnOff .OR. CBVAV(CBVAVNum)%FanType_Num == FanType_SimpleConstVolume)THEN
      IF(CBVAV(CBVAVNum)%FanType_Num.EQ.FanType_SimpleOnOff .AND. &
           .NOT. SameString(CBVAV(CBVAVNum)%FanType,'Fan:OnOff'))THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' has '//TRIM(cAlphaFields(10))//' = '// &
                              TRIM(CBVAV(CBVAVNum)%FanType)//' which is inconsistent with the fan object.')
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        CALL ShowContinueError(' The fan object ('//TRIM(CBVAV(CBVAVNum)%FanName)// &
                               ') is actually a valid fan type and the simulation continues.')
        CALL ShowContinueError(' Node connections errors may result due to the inconsistent fan type.')
      END IF
      IF(CBVAV(CBVAVNum)%FanType_Num.EQ.FanType_SimpleConstVolume .AND. &
        .NOT. SameString(CBVAV(CBVAVNum)%FanType,'Fan:ConstantVolume'))THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' has '//TRIM(cAlphaFields(10))//' = '// &
                              TRIM(CBVAV(CBVAVNum)%FanType)//' which is inconsistent with fan object.')
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CBVAV(CBVAVNum)%Name))
        CALL ShowContinueError(' The fan object ('//TRIM(CBVAV(CBVAVNum)%FanName)// &
                               ') is actually a valid fan type and the simulation continues.')
        CALL ShowContinueError(' Node connections errors may result due to the inconsistent fan type.')
      END IF
    END IF

    ! Add fan to component sets array
    IF (CBVAV(CBVAVNum)%FanPlace == BlowThru) THEN
      CompSetFanInlet   = NodeID(CBVAV(CBVAVNum)%MixerMixedAirNode)
      CompSetFanOutlet  = NodeID(CBVAV(CBVAVNum)%DXCoilInletNode)
    ELSE
      CompSetFanInlet   = NodeID(CBVAV(CBVAVNum)%HeatingCoilOutletNode)
      CompSetFanOutlet  = SplitterOutletNodeName
    END IF
    CompSetCoolInlet  = NodeID(CBVAV(CBVAVNum)%DXCoilInletNode)
    CompSetCoolOutlet = NodeID(CBVAV(CBVAVNum)%DXCoilOutletNode)

    ! Add fan to component sets array
    CALL SetUpCompSets(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                       CBVAV(CBVAVNum)%FanType,CBVAV(CBVAVNum)%FanName,CompSetFanInlet,CompSetFanOutlet)

    ! Add cooling coil to component sets array
    CALL SetUpCompSets(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                       CBVAV(CBVAVNum)%DXCoolCoilType,CBVAV(CBVAVNum)%DXCoolCoilName,CompSetCoolInlet,CompSetCoolOutlet)

    ! Add heating coil to component sets array
    CALL SetUpCompSets(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                       CBVAV(CBVAVNum)%HeatCoilType,CBVAV(CBVAVNum)%HeatCoilName,NodeID(CBVAV(CBVAVNum)%HeatingCoilInletNode), &
                       NodeID(CBVAV(CBVAVNum)%HeatingCoilOutletNode))

    ! Set up component set for OA mixer - use OA node and Mixed air node
    CALL SetUpCompSets(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                       CBVAV(CBVAVNum)%OAMixType,CBVAV(CBVAVNum)%OAMixName,NodeID(CBVAV(CBVAVNum)%MixerOutsideAirNode), &
                       NodeID(CBVAV(CBVAVNum)%MixerMixedAirNode))

    CALL TestCompSet(CBVAV(CBVAVNum)%UnitType,CBVAV(CBVAVNum)%Name,NodeID(CBVAV(CBVAVNum)%AirInNode), &
                     NodeID(CBVAV(CBVAVNum)%AirOutNode),'Air Nodes')

!   Find air loop associated with CBVAV system
    DO AirLoopNum = 1, NumPrimaryAirSys
      DO BranchNum = 1, PrimaryAirSystem(AirLoopNum)%NumBranches
         DO CompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
           IF(.NOT. SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name, &
                   CBVAV(CBVAVNum)%Name) .OR. &
                  .NOT. SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                   CBVAV(CBVAVNum)%UnitType))CYCLE
                   CBVAV(CBVAVNum)%AirLoopNumber = AirLoopNum
!         Should EXIT here or do other checking?
              EXIT
         END DO
      END DO
    END DO

    IF(CBVAV(CBVAVNum)%AirLoopNumber .GT. 0)THEN
      CBVAV(CBVAVNum)%NumControlledZones = AirToZoneNodeInfo(CBVAV(CBVAVNum)%AirLoopNumber)%NumZonesCooled
      ALLOCATE (CBVAV(CBVAVNum)%ControlledZoneNum(CBVAV(CBVAVNum)%NumControlledZones))
      ALLOCATE (CBVAV(CBVAVNum)%ActualZoneNum(CBVAV(CBVAVNum)%NumControlledZones))
      ALLOCATE (CBVAV(CBVAVNum)%ActualZoneNodeNum(CBVAV(CBVAVNum)%NumControlledZones))
      ALLOCATE (CBVAV(CBVAVNum)%CBVAVBoxOutletNode(CBVAV(CBVAVNum)%NumControlledZones))
      ALLOCATE (CBVAV(CBVAVNum)%ZoneSequenceCoolingNum(CBVAV(CBVAVNum)%NumControlledZones))
      ALLOCATE (CBVAV(CBVAVNum)%ZoneSequenceHeatingNum(CBVAV(CBVAVNum)%NumControlledZones))

      CBVAV(CBVAVNum)%ControlledZoneNum = 0
      CBVAV(CBVAVNum)%ActualZoneNum     = 0
      DO AirLoopZoneNum = 1, AirToZoneNodeInfo(CBVAV(CBVAVNum)%AirLoopNumber)%NumZonesCooled
        CBVAV(CBVAVNum)%ControlledZoneNum(AirLoopZoneNum) = &
                                    AirToZoneNodeInfo(CBVAV(CBVAVNum)%AirLoopNumber)%CoolCtrlZoneNums(AirLoopZoneNum)
        IF(CBVAV(CBVAVNum)%ControlledZoneNum(AirLoopZoneNum) .GT. 0)THEN
          CBVAV(CBVAVNum)%ActualZoneNodeNum(AirLoopZoneNum) = &
                         ZoneEquipConfig(CBVAV(CBVAVNum)%ControlledZoneNum(AirLoopZoneNum))%ZoneNode
          CBVAV(CBVAVNum)%ActualZoneNum(AirLoopZoneNum) = &
                         ZoneEquipConfig(CBVAV(CBVAVNum)%ControlledZoneNum(AirLoopZoneNum))%ActualZoneNum
          CBVAV(CBVAVNum)%CBVAVBoxOutletNode(AirLoopZoneNum) = &
                         AirToZoneNodeInfo(CBVAV(CBVAVNum)%AirLoopNumber)%CoolZoneInletNodes(AirLoopZoneNum)
          ! check for thermostat in controlled zone
          FoundTstatZone = .FALSE.
          DO TstatZoneNum = 1, NumTempControlledZones
            IF(TempControlledZone(TstatZoneNum)%ActualZoneNum .NE. CBVAV(CBVAVNum)%ControlledZoneNum(AirLoopZoneNum))CYCLE
            FoundTstatZone = .TRUE.
          END DO
          IF(.NOT. FoundTstatZone)THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')
            CALL ShowContinueError('Thermostat not found in zone = ' &
                                   //TRIM(ZoneEquipConfig(CBVAV(CBVAVNum)%ControlledZoneNum(AirLoopZoneNum))%ZoneName)// &
                                   ' and the simulation continues.')
            CALL ShowContinueError('This zone will not be controlled to a temperature setpoint.')
          END IF
        ELSE
         CALL ShowSevereError('Controlled Zone node not found.')
         ErrorsFound = .TRUE.
        END IF
        IF (ZoneEquipConfig(CBVAV(CBVAVNum)%ActualZoneNum(AirLoopZoneNum))%EquipListIndex > 0) THEN
          DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(CBVAV(CBVAVNum)%ActualZoneNum(AirLoopZoneNum))%EquipListIndex) &
                                %NumOfEquipTypes
            IF ((ZoneEquipList(ZoneEquipConfig(CBVAV(CBVAVNum)%ActualZoneNum(AirLoopZoneNum))%EquipListIndex)% &
                                EquipType_Num(EquipNum) == AirDistUnit_Num) &
                .OR. (ZoneEquipList(ZoneEquipConfig(CBVAV(CBVAVNum)%ActualZoneNum(AirLoopZoneNum))%EquipListIndex)% &
                                EquipType_Num(EquipNum) == DirectAir_Num) ) THEN
              CBVAV(CBVAVNum)%ZoneSequenceCoolingNum(AirLoopZoneNum) = &
                         ZoneEquipList(ZoneEquipConfig(CBVAV(CBVAVNum)%ActualZoneNum(AirLoopZoneNum))% &
                                EquipListIndex)%CoolingPriority(EquipNum)
              CBVAV(CBVAVNum)%ZoneSequenceHeatingNum(AirLoopZoneNum) = &
                         ZoneEquipList(ZoneEquipConfig(CBVAV(CBVAVNum)%ActualZoneNum(AirLoopZoneNum))% &
                                EquipListIndex)%HeatingPriority(EquipNum)
            ENDIF
          ENDDO
        ENDIF

      END DO
    ELSE
    END IF

  END DO ! CBVAVIndex = 1,NumCBVAV

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetCBVAV: Errors found in getting '//TRIM(CurrentModuleObject)//' input.')
    CALL ShowContinueError('... Preceding condition causes termination.')
  END IF

  DO CBVAVNum=1,NumCBVAV
    ! Setup Report variables for the Fan Coils
    CALL SetupOutputVariable('Unitary System Total Heating Rate [W]',CBVAV(CBVAVNum)%TotHeatEnergyRate,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Total Heating Energy [J]',CBVAV(CBVAVNum)%TotHeatEnergy,&
                             'System','Sum',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Total Cooling Rate [W]',CBVAV(CBVAVNum)%TotCoolEnergyRate,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Total Cooling Energy [J]',CBVAV(CBVAVNum)%TotCoolEnergy,&
                             'System','Sum',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Sensible Heating Rate [W]',CBVAV(CBVAVNum)%SensHeatEnergyRate,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Sensible Heating Energy [J]',CBVAV(CBVAVNum)%SensHeatEnergy,&
                             'System','Sum',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Sensible Cooling Rate [W]',CBVAV(CBVAVNum)%SensCoolEnergyRate,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Sensible Cooling Energy [J]',CBVAV(CBVAVNum)%SensCoolEnergy,&
                             'System','Sum',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Latent Heating Rate [W]',CBVAV(CBVAVNum)%LatHeatEnergyRate,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Latent Heating Energy [J]',CBVAV(CBVAVNum)%LatHeatEnergy,&
                             'System','Sum',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Latent Cooling Rate [W]',CBVAV(CBVAVNum)%LatCoolEnergyRate,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Latent Cooling Energy [J]',CBVAV(CBVAVNum)%LatCoolEnergy,&
                             'System','Sum',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Electric Power [W]',CBVAV(CBVAVNum)%ElecPower,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Electric Energy [J]',CBVAV(CBVAVNum)%ElecConsumption,&
                             'System','Sum',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Fan Part Load Ratio []',CBVAV(CBVAVNum)%FanPartLoadRatio,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Compressor Part Load Ratio []',CBVAV(CBVAVNum)%CompPartLoadRatio,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Bypass Air Mass Flow Rate [kg/s]',CBVAV(CBVAVNum)%BypassMassFlowRate,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
    CALL SetupOutputVariable('Unitary System Air Outlet Setpoint Temperature [C]',CBVAV(CBVAVNum)%OutletTempSetpoint,&
                             'System','Average',CBVAV(CBVAVNum)%Name)
  END DO

  RETURN
END SUBROUTINE GetCBVAV

SUBROUTINE InitCBVAV(CBVAVNum, FirstHVACIteration, AirLoopNum, QZnReq, OnOffAirFlowRatio, HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       B. Griffith, May 2009, EMS setpoint check
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the changeover-bypass VAV system components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations. The CBVAV system is simulated with no load (coils off) to
          ! determine the outlet temperature. A setpoint temperature is calculated on FirstHVACIteration = TRUE.
          ! Once the setpoint is calculated, the inlet mass flow rate on FirstHVACIteration = FALSE is used to
          ! determine the bypass fraction. The simulation converges quickly on mass flow rate. If the zone
          ! temperatures float in the deadband, additional iterations are required to converge on mass flow rate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,                 ONLY: GetFanIndex, GetFanVolFlow
  USE General,              ONLY: TrimSigDigits
  USE DataSizing,           ONLY: AutoSize
  USE Psychrometrics,       ONLY: PsyRhoAirFnPbTdbW
  USE ScheduleManager,      ONLY: GetCurrentScheduleValue
  USE DataAirLoop,          ONLY: AirLoopControlInfo
  USE DataGlobals,          ONLY: AnyEnergyManagementSystemInModel
  USE EMSManager,           ONLY: CheckIfNodeSetpointManagedByEMS, iHumidityRatioMaxSetpoint
  USE SteamCoils,           ONLY: SimulateSteamCoilComponents, GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, &
                                  GetSteamCoilCapacity=>GetCoilCapacity
  USE WaterCoils,           ONLY: GetCoilMaxWaterFlowRate, SimulateWaterCoilComponents
  USE DataPlant,            ONLY: TypeOf_CoilSteamAirHeating, ScanPlantLoopsForObject, TypeOf_CoilWaterSimpleHeating, &
                                  PlantLoop
  USE FluidProperties,      ONLY: GetDensityGlycol, GetSatDensityRefrig
  USE PlantUtilities,       ONLY: SetComponentFlowRate, InitComponentNodes
  USE DataGlobals,          ONLY: InitConvTemp, AnyPlantInModel

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: CBVAVNum            ! Index of the current CBVAV unit being simulated
  LOGICAL, INTENT (IN)    :: FirstHVACIteration ! TRUE if first HVAC iteration
  INTEGER, INTENT (IN)    :: AirLoopNum         ! air loop index
  REAL(r64),    INTENT (INOUT) :: QZnReq             ! Heating/Cooling load for all zones
  REAL(r64),    INTENT (INOUT) :: OnOffAirFlowRatio  ! Ratio of compressor ON airflow to average airflow over timestep
  LOGICAL, INTENT (INOUT) :: HXUnitOn           ! flag to enable heat exchanger

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InNode                           ! Inlet node number in CBVAV loop
  INTEGER             :: OutNode                          ! Outlet node number in CBVAV loop
  INTEGER             :: MixerOutsideAirNode              ! Outside air node number in CBVAV loop
  REAL(r64)           :: RhoAir                           ! Air density at InNode
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.           ! Initialization flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! Used for initializations each begin environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeFlag  ! Used for sizing CBVAV inputs one time
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag  ! Used for initializations plant component for heating coils
  REAL(r64)           :: QSensUnitOut                     ! Output of CBVAV system with coils off
  REAL(r64)           :: OutsideAirMultiplier             ! Outside air multiplier schedule (= 1.0 if no schedule)
  LOGICAL                        :: FanErrFlag = .FALSE.  ! Error flag returned during CALL to GetFanType
  INTEGER                        :: FanIndex              ! Index to CBVAV's supply air fan
  CHARACTER(len=MaxNameLength)   :: CurrentModuleObject        ! Object type for error messages
  LOGICAL                        :: EMSSetpointCheck = .FALSE. ! local temporary
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: SteamIndex           ! steam coil index
  REAL(r64)                      :: FluidDensity         ! steam or water coil fluid density
  REAL(r64)                      :: CoilMaxVolFlowRate   ! water or steam max volumetric water flow rate
  REAL(r64)                      :: QCoilActual          ! actual CBVAV steam heating coil load met (W)
  LOGICAL                        :: ErrorFlag            ! local error flag returned from data mining
  REAL(r64)                      :: mdot                 ! heating coil fluid mass flow rate, kg/s

  InNode  = CBVAV(CBVAVNum)%AirInNode
  OutNode = CBVAV(CBVAVNum)%AirOutNode

! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumCBVAV))
    ALLOCATE(MySizeFlag(NumCBVAV))
    ALLOCATE(MyPlantScanFlag(NumCBVAV))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    MyPlantScanFlag = .TRUE.

    MyOneTimeFlag = .FALSE.

  END IF

  IF (MyPlantScanFlag(CBVAVNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF ( (CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingWater) .OR. &
         (CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingSteam) ) THEN
      IF (CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingWater) THEN

        ErrorFlag=.false.
        CALL ScanPlantLoopsForObject( CBVAV(CBVAVNum)%HeatCoilName, &
                                          TypeOf_CoilWaterSimpleHeating , &
                                          CBVAV(CBVAVNum)%LoopNum, &
                                          CBVAV(CBVAVNum)%LoopSide, &
                                          CBVAV(CBVAVNum)%BranchNum, &
                                          CBVAV(CBVAVNum)%CompNum,   &
                                          ErrFlag=ErrorFlag)
        IF (ErrorFlag) THEN
          CALL ShowFatalError('InitCBVAV: Program terminated for previous conditions.')
        ENDIF

        CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                 CBVAV(CBVAVNum)%HeatCoilName,ErrorsFound)

        IF(CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
          FluidDensity = GetDensityGlycol(PlantLoop(CBVAV(CBVAVNum)%LoopNum)%FluidName, &
                                InitConvTemp, &
                                PlantLoop(CBVAV(CBVAVNum)%LoopNum)%FluidIndex, &
                                'InitCBVAV')
          CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                 CBVAV(CBVAVNum)%HeatCoilName,ErrorsFound) * FluidDensity
        END IF

      ELSEIF (CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingSteam) THEN

        ErrorFlag=.false.
        CALL ScanPlantLoopsForObject( CBVAV(CBVAVNum)%HeatCoilName, &
                                      TypeOf_CoilSteamAirHeating , &
                                      CBVAV(CBVAVNum)%LoopNum, &
                                      CBVAV(CBVAVNum)%LoopSide, &
                                      CBVAV(CBVAVNum)%BranchNum, &
                                      CBVAV(CBVAVNum)%CompNum,   &
                                      ErrFlag=ErrorFlag)

        IF (ErrorFlag) THEN
          CALL ShowFatalError('InitCBVAV: Program terminated for previous conditions.')
        ENDIF

        CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow = GetCoilMaxSteamFlowRate(CBVAV(CBVAVNum)%HeatCoilIndex,ErrorsFound)

        IF(CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow .GT. 0.0d0)THEN
          SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
          FluidDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitCBVAV')
          CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow =   &
             GetCoilMaxSteamFlowRate(CBVAV(CBVAVNum)%HeatCoilIndex,ErrorsFound) * FluidDensity
        END IF

      ENDIF

         !fill outlet node for heating coil
      CBVAV(CBVAVNum)%CoilOutletNode =  &
            PlantLoop(CBVAV(CBVAVNum)%LoopNum)%LoopSide(CBVAV(CBVAVNum)%LoopSide) &
                      %Branch(CBVAV(CBVAVNum)%BranchNum)%Comp(CBVAV(CBVAVNum)%CompNum)%NodeNumOut
      MyPlantScanFlag(CBVAVNum) = .FALSE.

    ELSE ! CBVAV is not connected to plant
      MyPlantScanFlag(CBVAVNum) = .FALSE.
    ENDIF
  ELSEIF (MyPlantScanFlag(CBVAVNum) .AND. .NOT. AnyPlantInModel) THEN
    MyPlantScanFlag(CBVAVNum) = .FALSE.
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(CBVAVNum) ) THEN
    CALL SizeCBVAV(CBVAVNum)
    ! Pass the fan cycling schedule index up to the air loop. Set the air loop unitary system flag.
    AirLoopControlInfo(AirLoopNum)%CycFanSchedPtr = CBVAV(CBVAVNum)%FanOpModeSchedPtr
!   Set UnitarySys flag to FALSE and let the heating coil autosize independently of the cooling coil
    AirLoopControlInfo(AirLoopNum)%UnitarySys = .FALSE.
    AirLoopControlInfo(AirLoopNum)%FanOpMode = CBVAV(CBVAVNum)%OpMode
    MySizeFlag(CBVAVNum) = .FALSE.
  END IF

! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(CBVAVNum)) THEN
    MixerOutsideAirNode = CBVAV(CBVAVNum)%MixerOutsideAirNode
    RhoAir = StdRhoAir
    ! set the mass flow rates from the input volume flow rates
    CBVAV(CBVAVNum)%MaxCoolAirMassFlow = RhoAir*CBVAV(CBVAVNum)%MaxCoolAirVolFlow
    CBVAV(CBVAVNum)%CoolOutAirMassFlow = RhoAir*CBVAV(CBVAVNum)%CoolOutAirVolFlow
    CBVAV(CBVAVNum)%MaxHeatAirMassFlow = RhoAir*CBVAV(CBVAVNum)%MaxHeatAirVolFlow
    CBVAV(CBVAVNum)%HeatOutAirMassFlow = RhoAir*CBVAV(CBVAVNum)%HeatOutAirVolFlow
    CBVAV(CBVAVNum)%MaxNoCoolHeatAirMassFlow = RhoAir*CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow
    CBVAV(CBVAVNum)%NoCoolHeatOutAirMassFlow = RhoAir*CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow
    ! set the node max and min mass flow rates
    Node(MixerOutsideAirNode)%MassFlowRateMax = MAX(CBVAV(CBVAVNum)%CoolOutAirMassFlow,CBVAV(CBVAVNum)%HeatOutAirMassFlow)
    Node(MixerOutsideAirNode)%MassFlowRateMaxAvail = MAX(CBVAV(CBVAVNum)%CoolOutAirMassFlow,CBVAV(CBVAVNum)%HeatOutAirMassFlow)
    Node(MixerOutsideAirNode)%MassFlowRateMin = 0.0d0
    Node(MixerOutsideAirNode)%MassFlowRateMinAvail = 0.0d0
    Node(InNode)%MassFlowRateMax = MAX(CBVAV(CBVAVNum)%MaxCoolAirMassFlow,CBVAV(CBVAVNum)%MaxHeatAirMassFlow)
    Node(InNode)%MassFlowRateMaxAvail = MAX(CBVAV(CBVAVNum)%MaxCoolAirMassFlow,CBVAV(CBVAVNum)%MaxHeatAirMassFlow)
    Node(InNode)%MassFlowRateMin = 0.0d0
    Node(InNode)%MassFlowRateMinAvail = 0.0d0
    Node(OutNode)%Temp = Node(InNode)%Temp
    Node(OutNode)%HumRat = Node(InNode)%HumRat
    Node(OutNode)%Enthalpy = Node(InNode)%Enthalpy
    Node(CBVAV(CBVAVNum)%MixerReliefAirNode) = Node(MixerOutsideAirNode)
    MyEnvrnFlag(CBVAVNum) = .FALSE.
    CBVAV(CBVAVNum)%LastMode = HeatingMode
!   set fluid-side hardware limits
    IF(CBVAV(CBVAVNum)%CoilControlNode .GT. 0)THEN
       !    If water coil max water flow rate is autosized, simulate once in order to mine max water flow rate
      IF(CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow .EQ. Autosize)THEN
        IF (CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingWater) THEN
            CALL SimulateWaterCoilComponents(CBVAV(CBVAVNum)%HeatCoilName,FirstHVACIteration, &
                                             CBVAV(CBVAVNum)%HeatCoilIndex)
            ErrorFlag = .FALSE.
            CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                              CBVAV(CBVAVNum)%HeatCoilName,ErrorFlag)
            IF (ErrorFlag) Then
              ErrorsFound = .TRUE.
            ENDIF
            IF(CoilMaxVolFlowRate .NE. Autosize) THEN
              FluidDensity = GetDensityGlycol(PlantLoop(CBVAV(CBVAVNum)%LoopNum)%fluidName, &
                                     InitConvTemp, &
                                     PlantLoop(CBVAV(CBVAVNum)%LoopNum)%fluidIndex, &
                                     'InitCBVAV')
              CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity
            ENDIF
        ENDIF
        IF (CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingSteam) THEN
            CALL SimulateSteamCoilComponents(CBVAV(CBVAVNum)%HeatCoilName, &
                                             FirstHVACIteration,    &
                                             1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                             CBVAV(CBVAVNum)%HeatCoilIndex, QCoilActual)
            ErrorFlag = .FALSE.
            CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(CBVAV(CBVAVNum)%HeatCoilIndex,ErrorFlag)
            IF (ErrorFlag) Then
              ErrorsFound = .TRUE.
            ENDIF
            IF(CoilMaxVolFlowRate .NE. Autosize) THEN
              SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
              FluidDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitCBVAV')
              CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity
            ENDIF
        ENDIF
      ENDIF  ! end of IF(CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow .EQ. Autosize)THEN

      Call InitComponentNodes(0.d0, CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow, &
                                    CBVAV(CBVAVNum)%CoilControlNode, &
                                    CBVAV(CBVAVNum)%CoilOutletNode, &
                                    CBVAV(CBVAVNum)%LoopNum, &
                                    CBVAV(CBVAVNum)%LoopSide, &
                                    CBVAV(CBVAVNum)%BranchNum, &
                                    CBVAV(CBVAVNum)%CompNum )

    END IF  ! end of IF(CBVAV(CBVAVNum)%CoilControlNode .GT. 0)THEN
  END IF ! end one time inits

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(CBVAVNum) = .TRUE.
  END IF

! IF CBVAV system was not autosized and the fan is autosized, check that fan volumetric flow rate is greater than CBVAV flow rates
  IF(.NOT. DoingSizing .AND. CBVAV(CBVAVNum)%CheckFanFlow)THEN
    CurrentModuleObject = 'AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass'
    FanErrFlag = .FALSE.
    CALL GetFanIndex(CBVAV(CBVAVNum)%FanName,FanIndex,FanErrFlag)
    IF (FanErrFlag) THEN
      CALL ShowContinueError('...Fan occurs in '//TRIM(CurrentModuleObject)//  &
                ' ='//TRIM(CBVAV(CBVAVNum)%Name))
    ELSE
!     Only get the fan volumetric flow rate if the fan is valid, otherwise GetFanVolFlow returns a 0 and
!     the following warnings are written to the error file.
      CALL GetFanVolFlow(FanIndex,CBVAV(CBVAVNum)%FanVolFlow)
    ENDIF
    IF(CBVAV(CBVAVNum)%FanVolFlow .NE. AutoSize)THEN
!     Check fan versus system supply air flow rates
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%MaxCoolAirVolFlow)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                            //TRIM(TrimSigDigits(CBVAV(CBVAVNum)%FanVolFlow,7))//' in fan object ' &
                            //TRIM(CBVAV(CBVAVNum)%FanName)//' is less than the maximum CBVAV system air flow rate' &
                            //' when cooling is required ('//TRIM(TrimSigDigits(CBVAV(CBVAVNum)%MaxCoolAirVolFlow,7))//').')
        CALL ShowContinueError(' The CBVAV system flow rate when cooling is required is reset to the' &
                              //' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in Changeover-bypass VAV system = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%MaxCoolAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
      END IF
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%MaxHeatAirVolFlow)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                            //TRIM(TrimSigDigits(CBVAV(CBVAVNum)%FanVolFlow,7))//' in fan object ' &
                            //TRIM(CBVAV(CBVAVNum)%FanName)//' is less than the maximum CBVAV system air flow rate' &
                            //' when heating is required ('//TRIM(TrimSigDigits(CBVAV(CBVAVNum)%MaxHeatAirVolFlow,7))//').')
        CALL ShowContinueError(' The CBVAV system flow rate when heating is required is reset to the' &
                              //' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in Changeover-bypass VAV system = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%MaxHeatAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
      END IF
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .AND. &
         CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .NE. 0.0d0)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                            //TRIM(TrimSigDigits(CBVAV(CBVAVNum)%FanVolFlow,7))//' in fan object ' &
                            //TRIM(CBVAV(CBVAVNum)%FanName)//' is less than the maximum CBVAV system air flow rate when no ' &
                       //'heating or cooling is needed ('//TRIM(TrimSigDigits(CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow,7))//').')
        CALL ShowContinueError(' The CBVAV system flow rate when no heating or cooling is needed is reset to the' &
                              //' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in Changeover-bypass VAV system = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
      END IF
!     Check fan versus outdoor air flow rates
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%CoolOutAirVolFlow)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                            //TRIM(TrimSigDigits(CBVAV(CBVAVNum)%FanVolFlow,7))//' in fan object ' &
                            //TRIM(CBVAV(CBVAVNum)%FanName)//' is less than the maximum CBVAV outdoor air flow rate' &
                            //' when cooling is required ('//TRIM(TrimSigDigits(CBVAV(CBVAVNum)%CoolOutAirVolFlow,7))//').')
        CALL ShowContinueError(' The CBVAV outdoor flow rate when cooling is required is reset to the' &
                              //' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in Changeover-bypass VAV system = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%CoolOutAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
      END IF
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%HeatOutAirVolFlow)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                            //TRIM(TrimSigDigits(CBVAV(CBVAVNum)%FanVolFlow,7))//' in fan object ' &
                            //TRIM(CBVAV(CBVAVNum)%FanName)//' is less than the maximum CBVAV outdoor air flow rate' &
                            //' when heating is required ('//TRIM(TrimSigDigits(CBVAV(CBVAVNum)%HeatOutAirVolFlow,7))//').')
        CALL ShowContinueError(' The CBVAV outdoor flow rate when heating is required is reset to the' &
                              //' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in Changeover-bypass VAV system = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%HeatOutAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
      END IF
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' - air flow rate = ' &
                            //TRIM(TrimSigDigits(CBVAV(CBVAVNum)%FanVolFlow,7))//' in fan object ' &
                            //TRIM(CBVAV(CBVAVNum)%FanName)//' is less than the maximum CBVAV outdoor air flow rate when no ' &
                       //'heating or cooling is needed ('//TRIM(TrimSigDigits(CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow,7))//').')
        CALL ShowContinueError(' The CBVAV outdoor flow rate when no heating or cooling is needed is reset to the' &
                              //' fan flow rate and the simulation continues.')
        CALL ShowContinueError(' Occurs in Changeover-bypass VAV system = '//TRIM(CBVAV(CBVAVNum)%Name))
        CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
      END IF
      MixerOutsideAirNode = CBVAV(CBVAVNum)%MixerOutsideAirNode
      RhoAir = StdRhoAir
      ! set the mass flow rates from the reset volume flow rates
      CBVAV(CBVAVNum)%MaxCoolAirMassFlow = RhoAir*CBVAV(CBVAVNum)%MaxCoolAirVolFlow
      CBVAV(CBVAVNum)%CoolOutAirMassFlow = RhoAir*CBVAV(CBVAVNum)%CoolOutAirVolFlow
      CBVAV(CBVAVNum)%MaxHeatAirMassFlow = RhoAir*CBVAV(CBVAVNum)%MaxHeatAirVolFlow
      CBVAV(CBVAVNum)%HeatOutAirMassFlow = RhoAir*CBVAV(CBVAVNum)%HeatOutAirVolFlow
      CBVAV(CBVAVNum)%MaxNoCoolHeatAirMassFlow = RhoAir*CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow
      CBVAV(CBVAVNum)%NoCoolHeatOutAirMassFlow = RhoAir*CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow
      ! set the node max and min mass flow rates based on reset volume flow rates
      Node(MixerOutsideAirNode)%MassFlowRateMax = MAX(CBVAV(CBVAVNum)%CoolOutAirMassFlow,CBVAV(CBVAVNum)%HeatOutAirMassFlow)
      Node(MixerOutsideAirNode)%MassFlowRateMaxAvail = MAX(CBVAV(CBVAVNum)%CoolOutAirMassFlow,CBVAV(CBVAVNum)%HeatOutAirMassFlow)
      Node(MixerOutsideAirNode)%MassFlowRateMin = 0.0d0
      Node(MixerOutsideAirNode)%MassFlowRateMinAvail = 0.0d0
      Node(InNode)%MassFlowRateMax = MAX(CBVAV(CBVAVNum)%MaxCoolAirMassFlow,CBVAV(CBVAVNum)%MaxHeatAirMassFlow)
      Node(InNode)%MassFlowRateMaxAvail = MAX(CBVAV(CBVAVNum)%MaxCoolAirMassFlow,CBVAV(CBVAVNum)%MaxHeatAirMassFlow)
      Node(InNode)%MassFlowRateMin = 0.0d0
      Node(InNode)%MassFlowRateMinAvail = 0.0d0
      Node(OutNode)%Temp = Node(InNode)%Temp
      Node(OutNode)%HumRat = Node(InNode)%HumRat
      Node(OutNode)%Enthalpy = Node(InNode)%Enthalpy
      Node(CBVAV(CBVAVNum)%MixerReliefAirNode) = Node(MixerOutsideAirNode)
      CBVAV(CBVAVNum)%CheckFanFlow = .FALSE.
      IF(CBVAV(CBVAVNum)%FanVolFlow .GT. 0.0d0)THEN
        CBVAV(CBVAVNum)%HeatingSpeedRatio = CBVAV(CBVAVNum)%MaxHeatAirVolFlow/CBVAV(CBVAVNum)%FanVolFlow
        CBVAV(CBVAVNum)%CoolingSpeedRatio = CBVAV(CBVAVNum)%MaxCoolAirVolFlow/CBVAV(CBVAVNum)%FanVolFlow
        CBVAV(CBVAVNum)%NoHeatCoolSpeedRatio = &
                                        CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow/CBVAV(CBVAVNum)%FanVolFlow
      END IF
    END IF
  END IF

  IF(CBVAV(CBVAVNum)%FanOpModeSchedPtr .GT. 0)THEN
    IF(GetCurrentScheduleValue(CBVAV(CBVAVNum)%FanOpModeSchedPtr) .EQ. 0.0d0)THEN
      CBVAV(CBVAVNum)%OpMode = CycFanCycCoil
    ELSE
      CBVAV(CBVAVNum)%OpMode = ContFanCycCoil
    END IF
  END IF

! Returns load only for zones requesting cooling (heating). If in deadband, Qzoneload = 0.
  CALL GetZoneLoads(CBVAVNum, QZnReq)

  IF(CBVAV(CBVAVNum)%OutAirSchPtr .GT. 0)THEN
    OutsideAirMultiplier = GetCurrentScheduleValue(CBVAV(CBVAVNum)%OutAirSchPtr)
  ELSE
    OutsideAirMultiplier = 1.0d0
  END IF

  ! Set the inlet node mass flow rate
  IF (CBVAV(CBVAVNum)%OpMode .EQ. ContFanCycCoil) THEN
  ! constant fan mode
    IF (CBVAV(CBVAVNum)%HeatCoolMode == HeatingMode) THEN
      CompOnMassFlow = CBVAV(CBVAVNum)%MaxHeatAirMassFlow
      CompOnFlowRatio = CBVAV(CBVAVNum)%HeatingSpeedRatio
      OACompOnMassFlow   = CBVAV(CBVAVNum)%HeatOutAirMassFlow * OutsideAirMultiplier
      CBVAV(CBVAVNum)%LastMode = HeatingMode
    ELSE IF (CBVAV(CBVAVNum)%HeatCoolMode == CoolingMode) THEN
      CompOnMassFlow = CBVAV(CBVAVNum)%MaxCoolAirMassFlow
      CompOnFlowRatio = CBVAV(CBVAVNum)%CoolingSpeedRatio
      OACompOnMassFlow   = CBVAV(CBVAVNum)%CoolOutAirMassFlow * OutsideAirMultiplier
      CBVAV(CBVAVNum)%LastMode = CoolingMode
    ELSE
      CompOnMassFlow = CBVAV(CBVAVNum)%MaxNoCoolHeatAirMassFlow
      CompOnFlowRatio = CBVAV(CBVAVNum)%NoHeatCoolSpeedRatio
      OACompOnMassFlow   = CBVAV(CBVAVNum)%NoCoolHeatOutAirMassFlow * OutsideAirMultiplier
    END IF

    IF (CBVAV(CBVAVNum)%AirFlowControl .EQ. UseCompressorOnFlow) THEN
      IF (CBVAV(CBVAVNum)%LastMode .EQ. HeatingMode) THEN
        CompOffMassFlow = CBVAV(CBVAVNum)%MaxHeatAirMassFlow
        CompOffFlowRatio = CBVAV(CBVAVNum)%HeatingSpeedRatio
        OACompOffMassFlow   = CBVAV(CBVAVNum)%HeatOutAirMassFlow * OutsideAirMultiplier
      ELSE
        CompOffMassFlow = CBVAV(CBVAVNum)%MaxCoolAirMassFlow
        CompOffFlowRatio = CBVAV(CBVAVNum)%CoolingSpeedRatio
        OACompOffMassFlow   = CBVAV(CBVAVNum)%CoolOutAirMassFlow * OutsideAirMultiplier
      END IF
    ELSE
      CompOffMassFlow = CBVAV(CBVAVNum)%MaxNoCoolHeatAirMassFlow
      CompOffFlowRatio = CBVAV(CBVAVNum)%NoHeatCoolSpeedRatio
      OACompOffMassFlow   = CBVAV(CBVAVNum)%NoCoolHeatOutAirMassFlow * OutsideAirMultiplier
    END IF
  ELSE
  ! cycling fan mode
    IF (CBVAV(CBVAVNum)%HeatCoolMode == HeatingMode) THEN
      CompOnMassFlow = CBVAV(CBVAVNum)%MaxHeatAirMassFlow
      CompOnFlowRatio = CBVAV(CBVAVNum)%HeatingSpeedRatio
      OACompOnMassFlow   = CBVAV(CBVAVNum)%HeatOutAirMassFlow * OutsideAirMultiplier
    ELSE IF (CBVAV(CBVAVNum)%HeatCoolMode == CoolingMode) THEN
      CompOnMassFlow = CBVAV(CBVAVNum)%MaxCoolAirMassFlow
      CompOnFlowRatio = CBVAV(CBVAVNum)%CoolingSpeedRatio
      OACompOnMassFlow   = CBVAV(CBVAVNum)%CoolOutAirMassFlow * OutsideAirMultiplier
    ELSE
      CompOnMassFlow = CBVAV(CBVAVNum)%MaxCoolAirMassFlow
      CompOnFlowRatio = CBVAV(CBVAVNum)%CoolingSpeedRatio
      OACompOnMassFlow = CBVAV(CBVAVNum)%CoolOutAirMassFlow * OutsideAirMultiplier
    END IF
    CompOffMassFlow = 0.0d0
    CompOffFlowRatio = 0.0d0
    OACompOffMassFlow = 0.0d0
  END IF

! Check for correct control node at outlet of unit
  IF(CBVAV(CBVAVNum)%HumRatMaxCheck)THEN
    IF (CBVAV(CBVAVNum)%DehumidControlType .GT. 0)THEN
      IF(Node(OutNode)%HumRatMax .EQ. SensedNodeFlagValue)THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          CALL ShowWarningError('Unitary System:VAV:ChangeOverBypass = '//TRIM(CBVAV(CBVAVNum)%Name))
          CALL ShowContinueError('Use SetpointManager:SingleZone:Humidity:Maximum to place a humidity setpoint at' // &
                                 ' the air outlet node of the unitary system.')
          CALL ShowContinueError('Setting Dehumidification Control Type to None and simulation continues.')
          CBVAV(CBVAVNum)%DehumidControlType = 0
        ELSE
            ! need call to EMS to check node
          EMSSetpointCheck = .FALSE.
          CALL CheckIfNodeSetpointManagedByEMS(OutNode,iHumidityRatioMaxSetpoint, EMSSetpointCheck)
          IF (EMSSetpointCheck) THEN
            CALL ShowWarningError('Unitary System:VAV:ChangeOverBypass = '//TRIM(CBVAV(CBVAVNum)%Name))
            CALL ShowContinueError('Use SetpointManager:SingleZone:Humidity:Maximum to place a humidity setpoint at' // &
                                   ' the air outlet node of the unitary system.')
            CALL ShowContinueError('Or use an EMS Actuator to place a maximum humidity setpoint at' // &
                                   ' the air outlet node of the unitary system.')
            CALL ShowContinueError('Setting Dehumidification Control Type to None and simulation continues.')
            CBVAV(CBVAVNum)%DehumidControlType = 0
          ENDIF
        ENDIF
      END IF
      CBVAV(CBVAVNum)%HumRatMaxCheck = .FALSE.
    ELSE
      CBVAV(CBVAVNum)%HumRatMaxCheck = .FALSE.
    END IF
  END IF

  ! Set the inlet node mass flow rate
  IF (GetCurrentScheduleValue(CBVAV(CBVAVNum)%SchedPtr) .gt. 0.0d0 .AND. CompOnMassFlow .NE. 0.0d0) THEN
    OnOffAirFlowRatio = 1.0d0
    IF(FirstHVACIteration)THEN
      Node(CBVAV(CBVAVNum)%AirInNode)%MassFlowRate           = CompOnMassFlow
      Node(CBVAV(CBVAVNum)%MixerInletAirNode)%MassFlowRate   = CompOnMassFlow
      Node(CBVAV(CBVAVNum)%MixerOutsideAirNode)%MassFlowRate = OACompOnMassFlow
      Node(CBVAV(CBVAVNum)%MixerReliefAirNode)%MassFlowRate  = OACompOnMassFlow
      ByPassDuctFlowFraction = 0.0d0
      PartLoadFrac           = 0.0d0
    ELSE
      IF (CBVAV(CBVAVNum)%HeatCoolMode /= 0) THEN
        PartLoadFrac = 1.0d0
      ELSE
        PartLoadFrac = 0.0d0
      END IF
      IF(CBVAV(CBVAVNum)%OpMode .EQ. CycFanCycCoil) THEN
        ByPassDuctFlowFraction = 0.0d0
      ELSE
        ByPassDuctFlowFraction = MAX(0.0d0,1.0d0 - (Node(CBVAV(CBVAVNum)%AirInNode)%MassFlowRate / CompOnMassFlow ))
      END IF
    END IF
  ELSE
    PartLoadFrac = 0.0d0
    Node(CBVAV(CBVAVNum)%AirInNode)%MassFlowRate           = 0.0d0
    Node(CBVAV(CBVAVNum)%AirOutNode)%MassFlowRate          = 0.0d0
    Node(CBVAV(CBVAVNum)%AirOutNode)%MassFlowRateMaxAvail  = 0.0d0

    Node(CBVAV(CBVAVNum)%MixerInletAirNode)%MassFlowRate   = 0.0d0
    Node(CBVAV(CBVAVNum)%MixerOutsideAirNode)%MassFlowRate = 0.0d0
    Node(CBVAV(CBVAVNum)%MixerReliefAirNode)%MassFlowRate  = 0.0d0

    OnOffAirFlowRatio      = 1.0d0
    ByPassDuctFlowFraction = 0.0d0

  END IF

  CALL CalcCBVAV(CBVAVNum,FirstHVACIteration,PartLoadFrac,QSensUnitOut,QZnReq,OnOffAirFlowRatio, HXUnitOn)

! If unit is scheduled OFF, setpoint is equal to inlet node temperature.
  IF (GetCurrentScheduleValue(CBVAV(CBVAVNum)%SchedPtr) .EQ. 0.0d0) THEN
    CBVAV(CBVAVNum)%OutletTempSetpoint = Node(InNode)%Temp
    RETURN
  END IF

  CALL SetAverageAirFlow(CBVAVNum, OnOffAirFlowRatio, FirstHVACIteration)

  IF(FirstHVACIteration)CBVAV(CBVAVNum)%OutletTempSetpoint = CalcSetpointTempTarget(CBVAVNum)

! The setpoint is used to control the DX coils at their respective outlet nodes (not the unit outlet), correct
! for fan heat for draw thru units only (fan heat is included at the outlet of each coil when blowthru is used)
  CBVAV(CBVAVNum)%CoilTempSetpoint = CBVAV(CBVAVNum)%OutletTempSetpoint
  IF(CBVAV(CBVAVNum)%FanPlace == DrawThru)THEN
    CBVAV(CBVAVNum)%CoilTempSetpoint = CBVAV(CBVAVNum)%CoilTempSetpoint - &
                 (Node(CBVAV(CBVAVNum)%AirOutNode)%Temp-Node(CBVAV(CBVAVNum)%FanInletNodeNum)%Temp)
  END IF

  IF(FirstHVACIteration) THEN
    IF (CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingWater) THEN
            CALL SimulateWaterCoilComponents(CBVAV(CBVAVNum)%HeatCoilName,FirstHVACIteration, &
                                             CBVAV(CBVAVNum)%HeatCoilIndex)

      !     set air-side and steam-side mass flow rates
      Node(CBVAV(CBVAVNum)%HeatingCoilInletNode)%MassFlowRate = CompOnMassFlow
      mdot = CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow
      Call SetComponentFlowRate(mdot, &
                                CBVAV(CBVAVNum)%CoilControlNode, &
                                CBVAV(CBVAVNum)%CoilOutletNode, &
                                CBVAV(CBVAVNum)%LoopNum, &
                                CBVAV(CBVAVNum)%LoopSide, &
                                CBVAV(CBVAVNum)%BranchNum, &
                                CBVAV(CBVAVNum)%CompNum )

      !     simulate water coil to find operating capacity
      CALL SimulateWaterCoilComponents(CBVAV(CBVAVNum)%HeatCoilName,FirstHVACIteration, &
                                       CBVAV(CBVAVNum)%HeatCoilIndex, QCoilActual)
      CBVAV(CBVAVNum)%DesignSuppHeatingCapacity = QCoilActual

    END IF ! from IF(MSHeatPump(MSHeatPumpNum)%SuppHeatCoilType == Coil_HeatingWater) THEN

    IF(CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingSteam) THEN

      !     set air-side and steam-side mass flow rates
      Node(CBVAV(CBVAVNum)%HeatingCoilInletNode)%MassFlowRate = CompOnMassFlow
      mdot = CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow
      Call SetComponentFlowRate(mdot, &
                                CBVAV(CBVAVNum)%CoilControlNode, &
                                CBVAV(CBVAVNum)%CoilOutletNode, &
                                CBVAV(CBVAVNum)%LoopNum, &
                                CBVAV(CBVAVNum)%LoopSide, &
                                CBVAV(CBVAVNum)%BranchNum, &
                                CBVAV(CBVAVNum)%CompNum )

!     simulate steam coil to find operating capacity
      CALL SimulateSteamCoilComponents(CBVAV(CBVAVNum)%HeatCoilName, &
                                       FirstHVACIteration,    &
                                       1.0d0, & !QCoilReq, simulate any load > 0 to get max capacity of steam coil
                                       CBVAV(CBVAVNum)%HeatCoilIndex, QCoilActual)
      CBVAV(CBVAVNum)%DesignSuppHeatingCapacity = QCoilActual

    END IF ! from IF(CBVAV(CBVAVNum)%HeatCoilType_Num == Coil_HeatingSteam) THEN
  END IF ! from IF( FirstHVACIteration ) THEN

  IF(CBVAV(CBVAVNum)%HeatCoolMode == 0 .AND. CBVAV(CBVAVNum)%OpMode == CycFanCycCoil .OR. CompOnMassFlow .EQ. 0.0d0)THEN
    QZnReq = 0.0d0
    PartLoadFrac = 0.0d0
    Node(CBVAV(CBVAVNum)%AirInNode)%MassFlowRate           = 0.0d0
    Node(CBVAV(CBVAVNum)%AirOutNode)%MassFlowRateMaxAvail  = 0.0d0
    Node(CBVAV(CBVAVNum)%MixerInletAirNode)%MassFlowRate   = 0.0d0
    Node(CBVAV(CBVAVNum)%MixerOutsideAirNode)%MassFlowRate = 0.0d0
    Node(CBVAV(CBVAVNum)%MixerReliefAirNode)%MassFlowRate  = 0.0d0
  END IF

  RETURN
END SUBROUTINE InitCBVAV

SUBROUTINE SizeCBVAV(CBVAVNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing changeover-bypass VAV components.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CBVAVNum       ! Index to CBVAV system

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (CBVAV(CBVAVNum)%MaxCoolAirVolFlow == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name)
      CBVAV(CBVAVNum)%MaxCoolAirVolFlow   = FinalSysSizing(CurSysNum)%DesMainVolFlow
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%MaxCoolAirVolFlow .AND. CBVAV(CBVAVNum)%FanVolFlow .NE. AutoSize)THEN
        CBVAV(CBVAVNum)%MaxCoolAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
        CALL ShowWarningError(TRIM(CBVAV(CBVAVNum)%UnitType)//' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')
        CALL ShowContinueError('The CBVAV system supply air fan air flow rate is less than the autosized value' &
                             //' for the maximum air flow rate in cooling mode. Consider autosizing the fan for' &
                             //' this simulation.')
        CALL ShowContinueError('The maximum air flow rate in cooling mode ' &
                             //'is reset to the supply air fan flow rate and the simulation continues.')
      END IF
      IF (CBVAV(CBVAVNum)%MaxCoolAirVolFlow < SmallAirVolFlow) THEN
        CBVAV(CBVAVNum)%MaxCoolAirVolFlow = 0.0d0
      END IF
      CALL ReportSizingOutput(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                              'maximum cooling air flow rate [m3/s]', CBVAV(CBVAVNum)%MaxCoolAirVolFlow)

    END IF

  END IF

  IF (CBVAV(CBVAVNum)%MaxHeatAirVolFlow == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name)
      CBVAV(CBVAVNum)%MaxHeatAirVolFlow   = FinalSysSizing(CurSysNum)%DesMainVolFlow
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%MaxHeatAirVolFlow .AND. CBVAV(CBVAVNum)%FanVolFlow .NE. AutoSize)THEN
        CBVAV(CBVAVNum)%MaxHeatAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
        CALL ShowWarningError(TRIM(CBVAV(CBVAVNum)%UnitType)//' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')
        CALL ShowContinueError('The CBVAV system supply air fan air flow rate is less than the autosized value' &
                             //' for the maximum air flow rate in heating mode. Consider autosizing the fan for' &
                             //' this simulation.')
        CALL ShowContinueError('The maximum air flow rate in heating mode ' &
                             //'is reset to the supply air fan flow rate and the simulation continues.')
      END IF
      IF (CBVAV(CBVAVNum)%MaxHeatAirVolFlow < SmallAirVolFlow) THEN
        CBVAV(CBVAVNum)%MaxHeatAirVolFlow = 0.0d0
      END IF
      CALL ReportSizingOutput(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                              'maximum heating air flow rate [m3/s]', CBVAV(CBVAVNum)%MaxHeatAirVolFlow)

    END IF

  END IF

  IF (CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name)
      CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow .AND. &
         CBVAV(CBVAVNum)%FanVolFlow .NE. AutoSize)THEN
        CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
        CALL ShowWarningError(TRIM(CBVAV(CBVAVNum)%UnitType)//' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')
        CALL ShowContinueError('The CBVAV system supply air fan air flow rate is less than the autosized value' &
                             //' for the maximum air flow rate when no heating or cooling is needed. Consider' &
                             //' autosizing the fan for this simulation.')
        CALL ShowContinueError('The maximum air flow rate when no heating or cooling is needed ' &
                             //'is reset to the supply air fan flow rate and the simulation continues.')
      END IF
      IF (CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow < SmallAirVolFlow) THEN
        CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow = 0.0d0
      END IF

      CALL ReportSizingOutput(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                              'maximum air flow rate when compressor/coil is off [m3/s]', CBVAV(CBVAVNum)%MaxNoCoolHeatAirVolFlow)

    END IF

  END IF

  IF (CBVAV(CBVAVNum)%CoolOutAirVolFlow == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name)
      CBVAV(CBVAVNum)%CoolOutAirVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%CoolOutAirVolFlow .AND. CBVAV(CBVAVNum)%FanVolFlow .NE. AutoSize)THEN
        CBVAV(CBVAVNum)%CoolOutAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
        CALL ShowWarningError(TRIM(CBVAV(CBVAVNum)%UnitType)//' "'//TRIM(CBVAV(CBVAVNum)%Name)//'"')
        CALL ShowContinueError('The CBVAV system supply air fan air flow rate is less than the autosized value' &
                             //' for the outdoor air flow rate in cooling mode. Consider autosizing the fan for' &
                             //' this simulation.')
        CALL ShowContinueError('The outdoor air flow rate in cooling mode ' &
                             //'is reset to the supply air fan flow rate and the simulation continues.')
      END IF
      IF (CBVAV(CBVAVNum)%CoolOutAirVolFlow < SmallAirVolFlow) THEN
        CBVAV(CBVAVNum)%CoolOutAirVolFlow = 0.0d0
      END IF
      CALL ReportSizingOutput(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                              'maximum outside air flow rate in cooling [m3/s]',CBVAV(CBVAVNum)%CoolOutAirVolFlow)

    END IF

  END IF

  IF (CBVAV(CBVAVNum)%HeatOutAirVolFlow == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name)
      CBVAV(CBVAVNum)%HeatOutAirVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%HeatOutAirVolFlow .AND. CBVAV(CBVAVNum)%FanVolFlow .NE. AutoSize)THEN
        CBVAV(CBVAVNum)%HeatOutAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
        CALL ShowContinueError('The CBVAV system supply air fan air flow rate is less than the autosized value' &
                             //' for the outdoor air flow rate in heating mode. Consider autosizing the fan for' &
                             //' this simulation.')
        CALL ShowContinueError('The outdoor air flow rate in heating mode ' &
                             //'is reset to the supply air fan flow rate and the simulation continues.')
      END IF
      IF (CBVAV(CBVAVNum)%HeatOutAirVolFlow < SmallAirVolFlow) THEN
        CBVAV(CBVAVNum)%HeatOutAirVolFlow = 0.0d0
      END IF
      CALL ReportSizingOutput(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                              'maximum outdoor air flow rate in heating [m3/s]',CBVAV(CBVAVNum)%CoolOutAirVolFlow)

    END IF

  END IF

  IF (CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name)
      CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
      IF(CBVAV(CBVAVNum)%FanVolFlow .LT. CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow .AND. &
         CBVAV(CBVAVNum)%FanVolFlow .NE. AutoSize)THEN
        CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow = CBVAV(CBVAVNum)%FanVolFlow
        CALL ShowContinueError('The CBVAV system supply air fan air flow rate is less than the autosized value' &
                             //' for the outdoor air flow rate when no heating or cooling is needed. Consider' &
                             //' autosizing the fan for this simulation.')
        CALL ShowContinueError('The outdoor air flow rate when no heating or cooling is needed ' &
                             //'is reset to the supply air fan flow rate and the simulation continues.')
      END IF
      IF (CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow < SmallAirVolFlow) THEN
        CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow = 0.0d0
      END IF
      CALL ReportSizingOutput(CBVAV(CBVAVNum)%UnitType, CBVAV(CBVAVNum)%Name, &
                            'maximum outdoor air flow rate when compressor is off [m3/s]',CBVAV(CBVAVNum)%NoCoolHeatOutAirVolFlow)

    END IF

  END IF

  RETURN
END SUBROUTINE SizeCBVAV

SUBROUTINE ControlCBVAVOutput(CBVAVNum,FirstHVACIteration,QZnReq,PartLoadFrac,OnOffAirFlowRatio, HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determine the part load fraction of the CBVAV system for this time step.

          ! METHODOLOGY EMPLOYED:
          ! Use RegulaFalsi technique to iterate on part-load ratio until convergence is achieved.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals,           ONLY: SmallTempDiff
  USE ScheduleManager,           ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT   (IN)  :: CBVAVNum           ! Index to CBVAV system
  LOGICAL, INTENT   (IN)  :: FirstHVACIteration ! Flag for 1st HVAC iteration
  REAL(r64)   , INTENT (INOUT) :: QZnReq             ! Cooling or heating output needed by zone [W]
  REAL(r64)   , INTENT   (OUT) :: PartLoadFrac       ! Unit part load fraction
  REAL(r64)   , INTENT (INOUT) :: OnOffAirFlowRatio  ! Ratio of compressor ON airflow to AVERAGE airflow over timestep
  LOGICAL, INTENT (INOUT) :: HXUnitOn           ! flag to enable heat exchanger

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !
  INTEGER, PARAMETER :: MaxIter  = 50           ! Maximum number of iterations

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)          :: FullOutput              ! Unit full output when compressor is operating [W]

  PartLoadFrac  = 0.0d0

  IF (GetCurrentScheduleValue(CBVAV(CBVAVNum)%SchedPtr) .EQ. 0.0d0) RETURN

  ! Get operating result
  PartLoadFrac  = 1.0d0
  CALL CalcCBVAV(CBVAVNum, FirstHVACIteration, PartLoadFrac, FullOutput, QZnReq, OnOffAirFlowRatio, HXUnitOn)

  IF((Node(CBVAV(CBVAVNum)%AirOutNode)%Temp - CBVAV(CBVAVNum)%OutletTempSetpoint) .GT. SmallTempDiff .AND. &
      CBVAV(CBVAVNum)%HeatCoolMode .GT. 0 .AND. PartLoadFrac .LT. 1.0d0)THEN
    CALL CalcCBVAV(CBVAVNum, FirstHVACIteration, PartLoadFrac, FullOutput, QZnReq, OnOffAirFlowRatio, HXUnitOn)
  END IF

  RETURN

END SUBROUTINE ControlCBVAVOutput

SUBROUTINE CalcCBVAV(CBVAVNum,FirstHVACIteration,PartLoadFrac,LoadMet,QZnReq,OnOffAirFlowRatio, HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate the components making up the changeover-bypass VAV system.

          ! METHODOLOGY EMPLOYED:
          ! Simulates the unit components sequentially in the air flow direction.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,                      ONLY: SimulateFanComponents
  USE DXCoils,                   ONLY: SimDXCoil, SimDXCoilMultiMode
  USE General,                   ONLY: SolveRegulaFalsi, RoundSigDigits
  USE MixedAir,                  ONLY: SimOAMixer
  USE DataHVACGlobals,           ONLY: SmallTempDiff
  USE Psychrometrics,            ONLY: PsyHFnTdbW, PsyCpAirFnWTdb, PsyTdpFnWPb
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT    (IN) :: CBVAVNum              ! Unit index in fan coil array
  LOGICAL, INTENT    (IN) :: FirstHVACIteration   ! Flag for 1st HVAC iteration
  REAL(r64)   , INTENT (INOUT) :: PartLoadFrac         ! Compressor part load fraction
  REAL(r64)   , INTENT   (OUT) :: LoadMet              ! Load met by unit (W)
  REAL(r64)   , INTENT (INOUT) :: QZnReq               ! Zone load (W)
  REAL(r64)   , INTENT (INOUT) :: OnOffAirFlowRatio    ! Ratio of compressor ON airflow to AVERAGE airflow over timestep
  LOGICAL, INTENT (IN)    :: HXUnitOn           ! flag to enable heat exchanger

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER  ::   MaxIte = 500           ! Maximum number of iterations

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: OutletNode           ! CBVAV air outlet node
  INTEGER      :: InletNode            ! CBVAV air inlet node
  REAL(r64)    :: MinHumRat            ! Minimum humidity ratio for sensible capacity calculation (kg/kg)
  REAL(r64)    :: Par(6)               ! RegulaFalsi parameters
  INTEGER      :: SolFla               ! Flag of RegulaFalsi solver
  REAL(r64)    :: QHeater              ! Load to be met by heater [W]
  REAL(r64)    :: QHeaterActual        ! actual heating load met [W]
  REAL(r64)    :: CpAir                ! Specific heat of air [J/kg-K]
  INTEGER      :: MixerOutsideAirNode  ! Outside air node number in OA mixer
  INTEGER      :: MixerReliefAirNode   ! Relief air node number in OA mixer
  INTEGER      :: DehumidMode          ! Dehumidification mode (0=normal, 1=enhanced)
  REAL(r64)    :: ApproachTemp
  REAL(r64)    :: DesiredDewPoint
  REAL(r64)    :: OutdoorDryBulbTemp   ! Dry-bulb temperature at outdoor condenser
  REAL(r64)    :: OutdoorBaroPress     ! Barometric pressure at outdoor condenser
          ! FLOW

  OutletNode = CBVAV(CBVAVNum)%AirOutNode
  InletNode  = CBVAV(CBVAVNum)%AirInNode
  MixerOutsideAirNode = CBVAV(CBVAVNum)%MixerOutsideAirNode
  MixerReliefAirNode  = CBVAV(CBVAVNum)%MixerReliefAirNode
  IF (CBVAV(CBVAVNum)%CondenserNodeNum > 0) THEN
    OutdoorDryBulbTemp = Node(CBVAV(CBVAVNum)%CondenserNodeNum)%Temp
    OutdoorBaroPress   = Node(CBVAV(CBVAVNum)%CondenserNodeNum)%Press
  ELSE
    OutdoorDryBulbTemp = OutDryBulbTemp
    OutdoorBaroPress   = OutBaroPress
  ENDIF

  SaveCompressorPLR = 0.0d0

! Bypass excess system air through bypass duct and calculate new mixed air conditions at OA mixer inlet node
  Node(CBVAV(CBVAVNum)%MixerInletAirNode)%Temp = (1.0d0 - ByPassDuctFlowFraction) * Node(InletNode)%Temp + &
                                          ByPassDuctFlowFraction * Node(OutletNode)%Temp
  Node(CBVAV(CBVAVNum)%MixerInletAirNode)%HumRat = (1.0d0 - ByPassDuctFlowFraction) * Node(InletNode)%HumRat + &
                                          ByPassDuctFlowFraction * Node(OutletNode)%HumRat
  Node(CBVAV(CBVAVNum)%MixerInletAirNode)%Enthalpy = PsyHFnTdbW(Node(CBVAV(CBVAVNum)%MixerInletAirNode)%Temp, &
                                                               Node(CBVAV(CBVAVNum)%MixerInletAirNode)%HumRat)
  CALL SimOAMixer(CBVAV(CBVAVNum)%OAMixName,FirstHVACIteration,CBVAV(CBVAVNum)%OAMixIndex)

  IF (CBVAV(CBVAVNum)%FanPlace .EQ. BlowThru)&
       CALL SimulateFanComponents(CBVAV(CBVAVNum)%FanName,FirstHVACIteration,CBVAV(CBVAVNum)%FanIndex,FanSpeedRatio)

! Simulate cooling coil if zone load is negative (cooling load)
  IF (CBVAV(CBVAVNum)%HeatCoolMode == CoolingMode) THEN
    IF(OutdoorDryBulbTemp .GE. CBVAV(CBVAVNum)%MinOATCompressor)THEN

      SELECT CASE(CBVAV(CBVAVNum)%DXCoolCoilType_Num)

      CASE(CoilDX_CoolingHXAssisted)
        CALL SimHXAssistedCoolingCoil(CBVAV(CBVAVNum)%DXCoolCoilName,FirstHVACIteration,On,PartLoadFrac, &
                                      CBVAV(CBVAVNum)%CoolCoilCompIndex, ContFanCycCoil, HXUnitEnable=HXUnitOn)
        IF(Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp .LE. CBVAV(CBVAVNum)%CoilTempSetpoint)THEN
!         If coil inlet temp is already below the setpoint, simulated with coil off
          PartLoadFrac = 0.0d0
          CALL SimHXAssistedCoolingCoil(CBVAV(CBVAVNum)%DXCoolCoilName,FirstHVACIteration,Off,PartLoadFrac, &
                                        CBVAV(CBVAVNum)%CoolCoilCompIndex, ContFanCycCoil, HXUnitEnable=HXUnitOn)
        ELSE IF(Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp .LT. CBVAV(CBVAVNum)%CoilTempSetpoint .AND. &
                Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp .GT. CBVAV(CBVAVNum)%CoilTempSetpoint)THEN
          Par(1) = REAL(CBVAV(CBVAVNum)%CoolCoilCompIndex,r64)
          Par(2) = CBVAV(CBVAVNum)%CoilTempSetpoint
          Par(3) = OnOffAirFlowRatio
          Par(4) = REAL(CBVAVNum,r64)
          IF(FirstHVACIteration)THEN
            Par(5) = 1.0d0
          ELSE
            Par(5) = 0.0d0
          END IF
          IF(HXUnitOn)THEN
            Par(6) = 1.0d0
          ELSE
            Par(6) = 0.0d0
          END IF
          CALL SolveRegulaFalsi(SmallTempDiff, MaxIte, SolFla, PartLoadFrac, HXAssistDXCoilResidual, 0.0d0, 1.0d0, Par)
          CALL SimHXAssistedCoolingCoil(CBVAV(CBVAVNum)%DXCoolCoilName,FirstHVACIteration,On,PartLoadFrac, &
                                    CBVAV(CBVAVNum)%CoolCoilCompIndex, ContFanCycCoil, HXUnitEnable=HXUnitOn)
          IF (SolFla == -1 .AND. .NOT. WarmupFlag) THEN
            IF(CBVAV(CBVAVNum)%HXDXIterationExceeded .LT. 1)THEN
              CBVAV(CBVAVNum)%HXDXIterationExceeded = CBVAV(CBVAVNum)%HXDXIterationExceeded + 1
              CALL ShowWarningError('Iteration limit exceeded calculating HX assisted DX unit part-load ratio, for unit = '// &
                              TRIM(CBVAV(CBVAVNum)%DXCoolCoilName))
              CALL ShowContinueError('Calculated part-load ratio = '//TRIM(RoundSigDigits(PartLoadFrac,3)))
              CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation continues.'// &
                                            ' Occurrence info: ')
            ELSE
              CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                        ', Iteration limit exceeded for HX assisted DX unit part-load ratio error continues.', &
                                           CBVAV(CBVAVNum)%HXDXIterationExceededIndex,PartLoadFrac,PartLoadFrac)
            END IF
          ELSE IF (SolFla == -2 .AND. .NOT. WarmupFlag) THEN
            PartLoadFrac = MAX(0.0d0,MIN(1.0d0,(Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp - CBVAV(CBVAVNum)%CoilTempSetpoint) / &
                           (Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp - Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp)))
            IF(CBVAV(CBVAVNum)%HXDXIterationFailed .LT. 1)THEN
              CBVAV(CBVAVNum)%HXDXIterationFailed = CBVAV(CBVAVNum)%HXDXIterationFailed + 1
              CALL ShowSevereError('HX assisted DX unit part-load ratio calculation failed: part-load ratio limits exceeded, ' &
                               //'for unit = '//TRIM(CBVAV(CBVAVNum)%DXCoolCoilName))
              CALL ShowContinueErrorTimeStamp('An estimated part-load ratio of '//TRIM(RoundSigDigits(PartLoadFrac,3))// &
                                            'will be used and the simulation continues. Occurrence info: ')
            ELSE
              CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                        ', Part-load ratio calculation failed for HX assisted DX unit error continues.', &
                                           CBVAV(CBVAVNum)%HXDXIterationFailedIndex,PartLoadFrac,PartLoadFrac)
            END IF
          END IF
        END IF
      CASE(CoilDX_CoolingSingleSpeed)
        CALL SimDXCoil(CBVAV(CBVAVNum)%DXCoolCoilName,On,FirstHVACIteration,PartLoadFrac,CBVAV(CBVAVNum)%CoolCoilCompIndex,  &
           ContFanCycCoil, OnOffAirFlowRatio)
        IF(Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp .LT. CBVAV(CBVAVNum)%CoilTempSetpoint)THEN
!         If coil inlet temp is already below the setpoint, simulated with coil off
          PartLoadFrac = 0.0d0
          CALL SimDXCoil(CBVAV(CBVAVNum)%DXCoolCoilName,On,FirstHVACIteration,PartLoadFrac,CBVAV(CBVAVNum)%CoolCoilCompIndex,  &
             ContFanCycCoil, OnOffAirFlowRatio)
        ELSE IF(Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp .LT. CBVAV(CBVAVNum)%CoilTempSetpoint .AND. &
                Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp .GT. CBVAV(CBVAVNum)%CoilTempSetpoint)THEN
          Par(1) = REAL(CBVAV(CBVAVNum)%CoolCoilCompIndex,r64)
          Par(2) = CBVAV(CBVAVNum)%CoilTempSetpoint
          Par(3) = OnOffAirFlowRatio
          CALL SolveRegulaFalsi(SmallTempDiff, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilResidual, 0.0d0, 1.0d0, Par)
          CALL SimDXCoil(CBVAV(CBVAVNum)%DXCoolCoilName,On,FirstHVACIteration,PartLoadFrac,  &
             CBVAV(CBVAVNum)%CoolCoilCompIndex,ContFanCycCoil, OnOffAirFlowRatio)
          IF (SolFla == -1 .AND. .NOT. WarmupFlag) THEN
            IF(CBVAV(CBVAVNum)%DXIterationExceeded .LT. 1)THEN
              CBVAV(CBVAVNum)%DXIterationExceeded = CBVAV(CBVAVNum)%DXIterationExceeded + 1
              CALL ShowWarningError('Iteration limit exceeded calculating DX unit part-load ratio, for unit = '// &
                              TRIM(CBVAV(CBVAVNum)%DXCoolCoilName))
              CALL ShowContinueError('Calculated part-load ratio = '//TRIM(RoundSigDigits(PartLoadFrac,3)))
              CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation continues.'// &
                                            ' Occurrence info: ')
            ELSE
              CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                        ', Iteration limit exceeded for DX unit part-load ratio calculation error continues.', &
                                           CBVAV(CBVAVNum)%DXIterationExceededIndex,PartLoadFrac,PartLoadFrac)
            END IF
          ELSE IF (SolFla == -2 .AND. .NOT. WarmupFlag) THEN
            PartLoadFrac = MAX(0.0d0,MIN(1.0d0,(Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp - CBVAV(CBVAVNum)%CoilTempSetpoint) / &
                           (Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp - Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp)))
            IF(CBVAV(CBVAVNum)%DXIterationFailed .LT. 1)THEN
              CBVAV(CBVAVNum)%DXIterationFailed = CBVAV(CBVAVNum)%DXIterationFailed + 1
              CALL ShowSevereError('DX unit part-load ratio calculation failed: part-load ratio limits exceeded, for unit = '// &
                               TRIM(CBVAV(CBVAVNum)%DXCoolCoilName))
              CALL ShowContinueErrorTimeStamp('An estimated part-load ratio of '//TRIM(RoundSigDigits(PartLoadFrac,3))// &
                                            'will be used and the simulation continues. Occurrence info: ')
            ELSE
              CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                        ', Part-load ratio calculation failed for DX unit error continues.', &
                                           CBVAV(CBVAVNum)%DXIterationFailedIndex,PartLoadFrac,PartLoadFrac)
            END IF
          END IF
        END IF

      CASE (CoilDX_CoolingTwoStageWHumControl)  ! Coil:Cooling:DX:TwoStageWithHumidityControlMode
                                                 ! formerly (v3 and beyond) Coil:DX:MultiMode:CoolingEmpirical

        ! If DXCoolingSystem runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
        ! Multimode coil will switch to enhanced dehumidification if available and needed, but it
        ! still runs to meet the sensible load

        ! Determine required part load for normal mode

        ! Get full load result
        DehumidMode  = 0
        CBVAV(CBVAVNum)%DehumidificationMode = DehumidMode
        CALL SimDXCoilMultiMode(CBVAV(CBVAVNum)%DXCoolCoilName,On,FirstHVACIteration,PartLoadFrac,DehumidMode,  &
           CBVAV(CBVAVNum)%CoolCoilCompIndex,ContFanCycCoil)
        IF (Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp < CBVAV(CBVAVNum)%CoilTempSetpoint) THEN
          PartLoadFrac = 0.0d0
          CALL SimDXCoilMultiMode(CBVAV(CBVAVNum)%DXCoolCoilName,On,FirstHVACIteration,PartLoadFrac,DehumidMode, &
                                  CBVAV(CBVAVNum)%CoolCoilCompIndex,ContFanCycCoil)
        ELSE IF (Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp > CBVAV(CBVAVNum)%CoilTempSetpoint) THEN
          PartLoadFrac = 1.0d0
        ELSE
          Par(1) = REAL(CBVAV(CBVAVNum)%CoolCoilCompIndex,r64)
          Par(2) = CBVAV(CBVAVNum)%CoilTempSetpoint
          ! Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
          Par(3) = REAL(DehumidMode,r64)
          CALL SolveRegulaFalsi(SmallTempDiff, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0d0, 1.0d0, Par)
          IF (SolFla == -1) THEN
            IF(CBVAV(CBVAVNum)%MMDXIterationExceeded .LT. 1)THEN
              CBVAV(CBVAVNum)%MMDXIterationExceeded = CBVAV(CBVAVNum)%MMDXIterationExceeded + 1
              CALL ShowWarningError('Iteration limit exceeded calculating DX unit part-load ratio, for unit='// &
                            TRIM(CBVAV(CBVAVNum)%Name))
              CALL ShowContinueErrorTimeStamp('Part-load ratio returned = '//TRIM(RoundSigDigits(PartLoadFrac,2)))
              CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation continues.'// &
                                            ' Occurrence info: ')
            ELSE
              CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                      ', Iteration limit exceeded calculating DX unit part-load ratio error continues.', &
                                         CBVAV(CBVAVNum)%MMDXIterationExceededIndex,PartLoadFrac,PartLoadFrac)
            END IF
          ELSE IF (SolFla == -2) THEN
            PartLoadFrac = MAX(0.0d0,MIN(1.0d0,(Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp - CBVAV(CBVAVNum)%CoilTempSetpoint) / &
                         (Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp - Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp)))
            IF(CBVAV(CBVAVNum)%MMDXIterationFailed .LT. 1)THEN
              CBVAV(CBVAVNum)%MMDXIterationFailed = CBVAV(CBVAVNum)%MMDXIterationFailed + 1
              CALL ShowSevereError('DX unit part-load ratio calculation failed: part-load ratio limits exceeded, for unit='// &
                             TRIM(CBVAV(CBVAVNum)%Name))
              CALL ShowContinueError('Estimated part-load ratio = '//TRIM(RoundSigDigits(PartLoadFrac,3)))
              CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation continues.'// &
                                              ' Occurrence info: ')
            ELSE
              CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                      ', Part-load ratio calculation failed for DX unit error continues.', &
                                         CBVAV(CBVAVNum)%MMDXIterationFailedIndex,PartLoadFrac,PartLoadFrac)
            END IF
          END IF
        END IF

       ! If humidity setpoint is not satisfied and humidity control type is Multimode,
       ! then turn on enhanced dehumidification mode 1

       IF (( Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%HumRat > Node(OutletNode)%HumRatMax ) .AND. &
           ( Node(CBVAV(CBVAVNum)%DXCoilInletNode)%HumRat > Node(OutletNode)%HumRatMax ) .AND. &
           ( CBVAV(CBVAVNum)%DehumidControlType .EQ. DehumidControl_Multimode ) .AND. &
             Node(OutletNode)%HumRatMax .GT. 0.0d0) THEN

         ! Determine required part load for enhanced dehumidification mode 1

         ! Get full load result
         PartLoadFrac = 1.0d0
         DehumidMode  = 1
         CBVAV(CBVAVNum)%DehumidificationMode = DehumidMode
         CALL SimDXCoilMultiMode(CBVAV(CBVAVNum)%DXCoolCoilName,On,FirstHVACIteration,PartLoadFrac,DehumidMode,  &
            CBVAV(CBVAVNum)%CoolCoilCompIndex, ContFanCycCoil)
         IF (Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp < CBVAV(CBVAVNum)%CoilTempSetpoint) THEN
           PartLoadFrac = 0.0d0
         ELSE IF (Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp > CBVAV(CBVAVNum)%CoilTempSetpoint) THEN
           PartLoadFrac = 1.0d0
         ELSE
           Par(1) = REAL(CBVAV(CBVAVNum)%CoolCoilCompIndex,r64)
           Par(2) = CBVAV(CBVAVNum)%CoilTempSetpoint
           ! Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
           Par(3) = REAL(DehumidMode,r64)
           CALL SolveRegulaFalsi(SmallTempDiff, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0d0, 1.0d0, Par)
           IF (SolFla == -1) THEN
             IF(CBVAV(CBVAVNum)%DMDXIterationExceeded .LT. 1)THEN
               CBVAV(CBVAVNum)%DMDXIterationExceeded = CBVAV(CBVAVNum)%DMDXIterationExceeded + 1
               CALL ShowWarningError('Iteration limit exceeded calculating DX unit dehumidifying part-load ratio, '// &
                                 'for unit = '//TRIM(CBVAV(CBVAVNum)%Name))
               CALL ShowContinueErrorTimeStamp('Part-load ratio returned='//TRIM(RoundSigDigits(PartLoadFrac,2)))
               CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation continues.'// &
                                            ' Occurrence info: ')
             ELSE
               CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                     ', Iteration limit exceeded calculating DX unit dehumidifying part-load ratio error ' &
                                        //'continues.',CBVAV(CBVAVNum)%DMDXIterationExceededIndex,PartLoadFrac,PartLoadFrac)
             END IF
           ELSE IF (SolFla == -2) THEN
             PartLoadFrac = MAX(0.0d0,MIN(1.0d0,(Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp-CBVAV(CBVAVNum)%CoilTempSetpoint) / &
                               (Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp - Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp)))
             IF(CBVAV(CBVAVNum)%DMDXIterationFailed .LT. 1)THEN
                CBVAV(CBVAVNum)%DMDXIterationFailed = CBVAV(CBVAVNum)%DMDXIterationFailed + 1
               CALL ShowSevereError('DX unit dehumidifying part-load ratio calculation failed: part-load ratio '// &
                                    'limits exceeded, for unit = '//TRIM(CBVAV(CBVAVNum)%Name))
               CALL ShowContinueError('Estimated part-load ratio = '//TRIM(RoundSigDigits(PartLoadFrac,3)))
               CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation continues.'// &
                                               ' Occurrence info: ')
             ELSE
               CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                     ', Dehumidifying part-load ratio calculation failed for DX unit error continues.', &
                                        CBVAV(CBVAVNum)%DMDXIterationFailedIndex,PartLoadFrac,PartLoadFrac)
             END IF
           END IF
         END IF
       END IF ! End if humidity ratio setpoint not met - multimode humidity control

          ! If humidity setpoint is not satisfied and humidity control type is CoolReheat,
          ! then run to meet latent load

          IF (( Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%HumRat > Node(OutletNode)%HumRatMax) .AND. &
              ( Node(CBVAV(CBVAVNum)%DXCoilInletNode)%HumRat > Node(OutletNode)%HumRatMax) .AND. &
              (CBVAV(CBVAVNum)%DehumidControlType .EQ. DehumidControl_CoolReheat) .AND. &
               Node(OutletNode)%HumRatMax .GT. 0.0d0) THEN

            ! Determine revised desired outlet temperature  - use approach temperature control strategy
            ! based on CONTROLLER:SIMPLE TEMPANDHUMRAT control type.

            ! Calculate the approach temperature (difference between SA dry-bulb temp and SA dew point temp)
            ApproachTemp = Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp - &
                           PsyTdpFnWPb(Node(OutletNode)%HumRat,OutdoorBaroPress)
            ! Calculate the dew point temperature at the SA humidity ratio setpoint
            DesiredDewPoint = PsyTdpFnWPb(Node(OutletNode)%HumRatMax, OutdoorBaroPress)
            ! Adjust the calculated dew point temperature by the approach temp
            CBVAV(CBVAVNum)%CoilTempSetpoint = &
              MIN(CBVAV(CBVAVNum)%CoilTempSetpoint, (DesiredDewPoint + ApproachTemp))

            ! Determine required part load for cool reheat at adjusted DesiredOutletTemp

            ! Get full load result
            PartLoadFrac = 1.0d0
            DehumidMode  = 0
            CBVAV(CBVAVNum)%DehumidificationMode = DehumidMode
            CALL SimDXCoilMultiMode(CBVAV(CBVAVNum)%DXCoolCoilName,On,FirstHVACIteration,PartLoadFrac,DehumidMode,  &
               CBVAV(CBVAVNum)%CoolCoilCompIndex, ContFanCycCoil)
              IF (Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp < CBVAV(CBVAVNum)%CoilTempSetpoint) THEN
                PartLoadFrac = 0.0d0
              ELSE IF (Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp > CBVAV(CBVAVNum)%CoilTempSetpoint) THEN
                PartLoadFrac = 1.0d0
              ELSE
                Par(1) = REAL(CBVAV(CBVAVNum)%CoolCoilCompIndex,r64)
                Par(2) = CBVAV(CBVAVNum)%CoilTempSetpoint
                ! Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                Par(3) = REAL(DehumidMode,r64)
                CALL SolveRegulaFalsi(SmallTempDiff, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0d0, 1.0d0, Par)
                IF (SolFla == -1) THEN
                  IF(CBVAV(CBVAVNum)%CRDXIterationExceeded .LT. 1)THEN
                    CBVAV(CBVAVNum)%CRDXIterationExceeded = CBVAV(CBVAVNum)%CRDXIterationExceeded + 1
                    CALL ShowWarningError('Iteration limit exceeded calculating DX unit cool reheat part-load ratio, '// &
                                  'for unit = '//TRIM(CBVAV(CBVAVNum)%Name))
                    CALL ShowContinueErrorTimeStamp('Part-load ratio returned = '//TRIM(RoundSigDigits(PartLoadFrac,2)))
                    CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation '// &
                                            'continues. Occurrence info: ')
                  ELSE
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                 ', Iteration limit exceeded calculating cool reheat part-load ratio DX unit error continues.', &
                                    CBVAV(CBVAVNum)%CRDXIterationExceededIndex,PartLoadFrac,PartLoadFrac)
                  END IF
                ELSE IF (SolFla == -2) THEN
                  PartLoadFrac = MAX(0.0d0,MIN(1.0d0,  &
                                              (Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp-CBVAV(CBVAVNum)%CoilTempSetpoint) / &
                               (Node(CBVAV(CBVAVNum)%DXCoilInletNode)%Temp - Node(CBVAV(CBVAVNum)%DXCoilOutletNode)%Temp)))
                  IF(CBVAV(CBVAVNum)%CRDXIterationFailed .LT. 1)THEN
                    CBVAV(CBVAVNum)%CRDXIterationFailed = CBVAV(CBVAVNum)%CRDXIterationFailed + 1
                    CALL ShowSevereError('DX unit cool reheat part-load ratio calculation failed: part-load ratio limits '// &
                                   'exceeded, for unit = '//TRIM(CBVAV(CBVAVNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio = '//TRIM(RoundSigDigits(PartLoadFrac,3)))
                    CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation '// &
                                               'continues. Occurrence info: ')
                  ELSE
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(CBVAV(CBVAVNum)%Name)// &
                                     ', Dehumidifying part-load ratio calculation failed for DX unit error continues.', &
                                        CBVAV(CBVAVNum)%DMDXIterationFailedIndex,PartLoadFrac,PartLoadFrac)
                  END IF
                END IF
              END IF
          END IF ! End if humidity ratio setpoint not met - CoolReheat humidity control

          IF(PartLoadFrac.GT.1.0d0) THEN
            PartLoadFrac = 1.0d0
          ELSEIF(PartLoadFrac < 0.0d0) THEN
            PartLoadFrac = 0.0d0
          END IF

      CASE DEFAULT
        CALL ShowFatalError('SimCBVAV System: Invalid DX Cooling Coil='//  &
                          TRIM(CBVAV(CBVAVNum)%DXCoolCoilType))

      END SELECT
    ELSE ! IF(OutdoorDryBulbTemp .GE. CBVAV(CBVAVNum)%MinOATCompressor)THEN
!     Simulate DX cooling coil with compressor off
      IF (CBVAV(CBVAVNum)%DXCoolCoilType_Num == CoilDX_CoolingHXAssisted) THEN
        CALL SimHXAssistedCoolingCoil(CBVAV(CBVAVNum)%DXCoolCoilName,FirstHVACIteration,Off,0.0d0,&
                                      CBVAV(CBVAVNum)%CoolCoilCompIndex, ContFanCycCoil, HXUnitEnable=HXUnitOn)
      ELSE IF(CBVAV(CBVAVNum)%DXCoolCoilType_Num == CoilDX_CoolingSingleSpeed)THEN
        CALL SimDXCoil(CBVAV(CBVAVNum)%DXCoolCoilName,Off,FirstHVACIteration, 0.0d0,CBVAV(CBVAVNum)%CoolCoilCompIndex,  &
           ContFanCycCoil, OnOffAirFlowRatio)
      ELSE IF(CBVAV(CBVAVNum)%DXCoolCoilType_Num == CoilDX_CoolingTwoStageWHumControl)THEN
        CALL SimDXCoilMultiMode(CBVAV(CBVAVNum)%DXCoolCoilName,Off,FirstHVACIteration, 0.0d0,0,CBVAV(CBVAVNum)%CoolCoilCompIndex, &
                                ContFanCycCoil)
      END IF
    END IF
    SaveCompressorPLR = DXCoilPartLoadRatio(CBVAV(CBVAVNum)%DXCoolCoilIndexNum)
! Simulate cooling coil with compressor off if zone requires heating
  ELSE ! HeatCoolMode == HeatingMode and no cooling is required, set PLR to 0
    IF (CBVAV(CBVAVNum)%DXCoolCoilType_Num == CoilDX_CoolingHXAssisted) THEN
      CALL SimHXAssistedCoolingCoil(CBVAV(CBVAVNum)%DXCoolCoilName,FirstHVACIteration,Off,0.0d0,CBVAV(CBVAVNum)%CoolCoilCompIndex, &
                                    ContFanCycCoil, HXUnitEnable=HXUnitOn)
    ELSE IF(CBVAV(CBVAVNum)%DXCoolCoilType_Num == CoilDX_CoolingSingleSpeed)THEN
      CALL SimDXCoil(CBVAV(CBVAVNum)%DXCoolCoilName,Off,FirstHVACIteration, 0.0d0,CBVAV(CBVAVNum)%CoolCoilCompIndex,  &
         ContFanCycCoil, OnOffAirFlowRatio)
    ELSE IF(CBVAV(CBVAVNum)%DXCoolCoilType_Num == CoilDX_CoolingTwoStageWHumControl)THEN
      CALL SimDXCoilMultiMode(CBVAV(CBVAVNum)%DXCoolCoilName,Off,FirstHVACIteration,0.0d0,0,CBVAV(CBVAVNum)%CoolCoilCompIndex, &
                              ContFanCycCoil)
    END IF
  END IF

! Simulate the heating coil based on coil type
  SELECT CASE(CBVAV(CBVAVNum)%HeatCoilType_Num)

  CASE(CoilDX_HeatingEmpirical)
!   Simulate DX heating coil if zone load is positive (heating load)
    IF (CBVAV(CBVAVNum)%HeatCoolMode == HeatingMode)THEN
      IF(OutdoorDryBulbTemp .GT. CBVAV(CBVAVNum)%MinOATCompressor)THEN
!       simulate the DX heating coil
        CALL SimDXCoil(CBVAV(CBVAVNum)%HeatCoilName,On,FirstHVACIteration, PartLoadFrac,CBVAV(CBVAVNum)%HeatCoilIndex,  &
           ContFanCycCoil, OnOffAirFlowRatio)
        IF(Node(CBVAV(CBVAVNum)%HeatingCoilOutletNode)%Temp .GT. CBVAV(CBVAVNum)%CoilTempSetpoint .AND. &
           Node(CBVAV(CBVAVNum)%HeatingCoilInletNode)%Temp .LT. CBVAV(CBVAVNum)%CoilTempSetpoint)THEN
            ! iterate to find PLR at CoilTempSetpoint
            Par(1) = REAL(CBVAV(CBVAVNum)%HeatCoilIndex,r64)
            Par(2) = MIN(CBVAV(CBVAVNum)%CoilTempSetpoint,CBVAV(CBVAVNum)%MaxLATHeating)
            Par(3) = OnOffAirFlowRatio
            CALL SolveRegulaFalsi(SmallTempDiff, MaxIte, SolFla, PartLoadFrac, DXHeatingCoilResidual, 0.0d0, 1.0d0, Par)
            CALL SimDXCoil(CBVAV(CBVAVNum)%HeatCoilName,On,FirstHVACIteration, PartLoadFrac,CBVAV(CBVAVNum)%HeatCoilIndex,  &
               ContFanCycCoil, OnOffAirFlowRatio)
            IF (SolFla == -1 .AND. .NOT. WarmupFlag) THEN
              CALL ShowWarningError('Iteration limit exceeded calculating DX unit part-load ratio, for unit = '// &
                                TRIM(CBVAV(CBVAVNum)%HeatCoilName))
              CALL ShowContinueError('Calculated part-load ratio = '//TRIM(RoundSigDigits(PartLoadFrac,3)))
              CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation continues.'// &
                                              ' Occurrence info: ')
            ELSE IF (SolFla == -2 .AND. .NOT. WarmupFlag) THEN
              CALL ShowSevereError('DX unit part-load ratio calculation failed: part-load ratio limits exceeded, for unit = '// &
                                 TRIM(CBVAV(CBVAVNum)%HeatCoilName))
              CALL ShowContinueErrorTimeStamp('A part-load ratio of '//TRIM(RoundSigDigits(PartLoadFrac,3))// &
                                              'will be used and the simulation continues. Occurrence info: ')
              CALL ShowContinueError('Please send this information to the EnergyPlus support group.')
            END IF
        END IF
      ELSE ! OAT .LT. MinOATCompressor
!       simulate DX heating coil with compressor off
        CALL SimDXCoil(CBVAV(CBVAVNum)%HeatCoilName,Off,FirstHVACIteration,0.0d0, CBVAV(CBVAVNum)%HeatCoilIndex,  &
                       ContFanCycCoil, OnOffAirFlowRatio)
      END IF
      SaveCompressorPLR = DXCoilPartLoadRatio(CBVAV(CBVAVNum)%DXHeatCoilIndexNum)
    ELSE ! HeatCoolMode = CoolingMode
!     simulate DX heating coil with compressor off when cooling load is required
      CALL SimDXCoil(CBVAV(CBVAVNum)%HeatCoilName,Off,FirstHVACIteration, 0.0d0,CBVAV(CBVAVNum)%HeatCoilIndex, &
                     ContFanCycCoil, OnOffAirFlowRatio)
    END IF
  CASE(Coil_HeatingGas, Coil_HeatingElectric, Coil_HeatingWater, Coil_HeatingSteam)  ! not a DX heating coil
    IF(CBVAV(CBVAVNum)%HeatCoolMode == HeatingMode)THEN
      CpAir = PsyCpAirFnWTdb(Node(CBVAV(CBVAVNum)%HeatingCoilInletNode)%HumRat,Node(CBVAV(CBVAVNum)%HeatingCoilInletNode)%Temp)
      QHeater = Node(CBVAV(CBVAVNum)%HeatingCoilInletNode)%MassFlowRate * CpAir * &
                  (CBVAV(CBVAVNum)%CoilTempSetpoint - Node(CBVAV(CBVAVNum)%HeatingCoilInletNode)%Temp)
    ELSE
      QHeater = 0.0d0
    END IF
    ! Added None DX heating coils calling point
    CALL CalcNonDXHeatingCoils(CBVAVNum,FirstHVACIteration,QHeater,CBVAV(CBVAVNum)%OpMode,QHeaterActual)
  CASE DEFAULT
        CALL ShowFatalError('SimCBVAV System: Invalid Heating Coil='//  &
                          TRIM(CBVAV(CBVAVNum)%HeatCoilType))

  END SELECT

  IF (CBVAV(CBVAVNum)%FanPlace .EQ. DrawThru)&
     CALL SimulateFanComponents(CBVAV(CBVAVNum)%FanName,FirstHVACIteration,CBVAV(CBVAVNum)%FanIndex,FanSpeedRatio)

  Node(OutletNode)%MassFlowRate =  (1.0d0 - ByPassDuctFlowFraction) * Node(CBVAV(CBVAVNum)%MixerInletAirNode)%MassFlowRate
  Node(OutletNode)%Temp         = Node(CBVAV(CBVAVNum)%SplitterOutletAirNode)%Temp
  Node(OutletNode)%HumRat       = Node(CBVAV(CBVAVNum)%SplitterOutletAirNode)%HumRat
  Node(OutletNode)%Quality      = Node(CBVAV(CBVAVNum)%SplitterOutletAirNode)%Quality
  Node(OutletNode)%Press        = Node(CBVAV(CBVAVNum)%SplitterOutletAirNode)%Press
  Node(OutletNode)%Enthalpy     = Node(CBVAV(CBVAVNum)%SplitterOutletAirNode)%Enthalpy
  Node(OutletNode)%Height       = Node(CBVAV(CBVAVNum)%SplitterOutletAirNode)%Height

  CBVAV(CBVAVNum)%BypassMassFlowRate = ByPassDuctFlowFraction * Node(CBVAV(CBVAVNum)%MixerInletAirNode)%MassFlowRate

! calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
  MinHumRat = MIN(Node(InletNode)%HumRat,Node(OutletNode)%HumRat)
  LoadMet   = Node(OutletNode)%MassFlowRate * &
             (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat) - PsyHFnTdbW(Node(InletNode)%Temp,MinHumRat))

RETURN
END SUBROUTINE CalcCBVAV

SUBROUTINE GetZoneLoads(CBVAVNum, QZoneReq)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is used to poll the thermostats in each zone and determine the
          ! mode of operation, either cooling, heating, or none.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands,      ONLY: ZoneSysEnergyDemand, CurDeadBandOrSetback

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: CBVAVNum            ! Index to CBVAV unit being simulated
  REAL(r64),    INTENT (INOUT) :: QZoneReq           ! Total zone load served by this air loop

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ZoneNum          ! Zone number of controlled zone in CBVAV loop
  REAL(r64)           :: QZoneReqCool     ! Total cooling load in all controlled zones [W]
  REAL(r64)           :: QZoneReqHeat     ! Total heating load in all controlled zones [W]
  REAL(r64)           :: ZoneLoad         ! Total load in controlled zone [W]
  REAL(r64)           :: ZoneLoadToCoolSPSequenced
  REAL(r64)           :: ZoneLoadToHeatSPSequenced

  QZoneReqCool = 0.0d0
  QZoneReqHeat = 0.0d0
  CBVAV(CBVAVNum)%NumZonesCooled = 0
  CBVAV(CBVAVNum)%NumZonesHeated = 0
  CBVAV(CBVAVNum)%HeatCoolMode = 0

  DO ZoneNum = 1, CBVAV(CBVAVNum)%NumControlledZones
    IF ((CBVAV(CBVAVNum)%ZoneSequenceCoolingNum(ZoneNum) > 0) .AND. (CBVAV(CBVAVNum)%ZoneSequenceHeatingNum(ZoneNum) > 0)) THEN
      ZoneLoadToCoolSPSequenced = ZoneSysEnergyDemand(CBVAV(CBVAVNum)%ControlledZoneNum(ZoneNum))%&
                  SequencedOutputRequiredToCoolingSP(CBVAV(CBVAVNum)%ZoneSequenceCoolingNum(ZoneNum))
      ZoneLoadToHeatSPSequenced = ZoneSysEnergyDemand(CBVAV(CBVAVNum)%ControlledZoneNum(ZoneNum))%&
                  SequencedOutputRequiredToHeatingSP(CBVAV(CBVAVNum)%ZoneSequenceHeatingNum(ZoneNum))
      IF (ZoneLoadToHeatSPSequenced > 0.d0 .AND. ZoneLoadToCoolSPSequenced > 0.d0) THEN
        ZoneLoad = ZoneLoadToHeatSPSequenced
      ELSEIF (ZoneLoadToHeatSPSequenced < 0.d0 .AND. ZoneLoadToCoolSPSequenced < 0.d0) THEN
        ZoneLoad = ZoneLoadToCoolSPSequenced
      ELSEIF (ZoneLoadToHeatSPSequenced <= 0.d0 .AND. ZoneLoadToCoolSPSequenced >= 0.d0) THEN
        ZoneLoad = 0.d0
      ENDIF
    ELSE
      ZoneLoad= ZoneSysEnergyDemand(CBVAV(CBVAVNum)%ControlledZoneNum(ZoneNum))%RemainingOutputRequired
    ENDIF


    IF(.NOT. CurDeadBandOrSetback(ZoneNum))THEN
      IF(ZoneLoad .GT. 0.0d0 .AND. ABS(ZoneLoad) .GT. SmallLoad)THEN
        QZoneReqHeat = QZoneReqHeat + ZoneLoad
        CBVAV(CBVAVNum)%NumZonesHeated = CBVAV(CBVAVNum)%NumZonesHeated + 1
      ELSE IF(ZoneLoad .LT. 0.0d0 .AND. ABS(ZoneLoad) .GT. SmallLoad)THEN
        QZoneReqCool = QZoneReqCool + ZoneLoad
        CBVAV(CBVAVNum)%NumZonesCooled = CBVAV(CBVAVNum)%NumZonesCooled + 1
      END IF
    END IF
  END DO

  SELECT CASE(CBVAV(CBVAVNum)%PriorityControl)
    CASE (CoolingPriority)
      IF(QZoneReqCool .LT. 0.0d0)THEN
        QZoneReq = QZoneReqCool
        CBVAV(CBVAVNum)%HeatCoolMode = CoolingMode
      ELSE IF(QZoneReqHeat .GT. 0.0d0)THEN
        QZoneReq = QZoneReqHeat
        CBVAV(CBVAVNum)%HeatCoolMode = HeatingMode
      ELSE
        QZoneReq = 0.0d0
      END IF
    CASE (HeatingPriority)
      IF(QZoneReqHeat .GT. 0.0d0)THEN
        QZoneReq = QZoneReqHeat
        CBVAV(CBVAVNum)%HeatCoolMode = HeatingMode
      ELSE IF(QZoneReqCool .LT. 0.0d0)THEN
        QZoneReq = QZoneReqCool
        CBVAV(CBVAVNum)%HeatCoolMode = CoolingMode
      ELSE
        QZoneReq = 0.0d0
      END IF
    CASE (ZonePriority)
      IF(CBVAV(CBVAVNum)%NumZonesHeated .GT. CBVAV(CBVAVNum)%NumZonesCooled)THEN
        IF(QZoneReqHeat .GT. 0.0d0)THEN
          QZoneReq = QZoneReqHeat
          CBVAV(CBVAVNum)%HeatCoolMode = HeatingMode
        ELSE IF(QZoneReqCool .LT. 0.0d0)THEN
          QZoneReq = QZoneReqCool
          CBVAV(CBVAVNum)%HeatCoolMode = CoolingMode
        ELSE
          QZoneReq = 0.0d0
        END IF
      ELSE IF(CBVAV(CBVAVNum)%NumZonesCooled .GT. CBVAV(CBVAVNum)%NumZonesHeated)THEN
        IF(QZoneReqCool .LT. 0.0d0)THEN
          QZoneReq = QZoneReqCool
          CBVAV(CBVAVNum)%HeatCoolMode = CoolingMode
        ELSE IF(QZoneReqHeat .GT. 0.0d0)THEN
          QZoneReq = QZoneReqHeat
          CBVAV(CBVAVNum)%HeatCoolMode = HeatingMode
        ELSE
          QZoneReq = 0.0d0
        END IF
      ELSE
        IF(ABS(QZoneReqCool) .GT. ABS(QZoneReqHeat) .AND. QZoneReqCool .NE. 0.0d0)THEN
          QZoneReq = QZoneReqCool
          CBVAV(CBVAVNum)%HeatCoolMode = CoolingMode
        ELSE IF(ABS(QZoneReqCool) .LT. ABS(QZoneReqHeat) .AND. QZoneReqHeat .NE. 0.0d0)THEN
          QZoneReq = QZoneReqHeat
          CBVAV(CBVAVNum)%HeatCoolMode = HeatingMode
        ELSE IF(ABS(QZoneReqCool) .EQ. ABS(QZoneReqHeat) .AND. QZoneReqCool .NE. 0.0d0)THEN
          QZoneReq = QZoneReqCool
          CBVAV(CBVAVNum)%HeatCoolMode = CoolingMode
        ELSE
          QZoneReq = 0.0d0
        END IF
      END IF
  END SELECT

 RETURN
END SUBROUTINE GetZoneLoads

REAL(r64) FUNCTION CalcSetpointTempTarget(CBVAVNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculate outlet air node temperature setpoint

          ! METHODOLOGY EMPLOYED:
          !  Calculate an outlet temperature to satisfy zone loads. This temperature is calculated
          !  based on 1 zone's VAV box fully opened. The other VAV boxes are partially open (modulated).

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE Psychrometrics,        ONLY: PsyCpAirFnWTdb
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CBVAVNumber      ! Index to changeover-bypass VAV system

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  INTEGER :: OutletNode          ! Outlet node of CBVAV system
  INTEGER :: ZoneNum             ! Index to controlled zone
  INTEGER :: ZoneNodeNum         ! Zone node number of controlled zone
  INTEGER :: BoxOutletNodeNum    ! CBVAV box outlet node (zone supply inlet node)
  REAL(r64)    :: DXCoolCoilInletTemp ! Air temperature of CBVAV DX cooling coil air inlet node [C]
  REAL(r64)    :: OutAirTemp          ! Outlet air temperature of CBVAV system [C]
  REAL(r64)    :: OutAirHumRat        ! Outlet air humidity ratio of CBVAV system [C]
  REAL(r64)    :: ZoneLoad            ! Zone load sensed by thermostat [W]
  REAL(r64)    :: CpSupplyAir         ! Specific heat of CBVAV system outlet air [J/kg-K]
  REAL(r64)    :: QToCoolSetPt        ! Zone load to cooling setpoint [W]
  REAL(r64)    :: QToHeatSetPt        ! Zone load to heating setpoint [W]
  REAL(r64)    :: SupplyAirTemp       ! Supply air temperature required to meet load [C]
  REAL(r64)    :: TSupplyToHeatSetPtMax ! Maximum of the supply air temperatures required to reach the heating setpoint [C]
  REAL(r64)    :: TSupplyToCoolSetPtMin ! Minimum of the supply air temperatures required to reach the cooling setpoint [C]
  REAL(r64)    :: SupplyAirTempToHeatSetPt ! Supply air temperature required to reach the heating setpoint [C]
  REAL(r64)    :: SupplyAirTempToCoolSetPt ! Supply air temperature required to reach the cooling setpoint [C]

  DXCoolCoilInletTemp = Node(CBVAV(CBVAVNumber)%DXCoilInletNode)%Temp
  OutAirTemp = Node(CBVAV(CBVAVNumber)%AirOutNode)%Temp
  OutAirHumRat = Node(CBVAV(CBVAVNumber)%AirOutNode)%HumRat

  IF(CBVAV(CBVAVNumber)%HeatCoolMode == CoolingMode)THEN ! Cooling required
    CalcSetpointTempTarget    = 99999.0d0
  ELSE IF(CBVAV(CBVAVNumber)%HeatCoolMode == HeatingMode)THEN ! Heating required
    CalcSetpointTempTarget    = -99999.0d0
  END IF
  TSupplyToHeatSetPtMax    = -99999.0d0
  TSupplyToCoolSetPtMin    = 99999.0d0

    OutletNode         = CBVAV(CBVAVNumber)%AirOutNode
    DO ZoneNum         = 1, CBVAV(CBVAVNumber)%NumControlledZones
      ZoneNodeNum      = CBVAV(CBVAVNumber)%ActualZoneNodeNum(ZoneNum)
      BoxOutletNodeNum = CBVAV(CBVAVNumber)%CBVAVBoxOutletNode(ZoneNum)
      IF ((CBVAV(CBVAVNumber)%ZoneSequenceCoolingNum(ZoneNum) > 0) .AND. &
          (CBVAV(CBVAVNumber)%ZoneSequenceHeatingNum(ZoneNum) > 0)) THEN
        QToCoolSetPt = ZoneSysEnergyDemand(CBVAV(CBVAVNumber)%ControlledZoneNum(ZoneNum))%&
                    SequencedOutputRequiredToCoolingSP(CBVAV(CBVAVNumber)%ZoneSequenceCoolingNum(ZoneNum))
        QToHeatSetPt = ZoneSysEnergyDemand(CBVAV(CBVAVNumber)%ControlledZoneNum(ZoneNum))%&
                    SequencedOutputRequiredToHeatingSP(CBVAV(CBVAVNumber)%ZoneSequenceHeatingNum(ZoneNum))
        IF (QToHeatSetPt > 0.d0 .AND. QToCoolSetPt > 0.d0) THEN
          ZoneLoad = QToHeatSetPt
        ELSEIF (QToHeatSetPt < 0.d0 .AND. QToCoolSetPt < 0.d0) THEN
          ZoneLoad = QToCoolSetPt
        ELSEIF (QToHeatSetPt <= 0.d0 .AND. QToCoolSetPt >= 0.d0) THEN
          ZoneLoad = 0.d0
        ENDIF
      ELSE
        ZoneLoad     = ZoneSysEnergyDemand(CBVAV(CBVAVNumber)%ControlledZoneNum(ZoneNum))%RemainingOutputRequired
        QToCoolSetPt = ZoneSysEnergyDemand(CBVAV(CBVAVNumber)%ControlledZoneNum(ZoneNum))%OutputRequiredToCoolingSP
        QToHeatSetPt = ZoneSysEnergyDemand(CBVAV(CBVAVNumber)%ControlledZoneNum(ZoneNum))%OutputRequiredToHeatingSP
      ENDIF

      CpSupplyAir      = PsyCpAirFnWTdb(OutAirHumRat,OutAirTemp)

!     Find the supply air temperature that will force the box to full flow
      IF(BoxOutletNodeNum .GT. 0)THEN
        IF(CpSupplyAir*Node(BoxOutletNodeNum)%MassFlowRateMax .EQ. 0.0d0)THEN
          SupplyAirTemp = Node(ZoneNodeNum)%Temp
        ELSE
!         The target supply air temperature is slightly
          SupplyAirTemp = Node(ZoneNodeNum)%Temp + ZoneLoad/(CpSupplyAir*Node(BoxOutletNodeNum)%MassFlowRateMax)
        END IF
      ELSE
        SupplyAirTemp = Node(ZoneNodeNum)%Temp
      END IF

!     Save the MIN (cooling) or MAX (heating) temperature for coil control
!     One box will always operate at maximum damper position minimizing overall system energy use
      IF(CBVAV(CBVAVNumber)%HeatCoolMode == CoolingMode)THEN
        CalcSetpointTempTarget = MIN(SupplyAirTemp,CalcSetpointTempTarget)
      ELSE IF(CBVAV(CBVAVNumber)%HeatCoolMode == HeatingMode)THEN
        CalcSetpointTempTarget = MAX(SupplyAirTemp,CalcSetpointTempTarget)
      ELSE
!       Should use CpAirAtCoolSetpoint or CpAirAtHeatSetpoint here?
!       If so, use ZoneThermostatSetPointLo(ZoneNum) and ZoneThermostatSetPointHi(ZoneNum)
!       along with the zone humidity ratio
        IF(CpSupplyAir*Node(BoxOutletNodeNum)%MassFlowRateMax .EQ. 0.0d0)THEN
          SupplyAirTempToHeatSetPt = Node(ZoneNodeNum)%Temp
          SupplyAirTempToCoolSetPt = Node(ZoneNodeNum)%Temp
        ELSE
          SupplyAirTempToHeatSetPt = Node(ZoneNodeNum)%Temp + QToHeatSetPt/(CpSupplyAir*Node(BoxOutletNodeNum)%MassFlowRateMax)
          SupplyAirTempToCoolSetPt = Node(ZoneNodeNum)%Temp + QToCoolSetPt/(CpSupplyAir*Node(BoxOutletNodeNum)%MassFlowRateMax)
        END IF
        TSupplyToHeatSetPtMax = MAX(SupplyAirTempToHeatSetPt, TSupplyToHeatSetPtMax)
        TSupplyToCoolSetPtMin = MIN(SupplyAirTempToCoolSetPt, TSupplyToCoolSetPtMin)
      END IF

    END DO

!   Account for floating condition where cooling/heating is required to avoid overshooting setpoint
    IF(CBVAV(CBVAVNumber)%HeatCoolMode == 0 .AND. CBVAV(CBVAVNumber)%OpMode == ContFanCycCoil)THEN
      IF(OutAirTemp .GT. TSupplyToCoolSetPtMin)THEN
        CalcSetpointTempTarget          = TSupplyToCoolSetPtMin
        CBVAV(CBVAVNumber)%HeatCoolMode = CoolingMode
      ELSEIF(OutAirTemp .LT. TSupplyToHeatSetPtMax)THEN
        CalcSetpointTempTarget          = TSupplyToHeatSetPtMax
        CBVAV(CBVAVNumber)%HeatCoolMode = HeatingMode
      ELSE
        CalcSetpointTempTarget = OutAirTemp
      END IF
!   Reset setpoint to inlet air temp if unit is OFF and in cycling fan mode
    ELSE IF(CBVAV(CBVAVNumber)%HeatCoolMode == 0 .AND. CBVAV(CBVAVNumber)%OpMode == CycFanCycCoil)THEN
      CalcSetpointTempTarget = Node(CBVAV(CBVAVNumber)%AirInNode)%Temp
!   Reset cooling/heating mode to OFF if mixed air inlet temperature is below/above setpoint temperature.
!   HeatCoolMode = 0 for OFF, 1 for cooling, 2 for heating
    ELSE IF(CBVAV(CBVAVNumber)%HeatCoolMode == CoolingMode .AND. DXCoolCoilInletTemp .LT. CalcSetpointTempTarget)THEN
      CalcSetpointTempTarget = DXCoolCoilInletTemp
      CBVAV(CBVAVNumber)%HeatCoolMode = 0
    ELSE IF(CBVAV(CBVAVNumber)%HeatCoolMode == HeatingMode .AND. DXCoolCoilInletTemp .GT. CalcSetpointTempTarget)THEN
      CalcSetpointTempTarget = DXCoolCoilInletTemp
      CBVAV(CBVAVNumber)%HeatCoolMode = 0
    END IF

!   Limit outlet node temperature to MAX/MIN specified in input
    IF(CalcSetpointTempTarget .LT. CBVAV(CBVAVNumber)%MinLATCooling) CalcSetpointTempTarget = CBVAV(CBVAVNumber)%MinLATCooling
    IF(CalcSetpointTempTarget .GT. CBVAV(CBVAVNumber)%MaxLATHeating) CalcSetpointTempTarget = CBVAV(CBVAVNumber)%MaxLATHeating

  RETURN

END FUNCTION CalcSetpointTempTarget

FUNCTION DOE2DXCoilResidual(PartLoadFrac, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2006
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, CalcDoe2DXCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadFrac               ! Compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = DX coil number
                                                    ! Par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CoilIndex              ! Index of this coil
  REAL(r64)    :: OutletAirTemp          ! Outlet air temperature [C]
  REAL(r64)    :: OnOffAirFlowFrac       ! Ratio of compressor ON to compressor OFF air mass flow rate

  CoilIndex        = INT(Par(1))
  OnOffAirFlowFrac = Par(3)

  CALL CalcDoe2DXCoil(CoilIndex,On,.FALSE., PartLoadFrac, ContFanCycCoil, OnOffAirFlowRatio=OnOffAirFlowFrac)

  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum      = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DOE2DXCoilResidual

FUNCTION HXAssistDXCoilResidual(PartLoadFrac, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2006
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired DX coil outlet temp - actual DX coil outlet temp)
          ! HX Assisted DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadFrac               ! Compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = DX coil number
                                                    ! Par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CoilIndex        ! Index of this coil
  REAL(r64)    :: OutletAirTemp    ! Outlet air temperature of DX cooling coil [C]
  REAL(r64)    :: OnOffAirFlowFrac ! Air flow fraction
  INTEGER :: CBVAVNumTemp      ! Local index to changeover-bypass VAV system
  LOGICAL :: FirstHVACIter    ! Local Flag denoting the first pass on the air loop simulation
  LOGICAL :: HXUnitOn         ! flag to enable heat exchanger

  CoilIndex        = INT(Par(1))
  OnOffAirFlowFrac = Par(3)
  CBVAVNumTemp      = INT(Par(4))
  IF(Par(5) .EQ. 1.0d0)THEN
    FirstHVACIter = .TRUE.
  ELSE
    FirstHVACIter = .FALSE.
  END IF
  IF(Par(6) .EQ. 1.0d0)THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF

  CALL SimHXAssistedCoolingCoil(CBVAV(CBVAVNumTemp)%DXCoolCoilName,FirstHVACIter,On,PartLoadFrac, CoilIndex, &
                                ContFanCycCoil, HXUnitEnable=HXUnitOn)

  OutletAirTemp = Node(CBVAV(CBVAVNumTemp)%DXCoilOutletNode)%Temp
  Residuum      = Par(2) - OutletAirTemp

  RETURN
END FUNCTION HXAssistDXCoilResidual

FUNCTION DXHeatingCoilResidual(PartLoadFrac, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2006
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, CalcDXHeatingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadFrac               ! Compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = DX coil number
                                                    ! Par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CoilIndex        ! Index of this coil
  REAL(r64)    :: OutletAirTemp    ! Outlet air temperature [C]
  REAL(r64)    :: OnOffAirFlowFrac ! Ratio of compressor ON to compressor OFF air mass flow rate

  CoilIndex        = INT(Par(1))
  OnOffAirFlowFrac = Par(3)

  CALL CalcDXHeatingCoil(CoilIndex,PartLoadFrac,ContFanCycCoil,OnOffAirFlowFrac)

  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum      = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DXHeatingCoilResidual

FUNCTION MultiModeDXCoilResidual(PartLoadRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         M. J. Witte, GARD Analytics, Inc.
          !       DATE WRITTEN   February 2005
          !                      (based on DOE2DXCoilResidual by Richard Raustad, FSEC)
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimDXCoilMultiMode to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, SimDXCoilMultiMode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
                                                    ! par(3) = dehumidification mode (0=normal, 1=enhanced)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CoilIndex       ! index of this coil
  REAL(r64) ::    OutletAirTemp   ! outlet air temperature [C]
  INTEGER :: DehumidMode     ! dehumidification mode (par3)
  INTEGER :: FanOpMode       ! allows parent object to control fan mode

  CoilIndex   = INT(Par(1))
  DehumidMode = INT(Par(3))
  FanOpMode = 2
  CALL SimDXCoilMultiMode('',On,.FALSE.,PartLoadRatio,DehumidMode,CoilIndex, FanOpMode)
  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION MultiModeDXCoilResidual

SUBROUTINE SetAverageAirFlow(CBVAVNum,OnOffAirFlowRatio, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the average air mass flow rates for this time step
          ! Set OnOffAirFlowRatio to be used by DX coils

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE MixedAir,              ONLY: SimOAMixer
  USE Psychrometrics,        ONLY: PsyCpAirFnWTdb
  USE ScheduleManager,       ONLY: GetCurrentScheduleValue
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)    :: CBVAVNum            ! Index to CBVAV system
  REAL(r64)   , INTENT (INOUT) :: OnOffAirFlowRatio  ! Ratio of compressor ON airflow to average airflow over timestep
  LOGICAL, INTENT (IN)    :: FirstHVACIteration ! Flag denoting the first pass on the air loop simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InletNode            ! Inlet node number for CBVAVNum
  INTEGER             :: OutletNode           ! Outlet node number for CBVAVNum
  INTEGER             :: MixerMixedAirNode    ! Mixed air node number in OA mixer
  INTEGER             :: MixerOutsideAirNode  ! Outside air node number in OA mixer
  INTEGER             :: MixerReliefAirNode   ! Relief air node number in OA mixer
  INTEGER             :: MixerInletAirNode   ! Mixed air node number in OA mixer
  REAL(r64)           :: AverageUnitMassFlow  ! Average system air mass flow rate over time step [kg/s]
  REAL(r64)           :: AverageOAMassFlow    ! Average outdoor air mass flow rate over time step [kg/s]
  REAL(r64)           :: CpSupplyAir          ! Specific heat of outlet air [J/kg-K]
  REAL(r64)           :: CpZoneAir            ! Specific heat of zone air [J/kg-K]
  REAL(r64)           :: DeltaCpTemp          ! Temperature difference from supply air to zone air [C]
  REAL(r64)           :: ZoneMassFlow         ! Zone mass flow rate required to meet zone load [kg/s]
  REAL(r64)           :: SystemMassFlow       ! System mass flow rate required for all zones [kg/s]
  INTEGER             :: ZoneNum              ! Index to zone
  REAL(r64)           :: ZoneLoad             ! Zone load calculated by ZoneTempPredictor [W]
  REAL(r64)           :: QToHeatSetPt         ! Load to heating setpoint [W]
  REAL(r64)           :: QToCoolSetPt         ! Load to cooling setpoint [W]
  INTEGER             :: ZoneNodeNum          ! Actual zone number
  INTEGER             :: BoxOutletNodeNum     ! Zone supply air inlet node number

  InletNode           = CBVAV(CBVAVNum)%AirInNode
  OutletNode          = CBVAV(CBVAVNum)%AirOutNode
  MixerMixedAirNode   = CBVAV(CBVAVNum)%MixerMixedAirNode
  MixerOutsideAirNode = CBVAV(CBVAVNum)%MixerOutsideAirNode
  MixerReliefAirNode  = CBVAV(CBVAVNum)%MixerReliefAirNode
  MixerInletAirNode   = CBVAV(CBVAVNum)%MixerInletAirNode

  SystemMassFlow      = 0.0d0
  CpSupplyAir         = PsyCpAirFnWTdb(Node(OutletNode)%HumRat,Node(OutletNode)%Temp)
! Determine zone air flow
  DO ZoneNum = 1, CBVAV(CBVAVNum)%NumControlledZones
    ZoneNodeNum       = CBVAV(CBVAVNum)%ActualZoneNodeNum(ZoneNum)
    BoxOutletNodeNum  = CBVAV(CBVAVNum)%CBVAVBoxOutletNode(ZoneNum)
    IF ((CBVAV(CBVAVNum)%ZoneSequenceCoolingNum(ZoneNum) > 0) .AND. (CBVAV(CBVAVNum)%ZoneSequenceHeatingNum(ZoneNum) > 0)) THEN
      QToCoolSetPt = ZoneSysEnergyDemand(CBVAV(CBVAVNum)%ControlledZoneNum(ZoneNum))%&
                  SequencedOutputRequiredToCoolingSP(CBVAV(CBVAVNum)%ZoneSequenceCoolingNum(ZoneNum))
      QToHeatSetPt = ZoneSysEnergyDemand(CBVAV(CBVAVNum)%ControlledZoneNum(ZoneNum))%&
                  SequencedOutputRequiredToHeatingSP(CBVAV(CBVAVNum)%ZoneSequenceHeatingNum(ZoneNum))
      IF (QToHeatSetPt > 0.d0 .AND. QToCoolSetPt > 0.d0) THEN
        ZoneLoad = QToHeatSetPt
      ELSEIF (QToHeatSetPt < 0.d0 .AND. QToCoolSetPt < 0.d0) THEN
        ZoneLoad = QToCoolSetPt
      ELSEIF (QToHeatSetPt <= 0.d0 .AND. QToCoolSetPt >= 0.d0) THEN
        ZoneLoad = 0.d0
      ENDIF
    ELSE
      ZoneLoad     = ZoneSysEnergyDemand(CBVAV(CBVAVNum)%ControlledZoneNum(ZoneNum))%RemainingOutputRequired
      QToHeatSetPt = ZoneSysEnergyDemand(CBVAV(CBVAVNum)%ControlledZoneNum(ZoneNum))%OutputRequiredToHeatingSP
    ENDIF
    CpZoneAir         = PsyCpAirFnWTdb(Node(ZoneNodeNum)%HumRat,Node(ZoneNodeNum)%Temp)
    DeltaCpTemp       = CpSupplyAir*Node(OutletNode)%Temp - CpZoneAir*Node(ZoneNodeNum)%Temp

    !Need to check DeltaCpTemp and ensure that it is not zero
    IF(DeltaCpTemp .NE. 0.0d0) THEN ! .AND. .NOT. CurDeadBandOrSetback(ZoneNum))THEN
      ZoneMassFlow    = ZoneLoad/DeltaCpTemp
    ELSE
!     reset to 0 so we don't add in the last zone's mass flow rate
      ZoneMassFlow    = 0.0d0
    END IF
    SystemMassFlow    = SystemMassFlow + &
                        MAX(Node(BoxOutletNodeNum)%MassFlowRateMin,MIN(ZoneMassFlow,Node(BoxOutletNodeNum)%MassFlowRateMax))
  END DO

  AverageUnitMassFlow      = CompOnMassFlow
  AverageOAMassFlow        = OACompOnMassFlow
  FanSpeedRatio            = CompOnFlowRatio

  Node(MixerInletAirNode) = Node(InletNode)

  Node(MixerMixedAirNode)%MassFlowRateMin          = 0.0d0

  IF (GetCurrentScheduleValue(CBVAV(CBVAVNum)%SchedPtr) .EQ. 0.0d0 .OR. AverageUnitMassFlow .EQ. 0.0d0) THEN
    Node(InletNode)%MassFlowRate                   = 0.0d0
    Node(MixerOutsideAirNode)%MassFlowRate         = 0.0d0
    Node(MixerReliefAirNode)%MassFlowRate          = 0.0d0
    OnOffAirFlowRatio                              = 0.0d0
    ByPassDuctFlowFraction                         = 0.0d0
  ELSE
    Node(MixerInletAirNode)%MassFlowRate           = AverageUnitMassFlow
    Node(MixerOutsideAirNode)%MassFlowRate         = AverageOAMassFlow
    Node(MixerReliefAirNode)%MassFlowRate          = AverageOAMassFlow
    IF (FirstHVACIteration) THEN
      OnOffAirFlowRatio                            = 1.0d0
      ByPassDuctFlowFraction                       = 0.0d0
    ELSE
      OnOffAirFlowRatio                            = 1.0d0
      ByPassDuctFlowFraction = MAX(0.0d0,1.0d0 - (Node(InletNode)%MassFlowRate/AverageUnitMassFlow))
    END IF
  END IF

  RETURN
END SUBROUTINE SetAverageAirFlow

SUBROUTINE ReportCBVAV(CBVAVNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fills some of the report variables for the changeover-bypass VAV system

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: CBVAVNum   ! Index of the current CBVAV unit being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ReportingConstant

          ! FLOW
  ReportingConstant = TimeStepSys*SecInHour

  CBVAV(CBVAVNum)%TotCoolEnergy   = CBVAV(CBVAVNum)%TotCoolEnergyRate  * ReportingConstant
  CBVAV(CBVAVNum)%TotHeatEnergy   = CBVAV(CBVAVNum)%TotHeatEnergyRate  * ReportingConstant
  CBVAV(CBVAVNum)%SensCoolEnergy  = CBVAV(CBVAVNum)%SensCoolEnergyRate * ReportingConstant
  CBVAV(CBVAVNum)%SensHeatEnergy  = CBVAV(CBVAVNum)%SensHeatEnergyRate * ReportingConstant
  CBVAV(CBVAVNum)%LatCoolEnergy   = CBVAV(CBVAVNum)%LatCoolEnergyRate  * ReportingConstant
  CBVAV(CBVAVNum)%LatHeatEnergy   = CBVAV(CBVAVNum)%LatHeatEnergyRate  * ReportingConstant
  CBVAV(CBVAVNum)%ElecConsumption = CBVAV(CBVAVNum)%ElecPower          * ReportingConstant

  RETURN
END SUBROUTINE ReportCBVAV

SUBROUTINE CalcNonDXHeatingCoils(CBVAVNum,FirstHVACIteration,HeatCoilLoad,FanMode,HeatCoilLoadmet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the four non dx heating coil types: Gas, Electric, hot water and steam.

          ! METHODOLOGY EMPLOYED:
          ! Simply calls the different heating coil component.  The hot water flow rate matching the coil load
          ! is calculated iteratively.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE HeatingCoils,              ONLY: SimulateHeatingCoilComponents
  USE WaterCoils,                ONLY: SimulateWaterCoilComponents
  USE SteamCoils,                ONLY: SimulateSteamCoilComponents
  USE PlantUtilities,            ONLY: SetComponentFlowRate
  USE General,                   ONLY: SolveRegulaFalsi,RoundSigDigits
  USE DataHVACGlobals,           ONLY: SmallLoad

  IMPLICIT NONE     ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,      INTENT(IN)    :: CBVAVNum                  ! Changeover bypass VAV unit index
  LOGICAL,      INTENT(IN)    :: FirstHVACIteration        ! flag for first HVAC iteration in the time step
  REAL(r64),    INTENT(INOUT) :: HeatCoilLoad              ! heating coil load to be met (Watts)
  REAL(r64),    INTENT(OUT)   :: HeatCoilLoadmet           ! coil heating load met
  INTEGER,      INTENT(IN)    :: FanMode                   ! fan operation mode

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: ErrTolerance = 0.001d0    ! convergence limit for hotwater coil
  INTEGER, PARAMETER :: SolveMaxIter=50

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: QCoilActual       ! actual heating load met
  REAL(r64)      :: mdot              ! heating coil steam or hot water mass flow rate
  REAL(r64)      :: MinWaterFlow      ! minimum water mass flow rate
  REAL(r64)      :: MaxHotWaterFlow   ! maximum hot water mass flow rate, kg/s
  REAL(r64)      :: HotWaterMdot      ! actual hot water mass flow rate
  REAL(r64), DIMENSION(3) :: Par      !
  INTEGER        :: SolFlag           ! error flag

  QCoilActual=0.0d0
  IF (HeatCoilLoad > SmallLoad) THEN
     Select Case (CBVAV(CBVAVNum)%HeatCoilType_Num)
        Case (Coil_HeatingGas, Coil_HeatingElectric)
          CALL SimulateHeatingCoilComponents(CBVAV(CBVAVNum)%HeatCoilName,FirstHVACIteration, &
                                             HeatCoilLoad, CBVAV(CBVAVNum)%HeatCoilIndex, &
                                             QCoilActual, .TRUE., FanMode)
        Case (Coil_HeatingWater)
          ! simulate the heating coil at maximum hot water flow rate
          MaxHotWaterFlow = CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow
          Call SetComponentFlowRate(MaxHotWaterFlow , &
                                    CBVAV(CBVAVNum)%CoilControlNode, &
                                    CBVAV(CBVAVNum)%CoilOutletNode, &
                                    CBVAV(CBVAVNum)%LoopNum, &
                                    CBVAV(CBVAVNum)%LoopSide, &
                                    CBVAV(CBVAVNum)%BranchNum, &
                                    CBVAV(CBVAVNum)%CompNum)
          CALL SimulateWaterCoilComponents(CBVAV(CBVAVNum)%HeatCoilName,FirstHVACIteration, &
                                           CBVAV(CBVAVNum)%HeatCoilIndex, QCoilActual, FanMode)
          IF ( QCoilActual > (HeatCoilLoad + SmallLoad)) Then
              ! control water flow to obtain output matching HeatCoilLoad
              SolFlag = 0
              MinWaterFlow = 0.0d0
              Par(1) = REAL(CBVAVNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = HeatCoilLoad
              CALL SolveRegulaFalsi(ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, &
                                    MinWaterFlow, MaxHotWaterFlow, Par)
              IF (SolFlag == -1) THEN
                IF (CBVAV(CBVAVNum)%HotWaterCoilMaxIterIndex == 0) THEN
                  CALL ShowWarningMessage('CalcNonDXHeatingCoils: Hot water coil control failed for '//  &
                     trim(CBVAV(CBVAVNum)%UnitType)//'="'//  &
                     TRIM(CBVAV(CBVAVNum)%Name)//'"')
                  CALL ShowContinueErrorTimeStamp(' ')
                  CALL ShowContinueError('  Iteration limit ['//trim(RoundSigDigits(SolveMaxIter))//  &
                      '] exceeded in calculating hot water mass flow rate')
                ENDIF
                CALL ShowRecurringWarningErrorAtEnd('CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit ['//  &
                   trim(RoundSigDigits(SolveMaxIter))//']) for '//trim(CBVAV(CBVAVNum)%UnitType)//'="'// &
                   TRIM(CBVAV(CBVAVNum)%Name),CBVAV(CBVAVNum)%HotWaterCoilMaxIterIndex)
              ELSE IF (SolFlag == -2) THEN
               IF (CBVAV(CBVAVNum)%HotWaterCoilMaxIterIndex2 == 0) THEN
                 CALL ShowWarningMessage('CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for '//  &
                     trim(CBVAV(CBVAVNum)%UnitType)//'="'// &
                     TRIM(CBVAV(CBVAVNum)%Name)//'"')
                 CALL ShowContinueErrorTimeStamp(' ')
                 CALL ShowContinueError('...Bad hot water maximum flow rate limits')
                 CALL ShowContinueError('...Given minimum water flow rate='//trim(RoundSigDigits(MinWaterFlow,3))//' kg/s')
                 CALL ShowContinueError('...Given maximum water flow rate='//trim(RoundSigDigits(MaxHotWaterFlow,3))//' kg/s')
               ENDIF
               CALL ShowRecurringWarningErrorAtEnd('CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for '//  &
                   trim(CBVAV(CBVAVNum)%UnitType)//'="'// &
                   TRIM(CBVAV(CBVAVNum)%Name)//'"', &
                   CBVAV(CBVAVNum)%HotWaterCoilMaxIterIndex2,  &
                   ReportMinOf=MinWaterFlow,ReportMaxOf=MaxHotWaterFlow,ReportMinUnits='[kg/s]',ReportMaxUnits='[kg/s]')
              END IF
              ! simulate the hot water heating coil
              QCoilActual = HeatCoilLoad
              ! simulate the hot water heating coil
              CALL SimulateWaterCoilComponents(CBVAV(CBVAVNum)%HeatCoilName,FirstHVACIteration, &
                                               CBVAV(CBVAVNum)%HeatCoilIndex, QCoilActual, FanMode)
        ENDIF
        Case (Coil_HeatingSteam)
          mdot = CBVAV(CBVAVNum)%MaxHeatCoilFluidFlow
          Call SetComponentFlowRate( mdot , &
                                    CBVAV(CBVAVNum)%CoilControlNode, &
                                    CBVAV(CBVAVNum)%CoilOutletNode, &
                                    CBVAV(CBVAVNum)%LoopNum, &
                                    CBVAV(CBVAVNum)%LoopSide, &
                                    CBVAV(CBVAVNum)%BranchNum, &
                                    CBVAV(CBVAVNum)%CompNum)

          ! simulate the steam heating coil
          CALL SimulateSteamCoilComponents(CBVAV(CBVAVNum)%HeatCoilName, FirstHVACIteration, &
                                           HeatCoilLoad, CBVAV(CBVAVNum)%HeatCoilIndex, &
                                           QCoilActual, FanMode)
     END Select
  ELSE
     Select Case (CBVAV(CBVAVNum)%HeatCoilType_Num)
        Case (Coil_HeatingGas, Coil_HeatingElectric)
          CALL SimulateHeatingCoilComponents(CBVAV(CBVAVNum)%HeatCoilName,FirstHVACIteration, &
                                             HeatCoilLoad, CBVAV(CBVAVNum)%HeatCoilIndex, &
                                             QCoilActual, .TRUE., FanMode)
        Case (Coil_HeatingWater)
          mdot = 0.0d0
          Call SetComponentFlowRate( mdot , &
                                    CBVAV(CBVAVNum)%CoilControlNode, &
                                    CBVAV(CBVAVNum)%CoilOutletNode, &
                                    CBVAV(CBVAVNum)%LoopNum, &
                                    CBVAV(CBVAVNum)%LoopSide, &
                                    CBVAV(CBVAVNum)%BranchNum, &
                                    CBVAV(CBVAVNum)%CompNum)
          QCoilActual = HeatCoilLoad
          ! simulate the hot water heating coil
          CALL SimulateWaterCoilComponents(CBVAV(CBVAVNum)%HeatCoilName,FirstHVACIteration, &
                                           CBVAV(CBVAVNum)%HeatCoilIndex, QCoilActual, FanMode)
        Case (Coil_HeatingSteam)
          mdot = 0.0d0
          Call SetComponentFlowRate( mdot , &
                                    CBVAV(CBVAVNum)%CoilControlNode, &
                                    CBVAV(CBVAVNum)%CoilOutletNode, &
                                    CBVAV(CBVAVNum)%LoopNum, &
                                    CBVAV(CBVAVNum)%LoopSide, &
                                    CBVAV(CBVAVNum)%BranchNum, &
                                    CBVAV(CBVAVNum)%CompNum)
            ! simulate the steam heating coil
          CALL SimulateSteamCoilComponents(CBVAV(CBVAVNum)%HeatCoilName, FirstHVACIteration, &
                                           HeatCoilLoad, CBVAV(CBVAVNum)%HeatCoilIndex, &
                                           QCoilActual, FanMode)
     END Select
  ENDIF
  HeatCoilLoadmet = QCoilActual

 RETURN

END SUBROUTINE CalcNonDXHeatingCoils

FUNCTION HotWaterCoilResidual(HWFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   January 2012
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Actual Coil Output - Requested Coil Load) / Requested Coil Load
          ! the actual coil output depends on the hot water flow rate which is varied to minimize the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls HotWaterCoilResidual, and calculates the residual as defined above.
          !

          ! REFERENCES:

          ! USE STATEMENTS:
  USE WaterCoils,     ONLY: SimulateWaterCoilComponents
  USE PlantUtilities, ONLY: SetComponentFlowRate


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)                         :: HWFlow   ! hot water flow rate in kg/s
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par      ! Par(5) is the requested coil load
  REAL(r64)                                     :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER               :: CBVAVNum
  LOGICAL               :: FirstHVACSoln
  REAL(r64)             :: QCoilActual             ! delivered coild load, W
  REAL(r64)             :: HeatCoilLoad            ! requested coild load, W
  REAL(r64)             :: mdot

  CBVAVNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  HeatCoilLoad =  Par(3)
  QCoilActual = HeatCoilLoad
  mdot = HWFlow
  Call SetComponentFlowRate( mdot , &
                             CBVAV(CBVAVNum)%CoilControlNode, &
                             CBVAV(CBVAVNum)%CoilOutletNode, &
                             CBVAV(CBVAVNum)%LoopNum, &
                             CBVAV(CBVAVNum)%LoopSide, &
                             CBVAV(CBVAVNum)%BranchNum, &
                             CBVAV(CBVAVNum)%CompNum)

    ! simulate the hot water supplemental heating coil
  CALL SimulateWaterCoilComponents(CBVAV(CBVAVNum)%HeatCoilName,FirstHVACSoln, &
                                   CBVAV(CBVAVNum)%HeatCoilIndex, QCoilActual, &
                                   CBVAV(CBVAVNum)%OpMode)
  IF (HeatCoilLoad /= 0.0d0) THEN
    Residuum = (QCoilActual - HeatCoilLoad)/ HeatCoilLoad
  ELSE !Objexx:Return Condition added to assure return value is set
    Residuum = 0.0d0
  ENDIF
  RETURN
END FUNCTION HotWaterCoilResidual

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

END MODULE HVACUnitaryBypassVAV
