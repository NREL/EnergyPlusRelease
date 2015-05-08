MODULE LowTempRadiantSystem

  ! Module containing the routines dealing with the low temperature radiant systems

  ! MODULE INFORMATION:
  !       AUTHOR         Rick Strand
  !       DATE WRITTEN   November 2000
  !       MODIFIED       Rick Strand March 2001 (additional controls, etc.)
  !                      Rick Strand July 2003 (added constant flow hydronic system)
  !                      B. Griffith Sept 2010, plant upgrades, generalize fluid properties
  !                      Rick Strand August 2011 (improved condensation handling)
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! The purpose of this module is to simulate low temperature radiant systems.
  ! It is the intention of this module to cover all types of low temperature
  ! radiant systems: wall, ceiling, floor, heating, cooling, panels, etc.

  ! METHODOLOGY EMPLOYED:
  ! Based on work done in IBLAST, this model has been revised for the structure
  ! of EnergyPlus.  It is still based on the QTF formulation of heat transfer
  ! through building elements with embedded heat sources/sinks.  Note that due
  ! to the fact that a radiant system is both a building heat transfer element
  ! and a controllable system that some iteration between the system and the
  ! surface heat balance routine is necessary.
  ! REFERENCES:
  ! IBLAST-QTF research program, completed in January 1995 (unreleased)
  ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
  !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
  !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
  !   Engineering.
  ! Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
  !   of Wisconsin-Madison.

  ! OTHER NOTES: This module contains three different types of radiant system
  ! models: (a) variable flow hydronic heating/cooling radiant system;
  ! (b) constant flow, variable controlled temperature heating/cooling radiant
  ! system; (c) electric resistance heating radiant system.  Systems (a) and
  ! (b) are hydronic systems--one which varies hydronic flow as the key control
  ! paramter (a) and one which varies the inlet hydronic temperature while
  ! keeping the flow rate through the radiant system constant (b).  In system
  ! (b), the injection rate from the main water loop is varied to obtain the
  ! proper inlet temperature.

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals,       ONLY : MaxNameLength, BeginTimeStepFlag, InitConvTemp, SysSizingCalc, WarmUpFlag, DisplayExtraWarnings
USE DataInterfaces

USE DataHeatBalance,   ONLY : Material, TotMaterials,MaxLayersInConstruct,   &
                           QRadThermInAbs,Construct, TotConstructs,  RegularMaterial,Air

USE DataSurfaces,      ONLY : Surface, TotSurfaces, HeatTransferModel_CTF
USE DataHeatBalFanSys, ONLY : QRadSysSource, TCondFDSourceNode  ! Heat source/sink value & temperature for CondFD algo.
USE DataHVACGlobals,   ONLY : SmallLoad

  ! Use statements for access to subroutines in other modules
USE Psychrometrics,    ONLY :PsyTdpFnWPb

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:
  ! System types:
INTEGER, PARAMETER :: HydronicSystem     = 1 ! Variable flow hydronic radiant system
INTEGER, PARAMETER :: ConstantFlowSystem = 2 ! Constant flow, variable (controlled) temperature radiant system
INTEGER, PARAMETER :: ElectricSystem     = 3 ! Electric resistance radiant heating system
CHARACTER(len=*), PARAMETER :: cHydronicSystem='ZoneHVAC:LowTemperatureRadiant:VariableFlow'
CHARACTER(len=*), PARAMETER :: cConstantFlowSystem='ZoneHVAC:LowTemperatureRadiant:ConstantFlow'
CHARACTER(len=*), PARAMETER :: cElectricSystem='ZoneHVAC:LowTemperatureRadiant:Electric'
  ! Operating modes:
INTEGER, PARAMETER :: NotOperating = 0  ! Parameter for use with OperatingMode variable, set for heating
INTEGER, PARAMETER :: HeatingMode = 1   ! Parameter for use with OperatingMode variable, set for heating
INTEGER, PARAMETER :: CoolingMode = 2   ! Parameter for use with OperatingMode variable, set for cooling
  ! Control types:
INTEGER, PARAMETER :: MATControl       = 1  ! Controls system using mean air temperature
INTEGER, PARAMETER :: MRTControl       = 2  ! Controls system using mean radiant temperature
INTEGER, PARAMETER :: OperativeControl = 3  ! Controls system using operative temperature
INTEGER, PARAMETER :: ODBControl       = 4  ! Controls system using outside air dry-bulb temperature
INTEGER, PARAMETER :: OWBControl       = 5  ! Controls system using outside air wet-bulb temperature
  ! Condensation control types:
INTEGER, PARAMETER :: CondCtrlNone      = 0 ! Condensation control--none, so system never shuts down
INTEGER, PARAMETER :: CondCtrlSimpleOff = 1 ! Condensation control--simple off, system shuts off when condensation predicted
INTEGER, PARAMETER :: CondCtrlVariedOff = 2 ! Condensation control--variable off, system modulates to keep running if possible
  ! Number of Circuits per Surface Calculation Method
INTEGER, PARAMETER :: OneCircuit             = 1 ! there is 1 circuit per surface
INTEGER, PARAMETER :: CalculateFromLength    = 2 ! The number of circuits is TubeLength*SurfaceFlowFrac / CircuitLength
CHARACTER(len=*), PARAMETER :: OnePerSurf = 'OnePerSurface'
CHARACTER(len=*), PARAMETER :: CalcFromLength = 'CalculateFromCircuitLength'
  ! Limit temperatures to indicate that a system cannot heat or cannot cool
REAL(r64) :: LowTempHeating  = -200.0d0 ! Used to indicate that a user does not have a heating control temperature
REAL(r64) :: HighTempCooling =  200.0d0 ! Used to indicate that a user does not have a cooling control temperature

  ! DERIVED TYPE DEFINITIONS:
TYPE, PUBLIC :: HydronicRadiantSystemData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name              = ' ' ! name of hydronic radiant system
  CHARACTER(len=MaxNameLength) :: SchedName         = ' ' ! availability schedule
  INTEGER                      :: SchedPtr          = 0   ! index to schedule
  CHARACTER(len=MaxNameLength) :: ZoneName          = ' ' ! Name of zone the system is serving
  INTEGER                      :: ZonePtr           = 0   ! Point to this zone in the Zone derived type
  CHARACTER(len=MaxNameLength) :: SurfListName      = ' ' ! Name of surface/surface list that is the radiant system
  INTEGER                      :: NumOfSurfaces     = 0   ! Number of surfaces included in this radiant system (coordinated control)
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: SurfacePtr          ! Pointer to the surface(s) in the Surface derived type
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SurfaceName         ! Name of surfaces that are the radiant system (can be one or more)
  REAL(r64), ALLOCATABLE, DIMENSION(:)                    :: SurfaceFlowFrac     ! Fraction of flow/pipe length for a particular surface
  REAL(r64), ALLOCATABLE, DIMENSION(:)                    :: NumCircuits         ! Number of fluid circuits in the surface
  REAL(r64)                    :: TotalSurfaceArea  = 0.0d0 ! Total surface area for all surfaces that are part of this radiant system
  REAL(r64)                    :: TubeDiameter      = 0.0d0 ! tube diameter for embedded tubing
  REAL(r64)                    :: TubeLength        = 0.0d0 ! tube length embedded in radiant surface
  INTEGER                      :: ControlType       = 0   ! Control type for the system (MAT, MRT, Op temp, ODB, OWB)
  LOGICAL                      :: HeatingSystem = .false. ! .TRUE. when the system is able to heat (parameters are valid)
  REAL(r64)                    :: WaterVolFlowMaxHeat=0.0d0 ! maximum water flow rate for heating, m3/s
  REAL(r64)                    :: WaterFlowMaxHeat  = 0.0d0 ! maximum water flow rate for heating, kg/s
  INTEGER                      :: HotWaterInNode    = 0   ! hot water inlet node
  INTEGER                      :: HotWaterOutNode   = 0   ! hot water outlet node
  REAL(r64)                    :: HotThrottlRange   = 0.0d0 ! Throttling range for heating [C]
  CHARACTER(len=MaxNameLength) :: HotSetptSched     = ' ' ! Schedule name for the zone setpoint temperature
  INTEGER                      :: HotSetptSchedPtr  = 0   ! Schedule index for the zone setpoint temperature
  INTEGER                      :: HWLoopNum         = 0
  INTEGER                      :: HWLoopSide        = 0
  INTEGER                      :: HWBranchNum       = 0
  INTEGER                      :: HWCompNum         = 0
  REAL(r64)                    :: WaterVolFlowMaxCool=0.0d0 ! maximum water flow rate for cooling, m3/s
  REAL(r64)                    :: WaterFlowMaxCool  = 0.0d0 ! maximum water flow rate for cooling, kg/s
  LOGICAL                      :: CoolingSystem =.false.  ! .TRUE. when the system is able to cool (parameters are valid)
  INTEGER                      :: ColdWaterInNode   = 0   ! cold water inlet node
  INTEGER                      :: ColdWaterOutNode  = 0   ! cold water outlet node
  REAL(r64)                    :: ColdThrottlRange  = 0.0d0 ! Throttling range for cooling [C]
  CHARACTER(len=MaxNameLength) :: ColdSetptSched    = ' ' ! Schedule name for the zone setpoint temperature
  INTEGER                      :: ColdSetptSchedPtr = 0   ! Schedule index for the zone setpoint temperature
  INTEGER                      :: CWLoopNum         = 0
  INTEGER                      :: CWLoopSide        = 0
  INTEGER                      :: CWBranchNum       = 0
  INTEGER                      :: CWCompNum         = 0
  INTEGER                      :: GlycolIndex       = 0   ! Index to Glycol (Water) Properties
  INTEGER                      :: CondErrIndex      = 0   ! Error index for recurring warning messages
  INTEGER                      :: CondCtrlType      = 1   ! Condensation control type (initialize to simple off)
  REAL(r64)                    :: CondDewPtDeltaT   = 1.0d0 ! Diff between surface temperature and dew point for cond. shut-off
  REAL(r64)                    :: CondCausedTimeOff = 0.0d0 ! Amount of time condensation did or could have turned system off
  LOGICAL                      :: CondCausedShutDown = .FALSE. ! .TRUE. when condensation predicted at surface
  INTEGER                      :: NumCircCalcMethod = 0   ! Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
  REAL(r64)                    :: CircLength        = 0.0d0 ! Circuit length {m}
  ! Other parameters
  LOGICAL                      :: EMSOverrideOnWaterMdot = .FALSE.
  REAL(r64)                    :: EMSWaterMdotOverrideValue = 0.0D0

  ! Report data
  REAL(r64)                    :: WaterInletTemp    = 0.0d0 ! water inlet temperature
  REAL(r64)                    :: WaterOutletTemp   = 0.0d0 ! water outlet temperature
  REAL(r64)                    :: WaterMassFlowRate = 0.0d0 ! water mass flow rate
  REAL(r64)                    :: HeatPower         = 0.0d0 ! heating sent to panel in Watts
  REAL(r64)                    :: HeatEnergy        = 0.0d0 ! heating sent to panel in Joules
  REAL(r64)                    :: CoolPower         = 0.0d0 ! cooling sent to panel in Watts
  REAL(r64)                    :: CoolEnergy        = 0.0d0 ! cooling sent to panel in Joules
  INTEGER                      :: OutRangeHiErrorCount = 0 ! recurring errors for crazy results too high fluid temperature
  INTEGER                      :: OutRangeLoErrorCount = 0 ! recurring errors for crazy results too low fluid temperature
END TYPE HydronicRadiantSystemData

TYPE, PUBLIC :: ConstantFlowRadiantSystemData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name              = ' ' ! name of hydronic radiant system
  CHARACTER(len=MaxNameLength) :: SchedName         = ' ' ! availability schedule
  INTEGER                      :: SchedPtr          = 0   ! index to schedule
  CHARACTER(len=MaxNameLength) :: ZoneName          = ' ' ! Name of zone the system is serving
  INTEGER                      :: ZonePtr           = 0   ! Point to this zone in the Zone derived type
  CHARACTER(len=MaxNameLength) :: SurfListName      = ' ' ! Name of surface/surface list that is the radiant system
  INTEGER                      :: NumOfSurfaces     = 0   ! Number of surfaces included in this radiant system (coordinated control)
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: SurfacePtr          ! Pointer to the surface(s) in the Surface derived type
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SurfaceName         ! Name of surfaces that are the radiant system (can be one or more)
  REAL(r64), ALLOCATABLE, DIMENSION(:)                    :: SurfaceFlowFrac     ! Fraction of flow/pipe length for a particular surface
  REAL(r64), ALLOCATABLE, DIMENSION(:)                    :: NumCircuits         ! Number of fluid circuits in the surface
  REAL(r64)                    :: TotalSurfaceArea  = 0.0d0 ! Total surface area for all surfaces that are part of this radiant system
  REAL(r64)                    :: TubeDiameter      = 0.0d0 ! tube diameter for embedded tubing
  REAL(r64)                    :: TubeLength        = 0.0d0 ! tube length embedded in radiant surface
  INTEGER                      :: ControlType       = 0   ! Control type for the system (MAT, MRT, Op temp, ODB, OWB)
  REAL(r64)                    :: WaterVolFlowMax   = 0.0d0 ! design nominal capacity of constant flow pump (volumetric flow rate)
  REAL(r64)                    :: ColdDesignWaterMassFlowRate =0.d0
  REAL(r64)                    :: HotDesignWaterMassFlowRate =0.d0

  REAL(r64)                    :: WaterMassFlowRate = 0.0d0 ! current flow rate through system (calculated)
  CHARACTER(len=MaxNameLength) :: VolFlowSched      = ' ' ! schedule of maximum flow at the current time
  INTEGER                      :: VolFlowSchedPtr   = 0   ! index to the volumetric flow schedule
  REAL(r64)                    :: NomPumpHead       = 0.0d0 ! nominal head of the constant flow pump
  REAL(r64)                    :: NomPowerUse       = 0.0d0 ! nominal power use of the constant flow pump
  REAL(r64)                    :: MotorEffic        = 0.0d0 ! efficiency of the pump motor
  REAL(r64)                    :: PumpEffic         = 0.0d0 ! overall efficiency of the pump (calculated)
  REAL(r64)                    :: FracMotorLossToFluid = 0.0d0 ! amount of heat generated by pump motor that is added to the fluid
  LOGICAL                      :: HeatingSystem = .false. ! .TRUE. when the system is able to heat (parameters are valid)
  INTEGER                      :: HotWaterInNode    = 0   ! hot water inlet node
  INTEGER                      :: HotWaterOutNode   = 0   ! hot water outlet node
  CHARACTER(len=MaxNameLength) :: HotWaterHiTempSched    = ' ' ! Schedule name for the highest water temperature
  INTEGER                      :: HotWaterHiTempSchedPtr = 0   ! Schedule index for the highest water temperature
  CHARACTER(len=MaxNameLength) :: HotWaterLoTempSched    = ' ' ! Schedule name for the lowest water temperature
  INTEGER                      :: HotWaterLoTempSchedPtr = 0   ! Schedule index for the lowest water temperature
  CHARACTER(len=MaxNameLength) :: HotCtrlHiTempSched     = ' ' ! Schedule name for the highest control temperature
                                                               ! (where the lowest water temperature is requested)
  INTEGER                      :: HotCtrlHiTempSchedPtr  = 0   ! Schedule index for the highest control temperature
                                                               ! (where the lowest water temperature is requested)
  CHARACTER(len=MaxNameLength) :: HotCtrlLoTempSched     = ' ' ! Schedule name for the lowest control temperature
                                                               ! (where the highest water temperature is requested)
  INTEGER                      :: HotCtrlLoTempSchedPtr  = 0   ! Schedule index for the lowest control temperature
                                                               ! (where the highest water temperature is requested)
  INTEGER                      :: HWLoopNum         = 0
  INTEGER                      :: HWLoopSide        = 0
  INTEGER                      :: HWBranchNum       = 0
  INTEGER                      :: HWCompNum         = 0
  LOGICAL                      :: CoolingSystem =.false.  ! .TRUE. when the system is able to cool (parameters are valid)
  INTEGER                      :: ColdWaterInNode   = 0   ! cold water inlet node
  INTEGER                      :: ColdWaterOutNode  = 0   ! cold water outlet node
  CHARACTER(len=MaxNameLength) :: ColdWaterHiTempSched    = ' ' ! Schedule name for the highest water temperature
  INTEGER                      :: ColdWaterHiTempSchedPtr = 0   ! Schedule index for the highest water temperature
  CHARACTER(len=MaxNameLength) :: ColdWaterLoTempSched    = ' ' ! Schedule name for the lowest water temperature
  INTEGER                      :: ColdWaterLoTempSchedPtr = 0   ! Schedule index for the lowest water temperature
  CHARACTER(len=MaxNameLength) :: ColdCtrlHiTempSched     = ' ' ! Schedule name for the highest control temperature
                                                               ! (where the lowest water temperature is requested)
  INTEGER                      :: ColdCtrlHiTempSchedPtr  = 0   ! Schedule index for the highest control temperature
                                                               ! (where the lowest water temperature is requested)
  CHARACTER(len=MaxNameLength) :: ColdCtrlLoTempSched     = ' ' ! Schedule name for the lowest control temperature
                                                               ! (where the highest water temperature is requested)
  INTEGER                      :: ColdCtrlLoTempSchedPtr  = 0   ! Schedule index for the lowest control temperature
                                                               ! (where the highest water temperature is requested)
  INTEGER                      :: CWLoopNum         = 0
  INTEGER                      :: CWLoopSide        = 0
  INTEGER                      :: CWBranchNum       = 0
  INTEGER                      :: CWCompNum         = 0
  INTEGER                      :: GlycolIndex       = 0   ! Index to Glycol (Water) Properties
  INTEGER                      :: CondErrIndex      = 0   ! Error index for warning messages
  INTEGER                      :: CondCtrlType      = 1   ! Condensation control type (initialize to simple off)
  REAL(r64)                    :: CondDewPtDeltaT   = 1.0d0 ! Diff between surface temperature and dew point for cond. shut-off
  REAL(r64)                    :: CondCausedTimeOff = 0.0d0 ! Amount of time condensation did or could have turned system off
  LOGICAL                      :: CondCausedShutDown = .FALSE. ! .TRUE. when condensation predicted at surface
  INTEGER                      :: NumCircCalcMethod = 0   ! Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
  REAL(r64)                    :: CircLength        = 0.0d0 ! Circuit length {m}
  ! Other parameters
  LOGICAL                      :: EMSOverrideOnWaterMdot = .FALSE.
  REAL(r64)                    :: EMSWaterMdotOverrideValue = 0.0D0

  ! Report data
  REAL(r64)                    :: WaterInletTemp     = 0.0d0 ! water inlet temperature
  REAL(r64)                    :: WaterOutletTemp    = 0.0d0 ! water outlet temperature
  REAL(r64)                    :: WaterInjectionRate = 0.0d0 ! water injection mass flow rate from main loop
  REAL(r64)                    :: WaterRecircRate    = 0.0d0 ! water recirculation rate (outlet from radiant system recirculated)
  REAL(r64)                    :: HeatPower          = 0.0d0 ! heating sent to panel in Watts
  REAL(r64)                    :: HeatEnergy         = 0.0d0 ! heating sent to panel in Joules
  REAL(r64)                    :: CoolPower          = 0.0d0 ! cooling sent to panel in Watts
  REAL(r64)                    :: CoolEnergy         = 0.0d0 ! cooling sent to panel in Joules
  REAL(r64)                    :: PumpPower          = 0.0d0 ! pump power in Watts
  REAL(r64)                    :: PumpEnergy         = 0.0d0 ! pump energy consumption in Joules
  REAL(r64)                    :: PumpMassFlowRate   = 0.0d0 ! mass flow rate through the radiant system in kg/sec
  REAL(r64)                    :: PumpHeattoFluid    = 0.0d0 ! heat transfer rate from pump motor to fluid in Watts
  REAL(r64)                    :: PumpHeattoFluidEnergy = 0.0d0  ! Pump Energy dissipated into fluid stream in Joules
  REAL(r64)                    :: PumpInletTemp      = 0.0d0 ! inlet temperature of pump (inlet temperature from loop)
  INTEGER                      :: OutRangeHiErrorCount = 0 ! recurring errors for crazy results too high fluid temperature
  INTEGER                      :: OutRangeLoErrorCount = 0 ! recurring errors for crazy results too low fluid temperature
END TYPE ConstantFlowRadiantSystemData

TYPE,PUBLIC :: ElectricRadiantSystemData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name              = ' ' ! name of hydronic radiant system
  CHARACTER(len=MaxNameLength) :: SchedName         = ' ' ! availability schedule
  INTEGER                      :: SchedPtr          = 0   ! index to schedule
  CHARACTER(len=MaxNameLength) :: ZoneName          = ' ' ! Name of zone the system is serving
  INTEGER                      :: ZonePtr           = 0   ! Point to this zone in the Zone derived type
  CHARACTER(len=MaxNameLength) :: SurfListName      = ' ' ! Name of surface/surface list that is the radiant system
  INTEGER                      :: NumOfSurfaces     = 0   ! Number of surfaces included in this radiant system (coordinated control)
  INTEGER, ALLOCATABLE,   &
                  DIMENSION(:) :: SurfacePtr              ! Pointer to the surface(s) in the Surface derived type
  CHARACTER(len=MaxNameLength), &
     ALLOCATABLE, DIMENSION(:) :: SurfaceName             ! Name of surfaces that are the radiant system (can be one or more)
  REAL(r64),    ALLOCATABLE,   &
                  DIMENSION(:) :: SurfacePowerFrac        ! Fraction of total power input to surface
  REAL(r64)                    :: TotalSurfaceArea  = 0.0d0 ! Total surface area for all surfaces that are part of this radiant system
  REAL(r64)                    :: MaxElecPower      = 0.0d0 ! Maximum electric power that can be supplied to surface, Watts
  INTEGER                      :: ControlType       = 0   ! Control type for the system (MAT, MRT, Op temp, ODB, OWB)
  REAL(r64)                    :: ThrottlRange      = 0.0d0 ! Throttling range for heating [C]
  CHARACTER(len=MaxNameLength) :: SetptSched        = ' ' ! Schedule name for the zone setpoint temperature
  INTEGER                      :: SetptSchedPtr     = 0   ! Schedule index for the zone setpoint temperature
  ! Other parameters
  ! Report data
  REAL(r64)                    :: ElecPower         = 0.0d0 ! heating sent to panel in Watts
  REAL(r64)                    :: ElecEnergy        = 0.0d0 ! heating sent to panel in Joules
  REAL(r64)                    :: HeatPower         = 0.0d0 ! heating sent to panel in Watts (same as ElecPower)
  REAL(r64)                    :: HeatEnergy        = 0.0d0 ! heating sent to panel in Joules (same as ElecEnergy)
END TYPE ElectricRadiantSystemData

TYPE RadSysTypeData
  ! This type used to track different components/types for efficiency
  CHARACTER(len=MaxNameLength) :: Name              = ' ' ! name of radiant system
  INTEGER                      :: SystemType        = 0   ! Type of System (see System Types in Parameters)
  INTEGER                      :: CompIndex         = 0   ! Index in specific system types
END TYPE

TYPE(HydronicRadiantSystemData),     DIMENSION(:), PUBLIC, ALLOCATABLE :: HydrRadSys
TYPE(ConstantFlowRadiantSystemData), DIMENSION(:), PUBLIC, ALLOCATABLE :: CFloRadSys
TYPE(ElectricRadiantSystemData),     DIMENSION(:), PUBLIC, ALLOCATABLE :: ElecRadSys
TYPE(RadSysTypeData),                DIMENSION(:), ALLOCATABLE :: RadSysTypes

  ! MODULE VARIABLE DECLARATIONS:
  ! Standard, run-of-the-mill variables...
INTEGER, PUBLIC :: NumOfHydrLowTempRadSys =0 ! Number of hydronic low tempererature radiant systems
INTEGER, PUBLIC :: NumOfCFloLowTempRadSys =0 ! Number of constant flow (hydronic) low tempererature radiant systems
INTEGER, PUBLIC :: NumOfElecLowTempRadSys =0 ! Number of electric low tempererature radiant systems
INTEGER :: CFloCondIterNum        =0 ! Number of iterations for a constant flow radiant system--controls variable cond sys ctrl
INTEGER :: TotalNumOfRadSystems   =0 ! Total number of low temperature radiant systems
INTEGER :: OperatingMode          =0 ! Used to keep track of whether system is in heating or cooling mode
INTEGER :: MaxCloNumOfSurfaces    =0 ! Used to set allocate size in CalcClo routine
LOGICAL :: VarOffCond = .FALSE.      ! Set to true when in cooling for constant flow system + variable off condensation predicted
REAL(r64) :: LoopReqTemp = 0.0d0       ! Temperature required at the inlet of the pump (from the loop) to meet control logic
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSysSrcAvg ! Average source over the time step for a particular radiant surface
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZeroSourceSumHATsurf ! Equal to SumHATsurf for all the walls in a zone with no source
  ! Record keeping variables used to calculate QRadSysSrcAvg locally
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastQRadSysSrc       ! Need to keep the last value in case we are still iterating
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastSysTimeElapsed   ! Need to keep the last value in case we are still iterating
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastTimeStepSys      ! Need to keep the last value in case we are still iterating
  ! Autosizing variables
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlagHydr
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlagCFlo
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlagElec
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


  ! SUBROUTINE SPECIFICATIONS FOR MODULE LowTempRadiantSystem
PUBLIC  SimLowTempRadiantSystem
PRIVATE GetLowTempRadiantSystem
PRIVATE InitLowTempRadiantSystem
PRIVATE SizeLowTempRadiantSystem
PRIVATE CalcLowTempHydrRadiantSystem
PUBLIC  CalcLowTempHydrRadSysComps
PRIVATE CalcLowTempCFloRadiantSystem
PUBLIC  CalcLowTempCFloRadSysComps
PRIVATE CalcLowTempElecRadiantSystem
PRIVATE UpdateLowTempRadiantSystem
PRIVATE CalcRadSysHXEffectTerm
PUBLIC  UpdateRadSysSourceValAvg
PRIVATE SumHATsurf
PRIVATE ReportLowTempRadiantSystem

CONTAINS

SUBROUTINE SimLowTempRadiantSystem(CompName,FirstHVACIteration,LoadMet,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName            ! name of the low temperature radiant system
  LOGICAL,          INTENT(IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),             INTENT(INOUT) :: LoadMet             ! load met by the radiant system, in Watts
  INTEGER,          INTENT(INOUT) :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: GetInputFlag = .TRUE.  ! First time, input is "gotten"
  INTEGER       :: RadSysNum              ! Radiant system number/index in local derived types
  INTEGER       :: SystemType             ! Type of radiant system: hydronic, constant flow, or electric

          ! FLOW:
  IF (GetInputFlag) THEN
    CALL GetLowTempRadiantSystem
    GetInputFlag=.FALSE.
  ENDIF

!          ! Get the radiant system index and type
!  RadSysNum = FindItemInList(CompName,HydrRadSys%Name,NumOfHydrLowTempRadSys)
!  IF (RadSysNum > 0) THEN  ! Found it and it is a hydronic system
!    SystemType = HydronicSystem
!  ELSE ! RadSysNum <= 0 so this CompName was not found among the hydronic systems-->check out electric systems
!    RadSysNum = FindItemInList(CompName,ElecRadSys%Name,NumOfElecLowTempRadSys)
!    IF (RadSysNum > 0) THEN  ! Found it and it is an electric system
!      SystemType = ElectricSystem
!    ELSE    ! RadSysNum <= 0 so this CompName was not found among the hydronic systems-->check out constant flow systems
!      RadSysNum = FindItemInList(CompName,CFloRadSys%Name,NumOfCFloLowTempRadSys)
!      IF (RadSysNum > 0) THEN  ! Found it and it is an electric system
!        SystemType = ConstantFlowSystem
!      ELSE  ! RadSysNum is still <= 0 so this CompName was not found among either radiant system type-->error
!        CALL ShowFatalError('SimLowTempRadiantSystem: Radiant System not found = '//TRIM(CompName))
!      END IF
!    END IF
!  END IF

  ! Find the correct High Temp Radiant System
  IF (CompIndex == 0) THEN
    RadSysNum = FindItemInList(CompName,RadSysTypes%Name,TotalNumOfRadSystems)
    IF (RadSysNum == 0) THEN
      CALL ShowFatalError('SimLowTempRadiantSystem: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=RadSysNum
    SystemType=RadSysTypes(RadSysNum)%SystemType
    SELECT CASE (SystemType)
      CASE (HydronicSystem)
        RadSysTypes(RadSysNum)%CompIndex=FindItemInList(CompName,HydrRadSys%Name,NumOfHydrLowTempRadSys)
      CASE (ConstantFlowSystem)
        RadSysTypes(RadSysNum)%CompIndex=FindItemInList(CompName,CFloRadSys%Name,NumOfCFloLowTempRadSys)
      CASE (ElectricSystem)
        RadSysTypes(RadSysNum)%CompIndex=FindItemInList(CompName,ElecRadSys%Name,NumOfElecLowTempRadSys)
    END SELECT
  ELSE
    RadSysNum=CompIndex
    SystemType=RadSysTypes(RadSysNum)%SystemType
    IF (RadSysNum > TotalNumOfRadSystems .or. RadSysNum < 1) THEN
      CALL ShowFatalError('SimLowTempRadiantSystem:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(RadSysNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(TotalNumOfRadSystems))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(RadSysNum)) THEN
      IF (CompName /= RadSysTypes(RadSysNum)%Name) THEN
        CALL ShowFatalError('SimLowTempRadiantSystem: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(RadSysNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(RadSysTypes(RadSysNum)%Name))
      ENDIF
      CheckEquipName(RadSysNum)=.false.
    ENDIF
  ENDIF

  CALL InitLowTempRadiantSystem(FirstHVACIteration,RadSysTypes(RadSysNum)%CompIndex,SystemType)

  SELECT CASE (SystemType)
    CASE (HydronicSystem)
      CALL CalcLowTempHydrRadiantSystem(RadSysTypes(RadSysNum)%CompIndex,LoadMet)
    CASE (ConstantFlowSystem)
      CALL CalcLowTempCFloRadiantSystem(RadSysTypes(RadSysNum)%CompIndex,LoadMet)
    CASE (ElectricSystem)
      CALL CalcLowTempElecRadiantSystem(RadSysTypes(RadSysNum)%CompIndex,LoadMet)
    CASE DEFAULT
      CALL ShowFatalError('SimLowTempRadiantSystem: Illegal system type for system '//TRIM(CompName))
  END SELECT

  CALL UpdateLowTempRadiantSystem(FirstHVACIteration,RadSysTypes(RadSysNum)%CompIndex,SystemType)

  CALL ReportLowTempRadiantSystem(RadSysTypes(RadSysNum)%CompIndex,SystemType)

  RETURN

END SUBROUTINE SimLowTempRadiantSystem


SUBROUTINE GetLowTempRadiantSystem

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       August 2003 (added constant flow system, made input extensible)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the input for low temperature radiant systems
          ! from the user input file.  This will contain all of the information
          ! needed to simulate a low temperature radiant system.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE BranchNodeConnections, ONLY : TestCompSet
  USE DataGlobals,        ONLY : NumOfZones, AnyEnergyManagementSystemInModel, ScheduleAlwaysOn
  USE DataInterfaces,     ONLY : SetupEMSInternalVariable,SetupEMSActuator
  USE DataHeatBalance,    ONLY : Zone, Construct
  USE DataSizing,         ONLY : AutoSize
  USE FluidProperties,    ONLY : FindGlycol
  USE InputProcessor,     ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList, SameString, GetObjectDefMaxArgs, VerifyName
  USE NodeInputManager,   ONLY : GetOnlySingleNode
  USE ScheduleManager,    ONLY : GetScheduleIndex
  USE DataLoopNode
  USE DataSurfaceLists

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
!  REAL(r64),        PARAMETER :: FlowFractionTolerance = 0.0001 ! Smallest deviation from unity for the sum of all fractions
  REAL(r64),        PARAMETER :: MinThrottlingRange = 0.5d0   ! Smallest throttling range allowed in degrees Celsius
  CHARACTER(len=*), PARAMETER :: MeanAirTemperature = 'MeanAirTemperature'
  CHARACTER(len=*), PARAMETER :: MeanRadiantTemperature = 'MeanRadiantTemperature'
  CHARACTER(len=*), PARAMETER :: OperativeTemperature = 'OperativeTemperature'
  CHARACTER(len=*), PARAMETER :: OutsideAirDryBulbTemperature = 'OutdoorDryBulbTemperature'
  CHARACTER(len=*), PARAMETER :: OutsideAirWetBulbTemperature = 'OutdoorWetBulbTemperature'
  CHARACTER(len=*), PARAMETER :: RoutineName='GetLowTempRadiantSystem: ' ! include trailing blank space
  CHARACTER(len=*), PARAMETER :: Off = 'Off'
  CHARACTER(len=*), PARAMETER :: SimpleOff = 'SimpleOff'
  CHARACTER(len=*), PARAMETER :: VariableOff = 'VariableOff'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CurrentModuleObject  ! for ease in getting objects
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  LOGICAL, ALLOCATABLE, &
                     DIMENSION(:) :: AssignedAsRadiantSurface    ! Set to true when a surface is part of a radiant system
  INTEGER                         :: CheckSurfNum  ! Surface number to check to see if it has already been used by a radiant system
  LOGICAL                         :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  INTEGER                         :: GlycolIndex ! Index of 'Water' in glycol data structure
  INTEGER                         :: IOStatus   ! Used in GetObjectItem
  INTEGER                         :: Item       ! Item to be "gotten"
  INTEGER                         :: MaxAlphas  ! Maximum number of alphas for these input keywords
  INTEGER                         :: MaxNumbers ! Maximum number of numbers for these input keywords
!unused1208  INTEGER    :: NameConflict ! Used to see if a surface name matches the name of a surface list (not allowed)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers    ! Numeric items for object
  INTEGER                         :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                         :: NumArgs    ! Unused variable that is part of a subroutine call
  INTEGER                         :: NumNumbers ! Number of Numbers for each GetObjectItem call
!unused1208  REAL(r64)  :: SumOfAllFractions   ! Summation of all of the fractions for splitting flow (must sum to 1)
  INTEGER                         :: SurfListNum ! Index within the SurfList derived type for a surface list name
  INTEGER                         :: SurfNum    ! DO loop counter for surfaces
!unused1208  INTEGER    :: ZoneForSurface  ! Zone number that a particular surface is attached to
  INTEGER                         :: BaseNum    ! Temporary number for creating RadiantSystemTypes structure
  LOGICAL                            :: IsNotOK ! Flag to verify name
  LOGICAL                            :: IsBlank ! Flag for blank name
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.


          ! FLOW:
          ! Initializations and allocations
  MaxAlphas=0
  MaxNumbers=0

  CALL GetObjectDefMaxArgs('ZoneHVAC:LowTemperatureRadiant:VariableFlow',NumArgs,NumAlphas,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)

  CALL GetObjectDefMaxArgs('ZoneHVAC:LowTemperatureRadiant:ConstantFlow',NumArgs,NumAlphas,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)

  CALL GetObjectDefMaxArgs('ZoneHVAC:LowTemperatureRadiant:Electric',NumArgs,NumAlphas,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)

  ALLOCATE(Alphas(MaxAlphas))
  Alphas=' '
  ALLOCATE(Numbers(MaxNumbers))
  Numbers=0.0d0
  ALLOCATE(cAlphaFields(MaxAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(MaxNumbers))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(MaxAlphas))
  lAlphaBlanks=.TRUE.
  ALLOCATE(lNumericBlanks(MaxNumbers))
  lNumericBlanks=.TRUE.

  NumOfHydrLowTempRadSys = GetNumObjectsFound('ZoneHVAC:LowTemperatureRadiant:VariableFlow')
  NumOfCFloLowTempRadSys = GetNumObjectsFound('ZoneHVAC:LowTemperatureRadiant:ConstantFlow')
  NumOfElecLowTempRadSys = GetNumObjectsFound('ZoneHVAC:LowTemperatureRadiant:Electric')

  TotalNumOfRadSystems=NumOfHydrLowTempRadSys+NumOfElecLowTempRadSys+NumOfCFloLowTempRadSys
  ALLOCATE(RadSysTypes(TotalNumOfRadSystems))
  ALLOCATE(CheckEquipName(TotalNumOfRadSystems))
  CheckEquipName=.true.

  ALLOCATE(HydrRadSys(NumOfHydrLowTempRadSys))
  IF (NumOfHydrLowTempRadSys > 0) THEN
    GlycolIndex                = FindGlycol('WATER')
    HydrRadSys%GlycolIndex     = GlycolIndex
    IF (GlycolIndex == 0) THEN
        CALL ShowSevereError('Hydronic radiant systems: no water property data found in input')
        ErrorsFound=.true.
    END IF
  ELSE
    HydrRadSys%GlycolIndex       = 0
  ENDIF

  ALLOCATE(CFloRadSys(NumOfCFloLowTempRadSys))
  IF (NumOfCFloLowTempRadSys > 0) THEN
    GlycolIndex            = FindGlycol('WATER')
    CFloRadSys%GlycolIndex = GlycolIndex
    IF (GlycolIndex == 0) THEN
      CALL ShowSevereError('Constant flow radiant systems: no water property data found in input')
      ErrorsFound = .TRUE.
    END IF
  ELSE
    CFloRadSys%GlycolIndex       = 0
  ENDIF

  ALLOCATE(ElecRadSys(NumOfElecLowTempRadSys))

! make sure data is gotten for surface lists
  BaseNum=GetNumberOfSurfaceLists()

          ! Obtain all of the user data related to hydronic low temperature radiant systems...
  BaseNum=0
  CurrentModuleObject = 'ZoneHVAC:LowTemperatureRadiant:VariableFlow'
  DO Item = 1, NumOfHydrLowTempRadSys

    CALL GetObjectItem(CurrentModuleObject,Item,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),RadSysTypes%Name,BaseNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    BaseNum=BaseNum+1
    RadSysTypes(BaseNum)%Name=Alphas(1)
    RadSysTypes(BaseNum)%SystemType=HydronicSystem
          ! General user input data
    HydrRadSys(Item)%Name = Alphas(1)

    HydrRadSys(Item)%SchedName = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      HydrRadSys(Item)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      HydrRadSys(Item)%SchedPtr  = GetScheduleIndex(Alphas(2))
      IF (HydrRadSys(Item)%SchedPtr == 0) THEN
        CALL ShowSevereError(TRIM(cAlphaFields(2))//' not found for '//TRIM(Alphas(1)))
        CALL ShowContinueError('Missing '//TRIM(cAlphaFields(2))//' is '//TRIM(Alphas(2)))
        ErrorsFound=.true.
      END IF
    END IF

    HydrRadSys(Item)%ZoneName = Alphas(3)
    HydrRadSys(Item)%ZonePtr  = FindIteminList(Alphas(3),Zone%Name,NumOfZones)
    IF (HydrRadSys(Item)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(3))//' = '//TRIM(Alphas(3)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
!    ELSEIF (Zone(HydrRadSys(Item)%ZonePtr)%Multiplier > 1 .or. Zone(HydrRadSys(Item)%ZonePtr)%ListMultiplier > 1) THEN
!      CALL ShowSevereError(RoutineName//'Zone Multiplier or Zone List Multipliers cannot be used (i.e., >1)')
!      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
!      CALL ShowContinueError('Duplicate the zone or make it larger rather than using multipliers.  Zone Referenced='//  &
!          TRIM(HydrRadSys(Item)%ZoneName))
!      ErrorsFound=.true.
    END IF

    HydrRadSys(Item)%SurfListName = Alphas(4)
    SurfListNum = 0
    IF (NumOfSurfaceLists > 0) SurfListNum = FindItemInList(HydrRadSys(Item)%SurfListName,SurfList%Name,NumOfSurfaceLists)
    IF (SurfListNum > 0) THEN   ! Found a valid surface list
      HydrRadSys(Item)%NumOfSurfaces = SurfList(SurfListNum)%NumOfSurfaces
      ALLOCATE (HydrRadSys(Item)%SurfacePtr(HydrRadSys(Item)%NumOfSurfaces))
      ALLOCATE (HydrRadSys(Item)%SurfaceName(HydrRadSys(Item)%NumOfSurfaces))
      ALLOCATE (HydrRadSys(Item)%SurfaceFlowFrac(HydrRadSys(Item)%NumOfSurfaces))
      ALLOCATE (HydrRadSys(Item)%NumCircuits(HydrRadSys(Item)%NumOfSurfaces))
      DO SurfNum = 1, SurfList(SurfListNum)%NumOfSurfaces
        HydrRadSys(Item)%SurfacePtr(SurfNum)      = SurfList(SurfListNum)%SurfPtr(SurfNum)
        HydrRadSys(Item)%SurfaceName(SurfNum)     = SurfList(SurfListNum)%SurfName(SurfNum)
        HydrRadSys(Item)%SurfaceFlowFrac(SurfNum) = SurfList(SurfListNum)%SurfFlowFrac(SurfNum)
        IF (HydrRadSys(Item)%SurfacePtr(SurfNum) > 0)THEN
          Surface( HydrRadSys(Item)%SurfacePtr(SurfNum) )%IntConvSurfHasActiveInIt = .TRUE.
        ENDIF
      END DO
    ELSE    ! User entered a single surface name rather than a surface list
      HydrRadSys(Item)%NumOfSurfaces = 1
      ALLOCATE (HydrRadSys(Item)%SurfacePtr(HydrRadSys(Item)%NumOfSurfaces))
      ALLOCATE (HydrRadSys(Item)%SurfaceName(HydrRadSys(Item)%NumOfSurfaces))
      ALLOCATE (HydrRadSys(Item)%SurfaceFlowFrac(HydrRadSys(Item)%NumOfSurfaces))
      ALLOCATE (HydrRadSys(Item)%NumCircuits(HydrRadSys(Item)%NumOfSurfaces))
      HydrRadSys(Item)%SurfaceName(1)     = HydrRadSys(Item)%SurfListName
      HydrRadSys(Item)%SurfacePtr(1)      = FindIteminList(HydrRadSys(Item)%SurfaceName(1),Surface%Name,TotSurfaces)
      HydrRadSys(Item)%SurfaceFlowFrac(1) = 1.0d0
      HydrRadSys(Item)%NumCircuits(1) = 0.0d0
          ! Error checking for single surfaces
      IF (HydrRadSys(Item)%SurfacePtr(1) == 0) THEN
        CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(4))//' = '//TRIM(Alphas(4)))
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        ErrorsFound=.true.
      ELSEIF (Surface(HydrRadSys(Item)%SurfacePtr(1))%PartOfVentSlabOrRadiantSurface) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", Invalid Surface')
        CALL ShowContinueError(trim(cAlphaFields(4))//'="'//TRIM(Alphas(4))//  &
           '" has been used in another radiant system or ventilated slab.')
        ErrorsFound=.true.
      END IF
      IF (HydrRadSys(Item)%SurfacePtr(1) /= 0) THEN
        Surface( HydrRadSys(Item)%SurfacePtr(1) )%IntConvSurfHasActiveInIt = .TRUE.
        Surface( HydrRadSys(Item)%SurfacePtr(1) )%PartOfVentSlabOrRadiantSurface = .true.
      ENDIF
    END IF

          ! Error checking for zones and construction information
    DO SurfNum = 1, HydrRadSys(Item)%NumOfSurfaces
      IF (HydrRadSys(Item)%SurfacePtr(SurfNum) == 0) CYCLE ! invalid surface -- detected earlier
      IF (Surface(HydrRadSys(Item)%SurfacePtr(SurfNum))%Zone /= HydrRadSys(Item)%ZonePtr) THEN
        CALL ShowSevereError('Surface referenced in '//TRIM(CurrentModuleObject)//  &
           ' not in same zone as Radiant System, surface='//  &
           TRIM(Surface(HydrRadSys(Item)%SurfacePtr(SurfNum))%Name))
        CALL ShowContinueError('Surface in Zone='//TRIM(Zone(Surface(HydrRadSys(Item)%SurfacePtr(SurfNum))%Zone)%Name)//' '// &
                         'Hydronic Radiant System in '//TRIM(cAlphaFields(3))//' = '//TRIM(Alphas(3)))
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        ErrorsFound=.true.
      END IF
      IF (Surface(HydrRadSys(Item)%SurfacePtr(SurfNum))%Construction == 0) CYCLE  ! Invalid construction -- detected earlier
      IF (.NOT. Construct(Surface(HydrRadSys(Item)%SurfacePtr(SurfNum))%Construction)%SourceSinkPresent) THEN
        CALL ShowSevereError('Construction referenced in Hydronic Radiant System Surface does not have a source/sink present')
        CALL ShowContinueError('Surface name= '//TRIM(Surface(HydrRadSys(Item)%SurfacePtr(SurfNum))%Name)// &
                               '  Construction name = '// &
                               TRIM(Construct(Surface(HydrRadSys(Item)%SurfacePtr(SurfNum))%Construction)%Name))
        CALL ShowContinueError('Construction needs to be defined with a "Construction:InternalSource" object.')
        ErrorsFound=.true.
      END IF
    END DO

    HydrRadSys(Item)%TubeDiameter = Numbers(1)
    HydrRadSys(Item)%TubeLength   = Numbers(2)

          ! Process the temperature control type
    IF (SameString(Alphas(5),MeanAirTemperature)) THEN
      HydrRadSys(Item)%ControlType = MATControl
    ELSE IF (SameString(Alphas(5),MeanRadiantTemperature)) THEN
      HydrRadSys(Item)%ControlType = MRTControl
    ELSE IF (SameString(Alphas(5),OperativeTemperature)) THEN
      HydrRadSys(Item)%ControlType = OperativeControl
    ELSE IF (SameString(Alphas(5),OutsideAirDryBulbTemperature)) THEN
      HydrRadSys(Item)%ControlType = ODBControl
    ELSE IF (SameString(Alphas(5),OutsideAirWetBulbTemperature)) THEN
      HydrRadSys(Item)%ControlType = OWBControl
    ELSE
      CALL ShowWarningError('Invalid '//TRIM(cAlphaFields(5))//' ='//TRIM(Alphas(5)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      CALL ShowContinueError('Control reset to MAT control for this Hydronic Radiant System.')
      HydrRadSys(Item)%ControlType = MATControl
    END IF

          ! Heating user input data
    HydrRadSys(Item)%WaterVolFlowMaxHeat = Numbers(3)

    HydrRadSys(Item)%HotWaterInNode = &
               GetOnlySingleNode(Alphas(6),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    HydrRadSys(Item)%HotWaterOutNode = &
               GetOnlySingleNode(Alphas(7),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

    IF ((.NOT. lAlphaBlanks(6)) .OR. (.NOT. lAlphaBlanks(7))) THEN
      CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(6),Alphas(7),'Hot Water Nodes')
    ENDIF

    HydrRadSys(Item)%HotThrottlRange = Numbers(4)
    IF (HydrRadSys(Item)%HotThrottlRange < MinThrottlingRange) THEN
      CALL ShowWarningError('ZoneHVAC:LowTemperatureRadiant:VariableFlow: Heating throttling range too small, reset to 0.5')
      CALL ShowContinueError('Occurs in Radiant System='//TRIM(HydrRadSys(Item)%Name))
      HydrRadSys(Item)%HotThrottlRange = MinThrottlingRange
    END IF

    HydrRadSys(Item)%HotSetptSched    = Alphas(8)
    HydrRadSys(Item)%HotSetptSchedPtr = GetScheduleIndex(Alphas(8))
    IF ((HydrRadSys(Item)%HotSetptSchedPtr == 0).AND.(.NOT. lAlphaBlanks(8))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(8))//' not found: '//TRIM(Alphas(8)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

    IF ( (HydrRadSys(Item)%WaterVolFlowMaxHeat == AutoSize) .AND. &
         ( lAlphaBlanks(6) .OR. lAlphaBlanks(7) .OR. lAlphaBlanks(8) .OR. &
           (HydrRadSys(Item)%HotWaterInNode <= 0) .OR. (HydrRadSys(Item)%HotWaterOutNode <= 0) .OR. &
           (HydrRadSys(Item)%HotSetptSchedPtr == 0) ) ) THEN
      CALL ShowSevereError('Hydronic radiant systems may not be autosized without specification of nodes or schedules.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' (heating input) = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

          ! Cooling user input data
    HydrRadSys(Item)%WaterVolFlowMaxCool = Numbers(5)

    HydrRadSys(Item)%ColdWaterInNode = &
               GetOnlySingleNode(Alphas(9),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)

    HydrRadSys(Item)%ColdWaterOutNode = &
               GetOnlySingleNode(Alphas(10),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

    IF ((.NOT. lAlphaBlanks(9)) .OR. (.NOT. lAlphaBlanks(10))) THEN
      CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(9),Alphas(10),'Chilled Water Nodes')
    ENDIF

    HydrRadSys(Item)%ColdThrottlRange = Numbers(6)
    IF (HydrRadSys(Item)%ColdThrottlRange < MinThrottlingRange) THEN
      CALL ShowWarningError('ZoneHVAC:LowTemperatureRadiant:VariableFlow: Cooling throttling range too small, reset to 0.5')
      CALL ShowContinueError('Occurs in Radiant System='//TRIM(HydrRadSys(Item)%Name))
      HydrRadSys(Item)%ColdThrottlRange = MinThrottlingRange
    END IF

    HydrRadSys(Item)%ColdSetptSched    = Alphas(11)
    HydrRadSys(Item)%ColdSetptSchedPtr = GetScheduleIndex(Alphas(11))
    IF ((HydrRadSys(Item)%ColdSetptSchedPtr == 0).AND.(.NOT. lAlphaBlanks(11))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(11))//' not found: '//TRIM(Alphas(11)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

    IF (SameString(Alphas(12),Off)) THEN
      HydrRadSys(Item)%CondCtrlType = CondCtrlNone
    ELSE IF (SameString(Alphas(12),SimpleOff)) THEN
      HydrRadSys(Item)%CondCtrlType = CondCtrlSimpleOff
    ELSE IF (SameString(Alphas(12),VariableOff)) THEN
      HydrRadSys(Item)%CondCtrlType = CondCtrlVariedOff
    ELSE
      HydrRadSys(Item)%CondCtrlType = CondCtrlSimpleOff
    END IF

    HydrRadSys(Item)%CondDewPtDeltaT = Numbers(7)

    IF (SameString(Alphas(13),OnePerSurf)) THEN
      HydrRadSys(Item)%NumCircCalcMethod = OneCircuit
    ELSE IF (SameString(Alphas(13),CalcFromLength)) THEN
      HydrRadSys(Item)%NumCircCalcMethod = CalculateFromLength
    ELSE
      HydrRadSys(Item)%NumCircCalcMethod = OneCircuit
    END IF

    HydrRadSys(Item)%CircLength = Numbers(8)

    IF ( (HydrRadSys(Item)%WaterVolFlowMaxCool == AutoSize) .AND. &
         (  lAlphaBlanks(9) .OR. lAlphaBlanks(10) .OR. lAlphaBlanks(11) .OR. &
           (HydrRadSys(Item)%ColdWaterInNode <= 0) .OR. (HydrRadSys(Item)%ColdWaterOutNode <= 0) .OR. &
           (HydrRadSys(Item)%ColdSetptSchedPtr == 0) ) ) THEN
      CALL ShowSevereError('Hydronic radiant systems may not be autosized without specification of nodes or schedules')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' (cooling input) ='//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

  END DO


          ! Obtain all of the user data related to constant flow (hydronic) low temperature radiant systems...

  CurrentModuleObject = 'ZoneHVAC:LowTemperatureRadiant:ConstantFlow'

  DO Item = 1, NumOfCFloLowTempRadSys

    CALL GetObjectItem(CurrentModuleObject,Item,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),RadSysTypes%Name,BaseNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    BaseNum=BaseNum+1
    RadSysTypes(BaseNum)%Name=Alphas(1)
    RadSysTypes(BaseNum)%SystemType=ConstantFlowSystem
          ! General user input data
    CFloRadSys(Item)%Name = Alphas(1)

    CFloRadSys(Item)%SchedName = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      CFloRadSys(Item)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      CFloRadSys(Item)%SchedPtr  = GetScheduleIndex(Alphas(2))
      IF (CFloRadSys(Item)%SchedPtr == 0) THEN
        CALL ShowSevereError(TRIM(cAlphaFields(2))//' not found for '//TRIM(Alphas(1)))
        CALL ShowContinueError('Missing '//TRIM(cAlphaFields(2))//' is '//TRIM(Alphas(2)))
        ErrorsFound=.true.
      END IF
    END IF

    CFloRadSys(Item)%ZoneName = Alphas(3)
    CFloRadSys(Item)%ZonePtr  = FindIteminList(Alphas(3),Zone%Name,NumOfZones)
    IF (CFloRadSys(Item)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(3))//' = '//TRIM(Alphas(3)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
!    ELSEIF (Zone(CFloRadSys(Item)%ZonePtr)%Multiplier > 1 .or. Zone(CFloRadSys(Item)%ZonePtr)%ListMultiplier > 1) THEN
!      CALL ShowSevereError(RoutineName//'Zone Multiplier or Zone List Multipliers cannot be used (i.e., >1)')
!      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
!      CALL ShowContinueError('Duplicate the zone or make it larger rather than using multipliers.  Zone Referenced='//  &
!          TRIM(CFloRadSys(Item)%ZoneName))
!      ErrorsFound=.true.
    END IF

    CFloRadSys(Item)%SurfListName = Alphas(4)
    SurfListNum = 0
    IF (NumOfSurfaceLists > 0) SurfListNum = FindItemInList(CFloRadSys(Item)%SurfListName,SurfList%Name,NumOfSurfaceLists)
    IF (SurfListNum > 0) THEN   ! Found a valid surface list
      CFloRadSys(Item)%NumOfSurfaces = SurfList(SurfListNum)%NumOfSurfaces
      ALLOCATE (CFloRadSys(Item)%SurfacePtr(CFloRadSys(Item)%NumOfSurfaces))
      ALLOCATE (CFloRadSys(Item)%SurfaceName(CFloRadSys(Item)%NumOfSurfaces))
      ALLOCATE (CFloRadSys(Item)%SurfaceFlowFrac(CFloRadSys(Item)%NumOfSurfaces))
      ALLOCATE (CFloRadSys(Item)%NumCircuits(CFloRadSys(Item)%NumOfSurfaces))
      MaxCloNumOfSurfaces=Max(MaxCloNumOfSurfaces,CFloRadSys(Item)%NumOfSurfaces)
      DO SurfNum = 1, SurfList(SurfListNum)%NumOfSurfaces
        CFloRadSys(Item)%SurfacePtr(SurfNum)      = SurfList(SurfListNum)%SurfPtr(SurfNum)
        CFloRadSys(Item)%SurfaceName(SurfNum)     = SurfList(SurfListNum)%SurfName(SurfNum)
        CFloRadSys(Item)%SurfaceFlowFrac(SurfNum) = SurfList(SurfListNum)%SurfFlowFrac(SurfNum)
        CFloRadSys(Item)%NumCircuits(SurfNum)     = 0.0d0
        IF (CFloRadSys(Item)%SurfacePtr(SurfNum) /= 0) THEN
          Surface( CFloRadSys(Item)%SurfacePtr(SurfNum) )%IntConvSurfHasActiveInIt = .TRUE.
        ENDIF
      END DO
    ELSE    ! User entered a single surface name rather than a surface list
      CFloRadSys(Item)%NumOfSurfaces = 1
      ALLOCATE (CFloRadSys(Item)%SurfacePtr(CFloRadSys(Item)%NumOfSurfaces))
      ALLOCATE (CFloRadSys(Item)%SurfaceName(CFloRadSys(Item)%NumOfSurfaces))
      ALLOCATE (CFloRadSys(Item)%SurfaceFlowFrac(CFloRadSys(Item)%NumOfSurfaces))
      ALLOCATE (CFloRadSys(Item)%NumCircuits(CFloRadSys(Item)%NumOfSurfaces))
      MaxCloNumOfSurfaces=Max(MaxCloNumOfSurfaces,CFloRadSys(Item)%NumOfSurfaces)
      CFloRadSys(Item)%SurfaceName(1)     = CFloRadSys(Item)%SurfListName
      CFloRadSys(Item)%SurfacePtr(1)      = FindIteminList(CFloRadSys(Item)%SurfaceName(1),Surface%Name,TotSurfaces)
      CFloRadSys(Item)%SurfaceFlowFrac(1) = 1.0d0
      CFloRadSys(Item)%NumCircuits(1) = 0.0d0
          ! Error checking for single surfaces
      IF (CFloRadSys(Item)%SurfacePtr(1) == 0) THEN
        CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(4))//' = '//TRIM(Alphas(4)))
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        ErrorsFound=.true.
      ELSEIF (Surface(CFloRadSys(Item)%SurfacePtr(1))%PartOfVentSlabOrRadiantSurface) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", Invalid Surface')
        CALL ShowContinueError(trim(cAlphaFields(4))//'="'//TRIM(Alphas(4))//  &
           '" has been used in another radiant system or ventilated slab.')
        ErrorsFound=.true.
      END IF
      IF (CFloRadSys(Item)%SurfacePtr(1) /= 0) THEN
        Surface( CFloRadSys(Item)%SurfacePtr(1) )%IntConvSurfHasActiveInIt = .TRUE.
        Surface( CFloRadSys(Item)%SurfacePtr(1) )%PartOfVentSlabOrRadiantSurface = .true.
      ENDIF
    END IF

          ! Error checking for zones and construction information
    DO SurfNum = 1, CFloRadSys(Item)%NumOfSurfaces
      IF (CFloRadSys(Item)%SurfacePtr(SurfNum) == 0) CYCLE ! invalid surface -- detected earlier
      IF (Surface(CFloRadSys(Item)%SurfacePtr(SurfNum))%Zone /= CFloRadSys(Item)%ZonePtr) THEN
        CALL ShowSevereError('Surface referenced in '//TRIM(CurrentModuleObject)//' not in same zone as Radiant System, '// &
                             'surface='//TRIM(Surface(CFloRadSys(Item)%SurfacePtr(SurfNum))%Name))
        CALL ShowContinueError('Surface in Zone='//TRIM(Zone(Surface(CFloRadSys(Item)%SurfacePtr(SurfNum))%Zone)%Name)//' '// &
                         'Constant Flow Radiant System in '//TRIM(cAlphaFields(3))//' = '//TRIM(Alphas(3)))
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        ErrorsFound=.true.
      END IF
      IF (Surface(CFloRadSys(Item)%SurfacePtr(SurfNum))%Construction == 0) CYCLE ! invalid construction, detected earlier
      IF (.NOT. Construct(Surface(CFloRadSys(Item)%SurfacePtr(SurfNum))%Construction)%SourceSinkPresent) THEN
        CALL ShowSevereError('Construction referenced in Constant Flow Radiant System Surface does not have a source/sink')
        CALL ShowContinueError('Surface name= '//TRIM(Surface(CFloRadSys(Item)%SurfacePtr(SurfNum))%Name)// &
                               '  Construction name = '// &
                               TRIM(Construct(Surface(CFloRadSys(Item)%SurfacePtr(SurfNum))%Construction)%Name))
        CALL ShowContinueError('Construction needs to be defined with a "Construction:InternalSource" object.')
        ErrorsFound=.true.
      END IF
    END DO

    CFloRadSys(Item)%TubeDiameter = Numbers(1)
    CFloRadSys(Item)%TubeLength   = Numbers(2)

          ! Process the temperature control type
    IF (SameString(Alphas(5),MeanAirTemperature)) THEN
      CFloRadSys(Item)%ControlType = MATControl
    ELSE IF (SameString(Alphas(5),MeanRadiantTemperature)) THEN
      CFloRadSys(Item)%ControlType = MRTControl
    ELSE IF (SameString(Alphas(5),OperativeTemperature)) THEN
      CFloRadSys(Item)%ControlType = OperativeControl
    ELSE IF (SameString(Alphas(5),OutsideAirDryBulbTemperature)) THEN
      CFloRadSys(Item)%ControlType = ODBControl
    ELSE IF (SameString(Alphas(5),OutsideAirWetBulbTemperature)) THEN
      CFloRadSys(Item)%ControlType = OWBControl
    ELSE
      CALL ShowWarningError('Invalid '//TRIM(cAlphaFields(5))//' ='//TRIM(Alphas(5)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      CALL ShowContinueError('Control reset to MAT control for this Constant Flow Radiant System.')
      CFloRadSys(Item)%ControlType = MATControl
    END IF

          ! Process pump input for constant flow (hydronic) radiant system
    CFloRadSys(Item)%WaterVolFlowMax = Numbers(3)
    CFloRadSys(Item)%VolFlowSched    = Alphas(6)
    CFloRadSys(Item)%VolFlowSchedPtr = GetScheduleIndex(Alphas(6))
    IF ((CFloRadSys(Item)%VolFlowSchedPtr == 0).AND.(.NOT. lAlphaBlanks(6))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(6))//' not found: '//TRIM(Alphas(6)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF
    CFloRadSys(Item)%NomPumpHead          = Numbers(4)
    CFloRadSys(Item)%NomPowerUse          = Numbers(5)
    CFloRadSys(Item)%MotorEffic           = Numbers(6)
    CFloRadSys(Item)%FracMotorLossToFluid = Numbers(7)

          ! Heating user input data
    CFloRadSys(Item)%HotWaterInNode = &
               GetOnlySingleNode(Alphas(7),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    CFloRadSys(Item)%HotWaterOutNode = &
               GetOnlySingleNode(Alphas(8),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

    IF ((.NOT. lAlphaBlanks(7)) .OR. (.NOT. lAlphaBlanks(8))) THEN
      CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(7),Alphas(8),'Hot Water Nodes')
    ENDIF

    CFloRadSys(Item)%HotWaterHiTempSched    = Alphas(9)
    CFloRadSys(Item)%HotWaterHiTempSchedPtr = GetScheduleIndex(Alphas(9))
    IF ((CFloRadSys(Item)%HotWaterHiTempSchedPtr == 0).AND.(.NOT. lAlphaBlanks(9))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(9))//' not found: '//TRIM(Alphas(9)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

    CFloRadSys(Item)%HotWaterLoTempSched    = Alphas(10)
    CFloRadSys(Item)%HotWaterLoTempSchedPtr = GetScheduleIndex(Alphas(10))
    IF ((CFloRadSys(Item)%HotWaterLoTempSchedPtr == 0).AND.(.NOT. lAlphaBlanks(10))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(10))//' not found: '//TRIM(Alphas(10)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

    CFloRadSys(Item)%HotCtrlHiTempSched    = Alphas(11)
    CFloRadSys(Item)%HotCtrlHiTempSchedPtr = GetScheduleIndex(Alphas(11))
    IF ((CFloRadSys(Item)%HotCtrlHiTempSchedPtr == 0).AND.(.NOT. lAlphaBlanks(11))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(11))//' not found: '//TRIM(Alphas(11)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

    CFloRadSys(Item)%HotCtrlLoTempSched    = Alphas(12)
    CFloRadSys(Item)%HotCtrlLoTempSchedPtr = GetScheduleIndex(Alphas(12))
    IF ((CFloRadSys(Item)%HotCtrlLoTempSchedPtr == 0).AND.(.NOT. lAlphaBlanks(12))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(12))//' not found: '//TRIM(Alphas(12)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

          ! Cooling user input data
    CFloRadSys(Item)%ColdWaterInNode = &
               GetOnlySingleNode(Alphas(13),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)

    CFloRadSys(Item)%ColdWaterOutNode = &
               GetOnlySingleNode(Alphas(14),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

    IF ((.NOT. lAlphaBlanks(13)) .OR. (.NOT. lAlphaBlanks(14))) THEN
      CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(13),Alphas(14),'Chilled Water Nodes')
    ENDIF

    CFloRadSys(Item)%ColdWaterHiTempSched    = Alphas(15)
    CFloRadSys(Item)%ColdWaterHiTempSchedPtr = GetScheduleIndex(Alphas(15))
    IF ((CFloRadSys(Item)%ColdWaterHiTempSchedPtr == 0).AND.(.NOT. lAlphaBlanks(15))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(15))//' not found: '//TRIM(Alphas(15)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

    CFloRadSys(Item)%ColdWaterLoTempSched    = Alphas(16)
    CFloRadSys(Item)%ColdWaterLoTempSchedPtr = GetScheduleIndex(Alphas(16))
    IF ((CFloRadSys(Item)%ColdWaterLoTempSchedPtr == 0).AND.(.NOT. lAlphaBlanks(16))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(16))//' not found: '//TRIM(Alphas(16)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

    CFloRadSys(Item)%ColdCtrlHiTempSched    = Alphas(17)
    CFloRadSys(Item)%ColdCtrlHiTempSchedPtr = GetScheduleIndex(Alphas(17))
    IF ((CFloRadSys(Item)%ColdCtrlHiTempSchedPtr == 0).AND.(.NOT. lAlphaBlanks(17))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(17))//' not found: '//TRIM(Alphas(17)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

    CFloRadSys(Item)%ColdCtrlLoTempSched    = Alphas(18)
    CFloRadSys(Item)%ColdCtrlLoTempSchedPtr = GetScheduleIndex(Alphas(18))
    IF ((CFloRadSys(Item)%ColdCtrlLoTempSchedPtr == 0).AND.(.NOT. lAlphaBlanks(18))) THEN
      CALL ShowSevereError(TRIM(cAlphaFields(18))//' not found: '//TRIM(Alphas(18)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
    END IF

    IF (SameString(Alphas(19),Off)) THEN
      CFloRadSys(Item)%CondCtrlType = CondCtrlNone
    ELSE IF (SameString(Alphas(19),SimpleOff)) THEN
      CFloRadSys(Item)%CondCtrlType = CondCtrlSimpleOff
    ELSE IF (SameString(Alphas(19),VariableOff)) THEN
      CFloRadSys(Item)%CondCtrlType = CondCtrlVariedOff
    ELSE
      CFloRadSys(Item)%CondCtrlType = CondCtrlSimpleOff
    END IF

    CFloRadSys(Item)%CondDewPtDeltaT = Numbers(8)

    IF (SameString(Alphas(20),OnePerSurf)) THEN
      CFloRadSys(Item)%NumCircCalcMethod = OneCircuit
    ELSE IF (SameString(Alphas(20),CalcFromLength)) THEN
      CFloRadSys(Item)%NumCircCalcMethod = CalculateFromLength
    ELSE
      CFloRadSys(Item)%NumCircCalcMethod = OneCircuit
    END IF

    CFloRadSys(Item)%CircLength = Numbers(9)

  END DO

          ! Obtain all of the user data related to electric low temperature radiant systems...
  CurrentModuleObject = 'ZoneHVAC:LowTemperatureRadiant:Electric'

  DO Item = 1, NumOfElecLowTempRadSys

    CALL GetObjectItem(CurrentModuleObject,Item,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),RadSysTypes%Name,BaseNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    BaseNum=BaseNum+1
    RadSysTypes(BaseNum)%Name=Alphas(1)
    RadSysTypes(BaseNum)%SystemType=ElectricSystem
          ! General user input data
    ElecRadSys(Item)%Name = Alphas(1)

    ElecRadSys(Item)%SchedName = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      ElecRadSys(Item)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      ElecRadSys(Item)%SchedPtr  = GetScheduleIndex(Alphas(2))
      IF (ElecRadSys(Item)%SchedPtr == 0) THEN
        CALL ShowSevereError(TRIM(cAlphaFields(2))//' not found for'//TRIM(Alphas(1)))
        CALL ShowContinueError('Incorrect '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2)))
        ErrorsFound=.true.
      END IF
    END IF

    ElecRadSys(Item)%ZoneName = Alphas(3)
    ElecRadSys(Item)%ZonePtr  = FindIteminList(Alphas(3),Zone%Name,NumOfZones)
    IF (ElecRadSys(Item)%ZonePtr == 0) THEN
      CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(3))//' = '//TRIM(Alphas(3)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ErrorsFound=.true.
!    ELSEIF (Zone(ElecRadSys(Item)%ZonePtr)%Multiplier > 1 .or. Zone(ElecRadSys(Item)%ZonePtr)%ListMultiplier > 1) THEN
!      CALL ShowSevereError(RoutineName//'Zone Multiplier or Zone List Multipliers cannot be used (i.e., >1)')
!      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
!      CALL ShowContinueError('Duplicate the zone or make it larger rather than using multipliers.  Zone Referenced='//  &
!          TRIM(ElecRadSys(Item)%ZoneName))
!      ErrorsFound=.true.
    END IF

    ElecRadSys(Item)%SurfListName = Alphas(4)
    SurfListNum = 0
    IF (NumOfSurfaceLists > 0) SurfListNum = FindItemInList(ElecRadSys(Item)%SurfListName,SurfList%Name,NumOfSurfaceLists)
    IF (SurfListNum > 0) THEN   ! Found a valid surface list
      ElecRadSys(Item)%NumOfSurfaces = SurfList(SurfListNum)%NumOfSurfaces
      ALLOCATE (ElecRadSys(Item)%SurfacePtr(ElecRadSys(Item)%NumOfSurfaces))
      ALLOCATE (ElecRadSys(Item)%SurfaceName(ElecRadSys(Item)%NumOfSurfaces))
      ALLOCATE (ElecRadSys(Item)%SurfacePowerFrac(ElecRadSys(Item)%NumOfSurfaces))
      DO SurfNum = 1, ElecRadSys(SurfListNum)%NumOfSurfaces
        ElecRadSys(Item)%SurfacePtr(SurfNum)       = SurfList(SurfListNum)%SurfPtr(SurfNum)
        ElecRadSys(Item)%SurfaceName(SurfNum)      = SurfList(SurfListNum)%SurfName(SurfNum)
        ElecRadSys(Item)%SurfacePowerFrac(SurfNum) = SurfList(SurfListNum)%SurfFlowFrac(SurfNum)
      END DO
    ELSE    ! User entered a single surface name rather than a surface list
      ElecRadSys(Item)%NumOfSurfaces = 1
      ALLOCATE (ElecRadSys(Item)%SurfacePtr(ElecRadSys(Item)%NumOfSurfaces))
      ALLOCATE (ElecRadSys(Item)%SurfaceName(ElecRadSys(Item)%NumOfSurfaces))
      ALLOCATE (ElecRadSys(Item)%SurfacePowerFrac(ElecRadSys(Item)%NumOfSurfaces))
      ElecRadSys(Item)%SurfaceName(1)      = ElecRadSys(Item)%SurfListName
      ElecRadSys(Item)%SurfacePtr(1)       = FindIteminList(ElecRadSys(Item)%SurfaceName(1),Surface%Name,TotSurfaces)
      ElecRadSys(Item)%SurfacePowerFrac(1) = 1.0d0
          ! Error checking for single surfaces
      IF (ElecRadSys(Item)%SurfacePtr(1) == 0) THEN
        CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(4))//' = '//TRIM(Alphas(4)))
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        ErrorsFound=.true.
      ELSEIF (Surface(ElecRadSys(Item)%SurfacePtr(1))%PartOfVentSlabOrRadiantSurface) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", Invalid Surface')
        CALL ShowContinueError(trim(cAlphaFields(4))//'="'//TRIM(Alphas(4))//  &
           '" has been used in another radiant system or ventilated slab.')
        ErrorsFound=.true.
      END IF
      IF (ElecRadSys(Item)%SurfacePtr(1) /= 0) THEN
        Surface( ElecRadSys(Item)%SurfacePtr(1) )%PartOfVentSlabOrRadiantSurface = .true.
      ENDIF
    END IF

          ! Error checking for zones and construction information
    DO SurfNum = 1, ElecRadSys(Item)%NumOfSurfaces
      IF (ElecRadSys(Item)%SurfacePtr(SurfNum) == 0) CYCLE  ! Invalid surface -- detected earlier
      IF (Surface(ElecRadSys(Item)%SurfacePtr(SurfNum))%Zone /= ElecRadSys(Item)%ZonePtr) THEN
        CALL ShowSevereError('Surface referenced in '//TRIM(CurrentModuleObject)//' not in same zone as Radiant System,'//  &
                              ' surface='//TRIM(Surface(ElecRadSys(Item)%SurfacePtr(SurfNum))%Name))
        CALL ShowContinueError('Surface in Zone='//TRIM(Zone(Surface(ElecRadSys(Item)%SurfacePtr(SurfNum))%Zone)%Name)//' '// &
                         'Electric Radiant System in '//TRIM(cAlphaFields(3))//' = '//TRIM(Alphas(3)))
        CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        ErrorsFound=.true.
      END IF
      IF (Surface(ElecRadSys(Item)%SurfacePtr(SurfNum))%Construction == 0) CYCLE  ! invalid construction -- detected earlier
      IF (.NOT. Construct(Surface(ElecRadSys(Item)%SurfacePtr(SurfNum))%Construction)%SourceSinkPresent) THEN
        CALL ShowSevereError('Construction referenced in Electric Radiant System Surface does not have a source/sink present')
        CALL ShowContinueError('Surface name= '//TRIM(Surface(ElecRadSys(Item)%SurfacePtr(SurfNum))%Name)// &
                               '  Construction name = '// &
                               TRIM(Construct(Surface(ElecRadSys(Item)%SurfacePtr(SurfNum))%Construction)%Name))
        CALL ShowContinueError('Construction needs to be defined with a "Construction:InternalSource" object.')
        ErrorsFound=.true.
      END IF
    END DO

          ! Heating user input data
    ElecRadSys(Item)%MaxElecPower = Numbers(1)

          ! Process the temperature control type
    IF (SameString(Alphas(5),MeanAirTemperature)) THEN
      ElecRadSys(Item)%ControlType = MATControl
    ELSE IF (SameString(Alphas(5),MeanRadiantTemperature)) THEN
      ElecRadSys(Item)%ControlType = MRTControl
    ELSE IF (SameString(Alphas(5),OperativeTemperature)) THEN
      ElecRadSys(Item)%ControlType = OperativeControl
    ELSE IF (SameString(Alphas(5),OutsideAirDryBulbTemperature)) THEN
      ElecRadSys(Item)%ControlType = ODBControl
    ELSE IF (SameString(Alphas(5),OutsideAirWetBulbTemperature)) THEN
      ElecRadSys(Item)%ControlType = OWBControl
    ELSE
      CALL ShowWarningError('Invalid '//TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5)))
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      CALL ShowContinueError('Control reset to MAT control for this Electric Radiant System.')
      ElecRadSys(Item)%ControlType = MATControl
    END IF

    ElecRadSys(Item)%ThrottlRange = Numbers(2)
    IF (ElecRadSys(Item)%ThrottlRange < MinThrottlingRange) THEN
      CALL ShowWarningError(TRIM(cNumericFields(2))//' out of range, reset to 0.5')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ElecRadSys(Item)%ThrottlRange = MinThrottlingRange
    END IF

    ElecRadSys(Item)%SetptSched    = Alphas(6)
    ElecRadSys(Item)%SetptSchedPtr = GetScheduleIndex(Alphas(6))
    IF (ElecRadSys(Item)%SetptSchedPtr == 0) THEN
      IF (lAlphaBlanks(6)) THEN
        CALL ShowSevereError(TRIM(cAlphaFields(6))//' must be input, missing for '//TRIM(Alphas(1)))
      ELSE
        CALL ShowSevereError(TRIM(cAlphaFields(6))//' not found for '//TRIM(Alphas(1)))
        CALL ShowContinueError('Incorrect '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
      END IF
      ErrorsFound=.true.
    END IF

  END DO

          ! Check to see if any surface is included in more than one radiant system.  This is not allowed
          ! and thus indicative that there is an error in the input file.  This is to make sure that two
          ! different radiant systems are competing for the same surface.  Allowing this to happen would
          ! result in lost energy somewhere and the situation really is not physically possible anyway.
  ALLOCATE (AssignedAsRadiantSurface(TotSurfaces))
  AssignedAsRadiantSurface = .FALSE.

  DO Item = 1, NumOfHydrLowTempRadSys
    DO SurfNum = 1, HydrRadSys(Item)%NumOfSurfaces
      CheckSurfNum = HydrRadSys(Item)%SurfacePtr(SurfNum)
      IF (CheckSurfNum == 0) CYCLE
      IF (AssignedAsRadiantSurface(CheckSurfNum)) THEN
        CALL ShowSevereError('Surface '//TRIM(Surface(CheckSurfNum)%Name)//' is referenced by more than one '// &
                             'radiant system--this is not allowed')
        ErrorsFound = .TRUE.
      ELSE
        AssignedAsRadiantSurface(CheckSurfNum) = .TRUE.
      END IF
          ! Also check the other side of interzone partitions
      IF ( (Surface(CheckSurfNum)%ExtBoundCond > 0) .AND. &
           (Surface(CheckSurfNum)%ExtBoundCond /= CheckSurfNum) ) THEN
        IF (AssignedAsRadiantSurface(Surface(CheckSurfNum)%ExtBoundCond)) THEN
          CALL ShowSevereError('Interzone surface '//TRIM(Surface(Surface(CheckSurfNum)%ExtBoundCond)%Name)// &
                               ' is referenced by more than one radiant system--this is not allowed')
          ErrorsFound = .TRUE.
        ELSE
          AssignedAsRadiantSurface(Surface(CheckSurfNum)%ExtBoundCond) = .TRUE.
        END IF
      END IF
    END DO
  END DO

  DO Item = 1, NumOfCFloLowTempRadSys
    DO SurfNum = 1, CFloRadSys(Item)%NumOfSurfaces
      CheckSurfNum = CFloRadSys(Item)%SurfacePtr(SurfNum)
      IF (CheckSurfNum == 0) CYCLE
      IF (AssignedAsRadiantSurface(CheckSurfNum)) THEN
        CALL ShowSevereError('Surface '//TRIM(Surface(CheckSurfNum)%Name)//' is referenced by more than one '// &
                             'radiant system--this is not allowed')
        ErrorsFound = .TRUE.
      ELSE
        AssignedAsRadiantSurface(CheckSurfNum) = .TRUE.
      END IF
          ! Also check the other side of interzone partitions
      IF ( (Surface(CheckSurfNum)%ExtBoundCond > 0) .AND. &
           (Surface(CheckSurfNum)%ExtBoundCond /= CheckSurfNum) ) THEN
        IF (AssignedAsRadiantSurface(Surface(CheckSurfNum)%ExtBoundCond)) THEN
          CALL ShowSevereError('Interzone surface '//TRIM(Surface(Surface(CheckSurfNum)%ExtBoundCond)%Name)// &
                               ' is referenced by more than one radiant system--this is not allowed')
          ErrorsFound = .TRUE.
        ELSE
          AssignedAsRadiantSurface(Surface(CheckSurfNum)%ExtBoundCond) = .TRUE.
        END IF
      END IF
    END DO
  END DO

  DO Item = 1, NumOfElecLowTempRadSys
    DO SurfNum = 1, ElecRadSys(Item)%NumOfSurfaces
      CheckSurfNum = ElecRadSys(Item)%SurfacePtr(SurfNum)
      IF (CheckSurfNum == 0) CYCLE
      IF (AssignedAsRadiantSurface(CheckSurfNum)) THEN
        CALL ShowSevereError('Surface '//TRIM(Surface(CheckSurfNum)%Name)//' is referenced by more than one '// &
                             'radiant system--this is not allowed')
        ErrorsFound = .TRUE.
      ELSE
        AssignedAsRadiantSurface(CheckSurfNum) = .TRUE.
      END IF
          ! Also check the other side of interzone partitions
      IF ( (Surface(CheckSurfNum)%ExtBoundCond > 0) .AND. &
           (Surface(CheckSurfNum)%ExtBoundCond /= CheckSurfNum) ) THEN
        IF (AssignedAsRadiantSurface(Surface(CheckSurfNum)%ExtBoundCond)) THEN
          CALL ShowSevereError('Interzone surface '//TRIM(Surface(Surface(CheckSurfNum)%ExtBoundCond)%Name)// &
                               ' is referenced by more than one radiant system--this is not allowed')
          ErrorsFound = .TRUE.
        ELSE
          AssignedAsRadiantSurface(Surface(CheckSurfNum)%ExtBoundCond) = .TRUE.
        END IF
      END IF
    END DO
  END DO

  DEALLOCATE(AssignedAsRadiantSurface)

!  DEALLOCATE(SurfList)

  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in input. Preceding conditions cause termination.')
  END IF

          ! Set up the output variables for low temperature radiant systems
  DO Item = 1, NumOfHydrLowTempRadSys
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Rate [W]',    &
                              HydrRadSys(Item)%HeatPower,'System','Average', &
                              HydrRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Energy [J]', &
                              HydrRadSys(Item)%HeatEnergy,'System','Sum',HydrRadSys(Item)%Name, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Fluid Energy [J]', &
                              HydrRadSys(Item)%HeatEnergy,'System','Sum',HydrRadSys(Item)%Name, &
                                ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='HEATINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Radiant HVAC Cooling Rate [W]',    &
                              HydrRadSys(Item)%CoolPower,'System','Average', &
                              HydrRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Cooling Energy [J]', &
                              HydrRadSys(Item)%CoolEnergy,'System','Sum',HydrRadSys(Item)%Name, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Radiant HVAC Cooling Fluid Energy [J]', &
                              HydrRadSys(Item)%CoolEnergy,'System','Sum',HydrRadSys(Item)%Name, &
                                ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Radiant HVAC Mass Flow Rate [kg/s]',      &
                              HydrRadSys(Item)%WaterMassFlowRate,'System','Average', &
                              HydrRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Inlet Temperature [C]',     &
                              HydrRadSys(Item)%WaterInletTemp,'System','Average', &
                              HydrRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Outlet Temperature [C]',     &
                              HydrRadSys(Item)%WaterOutletTemp,'System','Average', &
                              HydrRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Moisture Condensation Time [s]', &
                              HydrRadSys(Item)%CondCausedTimeOff,'System','Sum',HydrRadSys(Item)%Name)
    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSInternalVariable('Hydronic Low Temp Radiant Design Water Volume Flow Rate for Heating',  &
                                     HydrRadSys(Item)%Name, '[m3/s]', HydrRadSys(Item)%WaterVolFlowMaxHeat )
      CALL SetupEMSInternalVariable('Hydronic Low Temp Radiant Design Water Volume Flow Rate for Cooling',  &
                                     HydrRadSys(Item)%Name, '[m3/s]', HydrRadSys(Item)%WaterVolFlowMaxCool )
      CALL SetupEMSActuator('Hydronic Low Temp Radiant', HydrRadSys(Item)%Name, 'Water Mass Flow Rate', '[kg/s]', &
                                   HydrRadSys(Item)%EMSOverrideOnWaterMdot, HydrRadSys(Item)%EMSWaterMdotOverrideValue )
    ENDIF
  END DO

  DO Item = 1, NumOfCFloLowTempRadSys
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Rate [W]', &
                              CFloRadSys(Item)%HeatPower,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Energy [J]',       &
                              CFloRadSys(Item)%HeatEnergy,'System','Sum',CFloRadSys(Item)%Name, &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Fluid Heat Transfer Energy [J]',       &
                              CFloRadSys(Item)%HeatEnergy,'System','Sum',CFloRadSys(Item)%Name, &
                              ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='HEATINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Radiant HVAC Cooling Rate [W]', &
                              CFloRadSys(Item)%CoolPower,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Cooling Energy [J]',       &
                              CFloRadSys(Item)%CoolEnergy,'System','Sum',CFloRadSys(Item)%Name, &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Radiant HVAC Cooling Fluid Heat Transfer Energy [J]',       &
                              CFloRadSys(Item)%CoolEnergy,'System','Sum',CFloRadSys(Item)%Name, &
                              ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Radiant HVAC Mass Flow Rate [kg/s]', &
                              CFloRadSys(Item)%WaterMassFlowRate,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Injection Mass Flow Rate [kg/s]', &
                              CFloRadSys(Item)%WaterInjectionRate,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Recirculation Mass Flow Rate [kg/s]', &
                              CFloRadSys(Item)%WaterRecircRate,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Inlet Temperature [C]', &
                              CFloRadSys(Item)%WaterInletTemp,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Outlet Temperature [C]', &
                              CFloRadSys(Item)%WaterOutletTemp,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Pump Inlet Temperature [C]', &
                              CFloRadSys(Item)%PumpInletTemp,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Pump Electric Power [W]', &
                              CFloRadSys(Item)%PumpPower,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Pump Electric Energy [J]', &
                              CFloRadSys(Item)%PumpEnergy,'System','Sum',CFloRadSys(Item)%Name,      &
                              ResourceTypeKey='Electric',EndUseKey='Pumps',GroupKey='Plant')
    CALL SetupOutputVariable('Zone Radiant HVAC Pump Mass Flow Rate [kg/s]', &
                              CFloRadSys(Item)%PumpMassFlowRate,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Pump Fluid Heat Gain Rate [W]', &
                              CFloRadSys(Item)%PumpHeattoFluid,'System','Average',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Pump Fluid Heat Gain Energy [J]', &
                              CFloRadSys(Item)%PumpHeattoFluidEnergy,'System','Sum',CFloRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Moisture Condensation Time [s]', &
                              CFloRadSys(Item)%CondCausedTimeOff,'System','Sum',CFloRadSys(Item)%Name)
    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSInternalVariable('Constant Flow Low Temp Radiant Design Water Mass Flow Rate',  &
                                     CFloRadSys(Item)%Name, '[m3/s]', CFloRadSys(Item)%WaterVolFlowMax )
      CALL SetupEMSActuator('Constant Flow Low Temp Radiant', CFloRadSys(Item)%Name, 'Water Mass Flow Rate', '[kg/s]', &
                                   CFloRadSys(Item)%EMSOverrideOnWaterMdot, CFloRadSys(Item)%EMSWaterMdotOverrideValue )
    ENDIF
  END DO

  DO Item = 1, NumOfElecLowTempRadSys
    CALL SetupOutputVariable('Zone Radiant HVAC Electric Power [W]',  &
                              ElecRadSys(Item)%ElecPower,'System','Average', &
                              ElecRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Electric Energy [J]',               &
                              ElecRadSys(Item)%ElecEnergy,'System','Sum',ElecRadSys(Item)%Name, &
                              ResourceTypeKey='ELECTRICITY',EndUseKey='Heating',GroupKey='System')
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Rate [W]', &
                              ElecRadSys(Item)%HeatPower,'System','Average',ElecRadSys(Item)%Name)
    CALL SetupOutputVariable('Zone Radiant HVAC Heating Energy [J]',       &
                              ElecRadSys(Item)%HeatEnergy,'System','Sum',ElecRadSys(Item)%Name, &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
  END DO

  RETURN

END SUBROUTINE GetLowTempRadiantSystem


SUBROUTINE InitLowTempRadiantSystem(FirstHVACIteration,RadSysNum,SystemType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes variables relating to low temperature radiant
          ! systems.

          ! METHODOLOGY EMPLOYED:
          ! Simply initializes whatever needs initializing.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,        ONLY : NumOfZones, BeginEnvrnFlag, AnyPlantInModel
  USE DataLoopNode,       ONLY : Node
  USE ScheduleManager,    ONLY : GetCurrentScheduleValue
  USE DataZoneEquipment,  ONLY : ZoneEquipInputsFilled,CheckZoneEquipmentList
  USE DataPlant,          ONLY : ScanPlantLoopsForObject, PlantLoop, TypeOf_LowTempRadiant_VarFlow, &
                                 TypeOf_LowTempRadiant_ConstFlow
  USE PlantUtilities,     ONLY : SetComponentFlowRate, InitComponentNodes
  USE FluidProperties,    ONLY : GetDensityGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT(IN) :: RadSysNum  ! Index for the low temperature radiant system under consideration within the derived types
  INTEGER, INTENT(IN) :: SystemType ! Type of radiant system: hydronic, constant flow, or electric

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: ZeroTol = 0.0000001d0        ! Smallest non-zero value allowed

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: CurrentFlowSchedule   ! Schedule value for flow fraction in a constant flow radiant system
  LOGICAL, SAVE  :: ErrorsFound = .FALSE. ! In one-time initializations
  CHARACTER(len=10) Errout                ! Message for errors
  LOGICAL, SAVE  :: FirstTime = .TRUE.    ! For one-time initializations
  INTEGER        :: RadNum                ! Number of the radiant system (DO loop counter)
  INTEGER        :: RadSurfNum            ! Number of the radiant system surface (DO loop counter)
  INTEGER        :: SurfNum               ! Intermediate variable for keeping track of the surface number
  REAL(r64)      :: TotalEffic            ! Intermediate calculation variable for total pump efficiency
  INTEGER        :: WaterNodeIn           ! Node number for water inlet node
  INTEGER        :: ZoneNum               ! Intermediate variable for keeping track of the zone number
  LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:)  :: MyEnvrnFlagHydr
  LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:)  :: MyEnvrnFlagCFlo
  LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:)  :: MyEnvrnFlagElec
  LOGICAL, SAVE  :: MyEnvrnFlagGeneral = .TRUE.
  LOGICAL,SAVE   :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer        :: Loop
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.           ! Initialization flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlagHydr
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlagCFlo
  REAL(r64)  :: mdot ! local fluid mass flow rate
  REAL(r64)  :: rho  ! local fluid density
  LOGICAL    :: errFlag
          ! FLOW:
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlagHydr(NumOfHydrLowTempRadSys))
    ALLOCATE(MyEnvrnFlagCFlo(NumOfCFloLowTempRadSys))
    ALLOCATE(MyEnvrnFlagElec(NumOfElecLowTempRadSys))
    ALLOCATE(MyPlantScanFlagHydr(NumOfHydrLowTempRadSys))
    ALLOCATE(MyPlantScanFlagCFlo(NumOfCFloLowTempRadSys))
    MyPlantScanFlagHydr  = .TRUE.
    MyPlantScanFlagCFlo  = .TRUE.
    MyEnvrnFlagHydr   = .TRUE.
    MyEnvrnFlagCFlo   = .TRUE.
    MyEnvrnFlagElec   = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (FirstTime) THEN

    ALLOCATE(ZeroSourceSumHATsurf(NumOfZones))
    ZeroSourceSumHATsurf = 0.0D0
    ALLOCATE(QRadSysSrcAvg(TotSurfaces))
    QRadSysSrcAvg = 0.0D0
    ALLOCATE(LastQRadSysSrc(TotSurfaces))
    LastQRadSysSrc = 0.0D0
    ALLOCATE(LastSysTimeElapsed(TotSurfaces))
    LastSysTimeElapsed = 0.0D0
    ALLOCATE(LastTimeStepSys(TotSurfaces))
    LastTimeStepSys = 0.0D0
    ALLOCATE(MySizeFlagHydr(NumOfHydrLowTempRadSys))
    ALLOCATE(MySizeFlagCFlo(NumOfCFloLowTempRadSys))
    ALLOCATE(MySizeFlagElec(NumOfElecLowTempRadSys))
    MySizeFlagHydr = .TRUE.
    MySizeFlagCFlo = .TRUE.
    MySizeFlagElec = .TRUE.

          ! Initialize total areas for all radiant systems
    DO RadNum = 1, NumOfHydrLowTempRadSys
      HydrRadSys(RadNum)%TotalSurfaceArea = 0.0d0
      DO SurfNum = 1, HydrRadSys(RadNum)%NumOfSurfaces
        HydrRadSys(RadNum)%TotalSurfaceArea = HydrRadSys(RadNum)%TotalSurfaceArea &
                                                +Surface(HydrRadSys(RadNum)%SurfacePtr(SurfNum))%Area
      END DO
    END DO
    DO RadNum = 1, NumOfCFloLowTempRadSys
      CFloRadSys(RadNum)%TotalSurfaceArea = 0.0d0
      DO SurfNum = 1, CFloRadSys(RadNum)%NumOfSurfaces
        CFloRadSys(RadNum)%TotalSurfaceArea = CFloRadSys(RadNum)%TotalSurfaceArea &
                                                +Surface(CFloRadSys(RadNum)%SurfacePtr(SurfNum))%Area
      END DO
    END DO
    DO RadNum = 1, NumOfElecLowTempRadSys
      ElecRadSys(RadNum)%TotalSurfaceArea = 0.0d0
      DO SurfNum = 1, ElecRadSys(RadNum)%NumOfSurfaces
        ElecRadSys(RadNum)%TotalSurfaceArea = ElecRadSys(RadNum)%TotalSurfaceArea &
                                                +Surface(ElecRadSys(RadNum)%SurfacePtr(SurfNum))%Area
      END DO
    END DO

          ! Check pump parameters for constant flow hydronic radiant systems
    DO RadNum = 1, NumOfCFloLowTempRadSys
          ! Calculate the efficiency for each pump: The calculation
          ! is based on the PMPSIM code in the ASHRAE Secondary Toolkit
      IF ( (CFloRadSys(RadNum)%NomPowerUse > ZeroTol) .AND. (CFloRadSys(RadNum)%MotorEffic > ZeroTol) ) THEN
        TotalEffic = CFloRadSys(RadNum)%WaterVolFlowMax * CFloRadSys(RadNum)%NomPumpHead  &
                    /CFloRadSys(RadNum)%NomPowerUse
        CFloRadSys(RadNum)%PumpEffic = TotalEffic / CFloRadSys(RadNum)%MotorEffic
        IF (CFloRadSys(RadNum)%PumpEffic < .50d0) THEN
          WRITE(Errout,'(F10.2)') CFloRadSys(RadNum)%PumpEffic*100.d0
          CALL ShowWarningError('Check input. Calc Pump Efficiency='//TRIM(ADJUSTL(Errout))//   &
                                '% which is less than 50%, for pump in radiant system '//TRIM(CFloRadSys(RadNum)%Name))
        ELSE IF ( (CFloRadSys(RadNum)%PumpEffic > 0.95d0) .and. (CFloRadSys(RadNum)%PumpEffic <= 1.0d0) ) THEN
          WRITE(Errout,'(F10.2)') CFloRadSys(RadNum)%PumpEffic*100.d0
          CALL ShowWarningError('Check input.  Calc Pump Efficiency='//TRIM(ADJUSTL(Errout))//  &
                                '% is approaching 100%, for pump in radiant system '//TRIM(CFloRadSys(RadNum)%Name))
        ELSE IF (CFloRadSys(RadNum)%PumpEffic > 1.0d0) THEN
          WRITE(Errout,'(F10.2)') CFloRadSys(RadNum)%PumpEffic*100.d0
          CALL ShowSevereError('Check input.  Calc Pump Efficiency='//TRIM(ADJUSTL(Errout))//  &
                                '% which is bigger than 100%, for pump in radiant system '//TRIM(CFloRadSys(RadNum)%Name))
          ErrorsFound=.true.
        END IF
      ELSE
        CALL ShowSevereError('Check input.  Pump nominal power and motor efficiency cannot be 0, for pump='//  &
                              TRIM(CFloRadSys(RadNum)%Name))
        ErrorsFound=.true.
      END IF
    END DO

    FirstTime      = .FALSE.

  END IF

  IF (SystemType == HydronicSystem) THEN
    IF ( MyPlantScanFlagHydr(RadSysNum) .and.  ALLOCATED(PlantLoop) ) THEN
      errFlag=.false.
      IF (HydrRadSys(RadSysNum)%HotWaterInNode > 0) THEN
        CALL ScanPlantLoopsForObject( HydrRadSys(RadSysNum)%Name , &
                                     TypeOf_LowTempRadiant_VarFlow, &
                                     HydrRadSys(RadSysNum)%HWLoopNum, &
                                     HydrRadSys(RadSysNum)%HWLoopSide, &
                                     HydrRadSys(RadSysNum)%HWBranchNum, &
                                     HydrRadSys(RadSysNum)%HWCompNum, &
                                     InletNodeNumber =  HydrRadSys(RadSysNum)%HotWaterInNode,  &
                                     errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitLowTempRadiantSystem: Program terminated due to previous condition(s).')
        ENDIF
      ENDIF
      IF (HydrRadSys(RadSysNum)%ColdWaterInNode > 0) THEN
        CALL ScanPlantLoopsForObject( HydrRadSys(RadSysNum)%Name , &
                                     TypeOf_LowTempRadiant_VarFlow, &
                                     HydrRadSys(RadSysNum)%CWLoopNum, &
                                     HydrRadSys(RadSysNum)%CWLoopSide, &
                                     HydrRadSys(RadSysNum)%CWBranchNum, &
                                     HydrRadSys(RadSysNum)%CWCompNum, &
                                     InletNodeNumber =  HydrRadSys(RadSysNum)%ColdWaterInNode,  &
                                     errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitLowTempRadiantSystem: Program terminated due to previous condition(s).')
        ENDIF
      ENDIF
      MyPlantScanFlagHydr(RadSysNum) = .FALSE.
    ELSEIF (MyPlantScanFlagHydr(RadSysNum) .AND. .NOT. AnyPlantInModel) THEN
      MyPlantScanFlagHydr(RadSysNum) = .FALSE.
    ENDIF
  ENDIF

  IF (SystemType == ConstantFlowSystem) THEN
    IF ( MyPlantScanFlagCFlo(RadSysNum) .and.  ALLOCATED(PlantLoop) ) THEN
      errFlag=.false.
      IF (CFloRadSys(RadSysNum)%HotWaterInNode > 0) THEN
        CALL ScanPlantLoopsForObject( CFloRadSys(RadSysNum)%Name , &
                                       TypeOf_LowTempRadiant_ConstFlow, &
                                       CFloRadSys(RadSysNum)%HWLoopNum, &
                                       CFloRadSys(RadSysNum)%HWLoopSide, &
                                       CFloRadSys(RadSysNum)%HWBranchNum, &
                                       CFloRadSys(RadSysNum)%HWCompNum, &
                                       InletNodeNumber =  CFloRadSys(RadSysNum)%HotWaterInNode,  &
                                       errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitLowTempRadiantSystem: Program terminated due to previous condition(s).')
        ENDIF
      ENDIF
      IF (CFloRadSys(RadSysNum)%ColdWaterInNode > 0) THEN
        CALL ScanPlantLoopsForObject( CFloRadSys(RadSysNum)%Name , &
                                     TypeOf_LowTempRadiant_ConstFlow, &
                                     CFloRadSys(RadSysNum)%CWLoopNum, &
                                     CFloRadSys(RadSysNum)%CWLoopSide, &
                                     CFloRadSys(RadSysNum)%CWBranchNum, &
                                     CFloRadSys(RadSysNum)%CWCompNum, &
                                     InletNodeNumber =  CFloRadSys(RadSysNum)%ColdWaterInNode,  &
                                     errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitLowTempRadiantSystem: Program terminated due to previous condition(s).')
        ENDIF
      ENDIF
      MyPlantScanFlagCFlo(RadSysNum) = .FALSE.
    ELSEIF ( MyPlantScanFlagCFlo(RadSysNum) .AND. .NOT. AnyPlantInModel) THEN
      MyPlantScanFlagCFlo(RadSysNum) = .FALSE.
    ENDIF
  ENDIF


  ! need to check all units to see if they are on Zone Equipment List or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    DO Loop=1,TotalNumOfRadSystems
      SELECT CASE(RadSysTypes(Loop)%SystemType)

      CASE(HydronicSystem)
        IF (CheckZoneEquipmentList('ZoneHVAC:LowTemperatureRadiant:VariableFlow',RadSysTypes(Loop)%Name)) CYCLE
        CALL ShowSevereError('InitLowTempRadiantSystem: Unit=[ZoneHVAC:LowTemperatureRadiant:VariableFlow,'//  &
           TRIM(RadSysTypes(Loop)%Name)//  &
             '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
      CASE(ConstantFlowSystem)
        IF (CheckZoneEquipmentList('ZoneHVAC:LowTemperatureRadiant:ConstantFlow',RadSysTypes(Loop)%Name)) CYCLE
        CALL ShowSevereError('InitLowTempRadiantSystem: Unit=[ZoneHVAC:LowTemperatureRadiant:ConstantFlow,'//  &
           TRIM(RadSysTypes(Loop)%Name)//  &
             '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
      CASE(ElectricSystem)
        IF (CheckZoneEquipmentList('ZoneHVAC:LowTemperatureRadiant:Electric',RadSysTypes(Loop)%Name)) CYCLE
        CALL ShowSevereError('InitLowTempRadiantSystem: Unit=[ZoneHVAC:LowTemperatureRadiant:Electric,'//  &
           TRIM(RadSysTypes(Loop)%Name)//  &
             '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
      CASE DEFAULT  ! Illegal system, but checked earlier
      END SELECT
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. (SystemType==HydronicSystem) ) THEN
    IF (MySizeFlagHydr(RadSysNum) .AND. .NOT. MyPlantScanFlagHydr(RadSysNum)) THEN
      ! for each radiant system do the sizing once.
      CALL SizeLowTempRadiantSystem(RadSysNum,SystemType)
      MySizeFlagHydr(RadSysNum) = .FALSE.

      ! Can this system actually do cooling?
      IF ((HydrRadSys(RadSysNum)%WaterVolFlowMaxCool  > 0.0d0) .AND. &
        (HydrRadSys(RadSysNum)%ColdWaterInNode   > 0)   .AND. &
        (HydrRadSys(RadSysNum)%ColdWaterOutNode  > 0)   .AND. &
        (HydrRadSys(RadSysNum)%ColdSetptSchedPtr > 0) ) THEN
        HydrRadSys(RadSysNum)%CoolingSystem = .TRUE.
      END IF

          ! Can this system actually do heating?
      IF ((HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat > 0.0d0) .AND. &
        (HydrRadSys(RadSysNum)%HotWaterInNode   > 0)   .AND. &
        (HydrRadSys(RadSysNum)%HotWaterOutNode  > 0)   .AND. &
        (HydrRadSys(RadSysNum)%HotSetptSchedPtr > 0) ) THEN
        HydrRadSys(RadSysNum)%HeatingSystem = .TRUE.
      END IF

     !set design mass flow rates
      IF (HydrRadSys(RadSysNum)%HotWaterInNode > 0) THEN
        rho = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidName, &
                               60.d0, &
                               PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidIndex, &
                               'InitLowTempRadiantSystem')
        HydrRadSys(RadSysNum)%WaterFlowMaxHeat = rho*HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat
        CALL InitComponentNodes(0.0d0, HydrRadSys(RadSysNum)%WaterFlowMaxHeat, &
                            HydrRadSys(RadSysNum)%HotWaterInNode, &
                            HydrRadSys(RadSysNum)%HotWaterOutNode, &
                            HydrRadSys(RadSysNum)%HWLoopNum, &
                            HydrRadSys(RadSysNum)%HWLoopSide, &
                            HydrRadSys(RadSysNum)%HWBranchNum, &
                            HydrRadSys(RadSysNum)%HWCompNum  )
      ENDIF
      IF (HydrRadSys(RadSysNum)%ColdWaterInNode > 0) THEN
        rho = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidName, &
                               InitConvTemp, &
                               PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidIndex, &
                               'InitLowTempRadiantSystem')
        HydrRadSys(RadSysNum)%WaterFlowMaxCool = rho*HydrRadSys(RadSysNum)%WaterVolFlowMaxCool
        CALL InitComponentNodes(0.0d0, HydrRadSys(RadSysNum)%WaterFlowMaxCool, &
                            HydrRadSys(RadSysNum)%ColdWaterInNode, &
                            HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                            HydrRadSys(RadSysNum)%CWLoopNum, &
                            HydrRadSys(RadSysNum)%CWLoopSide, &
                            HydrRadSys(RadSysNum)%CWBranchNum, &
                            HydrRadSys(RadSysNum)%CWCompNum  )
      ENDIF
    END IF
  END IF

  IF ( .NOT. SysSizingCalc .AND. (SystemType==ConstantFlowSystem) ) THEN
    IF (MySizeFlagCFlo(RadSysNum) .AND. .NOT. MyPlantScanFlagCFlo(RadSysNum)) THEN
      ! for each radiant system do the sizing once.
      CALL SizeLowTempRadiantSystem(RadSysNum,SystemType)

            !set design mass flow rates
      IF (CFloRadSys(RadSysNum)%HotWaterInNode > 0) THEN
        rho = GetDensityGlycol(PlantLoop(CFloRadSys(RadSysNum)%HWLoopNum)%FluidName, &
                               60.d0, &
                               PlantLoop(CFloRadSys(RadSysNum)%HWLoopNum)%FluidIndex, &
                               'InitLowTempRadiantSystem')
        CFloRadSys(RadSysNum)%HotDesignWaterMassFlowRate = rho*CFloRadSys(RadSysNum)%WaterVolFlowMax
        CALL InitComponentNodes(0.0d0, CFloRadSys(RadSysNum)%HotDesignWaterMassFlowRate, &
                            CFloRadSys(RadSysNum)%HotWaterInNode, &
                            CFloRadSys(RadSysNum)%HotWaterOutNode, &
                            CFloRadSys(RadSysNum)%HWLoopNum, &
                            CFloRadSys(RadSysNum)%HWLoopSide, &
                            CFloRadSys(RadSysNum)%HWBranchNum, &
                            CFloRadSys(RadSysNum)%HWCompNum  )
      ENDIF
      IF (CFloRadSys(RadSysNum)%ColdWaterInNode > 0) THEN
        rho = GetDensityGlycol(PlantLoop(CFloRadSys(RadSysNum)%CWLoopNum)%FluidName, &
                               InitConvTemp, &
                               PlantLoop(CFloRadSys(RadSysNum)%CWLoopNum)%FluidIndex, &
                               'InitLowTempRadiantSystem')
        CFloRadSys(RadSysNum)%ColdDesignWaterMassFlowRate = rho*CFloRadSys(RadSysNum)%WaterVolFlowMax
        CALL InitComponentNodes(0.0d0, CFloRadSys(RadSysNum)%ColdDesignWaterMassFlowRate, &
                            CFloRadSys(RadSysNum)%ColdWaterInNode, &
                            CFloRadSys(RadSysNum)%ColdWaterOutNode, &
                            CFloRadSys(RadSysNum)%CWLoopNum, &
                            CFloRadSys(RadSysNum)%CWLoopSide, &
                            CFloRadSys(RadSysNum)%CWBranchNum, &
                            CFloRadSys(RadSysNum)%CWCompNum  )
      ENDIF
      MySizeFlagCFlo(RadSysNum) = .FALSE.
    END IF
  END IF

  IF ( .NOT. SysSizingCalc .AND. (SystemType==ElectricSystem) ) THEN
    IF (MySizeFlagElec(RadSysNum)) THEN
      ! for each radiant system do the sizing once.
      CALL SizeLowTempRadiantSystem(RadSysNum,SystemType)
      MySizeFlagElec(RadSysNum) = .FALSE.
    END IF
  END IF

  IF (BeginEnvrnFlag .and. MyEnvrnFlagGeneral) THEN
    ZeroSourceSumHATsurf = 0.D0
    QRadSysSrcAvg        = 0.D0
    LastQRadSysSrc       = 0.D0
    LastSysTimeElapsed   = 0.D0
    LastTimeStepSys      = 0.D0
    MyEnvrnFlagGeneral   = .FALSE.
  ENDIF
  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlagGeneral = .TRUE.

  IF (SystemType == HydronicSystem) THEN
    IF (BeginEnvrnFlag .and. MyEnvrnFlagHydr(RadSysNum)) THEN
      HydrRadSys(RadSysNum)%HeatPower          = 0.d0
      HydrRadSys(RadSysNum)%HeatEnergy         = 0.d0
      HydrRadSys(RadSysNum)%CoolPower          = 0.d0
      HydrRadSys(RadSysNum)%CoolEnergy         = 0.d0
      HydrRadSys(RadSysNum)%WaterInletTemp     = 0.d0
      HydrRadSys(RadSysNum)%WaterOutletTemp    = 0.d0
      HydrRadSys(RadSysNum)%WaterMassFlowRate  = 0.d0

      IF (.NOT. MyPlantScanFlagHydr(RadSysNum)) THEN
        IF (HydrRadSys(RadSysNum)%HotWaterInNode > 0) THEN
          CALL InitComponentNodes(0.0d0, HydrRadSys(RadSysNum)%WaterFlowMaxHeat, &
                            HydrRadSys(RadSysNum)%HotWaterInNode, &
                            HydrRadSys(RadSysNum)%HotWaterOutNode, &
                            HydrRadSys(RadSysNum)%HWLoopNum, &
                            HydrRadSys(RadSysNum)%HWLoopSide, &
                            HydrRadSys(RadSysNum)%HWBranchNum, &
                            HydrRadSys(RadSysNum)%HWCompNum  )
        ENDIF
        IF (HydrRadSys(RadSysNum)%ColdWaterInNode > 0) THEN
          CALL InitComponentNodes(0.0d0, HydrRadSys(RadSysNum)%WaterFlowMaxCool, &
                            HydrRadSys(RadSysNum)%ColdWaterInNode, &
                            HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                            HydrRadSys(RadSysNum)%CWLoopNum, &
                            HydrRadSys(RadSysNum)%CWLoopSide, &
                            HydrRadSys(RadSysNum)%CWBranchNum, &
                            HydrRadSys(RadSysNum)%CWCompNum  )
        ENDIF
      ENDIF
      MyEnvrnFlagHydr(RadSysNum)=.false.
    ENDIF
  ENDIF !NumOfHydrLowTempRadSys > 0
  IF (.NOT. BeginEnvrnFlag .AND. SystemType == HydronicSystem) MyEnvrnFlagHydr(RadSysNum)= .TRUE.

  IF (SystemType == ConstantFlowSystem) THEN
    IF (BeginEnvrnFlag .and. MyEnvrnFlagCFlo(RadSysNum)) THEN
      CFloRadSys(RadSysNum)%WaterInletTemp     = 0.0d0
      CFloRadSys(RadSysNum)%WaterOutletTemp    = 0.0d0
      CFloRadSys(RadSysNum)%PumpInletTemp      = 0.0d0
      CFloRadSys(RadSysNum)%WaterMassFlowRate  = 0.0d0
      CFloRadSys(RadSysNum)%WaterInjectionRate = 0.0d0
      CFloRadSys(RadSysNum)%WaterRecircRate    = 0.0d0
      CFloRadSys(RadSysNum)%HeatPower          = 0.0d0
      CFloRadSys(RadSysNum)%HeatEnergy         = 0.0d0
      CFloRadSys(RadSysNum)%CoolPower          = 0.0d0
      CFloRadSys(RadSysNum)%CoolEnergy         = 0.0d0
      CFloRadSys(RadSysNum)%PumpPower          = 0.0d0
      CFloRadSys(RadSysNum)%PumpMassFlowRate   = 0.0d0
      CFloRadSys(RadSysNum)%PumpHeattoFluid    = 0.0d0

      IF (.NOT. MyPlantScanFlagCFlo(RadSysNum)) THEN
        IF (CFloRadSys(RadSysNum)%HotWaterInNode > 0) THEN
          CALL InitComponentNodes(0.0d0, CFloRadSys(RadSysNum)%HotDesignWaterMassFlowRate, &
                            CFloRadSys(RadSysNum)%HotWaterInNode, &
                            CFloRadSys(RadSysNum)%HotWaterOutNode, &
                            CFloRadSys(RadSysNum)%HWLoopNum, &
                            CFloRadSys(RadSysNum)%HWLoopSide, &
                            CFloRadSys(RadSysNum)%HWBranchNum, &
                            CFloRadSys(RadSysNum)%HWCompNum  )
        ENDIF
        IF (CFloRadSys(RadSysNum)%ColdWaterInNode > 0) THEN
          CALL InitComponentNodes(0.0d0, CFloRadSys(RadSysNum)%ColdDesignWaterMassFlowRate, &
                            CFloRadSys(RadSysNum)%ColdWaterInNode, &
                            CFloRadSys(RadSysNum)%ColdWaterOutNode, &
                            CFloRadSys(RadSysNum)%CWLoopNum, &
                            CFloRadSys(RadSysNum)%CWLoopSide, &
                            CFloRadSys(RadSysNum)%CWBranchNum, &
                            CFloRadSys(RadSysNum)%CWCompNum  )
        ENDIF
      ENDIF
      MyEnvrnFlagCFlo(RadSysNum) = .FALSE.
    ENDIF
  ENDIF ! NumOfCFloLowTempRadSys > 0
  IF (.NOT. BeginEnvrnFlag .AND. SystemType == ConstantFlowSystem)  MyEnvrnFlagCFlo(RadSysNum) = .TRUE.

  IF (SystemType == ElectricSystem) THEN
    IF (BeginEnvrnFlag .and. MyEnvrnFlagElec(RadSysNum)) THEN
      ElecRadSys(RadSysNum)%HeatPower          = 0.0d0
      ElecRadSys(RadSysNum)%HeatEnergy         = 0.0d0
      ElecRadSys(RadSysNum)%ElecPower          = 0.0d0
      ElecRadSys(RadSysNum)%ElecEnergy         = 0.0d0
    ENDIF
    MyEnvrnFlagElec(RadSysNum)=.false.
  ENDIF
  IF (.not. BeginEnvrnFlag .AND. SystemType == ElectricSystem) MyEnvrnFlagElec(RadSysNum) = .TRUE.

  IF (SystemType==ConstantFlowSystem) THEN

        ! Can this system actually do heating?
    IF ( (CFloRadSys(RadSysNum)%WaterVolFlowMax        > 0.0d0) .AND. &
         (CFloRadSys(RadSysNum)%HotWaterInNode         > 0)   .AND. &
         (CFloRadSys(RadSysNum)%HotWaterOutNode        > 0)   .AND. &
         (CFloRadSys(RadSysNum)%HotWaterHiTempSchedPtr > 0)   .AND. &
         (CFloRadSys(RadSysNum)%HotWaterLoTempSchedPtr > 0)   .AND. &
         (CFloRadSys(RadSysNum)%HotCtrlHiTempSchedPtr  > 0)   .AND. &
         (CFloRadSys(RadSysNum)%HotCtrlLoTempSchedPtr  > 0) ) THEN
      CFloRadSys(RadSysNum)%HeatingSystem = .TRUE.
    END IF

    ! Can this system actually do cooling?
    IF ( (CFloRadSys(RadSysNum)%WaterVolFlowMax         > 0.0d0) .AND. &
         (CFloRadSys(RadSysNum)%ColdWaterInNode         > 0)   .AND. &
         (CFloRadSys(RadSysNum)%ColdWaterOutNode        > 0)   .AND. &
         (CFloRadSys(RadSysNum)%ColdWaterHiTempSchedPtr > 0)   .AND. &
         (CFloRadSys(RadSysNum)%ColdWaterLoTempSchedPtr > 0)   .AND. &
         (CFloRadSys(RadSysNum)%ColdCtrlHiTempSchedPtr  > 0)   .AND. &
         (CFloRadSys(RadSysNum)%ColdCtrlLoTempSchedPtr  > 0) ) THEN
      CFloRadSys(RadSysNum)%CoolingSystem = .TRUE.
    END IF

  END IF


  IF (BeginTimeStepFlag.AND.FirstHVACIteration) THEN    ! This is the first pass through in a particular time step

    SELECT CASE (SystemType)

      CASE (HydronicSystem)

        ZoneNum                       = HydrRadSys(RadSysNum)%ZonePtr
        ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum) ! Set this to figure what part of the load the radiant system meets
        DO RadSurfNum = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
          SurfNum                     = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
          QRadSysSrcAvg(SurfNum)      = 0.0D0  ! Initialize this variable to zero (radiant system defaults to off)
          LastQRadSysSrc(SurfNum)     = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
          LastSysTimeElapsed(SurfNum) = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
          LastTimeStepSys(SurfNum)    = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
        END DO

      CASE (ConstantFlowSystem)

        ZoneNum                       = CFloRadSys(RadSysNum)%ZonePtr
        ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum) ! Set this to figure what part of the load the radiant system meets
        DO RadSurfNum = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
          SurfNum                     = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
          QRadSysSrcAvg(SurfNum)      = 0.0D0  ! Initialize this variable to zero (radiant system defaults to off)
          LastQRadSysSrc(SurfNum)     = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
          LastSysTimeElapsed(SurfNum) = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
          LastTimeStepSys(SurfNum)    = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
        END DO

      CASE (ElectricSystem)

        ZoneNum                       = ElecRadSys(RadSysNum)%ZonePtr
        ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum) ! Set this to figure what part of the load the radiant system meets
        DO RadSurfNum = 1, ElecRadSys(RadSysNum)%NumOfSurfaces
          SurfNum                     = ElecRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
          QRadSysSrcAvg(SurfNum)      = 0.0D0  ! Initialize this variable to zero (radiant system defaults to off)
          LastQRadSysSrc(SurfNum)     = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
          LastSysTimeElapsed(SurfNum) = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
          LastTimeStepSys(SurfNum)    = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
        END DO

      CASE DEFAULT

        CALL ShowSevereError('Radiant system entered without specification of type: electric, constant flow, or hydronic?')
        CALL ShowContinueError('Occurs in Radiant System='//TRIM(HydrRadSys(RadSysNum)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')

    END SELECT

  END IF    ! ...for first pass through in a particular time step.

  SELECT CASE (SystemType)

    CASE (HydronicSystem)

          ! Initialize the appropriate node data
      IF (HydrRadSys(RadSysNum)%HeatingSystem) THEN
        mdot = 0.d0
        CALL SetComponentFlowRate ( mdot , &
                          HydrRadSys(RadSysNum)%HotWaterInNode, &
                          HydrRadSys(RadSysNum)%HotWaterOutNode, &
                          HydrRadSys(RadSysNum)%HWLoopNum, &
                          HydrRadSys(RadSysNum)%HWLoopSide, &
                          HydrRadSys(RadSysNum)%HWBranchNum, &
                          HydrRadSys(RadSysNum)%HWCompNum  )
      END IF
      IF (HydrRadSys(RadSysNum)%CoolingSystem) THEN
        mdot = 0.d0
        CALL SetComponentFlowRate ( mdot , &
                          HydrRadSys(RadSysNum)%ColdWaterInNode, &
                          HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                          HydrRadSys(RadSysNum)%CWLoopNum, &
                          HydrRadSys(RadSysNum)%CWLoopSide, &
                          HydrRadSys(RadSysNum)%CWBranchNum, &
                          HydrRadSys(RadSysNum)%CWCompNum  )
      END IF

    CASE (ConstantFlowSystem)
          ! Initialize the appropriate node data
      IF (CFloRadSys(RadSysNum)%HeatingSystem) THEN
        IF (CFloRadSys(RadSysNum)%VolFlowSchedPtr > 0) THEN
          CurrentFlowSchedule = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%VolFlowSchedPtr)
        ELSE
          CurrentFlowSchedule = 1.0d0 ! Allow user to avoid putting in a schedule (defaults to constant flow at all times)
        END IF
        IF (CurrentFlowSchedule > 1.0d0) CurrentFlowSchedule = 1.0d0    ! Do not allow more flow than design maximum
        IF (CurrentFlowSchedule < 0.0d0) CurrentFlowSchedule = 0.0d0    ! Do not allow negative flow

        CFloRadSys(RadSysNum)%WaterMassFlowRate = CFloRadSys(RadSysNum)%HotDesignWaterMassFlowRate * CurrentFlowSchedule

        IF (CFloRadSys(RadSysNum)%EMSOverrideOnWaterMdot) &
          CFloRadSys(RadSysNum)%WaterMassFlowRate = CFloRadSys(RadSysNum)%EMSWaterMdotOverrideValue

        CALL SetComponentFlowRate(CFloRadSys(RadSysNum)%WaterMassFlowRate, &
                                  CFloRadSys(RadSysNum)%HotWaterInNode, &
                                  CFloRadSys(RadSysNum)%HotWaterOutNode, &
                                  CFloRadSys(RadSysNum)%HWLoopNum, &
                                  CFloRadSys(RadSysNum)%HWLoopSide, &
                                  CFloRadSys(RadSysNum)%HWBranchNum, &
                                  CFloRadSys(RadSysNum)%HWCompNum )
      END IF
      IF (CFloRadSys(RadSysNum)%CoolingSystem) THEN
        IF (CFloRadSys(RadSysNum)%VolFlowSchedPtr > 0) THEN
          CurrentFlowSchedule = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%VolFlowSchedPtr)
        ELSE
          CurrentFlowSchedule = 1.0d0 ! Allow user to avoid putting in a schedule (defaults to constant flow at all times)
        END IF
        IF (CurrentFlowSchedule > 1.0d0) CurrentFlowSchedule = 1.0d0    ! Do not allow more flow than design maximum
        IF (CurrentFlowSchedule < 0.0d0) CurrentFlowSchedule = 0.0d0    ! Do not allow negative flow
        CFloRadSys(RadSysNum)%WaterMassFlowRate = CFloRadSys(RadSysNum)%ColdDesignWaterMassFlowRate &
                                                 *CurrentFlowSchedule

        IF (CFloRadSys(RadSysNum)%EMSOverrideOnWaterMdot) &
          CFloRadSys(RadSysNum)%WaterMassFlowRate = CFloRadSys(RadSysNum)%EMSWaterMdotOverrideValue

        CALL SetComponentFlowRate(CFloRadSys(RadSysNum)%WaterMassFlowRate, &
                                  CFloRadSys(RadSysNum)%ColdWaterInNode, &
                                  CFloRadSys(RadSysNum)%ColdWaterOutNode, &
                                  CFloRadSys(RadSysNum)%CWLoopNum, &
                                  CFloRadSys(RadSysNum)%CWLoopSide, &
                                  CFloRadSys(RadSysNum)%CWBranchNum, &
                                  CFloRadSys(RadSysNum)%CWCompNum)
      END IF

    CASE (ElectricSystem)

    CASE DEFAULT

  END SELECT

  OperatingMode = NotOperating ! System is not operating or can't operate; will be reset elsewhere, if necessary

  RETURN

END SUBROUTINE InitLowTempRadiantSystem

SUBROUTINE SizeLowTempRadiantSystem(RadSysNum,SystemType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing low temperature radiant components for which flow rates
          ! and tube length or max ekectric power have not been specified in the input

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays and plant sizing data. Maximum electric
          ! power is set to the design heat load. Tube length is calculated by rule-of-thumb from
          ! rge surface area.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE PlantUtilities,  ONLY : RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop, MyPlantSizingIndex
  USE General,         ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: RadSysNum  ! Index for the low temperature radiant system under consideration within the derived types
  INTEGER, INTENT(IN) :: SystemType ! Type of radiant system: hydronic, constant flow, or electric

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizNum     ! do loop index for plant sizing
  INTEGER             :: PltSizHeatNum ! index of plant sizing object for 1st heating loop
  INTEGER             :: PltSizCoolNum ! index of plant sizing object for 1st cooling loop
  INTEGER             :: SurfNum       ! surface index in radiant system data structure
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  REAL(r64)           :: rho
  REAL(r64)           :: Cp
  LOGICAL             :: IsAutosize              ! Indicator to autosize
  REAL(r64)           :: MaxElecPowerDes         ! Design electric power for reproting
  REAL(r64)           :: MaxElecPowerUser        ! User hard-sized electric power for reproting
  REAL(r64)           :: WaterVolFlowMaxHeatDes  ! Design hot water flow for reproting
  REAL(r64)           :: WaterVolFlowMaxHeatUser ! User hard-sized hot water flow for
  REAL(r64)           :: WaterVolFlowMaxCoolDes  ! Design chilled water flow for reproting
  REAL(r64)           :: WaterVolFlowMaxCoolUser ! User hard-sized chilled water flow for reproting
  REAL(r64)           :: TubeLengthDes           ! Design tube length for reproting
  REAL(r64)           :: TubeLengthUser          ! User hard-sized tube length for reproting

  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  MaxElecPowerDes = 0.0d0
  MaxElecPowerDes = 0.0d0
  WaterVolFlowMaxHeatDes = 0.0d0
  WaterVolFlowMaxHeatUser = 0.0d0
  WaterVolFlowMaxCoolDes = 0.0d0
  WaterVolFlowMaxCoolUser = 0.0d0
  TubeLengthDes = 0.0d0
  TubeLengthUser = 0.0d0

  IF (SystemType==ElectricSystem) THEN

    IF (ElecRadSys(RadSysNum)%MaxElecPower == AutoSize) THEN
      IsAutosize = .TRUE.
    END IF

    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation should continue
        IF (ElecRadSys(RadSysNum)%MaxElecPower > 0.0d0) THEN
          CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:Electric', ElecRadSys(RadSysNum)%Name, &
                                'User-Specified Maximum Electrical Power to Panel [W]', ElecRadSys(RadSysNum)%MaxElecPower)
        END IF
      ELSE ! Autosize or hard-size with sizing run
        CALL CheckZoneSizing('ZoneHVAC:LowTemperatureRadiant:Electric', ElecRadSys(RadSysNum)%Name)
        IF ((CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor) >= SmallLoad) THEN
          MaxElecPowerDes = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad *   &
                            CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor
        ELSE
          MaxElecPowerDes = 0.0d0
        END IF
        IF (IsAutosize) THEN
          ElecRadSys(RadSysNum)%MaxElecPower = MaxElecPowerDes
          CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:Electric', ElecRadSys(RadSysNum)%Name, &
                                'Design Size Maximum Electrical Power to Panel [W]', MaxElecPowerDes)
        ELSE ! Hard-size with sizing data
          IF (ElecRadSys(RadSysNum)%MaxElecPower > 0.0d0 .AND. MaxElecPowerDes > 0.0d0) THEN
            MaxElecPowerUser = ElecRadSys(RadSysNum)%MaxElecPower
            CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:Electric', ElecRadSys(RadSysNum)%Name, &
                                'Design Size Maximum Electrical Power to Panel [W]', MaxElecPowerDes, &
                                'User-Specified Maximum Electrical Power to Panel [W]', MaxElecPowerUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxElecPowerDes - MaxElecPowerUser)/MaxElecPowerUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeLowTempRadiantSystem: Potential issue with equipment sizing for ' &
                                     // 'ZoneHVAC:LowTemperatureRadiant:Electric = " '// &
                                      TRIM(HydrRadSys(RadSysNum)%Name)//'".')
                CALL ShowContinueError('User-Specified Maximum Electrical Power to Panel of '// &
                                      TRIM(RoundSigDigits(MaxElecPowerUser,2))// ' [W]')
                CALL ShowContinueError('differs from Design Size Maximum Electrical Power to Panel of ' // &
                                      TRIM(RoundSigDigits(MaxElecPowerDes,2))// ' [W]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  END IF

  IF (SystemType==HydronicSystem) THEN

    IF (HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat == AutoSize) THEN
      IsAutosize = .TRUE.
    END IF

    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation continue
        IF (HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat > 0.0d0) THEN
          CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name, &
                                'User-Specified Maximum Hot Water Flow [m3/s]', HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat)
        END IF
      ELSE ! Autosize or hard-size with sizing run
        CALL CheckZoneSizing('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name)

        IF (IsAutosize) THEN
          PltSizHeatNum = MyPlantSizingIndex('ZoneHVAC:LowTemperatureRadiant:VariableFlow',HydrRadSys(RadSysNum)%Name,&
                          HydrRadSys(RadSysNum)%HotWaterInNode,HydrRadSys(RadSysNum)%HotWaterOutNode,ErrorsFound)
          IF (PltSizHeatNum > 0) THEN
            IF ((CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad *   &
               CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor) >= SmallLoad) THEN
              rho = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidName, &
                             60.d0, &
                             PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidIndex, &
                             'SizeLowTempRadiantSystem')

              Cp  = GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidName, &
                             60.d0, &
                             PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidIndex, &
                             'SizeLowTempRadiantSystem')

              WaterVolFlowMaxHeatDes =   &
                 (CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor) / &
                                                        ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                                        Cp * rho )
            ELSE
              WaterVolFlowMaxHeatDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:LowTemperatureRadiant:VariableFlow' // ' Object=' &
                               //TRIM(HydrRadSys(RadSysNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat = WaterVolFlowMaxHeatDes
          CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name, &
                                  'Design Size Maximum Hot Water Flow [m3/s]', WaterVolFlowMaxHeatDes)
        ELSE ! hard-size with sizing data
          IF (HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat > 0.0d0 .AND. WaterVolFlowMaxHeatDes > 0.0d0) THEN
            WaterVolFlowMaxHeatUser = HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat
            CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name, &
                                  'Design Size Maximum Hot Water Flow [m3/s]', WaterVolFlowMaxHeatDes, &
                                  'User-Specified Maximum Hot Water Flow [m3/s]', WaterVolFlowMaxHeatUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(WaterVolFlowMaxHeatDes - WaterVolFlowMaxHeatUser)/WaterVolFlowMaxHeatUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeLowTempRadiantSystem: Potential issue with equipment sizing for ' &
                                     // 'ZoneHVAC:LowTemperatureRadiant:Electric = " '// &
                                      TRIM(HydrRadSys(RadSysNum)%Name)//'".')
                CALL ShowContinueError('User-Specified Maximum Hot Water Flow of '// &
                                      TRIM(RoundSigDigits(WaterVolFlowMaxHeatUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Hot Water Flow of ' // &
                                      TRIM(RoundSigDigits(WaterVolFlowMaxHeatDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF

    IsAutosize = .FALSE.
    IF (HydrRadSys(RadSysNum)%WaterVolFlowMaxCool == AutoSize) THEN
      IsAutosize = .TRUE.
    END IF
    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation continue
        IF (HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat > 0.0d0) THEN
          CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name, &
                                  'User-Specified Maximum Cold Water Flow [m3/s]', HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat)
        END IF
      ELSE ! Autosize or hard-size with sizing run
        CALL CheckZoneSizing('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name)
        IF (IsAutosize) THEN
          PltSizCoolNum = MyPlantSizingIndex('ZoneHVAC:LowTemperatureRadiant:VariableFlow',HydrRadSys(RadSysNum)%Name,&
                            HydrRadSys(RadSysNum)%ColdWaterInNode,HydrRadSys(RadSysNum)%ColdWaterOutNode,ErrorsFound)
          IF (PltSizCoolNum > 0) THEN
            IF ((CalcFinalZoneSizing(CurZoneEqNum)%DesCoolLoad *   &
                 CalcFinalZoneSizing(CurZoneEqNum)%CoolSizingFactor) >= SmallLoad) THEN
              rho = GetDensityGlycol(PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidName, &
                             5.d0, &
                             PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidIndex, &
                             'SizeLowTempRadiantSystem')

              Cp  = GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidName, &
                             5.d0, &
                             PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidIndex, &
                             'SizeLowTempRadiantSystem')

              WaterVolFlowMaxCoolDes =   &
               (CalcFinalZoneSizing(CurZoneEqNum)%DesCoolLoad * CalcFinalZoneSizing(CurZoneEqNum)%CoolSizingFactor) / &
                                                        ( PlantSizData(PltSizCoolNum)%DeltaT * &
                                                        Cp * rho )
            ELSE
              WaterVolFlowMaxCoolDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a cooling loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:LowTemperatureRadiant:VariableFlow' // ' Object=' &
                               //TRIM(HydrRadSys(RadSysNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          HydrRadSys(RadSysNum)%WaterVolFlowMaxCool = WaterVolFlowMaxCoolDes
          CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name, &
                                  'Design Size Maximum Cold Water Flow [m3/s]', WaterVolFlowMaxCoolDes)
        ELSE    ! hard-size with sizing data
          IF (HydrRadSys(RadSysNum)%WaterVolFlowMaxCool > 0.0d0 .AND. WaterVolFlowMaxCoolDes > 0.0d0) THEN
            WaterVolFlowMaxCoolUser = HydrRadSys(RadSysNum)%WaterVolFlowMaxCool
            CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name, &
                                  'Design Size Maximum Cold Water Flow [m3/s]', WaterVolFlowMaxCoolDes, &
                                  'User-Specified Maximum Cold Water Flow [m3/s]', WaterVolFlowMaxCoolUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(WaterVolFlowMaxCoolDes - WaterVolFlowMaxCoolUser)/WaterVolFlowMaxCoolUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeLowTempRadiantSystem: Potential issue with equipment sizing for ' &
                                      //'ZoneHVAC:LowTemperatureRadiant:Electric = " '// &
                                      TRIM(HydrRadSys(RadSysNum)%Name)//'".')
                CALL ShowContinueError('User-Specified Maximum Cool Water Flow of '// &
                                      TRIM(RoundSigDigits(WaterVolFlowMaxCoolUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Cool Water Flow of ' // &
                                      TRIM(RoundSigDigits(WaterVolFlowMaxCoolDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF

    IsAutosize = .FALSE.
    IF (HydrRadSys(RadSysNum)%TubeLength == AutoSize) THEN
      IsAutosize = .TRUE.
    END IF
    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation continue
        IF (HydrRadSys(RadSysNum)%TubeLength > 0.0d0) THEN
          CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name, &
                                'User-Specified Hydronic Tubing Length [m]', HydrRadSys(RadSysNum)%TubeLength)
        END IF
      ELSE ! Autosize or hard-size with sizing run
        ! assume tube spacing of 15 cm
        CALL CheckZoneSizing('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name)
        TubeLengthDes = HydrRadSys(RadSysNum)%TotalSurfaceArea / 0.15
        IF (IsAutosize) THEN
          HydrRadSys(RadSysNum)%TubeLength = TubeLengthDes
          CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name, &
                                'Design Size Hydronic Tubing Length [m]', TubeLengthDes)
        ELSE  ! hard-size with sizing data
          IF (HydrRadSys(RadSysNum)%TubeLength > 0.0d0 .AND. TubeLengthDes > 0.0d0) THEN
            TubeLengthUser = HydrRadSys(RadSysNum)%TubeLength
            CALL ReportSizingOutput('ZoneHVAC:LowTemperatureRadiant:VariableFlow', HydrRadSys(RadSysNum)%Name, &
                                'Design Size Hydronic Tubing Length [m]', TubeLengthDes, &
                                'User-Specified Hydronic Tubing Length [m]', TubeLengthUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(TubeLengthDes - TubeLengthUser)/TubeLengthUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeLowTempRadiantSystem: Potential issue with equipment sizing for ' &
                                      // 'ZoneHVAC:LowTemperatureRadiant:Electric = " '// &
                                      TRIM(HydrRadSys(RadSysNum)%Name)//'".')
                CALL ShowContinueError('User-Specified Hydronic Tubing Length of '// &
                                      TRIM(RoundSigDigits(TubeLengthUser,5))// ' [m]')
                CALL ShowContinueError('differs from Design Size Hydronic Tubing Length of ' // &
                                      TRIM(RoundSigDigits(TubeLengthDes,5))// ' [m]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF

    DO SurfNum = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
      IF (HydrRadSys(RadSysNum)%NumCircCalcMethod == CalculateFromLength) THEN
        HydrRadSys(RadSysNum)%NumCircuits(SurfNum) = (HydrRadSys(RadSysNum)%SurfaceFlowFrac(SurfNum) *   &
           HydrRadSys(RadSysNum)%TubeLength) / HydrRadSys(RadSysNum)%CircLength
        HydrRadSys(RadSysNum)%NumCircuits(SurfNum) = MAX(HydrRadSys(RadSysNum)%NumCircuits(SurfNum),1.0d0)
      ELSE
        HydrRadSys(RadSysNum)%NumCircuits(SurfNum) = 1.0d0
      END IF
    END DO

    CALL RegisterPlantCompDesignFlow(HydrRadSys(RadSysNum)%HotWaterInNode,HydrRadSys(RadSysNum)%WaterVolFlowMaxHeat)
    CALL RegisterPlantCompDesignFlow(HydrRadSys(RadSysNum)%ColdWaterInNode,HydrRadSys(RadSysNum)%WaterVolFlowMaxCool)

  END IF

  IF (SystemType==ConstantFlowSystem) THEN
    DO SurfNum = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
      IF (CFloRadSys(RadSysNum)%NumCircCalcMethod == CalculateFromLength) THEN
        CFloRadSys(RadSysNum)%NumCircuits(SurfNum) = (CFloRadSys(RadSysNum)%SurfaceFlowFrac(SurfNum) *   &
             CFloRadSys(RadSysNum)%TubeLength) / CFloRadSys(RadSysNum)%CircLength
        CFloRadSys(RadSysNum)%NumCircuits(SurfNum) = MAX(CFloRadSys(RadSysNum)%NumCircuits(SurfNum),1.0d0)
      ELSE
        CFloRadSys(RadSysNum)%NumCircuits(SurfNum) = 1.0d0
      END IF
    END DO
    IF (CFloRadSys(RadSysNum)%HotWaterInNode > 0) THEN
      CALL RegisterPlantCompDesignFlow(CFloRadSys(RadSysNum)%HotWaterInNode,CFloRadSys(RadSysNum)%WaterVolFlowMax)
    END IF
    IF (CFloRadSys(RadSysNum)%ColdWaterInNode > 0) THEN
      CALL RegisterPlantCompDesignFlow(CFloRadSys(RadSysNum)%ColdWaterInNode,CFloRadSys(RadSysNum)%WaterVolFlowMax)
    END IF
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeLowTempRadiantSystem


SUBROUTINE CalcLowTempHydrRadiantSystem(RadSysNum,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does all of the stuff that is necessary to simulate
          ! a low temperature hydronic radiant heating/cooling system.  Calls are
          ! made to appropriate subroutines either in this module or outside of it.

          ! METHODOLOGY EMPLOYED:
          ! Follows the methods used by many other pieces of zone equipment.
          ! Much like a water coil, a hydronic system will use the ControlCompOutput
          ! routine to determine what fraction of capacity the unit should be
          ! functioning at by controlling the flow rate of water to the element.

          ! REFERENCES:
          ! Other EnergyPlus modules
          ! IBLAST-QTF research program, completed in January 1995 (unreleased)
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.
          ! Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
          !   of Wisconsin-Madison.

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
!  USE DataEnvironment,   ONLY : OutDryBulbTemp, OutWetBulbTemp
  USE DataHeatBalance,   ONLY : MRT,Zone,ZoneData
  USE DataHeatBalFanSys, ONLY : MAT
  USE DataHVACGlobals,   ONLY : SmallLoad
  USE DataLoopNode,      ONLY : Node
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue
  USE PlantUtilities,    ONLY : SetComponentFlowRate
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: RadSysNum           ! name of the low temperature radiant system
  REAL(r64),    INTENT(INOUT) :: LoadMet             ! load met by the radiant system, in Watts

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ActWaterFlow     ! actual water flow for heating or cooling [kg/sec]
  INTEGER   :: ControlNode      ! the hot water or cold water inlet node
  REAL(r64) :: ControlTemp      ! temperature of whatever is controlling the radiant system
  REAL(r64) :: MassFlowFrac     ! fraction of the maximum water flow rate as determined by the control algorithm
  REAL(r64) :: MaxWaterFlow     ! maximum water flow for heating or cooling [kg/sec]
  REAL(r64) :: OffTempCool      ! temperature at which the flow rate throttles back to zero for cooling
  REAL(r64) :: OffTempHeat      ! temperature at which the flow rate throttles back to zero for heating
  REAL(r64) :: OpTemp           ! operative temperature (approximately the average of MAT and MRT) [Celsius]
  REAL(r64) :: QZnReq           ! heating or cooling needed by zone [Watts]
  REAL(r64) :: SetpointTemp     ! temperature "goal" for the radiant system [Celsius]
  INTEGER   :: SurfNum          ! Surface number in the Surface derived type for a radiant system surface
  INTEGER   :: SurfNum2         ! Surface number in the Surface derived type for a radiant system surface
  INTEGER   :: ZoneNum          ! number of zone being served
  REAL(r64) :: mdot             ! local temporary for fluid mass flow rate
  LOGICAL   :: SysRunning       ! True when system is running

          ! FLOW:
          ! initialize local variables
  ControlNode   = 0
  MaxWaterFlow  = 0.0d0
  ActWaterFlow  = 0.0d0
  ZoneNum       = HydrRadSys(RadSysNum)%ZonePtr
  OperatingMode = NotOperating
  SysRunning    = .true.

  IF (GetCurrentScheduleValue(HydrRadSys(RadSysNum)%SchedPtr) <= 0) THEN

          ! Unit is off or has no load upon it; set the flow rates to zero and then
          ! simulate the components with the no flow conditions
    DO SurfNum = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
      SurfNum2 = HydrRadSys(RadSysNum)%SurfacePtr(SurfNum)
      QRadSysSource(SurfNum2) = 0.0D0
      IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
        QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0    ! Also zero the other side of an interzone
    END DO
    IF (HydrRadSys(RadSysNum)%HeatingSystem) THEN !
      mdot = 0.d0
      CALL SetComponentFlowRate ( mdot , &
                          HydrRadSys(RadSysNum)%HotWaterInNode, &
                          HydrRadSys(RadSysNum)%HotWaterOutNode, &
                          HydrRadSys(RadSysNum)%HWLoopNum, &
                          HydrRadSys(RadSysNum)%HWLoopSide, &
                          HydrRadSys(RadSysNum)%HWBranchNum, &
                          HydrRadSys(RadSysNum)%HWCompNum  )
    ENDIF
    IF (HydrRadSys(RadSysNum)%CoolingSystem) THEN
      mdot = 0.d0
      CALL SetComponentFlowRate ( mdot , &
                          HydrRadSys(RadSysNum)%ColdWaterInNode, &
                          HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                          HydrRadSys(RadSysNum)%CWLoopNum, &
                          HydrRadSys(RadSysNum)%CWLoopSide, &
                          HydrRadSys(RadSysNum)%CWBranchNum, &
                          HydrRadSys(RadSysNum)%CWCompNum  )
    ENDIF
  ELSE    ! Unit might be on-->this section is intended to control the water mass flow rate being
          ! sent to the radiant system
    SELECT CASE (HydrRadSys(RadSysNum)%ControlType)
      CASE (MATControl)
        ControlTemp = MAT(ZoneNum)
      CASE (MRTControl)
        ControlTemp = MRT(ZoneNum)
      CASE (OperativeControl)
        ControlTemp = 0.5d0*(MAT(ZoneNum)+MRT(ZoneNum))
      CASE (ODBControl)
        ControlTemp = Zone(ZoneNum)%OutDryBulbTemp
      CASE (OWBControl)
        ControlTemp = Zone(ZoneNum)%OutWetBulbTemp
      CASE DEFAULT    ! Should never get here
        ControlTemp = MAT(ZoneNum)
        CALL ShowSevereError('Illegal control type in low temperature radiant system: '//TRIM(HydrRadSys(RadSysNum)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')
    END SELECT

    IF (HydrRadSys(RadSysNum)%HotSetptSchedPtr > 0) THEN
      SetpointTemp = GetCurrentScheduleValue(HydrRadSys(RadSysNum)%HotSetptSchedPtr)
      OffTempHeat  = SetpointTemp + 0.5d0*HydrRadSys(RadSysNum)%HotThrottlRange
    ELSE    ! This system is not capable of heating, set OffTempHeat to something really low
      OffTempHeat = LowTempHeating
    END IF
    IF (HydrRadSys(RadSysNum)%ColdSetptSchedPtr > 0) THEN
      SetpointTemp = GetCurrentScheduleValue(HydrRadSys(RadSysNum)%ColdSetptSchedPtr)
      OffTempCool  = SetpointTemp - 0.5d0*HydrRadSys(RadSysNum)%ColdThrottlRange
    ELSE    ! This system is not capable of cooling, set OffTempCool to something really high
      OffTempCool = HighTempCooling
    END IF

          ! Check for an illogical condition where a user enters controls that could
          ! potentially be heating or cooling at a particular control temperature
    IF (OffTempHeat > OffTempCool) THEN
      MassFlowFrac = 0.0d0
      CALL ShowSevereError('Overlapping heating and cooling control temps in radiant system: '//TRIM(HydrRadSys(RadSysNum)%Name))
      CALL ShowFatalError('Preceding condition causes termination.')

    ELSE  ! Temperatures for heating and cooling do not overlap--calculate the mass flow fraction

      IF (ControlTemp < OffTempHeat) THEN ! Heating mode
        OperatingMode = HeatingMode
        ControlNode   = HydrRadSys(RadSysNum)%HotWaterInNode
        MaxWaterFlow  = HydrRadSys(RadSysNum)%WaterFlowMaxHeat
        MassFlowFrac  = (OffTempHeat-ControlTemp)/HydrRadSys(RadSysNum)%HotThrottlRange
      ELSE IF (ControlTemp > OffTempCool) THEN ! Cooling mode
        OperatingMode = CoolingMode
        ControlNode   = HydrRadSys(RadSysNum)%ColdWaterInNode
        MaxWaterFlow  = HydrRadSys(RadSysNum)%WaterFlowMaxCool
        MassFlowFrac  = (ControlTemp-OffTempCool)/HydrRadSys(RadSysNum)%ColdThrottlRange
      ELSE ! ControlTemp is between OffTempHeat and OffTempCool--unit should not run
        MassFlowFrac = 0.0d0
      END IF

    END IF

          ! Calculate and limit the water flow rate
    ActWaterFlow = MassFlowFrac*MaxWaterFlow
    IF (ActWaterFlow < MassFlowTolerance) ActWaterFlow = 0.d0
    IF (HydrRadSys(RadSysNum)%EMSOverrideOnWaterMdot) ActWaterFlow = HydrRadSys(RadSysNum)%EMSWaterMdotOverrideValue

    IF (OperatingMode == HeatingMode) THEN
      IF (HydrRadSys(RadSysNum)%HeatingSystem) THEN
        CALL SetComponentFlowRate ( ActWaterFlow , &
                            HydrRadSys(RadSysNum)%HotWaterInNode, &
                            HydrRadSys(RadSysNum)%HotWaterOutNode, &
                            HydrRadSys(RadSysNum)%HWLoopNum, &
                            HydrRadSys(RadSysNum)%HWLoopSide, &
                            HydrRadSys(RadSysNum)%HWBranchNum, &
                            HydrRadSys(RadSysNum)%HWCompNum  )
      ELSE ! not heating system
        SysRunning=.false.
      ENDIF
    ELSEIF(OperatingMode == CoolingMode) THEN
      IF (HydrRadSys(RadSysNum)%CoolingSystem) THEN
        CALL SetComponentFlowRate ( ActWaterFlow , &
                        HydrRadSys(RadSysNum)%ColdWaterInNode, &
                        HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                        HydrRadSys(RadSysNum)%CWLoopNum, &
                        HydrRadSys(RadSysNum)%CWLoopSide, &
                        HydrRadSys(RadSysNum)%CWBranchNum, &
                        HydrRadSys(RadSysNum)%CWCompNum  )
      ELSE ! not cooling system
        SysRunning=.false.
      ENDIF
    ENDIF

          ! Now simulate the system...
    IF ( (OperatingMode == HeatingMode) .OR. (OperatingMode == CoolingMode) .and. SysRunning) &
      CALL CalcLowTempHydrRadSysComps(RadSysNum,LoadMet)

  END IF

  RETURN

END SUBROUTINE CalcLowTempHydrRadiantSystem


SUBROUTINE CalcLowTempHydrRadSysComps(RadSysNum,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves the radiant system based on how much water is (and
          ! the conditions of the water) supplied to the radiant system.

          ! METHODOLOGY EMPLOYED:
          ! Use heat exchanger formulas to obtain the heat source/sink for the radiant
          ! system based on the inlet conditions and flow rate of water.  Once that is
          ! determined, recalculate the surface heat balances to reflect this heat
          ! addition/subtraction.  The load met by the system is determined by the
          ! difference between the convection from all surfaces in the zone when
          ! there was no radiant system output and with a source/sink added.

          ! REFERENCES:
          ! IBLAST-QTF research program, completed in January 1995 (unreleased)
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.

          ! USE STATEMENTS:
  USE DataEnvironment,    ONLY : OutBaroPress
  USE General,            ONLY : RoundSigDigits
  USE DataHeatBalance,    ONLY : Construct, Zone
  USE DataHeatBalFanSys,  ONLY : RadSysTiHBConstCoef,            &
                                 RadSysTiHBToutCoef,RadSysTiHBQsrcCoef, &
                                 RadSysToHBConstCoef,RadSysToHBTinCoef, &
                                 RadSysToHBQsrcCoef,CTFTsrcConstPart,   &
                                 ZoneAirHumRat
  USE DataHeatBalSurface, ONLY : TH
  USE DataLoopNode,       ONLY : Node
  USE DataSurfaces,       ONLY : Surface, HeatTransferModel_CondFD, HeatTransferModel_CTF
  USE PlantUtilities,     ONLY : SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: RadSysNum          ! Index for the low temperature radiant system under consideration
  REAL(r64),    INTENT(INOUT) :: LoadMet            ! Load met by the low temperature radiant system, in Watts

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: CondDeltaTemp  = 1.0d0   ! How close the surface temperatures can get to the dewpoint temperature of a space
                                            ! before the radiant cooling system shuts off the flow.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CondSurfNum    ! Surface number (in radiant array) of
  INTEGER :: ConstrNum      ! Index for construction number in Construct derived type
  REAL(r64) :: DewPointTemp   ! Dew-point temperature based on the zone air conditions
  REAL(r64) :: EpsMdotCp      ! Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat
  REAL(r64) :: FullWaterMassFlow ! Original water mass flow rate before reducing the flow for condensation concerns
  REAL(r64) :: LowestRadSurfTemp ! Lowest surface temperature of a radiant system (when condensation is a concern)
  REAL(r64) :: PredictedCondTemp ! Temperature at which condensation is predicted (includes user parameter)
  INTEGER :: RadSurfNum     ! DO loop counter for the surfaces that comprise a particular radiant system
  INTEGER :: RadSurfNum2    ! DO loop counter for the surfaces that comprise a particular radiant system
  INTEGER :: RadSurfNum3    ! DO loop counter for the surfaces that comprise a particular radiant system
  REAL(r64) :: ReductionFrac ! Fraction that the flow should be reduced to avoid condensation
  INTEGER :: SurfNum        ! Index for radiant surface in Surface derived type
  INTEGER :: SurfNum2       ! Index for radiant surface in Surface derived type
  REAL(r64) :: SysWaterMassFlow ! System level water mass flow rate (includes effect of zone multiplier)
  REAL(r64) :: WaterMassFlow  ! Water mass flow rate in the radiant system, kg/s
  INTEGER :: WaterNodeIn    ! Node number of the water entering the radiant system
  REAL(r64) :: WaterTempIn    ! Temperature of the water entering the radiant system, in C
  REAL(r64) :: ZeroFlowSurfTemp ! Temperature of radiant surface when flow is zero
  INTEGER :: ZoneNum        ! Zone pointer for this radiant system

  REAL(r64) :: Ca,Cb,Cc,Cd,Ce,Cf,Cg,Ch,Ci,Cj,Ck,Cl ! Coefficients to relate the inlet water temperature to the heat source
                                                 ! For more info on Ca through Cl, see comments below

          ! FLOW:
          ! First, apply heat exchanger logic to find the heat source/sink to the system.
          ! This involves finding out the heat transfer characteristics of the hydronic
          ! loop and then applying the equations derived on pp. 113-118 of the dissertation.

          ! Set the conditions on the water side inlet
  SELECT CASE(OperatingMode)
    CASE (HeatingMode)
      WaterNodeIn = HydrRadSys(RadSysNum)%HotWaterInNode
    CASE (CoolingMode)
      WaterNodeIn = HydrRadSys(RadSysNum)%ColdWaterInNode
    CASE DEFAULT
      CALL ShowSevereError('Illegal low temperature radiant system operating mode')
      CALL ShowContinueError('Occurs in Radiant System='//TRIM(HydrRadSys(RadSysNum)%Name))
      CALL ShowFatalError('Preceding condition causes termination.')
  END SELECT
  ZoneNum          = HydrRadSys(RadSysNum)%ZonePtr
  SysWaterMassFlow = Node(WaterNodeIn)%MassFlowRate
  WaterMassFlow    = Node(WaterNodeIn)%MassFlowRate/REAL(Zone(ZoneNum)%Multiplier*Zone(ZoneNum)%ListMultiplier,r64)
  WaterTempIn      = Node(WaterNodeIn)%Temp

  IF (WaterMassFlow <= 0.0d0) THEN
          ! No flow or below minimum allowed so there is no heat source/sink
          ! This is possible with a mismatch between system and plant operation
          ! or a slight mismatch between zone and system controls.  This is not
          ! necessarily a "problem" so this exception is necessary in the code.
    DO RadSurfNum = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
      SurfNum                = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
      QRadSysSource(SurfNum) = 0.0D0
      IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
        QRadSysSource(Surface(SurfNum)%ExtBoundCond) = 0.0D0    ! Also zero the other side of an interzone
    END DO

  ELSE

    DO RadSurfNum = 1, HydrRadSys(RadSysNum)%NumOfSurfaces

      SurfNum = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
          ! Determine the heat exchanger "effectiveness" term
      EpsMdotCp = CalcRadSysHXEffectTerm(RadSysNum, HydronicSystem ,WaterTempIn,WaterMassFlow,               &
                                         HydrRadSys(RadSysNum)%SurfaceFlowFrac(RadSurfNum), &
                                         HydrRadSys(RadSysNum)%NumCircuits(RadSurfNum),     &
                                         HydrRadSys(RadSysNum)%TubeLength,                  &
                                         HydrRadSys(RadSysNum)%TubeDiameter,                &
                                         HydrRadSys(RadSysNum)%GlycolIndex)

          ! Obtain the heat balance coefficients and calculate the intermediate coefficients
          ! linking the inlet water temperature to the heat source/sink to the radiant system.
          ! The coefficients are based on the following development...
          ! The heat balance equations at the outside and inside surfaces are of the form:
          !   Tinside  = Ca + Cb*Toutside + Cc*q"
          !   Toutside = Cd + Ce*Tinside  + Cf*q"
          !   Tsource  = Cg + Ch*q"       + Ci*Tinside + Cj*Toutside
          ! where:
          !   Tinside is the temperature at the inside surface
          !   Toutside is the temperature at the outside surface
          !   Tsource is the temperature within the radiant system at the location of the source/sink
          !   Ca is all of the other terms in the inside heat balance (solar, LW exchange, conduction history terms, etc.)
          !   Cb is the current cross CTF term
          !   Cc is the QTF inside term for the current heat source/sink
          !   Cd is all of the other terms in the outside heat balance (solar, LW exchange, conduction history terms, etc.)
          !   Ce is the current cross CTF term (should be equal to Cb)
          !   Cf is the QTF outside term for the current heat source/sink
          !   Cg is the summation of all temperature and source history terms at the source/sink location
          !   Ch is the QTF term at the source/sink location for the current heat source/sink
          !   Ci is the CTF inside term for the current inside surface temperature
          !   Cj is the CTF outside term for the current outside surface temperature
          ! Note that it is necessary to not use "slow conduction" assumptions because the
          ! source/sink has an impact on BOTH the inside and outside surface heat balances.
          ! Hence the more general formulation.
          ! The first two T equations above can be solved to remove the other surface temperature.
          ! This results in the following equations:
          !   Tinside  = Ca + Cb*(Cd + Ce*Tinside + Cf*q") + Cc*q"   or...
          !   Tinside  = (Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)
          !   Toutside = Cd + Ce*(Ca + Cb*Toutside + Cc*q") + Cf*q"  or...
          !   Toutside = (Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb)
          ! Substituting the new equations for Tinside and Toutside as a function of C and q"
          ! into the equation for Tsource...
          !   Tsource  = Cg + Ch*q" + Ci*((Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)) &
          !                         + Cj*((Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb))
          ! Or rearranging this to get Tsource as a function of q", we get...
          !   Tsource  =  Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb)) &
          !             +(Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb)))*q"
          ! Or in a slightly simpler form...
          !   Tsource  = Ck + Cl*q"
          ! where:
          !   Ck = Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb))
          !   Cl = Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb))
          ! Note also that from heat exchanger "algebra", we have:
          !   q = epsilon*qmax    and    qmax = Mdot*Cp*(Twaterin-Tsource)
          ! So...
          !   q" = q/Area = (epsilon*Mdot*Cp/Area)*(Twaterin-Tsource)
          ! Or rearranging this equation:
          !   Tsource = -(q"*A/(epsilon*Mdot*Cp)) + Twaterin
          ! Setting this equation equal to the other equation for Tsource a couple lines up
          ! and rearranging to solve for q"...
          !   q" = (Twaterin - Ck) / (Cl + (A/(epsilon*Mdot*Cp))
          ! or
          !   q  = (Twaterin - Ck) / ((Cl/A) + (1/epsilon*Mdot*Cp))
          ! or
          !   q  = epsilon*Mdot*Cp*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
          ! which is the desired result, that is the heat source or sink to the radiant
          ! system as a function of the water inlet temperature (flow rate is also in there
          ! as well as all of the heat balance terms "hidden" in Ck and Cl).
      ConstrNum = Surface(SurfNum)%Construction

      IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CTF) THEN


        Ca = RadSysTiHBConstCoef(SurfNum)
        Cb = RadSysTiHBToutCoef(SurfNum)
        Cc = RadSysTiHBQsrcCoef(SurfNum)

        Cd = RadSysToHBConstCoef(SurfNum)
        Ce = RadSysToHBTinCoef(SurfNum)
        Cf = RadSysToHBQsrcCoef(SurfNum)

        Cg = CTFTsrcConstPart(SurfNum)
        Ch = Construct(ConstrNum)%CTFTSourceQ(0)
        Ci = Construct(ConstrNum)%CTFTSourceIn(0)
        Cj = Construct(ConstrNum)%CTFTSourceOut(0)

        Ck = Cg + ( ( Ci*(Ca+Cb*Cd) + Cj*(Cd+Ce*Ca) ) / ( 1.0d0 - Ce*Cb ) )
        Cl = Ch + ( ( Ci*(Cc+Cb*Cf) + Cj*(Cf+Ce*Cc) ) / ( 1.0d0 - Ce*Cb ) )

        QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - Ck) &
                              /(1.d0 + (EpsMdotCp*Cl/Surface(SurfNum)%Area) )

      ELSE IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD) THEN

        QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - TCondFDSourceNode(SurfNum))

      ENDIF

      IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
          QRadSysSource(Surface(SurfNum)%ExtBoundCond) = QRadSysSource(SurfNum)   ! Also set the other side of an interzone

    END DO

          ! "Temperature Comparison" Cut-off:
    DO RadSurfNum = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
          ! Check to see whether or not the system should really be running.  If
          ! QRadSysSource is negative when we are in heating mode or QRadSysSource
          ! is positive when we are in cooling mode, then the radiant system will
          ! be doing the opposite of its intention.  In this case, the flow rate
          ! is set to zero to avoid heating in cooling mode or cooling in heating
          ! mode.
      SurfNum = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum)

      IF ( ((OperatingMode==HeatingMode).AND.(QRadSysSource(SurfNum)<=0.0d0)) .OR. &
           ((OperatingMode==CoolingMode).AND.(QRadSysSource(SurfNum)>=0.0d0)) ) THEN
        WaterMassFlow         = 0.d0
        IF (OperatingMode==HeatingMode) THEN
          CALL SetComponentFlowRate ( WaterMassFlow , &
                          HydrRadSys(RadSysNum)%HotWaterInNode, &
                          HydrRadSys(RadSysNum)%HotWaterOutNode, &
                          HydrRadSys(RadSysNum)%HWLoopNum, &
                          HydrRadSys(RadSysNum)%HWLoopSide, &
                          HydrRadSys(RadSysNum)%HWBranchNum, &
                          HydrRadSys(RadSysNum)%HWCompNum  )

        ELSEIF (OperatingMode==CoolingMode) THEN
          CALL SetComponentFlowRate ( WaterMassFlow , &
                          HydrRadSys(RadSysNum)%ColdWaterInNode, &
                          HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                          HydrRadSys(RadSysNum)%CWLoopNum, &
                          HydrRadSys(RadSysNum)%CWLoopSide, &
                          HydrRadSys(RadSysNum)%CWBranchNum, &
                          HydrRadSys(RadSysNum)%CWCompNum  )
        ENDIF
        HydrRadSys(RadSysNum)%WaterMassFlowRate = WaterMassFlow

        DO RadSurfNum2 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
          SurfNum2 = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2)
          QRadSysSource(SurfNum2) = 0.0D0
          IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
            QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
        END DO
        EXIT ! outer do loop
      END IF
    END DO

          ! Condensation Cut-off:
          ! Check to see whether there are any surface temperatures within the radiant system that have
          ! dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
          ! A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
          ! conditions.
    HydrRadSys(RadSysNum)%CondCausedShutDown = .FALSE.
    DewPointTemp = PsyTdpFnWPb(ZoneAirHumRat(ZoneNum),OutBaroPress)

    IF ( (OperatingMode == CoolingMode) .AND. (HydrRadSys(RadSysNum)%CondCtrlType == CondCtrlSimpleOff) ) THEN

      DO RadSurfNum2 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
        IF (TH(HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2) < (DewPointTemp+HydrRadSys(RadSysNum)%CondDewPtDeltaT)) THEN
          ! Condensation warning--must shut off radiant system
          HydrRadSys(RadSysNum)%CondCausedShutDown = .TRUE.
          WaterMassFlow                           = 0.0d0
          CALL SetComponentFlowRate ( WaterMassFlow , &
                        HydrRadSys(RadSysNum)%ColdWaterInNode, &
                        HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                        HydrRadSys(RadSysNum)%CWLoopNum, &
                        HydrRadSys(RadSysNum)%CWLoopSide, &
                        HydrRadSys(RadSysNum)%CWBranchNum, &
                        HydrRadSys(RadSysNum)%CWCompNum  )
          HydrRadSys(RadSysNum)%WaterMassFlowRate = WaterMassFlow
          DO RadSurfNum3 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
            SurfNum2 = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum3)
            QRadSysSource(SurfNum2) = 0.0D0
            IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
              QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
          END DO
          ! Produce a warning message so that user knows the system was shut-off due to potential for condensation
          IF (.not. WarmUpFlag) THEN
            IF (HydrRadSys(RadSysNum)%CondErrIndex == 0) THEN  ! allow errors up to number of radiant systems
              CALL ShowWarningMessage(cHydronicSystem//' ['//TRIM(HydrRadSys(RadSysNum)%Name)//']')
              CALL ShowContinueError('Surface ['//trim(Surface(HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2))%Name)//  &
                 '] temperature below dew-point temperature--potential for condensation exists')
              CALL ShowContinueError('Flow to the radiant system will be shut-off to avoid condensation')
              CALL ShowContinueError('Predicted radiant system surface temperature = '// &
                                     trim(RoundSigDigits(TH(HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2),2)))
              CALL ShowContinueError('Zone dew-point temperature + safety delta T= '//  &
                 trim(RoundSigDigits(DewPointTemp+HydrRadSys(RadSysNum)%CondDewPtDeltaT,2)))
              CALL ShowContinueErrorTimeStamp(' ')
              CALL ShowContinueError('Note that a '//TRIM(RoundSigDigits(HydrRadSys(RadSysNum)%CondDewPtDeltaT,4))// &
                                     ' C safety was chosen in the input for the shut-off criteria')
              CALL ShowContinueError('Note also that this affects all surfaces that are part of this radiant system')
            END IF
            CALL ShowRecurringWarningErrorAtEnd(cHydronicSystem//' ['//TRIM(HydrRadSys(RadSysNum)%Name)//  &
                           '] condensation shut-off occurrence continues.',  &
                           HydrRadSys(RadSysNum)%CondErrIndex,ReportMinOf=DewPointTemp,ReportMaxOf=DewPointTemp,  &
                           ReportMaxUnits='C',ReportMinUnits='C')
          END IF
          EXIT ! outer do loop
        END IF
      END DO

    ELSE IF ( (OperatingMode == CoolingMode) .AND. (HydrRadSys(RadSysNum)%CondCtrlType == CondCtrlNone) ) THEN

      DO RadSurfNum2 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
        IF (TH(HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2) < DewPointTemp) THEN
          ! Condensation occurring but user does not want to shut radiant system off ever
          HydrRadSys(RadSysNum)%CondCausedShutDown = .TRUE.
        END IF
      END DO

    ELSE IF ( (OperatingMode == CoolingMode) .AND. (HydrRadSys(RadSysNum)%CondCtrlType == CondCtrlVariedOff) ) THEN

      LowestRadSurfTemp = 999.9d0
      CondSurfNum       = 0
      DO RadSurfNum2 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
        IF (TH(HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2) < (DewPointTemp+HydrRadSys(RadSysNum)%CondDewPtDeltaT)) THEN
          IF (TH(HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2) < LowestRadSurfTemp) THEN
            LowestRadSurfTemp = TH(HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2)
            CondSurfNum    = RadSurfNum2
          END IF
        END IF
      END DO

      IF (CondSurfNum > 0) THEN    ! Condensation predicted so let's deal with it
          ! Process here is: turn everything off and see what the resulting surface temperature is for
          ! the surface that was causing the lowest temperature.  Then, interpolate to find the flow that
          ! would still allow the system to operate without producing condensation.  Rerun the heat balance
          ! and recheck for condensation.  If condensation still exists, shut everything down.  This avoids
          ! excessive iteration and still makes an attempt to vary the flow rate.
          ! First, shut everything off...
        FullWaterMassFlow                       = WaterMassFlow
        WaterMassFlow                           = 0.0d0
        CALL SetComponentFlowRate (WaterMassFlow ,                         &
                                   HydrRadSys(RadSysNum)%ColdWaterInNode,  &
                                   HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                                   HydrRadSys(RadSysNum)%CWLoopNum,        &
                                   HydrRadSys(RadSysNum)%CWLoopSide,       &
                                   HydrRadSys(RadSysNum)%CWBranchNum,      &
                                   HydrRadSys(RadSysNum)%CWCompNum)
        HydrRadSys(RadSysNum)%WaterMassFlowRate = WaterMassFlow
        DO RadSurfNum3 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
          SurfNum2 = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum3)
          QRadSysSource(SurfNum2) = 0.0D0
          IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
            QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
        END DO
          ! Redo the heat balances since we have changed the heat source (set it to zero)
        CALL CalcHeatBalanceOutsideSurf(ZoneNum)
        CALL CalcHeatBalanceInsideSurf(ZoneNum)
          ! Now check all of the surface temperatures.  If any potentially have condensation, leave the system off.
        DO RadSurfNum2 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
          IF (TH(HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2) < (DewPointTemp+HydrRadSys(RadSysNum)%CondDewPtDeltaT)) THEN
            HydrRadSys(RadSysNum)%CondCausedShutDown = .TRUE.
          END IF
        END DO
          ! If the system does not need to be shut down, then let's see if we can vary the flow based
          ! on the lowest temperature surface from before.  This will use interpolation to try a new
          ! flow rate.
        IF (.NOT. HydrRadSys(RadSysNum)%CondCausedShutDown) THEN
          PredictedCondTemp = DewPointTemp+HydrRadSys(RadSysNum)%CondDewPtDeltaT
          ZeroFlowSurfTemp  = TH(HydrRadSys(RadSysNum)%SurfacePtr(CondSurfNum),1,2)
          ReductionFrac = (ZeroFlowSurfTemp-PredictedCondTemp)/ABS(ZeroFlowSurfTemp-LowestRadSurfTemp)
          IF (ReductionFrac < 0.0d0) ReductionFrac = 0.0d0    ! Shouldn't happen as the above check should have screened this out
          IF (ReductionFrac > 1.0d0) ReductionFrac = 1.0d0    ! Shouldn't happen either because condensation doesn't exist then
          WaterMassFlow    = ReductionFrac*FullWaterMassFlow
          SysWaterMassFlow = REAL(Zone(ZoneNum)%Multiplier*Zone(ZoneNum)%ListMultiplier,r64)*WaterMassFlow
          ! Got a new reduced flow rate that should work...reset loop variable and resimulate the system
          CALL SetComponentFlowRate (SysWaterMassFlow,                       &
                                     HydrRadSys(RadSysNum)%ColdWaterInNode,  &
                                     HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                                     HydrRadSys(RadSysNum)%CWLoopNum,        &
                                     HydrRadSys(RadSysNum)%CWLoopSide,       &
                                     HydrRadSys(RadSysNum)%CWBranchNum,      &
                                     HydrRadSys(RadSysNum)%CWCompNum)
          HydrRadSys(RadSysNum)%WaterMassFlowRate = SysWaterMassFlow

          ! Go through all of the surfaces again with the new flow rate...
          DO RadSurfNum3 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
            SurfNum = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum3)
          ! Determine the heat exchanger "effectiveness" term
            EpsMdotCp = CalcRadSysHXEffectTerm(RadSysNum,HydronicSystem,WaterTempIn,WaterMassFlow,  &
                                               HydrRadSys(RadSysNum)%SurfaceFlowFrac(RadSurfNum3),  &
                                               HydrRadSys(RadSysNum)%NumCircuits(RadSurfNum3),  &
                                               HydrRadSys(RadSysNum)%TubeLength,                    &
                                               HydrRadSys(RadSysNum)%TubeDiameter,                  &
                                               HydrRadSys(RadSysNum)%GlycolIndex)
            IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CTF) THEN
              ! For documentation on coefficients, see code earlier in this subroutine
              Ca = RadSysTiHBConstCoef(SurfNum)
              Cb = RadSysTiHBToutCoef(SurfNum)
              Cc = RadSysTiHBQsrcCoef(SurfNum)
              Cd = RadSysToHBConstCoef(SurfNum)
              Ce = RadSysToHBTinCoef(SurfNum)
              Cf = RadSysToHBQsrcCoef(SurfNum)
              Cg = CTFTsrcConstPart(SurfNum)
              Ch = Construct(ConstrNum)%CTFTSourceQ(0)
              Ci = Construct(ConstrNum)%CTFTSourceIn(0)
              Cj = Construct(ConstrNum)%CTFTSourceOut(0)
              Ck = Cg + ( ( Ci*(Ca+Cb*Cd) + Cj*(Cd+Ce*Ca) ) / ( 1.0d0 - Ce*Cb ) )
              Cl = Ch + ( ( Ci*(Cc+Cb*Cf) + Cj*(Cf+Ce*Cc) ) / ( 1.0d0 - Ce*Cb ) )
              QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - Ck) &
                                      /(1.0d0 + (EpsMdotCp*Cl/Surface(SurfNum)%Area) )
            ELSE IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD) THEN
              QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - TCondFDSourceNode(SurfNum))
            END IF
            IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
              QRadSysSource(Surface(SurfNum)%ExtBoundCond) = QRadSysSource(SurfNum)   ! Also set the other side of an interzone
          END DO

          ! Redo the heat balances since we have changed the heat source
          CALL CalcHeatBalanceOutsideSurf(ZoneNum)
          CALL CalcHeatBalanceInsideSurf(ZoneNum)

          ! Check for condensation one more time.  If no condensation, we are done.  If there is
          ! condensation, shut things down and be done.
          DO RadSurfNum2 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
            IF (HydrRadSys(RadSysNum)%CondCausedShutDown) EXIT
            IF (TH(HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2) < (PredictedCondTemp)) THEN
          ! Condensation still present--must shut off radiant system
              HydrRadSys(RadSysNum)%CondCausedShutDown = .TRUE.
              WaterMassFlow                            = 0.0d0
              RadSurfNum                               = RadSurfNum2
              CALL SetComponentFlowRate (WaterMassFlow ,                          &
                                         HydrRadSys(RadSysNum)%ColdWaterInNode,  &
                                         HydrRadSys(RadSysNum)%ColdWaterOutNode, &
                                         HydrRadSys(RadSysNum)%CWLoopNum,        &
                                         HydrRadSys(RadSysNum)%CWLoopSide,       &
                                         HydrRadSys(RadSysNum)%CWBranchNum,      &
                                         HydrRadSys(RadSysNum)%CWCompNum)
              HydrRadSys(RadSysNum)%WaterMassFlowRate = WaterMassFlow
              DO RadSurfNum3 = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
                SurfNum2 = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum3)
                QRadSysSource(SurfNum2) = 0.0D0
                IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
                  QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
              END DO
            END IF
          END DO

        END IF

        IF (HydrRadSys(RadSysNum)%CondCausedShutDown) THEN
          ! Produce a warning message so that user knows the system was shut-off due to potential for condensation
          IF (.not. WarmUpFlag) THEN
            IF (HydrRadSys(RadSysNum)%CondErrIndex == 0) THEN  ! allow errors up to number of radiant systems
              CALL ShowWarningMessage(cHydronicSystem//' ['//TRIM(HydrRadSys(RadSysNum)%Name)//']')
              CALL ShowContinueError('Surface ['//trim(Surface(HydrRadSys(RadSysNum)%SurfacePtr(CondSurfNum))%Name)//  &
                 '] temperature below dew-point temperature--potential for condensation exists')
              CALL ShowContinueError('Flow to the radiant system will be shut-off to avoid condensation')
              CALL ShowContinueError('Predicted radiant system surface temperature = '// &
                                     trim(RoundSigDigits(TH(HydrRadSys(RadSysNum)%SurfacePtr(CondSurfNum),1,2),2)))
              CALL ShowContinueError('Zone dew-point temperature + safety delta T= '//  &
                 trim(RoundSigDigits(DewPointTemp+HydrRadSys(RadSysNum)%CondDewPtDeltaT,2)))
              CALL ShowContinueErrorTimeStamp(' ')
              CALL ShowContinueError('Note that a '//TRIM(RoundSigDigits(HydrRadSys(RadSysNum)%CondDewPtDeltaT,4))// &
                                     ' C safety was chosen in the input for the shut-off criteria')
              CALL ShowContinueError('Note also that this affects all surfaces that are part of this radiant system')
            END IF
            CALL ShowRecurringWarningErrorAtEnd(cHydronicSystem//' ['//TRIM(HydrRadSys(RadSysNum)%Name)//  &
                           '] condensation shut-off occurrence continues.',  &
                           HydrRadSys(RadSysNum)%CondErrIndex,ReportMinOf=DewPointTemp,ReportMaxOf=DewPointTemp,  &
                           ReportMaxUnits='C',ReportMinUnits='C')
          END IF
        END IF
      END IF  ! Condensation Predicted in Variable Shut-Off Control Type
    END IF  ! In cooling mode and one of the condensation control types
  END IF  ! There was a non-zero flow

          ! Now that we have the source/sink term, we must redo the heat balances to obtain
          ! the new SumHATsurf value for the zone.  Note that the difference between the new
          ! SumHATsurf and the value originally calculated by the heat balance with a zero
          ! source for all radiant systems in the zone is the load met by the system (approximately).
  CALL CalcHeatBalanceOutsideSurf(ZoneNum)
  CALL CalcHeatBalanceInsideSurf(ZoneNum)

  LoadMet = SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum)

  RETURN

END SUBROUTINE CalcLowTempHydrRadSysComps


SUBROUTINE CalcLowTempCFloRadiantSystem(RadSysNum,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does all of the stuff that is necessary to simulate
          ! a constant flow low temperature hydronic radiant heating/cooling system.
          ! Calls are made to appropriate subroutines either in this module or
          ! outside of it.

          ! METHODOLOGY EMPLOYED:
          ! Similar in many aspects to the hydronic (variable flow) radiant system
          ! except that flow rate through the radiant system is constant (based on
          ! the user schedule) and the inlet temperature is varied by injecting
          ! more or less fluid from the main loop to achieve the desired inlet
          ! temperature.

          ! REFERENCES:
          ! Other EnergyPlus modules
          ! IBLAST-QTF research program, completed in January 1995 (unreleased)
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.
          ! Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
          !   of Wisconsin-Madison.

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE DataEnvironment,   ONLY : EnvironmentName, CurMnDy
  USE DataHeatBalance,   ONLY : MRT, Zone, ZoneData
  USE DataHeatBalFanSys, ONLY : MAT
  USE DataHVACGlobals,   ONLY : SmallLoad
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance
  USE DataLoopNode,      ONLY : Node
  USE FluidProperties,   ONLY : GetSpecificHeatGlycol
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue
  USE General,           ONLY : TrimSigDigits
  USE PlantUtilities,    ONLY : SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: RadSysNum ! name of the low temperature radiant system
  REAL(r64), INTENT(INOUT) :: LoadMet   ! load met by the radiant system, in Watts

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: LowCpFluidValue = 100.d0 ! lowest allowed Cp fluid value (to avoid dividing by zero) [J/kg-K]

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CpFluid          ! Specific heat of the fluid in the radiant system
  REAL(r64) :: InjectFlowRate   ! Calculated injection flow rate that will meet the inlet temperature requirement
  LOGICAL   :: Iteration        ! FALSE when a normal solution, TRUE when it is a solution where we must also find the inlet temp
  INTEGER   :: LoopInNode       ! Node on the loop that is the inlet to the constant flow radiant system
  REAL(r64) :: OffTempCool      ! temperature at which the cooling shuts down
  REAL(r64) :: OffTempHeat      ! temperature at which the heating shuts down
  REAL(r64) :: PumpPartLoadRat  ! Pump part load ratio (based on user schedule, or 1.0 for no schedule)
  REAL(r64) :: PumpTempRise     ! Temperature rise of the fluid as it passes through the pump
  REAL(r64) :: QZnReq           ! heating or cooling needed by zone [Watts]
  REAL(r64) :: RadInTemp        ! "Desired" radiant system water inlet temperature [Celsius]
  REAL(r64) :: SetpointTemp     ! temperature that will be used to control the radiant system [Celsius]
  REAL(r64) :: SetpointTempHi   ! Current high point in setpoint temperature range
  REAL(r64) :: SetpointTempLo   ! Current low point in setpoint temperature range
  REAL(r64) :: ShaftPower       ! Amount of power expended at the pump shaft
  INTEGER   :: SurfNum          ! Surface number in the Surface derived type for a radiant system surface
  INTEGER   :: SurfNum2         ! Surface number in the Surface derived type for a radiant system surface
  LOGICAL   :: SysRunning       ! TRUE when the system is running
  REAL(r64) :: SysWaterInTemp   ! Fluid temperature supplied from the loop
  REAL(r64) :: WaterTempHi      ! Current high point in water temperature range
  REAL(r64) :: WaterTempLo      ! Current low point in water temperature range
  INTEGER   :: ZoneNum          ! number of zone being served
  REAL(r64) :: mdot             ! local temporary for water mass flow rate kg/s

          ! FLOW:
          ! initialize local variables
  ZoneNum     = CFloRadSys(RadSysNum)%ZonePtr
  SysRunning  = .TRUE.  ! default to running and turn off only if not running
  VarOffCond  = .FALSE.

  IF (GetCurrentScheduleValue(CFloRadSys(RadSysNum)%SchedPtr) <= 0) SysRunning = .FALSE.

  IF (SysRunning) THEN  ! Unit is probably on-->this section is intended to control the water
                        ! mass flow rate being sent to the radiant system

          ! Set the current setpoint temperature (same procedure for either heating or cooling)
    SELECT CASE (CFloRadSys(RadSysNum)%ControlType)
      CASE (MATControl)
        SetpointTemp = MAT(ZoneNum)
      CASE (MRTControl)
        SetpointTemp = MRT(ZoneNum)
      CASE (OperativeControl)
        SetpointTemp = 0.5d0*(MAT(ZoneNum)+MRT(ZoneNum))
      CASE (ODBControl)
        SetpointTemp = Zone(ZoneNum)%OutDryBulbTemp
      CASE (OWBControl)
        SetpointTemp = Zone(ZoneNum)%OutWetBulbTemp
      CASE DEFAULT    ! Should never get here
        CALL ShowSevereError('Illegal control type in low temperature radiant system: '//TRIM(CFloRadSys(RadSysNum)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')
    END SELECT

          ! Avoid problems when there is no heating or cooling control because the system only cools or heats
    IF (CFloRadSys(RadSysNum)%HotCtrlHiTempSchedPtr > 0) THEN
      OffTempHeat = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%HotCtrlHiTempSchedPtr)
    ELSE
      OffTempHeat = LowTempHeating
    END IF
    IF (CFloRadSys(RadSysNum)%ColdCtrlLoTempSchedPtr > 0) THEN
      OffTempCool = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%ColdCtrlLoTempSchedPtr)
    ELSE
      OffTempCool = HighTempCooling
    END IF

          ! Now actually decide what to do based on the setpoint temperature in relation to the control temperatures
    IF (SetpointTemp < OffTempHeat) THEN  ! HEATING MODE

      OperatingMode = HeatingMode

      IF (.NOT.CFloRadSys(RadSysNum)%HeatingSystem) THEN

        SysRunning = .FALSE.    ! Can't heat unless it's a heating system

      ELSE  ! It is a heating system so set all of the values for controls

        SetpointTempHi = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%HotCtrlHiTempSchedPtr)
        SetpointTempLo = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%HotCtrlLoTempSchedPtr)
        IF (SetpointTempHi < SetpointTempLo) THEN
          CALL ShowSevereError('Heating setpoint temperature mismatch in'//TRIM(CFloRadSys(RadSysNum)%Name))
          CALL ShowContinueError('High setpoint temperature is less than low setpoint temperature--check your schedule input')
          CALL ShowFatalError('Preceding condition causes termination.')
        END IF

        WaterTempHi    = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%HotWaterHiTempSchedPtr)
        WaterTempLo    = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%HotWaterLoTempSchedPtr)
        IF (WaterTempHi < WaterTempLo) THEN
          CALL ShowSevereError('Heating water temperature mismatch in'//TRIM(CFloRadSys(RadSysNum)%Name))
          CALL ShowContinueError('High water temperature is less than low water temperature--check your schedule input')
          CALL ShowFatalError('Preceding condition causes termination.')
        END IF

        IF (SetpointTemp >= SetpointTempHi) THEN
          ! System is above high heating setpoint so we should be able to turn the system off
          RadInTemp = WaterTempLo
          SysRunning = .FALSE.
        ELSE IF (SetpointTemp <= SetpointTempLo) THEN
          ! System is running with its highest inlet temperature
          RadInTemp = WaterTempHi
        ELSE
          ! Interpolate to obtain the current radiant system inlet temperature
          RadInTemp = WaterTempHi - (WaterTempHi - WaterTempLo)*(SetpointTemp - SetpointTempLo)/(SetpointTempHi - SetpointTempLo)
        END IF

      END IF

    ELSE IF (SetpointTemp > OffTempCool) THEN ! COOLING MODE

      OperatingMode = CoolingMode

      IF (.NOT.CFloRadSys(RadSysNum)%CoolingSystem) THEN

        SysRunning = .FALSE.    ! Can't cool unless it's a cooling system

      ELSE  ! It is a cooling system so set all of the values for controls

        SetpointTempHi = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%ColdCtrlHiTempSchedPtr)
        SetpointTempLo = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%ColdCtrlLoTempSchedPtr)
        IF (SetpointTempHi < SetpointTempLo) THEN
          CALL ShowSevereError('Cooling setpoint temperature mismatch in'//TRIM(CFloRadSys(RadSysNum)%Name))
          CALL ShowContinueError('High setpoint temperature is less than low setpoint temperature--check your schedule input')
          CALL ShowFatalError('Preceding condition causes termination.')
        END IF

        WaterTempHi    = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%ColdWaterHiTempSchedPtr)
        WaterTempLo    = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%ColdWaterLoTempSchedPtr)
        IF (WaterTempHi < WaterTempLo) THEN
          CALL ShowSevereError('Cooling water temperature mismatch in'//TRIM(CFloRadSys(RadSysNum)%Name))
          CALL ShowContinueError('High water temperature is less than low water temperature--check your schedule input')
          CALL ShowFatalError('Preceding condition causes termination.')
        END IF

        IF (SetpointTemp <= SetpointTempLo) THEN
          ! System is below low cooling setpoint so we should be able to turn the system off
          RadInTemp = WaterTempHi
          SysRunning = .FALSE.
        ELSE IF (SetpointTemp >= SetpointTempHi) THEN
          ! System is running with its lowest inlet temperature
          RadInTemp = WaterTempLo
        ELSE
          ! Interpolate to obtain the current radiant system inlet temperature
          RadInTemp = WaterTempHi - (WaterTempHi - WaterTempLo)*(SetpointTemp - SetpointTempLo)/(SetpointTempHi - SetpointTempLo)
        END IF

      END IF

    ELSE    ! System is not running because the setpoint temperature is in the "deadband"

      RadInTemp  = SetpointTemp
      SysRunning = .FALSE.

    END IF

  END IF

  IF (SysRunning) THEN
    CpFluid = GetSpecificHeatGlycol('WATER',RadInTemp,CFloRadSys(RadSysNum)%GlycolIndex,'CalcLowTempCFloRadiantSystem')
  END IF

  IF ((.NOT. SysRunning).OR.(CpFluid < LowCpFluidValue)) THEN
          ! Unit is off or has no load upon it OR CpFluid value is "zero" so
          ! set the flow rates to zero and then simulate the components with
          ! the no flow conditions
    OperatingMode                            = NotOperating
    CFloRadSys(RadSysNum)%WaterMassFlowRate  = 0.0d0
    CFloRadSys(RadSysNum)%WaterInjectionRate = 0.0d0
    CFloRadSys(RadSysNum)%WaterRecircRate    = 0.0d0
    CFloRadSys(RadSysNum)%HeatPower          = 0.0d0
    CFloRadSys(RadSysNum)%CoolPower          = 0.0d0
    CFloRadSys(RadSysNum)%PumpPower          = 0.0d0
    CFloRadSys(RadSysNum)%PumpMassFlowRate   = 0.0d0
    CFloRadSys(RadSysNum)%PumpHeattoFluid    = 0.0d0

    DO SurfNum = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
      SurfNum2 = CFloRadSys(RadSysNum)%SurfacePtr(SurfNum)
      QRadSysSource(SurfNum2) = 0.0D0
      IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
        QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0    ! Also zero the other side of an interzone
    END DO

    ! turn off flow requests made during init because it is not actually running
    IF (CFloRadSys(RadSysNum)%CWLoopNum > 0 ) THEN
      mdot = 0.d0
      CALL SetComponentFlowRate(mdot, &
                          CFloRadSys(RadSysNum)%ColdWaterInNode, &
                          CFloRadSys(RadSysNum)%ColdWaterOutNode, &
                          CFloRadSys(RadSysNum)%CWLoopNum, &
                          CFloRadSys(RadSysNum)%CWLoopSide, &
                          CFloRadSys(RadSysNum)%CWBranchNum, &
                          CFloRadSys(RadSysNum)%CWCompNum)
    ENDIF
    IF (CFloRadSys(RadSysNum)%HWLoopNum > 0 ) THEN
      mdot = 0.d0
      CALL SetComponentFlowRate(mdot, &
                                CFloRadSys(RadSysNum)%HotWaterInNode, &
                                CFloRadSys(RadSysNum)%HotWaterOutNode, &
                                CFloRadSys(RadSysNum)%HWLoopNum, &
                                CFloRadSys(RadSysNum)%HWLoopSide, &
                                CFloRadSys(RadSysNum)%HWBranchNum, &
                                CFloRadSys(RadSysNum)%HWCompNum )
    ENDIF
  ELSE ! (SysRunning) so simulate the system...

          ! Determine pump flow rate and pump heat addition
    CFloRadSys(RadSysNum)%PumpMassFlowRate = CFloRadSys(RadSysNum)%WaterMassFlowRate   ! Set in InitLowTempRadiantSystem
    IF (CFloRadSys(RadSysNum)%VolFlowSchedPtr > 0) THEN
      PumpPartLoadRat = GetCurrentScheduleValue(CFloRadSys(RadSysNum)%VolFlowSchedPtr)
    ELSE
      PumpPartLoadRat = 1.0d0
    END IF
    CFloRadSys(RadSysNum)%PumpPower        = PumpPartLoadRat*CFloRadSys(RadSysNum)%NomPowerUse
    ShaftPower                             = CFloRadSys(RadSysNum)%PumpPower * CFloRadSys(RadSysNum)%MotorEffic
          ! This adds the pump heat based on User input for the pump (same as in Pump module)
          ! We assume that all of the heat ends up in the fluid eventually since this is a closed loop.
    CFloRadSys(RadSysNum)%PumpHeattoFluid  = ShaftPower                                       &
                                            +( (CFloRadSys(RadSysNum)%PumpPower - ShaftPower) &
                                              * CFloRadSys(RadSysNum)%FracMotorLossToFluid )
    IF (CFloRadSys(RadSysNum)%PumpMassFlowRate > 0.d0) THEN
      PumpTempRise = CFloRadSys(RadSysNum)%PumpHeattoFluid / (CFloRadSys(RadSysNum)%PumpMassFlowRate * CpFluid)
    ELSE
      PumpTempRise = 0.d0
    ENDIF

    LoopReqTemp  = RadInTemp - PumpTempRise ! Temperature required at the inlet of the pump to meet the temperature request

    IF (OperatingMode == HeatingMode) THEN

      ! in heating mode so shut down cold water flow request
      IF (CFloRadSys(RadSysNum)%CWLoopNum > 0 ) THEN
        mdot = 0.d0
        CALL SetComponentFlowRate(mdot, &
                            CFloRadSys(RadSysNum)%ColdWaterInNode, &
                            CFloRadSys(RadSysNum)%ColdWaterOutNode, &
                            CFloRadSys(RadSysNum)%CWLoopNum, &
                            CFloRadSys(RadSysNum)%CWLoopSide, &
                            CFloRadSys(RadSysNum)%CWBranchNum, &
                            CFloRadSys(RadSysNum)%CWCompNum)
      ENDIF
      LoopInNode     = CFloRadSys(RadSysNum)%HotWaterInNode
      SysWaterInTemp = Node(LoopInNode)%Temp
      Iteration      = .FALSE.

      IF ( (SysWaterInTemp >= LoopReqTemp) .AND. &
           (Node(LoopInNode)%MassFlowRateMaxAvail >= CFloRadSys(RadSysNum)%WaterMassFlowRate) ) THEN
          ! Case 1: Adequate temperature and flow
          ! Best condition--loop inlet temperature greater than requested and we have enough flow.
          ! So, proceed assuming the RadInTemp requested by the controls and then figure out the
          ! mixing after the outlet radiant temperature is calculated.
        CFloRadSys(RadSysNum)%WaterInletTemp = RadInTemp
        CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)

          ! We now have inlet and outlet temperatures--we still need to set the flow rates
        IF ((SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) /= 0.d0) THEN ! protect divide by zero
          CFloRadSys(RadSysNum)%WaterInjectionRate =  ( CFloRadSys(RadSysNum)%WaterMassFlowRate                    &
                                           *(CFloRadSys(RadSysNum)%WaterInletTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)   &
                                                       /(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) ) &
                                                     -( CFloRadSys(RadSysNum)%PumpHeattoFluid                      &
                                                       /(CpFluid*(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)) )
        ENDIF
        CFloRadSys(RadSysNum)%WaterRecircRate    = CFloRadSys(RadSysNum)%WaterMassFlowRate &
                                                  -CFloRadSys(RadSysNum)%WaterInjectionRate

      ELSE IF ( (SysWaterInTemp < LoopReqTemp) .AND. &
                (Node(LoopInNode)%MassFlowRateMaxAvail >= CFloRadSys(RadSysNum)%WaterMassFlowRate) ) THEN
          ! Case 2: Adequate flow but temperature too low
          ! Only thing to do is to reset the inlet temperature and assume that the loop will supply
          ! the entire flow to the component (no recirculation but potentially some bypass for the
          ! overall loop).  There is no way we can meet the control temperature so don't even try.
        CFloRadSys(RadSysNum)%WaterInletTemp = SysWaterInTemp + PumpTempRise
        CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)

          ! We now have inlet and outlet temperatures--we still need to set the flow rates
        IF ((SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) /= 0.d0) THEN ! protect divide by zero
          CFloRadSys(RadSysNum)%WaterInjectionRate =  ( CFloRadSys(RadSysNum)%WaterMassFlowRate                    &
                                             *(CFloRadSys(RadSysNum)%WaterInletTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)   &
                                                       /(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) ) &
                                                     -( CFloRadSys(RadSysNum)%PumpHeattoFluid                      &
                                                       /(CpFluid*(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)) )
        ELSE
          CFloRadSys(RadSysNum)%WaterInjectionRate = CFloRadSys(RadSysNum)%WaterMassFlowRate
        ENDIF
        IF (CFloRadSys(RadSysNum)%WaterInjectionRate > CFloRadSys(RadSysNum)%WaterMassFlowRate) &
          CFloRadSys(RadSysNum)%WaterInjectionRate = CFloRadSys(RadSysNum)%WaterMassFlowRate
        CFloRadSys(RadSysNum)%WaterRecircRate    = 0.0d0  ! by definition

      ELSE IF ( (SysWaterInTemp >= LoopReqTemp) .AND. &
                (Node(LoopInNode)%MassFlowRateMaxAvail < CFloRadSys(RadSysNum)%WaterMassFlowRate) ) THEN
          ! Case 3: Adequate temperature but loop flow is less than component flow
          ! This case might work out, but there is no guarantee that there is enough loop flow to
          ! mix with the recirculation flow and still provide a high enough temperature.  First
          ! step is to try the inlet temperature and flow rate as in Case 1.  If we can obtain
          ! the proper temperature inlet to the radiant system, then we are done.  If not, we
          ! have to repeat the solution for an unknown inlet temperature and a known recirculation
          ! rate.
        CFloRadSys(RadSysNum)%WaterInletTemp = RadInTemp
        CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)

          ! Now see if we can really get that desired into temperature (RadInTemp) by solving
          ! for the flow that is injected from the loop.  A heat balance for the mixer that relates
          ! the important quantities is:
          !   Mdotradsys*Cp*Tradsysin = Mdotloop*Cp*Tloop + (Mdotradsys-Mdotloop)*Cp*Tradsysout + PumpHeat
          ! or rearranging to get the injection flow (Mdotloop):
          !   Mdotloop = Mdotcomp*(Tradsysin-Tradsysout)/(Tloop-Tradsysout) - PumpHeat/(Cp*(Tloop-Tradsysout))
          ! If Mdotloop from this equation is greater that the loop flow rate (Node%MassFlowRate),
          ! then we cannot meet the inlet temperature and we have to "iterate" through the
          ! alternate solution.
        InjectFlowRate = ( CFloRadSys(RadSysNum)%WaterMassFlowRate                    &
                       *(CFloRadSys(RadSysNum)%WaterInletTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)   &
                          /(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) ) &
                        -( CFloRadSys(RadSysNum)%PumpHeattoFluid                      &
                          /(CpFluid*(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)) )
        IF (InjectFlowRate > Node(LoopInNode)%MassFlowRateMaxAvail) THEN
          ! We didn't have enough flow from the loop to meet our inlet temperature request.
          ! So, set the injection rate to the loop flow and calculate the recirculation flow.
          ! Then, resimulate the radiant system using these values (it will obtain the actual
          ! inlet temperature that results from this).
          CFloRadSys(RadSysNum)%WaterInjectionRate = Node(LoopInNode)%MassFlowRateMaxAvail
          CFloRadSys(RadSysNum)%WaterRecircRate    = CFloRadSys(RadSysNum)%WaterMassFlowRate &
                                                    -CFloRadSys(RadSysNum)%WaterInjectionRate
          CFloRadSys(RadSysNum)%WaterInletTemp     = SysWaterInTemp + PumpTempRise
          Iteration                                = .TRUE.
          CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)
        ELSE
          CFloRadSys(RadSysNum)%WaterInjectionRate = InjectFlowRate
          CFloRadSys(RadSysNum)%WaterRecircRate    = CFloRadSys(RadSysNum)%WaterMassFlowRate &
                                                    -CFloRadSys(RadSysNum)%WaterInjectionRate

        END IF

      ELSE IF ( (SysWaterInTemp < LoopReqTemp) .AND. &
                (Node(LoopInNode)%MassFlowRateMaxAvail < CFloRadSys(RadSysNum)%WaterMassFlowRate) ) THEN
          ! Case 4: Temperature too low and loop flow is less than component flow
          ! Worst condition--can't meet the temperature request at all.  Only thing to do is to
          ! set the loop flow and recirculation rate (known) and solve for the inlet temperature
          ! using the "iteration" solution scheme from "Case 3B" above
        CFloRadSys(RadSysNum)%WaterInjectionRate = Node(LoopInNode)%MassFlowRateMaxAvail
        CFloRadSys(RadSysNum)%WaterRecircRate    = CFloRadSys(RadSysNum)%WaterMassFlowRate &
                                                  -CFloRadSys(RadSysNum)%WaterInjectionRate
        CFloRadSys(RadSysNum)%WaterInletTemp     = SysWaterInTemp + PumpTempRise
        Iteration                                = .TRUE.
        CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)

      END IF

    ELSE IF (OperatingMode == CoolingMode) THEN

      ! in cooling mode so shut down heating water flow request
      IF (CFloRadSys(RadSysNum)%HWLoopNum > 0) THEN
        mdot = 0.d0
        CALL SetComponentFlowRate(mdot, &
                                  CFloRadSys(RadSysNum)%HotWaterInNode, &
                                  CFloRadSys(RadSysNum)%HotWaterOutNode, &
                                  CFloRadSys(RadSysNum)%HWLoopNum, &
                                  CFloRadSys(RadSysNum)%HWLoopSide, &
                                  CFloRadSys(RadSysNum)%HWBranchNum, &
                                  CFloRadSys(RadSysNum)%HWCompNum )
      ENDIF
      LoopInNode     = CFloRadSys(RadSysNum)%ColdWaterInNode
      SysWaterInTemp = Node(LoopInNode)%Temp
      CFloCondIterNum = 1
      DO WHILE ( (CFloCondIterNum <= 1) .OR. &
                 ( (CFloCondIterNum <= 2) .AND. (CFloRadSys(RadSysNum)%CondCtrlType == CondCtrlVariedOff) .AND. (VarOffCond) ) )
        Iteration      = .FALSE.

        IF ( (SysWaterInTemp <= LoopReqTemp) .AND. &
             (Node(LoopInNode)%MassFlowRateMaxAvail >= CFloRadSys(RadSysNum)%WaterMassFlowRate) ) THEN
          ! Case 1: Adequate temperature and flow
          ! Best condition--loop inlet temperature lower than requested and we have enough flow.
          ! So, proceed assuming the RadInTemp requested by the controls and then figure out the
          ! mixing after the outlet radiant temperature is calculated.

          ! This condition can also happen when LoopReqTemp has been reset  to dewpoint for condensation control
          IF (.NOT. VarOffCond) THEN
            CFloRadSys(RadSysNum)%WaterInletTemp = RadInTemp
          ELSE
            CFloRadSys(RadSysNum)%WaterInletTemp = LoopReqTemp
          ENDIF
          CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)

          ! We now have inlet and outlet temperatures--we still need to set the flow rates
          CFloRadSys(RadSysNum)%WaterInjectionRate =  ( CFloRadSys(RadSysNum)%WaterMassFlowRate                    &
                                                 *(CFloRadSys(RadSysNum)%WaterInletTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) &
                                                       /(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) ) &
                                                     -( CFloRadSys(RadSysNum)%PumpHeattoFluid                      &
                                                       /(CpFluid*(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)) )
          CFloRadSys(RadSysNum)%WaterRecircRate    = CFloRadSys(RadSysNum)%WaterMassFlowRate &
                                                    -CFloRadSys(RadSysNum)%WaterInjectionRate

        ELSE IF ( (SysWaterInTemp > LoopReqTemp) .AND. &
                  (Node(LoopInNode)%MassFlowRateMaxAvail >= CFloRadSys(RadSysNum)%WaterMassFlowRate) ) THEN
          ! Case 2: Adequate flow but temperature too high
          ! Only thing to do is to reset the inlet temperature and assume that the loop will supply
          ! the entire flow to the component (no recirculation but potentially some bypass for the
          ! overall loop).  There is no way we can meet the control temperature so don't even try.
          CFloRadSys(RadSysNum)%WaterInletTemp = SysWaterInTemp + PumpTempRise
          CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)

          ! We now have inlet and outlet temperatures--we still need to set the flow rates
          IF ((SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) /= 0.d0) THEN ! protect div by zero
            CFloRadSys(RadSysNum)%WaterInjectionRate =  ( CFloRadSys(RadSysNum)%WaterMassFlowRate                    &
                                                 *(CFloRadSys(RadSysNum)%WaterInletTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) &
                                                         /(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp ) ) &
                                                       -( CFloRadSys(RadSysNum)%PumpHeattoFluid                      &
                                                         /(CpFluid*(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)) )
          ELSE ! no temp change present, set injection rate to full flow
            CFloRadSys(RadSysNum)%WaterInjectionRate = CFloRadSys(RadSysNum)%WaterMassFlowRate
          ENDIF
          IF (CFloRadSys(RadSysNum)%WaterInjectionRate > CFloRadSys(RadSysNum)%WaterMassFlowRate) &
            CFloRadSys(RadSysNum)%WaterInjectionRate = CFloRadSys(RadSysNum)%WaterMassFlowRate
          CFloRadSys(RadSysNum)%WaterRecircRate    = 0.0d0  ! by definition

        ELSE IF ( (SysWaterInTemp <= LoopReqTemp) .AND. &
                  (Node(LoopInNode)%MassFlowRateMaxAvail < CFloRadSys(RadSysNum)%WaterMassFlowRate) ) THEN
          ! Case 3: Adequate temperature but loop flow is less than component flow
          ! This case might work out, but there is no guarantee that there is enough loop flow to
          ! mix with the recirculation flow and still provide a high enough temperature.  First
          ! step is to try the inlet temperature and flow rate as in Case 1.  If we can obtain
          ! the proper temperature inlet to the radiant system, then we are done.  If not, we
          ! have to repeat the solution for an unknown inlet temperature and a known recirculation
          ! rate.
          ! This condition might happen when LoopReqTemp has been reset  to dewpoint for condensation control
          IF (.NOT. VarOffCond) THEN
            CFloRadSys(RadSysNum)%WaterInletTemp = RadInTemp
          ELSE
            CFloRadSys(RadSysNum)%WaterInletTemp = LoopReqTemp
          ENDIF
          CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)

          ! Now see if we can really get that desired into temperature (RadInTemp) by solving
          ! for the flow that is injected from the loop.  A heat balance for the mixer that relates
          ! the important quantities is:
          !   Mdotradsys*Cp*Tradsysin = Mdotloop*Cp*Tloop + (Mdotradsys-Mdotloop)*Cp*Tradsysout + PumpHeat
          ! or rearranging to get the injection flow (Mdotloop):
          !   Mdotloop = Mdotcomp*(Tradsysin-Tradsysout)/(Tloop-Tradsysout) - PumpHeat/(Cp*(Tloop-Tradsysout))
          ! If Mdotloop from this equation is greater that the loop flow rate (Node%MassFlowRate),
          ! then we cannot meet the inlet temperature and we have to "iterate" through the
          ! alternate solution.
          InjectFlowRate = ( CFloRadSys(RadSysNum)%WaterMassFlowRate                    &
                            *(CFloRadSys(RadSysNum)%WaterInletTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)        &
                            /(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp) ) &
                          -( CFloRadSys(RadSysNum)%PumpHeattoFluid                      &
                            /(CpFluid*(SysWaterInTemp - CFloRadSys(RadSysNum)%WaterOutletTemp)) )
          IF (InjectFlowRate > Node(LoopInNode)%MassFlowRateMaxAvail) THEN
          ! We didn't have enough flow from the loop to meet our inlet temperature request.
          ! So, set the injection rate to the loop flow and calculate the recirculation flow.
          ! Then, resimulate the radiant system using these values (it will obtain the actual
          ! inlet temperature that results from this).
            CFloRadSys(RadSysNum)%WaterInjectionRate = Node(LoopInNode)%MassFlowRateMaxAvail
            CFloRadSys(RadSysNum)%WaterRecircRate    = CFloRadSys(RadSysNum)%WaterMassFlowRate &
                                                      -CFloRadSys(RadSysNum)%WaterInjectionRate
            CFloRadSys(RadSysNum)%WaterInletTemp     = SysWaterInTemp + PumpTempRise
            Iteration                                = .TRUE.
            CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)
          ELSE
            CFloRadSys(RadSysNum)%WaterInjectionRate = InjectFlowRate
            CFloRadSys(RadSysNum)%WaterRecircRate    = CFloRadSys(RadSysNum)%WaterMassFlowRate &
                                                      -CFloRadSys(RadSysNum)%WaterInjectionRate

          END IF

        ELSE IF ( (SysWaterInTemp > LoopReqTemp) .AND. &
                  (Node(LoopInNode)%MassFlowRateMaxAvail < CFloRadSys(RadSysNum)%WaterMassFlowRate) ) THEN
          ! Case 4: Temperature too low and loop flow is less than component flow
          ! Worst condition--can't meet the temperature request at all.  Only thing to do is to
          ! set the loop flow and recirculation rate (known) and solve for the inlet temperature
          ! using the "iteration" solution scheme from "Case 3B" above
          CFloRadSys(RadSysNum)%WaterInjectionRate = Node(LoopInNode)%MassFlowRateMaxAvail
          CFloRadSys(RadSysNum)%WaterRecircRate    = CFloRadSys(RadSysNum)%WaterMassFlowRate &
                                                    -CFloRadSys(RadSysNum)%WaterInjectionRate
          CFloRadSys(RadSysNum)%WaterInletTemp     = SysWaterInTemp + PumpTempRise
          Iteration                                = .TRUE.
          CALL CalcLowTempCFloRadSysComps(RadSysNum,LoopInNode,Iteration,LoadMet)

        END IF

        CFloCondIterNum = CFloCondIterNum + 1

      END DO

    END IF  ! Operating mode (heating or cooling)

          ! Case when system has been shut down because of condensation issues or other limitations:
    IF (CFloRadSys(RadSysNum)%WaterMassFlowRate < MassFlowTolerance) THEN
      CFloRadSys(RadSysNum)%WaterMassFlowRate  = 0.0d0
      CFloRadSys(RadSysNum)%WaterInjectionRate = 0.0d0
      CFloRadSys(RadSysNum)%WaterRecircRate    = 0.0d0
      CFloRadSys(RadSysNum)%PumpMassFlowRate   = 0.0d0
      OperatingMode                            = NotOperating
    END IF

          ! There are some cases when the pump heat is actually enough to provide all the heating that the system needs.
          ! In this case, the water injection flow rate will come back as a slightly negative number.  Reset it to zero
          ! and just recirculate all the flow through the local loop.
    IF (CFloRadSys(RadSysNum)%WaterInjectionRate < 0.0d0) THEN
      CFloRadSys(RadSysNum)%WaterInjectionRate = 0.0d0
      CFloRadSys(RadSysNum)%WaterRecircRate    = CFloRadSys(RadSysNum)%WaterMassFlowRate
    END IF

          ! Error check, just in case
    IF (CFloRadSys(RadSysNum)%WaterRecircRate < 0.0d0) THEN
      CALL ShowWarningError('Flow mismatch in radiant system--result will be an energy imbalance--should not get this error')
      CALL ShowContinueErrorTimeStamp('WaterRecircRate='//TRIM(TrimSigDigits(CFloRadSys(RadSysNum)%WaterRecircRate,2))//  &
                             ', in Radiant System='//TRIM(CFloRadSys(RadSysNum)%Name)//  &
                             ',')
      CFloRadSys(RadSysNum)%WaterRecircRate    = 0.0d0
      CFloRadSys(RadSysNum)%WaterInjectionRate = CFloRadSys(RadSysNum)%WaterMassFlowRate
    END IF

  END IF    ! System running mode (yes or no)

  RETURN

END SUBROUTINE CalcLowTempCFloRadiantSystem


SUBROUTINE CalcLowTempCFloRadSysComps(RadSysNum,MainLoopNodeIn,Iteration,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   August 2003
          !       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves the radiant system based on how much water is (and
          ! the conditions of the water) supplied to the radiant system.  The purpose
          ! of this subroutine is similar to CalcLowTempHydrRadSysComps except that
          ! it solves this for a constant flow hydronic radiant system.

          ! METHODOLOGY EMPLOYED:
          ! Use heat exchanger formulas to obtain the heat source/sink for the radiant
          ! system based on the inlet conditions and flow rate of water.  Once that is
          ! determined, recalculate the surface heat balances to reflect this heat
          ! addition/subtraction.  The load met by the system is determined by the
          ! difference between the convection from all surfaces in the zone when
          ! there was no radiant system output and with a source/sink added.

          ! REFERENCES:
          ! IBLAST-QTF research program, completed in January 1995 (unreleased)
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.

          ! USE STATEMENTS:
  USE DataEnvironment,    ONLY : OutBaroPress
  USE General,            ONLY : RoundSigDigits
  USE DataHeatBalance,    ONLY : Construct, Zone
  USE DataHeatBalFanSys,  ONLY : RadSysTiHBConstCoef,                   &
                                 RadSysTiHBToutCoef,RadSysTiHBQsrcCoef, &
                                 RadSysToHBConstCoef,RadSysToHBTinCoef, &
                                 RadSysToHBQsrcCoef,CTFTsrcConstPart,   &
                                 ZoneAirHumRat
  USE DataHeatBalSurface, ONLY : TH
  USE DataLoopNode,       ONLY : Node
  USE DataSurfaces,       ONLY : Surface, HeatTransferModel_CondFD, HeatTransferModel_CTF
  USE FluidProperties,    ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,     ONLY : SetComponentFlowRate


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: RadSysNum         ! Index for the low temperature radiant system under consideration
  INTEGER, INTENT(IN) :: MainLoopNodeIn    ! Node number on main loop of the inlet node to the radiant system
  LOGICAL, INTENT(IN) :: Iteration         ! FALSE for the regular solution, TRUE when we had to loop back
                                            ! through and figure out more information (i.e., did not know the
                                            ! inlet temperature directly)
  REAL(r64),    INTENT(INOUT) :: LoadMet           ! Load met by the low temperature radiant system, in Watts

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: CondDeltaTemp  = 1.0d0   ! How close the surface temperatures can get to the dewpoint temperature of a space
                                            ! before the radiant cooling system shuts off the flow.
  REAL(r64), PARAMETER :: TempCheckLimit = 0.1d0   ! Maximum allowed temperature difference between outlet temperature calculations
  REAL(r64), PARAMETER :: ZeroSystemResp = 0.1d0   ! Response below which the system response is really zero

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ConstrNum      ! Index for construction number in Construct derived type
  REAL(r64) :: Cp             ! Intermediate calculational variable for specific heat of water
  REAL(r64) :: DewPointTemp   ! Dew-point temperature based on the zone air conditions
  REAL(r64) :: EpsMdotCp      ! Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat
  REAL(r64) :: LoopTerm       ! Intermeidate calculation variable for determining the water inlet temperature
  REAL(r64) :: Mdot           ! Intermediate calculation variable for mass flow rate in a surface within the radiant system
  INTEGER :: RadSurfNum     ! DO loop counter for the surfaces that comprise a particular radiant system
  INTEGER :: RadSurfNum2    ! DO loop counter for the surfaces that comprise a particular radiant system
  INTEGER :: RadSurfNum3    ! DO loop counter for the surfaces that comprise a particular radiant system
  REAL(r64) :: RecircTerm     ! Intermeidate calculation variable for determining the water inlet temperature
  REAL(r64) :: SumFlowFracCkCm       ! Summation of surface flow fraction, Ck, and Cm product for each surface in the system
  REAL(r64) :: SumFlowFracOneMinusCm ! Summation of surface flow fraction times (1-Cm) for each surface in the radiant system
  INTEGER :: SurfNum        ! Index for radiant surface in Surface derived type
  INTEGER :: SurfNum2       ! Index for radiant surface in Surface derived type
  REAL(r64) :: TotalRadSysPower      ! Total heat source/sink to radiant system
  REAL(r64) :: TwiCoeff       ! Intermeidate calculation variable for determining the water inlet temperature
  REAL(r64) :: WaterMassFlow  ! Water mass flow rate in the radiant system, kg/s
  INTEGER :: WaterNodeIn    ! Node number of the water entering the radiant system
  REAL(r64) :: WaterOutletTempCheck ! Radiant system water outlet temperature (calculated from mixing all outlet streams together)
  REAL(r64) :: WaterTempIn    ! Temperature of the water entering the radiant system, in C
  INTEGER :: ZoneNum        ! number of zone being served
  REAL(r64) :: ZoneMult       ! Zone multiplier for this system

  REAL(r64) :: Ca,Cb,Cc,Cd,Ce,Cf,Cg,Ch,Ci,Cj,Ck,Cl  ! Coefficients to relate the inlet water temperature to the heat source
                                                  ! For more info on Ca through Cl, see comments below

  REAL(r64), SAVE, DIMENSION(:), ALLOCATABLE :: Ckj, Cmj     ! Coefficients for individual surfaces within a radiant system
  REAL(r64), SAVE, DIMENSION(:), ALLOCATABLE :: WaterTempOut ! Array of outlet water temperatures for
                                                             ! each surface in the radiant system

  LOGICAL, SAVE :: FirstTimeFlag=.true.  ! for setting size of Ckj, Cmj, WaterTempOut arrays

          ! FLOW:
          ! First, apply heat exchanger logic to find the heat source/sink to the system.
          ! This involves finding out the heat transfer characteristics of the hydronic
          ! loop and then applying the equations derived on pp. 113-118 of the dissertation.
  IF (FirstTimeFlag) THEN
    ALLOCATE (Ckj(MaxCloNumOfSurfaces))
    ALLOCATE (Cmj(MaxCloNumOfSurfaces))
    ALLOCATE (WaterTempOut(MaxCloNumOfSurfaces))
    FirstTimeFlag=.false.
  ENDIF

  Ckj          = 0.0d0
  Cmj          = 0.0d0
  WaterTempOut = CFloRadSys(RadSysNum)%WaterInletTemp

          ! Set the conditions on the water side inlet
  SELECT CASE(OperatingMode)
    CASE (HeatingMode)
      WaterNodeIn = CFloRadSys(RadSysNum)%HotWaterInNode
    CASE (CoolingMode)
      WaterNodeIn = CFloRadSys(RadSysNum)%ColdWaterInNode
    CASE DEFAULT
      CALL ShowSevereError('Illegal low temperature radiant system operating mode')
      CALL ShowContinueError('Occurs in Radiant System='//TRIM(CFloRadSys(RadSysNum)%Name))
      CALL ShowFatalError('Preceding condition causes termination.')
  END SELECT
  ZoneNum       = CFloRadSys(RadSysNum)%ZonePtr
  ZoneMult      = REAL(Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier,r64)
  WaterMassFlow = CFloRadSys(RadSysNum)%WaterMassFlowRate / ZoneMult
  WaterTempIn   = CFloRadSys(RadSysNum)%WaterInletTemp

  IF (WaterMassFlow <= 0.0d0) THEN
          ! No flow or below minimum allowed so there is no heat source/sink
          ! This is possible with a mismatch between system and plant operation
          ! or a slight mismatch between zone and system controls.  This is not
          ! necessarily a "problem" so this exception is necessary in the code.
    DO RadSurfNum = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
      SurfNum                = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
      QRadSysSource(SurfNum) = 0.0D0
      IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
        QRadSysSource(Surface(SurfNum)%ExtBoundCond) = 0.0D0    ! Also zero the other side of an interzone
    END DO

    CFloRadSys(RadSysNum)%WaterOutletTemp = CFloRadSys(RadSysNum)%WaterInletTemp

  ELSE

    DO RadSurfNum = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
      SurfNum = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
          ! Determine the heat exchanger "effectiveness" term
      EpsMdotCp = CalcRadSysHXEffectTerm(RadSysNum,ConstantFlowSystem,WaterTempIn,WaterMassFlow,               &
                                         CFloRadSys(RadSysNum)%SurfaceFlowFrac(RadSurfNum), &
                                         CFloRadSys(RadSysNum)%NumCircuits(RadSurfNum), &
                                         CFloRadSys(RadSysNum)%TubeLength,                  &
                                         CFloRadSys(RadSysNum)%TubeDiameter,                &
                                         CFloRadSys(RadSysNum)%GlycolIndex)

          ! Obtain the heat balance coefficients and calculate the intermediate coefficients
          ! linking the inlet water temperature to the heat source/sink to the radiant system.
          ! The coefficients are based on the following development...
          ! The heat balance equations at the outside and inside surfaces are of the form:
          !   Tinside  = Ca + Cb*Toutside + Cc*q"
          !   Toutside = Cd + Ce*Tinside  + Cf*q"
          !   Tsource  = Cg + Ch*q"       + Ci*Tinside + Cj*Toutside
          ! where:
          !   Tinside is the temperature at the inside surface
          !   Toutside is the temperature at the outside surface
          !   Tsource is the temperature within the radiant system at the location of the source/sink
          !   Ca is all of the other terms in the inside heat balance (solar, LW exchange, conduction history terms, etc.)
          !   Cb is the current cross CTF term
          !   Cc is the QTF inside term for the current heat source/sink
          !   Cd is all of the other terms in the outside heat balance (solar, LW exchange, conduction history terms, etc.)
          !   Ce is the current cross CTF term (should be equal to Cb)
          !   Cf is the QTF outside term for the current heat source/sink
          !   Cg is the summation of all temperature and source history terms at the source/sink location
          !   Ch is the QTF term at the source/sink location for the current heat source/sink
          !   Ci is the CTF inside term for the current inside surface temperature
          !   Cj is the CTF outside term for the current outside surface temperature
          ! Note that it is necessary to not use "slow conduction" assumptions because the
          ! source/sink has an impact on BOTH the inside and outside surface heat balances.
          ! Hence the more general formulation.
          ! The first two T equations above can be solved to remove the other surface temperature.
          ! This results in the following equations:
          !   Tinside  = Ca + Cb*(Cd + Ce*Tinside + Cf*q") + Cc*q"   or...
          !   Tinside  = (Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)
          !   Toutside = Cd + Ce*(Ca + Cb*Toutside + Cc*q") + Cf*q"  or...
          !   Toutside = (Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb)
          ! Substituting the new equations for Tinside and Toutside as a function of C and q"
          ! into the equation for Tsource...
          !   Tsource  = Cg + Ch*q" + Ci*((Ca + Cb*Cd + (Cc+Cb*Cf)*q") / (1 - Ce*Cb)) &
          !                         + Cj*((Cd + Ce*Ca + (Cf+Ce*Cc)*q") / (1 - Ce*Cb))
          ! Or rearranging this to get Tsource as a function of q", we get...
          !   Tsource  =  Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb)) &
          !             +(Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb)))*q"
          ! Or in a slightly simpler form...
          !   Tsource  = Ck + Cl*q"
          ! where:
          !   Ck = Cg + ((Ci*(Ca + Cb*Cd) + Cj*(Cd + Ce*Ca))/(1-Ce*Cb))
          !   Cl = Ch + ((Ci*(Cc + Cb*Cf) + Cj*(Cf + Ce*Cc))/(1-Ce*Cb))
          ! Note also that from heat exchanger "algebra", we have:
          !   q = epsilon*qmax    and    qmax = Mdot*Cp*(Twaterin-Tsource)
          ! So...
          !   q" = q/Area = (epsilon*Mdot*Cp/Area)*(Twaterin-Tsource)
          ! Or rearranging this equation:
          !   Tsource = -(q"*A/(epsilon*Mdot*Cp)) + Twaterin
          ! Setting this equation equal to the other equation for Tsource a couple lines up
          ! and rearranging to solve for q"...
          !   q" = (Twaterin - Ck) / (Cl + (A/(epsilon*Mdot*Cp))
          ! or
          !   q  = (Twaterin - Ck) / ((Cl/A) + (1/epsilon*Mdot*Cp))
          ! or
          !   q  = epsilon*Mdot*Cp*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
          ! which is the desired result, that is the heat source or sink to the radiant
          ! system as a function of the water inlet temperature (flow rate is also in there
          ! as well as all of the heat balance terms "hidden" in Ck and Cl).

      ConstrNum = Surface(SurfNum)%Construction

        Ca = RadSysTiHBConstCoef(SurfNum)
        Cb = RadSysTiHBToutCoef(SurfNum)
        Cc = RadSysTiHBQsrcCoef(SurfNum)

        Cd = RadSysToHBConstCoef(SurfNum)
        Ce = RadSysToHBTinCoef(SurfNum)
        Cf = RadSysToHBQsrcCoef(SurfNum)

        Cg = CTFTsrcConstPart(SurfNum)
        Ch = Construct(ConstrNum)%CTFTSourceQ(0)
        Ci = Construct(ConstrNum)%CTFTSourceIn(0)
        Cj = Construct(ConstrNum)%CTFTSourceOut(0)

        Ck = Cg + ( ( Ci*(Ca+Cb*Cd) + Cj*(Cd+Ce*Ca) ) / ( 1.0d0 - Ce*Cb ) )
        Cl = Ch + ( ( Ci*(Cc+Cb*Cf) + Cj*(Cf+Ce*Cc) ) / ( 1.0d0 - Ce*Cb ) )

      Mdot = WaterMassFlow * CFloRadSys(RadSysNum)%SurfaceFlowFrac(RadSurfNum)
      Cp   = GetSpecificHeatGlycol('WATER',WaterTempIn,CFloRadSys(RadSysNum)%GlycolIndex,'CalcLowTempCFloRadSysComps')

      IF (.NOT. Iteration) THEN

        IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CTF)  &
            QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - Ck) &
                                /(1.0d0 + (EpsMdotCp*Cl/Surface(SurfNum)%Area) )

        IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD ) &
            QRadSysSource(SurfNum) = EpsMdotCp * (WaterTempIn - TCondFDSourceNode(SurfNum))

        IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
          QRadSysSource(Surface(SurfNum)%ExtBoundCond) = QRadSysSource(SurfNum)   ! Also set the other side of an interzone
        WaterTempOut(RadSurfNum) = WaterTempIn - (QRadSysSource(SurfNum)/(Mdot*Cp))
      ELSE ! (Iteration)
          ! In this case, we did not know the inlet temperature directly and have
          ! to figure it out as part of the solution.  Thus, we have to do a little
          ! more algebra.
          ! The last equation in the previous block was:
          !   q = epsilon*Mdot*Cp*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
          ! which combines with:
          !   q = Mdot*Cp*(Twaterin - Twaterout,j)
          ! so that:
          !   (Twaterin - Twaterout.j) = epsilon*(Twaterin - Ck) / (1+(epsilon*Mdot*Cp*Cl/A))
          ! Let:
          !   Cm = epsilonj / (1+(epsilonj*Mdot,j*Cp*Cl,j/A))
          ! for each surface in the radiant system.  This results in:
          !   (Twaterin - Twaterout,j) = Cm,j*(Twaterin - Ck,j)
          ! Or:
          !   Twaterout,j = (1 - Cm,j)*Twaterin + Cm,j*Ck,j
          ! This holds for each surface that is part of the radiant system (j).  To get the
          ! overall outlet temperature, we have to do a mixing calculation after all of the
          ! surfaces have been simulated:
          !   Twaterout = SUM(Fractionj*Twaterout,j)
          ! We also have to solve an energy balance at the mixing valve and add in pump heat.
          ! The energy balance at the mixing valve relates the loop inlet temperature (Tloopin)
          ! and the overall outlet temperature (Twaterout):
          !   Tpumpin = (Mdotloop/Mdotradsys)*Tloopin + (Mdotrecirc/Mdotradsys)*Twaterout
          ! This can then be related to the inlet water temperature to the radiant system
          ! after pump heat has been taken into account:
          !   Twaterin = (Mdotloop/Mdotradsys)*Tloopin + (Mdotrecirc/Mdotradsys)*Twaterout + PumpHeat/(Mdotradsys*Cp)
          ! Pluggin in the definition of Twaterout (sum equation above) and then the definition
          ! of each individual Twaterout,j equation (which is solely a function of Twaterin
          ! and coefficients), we can obtain an equation for Twaterin that consists of all
          ! known quantities.  This requires us to calculate Ck,j and Cm,j for all the radiant
          ! surfaces in the system first and then coming up with a calculation for Twaterin.
          ! After than, individual Twaterout,j can be calculated along with QRadSysSource.
        Ckj(RadSurfNum) = Ck
        Cmj(RadSurfNum) = (EpsMdotCp/(Mdot*Cp))/(1.0d0+(EpsMdotCp*Cl/Surface(SurfNum)%Area))

        IF (RadSurfNum == CFloRadSys(RadSysNum)%NumOfSurfaces) THEN ! Last one so we can now do the other calculations
          ! Equation for Twaterin is:
          !   Twaterin = (LoopTerm + RecircTerm)/(TwiCoeff)
          ! where:
          !   LoopTerm   = (Mdotloop/Mdotradsys)*Tloopin + PumpHeat/(Mdotradsys*Cp)
          !   RecircTerm = (Mdotrecirc/Mdotradsys)*SUM(FlowFracj*Ck,j*Cm,j)
          !   TwiCoeff   = 1 - (Mdotrecirc/Mdotradsys)*SUM(FlowFracj*(1 - Cm,j))
          SumFlowFracCkCm       = 0.0d0
          SumFlowFracOneMinusCm = 0.0d0
          DO RadSurfNum2 = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
            SumFlowFracCkCm       = SumFlowFracCkCm &
                                   +(CFloRadSys(RadSysNum)%SurfaceFlowFrac(RadSurfNum2)*Ckj(RadSurfNum)*Cmj(RadSurfNum2))
            SumFlowFracOneMinusCm = SumFlowFracOneMinusCm &
                                   +(CFloRadSys(RadSysNum)%SurfaceFlowFrac(RadSurfNum2)*(1.0d0 - Cmj(RadSurfNum2)))
          END DO

          LoopTerm = (CFloRadSys(RadSysNum)%WaterInjectionRate/CFloRadSys(RadSysNum)%WaterMassFlowRate)*Node(MainLoopNodeIn)%Temp &
                    +(CFloRadSys(RadSysNum)%PumpHeattoFluid/(CFloRadSys(RadSysNum)%WaterMassFlowRate*Cp))

          RecircTerm = (CFloRadSys(RadSysNum)%WaterRecircRate/CFloRadSys(RadSysNum)%WaterMassFlowRate)*SumFlowFracCkCm

          TwiCoeff = 1.0d0 - (CFloRadSys(RadSysNum)%WaterRecircRate/CFloRadSys(RadSysNum)%WaterMassFlowRate)*SumFlowFracOneMinusCm

          WaterTempIn = (LoopTerm + RecircTerm)/(TwiCoeff)

          CFloRadSys(RadSysNum)%WaterInletTemp = WaterTempIn

          DO RadSurfNum2 = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
            WaterTempOut(RadSurfNum2) = WaterTempIn*(1.0d0 - Cmj(RadSurfNum2)) + (Ckj(RadSurfNum2) * Cmj(RadSurfNum2))
            Mdot                      = WaterMassFlow * CFloRadSys(RadSysNum)%SurfaceFlowFrac(RadSurfNum2)
            SurfNum                   = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum2)
            QRadSysSource(SurfNum)    = Mdot*Cp*(WaterTempIn - WaterTempOut(RadSurfNum2))
            IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
              QRadSysSource(Surface(SurfNum)%ExtBoundCond) = QRadSysSource(SurfNum)   ! Also set the other side of an interzone
          END DO

        END IF

      END IF

    END DO

    DO RadSurfNum = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
      SurfNum = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
          ! "Temperature Comparison" Cut-off:
          ! Check to see whether or not the system should really be running.  If
          ! QRadSysSource is negative when we are in heating mode or QRadSysSource
          ! is positive when we are in cooling mode, then the radiant system will
          ! be doing the opposite of its intention.  In this case, the flow rate
          ! is set to zero to avoid heating in cooling mode or cooling in heating
          ! mode.
      IF ( ((OperatingMode == HeatingMode).AND.(QRadSysSource(SurfNum) <= 0.0d0)) .OR. &
           ((OperatingMode == CoolingMode).AND.(QRadSysSource(SurfNum) >= 0.0d0)) ) THEN
        WaterMassFlow                           = 0.0d0
        IF (OperatingMode==HeatingMode) THEN
          CALL SetComponentFlowRate(WaterMassFlow, &
                                  CFloRadSys(RadSysNum)%HotWaterInNode, &
                                  CFloRadSys(RadSysNum)%HotWaterOutNode, &
                                  CFloRadSys(RadSysNum)%HWLoopNum, &
                                  CFloRadSys(RadSysNum)%HWLoopSide, &
                                  CFloRadSys(RadSysNum)%HWBranchNum, &
                                  CFloRadSys(RadSysNum)%HWCompNum )
        ELSEIF (OperatingMode==CoolingMode) THEN
          CALL SetComponentFlowRate(WaterMassFlow, &
                                  CFloRadSys(RadSysNum)%ColdWaterInNode, &
                                  CFloRadSys(RadSysNum)%ColdWaterOutNode, &
                                  CFloRadSys(RadSysNum)%CWLoopNum, &
                                  CFloRadSys(RadSysNum)%CWLoopSide, &
                                  CFloRadSys(RadSysNum)%CWBranchNum, &
                                  CFloRadSys(RadSysNum)%CWCompNum)
        ENDIF
        CFloRadSys(RadSysNum)%WaterMassFlowRate = WaterMassFlow
        DO RadSurfNum2 = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
          SurfNum2 = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum2)
          QRadSysSource(SurfNum2) = 0.0D0
          IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
            QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
        END DO
        EXIT ! outer do loop
      END IF
    END DO
        ! Condensation Cut-off:
        ! Check to see whether there are any surface temperatures within the radiant system that have
        ! dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
        ! A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
        ! conditions.
    CFloRadSys(RadSysNum)%CondCausedShutDown = .FALSE.
    DewPointTemp = PsyTdpFnWPb(ZoneAirHumRat(CFloRadSys(RadSysNum)%ZonePtr),OutBaroPress)

    IF ( (OperatingMode == CoolingMode) .AND. (CFloRadSys(RadSysNum)%CondCtrlType == CondCtrlSimpleOff) ) THEN

      DO RadSurfNum2 = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
        IF (TH(CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2) < (DewPointTemp+CFloRadSys(RadSysNum)%CondDewPtDeltaT)) THEN
        ! Condensation warning--must shut off radiant system
          CFloRadSys(RadSysNum)%CondCausedShutDown = .TRUE.
          WaterMassFlow                           = 0.0d0
          CALL SetComponentFlowRate(WaterMassFlow, &
                                CFloRadSys(RadSysNum)%ColdWaterInNode, &
                                CFloRadSys(RadSysNum)%ColdWaterOutNode, &
                                CFloRadSys(RadSysNum)%CWLoopNum, &
                                CFloRadSys(RadSysNum)%CWLoopSide, &
                                CFloRadSys(RadSysNum)%CWBranchNum, &
                                CFloRadSys(RadSysNum)%CWCompNum)
          CFloRadSys(RadSysNum)%WaterMassFlowRate = WaterMassFlow
          DO RadSurfNum3 = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
            SurfNum2 = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum3)
            QRadSysSource(SurfNum2) = 0.0D0
            IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
              QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
          END DO
        ! Produce a warning message so that user knows the system was shut-off due to potential for condensation
          IF (.not. WarmupFlag) THEN
            IF (CFloRadSys(RadSysNum)%CondErrIndex == 0) THEN  ! allow errors up to number of radiant systems
              CALL ShowWarningMessage(cConstantFlowSystem//' ['//TRIM(CFloRadSys(RadSysNum)%Name)//']')
              CALL ShowContinueError('Surface ['//trim(Surface(CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum2))%Name)//  &
                 '] temperature below dew-point temperature--potential for condensation exists')
              CALL ShowContinueError('Flow to the radiant system will be shut-off to avoid condensation')
              CALL ShowContinueError('Predicted radiant system surface temperature = '// &
                                     trim(RoundSigDigits(TH(CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2),2)))
              CALL ShowContinueError('Zone dew-point temperature + safety delta T= '//  &
                 trim(RoundSigDigits(DewPointTemp+CFloRadSys(RadSysNum)%CondDewPtDeltaT,2)))
              CALL ShowContinueErrorTimeStamp(' ')
              CALL ShowContinueError('Note that a '//TRIM(RoundSigDigits(CFloRadSys(RadSysNum)%CondDewPtDeltaT,4))// &
                                     ' C safety was chosen in the input for the shut-off criteria')
              CALL ShowContinueError('Note also that this affects all surfaces that are part of this radiant system')
            END IF
            CALL ShowRecurringWarningErrorAtEnd(cConstantFlowSystem//' ['//TRIM(CFloRadSys(RadSysNum)%Name)//  &
                         '] condensation shut-off occurrence continues.',  &
                         CFloRadSys(RadSysNum)%CondErrIndex,ReportMinOf=DewPointTemp,ReportMaxOf=DewPointTemp,  &
                         ReportMaxUnits='C',ReportMinUnits='C')
          END IF
          EXIT ! outer do loop
        END IF
      END DO

    ELSE IF ( (OperatingMode == CoolingMode) .AND. (CFloRadSys(RadSysNum)%CondCtrlType == CondCtrlNone) ) THEN

      DO RadSurfNum2 = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
        IF (TH(CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2) < DewPointTemp) THEN
        ! Condensation occurring but user does not want to shut radiant system off ever
          CFloRadSys(RadSysNum)%CondCausedShutDown = .TRUE.
        END IF
      END DO

    ELSE IF ( (OperatingMode == CoolingMode) .AND. (CFloRadSys(RadSysNum)%CondCtrlType == CondCtrlVariedOff) ) THEN

      DO RadSurfNum2 = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
        IF (TH(CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2) < (DewPointTemp+CFloRadSys(RadSysNum)%CondDewPtDeltaT)) THEN
          VarOffCond = .TRUE.
          IF (CFloCondIterNum >= 2) THEN
        ! We have already iterated once so now we must shut off radiant system
            CFloRadSys(RadSysNum)%CondCausedShutDown = .TRUE.
            WaterMassFlow                           = 0.0d0
            CALL SetComponentFlowRate(WaterMassFlow, &
                                CFloRadSys(RadSysNum)%ColdWaterInNode, &
                                CFloRadSys(RadSysNum)%ColdWaterOutNode, &
                                CFloRadSys(RadSysNum)%CWLoopNum, &
                                CFloRadSys(RadSysNum)%CWLoopSide, &
                                CFloRadSys(RadSysNum)%CWBranchNum, &
                                CFloRadSys(RadSysNum)%CWCompNum)
            CFloRadSys(RadSysNum)%WaterMassFlowRate = WaterMassFlow
            DO RadSurfNum3 = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
              SurfNum2 = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum3)
              QRadSysSource(SurfNum2) = 0.0D0
              IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
                QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
            END DO
        ! Produce a warning message so that user knows the system was shut-off due to potential for condensation
            IF (.not. WarmupFlag) THEN
              IF (CFloRadSys(RadSysNum)%CondErrIndex == 0) THEN  ! allow errors up to number of radiant systems
                CALL ShowWarningMessage(cConstantFlowSystem//' ['//TRIM(CFloRadSys(RadSysNum)%Name)//']')
                CALL ShowContinueError('Surface ['//trim(Surface(CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum2))%Name)//  &
                   '] temperature below dew-point temperature--potential for condensation exists')
                CALL ShowContinueError('Flow to the radiant system will be shut-off to avoid condensation')
                CALL ShowContinueError('Predicted radiant system surface temperature = '// &
                                       trim(RoundSigDigits(TH(CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum2),1,2),2)))
                CALL ShowContinueError('Zone dew-point temperature + safety delta T= '//  &
                   trim(RoundSigDigits(DewPointTemp+CFloRadSys(RadSysNum)%CondDewPtDeltaT,2)))
                CALL ShowContinueErrorTimeStamp(' ')
                CALL ShowContinueError('Note that a '//TRIM(RoundSigDigits(CFloRadSys(RadSysNum)%CondDewPtDeltaT,4))// &
                                       ' C safety was chosen in the input for the shut-off criteria')
                CALL ShowContinueError('Note also that this affects all surfaces that are part of this radiant system')
              END IF
              CALL ShowRecurringWarningErrorAtEnd(cConstantFlowSystem//' ['//TRIM(CFloRadSys(RadSysNum)%Name)//  &
                         '] condensation shut-off occurrence continues.',  &
                         CFloRadSys(RadSysNum)%CondErrIndex,ReportMinOf=DewPointTemp,ReportMaxOf=DewPointTemp,  &
                         ReportMaxUnits='C',ReportMinUnits='C')
            END IF
            EXIT ! outer do loop
          ELSE ! (First iteration--reset loop required temperature and try again to avoid condensation)
            LoopReqTemp       = DewPointTemp+CFloRadSys(RadSysNum)%CondDewPtDeltaT
          END IF
        END IF
      END DO

    END IF

          ! Determine radiant system outlet temperature (two ways to calculate--use as a check)
    WaterOutletTempCheck = 0.0d0
    TotalRadSysPower     = 0.0d0
    DO RadSurfNum = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
      SurfNum              = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
      TotalRadSysPower     = TotalRadSysPower + QRadSysSource(SurfNum)
      WaterOutletTempCheck = WaterOutletTempCheck &
                            +(CFloRadSys(RadSysNum)%SurfaceFlowFrac(RadSurfNum)*WaterTempOut(RadSurfNum))
    END DO
    TotalRadSysPower = ZoneMult * TotalRadSysPower

    IF (CFloRadSys(RadSysNum)%WaterMassFlowRate > 0.0d0) THEN
      Cp   = GetSpecificHeatGlycol('WATER',WaterTempIn,CFloRadSys(RadSysNum)%GlycolIndex,'CalcLowTempCFloRadSysComps')
      CFloRadSys(RadSysNum)%WaterOutletTemp = CFloRadSys(RadSysNum)%WaterInletTemp &
                                             -(TotalRadSysPower/(CFloRadSys(RadSysNum)%WaterMassFlowRate*Cp))
      IF ( (ABS(CFloRadSys(RadSysNum)%WaterOutletTemp-WaterOutletTempCheck) > TempCheckLimit) .AND. &
            (ABS(TotalRadSysPower) > ZeroSystemResp) ) THEN
          ! If the total system power is zero, that means we have shut down and the temperatures won't match because of that
        CALL ShowWarningError('Radiant system water outlet temperature calculation mismatch--this should not happen')
      END IF
    ELSE
      CFloRadSys(RadSysNum)%WaterOutletTemp = CFloRadSys(RadSysNum)%WaterInletTemp
    END IF

  END IF

          ! Now that we have the source/sink term(s), we must redo the heat balances to obtain
          ! the new SumHATsurf value for the zone.  Note that the difference between the new
          ! SumHATsurf and the value originally calculated by the heat balance with a zero
          ! source for all radiant systems in the zone is the load met by the system (approximately).
  CALL CalcHeatBalanceOutsideSurf(ZoneNum)
  CALL CalcHeatBalanceInsideSurf(ZoneNum)

  LoadMet = SumHATsurf(CFloRadSys(RadSysNum)%ZonePtr) - ZeroSourceSumHATsurf(CFloRadSys(RadSysNum)%ZonePtr)

!  DEALLOCATE (Ckj)
!  DEALLOCATE (Cmj)
!  DEALLOCATE (WaterTempOut)

  RETURN

END SUBROUTINE CalcLowTempCFloRadSysComps


SUBROUTINE CalcLowTempElecRadiantSystem(RadSysNum,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does all of the stuff that is necessary to simulate
          ! a low temperature electric radiant heating system.  Calls are made to
          ! appropriate subroutines either in this module or outside of it.

          ! METHODOLOGY EMPLOYED:
          ! Follows the methods used by many other pieces of zone equipment except
          ! that we are controlling the electrical input to the building element's
          ! resistance heating wires.  Note that cooling is not allowed for such
          ! a system.

          ! REFERENCES:
          ! Other EnergyPlus modules
          ! IBLAST-QTF research program, completed in January 1995 (unreleased)
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.
          ! Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
          !   of Wisconsin-Madison.

          ! USE STATEMENTS:
!  USE DataEnvironment,   ONLY : OutDryBulbTemp, OutWetBulbTemp
  USE DataHeatBalance,   ONLY : MRT, Zone, ZoneData
  USE DataHeatBalFanSys, ONLY : MAT
  USE DataHVACGlobals,   ONLY : SmallLoad
  USE DataZoneEnergyDemands
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: RadSysNum           ! name of the low temperature radiant system
  REAL(r64),    INTENT(INOUT) :: LoadMet             ! load met by the radiant system, in Watts

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ControlTemp    ! Temperature of the parameter that is controlling the radiant system
  REAL(r64) :: HeatFrac       ! fraction of maximum electrical heat input to radiant system [dimensionless]
  REAL(r64) :: OffTemp        ! Temperature above which the radiant system should be completely off [C]
  REAL(r64) :: OpTemp         ! Operative temperature (approximately the average of MRT and MAT) [C]
  REAL(r64) :: QZnReq         ! heating or cooling needed by zone [Watts]
  INTEGER   :: RadSurfNum     ! number of surface that is the radiant system
  REAL(r64) :: SetPtTemp      ! Setpoint temperature [C]
  INTEGER   :: SurfNum        ! intermediate variable for surface number in Surface derived type
  INTEGER   :: ZoneNum        ! number of zone being served

          ! FLOW:
          ! initialize local variables
  ZoneNum  = ElecRadSys(RadSysNum)%ZonePtr
  HeatFrac = 0.0d0

  IF (GetCurrentScheduleValue(ElecRadSys(RadSysNum)%SchedPtr) <= 0.0d0) THEN

          ! Unit is off; set the heat source terms to zero
    DO RadSurfNum = 1, ElecRadSys(RadSysNum)%NumOfSurfaces
      SurfNum                = ElecRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
      QRadSysSource(SurfNum) = 0.0D0
      IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
        QRadSysSource(Surface(SurfNum)%ExtBoundCond) = 0.0d0  ! Also zero the other side of an interzone
    END DO

  ELSE    ! Unit might be on-->this section is intended to determine whether the controls say
          ! that the unit should be on or not

          ! Determine the current setpoint temperature and the temperature at which the unit should be completely off
    SetptTemp = GetCurrentScheduleValue(ElecRadSys(RadSysNum)%SetptSchedPtr)
    OffTemp   = SetptTemp + 0.5d0*ElecRadSys(RadSysNum)%ThrottlRange

          ! Determine the control temperature--what the setpoint/offtemp is being compared to for unit operation
    SELECT CASE (ElecRadSys(RadSysNum)%ControlType)
      CASE (MATControl)
        ControlTemp = MAT(ZoneNum)
      CASE (MRTControl)
        ControlTemp = MRT(ZoneNum)
      CASE (OperativeControl)
        ControlTemp = (MAT(ZoneNum)+MRT(ZoneNum))/2.0d0
      CASE (ODBControl)
        ControlTemp = Zone(ZoneNum)%OutDryBulbTemp
      CASE (OWBControl)
        ControlTemp = Zone(ZoneNum)%OutWetBulbTemp
      CASE DEFAULT    ! Should never get here
        ControlTemp = MAT(ZoneNum)
        CALL ShowSevereError('Illegal control type in low temperature radiant system: '//TRIM(ElecRadSys(RadSysNum)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')
    END SELECT

    IF (ControlTemp < OffTemp) THEN  ! HEATING MODE

      OperatingMode = HeatingMode

      HeatFrac = (OffTemp - ControlTemp)/ElecRadSys(RadSysNum)%ThrottlRange
      IF (HeatFrac < 0.0d0) HeatFrac = 0.0d0
      IF (HeatFrac > 1.0d0) HeatFrac = 1.0d0

          ! Set the heat source for the low temperature electric radiant system
      DO RadSurfNum = 1, ElecRadSys(RadSysNum)%NumOfSurfaces
        SurfNum                = ElecRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
        QRadSysSource(SurfNum) = HeatFrac*ElecRadSys(RadSysNum)%MaxElecPower*ElecRadSys(RadSysNum)%SurfacePowerFrac(RadSurfNum)
        IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
          QRadSysSource(Surface(SurfNum)%ExtBoundCond) = QRadSysSource(SurfNum) ! Also set the other side of an interzone
      END DO

          ! Now "simulate" the system by recalculating the heat balances
      CALL CalcHeatBalanceOutsideSurf(ZoneNum)
      CALL CalcHeatBalanceInsideSurf(ZoneNum)

      LoadMet = SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum)

    ELSE  !  OFF or COOLING MODE (not allowed for an electric low temperature radiant system), turn it off

      DO RadSurfNum = 1, ElecRadSys(RadSysNum)%NumOfSurfaces
        SurfNum                = ElecRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
        QRadSysSource(SurfNum) = 0.0D0
        IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
          QRadSysSource(Surface(SurfNum)%ExtBoundCond) = 0.0d0  ! Also zero the other side of an interzone
      END DO

    END IF

  END IF

  RETURN

END SUBROUTINE CalcLowTempElecRadiantSystem


SUBROUTINE UpdateLowTempRadiantSystem(FirstHVACIteration,RadSysNum,SystemType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does any updating that needs to be done for low
          ! temperature radiant heating and cooling systems.  One of the most
          ! important functions of this routine is to update the average heat
          ! source/sink for a particular system over the various system time
          ! steps that make up the zone time step.  For hydronic systems,
          ! this routine must also set the outlet water conditions.

          ! METHODOLOGY EMPLOYED:
          ! For the source/sink average update, if the system time step elapsed
          ! is still what it used to be, then either we are still iterating or
          ! we had to go back and shorten the time step.  As a result, we have
          ! to subtract out the previous value that we added.  If the system
          ! time step elapsed is different, then we just need to add the new
          ! values to the running average.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : TimeStepZone
  USE DataHeatBalance, ONLY : Zone
  USE DataHVACGlobals, ONLY : TimeStepSys, SysTimeElapsed
  USE DataLoopNode,    ONLY : Node
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop
  USE PlantUtilities,  ONLY : SafeCopyPlantNode, SetComponentFlowRate


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT(IN) :: RadSysNum  ! Index for the low temperature radiant system under consideration within the derived types
  INTEGER, INTENT(IN) :: SystemType ! Type of radiant system: hydronic, constant flow, or electric

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: BypassMassFlow     ! Local bypass for a constant flow radiant system (could have recirculation and/or bypass)
  REAL(r64) :: CpWater            ! Specific heat of water
  INTEGER :: RadSurfNum         ! DO loop counter for radiant surfaces in the system
  INTEGER :: SurfNum            ! Surface index number for the current radiant system
  INTEGER :: WaterInletNode     ! Node number for the water side inlet of the radiant system
  REAL(r64) :: TotalHeatSource    ! Total heat source or sink for a particular radiant system (sum of all surface source/sinks)
  INTEGER :: TotRadSurfaces     ! Total number of radiant surfaces in this system
  REAL(r64) :: WaterMassFlow      ! Flow rate of water in the radiant system
  INTEGER :: WaterOutletNode    ! Node number for the water side outlet of the radiant system
  REAL(r64) :: ZoneMult           ! Zone multiplier
  INTEGER :: ZoneNum            ! Zone for this radiant system


          ! FLOW:
  SELECT CASE (SystemType)
    CASE (HydronicSystem)
      TotRadSurfaces = HydrRadSys(RadSysNum)%NumOfSurfaces
    CASE (ConstantFlowSystem)
      TotRadSurfaces = CFloRadSys(RadSysNum)%NumOfSurfaces
    CASE (ElectricSystem)
      TotRadSurfaces = ElecRadSys(RadSysNum)%NumOfSurfaces
  END SELECT

  DO RadSurfNum = 1, TotRadSurfaces

    SELECT CASE (SystemType)
      CASE (HydronicSystem)
        SurfNum = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
      CASE (ConstantFlowSystem)
        SurfNum = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
      CASE (ElectricSystem)
        SurfNum = ElecRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
    END SELECT

    IF (LastSysTimeElapsed(SurfNum) == SysTimeElapsed) THEN
          ! Still iterating or reducing system time step, so subtract old values which were
          ! not valid
      QRadSysSrcAvg(SurfNum) = QRadSysSrcAvg(SurfNum) &
                              -LastQRadSysSrc(SurfNum)*LastTimeStepSys(SurfNum)/TimeStepZone
    END IF

          ! Update the running average and the "last" values with the current values of the appropriate variables
    QRadSysSrcAvg(SurfNum) = QRadSysSrcAvg(SurfNum) &
                            +QRadSysSource(SurfNum)*TimeStepSys/TimeStepZone

    LastQRadSysSrc(SurfNum)     = QRadSysSource(SurfNum)
    LastSysTimeElapsed(SurfNum) = SysTimeElapsed
    LastTimeStepSys(SurfNum)    = TimeStepSys

  END DO

          ! For a hydronic system, calculate the water side outlet conditions and set the
          ! appropriate conditions on the correct HVAC node.
  IF (SystemType==HydronicSystem) THEN

          ! First sum up all of the heat sources/sinks associated with this system
    TotalHeatSource = 0.0d0
    DO RadSurfNum = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
      SurfNum         = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
      TotalHeatSource = TotalHeatSource + QRadSysSource(SurfNum)
    END DO
    ZoneNum         = HydrRadSys(RadSysNum)%ZonePtr
    ZoneMult        = REAL(Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier,r64)
    TotalHeatSource = ZoneMult * TotalHeatSource

          ! Update the heating side of things
    IF (HydrRadSys(RadSysNum)%HeatingSystem) THEN

      WaterInletNode  = HydrRadSys(RadSysNum)%HotWaterInNode
      WaterOutletNode = HydrRadSys(RadSysNum)%HotWaterOutNode
      WaterMassFlow   = Node(WaterInletNode)%MassFlowRate

      CpWater  = GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidName, &
                             Node(WaterInletNode)%Temp, &
                             PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidIndex, &
                             'UpdateLowTempRadiantSystem')

      SELECT CASE (OperatingMode)

        CASE (HeatingMode)
          IF ( (CpWater > 0.0d0) .AND. (WaterMassFlow > 0.0d0) ) THEN
            CALL SafeCopyPlantNode(WaterInletNode, WaterOutletNode)
            ! Node(WaterOutletNode) = Node(WaterInletNode) ! bad practice, e.g. wipes out setpoints on outlet
            Node(WaterOutletNode)%Temp = Node(WaterInletNode)%Temp &
                                        -TotalHeatSource/WaterMassFlow/CpWater
          ELSE
            CALL SafeCopyPlantNode(WaterInletNode, WaterOutletNode)
          END IF

        CASE DEFAULT ! CoolingMode or not on
          CALL SafeCopyPlantNode(WaterInletNode, WaterOutletNode)
      END SELECT
      CALL CheckForOutOfRangeTempResult(SystemType, RadSysNum, Node(WaterOutletNode)%Temp, &
                                         Node(WaterInletNode)%Temp, WaterMassFlow)

    END IF

    IF (HydrRadSys(RadSysNum)%CoolingSystem) THEN

      WaterInletNode  = HydrRadSys(RadSysNum)%ColdWaterInNode
      WaterOutletNode = HydrRadSys(RadSysNum)%ColdWaterOutNode
      WaterMassFlow   = Node(WaterInletNode)%MassFlowRate

      CpWater   =     GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidName, &
                             Node(WaterInletNode)%Temp, &
                             PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidIndex, &
                             'UpdateLowTempRadiantSystem')

      SELECT CASE (OperatingMode)

        CASE (CoolingMode)
          IF ( (CpWater > 0.0d0) .AND. (WaterMassFlow > 0.0d0) ) THEN
            CALL SafeCopyPlantNode(WaterInletNode,WaterOutletNode)
            Node(WaterOutletNode)%Temp = Node(WaterInletNode)%Temp &
                                        -TotalHeatSource/WaterMassFlow/CpWater
          ELSE
            CALL SafeCopyPlantNode(WaterInletNode,WaterOutletNode)
          END IF

        CASE DEFAULT ! HeatingMode or not on
          CALL SafeCopyPlantNode(WaterInletNode,WaterOutletNode)
      END SELECT
      CALL CheckForOutOfRangeTempResult(SystemType, RadSysNum, Node(WaterOutletNode)%Temp, &
                                         Node(WaterInletNode)%Temp, WaterMassFlow)
    END IF

  END IF  ! ...end of Hydronic System block

          ! For a constant flow system, calculate the water side outlet conditions
          ! and set the appropriate conditions on the correct HVAC node.  This may
          ! require mixing if the main system does not provide all of the flow that
          ! the local radiant system circulates.
  IF (SystemType==ConstantFlowSystem) THEN

          ! Update the heating side of things
    IF (CFloRadSys(RadSysNum)%HeatingSystem) THEN

      WaterInletNode  = CFloRadSys(RadSysNum)%HotWaterInNode
      WaterOutletNode = CFloRadSys(RadSysNum)%HotWaterOutNode
      CpWater         = GetSpecificHeatGlycol(PlantLoop(CFloRadSys(RadSysNum)%HWLoopNum)%FluidName, &
                             Node(WaterInletNode)%Temp, &
                             PlantLoop(CFloRadSys(RadSysNum)%HWLoopNum)%FluidIndex, &
                             'UpdateLowTempRadiantSystem')
      Call SafeCopyPlantNode(WaterInletNode, WaterOutletNode)

      IF (OperatingMode == HeatingMode) THEN

        ! Leave the inlet and outlet flow alone (if high enough) and perform a bypass if more flow than needed
        IF (Node(WaterInletNode)%MassFlowRate <= CFloRadSys(RadSysNum)%WaterInjectionRate) THEN
        ! Note that the water injection rate has already been restricted to the maximum available flow
          Node(WaterOutletNode)%Temp         = CFloRadSys(RadSysNum)%WaterOutletTemp
        ELSE
        ! Loop is providing more flow than needed so perform a local bypass and
        ! mix the flows to obtain the proper outlet temperature.  In this case,
        ! the mass flow rates on the loop are left alone and the outlet temperature
        ! is calculated from a simple steady-steady, steady-flow energy balance.
          BypassMassFlow = Node(WaterInletNode)%MassFlowRate - CFloRadSys(RadSysNum)%WaterInjectionRate
          Node(WaterOutletNode)%Temp = ( (BypassMassFlow*Node(WaterInletNode)%Temp)                                         &
                                        +(CFloRadSys(RadSysNum)%WaterInjectionRate*CFloRadSys(RadSysNum)%WaterOutletTemp) ) &
                                      /(Node(WaterOutletNode)%MassFlowRate)
        END IF
      END IF
      CALL CheckForOutOfRangeTempResult(SystemType, RadSysNum, Node(WaterOutletNode)%Temp, &
                                         Node(WaterInletNode)%Temp, Node(WaterOutletNode)%MassFlowRate)
    END IF

    IF (CFloRadSys(RadSysNum)%CoolingSystem) THEN

      WaterInletNode  = CFloRadSys(RadSysNum)%ColdWaterInNode
      WaterOutletNode = CFloRadSys(RadSysNum)%ColdWaterOutNode
      CpWater         = GetSpecificHeatGlycol('WATER',Node(WaterInletNode)%Temp,CFloRadSys(RadSysNum)%GlycolIndex, &
                        'UpdateLowTempRadiantSystem')

      CALL SafeCopyPlantNode(WaterInletNode, WaterOutletNode)

      IF (OperatingMode == CoolingMode) THEN

        IF (Node(WaterInletNode)%MassFlowRate <= CFloRadSys(RadSysNum)%WaterInjectionRate) THEN
        ! Note that the water injection rate has already been restricted to the maximum available flow

          Node(WaterOutletNode)%Temp         = CFloRadSys(RadSysNum)%WaterOutletTemp
        ELSE
        ! Loop is providing more flow than needed so perform a local bypass and
        ! mix the flows to obtain the proper outlet temperature.  In this case,
        ! the mass flow rates on the loop are left alone and the outlet temperature
        ! is calculated from a simple steady-steady, steady-flow energy balance.
          BypassMassFlow = Node(WaterInletNode)%MassFlowRate - CFloRadSys(RadSysNum)%WaterInjectionRate
          Node(WaterOutletNode)%Temp = ( (BypassMassFlow*Node(WaterInletNode)%Temp)                                         &
                                        +(CFloRadSys(RadSysNum)%WaterInjectionRate*CFloRadSys(RadSysNum)%WaterOutletTemp) ) &
                                      /(Node(WaterOutletNode)%MassFlowRate)
        END IF

        CALL  CheckForOutOfRangeTempResult(SystemType, RadSysNum, Node(WaterOutletNode)%Temp, Node(WaterInletNode)%Temp, &
                                            Node(WaterOutletNode)%MassFlowRate)

      END IF

    END IF

  END IF  ! ...end of Constant Flow System block

      ! Electric systems just burn electrical current and do not need to update nodes.


  RETURN

END SUBROUTINE UpdateLowTempRadiantSystem

SUBROUTINE CheckForOutOfRangeTempResult(SystemType, RadSysNum, outletTemp, inletTemp, mdot)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! check for crazy, out of range temperature results for fluid leaving radiant system

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY : RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: SystemType
  INTEGER, INTENT(IN)   :: RadSysNum
  REAL(r64), INTENT(IN) :: outletTemp
  REAL(r64), INTENT(IN) :: inletTemp
  REAL(r64), INTENT(IN) :: mdot

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: UpperRangeLimit = 500.d0  ! high error trigger limit for when model is not working
  REAL(r64), PARAMETER :: LowerRangeLimit = -300.d0 ! Low error trigger limit for when model is not working

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: WarnTooLow  = .FALSE.
  LOGICAL :: WarnTooHigh = .FALSE.
  WarnTooLow  = .FALSE.
  WarnTooHigh = .FALSE.
  IF (OutletTemp < LowerRangeLimit) THEN
    WarnTooLow  = .TRUE.
  ENDIF

  IF (OutletTemp > UpperRangeLimit) THEN
    WarnTooHigh = .TRUE.
  ENDIF

  IF (WarnTooLow .OR. WarnTooHigh) THEN

    SELECT CASE (SystemType)
    CASE (HydronicSystem)
      IF (WarnTooLow) THEN
        IF (HydrRadSys(RadSysNum)%OutRangeLoErrorCount == 0) THEN
          CALL ShowSevereMessage('UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical.')
          CALL ShowContinueError('Occurs for radiant system name = '//TRIM(HydrRadSys(RadSysNum)%Name) )
          CALL ShowContinueError('Calculated radiant system outlet temperature = ' &
                                   //TRIM(RoundSigDigits(outletTemp, 3))//' [C]')
          CALL ShowContinueError('Radiant system inlet temperature = ' &
                                   //TRIM(RoundSigDigits(inletTemp, 3))//' [C]')
          CALL ShowContinueError('A possible cause is that the materials used in the internal source construction are ' &
                                  // 'not compatible with the model.')
        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('UpdateLowTempRadiantSystem: Detected low out of range outlet temperature result ' &
                                            //'for radiant system name ='//TRIM(HydrRadSys(RadSysNum)%Name), &
                                             HydrRadSys(RadSysNum)%OutRangeLoErrorCount, &
                                             ReportMaxOf = outletTemp, &
                                             ReportMinOf = outletTemp )
      ENDIF
      IF (WarnTooHigh) THEN
        IF (HydrRadSys(RadSysNum)%OutRangeHiErrorCount == 0) THEN
          CALL ShowSevereMessage('UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical.')
          CALL ShowContinueError('Occurs for radiant system name = '//TRIM(HydrRadSys(RadSysNum)%Name) )
          CALL ShowContinueError('Calculated radiant system outlet temperature = ' &
                                   //TRIM(RoundSigDigits(outletTemp, 3))//' [C]')
          CALL ShowContinueError('Radiant system inlet temperature = ' &
                                   //TRIM(RoundSigDigits(inletTemp, 3))//' [C]')
          CALL ShowContinueError('A possible cause is that the materials used in the internal source construction are ' &
                                  // 'not compatible with the model.')


        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('UpdateLowTempRadiantSystem: Detected high out of range outlet temperature result ' &
                                            //' radiant system name ='//TRIM(HydrRadSys(RadSysNum)%Name), &
                                             HydrRadSys(RadSysNum)%OutRangeHiErrorCount, &
                                             ReportMaxOf = outletTemp, &
                                             ReportMinOf = outletTemp )

      ENDIF

    CASE (ConstantFlowSystem)
      IF (WarnTooLow) THEN

        IF (CFloRadSys(RadSysNum)%OutRangeLoErrorCount == 0) THEN
          CALL ShowSevereMessage('UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical.')
          CALL ShowContinueError('Occurs for radiant system name = '//TRIM(CFloRadSys(RadSysNum)%Name) )
          CALL ShowContinueError('Calculated radiant system outlet temperature = ' &
                                   //TRIM(RoundSigDigits(outletTemp, 3))//' [C]')
          CALL ShowContinueError('Radiant system inlet temperature = ' &
                                   //TRIM(RoundSigDigits(inletTemp, 3))//' [C]')
          CALL ShowContinueError('A possible cause is that the materials used in the internal source construction are ' &
                                  // 'not compatible with the model.')


        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('UpdateLowTempRadiantSystem: Detected high out of range temperature result for ' &
                                            //' radiant system name ='//TRIM(CFloRadSys(RadSysNum)%Name), &
                                             CFloRadSys(RadSysNum)%OutRangeLoErrorCount, &
                                             ReportMaxOf = outletTemp, &
                                             ReportMinOf = outletTemp )

      ENDIF
      IF (WarnTooHigh) THEN
        IF (CFloRadSys(RadSysNum)%OutRangeHiErrorCount == 0) THEN
          CALL ShowSevereMessage('UpdateLowTempRadiantSystem: model result for fluid outlet temperature is not physical.')
          CALL ShowContinueError('Occurs for radiant system name = '//TRIM(CFloRadSys(RadSysNum)%Name) )
          CALL ShowContinueError('Calculated radiant system outlet temperature = ' &
                                   //TRIM(RoundSigDigits(outletTemp, 3))//' [C]')
          CALL ShowContinueError('Radiant system inlet temperature = ' &
                                   //TRIM(RoundSigDigits(inletTemp, 3))//' [C]')
          CALL ShowContinueError('A possible cause is that the materials used in the internal source construction are ' &
                                  // 'not compatible with the model.')


        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('UpdateLowTempRadiantSystem: Detected high out of range temperature result for ' &
                                            //' radiant system name ='//TRIM(CFloRadSys(RadSysNum)%Name), &
                                             CFloRadSys(RadSysNum)%OutRangeHiErrorCount, &
                                             ReportMaxOf = outletTemp, &
                                             ReportMinOf = outletTemp )

      ENDIF
    END SELECT


  ENDIF

  RETURN

END SUBROUTINE CheckForOutOfRangeTempResult


REAL(r64) FUNCTION CalcRadSysHXEffectTerm(RadSysNum,SystemType, Temperature,WaterMassFlow,  &
   FlowFraction,NumCircs,TubeLength,TubeDiameter,GlycolIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   December 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the radiant system "heat exchanger"
          ! effectiveness term.  This is equal to the mass flow rate of water
          ! times the specific heat of water times the effectiveness of
          ! the heat exchanger (radiant system "coil").

          ! METHODOLOGY EMPLOYED:
          ! Assumes that the only real heat transfer term that we have to
          ! deal with is the convection from the water to the tube.  The
          ! other assumptions are that the tube inside surface temperature
          ! is equal to the "source location temperature" and that it is
          ! a CONSTANT throughout the radiant system.  This is to make
          ! the problem more tractable and to fit with other system assumptions
          ! that were made elsewhere in the radiant system model.

          ! REFERENCES:
          ! Property data for water shown below as parameters taken from
          !   Incropera and DeWitt, Introduction to Heat Transfer, Table A.6.
          ! Heat exchanger information also from Incropera and DeWitt.
          ! Code based loosely on code from IBLAST program (research version)

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : PI
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: RadSysNum      ! Index number of radiant system under consideration !unused1208
  INTEGER, INTENT(IN) :: SystemType ! Type of radiant system: hydronic, constant flow, or electric
  REAL(r64),    INTENT(IN) :: Temperature    ! Temperature of water entering the radiant system, in C
  REAL(r64),    INTENT(IN) :: WaterMassFlow  ! Mass flow rate of water in the radiant system, in kg/s
  REAL(r64),    INTENT(IN) :: FlowFraction   ! Mass flow rate fraction for this surface in the radiant system
  REAL(r64),    INTENT(IN) :: NumCircs       ! Number of fluid circuits in this surface
  REAL(r64),    INTENT(IN) :: TubeLength     ! Length of tubing in the radiant system, in m
  REAL(r64),    INTENT(IN) :: TubeDiameter   ! Inside diameter of the tubing in the radiant system, in m
  INTEGER, INTENT(INOUT) :: GlycolIndex ! Index for the fluid used in this radiant system

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),    PARAMETER :: MaxLaminarRe = 2300.d0    ! Maximum Reynolds number for laminar flow
  INTEGER, PARAMETER :: NumOfPropDivisions = 13
  REAL(r64),    PARAMETER :: MaxExpPower = 50.d0       ! Maximum power after which EXP argument would be zero for DP variables
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Temps=  &   ! Temperature, in C
                   (/1.85d0,6.85d0,11.85d0,16.85d0,21.85d0,26.85d0,31.85d0,36.85d0,41.85d0,46.85d0,  &
                    51.85d0,56.85d0,61.85d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Mu=  &      ! Viscosity, in Ns/m2
                   (/.001652d0,.001422d0,.001225d0,.00108d0,.000959d0,.000855d0,.000769d0,.000695d0,.000631d0,  &
                     .000577d0,.000528d0,.000489d0,.000453d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Conductivity=  &     ! Conductivity, in W/mK
                   (/.574d0,.582d0,.590d0,.598d0,.606d0,.613d0,.620d0,.628d0,.634d0,.640d0,.645d0,.650d0,.656d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Pr=  &      ! Prandtl number (dimensionless)
                   (/12.22d0,10.26d0,8.81d0,7.56d0,6.62d0,5.83d0,5.20d0,4.62d0,4.16d0,3.77d0,3.42d0,3.15d0,2.88d0/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Index
  REAL(r64) :: InterpFrac
  REAL(r64) :: NuD
  REAL(r64) :: ReD
  REAL(r64) :: NTU
  REAL(r64) :: CpWater
  REAL(r64) :: Kactual
  REAL(r64) :: MUactual
  REAL(r64) :: PRactual
  REAL(r64) :: Eff          ! HX effectiveness


          ! FLOW:
          ! First find out where we are in the range of temperatures
  Index = 1
  DO WHILE (Index <= NumOfPropDivisions)
    IF (Temperature < Temps(Index)) EXIT ! DO loop
    Index = Index + 1
  END DO

          ! Initialize thermal properties of water
  IF (Index == 1) THEN
    MUactual = Mu(Index)
    Kactual  = Conductivity(Index)
    PRactual = Pr(Index)
  ELSE IF (Index > NumOfPropDivisions) THEN
    Index    = NumOfPropDivisions
    MUactual = Mu(Index)
    Kactual  = Conductivity(Index)
    PRactual = Pr(Index)
  ELSE
    InterpFrac = (Temperature-Temps(Index-1))/(Temps(Index)-Temps(Index-1))
    MUactual   = Mu(Index-1) + InterpFrac*(Mu(Index)-Mu(Index-1))
    Kactual    = Conductivity(Index-1) + InterpFrac*(Conductivity(Index)-Conductivity(Index-1))
    PRactual   = Pr(Index-1) + InterpFrac*(Pr(Index)-Pr(Index-1))
  END IF
      ! arguments are glycol name, temperature, and concentration
  SELECT CASE( SystemType )
  CASE (HydronicSystem)
    SELECT CASE (OperatingMode)
    CASE (HeatingMode)
      CpWater  = GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidName, &
                             60.d0, &
                             PlantLoop(HydrRadSys(RadSysNum)%HWLoopNum)%FluidIndex, &
                             'CalcRadSysHXEffectTerm')
    CASE (CoolingMode)
      CpWater  = GetSpecificHeatGlycol(PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidName, &
                             Temperature, &
                             PlantLoop(HydrRadSys(RadSysNum)%CWLoopNum)%FluidIndex, &
                             'CalcRadSysHXEffectTerm')
    END SELECT
  CASE (ConstantFlowSystem)
    SELECT CASE (OperatingMode)

    CASE (HeatingMode)
      CpWater    = GetSpecificHeatGlycol(PlantLoop(CFloRadSys(RadSysNum)%HWLoopNum)%FluidName, &
                             Temperature, &
                             PlantLoop(CFloRadSys(RadSysNum)%HWLoopNum)%FluidIndex, &
                             'CalcRadSysHXEffectTerm')
    CASE (CoolingMode)
      CpWater    = GetSpecificHeatGlycol(PlantLoop(CFloRadSys(RadSysNum)%CWLoopNum)%FluidName, &
                             InitConvTemp, &
                             PlantLoop(CFloRadSys(RadSysNum)%CWLoopNum)%FluidIndex, &
                             'CalcRadSysHXEffectTerm')
    END SELECT
  END SELECT

          ! Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
  ReD = 4.0d0 * WaterMassFlow * FlowFraction / ( PI * MUactual * TubeDiameter * NumCircs )

          ! Calculate the Nusselt number based on what flow regime one is in
  IF (ReD >= MaxLaminarRe) THEN ! Turbulent flow --> use Colburn equation

    NuD = 0.023d0*(ReD**(0.8d0))*(PRactual**(1.d0/3.d0))

  ELSE    ! Laminar flow --> use constant surface temperature relation

    NuD = 3.66d0

  END IF

          ! Calculate the NTU parameter
          ! NTU = UA/[(Mdot*Cp)min]
          ! where: U = h (convection coefficient) and h = (k)(Nu)/D
          !        A = Pi*D*TubeLength
  NTU = PI * Kactual * NuD * TubeLength / (WaterMassFlow * CpWater)   ! FlowFraction cancels out here

          ! Calculate Epsilon*MassFlowRate*Cp
  IF (NTU > MaxExpPower) THEN
    Eff = 1.0d0
    CalcRadSysHXEffectTerm = FlowFraction*WaterMassFlow*CpWater
  ELSE
    Eff = (1.0d0 - EXP(-NTU))
    CalcRadSysHXEffectTerm = (1.0d0-EXP(-NTU))*FlowFraction*WaterMassFlow*CpWater
  END IF

  RETURN

END FUNCTION CalcRadSysHXEffectTerm


SUBROUTINE UpdateRadSysSourceValAvg(LowTempRadSysOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To transfer the average value of the heat source/sink over the entire
          ! zone time step back to the heat balance routines so that the heat
          ! balance algorithms can simulate one last time with the average source
          ! to maintain some reasonable amount of continuity and energy balance
          ! in the temperature and flux histories.

          ! METHODOLOGY EMPLOYED:
          ! All of the record keeping for the average term is done in the Update
          ! routine so the only other thing that this subroutine does is check to
          ! see if the system was even on.  If any average term is non-zero, then
          ! one or more of the radiant systems was running.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(OUT) :: LowTempRadSysOn ! .TRUE. if the radiant system has run this zone time step

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: CloseEnough = 0.01d0 ! Some arbitrarily small value to avoid zeros and numbers that are almost the same

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SurfNum    ! DO loop counter for surface index

          ! FLOW:
  LowTempRadSysOn = .FALSE.

          ! If this was never allocated, then there are no radiant systems in this input file (just RETURN)
  IF (.NOT.ALLOCATED(QRadSysSrcAvg)) RETURN

          ! If it was allocated, then we have to check to see if this was running at all...
  DO SurfNum = 1, TotSurfaces
    IF (QRadSysSrcAvg(SurfNum) /= 0.0D0) THEN
      LowTempRadSysOn = .TRUE.
      EXIT !DO loop
    END IF
  END DO

  QRadSysSource = QRadSysSrcAvg

          ! For interzone surfaces, QRadSysSrcAvg was only updated for the "active" side.  The active side
          ! would have a non-zero value at this point.  If the numbers differ, then we have to manually update.
  DO SurfNum = 1, TotSurfaces
    IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) THEN
      IF ( ABS(QRadSysSource(SurfNum)-QRadSysSource(Surface(SurfNum)%ExtBoundCond)) > CloseEnough ) THEN  ! numbers differ
        IF ( ABS(QRadSysSource(SurfNum)) > ABS(QRadSysSource(Surface(SurfNum)%ExtBoundCond)) ) THEN
          QRadSysSource(Surface(SurfNum)%ExtBoundCond) = QRadSysSource(SurfNum)
        ELSE
          QRadSysSource(SurfNum) = QRadSysSource(Surface(SurfNum)%ExtBoundCond)
        END IF
      END IF
    END IF
  END DO

  RETURN

END SUBROUTINE UpdateRadSysSourceValAvg


REAL(r64) FUNCTION SumHATsurf(ZoneNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
          ! The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
          ! and should be updated accordingly.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces
  USE DataHeatBalance
  USE DataHeatBalSurface

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum     ! Zone number

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: SurfNum     ! Surface number
  REAL(r64)           :: Area        ! Effective surface area

          ! FLOW:
  SumHATsurf = 0.0d0

  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

    Area = Surface(SurfNum)%Area

    IF (Surface(SurfNum)%Class == SurfaceClass_Window) THEN
      IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn .OR. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) THEN
        ! The area is the shade or blind are = sum of the glazing area and the divider area (which is zero if no divider)
        Area = Area + SurfaceWindow(SurfNum)%DividerArea
      END IF

      IF (SurfaceWindow(SurfNum)%FrameArea > 0.0d0) THEN
        ! Window frame contribution
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%FrameArea &
          * (1.0d0 + SurfaceWindow(SurfNum)%ProjCorrFrIn) * SurfaceWindow(SurfNum)%FrameTempSurfIn
      END IF

      IF (SurfaceWindow(SurfNum)%DividerArea > 0.0d0 .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntShadeOn &
          .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntBlindOn) THEN
        ! Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%DividerArea &
          * (1.0d0 + 2.0d0 * SurfaceWindow(SurfNum)%ProjCorrDivIn) * SurfaceWindow(SurfNum)%DividerTempSurfIn
      END IF
    END IF

    SumHATsurf = SumHATsurf + HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum)
  END DO

  RETURN

END FUNCTION SumHATsurf


SUBROUTINE ReportLowTempRadiantSystem(RadSysNum,SystemType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simply produces output for the low temperature radiant system.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : SecInHour
  USE DataHeatBalance, ONLY : Zone
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE DataLoopNode,    ONLY : Node
  USE DataSurfaces,    ONLY : Surface
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: RadSysNum  ! Index for the low temperature radiant system under consideration within the derived types
  INTEGER, INTENT(IN) :: SystemType ! Type of radiant system: hydronic, constant flow, or electric

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CpFluid            ! Specific heat of the fluid in the radiant system
  INTEGER :: RadSurfNum         ! DO loop counter for radiant surfaces in the system
  INTEGER :: SurfNum            ! Surface number (index) in Surface derived type
  REAL(r64) :: TotalRadSysPower   ! Total source/sink power for the radiant system (sum of all surfaces of the system)
  REAL(r64) :: ZoneMult           ! Total zone multiplier to apply to the system level variables

          ! FLOW:
  TotalRadSysPower = 0.0d0
  ZoneMult         = 1.0d0

  SELECT CASE (SystemType)

    CASE (HydronicSystem)

      DO RadSurfNum = 1, HydrRadSys(RadSysNum)%NumOfSurfaces
        SurfNum          = HydrRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
        TotalRadSysPower = TotalRadSysPower + QRadSysSource(SurfNum)
      END DO
      ZoneMult         = REAL(Zone(HydrRadSys(RadSysNum)%ZonePtr)%Multiplier *   &
                                 Zone(HydrRadSys(RadSysNum)%ZonePtr)%ListMultiplier,r64)
      TotalRadSysPower = ZoneMult * TotalRadSysPower

      HydrRadSys(RadSysNum)%HeatPower = 0.0d0
      HydrRadSys(RadSysNum)%CoolPower = 0.0d0

      SELECT CASE (OperatingMode)

        CASE (HeatingMode)
          HydrRadSys(RadSysNum)%WaterInletTemp    = Node(HydrRadSys(RadSysNum)%HotWaterInNode)%Temp
          HydrRadSys(RadSysNum)%WaterOutletTemp   = Node(HydrRadSys(RadSysNum)%HotWaterOutNode)%Temp
          HydrRadSys(RadSysNum)%WaterMassFlowRate = Node(HydrRadSys(RadSysNum)%HotWaterInNode)%MassFlowRate
          HydrRadSys(RadSysNum)%HeatPower         = TotalRadSysPower

        CASE (CoolingMode)
          HydrRadSys(RadSysNum)%WaterInletTemp    = Node(HydrRadSys(RadSysNum)%ColdWaterInNode)%Temp
          HydrRadSys(RadSysNum)%WaterOutletTemp   = Node(HydrRadSys(RadSysNum)%ColdWaterOutNode)%Temp
          HydrRadSys(RadSysNum)%WaterMassFlowRate = Node(HydrRadSys(RadSysNum)%ColdWaterInNode)%MassFlowRate
          HydrRadSys(RadSysNum)%CoolPower         = -TotalRadSysPower

        CASE DEFAULT ! Not Operating: Leave temperatures at previous values
          HydrRadSys(RadSysNum)%WaterMassFlowRate = 0.0d0
          HydrRadSys(RadSysNum)%WaterOutletTemp   = HydrRadSys(RadSysNum)%WaterInletTemp

      END SELECT

      HydrRadSys(RadSysNum)%HeatEnergy = HydrRadSys(RadSysNum)%HeatPower*TimeStepSys*SecInHour
      HydrRadSys(RadSysNum)%CoolEnergy = HydrRadSys(RadSysNum)%CoolPower*TimeStepSys*SecInHour

      IF (HydrRadSys(RadSysNum)%CondCausedShutDown) THEN
        HydrRadSys(RadSysNum)%CondCausedTimeOff = TimeStepSys*SecInHour
      ELSE
        HydrRadSys(RadSysNum)%CondCausedTimeOff = 0.0d0
      END IF

    CASE (ConstantFlowSystem)

      DO RadSurfNum = 1, CFloRadSys(RadSysNum)%NumOfSurfaces
        SurfNum          = CFloRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
        TotalRadSysPower = TotalRadSysPower + QRadSysSource(SurfNum)
      END DO
      ZoneMult         = REAL(Zone(CFloRadSys(RadSysNum)%ZonePtr)%Multiplier *   &
                                 Zone(CFloRadSys(RadSysNum)%ZonePtr)%ListMultiplier,r64)
      TotalRadSysPower = ZoneMult * TotalRadSysPower

      CFloRadSys(RadSysNum)%HeatPower = 0.0d0
      CFloRadSys(RadSysNum)%CoolPower = 0.0d0

      SELECT CASE (OperatingMode)
          ! Note that temperatures have already been set as part of the simulation
          ! step.  So, they do not need to be calculated here except for the pump
          ! inlet temperature which was not calculated elsewhere.  If the system is
          ! not operating, leave the temperatures with their previous values but
          ! zero out the flow and power quantities (should have already been done
          ! in another routine, but just in case...).

        CASE (HeatingMode)
          CpFluid = GetSpecificHeatGlycol(PlantLoop(CFloRadSys(RadSysNum)%HWLoopNum)%FluidName,&
                                          Node(CFloRadSys(RadSysNum)%HotWaterInNode)%Temp, &
                                          PlantLoop(CFloRadSys(RadSysNum)%HWLoopNum)%FluidIndex,&
                                          'ReportLowTempRadiantSystem')

          CFloRadSys(RadSysNum)%HeatPower     = TotalRadSysPower
          IF (CFloRadSys(RadSysNum)%PumpMassFlowRate > 0.D0) THEN
            CFloRadSys(RadSysNum)%PumpInletTemp = CFloRadSys(RadSysNum)%WaterInletTemp &
                                               -( CFloRadSys(RadSysNum)%PumpHeattoFluid &
                                                 /(CFloRadSys(RadSysNum)%PumpMassFlowRate*CpFluid) )
          ELSE
            CFloRadSys(RadSysNum)%PumpInletTemp = CFloRadSys(RadSysNum)%WaterInletTemp
          ENDIF

        CASE (CoolingMode)
          CpFluid = GetSpecificHeatGlycol(PlantLoop(CFloRadSys(RadSysNum)%CWLoopNum)%FluidName, &
                                          Node(CFloRadSys(RadSysNum)%ColdWaterInNode)%Temp, &
                                          PlantLoop(CFloRadSys(RadSysNum)%CWLoopNum)%FluidIndex,&
                                          'ReportLowTempRadiantSystem')

          CFloRadSys(RadSysNum)%CoolPower     = -TotalRadSysPower
          CFloRadSys(RadSysNum)%PumpInletTemp = CFloRadSys(RadSysNum)%WaterInletTemp &
                                               -( CFloRadSys(RadSysNum)%PumpHeattoFluid &
                                                 /(CFloRadSys(RadSysNum)%PumpMassFlowRate*CpFluid) )

        CASE DEFAULT ! Not Operating
          CFloRadSys(RadSysNum)%WaterOutletTemp    = CFloRadSys(RadSysNum)%WaterInletTemp
          CFloRadSys(RadSysNum)%PumpInletTemp      = CFloRadSys(RadSysNum)%WaterInletTemp
          CFloRadSys(RadSysNum)%WaterMassFlowRate  = 0.0d0
          CFloRadSys(RadSysNum)%WaterInjectionRate = 0.0d0
          CFloRadSys(RadSysNum)%WaterRecircRate    = 0.0d0
          CFloRadSys(RadSysNum)%HeatPower          = 0.0d0
          CFloRadSys(RadSysNum)%CoolPower          = 0.0d0
          CFloRadSys(RadSysNum)%PumpPower          = 0.0d0
          CFloRadSys(RadSysNum)%PumpMassFlowRate   = 0.0d0
          CFloRadSys(RadSysNum)%PumpHeattoFluid    = 0.0d0

      END SELECT

      CFloRadSys(RadSysNum)%HeatEnergy            = CFloRadSys(RadSysNum)%HeatPower*TimeStepSys*SecInHour
      CFloRadSys(RadSysNum)%CoolEnergy            = CFloRadSys(RadSysNum)%CoolPower*TimeStepSys*SecInHour
      CFloRadSys(RadSysNum)%PumpEnergy            = CFloRadSys(RadSysNum)%PumpPower*TimeStepSys*SecInHour
      CFloRadSys(RadSysNum)%PumpHeattoFluidEnergy = CFloRadSys(RadSysNum)%PumpHeattoFluid*TimeStepSys*SecInHour

      IF (CFloRadSys(RadSysNum)%CondCausedShutDown) THEN
        CFloRadSys(RadSysNum)%CondCausedTimeOff = TimeStepSys*SecInHour
      ELSE
        CFloRadSys(RadSysNum)%CondCausedTimeOff = 0.0d0
      END IF

    CASE (ElectricSystem)

      DO RadSurfNum = 1, ElecRadSys(RadSysNum)%NumOfSurfaces
        SurfNum          = ElecRadSys(RadSysNum)%SurfacePtr(RadSurfNum)
        TotalRadSysPower = TotalRadSysPower + QRadSysSource(SurfNum)
      END DO
      ZoneMult         = REAL(Zone(ElecRadSys(RadSysNum)%ZonePtr)%Multiplier *   &
                                       Zone(ElecRadSys(RadSysNum)%ZonePtr)%ListMultiplier,r64)
      TotalRadSysPower = ZoneMult * TotalRadSysPower

      ElecRadSys(RadSysNum)%ElecPower  = TotalRadSysPower
      ElecRadSys(RadSysNum)%ElecEnergy = ElecRadSys(RadSysNum)%ElecPower*TimeStepSys*SecInHour
      ElecRadSys(RadSysNum)%HeatPower  = ElecRadSys(RadSysNum)%ElecPower
      ElecRadSys(RadSysNum)%HeatEnergy = ElecRadSys(RadSysNum)%ElecEnergy

  END SELECT

  RETURN

END SUBROUTINE ReportLowTempRadiantSystem

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

END MODULE LowTempRadiantSystem

