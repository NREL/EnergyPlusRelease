MODULE VentilatedSlab

  ! Module containing the routines dealing with the Ventilated Slab

  ! MODULE INFORMATION:
  !       AUTHOR         Young Tae Chae, Rick Strand
  !       DATE WRITTEN   June 2008
  !       MODIFIED
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Simulate Ventilated Slab Systems.

  ! METHODOLOGY EMPLOYED:
  ! Systems are modeled as a collection of components: radiant panel, outside air mixer,
  ! fan, heating coil and/or cooling coil plus an integrated control
  ! algorithm that adjusts the hot or cold water flow to meet the setpoint
  ! condition.  Outside air mixing is handled locally as either fixed percent
  ! or as attempting to meet a prescribed mixed air temperature.

  ! REFERENCES:
  ! ASHRAE Systems and Equipment Handbook (SI), 1996. pp. 31.1-31.3
  ! Fred Buhl's fan coil module (FanCoilUnits.f90)

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, BeginTimeStepFlag, MaxNameLength, &
                           InitConvTemp, SysSizingCalc, WarmUpFlag, DisplayExtraWarnings
USE DataInterfaces

USE DataSurfaces,      ONLY : Surface, TotSurfaces
USE DataHeatBalFanSys, ONLY : QRadSysSource
USE DataHVACGlobals,   ONLY: FanElecPower, SmallAirVolFlow, ContFanCycCoil

  ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE Psychrometrics
Use FluidProperties
IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS

  ! Module Object
  CHARACTER(len=*), PARAMETER :: cMO_VentilatedSlab='ZoneHVAC:VentilatedSlab'

  ! Parameters for outside air control types:
  INTEGER, PARAMETER :: Heating_ElectricCoilType     = 1
  INTEGER, PARAMETER :: Heating_GasCoilType          = 2
  INTEGER, PARAMETER :: Heating_WaterCoilType        = 3
  INTEGER, PARAMETER :: Heating_SteamCoilType        = 4
  INTEGER, PARAMETER :: Cooling_CoilWaterCooling     = 1
  INTEGER, PARAMETER :: Cooling_CoilDetailedCooling  = 2
  INTEGER, PARAMETER :: Cooling_CoilHXAssisted       = 3
  INTEGER, PARAMETER :: VariablePercent  = 1
  INTEGER, PARAMETER :: FixedTemperature = 2
  INTEGER, PARAMETER :: FixedOAControl   = 3
  INTEGER, PARAMETER :: NotOperating = 0  ! Parameter for use with OperatingMode variable, set for no heating/cooling
  INTEGER, PARAMETER :: HeatingMode = 1   ! Parameter for use with OperatingMode variable, set for heating
  INTEGER, PARAMETER :: CoolingMode = 2   ! Parameter for use with OperatingMode variable, set for cooling
!Ventilated Slab Configurations
  INTEGER, PARAMETER :: SlabOnly     = 1 ! Air circulate through cores of slab only
  INTEGER, PARAMETER :: SlabAndZone = 2 ! Circulated Air is introduced to zone
  INTEGER, PARAMETER :: SeriesSlabs     = 3 !
!  Control Types
  INTEGER, PARAMETER :: MATControl       = 1  ! Controls system using mean air temperature
  INTEGER, PARAMETER :: MRTControl       = 2  ! Controls system using mean radiant temperature
  INTEGER, PARAMETER :: OPTControl       = 3  ! Controls system using operative temperature
  INTEGER, PARAMETER :: ODBControl       = 4  ! Controls system using outside air dry-bulb temperature
  INTEGER, PARAMETER :: OWBControl       = 5  ! Controls system using outside air wet-bulb temperature
  INTEGER, PARAMETER :: SURControl       = 6  ! Controls system using surface temperature !Phase2-A
  INTEGER, PARAMETER :: DPTZControl      = 7  ! Controls system using dew-point temperature of zone!Phase2-A

 ! coil operation
  INTEGER, PARAMETER :: On =  1              ! normal coil operation
  INTEGER, PARAMETER :: Off = 0              ! signal coil shouldn't run
  INTEGER, PARAMETER :: NoneOption             = 0
  INTEGER, PARAMETER :: BothOption             = 1
  INTEGER, PARAMETER :: HeatingOption          = 2
  INTEGER, PARAMETER :: CoolingOption          = 3
  INTEGER :: OperatingMode          =0 ! Used to keep track of whether system is in heating or cooling mode


  ! DERIVED TYPE DEFINITIONS
TYPE, PUBLIC :: VentilatedSlabData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name                =' ' ! name of system
  CHARACTER(len=MaxNameLength) :: SchedName           =' ' ! availability schedule
  INTEGER                      :: SchedPtr            =0   ! index to schedule
  CHARACTER(len=MaxNameLength) :: ZoneName            = ' '  ! Name of zone the system is serving
  INTEGER                      :: ZonePtr             = 0    ! Point to this zone in the Zone derived type

   ! Variables for Delivery Config.
  CHARACTER(len=MaxNameLength), &
     ALLOCATABLE, DIMENSION(:) :: ZName                      ! Name of zone the system is serving
  INTEGER, ALLOCATABLE,  &
                  DIMENSION(:) :: ZPtr                       ! Point to this zone in the Zone derived type
  CHARACTER(len=MaxNameLength) :: SurfListName        = ' '  ! Name of surface/surface list that is the radiant system
  INTEGER                      :: NumOfSurfaces       = 0    ! Number of surfaces included in this system (coordinated control)
  INTEGER, ALLOCATABLE,  &
                  DIMENSION(:) :: SurfacePtr                  ! Pointer to the slabs in the Surface derived type
  CHARACTER(len=MaxNameLength), &
     ALLOCATABLE, DIMENSION(:) :: SurfaceName                 ! Name of surfaces that are the radiant system (can be one or more)
  REAL(r64), ALLOCATABLE,   &
                  DIMENSION(:) :: SurfaceFlowFrac             ! Fraction of flow/pipe length for a particular surface
  REAL(r64), ALLOCATABLE,  &
                  DIMENSION(:) :: CDiameter                   ! Number of core diameter

  REAL(r64), ALLOCATABLE,  &
                  DIMENSION(:) :: CLength                     ! Number of core length

  REAL(r64), ALLOCATABLE,  &
                  DIMENSION(:) :: CNumbers                    ! Number of core numbers

  CHARACTER(len=MaxNameLength), &
     ALLOCATABLE, DIMENSION(:) :: SlabIn              ! Name of node that is slab inlet node

 CHARACTER(len=MaxNameLength), &
     ALLOCATABLE, DIMENSION(:) :: SlabOut             ! Name of node that is slab outlet node




  REAL(r64)                    :: TotalSurfaceArea  = 0.0d0 ! Total surface area for all surfaces that are part of this system
  REAL(r64)                    :: CoreDiameter      = 0.0d0 ! tube diameter for embedded tubing
  REAL(r64)                    :: CoreLength        = 0.0d0 ! tube length embedded in radiant surface
  REAL(r64)                    :: CoreNumbers        = 0.0d0 ! tube length embedded in radiant surface
  INTEGER                      :: ControlType       = 0   ! Control type for the system
                                                          ! (MAT, MRT, Op temp, ODB, OWB, DPTZ, Surf Temp.)
  INTEGER                      :: ReturnAirNode           =0   ! inlet air node number
  INTEGER                      :: RadInNode          =0   ! outlet air node number
  INTEGER                      :: ZoneAirInNode          =0   ! outlet air node number
  INTEGER                      :: FanOutletNode       =0   ! outlet node number for fan exit
                                                           ! (assumes fan is upstream of heating coil)
  INTEGER                      :: MSlabInNode         =0   !
  INTEGER                      :: MSlabOutNode         =0   !

  CHARACTER(len=MaxNameLength) :: FanName             =' ' ! name of fan
  INTEGER                      :: Fan_Index           =0
  INTEGER                      :: ControlCompTypeNum =0
  INTEGER                      :: CompErrIndex       =0
  REAL(r64)                    :: MaxAirVolFlow       =0.0d0 ! m3/s
  REAL(r64)                    :: MaxAirMassFlow      =0.0d0 ! kg/s
  INTEGER                      :: OAControlType       =0 ! type of control; options are VARIABLE PERCENT and FIXED TEMPERATURE
  CHARACTER(len=MaxNameLength) :: MinOASchedName      =' ' ! schedule of fraction for minimum outside air (all controls)
  INTEGER                      :: MinOASchedPtr       =0   ! index to schedule
  CHARACTER(len=MaxNameLength) :: MaxOASchedName      =' ' ! schedule of percentages for maximum outside air fraction (variable %)
  INTEGER                      :: MaxOASchedPtr       =0   ! index to schedule
  CHARACTER(len=MaxNameLength) :: TempSchedName       =' ' ! schedule of temperatures for desired "mixed air"
                                                           ! temperature (fixed temp.)
  INTEGER                      :: TempSchedPtr        =0   ! index to schedule
  INTEGER                      :: OutsideAirNode      =0   ! outside air node number
  INTEGER                      :: AirReliefNode       =0   ! relief air node number
  INTEGER                      :: OAMixerOutNode      =0   ! outlet node after the outside air mixer (inlet to coils if present)
  REAL(r64)                    :: OutAirVolFlow       =0.0d0 ! m3/s
  REAL(r64)                    :: OutAirMassFlow      =0.0d0 ! kg/s
  REAL(r64)                    :: MinOutAirVolFlow    =0.0d0 ! m3/s
  REAL(r64)                    :: MinOutAirMassFlow   =0.0d0 ! kg/s
  INTEGER                      :: SysConfg            =0 ! type of coil option; options are BOTH, HEATING, COOLING, AND NONE
  INTEGER                      :: CoilOption          =0 ! type of coil option; options are BOTH, HEATING, COOLING, AND NONE
  LOGICAL                      :: HCoilPresent        =.false. ! .TRUE. if ventilated slab has a heating coil
  INTEGER                      :: HCoilType           =0 ! type of heating coil (water, gas, electric, etc.)
  CHARACTER(len=MaxNameLength) :: HCoilName           =' ' ! name of heating coil
  CHARACTER(len=MaxNameLength) :: HCoilTypeCh         =' ' ! type of heating coil (character string)
  INTEGER                      :: HCoil_Index         =0
  INTEGER                      :: HCoil_PlantTypeNum  =0  !
  INTEGER                      :: HCoil_FluidIndex    =0
  CHARACTER(len=MaxNameLength) :: HCoilSchedName      =' ' ! availability schedule for the heating coil
  INTEGER                      :: HCoilSchedPtr       =0   ! index to schedule
  REAL(r64)                    :: HCoilSchedValue     =0.0d0
  REAL(r64)                    :: MaxVolHotWaterFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MaxVolHotSteamFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MaxHotWaterFlow     =0.0d0 ! kg/s
  REAL(r64)                    :: MaxHotSteamFlow=0.0d0
  REAl(r64)                    :: MinHotSteamFlow =0.0d0
  REAL(r64)                    :: MinVolHotWaterFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MinVolHotSteamFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MinHotWaterFlow     =0.0d0 ! kg/s
  INTEGER                      :: HotControlNode      =0   ! hot water control node
  INTEGER                      :: HotCoilOutNodeNum   =0   ! outlet of coil
  REAL(r64)                    :: HotControlOffset    =0.0d0 ! control tolerance
  INTEGER                      :: HWLoopNum           =0   ! index for plant loop with hot water coil
  INTEGER                      :: HWLoopSide          =0   ! index for plant loop side for hot water coil
  INTEGER                      :: HWBranchNum         =0   ! index for plant branch for hot water coil
  INTEGER                      :: HWCompNum           =0   ! index for plant component for hot water coil
  CHARACTER(len=MaxNameLength) :: HotAirHiTempSched    = ' ' ! Schedule name for the highest Air temperature
  INTEGER                      :: HotAirHiTempSchedPtr = 0   ! Schedule index for the highest Air temperature
  CHARACTER(len=MaxNameLength) :: HotAirLoTempSched    = ' ' ! Schedule name for the lowest Air temperature
  INTEGER                      :: HotAirLoTempSchedPtr = 0   ! Schedule index for the lowest Air temperature
  CHARACTER(len=MaxNameLength) :: HotCtrlHiTempSched     = ' ' ! Schedule name for the highest control temperature
                                                               ! (where the lowest Air temperature is requested)
  INTEGER                      :: HotCtrlHiTempSchedPtr  = 0   ! Schedule index for the highest control temperature
                                                               ! (where the lowest Air temperature is requested)
  CHARACTER(len=MaxNameLength) :: HotCtrlLoTempSched     = ' ' ! Schedule name for the lowest control temperature
                                                               ! (where the highest Air temperature is requested)
  INTEGER                      :: HotCtrlLoTempSchedPtr  = 0   ! Schedule index for the lowest control temperature
                                                               ! (where the highest Air temperature is requested)
  LOGICAL                      :: CCoilPresent        =.false. ! .TRUE. if ventilated slab has a cooling coil
  CHARACTER(len=MaxNameLength) :: CCoilName           =' ' ! name of cooling coil
  CHARACTER(len=MaxNameLength) :: CCoilTypeCh         =' ' ! type of cooling coil (character string)
  INTEGER                      :: CCoil_Index         =0
  CHARACTER(len=MaxNameLength) :: CCoilPlantName =' '  ! name of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
  CHARACTER(len=MaxNameLength) :: CCoilPlantType =' '  ! type of cooling coil (child<=CoilSystem:Cooling:Water:HeatExchangerAssisted)
  INTEGER                      :: CCoil_PlantTypeNum  =0  !
  INTEGER                      :: CCoilType           =0   ! type of cooling coil:
                                                           ! 'Coil:Cooling:Water:DetailedGeometry' or
                                                           ! 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
  CHARACTER(len=MaxNameLength) :: CCoilSchedName      =' ' ! availability schedule for the cooling coil
  INTEGER                      :: CCoilSchedPtr       =0   ! index to schedule
  REAL(r64)                    :: CCoilSchedValue     =0.0d0
  REAL(r64)                    :: MaxVolColdWaterFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MaxColdWaterFlow    =0.0d0 ! kg/s
  REAL(r64)                    :: MinVolColdWaterFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MinColdWaterFlow    =0.0d0 ! kg/s
  INTEGER                      :: ColdControlNode     =0   ! chilled water control node
  INTEGER                      :: ColdCoilOutNodeNum  =0   ! chilled water coil out nod
  REAL(r64)                    :: ColdControlOffset   =0.0d0 ! control tolerance
  INTEGER                      :: CWLoopNum           =0   ! index for plant loop with chilled water coil
  INTEGER                      :: CWLoopSide          =0   ! index for plant loop side for chilled water coil
  INTEGER                      :: CWBranchNum         =0   ! index for plant branch for chilled water coil
  INTEGER                      :: CWCompNum           =0   ! index for plant component for chilled water coil
  CHARACTER(len=MaxNameLength) :: ColdAirHiTempSched    = ' ' ! Schedule name for the highest air temperature
  INTEGER                      :: ColdAirHiTempSchedPtr = 0   ! Schedule index for the highest Air temperature
  CHARACTER(len=MaxNameLength) :: ColdAirLoTempSched    = ' ' ! Schedule name for the lowest Air temperature
  INTEGER                      :: ColdAirLoTempSchedPtr = 0   ! Schedule index for the lowest Air temperature
  CHARACTER(len=MaxNameLength) :: ColdCtrlHiTempSched     = ' ' ! Schedule name for the highest control temperature
                                                               ! (where the lowest Air temperature is requested)
  INTEGER                      :: ColdCtrlHiTempSchedPtr  = 0   ! Schedule index for the highest control temperature
                                                               ! (where the lowest Air temperature is requested)
  CHARACTER(len=MaxNameLength) :: ColdCtrlLoTempSched     = ' ' ! Schedule name for the lowest control temperature
                                                               ! (where the highest Air temperature is requested)
  INTEGER                      :: ColdCtrlLoTempSchedPtr  = 0   ! Schedule index for the lowest control temperature
                                                               ! (where the highest Air temperature is requested)
  INTEGER                      :: CondErrIndex      = 0   ! Error index for recurring warning messages
  INTEGER                      :: EnrgyImbalErrIndex = 0  ! Error index for recurring warning messages
  INTEGER                      :: RadSurfNum        = 0   ! Radiant Surface Number
  INTEGER                      :: MSlabIn        = 0   ! Internal Slab Inlet Node Number
  INTEGER                      :: MSlabOut       = 0   ! INternal Slab Outlet Node Number
  CHARACTER(len=MaxNameLength) :: DSSlabInNodeName  =' '
  CHARACTER(len=MaxNameLength) :: DSSlabOutNodeName  =' '
  ! Report data
  REAL(r64)                    :: DirectHeatLossPower          =0.0d0 ! system direct heat loss in W
  REAL(r64)                    :: DirectHeatLossEnergy         =0.0d0 ! system direct heat loss in J
  REAL(r64)                    :: DirectHeatGainPower          =0.0d0 ! system direct heat gain in W
  REAL(r64)                    :: DirectHeatGainEnergy         =0.0d0 ! system direct heat gain in J
  REAL(r64)                    :: TotalVentSlabRadPower        =0.0d0
  REAL(r64)                    :: RadHeatingPower              =0.0d0 ! radiant heating output in watts
  REAL(r64)                    :: RadHeatingEnergy             =0.0d0 ! radiant heating output in J
  REAL(r64)                    :: RadCoolingPower              =0.0d0 ! radiant cooling output in watts
  REAL(r64)                    :: RadCoolingEnergy             =0.0d0 ! radiant cooling output in J
  REAL(r64)                    :: HeatCoilPower                =0.0d0
  REAL(r64)                    :: HeatCoilEnergy               =0.0d0
  REAL(r64)                    :: TotCoolCoilPower             =0.0d0
  REAL(r64)                    :: TotCoolCoilEnergy            =0.0d0
  REAL(r64)                    :: SensCoolCoilPower            =0.0d0
  REAL(r64)                    :: SensCoolCoilEnergy           =0.0d0
  REAL(r64)                    :: LateCoolCoilPower            =0.0d0
  REAL(r64)                    :: LateCoolCoilEnergy           =0.0d0
  REAL(r64)                    :: ElecFanPower                 =0.0d0
  REAL(r64)                    :: ElecFanEnergy                =0.0d0
  REAL(r64)                    :: AirMassFlowRate              =0.0d0 ! Circulated air mass flow rate in kg/s
  REAL(r64)                    :: AirVolFlow                   =0.0d0 ! Circulated air volumetric flow rate in m3/s
  REAL(r64)                    :: SlabInTemp                   =0.0d0 ! Slab inlet temp in degree C
  REAL(r64)                    :: SlabOutTemp                  =0.0d0 ! Slab outlet temp in degree C
  REAL(r64)                    :: ReturnAirTemp                =0.0d0 !
  REAL(r64)                    :: FanOutletTemp                =0.0d0 ! FanOutlet temp in degree C
  REAL(r64)                    :: ZoneInletTemp                =0.0d0 ! supply air temp
  CHARACTER(len=MaxNameLength) :: AvailManagerListName         = ' ' ! Name of an availability manager list object
  INTEGER                      :: AvailStatus                  = 0

END TYPE VentilatedSlabData

TYPE (VentilatedSlabData), ALLOCATABLE, PUBLIC, DIMENSION(:) :: VentSlab

  ! MODULE VARIABLE DECLARATIONS:
LOGICAL :: HCoilOn         =.false. ! TRUE if the heating coil (gas or electric especially) should be running
INTEGER, PUBLIC :: NumOfVentSlabs  =0       ! Number of ventilated slab in the input file
REAL(r64)    :: OAMassFlowRate  =0.0d0     ! Outside air mass flow rate for the ventilated slab
DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: QRadSysSrcAvg ! Average source over the time step for a particular radiant surfaceD
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZeroSourceSumHATsurf ! Equal to SumHATsurf for all the walls in a zone with no source
INTEGER :: MaxCloNumOfSurfaces    =0 ! Used to set allocate size in CalcClo routine
REAL(r64)    :: QZnReq          =0.0d0     ! heating or cooling needed by system [watts]

  ! Record keeping variables used to calculate QRadSysSrcAvg locally

DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: LastQRadSysSrc       ! Need to keep the last value in case we are still iterating
REAL(r64), ALLOCATABLE, DIMENSION(:)        :: LastSysTimeElapsed   ! Need to keep the last value in case we are still iterating
REAL(r64), ALLOCATABLE, DIMENSION(:)        :: LastTimeStepSys      ! Need to keep the last value in case we are still iterating
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! Autosizing variables
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag

  ! SUBROUTINE SPECIFICATIONS FOR MODULE VentilatedSlab
PUBLIC  SimVentilatedSlab
PRIVATE GetVentilatedSlabInput
PRIVATE InitVentilatedSlab
PRIVATE SizeVentilatedSlab
PRIVATE CalcVentilatedSlab
PUBLIC  CalcVentilatedSlabComps
PUBLIC  CalcVentilatedSlabRadComps
PRIVATE SimVentSlabOAMixer
PRIVATE UpdateVentilatedSlab
PRIVATE CalcVentSlabHXEffectTerm
! PRIVATE UpdateVentilatedSlabValAvg
PRIVATE SumHATsurf
PUBLIC  ReportVentilatedSlab
CONTAINS

SUBROUTINE SimVentilatedSlab(CompName,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED
          ! This is re-engineered by Rick Strand and Young T. Chae for Ventilated Slab (June, 2008)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the main driver subroutine for the Ventilated Slab simulation.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,      ONLY: FindItemInList
  USE General,             ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: CompName            ! name of the fan coil unit
  INTEGER,          INTENT(IN)  :: ZoneNum             ! number of zone being served
  LOGICAL,          INTENT(IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),        INTENT(OUT) :: PowerMet            ! Sensible power supplied (W)
  REAL(r64),        INTENT(OUT) :: LatOutputProvided   ! Latent add/removal supplied by window AC (kg/s), dehumid = negative
  INTEGER,          INTENT(INOUT) :: CompIndex



          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: Item            ! index of ventilated slab being simulated
  LOGICAL,SAVE :: GetInputFlag = .true.  ! First time, input is "gotten"

          ! FLOW:
  IF (GetInputFlag) THEN
    CALL GetVentilatedSlabInput
    GetInputFlag=.false.
  ENDIF

  ! Find the correct VentilatedSlabInput
  IF (CompIndex == 0) THEN
    Item = FindItemInList(CompName,VentSlab%Name,NumOfVentSlabs)
    IF (Item == 0) THEN
      CALL ShowFatalError('SimVentilatedSlab: system not found='//TRIM(CompName))
    ENDIF
    CompIndex=Item
  ELSE
    Item=CompIndex
    IF (Item > NumOfVentSlabs .or. Item < 1) THEN
      CALL ShowFatalError('SimVentilatedSlab:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(Item))// &
                          ', Number of Systems='//TRIM(TrimSigDigits(NumOfVentSlabs))//  &
                          ', Entered System name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(Item)) THEN
      IF (CompName /= VentSlab(Item)%Name) THEN
        CALL ShowFatalError('SimVentilatedSlab: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(Item))// &
                            ', System name='//TRIM(CompName)//', stored System Name for that index='//  &
                            TRIM(VentSlab(Item)%Name))
      ENDIF
      CheckEquipName(Item)=.false.
    ENDIF
  ENDIF

  CALL InitVentilatedSlab(Item,ZoneNum,FirstHVACIteration)

  CALL CalcVentilatedSlab(Item,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

  CALL UpdateVentilatedSlab(Item, FirstHVACIteration)

  CALL ReportVentilatedSlab(Item)

  RETURN

END SUBROUTINE SimVentilatedSlab

SUBROUTINE GetVentilatedSlabInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   June 2008
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine obtains the input for ventilated slab and sets
          ! up the appropriate derived type.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! Fred Buhl's fan coil module (FanCoilUnits.f90)
          ! Kwang Ho Lee's Unit Ventilator Module (UnitVentilator.f90)
          ! Rick Strand's Low temperature Radiant system (RadiantSystemLowTemp.f90)

          ! USE STATEMENTS:
  USE InputProcessor,           ONLY : GetNumObjectsFound, GetObjectItem, VerifyName, SameString, FindItemInList,   &
                                       GetObjectDefMaxArgs
  USE NodeInputManager,         ONLY : GetOnlySingleNode
  USE BranchNodeConnections,    ONLY : SetUpCompSets
  USE WaterCoils,               ONLY : GetWaterCoilMaxFlowRate=>GetCoilMaxWaterFlowRate
  Use SteamCoils,               ONLY : GetSteamCoilMaxFlowRate=>GetCoilMaxWaterFlowRate
  USE HVACHXAssistedCoolingCoil,ONLY : GetHXAssistedCoilFlowRate=>GetCoilMaxWaterFlowRate,GetHXCoilTypeAndName
  USE DataGlobals,              ONLY : NumOfZones, ScheduleAlwaysOn
  USE DataHeatBalance,          ONLY : Zone, Construct
  USE DataSizing,               ONLY : AutoSize
  USE DataZoneEquipment,        ONLY : VentilatedSlab_Num
  USE ScheduleManager,          ONLY : GetScheduleIndex
  USE DataLoopNode
  USE DataSurfaceLists
  USE OutAirNodeManager,        ONLY: CheckAndAddAirNodeNumber
  USE FluidProperties,          ONLY: FindRefrigerant
  USE DataPlant,                ONLY: TypeOf_CoilWaterCooling, TypeOf_CoilWaterDetailedFlatCooling, &
                                      TypeOf_CoilWaterSimpleHeating, TypeOf_CoilSteamAirHeating
  USE DataHVACGlobals,          ONLY: ZoneComp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '
  CHARACTER(len=*), PARAMETER :: MeanAirTemperature = 'MeanAirTemperature'
  CHARACTER(len=*), PARAMETER :: MeanRadiantTemperature = 'MeanRadiantTemperature'
  CHARACTER(len=*), PARAMETER :: OperativeTemperature = 'OperativeTemperature'
  CHARACTER(len=*), PARAMETER :: OutsideAirDryBulbTemperature = 'OutdoorDryBulbTemperature'
  CHARACTER(len=*), PARAMETER :: OutsideAirWetBulbTemperature = 'OutdoorWetBulbTemperature'
  CHARACTER(len=*), PARAMETER :: SlabSurfaceTemperature = 'SurfaceTemperature'
  CHARACTER(len=*), PARAMETER :: SlabSurfaceDewPointTemperature = 'ZoneAirDewPointTemperature'
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='ZoneHVAC:VentilatedSlab'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: ErrorsFound=.false. ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: IOStatus            ! Used in GetObjectItem
  LOGICAL                        :: IsBlank             ! TRUE if the name is blank
  LOGICAL                        :: IsNotOk             ! TRUE if there was a problem with a list name
  INTEGER                         :: NumAlphas          ! Number of Alphas for each GetObjectItem call
  INTEGER                         :: NumArgs            ! Unused variable that is part of a subroutine call
  INTEGER                         :: NumNumbers         ! Number of Numbers for each GetObjectItem call
  INTEGER                         :: Item               ! Item to be "gotten"
  INTEGER                         :: BaseNum            ! Temporary number for creating RadiantSystemTypes structure
  LOGICAL                         :: errflag            ! interim error flag
  INTEGER                         :: SurfListNum        ! Index within the SurfList derived type for a surface list name
!unused0309  INTEGER                         :: NumOfSurfListVB  ! Number of surface lists in the user input file
  INTEGER                         :: SurfNum            ! DO loop counter for surfaces
  LOGICAL                         :: IsValid            ! Set for outside air node check
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaArgs      ! Alpha input items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: rNumericArgs          ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  LOGICAL                              :: SteamMessageNeeded

          ! FLOW:
          ! Figure out how many Ventilated Slab Systems there are in the input file

   SteamMessageNeeded=.true.
   CALL GetObjectDefMaxArgs(CurrentModuleObject,NumArgs,NumAlphas,NumNumbers)
   ALLOCATE(cAlphaArgs(NumAlphas))
   cAlphaArgs=' '
   ALLOCATE(cAlphaFields(NumAlphas))
   cAlphaFields=' '
   ALLOCATE(cNumericFields(NumNumbers))
   cNumericFields=' '
   ALLOCATE(rNumericArgs(NumNumbers))
   rNumericArgs=0.0d0
   ALLOCATE(lAlphaBlanks(NumAlphas))
   lAlphaBlanks=.true.
   ALLOCATE(lNumericBlanks(NumNumbers))
   lNumericBlanks=.true.

! make sure data is gotten for surface lists
  BaseNum=GetNumberOfSurfListVentSlab()

  NumOfVentSlabs=GetNumObjectsFound(CurrentModuleObject)
          ! Allocate the local derived type and do one-time initializations for all parts of it

  ALLOCATE(VentSlab(NumOfVentSlabs))
  ALLOCATE(CheckEquipName(NumOfVentSlabs))
  CheckEquipName=.true.

  DO Item = 1, NumOfVentSlabs    ! Begin looping over the entire ventilated slab systems found in the input file...

    CALL GetObjectItem(CurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(cAlphaArgs(1),VentSlab%Name, Item-1,IsNotOK,IsBlank,trim(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    END IF

    VentSlab(Item)%Name      = cAlphaArgs(1)
    VentSlab(Item)%SchedName = cAlphaArgs(2)
    IF (lAlphaBlanks(2)) THEN
      VentSlab(Item)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      VentSlab(Item)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))  ! convert schedule name to pointer
      IF (VentSlab(Item)%SchedPtr== 0) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    VentSlab(Item)%ZoneName = cAlphaArgs(3)
    VentSlab(Item)%ZonePtr  = FindIteminList(cAlphaArgs(3),Zone%Name,NumOfZones)
    IF (VentSlab(Item)%ZonePtr == 0) THEN
      IF (lAlphaBlanks(3)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(3))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(3))//'="'//trim(cAlphaArgs(3))//'" not found.')
      END IF
      ErrorsFound=.true.
    END IF

    VentSlab(Item)%SurfListName = cAlphaArgs(4)
    SurfListNum = 0
!    IF (NumOfSlabLists > 0) SurfListNum = FindItemInList(VentSlab(Item)%SurfListName, SlabList%Name, NumOfSlabLists)
    IF (NumOfSurfListVentSlab > 0) SurfListNum = FindItemInList(VentSlab(Item)%SurfListName,SlabList%Name,NumOfSurfListVentSlab)
    IF (SurfListNum > 0) THEN   ! Found a valid surface list
      VentSlab(Item)%NumOfSurfaces = SlabList(SurfListNum)%NumOfSurfaces
      ALLOCATE (VentSlab(Item)%ZName(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%ZPtr(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%SurfaceName(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%SurfacePtr(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%CDiameter(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%CLength(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%CNumbers(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%SlabIn(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%SlabOut(VentSlab(Item)%NumOfSurfaces))

      MaxCloNumOfSurfaces=Max(MaxCloNumOfSurfaces,VentSlab(Item)%NumOfSurfaces)
      DO SurfNum = 1, SlabList(SurfListNum)%NumOfSurfaces
        VentSlab(Item)%ZName(SurfNum)           = SlabList(SurfListNum)%ZoneName(SurfNum)
        VentSlab(Item)%ZPtr(SurfNum)            = SlabList(SurfListNum)%ZonePtr(SurfNum)
        VentSlab(Item)%SurfaceName(SurfNum)     = SlabList(SurfListNum)%SurfName(SurfNum)
        VentSlab(Item)%SurfacePtr(SurfNum)      = SlabList(SurfListNum)%SurfPtr(SurfNum)
        VentSlab(Item)%CDiameter(SurfNum)       = SlabList(SurfListNum)%CoreDiameter(SurfNum)
        VentSlab(Item)%CLength(SurfNum)         = SlabList(SurfListNum)%CoreLength(SurfNum)
        VentSlab(Item)%CNumbers(SurfNum)        = SlabList(SurfListNum)%CoreNumbers(SurfNum)
        VentSlab(Item)%SlabIn(SurfNum)          = SlabList(SurfListNum)%SlabInNodeName(SurfNum)
        VentSlab(Item)%SlabOut(SurfNum)         = SlabList(SurfListNum)%SlabOutNodeName(SurfNum)
        IF (VentSlab(Item)%SurfacePtr(SurfNum) /= 0) THEN
          Surface(VentSlab(Item)%SurfacePtr(SurfNum))%IntConvSurfHasActiveInIt = .TRUE.
        ENDIF
      END DO

    ELSE    ! User entered a single surface name rather than a surface list
      VentSlab(Item)%NumOfSurfaces = 1
      ALLOCATE (VentSlab(Item)%SurfacePtr(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%SurfaceName(VentSlab(Item)%NumOfSurfaces))
      ALLOCATE (VentSlab(Item)%SurfaceFlowFrac(VentSlab(Item)%NumOfSurfaces))
      MaxCloNumOfSurfaces=Max(MaxCloNumOfSurfaces,VentSlab(Item)%NumOfSurfaces)
      VentSlab(Item)%SurfaceName(1)     = VentSlab(Item)%SurfListName
      VentSlab(Item)%SurfacePtr(1)      = FindIteminList(VentSlab(Item)%SurfaceName(1),Surface%Name,TotSurfaces)
      VentSlab(Item)%SurfaceFlowFrac(1) = 1.0d0
          ! Error checking for single surfaces
      IF (VentSlab(Item)%SurfacePtr(1) == 0) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(4))//'="'//trim(cAlphaArgs(4))//'" not found.')
        ErrorsFound=.true.
      ELSEIF (Surface(VentSlab(Item)%SurfacePtr(1))%PartOfVentSlabOrRadiantSurface) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid Surface')
        CALL ShowContinueError(trim(cAlphaFields(4))//'="'//TRIM(cAlphaArgs(4))//  &
           '" has been used in another radiant system or ventilated slab.')
        ErrorsFound=.true.
      END IF
      IF (VentSlab(Item)%SurfacePtr(1) /= 0) THEN
        Surface(VentSlab(Item)%SurfacePtr(1))%IntConvSurfHasActiveInIt = .TRUE.
        Surface(VentSlab(Item)%SurfacePtr(1))%PartOfVentSlabOrRadiantSurface = .true.
      ENDIF
    END IF

          ! Error checking for zones and construction information

  IF (SurfListNum > 0) THEN

    DO SurfNum = 1, VentSlab(Item)%NumOfSurfaces

      IF (VentSlab(Item)%SurfacePtr(SurfNum) == 0) CYCLE ! invalid surface -- detected earlier
      IF (VentSlab(Item)%ZPtr(SurfNum) == 0) CYCLE ! invalid zone -- detected earlier
!      IF (Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Zone /= VentSlab(Item)%ZPtr(SurfNum)) THEN
!        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
!          'surface="'//TRIM(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Name)//'".')
!        CALL ShowContinueError('Surface in Zone='//TRIM(Zone(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Zone)%Name)//' '// &
!                         CurrentModuleObject//' in Zone='//TRIM(cAlphaArgs(3)))
!        ErrorsFound=.true.
!      END IF
      IF (Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Construction == 0) CYCLE ! invalid construction, detected earlier
      IF (.NOT. Construct(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Construction)%SourceSinkPresent) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
          'surface="'//TRIM(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Name)//'".')
        CALL ShowContinueError('Surface Construction does not have a source/sink, Construction name= "'// &
                 TRIM(Construct(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Construction)%Name)//'".')
        ErrorsFound=.true.
      END IF
    END DO
  ELSE
    DO SurfNum = 1, VentSlab(Item)%NumOfSurfaces
      IF (VentSlab(Item)%SurfacePtr(SurfNum) == 0) CYCLE ! invalid surface -- detected earlier
      IF (VentSlab(Item)%ZonePtr == 0) CYCLE ! invalid zone -- detected earlier
      IF (Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Zone /= VentSlab(Item)%ZonePtr) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
          'surface="'//TRIM(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Name)//'".')
        CALL ShowContinueError('Surface in Zone='//TRIM(Zone(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Zone)%Name)//' '// &
                         CurrentModuleObject//' in Zone='//TRIM(cAlphaArgs(3)))
        ErrorsFound=.true.
      END IF
      IF (Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Construction == 0) CYCLE ! invalid construction, detected earlier
      IF (.NOT. Construct(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Construction)%SourceSinkPresent) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
          'surface="'//TRIM(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Name)//'".')
        CALL ShowContinueError('Surface Construction does not have a source/sink, Construction name= "'// &
                 TRIM(Construct(Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Construction)%Name)//'".')
        ErrorsFound=.true.
      END IF
    END DO
  END IF

    VentSlab(Item)%MaxAirVolFlow     = rNumericArgs(1)

! Outside air information:
    VentSlab(Item)%MinOutAirVolFlow  = rNumericArgs(2)
    VentSlab(Item)%OutAirVolFlow     = rNumericArgs(3)

    SELECT CASE (cAlphaArgs(5))
      CASE ('VARIABLEPERCENT')
        VentSlab(Item)%OAControlType  = VariablePercent
        VentSlab(Item)%MaxOASchedName = cAlphaArgs(6)
        VentSlab(Item)%MaxOASchedPtr  = GetScheduleIndex(cAlphaArgs(7))  ! convert schedule name to pointer
        IF (VentSlab(Item)%MaxOASchedPtr == 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(7))//'="'//trim(cAlphaArgs(7))//'" not found.')
          ErrorsFound=.TRUE.
        ELSEIF (.not. CheckScheduleValueMinMax(VentSlab(Item)%MaxOASchedPtr,'>=0',0.0d0,'<=',1.0d0)) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(7))//'="'//trim(cAlphaArgs(7))//'" values out of range [0,1].')
          ErrorsFound=.TRUE.
        END IF
      CASE ('FIXEDAMOUNT')
        VentSlab(Item)%OAControlType  = FixedOAControl
        VentSlab(Item)%MaxOASchedName = cAlphaArgs(7)
        VentSlab(Item)%MaxOASchedPtr  = GetScheduleIndex(cAlphaArgs(7))  ! convert schedule name to pointer
        IF (VentSlab(Item)%MaxOASchedPtr == 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(7))//'="'//trim(cAlphaArgs(7))//'" not found.')
          ErrorsFound=.TRUE.
        ELSEIF (.not. CheckScheduleValueMinMax(VentSlab(Item)%MaxOASchedPtr,'>=0',0.0d0)) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(7))//'="'//trim(cAlphaArgs(7))//'" values out of range (must be >=0).')
          ErrorsFound=.TRUE.
        END IF
      CASE ('FIXEDTEMPERATURE')
        VentSlab(Item)%OAControlType  = FixedTemperature
        VentSlab(Item)%TempSchedName = cAlphaArgs(7)
        VentSlab(Item)%TempSchedPtr  = GetScheduleIndex(cAlphaArgs(7))  ! convert schedule name to pointer
        IF (VentSlab(Item)%TempSchedPtr == 0) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(7))//'="'//trim(cAlphaArgs(7))//'" not found.')
          ErrorsFound=.TRUE.
        END IF
      CASE DEFAULT
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(5))//'="'//trim(cAlphaArgs(5))//'".')
    END SELECT

    VentSlab(Item)%MinOASchedName                 = cAlphaArgs(6)
    VentSlab(Item)%MinOASchedPtr  = GetScheduleIndex(cAlphaArgs(6))  ! convert schedule name to pointer
    IF (VentSlab(Item)%MinOASchedPtr == 0) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(6))//'="'//trim(cAlphaArgs(6))//'" not found.')
      ErrorsFound=.TRUE.
    END IF

! System Configuration:
   IF (SameString(cAlphaArgs(8),'SlabOnly')) THEN
      VentSlab(Item)%SysConfg = SlabOnly
   ELSE IF (SameString(cAlphaArgs(8),'SlabAndZone')) THEN
      VentSlab(Item)%SysConfg = SlabandZone
    ELSE IF (SameString(cAlphaArgs(8),'SeriesSlabs')) THEN
      VentSlab(Item)%SysConfg = SeriesSlabs
   ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(8))//'="'//trim(cAlphaArgs(8))//'".')
      CALL ShowContinueError('Control reset to SLAB ONLY Configuration.')
      VentSlab(Item)%SysConfg = SlabOnly
   END IF


! Hollow Core information :
   VentSlab(Item)%CoreDiameter  = rNumericArgs(4)
   VentSlab(Item)%CoreLength    = rNumericArgs(5)
   VentSlab(Item)%CoreNumbers   = rNumericArgs(6)


      IF (SameString(cAlphaArgs(8),'SurfaceListNames')) THEN
           IF(.not. lNumericBlanks(4)) THEN
             CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" '//   &
                ' Core Diameter is not needed for the series slabs configuration- ignored.')
             CALL ShowContinueError('...It has been asigned on SlabGroup.')
           END IF
      END IF

      IF (SameString(cAlphaArgs(8),'SurfaceListNames')) THEN
           IF(.not. lNumericBlanks(5)) THEN
             CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" '//   &
                ' Core Length is not needed for the series slabs configuration- ignored.')
             CALL ShowContinueError('...It has been asigned on SlabGroup.')
           END IF
      END IF

      IF (SameString(cAlphaArgs(8),'SurfaceListNames')) THEN
           IF(.not. lNumericBlanks(6)) THEN
             CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" '//   &
                ' Core Numbers is not needed for the series slabs configuration- ignored.')
             CALL ShowContinueError('...It has been asigned on SlabGroup.')
           END IF
      END IF

! Process the temperature control type
   IF (SameString(cAlphaArgs(9),OutsideAirDryBulbTemperature)) THEN
      VentSlab(Item)%ControlType = ODBControl
   ELSE IF (SameString(cAlphaArgs(9),OutsideAirWetBulbTemperature)) THEN
      VentSlab(Item)%ControlType = OWBControl
   ELSE IF (SameString(cAlphaArgs(9),OperativeTemperature)) THEN
      VentSlab(Item)%ControlType = OPTControl
   ELSE IF (SameString(cAlphaArgs(9),MeanAirTemperature)) THEN
      VentSlab(Item)%ControlType = MATControl
   ELSE IF (SameString(cAlphaArgs(9),MeanRadiantTemperature)) THEN
      VentSlab(Item)%ControlType = MRTControl
   ELSE IF (SameString(cAlphaArgs(9),SlabSurfaceTemperature)) THEN
      VentSlab(Item)%ControlType = SURControl
   ELSE IF (SameString(cAlphaArgs(9),SlabSurfaceDewPointTemperature)) THEN
      VentSlab(Item)%ControlType = DPTZControl
   ELSE
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(9))//'="'//trim(cAlphaArgs(9))//'".')
      CALL ShowContinueError('Control reset to ODB control.')
      VentSlab(Item)%ControlType = ODBControl
   END IF

! Heating User Input Data For Ventilated Slab Control :

! High Air Temp :
   VentSlab(Item)%HotAirHiTempSched    = cAlphaArgs(10)
   VentSlab(Item)%HotAirHiTempSchedPtr = GetScheduleIndex(cAlphaArgs(10))
   IF ((VentSlab(Item)%HotAirHiTempSchedPtr == 0).AND. (.not. lAlphaBlanks(10))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(10))//'="'//trim(cAlphaArgs(10))//'" not found.')
     ErrorsFound=.true.
   END IF

! Low Air Temp :

   VentSlab(Item)%HotAirLoTempSched    = cAlphaArgs(11)
   VentSlab(Item)%HotAirLoTempSchedPtr = GetScheduleIndex(cAlphaArgs(11))
   IF ((VentSlab(Item)%HotAirLoTempSchedPtr == 0).AND. (.not. lAlphaBlanks(11))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(11))//'="'//trim(cAlphaArgs(11))//'" not found.')
     ErrorsFound=.true.
   END IF


   VentSlab(Item)%HotCtrlHiTempSched    = cAlphaArgs(12)
   VentSlab(Item)%HotCtrlHiTempSchedPtr = GetScheduleIndex(cAlphaArgs(12))
   IF ((VentSlab(Item)%HotCtrlHiTempSchedPtr == 0).AND. (.not. lAlphaBlanks(12))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(12))//'="'//trim(cAlphaArgs(12))//'" not found.')
     ErrorsFound=.true.
   END IF

   VentSlab(Item)%HotCtrlLoTempSched    = cAlphaArgs(13)
   VentSlab(Item)%HotCtrlLoTempSchedPtr = GetScheduleIndex(cAlphaArgs(13))
   IF ((VentSlab(Item)%HotCtrlLoTempSchedPtr == 0).AND. (.not. lAlphaBlanks(13))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(13))//'="'//trim(cAlphaArgs(13))//'" not found.')
     ErrorsFound=.true.
   END IF


! Cooling User Input Data For Ventilated Slab Control :
       ! Cooling High Temp Sch.
   VentSlab(Item)%ColdAirHiTempSched    = cAlphaArgs(13)
   VentSlab(Item)%ColdAirHiTempSchedPtr = GetScheduleIndex(cAlphaArgs(14))
   IF ((VentSlab(Item)%ColdAirHiTempSchedPtr == 0).AND. (.not. lAlphaBlanks(14))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(14))//'="'//trim(cAlphaArgs(14))//'" not found.')
     ErrorsFound=.true.
   END IF

       ! Cooling Low Temp Sch.

   VentSlab(Item)%ColdAirLoTempSched    = cAlphaArgs(15)
   VentSlab(Item)%ColdAirLoTempSchedPtr = GetScheduleIndex(cAlphaArgs(15))
   IF ((VentSlab(Item)%ColdAirLoTempSchedPtr == 0).AND. (.not. lAlphaBlanks(15))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(15))//'="'//trim(cAlphaArgs(15))//'" not found.')
     ErrorsFound=.true.
   END IF

       ! Cooling Control High Sch.

   VentSlab(Item)%ColdCtrlHiTempSched    = cAlphaArgs(16)
   VentSlab(Item)%ColdCtrlHiTempSchedPtr = GetScheduleIndex(cAlphaArgs(16))
   IF ((VentSlab(Item)%ColdCtrlHiTempSchedPtr == 0).AND. (.not. lAlphaBlanks(16))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(16))//'="'//trim(cAlphaArgs(16))//'" not found.')
     ErrorsFound=.true.
   END IF

       ! Cooling Control Low Sch.

   VentSlab(Item)%ColdCtrlLoTempSched    = cAlphaArgs(17)
   VentSlab(Item)%ColdCtrlLoTempSchedPtr = GetScheduleIndex(cAlphaArgs(17))
   IF ((VentSlab(Item)%ColdCtrlLoTempSchedPtr == 0).AND. (.not. lAlphaBlanks(17))) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(17))//'="'//trim(cAlphaArgs(17))//'" not found.')
     ErrorsFound=.true.
   END IF

          ! Main air nodes (except outside air node):
          ! Refer the Unit Ventilator Air Node note

          ! MJW CR7903 - Ventilated slab was not drawing properly in HVAC Diagram svg output
          !  This object is structured differently from other zone equipment in that it functions
          !  as both a parent and non-parent, and it has an implicit OA mixer.  This makes it difficult
          !  to register the nodes in a way that HVAC Diagram can understand and in a way that satisfies
          !  node connection tests.  Here's an explanation of the changes made for this CR:
          !      In general, nodes associated with the ventilated slab system (the overall parent object)
          !         are registered with "-SYSTEM" appended to the object type and object name
          !         This same suffix is also added later when SetUpCompSets is called, for the same reason
          !      In general, nodes associated with the implicit OA mixer object
          !         are registered with "-OA MIXER" appended to the object type and object name
          !      %ReturnAirNode is one inlet to the implicit oa mixer
          !         For SlabOnly and SeriesSlab this node does nothing,
          !             so NodeConnectionType_Internal,ObjectIsNotParent, -OA MIXER
          !         For SlabandZone, this node extracts air from the zone,
          !             so NodeConnectionType_Inlet,ObjectIsNotParent, -OA MIXER
          !         For SlabandZone, this node is also used to associate the whole system with a pair of zone inlet/exhaust nodes,
          !             so it is registered again as NodeConnectionType_Inlet,1,ObjectIsParent, -SYSTEM
          !      %RadInNode is the ultimate air inlet to the slab or series of slabs
          !         For all types of ventilated slab, this is NodeConnectionType_Inlet,ObjectIsNotParent
          !      %OAMixerOutNode is the outlet from the implicit OA mixer
          !         For all types of ventilated slab, this is NodeConnectionType_Outlet,ObjectIsNotParent
          !      %FanOutletNode is the outlet from the explicit fan child object (redundant input, should mine from child)
          !         For all types of ventilated slab, this is NodeConnectionType_Internal,ObjectIsParent
          !      %ZoneAirInNode is applicable only to SlabandZone configuration. It is the node that flows into the zone,
          !         and it is also the outlet from the ventilated slab section, so it must be registered twice
          !         First for the overall system, NodeConnectionType_Outlet,ObjectIsParent, -SYSTEM
          !         Second as the slab outlet, NodeConnectionType_Outlet,ObjectIsNotParent
          !      %OutsideAirNode is the outdoor air inlet to the OA mixer
          !         For all types of ventilated slab, this is NodeConnectionType_Inlet,ObjectIsNotParent, -OA MIXER


   IF (VentSlab(Item)%SysConfg == SlabOnly) THEN

     VentSlab(Item)%ReturnAirNode = &
               GetOnlySingleNode(cAlphaArgs(18),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-OA MIXER',TRIM(cAlphaArgs(1))//'-OA MIXER', &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsNotParent)
     VentSlab(Item)%RadInNode = &
               GetOnlySingleNode(cAlphaArgs(19),ErrorsFound,CurrentModuleObject,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

     VentSlab(Item)%OAMixerOutNode = &
               GetOnlySingleNode(cAlphaArgs(23),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-OA MIXER',TRIM(cAlphaArgs(1))//'-OA MIXER', &
                            NodeType_Air, NodeConnectionType_Outlet,1,ObjectIsNotParent)
     VentSlab(Item)%FanOutletNode = &
               GetOnlySingleNode(cAlphaArgs(24),ErrorsFound,CurrentModuleObject,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent)

   ELSE IF (VentSlab(Item)%SysConfg == SeriesSlabs) THEN

     VentSlab(Item)%ReturnAirNode = &
               GetOnlySingleNode(cAlphaArgs(18),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-OA MIXER',TRIM(cAlphaArgs(1))//'-OA MIXER', &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsNotParent)
     VentSlab(Item)%RadInNode = &
               GetOnlySingleNode(cAlphaArgs(19),ErrorsFound,CurrentModuleObject,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

     VentSlab(Item)%OAMixerOutNode = &
               GetOnlySingleNode(cAlphaArgs(23),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-OA MIXER',TRIM(cAlphaArgs(1))//'-OA MIXER', &
                            NodeType_Air, NodeConnectionType_Outlet,1,ObjectIsNotParent)
     VentSlab(Item)%FanOutletNode = &
               GetOnlySingleNode(cAlphaArgs(24),ErrorsFound,CurrentModuleObject,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent)

    ELSE IF (VentSlab(Item)%SysConfg == SlabandZone) THEN

      VentSlab(Item)%ReturnAirNode = &
               GetOnlySingleNode(cAlphaArgs(18),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-SYSTEM',TRIM(cAlphaArgs(1))//'-SYSTEM', &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
      VentSlab(Item)%ReturnAirNode = &
               GetOnlySingleNode(cAlphaArgs(18),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-OA MIXER',TRIM(cAlphaArgs(1))//'-OA MIXER', &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      VentSlab(Item)%RadInNode = &
               GetOnlySingleNode(cAlphaArgs(19),ErrorsFound,CurrentModuleObject,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      VentSlab(Item)%OAMixerOutNode = &
               GetOnlySingleNode(cAlphaArgs(23),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-OA MIXER',TRIM(cAlphaArgs(1))//'-OA MIXER', &
                            NodeType_Air, NodeConnectionType_Outlet,1,ObjectIsNotParent)
      VentSlab(Item)%FanOutletNode = &
               GetOnlySingleNode(cAlphaArgs(24),ErrorsFound,CurrentModuleObject,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent)
    END IF

   IF (VentSlab(Item)%SysConfg == SlabOnly) THEN
      IF(.not. lAlphaBlanks(20)) THEN
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" '//   &
            trim(cAlphaFields(20))//'="'//trim(cAlphaArgs(20))//'" not needed - ignored.')
        CALL ShowContinueError('It is used for "SlabAndZone" only')
      END IF

   ELSE IF (VentSlab(Item)%SysConfg == SlabandZone) THEN
      IF (lAlphaBlanks(20)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(20))//' is blank and must be entered.')
        ErrorsFound=.TRUE.
      END IF

      VentSlab(Item)%ZoneAirInNode = &
               GetOnlySingleNode(cAlphaArgs(20),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-SYSTEM',TRIM(cAlphaArgs(1))//'-SYSTEM', &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

      VentSlab(Item)%ZoneAirInNode = &
               GetOnlySingleNode(cAlphaArgs(20),ErrorsFound,CurrentModuleObject,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
   END IF


   VentSlab(Item)%OutsideAirNode = &
          !  Set connection type to 'Inlet', because it now uses an OA node
               GetOnlySingleNode(cAlphaArgs(21),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-OA MIXER',TRIM(cAlphaArgs(1))//'-OA MIXER', &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

   IF (.not. lAlphaBlanks(21)) THEN
      CALL CheckAndAddAirNodeNumber(VentSlab(Item)%OutsideAirNode,IsValid)
      IF (.not. IsValid) THEN
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//   &
            '", Adding OutdoorAir:Node='//TRIM(cAlphaArgs(21)))
      ENDIF

   ENDIF

   VentSlab(Item)%AirReliefNode = &
               GetOnlySingleNode(cAlphaArgs(22),ErrorsFound, &
                            TRIM(CurrentModuleObject)//'-OA MIXER',TRIM(cAlphaArgs(1))//'-OA MIXER', &
                            NodeType_Air,NodeConnectionType_ReliefAir,1,ObjectIsNotParent)



          ! Fan information:
   VentSlab(Item)%FanName       = cAlphaArgs(25)


   IF (VentSlab(Item)%OAControlType == FixedOAControl) THEN
      VentSlab(Item)%OutAirVolFlow = VentSlab(Item)%MinOutAirVolFlow
      VentSlab(Item)%MaxOASchedName = VentSlab(Item)%MinOASchedName
      VentSlab(Item)%MaxOASchedPtr  = GetScheduleIndex(VentSlab(Item)%MinOASchedName)
   END IF


    ! Add fan to component sets array
   CALL SetUpCompSets(TRIM(CurrentModuleObject)//'-SYSTEM', TRIM(VentSlab(Item)%Name)//'-SYSTEM', &
                       'UNDEFINED',cAlphaArgs(25),cAlphaArgs(23),cAlphaArgs(24))

   ! Coil options assign

   SELECT CASE (cAlphaArgs(26))
      CASE ('HEATINGANDCOOLING')
        VentSlab(Item)%CoilOption = BothOption
      CASE ('HEATING')
        VentSlab(Item)%CoilOption = HeatingOption
      CASE ('COOLING')
        VentSlab(Item)%CoilOption = CoolingOption
      CASE ('NONE')
        VentSlab(Item)%CoilOption = NoneOption
      CASE DEFAULT
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(26))//'="'//trim(cAlphaArgs(26))//'".')
        ErrorsFound = .TRUE.
   END SELECT

   IF (VentSlab(Item)%CoilOption == BothOption .or. VentSlab(Item)%CoilOption == HeatingOption) THEN
          ! Heating coil information:
!        A27, \field Heating Coil Object Type
!             \type choice
!             \key Coil:Heating:Water
!             \key Coil:Heating:Electric
!             \key Coil:Heating:Gas
!             \key Coil:Heating:Steam
!        A28, \field Heating Coil Name
!             \type object-list
!             \object-list HeatingCoilName

          ! Heating coil information:
     IF (.not. lAlphaBlanks(28)) THEN
       VentSlab(Item)%HCoilPresent     = .TRUE.
       VentSlab(Item)%HCoilTypeCh = cAlphaArgs(27)
       errflag=.false.

       SELECT CASE (cAlphaArgs(27))
         CASE ('COIL:HEATING:WATER')
           VentSlab(Item)%HCoilType = Heating_WaterCoilType
           VentSlab(Item)%HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating
         CASE ('COIL:HEATING:STEAM')
           VentSlab(Item)%HCoilType = Heating_SteamCoilType
           VentSlab(Item)%HCoil_PlantTypeNum = TypeOf_CoilSteamAirHeating
           VentSlab(Item)%HCoil_FluidIndex=FindRefrigerant('Steam')
           IF (VentSlab(Item)%HCoil_FluidIndex == 0) THEN
             CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
                'Steam Properties not found.')
             IF (SteamMessageNeeded) &
               CALL ShowContinueError('Steam Fluid Properties should have been included in the input file.')
             ErrorsFound=.true.
             SteamMessageNeeded=.false.
           ENDIF
         CASE ('COIL:HEATING:ELECTRIC')
           VentSlab(Item)%HCoilType = Heating_ElectricCoilType
         CASE ('COIL:HEATING:GAS')
           VentSlab(Item)%HCoilType = Heating_GasCoilType
         CASE DEFAULT
           CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
               trim(cAlphaFields(27))//'="'//trim(cAlphaArgs(27))//'".')
           ErrorsFound = .TRUE.
       END SELECT
       if (.not. errflag) then
         VentSlab(Item)%HCoilName        = cAlphaArgs(28)
         CALL ValidateComponent(cAlphaArgs(27),VentSlab(Item)%HCoilName,IsNotOK,CurrentModuleObject)
         IF (IsNotOK) THEN
           CALL ShowContinueError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cAlphaFields(28))//'="'//trim(cAlphaArgs(28))//'".')
           CALL ShowContinueError('... not valid for '//trim(cAlphaFields(27))//'="'//  &
             trim(cAlphaArgs(27))//'".')
           ErrorsFound=.true.
         ENDIF
       endif


       VentSlab(Item)%MinVolHotWaterFlow = 0.0d0
       VentSlab(Item)%MinVolHotSteamFlow = 0.0d0

    ! The heating coil control node is necessary for a hot water coil, but not necessary for an
    ! electric or gas coil.
       IF (VentSlab(Item)%HCoilType .EQ. Heating_GasCoilType .OR.   &
          VentSlab(Item)%HCoilType .EQ. Heating_ElectricCoilType) THEN
          IF (.not. lAlphaBlanks(29)) THEN
            CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" '//   &
              trim(cAlphaFields(29))//'="'//trim(cAlphaArgs(29))//'" not needed - ignored.')
            CALL ShowContinueError('..It is used for hot water coils only.')
          END IF
       ELSE
         IF(lAlphaBlanks(29)) THEN
           CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
              trim(cAlphaFields(29))//' is blank and must be entered.')
           ErrorsFound=.true.
         END IF
         VentSlab(Item)%HotControlNode  = &
            GetOnlySingleNode(cAlphaArgs(29),ErrorsFound,CurrentModuleObject,cAlphaArgs(1), &
                            NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
       END IF
       VentSlab(Item)%HotControlOffset = 0.001d0

       IF (VentSlab(Item)%HCoilType == Heating_WaterCoilType) THEN
           VentSlab(Item)%MaxVolHotWaterFlow = GetWaterCoilMaxFlowRate('Coil:Heating:Water',  &
                                                 VentSlab(Item)%HCoilName,ErrorsFound)
           VentSlab(Item)%MaxVolHotSteamFlow = GetWaterCoilMaxFlowRate('Coil:Heating:Water',  &
                                                 VentSlab(Item)%HCoilName,ErrorsFound)
       ELSEIF (VentSlab(Item)%HCoilType == Heating_SteamCoilType) THEN
           VentSlab(Item)%MaxVolHotWaterFlow = GetSteamCoilMaxFlowRate('Coil:Heating:Steam',  &
                                                     VentSlab(Item)%HCoilName,ErrorsFound)
           VentSlab(Item)%MaxVolHotSteamFlow = GetSteamCoilMaxFlowRate('Coil:Heating:Steam',  &
                                                 VentSlab(Item)%HCoilName,ErrorsFound)
       ENDIF

     ELSE  ! no heating coil
       CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
          '" missing heating coil.')
       CALL ShowContinueError('a heating coil is required for '//trim(cAlphaFields(26))//  &
          '="'//trim(cAlphaArgs(26))//'".')
       ErrorsFound=.true.
     ENDIF
   ENDIF

   IF (VentSlab(Item)%CoilOption == BothOption .or. VentSlab(Item)%CoilOption == CoolingOption) THEN
           ! Cooling coil information (if one is present):
!        A30, \field Cooling Coil Object Type
!             \type choice
!             \key Coil:Cooling:Water
!             \key Coil:Cooling:Water:DetailedGeometry
!             \key CoilSystem:Cooling:Water:HeatExchangerAssisted
!        A31, \field Cooling Coil Name
!             \type object-list
!             \object-list CoolingCoilsWater
           ! Cooling coil information (if one is present):
     IF (.not. lAlphaBlanks(31)) THEN
       VentSlab(Item)%CCoilPresent     = .TRUE.
       VentSlab(Item)%CCoilTypeCh = cAlphaArgs(30)
       errflag=.false.

       SELECT CASE (cAlphaArgs(30))
         CASE ('COIL:COOLING:WATER')
           VentSlab(Item)%CCoilType = Cooling_CoilWaterCooling
           VentSlab(Item)%CCoil_PlantTypeNum = TypeOf_CoilWaterCooling
           VentSlab(Item)%CCoilPlantName=cAlphaArgs(31)
         CASE ('COIL:COOLING:WATER:DETAILEDGEOMETRY')
           VentSlab(Item)%CCoilType = Cooling_CoilDetailedCooling
           VentSlab(Item)%CCoil_PlantTypeNum = TypeOf_CoilWaterDetailedFlatCooling
           VentSlab(Item)%CCoilPlantName=cAlphaArgs(31)
         CASE ('COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED')
           VentSlab(Item)%CCoilType = Cooling_CoilHXAssisted
           CALL GetHXCoilTypeAndName(cAlphaArgs(30),cAlphaArgs(31),ErrorsFound,  &
                   VentSlab(Item)%CCoilPlantType,VentSlab(Item)%CCoilPlantName)
           IF (SameString(VentSlab(Item)%CCoilPlantType,'Coil:Cooling:Water')) THEN
             VentSlab(Item)%CCoil_PlantTypeNum=TypeOf_CoilWaterCooling
           ELSEIF (SameString(VentSlab(Item)%CCoilPlantType,'Coil:Cooling:Water:DetailedGeometry')) THEN
             VentSlab(Item)%CCoil_PlantTypeNum=TypeOf_CoilWaterDetailedFlatCooling
           ELSE
             CALL ShowSevereError('GetVentilatedSlabInput: '//trim(CurrentModuleObject)//'="'//trim(VentSlab(Item)%Name)//  &
                 '", invalid')
             CALL ShowContinueError('For: '//TRIM(cAlphaFields(30))//'="'//TRIM(cAlphaArgs(30))//'".')
             CALL ShowContinueError('Invalid Coil Type='//trim(VentSlab(Item)%CCoilPlantType)//  &
                ', Name='//trim(VentSlab(Item)%CCoilPlantName))
             CALL ShowContinueError('must be "Coil:Cooling:Water" or "Coil:Cooling:Water:DetailedGeometry"')
             ErrorsFound=.true.
           ENDIF
         CASE DEFAULT
           CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
               trim(cAlphaFields(29))//'="'//trim(cAlphaArgs(29))//'".')
           ErrorsFound=.TRUE.
           errflag=.true.
       END SELECT

       if (.not. errflag) then
         VentSlab(Item)%CCoilName = cAlphaArgs(31)
         CALL ValidateComponent(cAlphaArgs(30),VentSlab(Item)%CCoilName,IsNotOK,'ZoneHVAC:VentilatedSlab ')
         IF (IsNotOK) THEN
           CALL ShowContinueError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cAlphaFields(31))//'="'//trim(cAlphaArgs(31))//'".')
           CALL ShowContinueError('... not valid for '//trim(cAlphaFields(30))//'="'//  &
              trim(cAlphaArgs(30))//'".')
           ErrorsFound=.true.
         ENDIF
       endif

       VentSlab(Item)%MinVolColdWaterFlow = 0.0d0

       VentSlab(Item)%ColdControlNode   = &
             GetOnlySingleNode(cAlphaArgs(32),ErrorsFound,CurrentModuleObject,cAlphaArgs(1), &
                          NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)

       IF (lAlphaBlanks(32)) THEN
         CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
            trim(cAlphaFields(32))//' is blank and must be entered.')
         ErrorsFound=.true.
       END IF

       VentSlab(Item)%ColdControlOffset = 0.001d0

       IF (VentSlab(Item)%CCoilType == Cooling_CoilWaterCooling) THEN
           VentSlab(Item)%MaxVolColdWaterFlow = GetWaterCoilMaxFlowRate('Coil:Cooling:Water',  &
                                                 VentSlab(Item)%CCoilName,ErrorsFound)
       ELSEIF (VentSlab(Item)%CCoilType == Cooling_CoilDetailedCooling) THEN
           VentSlab(Item)%MaxVolColdWaterFlow = GetWaterCoilMaxFlowRate('Coil:Cooling:Water:DetailedGeometry',  &
                                                 VentSlab(Item)%CCoilName,ErrorsFound)
       ELSEIF (VentSlab(Item)%CCoilType == Cooling_CoilHXAssisted) THEN
           VentSlab(Item)%MaxVolColdWaterFlow = GetHXAssistedCoilFlowRate('CoilSystem:Cooling:Water:HeatExchangerAssisted',  &
                                                 VentSlab(Item)%CCoilName,ErrorsFound)
       ENDIF

     ELSE ! No Cooling Coil
       CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
          '" missing cooling coil.')
       CALL ShowContinueError('a cooling coil is required for '//trim(cAlphaFields(26))//  &
          '="'//trim(cAlphaArgs(26))//'".')
       ErrorsFound=.true.
     END IF
   ENDIF
    IF (.NOT. lAlphaBlanks(33)) THEN
      VentSlab(Item)%AvailManagerListName = cAlphaArgs(33)
      ZoneComp(VentilatedSlab_Num)%ZoneCompAvailMgrs(Item)%AvailManagerListName = cAlphaArgs(33)
    ENDIF

   SELECT CASE (VentSlab(Item)%CoilOption)
      CASE (Bothoption) ! 'HeatingAndCooling'
           ! Add cooling coil to component sets array when present
        CALL SetUpCompSets(TRIM(CurrentModuleObject)//'-SYSTEM', TRIM(VentSlab(Item)%Name)//'-SYSTEM', &
            cAlphaArgs(30), cAlphaArgs(31), cAlphaArgs(24), 'UNDEFINED')

           ! Add heating coil to component sets array when cooling coil present
        CALL SetUpCompSets(TRIM(CurrentModuleObject)//'-SYSTEM', TRIM(VentSlab(Item)%Name)//'-SYSTEM', &
            cAlphaArgs(27), cAlphaArgs(28), 'UNDEFINED', cAlphaArgs(19))

     CASE (HeatingOption) ! 'Heating'
           ! Add heating coil to component sets array when no cooling coil present
        CALL SetUpCompSets(TRIM(CurrentModuleObject)//'-SYSTEM', TRIM(VentSlab(Item)%Name)//'-SYSTEM', &
            cAlphaArgs(27), cAlphaArgs(28), cAlphaArgs(24), cAlphaArgs(19))

     CASE (CoolingOption) ! 'Cooling'
           ! Add cooling coil to component sets array when no heating coil present
        CALL SetUpCompSets(TRIM(CurrentModuleObject)//'-SYSTEM', TRIM(VentSlab(Item)%Name)//'-SYSTEM', &
            cAlphaArgs(30), cAlphaArgs(31), cAlphaArgs(24), cAlphaArgs(19))

     CASE (NoneOption)

     CASE DEFAULT

   END SELECT

 END DO  ! ...loop over all of the ventilated slab found in the input file

  DEALLOCATE(cAlphaArgs)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(rNumericArgs)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  IF (ErrorsFound) Call ShowFatalError(CurrentModuleObject//' errors occurred in input.  Program terminates.')

          ! Setup Report variables for the VENTILATED SLAB
  DO Item = 1, NumOfVentSlabs
!   CALL SetupOutputVariable('Ventilated Slab Direct Heat Loss Rate [W]', &
!                             VentSlab(Item)%DirectHeatLossRate,'System', &
!                             'Average', VentSlab(Item)%Name)
!   CALL SetupOutputVariable('Ventilated Slab Direct Heat Loss [W]',        &
!                             VentSlab(Item)%DirectHeatLoss,'System', &
!                             'Sum', VentSlab(Item)%Name)
!   CALL SetupOutputVariable('Ventilated Slab Direct Heat Gain Rate [W]',        &
!                             VentSlab(Item)%DirectHeatGainRate,'System', &
!                            'Average', VentSlab(Item)%Name)
!   CALL SetupOutputVariable('Ventilated Slab Direct Heat Gain [J]',        &
!                           VentSlab(Item)%DirectHeatGain,'System', &
!                             'Sum', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Radiant Heating Rate [W]',        &
                             VentSlab(Item)%RadHeatingPower,'System', &
                             'Average', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Radiant Heating Energy [J]',        &
                             VentSlab(Item)%RadHeatingEnergy,'System', &
                             'Sum', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Radiant Cooling Rate [W]',        &
                             VentSlab(Item)%RadCoolingPower,'System', &
                             'Average', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Radiant Cooling Energy [J]',        &
                             VentSlab(Item)%RadCoolingEnergy,'System', &
                             'Sum', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Coil Heating Rate [W]',        &
                             VentSlab(Item)%HeatCoilPower,'System', &
                             'Average', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Coil Heating Energy [J]',        &
                             VentSlab(Item)%HeatCoilEnergy,'System', &
                             'Sum', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Coil Total Cooling Rate [W]',        &
                             VentSlab(Item)%TotCoolCoilPower,'System', &
                             'Average', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Coil Total Cooling Energy [J]',        &
                             VentSlab(Item)%TotCoolCoilEnergy,'System', &
                             'Sum', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Coil Sensible Cooling Rate [W]',        &
                             VentSlab(Item)%SensCoolCoilPower,'System', &
                             'Average', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Coil Sensible Cooling Energy [J]',        &
                             VentSlab(Item)%SensCoolCoilEnergy,'System', &
                             'Sum', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Coil Latent Cooling Rate [W]',        &
                             VentSlab(Item)%LateCoolCoilPower,'System', &
                             'Average', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Coil Latent Cooling Energy [J]',        &
                             VentSlab(Item)%LateCoolCoilEnergy,'System', &
                             'Sum', VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Air Mass Flow Rate [kg/s]',   &
                             VentSlab(Item)%AirMassFlowRate,'System','Average', &
                             VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Fan Electric Power [W]',  &
                             VentSlab(Item)%ElecFanPower,'System', &
                             'Average', VentSlab(Item)%Name)
!! Note that the ventilated slab fan electric is NOT metered because this value is already metered through the fan component
    CALL SetupOutputVariable('Zone Ventilated Slab Fan Electric Energy [J]',   &
                            VentSlab(Item)%ElecFanEnergy,'System','Sum', &
                             VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Inlet Air Temperature [C]',   &
                             VentSlab(Item)%SlabInTemp,'System','Average', &
                             VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Outlet Air Temperature [C]',   &
                             VentSlab(Item)%SlabOutTemp,'System','Average', &
                             VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Zone Inlet Air Temperature [C]',   &
                             VentSlab(Item)%ZoneInletTemp,'System','Average', &
                             VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Return Air Temperature [C]',   &
                             VentSlab(Item)%ReturnAirTemp,'System','Average', &
                             VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Fan Outlet Air Temperature [C]',   &
                             VentSlab(Item)%FanOutletTemp,'System','Average', &
                             VentSlab(Item)%Name)
    CALL SetupOutputVariable('Zone Ventilated Slab Fan Availability Status []', VentSlab(Item)%AvailStatus,&
                             'System','Average',VentSlab(Item)%Name)


  END DO

  RETURN

END SUBROUTINE GetVentilatedSlabInput


SUBROUTINE InitVentilatedSlab(Item, VentSlabZoneNum, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   June 2008
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes all of the data elements which are necessary
          ! to simulate a Ventilated Slab.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,   ONLY : OutBaroPress, OutDryBulbTemp, OutHumRat, StdBaroPress,StdRhoAir
  USE DataGlobals,       ONLY : NumOfZones, BeginEnvrnFlag, AnyPlantInModel
  USE DataLoopNode,      ONLY : Node
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue
  USE DataHeatBalFanSys, ONLY : MAT,ZoneAirHumRat
  USE DataZoneEquipment, ONLY : ZoneEquipInputsFilled,CheckZoneEquipmentList, VentilatedSlab_Num
  USE DataPlant,         ONLY : PlantLoop, ScanPlantLoopsForObject, TypeOf_CoilWaterSimpleHeating,&
                                TypeOf_CoilSteamAirHeating, TypeOf_CoilWaterCooling, TypeOf_CoilWaterDetailedFlatCooling
  USE FluidProperties,   ONLY : GetDensityGlycol
  USE PlantUtilities,    ONLY : InitComponentNodes
  USE DataHVACGlobals,   ONLY : ZoneComp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Item                ! index for the current ventilated slab
  INTEGER, INTENT(IN) :: VentSlabZoneNum     ! number of zone being served
  LOGICAL, INTENT(IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  REAL           :: CurrentFlowSchedule   ! Schedule value for flow fraction in a ventilated slab
  INTEGER        :: RadNum                ! Number of the radiant system (DO loop counter)
  INTEGER        :: RadSurfNum            ! Number of the radiant system surface (DO loop counter)
  INTEGER        :: SurfNum               ! Intermediate variable for keeping track of the surface number
  INTEGER        :: ZoneNum               ! Intermediate variable for keeping track of the zone number

  INTEGER        :: AirRelNode         ! relief air node number in Ventilated Slab loop
  INTEGER        :: ColdConNode        ! cold water control node number in Ventilated Slab loop
  LOGICAL,SAVE   :: MyOneTimeFlag = .true.
  LOGICAL,SAVE   :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  INTEGER        :: HotConNode         ! hot water control node number in Ventilated Slab loop
  INTEGER        :: InNode             ! inlet node number in Ventilated Slab loop
  INTEGER        :: OutNode            ! outlet node number in Ventilated Slab loop
  INTEGER        :: OutsideAirNode     ! outside air node number in Ventilated Slab loop
  REAL(r64)      :: RhoAir             ! air density at InNode
  REAL(r64)      :: TempSteamIn
  REAL(r64)      :: SteamDensity
  INTEGER        :: ZoneAirInNode
  INTEGER        :: MixOut
  REAL(r64)      :: rho
  LOGICAL        :: errFlag
          ! FLOW:

  ! Do the one time initializations

IF (MyOneTimeFlag) THEN
   ALLOCATE(MyEnvrnFlag(NumOfVentSlabs))
   ALLOCATE(MySizeFlag(NumOfVentSlabs))
   ALLOCATE(MyPlantScanFlag(NumOfVentSlabs))
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


    ! Initialize total areas for all radiant systems
    DO RadNum = 1, NumOfVentSlabs
      VentSlab(Item)%TotalSurfaceArea = 0.0d0
      DO SurfNum = 1, VentSlab(Item)%NumOfSurfaces
        VentSlab(Item)%TotalSurfaceArea = VentSlab(Item)%TotalSurfaceArea &
                                                +Surface(VentSlab(Item)%SurfacePtr(SurfNum))%Area
      END DO
    END DO
  MyEnvrnFlag = .TRUE.
  MySizeFlag = .TRUE.
  MyPlantScanFlag = .TRUE.
  MyOneTimeFlag = .FALSE.
END IF

IF (ALLOCATED(ZoneComp)) THEN
  ZoneComp(VentilatedSlab_Num)%ZoneCompAvailMgrs(Item)%ZoneNum = VentSlabZoneNum
  VentSlab(Item)%AvailStatus = ZoneComp(VentilatedSlab_Num)%ZoneCompAvailMgrs(Item)%AvailStatus
ENDIF

IF (MyPlantScanFlag(item) .AND. ALLOCATED(PlantLoop)) THEN
  IF ( (VentSlab(Item)%HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating) .or. &
       (VentSlab(Item)%HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating) ) THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject( VentSlab(Item)%HCoilName, &
                                  VentSlab(Item)%HCoil_PlantTypeNum, &
                                  VentSlab(Item)%HWLoopNum, &
                                  VentSlab(Item)%HWLoopSide, &
                                  VentSlab(Item)%HWBranchNum, &
                                  VentSlab(Item)%HWCompNum,  &
                                  errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowContinueError('Reference Unit="'//trim(VentSlab(Item)%Name)//'", type=ZoneHVAC:VentilatedSlab')
      CALL ShowFatalError('InitVentilatedSlab: Program terminated due to previous condition(s).')
    ENDIF

    VentSlab(Item)%HotCoilOutNodeNum   =    &
            PlantLoop(VentSlab(Item)%HWLoopNum)%LoopSide(VentSlab(Item)%HWLoopSide) &
                         %Branch(VentSlab(Item)%HWBranchNum)%Comp(VentSlab(Item)%HWCompNum)%NodeNumOut

  ENDIF
  IF ( (VentSlab(Item)%CCoil_PlantTypeNum == TypeOf_CoilWaterCooling) .or. &
       (VentSlab(Item)%CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling) ) THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject( VentSlab(Item)%CCoilPlantName, &
                                  VentSlab(Item)%CCoil_PlantTypeNum, &
                                  VentSlab(Item)%CWLoopNum, &
                                  VentSlab(Item)%CWLoopSide, &
                                  VentSlab(Item)%CWBranchNum, &
                                  VentSlab(Item)%CWCompNum)
    IF (errFlag) THEN
      CALL ShowContinueError('Reference Unit="'//trim(VentSlab(Item)%Name)//'", type=ZoneHVAC:VentilatedSlab')
      CALL ShowFatalError('InitVentilatedSlab: Program terminated due to previous condition(s).')
    ENDIF
    VentSlab(Item)%ColdCoilOutNodeNum   =    &
            PlantLoop(VentSlab(Item)%CWLoopNum)%LoopSide(VentSlab(Item)%CWLoopSide) &
                         %Branch(VentSlab(Item)%CWBranchNum)%Comp(VentSlab(Item)%CWCompNum)%NodeNumOut
  ELSE
    IF (VentSlab(Item)%CCoilPresent)  &
         CALL ShowFatalError('InitVentilatedSlab: Unit='//trim(VentSlab(Item)%Name)//  &
               ', invalid cooling coil type. Program terminated.')
  ENDIF
  MyPlantScanFlag(item) = .FALSE.
ELSEIf (MyPlantScanFlag(item) .AND. .NOT. AnyPlantInModel) THEN
  MyPlantScanFlag(item) = .FALSE.
ENDIF

! need to check all Ventilated Slab units to see if they are on Zone Equipment List or issue warning
IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
  ZoneEquipmentListChecked=.true.
  DO RadNum=1,NumOfVentSlabs
    IF (CheckZoneEquipmentList(cMO_VentilatedSlab,VentSlab(RadNum)%Name)) CYCLE
    CALL ShowSevereError('InitVentilatedSlab: Ventilated Slab Unit=['//TRIM(cMO_VentilatedSlab)//','//  &
       TRIM(VentSlab(RadNum)%Name)//  &
         '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
  ENDDO
ENDIF

IF ( .NOT. SysSizingCalc .AND. MySizeFlag(Item) .AND. .NOT. MyPlantScanFlag(item) ) THEN

  CALL SizeVentilatedSlab(Item)

  MySizeFlag(Item) = .FALSE.

END IF


  ! Do the one time initializations
IF (BeginEnvrnFlag .AND. MyEnvrnFlag(Item) .AND. .NOT. MyPlantScanFlag(item)) THEN

         ! Coil Part
    InNode         = VentSlab(Item)%ReturnAirNode
    OutNode        = VentSlab(Item)%RadInNode
    HotConNode     = VentSlab(Item)%HotControlNode
    ColdConNode    = VentSlab(Item)%ColdControlNode
    OutsideAirNode = VentSlab(Item)%OutsideAirNode
    RhoAir         = StdRhoAir

         ! Radiation Panel Part
    ZeroSourceSumHATsurf = 0.0D0
    QRadSysSrcAvg = 0.0D0
    LastQRadSysSrc = 0.0D0
    LastSysTimeElapsed = 0.0D0
    LastTimeStepSys = 0.0D0
    IF (NumOfVentSlabs > 0) THEN
      VentSlab%RadHeatingPower          = 0.0D0
      VentSlab%RadHeatingEnergy         = 0.0D0
      VentSlab%RadCoolingPower          = 0.0D0
      VentSlab%RadCoolingEnergy         = 0.0D0
    ENDIF

    ! set the initial Temperature of Return Air


    ! set the mass flow rates from the input volume flow rates
    VentSlab(Item)%MaxAirMassFlow = RhoAir*VentSlab(Item)%MaxAirVolFlow
    VentSlab(Item)%OutAirMassFlow = RhoAir*VentSlab(Item)%OutAirVolFlow
    VentSlab(Item)%MinOutAirMassFlow = RhoAir*VentSlab(Item)%MinOutAirVolFlow
    IF (VentSlab(Item)%OutAirMassFlow > VentSlab(Item)%MaxAirMassFlow) THEN
      VentSlab(Item)%OutAirMassFlow = VentSlab(Item)%MaxAirMassFlow
      VentSlab(Item)%MinOutAirMassFlow = VentSlab(Item)%OutAirMassFlow * &
        (VentSlab(Item)%MinOutAirVolFlow / VentSlab(Item)%OutAirVolFlow)
      CALL ShowWarningError('Outdoor air mass flow rate higher than unit flow rate, reset to unit flow rate for ' &
                            //TRIM(VentSlab(Item)%Name))
    END IF

    ! set the node max and min mass flow rates
    Node(OutsideAirNode)%MassFlowRateMax = VentSlab(Item)%OutAirMassFlow
    Node(OutsideAirNode)%MassFlowRateMin = 0.0d0

    Node(OutNode)%MassFlowRateMax = VentSlab(Item)%MaxAirMassFlow
    Node(OutNode)%MassFlowRateMin = 0.0d0

    Node(InNode)%MassFlowRateMax = VentSlab(Item)%MaxAirMassFlow
    Node(InNode)%MassFlowRateMin = 0.0d0

    IF (VentSlab(Item)%HCoilPresent ) THEN ! Only initialize these if a heating coil is actually present

      IF (VentSlab(Item)%HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating .AND. .NOT. MyPlantScanFlag(item)) THEN
        rho =  GetDensityGlycol(PlantLoop(VentSlab(Item)%HWLoopNum )%fluidName, &
                                       60.d0, &
                                      PlantLoop( VentSlab(Item)%HWLoopNum )%fluidIndex, &
                                        'InitVentilatedSlab' )

        VentSlab(Item)%MaxHotWaterFlow = rho * VentSlab(Item)%MaxVolHotWaterFlow
        VentSlab(Item)%MinHotWaterFlow = rho * VentSlab(Item)%MinVolHotWaterFlow

        CALL InitComponentNodes(VentSlab(Item)%MinHotWaterFlow, &
                              VentSlab(Item)%MaxHotWaterFlow, &
                              VentSlab(Item)%HotControlNode, &
                              VentSlab(Item)%HotCoilOutNodeNum, &
                              VentSlab(Item)%HWLoopNum,  &
                              VentSlab(Item)%HWLoopSide, &
                              VentSlab(Item)%HWBranchNum, &
                              VentSlab(Item)%HWCompNum)

      END IF
      IF (VentSlab(Item)%HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating .AND. .NOT. MyPlantScanFlag(item)) THEN
        TempSteamIn= 100.00d0
        SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,VentSlab(Item)%HCoil_FluidIndex,'InitVentilatedSlab')
        VentSlab(Item)%MaxHotSteamFlow = SteamDensity*VentSlab(Item)%MaxVolHotSteamFlow
        VentSlab(Item)%MinHotSteamFlow = SteamDensity*VentSlab(Item)%MinVolHotSteamFlow

        CALL InitComponentNodes(VentSlab(Item)%MinHotSteamFlow, &
                              VentSlab(Item)%MaxHotSteamFlow, &
                              VentSlab(Item)%HotControlNode, &
                              VentSlab(Item)%HotCoilOutNodeNum, &
                              VentSlab(Item)%HWLoopNum,  &
                              VentSlab(Item)%HWLoopSide, &
                              VentSlab(Item)%HWBranchNum, &
                              VentSlab(Item)%HWCompNum)

      END IF
    END IF     !(VentSlab(Item)%HCoilPresent)

    IF (VentSlab(Item)%CCoilPresent  .AND. .NOT. MyPlantScanFlag(item) ) THEN
      ! Only initialize these if a cooling coil is actually present
      IF ((VentSlab(Item)%CCoil_PlantTypeNum == TypeOf_CoilWaterCooling) .OR. &
          (VentSlab(Item)%CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling) ) THEN
        rho =  GetDensityGlycol(PlantLoop(VentSlab(Item)%CWLoopNum )%fluidName, &
                                         InitConvTemp, &
                                        PlantLoop( VentSlab(Item)%CWLoopNum )%fluidIndex, &
                                          'InitVentilatedSlab' )
        VentSlab(Item)%MaxColdWaterFlow = rho * VentSlab(Item)%MaxVolColdWaterFlow
        VentSlab(Item)%MinColdWaterFlow = rho * VentSlab(Item)%MinVolColdWaterFlow
        CALL InitComponentNodes(VentSlab(Item)%MinColdWaterFlow , &
                                VentSlab(Item)%MaxColdWaterFlow, &
                                VentSlab(Item)%ColdControlNode, &
                                VentSlab(Item)%ColdCoilOutNodeNum, &
                                VentSlab(Item)%CWLoopNum,  &
                                VentSlab(Item)%CWLoopSide, &
                                VentSlab(Item)%CWBranchNum, &
                                VentSlab(Item)%CWCompNum)
      ENDIF
    END IF


    MyEnvrnFlag(Item) = .FALSE.

END IF  ! ...end start of environment inits

  IF (.NOT. BeginEnvrnFlag)  THEN

   MyEnvrnFlag(Item) = .TRUE.
  END IF

          ! These initializations are done every iteration...
  InNode         = VentSlab(Item)%ReturnAirNode
  OutNode        = VentSlab(Item)%RadInNode
  OutsideAirNode = VentSlab(Item)%OutsideAirNode
  AirRelNode     = VentSlab(Item)%AirReliefNode
  ZoneAirInNode  = VentSlab(Item)%ZoneAirInNode
  MixOut         = VentSlab(Item)%OAMixerOutNode

          ! First, set the flow conditions up so that there is flow through the ventilated
          ! slab system(this will be shut down if the system is not available or there
          ! is no load
  Node(InNode)%MassFlowRate                 = VentSlab(Item)%MaxAirMassFlow
  Node(InNode)%MassFlowRateMaxAvail         = VentSlab(Item)%MaxAirMassFlow
  Node(InNode)%MassFlowRateMinAvail         = VentSlab(Item)%MaxAirMassFlow
  Node(OutNode)%MassFlowRate                = VentSlab(Item)%MaxAirMassFlow
  Node(OutNode)%MassFlowRateMaxAvail        = VentSlab(Item)%MaxAirMassFlow
  Node(OutNode)%MassFlowRateMinAvail        = VentSlab(Item)%MaxAirMassFlow
  Node(OutsideAirNode)%MassFlowRate         = VentSlab(Item)%OutAirMassFlow
  Node(OutsideAirNode)%MassFlowRateMaxAvail = VentSlab(Item)%OutAirMassFlow
  Node(OutsideAirNode)%MassFlowRateMinAvail = VentSlab(Item)%OutAirMassFlow
  Node(AirRelNode)%MassFlowRate             = VentSlab(Item)%OutAirMassFlow
  Node(AirRelNode)%MassFlowRateMaxAvail     = VentSlab(Item)%OutAirMassFlow
  Node(AirRelNode)%MassFlowRateMinAvail     = VentSlab(Item)%OutAirMassFlow

          ! Initialize the relief air (same as inlet conditions to the Ventilated Slab ..
          ! Note that mass flow rates will be taken care of later.
  Node(AirRelNode) = Node(InNode)
  OAMassFlowRate   = 0.0d0

          ! Just in case the system is off and conditions do not get sent through
          ! the system for some reason, set the outlet conditions equal to the inlet
          ! conditions of the ventilated slab mixer
  Node(OutNode)%Temp     = Node(InNode)%Temp
  Node(OutNode)%Press    = Node(InNode)%Press
  Node(OutNode)%HumRat   = Node(InNode)%HumRat
  Node(OutNode)%Enthalpy = Node(InNode)%Enthalpy

          ! These initializations only need to be done once at the start of the iterations...
  IF (BeginTimeStepFlag.AND.FirstHVACIteration) THEN
          ! Initialize the outside air conditions...
    Node(OutsideAirNode)%Temp     = Node(OutsideAirNode)%OutAirDryBulb
    Node(OutsideAirNode)%HumRat   = OutHumRat
    Node(OutsideAirNode)%Press    = OutBaroPress

       ! The first pass through in a particular time step
       ZoneNum                       = VentSlab(Item)%ZonePtr
       ZeroSourceSumHATsurf(ZoneNum) = SumHATsurf(ZoneNum) ! Set this to figure what part of the load the radiant system meets
        DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces
          SurfNum                     = VentSlab(Item)%SurfacePtr(RadSurfNum)
          QRadSysSrcAvg(SurfNum)      = 0.0D0  ! Initialize this variable to zero (radiant system defaults to off)
          LastQRadSysSrc(SurfNum)     = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
          LastSysTimeElapsed(SurfNum) = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
          LastTimeStepSys(SurfNum)    = 0.0D0  ! At the start of a time step, reset to zero so average calculation can begin again
        END DO
  END IF


  RETURN

END SUBROUTINE InitVentilatedSlab

SUBROUTINE SizeVentilatedSlab(Item)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   June 2008
          !       MODIFIED       July 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Ventilated Slab components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays and plant sizing data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor
  USE WaterCoils,     ONLY: SetCoilDesFlow, GetCoilWaterInletNode, GetCoilWaterOutletNode
  USE SteamCoils,     ONLY: GetCoilSteamInletNode, GetCoilSteamOutletNode
  USE HVACHXAssistedCoolingCoil, ONLY: GetHXDXCoilName, GetHXCoilType
!  USE BranchInputManager, ONLY: MyPlantSizingIndex
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,       ONLY: PlantLoop, MyPlantSizingIndex
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: Item

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizHeatNum ! index of plant sizing object for 1st heating loop
  INTEGER             :: PltSizCoolNum ! index of plant sizing object for 1st cooling loop
  LOGICAL             :: ErrorsFound
  REAL(r64)           :: CoilInTemp
  REAL(r64)           :: CoilOutTemp
  REAL(r64)           :: CoilOutHumRat
  REAL(r64)           :: CoilInHumRat
  REAL(r64)           :: DesCoilLoad
  REAL(r64)           :: TempSteamIn
  REAL(r64)           :: EnthSteamInDry
  REAL(r64)           :: EnthSteamOutWet
  REAL(r64)           :: LatentHeatSteam
  REAL(r64)           :: SteamDensity
  INTEGER             :: CoilWaterInletNode=0
  INTEGER             :: CoilWaterOutletNode=0
  INTEGER             :: CoilSteamInletNode=0
  INTEGER             :: CoilSteamOutletNode=0
  CHARACTER(len=MaxNameLength) :: CoolingCoilName
  CHARACTER(len=MaxNameLength) :: CoolingCoilType
  REAL(r64)           :: rho
  REAL(r64)           :: Cp
  INTEGER             :: DummyWaterIndex = 1
  LOGICAL             :: IsAutosize              ! Indicator to autosize
  REAL(r64)           :: MaxAirVolFlowDes        ! Autosized maximum air flow for reporting
  REAL(r64)           :: MaxAirVolFlowUser       ! Hardsized maximum air flow for reporting
  REAL(r64)           :: OutAirVolFlowDes        ! Autosized outdoor air flow for reporting
  REAL(r64)           :: OutAirVolFlowUser       ! Hardsized outdoor air flow for reporting
  REAL(r64)           :: MinOutAirVolFlowDes     ! Autosized minimum outdoor air flow for reporting
  REAL(r64)           :: MinOutAirVolFlowUser    ! Hardsized minimum outdoor air flow for reporting
  REAL(r64)           :: MaxVolHotWaterFlowDes   ! Autosized maximum hot water flow for reporting
  REAL(r64)           :: MaxVolHotWaterFlowUser  ! Hardsized maximum hot water flow for reporting
  REAL(r64)           :: MaxVolHotSteamFlowDes   ! Autosized maximum hot steam flow for reporting
  REAL(r64)           :: MaxVolHotSteamFlowUser  ! Hardsized maximum hot steam flow for reporting
  REAL(r64)           :: MaxVolColdWaterFlowDes  ! Autosized maximum cold water flow for reporting
  REAL(r64)           :: MaxVolColdWaterFlowUser ! Hardsized maximum cold water flow for reporting

  PltSizCoolNum = 0
  PltSizHeatNum = 0
  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  MaxAirVolFlowDes = 0.0d0
  MaxAirVolFlowUser = 0.0d0
  OutAirVolFlowDes = 0.0d0
  OutAirVolFlowUser = 0.0d0
  MinOutAirVolFlowDes = 0.0d0
  MinOutAirVolFlowUser = 0.0d0
  MaxVolHotWaterFlowDes = 0.0d0
  MaxVolHotWaterFlowUser = 0.0d0
  MaxVolHotSteamFlowDes = 0.0d0
  MaxVolHotSteamFlowUser = 0.0d0
  MaxVolColdWaterFlowDes = 0.0d0
  MaxVolColdWaterFlowUser = 0.0d0

  IF (VentSlab(Item)%MaxAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (VentSlab(Item)%MaxAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                              'User-Specified Maximum Air Flow Rate [m3/s]', VentSlab(Item)%MaxAirVolFlow)
      END IF
    ELSE ! Autosize or hard-size with sizing run
      CALL CheckZoneSizing(cMO_VentilatedSlab, VentSlab(Item)%Name)
      MaxAirVolFlowDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                              FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxAirVolFlowDes < SmallAirVolFlow) THEN
        MaxAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        VentSlab(Item)%MaxAirVolFlow = MaxAirVolFlowDes
        CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                              'Design Size Maximum Air Flow Rate [m3/s]', MaxAirVolFlowDes)
      ELSE ! Hard-size with sizing data
        IF (VentSlab(Item)%MaxAirVolFlow > 0.0d0 .AND. MaxAirVolFlowDes > 0.0d0) THEN
          MaxAirVolFlowUser = VentSlab(Item)%MaxAirVolFlow
          CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                              'Design Size Maximum Air Flow Rate [m3/s]', MaxAirVolFlowDes, &
                              'User-Specified Maximum Air Flow Rate [m3/s]', MaxAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxAirVolFlowDes - MaxAirVolFlowUser)/MaxAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = " '// &
                                      TRIM(VentSlab(Item)%Name)//'".')
              CALL ShowContinueError('User-Specified Maximum Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Air Flow of ' // &
                                      TRIM(RoundSigDigits(MaxAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (VentSlab(Item)%OutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (VentSlab(Item)%OutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                              'User-Specified Maximum Outdoor Air Flow Rate [m3/s]', VentSlab(Item)%OutAirVolFlow)
      END IF
    ELSE ! Autosize or hard-size with sizing run
      CALL CheckZoneSizing(cMO_VentilatedSlab, VentSlab(Item)%Name)
      OutAirVolFlowDes = VentSlab(Item)%MaxAirVolFlow
      IF (IsAutosize) THEN
        VentSlab(Item)%OutAirVolFlow = OutAirVolFlowDes
        CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                              'Design Size Maximum Outdoor Air Flow Rate [m3/s]', OutAirVolFlowDes)
      ELSE
        IF (VentSlab(Item)%OutAirVolFlow > 0.0d0 .AND. OutAirVolFlowDes > 0.0d0) THEN
          OutAirVolFlowUser = VentSlab(Item)%OutAirVolFlow
          CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                              'Design Size Maximum Outdoor Air Flow Rate [m3/s]', OutAirVolFlowDes, &
                              'User-Specified Maximum Outdoor Air Flow Rate [m3/s]', OutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(OutAirVolFlowDes - OutAirVolFlowUser)/OutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = " '// &
                                      TRIM(VentSlab(Item)%Name)//'".')
              CALL ShowContinueError('User-Specified Maximum Outdoor Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(OutAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Outdoor Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(OutAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (VentSlab(Item)%MinOutAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (VentSlab(Item)%MinOutAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                              'User-Specified Minimum Outdoor Air Flow Rate [m3/s]',VentSlab(Item)%MinOutAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cMO_VentilatedSlab, VentSlab(Item)%Name)
      MinOutAirVolFlowDes = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA, &
                                                   VentSlab(Item)%MaxAirVolFlow)
      IF (MinOutAirVolFlowDes < SmallAirVolFlow) THEN
        MinOutAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        VentSlab(Item)%MinOutAirVolFlow = MinOutAirVolFlowDes
        CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                              'Design Size Minimum Outdoor Air Flow Rate [m3/s]',MinOutAirVolFlowDes)
      ELSE ! Hard-size with sizing data
        IF (VentSlab(Item)%MinOutAirVolFlow > 0.0d0 .AND. MinOutAirVolFlowDes > 0.0d0) THEN
          MinOutAirVolFlowUser = VentSlab(Item)%MinOutAirVolFlow
          CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                              'Design Size Minimum Outdoor Air Flow Rate [m3/s]',MinOutAirVolFlowDes, &
                              'User-Specified Minimum Outdoor Air Flow Rate [m3/s]',MinOutAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MinOutAirVolFlowDes - MinOutAirVolFlowUser)/MinOutAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = " '// &
                                      TRIM(VentSlab(Item)%Name)//'".')
              CALL ShowContinueError('User-Specified Minimum Outdoor Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MinOutAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Minimum Outdoor Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MinOutAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (VentSlab(Item)%MaxVolHotWaterFlow==AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (VentSlab(Item)%HCoilType == Heating_WaterCoilType) THEN

    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
        IF (VentSlab(Item)%MaxVolHotWaterFlow > 0.0d0) THEN
          CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                                  'User-Specified Maximum Hot Water Flow [m3/s]', VentSlab(Item)%MaxVolHotWaterFlow)
        END IF
      ELSE  ! Autosize or hard-size with sizing run
        CALL CheckZoneSizing(cMO_VentilatedSlab, VentSlab(Item)%Name)

        CoilWaterInletNode = GetCoilWaterInletNode('Coil:Heating:Water',VentSlab(Item)%HCoilName,ErrorsFound)
        CoilWaterOutletNode = GetCoilWaterOutletNode('Coil:Heating:Water',VentSlab(Item)%HCoilName,ErrorsFound)
        IF (IsAutosize ) THEN
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Water', VentSlab(Item)%HCoilName, CoilWaterInletNode, &
                                       CoilWaterOutletNode, ErrorsFound)
        !END IF
          IF (PltSizHeatNum > 0) THEN
            IF (FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallAirVolFlow) THEN
              CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
              CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
              CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
              DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                              * FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow &
                              * (CoilOutTemp-CoilInTemp)
              rho = GetDensityGlycol(PlantLoop(VentSlab(Item)%HWLoopNum )%fluidName, &
                                    60.d0, &
                                      PlantLoop( VentSlab(Item)%HWLoopNum )%fluidIndex, &
                                        'SizeVentilatedSlab' )

              Cp  = GetSpecificHeatGlycol(PlantLoop(VentSlab(Item)%HWLoopNum )%fluidName, &
                                    60.d0, &
                                      PlantLoop( VentSlab(Item)%HWLoopNum )%fluidIndex, &
                                        'SizeVentilatedSlab' )

              MaxVolHotWaterFlowDes = DesCoilLoad / &
                                    ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                    Cp * rho )
            ELSE
              MaxVolHotWaterFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in ' // cMO_VentilatedSlab // ' Object=' &
                               //TRIM(VentSlab(Item)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          VentSlab(Item)%MaxVolHotWaterFlow =  MaxVolHotWaterFlowDes
          CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                                  'Design Size Maximum Hot Water Flow [m3/s]', MaxVolHotWaterFlowDes)
        ELSE ! Hard-size with sizing data
          IF (VentSlab(Item)%MaxVolHotWaterFlow > 0.0d0 .AND. MaxVolHotWaterFlowDes > 0.0d0) THEN
            MaxVolHotWaterFlowUser = VentSlab(Item)%MaxVolHotWaterFlow
            CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                                  'Design Size Maximum Hot Water Flow [m3/s]', MaxVolHotWaterFlowDes, &
                                  'User-Specified Maximum Hot Water Flow [m3/s]', MaxVolHotWaterFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolHotWaterFlowDes -  MaxVolHotWaterFlowUser)/MaxVolHotWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = " '// &
                                      TRIM(VentSlab(Item)%Name)//'".')
                CALL ShowContinueError('User-Specified Maximum Hot Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolHotWaterFlowUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Hot Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolHotWaterFlowDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  ELSE
    VentSlab(Item)%MaxVolHotWaterFlow = 0.0d0
  END IF

  IsAutosize = .FALSE.
  IF (VentSlab(Item)%MaxVolHotSteamFlow==AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF(VentSlab(Item)%HCoilType == Heating_SteamCoilType) THEN

    IF (CurZoneEqNum > 0) THEN
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
        IF (VentSlab(Item)%MaxVolHotSteamFlow > 0.0d0) THEN
          CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                                'User-Specified Maximum Steam Flow [m3/s]', VentSlab(Item)%MaxVolHotSteamFlow)
        END IF
      ELSE  ! Autosize or hard-size with sizing run
        CALL CheckZoneSizing('ZoneHVAC:VentilatedSlab', VentSlab(Item)%Name)

        CoilSteamInletNode = GetCoilSteamInletNode('Coil:Heating:Steam',VentSlab(Item)%HCoilName,ErrorsFound)
        CoilSteamOutletNode = GetCoilSteamOutletNode('Coil:Heating:Steam',VentSlab(Item)%HCoilName,ErrorsFound)
        IF (IsAutosize ) THEN
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Steam', VentSlab(Item)%HCoilName, CoilSteamInletNode, &
                                       CoilSteamOutletNode, ErrorsFound)
          IF (PltSizHeatNum > 0) THEN
            IF (FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallAirVolFlow) THEN
              CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
              CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
              CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
              DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                              * FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow &
                              * (CoilOutTemp-CoilInTemp)

              TempSteamIn= 100.00d0
              EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,VentSlab(Item)%HCoil_FluidIndex,'SizeVentilatedSlab')
              EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,VentSlab(Item)%HCoil_FluidIndex,'SizeVentilatedSlab')
              LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet
              SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,VentSlab(Item)%HCoil_FluidIndex,'SizeVentilatedSlab')
              Cp = GetSpecificHeatGlycol('WATER', 60.d0, DummyWaterIndex, 'SizeVentilatedSlab')
              rho = GetDensityGlycol('WATER', 60.d0, DummyWaterIndex, 'SizeVentilatedSlab')
              MaxVolHotSteamFlowDes = DesCoilLoad/((PlantSizData(PltSizHeatNum)%DeltaT * &
                                    Cp  * rho )+ &
                                    SteamDensity* LatentHeatSteam)
            ELSE
              MaxVolHotSteamFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of Steam flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in ' // 'ZoneHVAC:VentilatedSlab' // ' Object=' &
                               //TRIM(VentSlab(Item)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          VentSlab(Item)%MaxVolHotSteamFlow = MaxVolHotSteamFlowDes
          CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                                  'Design Size Maximum Steam Flow [m3/s]', MaxVolHotSteamFlowDes)
        ELSE
          IF (VentSlab(Item)%MaxVolHotSteamFlow > 0.0d0 .AND. MaxVolHotSteamFlowDes > 0.0d0) THEN
            MaxVolHotSteamFlowUser = VentSlab(Item)%MaxVolHotSteamFlow
            CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                                  'Design Size Maximum Steam Flow [m3/s]', MaxVolHotSteamFlowDes, &
                                  'User-Specified Maximum Steam Flow [m3/s]', MaxVolHotSteamFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolHotSteamFlowDes -  MaxVolHotSteamFlowUser)/MaxVolHotSteamFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = " '// &
                                      TRIM(VentSlab(Item)%Name)//'".')
                CALL ShowContinueError('User-Specified Maximum Steam Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolHotSteamFlowUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Steam Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolHotSteamFlowDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  ELSE
    VentSlab(Item)%MaxVolHotSteamFlow = 0.0d0
  END IF

  IsAutosize = .FALSE.
  IF (VentSlab(Item)%MaxVolColdWaterFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (VentSlab(Item)%MaxVolColdWaterFlow > 0.0d0) THEN
        CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                            'User-Specified Maximum Cold Water Flow [m3/s]', VentSlab(Item)%MaxVolColdWaterFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(cMO_VentilatedSlab, VentSlab(Item)%Name)
      IF (VentSlab(Item)%CCoilType == Cooling_CoilHXAssisted) THEN
        CoolingCoilName = GetHXDXCoilName(VentSlab(Item)%CCoilTypeCh,VentSlab(Item)%CCoilName,ErrorsFound)
        CoolingCoilType = GetHXCoilType(VentSlab(Item)%CCoilTypeCh,VentSlab(Item)%CCoilName,ErrorsFound)
      ELSE
        CoolingCoilName = VentSlab(Item)%CCoilName
        CoolingCoilType = VentSlab(Item)%CCoilTypeCh
      END IF
      CoilWaterInletNode = GetCoilWaterInletNode(CoolingCoilType,CoolingCoilName,ErrorsFound)
      CoilWaterOutletNode = GetCoilWaterOutletNode(CoolingCoilType,CoolingCoilName,ErrorsFound)
      IF (IsAutosize) THEN
        PltSizCoolNum = MyPlantSizingIndex(CoolingCoilType,CoolingCoilName, CoilWaterInletNode, &
                                         CoilWaterOutletNode, ErrorsFound)
        IF (PltSizCoolNum > 0) THEN
          IF (FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow >= SmallAirVolFlow) THEN
            CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
            CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
            CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
            CoilInHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
            DesCoilLoad = FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow &
                          * (PsyHFnTdbW(CoilInTemp, CoilInHumRat)-PsyHFnTdbW(CoilOutTemp, CoilOutHumRat))
            rho = GetDensityGlycol(PlantLoop(VentSlab(Item)%CWLoopNum )%fluidName, &
                                    5.d0, &
                                      PlantLoop( VentSlab(Item)%CWLoopNum )%fluidIndex, &
                                        'SizeVentilatedSlab' )

            Cp  = GetSpecificHeatGlycol(PlantLoop(VentSlab(Item)%CWLoopNum )%fluidName, &
                                    5.d0, &
                                      PlantLoop( VentSlab(Item)%CWLoopNum )%fluidIndex, &
                                        'SizeVentilatedSlab' )

            MaxVolColdWaterFlowDes = DesCoilLoad / &
                                   ( PlantSizData(PltSizCoolNum)%DeltaT * &
                                   Cp  * rho )
          ELSE
            MaxVolColdWaterFlowDes = 0.0d0
          END IF
        ELSE
          CALL ShowSevereError('Autosizing of water flow requires a cooling loop Sizing:Plant object')
          CALL ShowContinueError('Occurs in ' // cMO_VentilatedSlab // ' Object=' &
                               //TRIM(VentSlab(Item)%Name))
          Errorsfound = .TRUE.
        END IF
      END IF
      IF (IsAutosize) THEN
        VentSlab(Item)%MaxVolColdWaterFlow = MaxVolColdWaterFlowDes
        CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                                'Design Size Maximum Cold Water Flow [m3/s]', MaxVolColdWaterFlowDes)
      ELSE
        IF (VentSlab(Item)%MaxVolColdWaterFlow > 0.0d0 .AND. MaxVolColdWaterFlowDes > 0.0d0) THEN
          MaxVolColdWaterFlowUser = VentSlab(Item)%MaxVolColdWaterFlow
          CALL ReportSizingOutput(cMO_VentilatedSlab, VentSlab(Item)%Name, &
                                'Design Size Maximum Cold Water Flow [m3/s]', MaxVolColdWaterFlowDes, &
                                'User-Specified Maximum Cold Water Flow [m3/s]', MaxVolColdWaterFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxVolColdWaterFlowDes -  MaxVolColdWaterFlowUser)/MaxVolColdWaterFlowUser) >AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeVentilatedSlab: Potential issue with equipment sizing for ZoneHVAC:VentilatedSlab = " '// &
                                      TRIM(VentSlab(Item)%Name)//'".')
              CALL ShowContinueError('User-Specified Maximum Cold Water Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolColdWaterFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Cold Water Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolColdWaterFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IF (VentSlab(Item)%CCoilType == Cooling_CoilHXAssisted) THEN
    CoolingCoilName = GetHXDXCoilName(VentSlab(Item)%CCoilTypeCh,VentSlab(Item)%CCoilName,ErrorsFound)
    CoolingCoilType = GetHXCoilType(VentSlab(Item)%CCoilTypeCh,VentSlab(Item)%CCoilName,ErrorsFound)
  ELSE
    CoolingCoilName = VentSlab(Item)%CCoilName
    CoolingCoilType = VentSlab(Item)%CCoilTypeCh
  END IF
  CALL SetCoilDesFlow(CoolingCoilType,CoolingCoilName,VentSlab(Item)%MaxAirVolFlow,&
                       ErrorsFound)
  CALL SetCoilDesFlow(VentSlab(Item)%HCoilTypeCh,VentSlab(Item)%HCoilName,VentSlab(Item)%MaxAirVolFlow,&
                       ErrorsFound)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeVentilatedSlab

SUBROUTINE CalcVentilatedSlab(Item,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   June 2008
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine mainly controls the action of the Ventilated Slab
          ! (or more exactly, it controls the amount of outside air brought in)
          ! based on the user input for controls and the defined controls
          ! algorithms.

          ! METHODOLOGY EMPLOYED:
          ! Ventilated slab is controlled based on user input and what is happening in the
          ! simulation.  There are various cases to consider:
          ! 1. OFF: Unit is schedule off or there is no load on it.  All flow
          !    rates are set to zero and the temperatures are set to zone conditions
          !    (except for the outside air inlet).
          ! 2. HEATING/VARIABLE PERCENT: The unit is on, there is a heating load,
          !    and variable percent control is specified.  The outside air fraction
          !    is set to the minimum outside air fraction (schedule based) and the
          !    heating coil is activated.
          ! 3. HEATING/FIXED TEMPERATURE: The unit is on, there is a heating load,
          !    and fixed temperature control is specified.  The outside air fraction
          !    is varied in an attempt to obtain a mixed air temperature equal to
          !    the user specified temperature (schedule based).  The heating coil
          !    is activated, if necessary.
          ! 4. COOLING/NO COIL: The unit is on, there is a cooling load, and no
          !    coil is present or it has been scheduled off.  Set the amount of
          !    outside air based on the control type.  Simulate the "mixing box".
          ! 5. COOLING/WITH COIL: The unit is on, there is a cooling load, and
          !    a cooling coil is present and scheduled on.  Tries to use outside
          !    air as best as possible and then calls a cooling coil
          ! Note: controls are strictly temperature based and do not factor
          ! humidity into the equation (not an enthalpy economy cycle but rather
          ! a simple return air economy cycle).  In addition, temperature predictions
          ! are not strict energy balances here in the control routine though
          ! in the mixing routine an energy balance is preserved.


          ! REFERENCES:
          ! ASHRAE Systems and Equipment Handbook (SI), 1996. page 31.3

          ! USE STATEMENTS:


  USE DataZoneEnergyDemands
  USE DataEnvironment,           ONLY : OutDryBulbTemp, OutWetBulbTemp, EnvironmentName, CurMnDy, OutBaroPress
  USE DataHeatBalance,           ONLY : MRT
  USE DataHeatBalFanSys,         ONLY : MAT,ZoneAirHumRat
  USE DataHVACGlobals,           ONLY : SmallLoad, ZoneCompTurnFansOn, ZoneCompTurnFansOff
  USE DataLoopNode,              ONLY : Node
  USE ScheduleManager,           ONLY : GetCurrentScheduleValue
  USE HeatingCoils,              ONLY : CheckHeatingCoilSchedule
  USE WaterCoils,                ONLY : CheckWaterCoilSchedule
  USE HVACHXAssistedCoolingCoil, ONLY : CheckHXAssistedCoolingCoilSchedule
  Use SteamCoils,                ONLY : CheckSteamCoilSchedule
  USE General,                   ONLY : TrimSigDigits
  USE Fans,                      ONLY : SimulateFanComponents !12/18
  USE DataHeatBalSurface,        ONLY : TH
  USE NodeInputManager,          ONLY : GetOnlySingleNode
  USE DataInterfaces,            ONLY : ControlCompOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(INOUT)      :: Item               ! number of the current ventilated slab being simulated
  INTEGER, INTENT(IN)         :: ZoneNum            ! number of zone being served
  LOGICAL, INTENT(IN)         :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64), INTENT(OUT)      :: PowerMet           ! power supplied (W)
  REAL(r64), INTENT(OUT)      :: LatOutputProvided  ! latent capacity supplied (kg/s)


          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), PARAMETER :: LowTempDiff = 0.1d0      ! Smallest allowed temperature difference for comparisons
                                                   ! (below this value the temperatures are assumed equal)
  REAL(r64), PARAMETER :: LowOAFracDiff = 0.01d0   ! Smallest allowed outside air fraction difference for comparison
                                                   ! (below this value the fractions are assumed equal)
  REAL(r64), PARAMETER :: MinFlowAllowed = 0.001d0 ! lowest air flow rate allowed [kg/sec]

          ! INTERFACE BLOCK SPECIFICATIONS
          ! see use DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: AirMassFlow      ! air mass flow rate [kg/sec]
  INTEGER      :: AirRelNode       ! outside air relief node
  INTEGER      :: ControlNode      ! the hot water or cold water inlet node
  INTEGER      :: InletNode        ! system air inlet node
  INTEGER      :: FanOutletNode    ! system fan outlet node
  INTEGER      :: ZoneAirInNode    ! zone supply air node
  REAL(r64)    :: MaxOAFrac        ! maximum possible outside air fraction
  REAL(r64)    :: MaxWaterFlow     ! maximum water flow for heating or cooling [kg/sec]
  REAL(r64)    :: MinOAFrac        ! minimum possible outside air fraction
  REAL(r64)    :: MinWaterFlow     ! minimum water flow for heating or cooling [kg/sec]
  INTEGER      :: OutletNode       ! air outlet node
  INTEGER      :: OutsideAirNode   ! outside air node
  REAL(r64)    :: QTotUnitOut      ! total unit output [watts]
  REAL(r64)    :: QUnitOut         ! heating or sens. cooling provided by fan coil unit [watts]
  REAL(r64)    :: LatentOutput     ! Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
  REAL(r64)    :: SpecHumOut       ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
  REAL(r64)    :: SpecHumIn        ! Specific humidity ratio of inlet air (kg moisture / kg moist air)
  REAL(r64)    :: Tdesired         ! desired temperature after mixing inlet and outdoor air [degrees C]
  REAL(r64)    :: Tinlet           ! temperature of air coming into the ventilated slab [degrees C]
  REAL(r64)    :: Toutdoor         ! temperature of outdoor air being introduced into the ventilated slab [degrees C]
  Real(r64)    :: MaxSteamFlow
  Real(r64)    :: MinSteamFlow
  REAL(r64)    :: RadInTemp        ! "Desired" radiant system air inlet temperature [Celsius]**setpoint
  REAL(r64)    :: SetpointTemp     ! temperature that will be used to control the radiant system [Celsius]
  REAL(r64)    :: SetpointTempHi   ! Current high point in setpoint temperature range
  REAL(r64)    :: SetpointTempLo   ! Current low point in setpoint temperature range
  REAL(r64)    :: AirTempHi        ! Current high point in water temperature range
  REAL(r64)    :: AirTempLo        ! Current low point in water temperature range
  REAL(r64)    :: AirTempHeatHi    ! Current high point in water temperature range
  REAL(r64)    :: AirTempCoolLo    ! Current low point in water temperature range
  REAL(r64)    :: CpFan            ! Intermediate calculational variable for specific heat of air <<NOV9 Updated
  REAL(r64)    :: ZoneRadNum       ! number of zone being served *********************
  REAL(r64)    :: QZnReq
  INTEGER      :: RadSurfNum       ! DO loop counter for the surfaces that comprise a particular radiant system
  CHARACTER(len=MaxNameLength) ::MSlabIn
  CHARACTER(len=MaxNameLength) ::MSlabOut
  CHARACTER(len=MaxNameLength) ::SlabName
  INTEGER   :: MSlabInletNode
  INTEGER   :: MSlabOutletNode
  LOGICAL    :: ErrorsFound=.false. ! Set to true if errors in input, fatal at end of routine
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='ZoneHVAC:VentilatedSlab'


 SELECT CASE (VentSlab(Item)%CoilOption)
 CASE (BothOption)

  SELECT CASE(VentSlab(Item)%HCoilType)

    CASE(Heating_WaterCoilType)
      CALL CheckWaterCoilSchedule('Coil:Heating:Water',VentSlab(Item)%HCoilName,  &
                                  VentSlab(Item)%HCoilSchedValue,VentSlab(Item)%HCoil_Index)
    CASE(Heating_SteamCoilType)
      CALL CheckSteamCoilSchedule('Coil:Heating:Steam',VentSlab(Item)%HCoilName,  &
                                  VentSlab(Item)%HCoilSchedValue,VentSlab(Item)%HCoil_Index)
    CASE(Heating_ElectricCoilType)
      CALL CheckHeatingCoilSchedule('Coil:Heating:Electric',VentSlab(Item)%HCoilName,  &
                                    VentSlab(Item)%HCoilSchedValue,VentSlab(Item)%HCoil_Index)
    CASE(Heating_GasCoilType)
      CALL CheckHeatingCoilSchedule('Coil:Heating:Gas',VentSlab(Item)%HCoilName,  &
                                    VentSlab(Item)%HCoilSchedValue,VentSlab(Item)%HCoil_Index)
    CASE DEFAULT
  END SELECT


  SELECT CASE(VentSlab(Item)%CCoilType)

    CASE(Cooling_CoilWaterCooling)
      CALL CheckWaterCoilSchedule('Coil:Cooling:Water',VentSlab(Item)%CCoilName,  &
                                  VentSlab(Item)%CCoilSchedValue,VentSlab(Item)%CCoil_Index)
    CASE(Cooling_CoilDetailedCooling)
      CALL CheckWaterCoilSchedule('Coil:Cooling:Water:DetailedGeometry',VentSlab(Item)%CCoilName,  &
                                  VentSlab(Item)%CCoilSchedValue,VentSlab(Item)%CCoil_Index)
    CASE(Cooling_CoilHXAssisted)
      CALL CheckHXAssistedCoolingCoilSchedule('CoilSystem:Cooling:Water:HeatExchangerAssisted',VentSlab(Item)%CCoilName,  &
                                    VentSlab(Item)%CCoilSchedValue,VentSlab(Item)%CCoil_Index)
    CASE DEFAULT
  END SELECT

 CASE (HeatingOption)

    SELECT CASE(VentSlab(Item)%HCoilType)

    CASE(Heating_WaterCoilType)
      CALL CheckWaterCoilSchedule('Coil:Heating:Water',VentSlab(Item)%HCoilName,  &
                                  VentSlab(Item)%HCoilSchedValue,VentSlab(Item)%HCoil_Index)
    CASE(Heating_SteamCoilType)
      CALL CheckSteamCoilSchedule('Coil:Heating:Steam',VentSlab(Item)%HCoilName,  &
                                  VentSlab(Item)%HCoilSchedValue,VentSlab(Item)%HCoil_Index)
    CASE(Heating_ElectricCoilType)
      CALL CheckHeatingCoilSchedule('Coil:Heating:Electric',VentSlab(Item)%HCoilName,  &
                                    VentSlab(Item)%HCoilSchedValue,VentSlab(Item)%HCoil_Index)
    CASE(Heating_GasCoilType)
      CALL CheckHeatingCoilSchedule('Coil:Heating:Gas',VentSlab(Item)%HCoilName,  &
                                    VentSlab(Item)%HCoilSchedValue,VentSlab(Item)%HCoil_Index)
    CASE DEFAULT
   END SELECT

 CASE (CoolingOption)

  SELECT CASE(VentSlab(Item)%CCoilType)

    CASE(Cooling_CoilWaterCooling)
      CALL CheckWaterCoilSchedule('Coil:Cooling:Water',VentSlab(Item)%CCoilName,  &
                                  VentSlab(Item)%CCoilSchedValue,VentSlab(Item)%CCoil_Index)
    CASE(Cooling_CoilDetailedCooling)
      CALL CheckWaterCoilSchedule('Coil:Cooling:Water:DetailedGeometry',VentSlab(Item)%CCoilName,  &
                                  VentSlab(Item)%CCoilSchedValue,VentSlab(Item)%CCoil_Index)
    CASE(Cooling_CoilHXAssisted)
      CALL CheckHXAssistedCoolingCoilSchedule('CoilSystem:Cooling:Water:HeatExchangerAssisted',VentSlab(Item)%CCoilName,  &
                                    VentSlab(Item)%CCoilSchedValue,VentSlab(Item)%CCoil_Index)
    CASE DEFAULT

  END SELECT

 CASE (NoneOption)

 END SELECT

          ! FLOW:

  FanElecPower   = 0.0D0
          ! initialize local variables
  ControlNode    = 0
  QUnitOut       = 0.0D0
  LatentOutput   = 0.0D0
  MaxWaterFlow   = 0.0D0
  MinWaterFlow   = 0.0D0
  InletNode      = VentSlab(Item)%ReturnAirNode
  OutletNode     = VentSlab(Item)%RadInNode
  FanOutletNode  = VentSlab(Item)%FanOutletNode
  ZoneAirInNode  = VentSlab(Item)%ZoneAirInNode
  OutsideAirNode = VentSlab(Item)%OutsideAirNode
  AirRelNode     = VentSlab(Item)%AirReliefNode
  ZoneRadNum     = VentSlab(Item)%ZonePtr
  RadSurfNum     = VentSlab(Item)%NumOfSurfaces
  Tinlet         = Node(InletNode)%Temp
  Toutdoor       = Node(OutsideAirNode)%Temp

  ! Control Type Check
SELECT CASE (VentSlab(Item)%ControlType)
      CASE (MATControl)
        SetpointTemp = MAT(ZoneNum)
      CASE (MRTControl)
        SetpointTemp = MRT(ZoneNum)
      CASE (OPTControl)
        SetpointTemp = 0.5d0*(MAT(ZoneNum)+MRT(ZoneNum))
      CASE (ODBControl)
        SetpointTemp = OutDryBulbTemp
      CASE (OWBControl)
        SetpointTemp = OutWetBulbTemp
      CASE (SURControl)
        SetpointTemp = TH(VentSlab(Item)%SurfacePtr(RadSurfNum),1,2)
      CASE (DPTZControl)
        SetpointTemp = PsyTdpFnWPb(ZoneAirHumRat(VentSlab(Item)%ZonePtr),OutBaroPress)

      CASE DEFAULT    ! Should never get here
        CALL ShowSevereError('Illegal control type in low temperature radiant system: '//TRIM(VentSlab(Item)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')
    END SELECT

           ! Load Check

        AirTempHeatHi    = GetCurrentScheduleValue(VentSlab(Item)%HotCtrlHiTempSchedPtr)
        AirTempCoolLo    = GetCurrentScheduleValue(VentSlab(Item)%ColdCtrlLoTempSchedPtr)

IF (((SetpointTemp >= AirTempHeatHi) .AND. (SetpointTemp <= AirTempCoolLo)) .OR. &
     (GetCurrentScheduleValue(VentSlab(Item)%SchedPtr) <= 0) ) THEN

          ! System is off or has no load upon it; set the flow rates to zero and then
          ! simulate the components with the no flow conditions
    Node(InletNode)%MassFlowRate              = 0.0d0
    Node(InletNode)%MassFlowRateMaxAvail      = 0.0d0
    Node(InletNode)%MassFlowRateMinAvail      = 0.0d0
    Node(OutletNode)%MassFlowRate             = 0.0d0
    Node(OutletNode)%MassFlowRateMaxAvail     = 0.0d0
    Node(OutletNode)%MassFlowRateMinAvail     = 0.0d0
    Node(OutsideAirNode)%MassFlowRate         = 0.0d0
    Node(OutsideAirNode)%MassFlowRateMaxAvail = 0.0d0
    Node(OutsideAirNode)%MassFlowRateMinAvail = 0.0d0
    Node(AirRelNode)%MassFlowRate             = 0.0d0
    Node(AirRelNode)%MassFlowRateMaxAvail     = 0.0d0
    Node(AirRelNode)%MassFlowRateMinAvail     = 0.0d0
    AirMassFlow                               = Node(FanOutletNode)%MassFlowRate
    HCoilOn                                   = .FALSE.

! Node condition
    Node(InletNode)%Temp = TH(VentSlab(Item)%SurfacePtr(1),1,2)
    Node(FanOutletNode)%Temp = Node(InletNode)%Temp
    Node(OutletNode)%Temp    = Node(FanOutletNode)%Temp

! Node condition
  IF (VentSlab(Item)%SysConfg == SeriesSlabs) THEN
     DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces
       SlabName=VentSlab(Item)%SurfaceName(RadSurfNum)
       MSlabIn = VentSlab(Item)%SlabIn(RadSurfNum)
       MSlabOut = VentSlab(Item)%SlabOut(RadSurfNum)
          VentSlab(Item)%MslabInNode = &
               GetOnlySingleNode(MSlabIn,ErrorsFound,CurrentModuleObject,SlabName, &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsNotParent)
          VentSlab(Item)%MSlabOutNode = &
               GetOnlySingleNode(MSlabOut,ErrorsFound,CurrentModuleObject,SlabName, &
                           NodeType_Air,NodeConnectionType_Internal,1,ObjectIsNotParent)
        MSlabInletNode      = VentSlab(Item)%MslabInNode
        MSlabOutletNode    = VentSlab(Item)%MslabOutNode

        Node(MSlabInletNode)%Temp = Node(InletNode)%Temp
        Node(MSlabOutletNode)%Temp = Node(MSlabInletNode)%Temp
    END DO
   END IF

    CALL CalcVentilatedSlabComps(Item,FirstHVACIteration,QUnitOut)


Else ! System On

 IF (SetpointTemp< AirTempHeatHi) THEN  ! HEATING MODE
      OperatingMode = HeatingMode

     !Check the setpoint and temperature span
        SetpointTempHi = GetCurrentScheduleValue(VentSlab(Item)%HotCtrlHiTempSchedPtr)
        SetpointTempLo = GetCurrentScheduleValue(VentSlab(Item)%HotCtrlLoTempSchedPtr)
        IF (SetpointTempHi < SetpointTempLo) THEN
          CALL ShowSevereError('Heating setpoint temperature mismatch in'//TRIM(VentSlab(Item)%Name))
          CALL ShowContinueError('High setpoint temperature is less than low setpoint temperature--check your schedule input')
          CALL ShowFatalError('Preceding condition causes termination.')
        END IF
        AirTempHi    = GetCurrentScheduleValue(VentSlab(Item)%HotAirHiTempSchedPtr)
        AirTempLo    = GetCurrentScheduleValue(VentSlab(Item)%HotAirLoTempSchedPtr)

        IF (AirTempHi < AirTempLo) THEN
          CALL ShowSevereError('Heating Air temperature mismatch in'//TRIM(VentSlab(Item)%Name))
          CALL ShowContinueError('High Air temperature is less than low Air temperature--check your schedule input')
          CALL ShowFatalError('Preceding condition causes termination.')
        END IF

        IF (SetpointTemp >= SetpointTempHi) THEN
          ! System is above high heating setpoint so we should be able to turn the system off
          RadInTemp = AirTempLo

        ELSE IF (SetpointTemp <= SetpointTempLo) THEN
          ! System is running with its highest inlet temperature
          RadInTemp = AirTempHi
        ELSE
          ! Interpolate to obtain the current radiant system inlet temperature
          RadInTemp = AirTempHi - (AirTempHi - AirTempLo)*(SetpointTemp - SetpointTempLo)/(SetpointTempHi - SetpointTempLo)
        END IF

      Node(VentSlab(Item)%RadInNode)%Temp = RadInTemp

      ControlNode   = VentSlab(Item)%HotControlNode
      MaxWaterFlow  = VentSlab(Item)%MaxHotWaterFlow
      MinWaterFlow  = VentSlab(Item)%MinHotWaterFlow
      MaxSteamFlow  = VentSlab(Item)%MaxHotSteamFlow
      MinSteamFlow  = VentSlab(Item)%MinHotSteamFlow

! On the first HVAC iteration the system values are given to the controller, but after that
! the demand limits are in place and there needs to be feedback to the Zone Equipment

       IF(.not. FirstHVACIteration .and. VentSlab(Item)%HCoilType == Heating_WaterCoilType) THEN
         MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
         MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
       End IF

       IF(.not. FirstHVACIteration .and. VentSlab(Item)%HCoilType == Heating_SteamCoilType) THEN
         MaxSteamFlow = Node(ControlNode)%MassFlowRateMaxAvail
         MinSteamFlow = Node(ControlNode)%MassFlowRateMinAvail
       End IF

       HCoilOn       = .TRUE.

       IF(Node(OutsideAirNode)%MassFlowRate > 0.0d0) Then
          MinOAFrac = GetCurrentScheduleValue(VentSlab(Item)%MinOASchedPtr) * &
                    (VentSlab(Item)%MinOutAirMassFlow / Node(OutsideAirNode)%MassFlowRate)
       ELSE
         MinOAFrac = 0.0d0
       End IF

       MinOAFrac = MIN(1.0d0,MAX(0.0d0,MinOAFrac))

      IF ((.NOT.VentSlab(Item)%HCoilPresent) .OR. &
           (VentSlab(Item)%HCoilSchedValue <= 0.0d0) ) THEN
          ! In heating mode, but there is no coil to provide heating.  This is handled
          ! differently than if there was a heating coil present.  Fixed temperature
          ! will still try to vary the amount of outside air to meet the desired
          ! mixed air temperature, while variable percent will go to full ventilation
          ! when it is most advantageous.

          ! If there are no coil, Slab In Node is assumed to be Fan Outlet Node

          OutletNode   = FanOutletNode

    SELECT CASE (VentSlab(Item)%OAControlType)

      CASE (FixedOAControl)
      ! In this control type, the outdoor air flow rate is fixed to the maximum value
      ! which is equal to the minimum value, regardless of all the other conditions.
        OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

      CASE (VariablePercent)
      ! This algorithm is probably a bit simplistic in that it just bounces
      ! back and forth between the maximum outside air and the minimum.  In
      ! reality, a system *might* vary between the two based on the load in
      ! the zone.  This simple flow control might cause some overcooling but
      ! chances are that if there is a cooling load and the zone temperature
      ! gets above the outside temperature that overcooling won't be significant.
        Tinlet    = Node(InletNode)%Temp
        Toutdoor  = Node(OutsideAirNode)%Temp

        IF (Tinlet >= Toutdoor) THEN

          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

        ELSE ! Tinlet < Toutdoor

          MaxOAFrac = GetCurrentScheduleValue(VentSlab(Item)%MaxOASchedPtr)
          OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate

        END IF

      CASE (FixedTemperature)
      ! This is basically the same algorithm as for the heating case...
        Tdesired  = GetCurrentScheduleValue(VentSlab(Item)%TempSchedPtr)
        MaxOAFrac = 1.0d0

        IF (ABS(Tinlet-Toutdoor) <= LowTempDiff) THEN ! no difference in indoor and outdoor conditions-->set OA to minimum
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
        ELSE IF (ABS(MaxOAFrac-MinOAFrac) <= LowOAFracDiff) THEN  ! no difference in outside air fractions
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
        ELSE IF ( ( (Tdesired <= Tinlet) .AND. (Tdesired >= Toutdoor) ) .OR. &
                  ( (Tdesired >= Tinlet) .AND. (Tdesired <= Toutdoor) ) ) THEN
            ! Desired temperature is between the inlet and outdoor temperatures
            ! so vary the flow rate between no outside air and no recirculation air
            ! then applying the maximum and minimum limits the user has scheduled
            ! to make sure too much/little outside air is being introduced
            OAMassFlowRate = ((Tdesired - Tinlet)/(Toutdoor - Tinlet))*Node(InletNode)%MassFlowRate
            OAMassFlowRate = MAX(OAMassFlowRate,(MinOAFrac*Node(OutsideAirNode)%MassFlowRate))
            OAMassFlowRate = MIN(OAMassFlowRate,(MaxOAFrac*Node(OutsideAirNode)%MassFlowRate))
        ELSE IF ( (Tdesired < Tinlet) .AND. (Tdesired < Toutdoor) ) THEN
            ! Desired temperature is below both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet < Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
        ELSE IF ( (Tdesired > Tinlet) .AND. (Tdesired > Toutdoor) ) THEN
            ! Desired temperature is above both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet > Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE
            ! It should NEVER get to this point, but just in case...
            CALL ShowFatalError('Ventilated Slab simulation control: illogical condition for '//TRIM(VentSlab(Item)%Name))
          END IF

     END SELECT

     CALL CalcVentilatedSlabComps(Item,FirstHVACIteration,QUnitOUt)

      ElSE     ! Heating Coil present

    SELECT CASE (VentSlab(Item)%OAControlType)

      CASE (FixedOAControl)
              ! In this control type, the outdoor air flow rate is fixed to the maximum value
              ! which is equal to the minimum value, regardless of all the other conditions.
         If(Node(OutsideAirNode)%MassFlowRate > 0.0d0) Then
            MaxOAFrac = GetCurrentScheduleValue(VentSlab(Item)%MaxOASchedPtr)
          Else
            MaxOAFrac = 0.0d0
          End If
          MaxOAFrac = MIN(1.0d0,MAX(0.0d0,MinOAFrac))
          OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate

      CASE (VariablePercent)
              ! In heating mode, the ouside air for "variable percent" control
              ! is set to the minimum value


        OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

      CASE (FixedTemperature)
              ! This is basically the same algorithm as for the heating case...
        Tdesired  = GetCurrentScheduleValue(VentSlab(Item)%TempSchedPtr)
        MaxOAFrac = 1.0d0

        IF (ABS(Tinlet-Toutdoor) <= LowTempDiff) THEN ! no difference in indoor and outdoor conditions-->set OA to minimum
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
        ELSE IF (ABS(MaxOAFrac-MinOAFrac) <= LowOAFracDiff) THEN  ! no difference in outside air fractions
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
        ELSE IF ( ( (Tdesired <= Tinlet) .AND. (Tdesired >= Toutdoor) ) .OR. &
                    ( (Tdesired >= Tinlet) .AND. (Tdesired <= Toutdoor) ) ) THEN
            ! Desired temperature is between the inlet and outdoor temperatures
            ! so vary the flow rate between no outside air and no recirculation air
            ! then applying the maximum and minimum limits the user has scheduled
            ! to make sure too much/little outside air is being introduced
            OAMassFlowRate = ((Tdesired - Tinlet)/(Toutdoor - Tinlet))*Node(InletNode)%MassFlowRate
            OAMassFlowRate = MAX(OAMassFlowRate,(MinOAFrac*Node(OutsideAirNode)%MassFlowRate))
            OAMassFlowRate = MIN(OAMassFlowRate,(MaxOAFrac*Node(OutsideAirNode)%MassFlowRate))
        ELSE IF ( (Tdesired < Tinlet) .AND. (Tdesired < Toutdoor) ) THEN
            ! Desired temperature is below both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet < Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
        ELSE IF ( (Tdesired > Tinlet) .AND. (Tdesired > Toutdoor) ) THEN
            ! Desired temperature is above both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet > Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE
            ! It should NEVER get to this point, but just in case...
            CALL ShowFatalError('Ventilated Slab simulation control: illogical condition for '//TRIM(VentSlab(Item)%Name))
          END IF

     END SELECT

     CALL SimVentSlabOAMixer(Item)
     CALL SimulateFanComponents(VentSlab(Item)%FanName,FirstHVACIteration,VentSlab(Item)%Fan_Index, &
                                ZoneCompTurnFansOn = ZoneCompTurnFansOn,ZoneCompTurnFansOff = ZoneCompTurnFansOff)
     CpFan     = PsyCpAirFnWTdb(Node(FanOutletNode)%HumRat,Node(FanOutletNode)%Temp)
     QZnReq    = (Node(OutletNode)%MassFlowRate)*CpFan*(RadInTemp-Node(FanOutletNode)%Temp)

             ! Setup the coil configuration
      SELECT CASE (VentSlab(Item)%HCoilType)

        CASE (Heating_WaterCoilType)
          ! control water flow to obtain output matching QZnReq

        CALL ControlCompOutput(CompName=VentSlab(Item)%Name,CompType=cMO_VentilatedSlab,CompNum=Item, &
                                 FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                                 ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                                 MinFlow=MinWaterFlow,ControlOffSet=0.001d0, &
                                 ControlCompTypeNum=VentSlab(Item)%ControlCompTypeNum, &
                                 CompErrIndex=VentSlab(Item)%CompErrIndex,  &
                                 LoopNum     = VentSlab(Item)%HWLoopNum,    &
                                 LoopSide    = VentSlab(Item)%HWLoopSide,   &
                                 BranchIndex = VentSlab(Item)%HWBranchNum)

        CASE (Heating_GasCoilType,Heating_ElectricCoilType,Heating_SteamCoilType)

          CALL CalcVentilatedSlabComps(Item,FirstHVACIteration,QUnitOut)

      END SELECT

      END IF     !  Coil/no coil block


ElSE IF (SetpointTemp>AirTempCoolLo)  THEN  ! Cooling Mode

        OperatingMode = CoolingMode

        SetpointTempHi = GetCurrentScheduleValue(VentSlab(Item)%ColdCtrlHiTempSchedPtr)
        SetpointTempLo = GetCurrentScheduleValue(VentSlab(Item)%ColdCtrlLoTempSchedPtr)
        IF (SetpointTempHi < SetpointTempLo) THEN
          CALL ShowSevereError('Cooling setpoint temperature mismatch in'//TRIM(VentSlab(Item)%Name))
          CALL ShowContinueError('High setpoint temperature is less than low setpoint temperature--check your schedule input')
          CALL ShowFatalError('Preceding condition causes termination.')
        END IF

        AirTempHi    = GetCurrentScheduleValue(VentSlab(Item)%ColdAirHiTempSchedPtr)
        AirTempLo    = GetCurrentScheduleValue(VentSlab(Item)%ColdAirLoTempSchedPtr)
        IF (AirTempHi < AirTempLo) THEN
          CALL ShowSevereError('Cooling Air temperature mismatch in'//TRIM(VentSlab(Item)%Name))
          CALL ShowContinueError('High Air temperature is less than low Air temperature--check your schedule input')
          CALL ShowFatalError('Preceding condition causes termination.')
        END IF

        IF (SetpointTemp <= SetpointTempLo) THEN
          ! System is below low cooling setpoint so we should be able to turn the system off
          RadInTemp = AirTempHi
        ELSE IF (SetpointTemp >= SetpointTempHi) THEN
          ! System is running with its lowest inlet temperature
          RadInTemp = AirTempLo
        ELSE
          ! Interpolate to obtain the current radiant system inlet temperature
          RadInTemp = AirTempHi - (AirTempHi - AirTempLo)*(SetpointTemp - SetpointTempLo)/(SetpointTempHi - SetpointTempLo)

        END IF

      ControlNode   = VentSlab(Item)%ColdControlNode
      MaxWaterFlow  = VentSlab(Item)%MaxColdWaterFlow
      MinWaterFlow  = VentSlab(Item)%MinColdWaterFlow



        !On the first HVAC iteration the system values are given to the controller, but after that
        ! the demand limits are in place and there needs to be feedback to the Zone Equipment
      IF((.not. FirstHVACIteration) .and. (ControlNode > 0)  &
         .and. (VentSlab(Item)%CCoilPresent)) Then
         MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
         MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
      End IF
      HCoilOn       = .FALSE.



      If(Node(OutsideAirNode)%MassFlowRate > 0.0d0) Then
         MinOAFrac = GetCurrentScheduleValue(VentSlab(Item)%MinOASchedPtr) * &
                    (VentSlab(Item)%MinOutAirMassFlow / Node(OutsideAirNode)%MassFlowRate)
      Else
         MinOAFrac = 0.0d0
      End If
      MinOAFrac = MIN(1.0d0,MAX(0.0d0,MinOAFrac))


       IF ((.NOT.VentSlab(Item)%CCoilPresent) .OR. &
           (VentSlab(Item)%CCoilSchedValue <= 0.0d0) ) THEN
          ! In cooling mode, but there is no coil to provide cooling.  This is handled
          ! differently than if there was a cooling coil present.  Fixed temperature
          ! will still try to vary the amount of outside air to meet the desired
          ! mixed air temperature, while variable percent will go to full ventilation
          ! when it is most advantageous.

          ! If there are no coil, Slab In Node is assumed to be Fan Outlet Node
        OutletNode   = FanOutletNode

        SELECT CASE (VentSlab(Item)%OAControlType)

        CASE (FixedOAControl)
          ! In this control type, the outdoor air flow rate is fixed to the maximum value
          ! which is equal to the minimum value, regardless of all the other conditions.
          If(Node(OutsideAirNode)%MassFlowRate > 0.0d0) Then
            MaxOAFrac = GetCurrentScheduleValue(VentSlab(Item)%MaxOASchedPtr)
          Else
            MaxOAFrac = 0.0d0
          End If
          MaxOAFrac = MIN(1.0d0,MAX(0.0d0,MinOAFrac))
          OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate

        CASE (VariablePercent)
          ! This algorithm is probably a bit simplistic in that it just bounces
          ! back and forth between the maximum outside air and the minimum.  In
          ! reality, a system *might* vary between the two based on the load in
          ! the zone.  This simple flow control might cause some overcooling but
          ! chances are that if there is a cooling load and the zone temperature
          ! gets above the outside temperature that overcooling won't be significant.

          Tinlet    = Node(InletNode)%Temp
          Toutdoor  = Node(OutsideAirNode)%Temp

          IF (Tinlet <= Toutdoor) THEN

            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

          ELSE ! Tinlet > Toutdoor

            MaxOAFrac = GetCurrentScheduleValue(VentSlab(Item)%MaxOASchedPtr)
            OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate


          END IF

        CASE (FixedTemperature)
          ! This is basically the same algorithm as for the heating case...
          Tdesired  = GetCurrentScheduleValue(VentSlab(Item)%TempSchedPtr)
          MaxOAFrac = 1.0d0

          IF (ABS(Tinlet-Toutdoor) <= LowTempDiff) THEN ! no difference in indoor and outdoor conditions-->set OA to minimum
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE IF (ABS(MaxOAFrac-MinOAFrac) <= LowOAFracDiff) THEN  ! no difference in outside air fractions
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE IF ( ( (Tdesired <= Tinlet) .AND. (Tdesired >= Toutdoor) ) .OR. &
                    ( (Tdesired >= Tinlet) .AND. (Tdesired <= Toutdoor) ) ) THEN
            ! Desired temperature is between the inlet and outdoor temperatures
            ! so vary the flow rate between no outside air and no recirculation air
            ! then applying the maximum and minimum limits the user has scheduled
            ! to make sure too much/little outside air is being introduced
            OAMassFlowRate = ((Tdesired - Tinlet)/(Toutdoor - Tinlet))*Node(InletNode)%MassFlowRate
            OAMassFlowRate = MAX(OAMassFlowRate,(MinOAFrac*Node(OutsideAirNode)%MassFlowRate))
            OAMassFlowRate = MIN(OAMassFlowRate,(MaxOAFrac*Node(OutsideAirNode)%MassFlowRate))
          ELSE IF ( (Tdesired < Tinlet) .AND. (Tdesired < Toutdoor) ) THEN
            ! Desired temperature is below both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet < Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE IF ( (Tdesired > Tinlet) .AND. (Tdesired > Toutdoor) ) THEN
            ! Desired temperature is above both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet > Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE
            ! It should NEVER get to this point, but just in case...
            CALL ShowFatalError(cMO_VentilatedSlab//' simulation control: illogical condition for '//TRIM(VentSlab(Item)%Name))
          END IF

        END SELECT


        CALL CalcVentilatedSlabComps(Item,FirstHVACIteration,QUnitOut)


      ELSE
          ! There is a cooling load and there is a cooling coil present (presumably).
          ! Variable percent will throttle outside air back to the minimum while
          ! fixed temperature will still try to vary the outside air amount to meet
          ! the desired mixed air temperature.

        SELECT CASE (VentSlab(Item)%OAControlType)

        Case (FixedOAControl)
            ! In this control type, the outdoor air flow rate is fixed to the maximum value
            ! which is equal to the minimum value, regardless of all the other conditions.
          If(Node(OutsideAirNode)%MassFlowRate > 0.0d0) Then
            MaxOAFrac = GetCurrentScheduleValue(VentSlab(Item)%MaxOASchedPtr)
          Else
            MaxOAFrac = 0.0d0
          End If
          MaxOAFrac = MIN(1.0d0,MAX(0.0d0,MinOAFrac))
          OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate

        CASE (VariablePercent)
          ! A cooling coil is present so let it try to do the cooling...
          OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate

        CASE (FixedTemperature)
          ! This is basically the same algorithm as for the heating case...
          Tdesired  = GetCurrentScheduleValue(VentSlab(Item)%TempSchedPtr)

          MaxOAFrac = 1.0d0

          IF (ABS(Tinlet-Toutdoor) <= LowTempDiff) THEN ! no difference in indoor and outdoor conditions-->set OA to minimum
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE IF (ABS(MaxOAFrac-MinOAFrac) <= LowOAFracDiff) THEN  ! no difference in outside air fractions
            OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
          ELSE IF ( ( (Tdesired <= Tinlet) .AND. (Tdesired >= Toutdoor) ) .OR. &
                    ( (Tdesired >= Tinlet) .AND. (Tdesired <= Toutdoor) ) ) THEN
            ! Desired temperature is between the inlet and outdoor temperatures
            ! so vary the flow rate between no outside air and no recirculation air
            ! then applying the maximum and minimum limits the user has scheduled
            ! to make sure too much/little outside air is being introduced
            OAMassFlowRate = ((Tdesired - Tinlet)/(Toutdoor - Tinlet))*Node(InletNode)%MassFlowRate
            OAMassFlowRate = MAX(OAMassFlowRate,(MinOAFrac*Node(OutsideAirNode)%MassFlowRate))
            OAMassFlowRate = MIN(OAMassFlowRate,(MaxOAFrac*Node(OutsideAirNode)%MassFlowRate))
          ELSE IF ( (Tdesired < Tinlet) .AND. (Tdesired < Toutdoor) ) THEN
            ! Desired temperature is below both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet < Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE IF ( (Tdesired > Tinlet) .AND. (Tdesired > Toutdoor) ) THEN
            ! Desired temperature is above both the inlet and outdoor temperatures
            ! so use whichever flow rate (max or min) that will get closer
            IF (Tinlet > Toutdoor) THEN   ! Tinlet closer to Tdesired so use minimum outside air
              OAMassFlowRate = MinOAFrac*Node(OutsideAirNode)%MassFlowRate
            ELSE  ! Toutdoor closer to Tdesired so use maximum outside air
              OAMassFlowRate = MaxOAFrac*Node(OutsideAirNode)%MassFlowRate
            END IF
          ELSE
            ! It should NEVER get to this point, but just in case...
            CALL ShowFatalError(cMO_VentilatedSlab//' simulation control: illogical condition for '//TRIM(VentSlab(Item)%Name))
          END IF

        END SELECT

          ! control water flow to obtain output matching Low Setpoint Temperateure
        HCoilOn = .FALSE.

        CALL SimVentSlabOAMixer(Item)
        CALL SimulateFanComponents(VentSlab(Item)%FanName,FirstHVACIteration,VentSlab(Item)%Fan_Index, &
                                   ZoneCompTurnFansOn = ZoneCompTurnFansOn,ZoneCompTurnFansOff = ZoneCompTurnFansOff)

        CpFan                 = PsyCpAirFnWTdb(Node(FanOutletNode)%HumRat,Node(FanOutletNode)%Temp)
        QZnReq                = (Node(OutletNode)%MassFlowRate)*CpFan*(RadInTemp-Node(FanOutletNode)%Temp)

          CALL ControlCompOutput(CompName=VentSlab(Item)%Name,CompType=cMO_VentilatedSlab,CompNum=Item, &
                                 FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                                 ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                                 MinFlow=MinWaterFlow,ControlOffSet=0.001d0, &
                                 ControlCompTypeNum=VentSlab(Item)%ControlCompTypeNum, &
                                 CompErrIndex=VentSlab(Item)%CompErrIndex,  &
                                 LoopNum     = VentSlab(Item)%CWLoopNum,    &
                                 LoopSide    = VentSlab(Item)%CWLoopSide,   &
                                 BranchIndex = VentSlab(Item)%CWBranchNum)

      END IF



    END IF  ! ...end of HEATING/COOLING IF-THEN block

    CALL CalcVentilatedSlabRadComps(Item, FirstHVACIteration)


    AirMassFlow    = Node(Outletnode)%MassFlowRate
    QUnitOut       = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(FanOutletNode)%HumRat)  &
                     -PsyHFnTdbW(Node(FanOutletNode)%Temp,Node(FanOutletNode)%HumRat))


END IF    ! ...end of system ON/OFF IF-THEN block

! CR9155 Remove specific humidity calculations
  SpecHumOut = Node(OutletNode)%HumRat
  SpecHumIn  = Node(FanOutletNode)%HumRat
  LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate (kg/s), dehumid = negative

  QTotUnitOut = AirMassFlow * (Node(FanOutletNode)%Enthalpy - Node(OutletNode)%Enthalpy)

          ! Report variables...
  VentSlab(Item)%HeatCoilPower     = MAX(0.0d0,QUnitOut)
  VentSlab(Item)%SensCoolCoilPower = ABS(MIN(0.0d0,QUnitOut))
  VentSlab(Item)%TotCoolCoilPower  = ABS(MIN(0.0d0,QTotUnitOut))
  VentSlab(Item)%LateCoolCoilPower = VentSlab(Item)%TotCoolCoilPower - VentSlab(Item)%SensCoolCoilPower
  VentSlab(Item)%ElecFanPower      = FanElecPower
  VentSlab(Item)%AirMassFlowRate   = AirMassFlow

  PowerMet = QUnitOut
  LatOutputProvided = LatentOutput

  RETURN

END SUBROUTINE CalcVentilatedSlab



SUBROUTINE CalcVentilatedSlabComps(Item,FirstHVACIteration,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   June 2008
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine launches the individual component simulations.
          ! This is called either when the system is off to carry null conditions
          ! through the system or during control iterations to continue updating
          ! what is going on within the unit.

          ! METHODOLOGY EMPLOYED:
          ! Simply calls the different components in order.  Only slight wrinkles
          ! here are that the ventilatd slab system has it's own outside air mixed and
          ! that a cooling coil must be present in order to call a cooling coil
          ! simulation.  Other than that, the subroutine is very straightforward.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,         ONLY : SimulateFanComponents
  USE HeatingCoils, ONLY : SimulateHeatingCoilComponents
  USE WaterCoils,   ONLY : SimulateWaterCoilComponents
  USE HVACHXAssistedCoolingCoil, ONLY : SimHXAssistedCoolingCoil
  Use SteamCoils,                ONLY : SimulateSteamCoilComponents
  USE DataHVACGlobals,           ONLY : ZoneCompTurnFansOn, ZoneCompTurnFansOff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,       INTENT(IN)  :: Item        ! system index in ventilated slab array
  LOGICAL,       INTENT(IN)  :: FirstHVACIteration ! flag for 1st HVAV iteration in the time step
  REAL(r64),      INTENT(OUT)  :: LoadMet            ! load met by the system (watts)


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: AirMassFlow     ! total mass flow through the system
  REAL(r64)           :: CpAirZn         ! specific heat of dry air at zone conditions (zone conditions same as system inlet)
  INTEGER        :: HCoilInAirNode  ! inlet node number for fan exit/coil inlet
  INTEGER        :: InletNode       ! system air inlet node
  INTEGER        :: OutletNode      ! system air outlet node
!unused0309  INTEGER        :: HCoilOutAirNode
  REAL(r64)           :: QCoilReq        ! Heat addition required from an electric/gas heating coil
  REAL(r64)           :: HCoilOutAirTemp
  REAL(r64)           :: HCoilInAirTemp
!unused1208  REAL(r64)           :: RadInTemp       ! Set temperature for "Slab In Node"

          ! FLOW:

  CALL SimVentSlabOAMixer(Item)
  CALL SimulateFanComponents(VentSlab(Item)%FanName,FirstHVACIteration,VentSlab(Item)%Fan_Index, &
                              ZoneCompTurnFansOn = ZoneCompTurnFansOn,ZoneCompTurnFansOff = ZoneCompTurnFansOff)
  IF ((VentSlab(Item)%CCoilPresent) .AND. (VentSlab(Item)%CCoilSchedValue >= 0.0d0)) THEN
    IF(VentSlab(Item)%CCoilType == Cooling_CoilHXAssisted) THEN
      CALL SimHXAssistedCoolingCoil(VentSlab(Item)%CCoilName,FirstHVACIteration,On,0.0d0,VentSlab(Item)%CCoil_Index,ContFanCycCoil)
    ELSE
      CALL SimulateWaterCoilComponents(VentSlab(Item)%CCoilName,FirstHVACIteration,  &
                                       VentSlab(Item)%CCoil_Index)
    END IF

  END IF



  IF ((VentSlab(Item)%HCoilPresent).AND. (VentSlab(Item)%HCoilSchedValue >= 0.0d0)) THEN

    SELECT CASE (VentSlab(Item)%HCoilType)

      CASE (Heating_WaterCoilType)

        CALL SimulateWaterCoilComponents(VentSlab(Item)%HCoilName,FirstHVACIteration,  &
                                         VentSlab(Item)%HCoil_Index)

      CASE (Heating_SteamCoilType)

         IF (.NOT.HCoilOn) THEN
          QCoilReq = 0.0d0
        ELSE
          HCoilInAirNode = VentSlab(Item)%FanOutletNode
          CpAirZn        = PsyCpAirFnWTdb(Node(HCoilInAirNode)%HumRat,Node(HCoilInAirNode)%Temp)
          QCoilReq       = Node(HCoilInAirNode)%MassFlowRate * CpAirZn &
                                    *(Node(VentSlab(Item)%RadInNode)%Temp)-(Node(HCoilInAirNode)%Temp)
        END IF

        IF (QCoilReq < 0.0d0) QCoilReq = 0.0d0    ! a heating coil can only heat, not cool

        CALL SimulateSteamCoilComponents(CompName=VentSlab(Item)%HCoilName, &
                                           FirstHVACIteration=FirstHVACIteration,    &
                                           QCoilReq=QCoilReq,                        &
                                           CompIndex=VentSlab(Item)%HCoil_Index)



     CASE (Heating_ElectricCoilType,Heating_GasCoilType)

        IF (.NOT.HCoilOn) THEN
          QCoilReq = 0.0d0
        ELSE
         HCoilInAirTemp = Node(VentSlab(Item)%FanOutletNode)%Temp
         HCoilOutAirTemp = Node(VentSlab(Item)%RadInNode)%Temp
         CpAirZn        = PsyCpAirFnWTdb(Node(VentSlab(Item)%RadInNode)%HumRat,Node(VentSlab(Item)%RadInNode)%Temp)
         QCoilReq       = Node(VentSlab(Item)%FanOutletNode)%MassFlowRate * CpAirZn &
                                    *(HCoilOutAirTemp-HCoilInAirTemp)

        END IF

        IF (QCoilReq < 0.0d0) QCoilReq = 0.0d0    ! a heating coil can only heat, not cool



      CALL SimulateHeatingCoilComponents(CompName=VentSlab(Item)%HCoilName, &
                                           FirstHVACIteration=FirstHVACIteration,    &
                                           QCoilReq=QCoilReq,                        &
                                           CompIndex=VentSlab(Item)%HCoil_Index)

    END SELECT


  END IF


  InletNode   = VentSlab(Item)%FanOutletNode
  OutletNode  = VentSlab(Item)%RadInNode
  AirMassFlow = Node(OutletNode)%MassFlowRate

  LoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                         - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))



 RETURN

END SUBROUTINE CalcVentilatedSlabComps


SUBROUTINE CalcVentilatedSlabRadComps(Item, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
          !       DATE WRITTEN   June 2008
          !       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine launches the individual component simulations.
          ! This is called either when the system is off to carry null conditions
          ! through the system or during control iterations to continue updating
          ! what is going on within the system.

          ! METHODOLOGY EMPLOYED:
          ! Simply calls the different components in order.  Only slight wrinkles
          ! here are that the Ventilated Slab has it's own outside air mixed and
          ! that a cooling coil must be present in order to call a cooling coil
          ! simulation.  Other than that, the subroutine is very straightforward.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,    ONLY : OutBaroPress
  USE General,            ONLY : RoundSigDigits
  USE Fans,         ONLY : SimulateFanComponents
  USE HeatingCoils, ONLY : SimulateHeatingCoilComponents
  USE WaterCoils,   ONLY : SimulateWaterCoilComponents
  USE HVACHXAssistedCoolingCoil, ONLY :SimHXAssistedCoolingCoil
  Use SteamCoils,   ONLY: SimulateSteamCoilComponents
  USE DataHeatBalance,    ONLY : Construct, Zone
  USE DataHeatBalFanSys,  ONLY : RadSysTiHBConstCoef,                   &
                                 RadSysTiHBToutCoef,RadSysTiHBQsrcCoef, &
                                 RadSysToHBConstCoef,RadSysToHBTinCoef, &
                                 RadSysToHBQsrcCoef,CTFTsrcConstPart,   &
                                 ZoneAirHumRat, &
                                 MAT
  USE DataHeatBalSurface, ONLY : TH
  USE DataSurfaces,       ONLY : Surface
  USE NodeInputManager,         ONLY : GetOnlySingleNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: Item        ! System index in ventilated slab array
  LOGICAL, INTENT(IN)  :: FirstHVACIteration ! flag for 1st HVAV iteration in the time step !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: CondDeltaTemp  = 0.001d0   ! How close the surface temperatures can get to the dewpoint temperature
                                            ! of a space before the radiant cooling system shuts off the flow.
  REAL(r64), PARAMETER :: ZeroSystemResp = 0.1d0   ! Response below which the system response is really zero
  REAL(r64), PARAMETER :: TempCheckLimit = 0.1d0   ! Maximum allowed temperature difference between outlet temperature calculations
  REAL(r64), PARAMETER :: VentSlabAirTempToler = 0.001d0    !Maximum allowed temperature difference between the zone and return air
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='ZoneHVAC:VentilatedSlab'


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: ConstrNum      ! Index for construction number in Construct derived type
  REAL(r64):: CpAirZn             ! Intermediate calculational variable for specific heat of air
  REAL(r64):: DewPointTemp   ! Dew-point temperature based on the zone air conditions
  REAL(r64):: EpsMdotCpAirzn      ! Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat
  REAL(r64):: Mdot           ! Intermediate calculation variable for mass flow rate in a surface within the radiant system
  INTEGER  :: RadSurfNum        ! DO loop counter for the surfaces that comprise a particular radiant system
  INTEGER  :: RadSurfNum2    ! DO loop counter for the surfaces that comprise a particular radiant system
  INTEGER  :: RadSurfNum3    ! DO loop counter for the surfaces that comprise a particular radiant system
!unused0309  INTEGER  :: RadSurfNum4    ! DO loop counter for the surfaces that comprise a particular radiant system

  INTEGER  :: SurfNum        ! Index for radiant surface in Surface derived type
  INTEGER  :: SurfNum2       ! Index for radiant surface in Surface derived type
!unused0309  INTEGER  :: RadSurfNumNum
  REAL(r64):: TotalVentSlabRadPower      ! Total heat source/sink to radiant system
  REAL(r64):: AirMassFlow  ! air mass flow rate in the radiant system, kg/s
  INTEGER  :: SlabInNode    ! Node number of the air entering the radiant system
  REAL(r64):: AirOutletTempCheck  ! Radiant system air outlet temperature (calculated from mixing all outlet streams together)
  REAL(r64):: AirTempIn    ! Temperature of the air entering the radiant system, in C
  INTEGER  :: ZoneNum        ! number of zone being served
  REAL(r64):: ZoneMult       ! Zone multiplier for this system
  REAL(r64):: Ca,Cb,Cc,Cd,Ce,Cf,Cg,Ch,Ci,Cj,Ck,Cl  ! Coefficients to relate the inlet air temperature to the heat source
                              ! For more info on Ca through Cl, refer Constant Flow Radiant System
!unused0309  REAL(r64):: CoreNumber
  REAL(r64), SAVE :: Ckj, Cmj     ! Coefficients for individual surfaces within a radiant system
  REAL(r64), SAVE, DIMENSION(:), ALLOCATABLE :: AirTempOut ! Array of outlet air temperatures for each surface in the radiant system
  INTEGER  :: FanOutletNode      ! unit air outlet node
  INTEGER  :: OAInletNode      ! unit air outlet node
  INTEGER  :: MixoutNode      ! unit air outlet node
  INTEGER  :: Returnairnode ! discription
  INTEGER  :: ZoneAirInNode !supply air node
!For Phase 3
  REAL(r64)  :: CNumDS
  REAL(r64)  :: CLengDS
  REAL(r64)  :: CDiaDS
  REAL(r64)  :: FlowFrac
!unused0309  REAL(r64)  :: SlabAirOutTemp
  REAL(r64)  :: MSlabAirInTemp
  LOGICAL                        :: ErrorsFound=.false. ! Set to true if errors in input, fatal at end of routine

  CHARACTER(len=MaxNameLength) ::MSlabIn
  CHARACTER(len=MaxNameLength) ::MSlabOut
  CHARACTER(len=MaxNameLength) ::SlabName
  INTEGER   :: MSlabInletNode
  INTEGER   :: MSlabOutletNode
  INTEGER, SAVE :: CondensationErrorCount = 0   ! Counts for # times the radiant systems are shutdown due to condensation
  INTEGER, SAVE :: EnergyImbalanceErrorCount = 0 ! Counts for # times a temperature mismatch is found in the energy balance check
  LOGICAL, SAVE :: FirstTimeFlag=.true.  ! for setting size of Ckj, Cmj, AirTempOut arrays

          ! FLOW:

  IF (FirstTimeFlag) THEN
    ALLOCATE (AirTempOut(MaxCloNumOfSurfaces))
    FirstTimeFlag=.false.
  ENDIF

  Ckj          = 0.0d0
  Cmj          = 0.0d0

  SlabInNode       = VentSlab(Item)%RadInNode
  FanOutletNode    = VentSlab(Item)%FanOutletNode
  OAInletNode      = VentSlab(Item)%OutsideAirNode
  MixoutNode       = VentSlab(Item)%OAMixerOutNode
  Returnairnode    = VentSlab(Item)%ReturnAirNode
  ZoneAirInnode    = VentSlab(Item)%ZoneAirInNode



          ! Set the conditions on the air side inlet
  ZoneNum       = VentSlab(Item)%ZonePtr
  ZoneMult      = REAL(Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier,r64)
  AirMassFlow   = Node(VentSlab(Item)%RadInNode)%MassFlowRate / ZoneMult




      IF (OperatingMode==HeatingMode) THEN

        IF ((.NOT.VentSlab(Item)%HCoilPresent) .OR. &
             (VentSlab(Item)%HCoilSchedValue <= 0.0d0)) THEN

          AirTempIn = Node(FanOutletNode)%Temp
          Node(SlabInNode)%Temp = Node(FanOutletNode)%Temp    ! If coil not available or running, then coil in and out temps same

        ELSE

          AirTempIn     = Node(SlabInNode)%Temp
        END IF
      END IF

      IF (OperatingMode==CoolingMode) THEN

        IF ((.NOT.VentSlab(Item)%CCoilPresent) .OR. &
             (VentSlab(Item)%CCoilSchedValue <= 0.0d0)) THEN

          AirTempIn = Node(FanOutletNode)%Temp
          Node(SlabInNode)%Temp = Node(FanOutletNode)%Temp    ! If coil not available or running, then coil in and out temps same

         ELSE

           AirTempIn = Node(SlabInNode)%Temp
         END IF

       END IF

IF (AirMassFlow <= 0.0d0) THEN
          ! No flow or below minimum allowed so there is no heat source/sink
          ! This is possible with a mismatch between system and plant operation
          ! or a slight mismatch between zone and system controls.  This is not
          ! necessarily a "problem" so this exception is necessary in the code.

    DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces
      SurfNum= VentSlab(Item)%SurfacePtr(RadSurfNum)
      QRadSysSource(SurfNum)= 0.0D0
      IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
      QRadSysSource(Surface(SurfNum)%ExtBoundCond) = 0.0D0    ! Also zero the other side of an interzone
    END DO

    VentSlab(Item)%SlabOutTemp = VentSlab(Item)%SlabInTemp

    ! zero out node flows
    Node(SlabInNode)%MassFlowRate          = 0.0d0
    Node(FanOutletNode)%MassFlowRate       = 0.0d0
    Node(OAInletNode)%MassFlowRate         = 0.0d0
    Node(MixoutNode)%MassFlowRate          = 0.0d0
    Node(Returnairnode)%MassFlowRate       = 0.0d0
    Node(FanOutletNode)%Temp = Node(SlabInNode)%Temp
    AirMassFlow                            = 0.0d0
END IF

IF (AirMassFlow > 0.0d0) THEN

  IF ((VentSlab(Item)%SysConfg == SlabOnly).OR.(VentSlab(Item)%SysConfg == SlabAndZone)) THEN

    DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces
       SurfNum = VentSlab(Item)%SurfacePtr(RadSurfNum)
          ! Determine the heat exchanger "effectiveness" term
       EpsMdotCpAirZn = CalcVentSlabHXEffectTerm(Item,AirTempIn,AirMassFlow,               &
                                                 VentSlab(Item)%SurfaceFlowFrac(RadSurfNum)    , &
                                                 VentSlab(Item)%CoreLength,                  &
                                                 VentSlab(Item)%CoreDiameter, VentSlab(Item)%CoreNumbers)

          ! Obtain the heat balance coefficients and calculate the intermediate coefficients
          ! linking the inlet air temperature to the heat source/sink to the radiant system.
          ! The coefficients are based on the Constant Flow Radiation System.

      ConstrNum = Surface(SurfNum)%Construction

      Ca = RadSysTiHBConstCoef(SurfNum)
      Cb = RadSysTiHBToutCoef(SurfNum)
      Cc = RadSysTiHBQsrcCoef(SurfNum)

      Cd = RadSysToHBConstCoef(SurfNum)
      Ce = RadSysToHBTinCoef(SurfNum)
      Cf = RadSysToHBQsrcCoef(SurfNum)

      Cg = CTFTsrcConstPart(SurfNum)
      Ch = REAL(Construct(ConstrNum)%CTFTSourceQ(0),r64)
      Ci = REAL(Construct(ConstrNum)%CTFTSourceIn(0),r64)
      Cj = REAL(Construct(ConstrNum)%CTFTSourceOut(0),r64)

      Ck = Cg + ( ( Ci*(Ca+Cb*Cd) + Cj*(Cd+Ce*Ca) ) / ( 1.0d0 - Ce*Cb ) )
      Cl = Ch + ( ( Ci*(Cc+Cb*Cf) + Cj*(Cf+Ce*Cc) ) / ( 1.0d0 - Ce*Cb ) )

      Mdot = AirMassFlow * VentSlab(Item)%SurfaceFlowFrac(RadSurfNum)
      CpAirZn        = PsyCpAirFnWTdb(Node(VentSlab(Item)%RadInNode)%HumRat,Node(VentSlab(Item)%RadInNode)%Temp)

      QRadSysSource(SurfNum) = VentSlab(Item)%CoreNumbers * EpsMdotCpAirZn * (AirTempIn - Ck) &
                                /(1.0d0 + (EpsMdotCpAirZn*Cl/Surface(SurfNum)%Area) )

    IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
             QRadSysSource(Surface(SurfNum)%ExtBoundCond) = QRadSysSource (SurfNum)
       ! Also set the other side of an interzone!
          AirTempOut(RadSurfNum) = AirTempIn - (QRadSysSource(SurfNum)/(Mdot*CpAirZn))

          ! "Temperature Comparison" Cut-off:
          ! Check to see whether or not the system should really be running.  If
          ! QRadSysSource is negative when we are in heating mode or QRadSysSource
          ! is positive when we are in cooling mode, then the radiant system will
          ! be doing the opposite of its intention.  In this case, the flow rate
          ! is set to zero to avoid heating in cooling mode or cooling in heating
          ! mode.

       IF (((OperatingMode == HeatingMode).AND.(QRadSysSource(SurfNum) <= 0.0d0)) .OR. &
           ((OperatingMode == CoolingMode).AND.(QRadSysSource(SurfNum) >= 0.0d0)) ) THEN

! IF (.not. WarmupFlag) THEN
!   TempComparisonErrorCount = TempComparisonErrorCount + 1
!   IF (TempComparisonErrorCount <= NumOfVentSlabs) THEN
!     CALL ShowWarningError('Radaint Heat exchange is negative in Heating Mode or posive in Cooling Mode')
!     CALL ShowContinueError('Flow to the following ventilated slab will be shut-off to avoid heating in cooling mode or cooling &
!                             in heating mode')
!     CALL ShowContinueError('Ventilated Slab Name = '//TRIM(VentSlab(Item)%Name))
!     CALL ShowContinueError('All node temperature are reseted at the ventilated slab surface temperature = '// &
!                            RoundSigDigits(TH(VentSlab(Item)%SurfacePtr(RadSurfNum),1,2),2))
!     CALL ShowContinueErrorTimeStamp(' ')
!   ELSE
!     CALL ShowRecurringWarningErrorAtEnd('Ventilated Slab ['//TRIM(VentSlab(Item)%Name)//  &
!                  '] Temperature Comparison Error shut-off occurrence continues.',  &
!                  VentSlab(Item)%CondErrCount)
!   END IF
! END IF

            Node(SlabInNode)%MassFlowRate          = 0.0d0
            Node(FanOutletNode)%MassFlowRate       = 0.0d0
            Node(OAInletNode)%MassFlowRate         = 0.0d0
            Node(MixoutNode)%MassFlowRate          = 0.0d0
            Node(Returnairnode)%MassFlowRate       = 0.0d0
            AirMassFlow                            = 0.0d0

        DO RadSurfNum2 = 1, VentSlab(Item)%NumOfSurfaces
          SurfNum2 = VentSlab(Item)%SurfacePtr(RadSurfNum2)
          QRadSysSource(SurfNum2) = 0.0D0
          IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
            QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone

         IF (VentSlab(Item)%SysConfg == SlabOnly) THEN
!            Node(Returnairnode)%Temp = MAT(Zonenum)
            Node(Returnairnode)%Temp = TH(VentSlab(Item)%SurfacePtr(RadSurfNum),1,2)
            Node(FanOutletNode)%Temp = Node(Returnairnode)%Temp
            Node(SlabInNode)%Temp    = Node(FanOutletNode)%Temp
         ELSE IF (VentSlab(Item)%SysConfg == SlabandZone) THEN
            Node(ReturnAirNode)%Temp = MAT(Zonenum)
            Node(SlabInNode)%Temp    = Node(ReturnAirNode)%Temp
            Node(FanOutletNode)%Temp = Node(SlabInNode)%Temp
            Node(ZoneAirInNode)%Temp = Node(SlabInNode)%Temp
         END IF

       END DO
        EXIT ! outer do loop
     END IF

          ! Condensation Cut-off:
          ! Check to see whether there are any surface temperatures within the radiant system that have
          ! dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
          ! A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
          ! conditions.

     IF (OperatingMode == CoolingMode) THEN
        DewPointTemp = PsyTdpFnWPb(ZoneAirHumRat(VentSlab(Item)%ZonePtr),OutBaroPress)
        DO RadSurfNum2 = 1, VentSlab(Item)%NumOfSurfaces
          IF (TH(VentSlab(Item)%SurfacePtr(RadSurfNum2),1,2) < (DewPointTemp+CondDeltaTemp) ) THEN
          ! Condensation warning--must shut off radiant system
            Node(SlabInNode)%MassFlowRate          = 0.0d0
            Node(FanOutletNode)%MassFlowRate       = 0.0d0
            Node(OAInletNode)%MassFlowRate         = 0.0d0
            Node(MixoutNode)%MassFlowRate          = 0.0d0
            Node(Returnairnode)%MassFlowRate       = 0.0d0
            Node(FanOutletNode)%Temp = Node(SlabInNode)%Temp
            AirMassFlow                            = 0.0d0
            DO RadSurfNum3 = 1, VentSlab(Item)%NumOfSurfaces
              SurfNum2 = VentSlab(Item)%SurfacePtr(RadSurfNum3)
              QRadSysSource(SurfNum2) = 0.0D0
              IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
                QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
            END DO
          ! Produce a warning message so that user knows the system was shut-off due to potential for condensation
            IF (.not. WarmupFlag) THEN
              CondensationErrorCount = CondensationErrorCount + 1

              IF (VentSlab(Item)%CondErrIndex == 0) THEN
                CALL ShowWarningMessage(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//']')
                CALL ShowContinueError('Surface ['//trim(Surface(VentSlab(Item)%SurfacePtr(RadSurfNum2))%Name)//  &
                   '] temperature below dew-point temperature--potential for condensation exists')
                CALL ShowContinueError('Flow to the ventilated slab system will be shut-off to avoid condensation')
                CALL ShowContinueError('Predicted radiant system surface temperature = '// &
                                       trim(RoundSigDigits(TH(VentSlab(Item)%SurfacePtr(RadSurfNum2),1,2),2)))
                CALL ShowContinueError('Zone dew-point temperature + safety factor delta= '//  &
                   trim(RoundSigDigits(DewPointTemp+CondDeltaTemp,2)))
                CALL ShowContinueErrorTimeStamp(' ')
              ENDIF
              IF (CondensationErrorCount == 1) THEN
                  CALL ShowContinueError('Note that there is a '//TRIM(RoundSigDigits(CondDeltaTemp,4))// &
                                         ' C safety built-in to the shut-off criteria')
                  CALL ShowContinueError('Note also that this affects all surfaces that are part of this system')
              END IF
              CALL ShowRecurringWarningErrorAtEnd(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//  &
                         '] condensation shut-off occurrence continues.',  &
                         VentSlab(Item)%CondErrIndex,ReportMinOf=DewPointTemp,ReportMaxOf=DewPointTemp,  &
                         ReportMaxUnits='C',ReportMinUnits='C')
            END IF
            EXIT ! outer do loop
          END IF
        END DO
     END IF
    END DO

! Total Radiant Power
    AirOutletTempCheck = 0.0d0
    TotalVentSlabRadPower = 0.0d0
    DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces
      SurfNum              = VentSlab(Item)%SurfacePtr(RadSurfNum)
      TotalVentSlabRadPower     = TotalVentSlabRadPower + QRadSysSource(SurfNum)
      AirOutletTempCheck = AirOutletTempCheck+(VentSlab(Item)%SurfaceFlowFrac(RadSurfNum)*AirTempOut(RadSurfNum))
    END DO
    TotalVentSlabRadPower = ZoneMult * TotalVentSlabRadPower

! Return Air temp Check
     IF (VentSlab(Item)%SysConfg == SlabOnly) THEN
      IF (AirMassFlow> 0.0d0) THEN
         CpAirZn        = PsyCpAirFnWTdb(Node(VentSlab(Item)%RadInNode)%HumRat,Node(VentSlab(Item)%RadInNode)%Temp)!
         Node(ReturnAirNode)%Temp = Node(SlabInNode)%Temp &
                -(TotalVentSlabRadPower/(AirMassFlow*CpAirZn))
        IF ((ABS(Node(ReturnAirNode)%Temp-AirOutletTempCheck) > TempCheckLimit) .AND. &
            (ABS(TotalVentSlabRadPower) > ZeroSystemResp) ) THEN

          IF (.not. WarmupFlag) THEN
            EnergyImbalanceErrorCount = EnergyImbalanceErrorCount + 1
            IF (VentSlab(Item)%EnrgyImbalErrIndex == 0) THEN
              CALL ShowWarningMessage(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//']')
              CALL ShowContinueError('Ventilated Slab (slab only type) air outlet temperature calculation mismatch.')
              CALL ShowContinueError('This should not happen as it indicates a potential energy imbalance in the calculations.')
              CALL ShowContinueError('However, it could also result from improper input for the ventilated slab or')
              CALL ShowContinueError('illogical control temperatures.  Check your input for this ventilated slab and')
              CALL ShowContinueError('also look at the internal data shown below.')
              CALL ShowContinueError('Predicted return air temperature [C] from the overall energy balance = '// &
                                     trim(RoundSigDigits(Node(ReturnAirNode)%Temp,4)))
              CALL ShowContinueError('Predicted return air temperature [C] from the slab section energy balances = '// &
                                     trim(RoundSigDigits(AirOutletTempCheck,4)))
              CALL ShowContinueError('Total energy rate (power) [W] added to the slab = '// &
                                     trim(RoundSigDigits(TotalVentSlabRadPower,4)))
              CALL ShowContinueErrorTimeStamp(' ')
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//  &
                       '] temperature calculation mismatch occurrence continues.',  &
                       VentSlab(Item)%EnrgyImbalErrIndex)
          END IF

         END IF
       ELSE
         Node(ReturnAirNode)%Temp = Node(SlabInNode)%Temp
       END IF
     END IF


    IF (VentSlab(Item)%SysConfg == SlabandZone) THEN
      IF (AirMassFlow> 0.0d0) THEN
       Node(ZoneAirInNode)%Temp = Node(SlabInNode)%Temp &
                                   -(TotalVentSlabRadPower/(AirMassFlow*CpAirZn))
       IF ( (ABS(Node(ZoneAirInNode)%Temp-AirOutletTempCheck) > TempCheckLimit) .AND. &
            (ABS(TotalVentSlabRadPower) > ZeroSystemResp) ) THEN

          IF (.not. WarmupFlag) THEN
            EnergyImbalanceErrorCount = EnergyImbalanceErrorCount + 1
            IF (VentSlab(Item)%EnrgyImbalErrIndex == 0) THEN
              CALL ShowWarningMessage(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//']')
              CALL ShowContinueError('Ventilated Slab (slab only type) air outlet temperature calculation mismatch.')
              CALL ShowContinueError('This should not happen as it indicates a potential energy imbalance in the calculations.')
              CALL ShowContinueError('However, it could also result from improper input for the ventilated slab or')
              CALL ShowContinueError('illogical control temperatures.  Check your input for this ventilated slab and')
              CALL ShowContinueError('also look at the internal data shown below.')
              CALL ShowContinueError('Predicted return air temperature [C] from the overall energy balance = '// &
                                     trim(RoundSigDigits(Node(ReturnAirNode)%Temp,4)))
              CALL ShowContinueError('Predicted return air temperature [C] from the slab section energy balances = '// &
                                     trim(RoundSigDigits(AirOutletTempCheck,4)))
              CALL ShowContinueError('Total energy rate (power) [W] added to the slab = '// &
                                     trim(RoundSigDigits(TotalVentSlabRadPower,4)))
              CALL ShowContinueErrorTimeStamp(' ')
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//  &
                       '] temperature calculation mismatch occurrence continues.',  &
                       VentSlab(Item)%EnrgyImbalErrIndex)
          END IF

       END IF
!       IF ((.NOT. FirstHVACIteration) .AND. &
!          (ABS(Node(ReturnAirNode)%Temp-MAT(Zonenum)) > VentSlabAirTempToler))THEN
!          NeedtoIterate = .TRUE.
!      END IF
!         Node(ReturnAirNode)%Temp = MAT(Zonenum)
      ELSE
      Node(ZoneAirInNode)%Temp = Node(SlabInNode)%Temp
      Node(ReturnAirNode)%Temp= MAT(Zonenum)
      ENDIF
    END IF


          ! Now that we have the source/sink term, we must redo the heat balances to obtain
          ! the new SumHATsurf value for the zone.  Note that the difference between the new
          ! SumHATsurf and the value originally calculated by the heat balance with a zero
          ! source for all radiant systems in the zone is the load met by the system (approximately).
  CALL CalcHeatBalanceOutsideSurf(ZoneNum)
  CALL CalcHeatBalanceInsideSurf(ZoneNum)


  END IF !SYSCONFIG. SLABONLY&SLABANDZONE


  IF (VentSlab(Item)%SysConfg == SeriesSlabs) THEN



    DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces

       CNumDS=VentSlab(Item)%CNumbers(RadSurfNum)
       CLengDS=VentSlab(Item)%CLength(RadSurfNum)! for check
       CDiaDS=VentSlab(Item)%CDiameter(RadSurfNum)! for check
       FlowFrac=1.0d0

       SurfNum = VentSlab(Item)%SurfacePtr(RadSurfNum)


          ! Determine the heat exchanger "effectiveness" term
       EpsMdotCpAirZn = CalcVentSlabHXEffectTerm(Item,AirTempIn,AirMassFlow,               &
                                                 FlowFrac    , &
                                                 CLengDS,                  &
                                                 CDiaDS, CNumDS)

          ! Obtain the heat balance coefficients and calculate the intermediate coefficients
          ! linking the inlet air temperature to the heat source/sink to the radiant system.
          ! The coefficients are based on the Constant Flow Radiation System.

      ConstrNum = Surface(SurfNum)%Construction

      Ca = RadSysTiHBConstCoef(SurfNum)
      Cb = RadSysTiHBToutCoef(SurfNum)
      Cc = RadSysTiHBQsrcCoef(SurfNum)

      Cd = RadSysToHBConstCoef(SurfNum)
      Ce = RadSysToHBTinCoef(SurfNum)
      Cf = RadSysToHBQsrcCoef(SurfNum)

      Cg = CTFTsrcConstPart(SurfNum)
      Ch = REAL(Construct(ConstrNum)%CTFTSourceQ(0),r64)
      Ci = REAL(Construct(ConstrNum)%CTFTSourceIn(0),r64)
      Cj = REAL(Construct(ConstrNum)%CTFTSourceOut(0),r64)

      Ck = Cg + ( ( Ci*(Ca+Cb*Cd) + Cj*(Cd+Ce*Ca) ) / ( 1.0d0 - Ce*Cb ) )
      Cl = Ch + ( ( Ci*(Cc+Cb*Cf) + Cj*(Cf+Ce*Cc) ) / ( 1.0d0 - Ce*Cb ) )

      Mdot = AirMassFlow * FlowFrac
      CpAirZn        = PsyCpAirFnWTdb(Node(VentSlab(Item)%RadInNode)%HumRat,Node(VentSlab(Item)%RadInNode)%Temp)

      QRadSysSource(SurfNum) = CNumDS * EpsMdotCpAirZn * (AirTempIn - Ck) &
                                /(1.0d0 + (EpsMdotCpAirZn*Cl/Surface(SurfNum)%Area) )

    IF (Surface(SurfNum)%ExtBoundCond > 0 .AND. Surface(SurfNum)%ExtBoundCond /= SurfNum) &
             QRadSysSource(Surface(SurfNum)%ExtBoundCond) = QRadSysSource (SurfNum)
       ! Also set the other side of an interzone!


          AirTempOut(RadSurfNum) = AirTempIn - (QRadSysSource(SurfNum)/(Mdot*CpAirZn))
          AirTempIn = AirTempOut(RadSurfNum)
          ! "Temperature Comparison" Cut-off:
          ! Check to see whether or not the system should really be running.  If
          ! QRadSysSource is negative when we are in heating mode or QRadSysSource
          ! is positive when we are in cooling mode, then the radiant system will
          ! be doing the opposite of its intention.  In this case, the flow rate
          ! is set to zero to avoid heating in cooling mode or cooling in heating
          ! mode.

      IF (RadSurfNum.eq.1) THEN
        IF (((OperatingMode == HeatingMode).AND.(QRadSysSource(SurfNum) <= 0.0d0)) .OR. &
           ((OperatingMode == CoolingMode).AND.(QRadSysSource(SurfNum) >= 0.0d0)) ) THEN
!IF (.not. WarmupFlag) THEN
!  TempComparisonErrorCount = TempComparisonErrorCount + 1
!  IF (TempComparisonErrorCount <= NumOfVentSlabs) THEN
!    CALL ShowWarningError('Radaint Heat exchange is negative in Heating Mode or posive in Cooling Mode')
!    CALL ShowContinueError('Flow to the following ventilated slab will be shut-off to avoid heating in cooling mode or cooling &
!                            in heating mode')
!    CALL ShowContinueError('Ventilated Slab Name = '//TRIM(VentSlab(Item)%Name))
!    CALL ShowContinueError('Surface Name  = '//TRIM(VentSlab(Item)%SurfaceName(RadSurfNum)))
!    CALL ShowContinueError('All node temperature are reseted at the surface temperature of control zone = '// &
!                           RoundSigDigits(TH(VentSlab(Item)%SurfacePtr(1),1,2),2))
!    CALL ShowContinueErrorTimeStamp(' ')
!  ELSE
!    CALL ShowRecurringWarningErrorAtEnd('Ventilated Slab ['//TRIM(VentSlab(Item)%Name)//  &
!                 ']  shut-off occurrence continues due to temperature comparison error.',  &
!                 VentSlab(Item)%CondErrCount)
!  END IF
!END IF

            Node(SlabInNode)%MassFlowRate          = 0.0d0
            Node(FanOutletNode)%MassFlowRate       = 0.0d0
            Node(OAInletNode)%MassFlowRate         = 0.0d0
            Node(MixoutNode)%MassFlowRate          = 0.0d0
            Node(Returnairnode)%MassFlowRate       = 0.0d0
            AirMassFlow                            = 0.0d0

        DO RadSurfNum2 = 1, VentSlab(Item)%NumOfSurfaces
          SurfNum2 = VentSlab(Item)%SurfacePtr(RadSurfNum2)
          QRadSysSource(SurfNum2) = 0.0D0
          IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
            QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
       END DO
            Node(Returnairnode)%Temp = TH(VentSlab(Item)%SurfacePtr(1),1,2)
            Node(FanOutletNode)%Temp = Node(Returnairnode)%Temp
            Node(SlabInNode)%Temp    = Node(FanOutletNode)%Temp
         ! Each Internal node is reseted at the surface temperature

        EXIT ! outer do loop
     END IF
    END IF
          ! Condensation Cut-off:
          ! Check to see whether there are any surface temperatures within the radiant system that have
          ! dropped below the dew-point temperature.  If so, we need to shut off this radiant system.
          ! A safety parameter is added (hardwired parameter) to avoid getting too close to condensation
          ! conditions.

     IF (OperatingMode == CoolingMode) THEN
        DewPointTemp = PsyTdpFnWPb(ZoneAirHumRat(VentSlab(Item)%ZPtr(RadSurfNum)),OutBaroPress)
        DO RadSurfNum2 = 1, VentSlab(Item)%NumOfSurfaces
          IF (TH(VentSlab(Item)%SurfacePtr(RadSurfNum2),1,2) < (DewPointTemp+CondDeltaTemp) ) THEN
          ! Condensation warning--must shut off radiant system
            Node(SlabInNode)%MassFlowRate          = 0.0d0
            Node(FanOutletNode)%MassFlowRate       = 0.0d0
            Node(OAInletNode)%MassFlowRate         = 0.0d0
            Node(MixoutNode)%MassFlowRate          = 0.0d0
            Node(Returnairnode)%MassFlowRate       = 0.0d0
            Node(FanOutletNode)%Temp = Node(SlabInNode)%Temp
            AirMassFlow                            = 0.0d0
            DO RadSurfNum3 = 1, VentSlab(Item)%NumOfSurfaces
              SurfNum2 = VentSlab(Item)%SurfacePtr(RadSurfNum3)
              QRadSysSource(SurfNum2) = 0.0D0
              IF (Surface(SurfNum2)%ExtBoundCond > 0 .AND. Surface(SurfNum2)%ExtBoundCond /= SurfNum2) &
                QRadSysSource(Surface(SurfNum2)%ExtBoundCond) = 0.0D0   ! Also zero the other side of an interzone
            END DO
          ! Produce a warning message so that user knows the system was shut-off due to potential for condensation
            IF (.not. WarmupFlag) THEN
              CondensationErrorCount = CondensationErrorCount + 1
              IF (VentSlab(Item)%CondErrIndex == 0) THEN
                CALL ShowWarningMessage(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//']')
                CALL ShowContinueError('Surface ['//trim(Surface(VentSlab(Item)%SurfacePtr(RadSurfNum2))%Name)//  &
                   '] temperature below dew-point temperature--potential for condensation exists')
                CALL ShowContinueError('Flow to the ventilated slab system will be shut-off to avoid condensation')
                CALL ShowContinueError('Predicted radiant system surface temperature = '// &
                                       trim(RoundSigDigits(TH(VentSlab(Item)%SurfacePtr(RadSurfNum2),1,2),2)))
                CALL ShowContinueError('Zone dew-point temperature + safety factor delta= '//  &
                   trim(RoundSigDigits(DewPointTemp+CondDeltaTemp,2)))
                CALL ShowContinueErrorTimeStamp(' ')
              ENDIF
              IF (CondensationErrorCount == 1) THEN
                  CALL ShowContinueError('Note that there is a '//TRIM(RoundSigDigits(CondDeltaTemp,4))// &
                                         ' C safety built-in to the shut-off criteria')
                  CALL ShowContinueError('Note also that this affects all surfaces that are part of this system')
              END IF
              CALL ShowRecurringWarningErrorAtEnd(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//  &
                         '] condensation shut-off occurrence continues.',  &
                         VentSlab(Item)%CondErrIndex,ReportMinOf=DewPointTemp,ReportMaxOf=DewPointTemp,  &
                         ReportMaxUnits='C',ReportMinUnits='C')
            END IF
            EXIT ! outer do loop
          END IF
        END DO
     END IF
  END DO

  ! Total Radiant Power
    AirOutletTempCheck = 0.0d0
    TotalVentSlabRadPower = 0.0d0
    DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces
      SurfNum              = VentSlab(Item)%SurfacePtr(RadSurfNum)
      TotalVentSlabRadPower     = TotalVentSlabRadPower + QRadSysSource(SurfNum)
      AirOutletTempCheck = AirTempOut(RadSurfNum)
    END DO
    TotalVentSlabRadPower = ZoneMult * TotalVentSlabRadPower

! Intenal Node Temperature Check

       MSlabAirInTemp       = Node(SlabInNode)%Temp

    DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces
       SlabName=VentSlab(Item)%SurfaceName(RadSurfNum)
       MSlabIn = VentSlab(Item)%SlabIn(RadSurfNum)
       MSlabOut = VentSlab(Item)%SlabOut(RadSurfNum)
          VentSlab(Item)%MslabInNode = &
               GetOnlySingleNode(MSlabIn,ErrorsFound,CurrentModuleObject,SlabName, &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsNotParent)
          VentSlab(Item)%MSlabOutNode = &
               GetOnlySingleNode(MSlabOut,ErrorsFound,CurrentModuleObject,SlabName, &
                           NodeType_Air,NodeConnectionType_Internal,1,ObjectIsNotParent)
        MSlabInletNode      = VentSlab(Item)%MslabInNode
        MSlabOutletNode    = VentSlab(Item)%MslabOutNode
        SurfNum              = VentSlab(Item)%SurfacePtr(RadSurfNum)


      IF (AirMassFlow> 0.0d0) THEN

         CpAirZn        = PsyCpAirFnWTdb(Node(VentSlab(Item)%RadInNode)%HumRat,Node(VentSlab(Item)%RadInNode)%Temp)!

         Node(MSlabInletNode)%Temp = MSlabAirInTemp
         Node(MSlabOutletNode)%Temp = Node(MSlabInletNode)%Temp &
                                      -(QRadSysSource(SurfNum)/(AirMassFlow*CpAirZn))
         MSlabAirInTemp = Node(MSlabOutletNode)%Temp
      ELSE
         Node(MSlabInletNode)%Temp = Node(Returnairnode)%Temp
         Node(MSlabOutletNode)%Temp = Node(MSlabInletNode)%Temp
      END IF
    END DO

! Return Air temp Check
      IF (AirMassFlow> 0.0d0) THEN

        CpAirZn                  = PsyCpAirFnWTdb(Node(VentSlab(Item)%RadInNode)%HumRat,Node(VentSlab(Item)%RadInNode)%Temp)
        Node(ReturnAirNode)%Temp = Node(SlabInNode)%Temp - (TotalVentSlabRadPower/(AirMassFlow*CpAirZn))

        IF ((ABS(Node(ReturnAirNode)%Temp-AirOutletTempCheck) > TempCheckLimit) .AND. &
            (ABS(TotalVentSlabRadPower) > ZeroSystemResp) ) THEN ! Return air temperature check did not match calculated temp

          IF (.not. WarmupFlag) THEN
            EnergyImbalanceErrorCount = EnergyImbalanceErrorCount + 1
            IF (VentSlab(Item)%EnrgyImbalErrIndex == 0) THEN
              CALL ShowWarningMessage(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//']')
              CALL ShowContinueError('Ventilated Slab (slab only type) air outlet temperature calculation mismatch.')
              CALL ShowContinueError('This should not happen as it indicates a potential energy imbalance in the calculations.')
              CALL ShowContinueError('However, it could also result from improper input for the ventilated slab or')
              CALL ShowContinueError('illogical control temperatures.  Check your input for this ventilated slab and')
              CALL ShowContinueError('also look at the internal data shown below.')
              CALL ShowContinueError('Predicted return air temperature [C] from the overall energy balance = '// &
                                     trim(RoundSigDigits(Node(ReturnAirNode)%Temp,4)))
              CALL ShowContinueError('Predicted return air temperature [C] from the slab section energy balances = '// &
                                     trim(RoundSigDigits(AirOutletTempCheck,4)))
              CALL ShowContinueError('Total energy rate (power) [W] added to the slab = '// &
                                     trim(RoundSigDigits(TotalVentSlabRadPower,4)))
              CALL ShowContinueErrorTimeStamp(' ')
            ENDIF
            CALL ShowRecurringWarningErrorAtEnd(cMO_VentilatedSlab//' ['//TRIM(TRIM(VentSlab(Item)%Name))//  &
                       '] temperature calculation mismatch occurrence continues.',  &
                       VentSlab(Item)%EnrgyImbalErrIndex)
          END IF
        END IF

      ELSE
        Node(ReturnAirNode)%Temp = Node(SlabInNode)%Temp
      END IF


          ! Now that we have the source/sink term, we must redo the heat balances to obtain
          ! the new SumHATsurf value for the zone.  Note that the difference between the new
          ! SumHATsurf and the value originally calculated by the heat balance with a zero
          ! source for all radiant systems in the zone is the load met by the system (approximately).


  CALL CalcHeatBalanceOutsideSurf
  CALL CalcHeatBalanceInsideSurf

  END IF ! SeriesSlabs

END IF !(AirMassFlow > 0.0d0)

  RETURN

END SUBROUTINE CalcVentilatedSlabRadComps




SUBROUTINE SimVentSlabOAMixer(Item)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This responsibility of this subroutine is to set the air flow rates
          ! through the mixing box portion of the Ventilated Slab and then perform
          ! an energy balance to arrive at outlet conditions which then would
          ! serve as inlet conditions to the coils (or outlet conditions for
          ! the device).  There is some question as to whether this needs to be
          ! called every time the coils and fan are called since how the fans and
          ! coil operate won't presumable change how the mixer operates.  The
          ! method in which this routine is called is slightly cleaner though
          ! from a code readability standpoint though less efficient.

          ! METHODOLOGY EMPLOYED:
          ! The OAMassFlowRate has already been calculated in the main control
          ! algorithm.  Use this flow rate to establish all of the other flow
          ! rates and perform an energy balance on the mixing of the return and
          ! outdoor air streams.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE                                         ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Item           ! System index in Ventilated Slab array

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        :: AirRelNode              ! relief air node number in ventilated slab loop
  INTEGER        :: InletNode               ! inlet node number for ventilated slab loop
  REAL(r64)           :: OAFraction         ! Outside air fraction of inlet air
  INTEGER        :: OAMixOutNode   ! outside air mixer outlet node for ventilated slab loop
  INTEGER        :: OutsideAirNode    ! outside air node number in ventilated slab loop

          ! FLOW:
  AirRelNode     = VentSlab(Item)%AirReliefNode
  InletNode      = VentSlab(Item)%ReturnAirNode
  OAMixOutNode   = VentSlab(Item)%OAMixerOutNode
  OutsideAirNode = VentSlab(Item)%OutsideAirNode

          ! "Resolve" the air flow rates...

  Node(OutsideAirNode)%MassFlowRate           = OAMassFlowRate
  Node(OutsideAirNode)%MassFlowRateMinAvail   = OAMassFlowRate
  Node(OutsideAirNode)%MassFlowRateMaxAvail   = OAMassFlowRate

  Node(AirRelNode)%MassFlowRate                 = OAMassFlowRate
  Node(AirRelNode)%MassFlowRateMinAvail         = OAMassFlowRate
  Node(AirRelNode)%MassFlowRateMaxAvail         = OAMassFlowRate



  Node(OAMixOutNode)%MassFlowRate         = Node(InletNode)%MassFlowRate
  Node(OAMixOutNode)%MassFlowRateMinAvail = Node(InletNode)%MassFlowRate
  Node(OAMixOutNode)%MassFlowRateMaxAvail = Node(InletNode)%MassFlowRate

          ! "Inlet" conditions for InletNode and OutsideAirNode have already
          ! been set elsewhere so we just need to set the "outlet" conditions
  Node(AirRelNode)%Temp     = Node(InletNode)%Temp
  Node(AirRelNode)%Press    = Node(InletNode)%Press
  Node(AirRelNode)%HumRat   = Node(InletNode)%HumRat
  Node(AirRelNode)%Enthalpy = Node(InletNode)%Enthalpy

  IF (Node(InletNode)%MassFlowRate > 0.0d0) THEN

    OAFraction = Node(OutsideAirNode)%MassFlowRate/Node(InletNode)%MassFlowRate

  ELSE
    OAFraction = 0.0d0
  END IF

  Node(InletNode)%Enthalpy = PsyHFnTdbw(Node(InletNode)%Temp, Node(Inletnode)%Humrat)

          ! Perform an energy and moisture mass balance on the mixing portion of the OA Mixer of the ventilated slab
  Node(OAMixOutNode)%Enthalpy = OAFraction*Node(OutsideAirNode)%Enthalpy &
                               +(1.0d0-OAFraction)*Node(InletNode)%Enthalpy
  Node(OAMixOutNode)%HumRat   = OAFraction*Node(OutsideAirNode)%HumRat &
                               +(1.0d0-OAFraction)*Node(InletNode)%HumRat

          ! Find the other key state points based on calculated conditions
  Node(OAMixOutNode)%Temp  = PsyTdbFnHW(Node(OAMixOutNode)%Enthalpy,Node(OAMixOutNode)%HumRat)
  Node(OAMixOutNode)%Press = Node(InletNode)%Press

  RETURN

END SUBROUTINE SimVentSlabOAMixer


SUBROUTINE UpdateVentilatedSlab(Item,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Young Tae Chae, Rick Strand
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
  USE DataHeatBalFanSys,  ONLY : MAT

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep !unused1208
  INTEGER, INTENT(IN) :: Item  ! Index for the ventilated slab under consideration within the derived types

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: CpAppAir            ! Specific heat of air
  INTEGER :: RadSurfNum         ! DO loop counter for radiant surfaces in the ventilated slab
  INTEGER :: SurfNum            ! Surface index number for the current ventilated slab
  INTEGER :: AirInletNode     ! Node number for the air side inlet of the ventilated slab
  REAL(r64)    :: TotalHeatSource    ! Total heat source or sink for a particular system (sum of all surface source/sinks)
  INTEGER :: TotRadSurfaces     ! Total number of radiant surfaces in this system
  REAL(r64)    :: AirMassFlow      ! Flow rate of water in the radiant system
  INTEGER :: AirOutletNode    ! Node number for the water side outlet of the radiant system
  INTEGER :: FanOutNode    ! Node number for the water side outlet of the radiant system
  REAL(r64)    :: ZoneMult           ! Zone multiplier
  INTEGER :: ZoneNum            ! Zone for this ventilated slab
  INTEGER :: MixOutNode    ! Node number for the water side outlet of the radiant system
  INTEGER :: OANode    ! Node number for the water side outlet of the radiant system
  REAL(r64)    :: OAFraction            ! Outside air fraction of inlet air
  INTEGER :: ZoneInletNode     ! Node number for the air side inlet of the ventilated slab
          ! FLOW:
     ZoneNum       = VentSlab(Item)%ZonePtr
     TotRadSurfaces = VentSlab(Item)%NumOfSurfaces
     MixOutNode    = VentSlab(Item)%OAMixerOutNode
     OANode        = VentSlab(Item)%OutsideAirNode
     AirOutletNode = VentSlab(Item)%RadInNode
     FanOutNode    = VentSlab(Item)%FanOutletNode
     AirMassFlow   = Node(AirOutletNode)%MassFlowRate
     ZoneInletNode = VentSlab(Item)%ZoneAirInNode
     CpAppAir      = PsyCpAirFnWTdb(Node(AirOutletNode)%HumRat, Node(AirOutletNode)%Temp)
     AirInletNode  = VentSlab(Item)%ReturnAirNode



  DO RadSurfNum = 1, TotRadSurfaces

    SurfNum = VentSlab(Item)%SurfacePtr(RadSurfNum)

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

          ! First sum up all of the heat sources/sinks associated with this system
    TotalHeatSource = 0.0d0
    DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces
      SurfNum         = VentSlab(Item)%SurfacePtr(RadSurfNum)
      TotalHeatSource = TotalHeatSource + QRadSysSource(SurfNum)
    END DO
    ZoneNum         = VentSlab(Item)%ZonePtr
    ZoneMult        = REAL(Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier,r64)
    TotalHeatSource = ZoneMult * TotalHeatSource


         ! Update the heating side of things

    IF ((CpAppAir > 0.0d0) .AND. (AirMassFlow > 0.0d0)) THEN

       IF ((VentSlab(Item)%SysConfg == SlabOnly).OR.(VentSlab(Item)%SysConfg==SeriesSlabs)) THEN
            Node(AirInletNode) = Node(AirInletNode)
            Node(AirInletNode)%Temp = Node(AirOutletNode)%Temp &
                                        -TotalHeatSource/AirMassFlow/CpAppAir
            Node(AirInletNode)%MassFlowRate = Node(AirOutletNode)%MassFlowRate
            Node(AirInletNode)%HumRat = Node(AirOutletNode)%HumRat

       ELSE IF (VentSlab(Item)%SysConfg == SlabandZone) THEN
            Node(ZoneInletNode) = Node(ZoneInletNode)
            Node(ZoneInletNode)%Temp = Node(AirOutletNode)%Temp &
                                        -TotalHeatSource/AirMassFlow/CpAppAir
            Node(ZoneInletNode)%MassFlowRate = Node(AirOutletNode)%MassFlowRate
            Node(ZoneInletNode)%HumRat = Node(AirOutletNode)%HumRat
           Node(VentSlab(Item)%ReturnAirNode)%Temp = MAT(Zonenum)
       END IF


    ELSE
       IF ((VentSlab(Item)%SysConfg == SlabOnly).OR.(VentSlab(Item)%SysConfg == SeriesSlabs)) THEN
            Node(FanOutNode)= Node(AirOutletNode)
            QRadSysSource(SurfNum) = 0.0d0

       ELSE IF (VentSlab(Item)%SysConfg == SlabandZone) THEN
            Node(ZoneInletNode) = Node(AirInletNode)
            Node(FanOutNode)= Node(AirOutletNode)   ! Fan Resolve
            QRadSysSource(SurfNum) = 0.0d0
       END IF

     END IF




! Resolve mixouttemp


  IF (Node(AirInletNode)%MassFlowRate > 0.0d0) THEN

    OAFraction = Node(OANode)%MassFlowRate/Node(AirInletNode)%MassFlowRate

  ELSE
    OAFraction = 0.0d0
  END IF

  IF (OAFraction <= 0.0d0) Then

  Node(MixOutNode)%HumRat   = Node(AirInletNode)%HumRat
  Node(MixOutNode)%Temp     = Node(AirInletNode)%Temp

  Else

  Node(MixOutNode)%Enthalpy = OAFraction*Node(OANode)%Enthalpy &
                               +(1.0d0-OAFraction)*Node(AirInletNode)%Enthalpy
  Node(MixOutNode)%HumRat   = OAFraction*Node(OANode)%HumRat &
                               +(1.0d0-OAFraction)*Node(AirInletNode)%HumRat

  Node(MixOutNode)%Temp  = PsyTdbFnHW(Node(MixOutNode)%Enthalpy,Node(MixOutNode)%HumRat)

  END IF

  RETURN

END SUBROUTINE UpdateVentilatedSlab

REAL(r64) FUNCTION CalcVentSlabHXEffectTerm(Item,Temperature,AirMassFlow,FlowFraction,CoreLength,CoreDiameter,CoreNumbers)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   December 2000
          !       MODIFIED       June 2008 (air properties)
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
          ! Property data for air shown below as parameters taken from
          !   Mills, Heat Transfer, Table A.7.
          ! Heat exchanger information also from Incropera and DeWitt.
          ! Code based loosely on code from IBLAST program (research version)

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : PI

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Item      ! Index number of radiant system under consideration
  REAL(r64),    INTENT(IN) :: Temperature    ! Temperature of air entering the radiant system, in C
  REAL(r64),    INTENT(IN) :: AirMassFlow  ! Mass flow rate of water in the radiant system, in kg/s
  REAL(r64),    INTENT(IN) :: FlowFraction   ! Mass flow rate fraction for this surface in the radiant system
  REAL(r64),    INTENT(IN) :: CoreLength     ! Length of tubing in the radiant system, in m
  REAL(r64),    INTENT(IN) :: CoreDiameter   ! Inside diameter of the tubing in the radiant system, in m
  REAL(r64),    INTENT(IN) :: CoreNumbers    !

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: MaxLaminarRe = 2300.d0    ! Maximum Reynolds number for laminar flow
  INTEGER, PARAMETER :: NumOfPropDivisions = 13
  REAL(r64), PARAMETER :: MaxExpPower = 50.d0       ! Maximum power after which EXP argument would be zero for DP variables
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Temps=  &   ! Temperature, in C
                   (/1.85d0,6.85d0,11.85d0,16.85d0,21.85d0,26.85d0,31.85d0,36.85d0,41.85d0,46.85d0,51.85d0,56.85d0,61.85d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Mu=  &      ! Viscosity, in Ns/m2
                   (/0.0000088d0,0.0000176d0,0.00001781d0,0.00001802d0,0.000018225d0,0.00001843d0,0.00001865d0,0.00001887d0,  &
                     0.00001908d0,0.00001929d0,0.0000195d0,0.00001971d0,0.00001992d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Conductivity=  &     ! Conductivity, in W/mK
                   (/0.01275d0,0.0255d0,0.0258d0,0.0261d0,0.0264d0,0.0267d0,0.02705d0,0.0274d0,0.02775d0,0.0281d0,  &
                     0.0284d0,0.0287d0,0.01435d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Pr=  &      ! Prandtl number (dimensionless)
                   (/0.69d0,0.69d0,0.69d0,0.69d0,0.69d0,0.69d0,0.69d0,0.69d0,0.69d0,0.69d0,0.69d0,0.69d0,0.69d0/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Index
  REAL(r64)    :: InterpFrac
  REAL(r64)    :: NuD
  REAL(r64)    :: ReD
  REAL(r64)    :: NTU
  REAL(r64)    :: CpAppAir
  REAL(r64)    :: Kactual
  REAL(r64)    :: MUactual
  REAL(r64)    :: PRactual
  REAL(r64)    :: SysAirMassFlow            ! Specific heat of air


          ! FLOW:
          ! First find out where we are in the range of temperatures
  Index = 1
  DO WHILE (Index <= NumOfPropDivisions)
    IF (Temperature < Temps(Index)) EXIT ! DO loop
    Index = Index + 1
  END DO

          ! Initialize thermal properties of Air
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
    CpAppAir   = PsyCpAirFnWTdb(Node(VentSlab(Item)%RadInNode)%HumRat, Node(VentSlab(Item)%RadInNode)%Temp)
    SysAirMassFlow = AirMassFlow/CoreNumbers

          ! Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
  ReD = 4.0d0 * SysAirMassFlow * FlowFraction / ( PI * MUactual * CoreDiameter )

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
  NTU = PI * Kactual * NuD * CoreLength / (SysAirMassFlow * CpAppAir)   ! FlowFraction cancels out here

          ! Calculate Epsilon*MassFlowRate*Cp
  IF (NTU > MaxExpPower) THEN
    CalcVentSlabHXEffectTerm = FlowFraction*SysAirMassFlow*CpAppAir
  ELSE
    CalcVentSlabHXEffectTerm = (1.d0-EXP(-NTU))*FlowFraction*SysAirMassFlow*CpAppAir
  END IF

  RETURN

END FUNCTION CalcVentSlabHXEffectTerm

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


SUBROUTINE ReportVentilatedSlab(Item)

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
!unused-12/12/08  USE FluidProperties, ONLY : GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Item  ! Index for the ventilated slab under consideration within the derived types

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RadSurfNum         ! DO loop counter for radiant surfaces in the system
  INTEGER :: SurfNum            ! Surface number (index) in Surface derived type
  REAL(r64)    :: TotalVentSlabRadPower   ! Total source/sink power for the radiant system (sum of all surfaces of the system)
  REAL(r64)    :: ZoneMult           ! Total zone multiplier to apply to the system level variables

          ! FLOW:



! Slab Part
  TotalVentSlabRadPower = 0.0d0
  ZoneMult         = 1.0d0

      DO RadSurfNum = 1, VentSlab(Item)%NumOfSurfaces
        SurfNum          = VentSlab(Item)%SurfacePtr(RadSurfNum)
        TotalVentSlabRadPower = TotalVentSlabRadPower + QRadSysSource(SurfNum)
      END DO
      ZoneMult = REAL(Zone(VentSlab(Item)%ZonePtr)%Multiplier * Zone(VentSlab(Item)%ZonePtr)%ListMultiplier,r64)
      TotalVentSlabRadPower = ZoneMult * TotalVentSlabRadPower
      VentSlab(Item)%RadHeatingPower = 0.0d0
      VentSlab(Item)%RadCoolingPower = 0.0d0

        IF (TotalVentSlabRadPower >= 0.01d0) Then

      VentSlab(Item)%RadHeatingPower = +TotalVentSlabRadPower
        ELSE

      VentSlab(Item)%RadCoolingPower = -TotalVentSlabRadPower
        END IF


      VentSlab(Item)%RadHeatingEnergy = VentSlab(Item)%RadHeatingPower*TimeStepSys*SecInHour
      VentSlab(Item)%RadCoolingEnergy = VentSlab(Item)%RadCoolingPower*TimeStepSys*SecInHour


!Coil Part
  VentSlab(Item)%HeatCoilEnergy     = VentSlab(Item)%HeatCoilPower*TimeStepSys*SecInHour
  VentSlab(Item)%SensCoolCoilEnergy = VentSlab(Item)%SensCoolCoilPower*TimeStepSys*SecInHour
  VentSlab(Item)%LateCoolCoilEnergy = VentSlab(Item)%LateCoolCoilPower*TimeStepSys*SecInHour
  VentSlab(Item)%TotCoolCoilEnergy  = VentSlab(Item)% TotCoolCoilPower*TimeStepSys*SecInHour
  VentSlab(Item)%ElecFanEnergy     = VentSlab(Item)%ElecFanPower*TimeStepSys*SecInHour

  IF ((VentSlab(Item)%SysConfg == SlabOnly).OR. (VentSlab(Item)%SysConfg == SeriesSlabs)) THEN
    VentSlab(Item)%SlabInTemp =   Node(VentSlab(Item)%RadInNode)%Temp
    VentSlab(Item)%SlabOutTemp =   Node(VentSlab(Item)%ReturnAirNode)%Temp

  ELSE IF (VentSlab(Item)%Sysconfg == Slabandzone) THEN
    VentSlab(Item)%SlabInTemp =   Node(VentSlab(Item)%RadInNode)%Temp
    VentSlab(Item)%ZoneInletTemp = Node(VentSlab(Item)%ZoneAirInNode)%Temp
    VentSlab(Item)%SlabOutTemp =   Node(VentSlab(Item)%ReturnAirNode)%Temp
  END IF

  VentSlab(Item)%ReturnAirTemp = Node(VentSlab(Item)%ReturnAirNode)%Temp
  VentSlab(Item)%FanOutletTemp = Node(VentSlab(Item)%FanOutletNode)%Temp

  RETURN

END SUBROUTINE ReportVentilatedSlab

!*****************************************************************************************

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

END MODULE VentilatedSlab
