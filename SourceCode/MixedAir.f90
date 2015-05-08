MODULE MixedAir

  ! Module containing the routines dealing with the mixed air portion
  ! of the HVAC air loop.

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   October 1998
  !       MODIFIED       Shirey/Raustad FSEC, June/Aug 2003, Jan 2004
  !                      Lawrie, March 2006 - Module order (per template)
  !                      Craig Wray 22Aug2010 - Added Fan ComponentModel
  !                      Chandan Sharma, FSEC, 25Aug 2011 - Added ProportionalControl
  !                           to enhance CO2 based DCV control
  !                      Feb 2013 Bereket Nigusse, FSEC
  !                        Added DX Coil Model For 100% OA systems
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! simulate the mixed air portion of the EPlus air loop.

  ! METHODOLOGY EMPLOYED:
  ! An algorithmic controller will be employed - there is no attempt to
  ! simulate real controllers for the economizer. The mixed air controller
  ! will sense various node conditions and set some node flow rates.  Mixed
  ! air components will operate with predetermined flow rates.

  ! REFERENCES:

  ! OTHER NOTES:
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataAirLoop
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, MaxNameLength, NumOfZones, SysSizingCalc,  &
                           AnyEnergyManagementSystemInModel, DoZoneSizing, ScheduleAlwaysOn, OutputFileInits
USE DataInterfaces,  ONLY: ShowFatalError, ShowSevereError, ShowWarningError, ShowContinueError, SetupOutputVariable, &
                           SetupEMSActuator, ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
USE DataEnvironment
USE DataHVACGlobals
USE ScheduleManager
USE DataSizing
USE DataContaminantBalance, ONLY: Contaminant, OutdoorCO2, ZoneCO2GainFromPeople, ZoneAirCO2, OutdoorGC
USE FaultsManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: NoLockoutPossible = 0
INTEGER, PARAMETER :: LockoutWithHeatingPossible = 1
INTEGER, PARAMETER :: LockoutWithCompressorPossible = 2

INTEGER, PARAMETER :: NoEconomizer = 0
! Changed by Amit as a part on New Feature Proposal
INTEGER, PARAMETER :: FixedDryBulb = 1
INTEGER, PARAMETER :: FixedEnthalpy = 2
INTEGER, PARAMETER :: DifferentialDryBulb = 3
INTEGER, PARAMETER :: DifferentialEnthalpy = 4
INTEGER, PARAMETER :: FixedDewpointAndDryBulb = 5
INTEGER, PARAMETER :: ElectronicEnthalpy = 6
INTEGER, PARAMETER :: DifferentialDryBulbAndEnthalpy = 7
! coil operation
INTEGER, PARAMETER :: On =  1              ! normal coil operation
INTEGER, PARAMETER :: Off = 0              ! signal coil shouldn't run
! component types addressed by this module
INTEGER, PARAMETER :: OAMixer_Num      = 1
INTEGER, PARAMETER :: Fan_Simple_CV    = 2
INTEGER, PARAMETER :: Fan_Simple_VAV   = 3
INTEGER, PARAMETER :: WaterCoil_SimpleCool = 4
INTEGER, PARAMETER :: WaterCoil_Cooling = 5
INTEGER, PARAMETER :: WaterCoil_SimpleHeat = 6
INTEGER, PARAMETER :: SteamCoil_AirHeat = 7
INTEGER, PARAMETER :: WaterCoil_DetailedCool = 8
INTEGER, PARAMETER :: Coil_ElectricHeat = 9
INTEGER, PARAMETER :: Coil_GasHeat = 10
INTEGER, PARAMETER :: WaterCoil_CoolingHXAsst = 11
INTEGER, PARAMETER :: DXSystem = 12
INTEGER, PARAMETER :: HeatXchngr = 13
INTEGER, PARAMETER :: Desiccant = 14
INTEGER, PARAMETER :: Unglazed_SolarCollector = 15
INTEGER, PARAMETER :: EvapCooler = 16
INTEGER, PARAMETER :: PVT_AirBased = 17
INTEGER, PARAMETER :: Fan_ComponentModel = 18 !cpw22Aug2010 (new)
INTEGER, PARAMETER :: DXHeatPumpSystem = 19
INTEGER, PARAMETER :: Coil_UserDefined = 20
INTEGER, PARAMETER :: UnitarySystem    = 21

INTEGER, PARAMETER :: ControllerSimple = 1
INTEGER, PARAMETER :: ControllerOutsideAir = 2
INTEGER, PARAMETER :: ControllerStandAloneERV = 3

!!Zone Outdoor Air Method
!INTEGER, PARAMETER :: ZOAM_FlowPerPerson = 1  ! set the outdoor air flow rate based on number of people in the zone
!INTEGER, PARAMETER :: ZOAM_FlowPerZone = 2    ! sum the outdoor air flow rate per zone based on user input
!INTEGER, PARAMETER :: ZOAM_FlowPerArea = 3    ! sum the outdoor air flow rate based on zone area
!INTEGER, PARAMETER :: ZOAM_FlowPerACH = 4     ! sum the outdoor air flow rate based on number of air changes for the zone
!INTEGER, PARAMETER :: ZOAM_Sum = 5            ! sum the outdoor air flow rate of the people component and the space floor area component
!INTEGER, PARAMETER :: ZOAM_Max = 6            ! use the maximum of the outdoor air flow rate of the people component and
!                                              ! the space floor area component
!
!!System Outdoor Air Method
!INTEGER, PARAMETER :: SOAM_ZoneSum = 1  ! Sum the outdoor air flow rates of all zones
!INTEGER, PARAMETER :: SOAM_VRP = 2      ! Use ASHRAE Standard 62.1-2007 to calculate the system level outdoor air flow rates
!                                        !  considering the zone air distribution effectiveness and the system ventilation efficiency
!INTEGER, PARAMETER :: SOAM_IAQP = 3     ! Use ASHRAE Standard 62.1-2007 IAQP to calculate the system level outdoor air flow rates
!                                        ! based on the CO2 setpoint
!INTEGER, PARAMETER :: SOAM_ProportionalControl = 4     ! Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
!                                                       ! to calculate the system level outdoor air flow rates
!INTEGER, PARAMETER :: SOAM_IAQPGC = 5   ! Use ASHRAE Standard 62.1-2004 IAQP to calculate the system level outdoor air flow rates
!                                        ! based on the generic contaminant setpoint
!INTEGER, PARAMETER :: SOAM_IAQPCOM = 6  ! Take the maximum outdoor air rate from both CO2 and generic contaminant controls
!                                        ! based on the generic contaminant setpoint

Character(len=*), PARAMETER, DIMENSION(8) :: CurrentModuleObjects=  &
  (/'AirLoopHVAC:OutdoorAirSystem                 ',  &
    'AirLoopHVAC:OutdoorAirSystem:EquipmentList   ',  &
    'AirLoopHVAC:ControllerList                   ',  &
    'AvailabilityManagerAssignmentList            ',  &
    'Controller:OutdoorAir                        ',  &
    'ZoneHVAC:EnergyRecoveryVentilator:Controller ',  &
    'Controller:MechanicalVentilation             ',  &
    'OutdoorAir:Mixer                             '   /)

! Parameters below (CMO - Current Module Object.  used primarily in Get Inputs)
! Multiple Get Input routines in this module or these would be in individual routines.
INTEGER, PARAMETER :: CMO_OASystem = 1
INTEGER, PARAMETER :: CMO_AirLoopEqList = 2
INTEGER, PARAMETER :: CMO_ControllerList = 3
INTEGER, PARAMETER :: CMO_SysAvailMgrList = 4
INTEGER, PARAMETER :: CMO_OAController = 5
INTEGER, PARAMETER :: CMO_ERVController = 6
INTEGER, PARAMETER :: CMO_MechVentilation = 7
INTEGER, PARAMETER :: CMO_OAMixer = 8

!Type declarations in MixedAir module

TYPE ControllerListProps
  CHARACTER(len=MaxNameLength) :: Name  = ' '
  INTEGER :: NumControllers = 0  ! number of controllers on list
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: ControllerType
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: ControllerName
END TYPE ControllerListProps

TYPE OAControllerProps                     ! Derived type for Outside Air Controller data
  CHARACTER(len=MaxNameLength) :: Name = ' '
  CHARACTER(len=MaxNameLength) :: ControllerType = ' '
  INTEGER      :: ControllerType_Num   = 0          ! Parameter equivalent of controller type
  INTEGER      :: OACtrlIndex          = 0
  INTEGER      :: Lockout              = 0          ! 0=NoLockoutPossible; 1=LockoutWithHeatingPossible;
                                                    ! 2=LockoutWithCompressorPossible;
  LOGICAL      :: FixedMin             = .true.     ! Fixed Minimum or Proportional Minimum
  REAL(r64)    :: TempLim              = 0.0d0        ! Temperature Limit
  REAL(r64)    :: TempLowLim           = 0.0d0        ! Temperature Lower Limit
  REAL(r64)    :: EnthLim              = 0.0d0        ! Enthalpy Limit
  REAL(r64)    :: DPTempLim            = 0.0d0        ! Dew Point Temperature Limit
  INTEGER      :: EnthalpyCurvePtr     = 0          ! Electronic Enthalpy Curve Index (max HumRat = f[OAT])
  REAL(r64)    :: MinOA                = 0.0d0        ! Minimum outside air flow (m3/sec)
  REAL(r64)    :: MaxOA                = 0.0d0        ! Maximum outside air flow (m3/sec)
  INTEGER      :: Econo                = 0          ! 0 = NoEconomizer, 1 = FixedDryBulb, 2 = FixedEnthalpy, 3=DifferentialDryBulb,
                                                    ! 4=DifferentialEnthalpy, 5=FixedDewPointAndDryBulb, 6 = ElectronicEnthalpy,
                                                    ! 7 =DifferentialDryBulbAndEnthalpy
  Logical      :: EconBypass           = .FALSE.    ! ModulateFlow =FALSE , MinimumFlowWithBypass =TRUE
  INTEGER      :: MixNode              = 0          ! Controlled node (mixed air node)
  INTEGER      :: OANode               = 0          ! Actuated node (outside air node)
  INTEGER      :: InletNode            = 0          ! Inlet Air Node for into Mixer  (BTG Nov 2004)
  INTEGER      :: RelNode              = 0          ! Relief Air Node Number
  INTEGER      :: RetNode              = 0          ! Return Air Node Number
  CHARACTER(len=MaxNameLength) :: MinOASch = ' '    ! Name of the minimum outside air schedule
  INTEGER      :: MinOASchPtr          = 0          ! Index to the minimum outside air schedule
  REAL(r64)    :: RelMassFlow          = 0.0d0
  REAL(r64)    :: OAMassFlow           = 0.0d0
  REAL(r64)    :: ExhMassFlow          = 0.0d0
  REAL(r64)    :: MixMassFlow          = 0.0d0
  REAL(r64)    :: InletTemp            = 0.0d0
  REAL(r64)    :: InletEnth            = 0.0d0
  REAL(r64)    :: InletPress           = 0.0d0
  REAL(r64)    :: InletHumRat          = 0.0d0
  REAL(r64)    :: OATemp               = 0.0d0
  REAL(r64)    :: OAEnth               = 0.0d0
  REAL(r64)    :: OAPress              = 0.0d0
  REAL(r64)    :: OAHumRat             = 0.0d0
  REAL(r64)    :: RetTemp              = 0.0d0
  REAL(r64)    :: RetEnth              = 0.0d0
  REAL(r64)    :: MixSetTemp           = 0.0d0
  REAL(r64)    :: MinOAMassFlowRate    = 0.0d0        ! Minimum outside air flow (kg/s)
  REAL(r64)    :: MaxOAMassFlowRate    = 0.0d0        ! Maximum outside air flow (kg/s)
  INTEGER      :: ZoneEquipZoneNum     = 0
  CHARACTER(len=MaxNameLength) :: VentilationMechanicalName = ' ' ! Name of ventilation:mechanical object used for DCV
  INTEGER      :: VentMechObjectNum = 0                ! Index to VENTILATION:MECHANICAL object for this controller
  INTEGER      :: HumidistatZoneNum = 0                ! zone number where humidistat is located
  INTEGER      :: NodeNumofHumidistatZone = 0          ! node number of zone where humidistat is located
  REAL(r64)    :: HighRHOAFlowRatio = 1.0d0              ! Modify ratio with respect to maximum outdoor air flow rate (high RH)
  LOGICAL      :: ModifyDuringHighOAMoisture = .FALSE. ! flag to Modify outdoor air flow, TRUE when modify any time
                                                       ! FALSE when modify only when indoor air humrat is less than outdoor HR
  INTEGER      :: EconomizerOASchedPtr = 0             ! schedule to modify outdoor air flow
  CHARACTER(len=MaxNameLength) :: MinOAflowSch = ' '   ! Name of the Minimum fraction of Design/Mixed Mass of air
  CHARACTER(len=MaxNameLength) :: MaxOAflowSch = ' '   ! Name of the Maximum fraction of Design/Mixed Mass of air
  INTEGER      :: MinOAflowSchPtr          = 0         ! Index to the minimum outside air schedule
  INTEGER      :: MaxOAflowSchPtr          = 0         ! Index to the minimum outside air schedule

!   Economizer Status, which is currently following the EconomizerOperationFlag, might be something like "Economizer status
!   indicates when the conditions are favorable for the economizer to operate (i.e., none of the control limits have been exceeded).
!   While this status signal indicates favorable conditions for economizer operation, it does not guarantee that the air-side
!   economizer has increased outdoor air flow above the minimum level since the actual outdoor air flow rate is also governed
!   by other controls (e.g., mixed air setpoint tempeature, time of day economizer control, etc.).

  INTEGER      :: EconomizerStatus         = 0    ! Air Economizer status (1 = on, 0 = off or economizer not exists)
  INTEGER      :: HeatRecoveryBypassStatus = 0    ! OA Sys Heat Recovery Bypass status (1 = on, 0 = off or economizer not exists)
  INTEGER      :: HRHeatingCoilActive      = 0    ! OA Sys Heat Recovery Heating Coil Was Active status (1 = on, 0 = off)
  REAL(r64)    :: MixedAirTempAtMinOAFlow  = 0.d0 ! calculated mixed air temp when using special HX bypass control
  INTEGER      :: HighHumCtrlStatus        = 0    ! High Humidity Control status (1 = on, 0 = off or high hum ctrl not used)
  REAL(r64)    :: OAFractionRpt            = 0.0d0   ! Actual outdoor air fraction for reporting (based on mixed air flow rate),
                                                     ! 0 to 1 (normally)
  REAL(r64)    :: MinOAFracLimit           = 0.0d0   ! Minimum OA fraction limit
  LOGICAL      :: EMSOverrideOARate        = .FALSE. ! if true, EMS is calling to override OA rate
  REAL(r64)    :: EMSOARateValue           = 0.0D0   ! Value EMS is directing to use. [kg/s]
  INTEGER      :: HeatRecoveryBypassControlType = BypassWhenWithinEconomizerLimits    ! User input selects type of heat recovery optimization
END TYPE OAControllerProps

TYPE VentilationMechanicalProps   ! Derived type for Ventilation:Mechanical data
  CHARACTER(len=MaxNameLength)  :: Name              =' ' ! Name of Ventilation:Mechanical object
  CHARACTER(len=MaxNameLength)  :: SchName           =' ' ! Name of the mechanical ventilation schedule
  INTEGER                       :: SchPtr    = 0  ! Index to the mechanical ventilation schedule
  LOGICAL                       :: DCVFlag           = .FALSE. ! if true, implement OA based on demand controlled ventilation
  INTEGER                       :: NumofVentMechZones= 0  ! Number of zones with mechanical ventilation
  REAL(r64)                     :: TotAreaOAFlow     =0.0d0 ! Total outdoor air flow rate for all zones per area (m3/s/m2)
  REAL(r64)                     :: TotPeopleOAFlow   =0.0d0 ! Total outdoor air flow rate for all PEOPLE objects in zones (m3/s)
  REAL(r64)                     :: TotZoneOAFlow     =0.0d0 ! Total outdoor air flow rate for all zones (m3/s)
  REAL(r64)                     :: TotZoneOAACH      =0.0d0 ! Total outdoor air flow rate for all zones Air Changes per hour (m3/s/m3)
  INTEGER                       :: SystemOAMethod = 0         ! System Outdoor Air Method - SOAM_ZoneSum, SOAM_VRP
  REAL(r64)                     :: ZoneMaxOAFraction =1.0d0 ! Zone maximum outdoor air fraction

  REAL(r64), DIMENSION(:),ALLOCATABLE:: ZoneOAAreaRate          ! Mechanical ventilation rate (m3/s/m2) for each zone
  REAL(r64), DIMENSION(:),ALLOCATABLE:: ZoneOAPeopleRate        ! Mechanical ventilation rate (m3/s/person) for each zone
  REAL(r64), DIMENSION(:),ALLOCATABLE:: ZoneOAFlow        ! OA Flow Rate (m3/s/zone) for each zone
  REAL(r64), DIMENSION(:),ALLOCATABLE:: ZoneOAACH         ! OA ACH (m3/s/volume) for each zone
  INTEGER, DIMENSION(:),ALLOCATABLE:: Zone                ! Zones requiring mechanical ventilation
  INTEGER,DIMENSION(:),ALLOCATABLE :: ZoneDesignSpecOAObjIndex ! index of the design specification outdoor air object
                                                                                  ! for each zone in zone list
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE:: ZoneDesignSpecOAObjName ! name of the design specification outdoor air object
                                                                                  ! for each zone in zone list
  INTEGER    :: CO2MaxMinLimitErrorCount   = 0  ! Counter when max CO2 concentration < min CO2 concentration
                                                ! For SOAM_ProportionalControl
  INTEGER    :: CO2MaxMinLimitErrorIndex   = 0  ! Index for max CO2 concentration < min CO2 concentration recurring error message
                                                ! For SOAM_ProportionalControl
  INTEGER    :: CO2GainErrorCount   = 0  ! Counter when CO2 generation from people is zero for SOAM_ProportionalControl
  INTEGER    :: CO2GainErrorIndex   = 0  ! Index for recurring error message when CO2 generation from people is zero
                                         ! For SOAM_ProportionalControl

  REAL(r64), DIMENSION(:),ALLOCATABLE:: ZoneADEffCooling       ! Zone air distribution effectiveness in cooling mode
                                                               ! for each zone
  REAL(r64), DIMENSION(:),ALLOCATABLE:: ZoneADEffHeating       ! Zone air distribution effectiveness in heating mode
                                                               ! for each zone
  INTEGER, DIMENSION(:),ALLOCATABLE:: ZoneADEffSchPtr          ! Pointer to the zone air distribution effectiveness schedule
                                                               ! for each zone
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE:: ZoneADEffSchName  ! Zone air distribution effectiveness schedule name
                                                               ! for each zone

  INTEGER,DIMENSION(:),ALLOCATABLE :: ZoneDesignSpecADObjIndex ! index of the design specification zone air
                                                                   !  distribution object for each zone in the zone list
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE:: ZoneDesignSpecADObjName ! name of the design specification zone air
                                                               ! distribution object for each zone in the zone list
  REAL(r64), DIMENSION(:),ALLOCATABLE:: ZoneSecondaryRecirculation ! zone air secondary recirculation ratio
END TYPE VentilationMechanicalProps

TYPE OAMixerProps ! Derived type for Outside Air Mixing Component
  CHARACTER(len=MaxNameLength) :: Name = ' '
  INTEGER      :: MixerIndex           = 0  ! Set on first call...
  INTEGER      :: MixNode              = 0  ! Outlet node - mixed air
  INTEGER      :: InletNode            = 0  ! Inlet node for outside air stream (Nov. 2004 BTG was OANode )
  INTEGER      :: RelNode              = 0  ! Outlet node - relief air
  INTEGER      :: RetNode              = 0  ! Inlet node - return air
  REAL(r64)    :: MixTemp              = 0.0d0
  REAL(r64)    :: MixHumRat            = 0.0d0
  REAL(r64)    :: MixEnthalpy          = 0.0d0
  REAL(r64)    :: MixPressure          = 0.0d0
  REAL(r64)    :: MixMassFlowRate      = 0.0d0
  REAL(r64)    :: OATemp               = 0.0d0
  REAL(r64)    :: OAHumRat             = 0.0d0
  REAL(r64)    :: OAEnthalpy           = 0.0d0
  REAL(r64)    :: OAPressure           = 0.0d0
  REAL(r64)    :: OAMassFlowRate       = 0.0d0
  REAL(r64)    :: RelTemp              = 0.0d0
  REAL(r64)    :: RelHumRat            = 0.0d0
  REAL(r64)    :: RelEnthalpy          = 0.0d0
  REAL(r64)    :: RelPressure          = 0.0d0
  REAL(r64)    :: RelMassFlowRate      = 0.0d0
  REAL(r64)    :: RetTemp              = 0.0d0
  REAL(r64)    :: RetHumRat            = 0.0d0
  REAL(r64)    :: RetEnthalpy          = 0.0d0
  REAL(r64)    :: RetPressure          = 0.0d0
  REAL(r64)    :: RetMassFlowRate      = 0.0d0
END TYPE OAMixerProps

!MODULE VARIABLE DECLARATIONS:
TYPE (ControllerListProps), ALLOCATABLE, DIMENSION(:) :: ControllerLists
TYPE (OAControllerProps), ALLOCATABLE, DIMENSION(:) :: OAController
TYPE (OAMixerProps), ALLOCATABLE, DIMENSION(:)      :: OAMixer
TYPE (VentilationMechanicalProps), ALLOCATABLE, DIMENSION(:)  :: VentilationMechanical
INTEGER :: NumControllerLists = 0   ! Number of Controller Lists
INTEGER :: NumOAControllers=0       ! Number of OA Controllers (includes ERV controllers)
INTEGER :: NumERVControllers=0      ! Number of ERV Controllers
INTEGER :: NumOAMixers=0            ! Number of Outdoor Air Mixers
INTEGER :: NumVentMechControllers=0 ! Number of Controller:MechanicalVentilation objects in input deck

LOGICAL, ALLOCATABLE, DIMENSION(:) :: MyOneTimeErrorFlag
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MyOneTimeCheckUnitarySysFlag
LOGICAL :: GetOASysInputFlag        = .True.  ! Flag set to make sure you get input once
LOGICAL :: GetOAMixerInputFlag      = .True.  ! Flag set to make sure you get input once
LOGICAL :: GetOAControllerInputFlag = .True.  ! Flag set to make sure you get input once

!SUBROUTINE SPECIFICATIONS FOR MODULE MixedAir
          ! Driver/Manager Routines
PUBLIC  ManageOutsideAirSystem

          ! Get Input routines for module
PUBLIC  GetOutsideAirSysInputs
PUBLIC  GetOAControllerInputs
PUBLIC  GetOAMixerInputs

          ! Initialization routines for module
PRIVATE InitOutsideAirSys
PRIVATE InitOAController
PRIVATE InitOAMixer

          ! Algorithms/Calculation routines for the module
PRIVATE SimOutsideAirSys
PRIVATE SimOAComponent
PUBLIC  SimOAMixer
PUBLIC  SimOAController
PRIVATE CalcOAController
PRIVATE CalcOAMixer

     ! Sizing routine for the module
PRIVATE SizeOAController

     ! Update routines to check convergence and update nodes
PRIVATE UpdateOAController
PRIVATE UpdateOAMixer

     ! Utility routines for the module
PUBLIC  GetOAMixerNodeNumbers
PUBLIC  GetNumOAMixers
PUBLIC  GetNumOAControllers
PUBLIC  GetOAMixerReliefNodeNumber
PUBLIC  GetOASystemNumber
PUBLIC  FindOAMixerMatchForOASystem
PUBLIC  GetOAMixerIndex
PUBLIC  GetOAMixerInletNodeNumber
PUBLIC  GetOAMixerReturnNodeNumber
PUBLIC  GetOAMixerMixedNodeNumber
PUBLIC  SetOAControllerData
PUBLIC  CheckOAControllerName
PUBLIC  GetOASysControllerListIndex
PUBLIC  GetOASysNumSimpControllers
PUBLIC  GetOASysNumCoolingCoils
PUBLIC  GetOASysNumHeatingCoils
PUBLIC  CheckForControllerWaterCoil
PUBLIC  CheckControllerLists
PUBLIC  GetNumOASystems
PUBLIC  GetOACompListNumber
PUBLIC  GetOACompName
PUBLIC  GetOACompType
PUBLIC  GetOACompTypeNum

CONTAINS

SUBROUTINE ManageOutsideAirSystem(OASysName,FirstHVACIteration,AirLoopNum,OASysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Manage the outside air system

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
      USE InputProcessor, ONLY: FindItemInList

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    CHARACTER(len=*), INTENT(IN) :: OASysName
    LOGICAL, INTENT(IN)      :: FirstHVACIteration
    INTEGER, INTENT(IN)      :: AirLoopNum
    INTEGER, INTENT(INOUT)   :: OASysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

IF (GetOASysInputFlag) THEN
  CALL GetOutsideAirSysInputs
  GetOASysInputFlag=.false.
ENDIF

IF (OASysNum == 0) THEN
  OASysNum=FindItemInList(OASysName,OutsideAirSys%Name,NumOASystems)
  IF (OASysNum == 0) THEN
    CALL ShowFatalError('ManageOutsideAirSystem: AirLoopHVAC:OutdoorAirSystem not found='//TRIM(OASysName))
  ENDIF
ENDIF

CALL InitOutsideAirSys(OASysNum,FirstHVACIteration)

CALL SimOutsideAirSys(OASysNum,FirstHVACIteration,AirLoopNum)

RETURN

END SUBROUTINE ManageOutsideAirSystem

SUBROUTINE SimOutsideAirSys(OASysNum,FirstHVACIteration,AirLoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Simulate the controllers and components in the outside air system.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: FindItemInList, SameString

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OASysNum
    LOGICAL, INTENT(IN) :: FirstHVACIteration
    INTEGER, INTENT(IN) :: AirLoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: CompNum
!INTEGER :: CtrlNum
INTEGER :: OAMixerNum
INTEGER :: OAControllerNum
CHARACTER(len=MaxNameLength) :: CompType
CHARACTER(len=MaxNameLength) :: CompName
CHARACTER(len=MaxNameLength) :: CtrlName
LOGICAL :: FatalErrorFlag
LOGICAL :: Sim
LOGICAL :: OAHeatCoil
LOGICAL :: OACoolCoil
LOGICAL :: OAHX
LOGICAL :: ReSim

! SimOutsideAirSys can handle only 1 controller right now.  This must be
! an Outside Air Controller.  This is because of the lack of iteration
! and convergence control in the following code.
!  DO CtrlNum=1,OutsideAirSys(OASysNum)%NumControllers
!    CtrlName = OutsideAirSys(OASysNum)%ControllerName(CtrlNum)
!    CALL SimOAController(CtrlName,FirstHVACIteration)
!
!  END DO
FatalErrorFlag = .FALSE.
CtrlName = OutsideAirSys(OASysNum)%ControllerName(1)
CurOASysNum = OASysNum
Sim = .TRUE.
ReSim = .FALSE.
CALL SimOAController(CtrlName,OutsideAirSys(OASysNum)%ControllerIndex(1),FirstHVACIteration,AirLoopNum)

DO CompNum=1,OutsideAirSys(OASysNum)%NumComponents
  CompType = OutsideAirSys(OASysNum)%ComponentType(CompNum)
  CompName = OutsideAirSys(OASysNum)%ComponentName(CompNum)
  CALL SimOAComponent(CompType,CompName,OutsideAirSys(OASysNum)%ComponentType_Num(CompNum),  &
                  FirstHVACIteration,OutsideAirSys(OASysNum)%ComponentIndex(CompNum),AirLoopNum,Sim,OASysNum, &
                  OAHeatCoil,OACoolCoil,OAHX)
  IF (OAHX) ReSim = .TRUE.
END DO
! if there were heat exchangers and/or desiccant wheel in the OA path, need to simulate again
! in reverse order to propagate the air flow and conditions out the relief air path to the relief air
! exit node
IF (ReSim) THEN
  DO CompNum=OutsideAirSys(OASysNum)%NumComponents-1,1,-1
    CompType = OutsideAirSys(OASysNum)%ComponentType(CompNum)
    CompName = OutsideAirSys(OASysNum)%ComponentName(CompNum)
    CALL SimOAComponent(CompType,CompName,OutsideAirSys(OASysNum)%ComponentType_Num(CompNum),  &
                    FirstHVACIteration,OutsideAirSys(OASysNum)%ComponentIndex(CompNum),AirLoopNum,Sim,OASysNum, &
                    OAHeatCoil,OACoolCoil,OAHX)
  END DO
END IF

IF (MyOneTimeErrorFlag(OASysNum)) THEN
  IF (OutsideAirSys(OASysNum)%NumControllers - OutsideAirSys(OASysNum)%NumSimpleControllers > 1) THEN
    CALL ShowWarningError('AirLoopHVAC:OutdoorAirSystem ' // TRIM(OutsideAirSys(OASysNum)%Name) // &
                           ' has more than 1 outside air controller; only the 1st will be used')
  END IF
  DO CompNum=1,OutsideAirSys(OASysNum)%NumComponents
    CompType = OutsideAirSys(OASysNum)%ComponentType(CompNum)
    CompName = OutsideAirSys(OASysNum)%ComponentName(CompNum)
    IF (SameString(CompType,'OutdoorAir:Mixer')) THEN
      OAMixerNum=FindItemInList(CompName,OAMixer%Name,NumOAMixers)
      OAControllerNum=FindItemInList(CtrlName,OAController%Name,NumOAControllers)
      IF (OAController(OAControllerNum)%MixNode .NE. OAMixer(OAMixerNum)%MixNode) THEN
        CALL ShowSevereError('The mixed air node of Controller:OutdoorAir="'// TRIM(OAController(OAControllerNum)%Name)//'"')
        CALL ShowContinueError('should be the same node as the mixed air node of OutdoorAir:Mixer="' // &
                               TRIM(OAMixer(OAMixerNum)%Name)//'".')
        CALL ShowContinueError('Controller:OutdoorAir mixed air node="'//trim(NodeID(OAController(OAControllerNum)%MixNode))//'".')
        CALL ShowContinueError('OutdoorAir:Mixer mixed air node="'//trim(NodeID(OAMixer(OAMixerNum)%MixNode))//'".')
        FatalErrorFlag = .TRUE.
      END IF
      IF (OAController(OAControllerNum)%RelNode .NE. OAMixer(OAMixerNum)%RelNode) THEN
        CALL ShowSevereError('The relief air node of Controller:OutdoorAir="' // TRIM(OAController(OAControllerNum)%Name)//'"')
        CALL ShowContinueError('should be the same node as the relief air node of OutdoorAir:Mixer="' // &
                               TRIM(OAMixer(OAMixerNum)%Name)//'".')
        CALL ShowContinueError('Controller:OutdoorAir relief air node="'//trim(NodeID(OAController(OAControllerNum)%RelNode))//'".')
        CALL ShowContinueError('OutdoorAir:Mixer relief air node="'//trim(NodeID(OAMixer(OAMixerNum)%RelNode))//'".')
        FatalErrorFlag = .TRUE.
      END IF
      IF (OAController(OAControllerNum)%RetNode .NE. OAMixer(OAMixerNum)%RetNode) THEN
        CALL ShowSevereError('The return air node of Controller:OutdoorAir="' // TRIM(OAController(OAControllerNum)%Name)//'"')
        CALL ShowContinueError('should be the same node as the return air node of OutdoorAir:Mixer="' // &
                               TRIM(OAMixer(OAMixerNum)%Name)//'".')
        CALL ShowContinueError('Controller:OutdoorAir return air node="'//trim(NodeID(OAController(OAControllerNum)%RetNode))//'".')
        CALL ShowContinueError('OutdoorAir:Mixer return air node="'//trim(NodeID(OAMixer(OAMixerNum)%RetNode))//'".')
        FatalErrorFlag = .TRUE.
      END IF
    END IF
  END DO
  MyOneTimeErrorFlag(OASysNum) = .FALSE.
  IF (FatalErrorFlag) CALL ShowFatalError('Previous severe error(s) cause program termination')
END IF

CurOASysNum = 0
AirLoopControlInfo(AirLoopNum)%OASysComponentsSimulated = .TRUE.

RETURN
END SUBROUTINE SimOutsideAirSys

SUBROUTINE SimOAComponent(CompType,CompName,CompTypeNum,FirstHVACIteration,CompIndex,AirLoopNum,Sim,OASysNum, &
                          OAHeatingCoil,OACoolingCoil,OAHX)

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
          !       DATE WRITTEN:  Oct 1997
          !           MODIFIED:  Dec 1997 Fred Buhl, D Shirey Feb/Sept 2003
          !                      Nov 2004 M. J. Witte, GARD Analytics, Inc.
          !                        Add DXSystem:AirLoop as valid OA system equipment
          !                        Work supported by ASHRAE research project 1254-RP
          !      RE-ENGINEERED:  This is new code, not reengineered

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calls the individual air loop component simulation routines

          ! METHODOLOGY EMPLOYED: None

          ! REFERENCES: None

          ! USE Statements
  Use Fans, Only:SimulateFanComponents
  USE DataAirLoop, ONLY: AirLoopInputsFilled
  Use WaterCoils, Only:SimulateWaterCoilComponents
  Use HeatingCoils, Only:SimulateHeatingCoilComponents
  Use HeatRecovery, Only: SimHeatRecovery
  Use DesiccantDehumidifiers,  Only:SimDesiccantDehumidifier
  Use HVACHXAssistedCoolingCoil, Only:SimHXAssistedCoolingCoil
  Use HVACDXSystem, Only:SimDXCoolingSystem
  Use HVACDXHeatPumpSystem, ONLY: SimDXHeatPumpSystem
  Use SteamCoils, Only:SimulateSteamCoilComponents
  Use TranspiredCollector, Only:SimTranspiredCollector
  Use EvaporativeCoolers, Only:SimEvapCooler
  USE PhotovoltaicThermalCollectors, ONLY:SimPVTcollectors, CalledFromOutsideAirSystem
  USE UserDefinedComponents, ONLY: SimCoilUserDefined
  USE HVACUnitarySystem,     ONLY: SimUnitarySystem, GetUnitarySystemOAHeatCoolCoil, CheckUnitarySysCoilInOASysExists

IMPLICIT NONE

          ! SUBROUTINE ARGUMENTS:

LOGICAL, INTENT (IN) :: FirstHVACIteration
CHARACTER(len=*), INTENT (IN) :: CompType ! the component type
CHARACTER(len=*), INTENT (IN) :: CompName ! the component Name
INTEGER, INTENT(IN)           :: CompTypeNum ! Component Type -- Integerized for this module
INTEGER, INTENT(INOUT)        :: CompIndex
INTEGER, INTENT(IN)           :: AirLoopNum ! air loop index for economizer lockout coordination
LOGICAL, INTENT(IN)            :: Sim        ! if TRUE, simulate component; if FALSE, just set the coil exisitence flags
INTEGER, INTENT(IN)            :: OASysNum   ! index to outside air system
LOGICAL, INTENT(OUT), OPTIONAL :: OAHeatingCoil  ! TRUE indicates a heating coil has been found
LOGICAL, INTENT(OUT), OPTIONAL :: OACoolingCoil  ! TRUE indicates a cooling coil has been found
LOGICAL, INTENT(OUT), OPTIONAL :: OAHX           ! TRUE indicates a heat exchanger has been found

           ! SUBROUTINE PARAMETER DEFINITIONS: None

           ! INTERFACE BLOCK DEFINITIONS: None

           ! DERIVED TYPE DEFINITIONS: None

           ! SUBROUTINE LOCAL VARIABLE DEFINITIONS

OAHeatingCoil = .FALSE.
OACoolingCoil = .FALSE.
OAHX = .FALSE.

SELECT CASE(CompTypeNum)

  CASE (OAMixer_Num)  ! 'OutdoorAir:Mixer'
    IF (Sim) THEN
      CALL SimOAMixer(CompName,FirstHVACIteration,CompIndex)
    END IF

! Fan Types
  CASE(Fan_Simple_CV)  ! 'Fan:ConstantVolume'
    IF (Sim) Then
      CALL SimulateFanComponents(CompName,FirstHVACIteration,CompIndex)
    END IF
  CASE(Fan_Simple_VAV) ! 'Fan:VariableVolume'
    IF (Sim) Then
      CALL SimulateFanComponents(CompName,FirstHVACIteration,CompIndex)
    END IF
  !cpw22Aug2010 Add Fan:ComponentModel (new num=18)
  CASE(Fan_ComponentModel) ! 'Fan:ComponentModel'
    IF (Sim) Then
      CALL SimulateFanComponents(CompName,FirstHVACIteration,CompIndex)
    END IF

! Coil Types
  CASE(WaterCoil_Cooling)    ! 'Coil:Cooling:Water'
    IF (Sim) Then
      CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompIndex)
    END IF
    OACoolingCoil = .TRUE.
  CASE(WaterCoil_SimpleHeat) ! 'Coil:Heating:Water')
    IF (Sim) Then
      CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompIndex)
    END IF
    OAHeatingCoil = .TRUE.
  CASE(SteamCoil_AirHeat)    ! 'Coil:Heating:Steam'
    IF (Sim) Then
      CALL SimulateSteamCoilComponents(CompName,FirstHVACIteration,0.0d0,CompIndex)
    END IF
    OAHeatingCoil = .TRUE.
  CASE(WaterCoil_DetailedCool) ! 'Coil:Cooling:Water:DetailedGeometry'
    IF (Sim) Then
      CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompIndex)
    END IF
    OACoolingCoil = .TRUE.
  CASE(Coil_ElectricHeat)    ! 'Coil:Heating:Electric'
    IF (Sim) Then
!     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
      CALL SimulateHeatingCoilComponents(CompName=CompName,FirstHVACIteration=FirstHVACIteration,CompIndex=CompIndex)
    END IF
    OAHeatingCoil = .TRUE.
  CASE(Coil_GasHeat)  ! 'Coil:Heating:Gas'
    IF (Sim) Then
!     stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
      CALL SimulateHeatingCoilComponents(CompName=CompName,FirstHVACIteration=FirstHVACIteration,CompIndex=CompIndex)
    END IF
    OAHeatingCoil = .TRUE.
  CASE(WaterCoil_CoolingHXAsst)  ! 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
    IF (Sim) Then
      CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,0.0d0,CompIndex,ContFanCycCoil)
    END IF
    OACoolingCoil = .TRUE.
  CASE(DXSystem)  ! CoilSystem:Cooling:DX  old 'AirLoopHVAC:UnitaryCoolOnly'
    IF (Sim) Then
      CALL SimDXCoolingSystem(CompName,FirstHVACIteration,AirLoopNum, CompIndex)
    END IF
    OACoolingCoil = .TRUE.
  CASE(UnitarySystem)  ! AirLoopHVAC:UnitarySystem
    IF (Sim) Then
      CALL SimUnitarySystem(CompName,FirstHVACIteration,AirLoopNum, CompIndex)
    END IF
    IF(AirLoopInputsFilled)CALL GetUnitarySystemOAHeatCoolCoil(CompName, OACoolingCoil, OAHeatingCoil)
    IF(MyOneTimeCheckUnitarySysFlag(OASysNum))THEN
      IF(AirLoopInputsFilled)THEN
        CALL CheckUnitarySysCoilInOASysExists(CompName)
        MyOneTimeCheckUnitarySysFlag(OASysNum) = .FALSE.
      END IF
    END IF
  CASE (DXHeatPumpSystem)
    IF (sim) Then
      CALL SimDXHeatPumpSystem(CompName,FirstHVACIteration,AirLoopNum, CompIndex)
    ENDIF
    OAHeatingCoil = .TRUE.
  CASE (Coil_UserDefined)
    IF (sim) THEN
      CALL SimCoilUserDefined(CompName, CompIndex, AirLoopNum, OAHeatingCoil, OACoolingCoil  )
    ENDIF
! Heat recovery
  CASE(HeatXchngr)  ! 'HeatExchanger:AirToAir:FlatPlate', 'HeatExchanger:AirToAir:SensibleAndLatent',
                    ! 'HeatExchanger:Desiccant:BalancedFlow'
    IF (Sim) Then
      CALL SimHeatRecovery(CompName,FirstHVACIteration,CompIndex, ContFanCycCoil, &
           EconomizerFlag=AirLoopControlInfo(AirLoopNum)%HeatRecoveryBypass, &
           HighHumCtrlFlag=AirLoopControlInfo(AirLoopNum)%HighHumCtrlActive)
    END IF
    OAHX = .TRUE.

! Desiccant Dehumidifier
  CASE(Desiccant)  ! 'Dehumidifier:Desiccant:NoFans'
                   ! 'Dehumidifier:Desiccant:System'
    IF (Sim) Then
      CALL SimDesiccantDehumidifier(CompName,FirstHVACIteration,CompIndex)
    END IF
    OAHX = .TRUE.

! Unglazed Transpired Solar Collector
  CASE(Unglazed_SolarCollector)  ! 'SolarCollector:UnglazedTranspired'
    IF (Sim) Then
      CALL SimTranspiredCollector(CompName, CompIndex )
    END IF

! Air-based Photovoltaic-thermal flat plate collector
  CASE(PVT_AirBased)  ! 'SolarCollector:FlatPlate:PhotovoltaicThermal'
    IF (Sim) Then
      CALL SimPVTcollectors(CompIndex , FirstHVACIteration, CalledFromOutsideAirSystem, PVTName=CompName )
    END IF

! Evaporative Cooler Types
  CASE(EvapCooler) ! 'EvaporativeCooler:Direct:CelDekPad','EvaporativeCooler:Indirect:CelDekPad'
                   ! 'EvaporativeCooler:Indirect:WetCoil','EvaporativeCooler:Indirect:ResearchSpecial'
    IF (Sim) Then
      CALL SimEvapCooler(CompName,CompIndex)
    END IF

  CASE DEFAULT
    CALL ShowFatalError('Invalid Outside Air Component='//TRIM(CompType))

END SELECT

RETURN

END SUBROUTINE SimOAComponent

SUBROUTINE SimOAMixer(CompName,FirstHVACIteration,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Simulate an Outside Air Mixer component

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
      USE InputProcessor, ONLY: FindItemInList

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    CHARACTER(len=*), INTENT(IN) :: CompName
    LOGICAL, INTENT(IN)      :: FirstHVACIteration
    INTEGER, INTENT(INOUT)   :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER      :: OAMixerNum

IF (GetOAMixerInputFlag) THEN
  CALL GetOAMixerInputs
  GetOAMixerInputFlag=.false.
ENDIF

IF (CompIndex == 0) THEN
  OAMixerNum=FindItemInList(CompName,OAMixer%Name,NumOAMixers)
  CompIndex=OAMixerNum
  IF (OAMixerNum == 0) THEN
    CALL ShowFatalError('SimOAMixer: OutdoorAir:Mixer not found='//TRIM(CompName))
  ENDIF
ELSE
  OAMixerNum=CompIndex
ENDIF

CALL InitOAMixer(OAMixerNum,FirstHVACIteration)

CALL CalcOAMixer(OAMixerNum)

CALL UpdateOAMixer(OAMixerNum)

CALL ReportOAMixer(OAMixerNum)

RETURN

END SUBROUTINE SimOAMixer

SUBROUTINE SimOAController(CtrlName,CtrlIndex,FirstHVACIteration,AirLoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Simulate an Outside Air Controller component

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
      USE InputProcessor, ONLY: FindItemInList

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    CHARACTER(len=*), INTENT(IN) :: CtrlName
    INTEGER, INTENT(INOUT)       :: CtrlIndex
    LOGICAL, INTENT(IN)      :: FirstHVACIteration
    INTEGER, INTENT(IN)      :: AirLoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: OAControllerNum

IF (GetOAControllerInputFlag) THEN  ! Gets input for object  first time Sim routine is called
  CALL GetOAControllerInputs
  GetOAControllerInputFlag=.false.
END IF

IF (CtrlIndex == 0) THEN
  IF(NumOAControllers .GT. 0)THEN
    OAControllerNum=FindItemInList(CtrlName,OAController%Name,NumOAControllers)
  ELSE
    OAControllerNum=0
  END IF
  CtrlIndex=OAControllerNum
  IF (OAControllerNum == 0) THEN
    CALL ShowFatalError('SimOAController: Outside Air Controller not found='//TRIM(CtrlName))
  ENDIF
ELSE
  OAControllerNum = CtrlIndex
ENDIF

CALL InitOAController(OAControllerNum,FirstHVACIteration,AirLoopNum)

CALL CalcOAController(OAControllerNum,AirLoopNum)

CALL UpdateOAController(OAControllerNum)

CALL ReportOAController(OAControllerNum)

RETURN

END SUBROUTINE SimOAController

! Get Input Section of the Module
!******************************************************************************

SUBROUTINE GetOutsideAirSysInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Input the Outside Air System data and store it in the OutsideAirSys array.

          ! METHODOLOGY EMPLOYED:
          ! Use the Get routines from the InputProcessor module.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor
    USE BranchNodeConnections, ONLY: TestCompSet, SetUpCompSets
    USE HVACDXSystem,          ONLY: CheckDXCoolingCoilInOASysExists

    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetOutsideAirSysInputs: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: NumNums   ! Number of real numbers returned by GetObjectItem
INTEGER :: NumAlphas ! Number of alphanumerics returned by GetObjectItem
INTEGER :: IOSTAT
REAL(r64),  ALLOCATABLE, DIMENSION(:) :: NumArray
CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: AlphArray
INTEGER :: OASysNum
INTEGER :: CompNum
INTEGER :: Item
!unused0909INTEGER :: NumComponents
INTEGER :: AlphaNum
CHARACTER(len=MaxNameLength) :: ComponentListName
CHARACTER(len=MaxNameLength) :: ControllerListName
CHARACTER(len=MaxNameLength) :: AvailManagerListName
INTEGER :: NumInList
INTEGER :: InListNum
INTEGER :: ListNum
INTEGER :: NumSimpControllers    ! number of Controller:Simple objects in an OA System
LOGICAL :: ErrorsFound=.false.
LOGICAL :: IsNotOK               ! Flag to verify name
LOGICAL :: IsBlank               ! Flag for blank name
CHARACTER(len=MaxNameLength) :: CurrentModuleObject   ! Object type for getting and messages
CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
LOGICAL,  ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks     ! Logical array, alpha field input BLANK = .true.
LOGICAL,  ALLOCATABLE, DIMENSION(:) :: lNumericBlanks   ! Logical array, numeric field input BLANK = .true.
INTEGER :: MaxNums=0              ! Maximum number of numeric input fields
INTEGER :: MaxAlphas=0            ! Maximum number of alpha input fields
INTEGER :: TotalArgs=0            ! Total number of alpha and numeric arguments (max) for a
                                  !  certain object in the input file


IF (.not. GetOASysInputFlag) RETURN

CALL GetObjectDefMaxArgs(CurrentModuleObjects(CMO_OASystem),TotalArgs,NumAlphas,NumNums)
MaxNums=MAX(MaxNums,NumNums)
MaxAlphas=MAX(MaxAlphas,NumAlphas)
CALL GetObjectDefMaxArgs(CurrentModuleObjects(CMO_AirLoopEqList),TotalArgs,NumAlphas,NumNums)
MaxNums=MAX(MaxNums,NumNums)
MaxAlphas=MAX(MaxAlphas,NumAlphas)
CALL GetObjectDefMaxArgs(CurrentModuleObjects(CMO_ControllerList),TotalArgs,NumAlphas,NumNums)
MaxNums=MAX(MaxNums,NumNums)
MaxAlphas=MAX(MaxAlphas,NumAlphas)


ALLOCATE(AlphArray(MaxAlphas))
AlphArray=' '
ALLOCATE(cAlphaFields(MaxAlphas))
cAlphaFields=' '
ALLOCATE(NumArray(MaxNums))
NumArray=0.0d0
ALLOCATE(cNumericFields(MaxNums))
cNumericFields=' '
ALLOCATE(lAlphaBlanks(MaxAlphas))
lAlphaBlanks=.true.
ALLOCATE(lNumericBlanks(MaxNums))
lNumericBlanks=.true.

CurrentModuleObject = CurrentModuleObjects(CMO_ControllerList)
NumControllerLists = GetNumObjectsFound(CurrentModuleObject)

ALLOCATE(ControllerLists(NumControllerLists))

DO Item=1,NumControllerLists

    CALL GetObjectItem(CurrentModuleObject,Item,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),ControllerLists%Name,Item-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    ControllerLists(Item)%Name = AlphArray(1)
    IsNotOK=.false.
    IsBlank=.false.
    ControllerLists(Item)%NumControllers=(NumAlphas-1)/2
    ALLOCATE(ControllerLists(Item)%ControllerType(ControllerLists(Item)%NumControllers))
    ControllerLists(Item)%ControllerType=' '
    ALLOCATE(ControllerLists(Item)%ControllerName(ControllerLists(Item)%NumControllers))
    ControllerLists(Item)%ControllerName=' '
    AlphaNum=2
    DO CompNum=1,ControllerLists(Item)%NumControllers
      IF (SameString(AlphArray(AlphaNum),'Controller:WaterCoil') .or. &
          SameString(AlphArray(AlphaNum),'Controller:OutdoorAir') ) THEN
        ControllerLists(Item)%ControllerType(CompNum)=AlphArray(AlphaNum)
        ControllerLists(Item)%ControllerName(CompNum)=AlphArray(AlphaNum+1)
      ELSE
        CALL ShowSevereError('For '//trim(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '// &
          trim(cAlphaFields(AlphaNum)))
        CALL ShowContinueError('...entered="'//trim(AlphArray(AlphaNum))//'", should be Controller:WaterCoil '//  &
          ' or Controller:OutdoorAir.')
        ErrorsFound=.true.
      ENDIF
      AlphaNum=AlphaNum+2
    ENDDO

ENDDO

CurrentModuleObject = CurrentModuleObjects(CMO_OASystem)

NumOASystems = GetNumObjectsFound(CurrentModuleObject)

  ALLOCATE(OutsideAirSys(NumOASystems))
  ALLOCATE(OASysEqSizing(NumOASystems))
  ALLOCATE(MyOneTimeErrorFlag(NumOASystems))
  ALLOCATE(MyOneTimeCheckUnitarySysFlag(NumOASystems))
  MyOneTimeErrorFlag = .TRUE.
  MyOneTimeCheckUnitarySysFlag = .TRUE.

  DO OASysNum=1,NumOASystems

    CALL GetObjectItem(CurrentModuleObject,OASysNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),OutsideAirSys%Name,OASysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    OutsideAirSys(OASysNum)%Name = AlphArray(1)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(2),OutsideAirSys%ControllerListName,OASysNum-1,IsNotOK,IsBlank,  &
                    TRIM(CurrentModuleObject)//' '//TRIM(cAlphaFields(2))//' Name')
    IF (IsNotOK .and. AlphArray(1) /= 'xxxxx') THEN
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = "'//trim(AlphArray(1))//'".')
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(2)='xxxxx'
    ENDIF
    ControllerListName = AlphArray(2)
    OutsideAirSys(OASysNum)%ControllerListName = AlphArray(2)
    ComponentListName = AlphArray(3)
    OutsideAirSys(OASysNum)%ComponentListName = AlphArray(3)
    AvailManagerListName = AlphArray(4)

    CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),'UNDEFINED','UNDEFINED','Air Nodes')

    IF (.NOT. lAlphaBlanks(3)) THEN
      ListNum = GetObjectItemNum(CurrentModuleObjects(CMO_AirLoopEqList),ComponentListName)
      IF (ListNum > 0) THEN
        CALL GetObjectItem(CurrentModuleObjects(CMO_AirLoopEqList),ListNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT)
        NumInList = (NumAlphas-1)/2
        OutsideAirSys(OASysNum)%NumComponents = NumInList
        ALLOCATE(OutsideAirSys(OASysNum)%ComponentName(NumInList))
        ALLOCATE(OutsideAirSys(OASysNum)%ComponentType(NumInList))
        ALLOCATE(OutsideAirSys(OASysNum)%ComponentType_Num(NumInList))
        OutsideAirSys(OASysNum)%ComponentType_Num=0
        ALLOCATE(OutsideAirSys(OASysNum)%ComponentIndex(NumInList))
        OutsideAirSys(OASysNum)%ComponentIndex=0
        DO InListNum=1,NumInList
          OutsideAirSys(OASysNum)%ComponentName(InListNum) = AlphArray(InListNum*2+1)
          OutsideAirSys(OASysNum)%ComponentType(InListNum) = AlphArray(InListNum*2)

          ! Add equipment to component sets array
          CALL SetUpCompSets(TRIM(CurrentModuleObject),OutsideAirSys(OASysNum)%Name, &
                             OutsideAirSys(OASysNum)%ComponentType(InListNum), &
                             OutsideAirSys(OASysNum)%ComponentName(InListNum), &
                             'UNDEFINED','UNDEFINED')
        END DO
      ELSE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'" invalid '//  &
           TRIM(cAlphaFields(3))//'="'//TRIM(AlphArray(3))//'" not found.')
        ErrorsFound=.true.
      END IF
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'" invalid '//  &
           TRIM(cAlphaFields(3))//' is blank and must be entered.')
      ErrorsFound=.true.
    ENDIF

    ListNum = 0
    NumSimpControllers = 0
    IF (.NOT. lAlphaBlanks(2)) THEN
      ListNum = GetObjectItemNum(CurrentModuleObjects(CMO_ControllerList),ControllerListName)
      IF (ListNum > 0) THEN
        CALL GetObjectItem(CurrentModuleObjects(CMO_ControllerList),ListNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT)
        NumInList = (NumAlphas-1)/2
        OutsideAirSys(OASysNum)%NumControllers = NumInList
        ALLOCATE(OutsideAirSys(OASysNum)%ControllerName(NumInList))
        ALLOCATE(OutsideAirSys(OASysNum)%ControllerType(NumInList))
        ALLOCATE(OutsideAirSys(OASysNum)%ControllerIndex(NumInList))
        OutsideAirSys(OASysNum)%ControllerIndex=0
        DO InListNum=1,NumInList
          OutsideAirSys(OASysNum)%ControllerName(InListNum) = AlphArray(InListNum*2+1)
          OutsideAirSys(OASysNum)%ControllerType(InListNum) = AlphArray(InListNum*2)
          IF (.not. SameString(OutsideAirSys(OASysNum)%ControllerType(InListNum),CurrentModuleObjects(CMO_OAController))) THEN
            NumSimpControllers = NumSimpControllers +1
          ENDIF
        END DO
      ELSE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'" invalid '//  &
           TRIM(cAlphaFields(2))//'="'//TRIM(AlphArray(2))//'" not found.')
        ErrorsFound=.true.
      END IF
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'" invalid '//  &
           TRIM(cAlphaFields(2))//' is blank and must be entered.')
      ErrorsFound=.true.
    ENDIF
    OutsideAirSys(OASysNum)%ControllerListNum = ListNum
    OutsideAirSys(OASysNum)%NumSimpleControllers = NumSimpControllers

    IF (.NOT. lAlphaBlanks(4)) THEN
      ListNum=GetObjectItemNum(CurrentModuleObjects(CMO_SysAvailMgrList),AvailManagerListName)
      IF (ListNum <= 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'" invalid '//  &
           TRIM(cAlphaFields(4))//'="'//TRIM(AlphArray(4))//'" not found.')
        ErrorsFound=.true.
      ENDIF
    ENDIF
  END DO

  DO OASysNum=1,NumOASystems
    DO CompNum=1,OutsideAirSys(OASysNum)%NumComponents

      SELECT CASE(MakeUPPERCase(OutsideAirSys(OASysNum)%ComponentType(CompNum)))

        CASE ('OUTDOORAIR:MIXER')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= OAMixer_Num

  ! Fan Types
        CASE('FAN:CONSTANTVOLUME')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= Fan_Simple_CV
        CASE('FAN:VARIABLEVOLUME')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= Fan_Simple_VAV
        !cpw22Aug2010 Add Fan:ComponentModel (new)
        CASE('FAN:COMPONENTMODEL')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= Fan_ComponentModel

  ! Coil Types
        CASE('COIL:COOLING:WATER')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= WaterCoil_Cooling
        CASE('COIL:HEATING:WATER')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= WaterCoil_SimpleHeat
        CASE('COIL:HEATING:STEAM')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= SteamCoil_AirHeat
        CASE('COIL:COOLING:WATER:DETAILEDGEOMETRY')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= WaterCoil_DetailedCool
        CASE('COIL:HEATING:ELECTRIC')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= Coil_ElectricHeat
        CASE('COIL:HEATING:GAS')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= Coil_GasHeat
        CASE('COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= WaterCoil_CoolingHXAsst
        CASE('COILSYSTEM:COOLING:DX')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= DXSystem
          ! set the data for 100% DOAS DX cooling coil
          CALL CheckDXCoolingCoilInOASysExists(OutsideAirSys(OASysNum)%ComponentName(CompNum))
        CASE('COILSYSTEM:HEATING:DX')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= DXHeatPumpSystem
        CASE('AIRLOOPHVAC:UNITARYSYSTEM')
            OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= UnitarySystem
        CASE('COIL:USERDEFINED')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= Coil_UserDefined
  ! Heat recovery
        CASE('HEATEXCHANGER:AIRTOAIR:FLATPLATE')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= HeatXchngr
        CASE('HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= HeatXchngr
        CASE('HEATEXCHANGER:DESICCANT:BALANCEDFLOW')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= HeatXchngr

      ! Desiccant Dehumidifier
        CASE('DEHUMIDIFIER:DESICCANT:NOFANS')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= Desiccant
        CASE('DEHUMIDIFIER:DESICCANT:SYSTEM')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= Desiccant

  ! Unglazed Transpired Solar Collector
        CASE('SOLARCOLLECTOR:UNGLAZEDTRANSPIRED')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= Unglazed_SolarCollector

  ! PVT air heater
        CASE('SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL')
           OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= PVT_AirBased
  ! Evaporative Cooler Types
        CASE('EVAPORATIVECOOLER:DIRECT:CELDEKPAD')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= EvapCooler
        CASE('EVAPORATIVECOOLER:INDIRECT:CELDEKPAD')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= EvapCooler
        CASE('EVAPORATIVECOOLER:INDIRECT:WETCOIL')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= EvapCooler
        CASE('EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= EvapCooler
        CASE('EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL')
          OutsideAirSys(OASysNum)%ComponentType_Num(CompNum)= EvapCooler
        CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(AlphArray(1))//'" invalid '//  &
           'Outside Air Component="'//TRIM(OutsideAirSys(OASysNum)%ComponentType(CompNum))//'".')
          ErrorsFound=.true.

      END SELECT
    ENDDO

  ENDDO

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//'.')
ENDIF

DEALLOCATE(AlphArray)
DEALLOCATE(cAlphaFields)
DEALLOCATE(NumArray)
DEALLOCATE(cNumericFields)
DEALLOCATE(lAlphaBlanks)
DEALLOCATE(lNumericBlanks)

GetOASysInputFlag = .FALSE.

RETURN

END SUBROUTINE GetOutsideAirSysInputs

SUBROUTINE GetOAControllerInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       Shirey/Raustad FSEC, June 2003, Jan 2004
          !                      Mangesh Basarkar, 06/2011: Getting zone OA specifications from Design Specification Object
          !                      Tianzhen Hong, 3/2012: getting zone air distribution effectiveness and secondary recirculation
          !                       from DesignSpecification:ZoneAirDistribution objects
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Input the OAController data and store it in the OAController array.
          ! Input the Ventilation:Mechanical data and store it in the VentilationMechanical array.
          !  Condense Ventilation:Mechanical data array to include only unique zones specified for each instance of this object.

          ! METHODOLOGY EMPLOYED:
          ! Use the Get routines from the InputProcessor module.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor
    USE DataDefineEquip
    USE General,           ONLY: TrimSigDigits, RoundSigDigits
    USE NodeInputManager,  ONLY: GetOnlySingleNode
    USE DataZoneEquipment, ONLY: ZoneEquipConfig, ZoneEquipList, NumofZoneEquipLists
    USE DataHeatBalance,   ONLY: Zone, ZoneList, NumOfZoneLists
    USE CurveManager,      ONLY: GetCurveIndex, GetCurveType
    USE OutputReportPredefined

    USE DataAirSystems,    ONLY: PrimaryAirSystem
    USE DataZoneControls,  ONLY: HumidityControlZone, NumHumidityControlZones
    USE DataContaminantBalance, ONLY: Contaminant
    USE OutAirNodeManager, ONLY: CheckOutAirNodeNumber

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=1), PARAMETER :: Blank=' '
CHARACTER(len=*), PARAMETER :: RoutineName='GetOAControllerInputs: ' ! include trailing blank space


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: MaxNumAirLoopZones   ! maximum number of heating plus cooling zones attached to any air loop
INTEGER :: NumAirLoopZones      ! number of heating plus cooling zones attached to a given air loop
INTEGER :: NumofAirLoop         ! counter for NumPrimaryAirSys
INTEGER :: NumAirLoopCooledZones     ! number of cooling zones for a given air loop
INTEGER :: NumAirLoopCooledZonesTemp ! index for number of cooling zones
INTEGER :: AirLoopZones         ! total number of unique heating and cooling zones for each air loop
INTEGER :: NumAirLoopHeatedZones     ! number of heating zones for a given air loop
INTEGER :: NumAirLoopHeatedZonesTemp ! index for number of heating zones
INTEGER :: ZoneNum              ! zone number attached to a given air loop
LOGICAL :: CommonZone           ! logical for the same zone being a cooling zone and a heating zone
INTEGER :: NumNums         ! Number of real numbers returned by GetObjectItem
INTEGER :: NumAlphas       ! Number of alphanumerics returned by GetObjectItem
INTEGER :: OutAirNum       ! Number of Controller:OutdoorAir or CONTROLLER:STAND ALONE ERV objects
INTEGER :: OAControllerNum ! Index to Controller:OutdoorAir or CONTROLLER:STAND ALONE ERV objects
INTEGER :: VentMechNum ! Number of VENTILATION:MECHANICAL objects
INTEGER :: groupNum ! Index to group in extensible VENTILATION:MECHANICAL object
INTEGER :: IOSTAT      ! Status of GetObjectItem call
REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray
CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray
CHARACTER(len=MaxNameLength) :: CurrentModuleObject   ! Object type for getting and messages
CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
LOGICAL,  ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks     ! Logical array, alpha field input BLANK = .true.
LOGICAL,  ALLOCATABLE, DIMENSION(:) :: lNumericBlanks   ! Logical array, numeric field input BLANK = .true.
LOGICAL :: ErrorsFound=.false.   ! Flag identifying errors found during get input
LOGICAL :: IsNotOK               ! Flag to verify name
LOGICAL :: IsBlank               ! Flag for blank name
INTEGER :: ZoneListNum           ! Index to Zone List
INTEGER :: ScanZoneListNum       ! Index used to loop through zone list
INTEGER :: MechVentZoneCount     ! Index counter for zones with mechanical ventilation
!LOGICAL :: FoundUniqueZone       ! Flag to verify VENTIALTION:MECHANICAL zones are unique and no duplicates exist
!INTEGER :: NumVentMechZone       ! Index counter for checking mechanical ventilation zone uniqeness
LOGICAL :: ErrorInName           ! Error returned from VerifyName call
INTEGER :: NumArg                ! Number of arguments from GetObjectDefMaxArgs call
INTEGER :: MaxAlphas             ! Maximum alphas in multiple objects
INTEGER :: MaxNums               ! Maximum numbers in multiple objects
!INTEGER :: ERVControllerNum     ! Index to Controller:Stand Alone ERV
Integer :: ControlledZoneNum     ! Index to controlled zones
LOGICAL :: AirNodeFound          ! Used to determine if control zone is valid
LOGICAL :: AirLoopFound          ! Used to determine if control zone is served by furnace air loop
INTEGER :: AirLoopNumber         ! Used to determine if control zone is served by furnace air loop
INTEGER :: BranchNum             ! Used to determine if control zone is served by furnace air loop
INTEGER :: CompNum               ! Used to determine if control zone is served by furnace air loop
INTEGER :: HstatZoneNum          ! Used to determine if control zone has a humidistat object
INTEGER :: OASysNum              ! Used to find OA System index for OA Controller
INTEGER :: OASysIndex            ! Index to OA System
LOGICAL :: OASysFound            ! OA Controller found OA System index
REAL(r64) :: OAFlowRatio         ! Ratio of minimum OA flow rate to maximum OA flow rate

INTEGER :: NumGroups             ! Number of extensible input groups of the VentilationMechanical object
INTEGER :: numBaseNum            ! base number for numeric arguments (for readability)
!INTEGER :: OAIndex               ! Loop index for design specification outdoor air object list
!INTEGER :: NumControllerList = 0  ! Index to controller lists
INTEGER :: ControllerListNum = 0  ! Index used to loop through controller list
INTEGER :: ControllerNum = 0      ! Index to controllers in each controller list
INTEGER :: Num = 0                ! Index used to loop through controllers in list
INTEGER :: SysNum = 0             ! Index used to loop through OA systems
REAL(r64) :: DesSupplyVolFlowRate = 0.0d0  ! Temporary variable for design supply volumetric flow rate for air loop (m3/s)
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE:: DesignSpecOAObjName ! name of the design specification outdoor air object
INTEGER,DIMENSION(:),ALLOCATABLE:: DesignSpecOAObjIndex ! index of the design specification outdoor air object
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE:: VentMechZoneName ! Zone or Zone List to apply mechanical ventilation rate
REAL(r64), DIMENSION(:),ALLOCATABLE:: VentMechZoneOAAreaRate     ! Mechanical ventilation rate (m3/s/m2) for zone or zone list
REAL(r64), DIMENSION(:),ALLOCATABLE:: VentMechZoneOAPeopleRate   ! Mechanical ventilation rate (m3/s/person) for zone or zone list
REAL(r64), DIMENSION(:),ALLOCATABLE:: VentMechZoneOAFlow         ! Mechanical ventilation rate (m3/s/person) for zone or zone list
REAL(r64), DIMENSION(:),ALLOCATABLE:: VentMechZoneOAACH          ! Mechanical ventilation rate (m3/s/person) for zone or zone list

REAL(r64), DIMENSION(:),ALLOCATABLE:: VentMechZoneADEffCooling    ! Zone air distribution effectiveness in cooling mode
                                                                  ! for each zone or zone list
REAL(r64), DIMENSION(:),ALLOCATABLE:: VentMechZoneADEffHeating    ! Zone air distribution effectiveness in heating mode
                                                                  ! for each zone or zone list
INTEGER, DIMENSION(:),ALLOCATABLE:: VentMechZoneADEffSchPtr       ! Pointer to the zone air distribution effectiveness schedule
                                                                  ! for each zone or zone list
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE:: VentMechZoneADEffSchName  ! Zone air distribution effectiveness
                                                                                  !  schedule name for each zone or zone list

REAL(r64), DIMENSION(:),ALLOCATABLE:: VentMechZoneSecondaryRecirculation          ! Zone air secondary recirculation ratio
                                                                                  !  for each zone or zone list
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE:: DesignSpecZoneADObjName   ! name of the design specification zone air
                                                                                  !  distribution object for each zone or zone list
INTEGER,DIMENSION(:),ALLOCATABLE:: DesignSpecZoneADObjIndex       ! index of the design specification zone air distribution object

INTEGER :: ObjIndex = 0
INTEGER :: EquipListIndex = 0
INTEGER :: EquipNum = 0
INTEGER :: EquipListNum = 0
INTEGER :: ADUNum = 0
INTEGER :: jZone
INTEGER :: i

!First, call other get input routines in this module to make sure data is filled during this routine.
IF (GetOASysInputFlag) THEN  ! Gets input for object  first time Sim routine is called
  CALL GetOutsideAirSysInputs
  GetOASysInputFlag=.false.
END IF
IF (GetOAMixerInputFlag) THEN  ! Gets input for object  first time Sim routine is called
  CALL GetOAMixerInputs
  GetOAMixerInputFlag=.false.
END IF


CALL GetObjectDefMaxArgs(CurrentModuleObjects(CMO_OAController),NumArg,NumAlphas,NumNums)
MaxAlphas=NumAlphas
MaxNums=NumNums
CALL GetObjectDefMaxArgs(CurrentModuleObjects(CMO_ERVController),NumArg,NumAlphas,NumNums)
MaxAlphas=MAX(MaxAlphas,NumAlphas)
MaxNums=MAX(MaxNums,NumNums)
CALL GetObjectDefMaxArgs(CurrentModuleObjects(CMO_MechVentilation),NumArg,NumAlphas,NumNums)
MaxAlphas=MAX(MaxAlphas,NumAlphas)
MaxNums=MAX(MaxNums,NumNums)

ALLOCATE(AlphArray(MaxAlphas))
AlphArray=' '
ALLOCATE(NumArray(MaxNums))
NumArray=0.0d0
ALLOCATE(lAlphaBlanks(MaxAlphas))
lAlphaBlanks=.true.
ALLOCATE(lNumericBlanks(MaxNums))
lNumericBlanks=.true.
ALLOCATE(cAlphaFields(MaxAlphas))
cAlphaFields = ' '
ALLOCATE(cNumericFields(MaxNums))
cNumericFields = ' '

NumOAControllers = GetNumObjectsFound(CurrentModuleObjects(CMO_OAController))
NumERVControllers = GetNumObjectsFound(CurrentModuleObjects(CMO_ERVController))
NumOAControllers = NumOAControllers + NumERVControllers

!     Mangesh code to fix CR 8225 - 09/14/2010
!NumControllerList = GetNumObjectsFound("AirLoopHVAC:ControllerList")
!NumOASys = GetNumObjectsFound("AirLoopHVAC:OutdoorAirSystem")

IF (NumOAControllers.GT.0) THEN

  ALLOCATE(OAController(NumOAControllers))
  ALLOCATE(OAControllerInfo(NumOAControllers))
  CurrentModuleObject = CurrentModuleObjects(CMO_OAController)
  DO OutAirNum=1,NumOAControllers-NumERVControllers
    CALL GetObjectItem(CurrentModuleObject,OutAirNum,AlphArray,NumAlphas,&
                       NumArray,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),OAController%Name,OutAirNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    OAController(OutAirNum)%Name  = AlphArray(1)
    OAController(OutAirNum)%ControllerType  = TRIM(CurrentModuleObject)
    OAController(OutAirNum)%ControllerType_Num  = ControllerOutsideAir
    OAController(OutAirNum)%MaxOA = NumArray(2)
    OAController(OutAirNum)%MinOA = NumArray(1)
    OAController(OutAirNum)%MixNode    = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
    OAController(OutAirNum)%OANode     = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Actuator,1,ObjectIsNotParent)
    IF (.not. CheckOutAirNodeNumber(OAController(OutAirNum)%OANode)) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid field ')
      CALL ShowContinueError(TRIM(cAlphaFields(5))//'="'//trim(AlphArray(5))//'",'//  &
                              ' must be an OutdoorAir:Node for outdoor air to be effective.')
      ErrorsFound=.true.
    ENDIF
    IF (Samestring(AlphArray(6),'NoEconomizer')) THEN
      OAController(OutAirNum)%Econo = NoEconomizer
    ELSE IF (Samestring(AlphArray(6),'FixedDryBulb')) THEN
      OAController(OutAirNum)%Econo = FixedDryBulb
    ELSE IF (Samestring(AlphArray(6),'FixedEnthalpy')) THEN
      OAController(OutAirNum)%Econo = FixedEnthalpy
    ELSE IF (Samestring(AlphArray(6),'FixedDewPointAndDryBulb')) THEN
      OAController(OutAirNum)%Econo = FixedDewpointAndDryBulb
    ELSE IF (Samestring(AlphArray(6),'DifferentialDryBulb')) THEN
      OAController(OutAirNum)%Econo = DifferentialDryBulb
    ELSE IF (Samestring(AlphArray(6),'DifferentialEnthalpy')) THEN
      OAController(OutAirNum)%Econo = DifferentialEnthalpy
    ELSE IF (Samestring(AlphArray(6),'DifferentialDryBulbAndEnthalpy')) THEN
      OAController(OutAirNum)%Econo = DifferentialDryBulbAndEnthalpy
    ELSE IF (Samestring(AlphArray(6),'ElectronicEnthalpy')) THEN
      OAController(OutAirNum)%Econo = ElectronicEnthalpy
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
         TRIM(cAlphaFields(6))//'="'//trim(AlphArray(6))//'" value.')
      ErrorsFound=.true.
    END IF
    !Bypass choice - Added by Amit for new feature implementation
    IF(Samestring(AlphArray(7),'ModulateFlow')) THEN
      OAController(OutAirNum)%Econbypass = .FALSE.
    ELSE IF(Samestring(AlphArray(7),'MinimumFlowWithBypass')) THEN
      OAController(OutAirNum)%EconBypass = .TRUE.
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
         TRIM(cAlphaFields(7))//'="'//trim(AlphArray(7))//'" value.')
      ErrorsFound=.true.
    END IF

!    IF((OAController(OutAirNum)%Econo > NoEconomizer) .AND. OAController(OutAirNum)%EconBypass) THEN
!      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
!         TRIM(cAlphaFields(6))//'="'//trim(AlphArray(6))//'" and ')
!      CALL ShowContinueError(TRIM(cAlphaFields(7))//'="'//trim(AlphArray(7))//'" incompatible specifications.')
!      Errorsfound = .TRUE.
!    END IF
    IF (SameString(AlphArray(9),'NoLockout')) THEN
      OAController(OutAirNum)%Lockout = NoLockoutPossible
    ELSE IF (SameString(AlphArray(9),'LockoutWithHeating')) THEN
      OAController(OutAirNum)%Lockout = LockoutWithHeatingPossible
    ELSE IF (SameString(AlphArray(9),'LockoutWithCompressor')) THEN
      OAController(OutAirNum)%Lockout = LockoutWithCompressorPossible
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
         TRIM(cAlphaFields(9))//'="'//trim(AlphArray(9))//'" value.')
      ErrorsFound=.true.
    END IF
    IF (SameString(AlphArray(10),'FixedMinimum')) THEN
      OAController(OutAirNum)%FixedMin = .TRUE.
    ELSE
      OAController(OutAirNum)%FixedMin = .FALSE.
    END IF
    IF (lNumericBlanks(3)) THEN
      OAController(OutAirNum)%TempLim = BlankNumeric
    ELSE
      OAController(OutAirNum)%TempLim = NumArray(3)
    END IF

    IF (lNumericBlanks(4)) THEN
      OAController(OutAirNum)%EnthLim = BlankNumeric
    ELSE
      OAController(OutAirNum)%EnthLim = NumArray(4)
    END IF
    IF (lNumericBlanks(5)) THEN
      OAController(OutAirNum)%DPTempLim = BlankNumeric
    ELSE
      OAController(OutAirNum)%DPTempLim = NumArray(5)
    END IF

    IF (lNumericBlanks(6)) THEN
      OAController(OutAirNum)%TempLowLim = BlankNumeric
    ELSE
      OAController(OutAirNum)%TempLowLim = NumArray(6)
    END IF

    IF(.NOT. lAlphaBlanks(8))THEN
      OAController(OutAirNum)%EnthalpyCurvePtr = GetCurveIndex(AlphArray(8)) ! convert curve name to number
      IF (OAController(OutAirNum)%EnthalpyCurvePtr .EQ. 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
           TRIM(cAlphaFields(8))//'="'//trim(AlphArray(8))//'" not found.')
        ErrorsFound = .TRUE.
      ELSE
        ! Verify Curve Object, only legal types are Quadratic and Cubic
        SELECT CASE(GetCurveType(OAController(OutAirNum)%EnthalpyCurvePtr))

        CASE('QUADRATIC')

        CASE('CUBIC')

        CASE DEFAULT
          CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
             TRIM(cAlphaFields(8))//'="'//trim(AlphArray(8))//'".')
          CALL ShowContinueError('...must be Quadratic or Cubic curve.')
          ErrorsFound=.true.
        END SELECT
      END IF
    END IF

    OAController(OutAirNum)%RelNode    = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Actuator,1,ObjectIsNotParent)
    OAController(OutAirNum)%RetNode = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
    OAController(OutAirNum)%MinOASch = AlphArray(11)
    OAController(OutAirNum)%MinOASchPtr = GetScheduleIndex(AlphArray(11))
    IF (OAController(OutAirNum)%MinOASchPtr == 0 .AND. (.NOT. lAlphaBlanks(11))) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
             TRIM(cAlphaFields(11))//'="'//trim(AlphArray(11))//'" not found.')
      ErrorsFound=.true.
    ENDIF


   ! Changed by Amit for new feature implementation
    OAController(OutAirNum)%MinOAflowSch = AlphArray(12)
    OAController(OutAirNum)%MinOAflowSchPtr = GetScheduleIndex(AlphArray(12))
    IF (OAController(OutAirNum)%MinOAflowSchPtr == 0 .AND. (.NOT. lAlphaBlanks(12))) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
             TRIM(cAlphaFields(12))//'="'//trim(AlphArray(12))//'" not found.')
      ErrorsFound=.true.
    ENDIF

    OAController(OutAirNum)%MaxOAflowSch = AlphArray(13)
    OAController(OutAirNum)%MaxOAflowSchPtr = GetScheduleIndex(AlphArray(13))
    IF (OAController(OutAirNum)%MaxOAflowSchPtr == 0 .AND. (.NOT. lAlphaBlanks(13))) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
             TRIM(cAlphaFields(13))//'="'//trim(AlphArray(13))//'" not found.')
      ErrorsFound=.true.
    ENDIF
    OAController(OutAirNum)%VentilationMechanicalName = AlphArray(14)

!   Check for a time of day economizer control schedule
    OAController(OutAirNum)%EconomizerOASchedPtr = GetScheduleIndex(AlphArray(15))

!   High humidity control option can be used with any economizer flag
    IF(SameString(AlphArray(16),'Yes'))THEN

      OAController(OutAirNum)%HumidistatZoneNum = FindItemInList(AlphArray(17),Zone%Name,NumOfZones)

      ! Get the node number for the zone with the humidistat
      IF (OAController(OutAirNum)%HumidistatZoneNum > 0) THEN
        AirNodeFound=.FALSE.
        AirLoopFound=.FALSE.
        OASysFound  =.FALSE.
        DO ControlledZoneNum = 1,NumOfZones
          IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum /= OAController(OutAirNum)%HumidistatZoneNum) CYCLE
!           Find the controlled zone number for the specified humidistat location
            OAController(OutAirNum)%NodeNumofHumidistatZone=ZoneEquipConfig(ControlledZoneNum)%ZoneNode
!           Determine which OA System uses this OA Controller
            OASysIndex = 0
            DO OASysNum = 1, NumOASystems
              DO OAControllerNum = 1, OutsideAirSys(OASysNum)%NumControllers
              IF(.NOT. SameString(OutsideAirSys(OASysNum)%ControllerType(OAControllerNum),CurrentModuleObject) .OR. &
                 .NOT. SameString(OutsideAirSys(OASysNum)%ControllerName(OAControllerNum),OAController(OutAirNum)%Name)) CYCLE
                OASysIndex = OASysNum
                OASysFound = .TRUE.
                EXIT
              END DO
              IF(OASysFound) EXIT
            END DO
!           Determine if furnace is on air loop served by the humidistat location specified
            AirLoopNumber = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
            IF(AirLoopNumber .GT. 0 .AND. OASysIndex .GT. 0)THEN
              DO BranchNum = 1, PrimaryAirSystem(AirLoopNumber)%NumBranches
                DO CompNum = 1, PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%TotalComponents
                  IF(.NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%Name, &
                           OutsideAirSys(OASysIndex)%Name) .OR. &
                     .NOT. SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                           'AirLoopHVAC:OutdoorAirSystem'))CYCLE
                  AirLoopFound=.TRUE.
                  EXIT
                END DO
                IF(AirLoopFound)EXIT
              END DO
              DO HstatZoneNum = 1, NumHumidityControlZones
                IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. OAController(OutAirNum)%HumidistatZoneNum)CYCLE
                AirNodeFound=.TRUE.
                EXIT
              END DO
            ELSE
              IF(AirLoopNumber .EQ. 0)THEN
                CALL ShowSevereError('Did not find a Primary Air Loop for ' &
                                    //TRIM(OAController(OutAirNum)%ControllerType)//' = "' &
                                    //TRIM(OAController(OutAirNum)%Name)//'"')
                CALL ShowContinueError('Specified '//TRIM(cAlphaFields(17))//' = '//TRIM(AlphArray(17)))
                ErrorsFound=.TRUE.
              END IF
              IF(OASysIndex .EQ. 0)THEN
                CALL ShowSevereError('Did not find an AirLoopHVAC:OutdoorAirSystem for ' &
                                     //TRIM(OAController(OutAirNum)%ControllerType)//' = "' &
                                     //TRIM(OAController(OutAirNum)%Name)//'"')
                ErrorsFound=.TRUE.
              END IF
            END IF
          EXIT
        ENDDO
        IF (.not. AirNodeFound) THEN
          CALL ShowSevereError('Did not find Air Node (Zone with Humidistat), ' &
                               //TRIM(OAController(OutAirNum)%ControllerType)//' = "' &
                               //TRIM(OAController(OutAirNum)%Name)//'"')
          CALL ShowContinueError('Specified '//TRIM(cAlphaFields(17))//' = '//TRIM(AlphArray(17)))
          CALL ShowContinueError('Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Humidistat object' &
                                 //' must be specified for this zone.')
          ErrorsFound=.TRUE.
        ENDIF
        IF (.not. AirLoopFound) THEN
          CALL ShowSevereError('Did not find correct Primary Air Loop for ' &
                               //TRIM(OAController(OutAirNum)%ControllerType)//' = "' &
                               //TRIM(OAController(OutAirNum)%Name)//'"')
          CALL ShowContinueError(TRIM(cAlphaFields(17))//' = '//TRIM(AlphArray(17))// &
                                 ' is not served by this Primary Air Loop equipment.')
          ErrorsFound=.TRUE.
        ENDIF
      ELSE
        CALL ShowSevereError('Did not find Air Node (Zone with Humidistat), ' &
                            //TRIM(OAController(OutAirNum)%ControllerType)//' = "' &
                            //TRIM(OAController(OutAirNum)%Name)//'"')
        CALL ShowContinueError('Specified '//TRIM(cAlphaFields(17))//' = '//TRIM(AlphArray(17)))
        CALL ShowContinueError('Both a ZoneHVAC:EquipmentConnections object and a ZoneControl:Humidistat object' &
                               //' must be specified for this zone.')
        ErrorsFound=.TRUE.
      ENDIF

      OAController(OutAirNum)%HighRHOAFlowRatio = NumArray(7)
      IF(OAController(OutAirNum)%HighRHOAFlowRatio .LE. 0.0d0 .AND. NumNums .GT. 6)THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(OAController(OutAirNum)%Name)//'"')
        CALL ShowContinueError(' '//TRIM(cNumericFields(7))//' must be greater than 0.')
        CALL ShowContinueError(' '//TRIM(cNumericFields(7))//' is reset to 1 and the simulation continues.')
        OAController(OutAirNum)%HighRHOAFlowRatio = 1.0d0
      END IF

      IF(SameString(AlphArray(16),'Yes') .AND. OAController(OutAirNum)%FixedMin)THEN
        IF(OAController(OutAirNum)%MaxOA .GT. 0.0d0 .AND. OAController(OutAirNum)%MinOA .NE. AutoSize)THEN
          OAFlowRatio = OAController(OutAirNum)%MinOA/OAController(OutAirNum)%MaxOA
          IF(OAController(OutAirNum)%HighRHOAFlowRatio .LT. OAFlowRatio)THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(OAController(OutAirNum)%Name)//'"')
            CALL ShowContinueError('... A fixed minimum outside air flow rate and high humidity control have been specified.')
            CALL ShowContinueError('... The '//TRIM(cNumericFields(7))//' is less than the ratio of'// &
                                   ' the outside air controllers minimum to maximum outside air flow rate.')
            CALL ShowContinueError('... Controller '//TRIM(cNumericFields(1))//' = ' &
                                    //TRIM(TrimSigDigits(OAController(OutAirNum)%MinOA,4))//' m3/s.')
            CALL ShowContinueError('... Controller '//TRIM(cNumericFields(2))//' = ' &
                                    //TRIM(TrimSigDigits(OAController(OutAirNum)%MaxOA,4))//' m3/s.')
            CALL ShowContinueError('... Controller minimum to maximum flow ratio = ' &
                                    //TRIM(TrimSigDigits(OAFlowRatio,4))//'.')
            CALL ShowContinueError('... '//TRIM(cNumericFields(7))//' = ' &
                                    //TRIM(TrimSigDigits(OAController(OutAirNum)%HighRHOAFlowRatio,4))//'.')
          END IF
        END IF
      END If

      IF(SameString(AlphArray(18), 'Yes'))THEN
        OAController(OutAirNum)%ModifyDuringHighOAMoisture = .FALSE.
      ELSEIF (SameString(AlphArray(18), 'No'))THEN
        OAController(OutAirNum)%ModifyDuringHighOAMoisture = .TRUE.
      ELSE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(OAController(OutAirNum)%Name)//'", invalid field value')
        CALL ShowContinueError('...'//trim(cAlphaFields(18))//'="'//trim(AlphArray(18))//'" - valid values are "Yes" or "No".')
        ErrorsFound=.TRUE.
      END IF

    ELSEIF (SameString(AlphArray(16),'No') .or. lAlphaBlanks(16))THEN
      IF (NumAlphas >= 18) THEN
        IF(.not. SameString(AlphArray(18), 'Yes') .and. .not. SameString(AlphArray(18), 'No'))THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(OAController(OutAirNum)%Name)//'", invalid field value')
          CALL ShowContinueError('...'//trim(cAlphaFields(18))//'="'//trim(AlphArray(18))//'" - valid values are "Yes" or "No".')
          ErrorsFound=.TRUE.
        END IF
      ENDIF
    ELSE  ! Invalid field 16
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(OAController(OutAirNum)%Name)//'", invalid field value')
      CALL ShowContinueError('...'//trim(cAlphaFields(16))//'="'//trim(AlphArray(16))//'" - valid values are "Yes" or "No".')
      ErrorsFound=.TRUE.
      IF (NumAlphas >= 18) THEN
        IF(.not. SameString(AlphArray(18), 'Yes') .and. .not. SameString(AlphArray(18), 'No'))THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(OAController(OutAirNum)%Name)//'", invalid field value')
          CALL ShowContinueError('...'//trim(cAlphaFields(18))//'="'//trim(AlphArray(18))//'" - valid values are "Yes" or "No".')
          ErrorsFound=.TRUE.
        END IF
      ENDIF
    END IF

    IF(NumAlphas .GT. 18)THEN
      IF(.NOT. lAlphaBlanks(19))THEN
        IF(SameString(AlphArray(19), 'BypassWhenWithinEconomizerLimits'))THEN
          OAController(OutAirNum)%HeatRecoveryBypassControlType = BypassWhenWithinEconomizerLimits
        ELSE IF(SameString(AlphArray(19), 'BypassWhenOAFlowGreaterThanMinimum'))THEN
          OAController(OutAirNum)%HeatRecoveryBypassControlType = BypassWhenOAFlowGreaterThanMinimum
        ELSE
          CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
             TRIM(cAlphaFields(19))//'="'//trim(AlphArray(19))//'".')
          CALL ShowContinueError('...assuming "BypassWhenWithinEconomizerLimits" and the simulation continues.')
          OAController(OutAirNum)%HeatRecoveryBypassControlType = BypassWhenWithinEconomizerLimits
        END IF
      END IF
    END IF

    IF(SameString(AlphArray(16),'Yes') .AND. OAController(OutAirNum)%Econo .EQ. NoEconomizer)THEN
      CALL ShowWarningError(TRIM(OAController(OutAirNum)%ControllerType)//' "' &
                              //TRIM(OAController(OutAirNum)%Name)//'"')
      CALL ShowContinueError('...Economizer operation must be enabled when '//TRIM(cAlphaFields(16))//' is set to YES.')
      CALL ShowContinueError('...The high humidity control option will be disabled and the simulation continues.')
    END IF

!     Mangesh code to fix CR 8225 - 09/14/2010
    IF ((NumControllerLists > 0) .AND. (NumOASystems > 0)) THEN
OALp: DO AirLoopNumber = 1, NumPrimaryAirSys
        DesSupplyVolFlowRate = AirLoopFlow(AirLoopNumber)%DesSupply / StdRhoAir
        DO BranchNum = 1, PrimaryAirSystem(AirLoopNumber)%NumBranches
          DO CompNum = 1, PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%TotalComponents
            DO ControllerListNum = 1, NumControllerLists
              ControllerNum = ControllerLists(ControllerListNum)%NumControllers
              IF (ControllerNum > 0) THEN
                DO Num = 1, ControllerNum
                  DO SysNum = 1, NumOASystems
                    IF (SameString(PrimaryAirSystem(AirLoopNumber)%Branch(BranchNum)%Comp(CompNum)%Name,&
                        OutsideAirSys(SysNum)%Name)) THEN
                      IF (SameString(OutsideAirSys(SysNum)%ControllerListName,ControllerLists(ControllerListNum)%Name)) THEN
                        IF (SameString(OAController(OutAirNum)%Name,ControllerLists(ControllerListNum)%ControllerName(Num))) THEN
                          IF ((OAController(OutAirNum)%MinOA-DesSupplyVolFlowRate) > .0001d0) THEN
                            CALL ShowWarningError('Minimum outside air flow rate for OA Controller "' //   &
                               TRIM(OAController(OutAirNum)%Name) // &
                               '" is greater than maximum supply flow rate for Air Loop "'//  &
                               TRIM(PrimaryAirSystem(AirLoopNumber)%Name)//'"')
                            CALL ShowContinueError('...Min for OA Controller=['//  &
                                TRIM(RoundSigDigits(OAController(OutAirNum)%MinOA,6))//'], Max Supply Flow Rate=['//  &
                                TRIM(RoundSigDigits(DesSupplyVolFlowRate,6))//'].')
                            Call ShowContinueError('...Minimum outside air flow ' &
                                        // 'rate will be reset to equal maximum supply flow rate')
                            OAController(OutAirNum)%MinOA = DesSupplyVolFlowRate
                          ELSEIF ((OAController(OutAirNum)%MinOA-DesSupplyVolFlowRate) > 0.0d0) THEN
                            OAController(OutAirNum)%MinOA = DesSupplyVolFlowRate
                          ENDIF
                          Exit OALp  ! Found and checked
                        END IF
                      ENDIF
                    ENDIF
                  END DO
                END DO
              ENDIF
            END DO
          END DO
        END DO
      END DO OALp
    ENDIF

    ! add applicable faults identifier to avoid string comparison at each time step
    !  loop through each fault for each OA controller
    DO i = 1, NumFaults
      IF (Faults(i)%ControllerTypeEnum /= iController_AirEconomizer) CYCLE
      IF (SameString(OAController(OutAirNum)%Name, Faults(i)%ControllerName)) THEN
        Faults(i)%ControllerID = OutAirNum
      ENDIF
    ENDDO

  END DO  ! LOOP FOR OutAirNum

  IF (ErrorsFound) THEN
    DEALLOCATE(AlphArray)
    DEALLOCATE(NumArray)
    DEALLOCATE(lNumericBlanks)
    DEALLOCATE(lAlphaBlanks)
    DEALLOCATE(cAlphaFields)
    DEALLOCATE(cNumericFields)
    CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' inputs.')
  ENDIF

END IF

GetOAControllerInputFlag=.false.

! Find the maximum number of zones attached to any air loop, used for mechanical ventilation objects
MaxNumAirLoopZones = 0
DO NumofAirLoop = 1, NumPrimaryAirSys
  NumAirLoopZones = AirToZoneNodeInfo(NumofAirLoop)%NumZonesCooled + &
           AirToZoneNodeInfo(NumofAirLoop)%NumZonesHeated
! NumZonesCooled + NumZonesHeated must be > 0 or Fatal error is issued in SimAirServingZones
  MaxNumAirLoopZones = MAX(MaxNumAirLoopZones, NumAirLoopZones) ! Max number of zones on any air loop being simulated
END DO

IF (NumPrimaryAirSys .GT. 0) THEN
  ALLOCATE (AirLoopZoneInfo(NumPrimaryAirSys)) ! Defined in DataAirLoop.f90
END IF

! Find the zones attached to each air loop
DO NumofAirLoop = 1, NumPrimaryAirSys
  ALLOCATE (AirLoopZoneInfo(NumofAirLoop)%Zone(MaxNumAirLoopZones))
  ALLOCATE (AirLoopZoneInfo(NumofAirLoop)%ActualZoneNumber(MaxNumAirLoopZones))
  NumAirLoopCooledZones = AirToZoneNodeInfo(NumofAirLoop)%NumZonesCooled
  AirLoopZones = NumAirLoopCooledZones
  NumAirLoopHeatedZones = AirToZoneNodeInfo(NumofAirLoop)%NumZonesHeated
  ! Store cooling zone numbers in AirLoopZoneInfo data structure
  DO NumAirLoopCooledZonesTemp = 1, NumAirLoopCooledZones
    AirLoopZoneInfo(NumofAirLoop)%Zone(NumAirLoopCooledZonesTemp) = &
       AirToZoneNodeInfo(NumofAirLoop)%CoolCtrlZoneNums(NumAirLoopCooledZonesTemp)
    AirLoopZoneInfo(NumofAirLoop)%ActualZoneNumber(NumAirLoopCooledZonesTemp) = &
       ZoneEquipConfig(AirToZoneNodeInfo(NumofAirLoop)%CoolCtrlZoneNums(NumAirLoopCooledZonesTemp))%ActualZoneNum
  END DO
  ! Store heating zone numbers in AirLoopZoneInfo data structure
  ! Only store zone numbers that aren't already defined as cooling zones above
  DO NumAirLoopHeatedZonesTemp = 1,NumAirLoopHeatedZones
    ZoneNum = AirToZoneNodeInfo(NumofAirLoop)%HeatCtrlZoneNums(NumAirLoopHeatedZonesTemp)
    CommonZone = .FALSE.
    DO NumAirLoopCooledZonesTemp = 1, NumAirLoopCooledZones
      IF(ZoneNum /= AirToZoneNodeInfo(NumofAirLoop)%CoolCtrlZoneNums(NumAirLoopCooledZonesTemp)) CYCLE
        CommonZone = .TRUE.
    END DO
    IF(.NOT. CommonZone) THEN
       AirLoopZones = AirLoopZones + 1
       AirLoopZoneInfo(NumofAirLoop)%Zone(AirLoopZones) = ZoneNum
       AirLoopZoneInfo(NumofAirLoop)%ActualZoneNumber(AirLoopZones) = ZoneEquipConfig(ZoneNum)%ActualZoneNum
    END IF
  END DO
  AirLoopZoneInfo(NumofAirLoop)%NumZones = AirLoopZones
END DO

! Process Controller:MechanicalVentilation objects
CurrentModuleObject = CurrentModuleObjects(CMO_MechVentilation)
NumVentMechControllers = GetNumObjectsFound(CurrentModuleObject)
IF(NumVentMechControllers .GT. 0) THEN
  ALLOCATE(VentilationMechanical(NumVentMechControllers))
  DO VentMechNum=1,NumVentMechControllers
    CALL GetObjectItem(CurrentModuleObject,VentMechNum,AlphArray,NumAlphas,&
                       NumArray,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    MechVentZoneCount = 0

    NumGroups = (NumAlphas + NumNums - 5)/3
    IF (MOD((NumAlphas + NumNums - 5),3) /= 0) NumGroups=NumGroups+1
    VentilationMechanical(VentMechNum)%Name  = AlphArray(1)

    ! Check Controller:MechanicalVentilation object name
    ErrorInName = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(AlphArray(1),VentilationMechanical%Name,VentMechNum-1,ErrorInName,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) AlphArray(1)='xxxxx'
    END IF

    VentilationMechanical(VentMechNum)%SchName = AlphArray(2)
    IF (lAlphaBlanks(2)) THEN
      VentilationMechanical(VentMechNum)%SchPtr = ScheduleAlwaysOn
    ELSE
      VentilationMechanical(VentMechNum)%SchPtr = GetScheduleIndex(AlphArray(2))  ! convert schedule name to pointer
      IF (VentilationMechanical(VentMechNum)%SchPtr .EQ. 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'" invalid '//  &
               TRIM(cAlphaFields(2))//'="'//TRIM(AlphArray(2))//'" not found.')
        ErrorsFound=.TRUE.
      END IF
    ENDIF

!   Adding new flag for DCV
    IF (SameString(AlphArray(3), 'Yes')) THEN
        VentilationMechanical(VentMechNum)%DCVFlag = .TRUE.
    ELSEIF (SameString(AlphArray(3), 'No') .or. lAlphaBlanks(3)) THEN
        VentilationMechanical(VentMechNum)%DCVFlag = .FALSE.
    ELSE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'" invalid value '//  &
             TRIM(cAlphaFields(3))//'="'//TRIM(AlphArray(3))//'".')
      CALL ShowContinueError('...Valid values are "Yes" or "No".')
      ErrorsFound=.TRUE.
    ENDIF

    ! System outdoor air method
    SELECT CASE (MakeUPPERCase(AlphArray(4)))
      CASE ('ZONESUM')  ! Simplifily sum the zone OA flow rates
        VentilationMechanical(VentMechNum)%SystemOAMethod = SOAM_ZoneSum
      CASE ('VRP','VENTILATIONRATEPROCEDURE')      ! Ventilation Rate Procedure based on ASHRAE Standard 62.1-2007
        IF (SameString(AlphArray(4),'VRP')) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'".')
          CALL ShowContinueError('Deprecated value in '//TRIM(cAlphaFields(4))//'="'//    &
              TRIM(AlphArray(4))//'", using VentilationRateProcedure.')
        ENDIF
        VentilationMechanical(VentMechNum)%SystemOAMethod = SOAM_VRP
      CASE ('IAQP','INDOORAIRQUALITYPROCEDURE')      ! Indoor Air Quality Procedure based on ASHRAE Standard 62.1-2007
        VentilationMechanical(VentMechNum)%SystemOAMethod = SOAM_IAQP
        IF (SameString(AlphArray(4),'IAQP')) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'".')
          CALL ShowContinueError('Deprecated value in '//TRIM(cAlphaFields(4))//'="'//    &
              TRIM(AlphArray(4))//'", using IndoorAirQualityProcedure.')
        ENDIF
        IF (.NOT. Contaminant%CO2Simulation) Then
          CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'" valid '//  &
             TRIM(cAlphaFields(2))//'="'//TRIM(AlphArray(2))//'" requires CO2 simulation.')
          CALL ShowContinueError('The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance')
          ErrorsFound=.TRUE.
        END IF
      CASE ('PROPORTIONALCONTROL')      ! Proportional Control based on ASHRAE Standard 62.1-2004
        VentilationMechanical(VentMechNum)%SystemOAMethod = SOAM_ProportionalControl
        IF (.NOT. Contaminant%CO2Simulation) Then
          CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'" valid '//  &
             TRIM(cAlphaFields(2))//'="'//TRIM(AlphArray(2))//'" requires CO2 simulation.')
          CALL ShowContinueError('The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance')
          ErrorsFound=.TRUE.
        END IF
      CASE ('INDOORAIRQUALITYPROCEDUREGENERICCONTAMINANT')   ! Indoor Air Quality Procedure based on generic contaminant setpoint
        VentilationMechanical(VentMechNum)%SystemOAMethod = SOAM_IAQPGC
        IF (.NOT. Contaminant%GenericContamSimulation) Then
          CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'" valid '//  &
             TRIM(cAlphaFields(2))//'="'//TRIM(AlphArray(2))//'" requires generic contaminant simulation.')
          CALL ShowContinueError('The choice must be Yes for the field Generic Contaminant Concentration in ' &
               //' ZoneAirContaminantBalance')
          ErrorsFound=.TRUE.
        END IF
      CASE ('INDOORAIRQUALITYPROCEDURECOMBINED') ! Indoor Air Quality Procedure based on both generic contaminant and CO2 setpoint
        VentilationMechanical(VentMechNum)%SystemOAMethod = SOAM_IAQPCOM
        IF (.NOT. Contaminant%GenericContamSimulation) Then
          CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'" valid '//  &
             TRIM(cAlphaFields(2))//'="'//TRIM(AlphArray(2))//'" requires generic contaminant simulation.')
          CALL ShowContinueError('The choice must be Yes for the field Generic Contaminant Concentration in ' &
               //' ZoneAirContaminantBalance')
          ErrorsFound=.TRUE.
        END IF
        IF (.NOT. Contaminant%CO2Simulation) Then
          CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'" valid '//  &
             TRIM(cAlphaFields(2))//'="'//TRIM(AlphArray(2))//'" requires CO2 simulation.')
          CALL ShowContinueError('The choice must be Yes for the field Carbon Dioxide Concentration in ZoneAirContaminantBalance')
          ErrorsFound=.TRUE.
        END IF
      CASE DEFAULT      ! If specified incorrectly, show errors
        VentilationMechanical(VentMechNum)%SystemOAMethod = SOAM_ZoneSum
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'" incorrect specification for '//  &
             TRIM(cAlphaFields(4))//', the ZoneSum method will be used.')
        !ErrorsFound=.TRUE.
    END SELECT

    !Zone maximum outdoor air fraction
    VentilationMechanical(VentMechNum)%ZoneMaxOAFraction = NumArray(1)

    ALLOCATE(VentMechZoneName(NumGroups))
    ALLOCATE(DesignSpecOAObjName(NumGroups))
    ALLOCATE(DesignSpecOAObjIndex(NumGroups))
    ALLOCATE(VentMechZoneOAAreaRate(NumGroups))
    ALLOCATE(VentMechZoneOAPeopleRate(NumGroups))
    ALLOCATE(VentMechZoneOAFlow(NumGroups))
    ALLOCATE(VentMechZoneOAACH(NumGroups))
    ALLOCATE(VentMechZoneADEffCooling(NumGroups))
    ALLOCATE(VentMechZoneADEffHeating(NumGroups))
    ALLOCATE(VentMechZoneADEffSchPtr(NumGroups))
    ALLOCATE(VentMechZoneADEffSchName(NumGroups))

    ALLOCATE(VentMechZoneSecondaryRecirculation(NumGroups))
    ALLOCATE(DesignSpecZoneADObjName(NumGroups))
    ALLOCATE(DesignSpecZoneADObjIndex(NumGroups))

    VentMechZoneName = ' '
    DesignSpecOAObjName = ' '
    DesignSpecOAObjIndex = 0
    VentMechZoneOAAreaRate = 0.0d0
    VentMechZoneOAPeopleRate = 0.0d0

    ! use defaults for Cooling and Heating Effectiveness
    VentMechZoneADEffCooling = 1.0d0
    VentMechZoneADEffHeating = 1.0d0
    VentMechZoneADEffSchPtr = 0
    VentMechZoneADEffSchName = ' '
    VentMechZoneSecondaryRecirculation = 0.0d0
    DesignSpecZoneADObjName = ' '
    DesignSpecZoneADObjIndex = 0

!   First time through find the total number of zones requiring mechanical ventilation
!   May include duplicate zones. Will check for duplicate zones further down in this subroutine.
    DO groupNum = 1, NumGroups
      VentMechZoneName(groupNum) = AlphArray((groupNum-1)*3+5)

      DO OutAirNum = 1, numOAControllers
          IF (OAController(OutAirNum)%VentilationMechanicalName == VentilationMechanical(VentMechNum)%Name .AND. &
                    VentilationMechanical(VentMechNum)%DCVFlag) THEN
              AirLoopControlInfo(OutAirNum)%AirLoopDCVFlag = .TRUE.
          ELSE
              AirLoopControlInfo(OutAirNum)%AirLoopDCVFlag = .FALSE.
          ENDIF
      END DO

!     Getting OA details from design specification OA object
      IF (.not. lAlphaBlanks((groupNum-1)*3+6)) THEN
        DesignSpecOAObjName(groupNum) = AlphArray((groupNum-1)*3+6)
        ObjIndex = FindItemInList(DesignSpecOAObjName(groupNum),OARequirements%Name,numOARequirements)
        DesignSpecOAObjIndex(groupNum) = ObjIndex

        IF (ObjIndex > 0) THEN
          VentMechZoneOAAreaRate(groupNum)   = OARequirements(ObjIndex)%OAFlowPerArea
          VentMechZoneOAPeopleRate(groupNum) = OARequirements(ObjIndex)%OAFlowPerPerson
          VentMechZoneOAFlow(groupNum)       = OARequirements(ObjIndex)%OAFlowPerZone
          VentMechZoneOAACH(groupNum)        = OARequirements(ObjIndex)%OAFlowACH
!push this check to later...
!          IF (VentilationMechanical(VentMechNum)%SystemOAMethod == SOAM_ProportionalControl) THEN
!            IF (VentMechZoneOAACH(groupNum) .GT. 0.d0 .OR. VentMechZoneOAFlow(groupNum) .GT. 0.d0) THEN
!              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' = "'//  &
!                                                TRIM(VentilationMechanical(VentMechNum)%Name))
!              CALL ShowContinueError('Inappropriate outdoor air method for '//TRIM(cAlphaFields((groupNum-1)*3+6))//  &
!                                     ' = "'//TRIM(DesignSpecOAObjName(groupNum))//'".')
!              CALL ShowContinueError('Since '//TRIM(cAlphaFields(4))//' = "'//TRIM(AlphArray(4))//'", '//  &
!                                     'AirChanges/Hour or Flow/Zone outdoor air methods are not valid. '// &
!                                     TRIM(AlphArray(4))//' will be modeled. Simulation continues.... ')
!            ENDIF
!          ENDIF
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
             trim(VentilationMechanical(VentMechNum)%Name)//'", invalid')
          CALL ShowContinueError('... not found '//trim(cAlphaFields((groupNum-1)*3+6))//'="'//  &
                  TRIM(DesignSpecOAObjName(groupNum))//'".')
          ErrorsFound = .TRUE.
        ENDIF
      ELSE
        ! check whether a design specification OA object is referenced by a Sizing:Zone object for the current zone
        !  otherwise generates an error
!        IF (DoZoneSizing) THEN
!          ObjIndex = FindItemInList(VentMechZoneName(groupNum),ZoneSizingInput%ZoneName,NumZoneSizingInput)
!          ObjIndex = ZoneSizingInput(ObjIndex)%ZoneDesignSpecOAIndex
!          IF (ObjIndex > 0) THEN
!            VentMechZoneOAAreaRate(groupNum)   = OARequirements(ObjIndex)%OAFlowPerArea
!            VentMechZoneOAPeopleRate(groupNum) = OARequirements(ObjIndex)%OAFlowPerPerson
!            VentMechZoneOAFlow(groupNum)       = OARequirements(ObjIndex)%OAFlowPerZone
!            VentMechZoneOAACH(groupNum)        = OARequirements(ObjIndex)%OAFlowACH
!          ELSE
!            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
!               trim(VentilationMechanical(VentMechNum)%Name)//'", missing')
!            CALL ShowContinueError('...blank (required entry)'//trim(cAlphaFields((groupNum-1)*3+6)))
!            ErrorsFound = .TRUE.
!          ENDIF
      ENDIF

!      IF (VentMechZoneOAPeopleRate(groupNum) <= 0.0d0 .AND. &
!                             VentilationMechanical(VentMechNum)%DCVFlag) THEN
!          CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//trim(TRIM(VentilationMechanical(VentMechNum)%Name))//  &
!             '", Zone OA/person rate')
!          CALL ShowContinueError('Zone outside air per person rate not set in Design '//  &
!             'Specification Outdoor Air Object="'// &
!             TRIM(DesignSpecOAObjName(groupNum))//'".')
!      ENDIf
!
!      IF (VentMechZoneOAAreaRate(groupNum) .LT. 0.0d0) THEN
!        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//  &
!           '" has invalid Outdoor Air flow per area specified in object "' &
!              // TRIM(OARequirements(DesignSpecOAObjIndex(groupNum))%Name) //'". Value must be >= 0.0.')
!        ErrorsFound = .TRUE.
!      END IF
!      IF (VentMechZoneOAPeopleRate(groupNum) .LT. 0.0d0) THEN
!        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//  &
!           '" has invalid Outdoor Air flow per person specified in object "' &
!              // TRIM(OARequirements(DesignSpecOAObjIndex(groupNum))%Name) //'". Value must be >= 0.0.')
!        ErrorsFound = .TRUE.
!      END IF
!
     ! Get zone air distribution details from design specification Zone Air Distribution object
      IF (.not. lAlphaBlanks((groupNum-1)*3+7)) THEN
        DesignSpecZoneADObjName(groupNum) = AlphArray((groupNum-1)*3+7)
        ObjIndex=FindItemInList(DesignSpecZoneADObjName(groupNum),ZoneAirDistribution%Name,numZoneAirDistribution)
        DesignSpecZoneADObjIndex(groupNum) = ObjIndex

        IF (ObjIndex > 0) THEN
          ! found the design specification Zone Air Distribution object
          VentMechZoneADEffCooling(groupNum) = ZoneAirDistribution(ObjIndex)%ZoneADEffCooling
          VentMechZoneADEffHeating(groupNum) = ZoneAirDistribution(ObjIndex)%ZoneADEffHeating
          VentMechZoneSecondaryRecirculation(groupNum) = ZoneAirDistribution(ObjIndex)%ZoneSecondaryRecirculation
          VentMechZoneADEffSchName(groupNum) = ZoneAirDistribution(ObjIndex)%ZoneADEffSchName
          VentMechZoneADEffSchPtr(groupNum) = GetScheduleIndex(VentMechZoneADEffSchName(groupNum))
        ELSE
          ! Cannot find the design specification Zone Air Distribution object
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
            TRIM(VentilationMechanical(VentMechNum)%Name)//'", invalid')
          CALL ShowContinueError('... not found '//trim(cAlphaFields((groupNum-1)*3+7))//'="'//  &
            TRIM(DesignSpecZoneADObjName(groupNum))//'".')
          ErrorsFound = .TRUE.
        ENDIF
!push check to later
!       ! Error check to see if a single duct air terminal is assigned to a zone that has zone secondary recirculation
!
!        IF (VentMechZoneSecondaryRecirculation(groupNum) > 0.0d0) THEN
!          ZoneNum = FindItemInList(VentMechZoneName(groupNum),Zone%Name,NumOfZones)
!          IF (ZoneNum > 0) THEN
!            EquipListIndex = ZoneEquipConfig(ZoneNum)%EquipListIndex
!            IF (EquipListIndex > 0) THEN
!              EquipLoop: DO EquipListNum = 1, NumofZoneEquipLists
!                IF (EquipListNum == EquipListIndex) THEN
!                  DO EquipNum = 1, ZoneEquipList(EquipListNum)%NumOfEquipTypes
!                    IF (SameString(ZoneEquipList(EquipListNum)%EquipType(EquipNum),'ZONEHVAC:AIRDISTRIBUTIONUNIT')) THEN
!                      DO ADUNum = 1, NumAirDistUnits
!                        IF (SameString(ZoneEquipList(EquipListNum)%EquipName(EquipNum),AirDistUnit(ADUNum)%Name)) THEN
!                          IF ((AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctVAVReheat) &
!                             .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctConstVolReheat) &
!                             .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctVAVNoReheat) &
!                             .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctVAVReheatVSFan) &
!                             .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctCBVAVReheat) &
!                             .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctCBVAVNoReheat) &
!                             .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctConstVolCooledBeam) &
!                             .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == DualDuctVAVOutdoorAir)) THEN
!                            CALL ShowWarningError(RoutineName//  &
!                                 'A zone secondary recirculation fraction is specified for zone served by ')
!                            CALL ShowContinueError('...terminal unit "'//TRIM(AirDistUnit(ADUNum)%Name)//  &
!                                 '" , that indicates a single path system')
!                            CALL ShowContinueError('...The zone secondary recirculation for that zone was set to 0.0')
!                            VentMechZoneSecondaryRecirculation(groupNum) = 0.0d0
!                          END IF
!                          Exit EquipLoop
!                        END IF
!                      END DO
!                    END IF
!                  END DO
!                END IF
!              END DO EquipLoop
!            END IF
!          END IF
!        END IF
      ELSE
        ! check whether a ZoneAirDistribution object is referenced by the Sizing:Zone object for the current zone
        ! If not, use defaults which are already set
!        IF (DoZoneSizing) THEN
!          ObjIndex = FindItemInList(VentMechZoneName(groupNum),ZoneSizingInput%ZoneName,NumZoneSizingInput)
!          ObjIndex = ZoneSizingInput(ObjIndex)%ZoneAirDistributionIndex
!          IF (ObjIndex > 0) THEN
!            VentMechZoneADEffCooling(groupNum) = ZoneAirDistribution(ObjIndex)%ZoneADEffCooling
!            VentMechZoneADEffHeating(groupNum) = ZoneAirDistribution(ObjIndex)%ZoneADEffHeating
!            VentMechZoneSecondaryRecirculation(groupNum) = ZoneAirDistribution(ObjIndex)%ZoneSecondaryRecirculation
!            VentMechZoneADEffSchName(groupNum) = ZoneAirDistribution(ObjIndex)%ZoneADEffSchName
!            VentMechZoneADEffSchPtr(groupNum) = GetScheduleIndex(VentMechZoneADEffSchName(groupNum))
!          ENDIF
!        ENDIF
      ENDIF

      ZoneNum = FindItemInList(VentMechZoneName(groupNum),Zone%Name,NumOfZones)
      IF(ZoneNum .GT. 0)THEN
         MechVentZoneCount = MechVentZoneCount + 1
      ELSE
        ZoneListNum = FindItemInList(VentMechZoneName(groupNum),ZoneList%Name,NumOfZoneLists)
        IF(ZoneListNum .GT. 0)THEN
            MechVentZoneCount = MechVentZoneCount + ZoneList(ZoneListNum)%NumofZones
        ELSE
          CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//trim(AlphArray(1))//'" invalid '//  &
               TRIM(cAlphaFields((groupNum-1)*3+5))//' not found.')
          CALL ShowContinueError('Missing '//TRIM(cAlphaFields((groupNum-1)*3+5))//' = '&
                                 //TRIM(VentMechZoneName(groupNum)))
          ErrorsFound = .TRUE.
        END IF
      END IF
    END DO

    VentilationMechanical(VentMechNum)%NumofVentMechZones = MechVentZoneCount

!   Now allocate and store unique zone and associated ventilation rate information
    ALLOCATE(VentilationMechanical(VentMechNum)%Zone(MechVentZoneCount))
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(MechVentZoneCount))
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(MechVentZoneCount))
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneOAAreaRate(MechVentZoneCount))
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneOAPeopleRate(MechVentZoneCount))
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneOAFlow(MechVentZoneCount))
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneOAACH(MechVentZoneCount))
    VentilationMechanical(VentMechNum)%Zone = 0
    VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName = ' '
    VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex = 0
    VentilationMechanical(VentMechNum)%ZoneOAAreaRate = 0.0d0
    VentilationMechanical(VentMechNum)%ZoneOAPeopleRate = 0.0d0
    VentilationMechanical(VentMechNum)%ZoneOAFlow = 0.0d0
    VentilationMechanical(VentMechNum)%ZoneOAACH = 0.0d0

    ! added for new DCV, 2/12/2009
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneADEffCooling(MechVentZoneCount))
    ! Zone air distribution effectiveness in heating mode
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneADEffHeating(MechVentZoneCount))
    ! Indices to the zone air distribution effectiveness schedules
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneADEffSchPtr(MechVentZoneCount))
    ! Zone air distribution effectiveness schedule names
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneADEffSchName(MechVentZoneCount))
    ! Zone air secondary recirculation ratio, added 3/2012
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneSecondaryRecirculation(MechVentZoneCount))
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName(MechVentZoneCount))
    ALLOCATE(VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjIndex(MechVentZoneCount))

    VentilationMechanical(VentMechNum)%ZoneADEffCooling = 1.0d0
    VentilationMechanical(VentMechNum)%ZoneADEffHeating = 1.0d0
    VentilationMechanical(VentMechNum)%ZoneADEffSchPtr = 0
    VentilationMechanical(VentMechNum)%ZoneADEffSchName = ' '
    VentilationMechanical(VentMechNum)%ZoneSecondaryRecirculation = 0.0d0
    VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName = ' '
    VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjIndex = 0

    MechVentZoneCount = 0

!   Loop through zone names and list of zone names and store data
    DO groupNum = 1, NumGroups
      ZoneNum = FindItemInList(VentMechZoneName(groupNum),Zone%Name,NumOfZones)
      IF (ZoneNum > 0) THEN
        IF (ANY (VentilationMechanical(VentMechNum)%Zone == ZoneNum)) THEN
!          Disregard duplicate zone names, show warning and do not store data for this zone
           CALL ShowWarningError('Zone name = '//TRIM(VentMechZoneName(groupNum))// &
           ' for '//TRIM(CurrentModuleObject)//' object = '//TRIM(VentilationMechanical(VentMechNum)%Name))
           CALL ShowContinueError('is specified more than once. The first ventilation values specified for this zone will be used')
           CALL ShowContinueError('and the rest will be ignored. Simulation will continue..')
        ELSE
!          Store unique zone names
           MechVentZoneCount = MechVentZoneCount + 1
           VentilationMechanical(VentMechNum)%Zone(MechVentZoneCount) = ZoneNum
           ! Populating new temp array to hold design spec OA object for each zone
           IF (DesignSpecOAObjName(groupNum) /= blank) THEN
             VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(MechVentZoneCount) = &
                               DesignSpecOAObjName(groupNum)
             VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(MechVentZoneCount) = &
                               DesignSpecOAObjIndex(groupNum)
             VentilationMechanical(VentMechNum)%ZoneOAAreaRate(MechVentZoneCount) = &
                                VentMechZoneOAAreaRate(groupNum)
             VentilationMechanical(VentMechNum)%ZoneOAPeopleRate(MechVentZoneCount) = &
                                VentMechZoneOAPeopleRate(groupNum)
             VentilationMechanical(VentMechNum)%ZoneOAFlow(MechVentZoneCount) =  &
                                VentMechZoneOAFlow(groupNum)
             VentilationMechanical(VentMechNum)%ZoneOAACH = &
                                VentMechZoneOAACH(groupNum)
           ELSE
             IF (DoZoneSizing) THEN
               ObjIndex =   &
                   FindItemInList(VentMechZoneName(groupNum),ZoneSizingInput%ZoneName,NumZoneSizingInput)
               IF (ObjIndex > 0) THEN
                 VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(MechVentZoneCount) = &
                                ZoneSizingInput(ObjIndex)%DesignSpecOAObjName
                 VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(MechVentZoneCount) = &
                                ZoneSizingInput(ObjIndex)%ZoneDesignSpecOAIndex
                 ObjIndex = VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(MechVentZoneCount)
                 IF (ObjIndex > 0) THEN
                   VentilationMechanical(VentMechNum)%ZoneOAAreaRate(MechVentZoneCount) =  &
                      OARequirements(ObjIndex)%OAFlowPerArea
                   VentilationMechanical(VentMechNum)%ZoneOAPeopleRate(MechVentZoneCount) = &
                      OARequirements(ObjIndex)%OAFlowPerPerson
                   VentilationMechanical(VentMechNum)%ZoneOAFlow(MechVentZoneCount) =  &
                      OARequirements(ObjIndex)%OAFlowPerZone
                   VentilationMechanical(VentMechNum)%ZoneOAACH = &
                      OARequirements(ObjIndex)%OAFlowACH
                 ELSE  ! use defaults
                   VentilationMechanical(VentMechNum)%ZoneOAAreaRate(MechVentZoneCount) = 0.0d0
                   VentilationMechanical(VentMechNum)%ZoneOAPeopleRate(MechVentZoneCount) = 0.00944d0
                   VentilationMechanical(VentMechNum)%ZoneOAFlow(MechVentZoneCount) = 0.0d0
                   VentilationMechanical(VentMechNum)%ZoneOAACH = 0.0d0
                 ENDIF
               ELSE
                 CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
                             trim(VentilationMechanical(VentMechNum)%Name)//'", missing')
                 CALL ShowContinueError('...blank (required entry) - cannot locate in Sizing:Zone for Zone="'//  &
                    trim(VentMechZoneName(groupNum))//'".')
                 ErrorsFound = .TRUE.
               ENDIF
             ENDIF
           ENDIF
!!!! Zone Air Distribution inputs.
           IF (DesignSpecZoneADObjName(groupNum) /= blank) THEN
             ! new DCV inputs
             VentilationMechanical(VentMechNum)%ZoneADEffCooling(MechVentZoneCount) = &
               VentMechZoneADEffCooling(groupNum)
             VentilationMechanical(VentMechNum)%ZoneADEffHeating(MechVentZoneCount) = &
               VentMechZoneADEffHeating(groupNum)
             VentilationMechanical(VentMechNum)%ZoneADEffSchPtr(MechVentZoneCount) = &
               VentMechZoneADEffSchPtr(groupNum)
             VentilationMechanical(VentMechNum)%ZoneADEffSchName(MechVentZoneCount) = &
               VentMechZoneADEffSchName(groupNum)
             VentilationMechanical(VentMechNum)%ZoneSecondaryRecirculation(MechVentZoneCount) = &
               VentMechZoneSecondaryRecirculation(groupNum)
             VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName(MechVentZoneCount) = &
               DesignSpecZoneADObjName(groupNum)
             VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjIndex(MechVentZoneCount) = &
               DesignSpecZoneADObjIndex(groupNum)
          ELSE
            IF (DoZoneSizing) THEN
              ObjIndex =   &
                  FindItemInList(VentMechZoneName(groupNum),ZoneSizingInput%ZoneName,NumZoneSizingInput)
              IF (ObjIndex > 0) THEN
                VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName(MechVentZoneCount) = &
                              ZoneSizingInput(ObjIndex)%ZoneAirDistEffObjName
                VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjIndex(MechVentZoneCount) = &
                              ZoneSizingInput(ObjIndex)%ZoneAirDistributionIndex
                ObjIndex = VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjIndex(MechVentZoneCount)
              ELSE
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
                           trim(VentilationMechanical(VentMechNum)%Name)//'", missing')
                CALL ShowContinueError('...blank (required entry) - cannot locate in Sizing:Zone for Zone="'//  &
                   trim(VentMechZoneName(groupNum))//'".')
                ErrorsFound = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ELSE
!       Not a zone name, must be a zone list
        ZoneListNum = FindItemInList(VentMechZoneName(groupNum),ZoneList%Name,NumOfZoneLists)
        IF(ZoneListNum .GT. 0)THEN
          DO ScanZoneListNum = 1, ZoneList(ZoneListNum)%NumofZones
         ! check to make sure zone name is unique (not listed more than once)...
            ZoneNum = ZoneList(ZoneListNum)%Zone(ScanZoneListNum)
            IF (ANY(VentilationMechanical(VentMechNum)%Zone == ZoneNum)) THEN
!             Disregard duplicate zone names, show warning and do not store data for this zone
              CALL ShowWarningError('Zone name = '//TRIM(Zone(ZoneNum)%Name)// &
                                ' in ZoneList = '//TRIM(VentMechZoneName(groupNum))// &
                                ' for '//TRIM(CurrentModuleObject)//' object = '//TRIM(VentilationMechanical(VentMechNum)%Name))
              CALL ShowContinueError('is a duplicate. The first ventilation values specified for this zone will be used ')
              CALL ShowContinueError('and the rest will be ignored. The simulation will continue...')
            ELSE
!           Store data for each zone name from zone list (duplicate zone names accounted for in HeatBalanceManager)
              MechVentZoneCount = MechVentZoneCount + 1
              VentilationMechanical(VentMechNum)%Zone(MechVentZoneCount) = ZoneNum
              ! Populating new temp array to hold design spec OA object for each zone
              IF (DesignSpecOAObjName(groupNum) /= blank) THEN
                VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(MechVentZoneCount) = &
                               DesignSpecOAObjName(groupNum)
                VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(MechVentZoneCount) = &
                               DesignSpecOAObjIndex(groupNum)
                ObjIndex = VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(MechVentZoneCount)
              ELSE
                IF (DoZoneSizing) THEN
                  ObjIndex =   &
                     FindItemInList(Zone(ZoneList(ZoneListNum)%Zone(ScanZoneListNum))%Name,  &
                        ZoneSizingInput%ZoneName,NumZoneSizingInput)
                  IF (ObjIndex > 0) THEN
                    VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(MechVentZoneCount) = &
                                   ZoneSizingInput(ObjIndex)%DesignSpecOAObjName
                    VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(MechVentZoneCount) = &
                                   ZoneSizingInput(ObjIndex)%ZoneDesignSpecOAIndex
                    ObjIndex = VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(MechVentZoneCount)
                    IF (ObjIndex > 0) THEN
                      VentilationMechanical(VentMechNum)%ZoneADEffCooling(MechVentZoneCount) = &
                        ZoneAirDistribution(ObjIndex)%ZoneADEffCooling
                      VentilationMechanical(VentMechNum)%ZoneADEffHeating(MechVentZoneCount) = &
                        ZoneAirDistribution(ObjIndex)%ZoneADEffHeating
                      VentilationMechanical(VentMechNum)%ZoneADEffSchPtr(MechVentZoneCount) = &
                        ZoneAirDistribution(ObjIndex)%ZoneADEffSchPtr
                      VentilationMechanical(VentMechNum)%ZoneADEffSchName(MechVentZoneCount) = &
                        ZoneAirDistribution(ObjIndex)%ZoneADEffSchName
                      VentilationMechanical(VentMechNum)%ZoneSecondaryRecirculation(MechVentZoneCount) = &
                        ZoneAirDistribution(ObjIndex)%ZoneSecondaryRecirculation
                    ENDIF
                  ELSE
                    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
                                trim(VentilationMechanical(VentMechNum)%Name)//'", missing')
                    CALL ShowContinueError('...blank (required entry) - cannot locate in Sizing:Zone for Zone="'//  &
                       trim(Zone(ZoneList(ZoneListNum)%Zone(ScanZoneListNum))%Name)//'".')
                    ErrorsFound = .TRUE.
                  ENDIF
                ENDIF
              ENDIF
              IF (ObjIndex > 0) THEN
                VentilationMechanical(VentMechNum)%ZoneOAAreaRate(MechVentZoneCount) =   &
                  OARequirements(ObjIndex)%OAFlowPerArea
                VentilationMechanical(VentMechNum)%ZoneOAPeopleRate(MechVentZoneCount) =   &
                 OARequirements(ObjIndex)%OAFlowPerPerson
                VentilationMechanical(VentMechNum)%ZoneOAFlow(MechVentZoneCount) =   &
                 OARequirements(ObjIndex)%OAFlowPerZone
                VentilationMechanical(VentMechNum)%ZoneOAACH(MechVentZoneCount) =   &
                 OARequirements(ObjIndex)%OAFlowACH
!            ELSE
!              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
!                 trim(VentilationMechanical(VentMechNum)%Name)//'", invalid')
!              CALL ShowContinueError('... not found '//trim(cAlphaFields((groupNum-1)*3+6))//'="'//  &
!                      TRIM(VentilationMechanical(VentMechNum)%DesignSpecOAObjName(MechVentZoneCount))//'".')
!              ErrorsFound = .TRUE.
              ENDIF

              IF (VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName(MechVentZoneCount) /= blank) THEN
                ! new DCV inputs
                VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName(MechVentZoneCount) = &
                  DesignSpecZoneADObjName(groupNum)
                VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjIndex(MechVentZoneCount) = &
                  DesignSpecZoneADObjIndex(groupNum)
                ObjIndex = VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjIndex(MechVentZoneCount)
              ELSE
                IF (DoZoneSizing) THEN
                  ObjIndex =   &
                     FindItemInList(Zone(ZoneList(ZoneListNum)%Zone(ScanZoneListNum))%Name,  &
                                  ZoneSizingInput%ZoneName,NumZoneSizingInput)
                  IF (ObjIndex > 0) THEN
                    VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName(MechVentZoneCount) = &
                                   ZoneSizingInput(ObjIndex)%ZoneAirDistEffObjName
                    VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjIndex(MechVentZoneCount) = &
                                   ZoneSizingInput(ObjIndex)%ZoneAirDistributionIndex
                    ObjIndex = VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjIndex(MechVentZoneCount)
                  ELSE
                    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
                                trim(VentilationMechanical(VentMechNum)%Name)//'", missing')
                    CALL ShowContinueError('...blank (required entry) - cannot locate in Sizing:Zone for Zone="'//  &
                       trim(Zone(ZoneList(ZoneListNum)%Zone(ScanZoneListNum))%Name)//'".')
                    ErrorsFound = .TRUE.
                  ENDIF
                ENDIF
              ENDIF
              IF (ObjIndex > 0) THEN
                VentilationMechanical(VentMechNum)%ZoneADEffCooling(MechVentZoneCount) = &
                  ZoneAirDistribution(ObjIndex)%ZoneADEffCooling
                VentilationMechanical(VentMechNum)%ZoneADEffHeating(MechVentZoneCount) = &
                  ZoneAirDistribution(ObjIndex)%ZoneADEffHeating
                VentilationMechanical(VentMechNum)%ZoneADEffSchPtr(MechVentZoneCount) = &
                  ZoneAirDistribution(ObjIndex)%ZoneADEffSchPtr
                VentilationMechanical(VentMechNum)%ZoneADEffSchName(MechVentZoneCount) = &
                  ZoneAirDistribution(ObjIndex)%ZoneADEffSchName
                VentilationMechanical(VentMechNum)%ZoneSecondaryRecirculation(MechVentZoneCount) = &
                  ZoneAirDistribution(ObjIndex)%ZoneSecondaryRecirculation
              ENDIF
            ENDIF
          END DO
        END IF
      END IF
    END DO

!   Overwrite previous number of zones with number that does not include duplicates
    VentilationMechanical(VentMechNum)%NumofVentMechZones = MechVentZoneCount


!moved to after section in initialization where other zones are weeded out.
!    !predefined report
!    DO jZone = 1, VentilationMechanical(VentMechNum)%NumofVentMechZones
!      zoneName = zone(VentilationMechanical(VentMechNum)%Zone(jZone))%name
!      CALL PreDefTableEntry(pdchDCVventMechName,zoneName,VentilationMechanical(VentMechNum)%Name)
!      CALL PreDefTableEntry(pdchDCVperPerson,zoneName, VentilationMechanical(VentMechNum)%ZoneOAPeopleRate(jZone),6)
!      CALL PreDefTableEntry(pdchDCVperArea,zoneName, VentilationMechanical(VentMechNum)%ZoneOAAreaRate(jZone),6)
!
!      ! added for new DCV inputs
!      CALL PreDefTableEntry(pdchDCVZoneADEffCooling,zoneName, VentilationMechanical(VentMechNum)%ZoneADEffCooling(jZone),2)
!      CALL PreDefTableEntry(pdchDCVZoneADEffHeating,zoneName, VentilationMechanical(VentMechNum)%ZoneADEffHeating(jZone),2)
!      CALL PreDefTableEntry(pdchDCVZoneADEffSchName,zoneName,   &
!         GetScheduleName(VentilationMechanical(VentMechNum)%ZoneADEffSchPtr(jZone)))
!    END DO

  DEALLOCATE(VentMechZoneName)
  DEALLOCATE(DesignSpecOAObjName)
  DEALLOCATE(DesignSpecOAObjIndex)
  DEALLOCATE(VentMechZoneOAAreaRate)
  DEALLOCATE(VentMechZoneOAPeopleRate)
  DEALLOCATE(VentMechZoneADEffCooling)
  DEALLOCATE(VentMechZoneADEffHeating)
  DEALLOCATE(VentMechZoneADEffSchPtr)
  DEALLOCATE(VentMechZoneADEffSchName)
  DEALLOCATE(VentMechZoneOAFlow)
  DEALLOCATE(VentMechZoneOAACH)

  DEALLOCATE(DesignSpecZoneADObjName)
  DEALLOCATE(DesignSpecZoneADObjIndex)
  DEALLOCATE(VentMechZoneSecondaryRecirculation)

  END DO

  DO VentMechNum=1,NumVentMechControllers
    DO jZone=1,VentilationMechanical(VentMechNum)%NumofVentMechZones
      IF (VentilationMechanical(VentMechNum)%SystemOAMethod == SOAM_ProportionalControl) THEN
        IF (VentilationMechanical(VentMechNum)%ZoneOAACH(jZone) .GT. 0.d0 .OR.   &
            VentilationMechanical(VentMechNum)%ZoneOAFlow(jZone) .GT. 0.d0) THEN
          CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(VentilationMechanical(VentMechNum)%Name)//  &
             '", inappropriate outdoor air method')
          CALL ShowContinueError('Inappropriate method for Design Specification Outdoor Air Object Name="'// &
                                 TRIM(VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(jZone))//'".')
          CALL ShowContinueError('For Zone="'//trim(Zone(VentilationMechanical(VentMechNum)%Zone(jZone))%Name)//'".')
          CALL ShowContinueError('Since System Outdoor Air Method="ProportionalControl", '//  &
                                 'AirChanges/Hour or Flow/Zone outdoor air methods are not valid. Simulation continues.... ')
        ENDIF
      ENDIF

     ! Error check to see if a single duct air terminal is assigned to a zone that has zone secondary recirculation
      IF (VentilationMechanical(VentMechNum)%ZoneSecondaryRecirculation(jZone) > 0.0d0) THEN
        ZoneNum = VentilationMechanical(VentMechNum)%Zone(jZone)
        IF (ZoneNum > 0) THEN
          EquipListIndex = ZoneEquipConfig(ZoneNum)%EquipListIndex
          IF (EquipListIndex > 0) THEN
            EquipLoop: DO EquipListNum = 1, NumofZoneEquipLists
              IF (EquipListNum == EquipListIndex) THEN
                DO EquipNum = 1, ZoneEquipList(EquipListNum)%NumOfEquipTypes
                  IF (SameString(ZoneEquipList(EquipListNum)%EquipType(EquipNum),'ZONEHVAC:AIRDISTRIBUTIONUNIT')) THEN
                    DO ADUNum = 1, NumAirDistUnits
                      IF (SameString(ZoneEquipList(EquipListNum)%EquipName(EquipNum),AirDistUnit(ADUNum)%Name)) THEN
                        IF ((AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctVAVReheat) &
                           .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctConstVolReheat) &
                           .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctVAVNoReheat) &
                           .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctVAVReheatVSFan) &
                           .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctCBVAVReheat) &
                           .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctCBVAVNoReheat) &
                           .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == SingleDuctConstVolCooledBeam) &
                           .OR. (AirDistUnit(ADUNum)%EquipType_Num(EquipNum) == DualDuctVAVOutdoorAir)) THEN
                          CALL ShowWarningError(trim(CurrentModuleObject)//'="'//  &
                             trim(VentilationMechanical(VentMechNum)%Name)//'", inappropriate use of Zone secondary recirculation')
                          CALL ShowContinueError('A zone secondary recirculation fraction is specified for zone served by ')
                          CALL ShowContinueError('...terminal unit "'//TRIM(AirDistUnit(ADUNum)%Name)//  &
                               '" , that indicates a single path system')
                          CALL ShowContinueError('For Zone="'//  &
                             trim(Zone(VentilationMechanical(VentMechNum)%Zone(jZone))%Name)//'".')
                          CALL ShowContinueError('...The zone secondary recirculation for that zone was set to 0.0')
                          VentilationMechanical(VentMechNum)%ZoneSecondaryRecirculation(jZone) = 0.0d0
                        END IF
                        Exit EquipLoop
                      END IF
                    END DO
                  END IF
                END DO
              END IF
            END DO EquipLoop
          END IF
        END IF
      END IF
      IF (VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(jZone) == blank) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(VentilationMechanical(VentMechNum)%Name)//  &
           '", Design Specification Outdoor Air Object Name blank')
        CALL ShowContinueError('For Zone="'//trim(Zone(VentilationMechanical(VentMechNum)%Zone(jZone))%Name)//'".')
        CALL ShowContinueError('This field either needs to be filled in in this object or Sizing:Zone object.')
        CALL ShowContinueError('For this run, default values for these fields will be used.')
!        ErrorsFound=.true.
      ENDIF
!      IF (VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName(jZone) == blank) THEN
!        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(VentilationMechanical(VentMechNum)%Name)//  &
!           '", Design Specification Zone Air Distribution Object Name blank')
!        CALL ShowContinueError('For Zone="'//trim(Zone(VentilationMechanical(VentMechNum)%Zone(jZone))%Name)//'".')
!        CALL ShowContinueError('This field either needs to be filled in in this object or Sizing:Zone object.')
!        ErrorsFound=.true.
!      ENDIF
      IF (VentilationMechanical(VentMechNum)%ZoneOAPeopleRate(jZone) <= 0.0d0 .AND.   &
                                            VentilationMechanical(VentMechNum)%DCVFlag) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//trim(VentilationMechanical(VentMechNum)%Name)//  &
           '", Zone OA/person rate')
        CALL ShowContinueError('For Zone="'//trim(Zone(VentilationMechanical(VentMechNum)%Zone(jZone))%Name)//'".')
        CALL ShowContinueError('Zone outside air per person rate not set in Design '//  &
           'Specification Outdoor Air Object="'// &
           TRIM(VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(jZone))//'".')
      ENDIf

      IF (VentilationMechanical(VentMechNum)%ZoneOAAreaRate(jZone) .LT. 0.0d0) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(VentilationMechanical(VentMechNum)%Name)//  &
           '", invalid Outdoor Air flow per area')
        CALL ShowContinueError('For Zone="'//trim(Zone(VentilationMechanical(VentMechNum)%Zone(jZone))%Name)//'".')
        CALL ShowContinueError('invalid Outdoor Air flow per area specified in object="'// &
           TRIM(OARequirements(VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(jZone))%Name) //  &
            '". Value must be >= 0.0.')
        ErrorsFound = .TRUE.
      END IF
      IF (VentilationMechanical(VentMechNum)%ZoneOAPeopleRate(jZone) .LT. 0.0d0) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(VentilationMechanical(VentMechNum)%Name)//  &
           '", invalid Outdoor Air flow per person')
        CALL ShowContinueError('For Zone="'//trim(Zone(VentilationMechanical(VentMechNum)%Zone(jZone))%Name)//'".')
        CALL ShowContinueError('invalid Outdoor Air flow per person specified in object "'// &
           TRIM(OARequirements(VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjIndex(jZone))%Name) //  &
           '". Value must be >= 0.0.')
        ErrorsFound = .TRUE.
      END IF
    ENDDO
  ENDDO

! Link OA controller object with mechanical ventilation object
  DO OAControllerNum=1,NumOAControllers
    OAController(OAControllerNum)%VentMechObjectNum = &
       FindItemInList(OAController(OAControllerNum)%VentilationMechanicalName,VentilationMechanical%Name,NumVentMechControllers)
    IF(OAController(OAControllerNum)%VentMechObjectNum .EQ. 0 .AND. &
      OAController(OAControllerNum)%VentilationMechanicalName .NE. Blank) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(OAController(OAControllerNum)%VentilationMechanicalName)//  &
             '", non-match to Controller:OutdoorAir')
      CALL ShowContinueError('Invalid specified in Controller:OutdoorAir object = '//TRIM(OAController(OAControllerNum)%Name))
      CALL ShowContinueError(TRIM(CurrentModuleObject)//' object name must match the '//TRIM(CurrentModuleObject)// &
                             ' object name specified in Controller:OutdoorAir.')
      ErrorsFound = .TRUE.
    END IF
  END DO

! write to .eio file
  Write(OutputFileInits,700)
 700 Format('!<Controller:MechanicalVentilation>,Name,Availability Schedule Name,Demand Controlled Ventilation {Yes/No},', &
      'System Outdoor Air Method,Zone Maximum Outdoor Air Fraction,Number of Zones,Zone Name,DSOA Name,DSZAD Name')
  DO VentMechNum=1,NumVentMechControllers
    Write(OutputFileInits,'(A)',ADVANCE='NO') ' Controller:MechanicalVentilation,'//  &
       trim(VentilationMechanical(VentMechNum)%Name)//  &
       ','//trim(VentilationMechanical(VentMechNum)%SchName)//','
    IF (VentilationMechanical(VentMechNum)%DCVFlag) THEN
      Write(OutputFileInits,'(A)',ADVANCE='NO') 'Yes,'
    ELSE
      Write(OutputFileInits,'(A)',ADVANCE='NO') 'No,'
    ENDIF
    IF (VentilationMechanical(VentMechNum)%SystemOAMethod == SOAM_ZoneSum) THEN
      Write(OutputFileInits,'(A)',ADVANCE='NO') 'ZoneSum,'
    ELSEIF (VentilationMechanical(VentMechNum)%SystemOAMethod == SOAM_VRP) THEN
      Write(OutputFileInits,'(A)',ADVANCE='NO') 'VentilationRateProcedure,'
    ELSEIF (VentilationMechanical(VentMechNum)%SystemOAMethod == SOAM_IAQP) THEN
      Write(OutputFileInits,'(A)',ADVANCE='NO') 'IndoorAirQualityProcedure,'
    ELSEIF (VentilationMechanical(VentMechNum)%SystemOAMethod == SOAM_ProportionalControl) THEN
      Write(OutputFileInits,'(A)',ADVANCE='NO') 'ProportionalControl,'
    ELSEIF (VentilationMechanical(VentMechNum)%SystemOAMethod == SOAM_IAQPGC) THEN
      Write(OutputFileInits,'(A)',ADVANCE='NO') 'IndoorAirQualityGenericContaminant,'
    ELSEIF (VentilationMechanical(VentMechNum)%SystemOAMethod == SOAM_IAQPCOM) THEN
      Write(OutputFileInits,'(A)',ADVANCE='NO') 'IndoorAirQualityProcedureCombined,'
    ELSE
      Write(OutputFileInits,'(A)',ADVANCE='NO') 'Invalid/Unknown,'
    ENDIF
    Write(OutputFileInits,'(A)',ADVANCE='NO') trim(RoundSigDigits(VentilationMechanical(VentMechNum)%ZoneMaxOAFraction,2))//','
    Write(OutputFileInits,'(A)',ADVANCE='NO') trim(RoundSigDigits(VentilationMechanical(VentMechNum)%NumofVentMechZones))//','
    DO jZone=1,VentilationMechanical(VentMechNum)%NumofVentMechZones
      IF (jZone < VentilationMechanical(VentMechNum)%NumofVentMechZones) THEN
        Write(OutputFileInits,'(A)',ADVANCE='NO') trim(Zone(VentilationMechanical(VentMechNum)%Zone(jZone))%Name)//','//  &
           trim(VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(jZone))//','//  &
           trim(VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName(jZone))//','
      ELSE
        Write(OutputFileInits,'(A)') trim(Zone(VentilationMechanical(VentMechNum)%Zone(jZone))%Name)//','//  &
           trim(VentilationMechanical(VentMechNum)%ZoneDesignSpecOAObjName(jZone))//','//  &
           trim(VentilationMechanical(VentMechNum)%ZoneDesignSpecADObjName(jZone))
      ENDIF
    ENDDO
  ENDDO

END IF ! Number of Mechanical Ventilation Objects > 0

DEALLOCATE(AlphArray)
DEALLOCATE(NumArray)
DEALLOCATE(lNumericBlanks)
DEALLOCATE(lAlphaBlanks)
DEALLOCATE(cAlphaFields)
DEALLOCATE(cNumericFields)

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found when getting '//TRIM(CurrentModuleObject)//' inputs.')
END IF

RETURN
END SUBROUTINE GetOAControllerInputs

SUBROUTINE GetOAMixerInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Input the OAMixer data and store it in the OAMixer array.

          ! METHODOLOGY EMPLOYED:
          ! Use the Get routines from the InputProcessor module.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor
    USE NodeInputManager, ONLY: GetOnlySingleNode
    USE BranchNodeConnections, ONLY: TestCompSet

    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetOAMixerInputs: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: NumNums   ! Number of REAL(r64) numbers returned by GetObjectItem
INTEGER :: NumAlphas ! Number of alphanumerics returned by GetObjectItem
INTEGER :: NumArg    ! Number of arguments from GetObjectDefMaxArgs call
INTEGER :: OutAirNum
INTEGER :: IOSTAT
REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray ! array that holds numeric input values
CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray ! array that holds alpha input values
CHARACTER(len=MaxNameLength) :: CurrentModuleObject   ! Object type for getting and messages
CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
LOGICAL,  ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks     ! Logical array, alpha field input BLANK = .true.
LOGICAL,  ALLOCATABLE, DIMENSION(:) :: lNumericBlanks   ! Logical array, numeric field input BLANK = .true.
LOGICAL       :: ErrorsFound=.false.
LOGICAL       :: IsNotOK               ! Flag to verify name
LOGICAL       :: IsBlank               ! Flag for blank name

IF (.not. GetOAMixerInputFlag) RETURN

CALL GetObjectDefMaxArgs(CurrentModuleObjects(CMO_OAMixer),NumArg,NumAlphas,NumNums)

ALLOCATE(AlphArray(NumAlphas))
AlphArray=' '
ALLOCATE(NumArray(NumNums))
NumArray=0.0d0
ALLOCATE(lNumericBlanks(NumNums))
lNumericBlanks=.true.
ALLOCATE(lAlphaBlanks(NumAlphas))
lAlphaBlanks=.true.
ALLOCATE(cAlphaFields(NumAlphas))
cAlphaFields = ' '
ALLOCATE(cNumericFields(NumNums))
cNumericFields = ' '

CurrentModuleObject = CurrentModuleObjects(CMO_OAMixer)

NumOAMixers = GetNumObjectsFound(CurrentModuleObject)

IF (NumOAMixers.GT.0) THEN

  ALLOCATE(OAMixer(NumOAMixers))

  DO OutAirNum=1,NumOAMixers
    CALL GetObjectItem(CurrentModuleObject,OutAirNum,AlphArray,NumAlphas,&
                       NumArray,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),OAMixer%Name,OutAirNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF
    OAMixer(OutAirNum)%Name = AlphArray(1)
    OAMixer(OutAirNum)%MixNode  = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    OAMixer(OutAirNum)%InletNode   = &
           !  Set connection type to 'Inlet', because this is not necessarily directly from
           !  outside air.  Outside Air Inlet Node List will set the connection to outside air
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    OAMixer(OutAirNum)%RelNode  = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_ReliefAir,1,ObjectIsNotParent)
    OAMixer(OutAirNum)%RetNode   = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    ! Check for dupes in the four nodes.
    IF (OAMixer(OutAirNum)%MixNode == OAMixer(OutAirNum)%InletNode) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(OAMixer(OutAirNum)%Name)//  &
                           ' '//TRIM(cAlphaFields(3))//' = '//TRIM(NodeID(OAMixer(OutAirNum)%InletNode))// &
                           ' duplicates the '//TRIM(cAlphaFields(2))//'.')
      ErrorsFound=.true.
    ELSEIF (OAMixer(OutAirNum)%MixNode == OAMixer(OutAirNum)%RelNode) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(OAMixer(OutAirNum)%Name)//  &
                           ' '//TRIM(cAlphaFields(4))//' = '//TRIM(NodeID(OAMixer(OutAirNum)%RelNode))// &
                           ' duplicates the '//TRIM(cAlphaFields(2))//'.')
      ErrorsFound=.true.
    ELSEIF (OAMixer(OutAirNum)%MixNode == OAMixer(OutAirNum)%RetNode) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(OAMixer(OutAirNum)%Name)//  &
                           ' '//TRIM(cAlphaFields(5))//' = '//TRIM(NodeID(OAMixer(OutAirNum)%RetNode))// &
                           ' duplicates the '//TRIM(cAlphaFields(2))//'.')
      ErrorsFound=.true.
    ENDIF

    IF (OAMixer(OutAirNum)%InletNode == OAMixer(OutAirNum)%RelNode) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(OAMixer(OutAirNum)%Name)//  &
                           ' '//TRIM(cAlphaFields(4))//' = '//TRIM(NodeID(OAMixer(OutAirNum)%RelNode))// &
                           ' duplicates the '//TRIM(cAlphaFields(3))//'.')
      ErrorsFound=.true.
    ELSEIF (OAMixer(OutAirNum)%InletNode == OAMixer(OutAirNum)%RetNode) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(OAMixer(OutAirNum)%Name)//  &
                           ' '//TRIM(cAlphaFields(5))//' = '//TRIM(NodeID(OAMixer(OutAirNum)%RetNode))// &
                           ' duplicates the '//TRIM(cAlphaFields(3))//'.')
      ErrorsFound=.true.
    ENDIF

    IF (OAMixer(OutAirNum)%RelNode == OAMixer(OutAirNum)%RetNode) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(OAMixer(OutAirNum)%Name)//  &
                           ' '//TRIM(cAlphaFields(5))//' = '//TRIM(NodeID(OAMixer(OutAirNum)%RetNode))// &
                           ' duplicates the '//TRIM(cAlphaFields(4))//'.')
      ErrorsFound=.true.
    ENDIF

    CALL TestCompSet(TRIM(CurrentModuleObject),OAMixer(OutAirNum)%Name,AlphArray(3), &
                     AlphArray(2),'Air Nodes')

  END DO

END IF

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject))
ENDIF

GetOAMixerInputFlag = .FALSE.

DEALLOCATE(AlphArray)
DEALLOCATE(NumArray)
DEALLOCATE(lNumericBlanks)
DEALLOCATE(lAlphaBlanks)
DEALLOCATE(cAlphaFields)
DEALLOCATE(cNumericFields)

RETURN
END SUBROUTINE GetOAMixerInputs

! End of Get Input subroutines for the Module
!******************************************************************************

! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitOutsideAirSys(OASysNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Initialize the OutsideAirSys data structure

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
    USE DataLoopNode

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OASysNum !unused1208
    LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

IF (BeginEnvrnFlag .and. FirstHVACIteration) THEN
END IF

IF (BeginDayFlag) THEN
END IF

! Each time step
IF (FirstHVACIteration) THEN
END IF

! Each iteration

RETURN
END SUBROUTINE InitOutsideAirSys

SUBROUTINE InitOAController(OAControllerNum,FirstHVACIteration,AirLoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       Shirey/Raustad FSEC, June/Aug 2003, Feb 2004
          !                      Tianzhen Hong, Feb 2009 for DCV
          !                      Tianzhen Hong, Aug 2013 for economizer faults
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Initialize the OAController data structure with input node data

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
    USE DataLoopNode
    USE DataHeatBalance, ONLY: ZONEINTGAIN, Zone, ZoneList, TotPeople, People
    USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW
    USE InputProcessor, ONLY: FindItemInList, FindItem, SameString
    USE General, ONLY: RoundSigDigits
    USE OutputReportPredefined
    USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
    USE DataAirSystems,    ONLY: PrimaryAirSystem
    USE DataInterfaces,    ONLY: SetupEMSInternalVariable

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OAControllerNum
    LOGICAL, INTENT(IN) :: FirstHVACIteration
    INTEGER, INTENT(IN) :: AirLoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
LOGICAL,SAVE   :: MyOneTimeFlag             = .TRUE.          ! One-time initialization flag
LOGICAL,SAVE   :: MySetPointCheckFlag       = .TRUE.          ! One-time initialization flag
LOGICAL,SAVE   :: SetUpAirLoopHVACVariables = .TRUE.          ! One-time initialization flag
LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyEnvrnFlag        ! One-time initialization flag
LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MySizeFlag         ! One-time initialization flag
LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MechVentCheckFlag  ! One-time initialization flag
LOGICAL        :: FoundZone               ! Logical determines if ZONE object is accounted for in VENTILATION:MECHANICAL object
LOGICAL        :: FoundAreaZone           ! Logical determines if ZONE object is accounted for in VENTILATION:MECHANICAL object
LOGICAL        :: FoundPeopleZone         ! Logical determines if ZONE object is accounted for in VENTILATION:MECHANICAL object
LOGICAL        :: OASysFound              ! Logical determines if OA system found
LOGICAL        :: AirLoopFound            ! Logical determines if primary air loop found
LOGICAL        :: ErrorsFound             ! Errors found getting input
REAL(r64)      :: RhoAirStdInit           ! Standard air density
REAL(r64)      :: TotalPeopleOAFlow       ! Total outside air required for PEOPLE objects served by this OA controller
INTEGER        :: MixedAirNode            ! Controller:OutdoorAir mixed air node
INTEGER        :: OAControllerIndex       ! Index to Controller:OutdoorAir
INTEGER        :: ZoneNum                 ! DO loop index (zone number)
INTEGER        :: ZoneIndex               ! Index to zone in mechanical ventilation zone list
INTEGER        :: AirLoopZoneInfoZoneNum  ! Index to AirLoopZoneInfo structure
INTEGER        :: NumZone                 ! Zone number in AirLoopZoneInfo structure
INTEGER        :: PeopleNum               ! Index to PEOPLE objects
INTEGER        :: NumMechVentZone         ! Index to number of zones in VentilationMechanical structure
INTEGER        :: TempMechVentArrayCounter ! Temporary array counter
INTEGER        :: thisOASys               ! Temporary array counter
INTEGER        :: thisNumForMixer         ! Temporary array counter
INTEGER        :: thisMixerIndex          ! Temporary array counter
INTEGER        :: OASysNum                ! Temporary array counter
INTEGER        :: thisOAController        ! Temporary array counter
INTEGER        :: found                   ! Temporary index to equipment
INTEGER        :: OANode                  ! OA node index
INTEGER        :: VentMechObjectNum       ! Temporary variable
INTEGER        :: OAControllerLoop        ! Index to OA controller in an OA system
INTEGER        :: OAControllerLoop2       ! Index to OA controller in an OA system
INTEGER        :: thisAirLoop             ! Temporary array counter
INTEGER        :: BranchNum               ! Temporary array counter
INTEGER        :: CompNum                 ! Temporary array counter
CHARACTER(len=MaxNameLength) :: equipName ! Temporary equipment name
CHARACTER(len=MaxNameLength) :: airloopName ! Temporary equipment name
  REAL(r64), DIMENSION(:),ALLOCATABLE:: TempZoneOAAreaRate      ! Temporary array for mechanical ventilation rate (m3/s/m2) per zone
  REAL(r64), DIMENSION(:),ALLOCATABLE:: TempZoneOAPeopleRate    ! Temporary array for mechanical ventilation rate (m3/s/person) per zone
  REAL(r64), DIMENSION(:),ALLOCATABLE:: TempZoneOAFlow          ! Temporary array for mechanical ventilation rate (m3/s/flow) per zone
  REAL(r64), DIMENSION(:),ALLOCATABLE:: TempZoneOAACH           ! Temporary array for mechanical ventilation rate (m3/s/volume) per zone
  INTEGER, DIMENSION(:),ALLOCATABLE:: TempZoneOADSOAIndex       ! Temporary array for mechanical ventilation rate (m3/s/volume) per zone
  CHARACTER(len=MaxNameLength), DIMENSION(:),ALLOCATABLE:: TempZoneOADSOAName      ! Temporary array for mechanical ventilation rate (m3/s/volume) per zone
  INTEGER, DIMENSION(:),ALLOCATABLE:: TempZone            ! Temporary array for zones requiring mechanical ventilation
  REAL(r64), DIMENSION(:),ALLOCATABLE:: TempZoneADEffCooling   ! Temporary array for zone air distribution effectiveness
                                                               !  in cooling mode for each zone
  REAL(r64), DIMENSION(:),ALLOCATABLE:: TempZoneADEffHeating   ! Temporary array for zone air distribution effectiveness
                                                               ! in heating mode for each zone
  INTEGER, DIMENSION(:),ALLOCATABLE:: TempZoneADEffSchPtr      ! Temporary array for pointer to the zone air distribution
                                                               !  effectiveness schedule for each zone
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE:: TempZoneADEffSchName  ! Temporary array for zone air distribution
                                                               ! effectiveness schedule name for each zone
CHARACTER(len=MaxNameLength) :: zoneName
INTEGER :: jZone

REAL(r64) :: rSchVal
REAL(r64) :: rOffset
INTEGER   :: i
INTEGER   :: iEco


ErrorsFound = .FALSE.
OANode = 0

IF (MyOneTimeFlag) THEN

  ALLOCATE(MyEnvrnFlag(NumOAControllers))
  ALLOCATE(MySizeFlag(NumOAControllers))
  ALLOCATE(MechVentCheckFlag(NumOAControllers))
  MyEnvrnFlag = .TRUE.
  MySizeFlag = .TRUE.
  MechVentCheckFlag = .TRUE.

  ! Determine Inlet node index for OAController, not a user input for controller, but is obtained from OutsideAirSys and OAMixer
  ! This input setup needs to happen for all controllers, not just the one first passed into this init routine
  Do thisOAController=1, NumOAControllers

    SELECT CASE (OAController(thisOAController)%ControllerType_Num)

    CASE (ControllerOutsideAir)
      thisOASys = 0
      DO OASysNum=1, NumOASystems
        ! find which OAsys has this controller
        found = FindItemInList(OAController(thisOAController)%Name, OutsideAirSys(OASysNum)%ControllerName, &
                                     size(OutsideAirSys(OASysNum)%ControllerName))
        IF (found /= 0) then
          thisOASys = OASysNum
          EXIT  ! we found it
        ENDIF
      ENDDO
      IF (thisOASys == 0) then
        CALL ShowSevereError('InitOAController: Did not find OAController="'//TRIM(OAController(thisOAController)%Name)//'".')
        CALL ShowContinueError('in list of valid OA Controllers.')
        Errorsfound = .true.
        CYCLE
      ENDIF
      thisNumForMixer = FindItem(CurrentModuleObjects(CMO_OAMixer), OutsideAirSys(thisOASys)%ComponentType,   &
                                                            size(OutsideAirSys(thisOASys)%ComponentType) )
      If (thisNumForMixer /= 0) then
          equipName      = OutsideAirSys(thisOASys)%ComponentName(thisNumForMixer)
          thisMixerIndex = FindItemInList(equipName,OAMixer%Name, NumOAMixers )
          If (thisMixerIndex /= 0) then
            OAController(thisOAController)%InletNode  = OAMixer(thisMixerIndex)%InletNode
          ELSE
            CALL ShowSevereError('InitOAController: Did not find OAMixer="'//TRIM(equipName)//'".')
            CALL ShowContinueError('in list of valid OA Mixers.')
            Errorsfound = .true.
          ENDIF
      ELSE
        CALL ShowSevereError('InitOAController: Did not find OutdoorAir:Mixer Component="OutdoorAir:Mixer".')
        CALL ShowContinueError('in list of valid OA Components.')
        Errorsfound = .true.
      ENDIF

      IF (OAController(thisOAController)%InletNode == 0) THEN  !throw an error
          CALL ShowSevereError('InitOAController: Failed to find proper inlet node for OutdoorAir:Mixer and Controller = ' &
                  //TRIM(OAController(thisOAController)%Name) )
          ErrorsFound = .TRUE.
      ENDIF

    CASE (ControllerStandAloneERV)
      ! set the inlet node to also equal the OA node because this is a special controller for economizing stand alone ERV
      ! with the assumption that equipment is bypassed....

      OAController(thisOAController)%InletNode = OAController(thisOAController)%OANode

    CASE DEFAULT
      CALL ShowSevereError('InitOAController: Failed to find ControllerType: '&
                  //TRIM(OAController(thisOAController)%ControllerType) )
          ErrorsFound = .TRUE.

    END SELECT

  ENDDO
  MyOneTimeFlag = .false.

END IF

IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest .AND. .NOT. FirstHVACIteration) THEN
  DO OAControllerIndex=1,NumOAControllers
    MixedAirNode = OAController(OAControllerIndex)%MixNode
    IF (MixedAirNode > 0) THEN
!      IF (OAController(OAControllerIndex)%Econo == 1 .AND. .NOT. AirLoopControlInfo(AirLoopNum)%CyclingFan) THEN
      IF (OAController(OAControllerIndex)%Econo .GT. NoEconomizer .AND. AirLoopControlInfo(AirLoopNum)%AnyContFan) THEN
        IF (Node(MixedAirNode)%TempSetPoint == SensedNodeFlagValue) THEN
          IF (.NOT. AnyEnergyManagementSystemInModel) THEN
            CALL ShowSevereError('MixedAir: Missing temperature setpoint for economizer controller ' // &
                                  TRIM(OAController(OAControllerIndex)%Name))
            CALL ShowSevereError('Node Referenced (by Controller)='//TRIM(NodeID(MixedAirNode)))
            CALL ShowContinueError('  use a Setpoint Manager with Control Variable = "Temperature" to establish '//  &
                                   'a setpoint at the mixed air node.')
            SetPointErrorFlag = .TRUE.
          ELSE
            ! add call to check node in EMS
            CALL CheckIfNodeSetpointManagedByEMS(MixedAirNode,iTemperatureSetpoint, SetpointErrorFlag)
            IF (SetPointErrorFlag) THEN
              CALL ShowSevereError('MixedAir: Missing temperature setpoint for economizer controller ' // &
                                  TRIM(OAController(OAControllerIndex)%Name))
              CALL ShowSevereError('Node Referenced (by Controller)='//TRIM(NodeID(MixedAirNode)))
              CALL ShowContinueError('  use a Setpoint Manager with Control Variable = "Temperature" to establish '//  &
                                   'a setpoint at the mixed air node.')
              CALL ShowContinueError('Or add EMS Actuator to provide temperature setpoint at this node')
            ENDIF
          ENDIF
        END IF
      ENDIF
    END IF
  END DO

  MySetPointCheckFlag = .FALSE.
END IF

IF ( .NOT. SysSizingCalc .AND. MySizeFlag(OAControllerNum)) THEN
  CALL SizeOAController(OAControllerNum)
  IF (AirLoopNum > 0) THEN
    AirLoopControlInfo(AirLoopNum)%OACtrlNum = OAControllerNum
    AirLoopControlInfo(AirLoopNum)%OACtrlname = OAController(OAControllerNum)%Name
    IF (OAController(OAControllerNum)%Lockout == LockoutWithHeatingPossible) THEN
      AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithHeating = .TRUE.
      AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithCompressor = .FALSE.
      AirLoopControlInfo(AirLoopNum)%CanNotLockoutEcono = .FALSE.
    ELSE IF (OAController(OAControllerNum)%Lockout == LockoutWithCompressorPossible) THEN
      AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithHeating = .FALSE.
      AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithCompressor = .TRUE.
      AirLoopControlInfo(AirLoopNum)%CanNotLockoutEcono = .FALSE.
    ELSE
      AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithHeating = .FALSE.
      AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithCompressor = .FALSE.
      AirLoopControlInfo(AirLoopNum)%CanNotLockoutEcono = .TRUE.
    END IF
  END IF
  IF ((OAController(OAControllerNum)%MaxOA - OAController(OAControllerNum)%MinOA) .LT. -SmallAirVolFlow) THEN
       CALL ShowSevereError('For Controller:OutdoorAir: ' // TRIM(OAController(OAControllerNum)%Name))
       CALL ShowContinueError('  maximum outdoor air flow rate ('//TRIM(RoundSigDigits(OAController(OAControllerNum)%MaxOA,4))//  &
                         ') < minimum outdoor air flow rate ('//TRIM(RoundSigDigits(OAController(OAControllerNum)%MinOA,4))// &
                         ')')
       CALL ShowContinueError('  To set the minimum outside air flow rate use the '//  &
          '"Design (minimum) outdoor air flow rate" field in the Sizing:System object')
       ErrorsFound=.true.
  END IF
  MySizeFlag(OAControllerNum) = .FALSE.
END IF

IF (BeginEnvrnFlag .and. MyEnvrnFlag(OAControllerNum)) THEN
  OANode = OAController(OAControllerNum)%OANode
  RhoAirStdInit = StdRhoAir
  OAController(OAControllerNum)%MinOAMassFlowRate = OAController(OAControllerNum)%MinOA * RhoAirStdInit
  OAController(OAControllerNum)%MaxOAMassFlowRate = OAController(OAControllerNum)%MaxOA * RhoAirStdInit
  MyEnvrnFlag(OAControllerNum) = .FALSE.
  Node(OANode)%MassFlowRateMax = OAController(OAControllerNum)%MaxOAMassFlowRate

  !predefined reporting
  IF (OAController(OAControllerNum)%econo .GT. NoEconomizer) THEN
    equipName = OAController(OAControllerNum)%Name
    ! 90.1 descriptor for economizer controls
    ! Changed by Amit for New Feature implementation
    IF (OAController(OAControllerNum)%Econo .EQ. DifferentialEnthalpy) THEN
      CALL PreDefTableEntry(pdchEcoKind,equipName,'DifferentialEnthalpy')
    ELSE IF (OAController(OAControllerNum)%Econo .EQ. DifferentialDryBulb) THEN
      CALL PreDefTableEntry(pdchEcoKind,equipName,'DifferentialDryBulb')
    ELSE IF (OAController(OAControllerNum)%Econo .EQ. FixedEnthalpy) THEN
     CALL PreDefTableEntry(pdchEcoKind,equipName,'FixedEnthalpy')
    ELSE IF (OAController(OAControllerNum)%Econo .EQ. FixedDryBulb) THEN
      CALL PreDefTableEntry(pdchEcoKind,equipName,'FixedDryBulb')
    ELSE
      CALL PreDefTableEntry(pdchEcoKind,equipName,'Other')
    ENDIF

    CALL PreDefTableEntry(pdchEcoMinOA,equipName,OAController(OAControllerNum)%MinOA)
    CALL PreDefTableEntry(pdchEcoMaxOA,equipName,OAController(OAControllerNum)%MaxOA)
    !EnergyPlus input echos for economizer controls
    ! Chnged by Amit for new feature implementation
    IF(OAController(OAControllerNum)%Econo .EQ. DifferentialDryBulb) THEN
      CALL PreDefTableEntry(pdchEcoRetTemp,equipName,'Yes')
    ELSE
      CALL PreDefTableEntry(pdchEcoRetTemp,equipName,'No')
    ENDIF
    IF(OAController(OAControllerNum)%Econo .EQ. DifferentialEnthalpy) THEN
      CALL PreDefTableEntry(pdchEcoRetTemp,equipName,'Yes')
    ELSE
      CALL PreDefTableEntry(pdchEcoRetTemp,equipName,'No')
    ENDIF
    IF(OAController(OAControllerNum)%Econo .EQ. FixedDryBulb) THEN
      CALL PreDefTableEntry(pdchEcoRetTemp,equipName,OAController(OAControllerNum)%TempLim)
    ELSE
      CALL PreDefTableEntry(pdchEcoRetTemp,equipName,'-')
    ENDIF
    IF(OAController(OAControllerNum)%Econo .EQ. FixedEnthalpy) THEN
      CALL PreDefTableEntry(pdchEcoRetTemp,equipName,OAController(OAControllerNum)%EnthLim)
    ELSE
      CALL PreDefTableEntry(pdchEcoRetTemp,equipName,'-')
    ENDIF
  END IF
END IF

IF (.not. BeginEnvrnFlag) THEN
  MyEnvrnFlag(OAControllerNum) = .true.
ENDIF

VentMechObjectNum = OAController(OAControllerNum)%VentMechObjectNum
IF(MechVentCheckFlag(OAControllerNum))THEN
! Make these checks only once at the beginning of the simulation

! Make sure all air loop zones and air loop zones with people objects are covered by mechanical ventilation
! Issue a warning only if the zone is not accounted for in the associated mechanical ventilation object
  IF(VentMechObjectNum .GT. 0)THEN

    ALLOCATE(TempZoneOAAreaRate(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZoneOAPeopleRate(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZoneOAFlow(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZoneOAACH(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZoneOADSOAName(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZoneOADSOAIndex(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZone(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZoneADEffCooling(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZoneADEffHeating(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZoneADEffSchPtr(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    ALLOCATE(TempZoneADEffSchName(VentilationMechanical(VentMechObjectNum)%NumofVentMechZones))
    TempZoneOAAreaRate=0.0d0
    TempZoneOAPeopleRate=0.0d0
    TempZoneOAFlow=0.0d0
    TempZoneOAACH=0.0d0
    TempZoneOADSOAName=' '
    TempZoneOADSOAIndex=0
    TempZone=0
    TempZoneADEffCooling=1.0d0
    TempZoneADEffHeating=1.0d0
    TempZoneADEffSchPtr=0
    TempZoneADEffSchName=' '
  ! Make sure all zones with mechanical ventilation are on the correct air loop
    TempMechVentArrayCounter = 0
    DO NumMechVentZone = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
      ZoneNum = VentilationMechanical(VentMechObjectNum)%Zone(NumMechVentZone)
      FoundZone = .FALSE.

      DO AirLoopZoneInfoZoneNum = 1, AirLoopZoneInfo(AirLoopNum)%NumZones
        NumZone = AirLoopZoneInfo(AirLoopNum)%ActualZoneNumber(AirLoopZoneInfoZoneNum)
        IF(ZoneNum .EQ. NumZone)THEN
          FoundZone = .TRUE.
          TempMechVentArrayCounter = TempMechVentArrayCounter + 1
          TempZone(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%Zone(NumMechVentZone)
          TempZoneOAAreaRate(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneOAAreaRate(NumMechVentZone)
          TempZoneOAPeopleRate(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(NumMechVentZone)
          TempZoneOAFlow(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneOAFlow(NumMechVentZone)
          TempZoneOAACH(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneOAACH(NumMechVentZone)
          TempZoneOADSOAIndex(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjIndex(NumMechVentZone)
          TempZoneOADSOAName(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjName(NumMechVentZone)

          ! new DCV
          TempZoneADEffCooling(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneADEffCooling(NumMechVentZone)
          TempZoneADEffHeating(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneADEffHeating(NumMechVentZone)
          TempZoneADEffSchPtr(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneADEffSchPtr(NumMechVentZone)
          TempZoneADEffSchName(TempMechVentArrayCounter) = &
              VentilationMechanical(VentMechObjectNum)%ZoneADEffSchName(NumMechVentZone)

  !       Sum outside air per unit floor area for each mechanical ventilation object only once per simulation
          VentilationMechanical(VentMechObjectNum)%TotAreaOAFlow = &
                      VentilationMechanical(VentMechObjectNum)%TotAreaOAFlow + &
                      Zone(ZoneNum)%FloorArea * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                      VentilationMechanical(VentMechObjectNum)%ZoneOAAreaRate(NumMechVentZone)
          VentilationMechanical(VentMechObjectNum)%TotZoneOAFlow = &
                      VentilationMechanical(VentMechObjectNum)%TotZoneOAFlow + &
                      Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                      VentilationMechanical(VentMechObjectNum)%ZoneOAFlow(NumMechVentZone)
          VentilationMechanical(VentMechObjectNum)%TotZoneOAACH = &
                      VentilationMechanical(VentMechObjectNum)%TotZoneOAACH + &
                      Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                      (VentilationMechanical(VentMechObjectNum)%ZoneOAACH(NumMechVentZone)*Zone(ZoneNum)%Volume/3600.d0)
          EXIT
        END IF
      END DO
      IF(.NOT. FoundZone)THEN
        CALL ShowWarningError('Zone name = '//TRIM(Zone(ZoneNum)%Name)// &
                        ' in '//trim(CurrentModuleObjects(CMO_MechVentilation))//' object name = ' &
                        //TRIM(OAController(OAControllerNum)%VentilationMechanicalName)// &
                        ' is not on the same air loop as Controller:OutdoorAir = '//TRIM(OAController(OAControllerNum)%Name))
        CALL ShowContinueError('This zone will not be used and the simulation will continue...')
      END IF
    END DO

  ! Re-size final array to conserve environment space
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%Zone)
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneOAAreaRate)
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate)
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneOAFlow)
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneOAACH)
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjIndex)
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjName)

    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneADEffCooling)
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneADEffHeating)
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneADEffSchPtr)
    DEALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneADEffSchName)

    ALLOCATE(VentilationMechanical(VentMechObjectNum)%Zone(TempMechVentArrayCounter))
    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneOAAreaRate(TempMechVentArrayCounter))
    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(TempMechVentArrayCounter))
    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneOAFlow(TempMechVentArrayCounter))
    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneOAACH(TempMechVentArrayCounter))
    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjIndex(TempMechVentArrayCounter))
    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjName(TempMechVentArrayCounter))

    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneADEffCooling(TempMechVentArrayCounter))
    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneADEffHeating(TempMechVentArrayCounter))
    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneADEffSchPtr(TempMechVentArrayCounter))
    ALLOCATE(VentilationMechanical(VentMechObjectNum)%ZoneADEffSchName(TempMechVentArrayCounter))

    DO NumMechVentZone = 1, TempMechVentArrayCounter
      VentilationMechanical(VentMechObjectNum)%Zone(NumMechVentZone) = &
              TempZone(NumMechVentZone)
      VentilationMechanical(VentMechObjectNum)%ZoneOAAreaRate(NumMechVentZone) = &
              TempZoneOAAreaRate(NumMechVentZone)
      VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(NumMechVentZone) = &
              TempZoneOAPeopleRate(NumMechVentZone)
      VentilationMechanical(VentMechObjectNum)%ZoneOAFlow(NumMechVentZone) = &
              TempZoneOAFlow(NumMechVentZone)
      VentilationMechanical(VentMechObjectNum)%ZoneOAACH(NumMechVentZone) = &
              TempZoneOAACH(NumMechVentZone)
      VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjName(NumMechVentZone) = &
              TempZoneOADSOAName(NumMechVentZone)
      VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjIndex(NumMechVentZone) = &
              TempZoneOADSOAIndex(NumMechVentZone)

      VentilationMechanical(VentMechObjectNum)%ZoneADEffCooling(NumMechVentZone) = &
              TempZoneADEffCooling(NumMechVentZone)
      VentilationMechanical(VentMechObjectNum)%ZoneADEffHeating(NumMechVentZone) = &
              TempZoneADEffHeating(NumMechVentZone)
      VentilationMechanical(VentMechObjectNum)%ZoneADEffSchPtr(NumMechVentZone) = &
              TempZoneADEffSchPtr(NumMechVentZone)
      VentilationMechanical(VentMechObjectNum)%ZoneADEffSchName(NumMechVentZone) = &
              TempZoneADEffSchName(NumMechVentZone)
    END DO

    VentilationMechanical(VentMechObjectNum)%NumofVentMechZones = TempMechVentArrayCounter

    !predefined report
    DO jZone = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
      zoneName = zone(VentilationMechanical(VentMechObjectNum)%Zone(jZone))%name
      CALL PreDefTableEntry(pdchDCVventMechName,zoneName,VentilationMechanical(VentMechObjectNum)%Name)
      CALL PreDefTableEntry(pdchDCVperPerson,zoneName, VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(jZone),6)
      CALL PreDefTableEntry(pdchDCVperArea,zoneName, VentilationMechanical(VentMechObjectNum)%ZoneOAAreaRate(jZone),6)

      ! added for new DCV inputs
      CALL PreDefTableEntry(pdchDCVZoneADEffCooling,zoneName, VentilationMechanical(VentMechObjectNum)%ZoneADEffCooling(jZone),2)
      CALL PreDefTableEntry(pdchDCVZoneADEffHeating,zoneName, VentilationMechanical(VentMechObjectNum)%ZoneADEffHeating(jZone),2)
      CALL PreDefTableEntry(pdchDCVZoneADEffSchName,zoneName,   &
         GetScheduleName(VentilationMechanical(VentMechObjectNum)%ZoneADEffSchPtr(jZone)))
    END DO

  ! Delete temporary array
    DEALLOCATE(TempZone)
    DEALLOCATE(TempZoneOAAreaRate)
    DEALLOCATE(TempZoneOAPeopleRate)
    DEALLOCATE(TempZoneOAFlow)
    DEALLOCATE(TempZoneOAACH)
    DEALLOCATE(TempZoneOADSOAIndex)
    DEALLOCATE(TempZoneOADSOAName)

    DEALLOCATE(TempZoneADEffCooling)
    DEALLOCATE(TempZoneADEffHeating)
    DEALLOCATE(TempZoneADEffSchPtr)
    DEALLOCATE(TempZoneADEffSchName)

  ! Check to see if any zones on an air loop are not accounted for by a mechanical ventilation object
    DO AirLoopZoneInfoZoneNum = 1, AirLoopZoneInfo(AirLoopNum)%NumZones
      NumZone = AirLoopZoneInfo(AirLoopNum)%ActualZoneNumber(AirLoopZoneInfoZoneNum)
      FoundAreaZone = .FALSE.
      FoundPeopleZone = .FALSE.
        DO NumMechVentZone = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
          ZoneNum = VentilationMechanical(VentMechObjectNum)%Zone(NumMechVentZone)
          IF(ZoneNum .EQ. NumZone)THEN
            FoundAreaZone = .TRUE.
            IF(VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(NumMechVentZone) .GT. 0.0d0)THEN
              FoundPeopleZone = .TRUE.
            END IF
          EXIT
          END IF
        END DO
        IF(.NOT. FoundAreaZone)THEN
          CALL ShowWarningError('Zone name = '//TRIM(Zone(NumZone)%Name)// &
                              ' is not accounted for by '//trim(CurrentModuleObjects(CMO_MechVentilation))//' object name = ' &
                              //TRIM(OAController(OAControllerNum)%VentilationMechanicalName))
          CALL ShowContinueError('Ventilation per unit floor area has not been specified for this zone, which is connected to')
          CALL ShowContinueError('the air loop served by Controller:OutdoorAir = '//TRIM(OAController(OAControllerNum)%Name)//&
                                 '. Simulation will continue...')
        END IF
        IF(.NOT. FoundPeopleZone)THEN
          ! Loop through people objects to see if this zone has a people object and only then show a warning
          DO PeopleNum = 1, TotPeople
            IF(People(PeopleNum)%ZonePtr .EQ. NumZone)THEN
              IF (.not. FoundAreaZone)THEN
!  !             If the zone was found, then the people ventilation rate is set to 0
!                CALL ShowWarningError('PEOPLE object for zone = '//TRIM(Zone(NumZone)%Name)// &
!                                    ' is not accounted for by '//trim(CurrentModuleObjects(CMO_MechVentilation))//  &
!                                    ' object name = '//TRIM(OAController(OAControllerNum)%VentilationMechanicalName))
!                CALL ShowContinueError('A "PEOPLE" object has been specified in the idf for this zone, '// &
!                                       'but the ventilation rate is set to 0 in this Controller:MechanicalVentilation Object.')
!                CALL ShowContinueError('Check ventilation rate in Controller:MechanicalVentilation object. '//  &
!                   ' Simulation will continue.')
!              ELSE
  !             If the zone was not found, then the PEOPLE objects are not accounted for
                CALL ShowWarningError('PEOPLE object for zone = '//TRIM(Zone(NumZone)%Name)// &
                                    ' is not accounted for by '//trim(CurrentModuleObjects(CMO_MechVentilation))//  &
                                    ' object name = '//TRIM(OAController(OAControllerNum)%VentilationMechanicalName))
                CALL ShowContinueError('A "PEOPLE" object has been specified in the idf for this zone, but it is not included '// &
                                     'in this '//trim(CurrentModuleObjects(CMO_MechVentilation))//' Object.')
                CALL ShowContinueError('Check '//trim(CurrentModuleObjects(CMO_MechVentilation))//  &
                   ' object. Simulation will continue.')
              END IF
            END IF
          END DO
        ELSE  ! People > 0, check to make sure there is a people statement in the zone
          FoundAreaZone=.false.
          DO PeopleNum = 1, TotPeople
            IF (People(PeopleNum)%ZonePtr /= NumZone) CYCLE
            FoundAreaZone=.true.
            EXIT
          ENDDO
          IF (.not. FoundAreaZone) THEN
            CALL ShowWarningError(trim(CurrentModuleObjects(CMO_MechVentilation))//  &
                       ' = "'//TRIM(OAController(OAControllerNum)%VentilationMechanicalName)// &
                       '", Zone="'//TRIM(Zone(NumZone)%Name)//'".')
            CALL ShowContinueError('No "PEOPLE" object has been specified in the idf for this zone, '// &
                                   'but the ventilation rate is > 0 in this Controller:MechanicalVentilation Object.')
            CALL ShowContinueError('Check ventilation rate in Controller:MechanicalVentilation object. '//  &
                ' Simulation will continue.')
          ENDIF
        END IF
    END DO

  END IF

  MechVentCheckFlag(OAControllerNum) = .FALSE.

END IF
!****

! Perform a one time initialization of AirloopHVAC OA System report variables
!
! If AirloopHVAC objects are used, NumPrimaryAirSys > 0 and the initialization proceeds and then sets
! SetUpAirLoopHVACVariables to .FALSE. so this is never done again and only the first IF is checked
! each time through Init. If for some reason the primary air system have not yet been read in, this
! code waits for the air loop data to be available before performing the report variable initialization.
!
! If AirloopHVAC objects are not used, NumPrimaryAirSys is always equal to 0 and only these
! two IF statements are checked each time through Init (e.g., if StandAloneERV controllers are used
! without AirloopHVAC objects).
!
IF(SetUpAirLoopHVACVariables)THEN
  IF(NumPrimaryAirSys .GT. 0)THEN
    ! Added code to report (TH, 10/20/2008):
    !   air economizer status (1 = on, 0 = off or does not exist), and
    !   actual and minimum outside air fraction (0 to 1)
    DO OAControllerLoop=1,NumOAControllers
      !Find the outside air system that has the OA controller
      IF (OAController(OAControllerLoop)%ControllerType_Num == ControllerStandAloneERV) CYCLE  ! ERV controller not on airloop
      OASysFound = .FALSE.
      thisOASys = 0
      DO OASysNum = 1, NumOASystems
        DO OAControllerLoop2 = 1, OutsideAirSys(OASysNum)%NumControllers
          IF(SameString(OutsideAirSys(OASysNum)%ControllerName(OAControllerLoop2),&
                        OAController(OAControllerLoop)%Name))THEN
            thisOASys = OASysNum
            OASysFound = .TRUE.
            EXIT
          ENDIF
        END DO
        IF(OASysFound) EXIT
      END DO

      IF (ThisOASys <= 0) THEN
        !Check outside air system name
        CALL ShowWarningError('Cannot find the AirLoopHVAC:OutdoorAirSystem for the OA Controller: ' &
                              //TRIM(OAController(OAControllerNum)%Name))
        AirLoopFound = .FALSE.
      Else
        !Find the primary air loop that has the outside air system
        AirLoopFound = .FALSE.
        DO thisAirLoop = 1, NumPrimaryAirSys
          DO BranchNum = 1, PrimaryAirSystem(thisAirLoop)%NumBranches
            DO CompNum = 1, PrimaryAirSystem(thisAirLoop)%Branch(BranchNum)%TotalComponents
              IF(.NOT. SameString(PrimaryAirSystem(thisAirLoop)%Branch(BranchNum)%Comp(CompNum)%Name, &
                                  OutsideAirSys(thisOASys)%Name) .OR. &
                 .NOT. SameString(PrimaryAirSystem(thisAirLoop)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                                  'AirLoopHVAC:OutdoorAirSystem'))CYCLE
              AirLoopFound=.TRUE.
              EXIT
            END DO
            IF(AirLoopFound)EXIT
          END DO
          IF(AirLoopFound)EXIT
        ENDDO
      ENDIF
      ! Check primary air loop name
      IF (AirLoopFound .AND. thisAirLoop>0) THEN
        airloopName = PrimaryAirSystem(thisAirLoop)%Name   ! OutsideAirSys(OASysIndex)%Name
      ELSE
        CALL ShowWarningError('Cannot find the primary air loop for the OA Controller: ' &
                              //TRIM(OAController(OAControllerNum)%Name))
        airloopName='AirLoop not found'
      ENDIF

!    Note use of OAControllerLoop here to keep DO Loop index separate from InitOAController local variable
      ! CurrentModuleObject='AirLoopHVAC'
      CALL SetupOutputVariable('Air System Outdoor Air Economizer Status []', &
      OAController(OAControllerLoop)%EconomizerStatus, 'System','Average',airloopName)

      CALL SetupOutputVariable('Air System Outdoor Air Heat Recovery Bypass Status []', &
      OAController(OAControllerLoop)%HeatRecoveryBypassStatus, 'System','Average',airloopName)

      IF(OAController(OAControllerLoop)%HeatRecoveryBypassControlType == BypassWhenOAFlowGreaterThanMinimum)THEN
        CALL SetupOutputVariable('Air System Outdoor Air Heat Recovery Bypass Heating Coil Activity Status []', &
        OAController(OAControllerLoop)%HRHeatingCoilActive, 'System','Average',airloopName)
        CALL SetupOutputVariable('Air System Outdoor Air Heat Recovery Bypass Minimum Outdoor Air Mixed Air Temperature [C]', &
        OAController(OAControllerLoop)%MixedAirTempAtMinOAFlow, 'System','Average',airloopName)
      END IF

      CALL SetupOutputVariable('Air System Outdoor Air High Humidity Control Status []', &
      OAController(OAControllerLoop)%HighHumCtrlStatus, 'System','Average',airloopName)

      CALL SetupOutputVariable('Air System Outdoor Air Flow Fraction []', &
      OAController(OAControllerLoop)%OAFractionRpt, 'System','Average',airloopName)

      CALL SetupOutputVariable('Air System Outdoor Air Minimum Flow Fraction []', &
      OAController(OAControllerLoop)%MinOAFracLimit, 'System','Average',airloopName)

      CALL SetupOutputVariable('Air System Outdoor Air Mass Flow Rate [kg/s]', &
      OAController(OAControllerLoop)%OAMassFlow, 'System','Average',airloopName)

      CALL SetupOutputVariable('Air System Mixed Air Mass Flow Rate [kg/s]', &
      OAController(OAControllerLoop)%MixMassFlow, 'System','Average',airloopName)

      IF (AnyEnergyManagementSystemInModel) THEN
        CALL SetupEMSInternalVariable('Outdoor Air Controller Maximum Mass Flow Rate',   &
           OAController(OAControllerLoop)%Name, '[kg/s]', OAController(OAControllerLoop)%MaxOAMassFlowRate  )
        CALL SetupEMSInternalVariable('Outdoor Air Controller Minimum Mass Flow Rate',   &
           OAController(OAControllerLoop)%Name, '[kg/s]',  OAController(OAControllerLoop)%MinOAMassFlowRate  )
        CALL SetupEMSActuator('Outdoor Air Controller', OAController(OAControllerLoop)%Name, 'Air Mass Flow Rate' , '[kg/s]', &
           OAController(OAControllerLoop)%EMSOverrideOARate, OAController(OAControllerLoop)%EMSOARateValue )
      ENDIF


    END DO

    SetUpAirLoopHVACVariables = .FALSE.

  END IF
END IF


! Each time step
IF (FirstHVACIteration) THEN
! Mixed air setpoint. Set by a setpoint manager.
  IF(OAController(OAControllerNum)%ControllerType_Num == ControllerOutsideAir)THEN
    IF (Node(OAController(OAControllerNum)%MixNode)%TempSetPoint > 0.0d0) THEN
      OAController(OAControllerNum)%MixSetTemp = Node(OAController(OAControllerNum)%MixNode)%TempSetPoint
    ELSE
      OAController(OAControllerNum)%MixSetTemp = OAController(OAControllerNum)%TempLowLim
    END IF

    TotalPeopleOAFlow = 0.0d0
    IF(VentMechObjectNum /= 0)THEN
      DO ZoneIndex = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
        ZoneNum = VentilationMechanical(VentMechObjectNum)%Zone(ZoneIndex)

        ! ZoneIntGain(ZoneNum)%NOFOCC is the number of occupants of a zone at each time step, already counting the occupant schedule
        TotalPeopleOAFlow = TotalPeopleOAFlow + &
          ZoneIntGain(ZoneNum)%NOFOCC * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
          VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(ZoneIndex)
      END DO
      VentilationMechanical(VentMechObjectNum)%TotPeopleOAFlow = TotalPeopleOAFlow
    END IF
!
  ELSE
! Stand Alone ERV does not require a termperature setpoint schedule, make setpoint equal to lower economizer limit
    OAController(OAControllerNum)%MixSetTemp = OAController(OAControllerNum)%TempLowLim
  END IF

END IF

! Each iteration

IF(OAController(OAControllerNum)%ControllerType_Num == ControllerOutsideAir)THEN
  ! zone exhaust mass flow is saved in AirLoopFlow%ZoneExhaust
  ! the zone exhaust mass flow that is said to be balanced by simple air flows is saved in AirLoopFlow%ZoneExhaustBalanced
  IF (AirLoopNum > 0) THEN
    !OAController(OAControllerNum)%ExhMassFlow = AirLoopFlow(AirLoopNum)%ZoneExhaust
    OAController(OAControllerNum)%ExhMassFlow = AirLoopFlow(AirLoopNum)%ZoneExhaust - AirLoopFlow(AirLoopNum)%ZoneExhaustBalanced
    IF (AirLoopControlInfo(AirLoopNum)%LoopFlowRateSet .AND. .NOT. FirstHVACIteration) THEN
      ! if flow rate has been specified by a manager, set it to the specified value
      OAController(OAControllerNum)%MixMassFlow = AirLoopFlow(AirLoopNum)%ReqSupplyFrac * AirLoopFlow(AirLoopNum)%DesSupply
    ELSE
      OAController(OAControllerNum)%MixMassFlow = Node(OAController(OAControllerNum)%RetNode)%MassFlowRate + &
                                                    OAController(OAControllerNum)%ExhMassFlow
    END IF
  ELSE
    OAController(OAControllerNum)%ExhMassFlow = 0.0d0
    OAController(OAControllerNum)%MixMassFlow = Node(OAController(OAControllerNum)%RetNode)%MassFlowRate
  END IF
  IF (Node(OAController(OAControllerNum)%MixNode)%MassFlowRateMaxAvail <= 0.0d0) THEN
    OAController(OAControllerNum)%MixMassFlow = 0.0d0
  END IF
ELSE
  ! Mixed and exhaust flow rates are passed through to model CONTROLLER:STAND ALONE ERV in SimOAController
  OAController(OAControllerNum)%OAMassFlow = OAController(OAControllerNum)%MaxOAMassFlowRate
  OAController(OAControllerNum)%MixMassFlow = OAController(OAControllerNum)%MaxOAMassFlowRate
  OAController(OAControllerNum)%ExhMassFlow = Node(OAController(OAControllerNum)%RetNode)%MassFlowRate
END IF
OAController(OAControllerNum)%ExhMassFlow = MAX(OAController(OAControllerNum)%ExhMassFlow,0.0d0)

! Outside air values
OAController(OAControllerNum)%OATemp = Node(OAController(OAControllerNum)%OANode)%Temp
OAController(OAControllerNum)%OAEnth = Node(OAController(OAControllerNum)%OANode)%Enthalpy
OAController(OAControllerNum)%OAPress = Node(OAController(OAControllerNum)%OANode)%Press
OAController(OAControllerNum)%OAHumRat = Node(OAController(OAControllerNum)%OANode)%HumRat

! Inlet air values (on OA input side)
OAController(OAControllerNum)%InletTemp = Node(OAController(OAControllerNum)%InletNode)%Temp
OAController(OAControllerNum)%InletEnth = Node(OAController(OAControllerNum)%InletNode)%Enthalpy
OAController(OAControllerNum)%InletPress = Node(OAController(OAControllerNum)%InletNode)%Press
OAController(OAControllerNum)%InletHumRat = Node(OAController(OAControllerNum)%InletNode)%HumRat

! Return air values
OAController(OAControllerNum)%RetTemp = Node(OAController(OAControllerNum)%RetNode)%Temp
OAController(OAControllerNum)%RetEnth = Node(OAController(OAControllerNum)%RetNode)%Enthalpy


!
! Check sensors faults for the air economizer
!
iEco = OAController(OAControllerNum)%Econo
IF (AnyFaultsInModel .AND. (iEco > NoEconomizer)) THEN
  DO i = 1, NumFaults
    IF ((Faults(i)%ControllerTypeEnum == iController_AirEconomizer) .AND. &
        (Faults(i)%ControllerID == OAControllerNum)) THEN

      IF (GetCurrentScheduleValue(Faults(i)%AvaiSchedPtr) > 0.0d0) THEN
        rSchVal = 1.0d0
        IF (Faults(i)%SeveritySchedPtr > 0) THEN
          rSchVal = GetCurrentScheduleValue(Faults(i)%SeveritySchedPtr)
        ENDIF
      ELSE
        ! no fault
        CYCLE
      ENDIF

      rOffset = rSchVal * Faults(i)%Offset

      IF(ABS(rOffset) < 0.000000001d0) CYCLE

      ! ECONOMIZER - outdoor air dry-bulb temperature sensor offset
      SELECT CASE(iEco)
        CASE(FixedDryBulb, DifferentialDryBulb, FixedDewpointAndDryBulb, ElectronicEnthalpy, DifferentialDryBulbAndEnthalpy)
          IF(Faults(i)%FaultTypeEnum == iFault_TemperatureSensorOffset_OutdoorAir) THEN
            ! FaultModel:TemperatureSensorOffset:OutdoorAir
            OAController(OAControllerNum)%OATemp = OAController(OAControllerNum)%OATemp + rOffset
            OAController(OAControllerNum)%InletTemp = OAController(OAControllerNum)%InletTemp + rOffset
          ENDIF
        CASE DEFAULT
        END SELECT

      ! ECONOMIZER - outdoor air humidity ratio sensor offset. really needed ???
      SELECT CASE(iEco)
        CASE(FixedDewpointAndDryBulb, ElectronicEnthalpy)
          IF(Faults(i)%FaultTypeEnum == iFault_HumiditySensorOffset_OutdoorAir) THEN
            ! FaultModel:HumiditySensorOffset:OutdoorAir
            OAController(OAControllerNum)%OAHumRat = OAController(OAControllerNum)%OAHumRat + rOffset
            OAController(OAControllerNum)%InletHumRat = OAController(OAControllerNum)%InletHumRat + rOffset
          ENDIF
        CASE DEFAULT
        END SELECT

      ! ECONOMIZER - outdoor air enthalpy sensor offset
      SELECT CASE(iEco)
        CASE(FixedEnthalpy, ElectronicEnthalpy, DifferentialDryBulbAndEnthalpy)
          IF(Faults(i)%FaultTypeEnum == iFault_EnthalpySensorOffset_OutdoorAir) THEN
            ! FaultModel:EnthalpySensorOffset:OutdoorAir
            OAController(OAControllerNum)%OAEnth = OAController(OAControllerNum)%OAEnth + rOffset
            OAController(OAControllerNum)%InletEnth = OAController(OAControllerNum)%InletEnth + rOffset
          ENDIF
        CASE DEFAULT
       END SELECT

      ! ECONOMIZER - return air dry-bulb temperature sensor offset
      SELECT CASE(iEco)
        CASE(DifferentialDryBulb, DifferentialDryBulbAndEnthalpy)
          IF(Faults(i)%FaultTypeEnum == iFault_TemperatureSensorOffset_ReturnAir) THEN
            ! FaultModel:TemperatureSensorOffset:ReturnAir
            OAController(OAControllerNum)%RetTemp = OAController(OAControllerNum)%RetTemp + rOffset
          ENDIF
        CASE DEFAULT
      END SELECT

      ! ECONOMIZER - return air enthalpy sensor offset
      SELECT CASE(iEco)
        CASE(ElectronicEnthalpy, DifferentialDryBulbAndEnthalpy)
          IF(Faults(i)%FaultTypeEnum == iFault_EnthalpySensorOffset_ReturnAir) THEN
            ! FaultModel:EnthalpySensorOffset:ReturnAir
            OAController(OAControllerNum)%RetEnth = OAController(OAControllerNum)%RetEnth + rOffset
          ENDIF
        CASE DEFAULT
      END SELECT
    ENDIF
  ENDDO
ENDIF

IF (ErrorsFound) THEN
  CALL ShowFatalError('Error in '//trim(CurrentModuleObjects(CMO_OAController))//'; program terminated')
END IF

RETURN
END SUBROUTINE InitOAController

SUBROUTINE InitOAMixer(OAMixerNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Initialize the OAMixer data structure with input node data

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
    USE DataLoopNode

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OAMixerNum
    LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: RetNode
INTEGER :: InletNode
INTEGER :: RelNode

RetNode = OAMixer(OAMixerNum)%RetNode
InletNode = OAMixer(OAMixerNum)%InletNode
RelNode = OAMixer(OAMixerNum)%RelNode

IF (BeginEnvrnFlag .and. FirstHVACIteration) THEN
END IF

IF (BeginDayFlag) THEN
END IF

IF (FirstHVACIteration) THEN
END IF

! Each iteration

! Return air stream data
OAMixer(OAMixerNum)%RetTemp = Node(RetNode)%Temp
OAMixer(OAMixerNum)%RetHumRat = Node(RetNode)%HumRat
OAMixer(OAMixerNum)%RetEnthalpy = Node(RetNode)%Enthalpy
OAMixer(OAMixerNum)%RetPressure = Node(RetNode)%Press
OAMixer(OAMixerNum)%RetMassFlowRate = Node(RetNode)%MassFlowRate
! Outside air stream data
OAMixer(OAMixerNum)%OATemp = Node(InletNode)%Temp
OAMixer(OAMixerNum)%OAHumRat = Node(InletNode)%HumRat
OAMixer(OAMixerNum)%OAEnthalpy = Node(InletNode)%Enthalpy
OAMixer(OAMixerNum)%OAPressure = Node(InletNode)%Press
OAMixer(OAMixerNum)%OAMassFlowRate = Node(InletNode)%MassFlowRate
! Relief air data
OAMixer(OAMixerNum)%RelMassFlowRate = Node(RelNode)%MassFlowRate

RETURN
END SUBROUTINE InitOAMixer

! End of Initialization Section of the Module
!******************************************************************************

! Beginning Calculation Section of the Module
!******************************************************************************

SUBROUTINE CalcOAController(OAControllerNum,AirLoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       Shirey/Raustad FSEC, June 2003
          !                      Tianzhen Hong, Feb 2009 for new DCV
          !                      Brent Griffith ,EMS override of OA rate
          !                      Mangesh Basarkar, 06/2011: Modifying outside air calculation based on DCV flag
          !                      Chandan Sharma, FSEC, 25Aug 2011 - Added ProportionalControl
          !                           to enhance CO2 based DCV control
          !                      Tianzhen Hong, March 2012, zone maximum OA fraction - a TRACE feature
          !                      Tianzhen Hong, March 2012, multi-path VRP based on ASHRAE 62.1-2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Determine the outside air flow

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! DOE-2.1E Supplement pages 3.97 - 3.100
          ! BLAST User Reference pages 183 - 186
          ! ASHRAE Standard 62.1-2010

          ! USE STATEMENTS:
    USE General, ONLY: SolveRegulaFalsi, RoundSigDigits
    USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW, PsyTdpFnWPb, PsyTdbFnHW
    USE DataZoneEnergyDemands, ONLY: ZoneSysMoistureDemand, ZoneSysEnergyDemand
    USE CurveManager, ONLY: CurveValue
    USE InputProcessor,  ONLY: FindItemInList, GetNumObjectsFound
    USE DataHeatBalance, ONLY: ZoneIntGain, Zone, People, TotPeople
    USE DataZoneEquipment, ONLY: ZoneEquipConfig
    USE DataHeatBalFanSys, ONLY: ZoneAirHumRat
    USE DataContaminantBalance, ONLY: ZoneSysContDemand
    USE DataGlobals, ONLY: DisplayExtraWarnings
    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OAControllerNum
    INTEGER, INTENT(IN) :: AirLoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER, PARAMETER   :: MaxIte = 500          ! Maximum number of iterations
REAL(r64), PARAMETER :: Acc =  0.0001D0       ! Accuracy of result
CHARACTER(len=*),PARAMETER :: RoutineName='CalcOAController: '
CHARACTER(len=*), PARAMETER :: CurrentModuleObject=CurrentModuleObjects(CMO_MechVentilation)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: OutAirSignal               ! Used to set OA mass flow rate
REAL(r64) :: OutAirMinFrac              ! Local variable used to calculate min OA fraction
REAL(r64) :: MechVentOutsideAirMinFrac  ! fraction of OA specified by mechanical ventilation object
REAL(r64) :: MechVentOutsideAirFlow     ! outside air mass flow rate specified by mechanical ventilation object
REAL(r64) :: MinOASchedVal              ! value of the minimum outside air schedule
REAL(r64) :: EconomizerAirFlowScheduleValue ! value of economizer operation schedule (push-button type control schedule)
REAL(r64) :: OASignal                   ! Outside air flow rate fraction (0.0 to 1.0)
!unused1208 REAL(r64) :: OADPTemp                   ! Outside air dew point temperature
REAL(r64), DIMENSION(4)  :: Par         ! Par(1) = mixed air node number
                                   ! Par(2) = return air node number
                                   ! Par(3) = outside air node number
                                   ! Par(4) = mixed air mass flow rate
INTEGER :: SolFla                  ! Flag of solver
LOGICAL :: AirLoopEconoLockout     ! Economizer lockout flag
LOGICAL :: AirLoopCyclingFan       ! Type of air loop fan (TRUE if Fan:OnOff)
LOGICAL :: AirLoopNightVent        ! Night Ventilation flag for air loop
REAL(r64) :: MinOAflowfracVal
REAL(r64) :: MaxOAflowfracVal
LOGICAL   :: EconomizerOperationFlag   ! TRUE if OA economizer is active
LOGICAL   :: HighHumidityOperationFlag ! TRUE if zone humidistat senses a high humidity condition
! LOGICAL :: ErrorsFound=.false.    ! Flag identifying errors found during get input

! new local variables for DCV
INTEGER   :: VentMechObjectNum      ! Temporary variable
REAL(r64) :: ZoneOAPeople           ! Zone OA flow rate based on number of occupants
REAL(r64) :: ZoneOAArea             ! Zone OA flow rate based on space floor area
REAL(r64) :: ZoneOAFlow             ! Zone OA flow rate based on simple flow
REAL(r64) :: ZoneOAACH              ! Zone OA flow rate based on air changes per hour
REAL(r64) :: ZoneOABZ               ! Zone breathing-zone OA flow rate
REAL(r64) :: ZoneOAMin              ! Minimum Zone OA flow rate when the zone is unoccupied (i.e. ZoneOAPeople = 0)
                                    ! used for "ProportionalControl" System outdoor air method
REAL(r64) :: ZoneOAMax              ! Maximum Zone OA flow rate (ZoneOAPeople + ZoneOAArea)
                                    ! used for "ProportionalControl" System outdoor air method
REAL(r64) :: ZoneOA                 ! Zone OA flow rate
REAL(r64) :: ZoneOAFrac             ! Zone OA fraction (as a fraction of actual supply air flow rate)
REAL(r64) :: ZoneEz                 ! Zone air distribution effectiveness
REAL(r64) :: ZoneSA                 ! Zone supply air flow rate
REAL(r64) :: ZonePA                 ! Zone primary air flow rate
REAL(r64) :: SysOAuc                ! System uncorrected OA flow rate
REAL(r64) :: SysOA                  ! System supply OA flow rate
REAL(r64) :: SysEv                  ! System ventilation efficiency
REAL(r64) :: SysSA                  ! System supply air flow rate
REAL(r64) :: NodeTemp               ! node temperature
REAL(r64) :: NodeHumRat             ! node humidity ratio
REAL(r64) :: MassFlowRate           ! Temporary variable
REAL(r64) :: ZoneLoad               ! Zone loads
INTEGER   :: InNodeIndex            ! Temporary variable
INTEGER   :: ZoneEquipConfigNum     ! Temporary variable
INTEGER   :: ZoneIndex
INTEGER   :: ZoneNum
INTEGER   :: ZoneADEffSchPtr
CHARACTER(len=MaxNameLength) :: ZoneName ! Zone name
REAL(r64) :: RecircTemp             !- return air temp, used for custom economizer control calculation
REAL(r64) :: MixedAirTempAtMinOAFlow !- mixed air temperature at min flow rate, used for custom economizer control calculation
REAL(r64) :: RecircMassFlowRateAtMinOAFlow ! recirc air mass flow rate at min OA, used for custom economizer control calculation
REAL(r64) :: ReliefMassFlowAtMinOA ! relief air mass flow rate at min OA, used for custom economizer control calculation
INTEGER   :: OAIndex                 ! index to design specification outdoor air objects
INTEGER   :: PeopleNum
REAL(r64) :: ZoneMaxCO2                   ! Breathing-zone CO2 concentartion
REAL(r64) :: ZoneMinCO2                   ! Minimum CO2 concentration in zone
REAL(r64) :: ZoneContamControllerSched    ! Schedule value for ZoneControl:ContaminantController

LOGICAL   :: MultiPath = .FALSE.     ! TRUE if multi-path ventilation system such as dual fan dual duct, VAV with fan-powered box
REAL(r64) :: Ep = 1.0d0                ! zone primary air fraction
REAL(r64) :: Er = 0.0d0                ! zone secondary recirculation fraction
REAL(r64) :: Fa = 1.0d0                ! temporary variable used in multi-path VRP calc
REAL(r64) :: Fb = 1.0d0
REAL(r64) :: Fc = 1.0d0
REAL(r64) :: Xs = 1.0d0                ! uncorrected system outdoor air fraction
REAL(r64) :: Evz = 1.0d0               ! zone ventilation efficiency

INTEGER   :: PriNode                 ! primary node of zone terminal unit
INTEGER   :: InletNode               ! outlet node of zone terminal unit

MinOASchedVal = 1.0d0
ZoneMaxCO2 = 0.d0
ZoneMinCO2 = 0.d0
ZoneOAMin = 0.d0
ZoneOAMax = 0.d0
ZoneContamControllerSched = 0.d0

IF (AirLoopNum > 0) THEN
  AirLoopEconoLockout = AirLoopControlInfo(AirLoopNum)%EconoLockout
  AirLoopCyclingFan = AirLoopControlInfo(AirLoopNum)%CyclingFan
  AirLoopNightVent = AirLoopControlInfo(AirLoopNum)%NightVent
ELSE
  AirLoopEconoLockout = .FALSE.
  AirLoopCyclingFan = .FALSE.
  AirLoopNightVent = .FALSE.
END IF

! Check for no flow
IF (OAController(OAControllerNum)%MixMassFlow .LE. SmallMassFlow) THEN

  OAController(OAControllerNum)%OAMassFlow = 0.0d0        ! outside air mass flow rate
  OAController(OAControllerNum)%RelMassFlow = 0.0d0       ! relief air mass flow rate
  OAController(OAControllerNum)%MixMassFlow = 0.0d0       ! mixed air mass flow rate
  OAController(OAControllerNum)%MinOAFracLimit = 0.0d0    ! minimum OA fraction limit

  OAController(OAControllerNum)%EconomizerStatus = 0   ! economizer status for reporting
  OAController(OAControllerNum)%HeatRecoveryBypassStatus = 0   ! HR bypass status for reporting
  OAController(OAControllerNum)%HRHeatingCoilActive = 0 ! resets report variable
  OAController(OAControllerNum)%MixedAirTempAtMinOAFlow = Node(OAController(OAControllerNum)%RetNode)%Temp ! track return T
  OAController(OAControllerNum)%HighHumCtrlStatus = 0  ! high humdity control status for reporting
  OAController(OAControllerNum)%OAFractionRpt = 0.0d0     ! actual OA fraction for reporting

  OAControllerInfo(OAControllerNum)%EconoActive       = .FALSE. ! DataAirLoop variable (OA Controllers)
  OAControllerInfo(OAControllerNum)%HighHumCtrlActive = .FALSE. ! DataAirLoop variable (OA Controllers)

  ! also reset air loop data for use by other routines
  IF (AirLoopNum > 0) THEN
    AirLoopControlInfo(AirLoopNum)%EconoActive = .FALSE.        ! DataAirLoop variable (AirloopHVAC)
    AirLoopControlInfo(AirLoopNum)%HeatRecoveryBypass = .FALSE. ! DataAirLoop variable (AirloopHVAC)
    AirLoopControlInfo(AirLoopNum)%HighHumCtrlActive = .FALSE.  ! DataAirLoop variable (AirloopHVAC)
    AirLoopControlInfo(AirLoopNum)%ResimAirLoopFlag = .FALSE.   ! DataAirLoop variable (AirloopHVAC)
    AirLoopFlow(AirLoopNum)%OAFrac = 0.d0                       ! DataAirLoop variable (AirloopHVAC)
    AirLoopFlow(AirLoopNum)%OAMinFrac = 0.d0                    ! DataAirLoop variable (AirloopHVAC)
  END IF

  RETURN
END IF

! set OutAirMinFrac
IF (AirLoopNum > 0) THEN
  IF (AirLoopFlow(AirLoopNum)%DesSupply >= SmallAirVolFlow) THEN
    OutAirMinFrac = OAController(OAControllerNum)%MinOAMassFlowRate / AirLoopFlow(AirLoopNum)%DesSupply
  ELSE
    OutAirMinFrac = 0.0d0
  END IF
ELSE
  IF (OAController(OAControllerNum)%MaxOA >= SmallAirVolFlow) THEN
    OutAirMinFrac = OAController(OAControllerNum)%MinOA / OAController(OAControllerNum)%MaxOA
  ELSE
    OutAirMinFrac = 0.0d0
  END IF
END IF
IF (OAController(OAControllerNum)%MinOASchPtr .GT.0) THEN
  MinOASchedVal = GetCurrentScheduleValue(OAController(OAControllerNum)%MinOASchPtr)
  MinOASchedVal = MIN(MAX(MinOASchedVal,0.0d0),1.0d0)
  OutAirMinFrac = OutAirMinFrac * MinOASchedVal
END IF

! Get mechanical ventilation
VentMechObjectNum = OAController(OAControllerNum)%VentMechObjectNum
IF(AirLoopNum > 0 .AND. VentMechObjectNum /= 0)THEN
! Apply mechanical ventilation only when it is available/allowed
  IF(GetCurrentScheduleValue(VentilationMechanical(VentMechObjectNum)%SchPtr) > 0)THEN
    !IF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_ZoneSum) THEN
    ! no longer needed due to OA inputs consolidation
      ! keep simple DCV method implemened in E+ 3.0 which sums the zone OA as the system OA
      !  without considering the zone air distribution effectiveness or system ventilation efficiency
    !  MechVentOutsideAirFlow = (VentilationMechanical(VentMechObjectNum)%TotAreaOAFlow + &
    !    VentilationMechanical(VentMechObjectNum)%TotPeopleOAFlow) * StdRhoAir

    !ELSE IF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_IAQP) THEN
    IF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_IAQP) THEN
      ! IAQP for CO2 control
      SysOA = 0.0d0
      DO ZoneIndex = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
        ZoneNum = VentilationMechanical(VentMechObjectNum)%Zone(ZoneIndex)
        SysOA = SysOA + ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP
      END DO
      MechVentOutsideAirFlow = SysOA
    ELSE IF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_IAQPGC) THEN
      ! IAQP for generic contaminant control
      SysOA = 0.0d0
      DO ZoneIndex = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
        ZoneNum = VentilationMechanical(VentMechObjectNum)%Zone(ZoneIndex)
        SysOA = SysOA + ZoneSysContDemand(ZoneNum)%OutputRequiredToGCSP
      END DO
      MechVentOutsideAirFlow = SysOA
    ELSE IF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_IAQPCOM) THEN
      ! IAQP for both CO2 and generic contaminant control
      SysOA = 0.0d0
      DO ZoneIndex = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
        ZoneNum = VentilationMechanical(VentMechObjectNum)%Zone(ZoneIndex)
        SysOA = SysOA + ZoneSysContDemand(ZoneNum)%OutputRequiredToCO2SP
      END DO
      MechVentOutsideAirFlow = SysOA
      SysOA = 0.0d0
      DO ZoneIndex = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
        ZoneNum = VentilationMechanical(VentMechObjectNum)%Zone(ZoneIndex)
        SysOA = SysOA + ZoneSysContDemand(ZoneNum)%OutputRequiredToGCSP
      END DO
      MechVentOutsideAirFlow = MAX(SysOA, MechVentOutsideAirFlow)
    ELSE
      ! for system OA methods: Zone_Sum, VRP, CO2 methods
      ! new code for DCV method complying with the VRP defined in ASHRAE Standard 62.1-2010

      ! Loop through each zone first to calc uncorrected system OA flow rate
      SysOAuc = 0.0d0
      SysOA = 0.0d0
      DO ZoneIndex = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
        ZoneNum = VentilationMechanical(VentMechObjectNum)%Zone(ZoneIndex)

        ! Calc the zone OA flow rate based on the people component
        ! ZoneIntGain(ZoneNum)%NOFOCC is the number of occupants of a zone at each time step, already counting the occupant schedule
        !  Checking DCV flag before calculating zone OA per person
        IF (VentilationMechanical(VentMechObjectNum)%DCVFlag) THEN
          ZoneOAPeople = ZoneIntGain(ZoneNum)%NOFOCC * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                         VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(ZoneIndex)
        ELSE
          ZoneOAPeople = 0.0d0
          DO PeopleNum=1,TotPeople
            IF (People(PeopleNum)%ZonePtr /= ZoneNum) CYCLE
            ZoneOAPeople = ZoneOAPeople + People(PeopleNum)%NumberOfPeople * Zone(ZoneNum)%Multiplier *   &
               Zone(ZoneNum)%ListMultiplier * VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(ZoneIndex)
          ENDDO
        ENDIF

        ! Calc the zone OA flow rate based on the floor area component
        ZoneOAArea = Zone(ZoneNum)%FloorArea * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                     VentilationMechanical(VentMechObjectNum)%ZoneOAAreaRate(ZoneIndex)
        ZoneOAFlow = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                     VentilationMechanical(VentMechObjectNum)%ZoneOAFlow(ZoneIndex)
        ZoneOAACH =  Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                     (VentilationMechanical(VentMechObjectNum)%ZoneOAACH(ZoneIndex)*Zone(ZoneIndex)%Volume)/3600.d0

        ! Calc the breathing-zone OA flow rate
        OAIndex=VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjIndex(ZoneIndex)
        IF (OAIndex > 0) THEN
          SELECT CASE(OARequirements(OAIndex)%OAFlowMethod)
            CASE(OAFlowPPer)
              ZoneOABZ = ZoneOAPeople
            CASE(OAFlow)
              ZoneOABZ = ZoneOAFlow
            CASE(OAFlowPerArea)
              ZoneOABZ = ZoneOAArea
            CASE(OAFlowACH)
              ZoneOABZ = ZoneOAACH
            CASE(OAFlowSum)
              ZoneOABZ = ZoneOAPeople + ZoneOAArea + ZoneOAFlow + ZoneOAACH
            CASE(OAFlowMax)
              ZoneOABZ = MAX(ZoneOAPeople,ZoneOAArea,ZoneOAFlow,ZoneOAACH)
            CASE DEFAULT
              ZoneOABZ = 0.0D0
          END SELECT
        ELSE
          ZoneOABZ = 0.0D0
        ENDIF

        IF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_ZoneSum) THEN
          ! Sum the zone OA flow rates and done
          SysOA = SysOA + ZoneOABZ
        ELSE
          ! Calc the uncorrected system OA flow rate - VRP and ProportionalControl
          SysOAuc = SysOAuc + ZoneOABZ
        ENDIF
      ENDDO

      ! get system supply air flow rate
      IF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_VRP .OR. &
          VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_ProportionalControl) THEN
        ! Get system supply air flow rate
        IF (AirLoopControlInfo(AirLoopNum)%LoopFlowRateSet) THEN
          ! if flow rate has been specified by a manager, set it to the specified value
          ! DesSupply and SupFlow are mass flow rate in kg/s
          SysSA = AirLoopFlow(AirLoopNum)%ReqSupplyFrac * AirLoopFlow(AirLoopNum)%DesSupply
        ELSE
          SysSA = AirLoopFlow(AirLoopNum)%SupFlow
        ENDIF

        ! System supply air flow rate is always greater than or equal the system outdoor air flow rate
        IF ((SysSA > 0.0d0) .AND. (SysSA < (SysOAuc*StdRhoAir))) SysSA = SysOAuc*StdRhoAir

        ! calc Xs - average outdoor air fraction
        IF (SysSA > 0.0d0) THEN
          Xs = (SysOAuc*StdRhoAir) / SysSA
        ELSE
          Xs = 0.0d0
        ENDIF

        ! Loop through each zone again
        SysEv = 2.0d0 ! starting with a big fraction
        DO ZoneIndex = 1, VentilationMechanical(VentMechObjectNum)%NumofVentMechZones
          ZoneNum = VentilationMechanical(VentMechObjectNum)%Zone(ZoneIndex)
          ZoneName = Zone(ZoneNum)%Name
          ZoneEquipConfigNum = ZoneNum  ! correspondence - 1:1 of ZoneEquipConfig to Zone index
          ZoneEz = 0.0d0

          ! Calc the zone OA flow rate based on the people component
          ! ZoneIntGain(ZoneNum)%NOFOCC is the number of occupants of a zone at each time step, already counting the occupant schedule
          !  Checking DCV flag before calculating zone OA per person
          IF (VentilationMechanical(VentMechObjectNum)%DCVFlag) THEN
            ZoneOAPeople = ZoneIntGain(ZoneNum)%NOFOCC * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                           VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(ZoneIndex)
          ELSE
            ZoneOAPeople = 0.0d0
            DO PeopleNum=1,TotPeople
              IF (People(PeopleNum)%ZonePtr /= ZoneNum) CYCLE
              ZoneOAPeople = ZoneOAPeople + People(PeopleNum)%NumberOfPeople * Zone(ZoneNum)%Multiplier *   &
                Zone(ZoneNum)%ListMultiplier * VentilationMechanical(VentMechObjectNum)%ZoneOAPeopleRate(ZoneIndex)
            ENDDO
          ENDIF

          ! Calc the zone OA flow rate based on the floor area component
          ZoneOAArea = Zone(ZoneNum)%FloorArea * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                       VentilationMechanical(VentMechObjectNum)%ZoneOAAreaRate(ZoneIndex)
          ZoneOAFlow = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                       VentilationMechanical(VentMechObjectNum)%ZoneOAFlow(ZoneIndex)
          ZoneOAACH =  Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier * &
                       (VentilationMechanical(VentMechObjectNum)%ZoneOAACH(ZoneIndex)*Zone(ZoneIndex)%Volume)/3600.d0

          ! Calc the breathing-zone OA flow rate
          OAIndex=VentilationMechanical(VentMechObjectNum)%ZoneDesignSpecOAObjIndex(ZoneIndex)
          IF (OAIndex > 0) THEN
            SELECT CASE(OARequirements(OAIndex)%OAFlowMethod)
              CASE(OAFlowPPer)
                ZoneOABZ = ZoneOAPeople
              CASE(OAFlow)
                ZoneOABZ = ZoneOAFlow
              CASE(OAFlowPerArea)
                ZoneOABZ = ZoneOAArea
              CASE(OAFlowACH)
                ZoneOABZ = ZoneOAACH
              CASE(OAFlowSum)
                ZoneOABZ = ZoneOAPeople + ZoneOAArea + ZoneOAFlow + ZoneOAACH
              CASE(OAFlowMax)
                ZoneOABZ = MAX(ZoneOAPeople,ZoneOAArea,ZoneOAFlow,ZoneOAACH)
              CASE DEFAULT
                ZoneOABZ = 0.0D0
            END SELECT
          ENDIF

          ! use the ventilation rate procedure in ASHRAE Standard 62.1-2007
          ! Calc the zone supplied OA flow rate counting the zone air distribution effectiveness
          !  First check whether the zone air distribution effectiveness schedule exists, if yes uses it;
          !   otherwise uses the inputs of zone distribution effectiveness in cooling mode or heating mode
          ZoneADEffSchPtr = VentilationMechanical(VentMechObjectNum)%ZoneADEffSchPtr(ZoneIndex)
          IF (ZoneADEffSchPtr > 0) THEN
            ! Get schedule value for the zone air distribution effectiveness
            ZoneEz = GetCurrentScheduleValue(ZoneADEffSchPtr)
          ELSE
            ZoneLoad = ZoneSysEnergyDemand(ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum)%TotalOutputRequired

            ! Zone in cooling mode
            IF (ZoneLoad < 0.0d0) ZoneEz = VentilationMechanical(VentMechObjectNum)%ZoneADEffCooling(ZoneIndex)

            ! Zone in heating mode
            IF (ZoneLoad > 0.0d0) ZoneEz = VentilationMechanical(VentMechObjectNum)%ZoneADEffHeating(ZoneIndex)
          ENDIF
          IF (ZoneEz <= 0.0d0) THEN
            !Enforce defaults
            ZoneEz = 1.0d0
          ENDIF

          ! Calc zone supply OA flow rate
          IF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_VRP) THEN
            ! the VRP case
            ZoneOA = ZoneOABZ / ZoneEz

          ELSEIF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_ProportionalControl) THEN
            ! Check whether "Carbon Dioxide Control Availability Schedule" for ZoneControl:ContaminantController is specified
            IF (Zone(ZoneNum)%ZoneContamControllerSchedIndex .GT. 0.d0) THEN
              ! Check the availability schedule value for ZoneControl:ContaminantController
              ZoneContamControllerSched = GetCurrentScheduleValue(Zone(ZoneNum)%ZoneContamControllerSchedIndex)
              IF (ZoneContamControllerSched .GT. 0.d0) THEN
                ZoneOAMin = ZoneOAArea / ZoneEz
                ZoneOAMax = (ZoneOAArea + ZoneOAPeople) / ZoneEz

                IF (ZoneOAPeople .GT. 0.0d0) THEN
                  IF (ZoneCO2GainFromPeople(ZoneNum) .GT. 0.d0) THEN
                    IF (Zone(ZoneNum)%ZoneMinCO2SchedIndex .GT. 0.d0) THEN
                      ! Take the schedule value of "Minimum Carbon Dioxide Concentration Schedule Name"
                      ! in the ZoneControl:ContaminantController
                      ZoneMinCO2 = GetCurrentScheduleValue(Zone(ZoneNum)%ZoneMinCO2SchedIndex)
                    ELSE
                      ZoneMinCO2 = OutdoorCO2
                    ENDIF

                    ! Calculate zone maximum target CO2 concentration in PPM
                    ZoneMaxCO2 = OutdoorCO2 + &
                                (ZoneCO2GainFromPeople(ZoneNum) * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier *1.0d6) &
                                / ZoneOAMax

                    IF (ZoneMaxCO2 .LE. ZoneMinCO2) THEN
                      VentilationMechanical(VentMechObjectNum)%CO2MaxMinLimitErrorCount = &
                                                          VentilationMechanical(VentMechObjectNum)%CO2MaxMinLimitErrorCount+1
                      IF (VentilationMechanical(VentMechObjectNum)%CO2MaxMinLimitErrorCount < 2) THEN
                          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = "'// &
                                                            TRIM(VentilationMechanical(VentMechObjectNum)%Name) //'".')
                          CALL ShowContinueError('For System Outdoor Air Method = ProportionalControl,' // &
                                                 ' maximum target CO2 concentration ('//TRIM(RoundSigDigits(ZoneMaxCO2,2))// &
                                                 '), is not greater than minimum target CO2 concentration (' // &
                                                 TRIM(RoundSigDigits(ZoneMinCO2,2))// ').')
                          CALL ShowContinueError('"ProportionalControl" will not be modeled. ' // &
                                                     'Default "VentilationRateProcedure" will be modeled. Simulation continues...')
                          CALL ShowContinueErrorTimeStamp(' ')
                      ELSE
                          CALL ShowRecurringWarningErrorAtEnd(TRIM(CurrentModuleObject)//' = "'// &
                                            TRIM(VentilationMechanical(VentMechObjectNum)%Name)//&
                                            '", For System Outdoor Air Method = ProportionalControl,' // &
                                            ' maximum target CO2 concentration is not greater than ' // &
                                            ' minimum target CO2 concentration. Error continues...' &
                                            , VentilationMechanical(VentMechObjectNum)%CO2MaxMinLimitErrorIndex)
                      ENDIF

                      ZoneOA = ZoneOABZ / ZoneEz
                    ELSE

                      IF (ZoneAirCO2(ZoneNum) .LE. ZoneMinCO2) THEN
                      ! Zone air CO2 concentration is less than minimum zone CO2 concentration, set the Zone OA flow rate to
                      ! minimum Zone OA flow rate when the zone is unoccupied
                        ZoneOA = ZoneOAMin
                      ELSEIF (ZoneAirCO2(ZoneNum) .GE. ZoneMaxCO2) THEN
                      ! Zone air CO2 concentration is greater than maximum zone CO2 concentration, set the Zone OA flow rate to
                      ! maximum Zone OA flow rate (i.e. ZoneOAArea + ZoneOAPeople)
                        ZoneOA = ZoneOAMax
                      ELSE
                      ! Zone air CO2 concentration is between maximum and minimum limits of zone CO2 concentration,
                      ! set Zone OA flow rate by proportionally adjusting between ZoneOAMin and ZoneOAMax
                        ZoneOA = ZoneOAMin + (ZoneOAMax - ZoneOAMin) * &
                                             ((ZoneAirCO2(ZoneNum) - ZoneMinCO2)/(ZoneMaxCO2 - ZoneMinCO2))
                      ENDIF
                    ENDIF
                  ELSE
                    IF (DisplayExtraWarnings) THEN
                      VentilationMechanical(VentMechObjectNum)%CO2GainErrorCount = &
                                                          VentilationMechanical(VentMechObjectNum)%CO2GainErrorCount+1
                      IF (VentilationMechanical(VentMechObjectNum)%CO2GainErrorCount < 2) THEN
                        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = "'// &
                                                          TRIM(VentilationMechanical(VentMechObjectNum)%Name) //'".')
                        CALL ShowContinueError('For System Outdoor Air Method = ProportionalControl,' // &
                                               ' CO2 generation from people is not greater than zero.' // &
                                               ' Occurs in Zone ="'//TRIM(Zone(ZoneNum)%Name)// &
                                               '". ')
                        CALL ShowContinueError('"ProportionalControl" will not be modeled. ' // &
                                                   'Default "VentilationRateProcedure" will be modeled. Simulation continues...')
                        CALL ShowContinueErrorTimeStamp(' ')
                      ELSE
                        CALL ShowRecurringWarningErrorAtEnd(TRIM(CurrentModuleObject)//' = "'// &
                                           TRIM(VentilationMechanical(VentMechObjectNum)%Name)//&
                                           '", For System Outdoor Air Method = ProportionalControl,' // &
                                           ' CO2 generation from people is not greater than zero ' // &
                                           ' Error continues...' &
                                           , VentilationMechanical(VentMechObjectNum)%CO2GainErrorIndex)
                      ENDIF
                    ENDIF
                    ZoneOA = ZoneOABZ / ZoneEz
                  ENDIF
                ELSE
                  ! ZoneOAPeople is less than or equal to zero
                  ZoneOA = ZoneOABZ / ZoneEz
                ENDIF
              ELSE
                ! ZoneControl:ContaminantController is scheduled off (not available)
                ZoneOA = ZoneOABZ / ZoneEz
              ENDIF
            ELSE
             ! "Carbon Dioxide Control Availability Schedule" for ZoneControl:ContaminantController not found
              ZoneOA = ZoneOABZ / ZoneEz
            ENDIF
          ENDIF

          ! Get the zone supply air flow rate
          ZoneSA = 0.0d0
          ZonePA = 0.0d0
          Ep = 1.0d0
          IF (ZoneEquipConfigNum > 0) THEN
            DO InNodeIndex = 1,ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
              ! Assume primary air is always stored at the AirDistUnitCool (cooling deck if dual duct)
              PriNode = ZoneEquipConfig(ZoneEquipConfigNum)%AirDistUnitCool(InNodeIndex)%InNode
              IF (PriNode > 0) THEN
                NodeTemp = Node(PriNode)%Temp
                NodeHumRat = Node(PriNode)%HumRat
                MassFlowRate = Node(PriNode)%MassFlowRate
              ELSE
                MassFlowRate = 0.0d0
              END IF
              ! total primary air to terminal units of the zone
              IF (MassFlowRate > 0.0d0) ZonePA = ZonePA + MassFlowRate / PsyRhoAirFnPbTdbW(OutBaroPress,NodeTemp,NodeHumRat)

              ! or InletNode = ZoneEquipConfig(ZoneEquipConfigNum)%AirDistUnitCool(InNodeIndex)%OutNode
              InletNode = ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(InNodeIndex)
              IF (InletNode > 0) THEN
                NodeTemp = Node(InletNode)%Temp
                NodeHumRat = Node(InletNode)%HumRat  ! ZoneAirHumRat(ZoneNum)
                MassFlowRate = Node(InletNode)%MassFlowRate
              ELSE
                MassFlowRate = 0.0d0
              END IF
              ! total supply air to the zone
              IF (MassFlowRate > 0.0d0) ZoneSA = ZoneSA + MassFlowRate / PsyRhoAirFnPbTdbW(OutBaroPress,NodeTemp,NodeHumRat)
            END DO

            ! calc zone primary air fraction
            IF (ZoneSA > 0.0d0) Ep = ZonePA / ZoneSA
            IF (Ep > 1.0d0) Ep = 1.0d0
          END IF

          ! Calc the zone OA fraction = Zone OA flow rate / Zone supply air flow rate
          IF (ZoneSA > 0.0d0) THEN
            ZoneOAFrac = ZoneOA / ZoneSA
            ! Zone OA fraction cannot be more than 1
            IF (ZoneOAFrac > 1.0d0) ZoneOAFrac = 1.0d0
          ELSE
            ZoneOAFrac = 0.0d0
          ENDIF

          ! added for TRACE - zone maximum OA fraction - calculate the adjustment factor for the TU/zone supply air flow
          ! only for VRP system OA method
          ZoneSysEnergyDemand(ZoneEquipConfigNum)%SupplyAirAdjustFactor = 1.0D0

          IF (VentilationMechanical(VentMechObjectNum)%SystemOAMethod == SOAM_VRP) THEN
            IF (ZoneOAFrac > VentilationMechanical(VentMechObjectNum)%ZoneMaxOAFraction) THEN
              IF (VentilationMechanical(VentMechObjectNum)%ZoneMaxOAFraction > 0.0d0) THEN
                ZoneSysEnergyDemand(ZoneEquipConfigNum)%SupplyAirAdjustFactor =  &
                 ZoneOAFrac / VentilationMechanical(VentMechObjectNum)%ZoneMaxOAFraction
              ELSE
                ZoneSysEnergyDemand(ZoneEquipConfigNum)%SupplyAirAdjustFactor = 1.0D0
              ENDIF

              ! cap zone OA fraction at the maximum specified
              ZoneOAFrac = VentilationMechanical(VentMechObjectNum)%ZoneMaxOAFraction
            ENDIF
          ENDIF

          ! Zone air secondary recirculation fraction
          Er = VentilationMechanical(VentMechObjectNum)%ZoneSecondaryRecirculation(ZoneIndex)
          IF (Er > 0.0d0) THEN
            ! multi-path ventilation system using VRP
            Fa = Ep + (1.0d0 - Ep) * Er
            Fb = Ep
            Fc = 1.0d0 - (1.0d0 - ZoneEz)*(1.0d0 - Er)*(1.0d0 - Ep)

            ! Calc zone ventilation efficiency
            IF (Fa > 0.0d0) THEN
              Evz = 1.0d0 + Xs * Fb / Fa - ZoneOAFrac * Ep * Fc / Fa
            ELSE
              Evz = 1.0d0
            ENDIF
          ELSE
            ! single-path ventilation system
            Evz = 1.0d0 + Xs - ZoneOAFrac
          ENDIF

          ! calc system ventilation efficiency = Minimum of zone ventilation efficiency
          IF (Evz < 0.0d0) Evz = 0.0d0
          IF (Evz < SysEv) SysEv = Evz

        ENDDO  ! zone loop

        ! Calc the system supply OA flow rate counting the system ventilation efficiency
        IF (SysEv <= 0.0d0) SysEv = 1.0d0

        ! Calc system outdoor air requirement
        SysOA = SysOAuc / SysEv
      ENDIF

      ! Finally calc the system supply OA mass flow rate
      MechVentOutsideAirFlow = SysOA * StdRhoAir
    ENDIF

    MechVentOutsideAirMinFrac = MechVentOutsideAirFlow / AirLoopFlow(AirLoopNum)%DesSupply
  ELSE
    MechVentOutsideAirMinFrac = 0.0d0
    MechVentOutsideAirFlow    = 0.0d0
  END IF

  IF(AirLoopflow(AirLoopNum)%FanPLR .GT. 0.0D0)THEN
    MechVentOutsideAirMinFrac = MechVentOutsideAirMinFrac * AirLoopflow(AirLoopNum)%FanPLR
    MechVentOutsideAirFlow    = MechVentOutsideAirFlow * AirLoopflow(AirLoopNum)%FanPLR
  END IF
!****** use greater of Mechanical Ventilation Outside Air fraction and OutAirMinFrac
  OutAirMinFrac = MAX(OutAirMinFrac,MechVentOutsideAirMinFrac)
END IF

OutAirMinFrac = MIN(MAX(OutAirMinFrac,0.0d0),1.0d0)
IF (AirLoopNum > 0) THEN
  AirLoopFlow(AirLoopNum)%MinOutAir = OutAirMinFrac * AirLoopFlow(AirLoopNum)%DesSupply
END IF

! Define an outside air signal
IF (ABS(OAController(OAControllerNum)%RetTemp - OAController(OAControllerNum)%InletTemp) .GT. SmallTempDiff) THEN
  OutAirSignal = (OAController(OAControllerNum)%RetTemp - OAController(OAControllerNum)%MixSetTemp) &
                   / (OAController(OAControllerNum)%RetTemp - OAController(OAControllerNum)%InletTemp)
ELSE
  IF (OAController(OAControllerNum)%RetTemp - OAController(OAControllerNum)%MixSetTemp .LT. 0.0d0) THEN
    IF (OAController(OAControllerNum)%RetTemp - OAController(OAControllerNum)%InletTemp .GE. 0.0d0) THEN
      OutAirSignal = -1.d0
    ELSE
      OutAirSignal = 1.d0
    ENDIF
  ELSE
    IF (OAController(OAControllerNum)%RetTemp - OAController(OAControllerNum)%InletTemp .GE. 0.0d0) THEN
      OutAirSignal = 1.d0
    ELSE
      OutAirSignal = -1.d0
    ENDIF
  ENDIF
ENDIF
OutAirSignal = MIN(MAX(OutAirSignal,OutAirMinFrac),1.0d0)

! If no economizer, set to minimum and disable economizer and high humidity control
IF (OAController(OAControllerNum)%Econo .EQ. NoEconomizer) THEN
  OutAirSignal = OutAirMinFrac
  EconomizerOperationFlag = .FALSE.
  EconomizerAirFlowScheduleValue = 0.0d0
  HighHumidityOperationFlag = .FALSE.
ELSE IF (OAController(OAControllerNum)%MaxOA < SmallAirVolFlow) THEN
  OutAirSignal = OutAirMinFrac
  EconomizerOperationFlag = .FALSE.
  EconomizerAirFlowScheduleValue = 0.0d0
  HighHumidityOperationFlag = .FALSE.
ELSE IF (AirLoopEconoLockout) THEN
  OutAirSignal = OutAirMinFrac
  EconomizerOperationFlag = .FALSE.
  EconomizerAirFlowScheduleValue = 0.0d0
  HighHumidityOperationFlag = .FALSE.
ELSE
!Changed by Amit for new implementation
! Otherwise do the limit checks
  EconomizerOperationFlag = .TRUE.
  ! Outside air temp greater than mix air setpoint
  IF (OAController(OAControllerNum)%InletTemp.GT.OAController(OAControllerNum)%MixSetTemp) THEN
    OutAirSignal = 1.0d0
  ENDIF
  ! Return air temp limit
  IF (OAController(OAControllerNum)%Econo .EQ. DifferentialDryBulb) THEN
     If(OAController(OAControllerNum)%InletTemp.GT.OAController(OAControllerNum)%RetTemp) THEN
        OutAirSignal = OutAirMinFrac
        EconomizerOperationFlag = .FALSE.
     End if
     Call Checksetpoints(OAControllerNum,OutAirMinFrac,OutAirSignal,EconomizerOperationFlag)
  ENDIF
  ! Return air enthalpy limit
  IF (OAController(OAControllerNum)%Econo.EQ. DifferentialEnthalpy) THEN
     IF(OAController(OAControllerNum)%InletEnth.GT.OAController(OAControllerNum)%RetEnth) THEN
        OutAirSignal = OutAirMinFrac
        EconomizerOperationFlag = .FALSE.
     ENDIF
     Call Checksetpoints(OAControllerNum,OutAirMinFrac,OutAirSignal,EconomizerOperationFlag)
  END IF
  ! Outside air temperature limit
  IF (OAController(OAControllerNum)%Econo .EQ. FixedDryBulb) THEN
     Call Checksetpoints(OAControllerNum,OutAirMinFrac,OutAirSignal,EconomizerOperationFlag)
  END IF
  !Fixed Enthalpy limit
  IF (OAController(OAControllerNum)%Econo .EQ. FixedEnthalpy) THEN
      Call Checksetpoints(OAControllerNum,OutAirMinFrac,OutAirSignal,EconomizerOperationFlag)
  END IF
  !FIXED DEW POINT AND DRY BULB TEMPERATURE STRATEGY
  IF(OAController(OAControllernum)%Econo .EQ. FixedDewpointAndDryBulb) THEN
      Call Checksetpoints(OAControllerNum,OutAirMinFrac,OutAirSignal,EconomizerOperationFlag)
  END IF
  ! ELECRONIC ENTHALPY, HUMIDITY RATIO CURVE
  IF(OAController(OAControllernum)%Econo .EQ. ElectronicEnthalpy) THEN
      Call Checksetpoints(OAControllerNum,OutAirMinFrac,OutAirSignal,EconomizerOperationFlag)
  END IF
  ! Differential dry bulb and enthalpy strategy
  IF(OAController(OAControllerNum)%Econo .EQ. DifferentialDryBulbAndEnthalpy) THEN
     If(OAController(OAControllerNum)%InletTemp.GT.OAController(OAControllerNum)%RetTemp) THEN
        OutAirSignal = OutAirMinFrac
        EconomizerOperationFlag = .FALSE.
     End if
     IF(OAController(OAControllerNum)%InletEnth.GT.OAController(OAControllerNum)%RetEnth) THEN
        OutAirSignal = OutAirMinFrac
        EconomizerOperationFlag = .FALSE.
     ENDIF
     Call Checksetpoints(OAControllerNum,OutAirMinFrac,OutAirSignal,EconomizerOperationFlag)
  END IF

  IF (OAController(OAControllerNum)%TempLowLim /= BlankNumeric .AND. OAController(OAControllerNum)%OATemp &
                                                           .LT.OAController(OAControllerNum)%TempLowLim) THEN
    OutAirSignal = OutAirMinFrac
    EconomizerOperationFlag = .FALSE.
  END IF
  ! Increase air flow for humidity control
  ! (HumidistatZoneNum is greater than 0 IF High Humidity Control Flag = YES, checked in GetInput)
  IF(OAController(OAControllerNum)%HumidistatZoneNum .GT. 0)THEN
!   IF humidistat senses a moisture load check to see if modifying air flow is appropriate, otherwise disable modified air flow
    IF(ZoneSysMoistureDemand(OAController(OAControllerNum)%HumidistatZoneNum)%TotalOutputRequired .LT. 0.0d0)THEN
!     IF OAController is not allowed to modify air flow during high outdoor humrat condition, then disable modified air flow
!     if indoor humrat is less than or equal to outdoor humrat
      IF(.NOT. OAController(OAControllerNum)%ModifyDuringHighOAMoisture .AND. &
         Node(OAController(OAControllerNum)%NodeNumofHumidistatZone)%HumRat .LE. OAController(OAControllerNum)%OAHumRat)THEN
        HighHumidityOperationFlag = .FALSE.
      ELSE
        HighHumidityOperationFlag = .TRUE.
      END IF
    ELSE
      HighHumidityOperationFlag = .FALSE.
    END IF
  ELSE
    HighHumidityOperationFlag = .FALSE.
  END IF

! Check time of day economizer schedule, enable economizer if schedule value > 0
  EconomizerAirFlowScheduleValue = 0.0d0
  IF(OAController(OAControllerNum)%EconomizerOASchedPtr .GT. 0)THEN
    EconomizerAirFlowScheduleValue = GetCurrentScheduleValue(OAController(OAControllerNum)%EconomizerOASchedPtr)
    IF(EconomizerAirFlowScheduleValue .GT. 0.0d0) THEN
      EconomizerOperationFlag = .TRUE.
      OutAirSignal = 1.0d0
    END IF
  END IF

END IF

! OutAirSignal will not give exactly the correct mixed air temperature (equal to the setpoint) since
! it was calculated using the approximate method of sensible energy balance. Now we have to get the
! accurate result using a full mass, enthalpy and moisture balance and iteration.
IF ( OutAirSignal > OutAirMinFrac .AND. OutAirSignal < 1.0d0 .AND. &
     OAController(OAControllerNum)%MixMassFlow > VerySmallMassFlow .AND. &
     OAController(OAControllerNum)%ControllerType_Num == ControllerOutsideAir .AND. &
     .NOT. AirLoopNightVent) THEN
  Par(1) = OAController(OAControllerNum)%MixNode
  Par(2) = OAController(OAControllerNum)%RetNode
  Par(3) = OAController(OAControllerNum)%InletNode
  Par(4) = OAController(OAControllerNum)%MixMassFlow
  CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, OASignal, MixedAirControlTempResidual, OutAirMinFrac, 1.0d0, Par)
  IF (SolFla < 0) THEN
    OASignal = OutAirSignal
  END IF
ELSE
  OASignal = OutAirSignal
END IF

! Economizer choice "Bypass" forces minimum OA except when high humidity air flow is active based on indoor RH
IF (OAController(OAControllerNum)%EconBypass .AND. &
    EconomizerAirFlowScheduleValue .EQ. 0.0d0)THEN
 OASignal = OutAirMinFrac
END IF

! Set outdoor air signal based on OA flow ratio if high humidity air flow is enabled
IF(HighHumidityOperationFlag) THEN
  IF(OAController(OAControllerNum)%MixMassFlow .GT. 0.0d0)THEN
!   calculate the actual ratio of outside air to mixed air so the magnitude of OA during high humidity control is correct
    OASignal = MAX(OutAirMinFrac, &
                  (OAController(OAControllerNum)%HighRHOAFlowRatio*OAController(OAControllerNum)%MaxOAMassFlowRate/ &
                   OAController(OAControllerNum)%MixMassFlow))
  END IF
END IF

! Night ventilation control overrides economizer and high humidity control.
IF (AirLoopNightVent)OASignal = 1.0d0

! Changed by Amit for new feature
IF (OAController(OAControllerNum)%MinOAflowSchPtr .GT.0) THEN
  MinOAflowfracVal = GetCurrentScheduleValue(OAController(OAControllerNum)%MinOAflowSchPtr)
  MinOAflowfracVal = MIN(MAX(MinOAflowfracVal,0.0d0),1.0d0)
  IF(MinOAflowfracVal .GT. OutAirMinFrac)THEN
    OutAirMinFrac = MinOAflowfracVal
    IF(AirLoopNum > 0) &
    AirLoopFlow(AirLoopNum)%MinOutAir = OutAirMinFrac * OAController(OAControllerNum)%MixMassFlow
  END IF
  OASignal = Max(MinOAflowfracVal, OASignal)
END IF

IF (OAController(OAControllerNum)%MaxOAflowSchPtr .GT.0) THEN
  MaxOAflowfracVal = GetCurrentScheduleValue(OAController(OAControllerNum)%MaxOAflowSchPtr)
  MaxOAflowfracVal = MIN(MAX(MaxOAflowfracVal,0.0d0),1.0d0)
  IF(MaxOAflowfracVal .LT. OutAirMinFrac)THEN
    OutAirMinFrac = MaxOAflowfracVal
    IF(AirLoopNum > 0) &
    AirLoopFlow(AirLoopNum)%MinOutAir = OutAirMinFrac * OAController(OAControllerNum)%MixMassFlow
  END IF
  OASignal = Min(MaxOAflowfracVal, OASignal)

  IF(OAController(OAControllerNum)%MinOAflowSchPtr .GT.0) THEN
    IF (MaxOAflowfracVAl .LT. MinOAFlowfracval) THEN
      Call Showwarningerror('Min OA flow frac Greater than Max OA flow frac - check the Schedules in "Controller:OutdoorAir " ' &
                      //TRIM(OAController(OAControllerNum)%MinOAflowSch)//TRIM(OAController(OAControllerNum)%MaxOAflowSch))
    END IF
  END IF

END IF

! Calculate the outside air mass flow rate
!IF (OAController(OAControllerNum)%FixedMin) Then
 !IF(AirloopNum  > 0) THEN
 !   OAController(OAControllerNum)%OAMassflow = OASignal * AirLoopFlow(AirLoopNum)%DesSupply
! ELSE ! No Air Loop
 !   CALL Showsevereerror('Fixed minimum limit works only with Air loop defined')
!    Errorsfound =.TRUE.
 !END IF
!ELSE ! Its Propotional Minimum
 OAController(OAControllerNum)%OAMassflow = OASignal* OAController(OAControllerNum)%MixMassFlow
!END IF


! Do not allow OA to be below Ventilation:Mechanical flow rate or above mixed mass flow rate
IF(AirLoopNum > 0 .AND. VentMechObjectNum /= 0)THEN
  IF(MechVentOutsideAirFlow .GT. OAController(OAControllerNum)%OAMassFlow)THEN
    OAController(OAControllerNum)%OAMassFlow = MIN(MechVentOutsideAirFlow,OAController(OAControllerNum)%MixMassFlow)
  END IF
END IF

! Do not allow OA to be below Exh for controller:outside air
IF(OAController(OAControllerNum)%ControllerType_Num == ControllerOutsideAir)THEN
 OAController(OAControllerNum)%OAMassFlow = MAX(OAController(OAControllerNum)%ExhMassFlow,&
                                                 OAController(OAControllerNum)%OAMassFlow)
END IF

! if fixed minimum, don't let go below min OA
IF (OAController(OAControllerNum)%FixedMin) THEN
! cycling fans allow "average" min OA to be below minimum
  IF (.NOT. AirLoopCyclingFan) THEN
    OAController(OAControllerNum)%OAMassFlow = MAX(OAController(OAControllerNum)%OAMassFlow,&
                                                   OAController(OAControllerNum)%MinOAMassFlowRate * MinOASchedVal)
  END IF
END IF

! Don't let OA flow be > mixed air flow.
  OAController(OAControllerNum)%OAMassFlow = MIN(OAController(OAControllerNum)%OAMassFlow, &
                                               OAController(OAControllerNum)%MixMassFlow)

! Don't let the OA flow be > than the max OA limit. OA for high humidity control is allowed to be greater than max OA.
! Night Ventilation has priority and may override an OASignal > 1 high humidity condition with OASignal = 1
IF(HighHumidityOperationFlag)THEN
  OAController(OAControllerNum)%OAMassFlow = MIN(OAController(OAControllerNum)%OAMassFlow,&
                                               OAController(OAControllerNum)%MaxOAMassFlowRate*MAX(1.0d0,OASignal))
ELSE
  OAController(OAControllerNum)%OAMassFlow = MIN(OAController(OAControllerNum)%OAMassFlow,&
                                               OAController(OAControllerNum)%MaxOAMassFlowRate)
END IF

IF (OAController(OAControllerNum)%EMSOverrideOARate) THEN
  OAController(OAControllerNum)%OAMassFlow = OAController(OAControllerNum)%EMSOARateValue
ENDIF

! save the min outside air flow fraction and max outside air mass flow rate
IF (AirLoopNum > 0) THEN
  AirLoopFlow(AirLoopNum)%OAMinFrac = OutAirMinFrac
  IF(OAController(OAControllerNum)%MixMassFlow .GT. 0.0d0)THEN
    AirLoopFlow(AirLoopNum)%OAFrac = OAController(OAControllerNum)%OAMassflow / OAController(OAControllerNum)%MixMassFlow
  ELSE
    AirLoopFlow(AirLoopNum)%OAFrac = 0.0D0
  END IF
  OAController(OAControllerNum)%MinOAFracLimit = OutAirMinFrac
  IF(HighHumidityOperationFlag .AND. OASignal .GT. 1.0d0)THEN
    AirLoopFlow(AirLoopNum)%MaxOutAir = OAController(OAControllerNum)%MaxOAMassFlowRate * OASignal
  ELSE
    AirLoopFlow(AirLoopNum)%MaxOutAir = OAController(OAControllerNum)%MaxOAMassFlowRate
  END IF

  ! set the air loop economizer and high humidity control flags.
  IF (EconomizerOperationFlag) THEN

    IF(OAController(OAControllerNum)%HeatRecoveryBypassControlType == BypassWhenOAFlowGreaterThanMinimum)THEN
      ! Optional heat recovery bypass control flag uses additional logic to allow ERV optimization
      IF(AirLoopControlInfo(AirLoopNum)%CheckHeatRecoveryBypassStatus .AND. &
         AirLoopControlInfo(AirLoopNum)%OASysComponentsSimulated)THEN

        ReliefMassFlowAtMinOA = MAX(AirLoopFlow(AirLoopNum)%MinOutAir &
                                      - OAController(OAControllerNum)%ExhMassFlow,0.0d0)
        RecircMassFlowRateAtMinOAFlow = MAX(Node(OAController(OAControllerNum)%RetNode)%MassFlowRate &
                                      - ReliefMassFlowAtMinOA,0.0d0)
        IF((RecircMassFlowRateAtMinOAFlow+AirLoopFlow(AirLoopNum)%MinOutAir) .GT. 0.0D0)THEN
          RecircTemp = Node(OAController(OAControllerNum)%RetNode)%Temp
          MixedAirTempAtMinOAFlow = (RecircMassFlowRateAtMinOAFlow*RecircTemp + AirLoopFlow(AirLoopNum)%MinOutAir*&
                  Node(OAController(OAControllerNum)%OANode)%Temp) / &
                 (RecircMassFlowRateAtMinOAFlow+AirLoopFlow(AirLoopNum)%MinOutAir)
        ELSE
          MixedAirTempAtMinOAFlow = Node(OAController(OAControllerNum)%RetNode)%Temp
        END IF
        OAController(OAControllerNum)%MixedAirTempAtMinOAFlow = MixedAirTempAtMinOAFlow
        IF (OAController(OAControllerNum)%OAMassFlow > AirLoopFlow(AirLoopNum)%MinOutAir) THEN
          AirLoopControlInfo(AirLoopNum)%HeatRecoveryBypass = .TRUE.
          OAController(OAControllerNum)%HeatRecoveryBypassStatus = 1
        ELSE
          AirLoopControlInfo(AirLoopNum)%HeatRecoveryBypass = .FALSE.
          OAController(OAControllerNum)%HeatRecoveryBypassStatus = 0
        END IF
        IF (MixedAirTempAtMinOAFlow .LE. Node(OAController(OAControllerNum)%MixNode)%TempSetPoint) THEN
          AirLoopControlInfo(AirLoopNum)%EconomizerFlowLocked = .TRUE.
          OAController(OAControllerNum)%OAMassFlow = AirLoopFlow(AirLoopNum)%MinOutAir
          AirLoopFlow(AirLoopNum)%OAFrac = OAController(OAControllerNum)%OAMassflow / OAController(OAControllerNum)%MixMassFlow
          AirLoopFlow(AirLoopNum)%OAMinFrac = AirLoopFlow(AirLoopNum)%OAFrac
        ELSE ! IF (MixedAirTempAtMinOAFlow .LE. Node(OAController(OAControllerNum)%MixNode)%TempSetPoint) THEN
          AirLoopControlInfo(AirLoopNum)%EconomizerFlowLocked = .FALSE.
          OAController(OAControllerNum)%HRHeatingCoilActive = 0
        ENDIF ! IF (MixedAirTempAtMinOAFlow .LE. Node(OAController(OAControllerNum)%MixNode)%TempSetPoint) THEN
        AirLoopControlInfo(AirLoopNum)%CheckHeatRecoveryBypassStatus = .FALSE.
      END IF ! IF(AirLoopControlInfo(AirLoopNum)%CheckHeatRecoveryBypassStatus .AND. &
    ELSE ! IF(OAController(OAControllerNum)%HeatRecoveryBypassControlType == BypassWhenOAFlowGreaterThanMinimum)THEN
      AirLoopControlInfo(AirLoopNum)%HeatRecoveryBypass   = .TRUE.
      OAController(OAControllerNum)%HeatRecoveryBypassStatus = 1
    END IF ! IF(OAController(OAControllerNum)%HeatRecoveryBypassControlType == BypassWhenOAFlowGreaterThanMinimum)THEN

  ELSE ! IF (EconomizerOperationFlag) THEN
    AirLoopControlInfo(AirLoopNum)%HeatRecoveryBypass     = .FALSE.
    OAController(OAControllerNum)%HeatRecoveryBypassStatus = 0
    OAController(OAControllerNum)%MixedAirTempAtMinOAFlow = Node(OAController(OAControllerNum)%RetNode)%Temp
  ENDIF ! IF (EconomizerOperationFlag) THEN

  AirLoopControlInfo(AirLoopNum)%EconoActive = EconomizerOperationFlag
  AirLoopControlInfo(AirLoopNum)%HighHumCtrlActive = HighHumidityOperationFlag
  IF(AirLoopControlInfo(AirLoopNum)%EconomizerFlowLocked)THEN
    OAController(OAControllerNum)%OAMassFlow = AirLoopFlow(AirLoopNum)%MinOutAir
    AirLoopFlow(AirLoopNum)%OAFrac = OAController(OAControllerNum)%OAMassflow / OAController(OAControllerNum)%MixMassFlow
    AirLoopFlow(AirLoopNum)%OAMinFrac = AirLoopFlow(AirLoopNum)%OAFrac
  END IF

  ! turn on OA heat exchanger any time heating is active and user requests the special bypass control
  IF(AirLoopControlInfo(AirLoopNum)%HeatingActiveFlag .AND. &
      OAController(OAControllerNum)%HeatRecoveryBypassControlType == BypassWhenOAFlowGreaterThanMinimum)THEN
    AirLoopControlInfo(AirLoopNum)%HeatRecoveryBypass = .FALSE.
    OAController(OAControllerNum)%HeatRecoveryBypassStatus = 0
    OAController(OAControllerNum)%HRHeatingCoilActive = 1
    ! reset the OA flow to minimum
    OAController(OAControllerNum)%OAMassFlow = AirLoopFlow(AirLoopNum)%MinOutAir
    AirLoopFlow(AirLoopNum)%OAFrac = OAController(OAControllerNum)%OAMassflow / OAController(OAControllerNum)%MixMassFlow
    AirLoopFlow(AirLoopNum)%OAMinFrac = AirLoopFlow(AirLoopNum)%OAFrac

    ! The airloop needs to be simulated again so that the heating coil & HX can be resimulated
    IF(AirLoopControlInfo(AirLoopNum)%HeatRecoveryResimFlag .AND. AirLoopControlInfo(AirLoopNum)%OASysComponentsSimulated)THEN
      AirLoopControlInfo(AirLoopNum)%ResimAirLoopFlag = .TRUE.
      AirLoopControlInfo(AirLoopNum)%HeatRecoveryResimFlag = .FALSE.
      AirLoopControlInfo(AirLoopNum)%HeatRecoveryResimFlag2 = .TRUE.
      ! on the first iteration, air loop heating coils have not be simulated so HeatingCoilActive=FALSE
      ! on the second iteration, the heating coils could have been on, but logic tests here could deactivate heating coil
      ! reset heating coil active status and HX since logic tests may turn off heating coil
      ! the ResimAirLoopFlag will force another iteration and things should line up on subsequent iterations
      AirLoopControlInfo(AirLoopNum)%HeatingActiveFlag = .FALSE.
      OAController(OAControllerNum)%HRHeatingCoilActive = 0
      AirLoopControlInfo(AirLoopNum)%HeatRecoveryBypass = .TRUE.
      OAController(OAControllerNum)%HeatRecoveryBypassStatus = 1
    ELSE IF(AirLoopControlInfo(AirLoopNum)%HeatRecoveryResimFlag2)THEN
      AirLoopControlInfo(AirLoopNum)%ResimAirLoopFlag = .TRUE.
      AirLoopControlInfo(AirLoopNum)%HeatRecoveryResimFlag2 = .FALSE.
    ELSE
      AirLoopControlInfo(AirLoopNum)%ResimAirLoopFlag = .FALSE.
    END IF
  ELSE ! IF(AirLoopControlInfo(AirLoopNum)%HeatingActiveFlag)THEN
    OAController(OAControllerNum)%HRHeatingCoilActive = 0
  END IF ! IF(AirLoopControlInfo(AirLoopNum)%HeatingActiveFlag)THEN

END IF ! IF (AirLoopNum > 0) THEN

! Set the relief air flow rate (must be done last to account for changes in OAMassflow
OAController(OAControllerNum)%RelMassFlow = MAX(OAController(OAControllerNum)%OAMassFlow &
                                              - OAController(OAControllerNum)%ExhMassFlow,0.0d0)

! Set economizer report variable and status flag
IF (OAController(OAControllerNum)%Econo .EQ. NoEconomizer)THEN
  ! No economizer
  OAController(OAControllerNum)%EconomizerStatus = 0
  OAControllerInfo(OAControllerNum)%EconoActive = .FALSE.
ELSE
  ! With economizer.
  IF (EconomizerOperationFlag) THEN
    ! Economizer is enabled
    OAController(OAControllerNum)%EconomizerStatus = 1
    OAControllerInfo(OAControllerNum)%EconoActive = .TRUE.
  ELSE
    ! Economizer is disabled
    OAController(OAControllerNum)%EconomizerStatus = 0
    OAControllerInfo(OAControllerNum)%EconoActive = .FALSE.
  ENDIF
ENDIF

! Set high humidity control report variable and status flag
IF(HighHumidityOperationFlag)THEN
  OAController(OAControllerNum)%HighHumCtrlStatus = 1
  OAControllerInfo(OAControllerNum)%HighHumCtrlActive = .TRUE.
ELSE
  OAController(OAControllerNum)%HighHumCtrlStatus = 0
  OAControllerInfo(OAControllerNum)%HighHumCtrlActive = .FALSE.
END IF

! Save OA fraction for reporting
IF (OAController(OAControllerNum)%MixMassFlow > 0) THEN
  OAController(OAControllerNum)%OAFractionRpt = OAController(OAControllerNum)%OAMassflow / OAController(OAControllerNum)%MixMassFlow
ELSE
  IF (OAController(OAControllerNum)%OAMassflow > 0) THEN
    OAController(OAControllerNum)%OAFractionRpt = OASignal
  ELSE
    OAController(OAControllerNum)%OAFractionRpt = 0.0d0
  ENDIF
ENDIF

! IF (ErrorsFound) THEN
!   CALL ShowFatalError('Errors found in getting Controller:OutdoorAir inputs')
! ENDIF

RETURN
END SUBROUTINE CalcOAController

SUBROUTINE CalcOAMixer(OAMixerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Calculate the mixed air flow and conditions

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
USE Psychrometrics, ONLY:PsyTdbFnHW

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OAMixerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: RecircMassFlowRate
REAL(r64) :: RecircPressure
REAL(r64) :: RecircEnthalpy
REAL(r64) :: RecircHumRat

! Define a recirculation mass flow rate
RecircMassFlowRate = OAMixer(OAMixerNum)%RetMassFlowRate - OAMixer(OAMixerNum)%RelMassFlowRate
!In certain low flow conditions the return air mass flow rate can be below the outside air value established
!  by the user.  This check will ensure that this condition does not result in unphysical air properties.
If(RecircMassFlowRate < 0.0d0)THEN
  RecircMassFlowRate = 0.0d0
  OAMixer(OAMixerNum)%RelMassFlowRate = OAMixer(OAMixerNum)%RetMassFlowRate
END IF

! Pass through the return air conditions to the relief air stream.  The return air is "split" to
! the relief air and the recirculation air.
OAMixer(OAMixerNum)%RelTemp = OAMixer(OAMixerNum)%RetTemp
OAMixer(OAMixerNum)%RelHumRat = OAMixer(OAMixerNum)%RetHumRat
OAMixer(OAMixerNum)%RelEnthalpy = OAMixer(OAMixerNum)%RetEnthalpy
OAMixer(OAMixerNum)%RelPressure = OAMixer(OAMixerNum)%RetPressure
RecircPressure = OAMixer(OAMixerNum)%RetPressure
RecircEnthalpy = OAMixer(OAMixerNum)%RetEnthalpy
RecircHumRat = OAMixer(OAMixerNum)%RetHumRat
! The recirculation air and the outside air are mixed to form the mixed air stream
OAMixer(OAMixerNum)%MixMassFlowRate = OAMixer(OAMixerNum)%OAMassFlowRate + RecircMassFlowRate
! Check for zero flow
IF (OAMixer(OAMixerNum)%MixMassFlowRate <= VerySmallMassFlow) THEN
  OAMixer(OAMixerNum)%MixEnthalpy = OAMixer(OAMixerNum)%RetEnthalpy
  OAMixer(OAMixerNum)%MixHumRat = OAMixer(OAMixerNum)%RetHumRat
  OAMixer(OAMixerNum)%MixPressure = OAMixer(OAMixerNum)%RetPressure
  OAMixer(OAMixerNum)%MixTemp = OAMixer(OAMixerNum)%RetTemp
  RETURN
END IF

OAMixer(OAMixerNum)%MixEnthalpy = (RecircMassFlowRate*RecircEnthalpy + OAMixer(OAMixerNum)%OAMassFlowRate*&
                                   OAMixer(OAMixerNum)%OAEnthalpy) / OAMixer(OAMixerNum)%MixMassFlowRate
OAMixer(OAMixerNum)%MixHumRat = (RecircMassFlowRate*RecircHumRat + OAMixer(OAMixerNum)%OAMassFlowRate*&
                                 OAMixer(OAMixerNum)%OAHumRat) / OAMixer(OAMixerNum)%MixMassFlowRate
OAMixer(OAMixerNum)%MixPressure = (RecircMassFlowRate*RecircPressure + OAMixer(OAMixerNum)%OAMassFlowRate*&
                                   OAMixer(OAMixerNum)%OAPressure) / OAMixer(OAMixerNum)%MixMassFlowRate
! Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
OAMixer(OAMixerNum)%MixTemp = PsyTdbFnHW(OAMixer(OAMixerNum)%MixEnthalpy,OAMixer(OAMixerNum)%MixHumRat)

RETURN
END SUBROUTINE CalcOAMixer

! End of Calculation/Simulation Section of the Module
!******************************************************************************

! Beginning Sizing Section of the Module
!******************************************************************************

SUBROUTINE SizeOAController(OAControllerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing OAController Components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE InputProcessor, ONLY: SameString
  USE HVACHXAssistedCoolingCoil, ONLY: GetHXDXCoilName, GetHXCoilType
  USE WaterCoils,     ONLY: SetCoilDesFlow

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: OAControllerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject=CurrentModuleObjects(CMO_OAController)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: OAFlowRatio  ! Used for error checking
  CHARACTER(LEN=MaxNameLength)  :: CompType     ! Component type
  CHARACTER(LEN=MaxNameLength)  :: CompName     ! Component name
  CHARACTER(len=MaxNameLength)  :: CoilName
  CHARACTER(len=MaxNameLength)  :: CoilType
  INTEGER                       :: CompNum
  LOGICAL             :: ErrorsFound

  ErrorsFound = .FALSE.
  IF (OAController(OAControllerNum)%MaxOA == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      SELECT CASE(OAController(OAControllerNum)%ControllerType_Num)

      CASE(ControllerOutsideAir)

        CALL CheckSysSizing(CurrentModuleObject,OAController(OAControllerNum)%Name)

        SELECT CASE(CurDuctType)
          CASE(Main)
            OAController(OAControllerNum)%MaxOA = FinalSysSizing(CurSysNum)%DesMainVolFlow
          CASE(Cooling)
            OAController(OAControllerNum)%MaxOA = FinalSysSizing(CurSysNum)%DesCoolVolFlow
          CASE(Heating)
            OAController(OAControllerNum)%MaxOA = FinalSysSizing(CurSysNum)%DesHeatVolFlow
          CASE(Other)
            OAController(OAControllerNum)%MaxOA = FinalSysSizing(CurSysNum)%DesMainVolFlow
          CASE DEFAULT
            OAController(OAControllerNum)%MaxOA = FinalSysSizing(CurSysNum)%DesMainVolFlow
        END SELECT

      CASE(ControllerSimple)

      CASE(ControllerStandAloneERV)

      CASE DEFAULT

      END SELECT

    ELSE IF (CurZoneEqNum > 0) THEN

      SELECT CASE(OAController(OAControllerNum)%ControllerType_Num)

      CASE(ControllerOutsideAir)

        CALL CheckZoneSizing(CurrentModuleObject,OAController(OAControllerNum)%Name)
        OAController(OAControllerNum)%MaxOA = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                                FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)

      CASE(ControllerSimple)

      CASE(ControllerStandAloneERV)

      CASE DEFAULT

      END SELECT

    END IF

    IF (OAController(OAControllerNum)%MaxOA < SmallAirVolFlow) THEN
      OAController(OAControllerNum)%MaxOA = 0.0d0
    END IF

    CALL ReportSizingOutput(CurrentModuleObject,OAController(OAControllerNum)%Name,&
                            'Maximum Outdoor Air Flow Rate [m3/s]',OAController(OAControllerNum)%MaxOA)

  END IF

  IF (OAController(OAControllerNum)%MinOA == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(CurrentModuleObject,OAController(OAControllerNum)%Name)
      IF (FinalSysSizing(CurSysNum)%DesOutAirVolFlow >= SmallAirVolFlow) THEN
        OAController(OAControllerNum)%MinOA = MIN(FinalSysSizing(CurSysNum)%DesOutAirVolFlow,OAController(OAControllerNum)%MaxOA)
      ELSE
        OAController(OAControllerNum)%MinOA = 0.0d0
      END IF

    END IF

    CALL ReportSizingOutput(CurrentModuleObject,OAController(OAControllerNum)%Name,&
                            'Minimum Outdoor Air Flow Rate [m3/s]',OAController(OAControllerNum)%MinOA)

    IF(OAController(OAControllerNum)%HumidistatZoneNum .GT. 0 .AND. OAController(OAControllerNum)%FixedMin)THEN
      IF(OAController(OAControllerNum)%MaxOA .GT. 0.0d0)THEN
        OAFlowRatio = OAController(OAControllerNum)%MinOA/OAController(OAControllerNum)%MaxOA
        IF(OAController(OAControllerNum)%HighRHOAFlowRatio .LT. OAFlowRatio)THEN
          CALL ShowWarningError(CurrentModuleObject//' "'//TRIM(OAController(OAControllerNum)%Name)//'"')
          CALL ShowContinueError('... A fixed minimum outdoor air flow rate and high humidity control have been specified.')
          CALL ShowContinueError('... The High Humidity Outdoor Air Flow Ratio is less than the ratio of'// &
                                 ' the outdoor air controllers minimum to maximum outside air flow rate.')
          CALL ShowContinueError('... Controller minimum flow rate = ' &
                                    //TRIM(TrimSigDigits(OAController(OAControllerNum)%MinOA,4))//' m3/s.')
          CALL ShowContinueError('... Controller maximum flow rate = ' &
                                    //TRIM(TrimSigDigits(OAController(OAControllerNum)%MaxOA,4))//' m3/s.')
          CALL ShowContinueError('... Controller minimum to maximum flow ratio = ' &
                                    //TRIM(TrimSigDigits(OAFlowRatio,4))//'.')
          CALL ShowContinueError('... High humidity control flow ratio = ' &
                                    //TRIM(TrimSigDigits(OAController(OAControllerNum)%HighRHOAFlowRatio,4))//'.')
          END IF
        END IF
      END If

  END IF
  ! If there is an outside air system, loop over components in the OA system; pass the design air flow rate
  ! to the coil components that don't have design air flow as an input.
  IF (CurOASysNum > 0) THEN
    DO CompNum=1,OutsideAirSys(CurOASysNum)%NumComponents
      CompType = OutsideAirSys(CurOASysNum)%ComponentType(CompNum)
      CompName = OutsideAirSys(CurOASysNum)%ComponentName(CompNum)
      IF (SameString(CompType,'COIL:COOLING:WATER:DETAILEDGEOMETRY') .OR. SameString(CompType,'COIL:HEATING:WATER') .OR. &
            SameString(CompType,'COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED')) THEN
        IF (SameString(CompType,'COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED')) THEN
          CoilName = GetHXDXCoilName(CompType,CompName,ErrorsFound)
          CoilType = GetHXCoilType(CompType,CompName,ErrorsFound)
        ELSE
          CoilName = CompName
          CoilType = CompType
        END IF
        CALL SetCoilDesFlow(CoilType,CoilName,OAController(OAControllerNum)%MinOA,&
                       ErrorsFound)
      END IF
    END DO ! End of component loop
  END IF
  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizeOAController

! End of Sizing Section of the Module
!******************************************************************************

! Beginning Update/Reporting Section of the Module
!******************************************************************************

SUBROUTINE UpdateOAController(OAControllerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       Shirey/Raustad FSEC, June 2003
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Move the results of CalcOAController to the affected nodes

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
    USE DataLoopNode

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OAControllerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: OutAirNodeNum
INTEGER :: InletAirNodeNum
INTEGER :: RelAirNodeNum
INTEGER :: RetAirNodeNum

OutAirNodeNum = OAController(OAControllerNum)%OANode
InletAirNodeNum = OAController(OAControllerNum)%InletNode
RelAirNodeNum = OAController(OAControllerNum)%RelNode
RetAirNodeNum = OAController(OAControllerNum)%RetNode

IF(OAController(OAControllerNum)%ControllerType_Num == ControllerOutsideAir)THEN
! The outside air controller sets the outside air flow rate and the relief air flow rate
Node(OutAirNodeNum)%MassFlowRate = OAController(OAControllerNum)%OAMassFlow
Node(InletAirNodeNum)%MassFlowRate = OAController(OAControllerNum)%OAMassFlow
Node(RelAirNodeNum)%MassFlowRate = OAController(OAControllerNum)%RelMassFlow
Node(OutAirNodeNum)%MassFlowRateMaxAvail = OAController(OAControllerNum)%OAMassFlow
ELSE
! The ERV controller sets the supply and secondary inlet node information for the Stand Alone ERV
! Currently, the Stand Alone ERV only has constant air flows (supply and exhaust), and these are
! already set in HVACStandAloneERV.f90 (subroutine init). Therefore, these flow assignments below are
! currently redundant but may be useful in the future as mass flow rates can vary based on the controller signal.
Node(OutAirNodeNum)%MassFlowRate = OAController(OAControllerNum)%OAMassFlow
Node(OutAirNodeNum)%MassFlowRateMaxAvail = OAController(OAControllerNum)%OAMassFlow
!
Node(RetAirNodeNum)%MassFlowRate = Node(OAController(OAControllerNum)%RetNode)%MassFlowRate
Node(RetAirNodeNum)%MassFlowRateMaxAvail = Node(OAController(OAControllerNum)%RetNode)%MassFlowRate
!
END IF

IF (Contaminant%CO2Simulation .AND. OAController(OAControllerNum)%ControllerType_Num .EQ. ControllerSimple) Then
  Node(RelAirNodeNum)%CO2 = Node(InletAirNodeNum)%CO2
  If (Node(RetAirNodeNum)%MassFlowRate .GT. 0.0d0) Then
    Node(RetAirNodeNum)%CO2 = ((Node(InletAirNodeNum)%MassFlowRate-Node(RelAirNodeNum)%MassFlowRate)*Node(InletAirNodeNum)%CO2 + &
              Node(OutAirNodeNum)%MassFlowRate*OutdoorCO2) / Node(RetAirNodeNum)%MassFlowRate
  ELSE
    Node(RetAirNodeNum)%CO2 = Node(InletAirNodeNum)%CO2
  END If
End If

IF (Contaminant%GenericContamSimulation .AND. OAController(OAControllerNum)%ControllerType_Num .EQ. ControllerSimple) Then
  Node(RelAirNodeNum)%GenContam = Node(InletAirNodeNum)%GenContam
  If (Node(RetAirNodeNum)%MassFlowRate .GT. 0.0d0) Then
    Node(RetAirNodeNum)%GenContam = ((Node(InletAirNodeNum)%MassFlowRate-Node(RelAirNodeNum)%MassFlowRate)* &
       Node(InletAirNodeNum)%GenContam + Node(OutAirNodeNum)%MassFlowRate*OutdoorGC) / Node(RetAirNodeNum)%MassFlowRate
  ELSE
    Node(RetAirNodeNum)%GenContam = Node(InletAirNodeNum)%GenContam
  END If
End If

RETURN
END SUBROUTINE UpdateOAController

SUBROUTINE UpdateOAMixer(OAMixerNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Move the results of CalcOAMixer to the affected nodes

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
    USE DataLoopNode

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OAMixerNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: MixNode
INTEGER :: RelNode
INTEGER :: RetNode

MixNode = OAMixer(OAMixerNum)%MixNode
RelNode = OAMixer(OAMixerNum)%RelNode
RetNode = OAMixer(OAMixerNum)%RetNode
! Move mixed air data to the mixed air node
Node(MixNode)%MassFlowRate = OAMixer(OAMixerNum)%MixMassFlowRate
Node(MixNode)%Temp = OAMixer(OAMixerNum)%MixTemp
Node(MixNode)%HumRat = OAMixer(OAMixerNum)%MixHumRat
Node(MixNode)%Enthalpy = OAMixer(OAMixerNum)%MixEnthalpy
Node(MixNode)%Press = OAMixer(OAMixerNum)%MixPressure
Node(MixNode)%MassFlowRateMaxAvail = OAMixer(OAMixerNum)%MixMassFlowRate
! Move the relief air data to the relief air node
Node(RelNode)%MassFlowRate = OAMixer(OAMixerNum)%RelMassFlowRate
Node(RelNode)%Temp = OAMixer(OAMixerNum)%RelTemp
Node(RelNode)%HumRat = OAMixer(OAMixerNum)%RelHumRat
Node(RelNode)%Enthalpy = OAMixer(OAMixerNum)%RelEnthalpy
Node(RelNode)%Press = OAMixer(OAMixerNum)%RelPressure
Node(RelNode)%MassFlowRateMaxAvail = OAMixer(OAMixerNum)%RelMassFlowRate

IF (Contaminant%CO2Simulation) Then
  Node(RelNode)%CO2 = Node(RetNode)%CO2
  IF (OAMixer(OAMixerNum)%MixMassFlowRate <= VerySmallMassFlow) THEN
    Node(MixNode)%CO2 = Node(RetNode)%CO2
  ELSE
    Node(MixNode)%CO2 = ((Node(RetNode)%MassFlowRate - Node(RelNode)%MassFlowRate)*Node(RetNode)%CO2 + &
                  OAMixer(OAMixerNum)%OAMassFlowRate*OutdoorCO2) / OAMixer(OAMixerNum)%MixMassFlowRate
  END IF
End If

IF (Contaminant%GenericContamSimulation) Then
  Node(RelNode)%GenContam = Node(RetNode)%GenContam
  IF (OAMixer(OAMixerNum)%MixMassFlowRate <= VerySmallMassFlow) THEN
    Node(MixNode)%GenContam = Node(RetNode)%GenContam
  ELSE
    Node(MixNode)%GenContam = ((Node(RetNode)%MassFlowRate - Node(RelNode)%MassFlowRate)*Node(RetNode)%GenContam + &
                  OAMixer(OAMixerNum)%OAMassFlowRate*OutdoorGC) / OAMixer(OAMixerNum)%MixMassFlowRate
  END IF
End If

RETURN
END SUBROUTINE UpdateOAMixer

SUBROUTINE ReportOAMixer(OAMixerNum)

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OAMixerNum !unused1208

RETURN
END SUBROUTINE ReportOAMixer

SUBROUTINE ReportOAController(OAControllerNum)

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: OAControllerNum !unused1208


RETURN
END SUBROUTINE ReportOAController

! End of Sizing Section of the Module
!******************************************************************************

! Beginning Utility Section of the Module
!******************************************************************************

FUNCTION MixedAirControlTempResidual(OASignal, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April, 2003
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function TMixSetPoint - TMix.
          ! Economizer damper position (OASignal) is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Using a mass and energy balance at the mixed air node, calculates the
          ! mixed air temperature given the outside air damper position.

          ! REFERENCES:

          ! USE STATEMENTS:
USE Psychrometrics, ONLY:PsyTdbFnHW

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: OASignal ! Relative outside air flow rate (0 to 1)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = mixed node number
                                                    ! par(2) = return node number
                                                    ! par(3) = outside air node number
                                                    ! par(4) = mixed air flow rate
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: MixNode              ! mixed air node number
  INTEGER :: RetNode              ! return air node number
  INTEGER :: OANode               ! outside air node number
  REAL(r64)    :: MixMassFlowRate      ! mixed air mass flow rare [kg/s]
  REAL(r64)    :: OAMassFlowRate       ! outside air mass flow rate [kg/s]
  REAL(r64)    :: RecircMassFlowRate   ! recirculated air mass flow rate [kg/s]
  REAL(r64)    :: RecircEnth           ! recirculated air specific enthalpy [J/kg]
  REAL(r64)    :: RecircHumRat         ! recirculated air humidity ratio [kg water/kg dry air]
  REAL(r64)    :: MixEnth              ! mixed air specific enthalpy [J/kg]
  REAL(r64)    :: MixHumRat            ! mixed air humidity ratio [kg water/kg dry air]
  REAL(r64)    :: MixTemp              ! mixed air temperature [C]

  MixNode = INT(Par(1))
  RetNode = INT(Par(2))
  OANode  = INT(Par(3))
  MixMassFlowRate = Par(4)
  OAMassFlowRate = OASignal*MixMassFlowRate
  RecircMassFlowRate = MAX(MixMassFlowRate-OAMassFlowRate,0.0d0)
  RecircEnth = Node(RetNode)%Enthalpy
  RecircHumRat = Node(RetNode)%HumRat
  MixEnth = (RecircMassFlowRate*RecircEnth + OAMassFlowRate*Node(OANode)%Enthalpy) / MixMassFlowRate
  MixHumRat = (RecircMassFlowRate*RecircHumRat + OAMassFlowRate*Node(OANode)%HumRat) / MixMassFlowRate
  MixTemp = PsyTdbFnHW(MixEnth,MixHumRat)
  Residuum = Node(MixNode)%TempSetPoint - MixTemp

  RETURN
END FUNCTION MixedAirControlTempResidual

FUNCTION GetOAMixerNodeNumbers(OAMixerName,ErrorsFound) RESULT(OANodeNumbers)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given OA mixer and returns the node numbers.  If
          ! incorrect OA mixer name is given, errorsfound is returned as true
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: OAMixerName     ! must match OA mixer names for the OA mixer type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound     ! set to true if problem
  INTEGER, DIMENSION(4)        :: OANodeNumbers   ! return OA mixer nodes

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichOAMixer

  ! Obtains and Allocates OA mixer related parameters from input file
  IF (GetOAMixerInputFlag) THEN  !First time subroutine has been entered
    CALL GetOAMixerInputs
    GetOAMixerInputFlag=.false.
  End If

  WhichOAMixer=FindItemInList(OAMixerName,OAMixer%Name,NumOAMixers)
  IF (WhichOAMixer /= 0) THEN
    OANodeNumbers(1) = OAMixer(WhichOAMixer)%InletNode
    OANodeNumbers(2) = OAMixer(WhichOAMixer)%RelNode
    OANodeNumbers(3) = OAMixer(WhichOAMixer)%RetNode
    OANodeNumbers(4) = OAMixer(WhichOAMixer)%MixNode
  ENDIF

  IF (WhichOAMixer == 0) THEN
    CALL ShowSevereError('GetOAMixerNodeNumbers: Could not find OA Mixer = "'//TRIM(OAMixerName)//'"')
    ErrorsFound=.true.
    OANodeNumbers=0
  ENDIF

  RETURN

END FUNCTION GetOAMixerNodeNumbers

FUNCTION GetNumOAMixers() RESULT(NumberOfOAMixers)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the number of OA mixers is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER :: NumberOfOAMixers

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOAMixerInputFlag) THEN  !First time subroutine has been entered
    CALL GetOAMixerInputs
    GetOAMixerInputFlag=.false.
  End If

  NumberOfOAMixers=NumOAMixers

  RETURN

END FUNCTION GetNumOAMixers

FUNCTION GetNumOAControllers() RESULT(NumberOfOAControllers)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the number of OA Controllers is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER :: NumberOfOAControllers

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOAControllerInputFlag) THEN
    ! Make sure OAControllers are "gotten"
    CALL GetOAControllerInputs
    GetOAControllerInputFlag=.false.
  ENDIF

  NumberOfOAControllers=NumOAControllers

  RETURN

END FUNCTION GetNumOAControllers

FUNCTION GetOAMixerReliefNodeNumber(OAMixerNum) RESULT(ReliefNodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the relief node number of indicated
          ! mixer is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OAMixerNum  ! Which Mixer
  INTEGER :: ReliefNodeNumber ! Relief Node Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOAMixerInputFlag) THEN  !First time subroutine has been entered
    CALL GetOAMixerInputs
    GetOAMixerInputFlag=.false.
  End If

  IF (OAMixerNum > NumOAMixers) THEN
    CALL ShowFatalError('GetOAMixerReliefNodeNumber: Requested Mixer #='//TRIM(TrimSigDigits(OAMixerNum))//  &
                   ', which is > number of OA Mixers='//TRIM(TrimSigDigits(NumOAMixers)))
  ENDIF

  ReliefNodeNumber=OAMixer(OAMixerNum)%RelNode

  RETURN

END FUNCTION GetOAMixerReliefNodeNumber

FUNCTION GetOASysControllerListIndex(OASysNumber) RESULT(OASysControllerListNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the Controller List index of the indicated
          ! OA System is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OASysNumber         ! OA Sys Number
  INTEGER ::             OASysControllerListNum  ! OA Sys Controller List index

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  OASysControllerListNum = OutsideAirSys(OASysNumber)%ControllerListNum

  RETURN

END FUNCTION GetOASysControllerListIndex

FUNCTION GetOASysNumSimpControllers(OASysNumber) RESULT(OASysNumSimpControllers)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the number of Controller:Simple objects in the
          ! OA System is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OASysNumber         ! OA Sys Number
  INTEGER ::             OASysNumSimpControllers  ! number of Controller:Simple objects in this OA System

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  OASysNumSimpControllers = OutsideAirSys(OASysNumber)%NumSimpleControllers

  RETURN

END FUNCTION GetOASysNumSimpControllers

FUNCTION GetOASysNumHeatingCoils(OASysNumber) RESULT(NumHeatingCoils)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the number of heating coils in the
          ! OA System is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OASysNumber         ! OA Sys Number
  INTEGER             :: NumHeatingCoils     ! number of heating coils in this OA System

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: CompType
  CHARACTER(len=MaxNameLength) :: CompName
  LOGICAL                      :: Sim
  LOGICAL                      :: FirstHVACIteration
  LOGICAL                      :: OAHeatingCoil
  LOGICAL                      :: OACoolingCoil
  INTEGER                      :: CompNum
  INTEGER                      :: AirLoopNum
  LOGICAL                      :: OAHX

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  Sim = .FALSE.
  FirstHVACIteration = .FALSE.
  AirLoopNum = 0
  NumHeatingCoils = 0
  DO CompNum=1,OutsideAirSys(OASysNumber)%NumComponents
    CompType = OutsideAirSys(OASysNumber)%ComponentType(CompNum)
    CompName = OutsideAirSys(OASysNumber)%ComponentName(CompNum)
    CALL SimOAComponent(CompType,CompName,OutsideAirSys(OASysNumber)%ComponentType_Num(CompNum),  &
                        FirstHVACIteration,OutsideAirSys(OASysNumber)%ComponentIndex(CompNum),AirLoopNum,Sim,OASysNumber, &
                        OAHeatingCoil,OACoolingCoil,OAHX)
    IF (OAHeatingCoil) THEN
      NumHeatingCoils = NumHeatingCoils + 1
    END IF
  END DO

  RETURN

END FUNCTION GetOASysNumHeatingCoils

FUNCTION GetOASysNumCoolingCoils(OASysNumber) RESULT(NumCoolingCoils)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the number of cooling coils in the
          ! OA System is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OASysNumber         ! OA Sys Number
  INTEGER             :: NumCoolingCoils     ! number of cooling coils in this OA System

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: CompType
  CHARACTER(len=MaxNameLength) :: CompName
  LOGICAL                      :: Sim
  LOGICAL                      :: FirstHVACIteration
  LOGICAL                      :: OAHeatingCoil
  LOGICAL                      :: OACoolingCoil
  INTEGER                      :: CompNum
  INTEGER                      :: AirLoopNum
  LOGICAL                      :: OAHX

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  Sim = .FALSE.
  FirstHVACIteration = .FALSE.
  AirLoopNum = 0
  NumCoolingCoils = 0
  DO CompNum=1,OutsideAirSys(OASysNumber)%NumComponents
    CompType = OutsideAirSys(OASysNumber)%ComponentType(CompNum)
    CompName = OutsideAirSys(OASysNumber)%ComponentName(CompNum)
    CALL SimOAComponent(CompType,CompName,OutsideAirSys(OASysNumber)%ComponentType_Num(CompNum),  &
                        FirstHVACIteration,OutsideAirSys(OASysNumber)%ComponentIndex(CompNum),AirLoopNum,Sim,OASysNumber, &
                        OAHeatingCoil,OACoolingCoil,OAHX)
    IF (OACoolingCoil) THEN
      NumCoolingCoils = NumCoolingCoils + 1
    END IF
  END DO

  RETURN

END FUNCTION GetOASysNumCoolingCoils

FUNCTION GetOASystemNumber(OASysName) RESULT(OASysNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the OA System number of indicated
          ! OA System is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: OASysName  ! OA Sys Name
  INTEGER :: OASysNumber ! OA Sys Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  OASysNumber = FindItemInList(OASysName,OutsideAirSys%Name,NumOASystems)

  RETURN

END FUNCTION GetOASystemNumber

FUNCTION FindOAMixerMatchForOASystem(OASysNumber) RESULT(OAMixerNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the matched mixer number is found.
          ! Note -- only the first is looked at for an Outside Air System.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OASysNumber  ! Which OA System
  INTEGER :: OAMixerNumber ! Mixer Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: OACompNum

  IF (GetOAMixerInputFlag) THEN
    CALL GetOAMixerInputs
    GetOAMixerInputFlag=.false.
  ENDIF

  OAMixerNumber=0
  IF (OASysNumber > 0 .and. OASysNumber <= NumOASystems) THEN
    DO OACompNum=1,OutsideAirSys(OASysNumber)%NumComponents
      IF (SameString(OutsideAirSys(OASysNumber)%ComponentType(OACompNum),'OUTDOORAIR:MIXER')) THEN
        OAMixerNumber = FindItemInList(OutsideAirSys(OASysNumber)%ComponentName(OACompNum),OAMixer%Name,NumOAMixers)
        EXIT
      END IF
    END DO
  ENDIF

  RETURN

END FUNCTION FindOAMixerMatchForOASystem

FUNCTION GetOAMixerIndex(OAMixerName) RESULT(OAMixerindex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the mixer index of indicated
          ! mixer is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: OAMixerName  ! Which Mixer
  INTEGER :: OAMixerIndex ! Mixer Index

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOAMixerInputFlag) THEN
    CALL GetOAMixerInputs
    GetOAMixerInputFlag=.false.
  ENDIF

  OAMixerIndex=FindItem(OAMixerName,OAMixer%Name,NumOAMixers)

  IF (OAMixerIndex == 0) THEN
    CALL ShowSevereError('GetOAMixerIndex: Could not find OutdoorAir:Mixer, Name="'//TRIM(OAMixerName)//'"')
  ENDIF

  RETURN

END FUNCTION GetOAMixerIndex

FUNCTION GetOAMixerInletNodeNumber(OAMixerNumber) RESULT(OAMixerInletNodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the mixer inlet node number of indicated
          ! mixer is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OAMixerNumber  ! Which Mixer
  INTEGER :: OAMixerInletNodeNumber ! Mixer Inlet Node Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOAMixerInputFlag) THEN
    CALL GetOAMixerInputs
    GetOAMixerInputFlag=.false.
  ENDIF

  OAMixerInletNodeNumber=0
  IF (OAMixerNumber > 0 .and. OAMixerNumber <= NumOAMixers) THEN
    OAMixerInletNodeNumber=OAMixer(OAMixerNumber)%InletNode
  ENDIF

  RETURN

END FUNCTION GetOAMixerInletNodeNumber

FUNCTION GetOAMixerReturnNodeNumber(OAMixerNumber) RESULT(OAMixerReturnNodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   December 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the mixer return node number of indicated
          ! mixer is returned.

          ! METHODOLOGY EMPLOYED:
          ! followed Linda Lawrie's GetOAMixerInletNodeNumber

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OAMixerNumber  ! Which Mixer
  INTEGER :: OAMixerReturnNodeNumber ! Mixer Inlet Node Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOAMixerInputFlag) THEN
    CALL GetOAMixerInputs
    GetOAMixerInputFlag=.false.
  ENDIF

  OAMixerReturnNodeNumber=0
  IF (OAMixerNumber > 0 .and. OAMixerNumber <= NumOAMixers) THEN
    OAMixerReturnNodeNumber=OAMixer(OAMixerNumber)%RetNode
  ENDIF

  RETURN

END FUNCTION GetOAMixerReturnNodeNumber

FUNCTION GetOAMixerMixedNodeNumber(OAMixerNumber) RESULT(OAMixerMixedNodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   December 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the mixer mixed air node number of indicated
          ! mixer is returned.

          ! METHODOLOGY EMPLOYED:
          ! followed Linda Lawrie's GetOAMixerInletNodeNumber

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OAMixerNumber  ! Which Mixer
  INTEGER :: OAMixerMixedNodeNumber ! Mixer Inlet Node Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOAMixerInputFlag) THEN
    CALL GetOAMixerInputs
    GetOAMixerInputFlag=.false.
  ENDIF

  OAMixerMixedNodeNumber=0
  IF (OAMixerNumber > 0 .and. OAMixerNumber <= NumOAMixers) THEN
    OAMixerMixedNodeNumber=OAMixer(OAMixerNumber)%MixNode
  ENDIF

  RETURN

END FUNCTION GetOAMixerMixedNodeNumber

FUNCTION CheckForControllerWaterCoil(ControllerType,ControllerName) RESULT(OnControllerList)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine checks the controller list for existance of the
          ! reference coil.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ControllerType  ! should be passed in as UPPERCASE
  CHARACTER(len=*), INTENT(IN) :: ControllerName  ! should be passed in as UPPERCASE
  LOGICAL :: OnControllerList           ! true if found on controller list

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Num
  INTEGER :: CompNum

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  OnControllerList=.false.

  DO Num=1,NumControllerLists
    DO CompNum=1,ControllerLists(Num)%NumControllers

      IF (.not. SameString(ControllerLists(Num)%ControllerType(CompNum),ControllerType)) CYCLE
      IF (.not. SameString(ControllerLists(Num)%ControllerName(CompNum),ControllerName)) CYCLE
      OnControllerList =.true.
      EXIT
    ENDDO
  ENDDO

  RETURN

END FUNCTION CheckForControllerWaterCoil

SUBROUTINE CheckControllerLists(ErrFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine checks for a "dangling" controller list (AirLoopHVAC:ControllerList).
          ! It must be either found on a AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound,MakeUPPERCase,GetObjectItem,FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='CheckControllerLists'
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='AirLoopHVAC:ControllerList'
  CHARACTER(len=*), PARAMETER :: AirLoopObject='AirLoopHVAC'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers
  INTEGER :: NumControllers
  INTEGER :: NumAirLoop
  CHARACTER(len=MaxNameLength) :: ControllerListName
  INTEGER :: Item
  INTEGER :: IOSTAT
  INTEGER :: Found
  INTEGER :: Count
  INTEGER :: Loop
  CHARACTER(len=MaxNameLength) :: AirLoopName

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  NumControllers=GetNumObjectsFound(CurrentModuleObject)
  NumAirLoop=GetNumObjectsFound(AirLoopObject)
  AirLoopName=' '

  DO Item=1,NumControllers

    CALL GetObjectItem(CurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOSTAT)
    ControllerListName=cAlphaArgs(1)
    Count=0

    ! Check AirLoopHVAC -- brute force, get each AirLoopHVAC

    DO Loop=1,NumAirLoop
      CALL GetObjectItem(AirLoopObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOSTAT)
      IF (cAlphaArgs(2) /= ControllerListName) CYCLE
      Count=Count+1
      IF (Count == 1) AirLoopName=cAlphaArgs(1)
    ENDDO

    !  Now check AirLoopHVAC and AirLoopHVAC:OutdoorAirSystem
    Found=0
    IF (NumOASystems > 0) THEN
      Found=FindItemInList(ControllerListName,OutsideAirSys%ControllerListName,NumOASystems)
      IF (Found > 0) Count=Count+1
    ENDIF

    IF (Count == 0) THEN
      CALL ShowSevereError(CurrentModuleObject//'="'//trim(ControllerListName)//'" is not referenced on a '//  &
         'AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem object.')
      ErrFound=.true.
    ELSEIF (Count > 1) THEN
      CALL ShowSevereError(CurrentModuleObject//'="'//trim(ControllerListName)//'" has too many references on '//  &
         'AirLoopHVAC or AirLoopHVAC:OutdoorAirSystem objects.')
      IF (Found > 0) THEN
        CALL ShowContinueError('...AirLoopHVAC:OutdoorAirSystem="'//trim(OutsideAirSys(Found)%Name)//'".')
      ENDIF
      CALL ShowContinueError('...also on AirLoopHVAC="'//trim(AirLoopName)//'".')
      ErrFound=.true.
    ENDIF
  ENDDO


  RETURN

END SUBROUTINE CheckControllerLists


 SUBROUTINE SetOAControllerData(OACtrlNum,ErrorsFound,Name,ControllerType,ControllerType_Num,  &
                               LockoutType,FixedMin,TempLim,TempLowLim,EnthLim,  &
                               DPTempLim,EnthalpyCurvePtr, &
                               MaxOA,MinOA,EconoType,MixNode,OANode,InletNode,RelNode,RetNode, &
                               HumidistatZoneNum,HighRHOAFlowRatio,ModifyDuringHighOAMoisture, &
                               NodeNumofHumidistatZone,EconomizerOASchedPtr,Bypasstype)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine was designed for the ERV Controller which really needs some data from
          ! the ERV stand alone unit but caused a circular reference when it was here in the
          ! Mixed Air module setting OAController Data.  Also, this is an illustration of setting
          ! Data from an outside source.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits
  USE InputProcessor, ONLY: FindItemInList, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)       :: OACtrlNum  ! Number of OA Controller
  LOGICAL, INTENT(INOUT)    :: ErrorsFound ! Set to true if certain errors found
  CHARACTER(len=*),OPTIONAL :: Name                 ! Name of Controller
  CHARACTER(len=*),OPTIONAL :: ControllerType       ! Controller Type
  INTEGER,OPTIONAL          :: ControllerType_Num   ! Parameter equivalent of Controller Type

  CHARACTER(len=*),OPTIONAL :: LockoutType          ! Lock out type
  LOGICAL,OPTIONAL          :: FixedMin             ! Fixed Minimum or Proportional Minimum
  REAL(r64),OPTIONAL             :: TempLim              ! Temperature Limit
  REAL(r64),OPTIONAL             :: TempLowLim           ! Temperature Lower Limit
  REAL(r64),OPTIONAL             :: EnthLim              ! Enthalpy Limit
  REAL(r64),OPTIONAL             :: DPTempLim            ! Dew Point Temperature Limit
  INTEGER,OPTIONAL          :: EnthalpyCurvePtr     ! Electronic Enthalpy Limit Curve Index
  REAL(r64),OPTIONAL             :: MinOA                ! Minimum outside air flow (m3/sec)
  REAL(r64),OPTIONAL             :: MaxOA                ! Maximum outside air flow (m3/sec)
  CHARACTER(len=*),OPTIONAL :: EconoType            ! EconoType = No Economizer,Differential Enthalpy, Differential Dry bulb,
                                                    ! Differential Dry Bulb and Enthalpy and Any other Economizer Strategy present.
  INTEGER,OPTIONAL          :: MixNode              ! Controlled node (mixed air node)
  INTEGER,OPTIONAL          :: OANode               ! Actuated node (outside air node)
  INTEGER,OPTIONAL          :: InletNode            ! Inlet Air Node for into Mixer  (BTG Nov 2004)
  INTEGER,OPTIONAL          :: RelNode              ! Relief Air Node Number
  INTEGER,OPTIONAL          :: RetNode              ! Return Air Node Number
  INTEGER,OPTIONAL          :: HumidistatZoneNum    ! Zone number where humidistat is located
  REAL(r64),OPTIONAL             :: HighRHOAFlowRatio    ! Ratio of outside air flow to maximum outside air flow rate for high RH
  LOGICAL,OPTIONAL          :: ModifyDuringHighOAMoisture ! TRUE if modify air flow is allowed during high OA humrat conditions
  INTEGER,OPTIONAL          :: NodeNumofHumidistatZone   ! actual node number of controlled zone
  INTEGER,OPTIONAL          :: EconomizerOASchedPtr           ! Time of day schedule for increasing outdoor air
  CHARACTER(len=*),OPTIONAL :: Bypasstype       ! ActivateBypassAtMinOAFlow, SetOAFlowRate
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOAControllerInputFlag) THEN
    ! Make sure OAControllers are "gotten"
    CALL GetOAControllerInputs
    GetOAControllerInputFlag=.false.
  ENDIF

  IF (OACtrlNum <= 0 .or. OACtrlNum > NumOAControllers) THEN
    CALL ShowSevereError('SetOAControllerData: called with OA Controller Number out of range='//  &
         TRIM(TrimSigDigits(OACtrlNum))//' should be >0 and <'//TRIM(TrimSigDigits(NumOAControllers)))
    ErrorsFound=.true.
    RETURN
  ENDIF

  IF (PRESENT(Name)) THEN
    OAController(OACtrlNum)%Name=Name
  ENDIF

  IF (PRESENT(ControllerType)) THEN
    OAController(OACtrlNum)%ControllerType=ControllerType
  ENDIF

  IF (PRESENT(ControllerType_Num))THEN
    OAController(OACtrlNum)%ControllerType_Num = ControllerType_Num
  END IF


  IF (PRESENT(LockoutType)) THEN
    IF (SameString(LockoutType,'NOLOCKOUT')) THEN
      OAController(OACtrlNum)%Lockout = NoLockoutPossible
    ELSE IF (SameString(LockoutType,'LOCKOUTWITHHEATING')) THEN
      OAController(OACtrlNum)%Lockout = LockoutWithHeatingPossible
    ELSE IF (SameString(LockoutType,'LOCKOUTWITHCOMPRESSOR')) THEN
      OAController(OACtrlNum)%Lockout = LockoutWithCompressorPossible
    ELSE
      CALL ShowSevereError('SetOAControllerData: Illegal Lockout type specified='//TRIM(LockoutType))
      ErrorsFound=.true.
    END IF
  ENDIF

  IF (PRESENT(FixedMin)) THEN
    OAController(OACtrlNum)%FixedMin=FixedMin
  ENDIF

  IF (PRESENT(TempLim)) THEN
    OAController(OACtrlNum)%TempLim=TempLim
  ENDIF

  IF (PRESENT(TempLowLim)) THEN
    OAController(OACtrlNum)%TempLowLim=TempLowLim
  ENDIF

  IF (PRESENT(EnthLim)) THEN
    OAController(OACtrlNum)%EnthLim=EnthLim
  ENDIF

  IF (PRESENT(DPTempLim)) THEN
    OAController(OACtrlNum)%DPTempLim=DPTempLim
  ENDIF

  IF (PRESENT(EnthalpyCurvePtr)) THEN
    OAController(OACtrlNum)%EnthalpyCurvePtr=EnthalpyCurvePtr
  ENDIF

  IF (PRESENT(MinOA)) THEN
    OAController(OACtrlNum)%MinOA=MinOA
  ENDIF

  IF (PRESENT(MaxOA)) THEN
    OAController(OACtrlNum)%MaxOA=MaxOA
  ENDIF

  IF (PRESENT(EconoType)) THEN
    IF (SameString(EconoType,'NoEconomizer')) THEN
      OAController(OACtrlNum)%Econo = NoEconomizer
    ELSE IF (SameString(EconoType,'DifferentialDryBulbandenthalpy')) THEN
     OAController(OACtrlNum)%Econo = DifferentialDryBulbAndEnthalpy
    ELSE IF (SameString(EconoType,'DifferentialDryBulb')) THEN
     OAController(OACtrlNum)%Econo = DifferentialDryBulb
    ELSE IF (SameString(EconoType,'DifferentialEnthalpy')) THEN
     OAController(OACtrlNum)%Econo = DifferentialEnthalpy
    ELSE IF (SameString(EconoType,'Economizer Strategy Present')) THEN  ! from routines, not from input
      OAController(OACtrlNum)%Econo = FixedDryBulb
    ELSE
      CALL ShowSevereError('SetOAControllerData: Illegal Economizer type specified='//TRIM(EconoType))
      ErrorsFound=.true.
    END IF
  ENDIF

  IF(PRESENT(BypassType)) THEN
    IF (SameString(BypassType,'MINIMUMFLOWWITHBYPASS')) THEN
      OAController(OACtrlNum)%EconBypass = .TRUE.
    ELSE IF(SameString(BypassType,'MODULATEFLOW')) THEN
      OAController(OACtrlNum)%EconBypass = .FALSE.
    ELSE
      CALL ShowSevereError('SetOAControllerData:Illegal Economizer Bypass type specified ='//TRIM(BypassType))
      Errorsfound = .TRUE.
    END IF
  END IF

  IF (PRESENT(MixNode)) THEN
    OAController(OACtrlNum)%MixNode=MixNode
  ENDIF

  IF (PRESENT(OANode)) THEN
    OAController(OACtrlNum)%OANode=OANode
  ENDIF

  IF (PRESENT(InletNode)) THEN
    OAController(OACtrlNum)%InletNode=InletNode
  ENDIF

  IF (PRESENT(RelNode)) THEN
    OAController(OACtrlNum)%RelNode=RelNode
  ENDIF

  IF (PRESENT(RetNode)) THEN
    OAController(OACtrlNum)%RetNode=RetNode
  ENDIF

  IF (PRESENT(HumidistatZoneNum)) THEN
    OAController(OACtrlNum)%HumidistatZoneNum=HumidistatZoneNum
  ENDIF

  IF (PRESENT(HighRHOAFlowRatio)) THEN
    OAController(OACtrlNum)%HighRHOAFlowRatio=HighRHOAFlowRatio
  ENDIF

  IF (PRESENT(ModifyDuringHighOAMoisture)) THEN
    OAController(OACtrlNum)%ModifyDuringHighOAMoisture=ModifyDuringHighOAMoisture
  ENDIF

  IF (PRESENT(NodeNumofHumidistatZone)) THEN
    OAController(OACtrlNum)%NodeNumofHumidistatZone=NodeNumofHumidistatZone
  ENDIF

  IF (PRESENT(EconomizerOASchedPtr)) THEN
    OAController(OACtrlNum)%EconomizerOASchedPtr=EconomizerOASchedPtr
  END IF

  RETURN

END SUBROUTINE SetOAControllerData

SUBROUTINE CheckOAControllerName(OAControllerName,NumCurrentOAControllers,IsNotOK,IsBlank,SourceID)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! When OA Controller data is gotten from other routines, must check to make sure
          ! new name doesn't duplicate.  (Essentially a pass through to call Verify Name)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: VerifyName

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: OAControllerName  ! proposed name
  INTEGER, INTENT(IN)          :: NumCurrentOAControllers  ! Count on number of controllers
  LOGICAL, INTENT(INOUT)       :: IsNotOK           ! Pass through to VerifyName
  LOGICAL, INTENT(INOUT)       :: IsBlank           ! Pass through to VerifyName
  CHARACTER(len=*), INTENT(IN) :: SourceID          ! Pass through to VerifyName

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOAControllerInputFlag) THEN
    ! Make sure OAControllers are "gotten"
    CALL GetOAControllerInputs
    GetOAControllerInputFlag=.false.
  ENDIF

  CALL VerifyName(OAControllerName,OAController%Name,NumCurrentOAControllers,IsNotOK,IsBlank,SourceID)

  RETURN

END SUBROUTINE CheckOAControllerName

Subroutine Checksetpoints(OAControllerNum,OutAirMinFrac,OutAirSignal,EconomizerOperationFlag)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Amit bhansali
          !       DATE WRITTEN   August 2008?
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine checks the setpoints of the upper limits of temperatures, limit enthalpy
          ! Limit dew point, Enthalpy curve

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics, ONLY: PsyTdpFnWPb
  USE CurveManager, ONLY: CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: OAControllerNum            ! index to OA controller
  REAL(r64), INTENT(IN)    :: OutAirMinFrac              ! Local variable used to calculate min OA fraction
  REAL(r64), INTENT(INOUT) :: OutAirSignal               ! Used to set OA mass flow rate
  LOGICAL,   INTENT(INOUT) :: EconomizerOperationFlag    ! logical used to show economizer status

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                 :: OADPTemp                   ! Dew Point Temperature calculation

  IF (OAController(OAControllerNum)%TempLim /= BlankNumeric .AND. OAController(OAControllerNum)%OATemp &
                                                        .GT.OAController(OAControllerNum)%TempLim) THEN
    OutAirSignal = OutAirMinFrac
    EconomizerOperationFlag = .FALSE.
  END IF
  ! Outside air enthalpy limit
  IF (OAController(OAControllerNum)%EnthLim /= BlankNumeric .AND. OAController(OAControllerNum)%OAEnth &
                                                        .GT.OAController(OAControllerNum)%EnthLim) THEN
    OutAirSignal = OutAirMinFrac
    EconomizerOperationFlag = .FALSE.
  END IF

  IF (OAController(OAControllerNum)%DPTempLim /= BlankNumeric) THEN
    OADPTemp = PsyTdpFnWPb(OAController(OAControllerNum)%OAHumRat,OAController(OAControllerNum)%OAPress)
    IF(OADPTemp .GT. OAController(OAControllerNum)%DPTempLim) THEN
      OutAirSignal = OutAirMinFrac
      EconomizerOperationFlag = .FALSE.
    END IF
  END IF

  IF(OAController(OAControllerNum)%EnthalpyCurvePtr .GT. 0)THEN
    IF(OAController(OAControllerNum)%OAHumRat .GT. &
          CurveValue(OAController(OAControllerNum)%EnthalpyCurvePtr, &
                                  OAController(OAControllerNum)%OATemp))THEN
      OutAirSignal = OutAirMinFrac
      EconomizerOperationFlag = .FALSE.
    END IF
  END IF

  RETURN

  END SUBROUTINE Checksetpoints

  FUNCTION GetNumOASystems() RESULT(NumberOfOASystems)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Get Number of OA Systems, After making sure get input is done

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER :: NumberOfOASystems      ! Number of OA Systems

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

    IF (GetOASysInputFlag) THEN
      CALL GetOutsideAirSysInputs
      GetOASysInputFlag=.false.
    ENDIF

    NumberOfOASystems = NumOASystems

  END FUNCTION GetNumOASystems

  FUNCTION GetOACompListNumber(OASysNum) RESULT(NumOACompList)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Heejin Cho
          !       DATE WRITTEN   November 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the OA System number of indicated
          ! OA System is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: OASysNum         ! OA Sys Number
  INTEGER :: NumOACompList      ! OA Comp Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  NumOACompList = OutsideAirSys(OASysNum)%NumComponents

END FUNCTION GetOACompListNumber

FUNCTION GetOACompName(OASysNum, InListNum) RESULT(OACompName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Heejin Cho
          !       DATE WRITTEN   November 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the number of heating coils in the
          ! OA System is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OASysNum         ! OA Sys Number
  INTEGER, INTENT(IN) :: InListNum         ! In-list Number
  CHARACTER(len=MaxNameLength) :: OACompName
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  OACompName = OutsideAirSys(OASysNum)%ComponentName(InListNum)

END FUNCTION GetOACompName

FUNCTION GetOACompType(OASysNum, InListNum) RESULT(OACompType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Heejin Cho
          !       DATE WRITTEN   November 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the number of heating coils in the
          ! OA System is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OASysNum         ! OA Sys Number
  INTEGER, INTENT(IN) :: InListNum         ! In-list Number
  CHARACTER(len=MaxNameLength) :: OACompType
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  OACompType = OutsideAirSys(OASysNum)%ComponentType(InListNum)

END FUNCTION GetOACompType

FUNCTION GetOACompTypeNum(OASysNum, InListNum) RESULT(OACompTypeNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Heejin Cho
          !       DATE WRITTEN   November 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! After making sure get input is done, the number of heating coils in the
          ! OA System is returned.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OASysNum         ! OA Sys Number
  INTEGER, INTENT(IN) :: InListNum         ! In-list Number
  INTEGER :: OACompTypeNum
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  IF (GetOASysInputFlag) THEN
    CALL GetOutsideAirSysInputs
    GetOASysInputFlag=.false.
  ENDIF

  OACompTypeNum = OutsideAirSys(OASysNum)%ComponentType_Num(InListNum)

END FUNCTION GetOACompTypeNum

! End of Utility Section of the Module
!******************************************************************************


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

END MODULE MixedAir

