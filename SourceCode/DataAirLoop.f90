MODULE DataAirLoop    ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   November 2003
          !       MODIFIED       L. Gu, Jan. 24, 2007. Add more variables to get information on OnOff fan operation
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module contains type definitions and variables
          ! associated with HVAC air loops (AirLoopHVAC objects).

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:

          ! DERIVED TYPE DEFINITIONS:
TYPE AirLoopZoneEquipConnectData
  CHARACTER(len=MaxNameLength)      :: AirLoopName            = ' ' ! Name of Primary Air System
  INTEGER                           :: NumReturnNodes         = 0   ! Number of return nodes connected to system
  INTEGER                           :: NumSupplyNodes         = 0   ! number of supply nodes exiting primary air system
  INTEGER                           :: NumZonesCooled         = 0   ! number of zones cooled by this primary air system
  INTEGER                           :: NumZonesHeated         = 0   ! number of zones heated by this primary air system
  INTEGER,DIMENSION(:),ALLOCATABLE  :: ZoneEquipReturnNodeNum ! Zone Equip side return air node numbers
  INTEGER,DIMENSION(:),ALLOCATABLE  :: ZoneEquipSupplyNodeNum ! Zone equip side supply air node numbers
  INTEGER,DIMENSION(:),ALLOCATABLE  :: AirLoopReturnNodeNum   ! Air loop side return air node numbers
  INTEGER,DIMENSION(:),ALLOCATABLE  :: AirLoopSupplyNodeNum   ! Air loop side supply air node numbers
  INTEGER,DIMENSION(:),ALLOCATABLE  :: CoolCtrlZoneNums       ! Controlled zone numbers of zones cooled by this air loop
  INTEGER,DIMENSION(:),ALLOCATABLE  :: HeatCtrlZoneNums       ! Controlled zone numbers of zones heated by this air loop
  INTEGER,DIMENSION(:),ALLOCATABLE  :: CoolZoneInletNodes     ! Zone inlet node numbers of zones cooled by this air loop
  INTEGER,DIMENSION(:),ALLOCATABLE  :: HeatZoneInletNodes     ! Zone inlet node numbers of zones heated by this air loop
  INTEGER,DIMENSION(:),ALLOCATABLE  :: TermUnitCoolInletNodes ! Air terminal unit cooling inlet node numbers for this air loop
  INTEGER,DIMENSION(:),ALLOCATABLE  :: TermUnitHeatInletNodes ! Air terminal unit heating inlet node numbers for this air loop
  INTEGER,DIMENSION(:),ALLOCATABLE  :: SupplyDuctType         ! 1=main, 2=cooling, 3=heating, 4=other
 END TYPE AirLoopZoneEquipConnectData

TYPE AirLoopOutsideAirConnectData
  LOGICAL                       :: OASysExists            = .false. ! true if there is an Outside Air Sys
  INTEGER                       :: OASysInletNodeNum      = 0 ! node number of return air inlet to OA sys
  INTEGER                       :: OASysOutletNodeNum     = 0 ! node number of mixed air outlet of OA sys
END TYPE AirLoopOutsideAirConnectData

TYPE DefinePriAirSysAvailMgrs
  INTEGER                                               :: NumAvailManagers =0   ! number of availability managers for this system
  INTEGER                                               :: AvailStatus      =0   ! system availability status
  INTEGER                                               :: StartTime        =0   ! cycle on time (in SimTimeSteps)
  INTEGER                                               :: StopTime         =0   ! cycle off time (in SimTimeSteps)
  REAL(r64)                                             :: ReqSupplyFrac    =0.0d0 ! required system flow rate (as a fraction)
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: AvailManagerName ! name of each availability manager
  INTEGER,DIMENSION(:),ALLOCATABLE                      :: AvailManagerType ! type of availability manager
  INTEGER, DIMENSION(:), ALLOCATABLE                    :: AvailManagerNum  ! index for availability manager
END TYPE DefinePriAirSysAvailMgrs

TYPE AirLooptoZoneData ! Derived type for air loop connection to zones on air loop
  INTEGER                            :: NumZones = 0
  INTEGER, DIMENSION(:), ALLOCATABLE :: Zone
  INTEGER, DIMENSION(:), ALLOCATABLE :: ActualZoneNumber
END TYPE AirLooptoZoneData

TYPE AirLoopControlData ! Derived type for air control information
  CHARACTER(len=MaxNameLength) :: OACtrlName    = ' '! name of OA controller
  INTEGER :: OACtrlNum    = 0      ! index of OA controller
  LOGICAL :: CyclingFan=.FALSE.    ! TRUE if currently the air loop supply fan is cycling
  LOGICAL :: AnyContFan=.FALSE.    ! TRUE if at any time supply fan is continuous
  INTEGER :: CycFanSchedPtr   =0   ! index of schedule indicating whether fan is cycling or continuous in a unitary system
  INTEGER :: FanOpMode        =0   ! 1=cycling fan cycling compressor; 2=constant fan cycling comptressor
  LOGICAL :: UnitarySys       =.FALSE. ! TRUE if a unitary system
  LOGICAL :: UnitarySysSimulating = .TRUE. ! set FALSE for AirloopUnitarySystem after simulating to downstream coils can size independently
  LOGICAL :: Simple       =.false. ! TRUE if system has 1 branch and 1 component
  LOGICAL :: CanNotLockoutEcono           =.false. ! user input says econo lockout not allowed
  LOGICAL :: CanLockoutEconoWithHeating   =.false. ! user input says econo lockout with heating is allowed
  LOGICAL :: CanLockoutEconoWithCompressor=.false.  ! user input says econo lockout with compressor is allowed
  LOGICAL :: ReqstEconoLockoutWithHeating =.false.  ! there is a request to lockout the economizer due to heating
  LOGICAL :: ReqstEconoLockoutWithCompressor=.false.  ! there is a request to lockout the economizer due to compressor operation
  LOGICAL :: EconoActive       =.false. ! if true economizer is active
  LOGICAL :: HeatRecoveryBypass=.false. ! if true heat recovery is bypassed (not active)
  LOGICAL :: ResimAirLoopFlag            = .FALSE. ! Same as SimAir, will trigger re-sim of air loops
  LOGICAL :: HeatRecoveryResimFlag       = .TRUE.  ! Used to trigger new air loop sim when HX is used in OA system
  LOGICAL :: HeatRecoveryResimFlag2      = .FALSE. ! Used to trigger new air loop sim when HX is used in OA system
  LOGICAL :: CheckHeatRecoveryBypassStatus=.false. ! determines when heat recovery bypass is set
  LOGICAL :: EconomizerFlowLocked = .false. ! locks economizer flow for custon ERV operation
  LOGICAL :: HighHumCtrlActive =.false. ! if true high humidity control is active
  LOGICAL :: EconoLockout      =.false. ! if true the economizer will be locked out (OA flow set to minimum)
  LOGICAL :: LoopFlowRateSet   =.false. ! if true then the air loop flow rate should be set using ReqSupplyFrac
  LOGICAL :: NightVent         =.false. ! if true then air loop is in night ventilation mode
  LOGICAL :: AllowWarmRestartFlag = .FALSE. ! if true then speculative warm restart is attempted after first HVAC iteration
  LOGICAL :: NewFlowRateFlag   =.FALSE. ! true whenever the air mass flow rates have changed since last air loop sim
  LOGICAL :: ConvergedFlag     =.FALSE. ! true whenever the air loop sim was converged overall
  LOGICAL :: CoolingActiveFlag =.FALSE. ! true whenever the air loop cooling coil is operating
  LOGICAL :: HeatingActiveFlag =.FALSE. ! true whenever the air loop heating coil is operating
  LOGICAL :: OASysComponentsSimulated = .FALSE. !- true after OA components have been simulated
  LOGICAL :: AirLoopDCVFlag =.true.    ! TRUE if the air loop has OA Controller specifying a Mechanical controller with DCV
                                       ! - internal flag only
END TYPE AirLoopControlData

TYPE AirLoopFlowData ! Derived type for air loop flow information
  REAL(r64) :: ZoneExhaust    =0.d0 ! total of zone exhaust air mass flow rate for this loop [kg/s]
  REAL(r64) :: ZoneExhaustBalanced = 0.d0 !zone exhaust air that is balanced by simple air flow for loop [kg/s]
  REAL(r64) :: DesSupply      =0.d0 ! design supply air mass flow rate for loop [kg/s]
  REAL(r64) :: SysToZoneDesFlowRatio =0.d0 ! System design flow divided by the sum of the zone design flows
  REAL(r64) :: TotReturn      =0.d0 ! the return air mass flow rate for this loop [kg/s]
  REAL(r64) :: ReqSupplyFrac  =1.d0 ! required flow (as a fraction of DesSupply) set by a manager
  REAL(r64) :: MinOutAir      =0.d0 ! minimum outside air mass flow rate [kg/s]
  REAL(r64) :: MaxOutAir      =0.d0 ! maximum outside air mass flow rate [kg/s]
  REAL(r64) :: OAMinFrac      =0.d0 ! minimum outside air flow fraction this time step
  REAL(r64) :: Previous       =0.d0 ! Previous mass air flow rate for this loop [kg/s]
  REAL(r64) :: SupFlow        =0.d0 ! supply air flow rate [kg/s]
  REAL(r64) :: RetFlow        =0.d0 ! return air flow rate [kg/s]
  REAL(r64) :: RetFlow0       =0.d0 ! sum of zone return flows before adjusting for total loop exhaust
  REAL(r64) :: RecircFlow     =0.d0 ! sum of zone plenum recirculated flows
  REAL(r64) :: FanPLR         =0.d0 ! Operating PLR of air loop fan
  REAL(r64) :: OAFrac         =0.d0 ! fraction of outside air to mixed air mass flow rate
  LOGICAL :: FlowError   =.FALSE. ! error flag for flow error message
END TYPE AirLoopFlowData

TYPE OAControllerData
  LOGICAL :: EconoActive       =.false. ! if true economizer is active
  LOGICAL :: HighHumCtrlActive =.false. ! if true high humidity control is active
END TYPE OAControllerData

TYPE OutsideAirSysProps
  CHARACTER(len=MaxNameLength) :: Name  = ' '
  CHARACTER(len=MaxNameLength) :: ControllerListName  = ' '
  CHARACTER(len=MaxNameLength) :: ComponentListName  = ' '
  INTEGER      :: ControllerListNum     = 0                ! index of the Controller List
  INTEGER      :: NumComponents         = 0
  INTEGER      :: NumControllers        = 0
  INTEGER      :: NumSimpleControllers  = 0                ! number of CONTROLLER:SIMPLE objects in OA Sys controller list
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: ComponentName
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: ComponentType
  INTEGER, DIMENSION(:), ALLOCATABLE :: ComponentType_Num  ! Parameterized (see above) Component Types this
                                                           ! module can address
  INTEGER, DIMENSION(:), ALLOCATABLE :: ComponentIndex     ! Which one in list -- updated by routines called from here
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: ControllerName
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: ControllerType
  INTEGER, DIMENSION(:), ALLOCATABLE :: ControllerIndex    ! Which one in list -- updated by routines called from here
END TYPE OutsideAirSysProps

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
 TYPE (AirLoopZoneEquipConnectData), ALLOCATABLE, DIMENSION(:) :: AirToZoneNodeInfo
 TYPE (AirLoopOutsideAirConnectData), ALLOCATABLE, DIMENSION(:) :: AirToOANodeInfo
 TYPE (DefinePriAirSysAvailMgrs), ALLOCATABLE, DIMENSION(:) :: PriAirSysAvailMgr
 TYPE (AirLooptoZoneData), ALLOCATABLE, DIMENSION(:) :: AirLoopZoneInfo
 TYPE (AirLoopControlData), ALLOCATABLE, DIMENSION(:) ::AirLoopControlInfo
 TYPE (AirLoopFlowData), ALLOCATABLE, DIMENSION(:) :: AirLoopFlow
 TYPE (OAControllerData), ALLOCATABLE, DIMENSION(:) :: OAControllerInfo
 TYPE (OutsideAirSysProps), ALLOCATABLE, DIMENSION(:) :: OutsideAirSys


 INTEGER :: NumOASystems=0           ! Number of Outdoor Air Systems
 INTEGER :: LoopFanOperationMode      = 0   ! OnOff fan operation mode
 REAL(r64)    :: LoopSystemOnMassFlowrate  = 0.0d0 ! Loop mass flow rate during on cycle using an OnOff fan
 REAL(r64)    :: LoopSystemOffMassFlowrate = 0.0d0 ! Loop mass flow rate during off cycle using an OnOff fan
 REAL(r64)    :: LoopOnOffFanPartLoadRatio = 0.0d0 ! OnOff fan part load ratio
 REAL(r64)    :: LoopHeatingCoilMaxRTF     = 0.0d0 ! Maximum run time fraction for electric or gas heating coil in an HVAC Air Loop
 REAL(r64)    :: LoopONOffFanRTF           = 0.0d0 ! OnOff fan run time fraction in an HVAC Air Loop
 REAL(r64)    :: LoopDXCoilRTF             = 0.0d0 ! OnOff fan run time fraction in an HVAC Air Loop
 REAL(r64)    :: LoopCompCycRatio          = 0.0d0 ! Loop compressor cycling ratio for multispeed heat pump
 LOGICAL :: AirLoopInputsFilled   = .FALSE. ! Set to TRUE after first pass through air loop

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

END MODULE DataAirLoop
