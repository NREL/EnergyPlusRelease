MODULE DataAirSystems

  ! MODULE INFORMATION:
  !       AUTHOR         Plant code authors?
  !       DATE WRITTEN
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This data-only module contains the structures for various parts of the Plant and
  ! Condenser Loops.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules (only modules that should be used here and sparingly)
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength
USE DataPlant
IMPLICIT NONE         ! Enforce explicit typing of all variables

PUBLIC ! Everything public in data-only modules

  !MODULE PARAMETER DEFINITIONS:
          ! DERIVED TYPE DEFINITIONS

TYPE AirLoopCompData  !data for an individual component
  CHARACTER(len=MaxNameLength)   :: TypeOf        =' ' ! The 'keyWord' identifying  component type
  CHARACTER(len=MaxNameLength)   :: Name          =' ' ! Component name
  INTEGER                        :: CompType_Num  = 0  ! Numeric designator for CompType (TypeOf)
  INTEGER                        :: CompIndex     = 0  ! Component Index in whatever is using this component
  INTEGER                        :: FlowCtrl      = 0  ! Component flow control (ACTIVE/PASSIVE)
  LOGICAL                        :: ON            =.true. ! When true, the designated component or operation scheme is available
  LOGICAL                        :: Parent        =.false. ! When true, the designated component is made up of sub-components
  CHARACTER(len=MaxNameLength)   :: NodeNameIn    =' ' ! Component inlet node name
  CHARACTER(len=MaxNameLength)   :: NodeNameOut   =' ' ! Component outlet node name
  INTEGER                        :: NodeNumIn     =0   ! Component inlet node number
  INTEGER                        :: NodeNumOut    =0   ! Component outlet node number
  LOGICAL                        :: MeteredVarsFound = .FALSE.
  INTEGER                        :: NumMeteredVars = 0
  INTEGER                        :: NumSubcomps    = 0
  INTEGER                        :: EnergyTransComp= 0   !1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
  REAL(r64)                      :: Capacity =0.0d0 ! ventilation load factor
  INTEGER                        :: OpMode = 0
  REAL(r64)                      :: TotPlantSupplyElec    =0.0d0
  REAL(r64)                      :: PlantSupplyElecEff    =0.0d0
  REAL(r64)                      :: PeakPlantSupplyElecEff    =0.0d0
  REAL(r64)                      :: TotPlantSupplyGas     =0.0d0
  REAL(r64)                      :: PlantSupplyGasEff     =0.0d0
  REAL(r64)                      :: PeakPlantSupplyGasEff     =0.0d0
  REAL(r64)                      :: TotPlantSupplyPurch   =0.0d0
  REAL(r64)                      :: PlantSupplyPurchEff   =0.0d0
  REAL(r64)                      :: PeakPlantSupplyPurchEff   =0.0d0
  REAL(r64)                      :: TotPlantSupplyOther   =0.0d0
  REAL(r64)                      :: PlantSupplyOtherEff   =0.0d0
  REAL(r64)                      :: PeakPlantSupplyOtherEff   =0.0d0
  INTEGER                        :: AirSysToPlantPtr      = 0 !=0 No plant loop connection, >0 index to AirSysToPlant array
  TYPE(MeterData),ALLOCATABLE, &
                   DIMENSION(:)  :: MeteredVar    !Index of energy output report data
  Type (SubcomponentData), &
            ALLOCATABLE, DIMENSION(:) :: SubComp              ! Component list
END TYPE AirLoopCompData

TYPE AirLoopBranchData ! a branch is a sequence of components
  CHARACTER(len=MaxNameLength)   :: Name              =' ' ! Name of the branch
  CHARACTER(len=MaxNameLength)   :: ControlType       =' ' ! Control type for the branch (not used)
  REAL(r64)                      :: MinVolFlowRate    =0.0d0 ! minimum flow rate for the branch (m3/s)
  REAL(r64)                      :: MaxVolFlowRate    =0.0d0 ! maximum flow rate for the branch (m3/s)
  REAL(r64)                      :: MinMassFlowRate   =0.0d0 ! minimum mass flow rate for the branch (kg/s)
  REAL(r64)                      :: MaxMassFlowRate   =0.0d0 ! maximum mass flow rate for the branch (kg/s)
  INTEGER                        :: TotalComponents   =0   ! Total number of high level components on the branch
  INTEGER,ALLOCATABLE,DIMENSION(:) :: FirstCompIndex  ! Gives the component index in AllComp that corresponds to Comp
  INTEGER,ALLOCATABLE,DIMENSION(:) :: LastCompIndex   ! Gives comp index in AllComp that corresponds to last subcomponent
  INTEGER                        :: NodeNumIn         =0   ! Branch inlet node number
  INTEGER                        :: NodeNumOut        =0   ! Branch outlet node number
  INTEGER                        :: DuctType          =0   ! 1=main, 2=cooling, 3=heating, 4=other
  TYPE(AirLoopCompData), &
           ALLOCATABLE, DIMENSION(:) :: Comp              ! Component list--high level components

!  TYPE(ExpandedCompData), &
!           ALLOCATABLE, DIMENSION(:) :: MegaComp              ! Component list
!  This list would include children, grandchildren, etc.
  INTEGER                        :: TotalNodes        = 0 ! total number of nodes on branch
  INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeNum           ! node list (numbers)
END TYPE AirLoopBranchData

TYPE AirLoopSplitterData ! a splitter joins 1 inlet branch to multiple outlet branches
  LOGICAL                        :: Exists            =.false. ! True if there is a splitter (only 1 allowed per loop)
  CHARACTER(len=MaxNameLength)   :: Name              =' ' ! Name of the Splitter
  INTEGER                        :: NodeNumIn         =0   ! Node number for the inlet to the splitter
  INTEGER                        :: BranchNumIn       =0   ! Reference number for branch connected to splitter inlet
  CHARACTER(len=MaxNameLength)   :: NodeNameIn        =' ' ! Node name for the inlet to the splitter
  INTEGER                        :: TotalOutletNodes  =0   ! Number of outlet nodes for the splitter
  INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeNumOut        ! Node numbers for the outlets to the splitter
  INTEGER, ALLOCATABLE, DIMENSION(:) :: BranchNumOut      ! Reference numbers for branches connected to splitter outlet
  CHARACTER(len=MaxNameLength), &
           ALLOCATABLE, DIMENSION(:) :: NodeNameOut       ! Node names for the outlets to the splitter
END TYPE AirLoopSplitterData

TYPE AirLoopMixerData ! a mixer joins multiple inlet branches to a single outlet branch
  LOGICAL                        :: Exists            =.false. ! True if there is a Mixer (only 1 allowed per loop)
  CHARACTER(len=MaxNameLength)   :: Name              =' ' ! Name of the Mixer
  INTEGER                        :: NodeNumOut        =0   ! Node number for the outlet to the mixer
  INTEGER                        :: BranchNumOut      =0   ! Reference number for branch connected to mixer outlet
  CHARACTER(len=MaxNameLength)   :: NodeNameOut       =' ' ! Node name for the outlet to the mixer
  INTEGER                        :: TotalInletNodes   =0   ! Number of inlet nodes for the mixer
  INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeNumIn         ! Node numbers for the inlets to the mixer
  INTEGER, ALLOCATABLE, DIMENSION(:) :: BranchNumIn       ! Reference numbers for branches connected to mixer inlet
  CHARACTER(len=MaxNameLength), &
           ALLOCATABLE, DIMENSION(:) :: NodeNameIn        ! Node names for the inlets to the mixer
END TYPE AirLoopMixerData

! DefinePrimaryAirSystem contains the data for a primary air HVAC system
TYPE :: DefinePrimaryAirSystem ! There is an array of these for each primary air system
  CHARACTER(len=MaxNameLength)                      :: Name =' '               ! name of the system
  REAL(r64)                                         :: DesignVolFlowRate = 0.0d0 ! the design total supply air flow rate (m3/s)
  INTEGER                                           :: NumControllers    = 0   ! number of controllers on this air path
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: ControllerName ! name of each controller on this system
  CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: ControllerType ! type of each controller on this system
  INTEGER,DIMENSION(:),ALLOCATABLE                      :: ControllerIndex
  LOGICAL,DIMENSION(:),ALLOCATABLE                      :: CanBeLockedOutByEcono ! true if controller inactive
                                                                                 ! when the economizer is active
  INTEGER                                           :: NumBranches       = 0   ! number of branches making up this system
  TYPE (AirLoopBranchData),DIMENSION(:),ALLOCATABLE :: Branch ! data for each branch
  TYPE (AirLoopSplitterData)                        :: Splitter ! Data for splitter (if any)
  TYPE (AirLoopMixerData)                           :: Mixer    ! Data for mixer (if any)
  Logical,DIMENSION(:),ALLOCATABLE                  :: ControlConverged ! Convergence Parameter for controllers
  INTEGER                                           :: NumOutletBranches = 0
  INTEGER,DIMENSION(3)                              :: OutletBranchNum   = 0 ! branch numbers of system outlets
  INTEGER                                           :: NumInletBranches  = 0
  INTEGER,DIMENSION(3)                              :: InletBranchNum    = 0 ! branch number of system inlets
  LOGICAL                                           :: OASysExists       = .false. ! true if there is an Outside Air Sys
  INTEGER                                           :: OASysInletNodeNum = 0 ! node number of return air inlet to OA sys
  INTEGER                                           :: OASysOutletNodeNum= 0 ! node number of mixed air outlet of OA sys
  INTEGER                                           :: OAMixOAInNodeNum  = 0 ! node number of the OA stream inlet to the
                                                                             ! OA mixer component.
  LOGICAL                                           :: RABExists         = .false. ! true if there is a RAB
  INTEGER                                           :: RABMixInNode      = 0 ! node num of RAB mixer inlet
  INTEGER                                           :: SupMixInNode      = 0 ! node num of supply air inlet to mixer
  INTEGER                                           :: MixOutNode        = 0 ! outlet node of mixer
  INTEGER                                           :: RABSplitOutNode   = 0 ! node num of RAB splitter outlet
  INTEGER                                           :: OtherSplitOutNode = 0 ! node num of nonRAB splitter outlet
  INTEGER                                           :: NumOACoolCoils    = 0 ! number of cooling coils in the outside air system
  INTEGER                                           :: NumOAHeatCoils    = 0 ! number of heating coils in the outside air system
  LOGICAL                                           :: SizeAirloopCoil   = .TRUE. ! simulates air loop coils before calling controllers
END TYPE DefinePrimaryAirSystem

          ! The ConnectionPoint derived type is used to link quickly between loops at connection points
          ! and avoids the need for repetitive searches.
TYPE ConnectionPoint
  INTEGER :: LoopType  =0
  INTEGER :: LoopNum   =0
  INTEGER :: BranchNum =0
  INTEGER :: CompNum   =0
END TYPE ConnectionPoint

TYPE ConnectZoneComp
  INTEGER :: ZoneEqListNum       =0
  INTEGER :: ZoneEqCompNum       =0
  INTEGER :: PlantLoopType       =0
  INTEGER :: PlantLoopNum        =0
  INTEGER :: PlantLoopBranch     =0
  INTEGER :: PlantLoopComp       =0
  INTEGER :: FirstDemandSidePtr  =0
  INTEGER :: LastDemandSidePtr   =0
END TYPE ConnectZoneComp

TYPE ConnectZoneSubComp
  INTEGER :: ZoneEqListNum       =0
  INTEGER :: ZoneEqCompNum       =0
  INTEGER :: ZoneEqSubCompNum    =0
  INTEGER :: PlantLoopType       =0
  INTEGER :: PlantLoopNum        =0
  INTEGER :: PlantLoopBranch     =0
  INTEGER :: PlantLoopComp       =0
  INTEGER :: FirstDemandSidePtr  =0
  INTEGER :: LastDemandSidePtr   =0
END TYPE ConnectZoneSubComp

TYPE ConnectZoneSubSubComp
  INTEGER :: ZoneEqListNum       =0
  INTEGER :: ZoneEqCompNum       =0
  INTEGER :: ZoneEqSubCompNum    =0
  INTEGER :: ZoneEqSubSubCompNum =0
  INTEGER :: PlantLoopType       =0
  INTEGER :: PlantLoopNum        =0
  INTEGER :: PlantLoopBranch     =0
  INTEGER :: PlantLoopComp       =0
  INTEGER :: FirstDemandSidePtr  =0
  INTEGER :: LastDemandSidePtr   =0
END TYPE ConnectZoneSubSubComp

TYPE ConnectAirSysComp
  INTEGER :: AirLoopNum          =0
  INTEGER :: AirLoopBranch       =0
  INTEGER :: AirLoopComp         =0
  INTEGER :: PlantLoopType       =0
  INTEGER :: PlantLoopNum        =0
  INTEGER :: PlantLoopBranch     =0
  INTEGER :: PlantLoopComp       =0
  INTEGER :: FirstDemandSidePtr  =0
  INTEGER :: LastDemandSidePtr   =0
END TYPE ConnectAirSysComp

TYPE ConnectAirSysSubComp
  INTEGER :: AirLoopNum          =0
  INTEGER :: AirLoopBranch       =0
  INTEGER :: AirLoopComp         =0
  INTEGER :: AirLoopSubComp      =0
  INTEGER :: PlantLoopType       =0
  INTEGER :: PlantLoopNum        =0
  INTEGER :: PlantLoopBranch     =0
  INTEGER :: PlantLoopComp       =0
  INTEGER :: FirstDemandSidePtr  =0
  INTEGER :: LastDemandSidePtr   =0
END TYPE ConnectAirSysSubComp

TYPE ConnectAirSysSubSubComp
  INTEGER :: AirLoopNum          =0
  INTEGER :: AirLoopBranch       =0
  INTEGER :: AirLoopComp         =0
  INTEGER :: AirLoopSubComp      =0
  INTEGER :: AirLoopSubSubComp   =0
  INTEGER :: PlantLoopType       =0
  INTEGER :: PlantLoopNum        =0
  INTEGER :: PlantLoopBranch     =0
  INTEGER :: PlantLoopComp       =0
  INTEGER :: FirstDemandSidePtr  =0
  INTEGER :: LastDemandSidePtr   =0
END TYPE ConnectAirSysSubSubComp


          ! INTERFACE BLOCK SPECIFICATIONS
          ! None

          ! MODULE VARIABLE DECLARATIONS
! For each type of air path, define an array of DefineAirPaths
TYPE (DefinePrimaryAirSystem), ALLOCATABLE, DIMENSION(:) :: PrimaryAirSystem
TYPE (ConnectionPoint), ALLOCATABLE, DIMENSION(:) :: DemandSideConnect        ! Connections between loops
TYPE (ConnectZoneComp), ALLOCATABLE, DIMENSION(:)   :: ZoneCompToPlant        ! Connections between loops
TYPE (ConnectZoneSubComp), ALLOCATABLE, DIMENSION(:)   :: ZoneSubCompToPlant        ! Connections between loops
TYPE (ConnectZoneSubSubComp), ALLOCATABLE, DIMENSION(:)   :: ZoneSubSubCompToPlant        ! Connections between loops
TYPE (ConnectAirSysComp), ALLOCATABLE, DIMENSION(:)   :: AirSysCompToPlant        ! Connections between loops
TYPE (ConnectAirSysSubComp), ALLOCATABLE, DIMENSION(:)   :: AirSysSubCompToPlant        ! Connections between loops
TYPE (ConnectAirSysSubSubComp), ALLOCATABLE, DIMENSION(:)   :: AirSysSubSubCompToPlant        ! Connections between loops

! Temporary arrays
TYPE (ConnectionPoint), ALLOCATABLE, DIMENSION(:) :: TempDemandSideConnect
TYPE (ConnectZoneComp), ALLOCATABLE, DIMENSION(:)   :: TempZoneCompToPlant        ! Connections between loops
TYPE (ConnectZoneSubComp), ALLOCATABLE, DIMENSION(:)   :: TempZoneSubCompToPlant        ! Connections between loops
TYPE (ConnectZoneSubSubComp), ALLOCATABLE, DIMENSION(:)   :: TempZoneSubSubCompToPlant        ! Connections between loops
TYPE (ConnectAirSysComp), ALLOCATABLE, DIMENSION(:)   :: TempAirSysCompToPlant        ! Connections between loops
TYPE (ConnectAirSysSubComp), ALLOCATABLE, DIMENSION(:)   :: TempAirSysSubCompToPlant        ! Connections between loops
TYPE (ConnectAirSysSubSubComp), ALLOCATABLE, DIMENSION(:)   :: TempAirSysSubSubCompToPlant        ! Connections between loops


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


END MODULE DataAirSystems

