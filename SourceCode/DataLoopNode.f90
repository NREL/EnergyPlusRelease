MODULE DataLoopNode

  ! MODULE INFORMATION:
  !       AUTHOR         Development Team
  !       DATE WRITTEN   1996...
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This details the data structure for "nodes" -- the basis of the HVAC-Plant
  ! structure for EnergyPlus.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Must declare all variables

PUBLIC   ! Data Only Module

  !MODULE PARAMETER DEFINITIONS:
  ! Valid Fluid Types for Nodes
  INTEGER, PARAMETER :: NodeType_Unknown = 0  ! 'blank'
  INTEGER, PARAMETER :: NodeType_Air     = 1  ! 'Air'
  INTEGER, PARAMETER :: NodeType_Water   = 2  ! 'Water'
  INTEGER, PARAMETER :: NodeType_Steam   = 3  ! 'Steam'
  INTEGER, PARAMETER :: NodeType_Electric= 4  ! 'Electric'
  CHARACTER(len=*), PARAMETER, DIMENSION(0:4) :: ValidNodeFluidTypes=  &
                     (/'blank   ',  &
                       'Air     ',  &
                       'Water   ',  &
                       'Steam   ',  &
                       'Electric'/)
  INTEGER, PARAMETER :: NumValidNodeFluidTypes=4

  ! Valid Connection Types for Nodes
  CHARACTER(len=*), PARAMETER, DIMENSION(15) :: ValidConnectionTypes=  &
                     (/'Inlet              ',  &
                       'Outlet             ',  &
                       'Internal           ',  &
                       'ZoneNode           ',  &
                       'Sensor             ',  &
                       'Actuator           ',  &
                       'OutdoorAir         ',  &
                       'ReliefAir          ',  &
                       'ZoneInlet          ',  &
                       'ZoneReturn         ',  &
                       'ZoneExhaust        ',  &
                       'Setpoint           ',  &
                       'Electric           ',  &
                       'OutsideAirReference',  &
                       'InducedAir         '/)

  INTEGER, PARAMETER :: NumValidConnectionTypes=15

  INTEGER, PARAMETER :: NodeConnectionType_Inlet               = 1
  INTEGER, PARAMETER :: NodeConnectionType_Outlet              = 2
  INTEGER, PARAMETER :: NodeConnectionType_Internal            = 3
  INTEGER, PARAMETER :: NodeConnectionType_ZoneNode            = 4
  INTEGER, PARAMETER :: NodeConnectionType_Sensor              = 5
  INTEGER, PARAMETER :: NodeConnectionType_Actuator            = 6
  INTEGER, PARAMETER :: NodeConnectionType_OutsideAir          = 7
  INTEGER, PARAMETER :: NodeConnectionType_ReliefAir           = 8
  INTEGER, PARAMETER :: NodeConnectionType_ZoneInlet           = 9
  INTEGER, PARAMETER :: NodeConnectionType_ZoneReturn          = 10
  INTEGER, PARAMETER :: NodeConnectionType_ZoneExhaust         = 11
  INTEGER, PARAMETER :: NodeConnectionType_Setpoint            = 12
  INTEGER, PARAMETER :: NodeConnectionType_Electric            = 13
  INTEGER, PARAMETER :: NodeConnectionType_OutsideAirReference = 14
  INTEGER, PARAMETER :: NodeConnectionType_InducedAir          = 15

  ! Valid IsParent Types for Node Connections
  LOGICAL, PARAMETER    :: ObjectIsParent          = .TRUE.
  LOGICAL, PARAMETER    :: ObjectIsNotParent       = .FALSE.
  LOGICAL, PARAMETER    :: IncrementFluidStreamYes = .TRUE.
  LOGICAL, PARAMETER    :: IncrementFluidStreamNo  = .FALSE.
  REAL(r64), PARAMETER  :: SensedNodeFlagValue     = -999.d0
  REAL(r64), PARAMETER  :: SensedLoadFlagValue     = -999.d0

  ! DERIVED TYPE DEFINITIONS:
 TYPE NodeData
   INTEGER  :: FluidType                 = 0   ! must be one of the valid parameters
   INTEGER  :: FluidIndex                = 0   ! For Fluid Properties
   REAL(r64)     :: Temp                      = 0.d0 ! {C}
   REAL(r64)     :: TempMin                   = 0.d0 ! {C}
   REAL(r64)     :: TempMax                   = 0.d0 ! {C}
   REAL(r64)     :: TempSetPoint              = SensedNodeFlagValue ! {C}
   REAL(r64)     :: TempLastTimestep          = 0.d0 ! [C}   DSU
   REAL(r64)     :: MassFlowRateRequest       = 0.d0 ! {kg/s}  DSU
   REAL(r64)     :: MassFlowRate              = 0.d0 ! {kg/s}
   REAL(r64)     :: MassFlowRateMin           = 0.d0 ! {kg/s}
   REAL(r64)     :: MassFlowRateMax           = SensedNodeFlagValue ! {kg/s}
   REAL(r64)     :: MassFlowRateMinAvail      = 0.d0 ! {kg/s}
   REAL(r64)     :: MassFlowRateMaxAvail      = 0.d0 ! {kg/s}
   REAL(r64)     :: MassFlowRateSetPoint      = 0.d0 ! {kg/s}
   REAL(r64)     :: Quality                   = 0.d0 ! {0.0-1.0 vapor fraction/percent}
   REAL(r64)     :: Press                     = 0.d0 ! {Pa}
   REAL(r64)     :: Enthalpy                  = 0.d0 ! {J/kg}
   REAL(r64)     :: EnthalpyLastTimestep      = 0.d0 ! {J/kg}  DSU for steam?
   REAL(r64)     :: HumRat                    = 0.d0 ! {}
   REAL(r64)     :: HumRatMin                 = SensedNodeFlagValue ! {}
   REAL(r64)     :: HumRatMax                 = SensedNodeFlagValue ! {}
   REAL(r64)     :: HumRatSetPoint            = SensedNodeFlagValue ! {}
   REAL(r64)     :: TempSetPointHi            = SensedNodeFlagValue ! {C}
   REAL(r64)     :: TempSetPointLo            = SensedNodeFlagValue ! {C}
   REAL(r64)     :: Height                    = -1.d0 !  {m}
   !  Following are for Outdoor Air Nodes "read only"
   REAL(r64)     :: OutAirDryBulb             = 0.d0 ! {C}
   LOGICAL       :: EMSOverrideOutAirDryBulb  = .FALSE. ! if true, the EMS is calling to override outdoor air node drybulb setting
   REAL(r64)     :: EMSValueForOutAirDryBulb  = 0.d0 ! value EMS is directing to use for outdoor air node's drybulb {C}
   REAL(r64)     :: OutAirWetBulb             = 0.d0 ! {C}
   LOGICAL       :: EMSOverrideOutAirWetBulb  = .FALSE. ! if true, the EMS is calling to override outdoor air node wetbulb setting
   REAL(r64)     :: EMSValueForOutAirWetBulb  = 0.d0 ! value EMS is directing to use for outdoor air node's wetbulb {C}
   ! Contaminant
   REAL(r64)     :: CO2                       = 0.d0 ! {ppm}
   REAL(r64)     :: CO2SetPoint               = 0.d0 ! {ppm}
   REAL(r64)     :: GenContam                 = 0.d0 ! {ppm}
   REAL(r64)     :: GenContamSetPoint         = 0.d0 ! {ppm}
   LOGICAL       :: SPMNodeWetbulbRepReq      = .FALSE.  ! Set to true when node has SPM which follows wetbulb
 END TYPE NodeData

 TYPE MoreNodeData
   REAL(r64)     :: RelHumidity               = 0.d0 ! {%}
   REAL(r64)     :: ReportEnthalpy            = 0.d0 ! specific enthalpy calculated at the HVAC timestep [J/kg]
   REAL(r64)     :: VolFlowRateStdRho         = 0.d0 ! volume flow rate at standard density [m3/s]
   REAL(r64)     :: VolFlowRateCrntRho        = 0.d0 ! volume flow rate at current density, only used for air nodes [m3/s]
   REAL(r64)     :: WetbulbTemp               = 0.d0 ! wetbulb temperature [C]
   REAL(r64)     :: Density                   = 0.d0 ! reported density at current temperature [kg/m3]
   REAL(r64)     :: AirDewpointTemp           = 0.d0 ! reported system node dewpoint temperature [C]
 END TYPE MoreNodeData

 TYPE MarkedNodeData
   LOGICAL                      :: IsMarked      = .false. ! true if this is a marked node
   CHARACTER(len=MaxNameLength) :: ObjectType    = ' '     ! Object Type that needs it "marked"
   CHARACTER(len=MaxNameLength) :: ObjectName    = ' '     ! Object Name that needs it "marked"
   CHARACTER(len=MaxNameLength) :: FieldName     = ' '     ! FieldName that needs it "marked"
 END TYPE MarkedNodeData


  !MODULE VARIABLE DECLARATIONS:
 INTEGER :: NumOfNodes = 0
 INTEGER :: NumofSplitters = 0
 INTEGER :: NumofMixers = 0

 TYPE (NodeData), ALLOCATABLE, DIMENSION(:) :: Node !dim to num nodes in SimHVAC
 TYPE (NodeData) :: DefaultNodeValues=  &
     NodeData(0,          & ! FluidType
              0,          & ! FluidIndex
              0.0D0,        & ! Temp {C}
              0.0D0,        & ! TempMin {C}
              0.0D0,        & ! TempMax {C}
     SensedNodeFlagValue,   & ! TempSetPoint {C}
              0.0D0,        & ! TempLastTimeStep {C}
              0.0D0,        & ! MassFlowRateRequest {kg/s}
              0.0D0,        & ! MassFlowRate {kg/s}
              0.0D0,        & ! MassFlowRateMin {kg/s}
              0.0D0,        & ! MassFlowRateMax {kg/s}  !Objexx:Note SensedNodeFlagValue is default initializer
              0.0D0,        & ! MassFlowRateMinAvail {kg/s}
              0.0D0,        & ! MassFlowRateMaxAvail {kg/s}
              0.0D0,        & ! MassFlowRateSetPoint {kg/s}
              0.0D0,        & ! Quality {0.0-1.0 vapor fraction/percent}
              0.0D0,        & ! Press {Pa}   REAL(r64)     ::
              0.0D0,        & ! Enthalpy {J/kg}
              0.0D0,        & ! EnthalpyLastTimeStep {J/kg}
              0.0D0,        & ! HumRat {}
     SensedNodeFlagValue,   & ! HumRatMin {}
     SensedNodeFlagValue,   & ! HumRatMax {}
     SensedNodeFlagValue,   & ! HumRatSetPoint {}
     SensedNodeFlagValue,   & ! TempSetPointHi {C}
     SensedNodeFlagValue,   & ! TempSetPointLo {C}
             -1.0D0,        & ! Height {m}
              0.0D0,        & ! OutAirDryBulb {C}
              .FALSE.,      & ! EMSOverrideOutAirDryBulb
              0.0D0,        & ! EMSValueForOutAirDryBulb {C}
              0.0D0,          & ! OutAirWetBulb {C}
              .FALSE.,      & ! EMSOverrideOutAirWetBulb
              0.0D0,        & ! EMSValueForOutAirWetBulb {C}
              0.0D0,        & ! CO2 {ppm}
              0.0D0,        & ! CO2 setpoint {ppm}
              0.0D0,        & ! Generic contaminant {ppm}
              0.0D0,        & ! Generic contaminant setpoint {ppm}
              .FALSE.)        ! Set to true when node has SPM which follows wetbulb

 TYPE (MoreNodeData), ALLOCATABLE, DIMENSION(:) :: MoreNodeInfo
 TYPE (MarkedNodeData), ALLOCATABLE, DIMENSION(:) :: MarkedNode

 ! You will be tempted to put the following into the Node Derived type as
 ! the "Name" for the Node.  Don't do it!!!  Several areas of the code have
 ! the following assignments:  Node(somenodenumber)=Node(someothernodenumber) to
 ! set/update Node conditions.  If the Node derived type would include the name
 ! then the name would get changed and bad things would result...
 CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: NodeID


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

END MODULE DataLoopNode
