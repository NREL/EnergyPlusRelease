MODULE DataBranchNodeConnections

          ! Module containing the routines dealing with the Branch-Node connections (Component sets, Node Connections)

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This is a public data only module for Branch/Node connection data.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataGlobals, ONLY: MaxNameLength
          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
CHARACTER(len=*), PRIVATE, PARAMETER :: Blank=' '

          ! DERIVED TYPE DEFINITIONS:
TYPE ComponentListData
  CHARACTER(len=MaxNameLength)   :: ParentCType       = Blank ! Parent Object Type (Cannot be SPLITTER or MIXER)
  CHARACTER(len=MaxNameLength)   :: ParentCName       = Blank ! Parent Object Name
  CHARACTER(len=MaxNameLength)   :: CType             = Blank ! Component Type (Cannot be SPLITTER or MIXER)
  CHARACTER(len=MaxNameLength)   :: CName             = Blank ! Component Name
  CHARACTER(len=MaxNameLength)   :: InletNodeName     = Blank ! Inlet Node ID
  CHARACTER(len=MaxNameLength)   :: OutletNodeName    = Blank ! Outlet Node ID
  CHARACTER(len=MaxNameLength)   :: Description       = Blank ! Description of Component List Type
  LOGICAL                        :: InfoFilled        = .false. ! true when all information has been filled
END TYPE

TYPE NodeConnectionDef
  INTEGER                        :: NodeNumber     = 0       ! Node number of this node connection
  CHARACTER(len=MaxNameLength)   :: NodeName       = Blank   ! Node Name of this node connection
  CHARACTER(len=MaxNameLength)   :: ObjectType     = Blank   ! Object/Component Type of this node connection
  CHARACTER(len=MaxNameLength)   :: ObjectName     = Blank   ! Name of the Object/Component Type of this node connection
  CHARACTER(len=19)              :: ConnectionType = Blank   ! Connection Type (must be valid) for this node connection
  INTEGER                        :: FluidStream    = 0       ! Fluid Stream for this node connection
  LOGICAL                        :: ObjectIsParent = .false. ! Indicator whether the object is a parent or not
END TYPE

TYPE ParentListData
  CHARACTER(len=MaxNameLength)   :: CType             = Blank ! Component Type (Cannot be SPLITTER or MIXER)
  CHARACTER(len=MaxNameLength)   :: CName             = Blank ! Component Name
  CHARACTER(len=MaxNameLength)   :: InletNodeName     = Blank ! Inlet Node ID
  CHARACTER(len=MaxNameLength)   :: OutletNodeName    = Blank ! Outlet Node ID
  CHARACTER(len=MaxNameLength)   :: Description       = Blank ! Description of Component List Type
  LOGICAL                        :: InfoFilled        = .false. ! true when all information has been filled
END TYPE

TYPE EqNodeConnectionDef
  CHARACTER(len=MaxNameLength)   :: NodeName       = Blank   ! Node Name of this node connection
  CHARACTER(len=MaxNameLength)   :: ObjectType     = Blank   ! Object/Component Type of this node connection
  CHARACTER(len=MaxNameLength)   :: ObjectName     = Blank   ! Name of the Object/Component Type of this node connection
  CHARACTER(len=MaxNameLength)   :: InputFieldName = Blank   ! Input Field Name for this connection
  CHARACTER(len=19)              :: ConnectionType = Blank   ! Connection Type (must be valid) for this node connection
END TYPE


          ! MODULE VARIABLE DECLARATIONS:
TYPE (ComponentListData), ALLOCATABLE,  &
      DIMENSION(:)               :: CompSets
INTEGER                          :: NumCompSets=0             ! Number of Component Sets found in branches
INTEGER, PUBLIC                  :: NumNodeConnectionErrors=0 ! Count of node connection errors
TYPE (ParentListData), ALLOCATABLE, DIMENSION(:)       :: ParentNodeList
TYPE (NodeConnectionDef), ALLOCATABLE, DIMENSION(:)    :: NodeConnections
TYPE (NodeConnectionDef), ALLOCATABLE, DIMENSION(:)    :: tmpNodeConnections
TYPE (EqNodeConnectionDef), ALLOCATABLE, DIMENSION(:)    :: tmpEqNodeConnections
TYPE (EqNodeConnectionDef), ALLOCATABLE, DIMENSION(:)    :: AirTerminalNodeConnections

INTEGER,PUBLIC  :: NumOfNodeConnections = 0
INTEGER,PUBLIC  :: MaxNumOfNodeConnections = 0
INTEGER         :: NodeConnectionAlloc = 1000
INTEGER         :: NumOfActualParents=0
INTEGER,PUBLIC  :: NumOfAirTerminalNodes = 0
INTEGER,PUBLIC  :: MaxNumOfAirTerminalNodes = 0
INTEGER         :: EqNodeConnectionAlloc = 100

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

END MODULE DataBranchNodeConnections

