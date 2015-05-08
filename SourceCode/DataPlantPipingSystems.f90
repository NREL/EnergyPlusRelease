MODULE DataPlantPipingSystems

          ! Module containing the data structures dealing with the PlantPipingSystems

          ! MODULE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Contains all the data structures for PlantPipingSystems

          ! METHODOLOGY EMPLOYED:
          ! A pseudo-object-oriented approach is taken to use inheritance in the structure.
          ! For example, an abstract base cell class is defined with temperatures and other
          !  generic properties, then different cell types inherit from this by including it
          !  as a MyBase field within its own structure.  Not exactly OO inheritance, but
          !  it's close, and it increases code reuse, that's for sure!
          ! Enumerations are defined first with an EnumClassName_InstanceName format
          !

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals, ONLY: Pi, MaxNameLength
    USE DataPrecisionGlobals, ONLY: r64


    IMPLICIT NONE ! Enforce explicit typing of all variables

    PUBLIC ! Everything public for this data-only module

          ! MODULE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: PartitionType_BasementWall = -1
    INTEGER, PARAMETER :: PartitionType_BasementFloor = -2
    INTEGER, PARAMETER :: PartitionType_Pipe = -3

    INTEGER, PARAMETER :: RegionType_Pipe = -1
    INTEGER, PARAMETER :: RegionType_BasementWall = -2
    INTEGER, PARAMETER :: RegionType_BasementFloor = -3
    INTEGER, PARAMETER :: RegionType_XDirection = -4
    INTEGER, PARAMETER :: RegionType_YDirection = -5
    INTEGER, PARAMETER :: RegionType_ZDirection = -6

    INTEGER, PARAMETER :: MeshDistribution_Uniform = -1
    INTEGER, PARAMETER :: MeshDistribution_SymmetricGeometric = -2

    INTEGER, PARAMETER :: SegmentFlow_IncreasingZ = -1
    INTEGER, PARAMETER :: SegmentFlow_DecreasingZ = -2

    INTEGER, PARAMETER :: Direction_PositiveY = -1
    INTEGER, PARAMETER :: Direction_NegativeY = -2
    INTEGER, PARAMETER :: Direction_PositiveX = -3
    INTEGER, PARAMETER :: Direction_NegativeX = -4
    INTEGER, PARAMETER :: Direction_PositiveZ = -5
    INTEGER, PARAMETER :: Direction_NegativeZ = -6

    INTEGER, PARAMETER :: CellType_Unknown = -1
    INTEGER, PARAMETER :: CellType_Pipe = -2
    INTEGER, PARAMETER :: CellType_GeneralField = -3
    INTEGER, PARAMETER :: CellType_GroundSurface = -4
    INTEGER, PARAMETER :: CellType_FarfieldBoundary = -5
    INTEGER, PARAMETER :: CellType_AdiabaticWall = -6
    INTEGER, PARAMETER :: CellType_BasementWall = -7
    INTEGER, PARAMETER :: CellType_BasementFloor = -8
    INTEGER, PARAMETER :: CellType_BasementCorner = -9
    INTEGER, PARAMETER :: CellType_BasementCutaway = -10

          ! DERIVED TYPE DEFINITIONS:
    TYPE BaseThermalPropertySet
      REAL(r64) :: Conductivity = 0.0d0  !W/mK
      REAL(r64) :: Density = 0.0d0       !kg/m3
      REAL(r64) :: SpecificHeat = 0.0d0  !J/kgK
    END TYPE

    TYPE ExtendedFluidProperties ! : Inherits BaseThermalPropertySet
      TYPE(BaseThermalPropertySet) :: MyBase
      REAL(r64) :: Viscosity  !kg/m-s
      REAL(r64) :: Prandtl    !-
    END TYPE

    TYPE BaseCell
      REAL(r64) :: Temperature = 0.0d0 !C
      REAL(r64) :: Temperature_PrevIteration = 0.0d0 !C
      REAL(r64) :: Temperature_PrevTimeStep = 0.0d0 !C
      REAL(r64) :: Beta = 0.0d0 !K/W
      TYPE(BaseThermalPropertySet) :: Properties
    END TYPE

    TYPE RadialCellInformation ! : Inherits BaseCell
        TYPE(BaseCell) :: MyBase
        REAL(r64) :: RadialCentroid
        REAL(r64) :: InnerRadius
        REAL(r64) :: OuterRadius
    END TYPE

    TYPE FluidCellInformation ! : Inherits BaseCell
        TYPE(BaseCell) :: MyBase
        REAL(r64) :: PipeInnerRadius
        REAL(r64) :: Volume
        TYPE(ExtendedFluidProperties) :: Properties
    END TYPE

    TYPE CartesianPipeCellInformation ! Specialized cell information only used by cells which contain pipes
        TYPE(RadialCellInformation), ALLOCATABLE, DIMENSION(:) :: Soil
        TYPE(RadialCellInformation) :: Insulation
        TYPE(RadialCellInformation) :: Pipe
        TYPE(FluidCellInformation) :: Fluid
        REAL(r64) :: RadialSliceWidth
        REAL(r64) :: InterfaceVolume
    END TYPE

    TYPE Point
        INTEGER :: X
        INTEGER :: Y
    END TYPE

    TYPE PointF
        REAL(r64) :: X
        REAL(r64) :: Y
    END TYPE

    TYPE Point3DInteger
        INTEGER :: X
        INTEGER :: Y
        INTEGER :: Z
    END TYPE

    TYPE Point3DReal
        REAL(r64) :: X
        REAL(r64) :: Y
        REAL(r64) :: Z
    END TYPE

    TYPE DomainRectangle
        INTEGER :: XMin
        INTEGER :: XMax
        INTEGER :: Ymin
        INTEGER :: YMax
    END TYPE

    TYPE MeshPartition
        REAL(r64) :: rDimension
        INTEGER :: PartitionType !From Enum: ParitionType
        REAL(r64) :: TotalWidth
    END TYPE

    TYPE GridRegion
        REAL(r64) :: Min
        REAL(r64) :: Max
        INTEGER :: RegionType !From Enum: RegionType
        REAL(r64), ALLOCATABLE, DIMENSION(:) :: CellWidths
    END TYPE

    TYPE TempGridRegionData
        REAL(r64) :: Min
        REAL(r64) :: Max
        INTEGER   :: RegionType !From Enum: RegionType
    END TYPE

    TYPE RectangleF
        REAL(r64) :: X_min
        REAL(r64) :: Y_min
        REAL(r64) :: Width
        REAL(r64) :: Height
    END TYPE

    TYPE NeighborInformation
        REAL(r64) :: ThisCentroidToNeighborCentroid
        REAL(r64) :: ThisCentroidToNeighborWall
        REAL(r64) :: ThisWallToNeighborCentroid
        REAL(r64) :: ConductionResistance
        TYPE(Point3DInteger) :: NeighborCellIndeces
    END TYPE

    TYPE RadialSizing
        REAL(r64) :: InnerDia
        REAL(r64) :: OuterDia
    END TYPE

    TYPE DirectionNeighbor_Dictionary
        INTEGER :: Direction !From Enum: Direction
        TYPE(NeighborInformation) :: Value
    END TYPE

    TYPE CartesianCell
        TYPE(BaseCell) :: MyBase
        INTEGER :: X_index
        INTEGER :: Y_index
        INTEGER :: Z_index
        REAL(r64) :: X_min
        REAL(r64) :: X_max
        REAL(r64) :: Y_min
        REAL(r64) :: Y_max
        REAL(r64) :: Z_min
        REAL(r64) :: Z_max
        TYPE(Point3DReal) :: Centroid
        INTEGER :: CellType !From Enum: CellType
        INTEGER :: PipeIndex
        TYPE(DirectionNeighbor_Dictionary), ALLOCATABLE, DIMENSION(:) :: NeighborInformation
        TYPE(CartesianPipeCellInformation) :: PipeCellData
    END TYPE

    !Input data structure
    TYPE MeshExtents
        REAL(r64) :: Xmax  = 0.0d0
        REAL(r64) :: Ymax  = 0.0d0
        REAL(r64) :: Zmax  = 0.0d0
    END TYPE

    TYPE DistributionStructure
        INTEGER :: MeshDistribution =0 !From Enum: MeshDistribution
        INTEGER :: RegionMeshCount=0
        REAL(r64) :: GeometricSeriesCoefficient  = 0.0d0
    END TYPE

    TYPE MeshProperties
        TYPE(DistributionStructure) :: X
        TYPE(DistributionStructure) :: Y
        TYPE(DistributionStructure) :: Z
    END TYPE

    TYPE SimulationControl
        REAL(r64) :: MinimumTemperatureLimit = -1000.0d0
        REAL(r64) :: MaximumTemperatureLimit = 1000.0d0
        REAL(r64) :: Convergence_CurrentToPrevIteration  = 0.0d0
        INTEGER :: MaxIterationsPerTS=0
    END TYPE

    TYPE FarfieldInfo
        REAL(r64) :: AverageGroundTemperature  = 0.0d0 !C
        REAL(r64) :: AverageGroundTemperatureAmplitude  = 0.0d0 !C
        REAL(r64) :: PhaseShiftOfMinGroundTempDays  = 0.0d0 !days
        REAL(r64) :: PhaseShiftOfMinGroundTemp  = 0.0d0 !seconds
    END TYPE

    TYPE BasementZoneInfo
        REAL(r64) :: Depth  = 0.0d0 !m
        REAL(r64) :: Width  = 0.0d0 !m
        LOGICAL :: ShiftPipesByWidth=.false.
        CHARACTER(len=MaxNameLength) :: WallBoundaryOSCMName=' '
        INTEGER :: WallBoundaryOSCMIndex=0
        CHARACTER(len=MaxNameLength) :: FloorBoundaryOSCMName=' '
        INTEGER :: FloorBoundaryOSCMIndex=0
        INTEGER, ALLOCATABLE, DIMENSION(:) :: WallSurfacePointers
        INTEGER, ALLOCATABLE, DIMENSION(:) :: FloorSurfacePointers
        INTEGER :: BasementWallXIndex = -1
        INTEGER :: BasementFloorYIndex = -1
    END TYPE

    ! Internal structure

    TYPE DirectionReal_Dictionary
        INTEGER :: Direction=0 !From Enum: Direction
        REAL(r64) :: Value  = 0.0d0
    END TYPE

    TYPE ReportingInformation
        TYPE(DirectionReal_Dictionary), ALLOCATABLE, DIMENSION(:) :: SurfaceHeatTransfer
        REAL(r64) :: TotalBoundaryHeatTransfer  = 0.0d0
        REAL(r64) :: EnergyStoredInCells  = 0.0d0
        REAL(r64) :: AverageSurfaceTemperature  = 0.0d0
        REAL(r64) :: PipeCircuitHeatTransferMCpDT  = 0.0d0
        REAL(r64) :: PipeCircuitHeatTransferUADT  = 0.0d0
        REAL(r64) :: BasementWallHeatTransfer  = 0.0d0
        REAL(r64) :: BasementFloorHeatTransfer  = 0.0d0
        REAL(r64) :: AverageBasementFloorTemperature  = 0.0d0
        REAL(r64) :: AverageBasementWallTemperature  = 0.0d0
    END TYPE

    TYPE MeshPartitions
        TYPE(MeshPartition), ALLOCATABLE, DIMENSION(:) :: X
        TYPE(MeshPartition), ALLOCATABLE, DIMENSION(:) :: Y
    END TYPE

    TYPE MoistureInfo
        REAL(r64) :: Theta_liq = 0.3d0 !volumetric moisture content of the soil
        REAL(r64) :: Theta_sat = 0.5d0 !volumetric moisture content of soil at saturation
        REAL(r64) :: GroundCoverCoefficient = 0.408d0
    END TYPE

    !Simulation data structures

    ! 'Current' data structure for variables, this is one-per-domain
    TYPE CurSimConditionsInfo
        !Simulation conditions
        REAL(r64) :: PrevSimTimeSeconds = -1.0d0
        REAL(r64) :: CurSimTimeSeconds  = 0.0d0
        REAL(r64) :: CurSimTimeStepSize  = 0.0d0
        !Environmental conditions
        REAL(r64) :: CurAirTemp = 10.0d0
        REAL(r64) :: CurWindSpeed = 2.6d0
        REAL(r64) :: CurIncidentSolar = 0.0d0
        REAL(r64) :: CurRelativeHumidity = 100.0d0
    END TYPE

    TYPE PipeSegmentInfo

        ! ID
        CHARACTER(len=MaxNameLength) :: Name

        ! Misc inputs
        TYPE(PointF) :: PipeLocation
        TYPE(Point)  :: PipeCellCoordinates
        INTEGER      :: FlowDirection =0 !From Enum: SegmentFlow

        ! Pointer to parent pipe circuit
        INTEGER :: ParentCircuitIndex=0

        ! Reporting variables
        REAL(r64) :: InletTemperature  = 0.0d0
        REAL(r64) :: OutletTemperature  = 0.0d0
        REAL(r64) :: FluidHeatLoss  = 0.0d0

        ! Error handling flags
        LOGICAL :: PipeCellCoordinatesSet = .FALSE.

        ! Other flags
        LOGICAL :: IsActuallyPartOfAHorizontalTrench = .FALSE.

    END TYPE

    TYPE PipeCircuitInfo

        ! ID
        CHARACTER(len=MaxNameLength) :: Name=' '

        ! Inlet and outlet information
        CHARACTER(len=MaxNameLength) :: InletNodeName=' '
        CHARACTER(len=MaxNameLength) :: OutletNodeName=' '
        INTEGER                      :: InletNodeNum=0
        INTEGER                      :: OutletNodeNum=0
        TYPE(Point3DInteger)         :: CircuitInletCell
        TYPE(Point3DInteger)         :: CircuitOutletCell

        ! Names and pointers to pipe segments found in this pipe circuit
        CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: PipeSegmentNames
        INTEGER, ALLOCATABLE, DIMENSION(:) :: PipeSegmentIndeces

        ! Pointer to the domain which contains this pipe circuit
        INTEGER :: ParentDomainIndex=0

        ! Misc inputs
        TYPE(RadialSizing) :: PipeSize
        TYPE(RadialSizing) :: InsulationSize
        REAL(r64)          :: RadialMeshThickness    =0.0d0
        LOGICAL            :: HasInsulation=.false.
        REAL(r64)          :: DesignVolumeFlowRate    =0.0d0
        REAL(r64)          :: DesignMassFlowRate    =0.0d0
        REAL(r64)          :: Convergence_CurrentToPrevIteration    =0.0d0
        INTEGER            :: MaxIterationsPerTS    =0
        INTEGER            :: NumRadialCells    =0
        TYPE(BaseThermalPropertySet) :: PipeProperties
        TYPE(BaseThermalPropertySet) :: InsulationProperties

        ! A list of 3d cell indeces that span the entire length of this pipe circuit (useful for reporting)
        TYPE(Point3DInteger), ALLOCATABLE, DIMENSION(:) :: ListOfCircuitPoints

        ! Flags
        LOGICAL :: CheckEquipName = .TRUE.
        LOGICAL :: NeedToFindOnPlantLoop = .TRUE.
        LOGICAL :: IsActuallyPartOfAHorizontalTrench = .FALSE.

        ! Location of this pipe circuit in the PlantLoop topology
        INTEGER :: LoopNum    =0
        INTEGER :: LoopSideNum    =0
        INTEGER :: BranchNum    =0
        INTEGER :: CompNum    =0

        ! Current fluid property values
        REAL(r64) :: CurFluidDensity = 998.0d0
        REAL(r64) :: CurFluidViscosity = 0.0015d0
        REAL(r64) :: CurFluidConductivity = 0.58d0
        REAL(r64) :: CurFluidPrandtl = 7.0d0
        REAL(r64) :: CurFluidSpecificHeat = 4190.0d0
        TYPE(ExtendedFluidProperties) :: CurFluidPropertySet

        ! Variables used to pass information from INIT-type routines to CALC-type routines
        REAL(r64) :: CurCircuitInletTemp = 23.0d0
        REAL(r64) :: CurCircuitFlowRate = 0.1321d0
        REAL(r64) :: CurCircuitConvectionCoefficient =0.0d0

        ! Reporting variables
        REAL(r64) :: InletTemperature =0.0d0
        REAL(r64) :: OutletTemperature =0.0d0
        REAL(r64) :: FluidHeatLoss =0.0d0

    END TYPE

    TYPE FullDomainStructureInfo

        ! ID
        CHARACTER(LEN=MaxNameLength) :: Name=' '

        ! Names and pointers to circuits found in this domain
        CHARACTER(LEN=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: CircuitNames
        INTEGER, ALLOCATABLE, DIMENSION(:) :: CircuitIndeces

        ! Flag variables
        LOGICAL :: OneTimeInit = .TRUE.
        LOGICAL :: BeginSimInit = .TRUE.
        LOGICAL :: BeginSimEnvrn = .TRUE.
        LOGICAL :: DomainNeedsSimulation = .TRUE.
        LOGICAL :: DomainNeedsToBeMeshed = .TRUE.
        LOGICAL :: IsActuallyPartOfAHorizontalTrench = .FALSE.

        ! "Input" data structure variables
        TYPE(MeshExtents) :: Extents
        TYPE(MeshProperties) :: Mesh
        TYPE(BaseThermalPropertySet) :: GroundProperties
        TYPE(SimulationControl) :: SimControls
        TYPE(FarfieldInfo) :: Farfield
        TYPE(BasementZoneInfo) :: BasementZone
        TYPE(MoistureInfo) :: Moisture

        ! "Internal" data structure variables
        TYPE(MeshPartitions) :: Partitions
        TYPE(CurSimConditionsInfo) :: Cur
        TYPE(ReportingInformation) :: Reporting
        LOGICAL :: HasBasement=.false.

        ! Main 3D cells array
        TYPE(CartesianCell), ALLOCATABLE, DIMENSION(:,:,:) :: Cells

    END TYPE FullDomainStructureInfo

          ! MODULE VARIABLE DECLARATIONS:
    TYPE(FullDomainStructureInfo), ALLOCATABLE, DIMENSION(:) :: PipingSystemDomains
    TYPE(PipeCircuitInfo),         ALLOCATABLE, DIMENSION(:) :: PipingSystemCircuits
    TYPE(PipeSegmentInfo),         ALLOCATABLE, DIMENSION(:) :: PipingSystemSegments

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


END MODULE
