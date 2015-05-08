MODULE PlantPressureSystem

          ! Module containing the routines dealing with the PlantPressureSystem simulation

          ! MODULE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       February 2010: Add phase 2: loop flow correction
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module manages plant pressure-based simulations

          ! METHODOLOGY EMPLOYED:
          ! General EnergyPlus Methodology:

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          !  Phase 1: Pump Power Correction: -Loop/Parallel flows are not resolved based on pressure drop
          !                                  -Every flow path must see at least one branch with pressure information
          !                                  -Pump power is updated based on the required pump head
          !  Phase 2: Pump Flow Correction: -Loop flow resolved based on pump curve and loop pressure drop
          !                                 -Parallel flows not resolved
          !                                 -Every flow path must see at least one branch with pressure information
          !                                 -Pump curve must be given also
          !  Phase 3: Pressure Simulation: -Loop and parallel flows are resolved
          !                                -All branches must have pressure information and pump must have pump curve
          !                                -Not currently implemented

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength,Pi
USE DataInterfaces, ONLY: ShowWarningError,ShowSevereError,ShowFatalError,ShowContinueError,SetupOutputVariable
USE DataBranchAirLoopPlant

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public


          ! MODULE PARAMETER/ENUMERATIONS DEFINITIONS:
CHARACTER(*), PARAMETER        :: Blank                   = ' '


          ! DERIVED TYPE DEFINITIONS:
!TYPE, PUBLIC:: PlantPressureCurveData
!  CHARACTER(len=MaxNameLength) :: Name                    = Blank
!  REAL(r64)                    :: EquivDiameter           = 0.0d0   !- An effective diameter for calculation of Re & e/D [m]
!  REAL(r64)                    :: MinorLossCoeff          = 0.0d0   !- K factor                                          [-]
!  REAL(r64)                    :: EquivLength             = 0.0d0   !- An effective length to apply friction calculation [m]
!  REAL(r64)                    :: EquivRoughness          = 0.0d0   !- An effective roughness (e) to calculate e/D       [m]
!  LOGICAL                      :: ConstantFpresent        = .FALSE. !- Signal for if a constant value of f was entered
!  REAL(r64)                    :: ConstantF               = 0.0d0   !- Constant value of f (if applicable)               [-]
!END TYPE PlantPressureCurveData
!
!          ! MODULE VARIABLE DECLARATIONS:
!TYPE(PlantPressureCurveData), ALLOCATABLE, DIMENSION(:),PUBLIC :: PressureCurve
!LOGICAL  :: GetInputFlag = .TRUE. !Module level, since GetInput could be called by SIMPRESSUREDROP or by BRANCHINPUTMANAGER

          ! SUBROUTINE SPECIFICATIONS FOR MODULE:

     ! Driver/Manager Routines
PUBLIC  SimPressureDropSystem

     ! Initialization routines for module
!PRIVATE GetPressureSystemInput
PRIVATE InitPressureDrop

     ! Algorithms/Calculation routines for the module
PRIVATE BranchPressureDrop
!PRIVATE PressureCurveValue
!PRIVATE CalculateMoodyFrictionFactor

     ! Update routines to check convergence and update nodes
PRIVATE UpdatePressureDrop

     ! Utility routines for module
PRIVATE DistributePressureOnBranch
PRIVATE PassPressureAcrossMixer
PRIVATE PassPressureAcrossSplitter
PRIVATE PassPressureAcrossInterface

     ! Public Utility routines
!PUBLIC GetPressureCurveTypeAndIndex
PUBLIC ResolveLoopFlowVsPressure

CONTAINS

SUBROUTINE SimPressureDropSystem(LoopNum, FirstHVACIteration, CallType, LoopSideNum, BranchNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine is the public interface for pressure system simulation
          ! Calls are made to private components as needed

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant, ONLY : PressureCall_Init, PressureCall_Calc, PressureCall_Update, PlantLoop, Press_NoPressure

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,           INTENT(IN)   :: LoopNum            ! Plant Loop to update pressure information
  LOGICAL,           INTENT(IN)   :: FirstHVACIteration ! System flag
  INTEGER,           INTENT(IN)   :: CallType           ! Enumerated call type
  INTEGER, OPTIONAL, INTENT(IN)   :: LoopSideNum        ! Loop side num for specific branch simulation
  INTEGER, OPTIONAL, INTENT(IN)   :: BranchNUm          ! Branch num for specific branch simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  !Check if we need to get pressure curve input data
!  IF (GetInputFlag) CALL GetPressureSystemInput

  !Exit out of any calculation routines if we don't do pressure simulation for this loop
  IF ((PlantLoop(LoopNum)%PressureSimType == Press_NoPressure) .AND.  &
      ((CallType == PressureCall_Calc) .OR. (CallType == PressureCall_Update))) RETURN

  !Pass to another routine based on calling flag
  SELECT CASE (CallType)
    CASE (PressureCall_Init)
      CALL InitPressureDrop(LoopNum, FirstHVACIteration)
    CASE (PressureCall_Calc)
      CALL BranchPressureDrop(LoopNum,LoopSideNum,BranchNum) !Objexx:OPTIONAL LoopSideNum, BranchNum used without PRESENT check
    CASE (PressureCall_Update)
      CALL UpdatePressureDrop(LoopNum)
    CASE DEFAULT
      !Calling routines should only use the three possible keywords here
  END SELECT

  RETURN

END SUBROUTINE SimPressureDropSystem

!=================================================================================================!

!SUBROUTINE GetPressureSystemInput()
! Getinput for PressureSystem moved to CurveManager module
!=================================================================================================!

SUBROUTINE InitPressureDrop(LoopNum, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes output variables and data structure
          ! On FirstHVAC, updates the demand inlet node pressure

          ! METHODOLOGY EMPLOYED:
          ! General EnergyPlus Methodology

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,       ONLY :  PlantLoop, DemandSide, SupplySide, Press_NoPressure, CommonPipe_No
  USE DataEnvironment, ONLY :  StdBaroPress
  USE DataLoopNode,    ONLY :  Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      ::  LoopNum
  LOGICAL, INTENT(IN)      ::  FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER       :: LoopType_Plant     = 1
  INTEGER, PARAMETER       :: LoopType_Condenser = 2

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  !Initialization Variables
  LOGICAL, SAVE            ::  OneTimeInit = .TRUE.
  LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:) :: LoopInit

  !Simulation Variables
  LOGICAL                  ::  ErrorsFound
  INTEGER                  ::  LoopSideNum
  INTEGER                  ::  CompNum
  INTEGER                  ::  BranchNum
  INTEGER                  ::  NumBranches
  INTEGER                  ::  BranchPressureTally
  LOGICAL                  ::  SeriesPressureComponentFound
  LOGICAL, DIMENSION(2)    ::  FullParallelBranchSetFound
  LOGICAL, SAVE            ::  CommonPipeErrorEncountered = .FALSE.

  IF (OneTimeInit) THEN
    !First allocate the initialization array to each plant loop
    ALLOCATE(LoopInit(SIZE(PlantLoop)))
    LoopInit = .TRUE.
    OneTimeInit = .FALSE.
  END IF

  ! CurrentModuleObject='Curve:Functional:PressureDrop'
  IF (LoopInit(LoopNum)) THEN

    !Initialize
    ErrorsFound = .FALSE.
    FullParallelBranchSetFound = .FALSE.
    SeriesPressureComponentFound = .FALSE.

    !Need to go along plant loop and set up component pressure drop data structure!
    DO LoopSideNum = DemandSide, SupplySide

      !Loop through all branches on this loop side
      DO BranchNum = 1, SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch)

        !If this branch has valid pressure drop data
        IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureCurveIndex .GT. 0) THEN

          !Update flags for higher level structure
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%HasPressureComponents = .TRUE.
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%HasPressureComponents = .TRUE.
          PlantLoop(LoopNum)%HasPressureComponents = .TRUE.

          !Setup output variable
          CALL SetupOutputVariable('Plant Branch Pressure Difference [Pa]',  &
                       PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureDrop &
                       ,'Plant','Average', PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Name)

        END IF

      END DO

      !Set up loopside level variables if applicable
      IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%HasPressureComponents) THEN
        IF (LoopSideNum==DemandSide) THEN

          CALL SetupOutputVariable('Plant Demand Side Loop Pressure Difference [Pa]',  &
                      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%PressureDrop &
                      ,'Plant','Average', PlantLoop(LoopNum)%Name)

        ELSE IF (LoopSideNum==SupplySide) THEN

          CALL SetupOutputVariable('Plant Supply Side Loop Pressure Difference [Pa]',  &
                       PlantLoop(LoopNum)%LoopSide(LoopSideNum)%PressureDrop &
                       ,'Plant','Average', PlantLoop(LoopNum)%Name)

        END IF
      END IF

    END DO

    IF (PlantLoop(LoopNum)%HasPressureComponents) THEN

      !Set up loop level variables if applicable

      CALL SetupOutputVariable('Plant Loop Pressure Difference [Pa]',  &
                     PlantLoop(LoopNum)%PressureDrop &
                     ,'Plant','Average', PlantLoop(LoopNum)%Name)


      !Check for illegal configurations on this plant loop
      DO LoopSideNum = DemandSide, SupplySide
        !Check for illegal parallel branch setups
        BranchPressureTally = 0
        NumBranches = SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch)
        IF (NumBranches.GT.2) THEN
          DO BranchNum = 2, NumBranches-1
            IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%HasPressureComponents) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%HasParallelPressComps = .TRUE.
              BranchPressureTally = BranchPressureTally + 1
            END IF
          END DO
        END IF
        IF (BranchPressureTally == 0) THEN
          !no parallel branches, ok for this check
        ELSE IF (BranchPressureTally == SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch)-2) THEN
          !all parallel branches have pressure components
          FullParallelBranchSetFound(LoopSideNum) = .TRUE.
        ELSE
          !we aren't ok
          CALL ShowSevereError('Pressure drop component configuration error detected on loop: '//PlantLoop(LoopNum)%Name)
          CALL ShowContinueError('Pressure drop components must be on ALL or NONE of the parallel branches.')
          CALL ShowContinueError('Partial distribution is not allowed.')
          ErrorsFound = .TRUE.
        END IF
        IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(1)%HasPressureComponents .OR. &
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(NumBranches)%HasPressureComponents) THEN
          !we have a series component pressure branch (whether a single branch half loop or mixer/splitter setup
          SeriesPressureComponentFound = .TRUE.
        END IF
      END DO

      !Check for full path pressure data
      IF(ANY(FullParallelBranchSetFound) .OR. (SeriesPressureComponentFound))THEN
        !we are fine, either way we will always have a path with at least one pressure component hit
      ELSE
        CALL ShowSevereError('Pressure drop component configuration error detected on loop: '//PlantLoop(LoopNum)%Name)
        CALL ShowContinueError('The loop has at least one fluid path which does not encounter a pressure component.')
        CALL ShowContinueError('Either use at least one serial component for pressure drop OR all possible parallel paths')
        CALL ShowContinueError('must be pressure drop components.')
        ErrorsFound = .TRUE.
      END IF !valid pressure path

    END IF !Has pressure components

    IF (ErrorsFound) Call ShowFatalError('Preceding errors cause program termination')

    !Also issue one time warning if there is a mismatch between plant loop simulation type and whether objects were entered
    IF (PlantLoop(LoopNum)%HasPressureComponents .AND. (PlantLoop(LoopNum)%PressureSimType == Press_NoPressure)) THEN
      !Then we found pressure components on the branches, but the plant loop said it didn't want to do pressure simulation
      CALL ShowWarningError('Error for pressure simulation on plant loop: '//PlantLoop(LoopNum)%Name)
      CALL ShowContinueError('Plant loop contains pressure simulation components on the branches,')
      CALL ShowContinueError(' yet in the PlantLoop object, there is no pressure simulation specified.')
      CALL ShowContinueError('Simulation continues, ignoring pressure simulation data.')
    ELSE IF ((.NOT. PlantLoop(LoopNum)%HasPressureComponents) .AND. (PlantLoop(LoopNum)%PressureSimType .NE. Press_NoPressure)) THEN
      !Then we don't have any pressure components on the branches, yet the plant loop wants to do some sort of pressure simulation
      CALL ShowWarningError('Error for pressure simulation on plant loop: '//PlantLoop(LoopNum)%Name)
      CALL ShowContinueError('Plant loop is requesting a pressure simulation,')
      CALL ShowContinueError(' yet there are no pressure simulation components detected on any of the branches in that loop.')
      CALL ShowContinueError('Simulation continues, ignoring pressure simulation data.')
    END IF

    LoopInit(LoopNum) = .FALSE.

  END IF !LoopInit = TRUE

  !Initialize the entire plant loop to the outdoor pressure if that loop has data
  !This value at the demand side outlet node will be used as a starting reference point
  ! for pressure calcs
  !The value is smeared across the loop, however, so that any nodes before a pump will
  ! have a proper value for pressure
  IF (PlantLoop(LoopNum)%HasPressureComponents .AND. FirstHVACIteration) THEN
    DO LoopSideNum = DemandSide, SupplySide
      DO BranchNum = 1, SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch)
        DO CompNum = 1, SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp)
          Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%Press=StdBaroPress
          Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut)%Press=StdBaroPress
        END DO
      END DO
    END DO
  END IF

  !Now tell the pump routine whether or not to use the pressure data to calculate power
  IF (PlantLoop(LoopNum)%HasPressureComponents) THEN
    IF (FirstHVACIteration) THEN
      PlantLoop(LoopNum)%UsePressureForPumpCalcs = .FALSE.
    ELSE
      PlantLoop(LoopNum)%UsePressureForPumpCalcs = .TRUE.
    END IF
  ELSE !No Pressure Components
    PlantLoop(LoopNum)%UsePressureForPumpCalcs = .FALSE.
  END IF

  !Before we leave, override any settings in case we are doing common pipe simulation
  IF (PlantLoop(LoopNum)%HasPressureComponents) THEN
    !We need to make sure we aren't doing an invalid configuration here
    IF (PlantLoop(LoopNum)%CommonPipeType .NE. CommonPipe_No) THEN
      !There is a common pipe!
      IF (.NOT. CommonPipeErrorEncountered) THEN
        CALL ShowSevereError('Invalid pressure simulation configuration for Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowContinueError('Currently pressure simulations cannot be performed for loops with common pipes.')
        CALL ShowContinueError('To repair, either remove the common pipe simulation, or remove the pressure simulation.')
        CALL ShowContinueError('The simulation will continue, but the pump power is not updated with pressure drop data.')
        CALL ShowContinueError('Check all results including node pressures to ensure proper simulation.')
        CALL ShowContinueError('This message is reported once, but may have been encountered in multiple loops.')
        CommonPipeErrorEncountered = .TRUE.
      END IF
      PlantLoop(LoopNum)%UsePressureForPumpCalcs = .FALSE.
    END IF
  END IF


END SUBROUTINE InitPressureDrop

!=================================================================================================!

SUBROUTINE BranchPressureDrop(LoopNum,LoopSideNum,BranchNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This will choose an appropriate pressure drop calculation routine based on structure flags

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus Methodology

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode,    ONLY   :  Node
  USE FluidProperties, ONLY   :  GetDensityGlycol, GetViscosityGlycol
  USE DataPlant,       ONLY   :  PlantLoop
  USE CurveManager,    ONLY   :  CurveValue, PressureCurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)       ::  LoopNum            !Plant Loop Index
  INTEGER,   INTENT(IN)       ::  LoopSideNum        !LoopSide Index (1=Demand, 2=Supply) on Plant Loop LoopNum
  INTEGER,   INTENT(IN)       ::  BranchNum          !Branch Index on LoopSide LoopSideNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName  = 'CalcPlantPressureSystem'
  CHARACTER(len=*), PARAMETER :: DummyFluid   = ' '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                     :: FluidIndex          !Plant loop level Fluid Index
  INTEGER                     :: InletNodeNum        !Component inlet node number
  INTEGER                     :: OutletNodeNum       !Component outlet node number
  INTEGER                     :: PressureCurveType   !Type of curve used to evaluate pressure drop
  INTEGER                     :: PressureCurveIndex  !Curve index for PerfCurve structure
  REAL(r64)                   :: NodeMassFlow        !Nodal mass flow rate {kg/s}
  REAL(r64)                   :: NodeTemperature     !Nodal temperature {C}
  REAL(r64)                   :: NodeDensity         !Nodal density {kg/m3}
  REAL(r64)                   :: NodeViscosity       !Nodal viscosity, assuming mu here (dynamic viscosity)
  REAL(r64)                   :: BranchDeltaPress    !Pressure drop for component, {Pa}
  INTEGER, SAVE               :: ErrorCounter = 0    !For proper error handling

  !Exit early if need be
  IF (.NOT. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%HasPressureComponents) THEN
    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureDrop = 0.0d0
    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureEffectiveK = 0.0d0
    RETURN
  END IF

  !Get data from data structure
  FluidIndex         = PlantLoop(LoopNum)%FluidIndex
  InletNodeNum       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn
  OutletNodeNum      = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumOut
  PressureCurveType  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureCurveType
  PressureCurveIndex = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureCurveIndex

  !Get nodal conditions
  NodeMassFlow       = Node(InletNodeNum)%MassFlowRate
  NodeTemperature    = Node(InletNodeNum)%Temp
  NodeDensity        = GetDensityGlycol(DummyFluid, NodeTemperature, FluidIndex, RoutineName)
  NodeViscosity      = GetViscosityGlycol(DummyFluid, NodeTemperature, FluidIndex, RoutineName)

  !Call the appropriate pressure calculation routine
  SELECT CASE (PressureCurveType)
    CASE (PressureCurve_Pressure)
      !DeltaP = [f*(L/D) + K] * (rho * V^2) / 2
      BranchDeltaPress = PressureCurveValue(PressureCurveIndex, NodeMassFlow, NodeDensity, NodeViscosity)

    CASE (PressureCurve_Generic)
      !DeltaP = func(mdot)
      !Generic curve, only pass V1=mass flow rate
      BranchDeltaPress = CurveValue(PressureCurveIndex, NodeMassFlow)

    CASE DEFAULT
      !Shouldn't end up here, but just in case
      ErrorCounter = ErrorCounter + 1
      IF (ErrorCounter == 1) THEN
        CALL ShowSevereError('Plant pressure simulation encountered a branch which contains invalid branch pressure curve type.')
        CALL ShowContinueError('Occurs for branch: '//PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Name)
        CALL ShowContinueError('This error will be issued only once, although other branches may encounter the same problem')
        CALL ShowContinueError('For now, pressure drop on this branch will be set to zero.')
        CALL ShowContinueError('Verify all pressure inputs and pressure drop output variables to ensure proper simulation')
      END IF

  END SELECT

  !Log this pressure in the data structure to be handled by the update routine later
  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureDrop = BranchDeltaPress

  !Update the effective K-value for this branch
  IF (NodeMassFlow .GT. 0.0d0) THEN
    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureEffectiveK = BranchDeltaPress / (NodeMassFlow**2.0d0)
  ELSE
    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureEffectiveK = 0.0d0
  END IF

  RETURN

END SUBROUTINE BranchPressureDrop

!=================================================================================================!

!REAL(r64) FUNCTION PressureCurveValue(PressureCurveIndex, MassFlow, Density, Viscosity)
!
!          ! FUNCTION INFORMATION:
!          !       AUTHOR         Edwin Lee
!          !       DATE WRITTEN   August 2009
!          !       MODIFIED       na
!          !       RE-ENGINEERED  na
!
!          ! PURPOSE OF THIS FUNCTION:
!          ! This will evaluate the pressure drop for components which use pressure information
!
!          ! METHODOLOGY EMPLOYED:
!          ! Friction factor pressure drop equation:
!          ! DP = [f*(L/D) + K] * (rho * V^2) / 2
!
!          ! REFERENCES:
!          ! na
!
!          ! USE STATEMENTS:
!  USE DataPlant, ONLY : MassFlowTol
!  Use DataGlobals, ONLY : Pi
!
!  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
!
!          ! FUNCTION ARGUMENT DEFINITIONS:
!  INTEGER, INTENT(IN)      ::  PressureCurveIndex
!  REAL(r64), INTENT(IN)    ::  MassFlow
!  REAL(r64), INTENT(IN)    ::  Density
!  REAL(r64), INTENT(IN)    ::  Viscosity
!
!          ! FUNCTION PARAMETER DEFINITIONS:
!          ! na
!
!          ! INTERFACE BLOCK SPECIFICATIONS:
!          ! na
!
!          ! DERIVED TYPE DEFINITIONS:
!          ! na
!
!          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
!  REAL(r64)                ::  Diameter
!  REAL(r64)                ::  MinorLossCoeff
!  REAL(r64)                ::  Length
!  REAL(r64)                ::  Roughness
!  LOGICAL                  ::  IsConstFPresent
!  REAL(r64)                ::  ConstantF
!  REAL(r64)                ::  FrictionFactor
!  REAL(r64)                ::  CrossSectArea
!  REAL(r64)                ::  Velocity
!  REAL(r64)                ::  ReynoldsNumber
!  REAL(r64)                ::  RoughnessRatio
!
!  !Retrieve data from structure
!  Diameter        = PressureCurve(PressureCurveIndex)%EquivDiameter
!  MinorLossCoeff  = PressureCurve(PressureCurveIndex)%MinorLossCoeff
!  Length          = PressureCurve(PressureCurveIndex)%EquivLength
!  Roughness       = PressureCurve(PressureCurveIndex)%EquivRoughness
!  IsConstFPresent = PressureCurve(PressureCurveIndex)%ConstantFPresent
!  ConstantF       = PressureCurve(PressureCurveIndex)%ConstantF
!
!  !Intermediate calculations
!  CrossSectArea         =  (Pi / 4.0d0) * Diameter**2
!  Velocity              =  MassFlow / (Density * CrossSectArea)
!  ReynoldsNumber        =  Density * Diameter * Velocity / Viscosity !assuming mu here
!  RoughnessRatio        =  Roughness / Diameter
!
!  !If we don't have any flow then exit out
!  IF (MassFlow .LT. MassFlowTol) THEN
!    PressureCurveValue = 0.0d0
!    RETURN
!  END IF
!
!  !Calculate the friction factor
!  IF (IsConstFPresent) THEN   !use the constant value
!    FrictionFactor    =  ConstantF
!  ELSE ! must calculate f
!    FrictionFactor    =  CalculateMoodyFrictionFactor(ReynoldsNumber,RoughnessRatio)
!  END IF
!
!  !Pressure drop calculation
!  PressureCurveValue  =  (FrictionFactor * (Length / Diameter) + MinorLossCoeff) * (Density * Velocity**2) / 2.0d0
!
!END FUNCTION PressureCurveValue
!
!=================================================================================================!

!REAL(r64) FUNCTION CalculateMoodyFrictionFactor(ReynoldsNumber, RoughnessRatio)
!
!          ! FUNCTION INFORMATION:
!          !       AUTHOR         Edwin Lee
!          !       DATE WRITTEN   August 2009
!          !       MODIFIED       na
!          !       RE-ENGINEERED  na
!
!          ! PURPOSE OF THIS FUNCTION:
!          ! This will evaluate the moody friction factor based on Reynolds number and roughness ratio
!
!          ! METHODOLOGY EMPLOYED:
!          ! General empirical correlations for friction factor based on Moody Chart data
!
!          ! REFERENCES:
!          ! Haaland, SE (1983). "Simple and Explicit Formulas for the Friction Factor in Turbulent Flow".
!          !   Trans. ASIVIE, J. of Fluids Engineering 103: 89-90.
!
!          ! USE STATEMENTS:
!  USE General, ONLY: RoundSigDigits
!
!  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
!
!          ! FUNCTION ARGUMENT DEFINITIONS:
!  REAL(r64), INTENT(IN)    ::  ReynoldsNumber
!  REAL(r64), INTENT(IN)    ::  RoughnessRatio
!
!          ! FUNCTION PARAMETER DEFINITIONS:
!          ! na
!
!          ! INTERFACE BLOCK SPECIFICATIONS:
!          ! na
!
!          ! DERIVED TYPE DEFINITIONS:
!          ! na
!
!          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
!  REAL(r64)                    ::  Term1, Term2, Term3
!  CHARACTER(len=MaxNameLength) ::  RR, Re
!  LOGICAL, SAVE                ::  FrictionFactorErrorHasOccurred = .FALSE.
!
!  !Check for no flow before calculating values
!  IF (ReynoldsNumber .EQ. 0.0d0) THEN
!    CalculateMoodyFrictionFactor = 0.0d0
!    RETURN
!  END IF
!
!  !Check for no roughness also here
!  IF (RoughnessRatio .EQ. 0.0d0) THEN
!    CalculateMoodyFrictionFactor = 0.0d0
!    RETURN
!  END IF
!
!  !Calculate the friction factor
!  Term1 = (RoughnessRatio/3.7d0)**(1.11d0)
!  Term2 = 6.9d0/ReynoldsNumber
!  Term3 = -1.8d0 * LOG10(Term1 + Term2)
!  IF (Term3 .NE. 0.0d0) THEN
!    CalculateMoodyFrictionFactor = Term3 ** (-2.0d0)
!  ELSE
!    IF (.NOT. FrictionFactorErrorHasOccurred) THEN
!      RR=RoundSigDigits(RoughnessRatio,7)
!      Re=RoundSigDigits(ReynoldsNumber,1)
!      CALL ShowSevereError('Plant Pressure System: Error in moody friction factor calculation')
!      CALL ShowContinueError('Current Conditions: Roughness Ratio='//TRIM(RR)//'; Reynolds Number='//TRIM(Re))
!      CALL ShowContinueError('These conditions resulted in an unhandled numeric issue.')
!      CALL ShowContinueError('Please contact EnergyPlus support/development team to raise an alert about this issue')
!      CALL ShowContinueError('This issue will occur only one time.  The friction factor has been reset to 0.04 for calculations')
!      FrictionFactorErrorHasOccurred = .TRUE.
!    END IF
!    CalculateMoodyFrictionFactor = 0.04d0
!  END IF
!
!  RETURN
!
!END FUNCTION CalculateMoodyFrictionFactor
!
!!=================================================================================================!

SUBROUTINE UpdatePressureDrop(LoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Evaluate the pressure drop across an entire plant loop and places the value
          ! on the PlantLoop(:) data structure for the pump to use

          ! METHODOLOGY EMPLOYED:
          ! Assumes that the supply inlet is the starting node, which will be set to some standard pressure
          ! Then we move around the loop backward from this reference point and go until we hit a pump and stop.
          ! The pressure difference from reference to pump is the new required pump head.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,       ONLY :  PlantLoop, DemandSide, SupplySide
  USE DataLoopNode,    ONLY :  Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      ::  LoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  ::  LoopSideNum
  INTEGER                  ::  BranchNum
  INTEGER                  ::  NumBranches
  REAL(r64)                ::  BranchPressureDrop
  REAL(r64)                ::  LoopSidePressureDrop
  REAL(r64)                ::  LoopPressureDrop
  REAL(r64), ALLOCATABLE, DIMENSION(:) ::  ParallelBranchPressureDrops
  REAL(r64), ALLOCATABLE, DIMENSION(:) ::  ParallelBranchInletPressures
  INTEGER                  ::  ParallelBranchCounter
  REAL(r64)                ::  SplitterInletPressure
  REAL(r64)                ::  MixerPressure
  LOGICAL                  ::  FoundAPumpOnBranch
  REAL(r64)                ::  EffectiveLoopKValue
  REAL(r64)                ::  EffectiveLoopSideKValue
  REAL(r64)                ::  TempVal_SumOfOneByRootK

  !Exit if not needed
  IF (.NOT. PlantLoop(LoopNum)%HasPressureComponents) RETURN

  !Now go through and update the pressure drops as needed
  FoundAPumpOnBranch = .FALSE.
  LoopPressureDrop = 0.0d0
  DO LoopSideNum = DemandSide, SupplySide !Start at demand side outlet

    !Loop through all branches on this loop side
    LoopSidePressureDrop = 0.0d0
    NumBranches = SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch)

    !Split here based on a single branch loop or a splitter/mixer configuration
    IF (NumBranches == 1) THEN  !Just do the single branch

      !***SINGLE BRANCH***!
      BranchNum = 1
      CALL DistributePressureOnBranch(LoopNum,LoopSideNum,BranchNum,BranchPressureDrop,FoundAPumpOnBranch)
      LoopSidePressureDrop = LoopSidePressureDrop + BranchPressureDrop
      LoopPressureDrop = LoopPressureDrop + BranchPressureDrop
      !*******************!

    ELSE IF (NumBranches > 1) THEN  !Loop through all branches on this loop side, mixer/splitter configuration

      !***OUTLET BRANCH***!
      BranchNum = NumBranches
      CALL DistributePressureOnBranch(LoopNum,LoopSideNum,BranchNum,BranchPressureDrop,FoundAPumpOnBranch)
      LoopSidePressureDrop = LoopSidePressureDrop + BranchPressureDrop
      LoopPressureDrop = LoopPressureDrop + BranchPressureDrop
      !*******************!

      !***MIXER SIMULATION***!
      MixerPressure = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn)%Press
      CALL PassPressureAcrossMixer(LoopNum,LoopSideNum,MixerPressure,NumBranches)
      !**********************!

      !***PARALLEL BRANCHES***!
      IF (ALLOCATED(ParallelBranchPressureDrops)) DEALLOCATE(ParallelBranchPressureDrops)
      ALLOCATE(ParallelBranchPressureDrops(NumBranches-2))
      IF (ALLOCATED(ParallelBranchInletPressures)) DEALLOCATE(ParallelBranchInletPressures)
      ALLOCATE(ParallelBranchInletPressures(NumBranches-2))
      ParallelBranchCounter = 0
      DO BranchNum = NumBranches-1, 2, -1 !Working backward (not necessary, but consistent)
        ParallelBranchCounter = ParallelBranchCounter + 1
        CALL DistributePressureOnBranch(LoopNum,LoopSideNum,BranchNum, &
                                        ParallelBranchPressureDrops(ParallelBranchCounter),FoundAPumpOnBranch)
        !Store the branch inlet pressure so we can pass it properly across the splitter
        ParallelBranchInletPressures(ParallelBranchCounter) = &
          Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn)%Press
      END DO

      !Now take max inlet pressure to pass across splitter and max branch pressure for bookkeeping
      SplitterInletPressure = MAXVAL(ParallelBranchInletPressures)
      BranchPressureDrop = MAXVAL(ParallelBranchPressureDrops)
      LoopSidePressureDrop = LoopSidePressureDrop + BranchPressureDrop
      LoopPressureDrop = LoopPressureDrop + BranchPressureDrop
      !**********************!

      !If we found pumps on the parallel branches then we are done,
      ! If we are on the demand side, we have a common pipe situation and should issue a warning
      IF (FoundAPumpOnBranch) THEN
        IF (LoopSideNum == DemandSide) THEN
          CALL ShowSevereError('Pressure system information was found in a demand pump (common pipe) simulation')
          CALL ShowContinueError('Currently the pressure simulation is not set up to handle common pipe simulations')
          CALL ShowContinueError('Either modify simulation to avoid common pipe, or remove pressure curve information')
          CALL ShowFatalError('Pressure configuration mismatch causes program termination')
        END IF
        ! If we are on the supply side, we simply hit the branch pump, so we exit the IF statement as
        !  we don't need to simulate the splitter or inlet branch
        ! For now, not doing anything will leave the IF block
      END IF

      !If we haven't found a pump on the parallel branches then we need to go ahead
      ! and simulate the splitter and inlet branch

      !This may all be superfluous, if we just simulate the splitter and inlet branch we may be fine
      ! even if there were branch pumps found.
      IF (.NOT. FoundAPumpOnBranch) THEN

        !***SPLITTER SIMULATION***!
        CALL PassPressureAcrossSplitter(LoopNum,LoopSideNum,SplitterInletPressure)
        !*************************!

        !***INLET BRANCH***!
        BranchNum = 1
        CALL DistributePressureOnBranch(LoopNum,LoopSideNum,BranchNum,BranchPressureDrop,FoundAPumpOnBranch)
        LoopSidePressureDrop = LoopSidePressureDrop + BranchPressureDrop
        LoopPressureDrop = LoopPressureDrop + BranchPressureDrop
        !******************!

        !***PLANT INTERFACE***!
        IF (LoopSideNum == DemandSide) THEN
          CALL PassPressureAcrossInterface(LoopNum)
        END IF
        !*********************!

      END IF

    END IF

    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%PressureDrop = LoopSidePressureDrop

  END DO !LoopSides on this loop

  PlantLoop(LoopNum)%PressureDrop = LoopPressureDrop

  !Now do effective K value calculations
  EffectiveLoopKValue = 0.0d0

  DO LoopSideNum = DemandSide, SupplySide

    EffectiveLoopSideKValue = 0.0d0

    !Always take the first branch K, it may be the only branch on this half loop
    EffectiveLoopSideKValue = EffectiveLoopSideKValue + PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(1)%PressureEffectiveK

    !If there is only one branch then move to the other loop side
    IF (SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch).EQ.1) CYCLE

    !Add parallel branches if necessary by adding them as SUM(1/(sqrt(K_i)))
    TempVal_SumOfOneByRootK = 0.0d0
    DO BranchNum = 2, SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch)-1

      !Only add this branch if the K value is non-zero
      IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureEffectiveK .GT. 0.0d0) THEN
        TempVal_SumOfOneByRootK = TempVal_SumOfOneByRootK  &
                                  + (1.0d0 / SQRT(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureEffectiveK))
      END IF

    END DO

    !Add parallel branches if they are greater than zero, by taking the sum and performing (1/(SUM^2))
    IF (TempVal_SumOfOneByRootK .GT. 0.0d0)   &
       EffectiveLoopSideKValue = EffectiveLoopSideKValue + (1.0d0/(TempVal_SumOfOneByRootK ** 2))

    !Always take the last branch K, it will be in series
    BranchNum = SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch)
    EffectiveLoopSideKValue = EffectiveLoopSideKValue +   &
                                PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureEffectiveK

    !Assign this loop side's K-value
    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%PressureEffectiveK = EffectiveLoopSideKValue

    !Keep adding the overall loop K-value
    EffectiveLoopKValue = EffectiveLoopKValue + EffectiveLoopSideKValue

  END DO

  !Assign this loop's K-value
  PlantLoop(LoopNum)%PressureEffectiveK = EffectiveLoopKValue

  RETURN

END SUBROUTINE UpdatePressureDrop

!=================================================================================================!

SUBROUTINE DistributePressureOnBranch(LoopNum,LoopSideNum,BranchNum,BranchPressureDrop,PumpFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Apply proper pressure to nodes along branch

          ! METHODOLOGY EMPLOYED:
          ! Move backward through components, passing pressure upstream
          ! Account for branch pressure drop at branch inlet node
          ! Update PlantLoop(:)%LoopSide(:)%Branch(:)%PressureDrop Variable

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,    ONLY :  PlantLoop, GenEquipTypes_Pump, DemandSide, SupplySide
  USE DataLoopNode, ONLY :  Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)      ::  LoopNum
  INTEGER,   INTENT(IN)      ::  LoopSideNum
  INTEGER,   INTENT(IN)      ::  BranchNum
  REAL(r64), INTENT(INOUT)   ::  BranchPressureDrop
  LOGICAL                    ::  PumpFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  ::  CompNum
  INTEGER                  ::  NumCompsOnBranch
  REAL(r64)                ::  TempBranchPressureDrop

  !Initialize
  TempBranchPressureDrop = 0.0d0
  BranchPressureDrop = 0.0d0
  PumpFound = .FALSE.
  NumCompsOnBranch = SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp)

  !Retrieve temporary branch pressure drop
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%HasPressureComponents) THEN
    TempBranchPressureDrop = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PressureDrop
  END IF

  !If the last component on the branch is the pump, then check if a pressure drop is detected and set the flag and leave
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(NumCompsOnBranch)%GeneralEquipType==GenEquipTypes_Pump)THEN
    PumpFound = .TRUE.
    IF (TempBranchPressureDrop .NE. 0.0d0) THEN
      CALL ShowSevereError('Error in plant pressure simulation for plant loop: '//PlantLoop(LoopNum)%Name)
      IF (LoopNum == DemandSide) THEN
        CALL ShowContinueError('Occurs for demand side, branch: '//PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Name)
      ELSE IF (LoopNum == SupplySide) THEN
        CALL ShowContinueError('Occurs for supply side, branch: '//PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Name)
      END IF
      CALL ShowContinueError('Branch contains only a single pump component, yet also a pressure drop component.')
      CALL ShowContinueError('Either add a second component to this branch after the pump, or move pressure drop data.')
      CALL ShowFatalError('Preceding pressure drop error causes program termination')
    END IF
    RETURN
  END IF

  !Assign official branch pressure drop
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%HasPressureComponents) THEN
    BranchPressureDrop = TempBranchPressureDrop
  END IF

  !Otherwise update the inlet node of the last component on the branch with this corrected pressure
  !This essentially sets all the pressure drop on the branch to be accounted for on the last component
  Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(NumCompsOnBranch)%NodeNumIn)%Press &
   = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(NumCompsOnBranch)%NodeNumOut)%Press + BranchPressureDrop

  !Then Smear any internal nodes with this new node pressure by working backward through
  ! all but the last component, and passing node pressure upstream
  IF (NumCompsOnBranch > 1) THEN
    DO CompNum = NumCompsOnBranch-1, 1, -1

      !If this component is a pump, stop passing pressure upstream, and set flag to true for calling routine
      IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType==GenEquipTypes_Pump) THEN
        PumpFound = .TRUE.
        EXIT
      END IF

      !Otherwise just pass pressure upstream and move on
      Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%Press = &
      Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut)%Press

    END DO
  END IF


END SUBROUTINE DistributePressureOnBranch

!=================================================================================================!

SUBROUTINE PassPressureAcrossMixer(LoopNum,LoopSideNum,MixerPressure,NumBranchesOnLoopSide)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set mixer inlet pressures, or in other words, set mixer inlet branch outlet pressures

          ! METHODOLOGY EMPLOYED:
          ! Set outlet node pressures for all parallel branches on this loopside
          ! Note that this is extremely simple, but is set to it's own routine to allow for clarity
          !  when possible expansion occurs during further development

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,    ONLY :  PlantLoop
  USE DataLoopNode, ONLY :  Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)      ::  LoopNum
  INTEGER,   INTENT(IN)      ::  LoopSideNum
  REAL(r64), INTENT(INOUT)   ::  MixerPressure
  INTEGER,   INTENT(IN)      ::  NumBranchesOnLoopSide

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                    ::  BranchNum

  DO BranchNum = 2, NumBranchesOnLoopSide-1
    Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumOut)%Press = MixerPressure
  END DO

END SUBROUTINE PassPressureAcrossMixer

!=================================================================================================!

SUBROUTINE PassPressureAcrossSplitter(LoopNum,LoopSideNum,SplitterInletPressure)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the splitter inlet pressure in anticipation of the inlet branch pressure being simulated

          ! METHODOLOGY EMPLOYED:
          ! Set outlet node of loopside inlet branch to splitter pressure
          ! Note that this is extremely simple, but is set to it's own routine to allow for clarity
          !  when possible expansion occurs during further development

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,    ONLY :  PlantLoop
  USE DataLoopNode, ONLY :  Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)      ::  LoopNum
  INTEGER,   INTENT(IN)      ::  LoopSideNum
  REAL(r64), INTENT(INOUT)   ::  SplitterInletPressure

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER         ::  InletBranchNum = 1

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(InletBranchNum)%NodeNumOut)%Press = SplitterInletPressure

END SUBROUTINE PassPressureAcrossSplitter

!=================================================================================================!

SUBROUTINE PassPressureAcrossInterface(LoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Pass pressure backward across plant demand inlet/supply outlet interface

          ! METHODOLOGY EMPLOYED:
          ! Set outlet node pressure of supply side equal to inlet node pressure of demand side
          ! Note that this is extremely simple, but is set to it's own routine to allow for clarity
          !  when possible expansion occurs during further development

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,    ONLY :  PlantLoop, DemandSide, SupplySide
  USE DataLoopNode, ONLY :  Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)      ::  LoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: DemandInletNodeNum
  INTEGER :: SupplyOutletNodeNum

  DemandInletNodeNum = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn
  SupplyOutletNodeNum = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut

  Node(SupplyOutletNodeNum)%Press = Node(DemandInletNodeNum)%Press

END SUBROUTINE PassPressureAcrossInterface

!=================================================================================================!

!SUBROUTINE GetPressureCurveTypeAndIndex(PressureCurveName, PressureCurveType, PressureCurveIndex)
!
!          ! SUBROUTINE INFORMATION:
!          !       AUTHOR         Edwin Lee
!          !       DATE WRITTEN   August 2009
!          !       MODIFIED       na
!          !       RE-ENGINEERED  na
!
!          ! PURPOSE OF THIS SUBROUTINE:
!          ! Given a curve name, returns the curve type and index
!
!          ! METHODOLOGY EMPLOYED:
!          ! Curve types are:
!          !  PressureCurve_Error       = pressure name was given, but curve is not available
!          !  PressureCurve_None        = no pressure curve for this branch
!          !  PressureCurve_Pressure    = pressure curve based on friction/minor loss
!          !  PressureCurve_Generic     = curvemanager held curve which is function of flow rate
!
!          ! REFERENCES:
!          ! na
!
!          ! USE STATEMENTS:
!  USE InputProcessor, ONLY : FindItemInList
!  USE CurveManager,   ONLY : GetCurveIndex, GetCurveType
!  USE GlobalDataConstants,  ONLY : PressureCurve_None, PressureCurve_Pressure, PressureCurve_Generic, PressureCurve_Error
!
!  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
!
!          ! SUBROUTINE ARGUMENT DEFINITIONS:
!  CHARACTER(len=*), INTENT (IN)  :: PressureCurveName            ! name of the curve
!  INTEGER, INTENT(INOUT)         :: PressureCurveType
!  INTEGER, INTENT(INOUT)         :: PressureCurveIndex
!
!          ! SUBROUTINE PARAMETER DEFINITIONS:
!          ! na
!
!          ! INTERFACE BLOCK SPECIFICATIONS
!          ! na
!
!          ! DERIVED TYPE DEFINITIONS
!          ! na
!
!          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER           :: TempCurveIndex
!  LOGICAL           :: FoundCurve
!  CHARACTER(len=32) :: GenericCurveType
!
!  !If input is not gotten, go ahead and get it now
!  IF (GetInputFlag) CALL GetPressureSystemInput
!
!  !Initialize
!  FoundCurve = .FALSE.
!  PressureCurveType = PressureCurve_None
!  PressureCurveIndex = 0
!
!  !Try to retrieve a curve manager object
!  TempCurveIndex = GetCurveIndex(PressureCurveName)
!
!  !See if it is valid
!  IF (TempCurveIndex > 0) THEN
!    !We have to check the type of curve to make sure it is single independent variable type
!    GenericCurveType = GetCurveType(TempCurveIndex)
!    SELECT CASE (GenericCurveType)
!      CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'QUARTIC', 'EXPONENT')
!        PressureCurveType = PressureCurve_Generic
!        PressureCurveIndex = TempCurveIndex
!      CASE DEFAULT
!        CALL ShowSevereError('Plant Pressure Simulation: Found error for curve: '//PressureCurveName)
!        CALL ShowContinueError('Curve type detected: '//GenericCurveType)
!        CALL ShowContinueError('Generic curves should be single independent variable such that DeltaP = f(mdot)')
!        CALL ShowContinueError(' Therefore they should be of type: Linear, Quadratic, Cubic, Quartic, or Exponent')
!        CALL ShowFatalError('Errors in pressure simulation input cause program termination')
!    END SELECT
!    RETURN
!  END IF
!
!  !Then try to retrieve a pressure curve object
!  IF (ALLOCATED(PressureCurve)) THEN
!    IF (SIZE(PressureCurve) > 0) THEN
!      TempCurveIndex = FindItemInList(PressureCurveName,PressureCurve(1:SIZE(PressureCurve))%Name,SIZE(PressureCurve))
!    ELSE
!      TempCurveIndex = 0
!    END IF
!  END IF
!
!  !See if it is valid
!  IF (TempCurveIndex > 0) THEN
!    PressureCurveType = PressureCurve_Pressure
!    PressureCurveIndex = TempCurveIndex
!    RETURN
!  END IF
!
!  !If we made it here, we didn't find either type of match
!
!  !Last check, see if it is blank:
!  IF (TRIM(PressureCurveName)=='') THEN
!    PressureCurveType = PressureCurve_None
!    RETURN
!  END IF
!
!  !At this point, we had a non-blank user entry with no match
!  PressureCurveType = PressureCurve_Error
!  RETURN
!
!RETURN
!
!END SUBROUTINE
!
!
!=================================================================================================!


REAL(r64) FUNCTION ResolveLoopFlowVsPressure(LoopNum, SystemMassFlow, PumpCurveNum, &
                                             PumpSpeed, PumpImpellerDia, MinPhi, MaxPhi)  RESULT (ResolvedLoopMassFlowRate)


          ! FUNCTION INFORMATION:
          !       AUTHOR         Kaustubh Phalak
          !       DATE WRITTEN   Feb 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! To provide a means to simulate a constant speed pump curve and system curve to
          !  find a more realistic operating point for the plant.

          ! METHODOLOGY EMPLOYED:
          ! Pressure drop of complete loop is found for a perticular flow rate.
          !  i.e. pressuredrop = K * massflow ^ 2
          ! System curve is then solved with pump curve already entered
          !  and flow rate provided by the pump will be calculated.
          ! This routine does not trap for errors if a pressure simulation is not to be performed.
          ! Calling routine should only call this if needed.

          ! REFERENCES:
          !  -

          ! USE STATEMENTS:
  USE General,          ONLY: RoundSigDigits
  USE DataPlant,        ONLY: PlantLoop, SupplySide
  USE DataLoopNode,     ONLY: Node
  USE FluidProperties,  ONLY: GetDensityGlycol, GetViscosityGlycol
  USE CurveManager,     ONLY: CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,    INTENT(IN)  ::  LoopNum         !- Index of which plant/condenser loop is being simulated
  REAL(r64),  INTENT(IN)  ::  SystemMassFlow  !- Initial "guess" at system mass flow rate [kg/s]
  INTEGER,    INTENT(IN)  ::  PumpCurveNum    !- Pump curve to use when calling the curve manager for psi = f(phi)
  REAL(r64),  INTENT(IN)  ::  PumpSpeed       !- Pump rotational speed, [rps] (revs per second)
  REAL(r64),  INTENT(IN)  ::  PumpImpellerDia !- Nominal pump impeller diameter [m]
  REAL(r64),  INTENT(IN)  ::  MinPhi          !- Minimum allowable value of phi, requested by the pump manager from curve mgr
  REAL(r64),  INTENT(IN)  ::  MaxPhi          !- Maximum allowable value of phi, requested by the pump manager from curve mgr

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER   ::  RoutineName      = 'ResolvedLoopMassFlowRate: '
  INTEGER,          PARAMETER   ::  MaxIters         = 100
  CHARACTER(LEN=*), PARAMETER   ::  DummyFluidName   = ' '
  REAL(r64),        PARAMETER   ::  PressureConvergeCriteria = 0.1d0   !Pa
  REAL(r64),        PARAMETER   ::  ZeroTolerance    = 0.0001d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                     ::  PumpPressureRise
  Real(r64)                     ::  NodeTemperature
  Real(r64)                     ::  NodeDensity
  REAL(r64)                     ::  SystemPressureDrop
  REAL(r64)                     ::  PhiPump
  REAL(r64)                     ::  PhiSystem
  REAL(r64)                     ::  PsiPump
  INTEGER                       ::  FluidIndex
  INTEGER                       ::  Iteration
  REAL(r64)                     ::  LocalSystemMassFlow
  REAL(r64)                     ::  LoopEffectiveK
  LOGICAL                       ::  Converged
  INTEGER, SAVE                 ::  ZeroKWarningCounter = 0
  INTEGER, SAVE                 ::  MaxIterWarningCounter = 0
  REAL(r64), DIMENSION(3)       ::  MassFlowIterativeHistory
  REAL(r64)                     ::  MdotDeltaLatest
  REAL(r64)                     ::  MdotDeltaPrevious
  REAL(r64)                     ::  DampingFactor

   !Get loop level data
  FluidIndex         = PlantLoop(LoopNum)%FluidIndex
  LoopEffectiveK     = PlantLoop(LoopNum)%PressureEffectiveK
  SystemPressureDrop = LoopEffectiveK * SystemMassFlow **2.0d0

  !Read data off the node data structure
  NodeTemperature = Node(PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn)%Temp
  NodeDensity     = GetDensityGlycol(DummyFluidName, NodeTemperature, FluidIndex, RoutineName)

  !Store the passed in (requested, design) flow to the local value for performing iterations
  LocalSystemMassFlow = SystemMassFlow

  !Check and warn if invalid condition exists
  IF (LoopEffectiveK .LE. ZeroTolerance) THEN
    ZeroKWarningCounter = ZeroKWarningCounter + 1
    IF (ZeroKWarningCounter == 1) THEN
      CALL ShowWarningError('Pump pressure-flow resolution attempted, but invalid loop conditions encountered.')
      CALL ShowContinueError('Loop being calculated: '//PlantLoop(LoopNum)%Name)
      CALL ShowContinueError('An invalid pressure/flow condition existed which resulted in the approximation of')
      CALL ShowContinueError('the pressure coefficient K to be zero.  The pressure simulation will use the requested (design)')
      CALL ShowContinueError('pump flow in order to proceed with the simulation.  This warning is only issued once.')
    END IF
    ResolvedLoopMassFlowRate = SystemMassFlow
    RETURN
  END IF

  !Initialize flag
  Converged = .FALSE.

  !Initialize the mass flow history array and damping factor
  MassFlowIterativeHistory = LocalSystemMassFlow
  DampingFactor = 0.9d0

  !Start Convergence Loop
  DO Iteration = 1, MaxIters

    !Calculate System Mass Flow Rate
    LocalSystemMassFlow = SQRT(SystemPressureDrop / LoopEffectiveK)

    MassFlowIterativeHistory = EOSHIFT(MassFlowIterativeHistory, SHIFT=-1, BOUNDARY=LocalSystemMassFlow)

    PhiSystem = LocalSystemMassFlow / (NodeDensity * PumpSpeed * PumpImpellerDia)

    !4th order polynomial for non-dimensional pump curve
    PhiPump = PhiSystem

    !Constrain the value to the valid region
    PhiPump = MAX(PhiPump,MinPhi)
    PhiPump = MIN(PhiPump,MaxPhi)

    !Get the pump curve value from the curve manager
    PsiPump = CurveValue(PumpCurveNum, PhiPump)

    !Calcuate Pump Pressure rise
    PumpPressureRise = PsiPump * NodeDensity * (PumpSpeed**2) * (PumpImpellerDia**2)

    !Convergence Criteria Based on Pressure
    If (ABS(SystemPressureDrop - PumpPressureRise).LT.(PressureConvergeCriteria)) THEN
      ResolvedLoopMassFlowRate = LocalSystemMassFlow
      Converged = .TRUE.
      EXIT
    END IF

    IF (Iteration < 2) THEN
      !Don't do anything?
    ELSE
      MdotDeltaLatest = ABS(MassFlowIterativeHistory(1) - MassFlowIterativeHistory(2))
      MdotDeltaPrevious = ABS(MassFlowIterativeHistory(2) - MassFlowIterativeHistory(3))
      IF (MdotDeltaLatest < MdotDeltaPrevious) THEN
        !we are converging
        !DampingFactor = MIN(DampingFactor * 1.1, 0.9d0)
      ELSE
        !we are stuck or diverging
        DampingFactor = DampingFactor * 0.9d0
      END IF
    END IF

    !Update pressure value with damping factor
    SystemPressureDrop = DampingFactor * PumpPressureRise + (1.0d0 - DampingFactor) * SystemPressureDrop

  END DO

  !Check if we didn't converge
  IF (.NOT. Converged) THEN
    MaxIterWarningCounter = MaxIterWarningCounter + 1
    IF (MaxIterWarningCounter == 1) THEN
      CALL ShowWarningError('Pump pressure-flow resolution attempted, but iteration loop did not converge.')
      CALL ShowContinueError('Loop being calculated: '//PlantLoop(LoopNum)%Name)
      CALL ShowContinueError('A mismatch between the pump curve entered and the pressure drop components')
      CALL ShowContinueError('on the loop may be the cause.  The pressure simulation will use the requested (design)')
      CALL ShowContinueError('pump flow in order to proceed with the simulation.  This warning is only issued once.')
    END IF
    ResolvedLoopMassFlowRate = SystemMassFlow
    RETURN
  ENDIF

  RETURN

  End FUNCTION ResolveLoopFlowVsPressure

!=================================================================================================!

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

END MODULE PlantPressureSystem

