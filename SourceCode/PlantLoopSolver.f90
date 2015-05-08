MODULE PlantLoopSolver


          ! MODULE INFORMATION:
          !       AUTHOR         B. Griffith,  Dan Fisher, Sankaranarayanan K P, Rich Liesen, Edwin Lee
          !       DATE WRITTEN   Feb 2010
          !         This file developed from PlantSupplySideSolvers.f90 by Sankaranarayanan K P, Rich Liesen, Dan Fisher
          !       MODIFIED       na
          !       RE-ENGINEERED  Aug 2010 Edwin Lee


          ! PURPOSE OF THIS MODULE:
          ! This module contains subroutines to solve plant half loops of various configurations.


          ! METHODOLOGY EMPLOYED:
          ! Main worker calling driver for plant loop system model
          ! Calls various worker routines to model flow rates around a plant half loop
          ! The procedural flow depends on the pump(s), loop side, and operation scheme at the time (and current flow lock?)


          ! USE STATEMENTS:
 USE DataPrecisionGlobals, ONLY: r64
 USE DataInterfaces,       ONLY: ShowFatalError, ShowSevereError, ShowWarningError, &
                                 ShowContinueError, ShowRecurringWarningErrorAtEnd
 USE DataGlobals,          ONLY: MaxNameLength

 IMPLICIT NONE         ! Enforce explicit typing of all variables

 PRIVATE ! Everything private unless explicitly made public

          ! DERIVED TYPE DEFINITIONS
  TYPE Location
    INTEGER :: LoopNum     = 0
    INTEGER :: LoopSideNum = 0
    INTEGER :: BranchNum   = 0
    INTEGER :: CompNum     = 0
  END TYPE Location

  TYPE m_FlowControlValidator
    LOGICAL                      :: Valid      = .TRUE. ! Assume true
    TYPE(Location)               :: ErrorPoint          ! Branch where the error was thrown
    CHARACTER(len=175) :: Reason     = ' '    ! Brief description of error
  END TYPE m_FlowControlValidator

          ! MODULE VARIABLE DEFINITIONS
  REAL(r64) :: InitialDemandToLoopSetPoint
  REAL(r64) :: CurrentAlterationsToDemand
  REAL(r64) :: UpdatedDemandToLoopSetPoint
  REAL(r64) :: LoadToLoopSetPointThatWasntMet ! Unmet Demand
  REAL(r64) :: InitialDemandToLoopSetPointSAVED
  INTEGER   :: RefrigIndex = 0 ! Index denoting refrigerant used (possibly steam)

          ! SUBROUTINE SPECIFICATIONS:
  PUBLIC  PlantHalfLoopSolver
  PRIVATE ValidateFlowControlPaths
  PRIVATE SetupLoopFlowRequest
  PRIVATE SimulateAllLoopSideBranches
  PRIVATE SimulateLoopSideBranchGroup
  PRIVATE SimulateAllLoopSidePumps
  PRIVATE EvaluateLoopSetPointLoad
  !PRIVATE EvaluatePumpFlowConditions
  PRIVATE UpdateAnyLoopDemandAlterations
  PRIVATE UpdateLoopSideReportVars
  PRIVATE ResolveParallelFlows
  PRIVATE DetermineBranchFlowRequest
  PRIVATE PushBranchFlowCharacteristics
  PRIVATE CheckLoopExitNode
  PRIVATE CalcUnmetPlantDemand
  PRIVATE AdjustPumpFlowRequestByEMSControls

 CONTAINS

!==================================================================!
!=================== HYDRONIC HALF-LOOP SOLVER ====================!
!==================================================================!
SUBROUTINE PlantHalfLoopSolver(FirstHVACIteration, LoopSideNum, LoopNum, ReSimOtherSideNeeded)

          ! SUBROUTINE INFORMATION:
          !       AUTHORS:         Dan Fisher, Sankaranarayanan K P, Edwin Lee
          !       DATE WRITTEN:    April 1998
          !       MODIFIED         June 2005(Work in the Plant Super Manager Module)
          !                        July 2006
          !       RE-ENGINEERED    July 2010


          ! PURPOSE OF THIS SUBROUTINE:
          ! SimSupplyFlowSolution is the driver routine for plant loops.  It performs
          !  the following tasks for each half loop (supply or demand side):
          ! 1. Calculate flow request for half loop
          ! 2. Predict Loop Flow
          ! 3. Simulate the inlet branch
          ! 4. Simulate the parallel branches, distributing load if necessary
          ! 5. Set flow rates on parallel branches
          ! 6. Simulate outlet branch and update node and report variables


          ! METHODOLOGY EMPLOYED:
          ! The algorithm operates on a predictor/corrector flow setting method by simulating all available loop components
          ! based on component requested flow rates, then enforcing continuity on all loop branch flows by calling
          ! the flow resolver and locking those flows down.  Available components are then re-simulated using the
          ! corrected flow rates.


          ! USE STATEMENTS:
  USE HVACInterfaceManager,   ONLY: UpdatePlantLoopInterface
  USE PlantCondLoopOperation, ONLY: InitLoadDistribution

  USE PlantPressureSystem,    ONLY: SimPressureDropSystem
  USE DataPlant,              ONLY: DemandSide, SupplySide, TotNumLoops, FlowPumpQuery, &
                                    FlowUnlocked, FlowLocked, PressureCall_Update, PlantLoop, PressureCall_Init
  USE General,                ONLY: RoundSigDigits
  USE DataLoopNode,           ONLY: Node
  USE DataGlobals,            ONLY: BeginTimeStepFlag
  USE PlantUtilities,         ONLY: BoundValueToWithinTwoValues, BoundValueToNodeMinMaxAvail, TightenNodeMinMaxAvails

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: LoopNum
  INTEGER, INTENT(IN)     :: LoopSideNum
  LOGICAL, INTENT(IN)     :: FirstHVACIteration   ! TRUE if First HVAC iteration of Time step
  LOGICAL, INTENT(IN OUT) :: ReSimOtherSideNeeded

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  !~ Topology variables
  INTEGER                                   :: ThisSideInletNode  ! Plant loop side loop inlet
  INTEGER                                   :: ThisSide
  INTEGER                                   :: OtherSide

  !~ Initialization and validation flags
  TYPE(m_FlowControlValidator)              :: IsLoopSideValid

  !~ Flags
  LOGICAL                                   :: LoopShutDownFlag

  !~ Other variables
  REAL(r64)                                 :: ThisLoopSideFlow
  REAL(r64)                                 :: TotalPumpMaxAvailFlow
  REAL(r64)                                 :: TotalPumpMinAvailFlow


  ! Initialize variables
  InitialDemandToLoopSetPoint = 0.0d0
  CurrentAlterationsToDemand  = 0.0d0
  UpdatedDemandToLoopSetPoint = 0.0d0
  ThisSide                    = LoopSideNum
  OtherSide                   = 3 - ThisSide !will give us 1 if thisside is 2, or 2 if thisside is 1
  LoopShutDownFlag            = .FALSE.
  ThisSideInletNode           = PlantLoop(LoopNum)%LoopSide(ThisSide)%NodeNumIn

  ! The following block is related to validating the flow control paths of the loop side
  ! Since the control types are scheduled, I think BeginTimeStep should be a decent check frequency
  IF (BeginTimeStepFlag .AND. PlantLoop(LoopNum)%LoopSide(ThisSide)%OncePerTimeStepOperations) THEN

    ! Initialize loop side controls -- could just be done for one loop since this routine inherently
    !  loops over all plant/condenser loops.  Not sure if the penalty is worth investigating.
    CALL InitLoadDistribution(FirstHVACIteration)

    ! Now that the op scheme types are updated, do loopside validation
    IsLoopSideValid = ValidateFlowControlPaths(LoopNum, ThisSide)
    IF (.NOT. IsLoopSideValid%Valid) THEN
      CALL ShowFatalError('ERROR:'//IsLoopSideValid%Reason)
    END IF

    ! Set the flag to false so we won't do these again this time step
    PlantLoop(LoopNum)%LoopSide(ThisSide)%OncePerTimeStepOperations = .FALSE.

  ELSE

    ! Set the flag to true so that it is activated for the next time step
    PlantLoop(LoopNum)%LoopSide(ThisSide)%OncePerTimeStepOperations = .TRUE.

  END IF

  ! Do pressure system initialize if this is the demand side (therefore once per whole loop)
  IF (ThisSide == DemandSide) CALL SimPressureDropSystem(LoopNum, FirstHVACIteration, PressureCall_Init)

  ! First thing is to setup mass flow request information
  CALL SetupLoopFlowRequest(LoopNum, ThisSide, OtherSide, ThisLoopSideFlow)

  ! Now we know what the loop would "like" to run at, let's see the pump
  ! operation range (min/max avail) to see whether it is possible this time around
  IF (ALLOCATED(PlantLoop(LoopNum)%LoopSide(ThisSide)%Pumps)) THEN

    !~ Initialize pump values
    PlantLoop(LoopNum)%LoopSide(ThisSide)%Pumps%CurrentMinAvail = 0.0d0
    PlantLoop(LoopNum)%LoopSide(ThisSide)%Pumps%CurrentMaxAvail = 0.0d0
    PlantLoop(LoopNum)%LoopSide(ThisSide)%FlowLock = FlowPumpQuery

    !~ Simulate pumps
    CALL SimulateAllLoopSidePumps(LoopNum, ThisSide)

    !~ Calculate totals
    TotalPumpMinAvailFlow = SUM(PlantLoop(LoopNum)%LoopSide(ThisSide)%Pumps%CurrentMinAvail)
    TotalPumpMaxAvailFlow = SUM(PlantLoop(LoopNum)%LoopSide(ThisSide)%Pumps%CurrentMaxAvail)

    ! Use the pump min/max avail to attempt to constrain the loop side flow
    ThisLoopSideFlow = BoundValueToWithinTwoValues(ThisLoopSideFlow, TotalPumpMinAvailFlow, TotalPumpMaxAvailFlow)

  END IF

  ! Now we check flow restriction from the other side, both min and max avail.
  ! Doing this last basically means it wins, so the pump should pull down to meet the flow restriction
  ThisLoopSideFlow = BoundValueToNodeMinMaxAvail(ThisLoopSideFlow, ThisSideInletNode)

  ! Final preparation of loop inlet min/max avail if pumps exist
  IF (ALLOCATED(PlantLoop(LoopNum)%LoopSide(ThisSide)%Pumps)) THEN
    ! At this point, the pump limits should have been obeyed unless a flow restriction was encountered from the other side
    ! The pump may, however, have even tighter constraints than the other side
    ! At this point, the inlet node doesn't know anything about those limits
    ! Since we have already honored the other side flow restriction, try to honor the pump limits here
    CALL TightenNodeMinMaxAvails(ThisSideInletNode, TotalPumpMinAvailFlow, TotalPumpMaxAvailFlow)
  END IF

  ! Now reset the entering mass flow rate to the decided-upon flow rate
  Node(ThisSideInletNode)%MassFlowRate = ThisLoopSideFlow

  ! We also need to establish a baseline "other-side-based" loop demand based on this possible flow rate
  InitialDemandToLoopSetPoint = CalcOtherSideDemand(LoopNum, ThisSide)
  UpdatedDemandToLoopSetPoint = InitialDemandToLoopSetPoint

  LoadToLoopSetPointThatWasntMet = 0.0d0

  ! We now have a loop side flow request, along with inlet min/max avails.
  ! We can now make a first pass through the component simulation, requesting flow as necessary.
  ! Normal "supply side" components will set a mass flow rate on their outlet node to request flow,
  ! while "Demand side" components will set a a mass flow request on their inlet node to request flow.
  PlantLoop(LoopNum)%LoopSide(ThisSide)%FlowLock = FlowUnlocked
  CALL SimulateAllLoopSideBranches(LoopNum, ThisSide, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag)

  ! DSU? discussion/comments about loop solver/flow resolver interaction
  ! At this point, the components have been simulated.  They should have either:
  !  - logged a massflowrequest
  !  - or logged a massflowrate
  ! We need to decide what the components are going to do on FlowLock=0.
  ! If we want all control here at the solver level, the components just need to
  !  log their massflowrate on their outlet nodes, or some other mechanism.
  ! Then the loop solver can scan the branch and get the max, and this will be the requested
  !  flow rate for the branch.
  ! The loop solver will then set this as the branch outlet mass flow rate in preparation
  !  for the flow resolver.
  ! The loop solver may need to do something to the inlet/outlet branch, but I'm not sure yet.
  ! The following comment block is what I had already thought of, and it may still make sense.

  ! Now that all the flow requests have been logged, we need to prepare them for the
  !  flow resolver.  This will just take the requests and determine the desired flow
  !  request for that branch according to pump placement, pump type, and other component
  !  conditions.  In many cases, this will just be to simply take the max request from
  !  the branch, which will already be within pumping limits for that flow path.
  ! We can then call the flow resolver to lock down branch inlet flow rates.
  !DSU?

  ! The flow resolver takes information such as requested flows and min/max available flows and
  !  sets the corrected flow on the inlet to each parallel branch
  CALL ResolveParallelFlows(LoopNum, ThisSide, ThisLoopSideFlow, FirstHVACIteration)
!  CALL PropagateResolvedFlow(LoopNum, ThisSide)

  ! Re-Initialize variables for this next pass
  InitialDemandToLoopSetPointSAVED = InitialDemandToLoopSetPoint
  CurrentAlterationsToDemand  = 0.0d0
  UpdatedDemandToLoopSetPoint = InitialDemandToLoopSetPoint

  ! Now that flow rates have been resolved, we just need to set the flow lock status
  !  flag, and resimulate.  During this simulation each component will still use the
  !  SetFlowRequest routine, but this routine will also set the outlet flow rate
  !  equal to the inlet flow rate, accoridng to flowlock logic.
  PlantLoop(LoopNum)%LoopSide(ThisSide)%FlowLock = FlowLocked
  CALL SimulateAllLoopSideBranches(LoopNum, ThisSide, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag)

  ! A couple things are specific to which loopside we are on
  IF (LoopSideNum==DemandSide) THEN

    ! Pass the loop information via the HVAC interface manager
    CALL UpdatePlantLoopInterface(LoopNum, LoopSideNum,PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumOut, &
                                   PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn,  &
                                   ReSimOtherSideNeeded,PlantLoop(LoopNum)%CommonPipeType)

  ELSE !LoopSide == SupplySide

    ! Update pressure drop reporting, calculate total loop pressure drop for use elsewhere
    CALL SimPressureDropSystem(LoopNum, FirstHVACIteration, PressureCall_Update)

    ! Pass the loop information via the HVAC interface manager (only the flow)
    CALL UpdatePlantLoopInterface(LoopNum, LoopSideNum, PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut, &
                                   PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn, ReSimOtherSideNeeded, &
                                   PlantLoop(LoopNum)%CommonPipeType)

    ! Update the loop outlet node conditions
    CALL CheckLoopExitNode(LoopNum, FirstHVACIteration)

  END IF

  ! Update some reporting information at Plant half loop level
  CALL UpdateLoopSideReportVars(LoopNum, LoopSideNum, InitialDemandToLoopSetPointSAVED, LoadToLoopSetPointThatWasntMet)

 RETURN

END SUBROUTINE PlantHalfLoopSolver
!==================================================================!
!==================================================================!
!==================================================================!


!==================================================================!
!================= TOPOLOGY VALIDATION ROUTINE ====================!
!==================================================================!
TYPE(m_FlowControlValidator) FUNCTION ValidateFlowControlPaths(LoopNum, LoopSideNum) RESULT(ValidLoopSide)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine will scan all the loop side paths and validate the component topology according
          !  to current topology rules and regulations.

          ! METHODOLOGY EMPLOYED:
          ! Scan this loop side and begin by scanning the first branch, then follow with the remainder of the flow paths
          !  - this would be from splitter outlet nodes all the way to the loop side outlet node.
          ! The current rules are that "other types" of components (as defined below in the references) can be placed along each
          !  flow path as needed.  At this point, any number of "load-range based" components can be placed along the flow
          !  path.  After this, the user is allowed to place another set of any number of "other types" of components.
          ! The key restriction is that an "other type" of component may not be sandwiched by "load-range based" components.
          ! This is due to the load range based needing to be simulated all at once along each flow path.

          ! REFERENCES:
          ! "other types" of components: basically not load-range based heat transfer components.  This would include:
          !    • demand based components such as coils
          !    • component setpoint based operating components
          !    • heat exchanger components including waterside economizers
          ! "load-range based" components are heat transfer components which are controlled based on a single load range.
          !    • currently only one load range based scheme is available at a given time, although other control types
          !      may be enabled, such as component setpoint.
          ! Pumps are separate components since the pump heat is not accounted for in the flow path order.
          !  Improvements during the demand side rewrite has allowed pumps to be placed as -not- the first component on a branch
          !  Pumps can be placed anywhere, even between load-range based components, since they will not affect loop load

          ! RETURN VALUE:
          ! Returns a control validator flow structure, including a flag for successful or not, then if not successful
          !  the other values are filled in such as location on the loop where the error occurred and a message error description

          ! USE STATEMENTS:
  USE DataPlant,    ONLY: PlantLoop, LoadRangeBasedMin, LoadRangeBasedMax, PumpOpSchemeType, NoControlOpSchemeType, &
                          UnknownStatusOpSchemeType
  USE DataLoopNode, ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: LoopNum
  INTEGER, INTENT(IN)  :: LoopSideNum

          ! FUNCTION PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: Parallel = 1
  INTEGER, PARAMETER   :: Outlet   = 2

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  !~ Indexing variables
  INTEGER  :: BranchIndex
  INTEGER  :: CompIndex
  INTEGER  :: NumParallelPaths
  INTEGER  :: PathCounter
  INTEGER  :: ParallelOrOutletIndex

  !~ General variables
  LOGICAL  :: EncounteredLRB
  LOGICAL  :: EncounteredNonLRBAfterLRB

  !~ Initialze
  ValidLoopSide%Valid         = .TRUE.
  EncounteredLRB              = .FALSE.
  EncounteredNonLRBAfterLRB   = .FALSE.
  NumParallelPaths            =  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches - 2

  ! We'll start by stepping through the first branch, which may be the only branch
  ! If we find a load range based, then trip the flag and just keep moving
  ! If we only have one branch and all is good, then RETURN early
  ! If we have parallel branches, then start looping through each flow path to
  !  decide if it is a valid path.
  ! If any one path is invalid then all is wrong
  BranchIndex = 1
  DO CompIndex = 1,  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%TotalComponents

    SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%CurOpSchemeType)

      CASE (LoadRangeBasedMin:LoadRangeBasedMax) !~ load range based
        IF (EncounteredNonLRBAfterLRB) THEN
          ! We must have already encountered a LRB, then a non-LRB, and now another LRB, this is bad
          ValidLoopSide%Valid = .FALSE.
          ValidLoopSide%ErrorPoint%LoopNum     = LoopNum
          ValidLoopSide%ErrorPoint%LoopSideNum = LoopSideNum
          ValidLoopSide%ErrorPoint%BranchNum   = BranchIndex
          ValidLoopSide%ErrorPoint%CompNum     = CompIndex
          ValidLoopSide%Reason  = 'Invalid: Load range based components are separated by other control type components. '//&
                                  'Load Range Based should be grouped together on each flow path.'
          RETURN
        ELSE
          EncounteredLRB = .TRUE.
        END IF

      CASE (PumpOpSchemeType)       !~ pump
        ! For now this is just a placeholder, because I think pumps will be available anywhere,
        !  and they won't affect the load distribution

      CASE (NoControlOpSchemeType)   !~ Such as pipes
        ! For now this is just a placeholder, because these components shouldn't cause a problem anywhere...

      CASE (UnknownStatusOpSchemeType)  !~ Uninitialized, this should be a sufficient place to catch for this on branch 1
        !throw fatal
        CALL ShowSevereError('ValidateFlowControlPaths: Uninitialized operation scheme type for component Name: ' &
                    //TRIM(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%Name))
        CALL ShowFatalError('ValidateFlowControlPaths: developer notice, Inlet path validation loop')
!        WRITE(*,*) 'Uninitialized operation scheme type for the following component: '
!        WRITE(*,*) 'Name: '//TRIM(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%Name)
!        WRITE(*,*) 'TYPE: '//TRIM(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%TypeOf)
!        WRITE(*,*) 'Location: Loop/LoopSide/Branch/Comp'
!        100 FORMAT (3(I2,'//'), I2)
!        WRITE(*,100) LoopNum, LoopSideNum, BranchIndex, CompIndex
!        WRITE(*,*) 'Occurs in ValidateFlowControlPaths::Inlet path validation loop'
!        WRITE(*,*) 'Immediate program closure, press ENTER to end.'
!        READ(*,*)
!        STOP 45

      CASE DEFAULT !~ Other control type
        IF (EncounteredLRB) THEN
          EncounteredNonLRBAfterLRB = .TRUE.
        ELSE
          ! For now don't do anything, but we'll see...
        END IF

    END SELECT

  END DO

  ! Return early if we only needed to do the one branch
  IF (NumParallelPaths .LE. 0) RETURN

  ! Now, if we have multiple parallel branches, I think the easiest way is to go all the way from the inlet node
  !  of each parallel branch to the loop outlet node and check the flow path
  ! This way we don't have to remember the conditions on each of the parallel branches when we would finally move
  !  to analyzing the outlet node when all done
  ! This will reduce allocation on the heap because we will keep from storing that array
  ! For each parallel path, we will need to check two branches: the parallel branch and the loopside outlet branch
  DO PathCounter = 1, NumParallelPaths
    DO ParallelOrOutletIndex = Parallel, Outlet
      IF (ParallelOrOutletIndex == Parallel) THEN
        ! The branch index will be the current pathtype + 1 to add the inlet branch
        BranchIndex = PathCounter + 1
      ELSEIF (ParallelOrOutletIndex == Outlet) THEN
        ! The branch index will be the loopside outlet node
        BranchIndex =  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
      END IF

      !Now that we have the branch index, let's do the control type check over all the components
      DO CompIndex = 1,  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%TotalComponents

        SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%CurOpSchemeType)

          CASE (LoadRangeBasedMin:LoadRangeBasedMax) !~ load range based
            IF (EncounteredNonLRBAfterLRB) THEN
              ! We must have already encountered a LRB, then a non-LRB, and now another LRB, this is bad
              ValidLoopSide%Valid = .FALSE.
              ValidLoopSide%ErrorPoint%LoopNum     = LoopNum
              ValidLoopSide%ErrorPoint%LoopSideNum = LoopSideNum
              ValidLoopSide%ErrorPoint%BranchNum   = BranchIndex
              ValidLoopSide%ErrorPoint%CompNum     = CompIndex
              ValidLoopSide%Reason='Invalid: Load range based components are separated by other control type components. '//&
                                          'Load Range Based should be grouped together on each flow path.'
              RETURN
            ELSE
              EncounteredLRB = .TRUE.
            END IF

          CASE (NoControlOpSchemeType)   !~ Such as pipes
            ! For now this is just a placeholder, because these components shouldn't cause a problem anywhere...

          CASE (PumpOpSchemeType)       !~ pump
            ! For now this is just a placeholder, because I think pumps will be available anywhere,
            !  and they won't affect the load distribution

          CASE (UnknownStatusOpSchemeType)  !~ Uninitialized, this should be sufficient place to catch for this on other branches
            !throw fatal error
            Call ShowSevereError('ValidateFlowControlPaths: Uninitialized operation scheme type for component Name: ' &
                    //TRIM(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%Name))
            CALL ShowFatalError('ValidateFlowControlPaths: developer notice, problem in Parallel path validation loop')
!            WRITE(*,*) 'Uninitialized operation scheme type for the following component: '
!            WRITE(*,*) 'Name: '//TRIM(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%Name)
!            WRITE(*,*) 'TYPE: '//TRIM(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%TypeOf)
!            WRITE(*,*) 'Location: Loop/LoopSide/Branch/Comp'
!            WRITE(*,100) LoopNum, LoopSideNum, BranchIndex, CompIndex
!            WRITE(*,*) 'Occurs in ValidateFlowControlPaths::Parallel path validation loop'
!            WRITE(*,*) 'Immediate program closure, press ENTER to end.'
!            READ(*,*)
!            STOP 45

          CASE DEFAULT !~ Other control type
            IF (EncounteredLRB) THEN
              EncounteredNonLRBAfterLRB = .TRUE.
            ELSE
              ! For now don't do anything, but we'll see...
            END IF

        END SELECT

      END DO !~ CompIndex

    END DO !~ Parallel and Outlet Branches

  END DO !~ Parallel Paths

 RETURN

END FUNCTION ValidateFlowControlPaths
!==================================================================!
!==================================================================!
!==================================================================!


!==================================================================!
!==================== PREDICT LOOP FLOW ===========================!
!==================================================================!
SUBROUTINE SetupLoopFlowRequest(LoopNum, ThisSide, OtherSide, LoopFlow)

            ! FUNCTION INFORMATION:
            !       AUTHOR:          Dan Fisher, Edwin Lee
            !       DATE WRITTEN:    August 2010
            !       MODIFIED:        na
            !       RE-ENGINEERED:   na

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine sets up the flow request values and sums them up for each loop side
            ! Then makes a decision on the desired loop flow based on loop configuration

            ! METHODOLOGY EMPLOYED:
            ! Scan through the components on this loop side, and look at the mass flow request
            !  values on components inlet node.
            ! Check common pipe/pumping configuration for this loop side and the other loop side
            !  to determine what the loopside should flow

            ! USE STATEMENTS:
  USE DataPlant,      ONLY: PlantLoop, CommonPipe_No, LoadRangeBasedMin, LoadRangeBasedMax, &
                            LoopFlowStatus_Unknown, LoopFlowStatus_NeedyAndTurnsLoopOn, &
                            LoopFlowStatus_NeedyIfLoopOn, LoopFlowStatus_TakesWhatGets, TotNumLoops, &
                            GenEquipTypes_Pump,TypeOf_PumpConstantSpeed,TypeOf_PumpBankConstantSpeed, &
                            SupplySide, CommonPipe_TwoWay, DemandSide, CommonPipe_Single,TypeOf_PumpVariableSpeed, &
                            TypeOf_PumpBankVariableSpeed,TypeOf_PumpBankConstantSpeed
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE DataLoopNode,   ONLY: Node
  USE Pumps,          ONLY: PumpEquip
  USE PlantUtilities, ONLY: IntegerIsWithinTwoValues
  USE DataHVACGlobals,ONLY: SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)                :: LoopNum
  INTEGER,   INTENT(IN)                :: ThisSide
  INTEGER,   INTENT(IN)                :: OtherSide
  REAL(r64), INTENT(IN OUT)            :: LoopFlow  ! Once all flow requests are evaluated, this is the desired flow on this side

            ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER                   :: ThisSideFlowIndex  = 1
  INTEGER, PARAMETER                   :: OtherSideFlowIndex = 2

            ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
  INTEGER                              :: LoopCounter
  INTEGER                              :: LoopSideCounter
  INTEGER                              :: BranchCounter
  INTEGER                              :: CompCounter
  INTEGER                              :: CompIndex
  INTEGER                              :: NumBranchesOnThisLoopSide
  INTEGER                              :: NumCompsOnThisBranch
  INTEGER                              :: NodeToCheckRequest
  REAL(r64)                            :: ThisBranchFlowRequestNeedAndTurnOn
  REAL(r64)                            :: ThisBranchFlowRequestNeedIfOn
  REAL(r64)                            :: InletBranchRequestNeedAndTurnOn
  REAL(r64)                            :: InletBranchRequestNeedIfOn
  REAL(r64), DIMENSION(:), ALLOCATABLE, SAVE :: ParallelBranchRequestsNeedAndTurnOn
  REAL(r64), DIMENSION(:), ALLOCATABLE, SAVE :: ParallelBranchRequestsNeedIfOn
  REAL(r64), DIMENSION(:, :), ALLOCATABLE, SAVE :: LoadedConstantSpeedBranchFlowRateSteps
  REAL(r64), DIMENSION(:, :), ALLOCATABLE, SAVE :: NoLoadConstantSpeedBranchFlowRateSteps
  INTEGER                              :: ParallelBranchIndex
  REAL(r64)                            :: OutletBranchRequestNeedAndTurnOn
  REAL(r64)                            :: OutletBranchRequestNeedIfOn
  LOGICAL                              :: ThisSideHasPumps
  LOGICAL                              :: OtherSideHasPumps
  LOGICAL                              :: ThisLoopHasCommonPipe
  LOGICAL, DIMENSION(2)                :: ThisLoopHasConstantSpeedBranchPumps
  REAL(r64), DIMENSION(2)              :: EachSideFlowRequestNeedAndTurnOn   ! 2 for SupplySide/DemandSide
  REAL(r64), DIMENSION(2)              :: EachSideFlowRequestNeedIfOn       ! 2 for SupplySide/DemandSide
  REAL(r64), DIMENSION(2)              :: EachSideFlowRequestFinal  ! 2 for SupplySide/DemandSide

  LOGICAL, SAVE                        :: AllocatedParallelArray = .FALSE.
  INTEGER                              :: MaxParallelBranchCount
  INTEGER                              :: FlowPriorityStatus
  REAL(r64)                            :: tmpLoopFlow
  REAL(r64)                            :: AccumFlowSteps
  REAL(r64)                            :: MaxBranchPumpLoopSideFlow

  !~ One time init for array allocated
  IF (.NOT. AllocatedParallelArray) THEN
    MaxParallelBranchCount = 0
    DO LoopCounter = 1, TotNumLoops
      DO LoopSideCounter = 1, 2
        MaxParallelBranchCount = MAX(MaxParallelBranchCount,  PlantLoop(LoopCounter)%LoopSide(LoopSideCounter)%TotalBranches-2)
      END DO
    END DO
    ALLOCATE(ParallelBranchRequestsNeedAndTurnOn(MaxParallelBranchCount))
    ALLOCATE(ParallelBranchRequestsNeedIfOn(MaxParallelBranchCount))
    ALLOCATE(LoadedConstantSpeedBranchFlowRateSteps(2, MaxParallelBranchCount))
    ALLOCATE(NoLoadConstantSpeedBranchFlowRateSteps(2, MaxParallelBranchCount))
    AllocatedParallelArray = .TRUE.
  END IF

  !~ Initialize
  LoopFlow = 0.0d0
  ThisLoopHasConstantSpeedBranchPumps = .FALSE.
  EachSideFlowRequestNeedAndTurnOn  = 0.0d0
  EachSideFlowRequestNeedIfOn       = 0.0d0
  EachSideFlowRequestFinal          = 0.0d0
!  AtLeastOneNonLRBRequested       = .FALSE.

  !~ First we need to set up the flow requests on each loopside
  DO LoopSideCounter = DemandSide, SupplySide
    ! Clear things out for this loopside
    InletBranchRequestNeedAndTurnOn = 0.0d0
    InletBranchRequestNeedIfOn      = 0.0d0
    IF(AllocatedParallelArray) THEN
      ParallelBranchRequestsNeedAndTurnOn = 0.0d0
      ParallelBranchRequestsNeedIfOn      = 0.0d0
    ENDIF
    OutletBranchRequestNeedAndTurnOn = 0.0d0
    OutletBranchRequestNeedIfOn      = 0.0d0
    EachSideFlowRequestNeedAndTurnOn(LoopSideCounter) = 0.0d0
    EachSideFlowRequestNeedIfOn(LoopSideCounter)      = 0.0d0
    ! Now loop through all the branches on this loopside and get flow requests
    NumBranchesOnThisLoopSide =  PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%TotalBranches
    ParallelBranchIndex = 0
    DO BranchCounter = 1, NumBranchesOnThisLoopSide
      ThisBranchFlowRequestNeedAndTurnOn = 0.0d0
      ThisBranchFlowRequestNeedIfOn      = 0.0d0
      IF(BranchCounter > 1 .AND. BranchCounter < NumBranchesOnThisLoopSide) ParallelBranchIndex = ParallelBranchIndex + 1
      NumCompsOnThisBranch =  PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%TotalComponents
      DO CompCounter = 1, NumCompsOnThisBranch
        NodeToCheckRequest = PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp(CompCounter)%NodeNumIn
        FlowPriorityStatus = PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp(CompCounter)%FlowPriority

        IF (PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp(CompCounter)%GeneralEquipType &
               /= GenEquipTypes_Pump) THEN

          SELECT CASE (FlowPriorityStatus)

          CASE (LoopFlowStatus_Unknown)
             ! do nothing
          CASE (LoopFlowStatus_NeedyAndTurnsLoopOn)
            ThisBranchFlowRequestNeedAndTurnOn = MAX(ThisBranchFlowRequestNeedAndTurnOn,&
                                                     Node(NodeToCheckRequest)%MassFlowRateRequest)
            ThisBranchFlowRequestNeedIfOn      = MAX(ThisBranchFlowRequestNeedIfOn,     &
                                                     Node(NodeToCheckRequest)%MassFlowRateRequest)
          CASE (LoopFlowStatus_NeedyIfLoopOn)
            ThisBranchFlowRequestNeedIfOn      = MAX(ThisBranchFlowRequestNeedIfOn,     &
                                                     Node(NodeToCheckRequest)%MassFlowRateRequest)
          CASE (LoopFlowStatus_TakesWhatGets)
             ! do nothing
          END SELECT
       ELSE  ! handle pumps differently
         IF ((BranchCounter == 1) .AND. (LoopSideCounter == SupplySide) &
              .AND. (PlantLoop(LoopNum)%CommonPipeType == CommonPipe_TwoWay)) THEN
           ! special primary side flow request for two way common pipe
           CompIndex = PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp(CompCounter)%CompNum
           SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp(CompCounter)%TypeOf_Num)
            ! remove var speed pumps from this case statement if can set MassFlowRateRequest
           CASE (TypeOf_PumpConstantSpeed,TypeOf_PumpVariableSpeed,TypeOf_PumpBankVariableSpeed)

             IF (CompIndex > 0) THEN
               !
               ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  PumpEquip(CompIndex)%MassFlowRateMax)
             ENDIF
           CASE (TypeOf_PumpBankConstantSpeed )
             IF (CompIndex > 0) THEN
               !
               ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  &
                                                PumpEquip(CompIndex)%MassFlowRateMax/ PumpEquip(CompIndex)%NumPumpsInBank)
             ENDIF
           CASE DEFAULT
             ThisBranchFlowRequestNeedIfOn    = MAX(ThisBranchFlowRequestNeedIfOn,     &
                                                     Node(NodeToCheckRequest)%MassFlowRateRequest)
           END SELECT

         ELSEIF ((BranchCounter == 1) .AND. (LoopSideCounter == SupplySide) &
              .AND. (PlantLoop(LoopNum)%CommonPipeType == CommonPipe_Single)) THEN
           CompIndex = PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp(CompCounter)%CompNum
           SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp(CompCounter)%TypeOf_Num)
            ! remove var speed pumps from this case statement if can set MassFlowRateRequest
           CASE (TypeOf_PumpConstantSpeed,TypeOf_PumpVariableSpeed,TypeOf_PumpBankVariableSpeed)
             IF (CompIndex > 0) THEN
               !
               ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  PumpEquip(CompIndex)%MassFlowRateMax)
             ENDIF
           CASE (TypeOf_PumpBankConstantSpeed )
             IF (CompIndex > 0) THEN
               !
               ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  &
                                                        PumpEquip(CompIndex)%MassFlowRateMax/ PumpEquip(CompIndex)%NumPumpsInBank)
             ENDIF
           CASE DEFAULT
             ThisBranchFlowRequestNeedIfOn    = MAX(ThisBranchFlowRequestNeedIfOn,     &
                                                     Node(NodeToCheckRequest)%MassFlowRateRequest)
           END SELECT
         ELSE
           CompIndex = PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp(CompCounter)%CompNum
           SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp(CompCounter)%TypeOf_Num)

           CASE (TypeOf_PumpConstantSpeed)
             IF (CompIndex > 0) THEN
               IF (ParallelBranchIndex >= 1) THEN ! branch pump
                 IF (ANY(ABS(PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp%MyLoad) > SmallLoad )) THEN
                   ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  PumpEquip(CompIndex)%MassFlowRateMax)
                 ELSEIF (PlantLoop(LoopNum)%CommonPipeType  /= CommonPipe_No) THEN ! common pipe and constant branch pumps
                   ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  PumpEquip(CompIndex)%MassFlowRateMax)
                 ENDIF
                 ThisLoopHasConstantSpeedBranchPumps(LoopSideCounter) = .TRUE.
                 PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%HasConstantSpeedBranchPump = .TRUE.
                 PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%ConstantSpeedBranchMassFlow &
                            =  PumpEquip(CompIndex)%MassFlowRateMax
               ELSE ! inlet pump
                 ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  PumpEquip(CompIndex)%MassFlowRateMax)
               ENDIF
             ENDIF
           CASE (TypeOf_PumpBankConstantSpeed )
             IF (CompIndex > 0) THEN
               IF (ParallelBranchIndex >= 1) THEN ! branch pump
                 IF (ANY(ABS(PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp%MyLoad) > SmallLoad )) THEN
                   ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  &
                                         PumpEquip(CompIndex)%MassFlowRateMax/ PumpEquip(CompIndex)%NumPumpsInBank)
                 ELSEIF (PlantLoop(LoopNum)%CommonPipeType  /= CommonPipe_No) THEN ! common pipe and constant branch pumps
                   ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  &
                                         PumpEquip(CompIndex)%MassFlowRateMax/ PumpEquip(CompIndex)%NumPumpsInBank)
                 ENDIF
                 ThisLoopHasConstantSpeedBranchPumps(LoopSideCounter) = .TRUE.
                 PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%HasConstantSpeedBranchPump = .TRUE.
                 PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%ConstantSpeedBranchMassFlow &
                            =  PumpEquip(CompIndex)%MassFlowRateMax/ PumpEquip(CompIndex)%NumPumpsInBank
               ELSE ! inlet pump
                 ThisBranchFlowRequestNeedIfOn  = MAX(ThisBranchFlowRequestNeedIfOn,  &
                                       PumpEquip(CompIndex)%MassFlowRateMax/ PumpEquip(CompIndex)%NumPumpsInBank)
               ENDIF
             ENDIF
           END SELECT
         ENDIF
       ENDIF

      END DO
      IF (BranchCounter == 1) THEN ! inlet branch
        InletBranchRequestNeedAndTurnOn = ThisBranchFlowRequestNeedAndTurnOn
        InletBranchRequestNeedIfOn      = ThisBranchFlowRequestNeedIfOn
      ELSE IF (BranchCounter < NumBranchesOnThisLoopSide) THEN ! branchcounter = 1 is already caught
        ParallelBranchRequestsNeedAndTurnOn(ParallelBranchIndex) = ThisBranchFlowRequestNeedAndTurnOn
        ParallelBranchRequestsNeedIfOn(ParallelBranchIndex)      = ThisBranchFlowRequestNeedIfOn
      ELSE IF (BranchCounter == NumBranchesOnThisLoopSide) THEN ! outlet branch
        OutletBranchRequestNeedAndTurnOn = ThisBranchFlowRequestNeedAndTurnOn
        OutletBranchRequestNeedIfOn      = ThisBranchFlowRequestNeedIfOn
      END IF

      PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%RequestedMassFlow = &
                              MAX(ThisBranchFlowRequestNeedIfOn, ThisBranchFlowRequestNeedAndTurnOn)

    END DO
    EachSideFlowRequestNeedAndTurnOn(LoopSideCounter) &
         = MAX(InletBranchRequestNeedAndTurnOn, SUM(ParallelBranchRequestsNeedAndTurnOn), OutletBranchRequestNeedAndTurnOn)
    EachSideFlowRequestNeedIfOn(LoopSideCounter) &
         = MAX(InletBranchRequestNeedIfOn, SUM(ParallelBranchRequestsNeedIfOn), OutletBranchRequestNeedIfOn)
  END DO




  !~ Now that we have calculated each sides different status's requests, process to find final
  IF (Sum(EachSideFlowRequestNeedAndTurnOn) < MassFlowTolerance ) THEN
    EachSideFlowRequestFinal = 0.0d0
  ELSE ! some flow is needed and loop should try to run
    EachSideFlowRequestFinal(ThisSide) &
         = MAX(EachSideFlowRequestNeedAndTurnOn(ThisSide),  EachSideFlowRequestNeedIfOn(ThisSide) )
    EachSideFlowRequestFinal(OtherSide) &
         = MAX(EachSideFlowRequestNeedAndTurnOn(OtherSide), EachSideFlowRequestNeedIfOn(OtherSide) )

  ENDIF
  ! now store final flow requests on each loop side data structure
  PlantLoop(LoopNum)%LoopSide(ThisSide)%FlowRequest  = EachSideFlowRequestFinal(ThisSide)
  PlantLoop(LoopNum)%LoopSide(OtherSide)%FlowRequest = EachSideFlowRequestFinal(OtherSide)

  IF (PlantLoop(LoopNum)%CommonPipeType == CommonPipe_No) THEN
   !we may or may not have a pump on this side, but the flow request is the larger of the two side's final
    IF (.NOT. ANY(ThisLoopHasConstantSpeedBranchPumps)) THEN
      LoopFlow = MAXVAL(EachSideFlowRequestFinal)
    ELSE ! account for stepped loop flow rates required of branch pumps

      ! rules for setting flow when there are constant speed branch pumps.
      ! 1. Check if above routines already selected a loop flow rate based on the constant speed branches, if so then just use it
      IF ((ThisLoopHasConstantSpeedBranchPumps(ThisSide)) .AND. &
          (EachSideFlowRequestFinal(ThisSide) >= EachSideFlowRequestFinal(OtherSide))) THEN
        ! okay, just use basic logic
        LoopFlow = MAXVAL(EachSideFlowRequestFinal)
      ELSEIF ((ThisLoopHasConstantSpeedBranchPumps(OtherSide)) .AND. &
          (EachSideFlowRequestFinal(ThisSide) <= EachSideFlowRequestFinal(OtherSide))) THEN
        ! okay, just use basic logic
        LoopFlow = MAXVAL(EachSideFlowRequestFinal)
      ELSE ! not okay, we have a case that will likely need special correcting
        !  2. determine which loop side has the stepped data
        IF ((ThisLoopHasConstantSpeedBranchPumps(ThisSide)) .AND. &
            (EachSideFlowRequestFinal(ThisSide) < EachSideFlowRequestFinal(OtherSide))) THEN
          LoopSideCounter = ThisSide
        ELSEIF ((ThisLoopHasConstantSpeedBranchPumps(OtherSide)) .AND. &
           (EachSideFlowRequestFinal(OtherSide) < EachSideFlowRequestFinal(ThisSide))) THEN
          LoopSideCounter = OtherSide
        ENDIF

        ! 3. step through and find out needed information
        ! 3a.  search the loop side with branch pumps and find the steps available with non-zero Myloads
        ! 3b.  search the loop side with branch pumps and find the steps available with zero Myloads
        LoadedConstantSpeedBranchFlowRateSteps = 0.d0
        NoLoadConstantSpeedBranchFlowRateSteps = 0.d0
        ParallelBranchIndex = 0
        NumBranchesOnThisLoopSide =  PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%TotalBranches
        DO BranchCounter = 1, NumBranchesOnThisLoopSide
          IF(BranchCounter > 1 .AND. BranchCounter < NumBranchesOnThisLoopSide) ParallelBranchIndex = ParallelBranchIndex + 1
          IF (PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%HasConstantSpeedBranchPump) THEN
            IF  (ANY(ABS(PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%Comp%MyLoad) > SmallLoad )) THEN
              LoadedConstantSpeedBranchFlowRateSteps(LoopSideCounter, ParallelBranchIndex) = &
                PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%ConstantSpeedBranchMassFlow
            ELSE
              NoLoadConstantSpeedBranchFlowRateSteps(LoopSideCounter, ParallelBranchIndex) = &
                PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%ConstantSpeedBranchMassFlow
            ENDIF
          ENDIF
        ENDDO

        ! 4. allocate which branches to use,
        tmpLoopFlow = MAXVAL(EachSideFlowRequestFinal)
        AccumFlowSteps = 0.d0
        MaxBranchPumpLoopSideFlow = SUM(LoadedConstantSpeedBranchFlowRateSteps) + SUM(NoLoadConstantSpeedBranchFlowRateSteps)
        tmpLoopFlow = MIN(tmpLoopFlow, MaxBranchPumpLoopSideFlow)
        !  4b. first use all the branches with non-zero MyLoad
        IF (tmpLoopFlow <= Sum(LoadedConstantSpeedBranchFlowRateSteps)) THEN
          tmpLoopFlow = Sum(LoadedConstantSpeedBranchFlowRateSteps)
        ELSE
          AccumFlowSteps = Sum(LoadedConstantSpeedBranchFlowRateSteps)
          ParallelBranchIndex = 0
          DO BranchCounter = 1, NumBranchesOnThisLoopSide
            IF(BranchCounter > 1 .AND. BranchCounter < NumBranchesOnThisLoopSide) THEN
              ParallelBranchIndex = ParallelBranchIndex + 1
            ELSE
              CYCLE
            ENDIF
            IF (NoLoadConstantSpeedBranchFlowRateSteps(LoopSideCounter, ParallelBranchIndex) > 0.d0) THEN
                  !  add in branches with zero MyLoad  in branch input order until satisfied
              IF ((tmpLoopFlow > AccumFlowSteps) .AND. (tmpLoopFlow <= (AccumFlowSteps + &
                                                NoLoadConstantSpeedBranchFlowRateSteps(LoopSideCounter, ParallelBranchIndex)))) THEN
                !found it set requests and exit
                tmpLoopFlow = AccumFlowSteps +  NoLoadConstantSpeedBranchFlowRateSteps(LoopSideCounter, ParallelBranchIndex)
                PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%RequestedMassFlow &
                             = NoLoadConstantSpeedBranchFlowRateSteps(LoopSideCounter, ParallelBranchIndex)
                LoopFlow = tmpLoopFlow
                EXIT
              ELSEIF ((tmpLoopFlow > AccumFlowSteps) .AND. (tmpLoopFlow > (AccumFlowSteps + &
                                                NoLoadConstantSpeedBranchFlowRateSteps(LoopSideCounter, ParallelBranchIndex)))) THEN
                AccumFlowSteps = AccumFlowSteps +  NoLoadConstantSpeedBranchFlowRateSteps(LoopSideCounter, ParallelBranchIndex)
                PlantLoop(LoopNum)%LoopSide(LoopSideCounter)%Branch(BranchCounter)%RequestedMassFlow &
                             = NoLoadConstantSpeedBranchFlowRateSteps(LoopSideCounter, ParallelBranchIndex)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDIF
    ThisLoopHasCommonPipe    = .FALSE.
  ELSEIF (PlantLoop(LoopNum)%CommonPipeType == CommonPipe_TwoWay) THEN
    LoopFlow=EachSideFlowRequestFinal(ThisSide)
    ThisLoopHasCommonPipe    = .TRUE.
  ELSEIF (PlantLoop(LoopNum)%CommonPipeType == CommonPipe_Single) THEN
    LoopFlow=EachSideFlowRequestFinal(ThisSide)
    ThisLoopHasCommonPipe    = .TRUE.
  ENDIF


  ! do some diagnostic that are easy and fast at this point, the rest of this routine could be moved
  !?  should be caught previously in input~ Check erroneous conditions first before we do the logic below
   !~ Check loop configuration, as this will dictate the flow that we request for our loop side
  IF ( PlantLoop(LoopNum)%LoopSide(ThisSide)%TotalPumps  > 0) THEN
    ThisSideHasPumps         = .TRUE.
  ELSE
    ThisSideHasPumps         = .FALSE.
  ENDIF
  IF (PlantLoop(LoopNum)%LoopSide(OtherSide)%TotalPumps > 0) THEN
    OtherSideHasPumps        = .TRUE.
  ELSE
    OtherSideHasPumps        = .FALSE.
  ENDIF
  IF (ThisLoopHasCommonPipe .AND. .NOT. ThisSideHasPumps) THEN
    CALL ShowSevereError('SetupLoopFlowRequest: Common Pipe must have pumps on both sides of loop')
    CALL ShowContinueError('Occurs on plant loop name ="'//trim(PlantLoop(LoopNum)%Name)//'"')
    IF (ThisSide == DemandSide) THEN
      CALL ShowContinueError('Add a pump to the demand side of the plant loop')
    ELSEIF (ThisSide == SupplySide) THEN
      CALL ShowContinueError('Add a pump to the supply side of the plant loop')
    ENDIF
    CALL ShowFatalError('Program terminates due to preceding conditions.')


  ELSE IF (.NOT. ThisSideHasPumps .AND. .NOT. OtherSideHasPumps) THEN
    CALL ShowSevereError('SetupLoopFlowRequest: Problem in plant topology, no pumps specified on the loop')
    CALL ShowContinueError('Occurs on plant loop name ="'//trim(PlantLoop(LoopNum)%Name)//'"')
    CALL ShowContinueError('All plant loops require at least one pump')
    CALL ShowFatalError('Program terminates due to preceding conditions.')
  END IF


 RETURN

END SUBROUTINE SetupLoopFlowRequest
!==================================================================!
!==================================================================!
!==================================================================!


!==================================================================!
!================== LOOPSIDE BRANCH SIMULATION ====================!
!==================================================================!
SUBROUTINE SimulateAllLoopSideBranches(LoopNum, LoopSideNum, ThisLoopSideFlow, FirstHVACIteration, LoopShutDownFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine will step through all branch groups (single branch .OR. inlet/parallels/outlet)
          !  and call the branch group simulation routine.  This routine also calls to update the splitter
          !  and mixer.

          ! METHODOLOGY EMPLOYED:
          ! The number of branch groups is either 1 or 3.  1 would be a single branch half-loop.  3 would
          !  be the minimum for an inlet/parallels/outlet set.  The number of branch groups can then be
          !  calculated as #BrGrps = 1 + 2*L; where L is zero for single half loop and one for parallel-type set.
          !  This calculation can be reduced to the logical/integer conversion as shown in the code.
          ! The simulation then steps through each branch group.  If there are parallel branches, the splitter is
          !  updated on flowlock=0 to pass information through, then after the parallel branches the mixer is always
          !  updated.  The outlet branch "group" is then simulated.

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: UpdatePlantSplitter, UpdatePlantMixer
  USE DataPlant,      ONLY: PlantLoop, FlowUnlocked

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: LoopNum
  INTEGER,   INTENT(IN) :: LoopSideNum
  REAL(r64), INTENT(IN) :: ThisLoopSideFlow
  LOGICAL,   INTENT(IN) :: FirstHVACIteration
  LOGICAL,   INTENT(INOUT) :: LoopShutDownFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: InletBranchOrOneBranchHalfLoop = 1
  INTEGER, PARAMETER :: ParallelBranchSet              = 2
  INTEGER, PARAMETER :: OutletBranch                   = 3
  LOGICAL, PARAMETER :: StartingNewLoopSidePass        = .TRUE.

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumBranchGroups
  INTEGER :: BranchesGreaterThanOne
  INTEGER :: BranchGroup


  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches > 1) THEN
    BranchesGreaterThanOne = 1
  ELSE
    BranchesGreaterThanOne = 0
  ENDIF
  NumBranchGroups = 1 + 2 * BranchesGreaterThanOne

  DO BranchGroup = 1, NumBranchGroups

    IF ((BranchGroup > 1) .AND.  (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches == 1)) EXIT

    SELECT CASE(BranchGroup)

      CASE (InletBranchOrOneBranchHalfLoop)  ! This group would be the inlet branch, or the single half-loop branch
        CALL SimulateLoopSideBranchGroup(                        &
                                         LoopNum,                &
                                         LoopSideNum,            &
                                         1,                      &
                                         1,                      &
                                         ThisLoopSideFlow,       &
                                         FirstHVACIteration,     &
                                         LoopShutDownFlag,       &
                                         StartingNewLoopSidePass &
                                        )

      CASE (ParallelBranchSet)  ! This group is the parallel set of branches, or the single branch between the mix/split

        CALL UpdatePlantSplitter(LoopNum,LoopSideNum,1)

        CALL SimulateLoopSideBranchGroup(                      &
                                         LoopNum,              &
                                         LoopSideNum,          &
                                         2,                    &
                                         PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches - 1, &
                                         ThisLoopSideFlow,     &
                                         FirstHVACIteration,   &
                                         LoopShutDownFlag      &
                                        )
        CALL UpdatePlantMixer(LoopNum,LoopSideNum,1)

      CASE (OutletBranch)  ! This group is the outlet branch
        CALL SimulateLoopSideBranchGroup(                      &
                                         LoopNum,              &
                                         LoopSideNum,          &
                                         PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches, &
                                         PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches, &
                                         ThisLoopSideFlow,     &
                                         FirstHVACIteration,   &
                                         LoopShutDownFlag      &
                                        )

    END SELECT

  END DO

 RETURN

END SUBROUTINE SimulateAllLoopSideBranches
!==================================================================!
!==================================================================!
!==================================================================!


!==================================================================!
!================ SINGLE BRANCH GROUP SIMULATION ==================!
!==================================================================!
SUBROUTINE SimulateLoopSideBranchGroup(LoopNum, LoopSideNum, FirstBranchNum, LastBranchNum, FlowRequest, &
                                       FirstHVACIteration, LoopShutDownFlag, StartingNewLoopSidePass)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine will manage the component simulation on a single set of parallel branches
          ! This routine also reverts to a single branch simulation if there is "only one parallel" branch

          ! METHODOLOGY EMPLOYED:
          ! Loop through all components, and simulate first the non-load range based on each branch.
          ! When a load-range based (LRB) is encountered, the simulation moves to the next branch to do non-LRB components.
          ! When all paths are exhausted the simulation begins simulating LRB components.  Before each comp, the load distribution
          !  engine is called to handle the load distribution for this current pass.  If load is successfully distributed, this is
          !  flagged, and not called again.  If load is not distributed (i.e. this component isn't ON right now), then the
          !  load distribution engine will be called again before the next component.
          ! After all load distribution is done and those components are complete, the simulation moves back to do any
          !  remaining components that may be downstream.

          ! USE STATEMENTS:
 USE DataPlant,              ONLY: PlantLoop, DemandOpSchemeType, PumpOpSchemeType, LoadRangeBasedMin, LoadRangeBasedMax, &
                                   FlowLocked, NoControlOpSchemeType, CompSetPtBasedSchemeType, FreeRejectionOpSchemeType, &
                                   WSEconOpSchemeType, UnknownStatusOpSchemeType, PressureCall_Calc, EMSOpSchemeType, SupplySide
 USE DataLoopNode,           ONLY: Node
 USE PlantCondLoopOperation, ONLY: ManagePlantLoadDistribution
 USE PlantLoopEquip,         ONLY: SimPlantEquip
 USE PlantPressureSystem,    ONLY: SimPressureDropSystem
 USE General,                ONLY: TrimSigDigits

 IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)  :: LoopNum
  INTEGER,   INTENT(IN)  :: LoopSideNum
  INTEGER,   INTENT(IN)  :: FirstBranchNum
  INTEGER,   INTENT(IN)  :: LastBranchNum
  REAL(r64), INTENT(IN)  :: FlowRequest
  LOGICAL,   INTENT(IN)  :: FirstHVACIteration
  LOGICAL,   INTENT(INOUT)  :: LoopShutDownFlag
  LOGICAL,   INTENT(IN), OPTIONAL :: StartingNewLoopSidePass

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  !~ History values
  INTEGER, SAVE        :: LastLoopNum        = -1
  INTEGER, SAVE        :: LastLoopSideNum    = -1
  INTEGER, SAVE        :: LastFirstBranchNum = -1
  INTEGER, SAVE        :: LastLastBranchNum  = -1

  !~ Indexing variables
  INTEGER              :: BranchCounter      !~ This contains the index for the %Branch(:) structure
  INTEGER              :: BranchIndex        !~ This is a 1 - n value within the current branch group
  INTEGER              :: CompCounter        !~ This contains the index for the %Comp(:) structure
  INTEGER              :: StartingComponent
  INTEGER              :: EndingComponent
  INTEGER              :: NumBranchesInRegion

  !~ Flags
  LOGICAL, SAVE        :: EncounteredLRBObjDuringPass1
  LOGICAL, SAVE        :: EncounteredNonLBObjDuringPass2
  LOGICAL, SAVE        :: EncounteredAnyLRBObjects

  LOGICAL              :: LoadDistributionWasPerformed
  LOGICAL              :: DummyInit
  LOGICAL, PARAMETER   :: DoNotGetCompSizFac = .FALSE.
  CHARACTER(len=6), PARAMETER, DIMENSION(2) :: LoopSideNames = (/'Demand','Supply'/)

  !~ General variables
  TYPE (Location), DIMENSION(:), ALLOCATABLE, SAVE :: AccessibleBranches
  INTEGER,         DIMENSION(:), ALLOCATABLE, SAVE :: LastComponentSimulated
  REAL(r64)                                        :: LoadToLoopSetPoint
  TYPE(Location)                                   :: PumpLocation

  INTEGER :: curCompOpSchemePtr
  INTEGER :: OpSchemePtr
  !~ Debug variables

  ! We only need to reallocate the accessible array and reset the LastComponentSimulated if
  !  either is currently NOT allocated, or if we are coming into this routine with a
  !  new simulation region.  Otherwise leave it alone and save computation time
  IF (      (.NOT. ALLOCATED(AccessibleBranches))     &
       .OR. (.NOT. ALLOCATED(LastComponentSimulated)) &
       .OR. (LoopNum        .NE. LastLoopNum)         &
       .OR. (LoopSideNum    .NE. LastLoopSideNum)     &
       .OR. (FirstBranchNum .NE. LastFirstBranchNum)  &
       .OR. (LastBranchNum  .NE. LastLastBranchNum)   &
       .OR. (PRESENT(StartingNewLoopSidePass))        &
     ) THEN !we need to reallocate the accessible branch array

    ! How many will we need?
    NumBranchesInRegion = LastBranchNum - FirstBranchNum + 1

    ! Release the memory for the arrays to reset
    IF(ALLOCATED(AccessibleBranches)) DEALLOCATE(AccessibleBranches)
    IF(ALLOCATED(LastComponentSimulated)) DEALLOCATE(LastComponentSimulated)

    ! Reallocate for the number of locations we have available
    ALLOCATE(AccessibleBranches(NumBranchesInRegion))
    ALLOCATE(LastComponentSimulated(NumBranchesInRegion))
    LastComponentSimulated = 0

    BranchIndex = 0
    DO BranchCounter = FirstBranchNum, LastBranchNum
      BranchIndex = BranchIndex + 1
      AccessibleBranches(BranchIndex)%LoopNum     = LoopNum
      AccessibleBranches(BranchIndex)%LoopSideNum = LoopSideNum
      AccessibleBranches(BranchIndex)%BranchNum   = BranchCounter
    END DO

  END IF

  ! Store the arguments for the next call
  LastLoopNum        = LoopNum
  LastLoopSideNum    = LoopSideNum
  LastFirstBranchNum = FirstBranchNum
  LastLastBranchNum  = LastBranchNum

  ! Initialize this flag to false every time so we can call other routines
  DummyInit = .FALSE.

  ! If we are starting a new loop side, we need to initialize the encountered object
  IF (PRESENT(StartingNewLoopSidePass)) THEN
    EncounteredLRBObjDuringPass1   = .FALSE.
    EncounteredNonLBObjDuringPass2 = .FALSE.
    EncounteredAnyLRBObjects       = .FALSE.
  END IF

  ! We now know what plant simulation region is available to us, let's simulate this group
  EncounteredLRBObjDuringPass1 = .FALSE.
  BranchIndex = 0
  DO BranchCounter = FirstBranchNum, LastBranchNum
    BranchIndex = BranchIndex + 1

    !~ Always start from the last component we did the last time around + 1 and
    !~  try to make it all the way to the end of the loop
    StartingComponent = LastComponentSimulated(BranchIndex) + 1
    EndingComponent   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%TotalComponents
    DO CompCounter = StartingComponent, EndingComponent

      SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%Comp(CompCounter)%CurOpSchemeType)
        CASE (WSEconOpSchemeType)   !~ coils
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%Comp(CompCounter)%MyLoad = UpdatedDemandToLoopSetPoint
          CALL SimPlantEquip(LoopNum,LoopSideNum,BranchCounter,CompCounter,FirstHVACIteration,DummyInit,DoNotGetCompSizFac)
        CASE (LoadRangeBasedMin:LoadRangeBasedMax) !~ load range based
          EncounteredLRBObjDuringPass1 = .TRUE.
          EXIT ! don't do any more components on this branch
        CASE (PumpOpSchemeType)       !~ pump
          PumpLocation%LoopNum     = LoopNum
          PumpLocation%LoopSideNum = LoopSideNum
          PumpLocation%BranchNum   = BranchCounter
          PumpLocation%CompNum     = CompCounter
          IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%BranchPumpsExist) THEN
            CALL SimulateAllLoopSidePumps(LoopNum, LoopSideNum, &
                                SpecificPumpLocation = PumpLocation, &
                                SpecificPumpFlowRate = &
                                          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%RequestedMassFlow)
          ELSE
            CALL SimulateAllLoopSidePumps(LoopNum, LoopSideNum, &
                                    SpecificPumpLocation = PumpLocation, &
                                    SpecificPumpFlowRate = FlowRequest)
          ENDIF

        CASE (CompSetPtBasedSchemeType)
          CALL ManagePlantLoadDistribution(LoopNum,LoopSideNum, BranchCounter, CompCounter, LoadToLoopSetPoint, &
                                             LoadToLoopSetPointThatWasntMet, FirstHVACIteration, LoopShutDownFlag, &
                                             LoadDistributionWasPerformed)
          CALL SimPlantEquip(LoopNum,LoopSideNum,BranchCounter,CompCounter,FirstHVACIteration,DummyInit,DoNotGetCompSizFac)
        CASE ( EMSOpSchemeType )
          IF (LoopSideNum == SupplySide) THEN
            curCompOpSchemePtr = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%Comp(CompCounter)%CurCompLevelOpNum
            OpSchemePtr = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%&
                                     Comp(CompCounter)%OpScheme(curCompOpSchemePtr)%OpSchemePtr
            PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EMSIntVarLoopDemandRate = InitialDemandToLoopSetPoint
          ENDIF
          CALL ManagePlantLoadDistribution(LoopNum,LoopSideNum, BranchCounter, CompCounter, UpdatedDemandToLoopSetPoint, &
                                             LoadToLoopSetPointThatWasntMet, FirstHVACIteration, LoopShutDownFlag, &
                                             LoadDistributionWasPerformed)
          CALL SimPlantEquip(LoopNum,LoopSideNum,BranchCounter,CompCounter,FirstHVACIteration,DummyInit,DoNotGetCompSizFac)
        CASE DEFAULT  !demand, , etc.
          CALL SimPlantEquip(LoopNum,LoopSideNum,BranchCounter,CompCounter,FirstHVACIteration,DummyInit,DoNotGetCompSizFac)
      END SELECT

      ! Update loop demand as needed for changes this component may have made
      CALL UpdateAnyLoopDemandAlterations(LoopNum, LoopSideNum, BranchCounter, CompCounter)

      !~ If we didn't EXIT early, we must have simulated, so update array
      LastComponentSimulated(BranchIndex) = CompCounter

    END DO !~ CompCounter

    IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock == FlowLocked) THEN
      CALL SimPressureDropSystem(LoopNum, FirstHVACIteration, PressureCall_Calc, LoopSideNum, BranchCounter)
    END IF

  END DO !~ BranchCounter

  ! So now we have made one pass through all of the available components on these branches, skipping load based
  ! If we didn't encounter any load based objects during the first pass, then we must be done!
  IF (.NOT. EncounteredLRBObjDuringPass1) RETURN

  ! If we have load based now, we should go ahead and distribute the load
  ! If not then this branch group is done, since flow path validation was previously done
  LoadToLoopSetPoint = UpdatedDemandToLoopSetPoint
  LoadDistributionWasPerformed = .FALSE.

  ! The way the load distribution is set up, I think I should call this for every load range based component
  !  encountered until distribution is actually performed.  If we don't call for each component then we may
  !  call for a component that is not on the current equip list and then nothing would come on.
  EncounteredNonLBObjDuringPass2 = .FALSE.
  BranchIndex                    = 0
  DO BranchCounter = FirstBranchNum, LastBranchNum
    BranchIndex = BranchIndex + 1

    !~ Always start from the last component we did the last time around + 1 and
    !~  try to make it all the way to the end of the loop
    StartingComponent = LastComponentSimulated(BranchIndex) + 1
    EndingComponent   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%TotalComponents
    DO CompCounter = StartingComponent, EndingComponent

      SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%Comp(CompCounter)%CurOpSchemeType)
        CASE (NoControlOpSchemeType)       !~ pipes, for example
          CALL SimPlantEquip(LoopNum,LoopSideNum,BranchCounter,CompCounter,FirstHVACIteration,DummyInit,DoNotGetCompSizFac)
        CASE (DemandOpSchemeType, CompSetPtBasedSchemeType, FreeRejectionOpSchemeType)   !~ other control types
          EncounteredNonLBObjDuringPass2 = .TRUE.
          EXIT ! don't do anymore components on this branch
        CASE (LoadRangeBasedMin:LoadRangeBasedMax) !~ load range based
          EncounteredAnyLRBObjects = .TRUE.
          IF (.NOT. LoadDistributionWasPerformed) THEN !~ Still need to distribute load among load range based components
            CALL ManagePlantLoadDistribution(LoopNum,LoopSideNum, BranchCounter, CompCounter, LoadToLoopSetPoint, &
                                             LoadToLoopSetPointThatWasntMet, FirstHVACIteration, LoopShutDownFlag, &
                                             LoadDistributionWasPerformed)
          END IF
          CALL SimPlantEquip(LoopNum,LoopSideNum,BranchCounter,CompCounter,FirstHVACIteration,DummyInit,DoNotGetCompSizFac)
        CASE (PumpOpSchemeType)       !~ pump
          PumpLocation%LoopNum     = LoopNum
          PumpLocation%LoopSideNum = LoopSideNum
          PumpLocation%BranchNum   = BranchCounter
          PumpLocation%CompNum     = CompCounter
          IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%BranchPumpsExist) THEN
            CALL SimulateAllLoopSidePumps(LoopNum, LoopSideNum, &
                                SpecificPumpLocation = PumpLocation, &
                                SpecificPumpFlowRate = &
                                          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%RequestedMassFlow)
          ELSE
            CALL SimulateAllLoopSidePumps(LoopNum, LoopSideNum, &
                                    SpecificPumpLocation = PumpLocation, &
                                    SpecificPumpFlowRate = FlowRequest)
          ENDIF
      END SELECT

      !~ If we didn't EXIT early, we must have simulated, so update array
      LastComponentSimulated(BranchIndex) = CompCounter

    END DO !~ CompCounter

    !~ If we are locked, go ahead and simulate the pressure components on this branch
    IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock == FlowLocked) THEN
      CALL SimPressureDropSystem(LoopNum, FirstHVACIteration, PressureCall_Calc, LoopSideNum, BranchCounter)
    END IF


  END DO !~ BranchCounter


  ! So now we have made the load range based pass through all the components on each branch
  ! If we didn't see any other component types, then we are done, go away
  IF (.NOT. EncounteredNonLBObjDuringPass2) RETURN

  ! If we did encounter other objects than we just need to go back through and simulate them
  BranchIndex = 0
  DO BranchCounter = FirstBranchNum, LastBranchNum
    BranchIndex = BranchIndex + 1

    !~ Always start from the last component we did the last time around + 1 and
    !~  try to make it all the way to the end of the loop
    StartingComponent = LastComponentSimulated(BranchIndex) + 1
    EndingComponent   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%TotalComponents
    DO CompCounter = StartingComponent, EndingComponent

      SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%Comp(CompCounter)%CurOpSchemeType)
        CASE (DemandOpSchemeType)   !~ coils
          CALL SimPlantEquip(LoopNum,LoopSideNum,BranchCounter,CompCounter,FirstHVACIteration,DummyInit,DoNotGetCompSizFac)
        CASE (LoadRangeBasedMin:LoadRangeBasedMax) !~ load range based
          CALL ShowFatalError('Encountered Load Based Object after other components, invalid.')
        CASE (PumpOpSchemeType)     !~ pump
          PumpLocation%LoopNum     = LoopNum
          PumpLocation%LoopSideNum = LoopSideNum
          PumpLocation%BranchNum   = BranchCounter
          PumpLocation%CompNum     = CompCounter
          IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%BranchPumpsExist) THEN
            CALL SimulateAllLoopSidePumps(LoopNum, LoopSideNum, &
                                SpecificPumpLocation = PumpLocation, &
                                SpecificPumpFlowRate = &
                                          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%RequestedMassFlow)
          ELSE
            CALL SimulateAllLoopSidePumps(LoopNum, LoopSideNum, &
                                    SpecificPumpLocation = PumpLocation, &
                                    SpecificPumpFlowRate = FlowRequest)
          ENDIF
        CASE DEFAULT                !~ Typical control equipment
          CALL SimPlantEquip(LoopNum,LoopSideNum,BranchCounter,CompCounter,FirstHVACIteration,DummyInit,DoNotGetCompSizFac)
      END SELECT

      !~ If we didn't EXIT early, we must have simulated, so update array
      LastComponentSimulated(BranchIndex) = CompCounter

    END DO !~ CompCounter

    IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock == FlowLocked) THEN
      CALL SimPressureDropSystem(LoopNum, FirstHVACIteration, PressureCall_Calc, LoopSideNum, BranchCounter)
    END IF

  END DO !~ BranchCounter

  ! I suppose I could do a check on the last component simulated to make sure we actually exhausted all branches
  ! This would be the "THIRD" check on flow validation, but would be OK

 RETURN

END SUBROUTINE SimulateLoopSideBranchGroup
!==================================================================!
!==================================================================!
!==================================================================!

!==================================================================!
!==================== SIMULATE LOOP SIDE PUMPS ====================!
!==================================================================!
SUBROUTINE SimulateAllLoopSidePumps(LoopNum, ThisSide, SpecificPumpLocation, SpecificPumpFlowRate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! USE STATEMENTS:
 USE DataPlant,    ONLY: PlantLoop, TotNumLoops
 USE DataLoopNode, ONLY: Node
 USE Pumps,        ONLY: SimPumps

 IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,        INTENT(IN)                            :: LoopNum
  INTEGER,        INTENT(IN)                            :: ThisSide
  TYPE(Location), OPTIONAL,               INTENT(IN)    :: SpecificPumpLocation
  REAL(r64),      OPTIONAL,               INTENT(IN)    :: SpecificPumpFlowRate

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: LoopCounter
  INTEGER       :: LoopSideCounter
  INTEGER       :: PumpCounter
  INTEGER       :: PumpIndexStart
  INTEGER       :: PumpIndexEnd
  REAL(r64)     :: FlowToRequest
  LOGICAL       :: ThisPumpRunning
  REAL(r64)     :: ThisPumpFlowRate
  REAL(r64)     :: ThisPumpMinAvail
  REAL(r64)     :: ThisPumpMaxAvail
  INTEGER       :: PumpLoopNum
  INTEGER       :: PumpLoopSideNum
  INTEGER       :: PumpBranchNum
  INTEGER       :: PumpCompNum
  INTEGER       :: PumpOutletNode
  LOGICAL, SAVE :: EstablishedCompPumpIndeces = .FALSE.

  !~ One time sweep through all loops/loopsides/pumps, assigning indeces to the pl%ls%br%comp%indexinloopsidepumps variable
  IF (.NOT. EstablishedCompPumpIndeces) THEN
    DO LoopCounter = 1, TotNumLoops
      DO LoopSideCounter = 1, 2
        DO PumpCounter = 1, PlantLoop(LoopCounter)%LoopSide(LoopSideCounter)%TotalPumps
          PumpBranchNum  = PlantLoop(LoopCounter)%LoopSide(LoopSideCounter)%Pumps(PumpCounter)%BranchNum
          PumpCompNum    = PlantLoop(LoopCounter)%LoopSide(LoopSideCounter)%Pumps(PumpCounter)%CompNum
          PlantLoop(LoopCounter)%LoopSide(LoopSideCounter)%Branch(PumpBranchNum)%Comp(PumpCompNum)%IndexInLoopSidePumps &
                         = PumpCounter
        END DO
      END DO
    END DO
    EstablishedCompPumpIndeces = .TRUE.
  END IF

  ! If we have a specific loop/side/br/comp, then find the index and only do that one, otherwise do all pumps on the loop side
  IF (PRESENT(SpecificPumpLocation)) THEN
    PumpLoopNum     = SpecificPumpLocation%LoopNum
    PumpLoopSideNum = SpecificPumpLocation%LoopSideNum
    PumpBranchNum   = SpecificPumpLocation%BranchNum
    PumpCompNum     = SpecificPumpLocation%CompNum
    PumpIndexStart  = &
        PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Branch(PumpBranchNum)%Comp(PumpCompNum)%IndexInLoopSidePumps
    PumpIndexEnd    = PumpIndexStart
  ELSE
    PumpLoopNum     = LoopNum
    PumpLoopSideNum = ThisSide
    PumpIndexStart  = 1
    PumpIndexEnd    = PlantLoop(LoopNum)%LoopSide(ThisSide)%TotalPumps
  END IF

  ! If we have a flow rate to hit, then go for it, otherwise, just operate in request mode with zero flow
  IF (PRESENT(SpecificPumpFlowRate)) THEN
    FlowToRequest = SpecificPumpFlowRate
  ELSE
    FlowToRequest = 0.0d0
  END IF

  !~ Now loop through all the pumps and simulate them, keeping track of their status
  DO PumpCounter = PumpIndexStart, PumpIndexEnd

    !~ Set some variables
    PumpBranchNum  = PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Pumps(PumpCounter)%BranchNum
    PumpCompNum    = PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Pumps(PumpCounter)%CompNum
    PumpOutletNode = PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Pumps(PumpCounter)%PumpOutletNode

    CALL AdjustPumpFlowRequestByEMSControls(PumpLoopNum, PumpLoopSideNum, PumpBranchNum, PumpCompNum, FlowToRequest)

    ! Call SimPumps, routine takes a flow request, and returns some info about the status of the pump
    CALL SimPumps(PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Pumps(PumpCounter)%PumpName,     &
                  PumpLoopNum,                                                                      &
                  FlowToRequest,                                                                    &
                  ThisPumpRunning,                                                                  &
                  PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Branch(PumpBranchNum)%PumpIndex, &
                  PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Pumps(PumpCounter)%PumpHeatToFluid)

    !~ Pull some state information from the pump outlet node
    ThisPumpFlowRate = Node(PumpOutletNode)%MassFlowRate
    ThisPumpMinAvail = Node(PumpOutletNode)%MassFlowRateMinAvail
    ThisPumpMaxAvail = Node(PumpOutletNode)%MassFlowRateMaxAvail

    !~ Now update the data structure
    PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Pumps(PumpCounter)%CurrentMinAvail = ThisPumpMinAvail
    PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Pumps(PumpCounter)%CurrentMaxAvail = ThisPumpMaxAvail

  END DO

  !~ Update the loopside pump heat totality here
  IF(PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%TotalPumps > 0) THEN
    PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%TotalPumpHeat = &
      SUM(PlantLoop(PumpLoopNum)%LoopSide(PumpLoopSideNum)%Pumps%PumpHeatToFluid)
  END IF

 RETURN

END SUBROUTINE SimulateAllLoopSidePumps
!==================================================================!
!==================================================================!
!==================================================================!


!==================================================================!
!============ EVALUATE LOAD REQUIRED FOR WHOLE LOOP ===============!
!==================================================================!
REAL(r64) FUNCTION CalcOtherSideDemand(LoopNum, ThisSide) RESULT(Demand)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! To evaluate the demand to hit the loop setpoint based on the loop side inlet conditions

          ! METHODOLOGY EMPLOYED:
          ! This routine will simply call the evaluate loop setpoint routine but call it from
          !  the very beginning of this loop side, so that it is basically for the entire loop side

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: LoopNum
  INTEGER, INTENT(IN) :: ThisSide

          ! FUNCTION PARAMETER DEFINITIONS:
  INTEGER, DIMENSION(1), PARAMETER :: InitCompArray = (/0/)

  Demand = EvaluateLoopSetPointLoad(LoopNum, ThisSide, 1, 1, InitCompArray)

 RETURN

END FUNCTION CalcOtherSideDemand
!==================================================================!
!==================================================================!
!==================================================================!


!==================================================================!
!========= EVALUATE LOAD REQUIRED TO MEET LOOP SETPOINT ===========!
!==================================================================!
FUNCTION EvaluateLoopSetPointLoad(LoopNum, LoopSideNum, FirstBranchNum, LastBranchNum, LastComponentSimulated) &
   RESULT (LoadToLoopSetPoint)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! USE STATEMENTS:
 USE DataPlant,    ONLY: PlantLoop, LoopDemandTol, SingleSetPoint, DualSetPointDeadBand
 USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
 USE DataLoopNode, ONLY: Node, NodeType_Water, NodeType_Steam
 USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetSatEnthalpyRefrig
 USE General,         ONLY: RoundSigDigits

 IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER               :: LoopNum
  INTEGER               :: LoopSideNum
  INTEGER               :: FirstBranchNum
  INTEGER               :: LastBranchNum
  INTEGER, DIMENSION(:) :: LastComponentSimulated
  REAL(r64)             :: LoadToLoopSetPoint !function result
          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  !~ Indexing variables
  INTEGER              :: BranchCounter      !~ This contains the index for the %Branch(:) structure
  INTEGER              :: BranchIndex        !~ This is a 1 - n value within the current branch group
  INTEGER              :: StartingComponent  !~ The component which "would" be simulated next

  !~ General variables
  REAL(r64)                 :: EnteringTemperature
  REAL(r64)                 :: MassFlowRate
  REAL(r64)                 :: SumMdotTimesTemp
  REAL(r64)                 :: SumMdot
  REAL(r64)                 :: WeightedInletTemp
  REAL(r64)                 :: LoopSetPointTemperature
  REAL(r64)                 :: LoopSetPointTemperatureHi
  REAL(r64)                 :: LoopSetPointTemperatureLo
  REAL(r64)                 :: LoadtoHeatingSetPoint
  REAL(r64)                 :: LoadtoCoolingSetPoint
  REAL(r64)                 :: DeltaTemp
  INTEGER                   :: EnteringNodeNum
  REAL(r64)                 :: Cp
  REAL(r64)                 :: EnthalpySteamSatVapor  ! Enthalpy of saturated vapor
  REAL(r64)                 :: EnthalpySteamSatLiquid ! Enthalpy of saturated liquid
  REAL(r64)                 :: LatentHeatSteam        ! Latent heat of steam

  ! Initialize
  LoadToLoopSetPoint = 0.0d0

  ! Sweep across flow paths in this group and calculate the deltaT and then the load
  BranchIndex = 0
  SumMdotTimesTemp = 0.d0
  SumMdot = 0.d0
  DO BranchCounter = FirstBranchNum, LastBranchNum

    BranchIndex = BranchIndex + 1

    !~ Always start from the last component we did the last time around + 1 and
    !~  try to make it all the way to the end of the loop
    StartingComponent = LastComponentSimulated(BranchIndex) + 1
    EnteringNodeNum   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchCounter)%Comp(StartingComponent)%NodeNumIn

    EnteringTemperature = Node(EnteringNodeNum)%Temp
    MassFlowRate        = Node(EnteringNodeNum)%MassFlowRate

    SumMdotTimesTemp = SumMdotTimesTemp + (EnteringTemperature * MassFlowRate)
    SumMdot = SumMdot + (MassFlowRate)

  END DO

  IF ( SumMdot .LT. MassFlowTolerance ) THEN
    LoadToLoopSetPoint = 0.0d0
    RETURN
  END IF

  WeightedInletTemp = SumMdotTimesTemp / SumMdot

  IF (PlantLoop(LoopNum)%FluidType==NodeType_Water) THEN

    Cp = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName, WeightedInletTemp, &
                               PlantLoop(LoopNum)%FluidIndex, 'PlantLoopSolver::EvaluateLoopSetPointLoad')

    SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)

      CASE (SingleSetPoint)

        ! Pick up the loop setpoint temperature
        LoopSetPointTemperature = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetPoint
        ! Calculate the delta temperature
        DeltaTemp    = LoopSetPointTemperature - WeightedInletTemp

        ! Calculate the demand on the loop
        LoadToLoopSetPoint = SumMdot * Cp * DeltaTemp

      CASE (DualSetPointDeadBand)

        ! Get the range of setpoints
        LoopSetPointTemperatureHi = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetpointHi
        LoopSetPointTemperatureLo = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetpointLo

        !Calculate the demand on the loop
        IF (SumMdot > 0.0d0) THEN
          LoadtoHeatingSetPoint = SumMdot*Cp*(LoopSetPointTemperatureLo - WeightedInletTemp)
          LoadtoCoolingSetPoint = SumMdot*Cp*(LoopSetPointTemperatureHi - WeightedInletTemp)
          ! Possible combinations:
          ! 1  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
          ! 2  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
          ! 3  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
          ! 4  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Not Feasible if LoopSetPointHi >= LoopSetPointLo
  ! First trap bad set-points
          IF (LoadToHeatingSetPoint .GT. LoadToCoolingSetPoint ) THEN
            CALL ShowSevereError('Plant Loop: the Plant Loop Demand Calculation Scheme is set to DualSetPointDeadBand, '// &
                                 'but the heating-related low setpoint appears to be above the cooling-related high setpoint.')
            CALL ShowContinueError('For example, if using SetpointManager:Scheduled:DualSetpoint, then check that the' // &
                                   ' low setpoint is below the high setpoint.')
            CALL ShowContinueError('Occurs in PlantLoop='//TRIM(PlantLoop(LoopNum)%Name))
            CALL ShowContinueError('LoadToHeatingSetPoint='//TRIM(RoundSigDigits(LoadToHeatingSetPoint,3))//  &
                                   ', LoadToCoolingSetPoint='//TRIM(RoundSigDigits(LoadToCoolingSetPoint,3)))
            CALL ShowContinueError('Loop Heating Low Setpoint='//TRIM(RoundSigDigits(LoopSetPointTemperatureLo,2)))
            CALL ShowContinueError('Loop Cooling High Setpoint='//TRIM(RoundSigDigits(LoopSetPointTemperatureHi,2)))

            CALL ShowFatalError('Program terminates due to above conditions.')
          END IF
          IF (LoadToHeatingSetPoint .GT. 0.0d0 .AND. LoadToCoolingSetPoint .GT. 0.0d0) THEN
            LoadToLoopSetPoint = LoadToHeatingSetPoint
          ELSE IF (LoadToHeatingSetPoint .LT. 0.0d0 .AND. LoadToCoolingSetPoint .LT. 0.0d0) THEN
            LoadToLoopSetPoint = LoadToCoolingSetPoint
          ELSE IF (LoadToHeatingSetPoint .LE. 0.0d0 .AND. LoadToCoolingSetPoint .GE. 0.0d0) THEN ! deadband includes zero loads
            LoadToLoopSetPoint = 0.0d0
          ELSE
            CALL ShowSevereError('DualSetPointWithDeadBand: Unanticipated combination of heating and cooling loads - '// &
                                 'report to EnergyPlus Development Team')
            CALL ShowContinueError('occurs in PlantLoop='//TRIM(PlantLoop(LoopNum)%Name))
            CALL ShowContinueError('LoadToHeatingSetPoint='//TRIM(RoundSigDigits(LoadToHeatingSetPoint,3))//  &
                                   ', LoadToCoolingSetPoint='//TRIM(RoundSigDigits(LoadToCoolingSetPoint,3)))
            CALL ShowContinueError('Loop Heating Setpoint='//TRIM(RoundSigDigits(LoopSetPointTemperatureLo,2)))
            CALL ShowContinueError('Loop Cooling Setpoint='//TRIM(RoundSigDigits(LoopSetPointTemperatureHi,2)))
            CALL ShowFatalError('Program terminates due to above conditions.')
          END IF
        ELSE
          LoadToLoopSetPoint = 0.0d0
        END IF

      END SELECT


    ELSEIF (PlantLoop(LoopNum)%FluidType==NodeType_Steam) THEN

      Cp = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName, WeightedInletTemp, &
                                 PlantLoop(LoopNum)%FluidIndex, 'PlantLoopSolver::EvaluateLoopSetPointLoad')

      SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)

      CASE (SingleSetPoint)

        ! Pick up the loop setpoint temperature
        LoopSetPointTemperature = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetPoint

        ! Calculate the delta temperature
        DeltaTemp    = LoopSetPointTemperature - WeightedInletTemp

        EnthalpySteamSatVapor  =  GetSatEnthalpyRefrig('STEAM',LoopSetPointTemperature,1.0d0,RefrigIndex, &
                                               'PlantSupplySide:EvaluateLoopSetPointLoad')
        EnthalpySteamSatLiquid =  GetSatEnthalpyRefrig('STEAM',LoopSetPointTemperature,0.0d0,RefrigIndex, &
                                               'PlantSupplySide:EvaluateLoopSetPointLoad')

        LatentHeatSteam = EnthalpySteamSatVapor - EnthalpySteamSatLiquid

        ! Calculate the demand on the loop
        LoadToLoopSetPoint = SumMdot * ( Cp * DeltaTemp + LatentHeatSteam )


      END SELECT

    ELSE  ! only have two types, water serves for glycol.


    END IF

    ! Trim the demand to zero if it is very small
    IF(ABS(LoadToLoopSetPoint) < LoopDemandTol) LoadToLoopSetPoint = 0.0d0

 RETURN

END FUNCTION EvaluateLoopSetPointLoad
!==================================================================!
!==================================================================!
!==================================================================!

SUBROUTINE UpdateAnyLoopDemandAlterations(LoopNum, LoopSideNum, BranchNum, CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine will analyze the given component and determine if any
          !  alterations need to be made to the current loop demand value.  If so,
          !  it will make the changes to the module level loop demand variables.

          ! METHODOLOGY EMPLOYED:
          ! Components will always supply a useful delta T, even if it happens to be zero
          ! For flow rate, make decisions based on the component's current operating scheme type:
          !  • Demand based: these components will have a flow request on their inlet node
          !  • Pump: these components will not be included, as they no longer include heat at the pump
          !  • component setpoint: these components will have a flow request

          !    on their outlet node corresponding to their calculated delta T
          !  • load range based: these components do not 'alter' the load, they reject the load
          !    Therefore they are not included


          ! USE STATEMENTS:
  USE DataPlant,  ONLY: PlantLoop, DemandOpSchemeType, PumpOpSchemeType, LoadRangeBasedMin, LoadRangeBasedMax, &
                        NoControlOpSchemeType, CompSetPtBasedSchemeType, FreeRejectionOpSchemeType, WSEconOpSchemeType, &
                        UnknownStatusOpSchemeType, FlowUnlocked, FlowLocked
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE DataLoopNode, ONLY: Node
  USE FluidProperties, ONLY: GetSpecificHeatGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: LoopNum
  INTEGER, INTENT(IN) :: LoopSideNum
  INTEGER, INTENT(IN) :: BranchNum
  INTEGER, INTENT(IN) :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ComponentCp
  REAL(r64) :: ComponentMassFlowRate
  INTEGER   :: InletNode
  REAL(r64) :: InletTemp
  INTEGER   :: OutletNode
  REAL(r64) :: OutletTemp
  REAL(r64) :: AverageTemp
  REAL(r64) :: LoadAlteration

  ! Init to zero, so that if we don't find anything, we exit early
  ComponentMassFlowRate = 0.0d0

  ! Get information
  InletNode   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
  OutletNode  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut

  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock == FlowUnLocked) THEN

    ! For unlocked flow, use the inlet request -- !DSU? for now
    SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType)
      CASE (LoadRangeBasedMin:LoadRangeBasedMax)
        ! Don't do anything for load based components
      CASE DEFAULT
        ! pumps pipes, etc. will be lumped in here with other component types, but they will have no delta T anyway
        ComponentMassFlowRate = Node(InletNode)%MassFlowRateRequest
        !DSU? make sure components like economizers use the mass flow request
    END SELECT

  ELSEIF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock == FlowLocked) THEN

    ! For locked flow just use the mass flow rate
    SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType)
      CASE (LoadRangeBasedMin:LoadRangeBasedMax)
        ! Don't do anything for load based components
      CASE DEFAULT
        ! pumps pipes, etc. will be lumped in here with other component types, but they will have no delta T anyway
        ComponentMassFlowRate = Node(OutletNode)%MassFlowRate
    END SELECT

  ELSE ! flow pump query? problem?

  END IF

  ! Leave early if there wasn't a mass flow rate or request
  IF (ComponentMassFlowRate .LT. MassFlowTolerance) RETURN

  ! Get an average temperatre for the property call
  InletTemp   = Node(InletNode)%Temp
  OutletTemp  = Node(OutletNode)%Temp
  AverageTemp = (InletTemp + OutletTemp) / 2
  ComponentCp = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName, AverageTemp, &
                                      PlantLoop(LoopNum)%FluidIndex, 'PlantLoopSolver::UpdateAnyLoopDemandAlterations')

  ! Calculate the load altered by this component
  LoadAlteration = ComponentMassFlowRate * ComponentCp * (OutletTemp - InletTemp)

  ! Now alter the module level variables
  CurrentAlterationsToDemand = CurrentAlterationsToDemand + LoadAlteration
  UpdatedDemandToLoopSetPoint = InitialDemandToLoopSetPoint - CurrentAlterationsToDemand

  RETURN

END SUBROUTINE UpdateAnyLoopDemandAlterations
!==================================================================!
!==================================================================!
!==================================================================!

!==================================================================!
!=================== FLOW RESOLVER ROUTINE ========================!
!==================================================================!
SUBROUTINE ResolveParallelFlows(LoopNum, LoopSideNum, ThisLoopSideFlow, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brandon Anderson, Dan Fisher
          !       DATE WRITTEN   October 1999
          !       MODIFIED       May 2005 Sankaranarayanan K P, Rich Liesen
          !       RE-ENGINEERED  Sept 2010 Dan Fisher, Brent Griffith for demand side update

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine takes the overall loop side flow and distributes
          ! it among parallel branches. this is the main implementation of
          ! flow splitting for plant splitter/mixer

          ! METHODOLOGY EMPLOYED:
          ! Flow through the branches is currently determined by
          ! the active component on the branch, as well as the
          ! order of the branches following the splitter.
          ! SimPlantEquipment is run first, and the active components
          ! request their flow.  These flows are compared and a simple
          ! algorithm balances flow in the branches.  The flow in these
          ! branches is then locked down, via MassFlowRateMaxAvail and MinAvail
          ! SimPlant Equipment is then run again in order to get correct
          ! properties.  Finally, Max/MinAvail are reset for the next time step.

          ! USE STATEMENTS:
  USE DataPlant,      ONLY: PlantLoop, TypeOf_PumpVariableSpeed, TypeOf_PumpBankVariableSpeed
  USE DataBranchAirLoopPlant, ONLY: ControlType_Unknown,ControlType_Active, ControlType_Passive, &
                                    ControlType_SeriesActive, ControlType_Bypass, MassFlowTolerance
  USE DataLoopNode,   ONLY: Node
  USE DataInterfaces, ONLY: ShowSevereError, ShowContinueError, ShowContinueErrorTimeStamp
  USE General,        ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                       :: LoopNum         !plant loop number that we are balancing flow for
  INTEGER, INTENT(IN)                       :: LoopSideNum     !plant loop number that we are balancing flow for
  REAL(r64), INTENT(IN)   :: ThisLoopSideFlow ! [kg/s]  total flow to be split
  LOGICAL, INTENT(IN)     :: FirstHVACIteration   ! TRUE if First HVAC iteration of Time step


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=6), DIMENSION(2), PARAMETER :: LoopSideName = (/'Demand','Supply'/)
  INTEGER,                        PARAMETER :: SplitNum = 1        ! Only one splitter/mixer combination is allowed
  INTEGER,                        PARAMETER :: LoopSideSingleBranch = 1 ! For readability

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: NumActiveBranches      !Active branch counter
  REAL(r64) :: ActiveFlowRate      !The flow available when cycling through branches
  REAL(r64) :: PassiveFlowRate      !The flow available when cycling through branches
  REAL(r64) :: FracFlow            !The flow available when cycling through branches
  REAL(r64) :: ThisBranchRequestFrac      !The request ratio
  REAL(r64) :: totalMax      !The flow available when cycling through branches
  REAL(r64) :: FlowRemaining      !The flow available when cycling through branches
  INTEGER   :: OutletNum              !Splitter outlet
  INTEGER   :: MixerBranchOut
  INTEGER   :: SplitterBranchIn       !As the name implies
  INTEGER   :: SplitterBranchOut      !As the name implies
  INTEGER   :: LastNodeOnBranch       ! intermediate value used for better readabilty
  INTEGER   :: FirstNodeOnBranch       ! intermediate value used for better readabilty
  INTEGER   :: BranchNum       ! intermediate value used for better readabilty
  INTEGER   :: iBranch         ! DO loop counter for cycling through branches
  INTEGER   :: NumSplitOutlets      !As the name implies
  REAL(r64) :: OutletBranchMinAvail
  REAL(r64) :: OutletBranchMaxAvail
  REAL(r64) :: InletBranchMinAvail
  REAL(r64) :: InletBranchMaxAvail
  REAL(r64) :: BranchFlowReq
  REAL(r64) :: BranchMinAvail
  REAL(r64) :: BranchMaxAvail
  REAL(r64) :: ParallelBranchMaxAvail
  REAL(r64) :: ParallelBranchMinAvail
  REAL(r64) :: TotParallelBranchFlowReq
  REAL(r64) :: LoopFlowRate
  INTEGER   :: FirstNodeOnBranchIn
  INTEGER   :: FirstNodeOnBranchOut
  REAL(r64) :: StartingFlowRate
  REAL(r64) :: ThisBranchRequest
  INTEGER :: CompCounter
  INTEGER :: CompInletNode
  INTEGER :: CompOutletNode

! Error Messages from the old RequestNetworkFlowAndSolve

!
!        IF(PlantLoop(LoopNum)%Loopside(Loopsidenum)%Branch(CurBranch)%ControlType .NE. ControlType_Active .AND. &
!           PlantLoop(LoopNum)%Loopside(Loopsidenum)%Branch(CurBranch)%ControlType .NE. ControlType_Passive) THEN
!          CALL ShowSevereError ('PlantLoop:An Active component can be in series with active or passive components only')
!          CALL ShowContinueError('Occurs in Branch='//TRIM(PlantLoop(LoopNum)%Loopside(Loopsidenum)%Branch(CurBranch)%Name))
!          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
!          CALL ShowFatalError('Preceding condition causes termination.')
!        END IF
!
!      CASE (ControlType_Bypass)
!
!        IF(CurComp .NE. 1) THEN
!          CALL ShowSevereError ('PlantLoop:A Bypass pipe cannot be in series with another component')
!          CALL ShowContinueError('Occurs in Branch='//TRIM(PlantLoop(LoopNum)%Loopside(Loopsidenum)%Branch(CurBranch)%Name))
!          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
!          CALL ShowFatalError('Preceding condition causes termination.')
!        END IF                  !Set Branch ByPass Flag
!
!      CASE (ControlType_SeriesActive)
!
!        IF(CurComp .NE. 1) THEN
!          IF(PlantLoop(LoopNum)%Loopside(Loopsidenum)%Branch(CurBranch)%ControlType .NE. ControlType_SeriesActive) THEN
!            CALL ShowSevereError ('PlantLoop:A SeriesActive component can be in series with SeriesActive components only')
!            CALL ShowContinueError('Occurs in Branch='//TRIM(PlantLoop(LoopNum)%Loopside(Loopsidenum)%Branch(CurBranch)%Name))
!            CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
!            CALL ShowFatalError('Preceding condition causes termination.')
!          END IF
!        END IF
!
!
!      CASE (ControlType_Passive)
!
!        IF(PlantLoop(LoopNum)%Loopside(Loopsidenum)%Branch(CurBranch)%ControlType .NE. ControlType_Active .AND. &
!           PlantLoop(LoopNum)%Loopside(Loopsidenum)%Branch(CurBranch)%ControlType .NE. ControlType_Passive) THEN
!          CALL ShowSevereError ('PlantLoop:A Passive component can be in series with active or passive components only')
!          CALL ShowContinueError('Occurs in Branch='//TRIM(PlantLoop(LoopNum)%Loopside(Loopsidenum)%Branch(CurBranch)%Name))
!          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
!          CALL ShowFatalError('Preceding condition causes termination.')
!        END IF
!


   ! If there is no splitter then there is no continuity to enforce.
  IF (.NOT. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SplitterExists) THEN

        !If there's only one branch, then RETURN
    IF(PlantLoop(LoopNum)%Loopside(Loopsidenum)%TotalBranches == 1)THEN
      ! The branch should just try to meet the request previously calculated.  This should be good,
      ! just need to make sure that during FlowUnlocked, no one constrained Min/Max farther.
      ! This would have been propogated down the branch, so we can check the outlet node min/max avail for this.
      LastNodeOnBranch  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(LoopSideSingleBranch)%NodeNumOut
      FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(LoopSideSingleBranch)%NodeNumIn
      BranchMinAvail    = Node(LastNodeOnBranch)%MassFlowRateMinAvail
      BranchMaxAvail    = Node(LastNodeOnBranch)%MassFlowRateMaxAvail
      Node(FirstNodeOnBranch)%MassFlowRate = MIN(MAX(ThisLoopSideFlow, BranchMinAvail), BranchMaxAvail)
      ! now with flow locked, this single branch will just ran at the specified flow rate, so we are done
      RETURN
    ELSE
      CALL ShowSevereError('Plant topology problem for PlantLoop: '//PlantLoop(LoopNum)%Name//', '// &
                                                                     LoopSideName(LoopSideNum)//' side.')
      CALL ShowContinueError('There are multiple branches, yet no splitter.  This is an invalid configuration.')
      CALL ShowContinueError('Add a set of connectors, use put components on a single branch.')
      CALL ShowFatalError('Invalid plant topology causes program termination.')
      RETURN
    END IF
  END IF

        ! If a splitter/mixer combination exist on the loop
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SplitterExists .AND. &
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%MixerExists) THEN

        ! Zero out local variables
    TotParallelBranchFlowReq = 0.0d0
    NumSplitOutlets = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%TotalOutletNodes
    IF(NumSplitOutlets < 1)THEN
      CALL ShowSevereError('Plant topology problem for PlantLoop: '//PlantLoop(LoopNum)%Name//', '// &
                                                                     LoopSideName(LoopSideNum)//' side.')
      CALL ShowContinueError('Diagnostic error in PlantLoopSolver::ResolveParallelFlows.')
      CALL ShowContinueError('Splitter improperly specified, no splitter outlets.')
      CALL ShowFatalError('Invalid plant topology causes program termination.')
    ENDIF

    NumActiveBranches = 0
    ParallelBranchMaxAvail = 0.0d0
    ParallelBranchMinAvail = 0.0d0
    DO iBranch = 1, NumSplitOutlets
      BranchNum        = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(iBranch)
      SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(iBranch)
      LastNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(branchNum)%NodeNumOut
      FirstNodeOnBranch= PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(branchNum)%NodeNumIn
      BranchFlowReq    = DetermineBranchFlowRequest(LoopNum, LoopSideNum, BranchNum)

      !now, if we are have branch pumps, here is the situation:
      ! constant speed pumps lock in a flow request on the inlet node
      ! variable speed pumps which have other components on the branch do not log a request themselves
      ! the DetermineBranchFlowRequest routine only looks at the branch inlet node
      ! for variable speed branch pumps then, this won't work because the branch will be requesting zero
      ! so let's adjust for this here to make sure these branches get good representation
      DO CompCounter = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents

        !if this isn't a variable speed pump then just keep cycling
        IF ( (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%TypeOf_Num &
              .NE. TypeOf_PumpVariableSpeed) .AND. &
             (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%TypeOf_Num &
              .NE. TypeOf_PumpBankVariableSpeed) ) THEN
          CYCLE
        END IF

        CompInletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%NodeNumIn
        BranchFlowReq = MAX(BranchFlowReq, Node(CompInletNode)%MassFLowRateRequest)

      END DO

      BranchMinAvail   = Node(LastNodeOnBranch)%MassFlowRateMinAvail
      BranchMaxAvail   = Node(LastNodeOnBranch)%MassFlowRateMaxAvail
!            !sum the branch flow requests to a total parallel branch flow request
      IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_Active .OR. &
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_SeriesActive) THEN
         TotParallelBranchFlowReq = TotParallelBranchFlowReq + BranchFlowReq
         NumActiveBranches = NumActiveBranches + 1
      ENDIF
      Node(FirstNodeOnBranch)%MassFlowRate         = BranchFlowReq
      Node(FirstNodeOnBranch)%MassFlowRateMinAvail = BranchMinAvail
      Node(FirstNodeOnBranch)%MassFlowRateMaxAvail = BranchMaxAvail
      ParallelBranchMaxAvail = ParallelBranchMaxAvail + BranchMaxAvail
      ParallelBranchMinAvail = ParallelBranchMinAvail + BranchMinAvail
    END DO
!
!            ! Find branch number and flow rates at splitter inlet
    SplitterBranchIn      = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumIn
    LastNodeOnBranch      = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchIn)%NodeNumOut
    FirstNodeOnBranchIn   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchIn)%NodeNumIn
    InletBranchMinAvail   = Node(LastNodeOnBranch)%MassFlowRateMinAvail
    InletBranchMaxAvail   = Node(LastNodeOnBranch)%MassFlowRateMaxAvail
!            ! Find branch number and flow rates at mixer outlet
    MixerBranchOut        = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(SplitNum)%BranchNumOut
    LastNodeOnBranch      = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(MixerBranchOut)%NodeNumOut
    FirstNodeOnBranchOut  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(MixerBranchOut)%NodeNumIn
    OutletBranchMinAvail  = Node(LastNodeOnBranch)%MassFlowRateMinAvail
    OutletBranchMaxAvail  = Node(LastNodeOnBranch)%MassFlowRateMaxAvail

    LoopFlowRate = ThisLoopSideFlow

          !Reset branch inlet node flow rates for the first and last branch on loop
    Node(FirstNodeOnBranchIn)%MassFlowRate    = ThisLoopSideFlow
    Node(FirstNodeOnBranchOut)%MassFlowRate   = ThisLoopSideFlow

          !Reset branch inlet node Min/MaxAvails for the first and last branch on loop
    Node(FirstNodeOnBranchIn)%MassFlowRateMaxAvail    = MIN(Node(FirstNodeOnBranchIn)%MassFlowRateMaxAvail, &
                                                            ParallelBranchMaxAvail)
    Node(FirstNodeOnBranchIn)%MassFlowRateMaxAvail    = MIN(Node(FirstNodeOnBranchIn)%MassFlowRateMaxAvail, &
                                                            Node(FirstNodeOnBranchOut)%MassFlowRateMaxAvail)
    Node(FirstNodeOnBranchIn)%MassFlowRateMinAvail    = MAX(Node(FirstNodeOnBranchIn)%MassFlowRateMinAvail, &
                                                            ParallelBranchMinAvail)
    Node(FirstNodeOnBranchIn)%MassFlowRateMinAvail    = MAX(Node(FirstNodeOnBranchIn)%MassFlowRateMinAvail, &
                                                            Node(FirstNodeOnBranchOut)%MassFlowRateMinAvail)
    Node(FirstNodeOnBranchOut)%MassFlowRateMinAvail   = Node(FirstNodeOnBranchIn)%MassFlowRateMinAvail
    Node(FirstNodeOnBranchOut)%MassFlowRateMaxAvail   = Node(FirstNodeOnBranchIn)%MassFlowRateMaxAvail

          !Initialize the remaining flow variable
    FlowRemaining = ThisLoopSideFlow

        !Initialize flow on passive, bypass and uncontrolled parallel branches to zero.  For these branches
        !MinAvail is not enforced
    DO OutletNum = 1, NumSplitOutlets
      SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
      FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchOut)%NodeNumIn
      IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType /= ControlType_Active .AND. &
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType /= ControlType_SeriesActive) THEN
        Node(FirstNodeOnBranch)%MassFlowRate = 0.0d0
        CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, SplitterBranchOut,   &
                                           Node(FirstNodeOnBranch)%MassFlowRate, FirstHVACIteration)
      ENDIF
    END DO


!IF SUFFICIENT FLOW TO MEET ALL PARALLEL BRANCH FLOW REQUESTS
    IF (FlowRemaining < MassFlowTolerance) THEN ! no flow available at all for splitter
      DO OutletNum = 1, NumSplitOutlets
        SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
        DO CompCounter = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%TotalComponents

          FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%NodeNumIn
          CompInletNode  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%Comp(CompCounter)%NodeNumIn
          CompOutletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%Comp(CompCounter)%NodeNumOut
          Node(CompInletNode)%MassFlowRate          =  0.d0
          Node(CompInletNode)%MassFlowRateMaxAvail  =  0.d0
          Node(CompOutletNode)%MassFlowRate         =  0.d0
          Node(CompOutletNode)%MassFlowRateMaxAvail =  0.d0
        ENDDO
      END DO
      RETURN
    ELSEIF (FlowRemaining .GE. TotParallelBranchFlowReq) THEN

  ! 1) Satisfy flow demand of ACTIVE splitter outlet branches
      DO OutletNum = 1, NumSplitOutlets
        SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
        FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchOut)%NodeNumIn
        IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_Active .OR. &
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_SeriesActive) THEN
                          ! branch flow is min of requested flow and remaining flow
          Node(FirstNodeOnBranch)%MassFlowRate = MIN(Node(FirstNodeOnBranch)%MassFlowRate,FlowRemaining)
          IF(Node(FirstNodeOnBranch)%MassFlowRate < MassFlowTolerance) Node(FirstNodeOnBranch)%MassFlowRate = 0.0d0
          CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, SplitterBranchOut,   &
                                             Node(FirstNodeOnBranch)%MassFlowRate, FirstHVACIteration)
          FlowRemaining = FlowRemaining - Node(FirstNodeOnBranch)%MassFlowRate
          IF(FlowRemaining < MassFlowTolerance) FlowRemaining = 0.0d0
        ENDIF
      END DO
            !IF the active branches take the entire loop flow, return
      IF(FlowRemaining == 0.0d0)RETURN

  ! 2) Distribute remaining flow to PASSIVE branches
      totalMax = 0.0d0
      DO OutletNum = 1, NumSplitOutlets
        SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
        FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchOut)%NodeNumIn
        IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_Passive) THEN
            !Calculate the total max available
          totalMax = totalMax + Node(FirstNodeOnBranch)%MassFlowRateMaxAvail
        END IF
      END DO

      IF (totalMax > 0) THEN
        DO OutletNum = 1, NumSplitOutlets
          SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
          FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchOut)%NodeNumIn
          IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_Passive) THEN
            FracFlow = FlowRemaining / totalMax
            IF (FracFlow <= 1.0d0) THEN !the passive branches will take all the flow
              PassiveFlowRate = FracFlow * Node(FirstNodeOnBranch)%MassFlowRateMaxAvail
                  !Check against FlowRemaining
              PassiveFlowRate = MIN(FlowRemaining,PassiveFlowRate)
                  !Allow FlowRequest to be increased to meet minimum on branch
              PassiveFlowRate = MAX(PassiveFlowRate,Node(FirstNodeOnBranch)%MassFlowRateMinAvail)
              FlowRemaining = MAX((FlowRemaining - PassiveFlowRate),0.0d0)
              Node(FirstNodeOnBranch)%MassFlowRate = PassiveFlowRate
            ELSE !Each Branch receives maximum flow and BYPASS must be used
              Node(FirstNodeOnBranch)%MassFlowRate = MIN(Node(FirstNodeOnBranch)%MassFlowRateMaxAvail, FlowRemaining)
              FlowRemaining = FlowRemaining - Node(FirstNodeOnBranch)%MassFlowRate
            END IF
            CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, SplitterBranchOut,   &
                                               Node(FirstNodeOnBranch)%MassFlowRate, FirstHVACIteration)
          ENDIF
        ENDDO
      ENDIF !totalMax <=0 and flow should be assigned to active branches
            !IF the passive branches take the remaining loop flow, return
      IF(FlowRemaining == 0.0d0)RETURN

  ! 3) Distribute remaining flow to the BYPASS
        DO OutletNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%TotalOutletNodes
          SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
          FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchOut)%NodeNumIn
          IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_Bypass) THEN
          Node(FirstNodeOnBranch)%MassFlowRate = MIN(FlowRemaining,Node(FirstNodeOnBranch)%MassFlowRateMaxAvail)
          CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, SplitterBranchOut,   &
                                             Node(FirstNodeOnBranch)%MassFlowRate, FirstHVACIteration)
          FlowRemaining = FlowRemaining - Node(FirstNodeOnBranch)%MassFlowRate
          END IF
        END DO
            !IF the bypass take the remaining loop flow, return
        IF(FlowRemaining == 0.0d0)RETURN

  ! 4) If PASSIVE branches and BYPASS are at max and there's still flow, distribute remaining flow to ACTIVE branches
      IF(NumActiveBranches > 0)THEN
        ActiveFlowRate = FlowRemaining / NumActiveBranches
        DO OutletNum = 1, NumSplitOutlets
          SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
          FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchOut)%NodeNumIn
          IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_Active .OR. &
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_SeriesActive) THEN
                  !check Remaining flow (should be correct!)
            ActiveFlowRate = MIN(ActiveFlowRate, FlowRemaining)
                  !set the flow rate to the MIN((MassFlowRate+AvtiveFlowRate), MaxAvail)
            StartingFlowRate = Node(FirstNodeOnBranch)%MassFlowRate
            Node(FirstNodeOnBranch)%MassFlowRate = MIN((Node(FirstNodeOnBranch)%MassFlowRate + ActiveFlowRate), &
                                                        Node(FirstNodeOnBranch)%MassFlowRateMaxAvail)
            CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, SplitterBranchOut,   &
                                               Node(FirstNodeOnBranch)%MassFlowRate, FirstHVACIteration)
                  !adjust the remaining flow
            FlowRemaining = FlowRemaining - (Node(FirstNodeOnBranch)%MassFlowRate - StartingFlowRate)
          ENDIF
          IF(FlowRemaining == 0)EXIT
        END DO
            !IF the active branches take the remaining loop flow, return
        IF(FlowRemaining == 0.0d0)RETURN


  ! 5)  Step 4) could have left ACTIVE branches < MaxAvail.  Check to makes sure all ACTIVE branches are at MaxAvail
        DO OutletNum = 1, NumSplitOutlets
          SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
          FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchOut)%NodeNumIn
          IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_Active .OR. &
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_SeriesActive) THEN
            StartingFlowRate = Node(FirstNodeOnBranch)%MassFlowRate
            ActiveFlowRate = MIN(FlowRemaining, (Node(FirstNodeOnBranch)%MassFlowRateMaxAvail-StartingFlowRate))
            FlowRemaining = FlowRemaining - ActiveFlowRate
            Node(FirstNodeOnBranch)%MassFlowRate = StartingFlowRate + ActiveFlowRate
            CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, SplitterBranchOut,   &
                                               Node(FirstNodeOnBranch)%MassFlowRate, FirstHVACIteration)
          ENDIF
        END DO
      ENDIF
          !IF the active branches take the remaining loop flow, return
      IF(FlowRemaining == 0.0d0)RETURN

  ! 6) Adjust Inlet branch and outlet branch flow rates to match parallel branch rate
 !DSU? do we need this logic?   or should we fatal on a diagnostic error
      TotParallelBranchFlowReq =0.0d0
      DO iBranch = 1, NumSplitOutlets
        BranchNum        = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(iBranch)
        FirstNodeOnBranch= PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(branchNum)%NodeNumIn
            !calculate parallel branch flow rate
        TotParallelBranchFlowReq    = TotParallelBranchFlowReq + Node(FirstNodeOnBranch)%MassFlowRate
      END DO
            ! Reset the flow on the splitter inlet branch
      SplitterBranchIn      = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumIn
      FirstNodeOnBranchIn   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchIn)%NodeNumIn
      Node(FirstNodeOnBranchIn)%MassFlowRate = TotParallelBranchFlowReq
      CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, SplitterBranchIn,   &
                                         Node(FirstNodeOnBranchIn)%MassFlowRate, FirstHVACIteration)
            ! Reset the flow on the Mixer outlet branch
      MixerBranchOut        = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(SplitNum)%BranchNumOut
      FirstNodeOnBranchOut  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(MixerBranchOut)%NodeNumIn
      Node(FirstNodeOnBranchOut)%MassFlowRate = TotParallelBranchFlowReq
      CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, MixerBranchOut,   &
                                         Node(FirstNodeOnBranchOut)%MassFlowRate, FirstHVACIteration)
      RETURN

!IF INSUFFICIENT FLOW TO MEET ALL PARALLEL BRANCH FLOW REQUESTS
    ELSE IF(FlowRemaining < TotParallelBranchFlowReq) THEN

!DSU? didn't take the time to figure out what this should be... SplitterFlowIn = SplitterInletFlow(SplitNum)
    ! 1) apportion flow based on requested fraction of total
      DO OutletNum = 1, NumSplitOutlets
        SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
        ThisBranchRequest = DetermineBranchFlowRequest(LoopNum, LoopSideNum, SplitterBranchOut)
        FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchOut)%NodeNumIn
        IF ((PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_Active)  .OR. &
            (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(SplitterBranchOut)%ControlType == ControlType_SeriesActive) ) THEN
          ThisBranchRequestFrac = ThisBranchRequest / TotParallelBranchFlowReq
      !    FracFlow = Node(FirstNodeOnBranch)%MassFlowRate/TotParallelBranchFlowReq
      !    Node(FirstNodeOnBranch)%MassFlowRate = MIN((FracFlow * Node(FirstNodeOnBranch)%MassFlowRate),FlowRemaining)
          Node(FirstNodeOnBranch)%MassFlowRate = ThisBranchRequestFrac * ThisLoopSideFlow
          CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, SplitterBranchOut,   &
                                             Node(FirstNodeOnBranch)%MassFlowRate, FirstHVACIteration)
          FlowRemaining = FlowRemaining - Node(FirstNodeOnBranch)%MassFlowRate
        END IF
      END DO

    ! 1b) check if flow all apportioned
      IF(FlowRemaining > MassFlowTolerance)THEN
          !Call fatal diagnostic error. !The math should work out!
        CALL ShowSevereError('ResolveParallelFlows: Dev note, failed to redistribute restricted flow')
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Loop side flow = '//TRIM(RoundSigDigits(ThisLoopSideFlow,8))//' (kg/s)' )
        CALL ShowContinueError('Flow Remaining = '//TRIM(RoundSigDigits(FlowRemaining,8))//' (kg/s)' )
        CALL ShowContinueError('Parallel Branch requests  = '//TRIM(RoundSigDigits(TotParallelBranchFlowReq,8))//' (kg/s)' )
      ENDIF

    ! 2)  ! Reset the flow on the Mixer outlet branch
      MixerBranchOut        = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(SplitNum)%BranchNumOut
      FirstNodeOnBranchOut  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(MixerBranchOut)%NodeNumIn
      Node(FirstNodeOnBranchOut)%MassFlowRate = TotParallelBranchFlowReq
      CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, MixerBranchOut,   &
                                         Node(FirstNodeOnBranchOut)%MassFlowRate, FirstHVACIteration)

    END IF ! Total flow requested >= or < Total parallel request

  END IF ! Spittler/Mixer exists

  RETURN

END SUBROUTINE ResolveParallelFlows
!==================================================================!
!==================================================================!
!==================================================================!

SUBROUTINE PropagateResolvedFlow(LoopNum, LoopSideNum, FirstHVACIteration)

  USE DataLoopNode, ONLY: Node
  USE DataPlant,    ONLY: PlantLoop

  INTEGER, INTENT(IN) :: LoopNum
  INTEGER, INTENT(IN) :: LoopSideNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration

  INTEGER, PARAMETER :: SplitNum = 1

  INTEGER :: OutletNum
  INTEGER :: NumSplitOutlets
  INTEGER :: SplitterBranchOut
  INTEGER :: FirstNodeOnBranch


  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SplitterExists .AND. &
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%MixerExists) THEN

    NumSplitOutlets = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%TotalOutletNodes
    DO OutletNum = 1, NumSplitOutlets
      SplitterBranchOut = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
      FirstNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(SplitterBranchOut)%NodeNumIn
      CALL PushBranchFlowCharacteristics(LoopNum, LoopSideNum, SplitterBranchOut,   &
                                         Node(FirstNodeOnBranch)%MassFlowRate, FirstHVACIteration)
    END DO

  END IF

END SUBROUTINE


!==================================================================!
!================= EVALUATING BRANCH REQUEST ======================!
!==================================================================!
REAL(r64) FUNCTION DetermineBranchFlowRequest(LoopNum, LoopSideNum, BranchNum) RESULT(OverallFlowRequest)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine will analyze the given branch and determine the representative
          !  flow request.

          ! METHODOLOGY EMPLOYED:
          ! Several possibilities are available.  In any case, the request is constrained to within
          !  branch outlet min/max avail.  This assumes that the component flow routines will properly
          !  propogate the min/max avail down the branch.
          ! Some possibilities for flow request are:
          !  1) take the outlet flow rate -- assumes that the last component wins
          !  2) take the inlet flow rate request -- assumes that the request is propogated up and is good
          !  3) take the maximum request
          !  4) move down the loop and take the maximum "non-load-range-based" request within min/max avail bounds
          !     This assumes that load range based should not request flow for load-rejection purposes, and we
          !     should only "respond" to other component types.

          ! USE STATEMENTS:
  USE DataPlant,      ONLY: PlantLoop, LoadRangeBasedMin, LoadRangeBasedMax
  USE DataLoopNode,   ONLY: Node
  USE PlantUtilities, ONLY: BoundValueToNodeMinMaxAvail
  USE DataBranchAirLoopPlant, ONLY : ControlType_SeriesActive

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: LoopNum
  INTEGER, INTENT(IN) :: LoopSideNum
  INTEGER, INTENT(IN) :: BranchNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: OutletFlowRate   = 1
  INTEGER, PARAMETER :: InletFlowRequest = 2
  INTEGER, PARAMETER :: MaximumRequest   = 3
  INTEGER, PARAMETER :: MaxNonLRBRequest = 4
  INTEGER, PARAMETER :: WhichRequestCalculation = InletFlowRequest

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CompCounter
  INTEGER :: CompInletNode
  INTEGER :: BranchOutletNodeNUm
  INTEGER :: BranchInletNodeNum

  !~ Initialize
  BranchInletNodeNum  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn
  BranchOutletNodeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumOut
  OverallFlowRequest  = 0.0d0

  SELECT CASE (WhichRequestCalculation)

    CASE(OutletFlowRate)
      OverallFlowRequest = Node(BranchOutletNodeNum)%MassFlowRate

    CASE(InletFlowRequest)
      IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%ControlType /= ControlType_SeriesActive) THEN
        OverallFlowRequest = Node(BranchInletNodeNum)%MassFlowRateRequest
      ELSE ! is series active, so take largest request of all the component inlet nodes
        DO CompCounter = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
          CompInletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%NodeNumIn
          OverallFlowRequest = MAX(OverallFlowRequest, Node(CompInletNode)%MassFlowRateRequest)
        END DO
      ENDIF

    CASE(MaximumRequest)
      ! Assumes component inlet node is where request is held...could bandaid to include outlet node, but trying not to...
      DO CompCounter = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
        CompInletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%NodeNumIn
        OverallFlowRequest = MAX(OverallFlowRequest, Node(CompInletNode)%MassFLowRateRequest)
      END DO

    CASE(MaxNonLRBRequest)
      ! Assumes component inlet node is where request is held...could bandaid to include outlet node, but trying not to...
      DO CompCounter = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
        SELECT CASE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%CurOpSchemeType)
          CASE (LoadRangeBasedMin:LoadRangeBasedMax)
            ! don't include this request
          CASE DEFAULT
            ! include this
            CompInletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%NodeNumIn
            OverallFlowRequest = MAX(OverallFlowRequest, Node(CompInletNode)%MassFLowRateRequest)
        END SELECT
      END DO

  END SELECT

  !~ Now use a worker to bound the value to outlet min/max avail
  OverallFlowRequest = BoundValueToNodeMinMaxAvail(OverallFlowRequest, BranchOutletNodeNum)

 RETURN

END FUNCTION DetermineBranchFlowRequest
!==================================================================!
!==================================================================!
!==================================================================!


SUBROUTINE PushBranchFlowCharacteristics(LoopNum, LoopSideNum, BranchNum, ValueToPush, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine takes the flow resolved flow rate and pushes it
          !  down a branch.  In the process, if an externally connected
          !  component (air-water coil for example) is found to have a
          !  differing flow rate, the air sim flag is tripped to true, but
          !  the flow resolved flow rate is pushed down the loop to allow
          !  the plant to finish successfully.

          ! METHODOLOGY EMPLOYED:
          ! Push mass flow rate and max avail down each branch.  If the component
          !  is connected (or could be, for now) to an external loop such as
          !  an air loop, the current component outlet mass flow is checked
          !  vs the current resolved mass flow.  If the mass flow doesn't match,
          !  the air sim flag is tripped to true.

          ! Currently this routine is only performed for starved branches, when
          !  the coil is requesting too much flow, more than the plant can provide.
          ! If this were moved to every call type, including a minimum plant flow,
          !  you would need to provide a mass flow and min/max avail to push
          !  down the branch as well.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant    ! Use the entire module to allow all TypeOf's, would be a huge ONLY list
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE DataLoopNode, ONLY: Node
  USE PlantUtilities, ONLY: CheckPlantConvergence

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: LoopNum
  INTEGER,   INTENT(IN) :: LoopSideNum
  INTEGER,   INTENT(IN) :: BranchNum
  REAL(r64), INTENT(IN) :: ValueToPush
  LOGICAL, INTENT(IN)     :: FirstHVACIteration   ! TRUE if First HVAC iteration of Time step


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CompCounter
  INTEGER   :: BranchInletNode
  INTEGER   :: BranchOutletNode
  INTEGER   :: ComponentInletNode
  INTEGER   :: ComponentOutletNode
  INTEGER   :: ComponentTypeOfNum
  REAL(r64) :: MassFlowRateFound
  REAL(r64) :: MassFlow
  LOGICAL   :: PlantIsRigid
  !REAL(r64) :: MinAvail
 ! REAL(r64) :: MaxAvail

  BranchInletNode  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn
  BranchOutletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumOut

  !~ Possible error handling if needed
  IF (ValueToPush .NE. Node(BranchInletNode)%MassFlowRate) THEN
    ! Diagnostic problem, flow resolver isn't calling this routine properly
  END IF

  !~ This section would really be useful more later on if this routine has more logic regarding what to push down the branch
  MassFlow = ValueToPush
  !MinAvail = ValueToPush
 ! MaxAvail = ValueToPush

  PlantIsRigid = CheckPlantConvergence(LoopNum, LoopSideNum, FirstHVACIteration)

  !~ Loop across all component outlet nodes and update their mass flow and max avail
  DO CompCounter = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents

    !~ Pick up some values for convenience
    ComponentInletNode  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%NodeNumIn
    ComponentOutletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%NodeNumOut
    MassFlowRateFound   = Node(ComponentOutletNode)%MassFlowRate
    ComponentTypeOfNum  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompCounter)%TypeOf_Num

    !~ Push the values through
    Node(ComponentOutletNode)%MassFlowRate         = MassFlow

    IF (PlantIsRigid) THEN
      Node(ComponentInletNode)%MassFlowRateMinAvail = MassFlow
      Node(ComponentInletNode)%MassFlowRateMaxAvail = MassFlow
      Node(ComponentOutletNode)%MassFlowRateMinAvail = MassFlow
      Node(ComponentOutletNode)%MassFlowRateMaxAvail = MassFlow
    END IF
    !Node(ComponentOutletNode)%MassFlowRateMinAvail = MinAvail
   ! no this is 2-way valve which messes up flow options
    !      for demand components Node(ComponentOutletNode)%MassFlowRateMaxAvail = MaxAvail

    !~ If this value matches then we are good to move to the next component
    IF (ABS(MassFlow - MassFlowRateFound) < CriteriaDelta_MassFlowRate) CYCLE
    !~ Since there is a difference, we have to decide what to do based on the component type:
    !~  For plant connections, don't do anything, it SHOULD work itself out
    !~  For air connections, trip the loopside air flag
    !~  Similar for zone, none zone, and electric load center
    SELECT CASE (ComponentTypeOfNum)

      ! possibly air-connected components
      CASE ( TypeOf_CoilWaterCooling,             &
             TypeOf_CoilWaterDetailedFlatCooling, &
             TypeOf_CoilWaterSimpleHeating,       &
             TypeOf_CoilSteamAirHeating,          &
             TypeOf_CoilWAHPHeatingEquationFit,   &
             TypeOf_CoilWAHPCoolingEquationFit,   &
             TypeOf_CoilWAHPHeatingParamEst,      &
             TypeOf_CoilWAHPCoolingParamEst,      &
             TypeOf_CoilUserDefined ,             &
             TypeOf_CoilVSWAHPCoolingEquationFit, &
             TypeOf_CoilVSWAHPHeatingEquationFit, &
             TypeOf_PackagedTESCoolingCoil)

        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SimAirLoopsNeeded = .TRUE.
        !sometimes these coils are children in ZoneHVAC equipment
       ! PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SimZoneEquipNeeded= .TRUE.

      CASE(  TypeOf_BASEBOARD_CONV_WATER,         & !zone connected components
             TypeOf_BASEBOARD_RAD_CONV_STEAM,     &
             TypeOf_BASEBOARD_RAD_CONV_WATER,     &
             TypeOf_LowTempRadiant_VarFlow,       &
             TypeOf_LowTempRadiant_ConstFlow,     &
             TypeOf_CooledBeamAirTerminal,        &
             TypeOf_ZoneHVACAirUserDefined,       &
             TypeOf_AirTerminalUserDefined)

        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SimZoneEquipNeeded= .TRUE.

      CASE(  TypeOf_Generator_FCExhaust,          & !electric center connected components
             TypeOf_Generator_FCStackCooler,      &
             TypeOf_Generator_MicroCHP,           &
             TypeOf_Generator_MicroTurbine,       &
             TypeOf_Generator_ICEngine,           &
             TypeOf_Generator_CTurbine)

        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SimElectLoadCentrNeeded = .TRUE.

     END SELECT

  END DO


 RETURN

END SUBROUTINE PushBranchFlowCharacteristics

!==================================================================!
!================== REPORT VARIABLE UPDATE ========================!
!==================================================================!
SUBROUTINE UpdateLoopSideReportVars(LoopNum, LoopSide, OtherSideDemand, LocalRemLoopDemand)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 1998
          !       MODIFIED       Aug 2010 Edwin Lee -- add per loopside variable support
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! Update the report variables


          ! USE STATEMENTS:
  USE DataPlant,    ONLY: PlantLoop, PlantReport, SupplySide
  USE DataLoopNode, ONLY: Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(IN)  :: LoopNum
  INTEGER , INTENT(IN)  :: LoopSide
  REAL(r64), INTENT(IN) :: OtherSideDemand ! This is the 'other side' demand, based on other side flow
                                           ! and delta T (inlet to SetPt)
                                         ! This is evaluated once at the beginning of the loop side solver, before
                                         !  any of this side equipment alters it
  REAL(r64), INTENT(IN) :: LocalRemLoopDemand ! Unmet Demand after equipment has been simulated (report variable)


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  ! store node data in plant
  IF (LoopSide == SupplySide) THEN
    PlantReport(LoopNum)%InletNodeFlowrate     = Node(PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn)%MassFlowRate
    PlantReport(LoopNum)%InletNodeTemperature  = Node(PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn)%Temp
    PlantReport(LoopNum)%OutletNodeFlowrate    = Node(PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut)%MassFlowRate
    PlantReport(LoopNum)%OutletNodeTemperature = Node(PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut)%Temp

 ! In the baseline code, only reported supply side demand. so putting in "SupplySide" IF block for now but might expand later
    IF (OtherSideDemand < 0.0d0 ) THEN
      PlantReport(LoopNum)%CoolingDemand  = ABS(OtherSideDemand)
      PlantReport(LoopNum)%HeatingDemand  = 0.0d0
      PlantReport(LoopNum)%DemandNotDispatched    = -LocalRemLoopDemand  !  Setting sign based on old logic for now
    ELSE
      PlantReport(LoopNum)%HeatingDemand  = OtherSideDemand
      PlantReport(LoopNum)%CoolingDemand  = 0.0d0
      PlantReport(LoopNum)%DemandNotDispatched    = LocalRemLoopDemand  !  Setting sign based on old logic for now
    END IF

    CALL CalcUnmetPlantDemand(LoopNum, LoopSide)

  ENDIF

  RETURN

END SUBROUTINE UpdateLoopSideReportVars

SUBROUTINE CalcUnmetPlantDemand(LoopNum, LoopSideNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   June 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! determine the magnitude of unmet plant loads after the half loop simulation is done

          ! METHODOLOGY EMPLOYED:
          ! using the loop setpoint node, look at target vs current and
          ! calculate a demand based on mass flow times specific heat times delta T

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataPlant,    ONLY : PlantLoop, PlantReport, LoopDemandTol, SingleSetPoint, DualSetPointDeadBand
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance
  USE DataLoopNode, ONLY : Node, NodeType_Water, NodeType_Steam
  USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetSatEnthalpyRefrig
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(IN)  :: LoopNum
  INTEGER , INTENT(IN)  :: LoopSideNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER              :: BranchCounter      !~ This contains the index for the %Branch(:) structure
!  INTEGER              :: BranchIndex        !~ This is a 1 - n value within the current branch group
!  INTEGER              :: StartingComponent  !~ The component which "would" be simulated next

  !~ General variables
!  REAL(r64)                 :: EnteringTemperature
  REAL(r64)                 :: MassFlowRate
!  REAL(r64)                 :: SumMdotTimesTemp
!  REAL(r64)                 :: SumMdot
  REAL(r64)                 :: TargetTemp
  REAL(r64)                 :: LoopSetPointTemperature
  REAL(r64)                 :: LoopSetPointTemperatureHi
  REAL(r64)                 :: LoopSetPointTemperatureLo
  REAL(r64)                 :: LoadtoHeatingSetPoint
  REAL(r64)                 :: LoadtoCoolingSetPoint
  REAL(r64)                 :: DeltaTemp
!  INTEGER                   :: EnteringNodeNum
  REAL(r64)                 :: Cp
  REAL(r64)                 :: EnthalpySteamSatVapor  ! Enthalpy of saturated vapor
  REAL(r64)                 :: EnthalpySteamSatLiquid ! Enthalpy of saturated liquid
  REAL(r64)                 :: LatentHeatSteam        ! Latent heat of steam
  REAL(r64)                 :: LoadToLoopSetPoint

  ! Initialize
  LoadToLoopSetPoint = 0.0d0

  ! Get temperature at loop setpoint node.
  TargetTemp   = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%Temp
  MassFlowRate = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%MassFlowRate

  IF (PlantLoop(LoopNum)%FluidType==NodeType_Water) THEN

    Cp = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName, TargetTemp, &
                               PlantLoop(LoopNum)%FluidIndex, 'PlantLoopSolver::EvaluateLoopSetPointLoad')

    SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)

      CASE (SingleSetPoint)

        ! Pick up the loop setpoint temperature
        LoopSetPointTemperature = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetPoint
        ! Calculate the delta temperature
        DeltaTemp    = LoopSetPointTemperature - TargetTemp

        ! Calculate the demand on the loop
        LoadToLoopSetPoint = MassFlowRate * Cp * DeltaTemp

      CASE (DualSetPointDeadBand)

        ! Get the range of setpoints
        LoopSetPointTemperatureHi = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetpointHi
        LoopSetPointTemperatureLo = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetpointLo

        !Calculate the demand on the loop
        IF (MassFlowRate > 0.0d0) THEN
          LoadtoHeatingSetPoint = MassFlowRate*Cp*(LoopSetPointTemperatureLo - TargetTemp)
          LoadtoCoolingSetPoint = MassFlowRate*Cp*(LoopSetPointTemperatureHi - TargetTemp)
          ! Possible combinations:
          ! 1  LoadToHeatingSetPoint > 0 & LoadToCoolingSetPoint > 0 -->  Heating required
          ! 2  LoadToHeatingSetPoint < 0 & LoadToCoolingSetPoint < 0 -->  Cooling Required
          ! 3  LoadToHeatingSetPoint <=0 & LoadToCoolingSetPoint >=0 -->  Dead Band Operation - includes zero load cases
          ! 4  LoadToHeatingSetPoint  >  LoadToCoolingSetPoint       -->  Not Feasible if LoopSetPointHi >= LoopSetPointLo
          IF (LoadToHeatingSetPoint .GT. 0.0d0 .AND. LoadToCoolingSetPoint .GT. 0.0d0) THEN
            LoadToLoopSetPoint = LoadToHeatingSetPoint
          ELSE IF (LoadToHeatingSetPoint .LT. 0.0d0 .AND. LoadToCoolingSetPoint .LT. 0.0d0) THEN
            LoadToLoopSetPoint = LoadToCoolingSetPoint
          ELSE IF (LoadToHeatingSetPoint .LE. 0.0d0 .AND. LoadToCoolingSetPoint .GE. 0.0d0) THEN ! deadband includes zero loads
            LoadToLoopSetPoint = 0.0d0
          END IF
        ELSE
          LoadToLoopSetPoint = 0.0d0
        END IF

      END SELECT


    ELSEIF (PlantLoop(LoopNum)%FluidType==NodeType_Steam) THEN

      Cp = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName, TargetTemp, &
                                 PlantLoop(LoopNum)%FluidIndex, 'PlantLoopSolver::EvaluateLoopSetPointLoad')

      SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)

      CASE (SingleSetPoint)

        ! Pick up the loop setpoint temperature
        LoopSetPointTemperature = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempSetPoint

        ! Calculate the delta temperature
        DeltaTemp    = LoopSetPointTemperature - TargetTemp

        EnthalpySteamSatVapor  =  GetSatEnthalpyRefrig('STEAM',LoopSetPointTemperature,1.0d0,RefrigIndex, &
                                               'PlantSupplySide:EvaluateLoopSetPointLoad')
        EnthalpySteamSatLiquid =  GetSatEnthalpyRefrig('STEAM',LoopSetPointTemperature,0.0d0,RefrigIndex, &
                                               'PlantSupplySide:EvaluateLoopSetPointLoad')

        LatentHeatSteam = EnthalpySteamSatVapor - EnthalpySteamSatLiquid

        ! Calculate the demand on the loop
        LoadToLoopSetPoint = MassFlowRate * ( Cp * DeltaTemp + LatentHeatSteam )


      END SELECT

    ELSE  ! only have two types, water serves for glycol.


    END IF

    ! Trim the demand to zero if it is very small
    IF(ABS(LoadToLoopSetPoint) < LoopDemandTol) LoadToLoopSetPoint = 0.0d0

    PlantReport(LoopNum)%UnmetDemand = LoadToLoopSetPoint

  RETURN

END SUBROUTINE CalcUnmetPlantDemand


!==================================================================!
!==================================================================!
!==================================================================!


!==================================================================!
!================ VERIFYING LOOP EXIT NODE STATE ==================!
!==================================================================!
SUBROUTINE  CheckLoopExitNode(LoopNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   October 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets the temperature
          ! and mass flow rate of the plant loop supply side exit
          ! node.  As written, the routine calculates the exit
          ! temperature based on the fraction of loop demand met
          ! by the plant equipment.  This assumes that each piece
          ! of operating plant equipment produced chilled/hot water
          ! at the loop setpoint temperature.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: WarmUpFlag, BeginEnvrnFlag
  USE DataInterfaces,  ONLY: ShowWarningError,ShowContinueError,ShowContinueErrorTimeStamp,  &
                             ShowRecurringWarningErrorAtEnd
  USE DataPlant,       ONLY: PlantLoop, SupplySide,DemandSide
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE DataLoopNode,    ONLY: Node, NodeID
  USE General,         ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER ,INTENT(IN) :: LoopNum             !plant loop counter
  LOGICAL, INTENT(IN)     :: FirstHVACIteration   ! TRUE if First HVAC iteration of Time step

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: LoopInlet           !plant loop inlet node num.
  INTEGER             :: LoopOutlet          !plant loop outlet node num.

          !set local variables: loop inlet and outlet nodes
    LoopInlet = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn
    LoopOutlet = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut
          !Check continuity invalid...loop pumps now turned on and off
   If(.not.FirstHvacIteration .and. .not.WarmUpFlag)Then
    IF (ABS(Node(LoopOutlet)%MassFlowRate-Node(LoopInlet)%MassFlowRate) > MassFlowTolerance) THEN
      IF (PlantLoop(LoopNum)%MFErrIndex == 0) THEN
        CALL ShowWarningError ('PlantSupplySide: PlantLoop="'//TRIM(PlantLoop(LoopNum)%Name)//  &
           '", Error (CheckLoopExitNode) -- Mass Flow Rate Calculation. '//  &
           'Outlet and Inlet differ by more than tolerance.')
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Loop inlet node='//TRIM(NodeID(LoopInlet))//', flowrate='//  &
                 TRIM(RoundSigDigits(Node(LoopInlet)%MassFlowRate,4))//' kg/s')
        CALL ShowContinueError('Loop outlet node='//TRIM(NodeID(LoopOutlet))//', flowrate='//  &
                 TRIM(RoundSigDigits(Node(LoopOutlet)%MassFlowRate,4))//' kg/s')
        CALL ShowContinueError('This loop might be helped by a bypass.')
      END IF
      CALL ShowRecurringWarningErrorAtEnd('PlantSupplySide: PlantLoop="'//TRIM(PlantLoop(LoopNum)%Name)//  &
         '", Error -- Mass Flow Rate Calculation -- continues ** ',PlantLoop(LoopNum)%MFErrIndex)
    END IF
   END IF
          !Reset Max loop flow rate based on pump performance
    Node(LoopOutlet)%MassFlowRateMax = Node(LoopInlet)%MassFlowRateMax


  RETURN
END SUBROUTINE CheckLoopExitNode



SUBROUTINE AdjustPumpFlowRequestByEMSControls(LoopNum, LoopSideNum, BranchNum, CompNum, FlowToRequest)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! modify flow request to pump simulation if EMS is overriding pump component

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPLant, ONLY : PlantLoop

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
 INTEGER,INTENT(IN)         :: LoopNum
 INTEGER,INTENT(IN)         :: LoopSideNum
 INTEGER,INTENT(IN)         :: BranchNum
 INTEGER,INTENT(IN)         :: CompNum
 REAL(r64), INTENT(INOUT)   :: FlowToRequest

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  IF ((PlantLoop(LoopNum)%LoopSide(LoopSideNum)%EMSCtrl) .AND. &
      (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%EMSValue <= 0.d0)) THEN
    FlowToRequest = 0.d0
    RETURN
  ENDIF

  IF ((PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%EMSCtrlOverrideOn) .AND. &
      (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%EMSCtrlOverrideValue <= 0.d0)) THEN
    FlowToRequest = 0.d0
    RETURN
  ENDIF

  IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideOn) THEN
    IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideValue == 0.d0) THEN
      FlowToRequest = 0.d0
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE AdjustPumpFlowRequestByEMSControls
!==================================================================!
!==================================================================!
!==================================================================!


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
END MODULE PlantLoopSolver
