MODULE PlantUtilities

          ! Module containing the routines dealing with the <module_name>

          ! MODULE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! <use statements for data only modules>
          ! <use statements for access to subroutines in other modules>
USE DataPrecisionGlobals
USE DataInterfaces

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Everything public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

CONTAINS


SUBROUTINE InitComponentNodes(MinCompMdot,MaxCompMdot, InletNode,OutletNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Sept 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Central routine for initializing plant nodes connected to components
          !  typically used for BeginEnvrnFlag

          ! METHODOLOGY EMPLOYED:
          ! set massflowrate variables on inlet node
          !  reset inlet node if more restrictive

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode,         ONLY : Node, NodeID
  USE DataPlant,            ONLY : PlantLoop,  DemandOpSchemeType

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)    :: MinCompMdot !
  REAL(r64), INTENT(IN)    :: MaxCompMdot !
  INTEGER,   INTENT(IN)    :: InletNode  ! component's inlet node index in node structure
  INTEGER,   INTENT(IN)    :: OutletNode ! component's outlet node index in node structure
  INTEGER,   INTENT(IN)    :: LoopNum  ! plant loop index for PlantLoop structure
  INTEGER,   INTENT(IN)    :: LoopSideNum ! Loop side index for PlantLoop structure
  INTEGER,   INTENT(IN)    :: BranchIndex ! branch index for PlantLoop
  INTEGER,   INTENT(IN)    :: CompIndex  ! component index for PlantLoop
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: tmpMinCompMdot  ! local value
  REAL(r64)  :: tmpMaxCompMdot  ! local value

  tmpMinCompMdot = MinCompMdot
  tmpMaxCompMdot = MaxCompMdot
  ! trap bad values that can happen before all the setup is done
  IF (tmpMinCompMdot < 0.d0) tmpMinCompMdot = 0.d0
  IF (tmpMaxCompMdot < 0.d0) tmpMaxCompMdot = 0.d0


  ! reset outlet node
  Node(OutletNode)%MassFlowRate         = 0.d0
!  Node(OutletNode)%MassFlowRateMin      = MinCompMdot
!  Node(OutletNode)%MassFlowRateMinAvail = MinCompMdot
!  Node(OutletNode)%MassFlowRateMax      = MaxCompMdot
!  Node(OutletNode)%MassFlowRateMaxAvail = MaxCompMdot

  Node(InletNode)%MassFlowRateMin      = tmpMinCompMdot
  Node(InletNode)%MassFlowRateMinAvail = tmpMinCompMdot
  Node(InletNode)%MassFlowRateMax      = tmpMaxCompMdot
  Node(InletNode)%MassFlowRateMaxAvail = tmpMaxCompMdot
  !reset inlet node, but only change from inlet setting if set and more restrictive
  Node(InletNode)%MassFlowRate          = 0.d0
  Node(InletNode)%MassFlowRateRequest   = 0.d0
!  IF (Node(InletNode)%MassFlowRateMax > 0.d0) THEN !if inlet has been set, only change it if more restrictive
!    Node(InletNode)%MassFlowRateMax       = MIN(tmpMaxCompMdot, Node(InletNode)%MassFlowRateMax)
!  ELSE
!    Node(InletNode)%MassFlowRateMax       = tmpMaxCompMdot
!  ENDIF
!  IF (Node(InletNode)%MassFlowRateMaxAvail> 0.d0) THEN !if inlet has been set, only change it if more restrictive
!    Node(InletNode)%MassFlowRateMaxAvail  = MIN(tmpMaxCompMdot, Node(InletNode)%MassFlowRateMaxAvail)
!  ELSE
!    Node(InletNode)%MassFlowRateMaxAvail  = tmpMaxCompMdot
!  ENDIF
!  IF (Node(InletNode)%MassFlowRateMin > 0.d0) THEN
!    Node(InletNode)%MassFlowRateMin       = MAX(tmpMinCompMdot, Node(InletNode)%MassFlowRateMin)
!  ELSE
!    Node(InletNode)%MassFlowRateMin       = tmpMinCompMdot
!  ENDIF
!
!  IF (Node(InletNode)%MassFlowRateMinAvail > 0.d0) THEN
!    Node(InletNode)%MassFlowRateMinAvail  = MAX(tmpMinCompMdot, Node(InletNode)%MassFlowRateMinAvail)
!  ELSE
!    Node(InletNode)%MassFlowRateMinAvail  = tmpMinCompMdot
!  ENDIF


  RETURN

END SUBROUTINE InitComponentNodes


SUBROUTINE SetComponentFlowRate(CompFlow,InletNode,OutletNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! General purpose worker routine to set flows for a component model
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataLoopNode,         ONLY : Node, NodeID, NumOfNodes
  USE DataPlant,            ONLY : PlantLoop, DemandOpSchemeType, FlowUnlocked, &
                                   FlowLocked, PlantSizesOkayToFinalize
  USE DataBranchAirLoopPlant, ONLY : ControlType_SeriesActive, MassFlowTolerance
  USE DataInterfaces,       ONLY : ShowFatalError, ShowContinueError, ShowSevereError,ShowContinueErrorTimeStamp
  USE General,              ONLY : RoundSigDigits
  USE DataSizing,           ONLY : AutoSize
  USE DataGlobals,          ONLY : SysSizingCalc

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(INOUT) :: CompFlow  ![kg/s]
  INTEGER,   INTENT(IN)    :: LoopNum  ! plant loop index for PlantLoop structure
  INTEGER,   INTENT(IN)    :: LoopSideNum ! Loop side index for PlantLoop structure
  INTEGER,   INTENT(IN)    :: BranchIndex ! branch index for PlantLoop
  INTEGER,   INTENT(IN)    :: CompIndex  ! component index for PlantLoop
  INTEGER,   INTENT(IN)    :: InletNode  ! component's inlet node index in node structure
  INTEGER,   INTENT(IN)    :: OutletNode ! component's outlet node index in node structure

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: OneTimeDiagSetup = .TRUE.
  LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE :: NodeErrorMsgIssued
  LOGICAL, SAVE :: NullPlantErrorMsgIssued
  REAL(r64)     :: MdotOldRequest  ! initial value of mass flow
  INTEGER       :: CompInletNodeNum
  INTEGER       :: CompOutletNodeNum
  INTEGER       :: CompNum
  REAL(r64)     :: SeriesBranchHighFlowRequest ! local temporary used for sweeping across components on a branch
  REAL(r64)     :: SeriesBranchHardwareMaxLim
  REAL(r64)     :: SeriesBranchHardwareMinLim
  REAL(r64)     :: SeriesBranchMaxAvail
  REAL(r64)     :: SeriesBranchMinAvail

  IF (OneTimeDiagSetup) THEN
    ALLOCATE(NodeErrorMsgIssued(NumOfNodes) )
    NodeErrorMsgIssued = .FALSE.
    NullPlantErrorMsgIssued = .FALSE.
    OneTimeDiagSetup = .FALSE.
  ENDIF

  IF (LoopNum == 0) THEN ! protect from hard crash below
    IF (.NOT. NullPlantErrorMsgIssued ) THEN ! throw one dev error message
      IF (InletNode > 0) THEN
        CALL ShowSevereError('SetComponentFlowRate: trapped plant loop index = 0, check component with inlet node named=' &
                             //TRIM(NodeID(InletNode)) )
      ELSE
        CALL ShowSevereError('SetComponentFlowRate: trapped plant loop node id = 0')
      ENDIF
      NullPlantErrorMsgIssued = .TRUE.
    ENDIF
    RETURN
  ENDIF
          ! FLOW:

  MdotOldRequest = Node(InletNode)%MassFlowRateRequest

  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%CurOpSchemeType == DemandOpSchemeType) THEN
      ! store flow request on inlet node

    Node(InletNode)%MassFlowRateRequest   = CompFlow

    Node(OutletNode)%MassFlowRateMinAvail = MAX(Node(InletNode)%MassFlowRateMinAvail ,Node(InletNode)%MassFlowRateMin)
    !virtual 2-way valve (was tried but it clamps down demand side component's flow options so they can't find proper solutions)
    !  Node(OutletNode)%MassFlowRateMinAvail = MAX(Node(InletNode)%MassFlowRateMinAvail , CompFlow)

    Node(OutletNode)%MassFlowRateMaxAvail = MIN(Node(InletNode)%MassFlowRateMaxAvail,Node(InletNode)%MassFlowRateMax)
  !  Node(OutletNode)%MassFlowRateMaxAvail = MIN(Node(InletNode)%MassFlowRateMaxAvail , CompFlow)
  ELSE
   !DSU lodge the original request for all types
    Node(InletNode)%MassFlowRateRequest = CompFlow

  ENDIF

        !Update Min/Max Avail

  Node(OutletNode)%MassFlowRateMinAvail = MAX(Node(InletNode)%MassFlowRateMinAvail ,Node(InletNode)%MassFlowRateMin)
  IF (Node(InletNode)%MassFlowRateMax >= 0.d0) THEN
    Node(OutletNode)%MassFlowRateMaxAvail = MIN(Node(InletNode)%MassFlowRateMaxAvail,Node(InletNode)%MassFlowRateMax)
  ELSE

    IF (.NOT. SysSizingCalc .and. PlantSizesOkayToFinalize ) THEN
      ! throw error for developers, need to change a componennt model to set hardware limits on inlet
      If ( .NOT. NodeErrorMsgIssued(InletNode)) THEN

        CALL ShowSevereError('SetComponentFlowRate: check component model implementation for component with inlet node named=' &
                           //TRIM(NodeID(InletNode)) )
        CALL ShowContinueError('Inlet node MassFlowRatMax = '//TRIM(RoundSigDigits(Node(InletNode)%MassFlowRateMax,8)) )
        NodeErrorMsgIssued(InletNode) = .TRUE.
      ENDIF
    ENDIF
  ENDIF


        !Set loop flow rate
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Flowlock == FlowUnlocked)THEN
    IF (PlantLoop(LoopNum)%MaxVolFlowRate == AutoSize)THEN !still haven't sized the plant loop
      Node(OutletNode)%MassFlowRate = CompFlow
      Node(InletNode)%MassFlowRate = Node(OutletNode)%MassFlowRate
    ELSE !bound the flow by Min/Max available and hardware limits
      IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%FlowCtrl == ControlType_SeriesActive) THEN
        ! determine highest flow request for all the components on the branch
        SeriesBranchHighFlowRequest = 0.d0
        SeriesBranchHardwareMaxLim  = Node(InletNode)%MassFlowRateMax
        SeriesBranchHardwareMinLim  = 0.d0
        SeriesBranchMaxAvail        = Node(InletNode)%MassFlowRateMaxAvail
        SeriesBranchMinAvail        = 0.d0
        DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%TotalComponents
          CompInletNodeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompNum)%NodeNumIn
          SeriesBranchHighFlowRequest = MAX(Node(CompInletNodeNum)%MassFlowRateRequest, SeriesBranchHighFlowRequest)
          SeriesBranchHardwareMaxLim  = MIN(Node(CompInletNodeNum)%MassFlowRateMax, SeriesBranchHardwareMaxLim)
          SeriesBranchHardwareMinLim  = MAX(Node(CompInletNodeNum)%MassFlowRateMin, SeriesBranchHardwareMinLim)
          SeriesBranchMaxAvail        = MIN(Node(CompInletNodeNum)%MassFlowRateMaxAvail, SeriesBranchMaxAvail)
          SeriesBranchMinAvail        = MAX(Node(CompInletNodeNum)%MassFlowRateMinAvail, SeriesBranchMinAvail)
        ENDDO

        !take higher of branch max flow request and this new flow request
        CompFlow = MAX(CompFlow, SeriesBranchHighFlowRequest)

        ! apply constraints on component flow
        CompFlow = MAX(CompFlow, SeriesBranchHardwareMinLim)
        CompFlow = MAX(CompFlow, SeriesBranchMinAvail)
        CompFlow = MIN(CompFlow, SeriesBranchHardwareMaxLim)
        CompFlow = MIN(CompFlow, SeriesBranchMaxAvail)

        IF (CompFlow < MassFlowTolerance) CompFlow = 0.d0
        Node(OutletNode)%MassFlowRate = CompFlow
        Node(InletNode)%MassFlowRate = Node(OutletNode)%MassFlowRate
        DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%TotalComponents
          CompInletNodeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompNum)%NodeNumIn
          CompOutletNodeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompNum)%NodeNumOut
          Node(CompInletNodeNum)%MassFlowRate = Node(OutletNode)%MassFlowRate
          Node(CompOutletNodeNum)%MassFlowRate = Node(OutletNode)%MassFlowRate
        ENDDO

      ELSE ! not series active
        Node(OutletNode)%MassFlowRate = MAX(Node(OutletNode)%MassFlowRateMinAvail, CompFlow)
        Node(OutletNode)%MassFlowRate = MAX(Node(InletNode)%MassFlowRateMin,      Node(OutletNode)%MassFlowRate)
        Node(OutletNode)%MassFlowRate = MIN(Node(OutletNode)%MassFlowRateMaxAvail, Node(OutletNode)%MassFlowRate)
        Node(OutletNode)%MassFlowRate = MIN(Node(InletNode)%MassFlowRateMax,      Node(OutletNode)%MassFlowRate)
        IF (Node(OutletNode)%MassFlowRate < MassFlowTolerance) Node(OutletNode)%MassFlowRate = 0.d0
        CompFlow =  Node(OutletNode)%MassFlowRate
        Node(InletNode)%MassFlowRate = Node(OutletNode)%MassFlowRate
      ENDIF

    END IF
  ELSEIF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Flowlock == FlowLocked)THEN
    Node(OutletNode)%MassFlowRate = Node(InletNode)%MassFlowRate
    CompFlow =  Node(OutletNode)%MassFlowRate
!    IF (((CompFlow - Node(OutletNode)%MassFlowRateMaxAvail) > MassFlowTol) .OR. &
!        ((Node(OutletNode)%MassFlowRateMinAvail - CompFlow) > MassFlowTol)) THEN
!      IF ( .NOT. NodeErrorMsgIssued(InletNode)) THEN
!        CALL ShowSevereError('SetComponentFlowRate: Flow rate is out of range') !DEBUG error...should never get here
!        CALL ShowContinueErrorTimeStamp(' ')
!        CALL ShowContinueError('Component flow rate [kg/s] = '//TRIM(RoundSigDigits(CompFlow,8)) )
!        CALL ShowContinueError('Node maximum flow rate available [kg/s] = ' &
!                                 //TRIM(RoundSigDigits(Node(OutletNode)%MassFlowRateMaxAvail,8)) )
!        CALL ShowContinueError('Node minimum flow rate available [kg/s] = '&
!                                 //TRIM(RoundSigDigits(Node(OutletNode)%MassFlowRateMinAvail,8)) )
!        CALL ShowContinueError('Component named = ' &
!                             //TRIM(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%Name) )
!        NodeErrorMsgIssued(InletNode) = .TRUE.
!      ENDIF
!   !   CALL ShowFatalError('SetComponentFlowRate: out of range flow rate problem caused termination')
!    ENDIF
  ELSE
    CALL ShowFatalError('SetComponentFlowRate: Flow lock out of range') !DEBUG error...should never get here
  ENDIF


  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchIndex)%Comp(CompIndex)%CurOpSchemeType == DemandOpSchemeType) THEN
    IF ((MdotOldRequest > 0.d0) .AND. (CompFlow > 0.d0)) THEN ! sure that not coming back from a no flow reset
      IF (ABS(MdotOldRequest - Node(InletNode)%MassFlowRateRequest) > MassFlowTolerance) THEN !demand comp changed its flow request
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SimLoopSideNeeded = .TRUE.
      ENDIF
    ENDIF
  ENDIF

  RETURN
END SUBROUTINE SetComponentFlowRate             !DSU2

SUBROUTINE SetActuatedBranchFlowRate(CompFlow,ActuatedNode,LoopNum,LoopSideNum, BranchNum, ResetMode)    !DSU3

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! general purpse worker routine to set plant node variables for node
          ! and all nodes on the branch.  Used by HVAC water coil controller, that do not
          !  distinguish single component and have no inlet-outlet pair
          !  only a actuated noded of no clear position.  set flow on entire branch

          ! METHODOLOGY EMPLOYED:
          ! Set flow on node and branch while honoring constraints on actuated node

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataLoopNode,         ONLY : Node
  USE DataPlant,            ONLY : PlantLoop, FlowUnlocked, FlowLocked
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance
  USE DataInterfaces,       ONLY : ShowFatalError, ShowSevereError, ShowContinueErrorTimeStamp,&
                                   ShowContinueError
  USE DataSizing,           ONLY : AutoSize
  USE General,              ONLY : RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(INOUT) :: CompFlow
  INTEGER, INTENT(IN) :: ActuatedNode
  INTEGER, INTENT(IN) :: LoopNum
  INTEGER, INTENT(IN) :: LoopSideNum
  INTEGER, INTENT(IN) :: BranchNum
  LOGICAL, INTENT(IN) :: ResetMode  ! flag to indicate if this is a real flow set, or a reset flow setting.



          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: CompNum
  INTEGER  :: NodeNum
  REAL(r64) :: MdotOldRequest

          ! FLOW:
          ! store original flow
  MdotOldRequest = Node(ActuatedNode)%MassFlowRateRequest
  Node(ActuatedNode)%MassFlowRateRequest = CompFlow
  IF (LoopNum > 0 .AND. LoopSideNum > 0 .AND. (.NOT. ResetMode)) THEN
    IF ((MdotOldRequest > 0.d0) .AND. (CompFlow > 0.d0)) THEN ! sure that not coming back from a no flow reset
      IF ( (ABS(MdotOldRequest - Node(ActuatedNode)%MassFlowRateRequest) > MassFlowTolerance) .AND. &
           (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Flowlock == FlowUnlocked)) THEN
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SimLoopSideNeeded = .TRUE.
      ENDIF
    ENDIF
  ENDIF
          !Set loop flow rate

  IF (LoopNum > 0 .AND. LoopSideNum > 0 ) THEN
    IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Flowlock == FlowUnlocked)THEN
      IF (PlantLoop(LoopNum)%MaxVolFlowRate == AutoSize)THEN !still haven't sized the plant loop
        Node(ActuatedNode)%MassFlowRate = CompFlow
      ELSE !bound the flow by Min/Max available across entire branch



        Node(ActuatedNode)%MassFlowRate = MAX(Node(ActuatedNode)%MassFlowRateMinAvail, CompFlow)
        Node(ActuatedNode)%MassFlowRate = MAX(Node(ActuatedNode)%MassFlowRateMin , Node(ActuatedNode)%MassFlowRate )
        ! add MassFlowRateMin hardware constraints
        Node(ActuatedNode)%MassFlowRate = MIN(Node(ActuatedNode)%MassFlowRateMaxAvail, Node(ActuatedNode)%MassFlowRate)
        Node(ActuatedNode)%MassFlowRate = MIN(Node(ActuatedNode)%MassFlowRateMax, Node(ActuatedNode)%MassFlowRate)
        IF (Node(ActuatedNode)%MassFlowRate < MassFlowTolerance) Node(ActuatedNode)%MassFlowRate = 0.d0
        CompFlow  = Node(ActuatedNode)%MassFlowRate
        Do CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
          IF (ActuatedNode == PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn) THEN
!            ! found controller set to inlet of a component.  now set that component's outlet
            NodeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut
!            Node(ActuatedNode)%MassFlowRate = MAX( Node(ActuatedNode)%MassFlowRate , Node(NodeNum)%MassFlowRateMinAvail)
!            Node(ActuatedNode)%MassFlowRate = MAX( Node(ActuatedNode)%MassFlowRate , Node(ActuatedNode)%MassFlowRateMin)
!            Node(ActuatedNode)%MassFlowRate = MIN( Node(ActuatedNode)%MassFlowRate , Node(NodeNum)%MassFlowRateMaxAvail)
!            Node(ActuatedNode)%MassFlowRate = MIN( Node(ActuatedNode)%MassFlowRate , Node(ActuatedNode)%MassFlowRateMax)

            !virtual 2-way valve
       !     Node(NodeNum)%MassFlowRateMinAvail = MAX(Node(ActuatedNode)%MassFlowRateMinAvail ,Node(ActuatedNode)%MassFlowRateMin)
       !     Node(NodeNum)%MassFlowRateMinAvail = MAX(Node(ActuatedNode)%MassFlowRateMinAvail , CompFlow)
            Node(NodeNum)%MassFlowRateMinAvail = MAX(Node(ActuatedNode)%MassFlowRateMinAvail, Node(ActuatedNode)%MassFlowRateMin)
      !      Node(NodeNum)%MassFlowRateMaxAvail = MIN(Node(ActuatedNode)%MassFlowRateMaxAvail,Node(ActuatedNode)%MassFlowRateMax)
      !      Node(NodeNum)%MassFlowRateMaxAvail = MIN(Node(ActuatedNode)%MassFlowRateMaxAvail , CompFlow)
            Node(NodeNum)%MassFlowRateMaxAvail = MIN(Node(ActuatedNode)%MassFlowRateMaxAvail, Node(ActuatedNode)%MassFlowRateMax)
            Node(NodeNum)%MassFlowRate         = Node(ActuatedNode)%MassFlowRate

          ENDIF
        ENDDO
      END IF

    ELSEIF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Flowlock == FlowLocked)THEN

      CompFlow =  Node(ActuatedNode)%MassFlowRate
      ! do not change requested flow rate either
      Node(ActuatedNode)%MassFlowRateRequest = MdotOldRequest
      IF (((CompFlow - Node(ActuatedNode)%MassFlowRateMaxAvail) > MassFlowTolerance) .OR. &
           ((Node(ActuatedNode)%MassFlowRateMinAvail - CompFlow) > MassFlowTolerance)) THEN
        CALL ShowSevereError('SetActuatedBranchFlowRate: Flow rate is out of range') !DEBUG error...should never get here
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Component flow rate [kg/s] = '//TRIM(RoundSigDigits(CompFlow,8)) )
        CALL ShowContinueError('Node maximum flow rate available [kg/s] = ' &
                                 //TRIM(RoundSigDigits(Node(ActuatedNode)%MassFlowRateMaxAvail,8)) )
        CALL ShowContinueError('Node minimum flow rate available [kg/s] = '&
                                 //TRIM(RoundSigDigits(Node(ActuatedNode)%MassFlowRateMinAvail,8)) )
      ENDIF
    ELSE
      CALL ShowFatalError('SetActuatedBranchFlowRate: Flowlock out of range, value='//  &
         trim(RoundSigDigits(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Flowlock))) !DEBUG error...should never get here
    ENDIF



    Do CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
      NodeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
      Node(NodeNum)%MassFlowRate  = Node(ActuatedNode)%MassFlowRate
      Node(NodeNum)%MassFlowRateRequest = Node(ActuatedNode)%MassFlowRateRequest
      NodeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut
      Node(NodeNum)%MassFlowRate  = Node(ActuatedNode)%MassFlowRate
      Node(NodeNum)%MassFlowRateRequest = Node(ActuatedNode)%MassFlowRateRequest
    ENDDO

  ELSE
    ! early in simulation before plant loops are setup and found
    Node(ActuatedNode)%MassFlowRate = CompFlow
  ENDIF


  RETURN
END SUBROUTINE SetActuatedBranchFlowRate             !DSU3

REAL(r64) FUNCTION RegulateCondenserCompFlowReqOp(LoopNum, LoopSideNum, BranchNum, CompNum, TentativeFlowRequest) RESULT(FlowVal)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This functino will do some intelligent flow request logic for condenser equipment.
          ! Some condenser equipment (gruond heat exchangers, etc.) may not have a meaningful load value
          !  since this is an environment heat transfer component.
          ! The runflag is set, but may not be properly set, and the component may still request flow even
          !  when it doesn't need to.
          ! This function will do a little more advanced logic than just checking runflag to determine whether
          !  to request any flow

          ! METHODOLOGY EMPLOYED:
          ! Query run flag and myLoad
          ! If run flag is OFF, then the component should actually be OFF, and tentative flow request will be zeroed
          ! If the run flag is ON, then check the control type to determine if MyLoad is a meaningful value
          ! If it is meaningful then determine whether to do flow request based on myload
          ! If not then we will have no choice but to leave the flow request alone (uncontrolled operation?)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant, ONLY: PlantLoop, HeatingRBOpSchemeType, CoolingRBOpSchemeType, CompSetPtBasedSchemeType

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    INTEGER   :: LoopNum
    INTEGER   :: LoopSideNum
    INTEGER   :: BranchNum
    INTEGER   :: CompNum
    REAL(R64) :: TentativeFlowRequest

          ! FUNCTION PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: ZeroLoad = 0.0001d0

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: CompCurLoad
    LOGICAL   :: CompRunFlag
    INTEGER   :: CompOpScheme

    CompCurLoad = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad
    CompRunFlag = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON
    CompOpScheme = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType

    IF (CompRunFlag) THEN

        SELECT CASE (CompOpScheme)

        CASE (HeatingRBOpSchemeType, CoolingRBOpSchemeType, CompSetPtBasedSchemeType) ! These provide meaningful myload values
            IF (ABS(CompCurLoad) > ZeroLoad) THEN
                FlowVal = TentativeFlowRequest
            ELSE !no load
                FlowVal = 0.0d0
            END IF

        CASE DEFAULT ! Types that don't provide meaningful myload values
            FlowVal = TentativeFlowRequest

        END SELECT

    ELSE !runflag OFF

        FlowVal = 0.0d0

    END IF

  RETURN

END FUNCTION

SUBROUTINE UpdatePlantMixer(LoopNum,LoopSideNum,MixNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brandon Anderson, Dan Fisher
          !       DATE WRITTEN   October 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate the outlet conditions at the mixer

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataLoopNode,         ONLY : Node
  USE DataPlant,            ONLY : PlantLoop, SupplySide, DemandSide

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      INTEGER, INTENT(IN) :: LoopNum
      INTEGER, INTENT(IN) :: LoopSideNum
      INTEGER, INTENT(IN) :: MixNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNodeNum
  INTEGER :: MixerInletNode
  INTEGER :: MixerOutletNode
  INTEGER :: SplitterNum
  INTEGER :: SplitterInNode
  REAL(r64)    :: MixerOutletMassFlow  ! local calculation of mixer outlet mass flow rate
  REAL(r64)    :: MixerOutletMassFlowMaxAvail ! branch contribution to MassFlowRateMaxAvail at outlet
  REAL(r64)    :: MixerOutletMassFlowMinAvail
  REAL(r64)    :: MixerOutletTemp
  REAL(r64)    :: MixerInletMassFlow
  REAL(r64)    :: MassFrac
  REAL(r64)    :: MixerOutletPress
  REAL(r64)    :: MixerOutletQuality


          ! FLOW:
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%MixerExists) THEN

          !Find mixer outlet node number
    MixerOutletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%NodeNumOut

   ! Find corresponding splitter inlet node number--correspondence, but currently
   !  hard code things to a single split/mix setting it to the mixer number
    SplitterNum = MixNum
    SplitterInNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitterNum)%NodeNumIn
          !Initialize Mixer outlet temp and mass flow rate
    MixerOutletTemp             = 0.0d0
    MixerOutletMassFlow         = 0.0d0
    MixerOutletMassFlowMaxAvail = 0.0d0
    MixerOutletMassFlowMinAvail = 0.0d0
    MixerOutletPress            = 0.0d0
    MixerOutletQuality          = 0.0d0

          !Calculate Mixer outlet mass flow rate
    DO InletNodeNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%TotalInletNodes
      MixerInletNode              = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%NodeNumIn(InletNodeNum)
      MixerOutletMassFlow         = MixerOutletMassFlow + Node(MixerInletNode)%MassFlowRate
    END DO

          !Calculate Mixer outlet temperature
    DO InletNodeNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%TotalInletNodes
      MixerInletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%NodeNumIn(InletNodeNum)
      IF(MixerOutletMassFlow > 0.0d0) THEN
        MixerInletMassFlow = Node(MixerInletNode)%MassFlowRate
        MassFrac  = MixerInletMassFlow / MixerOutletMassFlow
          !mass flow weighted temp and enthalpy for each mixer inlet
        MixerOutletTemp             = MixerOutletTemp +  MassFrac * Node(MixerInletNode)%Temp
        MixerOutletQuality          = MixerOutletQuality +MassFrac*Node(MixerInletNode)%Quality
        MixerOutletMassFlowMaxAvail = MixerOutletMassFlowMaxAvail + Node(MixerInletNode)%MassFlowRateMaxAvail
        MixerOutletMassFlowMinAvail = MixerOutletMassFlowMinAvail + Node(MixerInletNode)%MassFlowRateMinAvail
        MixerOutletPress            = MAX(MixerOutletPress,Node(MixerInletNode)%Press)
      ELSE !MixerOutletMassFlow <=0, then perform the 'no flow' update.
        MixerOutletTemp             = Node(SplitterInNode)%Temp
        MixerOutletQuality          = Node(SplitterInNode)%Quality
        MixerOutletMassFlowMaxAvail = Node(SplitterInNode)%MassFlowRateMaxAvail
        MixerOutletMassFlowMinAvail = Node(SplitterInNode)%MassFlowRateMinAvail
        MixerOutletPress            = Node(SplitterInNode)%Press
        EXIT
      ENDIF
    ENDDO

    Node(MixerOutletNode)%MassFlowRate  = MixerOutletMassFlow
    Node(MixerOutletNode)%Temp          = MixerOutletTemp
    IF (PlantLoop(LoopNum)%HasPressureComponents) THEN
      !Don't update pressure, let pressure system handle this...
    ELSE
      !Go ahead and update!
      Node(MixerOutletNode)%Press         = MixerOutletPress
    END IF
    Node(MixerOutletNode)%Quality       = MixerOutletQuality

    ! set max/min avails on mixer outlet to be consistent with the following rules
    ! 1.  limited by the max/min avails on splitter inlet
    ! 2.  limited by the sum of max/min avails for each branch's mixer inlet node

    Node(MixerOutletNode)%MassFlowRateMaxAvail = MIN(MixerOutletMassFlowMaxAvail,Node(SplitterInNode)%MassFlowRateMaxAvail)
    Node(MixerOutletNode)%MassFlowRateMinAvail = MAX(MixerOutletMassFlowMinAvail,Node(SplitterInNode)%MassFlowRateMinAvail)


  END IF

  RETURN
END SUBROUTINE UpdatePlantMixer

FUNCTION AnyPlantSplitterMixerLacksContinuity() RESULT (NeedToReSimulate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Similiar to CheckPlantMixerSplitterConsistency, but used to decide if plant needs to iterate again

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant, ONLY : TotNumLoops, PlantLoop, DemandSide, SupplySide, &
                        CriteriaDelta_MassFlowRate
  USE DataLoopNode, ONLY : Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  LOGICAL :: NeedToReSimulate

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: LoopNum
  INTEGER :: SplitterInletNode
  INTEGER :: LoopSide
  INTEGER :: NumSplitterOutlets
  INTEGER :: OutletNum
  INTEGER :: BranchNum
  INTEGER :: LastNodeOnBranch
  REAL(r64) :: SumOutletFlow
  REAL(r64) :: AbsDifference


  NeedToReSimulate = .FALSE.

  DO LoopNum = 1, TotNumLoops
    DO LoopSide = DemandSide,SupplySide
      IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%SplitterExists) THEN
        SplitterInletNode = PlantLoop(LoopNum)%LoopSide(LoopSide)%Splitter(1)%NodeNumIn
        ! loop across branch outlet nodes and check mass continuity
        NumSplitterOutlets = PlantLoop(LoopNum)%LoopSide(LoopSide)%Splitter(1)%TotalOutletNodes
        SumOutletFlow = 0.d0
        DO OutletNum = 1, NumSplitterOutlets
          BranchNum        = PlantLoop(LoopNum)%LoopSide(LoopSide)%Splitter(1)%BranchNumOut(OutletNum)
          LastNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSide)%Branch(branchNum)%NodeNumOut
          SumOutletFlow = SumOutletFlow + Node(LastNodeOnBranch)%MassFlowRate
        ENDDO
        AbsDifference=ABS(Node(SplitterInletNode)%MassFlowRate - SumOutletFlow)
        IF (AbsDifference > CriteriaDelta_MassFlowRate) THEN
          NeedToReSimulate =  .TRUE.
          RETURN
        ENDIF
      ENDIF
    ENDDO
  ENDDO


  RETURN

END FUNCTION AnyPlantSplitterMixerLacksContinuity


SUBROUTINE CheckPlantMixerSplitterConsistency(LoopNum,LoopSideNum,SplitNum, MixNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Oct 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Check for plant flow resolver errors

          ! METHODOLOGY EMPLOYED:
          ! compare flow rate of splitter inlet to flow rate of mixer outlet

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode,         ONLY : Node
  USE DataPlant,            ONLY : PlantLoop, SupplySide, DemandSide, CriteriaDelta_MassFlowRate
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance
  USE DataInterfaces,       ONLY : ShowFatalError, ShowSevereError, ShowContinueError, ShowContinueErrorTimeStamp
  USE DataGlobals,          ONLY : WarmupFlag, DoingSizing
  USE General,              ONLY : RoundSigDigits
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: LoopNum
  INTEGER, INTENT(IN) :: LoopSideNum
  INTEGER, INTENT(IN) :: SplitNum
  INTEGER, INTENT(IN) :: MixNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: MixerOutletNode
  INTEGER  :: SplitterInletNode
  REAL(r64) :: AbsDifference
  INTEGER  :: NumSplitterOutlets
  REAL(r64) :: SumOutletFlow
  INTEGER   :: OutletNum
  INTEGER   :: BranchNum
  INTEGER   :: LastNodeOnBranch


  IF(.NOT. PlantLoop(LoopNum)%LoopHasConnectionComp) THEN
    IF (.not. DoingSizing .and. .not. WarmupFlag .and. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%MixerExists .and. &
        .not. FirstHVACIteration) THEN
          ! Find mixer outlet node number
      MixerOutletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%NodeNumOut
          ! Find splitter inlet node number
      SplitterInletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%NodeNumIn


      AbsDifference=ABS(Node(SplitterInletNode)%MassFlowRate - Node(MixerOutletNode)%MassFlowRate)
      IF (AbsDifference > MassFlowTolerance) THEN
        IF (PlantLoop(LoopNum)%MFErrIndex1 == 0) THEN
          CALL ShowSevereMessage('Plant flows do not resolve -- splitter inlet flow does not match mixer outlet flow ')
          CALL ShowContinueErrorTimeStamp(' ')
          CALL ShowContinueError('PlantLoop name= '//trim(PlantLoop(LoopNum)%Name) )
          CALL ShowContinueError('Plant Connector:Mixer name= '//trim(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%Name) )
          CALL ShowContinueError('Mixer outlet mass flow rate= '//  &
                                    trim(RoundSigDigits(Node(MixerOutletNode)%MassFlowRate,6))//' {kg/s}')
          CALL ShowContinueError('Plant Connector:Splitter name= '//  &
                                    trim(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%Name) )
          CALL ShowContinueError('Splitter inlet mass flow rate= '//  &
                                    trim(RoundSigDigits(Node(SplitterInletNode)%MassFlowRate,6))//' {kg/s}')
          CALL ShowContinueError('Difference in two mass flow rates= '//  &
                                    trim(RoundSigDigits(AbsDifference,6))//' {kg/s}')
        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('Plant Flows (Loop='//trim(PlantLoop(LoopNum)%Name)//  &
           ') splitter inlet flow not match mixer outlet flow',PlantLoop(LoopNum)%MFErrIndex1,    &
           ReportMaxOf=AbsDifference,ReportMinOf=AbsDifference,ReportMaxUnits='kg/s',ReportMinUnits='kg/s')
        IF (AbsDifference > MassFlowTolerance*10.0d0) THEN
          CALL ShowSevereError('Plant flows do not resolve -- splitter inlet flow does not match mixer outlet flow ')
          CALL ShowContinueErrorTimeStamp(' ')
          CALL ShowContinueError('PlantLoop name= '//trim(PlantLoop(LoopNum)%Name) )
          CALL ShowContinueError('Plant Connector:Mixer name= '//trim(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%Name) )
          CALL ShowContinueError('Mixer outlet mass flow rate= '//  &
                                    trim(RoundSigDigits(Node(MixerOutletNode)%MassFlowRate,6))//' {kg/s}')
          CALL ShowContinueError('Plant Connector:Splitter name= '//  &
                                    trim(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%Name) )
          CALL ShowContinueError('Splitter inlet mass flow rate= '//  &
                                    trim(RoundSigDigits(Node(SplitterInletNode)%MassFlowRate,6))//' {kg/s}')
          CALL ShowContinueError('Difference in two mass flow rates= '//  &
                                    trim(RoundSigDigits(AbsDifference,6))//' {kg/s}')
          CALL ShowFatalError('CheckPlantMixerSplitterConsistency: '//  &
                 'Simulation terminated because of problems in plant flow resolver')
        ENDIF
      ENDIF

      ! now check inside s/m to see if there are problems

      ! loop across branch outlet nodes and check mass continuity
      NumSplitterOutlets = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%TotalOutletNodes
      SumOutletFlow = 0.d0
    !  SumInletFlow  = 0.d0
      DO OutletNum = 1, NumSplitterOutlets
        BranchNum        = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%BranchNumOut(OutletNum)
        LastNodeOnBranch = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(branchNum)%NodeNumOut
        SumOutletFlow = SumOutletFlow + Node(LastNodeOnBranch)%MassFlowRate
      !  FirstNodeOnBranch= PlantLoop(LoopNum)%LoopSide(LoopSideNum)%branch(branchNum)%NodeNumIn
      !  SumInletFlow = SumInletFlow + Node(FirstNodeOnBranch)%MassFlowRate
      ENDDO
      AbsDifference=ABS(Node(SplitterInletNode)%MassFlowRate - SumOutletFlow)
      IF (AbsDifference > CriteriaDelta_MassFlowRate) THEN
        IF (PlantLoop(LoopNum)%MFErrIndex2 == 0) THEN
          CALL ShowSevereMessage('Plant flows do not resolve -- splitter inlet flow does not match branch outlet flows')
          CALL ShowContinueErrorTimeStamp(' ')
          CALL ShowContinueError('PlantLoop name= '//trim(PlantLoop(LoopNum)%Name) )
          CALL ShowContinueError('Plant Connector:Mixer name= '//trim(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%Name) )
          CALL ShowContinueError('Sum of Branch outlet mass flow rates= '//  &
                                    trim(RoundSigDigits(SumOutletFlow,6))//' {kg/s}')
          CALL ShowContinueError('Plant Connector:Splitter name= '//  &
                                    trim(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%Name) )
          CALL ShowContinueError('Splitter inlet mass flow rate= '//  &
                                    trim(RoundSigDigits(Node(SplitterInletNode)%MassFlowRate,6))//' {kg/s}')
          CALL ShowContinueError('Difference in two mass flow rates= '//  &
                                    trim(RoundSigDigits(AbsDifference,6))//' {kg/s}')
        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('Plant Flows (Loop='//trim(PlantLoop(LoopNum)%Name)//  &
           ') splitter inlet flow does not match branch outlet flows',PlantLoop(LoopNum)%MFErrIndex2,    &
           ReportMaxOf=AbsDifference,ReportMinOf=AbsDifference,ReportMaxUnits='kg/s',ReportMinUnits='kg/s')
!        IF (AbsDifference > CriteriaDelta_MassFlowRate*10.0d0) THEN
!          CALL ShowSevereError('Plant flows do not resolve -- splitter inlet flow does not match branch outlet flows')
!          CALL ShowContinueErrorTimeStamp(' ')
!          CALL ShowContinueError('PlantLoop name= '//trim(PlantLoop(LoopNum)%Name) )
!          CALL ShowContinueError('Plant Connector:Mixer name= '//trim(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(MixNum)%Name) )
!          CALL ShowContinueError('Sum of Branch outlet mass flow rates= '//  &
!                                    trim(RoundSigDigits(SumOutletFlow,6))//' {kg/s}')
!          CALL ShowContinueError('Plant Connector:Splitter name= '//  &
!                                    trim(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%Name) )
!          CALL ShowContinueError('Splitter inlet mass flow rate= '//  &
!                                    trim(RoundSigDigits(Node(SplitterInletNode)%MassFlowRate,6))//' {kg/s}')
!          CALL ShowContinueError('Difference in two mass flow rates= '//  &
!                                    trim(RoundSigDigits(AbsDifference,6))//' {kg/s}')
!          CALL ShowFatalError('CheckPlantMixerSplitterConsistency: Simulation terminated because of problems in plant flow resolver')
!        ENDIF
      ENDIF

    ENDIF
  END IF

  RETURN

END SUBROUTINE CheckPlantMixerSplitterConsistency

SUBROUTINE CheckForRunawayPlantTemps(LoopNum,LoopSideNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Sept 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Check for plant control errors revealed as run away fluid temps
          !  halt program so it won't siliently run in out of control state


          ! METHODOLOGY EMPLOYED:
          !  compare plant temps to plant min and max and halt if things run away
          !  sensitivity can be adjusted with parameters, picked somewhat arbitrary

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode,         ONLY : Node, NodeID
  USE DataPlant,            ONLY : PlantLoop, SupplySide, DemandSide, PlantReport, ShowBranchesOnLoop
  USE DataInterfaces,       ONLY : ShowFatalError, ShowSevereError, ShowContinueError, ShowContinueErrorTimeStamp, &
                                   ShowRecurringWarningErrorAtEnd
  USE DataGlobals,          ONLY : WarmupFlag, DoingSizing
  USE General,              ONLY : RoundSigDigits
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: LoopNum
  INTEGER, INTENT(IN) :: LoopSideNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64) , PARAMETER :: OverShootOffset = 5.d0
  REAL(r64) , PARAMETER :: UnderShootOffset = 5.d0
  REAL(r64) , PARAMETER :: FatalOverShootOffset = 200.d0
  REAL(r64) , PARAMETER :: FatalUnderShootOffset = 100.d0
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=4) :: hotcold
  LOGICAL :: makefatalerror
  CHARACTER(len=10) :: DemandSupply
  INTEGER :: LSN
  INTEGER :: BrN
  INTEGER :: CpN
  REAL(r64) :: LoopCapacity
  REAL(r64) :: LoopDemandSideCapacity
  REAL(r64) :: LoopSupplySideCapacity
  REAL(r64) :: DispatchedCapacity
  REAL(r64) :: LoopDemandSideDispatchedCapacity
  REAL(r64) :: LoopSupplySideDispatchedCapacity

  makefatalerror=.false.
  IF (Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut)%Temp > (PlantLoop(LoopNum)%MaxTemp + OverShootOffset )) THEN

  ! first stage, throw recurring warning that plant loop is getting out of control
    CALL ShowRecurringWarningErrorAtEnd('Plant loop exceeding upper temperature limit, PlantLoop="' // &
                      TRIM(PlantLoop(LoopNum)%Name)//'"' , PlantLoop(LoopNum)%MaxTempErrIndex, &
                          ReportMaxOf=Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut)%Temp )

    IF (Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut)%Temp &
                               > (PlantLoop(LoopNum)%MaxTemp + FatalOverShootOffset )) THEN
      hotcold='hot'
      makefatalerror=.true.
    ENDIF

  ENDIF

  IF (Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut)%Temp < (PlantLoop(LoopNum)%MinTemp - UnderShootOffset )) THEN

  ! first stage, throw recurring warning that plant loop is getting out of control
    CALL ShowRecurringWarningErrorAtEnd('Plant loop falling below lower temperature limit, PlantLoop="' // &
                      TRIM(PlantLoop(LoopNum)%Name)//'"' , PlantLoop(LoopNum)%MinTempErrIndex, &
                          ReportMinOf=Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut)%Temp )

    IF (Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut)%Temp &
                             < (PlantLoop(LoopNum)%MinTemp - FatalUnderShootOffset )) THEN
      hotcold='cold'
      makefatalerror=.true.
    ENDIF

  ENDIF

  IF (makefatalerror) THEN
    CALL ShowSevereError('Plant temperatures are getting far too '//trim(hotcold)//  &
       ', check controls and relative loads and capacities')
    CALL ShowContinueErrorTimeStamp('')
    IF (LoopSideNum == DemandSide) THEN
      DemandSupply='Demand'
    ELSEIF (LoopSideNum == SupplySide) THEN
      DemandSupply='Supply'
    ELSE
      DemandSupply='Unknown'
    ENDIF
    CALL ShowContinueError('PlantLoop Name ('//trim(DemandSupply)//'Side)= '//TRIM(PlantLoop(LoopNum)%Name) )
    CALL ShowContinueError('PlantLoop Setpoint Temperature='//  &
       TRIM(RoundSigDigits(Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint,1))//' {C}')
    IF (PlantLoop(LoopNum)%LoopSide(SupplySide)%InletNodeSetPt) THEN
      CALL ShowContinueError('PlantLoop Inlet Node (SupplySide) has a Setpoint.')
    ELSE
      CALL ShowContinueError('PlantLoop Inlet Node (SupplySide) does not have a Setpoint.')
    ENDIF
    IF (PlantLoop(LoopNum)%LoopSide(DemandSide)%InletNodeSetPt) THEN
      CALL ShowContinueError('PlantLoop Inlet Node (DemandSide) has a Setpoint.')
    ELSE
      CALL ShowContinueError('PlantLoop Inlet Node (DemandSide) does not have a Setpoint.')
    ENDIF
    IF (PlantLoop(LoopNum)%LoopSide(SupplySide)%OutletNodeSetPt) THEN
      CALL ShowContinueError('PlantLoop Outlet Node (SupplySide) has a Setpoint.')
    ELSE
      CALL ShowContinueError('PlantLoop Outlet Node (SupplySide) does not have a Setpoint.')
    ENDIF
    IF (PlantLoop(LoopNum)%LoopSide(DemandSide)%OutletNodeSetPt) THEN
      CALL ShowContinueError('PlantLoop Outlet Node (DemandSide) has a Setpoint.')
    ELSE
      CALL ShowContinueError('PlantLoop Outlet Node (DemandSide) does not have a Setpoint.')
    ENDIF
    CALL ShowContinueError('PlantLoop Outlet Node ('//trim(DemandSupply)//'Side) "'//  &
       TRIM(NodeID(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut))// &
       '" has temperature='// &
       TRIM(RoundSigDigits(Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut)%Temp,1))//' {C}')
    CALL ShowContinueError('PlantLoop  Inlet Node ('//trim(DemandSupply)//'Side) "'//  &
       TRIM(NodeID(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumIn))// &
       '" has temperature='// &
       TRIM(RoundSigDigits(Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumIn)%Temp,1))//' {C}')
    CALL ShowContinueError('PlantLoop Minimum Temperature='//  &
       trim(RoundSigDigits(PlantLoop(LoopNum)%MinTemp,1))//' {C}')
    CALL ShowContinueError('PlantLoop Maximum Temperature='//  &
       trim(RoundSigDigits(PlantLoop(LoopNum)%MaxTemp,1))//' {C}')
    CALL ShowContinueError('PlantLoop Flow Request (SupplySide)='//  &
       trim(RoundSigDigits(PlantLoop(LoopNum)%LoopSide(SupplySide)%FlowRequest,1))// ' {kg/s}')
    CALL ShowContinueError('PlantLoop Flow Request (DemandSide)='//  &
       trim(RoundSigDigits(PlantLoop(LoopNum)%LoopSide(DemandSide)%FlowRequest,1))// ' {kg/s}')
    CALL ShowContinueError('PlantLoop Node ('//trim(DemandSupply)//'Side) "'//  &
       TRIM(NodeID(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut))// &
       '" has mass flow rate ='// &
       TRIM(RoundSigDigits(Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut)%MassFlowRate,1))//' {kg/s}')
    CALL ShowContinueError('PlantLoop PumpHeat (SupplySide)='//  &
       trim(RoundSigDigits(PlantLoop(LoopNum)%LoopSide(SupplySide)%TotalPumpHeat,1))//' {W}')
    CALL ShowContinueError('PlantLoop PumpHeat (DemandSide)='//  &
       trim(RoundSigDigits(PlantLoop(LoopNum)%LoopSide(DemandSide)%TotalPumpHeat,1))//' {W}')
    CALL ShowContinueError('PlantLoop Cooling Demand='//trim(RoundSigDigits(PlantReport(LoopNum)%CoolingDemand,1))//' {W}')
    CALL ShowContinueError('PlantLoop Heating Demand='//trim(RoundSigDigits(PlantReport(LoopNum)%HeatingDemand,1))//' {W}')
    CALL ShowContinueError('PlantLoop Demand not Dispatched='//  &
       trim(RoundSigDigits(PlantReport(LoopNum)%DemandNotDispatched,1))//' {W}')
    CALL ShowContinueError('PlantLoop Unmet Demand='//trim(RoundSigDigits(PlantReport(LoopNum)%UnMetDemand,1))//' {W}')

    LoopCapacity=0.0d0
    DispatchedCapacity = 0.d0
    DO LSN=DemandSide,SupplySide
      DO BrN=1,PlantLoop(LoopNum)%LoopSide(LSN)%TotalBranches
        DO CpN=1,PlantLoop(LoopNum)%LoopSide(LSN)%Branch(BrN)%TotalComponents
          LoopCapacity=LoopCapacity+PlantLoop(LoopNum)%LoopSide(LSN)%Branch(BrN)%Comp(CpN)%MaxLoad
          DispatchedCapacity = DispatchedCapacity + ABS(PlantLoop(LoopNum)%LoopSide(LSN)%Branch(BrN)%Comp(CpN)%MyLoad)
        ENDDO
      ENDDO
      IF (LSN == DemandSide) THEN
        LoopDemandSideCapacity=LoopCapacity
        LoopDemandSideDispatchedCapacity = DispatchedCapacity
      ELSE
        LoopSupplySideCapacity=LoopCapacity-LoopDemandSideCapacity
        LoopSupplySideDispatchedCapacity = DispatchedCapacity-LoopDemandSideDispatchedCapacity
      ENDIF
    ENDDO
    CALL ShowContinueError('PlantLoop Capacity='//trim(RoundSigDigits(LoopCapacity,1))//' {W}')
    CALL ShowContinueError('PlantLoop Capacity (SupplySide)='//trim(RoundSigDigits(LoopSupplySideCapacity,1))//' {W}')
    CALL ShowContinueError('PlantLoop Capacity (DemandSide)='//trim(RoundSigDigits(LoopDemandSideCapacity,1))//' {W}')
    CALL ShowContinueError('PlantLoop Operation Scheme='//trim(PlantLoop(LoopNum)%OperationScheme))
    CALL ShowContinueError('PlantLoop Operation Dispatched Load = '//trim(RoundSigDigits(DispatchedCapacity, 1))// ' {W}')
    CALL ShowContinueError('PlantLoop Operation Dispatched Load (SupplySide)= ' &
                              //trim(RoundSigDigits(LoopSupplySideDispatchedCapacity, 1))// ' {W}')
    CALL ShowContinueError('PlantLoop Operation Dispatched Load (DemandSide)= ' &
                              //trim(RoundSigDigits(LoopDemandSideDispatchedCapacity, 1))// ' {W}')
    CALL ShowContinueError('Branches on the Loop.')
    CALL ShowBranchesOnLoop(LoopNum)
    CALL ShowContinueError('*************************')
    CALL ShowContinueError('Possible things to look for to correct this problem are:')
    CALL ShowContinueError('  Capacity, Operation Scheme, Mass flow problems, Pump Heat building up over time.')
    CALL ShowContinueError('  Try a shorter runperiod to stop before it fatals and look at')
    CALL ShowContinueError('    lots of node time series data to see what is going wrong.')
    CALL ShowContinueError('  If this is happening during Warmup, you can use Output:Diagnostics,ReportDuringWarmup;')
    CALL ShowContinueError('  This is detected at the loop level, but the typical problems are in the components.')
    CALL ShowFatalError('CheckForRunawayPlantTemps: Simulation terminated because of run away plant temperatures, too '//  &
       trim(hotcold))
  ENDIF

  RETURN

END SUBROUTINE CheckForRunawayPlantTemps


SUBROUTINE UpdatePlantSplitter(LoopNum, LoopSideNum, SplitNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brandon Anderson, Dan Fisher
          !       DATE WRITTEN   October 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the outlet conditions of the splitter

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataPlant,     ONLY : PlantLoop, DemandSide,SupplySide
  USE DataLoopNode,  ONLY : Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      INTEGER, INTENT(IN) :: LoopNum
      INTEGER, INTENT(IN) :: LoopSideNum
    !  INTEGER, INTENT(IN) :: FlowLock
      INTEGER, INTENT(IN) :: SplitNum
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

      INTEGER :: SplitterInletNode
      INTEGER :: SplitterOutletNode
      INTEGER :: CurNode


   ! Update Temperatures across splitter
   IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SplitterExists) THEN

          ! Set branch number at splitter inlet
        SplitterInletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%NodeNumIn

        !Loop over outlet nodes
        DO CurNode = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%TotalOutletNodes
          SplitterOutletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%NodeNumOut(CurNode)

          !Inlet Temp equals exit Temp to all outlet branches
          Node(SplitterOutletNode)%Temp = Node(SplitterInletNode)%Temp
          Node(SplitterOutletNode)%TempMin = Node(SplitterInletNode)%TempMin
          Node(SplitterOutletNode)%TempMax = Node(SplitterInletNode)%TempMax
          IF (PlantLoop(LoopNum)%HasPressureComponents) THEN
            !Don't update pressure, let pressure system handle this...
          ELSE
            !Go ahead and update!
            Node(SplitterOutletNode)%Press = Node(SplitterInletNode)%Press
          END IF
          Node(SplitterOutletNode)%Quality = Node(SplitterInletNode)%Quality

          !DSU? These two blocks and the following one which I added need to be cleaned up
          ! I think we will always pass maxavail down the splitter, min avail is the issue.
          ! Changed to include hardware max in next line 7/26/2011
          Node(SplitterOutletNode)%MassFlowRateMaxAvail = MIN(Node(SplitterInletNode)%MassFlowRateMaxAvail, &
                                                               Node(SplitterOutletNode)%MassFlowRateMax)
          Node(SplitterOutletNode)%MassFlowRateMinAvail = 0.0d0
!
!          If(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock == 0 .and. LoopSideNum == SupplySide) Then
!             Node(SplitterOutletNode)%MassFlowRateMaxAvail = Node(SplitterInletNode)%MassFlowRateMaxAvail
!             Node(SplitterOutletNode)%MassFlowRateMinAvail = 0.0d0
!             !Node(SplitterInletNode)%MassFlowRateMinAvail CR branch pumps (7643)
!          End If
!          If(LoopSideNum == DemandSide .AND. (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%LoopPump .OR. &
!             PlantLoop(LoopNum)%LoopSide(LoopSideNum)%BranchPump)) Then
!             Node(SplitterOutletNode)%MassFlowRateMaxAvail = Node(SplitterInletNode)%MassFlowRateMaxAvail
!             Node(SplitterOutletNode)%MassFlowRateMinAvail = 0.0d0
!          End If

          !DSU? Not sure about passing min avail if it is nonzero.  I am testing a pump with nonzero
          ! min flow rate, and it is causing problems because this routine passes zero down.  Perhaps if
          ! it is a single parallel branch, we are safe to assume we need to just pass it down.
          ! But need to test for multiple branches (or at least think about it), to see what we need to do...
          IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(SplitNum)%TotalOutletNodes == 1) THEN
            Node(SplitterOutletNode)%MassFlowRateMinAvail = Node(SplitterInletNode)%MassFlowRateMinAvail
          END IF

        END DO

  END IF

  RETURN
END SUBROUTINE UpdatePlantSplitter


SUBROUTINE SetAllFlowLocks(Value)

   ! SUBROUTINE INFORMATION:
   !       AUTHOR         Edwin Lee
   !       DATE WRITTEN   November 2009
   !       MODIFIED       na
   !       RE-ENGINEERED  na
   !
   ! PURPOSE OF THIS SUBROUTINE:
   ! This subroutine will set both loopside flowlocks on all plant loops to the input value (0 or 1)
   ! Initially this routine is used as a quick replacement for the FlowLock=0 and FlowLock=1 statements
   !  in order to provide the same behavior through phase I of the demand side rewrite
   ! Eventually this routine may be employed again to quickly initialize all loops once phase III is complete
   !
   ! METHODOLOGY EMPLOYED:
   ! Standard EnergyPlus methodology.
   !
   ! REFERENCES:
   ! na
   !
   ! USE STATEMENTS:
 USE DataPlant, ONLY : PlantLoop

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

   ! SUBROUTINE ARGUMENT DEFINITIONS:
 INTEGER, INTENT(IN)  :: Value

   ! SUBROUTINE PARAMETER DEFINITIONS:
   ! na

   ! INTERFACE BLOCK SPECIFICATIONS
   ! na

   ! DERIVED TYPE DEFINITIONS
   ! na

   ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 INTEGER     :: LoopNum
 INTEGER     :: LoopSideNum
 INTEGER     :: NumCount

   ! Flow:
  IF (ALLOCATED(PlantLoop)) THEN
    NumCount=SIZE(PlantLoop)
  ELSE
    NumCount=0
  ENDIF
  DO LoopNum = 1, NumCount !SIZE(PlantLoop)
   DO LoopSideNum = 1, SIZE(PlantLoop(LoopNum)%LoopSide)
     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock = Value
   END DO
 END DO

END SUBROUTINE SetAllFlowLocks

SUBROUTINE ResetAllPlantInterConnectFlags()

   ! SUBROUTINE INFORMATION:
   !       AUTHOR         Edwin Lee
   !       DATE WRITTEN   September 2010
   !       MODIFIED       na
   !       RE-ENGINEERED  na
   !
   ! PURPOSE OF THIS SUBROUTINE:
   ! This subroutine will reset all interconnected (air, zone, etc.) sim flags for both loopsides of all loops
   !
   ! METHODOLOGY EMPLOYED:
   ! Standard EnergyPlus methodology.
   !
   ! REFERENCES:
   ! na
   !
   ! USE STATEMENTS:
 USE DataPlant, ONLY : PlantLoop, TotNumLoops

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

   ! SUBROUTINE ARGUMENT DEFINITIONS:
   ! na

   ! SUBROUTINE PARAMETER DEFINITIONS:
   ! na

   ! INTERFACE BLOCK SPECIFICATIONS
   ! na

   ! DERIVED TYPE DEFINITIONS
   ! na

   ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 INTEGER     :: LoopNum

   ! Flow:
 DO LoopNum = 1, TotNumLoops
      PlantLoop(LoopNum)%LoopSide%SimAirLoopsNeeded = .FALSE.
      PlantLoop(LoopNum)%LoopSide%SimZoneEquipNeeded = .FALSE.
      PlantLoop(LoopNum)%LoopSide%SimNonZoneEquipNeeded = .FALSE.
      PlantLoop(LoopNum)%LoopSide%SimElectLoadCentrNeeded = .FALSE.
 END DO

END SUBROUTINE ResetAllPlantInterConnectFlags

SUBROUTINE PullCompInterconnectTrigger(LoopNum, LoopSide, BranchNum, CompNum, &
                                       UniqueCriteriaCheckIndex,              &
                                       ConnectedLoopNum, ConnectedLoopSide,   &
                                       CriteriaType, CriteriaValue)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provides a generic means for components to trigger interconnected loop sides sim flags

          ! METHODOLOGY EMPLOYED:
          ! Determine convergence criteria based on *CriteriaType* variable.  This routine only turns
          !  the loop side sim flag ON, it doesn't turn it OFF.
          ! The convergence value history was originally going to be put at the Branch()%Comp()%...
          !  level, but this would be quite difficult if we had multiple convergence checks for the
          !  same component, such as if a chiller was trying to turn on the condenser side and the
          !  heat recovery side.
          ! It was determined to use a local array, which is only reallocated during the first stages
          !  of the simulation when components are first calling their sim flag requests.  After that
          !  the INOUT index variable will be used to avoid reallocation and string compares.
          ! Error handling will be put in to ensure unique identifiers are used for debugging purposes.
          ! A single component may have multiple check indeces, but a single index will only have one
          !  associated component.  Therefore whenever we come in with a non-zero index, we will just
          !  verify that the stored loop/side/branch/comp matches

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,      ONLY: PlantLoop, CriteriaType_MassFlowRate, CriteriaType_Temperature, CriteriaType_HeatTransferRate, &
                            CriteriaDelta_MassFlowRate, CriteriaDelta_Temperature, CriteriaDelta_HeatTransferRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,          INTENT(IN)     :: LoopNum                  ! component's loop index
  INTEGER,          INTENT(IN)     :: LoopSide                 ! component's loop side number
  INTEGER,          INTENT(IN)     :: BranchNum                ! Component's branch number
  INTEGER,          INTENT(IN)     :: CompNum                  ! Component's comp number
  INTEGER,          INTENT(IN OUT) :: UniqueCriteriaCheckIndex ! An integer given to this particular check
                                                               !      -- set this to zero initially in calling routine
  INTEGER,          INTENT(IN)     :: ConnectedLoopNum         ! Component's interconnected loop number
  INTEGER,          INTENT(IN)     :: ConnectedLoopSide        ! Component's interconnected loop side number
  INTEGER,          INTENT(IN)     :: CriteriaType             ! The criteria check to use, see DataPlant: SimFlagCriteriaTypes
  REAL(r64),        INTENT(IN)     :: CriteriaValue            ! The value of the criteria check to evaluate

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  TYPE CriteriaData
    INTEGER    :: CallingCompLoopNum     = 0     ! for debug error handling
    INTEGER    :: CallingCompLoopSideNum = 0     ! for debug error handling
    INTEGER    :: CallingCompBranchNum   = 0     ! for debug error handling
    INTEGER    :: CallingCompCompNum     = 0     ! for debug error handling
    REAL(r64)  :: ThisCriteriaCheckValue = 0.0d0 ! the previous value, to check the current against
  END TYPE CriteriaData

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE(CriteriaData), DIMENSION(:), ALLOCATABLE, SAVE :: CriteriaChecks ! stores criteria information
  TYPE(CriteriaData), DIMENSION(:), ALLOCATABLE :: TempCriteriaChecks   ! used for reallocation during initial calls
  TYPE(CriteriaData) :: CurCriteria ! for convenience
  INTEGER  :: PreviousNumChecksStored
  INTEGER, SAVE  :: CurrentNumChecksStored

  IF (UniqueCriteriaCheckIndex .LE. 0) THEN ! If we don't yet have an index, we need to initialize

    ! We need to start by allocating, or REallocating the array
    IF (.NOT. ALLOCATED(CriteriaChecks)) THEN
      CurrentNumChecksStored = 1
      ALLOCATE(CriteriaChecks(CurrentNumChecksStored))
    ELSE
      IF (ALLOCATED(TempCriteriaChecks)) DEALLOCATE(TempCriteriaChecks)
      PreviousNumChecksStored = SIZE(CriteriaChecks)
      CurrentNumChecksStored = PreviousNumChecksStored + 1
      ALLOCATE(TempCriteriaChecks(CurrentNumChecksStored))
      TempCriteriaChecks(1:PreviousNumChecksStored) = CriteriaChecks
      DEALLOCATE(CriteriaChecks)
      ALLOCATE(CriteriaChecks(CurrentNumChecksStored))
    END IF

    ! Store the unique name and location
    CriteriaChecks(CurrentNumChecksStored)%CallingCompLoopNum = LoopNum
    CriteriaChecks(CurrentNumChecksStored)%CallingCompLoopSideNum = LoopSide
    CriteriaChecks(CurrentNumChecksStored)%CallingCompBranchNum = BranchNum
    CriteriaChecks(CurrentNumChecksStored)%CallingCompCompNum = CompNum

    ! Since this was the first pass, it is safe to assume something has changed!
    ! Therefore we'll set the sim flag to true
    PlantLoop(ConnectedLoopNum)%LoopSide(ConnectedLoopSide)%SimLoopSideNeeded = .TRUE.

    ! Make sure we return the proper value of index
    UniqueCriteriaCheckIndex = CurrentNumChecksStored

  ELSE ! We already have an index

    ! If we have an index, we need to do a brief error handling, then determine
    !  sim flag status based on the criteria type

    ! First store the current check in a single variable instead of array for readability
    CurCriteria = CriteriaChecks(UniqueCriteriaCheckIndex)

    ! Check to make sure we didn't reuse the index in multiple components
    IF (     CurCriteria%CallingCompLoopNum     .NE. LoopNum &
        .OR. CurCriteria%CallingCompLoopSideNum .NE. LoopSide &
        .OR. CurCriteria%CallingCompBranchNum   .NE. BranchNum &
        .OR. CurCriteria%CallingCompCompNum     .NE. CompNum) THEN
      ! Diagnostic fatal: component does not properly utilize unique indexing
    END IF

    ! Initialize, then check if we are out of range
    SELECT CASE (CriteriaType)
      CASE (CriteriaType_MassFlowRate)
        IF (ABS(CurCriteria%ThisCriteriaCheckValue-CriteriaValue)>CriteriaDelta_MassFlowRate) THEN
          PlantLoop(ConnectedLoopNum)%LoopSide(ConnectedLoopSide)%SimLoopSideNeeded = .TRUE.
        END IF

      CASE (CriteriaType_Temperature)
        IF (ABS(CurCriteria%ThisCriteriaCheckValue-CriteriaValue)>CriteriaDelta_Temperature) THEN
          PlantLoop(ConnectedLoopNum)%LoopSide(ConnectedLoopSide)%SimLoopSideNeeded = .TRUE.
        END IF

      CASE (CriteriaType_HeatTransferRate)
        IF (ABS(CurCriteria%ThisCriteriaCheckValue-CriteriaValue)>CriteriaDelta_HeatTransferRate) THEN
          PlantLoop(ConnectedLoopNum)%LoopSide(ConnectedLoopSide)%SimLoopSideNeeded = .TRUE.
        END IF

      CASE DEFAULT
       ! Diagnostic fatal: improper criteria type

    END SELECT

  END IF ! if we have an index or not

  ! Store the value for the next pass
  CriteriaChecks(UniqueCriteriaCheckIndex)%ThisCriteriaCheckValue = CriteriaValue

 RETURN

END SUBROUTINE PullCompInterconnectTrigger

SUBROUTINE UpdateChillerComponentCondenserSide(LoopNum, LoopSide, TypeOfNum,              &
                                     InletNodeNum, OutletNodeNum, ModelCondenserHeatRate, &
                                     ModelInletTemp, ModelOutletTemp, ModelMassFlowRate, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   February 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! provides reusable update routine for water cooled chiller's condenser water
          ! connection to plant loops

          ! METHODOLOGY EMPLOYED:
          ! check if anything changed or doesn't agree and set simulation flags.
          ! update outlet conditions if needed or possible


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,      ONLY: PlantLoop
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE DataLoopNode ,  ONLY: Node
  USE FluidProperties, ONLY: GetSpecificHeatGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER ,  INTENT(IN) :: LoopNum  ! component's loop index
  INTEGER ,  INTENT(IN) :: LoopSide ! component's loop side number
  INTEGER ,  INTENT(IN) :: TypeOfNum ! Component's type index
  INTEGER ,  INTENT(IN) :: InletNodeNum ! Component's inlet node pointer
  INTEGER ,  INTENT(IN) :: OutletNodeNum ! Component's outlet node pointer
  REAL(r64), INTENT(IN) :: ModelCondenserHeatRate ! model's heat rejection rate at condenser (W)
  REAL(r64), INTENT(IN) :: ModelInletTemp ! model's inlet temperature (C)
  REAL(r64), INTENT(IN) :: ModelOutletTemp ! model's outlet temperature (C)
  REAL(r64), INTENT(IN) :: ModelMassFlowRate  ! model's condenser water mass flow rate (kg/s)
  LOGICAL ,  INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: DidAnythingChange = .FALSE. ! set to true if conditions changed
  INTEGER :: OtherLoopNum  ! local loop pointer for remote connected loop
  INTEGER :: OtherLoopSide ! local loop side pointer for remote connected loop
  INTEGER :: ConnectLoopNum  ! local do loop counter
  REAL(r64) :: Cp

  DidAnythingChange = .FALSE.

  !check if any conditions have changed
  IF (Node(InletNodeNum)%MassFlowRate /= ModelMassFlowRate) DidAnythingChange = .TRUE.

  IF (Node(OutletNodeNum)%MassFlowRate /= ModelMassFlowRate) DidAnythingChange = .TRUE.

  IF (Node(InletNodeNum)%Temp /= ModelInletTemp) DidAnythingChange = .TRUE.

  IF (Node(OutletNodeNum)%Temp /= ModelOutletTemp) DidAnythingChange = .TRUE.

  ! could also check heat rate agains McDeltaT from node data

  IF ((Node(InletNodeNum)%MassFlowRate == 0.0D0) .AND. (ModelCondenserHeatRate > 0.0D0) ) THEN

  ! DSU3 TODO also send a request that condenser loop be made available, interlock message infrastructure??

    DidAnythingChange = .TRUE.

  ENDIF


  IF (DidAnythingChange .OR. FirstHVACIteration) THEN
   ! use current mass flow rate and inlet temp from Node and recalculate outlet temp
    IF (Node(InletNodeNum)%MassFlowRate > MassFlowTolerance) THEN
          ! update node outlet conditions
      Cp = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName, ModelInletTemp, &
                       PlantLoop(LoopNum)%FluidIndex, 'UpdateChillerComponentCondenserSide')
      Node(OutletNodeNum)%Temp =  Node(InletNodeNum)%Temp + ModelCondenserHeatRate &
                                                /(Node(InletNodeNum)%MassFlowRate*Cp)

    ENDIF

    ! set sim flag for this loop
    PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded = .TRUE.

    !set sim flag on connected loops to true because this side changed
    IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%TotalConnected > 0) THEN
      DO ConnectLoopNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSide)%TotalConnected
        IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%Connected(ConnectLoopNum)%LoopDemandsOnRemote) THEN
          OtherLoopNum  = PlantLoop(LoopNum)%LoopSide(LoopSide)%Connected(ConnectLoopNum)%LoopNum
          OtherLoopSide = PlantLoop(LoopNum)%LoopSide(LoopSide)%Connected(ConnectLoopNum)%LoopSideNum
          PlantLoop(OtherLoopNum)%LoopSide(OtherLoopSide)%SimLoopSideNeeded = .TRUE.
        ENDIF
      ENDDO
    ENDIF

  ELSE ! nothing changed so turn off sim flag
    PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded = .FALSE.
  ENDIF

  RETURN

END SUBROUTINE UpdateChillerComponentCondenserSide

SUBROUTINE UpdateComponentHeatRecoverySide(LoopNum, LoopSide, TypeOfNum,              &
                                     InletNodeNum, OutletNodeNum, ModelRecoveryHeatRate, &
                                     ModelInletTemp, ModelOutletTemp, ModelMassFlowRate, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Sept 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! provides reusable update routine for heat recovery type
          ! connection to plant loops

          ! METHODOLOGY EMPLOYED:
          ! check if anything changed or doesn't agree and set simulation flags.
          ! update outlet conditions if needed or possible


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,      ONLY: PlantLoop
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE DataLoopNode ,  ONLY: Node
  USE FluidProperties, ONLY: GetSpecificHeatGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER ,  INTENT(IN) :: LoopNum  ! component's loop index
  INTEGER ,  INTENT(IN) :: LoopSide ! component's loop side number
  INTEGER ,  INTENT(IN) :: TypeOfNum ! Component's type index
  INTEGER ,  INTENT(IN) :: InletNodeNum ! Component's inlet node pointer
  INTEGER ,  INTENT(IN) :: OutletNodeNum ! Component's outlet node pointer
  REAL(r64), INTENT(IN) :: ModelRecoveryHeatRate ! model's heat rejection rate at recovery (W)
  REAL(r64), INTENT(IN) :: ModelInletTemp ! model's inlet temperature (C)
  REAL(r64), INTENT(IN) :: ModelOutletTemp ! model's outlet temperature (C)
  REAL(r64), INTENT(IN) :: ModelMassFlowRate  ! model's condenser water mass flow rate (kg/s)
  LOGICAL ,  INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: DidAnythingChange = .FALSE. ! set to true if conditions changed
  INTEGER :: OtherLoopNum  ! local loop pointer for remote connected loop
  INTEGER :: OtherLoopSide ! local loop side pointer for remote connected loop
!  INTEGER :: CountConnectedLoops ! local total number of connected loops
  INTEGER :: ConnectLoopNum  ! local do loop counter
  REAL(r64) :: Cp ! local fluid specific heat

  DidAnythingChange = .FALSE.

  !check if any conditions have changed
  IF (Node(InletNodeNum)%MassFlowRate /= ModelMassFlowRate) DidAnythingChange = .TRUE.

  IF (Node(OutletNodeNum)%MassFlowRate /= ModelMassFlowRate) DidAnythingChange = .TRUE.

  IF (Node(InletNodeNum)%Temp /= ModelInletTemp) DidAnythingChange = .TRUE.

  IF (Node(OutletNodeNum)%Temp /= ModelOutletTemp) DidAnythingChange = .TRUE.

  ! could also check heat rate agains McDeltaT from node data

  IF ((Node(InletNodeNum)%MassFlowRate == 0.0D0) .AND. (ModelRecoveryHeatRate > 0.0D0) ) THEN
    !no flow but trying to move heat to this loop problem!

    DidAnythingChange = .TRUE.

  ENDIF


  IF (DidAnythingChange .OR. FirstHVACIteration) THEN
   ! use current mass flow rate and inlet temp from Node and recalculate outlet temp
    IF (Node(InletNodeNum)%MassFlowRate > MassFlowTolerance) THEN
          ! update node outlet conditions
      Cp = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName, ModelInletTemp, &
                       PlantLoop(LoopNum)%FluidIndex, 'UpdateComponentHeatRecoverySide')
      Node(OutletNodeNum)%Temp =  Node(InletNodeNum)%Temp + ModelRecoveryHeatRate &
                                /(Node(InletNodeNum)%MassFlowRate*Cp )

    ENDIF

    ! set sim flag for this loop
    PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded = .TRUE.

    !set sim flag on connected loops to true because this side changed
    IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%TotalConnected > 0) THEN
      DO ConnectLoopNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSide)%TotalConnected
        IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%Connected(ConnectLoopNum)%LoopDemandsOnRemote) THEN
          OtherLoopNum  = PlantLoop(LoopNum)%LoopSide(LoopSide)%Connected(ConnectLoopNum)%LoopNum
          OtherLoopSide = PlantLoop(LoopNum)%LoopSide(LoopSide)%Connected(ConnectLoopNum)%LoopSideNum
          PlantLoop(OtherLoopNum)%LoopSide(OtherLoopSide)%SimLoopSideNeeded = .TRUE.
        ENDIF
      ENDDO
    ENDIF

  ELSE ! nothing changed so turn off sim flag
    PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded = .FALSE.
  ENDIF

  RETURN

END SUBROUTINE UpdateComponentHeatRecoverySide


SUBROUTINE UpdateAbsorberChillerComponentGeneratorSide(LoopNum, LoopSide, TypeOfNum,              &
                                     InletNodeNum, OutletNodeNum, HeatSourceType, &
                                     ModelGeneratorHeatRate, ModelMassFlowRate, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   February 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! provides reusable update routine for absoption chiller's generator
          ! connection to plant loops

          ! METHODOLOGY EMPLOYED:
          ! check if anything changed or doesn't agree and set simulation flags.
          ! update outlet conditions if needed or possible

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant,      ONLY: PlantLoop
  USE DataLoopNode ,  ONLY: Node, NodeType_Water, NodeType_Steam


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER ,  INTENT(IN) :: LoopNum  ! component's loop index
  INTEGER ,  INTENT(IN) :: LoopSide ! component's loop side number
  INTEGER ,  INTENT(IN) :: TypeOfNum ! Component's type index
  INTEGER ,  INTENT(IN) :: InletNodeNum ! Component's inlet node pointer
  INTEGER ,  INTENT(IN) :: OutletNodeNum ! Component's outlet node pointer
  INTEGER ,  INTENT(IN) :: HeatSourceType ! Type of fluid in Generator loop
  REAL(r64), INTENT(IN) :: ModelGeneratorHeatRate ! model's generator heat rate (W)
  REAL(r64), INTENT(IN) :: ModelMassFlowRate  ! model's generator mass flow rate (kg/s)
  LOGICAL,   INTENT(IN) :: FirstHVACIteration
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: DidAnythingChange = .FALSE. ! set to true if conditions changed
  INTEGER :: OtherLoopNum  ! local loop pointer for remote connected loop
  INTEGER :: OtherLoopSide ! local loop side pointer for remote connected loop
!  INTEGER :: CountConnectedLoops ! local total number of connected loops
  INTEGER :: ConnectLoopNum  ! local do loop counter

  DidAnythingChange = .FALSE.

  ! check if node heat rate compares well with generator heat rate
  IF (HeatSourceType == NodeType_Water) THEN

  ELSEIF (HeatSourceType == NodeType_Steam) THEN

  ELSE
   ! throw error

  ENDIF


  !check if any conditions have changed
  IF (Node(InletNodeNum)%MassFlowRate /= ModelMassFlowRate) DidAnythingChange = .TRUE.

    IF ((Node(InletNodeNum)%MassFlowRate == 0.0D0) .AND. (ModelGeneratorHeatRate > 0.0D0) ) THEN

  ! DSU3 TODO also send a request that generator loop be made available, interlock message infrastructure??

    DidAnythingChange = .TRUE.

  ENDIF

  IF (DidAnythingChange .OR. FirstHVACIteration) THEN

    ! set sim flag for this loop
    PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded = .TRUE.

    !set sim flag on connected loops to true because this side changed
    IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%TotalConnected > 0) THEN
      DO ConnectLoopNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSide)%TotalConnected
        IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%Connected(ConnectLoopNum)%LoopDemandsOnRemote) THEN
          OtherLoopNum  = PlantLoop(LoopNum)%LoopSide(LoopSide)%Connected(ConnectLoopNum)%LoopNum
          OtherLoopSide = PlantLoop(LoopNum)%LoopSide(LoopSide)%Connected(ConnectLoopNum)%LoopSideNum
          PlantLoop(OtherLoopNum)%LoopSide(OtherLoopSide)%SimLoopSideNeeded = .TRUE.
        ENDIF
      ENDDO
    ENDIF

  ELSE ! nothing changed so turn off sim flag
    PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded = .FALSE.
  ENDIF

  RETURN

END SUBROUTINE UpdateAbsorberChillerComponentGeneratorSide

SUBROUTINE InterConnectTwoPlantLoopSides(Loop1Num, Loop1LoopSideNum,Loop2Num, Loop2LoopSideNum,&
                             PlantComponentTypeOfNum, Loop1DemandsOnLoop2 )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   February 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Setup PlantLoop data structure pointers to direct interacting loops

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataPlant, Only: PlantLoop, ConnectedLoopData

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Loop1Num
  INTEGER, INTENT(IN) :: Loop1LoopSideNum
  INTEGER, INTENT(IN) :: Loop2Num
  INTEGER, INTENT(IN) :: Loop2LoopSideNum
  INTEGER, INTENT(IN) :: PlantComponentTypeOfNum
  LOGICAL, INTENT(IN) :: Loop1DemandsOnLoop2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:



          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE(ConnectedLoopData), DIMENSION(:), ALLOCATABLE :: TemporaryConnectedLoops
  INTEGER :: CurrentNumConnectedLoops
  LOGICAL :: Loop2DemandsOnLoop1

  IF (Loop1Num .EQ. 0 .OR. Loop1LoopSideNum .EQ. 0 .OR. &
      Loop2Num .EQ. 0 .OR. Loop2LoopSideNum .EQ. 0) THEN
    RETURN   ! Associated ScanPlantLoopsForObject couldn't find the component in the the plant loop structure...
  ENDIF      ! This is a Fatal error condition

  Loop2DemandsOnLoop1 = .FALSE.
  IF (.NOT. Loop1DemandsOnLoop2) Loop2DemandsOnLoop1 = .TRUE.

  IF (ALLOCATED(PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected)) THEN
    CurrentNumConnectedLoops = PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%TotalConnected
    ALLOCATE(TemporaryConnectedLoops(CurrentNumConnectedLoops))
    TemporaryConnectedLoops = PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected

    DEALLOCATE( PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected )
    ALLOCATE( PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(CurrentNumConnectedLoops +1) )
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%TotalConnected = CurrentNumConnectedLoops +1
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(1:CurrentNumConnectedLoops) = TemporaryConnectedLoops
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(CurrentNumConnectedLoops+1)%LoopNum  = Loop2Num
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(CurrentNumConnectedLoops+1)%LoopSideNum = Loop2LoopSideNum
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(CurrentNumConnectedLoops+1)%ConnectorTypeOf_Num &
                      = PlantComponentTypeOfNum
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(CurrentNumConnectedLoops+1)%LoopDemandsOnRemote &
                      = Loop1DemandsOnLoop2

    DEALLOCATE(TemporaryConnectedLoops)
  ELSE
    ALLOCATE( PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(1) )
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%TotalConnected = 1
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(1)%LoopNum  = Loop2Num
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(1)%LoopSideNum = Loop2LoopSideNum
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(1)%ConnectorTypeOf_Num &
                      = PlantComponentTypeOfNum
    PlantLoop(Loop1Num)%LoopSide(Loop1LoopSideNum)%Connected(1)%LoopDemandsOnRemote &
                      = Loop1DemandsOnLoop2
  ENDIF


  IF (ALLOCATED(PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected)) THEN

    CurrentNumConnectedLoops = PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%TotalConnected
    ALLOCATE(TemporaryConnectedLoops(CurrentNumConnectedLoops))
    TemporaryConnectedLoops = PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected

    DEALLOCATE( PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected )
    ALLOCATE( PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(CurrentNumConnectedLoops +1) )
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%TotalConnected = CurrentNumConnectedLoops +1
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(1:CurrentNumConnectedLoops) = TemporaryConnectedLoops
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(CurrentNumConnectedLoops+1)%LoopNum  = Loop1Num
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(CurrentNumConnectedLoops+1)%LoopSideNum = Loop1LoopSideNum
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(CurrentNumConnectedLoops+1)%ConnectorTypeOf_Num &
                      = PlantComponentTypeOfNum
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(CurrentNumConnectedLoops+1)%LoopDemandsOnRemote &
                      = Loop2DemandsOnLoop1
    DEALLOCATE(TemporaryConnectedLoops)
  ELSE
    ALLOCATE( PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(1) )
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%TotalConnected = 1
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(1)%LoopNum  = Loop1Num
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(1)%LoopSideNum = Loop1LoopSideNum
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(1)%ConnectorTypeOf_Num   = PlantComponentTypeOfNum
    PlantLoop(Loop2Num)%LoopSide(Loop2LoopSideNum)%Connected(1)%LoopDemandsOnRemote   = Loop2DemandsOnLoop1
  ENDIF


  RETURN

END SUBROUTINE InterConnectTwoPlantLoopSides

SUBROUTINE ShiftPlantLoopSideCallingOrder(OldIndex, NewIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   <April 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! re-arrange the calling order, move one loop side from an old index to a new one

          ! METHODOLOGY EMPLOYED:
          ! move

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant
  USE General, ONLY : RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: OldIndex
  INTEGER, INTENT(IN) :: NewIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (PlantCallingOrderInfoStruct), ALLOCATABLE, DIMENSION(:) :: TempPlantCallingOrderInfo
  TYPE (PlantCallingOrderInfoStruct) :: RecordToMoveInPlantCallingOrderInfo

  IF (OldIndex == 0)  THEN
    CALL ShowSevereError('ShiftPlantLoopSideCallingOrder: developer error notice of invalid index, Old Index=0')
  ENDIF
  IF (NewIndex == 0)  THEN
    CALL ShowSevereError('ShiftPlantLoopSideCallingOrder: developer error notice of invalid index, New Index=1')
  ENDIF
  If ((OldIndex == 0) .or. (NewIndex == 0)) THEN
    RETURN
  ENDIF

  IF (.not. ALLOCATED(TempPlantCallingOrderInfo) )  ALLOCATE ( TempPlantCallingOrderInfo(TotNumHalfLoops))

  ! store copy of prior structure
  TempPlantCallingOrderInfo = PlantCallingOrderInfo

  RecordToMoveInPlantCallingOrderInfo = PlantCallingOrderInfo(OldIndex)

  IF (OldIndex == NewIndex) THEN
    ! do nothing, no shift needed.
  ELSEIF ((OldIndex == 1) .AND.    (NewIndex > OldIndex) .AND. (NewIndex < TotNumHalfLoops)) THEN
   ! example was:      1  2  3  4  5  6  7  8 (with OI = 1, NI = 5)
   ! example shifted:  2  3  4  5  1  6  7  8

    PlantCallingOrderInfo(1:NewIndex -1) = TempPlantCallingOrderInfo(2:NewIndex)
    PlantCallingOrderInfo(NewIndex)      = RecordToMoveInPlantCallingOrderInfo
    PlantCallingOrderInfo(NewIndex+1:TotNumHalfLoops) = TempPlantCallingOrderInfo(NewIndex+1:TotNumHalfLoops)

  ELSEIF ((OldIndex == 1) .AND.    (NewIndex > OldIndex) .AND. (NewIndex == TotNumHalfLoops)) THEN
   ! example was:      1  2  3  4  5  6  7  8 (with OI = 1, NI = 8)
   ! example shifted:  2  3  4  5  6  7  8  1

    PlantCallingOrderInfo(1:NewIndex -1) = TempPlantCallingOrderInfo(2:NewIndex)
    PlantCallingOrderInfo(NewIndex)      = RecordToMoveInPlantCallingOrderInfo
  ELSEIF ((OldIndex > 1) .AND. (NewIndex > OldIndex) .AND. (NewIndex < TotNumHalfLoops) ) THEN
   ! example was:      1  2  3  4  5  6  7  8 (with OI = 3, NI = 6)
   ! example shifted:  1  2  4  5  6  3  7  8
    PlantCallingOrderInfo(1:OldIndex-1)               = TempPlantCallingOrderInfo(1:OldIndex-1)
    PlantCallingOrderInfo(OldIndex:NewIndex-1)        = TempPlantCallingOrderInfo(OldIndex+1:NewIndex)
    PlantCallingOrderInfo(NewIndex)                   = RecordToMoveInPlantCallingOrderInfo
    PlantCallingOrderInfo(NewIndex+1:TotNumHalfLoops) = TempPlantCallingOrderInfo(NewIndex+1:TotNumHalfLoops)
  ELSEIF ((OldIndex > 1) .AND. (NewIndex > OldIndex) .AND. (NewIndex == TotNumHalfLoops) ) THEN
   ! example was:      1  2  3  4  5  6  7  8 (with OI = 3, NI = 8)
   ! example shifted:  1  2  4  5  6  7  8  3
    PlantCallingOrderInfo(1:OldIndex-1)               = TempPlantCallingOrderInfo(1:OldIndex-1)
    PlantCallingOrderInfo(OldIndex:NewIndex-1)        = TempPlantCallingOrderInfo(OldIndex+1:NewIndex)
    PlantCallingOrderInfo(NewIndex)                   = RecordToMoveInPlantCallingOrderInfo
  ELSEIF((OldIndex > 1) .AND.  (NewIndex < OldIndex) .AND. (NewIndex == 1) ) THEN
   ! example was:      1  2  3  4  5  6  7  8 (with OI = 3, NI = 1)
   ! example shifted:  3  1  2  4  5  6  7  8
    PlantCallingOrderInfo(NewIndex)                   = RecordToMoveInPlantCallingOrderInfo
    PlantCallingOrderInfo(NewIndex+1:OldIndex) = TempPlantCallingOrderInfo(1:OldIndex-1)
    PlantCallingOrderInfo(OldIndex+1:TotNumHalfLoops) = TempPlantCallingOrderInfo(OldIndex+1:TotNumHalfLoops)

  ELSEIF((OldIndex > 1) .AND.  (NewIndex < OldIndex) .AND. (NewIndex > 1) ) THEN
   ! example was:      1  2  3  4  5  6  7  8 (with OI = 3, NI = 2)
   ! example shifted:  1  3  2  4  5  6  7  8
    PlantCallingOrderInfo(1:NewIndex-1)   = TempPlantCallingOrderInfo(1:NewIndex-1)
    PlantCallingOrderInfo(NewIndex)       = RecordToMoveInPlantCallingOrderInfo
    PlantCallingOrderInfo(NewIndex+1:OldIndex) = TempPlantCallingOrderInfo(NewIndex:NewIndex +(OldIndex-NewIndex)-1)
    PlantCallingOrderInfo(OldIndex+1:TotNumHalfLoops) = TempPlantCallingOrderInfo(OldIndex+1:TotNumHalfLoops)

  ELSE
   CALL ShowSevereError('ShiftPlantLoopSideCallingOrder: developer error notice, ' &
                        //'caught unexpected logical case in ShiftPlantLoopSideCallingOrder PlantUtilities')

  ENDIF




  RETURN

END SUBROUTINE ShiftPlantLoopSideCallingOrder

SUBROUTINE RegisterPlantCompDesignFlow(ComponentInletNodeNum,DesPlantFlow)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl(previosly SaveCompDesWaterFlow in General.f90)
          !       DATE WRITTEN   January 2004
          !       MODIFIED
          !       RE-ENGINEERED  B. Griffith April 2011, allow to enter repeatedly

          ! PURPOSE OF THIS SUBROUTINE:
          ! Regester the design fluid flow rates of plant components for sizing purposes
          ! in an array that can be accessed by the plant manager routines
          ! allows sizing routines to iterate by safely processing repeated calls from the same component

          ! METHODOLOGY EMPLOYED:
          ! Derived from SaveCompDesWaterFlow but changed to allow re entry with the same node just update
          ! the information at the same location in the structure
          ! The design flow rate is stored in a dynamic structure array along with the plant component's inlet node number
          ! (which is used by plant as a component identifier instead if name and type).


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)   :: ComponentInletNodeNum ! the component's water inlet node number
                                                   ! (condenser side for water / water components)
  REAL(r64), INTENT(IN)   :: DesPlantFlow          ! the component's design fluid volume flow rate [m3/s]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (CompDesWaterFlowData), ALLOCATABLE, DIMENSION(:) :: CompDesWaterFlow0 ! scratch array to store components'
                                                                            ! design water flow rate
  INTEGER :: NumPlantComps
  INTEGER :: PlantCompNum  ! component do loop index
  Logical :: Found
  INTEGER :: thisCallNodeIndex

  NumPlantComps = SaveNumPlantComps

  IF (NumPlantComps == 0) THEN ! first time in, fill and return
    NumPlantComps = 1
    ALLOCATE(CompDesWaterFlow(NumPlantComps))
   ! save the new data
    CompDesWaterFlow(NumPlantComps)%SupNode        = ComponentInletNodeNum
    CompDesWaterFlow(NumPlantComps)%DesVolFlowRate = DesPlantFlow
    SaveNumPlantComps = NumPlantComps
    RETURN
  ENDIF

  Found = .FALSE.
  ! find node num index in structure if any
  Do PlantCompNum = 1, NumPlantComps
    IF (ComponentInletNodeNum == CompDesWaterFlow(PlantCompNum)%SupNode) THEN
      Found = .TRUE.
      thisCallNodeIndex = PlantCompNum
    ENDIF
    IF (Found) EXIT
  ENDDO

  IF (.NOT. Found) THEN ! grow structure and add new node at the end
    NumPlantComps = NumPlantComps + 1 ! increment the number of components that use water as a source of heat or coolth
  ! save the existing data in a scratch array
    ALLOCATE(CompDesWaterFlow0(NumPlantComps-1))
    CompDesWaterFlow0(1:NumPlantComps-1)%SupNode        = CompDesWaterFlow(1:NumPlantComps-1)%SupNode
    CompDesWaterFlow0(1:NumPlantComps-1)%DesVolFlowRate = CompDesWaterFlow(1:NumPlantComps-1)%DesVolFlowRate

  ! get rid of the old array
    DEALLOCATE(CompDesWaterFlow)
    ! allocate a new array
    ALLOCATE(CompDesWaterFlow(NumPlantComps))
    ! save the new data
    CompDesWaterFlow(NumPlantComps)%SupNode        = ComponentInletNodeNum
    CompDesWaterFlow(NumPlantComps)%DesVolFlowRate = DesPlantFlow
    ! move the old data back from the scratch array

    CompDesWaterFlow(1:NumPlantComps-1)%SupNode        = CompDesWaterFlow0(1:NumPlantComps-1)%SupNode
    CompDesWaterFlow(1:NumPlantComps-1)%DesVolFlowRate = CompDesWaterFlow0(1:NumPlantComps-1)%DesVolFlowRate

    DEALLOCATE(CompDesWaterFlow0)
    SaveNumPlantComps = NumPlantComps

  ELSE

    CompDesWaterFlow(thisCallNodeIndex)%SupNode        = ComponentInletNodeNum
    CompDesWaterFlow(thisCallNodeIndex)%DesVolFlowRate = DesPlantFlow

  ENDIF


  RETURN

END SUBROUTINE RegisterPlantCompDesignFlow

SUBROUTINE SafeCopyPlantNode( InletNodeNum, OutletNodeNum, LoopNum, OutletTemp )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.  Griffith
          !       DATE WRITTEN   February, 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provide a safer alternative for Node(outlet) = Node(inlet)
          ! Intended just for plant

          ! METHODOLOGY EMPLOYED:
          ! Copy over state variables but not setpoints
          ! derived from adiabatic Pipes
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode , ONLY : Node
  USE DataPlant,     ONLY : PlantLoop

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(IN) :: InletNodeNum
  INTEGER , INTENT(IN) :: OutletNodeNum
  INTEGER , INTENT(IN) , OPTIONAL :: LoopNum
  REAL(r64), INTENT(IN), OPTIONAL :: OutletTemp !set on outlet node if present and water.

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Node(OutletNodeNum)%FluidType            = Node(InletNodeNum)%FluidType

  Node(OutletNodeNum)%Temp                 = Node(InletNodeNum)%Temp
  Node(OutletNodeNum)%MassFlowRate         = Node(InletNodeNum)%MassFlowRate
  Node(OutletNodeNum)%Quality              = Node(InletNodeNum)%Quality
  Node(OutletNodeNum)%Enthalpy             = Node(InletNodeNum)%Enthalpy  ! should have routines that keep this current with temp?

  Node(OutletNodeNum)%TempMin              = Node(InletNodeNum)%TempMin
  Node(OutletNodeNum)%TempMax              = Node(InletNodeNum)%TempMax
!DSU3 not don't do this, upstream components outlet might stomp on this components inlet
!  so don't propagate hardware limits downstream.  Node(OutletNodeNum)%MassFlowRateMin      = Node(InletNodeNum)%MassFlowRateMin
!DSU3 not don't do this                            Node(OutletNodeNum)%MassFlowRateMax      = Node(InletNodeNum)%MassFlowRateMax
! DSU3 hopefully these next two go away once changes are broadly implemented...
  Node(OutletNodeNum)%MassFlowRateMinAvail = MAX(Node(InletNodeNum)%MassFlowRateMin, Node(InletNodeNum)%MassFlowRateMinAvail)
  Node(OutletNodeNum)%MassFlowRateMaxAvail = MIN(Node(InletNodeNum)%MassFlowRateMax, Node(InletNodeNum)%MassFlowRateMaxAvail)

  Node(OutletNodeNum)%HumRat               = Node(InletNodeNum)%HumRat ! air only?

  !Only pass pressure if we aren't doing a pressure simulation
  IF (Present(LoopNum)) THEN
    IF (PlantLoop(LoopNum)%PressureSimType > 1) THEN
      !Don't do anything
    ELSE
      Node(OutletNodeNum)%Press              = Node(InletNodeNum)%Press
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE SafeCopyPlantNode

REAL(r64) FUNCTION BoundValueToNodeMinMaxAvail(ValueToBound, NodeNumToBoundWith) RESULT(BoundedValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provides a clean way to quickly bound a generic value to within any node's minavail and maxavail range

          ! METHODOLOGY EMPLOYED:
          ! Bound up to min avail, down to max avail

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY : Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: ValueToBound
  INTEGER,   INTENT(IN) :: NodeNumToBoundWith

  BoundedValue = ValueToBound
  BoundedValue = MAX(BoundedValue, Node(NodeNumToBoundWith)%MassFlowRateMinAvail)
  BoundedValue = MIN(BoundedValue, Node(NodeNumToBoundWith)%MassFlowRateMaxAvail)

 RETURN

END FUNCTION BoundValueToNodeMinMaxAvail

SUBROUTINE TightenNodeMinMaxAvails(NodeNum, NewMinAvail, NewMaxAvail)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   January, 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provides a means of tightening up min/max avail on a node if possible

          ! METHODOLOGY EMPLOYED:
          ! Bring up node min avail to new min avail if it doesn't violate any other node conditions
          ! Pull down node max avail to new max avail if it doesn't violate any other node conditions
          ! Assumes that current min/max avails are already honoring hardware min/max values, so they aren't checked here

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode , ONLY : Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: NodeNum
  REAL(r64), INTENT(IN) :: NewMinAvail
  REAL(r64), INTENT(IN) :: NewMaxAvail

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: OldMinAvail
  REAL(r64) :: OldMaxAvail

  OldMinAvail = Node(NodeNum)%MassFlowRateMinAvail
  OldMaxAvail = Node(NodeNum)%MassFlowRateMaxAvail

  ! If the new min avail is higher than previous, and it isn't higher than the max avail, update MIN AVAIL
  IF ((NewMinAvail .GT. OldMinAvail) .AND. (NewMinAvail .LE. OldMaxAvail)) Node(NodeNum)%MassFlowRateMinAvail = NewMinAvail

  ! If the new max avail is lower than previous, and it isn't lower than the min avail, update MAX AVAIL
  IF ((NewMaxAvail .LT. OldMaxAvail) .AND. (NewMaxAvail .GE. OldMinAvail)) Node(NodeNum)%MassFlowRateMaxAvail = NewMaxAvail


END SUBROUTINE

REAL(r64) FUNCTION BoundValueToWithinTwoValues(ValueToBound, LowerBound, UpperBound) RESULT(BoundedValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provides a clean way to quickly bound a generic value to within any two other values

          ! METHODOLOGY EMPLOYED:
          ! Bound up to min and down to max

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY : Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: ValueToBound
  REAL(r64), INTENT(IN) :: LowerBound
  REAL(r64), INTENT(IN) :: UpperBound

  BoundedValue = ValueToBound
  BoundedValue = MAX(BoundedValue, LowerBound)
  BoundedValue = MIN(BoundedValue, UpperBound)

 RETURN

END FUNCTION BoundValueToWithinTwoValues

LOGICAL FUNCTION IntegerIsWithinTwoValues(ValueToCheck, LowerBound, UpperBound) RESULT(ValueIsBetween)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provides a clean way to quickly check if an integer is within two values

          ! METHODOLOGY EMPLOYED:
          ! TRUE if ValueToCheck = [LowerBound, UpperBound]
          ! in other words, it returns true if ValueToCheck=LowerBound, or if ValueToCheck=UpperBound

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ValueToCheck
  INTEGER, INTENT(IN) :: LowerBound
  INTEGER, INTENT(IN) :: UpperBound

  ValueIsBetween = (ValueToCheck.GE.LowerBound).AND.(ValueToCheck.LE.UpperBound)

 RETURN

END FUNCTION IntegerIsWithinTwoValues

SUBROUTINE LogPlantConvergencePoints(FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine stores the history of the plant convergence to check for stuck (max iteration) conditions

          ! METHODOLOGY EMPLOYED:
          ! Loop across all loops and loopsides
          !   On first hvac, reset the history arrays to begin anew
          !   Pick up the loopside inlet and outlet temp and flow rate
          !   Store this in the history array of each node using EOSHIFT

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant
  USE DataLoopNode

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ThisLoopNum
  INTEGER :: ThisLoopSide
  INTEGER :: InletNodeNum
  INTEGER :: OutletNodeNum
  REAL(r64) :: InletNodeTemp
  REAL(r64) :: InletNodeMdot
  REAL(r64) :: OutletNodeTemp
  REAL(r64) :: OutletNodeMdot

  DO ThisLoopNum = 1, SIZE(PlantLoop)
    DO ThisLoopSide = 1, SIZE(PlantLoop(ThisLoopNum)%LoopSide)

      IF (FirstHVACIteration) THEN
        PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory =   0.0d0
        PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory =  0.0d0
        PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory =  0.0d0
        PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory = 0.0d0
      END IF

      InletNodeNum  = PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%NodeNumIn
      InletNodeTemp = Node(InletNodeNum)%Temp
      InletNodeMdot = Node(InletNodeNum)%MassFlowRate

      OutletNodeNum  = PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%NodeNumOut
      OutletNodeTemp = Node(OutletNodeNum)%Temp
      OutletNodeMdot = Node(OutletNodeNum)%MassFlowRate

      PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory = &
        EOSHIFT(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory, -1, InletNodeTemp)

      PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory = &
        EOSHIFT(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory, -1, InletNodeMdot)

      PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory = &
        EOSHIFT(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory, -1, OutletNodeTemp)

      PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory = &
        EOSHIFT(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory, -1, OutletNodeMdot)

    END DO
  END DO

END SUBROUTINE LogPlantConvergencePoints

LOGICAL FUNCTION CheckPlantConvergence(ThisLoopNum, ThisLoopSide, FirstHVACIteration) RESULT(Converged)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine checks the history values in the convergence arrays of this loop/loopside combination

          ! METHODOLOGY EMPLOYED:
          ! On FirstHVAC, we are not converged yet, thus forcing at least two iterations
          ! Calculate the average of each related variable history (generalized: could be any number of history terms)
          ! If any of the history terms do not match this average, then at least one value is different, so not converged
          ! Although this routine appears to check for convergence, it is also used to check for stuck (max iteration) conditions
          !  in cases where demand side (air loop, for example) equipment is "fighting" with the plant loop
          ! The result of this routine can help the plant "lock-in" and take action to stop the iteration

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant
  USE DataLoopNode

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ThisLoopNum, ThisLoopSide
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: InletAvgTemp
  REAL(r64) :: InletAvgMdot
  REAL(r64) :: OutletAvgTemp
  REAL(r64) :: OutletAvgMdot

  IF (FirstHVACIteration) THEN
    Converged = .FALSE.
    RETURN
  END IF

  InletAvgTemp  = SUM(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory)   / &
                 SIZE(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory)

  IF(ANY(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%TemperatureHistory .NE. InletAvgTemp)) THEN
    Converged = .FALSE.
    RETURN
  END IF

  InletAvgMdot  = SUM(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory)  / &
                 SIZE(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory)

  IF(ANY(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%InletNode%MassFlowRateHistory .NE. InletAvgMdot)) THEN
    Converged = .FALSE.
    RETURN
  END IF

  OutletAvgTemp = SUM(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory)  / &
                 SIZE(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory)

  IF(ANY(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%TemperatureHistory .NE. OutletAvgTemp)) THEN
    Converged = .FALSE.
    RETURN
  END IF

  OutletAvgMdot = SUM(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory) / &
                 SIZE(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory)

  IF(ANY(PlantLoop(ThisLoopNum)%LoopSide(ThisLoopSide)%OutletNode%MassFlowRateHistory .NE. OutletAvgMdot)) THEN
    Converged = .FALSE.
    RETURN
  END IF

  !If we made it this far, we're good!
  Converged = .TRUE.

 RETURN

END FUNCTION CheckPlantConvergence

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

END MODULE PlantUtilities
