MODULE BranchNodeConnections

          ! Module containing the routines dealing with the Branch/Node Connections (CompSets, etc)

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module encapsulates the connection data necessary for some of the checks
          ! needed in the branch-node data

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataGlobals, ONLY: MaxNameLength,outputfiledebug
USE DataInterfaces, ONLY: ShowFatalError, ShowWarningError, ShowSevereError, ShowMessage, ShowContinueError
USE DataLoopNode
USE DataBranchNodeConnections


IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: Blank=' '

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE
PUBLIC  SetUpCompSets
PUBLIC  TestCompSet
PUBLIC  RegisterNodeConnection
PUBLIC  CheckNodeConnections
PUBLIC  IsParentObject
PRIVATE WhichParentSet
PUBLIC  GetParentData
PUBLIC  GetNumChildren
PUBLIC  GetComponentData
PUBLIC  GetChildrenData
PUBLIC  TestInletOutletNodes
PUBLIC  TestCompSetInletOutletNodes
PUBLIC  GetNodeConnectionType
PRIVATE FindAllNumbersinList

CONTAINS

SUBROUTINE RegisterNodeConnection(NodeNumber,NodeName,ObjectType,ObjectName,ConnectionType,FluidStream,IsParent,  &
   errFlag,InputFieldName)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine registers a node connection in the Node Connection data structure.  This
          ! structure is intended to help with HVAC diagramming as well as validation of nodes.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString, MakeUPPERCase, FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: NodeNumber     ! Number for this Node
  CHARACTER(len=*), INTENT(IN) :: NodeName       ! Name of this Node
  CHARACTER(len=*), INTENT(IN) :: ObjectType     ! Type of object this Node is connected to (e.g. Chiller:Electric)
  CHARACTER(len=*), INTENT(IN) :: ObjectName     ! Name of object this Node is connected to (e.g. MyChiller)
  CHARACTER(len=*), INTENT(IN) :: ConnectionType ! Connection Type for this Node (must be valid)
  INTEGER, INTENT(IN)          :: FluidStream    ! Count on Fluid Streams
  LOGICAL, INTENT(IN)          :: IsParent       ! True when node is a parent node
  LOGICAL, INTENT(INOUT)       :: errFlag        ! Will be True if errors already detected or if errors found here
  CHARACTER(len=*), INTENT(IN),OPTIONAL :: InputFieldName ! Input Field Name

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='RegisterNodeConnection: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL ErrorsFoundHere
  INTEGER Count
  LOGICAL MakeNew
  INTEGER Found

  ErrorsFoundHere=.false.
  IF (.not. IsValidConnectionType(ConnectionType)) THEN
    CALL ShowSevereError(RoutineName//'Invalid ConnectionType='//TRIM(ConnectionType))
    CALL ShowContinueError('Occurs for Node='//TRIM(NodeName)//', ObjectType='//TRIM(ObjectType)//  &
                           ', ObjectName='//TRIM(ObjectName))
    ErrorsFoundHere=.true.
  ENDIF

  MakeNew=.true.
  DO Count=1,NumOfNodeConnections
    IF (NodeConnections(Count)%NodeNumber /= NodeNumber) CYCLE
    IF (.not. SameString(NodeConnections(Count)%ObjectType,ObjectType)) CYCLE
    IF (.not. SameString(NodeConnections(Count)%ObjectName,ObjectName)) CYCLE
    IF (.not. SameString(NodeConnections(Count)%ConnectionType,ConnectionType)) CYCLE
    IF (NodeConnections(Count)%FluidStream /= FluidStream) CYCLE
    IF ( (NodeConnections(Count)%ObjectIsParent .and. .not. IsParent) .or.  &
        (.not. NodeConnections(Count)%ObjectIsParent .and. IsParent) ) THEN
      CALL ShowSevereError(RoutineName//'Node registered for both Parent and "not" Parent')
      CALL ShowContinueError('Occurs for Node='//TRIM(NodeName)//', ObjectType='//TRIM(ObjectType)//  &
                           ', ObjectName='//TRIM(ObjectName))
      ErrorsFoundHere=.true.
    ENDIF
    MakeNew=.false.
  ENDDO
  IF (MakeNew) THEN
    NumOfNodeConnections=NumOfNodeConnections+1
    IF (NumOfNodeConnections > 1 .and. NumOfNodeConnections > MaxNumOfNodeConnections) THEN
      ALLOCATE(TmpNodeConnections(MaxNumOfNodeConnections+NodeConnectionAlloc))
      TmpNodeConnections(1:NumOfNodeConnections-1)=NodeConnections(1:NumOfNodeConnections-1)
      DEALLOCATE(NodeConnections)
      ALLOCATE(NodeConnections(MaxNumOfNodeConnections+NodeConnectionAlloc))
      NodeConnections(1:NumOfNodeConnections-1)=TmpNodeConnections(1:NumOfNodeConnections-1)
      DEALLOCATE(TmpNodeConnections)
      MaxNumOfNodeConnections=MaxNumOfNodeConnections+NodeConnectionAlloc
    ELSEIF (NumOfNodeConnections == 1) THEN
      ALLOCATE(NodeConnections(NodeConnectionAlloc))
      MaxNumOfNodeConnections=NodeConnectionAlloc
    ENDIF

    NodeConnections(NumOfNodeConnections)%NodeNumber=NodeNumber
    NodeConnections(NumOfNodeConnections)%NodeName=NodeName
    NodeConnections(NumOfNodeConnections)%ObjectType=MakeUPPERCase(ObjectType)
    NodeConnections(NumOfNodeConnections)%ObjectName=ObjectName
    NodeConnections(NumOfNodeConnections)%ConnectionType=ConnectionType
    NodeConnections(NumOfNodeConnections)%FluidStream=FluidStream
    NodeConnections(NumOfNodeConnections)%ObjectIsParent=IsParent

  ENDIF

  IF (SameString(ObjectType(1:MIN(len_Trim(ObjectType),12)),'AirTerminal:')) THEN
    IF (PRESENT(InputFieldName)) THEN
      NumOfAirTerminalNodes=NumOfAirTerminalNodes+1
      IF (NumOfAirTerminalNodes > 1 .and. NumOfAirTerminalNodes > MaxNumOfAirTerminalNodes) THEN
        ALLOCATE(tmpEqNodeConnections(MaxNumOfAirTerminalNodes))
        tmpEqNodeConnections(1:NumOfAirTerminalNodes-1)=AirTerminalNodeConnections(1:NumOfAirTerminalNodes-1)
        DEALLOCATE(AirTerminalNodeConnections)
        ALLOCATE(AirTerminalNodeConnections(MaxNumOfAirTerminalNodes+EqNodeConnectionAlloc))
        AirTerminalNodeConnections(1:NumOfAirTerminalNodes-1)=tmpEqNodeConnections(1:NumOfAirTerminalNodes-1)
        DEALLOCATE(tmpEqNodeConnections)
        MaxNumOfAirTerminalNodes=MaxNumOfAirTerminalNodes+EqNodeConnectionAlloc
      ELSEIF (NumOfAirTerminalNodes == 1) THEN
        ALLOCATE(AirTerminalNodeConnections(EqNodeConnectionAlloc))
        MaxNumOfAirTerminalNodes=EqNodeConnectionAlloc
      ENDIF

      ! Check out AirTerminal inlet/outlet nodes
      Found=FindItemInList(NodeName,AirTerminalNodeConnections%NodeName,NumOfAirTerminalNodes-1)
      IF (Found /= 0) THEN  ! Nodename already used
        CALL ShowSevereError(RoutineName//trim(ObjectType)//'="'//trim(ObjectName)//'" node name duplicated.')
        CALL ShowContinueError('NodeName="'//trim(NodeName)//'", entered as type='//trim(ConnectionType))
        CALL ShowContinueError('In Field='//trim(InputFieldName))
        CALL ShowContinueError('Already used in '//trim(AirTerminalNodeConnections(Found)%ObjectType)//'="'//  &
           trim(AirTerminalNodeConnections(Found)%ObjectName)//'".')
        CALL ShowContinueError(' as type='//trim(AirTerminalNodeConnections(Found)%ConnectionType)//  &
                                 ', In Field='//trim(AirTerminalNodeConnections(Found)%InputFieldName))
        ErrorsFoundHere=.true.
      ELSE
        AirTerminalNodeConnections(NumOfAirTerminalNodes)%NodeName=NodeName
        AirTerminalNodeConnections(NumOfAirTerminalNodes)%ObjectType=ObjectType
        AirTerminalNodeConnections(NumOfAirTerminalNodes)%ObjectName=ObjectName
        AirTerminalNodeConnections(NumOfAirTerminalNodes)%ConnectionType=ConnectionType
        AirTerminalNodeConnections(NumOfAirTerminalNodes)%InputFieldName=InputFieldName
      ENDIF
    ELSE
      CALL ShowSevereError(RoutineName//trim(ObjectType)//', Developer Error: Input Field Name not included.')
      CALL ShowContinueError('Node names not checked for duplication.')
    ENDIF
  ENDIF

  IF (ErrorsFoundHere) THEN
    errFlag=.true.
  ENDIF

  RETURN

END SUBROUTINE RegisterNodeConnection

FUNCTION IsValidConnectionType(ConnectionType) RESULT(IsValid)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function determines if a connection type is valid.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ConnectionType
  LOGICAL :: IsValid

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Count

  IsValid=.false.
  DO Count=1,NumValidConnectionTypes
    IF (ConnectionType /= ValidConnectionTypes(Count)) CYCLE
    IsValid=.true.
    EXIT
  ENDDO

  RETURN

END FUNCTION IsValidConnectionType

SUBROUTINE CheckNodeConnections(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine processes the node connection data structure looking at:
          !
          ! 1.  In the NodeConnections list, for any node which appears as a sensor or an
          ! actuator, the same node must also appear in the connections list at least once
          ! as a node type which is not sensor or actuator or outsideair.
          !
          ! 2.  In the NodeConnections list, for any node which appears as a setpoint, the
          ! same node must also appear in the connections list at least once as a node type
          ! which is not a setpoint or outsideair.
          !
          ! 3.  Every ZoneInlet must appear as an outlet from something, otherwise it will
          ! do nothing.
          !
          ! 4.  Every ZoneExhaust must appear as an inlet to something,
          ! otherwise it will do nothing.
          !
          ! 5.  Every inlet node should match either an Outlet, ZoneReturn, ZoneExhaust, ReliefAir,
          ! or OutsideAir node.
          !  With the current data structure, when checking inlets:
          !    a)  If an InletNode's object is AirLoopHVAC, CondenserLoop, or PlantLoop, then skip the test.
          !    b)  If an InletNode's object is not one of the above types, it is valid if the
          !        same node name appears as an INLET to an AirLoopHVAC, CondenserLoop, or PlantLoop.
          !
          ! 6.  Any given node can only be an inlet once in the list of Non-Parent Node Connections
          !
          ! 7.  Any given node can only be an outlet once in the list of Non-Parent Node Connections


          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop1
  INTEGER Loop2
  LOGICAL IsValid
  LOGICAL IsInlet
  LOGICAL IsOutlet
  LOGICAL MatchedAtLeastOne
  INTEGER :: ErrorCounter
  INTEGER :: Object
  INTEGER :: StartConnect
  INTEGER :: EndConnect
  INTEGER, ALLOCATABLE, DIMENSION(:) :: FluidStreamInletCount
  INTEGER, ALLOCATABLE, DIMENSION(:) :: FluidStreamOutletCount
  INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeObjects
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: FluidStreamCounts
  INTEGER :: NumObjects
  INTEGER :: MaxFluidStream

  ErrorCounter=0
  !  Check 1 -- check sensor and actuator nodes
  DO Loop1=1,NumOfNodeConnections
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Sensor)) CYCLE
    IsValid=.false.
    DO Loop2=1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Actuator)) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Sensor)) CYCLE
      IsValid=.true.
    ENDDO
    IF (.not. IsValid) THEN
      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
            '", Sensor node did not find a matching node of appropriate type (other than Actuator or Sensor).')
      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
      ErrorCounter=ErrorCounter+1
      ErrorsFound=.true.
    ENDIF
  ENDDO

  DO Loop1=1,NumOfNodeConnections
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Actuator)) CYCLE
    IsValid=.false.
    DO Loop2=1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Actuator)) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Sensor)) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_OutsideAir)) CYCLE
      IsValid=.true.
    ENDDO
    IF (.not. IsValid) THEN
      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
            '", Actuator node did not find a matching node of appropriate type (other than Actuator, Sensor, OutsideAir).')
      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
      ErrorCounter=ErrorCounter+1
      ErrorsFound=.true.
    ENDIF
  ENDDO

  ! Check 2 -- setpoint nodes
  ! Check 2a -- setpoint node must also be an inlet or an outlet (CR8212)
  DO Loop1=1,NumOfNodeConnections
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Setpoint)) CYCLE
    IsValid=.false.
    isInlet=.false.
    isOutlet=.false.
    DO Loop2=1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Setpoint)) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_OutsideAir)) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Inlet)) isInlet=.true.
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Outlet)) isOutlet=.true.
      IsValid=.true.
    ENDDO
    IF (.not. IsValid) THEN
      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
            '", Setpoint node did not find a matching node of appropriate type (other than Setpoint, OutsideAir).')
      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
      ErrorCounter=ErrorCounter+1
      ErrorsFound=.true.
    ENDIF
    IF (.not. isInlet .and. .not. isOutlet) THEN
      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
            '", Setpoint node did not find a matching node of type Inlet or Outlet.')
      CALL ShowContinueError('It appears this node is not part of the HVAC system.')
      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
      ErrorCounter=ErrorCounter+1
!      ErrorsFound=.true.
    ENDIF
  ENDDO

  ! Check 2a -- setpoint node must also be an inlet or an outlet (CR8212)
!  DO Loop1=1,NumOfNodeConnections
!    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Setpoint)) CYCLE
!    IsValid=.false.
!    isInlet=.false.
!    isOutlet=.false.
!    DO Loop2=1, NumOfNodeConnections
!      IF (Loop1 == Loop2) CYCLE
!      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
!      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Inlet)) isInlet=.true.
!      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Outlet)) isOutlet=.true.
!      IF (isInlet .or. isOutlet) EXIT
!    ENDDO
!    IF (.not. isInlet .and. .not. isOutlet) THEN
!      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
!            '", Setpoint node did not find a matching node of type Inlet or Outlet.')
!      CALL ShowContinueError('It appears this node is not part of the HVAC system.')
!      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
!             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
!      ErrorCounter=ErrorCounter+1
!      ErrorsFound=.true.
!    ENDIF
!  ENDDO

  ! Check 3 -- zone inlet nodes -- must be an outlet somewhere
  DO Loop1=1,NumOfNodeConnections
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_ZoneInlet)) CYCLE
    IsValid=.false.
    DO Loop2=1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Outlet)) CYCLE
      IsValid=.true.
    ENDDO
    IF (.not. IsValid) THEN
      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
            '", ZoneInlet node did not find an outlet node.')
      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
      ErrorCounter=ErrorCounter+1
!      ErrorsFound=.true.
    ENDIF
  ENDDO

  ! Check 4 -- zone exhaust nodes -- must be an inlet somewhere
  DO Loop1=1,NumOfNodeConnections
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_ZoneExhaust)) CYCLE
    IsValid=.false.
    DO Loop2=1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Inlet)) CYCLE
      IsValid=.true.
    ENDDO
    IF (.not. IsValid) THEN
      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
            '", ZoneExhaust node did not find a matching inlet node.')
      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
      ErrorCounter=ErrorCounter+1
!      ErrorsFound=.true.
    ENDIF
  ENDDO

  ! Check 5 -- return plenum induced air outlet nodes -- must be an inlet somewhere
  DO Loop1=1,NumOfNodeConnections
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_InducedAir)) CYCLE
    IsValid=.false.
    DO Loop2=1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Inlet)) CYCLE
      IsValid=.true.
    ENDDO
    IF (.not. IsValid) THEN
      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
            '", Return plenum induced air outlet node did not find a matching inlet node.')
      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
      ErrorCounter=ErrorCounter+1
      ErrorsFound=.true.
    ENDIF
  ENDDO

  ! Check 6 -- every inlet should have a matching outlet, zonereturn, zoneexhaust, induced air, reliefair or outsideair
          !    a)  If an InletNode's object is AirLoopHVAC, CondenserLoop, or PlantLoop, then skip the test.
          !    b)  If an InletNode's object is not one of the above types, it is valid if the
          !        same node name appears as an INLET to an AirLoopHVAC, CondenserLoop, or PlantLoop.
  DO Loop1=1,NumOfNodeConnections
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Inlet)) CYCLE
    IF (NodeConnections(Loop1)%ObjectType == 'AIRLOOPHVAC' .or.  &
        NodeConnections(Loop1)%ObjectType == 'CONDENSERLOOP'   .or.  &
        NodeConnections(Loop1)%ObjectType == 'PLANTLOOP'          ) CYCLE
    IsValid=.false.
    MatchedAtLeastOne=.false.
    DO Loop2=1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Outlet)      .or. &
          NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_ZoneReturn)  .or. &
          NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_ZoneExhaust) .or. &
          NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_InducedAir) .or. &
          NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_ReliefAir)   .or. &
          NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_OutsideAir)) THEN
            MatchedAtLeastOne=.true.
            CYCLE
      ENDIF
      IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Inlet) .and.  &
          (NodeConnections(Loop2)%ObjectType == 'AIRLOOPHVAC' .or.  &
           NodeConnections(Loop2)%ObjectType == 'CONDENSERLOOP'   .or.  &
           NodeConnections(Loop2)%ObjectType == 'PLANTLOOP'          ) ) THEN
             MatchedAtLeastOne=.true.
             CYCLE
      ENDIF
      IsValid=.false.
    ENDDO
    IF (.not. IsValid .and. .not. MatchedAtLeastOne) THEN
      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
            '", Inlet node did not find an appropriate matching "outlet" node.')
      CALL ShowContinueError('If this is an outdoor air inlet node, '// &
            'it must be listed in an OutdoorAir:Node or OutdoorAir:NodeList object.')
      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
      ErrorCounter=ErrorCounter+1
!      ErrorsFound=.true.
    ENDIF
  ENDDO

  ! Check 7 -- non-parent inlet nodes -- must never be an inlet more than once
  DO Loop1=1,NumOfNodeConnections
    ! Only non-parent node connections
    IF (NodeConnections(Loop1)%ObjectIsParent) CYCLE
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Inlet)) CYCLE
    DO Loop2=Loop1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop2)%ObjectIsParent) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Inlet)) CYCLE
      IF (NodeConnections(Loop2)%NodeNumber == NodeConnections(Loop1)%NodeNumber) THEN
        CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
              '", The same node appears as a non-parent Inlet node more than once.')
        CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
               ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
        CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop2)%ObjectType)//  &
               ', Name='//TRIM(NodeConnections(Loop2)%ObjectName))
        ErrorCounter=ErrorCounter+1
!        ErrorsFound=.true.
        EXIT
      ENDIF
    ENDDO
  ENDDO

  ! Check 8 -- non-parent outlet nodes -- must never be an outlet more than once
  DO Loop1=1,NumOfNodeConnections
    ! Only non-parent node connections
    IF (NodeConnections(Loop1)%ObjectIsParent) CYCLE
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Outlet)) CYCLE
    ! Skip if DIRECT AIR, because it only has one node which is an outlet, so it dupes the outlet which feeds it
    IF (NodeConnections(Loop1)%ObjectType == 'AIRTERMINAL:SINGLEDUCT:UNCONTROLLED') CYCLE
    IsValid=.true.
    DO Loop2=Loop1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop2)%ObjectIsParent) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_Outlet)) CYCLE
      ! Skip if DIRECT AIR, because it only has one node which is an outlet, so it dupes the outlet which feeds it
      IF (NodeConnections(Loop2)%ObjectType == 'AIRTERMINAL:SINGLEDUCT:UNCONTROLLED') CYCLE
      IF (NodeConnections(Loop2)%NodeNumber == NodeConnections(Loop1)%NodeNumber) THEN
        ! Skip if one of the
        CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
              '", The same node appears as a non-parent Outlet node more than once.')
        CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
               ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
        CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop2)%ObjectType)//  &
               ', Name='//TRIM(NodeConnections(Loop2)%ObjectName))
        ErrorCounter=ErrorCounter+1
!        ErrorsFound=.true.
        EXIT
      ENDIF
    ENDDO
  ENDDO

  ! Check 9 -- nodes of type OutsideAirReference must be registered as NodeConnectionType_OutsideAir
  DO Loop1=1,NumOfNodeConnections
    IF (NodeConnections(Loop1)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_OutsideAirReference)) CYCLE
    IsValid=.false.
    DO Loop2=1, NumOfNodeConnections
      IF (Loop1 == Loop2) CYCLE
      IF (NodeConnections(Loop1)%NodeNumber /= NodeConnections(Loop2)%NodeNumber) CYCLE
      IF (NodeConnections(Loop2)%ConnectionType /= ValidConnectionTypes(NodeConnectionType_OutsideAir)) CYCLE
      IsValid=.true.
      EXIT
    ENDDO
    IF (.not. IsValid) THEN
      CALL ShowSevereError('Node Connection Error, Node="'//TRIM(NodeConnections(Loop1)%NodeName)//  &
            '", Outdoor Air Reference did not find an appropriate "outdoor air" node.')
      CALL ShowContinueError('This node must be listed in an OutdoorAir:Node or OutdoorAir:NodeList '//  &
                             'object in order to set its conditions.')
      CALL ShowContinueError('Reference Object='//TRIM(NodeConnections(Loop1)%ObjectType)//  &
             ', Name='//TRIM(NodeConnections(Loop1)%ObjectName))
      ErrorCounter=ErrorCounter+1
!      ErrorsFound=.true.
    ENDIF
  ENDDO

  ! Check 10 -- fluid streams cannot have multiple inlet/outlet nodes on same component
  !  can have multiple inlets with one outlet or vice versa but cannot have multiple both inlet and outlet
  IF (NumOfNodeConnections > 0) THEN
    MaxFluidStream=MAXVAL(NodeConnections%FluidStream)
    ALLOCATE(FluidStreamInletCount(MaxFluidStream))
    ALLOCATE(FluidStreamOutletCount(MaxFluidStream))
    ALLOCATE(FluidStreamCounts(MaxFluidStream))
    ALLOCATE(NodeObjects(NumOfNodeConnections))
    FluidStreamInletCount=0
    FluidStreamOutletCount=0
    NodeObjects=0
    FluidStreamCounts=.false.
    ! Following code relies on node connections for single object type/name being grouped together
    Object=1
    StartConnect=1
    EndConnect=0
    NumObjects=2
    NodeObjects(1)=1
    DO WHILE (Object < NumOfNodeConnections)
      IF (NodeConnections(Object)%ObjectType /= NodeConnections(Object+1)%ObjectType .or.   &
          NodeConnections(Object)%ObjectName /= NodeConnections(Object+1)%ObjectName) THEN
        EndConnect=Object+1
        NodeObjects(NumObjects)=EndConnect
        IF (Object+1 < NumOfNodeConnections) NumObjects=NumObjects+1
      ENDIF
      Object=Object+1
    ENDDO
    ! NodeObjects now contains each consecutive object...
    DO Object=1,NumObjects-1
      IsValid=.true.
      FluidStreamInletCount=0
      FluidStreamOutletCount=0
      FluidStreamCounts=.false.
      Loop1=NodeObjects(Object)
      IF (NodeConnections(Loop1)%ObjectIsParent) CYCLE
      IF (NodeConnections(Loop1)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Inlet))   &
         FluidStreamInletCount(NodeConnections(Loop1)%FluidStream)=FluidStreamInletCount(NodeConnections(Loop1)%FluidStream)+1
      IF (NodeConnections(Loop1)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Outlet))   &
         FluidStreamOutletCount(NodeConnections(Loop1)%FluidStream)=FluidStreamOutletCount(NodeConnections(Loop1)%FluidStream)+1
      DO Loop2=Loop1+1,NodeObjects(Object+1)-1
        IF (NodeConnections(Loop2)%ObjectIsParent) CYCLE
        IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Inlet))   &
           FluidStreamInletCount(NodeConnections(Loop2)%FluidStream)=FluidStreamInletCount(NodeConnections(Loop2)%FluidStream)+1
        IF (NodeConnections(Loop2)%ConnectionType == ValidConnectionTypes(NodeConnectionType_Outlet))   &
           FluidStreamOutletCount(NodeConnections(Loop2)%FluidStream)=FluidStreamOutletCount(NodeConnections(Loop2)%FluidStream)+1
      ENDDO
      DO Loop2=1,MaxFluidStream
        IF (FluidStreamInletCount(Loop2) > 1 .and. FluidStreamOutletCount(Loop2) > 1) THEN
          IsValid=.false.
          FluidStreamCounts(Loop2)=.true.
        ENDIF
      ENDDO
      IF (.not. IsValid) THEN
        CALL ShowSevereError('(Developer) Node Connection Error, Object='//trim(NodeConnections(Loop1)%ObjectType)//':'//  &
           trim(NodeConnections(Loop1)%ObjectName))
        CALL ShowContinueError('Object has multiple connections on both inlet and outlet fluid streams.')
        DO Loop2=1,MaxFluidStream
          IF (FluidStreamCounts(Loop2))   &
             CALL ShowContinueError('...occurs in Fluid Stream ['//trim(RoundSigDigits(Loop2))//'].')
        ENDDO
        ErrorCounter=ErrorCounter+1
        ErrorsFound=.true.
      ENDIF
    ENDDO
    DEALLOCATE(FluidStreamInletCount)
    DEALLOCATE(FluidStreamOutletCount)
    DEALLOCATE(FluidStreamCounts)
    DEALLOCATE(NodeObjects)
  ENDIF



  NumNodeConnectionErrors=NumNodeConnectionErrors+ErrorCounter
  RETURN

END SUBROUTINE CheckNodeConnections

FUNCTION IsParentObject(ComponentType,ComponentName) RESULT(IsParent)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine determines if a component name is a parent node.

          ! METHODOLOGY EMPLOYED:
          ! Traverses CompSet structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ComponentType
  CHARACTER(len=*), INTENT(IN) :: ComponentName
  LOGICAL                      :: IsParent      ! True if this combination is a parent

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  IsParent=.false.
  DO Loop=1,NumOfNodeConnections
    IF (NodeConnections(Loop)%ObjectType == ComponentType .and.  &
        NodeConnections(Loop)%ObjectName == ComponentName) THEN
      IF (NodeConnections(Loop)%ObjectIsParent) THEN
        IsParent=.true.
      ENDIF
      EXIT
    ENDIF
  ENDDO
  IF (.not. IsParent) THEN
    IsParent=IsParentObjectCompSet(ComponentType,ComponentName)
  ENDIF

  RETURN

END FUNCTION IsParentObject

FUNCTION WhichParentSet(ComponentType,ComponentName) RESULT(WhichOne)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine determines which parent node list (number) for a given component name
          ! and type.

          ! METHODOLOGY EMPLOYED:
          ! Traverses CompSet structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ComponentType
  CHARACTER(len=*), INTENT(IN) :: ComponentName
  INTEGER                      :: WhichOne

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  WhichOne=0
  DO Loop=1,NumOfActualParents
    IF (ParentNodeList(Loop)%CType == ComponentType .and. ParentNodeList(Loop)%CName == ComponentName) THEN
      WhichOne=Loop
      EXIT
    ENDIF
  ENDDO

  RETURN

END FUNCTION WhichParentSet

SUBROUTINE GetParentData(ComponentType,ComponentName,     &
                           InletNodeName,InletNodeNum,                &
                           OutletNodeName,OutletNodeNum,              &
                           ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets node data for a given Parent Component Type and Name Name.

          ! METHODOLOGY EMPLOYED:
          ! Traverses CompSet structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)   :: ComponentType
  CHARACTER(len=*), INTENT(IN)   :: ComponentName
  CHARACTER(len=*)               :: InletNodeName
  INTEGER                        :: InletNodeNum
  CHARACTER(len=*)               :: OutletNodeName
  INTEGER                        :: OutletNodeNum
  LOGICAL, INTENT(INOUT)         :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER Loop
  LOGICAL ErrInObject
  INTEGER Which

  InletNodeName=Blank
  InletNodeNum=0
  OutletNodeName=Blank
  OutletNodeNum=0
  ErrInObject=.false.

  Which=WhichParentSet(ComponentType,ComponentName)
  IF (Which /= 0) THEN
    InletNodeName=ParentNodeList(Which)%InletNodeName
    OutletNodeName=ParentNodeList(Which)%OutletNodeName
    ! Get Node Numbers
    InletNodeNum=FindItemInList(InletNodeName,NodeID(1:NumOfNodes),NumOfNodes)
    OutletNodeNum=FindItemInList(OutletNodeName,NodeID(1:NumOfNodes),NumOfNodes)
!    IF (InletNodeNum == 0 .and. ComponentType /= 'ZONEHVAC:AIRDISTRIBUTIONUNIT') THEN
!      CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
!        ', Component Name='//TRIM(ComponentName))
!      CALL ShowContinueError('..Inlet Node Name, not found='//TRIM(InletNodeName))
!!      ErrInObject=.true.
!    ENDIF
!    IF (OutletNodeNum == 0) THEN
!      CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
!        ', Component Name='//TRIM(ComponentName))
!      CALL ShowContinueError('..Outlet Node Name, not found='//TRIM(OutletNodeName))
!!      ErrInObject=.true.
!    ENDIF
  ELSEIF (IsParentObjectCompSet(ComponentType,ComponentName)) THEN
    Which=WhichCompSet(ComponentType,ComponentName)
    IF (Which /= 0) THEN
      InletNodeName=CompSets(Which)%InletNodeName
      OutletNodeName=CompSets(Which)%OutletNodeName
      InletNodeNum=FindItemInList(InletNodeName,NodeID(1:NumOfNodes),NumOfNodes)
      OutletNodeNum=FindItemInList(OutletNodeName,NodeID(1:NumOfNodes),NumOfNodes)
!      IF (InletNodeNum == 0 .and. ComponentType /= 'ZONEHVAC:AIRDISTRIBUTIONUNIT') THEN
!        CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
!          ', Component Name='//TRIM(ComponentName))
!        CALL ShowContinueError('..Inlet Node Name, not found='//TRIM(InletNodeName))
!  !      ErrInObject=.true.
!      ENDIF
!      IF (OutletNodeNum == 0) THEN
!        CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
!          ', Component Name='//TRIM(ComponentName))
!        CALL ShowContinueError('..Outlet Node Name, not found='//TRIM(OutletNodeName))
!  !      ErrInObject=.true.
!      ENDIF
    ELSE
      ErrInObject=.true.
      CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
        ', Component Name='//TRIM(ComponentName)//' not found.')
    ENDIF
  ELSE
    ErrInObject=.true.
    CALL ShowWarningError('GetParentData: Component Type='//TRIM(ComponentType)//  &
      ', Component Name='//TRIM(ComponentName)//' not found.')
  ENDIF

  IF (ErrInObject) ErrorsFound=.true.

  RETURN

END SUBROUTINE GetParentData

FUNCTION IsParentObjectCompSet(ComponentType,ComponentName) RESULT(IsParent)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine determines if a component name is a parent node.

          ! METHODOLOGY EMPLOYED:
          ! Traverses CompSet structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ComponentType
  CHARACTER(len=*), INTENT(IN) :: ComponentName
  LOGICAL                      :: IsParent      ! True if this combination is a parent

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  IsParent=.false.
  DO Loop=1,NumCompSets
    IF (CompSets(Loop)%ParentCType == ComponentType .and. CompSets(Loop)%ParentCName == ComponentName) THEN
      IsParent=.true.
      EXIT
    ENDIF
  ENDDO

  RETURN

END FUNCTION IsParentObjectCompSet

FUNCTION WhichCompSet(ComponentType,ComponentName) RESULT(WhichOne)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine determines which comp set (number) for a given component name
          ! and type.

          ! METHODOLOGY EMPLOYED:
          ! Traverses CompSet structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ComponentType
  CHARACTER(len=*), INTENT(IN) :: ComponentName
  INTEGER                      :: WhichOne

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  WhichOne=0
  DO Loop=1,NumCompSets
    IF (CompSets(Loop)%CType == ComponentType .and. CompSets(Loop)%CName == ComponentName) THEN
      WhichOne=Loop
      EXIT
    ENDIF
  ENDDO

  RETURN

END FUNCTION WhichCompSet

FUNCTION WhichParentCompSet(ComponentType,ComponentName) RESULT(WhichOne)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine determines which comp set (number) for a given component name
          ! and type.

          ! METHODOLOGY EMPLOYED:
          ! Traverses CompSet structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ComponentType
  CHARACTER(len=*), INTENT(IN) :: ComponentName
  INTEGER                      :: WhichOne

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  WhichOne=0
  DO Loop=1,NumCompSets
    IF (CompSets(Loop)%ParentCType == ComponentType .and. CompSets(Loop)%ParentCName == ComponentName) THEN
      WhichOne=Loop
      EXIT
    ENDIF
  ENDDO

  RETURN

END FUNCTION WhichParentCompSet

FUNCTION GetNumChildren(ComponentType,ComponentName) RESULT(NumChildren)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This routine counts the number of children for a parent Component Set.

          ! METHODOLOGY EMPLOYED:
          ! Traverses CompSet structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ComponentType
  CHARACTER(len=*), INTENT(IN) :: ComponentName
  INTEGER                      :: NumChildren

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

  NumChildren=0
  IF (IsParentObject(ComponentType,ComponentName)) THEN
    DO Loop=1,NumCompSets
      IF (CompSets(Loop)%ParentCType == ComponentType .and. CompSets(Loop)%ParentCName == ComponentName) THEN
        NumChildren=NumChildren+1
      ENDIF
    ENDDO
  ENDIF

  RETURN

END FUNCTION GetNumChildren

SUBROUTINE GetComponentData(ComponentType,ComponentName,IsParent,                        &
                           NumInlets,InletNodeNames,InletNodeNums,InletFluidStreams,     &
                           NumOutlets,OutletNodeNames,OutletNodeNums,OutletFluidStreams, &
                           ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets data for a given Component Type and Name Name.

          ! METHODOLOGY EMPLOYED:
          ! Traverses CompSet structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)               :: ComponentType
  CHARACTER(len=*), INTENT(IN)               :: ComponentName
  LOGICAL, INTENT(INOUT)                     :: IsParent
  INTEGER                                    :: NumInlets
  CHARACTER(len=*),ALLOCATABLE, DIMENSION(:) :: InletNodeNames
  INTEGER, ALLOCATABLE, DIMENSION(:)         :: InletNodeNums
  INTEGER, ALLOCATABLE, DIMENSION(:)         :: InletFluidStreams
  CHARACTER(len=*),ALLOCATABLE, DIMENSION(:) :: OutletNodeNames
  INTEGER                                    :: NumOutlets
  INTEGER, ALLOCATABLE, DIMENSION(:)         :: OutletNodeNums
  INTEGER, ALLOCATABLE, DIMENSION(:)         :: OutletFluidStreams
  LOGICAL, INTENT(INOUT)         :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER Loop
  LOGICAL ErrInObject
  INTEGER Which
!unused1109  LOGICAL FoundObject

  IF (ALLOCATED(InletNodeNames)) DEALLOCATE(InletNodeNames)
  IF (ALLOCATED(InletNodeNums)) DEALLOCATE(InletNodeNums)
  IF (ALLOCATED(InletFluidStreams)) DEALLOCATE(InletFluidStreams)
  IF (ALLOCATED(OutletNodeNames)) DEALLOCATE(OutletNodeNames)
  IF (ALLOCATED(OutletNodeNums)) DEALLOCATE(OutletNodeNums)
  IF (ALLOCATED(OutletFluidStreams)) DEALLOCATE(OutletFluidStreams)

  NumInlets=0
  NumOutlets=0

!  FoundObject=.false.
  IsParent=.false.
  DO Which=1,NumOfNodeConnections
    IF (NodeConnections(Which)%ObjectType /= ComponentType .or. NodeConnections(Which)%ObjectName /= ComponentName) CYCLE
!    FoundObject=.true.
    IF (NodeConnections(Which)%ObjectIsParent) IsParent=.true.
    IF (SameString(NodeConnections(Which)%ConnectionType,'Inlet')) NumInlets=NumInlets+1
    IF (SameString(NodeConnections(Which)%ConnectionType,'Outlet')) NumOutlets=NumOutlets+1
  ENDDO

  ALLOCATE(InletNodeNames(NumInlets))
  ALLOCATE(InletNodeNums(NumInlets))
  ALLOCATE(InletFluidStreams(NumInlets))
  ALLOCATE(OutletNodeNames(NumOutlets))
  ALLOCATE(OutletNodeNums(NumOutlets))
  ALLOCATE(OutletFluidStreams(NumOutlets))

  InletNodeNames=Blank
  InletNodeNums=0
  InletFluidStreams=0
  OutletNodeNames=Blank
  OutletNodeNums=0
  OutletFluidStreams=0
  NumInlets=0
  NumOutlets=0
  ErrInObject=.false.

!  IF (IsParentObject(ComponentType,ComponentName)) THEN
!    IsParent=.true.
!  ENDIF

  DO Which=1,NumOfNodeConnections
    IF (NodeConnections(Which)%ObjectType /= ComponentType .or. NodeConnections(Which)%ObjectName /= ComponentName) CYCLE
    IF (SameString(NodeConnections(Which)%ConnectionType,'Inlet')) THEN
      NumInlets=NumInlets+1
      InletNodeNames(NumInlets)=NodeConnections(Which)%NodeName
      InletNodeNums(NumInlets)=NodeConnections(Which)%NodeNumber
      InletFluidStreams(NumInlets)=NodeConnections(Which)%FluidStream
    ENDIF
    IF (SameString(NodeConnections(Which)%ConnectionType,'Outlet')) THEN
      NumOutlets=NumOutlets+1
      OutletNodeNames(NumOutlets)=NodeConnections(Which)%NodeName
      OutletNodeNums(NumOutlets)=NodeConnections(Which)%NodeNumber
      OutletFluidStreams(NumOutlets)=NodeConnections(Which)%FluidStream
    ENDIF
  ENDDO
  IF (ErrInObject) THEN
    CALL ShowWarningError('GetComponentData: Component Type='//TRIM(ComponentType)//  &
      ', Component Name='//TRIM(ComponentName)//' not found.')
  ENDIF

  IF (ErrInObject) ErrorsFound=.true.

  RETURN

END SUBROUTINE GetComponentData

SUBROUTINE GetChildrenData(ComponentType,ComponentName,NumChildren,   &
                           ChildrenCType,ChildrenCName,               &
                           InletNodeName,InletNodeNum,                &
                           OutletNodeName,OutletNodeNum,              &
                           ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets children data for given parent node.

          ! METHODOLOGY EMPLOYED:
          ! Traverses CompSet structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)   :: ComponentType
  CHARACTER(len=*), INTENT(IN)   :: ComponentName
  INTEGER, INTENT(INOUT)         :: NumChildren
  CHARACTER(len=*), DIMENSION(:) :: ChildrenCType
  CHARACTER(len=*), DIMENSION(:) :: ChildrenCName
  CHARACTER(len=*), DIMENSION(:) :: InletNodeName
  INTEGER, DIMENSION(:)          :: InletNodeNum
  CHARACTER(len=*), DIMENSION(:) :: OutletNodeName
  INTEGER, DIMENSION(:)          :: OutletNodeNum
  LOGICAL, INTENT(INOUT)         :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ChildCType
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ChildCName
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ChildInNodeName
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ChildOutNodeName
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: ChildInNodeNum
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: ChildOutNodeNum
  INTEGER Loop
  INTEGER CountNum
  LOGICAL ErrInObject
  CHARACTER(len=MaxNameLength) :: MatchNodeName
  CHARACTER(len=MaxNameLength) :: ParentInletNodeName
  CHARACTER(len=MaxNameLength) :: ParentOutletNodeName
  INTEGER ParentInletNodeNum
  INTEGER ParentOutletNodeNum
!unused1109  LOGICAL Matched
  INTEGER CountMatchLoop

  ChildrenCType=Blank
  ChildrenCName=Blank
  InletNodeName=Blank
  InletNodeNum=0
  OutletNodeName=Blank
  OutletNodeNum=0
  ErrInObject=.false.

  IF (IsParentObject(ComponentType,ComponentName)) THEN
    NumChildren=GetNumChildren(ComponentType,ComponentName)
    IF (NumChildren == 0) THEN
      CALL ShowWarningError('GetChildrenData: Parent Node has no children, node='//  &
                   TRIM(ComponentType)//':'//TRIM(ComponentName))
    ELSE
      CALL GetParentData(ComponentType,ComponentName,ParentInletNodeName,ParentInletNodeNum,  &
                         ParentOutletNodeName,ParentOutletNodeNum,ErrInObject)
      ALLOCATE(ChildCType(NumChildren))
      ALLOCATE(ChildCName(NumChildren))
      ALLOCATE(ChildInNodeName(NumChildren))
      ALLOCATE(ChildOutNodeName(NumChildren))
      ALLOCATE(ChildInNodeNum(NumChildren))
      ALLOCATE(ChildOutNodeNum(NumChildren))
      ChildCType=Blank
      ChildCName=Blank
      ChildInNodeName=Blank
      ChildOutNodeName=Blank
      ChildInNodeNum=0
      ChildOutNodeNum=0
      CountNum=0
      DO Loop=1,NumCompSets
        IF (CompSets(Loop)%ParentCType == ComponentType .and. CompSets(Loop)%ParentCName == ComponentName) THEN
          CountNum=CountNum+1
          ChildCType(CountNum)=CompSets(Loop)%CType
          ChildCName(CountNum)=CompSets(Loop)%CName
          ChildInNodeName(CountNum)=CompSets(Loop)%InletNodeName
          ChildOutNodeName(CountNum)=CompSets(Loop)%OutletNodeName
          ! Get Node Numbers
          ChildInNodeNum(CountNum)=FindItemInList(ChildInNodeName(CountNum),NodeID(1:NumOfNodes),NumOfNodes)
!          IF (ChildInNodeNum(CountNum) == 0) THEN
!            CALL ShowSevereError('GetChildrenData: Inlet Node not previously assigned, Node='//  &
!                    TRIM(ChildInNodeName(CountNum)))
!            CALL ShowContinueError('..Component='//TRIM(ChildCType(CountNum))//':'//TRIM(ChildCName(CountNum)))
!            CALL ShowContinueError('..Parent Object='//TRIM(ComponentType)//':'//TRIM(ComponentName))
!            ErrInObject=.true.
!          ENDIF
          ChildOutNodeNum(CountNum)=FindItemInList(ChildOutNodeName(CountNum),NodeID(1:NumOfNodes),NumOfNodes)
!          IF (ChildOutNodeNum(CountNum) == 0) THEN
!            CALL ShowSevereError('GetChildrenData: Outlet Node not previously assigned, Node='//  &
!                    TRIM(ChildOutNodeName(CountNum)))
!            CALL ShowContinueError('..Component='//TRIM(ChildCType(CountNum))//':'//TRIM(ChildCName(CountNum)))
!            CALL ShowContinueError('..Parent Object='//TRIM(ComponentType)//':'//TRIM(ComponentName))
!            ErrInObject=.true.
!          ENDIF
        ENDIF
      ENDDO
      IF (CountNum /= NumChildren) THEN
        CALL ShowSevereError('GetChildrenData: Counted nodes not equal to GetNumChildren count')
        ErrInObject=.true.
      ELSE
        ! Children arrays built.  Now "sort" for flow connection order(?)
        MatchNodeName=ParentInletNodeName
        CountNum=0
        CountMatchLoop=0
        DO WHILE (CountMatchLoop < NumChildren)
          CountMatchLoop=CountMatchLoop+1
!          Matched=.false.
          DO Loop=1,NumChildren
            IF (ChildInNodeName(Loop) == MatchNodeName) THEN
              CountNum=CountNum+1
              ChildrenCType(CountNum)=ChildCType(Loop)
              ChildrenCName(CountNum)=ChildCName(Loop)
              InletNodeName(CountNum)=ChildInNodeName(Loop)
              InletNodeNum(CountNum)=ChildInNodeNum(Loop)
              OutletNodeName(CountNum)=ChildOutNodeName(Loop)
              OutletNodeNum(CountNum)=ChildOutNodeNum(Loop)
              ChildInNodeName(Loop)=Blank ! So it won't match anymore
!              Matched=.true.
              MatchNodeName=ChildOutNodeName(Loop)
              EXIT
            ENDIF
          ENDDO
!          IF (.not. Matched .and. MatchNodeName /= blank) THEN
!            IF (CountMatchLoop > 1) THEN
!              CALL ShowSevereError('GetChildrenData: Sorting for flow connection order..'//  &
!                                 'Required Child Node, not matched.  Expected Inlet Node='//  &
!                                 TRIM(MatchNodeName))
!            ELSE
!              CALL ShowSevereError('GetChildrenData: Sorting for 1st node in flow connection order..'//  &
!                                 'Required Child Node, not matched.  Expected Inlet Node='//  &
!                                 TRIM(MatchNodeName))
!            ENDIF
!            CALL ShowContinueError('..Parent Object='//TRIM(ComponentType)//':'//TRIM(ComponentName))
!            ErrInObject=.true.
!          ENDIF
        ENDDO
        IF (MatchNodeName /= ParentOutletNodeName) THEN
          DO Loop=1,NumChildren
            IF (ChildInNodeName(Loop) == Blank) CYCLE
            IF (ChildOutNodeName(Loop) == ParentOutletNodeName) EXIT
!            CALL ShowSevereError('GetChildrenData: Sorting for flow connection order..'//  &
!                                 'Required Child Node, not matched.  Expected (Last) Outlet Node='//  &
!                                 TRIM(MatchNodeName))
!            CALL ShowContinueError('..does not match Parent Outlet Node='//TRIM(ParentOutletNodeName))
!            CALL ShowContinueError('..Parent Object='//TRIM(ComponentType)//':'//TRIM(ComponentName))
            EXIT
!          ErrInObject=.true.
          ENDDO
        ENDIF
        DO Loop=1,NumChildren
          IF (ChildInNodeName(Loop) == Blank) CYCLE
          CountNum=CountNum+1
          ChildrenCType(CountNum)=ChildCType(Loop)
          ChildrenCName(CountNum)=ChildCName(Loop)
          InletNodeName(CountNum)=ChildInNodeName(Loop)
          InletNodeNum(CountNum)=ChildInNodeNum(Loop)
          OutletNodeName(CountNum)=ChildOutNodeName(Loop)
          OutletNodeNum(CountNum)=ChildOutNodeNum(Loop)
        ENDDO
        DEALLOCATE(ChildCType)
        DEALLOCATE(ChildCName)
        DEALLOCATE(ChildInNodeName)
        DEALLOCATE(ChildOutNodeName)
        DEALLOCATE(ChildInNodeNum)
        DEALLOCATE(ChildOutNodeNum)
      ENDIF
    ENDIF
  ELSE
    CALL ShowSevereError('GetChildrenData: Requested Children Data for non Parent Node='//  &
                   TRIM(ComponentType)//':'//TRIM(ComponentName))
    ErrInObject=.true.
  ENDIF

  IF (ErrInObject) ErrorsFound=.true.

  RETURN

END SUBROUTINE GetChildrenData

SUBROUTINE SetUpCompSets(ParentType,ParentName,CompType,CompName,InletNode,OutletNode,Description)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets up "Component Sets" as input in the branch
          ! lists.  These can be used later to verify that the proper names and
          ! inlet/outlet nodes have been input.  This routine assumes that identical
          ! "CompSets" cannot be used in multiple places and issues a warning if they are.
          !
          ! This subroutine also

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase,SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ParentType  ! Parent Object Type
  CHARACTER(len=*), INTENT(IN) :: ParentName  ! Parent Object Name
  CHARACTER(len=*), INTENT(IN) :: CompType    ! Component Type
  CHARACTER(len=*), INTENT(IN) :: CompName    ! Component Name
  CHARACTER(len=*), INTENT(IN) :: InletNode   ! Inlet Node Name
  CHARACTER(len=*), INTENT(IN) :: OutletNode  ! Outlet Node Name
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: Description  ! Description

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE (ComponentListData), ALLOCATABLE, DIMENSION(:)  :: TempCompSets
  CHARACTER(len=MaxNameLength)  :: CompTypeUC    ! Component type in upper case
  CHARACTER(len=MaxNameLength)  :: ParentTypeUC  ! Parent component type in upper case
  INTEGER Count, Count2
  INTEGER Found, Found2

  ParentTypeUC = MakeUPPERCase(ParentType)
  CompTypeUC   = MakeUPPERCase(CompType)
  Found=0

  ! See if Component-Nodes set is already there - should be unique
  ! Try to fill in blanks (passed in as undefined
  DO Count=1,NumCompSets
!    IF (CompTypeUC /= CompSets(Count)%CType .or. CompName /= CompSets(Count)%CName) CYCLE
    IF (CompName /= CompSets(Count)%CName) CYCLE
    IF (CompTypeUC /= 'UNDEFINED') THEN
      IF (CompTypeUC /= CompSets(Count)%CType) CYCLE
    ENDIF
    ! Component name matches, component type matches or is undefined
    IF (InletNode /= 'UNDEFINED') THEN
      IF (Compsets(Count)%InletNodeName /= 'UNDEFINED') THEN
        IF (InletNode /= CompSets(Count)%InletNodeName) CYCLE
      ENDIF
    ENDIF
    IF (OutletNode /= 'UNDEFINED') THEN
      IF (Compsets(Count)%OutletNodeName /= 'UNDEFINED') THEN
        IF (OutletNode /= CompSets(Count)%OutletNodeName) CYCLE
      ENDIF
    ENDIF
    !  See if something undefined and set here
    IF (CompSets(Count)%ParentCType == 'UNDEFINED' .and. CompSets(Count)%ParentCName == 'UNDEFINED') THEN
        ! Assume this is a further definition for this compset
      CompSets(Count)%ParentCType=ParentTypeUC
      CompSets(Count)%ParentCName=ParentName
      IF (PRESENT(Description)) CompSets(Count)%Description=Description
      Found=Count
      EXIT
    ENDIF
  ENDDO
  IF (Found == 0) THEN
    DO Count=1,NumCompSets
      Found=0
      ! Test if inlet node has been used before as an inlet node
      ! If the matching node name does not belong to the parent object, then error
      ! For example a fan may share the same inlet node as the furnace object which is its parent
      IF (InletNode /= CompSets(Count)%InletNodeName) THEN
        CYCLE
      ! If parent type is "UNDEFINED" then no error
      ELSEIF ((ParentTypeUC == 'UNDEFINED') .or. (CompSets(Count)%ParentCType == 'UNDEFINED')) THEN
      ! If node name is "UNDEFINED" then no error
      ELSEIF (InletNode /= 'UNDEFINED') THEN
        ! If the matching node name does not belong to the parent or child object, then error
        ! For example a fan may share the same inlet node as the furnace object which is its parent
        IF ((TRIM(ParentTypeUC) == TRIM(CompSets(Count)%CType)) .and. (TRIM(ParentName) == TRIM(CompSets(Count)%CName))) THEN
          ! OK - The duplicate inlet node belongs to this component's parent
        ELSEIF ((TRIM(CompTypeUC) == TRIM(CompSets(Count)%ParentCType)) .and.  &
                (TRIM(CompName) == TRIM(CompSets(Count)%ParentCName))) THEN
          ! OK - The duplicate inlet node belongs to a child of this component
        ELSE
          ! Due to possibility of grandparents or more, if the matching node name
          ! belongs to a component that appears as a parent, then OK
          Found2=0
          DO Count2=1,NumCompSets
            IF ((TRIM(CompSets(Count)%CType) == TRIM(CompSets(Count2)%ParentCType)) .and. &
                (TRIM(CompSets(Count)%CName) == TRIM(CompSets(Count2)%ParentCName))) Found2=1
            IF ((TRIM(CompTypeUC) == TRIM(CompSets(Count2)%ParentCType)) .and. &
                (TRIM(CompName) == TRIM(CompSets(Count2)%ParentCName))) Found2=1
          ENDDO
          IF (Found2 == 0) THEN
            CALL ShowWarningError  ('Node used as an inlet more than once: '//TRIM(InletNode))
            CALL ShowContinueError ('  Used by     : '//TRIM(CompSets(Count)%ParentCType)//', name='//  &
                                    TRIM(CompSets(Count)%ParentCName))
            CALL ShowContinueError ('  as inlet for: '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
            CALL ShowContinueError ('  and  by     : '//TRIM(ParentTypeUC)//', name='//TRIM(ParentName))
            CALL ShowContinueError ('  as inlet for: '//TRIM(CompTypeUC)//', name='//TRIM(CompName))
          ENDIF
        ENDIF
      ENDIF
      ! Test if outlet node has been used before as an outlet node
      ! If the matching node name does not belong to the parent or child object, then error
      ! For example a fan may share the same outlet node as the furnace object which is its parent
      IF (OutletNode /= CompSets(Count)%OutletNodeName) THEN
        CYCLE
      ! If parent type is "UNDEFINED" then no error
      ELSEIF ((ParentTypeUC == 'UNDEFINED') .or. (CompSets(Count)%ParentCType == 'UNDEFINED')) THEN
      ! If node name is "UNDEFINED" then no error
      ELSEIF (OutletNode /= 'UNDEFINED') THEN
        IF ((TRIM(ParentTypeUC) == TRIM(CompSets(Count)%CType)) .and. (TRIM(ParentName) == TRIM(CompSets(Count)%CName))) THEN
          ! OK - The duplicate outlet node belongs to this component's parent
        ELSEIF ((TRIM(CompTypeUC) == TRIM(CompSets(Count)%ParentCType)) .and.  &
                (TRIM(CompName) == TRIM(CompSets(Count)%ParentCName))) THEN
          ! OK - The duplicate outlet node belongs to a child of this component
        ELSE
          ! Due to possibility of grandparents or more, if the matching node name
          ! belongs to a component that appears as a parent, then OK
          Found2=0
          DO Count2=1,NumCompSets
            IF ((TRIM(CompSets(Count)%CType) == TRIM(CompSets(Count2)%ParentCType)) .and. &
                (TRIM(CompSets(Count)%CName) == TRIM(CompSets(Count2)%ParentCName))) Found2=1
            IF ((TRIM(CompTypeUC) == TRIM(CompSets(Count2)%ParentCType)) .and. &
                (TRIM(CompName) == TRIM(CompSets(Count2)%ParentCName))) Found2=1
          ENDDO
          ! This rule is violated by dual duct units, so let it pass
          IF ((Found2 == 0) .AND. (.not. SameString(CompSets(Count)%CType(1:21),'AirTerminal:DualDuct:')) &
                            .AND. (.not. SameString(CompTypeUC(1:21),'AirTerminal:DualDuct:')) ) THEN
            CALL ShowWarningError  ('Node used as an outlet more than once: '//TRIM(OutletNode))
            CALL ShowContinueError ('  Used by     : '//TRIM(CompSets(Count)%ParentCType)//', name='//   &
                                    TRIM(CompSets(Count)%ParentCName))
            CALL ShowContinueError ('  as outlet for: '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
            CALL ShowContinueError ('  and  by     : '//TRIM(ParentTypeUC)//', name='//TRIM(ParentName))
            CALL ShowContinueError ('  as outlet for: '//TRIM(CompTypeUC)//', name='//TRIM(CompName))
          ENDIF
        ENDIF
      ENDIF
      IF (CompTypeUC /= CompSets(Count)%CType .and. CompTypeUC /= 'UNDEFINED') CYCLE
      IF (CompName /= CompSets(Count)%CName) CYCLE
      Found=Count
      EXIT
    ENDDO
  ENDIF
  IF (Found == 0) THEN
    NumCompSets=NumCompSets+1
    ALLOCATE(TempCompSets(NumCompSets))
    IF (NumCompSets > 1) THEN
      TempCompSets(1:NumCompSets-1)=CompSets
      DEALLOCATE(CompSets)
    ENDIF
    TempCompSets(NumCompSets)%CName=Blank
    TempCompSets(NumCompSets)%CType=Blank
    TempCompSets(NumCompSets)%InletNodeName=Blank
    TempCompSets(NumCompSets)%OutletNodeName=Blank
    TempCompSets(NumCompSets)%ParentCType=Blank
    TempCompSets(NumCompSets)%ParentCName=Blank
    TempCompSets(NumCompSets)%Description='UNDEFINED'
    ALLOCATE(CompSets(NumCompSets))
    CompSets=TempCompSets
    DEALLOCATE(TempCompSets)
    CompSets(NumCompSets)%ParentCType=ParentTypeUC
    CompSets(NumCompSets)%ParentCName=ParentName
    CompSets(NumCompSets)%CType=CompTypeUC
    CompSets(NumCompSets)%CName=CompName
    CompSets(NumCompSets)%InletNodeName=InletNode
    CompSets(NumCompSets)%OutletNodeName=OutletNode
    IF (PRESENT(Description)) THEN
      CompSets(NumCompSets)%Description=Description
    ELSE
      CompSets(NumCompSets)%Description='UNDEFINED'
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE SetUpCompSets

SUBROUTINE TestInletOutletNodes(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine tests the branches to see if a duplicate inlet node
          ! exists under a different name in the sequence; likewise for outlet.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count
  INTEGER Other
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: AlreadyNoted

  ! Test component sets created by branches
  ALLOCATE(AlreadyNoted(NumCompSets))
  AlreadyNoted=.false.
  DO Count=1,NumCompSets
    DO Other=1,NumCompSets
      IF (Count == Other) CYCLE
      IF (CompSets(Count)%InletNodeName /= CompSets(Other)%InletNodeName) CYCLE
      IF (AlreadyNoted(Count)) CYCLE
      !  All other values must match
      IF (CompSets(Count)%CType /= CompSets(Other)%CType .or.   &
          CompSets(Count)%CName /= CompSets(Other)%CName .or.   &
          CompSets(Count)%OutletNodeName /= CompSets(Other)%OutletNodeName) THEN
        AlreadyNoted(Other)=.true.
        CALL ShowWarningError  ('Node used as an inlet more than once: '//TRIM(CompSets(Count)%InletNodeName))
        CALL ShowContinueError ('  Used by     : '//TRIM(CompSets(Count)%ParentCType)//', name='//TRIM(CompSets(Count)%ParentCName))
        CALL ShowContinueError ('  as inlet for: '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
        CALL ShowContinueError ('  and  by     : '//TRIM(CompSets(Other)%ParentCType)//', name='//TRIM(CompSets(Other)%ParentCName))
        CALL ShowContinueError ('  as inlet for: '//TRIM(CompSets(Other)%CType)//', name='//TRIM(CompSets(Other)%CName))
!        ErrorsFound=.true.
      ENDIF
    ENDDO
  ENDDO

  AlreadyNoted=.false.
  DO Count=1,NumCompSets
    DO Other=1,NumCompSets
      IF (Count == Other) CYCLE
      IF (CompSets(Count)%OutletNodeName /= CompSets(Other)%OutletNodeName) CYCLE
      IF (AlreadyNoted(Count)) CYCLE
      !  All other values must match
      IF (CompSets(Count)%CType /= CompSets(Other)%CType .or.   &
          CompSets(Count)%CName /= CompSets(Other)%CName .or.   &
          CompSets(Count)%InletNodeName /= CompSets(Other)%InletNodeName) THEN
        AlreadyNoted(Other)=.true.
        CALL ShowWarningError  ('Node used as an outlet more than once: '//TRIM(CompSets(Count)%OutletNodeName))
        CALL ShowContinueError ('  Used by      : '//TRIM(CompSets(Count)%ParentCType)//  &
                                ', name='//TRIM(CompSets(Count)%ParentCName))
        CALL ShowContinueError ('  as outlet for: '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
        CALL ShowContinueError ('  and  by      : '//TRIM(CompSets(Other)%ParentCType)//  &
                                ', name='//TRIM(CompSets(Other)%ParentCName))
        CALL ShowContinueError ('  as outlet for: '//TRIM(CompSets(Other)%CType)//', name='//TRIM(CompSets(Other)%CName))
!        ErrorsFound=.true.
      ENDIF
    ENDDO
  ENDDO

  DEALLOCATE(AlreadyNoted)

  RETURN

END SUBROUTINE TestInletOutletNodes

SUBROUTINE TestCompSet(CompType,CompName,InletNode,OutletNode,Description)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   November 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Register a child component in the CompSets data structure.
          !
          ! NOTE:  This function was originally designed to test the stored "Component Sets" to
          ! see if there was one of this combination in there.  Thus the name "TestCompSet".
          ! However, this was based on a false assumption that input would always be gotten
          ! first for the parent object, then for the child object.  But this is often not the
          ! case.  Ultimately, the name of this function should be changed or it should be merged
          ! into SetUpCompSets.
          !
          ! Until then, this function does the following:
          !   a)  Search CompSets for this combination of component type, component name,
          !       inlet node and outlet node.  If component type/name match and the existing
          !       node names are UNDEFINED, this compset is assumed to be a match.
          !
          !   b)  If found, fill in any missing data such as node names or node description
          !
          !   c)  If not found, call SetUpCompSets (with parent type and name UNDEFINED)
          !       to add a new item in the CompSets array

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType    ! Component Type
  CHARACTER(len=*), INTENT(IN) :: CompName    ! Component Name
  CHARACTER(len=*), INTENT(IN) :: InletNode   ! Inlet Node Name
  CHARACTER(len=*), INTENT(IN) :: OutletNode  ! Outlet Node Name
  CHARACTER(len=*), INTENT(IN) :: Description ! Description of Node Pair (for warning message)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Count
  INTEGER Found
  CHARACTER(len=MaxNameLength)  :: CompTypeUC    ! Component type in upper case

  CompTypeUC   = MakeUPPERCase(CompType)

  ! See if Already there
  Found=0
  DO Count=1,NumCompSets
    IF ((CompTypeUC /= CompSets(Count)%CType) .and. (CompSets(Count)%CType /= 'UNDEFINED')) CYCLE
    IF (CompName /= CompSets(Count)%CName) CYCLE
    IF ((InletNode /= CompSets(Count)%InletNodeName) .and. (CompSets(Count)%InletNodeName /= 'UNDEFINED') &
         .and. (InletNode /= 'UNDEFINED')) CYCLE
    IF ((OutletNode /= CompSets(Count)%OutletNodeName) .and. (CompSets(Count)%OutletNodeName /= 'UNDEFINED') &
         .and. (OutletNode /= 'UNDEFINED')) CYCLE

    Found=Count
    EXIT
  ENDDO

  IF (Found == 0) THEN
    CALL SetUpCompSets('UNDEFINED','UNDEFINED',CompType,CompName,InletNode,OutletNode,Description)
  ELSE
    ! Fill in node names and component type for previously undefined values:
    !   If the parent object did not specify a component type or inlet or outlet node, then that value
    !   is UNDEFINED in CompSets.  When a component calls TestCompSet, the comp type and inlet and
    !   outlet nodes are known, so they can be filled in for future reference.
    IF (CompSets(Found)%CType          == 'UNDEFINED') CompSets(Found)%CType          = CompTypeUC
    IF (CompSets(Found)%InletNodeName  == 'UNDEFINED') CompSets(Found)%InletNodeName  = InletNode
    IF (CompSets(Found)%OutletNodeName == 'UNDEFINED') CompSets(Found)%OutletNodeName = OutletNode
    IF (CompSets(Found)%Description    == 'UNDEFINED') CompSets(Found)%Description    = Description
  ENDIF

  RETURN

END SUBROUTINE TestCompSet

SUBROUTINE TestCompSetInletOutletNodes(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine tests the comp sets to see if a duplicate comp name
          ! exists under a different set of inlet/outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count
  INTEGER Other
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: AlreadyNoted

  ! Test component sets created by branches
  ALLOCATE(AlreadyNoted(NumCompSets))
  AlreadyNoted=.false.
  DO Count=1,NumCompSets
    DO Other=1,NumCompSets
      IF (Count == Other) CYCLE
      IF (CompSets(Count)%CType == 'SOLARCOLLECTOR:UNGLAZEDTRANSPIRED') CYCLE
      IF (CompSets(Count)%CType /= CompSets(Other)%CType .or.   &
          CompSets(Count)%CName /= CompSets(Other)%CName) CYCLE
      IF (CompSets(Count)%Description /= CompSets(Other)%Description) THEN
        IF (CompSets(Count)%Description /= 'UNDEFINED' .and.  &
            CompSets(Other)%Description /= 'UNDEFINED') CYCLE
      ENDIF
      IF (CompSets(Count)%InletNodeName == CompSets(Other)%InletNodeName) CYCLE
      IF (CompSets(Count)%OutletNodeName == CompSets(Other)%OutletNodeName) CYCLE
      IF (AlreadyNoted(Count)) CYCLE
      !  All other values must match
      AlreadyNoted(Other)=.true.
      CALL ShowSevereError   ('Same component name and type has differing Node Names.')
      CALL ShowContinueError ('   Component:    '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
      CALL ShowContinueError ('   Nodes, inlet: '//TRIM(CompSets(Count)%InletNodeName)//', outlet: '//  &
         TRIM(CompSets(Count)%OutletNodeName))
      CALL ShowContinueError (' & Nodes, inlet: '//TRIM(CompSets(Other)%InletNodeName)//', outlet: '//  &
         TRIM(CompSets(Other)%OutletNodeName))
      CALL SHowContinueError ('   Node Types:   '//TRIM(CompSets(Count)%Description)//' & '//TRIM(CompSets(Other)%Description))
      ErrorsFound=.true.
    ENDDO
  ENDDO

!  AlreadyNoted=.false.
!  DO Count=1,NumCompSets
!    DO Other=1,NumCompSets
!      IF (Count == Other) CYCLE
!      IF (CompSets(Count)%InletNodeName /= CompSets(Other)%InletNodeName) CYCLE
!      IF (AlreadyNoted(Count)) CYCLE
!      !  All other values must match
!      IF (CompSets(Count)%ParentCType == 'BRANCH' .or. CompSets(Other)%ParentCType == 'BRANCH') CYCLE
!      IF (CompSets(Count)%Description /= CompSets(Other)%Description) CYCLE
!      IF (CompSets(Count)%CType == CompSets(Other)%CType) THEN
!        AlreadyNoted(Other)=.true.
!        CALL ShowWarningError  ('Node used as an inlet more than once: '//TRIM(CompSets(Count)%InletNodeName))
!        CALL ShowContinueError ('  Used by     : '//TRIM(CompSets(Count)%ParentCType)//  &
!                                                         ', name='//TRIM(CompSets(Count)%ParentCName))
!        CALL ShowContinueError ('  as inlet for: '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
!        CALL ShowContinueError ('  and  by     : '//TRIM(CompSets(Other)%ParentCType)//  &
!                                                         ', name='//TRIM(CompSets(Other)%ParentCName))
!        CALL ShowContinueError ('  as inlet for: '//TRIM(CompSets(Other)%CType)//', name='//TRIM(CompSets(Other)%CName))
!        ErrorsFound=.true.
!      ENDIF
!    ENDDO
!  ENDDO

!  AlreadyNoted=.false.
!  DO Count=1,NumCompSets
!    DO Other=1,NumCompSets
!      IF (Count == Other) CYCLE
!      IF (CompSets(Count)%OutletNodeName /= CompSets(Other)%OutletNodeName) CYCLE
!      IF (AlreadyNoted(Count)) CYCLE
!      !  All other values must match
!      IF (CompSets(Count)%ParentCType == 'BRANCH' .or. CompSets(Other)%ParentCType == 'BRANCH') CYCLE
!      IF (CompSets(Count)%Description /= CompSets(Other)%Description) CYCLE
!      IF (CompSets(Count)%CType /= CompSets(Other)%CType) THEN
!        AlreadyNoted(Other)=.true.
!        CALL ShowWarningError  ('Node used as an outlet more than once: '//TRIM(CompSets(Count)%OutletNodeName))
!        CALL ShowContinueError ('  Used by      : '//TRIM(CompSets(Count)%ParentCType)//  &
!                                ', name='//TRIM(CompSets(Count)%ParentCName))
!        CALL ShowContinueError ('  as outlet for: '//TRIM(CompSets(Count)%CType)//', name='//TRIM(CompSets(Count)%CName))
!        CALL ShowContinueError ('  and  by      : '//TRIM(CompSets(Other)%ParentCType)//  &
!                                ', name='//TRIM(CompSets(Other)%ParentCName))
!        CALL ShowContinueError ('  as outlet for: '//TRIM(CompSets(Other)%CType)//', name='//TRIM(CompSets(Other)%CName))
!        ErrorsFound=.true.
!      ENDIF
!    ENDDO
!  ENDDO

  DEALLOCATE(AlreadyNoted)

  RETURN

END SUBROUTINE TestCompSetInletOutletNodes

SUBROUTINE GetNodeConnectionType(NodeNumber, NodeConnectType, ErrFlag)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Jan 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function provides a connection type with given node number

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,  INTENT(IN)    :: NodeNumber
  LOGICAL,  INTENT(INOUT) :: ErrFlag
  INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeConnectType

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: NodeConnectIndex, NumInList
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ListArray

  IF (ALLOCATED(NodeConnectType)) DEALLOCATE(NodeConnectType)

  CALL FindAllNumbersInList(NodeNumber,NodeConnections%NodeNumber,NumOfNodeConnections, NumInList, ListArray)

  ALLOCATE(NodeConnectType(NumInList))

  IF(NumInList .GT. 0)THEN
    DO NodeConnectIndex = 1, NumInList
      NodeConnectType(NodeConnectIndex) = &
         FindItemInList(NodeConnections(ListArray(NodeConnectIndex))%ConnectionType,ValidConnectionTypes,NumValidConnectionTypes)
    END DO
  ELSE
    IF(NodeNumber .GT. 0)THEN
      CALL ShowWarningError('Node not found = '//TRIM(NodeID(NodeNumber))//'.')
    ELSE
      CALL ShowWarningError('Invalid node number passed = 0.')
    END IF
    ErrFlag = .TRUE.
  END IF

  RETURN

END SUBROUTINE GetNodeConnectionType

SUBROUTINE FindAllNumbersinList(WhichNumber,ListofItems,NumItems, CountOfItems, AllNumbersInList)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   January 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up a number(integer) in a similar list of
          ! items and returns the index of the item in the list, if
          ! found.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WhichNumber
  INTEGER, INTENT(IN), DIMENSION(*) :: ListofItems
  INTEGER, INTENT(IN) :: NumItems

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count         ! Counter for DO loops
  INTEGER CountOfItems  ! Number of items found
  INTEGER, ALLOCATABLE, DIMENSION(:) :: AllNumbersInList ! Index array to all numbers found

  CountOfItems = 0

  IF (ALLOCATED(AllNumbersInList)) DEALLOCATE(AllNumbersInList)

  DO Count=1,NumItems
    IF (WhichNumber == ListofItems(Count)) THEN
      CountOfItems=CountOfItems+1
    ENDIF
  END DO

  IF(CountOfItems .GT. 0)THEN

    ALLOCATE(AllNumbersInList(CountOfItems))
    AllNumbersinList = 0
    CountOfItems = 0

    DO Count=1,NumItems
      IF (WhichNumber == ListofItems(Count)) THEN
        CountOfItems = CountOfItems + 1
        AllNumbersInList(CountOfItems)=Count
      ENDIF
    END DO

  END IF

  RETURN

END SUBROUTINE FindAllNumbersinList


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

END MODULE BranchNodeConnections

