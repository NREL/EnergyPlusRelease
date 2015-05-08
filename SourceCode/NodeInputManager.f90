MODULE NodeInputManager

          ! MODULE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To provide utilities for reading and assigning indices for the
          ! nodes in the HVAC loops.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:


          ! OTHER NOTES:
          !
          !

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, OutputFileBNDetails, DisplayAdvancedReportVariables
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowContinueError, ShowFatalError, &
                       SetupOutputVariable
USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName, MakeUPPERCase, SameString,  &
                           GetObjectDefMaxArgs
USE General, ONLY: TrimSigDigits
USE DataLoopNode
USE BranchNodeConnections

 IMPLICIT NONE         ! Enforce explicit typing of all variables
 PRIVATE

          !MODULE PARAMETER DEFINITIONS
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! DERIVED TYPE DEFINITIONS

   TYPE NodeListDef                  ! Derived Type for Node Lists
     CHARACTER(len=MaxNameLength)   :: Name             =' ' ! Name of this Node List
     INTEGER                        :: NumOfNodesInList = 0  ! Number of Nodes in this Node List
     CHARACTER(len=MaxNameLength),  &
              ALLOCATABLE, DIMENSION(:) :: NodeNames        ! List of Names in this Node List
     INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeNumbers      ! Number of each Node (ref NodeNames) in this Node List
   END TYPE NodeListDef

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  TYPE (NodeListDef), ALLOCATABLE, DIMENSION(:)          :: NodeLists      ! Node Lists

  INTEGER         :: NumOfNodeLists=0                   ! Total number of Node Lists in IDF
  INTEGER         :: NumOfUniqueNodeNames=0             ! Number of Unique Node Names (current)
  ! The following is a module level flag because there are several possible "entries" into
  ! this module that may need to get the Node Inputs.
  LOGICAL         :: GetNodeInputFlag=.true.            ! Flag to Get Node Input(s)
  TYPE (NodeData), ALLOCATABLE, DIMENSION(:)              :: TmpNode    ! Used to "reallocate" Node Structure
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: TmpNodeID  ! Used to "reallocate" name arrays
  TYPE (MarkedNodeData), ALLOCATABLE, DIMENSION(:)        :: TmpMarkedNode   ! Marked nodes must exist somewhere else
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: NodeRef    ! Number of times a Node is "referenced"
  INTEGER, ALLOCATABLE, DIMENSION(:)                      :: TmpNodeRef ! used to reallocate
  CHARACTER(len=MaxNameLength)                            :: CurCheckContextName = ' '  ! Used in Uniqueness checks
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: UniqueNodeNames  ! used in uniqueness checks
  INTEGER         :: NumCheckNodes=0                    ! Num of Unique nodes in check
  INTEGER         :: MaxCheckNodes=0                    ! Current "max" unique nodes in check
  LOGICAL         :: NodeVarsSetup=.false.  ! Setup indicator of node vars for reporting (also that all nodes have been entered)
  LOGICAL, PUBLIC, SAVE,  ALLOCATABLE, DIMENSION(:) :: NodeWetbulbRepReq


PUBLIC   GetNodeNums
PRIVATE  GetNodeList
PUBLIC   SetupNodeVarsForReporting
PUBLIC   GetOnlySingleNode
PUBLIC   InitUniqueNodeCheck
PUBLIC   CheckUniqueNodes
PUBLIC   EndUniqueNodeCheck
PUBLIC   CalcMoreNodeInfo
PUBLIC   MarkNode
PUBLIC   CheckMarkedNodes


CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************

SUBROUTINE GetNodeNums(Name,NumNodes,NodeNumbers,ErrorsFound,NodeFluidType,NodeObjectType,NodeObjectName,  &
                            NodeConnectionType,NodeFluidStream,ObjectIsParent,IncrementFluidStream,InputFieldName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1999
          !       MODIFIED       February 2004, Fluid Type checking/setting
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calls the Node Manager to determine if the
          ! entered name has already been assigned and if it is a list
          ! or if it is a single node.  If it has not been assigned, then
          ! it is a single node and will need to be entered in the Node
          ! data structure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)       :: Name               ! Name for which to obtain information
  INTEGER, INTENT(OUT)               :: NumNodes           ! Number of nodes accompanying this Name
  INTEGER, INTENT(OUT), DIMENSION(:) :: NodeNumbers        ! Node Numbers accompanying this Name
  LOGICAL, INTENT(INOUT)             :: ErrorsFound        ! True when errors are found...
  INTEGER, INTENT(IN)                :: NodeFluidType      ! Fluidtype for checking/setting node FluidType
  CHARACTER(len=*), INTENT(IN)       :: NodeObjectType     ! Node Object Type (i.e. "Chiller:Electric")
  CHARACTER(len=*), INTENT(IN)       :: NodeObjectName     ! Node Object Name (i.e. "MyChiller")
  INTEGER, INTENT(IN)                :: NodeConnectionType ! Node Connection Type (see DataLoopNode)
  INTEGER, INTENT(IN)                :: NodeFluidStream    ! Which Fluid Stream (1,2,3,...)
  LOGICAL, INTENT(IN)                :: ObjectIsParent     ! True/False
  LOGICAL, INTENT(IN), OPTIONAL      :: IncrementFluidStream  ! True/False
  CHARACTER(len=*), INTENT(IN), OPTIONAL      :: InputFieldName  ! Input Field Name

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetNodeNums: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER ThisOne                ! Indicator for this Name
!  CHARACTER(len=20) :: CaseNodeFluidType
  CHARACTER(len=20) :: cNodeFluidType
  CHARACTER(len=32) :: ConnectionType
  INTEGER Loop
  INTEGER FluidStreamNum         ! Fluid stream number passed to RegisterNodeConnection

  IF (GetNodeInputFlag) THEN
    CALL GetNodeListsInput(ErrorsFound)
    GetNodeInputFlag=.false.
  ENDIF

  IF (NodeFluidType /= NodeType_Air .and. NodeFluidType /= NodeType_Water .and. &
      NodeFluidType /= NodeType_Electric .and.NodeFluidType /= NodeType_Steam .and. &
      NodeFluidType /= NodeType_Unknown) THEN
    WRITE(cNodeFluidType,*) NodeFluidType
    cNodeFluidType=ADJUSTL(cNodeFluidType)
    CALL ShowSevereError(RoutineName//trim(NodeObjectType)//'="'//trim(NodeObjectName)//'", invalid fluid type.')
    CALL ShowContinueError('..Invalid FluidType='//TRIM(cNodeFluidType))
    ErrorsFound=.true.
    CALL ShowFatalError('Preceding issue causes termination.')
  ENDIF

  IF (Name /= '  ') THEN
    ThisOne=FindItemInList(Name,NodeLists%Name,NumOfNodeLists)
    IF (ThisOne /= 0) THEN
      NumNodes=NodeLists(ThisOne)%NumOfNodesInList
      NodeNumbers(1:NumNodes)=NodeLists(ThisOne)%NodeNumbers(1:NumNodes)
      DO Loop=1,NumNodes
        IF (NodeFluidType /= NodeType_Unknown .and. Node(NodeNumbers(Loop))%FluidType /= NodeType_Unknown) THEN
         IF (Node(NodeNumbers(Loop))%FluidType /= NodeFluidType) THEN
            CALL ShowSevereError(RoutineName//trim(NodeObjectType)//'="'//trim(NodeObjectName)//'", invalid data.')
            IF (PRESENT(InputFieldName)) CALL ShowContinueError('...Ref field='//trim(InputFieldName))
            CALL ShowContinueError('Existing Fluid type for node, incorrect for request. Node='//TRIM(NodeID(NodeNumbers(Loop))))
            CALL ShowContinueError('Existing Fluid type='//TRIM(ValidNodeFluidTypes(Node(NodeNumbers(Loop))%FluidType))//  &
                                   ', Requested Fluid Type='//TRIM(ValidNodeFluidTypes(NodeFluidType)))
            ErrorsFound=.true.
          ENDIF
        ENDIF
        IF (Node(NodeNumbers(Loop))%FluidType == NodeType_Unknown) THEN
          Node(NodeNumbers(Loop))%FluidType=NodeFluidType
        ENDIF
        NodeRef(NodeNumbers(Loop))=NodeRef(NodeNumbers(Loop))+1
      ENDDO
    ELSE
      ThisOne=AssignNodeNumber(Name,NodeFluidType,ErrorsFound)
      NumNodes=1
      NodeNumbers(1)=ThisOne
    ENDIF
  ELSE
    NumNodes=0
    NodeNumbers(1)=0
  ENDIF

          ! Most calls to this routined use a fixed fluid stream number for all nodes, this is the default
  FluidStreamNum = NodeFluidStream
  DO Loop=1,NumNodes
    IF (NodeConnectionType >= 1 .and. NodeConnectionType <= NumValidConnectionTypes) THEN
      ConnectionType=ValidConnectionTypes(NodeConnectionType)
    ELSE
      ConnectionType=trim(TrimSigDigits(NodeConnectionType))//'-unknown'
    ENDIF
          ! If requested, assign NodeFluidStream to the first node and increment the fluid stream number
          ! for each remaining node in the list
    IF (PRESENT(IncrementFluidStream)) THEN
      IF (IncrementFluidStream) FluidStreamNum = NodeFluidStream + (Loop - 1)
    ENDIF
    CALL RegisterNodeConnection(NodeNumbers(Loop),NodeID(NodeNumbers(Loop)),NodeObjectType,NodeObjectName,  &
                                ConnectionType,FluidStreamNum,ObjectIsParent,ErrorsFound,InputFieldName=InputFieldName)
  ENDDO

  RETURN

END SUBROUTINE GetNodeNums

SUBROUTINE GetNodeList(Name,NumNodes,NodeNumbers,ErrFlag,NodeFluidType,NodeObjectType,NodeObjectName,  &
                                                         NodeConnectionType,NodeFluidStream,ObjectIsParent,InputFieldName)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1999
          !       MODIFIED       February 2003, Error Flag added
          !                      February 2004, Fluid Type
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called when the Get routines are specifically looking
          ! for a Node List.  It should exist.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)       :: Name               ! Node List Name for which information is obtained
  INTEGER, INTENT(OUT)               :: NumNodes           ! Number of nodes accompanying this Name
  INTEGER, INTENT(OUT), DIMENSION(:) :: NodeNumbers        ! NodeNumbers accompanying this Name
  LOGICAL, INTENT(OUT)               :: ErrFlag            ! Set to true when requested Node List not found
  INTEGER, INTENT(IN)                :: NodeFluidType      ! Fluidtype for checking/setting node FluidType
  CHARACTER(len=*), INTENT(IN)       :: NodeObjectType     ! Node Object Type (i.e. "Chiller:Electric")
  CHARACTER(len=*), INTENT(IN)       :: NodeObjectName     ! Node Object Name (i.e. "MyChiller")
  INTEGER, INTENT(IN)                :: NodeConnectionType ! Node Connection Type (see DataLoopNode)
  INTEGER, INTENT(IN)                :: NodeFluidStream    ! Which Fluid Stream (1,2,3,...)
  LOGICAL, INTENT(IN)                :: ObjectIsParent     ! True/False
  CHARACTER(len=*), INTENT(IN), OPTIONAL      :: InputFieldName  ! Input Field Name

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetNodeList: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Try       ! Indicator for this Name

  IF (GetNodeInputFlag) THEN
    CALL GetNodeListsInput(ErrFlag)
    GetNodeInputFlag=.false.
  ENDIF

!  FluidType=NodeFluidType

  NumNodes=0
  NodeNumbers(1)=0
  ErrFlag=.false.

  Try=0
  IF (NumOfNodeLists > 0) THEN
    Try=FindItemInList(Name,NodeLists(1:NumOfNodeLists)%Name,NumOfNodeLists)
  ENDIF

  IF (Try /= 0) THEN
    CALL GetNodeNums(Name,NumNodes,NodeNumbers,ErrFlag,NodeFluidType,NodeObjectType,NodeObjectName,NodeConnectionType,  &
                                                       NodeFluidStream,ObjectIsParent,InputFieldName=InputFieldName)
  ELSE
    ! only valid "error" here is when the Node List is blank
    IF (Name /= Blank) THEN
      CALL ShowSevereError(RoutineName//trim(NodeObjectType)//'="'//trim(NodeObjectName)//'", invalid data.')
      IF (PRESENT(InputFieldName)) CALL ShowContinueError('...Ref field='//trim(InputFieldName))
      CALL ShowContinueError('NodeList not found="'//TRIM(Name)//'".')
      ErrFlag=.true.
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE GetNodeList

SUBROUTINE SetupNodeVarsForReporting

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called when the indicated number of
          ! Nodes have been found (TOTAL NODE NUMBER) or when HVAC warmup is
          ! complete, whichever condition is reached first.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataErrorTracking, ONLY: AbortProcessing  ! used here to determine if this routine called during fatal error processing
  USE DataContaminantBalance, ONLY: Contaminant

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
    INTEGER NumNode                 ! Loop Variable
    INTEGER Count0
    CHARACTER(len=20) ChrOut
    CHARACTER(len=20) ChrOut1
    CHARACTER(len=20) ChrOut2

    IF (.not. NodeVarsSetup) THEN
      IF (.not. AbortProcessing) THEN
        ALLOCATE(MoreNodeInfo(NumOfUniqueNodeNames))
        DO NumNode = 1, NumOfUniqueNodeNames
   ! Setup Report variables for the Nodes for HVAC Reporting, CurrentModuleObject='Node Name'
          CALL SetupOutputVariable('System Node Temperature [C]', Node(NumNode)%Temp,'System','Average',NodeID(NumNode))
          CALL SetupOutputVariable('System Node Mass Flow Rate [kg/s]', &
                                    Node(NumNode)%MassFlowRate,'System','Average',NodeID(NumNode))
          CALL SetupOutputVariable('System Node Humidity Ratio [kgWater/kgDryAir]', Node(NumNode)%HumRat,'System','Average',  &
             NodeID(NumNode))
          CALL SetupOutputVariable('System Node Setpoint Temperature [C]', &
                                    Node(NumNode)%TempSetPoint,'System','Average',NodeID(NumNode))
          CALL SetupOutputVariable('System Node Setpoint High Temperature [C]', Node(NumNode)%TempSetPointHi,'System','Average',  &
                                                                      NodeID(NumNode))
          CALL SetupOutputVariable('System Node Setpoint Low Temperature [C]', Node(NumNode)%TempSetPointLo,'System','Average',  &
                                                                      NodeID(NumNode))
          CALL SetupOutputVariable('System Node Setpoint Humidity Ratio [kgWater/kgDryAir]', Node(NumNode)%HumRatSetPoint,  &
             'System','Average',NodeID(NumNode))
          CALL SetupOutputVariable('System Node Setpoint Minimum Humidity Ratio [kgWater/kgDryAir]', Node(NumNode)%HumRatMin,  &
             'System','Average',NodeID(NumNode))
          CALL SetupOutputVariable('System Node Setpoint Maximum Humidity Ratio [kgWater/kgDryAir]', Node(NumNode)%HumRatMax,  &
             'System','Average',NodeID(NumNode))
          CALL SetupOutputVariable('System Node Relative Humidity [%]', MoreNodeInfo(NumNode)%RelHumidity,'System','Average',  &
                                           NodeID(NumNode))
          CALL SetupOutputVariable('System Node Pressure [Pa]', Node(NumNode)%Press,'System','Average',  &
                                           NodeID(NumNode))
          CALL SetupOutputVariable('System Node Standard Density Volume Flow Rate [m3/s]', &
                                    MoreNodeInfo(NumNode)%VolFlowRateStdRho, 'System',     &
                                   'Average', NodeID(NumNode))
          IF (Node(NumNode)%FluidType == NodeType_Air .OR. Node(NumNode)%FluidType == NodeType_Water) THEN
           ! setup volume flow rate report for actual/current density
            CALL SetupOutputVariable('System Node Current Density Volume Flow Rate [m3/s]', &
                                    MoreNodeInfo(NumNode)%VolFlowRateCrntRho, 'System',     &
                                   'Average', NodeID(NumNode))

            CALL SetupOutputVariable('System Node Current Density [kg/m3]', &
                                    MoreNodeInfo(NumNode)%Density, 'System',     &
                                   'Average', NodeID(NumNode))
          ENDIF

          CALL SetupOutputVariable('System Node Enthalpy [J/kg]', MoreNodeInfo(NumNode)%ReportEnthalpy, 'System', &
                                   'Average', NodeID(NumNode))
          CALL SetupOutputVariable('System Node Wetbulb Temperature [C]', MoreNodeInfo(NumNode)%WetbulbTemp, 'System', &
                                   'Average', NodeID(NumNode))
          CALL SetupOutputVariable('System Node Dewpoint Temperature [C]', MoreNodeInfo(NumNode)%AirDewpointTemp, 'System', &
                                   'Average', NodeID(NumNode))
          CALL SetupOutputVariable('System Node Quality []', Node(NumNode)%Quality,  &
                                   'System','Average',NodeID(NumNode))
          CALL SetupOutputVariable('System Node Height [m]', Node(NumNode)%Height,  &
                                   'System','Average',NodeID(NumNode))
          IF (DisplayAdvancedReportVariables) THEN
            CALL SetupOutputVariable('System Node Minimum Temperature [C]', &
                                      Node(NumNode)%TempMin,'System','Average',NodeID(NumNode))
            CALL SetupOutputVariable('System Node Maximum Temperature [C]', &
                                      Node(NumNode)%TempMax,'System','Average',NodeID(NumNode))
            CALL SetupOutputVariable('System Node Minimum Limit Mass Flow Rate [kg/s]', Node(NumNode)%MassFlowRateMin,  &
                                     'System','Average',NodeID(NumNode))
            CALL SetupOutputVariable('System Node Maximum Limit Mass Flow Rate [kg/s]', Node(NumNode)%MassFlowRateMax,  &
                                     'System','Average',NodeID(NumNode))
            CALL SetupOutputVariable('System Node Minimum Available Mass Flow Rate [kg/s]', Node(NumNode)%MassFlowRateMinAvail,  &
                                     'System','Average',NodeID(NumNode))
            CALL SetupOutputVariable('System Node Maximum Available Mass Flow Rate [kg/s]', Node(NumNode)%MassFlowRateMaxAvail,  &
                                     'System','Average',NodeID(NumNode))
            CALL SetupOutputVariable('System Node Setpoint Mass Flow Rate [kg/s]', Node(NumNode)%MassFlowRateSetPoint,  &
                                     'System','Average',NodeID(NumNode))
            CALL SetupOutputVariable('System Node Requested Mass Flow Rate [kg/s]', Node(NumNode)%MassFlowRateRequest,  &
                                     'System','Average',NodeID(NumNode))
            CALL SetupOutputVariable('System Node Last Timestep Temperature [C]', Node(NumNode)%TempLastTimestep,  &
                                     'System','Average',NodeID(NumNode))
            CALL SetupOutputVariable('System Node Last Timestep Enthalpy [J/kg]', Node(NumNode)%EnthalpyLastTimestep,  &
                                     'System','Average',NodeID(NumNode))


          ENDIF
          IF (Contaminant%CO2Simulation) Then
            CALL SetupOutputVariable('System Node CO2 Concentration [ppm]', Node(NumNode)%CO2,'System', &
                                    'Average',NodeID(NumNode))
          End If
          IF (Contaminant%GenericContamSimulation) Then
            CALL SetupOutputVariable('System Node Generic Air Contaminant Concentration [ppm]', Node(NumNode)%GenContam,'System', &
                                    'Average',NodeID(NumNode))
          End If
        ENDDO
      ENDIF
      NodeVarsSetup=.true.

      WRITE(OutputFileBNDetails,701) '! This file shows details about the branches, nodes, and other'
      WRITE(OutputFileBNDetails,701) '! elements of the flow connections.'
      WRITE(OutputFileBNDetails,701) '! This file is intended for use in "debugging" potential problems'
      WRITE(OutputFileBNDetails,701) '! that may also be detected by the program, but may be more easily'
      WRITE(OutputFileBNDetails,701) '! identified by "eye".'
      WRITE(OutputFileBNDetails,701) '! This file is also intended to support software which draws a'
      WRITE(OutputFileBNDetails,701) '! schematic diagram of the HVAC system.'
      WRITE(OutputFileBNDetails,701) '! ==============================================================='
      ! Show the node names on the Branch-Node Details file
      WRITE(OutputFileBNDetails,700)
      WRITE(ChrOut,*) NumOfUniqueNodeNames
      WRITE(OutputFileBNDetails,701) ' #Nodes,'//ADJUSTL(ChrOut)
      IF (NumOfUniqueNodeNames > 0) THEN
        WRITE(OutputFileBNDetails,702)
      ENDIF
      Count0=0
      DO NumNode = 1, NumOfUniqueNodeNames
        WRITE(ChrOut,*) NumNode
        ChrOut=ADJUSTL(ChrOut)
        WRITE(ChrOut1,*) NodeRef(NumNode)
        ChrOut1=ADJUSTL(ChrOut1)
        ChrOut2=ValidNodeFluidTypes(Node(NumNode)%FluidType)
        WRITE(OutputFileBNDetails,701) ' Node,'//TRIM(ChrOut)//','//TRIM(NodeID(NumNode))//','//  &
                                                 TRIM(ChrOut2)//','//TRIM(ChrOut1)
        IF (NodeRef(NumNode) == 0) Count0=Count0+1
      ENDDO
      ! Show suspicious node names on the Branch-Node Details file
      IF (Count0 > 0) THEN
        WRITE(OutputFileBNDetails,701) '! ==============================================================='
        WRITE(OutputFileBNDetails,701) '! Suspicious nodes have 0 references.  It is normal for some nodes, however.'
        WRITE(OutputFileBNDetails,701) '! Listing nodes with 0 references (culled from previous list):'
        WRITE(OutputFileBNDetails,703)
        DO NumNode = 1, NumOfUniqueNodeNames
          IF (NodeRef(NumNode) > 0) CYCLE
          WRITE(ChrOut,*) NumNode
          ChrOut=ADJUSTL(ChrOut)
          WRITE(ChrOut1,*) NodeRef(NumNode)
          ChrOut1=ADJUSTL(ChrOut1)
          ChrOut2=ValidNodeFluidTypes(Node(NumNode)%FluidType)
          WRITE(OutputFileBNDetails,701) ' Suspicious Node,'//TRIM(ChrOut)//','//TRIM(NodeID(NumNode))//','//  &
                                                   TRIM(ChrOut2)//','//TRIM(ChrOut1)
        ENDDO
      ENDIF
    ENDIF

 700 FORMAT('! #Nodes,<Number of Unique Nodes>')
 701 FORMAT(A)
 702 FORMAT('! <Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>')
 703 FORMAT('! <Suspicious Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>')

  RETURN

END SUBROUTINE SetupNodeVarsForReporting

SUBROUTINE GetNodeListsInput(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the Node Lists from the IDF and fills the
          ! Node List Data Structure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(OUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetNodeListsInput: '
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='NodeList'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop                    ! Loop Variable
  INTEGER Loop1                   ! Loop Variable
  INTEGER Loop2                   ! Loop Variable
  INTEGER :: NumAlphas            ! Number of alphas in IDF item
  INTEGER :: NumNumbers           ! Number of numerics in IDF item
  INTEGER :: IOStatus             ! IOStatus for IDF item (not checked)
  INTEGER :: NCount               ! Actual number of node lists
  LOGICAL :: IsNotOK              ! Flag to verify name
  LOGICAL :: IsBlank              ! Flag for blank name
  LOGICAL :: flagError            ! true when error node list name should be output
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphas
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: rNumbers

  ErrorsFound=.false.
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NCount,NumAlphas,NumNumbers)
  ALLOCATE(cAlphas(NumAlphas))
  ALLOCATE(rNumbers(NumNumbers))
  NumOfNodeLists=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(NodeLists(NumOfNodeLists))
  IF (NumOfNodeLists >0) THEN
    NodeLists(1:NumOfNodeLists)%Name=' '
    NodeLists(1:NumOfNodeLists)%NumOfNodesInList=0
  ENDIF

  NCount=0
  DO Loop=1,NumOfNodeLists
    CALL GetObjectItem(CurrentModuleObject,Loop,cAlphas,NumAlphas,rNumbers,NumNumbers,IOStatus)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphas(1),NodeLists%Name,NCount,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      CYCLE
    ENDIF
    NCount=NCount+1
    NodeLists(NCount)%Name=cAlphas(1)
    ALLOCATE(NodeLists(NCount)%NodeNames(NumAlphas-1))
    NodeLists(NCount)%NodeNames=' '
    ALLOCATE(NodeLists(NCount)%NodeNumbers(NumAlphas-1))
    NodeLists(NCount)%NodeNumbers=0
    NodeLists(NCount)%NumOfNodesInList=NumAlphas-1
    IF (NumAlphas <= 1) THEN
      IF (NumAlphas == 1) THEN
        CALL ShowSevereError(RoutineName//CurrentModuleObject//'="'//trim(cAlphas(1))//'" does not have any nodes.')
      ELSE
        CALL ShowSevereError(RoutineName//CurrentModuleObject//'=<blank> does not have any nodes or nodelist name.')
      ENDIF
      ErrorsFound=.true.
      CYCLE
    ENDIF
    !  Put all in, then determine unique
    DO Loop1=1,NumAlphas-1
      NodeLists(NCount)%NodeNames(Loop1)=cAlphas(Loop1+1)
      IF (cAlphas(Loop1+1) == blank) THEN
        CALL ShowWarningError(RoutineName//CurrentModuleObject//'="'//trim(cAlphas(1))//'", blank node name in list.')
        NodeLists(NCount)%NumOfNodesInList=NodeLists(NCount)%NumOfNodesInList-1
        IF (NodeLists(NCount)%NumOfNodesInList <= 0) THEN
          CALL ShowSevereError(RoutineName//CurrentModuleObject//'="'//trim(cAlphas(1))//'" does not have any nodes.')
          ErrorsFound=.true.
          EXIT
        ENDIF
        CYCLE
      ENDIF
      NodeLists(NCount)%NodeNumbers(Loop1)=AssignNodeNumber(NodeLists(NCount)%NodeNames(Loop1),NodeType_Unknown,ErrorsFound)
      IF (SameString(NodeLists(NCount)%NodeNames(Loop1),NodeLists(NCount)%Name)) THEN
        CALL ShowSevereError(RoutineName//CurrentModuleObject//'="'//trim(cAlphas(1))//'", invalid node name in list.')
        CALL ShowContinueError('... Node '//trim(TrimSigDigits(Loop1))//' Name="'//trim(cAlphas(Loop1+1))//  &
           '", duplicates NodeList Name.')
        ErrorsFound=.true.
      ENDIF
    ENDDO
    ! Error on any duplicates
    flagError=.true.
    DO Loop1=1,NodeLists(NCount)%NumOfNodesInList
      DO Loop2=Loop1+1,NodeLists(NCount)%NumOfNodesInList
        IF (NodeLists(NCount)%NodeNumbers(Loop1) /= NodeLists(NCount)%NodeNumbers(Loop2)) CYCLE
        IF (flagError) THEN  ! only list nodelist name once
          CALL ShowSevereError(RoutineName//CurrentModuleObject//'="'//trim(cAlphas(1))//'" has duplicate nodes:')
          flagError=.false.
        ENDIF
        CALL ShowContinueError('...list item='//  &
           trim(TrimSigDigits(Loop1))//', "'//trim(NodeID(NodeLists(NCount)%NodeNumbers(Loop1)))//'", duplicate list item='//  &
           trim(TrimSigDigits(Loop2))//', "'//trim(NodeID(NodeLists(NCount)%NodeNumbers(Loop2)))//'".')
        ErrorsFound=.true.
      ENDDO
    ENDDO
  ENDDO

  DO Loop=1,NumOfNodeLists
    DO Loop2=1,NodeLists(Loop)%NumOfNodesInList
      DO Loop1=1,NumOfNodeLists
        IF (Loop == Loop1) CYCLE   ! within a nodelist have already checked to see if node name duplicates nodelist name
        IF (.not. SameString(NodeLists(Loop)%NodeNames(Loop2),NodeLists(Loop1)%Name)) CYCLE
        CALL ShowSevereError(RoutineName//CurrentModuleObject//'="'//trim(NodeLists(Loop1)%Name)//'", invalid node name in list.')
        CALL ShowContinueError('... Node '//trim(TrimSigDigits(Loop2))//' Name="'//  &
           trim(NodeLists(Loop)%NodeNames(Loop2))//'", duplicates NodeList Name.')
        CALL ShowContinueError('... NodeList="'//trim(NodeLists(Loop1)%Name)//'", is duplicated.')
        CALL ShowContinueError('... Items in NodeLists must not be the name of another NodeList.')
        ErrorsFound=.true.
      ENDDO
    ENDDO
  ENDDO

  DEALLOCATE(cAlphas)
  DEALLOCATE(rNumbers)

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//CurrentModuleObject//': Error getting input - causes termination.')
  ENDIF

  RETURN

END SUBROUTINE GetNodeListsInput

INTEGER FUNCTION AssignNodeNumber(Name,NodeFluidType,ErrorsFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function assigns a node number to this name.


          ! METHODOLOGY EMPLOYED:
          ! Look to see if a name has already been entered.  Use the index of
          ! the array as the node number, if there.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Name  ! Name for assignment
  INTEGER, INTENT(IN)          :: NodeFluidType ! must be valid
  LOGICAL, INTENT(INOUT)       :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumNode=0             ! Loop Variable
  CHARACTER(len=25) :: cNodeFluidType =' '

  IF (NodeFluidType /= NodeType_Air .and. NodeFluidType /= NodeType_Water .and. &
      NodeFluidType /= NodeType_Electric .and.NodeFluidType /= NodeType_Steam .and. &
      NodeFluidType /= NodeType_Unknown) THEN
    WRITE(cNodeFluidType,*) NodeFluidType
    cNodeFluidType=ADJUSTL(cNodeFluidType)
    CALL ShowSevereError('AssignNodeNumber: Invalid FluidType='//TRIM(cNodeFluidType))
    ErrorsFound=.true.
    CALL ShowFatalError('AssignNodeNumber: Preceding issue causes termination.')
  ENDIF

  NumNode=0
  IF (NumOfUniqueNodeNames > 0) THEN
    NumNode=FindItemInList(Name,NodeID(1:NumOfUniqueNodeNames),NumOfUniqueNodeNames)
    IF (NumNode > 0) THEN
      AssignNodeNumber=NumNode
      NodeRef(NumNode)=NodeRef(NumNode)+1
      IF (NodeFluidType /= NodeType_Unknown) THEN
        IF (Node(NumNode)%FluidType /= NodeFluidType .and. Node(NumNode)%FluidType /= NodeType_Unknown) THEN
          CALL ShowSevereError('Existing Fluid type for node, incorrect for request. Node='//TRIM(NodeID(NumNode)))
          CALL ShowContinueError('Existing Fluid type='//TRIM(ValidNodeFluidTypes(Node(NumNode)%FluidType))//  &
                                 ', Requested Fluid Type='//TRIM(ValidNodeFluidTypes(NodeFluidType)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      IF (Node(NumNode)%FluidType == NodeType_Unknown) THEN
        Node(NumNode)%FluidType=NodeFluidType
      ENDIF
    ELSE
      NumOfUniqueNodeNames=NumOfUniqueNodeNames+1
      NumOfNodes=NumOfUniqueNodeNames
      ALLOCATE(TmpNode(NumOfNodes))
      ALLOCATE(TmpNodeID(0:NumOfNodes))
      ALLOCATE(TmpNodeRef(NumOfNodes))
      ALLOCATE(TmpMarkedNode(NumOfNodes))

      TmpNode(1:NumOfNodes-1)=Node(1:NumOfNodes-1)
      TmpNodeID(0:NumOfNodes-1)=NodeID(0:NumOfNodes-1)
      TmpNodeRef(1:NumOfNodes-1)=NodeRef(1:NumOfNodes-1)
      TmpMarkedNode(1:NumOfNodes-1)=MarkedNode(1:NumOfNodes-1)

      DEALLOCATE(Node)
      DEALLOCATE(NodeID)
      DEALLOCATE(NodeRef)
      DEALLOCATE(MarkedNode)
      ALLOCATE(Node(NumOfNodes))
      ALLOCATE(NodeID(0:NumOfNodes))
      ALLOCATE(NodeRef(NumOfNodes))
      ALLOCATE(MarkedNode(NumOfNodes))
      Node(1:NumOfNodes-1)=TmpNode(1:NumOfNodes-1)
      NodeID(0:NumOfNodes-1)=TmpNodeID(0:NumOfNodes-1)
      NodeRef(1:NumOfNodes-1)=TmpNodeRef(1:NumOfNodes-1)
      MarkedNode(1:NumOfNodes-1)=TmpMarkedNode(1:NumOfNodes-1)
      DEALLOCATE(TmpNode)
      DEALLOCATE(TmpNodeID)
      DEALLOCATE(TmpNodeRef)
      DEALLOCATE(TmpMarkedNode)
      ! Set new item in derived type Node to zero.
      Node(NumOfNodes)%FluidType            =NodeFluidType
      ! Allocate takes care of defining
      NodeID(NumOfNodes)=' '
      NodeRef(NumOfNodes)=0

      NodeID(NumOfUniqueNodeNames)=Name
      AssignNodeNumber=NumOfUniqueNodeNames
    ENDIF
  ELSE
    ALLOCATE(Node(1))
    Node(1)%FluidType            =NodeFluidType
      ! Allocate takes care of defining
    NumOfNodes=1
    ALLOCATE(NodeID(0:1))
    ALLOCATE(NodeRef(1))
    ALLOCATE(MarkedNode(1))

    NumOfUniqueNodeNames=1
    NodeID(0)='Undefined'
    NodeID(NumOfUniqueNodeNames)=Name
    AssignNodeNumber=1
    NodeRef(1)=0
  ENDIF


  RETURN

END FUNCTION AssignNodeNumber

FUNCTION GetOnlySingleNode(NodeName,errFlag,NodeObjectType,NodeObjectName,NodeFluidType,NodeConnectionType,  &
                           NodeFluidStream,ObjectIsParent,InputFieldName) RESULT (GetSingleNodeResult)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie; adapted from GasAbsorptionChiller;Jason Glazer
          !       DATE WRITTEN   December 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function gets a single node (or error message results) using the
          ! node id from the input file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)   :: NodeName
  LOGICAL, INTENT(INOUT)         :: errFlag
  CHARACTER(len=*), INTENT(IN)   :: NodeObjectType     ! Node Object Type (i.e. "Chiller:Electric")
  CHARACTER(len=*), INTENT(IN)   :: NodeObjectName     ! Node Object Name (i.e. "MyChiller")
  INTEGER, INTENT(IN)            :: NodeFluidType      ! Fluidtype for checking/setting node FluidType
  INTEGER, INTENT(IN)            :: NodeConnectionType ! Node Connection Type (see DataLoopNode)
  INTEGER, INTENT(IN)            :: NodeFluidStream    ! Which Fluid Stream (1,2,3,...)
  LOGICAL, INTENT(IN)            :: ObjectIsParent     ! True/False
  INTEGER                        :: GetSingleNodeResult
  CHARACTER(len=*), INTENT(IN), OPTIONAL   :: InputFieldName     ! Input Field Name

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetOnlySingleNode: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER                 :: NumNodes
  INTEGER, ALLOCATABLE, DIMENSION(:), SAVE  :: NodeNums
  INTEGER                 :: FluidType
  CHARACTER(len=32)       :: ConnectionType
  LOGICAL, SAVE :: firsttime=.true.
  INTEGER :: NumParams
  INTEGER :: NumAlphas
  INTEGER :: NumNums

  if (firsttime) then
    CALL GetObjectDefMaxArgs('NodeList',NumParams,NumAlphas,NumNums)
    ALLOCATE(NodeNums(NumParams))
    NodeNums=0
    firsttime=.false.
  endif

  FluidType=NodeFluidType

  CALL GetNodeNums(NodeName,NumNodes,NodeNums,ErrFlag,FluidType,NodeObjectType,NodeObjectName,NodeConnectionType,  &
                   NodeFluidStream,ObjectIsParent,InputFieldName=InputFieldName)

  IF (NumNodes > 1) THEN
    CALL ShowSevereError(RoutineName//trim(NodeObjectType)//'="'//trim(NodeObjectName)//'", invalid data.')
    IF (PRESENT(InputFieldName)) CALL ShowContinueError('...Ref field='//trim(InputFieldName))
    CALL ShowContinueError('Only 1st Node used from NodeList="'//TRIM(NodeName)//'".')
    CALL ShowContinueError('...a Nodelist may not be valid in this context.')
    errFlag=.true.
  ELSEIF (NumNodes == 0) THEN
    NodeNums(1)=0
  ENDIF
  IF (NumNodes > 0) THEN
    IF (NodeConnectionType >= 1 .and. NodeConnectionType <= NumValidConnectionTypes) THEN
      ConnectionType=ValidConnectionTypes(NodeConnectionType)
    ELSE
      ConnectionType=trim(TrimSigDigits(NodeConnectionType))//'-unknown'
    ENDIF
!    CALL RegisterNodeConnection(NodeNums(1),NodeID(NodeNums(1)),NodeObjectType,NodeObjectName,  &
!                                  ConnectionType,NodeFluidStream,ObjectIsParent,errFlag)
  ENDIF

  GetSingleNodeResult = NodeNums(1)

  RETURN

END FUNCTION GetOnlySingleNode

SUBROUTINE InitUniqueNodeCheck(ContextName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine begins a process of checking for unique node names
          ! in a sequence of nodes.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ContextName

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL ErrFlag

  ! Begin set up of Uniqueness context

  IF (GetNodeInputFlag) THEN
    CALL GetNodeListsInput(ErrFlag)
    GetNodeInputFlag=.false.
  ENDIF

  IF (CurCheckContextName /= Blank) THEN
    CALL ShowFatalError('Init Uniqueness called for "'//TRIM(ContextName)//', but checks for "'//TRIM(CurCheckContextName)//  &
                        '" was already in progress.')
  ENDIF
  IF (ContextName == Blank) THEN
    CALL ShowFatalError('Init Uniqueness called with Blank Context Name')
  ENDIF
  IF (ALLOCATED(UniqueNodeNames)) THEN
    DEALLOCATE(UniqueNodeNames)
  ENDIF

  NumCheckNodes=0
  MaxCheckNodes=100
  ALLOCATE(UniqueNodeNames(MaxCheckNodes))
  UniqueNodeNames=Blank
  CurCheckContextName=ContextName

  RETURN

END SUBROUTINE InitUniqueNodeCheck

SUBROUTINE CheckUniqueNodes(NodeTypes,CheckType,ErrorsFound,CheckName,CheckNumber,ObjectName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine checks the appropriate input argument for uniqueness.
          ! Call CheckUniqueNodes(NodeTypes,CheckType,ErrorsFound,CheckName,CheckNumber)
          ! NodeTypes - used in error message (if any produced)
          ! CheckType - "NodeName' or 'NodeNumber' (only 1 can be input per time)
          ! ErrorsFound - true if error found by routine
          ! CheckName - NodeName entered
          ! CheckNumber - Node Number entered
          ! only 1 of CheckName or CheckNumber need be entered.
          ! ObjectName - "Name" field of object (i.e., CurCheckContextName)

          ! METHODOLOGY EMPLOYED:
          ! checks the current list of items for this (again)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: NodeTypes
  CHARACTER(len=*), INTENT(IN) :: CheckType
  LOGICAL, INTENT(INOUT)       :: ErrorsFound
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: CheckName
  INTEGER, INTENT(IN), OPTIONAL :: CheckNumber
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ObjectName

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Found

  SELECT CASE (MakeUPPERCase(CheckType))

    CASE ('NODENAME','NODENAMES','NODE NAME','NODE NAMES')
      IF (.not. PRESENT(CheckName)) THEN
        CALL ShowFatalError('Routine CheckUniqueNodes called with Nodetypes=NodeName, '// &
                            'but did not include CheckName argument.')
      ENDIF
      IF (CheckName /= Blank) THEN
        Found=FindItemInList(CheckName,UniqueNodeNames,NumCheckNodes)
        IF (Found /= 0) THEN
          CALL ShowSevereError(trim(CurCheckContextName)//'="'//trim(ObjectName)//'", duplicate node names found.')
          CALL ShowContinueError('...for Node Type(s)='//trim(NodeTypes)//', duplicate node name="'//trim(CheckName)//'".')
          CALL ShowContinueError('...Nodes must be unique across instances of this object.')
!          CALL ShowSevereError('Node Types='//TRIM(NodeTypes)//', Non Unique Name found='//TRIM(CheckName))
!          CALL ShowContinueError('Context='//TRIM(CurCheckContextName))
          ErrorsFound=.true.
        ELSE
          NumCheckNodes=NumCheckNodes+1
          IF (NumCheckNodes > MaxCheckNodes) THEN
            ALLOCATE(TmpNodeID(MaxCheckNodes+100))
            TmpNodeID=Blank
            TmpNodeID(1:NumCheckNodes-1)=UniqueNodeNames
            DEALLOCATE(UniqueNodeNames)
            MaxCheckNodes=MaxCheckNodes+100
            ALLOCATE(UniqueNodeNames(MaxCheckNodes))
            UniqueNodeNames=TmpNodeID
            DEALLOCATE(TmpNodeID)
          ENDIF
          UniqueNodeNames(NumCheckNodes)=CheckName
        ENDIF
      ENDIF

    CASE ('NODENUMBER','NODENUMBERS','NODE NUMBER','NODE NUMBERS')
      IF (.not. PRESENT(CheckNumber)) THEN
        CALL ShowFatalError('Routine CheckUniqueNodes called with Nodetypes=NodeNumber, '// &
                            'but did not include CheckNumber argument.')
      ENDIF
      IF (CheckNumber /= 0) THEN
        Found=FindItemInList(NodeID(CheckNumber),UniqueNodeNames,NumCheckNodes)
        IF (Found /= 0) THEN
          CALL ShowSevereError(trim(CurCheckContextName)//'="'//trim(ObjectName)//'", duplicate node names found.')
          CALL ShowContinueError('...for Node Type(s)='//trim(NodeTypes)//', duplicate node name="'//trim(CheckName)//'".')
          CALL ShowContinueError('...Nodes must be unique across instances of this object.')
!          CALL ShowSevereError('Node Types='//TRIM(NodeTypes)//', Non Unique Name found='//TRIM(NodeID(CheckNumber)))
!          CALL ShowContinueError('Context='//TRIM(CurCheckContextName))
          ErrorsFound=.true.
        ELSE
          NumCheckNodes=NumCheckNodes+1
          IF (NumCheckNodes > MaxCheckNodes) THEN
            ALLOCATE(TmpNodeID(MaxCheckNodes+100))
            TmpNodeID=Blank
            TmpNodeID(1:NumCheckNodes-1)=UniqueNodeNames
            DEALLOCATE(UniqueNodeNames)
            MaxCheckNodes=MaxCheckNodes+100
            ALLOCATE(UniqueNodeNames(MaxCheckNodes))
            UniqueNodeNames=TmpNodeID
            DEALLOCATE(TmpNodeID)
          ENDIF
          UniqueNodeNames(NumCheckNodes)=NodeID(CheckNumber)
        ENDIF
      ENDIF

    CASE DEFAULT
      CALL ShowFatalError('CheckUniqueNodes called with invalid Check Type='//TRIM(CheckType))
      ErrorsFound=.true.

  END SELECT

  RETURN

END SUBROUTINE CheckUniqueNodes

SUBROUTINE EndUniqueNodeCheck(ContextName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine marks the end of a unique node check.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ContextName

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (CurCheckContextName /= ContextName) THEN
    CALL ShowFatalError('End Uniqueness called for "'//TRIM(ContextName)//', but checks for "'//TRIM(CurCheckContextName)//  &
                        '" was in progress.')
  ENDIF
  IF (ContextName == Blank) THEN
    CALL ShowFatalError('End Uniqueness called with Blank Context Name')
  ENDIF
  CurCheckContextName=Blank
  IF (ALLOCATED(UniqueNodeNames)) THEN
    DEALLOCATE(UniqueNodeNames)
  ENDIF

  RETURN

END SUBROUTINE EndUniqueNodeCheck

SUBROUTINE CalcMoreNodeInfo

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate additional node information for reporting

          ! METHODOLOGY EMPLOYED:
          ! Input is the existing node data plus environment variables. Output is
          ! stored in MoreNodeInfo.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: StdBaroPress,OutBaroPress,StdRhoAir
  USE Psychrometrics,  ONLY: PsyRhoAirFnPbTdbW,RhoH2O,PsyHFnTdbW,CpCw,PsyTwbFnTdbWPb,PsyRhFnTdbWPb,  &
                             PsyTdpFnWPb
  USE DataGlobals ,    ONLY: InitConvTemp
  USE DataInterfaces,  ONLY: ShowWarningError,ShowContinueErrorTimeStamp
  USE OutputProcessor, ONLY: ReqReportVariables,ReqRepVars,NumOfReqVariables
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE FluidProperties, ONLY: GetSatDensityRefrig, GetSatEnthalpyRefrig, GetSpecificHeatGlycol, &
                             GetDensityGlycol, GetGlycolNameByIndex, NumOfGlycols
  USE General,         ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: iNode                      ! node loop index
  INTEGER       :: iReq                       ! requested report variables loop index
  LOGICAL, SAVE :: MyOneTimeFlag = .TRUE.     ! one time flag
  REAL(r64), SAVE    :: RhoAirStdInit
  REAL(r64), SAVE    :: RhoWaterStdInit
  INTEGER, SAVE,  ALLOCATABLE, DIMENSION(:) :: NodeWetbulbSchedPtr
  LOGICAL, SAVE,  ALLOCATABLE, DIMENSION(:) :: NodeRelHumidityRepReq
  INTEGER, SAVE,  ALLOCATABLE, DIMENSION(:) :: NodeRelHumiditySchedPtr
  LOGICAL, SAVE,  ALLOCATABLE, DIMENSION(:) :: NodeDewpointRepReq
  INTEGER, SAVE,  ALLOCATABLE, DIMENSION(:) :: NodeDewpointSchedPtr
  LOGICAL       :: ReportWetbulb
  LOGICAL       :: ReportRelHumidity
  LOGICAL       :: ReportDewpoint
  REAL(r64)     :: SteamDensity
  REAL(r64)     :: EnthSteamInDry
  REAL(r64)     :: RhoAirCurrent ! temporary value for current air density f(baro, db , W)
!  REAL(r64)     :: rRhoVapor
!  INTEGER,save :: Count=0
  CHARACTER(len=MaxNameLength+18) :: NodeReportingString
  CHARACTER(len=MaxNameLength+18) :: FluidName
  REAL(r64) :: rho
  REAL(r64) :: Cp
  REAL(r64) :: rhoStd

  IF (MyOneTimeFlag) THEN
    RhoAirStdInit = StdRhoAir
    RhoWaterStdInit = RhoH2O(InitConvTemp)
    ALLOCATE(NodeWetbulbRepReq(NumOfNodes))
    ALLOCATE(NodeWetbulbSchedPtr(NumOfNodes))
    ALLOCATE(NodeRelHumidityRepReq(NumOfNodes))
    ALLOCATE(NodeRelHumiditySchedPtr(NumOfNodes))
    ALLOCATE(NodeDewpointRepReq(NumOfNodes))
    ALLOCATE(NodeDewpointSchedPtr(NumOfNodes))
    NodeWetbulbRepReq = .FALSE.
    NodeWetbulbSchedPtr = 0
    NodeRelHumidityRepReq = .FALSE.
    NodeRelHumiditySchedPtr = 0
    NodeDewpointRepReq = .FALSE.
    NodeDewpointSchedPtr = 0

    DO iNode=1,NumOfNodes
      DO iReq=1,NumOfReqVariables
        IF ( SameString(ReqRepVars(iReq)%VarName,'System Node Wetbulb Temperature') .AND. &
             ( SameString(ReqRepVars(iReq)%Key,NodeID(iNode)) .OR. SameString(ReqRepVars(iReq)%Key,Blank) ) ) THEN
          NodeWetbulbRepReq(iNode) = .TRUE.
          NodeWetbulbSchedPtr(iNode) = ReqRepVars(iReq)%SchedPtr
          EXIT
        END IF
      END DO
    END DO
    DO iNode=1,NumOfNodes
      DO iReq=1,NumOfReqVariables
        IF ( SameString(ReqRepVars(iReq)%VarName,'System Node Relative Humidity') .AND. &
             ( SameString(ReqRepVars(iReq)%Key,NodeID(iNode)) .OR. SameString(ReqRepVars(iReq)%Key,Blank) ) ) THEN
          NodeRelHumidityRepReq(iNode) = .TRUE.
          NodeRelHumiditySchedPtr(iNode) = ReqRepVars(iReq)%SchedPtr
          EXIT
        END IF
      END DO
    END DO
    DO iNode=1,NumOfNodes
      DO iReq=1,NumOfReqVariables
        IF ( SameString(ReqRepVars(iReq)%VarName,'System Node Dewpoint Temperature') .AND. &
             ( SameString(ReqRepVars(iReq)%Key,NodeID(iNode)) .OR. SameString(ReqRepVars(iReq)%Key,Blank) ) ) THEN
          NodeDewpointRepReq(iNode) = .TRUE.
          NodeDewpointSchedPtr(iNode) = ReqRepVars(iReq)%SchedPtr
          EXIT
        END IF
      END DO
    END DO
    MyOneTimeFlag = .FALSE.
  END IF
  DO iNode=1,NumOfNodes
    NodeReportingString = 'NodeReportingCalc:'//NodeID(iNode)
    ReportWetbulb = .FALSE.
    ReportRelHumidity = .FALSE.
    ReportDewpoint = .false.
    IF ( NodeWetbulbRepReq(iNode) .AND. NodeWetbulbSchedPtr(iNode) > 0) THEN
      ReportWetbulb = (GetCurrentScheduleValue(NodeWetbulbSchedPtr(iNode)) > 0.0d0)
    ELSE IF ( NodeWetbulbRepReq(iNode) .AND. NodeWetbulbSchedPtr(iNode) == 0) THEN
      ReportWetbulb = .TRUE.
    ELSE IF ( Node(iNode)%SPMNodeWetbulbRepReq) THEN
      ReportWetbulb = .TRUE.
    END IF
    IF ( NodeRelHumidityRepReq(iNode) .AND. NodeRelHumiditySchedPtr(iNode) > 0) THEN
      ReportRelHumidity = (GetCurrentScheduleValue(NodeRelHumiditySchedPtr(iNode)) > 0.0d0)
    ELSE IF ( NodeRelHumidityRepReq(iNode) .AND. NodeRelHumiditySchedPtr(iNode) == 0) THEN
      ReportRelHumidity = .TRUE.
    END IF
    IF ( NodeDewpointRepReq(iNode) .AND. NodeDewpointSchedPtr(iNode) > 0) THEN
      ReportDewpoint = (GetCurrentScheduleValue(NodeDewpointSchedPtr(iNode)) > 0.0d0)
    ELSE IF ( NodeDewpointRepReq(iNode) .AND. NodeDewpointSchedPtr(iNode) == 0) THEN
      ReportDewpoint = .TRUE.
    END IF
    ! calculate the volume flow rate
    IF (Node(iNode)%FluidType == NodeType_Air) THEN
      MoreNodeInfo(iNode)%VolFlowRateStdRho = Node(iNode)%MassFlowRate / RhoAirStdInit
       ! if Node%Press was reliable could be used here.
      RhoAirCurrent = PsyRhoAirFnPbTdbW(OutBaroPress, Node(iNode)%Temp, Node(iNode)%HumRat)
      MoreNodeInfo(iNode)%Density=RhoAirCurrent
      IF (RhoAirCurrent /= 0.0D0) MoreNodeInfo(iNode)%VolFlowRateCrntRho = Node(iNode)%MassFlowRate / RhoAirCurrent
      MoreNodeInfo(iNode)%ReportEnthalpy = PsyHFnTdbW(Node(iNode)%Temp,Node(iNode)%HumRat)
      IF (ReportWetBulb) THEN
        ! if Node%Press was reliable could be used here.
        MoreNodeInfo(iNode)%WetbulbTemp = PsyTwbFnTdbWPb(Node(iNode)%Temp,Node(iNode)%HumRat,OutBaroPress,  &
           NodeReportingString)
      ELSE
        MoreNodeInfo(iNode)%WetbulbTemp = 0.0d0
      END IF
      IF (ReportDewpoint) THEN
        MoreNodeInfo(iNode)%AirDewpointTemp=PsyTdpFnWPb(Node(iNode)%HumRat,OutBaroPress)
      ELSE
        MoreNodeInfo(iNode)%AirDewpointTemp=0.0d0
      ENDIF
      IF (ReportRelHumidity) THEN
        ! if Node%Press was reliable could be used here.
        ! following routines don't issue psych errors and may be more reliable.
        MoreNodeInfo(iNode)%RelHumidity = 100.0d0 *   &
             PsyRhFnTdbWPb(Node(iNode)%Temp,Node(iNode)%HumRat,OutBaroPress,NodeReportingString)
!        rRhoVapor=PsyRhovFnTdbWPb(Node(iNode)%Temp,Node(iNode)%HumRat,OutBaroPress,'NodeReportingCalc:'//TRIM(NodeID(iNode)))
!        MoreNodeInfo(iNode)%RelHumidity = 100.0 * PsyRhFnTdbRhov(Node(iNode)%Temp,rRhoVapor,  &
!              'NodeReportingCalc:'//TRIM(NodeID(iNode)))

      ELSE
        MoreNodeInfo(iNode)%RelHumidity = 0.0d0
      ENDIF
    ELSE IF (Node(iNode)%FluidType == NodeType_Water) THEN


      IF (.NOT. ((Node(iNode)%FluidIndex > 0) .AND. (Node(iNode)%FluidIndex <= NumOfGlycols))) THEN
        rho = RhoWaterStdInit
        rhoStd = RhoWaterStdInit
        Cp  = CpCw(Node(iNode)%Temp)
      ELSE
        FluidName = GetGlycolNameByIndex(Node(iNode)%FluidIndex)
        Cp    =  GetSpecificHeatGlycol(FluidName,            &
                                     Node(iNode)%Temp,       &
                                     Node(iNode)%FluidIndex, &
                                     NodeReportingString)
        rhoStd = GetDensityGlycol(    FluidName,  &
                                      InitConvTemp, &
                                      Node(iNode)%FluidIndex, &
                                      NodeReportingString)
        rho    = GetDensityGlycol(    FluidName,  &
                                      Node(iNode)%Temp, &
                                      Node(iNode)%FluidIndex, &
                                      NodeReportingString)
      ENDIF

      MoreNodeInfo(iNode)%VolFlowRateStdRho = Node(iNode)%MassFlowRate / rhoStd
      MoreNodeInfo(iNode)%VolFlowRateCrntRho = Node(iNode)%MassFlowRate / rho
      MoreNodeInfo(iNode)%Density     = rho
      MoreNodeInfo(iNode)%ReportEnthalpy = Cp*Node(iNode)%Temp
      MoreNodeInfo(iNode)%WetbulbTemp = 0.0d0
      MoreNodeInfo(iNode)%RelHumidity = 100.0d0
    ELSE IF (Node(iNode)%FluidType == NodeType_Steam) THEN
        IF(Node(iNode)%Quality==1.0d0)Then
            SteamDensity=GetSatDensityRefrig("STEAM",Node(iNode)%Temp,Node(iNode)%Quality,  &
                           Node(iNode)%FluidIndex,'CalcMoreNodeInfo')
            EnthSteamInDry=GetSatEnthalpyRefrig("STEAM",Node(iNode)%Temp,Node(iNode)%Quality,  &
                           Node(iNode)%FluidIndex,'CalcMoreNodeInfo')
            MoreNodeInfo(iNode)%VolFlowRateStdRho = Node(iNode)%MassFlowRate / SteamDensity
            MoreNodeInfo(iNode)%ReportEnthalpy = EnthSteamInDry
            MoreNodeInfo(iNode)%WetbulbTemp = 0.0d0
            MoreNodeInfo(iNode)%RelHumidity = 0.0d0
        ElseIf(Node(iNode)%Quality==0.0d0)Then    !The node has condensate water through it
            MoreNodeInfo(iNode)%VolFlowRateStdRho = Node(iNode)%MassFlowRate / RhoWaterStdInit
            MoreNodeInfo(iNode)%ReportEnthalpy = CpCw(Node(iNode)%Temp)*Node(iNode)%Temp
            MoreNodeInfo(iNode)%WetbulbTemp = 0.0d0
            MoreNodeInfo(iNode)%RelHumidity = 0.0d0
        EndIf
    ELSE IF (Node(iNode)%FluidType == NodeType_Electric) THEN
      MoreNodeInfo(iNode)%VolFlowRateStdRho = 0.0d0
      MoreNodeInfo(iNode)%ReportEnthalpy = 0.0d0
      MoreNodeInfo(iNode)%WetbulbTemp = 0.0d0
      MoreNodeInfo(iNode)%RelHumidity = 0.0d0
    ELSE
      MoreNodeInfo(iNode)%VolFlowRateStdRho = Node(iNode)%MassFlowRate / RhoAirStdInit
      IF (Node(iNode)%HumRat > 0.0d0) THEN
        MoreNodeInfo(iNode)%ReportEnthalpy = PsyHFnTdbW(Node(iNode)%Temp,Node(iNode)%HumRat)
        IF (ReportWetBulb) THEN
          MoreNodeInfo(iNode)%WetbulbTemp = PsyTwbFnTdbWPb(Node(iNode)%Temp,Node(iNode)%HumRat,StdBaroPress)
        ELSE
          MoreNodeInfo(iNode)%WetbulbTemp = 0.0d0
        END IF
      ELSE
        MoreNodeInfo(iNode)%ReportEnthalpy = CpCw(Node(iNode)%Temp)*Node(iNode)%Temp
        MoreNodeInfo(iNode)%WetbulbTemp = 0.0d0
      END IF
    END IF
  END DO

  RETURN

END SUBROUTINE CalcMoreNodeInfo

SUBROUTINE MarkNode(NodeNumber,ObjectType,ObjectName,FieldName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine marks a node -- this node needs to exist in more than one object.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NodeNumber ! Node Number to be marked
  CHARACTER(len=*), INTENT(IN) :: ObjectType
  CHARACTER(len=*), INTENT(IN) :: ObjectName
  CHARACTER(len=*), INTENT(IN) :: FieldName

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  MarkedNode(NodeNumber)%IsMarked=.true.
  MarkedNode(NodeNumber)%ObjectType=ObjectType
  MarkedNode(NodeNumber)%ObjectName=ObjectName
  MarkedNode(NodeNumber)%FieldName=FieldName

  RETURN

END SUBROUTINE MarkNode

SUBROUTINE CheckMarkedNodes(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine checks "marked" nodes.

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
  INTEGER NodeNum

  DO NodeNum=1,NumOfNodes
    IF (MarkedNode(NodeNum)%IsMarked) THEN
      IF (NodeRef(NodeNum) == 0) THEN
        CALL ShowSevereError('Node="'//TRIM(NodeID(NodeNum))//'" did not find reference by another object.')
        CALL ShowContinueError('Object="'//TRIM(MarkedNode(NodeNum)%ObjectType)//  &
                    '", Name="'//TRIM(MarkedNode(NodeNum)%ObjectName)//'", Field=['//  &
                    TRIM(MarkedNode(NodeNum)%FieldName)//']')
        ErrorsFound=.true.
      ENDIF
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE CheckMarkedNodes

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

END MODULE NodeInputManager

