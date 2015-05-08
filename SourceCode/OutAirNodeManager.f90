MODULE OutAirNodeManager
  ! Module containing the routines that deal with the outside air nodes

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   September 1998
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and update the conditions for all the outside
  ! air nodes in the problem.

  ! METHODOLOGY EMPLOYED:
  ! Outside air nodes provide the connection to outside conditions for the
  ! EnergyPlus HVAC simulation. The  list of air nodes specified in the input
  ! file will be read in. Each air node will be updated to the outside environmental
  ! conditions at the start of each EnergyPlus main time step.

  ! REFERENCES:

  ! OTHER NOTES:
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataEnvironment
USE DataInterfaces
USE DataContaminantBalance, ONLY: Contaminant, OutdoorCO2, OutdoorGC
!USE DataHVACGlobals, ONLY: FirstTimeStepSysFlag

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

!MODULE PARAMETER DEFINITIONS:

!Type declarations in OutAirNodeManager module

!MODULE VARIABLE DECLARATIONS:

INTEGER, ALLOCATABLE, PUBLIC, DIMENSION(:) :: OutsideAirNodeList    ! List of all outside air inlet nodes
INTEGER, PUBLIC                            :: NumOutsideAirNodes = 0 ! Number of single outside air nodes
LOGICAL                                    :: GetOutAirNodesInputFlag = .True.  ! Flag set to make sure you get input once

!SUBROUTINE SPECIFICATIONS FOR MODULE OutAirNodeManager

PUBLIC  SetOutAirNodes
PRIVATE GetOutAirNodesInput
PRIVATE InitOutAirNodes
PUBLIC  CheckOutAirNodeNumber
PUBLIC  CheckAndAddAirNodeNumber

CONTAINS

SUBROUTINE SetOutAirNodes

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Make sure the outside air nodes are prepared for the HVAC simulation

          ! METHODOLOGY EMPLOYED:
          ! Use appropriate flag to check for needed action

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF (GetOutAirNodesInputFlag) THEN ! First time subroutine has been entered
    CALL GetOutAirNodesInput  ! Get OutAir Nodes data
    GetOutAirNodesInputFlag = .FALSE.
  END IF
  CALL InitOutAirNodes

  RETURN
END SUBROUTINE SetOutAirNodes

SUBROUTINE GetOutAirNodesInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Read in the list of outside air nodes & store in array OutAirInletNodeList

          ! METHODOLOGY EMPLOYED:
          ! Use the Get routines from the InputProcessor module.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE NodeInputManager

  IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER  :: RoutineName = 'GetOutAirNodesInput: '  ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumOutAirInletNodeLists
  INTEGER :: NumOutsideAirNodeSingles
  INTEGER :: NumNums   ! Number of REAL(r64) numbers returned by GetObjectItem
  INTEGER :: NumAlphas ! Number of alphanumerics returned by GetObjectItem
  INTEGER :: NumParams
  INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeNums
  INTEGER :: NumNodes
  INTEGER :: IOStat ! Status flag from GetObjectItem
  INTEGER :: NodeNum ! index into NodeNums
!  INTEGER :: OutAirNodeNum ! index into OutAirInletNodeList
  INTEGER :: OutAirInletNodeListNum ! OUTSIDE AIR INLET NODE LIST index
  INTEGER :: OutsideAirNodeSingleNum ! OUTSIDE AIR NODE index
  INTEGER :: AlphaNum ! index into Alphas
  INTEGER :: ListSize ! size of OutAirInletNodeList
!  LOGICAL :: AlreadyInList ! flag used for checking for duplicate input
  LOGICAL :: ErrorsFound
  LOGICAL :: ErrInList
  INTEGER :: CurSize
  INTEGER :: NextFluidStreamNum  ! Fluid stream index (all outside air inlet nodes need a unique fluid stream number)
  INTEGER, ALLOCATABLE, DIMENSION(:) :: TmpNums
  INTEGER, ALLOCATABLE, DIMENSION(:) :: TmpNums1
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject      ! Object type for getting and error messages
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  INTEGER :: MaxNums=0                ! Maximum number of numeric input fields
  INTEGER :: MaxAlphas=0              ! Maximum number of alpha input fields
  INTEGER :: TotalArgs=0              ! Total number of alpha and numeric arguments (max) for a


  NumOutAirInletNodeLists = GetNumObjectsFound('OutdoorAir:NodeList')
  NumOutsideAirNodeSingles = GetNumObjectsFound('OutdoorAir:Node')
  NumOutsideAirNodes = 0
  ErrorsFound = .FALSE.
  NextFluidStreamNum = 1

  ListSize = 0
  CurSize = 100
  ALLOCATE(TmpNums(CurSize))
  TmpNums = 0

  CALL GetObjectDefMaxArgs('NodeList',NumParams,NumAlphas,NumNums)
  ALLOCATE(NodeNums(NumParams))
  NodeNums=0

  CALL GetObjectDefMaxArgs('OutdoorAir:NodeList',TotalArgs,NumAlphas,NumNums)
  MaxNums=MAX(MaxNums,NumNums)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('OutdoorAir:Node',TotalArgs,NumAlphas,NumNums)
  MaxNums=MAX(MaxNums,NumNums)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)

  ALLOCATE(Alphas(MaxAlphas))
  Alphas=' '
  ALLOCATE(cAlphaFields(MaxAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(MaxNums))
  cNumericFields=' '
  ALLOCATE(Numbers(MaxNums))
  Numbers=0.0d0
  ALLOCATE(lAlphaBlanks(MaxAlphas))
  lAlphaBlanks=.TRUE.
  ALLOCATE(lNumericBlanks(MaxNums))
  lNumericBlanks=.TRUE.

  IF (NumOutAirInletNodeLists > 0) THEN
    ! Loop over all outside air inlet nodes in the input and count them
    CurrentModuleObject = 'OutdoorAir:NodeList'
    DO OutAirInletNodeListNum = 1, NumOutAirInletNodeLists
      CALL GetObjectItem(CurrentModuleObject,OutAirInletNodeListNum,Alphas,NumAlphas,Numbers,NumNums,IOStat, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

      DO AlphaNum = 1, NumAlphas
        ErrInList = .FALSE.
              !  To support HVAC diagram, every outside inlet node must have a unique fluid stream number
              !  GetNodeNums will increment the value across a node list, the starting value must be incremented
              !  here across lists and across objects
        CALL GetNodeNums(Alphas(AlphaNum),NumNodes,NodeNums,ErrInList,NodeType_Air, &
                         TRIM(CurrentModuleObject),TRIM(CurrentModuleObject),NodeConnectionType_OutsideAir, &
                         NextFluidStreamNum,ObjectIsNotParent, IncrementFluidStreamYes,InputFieldName=cAlphaFields(AlphaNum))
        NextFluidStreamNum = NextFluidStreamNum + NumNodes
        IF (ErrInList) THEN
          CALL ShowContinueError('Occurred in '//TRIM(CurrentModuleObject)//', '// &
                                 TRIM(cAlphaFields(AlphaNum))//' = '//TRIM(Alphas(AlphaNum)))
          ErrorsFound = .TRUE.
        END IF
        DO NodeNum = 1, NumNodes
          ! Duplicates here are not a problem, just ignore
          IF (.not. ANY(TmpNums == NodeNums(NodeNum))) THEN
            ListSize = ListSize + 1
            IF (ListSize > CurSize) THEN
              ALLOCATE(TmpNums1(CurSize+100))
              TmpNums1(1:CurSize) = TmpNums(1:CurSize)
              TmpNums1(CurSize+1:CurSize+100) = 0
              DEALLOCATE(TmpNums)
              CurSize = CurSize + 100
              ALLOCATE(TmpNums(CurSize))
              TmpNums = TmpNums1
              DEALLOCATE(TmpNums1)
            ENDIF
            TmpNums(ListSize) = NodeNums(NodeNum)
          END IF
        END DO
      END DO
    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input.')
    END IF
  END IF

  IF (NumOutsideAirNodeSingles > 0) THEN
    ! Loop over all single outside air nodes in the input
    CurrentModuleObject = 'OutdoorAir:Node'
    DO OutsideAirNodeSingleNum = 1, NumOutsideAirNodeSingles
      CALL GetObjectItem(CurrentModuleObject,OutsideAirNodeSingleNum,Alphas,NumAlphas,Numbers,NumNums,IOStat, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

      ErrInList = .FALSE.
            !  To support HVAC diagram, every outside inlet node must have a unique fluid stream number
            !  GetNodeNums will increment the value across a node list, the starting value must be incremented
            !  here across lists and across objects
      CALL GetNodeNums(Alphas(1),NumNodes,NodeNums,ErrInList,NodeType_Air, &
                       TRIM(CurrentModuleObject),TRIM(CurrentModuleObject),NodeConnectionType_OutsideAir, &
                       NextFluidStreamNum,ObjectIsNotParent,IncrementFluidStreamYes,InputFieldName=cAlphaFields(1))
      NextFluidStreamNum = NextFluidStreamNum + NumNodes
      IF (ErrInList) THEN
        CALL ShowContinueError('Occurred in '//TRIM(CurrentModuleObject)//', '//TRIM(cAlphaFields(1))//' = '//TRIM(Alphas(1)))
        ErrorsFound = .TRUE.
      END IF

      IF (NumNodes > 1) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//', '//TRIM(cAlphaFields(1))//' = '//TRIM(Alphas(1)))
        CALL ShowContinueError('...appears to point to a node list, not a single node.')
        ErrorsFound=.true.
        CYCLE
      ENDIF

      IF (.not. ANY(TmpNums == NodeNums(1))) THEN
        ListSize = ListSize + 1
        IF (ListSize > CurSize) THEN
          ALLOCATE(TmpNums1(CurSize+100))
          TmpNums1(1:CurSize) = TmpNums(1:CurSize)
          TmpNums1(CurSize+1:CurSize+100) = 0
          DEALLOCATE(TmpNums)
          CurSize = CurSize + 100
          ALLOCATE(TmpNums(CurSize))
          TmpNums = TmpNums1
          DEALLOCATE(TmpNums1)
        ENDIF
        TmpNums(ListSize) = NodeNums(1)
      ELSE  ! Duplicates are a problem
        CALL ShowSevereError(TRIM(CurrentModuleObject)//', duplicate '//TRIM(cAlphaFields(1))//' = '//TRIM(Alphas(1)))
        CALL ShowContinueError('Duplicate '//TRIM(cAlphaFields(1))//' might be found in an OutdoorAir:NodeList.')
        ErrorsFound = .TRUE.
        CYCLE
      END IF

     ! Set additional node properties
     IF (NumNums > 0) Node(NodeNums(1))%Height = Numbers(1)

    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input.')
    END IF
  END IF

  IF (ListSize > 0) THEN
    NumOutsideAirNodes = ListSize
    ALLOCATE(OutsideAirNodeList(ListSize))
    OutsideAirNodeList = TmpNums(1:ListSize)
  END IF
  DEALLOCATE(TmpNums)

  DEALLOCATE(Alphas)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(Numbers)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  RETURN

END SUBROUTINE GetOutAirNodesInput

SUBROUTINE InitOutAirNodes
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Sept 1998
          !       MODIFIED       B. Griffith, added EMS override
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initialize the outside air node data data.  In Particular,
          ! set the outside air nodes to the outside conditions at the
          ! start of every heat balance time step.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics, ONLY: PsyHFnTdbW, PsyWFnTdbTwbPb

  IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: OutsideAirNodeNum
  INTEGER :: NodeNum

  ! Do the begin time step initialization
    DO OutsideAirNodeNum = 1, NumOutsideAirNodes
      NodeNum = OutsideAirNodeList(OutsideAirNodeNum)
      IF (Node(NodeNum)%Height < 0.0d0) THEN
        ! Note -- this setting is different than the DataEnvironment "AT" settings.
        Node(NodeNum)%OutAirDryBulb = OutDryBulbTemp
        Node(NodeNum)%OutAirWetBulb = OutWetBulbTemp
      ELSE
        Node(NodeNum)%OutAirDryBulb = OutDryBulbTempAt(Node(NodeNum)%Height)
        Node(NodeNum)%OutAirWetBulb = OutWetBulbTempAt(Node(NodeNum)%Height)
      END IF

      IF (Node(NodeNum)%EMSOverrideOutAirDryBulb) &
           Node(NodeNum)%OutAirDryBulb = Node(NodeNum)%EMSValueForOutAirDryBulb

      IF (Node(NodeNum)%EMSOverrideOutAirWetBulb) THEN
        Node(NodeNum)%OutAirWetBulb = Node(NodeNum)%EMSValueForOutAirWetBulb
        Node(NodeNum)%HumRat = PsyWFnTdbTwbPb(Node(NodeNum)%OutAirDryBulb, Node(NodeNum)%OutAirWetBulb, OutBaroPress)
        Node(NodeNum)%Enthalpy = PsyHFnTdbW(Node(NodeNum)%OutAirDryBulb,Node(NodeNum)%HumRat)
      ELSE
        Node(NodeNum)%HumRat = OutHumRat
        Node(NodeNum)%Enthalpy = PsyHFnTdbW(Node(NodeNum)%OutAirDryBulb,OutHumRat)
      ENDIF

      Node(NodeNum)%Temp = Node(NodeNum)%OutAirDryBulb
      Node(NodeNum)%Press = OutBaroPress
      Node(NodeNum)%Quality = 0.0d0
      ! Add contaminants
      IF (Contaminant%CO2Simulation) Node(NodeNum)%CO2 = OutdoorCo2
      IF (Contaminant%GenericContamSimulation) Node(NodeNum)%GenContam = OutdoorGC
    END DO

  RETURN

END SUBROUTINE InitOutAirNodes

FUNCTION CheckOutAirNodeNumber(NodeNumber) RESULT(Okay)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Feb 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provide a entry into the OutAirNode List for checking from other routines.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NodeNumber  ! Number of node to check to see if in Outside Air list
  LOGICAL :: Okay  ! True if found, false if not

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetOutAirNodesInputFlag) THEN ! First time subroutine has been entered
    CALL GetOutAirNodesInput  ! Get Out Air Nodes data
    GetOutAirNodesInputFlag = .FALSE.
    CALL SetOutAirNodes
  END IF

  IF (ANY(OutsideAirNodeList == NodeNumber)) THEN
    Okay=.true.
  ELSE
    Okay=.false.
  ENDIF

  RETURN

END FUNCTION CheckOutAirNodeNumber

SUBROUTINE CheckAndAddAirNodeNumber(NodeNumber,Okay)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! At the time of writing, some items (namely Chillers) have "made up" node
          ! names for nodes that are "outside air nodes".  Rather than fatal out, add
          ! this subroutine which will check and then add a outside air node, if necessary.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics, ONLY: PsyHFnTdbW
  USE NodeInputManager

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: NodeNumber  ! Number of node to check to see if in Outside Air list
  LOGICAL, INTENT(OUT) :: Okay  ! True if found, false if not

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, ALLOCATABLE, DIMENSION(:) :: TmpNums
  INTEGER :: DummyNumber
  LOGICAL :: errflag=.false.

  IF (GetOutAirNodesInputFlag) THEN ! First time subroutine has been entered
    CALL GetOutAirNodesInput  ! Get Out Air Nodes data
    GetOutAirNodesInputFlag = .FALSE.
    CALL SetOutAirNodes
  END IF

  Okay=.false.

  IF(NumOutsideAirNodes > 0)THEN
    IF (ANY(OutsideAirNodeList == NodeNumber)) THEN
      Okay=.true.
    ELSE
      Okay=.false.
    ENDIF
  ELSE
    Okay=.false.
  END IF

  IF (NodeNumber > 0) THEN
    IF (.not. Okay) THEN
      ! Add new outside air node to list
      ALLOCATE(TmpNums(NumOutsideAirNodes+1))
      IF (NumOutsideAirNodes > 0)THEN
        TmpNums(1:NumOutsideAirNodes)=OutsideAirNodeList
        DEALLOCATE(OutsideAirNodeList)
      ELSE
        TmpNums(1:1)=NodeNumber
      END IF
      NumOutsideAirNodes=NumOutsideAirNodes+1
      ALLOCATE(OutsideAirNodeList(NumOutsideAirNodes))
      OutsideAirNodeList=TmpNums
      OutsideAirNodeList(NumOutsideAirNodes)=NodeNumber
!register new node..
      CALL GetNodeNums(NodeID(NodeNumber),DummyNumber,TmpNums,errflag,NodeType_Air, &
          'OutdoorAir:Node','OutdoorAir:Node',NodeConnectionType_OutsideAir, &
            NumOutsideAirNodes,ObjectIsNotParent,IncrementFluidStreamYes)
      DEALLOCATE(TmpNums)
      IF (Node(NodeNumber)%Height < 0.0d0) THEN
        Node(NodeNumber)%OutAirDryBulb = OutDryBulbTemp
        Node(NodeNumber)%OutAirWetBulb = OutWetBulbTemp
      ELSE
        Node(NodeNumber)%OutAirDryBulb = OutDryBulbTempAt(Node(NodeNumber)%Height)
        Node(NodeNumber)%OutAirWetBulb = OutWetBulbTempAt(Node(NodeNumber)%Height)
      END IF
      Node(NodeNumber)%Temp = Node(NodeNumber)%OutAirDryBulb
      Node(NodeNumber)%HumRat = OutHumRat
      Node(NodeNumber)%Enthalpy = PsyHFnTdbW(Node(NodeNumber)%Temp,OutHumRat)
      Node(NodeNumber)%Press = OutBaroPress
      Node(NodeNumber)%Quality = 0.0d0
      ! Add contaminants
      IF (Contaminant%CO2Simulation) Node(NodeNumber)%CO2 = OutdoorCo2
      IF (Contaminant%GenericContamSimulation) Node(NodeNumber)%GenContam = OutdoorGC
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CheckAndAddAirNodeNumber

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

END MODULE OutAirNodeManager
