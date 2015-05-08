MODULE BranchInputManager

  ! Module containing the routines dealing with the BRANCH and CONNECTOR
  ! lists input.

  ! MODULE INFORMATION:
  !       AUTHOR         Linda Lawrie
  !       DATE WRITTEN   October 1999
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To Get the IDD objects "BranchList", "Branch", "ConnectorList",
  ! "Connector:Splitter", and "Connector:Mixer".  Also, to supply other modules/routines with
  ! information about these objects.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, OutputFileBNDetails, DisplayExtraWarnings
USE DataInterfaces, ONLY: ShowFatalError, ShowWarningError, ShowSevereError, ShowMessage, ShowContinueError, ShowSevereMessage
USE DataLoopNode
USE DataBranchAirLoopPlant

  ! Use statements for access to subroutines in other modules
USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem, FindItemInList, VerifyName, GetObjectDefMaxArgs, MakeUPPERCase,   &
                          SameString
USE NodeInputManager
USE BranchNodeConnections

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
  CHARACTER(len=*), PARAMETER :: cMIXER='Connector:Mixer'
  CHARACTER(len=*), PARAMETER :: cSPLITTER='Connector:Splitter'
  CHARACTER(len=*), PARAMETER :: Blank=' '


  !DERIVED TYPE DEFINITIONS
TYPE ConnectorData
  CHARACTER(len=MaxNameLength)       :: Name              = Blank ! Name for this Connector
  INTEGER                            :: NumOfConnectors   = 0     ! Number of Connectors in this group
  INTEGER                            :: NumOfSplitters    = 0     ! Number of Splitters in this connector group
  INTEGER                            :: NumOfMixers       = 0     ! Number of Mixers in this connector group
  CHARACTER(len=32), DIMENSION(:), &
                     ALLOCATABLE     :: ConnectorType    ! Connector:Splitter or Connector:Mixer
  CHARACTER(len=MaxNameLength), &
     DIMENSION(:), ALLOCATABLE       :: ConnectorName    ! Name for that Connector:Splitter or Connector:Mixer
  INTEGER, DIMENSION(:), ALLOCATABLE :: ConnectorMatchNo ! Pointer to index where this Splitter or Mixer matches
                                                         ! Splitter => Mixer or Mixer => Splitter.  0 indicates no match
END TYPE

TYPE BranchListData
  CHARACTER(len=MaxNameLength)   :: Name              = Blank ! Name of this Branch List
  INTEGER                        :: NumOfBranchNames  = 0     ! Number of Branches on the Branch List
  CHARACTER(len=MaxNameLength), &
     ALLOCATABLE, DIMENSION(:)   :: BranchNames               ! Names of the branches on this branch list
  CHARACTER(len=MaxNameLength)   :: LoopName          = Blank ! Name of Loop this Branch list belongs to
  CHARACTER(len=20)              :: LoopType          = Blank ! Loop type this branch is on
END TYPE

TYPE ComponentData
  CHARACTER(len=MaxNameLength)   :: CType             = Blank ! Component Type (Cannot be SPLITTER or MIXER)
  CHARACTER(len=MaxNameLength)   :: Name              = Blank ! Component Name
  INTEGER                        :: CtrlType          = 0     ! Active, Passive, Bypass (1,2,3)
  CHARACTER(len=MaxNameLength)   :: InletNodeName     = Blank ! Inlet Node ID
  INTEGER                        :: InletNode         = 0     ! Inlet Node Number
  CHARACTER(len=MaxNameLength)   :: OutletNodeName    = Blank ! Outlet Node ID
  INTEGER                        :: OutletNode        = 0     ! Outlet Node Number
END TYPE

TYPE BranchData
  CHARACTER(len=MaxNameLength)   :: Name              = Blank ! Name for this Branch
  CHARACTER(len=MaxNameLength)   :: AssignedLoopName  = Blank ! Loop Name for this branch
  REAL(r64)                      :: MaxFlowRate       = 0.0d0   ! Max Flow Rate of the Branch
  INTEGER                        :: PressureCurveType = 0     ! Integer index of pressure curve type
  INTEGER                        :: PressureCurveIndex= 0     ! Integer index of pressure curve
  INTEGER                        :: FluidType         = NodeType_Unknown ! Fluid type (see DataLoopNode)
  INTEGER                        :: NumOfComponents   = 0     ! Number of Components on this Branch
  TYPE(ComponentData),  &
     ALLOCATABLE, DIMENSION(:)   :: Component         ! Component definitions for each component
END TYPE

TYPE SplitterData
  CHARACTER(len=MaxNameLength)   :: Name              = Blank ! Splitter Name
  CHARACTER(len=MaxNameLength)   :: InletBranchName   = Blank ! Splitter Inlet Branch Name
  INTEGER                        :: NumOutletBranches = 0     ! Number of outlets on this Splitter
  CHARACTER(len=MaxNameLength), &
     ALLOCATABLE, DIMENSION(:)   :: OutletBranchNames ! Names of the Outlet Branches
END TYPE

TYPE MixerData
  CHARACTER(len=MaxNameLength)   :: Name              = Blank ! Mixer Name
  CHARACTER(len=MaxNameLength)   :: OutletBranchName  = Blank ! Mixer Outlet Branch Name
  INTEGER                        :: NumInletBranches  = 0     ! Number of inlets for this Mixer
  CHARACTER(len=MaxNameLength), &
     ALLOCATABLE, DIMENSION(:)   :: InletBranchNames  ! Names of Inlet Branches
END TYPE

  !MODULE VARIABLE DECLARATIONS:
INTEGER                          :: NumOfBranchLists=0    ! Number of Branch Lists found in IDF
TYPE (BranchListData),  &
      ALLOCATABLE, DIMENSION(:)  :: BranchList            ! Branch List data for each Branch List
INTEGER                          :: NumOfBranches=0       ! Number of Branches found in IDF
TYPE (BranchData), ALLOCATABLE, &
      DIMENSION(:)               :: Branch                ! Branch Data for each Branch
INTEGER                          :: NumOfConnectorLists=0 ! Number of Connector Lists found in IDF
TYPE (ConnectorData), ALLOCATABLE, &
      DIMENSION(:)               :: ConnectorLists        ! Connector List data for each Connector List
INTEGER                          :: NumSplitters=0        ! Number of Splitters found in IDF
TYPE (SplitterData), ALLOCATABLE,  &
      DIMENSION(:)               :: Splitters             ! Splitter Data for each Splitter
INTEGER                          :: NumMixers=0           ! Number of Mixers found in IDF
TYPE (MixerData), ALLOCATABLE,  &
      DIMENSION(:)               :: Mixers                ! Mixer Data for each Mixer

LOGICAL :: GetBranchInputFlag=.true.        ! Flag used to retrieve Input
LOGICAL :: GetBranchListInputFlag=.true.    ! Flag used to retrieve Input
LOGICAL :: GetSplitterInputFlag=.true.      ! Flag used to retrieve Input
LOGICAL :: GetMixerInputFlag=.true.         ! Flag used to retrieve Input
LOGICAL :: GetConnectorListInputFlag=.true. ! Flag used to retrieve Input
LOGICAL,PUBLIC :: InvalidBranchDefinitions=.false.

CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in getting objects

  !SUBROUTINE SPECIFICATIONS FOR MODULE BranchInputManager
PUBLIC  ManageBranchInput
PUBLIC  GetBranchList
PUBLIC  NumBranchesInBranchList
PUBLIC  GetBranchData
PUBLIC  NumCompsInBranch
PUBLIC  GetLoopSplitter
PUBLIC  GetLoopMixer
PUBLIC  TestBranchIntegrity
PUBLIC  AuditBranches
PUBLIC  GetAirBranchIndex
PUBLIC  GetBranchFlow
PUBLIC  GetBranchFanTypeName
!PUBLIC  TestAirPathIntegrity
!PRIVATE TestSupplyAirPathIntegrity
!PRIVATE TestReturnAirPathIntegrity
PRIVATE GetConnectorList
PRIVATE GetConnectorListInput
PRIVATE GetSplitterInput
PRIVATE GetMixerInput
PRIVATE GetInternalBranchData
PRIVATE GetBranchInput
PRIVATE GetBranchListInput
PUBLIC  GetNumSplitterMixerInConntrList
PUBLIC  GetFirstBranchInletNodeName
PUBLIC  GetLastBranchOutletNodeName
PUBLIC  CheckSystemBranchFlow
PRIVATE CheckBranchForOASys
!PUBLIC  MyPlantSizingIndex
PRIVATE FindPlantLoopBranchConnection
PRIVATE FindCondenserLoopBranchConnection
PRIVATE FindAirLoopBranchConnection
PRIVATE FindAirPlantCondenserLoopFromBranchList

CONTAINS

SUBROUTINE ManageBranchInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Nov 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called from HVACManager to make sure that branch input is
          ! gathered prior to need.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na

  IF (GetBranchInputFlag) THEN
    CALL GetBranchInput
    IF (GetBranchListInputFlag) THEN
      GetBranchListInputFlag=.false.
      CALL GetBranchListInput
    ENDIF
    CALL AuditBranches(.false.)
    GetBranchInputFlag=.false.
  ENDIF

  RETURN

END SUBROUTINE ManageBranchInput

!==================================================================================
!   Routines that "get" data from internal branch management structure
!==================================================================================

SUBROUTINE GetBranchList(LoopName,BranchListName,NumBranchNames,BranchNames,LoopType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 1999
          !       MODIFIED       October 2001, Automatic Extensibility
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine "gets" the branch list specified in a Plant or Condenser loop and
          ! returns number and names to the outside calling routine.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)   :: LoopName        ! Name of Loop Branch List is on
  CHARACTER(len=*), INTENT(IN)   :: BranchListName  ! Branch List Name from Input
  INTEGER, INTENT(INOUT)         :: NumBranchNames  ! Number of Branches for this Branch List
  CHARACTER(len=MaxNameLength), INTENT(OUT),  &
                  DIMENSION(:)   :: BranchNames     ! Names of Branches on this Branch List
  CHARACTER(len=*), INTENT(IN)   :: LoopType        ! Type of Loop Branch list is on

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found                  ! Points to correct Branch List/Branch
  LOGICAL :: ErrFound               ! True when error has occured (cannot find Branch List)

  ErrFound=.false.

  IF (GetBranchListInputFlag) THEN
    GetBranchListInputFlag=.false.
    CALL GetBranchListInput
  ENDIF

  !  Find this BranchList in the master BranchList Names
  Found=FindItemInList(BranchListName,BranchList%Name,NumOfBranchLists)
  IF (Found == 0) THEN
    CALL ShowFatalError('GetBranchList: BranchList Name not found='//TRIM(BranchListName))
  ENDIF

  ! Set data
  IF (BranchList(Found)%LoopName == Blank) THEN
    BranchList(Found)%LoopName=LoopName
    BranchList(Found)%LoopType=LoopType
  ELSEIF (BranchList(Found)%LoopName /= LoopName) THEN
    CALL ShowSevereError('GetBranchList: BranchList Loop Name already assigned')
    CALL ShowContinueError('BranchList='//TRIM(BranchList(Found)%Name)//', already assigned to loop='//  &
         TRIM(BranchList(Found)%LoopName))
    CALL ShowContinueError('Now requesting assignment to Loop='//TRIM(LoopName))
    ErrFound=.true.
  ENDIF


  ! Return data
  NumBranchNames=BranchList(Found)%NumOfBranchNames
  IF (SIZE(BranchNames) < NumBranchNames) THEN
    CALL ShowSevereError('GetBranchList: Branch Names array not big enough to hold Branch Names')
    CALL ShowContinueError('Input BranchListName='//TRIM(BranchListName)//', in Loop='//TRIM(LoopName))
    CALL ShowContinueError('BranchName Array size='//TRIM(TrimSigDigits(SIZE(BranchNames)))//  &
            ', but input size='//TRIM(TrimSigDigits(NumBranchNames)))
    ErrFound=.true.
  ELSE
    BranchNames=Blank
    BranchNames(1:NumBranchNames)=BranchList(Found)%BranchNames(1:NumBranchNames)
  ENDIF

  IF (ErrFound) THEN
    CALL ShowFatalError('GetBranchList: preceding condition(s) causes program termination.')
  ENDIF

  RETURN

END SUBROUTINE GetBranchList

INTEGER FUNCTION NumBranchesInBranchList(BranchListName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the number of branches in a branch list so that the calling
          ! routine can allocate arrays before calling GetBranchList.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowFatalError
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: BranchListName

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Found

  IF (GetBranchListInputFlag) THEN
    GetBranchListInputFlag=.false.
    CALL GetBranchListInput
  ENDIF

  !  Find this BranchList in the master BranchList Names
  Found=FindItemInList(BranchListName,BranchList%Name,NumOfBranchLists)
  IF (Found == 0) THEN
    CALL ShowFatalError('NumBranchesInBranchList: BranchList Name not found='//TRIM(BranchListName))
  ENDIF

  NumBranchesInBranchList=BranchList(Found)%NumOfBranchNames

  RETURN

END FUNCTION NumBranchesInBranchList

SUBROUTINE GetBranchData(LoopName,BranchName,BranchMaxFlow,PressCurveType,PressCurveIndex,NumComps,CompType,CompName, &
           CompInletNodeNames,CompInletNodeNums,CompOutletNodeNames,CompOutletNodeNums,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 1999
          !       MODIFIED       October 2001, Automatic Extensibility
          !                      September 2012, B. Griffith, removed component control types
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets the Branch Data (internal structure) for the requested
          ! Branch Name and returns it in "list structure" to the calling routine.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: LoopName            ! Loop Name of this Branch
  CHARACTER(len=*), INTENT(IN) :: BranchName          ! Requested Branch Name
  REAL(r64), INTENT(OUT)            :: BranchMaxFlow       ! Max Flow Rate for Branch
  INTEGER, INTENT(OUT) :: PressCurveType  ! Index of a pressure curve object
  INTEGER, INTENT(OUT) :: PressCurveIndex  ! Index of a pressure curve object
  INTEGER, INTENT(INOUT)       :: NumComps            ! Number of Components on Branch
  CHARACTER(len=MaxNameLength), INTENT(OUT), &
                 DIMENSION(:)  :: CompType            ! Component Type for each item on Branch
  CHARACTER(len=MaxNameLength), INTENT(OUT), &
                 DIMENSION(:)  :: CompName            ! Component Name for each item on Branch
  CHARACTER(len=MaxNameLength), INTENT(OUT), &
                 DIMENSION(:)  :: CompInletNodeNames  ! Component Inlet Node IDs for each item on Branch
  INTEGER, INTENT(OUT), &
                 DIMENSION(:)  :: CompInletNodeNums   ! Component Inlet Node Numbers for each item on Branch
  CHARACTER(len=MaxNameLength), INTENT(OUT), &
                 DIMENSION(:)  :: CompOutletNodeNames ! Component Outlet Node IDs for each item on Branch
  INTEGER, INTENT(OUT), &
                 DIMENSION(:)  :: CompOutletNodeNums  ! Component Outlet Node Numbers for each item on Branch
  LOGICAL, INTENT(INOUT)         :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER               :: Count        ! Loop Counter
  TYPE (ComponentData), ALLOCATABLE, SAVE, &
       DIMENSION(:)     :: BComponents  ! Component data to be returned
  INTEGER               :: MinCompsAllowed



  ! NumComps now defined on input

  ALLOCATE(BComponents(NumComps))

  CALL GetInternalBranchData(LoopName,BranchName,BranchMaxFlow,PressCurveType,PressCurveIndex,NumComps,BComponents,ErrorsFound)

  MinCompsAllowed=MIN(SIZE(CompType),SIZE(CompName),SIZE(CompInletNodeNames),  &
                      SIZE(CompInletNodeNums),SIZE(CompOutletNodeNames),SIZE(CompOutletNodeNums))
  IF (MinCompsAllowed < NumComps) THEN
    CALL ShowSevereError('GetBranchData: Component List arrays not big enough to hold Number of Components')
    CALL ShowContinueError('Input BranchName='//TRIM(BranchName)//', in Loop='//TRIM(LoopName))
    CALL ShowContinueError('Max Component Array size='//TRIM(TrimSigDigits(MinCompsAllowed))//  &
            ', but input size='//TRIM(TrimSigDigits(NumComps)))
    CALL ShowFatalError('Program terminates due to preceding conditions.')
  ENDIF

  DO Count=1,NumComps
    CompType(Count)=BComponents(Count)%CType
    CompName(Count)=BComponents(Count)%Name
    CompInletNodeNames(Count)=BComponents(Count)%InletNodeName
    CompInletNodeNums(Count)=BComponents(Count)%InletNode
    CompOutletNodeNames(Count)=BComponents(Count)%OutletNodeName
    CompOutletNodeNums(Count)=BComponents(Count)%OutletNode
  ENDDO
  DEALLOCATE(BComponents)


  RETURN

END SUBROUTINE GetBranchData

INTEGER FUNCTION NumCompsInBranch(BranchName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the number of components in a branch so that the calling
          ! routine can allocate arrays before calling GetBranchData.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: BranchName

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Found

  IF (GetBranchInputFlag) THEN
    GetBranchInputFlag=.false.
    CALL GetBranchInput
  ENDIF

  Found=FindItemInList(BranchName,Branch%Name,NumOfBranches)
  IF (Found == 0) THEN
    CALL ShowSevereError('NumCompsInBranch:  Branch not found='//TRIM(BranchName))
    NumCompsInBranch=0
  ELSE
    NumCompsInBranch=Branch(Found)%NumOfComponents
  ENDIF

  RETURN

END FUNCTION NumCompsInBranch

INTEGER FUNCTION GetAirBranchIndex(CompType, CompName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the branch index so that the calling
          ! routine can search for a fan on this branch or use branch flow for sizing.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType
  CHARACTER(len=*), INTENT(IN) :: CompName

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER BranchNum
  INTEGER CompNum
  INTEGER NumBranches

  IF (GetBranchInputFlag) THEN
    GetBranchInputFlag=.false.
    CALL GetBranchInput
  ENDIF

  NumBranches=SIZE(BRANCH)

  IF (NumBranches == 0) THEN
    CALL ShowSevereError('GetAirBranchIndex:  Branch not found with component = '//TRIM(CompType)//' "'//TRIM(CompName)//'"')
    GetAirBranchIndex=0
  ELSE
    BranchLoop: DO BranchNum = 1, NumBranches
      DO CompNum = 1, Branch(BranchNum)%NumOfComponents
        IF(SameString(CompType,Branch(BranchNum)%Component(CompNum)%CType) .AND. &
           SameString(CompName,Branch(BranchNum)%Component(CompNum)%Name))THEN
          GetAirBranchIndex=BranchNum
          EXIT BranchLoop
        END IF
      END DO
    END DO BranchLoop
  ENDIF

  RETURN

END FUNCTION GetAirBranchIndex

REAL(r64) FUNCTION GetBranchFlow(BranchNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the branch index so that the calling
          ! routine can search for a fan on this branch or use branch flow for sizing.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BranchNum

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER NumBranches

  IF (GetBranchInputFlag) THEN
    GetBranchInputFlag=.false.
    CALL GetBranchInput
  ENDIF

  NumBranches=SIZE(BRANCH)

  IF (NumBranches == 0) THEN
    CALL ShowSevereError('GetBranchFlow:  Branch index not found = '//TrimSigDigits(BranchNum,0))
    GetBranchFlow = 0.d0
  ELSE
    IF(BranchNum > 0 .AND. BranchNum <= NumBranches)THEN
      GetBranchFlow = Branch(BranchNum)%MaxFlowRate
    END IF
  ENDIF

  RETURN

END FUNCTION GetBranchFlow

SUBROUTINE GetBranchFanTypeName(BranchNum, FanType, FanName, ErrFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   April 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the branch fan flow rate so that the calling
          ! routine can either use this flow or use then branch flow for sizing.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BranchNum
  CHARACTER(len=MaxNameLength), INTENT(INOUT) :: FanType
  CHARACTER(len=MaxNameLength), INTENT(INOUT) :: FanName
  LOGICAL :: ErrFound

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=1), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER CompNum
  INTEGER NumBranches

  IF (GetBranchInputFlag) THEN
    GetBranchInputFlag=.false.
    CALL GetBranchInput
  ENDIF

  ErrFound = .FALSE.
  NumBranches=SIZE(BRANCH)

  FanType=Blank
  FanName=Blank

  IF (NumBranches == 0) THEN
    CALL ShowSevereError('GetBranchFanTypeName:  Branch index not found = '//TrimSigDigits(BranchNum,0))
    ErrFound = .TRUE.
  ELSE
    IF(BranchNum > 0 .AND. BranchNum <= NumBranches)THEN
      DO CompNum = 1, Branch(BranchNum)%NumOfComponents
        IF(SameString('Fan:OnOff',Branch(BranchNum)%Component(CompNum)%CType) .OR. &
           SameString('Fan:ConstantVolume',Branch(BranchNum)%Component(CompNum)%CType) .OR. &
           SameString('Fan:VariableVolume',Branch(BranchNum)%Component(CompNum)%CType))THEN
          FanType=Branch(BranchNum)%Component(CompNum)%CType
          FanName=Branch(BranchNum)%Component(CompNum)%Name
          EXIT
        END IF
      END DO
      IF(FanType == Blank)ErrFound = .TRUE.
    ELSE
      CALL ShowSevereError('GetBranchFanTypeName:  Branch index not found = '//TrimSigDigits(BranchNum,0))
      ErrFound = .TRUE.
    END IF
  ENDIF

  RETURN

END SUBROUTINE GetBranchFanTypeName

SUBROUTINE CheckBranchForOASys(CompType, CompName, OASysFlag, ErrFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns TRUE if the branch contains an OA System

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType
  CHARACTER(len=*), INTENT(IN) :: CompName
  LOGICAL, INTENT(INOUT)                   :: OASysFlag
  LOGICAL, INTENT(INOUT)                   :: ErrFound

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=1), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER CompNum        ! loop counter
  INTEGER NumBranches    ! number of branches
  INTEGER BranchNum      ! loop index
  INTEGER AirBranchIndex ! index to branch containing CompType, CompName

  IF (GetBranchInputFlag) THEN
    GetBranchInputFlag=.false.
    CALL GetBranchInput
  ENDIF

  ErrFound = .FALSE.
  OASysFlag = .FALSE.
  NumBranches=SIZE(BRANCH)

  BranchLoop: DO BranchNum = 1, NumBranches
    DO CompNum = 1, Branch(BranchNum)%NumOfComponents
      IF(.NOT. SameString(CompType,Branch(BranchNum)%Component(CompNum)%CType) .AND. &
         .NOT. SameString(CompName,Branch(BranchNum)%Component(CompNum)%Name))CYCLE
      AirBranchIndex=BranchNum
      EXIT BranchLoop
    END DO
  END DO BranchLoop

  IF (AirBranchIndex == 0) THEN
    CALL ShowSevereError('CheckBranchForOASys:  Branch index not found = '//TrimSigDigits(AirBranchIndex,0))
    ErrFound = .TRUE.
  ELSE
    IF(AirBranchIndex > 0 .AND. AirBranchIndex <= NumBranches)THEN
      DO CompNum = 1, Branch(AirBranchIndex)%NumOfComponents
        IF(.not. SameString('AirLoopHVAC:OutdoorAirSystem',Branch(AirBranchIndex)%Component(CompNum)%CType))CYCLE
        OASysFlag = .TRUE.
        EXIT
      END DO
    ELSE
      CALL ShowSevereError('CheckBranchForOASys:  Branch index not found = '//TrimSigDigits(AirBranchIndex,0))
      ErrFound = .TRUE.
    END IF
  ENDIF

  RETURN

END SUBROUTINE CheckBranchForOASys

SUBROUTINE GetInternalBranchData(LoopName,BranchName,BranchMaxFlow,PressCurveType,PressCurveIndex,NumComps,BComponents,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets the Branch Data (internal structure) for the requested
          ! Branch Name and returns it to the calling routine.  This is used internally
          ! in the module.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: LoopName            ! Loop Name for Branch
  CHARACTER(len=*), INTENT(IN) :: BranchName          ! Requested Branch Name
  REAL(r64), INTENT(OUT)            :: BranchMaxFlow       ! Max Flow Rate for Branch
  INTEGER, INTENT(OUT) :: PressCurveType         ! Index of pressure curve object
  INTEGER, INTENT(OUT) :: PressCurveIndex         ! Index of pressure curve object
  INTEGER, INTENT(INOUT)       :: NumComps            ! Number of Components on Branch
  TYPE (ComponentData), INTENT(INOUT), &
                 DIMENSION(:)  :: BComponents         ! Component data returned
  LOGICAL, INTENT(INOUT)       :: ErrorsFound         ! True when Loop Name is already assigned and this not same loop

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: Found                ! Pointer to requested Branch Name

  IF (GetBranchInputFlag) THEN
    CALL GetBranchInput
    GetBranchInputFlag=.false.
  ENDIF

  Found=FindItemInList(BranchName,Branch%Name,NumOfBranches)
  IF (Found == 0) THEN
    CALL ShowSevereError('GetInternalBranchData:  Branch not found='//TRIM(BranchName))
    ErrorsFound=.true.
    BranchMaxFlow=0.0d0
    NumComps=0
  ELSE
    IF (Branch(Found)%AssignedLoopName == Blank) THEN
      Branch(Found)%AssignedLoopName=LoopName
      BranchMaxFlow=Branch(Found)%MaxFlowRate
      PressCurveType=Branch(Found)%PressureCurveType
      PressCurveIndex=Branch(Found)%PressureCurveIndex
      NumComps=Branch(Found)%NumOfComponents
!      IF (ALLOCATED(BComponents)) THEN
!        DEALLOCATE(BComponents)
!      ENDIF
!      ALLOCATE(BComponents(NumComps))
      BComponents(1:NumComps)=Branch(Found)%Component(1:NumComps)
    ELSEIF (Branch(Found)%AssignedLoopName /= LoopName) THEN
      CALL ShowSevereError('Attempt to assign branch to two different loops, Branch='//TRIM(BranchName))
      CALL ShowContinueError('Branch already assigned to loop='//TRIM(Branch(Found)%AssignedLoopName))
      CALL ShowContinueError('New attempt to assign to loop='//TRIM(LoopName))
      ErrorsFound=.true.
      BranchMaxFlow=0.0d0
      NumComps=0
    ELSE
      BranchMaxFlow=Branch(Found)%MaxFlowRate
      PressCurveType=Branch(Found)%PressureCurveType
      PressCurveIndex=Branch(Found)%PressureCurveIndex
      NumComps=Branch(Found)%NumOfComponents
!      IF (ALLOCATED(BComponents)) THEN
!        DEALLOCATE(BComponents)
!      ENDIF
!      ALLOCATE(BComponents(NumComps))
      BComponents(1:NumComps)=Branch(Found)%Component(1:NumComps)
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE GetInternalBranchData

SUBROUTINE GetNumSplitterMixerInConntrList(LoopName,ConnectorListName,NumSplitters,NumMixers,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   April 2005
          !       MODIFIED       Linda Lawrie - September 2005
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine returns the number of splitter and mixers in a connector list item
          ! The data is filled from the idd object 'ConnectorList'

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)              :: LoopName           ! Loop Name for this Splitter (used in error message)
  CHARACTER(len=*), INTENT(IN)              :: ConnectorListName  ! Requested Connector List Name
  INTEGER,          INTENT(OUT)             :: NumSplitters       ! Number of splitters in the loop
  INTEGER,          INTENT(OUT)             :: NumMixers          ! Number of mixers in the loop
  LOGICAL,          INTENT(INOUT)           :: ErrorsFound        ! if no connector list

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ConnNum

  IF (GetConnectorListInputFlag) THEN
    CALL GetConnectorListInput
    GetConnectorListInputFlag=.false.
  ENDIF

  NumSplitters = 0
  NumMixers = 0
  ConnNum=FindItemInList(ConnectorListName,ConnectorLists%Name,NumOfConnectorLists)

  IF (ConnNum > 0) THEN
    NumSplitters=ConnectorLists(ConnNum)%NumOfSplitters
    NumMixers=ConnectorLists(ConnNum)%NumOfMixers
  ELSE
    CALL ShowSevereError('Ref: Loop='//TRIM(LoopName)//', Connector List not found='//TRIM(ConnectorListName))
    ErrorsFound=.true.
  END IF

  RETURN

END SUBROUTINE GetNumSplitterMixerInConntrList

SUBROUTINE GetConnectorList(ConnectorListName,Connectoid,NumInList)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains connector data for requested connector list.  Also,
          ! this subroutine gets the input for the following IDD structure:
          ! ConnectorList,
          !         \memo only two connectors allowed per loop
          !         \memo if two entered, one must be Connector:Splitter and one must be Connector:Mixer
          !     A1, \field Name
          !         \required-field
          !         \reference ConnectorLists
          !     A2, \field Connector 1 Object Type
          !         \required-field
          !         \key Connector:Splitter
          !         \key Connector:Mixer
          !     A3, \field Connector 1 Name
          !         \required-field
          !     A4, \field Connector 2 Object Type
          !         \key Connector:Splitter
          !         \key Connector:Mixer
          !     A5; \field Connector 2 Name


          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)     :: ConnectorListName  ! Requested Connector List
  TYPE(ConnectorData), INTENT(INOUT) :: Connectoid         ! Returned Connector Data
  INTEGER, OPTIONAL, INTENT(IN)    :: NumInList          ! Number of the current connector in the list of connectors

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: Count                     ! Loop Counter

  IF (GetConnectorListInputFlag) THEN
    CALL GetConnectorListInput
    GetConnectorListInputFlag=.false.
  ENDIF

  IF (ConnectorListName /= '   ') THEN
    Count=FindItemInList(ConnectorListName,ConnectorLists%Name,NumOfConnectorLists)
    IF (Count == 0) THEN
      CALL ShowFatalError('GetConnectorList: Connector List not found='//TRIM(ConnectorListName))
    ENDIF
    Connectoid=ConnectorLists(Count)
    IF(PRESENT(NumInList)) THEN
     Connectoid%ConnectorType(1) = ConnectorLists(Count)%ConnectorType(NumInList)
     Connectoid%ConnectorName(1) = ConnectorLists(Count)%ConnectorName(NumInList)
     Connectoid%ConnectorType(2) = ' '
     Connectoid%ConnectorName(2) = ' '
    END IF
  ELSE
    Connectoid%Name=' '
    Connectoid%NumOfConnectors=0
    Connectoid%ConnectorType(1)=' '
    Connectoid%ConnectorType(2)=' '
    Connectoid%ConnectorName(1)=' '
    Connectoid%ConnectorName(2)=' '
  ENDIF

  RETURN

END SUBROUTINE GetConnectorList

SUBROUTINE GetLoopMixer(LoopName,ConnectorListName,MixerName,IsMixer,OutletNodeName,OutletNodeNum,NumInletNodes,  &
           InletNodeNames,InletNodeNums,ErrorsFound,ConnectorNumber,MixerNumber)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 1999
          !       MODIFIED       October 2001, Automatic Extensibility
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets the data for the requested Connector List and returns values indicating
          ! if this connector list name is a mixer or not.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits
  USE InputProcessor,  ONLY: SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)              :: LoopName           ! Loop Name for Mixer
  CHARACTER(len=*), INTENT(IN)              :: ConnectorListName  ! Requested Connector List Name
  CHARACTER(len=MaxNameLength), INTENT(OUT) :: MixerName          ! Name of Mixer
  LOGICAL, INTENT(OUT)                      :: IsMixer            ! True when Mixer is on this connector
  CHARACTER(len=MaxNameLength), INTENT(OUT) :: OutletNodeName     ! Outlet Node ID
  INTEGER, INTENT(OUT)                      :: OutletNodeNum      ! Outlet Node Number
  INTEGER, INTENT(OUT)                      :: NumInletNodes      ! Number of Inlet Nodes
  CHARACTER(len=MaxNameLength), INTENT(OUT), &
                  DIMENSION(:)              :: InletNodeNames     ! Inlet Node IDs
  INTEGER, INTENT(OUT), DIMENSION(:)        :: InletNodeNums      ! Inlet Node Numbers

  LOGICAL, INTENT(INOUT) :: ErrorsFound
  INTEGER, OPTIONAL, INTENT(IN)             :: ConnectorNumber    ! number of the current item in connector list
  INTEGER, OPTIONAL, INTENT(INOUT)            :: MixerNumber        ! Mixer number for this specific splitter

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: Count                     ! Loop Counter
  INTEGER  :: Loop                      ! Loop Counter
  TYPE (ConnectorData) :: Connectoid    ! Connector Data
  TYPE (ComponentData), ALLOCATABLE, &
         DIMENSION(:)  :: BComponents   ! Branch Component Data
  INTEGER NumComps                      ! Number of Components on this Branch
  REAL(r64) MaxFlowRate                      ! Branch Max Flow Rate
  INTEGER :: PressCurveType
  INTEGER :: PressCurveIndex
  LOGICAL :: errFlag                     ! Error flag from RegisterNodeConnection
  INTEGER :: NumParams
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers

  IF (GetMixerInputFlag) THEN
    CALL GetMixerInput
    GetMixerInputFlag=.false.
  ENDIF

  CALL GetConnectorList(ConnectorListName,Connectoid,ConnectorNumber)
  IF (SameString(Connectoid%ConnectorType(1),cMIXER)) THEN
    Count=FindItemInList(Connectoid%ConnectorName(1),Mixers%Name,NumMixers)
    IF(PRESENT(MixerNumber)) MixerNumber = MixerNumber + 1
    IF (Count == 0) THEN
      CALL ShowFatalError('GetLoopMixer: No Mixer Found='//TRIM(Connectoid%ConnectorName(1)))
    ENDIF
  ELSEIF (SameString(Connectoid%ConnectorType(2),cMIXER)) THEN
    Count=FindItemInList(Connectoid%ConnectorName(2),Mixers%Name,NumMixers)
    IF (Count == 0) THEN
      CALL ShowFatalError('GetLoopMixer: No Mixer Found='//TRIM(Connectoid%ConnectorName(2)))
    ENDIF
  ELSE
    Count=0
  ENDIF

  ! Set defaults for later error potential
  IsMixer=.false.
  MixerName=Blank
  OutletNodeName=Blank
  OutletNodeNum=0
  NumInletNodes=0
  InletNodeNames=Blank
  InletNodeNums=0

  IF (Count /= 0) THEN   ! Build up Output list(s). For each component(?)

                         ! The inlet nodes for the mixer will be the last "outlet" node of
                         ! each corresponding inlet branch.  The outlet node for the mixer
                         ! will be the first "inlet" node of the outlet branch since that
                         ! would be the first node on the branch.
    MixerName=Mixers(Count)%Name
    IsMixer=.true.
    ! The number of "components" on a Mixer is the number of branches.  This is the number of alpha arguments -1.
    CALL GetObjectDefMaxArgs('Branch',NumParams,NumAlphas,NumNumbers)
    ALLOCATE(BComponents(NumAlphas-1))
    errFlag=.false.
    CALL GetInternalBranchData(LoopName,Mixers(Count)%OutletBranchName,MaxFlowRate,PressCurveType,PressCurveIndex, &
                               NumComps,BComponents,errFlag)
    IF (errFlag) THEN
      CALL ShowContinueError('..occurs for Connector:Mixer Name='//Mixers(Count)%Name)
      ErrorsFound=.true.
    ENDIF
    IF (NumComps > 0) THEN
      OutletNodeName=BComponents(1)%InletNodeName
      OutletNodeNum=BComponents(1)%InletNode
      NumInletNodes=Mixers(Count)%NumInletBranches
           ! Register this node connection because the mixer gets node information indirectly from the branch
      errFlag=.false.
      CALL RegisterNodeConnection(OutletNodeNum,NodeID(OutletNodeNum),'Connector:Mixer',MixerName,  &
           ValidConnectionTypes(NodeConnectionType_Outlet),1,ObjectIsNotParent,errFlag)

      IF (NumInletNodes > SIZE(InletNodeNames) .or. NumInletNodes > SIZE(InletNodeNums)) THEN
        CALL ShowSevereError('GetLoopMixer: Connector:Mixer='//TRIM(MixerName)//' contains too many inlets for size of '//  &
                            'Inlet Array.')
        CALL ShowContinueError('Max array size='//TRIM(TrimSigDigits(SIZE(InletNodeNames)))//  &
                                 ', Mixer statement inlets='//TRIM(TrimSigDigits(NumInletNodes)))
        CALL ShowFatalError('Program terminates due to preceding condition.')
      ENDIF
      InletNodeNums=0
      InletNodeNames=Blank

      DO Loop=1,Mixers(Count)%NumInletBranches
        CALL GetInternalBranchData(LoopName,Mixers(Count)%InletBranchNames(Loop),MaxFlowRate,PressCurveType,PressCurveIndex, &
                                   NumComps,BComponents,ErrorsFound)
        IF (NumComps > 0) THEN
          InletNodeNames(Loop)=BComponents(NumComps)%OutletNodeName
          InletNodeNums(Loop)=BComponents(NumComps)%OutletNode
           ! Register this node connection because the mixer gets node information indirectly from the branch
          errFlag=.false.
          CALL RegisterNodeConnection(InletNodeNums(Loop),NodeID(InletNodeNums(Loop)),'Connector:Mixer',MixerName,  &
               ValidConnectionTypes(NodeConnectionType_Inlet),1,ObjectIsNotParent,errFlag)
        ENDIF
      ENDDO
    ELSE
      ! Set so cascading errors don't happen?
      IsMixer=.false.
    ENDIF
    DEALLOCATE(BComponents)
  ENDIF

  RETURN

END SUBROUTINE GetLoopMixer

SUBROUTINE GetLoopSplitter(LoopName,ConnectorListName,SplitterName,IsSplitter,InletNodeName,InletNodeNum,NumOutletNodes,  &
                           OutletNodeNames,OutletNodeNums,ErrorsFound,ConnectorNumber,SplitterNumber)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 1999
          !       MODIFIED       October 2001, Automatic Extensibility
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets the data for the requested Connector List and returns values indicating
          ! if this connector list name is a splitter or not.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)              :: LoopName           ! Loop Name for this Splitter
  CHARACTER(len=*), INTENT(IN)              :: ConnectorListName  ! Requested Connector List Name
  CHARACTER(len=MaxNameLength), INTENT(OUT) :: SplitterName       ! Name of Splitter
  LOGICAL, INTENT(OUT)                      :: IsSplitter         ! True if splitter on this connector list
  CHARACTER(len=MaxNameLength), INTENT(OUT) :: InletNodeName      ! Inlet Node ID
  INTEGER, INTENT(OUT)                      :: InletNodeNum       ! Inlet Node Number
  INTEGER, INTENT(OUT)                      :: NumOutletNodes     ! Number of Outlet Nodes
  CHARACTER(len=MaxNameLength), INTENT(OUT), &
                  DIMENSION(:)              :: OutletNodeNames    ! Outlet Node IDs
  INTEGER, INTENT(OUT), DIMENSION(:)        :: OutletNodeNums     ! Outlet Node Numbers

  LOGICAL, INTENT(INOUT) :: ErrorsFound
  INTEGER, OPTIONAL, INTENT(IN)             :: ConnectorNumber    ! number of the current item in connector list
  INTEGER, OPTIONAL, INTENT(INOUT)            :: SplitterNumber     ! splitter number for this specific splitter

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: Count                     ! Loop Counter
  INTEGER  :: Loop                      ! Loop Counter
  TYPE (ConnectorData) :: Connectoid    ! Connector Data
  TYPE (ComponentData), ALLOCATABLE, &
         DIMENSION(:)  :: BComponents   ! Branch Component Data
  INTEGER NumComps                      ! Number of Components on this Branch
  REAL(r64) MaxFlowRate                      ! Branch Max Flow Rate
  INTEGER :: PressCurveType
  INTEGER :: PressCurveIndex
  LOGICAL :: errFlag                     ! Error flag from RegisterNodeConnection
  INTEGER :: NumParams
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers

  IF (GetSplitterInputFlag) THEN
    CALL GetSplitterInput
    GetSplitterInputFlag=.false.
  ENDIF

  IF (ConnectorListName == Blank) THEN
    CALL ShowSevereError('GetLoopSplitter: ConnectorListName is blank.  LoopName='//TRIM(LoopName))
    CALL ShowFatalError('Program terminates due to previous condition.')
  ENDIF
  CALL GetConnectorList(ConnectorListName,Connectoid,ConnectorNumber)
  IF (SameString(Connectoid%ConnectorType(1) , cSPLITTER)) THEN
    Count=FindItemInList(Connectoid%ConnectorName(1),Splitters%Name,NumSplitters)
    IF(PRESENT(SplitterNumber)) SplitterNumber = SplitterNumber + 1
    IF (Count == 0) THEN
      CALL ShowFatalError('GetLoopSplitter: No Splitter Found='//TRIM(Connectoid%ConnectorName(1)))
    ENDIF
  ELSEIF (SameString(Connectoid%ConnectorType(2) , cSPLITTER)) THEN
    Count=FindItemInList(Connectoid%ConnectorName(2),Splitters%Name,NumSplitters)
    IF (Count == 0) THEN
      CALL ShowFatalError('GetLoopSplitter: No Splitter Found='//TRIM(Connectoid%ConnectorName(2)))
    ENDIF
  ELSE
    Count=0
  ENDIF

    ! Default for any errors
  SplitterName=Blank
  IsSplitter=.false.
  InletNodeName=Blank
  InletNodeNum=0
  NumOutletNodes=0
  OutletNodeNames=Blank
  OutletNodeNums=0

  IF (Count /= 0) THEN   ! Build up Output list(s). For each component(?)

                         ! The inlet node for the splitter will be the last "outlet" node of the inlet
                         ! branch. The outlet nodes for the splitter will be the first "inlet" node of
                         ! each corresponding outlet branch since that would be the first node on the branch.

    SplitterName=Splitters(Count)%Name
    IsSplitter=.true.
    ! The number of "components" on a Splitter is the number of branches.  This is the number of alpha arguments -1.
    CALL GetObjectDefMaxArgs('Branch',NumParams,NumAlphas,NumNumbers)
    ALLOCATE(BComponents(NumAlphas-1))
    errFlag=.false.
    CALL GetInternalBranchData(LoopName,Splitters(Count)%InletBranchName,MaxFlowRate,PressCurveType,PressCurveIndex, &
                               NumComps,BComponents,errFlag)
    IF (errFlag) THEN
      CALL ShowContinueError('..occurs for Splitter Name='//TRIM(Splitters(Count)%Name))
      ErrorsFound=.true.
    ENDIF
    IF (NumComps > 0) THEN
      InletNodeName=BComponents(NumComps)%OutletNodeName
      InletNodeNum=BComponents(NumComps)%OutletNode
      NumOutletNodes=Splitters(Count)%NumOutletBranches
           ! Register this node connection because the splitter gets node information indirectly from the branch
      errFlag=.false.
      CALL RegisterNodeConnection(InletNodeNum,NodeID(InletNodeNum),'Connector:Splitter',SplitterName,  &
           ValidConnectionTypes(NodeConnectionType_Inlet),1,ObjectIsNotParent,errFlag)

      IF (NumOutletNodes > SIZE(OutletNodeNames) .or. NumOutletNodes > SIZE(OutletNodeNums)) THEN
        CALL ShowSevereError('GetLoopSplitter: Connector:Splitter='//TRIM(SplitterName)// &
                             ' contains too many outlets for size of '//  &
                            'Outlet Array.')
        CALL ShowContinueError('Max array size='//TRIM(TrimSigDigits(SIZE(OutletNodeNames)))//  &
                                 ', Splitter statement outlets='//TRIM(TrimSigDigits(NumOutLetNodes)))
        CALL ShowFatalError('Program terminates due to preceding condition.')
      ENDIF
      OutletNodeNums=0
      OutletNodeNames=Blank

      DO Loop=1,Splitters(Count)%NumOutletBranches
        CALL GetInternalBranchData(LoopName,Splitters(Count)%OutletBranchNames(Loop),MaxFlowRate,PressCurveType,PressCurveIndex,&
                                   NumComps,BComponents,ErrorsFound)
        IF (NumComps > 0) THEN
          OutletNodeNames(Loop)=BComponents(1)%InletNodeName
          OutletNodeNums(Loop)=BComponents(1)%InletNode
           ! Register this node connection because the splitter gets node information indirectly from the branch
          errFlag=.false.
          CALL RegisterNodeConnection(OutletNodeNums(Loop),NodeID(OutletNodeNums(Loop)),'Connector:Splitter',SplitterName,  &
               ValidConnectionTypes(NodeConnectionType_Outlet),1,ObjectIsNotParent,errFlag)
        ENDIF
      ENDDO
    ELSE
      !  Set so cascading errors don't happen
      IsSplitter=.false.
    ENDIF
    DEALLOCATE(BComponents)
  ENDIF

  RETURN

END SUBROUTINE GetLoopSplitter

FUNCTION GetFirstBranchInletNodeName(BranchListName) RESULT(InletNodeName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function uses the branch structure to obtain the inlet node
          ! of the first branch from referenced Branch List.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: BranchListName  ! Branch List name to search
  CHARACTER(len=MaxNameLength) :: InletNodeName   ! Inlet node name of first branch in branch list

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found1          ! Pointer to Branch List Name
  INTEGER :: Found2          ! Pointer to Branch data

  IF (GetBranchListInputFlag) THEN
    GetBranchListInputFlag=.false.
    CALL GetBranchListInput
  ENDIF

  Found1=FindItemInList(BranchListName,BranchList%Name,NumOfBranchLists)
  IF (Found1 == 0) THEN
    CALL ShowSevereError('GetFirstBranchInletNodeName: BranchList="'//TRIM(BranchListName)//'", not a valid BranchList Name')
    InletNodeName='Invalid Node Name'
  ELSE
    Found2=FindItemInList(BranchList(Found1)%BranchNames(1),Branch%Name,NumOfBranches)
    IF (Found2 == 0) THEN
      CALL ShowSevereError('GetFirstBranchInletNodeName: BranchList="'//TRIM(BranchListName)//'", Branch="'//  &
                          TRIM(BranchList(Found1)%BranchNames(1))//'" not a valid Branch Name')
      InletNodeName='Invalid Node Name'
    ELSE
      InletNodeName=Branch(Found2)%Component(1)%InletNodeName
    ENDIF
  ENDIF

  RETURN

END FUNCTION GetFirstBranchInletNodeName

FUNCTION GetLastBranchOutletNodeName(BranchListName) RESULT(OutletNodeName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function uses the branch structure to obtain the outlet node
          ! of the last branch from referenced Branch List.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: BranchListName  ! Branch List name to search
  CHARACTER(len=MaxNameLength) :: OutletNodeName  ! Outlet node name of last branch in branch list

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found1          ! Pointer to Branch List Name
  INTEGER :: Found2          ! Pointer to Branch data

  IF (GetBranchListInputFlag) THEN
    GetBranchListInputFlag=.false.
    CALL GetBranchListInput
  ENDIF

  Found1=FindItemInList(BranchListName,BranchList%Name,NumOfBranchLists)
  IF (Found1 == 0) THEN
    CALL ShowSevereError('GetLastBranchOutletNodeName: BranchList="'//TRIM(BranchListName)//'", not a valid BranchList Name')
    OutletNodeName='Invalid Node Name'
  ELSE
    Found2=FindItemInList(BranchList(Found1)%BranchNames(BranchList(Found1)%NumOfBranchNames),Branch%Name,NumOfBranches)
    IF (Found2 == 0) THEN
      CALL ShowSevereError('GetLastBranchOutletNodeName: BranchList="'//TRIM(BranchListName)//'", Branch="'//  &
                          TRIM(BranchList(Found1)%BranchNames(BranchList(Found1)%NumOfBranchNames))//'" not a valid Branch Name')
      OutletNodeName='Invalid Node Name'
    ELSE
      OutletNodeName=Branch(Found2)%Component(Branch(Found2)%NumOfComponents)%OutletNodeName
    ENDIF
  ENDIF

  RETURN

END FUNCTION GetLastBranchOutletNodeName

SUBROUTINE CheckSystemBranchFlow(SystemType,SystemName,BranchFlow,BranchFanFlow,ErrFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   August 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is used to check the branch flow rate with respect to system flow rate

          ! METHODOLOGY EMPLOYED:
          ! Obtains branch and branch fan flow rate.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataHVACGlobals,    ONLY: SmallAirVolFlow
  USE General,            ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: SystemType    ! type of air loop equipment
  CHARACTER(len=*), INTENT(IN) :: SystemName    ! name of air loop equipment
  REAL(r64),INTENT(INOUT)      :: BranchFlow    ! branch volumetric flow rate [m3/s]
  REAL(r64), INTENT(IN)        :: BranchFanFlow ! branch flow rate [m3/s]
  LOGICAL, INTENT(INOUT)       :: ErrFound      ! logical error flag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: BranchNum          ! Index to branch on air loop
  INTEGER   :: SizeBranch         ! size of branch list
  LOGICAL   :: OASysFlag          ! TRUE when outdoor air system exists
  CHARACTER(len=MaxNameLength) :: BranchName  ! name of air loop branch

  IF (GetBranchInputFlag) THEN
    GetBranchInputFlag=.false.
    CALL GetBranchInput
  ENDIF

  SizeBranch = SIZE(Branch)
  BranchNum = GetAirBranchIndex(TRIM(SystemType),TRIM(SystemName))
  BranchName = ' '
  BranchFlow = 0.0d0
  ErrFound = .FALSE.
  OASysFlag = .FALSE.

  IF(BranchNum .GT. 0 .AND. BranchNum .LE. SizeBranch)THEN
    BranchFlow = Branch(BranchNum)%MaxFlowRate
    BranchName = Branch(BranchNum)%Name
  ELSE
    ErrFound = .TRUE.
    CALL ShowSevereError('CheckSystemBranchFlow: Branch index not found = '//TrimSigDigits(BranchNum,0))
    CALL ShowContinueError('Branch search for system type and name = '//TRIM(SystemType)//' "'//TRIM(SystemName)//'"')
  END IF

  IF(BranchFanFlow .GT. 0.0d0 .AND. .NOT. ErrFound)THEN
    IF(BranchFlow .NE. Autosize)THEN
      CALL CheckBranchForOASys(TRIM(SystemType),TRIM(SystemName), OASysFlag, ErrFound)
      IF(ABS(BranchFlow-BranchFanFlow) .GT. SmallAirVolFlow .AND. OASysFlag)THEN
        CALL ShowWarningError('Branch maximum flow rate differs from system flow rate.')
        CALL ShowContinueError('Branch = '//TRIM(BranchName)//' has volume flow rate = '// &
                               TRIM(TrimSigDigits(BranchFlow,6))//' m3/s.')
        CALL ShowContinueError('System = '//TRIM(SystemType)//' "'//TRIM(SystemName)//'" has volume flow rate = '// &
            TRIM(TrimSigDigits(BranchFanFlow,6))//' m3/s.')
        CALL ShowContinueError('A branch flow rate that is different from the system flow rate can cause'// &
                             ' discrepancies with outdoor air control.')
      END IF
    END IF
  END IF

  RETURN

END SUBROUTINE CheckSystemBranchFlow

!==================================================================================
!   Routines that get the input for the internal branch management structure
!==================================================================================

SUBROUTINE GetBranchInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 1999
          !       MODIFIED       October 2001, Automatic Extensibility
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the following IDD structure:
          ! Branch,
          !         \extensible:5 Just duplicate last 5 fields and \ comments (changing numbering, please)
          !         \memo List components on the branch in simulation and connection order
          !         \memo Note: this should NOT include splitters or mixers which define
          !         \memo endpoints of branches
          !    A1,  \field Name
          !         \required-field
          !         \reference Branches
          !    N1, \field Maximum Flow Rate
          !         \default 0
          !         \units m3/s
          !         \minimum 0
          !         \autosizable
          !    A2, \field Pressure Curve Name
          !         \type object-list
          !         \reference AllCurves
          !    A3, \field Component 1 Object Type
          !         \required-field
          !    A4, \field Component 1 Name
          !         \required-field
          !    A5, \field Component 1 Inlet Node Name
          !         \required-field
          !    A6, \field Component 1 Outlet Node Name
          !         \required-field
          !    A7, \field Component 1 Branch Control Type
          !         \required-field
          !        \type choice
          !        \key Active
          !        \key Passive
          !        \key SeriesActive
          !        \key Bypass
          !        \note for ACTIVE, Component tries to set branch flow and turns off branch if the component is off
          !        \note for PASSIVE, Component does not try to set branch flow
          !        \note for SERIESACTIVE, component is active but does not turn off branch when the component is off
          !        \note for BYPASS,  Component designates a loop bypass

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE CurveManager, ONLY: GetPressureCurveTypeAndIndex
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetBranchInput: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: GetInputFlag=.true.   ! Set for first time call
  INTEGER       :: Count                 ! Loop Counter
  INTEGER       :: BCount                ! Actual Num of Branches
  INTEGER       :: Comp                  ! Loop Counter
  INTEGER       :: Loop                  ! Loop Counter
  INTEGER       :: NumNodes              ! Number of Nodes from NodeInputManager
  INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeNums     ! Possible Array of Node Numbers (only 1 allowed)
  LOGICAL       :: ErrFound              ! Flag for error detection
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  INTEGER       :: NumInComps            ! Number of components actually verified (no SPLITTER or MIXER allowed)
  INTEGER NumAlphas                      ! Used to retrieve names from IDF
  CHARACTER(len=MaxNameLength), ALLOCATABLE, &
                 DIMENSION(:):: Alphas   ! Used to retrieve names from IDF
  INTEGER NumNumbers                     ! Used to retrieve numbers from IDF
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers ! Used to retrieve numbers from IDF
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFields
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFields
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks
  INTEGER                         :: IOStat  ! Could be used in the Get Routines, not currently checked
  INTEGER :: NumParams
  INTEGER ConnectionType  ! Used to pass variable node connection type to GetNodeNums
  INTEGER PressureCurveType
  INTEGER PressureCurveIndex

    IF (GetInputFlag) THEN
      CurrentModuleObject='Branch'
      NumOfBranches=GetNumObjectsFound(CurrentModuleObject)
      IF (NumOfBranches > 0) THEN
        ALLOCATE(Branch(NumOfBranches))
        Branch%AssignedLoopName=Blank
        ErrFound=.false.
        CALL GetObjectDefMaxArgs('NodeList',NumParams,NumAlphas,NumNumbers)
        ALLOCATE(NodeNums(NumParams))
        NodeNums=0
        CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNumbers)
        ALLOCATE(Alphas(NumAlphas))
        Alphas=' '
        ALLOCATE(Numbers(NumNumbers))
        Numbers=0.0d0
        ALLOCATE(cAlphaFields(NumAlphas))
        cAlphaFields=' '
        ALLOCATE(cNumericFields(NumNumbers))
        cNumericFields=' '
        ALLOCATE(lAlphaBlanks(NumAlphas))
        lAlphaBlanks=.true.
        ALLOCATE(lNumericBlanks(NumNumbers))
        lNumericBlanks=.true.
        BCount=0
        DO Count=1,NumOfBranches
          CALL GetObjectItem(CurrentModuleObject,Count,Alphas,NumAlphas,Numbers,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
          IsNotOK=.false.
          IsBlank=.false.
          CALL VerifyName(Alphas(1),Branch%Name,BCount,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
          IF (IsNotOK) THEN
            ErrFound=.true.
            IF (IsBlank) THEN
              CYCLE
            ELSE
              Alphas(1)=TRIM(Alphas(1))//'--dup'
            ENDIF
          ENDIF
          BCount=BCount+1
          Branch(BCount)%Name=Alphas(1)
          Branch(BCount)%MaxFlowRate=Numbers(1)
          CALL GetPressureCurveTypeAndIndex(Alphas(2), PressureCurveType, PressureCurveIndex)
          IF (PressureCurveType == PressureCurve_Error) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", invalid data.')
            CALL ShowContinueError('..Invalid '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//'".')
            CALL ShowContinueError('This curve could not be found in the input deck.  Ensure that this curve has been entered')
            CALL ShowContinueError(' as either a Curve:Functional:PressureDrop or one of Curve:{Linear,Quadratic,Cubic,Exponent}')
            CALL ShowContinueError('This error could be caused by a misspelled curve name')
            ErrFound = .TRUE.
          END IF
          Branch(BCount)%PressureCurveType = PressureCurveType
          Branch(BCount)%PressureCurveIndex = PressureCurveIndex
          Branch(BCount)%NumOfComponents=(NumAlphas-2)/5
          IF (Branch(BCount)%NumOfComponents*5 /= (NumAlphas-2)) Branch(BCount)%NumOfComponents=Branch(BCount)%NumOfComponents+1
          NumInComps=Branch(BCount)%NumOfComponents
          ALLOCATE(Branch(BCount)%Component(Branch(BCount)%NumOfComponents))
          Comp=1
          DO Loop=3,NumAlphas,5
            IF (SameString(Alphas(Loop),cSPLITTER) .or. SameString(Alphas(Loop),cMIXER)) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", invalid data.')
              CALL ShowContinueError('Connector:Splitter/Connector:Mixer not allowed in object '//  &
                 TRIM(CurrentModuleObject))
              ErrFound=.true.
              CYCLE
            ENDIF
            IF (Comp > NumInComps) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", invalid data.')
              CALL ShowContinueError('...Number of Arguments indicate ['//trim(RoundSigDigits(NumInComps))// &
                  '], but count of fields indicates ['//trim(RoundSigDigits(Comp))//']')
              CALL ShowContinueError('...examine '//trim(CurrentModuleObject)//' carefully.')
              CYCLE
            ENDIF
            Branch(BCount)%Component(Comp)%CType=Alphas(Loop)
            Branch(BCount)%Component(Comp)%Name=Alphas(Loop+1)
            CALL ValidateComponent(Alphas(Loop),Alphas(Loop+1),IsNotOK,TRIM(CurrentModuleObject))
            IF (IsNotOK) THEN
              CALL ShowContinueError('Occurs on '//TRIM(CurrentModuleObject)//'='//TRIM(Alphas(1)))
              ErrFound=.true.
            ENDIF
            Branch(BCount)%Component(Comp)%InletNodeName=Alphas(Loop+2)
                 ! If first component on branch, then inlet node is inlet to branch, otherwise node is internal
            IF (Loop == 3) THEN
              ConnectionType = NodeConnectionType_Inlet
            ELSE
              ConnectionType = NodeConnectionType_Internal
            ENDIF
            IF (.not. lAlphaBlanks(Loop+2)) THEN
              CALL GetNodeNums(Branch(BCount)%Component(Comp)%InletNodeName,NumNodes,NodeNums,ErrFound,NodeType_Unknown, &
                   TRIM(CurrentModuleObject),Branch(BCount)%Name,ConnectionType,1,ObjectIsParent,  &
                   InputFieldName=cAlphaFields(Loop+2))
              IF (NumNodes > 1) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", invalid data.')
                CALL ShowContinueError('..invalid '//TRIM(cAlphaFields(Loop+2))//'="'//  &
                   TRIM(Branch(BCount)%Component(Comp)%InletNodeName)//'" must be a single node - appears to be a list.')
                CALL ShowContinueError('Occurs on '//TRIM(cAlphaFields(Loop))//'="'//TRIM(Alphas(Loop))//'", '//  &
                   TRIM(cAlphaFields(Loop+1))//'="'//TRIM(Alphas(Loop+1))//'".')
                ErrFound=.true.
              ELSE
                Branch(BCount)%Component(Comp)%InletNode=NodeNums(1)
              ENDIF
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", invalid data.')
              CALL ShowContinueError('blank required field: '//TRIM(cAlphaFields(Loop+2)))
              CALL ShowContinueError('Occurs on '//TRIM(cAlphaFields(Loop))//'="'//TRIM(Alphas(Loop))//'", '//  &
                 TRIM(cAlphaFields(Loop+1))//'="'//TRIM(Alphas(Loop+1))//'".')
              ErrFound=.true.
            ENDIF
            Branch(BCount)%Component(Comp)%OutletNodeName=Alphas(Loop+3)
                 ! If last component on branch, then outlet node is outlet from branch, otherwise node is internal
            IF (Loop == NumAlphas-4) THEN
              ConnectionType = NodeConnectionType_Outlet
            ELSE
              ConnectionType = NodeConnectionType_Internal
            ENDIF
            IF (.not. lAlphaBlanks(Loop+3)) THEN
              CALL GetNodeNums(Branch(BCount)%Component(Comp)%OutletNodeName,NumNodes,NodeNums,ErrFound,NodeType_Unknown, &
                   TRIM(CurrentModuleObject),Branch(BCount)%Name,ConnectionType,1,ObjectIsParent,  &
                   InputFieldName=cAlphaFields(Loop+3))
              IF (NumNodes > 1) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", invalid data.')
                CALL ShowContinueError('..invalid '//TRIM(cAlphaFields(Loop+2))//'="'//  &
                   TRIM(Branch(BCount)%Component(Comp)%InletNodeName)//'" must be a single node - appears to be a list.')
                CALL ShowContinueError('Occurs on '//TRIM(cAlphaFields(Loop))//'="'//TRIM(Alphas(Loop))//'", '//  &
                   TRIM(cAlphaFields(Loop+1))//'="'//TRIM(Alphas(Loop+1))//'".')
                ErrFound=.true.
              ELSE
                Branch(BCount)%Component(Comp)%OutletNode=NodeNums(1)
              ENDIF
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", invalid data.')
              CALL ShowContinueError('blank required field: '//TRIM(cAlphaFields(Loop+3)))
              CALL ShowContinueError('Occurs on '//TRIM(cAlphaFields(Loop))//'="'//TRIM(Alphas(Loop))//'", '//  &
                 TRIM(cAlphaFields(Loop+1))//'="'//TRIM(Alphas(Loop+1))//'".')
              ErrFound=.true.
            ENDIF

            IF (.not. lAlphaBlanks(Loop) .and. .not. lAlphaBlanks(Loop+1) .and.   &  !no blanks in required field set
                .not. lAlphaBlanks(Loop+2) .and. .not. lAlphaBlanks(Loop+3))       &
              CALL SetUpCompSets(TRIM(CurrentModuleObject),Branch(BCount)%Name, &
                     Alphas(Loop),Alphas(Loop+1),Alphas(Loop+2),Alphas(Loop+3))

!            deprecated control type, was using (Alphas(Loop+4))

            Comp=Comp+1
          ENDDO
          Branch(BCount)%NumOfComponents=NumInComps
        ENDDO

        NumOfBranches=BCount
        DEALLOCATE(NodeNums)
        DEALLOCATE(Alphas)
        DEALLOCATE(Numbers)
        DEALLOCATE(cAlphaFields)
        DEALLOCATE(cNumericFields)
        DEALLOCATE(lAlphaBlanks)
        DEALLOCATE(lNumericBlanks)
        IF (ErrFound) THEN
          CALL ShowSevereError(RoutineName//' Invalid '//TRIM(CurrentModuleObject)//  &
             ' Input, preceding condition(s) will likely cause termination.')
          InvalidBranchDefinitions=.true.
        ENDIF
        CALL TestInletOutletNodes(ErrFound)
        GetInputFlag=.false.
      ENDIF
    ENDIF

  RETURN

END SUBROUTINE GetBranchInput

SUBROUTINE GetBranchListInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the branch list input and fills up the structures for
          ! branch lists.
          ! This subroutine gets the input for the following IDD structure:
          ! BRANCH LIST,
          !  \extensible:1 Just duplicate last field and \ comments (changing numbering, please)
          !  \memo Branches MUST be listed in flow order: inlet branch, then parallel branches, then outlet branch.
          !  \memo Branches are simulated in the order listed.  Branch names cannot be duplicated within a single branch list.
          !    A1, \field Branch List Name
          !        \required-field
          !        \reference BranchLists
          !    A2, \field Branch Name 1
          !        \required-field
          !        \type object-list
          !        \object-list Branches
          !    A3, \field Branch Name 2
          !        \type object-list
          !        \object-list Branches


          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetBranchListInput: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count                           ! Loop Counter
  INTEGER BCount                          ! Actual Branch List Count
  INTEGER Loop                            ! Loop Counter
  INTEGER Found                           ! Points to correct Branch List/Branch
  LOGICAL       :: ErrFound               ! True when error has occured (cannot find Branch List)
                                          ! Following are needed because routine calls GetBranchInput
                                          ! which would overwrite the module Alphas and NumAlphas
  LOGICAL IsNotOK                         ! Flag for "VerifyName" routine
  LOGICAL IsBlank                         ! Flag for "blank" name
  INTEGER NumAlphas                       ! Used to retrieve Branch list from IDF
  CHARACTER(len=MaxNameLength), ALLOCATABLE, &
                 DIMENSION(:):: Alphas   ! Used to retrieve names from IDF
  INTEGER NumNumbers
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers ! Not used in this object
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFields
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFields
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks
  INTEGER                         :: IOStat  ! Could be used in the Get Routines, not currently checked
  INTEGER :: NumParams
  CHARACTER(len=MaxNameLength) :: TestName

  ErrFound=.false.
  CurrentModuleObject='BranchList'
  NumOfBranchLists=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(BranchList(NumOfBranchLists))
  BranchList%LoopName=Blank
  BranchList%LoopType=Blank
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNumbers)
  ALLOCATE(Alphas(NumAlphas))
  Alphas=' '
  ALLOCATE(Numbers(NumNumbers))
  Numbers=0.0d0
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNumbers))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(NumNumbers))
  lNumericBlanks=.true.

  IF (NumNumbers > 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//  &
       ' Object definition contains numbers, cannot be decoded by GetBranchListInput routine.')
    ErrFound=.true.
  ENDIF
  BCount=0
  DO Count=1,NumOfBranchLists
    CALL GetObjectItem(CurrentModuleObject,Count,Alphas,NumAlphas,Numbers,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),BranchList%Name,BCount,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrFound=.true.
      CYCLE
    ENDIF

    BCount=BCount+1
    BranchList(BCount)%Name=Alphas(1)
    BranchList(BCount)%NumOfBranchNames=NumAlphas-1
    ALLOCATE(BranchList(BCount)%BranchNames(NumAlphas-1))
    IF (BranchList(BCount)%NumOfBranchNames == 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(BranchList(BCount)%Name)//  &
         '", No branch names entered.')
      ErrFound=.true.
    ELSE
      BranchList(BCount)%BranchNames(1:NumAlphas-1)=Alphas(2:NumAlphas)
      DO Loop=1,BranchList(BCount)%NumOfBranchNames
        ! If NumOfBranches = 0 then Branches havent been read yet.
        IF (NumOfBranches == 0) THEN
           CALL GetBranchInput
        ENDIF
        IF (BranchList(BCount)%BranchNames(Loop) /= Blank) THEN
           Found=FindItemInList(BranchList(BCount)%BranchNames(Loop),Branch%Name,NumOfBranches)
           IF (Found == 0) THEN
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(BranchList(BCount)%Name)//  &
                 '", invalid data.')
             CALL ShowContinueError('..invalid Branch Name not found="'//  &
                 TRIM(BranchList(BCount)%BranchNames(Loop))//'".')
             ErrFound=.true.
           ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDDO

    ! Check for duplicate names specified in Branch Lists
  DO Count=1,NumOfBranchLists
    IF (BranchList(Count)%NumOfBranchNames == 0) CYCLE
    TestName=BranchList(Count)%BranchNames(1)
    DO Loop=2,BranchList(Count)%NumOfBranchNames
      IF (TestName /= BranchList(Count)%BranchNames(Loop)) CYCLE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(BranchList(BCount)%Name)//  &
                '", invalid data.')
      CALL ShowContinueError('..invalid: duplicate branch name specified in the list.')
      CALL ShowContinueError('..Branch Name='//TRIM(TestName))
      CALL ShowContinueError('..Branch Name #'//TRIM(TrimSigDigits(Loop))//  &
         ' is duplicate.')
      ErrFound=.true.
    ENDDO
  ENDDO

  IF (ErrFound) THEN
    CALL ShowSevereError(RoutineName//' Invalid Input -- preceding condition(s) will likely cause termination.')
  ENDIF
  NumOfBranchLists=BCount
  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  RETURN

END SUBROUTINE GetBranchListInput

SUBROUTINE GetConnectorListInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains connector list input from IDF.
          ! ConnectorList,
          !         \memo only two connectors allowed per loop
          !         \memo if two entered, one must be Connector:Splitter and one must be Connector:Mixer
          !     A1, \field Name
          !         \required-field
          !         \reference ConnectorLists
          !     A2, \field Connector 1 Object Type
          !         \required-field
          !         \key Connector:Splitter
          !         \key Connector:Mixer
          !     A3, \field Connector 1 Name
          !         \required-field
          !     A4, \field Connector 2 Object Type
          !         \key Connector:Splitter
          !         \key Connector:Mixer
          !     A5; \field Connector 2 Name
          !  This is in the process of possibly being extended, thus the code herein.


          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY : SameString

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
  INTEGER :: Count                     ! Loop Counter
  INTEGER :: NumAlphas                      ! Used to retrieve names from IDF
  CHARACTER(len=MaxNameLength), ALLOCATABLE, &
                 DIMENSION(:):: Alphas   ! Used to retrieve names from IDF
  INTEGER NumNumbers                     ! Used to retrieve numbers from IDF
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers ! Used to retrieve numbers from IDF
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFields
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFields
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks
  INTEGER                         :: IOStat  ! Could be used in the Get Routines, not currently checked
  INTEGER :: NumParams
  INTEGER :: NumConnectors
  INTEGER :: CCount
  INTEGER :: Arg
  INTEGER :: SplitNum
  INTEGER :: MixerNum
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: BranchNames
  INTEGER :: NumBranchNames
  LOGICAL :: ErrorsFound
  INTEGER :: Loop
  INTEGER :: Loop1
  INTEGER :: Loop2
  LOGICAL :: CurMixer
  LOGICAL :: CurSplitter
  INTEGER :: TestNum
  LOGICAL :: MatchFound


  IF (.not. GetConnectorListInputFlag) RETURN
  ErrorsFound=.false.
  CurrentModuleObject='ConnectorList'
  NumOfConnectorLists=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(ConnectorLists(NumOfConnectorLists))
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNumbers)
  IF (NumAlphas /= 5 .or. NumNumbers /= 0) THEN
    CALL ShowWarningError('GetConnectorList: Illegal "extension" to '//TRIM(CurrentModuleObject)//' object. '//  &
       'Internal code does not support > 2 connectors (Connector:Splitter and Connector:Mixer)')
  ENDIF
  ALLOCATE(Alphas(NumAlphas))
  Alphas=' '
  ALLOCATE(Numbers(NumNumbers))
  Numbers=0.0d0
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNumbers))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(NumNumbers))
  lNumericBlanks=.true.
  DO Count=1,NumOfConnectorLists
    CALL GetObjectItem(CurrentModuleObject,Count,Alphas,NumAlphas,Numbers,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    ConnectorLists(Count)%Name=Alphas(1)
    NumConnectors=(NumAlphas-1)/2  ! potential problem if puts in type but not name
    IF (MOD(NumAlphas-1,2) /= 0) NumConnectors=NumConnectors+1
    ConnectorLists(Count)%NumOfConnectors=NumConnectors
    ALLOCATE(ConnectorLists(Count)%ConnectorType(NumConnectors))
    ALLOCATE(ConnectorLists(Count)%ConnectorName(NumConnectors))
    ALLOCATE(ConnectorLists(Count)%ConnectorMatchNo(NumConnectors))
    ConnectorLists(Count)%ConnectorType='UNKNOWN'
    ConnectorLists(Count)%ConnectorName='UNKNOWN'
    ConnectorLists(Count)%ConnectorMatchNo=0
    ConnectorLists(Count)%NumOfSplitters=0
    ConnectorLists(Count)%NumOfMixers=0

    CCount=0
    DO Arg=2,NumAlphas,2
      CCount=CCount+1
      IF (SameString(Alphas(Arg) , cSPLITTER)) THEN
        ConnectorLists(Count)%ConnectorType(CCount)=Alphas(Arg)(1:30)
        ConnectorLists(Count)%NumOfSplitters=ConnectorLists(Count)%NumOfSplitters+1
      ELSEIF (SameString(Alphas(Arg) , cMIXER)) THEN
        ConnectorLists(Count)%ConnectorType(CCount)=Alphas(Arg)(1:30)
        ConnectorLists(Count)%NumOfMixers=ConnectorLists(Count)%NumOfMixers+1
      ELSE
        CALL ShowWarningError('GetConnectorListInput: Invalid '//TRIM(cAlphaFields(Arg))//'='// &
          TRIM(Alphas(Arg))//' in '//TRIM(CurrentModuleObject)//'='//TRIM(Alphas(1)))
      ENDIF
      ConnectorLists(Count)%ConnectorName(CCount)=Alphas(Arg+1)
    ENDDO
  ENDDO
  GetConnectorListInputFlag=.false.
  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  ! Validity checks on Connector Lists
  IF (GetSplitterInputFlag) THEN
    CALL GetSplitterInput
    GetSplitterInputFlag=.false.
  ENDIF
  IF (GetMixerInputFlag) THEN
    CALL GetMixerInput
    GetMixerInputFlag=.false.
  ENDIF

  SplitNum=0
  MixerNum=0
  DO Count=1,NumOfConnectorLists
    IF (ConnectorLists(Count)%NumOfConnectors <= 1) CYCLE  ! Air Loop only has one.
    IF (ConnectorLists(Count)%NumOfConnectors > 2) CYCLE  ! Rules not clear for this case
    DO Loop=1,ConnectorLists(Count)%NumOfConnectors
      IF (ConnectorLists(Count)%ConnectorMatchNo(Loop) /= 0) CYCLE
      IF (SameString(ConnectorLists(Count)%ConnectorType(Loop) , cSPLITTER)) THEN
        CurSplitter=.true.
        CurMixer=.false.
        SplitNum=FindItemInList(ConnectorLists(Count)%ConnectorName(Loop),Splitters%Name,NumSplitters)
        ! Following code sets up branch names to be matched from Splitter/Mixer data structure
        IF (SplitNum == 0) THEN
          CALL ShowSevereError('Invalid Connector:Splitter(none)='//TRIM(ConnectorLists(Count)%ConnectorName(Loop)//  &
                       ', referenced by '//TRIM(CurrentModuleObject)//'='//TRIM(ConnectorLists(Count)%Name)))
          ErrorsFound=.true.
          CYCLE
        ENDIF
        NumBranchNames=Splitters(SplitNum)%NumOutletBranches
        ALLOCATE(BranchNames(NumBranchNames))
        BranchNames=Splitters(SplitNum)%OutletBranchNames
      ELSEIF (SameString(ConnectorLists(Count)%ConnectorType(Loop) , cMIXER)) THEN
        CurSplitter=.true.
        CurMixer=.false.
        MixerNum=FindItemInList(ConnectorLists(Count)%ConnectorName(Loop),Mixers%Name,NumMixers)
        IF (MixerNum == 0) THEN
          CALL ShowSevereError('Invalid Connector:Mixer(none)='//TRIM(ConnectorLists(Count)%ConnectorName(Loop))//  &
                       ', referenced by '//TRIM(CurrentModuleObject)//'='//TRIM(ConnectorLists(Count)%Name))
          ErrorsFound=.true.
          CYCLE
        ENDIF
        NumBranchNames=Mixers(MixerNum)%NumInletBranches
        ALLOCATE(BranchNames(NumBranchNames))
        BranchNames=Mixers(MixerNum)%InletBranchNames
      ELSE
        CYCLE
      ENDIF
      ! Try to match mixer to splitter
      DO Loop1=Loop+1,ConnectorLists(Count)%NumOfConnectors
        IF (CurMixer .and. .NOT. SameString(ConnectorLists(Count)%ConnectorType(Loop1) , cSPLITTER)) CYCLE
        IF (CurSplitter .and. .NOT. SameString(ConnectorLists(Count)%ConnectorType(Loop1) , cMIXER)) CYCLE
        IF (ConnectorLists(Count)%ConnectorMatchNo(Loop1) /= 0) CYCLE
        SELECT CASE (CurSplitter)
          CASE(.true.)
            ! Current "item" is a splitter, candidate is a mixer.
            MixerNum=FindItemInList(ConnectorLists(Count)%ConnectorName(Loop1),Mixers%Name,NumMixers)
            IF (MixerNum == 0) CYCLE
            IF (Mixers(MixerNum)%NumInletBranches /= NumBranchNames) CYCLE
            MatchFound=.true.
            DO Loop2=1,Mixers(MixerNum)%NumInletBranches
              TestNum=FindItemInList(Mixers(MixerNum)%InletBranchNames(Loop2),BranchNames,NumBranchNames)
              IF (TestNum == 0) THEN
                MatchFound=.false.
                EXIT
              ENDIF
            ENDDO
            IF (MatchFound) THEN
              ConnectorLists(Count)%ConnectorMatchNo(Loop1)=MixerNum
              ConnectorLists(Count)%ConnectorMatchNo(Loop)=SplitNum
            ENDIF
          CASE(.false.)
            ! Current "item" is a splitter, candidate is a mixer.
            SplitNum=FindItemInList(ConnectorLists(Count)%ConnectorName(Loop1),Splitters%Name,NumSplitters)
            IF (SplitNum == 0) CYCLE
            IF (Splitters(SplitNum)%NumOutletBranches /= NumBranchNames) CYCLE
            MatchFound=.true.
            DO Loop2=1,Splitters(SplitNum)%NumOutletBranches
              TestNum=FindItemInList(Splitters(SplitNum)%OutletBranchNames(Loop2),BranchNames,NumBranchNames)
              IF (TestNum == 0) THEN
                MatchFound=.false.
                EXIT
              ENDIF
            ENDDO
            IF (MatchFound) THEN
              ConnectorLists(Count)%ConnectorMatchNo(Loop1)=SplitNum
              ConnectorLists(Count)%ConnectorMatchNo(Loop)=MixerNum
            ENDIF
        END SELECT
      ENDDO
      DEALLOCATE(BranchNames)
    ENDDO
  ENDDO

  DO Count=1,NumOfConnectorLists
    IF (ConnectorLists(Count)%NumOfConnectors <= 1) CYCLE  ! Air Loop only has one.
    IF (ConnectorLists(Count)%NumOfConnectors > 2) CYCLE  ! Rules not clear
    DO Loop=1,ConnectorLists(Count)%NumOfConnectors
      IF (ConnectorLists(Count)%ConnectorMatchNo(Loop) /= 0) CYCLE
      !  = 0, not matched.
      CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//'='//TRIM(ConnectorLists(Count)%Name))
      CALL ShowContinueError('...Item='//TRIM(ConnectorLists(Count)%ConnectorName(Loop))//', Type='//  &
                    TRIM(ConnectorLists(Count)%ConnectorType(Loop))//' was not matched.')
      IF (SameString(ConnectorLists(Count)%ConnectorType(Loop),'Connector:Splitter')) THEN
        CALL ShowContinueError('The BranchList for this Connector:Splitter does not match the BranchList'//  &
           ' for its corresponding Connector:Mixer.')
      ELSE
        CALL ShowContinueError('The BranchList for this Connector:Mixer does not match the BranchList'//  &
           ' for its corresponding Connector:Splitter.')
      ENDIF
      ErrorsFound=.true.
    ENDDO
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetConnectorListInput: Program terminates for preceding conditions.')
  ENDIF

  RETURN

END SUBROUTINE GetConnectorListInput

SUBROUTINE GetSplitterInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Sept 2005 (moved from GetLoopSplitter)
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the Splitter data that is used in Loops.
          ! IDD structure:
          ! Connector:Splitter,
          !   \min-fields 3
          !        \extensible:1 Just duplicate last field and \ comments (changing numbering, please)
          !        \memo Split one air/water stream into N outlet streams.  Branch names cannot be duplicated
          !        \memo within a single Splitter list.
          !    A1, \field Name
          !         \required-field
          !    A2, \field Inlet Branch Name
          !         \required-field
          !         \type object-list
          !         \object-list Branches
          !    A3, \field Outlet Branch 1 Name
          !         \required-field
          !         \type object-list
          !         \object-list Branches
          !    A4, \field Outlet Branch 2 Name
          !         \type object-list
          !         \object-list Branches


          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NumAlphas                      ! Used to retrieve names from IDF
  CHARACTER(len=MaxNameLength), ALLOCATABLE, &
                 DIMENSION(:):: Alphas   ! Used to retrieve names from IDF
  INTEGER NumNumbers                     ! Used to retrieve numbers from IDF
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers ! Used to retrieve numbers from IDF
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFields
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFields
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks
  INTEGER                         :: IOStat  ! Could be used in the Get Routines, not currently checked
  INTEGER    :: NumParams
  INTEGER :: Loop
  INTEGER :: Loop1
  INTEGER :: Count
  INTEGER :: Found
  LOGICAL :: ErrorsFound=.false.
  CHARACTER(len=MaxNameLength) :: TestName
  CHARACTER(len=MaxNameLength) :: BranchListName
  CHARACTER(len=6) :: FoundSupplyDemandAir
  CHARACTER(len=6) :: SaveSupplyDemandAir
  CHARACTER(len=9) :: FoundLoop
  CHARACTER(len=9) :: SaveLoop
  LOGICAL  :: MatchedLoop

  IF (.not. GetSplitterInputFlag) RETURN
  CurrentModuleObject = cSPLITTER
  NumSplitters=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(Splitters(NumSplitters))
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNumbers)
  ALLOCATE(Alphas(NumAlphas))
  Alphas=' '
  ALLOCATE(Numbers(NumNumbers))
  Numbers=0.0d0
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNumbers))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(NumNumbers))
  lNumericBlanks=.true.
  DO Count=1,NumSplitters
    CALL GetObjectItem(CurrentModuleObject,Count,Alphas,NumAlphas,Numbers,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    Splitters(Count)%Name=Alphas(1)
    Splitters(Count)%InletBranchName=Alphas(2)
    Splitters(Count)%NumOutletBranches=NumAlphas-2
    ALLOCATE(Splitters(Count)%OutletBranchNames(Splitters(Count)%NumOutletBranches))
    DO Loop=1,Splitters(Count)%NumOutletBranches
      Splitters(Count)%OutletBranchNames(Loop)=Alphas(2+Loop)
    ENDDO
  ENDDO
  GetSplitterInputFlag=.false.
  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  ! More validity -- check splitter "names" against branches.
  IF (.not. GetBranchInputFlag) THEN
    CALL GetBranchInput
    GetBranchInputFlag=.false.
  ENDIF
  DO Count=1,NumSplitters
    Found=FindItemInList(Splitters(Count)%InletBranchName,Branch%Name,NumOfBranches)
    IF (Found == 0) THEN
      CALL ShowSevereError('GetSplitterInput: Invalid Branch='//TRIM(Splitters(Count)%InletBranchName)//  &
                   ', referenced as Inlet Branch to '//TRIM(CurrentModuleObject)//'='//TRIM(Splitters(Count)%Name))
      ErrorsFound=.true.
    ENDIF
    DO Loop=1,Splitters(Count)%NumOutletBranches
      Found=FindItemInList(Splitters(Count)%OutletBranchNames(Loop),Branch%Name,NumOfBranches)
      IF (Found == 0) THEN
        CALL ShowSevereError('GetSplitterInput: Invalid Branch='//TRIM(Splitters(Count)%OutletBranchNames(Loop))//  &
                     ', referenced as Outlet Branch # '//TRIM(TrimSigDigits(Loop))//  &
                     ' to '//TRIM(CurrentModuleObject)//'='//TRIM(Splitters(Count)%Name))
        ErrorsFound=.true.
      ENDIF
    ENDDO
  ENDDO

    ! Check for duplicate names specified in Splitters
  DO Count=1,NumSplitters
    TestName=Splitters(Count)%InletBranchName
    DO Loop=1,Splitters(Count)%NumOutletBranches
      IF (TestName /= Splitters(Count)%OutletBranchNames(Loop)) CYCLE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'='//TRIM(Splitters(Count)%Name)//  &
         ' specifies an outlet node name the same as the inlet node.')
      CALL ShowContinueError('..Inlet Node='//TRIM(TestName))
      CALL ShowContinueError('..Outlet Node #'//TRIM(TrimSigDigits(Loop))//  &
         ' is duplicate.')
      ErrorsFound=.true.
    ENDDO
    DO Loop=1,Splitters(Count)%NumOutletBranches
      DO Loop1=Loop+1,Splitters(Count)%NumOutletBranches
        IF (Splitters(Count)%OutletBranchNames(Loop) /= Splitters(Count)%OutletBranchNames(Loop1)) CYCLE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'='//TRIM(Splitters(Count)%Name)//  &
           ' specifies duplicate outlet nodes in its outlet node list.')
        CALL ShowContinueError('..Outlet Node #'//TRIM(TrimSigDigits(Loop))//  &
           ' Name='//TRIM(Splitters(Count)%OutletBranchNames(Loop)))
        CALL ShowContinueError('..Outlet Node #'//TRIM(TrimSigDigits(Loop))//  &
           ' is duplicate.')
        ErrorsFound=.true.
      ENDDO
    ENDDO
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetSplitterInput: Fatal Errors Found in '//TRIM(CurrentModuleObject)//', program terminates.')
  ENDIF

  !  Everything supposed to be good.  Now make sure all branches in Splitter on same side of loop.
  SaveSupplyDemandAir=Blank
  DO Count=1,NumSplitters
    ! 2.  Find the branch name in branchlist
    TestName=Splitters(Count)%InletBranchName
    BranchListName=Blank
    DO Loop1=1,NumOfBranchLists
      IF (ANY(BranchList(Loop1)%BranchNames == TestName)) THEN
        BranchListName=BranchList(Loop1)%Name
        EXIT
      ENDIF
    ENDDO

    IF (BranchListName /= Blank) THEN
      FoundSupplyDemandAir=Blank
      FoundLoop=Blank
      MatchedLoop=.false.
      ! 3.  Find the loop and type
      CALL FindAirPlantCondenserLoopFromBranchList(BranchListName,FoundLoop,FoundSupplyDemandAir,MatchedLoop)
      IF (MatchedLoop) THEN
        SaveSupplyDemandAir=FoundSupplyDemandAir
        SaveLoop=FoundLoop
      ELSE
        CALL ShowSevereError('GetSplitterInput: Inlet Splitter Branch="'//TRIM(TestName)//'" and BranchList="'//  &
                          TRIM(BranchListName)//'" not matched to a Air/Plant/Condenser Loop')
        CALL ShowContinueError('...and therefore, not a valid Loop Splitter.')
        CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Splitters(Count)%Name))
        ErrorsFound=.true.
      ENDIF
    ELSE
      CALL ShowSevereError('GetSplitterInput: Inlet Splitter Branch="'//TRIM(TestName)//'" not on BranchList')
      CALL ShowContinueError('...and therefore, not a valid Loop Splitter.')
      CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Splitters(Count)%Name))
      ErrorsFound=.true.
    ENDIF
    DO Loop=1,Splitters(Count)%NumOutletBranches
      TestName=Splitters(Count)%OutletBranchNames(Loop)
      BranchListName=Blank
      DO Loop1=1,NumOfBranchLists
        IF (ANY(BranchList(Loop1)%BranchNames == TestName)) THEN
          BranchListName=BranchList(Loop1)%Name
          EXIT
        ENDIF
      ENDDO

      IF (BranchListName /= Blank) THEN
        FoundSupplyDemandAir=Blank
        FoundLoop=Blank
        MatchedLoop=.false.
        ! 3.  Find the loop and type
        CALL FindAirPlantCondenserLoopFromBranchList(BranchListName,FoundLoop,FoundSupplyDemandAir,MatchedLoop)
        IF (MatchedLoop) THEN
          IF (SaveSupplyDemandAir /= FoundSupplyDemandAir .or. SaveLoop /= FoundLoop) THEN
            CALL ShowSevereError('GetSplitterInput: Outlet Splitter Branch="'//TRIM(TestName)//  &
               '" does not match types of Inlet Branch.')
            CALL ShowContinueError('...Inlet Branch is on "'//TRIM(SaveLoop)//'" on "'//  &
               TRIM(SaveSupplyDemandAir)//'" side.')
            CALL ShowContinueError('...Outlet Branch is on "'//TRIM(FoundLoop)//'" on "'//  &
               TRIM(FoundSupplyDemandAir)//'" side.')
            CALL ShowContinueError('...All branches in Loop Splitter must be on same kind of loop and supply/demand side.')
            CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Splitters(Count)%Name))
            ErrorsFound=.true.
          ENDIF
        ELSE
          CALL ShowSevereError('GetSplitterInput: Outlet Splitter Branch="'//TRIM(TestName)//'" and BranchList="'//  &
                            TRIM(BranchListName)//'" not matched to a Air/Plant/Condenser Loop')
          CALL ShowContinueError('...and therefore, not a valid Loop Splitter.')
          CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Splitters(Count)%Name))
          ErrorsFound=.true.
        ENDIF
      ELSE
        CALL ShowSevereError('GetSplitterInput: Outlet Splitter Branch="'//TRIM(TestName)//'" not on BranchList')
        CALL ShowContinueError('...and therefore, not a valid Loop Splitter')
        CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Splitters(Count)%Name))
        ErrorsFound=.true.
      ENDIF
    ENDDO

  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetSplitterInput: Fatal Errors Found in '//TRIM(CurrentModuleObject)//', program terminates.')
  ENDIF

  RETURN

END SUBROUTINE GetSplitterInput

SUBROUTINE GetMixerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Sept 2005 (moved from GetLoopMixer)
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the Mixer data that is used in Loops.
          ! IDD Structure:
          ! Connector:Mixer,
          !   \min-fields 3
          !        \extensible:1 Just duplicate last field and \ comments (changing numbering, please)
          !        \memo Mix N inlet air/water streams into one.  Branch names cannot be duplicated within
          !        \memo a single mixer list.
          !    A1 , \field Name
          !         \required-field
          !    A2 , \field Outlet Branch Name
          !         \required-field
          !         \type object-list
          !         \object-list Branches
          !    A3 , \field Inlet Branch 1 Name
          !         \required-field
          !         \type object-list
          !         \object-list Branches
          !    A4 , \field Inlet Branch 2 Name
          !         \type object-list
          !         \object-list Branches

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NumAlphas                      ! Used to retrieve names from IDF
  CHARACTER(len=MaxNameLength), ALLOCATABLE, &
                 DIMENSION(:):: Alphas   ! Used to retrieve names from IDF
  INTEGER NumNumbers                     ! Used to retrieve numbers from IDF
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers ! Used to retrieve numbers from IDF
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFields
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFields
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks
  INTEGER                         :: IOStat  ! Could be used in the Get Routines, not currently checked
  INTEGER :: NumParams
  INTEGER :: Loop
  INTEGER :: Loop1
  INTEGER :: Count
  INTEGER :: Found
  LOGICAL :: ErrorsFound=.false.
  CHARACTER(len=MaxNameLength) :: TestName
  CHARACTER(len=MaxNameLength) :: BranchListName
  CHARACTER(len=6) :: FoundSupplyDemandAir
  CHARACTER(len=6) :: SaveSupplyDemandAir
  CHARACTER(len=9) :: FoundLoop
  CHARACTER(len=9) :: SaveLoop
  LOGICAL  :: MatchedLoop

  IF (.not. GetMixerInputFlag) RETURN

  CurrentModuleObject = cMIXER

  NumMixers=GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(Mixers(NumMixers))
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNumbers)
  ALLOCATE(Alphas(NumAlphas))
  Alphas=' '
  ALLOCATE(Numbers(NumNumbers))
  Numbers=0.0d0
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNumbers))
  cNumericFields=' '
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(NumNumbers))
  lNumericBlanks=.true.
  DO Count=1,NumMixers
    CALL GetObjectItem(CurrentModuleObject,Count,Alphas,NumAlphas,Numbers,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                   AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    Mixers(Count)%Name=Alphas(1)
    Mixers(Count)%OutletBranchName=Alphas(2)
    Mixers(Count)%NumInletBranches=NumAlphas-2
    ALLOCATE(Mixers(Count)%InletBranchNames(Mixers(Count)%NumInletBranches))
    DO Loop=1,Mixers(Count)%NumInletBranches
      Mixers(Count)%InletBranchNames(Loop)=Alphas(2+Loop)
    ENDDO
  ENDDO
  GetMixerInputFlag=.false.
  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  ! More validity -- check mixer "names" against branches.
  IF (.not. GetBranchInputFlag) THEN
    CALL GetBranchInput
    GetBranchInputFlag=.false.
  ENDIF
  DO Count=1,NumMixers
    Found=FindItemInList(Mixers(Count)%OutletBranchName,Branch%Name,NumOfBranches)
    IF (Found == 0) THEN
      CALL ShowSevereError('GetMixerInput: Invalid Branch='//TRIM(Mixers(Count)%OutletBranchName)//  &
                   ', referenced as Outlet Branch in '//TRIM(CurrentModuleObject)//'='//TRIM(Mixers(Count)%Name))
      ErrorsFound=.true.
    ENDIF
    DO Loop=1,Mixers(Count)%NumInletBranches
      Found=FindItemInList(Mixers(Count)%InletBranchNames(Loop),Branch%Name,NumOfBranches)
      IF (Found == 0) THEN
        CALL ShowSevereError('GetMixerInput: Invalid Branch='//TRIM(Mixers(Count)%InletBranchNames(Loop))//  &
                     ', referenced as Inlet Branch # '//TRIM(TrimSigDigits(Loop))//  &
                     ' in ' //TRIM(CurrentModuleObject)//'='//TRIM(Mixers(Count)%Name))
        ErrorsFound=.true.
      ENDIF
    ENDDO
  ENDDO

    ! Check for duplicate names specified in Mixer
  DO Count=1,NumMixers
    TestName=Mixers(Count)%OutletBranchName
    DO Loop=1,Mixers(Count)%NumInletBranches
      IF (TestName /= Mixers(Count)%InletBranchNames(Loop)) CYCLE
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'='//TRIM(Mixers(Count)%Name)//  &
         ' specifies an inlet node name the same as the outlet node.')
      CALL ShowContinueError('..Outlet Node='//TRIM(TestName))
      CALL ShowContinueError('..Inlet Node #'//TRIM(TrimSigDigits(Loop))//  &
         ' is duplicate.')
      ErrorsFound=.true.
    ENDDO
    DO Loop=1,Mixers(Count)%NumInletBranches
      DO Loop1=Loop+1,Mixers(Count)%NumInletBranches
        IF (Mixers(Count)%InletBranchNames(Loop) /= Mixers(Count)%InletBranchNames(Loop1)) CYCLE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'='//TRIM(Mixers(Count)%Name)//  &
           ' specifies duplicate inlet nodes in its inlet node list.')
        CALL ShowContinueError('..Inlet Node #'//TRIM(TrimSigDigits(Loop))//  &
           ' Name='//TRIM(Mixers(Count)%InletBranchNames(Loop)))
        CALL ShowContinueError('..Inlet Node #'//TRIM(TrimSigDigits(Loop))//  &
           ' is duplicate.')
        ErrorsFound=.true.
      ENDDO
    ENDDO
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetMixerInput: Fatal Errors Found in '//TRIM(CurrentModuleObject)//', program terminates.')
  ENDIF

  !  Everything supposed to be good.  Now make sure all branches in Splitter on same side of loop.
  SaveSupplyDemandAir=Blank
  DO Count=1,NumMixers
    ! 2.  Find the branch name in branchlist
    TestName=Mixers(Count)%OutletBranchName
    BranchListName=Blank
    DO Loop1=1,NumOfBranchLists
      IF (ANY(BranchList(Loop1)%BranchNames == TestName)) THEN
        BranchListName=BranchList(Loop1)%Name
        EXIT
      ENDIF
    ENDDO

    IF (BranchListName /= Blank) THEN
      FoundSupplyDemandAir=Blank
      FoundLoop=Blank
      MatchedLoop=.false.
      ! 3.  Find the loop and type
      CALL FindAirPlantCondenserLoopFromBranchList(BranchListName,FoundLoop,FoundSupplyDemandAir,MatchedLoop)
      IF (MatchedLoop) THEN
        SaveSupplyDemandAir=FoundSupplyDemandAir
        SaveLoop=FoundLoop
      ELSE
        CALL ShowSevereError('GetMixerInput: Outlet Mixer Branch="'//TRIM(TestName)//'" and BranchList="'//  &
                          TRIM(BranchListName)//'" not matched to a Air/Plant/Condenser Loop')
        CALL ShowContinueError('...and therefore, not a valid Loop Mixer.')
        CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Mixers(Count)%Name))
        ErrorsFound=.true.
      ENDIF
    ELSE
      CALL ShowSevereError('GetMixerInput: Outlet Mixer Branch="'//TRIM(TestName)//'" not on BranchList')
      CALL ShowContinueError('...and therefore, not a valid Loop Mixer.')
      CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Mixers(Count)%Name))
      ErrorsFound=.true.
    ENDIF
    DO Loop=1,Mixers(Count)%NumInletBranches
      TestName=Mixers(Count)%InletBranchNames(Loop)
      BranchListName=Blank
      DO Loop1=1,NumOfBranchLists
        IF (ANY(BranchList(Loop1)%BranchNames == TestName)) THEN
          BranchListName=BranchList(Loop1)%Name
          EXIT
        ENDIF
      ENDDO

      IF (BranchListName /= Blank) THEN
        FoundSupplyDemandAir=Blank
        FoundLoop=Blank
        MatchedLoop=.false.
        ! 3.  Find the plant loop and type
        CALL FindAirPlantCondenserLoopFromBranchList(BranchListName,FoundLoop,FoundSupplyDemandAir,MatchedLoop)
        IF (MatchedLoop) THEN
          IF (SaveSupplyDemandAir /= FoundSupplyDemandAir .or. SaveLoop /= FoundLoop) THEN
            CALL ShowSevereError('GetMixerInput: Outlet Mixer Branch="'//TRIM(TestName)//  &
               '" does not match types of Inlet Branch.')
            CALL ShowContinueError('...Outlet Branch is on "'//TRIM(SaveLoop)//'" on "'//  &
               TRIM(SaveSupplyDemandAir)//'" side.')
            CALL ShowContinueError('...Inlet Branch is on "'//TRIM(FoundLoop)//'" on "'//  &
               TRIM(FoundSupplyDemandAir)//'" side.')
            CALL ShowContinueError('...All branches in Loop Mixer must be on same kind of loop and supply/demand side.')
            CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Mixers(Count)%Name))
            ErrorsFound=.true.
          ENDIF
        ELSE
          CALL ShowSevereError('GetMixerInput: Inlet Mixer Branch="'//TRIM(TestName)//'" and BranchList="'//  &
                            TRIM(BranchListName)//'" not matched to a Air/Plant/Condenser Loop')
          CALL ShowContinueError('...and therefore, not a valid Loop Mixer.')
          CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Mixers(Count)%Name))
          ErrorsFound=.true.
        ENDIF
      ELSE
        CALL ShowSevereError('GetMixerInput: Inlet Mixer Branch="'//TRIM(TestName)//'" not on BranchList')
        CALL ShowContinueError('...and therefore, not a valid Loop Mixer')
        CALL ShowContinueError('...'//TRIM(CurrentModuleObject)//'='//TRIM(Mixers(Count)%Name))
        ErrorsFound=.true.
      ENDIF
    ENDDO
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetMixerInput: Fatal Errors Found in '//TRIM(CurrentModuleObject)//', program terminates.')
  ENDIF

  RETURN

END SUBROUTINE GetMixerInput

SUBROUTINE FindPlantLoopBranchConnection(BranchListName,FoundPlantLoopName,FoundPlantLoopNum,FoundSupplyDemand,  &
                                         FoundVolFlowRate,MatchedPlantLoop)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! An auxiliary routine locate a plant loop and type from a BranchListName

          ! METHODOLOGY EMPLOYED:
          ! Calls GetObject for PLANT LOOP

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: BranchListName
  CHARACTER(len=*), INTENT(INOUT) :: FoundPlantLoopName
  INTEGER, INTENT(INOUT)          :: FoundPlantLoopNum
  CHARACTER(len=*), INTENT(INOUT) :: FoundSupplyDemand
  REAL(r64), INTENT(INOUT)        :: FoundVolFlowRate
  LOGICAL, INTENT(INOUT)          :: MatchedPlantLoop

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Num
  INTEGER NumPlantLoops
  INTEGER NumParams
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas
  INTEGER NumAlphas
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers
  INTEGER NumNumbers
  INTEGER IOSTAT

      ! Get Inputs
  CurrentModuleObject = 'PlantLoop'

  NumPlantLoops=GetNumObjectsFound(CurrentModuleObject)
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNumbers)
  ALLOCATE(Alphas(NumAlphas))
  ALLOCATE(Numbers(NumNumbers))

  DO Num=1,NumPlantLoops
    CALL GetObjectItem(CurrentModuleObject,Num,Alphas,NumAlphas,Numbers,NumNumbers,IOSTAT)
    ! Only looking for BranchList here.
    IF (Alphas(8) == BranchListName) THEN
      FoundPlantLoopName=Alphas(1)
      FoundSupplyDemand='Supply'
      FoundVolFlowRate=Numbers(3)
      FoundPlantLoopNum=Num
      MatchedPlantLoop=.true.
      EXIT
    ELSEIF (Alphas(12) == BranchListName) THEN
      FoundPlantLoopName=Alphas(1)
      FoundSupplyDemand='Demand'
      FoundVolFlowRate=Numbers(3)
      FoundPlantLoopNum=Num
      MatchedPlantLoop=.true.
      EXIT
    ENDIF
  ENDDO

  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)

  RETURN

END SUBROUTINE FindPlantLoopBranchConnection

SUBROUTINE FindCondenserLoopBranchConnection(BranchListName,FoundCondLoopName,FoundCondLoopNum,FoundSupplyDemand,  &
                                         FoundVolFlowRate,MatchedCondLoop)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! An auxiliary routine locate a condenser loop and type from a BranchListName

          ! METHODOLOGY EMPLOYED:
          ! calls GetObject for CONDENSER LOOP

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: BranchListName
  CHARACTER(len=*), INTENT(INOUT) :: FoundCondLoopName
  INTEGER, INTENT(INOUT)          :: FoundCondLoopNum
  CHARACTER(len=*), INTENT(INOUT) :: FoundSupplyDemand
  REAL(r64), INTENT(INOUT)        :: FoundVolFlowRate
  LOGICAL, INTENT(INOUT)          :: MatchedCondLoop

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Num
  INTEGER NumCondLoops
  INTEGER NumParams
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas
  INTEGER NumAlphas
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers
  INTEGER NumNumbers
  INTEGER IOSTAT

      ! Get Inputs
  CurrentModuleObject = 'CondenserLoop'

  NumCondLoops=GetNumObjectsFound(CurrentModuleObject)
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNumbers)
  ALLOCATE(Alphas(NumAlphas))
  ALLOCATE(Numbers(NumNumbers))

  DO Num=1,NumCondLoops
    CALL GetObjectItem(CurrentModuleObject,Num,Alphas,NumAlphas,Numbers,NumNumbers,IOSTAT)
    ! Only looking for BranchList here.
    IF (Alphas(8) == BranchListName) THEN
      FoundCondLoopName=Alphas(1)
      FoundSupplyDemand='Supply'
      FoundVolFlowRate=Numbers(3)
      FoundCondLoopNum=Num
      MatchedCondLoop=.true.
      EXIT
    ELSEIF (Alphas(12) == BranchListName) THEN
      FoundCondLoopName=Alphas(1)
      FoundSupplyDemand='Demand'
      FoundVolFlowRate=Numbers(3)
      FoundCondLoopNum=Num
      MatchedCondLoop=.true.
      EXIT
    ENDIF
  ENDDO

  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)

  RETURN

END SUBROUTINE FindCondenserLoopBranchConnection

SUBROUTINE FindAirLoopBranchConnection(BranchListName,FoundAirLoopName,FoundAirLoopNum,FoundAir,  &
                                         FoundVolFlowRate,MatchedAirLoop)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! An auxiliary routine locate a Airenser loop and type from a BranchListName

          ! METHODOLOGY EMPLOYED:
          ! calls GetObject for PRIMARY AIR LOOP

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: BranchListName
  CHARACTER(len=*), INTENT(INOUT) :: FoundAirLoopName
  INTEGER, INTENT(INOUT)          :: FoundAirLoopNum
  CHARACTER(len=*), INTENT(INOUT) :: FoundAir
  REAL(r64), INTENT(INOUT)        :: FoundVolFlowRate
  LOGICAL, INTENT(INOUT)          :: MatchedAirLoop

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Num
  INTEGER NumAirLoops
  INTEGER NumParams
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas
  INTEGER NumAlphas
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers
  INTEGER NumNumbers
  INTEGER IOSTAT

      ! Get Inputs
  CurrentModuleObject = 'AirLoopHVAC'
  NumAirLoops=GetNumObjectsFound(CurrentModuleObject)
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNumbers)
  ALLOCATE(Alphas(NumAlphas))
  ALLOCATE(Numbers(NumNumbers))

  DO Num=1,NumAirLoops
    CALL GetObjectItem(CurrentModuleObject,Num,Alphas,NumAlphas,Numbers,NumNumbers,IOSTAT)
    ! Only looking for BranchList here.
    IF (Alphas(4) == BranchListName) THEN
      FoundAirLoopName=Alphas(1)
      FoundAir='Air'
      FoundVolFlowRate=Numbers(1)
      FoundAirLoopNum=Num
      MatchedAirLoop=.true.
      EXIT
    ENDIF
  ENDDO

  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)

  RETURN

END SUBROUTINE FindAirLoopBranchConnection

SUBROUTINE FindAirPlantCondenserLoopFromBranchList(BranchListName,LoopType,LoopSupplyDemandAir,MatchedLoop)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Assist in validating Loop Splitter/Mixer connections.

          ! METHODOLOGY EMPLOYED:
          ! Call two previously written subroutines that match a Branch List Name to
          ! Plant or Condenser Loop

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: BranchListName    ! Branch List Name
  CHARACTER(len=*), INTENT(INOUT) :: LoopType          ! LoopType (if found, Plant,Condenser or Air)
  CHARACTER(len=*), INTENT(INOUT) :: LoopSupplyDemandAir  ! Supply if "Supply" or Demand if "Demand" or Air if "Air"
  LOGICAL, INTENT(INOUT)          :: MatchedLoop       ! true if found

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   CHARACTER(len=MaxNameLength) :: FoundLoopName
   INTEGER :: FoundLoopNum
   REAL(r64) :: FoundLoopVolFlowRate

   LoopSupplyDemandAir=Blank
   FoundLoopName=Blank
   FoundLoopNum=0
   FoundLoopVolFlowRate=0.0d0
   MatchedLoop=.false.
   LoopType=Blank

   ! Try Plant first
   CALL FindPlantLoopBranchConnection(BranchListName,FoundLoopName,FoundLoopNum,  &
                                      LoopSupplyDemandAir,FoundLoopVolFlowRate,MatchedLoop)

   IF (MatchedLoop) LoopType='Plant'
   IF (.not. MatchedLoop) THEN   ! Try Condenser Loop
     LoopSupplyDemandAir=Blank
     FoundLoopName=Blank
     FoundLoopNum=0
     FoundLoopVolFlowRate=0.0d0
     MatchedLoop=.false.

     ! Try Condenser
     CALL FindCondenserLoopBranchConnection(BranchListName,FoundLoopName,FoundLoopNum,  &
                                      LoopSupplyDemandAir,FoundLoopVolFlowRate,MatchedLoop)
     IF (MatchedLoop) LoopType='Condenser'
   ENDIF

   IF (.not. MatchedLoop) THEN   ! Try Air Loop
     LoopSupplyDemandAir=Blank
     FoundLoopName=Blank
     FoundLoopNum=0
     FoundLoopVolFlowRate=0.0d0
     MatchedLoop=.false.

     ! Try Air
     CALL FindAirLoopBranchConnection(BranchListName,FoundLoopName,FoundLoopNum,  &
                                      LoopSupplyDemandAir,FoundLoopVolFlowRate,MatchedLoop)
     IF (MatchedLoop) LoopType='Air'
   ENDIF
  RETURN

END SUBROUTINE FindAirPlantCondenserLoopFromBranchList

!==================================================================================
!   Routines that test branch integrity
!==================================================================================

SUBROUTINE AuditBranches(mustprint,CompType,CompName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine will point out any "dangling branches" that are not included on a BranchList.
          ! Warnings are produced as the user might clutter up the input file with unused branches.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DataErrorTracking, ONLY: TotalSevereErrors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: mustprint ! true if the warning should be printed.
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: CompType ! when mustprint (ScanPlantLoop)  use CompType in error message and scan
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: CompName ! when mustprint (ScanPlantLoop)  use CompName in error message and scan

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumDanglingCount  ! when mustprint not true, count and report
  INTEGER :: BlNum   ! Branch List Counter
  INTEGER :: BrN     ! Branch Counter
  INTEGER :: CpN     ! Components on Branch
  INTEGER :: Found   ! non-zero when found
  CHARACTER(len=MaxNameLength) :: FoundBranchName  ! Branch matching compname/type
  LOGICAL :: NeverFound

  NumDanglingCount=0
  NeverFound=.true.
  DO BrN=1,NumOfBranches
    Found=0
    FoundBranchName=' '
    IF (PRESENT(CompType) .and. PRESENT(CompName)) THEN
      DO CpN=1,Branch(BrN)%NumOfComponents
        IF (.not. SameString(CompType,Branch(BrN)%Component(CpN)%CType) .or.   &
            .not. SameString(CompName,Branch(BrN)%Component(CpN)%Name) ) CYCLE
        FoundBranchName=Branch(BrN)%Name
        NeverFound=.false.
      ENDDO
    ENDIF
    DO BlNum=1,NumOfBranchLists
      Found=FindItemInList(Branch(BrN)%Name,BranchList(BlNum)%BranchNames,BranchList(BlNum)%NumOfBranchNames)
      IF (Found /= 0) EXIT
    ENDDO
    IF (Found /= 0) CYCLE
    NumDanglingCount=NumDanglingCount+1
    IF (DisplayExtraWarnings .or. mustprint) THEN
      IF (mustprint) THEN
        CALL ShowContinueError('AuditBranches: Branch="'//trim(Branch(BrN)%Name)//'" not found on any BranchLists.')
        IF (FoundBranchName /= ' ') THEN
          CALL ShowContinueError('Branch contains component, type="'//trim(CompType)//'", name="'//  &
             trim(CompName)//'"')
        ENDIF
      ELSE
        CALL ShowSevereMessage('AuditBranches: Branch="'//trim(Branch(BrN)%Name)//'" not found on any BranchLists.')
        TotalSevereErrors=TotalSevereErrors+1
      ENDIF
    ENDIF
  ENDDO
  IF (mustprint .and. NeverFound) THEN  ! this may be caught during branch input, not sure
    CALL ShowContinueError('Component, type="'//trim(CompType)//'", name="'//trim(CompName)//'" was not found on any Branch.')
    CALL ShowContinueError('Look for mistyped branch or component names/types.')
  ENDIF
  IF (.not. mustprint .and. NumDanglingCount > 0) THEN
    CALL ShowSevereMessage('AuditBranches: There are '//trim(RoundSigDigits(NumDanglingCount))//  &
       ' branch(es) that do not appear on any BranchList.')
    TotalSevereErrors=TotalSevereErrors+NumDanglingCount
    CALL ShowContinueError('Use Output:Diagnostics,DisplayExtraWarnings; for detail of each branch not on a branch list.')
  ENDIF

  RETURN

END SUBROUTINE AuditBranches

SUBROUTINE TestBranchIntegrity(ErrFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine tests branch integrity and displays the loop for each branch.
          ! Also, input and output nodes.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE NodeInputManager, ONLY: InitUniqueNodeCheck,CheckUniqueNodes,EndUniqueNodeCheck
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
  TYPE BranchUniqueNodes
    INTEGER :: NumNodes=0
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: UniqueNodeNames
  END TYPE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER Count
  INTEGER       :: MatchNode             ! Node Number for match
  CHARACTER(len=MaxNameLength) &
                        :: MatchNodeName ! Name for error message if not matched
  CHARACTER(len=MaxNameLength) &
                        :: BranchInletNodeName ! Branch Inlet Node Name
  CHARACTER(len=MaxNameLength) &
                        :: BranchOutletNodeName ! Branch Outlet Node Name
  CHARACTER(len=MaxNameLength) &
                        :: BranchLoopName ! Loop Name which Branch is part of
  CHARACTER(len=MaxNameLength) &
                        :: BranchLoopType ! Loop Type which Branch is part of
  INTEGER       :: NumErr                ! Error Counter
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: BranchReported
  TYPE(BranchUniqueNodes), DIMENSION(:), ALLOCATABLE :: BranchNodes
  INTEGER BCount
  INTEGER Found
  CHARACTER(len=20) ChrOut
  CHARACTER(len=20) ChrOut1
!  LOGICAL UniqueNodeError
  INTEGER NodeNum
  INTEGER Loop2
  LOGICAL :: IsAirBranch
  INTEGER :: BranchFluidType
  LOGICAL :: MixedFluidTypesOnBranchList
  INTEGER :: InitialBranchFluidNode
  INTEGER,ALLOCATABLE, DIMENSION(:) :: BranchFluidNodes
  INTEGER,ALLOCATABLE, DIMENSION(:) :: FoundBranches
  INTEGER,ALLOCATABLE, DIMENSION(:) :: BranchPtrs
  INTEGER :: NumNodesOnBranchList
  INTEGER :: NumFluidNodes
  CHARACTER(len=MaxNameLength) :: OriginalBranchFluidType
  CHARACTER(len=MaxNameLength) :: cBranchFluidType
  INTEGER :: Ptr
  INTEGER :: EndPtr

  ALLOCATE(BranchReported(NumOfBranches))
  BranchReported=.false.

  ! Do by Branch Lists
  CALL ShowMessage('Testing Individual Branch Integrity')
  ErrFound=.false.

  ALLOCATE(BranchNodes(NumOfBranches))

  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(OutputFileBNDetails,700)
  WRITE(ChrOut,*) NumOfBranchLists
  WRITE(OutputFileBNDetails,701) ' #Branch Lists,'//TRIM(ADJUSTL(ChrOut))
  WRITE(OutputFileBNDetails,702)
  WRITE(OutputFileBNDetails,704)

 700 FORMAT('! <#Branch Lists>,<Number of Branch Lists>')
 701 FORMAT(A)
 702 FORMAT('! <Branch List>,<Branch List Count>,<Branch List Name>,<Loop Name>,<Loop Type>,<Number of Branches>')
 704 FORMAT('! <Branch>,<Branch Count>,<Branch Name>,<Loop Name>,<Loop Type>,<Branch Inlet Node Name>,<Branch Outlet Node Name>')
 706 FORMAT('! <# Orphaned Branches>,<Number of Branches not on Branch Lists>')

  DO BCount=1,NumOfBranchLists

    WRITE(ChrOut,*) BCount
    WRITE(ChrOut1,*) BranchList(BCount)%NumOfBranchNames
    WRITE(OutputFileBNDetails,701) ' Branch List,'//TRIM(ADJUSTL(ChrOut))//','//TRIM(BranchList(BCount)%Name)//','//  &
                                   TRIM(BranchList(BCount)%LoopName)//','//TRIM(BranchList(BCount)%LoopType)//','//  &
                                   TRIM(ADJUSTL(ChrOut1))

    IsAirBranch=.false.
    BranchFluidType=NodeType_Unknown
    MixedFluidTypesOnBranchList=.false.
    NumNodesOnBranchList=0
    ALLOCATE(FoundBranches(BranchList(BCount)%NumOfBranchNames))
    FoundBranches=0
    ALLOCATE(BranchPtrs(BranchList(BCount)%NumOfBranchNames+2))
    BranchPtrs=0
    DO Count=1,BranchList(BCount)%NumOfBranchNames
      Found=FindItemInList(BranchList(BCount)%BranchNames(Count),Branch%Name,NumOfBranches)
      IF (Found > 0) THEN
        NumNodesOnBranchList=NumNodesOnBranchList+Branch(Found)%NumOfComponents*2
        FoundBranches(Count)=Found
        BranchPtrs(Count)=NumNodesOnBranchList
      ELSE
        CALL ShowSevereError('Branch not found='//TRIM(BranchList(BCount)%BranchNames(Count)))
        ErrFound=.true.
      ENDIF
    ENDDO
    BranchPtrs(BranchList(BCount)%NumOfBranchNames+1)=BranchPtrs(BranchList(BCount)%NumOfBranchNames)+1
    ALLOCATE(BranchFluidNodes(NumNodesOnBranchList))
    BranchFluidNodes=0
    OriginalBranchFluidType=Blank
    NumFluidNodes=0
    DO Count=1,BranchList(BCount)%NumOfBranchNames

      ChrOut=RoundSigDigits(Count)
!      WRITE(ChrOut,*) Count
!      ChrOut=ADJUSTL(ChrOut)

      Found=FoundBranches(Count)
      IF (Found == 0) THEN
        WRITE(OutputFileBNDetails,701) '   Branch,'//TRIM(ChrOut)//','//  &
           TRIM(BranchList(BCount)%BranchNames(Count))//'(not found),'//  &
           '**Unknown**,**Unknown**,**Unknown**,**Unknown**'
        CYCLE
      ENDIF
      BranchReported(Found)=.true.
               ! Check Branch for connections

      MatchNode=0
      InitialBranchFluidNode=0
      IF (Branch(Found)%NumOfComponents > 0) THEN
        MatchNode=Branch(Found)%Component(1)%InletNode
        MatchNodeName=Branch(Found)%Component(1)%InletNodeName
        BranchInletNodeName=Branch(Found)%Component(1)%InletNodeName
      ELSE
        CALL ShowWarningError('Branch has no components='//TRIM(Branch(Found)%Name))
      ENDIF
      NumErr=0
      DO Loop=1,Branch(Found)%NumOfComponents
        IF (Node(Branch(Found)%Component(Loop)%InletNode)%FluidType == NodeType_Air) IsAirBranch=.true.
        IF (BranchFluidType == NodeType_Unknown) THEN
          NumFluidNodes=NumFluidNodes+1
          BranchFluidNodes(NumFluidNodes)=Branch(Found)%Component(Loop)%InletNode
          BranchFluidType=Node(Branch(Found)%Component(Loop)%InletNode)%FluidType
          InitialBranchFluidNode=Branch(Found)%Component(Loop)%InletNode
          OriginalBranchFluidType=ValidNodeFluidTypes(BranchFluidType)
        ELSEIF (BranchFluidType /= Node(Branch(Found)%Component(Loop)%InletNode)%FluidType .and.  &
                Node(Branch(Found)%Component(Loop)%InletNode)%FluidType /= NodeType_Unknown ) THEN
          NumFluidNodes=NumFluidNodes+1
          BranchFluidNodes(NumFluidNodes)=Branch(Found)%Component(Loop)%InletNode
          MixedFluidTypesOnBranchList=.true.
        ELSE
          NumFluidNodes=NumFluidNodes+1
          BranchFluidNodes(NumFluidNodes)=Branch(Found)%Component(Loop)%InletNode
        ENDIF
        IF (Node(Branch(Found)%Component(Loop)%OutletNode)%FluidType == NodeType_Air) IsAirBranch=.true.
        IF (BranchFluidType == NodeType_Unknown) THEN
          NumFluidNodes=NumFluidNodes+1
          BranchFluidNodes(NumFluidNodes)=Branch(Found)%Component(Loop)%InletNode
          BranchFluidType=Node(Branch(Found)%Component(Loop)%OutletNode)%FluidType
          InitialBranchFluidNode=Branch(Found)%Component(Loop)%OutletNode
          OriginalBranchFluidType=ValidNodeFluidTypes(BranchFluidType)
        ELSEIF (BranchFluidType /= Node(Branch(Found)%Component(Loop)%OutletNode)%FluidType .and.  &
                Node(Branch(Found)%Component(Loop)%OutletNode)%FluidType /= NodeType_Unknown ) THEN
          NumFluidNodes=NumFluidNodes+1
          BranchFluidNodes(NumFluidNodes)=Branch(Found)%Component(Loop)%OutletNode
          MixedFluidTypesOnBranchList=.true.
        ELSE
          NumFluidNodes=NumFluidNodes+1
          BranchFluidNodes(NumFluidNodes)=Branch(Found)%Component(Loop)%OutletNode
        ENDIF
        IF (Branch(Found)%Component(Loop)%InletNode /= MatchNode) THEN
          CALL ShowSevereError('Error Detected in BranchList='//TRIM(BranchList(BCount)%Name))
          CALL ShowContinueError('Actual Error occurs in Branch='//TRIM(Branch(Found)%Name))
          CALL ShowContinueError('Branch Outlet does not match Inlet, Outlet='//TRIM(MatchNodeName))
          CALL ShowContinueError('Inlet Name='//TRIM(Branch(Found)%Component(Loop)%InletNodeName))
          ErrFound=.true.
          NumErr=NumErr+1
        ELSE
          MatchNode=Branch(Found)%Component(Loop)%OutletNode
          MatchNodeName=Branch(Found)%Component(Loop)%OutletNodeName
        ENDIF
      ENDDO
      Branch(Found)%FluidType=BranchFluidType
      IF (IsAirBranch .and. Branch(Found)%MaxFlowRate == 0.0d0) THEN
        CALL ShowSevereError('Branch='//TRIM(Branch(Found)%Name)//' is an air branch with zero max flow rate.')
        ErrFound=.true.
      ENDIF
      BranchOutletNodeName=MatchNodeName
      IF (Branch(Found)%AssignedLoopName == Blank) THEN
        BranchLoopName = '**Unknown**'
        BranchLoopType = '**Unknown**'
      ELSEIF (TRIM(Branch(Found)%AssignedLoopName) ==  TRIM(BranchList(BCount)%LoopName)) THEN
        BranchLoopName = TRIM(BranchList(BCount)%LoopName)
        BranchLoopType = TRIM(BranchList(BCount)%LoopType)
      ELSE
        BranchLoopName = TRIM(Branch(Found)%AssignedLoopName)
        BranchLoopType = '**Unknown**'
      ENDIF
      WRITE(OutputFileBNDetails,701) '   Branch,'//TRIM(ChrOut)//','//TRIM(Branch(Found)%Name)//','//  &
        TRIM(BranchLoopName)//','//TRIM(BranchLoopType)//','//  &
        TRIM(BranchInletNodeName)//','//TRIM(BranchOutletNodeName)
    ENDDO
    IF (MixedFluidTypesOnBranchList) THEN
      CALL ShowSevereError('BranchList='//TRIM(BranchList(BCount)%Name)//' has mixed fluid types in its nodes.')
      Errfound=.true.
      IF (OriginalBranchFluidType == Blank) OriginalBranchFluidType='**Unknown**'
      CALL ShowContinueError('Initial Node='//trim(NodeID(InitialBranchFluidNode))//  &
           ', Fluid Type='//trim(OriginalBranchFluidType))
      CALL ShowContinueError('BranchList Topology - Note nodes which do not match that fluid type:')
      Ptr=1
      EndPtr=BranchPtrs(1)
      DO Loop=1,BranchList(BCount)%NumOfBranchNames
        IF (FoundBranches(Loop) /= 0) THEN
          CALL ShowContinueError('..Branch='//trim(Branch(FoundBranches(Loop))%Name))
        ELSE
          CALL ShowContinueError('..Illegal Branch='//trim(BranchList(BCount)%BranchNames(Loop)))
          CYCLE
        ENDIF
        DO Loop2=Ptr,EndPtr
          cBranchFluidType=ValidNodeFluidTypes(Node(BranchFluidNodes(Loop2))%FluidType)
          IF (cBranchFluidType == Blank) cBranchFluidType='**Unknown**'
          CALL ShowContinueError('....Node='//trim(NodeID(BranchFluidNodes(Loop2)))//', Fluid Type='//  &
            trim(cBranchFluidType))
        ENDDO
        Ptr=EndPtr+1
        EndPtr=BranchPtrs(Loop+1)
      ENDDO
    ENDIF
    DEALLOCATE(BranchFluidNodes)
    DEALLOCATE(BranchPtrs)
    DEALLOCATE(FoundBranches)
  ENDDO

  ! Build node names in branches
  DO Count=1,NumOfBranches
    ALLOCATE(BranchNodes(Count)%UniqueNodeNames(Branch(Count)%NumOfComponents*2))
    BranchNodes(Count)%UniqueNodeNames=Blank
    NodeNum=0
    DO Loop=1,Branch(Count)%NumOfComponents
      Found=FindItemInList(Branch(Count)%Component(Loop)%InletNodeName,BranchNodes(Count)%UniqueNodeNames,NodeNum)
      IF (Found == 0) THEN
        NodeNum=NodeNum+1
        BranchNodes(Count)%UniqueNodeNames(NodeNum)=Branch(Count)%Component(Loop)%InletNodeName
      ENDIF
      Found=FindItemInList(Branch(Count)%Component(Loop)%OutletNodeName,BranchNodes(Count)%UniqueNodeNames,NodeNum)
      IF (Found == 0) THEN
        NodeNum=NodeNum+1
        BranchNodes(Count)%UniqueNodeNames(NodeNum)=Branch(Count)%Component(Loop)%OutletNodeName
      ENDIF
    ENDDO
    BranchNodes(Count)%NumNodes=NodeNum
  ENDDO
  ! Check Uniqueness branch to branch
  DO Count=1,NumOfBranches
    DO Loop=Count+1,NumOfBranches
      DO Loop2=1,BranchNodes(Count)%NumNodes
        Found=FindItemInList(BranchNodes(Count)%UniqueNodeNames(Loop2),  &
                           BranchNodes(Loop)%UniqueNodeNames,BranchNodes(Loop)%NumNodes)
        IF (Found /= 0) THEN
          CALL ShowSevereError('Non-unique node name found, name='//TRIM(BranchNodes(Count)%UniqueNodeNames(Loop2)))
          CALL ShowContinueError('..1st occurence in Branch='//TRIM(Branch(Count)%Name))
          CALL ShowContinueError('..duplicate occurrence in Branch='//TRIM(Branch(Loop)%Name))
          ErrFound=.true.
        ENDIF
      ENDDO
    ENDDO
  ENDDO
  DO Count=1,NumOfBranches
    DEALLOCATE(BranchNodes(Count)%UniqueNodeNames)
  ENDDO
  DEALLOCATE(BranchNodes)

  BCount=0
  DO Count=1,NumOfBranches
    IF (BranchReported(Count)) CYCLE
    BCount=BCount+1
  ENDDO
  IF (BCount > 0) THEN
    WRITE(OutputFileBNDetails,706)
    ChrOut=RoundSigDigits(BCount)
!    WRITE(ChrOut,*) BCount
    WRITE(OutputFileBNDetails,701) ' #Orphaned Branches,'//TRIM(ChrOut)
    CALL ShowWarningError('There are orphaned Branches in input. See .bnd file for details.')

    BCount=0

    DO Count=1,NumOfBranches
      IF (BranchReported(Count)) CYCLE
      BCount=BCount+1
      CALL ShowWarningError('Orphan Branch="'//trim(Branch(Count)%Name)//'".')

!        WRITE(ChrOut,*) BCount
!        ChrOut=ADJUSTL(ChrOut)
        ChrOut=RoundSigDigits(BCount)
        IF (Branch(Count)%NumOfComponents > 0) THEN
          MatchNode=Branch(Count)%Component(1)%InletNode
          MatchNodeName=Branch(Count)%Component(1)%InletNodeName
          BranchInletNodeName=Branch(Count)%Component(1)%InletNodeName
        ELSE
          CALL ShowWarningError('Branch has no components='//TRIM(Branch(Count)%Name))
        ENDIF
        NumErr=0
        DO Loop=1,Branch(Count)%NumOfComponents
          IF (Branch(Count)%Component(Loop)%InletNode /= MatchNode) THEN
            CALL ShowSevereError('Error Detected in Branch='//TRIM(Branch(Count)%Name))
            CALL ShowContinueError('Branch Outlet does not match Inlet, Outlet='//TRIM(MatchNodeName))
            CALL ShowContinueError('Inlet Name='//TRIM(Branch(Count)%Component(Loop)%InletNodeName))
            ErrFound=.true.
            NumErr=NumErr+1
          ELSE
            MatchNode=Branch(Count)%Component(Loop)%OutletNode
            MatchNodeName=Branch(Count)%Component(Loop)%OutletNodeName
          ENDIF
        ENDDO
        BranchOutletNodeName=MatchNodeName
        IF (Branch(Count)%AssignedLoopName == Blank) THEN
          BranchLoopName = '**Unknown**'
          BranchLoopType = '**Unknown**'
        ELSE
          BranchLoopName = TRIM(Branch(Count)%AssignedLoopName)
          BranchLoopType = '**Unknown**'
        ENDIF
        WRITE(OutputFileBNDetails,701) ' Branch,'//TRIM(ChrOut)//','//TRIM(Branch(Count)%Name)//','//  &
          TRIM(BranchLoopName)//','//TRIM(BranchLoopType)//','//  &
          TRIM(BranchInletNodeName)//','//TRIM(BranchOutletNodeName)
    ENDDO
  ENDIF

  IF (ErrFound) THEN
    CALL ShowSevereError('Branch(es) did not pass integrity testing')
  ELSE
    CALL ShowMessage('All Branches passed integrity testing')
  ENDIF

  RETURN

END SUBROUTINE TestBranchIntegrity

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

END MODULE BranchInputManager

