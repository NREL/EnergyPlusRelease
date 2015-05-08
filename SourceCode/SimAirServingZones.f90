MODULE SimAirServingZones

          ! MODULE INFORMATION
          !       AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
          !       DATE WRITTEN:  Oct 1997
          !       MODIFIED:  Dec 1997 Fred Buhl; Richard Liesen  Apr 1998,
          !                  Dec 1999 Fred Buhl
          !                  22Aug2010 Craig Wray - added Fan:ComponentModel
          !       RE-ENGINEERED:  This is new code, not reengineered

          ! PURPOSE OF THIS MODULE:
          ! Contains the data and code for simulating the HVAC forced
          ! air systems.

          ! METHODOLOGY EMPLOYED:
          ! Successive iteration forward from the return air inlet
          ! to the supply air outlets.

          ! REFERENCES:
          ! None

          ! OTHER NOTES:
          ! None

          ! USE STATEMENTS
USE DataPrecisionGlobals
USE DataLoopNode
USE DataAirLoop
USE DataGlobals
USE DataHVACGlobals
USE DataSizing
USE DataEnvironment, ONLY: TotDesDays, CurEnvirNum, EnvironmentName, CurMnDy, TotRunDesPersDays
USE DataZoneEquipment
USE DataAirSystems
USE DataInterfaces

IMPLICIT NONE

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
! coil operation
INTEGER, PARAMETER :: CoilOn =  1              ! normal coil operation
INTEGER, PARAMETER :: CoilOff = 0              ! signal coil shouldn't run
INTEGER, PARAMETER :: BeforeBranchSim = 1
INTEGER, PARAMETER :: AfterBranchSim =  2
! CompType numerics -- for this module
! component types addressed by this module
INTEGER, PARAMETER :: OAMixer_Num      = 1
INTEGER, PARAMETER :: Fan_Simple_CV    = 2
INTEGER, PARAMETER :: Fan_Simple_VAV   = 3
INTEGER, PARAMETER :: WaterCoil_SimpleCool = 4
INTEGER, PARAMETER :: WaterCoil_Cooling = 5
INTEGER, PARAMETER :: WaterCoil_SimpleHeat = 6
INTEGER, PARAMETER :: SteamCoil_AirHeat = 7
INTEGER, PARAMETER :: WaterCoil_DetailedCool = 8
INTEGER, PARAMETER :: Coil_ElectricHeat = 9
INTEGER, PARAMETER :: Coil_GasHeat = 10
INTEGER, PARAMETER :: WaterCoil_CoolingHXAsst = 11
INTEGER, PARAMETER :: DXCoil_CoolingHXAsst = 12
INTEGER, PARAMETER :: Coil_DeSuperHeat = 13
INTEGER, PARAMETER :: DXSystem = 14
INTEGER, PARAMETER :: HeatXchngr = 15
INTEGER, PARAMETER :: Desiccant = 16
INTEGER, PARAMETER :: Unglazed_SolarCollector = 17
INTEGER, PARAMETER :: EvapCooler = 18
INTEGER, PARAMETER :: UnitarySystem = 19
INTEGER, PARAMETER :: Furnace_UnitarySys = 20
INTEGER, PARAMETER :: Humidifier = 21
INTEGER, PARAMETER :: Duct = 22
INTEGER, PARAMETER :: UnitarySystem_BypassVAVSys = 23
INTEGER, PARAMETER :: UnitarySystem_MSHeatPump = 24
INTEGER, PARAMETER :: Fan_ComponentModel = 25 !cpw22Aug2010 (new)
INTEGER, PARAMETER :: DXHeatPumpSystem = 26
INTEGER, PARAMETER :: CoilUserDefined  = 27

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
LOGICAL   :: GetAirLoopInputFlag = .True.  ! Flag set to make sure you get input once
INTEGER   :: NumOfTimeStepInDay ! number of zone time steps in a day

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
PUBLIC  ManageAirLoops

          ! Get Input routines for module
PRIVATE GetAirPathData

          ! Initialization routines for module
PRIVATE InitAirLoops

          ! Simulation subroutines for the module
PRIVATE SimAirLoops
PRIVATE SimAirLoop
PRIVATE SolveAirLoopControllers
PRIVATE ReSolveAirLoopControllers
PRIVATE SimAirLoopComponents
PRIVATE SimAirLoopComponent
PRIVATE UpdateBranchConnections
PRIVATE ResolveSysFlow
PRIVATE SizeAirLoops
PRIVATE SetUpSysSizingArrays
PUBLIC  UpdateSysSizing
PRIVATE SizeAirLoopBranches

          ! Reporting routines for module


CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE ManageAirLoops(FirstHVACIteration,SimAir,SimZoneEquipment)

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
          !       DATE WRITTEN:  Oct 1997
          !           MODIFIED:  Dec 1997 Fred Buhl
          !      RE-ENGINEERED:  This is new code, not reengineered

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the manager subroutine for the air loop simulation.
          ! Called from SimSelectedEquipment, which is called from SimHVAC,
          ! which is called from ManageHVAC, the top level system/plant driver.
          ! The subroutine performs the usual manager functions: it calls the
          ! Get, Init, Sim, Update, and Report routines.

          ! METHODOLOGY EMPLOYED:
          ! not applicable:

          ! REFERENCES: None

          ! USE STATEMENTS:
  USE MixedAir,            ONLY : ManageOutsideAirSystem

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)    :: FirstHVACIteration  ! TRUE if first full HVAC iteration in an HVAC timestep
  LOGICAL, INTENT(INOUT) :: SimAir              ! TRUE means air loops must be (re)simulated
  LOGICAL, INTENT(INOUT) :: SimZoneEquipment    ! TRUE means zone equipment must be (re) simulated

          ! SUBROUTINE PARAMETER DEFINITIONS: none

          ! INTERFACE BLOCK SPECIFICATIONS: none
          ! na

          ! DERIVED TYPE DEFINITIONS: none

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS: none

          ! FLOW:

  IF (GetAirLoopInputFlag) THEN  !First time subroutine has been entered
      CALL GetAirPathData ! Get air loop descriptions from input file
      GetAirLoopInputFlag=.false.
  END IF

    ! Initialize air loop related parameters
  CALL InitAirLoops(FirstHVACIteration)

    ! Call the AirLoop Simulation
  IF (SysSizingCalc) THEN
    CALL SizeAirLoops
  ELSE
    CALL SimAirLoops(FirstHVACIteration,SimZoneEquipment)
  END IF

  ! This flag could be used to resimulate only the air loops that needed additional iterations.
  ! This flag would have to be moved inside SimAirLoops to gain this flexibility.
  SimAir = ANY(AirLoopControlInfo%ResimAirLoopFlag)

  RETURN

END SUBROUTINE ManageAirLoops

! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetAirPathData

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Fred Buhl
          !       DATE WRITTEN:  Jan 1998
          !           MODIFIED: Richard Liesen April 1998, Fred Buhl Dec 1999
          !      RE-ENGINEERED:  This is new code, not reengineered

          ! PURPOSE OF THIS SUBROUTINE:
          ! Input all the data needed to simulate the air loops in the problem.

          ! METHODOLOGY EMPLOYED:
          ! Use the various "Get" routines from the InputProcessor module to
          ! obtain input data and store it in the data structures defined in MODULE SimAirServingZones

          ! REFERENCES: This gets the following object:
          ! AirLoopHVAC,
          !        \min-fields 10
          !        \memo Defines a central forced air system
          !    A1, \field Name
          !        \required-field
          !        \type alpha
          !        \reference AirPrimaryLoops
          !    A2, \field Controller List Name
          !        \note Enter the name of an AirLoopHVAC:ControllerList object.
          !        \type object-list
          !        \object-list ControllerLists
          !    A3, \field Availability Manager List Name
          !        \note Enter the name of an AvailabilityManagerAssignmentList object.
          !        \type object-list
          !        \object-list SystemAvailabilityManagerLists
          !    N1, \field Design Primary Air Flow Rate
          !        \default 0
          !        \units m3/s
          !        \autosizable
          !    A4, \field BranchList Name
          !        \note Name of a BranchList containing all the branches in this air loop
          !        \required-field
          !        \type object-list
          !        \object-list BranchLists
          !    A5, \field ConnectorList Name
          !        \note Name of a Connector List containing all the splitters and mixers in the loop
          !        \type object-list
          !        \object-list ConnectorLists
          !    A6, \field Supply Side Inlet Node Name
          !        \note Name of inlet node where return air enters the supply side of the air loop
          !        \required-field
          !    A7, \field Demand Side Outlet Node Name
          !        \note Name of outlet node where return air leaves the demand side and enters the supply side.
          !        \required-field
          !    A8, \field Demand Side Inlet Node Names
          !        \note Name of a Node or NodeList containing the inlet node(s) supplying air to zone equipment.
          !        \required-field
          !    A9; \field Supply Side Outlet Node Names
          !        \note Name of a Node or NodeList containing the outlet node(s) supplying air to the demand side.
          !        \required-field

          ! USE STATEMENTS:

USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, GetObjectItemNum, FindItemInList, GetObjectDefMaxArgs, &
                          SameString, MakeUPPERCase
USE NodeInputManager, ONLY: GetNodeNums,GetOnlySingleNode
USE BranchInputManager, ONLY: GetBranchList, GetBranchData, GetLoopSplitter, NumBranchesInBranchList,   &
                              NumCompsInBranch, GetLoopMixer, GetNumSplitterMixerInConntrList
USE SystemAvailabilityManager, ONLY: GetAirLoopAvailabilityManager
USE MixedAir, ONLY: GetOASystemNumber, FindOAMixerMatchForOASystem, GetOAMixerInletNodeNumber, GetOASysControllerListIndex, &
                    GetOASysNumSimpControllers, GetOASysNumCoolingCoils, GetOASysNumHeatingCoils, &
                    GetOACompListNumber, GetOACompName, GetOACompType, GetOACompTypeNum, GetNumOASystems
USE HVACControllers, ONLY: CheckCoilWaterInletNode, GetControllerActuatorNodeNum
USE WaterCoils, ONLY: GetCoilWaterInletNode
USE General, ONLY: RoundSigDigits
USE DataConvergParams, ONLY: AirLoopConvergence
!USE DataMixedAir, ONLY: OAMixer, OutsideAirSys, NumOAMixers, NumOASys

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS: none

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetAirPathData: '

          ! INTERFACE BLOCK DEFINITIONS: None

          ! DERIVED TYPE DEFINITIONS:
TYPE AirUniqueNodes
  CHARACTER(len=MaxNameLength) :: NodeName = ' '
  CHARACTER(len=MaxNameLength) :: AirLoopName = ' '
  CHARACTER(len=MaxNameLength) :: FieldName = ' '
  LOGICAL :: NodeNameUsed=.FALSE.
END TYPE

          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS
INTEGER                                             :: NumNumbers   ! number of numbers returned by GetObjectItem
REAL(r64), DIMENSION(:), ALLOCATABLE                :: Numbers      ! numbers (REAL(r64)s) returned by GetObjectItem
CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: cNumericFields ! Numeric field names
LOGICAL, DIMENSION(:), ALLOCATABLE                  :: lNumericBlanks ! Logical array, numeric field input BLANK = .true.
INTEGER                                             :: NumAlphas    ! number of strings returned by GetObjectItem
INTEGER                                             :: NumParams
INTEGER                                             :: MaxNumbers
INTEGER                                             :: MaxAlphas
CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: Alphas       ! alpha strings returned by GetObjectItem
CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: cAlphaFields ! Alpha field names
LOGICAL, DIMENSION(:), ALLOCATABLE                  :: lAlphaBlanks ! Logical array, alpha field input BLANK = .true.
CHARACTER(len=MaxNameLength)                        :: CurrentModuleObject ! Object type for getting and error messages
INTEGER                                             :: NumNodes     ! number of nodes returned by GetNodeNums
INTEGER, ALLOCATABLE, DIMENSION(:)                  :: NodeNums     ! node numbers returned by GetNodeNums
INTEGER                                             :: NodeNum      ! a node number
INTEGER                                             :: AirSysNum    ! an air system (air loop) number
INTEGER                                             :: OANum        ! outside aur system index
INTEGER                                             :: OASysNum
INTEGER                                             :: NumInList
INTEGER                                             :: OACompNum
INTEGER                                             :: OAMixNum     ! outside air mixer index
INTEGER                                             :: IOStat       ! status number returned by GetObjectItem
INTEGER                                             :: NumControllers ! number of controllers
INTEGER                                             :: ControllerListNum ! Controller List index
INTEGER                                             :: ControllerNum  ! Controller index
INTEGER                                             :: I ! do loop index
INTEGER                                             :: BranchNum ! branch index
INTEGER                                             :: CompNum ! component index
INTEGER                                             :: NumCompsOnBranch  ! Number of components on a branch
INTEGER                                             :: OutBranchNum ! outlet branch index
INTEGER                                             :: InBranchNum ! inlet branch index
CHARACTER(len=MaxNameLength)                        :: ControllerName ! controller name
CHARACTER(len=MaxNameLength)                        :: ControllerType ! controller type
CHARACTER(len=MaxNameLength)                        :: BranchListName ! Name of a Branch List object
CHARACTER(len=MaxNameLength)                        :: ControllerListName ! Name of a controller list object
CHARACTER(len=MaxNameLength)                        :: AvailManagerListName ! Name of an availability manager list object
CHARACTER(len=MaxNameLength)                        :: ConnectorListName! Name of a connector list object
CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: BranchNames     ! Branch names from GetBranchList call
CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: CompTypes       ! Component types from GetBranchList call
CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: CompNames       ! Component names from GetBranchList call
CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: InletNodeNames  ! Component inlet node names from GetBranchData call
CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: OutletNodeNames ! Component outlet node names from GetBranchData call
CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: NodeNames       ! Outlet node names from GetLoopSplitter call
INTEGER,ALLOCATABLE,SAVE,DIMENSION(:) :: NodeNumbers                         ! Outlet node numbers from GetLoopSplitter call
INTEGER,ALLOCATABLE,SAVE,DIMENSION(:) :: InletNodeNumbers  ! Component inlet node numbers from GetBranchData call
INTEGER,ALLOCATABLE,SAVE,DIMENSION(:) :: OutletNodeNumbers ! Component outlet node numbers from GetBranchData call
INTEGER,DIMENSION(2)                  :: DummyInteger      ! Placeholder for corresponding plant loop branch pressure drop info
LOGICAL :: ErrorsFound=.FALSE.         ! TRUE if errors detected in input
LOGICAL       :: IsNotOK               ! Flag to verify name
LOGICAL       :: IsBlank               ! Flag for blank name
LOGICAL,ALLOCATABLE,SAVE,DIMENSION(:) :: PackagedUnit
integer test
integer count
LOGICAL ErrInList
INTEGER :: ConListNum = 0                 ! index of a Connector List object in the input
LOGICAL :: SplitterExists = .FALSE.       ! TRUE if there is a slitter in a primary air system
LOGICAL :: MixerExists =    .FALSE.       ! TRUE if there is a mixer in a primary air system
LOGICAL :: errflag
TYPE (AirUniqueNodes), ALLOCATABLE, DIMENSION(:) :: TestUniqueNodes
INTEGER :: TestUniqueNodesNum=0
INTEGER :: NumOASysSimpControllers            ! number of simple controllers in the OA Sys of an air primary system
INTEGER :: NumOASysControllers                ! total number of controllers in the OA Sys
INTEGER :: OASysContListNum               ! index of the controller list of the OA Sys
INTEGER :: OASysControllerNum             ! index of OA Sys simple controller in the air primary system controller lists
LOGICAL :: NodeNotFound                   ! true if matching actuator node not found
INTEGER :: CompType_Num                   ! numeric equivalent for component type
CHARACTER(len=MaxNameLength)  :: CompType ! component type
INTEGER :: WaterCoilNodeNum               ! numeric equivalent for water coil node number
INTEGER :: ActuatorNodeNum                ! numeric equivalent for controller actuator node number
CHARACTER(len=MaxNameLength), DIMENSION(3) :: MatchNodeName


CALL GetObjectDefMaxArgs('AirLoopHVAC',NumParams,MaxAlphas,MaxNumbers)
CALL GetObjectDefMaxArgs('ConnectorList',NumParams,NumAlphas,NumNumbers)
MaxAlphas=MAX(MaxAlphas,NumAlphas)
MaxNumbers=MAX(MaxNumbers,NumNumbers)
CALL GetObjectDefMaxArgs('AirLoopHVAC:ControllerList',NumParams,NumAlphas,NumNumbers)
MaxAlphas=MAX(MaxAlphas,NumAlphas)
MaxNumbers=MAX(MaxNumbers,NumNumbers)

ALLOCATE(Numbers(MaxNumbers))
ALLOCATE(cNumericFields(MaxNumbers))
ALLOCATE(lNumericBlanks(MaxNumbers))
ALLOCATE(Alphas(MaxAlphas))
ALLOCATE(cAlphaFields(MaxAlphas))
ALLOCATE(lAlphaBlanks(MaxAlphas))

! Initialize some local arrays
Numbers = 0.0d0
cNumericFields = ' '
lNumericBlanks = .true.
Alphas = ' '
cAlphaFields = ' '
lAlphaBlanks = .true.

NumOfTimeStepInDay = NumOfTimeStepInHour * 24

CALL GetObjectDefMaxArgs('NodeList',NumParams,NumAlphas,NumNumbers)
ALLOCATE(NodeNums(NumParams))
NodeNums=0

! Find number of primary air systems
NumPrimaryAirSys = GetNumObjectsFound('AirLoopHVAC')
ALLOCATE(TestUniqueNodes(NumPrimaryAirSys*4))  ! used to look at specific nodes that must be unique, fields A6-A9

ALLOCATE(PrimaryAirSystem(NumPrimaryAirSys)) ! alloacate the primary air sys data array
ALLOCATE(AirToZoneNodeInfo(NumPrimaryAirSys)) ! allocate the array that stores the air sys / zone equp connection data
ALLOCATE(AirToOANodeInfo(NumPrimaryAirSys)) ! allocate the array that stores the OA node connections (reporting)
ALLOCATE(PackagedUnit(NumPrimaryAirSys))
ALLOCATE(AirLoopControlInfo(NumPrimaryAirSys))
ALLOCATE(AirLoopFlow(NumPrimaryAirSys))
ALLOCATE(AirLoopConvergence(NumPrimaryAirSys))
ALLOCATE(UnitarySysEqSizing(NumPrimaryAirSys))

IF (NumPrimaryAirSys <= 0) THEN
  DEALLOCATE(TestUniqueNodes)
  DEALLOCATE(NodeNums)
  RETURN
END IF

! Loop through the primary air systems and obtain the data for each system
DO AirSysNum=1,NumPrimaryAirSys
    NumOASysControllers = 0
    NumOASysSimpControllers = 0
    OASysContListNum = 0
    PackagedUnit(AirSysNum) = .FALSE.
    PrimaryAirSystem(AirSysNum)%OASysExists = .FALSE. ! init Outside Air system connection data to none
    PrimaryAirSystem(AirSysNum)%OASysInletNodeNum = 0
    PrimaryAirSystem(AirSysNum)%OASysOutletNodeNum = 0
    PrimaryAirSystem(AirSysNum)%NumOAHeatCoils = 0
    PrimaryAirSystem(AirSysNum)%NumOACoolCoils = 0

    CurrentModuleObject = 'AirLoopHVAC'

    CALL GetObjectItem(CurrentModuleObject,AirSysNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStat, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &    ! get all the input data for the air system
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    ! Assign the air system data to the simulation variables.
    ! Data needed to simulate the system goes into PrimaryAirSystem.
    ! Data connecting the air system to the zone equioment goes into AirToZoneNodeInfo (in DataLoopNode).
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),PrimaryAirSystem%Name,AirSysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    PrimaryAirSystem(AirSysNum)%Name         = Alphas(1)
    AirToZoneNodeInfo(AirSysNum)%AirLoopName = Alphas(1)
    IF (NumAlphas < 9) THEN
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", insufficient information.')
      CALL ShowContinueError('...Have supplied less than 9 alpha fields.')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    IF (NumNumbers < 1) THEN
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", insufficient information.')
      CALL ShowContinueError('...Have supplied less than 1 numeric field.')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    PrimaryAirSystem(AirSysNum)%DesignVolFlowRate = Numbers(1)
    !Only allow one return air node
    AirToZoneNodeInfo(AirSysNum)%NumReturnNodes  = 1
    ! Allocate the return air node arrays
    Allocate(AirToZoneNodeInfo(AirSysNum)%ZoneEquipReturnNodeNum(AirToZoneNodeInfo(AirSysNum)%NumReturnNodes))
    Allocate(AirToZoneNodeInfo(AirSysNum)%AirLoopReturnNodeNum(AirToZoneNodeInfo(AirSysNum)%NumReturnNodes))
    ! fill the return air node arrays with node numbers
    AirToZoneNodeInfo(AirSysNum)%AirLoopReturnNodeNum(1)= &
               GetOnlySingleNode(Alphas(6),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
    AirToZoneNodeInfo(AirSysNum)%ZoneEquipReturnNodeNum(1)= &
               GetOnlySingleNode(Alphas(7),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
    ! work on unique nodes
    test=finditeminlist(Alphas(6),TestUniqueNodes%NodeName,TestUniqueNodesNum)
    if (test == 0) THEN
      TestUniqueNodesNum=TestUniqueNodesNum+1
      TestUniqueNodes(TestUniqueNodesNum)%NodeName     = Alphas(6)
      TestUniqueNodes(TestUniqueNodesNum)%AirLoopName  = Alphas(1)
      TestUniqueNodes(TestUniqueNodesNum)%FieldName    = TRIM(cAlphaFields(6))
      TestUniqueNodes(TestUniqueNodesNum)%NodeNameUsed = .true.
    else
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", duplicate node name.')
      CALL ShowContinueError('...used for '//TRIM(cAlphaFields(6))//'="'//TRIM(Alphas(6))//'"')
      CALL ShowContinueError('...first used in '//TRIM(CurrentModuleObject)//'="'//TRIM(TestUniqueNodes(test)%AirLoopName)// &
                             '" for '// TRIM(TestUniqueNodes(test)%FieldName))
      ErrorsFound=.true.
    endif
    test=finditeminlist(Alphas(7),TestUniqueNodes%NodeName,TestUniqueNodesNum)
    if (test == 0) THEN
      TestUniqueNodesNum=TestUniqueNodesNum+1
      TestUniqueNodes(TestUniqueNodesNum)%NodeName     = Alphas(7)
      TestUniqueNodes(TestUniqueNodesNum)%AirLoopName  = Alphas(1)
      TestUniqueNodes(TestUniqueNodesNum)%FieldName    = TRIM(cAlphaFields(7))
      TestUniqueNodes(TestUniqueNodesNum)%NodeNameUsed = .true.
    else
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", duplicate node name.')
      CALL ShowContinueError('...used for '//TRIM(cAlphaFields(7))//'="'//TRIM(Alphas(7))//'"')
      CALL ShowContinueError('...first used in '//TRIM(CurrentModuleObject)//'="'//TRIM(TestUniqueNodes(test)%AirLoopName)// &
                             '" for '//TRIM(TestUniqueNodes(test)%FieldName))
      ErrorsFound=.true.
    endif
    test=finditeminlist(Alphas(8),TestUniqueNodes%NodeName,TestUniqueNodesNum)
    if (test == 0) THEN
      TestUniqueNodesNum=TestUniqueNodesNum+1
      TestUniqueNodes(TestUniqueNodesNum)%NodeName     = Alphas(8)
      TestUniqueNodes(TestUniqueNodesNum)%AirLoopName  = Alphas(1)
      TestUniqueNodes(TestUniqueNodesNum)%FieldName    = TRIM(cAlphaFields(8))
      TestUniqueNodes(TestUniqueNodesNum)%NodeNameUsed =.true.
    else
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", duplicate node name/list.')
      CALL ShowContinueError('...used for '//TRIM(cAlphaFields(8))//'="'//TRIM(Alphas(8))//'"')
      CALL ShowContinueError('...first used in '//TRIM(CurrentModuleObject)//'="'//TRIM(TestUniqueNodes(test)%AirLoopName)//  &
                             '" for '//TRIM(TestUniqueNodes(test)%FieldName))
      ErrorsFound=.true.
    endif
    test=finditeminlist(Alphas(9),TestUniqueNodes%NodeName,TestUniqueNodesNum)
    if (test == 0) THEN
      TestUniqueNodesNum=TestUniqueNodesNum+1
      TestUniqueNodes(TestUniqueNodesNum)%NodeName     = Alphas(9)
      TestUniqueNodes(TestUniqueNodesNum)%AirLoopName  = Alphas(1)
      TestUniqueNodes(TestUniqueNodesNum)%FieldName    = TRIM(cAlphaFields(9))
      TestUniqueNodes(TestUniqueNodesNum)%NodeNameUsed = .true.
    else
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", duplicate node name/list.')
      CALL ShowContinueError('...used for '//TRIM(cAlphaFields(9))//'="'//TRIM(Alphas(9))//'"')
      CALL ShowContinueError('...first used in '//TRIM(CurrentModuleObject)//'="'//TRIM(TestUniqueNodes(test)%AirLoopName)// &
                             '" for '// TRIM(TestUniqueNodes(test)%FieldName))
      ErrorsFound=.true.
    endif
! this test depends on the controlled zone input having been "gotten"
    test=0
    do count=1,NumReturnAirPaths
      if (returnairpath(count)%OutletNodeNum == AirToZoneNodeInfo(AirSysNum)%ZoneEquipReturnNodeNum(1)) THEN
        test=returnairpath(count)%OutletNodeNum
        exit
      endif
    enddo
    IF (test == 0) THEN  ! there, see if it's in the controlled zone info
      do count=1,NumOfZones
        IF (ZoneEquipConfig(count)%ReturnAirNode /= AirToZoneNodeInfo(AirSysNum)%ZoneEquipReturnNodeNum(1)) CYCLE
        test=count
        exit
      enddo
    ENDIF
    IF (test == 0) THEN
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", invalid.')
      CALL ShowContinueError(TRIM(cAlphaFields(7))//' (Return Air Path or ZoneHVAC:EquipmentConnections) not valid = "'//  &
                           TRIM(Alphas(7))//'".')
      ErrorsFound=.true.
    ENDIF
    ! Get the supply nodes
    ErrInList=.false.
    CALL GetNodeNums(Alphas(8),NumNodes,NodeNums,ErrInList,NodeType_Air,TRIM(CurrentModuleObject), &
                     PrimaryAirSystem(AirSysNum)%Name,NodeConnectionType_Inlet,1,ObjectIsParent,  &
                     InputFieldName=cAlphaFields(8))
    IF (ErrInList) THEN
!      CALL ShowContinueError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
!                         '", invalid '//trim(cAlphaFields(8))//'.')
      ErrorsFound=.true.
    ENDIF
    ! Allow at most 3 supply nodes (for a 3 deck system)
    IF (NumNodes > 3) THEN
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
         '", too many nodes.')
      CALL ShowContinueError('Only 1st 3 Nodes will be used from '//TRIM(cAlphaFields(8))//'="'//TRIM(Alphas(8))//'".')
      ErrorsFound=.true.
    ENDIF
    IF (NumNodes.EQ.0) THEN
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
         '", too few nodes.')
      CALL ShowContinueError('There must be at least 1 supply node in the system.')
      ErrorsFound=.true.
    END IF
    AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes = NumNodes
    ! Allocate the supply node arrays in AirToZoneNodeInfo
    Allocate(AirToZoneNodeInfo(AirSysNum)%ZoneEquipSupplyNodeNum(AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes))
    Allocate(AirToZoneNodeInfo(AirSysNum)%AirLoopSupplyNodeNum(AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes))
    Allocate(AirToZoneNodeInfo(AirSysNum)%SupplyDuctType(AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes))
    ! Fill the supply node arrays with node numbers
    DO I = 1, AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes
      AirToZoneNodeInfo(AirSysNum)%ZoneEquipSupplyNodeNum(I) = NodeNums(I)
      AirToZoneNodeInfo(AirSysNum)%SupplyDuctType(I) = 0
    END DO
    ErrInList=.false.
    CALL GetNodeNums(Alphas(9),NumNodes,NodeNums,ErrInList,NodeType_Air,TRIM(CurrentModuleObject), &
                     PrimaryAirSystem(AirSysNum)%Name,NodeConnectionType_Outlet,1,ObjectIsParent,  &
                     InputFieldName=cAlphaFields(9))
    IF (ErrInList) THEN
!      CALL ShowContinueError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
!                         '", invalid '//trim(cAlphaFields(9))//'.')
      ErrorsFound=.true.
    ENDIF
    IF (NumNodes.NE.AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes) THEN
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(Alphas(1))//'", node mismatch.')
      CALL ShowContinueError('...number of air system exit nodes ['//trim(RoundSigDigits(NumNodes))//  &
              '] must match number of zone equip inlet nodes ['//  &
              trim(RoundSigDigits(AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes))//'].')
      ErrorsFound=.true.
    END IF
    DO I = 1, AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes
      AirToZoneNodeInfo(AirSysNum)%AirLoopSupplyNodeNum(I) = NodeNums(I)
    END DO
    AirToZoneNodeInfo(AirSysNum)%NumZonesCooled = 0
    AirToZoneNodeInfo(AirSysNum)%NumZonesHeated = 0
    !Branch, Controller, Availability Manager and Connector List Names to access later
    ControllerListName   = Alphas(2)
    BranchListName       = Alphas(4)
    AvailManagerListName = Alphas(3)
    ConnectorListName    = Alphas(5)
    PrimaryAirSystem(AirSysNum)%NumBranches=NumBranchesInBranchList(BranchListName)
    IF (PrimaryAirSystem(AirSysNum)%NumBranches.EQ.0) THEN
      CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
         '", insufficient information.')
      CALL ShowContinueError('...there must be at least 1 branch specified.')
      ErrorsFound=.true.
    END IF
    ALLOCATE(BranchNames(PrimaryAirSystem(AirSysNum)%NumBranches))
    BranchNames=' '
    ! get the branch lists
    CALL GetBranchList(PrimaryAirSystem(AirSysNum)%Name,BranchListName,  &
                       PrimaryAirSystem(AirSysNum)%NumBranches,BranchNames,'Air')
    ALLOCATE(PrimaryAirSystem(AirSysNum)%Branch(PrimaryAirSystem(AirSysNum)%NumBranches))
    ! Cycle through all of the branches and set up the branch data
    DO BranchNum = 1,PrimaryAirSystem(AirSysNum)%NumBranches
      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Name = BranchNames(BranchNum)
      NumCompsOnBranch=NumCompsInBranch(BranchNames(BranchNum))
      IF (NumCompsOnBranch <= 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
           '", insufficient information.')
        CALL ShowContinueError('...Branch="'//trim(BranchNames(BranchNum))//'", no components on branch.')
        ErrorsFound=.true.
        CYCLE
      ENDIF
      ALLOCATE(CompTypes(NumCompsOnBranch))
      CompTypes=' '
      ALLOCATE(CompNames(NumCompsOnBranch))
      CompNames=' '
      ALLOCATE(InletNodeNames(NumCompsOnBranch))
      InletNodeNames=' '
      ALLOCATE(InletNodeNumbers(NumCompsOnBranch))
      InletNodeNumbers=0
      ALLOCATE(OutletNodeNames(NumCompsOnBranch))
      OutletNodeNames=' '
      ALLOCATE(OutletNodeNumbers(NumCompsOnBranch))
      OutletNodeNumbers=0

      CALL GetBranchData(PrimaryAirSystem(AirSysNum)%Name,BranchNames(BranchNum),       &
                         PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%MaxVolFlowRate,  &
                         DummyInteger(1), DummyInteger(2), & !Placeholders for plant branch pressure data (not used in air loops)
                         NumCompsOnBranch, &
                         CompTypes,CompNames,      &
                         InletNodeNames,InletNodeNumbers,     &
                         OutletNodeNames,OutletNodeNumbers,ErrorsFound)
      ALLOCATE (PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(NumCompsOnBranch))
      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%TotalComponents = NumCompsOnBranch

      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%TotalNodes = NumCompsOnBranch+1
      ALLOCATE (PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNum(NumCompsOnBranch+1))
      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNum(1) = InletNodeNumbers(1)
      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%DuctType   = Main
      DO CompNum = 1, PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%TotalComponents

        PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf      = CompTypes(CompNum)
        PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%Name        = CompNames(CompNum)
        PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompIndex   = 0
        PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameIn  = InletNodeNames(CompNum)
        PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn   = InletNodeNumbers(CompNum)
        PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameOut = OutletNodeNames(CompNum)
        PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut  = OutletNodeNumbers(CompNum)
        PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNum(CompNum+1) = OutletNodeNumbers(CompNum)

        ! Check for Outside Air system; if there, store its connection node numbers to primary air system
        IF (SameString(CompTypes(CompNum),'AirLoopHVAC:OutdoorAirSystem')) THEN
          IF (PrimaryAirSystem(AirSysNum)%OASysExists) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
               '", too many outdoor air systems.')
            CALL ShowContinueError('Only one AirLoopHVAC:OutdoorAirSystem allowed.')
            ErrorsFound=.true.
            CYCLE
          END IF
          PrimaryAirSystem(AirSysNum)%OASysExists = .TRUE.
          PrimaryAirSystem(AirSysNum)%OASysInletNodeNum = InletNodeNumbers(CompNum)
          PrimaryAirSystem(AirSysNum)%OASysOutletNodeNum = OutletNodeNumbers(CompNum)
          AirToOANodeInfo(AirSysNum)%OASysExists = .TRUE.
          AirToOANodeInfo(AirSysNum)%OASysInletNodeNum = InletNodeNumbers(CompNum)
          AirToOANodeInfo(AirSysNum)%OASysOutletNodeNum = OutletNodeNumbers(CompNum)
          OANum=GetOASystemNumber(CompNames(CompNum))
          IF (OANum > 0) THEN
            NumOASysSimpControllers = GetOASysNumSimpControllers(OANum)
            PrimaryAirSystem(AirSysNum)%NumOAHeatCoils = GetOASysNumHeatingCoils(OANum)
            PrimaryAirSystem(AirSysNum)%NumOACoolCoils = GetOASysNumCoolingCoils(OANum)
            OASysContListNum = GetOASysControllerListIndex(OANum)
            OAMixNum=FindOAMixerMatchForOASystem(OANum)
            IF (OAMixNum > 0) THEN
              PrimaryAirSystem(AirSysNum)%OAMixOAInNodeNum = GetOAMixerInletNodeNumber(OAMixNum)
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
                 '", item not found.')
              CALL ShowContinueError('OutdoorAir:Mixer for AirLoopHVAC:OutdoorAirSystem="'//TRIM(CompNames(CompNum))//  &
                 '" not found.')
              ErrorsFound = .true.
            END IF
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
                 '", item not found.')
            CALL ShowContinueError('AirLoopHVAC:OutdoorAirSystem="'//TRIM(CompNames(CompNum))//'" not found.')
            CALL ShowContinueError('  referenced in Branch="'//TRIM(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Name)//'".')
            ErrorsFound = .true.
          END IF
        END IF
        SELECT CASE(MakeUPPERCase(CompTypes(CompNum)))
          CASE('COILSYSTEM:COOLING:DX')
            PackagedUnit(AirSysNum) = .TRUE.
          CASE('COILSYSTEM:HEATING:DX')
            PackagedUnit(AirSysNum) = .TRUE.
          CASE('AIRLOOPHVAC:UNITARYSYSTEM')
            PackagedUnit(AirSysNum) = .TRUE.
          CASE('AIRLOOPHVAC:UNITARY:FURNACE:HEATONLY')
            PackagedUnit(AirSysNum) = .TRUE.
          CASE('AIRLOOPHVAC:UNITARY:FURNACE:HEATCOOL')
            PackagedUnit(AirSysNum) = .TRUE.
          CASE('AIRLOOPHVAC:UNITARYHEATONLY')
            PackagedUnit(AirSysNum) = .TRUE.
          CASE('AIRLOOPHVAC:UNITARYHEATCOOL')
            PackagedUnit(AirSysNum) = .TRUE.
          CASE('AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR')
           PackagedUnit(AirSysNum) = .TRUE.
          CASE('AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR')
           PackagedUnit(AirSysNum) = .TRUE.
          CASE('AIRLOOPHVAC:UNITARYHEATCOOL:VAVCHANGEOVERBYPASS')
           PackagedUnit(AirSysNum) = .TRUE.
          CASE('AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED')
           PackagedUnit(AirSysNum) = .TRUE.
        END SELECT

      END DO ! end of component loop

      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%ControlType = ' '
      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%MinVolFlowRate = 0.0d0
      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNumIn = InletNodeNumbers(1)
      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNumOut = OutletNodeNumbers(NumCompsOnBranch)
      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%MaxMassFlowRate = 0.0d0
      PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%MinMassFlowRate = 0.0d0

      DEALLOCATE(CompTypes)
      DEALLOCATE(CompNames)
      DEALLOCATE(InletNodeNames)
      DEALLOCATE(InletNodeNumbers)
      DEALLOCATE(OutletNodeNames)
      DEALLOCATE(OutletNodeNumbers)

    END DO ! end of branch loop

    DEALLOCATE(BranchNames)

    ! find and store the primary air system outlet branch reference numbers
    PrimaryAirSystem(AirSysNum)%NumOutletBranches = AirToZoneNodeInfo(AirSysNum)%NumSupplyNodes
    DO OutBranchNum=1,3
      PrimaryAirSystem(AirSysNum)%OutletBranchNum(OutBranchNum) = 0
      IF (OutBranchNum.GT.PrimaryAirSystem(AirSysNum)%NumOutletBranches) EXIT
      MatchNodeName(OutBranchNum)=NodeID(AirToZoneNodeInfo(AirSysNum)%AirLoopSupplyNodeNum(OutBranchNum))
      DO BranchNum = 1,PrimaryAirSystem(AirSysNum)%NumBranches
        IF (AirToZoneNodeInfo(AirSysNum)%AirLoopSupplyNodeNum(OutBranchNum) .EQ. &
            PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNumOut) THEN
          PrimaryAirSystem(AirSysNum)%OutletBranchNum(OutBranchNum) = BranchNum
        END IF
      END DO
    END DO
    !  Check for errors
    DO OutBranchNum=1,PrimaryAirSystem(AirSysNum)%NumOutletBranches
      IF (PrimaryAirSystem(AirSysNum)%OutletBranchNum(OutBranchNum) /= 0) CYCLE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
                 '", branch in error.')
      CALL ShowContinueError('Probable missing or misspelled node referenced in the branch(es):')
      DO BranchNum=1,PrimaryAirSystem(AirSysNum)%NumBranches
        CALL ShowContinueError('Possible Error in Branch Object="'//TRIM(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Name)//'".')
      ENDDO
      CALL ShowContinueError('...looking to match to Node="'//trim(MatchNodeName(OutBranchNum))//'".')
      ErrorsFound=.true.
    END DO

    ! find and store the primary air system inlet branch numbers
    PrimaryAirSystem(AirSysNum)%NumInletBranches = AirToZoneNodeInfo(AirSysNum)%NumReturnNodes
    DO InBranchNum=1,PrimaryAirSystem(AirSysNum)%NumInletBranches
      PrimaryAirSystem(AirSysNum)%InletBranchNum(InBranchNum) = 0
      DO BranchNum = 1,PrimaryAirSystem(AirSysNum)%NumBranches
        IF (AirToZoneNodeInfo(AirSysNum)%AirLoopReturnNodeNum(InBranchNum) .EQ. &
            PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNumIn) THEN
          PrimaryAirSystem(AirSysNum)%InletBranchNum(InBranchNum) = BranchNum
        END IF
      END DO
      IF (PrimaryAirSystem(AirSysNum)%InletBranchNum(InBranchNum) == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
                   '", connection to zone.')
        CALL ShowContinueError('No Connection found for Return Air from Zone')
        CALL ShowContinueError('Expected node name ="'//  &
                               TRIM(NodeID(AirToZoneNodeInfo(AirSysNum)%AirLoopReturnNodeNum(InBranchNum)))//'".')
        ErrorsFound=.true.
      ENDIF
    END DO

    ! Check to see if a spliter and/or mixer exist
    SplitterExists = .FALSE.
    MixerExists = .FALSE.

    IF (ConnectorListName.NE.' ') THEN
      ConListNum = GetObjectItemNum('ConnectorList',ConnectorListName)
      IF (ConListNum>0) THEN
        CALL GetObjectItem('ConnectorList',ConListNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStat)
        IF ((SameString(Alphas(2),'Connector:Splitter')) .OR.  (SameString(Alphas(4),'Connector:Splitter'))) THEN
          SplitterExists = .TRUE.
        END IF
        IF ((SameString(Alphas(2),'Connector:Mixer')) .OR. (SameString(Alphas(4),'Connector:Mixer'))) THEN
          MixerExists = .TRUE.
        END IF
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
                   '", connector list object.')
        CALL ShowContinueError('ConnectorList object="'// TRIM(ConnectorListName) //'" not found in input.')
      END IF
      errflag=.false.
      CALL GetNumSplitterMixerInConntrList('AirLoop',ConnectorListName,NumofSplitters,NumofMixers,errflag)
      if (errflag) then
      endif
    END IF

    ! If there is a SPLITTER, get its data
    IF (SplitterExists) THEN
      CALL GetObjectDefMaxArgs('Connector:Splitter',NumParams,NumAlphas,NumNodes)
      ALLOCATE(NodeNames(NumAlphas))
      ALLOCATE(NodeNumbers(NumAlphas))
      CALL GetLoopSplitter(PrimaryAirSystem(AirSysNum)%Name,ConnectorListName, &
                           PrimaryAirSystem(AirSysNum)%Splitter%Name,                  &
                           PrimaryAirSystem(AirSysNum)%Splitter%Exists,                &
                           PrimaryAirSystem(AirSysNum)%Splitter%NodeNameIn,            &
                           PrimaryAirSystem(AirSysNum)%Splitter%NodeNumIn,             &
                           PrimaryAirSystem(AirSysNum)%Splitter%TotalOutletNodes,      &
                           NodeNames,NodeNumbers,ErrorsFound)

      ALLOCATE(PrimaryAirSystem(AirSysNum)%Splitter%NodeNameOut(PrimaryAirSystem(AirSysNum)%Splitter%TotalOutletNodes))
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Splitter%NodeNumOut(PrimaryAirSystem(AirSysNum)%Splitter%TotalOutletNodes))
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Splitter%BranchNumOut(PrimaryAirSystem(AirSysNum)%Splitter%TotalOutletNodes))

      DO NodeNum = 1, PrimaryAirSystem(AirSysNum)%Splitter%TotalOutletNodes

        PrimaryAirSystem(AirSysNum)%Splitter%NodeNameOut(NodeNum) = NodeNames(NodeNum)
        PrimaryAirSystem(AirSysNum)%Splitter%NodeNumOut(NodeNum)  = NodeNumbers(NodeNum)

        PrimaryAirSystem(AirSysNum)%Splitter%BranchNumOut(NodeNum) = 0
        DO BranchNum=1,PrimaryAirSystem(AirSysNum)%NumBranches

          IF (PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNumIn .EQ. &
            PrimaryAirSystem(AirSysNum)%Splitter%NodeNumOut(NodeNum)) THEN
              PrimaryAirSystem(AirSysNum)%Splitter%BranchNumOut(NodeNum) = BranchNum
              EXIT
          END IF

        END DO

      END DO

      PrimaryAirSystem(AirSysNum)%Splitter%BranchNumIn = 0
      DO BranchNum=1,PrimaryAirSystem(AirSysNum)%NumBranches

        IF (PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNumOut .EQ. &
          PrimaryAirSystem(AirSysNum)%Splitter%NodeNumIn) THEN
            PrimaryAirSystem(AirSysNum)%Splitter%BranchNumIn = BranchNum
            EXIT
        END IF

      END DO

      IF (ALLOCATED(NodeNames)) THEN
        DEALLOCATE(NodeNames)
        DEALLOCATE(NodeNumbers)
      ENDIF

    ELSE
      PrimaryAirSystem(AirSysNum)%Splitter%Exists = .FALSE.
      PrimaryAirSystem(AirSysNum)%Splitter%NodeNumIn = 0
      PrimaryAirSystem(AirSysNum)%Splitter%BranchNumIn = 0
      PrimaryAirSystem(AirSysNum)%Splitter%NodeNameIn = ' '
      PrimaryAirSystem(AirSysNum)%Splitter%TotalOutletNodes = 0
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Splitter%NodeNumOut(0))
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Splitter%BranchNumOut(0))
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Splitter%NodeNameOut(0))
    END IF

    ! If there is a MIXER, get its data
    IF (MixerExists) THEN
      CALL GetObjectDefMaxArgs('Connector:Mixer',NumParams,NumAlphas,NumNodes)
      ALLOCATE(NodeNames(NumAlphas))
      ALLOCATE(NodeNumbers(NumAlphas))
      CALL GetLoopMixer(PrimaryAirSystem(AirSysNum)%Name,ConnectorListName, &
                           PrimaryAirSystem(AirSysNum)%Mixer%Name,                  &
                           PrimaryAirSystem(AirSysNum)%Mixer%Exists,                &
                           PrimaryAirSystem(AirSysNum)%Mixer%NodeNameOut,           &
                           PrimaryAirSystem(AirSysNum)%Mixer%NodeNumOut,            &
                           PrimaryAirSystem(AirSysNum)%Mixer%TotalInletNodes,       &
                           NodeNames,NodeNumbers,ErrorsFound)

      ALLOCATE(PrimaryAirSystem(AirSysNum)%Mixer%NodeNameIn(PrimaryAirSystem(AirSysNum)%Mixer%TotalInletNodes))
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Mixer%NodeNumIn(PrimaryAirSystem(AirSysNum)%Mixer%TotalInletNodes))
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Mixer%BranchNumIn(PrimaryAirSystem(AirSysNum)%Mixer%TotalInletNodes))

      DO NodeNum = 1, PrimaryAirSystem(AirSysNum)%Mixer%TotalInletNodes

        PrimaryAirSystem(AirSysNum)%Mixer%NodeNameIn(NodeNum) = NodeNames(NodeNum)
        PrimaryAirSystem(AirSysNum)%Mixer%NodeNumIn(NodeNum)  = NodeNumbers(NodeNum)

        PrimaryAirSystem(AirSysNum)%Mixer%BranchNumIn(NodeNum) = 0
        DO BranchNum=1,PrimaryAirSystem(AirSysNum)%NumBranches

          IF (PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNumIn .EQ. &
            PrimaryAirSystem(AirSysNum)%Mixer%NodeNumIn(NodeNum)) THEN
              PrimaryAirSystem(AirSysNum)%Mixer%BranchNumIn(NodeNum) = BranchNum
              EXIT
          END IF

        END DO

      END DO

      PrimaryAirSystem(AirSysNum)%Mixer%BranchNumOut = 0
      DO BranchNum=1,PrimaryAirSystem(AirSysNum)%NumBranches

        IF (PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNumIn .EQ. &
          PrimaryAirSystem(AirSysNum)%Mixer%NodeNumOut) THEN
            PrimaryAirSystem(AirSysNum)%Mixer%BranchNumOut = BranchNum
            EXIT
        END IF

      END DO

      IF (ALLOCATED(NodeNames)) THEN
        DEALLOCATE(NodeNames)
        DEALLOCATE(NodeNumbers)
      ENDIF

    ELSE
      PrimaryAirSystem(AirSysNum)%Mixer%Exists = .FALSE.
      PrimaryAirSystem(AirSysNum)%Mixer%NodeNumOut = 0
      PrimaryAirSystem(AirSysNum)%Mixer%BranchNumOut = 0
      PrimaryAirSystem(AirSysNum)%Mixer%NodeNameOut = ' '
      PrimaryAirSystem(AirSysNum)%Mixer%TotalInletNodes = 0
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Mixer%NodeNumIn(0))
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Mixer%BranchNumIn(0))
      ALLOCATE(PrimaryAirSystem(AirSysNum)%Mixer%NodeNameIn(0))
    END IF

    NumControllers = 0
    IF (ControllerListName /= ' ') THEN   ! If not blank, then must be there and valid
      ! Loop through the controller lists until you find the one attached to this primary air system
      ControllerListNum = GetObjectItemNum('AirLoopHVAC:ControllerList',ControllerListName)
      IF (ControllerListNum > 0) THEN
        CALL GetObjectItem('AirLoopHVAC:ControllerList',ControllerListNum,Alphas,NumAlphas,&
                           Numbers,NumNumbers,IOStat)
        !Check the current controller list and if it matches input names
        NumControllers = (NumAlphas-1)/2  !Subtract off the controller list name first
        ! store all the controller data
        PrimaryAirSystem(AirSysNum)%NumControllers = NumControllers + NumOASysSimpControllers
        Allocate(PrimaryAirSystem(AirSysNum)%ControllerName(NumControllers + NumOASysSimpControllers))
        Allocate(PrimaryAirSystem(AirSysNum)%ControllerType(NumControllers + NumOASysSimpControllers))
        Allocate(PrimaryAirSystem(AirSysNum)%ControllerIndex(NumControllers + NumOASysSimpControllers))
        PrimaryAirSystem(AirSysNum)%ControllerIndex=0
        Allocate(PrimaryAirSystem(AirSysNum)%ControlConverged(NumControllers + NumOASysSimpControllers))
        Allocate(PrimaryAirSystem(AirSysNum)%CanBeLockedOutByEcono(NumControllers + NumOASysSimpControllers))
        DO ControllerNum=NumOASysSimpControllers+1,NumOASysSimpControllers+NumControllers
          ControllerName = Alphas((ControllerNum-NumOASysSimpControllers)*2+1)
          ControllerType = Alphas((ControllerNum-NumOASysSimpControllers)*2)
          PrimaryAirSystem(AirSysNum)%ControllerName(ControllerNum) = ControllerName
          PrimaryAirSystem(AirSysNum)%ControllerType(ControllerNum) = ControllerType
          IsNotOK=.false.
          CALL ValidateComponent(ControllerType,ControllerName,IsNotOK,TRIM(CurrentModuleObject))
          IF (IsNotOk) THEN
            CALL ShowContinueError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
                                   '", for ControllerList="'//TRIM(ControllerListName)//'".')
            ErrorsFound=.true.
          ENDIF
          PrimaryAirSystem(AirSysNum)%ControlConverged(ControllerNum) = .False.
          PrimaryAirSystem(AirSysNum)%CanBeLockedOutByEcono(ControllerNum) = .False.
        END DO  !End of ControllerListNum Loop
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
                   '", controller list object.')
        CALL ShowContinueError('ControllerList object="'// TRIM(ControllerListName) //'" not found in input.')
        ErrorsFound=.true.
      ENDIF
    ENDIF
    IF (NumOASysSimpControllers > 0) THEN
      CALL GetObjectItem('AirLoopHVAC:ControllerList',OASysContListNum,Alphas,NumAlphas,&
                         Numbers,NumNumbers,IOStat)
      ! allocate air primary system controller lists if not already done
      IF (NumControllers .EQ. 0) THEN
        PrimaryAirSystem(AirSysNum)%NumControllers = NumOASysSimpControllers
        Allocate(PrimaryAirSystem(AirSysNum)%ControllerName(NumOASysSimpControllers))
        Allocate(PrimaryAirSystem(AirSysNum)%ControllerType(NumOASysSimpControllers))
        Allocate(PrimaryAirSystem(AirSysNum)%ControllerIndex(NumOASysSimpControllers))
        PrimaryAirSystem(AirSysNum)%ControllerIndex=0
        Allocate(PrimaryAirSystem(AirSysNum)%ControlConverged(NumOASysSimpControllers))
        Allocate(PrimaryAirSystem(AirSysNum)%CanBeLockedOutByEcono(NumOASysSimpControllers))
        PrimaryAirSystem(AirSysNum)%ControlConverged=.FALSE.
        PrimaryAirSystem(AirSysNum)%CanBeLockedOutByEcono=.FALSE.
      END IF
      ! loop over the OA Sys controllers and move them up to the primary air system controller lists
      OASysControllerNum = 0
      NumOASysControllers = (NumAlphas-1)/2
      DO ControllerNum=1,NumOASysControllers
        ControllerName = Alphas(ControllerNum*2+1)
        ControllerType = Alphas(ControllerNum*2)
        IF (.not. SameString(ControllerType,'Controller:OutdoorAir')) THEN
          OASysControllerNum = OASysControllerNum + 1
          PrimaryAirSystem(AirSysNum)%ControllerName(OASysControllerNum) = ControllerName
          PrimaryAirSystem(AirSysNum)%ControllerType(OASysControllerNum) = ControllerType
          PrimaryAirSystem(AirSysNum)%ControlConverged(OASysControllerNum) = .False.
          PrimaryAirSystem(AirSysNum)%CanBeLockedOutByEcono(OASysControllerNum) = .TRUE.
!         Coil controllers can be entered either in the air loop controller list or the
!         OA system controller list. The CanBeLockedOutByEcono should only be set for OA coils
!         First get the OA controller actuator node and then compare to the air loop coil water inlet node
!         If these node numbers match, the coil is in the main air loop and the lockout flag should be reset to FALSE
          CALL GetControllerActuatorNodeNum(ControllerName,ActuatorNodeNum,ErrFlag)
          DO BranchNum = 1, PrimaryAirSystem(AirSysNum)%NumBranches
            DO CompNum = 1, PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%TotalComponents
              IF(SameString(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                            'AirloopHVAC:OutdoorAirSystem'))CYCLE
              CompType = PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
              WaterCoilNodeNum = -1
              IF (SameString(CompType,'Coil:Cooling:Water:DetailedGeometry') .OR. &
                  SameString(CompType,'Coil:Heating:Water') .OR. &
                  SameString(CompType,'Coil:Cooling:Water')) THEN
                WaterCoilNodeNum = GetCoilWaterInletNode(  &
                   PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf,  &
                   PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%Name,  &
                   ErrorsFound)
              END IF
              IF(WaterCoilNodeNum == ActuatorNodeNum) THEN
                PrimaryAirSystem(AirSysNum)%CanBeLockedOutByEcono(OASysControllerNum) = .FALSE.
              END IF
            END DO
          END DO
        END IF
      END DO
    END IF
    IF (NumControllers+NumOASysSimpControllers .EQ. 0) THEN
      IF (.NOT. PackagedUnit(AirSysNum)) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//  &
                         '" has no Controllers.')
      END IF
      PrimaryAirSystem(AirSysNum)%NumControllers = 0
      Allocate(PrimaryAirSystem(AirSysNum)%ControllerName(0))
      Allocate(PrimaryAirSystem(AirSysNum)%ControllerType(0))
      Allocate(PrimaryAirSystem(AirSysNum)%ControlConverged(0))
      Allocate(PrimaryAirSystem(AirSysNum)%CanBeLockedOutByEcono(0))
    END IF

    errflag=.false.
    CALL GetAirLoopAvailabilityManager(AvailManagerListName,AirSysNum,NumPrimaryAirSys,errflag)

    IF (errflag) THEN
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(PrimaryAirSystem(AirSysNum)%Name))
      ErrorsFound=.true.
    ENDIF

END DO  !End Air Loop

DEALLOCATE(Numbers)
DEALLOCATE(cNumericFields)
DEALLOCATE(lNumericBlanks)
DEALLOCATE(Alphas)
DEALLOCATE(cAlphaFields)
DEALLOCATE(lAlphaBlanks)

DEALLOCATE(TestUniqueNodes)
DO AirSysNum=1,NumPrimaryAirSys
  DO BranchNum = 1,PrimaryAirSystem(AirSysNum)%NumBranches
    DO CompNum = 1, PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%TotalComponents

      SELECT CASE (MakeUPPERCase(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf))

        CASE('AIRLOOPHVAC:OUTDOORAIRSYSTEM')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=OAMixer_Num

      ! Fan Types for the air sys simulation
        CASE('FAN:CONSTANTVOLUME')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Fan_Simple_CV

        CASE('FAN:VARIABLEVOLUME')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Fan_Simple_VAV

        ! cpw22Aug2010 Add Fan_ComponentModel type (new num=24)
        CASE('FAN:COMPONENTMODEL')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Fan_ComponentModel

      ! Coil Types for the air sys simulation
!        HX Assisted coils are not allowed on a branch at this time
!        CASE('COILSYSTEM:COOLING:DX:HEATEXCHANGERASSISTED')
!          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=DXCoil_CoolingHXAsst
        CASE('COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=WaterCoil_CoolingHXAsst
        CASE('COIL:HEATING:WATER')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=WaterCoil_SimpleHeat
        CASE('COIL:HEATING:STEAM')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=SteamCoil_AirHeat
        CASE('COIL:COOLING:WATER:DETAILEDGEOMETRY')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=WaterCoil_DetailedCool
        CASE('COIL:COOLING:WATER')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=WaterCoil_Cooling
        CASE('COIL:HEATING:ELECTRIC')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Coil_ElectricHeat
        CASE('COIL:HEATING:GAS')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Coil_GasHeat

      ! Heat reclaim
        CASE('COIL:HEATING:DESUPERHEATER')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Coil_DeSuperHeat

        CASE('COILSYSTEM:COOLING:DX')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=DXSystem
        CASE('COILSYSTEM:HEATING:DX')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=DXHeatPumpSystem
        CASE('COIL:USERDEFINED')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=CoilUserDefined
        CASE('AIRLOOPHVAC:UNITARYSYSTEM')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=UnitarySystem
        CASE('AIRLOOPHVAC:UNITARY:FURNACE:HEATONLY')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Furnace_UnitarySys
        CASE('AIRLOOPHVAC:UNITARY:FURNACE:HEATCOOL')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Furnace_UnitarySys
        CASE('AIRLOOPHVAC:UNITARYHEATONLY')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Furnace_UnitarySys
        CASE('AIRLOOPHVAC:UNITARYHEATCOOL')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Furnace_UnitarySys
        CASE('AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Furnace_UnitarySys
        CASE('AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Furnace_UnitarySys

        CASE('AIRLOOPHVAC:UNITARYHEATCOOL:VAVCHANGEOVERBYPASS')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=UnitarySystem_BypassVAVSys

      ! Humidifier Types for the air system simulation
        CASE('HUMIDIFIER:STEAM:ELECTRIC')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Humidifier

      ! Evap Cooler Types for the air system simulation
        CASE('EVAPORATIVECOOLER:DIRECT:CELDEKPAD')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=EvapCooler
        CASE('EVAPORATIVECOOLER:INDIRECT:CELDEKPAD')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=EvapCooler
        CASE('EVAPORATIVECOOLER:INDIRECT:WETCOIL')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=EvapCooler
        CASE('EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=EvapCooler
        CASE('EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=EvapCooler

      ! Desiccant Dehumidifier Types for the air system simulation
        CASE('DEHUMIDIFIER:DESICCANT:NOFANS')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Desiccant
        CASE('DEHUMIDIFIER:DESICCANT:SYSTEM')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Desiccant

      ! Heat recovery
        CASE('HEATEXCHANGER:AIRTOAIR:FLATPLATE')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=HeatXchngr

        CASE('HEATEXCHANGER:DESICCANT:BALANCEDFLOW')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=HeatXchngr

      ! Ducts
        CASE('DUCT')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=Duct

        CASE('AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED')
          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=UnitarySystem_MSHeatPump

        CASE ('FAN:ONOFF',  &
              'COIL:COOLING:DX:SINGLESPEED', &
              'COIL:HEATING:DX:SINGLESPEED', &
              'COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE',  &
              'COIL:COOLING:DX:MULTISPEED',  &
              'COIL:HEATING:DX:MULTISPEED')
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = "'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//'".')
          CALL ShowContinueError('..Invalid Air Loop Component Type = "'//  &
                               TRIM(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf)//'".')
          CALL ShowContinueError('..Air Loop Component Name = "'//  &
                                 TRIM(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%Name)//'".')
          CALL ShowContinueError('..reference Branch = "'// &
                                 TRIM(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Name)//'".')
          CALL ShowContinueError('...This component may only be referenced by a parent component '//  &
               'such as AirLoopHVAC:Unitary:Furnace:HeatCool or similar.')
          ErrorsFound=.true.

        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = "'//TRIM(PrimaryAirSystem(AirSysNum)%Name)//'".')
          CALL ShowContinueError('..Invalid Air Loop Component Type = "'//  &
                               TRIM(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf)//'".')
          CALL ShowContinueError('..Air Loop Component Name = "'//  &
                                 TRIM(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%Name)//'".')
          CALL ShowContinueError('..reference Branch = "'// &
                                 TRIM(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Name)//'".')
          ErrorsFound=.true.

      END SELECT

    ENDDO
  ENDDO
ENDDO

! check that actuator nodes are matched by a water coil inlet node

DO AirSysNum=1,NumPrimaryAirSys
    DO BranchNum = 1, PrimaryAirSystem(AirSysNum)%NumBranches
        DO CompNum = 1, PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%TotalComponents
            CompType_Num = PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num
            If (CompType_Num == WaterCoil_DetailedCool .OR. CompType_Num == WaterCoil_SimpleHeat .OR. &
                CompType_Num == WaterCoil_Cooling) THEN
                WaterCoilNodeNum = GetCoilWaterInletNode(  &
                   PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf,  &
                   PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%Name,  &
                   ErrorsFound)
                CALL CheckCoilWaterInletNode(WaterCoilNodeNum, NodeNotFound)
                IF (NodeNotFound) THEN
                    ErrorsFound=.true.
                    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
                        TRIM(PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%Name)// &
                        '", invalid actuator.')
                    CALL ShowContinueError('...this coil requires a water coil controller '// &
                        'and the inlet node of a water coil must also be an actuator node of a water coil controller.')
                END IF
            END IF
        END DO
    END DO
END DO

OANum=GetNumOASystems()
DO OASysNum=1,OANum
    NumInList=GetOACompListNumber(OASysNum)
        DO OACompNum = 1, NumInList
           CompType_Num = GetOACompTypeNum(OASysNum, OACompNum)
           If (CompType_Num == WaterCoil_DetailedCool .OR. CompType_Num == WaterCoil_SimpleHeat .OR. &
               CompType_Num == WaterCoil_Cooling) THEN
               WaterCoilNodeNum = GetCoilWaterInletNode(  &
               GetOACompType(OASysNum, OACompNum),  &
               GetOACompName(OASysNum, OACompNum),  &
               ErrorsFound)
               CALL CheckCoilWaterInletNode(WaterCoilNodeNum, NodeNotFound)
               IF (NodeNotFound) THEN
                   ErrorsFound=.true.
                   CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
                       TRIM(GetOACompName(OASysNum, OACompNum))// &
                       '", invalid actuator.')
                   CALL ShowContinueError('...this coil requires a water coil controller '// &
                       'and the inlet node of a water coil must also be an actuator node of a water coil controller.')
               END IF
           END IF
        END DO
END DO

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found retrieving input for '//trim(CurrentModuleObject)//'.')
ENDIF

DO AirSysNum=1,NumPrimaryAirSys
  CALL SetupOutputVariable('Air System Simulation Cycle On Off Status []', PriAirSysAvailMgr(AirSysNum)%AvailStatus, &
                           'HVAC','Average', PrimaryAirSystem(AirSysNum)%Name)
END DO


RETURN

END SUBROUTINE GetAirPathData

! End of Get Input subroutines for the Module
!******************************************************************************

! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitAirLoops(FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   April 1998
          !       MODIFIED       Dec 1999 Fred Buhl
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes the primary air system simulation

          ! METHODOLOGY EMPLOYED:
          ! (1) For the first simulation in an HVAC timestep, the air system is initialized to
          !     design air flow rates.
          ! (2) For subsequent simulations, air flow data is set by the zone equipment inlet
          !     nodes and the return air node.
          ! (3) Other air system node data such as temperatures and humidity ratios are only
          !     initialized at the start of an environment (run period or design day).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataEnvironment, ONLY: StdBaroPress, OutHumRat, StdRhoAir
  USE SplitterComponent, ONLY: SplitterCond, NumSplitters
  USE InputProcessor, ONLY: FindItemInList, SameString
  USE Psychrometrics, ONLY: PsyHFnTdbW,PsyRhoAirFnPbTdbW
  USE ZonePlenum, ONLY: ZoneSupPlenCond, NumZoneSupplyPlenums
  USE DataConvergParams, ONLY: HVACFlowRateToler, AirLoopConvergence, ZoneInletConvergence
  USE DataContaminantBalance, ONLY: Contaminant, OutdoorCO2, OutdoorGC
  USE General, ONLY: FindNumberinList

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
  LOGICAL, INTENT (IN) :: FirstHVACIteration ! TRUE if first full HVAC iteration in an HVAC timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumAllSupAirPathNodes  ! total number of nodes in a supply air path including duplicates
  INTEGER :: NumSupAirPathNodes  ! total number of nodes in a supply air path
  INTEGER :: NumSupAirPathOutNodes  ! total number of outlet nodes in a supply air path
  INTEGER :: NumSupAirPathIntNodes  ! total number of intermediate nodes in a supply air path
  INTEGER :: NodeIndex  ! DO loop index for nodes on branch
  INTEGER :: SupNodeIndex      ! do loop index of a supply air path node
  INTEGER :: SupNodeIndex2     ! 2nd do loop index of a supply air path node
  INTEGER :: SupAirPathNodeNum ! index of a supply air path node
  INTEGER :: SupAirPathOutNodeNum ! index of a supply air path outlet node
  INTEGER :: AirLoopNum ! DO loop counter for air systems
  INTEGER :: BranchNum  ! DO loop counter for branches
  INTEGER :: OutBranchNum     ! reference number of an outlet branch
  INTEGER :: InBranchNum      ! reference number of an inlet branch
!unused  INTEGER :: InletBranchNum   ! Branch reference number of splitter inlet branch
  INTEGER :: NodeNum    ! a node number
  INTEGER :: OutNum     ! DO loop index for outlet branches
  INTEGER :: InNum      ! DO loop index for inlet branches
  INTEGER :: CompNum      ! DO loop index for  branch components
  INTEGER :: ZoneSideNodeNum  ! a Zone Equipment inlet node number
  INTEGER :: BranchNodeIndex  ! DO loop index for nodes on a branch
  INTEGER :: NodeNumOut       ! node number of a branch outlet node
  INTEGER :: NodeNumIn        ! node number of a splitter inlet node or a branch inlet node
  INTEGER :: SplitterOutNum   ! DO loop index of splitter outlets
  INTEGER :: PlenumOutNum     ! DO loop index of supply plenum outlets
  REAL(r64)    :: MassFlowSaved    ! mass flow rate for a node saved from previous call
  REAL(r64)    :: MassFlowSet      ! desired mass flow rate for a node
  REAL(r64)    :: SumZoneDesFlow=0.0d0 ! sum of the zone design air mass flow rates for zones served by a system
  INTEGER :: SupAirPath       ! supply air path do loop index
  INTEGER :: SupAirPathNum    ! specific supply air path index
  INTEGER :: SplitterNum      ! Zone equip splitter index
  INTEGER :: PlenumNum        ! supply plenum index
  INTEGER :: CtrlZoneNum      ! Controlled zone index
  INTEGER :: ZoneInNum        ! zone inlet index
  INTEGER :: NumZonesCool     ! number of zones in system supplied with cooling
  INTEGER :: NumZonesHeat     ! number of zones in system supplied with heating
  INTEGER :: ZoneInSysIndex   ! index into CoolCtrlZoneNums or HeatCtrlZoneNums
  INTEGER :: NumComponentsInSys  ! total number of components in the primary air system
  INTEGER :: NumComponentsOnBranch  ! total number of components in the primary air system
  LOGICAL :: FoundSupPathZoneConnect ! true if there is a valid connection between the supply air path
                                     ! and a zone terminal unit inlet
  INTEGER :: TUInNode=0       ! inlet node number of a terminal unit
  REAL(r64),SAVE    :: MassFlowSetToler
  INTEGER,DIMENSION(:),SAVE,ALLOCATABLE  ::  CtrlZoneNumsCool
  INTEGER,DIMENSION(:),SAVE,ALLOCATABLE  ::  CtrlZoneNumsHeat
  INTEGER,DIMENSION(:),SAVE,ALLOCATABLE  ::  ZoneInletNodesCool
  INTEGER,DIMENSION(:),SAVE,ALLOCATABLE  ::  ZoneInletNodesHeat
  INTEGER,DIMENSION(:),SAVE,ALLOCATABLE  ::  TermInletNodesCool
  INTEGER,DIMENSION(:),SAVE,ALLOCATABLE  ::  TermInletNodesHeat
  INTEGER,DIMENSION(:),SAVE,ALLOCATABLE  ::  SupNode
  INTEGER,DIMENSION(:),SAVE,ALLOCATABLE  ::  SupNodeType

    !Dimension the local subcomponent arrays


  !Simulation Flags
  LOGICAL,SAVE :: MyEnvrnFlag=.true.
  LOGICAL,SAVE :: MyOneTimeFlag = .true.
  LOGICAL,SAVE :: MyBranchSizingFlag = .true.
  LOGICAL :: ErrorsFound
  REAL(r64) :: OAReliefDiff = 0.d0 ! local for massflow change across OA system, kg/s

  INTEGER, DIMENSION(:), ALLOCATABLE    :: tmpNodeARR
  INTEGER :: nodeCount
  INTEGER :: nodeLoop
  INTEGER :: ZoneNum


  ErrorsFound = .FALSE.
  AirLoopInit = .TRUE.


  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ! Figure out what zones are served by each primary air system (air loop) and
    ! store the results in AirToZoneNodeInfo()%CoolCtrlZoneNums and AirToZoneNodeInfo()%HeatCtrlZoneNums

    ! Allocate scratch arrays for storing controlled zone numbers for each air loop.
    ALLOCATE(CtrlZoneNumsCool(NumOfZones))
    ALLOCATE(CtrlZoneNumsHeat(NumOfZones))
    ALLOCATE(ZoneInletNodesCool(NumOfZones))
    ALLOCATE(ZoneInletNodesHeat(NumOfZones))
    ALLOCATE(TermInletNodesCool(NumOfZones))
    ALLOCATE(TermInletNodesHeat(NumOfZones))

    MassFlowSetToler = HVACFlowRateToler * 0.00001d0

    SupplyAirPathLoop: DO SupAirPath=1,NumSupplyAirPaths

      NumAllSupAirPathNodes = 0
      SupAirPathNodeNum = 0
      SupAirPathOutNodeNum = 0
      NumSupAirPathOutNodes = 0
      NumSupAirPathNodes = 0
      NumSupAirPathIntNodes = 0

  ! each supply air path may have up to one splitter and one plenum.  Check for all combinations count
  ! all nodes (including duplicates)
      DO CompNum=1,SupplyAirPath(SupAirPath)%NumOfComponents
        IF (SameString(SupplyAirPath(SupAirPath)%ComponentType(CompNum),'AirLoopHVAC:ZoneSplitter')) THEN
          SplitterNum=FindItemInList(SupplyAirPath(SupAirPath)%ComponentName(CompNum), &
                                     SplitterCond%SplitterName, NumSplitters)
          IF (SplitterNum == 0) THEN
            CALL ShowSevereError('AirLoopHVAC:ZoneSplitter not found='//TRIM(SupplyAirPath(SupAirPath)%ComponentName(CompNum)))
            CALL ShowContinueError('Occurs in AirLoopHVAC:SupplyPath='//TRIM(SupplyAirPath(SupAirPath)%Name))
            ErrorsFound=.true.
          ENDIF
          SupplyAirPath(SupAirPath)%SplitterIndex(CompNum) = SplitterNum
          NumAllSupAirPathNodes = NumAllSupAirPathNodes + SplitterCond(SplitterNum)%NumOutletNodes + 1
        ELSE IF (SameString(SupplyAirPath(SupAirPath)%ComponentType(CompNum),'AirLoopHVAC:SupplyPlenum')) THEN
          PlenumNum=FindItemInList(SupplyAirPath(SupAirPath)%ComponentName(CompNum), &
                                     ZoneSupPlenCond%ZonePlenumName, NumZoneSupplyPlenums)
          IF (PlenumNum == 0) THEN
            CALL ShowSevereError('AirLoopHVAC:SupplyPlenum not found='//TRIM(SupplyAirPath(SupAirPath)%ComponentName(CompNum)))
            CALL ShowContinueError('Occurs in AirLoopHVAC:SupplyPath='//TRIM(SupplyAirPath(SupAirPath)%Name))
            ErrorsFound=.true.
          ENDIF
          SupplyAirPath(SupAirPath)%PlenumIndex(CompNum) = PlenumNum
          NumAllSupAirPathNodes = NumAllSupAirPathNodes + ZoneSupPlenCond(PlenumNum)%NumOutletNodes + 1
        END IF
      END DO
      ALLOCATE(SupNode(NumAllSupAirPathNodes))
      ALLOCATE(SupNodeType(NumAllSupAirPathNodes))

  ! figure out the order of the splitter and plenum in the path, by flagging the first node of the component
  ! as either a 'pathinlet' or a 'compinlet'
      DO CompNum=1,SupplyAirPath(SupAirPath)%NumOfComponents
        SplitterNum = SupplyAirPath(SupAirPath)%SplitterIndex(CompNum)
        PlenumNum = SupplyAirPath(SupAirPath)%PlenumIndex(CompNum)
        IF (SplitterNum > 0) THEN
          SupAirPathNodeNum = SupAirPathNodeNum + 1
          SupNode(SupAirPathNodeNum) = SplitterCond(SplitterNum)%InletNode
          IF (CompNum == 1) THEN
            SupNodeType(SupAirPathNodeNum) = PathInlet
          ELSE
            SupNodeType(SupAirPathNodeNum) = CompInlet
          END IF
          DO SplitterOutNum=1,SplitterCond(SplitterNum)%NumOutletNodes
            SupAirPathNodeNum = SupAirPathNodeNum + 1
            SupNode(SupAirPathNodeNum) = SplitterCond(SplitterNum)%OutletNode(SplitterOutNum)
            SupNodeType(SupAirPathNodeNum) = 0
          END DO
        ELSE IF (PlenumNum > 0) THEN
          SupAirPathNodeNum = SupAirPathNodeNum + 1
          SupNode(SupAirPathNodeNum) = ZoneSupPlenCond(PlenumNum)%InletNode
          IF (CompNum == 1) THEN
            SupNodeType(SupAirPathNodeNum) = PathInlet
          ELSE
            SupNodeType(SupAirPathNodeNum) = CompInlet
          END IF
          DO PlenumOutNum=1,ZoneSupPlenCond(PlenumNum)%NumOutletNodes
            SupAirPathNodeNum = SupAirPathNodeNum + 1
            SupNode(SupAirPathNodeNum) = ZoneSupPlenCond(PlenumNum)%OutletNode(PlenumOutNum)
            SupNodeType(SupAirPathNodeNum) = 0
          END DO
        END IF
      END DO

  ! find the nodes that connect a splitter and a plenum
      DO SupNodeIndex=1,NumAllSupAirPathNodes
        IF (SupNodeType(SupNodeIndex) == 0) THEN
          DO SupNodeIndex2=SupNodeIndex+1,NumAllSupAirPathNodes
            IF ( (SupNode(SupNodeIndex) == SupNode(SupNodeIndex2)) .AND. (SupNodeType(SupNodeIndex2) == CompInlet) ) THEN
              SupNodeType(SupNodeIndex) = Intermediate
              EXIT
            END IF
          END DO
        END IF
      END DO

  !  the rest of the nodes are outlet nodes and count the duplicated intermediate nodes
      DO SupNodeIndex=1,NumAllSupAirPathNodes
        IF (SupNodeType(SupNodeIndex) == 0) THEN
          NumSupAirPathOutNodes = NumSupAirPathOutNodes + 1
          SupNodeType(SupNodeIndex) = Outlet
        ELSE IF (SupNodeType(SupNodeIndex) == Intermediate) THEN
          NumSupAirPathIntNodes = NumSupAirPathIntNodes + 1
        END IF
      END DO

  !  eliminate the duplicates to find the number of nodes in the supply air path
      NumSupAirPathNodes = NumAllSupAirPathNodes - NumSupAirPathIntNodes
      SupAirPathNodeNum = 0
      ALLOCATE(SupplyAirPath(SupAirPath)%OutletNode(NumSupAirPathOutNodes))
      ALLOCATE(SupplyAirPath(SupAirPath)%Node(NumSupAirPathNodes))
      ALLOCATE(SupplyAirPath(SupAirPath)%NodeType(NumSupAirPathNodes))
      SupplyAirPath(SupAirPath)%NumNodes = NumSupAirPathNodes
      SupplyAirPath(SupAirPath)%NumOutletNodes = NumSupAirPathOutNodes

  ! transfer data from the local SupNode array to the SupplyAirPath data structure
      DO SupNodeIndex=1,NumAllSupAirPathNodes
        IF (SupNodeType(SupNodeIndex) == PathInlet .OR. SupNodeType(SupNodeIndex) == Intermediate &
            .OR. SupNodeType(SupNodeIndex) == Outlet) THEN
          SupAirPathNodeNum = SupAirPathNodeNum + 1
            ! map the local node numbers to the HVAC (global) node numbers
          SupplyAirPath(SupAirPath)%Node(SupAirPathNodeNum) = SupNode(SupNodeIndex)
          SupplyAirPath(SupAirPath)%NodeType(SupAirPathNodeNum) = SupNodeType(SupNodeIndex)
        END IF
        IF (SupNodeType(SupNodeIndex) == Outlet) THEN
          SupAirPathOutNodeNum = SupAirPathOutNodeNum + 1
            ! map the outlet node number to the HVAC (global) node number
          SupplyAirPath(SupAirPath)%OutletNode(SupAirPathOutNodeNum) = SupNode(SupNodeIndex)
        END IF
      END DO
      DEALLOCATE(SupNode)
      DEALLOCATE(SupNodeType)

    END DO SupplyAirPathLoop

!Now loop over the air loops
    PrimaryAirSysLoop: DO AirLoopNum = 1, NumPrimaryAirSys

      CtrlZoneNumsCool = 0
      CtrlZoneNumsHeat = 0
      ZoneInletNodesCool = 0
      ZoneInletNodesHeat = 0
      NumZonesCool = 0
      NumZonesHeat = 0
      NumComponentsInSys = 0

      ! count the number of components in this primary air system
      DO BranchNum=1,PrimaryAirSystem(AirLoopNum)%NumBranches
        NumComponentsInSys = NumComponentsInSys + PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
      END DO
      ! set the Simple flag
      IF (PrimaryAirSystem(AirLoopNum)%NumBranches == 1 .AND. NumComponentsInSys ==1) THEN
        AirLoopControlInfo(AirLoopNum)%Simple = .TRUE.
      END IF

      ! loop over the air loop's output nodes
      AirSysOutletsLoop: DO OutNum=1,AirToZoneNodeInfo(AirLoopNum)%NumSupplyNodes
        ZoneSideNodeNum = AirToZoneNodeInfo(AirLoopNum)%ZoneEquipSupplyNodeNum(OutNum)
        ! find the corresponding branch number
        OutBranchNum = PrimaryAirSystem(AirLoopNum)%OutletBranchNum(OutNum)
        ! find the supply air path corresponding to each air loop outlet node
        SupAirPathNum = 0
      ! loop over the air loop's output nodes
        SupplyAirPathLoop2: DO SupAirPath=1,NumSupplyAirPaths
          IF (ZoneSideNodeNum .EQ. SupplyAirPath(SupAirPath)%InletNodeNum) THEN
            SupAirPathNum = SupAirPath
            EXIT SupplyAirPathLoop2
          END IF
        END DO SupplyAirPathLoop2
        IF (SupAirPathNum > 0) THEN
          NumSupAirPathOutNodes= SupplyAirPath(SupAirPathNum)%NumOutletNodes
        ELSE
          NumSupAirPathOutNodes = 0
        END IF

        ! Now Loop over the Supply Air Path outlet nodes and find out which zone and which air terminal
        ! unit on that zone is connected to that supply air path.
        SupplyAirPathOutletLoop: DO SupAirPathOutNodeNum=1,NumSupAirPathOutNodes
          FoundSupPathZoneConnect = .FALSE.
          ! loop over all controlled zones.
          ControlledZoneLoop: DO CtrlZoneNum=1,NumOfZones
            IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
            ! Loop over the air distribution unit inlets for each controlled zone.
            ! Look for a match between the zone splitter outlet node and the air distribution unit inlet node.
            ! When match found save the controlled zone number in CtrlZoneNumsCool or CtrlZoneNumsHeat
            ZoneAirDistUnitInletsLoop: DO ZoneInNum=1,ZoneEquipConfig(CtrlZoneNum)%NumInletNodes
                 NumComponentsOnBranch = PrimaryAirSystem(AirLoopNum)%Branch(OutBranchNum)%TotalComponents

                    !BEGIN COOLING: Check for a match between the cooling air distribution unit inlet
                    !and the supply air path outlet
              IF (SupplyAirPath(SupAirPathNum)%OutletNode(SupAirPathOutNodeNum) .EQ. &
                  ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode) THEN
                IF (FindNumberinList(CtrlZoneNum,CtrlZoneNumsCool,NumZonesCool) == 0) THEN
                  NumZonesCool = NumZonesCool + 1
                  ! Set Duct Type for branch for dual duct
                  IF (NumZonesCool == 1 .AND. OutBranchNum > 1) THEN
                    PrimaryAirSystem(AirLoopNum)%Branch(OutBranchNum)%DuctType = Cooling
                  END IF
                  IF (NumZonesCool == 1) THEN
                    AirToZoneNodeInfo(AirLoopNum)%SupplyDuctType(OutNum) = Cooling
                  END IF
                  CtrlZoneNumsCool(NumZonesCool) = CtrlZoneNum
                  ZoneInletNodesCool(NumZonesCool) = ZoneEquipConfig(CtrlZoneNum)%InletNode(ZoneInNum)
                  TermInletNodesCool(NumZonesCool) = ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode
                  ZoneEquipConfig(CtrlZoneNum)%AirLoopNum = AirLoopNum
                ENDIF
                FoundSupPathZoneConnect = .TRUE.

                    !set the supply air path
                ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%SupplyAirPathExists = .TRUE.

                    !Once a match is found between a supply air path outlet node and an air distribution inlet
                    !node, we go on to the next supply air path outlet.  Therefore, *both* the air distribution
                    !unit loop and the controlled zone loop may be exited.
                EXIT ControlledZoneLoop
              END IF !end check for cooling air distribution units

                     !END COOLING: end check for match between supply air path outlet and cooling air
                     !distribution inlet

                    ! BEGIN HEATING: If we don't get a match, check for a heating match
              IF (SupplyAirPath(SupAirPathNum)%OutletNode(SupAirPathOutNodeNum) .EQ. &
                  ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%InNode) THEN
                IF (FindNumberinList(CtrlZoneNum,CtrlZoneNumsHeat,NumZonesHeat) == 0) THEN
                  NumZonesHeat = NumZonesHeat + 1
                  ! Set Duct Type for branch for dual duct
                  IF (NumZonesHeat == 1 .AND. OutBranchNum > 1) THEN
                    PrimaryAirSystem(AirLoopNum)%Branch(OutBranchNum)%DuctType = Heating
                  END IF
                  IF (NumZonesHeat == 1) THEN
                    AirToZoneNodeInfo(AirLoopNum)%SupplyDuctType(OutNum) = Heating
                  END IF
                  CtrlZoneNumsHeat(NumZonesHeat) = CtrlZoneNum
                  ZoneInletNodesHeat(NumZonesHeat) = ZoneEquipConfig(CtrlZoneNum)%InletNode(ZoneInNum)
                  TermInletNodesHeat(NumZonesHeat) = ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%InNode
                  IF (ZoneEquipConfig(CtrlZoneNum)%AirLoopNum == 0) ZoneEquipConfig(CtrlZoneNum)%AirLoopNum = AirLoopNum
                ENDIF
                FoundSupPathZoneConnect = .TRUE.

                    !Set the supply air path flag
                ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%SupplyAirPathExists = .TRUE.

                    !Once a match is found between a supply air path outlet node and an air distribution inlet
                    !node, we go on to the next supply air path outlet.  Therefore, *both* the air distribution
                    !unit loop and the controlled zone loop may be exited.
                EXIT ControlledZoneLoop
              END IF !end check for heatingair distribution units
            END DO ZoneAirDistUnitInletsLoop
          END DO ControlledZoneLoop

                !If the supply air path is not connected to either a heating or a cooling air distribution
                !unit...we have a problem!
          IF ( .NOT. FoundSupPathZoneConnect) THEN
            CALL ShowSevereError('Node ' // TRIM(NodeID(SupplyAirPath(SupAirPathNum)%OutletNode(SupAirPathOutNodeNum))) &
                                 // ' connects to no component')
            CALL ShowContinueError('Occurs in Supply Air Path='//TRIM(SupplyAirPath(SupAirPathNum)%Name))
            CALL ShowContinueError('Check the connection to a ZoneHVAC:EquipmentConnections object')
            CALL ShowContinueError('Check if this component is missing from the Supply Air Path')
            ErrorsFound=.true.
          END IF

        END DO SupplyAirPathOutletLoop

       ! What if there is no supply air path & the air loop outlet is just hooked directly to
        ! an air distribution unit of a single zone? In this case look for a match between
        ! ZoneSideNodeNum and a zone's air distribution unit inlets.
        IF (SupAirPathNum.EQ.0) THEN

          ControlledZoneLoop2: DO CtrlZoneNum=1,NumOfZones
            IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
            ! Loop over the air distribution unit inlets for each controlled zone.
            ! Look for a match between the zone equip inlet node and the air distribution unit inlet node.
            ! When match found save the controlled zone number in CtrlZoneNumsCool or CtrlZoneNumsHeat
            ZoneAirDistUnitInletsLoop2: DO ZoneInNum=1,ZoneEquipConfig(CtrlZoneNum)%NumInletNodes

              !set supply air path flag
              ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%SupplyAirPathExists = .FALSE.

              IF (ZoneSideNodeNum .EQ. ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode) THEN
                NumZonesCool = NumZonesCool + 1
                ! Set Duct Type for branch for dual duct
                IF (NumZonesCool == 1 .AND. OutBranchNum > 1) THEN
                  PrimaryAirSystem(AirLoopNum)%Branch(OutBranchNum)%DuctType = Cooling
                END IF
                CtrlZoneNumsCool(NumZonesCool) = CtrlZoneNum
                ZoneInletNodesCool(NumZonesCool) = ZoneEquipConfig(CtrlZoneNum)%InletNode(ZoneInNum)
                TermInletNodesCool(NumZonesCool) = ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode
                IF (ZoneEquipConfig(CtrlZoneNum)%AirLoopNum == 0) ZoneEquipConfig(CtrlZoneNum)%AirLoopNum = AirLoopNum

                EXIT ControlledZoneLoop2

              ENDIF

              IF (ZoneSideNodeNum .EQ. ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%InNode) THEN
                NumZonesHeat = NumZonesHeat + 1
                ! Set Duct Type for branch for dual duct
                IF (NumZonesHeat == 1 .AND. OutBranchNum > 1) THEN
                  PrimaryAirSystem(AirLoopNum)%Branch(OutBranchNum)%DuctType = Heating
                END IF
                CtrlZoneNumsHeat(NumZonesHeat) = CtrlZoneNum
                ZoneInletNodesHeat(NumZonesHeat) = ZoneEquipConfig(CtrlZoneNum)%InletNode(ZoneInNum)
                TermInletNodesHeat(NumZonesHeat) = ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%InNode
                IF (ZoneEquipConfig(CtrlZoneNum)%AirLoopNum == 0) ZoneEquipConfig(CtrlZoneNum)%AirLoopNum = AirLoopNum

                EXIT ControlledZoneLoop2

              END IF

            END DO ZoneAirDistUnitInletsLoop2
          END DO ControlledZoneLoop2
        END IF  ! End of no supply air path case

      END DO AirSysOutletsLoop

      ! we now know the number of heated and cooled zones served by this primary air system.
      ! Allocate the subarrays in AirToZoneNodeInfo
      ALLOCATE(AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(NumZonesCool))
      ALLOCATE(AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(NumZonesHeat))
      ALLOCATE(AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(NumZonesCool))
      ALLOCATE(AirToZoneNodeInfo(AirLoopNum)%HeatZoneInletNodes(NumZonesHeat))
      ALLOCATE(AirToZoneNodeInfo(AirLoopNum)%TermUnitCoolInletNodes(NumZonesCool))
      ALLOCATE(AirToZoneNodeInfo(AirLoopNum)%TermUnitHeatInletNodes(NumZonesHeat))
      ! Move the controlled zone numbers from the scratch arrays into AirToZoneNodeInfo
      CooledZonesLoop: DO ZoneInSysIndex=1,NumZonesCool
        AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZoneInSysIndex) = CtrlZoneNumsCool(ZoneInSysIndex)
        AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(ZoneInSysIndex) = ZoneInletNodesCool(ZoneInSysIndex)
        AirToZoneNodeInfo(AirLoopNum)%TermUnitCoolInletNodes(ZoneInSysIndex) = TermInletNodesCool(ZoneInSysIndex)
      END DO CooledZonesLoop

      HeatedZonesLoop: DO ZoneInSysIndex=1,NumZonesHeat
        AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZoneInSysIndex) = CtrlZoneNumsHeat(ZoneInSysIndex)
        AirToZoneNodeInfo(AirLoopNum)%HeatZoneInletNodes(ZoneInSysIndex) = ZoneInletNodesHeat(ZoneInSysIndex)
        AirToZoneNodeInfo(AirLoopNum)%TermUnitHeatInletNodes(ZoneInSysIndex) = TermInletNodesHeat(ZoneInSysIndex)
      END DO HeatedZonesLoop

      AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled = NumZonesCool
      AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated = NumZonesHeat

      IF ( (NumZonesCool+NumZonesHeat) == 0) THEN
        CALL ShowSevereError('An outlet node in AirLoopHVAC="' // TRIM(PrimaryAirSystem(AirLoopNum)%Name) // &
                              '" is not connected to any zone')
        CALL ShowContinueError('Could not match ZoneEquipGroup Inlet Node="'//TRIM(NodeID(ZoneSideNodeNum))//  &
                             '" to any Supply Air Path or controlled zone')
        ErrorsFound = .TRUE.
      END IF

      ! now fill the return air bypass information needed by the RAB setpoint manager
      IF (PrimaryAirSystem(AirLoopNum)%Splitter%Exists .AND. PrimaryAirSystem(AirLoopNum)%Mixer%Exists) THEN
        PrimaryAirSystem(AirLoopNum)%RABExists = .TRUE.
          DO BranchNum=1,PrimaryAirSystem(AirLoopNum)%NumBranches
            ! find the RAB branch; its inlet is a splitter outlet and it outlet is a mixer inlet
            IF ( (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumIn == &
                  PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumOut(1) .OR. &
                  PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumIn == &
                  PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumOut(2) ) .AND. &
                 (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumOut == &
                  PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumIn(1) .OR. &
                  PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumOut == &
                  PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumIn(2)) .AND. &
                 (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents == 1) .AND. &
                 (SameString(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(1)%TypeOf,'Duct')) ) THEN
              ! set the RAB splitter outlet node and the RAB mixer inlet node
              PrimaryAirSystem(AirLoopNum)%RABSplitOutNode = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumIn
              PrimaryAirSystem(AirLoopNum)%RABMixInNode = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumOut
              ! set the other nodes
              IF (PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumOut(1) == PrimaryAirSystem(AirLoopNum)%RABSplitOutNode) THEN
                PrimaryAirSystem(AirLoopNum)%OtherSplitOutNode = PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumOut(2)
              ELSE
                PrimaryAirSystem(AirLoopNum)%OtherSplitOutNode = PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumOut(1)
              END IF
              IF (PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumIn(1) == PrimaryAirSystem(AirLoopNum)%RABMixInNode) THEN
                PrimaryAirSystem(AirLoopNum)%SupMixInNode = PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumIn(2)
              ELSE
                PrimaryAirSystem(AirLoopNum)%SupMixInNode = PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumIn(1)
              END IF
              ! set the duct type
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%DuctType = RAB
            END IF
          END DO
          PrimaryAirSystem(AirLoopNum)%MixOutNode = PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumOut
        END IF

    END DO PrimaryAirSysLoop


    ! now register zone inlet nodes as critical demand nodes in the convergence tracking
    ALLOCATE(ZoneInletConvergence(NumOfZones))
    DO ZoneNum = 1, NumOfZones
      IF (ZoneEquipConfig(ZoneNum)%NumInletNodes > 0) THEN
        ZoneInletConvergence(ZoneNum)%NumInletNodes = ZoneEquipConfig(ZoneNum)%NumInletNodes
        ALLOCATE(ZoneInletConvergence(ZoneNum)%InletNode(ZoneEquipConfig(ZoneNum)%NumInletNodes))
        Do nodeLoop =1, ZoneEquipConfig(ZoneNum)%NumInletNodes
          ZoneInletConvergence(ZoneNum)%InletNode(nodeLoop)%NodeNum = ZoneEquipConfig(ZoneNum)%InletNode(nodeLoop)
        ENDDO
      ENDIF
    END DO

    MyOneTimeFlag = .false.

    DEALLOCATE(CtrlZoneNumsCool)
    DEALLOCATE(CtrlZoneNumsHeat)
    DEALLOCATE(ZoneInletNodesCool)
    DEALLOCATE(ZoneInletNodesHeat)
    DEALLOCATE(TermInletNodesCool)
    DEALLOCATE(TermInletNodesHeat)

    IF (ErrorsFound) THEN
      CALL ShowFatalError('Preceding errors cause termination')
    END IF

  END IF !one time flag

  ! Size the air loop branch air flows
  IF ( .NOT. SysSizingCalc .AND. MyBranchSizingFlag) THEN

    DO AirLoopNum = 1, NumPrimaryAirSys

      DO BranchNum=1,PrimaryAirSystem(AirLoopNum)%NumBranches
        CALL SizeAirLoopBranches(AirLoopNum,BranchNum)
      END DO

    END DO

    MyBranchSizingFlag = .FALSE.

    ! calculate the ratio of air loop design flow to the sum of the zone design flows
    DO AirLoopNum = 1, NumPrimaryAirSys
      SumZoneDesFlow = 0.0d0
      DO ZoneInSysIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
        TUInNode = AirToZoneNodeInfo(AirLoopNum)%TermUnitCoolInletNodes(ZoneInSysIndex)
        SumZoneDesFlow = SumZoneDesFlow + Node(TUInNode)%MassFlowRateMax
      END DO
      IF (SumZoneDesFlow > VerySmallMassFlow) THEN
        AirLoopFlow(AirLoopNum)%SysToZoneDesFlowRatio = PrimaryAirSystem(AirLoopNum)%DesignVolFlowRate*StdRhoAir/SumZoneDesFlow
      ELSE
        AirLoopFlow(AirLoopNum)%SysToZoneDesFlowRatio = 1.0d0
      END IF
    END DO

  END IF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. FirstHVACIteration .and. MyEnvrnFlag) THEN

    IF (NumPrimaryAirSys > 0) THEN
      PriAirSysAvailMgr%AvailStatus = NoAction
      PriAirSysAvailMgr%StartTime = 0
      PriAirSysAvailMgr%StopTime = 0
    END IF

    DO AirLoopNum = 1, NumPrimaryAirSys   ! Start looping through all of the air loops...

      DO BranchNum = 1,PrimaryAirSystem(AirLoopNum)%NumBranches ! loop over all branches in system
        PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxMassFlowRate = StdRhoAir * &
                               PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate
        PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MinMassFlowRate = StdRhoAir * &
                               PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MinVolFlowRate
        DO NodeIndex = 1,PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalNodes ! loop over alll nodes on branch

          NodeNum = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNum(NodeIndex)

          ! Initialize the nodes to a standard set of initial conditions that will
          !  change after the first iteration to a system value
          Node(NodeNum)%Temp         = 20.0d0
          Node(NodeNum)%HumRat       = OutHumRat
          Node(NodeNum)%Enthalpy     = PsyHFnTdbW(Node(NodeNum)%Temp,Node(NodeNum)%HumRat)
          ! set the node mass flow rates to the branch mass flow rate
          Node(NodeNum)%MassFlowRate = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxMassFlowRate
          Node(NodeNum)%MassFlowRateMax = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxMassFlowRate
          Node(NodeNum)%MassFlowRateMaxAvail = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxMassFlowRate
          Node(NodeNum)%MassFlowRateMin = 0.0d0
          Node(NodeNum)%MassFlowRateSetPoint = 0.0d0
          Node(NodeNum)%MassFlowRateMinAvail = 0.0d0
          Node(NodeNum)%Press        = StdBaroPress
          Node(NodeNum)%Quality      = 0.0d0
          IF (Contaminant%CO2Simulation) Then
            Node(NodeNum)%CO2 = OutdoorCO2
          End If
          IF (Contaminant%GenericContamSimulation) Then
            Node(NodeNum)%GenContam = OutdoorGC
          End If

        END DO ! end of loop over nodes on each branch

      END DO  ! end of loop through branches in system

    END DO  ! end of loop over primary air systems
    MyEnvrnFlag=.false.

  END IF  ! End the environment initializations

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.true.
  ENDIF

  ! Do the Begin Day initializations
  IF (BeginDayFlag) THEN

  END IF

  ! There are no hourly initializations done in the heat balance

  ! Do the following initializations (every time step).

  DO AirLoopNum = 1, NumPrimaryAirSys
    ! zero all MassFlowRateSetPoints
    DO BranchNum = 1,PrimaryAirSystem(AirLoopNum)%NumBranches ! loop over all branches in system
      IF (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%DuctType == RAB) CYCLE
      DO NodeIndex = 1,PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalNodes ! loop over alll nodes on branch
        NodeNum = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNum(NodeIndex)
        Node(NodeNum)%MassFlowRateSetPoint = 0.0d0
        ! Reset MassFlowRateMaxAvail at start of each HVAC simulation
        IF (FirstHVACIteration) THEN
          Node(NodeNum)%MassFlowRateMaxAvail = Node(NodeNum)%MassFlowRateMax
          Node(NodeNum)%MassFlowRateMinAvail = Node(NodeNum)%MassFlowRateMin
        END IF
      END DO
    END DO

    ! set the required flow (from zone equipment) at system outlet nodes
    DO OutNum=1,PrimaryAirSystem(AirLoopNum)%NumOutletBranches
      OutBranchNum = PrimaryAirSystem(AirLoopNum)%OutletBranchNum(OutNum)
      NodeNumOut = PrimaryAirSystem(AirLoopNum)%Branch(OutBranchNum)%NodeNumOut
      ZoneSideNodeNum = AirToZoneNodeInfo(AirLoopNum)%ZoneEquipSupplyNodeNum(OutNum)

      IF (.NOT. FirstHVACIteration) THEN
        MassFlowSet = Node(ZoneSideNodeNum)%MassFlowRate
      ELSE ! first time through in each HVAC timestep, use design mass flow rates for required mass flows
        MassFlowSet = PrimaryAirSystem(AirLoopNum)%Branch(OutBranchNum)%MaxMassFlowRate
      END IF
      ! Need to make sure that flows are greater than zero
      IF (MassFlowSet .GE. 0.0d0) THEN
         Node(NodeNumOut)%MassFlowRateSetPoint = MassFlowSet
      ELSE IF (MassFlowSet .LT. 0.0d0) THEN
         Node(NodeNumOut)%MassFlowRateSetPoint = 0.0d0
      END IF

      IF (Node(NodeNumOut)%MassFlowRateSetPoint < MassFlowSetToler) THEN
        Node(NodeNumOut)%MassFlowRateSetPoint = 0.0d0
      END IF

      ! Pass the required mass flow upstream to the start of each outlet branch
      DO BranchNodeIndex=PrimaryAirSystem(AirLoopNum)%Branch(OutBranchNum)%TotalNodes-1,1,-1
        NodeNum = PrimaryAirSystem(AirLoopNum)%Branch(OutBranchNum)%NodeNum(BranchNodeIndex)
        IF (PrimaryAirSystem(AirLoopNum)%OASysExists .AND.  &
              (NodeNum == PrimaryAirSystem(AirLoopNum)%OASysInletNodeNum) ) THEN
              ! need to modify if OA relief and supply not balanced because of exhaust fans
           OAReliefDiff = Node(PrimaryAirSystem(AirLoopNum)%OASysOutletNodeNum)%MassFlowRate - Node(NodeNum)%MassFlowRate
           If (OAReliefDiff > 0.d0) THEN
             Node(NodeNum)%MassFlowRateSetPoint = Node(NodeNumOut)%MassFlowRateSetPoint - OAReliefDiff
           ELSE
             Node(NodeNum)%MassFlowRateSetPoint = Node(NodeNumOut)%MassFlowRateSetPoint
           ENDIF
        ELSE
          Node(NodeNum)%MassFlowRateSetPoint = Node(NodeNumOut)%MassFlowRateSetPoint
        ENDIF
      END DO ! end loop over branch nodes

    END DO  ! end loop over outlet branches

    ! sum and save the total loop return air mass flow rate
    AirLoopFlow(AirLoopNum)%TotReturn = 0.0d0
    DO InBranchNum=1,AirToZoneNodeInfo(AirLoopNum)%NumReturnNodes
      AirLoopFlow(AirLoopNum)%TotReturn = AirLoopFlow(AirLoopNum)%TotReturn + &
        Node(AirToZoneNodeInfo(AirLoopNum)%AirLoopReturnNodeNum(InBranchNum))%MassFlowRate
    END DO

    ! [DC/LBNL] Initialize flag for current air loop
    AirLoopControlInfo(AirLoopNum)%NewFlowRateFlag = .FALSE.

    ! start each HVAC simulation at design air flow rate
    IF (FirstHVACIteration) THEN
      ! At each new HVAC iteration reset air loop converged flag to avoid attempting a warm restart
      ! in SimAirLoop
      AirLoopControlInfo%ConvergedFlag = .FALSE.

      DO InNum=1,PrimaryAirSystem(AirLoopNum)%NumInletBranches
        InBranchNum =  PrimaryAirSystem(AirLoopNum)%InletBranchNum(InNum)
        IF (InBranchNum == 0) THEN
          CALL ShowFatalError('Missing Inlet Branch on Primary Air System='//TRIM(PrimaryAirSystem(AirLoopNum)%Name))
        ENDIF
        NodeNumIn = PrimaryAirSystem(AirLoopNum)%Branch(InBranchNum)%NodeNumIn

        ! [DC/LBNL] Save previous mass flow rate
        MassFlowSaved = Node(NodeNumIn)%MassFlowRate

        Node(NodeNumIn)%MassFlowRate = PrimaryAirSystem(AirLoopNum)%Branch(InBranchNum)%MaxMassFlowRate
        AirLoopFlow(AirLoopNum)%DesSupply = PrimaryAirSystem(AirLoopNum)%Branch(InBranchNum)%MaxMassFlowRate

        ! [DC/LBNL] Detect if air mass flow rate has changed since last air loop simulation
        IF ( Node(NodeNumIn)%MassFlowRate /= MassFlowSaved ) THEN
          AirLoopControlInfo(AirLoopNum)%NewFlowRateFlag = .TRUE.
        END IF

      END DO ! end loop over inlet branches
      AirLoopControlInfo(AirLoopNum)%EconoLockout = .FALSE.
    END IF
    ! if a flow rate is specified for the loop use it here
    IF (AirLoopControlInfo(AirLoopNum)%LoopFlowRateSet .AND. .NOT. FirstHVACIteration) THEN
      DO InNum=1,PrimaryAirSystem(AirLoopNum)%NumInletBranches
        InBranchNum =  PrimaryAirSystem(AirLoopNum)%InletBranchNum(InNum)
        NodeNumIn = PrimaryAirSystem(AirLoopNum)%Branch(InBranchNum)%NodeNumIn
        Node(NodeNumIn)%MassFlowRate = AirLoopFlow(AirLoopNum)%DesSupply * AirLoopFlow(AirLoopNum)%ReqSupplyFrac - &
                                       (AirLoopFlow(AirLoopNum)%ZoneExhaust - AirLoopFlow(AirLoopNum)%ZoneExhaustBalanced)
      END DO
    END IF

  END DO   ! end loop over primary air systems

  RETURN

END SUBROUTINE InitAirLoops

 ! Begin Algorithm Section of the Module
!******************************************************************************

SUBROUTINE SimAirLoops(FirstHVACIteration,SimZoneEquipment)

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
          !       DATE WRITTEN:  Oct 1997
          !           MODIFIED:  Dec 1997 Fred Buhl
          !           MODIFIED:  Apr 1998 Richard Liesen
          !           MODIFIED:  Dec 1999 Fred Buhl
          !           MODIFIED:  Feb 2006 Dimitri Curtil (LBNL)
          !                      - Moved air loop simulation to SimAirLoop() routine.
          !      RE-ENGINEERED:  This is new code, not reengineered

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the driver subroutine for the air loop simulation. It simulates
          ! each primary air system in the problem and passes the outlet node conditions
          ! on to the attached Zone Equipment inlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! For each primary air system:
          ! (1) each component in the system is simulated in natural order, beginning at
          !     the return air inlet and progressing to the supply air outlets. Node data
          !     is passed in the same direction.
          ! (2) The controllers and their actions are simulated.
          ! (3) Steps 2 and 3 are repeated until the control criteria are satisfied.
          ! (4) A mass balance check is performed; if it fails, mass balance is imposed
          !     and steps 1, 2, and 3 are repeated. At the end we should have a correct,
          !     self consistent primary air system simulation.
          !

          ! REFERENCES: None

          ! USE STATEMENTS:
  USE HVACInterfaceManager,   ONLY : UpdateHVACInterface
  USE MixedAir,               ONLY : SimOAController
  USE DataGlobals,            ONLY : BeginTimeStepFlag
  USE General,                ONLY : GetPreviousHVACTime
  USE DataConvergParams,      ONLY : CalledFromAirSystemSupplySideDeck1, CalledFromAirSystemSupplySideDeck2
  IMPLICIT NONE

       ! SUBROUTINE ARGUMENT DEFINITIONS:
  ! TRUE if first full HVAC iteration in an HVAC timestep
  LOGICAL, INTENT(IN)    :: FirstHVACIteration
  ! TRUE if Zone Equipment needs to be resimulated.
  LOGICAL, INTENT(INOUT) :: SimZoneEquipment

       ! SUBROUTINE PARAMETER DEFINITIONS: None

       ! INTERFACE BLOCK DEFINITIONS: None

       ! DERIVED TYPE DEFINITIONS: None

       ! SUBROUTINE LOCAL VARIABLE DEFINITIONS
  ! Last saved HVAC time stamp at beginning of step in seconds.
  ! Used to control when to reset the statistic counters for each new HVAC step.
  REAL(r64), SAVE :: SavedPreviousHVACTime = 0.0d0
  REAL(r64) :: rxTime
  ! Maximum of iteration counters across all air loops
  INTEGER, SAVE          :: IterMax = 0
  ! Aggregated number of iterations across all air loops
  INTEGER, SAVE          :: IterTot = 0
  ! Aggregated number fo times SimAirLoopComponents() has been invoked across all air loops
  INTEGER, SAVE          :: NumCallsTot = 0
  ! Primary Air Sys DO loop index
  INTEGER                :: AirLoopNum
  ! Max number of iterations performed by controllers on each air loop
  INTEGER                :: AirLoopIterMax
  ! Aggregated number of iterations across all controllers on each air loop
  INTEGER                :: AirLoopIterTot
  ! Total number of times SimAirLoopComponents() has been invoked to simulate each air loop
  INTEGER                :: AirLoopNumCalls
  ! Primary air system outlet DO loop index
  INTEGER                :: AirSysOutNum
  ! DO loop index; there are 2 passes - the 2nd is done only if mass balance fails
  INTEGER                :: AirLoopPass
  ! Output variable setup flag
  LOGICAL, SAVE          :: OutputSetupFlag = .FALSE.
  ! Flag set by ResolveSysFlow; if TRUE, mass balance failed and there must be a second pass
  LOGICAL                :: SysReSim
  INTEGER                :: CalledFrom

          ! FLOW:

  ! Set up output variables
  IF (.NOT. OutputSetupFlag) THEN
    CALL SetupOutputVariable('Air System Simulation Maximum Iteration Count []',          IterMax,     'HVAC', 'Sum', 'SimAir')
    CALL SetupOutputVariable('Air System Simulation Iteration Count []',          IterTot,     'HVAC', 'Sum', 'SimAir')
    CALL SetupOutputVariable('Air System Component Model Simulation Calls []', NumCallsTot, 'HVAC', 'Sum', 'SimAir')
    OutputSetupFlag = .TRUE.
  END IF

  ! BUG: IterMax should not be aggregated as a Sum output variable
  !      We need a new aggregation scheme to track the max value across HVAC steps
  !      instead of suming it up.
  IterMax = 0

  ! Reset counters to capture statistics for the current zone time step
  !
  ! Aggregate statistics over all HVAC time steps, even the rejected ones, to properly
  ! reflect the numerical work. The condition to detect a new HVAC time step is essentially
  ! based on the time stamp at the beginning of the current HVAC step (expressed in seconds).
  IF ( FirstHVACIteration) THEN
    rxTime=GetPreviousHVACTime()
    IF (SavedPreviousHVACTime /= rxTime) THEN
      SavedPreviousHVACTime = rxTime
      IterTot     = 0
      NumCallsTot = 0
    END IF
  END IF

  ! Loop over all the primary air loop; simulate their components (equipment)
  ! and controllers
  DO AirLoopNum = 1, NumPrimaryAirSys   ! NumPrimaryAirSys is the number of primary air loops

    ! Check to see if System Availability Managers are asking for fans to cycle on or shut off
    ! and set fan on/off flags accordingly.
    TurnFansOn  = .FALSE.
    TurnFansOff = .FALSE.
    NightVentOn = .FALSE.
    IF (PriAirSysAvailMgr(AirLoopNum)%AvailStatus .EQ. CycleOn) THEN
      TurnFansOn = .TRUE.
    END IF
    IF (PriAirSysAvailMgr(AirLoopNum)%AvailStatus .EQ. ForceOff) THEN
      TurnFansOff = .TRUE.
    END IF
    IF (AirLoopControlInfo(AirLoopNum)%NightVent) THEN
      NightVentOn = .TRUE.
    END IF

!   Set current system number for sizing routines
    CurSysNum = AirLoopNum

! RR why is this called here, it's called first in SimAirLoop. Causes no diff's to comment out.
!    IF (AirLoopControlInfo(AirLoopNum)%OACtrlNum > 0) THEN
!      CALL SimOAController( &
!        AirLoopControlInfo(AirLoopNum)%OACtrlName, &
!        AirLoopControlInfo(AirLoopNum)%OACtrlNum,  &
!        FirstHVACIteration, &
!        AirLoopNum )
!    END IF

    ! 2 passes; 1 usually suffices; 2 is done if ResolveSysFlow detects a failure of mass balance
    SimPasses : DO AirLoopPass=1,2

      SysReSim = .FALSE.

      ! Simulate controllers on air loop with current air mass flow rates
      CALL SimAirLoop( &
        FirstHVACIteration, AirLoopNum, AirLoopPass, &
        AirLoopIterMax, AirLoopIterTot, AirLoopNumCalls &
      )

      ! Update tracker for maximum number of iterations needed by any controller on all air loops
      IterMax = MAX(IterMax, AirLoopIterMax)
      ! Update tracker for aggregated number of iterations needed by all controllers on all air loops
      IterTot = IterTot + AirLoopIterTot
      ! Update tracker for total number of times SimAirLoopComponents() has been invoked across all air loops
      NumCallsTot = NumCallsTot + AirLoopNumCalls

      ! At the end of the first pass, check whether a second pass is needed or not
      IF (AirLoopPass.EQ.1) THEN
        ! If simple system, skip second pass
        IF (AirLoopControlInfo(AirLoopNum)%Simple) EXIT SimPasses
        CALL ResolveSysFlow(AirLoopNum, SysReSim)
        ! If mass balance OK, skip second pass
        IF (.NOT.SysReSim) EXIT SimPasses
      END IF

    END DO SimPasses ! end pass loop

    ! Air system side has been simulated, now transfer conditions across to
    ! the zone equipment side, looping through all supply air paths for this
    ! air loop.
    DO AirSysOutNum = 1, AirToZoneNodeInfo(AirLoopNum)%NumSupplyNodes
      IF (AirSysOutNum == 1) CalledFrom = CalledFromAirSystemSupplySideDeck1
      IF (AirSysOutNum == 2) CalledFrom = CalledFromAirSystemSupplySideDeck2
      CALL UpdateHVACInterface( AirLoopNum, CalledFrom, &
        AirToZoneNodeInfo(AirLoopNum)%AirLoopSupplyNodeNum(AirSysOutNum),   &
        AirToZoneNodeInfo(AirLoopNum)%ZoneEquipSupplyNodeNum(AirSysOutNum), &
        SimZoneEquipment )
    END DO  ! ...end of DO loop over supply air paths for this air loop.

  END DO ! End of Air Loop iteration

! Reset current system number for sizing routines
  CurSysNum = 0

  RETURN
END SUBROUTINE SimAirLoops

SUBROUTINE SimAirLoop(FirstHVACIteration, AirLoopNum, AirLoopPass, &
                      AirLoopIterMax, AirLoopIterTot, AirLoopNumCalls )

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Dimitri Curtil (LBNL)
          !       DATE WRITTEN:  March 2006
          !                      - Fine-tuned outer loop over controllers.
          !                      - Added convergence tracing for air loop controllers.
          !                      - Added mechanism for speculative warm restart after first iteration.
          !      RE-ENGINEERED:  This is new code based on the code that used to be part
          !                      of SimAirLoops().

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the desired air loop by solving for all the
          ! controllers on the air loop in the order they are specified.

          ! METHODOLOGY EMPLOYED:
          ! To speed up the simulation, we introduced the possiblity to perform the controller
          ! simulation on each air loop using a warm restart from the solution obtained
          ! at the previous HVAC step iteration. This is only attempted if the air mass flow
          ! rate(s) for the air system have not changed since the last iteration.
          ! Of course if the warm restart fails, then we perform a normal simulation from
          ! a cold start. We refer to this scheme as speculative warm restart.
          !

          ! REFERENCES: None

          ! USE STATEMENTS:
  USE DataHVACControllers
  USE DataSystemVariables
  USE HVACControllers,        ONLY : TrackAirLoopControllers, TraceAirLoopControllers
  USE General,                ONLY : CreateSysTimeIntervalString

  IMPLICIT NONE

       ! SUBROUTINE ARGUMENT DEFINITIONS:
  ! TRUE if first full HVAC iteration in an HVAC timestep
  LOGICAL, INTENT(IN)          :: FirstHVACIteration
  ! Index of the air loop to simulate
  INTEGER, INTENT(IN)          :: AirLoopNum
  ! There are 2 passes - the 2nd is done only if mass balance fails
  INTEGER, INTENT(IN)          :: AirLoopPass
  ! Max number of iterations performed by controllers across this air loop
  INTEGER, INTENT(OUT)         :: AirLoopIterMax
  ! Aggregated number of iterations across all controllers on this air loop
  INTEGER, INTENT(OUT)         :: AirLoopIterTot
  ! Total number of times SimAirLoopComponents() has been invoked to simulate this air loop
  INTEGER, INTENT(OUT)         :: AirLoopNumCalls

       ! SUBROUTINE PARAMETER DEFINITIONS: None

       ! INTERFACE BLOCK DEFINITIONS: None

       ! DERIVED TYPE DEFINITIONS: None

       ! SUBROUTINE LOCAL VARIABLE DEFINITIONS
  ! Maximum number of iterations performed by each controller on this air loop
  INTEGER                :: IterMax = 0
  ! Aggregated number of iterations performed by each controller on this air loop
  INTEGER                :: IterTot = 0
  ! Number of times SimAirLoopComponents() has been invoked per air loop for either Solve or ReSolve operations
  INTEGER                :: NumCalls = 0
  ! TRUE when primary air system & controllers simulation has converged;
  LOGICAL                :: AirLoopConvergedFlag = .FALSE.
  ! TRUE when speculative warm restart is allowed; FALSE otherwise.
  LOGICAL                :: DoWarmRestartFlag = .FALSE.
  ! If Status<0, no speculative warm restart attempted.
  ! If Status==0, warm restart failed.
  ! If Status>0, warm restart succeeded.
  INTEGER                :: WarmRestartStatus = iControllerWarmRestartNone

          ! FLOW:

  ! Reset air loop trackers to zero
  AirLoopIterMax  = 0
  AirLoopIterTot  = 0
  AirLoopNumCalls = 0

  ! Perform air loop simulation to satisfy convergence for all controllers
  ! If first HVAC iteration or new air flow we force a cold restart.
  ! Otherwise we attempt a speculative warm restart.
  !
  ! TODO: Improve detection of when air flow rate has changed since last air loop simulation
  ! TODO: Detect whether warm restart is supported on air loop on very first air loop
  !       simulation only instead of at each HVAC iteration as done now.
  DoWarmRestartFlag = &
      ! Only enabled if there are controllers on the air loop
      PrimaryAirSystem(AirLoopNum)%NumControllers > 0 .AND.      &
      ! Check that the speculative warm restart feature is allowed
      AirLoopControlInfo(AirLoopNum)%AllowWarmRestartFlag .AND.  &
      ! Never done at first HVAC iteration
      .NOT. FirstHVACIteration .AND.                             &
      ! Never done during sizing
      .NOT. SysSizingCalc .AND.                                  &
      ! Next condition is true whenever the final check for the air loop was converged
      ! at the previous SimAirLoop call
      AirLoopControlInfo(AirLoopNum)%ConvergedFlag .AND.         &
      ! Next conditions should detect when air mass flow rates have changed
      .NOT. AirLoopControlInfo(AirLoopNum)%LoopFlowRateSet .AND. &
      .NOT. AirLoopControlInfo(AirLoopNum)%NewFlowRateFlag

  IF ( .NOT.DoWarmRestartFlag ) THEN
    ! Solve controllers with cold start using default initial values
    CALL SolveAirLoopControllers( &
      FirstHVACIteration, AirLoopPass, AirLoopNum, AirLoopConvergedFlag, &
      IterMax, IterTot, NumCalls )

    ! Update air loop trackers
    WarmRestartStatus = iControllerWarmRestartNone
    AirLoopNumCalls = AirLoopNumCalls + NumCalls
    AirLoopIterMax  = MAX(AirLoopIterMax, IterMax)
    AirLoopIterTot  = AirLoopIterTot + IterTot
  ELSE
    ! First try with speculative warm restart using previous solution
    CALL ReSolveAirLoopControllers( &
      FirstHVACIteration, AirLoopPass, AirLoopNum, AirLoopConvergedFlag, &
      IterMax, IterTot, NumCalls )

    ! Update air loop trackers
    WarmRestartStatus = iControllerWarmRestartSuccess
    AirLoopNumCalls = AirLoopNumCalls + NumCalls
    AirLoopIterMax  = MAX(AirLoopIterMax, IterMax)
    AirLoopIterTot  = AirLoopIterTot + IterTot

    ! Retry with cold start using default initial values if speculative warm restart did not work
    IF ( .NOT.AirLoopConvergedFlag ) THEN
      CALL SolveAirLoopControllers( &
        FirstHVACIteration, AirLoopPass, AirLoopNum, AirLoopConvergedFlag, &
        IterMax, IterTot, NumCalls )

      ! Update air loop trackers
      WarmRestartStatus = iControllerWarmRestartFail
      AirLoopNumCalls = AirLoopNumCalls + NumCalls
      AirLoopIterMax  = MAX(AirLoopIterMax, IterMax)
      AirLoopIterTot  = AirLoopIterTot + IterTot
    END IF
  END IF

  ! Updates air loop statistics
  !
  ! To enable runtime statistics tracking for each air loop, define the environment variable
  ! TRACK_AIRLOOP=YES or TRACK_AIRLOOP=Y
  IF ( TrackAirLoopEnvFlag ) THEN
    CALL TrackAirLoopControllers( &
      AirLoopNum, &
      WarmRestartStatus, &
      AirLoopIterMax, &
      AirLoopIterTot, &
      AirLoopNumCalls &
    )
  END IF

  ! Generate trace for all controllers on this air loop
  !
  ! To enable generating a trace file with the converged solution for all controllers on each air loop,
  ! define the environment variable TRACE_AIRLOOP=YES or TRACE_AIRLOOP=Y.
  IF ( TraceAirLoopEnvFlag ) THEN
    CALL TraceAirLoopControllers( &
      FirstHVACIteration, &
      AirLoopNum, AirLoopPass, AirLoopConvergedFlag, &
      AirLoopNumCalls &
    )
  END IF

  ! When there is more than 1 controller on an air loop, each controller sensing
  ! different nodes with potentially different setpoints, it is likely that
  ! AirLoopConvergedFlag will be false as the individual setpoints will not
  ! be satisfied once all the controllers have been simulated. Typically, this could
  ! happen if
  ! If this is the case then we do not want to try a warm restart as it is very
  ! unlikely to succeed.
  AirLoopControlInfo(AirLoopNum)%ConvergedFlag = AirLoopConvergedFlag

  RETURN
END SUBROUTINE SimAirLoop

SUBROUTINE SolveAirLoopControllers(FirstHVACIteration, AirLoopPass, AirLoopNum, AirLoopConvergedFlag, &
                                   IterMax, IterTot, NumCalls)

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Dimitri Curtil (LBNL)
          !       DATE WRITTEN:  Feb 2006
          !           MODIFIED:
          !      RE-ENGINEERED:  This is reengineered code that used to be in SimAirLoops()

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves for the controllers on the specfied air loop assuming a cold start.

          ! METHODOLOGY EMPLOYED:
          ! For the specified primary air system:
          ! (1) each component in the system is simulated in natural order, beginning at
          !     the return air inlet and progressing to the supply air outlets. Node data
          !     is passed in the same direction.
          ! (2) The controllers and their actions are simulated.
          ! (3) Steps 2 and 3 are repeated until the control criteria are satisfied.

          ! REFERENCES: None

          ! USE STATEMENTS:
  USE DataHVACControllers
  USE HVACControllers,        ONLY : ManageControllers
  USE General,                ONLY : CreateSysTimeIntervalString

  IMPLICIT NONE

       ! SUBROUTINE ARGUMENT DEFINITIONS:
  ! TRUE if first full HVAC iteration in an HVAC timestep
  LOGICAL, INTENT(IN)          :: FirstHVACIteration
  ! DO loop index; there are 2 passes the 2nd is done only if mass balance fails
  INTEGER, INTENT(IN)          :: AirLoopPass
  ! Index of the air loop being simulated
  INTEGER, INTENT(IN)          :: AirLoopNum
  ! TRUE when primary air system & controllers simulation has converged;
  LOGICAL, INTENT(OUT)         :: AirLoopConvergedFlag
  ! Max number of iterations performed by controllers across this air loop
  INTEGER, INTENT(OUT)         :: IterMax
  ! Aggregated number of iterations across all controllers on this air loop
  INTEGER, INTENT(OUT)         :: IterTot
  ! Total number of times SimAirLoopComponents() has been invoked
  INTEGER, INTENT(OUT)         :: NumCalls

       ! SUBROUTINE PARAMETER DEFINITIONS:
  ! Maximum iterations of an air system/controllers simulation sequence
  INTEGER, PARAMETER           :: MaxIter = 50

           ! INTERFACE BLOCK DEFINITIONS: None

           ! DERIVED TYPE DEFINITIONS: None

           ! SUBROUTINE LOCAL VARIABLE DEFINITIONS
  ! TRUE if controller supports speculative warm restart
  LOGICAL                      :: AllowWarmRestartFlag
  ! TRUE when controller has converged
  LOGICAL                      :: ControllerConvergedFlag
  ! TRUE when air loop has been evaluated with latest actuated variables
  LOGICAL                      :: IsUpToDateFlag
  ! Iteration counter
  INTEGER                      :: Iter = 0
  ! Controller DO loop index
  INTEGER                      :: AirLoopControlNum
  ! Number of times that the maximum iterations was exceeded
  INTEGER, SAVE                :: ErrCount = 0
  ! Number of times that the maximum iterations was exceeded
  INTEGER, SAVE                :: MaxErrCount = 0
  ! Placeholder for environment name used in error reporting
  CHARACTER(LEN=MaxNameLength*2),SAVE :: ErrEnvironmentName=' '
  ! A character string equivalent of ErrCount
  CHARACTER(LEN=20)            :: CharErrOut

          ! FLOW:

  ! To track number of calls to SimAirLoopComponents() for each air loop
  ! Represents the most computationally expensive operation in the iteration.
  ! Best metric to use to assess the runtime performance of air loop simulation
  NumCalls = 0
  IterMax  = 0
  IterTot  = 0

  AirLoopConvergedFlag = .TRUE.
  IsUpToDateFlag       = .FALSE.
  PrimaryAirSystem(AirLoopNum)%ControlConverged = .FALSE.

  AllowWarmRestartFlag = .TRUE.
  AirLoopControlInfo(AirLoopNum)%AllowWarmRestartFlag = .TRUE.

  ! When using controllers, size air loop coils so ControllerProps (e.g., Min/Max Actuated) can be set
  IF(PrimaryAirSystem(AirLoopNum)%SizeAirloopCoil)THEN
    IF(PrimaryAirSystem(AirLoopNum)%NumControllers>0) &
       CALL SimAirLoopComponents( AirLoopNum, FirstHVACIteration )
    PrimaryAirSystem(AirLoopNum)%SizeAirloopCoil = .FALSE.
  END IF

  ! This call to ManageControllers reinitializes the controllers actuated variables to zero
  ! E.g., actuator inlet water flow
  DO AirLoopControlNum = 1,PrimaryAirSystem(AirLoopNum)%NumControllers

    CALL ManageControllers( &
      PrimaryAirSystem(AirLoopNum)%ControllerName(AirLoopControlNum), &
      PrimaryAirSystem(AirLoopNum)%ControllerIndex(AirLoopControlNum), &
      FirstHVACIteration, AirLoopNum, AirLoopPass, &
      iControllerOpColdStart, ControllerConvergedFlag, IsUpToDateFlag, AllowWarmRestartFlag )

    ! Detect whether the speculative warm restart feature is supported by each controller
    ! on this air loop.
    AirLoopControlInfo(AirLoopNum)%AllowWarmRestartFlag = &
      AirLoopControlInfo(AirLoopNum)%AllowWarmRestartFlag .AND. AllowWarmRestartFlag
  END DO

  ! Evaluate air loop components with new actuated variables
  NumCalls = NumCalls + 1
  CALL SimAirLoopComponents( AirLoopNum, FirstHVACIteration )
  IsUpToDateFlag = .TRUE.

  ! Loop over the air sys controllers until convergence or MaxIter iterations
  DO AirLoopControlNum = 1,PrimaryAirSystem(AirLoopNum)%NumControllers

    Iter = 0
    ControllerConvergedFlag = .FALSE.
    ! if the controller can be locked out by the economizer operation and the economizer is active, leave the controller inactive
    IF (AirLoopControlInfo(AirLoopNum)%EconoActive .and. &
        PrimaryAirSystem(AirLoopNum)%CanBeLockedOutByEcono(AirLoopControlNum)) THEN
      ControllerConvergedFlag = .TRUE.
      CYCLE
    END IF

    ! For each controller in sequence, iterate until convergence
    DO WHILE (.NOT. ControllerConvergedFlag)

      Iter = Iter + 1

      CALL ManageControllers( &
        PrimaryAirSystem(AirLoopNum)%ControllerName(AirLoopControlNum), &
        PrimaryAirSystem(AirLoopNum)%ControllerIndex(AirLoopControlNum), &
        FirstHVACIteration, AirLoopNum, AirLoopPass, &
        iControllerOpIterate, ControllerConvergedFlag, IsUpToDateFlag )

      PrimaryAirSystem(AirLoopNum)%ControlConverged(AirLoopControlNum) = ControllerConvergedFlag

      IF ( .NOT.ControllerConvergedFlag ) THEN
        ! Only check abnormal termination if not yet converged
        ! The iteration counter has been exceeded.
        IF (Iter > MaxIter) THEN
          ! Indicate that this air loop is not converged
          AirLoopConvergedFlag = .FALSE.

          ! The warning message will be suppressed during the warm up days.
          IF ( .NOT.WarmUpFlag ) THEN
            ErrCount = ErrCount + 1
            IF (ErrCount < 15) THEN
              ErrEnvironmentName = EnvironmentName
              WRITE(CharErrOut,*) MaxIter
              CharErrOut=ADJUSTL(CharErrOut)
              CALL ShowWarningError ( &
                'SolveAirLoopControllers: Maximum iterations ('//TRIM(CharErrOut)//') exceeded for '//  &
                TRIM(PrimaryAirSystem(AirLoopNum)%Name)//', at '//  &
                TRIM(EnvironmentName)//', '//TRIM(CurMnDy)//' '// &
                TRIM(CreateSysTimeIntervalString()) &
              )
            ELSE
              IF (EnvironmentName /= ErrEnvironmentName) THEN
                MaxErrCount = 0
                ErrEnvironmentName = EnvironmentName
              END IF
              CALL ShowRecurringWarningErrorAtEnd( &
                'SolveAirLoopControllers: Exceeding Maximum iterations for '//  &
                TRIM(PrimaryAirSystem(AirLoopNum)%Name)//' during '//  &
                TRIM(EnvironmentName)//' continues', MaxErrCount &
              )
            END IF
          END IF

          ! It is necessary to execute this statement anytime, even if the warning message is suppressed.
          ! To continue the simulation it must be able to goto the Exit statement
          EXIT  ! It will not converge this time
        END IF

        ! Re-evaluate air loop components with new actuated variables
        NumCalls = NumCalls + 1
        CALL SimAirLoopComponents( AirLoopNum, FirstHVACIteration )
        IsUpToDateFlag = .TRUE.

      END IF

    END DO  ! End of the Convergence Iteration

    ! Update tracker for max iteration counter across all controllers on this air loops
    IterMax = MAX(IterMax, Iter)
    ! Update tracker for aggregated counter of air loop inner iterations across controllers
    ! on this air loop
    IterTot = IterTot + Iter

  END DO ! End of controller loop

  ! Once the controllers are converged then need to simulate the components once
  ! more to ensure that they are simulated with the latest values.
  IF ( .NOT.IsUpToDateFlag .OR. .NOT.AirLoopConvergedFlag ) THEN
    NumCalls = NumCalls + 1
    CALL SimAirLoopComponents( AirLoopNum, FirstHVACIteration )
    IsUpToDateFlag = .TRUE.
  END IF

  ! Check that all active controllers are still convergence
  DO AirLoopControlNum = 1,PrimaryAirSystem(AirLoopNum)%NumControllers

    ControllerConvergedFlag = .FALSE.

    CALL ManageControllers( &
      PrimaryAirSystem(AirLoopNum)%ControllerName(AirLoopControlNum), &
      PrimaryAirSystem(AirLoopNum)%ControllerIndex(AirLoopControlNum), &
      FirstHVACIteration, AirLoopNum, AirLoopPass, &
      iControllerOpEnd, ControllerConvergedFlag, IsUpToDateFlag )

    PrimaryAirSystem(AirLoopNum)%ControlConverged(AirLoopControlNum) = ControllerConvergedFlag

    AirLoopConvergedFlag = AirLoopConvergedFlag .AND. ControllerConvergedFlag

  END DO

  RETURN

END SUBROUTINE SolveAirLoopControllers

SUBROUTINE ReSolveAirLoopControllers(FirstHVACIteration, AirLoopPass, AirLoopNum, AirLoopConvergedFlag, &
                                     IterMax, IterTot, NumCalls)

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Dimitri Curtil (LBNL)
          !       DATE WRITTEN:  Feb 2006
          !           MODIFIED:
          !      RE-ENGINEERED:  This is new code

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine solves for the controllers on the specfied air loop by reusing
          ! the solution from the previous HVAC iteration.
          ! It is used in the context of the optimization technique referred to as
          ! speculative warm restart.

          ! METHODOLOGY EMPLOYED:
          ! For the specified primary air system:
          ! (1) each component in the system is simulated in natural order, beginning at
          !     the return air inlet and progressing to the supply air outlets. Node data
          !     is passed in the same direction.
          ! (2) The controllers and their actions are simulated.

          ! REFERENCES: None

          ! USE STATEMENTS:
  USE DataHVACControllers
  USE HVACControllers,        ONLY : ManageControllers

  IMPLICIT NONE

       ! SUBROUTINE ARGUMENT DEFINITIONS:
  ! TRUE if first full HVAC iteration in an HVAC timestep
  LOGICAL, INTENT(IN)          :: FirstHVACIteration
  ! DO loop index; there are 2 passes the 2nd is done only if mass balance fails
  INTEGER, INTENT(IN)          :: AirLoopPass
  INTEGER, INTENT(IN)          :: AirLoopNum
  ! TRUE when primary air system & controllers simulation has converged;
  LOGICAL, INTENT(OUT)         :: AirLoopConvergedFlag
  ! Max number of iterations performed by controllers across all air loops
  INTEGER, INTENT(OUT)         :: IterMax
  ! Aggregated number of iterations across all air loops
  INTEGER, INTENT(OUT)         :: IterTot
  ! Total number of times SimAirLoopComponents() has been invoked
  INTEGER, INTENT(OUT)         :: NumCalls

       ! SUBROUTINE PARAMETER DEFINITIONS: None

       ! INTERFACE BLOCK DEFINITIONS: None

       ! DERIVED TYPE DEFINITIONS: None

       ! SUBROUTINE LOCAL VARIABLE DEFINITIONS
  ! Controller DO loop index
  INTEGER                      :: AirLoopControlNum
  ! TRUE when controller has converged
  LOGICAL                      :: ControllerConvergedFlag
  ! TRUE when air loop needs to be refreshed.
  ! Note that it is not used by ManageControllers() in the WARM_RESTART mode.
  LOGICAL                      :: IsUpToDateFlag

       ! FLOW:

  ! To track number of calls to SimAirLoopComponents() for each air loop
  ! Represents the most computationally expensive operation in the iteration.
  ! Best metric to use to assess the runtime performance of air loop simulation
  NumCalls = 0
  IterMax  = 0
  IterTot  = 0

  AirLoopConvergedFlag = .TRUE.
  IsUpToDateFlag       = .FALSE.
  PrimaryAirSystem(AirLoopNum)%ControlConverged = .FALSE.

  ! This call to ManageControllers reinitializes the controllers actuated variables to zero
  ! E.g., actuator inlet water flow
  DO AirLoopControlNum = 1,PrimaryAirSystem(AirLoopNum)%NumControllers

    CALL ManageControllers( &
      PrimaryAirSystem(AirLoopNum)%ControllerName(AirLoopControlNum), &
      PrimaryAirSystem(AirLoopNum)%ControllerIndex(AirLoopControlNum), &
      FirstHVACIteration, AirLoopNum, AirLoopPass, &
      iControllerOpWarmRestart, ControllerConvergedFlag, IsUpToDateFlag )

  END DO

  ! Evaluate air loop components with new actuated variables
  NumCalls = NumCalls + 1
  CALL SimAirLoopComponents( AirLoopNum, FirstHVACIteration )
  IsUpToDateFlag = .TRUE.

  ! Check that all active controllers are still convergence
  ! Check that actuated variables are within min/max constraints
  DO AirLoopControlNum = 1,PrimaryAirSystem(AirLoopNum)%NumControllers

    ControllerConvergedFlag = .FALSE.

    CALL ManageControllers( &
      PrimaryAirSystem(AirLoopNum)%ControllerName(AirLoopControlNum), &
      PrimaryAirSystem(AirLoopNum)%ControllerIndex(AirLoopControlNum), &
      FirstHVACIteration, AirLoopNum, AirLoopPass, &
      iControllerOpEnd, ControllerConvergedFlag, IsUpToDateFlag )

    PrimaryAirSystem(AirLoopNum)%ControlConverged(AirLoopControlNum) = ControllerConvergedFlag

    AirLoopConvergedFlag = AirLoopConvergedFlag .AND. ControllerConvergedFlag

    ! Update tracker for max iteration counter across all controllers on all air loops
    IterMax = MAX(IterMax,0)
    ! Update tracker for aggregated counter of air loop inner iterations across all controllers
    IterTot = IterTot + 0

  END DO ! end of controller loop

  RETURN

END SUBROUTINE ReSolveAirLoopControllers

SUBROUTINE SimAirLoopComponents( AirLoopNum, FirstHVACIteration )
          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Dimitri Curtil (LBNL)
          !       DATE WRITTEN:  Feb 2006
          !           MODIFIED:
          !      RE-ENGINEERED:

          ! PURPOSE OF THIS SUBROUTINE:
          ! This simulates all components on a particular air loop in the primary air system.
          ! This code used to appear in different places in SimAirLoops(). Now consolidated
          ! into one subroutine called many times.

          ! METHODOLOGY EMPLOYED:
          ! For each branch in the air loop:
          ! (1) update branch connection with (BeforeBranchSim)
          ! (2) simulate each component
          ! (3) update branch connection with (AfterBranchSim) to enforce continuity through splitter
          !
          ! Sets current branch number to CurBranchNum defined in MODULE DataSizing
          ! Sets duct type of current branch to CurDuctType defined in MODULE DataSizing
          ! Upon exiting, resets both counters to 0.

          ! REFERENCES: None

          ! USE STATEMENTS: None

  IMPLICIT NONE

       ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: AirLoopNum         ! Index of the air loop being currently simulated
  LOGICAL, INTENT(IN)     :: FirstHVACIteration ! TRUE if first full HVAC iteration in an HVAC timestep

       ! SUBROUTINE PARAMETER DEFINITIONS: None

       ! INTERFACE BLOCK DEFINITIONS: None

       ! DERIVED TYPE DEFINITIONS: None

       ! SUBROUTINE LOCAL VARIABLE DEFINITIONS: None
  INTEGER                       :: BranchNum    ! Branch DO loop index
  INTEGER                       :: CompNum      ! Component DO loop index
  CHARACTER(LEN=MaxNameLength)  :: CompType     ! Component type
  CHARACTER(LEN=MaxNameLength)  :: CompName     ! Component name
  INTEGER                       :: CompType_Num ! Numeric equivalent for CompType

  DO BranchNum=1,PrimaryAirSystem(AirLoopNum)%NumBranches  ! loop over all branches in air system

    CALL UpdateBranchConnections(AirLoopNum,BranchNum,BeforeBranchSim)

    CurBranchNum = BranchNum
    CurDuctType  = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%DuctType

    ! Loop over components in branch
    DO CompNum = 1,PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
      CompType     = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
      CompName     = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name
      CompType_Num = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num

      ! Simulate each component on PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Name
      CALL SimAirLoopComponent(CompName, CompType_Num,    &
                               FirstHVACIteration, AirLoopNum,  &
                               PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%CompIndex )
    END DO ! End of component loop

    ! Enforce continuity through the splitter
    CALL UpdateBranchConnections(AirLoopNum,BranchNum,AfterBranchSim)

  END DO ! End of branch loop

  CurBranchNum = 0
  CurDuctType  = 0

  RETURN
END SUBROUTINE SimAirLoopComponents

SUBROUTINE SimAirLoopComponent(CompName,CompType_Num,FirstHVACIteration,AirLoopNum,CompIndex)

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
          !       DATE WRITTEN:  Oct 1997
          !           MODIFIED:  Dec 1997 Fred Buhl, Richard Raustad,FSEC Sept 2003
          !      RE-ENGINEERED:  This is new code, not reengineered

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calls the individual air loop component simulation routines

          ! METHODOLOGY EMPLOYED: None

          ! REFERENCES: None

          ! USE Statements
  USE Fans,                      ONLY:SimulateFanComponents
  USE MixedAir,                  ONLY:ManageOutsideAirSystem
  USE Furnaces,                  ONLY:SimFurnace
  USE HVACDuct,                  ONLY:SimDuct
  USE SteamCoils,                ONLY:SimulateSteamCoilComponents
  USE WaterCoils,                ONLY:SimulateWaterCoilComponents
  USE Humidifiers,               ONLY:SimHumidifier
  USE HeatingCoils,              ONLY:SimulateHeatingCoilComponents
  USE HeatRecovery,              ONLY:SimHeatRecovery
  USE HVACDXSystem,              ONLY:SimDXCoolingSystem
  USE HVACUnitarySystem,         ONLY:SimUnitarySystem
  USE HVACDXHeatPumpSystem,      ONLY:SimDXHeatPumpSystem
  USE EvaporativeCoolers,        ONLY:SimEvapCooler
  USE HVACUnitaryBypassVAV,      ONLY:SimUnitaryBypassVAV
  USE DesiccantDehumidifiers,    ONLY:SimDesiccantDehumidifier
  USE HVACHXAssistedCoolingCoil, ONLY:SimHXAssistedCoolingCoil
  USE HVACMultiSpeedHeatPump,    ONLY:SimMSHeatPump
  USE UserDefinedComponents,     ONLY:SimCoilUserDefined

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:

CHARACTER(len=*), INTENT (IN) :: CompName ! the component Name
INTEGER, INTENT (IN)          :: CompType_Num ! numeric equivalent for component type
LOGICAL, INTENT (IN)          :: FirstHVACIteration ! TRUE if first full HVAC iteration in an HVAC timestep
INTEGER, INTENT (IN)          :: AirLoopNum ! Primary air loop number
INTEGER, INTENT (INOUT)       :: CompIndex  ! numeric pointer for CompType/CompName -- passed back from other routines

           ! SUBROUTINE PARAMETER DEFINITIONS: None

           ! INTERFACE BLOCK DEFINITIONS: None

           ! DERIVED TYPE DEFINITIONS: None

           ! SUBROUTINE LOCAL VARIABLE DEFINITIONS:
REAL(r64) :: QActual
LOGICAL   :: CoolingActive
LOGICAL   :: HeatingActive

           ! FLOW:

CoolingActive = .FALSE.
HeatingActive = .FALSE.

SELECT CASE(CompType_Num)

  CASE(OAMixer_Num)  ! 'OUTSIDE AIR SYSTEM'
    CALL ManageOutsideAirSystem(CompName,FirstHVACIteration,AirLoopNum,CompIndex)

! Fan Types for the air sys simulation
  CASE(Fan_Simple_CV)  ! 'Fan:ConstantVolume'
    CALL SimulateFanComponents(CompName,FirstHVACIteration,CompIndex)

  CASE(Fan_Simple_VAV)  ! 'Fan:VariableVolume'
    CALL SimulateFanComponents(CompName,FirstHVACIteration,CompIndex)

  ! cpw22Aug2010 Add Fan:ComponentModel (new)
  CASE(Fan_ComponentModel)  ! 'Fan:ComponentModel'
    CALL SimulateFanComponents(CompName,FirstHVACIteration,CompIndex)

! Coil Types for the air sys simulation
!  Currently no control for HX Assisted coils
!  CASE(DXCoil_CoolingHXAsst)  ! 'CoilSystem:Cooling:DX:HeatExchangerAssisted'
!    CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,CoilOn,0.0,CompIndex,ContFanCycCoil)
  CASE(WaterCoil_CoolingHXAsst) ! 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
    CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,CoilOn,constant_zero,CompIndex,ContFanCycCoil, QTotOut=QActual)
    IF(QActual .GT. 0.0D0) CoolingActive = .TRUE. ! determine if coil is ON

  CASE(WaterCoil_SimpleHeat)  ! 'Coil:Heating:Water'
    CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompIndex,QActual=QActual)
    IF(QActual .GT. 0.0D0) HeatingActive = .TRUE. ! determine if coil is ON

  CASE(SteamCoil_AirHeat)  ! 'Coil:Heating:Steam'
    CALL SimulateSteamCoilComponents(CompName=CompName,       &
                        FirstHVACIteration=FirstHVACIteration, &
                        QCoilReq=constant_zero,CompIndex=CompIndex,QCoilActual=QActual)
    IF(QActual .GT. 0.0D0) HeatingActive = .TRUE. ! determine if coil is ON

  CASE(WaterCoil_DetailedCool)  ! 'Coil:Cooling:Water:DetailedGeometry'
    CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompIndex,QActual=QActual)
    IF(QActual .GT. 0.0D0) CoolingActive = .TRUE. ! determine if coil is ON

  CASE(WaterCoil_Cooling)  ! 'Coil:Cooling:Water'
    CALL SimulateWaterCoilComponents(CompName,FirstHVACIteration,CompIndex,QActual=QActual)
    IF(QActual .GT. 0.0D0) CoolingActive = .TRUE. ! determine if coil is ON

! stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
  CASE(Coil_ElectricHeat)  ! 'Coil:Heating:Electric'
    CALL SimulateHeatingCoilComponents(CompName=CompName,      &
                        FirstHVACIteration=FirstHVACIteration, &
                        CompIndex=CompIndex,QCoilActual=QActual)
    IF(QActual .GT. 0.0D0) HeatingActive = .TRUE. ! determine if coil is ON

! stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
  CASE(Coil_GasHeat)  ! 'Coil:Heating:Gas'
    CALL SimulateHeatingCoilComponents(CompName=CompName,      &
                        FirstHVACIteration=FirstHVACIteration, &
                        CompIndex=CompIndex,QCoilActual=QActual)
    IF(QActual .GT. 0.0D0) HeatingActive = .TRUE. ! determine if coil is ON
! stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
  CASE(Coil_DeSuperHeat)  ! 'Coil:Heating:Desuperheater' - heat reclaim
    CALL SimulateHeatingCoilComponents(CompName=CompName,      &
                        FirstHVACIteration=FirstHVACIteration, &
                        CompIndex=CompIndex,QCoilActual=QActual)
    IF(QActual .GT. 0.0D0) HeatingActive = .TRUE. ! determine if coil is ON

  CASE(DXSystem)  ! CoilSystem:Cooling:DX  old 'AirLoopHVAC:UnitaryCoolOnly'
    CALL SimDXCoolingSystem(CompName, FirstHVACIteration, AirLoopNum, CompIndex, QTotOut=QActual)
    IF(QActual .GT. 0.0D0) CoolingActive = .TRUE. ! determine if coil is ON

  CASE(DXHeatPumpSystem) ! 'CoilSystem:Heating:DX'
    CALL SimDXHeatPumpSystem(CompName, FirstHVACIteration, AirLoopNum, CompIndex, QTotOut=QActual)
    IF(QActual .GT. 0.0D0) HeatingActive = .TRUE. ! determine if coil is ON

  CASE(CoilUserDefined) ! Coil:UserDefined
    CALL SimCoilUserDefined(CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive  )

  CASE(UnitarySystem)  ! 'AirLoopHVAC:UnitarySystem'
    CALL SimUnitarySystem(CompName, FirstHVACIteration, AirLoopNum, CompIndex, &
                          HeatActive=HeatingActive, CoolActive=CoolingActive)

  CASE(Furnace_UnitarySys)  ! 'AirLoopHVAC:Unitary:Furnace:HeatOnly', 'AirLoopHVAC:Unitary:Furnace:HeatCool',
                            ! 'AirLoopHVAC:UnitaryHeatOnly', 'AirLoopHVAC:UnitaryHeatCool'
                            ! 'AirLoopHVAC:UnitaryHeatPump:AirToAir', 'AirLoopHVAC:UnitaryHeatPump:WaterToAir'
    CALL SimFurnace(CompName, FirstHVACIteration, AirLoopNum, CompIndex)

  CASE(UnitarySystem_BypassVAVSys)     ! 'AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass'
    CALL SimUnitaryBypassVAV(CompName, FirstHVACIteration, AirLoopNum, CompIndex)

  CASE(UnitarySystem_MSHeatPump)     ! 'AirLoopHVAC:UnitaryHeatPump:AirToAir:Multispeed'
    CALL SimMSHeatPump(CompName, FirstHVACIteration, AirLoopNum, CompIndex)

! Humidifier Types for the air system simulation
  CASE(Humidifier)  ! 'Humidifier:Steam:Electric'
    CALL SimHumidifier(CompName,FirstHVACIteration, CompIndex)

! Evap Cooler Types for the air system simulation
  CASE(EvapCooler)  ! 'EvaporativeCooler:Direct:CelDekPad', 'EvaporativeCooler:Indirect:CelDekPad'
                    ! 'EvaporativeCooler:Indirect:WetCoil', 'EvaporativeCooler:Indirect:ResearchSpecial'
    CALL SimEvapCooler(CompName, CompIndex)

! Desiccant Dehumidifier Types for the air system simulation
  CASE(Desiccant)  ! 'Dehumidifier:Desiccant:NoFans', 'Dehumidifier:Desiccant:System'
    CALL SimDesiccantDehumidifier(CompName,FirstHVACIteration, CompIndex)

! Heat recovery
  CASE(HeatXchngr)  ! 'HeatExchanger:AirToAir:FlatPlate', 'HeatExchanger:AirToAir:SensibleAndLatent'
                    ! 'HeatExchanger:Desiccant:BalancedFlow'
    CALL SimHeatRecovery(CompName, FirstHVACIteration, CompIndex, ContFanCycCoil, &
                         EconomizerFlag=AirLoopControlInfo(AirLoopNum)%EconoActive, &
                         HighHumCtrlFlag=AirLoopControlInfo(AirLoopNum)%HighHumCtrlActive)

! Ducts
  CASE(Duct)  ! 'Duct'
    CALL SimDuct(CompName,FirstHVACIteration,CompIndex)

  CASE DEFAULT

END SELECT

! Set AirLoopControlInfo flag to identify coil operation for "Air Loop Coils"
! Any coil operation from multiple coils causes flag to be TRUE
! Flag is reset at beginning of each iteration (Subroutine SimHVAC)
AirLoopControlInfo(AirLoopNum)%CoolingActiveFlag = AirLoopControlInfo(AirLoopNum)%CoolingActiveFlag .OR. CoolingActive
AirLoopControlInfo(AirLoopNum)%HeatingActiveFlag = AirLoopControlInfo(AirLoopNum)%HeatingActiveFlag .OR. HeatingActive

RETURN

END SUBROUTINE SimAirLoopComponent

SUBROUTINE UpdateBranchConnections(AirLoopNum,BranchNum,Update)

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Fred Buhl
          !       DATE WRITTEN:  Nov 1999
          !           MODIFIED:
          !      RE-ENGINEERED:  This is new code, not reengineered

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine passes node data from a branch exit node through a
          ! splitter.

          ! METHODOLOGY EMPLOYED:
          ! Temperature, humidity ratio, and enthalpy are passed through from
          ! the inlet to the outlets. The mass flow is divided among the outlets
          ! according to the required mass flows established by the zone equipment
          ! simulation. The required mass flows are were stored in the node data
          ! as MassFlowRateSetPoints in the InitAirLoops routine.

          ! REFERENCES: None

          ! USE STATEMENTS
USE Psychrometrics, ONlY: PsyTdbFnHW
USE DataContaminantBalance, ONLY: Contaminant

IMPLICIT NONE

          ! SUBROUTINE ARGUMENTS:
INTEGER, INTENT(IN) :: BranchNum ! branch reference number
INTEGER, INTENT(IN) :: AirLoopNum ! primary air system number
INTEGER, INTENT(IN) :: Update     ! 1=BeforeBranchSim; 2=AfterBranchSim

          ! SUBROUTINE PARAMETER DEFINITIONS: None

          ! INTERFACE BLOCK DEFINITIONS: None

          ! DERIVED TYPE DEFINITIONS: None

          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS

INTEGER :: OutletNum ! splitter outlet DO loop index
INTEGER :: InletNum  ! mixer inlet DO loop index
INTEGER :: InletNodeNum ! node number of splitter inlet node
INTEGER :: OutletNodeNum ! node number of a splitter outlet node
INTEGER :: RABNodeNum    ! splitter outlet RAB node
INTEGER :: NonRABNodeNum ! splitter outlet nonRAB node
REAL(r64)    :: MassFlowRateSetSum ! sum of mass flow rate setpoints for splitter outlet nodes
REAL(r64)    :: MassFlowRateOut    ! outlet mass flow rate of mixer
REAL(r64)    :: MassFlowRateMinAvailOut ! outlet minimum available mass flow rate
REAL(r64)    :: OutletHumRat       ! outlet humidity ratio of mixer
REAL(r64)    :: OutletEnthalpy     ! outlet enthalpy of mixer
REAL(r64)    :: OutletPress
REAL(r64)    :: OutletCO2          ! outlet CO2 of mixer
REAL(r64)    :: OutletGC           ! outlet generic contaminant of mixer

          ! FLOW
MassFlowRateSetSum = 0.0d0
MassFlowRateOut = 0.0d0
MassFlowRateMinAvailOut = 0.0d0
OutletHumRat = 0.0d0
OutletEnthalpy = 0.0d0
OutletPress = 0.0d0
RABNodeNum = 0
NonRABNodeNum = 0
OutletCO2 = 0.0d0
OutletGC = 0.0d0

IF (PrimaryAirSystem(AirLoopNum)%Splitter%Exists .AND. Update == AfterBranchSim) THEN
  ! if we are at an inlet branch, pass data through the splitter
  IF (PrimaryAirSystem(AirLoopNum)%Splitter%BranchNumIn.EQ.BranchNum) THEN
    InletNodeNum = PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumIn
    ! Pass node data through the splitter
    DO OutletNum=1,PrimaryAirSystem(AirLoopNum)%Splitter%TotalOutletNodes
      OutletNodeNum = PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumOut(OutletNum)
      Node(OutletNodeNum)%Temp = Node(InletNodeNum)%Temp
      Node(OutletNodeNum)%HumRat = Node(InletNodeNum)%HumRat
      Node(OutletNodeNum)%Enthalpy = Node(InletNodeNum)%Enthalpy
      Node(OutletNodeNum)%Press = Node(InletNodeNum)%Press
      MassFlowRateSetSum = MassFlowRateSetSum + MIN(Node(OutletNodeNum)%MassFlowRateSetPoint, &
                                                    Node(OutletNodeNum)%MassFlowRateMaxAvail)
      IF (Contaminant%CO2Simulation) Then
        Node(OutletNodeNum)%CO2 = Node(InletNodeNum)%CO2
      End If
      IF (Contaminant%GenericContamSimulation) Then
        Node(OutletNodeNum)%GenContam = Node(InletNodeNum)%GenContam
      End If

    END DO
    IF (.NOT. PrimaryAirSystem(AirLoopNum)%RABExists) THEN
      ! set the outlet mass flows
      DO OutletNum=1,PrimaryAirSystem(AirLoopNum)%Splitter%TotalOutletNodes
        OutletNodeNum = PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumOut(OutletNum)
        IF (MassFlowRateSetSum.LT.SmallMassFlow .OR. Node(InletNodeNum)%MassFlowRate.LT.SmallMassFlow) THEN
          Node(OutletNodeNum)%MassFlowRate = 0.0d0
        ELSE
          Node(OutletNodeNum)%MassFlowRate = Node(InletNodeNum)%MassFlowRate * &
                                             (MIN(Node(OutletNodeNum)%MassFlowRateSetPoint, &
                                                  Node(OutletNodeNum)%MassFlowRateMaxAvail) &
                                             / MassFlowRateSetSum)
        END IF
        Node(OutletNodeNum)%MassFlowRateMaxAvail = Node(InletNodeNum)%MassFlowRateMaxAvail
        Node(OutletNodeNum)%MassFlowRateMinAvail = 0.0d0
      END DO
    ELSE ! set the RAB flow rates
      RABNodeNum = PrimaryAirSystem(AirLoopNum)%RABSplitOutNode
      NonRABNodeNum = PrimaryAirSystem(AirLoopNum)%OtherSplitOutNode
      IF (AirLoopControlInfo(AirLoopNum)%EconoActive) THEN
        Node(RABNodeNum)%MassFlowRate = 0.0d0
        Node(NonRABNodeNum)%MassFlowRate = Node(InletNodeNum)%MassFlowRate
      ELSE
        Node(RABNodeNum)%MassFlowRate = Node(RABNodeNum)%MassFlowRateSetPoint
        Node(NonRABNodeNum)%MassFlowRate = Node(InletNodeNum)%MassFlowRate - Node(RABNodeNum)%MassFlowRate
        IF (Node(NonRABNodeNum)%MassFlowRate <= AirLoopFlow(AirLoopNum)%MinOutAir) THEN
          Node(NonRABNodeNum)%MassFlowRate = MIN(AirLoopFlow(AirLoopNum)%MinOutAir,Node(InletNodeNum)%MassFlowRate)
          Node(RABNodeNum)%MassFlowRate = Node(InletNodeNum)%MassFlowRate - Node(NonRABNodeNum)%MassFlowRate
        END IF
      END IF
    END IF
  END IF
END IF

IF (PrimaryAirSystem(AirLoopNum)%Mixer%Exists .AND. Update == BeforeBranchSim) THEN
  ! if we are at a mixer outlet branch, calculate the outlet branch conditions
  IF (PrimaryAirSystem(AirLoopNum)%Mixer%BranchNumOut.EQ.BranchNum) THEN
    OutletNodeNum = PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumOut
    ! get the outlet mass flow rate and the outlet minavail mass flow rate
    DO InletNum=1,PrimaryAirSystem(AirLoopNum)%MIxer%TotalInletNodes
      InletNodeNum = PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumIn(InletNum)
      MassFlowRateOut = MassFlowRateOut + Node(InletNodeNum)%MassFlowRate
      MassFlowRateMinAvailOut = MassFlowRateMinAvailOut + Node(InletNodeNum)%MassFlowRateMinAvail
    END DO
    ! set the outlet mass flow
    Node(OutletNodeNum)%MassFlowRate = MassFlowRateOut
    Node(OutletNodeNum)%MassFlowRateMinAvail = MassFlowRateMinAvailOut
    Node(OutletNodeNum)%MassFlowRateMaxAvail = Node(OutletNodeNum)%MassFlowRateMax
    ! calculate the outlet humidity ratio and enthalpy and pressure
    IF (MassFlowRateOut > 0.0d0) THEN
      DO InletNum=1,PrimaryAirSystem(AirLoopNum)%Mixer%TotalInletNodes
        InletNodeNum = PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumIn(InletNum)
        OutletHumRat = OutletHumRat + (Node(InletNodeNum)%MassFlowRate * Node(InletNodeNum)%HumRat) / MassFlowRateOut
        OutletEnthalpy = OutletEnthalpy + (Node(InletNodeNum)%MassFlowRate * Node(InletNodeNum)%Enthalpy) / MassFlowRateOut
        OutletPress = OutletPress + (Node(InletNodeNum)%MassFlowRate * Node(InletNodeNum)%Press) / MassFlowRateOut
        IF (Contaminant%CO2Simulation) Then
          OutletCO2 = OutletCO2 + (Node(InletNodeNum)%MassFlowRate * Node(InletNodeNum)%CO2) / MassFlowRateOut
        END IF
        IF (Contaminant%GenericContamSimulation) Then
          OutletGC = OutletGC + (Node(InletNodeNum)%MassFlowRate * Node(InletNodeNum)%GenContam) / MassFlowRateOut
        END IF
      END DO
    ELSE
      InletNodeNum = PrimaryAirSystem(AirLoopNum)%Mixer%NodeNumIn(1)
      OutletHumRat = Node(InletNodeNum)%HumRat
      OutletEnthalpy = Node(InletNodeNum)%Enthalpy
      OutletPress = Node(InletNodeNum)%Press
      IF (Contaminant%CO2Simulation) Then
        OutletCO2 = Node(InletNodeNum)%CO2
      END IF
      IF (Contaminant%GenericContamSimulation) Then
        OutletGC = Node(InletNodeNum)%GenContam
      END IF
    END IF
    Node(OutletNodeNum)%HumRat = OutletHumRat
    Node(OutletNodeNum)%Enthalpy = OutletEnthalpy
    Node(OutletNodeNum)%Press = OutletPress
    ! calculate the outlet temperature
    Node(OutletNodeNum)%Temp = PsyTdbFnHW(OutletEnthalpy,OutletHumRat)
    IF (Contaminant%CO2Simulation) Then
      Node(OutletNodeNum)%CO2 = OutletCO2
    END IF
    IF (Contaminant%GenericContamSimulation) Then
      Node(OutletNodeNum)%GenContam = OutletGC
    END IF
  END IF
END IF

RETURN
END SUBROUTINE UpdateBranchConnections

SUBROUTINE ResolveSysFlow(SysNum,SysReSim)

          ! SUBROUTINE INFORMATION
          !             AUTHOR:  Fred Buhl
          !       DATE WRITTEN:  Dec 1999
          !           MODIFIED:
          !      RE-ENGINEERED:  This is new code, not reengineered

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutines checks for mass flow balance in all air system branches
          ! and across all connections. If there is a failure of mass flow
          ! balance, mass flows are imposed to achieve mass flow balance and
          ! the resimulate flag SysReSim is set to true.

          ! METHODOLOGY EMPLOYED:
          ! Node()%MassFlowRateMaxAvail for every node is set to the minimum
          ! Node()%MassFlowRateMaxAvail on each branch.  Mass balance is imposed
          ! at the branch connections. System inlet mass flows are forced to
          ! be less than or equal to the resulting inlet MassFlowRateMaxAvails.

          ! REFERENCES: None

          ! USE STATEMENTS:none

IMPLICIT NONE

          ! SUBROUTINE ARGUMENTS:
INTEGER, INTENT(IN)  :: SysNum ! the primary air system number
LOGICAL, INTENT(INOUT) :: SysReSim  ! Set to TRUE if mass balance fails and resimulation is needed

          ! SUBROUTINE PARAMETER DEFINITIONS: None

          ! INTERFACE BLOCK DEFINITIONS: None

          ! DERIVED TYPE DEFINITIONS: None

          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS

INTEGER :: BranchNum  ! branch DO loop index
INTEGER :: NodeIndex  ! node on branch DO loop index
REAL(r64)    :: MassFlowRateOutSum ! sum of splitter outlet mass flow rates (imposed)
REAL(r64)    :: BranchMassFlowMaxAvail ! branch level maximum flow rate possible
INTEGER :: OutletNum ! splitter outlet DO loop index
INTEGER :: OutletNodeNum ! a splitter outlet node number
INTEGER :: InletNodeNum ! splitter inlet node number
INTEGER :: NodeNum ! a node number
INTEGER :: NodeNumNext ! node number of next node on a branch
INTEGER :: InNodeNum ! air system inlet node
INTEGER :: InBranchNum ! air system inlet branch number
INTEGER :: InBranchIndex  ! air sys inlet branch DO loop index

          ! FLOW

! Find the minimum MassFlowMaxAvail for each branch in the system and store it on the branch inlet node.
! Check for mass flow conservation on each branch. Set SysReSim to TRUE is mass flow not conserved.
DO BranchNum=1,PrimaryAirSystem(SysNum)%NumBranches  ! loop over branches in system
  ! Initialize branch max avail mass flow to max avail mass flow at outlet node
  BranchMassFlowMaxAvail = Node(PrimaryAirSystem(SysNum)%Branch(BranchNum)%NodeNumOut)%MassFlowRateMaxAvail
  DO NodeIndex=1,PrimaryAirSystem(SysNum)%Branch(BranchNum)%TotalNodes ! loop over nodes on branch
    ! Get the new smallest max avail mass flow
    NodeNum = PrimaryAirSystem(SysNum)%Branch(BranchNum)%NodeNum(NodeIndex)
    BranchMassFlowMaxAvail = MIN(BranchMassFlowMaxAvail,Node(NodeNum)%MassFlowRateMaxAvail)
    ! Check for mass flow conservation on the branch
    IF (NodeIndex.LT.PrimaryAirSystem(SysNum)%Branch(BranchNum)%TotalNodes) THEN
      ! Set ReSim flag to TRUE if mass flow not conserved on this branch
      NodeNumNext = PrimaryAirSystem(SysNum)%Branch(BranchNum)%NodeNum(NodeIndex+1)
      IF (NodeNum.EQ.PrimaryAirSystem(SysNum)%OASysInletNodeNum) CYCLE ! don't enforce mass balance across OA Sys
      IF (ABS(Node(NodeNum)%MassFlowRate - Node(NodeNumNext)%MassFlowRate) .GT. SmallMassFlow) SysReSim = .TRUE.
    END IF
  END DO ! end node loop
  ! Store the minimum MassFlowMasAvail for this branch on the branch inlet node
  Node(PrimaryAirSystem(SysNum)%Branch(BranchNum)%NodeNumIn)%MassFlowRateMaxAvail = BranchMassFlowMaxAvail
END DO ! end branch loop
! force resimulation for fan-cycling, nonsimple systems
IF ( .NOT. AirLoopControlInfo(SysNum)%Simple .AND. AirLoopControlInfo(SysNum)%CyclingFan) THEN
  SysReSim = .TRUE.
END IF

! If mass flow conserved on each branch, check for mass balance across splitter
IF (.NOT.SysReSim .AND. PrimaryAirSystem(SysNum)%Splitter%Exists) THEN
  MassFlowRateOutSum = 0.0d0
  InletNodeNum = PrimaryAirSystem(SysNum)%Splitter%NodeNumIn
  ! Get sum of splitter outlet mass flows
  DO OutletNum=1,PrimaryAirSystem(SysNum)%Splitter%TotalOutletNodes
    OutletNodeNum = PrimaryAirSystem(SysNum)%Splitter%NodeNumOut(OutletNum)
    MassFlowRateOutSum = MassFlowRateOutSum + Node(OutletNodeNum)%MassFlowRate
  END DO
  ! Check whether sum of splitter outlet mass flows equals splitter inlet flow.
  IF (ABS(MassFlowRateOutSum-Node(InletNodeNum)%MassFlowRate) .GT. SmallMassFlow) SysReSim = .TRUE.
END IF

! If mass balance failed, resimulation is needed. Impose a mass balance for the new simulation.
IF (SysReSim) THEN
! Set the MassFlowRateMaxAvail on each node to the minimum MassFlowRateMaxAvail for the branch.
  DO BranchNum=1,PrimaryAirSystem(SysNum)%NumBranches ! loop over branches in system
    DO NodeIndex=2,PrimaryAirSystem(SysNum)%Branch(BranchNum)%TotalNodes ! loop over nodes on branch
      NodeNum = PrimaryAirSystem(SysNum)%Branch(BranchNum)%NodeNum(NodeIndex)
      Node(NodeNum)%MassFlowRateMaxAvail = Node(PrimaryAirSystem(SysNum)%Branch(BranchNum)%NodeNumIn)%MassFlowRateMaxAvail
    END DO
  END DO

  ! Impose mass balance at splitter
  IF (PrimaryAirSystem(SysNum)%Splitter%Exists) THEN
    InBranchNum = PrimaryAirSystem(SysNum)%Splitter%BranchNumIn
    MassFlowRateOutSum = 0.0d0
    InletNodeNum = PrimaryAirSystem(SysNum)%Splitter%NodeNumIn
    DO OutletNum=1,PrimaryAirSystem(SysNum)%Splitter%TotalOutletNodes
      OutletNodeNum = PrimaryAirSystem(SysNum)%Splitter%NodeNumOut(OutletNum)
      MassFlowRateOutSum  = MassFlowRateOutSum + MIN(Node(OutletNodeNum)%MassFlowRateMaxAvail,&
                                                     Node(OutletNodeNum)%MassFlowRateSetPoint)
    END DO
    ! set the splitter inlet Max Avail mass flow rate
    IF (Node(InletNodeNum)%MassFlowRateMaxAvail .GT. MassFlowRateOutSum+SmallMassFlow) THEN
      Node(InletNodeNum)%MassFlowRateMaxAvail = MassFlowRateOutSum
    END IF
    ! Pass the splitter inlet Max Avail mass flow rate upstream to the mixed air node
    DO NodeIndex=PrimaryAirSystem(SysNum)%Branch(InBranchNum)%TotalNodes-1,1,-1
      NodeNum = PrimaryAirSystem(SysNum)%Branch(InBranchNum)%NodeNum(NodeIndex)
      Node(NodeNum)%MassFlowRateMaxAvail = Node(InletNodeNum)%MassFlowRateMaxAvail
      IF (NodeNum .EQ. PrimaryAirSystem(SysNum)%OASysOutletNodeNum) EXIT
    END DO
  END IF

  ! Make sure air system inlet nodes have flow consistent with MassFlowRateMaxAvail
  DO InBranchIndex=1,PrimaryAirSystem(SysNum)%NumInletBranches
    InBranchNum = PrimaryAirSystem(SysNum)%InletBranchNum(InBranchIndex)
    InNodeNum  = PrimaryAirSystem(SysNum)%Branch(InBranchNum)%NodeNumIn
    Node(InNodeNum)%MassFlowRate = MIN(Node(InNodeNum)%MassFlowRate,Node(InNodeNum)%MassFlowRateMaxAvail)
  END DO
END IF

END SUBROUTINE ResolveSysFlow

SUBROUTINE SizeAirLoops

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Will perform central air system sizing simulations. Right now just
          ! initializes system sizing arrays. Calculations based on System Sizing
          ! input and the Zone Sizing simulations are done in UpdateSysSizing.

          ! METHODOLOGY EMPLOYED:
          ! Will run purchased hot and chilled water type simulations to determine
          ! central plant flow rates. Right now just uses one time flag to call
          ! SetUpSysSizingArrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! none

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
  LOGICAL,SAVE  :: MyOneTimeFlag = .true.

  IF (MyOneTimeFlag) THEN
    CALL SetUpSysSizingArrays
    MyOneTimeFlag = .FALSE.
  END IF

  RETURN

END SUBROUTINE SizeAirLoops

SUBROUTINE SizeAirLoopBranches(AirLoopNum,BranchNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing air loop branches for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE HVACHXAssistedCoolingCoil, ONLY: GetHXDXCoilName, GetHXCoilType
  USE WaterCoils,     ONLY: SetCoilDesFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: AirLoopNum
  Integer, Intent(IN) :: BranchNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  CHARACTER(LEN=MaxNameLength)  :: CompType     ! Component type
  CHARACTER(LEN=MaxNameLength)  :: CompName     ! Component name
  CHARACTER(len=MaxNameLength)  :: CoilName
  CHARACTER(len=MaxNameLength)  :: CoilType
  INTEGER                       :: CompType_Num ! Numeric equivalent for CompType
  INTEGER                       :: CompNum
  LOGICAL             :: ErrorsFound

  ErrorsFound = .FALSE.
  IF (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate == AutoSize) THEN

    CALL CheckSysSizing('Branch',PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Name)

    SELECT CASE(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%DuctType)
      CASE(Main)
        PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate = FinalSysSizing(AirLoopNum)%DesMainVolFlow
      CASE(Cooling)
        ! PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate = FinalSysSizing(AirLoopNum)%DesCoolVolFlow
        PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate = FinalSysSizing(AirLoopNum)%DesMainVolFlow
      CASE(Heating)
        ! PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate = FinalSysSizing(AirLoopNum)%DesHeatVolFlow
        PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate = FinalSysSizing(AirLoopNum)%DesMainVolFlow
      CASE(Other)
        PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate = FinalSysSizing(AirLoopNum)%DesMainVolFlow
      CASE DEFAULT
        PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate = FinalSysSizing(AirLoopNum)%DesMainVolFlow
    END SELECT

      CALL ReportSizingOutput('Branch',PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Name,&
                              'Maximum Flow Rate [m3/s]', &
                              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate)

  END IF

  IF (BranchNum == 1) THEN

    IF (PrimaryAirSystem(AirLoopNum)%DesignVolFlowRate == AutoSize) THEN
      CALL CheckSysSizing('AirLoopHVAC',PrimaryAirSystem(AirLoopNum)%Name)
      PrimaryAirSystem(AirLoopNum)%DesignVolFlowRate = FinalSysSizing(AirLoopNum)%DesMainVolFlow
      CALL ReportSizingOutput('AirLoopHVAC',PrimaryAirSystem(AirLoopNum)%Name, &
                              'Design Supply Air Flow Rate [m3/s]', &
                              PrimaryAirSystem(AirLoopNum)%DesignVolFlowRate)
    END IF

    IF (PrimaryAirSystem(AirLoopNum)%DesignVolFlowRate < SmallAirVolFlow) THEN
      CALL ShowSevereError('AirLoopHVAC ' // TRIM(PrimaryAirSystem(AirLoopNum)%Name) // &
                          ' has no air flow')
      CALL ShowContinueError('Check flow rate inputs for components in this air loop and,')
      CALL ShowContinueError('if autosized, check Sizing:Zone and Sizing:System objects and related inputs.')
      CALL ShowFatalError('Previous condition causes termination.')

    END IF

  END IF

  ! Loop over components in branch; pass the design air flow rate to the coil components that don't have
  ! design air flow as an input
    DO CompNum = 1,PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
      CompType     = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
      CompName     = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name
      CompType_Num = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num
      IF (CompType_Num ==  WaterCoil_DetailedCool .OR. CompType_Num == WaterCoil_SimpleHeat .OR. &
          CompType_Num == WaterCoil_CoolingHXAsst) THEN
        IF (CompType_Num == WaterCoil_CoolingHXAsst) THEN
          CoilName = GetHXDXCoilName(CompType,CompName,ErrorsFound)
          CoilType = GetHXCoilType(CompType,CompName,ErrorsFound)
        ELSE
          CoilName = CompName
          CoilType = CompType
        END IF
        CALL SetCoilDesFlow(CoilType,CoilName,PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%MaxVolFlowRate,&
                       ErrorsFound)
      END IF
    END DO ! End of component loop
    IF (ErrorsFound) THEN
      CALL ShowFatalError('Preceding sizing errors cause program termination')
    END IF

  RETURN

END  SUBROUTINE SizeAirLoopBranches

SUBROUTINE SetUpSysSizingArrays

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Allocate and fill the SysSizing data array.

          ! METHODOLOGY EMPLOYED:
          ! Uses data from System Sizing input and the system to zone connection data
          ! calculated in InitAirLoops and stored in AirToZoneNodeInfo in DataLoopNode..

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY : FindItemInList
  USE General, ONLY: FindNumberinList
  USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW
  USE DataEnvironment, ONLY : OutBaroPress
  USE OutputReportPredefined
  USE DataHeatBalance, ONLY: Zone
  USE DataDefineEquip, ONLY: AirDistUnit, NumAirDistUnits

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
  INTEGER :: AirLoopNum ! primary air system index
  INTEGER :: SysSizNum  ! system sizing input index
  INTEGER :: DesDayEnvrnNum ! design day index
  INTEGER :: NumZonesCooled ! number of zones cooled by a system
  INTEGER :: NumZonesHeated ! number of zones heated by a system
  REAL(r64)    :: MinOAFlow      ! design minimum outside air flow for a system
  INTEGER :: ZonesCooledNum ! loop index over zones cooled by a system
  INTEGER :: ZonesHeatedNum ! loop index over zones heated by a system
  INTEGER :: CtrlZoneNum    ! loop index over all controlled zones
  INTEGER :: MatchingCooledZoneNum ! the cooled zone index matching a heated zone in a system
  INTEGER :: PrimAirIndex        ! index of System Sizing primary air system name in primary air system array
  INTEGER :: SysSizIndex         ! system sizing do loop index
  LOGICAL :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  REAL(r64)     :: ZoneOAFracCooling ! zone OA fraction for cooling design air flow
  REAL(r64)     :: ZoneOAFracHeating ! zone OA fraction for heating design air flow
  REAL(r64) :: Ep = 1.0d0                ! zone primary air fraction
  REAL(r64) :: Er = 0.0d0                ! zone secondary recirculation fraction
  REAL(r64) :: ZoneSA                 ! Zone supply air flow rate
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: VdzClgByZone  !saved value of cooling based ZoneSA which is Vdz used in 62.1 tabular report (also used for zone level Vps)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: VdzHtgByZone  !saved value of heating based ZoneSA which is Vdz used in 62.1 tabular report (also used for zone level Vps)
  REAL(r64) :: ZonePA                 ! Zone primary air flow rate
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: VpzClgByZone    !saved value of cooling based ZonePA which is Vpz used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: VpzMinClgByZone !saved value of minimum cooling based ZonePA which is VpzClg-min used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: VpzHtgByZone    !saved value of heating based ZonePA which is Vpz used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: VpzMinHtgByZone !saved value of minimum heating based ZonePA which is VpzHtg-min used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: VpzClgSumBySys  !sum of saved value of cooling based ZonePA which is Vpz-sum used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: VpzHtgSumBySys  !sum of saved value of heating based ZonePA which is Vpz-sum used in 62.1 tabular report
  REAL(r64) :: NodeTemp               ! node temperature
  REAL(r64) :: NodeHumRat             ! node humidity ratio
  REAL(r64) :: MassFlowRate           ! Temporary variable
  REAL(r64) :: ClgSupplyAirAdjustFactor  ! temporary variable
  REAL(r64) :: HtgSupplyAirAdjustFactor  ! temporary variable
  REAL(r64) :: SysOAUnc               ! uncorrected system OA summing up people and area based OA for all zones for VRP
  REAL(r64) :: ZoneOAUnc              ! uncorrected zone OA summing up people and area based OA for each zone
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: VbzByZone  !saved value of ZoneOAUnc which is Vbz used in 62.1 tabular report
  REAL(r64) :: TotalPeople            ! total number of people in each zone
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: PzSumBySysCool  !saved value of TotalPeople which is Pz-sum used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: PzSumBySysHeat  !saved value of TotalPeople which is Pz-sum used in 62.1 tabular report
  REAL(r64) :: PeakPeople             ! peak population based on maximum people schedule value
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: PsBySysCool  !saved value of PeakPeople which is Ps used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: PsBySysHeat  !saved value of PeakPeople which is Ps used in 62.1 tabular report
  REAL(r64) :: PopulationDiversity    ! ratio of total system co-incident peak population to sum of people for all zones in system
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: DBySysCool  !saved value of PopulatonDiversity which is D used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: DBySysHeat  !saved value of PopulatonDiversity which is D used in 62.1 tabular report
  REAL(r64) :: RpPzSum    !Rp times Pz used for computing the system total Rp value for 62.1 tabular report
  REAL(r64) :: PzSum      !Pz sum for system total Pz for 62.1 tabular report
  REAL(r64) :: RaAzSum    !Ra time Az used for computing the system tota Ra value for 62.1 tabular report
  REAL(r64) :: AzSum      !Az sum for system total Az for 62.1 tabular report
  REAL(r64) :: VbzSum     !Vbz sum for system total Vbz for 62.1 tabular report
  REAL(r64) :: VozClgSum  !Voz-clg sum for system total Voz-clg for 62.1 tabular report
  REAL(r64) :: VozHtgSum  !Voz-htg sum for system total Voz-htg for 62.1 tabular report
  REAL(r64) :: VdzSum     !Vdz-sum for system total Vbz for 62.1 tabular report
  REAL(r64) :: VdzHtgSum  !heating based Vdz-sum for system total Vbz for 62.1 tabular report
  REAL(r64) :: VpzMin     !Vpz-min for system total Vbz for 62.1 tabular report

!  INTEGER :: ZoneIndex
  INTEGER :: iAirDistUnit

  ! allocate arrays used to store values for standard 62.1 tabular report
  IF (.NOT. ALLOCATED(VpzClgByZone)) THEN
    ALLOCATE (VdzClgByZone(NumOfZones))
    VdzClgByZone = 0.0d0
    ALLOCATE (VdzHtgByZone(NumOfZones))
    VdzHtgByZone = 0.0d0
    ALLOCATE (VpzClgByZone(NumOfZones))
    VpzClgByZone = 0.0d0
    ALLOCATE (VpzMinClgByZone(NumOfZones))
    VpzMinClgByZone = 0.0d0
     ALLOCATE (VpzHtgByZone(NumOfZones))
    VpzHtgByZone = 0.0d0
    ALLOCATE (VpzMinHtgByZone(NumOfZones))
    VpzMinHtgByZone = 0.0d0
    ALLOCATE (VpzClgSumBySys(NumPrimaryAirSys))
    VpzClgSumBySys = 0.0d0
    ALLOCATE (VpzHtgSumBySys(NumPrimaryAirSys))
    VpzHtgSumBySys = 0.0d0
    ALLOCATE (VbzByZone(NumOfZones))
    VbzByZone = 0.0d0
    ALLOCATE (PzSumBySysCool(NumPrimaryAirSys))
    PzSumBySysCool = 0.0d0
    ALLOCATE (PzSumBySysHeat(NumPrimaryAirSys))
    PzSumBySysHeat = 0.0d0
    ALLOCATE (PsBySysCool(NumPrimaryAirSys))
    PsBySysCool = 0.0d0
    ALLOCATE (PsBySysHeat(NumPrimaryAirSys))
    PsBySysHeat = 0.0d0
    ALLOCATE (DBySysCool(NumPrimaryAirSys))
    DBySysCool = 0.0d0
    ALLOCATE (DBySysHeat(NumPrimaryAirSys))
    DBySysHeat = 0.0d0
  END IF

  DO SysSizIndex=1,NumSysSizInput
    PrimAirIndex = FindItemInList(SysSizInput(SysSizIndex)%AirPriLoopName,PrimaryAirSystem%Name,NumPrimaryAirSys)
    IF (PrimAirIndex == 0) THEN
      CALL ShowSevereError ('Sizing:System: '//TRIM(SysSizInput(SysSizIndex)%AirPriLoopName)// &
                           ' references unknown AirLoopHVAC')
      ErrorsFound = .TRUE.
    ELSE
      SysSizInput(SysSizIndex)%AirLoopNum = PrimAirIndex
    END IF
  END DO
  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in Sizing:System input')
  END IF

  ALLOCATE(SysSizing(NumPrimaryAirSys,TotDesDays+TotRunDesPersDays))
  ALLOCATE(FinalSysSizing(NumPrimaryAirSys))
  ALLOCATE(CalcSysSizing(NumPrimaryAirSys))

  DO DesDayEnvrnNum=1,TotDesDays+TotRunDesPersDays
    DO AirLoopNum=1,NumPrimaryAirSys

      SysSizing(AirLoopNum,DesDayEnvrnNum)%AirPriLoopName = PrimaryAirSystem(AirLoopNum)%Name
      SysSizNum = FindItemInList(SysSizing(AirLoopNum,DesDayEnvrnNum)%AirPriLoopName,SysSizInput%AirPriLoopName,NumSysSizInput)
      IF (SysSizNum > 0) THEN  ! move data from system sizing input
        SysSizing(AirLoopNum,DesDayEnvrnNum)%LoadSizeType = SysSizInput(SysSizNum)%LoadSizeType
        SysSizing(AirLoopNum,DesDayEnvrnNum)%DesOutAirVolFlow = SysSizInput(SysSizNum)%DesOutAirVolFlow
        SysSizing(AirLoopNum,DesDayEnvrnNum)%SysAirMinFlowRat = SysSizInput(SysSizNum)%SysAirMinFlowRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%PreheatTemp = SysSizInput(SysSizNum)%PreheatTemp
        SysSizing(AirLoopNum,DesDayEnvrnNum)%PreheatHumRat = SysSizInput(SysSizNum)%PreheatHumRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%PrecoolTemp = SysSizInput(SysSizNum)%PrecoolTemp
        SysSizing(AirLoopNum,DesDayEnvrnNum)%PrecoolHumRat = SysSizInput(SysSizNum)%PrecoolHumRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolSupTemp = SysSizInput(SysSizNum)%CoolSupTemp
        SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatSupTemp = SysSizInput(SysSizNum)%HeatSupTemp
        SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolSupHumRat = SysSizInput(SysSizNum)%CoolSupHumRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatSupHumRat = SysSizInput(SysSizNum)%HeatSupHumRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%SizingOption = SysSizInput(SysSizNum)%SizingOption
        SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolOAOption = SysSizInput(SysSizNum)%CoolOAOption
        SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatOAOption = SysSizInput(SysSizNum)%HeatOAOption
        SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolAirDesMethod = SysSizInput(SysSizNum)%CoolAirDesMethod
        SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatAirDesMethod = SysSizInput(SysSizNum)%HeatAirDesMethod
        SysSizing(AirLoopNum,DesDayEnvrnNum)%InpDesCoolAirFlow = SysSizInput(SysSizNum)%DesCoolAirFlow
        SysSizing(AirLoopNum,DesDayEnvrnNum)%InpDesHeatAirFlow = SysSizInput(SysSizNum)%DesHeatAirFlow
        SysSizing(AirLoopNum,DesDayEnvrnNum)%MaxZoneOAFraction = SysSizInput(SysSizNum)%MaxZoneOAFraction
        SysSizing(AirLoopNum,DesDayEnvrnNum)%OAAutosized = SysSizInput(SysSizNum)%OAAutosized

      ELSE ! Set missing inputs to the first
        SysSizing(AirLoopNum,DesDayEnvrnNum)%LoadSizeType = SysSizInput(1)%LoadSizeType
        SysSizing(AirLoopNum,DesDayEnvrnNum)%DesOutAirVolFlow = SysSizInput(1)%DesOutAirVolFlow
        SysSizing(AirLoopNum,DesDayEnvrnNum)%SysAirMinFlowRat = SysSizInput(1)%SysAirMinFlowRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%PreheatTemp = SysSizInput(1)%PreheatTemp
        SysSizing(AirLoopNum,DesDayEnvrnNum)%PreheatHumRat = SysSizInput(1)%PreheatHumRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%PrecoolTemp = SysSizInput(1)%PrecoolTemp
        SysSizing(AirLoopNum,DesDayEnvrnNum)%PrecoolHumRat = SysSizInput(1)%PrecoolHumRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolSupTemp = SysSizInput(1)%CoolSupTemp
        SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatSupTemp = SysSizInput(1)%HeatSupTemp
        SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolSupHumRat = SysSizInput(1)%CoolSupHumRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatSupHumRat = SysSizInput(1)%HeatSupHumRat
        SysSizing(AirLoopNum,DesDayEnvrnNum)%SizingOption = SysSizInput(1)%SizingOption
        SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolOAOption = SysSizInput(1)%CoolOAOption
        SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatOAOption = SysSizInput(1)%HeatOAOption
        SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolAirDesMethod = SysSizInput(1)%CoolAirDesMethod
        SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatAirDesMethod = SysSizInput(1)%HeatAirDesMethod
        SysSizing(AirLoopNum,DesDayEnvrnNum)%InpDesCoolAirFlow = SysSizInput(1)%DesCoolAirFlow
        SysSizing(AirLoopNum,DesDayEnvrnNum)%InpDesHeatAirFlow = SysSizInput(1)%DesHeatAirFlow
        SysSizing(AirLoopNum,DesDayEnvrnNum)%MaxZoneOAFraction = SysSizInput(1)%MaxZoneOAFraction
        SysSizing(AirLoopNum,DesDayEnvrnNum)%OAAutosized = SysSizInput(1)%OAAutosized
      END IF
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatFlowSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolFlowSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%SensCoolCapSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatCapSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%PreHeatCapSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%SysCoolRetTempSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%SysCoolRetHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%SysHeatRetTempSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%SysHeatRetHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%SysCoolOutTempSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%SysCoolOutHumRatSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%SysHeatOutTempSeq(NumOfTimeStepInDay))
      ALLOCATE(SysSizing(AirLoopNum,DesDayEnvrnNum)%SysHeatOutHumRatSeq(NumOfTimeStepInDay))
      SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatFlowSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%CoolFlowSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%SensCoolCapSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%HeatCapSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%PreHeatCapSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%SysCoolRetTempSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%SysCoolRetHumRatSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%SysHeatRetTempSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%SysHeatRetHumRatSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%SysCoolOutTempSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%SysCoolOutHumRatSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%SysHeatOutTempSeq = 0.0d0
      SysSizing(AirLoopNum,DesDayEnvrnNum)%SysHeatOutHumRatSeq = 0.0d0
    END DO ! end the primary air system loop
  END DO

  DO AirLoopNum=1,NumPrimaryAirSys

    FinalSysSizing(AirLoopNum)%AirPriLoopName = PrimaryAirSystem(AirLoopNum)%Name
    CalcSysSizing(AirLoopNum)%AirPriLoopName = PrimaryAirSystem(AirLoopNum)%Name
    SysSizNum = FindItemInList(FinalSysSizing(AirLoopNum)%AirPriLoopName,SysSizInput%AirPriLoopName,NumSysSizInput)
    IF (SysSizNum > 0) THEN  ! move data from system sizing input
      FinalSysSizing(AirLoopNum)%LoadSizeType = SysSizInput(SysSizNum)%LoadSizeType
      FinalSysSizing(AirLoopNum)%DesOutAirVolFlow = SysSizInput(SysSizNum)%DesOutAirVolFlow
      FinalSysSizing(AirLoopNum)%SysAirMinFlowRat = SysSizInput(SysSizNum)%SysAirMinFlowRat
      FinalSysSizing(AirLoopNum)%PreheatTemp = SysSizInput(SysSizNum)%PreheatTemp
      FinalSysSizing(AirLoopNum)%PreheatHumRat = SysSizInput(SysSizNum)%PreheatHumRat
      FinalSysSizing(AirLoopNum)%PrecoolTemp = SysSizInput(SysSizNum)%PrecoolTemp
      FinalSysSizing(AirLoopNum)%PrecoolHumRat = SysSizInput(SysSizNum)%PrecoolHumRat
      FinalSysSizing(AirLoopNum)%CoolSupTemp = SysSizInput(SysSizNum)%CoolSupTemp
      FinalSysSizing(AirLoopNum)%HeatSupTemp = SysSizInput(SysSizNum)%HeatSupTemp
      FinalSysSizing(AirLoopNum)%CoolSupHumRat = SysSizInput(SysSizNum)%CoolSupHumRat
      FinalSysSizing(AirLoopNum)%HeatSupHumRat = SysSizInput(SysSizNum)%HeatSupHumRat
      FinalSysSizing(AirLoopNum)%SizingOption = SysSizInput(SysSizNum)%SizingOption
      FinalSysSizing(AirLoopNum)%CoolOAOption = SysSizInput(SysSizNum)%CoolOAOption
      FinalSysSizing(AirLoopNum)%HeatOAOption = SysSizInput(SysSizNum)%HeatOAOption
      FinalSysSizing(AirLoopNum)%CoolAirDesMethod = SysSizInput(SysSizNum)%CoolAirDesMethod
      FinalSysSizing(AirLoopNum)%HeatAirDesMethod = SysSizInput(SysSizNum)%HeatAirDesMethod
      FinalSysSizing(AirLoopNum)%InpDesCoolAirFlow = SysSizInput(SysSizNum)%DesCoolAirFlow
      FinalSysSizing(AirLoopNum)%InpDesHeatAirFlow = SysSizInput(SysSizNum)%DesHeatAirFlow
      FinalSysSizing(AirLoopNum)%SystemOAMethod = SysSizInput(SysSizNum)%SystemOAMethod
      FinalSysSizing(AirLoopNum)%MaxZoneOAFraction = SysSizInput(SysSizNum)%MaxZoneOAFraction
      FinalSysSizing(AirLoopNum)%OAAutosized = SysSizInput(SysSizNum)%OAAutosized
      CalcSysSizing(AirLoopNum)%LoadSizeType = SysSizInput(SysSizNum)%LoadSizeType
      CalcSysSizing(AirLoopNum)%DesOutAirVolFlow = SysSizInput(SysSizNum)%DesOutAirVolFlow
      CalcSysSizing(AirLoopNum)%SysAirMinFlowRat = SysSizInput(SysSizNum)%SysAirMinFlowRat
      CalcSysSizing(AirLoopNum)%PreheatTemp = SysSizInput(SysSizNum)%PreheatTemp
      CalcSysSizing(AirLoopNum)%PreheatHumRat = SysSizInput(SysSizNum)%PreheatHumRat
      CalcSysSizing(AirLoopNum)%PrecoolTemp = SysSizInput(SysSizNum)%PrecoolTemp
      CalcSysSizing(AirLoopNum)%PrecoolHumRat = SysSizInput(SysSizNum)%PrecoolHumRat
      CalcSysSizing(AirLoopNum)%CoolSupTemp = SysSizInput(SysSizNum)%CoolSupTemp
      CalcSysSizing(AirLoopNum)%HeatSupTemp = SysSizInput(SysSizNum)%HeatSupTemp
      CalcSysSizing(AirLoopNum)%CoolSupHumRat = SysSizInput(SysSizNum)%CoolSupHumRat
      CalcSysSizing(AirLoopNum)%HeatSupHumRat = SysSizInput(SysSizNum)%HeatSupHumRat
      CalcSysSizing(AirLoopNum)%SizingOption = SysSizInput(SysSizNum)%SizingOption
      CalcSysSizing(AirLoopNum)%CoolOAOption = SysSizInput(SysSizNum)%CoolOAOption
      CalcSysSizing(AirLoopNum)%HeatOAOption = SysSizInput(SysSizNum)%HeatOAOption
      CalcSysSizing(AirLoopNum)%CoolAirDesMethod = SysSizInput(SysSizNum)%CoolAirDesMethod
      CalcSysSizing(AirLoopNum)%HeatAirDesMethod = SysSizInput(SysSizNum)%HeatAirDesMethod
      CalcSysSizing(AirLoopNum)%InpDesCoolAirFlow = SysSizInput(SysSizNum)%DesCoolAirFlow
      CalcSysSizing(AirLoopNum)%InpDesHeatAirFlow = SysSizInput(SysSizNum)%DesHeatAirFlow
      CalcSysSizing(AirLoopNum)%SystemOAMethod = SysSizInput(SysSizNum)%SystemOAMethod
      CalcSysSizing(AirLoopNum)%MaxZoneOAFraction = SysSizInput(SysSizNum)%MaxZoneOAFraction
      CalcSysSizing(AirLoopNum)%OAAutosized = SysSizInput(SysSizNum)%OAAutosized
    ELSE ! Set missing inputs to the first
      CALL ShowWarningError('SetUpSysSizingArrays: Sizing for System (HVACAirLoop)="'//  &
          trim(FinalSysSizing(AirLoopNum)%AirPriLoopName)//'" will use Sizing:System specifications listed for System="'//  &
          trim(SysSizInput(1)%AirPriLoopName)//'".')
      FinalSysSizing(AirLoopNum)%LoadSizeType = SysSizInput(1)%LoadSizeType
      FinalSysSizing(AirLoopNum)%DesOutAirVolFlow = SysSizInput(1)%DesOutAirVolFlow
      FinalSysSizing(AirLoopNum)%SysAirMinFlowRat = SysSizInput(1)%SysAirMinFlowRat
      FinalSysSizing(AirLoopNum)%PreheatTemp = SysSizInput(1)%PreheatTemp
      FinalSysSizing(AirLoopNum)%PreheatHumRat = SysSizInput(1)%PreheatHumRat
      FinalSysSizing(AirLoopNum)%PrecoolTemp = SysSizInput(1)%PrecoolTemp
      FinalSysSizing(AirLoopNum)%PrecoolHumRat = SysSizInput(1)%PrecoolHumRat
      FinalSysSizing(AirLoopNum)%CoolSupTemp = SysSizInput(1)%CoolSupTemp
      FinalSysSizing(AirLoopNum)%HeatSupTemp = SysSizInput(1)%HeatSupTemp
      FinalSysSizing(AirLoopNum)%CoolSupHumRat = SysSizInput(1)%CoolSupHumRat
      FinalSysSizing(AirLoopNum)%HeatSupHumRat = SysSizInput(1)%HeatSupHumRat
      FinalSysSizing(AirLoopNum)%SizingOption = SysSizInput(1)%SizingOption
      FinalSysSizing(AirLoopNum)%CoolOAOption = SysSizInput(1)%CoolOAOption
      FinalSysSizing(AirLoopNum)%HeatOAOption = SysSizInput(1)%HeatOAOption
      FinalSysSizing(AirLoopNum)%CoolAirDesMethod = SysSizInput(1)%CoolAirDesMethod
      FinalSysSizing(AirLoopNum)%HeatAirDesMethod = SysSizInput(1)%HeatAirDesMethod
      FinalSysSizing(AirLoopNum)%InpDesCoolAirFlow = SysSizInput(1)%DesCoolAirFlow
      FinalSysSizing(AirLoopNum)%InpDesHeatAirFlow = SysSizInput(1)%DesHeatAirFlow
      FinalSysSizing(AirLoopNum)%SystemOAMethod = SysSizInput(1)%SystemOAMethod
      FinalSysSizing(AirLoopNum)%MaxZoneOAFraction = SysSizInput(1)%MaxZoneOAFraction
      FinalSysSizing(AirLoopNum)%OAAutosized = SysSizInput(1)%OAAutosized
      CalcSysSizing(AirLoopNum)%LoadSizeType = SysSizInput(1)%LoadSizeType
      CalcSysSizing(AirLoopNum)%DesOutAirVolFlow = SysSizInput(1)%DesOutAirVolFlow
      CalcSysSizing(AirLoopNum)%SysAirMinFlowRat = SysSizInput(1)%SysAirMinFlowRat
      CalcSysSizing(AirLoopNum)%PreheatTemp = SysSizInput(1)%PreheatTemp
      CalcSysSizing(AirLoopNum)%PreheatHumRat = SysSizInput(1)%PreheatHumRat
      CalcSysSizing(AirLoopNum)%PrecoolTemp = SysSizInput(1)%PrecoolTemp
      CalcSysSizing(AirLoopNum)%PrecoolHumRat = SysSizInput(1)%PrecoolHumRat
      CalcSysSizing(AirLoopNum)%CoolSupTemp = SysSizInput(1)%CoolSupTemp
      CalcSysSizing(AirLoopNum)%HeatSupTemp = SysSizInput(1)%HeatSupTemp
      CalcSysSizing(AirLoopNum)%CoolSupHumRat = SysSizInput(1)%CoolSupHumRat
      CalcSysSizing(AirLoopNum)%HeatSupHumRat = SysSizInput(1)%HeatSupHumRat
      CalcSysSizing(AirLoopNum)%SizingOption = SysSizInput(1)%SizingOption
      CalcSysSizing(AirLoopNum)%CoolOAOption = SysSizInput(1)%CoolOAOption
      CalcSysSizing(AirLoopNum)%HeatOAOption = SysSizInput(1)%HeatOAOption
      CalcSysSizing(AirLoopNum)%CoolAirDesMethod = SysSizInput(1)%CoolAirDesMethod
      CalcSysSizing(AirLoopNum)%HeatAirDesMethod = SysSizInput(1)%HeatAirDesMethod
      CalcSysSizing(AirLoopNum)%InpDesCoolAirFlow = SysSizInput(1)%DesCoolAirFlow
      CalcSysSizing(AirLoopNum)%InpDesHeatAirFlow = SysSizInput(1)%DesHeatAirFlow
      CalcSysSizing(AirLoopNum)%SystemOAMethod = SysSizInput(1)%SystemOAMethod
      CalcSysSizing(AirLoopNum)%MaxZoneOAFraction = SysSizInput(1)%MaxZoneOAFraction
      CalcSysSizing(AirLoopNum)%OAAutosized = SysSizInput(1)%OAAutosized
    END IF
    ALLOCATE(FinalSysSizing(AirLoopNum)%HeatFlowSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%CoolFlowSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%SensCoolCapSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%HeatCapSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%PreHeatCapSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%SysCoolRetTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%SysCoolRetHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%SysHeatRetTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%SysHeatRetHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%SysCoolOutTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%SysCoolOutHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%SysHeatOutTempSeq(NumOfTimeStepInDay))
    ALLOCATE(FinalSysSizing(AirLoopNum)%SysHeatOutHumRatSeq(NumOfTimeStepInDay))
    FinalSysSizing(AirLoopNum)%HeatFlowSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%CoolFlowSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%SensCoolCapSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%HeatCapSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%PreHeatCapSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%SysCoolRetTempSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%SysCoolRetHumRatSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%SysHeatRetTempSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%SysHeatRetHumRatSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%SysCoolOutTempSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%SysCoolOutHumRatSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%SysHeatOutTempSeq = 0.0d0
    FinalSysSizing(AirLoopNum)%SysHeatOutHumRatSeq = 0.0d0
    ALLOCATE(CalcSysSizing(AirLoopNum)%HeatFlowSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%CoolFlowSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%SensCoolCapSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%HeatCapSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%PreHeatCapSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%SysCoolRetTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%SysCoolRetHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%SysHeatRetTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%SysHeatRetHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%SysCoolOutTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%SysCoolOutHumRatSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%SysHeatOutTempSeq(NumOfTimeStepInDay))
    ALLOCATE(CalcSysSizing(AirLoopNum)%SysHeatOutHumRatSeq(NumOfTimeStepInDay))
    CalcSysSizing(AirLoopNum)%HeatFlowSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%CoolFlowSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%SensCoolCapSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%HeatCapSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%PreHeatCapSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%SysCoolRetTempSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%SysCoolRetHumRatSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%SysHeatRetTempSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%SysHeatRetHumRatSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%SysCoolOutTempSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%SysCoolOutHumRatSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%SysHeatOutTempSeq = 0.0d0
    CalcSysSizing(AirLoopNum)%SysHeatOutHumRatSeq = 0.0d0

    IF (AnyEnergyManagementSystemInModel) THEN

      CALL SetupEMSInternalVariable('Intermediate Air System Main Supply Volume Flow Rate',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[m3/s]', &
                                    FinalSysSizing(AirLoopNum)%DesMainVolFlow )
      CALL SetupEMSActuator('Sizing:System', FinalSysSizing(AirLoopNum)%AirPriLoopName, 'Main Supply Volume Flow Rate', &
                       '[m3/s]', FinalSysSizing(AirLoopNum)%EMSOverrideDesMainVolFlowOn, &
                       FinalSysSizing(AirLoopNum)%EMSValueDesMainVolFlow )

      CALL SetupEMSInternalVariable('Intermediate Air System Coincident Peak Cooling Mass Flow Rate',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kg/s]', &
                                    FinalSysSizing(AirLoopNum)%CoinCoolMassFlow )
      CALL SetupEMSActuator('Sizing:System', FinalSysSizing(AirLoopNum)%AirPriLoopName, &
                       'Main Supply Coincident Peak Cooling Mass Flow Rate', &
                       '[kg/s]', FinalSysSizing(AirLoopNum)%EMSOverrideCoinCoolMassFlowOn, &
                       FinalSysSizing(AirLoopNum)%EMSValueCoinCoolMassFlow )

      CALL SetupEMSInternalVariable('Intermediate Air System Coincident Peak Heating Mass Flow Rate',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kg/s]', &
                                    FinalSysSizing(AirLoopNum)%CoinHeatMassFlow )
      CALL SetupEMSActuator('Sizing:System', FinalSysSizing(AirLoopNum)%AirPriLoopName,    &
                       'Main Supply Coincident Peak Heating Mass Flow Rate',               &
                       '[kg/s]', FinalSysSizing(AirLoopNum)%EMSOverrideCoinHeatMassFlowOn, &
                       FinalSysSizing(AirLoopNum)%EMSValueCoinHeatMassFlow )

      CALL SetupEMSInternalVariable('Intermediate Air System Noncoincident Peak Cooling Mass Flow Rate',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kg/s]', &
                                    FinalSysSizing(AirLoopNum)%NonCoinCoolMassFlow )
      CALL SetupEMSActuator('Sizing:System', FinalSysSizing(AirLoopNum)%AirPriLoopName, &
                       'Main Supply Noncoincident Peak Cooling Mass Flow Rate', &
                       '[kg/s]', FinalSysSizing(AirLoopNum)%EMSOverrideNonCoinCoolMassFlowOn, &
                       FinalSysSizing(AirLoopNum)%EMSValueNonCoinCoolMassFlow )
      CALL SetupEMSInternalVariable('Intermediate Air System Noncoincident Peak Heating Mass Flow Rate',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kg/s]', &
                                    FinalSysSizing(AirLoopNum)%NonCoinHeatMassFlow )
      CALL SetupEMSActuator('Sizing:System', FinalSysSizing(AirLoopNum)%AirPriLoopName,    &
                       'Main Supply Noncoincident Peak Heating Mass Flow Rate',               &
                       '[kg/s]', FinalSysSizing(AirLoopNum)%EMSOverrideNonCoinHeatMassFlowOn, &
                       FinalSysSizing(AirLoopNum)%EMSValueNonCoinHeatMassFlow )

      CALL SetupEMSInternalVariable('Intermediate Air System Heating Volume Flow Rate',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[m3/s]', &
                                    FinalSysSizing(AirLoopNum)%DesHeatVolFlow )
      CALL SetupEMSActuator('Sizing:System', FinalSysSizing(AirLoopNum)%AirPriLoopName, &
                        'Main Heating Volume Flow Rate', &
                       '[m3/s]', FinalSysSizing(AirLoopNum)%EMSOverrideDesHeatVolFlowOn, &
                       FinalSysSizing(AirLoopNum)%EMSValueDesHeatVolFlow )

      CALL SetupEMSInternalVariable('Intermediate Air System Cooling Volume Flow Rate',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[m3/s]', &
                                    FinalSysSizing(AirLoopNum)%DesCoolVolFlow )
      CALL SetupEMSActuator('Sizing:System', FinalSysSizing(AirLoopNum)%AirPriLoopName, &
                                  'Main Cooling Volume Flow Rate', &
                       '[m3/s]', FinalSysSizing(AirLoopNum)%EMSOverrideDesCoolVolFlowOn, &
                       FinalSysSizing(AirLoopNum)%EMSValueDesCoolVolFlow )
      ! internal variables useful for sizing air system component models
      CALL SetupEMSInternalVariable('Air System Cooling Design Sensible Capacity',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[W]', &
                                    FinalSysSizing(AirLoopNum)%SensCoolCap )
      CALL SetupEMSInternalVariable('Air System Heating Design Sensible Capacity',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[W]', &
                                    FinalSysSizing(AirLoopNum)%HeatCap )
      CALL SetupEMSInternalVariable('Air System Preheating Design Sensible Capacity',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[W]', &
                                    FinalSysSizing(AirLoopNum)%PreheatCap )

      CALL SetupEMSInternalVariable('Air System Outdoor Air Design Volume Flow Rate',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[m3/s]', &
                                    FinalSysSizing(AirLoopNum)%DesOutAirVolFlow )

      CALL SetupEMSInternalVariable('Air System Cooling Design Mixed Air Temperature',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[C]', &
                                    FinalSysSizing(AirLoopNum)%CoolMixTemp )
      CALL SetupEMSInternalVariable('Air System Cooling Design Mixed Air Humidity Ratio',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kgWater/kgDryAir]', &
                                    FinalSysSizing(AirLoopNum)%CoolMixHumRat )
      CALL SetupEMSInternalVariable('Air System Cooling Design Return Air Temperature',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[C]', &
                                    FinalSysSizing(AirLoopNum)%CoolRetTemp )
      CALL SetupEMSInternalVariable('Air System Cooling Design Return Air Humidity Ratio',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kgWater/kgDryAir]', &
                                    FinalSysSizing(AirLoopNum)%CoolRetHumRat )
      CALL SetupEMSInternalVariable('Air System Cooling Design Outdoor Air Temperature',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[C]', &
                                    FinalSysSizing(AirLoopNum)%CoolOutTemp )
      CALL SetupEMSInternalVariable('Air System Cooling Design Outdoor Air Humidity Ratio',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kgWater/kgDryAir]', &
                                    FinalSysSizing(AirLoopNum)%CoolOutHumRat )

      CALL SetupEMSInternalVariable('Air System Heating Design Mixed Air Temperature',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[C]', &
                                    FinalSysSizing(AirLoopNum)%HeatMixTemp )
      CALL SetupEMSInternalVariable('Air System Heating Design Mixed Air Humidity Ratio',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kgWater/kgDryAir]', &
                                    FinalSysSizing(AirLoopNum)%HeatMixHumRat )
      CALL SetupEMSInternalVariable('Air System Heating Design Return Air Temperature',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[C]', &
                                    FinalSysSizing(AirLoopNum)%HeatRetTemp )
      CALL SetupEMSInternalVariable('Air System Heating Design Return Air Humidity Ratio',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kgWater/kgDryAir]', &
                                    FinalSysSizing(AirLoopNum)%HeatRetHumRat )
      CALL SetupEMSInternalVariable('Air System Heating Design Outdoor Air Temperature',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[C]', &
                                    FinalSysSizing(AirLoopNum)%HeatOutTemp )
      CALL SetupEMSInternalVariable('Air System Heating Design Outdoor Air Humidity Ratio',  &
                                      FinalSysSizing(AirLoopNum)%AirPriLoopName, '[kgWater/kgDryAir]', &
                                    FinalSysSizing(AirLoopNum)%HeatOutHumRat )
    ENDIF

  END DO ! end the primary air system loop

  ! If the system design minimum outside air flow rate is autosized, calculate it from the zone data
  DO AirLoopNum=1,NumPrimaryAirSys
    MinOAFlow = 0.0d0
    SysOAUnc = 0.0d0
    PopulationDiversity = 1.0d0
    TotalPeople = 0.0d0
    PeakPeople = 0.0d0
    ClgSupplyAirAdjustFactor = 1.0d0
    HtgSupplyAirAdjustFactor = 1.0d0
    SysSizNum = FindItemInList(FinalSysSizing(AirLoopNum)%AirPriLoopName,SysSizInput%AirPriLoopName,NumSysSizInput)
    IF (SysSizNum == 0) SysSizNum=1  ! use first when none applicable
    IF (FinalSysSizing(AirLoopNum)%OAAutosized) THEN
      NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
      DO ZonesCooledNum=1,NumZonesCooled
        CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
        TotalPeople = TotalPeople + FinalZoneSizing(CtrlZoneNum)%TotPeopleInZone
        PeakPeople = PeakPeople + FinalZoneSizing(CtrlZoneNum)%ZonePeakOccupancy
      END DO
      IF (TotalPeople > 0.0d0) THEN
        PopulationDiversity = PeakPeople / TotalPeople
      ELSE
        PopulationDiversity = 1.0d0
      END IF
      !save population for standard 62.1 report
      PzSumBySysCool(AirLoopNum) = TotalPeople
      PsBySysCool(AirLoopNum) = PeakPeople
      DBySysCool(AirLoopNum) = PopulationDiversity

      PzSumBySysHeat(AirLoopNum) = TotalPeople
      PsBySysHeat(AirLoopNum) = PeakPeople
      DBySysHeat(AirLoopNum) = PopulationDiversity

      DO ZonesCooledNum=1,NumZonesCooled
        CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
        IF (SysSizNum > 0) THEN
          IF (SysSizInput(SysSizNum)%SystemOAMethod == SOAM_ZoneSum) THEN ! ZoneSum Method
            MinOAFlow = MinOAFlow +  FinalZoneSizing(CtrlZoneNum)%MinOA
            ZoneOAFracCooling = 0.0d0
          ELSEIF (SysSizInput(SysSizNum)%SystemOAMethod == SOAM_VRP) THEN ! Ventilation Rate Procedure
            ZoneOAUnc = PopulationDiversity*FinalZoneSizing(CtrlZoneNum)%TotalOAFromPeople +   &
               FinalZoneSizing(CtrlZoneNum)%TotalOAFromArea

            !save for Standard 62 tabular report
            VbzByZone(CtrlZoneNum) = ZoneOAUnc
            SysOAUnc = SysOAUnc + ZoneOAUnc

            ! CR 8872 - check to see if uncorrected OA is calculated to be greater than 0
            IF (.NOT. ZoneOAUnc > 0.0d0) THEN
              CALL ShowSevereError('Sizing:System - The system outdoor air method is set to VRP in ' //  &
                 TRIM(FinalSysSizing(AirLoopNum)%AirPriLoopName))
              CALL ShowContinueError('But zone "'// TRIM(FinalZoneSizing(CtrlZoneNum)%ZoneName) //  &
                 '" associated with system does not have OA flow/person')
              CALL ShowContinueError('or flow/area values specified in DesignSpecification:OutdoorAir '//  &
                 'object associated with the zone')
            END IF

            !Save Std 62.1 cooling ventilation required by zone
            FinalZoneSizing(CtrlZoneNum)%VozClgByZone = ZoneOAUnc / FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling
            MinOAFlow = MinOAFlow + FinalZoneSizing(CtrlZoneNum)%VozClgByZone

            IF (FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow > 0.0d0) THEN
              IF (FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation > 0.0d0 .OR.   &
                 FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin <= 0) THEN
                ! multi-path system or VAV Minimum not defined
                ZoneOAFracCooling = FinalZoneSizing(CtrlZoneNum)%VozClgByZone / FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow
              ELSE
                ! Single path; Use VAV Minimum as the Vpz in the Zp = Voz / Vpz equations
                ZoneOAFracCooling = FinalZoneSizing(CtrlZoneNum)%VozClgByZone / FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin
              ENDIF
            ELSE
              ZoneOAFracCooling = 0.0d0
            ENDIF
          ELSE  ! error
          ENDIF
        ELSE ! ZoneSum Method
          MinOAFlow = MinOAFlow + FinalZoneSizing(CtrlZoneNum)%MinOA
          ZoneOAFracCooling = 0.0d0
        ENDIF

        ! Calc maximum zone OA fraction and supply air adjustment factor based on
        ! user entered max allowed OA fraction - a TRACE feature
        IF (FinalSysSizing(AirLoopNum)%MaxZoneOAFraction > 0 .AND.   &
            ZoneOAFracCooling > FinalSysSizing(AirLoopNum)%MaxZoneOAFraction) THEN
          IF (FinalSysSizing(AirLoopNum)%CoolAirDesMethod == FromDDCalc) THEN ! DesignDay Method
            ClgSupplyAirAdjustFactor = ZoneOAFracCooling / FinalSysSizing(AirLoopNum)%MaxZoneOAFraction
            IF (FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation > 0.0d0 .OR.   &
               FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin <= 0) THEN
              !multi-path system or VAV Minimum not defined
              FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow = FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow * ClgSupplyAirAdjustFactor
            ELSE
              !Single path; Use VAV Minimum as the Vpz in the Zp = Voz / Vpz equations
              FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin = FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin *   &
                 ClgSupplyAirAdjustFactor
              !Don't allow the design cooling airflow to be less than the VAV minimum airflow
              FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow = MAX(FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow,   &
                 FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin)
            ENDIF
            !Don't allow the design terminal airflow to be less than the design cooling airflow
            TermUnitSizing(CtrlZoneNum)%AirVolFlow = MAX(TermUnitSizing(CtrlZoneNum)%AirVolFlow,   &
               FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow)
            ZoneOAFracCooling = FinalSysSizing(AirLoopNum)%MaxZoneOAFraction
          ELSE
            ClgSupplyAirAdjustFactor = 1.0D0
          ENDIF
        ELSE
           ClgSupplyAirAdjustFactor = 1.0D0
        ENDIF

        ZoneSA = 0.0d0
        ZonePA = 0.0d0
        Ep = 1.0d0
        IF (FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation > 0.0d0) THEN ! multi-path system
             !Vpz: "Primary" supply air from main air handler served by an oa mixer
          ZonePA = FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow
             !Vdz: "Discharge" supply air delivered to zone by terminal unit
          ZoneSA = MAX(TermUnitSizing(CtrlZoneNum)%AirVolFlow,ZonePA)

          ! For re-circulation systems, Vpz used to determine Zpz is the design terminal airflow
          ! Std 62.1-2010, section 6.2.5.1: "Vpz (used to determin Zpz) is the primary airflow rate
          ! rate to the ventilation zone from the air handler, including outdoor air and recirculated air.
          VpzMinClgByZone(CtrlZoneNum) = ZoneSA

        ELSE ! single path system
             !Vdz: "Discharge" supply air delivered to zone by terminal unit
          ZonePA = FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow
             !Vpz: "Primary" supply air from main air handler served by an oa mixer
          ZoneSA = FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow

          ! Save VpzMin in case this is a single path VAV system.
          ! Std 62.1-2010, section 6.2.5.1: "For VAV-system design purposes, Vpz is the lowest zone primary
          ! airflow value expected at the design condition analyzed."
          VpzMinClgByZone(CtrlZoneNum) = FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin

          ! In case for some reason the VAV minimum has not been defined, use the design primary airflow
          IF (FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin <= 0) VpzMinClgByZone(CtrlZoneNum) = ZonePA
        END IF

        !save zone discharge supply airflow
        VdzClgByZone(CtrlZoneNum) = ZoneSA

        !save Vpz zone primary airflow for standard 62.1 report
        VpzClgByZone(CtrlZoneNum) = ZonePA
        VpzClgSumBySys(AirLoopNum) = VpzClgSumBySys(AirLoopNum) + ZonePA

        ! Fraction of required zone ventilation to minimum primary airflow expected at condition analyzed
        FinalZoneSizing(CtrlZoneNum)%ZpzClgByZone = 0.0d0
        IF (VpzMinClgByZone(CtrlZoneNum) > 0) THEN
          FinalZoneSizing(CtrlZoneNum)%ZpzClgByZone =   &
             MIN(1.0D0, FinalZoneSizing(CtrlZoneNum)%VozClgByZone / VpzMinClgByZone(CtrlZoneNum))
        ENDIF

        ! calc zone primary air fraction
        IF (ZoneSA > 0.0d0) Ep = ZonePA / ZoneSA
        IF (Ep > 1.0d0) Ep = 1.0d0
        FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFraction = Ep
        FinalZoneSizing(CtrlZoneNum)%ZoneOAFracCooling = ZoneOAFracCooling
      END DO

      NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
      IF (NumZonesHeated > 0) THEN
        DO ZonesHeatedNum=1,NumZonesHeated
          CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
          MatchingCooledZoneNum = FindNumberinList(CtrlZoneNum,AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums, &
                                                 NumZonesCooled)
          IF (MatchingCooledZoneNum == 0) THEN
            IF (SysSizNum > 0) THEN
              IF (SysSizInput(SysSizNum)%SystemOAMethod == SOAM_ZoneSum) THEN ! ZoneSum Method
                MinOAFlow = MinOAFlow +  FinalZoneSizing(CtrlZoneNum)%MinOA
                ZoneOAFracHeating = 0.0d0
              ELSEIF (SysSizInput(SysSizNum)%SystemOAMethod == SOAM_VRP) THEN ! Ventilation Rate Procedure
                ZoneOAUnc = PopulationDiversity * FinalZoneSizing(CtrlZoneNum)%TotalOAFromPeople +   &
                   FinalZoneSizing(CtrlZoneNum)%TotalOAFromArea

                !save for Standard 62 tabular report
                VbzByZone(CtrlZoneNum) = ZoneOAUnc
                SysOAUnc = SysOAUnc + ZoneOAUnc

                ! Save Std 62.1 heating ventilation required by zone
                FinalZoneSizing(CtrlZoneNum)%VozHtgByZone = ZoneOAUnc / FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating
                MinOAFlow = MinOAFlow + FinalZoneSizing(CtrlZoneNum)%VozHtgByZone

                IF (FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow > 0.0d0) THEN
                  IF (FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation > 0.0d0) THEN ! multi-path system
                    ! multi-path system
                    ZoneOAFracHeating = FinalZoneSizing(CtrlZoneNum)%VozHtgByZone / TermUnitSizing(CtrlZoneNum)%AirVolFlow
                  ELSE
                    ! Single path system
                    ZoneOAFracHeating = FinalZoneSizing(CtrlZoneNum)%VozHtgByZone / FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
                  ENDIF
                ELSE
                  ZoneOAFracHeating = 0.0d0
                ENDIF
              ELSE  ! would be error
              ENDIF
            ELSE ! ZoneSum Method
              MinOAFlow = MinOAFlow + FinalZoneSizing(CtrlZoneNum)%MinOA
              ZoneOAFracHeating = 0.0d0
            END IF
          ELSE
            ZoneOAFracHeating = 0.0d0
          END IF

          ! Calc maximum zone OA fraction and supply air adjustment factor based
          ! on user entered max allowed OA fraction - a TRACE feature
          IF (FinalSysSizing(AirLoopNum)%MaxZoneOAFraction > 0 .AND.   &
              ZoneOAFracHeating > FinalSysSizing(AirLoopNum)%MaxZoneOAFraction) THEN
            IF (FinalSysSizing(AirLoopNum)%CoolAirDesMethod == FromDDCalc) THEN ! DesignDay Method
              HtgSupplyAirAdjustFactor = ZoneOAFracHeating / FinalSysSizing(AirLoopNum)%MaxZoneOAFraction
              IF (FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation > 0.0d0 .OR.   &
                 FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin <= 0) THEN
                ! multi-path system or VAV Heating airflow max not defined
                TermUnitSizing(CtrlZoneNum)%AirVolFlow = TermUnitSizing(CtrlZoneNum)%AirVolFlow * HtgSupplyAirAdjustFactor
              ELSE
                ! Single path; Use VAV Heating airflow max as the Vpz in the Zp = Voz / Vpz equations
                FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow =   &
                   FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow * HtgSupplyAirAdjustFactor
                ! Don't allow the design terminal airflow to be less than the design heating airflow
                TermUnitSizing(CtrlZoneNum)%AirVolFlow= MAX(TermUnitSizing(CtrlZoneNum)%AirVolFlow,   &
                   FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow)
              ENDIF
              ZoneOAFracHeating = FinalSysSizing(AirLoopNum)%MaxZoneOAFraction
            ELSE
              HtgSupplyAirAdjustFactor = 1.0D0
            ENDIF
          ELSE
            HtgSupplyAirAdjustFactor = 1.0D0
          ENDIF

          ZoneSA = 0.0d0
          ZonePA = 0.0d0
          Ep = 1.0d0
          IF (FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation > 0.0d0) THEN ! multi-path system
                !Vpz: "Primary" supply air from main air handler served by an oa mixer
            ZonePA = FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
                !Vdz: "Discharge" supply air delivered to zone by terminal unit
            ZoneSA = MAX(TermUnitSizing(CtrlZoneNum)%AirVolFlow, ZonePA)

            ! For re-circulation systems, Vpz used to determine Zpz is the design terminal airflow
            ! Std 62.1-2010, section 6.2.5.1: "Vpz (used to determin Zpz) is the primary airflow rate
            ! rate to the ventilation zone from the air handler, including outdoor air and recirculated air.
            VpzMinHtgByZone(CtrlZoneNum) = ZoneSA

          ELSE ! single path system

            ZonePA = FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
            ZoneSA = FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow

            ! We do not use the cooling VAV min for heating because the VAV-box heating maximum may be larger.
            VpzMinHtgByZone(CtrlZoneNum) = ZoneSA
          END IF

          !save Vdz zone discharge supply airflow for standard 62.1 report
          VdzHtgByZone(CtrlZoneNum) = ZoneSA

          !save Vpz zone primary airflow for standard 62.1 report
          VpzHtgByZone(CtrlZoneNum) = ZonePA
          VpzHtgSumBySys(AirLoopNum) = VpzHtgSumBySys(AirLoopNum) + ZonePA

          ! Fraction of required zone ventilation to minimum primary airflow expected at condition analyzed
          FinalZoneSizing(CtrlZoneNum)%ZpzHtgByZone = 0.0d0
          IF (VpzMinHtgByZone(CtrlZoneNum) > 0) THEN
            FinalZoneSizing(CtrlZoneNum)%ZpzHtgByZone =   &
               MIN(1.0D0, FinalZoneSizing(CtrlZoneNum)%VozHtgByZone / VpzMinHtgByZone(CtrlZoneNum))
          ENDIF

          ! calc zone primary air fraction
          IF (ZoneSA > 0.0d0) Ep = ZonePA / ZoneSA
          IF (Ep > 1.0d0) Ep = 1.0d0
          FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFractionHtg = Ep
          FinalZoneSizing(CtrlZoneNum)%ZoneOAFracHeating = ZoneOAFracHeating
        END DO

      ELSE ! getting heating flow based values for Std 62.1 report for single path systems
        ZoneOAFracHeating = 0.0d0
        DO ZonesHeatedNum=1,NumZonesCooled
          CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedNum)

          ! Voz ventilation airflow required during heating mode
          FinalZoneSizing(CtrlZoneNum)%VozHtgByZone = VbzByZone(CtrlZoneNum) / FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating

          IF (FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow > 0.0d0) THEN
            ! ZoneOAFracHeating = FinalZoneSizing(CtrlZoneNum)%MinOA / FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
            IF (FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation > 0.0d0) THEN ! multi-path system
              ! multi-path system
              IF (TermUnitSizing(CtrlZoneNum)%AirVolFlow .NE. 0) THEN
                ZoneOAFracHeating = FinalZoneSizing(CtrlZoneNum)%VozHtgByZone / TermUnitSizing(CtrlZoneNum)%AirVolFlow
              END IF
            ELSE
              ! Single path system
              ZoneOAFracHeating = FinalZoneSizing(CtrlZoneNum)%VozHtgByZone / FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
            ENDIF
          ELSE
            ZoneOAFracHeating = 0.0d0
          ENDIF

          ! Calc maximum zone OA fraction and supply air adjustment factor based
          ! on user entered max allowed OA fraction - a TRACE feature
          IF (FinalSysSizing(AirLoopNum)%MaxZoneOAFraction > 0 .AND.   &
              ZoneOAFracHeating > FinalSysSizing(AirLoopNum)%MaxZoneOAFraction) THEN
            IF (FinalSysSizing(AirLoopNum)%HeatAirDesMethod == FromDDCalc) THEN ! DesignDay Method
              HtgSupplyAirAdjustFactor = ZoneOAFracHeating / FinalSysSizing(AirLoopNum)%MaxZoneOAFraction
              IF (FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation > 0.0d0 .OR.   &
                 FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin <= 0) THEN
                ! multi-path system or VAV Heating airflow max not defined
                TermUnitSizing(CtrlZoneNum)%AirVolFlow = TermUnitSizing(CtrlZoneNum)%AirVolFlow * HtgSupplyAirAdjustFactor
              ELSE
                ! Single path; Use VAV Heating airflow max as the Vpz in the Zp = Voz / Vpz equations
                FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow =   &
                   FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow * HtgSupplyAirAdjustFactor
                ! Don't allow the design terminal airflow to be less than the design heating airflow
                TermUnitSizing(CtrlZoneNum)%AirVolFlow=   &
                   MAX(TermUnitSizing(CtrlZoneNum)%AirVolFlow, FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow)
              ENDIF
              ZoneOAFracHeating = FinalSysSizing(AirLoopNum)%MaxZoneOAFraction
            ENDIF
          ENDIF
          ZonePA = FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
          ZoneSA = FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
          !save Vdz zone discharge airflow for standard 62.1 report
          VdzHtgByZone(CtrlZoneNum) = ZoneSA
          !save Vpz zone primary airflow for standard 62.1 report
          VpzHtgByZone(CtrlZoneNum) = ZonePA
          VpzHtgSumBySys(AirLoopNum) = VpzHtgSumBySys(AirLoopNum) + ZonePA

          ! We do not use the cooling VAV min for heating because the VAV-box heating maximum may be larger.
          VpzMinHtgByZone(CtrlZoneNum) = ZoneSA

          ! Save Std 62.1 heating ventilation required by zone
          FinalZoneSizing(CtrlZoneNum)%VozHtgByZone = VbzByZone(CtrlZoneNum) / FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating

          ! Fraction of required zone ventilation to minimum primary airflow expected at condition analyzed
          FinalZoneSizing(CtrlZoneNum)%ZpzHtgByZone = 0.0d0
          IF (VpzMinHtgByZone(CtrlZoneNum) > 0) THEN
            FinalZoneSizing(CtrlZoneNum)%ZpzHtgByZone = FinalZoneSizing(CtrlZoneNum)%VozHtgByZone/ VpzMinHtgByZone(CtrlZoneNum)
          ENDIF

          ! calc zone primary air fraction
          Ep = 1.0d0
          IF (ZoneSA > 0.0d0) Ep = ZonePA / ZoneSA
          IF (Ep > 1.0d0) Ep = 1.0d0
          FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFractionHtg = Ep
          FinalZoneSizing(CtrlZoneNum)%ZoneOAFracHeating = ZoneOAFracHeating
        END DO
        FinalZoneSizing(CtrlZoneNum)%SupplyAirAdjustFactor = MAX(ClgSupplyAirAdjustFactor,HtgSupplyAirAdjustFactor)
        CalcZoneSizing(CtrlZoneNum, CurOverallSimDay)%SupplyAirAdjustFactor = FinalZoneSizing(CtrlZoneNum)%SupplyAirAdjustFactor
      END IF

      FinalSysSizing(AirLoopNum)%SysUncOA = SysOAUnc
      CalcSysSizing(AirLoopNum)%SysUncOA = SysOAUnc

      FinalSysSizing(AirLoopNum)%DesOutAirVolFlow = MinOAFlow
      CalcSysSizing(AirLoopNum)%DesOutAirVolFlow = MinOAFlow

      DO DesDayEnvrnNum=1,TotDesDays+TotRunDesPersDays
        SysSizing(AirLoopNum,DesDayEnvrnNum)%DesOutAirVolFlow = FinalSysSizing(AirLoopNum)%DesOutAirVolFlow
      END DO
    END IF
  END DO

  ! write predefined standard 62.1 report data
  DO AirLoopNum=1,NumPrimaryAirSys
    IF (FinalSysSizing(AirLoopNum)%SystemOAMethod == SOAM_VRP) THEN
      NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
      !System Ventilation Requirements for Cooling
      CALL PreDefTableEntry(pdchS62svrClSumVpz,FinalSysSizing(AirLoopNum)%AirPriLoopName,VpzClgSumBySys(AirLoopNum),3)      !Vpz-sum
      CALL PreDefTableEntry(pdchS62svrClPs,FinalSysSizing(AirLoopNum)%AirPriLoopName,PsBySysCool(AirLoopNum))               !Ps
      CALL PreDefTableEntry(pdchS62svrClSumPz,FinalSysSizing(AirLoopNum)%AirPriLoopName,PzSumBySysCool(AirLoopNum))         !Pz-sum
      CALL PreDefTableEntry(pdchS62svrClD,FinalSysSizing(AirLoopNum)%AirPriLoopName,DBySysCool(AirLoopNum))                 !D
      CALL PreDefTableEntry(pdchS62svrClVou,FinalSysSizing(AirLoopNum)%AirPriLoopName,FinalSysSizing(AirLoopNum)%SysUncOA,4)  !Vou

      !System Ventilation Requirements for Heating
      CALL PreDefTableEntry(pdchS62svrHtSumVpz,FinalSysSizing(AirLoopNum)%AirPriLoopName,VpzHtgSumBySys(AirLoopNum),3)      !Vpz-sum
      CALL PreDefTableEntry(pdchS62svrHtPs,FinalSysSizing(AirLoopNum)%AirPriLoopName,PsBySysHeat(AirLoopNum))               !Ps
      CALL PreDefTableEntry(pdchS62svrHtSumPz,FinalSysSizing(AirLoopNum)%AirPriLoopName,PzSumBySysHeat(AirLoopNum))         !Pz-sum
      CALL PreDefTableEntry(pdchS62svrHtD,FinalSysSizing(AirLoopNum)%AirPriLoopName,DBySysHeat(AirLoopNum))                 !D
      CALL PreDefTableEntry(pdchS62svrHtVou,FinalSysSizing(AirLoopNum)%AirPriLoopName,FinalSysSizing(AirLoopNum)%SysUncOA,4)  !Vou

      ! clear temporary values for system ventilation parameters report
      RpPzSum = 0.0d0
      RaAzSum = 0.0d0
      AzSum = 0.0d0
      VbzSum = 0.0d0
      VozClgSum = 0.0d0
      VozHtgSum = 0.0d0
      VdzSum = 0.0d0
      VdzHtgSum = 0.0d0
      VpzMin = 0.0d0
      DO ZonesCooledNum=1,NumZonesCooled ! loop over cooled zones
        CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
        !Zone Ventilation Parameters
        CALL PreDefTableEntry(pdchS62zvpAlN,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           AirToZoneNodeInfo(AirLoopNum)%AirLoopName)               !Air loop name
        CALL PreDefTableEntry(pdchS62zvpRp,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           FinalZoneSizing(CtrlZoneNum)%DesOAFlowPPer,6) !Rp
        CALL PreDefTableEntry(pdchS62zvpPz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           FinalZoneSizing(CtrlZoneNum)%TotPeopleInZone)   !Pz
        CALL PreDefTableEntry(pdchS62zvpRa,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           FinalZoneSizing(CtrlZoneNum)%DesOAFlowPerArea,6)   !Ra
  !      ZoneIndex = FinalZoneSizing(CtrlZoneNum)%ActualZoneNum
        CALL PreDefTableEntry(pdchS62zvpAz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           FinalZoneSizing(CtrlZoneNum)%TotalZoneFloorArea) ! Az
        CALL PreDefTableEntry(pdchS62zvpVbz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           VbzByZone(CtrlZoneNum),4)       !Vbz
        CALL PreDefTableEntry(pdchS62zvpClEz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling,3)  !Ez-clg
        IF (FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling .NE. 0.0d0) THEN
          CALL PreDefTableEntry(pdchS62zvpClVoz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
             VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling,4)  !Voz-clg
          CALL PreDefTableEntry(pdchS62zcdVozclg,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
             VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling,4)  !Voz-clg
          CALL PreDefTableEntry(pdchS62zcdZpz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
             FinalZoneSizing(CtrlZoneNum)%ZpzClgByZone,3)  !Zpz = Voz/Vpz (see eq 6-5 in 62.1-2010)
          VozClgSum = VozClgSum + VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling
        END IF
        ! accumulate values for system ventilation parameters report
        RpPzSum = RpPzSum + FinalZoneSizing(CtrlZoneNum)%DesOAFlowPPer * FinalZoneSizing(CtrlZoneNum)%TotPeopleInZone
        RaAzSum = RaAzSum + FinalZoneSizing(CtrlZoneNum)%DesOAFlowPerArea * FinalZoneSizing(CtrlZoneNum)%TotalZoneFloorArea
        AzSum = AzSum + FinalZoneSizing(CtrlZoneNum)%TotalZoneFloorArea
        VbzSum = VbzSum + VbzByZone(CtrlZoneNum)
        !Zone Ventilation Calculations for Cooling Design
        CALL PreDefTableEntry(pdchS62zcdVpz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           VpzClgByZone(CtrlZoneNum),3)               !Vpz LS:
        VpzMin = VpzMin + VpzMinClgByZone(CtrlZoneNum)
        CALL PreDefTableEntry(pdchS62zcdVdz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           VdzClgByZone(CtrlZoneNum),4)               !Vdz
        CALL PreDefTableEntry(pdchS62zcdVpzmin,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           VpzMinClgByZone(CtrlZoneNum),4)         !Vpz-min
        VdzSum = VdzSum + VdzClgByZone(CtrlZoneNum)
        !box type
        DO iAirDistUnit = 1, NumAirDistUnits
          IF (AirDistUnit(iAirDistUnit)%ZoneEqNum .EQ. CtrlZoneNum) THEN
            CALL PreDefTableEntry(pdchS62zcdBox,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
               AirDistUnit(iAirDistUnit)%EquipType(1))  !use first type of equipment listed
            EXIT !if it has been found no more searching is needed
          END IF
        END DO
      END DO
      NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
      CALL PreDefTableEntry(pdchS62scdVpzmin,FinalSysSizing(AirLoopNum)%AirPriLoopName,VpzMin,4)           !Vpz-min

      VpzMin = 0.0d0
      IF (NumZonesHeated > 0) THEN
        DO ZonesHeatedNum=1,NumZonesHeated ! loop over the heated zones
        CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
        !Zone Ventilation Calculations for Heating Design
        CALL PreDefTableEntry(pdchS62zhdVpz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           VpzHtgByZone(CtrlZoneNum),3)            !Vpz
        VpzMin = VpzMin + VpzMinHtgByZone(CtrlZoneNum)

        CALL PreDefTableEntry(pdchS62zhdVdz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           VdzHtgByZone(CtrlZoneNum),4)               !Vdz
        CALL PreDefTableEntry(pdchS62zhdVpzmin,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           VpzMinHtgByZone(CtrlZoneNum),4)       !Vpz-min

        VdzHtgSum = VdzHtgSum + VdzHtgByZone(CtrlZoneNum)
        !box type
        DO iAirDistUnit = 1, NumAirDistUnits
          IF (AirDistUnit(iAirDistUnit)%ZoneEqNum .EQ. CtrlZoneNum) THEN
            CALL PreDefTableEntry(pdchS62zhdBox,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
               AirDistUnit(iAirDistUnit)%EquipType(1))  !use first type of equipment listed
            EXIT !if it has been found no more searching is needed
          END IF
        END DO
        !Zone Ventilation Parameters
        CALL PreDefTableEntry(pdchS62zvpHtEz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating,3)  !Ez-htg
        IF (FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating .NE. 0.0d0) THEN
          CALL PreDefTableEntry(pdchS62zvpHtVoz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
             VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating,4)  !Voz-htg
          CALL PreDefTableEntry(pdchS62zhdVozhtg,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
             VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating,4)  !Voz-htg
          CALL PreDefTableEntry(pdchS62zhdZpz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
             FinalZoneSizing(CtrlZoneNum)%ZpzHtgByZone,3)  !Zpz = Voz/Vpz (see eq 6-5 in 62.1-2010)
          VozHtgSum = VozHtgSum + VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating
        END IF
        END DO
      ELSE
        DO ZonesHeatedNum=1,NumZonesCooled ! loop over the cooled zones if no centrally heated zones
        CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedNum)
        !Zone Ventilation Calculations for Heating Design
        CALL PreDefTableEntry(pdchS62zhdVpz,FinalZoneSizing(CtrlZoneNum)%ZoneName,VpzHtgByZone(CtrlZoneNum),3)        !Vpz
        VpzMin = VpzMin + VpzMinHtgByZone(CtrlZoneNum)
        CALL PreDefTableEntry(pdchS62zhdVdz,FinalZoneSizing(CtrlZoneNum)%ZoneName,VdzHtgByZone(CtrlZoneNum),3)        !Vdz
        CALL PreDefTableEntry(pdchS62zhdVpzmin,FinalZoneSizing(CtrlZoneNum)%ZoneName,VpzMinHtgByZone(CtrlZoneNum),3)  !Vpz-min
        VdzHtgSum = VdzHtgSum + VdzHtgByZone(CtrlZoneNum)
        !box type
        DO iAirDistUnit = 1, NumAirDistUnits
          IF (AirDistUnit(iAirDistUnit)%ZoneEqNum .EQ. CtrlZoneNum) THEN
              !use first type of equipment listed
            CALL PreDefTableEntry(pdchS62zhdBox,FinalZoneSizing(CtrlZoneNum)%ZoneName,AirDistUnit(iAirDistUnit)%EquipType(1))
            EXIT !if it has been found no more searching is needed
          END IF
        END DO
        !Zone Ventilation Parameters
        CALL PreDefTableEntry(pdchS62zvpHtEz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
           FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating,3)  !Ez-htg
        IF (FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating .NE. 0.0d0) THEN
          CALL PreDefTableEntry(pdchS62zvpHtVoz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &    !Voz-htg
                         VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating,4)
          CALL PreDefTableEntry(pdchS62zhdVozhtg,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &     !Voz-htg
                         VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating,4)
          IF (VpzHtgByZone(CtrlZoneNum) .NE. 0.0d0) THEN
            CALL PreDefTableEntry(pdchS62zhdZpz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  & !Zpz = Voz/Vpz (see eq 6-5 in 62.1-2010)
                        (VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating)/VpzHtgByZone(CtrlZoneNum),3)
          END IF
          VozHtgSum = VozHtgSum + VbzByZone(CtrlZoneNum)/FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating
        END IF
        END DO
      END IF

      CALL PreDefTableEntry(pdchS62shdVpzmin,FinalSysSizing(AirLoopNum)%AirPriLoopName,VpzMin)           !Vpz-min
      !System Ventilation Parameters
      IF (PzSumBySysCool(AirLoopNum) .NE. 0.0d0) THEN
        CALL PreDefTableEntry(pdchS62svpRp,FinalSysSizing(AirLoopNum)%AirPriLoopName,RpPzSum/PzSumBySysCool(AirLoopNum),6)
      END IF
      CALL PreDefTableEntry(pdchS62svpPz,FinalSysSizing(AirLoopNum)%AirPriLoopName,PzSumBySysCool(AirLoopNum))
      IF (AzSum .NE. 0.0d0) THEN
        CALL PreDefTableEntry(pdchS62svpRa,FinalSysSizing(AirLoopNum)%AirPriLoopName,RaAzSum/AzSum,6)
      END IF
      CALL PreDefTableEntry(pdchS62svpAz,FinalSysSizing(AirLoopNum)%AirPriLoopName,AzSum)
      CALL PreDefTableEntry(pdchS62svpVbz,FinalSysSizing(AirLoopNum)%AirPriLoopName,VbzSum,4)
      CALL PreDefTableEntry(pdchS62svpClVoz,FinalSysSizing(AirLoopNum)%AirPriLoopName,VozClgSum,4)  !Voz-clg
      CALL PreDefTableEntry(pdchS62svpHtVoz,FinalSysSizing(AirLoopNum)%AirPriLoopName,VozHtgSum,4)  !Voz-htg
      !System Ventilation Calculations for Cooling Design
      CALL PreDefTableEntry(pdchS62scdVpz,FinalSysSizing(AirLoopNum)%AirPriLoopName,VpzClgSumBySys(AirLoopNum))           !Vpz-sum
      CALL PreDefTableEntry(pdchS62scdVdz,FinalSysSizing(AirLoopNum)%AirPriLoopName,VdzSum)           !Vdz-sum
      CALL PreDefTableEntry(pdchS62scdVozclg,FinalSysSizing(AirLoopNum)%AirPriLoopName,VozClgSum,4)  !Voz-clg
      !System Ventilation Calculations for Heating Design
      CALL PreDefTableEntry(pdchS62shdVpz,FinalSysSizing(AirLoopNum)%AirPriLoopName,VpzHtgSumBySys(AirLoopNum))           !Vpz-sum
      CALL PreDefTableEntry(pdchS62shdVdz,FinalSysSizing(AirLoopNum)%AirPriLoopName,VdzHtgSum)           !Vdz-sum
      CALL PreDefTableEntry(pdchS62shdVozhtg,FinalSysSizing(AirLoopNum)%AirPriLoopName,VozHtgSum,4)  !Voz-htg
    END IF
  END DO

  RETURN

END SUBROUTINE SetUpSysSizingArrays

SUBROUTINE UpdateSysSizing(CallIndicator)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Update the result variables of the zone sizing calculation

          ! METHODOLOGY EMPLOYED:
          ! CallIndicator = 1 (BeginDay) zero the result arrays
          ! CallIndicator = 2 (DuringDay) fill arrays, averaging over 1 zone time step
          ! CallIndicator = 3 (EndDay) calculate daily maxima
          ! CallIndicator = 5 (EndSysSizingCalc) write out results

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: FindNumberinList
  USE DataEnvironment, ONLY: StdBaroPress, OutDryBulbTemp, OutHumRat, StdRhoAir
  USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb
  USE EMSManager,     ONLY: ManageEMS
  USE OutputReportPredefined
  USE DataHeatBalance,   ONLY: Zone

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: CallIndicator

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  CHARACTER(len=*), PARAMETER :: SSizeFmt10="('Time')"
  CHARACTER(len=*), PARAMETER :: SSizeFmt11="(A1,A,A,A1,A,A,A1,A,A,A1,A,A)"
  CHARACTER(len=*), PARAMETER :: SSizeFmt20="(I2.2,':',I2.2,':00')"
  CHARACTER(len=*), PARAMETER :: SSizeFmt21="(A1,ES12.6,A1,ES12.6,A1,ES12.6,A1,ES12.6)"
  CHARACTER(len=*), PARAMETER :: SSizeFmt30="('Coinc Peak   ')"
  CHARACTER(len=*), PARAMETER :: SSizeFmt31="(A1,ES12.6,A1,ES12.6,A1,ES12.6,A1,ES12.6)"
  CHARACTER(len=*), PARAMETER :: SSizeFmt40="('NonCoinc Peak')"
  CHARACTER(len=*), PARAMETER :: SSizeFmt41="(A1,ES12.6,A1,ES12.6,A1,ES12.6,A1,ES12.6 )"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: AirLoopNum ! primary air system index
  INTEGER :: TimeStepInDay      ! zone time step in day
  INTEGER :: TimeStepIndex      ! zone time step index
  INTEGER :: NumZonesHeated     ! number of zones heated by a system
  INTEGER :: NumZonesCooled     ! numberof zones cooled by a system
  INTEGER :: ZonesHeatedNum     ! loop index of zones heated in a system
  INTEGER :: ZonesCooledNum     ! loop index of zones cooled in a system
  INTEGER :: CtrlZoneNum        ! controlled zone number of a zone
  INTEGER :: I                  ! write statement index
!  REAL(r64)    :: HourFrac           ! fractional hour
  INTEGER :: NumOfTimeStepInDay ! number of zone time steps in a day
  REAL(r64)    :: SysCoolRetTemp     ! system cooling return temperature for a time step [C]
  REAL(r64)    :: SysHeatRetTemp     ! system heating return temperature for a time step [C]
  REAL(r64)    :: RhoAir             ! density of air kg/m3
  REAL(r64)    :: OutAirFrac         ! outside air fraction
  REAL(r64)    :: SysCoolMixTemp     ! system cooling mixed air temperature [C]
  REAL(r64)    :: SysHeatMixTemp     ! system heating mixed air temperature [C]
  REAL(r64)    :: SysSensCoolCap     ! system sensible cooling capacity [W]
  REAL(r64)    :: SysHeatCap         ! system heating capacity [W]
  INTEGER :: HourCounter        ! Hour Counter
  INTEGER :: TimeStepCounter    ! Time Step Counter
  INTEGER :: Minutes            ! Current Minutes Counter
  INTEGER :: HourPrint          ! Hour to print (timestamp)
  INTEGER :: DDNum              ! design day index
  INTEGER :: CoolDDNum          ! design day index of a peak cooling day
  INTEGER :: HeatDDNum          ! design day index of a peak cooling day
  INTEGER :: CoolTimeStepNum    ! time step index (in day) of a cooling peak
  INTEGER :: HeatTimeStepNum    ! time step index (in day) of a cooling peak
  REAL(r64)    :: OutAirTemp         ! outside air temperature
  REAL(r64)    :: OutAirHumRat       ! outside air humifity ratio
  REAL(r64)    :: SysCoolMixHumRat   ! system cooling mixed air humidity ratio [kg water/kg dry air]
  REAL(r64)    :: SysCoolRetHumRat   ! system coolingreturn air humifity ratio [kg water/kg dry air]
  REAL(r64)    :: SysHeatMixHumRat   ! system heating mixed air humidity ratio [kg water/kg dry air]
  REAL(r64)    :: SysHeatRetHumRat   ! system heatingreturn air humifity ratio [kg water/kg dry air]
  REAL(r64)    :: SysCoolOutTemp     ! system cooling outside air temperature [C]
  REAL(r64)    :: SysCoolOutHumRat   ! system cooling outside air humidity ratio [kg water/kg dry air]
  REAL(r64)    :: SysHeatOutTemp     ! system heating outside air temperature [C]
  REAL(r64)    :: SysHeatOutHumRat   ! system heating outside air humidity ratio [kg water/kg dry air]
  REAL(r64)    :: SysCoolSizingRat   ! ratio of user input design flow for cooling divided by calculated design cooling flow
  REAL(r64)    :: SysHeatSizingRat   ! ratio of user input design flow for heating divided by calculated design heating flow
  REAL(r64)    :: ZoneOARatio        ! ratio of zone OA flow to zone design cooling or heating flow
  REAL(r64)    :: RetTempRise        ! difference between zone return temperature and zone temperature [delta K]
  REAL(r64)    :: SysCoolingEv       ! System level ventilation effectiveness for cooling mode
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: EvBySysCool !saved value of SysCoolingEv used in 62.1 tabular report
  REAL(r64)    :: SysHeatingEv       ! System level ventilation effectiveness for heating mode
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: EvBySysHeat !saved value of SysHeatingEv used in 62.1 tabular report
  REAL(r64) :: Ep = 1.0d0                ! zone primary air fraction
  REAL(r64) :: Er = 0.0d0                ! zone secondary recirculation fraction
  REAL(r64) :: Fa = 1.0d0                ! temporary variable used in multi-path VRP calc
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: FaByZoneCool  !saved value of Fa used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: FaByZoneHeat  !saved value of Fa used in 62.1 tabular report
  REAL(r64) :: Fb = 1.0d0                ! temporary variable used in multi-path VRP calc
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: FbByZoneCool  !saved value of Fb used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: FbByZoneHeat  !saved value of Fb used in 62.1 tabular report
  REAL(r64) :: Fc = 1.0d0                ! temporary variable used in multi-path VRP calc
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: FcByZoneCool  !saved value of Fc used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: FcByZoneHeat  !saved value of Fc used in 62.1 tabular report
  REAL(r64) :: Xs = 1.0d0                ! uncorrected system outdoor air fraction
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: XsBySysCool !saved value of Xs used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: XsBySysHeat !saved value of Xs used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: EvzByZoneCool !saved value of Evz (zone vent effy) used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: EvzByZoneHeat !saved value of Evz (zone vent effy) used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: EvzByZoneCoolPrev !saved value of Evz (zone vent effy) used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: EvzByZoneHeatPrev !saved value of Evz (zone vent effy) used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE  :: VotClgBySys  !saved value of cooling ventilation required at primary AHU, used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE  :: VotHtgBySys  !saved value of heating ventilation required at primary AHU, used in 62.1 tabular report
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE  :: VozSumClgBySys  !saved value of cooling ventilation required at clg zones
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE  :: VozSumHtgBySys  !saved value of cooling ventilation required at htg zones
  REAL(r64) :: Evz = 1.0d0               ! zone ventilation efficiency
  REAL(r64) :: MinHeatingEvz = 1.0d0     ! minimum zone ventilation efficiency for heating (to be used as system efficiency)
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: EvzMinBySysHeat !saved value of EvzMin used in 62.1 tabular report
  REAL(r64) :: MinCoolingEvz = 1.0d0     ! minimum zone ventilation efficiency for cooling (to be used as system efficiency)
  REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: EvzMinBySysCool !saved value of EvzMin used in 62.1 tabular report
  REAL(r64) :: ZoneOAFrac = 0.0d0        ! zone OA fraction
  REAL(r64) :: ZoneEz = 1.0d0            ! zone air distribution effectiveness
  REAL(r64) :: Vou = 0.0D0             ! Uncorrected outdoor air intake for all zones per ASHRAE std 62.1
  REAL(r64) :: Vot = 0.0D0             ! Required outdoor air intake at primary AHU per ASHRAE std 62.1
  REAL(r64) :: VotMax = 0.0D0          ! Max of required cooling/heating outdoor air intake at primary AHU per ASHRAE std 62.1
  REAL(r64) :: VozBySys = 0.0D0        ! Sum of zone required outdoor air intake per ASHRAE std 62.1
  REAL(r64) :: Ratio   = 1D0           ! Ratio of VozBySys / VotMax
! not changing this for the unitary system check in
!  REAL(r64) :: Ratio   = 1.0d0           ! Ratio of VozBySys / VotMax
  REAL(r64) :: SysHtgPeakAirflow       ! Peak heating airflow
  INTEGER   :: NumZonesForHtg          ! Number of heating zones for given primary system
  INTEGER   :: MatchingCooledZoneNum   ! temporary variable
  real(r64) :: termunitsizingtempfrac  ! 1.0/(1.0+termunitsizing(ctrlzone)%inducrat)
  real(r64) :: termunitsizingtemp      ! (1.0+termunitsizing(ctrlzone)%inducrat)

  NumOfTimeStepInDay = NumOfTimeStepInHour * 24
!  NumZonesCooled=0
!  NumZonesHeated=0

  ! allocate arrays used to store values for standard 62.1 tabular report
  IF (.NOT. ALLOCATED(FaByZoneCool)) THEN
    ALLOCATE (FaByZoneCool(NumOfZones))
    FaByZoneCool = 0.0d0
    ALLOCATE (FaByZoneHeat(NumOfZones))
    FaByZoneHeat = 0.0d0
    ALLOCATE (FbByZoneCool(NumOfZones))
    FbByZoneCool = 0.0d0
    ALLOCATE (FbByZoneHeat(NumOfZones))
    FbByZoneHeat = 0.0d0
    ALLOCATE (FcByZoneCool(NumOfZones))
    FcByZoneCool = 0.0d0
    ALLOCATE (FcByZoneHeat(NumOfZones))
    FcByZoneHeat = 0.0d0
    ALLOCATE (EvBySysCool(NumPrimaryAirSys))
    EvBySysCool = 1.0d0
    ALLOCATE (EvBySysHeat(NumPrimaryAirSys))
    EvBySysHeat = 1.0d0
    ALLOCATE (XsBySysCool(NumPrimaryAirSys))
    XsBySysCool = 1.0d0
    ALLOCATE (XsBySysHeat(NumPrimaryAirSys))
    XsBySysHeat = 1.0d0
    ALLOCATE (EvzByZoneCool(NumOfZones))
    EvzByZoneCool = 1.0d0
    ALLOCATE (EvzByZoneCoolPrev(NumOfZones))
    EvzByZoneCoolPrev = 1.0d0
    ALLOCATE (EvzByZoneHeat(NumOfZones))
    EvzByZoneHeat = 1.0d0
    ALLOCATE (EvzByZoneHeatPrev(NumOfZones))
    EvzByZoneHeatPrev = 1.0d0
    ALLOCATE (EvzMinBySysCool(NumPrimaryAirSys))
    EvzMinBySysCool = 1.0d0
    ALLOCATE (EvzMinBySysHeat(NumPrimaryAirSys))
    EvzMinBySysHeat = 1.0d0
    ALLOCATE (VotClgBySys(NumPrimaryAirSys))
    VotClgBySys = 0.0d0
    ALLOCATE (VotHtgBySys(NumPrimaryAirSys))
    VotHtgBySys = 0.0d0
    ALLOCATE (VozSumClgBySys(NumPrimaryAirSys))
    VozSumClgBySys = 0.0d0
    ALLOCATE (VozSumHtgBySys(NumPrimaryAirSys))
    VozSumHtgBySys = 0.0d0
  END IF

  SELECT CASE (CallIndicator)

    CASE (BeginDay)

      ! Correct the zone return temperature in ZoneSizing for the case of induction units. The calc in
      ! ZoneEquipmentManager assumes all the air entering the zone goes into the return node.
      DO CtrlZoneNum=1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        termunitsizingtemp=(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat)
        termunitsizingtempfrac=(1.0d0/termunitsizingtemp)
        RetTempRise = ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtCoolPeak - &
                        ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtCoolPeak
        IF (RetTempRise > 0.01d0) THEN
!avoid possible compiler bug
!          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtCoolPeak = &
!            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtCoolPeak + RetTempRise * &
!           (1.d0/(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat))
          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtCoolPeak = &
            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtCoolPeak +   &
            RetTempRise * termunitsizingtempfrac
        END IF
        RetTempRise = ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtHeatPeak - &
                        ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtHeatPeak
        IF (RetTempRise > 0.01d0) THEN
!avoid possible compiler bug
!          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtHeatPeak = &
!            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtHeatPeak + RetTempRise * &
!            (1./(1.+TermUnitSizing(CtrlZoneNum)%InducRat))
          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtHeatPeak = &
            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtHeatPeak +   &
            RetTempRise * termunitsizingtempfrac
        END IF
      END DO

      DO AirLoopNum=1,NumPrimaryAirSys ! start of begin day loop over primary air systems

        NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
        NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
        SysSizing(AirLoopNum,CurOverallSimDay)%CoolDesDay = EnvironmentName
        SysSizing(AirLoopNum,CurOverallSimDay)%HeatDesDay = EnvironmentName

        DO ZonesCooledNum=1,NumZonesCooled ! loop over cooled zones
          CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
          SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinCoolMassFlow = &
            SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinCoolMassFlow + &
            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesCoolMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
        END DO ! end of loop over cooled zones

        IF (NumZonesHeated.GT.0) THEN ! if there are zones supplied with central hot air
          DO ZonesHeatedNum=1,NumZonesHeated ! loop over heated zones
            CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
            SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinHeatMassFlow = &
              SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinHeatMassFlow + &
              ZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          END DO ! end of loop over heated zones
        ELSE                          ! otherwise use cool supply zones
          DO ZonesCooledNum=1,NumZonesCooled !loop over cooled zones
            CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
            SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinHeatMassFlow = &
              SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinHeatMassFlow + &
              ZoneSizing(CtrlZoneNum,CurOverallSimDay)%DesHeatMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          END DO ! end of loop over cooled zones
        END IF ! End of heat / cool zone if - else

      END DO ! End of begin day loop over primary air systems

    CASE (DuringDay)

      TimeStepInDay = (HourOfDay-1)*NumOfTimeStepInHour + TimeStep ! calculate current zone time step index

      ! Correct the zone return temperature in ZoneSizing for the case of induction units. The calc in
      ! ZoneEquipmentManager assumes all the air entering the zone goes into the return node.
      DO CtrlZoneNum=1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        termunitsizingtemp=(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat)
        termunitsizingtempfrac=(1.0d0/termunitsizingtemp)
        RetTempRise = ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq(TimeStepInDay) - &
                        ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneTempSeq(TimeStepInDay)
        IF (RetTempRise > 0.01d0) THEN
!avoid possible compiler bug
!          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq(TimeStepInDay) = &
!            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneTempSeq(TimeStepInDay) + RetTempRise * &
!           (1.d0/(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat))
          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq(TimeStepInDay) = &
            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneTempSeq(TimeStepInDay) +   &
            RetTempRise * termunitsizingtempfrac
        END IF
        RetTempRise = ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq(TimeStepInDay) - &
                        ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneTempSeq(TimeStepInDay)
        IF (RetTempRise > 0.01d0) THEN
!avoid possible compiler bug
!          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq(TimeStepInDay) = &
!            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneTempSeq(TimeStepInDay) + RetTempRise * &
!           (1.d0/(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat))
          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq(TimeStepInDay) = &
            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneTempSeq(TimeStepInDay) +   &
            RetTempRise * termunitsizingtempfrac
        END IF
      END DO

      DO AirLoopNum=1,NumPrimaryAirSys ! start of zone time step loop over primary air systems

        NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
        NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated

        SysCoolRetTemp = 0.0d0
        OutAirFrac = 0.0d0
        SysCoolMixTemp = 0.0d0
        SysSensCoolCap = 0.0d0
        SysCoolRetHumRat = 0.0d0
        SysCoolMixHumRat = 0.0d0

        DO ZonesCooledNum=1,NumZonesCooled ! loop over zones cooled by central system
          CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
          ! sum up the system mass flow rate for this time step
          SysSizing(AirLoopNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay) = &
            SysSizing(AirLoopNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay) + &
            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay) / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          ! calculate the return air temperature for this time step
          SysCoolRetTemp = SysCoolRetTemp + ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq(TimeStepInDay) * &
                                      ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay) &
                                       / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          SysCoolRetHumRat = SysCoolRetHumRat + ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneHumRatSeq(TimeStepInDay) * &
                                      ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay) &
                                       / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
        END DO ! end of loop over zones cooled by central system
        ! check that there is system mass flow
        IF (SysSizing(AirLoopNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay) > 0.0d0) THEN
          ! complete return air temp calc
          SysCoolRetTemp = SysCoolRetTemp / SysSizing(AirLoopNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay)
          SysCoolRetHumRat = SysCoolRetHumRat / SysSizing(AirLoopNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay)
          SysSizing(AirLoopNum,CurOverallSimDay)%SysCoolRetTempSeq(TimeStepInDay) = SysCoolRetTemp
          SysSizing(AirLoopNum,CurOverallSimDay)%SysCoolRetHumRatSeq(TimeStepInDay) = SysCoolRetHumRat
          ! calculate the outside air fraction for this time step
          RhoAir = StdRhoAir
          IF (SysSizing(AirLoopNum,CurOverallSimDay)%CoolOAOption == MinOA) THEN
            OutAirFrac = RhoAir*SysSizing(AirLoopNum,CurOverallSimDay)%DesOutAirVolFlow / &
                         SysSizing(AirLoopNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay)
            OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
          ELSE
            OutAirFrac = 1.0d0
          END IF
          ! now calculate the mixed air temperature
          SysCoolMixTemp = OutDryBulbTemp*OutAirFrac + SysCoolRetTemp*(1.0d0-OutAirFrac)
          SysCoolMixHumRat = OutHumRat*OutAirFrac + SysCoolRetHumRat*(1.0d0-OutAirFrac)
          SysSizing(AirLoopNum,CurOverallSimDay)%SysCoolOutTempSeq(TimeStepInDay) = OutDryBulbTemp
          SysSizing(AirLoopNum,CurOverallSimDay)%SysCoolOutHumRatSeq(TimeStepInDay) = OutHumRat
          ! From the mixed air temp, system design supply air temp, and the mass flow rate
          ! calculate the system sensible cooling capacity
          SysSensCoolCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) *   &
                              SysSizing(AirLoopNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay) * &
                             (SysCoolMixTemp - SysSizing(AirLoopNum,CurOverallSimDay)%CoolSupTemp)
          SysSensCoolCap = MAX(0.0d0,SysSensCoolCap)
          ! Save the sens cool cap for this time step
          SysSizing(AirLoopNum,CurOverallSimDay)%SensCoolCapSeq(TimeStepInDay) = SysSensCoolCap
        END IF ! end of system mass flow check

        ! get the maximum system sensible cooling capacity
        IF (SysSensCoolCap > SysSizing(AirLoopNum,CurOverallSimDay)%SensCoolCap) THEN
          SysSizing(AirLoopNum,CurOverallSimDay)%SensCoolCap = SysSensCoolCap
          SysSizing(AirLoopNum,CurOverallSimDay)%CoolMixTemp = SysCoolMixTemp
          SysSizing(AirLoopNum,CurOverallSimDay)%CoolMixHumRat = SysCoolMixHumRat
          SysSizing(AirLoopNum,CurOverallSimDay)%CoolRetTemp = SysCoolRetTemp
          SysSizing(AirLoopNum,CurOverallSimDay)%CoolRetHumRat = SysCoolRetHumRat
          SysSizing(AirLoopNum,CurOverallSimDay)%CoolOutTemp = OutDryBulbTemp
          SysSizing(AirLoopNum,CurOverallSimDay)%CoolOutHumRat = OutHumRat
        END IF
        ! get the maximum cooling mass flow rate
        SysSizing(AirLoopNum,CurOverallSimDay)%CoinCoolMassFlow = MAX(SysSizing(AirLoopNum,CurOverallSimDay)%CoinCoolMassFlow, &
                                                   SysSizing(AirLoopNum,CurOverallSimDay)%CoolFlowSeq(TimeStepInDay))

        SysHeatRetTemp = 0.0d0
        OutAirFrac = 0.0d0
        SysHeatMixTemp = 0.0d0
        SysHeatCap = 0.0d0
        SysHeatRetHumRat = 0.0d0
        SysHeatMixHumRat = 0.0d0

        IF (NumZonesHeated.GT.0) THEN ! IF there are centrally heated zones

          DO ZonesHeatedNum=1,NumZonesHeated ! loop over the heated zones
            CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
            ! sum up the heating mass flow rate for this time step
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) = &
              SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) + &
              ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            ! calculate the return air temperature for this time step
            SysHeatRetTemp = SysHeatRetTemp + ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq(TimeStepInDay) * &
                                          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) &
                                           / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            SysHeatRetHumRat = SysHeatRetHumRat + ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneHumRatSeq(TimeStepInDay) * &
                                          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) &
                                           / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          END DO ! end heated zones loop
          ! check that the system flow rate is nonzero
          IF (SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) > 0.0d0) THEN
            ! complete return air temp calc
            SysHeatRetTemp = SysHeatRetTemp / SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay)
            SysHeatRetHumRat = SysHeatRetHumRat / SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay)
            SysSizing(AirLoopNum,CurOverallSimDay)%SysHeatRetTempSeq(TimeStepInDay) = SysHeatRetTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%SysHeatRetHumRatSeq(TimeStepInDay) = SysHeatRetHumRat
            ! calculate the outside air fraction for this time step
            RhoAir = StdRhoAir
            IF (SysSizing(AirLoopNum,CurOverallSimDay)%HeatOAOption == MinOA) THEN
              OutAirFrac = RhoAir*SysSizing(AirLoopNum,CurOverallSimDay)%DesOutAirVolFlow / &
                             SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay)
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            ELSE
              OutAirFrac = 1.0d0
            END IF
            ! calculate the mixed air temperature
            SysHeatMixTemp = OutDryBulbTemp*OutAirFrac + SysHeatRetTemp*(1.0d0-OutAirFrac)
            SysHeatMixHumRat = OutHumRat*OutAirFrac + SysHeatRetHumRat*(1.0d0-OutAirFrac)
            SysSizing(AirLoopNum,CurOverallSimDay)%SysHeatOutTempSeq(TimeStepInDay) = OutDryBulbTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%SysHeatOutHumRatSeq(TimeStepInDay) = OutHumRat
            ! From the mixed air temp, heating supply air temp, and mass flow rate calculate the system heating capacity
            SysHeatCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) *   &
                           SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) * &
                           ( SysSizing(AirLoopNum,CurOverallSimDay)%HeatSupTemp - SysHeatMixTemp )
            SysHeatCap = MAX(0.0d0,SysHeatCap)
            ! save the system heating capacity for the time step
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatCapSeq(TimeStepInDay) = SysHeatCap
          END IF ! end system flow rate IF

          ! Get the maximum system heating capacity
          IF (SysHeatCap > SysSizing(AirLoopNum,CurOverallSimDay)%HeatCap) THEN
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatCap = SysHeatCap
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatMixTemp = SysHeatMixTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatMixHumRat = SysHeatMixHumRat
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatRetTemp = SysHeatRetTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatRetHumRat = SysHeatRetHumRat
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatOutTemp = OutDryBulbTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatOutHumRat = OutHumRat
          END IF
          ! Get the maximum system heating flow rate
          SysSizing(AirLoopNum,CurOverallSimDay)%CoinHeatMassFlow = &
                                                     MAX(SysSizing(AirLoopNum,CurOverallSimDay)%CoinHeatMassFlow, &
                                                     SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay))

        ELSE ! No centrally heated zones: use cooled zones

          DO ZonesCooledNum=1,NumZonesCooled ! loop over the cooled zones
            CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
              ! sum up the heating mass flow rate for this time step
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) = &
              SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) + &
              ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            ! calculate the return air temperature for this time step
            SysHeatRetTemp = SysHeatRetTemp + ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneRetTempSeq(TimeStepInDay) * &
                                          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) &
                                           / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            SysHeatRetHumRat = SysHeatRetHumRat + ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatZoneHumRatSeq(TimeStepInDay) * &
                                          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) &
                                           / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          END DO ! end of cooled zones loop

          IF (SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) > 0.0d0) THEN
            ! complete return air temp calc
            SysHeatRetTemp = SysHeatRetTemp / SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay)
            SysHeatRetHumRat = SysHeatRetHumRat / SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay)
            SysSizing(AirLoopNum,CurOverallSimDay)%SysHeatRetTempSeq(TimeStepInDay) = SysHeatRetTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%SysHeatRetHumRatSeq(TimeStepInDay) = SysHeatRetHumRat
            ! calculate the outside air fraction for this time step
            RhoAir = StdRhoAir
            IF (SysSizing(AirLoopNum,CurOverallSimDay)%HeatOAOption == MinOA) THEN
              OutAirFrac = RhoAir*SysSizing(AirLoopNum,CurOverallSimDay)%DesOutAirVolFlow / &
                             SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay)
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            ELSE
              OutAirFrac = 1.0d0
            END IF
            ! calculate the mixed air temperature
            SysHeatMixTemp = OutDryBulbTemp*OutAirFrac + SysHeatRetTemp*(1.0d0-OutAirFrac)
            SysHeatMixHumRat = OutHumRat*OutAirFrac + SysHeatRetHumRat*(1.0d0-OutAirFrac)
            SysSizing(AirLoopNum,CurOverallSimDay)%SysHeatOutTempSeq(TimeStepInDay) = OutDryBulbTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%SysHeatOutHumRatSeq(TimeStepInDay) = OutHumRat
            ! From the mixed air temp, heating supply air temp, and mass flow rate calculate the system heating capacity
            SysHeatCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) *   &
                           SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay) * &
                           ( SysSizing(AirLoopNum,CurOverallSimDay)%HeatSupTemp - SysHeatMixTemp )
            SysHeatCap = MAX(0.0d0,SysHeatCap)
            ! save the system heating capacity for the time step
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatCapSeq(TimeStepInDay) = SysHeatCap
          END IF ! end system flow rate IF

          ! Get the maximum system heating capacity
          IF (SysHeatCap > SysSizing(AirLoopNum,CurOverallSimDay)%HeatCap) THEN
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatCap = SysHeatCap
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatMixTemp = SysHeatMixTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatMixHumRat = SysHeatMixHumRat
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatRetTemp = SysHeatRetTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatRetHumRat = SysHeatRetHumRat
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatOutTemp = OutDryBulbTemp
            SysSizing(AirLoopNum,CurOverallSimDay)%HeatOutHumRat = OutHumRat
          END IF          ! Get the maximum system heating flow rate
          SysSizing(AirLoopNum,CurOverallSimDay)%CoinHeatMassFlow = &
                                                     MAX(SysSizing(AirLoopNum,CurOverallSimDay)%CoinHeatMassFlow, &
                                                     SysSizing(AirLoopNum,CurOverallSimDay)%HeatFlowSeq(TimeStepInDay))
        END IF

      END DO ! end of loop over primary air systems

    CASE (EndDay)

      ! Get design flows
      SysCoolingEv = 1.0d0
      SysHeatingEv = 1.0d0
      DO AirLoopNum=1,NumPrimaryAirSys

        NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
        NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated

        SELECT CASE(SysSizing(AirLoopNum,CurOverallSimDay)%SizingOption)
          CASE(Coincident)
            IF (FinalSysSizing(AirLoopNum)%SystemOAMethod == SOAM_ZoneSum) THEN
              SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow = SysSizing(AirLoopNum,CurOverallSimDay)%CoinCoolMassFlow / &
                                                     StdRhoAir
              SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow = SysSizing(AirLoopNum,CurOverallSimDay)%CoinHeatMassFlow / &
                                                     StdRhoAir
            ELSEIF (FinalSysSizing(AirLoopNum)%SystemOAMethod == SOAM_VRP) THEN ! Ventilation Rate Procedure
              ! cooling
              SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow = SysSizing(AirLoopNum,CurOverallSimDay)%CoinCoolMassFlow / &
                                                     StdRhoAir
              IF (SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow > 0) THEN
                OutAirFrac = SysSizing(AirLoopNum,CurOverallSimDay)%DesOutAirVolFlow / &
                             SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow
              ELSE
                OutAirFrac = 0.0d0
              ENDIF
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
              IF (SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow > 0) THEN
                Xs = MIN(1.0d0,FinalSysSizing(AirLoopNum)%SysUncOA / SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow)
              ELSE
                Xs = 0.0d0
              ENDIF
              IF (FinalSysSizing(AirLoopNum)%OAAutosized .AND. SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow > 0) THEN
                NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
                MinCoolingEvz = 1.0d0
                VozSumClgBySys(AirLoopNum) = 0.0d0
                DO ZonesCooledNum=1,NumZonesCooled
                  CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)

                  ! Zone air secondary recirculation fraction
                  Er = FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation
                  Ep = FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFraction
                  ZoneOAFrac = FinalZoneSizing(CtrlZoneNum)%ZpzClgByZone
                  ZoneEz = FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling
                  IF (Er > 0.0d0) THEN
                    ! multi-path ventilation system using VRP
                    Fa = Ep + (1.0d0 - Ep) * Er
                    Fb = Ep
                    Fc = 1.0d0 - (1.0d0 - ZoneEz)*(1.0d0 - Er)*(1.0d0 - Ep)
                    !save Fa Fb and Fc for standard 62.1 report
                    FaByZoneCool(CtrlZoneNum) = Fa
                    FbByZoneCool(CtrlZoneNum) = Fb
                    FcByZoneCool(CtrlZoneNum) = Fc

                    ! Calc zone ventilation efficiency
                    IF (Fa > 0.0d0) THEN
                      SysCoolingEv = 1.0d0 + Xs * Fb / Fa - ZoneOAFrac * Ep * Fc / Fa
                    ELSE
                      SysCoolingEv = 1.0d0
                    ENDIF

                  ELSE
                    ! single-path ventilation system
                    SysCoolingEv = 1.0d0 + Xs - ZoneOAFrac
                  ENDIF
                  IF (SysCoolingEv < MinCoolingEvz) MinCoolingEvz = SysCoolingEv
                  EvzByZoneCoolPrev(CtrlZoneNum) = EvzByZoneCool(CtrlZoneNum)    ! Save previous EvzByZoneCool
                  EvzByZoneCool(CtrlZoneNum) = SysCoolingEv
                  VozSumClgBySys(AirLoopNum) = VozSumClgBySys(AirLoopNum) + FinalZoneSizing(CtrlZoneNum)%VozClgByZone
                END DO

                IF (MinCoolingEvz > 0) THEN
                  ! (However, I don't think people diversity can be done correctly in E+ Sizing so assuming D=1 in this equation
                  !Vou = Diversity*(Rp*Pz) + Ra*Az
                  Vou = FinalSysSizing(AirLoopNum)%SysUncOA
                  Vot = Vou / MinCoolingEvz
                  IF (Vot > VotClgBySys(AirLoopNum)) THEN
                    !This might be the cooling design day so only update if Vot is larger than the previous
                    VotClgBySys(AirLoopNum) = Vot
                    XsBySysCool(AirLoopNum) = Xs
                    EvzMinBySysCool(AirLoopNum) = MinCoolingEvz
                  ELSE
                    !Restore EvzByZoneCool() since it was reset by the current (but not highest Vot) design day
                    DO ZonesCooledNum=1,NumZonesCooled
                      CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
                      EvzByZoneCool(CtrlZoneNum)= EvzByZoneCoolPrev(CtrlZoneNum)
                    ENDDO
                  ENDIF
                END IF
              ENDIF

              ! heating
              SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow =   &
                  SysSizing(AirLoopNum,CurOverallSimDay)%CoinHeatMassFlow / StdRhoAir
              IF (SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow > 0) THEN
                OutAirFrac = SysSizing(AirLoopNum,CurOverallSimDay)%DesOutAirVolFlow / &
                                  SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow
              ELSE
                OutAirFrac = 0.0d0
              END IF
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))

              ! This is a bit of a cludge. If the design zone heating airflows were increased due to
              ! the MaxZoneOaFraction, then the SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow
              ! variable will be out of sync with the
              IF (FinalSysSizing(AirLoopNum)%MaxZoneOAFraction > 0 .AND. &
                  FinalSysSizing(AirLoopNum)%HeatAirDesMethod == FromDDCalc) THEN
                SysHtgPeakAirflow = 0.0d0
                IF (NumZonesHeated > 0) THEN
                  DO ZonesHeatedNum=1,NumZonesHeated
                    CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
                    SysHtgPeakAirflow = SysHtgPeakAirflow + FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
                  ENDDO
                ELSE
                  DO ZonesHeatedNum=1,NumZonesCooled
                    CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedNum)
                    SysHtgPeakAirflow = SysHtgPeakAirflow + FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
                  ENDDO
                ENDIF
              ELSE
                SysHtgPeakAirflow = SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow
              ENDIF

              IF (SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow > 0) THEN
                ! SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow may be out of sync with FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
                Xs = MIN(1.0d0,FinalSysSizing(AirLoopNum)%SysUncOA / &
                    MAX(SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow, SysHtgPeakAirflow))
              ELSE
                Xs = 0.0d0
              END IF

              IF (FinalSysSizing(AirLoopNum)%OAAutosized .AND. SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow > 0) THEN
                NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
                MinHeatingEvz = 1.0d0
                VozSumHtgBySys(AirLoopNum) = 0.0d0
                IF (NumZonesHeated > 0) THEN
                  DO ZonesHeatedNum=1,NumZonesHeated
                    CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
                    MatchingCooledZoneNum = FindNumberinList(CtrlZoneNum,AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums, &
                                                 NumZonesCooled)
                    IF (MatchingCooledZoneNum == 0) THEN
                      ! Zone air secondary recirculation fraction
                      Er = FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation
                      Ep = FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFractionHtg
                      ZoneOAFrac = FinalZoneSizing(CtrlZoneNum)%ZpzHtgByZone
                      ZoneEz = FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating
                      IF (Er > 0.0d0) THEN
                        ! multi-path ventilation system using VRP
                        Fa = Ep + (1.0d0 - Ep) * Er
                        Fb = Ep
                        Fc = 1.0d0 - (1.0d0 - ZoneEz)*(1.0d0 - Er)*(1.0d0 - Ep)
                        !save Fa Fb and Fc for standard 62.1 report
                        FaByZoneHeat(CtrlZoneNum) = Fa
                        FbByZoneHeat(CtrlZoneNum) = Fb
                        FcByZoneHeat(CtrlZoneNum) = Fc

                        ! Calc zone ventilation efficiency
                        IF (Fa > 0.0d0) THEN
                          SysHeatingEv = 1.0d0 + Xs * Fb / Fa - ZoneOAFrac * Ep * Fc / Fa
                        ELSE
                          SysHeatingEv = 1.0d0
                        ENDIF
                      ELSE
                        ! single-path ventilation system
                        SysHeatingEv = 1.0d0 + Xs - ZoneOAFrac
                      ENDIF
                      IF (SysHeatingEv < MinHeatingEvz) MinHeatingEvz = SysHeatingEv
                      EvzByZoneHeatPrev(CtrlZoneNum) = EvzByZoneHeat(CtrlZoneNum)  ! Save previous EvzByZoneHeat
                      EvzByZoneHeat(CtrlZoneNum) = SysHeatingEv
                      VozSumHtgBySys(AirLoopNum) = VozSumHtgBySys(AirLoopNum) + FinalZoneSizing(CtrlZoneNum)%VozHtgByZone
                    END IF
                  END DO
                ELSE
                  DO ZonesHeatedNum=1,NumZonesCooled
                    CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedNum)
                    ! Zone air secondary recirculation fraction
                    Er = FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation
                    Ep = FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFractionHtg
                    ZoneOAFrac = FinalZoneSizing(CtrlZoneNum)%ZpzHtgByZone
                    ZoneEz = FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating
                    IF (Er > 0.0d0) THEN
                      ! multi-path ventilation system using VRP
                      Fa = Ep + (1.0d0 - Ep) * Er
                      Fb = Ep
                      Fc = 1.0d0 - (1.0d0 - ZoneEz)*(1.0d0 - Er)*(1.0d0 - Ep)
                      !save Fa Fb and Fc for standard 62.1 report
                      FaByZoneHeat(CtrlZoneNum) = Fa
                      FbByZoneHeat(CtrlZoneNum) = Fb
                      FcByZoneHeat(CtrlZoneNum) = Fc

                      ! Calc zone ventilation efficiency
                      IF (Fa > 0.0d0) THEN
                        SysHeatingEv = 1.0d0 + Xs * Fb / Fa - ZoneOAFrac * Ep * Fc / Fa
                      ELSE
                        SysHeatingEv = 1.0d0
                      ENDIF
                    ELSE
                      ! single-path ventilation system
                      SysHeatingEv = 1.0d0 + Xs - ZoneOAFrac
                    ENDIF
                    IF (SysHeatingEv < MinHeatingEvz) MinHeatingEvz = SysHeatingEv
                    EvzByZoneHeatPrev(CtrlZoneNum) = EvzByZoneHeat(CtrlZoneNum)   ! Save previous EvzByZoneHeat
                    EvzByZoneHeat(CtrlZoneNum) = SysHeatingEv
                    VozSumHtgBySys(AirLoopNum) = VozSumHtgBySys(AirLoopNum) + FinalZoneSizing(CtrlZoneNum)%VozHtgByZone
                  END DO
                END IF

                IF (MinHeatingEvz > 0) THEN
                  ! Std 62.1-2010, section 6.2.5.4: Eq. 6.6
                  ! (However, I don't think people diversity can be done correctly in E+ Sizing so assuming D=1 in this equation
                  !Vou = Diversity*(Rp*Pz) + Ra*Az
                  Vou = FinalSysSizing(AirLoopNum)%SysUncOA
                  Vot = Vou / MinHeatingEvz
                  IF (Vot > VotHtgBySys(AirLoopNum)) THEN
                    !This might be the cooling design day so only update if Vot is larger than the previous
                    VotHtgBySys(AirLoopNum) = Vot
                    XsBySysHeat(AirLoopNum) = Xs
                    EvzMinBySysHeat(AirLoopNum) = MinHeatingEvz
                  ELSE
                    !Restore EvzByZoneHeat() since it was reset by the current (but not highest Vot) design day
                    IF (NumZonesHeated > 0) THEN
                      DO ZonesHeatedNum=1,NumZonesHeated
                        CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
                        EvzByZoneHeat(CtrlZoneNum)= EvzByZoneHeatPrev(CtrlZoneNum)
                      ENDDO
                    ELSE
                      DO ZonesHeatedNum=1,NumZonesCooled
                        CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedNum)
                        EvzByZoneHeat(CtrlZoneNum)= EvzByZoneHeatPrev(CtrlZoneNum)
                      ENDDO
                    ENDIF
                  ENDIF
                END IF
              ENDIF
            ELSE  ! error
            END IF
            SysSizing(AirLoopNum,CurOverallSimDay)%DesMainVolFlow =   &
               MAX(SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow, SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow)
          CASE(NonCoincident)
            IF (FinalSysSizing(AirLoopNum)%SystemOAMethod == SOAM_ZoneSum) THEN
              SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow =   &
                   SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinCoolMassFlow / StdRhoAir
              SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow =   &
                   SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinHeatMassFlow / StdRhoAir
            ELSEIF (FinalSysSizing(AirLoopNum)%SystemOAMethod == SOAM_VRP) THEN ! Ventilation Rate Procedure
              ! cooling
              SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow =   &
                 SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinCoolMassFlow / StdRhoAir
              IF (SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow > 0) THEN
                OutAirFrac = SysSizing(AirLoopNum,CurOverallSimDay)%DesOutAirVolFlow / &
                                         SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow
              ELSE
                OutAirFrac = 0.0d0
              END IF
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))

              IF (SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow > 0) THEN
                Xs = MIN(1.0d0,FinalSysSizing(AirLoopNum)%SysUncOA / SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow)
              ELSE
                Xs = 0.0d0
              ENDIF
              IF (FinalSysSizing(AirLoopNum)%OAAutosized .AND. SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow > 0) THEN
                NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
                MinCoolingEvz = 1.0d0
                VozSumClgBySys(AirLoopNum) = 0.0d0
                DO ZonesCooledNum=1,NumZonesCooled
                  CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)

                  ! Zone air secondary recirculation fraction
                  Er = FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation
                  Ep = FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFraction
                  ZoneOAFrac = FinalZoneSizing(CtrlZoneNum)%ZpzClgByZone
                  ZoneEz = FinalZoneSizing(CtrlZoneNum)%ZoneADEffCooling
                  IF (Er > 0.0d0) THEN
                    ! multi-path ventilation system using VRP
                    Fa = Ep + (1.0d0 - Ep) * Er
                    Fb = Ep
                    Fc = 1.0d0 - (1.0d0 - ZoneEz)*(1.0d0 - Er)*(1.0d0 - Ep)
                    !save Fa Fb and Fc for standard 62.1 report
                    FaByZoneCool(CtrlZoneNum) = Fa
                    FbByZoneCool(CtrlZoneNum) = Fb
                    FcByZoneCool(CtrlZoneNum) = Fc

                    ! Calc zone ventilation efficiency
                    IF (Fa > 0.0d0) THEN
                      SysCoolingEv = 1.0d0 + Xs * Fb / Fa - ZoneOAFrac * Ep * Fc / Fa
                    ELSE
                      SysCoolingEv = 1.0d0
                    ENDIF
                  ELSE
                    ! single-path ventilation system
                    SysCoolingEv = 1.0d0 + Xs - ZoneOAFrac
                  END IF
                  IF (SysCoolingEv < MinCoolingEvz) MinCoolingEvz = SysCoolingEv
                  EvzByZoneCoolPrev(CtrlZoneNum) = EvzByZoneCool(CtrlZoneNum)
                  EvzByZoneCool(CtrlZoneNum) = SysCoolingEv
                  VozSumClgBySys(AirLoopNum) = VozSumClgBySys(AirLoopNum) + FinalZoneSizing(CtrlZoneNum)%VozClgByZone
                END DO

                IF (MinCoolingEvz > 0) THEN
                  ! Std 62.1-2010, section 6.2.5.4: Eq. 6.6
                  ! (However, I don't think people diversity can be done correctly in E+ Sizing so assuming D=1 in this equation
                  !Vou = Diversity*(Rp*Pz) + Ra*Az
                  Vou = FinalSysSizing(AirLoopNum)%SysUncOA
                  Vot = Vou / MinCoolingEvz
                  IF (Vot > VotClgBySys(AirLoopNum)) THEN
                    !This might be the cooling design day so only update if Vot is larger than the previous
                    VotClgBySys(AirLoopNum) = Vot
                    XsBySysCool(AirLoopNum) = Xs
                    EvzMinBySysCool(AirLoopNum) = MinCoolingEvz
                  ELSE
                    !Restore EvzByZoneCool() since it was reset by the current (but not highest Vot) design day
                    DO ZonesCooledNum=1,NumZonesCooled
                      CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
                      EvzByZoneCool(CtrlZoneNum)= EvzByZoneCoolPrev(CtrlZoneNum)
                    ENDDO
                  ENDIF
                END IF
              END IF

              ! heating
              SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow =   &
                  SysSizing(AirLoopNum,CurOverallSimDay)%NonCoinHeatMassFlow / StdRhoAir
              IF (SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow > 0) THEN
                OutAirFrac = SysSizing(AirLoopNum,CurOverallSimDay)%DesOutAirVolFlow / &
                                  SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow
              ELSE
                OutAirFrac = 0.0d0
              END IF
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))

              IF (SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow > 0) THEN
                Xs = MIN(1.0d0,FinalSysSizing(AirLoopNum)%SysUncOA / SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow)
              ELSE
                Xs = 0.0d0
              END IF
              IF (FinalSysSizing(AirLoopNum)%OAAutosized .AND. SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow > 0) THEN
                NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
                MinHeatingEvz = 1.0d0
                VozSumHtgBySys(AirLoopNum) = 0.0d0
                IF (NumZonesHeated > 0) THEN
                  DO ZonesHeatedNum=1,NumZonesHeated
                    CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
                    MatchingCooledZoneNum = FindNumberinList(CtrlZoneNum,AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums, &
                                                 NumZonesCooled)
                    IF (MatchingCooledZoneNum == 0) THEN
                      ! Zone air secondary recirculation fraction
                      Er = FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation
                      Ep = FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFractionHtg
                      ZoneOAFrac = FinalZoneSizing(CtrlZoneNum)%ZpzHtgByZone
                      ZoneEz = FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating
                      IF (Er > 0.0d0) THEN
                        ! multi-path ventilation system using VRP
                        Fa = Ep + (1.0d0 - Ep) * Er
                        Fb = Ep
                        Fc = 1.0d0 - (1.0d0 - ZoneEz)*(1.0d0 - Er)*(1.0d0 - Ep)

                        ! Calc zone ventilation efficiency
                        IF (Fa > 0.0d0) THEN
                          SysHeatingEv = 1.0d0 + Xs * Fb / Fa - ZoneOAFrac * Ep * Fc / Fa
                        ELSE
                          SysHeatingEv = 1.0d0
                        END IF
                      ELSE
                        ! single-path ventilation system
                        SysHeatingEv = 1.0d0 + Xs - ZoneOAFrac
                      END IF
                    END IF
                    IF (SysHeatingEv < MinHeatingEvz) MinHeatingEvz = SysHeatingEv
                    EvzByZoneHeatPrev(CtrlZoneNum) = EvzByZoneHeat(CtrlZoneNum)        ! Save previous EvzByZoneHeat
                    EvzByZoneHeat(CtrlZoneNum) = SysHeatingEv
                    VozSumHtgBySys(AirLoopNum) = VozSumHtgBySys(AirLoopNum) + FinalZoneSizing(CtrlZoneNum)%VozHtgByZone
                  END DO
                ELSE
                  NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
                  DO ZonesHeatedNum=1,NumZonesCooled
                    CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedNum)
                    ! Zone air secondary recirculation fraction
                    Er = FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation
                    Ep = FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFractionHtg
                    ZoneOAFrac = FinalZoneSizing(CtrlZoneNum)%ZpzHtgByZone
                    ZoneEz = FinalZoneSizing(CtrlZoneNum)%ZoneADEffHeating
                    IF (Er > 0.0d0) THEN
                      ! multi-path ventilation system using VRP
                      Fa = Ep + (1.0d0 - Ep) * Er
                      Fb = Ep
                      Fc = 1.0d0 - (1.0d0 - ZoneEz)*(1.0d0 - Er)*(1.0d0 - Ep)

                      ! Calc zone ventilation efficiency
                      IF (Fa > 0.0d0) THEN
                        SysHeatingEv = 1.0d0 + Xs * Fb / Fa - ZoneOAFrac * Ep * Fc / Fa
                      ELSE
                        SysHeatingEv = 1.0d0
                      END IF
                    ELSE
                      ! single-path ventilation system
                      SysHeatingEv = 1.0d0 + Xs - ZoneOAFrac
                    END IF
                    IF (SysHeatingEv < MinHeatingEvz) MinHeatingEvz = SysHeatingEv
                    EvzByZoneHeatPrev(CtrlZoneNum) = EvzByZoneHeat(CtrlZoneNum)     ! Save previous EvzByZoneHeat
                    EvzByZoneHeat(CtrlZoneNum) = SysHeatingEv
                    VozSumHtgBySys(AirLoopNum) = VozSumHtgBySys(AirLoopNum) + FinalZoneSizing(CtrlZoneNum)%VozHtgByZone
                  END DO
                END IF

                IF (MinHeatingEvz > 0) THEN
                  ! Std 62.1-2010, section 6.2.5.4: Eq. 6.6
                  ! (However, I don't think people diversity can be done correctly in E+ Sizing so assuming D=1 in this equation
                  !Vou = Diversity*(Rp*Pz) + Ra*Az
                  Vou = FinalSysSizing(AirLoopNum)%SysUncOA
                  Vot = Vou / MinHeatingEvz
                  IF (Vot > VotHtgBySys(AirLoopNum)) THEN
                    !This might be the cooling design day so only update if Vot is larger than the previous
                    VotHtgBySys(AirLoopNum) = Vot
                    XsBySysHeat(AirLoopNum) = Xs
                    EvzMinBySysHeat(AirLoopNum) = MinHeatingEvz
                  ELSE
                    !Restore EvzByZoneHeat() since it was just reset by the current (but not highest Vot) design day
                    IF (NumZonesHeated > 0) THEN
                      DO ZonesHeatedNum=1,NumZonesHeated
                        CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
                        EvzByZoneHeat(CtrlZoneNum)= EvzByZoneHeatPrev(CtrlZoneNum)
                      ENDDO
                    ELSE
                      DO ZonesHeatedNum=1,NumZonesCooled
                        CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedNum)
                        EvzByZoneHeat(CtrlZoneNum)= EvzByZoneHeatPrev(CtrlZoneNum)
                      ENDDO
                    ENDIF
                  ENDIF
                END IF
              END IF
            ELSE  ! error
            END IF

            SysSizing(AirLoopNum,CurOverallSimDay)%DesMainVolFlow = MAX(SysSizing(AirLoopNum,CurOverallSimDay)%DesCoolVolFlow, &
                                                       SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow)
        END SELECT

         ! If the ventilation was autosized using the ASHRAE VRP method, then the design zone ventilation value
         ! must be based on the larger of the system-level cooling Vot and/or heating Vot
         IF (FinalSysSizing(AirLoopNum)%OAAutosized .AND. FinalSysSizing(AirLoopNum)%SystemOAMethod == SOAM_VRP) THEN
            VotMax = MAX(VotClgBySys(AirLoopNum), VotHtgBySys(AirLoopNum))

            !Reset the system level ventilation
            FinalSysSizing(AirLoopNum)%DesOutAirVolFlow = VotMax
            CalcSysSizing(AirLoopNum)%DesOutAirVolFlow = VotMax

            IF (VotClgBySys(AirLoopNum) >= VotHtgBySys(AirLoopNum)) THEN
             !**Reset zone min ventilation based on max cooling Vot
                !The system-level Vot will always be larger than the sum of the zone Voz
                ! and so the zone-level Voz must be prorated so their sum equals the system level Vot
                Ratio = 1.0d0
                IF (VozSumClgBySys(AirLoopNum) > 0) Ratio = VotClgBySys(AirLoopNum)/ VozSumClgBySys(AirLoopNum)
                DO ZonesCooledNum=1,NumZonesCooled
                   CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
                   FinalZoneSizing(CtrlZoneNum)%MinOA = Ratio*FinalZoneSizing(CtrlZoneNum)%VozClgByZone
                ENDDO
            ELSE
             !**Reset zone min ventilation based on max heating Vot
                !What are number of zones attached to this ventilation-fed AHU
                NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
                NumZonesForHtg = NumZonesHeated
                IF (NumZonesHeated == 0) NumZonesHeated = NumZonesCooled

                !The system-level Vot will always be larger than the sum of the zone Voz
                ! and so the zone-level Voz must be prorated so their sum equals the system level Vot
                Ratio = 1.0d0
                IF (VozSumHtgBySys(AirLoopNum) > 0) Ratio = VotHtgBySys(AirLoopNum)/VozSumHtgBySys(AirLoopNum)
                DO ZonesHeatedNum=1,NumZonesForHtg
                  IF (NumZonesHeated == 0) THEN
                     CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedNum)
                  ELSE
                     CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
                  ENDIF
                  FinalZoneSizing(CtrlZoneNum)%MinOA = Ratio*FinalZoneSizing(CtrlZoneNum)%VozHtgByZone
               ENDDO
            ENDIF

         ENDIF

      END DO

    CASE (EndSysSizingCalc)

      ! Correct the zone return temperature in FinalZoneSizing for the case of induction units. The calc in
      ! ZoneEquipmentManager assumes all the air entering the zone goes into the return node.
      DO CtrlZoneNum=1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
        termunitsizingtemp=(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat)
        termunitsizingtempfrac=(1.0d0/termunitsizingtemp)
        RetTempRise = FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtCoolPeak - &
                        FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak
        IF (RetTempRise > 0.01d0) THEN
!avoid possible compiler bug
!          FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtCoolPeak = &
!            FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak + RetTempRise * &
!           (1.d0/(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat))
          FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtCoolPeak = &
            FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak +   &
            RetTempRise * termunitsizingtempfrac
        END IF
        RetTempRise = FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak - &
                        FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak
        IF (RetTempRise > 0.01d0) THEN
!avoid possible compiler bug
!          FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak = &
!            FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak + RetTempRise * &
!            (1.d0/(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat))
          FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak = &
            FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak +  &
            RetTempRise * termunitsizingtempfrac
        END IF
        DO TimeStepIndex=1,NumOfTimeStepInDay
          RetTempRise = FinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex) - &
                          FinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex)
          IF (RetTempRise > 0.01d0) THEN
!avoid possible compiler bug
!            FinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex) = &
!              FinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex) + RetTempRise * &
!             (1.d0/(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat))
            FinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex) = &
              FinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex) +   &
              RetTempRise * termunitsizingtempfrac
          END IF
          RetTempRise = FinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex) - &
                          FinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex)
          IF (RetTempRise > 0.01d0) THEN
!avoid possible compiler bug
!            FinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex) = &
!              FinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex) + RetTempRise * &
!             (1.d0/(1.d0+TermUnitSizing(CtrlZoneNum)%InducRat))
            FinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex) = &
              FinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex) +   &
              RetTempRise * termunitsizingtempfrac
          END IF
        END DO
      END DO

      ! Get final design flows
      DO AirLoopNum=1,NumPrimaryAirSys

        ! For coincident sizing, loop over design days and pick out the largest central heating amd
        ! cooling flow rates and associated data

        DO DDNum=1,TotDesDays+TotRunDesPersDays

          IF ( SysSizing(AirLoopNum,DDNum)%SensCoolCap > CalcSysSizing(AirLoopNum)%SensCoolCap )  THEN
            CalcSysSizing(AirLoopNum)%DesCoolVolFlow = SysSizing(AirLoopNum,DDNum)%DesCoolVolFlow
            CalcSysSizing(AirLoopNum)%CoolDesDay = SysSizing(AirLoopNum,DDNum)%CoolDesDay
            CalcSysSizing(AirLoopNum)%CoinCoolMassFlow = SysSizing(AirLoopNum,DDNum)%CoinCoolMassFlow
            CalcSysSizing(AirLoopNum)%SensCoolCap = SysSizing(AirLoopNum,DDNum)%SensCoolCap
            CalcSysSizing(AirLoopNum)%CoolFlowSeq = SysSizing(AirLoopNum,DDNum)%CoolFlowSeq
            CalcSysSizing(AirLoopNum)%SensCoolCapSeq = SysSizing(AirLoopNum,DDNum)%SensCoolCapSeq
            CalcSysSizing(AirLoopNum)%CoolMixTemp = SysSizing(AirLoopNum,DDNum)%CoolMixTemp
            CalcSysSizing(AirLoopNum)%CoolRetTemp = SysSizing(AirLoopNum,DDNum)%CoolRetTemp
            CalcSysSizing(AirLoopNum)%CoolMixHumRat = SysSizing(AirLoopNum,DDNum)%CoolMixHumRat
            CalcSysSizing(AirLoopNum)%CoolRetHumRat = SysSizing(AirLoopNum,DDNum)%CoolRetHumRat
            CalcSysSizing(AirLoopNum)%CoolOutTemp = SysSizing(AirLoopNum,DDNum)%CoolOutTemp
            CalcSysSizing(AirLoopNum)%CoolOutHumRat = SysSizing(AirLoopNum,DDNum)%CoolOutHumRat
            CalcSysSizing(AirLoopNum)%SysCoolRetTempSeq = SysSizing(AirLoopNum,DDNum)%SysCoolRetTempSeq
            CalcSysSizing(AirLoopNum)%SysCoolRetHumRatSeq = SysSizing(AirLoopNum,DDNum)%SysCoolRetHumRatSeq
            CalcSysSizing(AirLoopNum)%SysCoolOutTempSeq = SysSizing(AirLoopNum,DDNum)%SysCoolOutTempSeq
            CalcSysSizing(AirLoopNum)%SysCoolOutHumRatSeq = SysSizing(AirLoopNum,DDNum)%SysCoolOutHumRatSeq
          END IF

          IF ( SysSizing(AirLoopNum,DDNum)%HeatCap > CalcSysSizing(AirLoopNum)%HeatCap )  THEN
            CalcSysSizing(AirLoopNum)%DesHeatVolFlow = SysSizing(AirLoopNum,DDNum)%DesHeatVolFlow
            CalcSysSizing(AirLoopNum)%HeatDesDay = SysSizing(AirLoopNum,DDNum)%HeatDesDay
            CalcSysSizing(AirLoopNum)%CoinHeatMassFlow = SysSizing(AirLoopNum,DDNum)%CoinHeatMassFlow
            CalcSysSizing(AirLoopNum)%HeatCap = SysSizing(AirLoopNum,DDNum)%HeatCap
            CalcSysSizing(AirLoopNum)%PreHeatCap = SysSizing(AirLoopNum,DDNum)%PreHeatCap
            CalcSysSizing(AirLoopNum)%HeatFlowSeq = SysSizing(AirLoopNum,DDNum)%HeatFlowSeq
            CalcSysSizing(AirLoopNum)%HeatCapSeq = SysSizing(AirLoopNum,DDNum)%HeatCapSeq
            CalcSysSizing(AirLoopNum)%PreHeatCapSeq = SysSizing(AirLoopNum,DDNum)%PreHeatCapSeq
            CalcSysSizing(AirLoopNum)%HeatMixTemp = SysSizing(AirLoopNum,DDNum)%HeatMixTemp
            CalcSysSizing(AirLoopNum)%HeatRetTemp = SysSizing(AirLoopNum,DDNum)%HeatRetTemp
            CalcSysSizing(AirLoopNum)%HeatMixHumRat = SysSizing(AirLoopNum,DDNum)%HeatMixHumRat
            CalcSysSizing(AirLoopNum)%HeatRetHumRat = SysSizing(AirLoopNum,DDNum)%HeatRetHumRat
            CalcSysSizing(AirLoopNum)%HeatOutTemp = SysSizing(AirLoopNum,DDNum)%HeatOutTemp
            CalcSysSizing(AirLoopNum)%HeatOutHumRat = SysSizing(AirLoopNum,DDNum)%HeatOutHumRat
            CalcSysSizing(AirLoopNum)%SysHeatRetTempSeq = SysSizing(AirLoopNum,DDNum)%SysHeatRetTempSeq
            CalcSysSizing(AirLoopNum)%SysHeatRetHumRatSeq = SysSizing(AirLoopNum,DDNum)%SysHeatRetHumRatSeq
            CalcSysSizing(AirLoopNum)%SysHeatOutTempSeq = SysSizing(AirLoopNum,DDNum)%SysHeatOutTempSeq
            CalcSysSizing(AirLoopNum)%SysHeatOutHumRatSeq = SysSizing(AirLoopNum,DDNum)%SysHeatOutHumRatSeq
          END IF

        END DO

        CalcSysSizing(AirLoopNum)%DesMainVolFlow = MAX(CalcSysSizing(AirLoopNum)%DesCoolVolFlow, &
                                                            CalcSysSizing(AirLoopNum)%DesHeatVolFlow)

        ! For noncoincident sizing, find the max heat and cool mass flow for each zone over all the
        ! design days. Then calculate the associated heating and cooling capacities.

        NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
        NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
        SysCoolRetTemp = 0.0d0
        OutAirFrac = 0.0d0
        SysCoolMixTemp = 0.0d0
        SysSensCoolCap = 0.0d0
        CoolTimeStepNum = 0
        CoolDDNum = 0
        OutAirTemp = 0.0d0
        OutAirHumRat = 0.0d0
        SysCoolMixHumRat = 0.0d0
        SysCoolRetHumRat = 0.0d0
        SysCoolOutTemp = 0.0d0
        SysCoolOutHumRat = 0.0d0

        DO ZonesCooledNum=1,NumZonesCooled ! loop over cooled zones
          CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
          ! save the system cooling supply air temp
          FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInTempTU = CalcSysSizing(AirLoopNum)%CoolSupTemp
          ! save the system cooling supply air hum rat
          FinalZoneSizing(CtrlZoneNum)%DesCoolCoilInHumRatTU = CalcSysSizing(AirLoopNum)%CoolSupHumRat
          IF (FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow <= 0.0d0) CYCLE
          CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow = &
            CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow + &
            FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          SysCoolRetTemp = SysCoolRetTemp + FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtCoolPeak * &
                                      FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow &
                                       / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          SysCoolRetHumRat = SysCoolRetHumRat + FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtCoolPeak * &
                                      FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow &
                                       / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          CoolDDNum = FinalZoneSizing(CtrlZoneNum)%CoolDDNum
          CoolTimeStepNum = FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtCoolMax
          OutAirTemp = OutAirTemp + DesDayWeath(CoolDDNum)%Temp(CoolTimeStepNum)* &
                         FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          OutAirHumRat = OutAirHumRat + DesDayWeath(CoolDDNum)%HumRat(CoolTimeStepNum)* &
                           FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
        END DO
        IF ( CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow > 0.0 ) THEN
          SysCoolRetTemp = SysCoolRetTemp / CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow
          SysCoolRetHumRat = SysCoolRetHumRat / CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow
          OutAirTemp = OutAirTemp / CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow
          OutAirHumRat = OutAirHumRat / CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow
          SysCoolOutTemp = OutAirTemp
          SysCoolOutHumRat = OutAirHumRat
          RhoAir = StdRhoAir
          IF (CalcSysSizing(AirLoopNum)%CoolOAOption == MinOA) THEN
            OutAirFrac = RhoAir*CalcSysSizing(AirLoopNum)%DesOutAirVolFlow / &
                         CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow
            OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
          ELSE
            OutAirFrac = 1.0d0
          END IF
          SysCoolMixTemp = OutAirTemp*OutAirFrac + SysCoolRetTemp*(1.d0-OutAirFrac)
          SysCoolMixHumRat = OutAirHumRat*OutAirFrac + SysCoolRetHumRat*(1.d0-OutAirFrac)
          SysSensCoolCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) * CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow * &
                           (SysCoolMixTemp - CalcSysSizing(AirLoopNum)%CoolSupTemp)
          SysSensCoolCap = MAX(0.0d0,SysSensCoolCap)
        END IF

        SysHeatRetTemp = 0.0d0
        OutAirFrac = 0.0d0
        SysHeatMixTemp = 0.0d0
        SysHeatCap = 0.0d0
        HeatTimeStepNum = 0
        HeatDDNum = 0
        OutAirTemp = 0.0d0
        OutAirHumRat = 0.0d0
        SysHeatMixHumRat = 0.0d0
        SysHeatRetHumRat = 0.0d0
        SysHeatOutTemp = 0.0d0
        SysHeatOutHumRat = 0.0d0

        IF (NumZonesHeated.GT.0) THEN ! IF there are centrally heated zones

          DO ZonesHeatedNum=1,NumZonesHeated ! loop over the heated zones
            CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
            ! save the system heating supply air temp
            FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInTempTU = CalcSysSizing(AirLoopNum)%HeatSupTemp
            ! save the system heating supply air hum rat
            FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInHumRatTU= CalcSysSizing(AirLoopNum)%HeatSupHumRat
            IF (FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow <= 0.0d0) CYCLE
            CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow = &
              CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow + &
              FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            SysHeatRetTemp = SysHeatRetTemp + FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak * &
                                      FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow &
                                        / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            SysHeatRetHumRat = SysHeatRetHumRat + FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak * &
                                      FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow &
                                       / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            HeatDDNum = FinalZoneSizing(CtrlZoneNum)%HeatDDNum
            HeatTimeStepNum = FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax
            OutAirTemp = OutAirTemp + DesDayWeath(HeatDDNum)%Temp(HeatTimeStepNum) * &
                           FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            OutAirHumRat = OutAirHumRat + DesDayWeath(HeatDDNum)%HumRat(HeatTimeStepNum) * &
                             FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          END DO
          IF ( CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow > 0.0d0 ) THEN
            SysHeatRetTemp = SysHeatRetTemp / CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
            SysHeatRetHumRat = SysHeatRetHumRat / CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
            OutAirTemp = OutAirTemp / CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
            OutAirHumRat = OutAirHumRat / CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
            SysHeatOutTemp = OutAirTemp
            SysHeatOutHumRat = OutAirHumRat
            RhoAir = StdRhoAir
            IF (CalcSysSizing(AirLoopNum)%HeatOAOption == MinOA) THEN
              OutAirFrac = RhoAir*CalcSysSizing(AirLoopNum)%DesOutAirVolFlow / &
                           CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            ELSE
              OutAirFrac = 1.0d0
            END IF
            SysHeatMixTemp = OutAirTemp*OutAirFrac + SysHeatRetTemp*(1.0d0-OutAirFrac)
            SysHeatMixHumRat = OutAirHumRat*OutAirFrac + SysHeatRetHumRat*(1.0d0-OutAirFrac)
            SysHeatCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) * CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow * &
                              (CalcSysSizing(AirLoopNum)%HeatSupTemp - SysHeatMixTemp)
            SysHeatCap = MAX(0.0d0,SysHeatCap)
          END IF

        ELSE ! No centrally heated zones: use cooled zones

          DO ZonesCooledNum=1,NumZonesCooled ! loop over the cooled zones
            CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
            ! save the system heating supply air temp
            FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInTempTU = CalcSysSizing(AirLoopNum)%HeatSupTemp
            ! save the system heating supply air hum rat
            FinalZoneSizing(CtrlZoneNum)%DesHeatCoilInHumRatTU = CalcSysSizing(AirLoopNum)%HeatSupHumRat
            IF (FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow <= 0.0d0) CYCLE
            CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow = &
              CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow + &
              FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            SysHeatRetTemp = SysHeatRetTemp + FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak * &
                                      FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow &
                                       / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            SysHeatRetHumRat = SysHeatRetHumRat + FinalZoneSizing(CtrlZoneNum)%ZoneHumRatAtHeatPeak * &
                                      FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow &
                                       / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            HeatDDNum = FinalZoneSizing(CtrlZoneNum)%HeatDDNum
            HeatTimeStepNum = FinalZoneSizing(CtrlZoneNum)%TimeStepNumAtHeatMax
            OutAirTemp = OutAirTemp + DesDayWeath(HeatDDNum)%Temp(HeatTimeStepNum) * &
                           FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            OutAirHumRat = OutAirHumRat + DesDayWeath(HeatDDNum)%HumRat(HeatTimeStepNum) * &
                             FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow / (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
          END DO
          IF ( CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow > 0.0d0 ) THEN
            SysHeatRetTemp = SysHeatRetTemp / CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
            SysHeatRetHumRat = SysHeatRetHumRat / CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
            OutAirTemp = OutAirTemp / CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
            OutAirHumRat = OutAirHumRat / CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
            SysHeatOutTemp = OutAirTemp
            SysHeatOutHumRat = OutAirHumRat
            RhoAir = StdRhoAir
            IF (CalcSysSizing(AirLoopNum)%HeatOAOption == MinOA) THEN
              OutAirFrac = RhoAir*CalcSysSizing(AirLoopNum)%DesOutAirVolFlow / &
                           CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            ELSE
              OutAirFrac = 1.0d0
            END IF
            SysHeatMixTemp = OutAirTemp*OutAirFrac + SysHeatRetTemp*(1.d0-OutAirFrac)
            SysHeatMixHumRat = OutAirHumRat*OutAirFrac + SysHeatRetHumRat*(1.d0-OutAirFrac)
            SysHeatCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) * CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow * &
                              (CalcSysSizing(AirLoopNum)%HeatSupTemp - SysHeatMixTemp)
            SysHeatCap = MAX(0.0d0,SysHeatCap)
          END IF

        END IF

        ! move the noncoincident results into the system sizing array
        IF (CalcSysSizing(AirLoopNum)%SizingOption == NonCoincident) THEN
          ! But first check to see if the noncoincident result is actually bigger than the coincident (for 100% outside air)
          IF ( .not. (FinalSysSizing(AirLoopNum)%CoolOAOption == 1 .and. SysSensCoolCap <= 0.0d0)) THEN
            CalcSysSizing(AirLoopNum)%SensCoolCap = SysSensCoolCap
            CalcSysSizing(AirLoopNum)%CoolMixTemp = SysCoolMixTemp
            CalcSysSizing(AirLoopNum)%CoolRetTemp = SysCoolRetTemp
            CalcSysSizing(AirLoopNum)%CoolMixHumRat = SysCoolMixHumRat
            CalcSysSizing(AirLoopNum)%CoolRetHumRat = SysCoolRetHumRat
            CalcSysSizing(AirLoopNum)%CoolOutTemp = SysCoolOutTemp
            CalcSysSizing(AirLoopNum)%CoolOutHumRat = SysCoolOutHumRat
          END IF
          ! check to see is the noncoincident result is actually bigger than the coincident (for 100% outside air)
          IF ( .not. (FinalSysSizing(AirLoopNum)%HeatOAOption == 1 .and. SysHeatCap < 0.0d0)) THEN
            CalcSysSizing(AirLoopNum)%HeatCap = SysHeatCap
            CalcSysSizing(AirLoopNum)%HeatMixTemp = SysHeatMixTemp
            CalcSysSizing(AirLoopNum)%HeatRetTemp = SysHeatRetTemp
            CalcSysSizing(AirLoopNum)%HeatMixHumRat = SysHeatMixHumRat
            CalcSysSizing(AirLoopNum)%HeatRetHumRat = SysHeatRetHumRat
            CalcSysSizing(AirLoopNum)%HeatOutTemp = SysHeatOutTemp
            CalcSysSizing(AirLoopNum)%HeatOutHumRat = SysHeatOutHumRat
          END IF
          CalcSysSizing(AirLoopNum)%DesCoolVolFlow = CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow / &
                                                        StdRhoAir
          CalcSysSizing(AirLoopNum)%DesHeatVolFlow = CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow / &
                                                        StdRhoAir
          CalcSysSizing(AirLoopNum)%DesMainVolFlow = MAX(CalcSysSizing(AirLoopNum)%DesCoolVolFlow, &
                                                       CalcSysSizing(AirLoopNum)%DesHeatVolFlow)

        END IF

      END DO

      ! Move final system design data (calculated from zone data) to user design array
      FinalSysSizing%CoolDesDay = CalcSysSizing%CoolDesDay
      FinalSysSizing%HeatDesDay = CalcSysSizing%HeatDesDay
      FinalSysSizing%CoinCoolMassFlow = CalcSysSizing%CoinCoolMassFlow
      FinalSysSizing%CoinHeatMassFlow = CalcSysSizing%CoinHeatMassFlow
      FinalSysSizing%NonCoinCoolMassFlow = CalcSysSizing%NonCoinCoolMassFlow
      FinalSysSizing%NonCoinHeatMassFlow = CalcSysSizing%NonCoinHeatMassFlow
      FinalSysSizing%DesMainVolFlow = CalcSysSizing%DesMainVolFlow
      FinalSysSizing%DesHeatVolFlow = CalcSysSizing%DesHeatVolFlow
      FinalSysSizing%DesCoolVolFlow = CalcSysSizing%DesCoolVolFlow
      FinalSysSizing%SensCoolCap = CalcSysSizing%SensCoolCap
      FinalSysSizing%HeatCap = CalcSysSizing%HeatCap
      FinalSysSizing%PreheatCap = CalcSysSizing%PreheatCap
      FinalSysSizing%CoolMixTemp = CalcSysSizing%CoolMixTemp
      FinalSysSizing%CoolMixHumRat = CalcSysSizing%CoolMixHumRat
      FinalSysSizing%CoolRetTemp = CalcSysSizing%CoolRetTemp
      FinalSysSizing%CoolRetHumRat = CalcSysSizing%CoolRetHumRat
      FinalSysSizing%CoolOutTemp = CalcSysSizing%CoolOutTemp
      FinalSysSizing%CoolOutHumRat = CalcSysSizing%CoolOutHumRat
      FinalSysSizing%HeatMixTemp = CalcSysSizing%HeatMixTemp
      FinalSysSizing%HeatMixHumRat = CalcSysSizing%HeatMixHumRat
      FinalSysSizing%HeatRetTemp = CalcSysSizing%HeatRetTemp
      FinalSysSizing%HeatRetHumRat = CalcSysSizing%HeatRetHumRat
      FinalSysSizing%HeatOutTemp = CalcSysSizing%HeatOutTemp
      FinalSysSizing%HeatOutHumRat = CalcSysSizing%HeatOutHumRat

      DO AirLoopNum=1,NumPrimaryAirSys
        DO TimeStepIndex=1,NumOfTimeStepInDay
          FinalSysSizing(AirLoopNum)%HeatFlowSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum)%HeatFlowSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%CoolFlowSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum)%CoolFlowSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%SensCoolCapSeq(TimeStepIndex) = &
            CalcSysSizing(AirLoopNum)%SensCoolCapSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%HeatCapSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum)%HeatCapSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%PreheatCapSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum)%PreheatCapSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%SysCoolRetTempSeq(TimeStepIndex) = &
            CalcSysSizing(AirLoopNum)%SysCoolRetTempSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%SysCoolRetHumRatSeq(TimeStepIndex) = &
            CalcSysSizing(AirLoopNum)%SysCoolRetHumRatSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%SysHeatRetTempSeq(TimeStepIndex) = &
            CalcSysSizing(AirLoopNum)%SysHeatRetTempSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%SysHeatRetHumRatSeq(TimeStepIndex) = &
            CalcSysSizing(AirLoopNum)%SysHeatRetHumRatSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%SysCoolOutTempSeq(TimeStepIndex) = &
            CalcSysSizing(AirLoopNum)%SysCoolOutTempSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%SysCoolOutHumRatSeq(TimeStepIndex) = &
            CalcSysSizing(AirLoopNum)%SysCoolOutHumRatSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%SysHeatOutTempSeq(TimeStepIndex) = &
            CalcSysSizing(AirLoopNum)%SysHeatOutTempSeq(TimeStepIndex)
          FinalSysSizing(AirLoopNum)%SysHeatOutHumRatSeq(TimeStepIndex) = &
            CalcSysSizing(AirLoopNum)%SysHeatOutHumRatSeq(TimeStepIndex)
        END DO
      END DO

      ! Capture the changes to FinalZoneSizing in TermUnitFinalZoneSizing
      TermUnitFinalZoneSizing = FinalZoneSizing

      ! Check for user input design system flow rates. Set the sizing ratios.
      DO AirLoopNum=1,NumPrimaryAirSys

        NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
        NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
        RhoAir = StdRhoAir
        SysCoolSizingRat = 0.0d0
        IF (CalcSysSizing(AirLoopNum)%InpDesCoolAirFlow > 0.0 .AND. &
            CalcSysSizing(AirLoopNum)%DesCoolVolFlow > 0.0 .AND. &
            CalcSysSizing(AirLoopNum)%CoolAirDesMethod == InpDesAirFlow) THEN
          SysCoolSizingRat = CalcSysSizing(AirLoopNum)%InpDesCoolAirFlow / &
            CalcSysSizing(AirLoopNum)%DesCoolVolFlow
        ELSE
          SysCoolSizingRat = 1.0d0
        END IF

        SysHeatSizingRat = 0.0d0
        IF (CalcSysSizing(AirLoopNum)%InpDesHeatAirFlow > 0.0 .AND. &
            CalcSysSizing(AirLoopNum)%DesHeatVolFlow > 0.0 .AND. &
            CalcSysSizing(AirLoopNum)%HeatAirDesMethod == InpDesAirFlow) THEN
          SysHeatSizingRat = CalcSysSizing(AirLoopNum)%InpDesHeatAirFlow / &
            CalcSysSizing(AirLoopNum)%DesHeatVolFlow
        ELSE
          SysHeatSizingRat = 1.0d0
        END IF

        IF (CalcSysSizing(AirLoopNum)%LoadSizeType == Ventilation .AND. SysCoolSizingRat == 1.0d0) THEN
          IF (CalcSysSizing(AirLoopNum)%DesCoolVolFlow > 0.d0) THEN
            SysCoolSizingRat = CalcSysSizing(AirLoopNum)%DesOutAirVolFlow / CalcSysSizing(AirLoopNum)%DesCoolVolFlow
          ELSE
            SysCoolSizingRat = 1.d0
          ENDIF
        END IF
        IF (CalcSysSizing(AirLoopNum)%LoadSizeType == Ventilation .AND. SysHeatSizingRat == 1.0d0) THEN
          IF ( CalcSysSizing(AirLoopNum)%DesHeatVolFlow > 0.d0 ) THEN
            SysHeatSizingRat = CalcSysSizing(AirLoopNum)%DesOutAirVolFlow / CalcSysSizing(AirLoopNum)%DesHeatVolFlow
          ELSE
            SysHeatSizingRat = 1.d0
          ENDIF
        END IF

        ! Calculate the new user modified system design quantities
        IF (ABS(SysCoolSizingRat-1.0d0) > .00001d0) THEN

          FinalSysSizing(AirLoopNum)%CoinCoolMassFlow = SysCoolSizingRat*CalcSysSizing(AirLoopNum)%CoinCoolMassFlow
          FinalSysSizing(AirLoopNum)%NonCoinCoolMassFlow = SysCoolSizingRat*CalcSysSizing(AirLoopNum)%NonCoinCoolMassFlow
          FinalSysSizing(AirLoopNum)%DesCoolVolFlow = SysCoolSizingRat*CalcSysSizing(AirLoopNum)%DesCoolVolFlow

          IF (FinalSysSizing(AirLoopNum)%DesCoolVolFlow > 0.0d0) THEN

            DO TimeStepIndex=1,NumOfTimeStepInDay

              IF (CalcSysSizing(AirLoopNum)%CoolFlowSeq(TimeStepIndex) > 0.0d0) THEN

                FinalSysSizing(AirLoopNum)%CoolFlowSeq(TimeStepIndex) = &
                  SysCoolSizingRat*CalcSysSizing(AirLoopNum)%CoolFlowSeq(TimeStepIndex)
                IF (FinalSysSizing(AirLoopNum)%CoolOAOption == MinOA) THEN
                  OutAirFrac = RhoAir*FinalSysSizing(AirLoopNum)%DesOutAirVolFlow / &
                                 FinalSysSizing(AirLoopNum)%CoolFlowSeq(TimeStepIndex)
                  OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
                ELSE
                  OutAirFrac = 1.0d0
                END IF
                SysCoolMixTemp = FinalSysSizing(AirLoopNum)%SysCoolOutTempSeq(TimeStepIndex)*OutAirFrac + &
                                   FinalSysSizing(AirLoopNum)%SysCoolRetTempSeq(TimeStepIndex)*(1.d0-OutAirFrac)
                SysCoolMixHumRat = FinalSysSizing(AirLoopNum)%SysCoolOutHumRatSeq(TimeStepIndex)*OutAirFrac + &
                                     FinalSysSizing(AirLoopNum)%SysCoolRetHumRatSeq(TimeStepIndex)*(1.d0-OutAirFrac)
                SysSensCoolCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) *   &
                                   FinalSysSizing(AirLoopNum)%CoolFlowSeq(TimeStepIndex) * &
                                   (SysCoolMixTemp - FinalSysSizing(AirLoopNum)%CoolSupTemp)
                SysSensCoolCap = MAX(0.0d0,SysSensCoolCap)
                FinalSysSizing(AirLoopNum)%SensCoolCapSeq(TimeStepIndex) = SysSensCoolCap

              END IF

            END DO

            IF (FinalSysSizing(AirLoopNum)%CoolOAOption == MinOA) THEN
              OutAirFrac = FinalSysSizing(AirLoopNum)%DesOutAirVolFlow / FinalSysSizing(AirLoopNum)%DesCoolVolFlow
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            ELSE
              OutAirFrac = 1.0d0
            END IF
            FinalSysSizing(AirLoopNum)%CoolMixTemp = FinalSysSizing(AirLoopNum)%CoolOutTemp*OutAirFrac + &
                                                      FinalSysSizing(AirLoopNum)%CoolRetTemp*(1.d0-OutAirFrac)
            FinalSysSizing(AirLoopNum)%CoolMixHumRat = FinalSysSizing(AirLoopNum)%CoolOutHumRat*OutAirFrac + &
                                                        FinalSysSizing(AirLoopNum)%CoolRetHumRat*(1.d0-OutAirFrac)
            FinalSysSizing(AirLoopNum)%SensCoolCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) * RhoAir * &
              FinalSysSizing(AirLoopNum)%DesCoolVolFlow * &
              (FinalSysSizing(AirLoopNum)%CoolMixTemp - FinalSysSizing(AirLoopNum)%CoolSupTemp)
            FinalSysSizing(AirLoopNum)%SensCoolCap = MAX(0.0d0,FinalSysSizing(AirLoopNum)%SensCoolCap)

          END IF

          ! take account of the user input system flow rates and alter the zone flow rates to match
          DO ZonesCooledNum=1,NumZonesCooled
            CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
            IF ((SysCoolSizingRat .NE. 1.0d0) .AND. (FinalSysSizing(AirLoopNum)%LoadSizeType == Ventilation) .AND. &
                (FinalZoneSizing(CtrlZoneNum)%MinOA > 0.0d0)) THEN
              ! size on ventilation load
              ZoneOARatio = FinalZoneSizing(CtrlZoneNum)%MinOA /   &
                               MAX(FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow,FinalZoneSizing(CtrlZoneNum)%MinOA)
              TermUnitFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow = FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow * ZoneOARatio &
                                                                      * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
              TermUnitFinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow = FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow &
                                                                       * ZoneOARatio * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
              TermUnitFinalZoneSizing(CtrlZoneNum)%DesCoolLoad = FinalZoneSizing(CtrlZoneNum)%DesCoolLoad * ZoneOARatio &
                                                                   * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
              TermUnitFinalZoneSizing(CtrlZoneNum)%CoolFlowSeq = FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq * ZoneOARatio &
                                                                   * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
              TermUnitFinalZoneSizing(CtrlZoneNum)%CoolLoadSeq = FinalZoneSizing(CtrlZoneNum)%CoolLoadSeq * ZoneOARatio &
                                                                   * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
            ELSE IF ( (SysCoolSizingRat > 1.0d0) .OR. &
                      (SysCoolSizingRat < 1.0d0 .AND. FinalSysSizing(AirLoopNum)%SizingOption == NonCoincident) ) THEN
              ! size on user input system design flows
              TermUnitFinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow =   &
                                     FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlow * SysCoolSizingRat
              TermUnitFinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow =   &
                                     FinalZoneSizing(CtrlZoneNum)%DesCoolMassFlow * SysCoolSizingRat
              TermUnitFinalZoneSizing(CtrlZoneNum)%DesCoolLoad = FinalZoneSizing(CtrlZoneNum)%DesCoolLoad * SysCoolSizingRat
              TermUnitFinalZoneSizing(CtrlZoneNum)%CoolFlowSeq = FinalZoneSizing(CtrlZoneNum)%CoolFlowSeq * SysCoolSizingRat
              TermUnitFinalZoneSizing(CtrlZoneNum)%CoolLoadSeq = FinalZoneSizing(CtrlZoneNum)%CoolLoadSeq * SysCoolSizingRat
            END IF
          END DO

        END IF

        IF (ABS(SysHeatSizingRat-1.0) > .00001d0) THEN

          FinalSysSizing(AirLoopNum)%CoinHeatMassFlow = SysHeatSizingRat*CalcSysSizing(AirLoopNum)%CoinHeatMassFlow
          FinalSysSizing(AirLoopNum)%NonCoinHeatMassFlow = SysHeatSizingRat*CalcSysSizing(AirLoopNum)%NonCoinHeatMassFlow
          FinalSysSizing(AirLoopNum)%DesHeatVolFlow = SysHeatSizingRat*CalcSysSizing(AirLoopNum)%DesHeatVolFlow

          IF (FinalSysSizing(AirLoopNum)%DesHeatVolFlow > 0.0d0) THEN

            DO TimeStepIndex=1,NumOfTimeStepInDay

              IF (CalcSysSizing(AirLoopNum)%HeatFlowSeq(TimeStepIndex) > 0.0d0) THEN

                FinalSysSizing(AirLoopNum)%HeatFlowSeq(TimeStepIndex) = &
                  SysHeatSizingRat*CalcSysSizing(AirLoopNum)%HeatFlowSeq(TimeStepIndex)
                IF (FinalSysSizing(AirLoopNum)%HeatOAOption == MinOA) THEN
                  OutAirFrac = RhoAir*FinalSysSizing(AirLoopNum)%DesOutAirVolFlow / &
                                 FinalSysSizing(AirLoopNum)%HeatFlowSeq(TimeStepIndex)
                  OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
                ELSE
                  OutAirFrac = 1.0d0
                END IF
                SysHeatMixTemp = FinalSysSizing(AirLoopNum)%SysHeatOutTempSeq(TimeStepIndex)*OutAirFrac + &
                                   FinalSysSizing(AirLoopNum)%SysHeatRetTempSeq(TimeStepIndex)*(1.0d0-OutAirFrac)
                SysHeatMixHumRat = FinalSysSizing(AirLoopNum)%SysHeatOutHumRatSeq(TimeStepIndex)*OutAirFrac + &
                                     FinalSysSizing(AirLoopNum)%SysHeatRetHumRatSeq(TimeStepIndex)*(1.0d0-OutAirFrac)
                SysHeatCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) *   &
                                FinalSysSizing(AirLoopNum)%HeatFlowSeq(TimeStepIndex) * &
                               (FinalSysSizing(AirLoopNum)%HeatSupTemp - SysHeatMixTemp)
                SysHeatCap = MAX(0.0d0,SysHeatCap)
                FinalSysSizing(AirLoopNum)%HeatCapSeq(TimeStepIndex) = SysHeatCap

              END IF

            END DO

            IF (FinalSysSizing(AirLoopNum)%HeatOAOption == MinOA) THEN
              OutAirFrac = FinalSysSizing(AirLoopNum)%DesOutAirVolFlow / FinalSysSizing(AirLoopNum)%DesHeatVolFlow
              OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            ELSE
              OutAirFrac = 1.0d0
            END IF
            FinalSysSizing(AirLoopNum)%HeatMixTemp = FinalSysSizing(AirLoopNum)%HeatOutTemp*OutAirFrac + &
                                                  FinalSysSizing(AirLoopNum)%HeatRetTemp*(1.d0-OutAirFrac)
            FinalSysSizing(AirLoopNum)%HeatMixHumRat = FinalSysSizing(AirLoopNum)%HeatOutHumRat*OutAirFrac + &
                                                  FinalSysSizing(AirLoopNum)%HeatRetHumRat*(1.d0-OutAirFrac)
            FinalSysSizing(AirLoopNum)%HeatCap = PsyCpAirFnWTdb(constant_zero,constant_twenty) * RhoAir * &
              FinalSysSizing(AirLoopNum)%DesHeatVolFlow * &
              (FinalSysSizing(AirLoopNum)%HeatSupTemp - FinalSysSizing(AirLoopNum)%HeatMixTemp)
            FinalSysSizing(AirLoopNum)%HeatCap = MAX(0.0d0,FinalSysSizing(AirLoopNum)%HeatCap)

          END IF
          ! take account of the user input system flow rates and alter the zone flow rates to match (for terminal unit sizing)
          IF (NumZonesHeated.GT.0) THEN ! IF there are centrally heated zones
            DO ZonesHeatedNum=1,NumZonesHeated ! loop over the heated zones
              CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
              IF ((SysHeatSizingRat .NE. 1.0d0) .AND. (FinalSysSizing(AirLoopNum)%LoadSizeType == Ventilation) .AND. &
                  (FinalZoneSizing(CtrlZoneNum)%MinOA > 0.0d0)) THEN
                ZoneOARatio = FinalZoneSizing(CtrlZoneNum)%MinOA /   &
                                     MAX(FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow,FinalZoneSizing(CtrlZoneNum)%MinOA)
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow = FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow &
                                                       * ZoneOARatio * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow =  FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow &
                                                       * ZoneOARatio * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatLoad = FinalZoneSizing(CtrlZoneNum)%DesHeatLoad * ZoneOARatio &
                                                       * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
                TermUnitFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq = FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq * ZoneOARatio &
                                                       * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
                TermUnitFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq = FinalZoneSizing(CtrlZoneNum)%HeatLoadSeq * ZoneOARatio &
                                                       * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
              ELSE IF ( (SysHeatSizingRat > 1.0d0) .OR. &
                        (SysHeatSizingRat < 1.0d0 .AND. FinalSysSizing(AirLoopNum)%SizingOption == NonCoincident) ) THEN
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow =   &
                                        FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow * SysHeatSizingRat
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow =   &
                                        FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow * SysHeatSizingRat
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatLoad = FinalZoneSizing(CtrlZoneNum)%DesHeatLoad * SysHeatSizingRat
                TermUnitFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq = FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq * SysHeatSizingRat
                TermUnitFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq = FinalZoneSizing(CtrlZoneNum)%HeatLoadSeq * SysHeatSizingRat
              END IF
            END DO
          ELSE ! No centrally heated zones: use cooled zones
            DO ZonesCooledNum=1,NumZonesCooled ! loop over the cooled zones
              CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
              IF ((SysHeatSizingRat .NE. 1.0d0) .AND. (FinalSysSizing(AirLoopNum)%LoadSizeType == Ventilation) .AND. &
                  (FinalZoneSizing(CtrlZoneNum)%MinOA <= 0.0d0)) THEN
                CALL ShowWarningError('FinalSystemSizing: AirLoop="'//trim(AirToZoneNodeInfo(AirLoopNum)%AirLoopName)//  &
                   '", Requested sizing on Ventilation,')
                CALL ShowContinueError('but Zone has no design OA Flow. Zone="'//  &
                   trim(ZoneEquipConfig(CtrlZoneNum)%ZoneName)//'".')
              ENDIF
              IF ((SysHeatSizingRat .NE. 1.0d0) .AND. (FinalSysSizing(AirLoopNum)%LoadSizeType == Ventilation) .AND. &
                  (FinalZoneSizing(CtrlZoneNum)%MinOA > 0.0d0)) THEN
                ZoneOARatio = FinalZoneSizing(CtrlZoneNum)%MinOA /   &
                                        MAX(FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow,FinalZoneSizing(CtrlZoneNum)%MinOA)
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow = FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow * ZoneOARatio &
                                                                        * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow = FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow * ZoneOARatio &
                                                                         * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatLoad = FinalZoneSizing(CtrlZoneNum)%DesHeatLoad * ZoneOARatio &
                                                                     * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
                TermUnitFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq = FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq * ZoneOARatio &
                                                                     * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
                TermUnitFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq = FinalZoneSizing(CtrlZoneNum)%HeatLoadSeq * ZoneOARatio &
                                                                     * (1.d0 + TermUnitSizing(CtrlZoneNum)%InducRat)
              ELSE  IF ((SysHeatSizingRat .NE. 1.0d0) .AND. (FinalSysSizing(AirLoopNum)%LoadSizeType == Ventilation) .AND. &
                        (FinalZoneSizing(CtrlZoneNum)%MinOA > 0.0d0)) THEN
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow =   &
                                        FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow * SysHeatSizingRat
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow =   &
                                        FinalZoneSizing(CtrlZoneNum)%DesHeatMassFlow * SysHeatSizingRat
                TermUnitFinalZoneSizing(CtrlZoneNum)%DesHeatLoad = FinalZoneSizing(CtrlZoneNum)%DesHeatLoad * SysHeatSizingRat
                TermUnitFinalZoneSizing(CtrlZoneNum)%HeatFlowSeq = FinalZoneSizing(CtrlZoneNum)%HeatFlowSeq * SysHeatSizingRat
                TermUnitFinalZoneSizing(CtrlZoneNum)%HeatLoadSeq = FinalZoneSizing(CtrlZoneNum)%HeatLoadSeq * SysHeatSizingRat
              END IF
            END DO
          END IF

        END IF

        FinalSysSizing(AirLoopNum)%DesMainVolFlow = MAX(FinalSysSizing(AirLoopNum)%DesCoolVolFlow, &
                                                     FinalSysSizing(AirLoopNum)%DesHeatVolFlow)

        ! loop over the zones cooled by this system and sum up the min cooling flow rates to get the
        ! min system cooling flow rate
        DO ZonesCooledNum=1,NumZonesCooled
          CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
          FinalSysSizing(AirLoopNum)%DesCoolVolFlowMin = FinalSysSizing(AirLoopNum)%DesCoolVolFlowMin +   &
             FinalZoneSizing(CtrlZoneNum)%DesCoolVolFlowMin
        END DO
        IF (FinalSysSizing(AirLoopNum)%DesCoolVolFlowMin <= 0.0d0) THEN
          FinalSysSizing(AirLoopNum)%DesCoolVolFlowMin = FinalSysSizing(AirLoopNum)%DesOutAirVolFlow
        END IF
      END DO


      ! EMS calling point to customize zone sizing results
      CALL ManageEMS(emsCallFromSystemSizing)

      !EMS override point
      IF (AnyEnergyManagementSystemInModel) THEN
        DO AirLoopNum=1,NumPrimaryAirSys
          IF (FinalSysSizing(AirLoopNum)%EMSOverrideCoinCoolMassFlowOn) &
              FinalSysSizing(AirLoopNum)%CoinCoolMassFlow = FinalSysSizing(AirLoopNum)%EMSValueCoinCoolMassFlow
          IF (FinalSysSizing(AirLoopNum)%EMSOverrideCoinHeatMassFlowOn) &
              FinalSysSizing(AirLoopNum)%CoinHeatMassFlow = FinalSysSizing(AirLoopNum)%EMSValueCoinHeatMassFlow
          IF (FinalSysSizing(AirLoopNum)%EMSOverrideNonCoinCoolMassFlowOn) &
              FinalSysSizing(AirLoopNum)%NonCoinCoolMassFlow = FinalSysSizing(AirLoopNum)%EMSValueNonCoinCoolMassFlow
          IF (FinalSysSizing(AirLoopNum)%EMSOverrideNonCoinHeatMassFlowOn) &
              FinalSysSizing(AirLoopNum)%NonCoinHeatMassFlow = FinalSysSizing(AirLoopNum)%EMSValueNonCoinHeatMassFlow
          IF (FinalSysSizing(AirLoopNum)%EMSOverrideDesMainVolFlowOn) &
              FinalSysSizing(AirLoopNum)%DesMainVolFlow = FinalSysSizing(AirLoopNum)%EMSValueDesMainVolFlow
          IF (FinalSysSizing(AirLoopNum)%EMSOverrideDesHeatVolFlowOn) &
              FinalSysSizing(AirLoopNum)%DesHeatVolFlow = FinalSysSizing(AirLoopNum)%EMSValueDesHeatVolFlow
          IF (FinalSysSizing(AirLoopNum)%EMSOverrideDesCoolVolFlowOn) &
              FinalSysSizing(AirLoopNum)%DesCoolVolFlow = FinalSysSizing(AirLoopNum)%EMSValueDesCoolVolFlow

        ENDDO ! over NumPrimaryAirSys
      ENDIF

      ! write out the sys design calc results
      WRITE(OutputFileSysSizing,SSizeFmt10,ADVANCE='No')
      DO I=1,NumPrimaryAirSys
        WRITE(OutputFileSysSizing,SSizeFmt11,ADVANCE='No') SizingFileColSep,  &
                          TRIM(CalcSysSizing(I)%AirPriLoopName),':Des Heat Mass Flow [kg/s]',  &
                          SizingFileColSep,TRIM(CalcSysSizing(I)%AirPriLoopName),':Des Cool Mass Flow [kg/s]',  &
                          SizingFileColSep,TRIM(CalcSysSizing(I)%AirPriLoopName),':Des Heat Cap [W]',  &
                          SizingFileColSep,TRIM(CalcSysSizing(I)%AirPriLoopName),':Des Sens Cool Cap [W]'
      ENDDO
      WRITE(OutputFileSysSizing,fmta) ' '
!      HourFrac = 0.0
      Minutes=0
      TimeStepIndex=0
      DO HourCounter=1,24
        DO TimeStepCounter=1,NumOfTimeStepInHour
          TimeStepIndex=TimeStepIndex+1
          Minutes=Minutes+MinutesPerTimeStep
          IF (Minutes == 60) THEN
            Minutes=0
            HourPrint=HourCounter
          ELSE
            HourPrint=HourCounter-1
          ENDIF
!      DO TimeStepIndex=1,NumOfTimeStepInDay
!        HourFrac = HourFrac + TimeStepZone
          WRITE(OutputFileSysSizing,SSizeFmt20,ADVANCE='No') HourPrint,Minutes
          DO I=1,NumPrimaryAirSys
            WRITE(OutputFileSysSizing,SSizeFmt21,ADVANCE='No')                                &
                            SizingFileColSep,CalcSysSizing(I)%HeatFlowSeq(TimeStepIndex),     &
                            SizingFileColSep,CalcSysSizing(I)%CoolFlowSeq(TimeStepIndex),     &
                            SizingFileColSep,CalcSysSizing(I)%HeatCapSeq(TimeStepIndex),      &
                            SizingFileColSep,CalcSysSizing(I)%SensCoolCapSeq(TimeStepIndex)
          END DO
          WRITE(OutputFileSysSizing,fmta) ' '
        END DO
      END DO
      WRITE(OutputFileSysSizing,SSizeFmt30,ADVANCE='No')
      DO I=1,NumPrimaryAirSys
        WRITE(OutputFileSysSizing,SSizeFmt31,ADVANCE='No')                &
                     SizingFileColSep,CalcSysSizing(I)%CoinHeatMassFlow,  &
                     SizingFileColSep,CalcSysSizing(I)%CoinCoolMassFlow,  &
                     SizingFileColSep,CalcSysSizing(I)%HeatCap,           &
                     SizingFileColSep,CalcSysSizing(I)%SensCoolCap
      END DO
      WRITE(OutputFileSysSizing,fmta) ' '
      WRITE(OutputFileSysSizing,SSizeFmt40,ADVANCE='No')
      DO I=1,NumPrimaryAirSys
        WRITE(OutputFileSysSizing,SSizeFmt41,ADVANCE='No')                  &
                     SizingFileColSep,CalcSysSizing(I)%NonCoinHeatMassFlow, &
                     SizingFileColSep,CalcSysSizing(I)%NonCoinCoolMassFlow, &
                     SizingFileColSep,CalcSysSizing(I)%HeatCap,             &
                     SizingFileColSep,CalcSysSizing(I)%SensCoolCap
      END DO
      WRITE(OutputFileSysSizing,fmta) ' '

      ! write predefined standard 62.1 report data
      DO AirLoopNum=1,NumPrimaryAirSys
        IF (FinalSysSizing(AirLoopNum)%SystemOAMethod == SOAM_VRP) THEN
          !system ventilation requirements for cooling table
          CALL PreDefTableEntry(pdchS62svrClVps,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             FinalSysSizing(AirLoopNum)%DesCoolVolFlow,3) !Vps
          CALL PreDefTableEntry(pdchS62svrClXs,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             XsBySysCool(AirLoopNum),3)   !Xs
          CALL PreDefTableEntry(pdchS62svrClEv,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             EvzMinBySysCool(AirLoopNum),3)   !Ev
          !system ventilation requirements for heating table
          CALL PreDefTableEntry(pdchS62svrHtVps,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             FinalSysSizing(AirLoopNum)%DesHeatVolFlow,3) !Vps
          CALL PreDefTableEntry(pdchS62svrHtXs,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             XsBySysHeat(AirLoopNum),3)   !Xs
          CALL PreDefTableEntry(pdchS62svrHtEv,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             EvzMinBySysHeat(AirLoopNum),3)   !Ev
          CALL PreDefTableEntry(pdchS62svrHtVot,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             VotHtgBySys(AirLoopNum),4)    !Vot
          IF (FinalSysSizing(AirLoopNum)%DesHeatVolFlow .NE. 0.0d0) THEN     ! Move here from other routine
            CALL PreDefTableEntry(pdchS62svrHtPercOA,FinalSysSizing(AirLoopNum)%AirPriLoopName, &
                  VotHtgBySys(AirLoopNum) / FinalSysSizing(AirLoopNum)%DesHeatVolFlow)  !%OA
          END IF
          !system ventilation calculations for cooling table
          CALL PreDefTableEntry(pdchS62scdVps,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             FinalSysSizing(AirLoopNum)%DesCoolVolFlow,3) !Vps
          CALL PreDefTableEntry(pdchS62scdEvz,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             EvzMinBySysCool(AirLoopNum),3)   !Evz-min
          CALL PreDefTableEntry(pdchS62svrClVot,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             VotClgBySys(AirLoopNum),4)              !Vot
          IF (FinalSysSizing(AirLoopNum)%DesCoolVolFlow .NE. 0.0d0) THEN    ! Move here
            CALL PreDefTableEntry(pdchS62svrClPercOA,FinalSysSizing(AirLoopNum)%AirPriLoopName, &
                VotClgBySys(AirLoopNum) / FinalSysSizing(AirLoopNum)%DesCoolVolFlow)  !%OA
          END IF

          !system ventilation calculations for heating table
          CALL PreDefTableEntry(pdchS62shdVps,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             FinalSysSizing(AirLoopNum)%DesHeatVolFlow,3) !Vps
          CALL PreDefTableEntry(pdchS62shdEvz,FinalSysSizing(AirLoopNum)%AirPriLoopName,  &
             EvzMinBySysHeat(AirLoopNum),3)   !Evz-min
          !zone cooling design table
          NumZonesCooled = AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
          DO ZonesCooledNum=1,NumZonesCooled ! loop over cooled zones
            CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledNum)
            CALL PreDefTableEntry(pdchS62zcdAlN,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
               AirToZoneNodeInfo(AirLoopNum)%AirLoopName)               !Air loop name
            CALL PreDefTableEntry(pdchS62zcdEvz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
               EvzByZoneCool(CtrlZoneNum),3)                              !Evz
            CALL PreDefTableEntry(pdchS62zcdEr,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
               FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation,3)  !Er
            CALL PreDefTableEntry(pdchS62zcdFa,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
               FaByZoneCool(CtrlZoneNum),3)                                !Fa
            CALL PreDefTableEntry(pdchS62zcdFb,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
               FbByZoneCool(CtrlZoneNum),3)                                !Fb
            CALL PreDefTableEntry(pdchS62zcdFc,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
               FcByZoneCool(CtrlZoneNum),3)                                !Fc
            CALL PreDefTableEntry(pdchS62zcdEp,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
               FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFraction,3)      !Ep
          END DO
          !zone heating design table
          NumZonesHeated = AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
          IF (NumZonesHeated > 0) THEN
            DO ZonesHeatedNum=1,NumZonesHeated ! loop over the heated zones
              CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedNum)
              CALL PreDefTableEntry(pdchS62zhdAlN,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 AirToZoneNodeInfo(AirLoopNum)%AirLoopName)               !Air loop name
              CALL PreDefTableEntry(pdchS62zhdEvz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 EvzByZoneHeat(CtrlZoneNum),3)                              !Evz
              CALL PreDefTableEntry(pdchS62zhdEr,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation,3)  !Er
              CALL PreDefTableEntry(pdchS62zhdFa,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FaByZoneHeat(CtrlZoneNum),3)                                !Fa
              CALL PreDefTableEntry(pdchS62zhdFb,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FbByZoneHeat(CtrlZoneNum),3)                                !Fb
              CALL PreDefTableEntry(pdchS62zhdFc,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FcByZoneHeat(CtrlZoneNum),3)                                !Fc
              CALL PreDefTableEntry(pdchS62zhdEp,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFractionHtg,3)   !Ep
            END DO
          ELSE
            DO ZonesHeatedNum=1,NumZonesCooled ! loop over the heated zones
              CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedNum)
              CALL PreDefTableEntry(pdchS62zhdAlN,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 AirToZoneNodeInfo(AirLoopNum)%AirLoopName)               !Air loop name
              CALL PreDefTableEntry(pdchS62zhdEvz,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 EvzByZoneHeat(CtrlZoneNum),3)                              !Evz
              CALL PreDefTableEntry(pdchS62zhdEr,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FinalZoneSizing(CtrlZoneNum)%ZoneSecondaryRecirculation,3)  !Er
              CALL PreDefTableEntry(pdchS62zhdFa,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FaByZoneHeat(CtrlZoneNum),3)                                !Fa
              CALL PreDefTableEntry(pdchS62zhdFb,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FbByZoneHeat(CtrlZoneNum),3)                                !Fb
              CALL PreDefTableEntry(pdchS62zhdFc,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FcByZoneHeat(CtrlZoneNum),3)                                !Fc
              CALL PreDefTableEntry(pdchS62zhdEp,FinalZoneSizing(CtrlZoneNum)%ZoneName,  &
                 FinalZoneSizing(CtrlZoneNum)%ZonePrimaryAirFractionHtg,3)   !Ep
            END DO
          END IF
        END IF
      END DO

  END SELECT


  RETURN

END SUBROUTINE UpdateSysSizing

! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Reporting subroutines for the SimAir Module
! *****************************************************************************

!        End of Reporting subroutines for the SimAir Module
! *****************************************************************************

!        Utility Subroutines for the SimAir Module
! *****************************************************************************

!        End of Utility subroutines for the SimAir Module
! *****************************************************************************

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

END MODULE SimAirServingZones
